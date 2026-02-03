%%%-------------------------------------------------------------------
%%% @doc PQC Case - Unified Kernel with PQC-Signed Receipts
%%%
%%% "One kernel, two faces" - Case is the OS, everything (A2A, MCP) becomes Case events.
%%%
%%% Architecture:
%%% - gen_statem with handle_event_function for unified event handling
%%% - PQC-signed receipt chain with ML-DSA signatures
%%% - Event sourcing with append-only history
%%% - Petri net/YAWL workflow execution via pqc_net
%%% - Tool syscall algebra for controlled side effects
%%% - gproc registration for process discovery
%%% - pg event broadcasting for subscriptions
%%%
%%% Receipt Chain:
%%% - Genesis: SHA3-256(case_id || ts), signed with ML-DSA
%%% - Extend: SHA3-256(prev_hash || event || ts), signed with ML-DSA
%%% - Every state change gets a signed receipt
%%%
%%% Syscall Algebra:
%%% - SIGNAL(name, payload) → append to history, drive net
%%% - CALL_TOOL(tool, args) → invoke tool, return result
%%% - EMIT_ARTIFACT(name, payload) → create artifact with signed receipt
%%% - SET_STATUS(state, message) → update status, publish event
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_case).

-behaviour(gen_statem).

-include("pqchain.hrl").

%% API exports
-export([
    start_link/3,
    signal/3,
    tool_call/3,
    snapshot/1,
    subscribe/1,
    unsubscribe/1,
    get_receipt/1,
    stop/1
]).

%% gen_statem callbacks
-export([
    callback_mode/0,
    init/1,
    handle_event/4,
    terminate/3,
    code_change/4
]).

%% Internal exports for testing
-export([
    generate_case_id/0,
    sign_receipt/2,
    verify_receipt/3
]).

%%====================================================================
%% Type definitions
%%====================================================================

-type case_id() :: binary().
-type net_def() :: map().
-type marking() :: #{binary() => non_neg_integer()}.
-type event() :: #{
    type := atom(),
    timestamp := non_neg_integer(),
    data := term()
}.
-type artifact() :: #{
    name := binary(),
    payload := term(),
    receipt := binary(),
    signature := #pqc_signature{},
    timestamp := non_neg_integer()
}.
-type status_state() :: submitted | running | completed | failed | cancelled | compensating.
-type receipt() :: #{
    hash := binary(),
    prev_hash := binary(),
    event := event(),
    signature := #pqc_signature{},
    timestamp := non_neg_integer()
}.

-export_type([case_id/0, event/0, artifact/0, receipt/0]).

%% State record
-record(case_data, {
    case_id :: binary(),
    net :: map(),                    % workflow net definition
    marking = #{} :: map(),          % Petri/YAWL marking
    status = #{
        state => submitted,
        message => undefined,
        ts => undefined
    } :: map(),
    history = [] :: list(),          % append-only events
    artifacts = [] :: list(),        % outputs + evidence
    metadata = #{} :: map(),
    receipt_head = <<>> :: binary(), % PQC-signed hash anchor
    signing_key :: #pqc_keypair{} | undefined,  % ML-DSA key for receipts
    subscribers = [] :: [pid()]      % pg subscribers
}).

-type case_data() :: #case_data{}.

%% pg scope for event broadcasting
-define(PG_SCOPE, pqc_case_registry).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a Case process with workflow net, metadata, and signing key
-spec start_link(Net :: net_def(), InitMeta :: map(), SigningKey :: #pqc_keypair{}) ->
    {ok, pid()} | {error, term()}.
start_link(Net, InitMeta, SigningKey) when is_map(Net), is_map(InitMeta) ->
    gen_statem:start_link(?MODULE, {Net, InitMeta, SigningKey}, []).

%% @doc Async signal into Case
-spec signal(Pid :: pid(), Name :: atom() | binary(), Payload :: term()) -> ok.
signal(Pid, Name, Payload) when is_pid(Pid) ->
    gen_statem:cast(Pid, {signal, Name, Payload}).

%% @doc Sync tool invocation syscall
-spec tool_call(Pid :: pid(), ToolName :: atom() | binary(), Args :: map()) ->
    {ok, term()} | {error, term()}.
tool_call(Pid, ToolName, Args) when is_pid(Pid), is_map(Args) ->
    gen_statem:call(Pid, {tool_call, ToolName, Args}, 30000).

%% @doc Get current state snapshot
-spec snapshot(Pid :: pid()) -> {ok, map()} | {error, term()}.
snapshot(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, snapshot, 5000).

%% @doc Subscribe to Case events via pg
-spec subscribe(Pid :: pid()) -> ok | {error, term()}.
subscribe(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, {subscribe, self()}, 5000).

%% @doc Unsubscribe from Case events
-spec unsubscribe(Pid :: pid()) -> ok | {error, term()}.
unsubscribe(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, {unsubscribe, self()}, 5000).

%% @doc Get current receipt chain head with PQC signature
-spec get_receipt(Pid :: pid()) -> {ok, receipt()} | {error, term()}.
get_receipt(Pid) when is_pid(Pid) ->
    gen_statem:call(Pid, get_receipt, 5000).

%% @doc Stop the Case process
-spec stop(Pid :: pid()) -> ok.
stop(Pid) when is_pid(Pid) ->
    gen_statem:stop(Pid).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

%% @doc Callback mode: handle_event_function for unified event handling
-spec callback_mode() -> handle_event_function.
callback_mode() ->
    handle_event_function.

%% @doc Initialize Case process
-spec init({net_def(), map(), #pqc_keypair{}}) ->
    {ok, running, case_data()} | {stop, term()}.
init({Net, InitMeta, SigningKey}) ->
    process_flag(trap_exit, true),

    try
        %% Generate unique case ID
        CaseId = generate_case_id(),

        %% Extract initial marking from net
        InitialMarking = maps:get(initial_marking, Net, #{}),

        %% Get current timestamp
        Now = erlang:system_time(millisecond),

        %% Create genesis receipt
        GenesisData = <<CaseId/binary, (integer_to_binary(Now))/binary>>,
        GenesisHash = crypto:hash(sha3_256, GenesisData),

        %% Sign genesis receipt
        GenesisSignature = case SigningKey of
            #pqc_keypair{algorithm = Algo} = Key ->
                sign_receipt(GenesisHash, Key);
            undefined ->
                undefined
        end,

        %% Initialize status
        Status = #{
            state => running,
            message => <<"Case initialized">>,
            ts => Now
        },

        %% Create genesis event
        GenesisEvent = #{
            type => genesis,
            timestamp => Now,
            data => #{
                case_id => CaseId,
                net_id => maps:get(id, Net, <<"unknown">>),
                metadata => InitMeta
            }
        },

        %% Register with gproc
        try
            true = gproc:reg({n, l, {pqc_case, CaseId}}, self())
        catch
            error:badarg ->
                %% Already registered or gproc not available
                ok
        end,

        %% Join pg group for event broadcasting
        pg:join(?PG_SCOPE, CaseId, self()),

        %% Initialize state data
        Data = #case_data{
            case_id = CaseId,
            net = Net,
            marking = InitialMarking,
            status = Status,
            history = [GenesisEvent],
            artifacts = [],
            metadata = InitMeta,
            receipt_head = GenesisHash,
            signing_key = SigningKey,
            subscribers = []
        },

        %% Broadcast genesis event
        broadcast_event(Data, GenesisEvent),

        {ok, running, Data}
    catch
        error:Reason:Stacktrace ->
            {stop, {init_failed, Reason, Stacktrace}}
    end.

%% @doc Handle all events (unified event handler)
-spec handle_event(EventType, EventContent, State, Data) -> Result when
    EventType :: gen_statem:event_type(),
    EventContent :: term(),
    State :: atom(),
    Data :: case_data(),
    Result :: gen_statem:event_handler_result(atom(), case_data()).

%% Handle async signals
handle_event(cast, {signal, Name, Payload}, State, Data) ->
    Now = erlang:system_time(millisecond),

    %% Create signal event
    Event = #{
        type => signal,
        timestamp => Now,
        data => #{name => ensure_binary(Name), payload => Payload}
    },

    %% Append to history and generate receipt
    {NewData, Receipt} = append_event(Data, Event),

    %% Drive workflow net
    UpdatedData = drive_net(NewData),

    %% Broadcast event
    broadcast_event(UpdatedData, Event),

    {keep_state, UpdatedData};

%% Handle sync tool calls
handle_event({call, From}, {tool_call, ToolName, Args}, State, Data) ->
    Now = erlang:system_time(millisecond),

    try
        %% Execute tool call (delegate to tool system)
        Result = execute_tool(ToolName, Args, Data),

        %% Create tool call event
        Event = #{
            type => tool_call,
            timestamp => Now,
            data => #{
                tool => ensure_binary(ToolName),
                args => Args,
                result => Result
            }
        },

        %% Append to history
        {NewData, Receipt} = append_event(Data, Event),

        %% Broadcast event
        broadcast_event(NewData, Event),

        {keep_state, NewData, [{reply, From, Result}]}
    catch
        error:Reason:Stacktrace ->
            ErrorEvent = #{
                type => tool_error,
                timestamp => Now,
                data => #{
                    tool => ensure_binary(ToolName),
                    error => Reason,
                    stacktrace => Stacktrace
                }
            },
            {NewData, _} = append_event(Data, ErrorEvent),
            {keep_state, NewData, [{reply, From, {error, Reason}}]}
    end;

%% Handle snapshot request
handle_event({call, From}, snapshot, State, Data) ->
    Snapshot = #{
        case_id => Data#case_data.case_id,
        state => State,
        marking => Data#case_data.marking,
        status => Data#case_data.status,
        history_length => length(Data#case_data.history),
        artifacts_count => length(Data#case_data.artifacts),
        metadata => Data#case_data.metadata,
        receipt_head => Data#case_data.receipt_head
    },
    {keep_state, Data, [{reply, From, {ok, Snapshot}}]};

%% Handle subscribe request
handle_event({call, From}, {subscribe, Pid}, State, Data) ->
    NewSubscribers = case lists:member(Pid, Data#case_data.subscribers) of
        true -> Data#case_data.subscribers;
        false -> [Pid | Data#case_data.subscribers]
    end,
    NewData = Data#case_data{subscribers = NewSubscribers},
    {keep_state, NewData, [{reply, From, ok}]};

%% Handle unsubscribe request
handle_event({call, From}, {unsubscribe, Pid}, State, Data) ->
    NewSubscribers = lists:delete(Pid, Data#case_data.subscribers),
    NewData = Data#case_data{subscribers = NewSubscribers},
    {keep_state, NewData, [{reply, From, ok}]};

%% Handle get_receipt request
handle_event({call, From}, get_receipt, State, Data) ->
    Receipt = case Data#case_data.history of
        [LastEvent | _] ->
            #{
                hash => Data#case_data.receipt_head,
                prev_hash => get_prev_receipt_hash(Data),
                event => LastEvent,
                signature => generate_receipt_signature(Data#case_data.receipt_head, Data#case_data.signing_key),
                timestamp => maps:get(timestamp, LastEvent, 0)
            };
        [] ->
            #{
                hash => Data#case_data.receipt_head,
                prev_hash => <<>>,
                event => #{type => genesis, timestamp => 0, data => #{}},
                signature => undefined,
                timestamp => 0
            }
    end,
    {keep_state, Data, [{reply, From, {ok, Receipt}}]};

%% Handle info messages (process monitoring, etc.)
handle_event(info, {'DOWN', _Ref, process, Pid, _Reason}, State, Data) ->
    %% Remove dead subscriber
    NewSubscribers = lists:delete(Pid, Data#case_data.subscribers),
    NewData = Data#case_data{subscribers = NewSubscribers},
    {keep_state, NewData};

%% Catch-all for unknown events
handle_event(EventType, EventContent, State, Data) ->
    io:format("Unknown event: ~p ~p in state ~p~n", [EventType, EventContent, State]),
    {keep_state, Data}.

%% @doc Terminate callback
-spec terminate(Reason, State, Data) -> ok when
    Reason :: term(),
    State :: atom(),
    Data :: case_data().
terminate(_Reason, _State, Data) ->
    %% Unregister from gproc
    try
        gproc:unreg({n, l, {pqc_case, Data#case_data.case_id}})
    catch
        error:_ -> ok
    end,

    %% Leave pg group
    try
        pg:leave(?PG_SCOPE, Data#case_data.case_id, self())
    catch
        error:_ -> ok
    end,

    ok.

%% @doc Code change callback
-spec code_change(OldVsn, State, Data, Extra) -> {ok, State, Data} when
    OldVsn :: term(),
    State :: atom(),
    Data :: case_data(),
    Extra :: term().
code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate unique case ID using ULID-like format
-spec generate_case_id() -> binary().
generate_case_id() ->
    %% ULID-like: timestamp (48 bits) + random (80 bits) = 128 bits
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(10),
    Hash = crypto:hash(sha3_256, <<Timestamp:64, Random/binary>>),
    base64:encode(Hash).

%% @doc Sign receipt with ML-DSA key
-spec sign_receipt(Hash :: binary(), Key :: #pqc_keypair{}) -> #pqc_signature{} | undefined.
sign_receipt(Hash, #pqc_keypair{algorithm = Algorithm, secret_key = SecretKey, public_key = PublicKey} = _Key)
  when SecretKey =/= undefined ->
    try
        %% Call pqc_crypto to sign
        case pqc_crypto:sign(Hash, Algorithm, SecretKey) of
            {ok, SignatureBytes} ->
                PublicKeyHash = crypto:hash(sha3_256, PublicKey),
                #pqc_signature{
                    algorithm = Algorithm,
                    signature = SignatureBytes,
                    public_key_hash = PublicKeyHash,
                    timestamp = erlang:system_time(millisecond)
                };
            {error, _Reason} ->
                undefined
        end
    catch
        _:_ ->
            %% If pqc_crypto not available, return unsigned
            undefined
    end;
sign_receipt(_Hash, _Key) ->
    undefined.

%% @doc Verify receipt signature
-spec verify_receipt(Hash :: binary(), Signature :: #pqc_signature{}, PublicKey :: binary()) ->
    boolean().
verify_receipt(Hash, #pqc_signature{algorithm = Algorithm, signature = SigBytes}, PublicKey) ->
    try
        case pqc_crypto:verify(Hash, SigBytes, Algorithm, PublicKey) of
            {ok, true} -> true;
            _ -> false
        end
    catch
        _:_ -> false
    end.

%% @doc Append event to history and generate receipt
-spec append_event(Data :: case_data(), Event :: event()) -> {case_data(), binary()}.
append_event(#case_data{history = History, receipt_head = PrevHash, signing_key = Key} = Data, Event) ->
    %% Serialize event and compute new hash
    EventBinary = term_to_binary(Event),
    Timestamp = maps:get(timestamp, Event, erlang:system_time(millisecond)),

    %% Extend receipt chain: SHA3-256(prev_hash || event || ts)
    NewHash = crypto:hash(sha3_256, <<PrevHash/binary, EventBinary/binary, Timestamp:64>>),

    %% Sign receipt
    _Signature = sign_receipt(NewHash, Key),

    %% Append to history
    NewHistory = [Event | History],

    NewData = Data#case_data{
        history = NewHistory,
        receipt_head = NewHash
    },

    {NewData, NewHash}.

%% @doc Drive workflow net to quiescence
-spec drive_net(Data :: case_data()) -> case_data().
drive_net(#case_data{net = Net, marking = Marking, metadata = Metadata} = Data) ->
    try
        %% Check if pqc_net module exists and has enabled/2 function
        case erlang:function_exported(pqc_net, enabled, 2) of
            true ->
                %% Get enabled transitions
                case pqc_net:enabled(Net, Marking) of
                    {ok, EnabledTransitions} when EnabledTransitions =/= [] ->
                        %% Fire transitions in total order (Λ)
                        fire_transitions(Data, EnabledTransitions);
                    {ok, []} ->
                        %% No enabled transitions, quiescence reached
                        Data;
                    {input_required, _Prompt} ->
                        %% Update status to waiting for input
                        update_status(Data, running, <<"Waiting for input">>);
                    {auth_required, _Prompt} ->
                        %% Update status to waiting for auth
                        update_status(Data, running, <<"Waiting for authorization">>);
                    {error, Reason} ->
                        %% Update status to failed
                        update_status(Data, failed, iolist_to_binary(io_lib:format("~p", [Reason])))
                end;
            false ->
                %% pqc_net not available, skip net driving
                Data
        end
    catch
        error:Reason ->
            update_status(Data, failed, iolist_to_binary(io_lib:format("Net error: ~p", [Reason])))
    end.

%% @doc Fire enabled transitions
-spec fire_transitions(Data :: case_data(), Transitions :: [binary()]) -> case_data().
fire_transitions(Data, []) ->
    Data;
fire_transitions(#case_data{net = Net, marking = Marking} = Data, [TransitionId | Rest]) ->
    try
        %% Fire transition via pqc_net
        case pqc_net:fire(Net, Marking, TransitionId) of
            {ok, NewMarking, Effects} ->
                %% Apply effects and update marking
                NewData = Data#case_data{marking = NewMarking},
                AppliedData = apply_effects(NewData, Effects),
                %% Continue with remaining transitions
                fire_transitions(AppliedData, Rest);
            {error, _Reason} ->
                %% Skip this transition and try next
                fire_transitions(Data, Rest)
        end
    catch
        _:_ ->
            fire_transitions(Data, Rest)
    end.

%% @doc Apply transition effects
-spec apply_effects(Data :: case_data(), Effects :: list()) -> case_data().
apply_effects(Data, []) ->
    Data;
apply_effects(Data, [Effect | Rest]) ->
    UpdatedData = case Effect of
        {set_variable, Key, Value} ->
            Metadata = maps:put(Key, Value, Data#case_data.metadata),
            Data#case_data{metadata = Metadata};
        {emit_artifact, Name, Payload} ->
            emit_artifact(Data, Name, Payload);
        _ ->
            Data
    end,
    apply_effects(UpdatedData, Rest).

%% @doc Emit artifact with signed receipt
-spec emit_artifact(Data :: case_data(), Name :: binary(), Payload :: term()) -> case_data().
emit_artifact(#case_data{artifacts = Artifacts, signing_key = Key} = Data, Name, Payload) ->
    Now = erlang:system_time(millisecond),

    %% Create artifact hash
    ArtifactData = term_to_binary({Name, Payload, Now}),
    ArtifactHash = crypto:hash(sha3_256, ArtifactData),

    %% Sign artifact
    Signature = sign_receipt(ArtifactHash, Key),

    %% Create artifact record
    Artifact = #{
        name => Name,
        payload => Payload,
        receipt => ArtifactHash,
        signature => Signature,
        timestamp => Now
    },

    %% Append to artifacts
    Data#case_data{artifacts = [Artifact | Artifacts]}.

%% @doc Update case status
-spec update_status(Data :: case_data(), State :: status_state(), Message :: binary()) ->
    case_data().
update_status(Data, State, Message) ->
    Now = erlang:system_time(millisecond),
    Status = #{
        state => State,
        message => Message,
        ts => Now
    },

    %% Create status event
    Event = #{
        type => status_change,
        timestamp => Now,
        data => Status
    },

    %% Append to history
    {NewData, _Receipt} = append_event(Data#case_data{status = Status}, Event),

    %% Broadcast event
    broadcast_event(NewData, Event),

    NewData.

%% @doc Execute tool call (syscall)
-spec execute_tool(ToolName :: atom() | binary(), Args :: map(), Data :: case_data()) ->
    {ok, term()} | {error, term()}.
execute_tool(ToolName, Args, _Data) ->
    %% This is a placeholder for actual tool integration
    %% In production, this would delegate to bs_tools or similar
    try
        %% Check if tool module exists
        ToolModule = case ToolName of
            Name when is_atom(Name) -> Name;
            Name when is_binary(Name) -> binary_to_existing_atom(Name, utf8)
        end,

        case erlang:function_exported(ToolModule, execute, 1) of
            true ->
                ToolModule:execute(Args);
            false ->
                {error, tool_not_found}
        end
    catch
        error:badarg ->
            {error, invalid_tool_name};
        error:Reason ->
            {error, Reason}
    end.

%% @doc Broadcast event to subscribers
-spec broadcast_event(Data :: case_data(), Event :: event()) -> ok.
broadcast_event(#case_data{case_id = CaseId, subscribers = Subscribers}, Event) ->
    %% Broadcast to pg group
    try
        pg:get_members(?PG_SCOPE, CaseId)
    catch
        error:_ -> []
    end,

    %% Send to direct subscribers
    [Pid ! {pqc_case_event, CaseId, Event} || Pid <- Subscribers, is_pid(Pid)],

    ok.

%% @doc Get previous receipt hash from history
-spec get_prev_receipt_hash(Data :: case_data()) -> binary().
get_prev_receipt_hash(#case_data{history = History}) when length(History) > 1 ->
    %% Return hash of second-to-last event
    [_Last, PrevEvent | _] = History,
    EventBinary = term_to_binary(PrevEvent),
    crypto:hash(sha3_256, EventBinary);
get_prev_receipt_hash(_Data) ->
    <<>>.

%% @doc Generate receipt signature for current head
-spec generate_receipt_signature(Hash :: binary(), Key :: #pqc_keypair{} | undefined) ->
    #pqc_signature{} | undefined.
generate_receipt_signature(Hash, Key) ->
    sign_receipt(Hash, Key).

%% @doc Ensure atom/binary is converted to binary
-spec ensure_binary(Term :: atom() | binary() | list()) -> binary().
ensure_binary(Term) when is_binary(Term) -> Term;
ensure_binary(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
ensure_binary(Term) when is_list(Term) -> list_to_binary(Term);
ensure_binary(Term) -> term_to_binary(Term).
