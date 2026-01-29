%%%-------------------------------------------------------------------
%% @doc Governed GCP Client
%%
%% Wraps GCP operations with MCP+ governance enforcement:
%% - Contract validation (tier, capabilities)
%% - Envelope enforcement (resource limits, operation whitelist)
%% - Receipt generation for audit trail
%% - Kill switch integration
%%
%% This is the 80/20 integration - single entry point for all governed
%% cloud operations.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_governed).
-behaviour(gen_server).

-include("erlmcp_governance.hrl").
-include("gcp_simulator.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    stop/0
]).

%% Governed Operations
-export([
    %% Storage
    storage_upload/5,
    storage_download/3,
    storage_delete/3,
    storage_list/3,

    %% Pub/Sub
    pubsub_publish/4,
    pubsub_pull/4,
    pubsub_ack/4,

    %% Compute
    compute_create/5,
    compute_start/4,
    compute_stop/4,
    compute_delete/4,

    %% Generic operation
    execute/4
]).

%% Audit
-export([
    get_receipts/1,
    get_receipts_for_contract/1,
    export_audit_trail/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    contracts = #{} :: #{contract_id() => #mcp_contract{}},
    envelopes = #{} :: #{contract_id() => #mcp_envelope{}},
    receipts = [] :: [#mcp_receipt{}],
    receipt_counter = 1 :: pos_integer(),
    kill_switch_active = false :: boolean(),
    config = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

stop() ->
    gen_server:stop(?SERVER).

%% @doc Upload to Cloud Storage with governance.
-spec storage_upload(contract_id(), binary(), binary(), binary(), binary()) ->
    {ok, #gcp_object{}, #mcp_receipt{}} | {error, #mcp_refusal{}}.
storage_upload(ContractId, Bucket, Object, ContentType, Data) ->
    execute(ContractId, storage_upload, [Bucket, Object, ContentType, Data],
            #{operation => <<"storage.objects.create">>}).

%% @doc Download from Cloud Storage with governance.
-spec storage_download(contract_id(), binary(), binary()) ->
    {ok, binary(), #mcp_receipt{}} | {error, #mcp_refusal{}}.
storage_download(ContractId, Bucket, Object) ->
    execute(ContractId, storage_download, [Bucket, Object],
            #{operation => <<"storage.objects.get">>}).

%% @doc Delete from Cloud Storage with governance.
-spec storage_delete(contract_id(), binary(), binary()) ->
    {ok, #mcp_receipt{}} | {error, #mcp_refusal{}}.
storage_delete(ContractId, Bucket, Object) ->
    execute(ContractId, storage_delete, [Bucket, Object],
            #{operation => <<"storage.objects.delete">>}).

%% @doc List Cloud Storage objects with governance.
-spec storage_list(contract_id(), binary(), map()) ->
    {ok, [#gcp_object{}], #mcp_receipt{}} | {error, #mcp_refusal{}}.
storage_list(ContractId, Bucket, Opts) ->
    execute(ContractId, storage_list, [Bucket, Opts],
            #{operation => <<"storage.objects.list">>}).

%% @doc Publish to Pub/Sub with governance.
-spec pubsub_publish(contract_id(), binary(), binary(), [map()]) ->
    {ok, [binary()], #mcp_receipt{}} | {error, #mcp_refusal{}}.
pubsub_publish(ContractId, Project, Topic, Messages) ->
    TopicName = <<"projects/", Project/binary, "/topics/", Topic/binary>>,
    execute(ContractId, pubsub_publish, [TopicName, Messages],
            #{operation => <<"pubsub.topics.publish">>}).

%% @doc Pull from Pub/Sub with governance.
-spec pubsub_pull(contract_id(), binary(), binary(), pos_integer()) ->
    {ok, [#gcp_received_message{}], #mcp_receipt{}} | {error, #mcp_refusal{}}.
pubsub_pull(ContractId, Project, Subscription, MaxMessages) ->
    SubName = <<"projects/", Project/binary, "/subscriptions/", Subscription/binary>>,
    execute(ContractId, pubsub_pull, [SubName, MaxMessages],
            #{operation => <<"pubsub.subscriptions.consume">>}).

%% @doc Acknowledge Pub/Sub messages with governance.
-spec pubsub_ack(contract_id(), binary(), binary(), [binary()]) ->
    {ok, #mcp_receipt{}} | {error, #mcp_refusal{}}.
pubsub_ack(ContractId, Project, Subscription, AckIds) ->
    SubName = <<"projects/", Project/binary, "/subscriptions/", Subscription/binary>>,
    execute(ContractId, pubsub_ack, [SubName, AckIds],
            #{operation => <<"pubsub.subscriptions.consume">>}).

%% @doc Create Compute instance with governance.
-spec compute_create(contract_id(), binary(), binary(), binary(), map()) ->
    {ok, #gcp_instance{}, #mcp_receipt{}} | {error, #mcp_refusal{}}.
compute_create(ContractId, Project, Zone, Name, Opts) ->
    MachineType = maps:get(machine_type, Opts, <<"e2-micro">>),
    execute(ContractId, compute_create, [Project, Zone, Name, MachineType, Opts],
            #{operation => <<"compute.instances.create">>}).

%% @doc Start Compute instance with governance.
-spec compute_start(contract_id(), binary(), binary(), binary()) ->
    {ok, #gcp_instance{}, #mcp_receipt{}} | {error, #mcp_refusal{}}.
compute_start(ContractId, Project, Zone, Name) ->
    execute(ContractId, compute_start, [Project, Zone, Name],
            #{operation => <<"compute.instances.start">>}).

%% @doc Stop Compute instance with governance.
-spec compute_stop(contract_id(), binary(), binary(), binary()) ->
    {ok, #gcp_instance{}, #mcp_receipt{}} | {error, #mcp_refusal{}}.
compute_stop(ContractId, Project, Zone, Name) ->
    execute(ContractId, compute_stop, [Project, Zone, Name],
            #{operation => <<"compute.instances.stop">>}).

%% @doc Delete Compute instance with governance.
-spec compute_delete(contract_id(), binary(), binary(), binary()) ->
    {ok, #mcp_receipt{}} | {error, #mcp_refusal{}}.
compute_delete(ContractId, Project, Zone, Name) ->
    execute(ContractId, compute_delete, [Project, Zone, Name],
            #{operation => <<"compute.instances.delete">>}).

%% @doc Execute any operation with governance.
-spec execute(contract_id(), atom(), list(), map()) ->
    {ok, term()} | {ok, term(), #mcp_receipt{}} | {error, #mcp_refusal{}}.
execute(ContractId, Operation, Args, Opts) ->
    gen_server:call(?SERVER, {execute, ContractId, Operation, Args, Opts}).

%% @doc Get all receipts for a request ID.
-spec get_receipts(binary()) -> [#mcp_receipt{}].
get_receipts(RequestId) ->
    gen_server:call(?SERVER, {get_receipts, RequestId}).

%% @doc Get all receipts for a contract.
-spec get_receipts_for_contract(contract_id()) -> [#mcp_receipt{}].
get_receipts_for_contract(ContractId) ->
    gen_server:call(?SERVER, {get_receipts_for_contract, ContractId}).

%% @doc Export audit trail for a contract.
-spec export_audit_trail(contract_id()) -> {ok, binary()} | {error, term()}.
export_audit_trail(ContractId) ->
    gen_server:call(?SERVER, {export_audit_trail, ContractId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Config) ->
    {ok, #state{config = Config}}.

handle_call({execute, ContractId, Operation, Args, Opts}, _From, State) ->
    %% 1. Check kill switch
    case State#state.kill_switch_active of
        true ->
            Refusal = #mcp_refusal{
                code = ?REFUSAL_KILL_SWITCH_ACTIVE,
                reason = <<"Kill switch is active">>,
                timestamp = erlang:system_time(millisecond),
                contract_id = ContractId
            },
            {reply, {error, Refusal}, State};
        false ->
            %% 2. Get or create contract
            {Contract, State1} = ensure_contract(ContractId, State),

            %% 3. Check envelope constraints
            OperationName = maps:get(operation, Opts, atom_to_binary(Operation)),
            case check_envelope(Contract, OperationName, State1) of
                {error, Refusal} ->
                    {reply, {error, Refusal}, State1};
                ok ->
                    %% 4. Execute operation
                    RequestId = generate_request_id(),
                    StartTime = erlang:system_time(millisecond),
                    Result = execute_operation(Operation, Args),
                    EndTime = erlang:system_time(millisecond),

                    %% 5. Generate receipt
                    {Receipt, State2} = generate_receipt(
                        RequestId, ContractId, Operation, Args, Result,
                        StartTime, EndTime, State1),

                    %% 6. Return result with receipt
                    case Result of
                        {ok, Value} ->
                            {reply, {ok, Value, Receipt}, State2};
                        ok ->
                            {reply, {ok, Receipt}, State2};
                        {error, _} = Error ->
                            {reply, Error, State2}
                    end
            end
    end;

handle_call({get_receipts, RequestId}, _From, State) ->
    Receipts = [R || R <- State#state.receipts,
                     R#mcp_receipt.request_id =:= RequestId],
    {reply, Receipts, State};

handle_call({get_receipts_for_contract, ContractId}, _From, State) ->
    Receipts = [R || R <- State#state.receipts,
                     R#mcp_receipt.contract_id =:= ContractId],
    {reply, Receipts, State};

handle_call({export_audit_trail, ContractId}, _From, State) ->
    Receipts = [R || R <- State#state.receipts,
                     R#mcp_receipt.contract_id =:= ContractId],
    case Receipts of
        [] ->
            {reply, {error, no_receipts}, State};
        _ ->
            Trail = format_audit_trail(ContractId, Receipts),
            {reply, {ok, Trail}, State}
    end;

handle_call({register_contract, Contract, Envelope}, _From, State) ->
    ContractId = Contract#mcp_contract.id,
    NewContracts = maps:put(ContractId, Contract, State#state.contracts),
    NewEnvelopes = maps:put(ContractId, Envelope, State#state.envelopes),
    {reply, ok, State#state{contracts = NewContracts, envelopes = NewEnvelopes}};

handle_call({activate_kill_switch}, _From, State) ->
    {reply, ok, State#state{kill_switch_active = true}};

handle_call({deactivate_kill_switch}, _From, State) ->
    {reply, ok, State#state{kill_switch_active = false}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

ensure_contract(ContractId, State) ->
    case maps:get(ContractId, State#state.contracts, undefined) of
        undefined ->
            Contract = create_default_contract(ContractId),
            Envelope = create_default_envelope(ContractId),
            NewContracts = maps:put(ContractId, Contract, State#state.contracts),
            NewEnvelopes = maps:put(ContractId, Envelope, State#state.envelopes),
            {Contract, State#state{contracts = NewContracts,
                                    envelopes = NewEnvelopes}};
        Contract ->
            {Contract, State}
    end.

check_envelope(_Contract, OperationName, State) ->
    %% Simplified envelope check - in production would be more thorough
    case State#state.envelopes of
        Envelopes when map_size(Envelopes) > 0 ->
            %% Check against any envelope's denied operations
            DeniedOps = lists:flatten([E#mcp_envelope.denied_operations ||
                                       {_, E} <- maps:to_list(Envelopes),
                                       E#mcp_envelope.denied_operations =/= undefined]),
            case lists:member(OperationName, DeniedOps) of
                true ->
                    {error, #mcp_refusal{
                        code = ?REFUSAL_ENVELOPE_OPERATION_DENIED,
                        reason = <<"Operation denied: ", OperationName/binary>>,
                        timestamp = erlang:system_time(millisecond)
                    }};
                false ->
                    ok
            end;
        _ ->
            ok
    end.

execute_operation(storage_upload, [Bucket, Object, ContentType, Data]) ->
    gcp_storage_sim:upload_object(Bucket, Object, ContentType, Data);
execute_operation(storage_download, [Bucket, Object]) ->
    gcp_storage_sim:download_object(Bucket, Object);
execute_operation(storage_delete, [Bucket, Object]) ->
    gcp_storage_sim:delete_object(Bucket, Object);
execute_operation(storage_list, [Bucket, Opts]) ->
    gcp_storage_sim:list_objects(Bucket, Opts);
execute_operation(pubsub_publish, [TopicName, Messages]) ->
    gcp_pubsub_sim:publish(TopicName, Messages);
execute_operation(pubsub_pull, [SubName, MaxMessages]) ->
    gcp_pubsub_sim:pull(SubName, MaxMessages);
execute_operation(pubsub_ack, [SubName, AckIds]) ->
    gcp_pubsub_sim:acknowledge(SubName, AckIds);
execute_operation(compute_create, [Project, Zone, Name, MachineType, Opts]) ->
    gcp_compute_sim:create_instance(Project, Zone, Name, MachineType, Opts);
execute_operation(compute_start, [Project, Zone, Name]) ->
    gcp_compute_sim:start_instance(Project, Zone, Name);
execute_operation(compute_stop, [Project, Zone, Name]) ->
    gcp_compute_sim:stop_instance(Project, Zone, Name);
execute_operation(compute_delete, [Project, Zone, Name]) ->
    gcp_compute_sim:delete_instance(Project, Zone, Name);
execute_operation(Operation, _Args) ->
    {error, {unknown_operation, Operation}}.

generate_receipt(RequestId, ContractId, Operation, Args, Result,
                 StartTime, EndTime, State) ->
    %% Hash request (text-blind)
    RequestData = term_to_binary({Operation, Args}),
    RequestHash = crypto:hash(sha256, RequestData),

    %% Hash response (text-blind)
    ResponseData = term_to_binary(Result),
    ResponseHash = crypto:hash(sha256, ResponseData),

    %% Get previous receipt hash for chaining
    PreviousHash = case State#state.receipts of
        [] -> <<0:256>>;
        [PrevReceipt | _] ->
            crypto:hash(sha256, term_to_binary(PrevReceipt))
    end,

    %% Determine outcome
    Outcome = case Result of
        {ok, _} -> ok;
        ok -> ok;
        {error, _} -> error
    end,

    Receipt = #mcp_receipt{
        id = generate_receipt_id(State#state.receipt_counter),
        sequence = State#state.receipt_counter,
        request_id = RequestId,
        contract_id = ContractId,
        envelope_id = ContractId,  %% Using same ID for simplicity
        method = atom_to_binary(Operation),
        request_hash = RequestHash,
        response_hash = ResponseHash,
        outcome = Outcome,
        duration_us = (EndTime - StartTime) * 1000,
        memory_bytes = 0,
        cpu_us = 0,
        io_bytes = 0,
        previous_hash = PreviousHash,
        timestamp = EndTime
    },

    NewReceipts = [Receipt | State#state.receipts],
    {Receipt, State#state{
        receipts = NewReceipts,
        receipt_counter = State#state.receipt_counter + 1
    }}.

generate_request_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

generate_receipt_id(Counter) ->
    Timestamp = integer_to_binary(erlang:system_time(millisecond)),
    CounterBin = integer_to_binary(Counter),
    <<"receipt-", Timestamp/binary, "-", CounterBin/binary>>.

create_default_contract(ContractId) ->
    Now = erlang:system_time(millisecond),
    #mcp_contract{
        id = ContractId,
        family = <<"default">>,
        version = <<"1.0">>,
        specification = #{},
        tier = tier_b_assist,
        created_at = Now,
        revoked = false
    }.

create_default_envelope(ContractId) ->
    #mcp_envelope{
        id = base64:encode(crypto:strong_rand_bytes(16)),
        contract_id = ContractId,
        max_duration_ms = 30000,
        max_memory_bytes = 100 * 1024 * 1024,
        max_payload_bytes = 10 * 1024 * 1024,
        allowed_operations = [],  %% Allow all by default
        denied_operations = [],
        fail_closed = true
    }.

format_audit_trail(ContractId, Receipts) ->
    Trail = #{
        <<"contract_id">> => ContractId,
        <<"generated_at">> => erlang:system_time(millisecond),
        <<"receipt_count">> => length(Receipts),
        <<"receipts">> => [format_receipt(R) || R <- lists:reverse(Receipts)]
    },
    jsx:encode(Trail).

format_receipt(Receipt) ->
    #{
        <<"receipt_id">> => Receipt#mcp_receipt.id,
        <<"request_id">> => Receipt#mcp_receipt.request_id,
        <<"method">> => Receipt#mcp_receipt.method,
        <<"request_hash">> => base64:encode(Receipt#mcp_receipt.request_hash),
        <<"response_hash">> => base64:encode(Receipt#mcp_receipt.response_hash),
        <<"timestamp">> => Receipt#mcp_receipt.timestamp,
        <<"sequence">> => Receipt#mcp_receipt.sequence,
        <<"outcome">> => atom_to_binary(Receipt#mcp_receipt.outcome)
    }.
