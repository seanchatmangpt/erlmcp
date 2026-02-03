%%%-------------------------------------------------------------------
%%% @doc PQC Events - Event Handling and Rate-Limited Printing
%%%
%%% Handles event emission, logging, and rate-limited output for PQChain workflow cases.
%%%
%%% Features:
%%% - Rate-limited printing on status boundaries (failed, input_required, auth_required, completed)
%%% - Event emission to pg subscribers with per-case rate limiting
%%% - Token bucket rate limiter (status: unlimited, artifacts: 100/s, debug: 10/s)
%%% - Structured logging with metadata
%%% - JSON serialization for events
%%% - Human-readable event formatting
%%%
%%% Architecture:
%%% - Uses pg (process groups) for pub/sub event broadcasting
%%% - ETS-backed rate limiter state per case
%%% - Stateless event formatting and serialization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_events).

-include("pqchain.hrl").

%% API exports
-export([
    maybe_print/1,
    emit/3,
    subscribe/1,
    unsubscribe/1,
    format_event/1,
    to_json/1,
    from_json/1,
    get_rate_limiter/1,
    check_rate/2
]).

%% Internal exports for testing
-export([
    init_rate_limiter/0,
    update_token_bucket/2
]).

%%====================================================================
%% Type definitions
%%====================================================================

-type case_id() :: binary().
-type event_type() :: status | artifact | transition_fired | tool_called |
                      signal_received | case_terminated | debug.
-type event_data() :: map().
-type event() :: #{
    type := event_type(),
    case_id := case_id(),
    timestamp := non_neg_integer(),
    data := event_data()
}.

-type rate_limiter_state() :: #{
    status => token_bucket(),
    artifact => token_bucket(),
    debug => token_bucket()
}.

-type token_bucket() :: #{
    tokens := float(),
    capacity := float(),
    rate := float(),  % tokens per second
    last_update := non_neg_integer()  % erlang:system_time(millisecond)
}.

-export_type([event/0, event_type/0, rate_limiter_state/0]).

%%====================================================================
%% Constants
%%====================================================================

%% Rate limits (tokens per second)
-define(RATE_STATUS, unlimited).     % Status changes: unlimited
-define(RATE_ARTIFACT, 100.0).       % Artifacts: 100/second
-define(RATE_DEBUG, 10.0).           % Debug events: 10/second

%% Token bucket capacities (burst size)
-define(CAPACITY_ARTIFACT, 200.0).
-define(CAPACITY_DEBUG, 20.0).

%% pg scope for event broadcasting
-define(PG_SCOPE, pqc_case_registry).

%% ETS table for rate limiters
-define(RATE_LIMITER_TABLE, pqc_rate_limiters).

%% Status states that trigger printing
-define(PRINT_STATES, [failed, input_required, auth_required, completed, cancelled]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Rate-limited printing on status boundaries.
%% Prints when status transitions to: failed, input_required, auth_required, completed.
-spec maybe_print(CaseData :: map()) -> ok.
maybe_print(#{case_id := CaseId, status := Status} = CaseData) ->
    State = maps:get(state, Status, undefined),
    Message = maps:get(message, Status, undefined),

    case lists:member(State, ?PRINT_STATES) of
        true ->
            %% Get additional context
            Marking = maps:get(marking, CaseData, #{}),
            ReceiptHead = maps:get(receipt_head, CaseData, <<>>),

            %% Log with structured metadata
            logger:info("Case status boundary reached", #{
                case_id => CaseId,
                status_state => State,
                status_message => Message,
                marking => Marking,
                receipt_head => format_hash(ReceiptHead),
                timestamp => erlang:system_time(millisecond)
            }),

            %% Emit status event
            emit(CaseId, status, Status);
        false ->
            ok
    end;
maybe_print(_CaseData) ->
    ok.

%% @doc Emit event to pg subscribers with rate limiting.
%% Applies per-case, per-type rate limiting using token bucket algorithm.
-spec emit(CaseId :: case_id(), EventType :: event_type(), EventData :: event_data()) -> ok.
emit(CaseId, EventType, EventData) when is_binary(CaseId), is_map(EventData) ->
    Now = erlang:system_time(millisecond),

    %% Get rate limiter state for this case
    RateLimiter = get_rate_limiter(CaseId),

    %% Check if event should be emitted
    case check_rate(RateLimiter, EventType) of
        {allow, NewRateLimiter} ->
            %% Update rate limiter state
            store_rate_limiter(CaseId, NewRateLimiter),

            %% Create event
            Event = #{
                type => EventType,
                case_id => CaseId,
                timestamp => Now,
                data => EventData
            },

            %% Log event
            log_event(Event),

            %% Broadcast to pg subscribers
            broadcast_event(CaseId, Event),

            ok;
        {deny, _NewRateLimiter} ->
            %% Rate limit exceeded, drop event silently
            ok
    end.

%% @doc Subscribe to events for a case.
-spec subscribe(CaseId :: case_id()) -> ok.
subscribe(CaseId) when is_binary(CaseId) ->
    try
        pg:join(?PG_SCOPE, CaseId, self()),
        ok
    catch
        error:Reason ->
            logger:warning("Failed to subscribe to case ~s: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%% @doc Unsubscribe from case events.
-spec unsubscribe(CaseId :: case_id()) -> ok.
unsubscribe(CaseId) when is_binary(CaseId) ->
    try
        pg:leave(?PG_SCOPE, CaseId, self()),
        ok
    catch
        error:Reason ->
            logger:warning("Failed to unsubscribe from case ~s: ~p", [CaseId, Reason]),
            {error, Reason}
    end.

%% @doc Format event as human-readable string.
-spec format_event(Event :: event()) -> iolist().
format_event(#{type := Type, case_id := CaseId, timestamp := Ts, data := Data}) ->
    FormattedTs = format_timestamp(Ts),
    FormattedData = format_event_data(Type, Data),
    io_lib:format("[~s] Case ~s - ~s: ~s", [FormattedTs, format_case_id(CaseId), Type, FormattedData]).

%% @doc Convert event to JSON-serializable map.
-spec to_json(Event :: event()) -> map().
to_json(#{type := Type, case_id := CaseId, timestamp := Ts, data := Data} = Event) ->
    #{
        <<"type">> => atom_to_binary(Type, utf8),
        <<"case_id">> => CaseId,
        <<"timestamp">> => Ts,
        <<"data">> => data_to_json(Type, Data)
    }.

%% @doc Parse event from JSON map.
-spec from_json(JsonMap :: map()) -> event().
from_json(#{<<"type">> := TypeBin, <<"case_id">> := CaseId,
            <<"timestamp">> := Ts, <<"data">> := DataJson}) ->
    Type = binary_to_existing_atom(TypeBin, utf8),
    Data = data_from_json(Type, DataJson),
    #{
        type => Type,
        case_id => CaseId,
        timestamp => Ts,
        data => Data
    }.

%% @doc Get rate limiter state for a case (token bucket).
%% Initializes if not present.
-spec get_rate_limiter(CaseId :: case_id()) -> rate_limiter_state().
get_rate_limiter(CaseId) when is_binary(CaseId) ->
    ensure_table_exists(),

    case ets:lookup(?RATE_LIMITER_TABLE, CaseId) of
        [{CaseId, RateLimiter}] ->
            RateLimiter;
        [] ->
            %% Initialize new rate limiter
            RateLimiter = init_rate_limiter(),
            ets:insert(?RATE_LIMITER_TABLE, {CaseId, RateLimiter}),
            RateLimiter
    end.

%% @doc Check if event should be emitted based on rate limits.
%% Returns {allow | deny, NewState}.
-spec check_rate(RateLimiterState :: rate_limiter_state(), EventType :: event_type()) ->
    {allow | deny, rate_limiter_state()}.
check_rate(RateLimiter, status) ->
    %% Status events are unlimited
    {allow, RateLimiter};
check_rate(RateLimiter, artifact) ->
    check_token_bucket(RateLimiter, artifact);
check_rate(RateLimiter, debug) ->
    check_token_bucket(RateLimiter, debug);
check_rate(RateLimiter, _OtherType) ->
    %% Other event types: use debug rate limit
    check_token_bucket(RateLimiter, debug).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize rate limiter state for a case
-spec init_rate_limiter() -> rate_limiter_state().
init_rate_limiter() ->
    Now = erlang:system_time(millisecond),
    #{
        artifact => #{
            tokens => ?CAPACITY_ARTIFACT,
            capacity => ?CAPACITY_ARTIFACT,
            rate => ?RATE_ARTIFACT,
            last_update => Now
        },
        debug => #{
            tokens => ?CAPACITY_DEBUG,
            capacity => ?CAPACITY_DEBUG,
            rate => ?RATE_DEBUG,
            last_update => Now
        }
    }.

%% @doc Check token bucket and update state
-spec check_token_bucket(RateLimiter :: rate_limiter_state(), BucketType :: atom()) ->
    {allow | deny, rate_limiter_state()}.
check_token_bucket(RateLimiter, BucketType) ->
    Bucket = maps:get(BucketType, RateLimiter),

    %% Update tokens based on elapsed time
    UpdatedBucket = update_token_bucket(Bucket, erlang:system_time(millisecond)),
    Tokens = maps:get(tokens, UpdatedBucket),

    if
        Tokens >= 1.0 ->
            %% Consume 1 token
            NewBucket = UpdatedBucket#{tokens => Tokens - 1.0},
            NewRateLimiter = RateLimiter#{BucketType => NewBucket},
            {allow, NewRateLimiter};
        true ->
            %% Not enough tokens
            NewRateLimiter = RateLimiter#{BucketType => UpdatedBucket},
            {deny, NewRateLimiter}
    end.

%% @doc Update token bucket based on elapsed time
-spec update_token_bucket(Bucket :: token_bucket(), Now :: non_neg_integer()) -> token_bucket().
update_token_bucket(#{tokens := Tokens, capacity := Capacity, rate := Rate, last_update := LastUpdate} = Bucket, Now) ->
    ElapsedMs = Now - LastUpdate,
    ElapsedSec = ElapsedMs / 1000.0,

    %% Add tokens based on rate
    NewTokens = min(Capacity, Tokens + (Rate * ElapsedSec)),

    Bucket#{
        tokens => NewTokens,
        last_update => Now
    }.

%% @doc Store rate limiter state for a case
-spec store_rate_limiter(CaseId :: case_id(), RateLimiter :: rate_limiter_state()) -> ok.
store_rate_limiter(CaseId, RateLimiter) ->
    ensure_table_exists(),
    ets:insert(?RATE_LIMITER_TABLE, {CaseId, RateLimiter}),
    ok.

%% @doc Ensure ETS table exists
-spec ensure_table_exists() -> ok.
ensure_table_exists() ->
    case ets:whereis(?RATE_LIMITER_TABLE) of
        undefined ->
            try
                ets:new(?RATE_LIMITER_TABLE, [named_table, public, set, {read_concurrency, true}]),
                ok
            catch
                error:badarg ->
                    %% Table already exists (race condition)
                    ok
            end;
        _Tid ->
            ok
    end.

%% @doc Log event with structured metadata
-spec log_event(Event :: event()) -> ok.
log_event(#{type := Type, case_id := CaseId, timestamp := Ts, data := Data}) ->
    case Type of
        status ->
            State = maps:get(state, Data, undefined),
            Message = maps:get(message, Data, undefined),
            logger:info("Case status changed", #{
                case_id => CaseId,
                status_state => State,
                status_message => Message,
                timestamp => Ts
            });
        artifact ->
            Name = maps:get(name, Data, undefined),
            logger:info("Artifact created", #{
                case_id => CaseId,
                artifact_name => Name,
                timestamp => Ts
            });
        transition_fired ->
            Tid = maps:get(transition_id, Data, undefined),
            logger:debug("Transition fired", #{
                case_id => CaseId,
                transition_id => Tid,
                timestamp => Ts
            });
        tool_called ->
            ToolName = maps:get(tool, Data, undefined),
            logger:debug("Tool called", #{
                case_id => CaseId,
                tool_name => ToolName,
                timestamp => Ts
            });
        signal_received ->
            SignalName = maps:get(name, Data, undefined),
            logger:debug("Signal received", #{
                case_id => CaseId,
                signal_name => SignalName,
                timestamp => Ts
            });
        case_terminated ->
            Reason = maps:get(reason, Data, undefined),
            logger:info("Case terminated", #{
                case_id => CaseId,
                reason => Reason,
                timestamp => Ts
            });
        _ ->
            logger:debug("Event emitted", #{
                case_id => CaseId,
                event_type => Type,
                timestamp => Ts
            })
    end.

%% @doc Broadcast event to pg subscribers
-spec broadcast_event(CaseId :: case_id(), Event :: event()) -> ok.
broadcast_event(CaseId, Event) ->
    try
        Members = pg:get_members(?PG_SCOPE, CaseId),
        [Pid ! {pqc_case_event, CaseId, Event} || Pid <- Members, is_pid(Pid)],
        ok
    catch
        error:Reason ->
            logger:warning("Failed to broadcast event for case ~s: ~p", [CaseId, Reason]),
            ok
    end.

%% @doc Format event data for display
-spec format_event_data(Type :: event_type(), Data :: event_data()) -> iolist().
format_event_data(status, Data) ->
    State = maps:get(state, Data, undefined),
    Message = maps:get(message, Data, <<"no message">>),
    io_lib:format("state=~p, message=~s", [State, Message]);
format_event_data(artifact, Data) ->
    Name = maps:get(name, Data, <<"unnamed">>),
    io_lib:format("artifact_name=~s", [Name]);
format_event_data(transition_fired, Data) ->
    Tid = maps:get(transition_id, Data, undefined),
    Effects = maps:get(effects, Data, []),
    io_lib:format("transition=~p, effects=~p", [Tid, Effects]);
format_event_data(tool_called, Data) ->
    Tool = maps:get(tool, Data, <<"unknown">>),
    Result = maps:get(result, Data, undefined),
    io_lib:format("tool=~s, result=~p", [Tool, Result]);
format_event_data(signal_received, Data) ->
    Name = maps:get(name, Data, <<"unnamed">>),
    io_lib:format("signal=~s", [Name]);
format_event_data(case_terminated, Data) ->
    Reason = maps:get(reason, Data, normal),
    io_lib:format("reason=~p", [Reason]);
format_event_data(_Type, Data) ->
    io_lib:format("~p", [Data]).

%% @doc Convert event data to JSON
-spec data_to_json(Type :: event_type(), Data :: event_data()) -> map().
data_to_json(_Type, Data) when is_map(Data) ->
    maps:fold(fun(K, V, Acc) ->
        JsonK = case is_atom(K) of
            true -> atom_to_binary(K, utf8);
            false -> K
        end,
        JsonV = term_to_json(V),
        Acc#{JsonK => JsonV}
    end, #{}, Data).

%% @doc Convert event data from JSON
-spec data_from_json(Type :: event_type(), DataJson :: map()) -> event_data().
data_from_json(_Type, DataJson) when is_map(DataJson) ->
    maps:fold(fun(K, V, Acc) ->
        ErlK = try binary_to_existing_atom(K, utf8) catch _:_ -> K end,
        ErlV = term_from_json(V),
        Acc#{ErlK => ErlV}
    end, #{}, DataJson).

%% @doc Convert term to JSON-safe term
-spec term_to_json(Term :: term()) -> term().
term_to_json(Term) when is_atom(Term) -> atom_to_binary(Term, utf8);
term_to_json(Term) when is_binary(Term) -> Term;
term_to_json(Term) when is_number(Term) -> Term;
term_to_json(Term) when is_list(Term) -> [term_to_json(T) || T <- Term];
term_to_json(Term) when is_map(Term) ->
    maps:fold(fun(K, V, Acc) ->
        JsonK = term_to_json(K),
        JsonV = term_to_json(V),
        Acc#{JsonK => JsonV}
    end, #{}, Term);
term_to_json(Term) -> term_to_binary(Term).

%% @doc Convert JSON term to Erlang term
-spec term_from_json(Term :: term()) -> term().
term_from_json(Term) when is_binary(Term) ->
    try binary_to_existing_atom(Term, utf8) catch _:_ -> Term end;
term_from_json(Term) when is_number(Term) -> Term;
term_from_json(Term) when is_list(Term) -> [term_from_json(T) || T <- Term];
term_from_json(Term) when is_map(Term) ->
    maps:fold(fun(K, V, Acc) ->
        ErlK = term_from_json(K),
        ErlV = term_from_json(V),
        Acc#{ErlK => ErlV}
    end, #{}, Term);
term_from_json(Term) -> Term.

%% @doc Format timestamp as ISO8601
-spec format_timestamp(Ts :: non_neg_integer()) -> binary().
format_timestamp(Ts) ->
    Seconds = Ts div 1000,
    Millis = Ts rem 1000,
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(Seconds, second),
    iolist_to_binary(
        io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                      [Year, Month, Day, Hour, Min, Sec, Millis])
    ).

%% @doc Format case ID (truncate for display)
-spec format_case_id(CaseId :: case_id()) -> binary().
format_case_id(CaseId) when byte_size(CaseId) > 16 ->
    <<Prefix:8/binary, _/binary>> = CaseId,
    <<Prefix/binary, "...">>;
format_case_id(CaseId) ->
    CaseId.

%% @doc Format hash (truncate and encode as hex)
-spec format_hash(Hash :: binary()) -> binary().
format_hash(<<>>) ->
    <<"(empty)">>;
format_hash(Hash) when byte_size(Hash) > 8 ->
    <<Prefix:4/binary, _/binary>> = Hash,
    HexPrefix = binary:encode_hex(Prefix, lowercase),
    <<HexPrefix/binary, "...">>;
format_hash(Hash) ->
    binary:encode_hex(Hash, lowercase).
