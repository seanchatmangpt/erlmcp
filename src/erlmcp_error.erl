%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Error Handling Module with Context Information and Categorization
%%%
%%% This module provides comprehensive error handling for erlmcp systems
%%% designed to support 100K concurrent operations with:
%%%
%%% 1. Structured error context (node, connection, operation, timestamp)
%%% 2. Error categorization (transient vs permanent, retryable vs not)
%%% 3. Severity levels (debug, info, warning, error, critical)
%%% 4. Stack traces and diagnostic information
%%% 5. Error recovery helpers
%%% 6. Efficient logging at scale
%%%
%%% Error Context Structure:
%%%   #{
%%%       node => atom(),           % Erlang node
%%%       connection_id => term(),  % Connection/client identifier
%%%       operation => atom(),      % Current operation
%%%       timestamp => integer(),   % Millisecond timestamp
%%%       error_id => binary(),     % Unique error ID for tracing
%%%       user_id => term(),        % Optional user/session identifier
%%%       request_id => term(),     % Optional request identifier
%%%       stack_trace => term()     % Exception stack trace
%%%   }
%%%
%%% Error Categorization:
%%%   - transient: Temporary, may succeed on retry (connection issues, timeouts)
%%%   - permanent: Persistent, will not succeed on retry (not found, permission)
%%%   - unknown: Cannot determine category
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_error).

-export([
    % Context creation and management
    new_context/1, new_context/2,
    add_context/3,
    get_context/2,
    context_to_map/1,

    % Error creation with context
    error/2, error/3, error/4,
    wrap_error/2, wrap_error/3,

    % Error categorization
    categorize/1,
    is_retryable/1,
    is_transient/1,

    % Error logging
    log_error/2, log_error/3, log_error/4,
    log_error_with_context/3, log_error_with_context/4,

    % Error recovery helpers
    should_retry/1, should_retry/2,
    backoff_delay/2,
    extract_error_info/1,

    % Batch error metrics
    start_error_collector/0,
    collect_error/2,
    get_error_stats/0,
    reset_error_stats/0,

    % Diagnostics
    error_to_string/1,
    explain_error/1
]).

% Internal exports for helper functions
-export([
    generate_error_id/0,
    map_exception_to_code/1,
    categorize_code/1,
    categorize_reason/1,
    detect_severity/1,
    extract_context_from_state/1,
    format_term/1,
    send_to_collector/1,
    error_collector_loop/0
]).

-include("erlmcp.hrl").

%% Types
-type error_context() :: #{
    node => atom(),
    connection_id => term(),
    operation => atom(),
    timestamp => integer(),
    error_id => binary(),
    user_id => term(),
    request_id => term(),
    stack_trace => list(),
    phase => atom(),
    transport => atom(),
    active_operations => integer()
}.

-type error_category() :: transient | permanent | unknown.

-type error_info() :: {
    Code :: integer(),
    Message :: binary(),
    Data :: map(),
    Context :: error_context()
}.

-type severity_level() :: debug | info | warning | error | critical.

-type recovery_action() :: retry | backoff | circuit_break | fail | ignore.

-export_type([error_context/0, error_category/0, error_info/0, severity_level/0, recovery_action/0]).

%% ETS table for error statistics (process local, per connection)
-define(ERROR_STATS_TABLE, erlmcp_error_stats).
-define(ERROR_COLLECTOR_PID, erlmcp_error_collector).

%%====================================================================
%% Context Management
%%====================================================================

%% @doc Create a new error context with default values
-spec new_context(atom()) -> error_context().
new_context(Operation) ->
    new_context(Operation, #{}).

%% @doc Create a new error context with custom fields
-spec new_context(atom(), map()) -> error_context().
new_context(Operation, Fields) ->
    Defaults = #{
        node => node(),
        connection_id => undefined,
        operation => Operation,
        timestamp => erlang:system_time(millisecond),
        error_id => generate_error_id(),
        user_id => undefined,
        request_id => undefined,
        stack_trace => [],
        phase => unknown,
        transport => unknown,
        active_operations => 0
    },
    maps:merge(Defaults, Fields).

%% @doc Add or update a context field
-spec add_context(error_context(), atom(), term()) -> error_context().
add_context(Context, Key, Value) ->
    maps:put(Key, Value, Context).

%% @doc Get a context field with default value
-spec get_context(error_context(), atom()) -> term().
get_context(Context, Key) ->
    maps:get(Key, Context, undefined).

%% @doc Convert context to displayable map
-spec context_to_map(error_context()) -> map().
context_to_map(Context) ->
    maps:map(fun
        (stack_trace, _) -> <<"<stack trace>">>;
        (_, V) -> V
    end, Context).

%%====================================================================
%% Error Creation
%%====================================================================

%% @doc Create an error with code, message, and context
-spec error(integer(), binary()) -> {error, error_info()}.
error(Code, Message) ->
    error(Code, Message, #{}, new_context(unknown)).

%% @doc Create an error with data and context
-spec error(integer(), binary(), map()) -> {error, error_info()}.
error(Code, Message, Data) ->
    error(Code, Message, Data, new_context(unknown)).

%% @doc Create an error with full context
-spec error(integer(), binary(), map(), error_context()) -> {error, error_info()}.
error(Code, Message, Data, Context) ->
    {error, {Code, Message, Data, Context}}.

%% @doc Wrap an existing error with context
-spec wrap_error(term(), atom()) -> {error, error_info()}.
wrap_error(Error, Operation) ->
    wrap_error(Error, Operation, new_context(Operation)).

%% @doc Wrap an existing error with operation and context
-spec wrap_error(term(), atom(), error_context()) -> {error, error_info()}.
wrap_error({error, {Code, Message, Data}}, _Op, Context) ->
    {error, {Code, Message, Data, Context}};
wrap_error({error, {Code, Message}}, _Op, Context) ->
    {error, {Code, Message, #{}, Context}};
wrap_error({error, Reason}, Op, Context) ->
    Code = map_exception_to_code(Reason),
    Message = atom_to_binary(Reason, utf8),
    {error, {Code, Message, #{original => Reason}, add_context(Context, operation, Op)}};
wrap_error(Other, Op, Context) ->
    Code = ?JSONRPC_INTERNAL_ERROR,
    Message = <<"Internal error">>,
    {error, {Code, Message, #{reason => format_term(Other)}, add_context(Context, operation, Op)}}.

%%====================================================================
%% Error Categorization
%%====================================================================

%% @doc Categorize an error by code and reason
-spec categorize(term()) -> error_category().
categorize({error, {Code, _Msg, _Data, _Context}}) ->
    categorize_code(Code);
categorize({error, {Code, _Msg, _Data}}) ->
    categorize_code(Code);
categorize({error, {Code, _Msg}}) ->
    categorize_code(Code);
categorize({error, Reason}) ->
    categorize_reason(Reason);
categorize(Code) when is_integer(Code) ->
    categorize_code(Code);
categorize(_) ->
    unknown.

%% @doc Check if error is retryable
-spec is_retryable(term()) -> boolean().
is_retryable(Error) ->
    case categorize(Error) of
        transient -> true;
        _ -> false
    end.

%% @doc Check if error is transient
-spec is_transient(term()) -> boolean().
is_transient(Error) ->
    categorize(Error) =:= transient.

%%====================================================================
%% Error Logging with Structured Output
%%====================================================================

%% @doc Log an error with automatic severity detection
-spec log_error(term(), error_context()) -> ok.
log_error(Error, Context) ->
    Severity = detect_severity(Error),
    log_error(Error, Context, Severity, #{}).

%% @doc Log an error with specified severity
-spec log_error(term(), error_context(), severity_level()) -> ok.
log_error(Error, Context, Severity) ->
    log_error(Error, Context, Severity, #{}).

%% @doc Log an error with severity and extra fields
-spec log_error(term(), error_context(), severity_level(), map()) -> ok.
log_error(Error, Context, Severity, Extra) ->
    ErrorId = maps:get(error_id, Context, undefined),
    ErrorMsg = error_to_string(Error),
    Category = categorize(Error),
    ConnectionId = maps:get(connection_id, Context, unknown),
    Operation = maps:get(operation, Context, unknown),
    Node = maps:get(node, Context, node()),
    Timestamp = maps:get(timestamp, Context, erlang:system_time(millisecond)),

    LogEntry = maps:merge(
        #{
            error_id => ErrorId,
            error => ErrorMsg,
            category => Category,
            severity => Severity,
            connection_id => ConnectionId,
            operation => Operation,
            node => Node,
            timestamp => Timestamp,
            context => context_to_map(Context)
        },
        Extra
    ),

    % Send to error collector for batch stats
    catch send_to_collector({log, LogEntry}),

    % Log to OTP logger with structured output
    case Severity of
        critical ->
            logger:critical(
                "CRITICAL ERROR: ~s [~w]",
                [ErrorId, Operation],
                LogEntry
            );
        error ->
            logger:error(
                "ERROR: ~s [~w]",
                [ErrorId, Operation],
                LogEntry
            );
        warning ->
            logger:warning(
                "WARNING: ~s [~w]",
                [ErrorId, Operation],
                LogEntry
            );
        info ->
            logger:info(
                "INFO: ~s [~w]",
                [ErrorId, Operation],
                LogEntry
            );
        debug ->
            logger:debug(
                "DEBUG: ~s [~w]",
                [ErrorId, Operation],
                LogEntry
            )
    end,

    ok.

%% @doc Log error with automatic context from process state
-spec log_error_with_context(term(), severity_level(), map()) -> ok.
log_error_with_context(Error, Severity, State) ->
    Context = extract_context_from_state(State),
    log_error(Error, Context, Severity).

%% @doc Log error with severity and extra fields from state
-spec log_error_with_context(term(), severity_level(), map(), map()) -> ok.
log_error_with_context(Error, Severity, State, Extra) ->
    Context = extract_context_from_state(State),
    log_error(Error, Context, Severity, Extra).

%%====================================================================
%% Error Recovery Helpers
%%====================================================================

%% @doc Determine if error should be retried (boolean shorthand)
-spec should_retry(term()) -> boolean().
should_retry(Error) ->
    should_retry(Error, 3).

%% @doc Determine if error should be retried with attempt count
-spec should_retry(term(), integer()) -> boolean().
should_retry(Error, Attempt) when Attempt > 0 ->
    is_retryable(Error);
should_retry(_Error, _Attempt) ->
    false.

%% @doc Calculate exponential backoff delay in milliseconds
-spec backoff_delay(integer(), integer()) -> integer().
backoff_delay(Attempt, MaxAttempts) when Attempt < MaxAttempts ->
    BaseDelay = 100,
    MaxDelay = 30000,
    Delay = min(BaseDelay * (2 bsl (Attempt - 1)), MaxDelay),
    % Add jitter to prevent thundering herd
    Jitter = crypto:strong_rand_range(Delay div 4),
    Delay + Jitter;
backoff_delay(_Attempt, _MaxAttempts) ->
    0.

%% @doc Extract useful error information
-spec extract_error_info(term()) -> map().
extract_error_info({error, {Code, Message, Data, Context}}) ->
    #{
        code => Code,
        message => Message,
        data => Data,
        context => context_to_map(Context),
        category => categorize_code(Code),
        retryable => is_retryable({error, Code})
    };
extract_error_info({error, {Code, Message, Data}}) ->
    #{
        code => Code,
        message => Message,
        data => Data,
        category => categorize_code(Code),
        retryable => is_retryable({error, Code})
    };
extract_error_info({error, Reason}) ->
    #{
        reason => format_term(Reason),
        category => categorize_reason(Reason),
        retryable => is_retryable({error, Reason})
    };
extract_error_info(Other) ->
    #{
        error => format_term(Other),
        category => unknown,
        retryable => false
    }.

%%====================================================================
%% Batch Error Metrics (for 100K scale)
%%====================================================================

%% @doc Start error collector process
-spec start_error_collector() -> {ok, pid()} | {error, already_started}.
start_error_collector() ->
    case whereis(?ERROR_COLLECTOR_PID) of
        undefined ->
            Pid = spawn(fun error_collector_loop/0),
            try
                register(?ERROR_COLLECTOR_PID, Pid),
                {ok, Pid}
            catch
                error:_ -> {error, already_started}
            end;
        Pid ->
            {error, already_started}
    end.

%% @doc Collect error for statistics
-spec collect_error(atom(), integer()) -> ok.
collect_error(Category, Count) ->
    catch send_to_collector({collect, Category, Count}),
    ok.

%% @doc Get error statistics
-spec get_error_stats() -> map().
get_error_stats() ->
    catch send_to_collector({get_stats, self()}),
    receive
        {stats, Stats} -> Stats
    after 1000 -> #{}
    end.

%% @doc Reset error statistics
-spec reset_error_stats() -> ok.
reset_error_stats() ->
    catch send_to_collector(reset),
    ok.

%% Internal error collector loop
error_collector_loop() ->
    error_collector_loop(#{
        transient => 0,
        permanent => 0,
        unknown => 0,
        total => 0,
        logs => []
    }).

error_collector_loop(Stats) ->
    receive
        {collect, Category, Count} ->
            NewStats = maps:update_with(Category, fun(V) -> V + Count end, Count, Stats),
            UpdatedStats = NewStats#{total => maps:get(total, NewStats, 0) + Count},
            error_collector_loop(UpdatedStats);

        {log, LogEntry} ->
            Logs = maps:get(logs, Stats, []),
            % Keep last 1000 logs for debugging
            NewLogs = lists:sublist([LogEntry | Logs], 1000),
            error_collector_loop(Stats#{logs => NewLogs});

        {get_stats, From} ->
            From ! {stats, Stats},
            error_collector_loop(Stats);

        reset ->
            error_collector_loop(#{
                transient => 0,
                permanent => 0,
                unknown => 0,
                total => 0,
                logs => []
            });

        _Other ->
            error_collector_loop(Stats)
    end.

%%====================================================================
%% Diagnostics and Display
%%====================================================================

%% @doc Convert error to human-readable string
-spec error_to_string(term()) -> binary().
error_to_string({error, {Code, Message, _Data, _Context}}) ->
    iolist_to_binary([atom_to_binary(Code, utf8), <<": ">>, Message]);
error_to_string({error, {Code, Message, _Data}}) ->
    iolist_to_binary([atom_to_binary(Code, utf8), <<": ">>, Message]);
error_to_string({error, {Code, Message}}) ->
    iolist_to_binary([atom_to_binary(Code, utf8), <<": ">>, Message]);
error_to_string({error, Reason}) ->
    iolist_to_binary(["Error: ", format_term(Reason)]);
error_to_string(Term) ->
    iolist_to_binary(["Unknown error: ", format_term(Term)]).

%% @doc Explain error in detail with recovery suggestions
-spec explain_error(term()) -> binary().
explain_error({error, {Code, Message, Data, Context}}) ->
    OpBin = atom_to_binary(maps:get(operation, Context, unknown), utf8),
    ConnBin = format_term(maps:get(connection_id, Context, unknown)),
    NodeBin = atom_to_binary(maps:get(node, Context, node()), utf8),

    Explanation = case categorize_code(Code) of
        transient ->
            <<"Transient error (may succeed on retry). ">>;
        permanent ->
            <<"Permanent error (will not succeed on retry). ">>;
        unknown ->
            <<"Unknown error category. ">>
    end,

    Recovery = case Code of
        ?MCP_ERROR_TIMEOUT ->
            <<"Consider increasing timeout or checking service health.">>;
        ?MCP_ERROR_RESOURCE_NOT_FOUND ->
            <<"Resource not available. Check URI and availability.">>;
        ?MCP_ERROR_RATE_LIMITED ->
            <<"Rate limited. Implement backoff strategy.">>;
        ?JSONRPC_INVALID_PARAMS ->
            <<"Invalid parameters. Check request structure.">>;
        _ ->
            <<"Check logs for details.">>
    end,

    iolist_to_binary([
        Explanation, <<"\n">>,
        <<"Message: ">>, Message, <<"\n">>,
        <<"Operation: ">>, OpBin, <<"\n">>,
        <<"Connection: ">>, ConnBin, <<"\n">>,
        <<"Node: ">>, NodeBin, <<"\n">>,
        <<"Recovery: ">>, Recovery, <<"\n">>,
        case maps:size(Data) > 0 of
            true -> iolist_to_binary([<<"Data: ">>, format_term(Data)]);
            false -> <<"">>
        end
    ]);
explain_error(Error) ->
    iolist_to_binary([<<"Explanation not available for error: ">>, format_term(Error)]).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Generate unique error ID for tracing
generate_error_id() ->
    % Format: erlmcp-<node>-<timestamp>-<random>
    Node = atom_to_binary(node(), utf8),
    Ts = integer_to_binary(erlang:system_time(millisecond) rem 1000000),
    Rand = base64:encode(crypto:strong_rand_bytes(4)),
    iolist_to_binary([<<"erlmcp-">>, Node, <<"-">>, Ts, <<"-">>, Rand]).

%% @doc Map exception types to JSON-RPC error codes
map_exception_to_code(timeout) ->
    ?MCP_ERROR_TIMEOUT;
map_exception_to_code(not_found) ->
    ?MCP_ERROR_RESOURCE_NOT_FOUND;
map_exception_to_code(badarg) ->
    ?JSONRPC_INVALID_PARAMS;
map_exception_to_code(function_clause) ->
    ?JSONRPC_INVALID_PARAMS;
map_exception_to_code(noproc) ->
    ?JSONRPC_INTERNAL_ERROR;
map_exception_to_code(_) ->
    ?JSONRPC_INTERNAL_ERROR.

%% @doc Categorize by error code
categorize_code(Code) ->
    case Code of
        % Transient errors
        ?MCP_ERROR_TIMEOUT -> transient;
        ?MCP_ERROR_TRANSPORT_ERROR -> transient;
        ?MCP_ERROR_RATE_LIMITED -> transient;

        % Permanent errors
        ?MCP_ERROR_RESOURCE_NOT_FOUND -> permanent;
        ?MCP_ERROR_TOOL_NOT_FOUND -> permanent;
        ?MCP_ERROR_PROMPT_NOT_FOUND -> permanent;
        ?MCP_ERROR_NOT_INITIALIZED -> permanent;
        ?JSONRPC_INVALID_REQUEST -> permanent;
        ?JSONRPC_INVALID_PARAMS -> permanent;
        ?JSONRPC_METHOD_NOT_FOUND -> permanent;

        % Unknown
        _ -> unknown
    end.

%% @doc Categorize by exception reason
categorize_reason(timeout) ->
    transient;
categorize_reason(econnrefused) ->
    transient;
categorize_reason(econnreset) ->
    transient;
categorize_reason(enetunreach) ->
    transient;
categorize_reason(ehostunreach) ->
    transient;
categorize_reason(eaddrinuse) ->
    transient;
categorize_reason(not_found) ->
    permanent;
categorize_reason(badarg) ->
    permanent;
categorize_reason(function_clause) ->
    permanent;
categorize_reason(undefined_behavior) ->
    permanent;
categorize_reason(_) ->
    unknown.

%% @doc Detect error severity from code
detect_severity(Error) ->
    case categorize(Error) of
        unknown ->
            critical;  % Unknown errors are critical
        permanent ->
            error;  % Permanent errors are errors
        transient ->
            warning  % Transient errors are warnings
    end.

%% @doc Extract context from gen_server state (common pattern)
extract_context_from_state(State) when is_map(State) ->
    new_context(
        maps:get(operation, State, unknown),
        #{
            connection_id => maps:get(client_id, State, undefined),
            user_id => maps:get(user_id, State, undefined),
            request_id => maps:get(request_id, State, undefined),
            phase => maps:get(phase, State, unknown),
            transport => maps:get(transport, State, unknown),
            active_operations => maps:get(pending_requests, State, undefined)
        }
    );
extract_context_from_state(_) ->
    new_context(unknown).

%% @doc Format term for logging
format_term(Term) ->
    case Term of
        B when is_binary(B) -> B;
        S when is_list(S) -> iolist_to_binary(S);
        A when is_atom(A) -> atom_to_binary(A, utf8);
        I when is_integer(I) -> integer_to_binary(I);
        _ -> iolist_to_binary(io_lib:format("~p", [Term]))
    end.

%% @doc Send message to error collector safely
-spec send_to_collector(term()) -> ok.

%% @doc Send message to error collector safely
send_to_collector(Msg) ->
    case whereis(?ERROR_COLLECTOR_PID) of
        undefined -> ok;
        CollectorPid -> CollectorPid ! Msg, ok
    end.
