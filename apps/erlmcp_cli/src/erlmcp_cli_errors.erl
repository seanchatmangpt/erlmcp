%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_errors - CLI Error Handling
%%%
%%% Provides comprehensive error handling with proper MCP error codes
%%% (-32000 to -32099). Implements structured error responses and
%%% error recovery mechanisms.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_errors).

-behaviour(gen_server).

%% API
-export([start_link/0, handle_error/2, handle_error/3, create_error/2, create_error/3,
         create_error/4, get_error_code/1, get_error_message/1, is_retryable/1, should_retry/2,
         retry_with_backoff/2, log_error/2, record_error_metrics/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(error_state,
        {error_registry :: map(),               % Error code registry
         error_patterns :: map(),               % Error pattern matching
         recovery_strategies :: map(),          % Recovery strategies
         metrics :: map(),                       % Error metrics
         thresholds :: map(),                   % Error thresholds
         handlers :: list()}).                   % Error handlers

%% MCP Error Codes (-32000 to -32099)
-define(MCP_ERROR_CODES,
        #{%% -32000 to -32099: Internal errors
          -32000 => {internal_error, "Internal error"},
          -32001 => {internal_error, "Parse error"},
          -32002 => {internal_error, "Invalid request"},
          -32003 => {internal_error, "Method not found"},
          -32004 => {internal_error, "Invalid params"},
          -32005 => {internal_error, "Internal error"},
          -32006 => {internal_error, "Request limit exceeded"},
          -32007 => {internal_error, "Rate limit exceeded"},
          -32008 => {internal_error, "Permission denied"},
          -32009 => {internal_error, "Authentication failed"},
          -32010 => {internal_error, "Authorization failed"},
          -32011 => {internal_error, "Invalid configuration"},
          -32012 => {internal_error, "Resource not found"},
          -32013 => {internal_error, "Resource already exists"},
          -32014 => {internal_error, "Resource busy"},
          -32015 => {internal_error, "Resource conflict"},
          -32016 => {internal_error, "Resource limit exceeded"},
          -32017 => {internal_error, "Invalid operation"},
          -32018 => {internal_error, "Operation not supported"},
          -32019 => {internal_error, "Operation timeout"},
          -32020 => {internal_error, "Operation cancelled"},
          -32021 => {internal_error, "Operation failed"},
          -32022 => {internal_error, "Network error"},
          -32023 => {internal_error, "Connection error"},
          -32024 => {internal_error, "Protocol error"},
          -32025 => {internal_error, "Validation error"},
          -32026 => {internal_error, "Data error"},
          -32027 => {internal_error, "Encoding error"},
          -32028 => {internal_error, "Decoding error"},
          -32029 => {internal_error, "Serialization error"},
          -32030 => {internal_error, "Deserialization error"},
          -32031 => {internal_error, "Storage error"},
          -32032 => {internal_error, "Database error"},
          -32033 => {internal_error, "File system error"},
          -32034 => {internal_error, "Security error"},
          -32035 => {internal_error, "Encryption error"},
          -32036 => {internal_error, "Decryption error"},
          -32037 => {internal_error, "Signature error"},
          -32038 => {internal_error, "Certificate error"},
          -32039 => {internal_error, "Private key error"},
          -32040 => {internal_error, "Public key error"},
          -32041 => {internal_error, "Key error"},
          -32042 => {internal_error, "Certificate error"},
          -32043 => {internal_error, "SSL/TLS error"},
          -32044 => {internal_error, "WebSocket error"},
          -32045 => {internal_error, "HTTP error"},
          -32046 => {internal_error, "TCP error"},
          -32047 => {internal_error, "UDP error"},
          -32048 => {internal_error, "DNS error"},
          -32049 => {internal_error, "Network timeout"},
          -32050 => {internal_error, "Connection refused"},
          -32051 => {internal_error, "Connection timeout"},
          -32052 => {internal_error, "Connection reset"},
          -32053 => {internal_error, "Connection aborted"},
          -32054 => {internal_error, "Connection broken"},
          -32055 => {internal_error, "Connection lost"},
          -32056 => {internal_error, "Connection failed"},
          -32057 => {internal_error, "Network unreachable"},
          -32058 => {internal_error, "Host unreachable"},
          -32059 => {internal_error, "Network down"},
          -32060 => {internal_error, "Network error"},
          -32061 => {internal_error, "Service unavailable"},
          -32062 => {internal_error, "Service timeout"},
          -32063 => {internal_error, "Service overloaded"},
          -32064 => {internal_error, "Service down"},
          -32065 => {internal_error, "Service error"},
          -32066 => {internal_error, "Service not found"},
          -32067 => {internal_error, "Service unavailable"},
          -32068 => {internal_error, "Service timeout"},
          -32069 => {internal_error, "Service error"},
          -32070 => {internal_error, "Client error"},
          -32071 => {internal_error, "Server error"},
          -32072 => {internal_error, "Gateway error"},
          -32073 => {internal_error, "Proxy error"},
          -32074 => {internal_error, "Load balancer error"},
          -32075 => {internal_error, "CDN error"},
          -32076 => {internal_error, "Cache error"},
          -32077 => {internal_error, "Database error"},
          -32078 => {internal_error, "Query error"},
          -32079 => {internal_error, "Transaction error"},
          -32080 => {internal_error, "Rollback error"},
          -32081 => {internal_error, "Lock error"},
          -32082 => {internal_error, "Timeout error"},
          -32083 => {internal_error, "Deadlock error"},
          -32084 => {internal_error, "Connection pool error"},
          -32085 => {internal_error, "Pool exhausted"},
          -32086 => {internal_error, "Pool timeout"},
          -32087 => {internal_error, "Pool error"},
          -32088 => {internal_error, "Memory error"},
          -32089 => {internal_error, "Memory leak"},
          -32090 => {internal_error, "Memory exhausted"},
          -32091 => {internal_error, "Memory limit exceeded"},
          -32092 => {internal_error, "Disk error"},
          -32093 => {internal_error, "Disk full"},
          -32094 => {internal_error, "Disk space exhausted"},
          -32095 => {internal_error, "Disk I/O error"},
          -32096 => {internal_error, "File error"},
          -32097 => {internal_error, "File not found"},
          -32098 => {internal_error, "File permission denied"},
          -32099 => {internal_error, "File system error"}}).
%% Error categories
-define(ERROR_CATEGORIES,
        #{internal => {-32000, -32099},
          network => {-32022, -32075},
          resource => {-32012, -32021},
          security => {-32008, -32050},
          validation => {-32025, -32028},
          operation => {-32017, -32025},
          service => {-32062, -32076},
          database => {-32077, -32088},
          system => {-32088, -32099}}).
%% Default error thresholds
-define(DEFAULT_THRESHOLDS,
        #{retry_max_attempts => 3,
          retry_base_delay => 1000,
          retry_max_delay => 30000,
          error_rate_limit => 1000,
          error_log_threshold => 10,
          error_alert_threshold => 100}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the error handler
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Handle error with context
-spec handle_error(term(), term()) -> ok.
handle_error(Error, Context) ->
    handle_error(Error, Context, #{}).

%% @doc Handle error with context and options
-spec handle_error(term(), term(), map()) -> ok.
handle_error(Error, Context, Options) ->
    gen_server:cast(?SERVER, {handle_error, Error, Context, Options}).

%% @doc Create error response
-spec create_error(integer(), term()) -> map().
create_error(Code, Data) ->
    create_error(Code, Data, #{}).

%% @doc Create error response with metadata
-spec create_error(integer(), term(), map()) -> map().
create_error(Code, Data, Metadata) ->
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"error">> =>
          #{<<"code">> => Code,
            <<"message">> => get_error_message(Code),
            <<"data">> => #{<<"error">> => Data, <<"metadata">> => Metadata}},
      <<"id">> => null}.

%% @doc Create error response with cause
-spec create_error(integer(), term(), map(), term()) -> map().
create_error(Code, Data, Metadata, Cause) ->
    #{<<"jsonrpc">> => <<"2.0">>,
      <<"error">> =>
          #{<<"code">> => Code,
            <<"message">> => get_error_message(Code),
            <<"data">> =>
                #{<<"error">> => Data,
                  <<"metadata">> => Metadata,
                  <<"cause">> => Cause}},
      <<"id">> => null}.

%% @doc Get error code from error response
-spec get_error_code(map()) -> integer().
get_error_code(Response) ->
    maps:get(<<"code">>, maps:get(<<"error">>, Response), -32000).

%% @doc Get error message from error response
-spec get_error_message(map()) -> binary().
get_error_message(Response) ->
    case maps:get(<<"error">>, Response) of
        undefined ->
            <<"Internal error">>;
        Error ->
            maps:get(<<"message">>, Error, <<"Unknown error">>)
    end.

%% @doc Check if error is retryable
-spec is_retryable(term()) -> boolean().
is_retryable(Error) ->
    case Error of
        {error, Reason}
            when Reason == timeout;
                 Reason == connection_refused;
                 Reason == network_error;
                 Reason == service_unavailable ->
            true;
        {error, {network, Reason}} ->
            true;
        {error, {timeout, _}} ->
            true;
        {error, {retryable, _}} ->
            true;
        _ ->
            false
    end.

%% @doc Check if error should be retried
-spec should_retry(term(), integer()) -> boolean().
should_retry(Error, Attempts) ->
    is_retryable(Error) andalso Attempts > 0.

%% @doc Retry operation with backoff
-spec retry_with_backoff(fun(), integer()) -> {ok, term()} | {error, term()}.
retry_with_backoff(Operation, Attempts) ->
    retry_with_backoff(Operation, Attempts, 1, 1000).

%% @doc Log error
-spec log_error(term(), term()) -> ok.
log_error(Error, Context) ->
    gen_server:cast(?SERVER, {log_error, Error, Context}).

%% @doc Record error metrics
-spec record_error_metrics(term(), term()) -> ok.
record_error_metrics(Error, Context) ->
    gen_server:cast(?SERVER, {record_error_metrics, Error, Context}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the error handler
-spec init(term()) -> {ok, #error_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for error handler initialization
    erlmcp_otel:with_span("cli.error.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize error registry
                             ErrorRegistry = ?MCP_ERROR_CODES,

                             %% Initialize error patterns
                             ErrorPatterns = init_error_patterns(),

                             %% Initialize recovery strategies
                             RecoveryStrategies = init_recovery_strategies(),

                             %% Initialize metrics
                             Metrics = init_metrics(),

                             %% Initialize thresholds
                             Thresholds = ?DEFAULT_THRESHOLDS,

                             %% Initialize error handlers
                             Handlers = init_error_handlers(),

                             State =
                                 #error_state{error_registry = ErrorRegistry,
                                              error_patterns = ErrorPatterns,
                                              recovery_strategies = RecoveryStrategies,
                                              metrics = Metrics,
                                              thresholds = Thresholds,
                                              handlers = Handlers},

                             %% Start error cleanup timer
                             erlang:send_after(300000, self(), cleanup_errors), % 5 minutes

                             erlmcp_metrics:record("cli.error.initialized", 1),
                             {ok, State}
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #error_state{}) -> {reply, term(), #error_state{}}.
handle_call(get_error_registry, _From, State) ->
    {reply, State#error_state.error_registry, State};
handle_call(get_error_metrics, _From, State) ->
    {reply, State#error_state.metrics, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #error_state{}) -> {noreply, #error_state{}}.
handle_cast({handle_error, Error, Context, Options}, State) ->
    %% Create OTEL span for error handling
    SpanCtx =
        erlmcp_otel:inject_span("cli.error.handle",
                                #{<<"error_type">> => format_error_type(Error)},
                                undefined),

    try
        %% Log error
        log_error_internal(Error, Context, State),

        %% Record metrics
        record_error_metrics_internal(Error, Context, State),

        %% Handle error according to recovery strategy
        case get_recovery_strategy(Error, State) of
            {retry, Backoff} ->
                handle_retry_error(Error, Context, Backoff, State);
            {fallback, Fallback} ->
                handle_fallback_error(Error, Context, Fallback, State);
            {escalate, Severity} ->
                handle_escalate_error(Error, Context, Severity, State);
            {ignore, Reason} ->
                handle_ignore_error(Error, Context, Reason, State);
            {log_only, Reason} ->
                handle_log_only_error(Error, Context, Reason, State);
            unknown ->
                handle_unknown_error(Error, Context, State)
        end
    catch
        Error1:Reason1 ->
            lager:warning("Error handler failed: ~p:~p", [Error1, Reason1]),
            {noreply, State}
    end;
handle_cast({log_error, Error, Context}, State) ->
    log_error_internal(Error, Context, State),
    {noreply, State};
handle_cast({record_error_metrics, Error, Context}, State) ->
    record_error_metrics_internal(Error, Context, State),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info( term( ) , #error_state{ } ) -> { noreply , #error_state{ } } } .

handle_info(cleanup_errors, State) ->
    %% Create OTEL span for error cleanup
    SpanCtx = erlmcp_otel:inject_span("cli.error.cleanup", #{}, undefined),

    %% Clean up old error metrics
    Now = erlang:system_time(millisecond),
    CleanupThreshold = 86400000, % 24 hours

    OldMetrics =
        maps:filter(fun(_Error, Timestamp) -> Now - Timestamp > CleanupThreshold end,
                    State#error_state.metrics),

    erlmcp_otel:record_event(SpanCtx,
                             <<"errors.cleaned">>,
                             #{<<"cleaned_count">> => maps:size(OldMetrics)}),
    erlmcp_metrics:record("cli.error.cleanup.executed", 1),

    %% Schedule next cleanup
    erlang:send_after(300000, self(), cleanup_errors),

    {noreply, State#error_state{metrics = OldMetrics}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #error_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for error handler termination
    erlmcp_otel:with_span("cli.error.terminate",
                          #{<<"error_count">> => maps:size(State#error_state.metrics)},
                          fun() ->
                             %% Notify error handlers of shutdown
                             lists:foreach(fun(Handler) -> Handler ! {error_handler_shutdown} end,
                                           State#error_state.handlers),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.error.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #error_state{}, term()) -> {ok, #error_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize error patterns
-spec init_error_patterns() -> map().
init_error_patterns() ->
    #{{network, timeout} => {-32049, retryable},
      {network, connection_refused} => {-32050, retryable},
      {network, connection_timeout} => {-32051, retryable},
      {network, connection_reset} => {-32052, retryable},
      {network, connection_aborted} => {-32053, retryable},
      {network, connection_lost} => {-32054, retryable},
      {network, network_unreachable} => {-32059, retryable},
      {network, host_unreachable} => {-32058, retryable},
      {service, unavailable} => {-32067, retryable},
      {service, timeout} => {-32068, retryable},
      {service, overloaded} => {-32069, retryable},
      {validation, invalid_params} => {-32004, non_retryable},
      {validation, invalid_request} => {-32002, non_retryable},
      {validation, method_not_found} => {-32003, non_retryable},
      {security, authentication_failed} => {-32009, non_retryable},
      {security, authorization_failed} => {-32010, non_retryable},
      {security, permission_denied} => {-32008, non_retryable},
      {internal, internal_error} => {-32000, unknown},
      {internal, parse_error} => {-32001, unknown},
      {operation, operation_not_supported} => {-32018, non_retryable},
      {operation, operation_timeout} => {-32019, unknown},
      {operation, operation_cancelled} => {-32020, non_retryable},
      {resource, resource_not_found} => {-32012, non_retryable},
      {resource, resource_already_exists} => {-32013, non_retryable},
      {resource, resource_busy} => {-32014, retryable},
      {resource, resource_conflict} => {-32015, non_retryable},
      {resource, resource_limit_exceeded} => {-32016, non_retryable}}.

%% @doc Initialize recovery strategies
-spec init_recovery_strategies() -> map().
init_recovery_strategies() ->
    #{retryable => {retry, 1000},
      non_retryable => {fallback, default_fallback},
      unknown => {escalate, high}}.

%% @doc Initialize error handlers
-spec init_error_handlers() -> list().
init_error_handlers() ->
    %% List of error handler processes
    [].

%% @doc Get recovery strategy
-spec get_recovery_strategy(term(), #error_state{}) ->
                               {retry, integer()} |
                               {fallback, term()} |
                               {escalate, atom()} |
                               {ignore, term()} |
                               {log_only, term()} |
                               unknown.
get_recovery_strategy(Error, State) ->
    try
        %% Match error pattern
        case match_error_pattern(Error, State) of
            {Code, Type} ->
                case maps:get(Type, State#error_state.recovery_strategies, unknown) of
                    unknown ->
                        unknown;
                    Strategy ->
                        Strategy
                end;
            unknown ->
                unknown
        end
    catch
        _ ->
            unknown
    end.

%% @doc Match error pattern
-spec match_error_pattern(term(), #error_state{}) -> {integer(), atom()} | unknown.
match_error_pattern(Error, State) ->
    ErrorPatterns = State#error_state.error_patterns,

    %% Try to match against error patterns
    case Error of
        {error, {Category, Reason}} ->
            case maps:get({Category, Reason}, ErrorPatterns, unknown) of
                unknown ->
                    case maps:get({Category, any}, ErrorPatterns, unknown) of
                        unknown ->
                            unknown;
                        Pattern ->
                            Pattern
                    end;
                Pattern ->
                    Pattern
            end;
        {error, Reason} ->
            case maps:get({any, Reason}, ErrorPatterns, unknown) of
                unknown ->
                    unknown;
                Pattern ->
                    Pattern
            end;
        Reason ->
            case maps:get({any, Reason}, ErrorPatterns, unknown) of
                unknown ->
                    unknown;
                Pattern ->
                    Pattern
            end
    end.

%% @doc Handle retry error
-spec handle_retry_error(term(), term(), integer(), #error_state{}) -> {noreply, #error_state{}}.
handle_retry_error(Error, Context, Backoff, State) ->
    lager:warning("Retrying error after backoff: ~p", [Error]),

    %% Schedule retry
    erlang:send_after(Backoff, self(), {retry_error, Error, Context}),

    {noreply, State}.

%% @doc Handle fallback error
-spec handle_fallback_error(term(), term(), term(), #error_state{}) -> {noreply, #error_state{}}.
handle_fallback_error(Error, Context, Fallback, State) ->
    lager:warning("Handling error with fallback: ~p", [Error]),

    %% Execute fallback strategy
    case execute_fallback(Fallback, Context) of
        ok ->
            {noreply, State};
        {error, FallbackError} ->
            %% Fallback failed, escalate
            handle_escalate_error(FallbackError, Context, high, State)
    end.

%% @doc Handle escalate error
-spec handle_escalate_error(term(), term(), atom(), #error_state{}) -> {noreply, #error_state{}}.
handle_escalate_error(Error, Context, Severity, State) ->
    lager:warning("Escalating error with severity ~p: ~p", [Severity, Error]),

    %% Send escalation alert
    send_escalation_alert(Error, Context, Severity),

    %% Notify handlers
    lists:foreach(fun(Handler) -> Handler ! {error_escalated, Error, Context, Severity} end,
                  State#error_state.handlers),

    {noreply, State}.

%% @doc Handle ignore error
-spec handle_ignore_error(term(), term(), term(), #error_state{}) -> {noreply, #error_state{}}.
handle_ignore_error(Error, Context, Reason, State) ->
    lager:info("Ignoring error: ~p (~p)", [Error, Reason]),

    {noreply, State}.

%% @doc Handle log only error
-spec handle_log_only_error(term(), term(), term(), #error_state{}) -> {noreply, #error_state{}}.
handle_log_only_error(Error, Context, Reason, State) ->
    lager:info("Logging only error: ~p (~p)", [Error, Reason]),

    {noreply, State}.

%% @doc Handle unknown error
-spec handle_unknown_error(term(), term(), #error_state{}) -> {noreply, #error_state{}}.
handle_unknown_error(Error, Context, State) ->
    lager:warning("Unknown error type: ~p", [Error]),

    %% Escalate unknown errors
    handle_escalate_error(Error, Context, high, State).

%% @doc Execute fallback strategy
-spec execute_fallback(term(), term()) -> ok | {error, term()}.
execute_fallback(Fallback, Context) ->
    try
        case Fallback of
            default_fallback ->
                execute_default_fallback(Context);
            {module, Module, Function} ->
                erlang:apply(Module, Function, [Context]);
            {function, Function} ->
                Function(Context);
            _ ->
                {error, invalid_fallback}
        end
    catch
        Error:Reason ->
            {error, {fallback_error, Error, Reason}}
    end.

%% @doc Execute default fallback
-spec execute_default_fallback(term()) -> ok.
execute_default_fallback(_Context) ->
    %% Default fallback action
    ok.

%% @doc Send escalation alert
-spec send_escalation_alert(term(), term(), atom()) -> ok.
send_escalation_alert(Error, Context, Severity) ->
    %% Send alert to monitoring system
    Alert =
        #{<<"error">> => Error,
          <<"context">> => Context,
          <<"severity">> => Severity,
          <<"timestamp">> => erlang:system_time(millisecond)},
    erlmcp_observability:send_alert(Alert).

%% @doc Log error internally
-spec log_error_internal(term(), term(), #error_state{}) -> ok.
log_error_internal(Error, Context, State) ->
    %% Log to error log
    LogEntry =
        #{<<"error">> => Error,
          <<"context">> => Context,
          <<"timestamp">> => erlang:system_time(millisecond)},

    %% Log to file
    log_to_file(LogEntry),

    %% Log to monitoring system
    erlmcp_observability:log_error(LogEntry).

%% @doc Record error metrics internally
-spec record_error_metrics_internal(term(), term(), #error_state{}) -> #error_state{}.
record_error_metrics_internal(Error, Context, State) ->
    %% Record error type metrics
    ErrorType = format_error_type(Error),
    NewMetrics = maps:put(ErrorType, erlang:system_time(millisecond), State#error_state.metrics),

    %% Check error rate thresholds
    check_error_rate_thresholds(State, NewMetrics),

    State#error_state{metrics = NewMetrics}.

%% @doc Check error rate thresholds
-spec check_error_rate_thresholds(#error_state{}, map()) -> ok.
check_error_rate_thresholds(State, Metrics) ->
    %% Check if error rate exceeds threshold
    RecentErrors =
        lists:filter(fun({_, Timestamp}) ->
                        erlang:system_time(millisecond) - Timestamp < 60000 % 1 minute
                     end,
                     maps:to_list(Metrics)),

    ErrorCount = length(RecentErrors),
    AlertThreshold = maps:get(error_alert_threshold, State#error_state.thresholds, 100),

    if ErrorCount >= AlertThreshold ->
           send_error_rate_alert(ErrorCount, AlertThreshold);
       true ->
           ok
    end.

%% @doc Send error rate alert
-spec send_error_rate_alert(integer(), integer()) -> ok.
send_error_rate_alert(ErrorCount, AlertThreshold) ->
    Alert =
        #{<<"type">> => <<"error_rate_alert">>,
          <<"error_count">> => ErrorCount,
          <<"threshold">> => AlertThreshold,
          <<"timestamp">> => erlang:system_time(millisecond)},
    erlmcp_observability:send_alert(Alert).

%% @doc Log to file
-spec log_to_file(map()) -> ok.
log_to_file( LogEntry ) -> try LogFile = filename : join( os : getenv( "HOME" , "." ) , ".erlmcp_errors.log" ) , LogLine = jsx : encode( LogEntry ) << "\n" >> , case file : open( LogFile , [ append , binary ] ) of { ok , File } -> file : write( File , LogLine ) , file : close( File ) ; { error , _ } -> ok end catch _ -> ok end .

%% @doc Format error type
-spec format_error_type(term()) -> binary().
format_error_type(Error) ->
    case Error of
        {error, {Category, _}} ->
            atom_to_binary(Category, utf8);
        {error, Category} ->
            atom_to_binary(Category, utf8);
        {Category, _} ->
            atom_to_binary(Category, utf8);
        Category ->
            atom_to_binary(Category, utf8);
        _ ->
            <<"unknown_error">>
    end.

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{<<"total_errors">> => 0,
      <<"network_errors">> => 0,
      <<"validation_errors">> => 0,
      <<"security_errors">> => 0,
      <<"internal_errors">> => 0,
      <<"service_errors">> => 0,
      <<"resource_errors">> => 0,
      <<"operation_errors">> => 0}.

%% @doc Retry with backoff (internal)
-spec retry_with_backoff(fun(), integer(), integer(), integer()) -> {ok, term()} | {error, term()}.
retry_with_backoff(Operation, Attempts, CurrentAttempt, Delay) ->
    case Operation() of
        {ok, Result} ->
            {ok, Result};
        {error, Error} when Attempts > 0 ->
            case is_retryable(Error) of
                true ->
                    BackoffDelay =
                        min(Delay * 2, maps:get(retry_max_delay, ?DEFAULT_THRESHOLDS, 30000)),
                    timer:sleep(BackoffDelay),
                    retry_with_backoff(Operation, Attempts - 1, CurrentAttempt + 1, BackoffDelay);
                false ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.
