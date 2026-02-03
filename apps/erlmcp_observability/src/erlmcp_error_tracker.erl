%%%-------------------------------------------------------------------
%%% @doc
%%% Error Tracker for erlmcp
%%%
%%% Tracks, categorizes, and reports errors across the erlmcp system.
%%% Provides error rate monitoring, pattern detection, and integration
%%% with alerting and monitoring systems.
%%%
%%% Features:
%%% - Error classification and categorization
%%% - Error rate monitoring with sliding windows
%%% - Exception pattern detection
%%% - Error context correlation
%%% - Integration with alert manager
%%% - Error reporting and analytics
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_error_tracker).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         track_error/2, track_error/3,
         get_error_stats/0, get_error_history/1,
         get_error_patterns/0, get_error_trends/1,
         set_error_thresholds/2, get_error_thresholds/1,
         get_error_report/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record.error_category, {
    id :: binary(),
    name :: binary(),
    severity :: low | medium | high | critical,
    pattern :: binary(),  % Regex pattern for matching
    description :: binary()
}.

-record.error_threshold, {
    error_type :: binary(),
    threshold :: float(),  % Rate threshold
    window :: pos_integer(),  % Time window in ms
    action :: alert | log | ignore
}.

#error_record, {
    id :: binary(),
    timestamp :: integer(),
    category :: binary(),
    type :: binary(),
    message :: binary(),
    details :: map(),
    stacktrace :: list(),
    context :: map(),
    severity :: low | medium | high | critical,
    resolved :: boolean()
}.

#window_stats, {
    total_errors :: integer(),
    window_start :: integer(),
    error_count :: map(),  % Type -> count
    rate_history :: queue:queue(integer())
}.

#error_pattern, {
    pattern :: binary(),
    frequency :: integer(),
    last_seen :: integer(),
    locations :: [binary()]
}.

#error_context, {
    source :: binary(),
    service :: binary(),
    component :: binary(),
    user_id :: binary(),
    session_id :: binary(),
    trace_id :: binary(),
    span_id :: binary()
}.

-record.state, {
    error_records :: #{binary() => #error_record{}},
    error_categories :: #{binary() => #error_category{}},
    error_thresholds :: #{binary() => #error_threshold{}},
    window_stats :: #window_stats{},
    error_patterns :: [#error_pattern{}],
    history_size :: pos_integer(),
    cleanup_interval :: pos_integer(),
    enabled :: boolean(),
    alert_manager :: pid()
}.

-define(HISTORY_SIZE, 10000).
-define(DEFAULT_WINDOW, 60000).  % 1 minute
-define(CLEANUP_INTERVAL, 300000).  % 5 minutes
-define(PATTERN_MIN_FREQUENCY, 5).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the error tracker
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start with configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Track an error
-spec track_error(binary(), binary()) -> ok.
track_error(ErrorType, Message) ->
    track_error(ErrorType, Message, #{}).

%% @doc Track an error with context
-spec track_error(binary(), binary(), map()) -> ok.
track_error(ErrorType, Message, Context) ->
    gen_server:call(?MODULE, {track_error, ErrorType, Message, Context}).

%% @doc Get error statistics
-spec get_error_stats() -> map().
get_error_stats() ->
    gen_server:call(?MODULE, get_error_stats).

%% @doc Get error history for a specific error type
-spec get_error_history(binary()) -> [#error_record{}].
get_error_history(ErrorType) ->
    gen_server:call(?MODULE, {get_error_history, ErrorType}).

%% @doc Get detected error patterns
-spec get_error_patterns() -> [#error_pattern{}].
get_error_patterns() ->
    gen_server:call(?MODULE, get_error_patterns).

%% @doc Get error trends for a specific error type
-spec get_error_trends(binary()) -> map().
get_error_trends(ErrorType) ->
    gen_server:call(?MODULE, {get_error_trends, ErrorType}).

%% @doc Set error thresholds
-spec set_error_thresholds(binary(), map()) -> ok.
set_error_thresholds(ErrorType, ThresholdData) ->
    gen_server:call(?MODULE, {set_error_thresholds, ErrorType, ThresholdData}).

%% @doc Get error thresholds
-spec get_error_thresholds(binary()) -> map() | undefined.
get_error_thresholds(ErrorType) ->
    gen_server:call(?MODULE, {get_error_thresholds, ErrorType}).

%% @doc Get error report
-spec get_error_report() -> map().
get_error_report() ->
    gen_server:call(?MODULE, get_error_report).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Initialize default error categories
    DefaultCategories = load_default_categories(),

    %% Initialize default thresholds
    DefaultThresholds = load_default_thresholds(),

    %% Initialize window stats
    WindowStats = #window_stats{
        total_errors = 0,
        window_start = erlang:system_time(millisecond),
        error_count = #{},
        rate_history = queue:new()
    },

    %% Initialize state
    State = #state{
        error_records = #{},
        error_categories = DefaultCategories,
        error_thresholds = DefaultThresholds,
        window_stats = WindowStats,
        error_patterns = [],
        history_size = maps:get(history_size, Config, ?HISTORY_SIZE),
        cleanup_interval = maps:get(cleanup_interval, Config, ?CLEANUP_INTERVAL),
        enabled = maps:get(enabled, Config, true),
        alert_manager = undefined
    },

    %% Start cleanup timer
    erlang:send_after(State#state.cleanup_interval, self(), cleanup_errors),

    %% Register with alert manager if available
    case whereis(erlmcp_alert_manager) of
        undefined -> ok;
        Pid -> State#state{alert_manager = Pid}
    end,

    {ok, State}.

handle_call({track_error, ErrorType, Message, Context}, _From, State) ->
    %% Create error record
    ErrorRecord = create_error_record(ErrorType, Message, Context),

    %% Update window stats
    UpdatedStats = update_window_stats(ErrorRecord, State#state.window_stats),

    %% Check thresholds
    check_threshold_violations(ErrorRecord, State#state.error_thresholds),

    DetectPatterns = detect_error_patterns(ErrorRecord, State#state.error_patterns),

    StoreState = store_error_record(ErrorRecord, State),

    %% Check if alert manager is available
    AlertState = case State#state.alert_manager of
        undefined -> StoreState;
        Pid -> send_error_alert(ErrorRecord, Pid), StoreState
    end,

    {reply, ok, AlertState#state{
        window_stats = UpdatedStats,
        error_patterns = DetectPatterns
    }};

handle_call(get_error_stats, _From, State) ->
    %% Calculate error statistics
    Stats = calculate_error_statistics(State),
    {reply, Stats, State};

handle_call({get_error_history, ErrorType}, _From, State) ->
    %% Filter error records by type
    History = [E || E <- maps:values(State#state.error_records),
                    E#error_record.type =:= ErrorType],
    {reply, lists:sort(fun(E1, E2) -> E1#error_record.timestamp > E2#error_record.timestamp end, History), State};

handle_call(get_error_patterns, _From, State) ->
    {reply, State#state.error_patterns, State};

handle_call({get_error_trends, ErrorType}, _From, State) ->
    %% Calculate trends for error type
    Trends = calculate_error_trends(ErrorType, State),
    {reply, Trends, State};

handle_call({set_error_thresholds, ErrorType, ThresholdData}, _From, State) ->
    %% Create threshold record
    Threshold = create_error_threshold(ErrorType, ThresholdData),
    Thresholds = maps:put(ErrorType, Threshold, State#state.error_thresholds),

    ?LOG_INFO("Set error threshold for: ~s", [ErrorType]),

    {reply, ok, State#state{error_thresholds = Thresholds}};

handle_call({get_error_thresholds, ErrorType}, _From, State) ->
    Threshold = maps:get(ErrorType, State#state.error_thresholds, undefined),
    {reply, Threshold, State};

handle_call(get_error_report, _From, State) ->
    %% Generate comprehensive error report
    Report = generate_error_report(State),
    {reply, Report, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_errors, State) ->
    %% Clean up old errors
    Now = erlang:system_time(millisecond),
    Cutoff = Now - State#state.cleanup_interval,

    CleanedRecords = maps:filter(fun(_Id, Error) ->
        Error#error_record.timestamp > Cutoff
    end, State#state.error_records),

    %% Update window stats
    UpdatedStats = reset_window_stats(State#state.window_stats),

    %% Schedule next cleanup
    erlang:send_after(State#state.cleanup_interval, self(), cleanup_errors),

    {noreply, State#state{
        error_records = CleanedRecords,
        window_stats = UpdatedStats
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up resources
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Create error record
create_error_record(ErrorType, Message, Context) ->
    ErrorId = generate_error_id(),

    %% Determine error category
    Category = determine_error_category(ErrorType, Message),

    %% Determine severity
    Severity = determine_error_severity(Category),

    %% Create context record
    ErrorContext = create_error_context(Context),

    #error_record{
        id = ErrorId,
        timestamp = erlang:system_time(millisecond),
        category = Category,
        type = ErrorType,
        message = Message,
        details = Context,
        stacktrace = maps:get(stacktrace, Context, []),
        context = ErrorContext,
        severity = Severity,
        resolved = false
    }.

%% @brief Create error context
create_error_context(Context) ->
    #error_context{
        source = maps:get(source, Context, <<"unknown">>),
        service = maps:get(service, Context, <<"erlmcp">>),
        component = maps:get(component, Context, <<"unknown">>),
        user_id = maps:get(user_id, Context, undefined),
        session_id = maps:get(session_id, Context, undefined),
        trace_id = maps:get(trace_id, Context, undefined),
        span_id = maps:get(span_id, Context, undefined)
    }.

%% @brief Determine error category
determine_error_category(ErrorType, Message) ->
    %% Check against known categories
    Categories = [
        #{pattern => "timeout", category => <<"timeout">>},
        #{pattern => "not_found", category => <<"not_found">>},
        #{pattern => "rate_limit", category => <<"rate_limit">>},
        #{pattern => "validation", category => <<"validation">>},
        #{pattern => "connection", category => <<"connection">>},
        #{pattern => "internal", category => <<"internal">>}
    ],

    lists:foldl(fun(Cat, Acc) ->
        case re:run(Message, Cat#{pattern}, [caseless]) of
            nomatch -> Acc;
            _ -> Cat#{category}
        end
    end, <<"unknown">>, Categories).

%% @brief Determine error severity
determine_error_severity(Category) ->
    case Category of
        <<"timeout">> -> medium;
        <<"not_found">> -> low;
        <<"rate_limit">> -> high;
        <<"validation">> -> low;
        <<"connection">> -> high;
        <<"internal">> -> critical;
        _ -> medium
    end.

%% @brief Load default error categories
load_default_categories() ->
    maps:from_list([
        {<<"timeout">>, #error_category{
            id => <<"timeout">>,
            name => <<"Timeout Errors">>,
            severity => medium,
            pattern => <<"timeout">>,
            description => <<"Request timeout errors">>
        }},
        {<<"not_found">>, #error_category{
            id => <<"not_found">>,
            name => <<"Not Found">>,
            severity => low,
            pattern => <<"not_found|404">>,
            description => <<"Resource not found errors">>
        }},
        {<<"rate_limit">>, #error_category{
            id => <<"rate_limit">>,
            name => <<"Rate Limit">>,
            severity => high,
            pattern => <<"rate_limit|429">>,
            description => <<"Rate limiting errors">>
        }},
        {<<"validation">>, #error_category{
            id => <<"validation">>,
            name => <<"Validation">>,
            severity => low,
            pattern => <<"validation|bad_request">>,
            description => <<"Input validation errors">>
        }},
        {<<"connection">>, #error_category{
            id => <<"connection">>,
            name => <<"Connection">>,
            severity => high,
            pattern => <<"connection|network">>,
            description => <<"Network connection errors">>
        }},
        {<<"internal">>, #error_category{
            id => <<"internal">>,
            name => <<"Internal Error">>,
            severity => critical,
            pattern => <<"internal|server_error">>,
            description => <<"Internal server errors">>
        }}
    ]).

%% @brief Load default error thresholds
load_default_thresholds() ->
    maps:from_list([
        {<<"timeout">>, #error_threshold{
            error_type => <<"timeout">>,
            threshold => 0.05,
            window => 60000,
            action => alert
        }},
        {<<"not_found">>, #error_threshold{
            error_type => <<"not_found">>,
            threshold => 0.1,
            window => 60000,
            action => log
        }},
        {<<"rate_limit">>, #error_threshold{
            error_type => <<"rate_limit">>,
            threshold => 0.02,
            window => 60000,
            action => alert
        }},
        {<<"validation">>, #error_threshold{
            error_type => <<"validation">>,
            threshold => 0.1,
            window => 60000,
            action => log
        }}
    ]).

%% @brief Update window statistics
update_window_stats(ErrorRecord, Stats) ->
    Now = erlang:system_time(millisecond),

    %% Update error count
    Type = ErrorRecord#error_record.type,
    CountMap = Stats#window_stats.error_count,
    NewCountMap = maps:update_with(Type, fun(C) -> C + 1 end, 1, CountMap),

    %% Add to rate history
    NewRateHistory = queue:in(1, Stats#window_stats.rate_history),

    %% Trim history
    TrimmedHistory = trim_history(NewRateHistory, 60),  % Keep 60 data points

    Stats#window_stats{
        total_errors = Stats#window_stats.total_errors + 1,
        error_count = NewCountMap,
        rate_history = TrimmedHistory
    }.

%% @brief Reset window statistics
reset_window_stats(Stats) ->
    Stats#window_stats{
        total_errors = 0,
        window_start = erlang:system_time(millisecond),
        error_count = #{},
        rate_history = queue:new()
    }.

%% @brief Check threshold violations
check_threshold_violations(ErrorRecord, Thresholds) ->
    Threshold = maps:get(ErrorRecord#error_record.type, Thresholds, undefined),

    case Threshold of
        undefined -> ok;
        _ ->
            Rate = calculate_error_rate(ErrorRecord#error_record.type, Threshold#error_threshold.window),
            if Rate >= Threshold#error_threshold.threshold ->
                   case Threshold#error_threshold.action of
                       alert -> send_error_alert(ErrorRecord);
                       log -> log_error(ErrorRecord);
                       ignore -> ok
                   end;
               true ->
                   ok
            end
    end.

%% @brief Calculate error rate
calculate_error_rate(ErrorType, Window) ->
    This = self(),
    Reply = gen_server:call(This, {get_error_stats}),
    case Reply#{error_rates} of
        #{ErrorType := Rate} -> Rate;
        _ -> 0.0
    end.

%% @brief Detect error patterns
detect_error_patterns(ErrorRecord, ExistingPatterns) ->
    Message = ErrorRecord#error_record.message,
    Type = ErrorRecord#error_record.type;

    %% Check if message matches existing patterns
    UpdatedPatterns = lists:foldl(fun(Pattern, Acc) ->
        case re:run(Message, Pattern#error_pattern.pattern, [caseless]) of
            nomatch -> Acc;
            _ -> update_pattern(Pattern, ErrorRecord, Acc)
        end
    end, ExistingPatterns, ExistingPatterns),

    %% Check if this is a new pattern
    case is_new_pattern(Message, UpdatedPatterns) of
        true -> [#error_pattern{
            pattern = extract_pattern(Message),
            frequency = 1,
            last_seen = ErrorRecord#error_record.timestamp,
            locations = [ErrorRecord#error_record.context#{source}]
        } | UpdatedPatterns];
        false -> UpdatedPatterns
    end.

%% @brief Update pattern statistics
update_pattern(Pattern, ErrorRecord, Patterns) ->
    UpdatedPattern = Pattern#error_pattern{
        frequency = Pattern#error_pattern.frequency + 1,
        last_seen = ErrorRecord#error_record.timestamp,
        locations = [ErrorRecord#error_record.context#{source} | Pattern#error_pattern.locations]
    },

    lists:map(fun(P) ->
        if P#error_pattern.pattern =:= Pattern#error_pattern.pattern ->
                UpdatedPattern;
           true ->
                P
        end
    end, Patterns).

%% @brief Check if message represents a new pattern
is_new_pattern(Message, Patterns) ->
    PatternString = extract_pattern(Message),
    lists:all(fun(P) ->
        P#error_pattern.pattern =/= PatternString
    end, Patterns).

%% @brief Extract pattern from message
extract_pattern(Message) ->
    %% Extract variables from message
    Pattern = re:replace(Message, "\\d+", "[0-9]", [global]),
    re:replace(Pattern, "[a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12}", "[ID]", [global]).

%% @brief Store error record
store_error_record(ErrorRecord, State) ->
    %% Add to records map
    Records = maps:put(ErrorRecord#error_record.id, ErrorRecord, State#state.error_records);

    %% Trim records to size limit
    TrimmedRecords = trim_records(Records, State#state.history_size);

    State#state{error_records = TrimmedRecords}.

%% @brief Calculate error statistics
calculate_error_statistics(State) ->
    Records = maps:values(State#state.error_records);

    %% Count by type
    TypeCounts = lists:foldl(fun(Record, Acc) ->
        Type = Record#error_record.type;
        maps:update_with(Type, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Records);

    %% Count by severity
    SeverityCounts = lists:foldl(fun(Record, Acc) ->
        Severity = Record#error_record.severity;
        maps:update_with(Severity, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Records);

    %% Calculate rates
    ErrorRates = calculate_error_rates(State#state.window_stats);

    #{
        total_errors => length(Records),
        error_types => TypeCounts,
        severity_distribution => SeverityCounts,
        error_rates => ErrorRates,
        pattern_count => length(State#state.error_patterns)
    }.

%% @brief Calculate error rates
calculate_error_rates(Stats) ->
    %% Calculate rate for each type in current window
    maps:map(fun(_Type, Count) ->
        Count / 60.0  % Per second
    end, Stats#window_stats.error_count).

%% @brief Calculate error trends
calculate_error_trends(ErrorType, State) ->
    History = [E || E <- maps:values(State#state.error_records),
                    E#error_record.type =:= ErrorType];

    %% Sort by timestamp
    SortedHistory = lists:sort(fun(E1, E2) ->
        E1#error_record.timestamp < E2#error_record.timestamp
    end, History);

    %% Calculate trend
    Trend = analyze_error_trend(SortedHistory);

    #{
        error_type => ErrorType,
        count => length(SortedHistory),
        trend => Trend,
        recent_rate => calculate_recent_rate(SortedHistory),
        peak_time => find_peak_time(SortedHistory)
    }.

%% @brief Analyze error trend
analyze_error_trend(History) ->
    case length(History) of
        0 -> stable;
        N when N < 5 -> unknown;
        _ ->
            Recent = lists:sublist(History, 10),
            Old = lists:sublist(History, length(History) - 10, 10);

            RecentCount = length(Recent),
            OldCount = length(Old);

            if RecentCount > OldCount * 1.5 -> increasing;
               RecentCount < OldCount * 0.5 -> decreasing;
               true -> stable
            end
    end.

%% @brief Calculate recent error rate
calculate_recent_rate(History) ->
    case length(History) of
        0 -> 0.0;
        _ ->
            Recent = lists:sublist(History, 10),
            length(Recent) / 10.0
    end.

%% @brief Find peak error time
find_peak_time(History) ->
    case History of
        [] -> undefined;
        _ ->
            %% Find time period with most errors
            Bins = create_time_bins(History),
            PeakBin = lists:max([{Count, Bin} || {Bin, Count} <- Bins]),
            element(2, PeakBin)
    end.

%% @brief Create time bins
create_time_bins(History) ->
    BinSize = 3600000,  % 1 hour
    Now = erlang:system_time(millisecond);

    Bins = lists:foldl(fun(Record, Acc) ->
        Bin = (Record#error_record.timestamp div BinSize) * BinSize;
        maps:update_with(Bin, fun(L) -> [Record | L] end, [Record], Acc)
    end, #{}, History);

    %% Count errors in each bin
    maps:map(fun(_Bin, Records) -> length(Records) end, Bins).

%% @brief Send error alert
send_error_alert(ErrorRecord) ->
    Alert = #{
        type => "error",
        source => "erlmcp_error_tracker",
        severity => ErrorRecord#error_record.severity,
        title => io_lib:format("Error: ~s", [ErrorRecord#error_record.type]),
        message => ErrorRecord#error_record.message,
        details => ErrorRecord#error_record.details,
        timestamp => ErrorRecord#error_record.timestamp
    },

    case whereis(erlmcp_alert_manager) of
        undefined -> ok;
        Pid -> erlmcp_alert_manager:send_alert(Alert)
    end.

%% @brief Log error
log_error(ErrorRecord) ->
    ?LOG_WARNING("Error tracked: ~s - ~s", [
        ErrorRecord#error_record.type,
        ErrorRecord#error_record.message
    ]).

%% @brief Create error threshold
create_error_threshold(ErrorType, Data) ->
    #error_threshold{
        error_type = ErrorType,
        threshold = maps:get(threshold, Data, 0.05),
        window = maps:get(window, Data, 60000),
        action = maps:get(action, Data, alert)
    }.

%% @brief Generate error report
generate_error_report(State) ->
    Stats = calculate_error_statistics(State);

    #{
        summary => Stats,
        categories => maps:values(State#state.error_categories),
        patterns => State#state.error_patterns,
        thresholds => maps:values(State#state.error_thresholds),
        window => State#state.window_stats,
        generated_at => erlang:system_time(millisecond)
    }.

%% @brief Trim records to size limit
trim_records(Records, MaxSize) ->
    case map_size(Records) > MaxSize of
        true -> lists:foldl(fun(_, Acc) -> maps:remove(lists:nth(map_size(Acc), maps:keys(Acc)), Acc) end, Records, lists:seq(1, map_size(Records) - MaxSize));
        false -> Records
    end.

%% @brief Trim history queue to size limit
trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of
        true -> queue:drop(Queue);
        false -> Queue
    end.

%% @brief Generate unique error ID
generate_error_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).