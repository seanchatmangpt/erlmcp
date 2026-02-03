%%%-------------------------------------------------------------------
%%% @doc Error Tracker for erlmcp
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
-record(error_category, {
    id,
    name,
    severity,
    pattern,
    description
}).

-record(error_threshold, {
    error_type,
    threshold,
    window,
    action
}).

-record(error_record, {
    id,
    timestamp,
    category,
    type,
    message,
    details,
    stacktrace,
    context,
    severity,
    resolved
}).

-record(window_stats, {
    total_errors,
    window_start,
    error_count,
    rate_history
}).

-record(error_pattern, {
    pattern,
    frequency,
    last_seen,
    locations
}).

-record(error_context, {
    source,
    service,
    component,
    user_id,
    session_id,
    trace_id,
    span_id
}).

-record(state, {
    error_records,
    error_categories,
    error_thresholds,
    window_stats,
    error_patterns,
    history_size,
    cleanup_interval,
    enabled,
    alert_manager
}).

-define(HISTORY_SIZE, 10000).
-define(DEFAULT_WINDOW, 60000).
-define(CLEANUP_INTERVAL, 300000).

%% API
start_link() -> start_link(#{}).
start_link(Config) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

track_error(ErrorType, Message) -> track_error(ErrorType, Message, #{}).
track_error(ErrorType, Message, Context) ->
    gen_server:call(?MODULE, {track_error, ErrorType, Message, Context}).

get_error_stats() -> gen_server:call(?MODULE, get_error_stats).
get_error_history(ErrorType) -> gen_server:call(?MODULE, {get_error_history, ErrorType}).
get_error_patterns() -> gen_server:call(?MODULE, get_error_patterns).
get_error_trends(ErrorType) -> gen_server:call(?MODULE, {get_error_trends, ErrorType}).
set_error_thresholds(ErrorType, ThresholdData) ->
    gen_server:call(?MODULE, {set_error_thresholds, ErrorType, ThresholdData}).
get_error_thresholds(ErrorType) -> gen_server:call(?MODULE, {get_error_thresholds, ErrorType}).
get_error_report() -> gen_server:call(?MODULE, get_error_report).

%% gen_server callbacks
init(Config) ->
    process_flag(trap_exit, true),
    DefaultCategories = #{},
    DefaultThresholds = #{},
    WindowStats = #window_stats{
        total_errors = 0,
        window_start = erlang:system_time(millisecond),
        error_count = #{},
        rate_history = queue:new()
    },
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
    erlang:send_after(State#state.cleanup_interval, self(), cleanup_errors),
    {ok, State}.

handle_call({track_error, ErrorType, Message, Context}, _From, State) ->
    ErrorRecord = create_error_record(ErrorType, Message, Context),
    UpdatedStats = update_window_stats(ErrorRecord, State#state.window_stats),
    StoreState = store_error_record(ErrorRecord, State),
    {reply, ok, StoreState#state{window_stats = UpdatedStats}};

handle_call(get_error_stats, _From, State) ->
    Stats = calculate_error_statistics(State),
    {reply, Stats, State};

handle_call({get_error_history, ErrorType}, _From, State) ->
    History = [E || E <- maps:values(State#state.error_records), E#error_record.type =:= ErrorType],
    SortedHistory = lists:sort(fun(E1, E2) -> E1#error_record.timestamp > E2#error_record.timestamp end, History),
    {reply, SortedHistory, State};

handle_call(get_error_patterns, _From, State) ->
    {reply, State#state.error_patterns, State};

handle_call({get_error_trends, ErrorType}, _From, State) ->
    Trends = calculate_error_trends(ErrorType, State),
    {reply, Trends, State};

handle_call({set_error_thresholds, ErrorType, ThresholdData}, _From, State) ->
    Threshold = create_error_threshold(ErrorType, ThresholdData),
    Thresholds = maps:put(ErrorType, Threshold, State#state.error_thresholds),
    ?LOG_INFO("Set error threshold for: ~s", [ErrorType]),
    {reply, ok, State#state{error_thresholds = Thresholds}};

handle_call({get_error_thresholds, ErrorType}, _From, State) ->
    Threshold = maps:get(ErrorType, State#state.error_thresholds, undefined),
    {reply, Threshold, State};

handle_call(get_error_report, _From, State) ->
    Report = generate_error_report(State),
    {reply, Report, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(cleanup_errors, State) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - State#state.cleanup_interval,
    CleanedRecords = maps:filter(fun(_Id, Error) -> Error#error_record.timestamp > Cutoff end, State#state.error_records),
    UpdatedStats = reset_window_stats(State#state.window_stats),
    erlang:send_after(State#state.cleanup_interval, self(), cleanup_errors),
    {noreply, State#state{error_records = CleanedRecords, window_stats = UpdatedStats}};

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% Internal
create_error_record(ErrorType, Message, Context) ->
    ErrorId = generate_error_id(),
    Category = determine_error_category(ErrorType, Message),
    Severity = determine_error_severity(Category),
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

create_error_context(Context) ->
    #error_context{
        source = maps:get(source, Context, <<"unknown">>),
        service = maps:get(service, Context, <<"erlmcp">>),
        component = maps:get(component, Context, <<"unknown">>),
        user_id = maps:get(user_id, Context, <<>>),
        session_id = maps:get(session_id, Context, <<>>),
        trace_id = maps:get(trace_id, Context, <<>>),
        span_id = maps:get(span_id, Context, <<>>)
    }.

determine_error_category(_ErrorType, Message) ->
    case re:run(Message, "timeout", [caseless]) of
        {match, _} -> <<"timeout">>;
        nomatch -> <<"unknown">>
    end.

determine_error_severity(<<"timeout">>) -> medium;
determine_error_severity(_) -> medium.

update_window_stats(ErrorRecord, Stats) ->
    Type = ErrorRecord#error_record.type,
    CountMap = Stats#window_stats.error_count,
    NewCountMap = maps:update_with(Type, fun(C) -> C + 1 end, 1, CountMap),
    NewRateHistory = queue:in(1, Stats#window_stats.rate_history),
    TrimmedHistory = trim_history(NewRateHistory, 60),
    Stats#window_stats{
        total_errors = Stats#window_stats.total_errors + 1,
        error_count = NewCountMap,
        rate_history = TrimmedHistory
    }.

reset_window_stats(Stats) ->
    Stats#window_stats{
        total_errors = 0,
        window_start = erlang:system_time(millisecond),
        error_count = #{},
        rate_history = queue:new()
    }.

store_error_record(ErrorRecord, State) ->
    Records = maps:put(ErrorRecord#error_record.id, ErrorRecord, State#state.error_records),
    TrimmedRecords = trim_records(Records, State#state.history_size),
    State#state{error_records = TrimmedRecords}.

calculate_error_statistics(State) ->
    Records = maps:values(State#state.error_records),
    #{
        total_errors => length(Records),
        error_types => #{},
        severity_distribution => #{},
        error_rates => #{},
        pattern_count => length(State#state.error_patterns)
    }.

calculate_error_trends(ErrorType, State) ->
    History = [E || E <- maps:values(State#state.error_records), E#error_record.type =:= ErrorType],
    #{
        error_type => ErrorType,
        count => length(History),
        trend => stable,
        recent_rate => 0.0,
        peak_time => undefined
    }.

create_error_threshold(ErrorType, Data) ->
    Action = case maps:get(action, Data, alert) of
        alert -> alert;
        log -> log;
        ignore -> ignore;
        _ -> alert
    end,
    #error_threshold{
        error_type = ErrorType,
        threshold = maps:get(threshold, Data, 0.05),
        window = maps:get(window, Data, 60000),
        action = Action
    }.

generate_error_report(State) ->
    Stats = calculate_error_statistics(State),
    #{
        summary => Stats,
        categories => maps:values(State#state.error_categories),
        patterns => State#state.error_patterns,
        thresholds => maps:values(State#state.error_thresholds),
        window => State#state.window_stats,
        generated_at => erlang:system_time(millisecond)
    }.

trim_records(Records, MaxSize) ->
    case map_size(Records) > MaxSize of
        true ->
            Keys = maps:keys(Records),
            ToRemove = lists:sublist(Keys, map_size(Records) - MaxSize),
            lists:foldl(fun(K, Acc) -> maps:remove(K, Acc) end, Records, ToRemove);
        false -> Records
    end.

trim_history(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of true -> queue:drop(Queue); false -> Queue end.

generate_error_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).
