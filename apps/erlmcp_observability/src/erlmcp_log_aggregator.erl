%%%-------------------------------------------------------------------
%%% @doc
%%% Log Aggregator for erlmcp
%%%
%%% Aggregates, formats, and streams logs to Loki/Grafana.
%%% Provides structured logging, log retention, and log search capabilities.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_log_aggregator).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         log/4, log/5,
         get_logs/1, search_logs/2,
         set_retention_policy/2, get_retention_policies/0,
         stream_to_loki/1, stop_loki_stream/0,
         get_log_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(log_entry, {
    id :: binary(),
    timestamp :: integer(),
    level :: atom(),
    message :: binary(),
    metadata :: map(),
    tags :: [binary()],
    trace_id :: binary() | undefined,
    span_id :: binary() | undefined,
    component :: binary(),
    node :: binary()
}).

-record(retention_policy, {
    id :: binary(),
    level :: binary(),
    duration :: binary(),
    max_size :: integer()
}).

-record(loki_config, {
    endpoint :: binary(),
    username :: binary() | undefined,
    password :: binary() | undefined,
    batch_size :: integer(),
    flush_interval :: integer(),
    enabled :: boolean()
}).

-record(log_stats, {
    total_logs :: integer(),
    logs_by_level :: map(),
    logs_by_component :: map(),
    recent_rate :: float(),
    active_streams :: integer()
}).

-record(state, {
    logs :: queue:queue(#log_entry{}),
    retention_policies :: map(),
    loki_config :: #loki_config{},
    buffer :: [#log_entry{}],
    stats :: #log_stats{},
    history_size :: pos_integer(),
    flush_interval :: pos_integer(),
    compression_enabled :: boolean(),
    batch_size :: integer(),
    log_format :: atom(),
    enabled :: boolean(),
    flush_ref :: reference() | undefined
}).

-define(DEFAULT_BATCH_SIZE, 100).
-define(DEFAULT_FLUSH_INTERVAL, 5000).
-define(DEFAULT_HISTORY_SIZE, 1000000).
-define(DEFAULT_RETENTION, <<"7d">>).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

log(Level, Component, Message, Metadata) ->
    log(Level, Component, Message, Metadata, []).

log(Level, Component, Message, Metadata, Tags) ->
    gen_server:cast(?MODULE, {log, Level, Component, Message, Metadata, Tags}).

get_logs(Filter) ->
    gen_server:call(?MODULE, {get_logs, Filter}).

search_logs(Query, Options) ->
    gen_server:call(?MODULE, {search_logs, Query, Options}).

set_retention_policy(Level, Policy) ->
    gen_server:call(?MODULE, {set_retention_policy, Level, Policy}).

get_retention_policies() ->
    gen_server:call(?MODULE, get_retention_policies).

stream_to_loki(Config) ->
    gen_server:call(?MODULE, {stream_to_loki, Config}).

stop_loki_stream() ->
    gen_server:call(?MODULE, stop_loki_stream).

get_log_stats() ->
    gen_server:call(?MODULE, get_log_stats).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    process_flag(trap_exit, true),

    DefaultRetention = load_default_retention_policies(),
    LokiConfig = create_loki_config(Config),
    InitialStats = #log_stats{
        total_logs = 0,
        logs_by_level = maps:new(),
        logs_by_component = maps:new(),
        recent_rate = 0.0,
        active_streams = 0
    },

    State = #state{
        logs = queue:new(),
        retention_policies = DefaultRetention,
        loki_config = LokiConfig,
        buffer = [],
        stats = InitialStats,
        history_size = maps:get(history_size, Config, ?DEFAULT_HISTORY_SIZE),
        flush_interval = maps:get(flush_interval, Config, ?DEFAULT_FLUSH_INTERVAL),
        compression_enabled = maps:get(compression_enabled, Config, true),
        batch_size = maps:get(batch_size, Config, ?DEFAULT_BATCH_SIZE),
        log_format = maps:get(log_format, Config, json),
        enabled = maps:get(enabled, Config, true)
    },

    FlushRef = erlang:send_after(State#state.flush_interval, self(), flush_logs),
    {ok, State#state{flush_ref = FlushRef}}.

handle_call({get_logs, Filter}, _From, State) ->
    Logs = apply_filter(queue:to_list(State#state.logs), Filter),
    FilteredLogs = apply_retention(Logs, State#state.retention_policies),
    {reply, FilteredLogs, State};

handle_call({search_logs, Query, Options}, _From, State) ->
    Results = search_logs_in_queue(queue:to_list(State#state.logs), Query, Options),
    {reply, Results, State};

handle_call({set_retention_policy, Level, Policy}, _From, State) ->
    RetentionPolicy = create_retention_policy(Level, Policy),
    Policies = maps:put(Level, RetentionPolicy, State#state.retention_policies),
    ?LOG_INFO("Set retention policy for level: ~s", [Level]),
    {reply, ok, State#state{retention_policies = Policies}};

handle_call(get_retention_policies, _From, State) ->
    {reply, State#state.retention_policies, State};

handle_call({stream_to_loki, Config}, _From, State) ->
    LokiConfig = create_loki_config(Config),
    ?LOG_INFO("Started Loki streaming to: ~s", [LokiConfig#loki_config.endpoint]),
    {reply, ok, State#state{loki_config = LokiConfig}};

handle_call(stop_loki_stream, _From, State) ->
    DisabledLoki = State#state.loki_config#loki_config{enabled = false},
    ?LOG_INFO("Stopped Loki streaming"),
    {reply, ok, State#state{loki_config = DisabledLoki}};

handle_call(get_log_stats, _From, State) ->
    {reply, State#state.stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log, Level, Component, Message, Metadata, Tags}, State) ->
    LogEntry = create_log_entry(Level, Component, Message, Metadata, Tags),
    UpdatedStats = update_log_stats(LogEntry, State#state.stats),
    Buffer = [LogEntry | State#state.buffer],

    NewState = case length(Buffer) >= State#state.batch_size of
        true -> flush_buffer(Buffer, State);
        false -> State
    end,

    {noreply, NewState#state{buffer = Buffer, stats = UpdatedStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_logs, State) ->
    NewState = case State#state.buffer of
        [] -> State;
        Buffer -> flush_buffer(Buffer, State)
    end,

    FlushRef = erlang:send_after(State#state.flush_interval, self(), flush_logs),
    {noreply, NewState#state{flush_ref = FlushRef}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.buffer of
        [] -> ok;
        Buffer -> flush_buffer(Buffer, State)
    end,

    case State#state.flush_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

create_log_entry(Level, Component, Message, Metadata, Tags) ->
    LogId = generate_log_id(),
    #log_entry{
        id = LogId,
        timestamp = erlang:system_time(millisecond),
        level = Level,
        message = Message,
        metadata = Metadata,
        tags = Tags,
        trace_id = maps:get(trace_id, Metadata, undefined),
        span_id = maps:get(span_id, Metadata, undefined),
        component = Component,
        node = atom_to_binary(node())
    }.

create_retention_policy(Level, Policy) ->
    #retention_policy{
        id = generate_policy_id(),
        level = Level,
        duration = maps:get(duration, Policy, ?DEFAULT_RETENTION),
        max_size = maps:get(max_size, Policy, 1073741824)
    }.

create_loki_config(Config) ->
    #loki_config{
        endpoint = maps:get(endpoint, Config, <<"http://localhost:3100">>),
        username = maps:get(username, Config, undefined),
        password = maps:get(password, Config, undefined),
        batch_size = maps:get(batch_size, Config, ?DEFAULT_BATCH_SIZE),
        flush_interval = maps:get(flush_interval, Config, ?DEFAULT_FLUSH_INTERVAL),
        enabled = maps:get(enabled, Config, true)
    }.

update_log_stats(LogEntry, Stats) ->
    Total = Stats#log_stats.total_logs + 1,
    LevelKey = atom_to_binary(LogEntry#log_entry.level),
    ComponentKey = LogEntry#log_entry.component,

    LevelMap = maps:update_with(
        LevelKey,
        fun(C) -> C + 1 end,
        1,
        Stats#log_stats.logs_by_level
    ),

    ComponentMap = maps:update_with(
        ComponentKey,
        fun(C) -> C + 1 end,
        1,
        Stats#log_stats.logs_by_component
    ),

    Rate = Stats#log_stats.recent_rate * 0.9 + 0.1,

    Stats#log_stats{
        total_logs = Total,
        logs_by_level = LevelMap,
        logs_by_component = ComponentMap,
        recent_rate = Rate
    }.

apply_filter(Logs, Filter) ->
    TimeFiltered = apply_time_filter(Logs, Filter),
    LevelFiltered = apply_level_filter(TimeFiltered, Filter),
    ComponentFiltered = apply_component_filter(LevelFiltered, Filter),
    TagFiltered = apply_tag_filter(ComponentFiltered, Filter),
    apply_search_filter(TagFiltered, Filter).

apply_time_filter(Logs, Filter) ->
    case maps:get(time_range, Filter, undefined) of
        undefined -> Logs;
        {Start, End} ->
            lists:filter(fun(Log) ->
                Time = Log#log_entry.timestamp,
                Time >= Start andalso Time =< End
            end, Logs)
    end.

apply_level_filter(Logs, Filter) ->
    case maps:get(min_level, Filter, undefined) of
        undefined -> Logs;
        MinLevel ->
            lists:filter(fun(Log) ->
                Log#log_entry.level >= MinLevel
            end, Logs)
    end.

apply_component_filter(Logs, Filter) ->
    case maps:get(component, Filter, undefined) of
        undefined -> Logs;
        Component ->
            lists:filter(fun(Log) ->
                Log#log_entry.component =:= Component
            end, Logs)
    end.

apply_tag_filter(Logs, Filter) ->
    case maps:get(tags, Filter, undefined) of
        undefined -> Logs;
        RequiredTags ->
            lists:filter(fun(Log) ->
                LogTags = Log#log_entry.tags,
                lists:all(fun(Tag) -> lists:member(Tag, LogTags) end, RequiredTags)
            end, Logs)
    end.

apply_search_filter(Logs, Filter) ->
    case maps:get(search, Filter, undefined) of
        undefined -> Logs;
        Query ->
            lists:filter(fun(Log) ->
                case Log#log_entry.message of
                    Message when is_binary(Message) ->
                        binary:match(Message, Query) =/= nomatch;
                    _ ->
                        false
                end
            end, Logs)
    end.

search_logs_in_queue(Logs, Query, Options) ->
    Results = lists:filter(fun(Log) ->
        binary:match(Log#log_entry.message, Query) =/= nomatch
    end, Logs),

    case maps:get(page, Options, undefined) of
        undefined -> Results;
        {Page, PageSize} ->
            PageNum = Page - 1,
            Start = PageNum * PageSize,
            lists:sublist(Results, Start + 1, PageSize)
    end.

apply_retention(Logs, Policies) ->
    Now = erlang:system_time(millisecond),
    lists:foldl(fun(Log, Acc) ->
        case should_retain(Log, Policies, Now) of
            true -> [Log | Acc];
            false -> Acc
        end
    end, [], Logs).

should_retain(Log, Policies, Now) ->
    Level = atom_to_binary(Log#log_entry.level),
    case maps:get(Level, Policies, undefined) of
        undefined -> true;
        Policy ->
            Duration = parse_duration(Policy#retention_policy.duration),
            Cutoff = Now - Duration,
            Log#log_entry.timestamp >= Cutoff
    end.

parse_duration(Duration) when is_binary(Duration) ->
    parse_duration(binary_to_list(Duration));
parse_duration(Duration) ->
    case re:run(Duration, "^P(\\d+D)?(T(\\d+H)?(\\d+M)?(\\d+S)?)?$", [{capture, all, list}]) of
        nomatch -> 0;
        {match, [_, DaysPart, _, HoursPart, MinutesPart, SecondsPart]} ->
            Days = case DaysPart of
                undefined -> 0;
                _ -> list_to_integer(lists:sublist(DaysPart, 2, 1))
            end,
            Hours = case HoursPart of
                undefined -> 0;
                _ -> list_to_integer(lists:sublist(HoursPart, 2, 1))
            end,
            Minutes = case MinutesPart of
                undefined -> 0;
                _ -> list_to_integer(lists:sublist(MinutesPart, 2, 1))
            end,
            Seconds = case SecondsPart of
                undefined -> 0;
                _ -> list_to_integer(lists:sublist(SecondsPart, 2, 1))
            end,
            Days * 86400000 + Hours * 3600000 + Minutes * 60000 + Seconds * 1000
    end.

flush_buffer(Buffer, State) ->
    FormattedLogs = format_logs_for_loki(Buffer, State#state.log_format),

    case State#state.loki_config#loki_config.enabled of
        true ->
            send_to_loki(FormattedLogs, State#state.loki_config);
        false ->
            ok
    end,

    NewLogs = lists:foldl(fun(Log, Acc) -> queue:in(Log, Acc) end, State#state.logs, Buffer),
    TrimmedLogs = trim_queue(NewLogs, State#state.history_size),

    State#state{
        logs = TrimmedLogs,
        buffer = []
    }.

format_logs_for_loki(Logs, Format) ->
    case Format of
        json ->
            lists:map(fun(Log) -> format_log_as_json(Log) end, Logs);
        _ ->
            lists:map(fun(Log) -> format_log_as_text(Log) end, Logs)
    end.

format_log_as_json(Log) ->
    LogMap = #{
        <<"component">> => Log#log_entry.component,
        <<"node">> => Log#log_entry.node,
        <<"trace_id">> => Log#log_entry.trace_id,
        <<"span_id">> => Log#log_entry.span_id,
        <<"tags">> => Log#log_entry.tags,
        <<"timestamp">> => Log#log_entry.timestamp,
        <<"level">> => atom_to_binary(Log#log_entry.level),
        <<"message">> => Log#log_entry.message
    },
    try
        json:encode(LogMap)
    catch
        error:undef ->
            iolist_to_binary(io_lib:format("~p", [Log]))
    end.

format_log_as_text(Log) ->
    iolist_to_binary(
        io_lib:format("[~s] [~p] [~s] [~s] ~s~n",
            [format_timestamp(Log#log_entry.timestamp),
             Log#log_entry.level,
             Log#log_entry.component,
             Log#log_entry.node,
             Log#log_entry.message])
    ).

format_timestamp(Timestamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:now_to_local_time(Timestamp div 1000),
    iolist_to_binary(
        io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                      [Year, Month, Day, Hour, Minute, Second])).

send_to_loki(Logs, LokiConfig) ->
    BatchSize = LokiConfig#loki_config.batch_size,

    case length(Logs) > BatchSize of
        true ->
            lists:foreach(fun(Batch) ->
                send_loki_batch(Batch, LokiConfig)
            end, split_into_batches(Logs, BatchSize));
        false ->
            send_loki_batch(Logs, LokiConfig)
    end.

send_loki_batch(Batch, LokiConfig) ->
    Payload = create_loki_payload(Batch),
    Headers = prepare_loki_headers(LokiConfig),

    case httpc:request(post,
        {binary_to_list(LokiConfig#loki_config.endpoint), Headers,
         "application/json", Payload}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, Code, _}, _, Body}} ->
            ?LOG_WARNING("Loki request failed with status ~p: ~s", [Code, Body]);
        {error, Reason} ->
            ?LOG_WARNING("Loki request failed: ~p", [Reason])
    end.

create_loki_payload(Logs) ->
    Streams = lists:foldl(fun(Log, Acc) ->
        StreamName = Log#log_entry.component,
        LogEntry = maps:put(<<"timestamp">>, Log#log_entry.timestamp,
            maps:put(<<"line">>, Log#log_entry.message, maps:new())),

        case lists:keyfind(StreamName, 1, Acc) of
            false -> [{StreamName, [LogEntry]} | Acc];
            {StreamName, Entries} ->
                lists:keyreplace(StreamName, 1, Acc, {StreamName, [LogEntry | Entries]})
        end
    end, [], Logs),

    try
        json:encode(maps:put(<<"streams">>, Streams, maps:new()))
    catch
        error:undef ->
            <<"{}">>
    end.

prepare_loki_headers(LokiConfig) ->
    Headers = [{"Content-Type", "application/json"}],

    case LokiConfig#loki_config.username of
        undefined -> Headers;
        Username ->
            AuthString = base64:encode(<<Username/binary, ":", (LokiConfig#loki_config.password)/binary>>),
            [{"Authorization", "Basic " ++ binary_to_list(AuthString)} | Headers]
    end.

split_into_batches(Logs, BatchSize) ->
    split_into_batches(Logs, BatchSize, []).

split_into_batches([], _BatchSize, Acc) ->
    lists:reverse(Acc);
split_into_batches(Logs, BatchSize, Acc) ->
    {Batch, Rest} = lists:split(BatchSize, Logs),
    split_into_batches(Rest, BatchSize, [Batch | Acc]).

trim_queue(Queue, MaxSize) ->
    case queue:len(Queue) > MaxSize of
        true -> queue:drop(Queue);
        false -> Queue
    end.

load_default_retention_policies() ->
    maps:from_list([
        {<<"debug">>, #retention_policy{
            id = <<"debug_retention">>,
            level = <<"debug">>,
            duration = <<"1d">>,
            max_size = 1073741824
        }},
        {<<"info">>, #retention_policy{
            id = <<"info_retention">>,
            level = <<"info">>,
            duration = <<"7d">>,
            max_size = 5368709120
        }},
        {<<"warning">>, #retention_policy{
            id = <<"warning_retention">>,
            level = <<"warning">>,
            duration = <<"30d">>,
            max_size = 10737418240
        }},
        {<<"error">>, #retention_policy{
            id = <<"error_retention">>,
            level = <<"error">>,
            duration = <<"90d">>,
            max_size = 21474836480
        }},
        {<<"critical">>, #retention_policy{
            id = <<"critical_retention">>,
            level = <<"critical">>,
            duration = <<"365d">>,
            max_size = 42949672960
        }}
    ]).

generate_log_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).

generate_policy_id() ->
    Id = crypto:strong_rand_bytes(8),
    integer_to_binary(binary:decode_unsigned(Id), 16).
