%%%-------------------------------------------------------------------
%%% @doc Chaos Engineering Metrics Collector
%%%
%%% Collects and aggregates metrics during chaos experiments to
%%% validate nine-nines (99.9999999%) availability posture.
%%%
%%% Metrics collected:
%%% - Latency (p50, p95, p99, p999 in microseconds)
%%% - Throughput (msg/sec per role: client, server, registry)
%%% - Memory usage (heap, RSS per process)
%%% - GC pause times
%%% - Process counts
%%% - Queue depths over time
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_metrics).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    record_latency/3,
    record_throughput/3,
    record_memory/2,
    record_gc_pause/1,
    record_queue_depth/2,
    get_latency_percentiles/1,
    get_throughput_summary/0,
    get_memory_summary/0,
    get_all_metrics/0,
    reset_metrics/0,
    export_to_file/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type metric_type() :: latency | throughput | memory | gc_pause | queue_depth.
-type role() :: client | server | registry | session | transport.

-record(latency_sample, {
    role :: role(),
    operation :: binary(),
    timestamp :: integer(),
    duration_us :: non_neg_integer()
}).

-record(throughput_sample, {
    role :: role(),
    timestamp :: integer(),
    msg_count :: non_neg_integer()
}).

-record(memory_sample, {
    pid :: pid(),
    timestamp :: integer(),
    heap_mib :: float(),
    rss_mib :: float()
}).

-record(gc_sample, {
    pid :: pid(),
    timestamp :: integer(),
    pause_us :: non_neg_integer()
}).

-record(queue_sample, {
    pid :: pid(),
    timestamp :: integer(),
    depth :: non_neg_integer()
}).

-record(state, {
    latencies = [] :: [#latency_sample{}],
    throughput = [] :: [#throughput_sample{}],
    memory = [] :: [#memory_sample{}],
    gc_pauses = [] :: [#gc_sample{}],
    queue_depths = [] :: [#queue_sample{}],
    start_time :: integer(),
    sample_count = 0 :: non_neg_integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Record a latency sample
-spec record_latency(role(), binary(), non_neg_integer()) -> ok.
record_latency(Role, Operation, DurationUs) ->
    gen_server:cast(?MODULE, {record_latency, Role, Operation, DurationUs}).

%% @doc Record throughput sample
-spec record_throughput(role(), non_neg_integer(), integer()) -> ok.
record_throughput(Role, MsgCount, Timestamp) ->
    gen_server:cast(?MODULE, {record_throughput, Role, MsgCount, Timestamp}).

%% @doc Record memory usage
-spec record_memory(pid(), #{heap_mib := float(), rss_mib := float()}) -> ok.
record_memory(Pid, #{heap_mib := HeapMiB, rss_mib := RssMiB}) ->
    gen_server:cast(?MODULE, {record_memory, Pid, HeapMiB, RssMiB}).

%% @doc Record GC pause
-spec record_gc_pause(non_neg_integer()) -> ok.
record_gc_pause(PauseUs) ->
    gen_server:cast(?MODULE, {record_gc_pause, self(), PauseUs}).

%% @doc Record queue depth
-spec record_queue_depth(pid(), non_neg_integer()) -> ok.
record_queue_depth(Pid, Depth) ->
    gen_server:cast(?MODULE, {record_queue_depth, Pid, Depth}).

%% @doc Get latency percentiles for a role
-spec get_latency_percentiles(role()) -> #{
    p50 := float(),
    p95 := float(),
    p99 := float(),
    p999 := float()
}.
get_latency_percentiles(Role) ->
    gen_server:call(?MODULE, {get_latency_percentiles, Role}).

%% @doc Get throughput summary
-spec get_throughput_summary() -> map().
get_throughput_summary() ->
    gen_server:call(?MODULE, get_throughput_summary).

%% @doc Get memory summary
-spec get_memory_summary() -> map().
get_memory_summary() ->
    gen_server:call(?MODULE, get_memory_summary).

%% @doc Get all metrics
-spec get_all_metrics() -> map().
get_all_metrics() ->
    gen_server:call(?MODULE, get_all_metrics).

%% @doc Reset all metrics
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%% @doc Export metrics to file (JSON format)
-spec export_to_file(file:filename()) -> ok | {error, term()}.
export_to_file(Filename) ->
    gen_server:call(?MODULE, {export_to_file, Filename}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(_Opts) ->
    ?LOG_INFO("Starting chaos metrics collector"),
    {ok, #state{start_time = erlang:monotonic_time(millisecond)}}.

handle_call({get_latency_percentiles, Role}, _From, State) ->
    Samples = [S#latency_sample.duration_us
               || S <- State#state.latencies,
                  S#latency_sample.role =:= Role],
    Percentiles = calculate_percentiles(Samples),
    {reply, Percentiles, State};

handle_call(get_throughput_summary, _From, State) ->
    Summary = calculate_throughput_summary(State#state.throughput),
    {reply, Summary, State};

handle_call(get_memory_summary, _From, State) ->
    Summary = calculate_memory_summary(State#state.memory),
    {reply, Summary, State};

handle_call(get_all_metrics, _From, State) ->
    Metrics = #{
        latency => group_latencies_by_role(State#state.latencies),
        throughput => calculate_throughput_summary(State#state.throughput),
        memory => calculate_memory_summary(State#state.memory),
        gc_pauses => calculate_gc_summary(State#state.gc_pauses),
        queue_depths => calculate_queue_summary(State#state.queue_depths),
        sample_count => State#state.sample_count,
        duration_ms => erlang:monotonic_time(millisecond) - State#state.start_time
    },
    {reply, Metrics, State};

handle_call(reset_metrics, _From, _State) ->
    NewState = #state{start_time = erlang:monotonic_time(millisecond)},
    {reply, ok, NewState};

handle_call({export_to_file, Filename}, _From, State) ->
    Metrics = get_all_metrics_internal(State),
    Json = jsx:encode(Metrics),
    Result = file:write_file(Filename, Json),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_latency, Role, Operation, DurationUs}, State) ->
    Sample = #latency_sample{
        role = Role,
        operation = Operation,
        timestamp = erlang:monotonic_time(millisecond),
        duration_us = DurationUs
    },
    {noreply, State#state{
        latencies = [Sample | State#state.latencies],
        sample_count = State#state.sample_count + 1
    }};

handle_cast({record_throughput, Role, MsgCount, Timestamp}, State) ->
    Sample = #throughput_sample{
        role = Role,
        timestamp = Timestamp,
        msg_count = MsgCount
    },
    {noreply, State#state{
        throughput = [Sample | State#state.throughput],
        sample_count = State#state.sample_count + 1
    }};

handle_cast({record_memory, Pid, HeapMiB, RssMiB}, State) ->
    Sample = #memory_sample{
        pid = Pid,
        timestamp = erlang:monotonic_time(millisecond),
        heap_mib = HeapMiB,
        rss_mib = RssMiB
    },
    {noreply, State#state{
        memory = [Sample | State#state.memory],
        sample_count = State#state.sample_count + 1
    }};

handle_cast({record_gc_pause, Pid, PauseUs}, State) ->
    Sample = #gc_sample{
        pid = Pid,
        timestamp = erlang:monotonic_time(millisecond),
        pause_us = PauseUs
    },
    {noreply, State#state{
        gc_pauses = [Sample | State#state.gc_pauses],
        sample_count = State#state.sample_count + 1
    }};

handle_cast({record_queue_depth, Pid, Depth}, State) ->
    Sample = #queue_sample{
        pid = Pid,
        timestamp = erlang:monotonic_time(millisecond),
        depth = Depth
    },
    {noreply, State#state{
        queue_depths = [Sample | State#state.queue_depths],
        sample_count = State#state.sample_count + 1
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Calculate percentiles from samples
calculate_percentiles([]) ->
    #{p50 => 0.0, p95 => 0.0, p99 => 0.0, p999 => 0.0};
calculate_percentiles(Samples) ->
    Sorted = lists:sort(Samples),
    Len = length(Sorted),
    #{
        p50 => percentile(Sorted, Len, 50),
        p95 => percentile(Sorted, Len, 95),
        p99 => percentile(Sorted, Len, 99),
        p999 => percentile(Sorted, Len, 99.9)
    }.

percentile(Sorted, Len, Percentile) ->
    Index = max(1, round((Percentile / 100) * Len)),
    lists:nth(Index, Sorted) * 1.0.

%% @doc Calculate throughput summary
calculate_throughput_summary([]) ->
    #{total_msgs => 0, msg_per_sec => 0.0};
calculate_throughput_summary(Samples) ->
    TotalMsgs = lists:sum([S#throughput_sample.msg_count || S <- Samples]),
    Timestamps = [S#throughput_sample.timestamp || S <- Samples],
    MinTime = lists:min(Timestamps),
    MaxTime = lists:max(Timestamps),
    DurationSec = max(1, (MaxTime - MinTime) div 1000),
    #{
        total_msgs => TotalMsgs,
        msg_per_sec => TotalMsgs / DurationSec,
        by_role => group_throughput_by_role(Samples)
    }.

group_throughput_by_role(Samples) ->
    Grouped = lists:foldl(fun(S, Acc) ->
        Role = S#throughput_sample.role,
        Count = S#throughput_sample.msg_count,
        maps:update_with(Role, fun(V) -> V + Count end, Count, Acc)
    end, #{}, Samples),
    Grouped.

%% @doc Calculate memory summary
calculate_memory_summary([]) ->
    #{avg_heap_mib => 0.0, avg_rss_mib => 0.0, peak_heap_mib => 0.0, peak_rss_mib => 0.0};
calculate_memory_summary(Samples) ->
    HeapValues = [S#memory_sample.heap_mib || S <- Samples],
    RssValues = [S#memory_sample.rss_mib || S <- Samples],
    #{
        avg_heap_mib => avg(HeapValues),
        avg_rss_mib => avg(RssValues),
        peak_heap_mib => lists:max(HeapValues),
        peak_rss_mib => lists:max(RssValues),
        sample_count => length(Samples)
    }.

%% @doc Calculate GC pause summary
calculate_gc_summary([]) ->
    #{avg_pause_us => 0.0, max_pause_us => 0, p99_pause_us => 0.0};
calculate_gc_summary(Samples) ->
    Pauses = [S#gc_sample.pause_us || S <- Samples],
    Sorted = lists:sort(Pauses),
    Len = length(Sorted),
    #{
        avg_pause_us => avg(Pauses),
        max_pause_us => lists:max(Pauses),
        p99_pause_us => percentile(Sorted, Len, 99),
        sample_count => Len
    }.

%% @doc Calculate queue depth summary
calculate_queue_summary([]) ->
    #{avg_depth => 0.0, max_depth => 0, p95_depth => 0.0};
calculate_queue_summary(Samples) ->
    Depths = [S#queue_sample.depth || S <- Samples],
    Sorted = lists:sort(Depths),
    Len = length(Sorted),
    #{
        avg_depth => avg(Depths),
        max_depth => lists:max(Depths),
        p95_depth => percentile(Sorted, Len, 95),
        sample_count => Len
    }.

%% @doc Group latencies by role
group_latencies_by_role(Latencies) ->
    Grouped = lists:foldl(fun(S, Acc) ->
        Role = S#latency_sample.role,
        Duration = S#latency_sample.duration_us,
        maps:update_with(Role, fun(V) -> [Duration | V] end, [Duration], Acc)
    end, #{}, Latencies),
    maps:map(fun(_Role, Durations) ->
        calculate_percentiles(Durations)
    end, Grouped).

%% @doc Average of list
avg([]) -> 0.0;
avg(List) ->
    lists:sum(List) / length(List).

%% @doc Get all metrics (internal)
get_all_metrics_internal(State) ->
    #{
        latency => group_latencies_by_role(State#state.latencies),
        throughput => calculate_throughput_summary(State#state.throughput),
        memory => calculate_memory_summary(State#state.memory),
        gc_pauses => calculate_gc_summary(State#state.gc_pauses),
        queue_depths => calculate_queue_summary(State#state.queue_depths),
        sample_count => State#state.sample_count,
        duration_ms => erlang:monotonic_time(millisecond) - State#state.start_time
    }.
