%%%-------------------------------------------------------------------
%%% @doc
%%% ERLMCP Metrics Collector - Real-Time Performance Measurement
%%%
%%% Collects comprehensive metrics during stress testing:
%%% - Connection statistics (count, rate, distribution)
%%% - Message metrics (throughput, latency distribution)
%%% - Resource usage (memory, CPU, GC)
%%% - Error rates and failure modes
%%% - Percentile latency distributions
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_metrics_collector).

-behaviour(gen_server).

-export([
    start_link/0,
    start_link/1,
    record_request/3,
    record_connection/1,
    record_disconnection/1,
    record_error/2,
    get_metrics/0,
    get_latency_stats/0,
    get_throughput_stats/0,
    get_error_stats/0,
    stop/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    metrics = #{} :: map(),
    latencies = [] :: list(),
    requests = 0 :: integer(),
    errors = 0 :: integer(),
    connections_active = 0 :: integer(),
    start_time :: integer(),
    sample_interval = 1000 :: integer()
}).

%% ===================================================================
%% API
%% ===================================================================

start_link() ->
    start_link(#{}).

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

record_request(StartTime, EndTime, Result) ->
    gen_server:cast(?MODULE, {record_request, StartTime, EndTime, Result}).

record_connection(ConnectionId) ->
    gen_server:cast(?MODULE, {record_connection, ConnectionId}).

record_disconnection(ConnectionId) ->
    gen_server:cast(?MODULE, {record_disconnection, ConnectionId}).

record_error(ErrorType, Details) ->
    gen_server:cast(?MODULE, {record_error, ErrorType, Details}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_latency_stats() ->
    gen_server:call(?MODULE, get_latency_stats).

get_throughput_stats() ->
    gen_server:call(?MODULE, get_throughput_stats).

get_error_stats() ->
    gen_server:call(?MODULE, get_error_stats).

stop() ->
    gen_server:call(?MODULE, stop).

%% ===================================================================
%% Callbacks
%% ===================================================================

init(_Options) ->
    State = #state{
        start_time = erlang:system_time(millisecond),
        metrics = #{}
    },
    {ok, State}.

handle_call(get_metrics, _From, State) ->
    Metrics = build_metrics_snapshot(State),
    {reply, Metrics, State};

handle_call(get_latency_stats, _From, State) ->
    Stats = calculate_latency_stats(State#state.latencies),
    {reply, Stats, State};

handle_call(get_throughput_stats, _From, State) ->
    Stats = calculate_throughput_stats(State),
    {reply, Stats, State};

handle_call(get_error_stats, _From, State) ->
    Stats = #{
        total_errors => State#state.errors,
        error_rate => calculate_error_rate(State),
        active_connections => State#state.connections_active
    },
    {reply, Stats, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Request, From, State) ->
    io:format("Unexpected call: ~w from ~w~n", [Request, From]),
    {reply, {error, unknown_request}, State}.

handle_cast({record_request, StartTime, EndTime, Result}, State) ->
    Latency = EndTime - StartTime,
    NewLatencies = [Latency | State#state.latencies],
    NewRequests = State#state.requests + 1,
    NewErrors = case Result of
        success -> State#state.errors;
        _ -> State#state.errors + 1
    end,
    {noreply, State#state{
        latencies = NewLatencies,
        requests = NewRequests,
        errors = NewErrors
    }};

handle_cast({record_connection, _ConnectionId}, State) ->
    NewConnections = State#state.connections_active + 1,
    {noreply, State#state{connections_active = NewConnections}};

handle_cast({record_disconnection, _ConnectionId}, State) ->
    NewConnections = max(0, State#state.connections_active - 1),
    {noreply, State#state{connections_active = NewConnections}};

handle_cast({record_error, _ErrorType, _Details}, State) ->
    {noreply, State#state{errors = State#state.errors + 1}};

handle_cast(Message, State) ->
    io:format("Unexpected cast: ~w~n", [Message]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Unexpected info: ~w~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Internal Functions
%% ===================================================================

build_metrics_snapshot(State) ->
    ElapsedMs = erlang:system_time(millisecond) - State#state.start_time,
    ElapsedSec = ElapsedMs / 1000,

    #{
        timestamp => erlang:system_time(millisecond),
        elapsed_seconds => ElapsedSec,
        active_connections => State#state.connections_active,
        total_requests => State#state.requests,
        total_errors => State#state.errors,
        error_rate => calculate_error_rate(State),
        throughput_rps => (State#state.requests / ElapsedSec),
        memory_mb => get_memory_usage(),
        cpu_percent => get_cpu_usage(),
        gc_stats => get_gc_stats(),
        latency_stats => calculate_latency_stats(State#state.latencies)
    }.

calculate_latency_stats(Latencies) when length(Latencies) =:= 0 ->
    #{
        min => 0,
        max => 0,
        avg => 0,
        p50 => 0,
        p95 => 0,
        p99 => 0,
        count => 0
    };

calculate_latency_stats(Latencies) ->
    Sorted = lists:sort(Latencies),
    Count = length(Sorted),

    Min = lists:min(Sorted),
    Max = lists:max(Sorted),
    Avg = lists:sum(Sorted) / Count,

    P50Index = round(Count * 0.50),
    P95Index = round(Count * 0.95),
    P99Index = round(Count * 0.99),

    P50 = get_percentile(Sorted, P50Index),
    P95 = get_percentile(Sorted, P95Index),
    P99 = get_percentile(Sorted, P99Index),

    #{
        min => Min,
        max => Max,
        avg => Avg,
        p50 => P50,
        p95 => P95,
        p99 => P99,
        count => Count
    }.

get_percentile(Sorted, Index) when Index > 0, Index =< length(Sorted) ->
    lists:nth(Index, Sorted);
get_percentile(Sorted, _Index) ->
    case length(Sorted) of
        0 -> 0;
        Len -> lists:nth(Len, Sorted)
    end.

calculate_throughput_stats(State) ->
    ElapsedSec = (erlang:system_time(millisecond) - State#state.start_time) / 1000,

    case ElapsedSec > 0 of
        true ->
            ThroughputRps = State#state.requests / ElapsedSec,
            #{
                requests_per_second => ThroughputRps,
                total_requests => State#state.requests,
                elapsed_seconds => ElapsedSec
            };
        false ->
            #{
                requests_per_second => 0,
                total_requests => State#state.requests,
                elapsed_seconds => 0
            }
    end.

calculate_error_rate(State) ->
    case State#state.requests of
        0 -> 0.0;
        Total -> (State#state.errors / Total) * 100
    end.

get_memory_usage() ->
    case catch erlang:memory() of
        {error, _} -> 0;
        Memory when is_list(Memory) ->
            Total = proplists:get_value(total, Memory, 0),
            Total / (1024 * 1024);  % Convert to MB
        _ -> 0
    end.

get_cpu_usage() ->
    % Placeholder - would implement actual CPU measurement
    0.0.

get_gc_stats() ->
    case catch erlang:statistics(garbage_collection) of
        {error, _} -> #{};
        Stats when is_tuple(Stats) ->
            {GCCount, WordsReclaimed, _} = Stats,
            #{
                gc_count => GCCount,
                words_reclaimed => WordsReclaimed
            };
        _ -> #{}
    end.
