-module(erlmcp_metrics_server).

-behaviour(gen_server).

%% API
-export([start_link/0, get_metrics/0, record_message/1, record_error/0, record_latency/1,
         get_concurrent_connections/0, increment_connections/1, decrement_connections/1,
         reset_metrics/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Internal state record
-record(state,
        {start_time :: integer(),              % Milliseconds since epoch
         total_messages = 0 :: non_neg_integer(),
         total_errors = 0 :: non_neg_integer(),
         concurrent_connections = 0 :: non_neg_integer(),
         latencies = [] :: [non_neg_integer()],  % Last 10000 latency measurements
         latency_window = 0 :: non_neg_integer(), % Current window sum
         message_rate_window = 0 :: non_neg_integer(), % Messages in current 1-sec window
         window_timer :: reference() | undefined,
         error_rate_window = 0 :: non_neg_integer(),  % Errors in current 1-sec window
         last_metrics = #{} :: map()}).  % Cached metrics for HTTP responses

-define(SERVER, ?MODULE).
-define(LATENCY_WINDOW_SIZE, 10000).
-define(RATE_WINDOW_INTERVAL, 1000).  % 1 second in ms

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?SERVER, get_metrics, 5000).

-spec record_message(pos_integer()) -> ok.
record_message(Count) when is_integer(Count), Count > 0 ->
    gen_server:cast(?SERVER, {record_message, Count}).

-spec record_error() -> ok.
record_error() ->
    gen_server:cast(?SERVER, record_error).

-spec record_latency(number()) -> ok.
record_latency(LatencyMs) when is_number(LatencyMs) ->
    gen_server:cast(?SERVER, {record_latency, LatencyMs}).

-spec get_concurrent_connections() -> non_neg_integer().
get_concurrent_connections() ->
    gen_server:call(?SERVER, get_concurrent_connections).

-spec increment_connections(non_neg_integer()) -> ok.
increment_connections(Count) when is_integer(Count), Count >= 0 ->
    gen_server:cast(?SERVER, {increment_connections, Count}).

-spec decrement_connections(non_neg_integer()) -> ok.
decrement_connections(Count) when is_integer(Count), Count >= 0 ->
    gen_server:cast(?SERVER, {decrement_connections, Count}).

-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?SERVER, reset_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?LOG_INFO("Metrics server starting~n", []),
    {ok, TimerRef} = timer:send_interval(?RATE_WINDOW_INTERVAL, reset_rate_window),
    State = #state{start_time = erlang:system_time(millisecond), window_timer = TimerRef},
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_metrics, _From, State) ->
    Metrics = calculate_metrics(State),
    {reply, Metrics, State#state{last_metrics = Metrics}};
handle_call(get_concurrent_connections, _From, State) ->
    {reply, State#state.concurrent_connections, State};
handle_call(reset_metrics, _From, _State) ->
    NewState = #state{start_time = erlang:system_time(millisecond), window_timer = undefined},
    {ok, TimerRef} = timer:send_interval(?RATE_WINDOW_INTERVAL, reset_rate_window),
    {reply, ok, NewState#state{window_timer = TimerRef}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_message, Count}, State) ->
    NewState =
        State#state{total_messages = State#state.total_messages + Count,
                    message_rate_window = State#state.message_rate_window + Count},
    {noreply, NewState};
handle_cast(record_error, State) ->
    NewState =
        State#state{total_errors = State#state.total_errors + 1,
                    error_rate_window = State#state.error_rate_window + 1},
    {noreply, NewState};
handle_cast({record_latency, LatencyMs}, State) ->
    Latencies = State#state.latencies,
    NewLatencies =
        case length(Latencies) >= ?LATENCY_WINDOW_SIZE of
            true ->
                lists:sublist(Latencies, 1, ?LATENCY_WINDOW_SIZE - 1) ++ [round(LatencyMs)];
            false ->
                Latencies ++ [round(LatencyMs)]
        end,
    {noreply, State#state{latencies = NewLatencies}};
handle_cast({increment_connections, Count}, State) ->
    NewConnections = State#state.concurrent_connections + Count,
    {noreply, State#state{concurrent_connections = NewConnections}};
handle_cast({decrement_connections, Count}, State) ->
    NewConnections = max(0, State#state.concurrent_connections - Count),
    {noreply, State#state{concurrent_connections = NewConnections}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(reset_rate_window, State) ->
    % Window metrics are already captured in calculate_metrics, just reset them
    NewState = State#state{message_rate_window = 0, error_rate_window = 0},
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.window_timer of
        undefined ->
            ok;
        TimerRef ->
            timer:cancel(TimerRef),
            ok
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec calculate_metrics(#state{}) -> map().
calculate_metrics(State) ->
    Uptime = erlang:system_time(millisecond) - State#state.start_time,
    MessageRate = State#state.message_rate_window,
    ErrorRate = State#state.error_rate_window,
    Latencies = State#state.latencies,
    LatencyStats = calculate_latency_stats(Latencies),

    Nodes = erlang:nodes([connected]) ++ [erlang:node()],
    NodeMetrics = lists:map(fun get_node_metrics/1, Nodes),

    #{timestamp => erlang:system_time(millisecond),
      uptime_ms => Uptime,
      uptime_human => format_uptime(Uptime),
      concurrent_connections => State#state.concurrent_connections,
      total_messages => State#state.total_messages,
      total_errors => State#state.total_errors,
      message_rate_per_sec => MessageRate,
      error_rate_per_sec => ErrorRate,
      error_percentage => safe_divide(ErrorRate, MessageRate + ErrorRate),
      latency_stats => LatencyStats,
      latency_samples_count => length(Latencies),
      nodes => NodeMetrics,
      system_metrics => get_system_metrics()}.

-spec calculate_latency_stats([number()]) -> map().
calculate_latency_stats([]) ->
    #{p50 => 0,
      p95 => 0,
      p99 => 0,
      min => 0,
      max => 0,
      avg => 0};
calculate_latency_stats(Latencies) ->
    Sorted = lists:sort(Latencies),
    Count = length(Sorted),
    P50Index = max(1, round(Count * 0.50)),
    P95Index = max(1, round(Count * 0.95)),
    P99Index = max(1, round(Count * 0.99)),

    P50 = lists:nth(P50Index, Sorted),
    P95 = lists:nth(P95Index, Sorted),
    P99 = lists:nth(P99Index, Sorted),
    Min = lists:nth(1, Sorted),
    Max = lists:nth(Count, Sorted),
    Avg = round(lists:sum(Sorted) / Count),

    #{p50 => P50,
      p95 => P95,
      p99 => P99,
      min => Min,
      max => Max,
      avg => Avg}.

-spec get_node_metrics(atom()) -> map().
get_node_metrics(Node) ->
    try
        case rpc:call(Node, erlang, statistics, [runtime], 5000) of
            {_Total, _Since} ->
                Memory = rpc:call(Node, erlang, memory, [], 5000),
                ProcessCount = rpc:call(Node, erlang, system_info, [process_count], 5000),
                #{node => Node,
                  process_count => ProcessCount,
                  memory => normalize_memory(Memory)};
            _ ->
                #{node => Node, status => unreachable}
        end
    catch
        _:_ ->
            #{node => Node, status => error}
    end.

-spec get_system_metrics() -> map().
get_system_metrics() ->
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    PortCount = erlang:system_info(port_count),

    #{memory => normalize_memory(Memory),
      process_count => ProcessCount,
      port_count => PortCount,
      schedulers => erlang:system_info(schedulers),
      schedulers_online => erlang:system_info(schedulers_online)}.

-spec normalize_memory(list() | map()) -> map().
normalize_memory(Memory) when is_list(Memory) ->
    maps:from_list(Memory);
normalize_memory(Memory) when is_map(Memory) ->
    Memory.

-spec safe_divide(number(), number()) -> float().
safe_divide(_Num, 0) ->
    0.0;
safe_divide(Num, Denom) ->
    Num / Denom.

-spec format_uptime(non_neg_integer()) -> binary().
format_uptime(Ms) ->
    TotalSeconds = Ms div 1000,
    Days = TotalSeconds div 86400,
    Hours = TotalSeconds rem 86400 div 3600,
    Minutes = TotalSeconds rem 3600 div 60,
    Seconds = TotalSeconds rem 60,

    iolist_to_binary(io_lib:format("~Bd ~Bh ~Bm ~Bs", [Days, Hours, Minutes, Seconds])).
