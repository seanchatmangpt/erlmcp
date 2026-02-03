%%%-------------------------------------------------------------------
%%% @doc
%%% Load Generator
%%%
 This module implements high-performance load generators for different
 protocols with configurable behavior and metrics collection.
%%%
 Features:
%%% - Protocol-specific request generation (HTTP, WebSocket, SSE, TCP)
%%% - Configurable think times and request rates
%%% - Automatic rate adjustment based on errors
%%% - Comprehensive metrics tracking
%%% - Backpressure handling
%%% - Connection pooling optimization
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_generator).

-behaviour(gen_server).

-export([start_link/2, get_stats/1, pause/1, resume/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-record(state, {
    generator_id :: binary(),
    config :: load_test_config(),
    protocol :: atom(),
    target :: string(),
    connections :: ets:tid(),
    stats :: load_generator_stats(),
    paused :: boolean(),
    rate_limiter :: pid(),
    connection_pool :: pid(),
    metrics_collector :: pid(),
    batch_size :: pos_integer(),
    think_time :: pos_integer(),
    current_rate :: pos_integer(),
    max_rate :: pos_integer(),
    error_count :: pos_integer(),
    last_request_time :: pos_integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(binary(), load_test_config()) -> {ok, pid()} | {error, term()}.
start_link(GeneratorId, Config) ->
    gen_server:start_link({local, GeneratorId}, ?MODULE, [GeneratorId, Config], []).

-spec get_stats(pid()) -> {ok, load_generator_stats()} | {error, term()}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

-spec pause(pid()) -> ok.
pause(Pid) ->
    gen_server:cast(Pid, pause).

-spec resume(pid()) -> ok.
resume(Pid) ->
    gen_server:cast(Pid, resume).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([binary(), load_test_config()]) -> {ok, state()}.
init([GeneratorId, Config]) ->
    process_flag(trap_exit, true),

    %% Initialize state
    Protocol = maps:get(protocol, Config),
    Target = maps:get(target_endpoint, Config),
    ThinkTime = maps:get(think_time, Config, ?DEFAULT_THINK_TIME),
    BatchSize = maps:get(batch_size, Config, 100),

    InitialStats = #{
        active => 0,
        completed => 0,
        failed => 0,
        in_progress => 0,
        avg_latency => 0.0,
        p95_latency => 0.0,
        p99_latency => 0.0
    },

    State = #state{
        generator_id = GeneratorId,
        config = Config,
        protocol = Protocol,
        target = Target,
        stats = InitialStats,
        paused = false,
        think_time = ThinkTime,
        batch_size = BatchSize,
        current_rate = maps:get(request_rate, Config, ?DEFAULT_RATE),
        max_rate = maps:get(max_rate, Config, 1000),
        error_count = 0,
        last_request_time = erlang:system_time(millisecond)
    },

    %% Initialize connection pool
    {ok, NewState} = initialize_connection_pool(State),

    %% Initialize rate limiter
    {ok, State1} = initialize_rate_limiter(NewState),

    %% Initialize metrics collector
    {ok, MetricsPid} = erlmcp_load_testing_metrics:get_collector(Config),
    State2 = State1#state{metrics_collector = MetricsPid},

    %% Initialize connection tracking
    ConnTable = ets:new(connections, [set, private]),
    State3 = State2#state{connections = ConnTable},

    %% Start load generation
    {ok, State3}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                       {reply, term(), state()} | {stop, term(), state()}.
handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_cast(pause, State) ->
    %% Stop load generation
    NewState = State#state{paused = true},

    %% Update metrics
    erlmcp_load_testing_metrics:generator_paused(
        State#state.generator_id, State#state.stats),

    {noreply, NewState};

handle_cast(resume, State) ->
    %% Resume load generation
    NewState = State#state{paused = false},

    %% Update metrics
    erlmcp_load_testing_metrics:generator_resumed(
        State#state.generator_id, State#state.stats),

    %% Continue generating load
    continue_generation(NewState),

    {noreply, NewState};

handle_cast(stop, State) ->
    %% Stop load generation
    NewState = State#state{paused = true},

    %% Update metrics
    erlmcp_load_testing_metrics:generator_stopped(
        State#state.generator_id, State#state.stats),

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({rate_adjust, NewRate}, State) ->
    %% Adjust rate based on error rate
    NewState = State#state{current_rate = NewRate},
    continue_generation(NewState),
    {noreply, NewState};

handle_info(request_batch_complete, State) ->
    %% Update stats after batch completion
    {ok, UpdatedStats} = update_stats(State),
    NewState = State#state{stats = UpdatedStats},
    continue_generation(NewState),
    {noreply, NewState};

handle_info(connection_error, State) ->
    %% Handle connection errors
    NewErrorCount = State#state.error_count + 1,
    NewState = State#state{error_count = NewErrorCount},

    %% Adjust rate if error rate is high
    case NewErrorCount / (State#state.stats#completed + 1) > 0.1 of
        true ->
            AdjustedRate = max(1, State#state.current_rate div 2),
            erlmcp_load_testing_rate_limiter:set_rate(
                State#state.rate_limiter, AdjustedRate);
        false ->
            ok
    end,

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Cleanup resources
    ets:delete(State#state.connections),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize connection pool
-spec initialize_connection_pool(state()) -> {ok, state()}.
initialize_connection_pool(State) ->
    Protocol = State#state.protocol,
    Config = State#state.config,
    PoolSize = maps:get(connection_pool_size, Config, ?DEFAULT_POOL_SIZE),

    case Protocol of
        http ->
            {ok, PoolPid} = erlmcp_load_testing_http_pool:start(
                State#state.target, PoolSize),
            State#state{connection_pool = PoolPid};
        websocket ->
            {ok, PoolPid} = erlmcp_load_testing_ws_pool:start(
                State#state.target, PoolSize),
            State#state{connection_pool = PoolPid};
        sse ->
            {ok, PoolPid} = erlmcp_load_testing_sse_pool:start(
                State#state.target, PoolSize),
            State#state{connection_pool = PoolPid};
        tcp ->
            {ok, PoolPid} = erlmcp_load_testing_tcp_pool:start(
                State#state.target, PoolSize),
            State#state{connection_pool = PoolPid}
    end.

%% Initialize rate limiter
-spec initialize_rate_limiter(state()) -> {ok, state()}.
initialize_rate_limiter(State) ->
    InitialRate = State#state.current_rate,
    {ok, RateLimiterPid} = erlmcp_load_testing_rate_limiter:start(InitialRate),
    {ok, State#state{rate_limiter = RateLimiterPid}}.

%% Continue generating load
-spec continue_generation(state()) -> ok.
continue_generation(State) ->
    case State#state.paused of
        true ->
            ok;
        false ->
            %% Send requests at current rate
            send_request_batch(State)
    end.

%% Send request batch
-spec send_request_batch(state()) -> ok.
send_request_batch(State) ->
    CurrentTime = erlang:system_time(millisecond),
    TimeSinceLast = CurrentTime - State#state.last_request_time,

    %% Check rate limit
    case erlmcp_load_testing_rate_limiter:can_send(State#state.rate_limiter) of
        true ->
            %% Send batch of requests
            send_requests(State, State#state.batch_size),
            State#state{last_request_time = CurrentTime};
        false ->
            %% Wait and retry
            erlang:send_after(TimeSinceLast, self(), request_batch_complete)
    end.

%% Send requests
-spec send_requests(state(), pos_integer()) -> ok.
send_requests(State, Count) ->
    lists:foreach(fun(_) ->
                    send_single_request(State)
                end, lists:seq(1, Count)).

%% Send single request
-spec send_single_request(state()) -> ok.
send_single_request(State) ->
    Protocol = State#state.protocol,
    StartTime = erlang:system_time(millisecond),

    case Protocol of
        http ->
            send_http_request(State, StartTime);
        websocket ->
            send_websocket_request(State, StartTime);
        sse ->
            send_sse_request(State, StartTime);
        tcp ->
            send_tcp_request(State, StartTime)
    end.

%% Send HTTP request
-spec send_http_request(state(), pos_integer()) -> ok.
send_http_request(State, StartTime) ->
    Pool = State#state.connection_pool,
    Headers = maps:get(headers, State#state.config, #{}),
    Payload = generate_payload(State),

    %% Async HTTP request
    erlmcp_load_testing_http_pool:request(Pool, Headers, Payload,
                                          fun(Result) ->
                                              handle_http_response(
                                                  State, StartTime, Result)
                                          end).

%% Send WebSocket request
-spec send_websocket_request(state(), pos_integer()) -> ok.
send_websocket_request(State, StartTime) ->
    Pool = State#state.connection_pool,
    Payload = generate_payload(State),

    %% Async WebSocket message
    erlmcp_load_testing_ws_pool:send_message(Pool, Payload,
                                            fun(Result) ->
                                                handle_ws_response(
                                                    State, StartTime, Result)
                                            end).

%% Send SSE request
-spec send_sse_request(state(), pos_integer()) -> ok.
send_sse_request(State, StartTime) ->
    Pool = State#state.connection_pool,
    Payload = generate_payload(State),

    %% Async SSE event
    erlmcp_load_testing_sse_pool:send_event(Pool, Payload,
                                           fun(Result) ->
                                               handle_sse_response(
                                                   State, StartTime, Result)
                                           end).

%% Send TCP request
-spec send_tcp_request(state(), pos_integer()) -> ok.
send_tcp_request(State, StartTime) ->
    Pool = State#state.connection_pool,
    Payload = generate_payload(State),

    %% Async TCP request
    erlmcp_load_testing_tcp_pool:send_request(Pool, Payload,
                                              fun(Result) ->
                                                  handle_tcp_response(
                                                      State, StartTime, Result)
                                              end).

%% Generate request payload
-spec generate_payload(state()) -> binary().
generate_payload(State) ->
    PayloadSize = maps:get(payload_size, State#state.config, ?DEFAULT_PAYLOAD_SIZE),
    <<Rand:PayloadSize/unit:8>>.

%% Handle HTTP response
-spec handle_http_response(state(), pos_integer(), term()) -> ok.
handle_http_response(State, StartTime, Result) ->
    EndTime = erlang:system_time(millisecond),
    Latency = EndTime - StartTime,

    case Result of
        {ok, Status} when Status >= 200, Status < 300 ->
            %% Success
            update_success_metrics(State, Latency);
        {error, Reason} ->
            %% Error
            update_error_metrics(State, Latency, Reason)
    end.

%% Handle WebSocket response
-spec handle_ws_response(state(), pos_integer(), term()) -> ok.
handle_ws_response(State, StartTime, Result) ->
    EndTime = erlang:system_time(millisecond),
    Latency = EndTime - StartTime,

    case Result of
        ok ->
            %% Success
            update_success_metrics(State, Latency);
        {error, Reason} ->
            %% Error
            update_error_metrics(State, Latency, Reason)
    end.

%% Handle SSE response
-spec handle_sse_response(state(), pos_integer(), term()) -> ok.
handle_sse_response(State, StartTime, Result) ->
    EndTime = erlang:system_time(millisecond),
    Latency = EndTime - StartTime,

    case Result of
        ok ->
            %% Success
            update_success_metrics(State, Latency);
        {error, Reason} ->
            %% Error
            update_error_metrics(State, Latency, Reason)
    end.

%% Handle TCP response
-spec handle_tcp_response(state(), pos_integer(), term()) -> ok.
handle_tcp_response(State, StartTime, Result) ->
    EndTime = erlang:system_time(millisecond),
    Latency = EndTime - StartTime,

    case Result of
        {ok, _} ->
            %% Success
            update_success_metrics(State, Latency);
        {error, Reason} ->
            %% Error
            update_error_metrics(State, Latency, Reason)
    end.

%% Update success metrics
-spec update_success_metrics(state(), pos_integer()) -> ok.
update_success_metrics(State, Latency) ->
    %% Update stats
    OldStats = State#state.stats,
    NewStats = OldStats#{
        completed => OldStats#completed + 1,
        in_progress => max(0, OldStats#in_progress - 1),
        avg_latency => calculate_average(OldStats#avg_latency, Latency),
        p95_latency => calculate_percentile(OldStats#p95_latency, Latency, 95),
        p99_latency => calculate_percentile(OldStats#p99_latency, Latency, 99)
    },

    State#state{stats = NewStats},

    %% Collect metrics
    erlmcp_load_testing_metrics:record_success(
        State#state.generator_id, Latency).

%% Update error metrics
-spec update_error_metrics(state(), pos_integer(), term()) -> ok.
update_error_metrics(State, Latency, Reason) ->
    %% Update stats
    OldStats = State#state.stats,
    NewStats = OldStats#{
        failed => OldStats#failed + 1,
        in_progress => max(0, OldStats#in_progress - 1)
    },

    State#state{stats = NewStats, error_count = State#state.error_count + 1},

    %% Collect metrics
    erlmcp_load_testing_metrics:record_error(
        State#state.generator_id, Latency, Reason),

    %% Send error notification
    self() ! connection_error.

%% Update statistics
-spec update_stats(state()) -> {ok, load_generator_stats()}.
update_stats(State) ->
    Stats = State#state.stats,

    %% Calculate percentiles
    HistData = ets:tab2list(connections),
    Latencies = [Lat || #{latency := Lat} <- HistData],

    NewStats = Stats#{
        p95_latency => calculate_percentile_list(Latencies, 95),
        p99_latency => calculate_percentile_list(Latencies, 99)
    },

    {ok, NewStats}.

%% Calculate moving average
-spec calculate_average(float(), pos_integer()) -> float().
calculate_average(OldAvg, NewValue) ->
    (OldAvg * 0.9) + (NewValue * 0.1).

%% Calculate percentile
-spec calculate_percentile(float(), pos_integer(), pos_integer()) -> float().
calculate_percentile(Current, NewValue, Percentile) ->
    %% Simple percentile calculation
    case NewValue > Current of
        true ->
            NewValue * (Percentile / 100);
        false ->
            Current
    end.

%% Calculate percentile from list
-spec calculate_percentile_list([pos_integer()], pos_integer()) -> float().
calculate_percentile_list(List, Percentile) when List =/= [] ->
    Sorted = lists:sort(List),
    Length = length(Sorted),
    Index = trunc((Length * Percentile) / 100),
    lists:nth(Index + 1, Sorted);
calculate_percentile_list(_, _) ->
    0.0.