%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos Tracking System
%%% Tracks and reports on chaos test metrics, failures, and recovery
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_tracking).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    new/0,
    start_test/1,
    end_test/1,
    reset/0,
    log_metric/3,
    log_connection/3,
    log_failure/3,
    get_current_metrics/1,
    get_test_summary/1,
    dump_metrics/1,
    log_metrics_snapshot/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").

-define(SERVER, ?MODULE).

-record(state, {
    tests = #{},           % Per-test metrics
    global_metrics = #{},  % Global metrics
    start_times = #{}      % Test start times
}).

-record(test_metrics, {
    test_id,
    metrics = #{},
    connections = #{},
    failures = [],
    start_time,
    end_time
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

new() ->
    {ok, _} = start_link(),
    ok.

start_test(TestId) ->
    gen_server:call(?SERVER, {start_test, TestId}).

end_test(TestId) ->
    gen_server:call(?SERVER, {end_test, TestId}).

reset() ->
    gen_server:call(?SERVER, reset).

log_metric(TestId, Key, Value) ->
    gen_server:call(?SERVER, {log_metric, TestId, Key, Value}).

log_connection(TestId, WorkerId, ConnCount) ->
    gen_server:call(?SERVER, {log_connection, TestId, WorkerId, ConnCount}).

log_failure(TestId, FailureType, Details) ->
    gen_server:call(?SERVER, {log_failure, TestId, FailureType, Details}).

get_current_metrics(TestId) ->
    gen_server:call(?SERVER, {get_metrics, TestId}).

get_test_summary(TestId) ->
    gen_server:call(?SERVER, {get_summary, TestId}).

dump_metrics(TestId) ->
    gen_server:call(?SERVER, {dump_metrics, TestId}).

log_metrics_snapshot(TestId, Metrics) ->
    gen_server:call(?SERVER, {log_snapshot, TestId, Metrics}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #state{tests = #{}, global_metrics = #{}, start_times = #{}}}.

handle_call({start_test, TestId}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    NewTests = maps:put(TestId, #test_metrics{
        test_id = TestId,
        start_time = StartTime
    }, State#state.tests),
    NewStartTimes = maps:put(TestId, StartTime, State#state.start_times),
    {reply, ok, State#state{tests = NewTests, start_times = NewStartTimes}};

handle_call({end_test, TestId}, _From, State) ->
    EndTime = erlang:monotonic_time(millisecond),
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TestMetrics ->
            UpdatedMetrics = TestMetrics#test_metrics{end_time = EndTime},
            NewTests = maps:put(TestId, UpdatedMetrics, State#state.tests),
            {reply, ok, State#state{tests = NewTests}}
    end;

handle_call(reset, _From, _State) ->
    {reply, ok, #state{tests = #{}, global_metrics = #{}, start_times = #{}}};

handle_call({log_metric, TestId, Key, Value}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, test_not_found}, State};
        TestMetrics ->
            CurrentMetrics = TestMetrics#test_metrics.metrics,
            NewMetrics = maps:put(Key, Value, CurrentMetrics),
            UpdatedTestMetrics = TestMetrics#test_metrics{metrics = NewMetrics},
            NewTests = maps:put(TestId, UpdatedTestMetrics, State#state.tests),
            {reply, ok, State#state{tests = NewTests}}
    end;

handle_call({log_connection, TestId, WorkerId, ConnCount}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, test_not_found}, State};
        TestMetrics ->
            CurrentConnections = TestMetrics#test_metrics.connections,
            NewConnections = maps:put(WorkerId, ConnCount, CurrentConnections),
            UpdatedTestMetrics = TestMetrics#test_metrics{connections = NewConnections},
            NewTests = maps:put(TestId, UpdatedTestMetrics, State#state.tests),
            {reply, ok, State#state{tests = NewTests}}
    end;

handle_call({log_failure, TestId, FailureType, Details}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, test_not_found}, State};
        TestMetrics ->
            NewFailures = [
                {FailureType, Details, erlang:monotonic_time(millisecond)}
                | TestMetrics#test_metrics.failures
            ],
            UpdatedTestMetrics = TestMetrics#test_metrics{failures = NewFailures},
            NewTests = maps:put(TestId, UpdatedTestMetrics, State#state.tests),
            {reply, ok, State#state{tests = NewTests}}
    end;

handle_call({get_metrics, TestId}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TestMetrics ->
            {reply, {ok, TestMetrics#test_metrics.metrics}, State}
    end;

handle_call({get_summary, TestId}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TestMetrics ->
            Summary = format_test_summary(TestId, TestMetrics),
            {reply, {ok, Summary}, State}
    end;

handle_call({dump_metrics, TestId}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            io:format("Test not found: ~p~n", [TestId]),
            {reply, {error, not_found}, State};
        TestMetrics ->
            dump_test_metrics(TestId, TestMetrics),
            {reply, ok, State}
    end;

handle_call({log_snapshot, TestId, Metrics}, _From, State) ->
    case maps:get(TestId, State#state.tests, undefined) of
        undefined ->
            {reply, {error, test_not_found}, State};
        TestMetrics ->
            CurrentMetrics = TestMetrics#test_metrics.metrics,
            NewMetrics = maps:merge(CurrentMetrics, Metrics),
            UpdatedTestMetrics = TestMetrics#test_metrics{metrics = NewMetrics},
            NewTests = maps:put(TestId, UpdatedTestMetrics, State#state.tests),
            {reply, ok, State#state{tests = NewTests}}
    end;

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

format_test_summary(TestId, TestMetrics) ->
    io:format(
        "~n~n========== CHAOS TEST SUMMARY ==========~n" ++
        "Test ID: ~p~n" ++
        "Start Time: ~p~n" ++
        "End Time: ~p~n",
        [TestId, TestMetrics#test_metrics.start_time,
         TestMetrics#test_metrics.end_time]
    ),

    Metrics = TestMetrics#test_metrics.metrics,
    io:format("~nMetrics:~n", []),
    maps:foreach(fun(K, V) ->
        io:format("  ~p: ~p~n", [K, V])
    end, Metrics),

    Connections = TestMetrics#test_metrics.connections,
    case maps:size(Connections) > 0 of
        true ->
            TotalConns = lists:sum(maps:values(Connections)),
            AvgConns = TotalConns div max(1, maps:size(Connections)),
            io:format("~nConnections:~n  Total: ~p~n  Average: ~p~n",
                     [TotalConns, AvgConns]);
        false ->
            ok
    end,

    Failures = TestMetrics#test_metrics.failures,
    case length(Failures) > 0 of
        true ->
            io:format("~nFailures (~p):~n", [length(Failures)]),
            lists:foreach(fun({Type, Details, _Time}) ->
                io:format("  - ~p: ~p~n", [Type, Details])
            end, Failures);
        false ->
            io:format("~nFailures: None~n", [])
    end,

    io:format("~n========================================~n~n", []).

dump_test_metrics(TestId, TestMetrics) ->
    io:format(
        "~n~n========== DETAILED METRICS DUMP ==========~n" ++
        "Test ID: ~p~n",
        [TestId]
    ),

    StartTime = TestMetrics#test_metrics.start_time,
    EndTime = TestMetrics#test_metrics.end_time,
    Duration = case EndTime of
        undefined -> unknown;
        _ -> EndTime - StartTime
    end,

    io:format("Duration: ~p ms~n", [Duration]),

    Metrics = TestMetrics#test_metrics.metrics,
    io:format("~nAll Metrics:~n", []),
    SortedMetrics = lists:sort(maps:to_list(Metrics)),
    lists:foreach(fun({K, V}) ->
        format_metric_value(K, V)
    end, SortedMetrics),

    io:format("~n=========================================~n~n", []).

format_metric_value(Key, Value) when is_float(Value) ->
    io:format("  ~p: ~.4f~n", [Key, Value]);
format_metric_value(Key, Value) ->
    io:format("  ~p: ~p~n", [Key, Value]).
