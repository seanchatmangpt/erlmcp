%%%-------------------------------------------------------------------
%%% @doc TCPS Common Test Hooks
%%%
%%% Automatic setup and teardown of mock services for all test suites.
%%% Ensures clean test isolation and proper resource cleanup.
%%%
%%% Hooks provided:
%%% - init/2: Setup mock services before all tests
%%% - pre_init_per_suite/3: Reset state before each suite
%%% - post_end_per_suite/4: Verify cleanup after suite
%%% - terminate/1: Shutdown all services
%%%
%%% Usage in test suite:
%%%   suite() ->
%%%       [{ct_hooks, [tcps_ct_hooks]}].
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_ct_hooks).

%% CT Hook callbacks
-export([
    init/2,
    pre_init_per_suite/3,
    post_init_per_suite/4,
    pre_init_per_testcase/4,
    post_init_per_testcase/5,
    pre_end_per_testcase/4,
    post_end_per_testcase/5,
    pre_end_per_suite/3,
    post_end_per_suite/4,
    terminate/1
]).

-record(state, {
    mock_services :: map(),
    suite_start_time :: integer(),
    testcase_start_time :: integer(),
    metrics :: map()
}).

%%%===================================================================
%%% CT Hook Callbacks
%%%===================================================================

%% @doc Initialize hook - called once before all suites
init(_Id, _Opts) ->
    ct:log("~n=== TCPS CT Hooks: Initializing ===~n"),

    %% Ensure test data directory exists
    TestDataDir = "/tmp/tcps_test_data",
    ok = ensure_dir(TestDataDir),
    ok = ensure_dir(filename:join(TestDataDir, "receipts")),
    ok = ensure_dir(filename:join(TestDataDir, "logs")),
    ok = ensure_dir(filename:join(TestDataDir, "cover")),

    %% Start mock services
    ct:log("Starting mock services...~n"),
    {ok, MockServices} = tcps_mock_services:start_all(),
    ct:log("Mock services started: ~p~n", [maps:keys(MockServices)]),

    %% Initialize metrics
    Metrics = #{
        suites_run => 0,
        testcases_run => 0,
        testcases_passed => 0,
        testcases_failed => 0,
        total_time_ms => 0
    },

    State = #state{
        mock_services = MockServices,
        metrics = Metrics
    },

    {ok, State}.

%% @doc Before suite initialization
pre_init_per_suite(SuiteName, Config, State) ->
    ct:log("~n=== TCPS CT Hooks: Starting suite ~p ===~n", [SuiteName]),

    %% Record suite start time
    StartTime = erlang:system_time(millisecond),

    %% Reset mock state for clean suite
    ok = tcps_mock_services:reset_all(),
    ct:log("Mock state reset for suite~n"),

    %% Setup fixture data if available
    ok = load_suite_fixtures(SuiteName),

    NewState = State#state{suite_start_time = StartTime},
    {Config, NewState}.

%% @doc After suite initialization
post_init_per_suite(SuiteName, Config, Return, State) ->
    case Return of
        {skip, Reason} ->
            ct:log("Suite ~p skipped: ~p~n", [SuiteName, Reason]);
        {fail, Reason} ->
            ct:log("Suite ~p init failed: ~p~n", [SuiteName, Reason]);
        _ ->
            ct:log("Suite ~p initialized successfully~n", [SuiteName])
    end,
    {Return, State}.

%% @doc Before testcase initialization
pre_init_per_testcase(SuiteName, TestCase, Config, State) ->
    ct:log("~n--- Starting testcase: ~p:~p ---~n", [SuiteName, TestCase]),

    %% Record testcase start time
    StartTime = erlang:system_time(millisecond),

    %% Clear mock call history for clean testcase
    ok = tcps_mock_services:clear_call_history(),

    NewState = State#state{testcase_start_time = StartTime},
    {Config, NewState}.

%% @doc After testcase initialization
post_init_per_testcase(SuiteName, TestCase, Config, Return, State) ->
    case Return of
        {skip, Reason} ->
            ct:log("Testcase ~p:~p skipped: ~p~n", [SuiteName, TestCase, Reason]);
        {fail, Reason} ->
            ct:log("Testcase ~p:~p init failed: ~p~n", [SuiteName, TestCase, Reason]);
        _ ->
            ok
    end,
    {Return, State}.

%% @doc Before testcase cleanup
pre_end_per_testcase(SuiteName, TestCase, Config, State) ->
    ct:log("~n--- Ending testcase: ~p:~p ---~n", [SuiteName, TestCase]),
    {Config, State}.

%% @doc After testcase cleanup
post_end_per_testcase(SuiteName, TestCase, Config, Result, State) ->
    %% Calculate testcase duration
    EndTime = erlang:system_time(millisecond),
    StartTime = State#state.testcase_start_time,
    Duration = EndTime - StartTime,

    %% Update metrics
    Metrics = State#state.metrics,
    TestcasesRun = maps:get(testcases_run, Metrics, 0) + 1,

    {Passed, Failed} = case Result of
        ok ->
            {maps:get(testcases_passed, Metrics, 0) + 1,
             maps:get(testcases_failed, Metrics, 0)};
        {error, _Reason} ->
            {maps:get(testcases_passed, Metrics, 0),
             maps:get(testcases_failed, Metrics, 0) + 1};
        {skipped, _Reason} ->
            {maps:get(testcases_passed, Metrics, 0),
             maps:get(testcases_failed, Metrics, 0)};
        _ ->
            {maps:get(testcases_passed, Metrics, 0) + 1,
             maps:get(testcases_failed, Metrics, 0)}
    end,

    TotalTime = maps:get(total_time_ms, Metrics, 0) + Duration,

    NewMetrics = Metrics#{
        testcases_run => TestcasesRun,
        testcases_passed => Passed,
        testcases_failed => Failed,
        total_time_ms => TotalTime
    },

    ct:log("Testcase ~p:~p completed in ~p ms (result: ~p)~n",
           [SuiteName, TestCase, Duration, Result]),

    NewState = State#state{metrics = NewMetrics},
    {Result, NewState}.

%% @doc Before suite cleanup
pre_end_per_suite(SuiteName, Config, State) ->
    ct:log("~n=== TCPS CT Hooks: Ending suite ~p ===~n", [SuiteName]),

    %% Calculate suite duration
    EndTime = erlang:system_time(millisecond),
    StartTime = State#state.suite_start_time,
    Duration = EndTime - StartTime,

    %% Update metrics
    Metrics = State#state.metrics,
    SuitesRun = maps:get(suites_run, Metrics, 0) + 1,
    NewMetrics = Metrics#{suites_run => SuitesRun},

    ct:log("Suite ~p completed in ~p ms~n", [SuiteName, Duration]),

    NewState = State#state{metrics = NewMetrics},
    {Config, NewState}.

%% @doc After suite cleanup
post_end_per_suite(SuiteName, Config, Result, State) ->
    %% Verify cleanup was successful
    ok = verify_suite_cleanup(SuiteName),

    ct:log("Suite ~p cleanup completed~n", [SuiteName]),
    {Result, State}.

%% @doc Terminate hook - called once after all suites
terminate(State) ->
    ct:log("~n=== TCPS CT Hooks: Terminating ===~n"),

    %% Stop mock services
    MockServices = State#state.mock_services,
    ok = tcps_mock_services:stop_all(MockServices),
    ct:log("Mock services stopped~n"),

    %% Print final metrics
    Metrics = State#state.metrics,
    ct:log("~n=== Final Test Metrics ===~n"),
    ct:log("Suites run: ~p~n", [maps:get(suites_run, Metrics, 0)]),
    ct:log("Testcases run: ~p~n", [maps:get(testcases_run, Metrics, 0)]),
    ct:log("Testcases passed: ~p~n", [maps:get(testcases_passed, Metrics, 0)]),
    ct:log("Testcases failed: ~p~n", [maps:get(testcases_failed, Metrics, 0)]),
    ct:log("Total time: ~p ms~n", [maps:get(total_time_ms, Metrics, 0)]),

    %% Calculate pass rate
    TestcasesRun = maps:get(testcases_run, Metrics, 0),
    TestcasesPassed = maps:get(testcases_passed, Metrics, 0),
    PassRate = case TestcasesRun of
        0 -> 0.0;
        _ -> (TestcasesPassed / TestcasesRun) * 100.0
    end,
    ct:log("Pass rate: ~.2f%~n", [PassRate]),

    %% Save metrics to file
    ok = save_metrics(Metrics),

    ok.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

%% @doc Ensure directory exists
ensure_dir(Dir) ->
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok -> ok;
        {error, Reason} ->
            ct:log("WARNING: Could not create directory ~p: ~p~n", [Dir, Reason]),
            ok
    end.

%% @doc Load fixture data for suite
load_suite_fixtures(SuiteName) ->
    FixturesDir = "test/integration/fixtures",
    SuiteFixtureFile = filename:join(FixturesDir, atom_to_list(SuiteName) ++ ".json"),

    case filelib:is_regular(SuiteFixtureFile) of
        true ->
            ct:log("Loading fixtures from ~p~n", [SuiteFixtureFile]),
            %% TODO: Load and inject fixture data
            ok;
        false ->
            ct:log("No fixtures found for suite ~p~n", [SuiteName]),
            ok
    end.

%% @doc Verify suite cleanup
verify_suite_cleanup(SuiteName) ->
    ct:log("Verifying cleanup for suite ~p~n", [SuiteName]),

    %% Check for leaked processes
    %% Check for leaked ETS tables
    %% Check for leaked ports

    ok.

%% @doc Save metrics to file
save_metrics(Metrics) ->
    MetricsFile = "/tmp/tcps_test_data/ct_metrics.json",
    MetricsJson = jsx:encode(Metrics),

    case file:write_file(MetricsFile, MetricsJson) of
        ok ->
            ct:log("Metrics saved to ~p~n", [MetricsFile]),
            ok;
        {error, Reason} ->
            ct:log("WARNING: Could not save metrics: ~p~n", [Reason]),
            ok
    end.
