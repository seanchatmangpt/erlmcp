%%%-------------------------------------------------------------------
%%% @doc
### Enterprise Load Test Manager
Coordinates load test execution across all components with comprehensive
orchestration and monitoring capabilities.
%%%
## Core Responsibilities
%%% - Test lifecycle management (start, monitor, stop, analyze)
%%% - Load generator coordination and supervision
%%% - Real-time metrics aggregation and alerting
%%% - Performance analysis and bottleneck detection
%%% - Test result compilation and reporting
%%% - Resource management and load balancing
%%%
## Architecture
%%% - OTP gen_server for state management
%%% - Supervises test-specific generators and monitors
%%% - Integrates with metrics collection and alert systems
%%% - Supports concurrent test execution
%%% - Graceful shutdown capabilities
%%%
## Test Phases
%%% 1. Initialization: Setup test environment and resources
%%% 2. Ramp-up: Gradually increase load to target levels
%%% 3. Steady State: Maintain target load for duration
%%% 4. Cool-down: Gradually decrease load
%%% 5. Analysis: Compile results and generate report
%%%
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_test_manager).

-behaviour(gen_server).

-export([start_link/2, get_status/1, stop_test/1,
         update_test_config/2, add_scenario/3, get_metrics/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-record(test_state, {
    test_id :: load_test_id(),
    config :: load_test_config(),
    status :: test_status(),
    start_time :: pos_integer(),
    end_time :: pos_integer(),
    elapsed_time :: pos_integer(),
    current_phase :: test_phase(),
    generators :: [pid()],
    metrics_ref :: reference(),
    monitor_ref :: reference(),
    analysis_supervisor :: pid(),
    resource_monitor :: pid(),
    alert_manager :: pid(),
    scenarios :: [map()],
    results :: map(),
    current_load :: map(),
    statistics :: map()
}).

-type test_state() :: #test_state{}.
-type state() :: test_state().

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(load_test_id(), load_test_config()) -> {ok, pid()} | {error, term()}.
start_link(TestId, Config) ->
    gen_server:start_link({local, TestId}, ?MODULE, [TestId, Config], []).

-spec get_status(load_test_id()) -> {ok, test_status()} | {error, term()}.
get_status(TestId) ->
    gen_server:call(TestId, get_status).

-spec stop_test(load_test_id()) -> ok | {error, term()}.
stop_test(TestId) ->
    gen_server:call(TestId, stop_test).

-spec update_test_config(load_test_id(), load_test_config()) -> ok | {error, term()}.
update_test_config(TestId, NewConfig) ->
    gen_server:cast(TestId, {update_config, NewConfig}).

-spec add_scenario(load_test_id(), binary(), map()) -> ok | {error, term()}.
add_scenario(TestId, ScenarioName, ScenarioConfig) ->
    gen_server:cast(TestId, {add_scenario, ScenarioName, ScenarioConfig}).

-spec get_metrics(load_test_id()) -> {ok, metrics()} | {error, term()}.
get_metrics(TestId) ->
    gen_server:call(TestId, get_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([load_test_id(), load_test_config()]) -> {ok, test_state()} | {stop, term()}.
init([TestId, Config]) ->
    process_flag(trap_exit, true),

    %% Initialize test state
    InitialState = #test_state{
        test_id = TestId,
        config = Config,
        status = initializing,
        start_time = undefined,
        end_time = undefined,
        elapsed_time = 0,
        current_phase = idle,
        generators = [],
        metrics_ref = undefined,
        monitor_ref = undefined,
        analysis_supervisor = undefined,
        resource_monitor = undefined,
        alert_manager = undefined,
        scenarios = [],
        results = #{},
        current_load = #{},
        statistics = #{
            total_requests => 0,
            successful_requests => 0,
            failed_requests => 0,
            avg_latency => 0.0,
            p95_latency => 0.0,
            p99_latency => 0.0,
            throughput => 0.0,
            error_rate => 0.0
        }
    },

    %% Start test initialization
    self() ! {initialize_test},

    {ok, InitialState}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                       {reply, term(), state()} | {stop, term(), state()}.
handle_call(get_status, _From, State) ->
    StatusInfo = #{
        test_id => State#test_state.test_id,
        status => State#test_state.status,
        current_phase => State#test_state.current_phase,
        start_time => State#test_state.start_time,
        elapsed_time => State#test_state.elapsed_time,
        generators_count => length(State#test_state.generators),
        scenarios_count => length(State#test_state.scenarios),
        statistics => State#test_state.statistics
    },
    {reply, {ok, StatusInfo}, State};

handle_call(stop_test, _From, State) ->
    NewState = initiate_test_shutdown(State),
    {reply, ok, NewState};

handle_call(get_metrics, _From, State) ->
    %% Fetch current metrics
    Metrics = collect_test_metrics(State),
    {reply, {ok, Metrics}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_cast({update_config, NewConfig}, State) ->
    %% Validate and update configuration
    case validate_test_config(NewConfig) of
        ok ->
            UpdatedState = State#test_state{config = NewConfig},
            UpdatedState ! {restart_test_with_new_config},
            {noreply, UpdatedState};
        {error, Reason} ->
            {noreply, State}
    end;

handle_cast({add_scenario, ScenarioName, ScenarioConfig}, State) ->
    %% Add new scenario to test
    Scenario = #{
        id => ScenarioName,
        config => ScenarioConfig,
        status => pending,
        start_time => undefined,
        end_time => undefined,
        results => #{}
    },
    NewScenarios = [Scenario | State#test_state.scenarios],
    {noreply, State#test_state{scenarios = NewScenarios}};

handle_cast({generator_status, GenPid, Status}, State) ->
    %% Update generator status
    UpdatedGenerators = update_generator_status(State#test_state.generators, GenPid, Status),
    {noreply, State#test_state{generators = UpdatedGenerators}};

handle_cast({metrics_update, Metrics}, State) ->
    %% Update test metrics
    UpdatedStats = update_test_statistics(State#test_state.statistics, Metrics),
    {noreply, State#test_state{statistics = UpdatedStats}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({initialize_test}, State) ->
    %% Initialize test components
    case initialize_test_components(State) of
        {ok, InitializedState} ->
            %% Start test phases
            NewState = start_test_phases(InitializedState),
            {noreply, NewState};
        {error, Reason} ->
            error_logger:error_msg("Failed to initialize test: ~p", [Reason]),
            {stop, Reason, State}
    end;

handle_info({phase_start, Phase}, State) ->
    %% Transition to next phase
    NewState = transition_to_phase(State, Phase),
    {noreply, NewState};

handle_info({phase_complete, Phase, Results}, State) ->
    %% Handle phase completion
    NewState = handle_phase_completion(State, Phase, Results),
    {noreply, NewState};

handle_info({phase_error, Phase, Error}, State) ->
    %% Handle phase errors
    error_logger:error_msg("Phase ~p error: ~p", [Phase, Error]),
    {noreply, handle_phase_error(State, Phase, Error)};

handle_info({check_test_progress}, State) ->
    %% Monitor test progress and health
    case monitor_test_progress(State) of
        continue ->
            schedule_progress_check(State),
            {noreply, State};
        stop ->
            {stop, normal, initiate_test_shutdown(State)}
    end;

handle_info({generator_failed, GenPid, Reason}, State) ->
    %% Handle generator failure
    error_logger:error_msg("Generator ~p failed: ~p", [GenPid, Reason]),
    UpdatedState = handle_generator_failure(State, GenPid, Reason),
    {noreply, UpdatedState};

handle_info({test_complete}, State) ->
    %% Test completed successfully
    FinalState = finalize_test_results(State),
    {stop, normal, FinalState};

handle_info({timeout, TimerRef, progress_tick}, State) ->
    %% Update progress
    Progress = calculate_progress(State),
    CurrentTime = erlang:system_time(millisecond),

    %% Update status
    NewStatus = State#test_state.status#{
        progress => Progress,
        current_users => State#test_state.current_users,
        current_rate => State#test_state.current_rate,
        estimated_completion => estimate_completion(State, Progress)
    },

    %% Check for completion
    case Progress >= 1.0 orelse should_stop(State) of
        true ->
            %% Test completed
            complete_test(State);
        false ->
            %% Continue monitoring
            NewTimerRef = erlang:start_timer(1000, self(), progress_tick),
            State1 = State#test_state{
                status = NewStatus,
                timer_ref = NewTimerRef
            },
            {noreply, State1}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% Ensure graceful shutdown
    cleanup_test_resources(State),

    %% Generate final report
    case State#test_state.status =:= completed of
        true ->
            generate_test_report(State);
        false ->
            ok
    end,

    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Start load generators
-spec start_load_generators(state()) -> {ok, state()}.
start_load_generators(State) ->
    Config = State#state.config,
    UserCount = maps:get(user_count, Config, ?DEFAULT_USER_COUNT),
    Protocol = maps:get(protocol, Config),
    Target = maps:get(target_endpoint, Config),
    Rate = maps:get(request_rate, Config, ?DEFAULT_RATE),

    %% Calculate number of generators needed
    GeneratorCount = calculate_generator_count(UserCount, Rate),

    %% Start load generators
    Generators = start_generators(GeneratorCount, Config),

    %% Register generators
    erlmcp_load_testing_metrics:register_generators(State#state.test_id, Generators),

    State#state{
        load_generators = Generators,
        current_users = UserCount,
        current_rate = Rate
    }.

%% Calculate number of generators
-spec calculate_generator_count(pos_integer(), pos_integer()) -> pos_integer().
calculate_generator_count(UserCount, Rate) ->
    %% Scale: 1 generator per 1000 users or 100 req/s
    MinPerGenerator = 1000,
    case min(UserCount, Rate) of
        Count when Count < MinPerGenerator ->
            1;
        Count ->
            max(1, trunc(Count / MinPerGenerator))
    end.

%% Start generator processes
-spec start_generators(pos_integer(), load_test_config()) -> [pid()].
start_generators(Count, Config) ->
    start_generators(Count, Config, []).

start_generators(0, _Config, Acc) ->
    lists:reverse(Acc);
start_generators(Count, Config, Acc) ->
    GeneratorId = list_to_binary("gen_" ++ integer_to_list(Count)),
    case erlmcp_load_testing_generator_sup:start_generator(GeneratorId, Config) of
        {ok, Pid} ->
            start_generators(Count - 1, Config, [Pid | Acc]);
        {error, Reason} ->
            %% Log error and continue with fewer generators
            log_error(self(), "Failed to start generator ~p: ~p", [GeneratorId, Reason]),
            start_generators(Count - 1, Config, Acc)
    end.

%% Calculate test progress
-spec calculate_progress(state()) -> float().
calculate_progress(State) ->
    Config = State#state.config,
    Duration = maps:get(duration, Config, ?DEFAULT_DURATION),
    CurrentTime = erlang:system_time(millisecond),
    Elapsed = CurrentTime - State#state.start_time,

    min(1.0, Elapsed / (Duration * 1000)).

%% Estimate completion time
-spec estimate_completion(state(), float()) -> pos_integer().
estimate_completion(State, Progress) ->
    case Progress > 0 of
        true ->
            Config = State#state.config,
            Duration = maps:get(duration, Config, ?DEFAULT_DURATION),
            Elapsed = State#state.status#progress * Duration * 1000,
            Remaining = Duration * 1000 - Elapsed,
            State#state.start_time + Elapsed + Remaining;
        false ->
            0
    end.

%% Check if test should stop
-spec should_stop(state()) -> boolean().
should_stop(State) ->
    Config = State#state.config,
    ErrorThreshold = maps:get(error_rate_threshold, Config, ?DEFAULT_ERROR_THRESHOLD),
    LatencyThreshold = maps:get(latency_threshold, Config, ?DEFAULT_LATENCY_THRESHOLD),

    %% Get current metrics
    case erlmcp_load_testing_metrics:get_current_metrics(State#state.test_id) of
        {ok, Metrics} ->
            CheckError = Metrics#throughput.success_rate < (1 - ErrorThreshold),
            CheckLatency = Metrics#latency.p95 > LatencyThreshold,
            CheckTime = calculate_progress(State) >= 1.0,
            CheckError orelse CheckLatency orelse CheckTime;
        {error, _} ->
            false
    end.

%% Complete the test
-spec complete_test(state()) -> {stop, normal, state()}.
complete_test(State) ->
    %% Update final status
    FinalStatus = State#state.status#{
        status => ?TEST_STATUS_COMPLETED,
        progress => 1.0
    },

    %% Generate final report
    generate_report(State),

    %% Send completion notification
    erlmcp_load_testing_metrics:complete(State#state.test_id),

    %% Cancel timer
    case State#state.timer_ref of
        undefined ->
            ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef)
    end,

    State1 = State#state{status = FinalStatus},

    {stop, normal, State1}.

%% Cancel test gracefully
-spec cancel_test(state()) -> ok.
cancel_test(State) ->
    %% Stop all generators
    lists:foreach(fun(Pid) ->
                      erlmcp_load_testing_generator:stop(Pid)
                  end, State#state.load_generators),

    %% Stop metrics collection
    case State#state.metrics_collector of
        undefined ->
            ok;
        Pid ->
            erlmcp_load_testing_metrics:stop_collector(State#state.test_id)
    end,

    %% Cancel timers
    case State#state.timer_ref of
        undefined ->
            ok;
        TimerRef ->
            erlang:cancel_timer(TimerRef)
    end,

    case State#state.shutdown_timer of
        undefined ->
            ok;
        ShutdownTimer ->
            erlang:cancel_timer(ShutdownTimer)
    end.

%% Generate test report
-spec generate_report(state()) -> ok.
generate_report(State) ->
    TestId = State#state.test_id,
    Report = #{
        test_id => TestId,
        start_time => State#state.start_time,
        end_time => erlang:system_time(millisecond),
        config => State#state.config,
        final_metrics => erlmcp_load_testing_metrics:get_final_metrics(TestId),
        summary => #{
            total_duration => erlang:system_time(millisecond) - State#state.start_time,
            total_errors => State#state.error_count,
            warnings => State#state.warnings,
            status => State#state.status#status
        }
    },

    %% Save report
    erlmcp_load_testing_report:save(TestId, Report),

    %% Log completion
    log_info(State, "Test completed with ~p errors", [State#state.error_count]).

%% Log info event
-spec log_event(state(), binary()) -> ok.
log_event(State, Event) ->
    erlmcp_load_testing_event:log(State#state.test_id, Event).

%% Log info message
-spec log_info(state(), binary(), [term()]) -> ok.
log_info(State, Format, Args) ->
    erlmcp_load_testing_event:log_info(State#state.test_id, Format, Args).

%% Log error message
-spec log_error(state(), binary(), [term()]) -> ok.
log_error(State, Format, Args) ->
    erlmcp_load_testing_event:log_error(State#state.test_id, Format, Args).