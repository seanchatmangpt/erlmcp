%%%-------------------------------------------------------------------
%%% @doc TCPS Simulator
%%% Simulation engine for TCPS scenarios with telemetry collection.
%%%
%%% This module provides:
%%% - 6 simulation scenarios (basic production, defect handling, high load, etc.)
%%% - Realistic work order generation and processing
%%% - Quality gate simulation with configurable pass/fail rates
%%% - Andon trigger simulation
%%% - OTEL telemetry collection for all operations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_simulator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_scenario/1,
    start_scenario/2,
    stop_scenario/1,
    get_scenario_status/1,
    list_scenarios/0,
    pause_scenario/1,
    resume_scenario/1,
    get_metrics/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

-record(state, {
    scenarios = #{} :: #{scenario_id() => scenario_state()},
    metrics = #{} :: #{scenario_id() => metrics()}
}).

-type scenario_id() :: binary().
-type scenario_type() :: basic_production | defect_scenario | high_load |
                        bottleneck_analysis | optimization_test | stress_test.
-type scenario_state() :: #{
    id := scenario_id(),
    type := scenario_type(),
    status := running | paused | stopped | completed,
    config := map(),
    work_orders := [map()],
    started_at := integer(),
    completed_at => integer()
}.
-type metrics() :: #{
    work_orders_created := non_neg_integer(),
    work_orders_completed := non_neg_integer(),
    quality_gates_passed := non_neg_integer(),
    quality_gates_failed := non_neg_integer(),
    andon_alerts := non_neg_integer(),
    total_duration_ms := non_neg_integer(),
    throughput_per_second := float()
}.

-export_type([scenario_id/0, scenario_type/0, scenario_state/0, metrics/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the simulator
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start a simulation scenario with default configuration
-spec start_scenario(scenario_type()) -> {ok, scenario_id()} | {error, term()}.
start_scenario(Type) ->
    start_scenario(Type, #{}).

%% @doc Start a simulation scenario with custom configuration
-spec start_scenario(scenario_type(), map()) -> {ok, scenario_id()} | {error, term()}.
start_scenario(Type, Config) ->
    gen_server:call(?SERVER, {start_scenario, Type, Config}).

%% @doc Stop a running scenario
-spec stop_scenario(scenario_id()) -> ok | {error, term()}.
stop_scenario(ScenarioId) ->
    gen_server:call(?SERVER, {stop_scenario, ScenarioId}).

%% @doc Get the status of a scenario
-spec get_scenario_status(scenario_id()) -> {ok, scenario_state()} | {error, not_found}.
get_scenario_status(ScenarioId) ->
    gen_server:call(?SERVER, {get_status, ScenarioId}).

%% @doc List all scenarios
-spec list_scenarios() -> [scenario_state()].
list_scenarios() ->
    gen_server:call(?SERVER, list_scenarios).

%% @doc Pause a running scenario
-spec pause_scenario(scenario_id()) -> ok | {error, term()}.
pause_scenario(ScenarioId) ->
    gen_server:call(?SERVER, {pause_scenario, ScenarioId}).

%% @doc Resume a paused scenario
-spec resume_scenario(scenario_id()) -> ok | {error, term()}.
resume_scenario(ScenarioId) ->
    gen_server:call(?SERVER, {resume_scenario, ScenarioId}).

%% @doc Get metrics for a scenario
-spec get_metrics(scenario_id()) -> {ok, metrics()} | {error, not_found}.
get_metrics(ScenarioId) ->
    gen_server:call(?SERVER, {get_metrics, ScenarioId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    otel:add_event(<<"tcps_simulator_started">>, #{}),
    {ok, #state{}}.

handle_call({start_scenario, Type, Config}, _From, State) ->
    ScenarioId = generate_scenario_id(),

    Scenario = #{
        id => ScenarioId,
        type => Type,
        status => running,
        config => merge_default_config(Type, Config),
        work_orders => [],
        started_at => erlang:system_time(millisecond)
    },

    % Initialize metrics
    Metrics = init_metrics(),

    % Start scenario execution
    self() ! {execute_scenario, ScenarioId},

    NewState = State#state{
        scenarios = maps:put(ScenarioId, Scenario, State#state.scenarios),
        metrics = maps:put(ScenarioId, Metrics, State#state.metrics)
    },

    otel:add_event(<<"scenario_started">>, #{
        scenario_id => ScenarioId,
        type => Type
    }),

    {reply, {ok, ScenarioId}, NewState};

handle_call({stop_scenario, ScenarioId}, _From, State) ->
    case maps:find(ScenarioId, State#state.scenarios) of
        {ok, Scenario} ->
            UpdatedScenario = Scenario#{
                status := stopped,
                completed_at => erlang:system_time(millisecond)
            },
            NewState = State#state{
                scenarios = maps:put(ScenarioId, UpdatedScenario, State#state.scenarios)
            },
            otel:add_event(<<"scenario_stopped">>, #{scenario_id => ScenarioId}),
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_status, ScenarioId}, _From, State) ->
    case maps:find(ScenarioId, State#state.scenarios) of
        {ok, Scenario} -> {reply, {ok, Scenario}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(list_scenarios, _From, State) ->
    Scenarios = maps:values(State#state.scenarios),
    {reply, Scenarios, State};

handle_call({pause_scenario, ScenarioId}, _From, State) ->
    case maps:find(ScenarioId, State#state.scenarios) of
        {ok, Scenario} when map_get(status, Scenario) =:= running ->
            UpdatedScenario = Scenario#{status := paused},
            NewState = State#state{
                scenarios = maps:put(ScenarioId, UpdatedScenario, State#state.scenarios)
            },
            otel:add_event(<<"scenario_paused">>, #{scenario_id => ScenarioId}),
            {reply, ok, NewState};
        {ok, _} ->
            {reply, {error, not_running}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({resume_scenario, ScenarioId}, _From, State) ->
    case maps:find(ScenarioId, State#state.scenarios) of
        {ok, Scenario} when map_get(status, Scenario) =:= paused ->
            UpdatedScenario = Scenario#{status := running},
            NewState = State#state{
                scenarios = maps:put(ScenarioId, UpdatedScenario, State#state.scenarios)
            },
            self() ! {execute_scenario, ScenarioId},
            otel:add_event(<<"scenario_resumed">>, #{scenario_id => ScenarioId}),
            {reply, ok, NewState};
        {ok, _} ->
            {reply, {error, not_paused}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_metrics, ScenarioId}, _From, State) ->
    case maps:find(ScenarioId, State#state.metrics) of
        {ok, Metrics} -> {reply, {ok, Metrics}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({execute_scenario, ScenarioId}, State) ->
    case maps:find(ScenarioId, State#state.scenarios) of
        {ok, Scenario} when map_get(status, Scenario) =:= running ->
            Type = maps:get(type, Scenario),
            Config = maps:get(config, Scenario),

            % Execute one iteration of the scenario
            {UpdatedScenario, UpdatedMetrics} = execute_scenario_iteration(Scenario, maps:get(ScenarioId, State#state.metrics), Type, Config),

            % Check if scenario should continue
            ShouldContinue = should_continue(UpdatedScenario, Config),

            NewState = State#state{
                scenarios = maps:put(ScenarioId, UpdatedScenario, State#state.scenarios),
                metrics = maps:put(ScenarioId, UpdatedMetrics, State#state.metrics)
            },

            case ShouldContinue of
                true ->
                    % Schedule next iteration
                    Delay = maps:get(iteration_delay_ms, Config, 100),
                    erlang:send_after(Delay, self(), {execute_scenario, ScenarioId}),
                    {noreply, NewState};
                false ->
                    % Mark as completed
                    CompletedScenario = UpdatedScenario#{
                        status := completed,
                        completed_at => erlang:system_time(millisecond)
                    },
                    FinalState = NewState#state{
                        scenarios = maps:put(ScenarioId, CompletedScenario, NewState#state.scenarios)
                    },
                    otel:add_event(<<"scenario_completed">>, #{scenario_id => ScenarioId}),
                    {noreply, FinalState}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    otel:add_event(<<"tcps_simulator_terminated">>, #{}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
generate_scenario_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("scenario_~p_~p", [Timestamp, Random])).

%% @private
init_metrics() ->
    #{
        work_orders_created => 0,
        work_orders_completed => 0,
        quality_gates_passed => 0,
        quality_gates_failed => 0,
        andon_alerts => 0,
        total_duration_ms => 0,
        throughput_per_second => 0.0
    }.

%% @private
merge_default_config(Type, UserConfig) ->
    DefaultConfig = case Type of
        basic_production ->
            #{work_orders => 10, quality_gate_pass_rate => 0.95, iteration_delay_ms => 100};
        defect_scenario ->
            #{work_orders => 20, quality_gate_pass_rate => 0.70, andon_trigger_rate => 0.15, iteration_delay_ms => 150};
        high_load ->
            #{work_orders => 100, quality_gate_pass_rate => 0.90, iteration_delay_ms => 50};
        bottleneck_analysis ->
            #{work_orders => 50, quality_gate_pass_rate => 0.85, slow_operation_rate => 0.20, iteration_delay_ms => 200};
        optimization_test ->
            #{work_orders => 30, quality_gate_pass_rate => 0.92, wip_limit => 5, iteration_delay_ms => 100};
        stress_test ->
            #{work_orders => 1000, quality_gate_pass_rate => 0.88, iteration_delay_ms => 10}
    end,
    maps:merge(DefaultConfig, UserConfig).

%% @private
execute_scenario_iteration(Scenario, Metrics, Type, Config) ->
    otel:with_span(
        <<"tcps.simulator.iteration">>,
        #{scenario_id => maps:get(id, Scenario), type => Type},
        fun(_Ctx) ->
            % Create work order
            WO = create_simulated_work_order(),
            NewWorkOrders = [WO | maps:get(work_orders, Scenario)],

            % Run quality gate
            PassRate = maps:get(quality_gate_pass_rate, Config),
            QualityPassed = rand:uniform() < PassRate,

            % Update metrics
            NewMetrics = Metrics#{
                work_orders_created := maps:get(work_orders_created, Metrics) + 1,
                work_orders_completed := maps:get(work_orders_completed, Metrics) + (if QualityPassed -> 1; true -> 0 end),
                quality_gates_passed := maps:get(quality_gates_passed, Metrics) + (if QualityPassed -> 1; true -> 0 end),
                quality_gates_failed := maps:get(quality_gates_failed, Metrics) + (if QualityPassed -> 0; true -> 1 end)
            },

            % Check for andon trigger
            AndonRate = maps:get(andon_trigger_rate, Config, 0.0),
            ShouldTriggerAndon = rand:uniform() < AndonRate,
            FinalMetrics = if
                ShouldTriggerAndon ->
                    otel:add_event(<<"andon_triggered">>, #{scenario_id => maps:get(id, Scenario)}),
                    NewMetrics#{andon_alerts := maps:get(andon_alerts, NewMetrics) + 1};
                true ->
                    NewMetrics
            end,

            % Calculate throughput
            Duration = erlang:system_time(millisecond) - maps:get(started_at, Scenario),
            Throughput = (maps:get(work_orders_completed, FinalMetrics) / (Duration / 1000.0)),

            MetricsWithThroughput = FinalMetrics#{
                total_duration_ms := Duration,
                throughput_per_second := Throughput
            },

            UpdatedScenario = Scenario#{work_orders := NewWorkOrders},
            {UpdatedScenario, MetricsWithThroughput}
        end
    ).

%% @private
create_simulated_work_order() ->
    Types = [feature, bugfix, refactor, test],
    Priorities = [low, medium, high, critical],

    #{
        id => generate_scenario_id(),
        type => lists:nth(rand:uniform(length(Types)), Types),
        priority => lists:nth(rand:uniform(length(Priorities)), Priorities),
        created_at => erlang:system_time(millisecond)
    }.

%% @private
should_continue(Scenario, Config) ->
    MaxWorkOrders = maps:get(work_orders, Config),
    CurrentCount = length(maps:get(work_orders, Scenario)),
    CurrentCount < MaxWorkOrders.

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_stop_test() ->
    {ok, Pid} = start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(Pid).

basic_scenario_test() ->
    {ok, _Pid} = start_link(),
    {ok, ScenarioId} = start_scenario(basic_production, #{work_orders => 5, iteration_delay_ms => 10}),

    % Wait for completion
    timer:sleep(200),

    {ok, Status} = get_scenario_status(ScenarioId),
    ?assertEqual(completed, maps:get(status, Status)),

    {ok, Metrics} = get_metrics(ScenarioId),
    ?assertEqual(5, maps:get(work_orders_created, Metrics)),
    ?assert(maps:get(work_orders_completed, Metrics) >= 4),

    gen_server:stop(?SERVER).

pause_resume_test() ->
    {ok, _Pid} = start_link(),
    {ok, ScenarioId} = start_scenario(basic_production, #{work_orders => 100}),

    timer:sleep(50),
    ?assertEqual(ok, pause_scenario(ScenarioId)),
    {ok, Status1} = get_scenario_status(ScenarioId),
    ?assertEqual(paused, maps:get(status, Status1)),

    ?assertEqual(ok, resume_scenario(ScenarioId)),
    {ok, Status2} = get_scenario_status(ScenarioId),
    ?assertEqual(running, maps:get(status, Status2)),

    stop_scenario(ScenarioId),
    gen_server:stop(?SERVER).

list_scenarios_test() ->
    {ok, _Pid} = start_link(),
    {ok, _Id1} = start_scenario(basic_production),
    {ok, _Id2} = start_scenario(defect_scenario),

    Scenarios = list_scenarios(),
    ?assertEqual(2, length(Scenarios)),

    gen_server:stop(?SERVER).

defect_scenario_test() ->
    {ok, _Pid} = start_link(),
    {ok, ScenarioId} = start_scenario(defect_scenario, #{work_orders => 10, iteration_delay_ms => 10}),

    timer:sleep(300),

    {ok, Metrics} = get_metrics(ScenarioId),
    ?assert(maps:get(andon_alerts, Metrics) > 0),
    ?assert(maps:get(quality_gates_failed, Metrics) > 0),

    gen_server:stop(?SERVER).

-endif.
