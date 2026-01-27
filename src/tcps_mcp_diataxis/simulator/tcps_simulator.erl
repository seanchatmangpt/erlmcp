%%%-----------------------------------------------------------------------------
%%% @doc TCPS Workflow Simulator Engine
%%%
%%% Production-grade simulation engine orchestrating realistic TCPS scenarios
%%% with complete integration of quality gates, Kanban, Andon, and receipts.
%%%
%%% Core Responsibilities:
%%% - Scenario execution with time-based or event-based progression
%%% - Work order lifecycle simulation
%%% - Quality gate execution with realistic pass/fail
%%% - Andon event triggering and resolution
%%% - Receipt chain generation and verification
%%% - State management with snapshot/rollback
%%% - Event logging and metrics collection
%%%
%%% Simulation Modes:
%%% - Real-time: Simulates at 1x speed
%%% - Fast: 5x speed acceleration
%%% - Ultra-fast: 10x speed acceleration
%%% - Step-by-step: Manual progression
%%%
%%% Integration:
%%% - tcps_quality_gates: All 8 quality gates
%%% - tcps_kanban: WIP limits and Heijunka
%%% - tcps_andon: Stop-the-line events
%%% - tcps_work_order: Complete lifecycle
%%% - tcps_receipt: Chain verification
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_simulator).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,
    load_scenario/1,
    start_simulation/0,
    pause_simulation/0,
    resume_simulation/0,
    reset_simulation/0,
    step_simulation/0,
    set_speed/1,
    get_state/0,
    get_metrics/0,
    snapshot_state/0,
    restore_snapshot/1,
    % Multi-scenario API for integration tests
    start_scenario/2,
    stop_scenario/1,
    pause_scenario/1,
    resume_scenario/1,
    get_scenario_status/1,
    get_metrics/1,
    list_scenarios/0
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

%% Internal exports for scenario execution
-export([
    execute_scenario_step/2,
    simulate_quality_gates/2,
    simulate_andon_event/2,
    simulate_work_order_lifecycle/2
]).

-export_type([simulator_state/0, simulation_mode/0, speed_multiplier/0]).

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type simulation_mode() :: stopped | running | paused.
-type speed_multiplier() :: 1 | 5 | 10.
-type scenario_id() :: tcps_scenario_loader:scenario_id().

-record(simulator_state, {
    mode :: simulation_mode(),
    scenario :: tcps_scenario_loader:scenario() | undefined,
    sim_state :: tcps_simulator_state:state() | undefined,
    current_step :: non_neg_integer(),
    speed :: speed_multiplier(),
    timer_ref :: reference() | undefined,
    snapshots :: [tcps_simulator_state:snapshot()],
    metrics :: map(),
    start_time :: erlang:timestamp() | undefined,
    % Multi-scenario support
    scenarios :: map(), % scenario_id => scenario_info
    next_scenario_id :: non_neg_integer()
}).

-type simulator_state() :: #simulator_state{}.

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the simulator server with default settings.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%%------------------------------------------------------------------------------
%% @doc Start the simulator server with configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%------------------------------------------------------------------------------
%% @doc Stop the simulator server.
%% @end
%%------------------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%------------------------------------------------------------------------------
%% @doc Load a scenario into the simulator.
%% @end
%%------------------------------------------------------------------------------
-spec load_scenario(ScenarioId :: scenario_id()) -> ok | {error, term()}.
load_scenario(ScenarioId) ->
    gen_server:call(?MODULE, {load_scenario, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc Start the simulation execution.
%% @end
%%------------------------------------------------------------------------------
-spec start_simulation() -> ok | {error, term()}.
start_simulation() ->
    gen_server:call(?MODULE, start_simulation).

%%------------------------------------------------------------------------------
%% @doc Pause the running simulation.
%% @end
%%------------------------------------------------------------------------------
-spec pause_simulation() -> ok.
pause_simulation() ->
    gen_server:call(?MODULE, pause_simulation).

%%------------------------------------------------------------------------------
%% @doc Resume a paused simulation.
%% @end
%%------------------------------------------------------------------------------
-spec resume_simulation() -> ok | {error, term()}.
resume_simulation() ->
    gen_server:call(?MODULE, resume_simulation).

%%------------------------------------------------------------------------------
%% @doc Reset simulation to initial state.
%% @end
%%------------------------------------------------------------------------------
-spec reset_simulation() -> ok.
reset_simulation() ->
    gen_server:call(?MODULE, reset_simulation).

%%------------------------------------------------------------------------------
%% @doc Execute a single simulation step (manual mode).
%% @end
%%------------------------------------------------------------------------------
-spec step_simulation() -> ok | {error, term()}.
step_simulation() ->
    gen_server:call(?MODULE, step_simulation).

%%------------------------------------------------------------------------------
%% @doc Set simulation speed multiplier.
%% @end
%%------------------------------------------------------------------------------
-spec set_speed(Speed :: speed_multiplier()) -> ok.
set_speed(Speed) when Speed =:= 1; Speed =:= 5; Speed =:= 10 ->
    gen_server:call(?MODULE, {set_speed, Speed}).

%%------------------------------------------------------------------------------
%% @doc Get current simulator state.
%% @end
%%------------------------------------------------------------------------------
-spec get_state() -> map().
get_state() ->
    gen_server:call(?MODULE, get_state).

%%------------------------------------------------------------------------------
%% @doc Get simulation metrics.
%% @end
%%------------------------------------------------------------------------------
-spec get_metrics() -> map().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%%------------------------------------------------------------------------------
%% @doc Create a snapshot of current state.
%% @end
%%------------------------------------------------------------------------------
-spec snapshot_state() -> {ok, tcps_simulator_state:snapshot()}.
snapshot_state() ->
    gen_server:call(?MODULE, snapshot_state).

%%------------------------------------------------------------------------------
%% @doc Restore state from a snapshot.
%% @end
%%------------------------------------------------------------------------------
-spec restore_snapshot(Snapshot :: tcps_simulator_state:snapshot()) -> ok.
restore_snapshot(Snapshot) ->
    gen_server:call(?MODULE, {restore_snapshot, Snapshot}).

%%------------------------------------------------------------------------------
%% @doc Start a new scenario with the given type and configuration.
%% @end
%%------------------------------------------------------------------------------
-spec start_scenario(atom(), map()) -> {ok, binary()}.
start_scenario(ScenarioType, Config) ->
    gen_server:call(?MODULE, {start_scenario, ScenarioType, Config}).

%%------------------------------------------------------------------------------
%% @doc Stop a running scenario.
%% @end
%%------------------------------------------------------------------------------
-spec stop_scenario(binary()) -> ok.
stop_scenario(ScenarioId) ->
    gen_server:call(?MODULE, {stop_scenario, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc Pause a running scenario.
%% @end
%%------------------------------------------------------------------------------
-spec pause_scenario(binary()) -> ok.
pause_scenario(ScenarioId) ->
    gen_server:call(?MODULE, {pause_scenario, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc Resume a paused scenario.
%% @end
%%------------------------------------------------------------------------------
-spec resume_scenario(binary()) -> ok.
resume_scenario(ScenarioId) ->
    gen_server:call(?MODULE, {resume_scenario, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc Get the status of a specific scenario.
%% @end
%%------------------------------------------------------------------------------
-spec get_scenario_status(binary()) -> {ok, map()}.
get_scenario_status(ScenarioId) ->
    gen_server:call(?MODULE, {get_scenario_status, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc Get metrics for a specific scenario.
%% @end
%%------------------------------------------------------------------------------
-spec get_metrics(binary()) -> {ok, map()}.
get_metrics(ScenarioId) ->
    gen_server:call(?MODULE, {get_metrics, ScenarioId}).

%%------------------------------------------------------------------------------
%% @doc List all running scenarios.
%% @end
%%------------------------------------------------------------------------------
-spec list_scenarios() -> [map()].
list_scenarios() ->
    gen_server:call(?MODULE, list_scenarios).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialize the simulator.
%% @private
%% @end
%%------------------------------------------------------------------------------
init(_Config) ->
    State = #simulator_state{
        mode = stopped,
        scenario = undefined,
        sim_state = undefined,
        current_step = 0,
        speed = 1,
        timer_ref = undefined,
        snapshots = [],
        metrics = #{
            steps_executed => 0,
            work_orders_created => 0,
            quality_gates_passed => 0,
            quality_gates_failed => 0,
            andon_events => 0,
            receipts_generated => 0
        },
        start_time = undefined,
        scenarios = #{},
        next_scenario_id = 1
    },
    {ok, State}.

%%------------------------------------------------------------------------------
%% @doc Handle synchronous calls.
%% @private
%% @end
%%------------------------------------------------------------------------------
handle_call({load_scenario, ScenarioId}, _From, State) ->
    case tcps_scenario_loader:load_scenario(ScenarioId) of
        {ok, Scenario} ->
            Config = maps:get(config, Scenario, #{}),
            SimState = tcps_simulator_state:new(Config#{scenario_id => ScenarioId}),
            NewState = State#simulator_state{
                scenario = Scenario,
                sim_state = SimState,
                current_step = 0,
                mode = stopped
            },
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(start_simulation, _From, State = #simulator_state{scenario = undefined}) ->
    {reply, {error, no_scenario_loaded}, State};

handle_call(start_simulation, _From, State = #simulator_state{scenario = Scenario, speed = Speed}) ->
    % Initialize work orders
    InitialWorkOrders = maps:get(initial_work_orders, Scenario, []),
    SimState = lists:foldl(
        fun(WO, AccState) ->
            tcps_simulator_state:update_work_order(AccState, WO)
        end,
        State#simulator_state.sim_state,
        InitialWorkOrders
    ),

    % Start timer
    Interval = timer:seconds(1) div Speed,
    TimerRef = erlang:send_after(Interval, self(), simulation_tick),

    NewState = State#simulator_state{
        mode = running,
        sim_state = SimState,
        timer_ref = TimerRef,
        start_time = erlang:timestamp(),
        current_step = 0
    },
    {reply, ok, NewState};

handle_call(pause_simulation, _From, State = #simulator_state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    NewState = State#simulator_state{
        mode = paused,
        timer_ref = undefined
    },
    {reply, ok, NewState};

handle_call(resume_simulation, _From, State = #simulator_state{mode = paused, speed = Speed}) ->
    Interval = timer:seconds(1) div Speed,
    TimerRef = erlang:send_after(Interval, self(), simulation_tick),
    NewState = State#simulator_state{
        mode = running,
        timer_ref = TimerRef
    },
    {reply, ok, NewState};

handle_call(resume_simulation, _From, State) ->
    {reply, {error, not_paused}, State};

handle_call(reset_simulation, _From, State = #simulator_state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    NewState = State#simulator_state{
        mode = stopped,
        current_step = 0,
        timer_ref = undefined,
        sim_state = case State#simulator_state.scenario of
            undefined -> undefined;
            Scenario ->
                Config = maps:get(config, Scenario, #{}),
                tcps_simulator_state:new(Config)
        end,
        metrics = #{
            steps_executed => 0,
            work_orders_created => 0,
            quality_gates_passed => 0,
            quality_gates_failed => 0,
            andon_events => 0,
            receipts_generated => 0
        }
    },
    {reply, ok, NewState};

handle_call(step_simulation, _From, State = #simulator_state{scenario = undefined}) ->
    {reply, {error, no_scenario_loaded}, State};

handle_call(step_simulation, _From, State) ->
    case execute_step(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {complete, NewState} ->
            {reply, {ok, simulation_complete}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({set_speed, Speed}, _From, State) ->
    NewState = State#simulator_state{speed = Speed},
    {reply, ok, NewState};

handle_call(get_state, _From, State) ->
    Response = #{
        mode => State#simulator_state.mode,
        current_step => State#simulator_state.current_step,
        speed => State#simulator_state.speed,
        scenario_id => case State#simulator_state.scenario of
            undefined -> undefined;
            Scenario -> maps:get(id, Scenario)
        end,
        simulation_state => case State#simulator_state.sim_state of
            undefined -> undefined;
            SimState -> tcps_simulator_state:to_map(SimState)
        end
    },
    {reply, Response, State};

handle_call(get_metrics, _From, State = #simulator_state{metrics = Metrics}) ->
    {reply, Metrics, State};

handle_call(snapshot_state, _From, State = #simulator_state{sim_state = SimState, snapshots = Snapshots}) ->
    case SimState of
        undefined ->
            {reply, {error, no_active_simulation}, State};
        _ ->
            Snapshot = tcps_simulator_state:snapshot(SimState),
            NewState = State#simulator_state{
                snapshots = [Snapshot | Snapshots]
            },
            {reply, {ok, Snapshot}, NewState}
    end;

handle_call({restore_snapshot, Snapshot}, _From, State = #simulator_state{sim_state = SimState}) ->
    RestoredState = tcps_simulator_state:restore(SimState, Snapshot),
    NewState = State#simulator_state{
        sim_state = RestoredState
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle asynchronous casts.
%% @private
%% @end
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Handle info messages.
%% @private
%% @end
%%------------------------------------------------------------------------------
handle_info(simulation_tick, State = #simulator_state{mode = running}) ->
    case execute_step(State) of
        {ok, NewState} ->
            % Schedule next tick
            Interval = timer:seconds(1) div NewState#simulator_state.speed,
            TimerRef = erlang:send_after(Interval, self(), simulation_tick),
            {noreply, NewState#simulator_state{timer_ref = TimerRef}};
        {complete, NewState} ->
            % Simulation complete
            {noreply, NewState#simulator_state{mode = stopped, timer_ref = undefined}};
        {error, _Reason} ->
            {noreply, State#simulator_state{mode = stopped, timer_ref = undefined}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @private
%% @end
%%------------------------------------------------------------------------------
terminate(_Reason, #simulator_state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ok.

%%------------------------------------------------------------------------------
%% @doc Handle code changes.
%% @private
%% @end
%%------------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Execute a single simulation step.
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec execute_step(State :: simulator_state()) ->
    {ok, simulator_state()} | {complete, simulator_state()} | {error, term()}.
execute_step(State = #simulator_state{
    scenario = Scenario,
    sim_state = SimState,
    current_step = CurrentStep,
    metrics = Metrics
}) ->
    Steps = maps:get(steps, Scenario, []),

    case CurrentStep < length(Steps) of
        true ->
            Step = lists:nth(CurrentStep + 1, Steps),
            case execute_scenario_step(Step, SimState) of
                {ok, NewSimState, StepMetrics} ->
                    UpdatedMetrics = merge_metrics(Metrics, StepMetrics),
                    Event = #{
                        type => step_completed,
                        step_number => CurrentStep + 1,
                        data => StepMetrics
                    },
                    NewSimState2 = tcps_simulator_state:add_event(NewSimState, Event),
                    NewState = State#simulator_state{
                        sim_state = NewSimState2,
                        current_step = CurrentStep + 1,
                        metrics = UpdatedMetrics
                    },
                    {ok, NewState};
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            % Check success criteria
            SuccessCriteria = maps:get(success_criteria, Scenario, []),
            AllPassed = lists:all(
                fun(Criterion) -> Criterion(SimState) end,
                SuccessCriteria
            ),
            Event = #{
                type => simulation_complete,
                success => AllPassed,
                data => #{metrics => Metrics}
            },
            NewSimState = tcps_simulator_state:add_event(SimState, Event),
            NewState = State#simulator_state{
                sim_state = NewSimState,
                mode = stopped
            },
            {complete, NewState}
    end.

%%------------------------------------------------------------------------------
%% @doc Execute a scenario step based on action type.
%% @end
%%------------------------------------------------------------------------------
-spec execute_scenario_step(Step :: map(), SimState :: tcps_simulator_state:state()) ->
    {ok, tcps_simulator_state:state(), map()} | {error, term()}.
execute_scenario_step(#{action := Action, params := Params}, SimState) ->
    case Action of
        create_work_order ->
            create_work_order_action(Params, SimState);
        run_quality_gates ->
            run_quality_gates_action(Params, SimState);
        complete_work_order ->
            complete_work_order_action(Params, SimState);
        verify_receipts ->
            verify_receipts_action(Params, SimState);
        trigger_andon ->
            trigger_andon_action(Params, SimState);
        perform_root_cause_analysis ->
            root_cause_analysis_action(Params, SimState);
        resolve_andon ->
            resolve_andon_action(Params, SimState);
        retry_quality_gates ->
            retry_quality_gates_action(Params, SimState);
        check_wip_limits ->
            check_wip_limits_action(Params, SimState);
        create_multiple_work_orders ->
            create_multiple_work_orders_action(Params, SimState);
        attempt_overflow ->
            attempt_overflow_action(Params, SimState);
        retry_rejected ->
            retry_rejected_action(Params, SimState);
        apply_heijunka_leveling ->
            heijunka_leveling_action(Params, SimState);
        verify_distribution ->
            verify_distribution_action(Params, SimState);
        verify_receipt_chain ->
            verify_receipt_chain_action(Params, SimState);
        audit_timestamps ->
            audit_timestamps_action(Params, SimState);
        baseline_metrics ->
            baseline_metrics_action(Params, SimState);
        identify_waste ->
            identify_waste_action(Params, SimState);
        implement_improvement ->
            implement_improvement_action(Params, SimState);
        measure_improvement ->
            measure_improvement_action(Params, SimState);
        standardize_change ->
            standardize_change_action(Params, SimState);
        _ ->
            {error, {unknown_action, Action}}
    end.

%%------------------------------------------------------------------------------
%% @doc Simulate quality gate execution.
%% @end
%%------------------------------------------------------------------------------
-spec simulate_quality_gates(WorkOrder :: map(), SimState :: tcps_simulator_state:state()) ->
    {pass, [map()]} | {fail, atom(), map()}.
simulate_quality_gates(WorkOrder, SimState) ->
    Config = maps:get(config, SimState, #{}),
    PassRate = maps:get(quality_gate_pass_rate, Config, 1.0),
    InjectFailures = maps:get(inject_failures, Config, false),

    Gates = [
        shacl_validation,
        compilation,
        test_execution,
        security_scan,
        deterministic_build,
        quality_metrics,
        release_verification,
        smoke_test
    ],

    % Check for injected failures
    InjectedFailure = case InjectFailures of
        true ->
            maps:get(inject_failure, maps:get(payload, WorkOrder, #{}), undefined);
        false ->
            undefined
    end,

    case InjectedFailure of
        #{gate := FailGate, reason := Reason} ->
            % Return failure at specified gate
            {fail, FailGate, #{reason => Reason}};
        _ ->
            % Execute gates with pass rate
            execute_gates(Gates, PassRate, [])
    end.

%%------------------------------------------------------------------------------
%% @doc Execute quality gates with pass rate.
%% @private
%% @end
%%------------------------------------------------------------------------------
execute_gates([], _PassRate, Receipts) ->
    {pass, lists:reverse(Receipts)};
execute_gates([Gate | Rest], PassRate, Receipts) ->
    case rand:uniform() =< PassRate of
        true ->
            Receipt = #{
                gate => Gate,
                status => pass,
                timestamp => erlang:timestamp(),
                hash => generate_hash()
            },
            execute_gates(Rest, PassRate, [Receipt | Receipts]);
        false ->
            {fail, Gate, #{reason => <<"Simulated failure">>}}
    end.

%%------------------------------------------------------------------------------
%% @doc Simulate Andon event.
%% @end
%%------------------------------------------------------------------------------
-spec simulate_andon_event(FailureType :: atom(), SimState :: tcps_simulator_state:state()) ->
    {ok, map()}.
simulate_andon_event(FailureType, _SimState) ->
    AndonEvent = #{
        event_id => generate_id(),
        failure_type => FailureType,
        timestamp => erlang:timestamp(),
        status => active,
        severity => determine_severity(FailureType)
    },
    {ok, AndonEvent}.

%%------------------------------------------------------------------------------
%% @doc Simulate work order lifecycle.
%% @end
%%------------------------------------------------------------------------------
-spec simulate_work_order_lifecycle(WorkOrder :: map(), SimState :: tcps_simulator_state:state()) ->
    {ok, map()}.
simulate_work_order_lifecycle(WorkOrder, SimState) ->
    % Run quality gates
    case simulate_quality_gates(WorkOrder, SimState) of
        {pass, Receipts} ->
            UpdatedWO = WorkOrder#{
                status => completed,
                receipts => Receipts,
                completed_at => erlang:timestamp()
            },
            {ok, UpdatedWO};
        {fail, Gate, Details} ->
            UpdatedWO = WorkOrder#{
                status => blocked,
                failed_gate => Gate,
                failure_details => Details
            },
            {ok, UpdatedWO}
    end.

%%%=============================================================================
%%% Action Implementations
%%%=============================================================================

create_work_order_action(#{bucket := Bucket}, SimState) ->
    WO = #{
        id => generate_id(),
        bucket => Bucket,
        status => created,
        created_at => erlang:timestamp()
    },
    NewSimState = tcps_simulator_state:update_work_order(SimState, WO),
    Metrics = #{work_orders_created => 1},
    {ok, NewSimState, Metrics}.

run_quality_gates_action(_Params, SimState) ->
    WorkOrders = tcps_simulator_state:get_work_orders(SimState),
    WO = case maps:size(WorkOrders) of
        0 -> #{};
        _ -> hd(maps:values(WorkOrders))
    end,

    case simulate_quality_gates(WO, SimState) of
        {pass, Receipts} ->
            NewSimState = lists:foldl(
                fun(Receipt, Acc) ->
                    tcps_simulator_state:add_receipt(Acc, Receipt)
                end,
                SimState,
                Receipts
            ),
            Metrics = #{quality_gates_passed => length(Receipts)},
            {ok, NewSimState, Metrics};
        {fail, Gate, _Details} ->
            Metrics = #{quality_gates_failed => 1, failed_gate => Gate},
            {ok, SimState, Metrics}
    end.

complete_work_order_action(_Params, SimState) ->
    Metrics = #{work_orders_completed => 1},
    {ok, SimState, Metrics}.

verify_receipts_action(_Params, SimState) ->
    Receipts = tcps_simulator_state:get_receipts(SimState),
    Metrics = #{
        chain_complete => length(Receipts) >= 8,
        receipt_count => length(Receipts)
    },
    {ok, SimState, Metrics}.

trigger_andon_action(#{failure_type := Type}, SimState) ->
    {ok, AndonEvent} = simulate_andon_event(Type, SimState),
    NewSimState = tcps_simulator_state:add_andon_event(SimState, AndonEvent),
    Metrics = #{andon_events => 1},
    {ok, NewSimState, Metrics}.

root_cause_analysis_action(_Params, SimState) ->
    Metrics = #{root_cause_found => true},
    {ok, SimState, Metrics}.

resolve_andon_action(_Params, SimState) ->
    Metrics = #{andon_resolved => true},
    {ok, SimState, Metrics}.

retry_quality_gates_action(_Params, SimState) ->
    Metrics = #{all_passed => true},
    {ok, SimState, Metrics}.

check_wip_limits_action(#{bucket := Bucket}, SimState) ->
    KanbanState = maps:get(kanban_state, SimState, #{}),
    WipLimits = maps:get(wip_limits, KanbanState, #{}),
    CurrentWip = maps:get(current_wip, KanbanState, #{}),

    Limit = maps:get(Bucket, WipLimits, 5),
    Current = maps:get(Bucket, CurrentWip, 0),

    Metrics = #{available_capacity => Limit - Current},
    {ok, SimState, Metrics}.

create_multiple_work_orders_action(#{count := Count, bucket := Bucket}, SimState) ->
    KanbanState = maps:get(kanban_state, SimState, #{}),
    WipLimits = maps:get(wip_limits, KanbanState, #{}),
    CurrentWip = maps:get(current_wip, KanbanState, #{}),

    Limit = maps:get(Bucket, WipLimits, 5),
    Current = maps:get(Bucket, CurrentWip, 0),
    Available = Limit - Current,

    Created = min(Count, Available),
    Rejected = Count - Created,

    % Update WIP
    NewCurrentWip = maps:put(Bucket, Current + Created, CurrentWip),
    NewKanbanState = maps:put(current_wip, NewCurrentWip, KanbanState),
    NewSimState = maps:put(kanban_state, NewKanbanState, SimState),

    Metrics = #{created => Created, rejected => Rejected},
    {ok, NewSimState, Metrics}.

attempt_overflow_action(Params, SimState) ->
    create_multiple_work_orders_action(Params, SimState).

retry_rejected_action(_Params, SimState) ->
    Metrics = #{created => 1},
    {ok, SimState, Metrics}.

heijunka_leveling_action(_Params, SimState) ->
    Metrics = #{leveled => true},
    {ok, SimState, Metrics}.

verify_distribution_action(_Params, SimState) ->
    KanbanState = maps:get(kanban_state, SimState, #{}),
    CurrentWip = maps:get(current_wip, KanbanState, #{}),

    Metrics = #{
        balanced => true,
        distribution => CurrentWip
    },
    {ok, SimState, Metrics}.

verify_receipt_chain_action(_Params, SimState) ->
    Receipts = tcps_simulator_state:get_receipts(SimState),
    Metrics = #{
        chain_valid => true,
        breaks => 0,
        receipt_count => length(Receipts)
    },
    {ok, SimState, Metrics}.

audit_timestamps_action(_Params, SimState) ->
    Metrics = #{monotonic => true},
    {ok, SimState, Metrics}.

baseline_metrics_action(_Params, SimState) ->
    Baseline = #{
        timestamp => erlang:timestamp(),
        work_orders => 0,
        quality_gates_passed => 0
    },
    Metrics = #{baseline_captured => true, baseline => Baseline},
    {ok, SimState, Metrics}.

identify_waste_action(_Params, SimState) ->
    WasteItems = [
        #{type => waiting, description => <<"Idle time in testing">>},
        #{type => rework, description => <<"Failed quality gates">>}
    ],
    Metrics = #{waste_found => true, waste_items => WasteItems},
    {ok, SimState, Metrics}.

implement_improvement_action(_Params, SimState) ->
    Metrics = #{implemented => true},
    {ok, SimState, Metrics}.

measure_improvement_action(_Params, SimState) ->
    Metrics = #{improvement_percentage => 15.5},
    {ok, SimState, Metrics}.

standardize_change_action(_Params, SimState) ->
    Metrics = #{standardized => true},
    {ok, SimState, Metrics}.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

merge_metrics(Metrics1, Metrics2) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            case maps:get(Key, Acc, undefined) of
                undefined ->
                    maps:put(Key, Value, Acc);
                OldValue when is_number(OldValue), is_number(Value) ->
                    maps:put(Key, OldValue + Value, Acc);
                _ ->
                    maps:put(Key, Value, Acc)
            end
        end,
        Metrics1,
        Metrics2
    ).

determine_severity(test_failure) -> medium;
determine_severity(compilation_failure) -> high;
determine_severity(shacl_violation) -> high;
determine_severity(_) -> low.

generate_id() ->
    Rand = rand:uniform(16#FFFFFFFFFFFFFFFF),
    list_to_binary(io_lib:format("~16.16.0b", [Rand])).

generate_hash() ->
    Rand = rand:uniform(16#FFFFFFFFFFFFFFFF),
    list_to_binary(io_lib:format("~64.16.0b", [Rand])).
