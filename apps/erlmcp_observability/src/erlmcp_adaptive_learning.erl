%%%-------------------------------------------------------------------
%%% @doc
%%% Adaptive Learning System Core for erlmcp v3 Operations
%%%
%%% This module implements the 4-step intelligence pipeline:
%%% RETRIEVE (HNSW search) -> JUDGE (verdict) -> DISTILL (pattern extraction)
%%%   -> CONSOLIDATE (EWC++)
%%%
%%% The system learns from operational trajectories to optimize performance
%%% and prevent failures through adaptive pattern recognition.
%%%
%%% == Architecture ==
%%%
%%%    ┌──────────────────────────────────────────────────────────────┐
%%%    │                    ADAPTIVE LEARNING SYSTEM                  │
%%%    ├──────────────────────────────────────────────────────────────┤
%%%    │                                                               │
%%%    │  ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌───────┐ │
%%%    │  │ RETRIEVE │───▶│  JUDGE   │───▶│ DISTILL  │───▶│EWC++  │ │
%%%    │  │          │    │          │    │          │    │       │ │
%%%    │  │ HNSW     │    │ Verdicts │    │ LoRA     │    │Prevent│ │
%%%    │  │ 150x     │    │ Success/ │    │ Extract  │    │Forget │ │
%%%    │  │ faster   │    │ Failure  │    │ Learnings│    │       │ │
%%%    │  └──────────┘    └──────────┘    └──────────┘    └───────┘ │
%%%    │       │               │               │               │      │
%%%    │       ▼               ▼               ▼               ▼      │
%%%    │  ┌─────────────────────────────────────────────────────┐  │
%%%    │  │                    PATTERN MEMORY                   │  │
%%%    │  │  AgentDB + HNSW Index + SQLite Persistence          │  │
%%%    │  └─────────────────────────────────────────────────────┘  │
%%%    │                                                               │
%%%    └──────────────────────────────────────────────────────────────┘
%%%
%%% == Usage ==
%%%
%%% Start the learning system:
%%%   erlmcp_adaptive_learning:start_link().
%%%
%%% Track a trajectory:
%%%   erlmcp_adaptive_learning:start_trajectory(SessionId, AgentType, Task).
%%%   erlmcp_adaptive_learning:record_step(SessionId, Operation, Outcome).
%%%   erlmcp_adaptive_learning:end_trajectory(SessionId, Verdict, Reward).
%%%
%%% Query similar patterns:
%%%   erlmcp_adaptive_learning:find_similar_patterns(Task, AgentType).
%%%
%%% Get optimization recommendations:
%%%   erlmcp_adaptive_learning:get_recommendations().
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_adaptive_learning).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([start_trajectory/3, start_trajectory/4]).
-export([record_step/4, record_step/5]).
-export([end_trajectory/3, end_trajectory/4]).
-export([find_similar_patterns/2, find_similar_patterns/3]).
-export([get_pattern/1, get_all_patterns/0]).
-export([get_recommendations/0, get_recommendations/1]).
-export([set_optimization_policy/2]).
-export([force_consolidation/0]).
-export([get_anomalies/0, get_anomalies/1]).
-export([get_metrics/0, reset_metrics/0]).
-export([export_patterns/1, import_patterns/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include("erlmcp_learning.hrl").

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type config() :: #{
    embedding_dim => embedding_dim(),
    distance_metric => distance_metric(),
    replay_buffer_size => pos_integer(),
    consolidation_interval_ms => pos_integer(),
    optimization_interval_ms => pos_integer(),
    anomaly_sigma_threshold => float()
}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Start the adaptive learning system with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the adaptive learning system with custom config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Start tracking a new trajectory
-spec start_trajectory(session_id(), agent_type(), binary()) ->
    {ok, trajectory_id()} | {error, term()}.
start_trajectory(SessionId, AgentType, TaskDescription) ->
    start_trajectory(SessionId, AgentType, TaskDescription, #{}).

-spec start_trajectory(session_id(), agent_type(), binary(), map()) ->
    {ok, trajectory_id()} | {error, term()}.
start_trajectory(SessionId, AgentType, TaskDescription, Metadata) ->
    gen_server:call(?MODULE, {start_trajectory, SessionId, AgentType,
                              TaskDescription, Metadata}).

%% @doc Record a step within a trajectory
-spec record_step(session_id(), operation_name(), outcome(), non_neg_integer()) ->
    ok | {error, term()}.
record_step(SessionId, Operation, Outcome, DurationMs) ->
    record_step(SessionId, Operation, Outcome, DurationMs, #{}).

-spec record_step(session_id(), operation_name(), outcome(), non_neg_integer(), map()) ->
    ok | {error, term()}.
record_step(SessionId, Operation, Outcome, DurationMs, Metrics) ->
    gen_server:cast(?MODULE, {record_step, SessionId, Operation,
                             Outcome, DurationMs, Metrics}).

%% @doc End a trajectory with verdict and reward
-spec end_trajectory(session_id(), verdict(), reward_value()) ->
    ok | {error, term()}.
end_trajectory(SessionId, Verdict, Reward) ->
    end_trajectory(SessionId, Verdict, Reward, #{}).

-spec end_trajectory(session_id(), verdict(), reward_value(), map()) ->
    ok | {error, term()}.
end_trajectory(SessionId, Verdict, Reward, Metadata) ->
    gen_server:cast(?MODULE, {end_trajectory, SessionId, Verdict, Reward, Metadata}).

%% @doc Find patterns similar to a task description
-spec find_similar_patterns(binary(), agent_type()) ->
    {ok, [#pattern_match{}]} | {error, term()}.
find_similar_patterns(TaskDescription, AgentType) ->
    find_similar_patterns(TaskDescription, AgentType, 10).

-spec find_similar_patterns(binary(), agent_type(), pos_integer()) ->
    {ok, [#pattern_match{}]} | {error, term()}.
find_similar_patterns(TaskDescription, AgentType, K) ->
    gen_server:call(?MODULE, {find_similar, TaskDescription, AgentType, K}).

%% @doc Get a specific pattern by ID
-spec get_pattern(pattern_id()) -> {ok, #pattern{}} | {error, not_found}.
get_pattern(PatternId) ->
    gen_server:call(?MODULE, {get_pattern, PatternId}).

%% @doc Get all stored patterns
-spec get_all_patterns() -> {ok, [#pattern{}]}.
get_all_patterns() ->
    gen_server:call(?MODULE, get_all_patterns).

%% @doc Get optimization recommendations
-spec get_recommendations() -> {ok, [#recommendation{}]}.
get_recommendations() ->
    get_recommendations(10).

-spec get_recommendations(pos_integer()) -> {ok, [#recommendation{}]}.
get_recommendations(Limit) ->
    gen_server:call(?MODULE, {get_recommendations, Limit}).

%% @doc Set an optimization policy
-spec set_optimization_policy(binary(), map()) -> ok | {error, term()}.
set_optimization_policy(PolicyName, Parameters) ->
    gen_server:call(?MODULE, {set_policy, PolicyName, Parameters}).

%% @doc Force pattern consolidation
-spec force_consolidation() -> ok.
force_consolidation() ->
    gen_server:cast(?MODULE, force_consolidation).

%% @doc Get detected anomalies
-spec get_anomalies() -> {ok, [#anomaly{}]}.
get_anomalies() ->
    get_anomalies(100).

-spec get_anomalies(pos_integer()) -> {ok, [#anomaly{}]}.
get_anomalies(Limit) ->
    gen_server:call(?MODULE, {get_anomalies, Limit}).

%% @doc Get learning system metrics
-spec get_metrics() -> {ok, map()}.
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%% @doc Reset learning metrics
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%% @doc Export patterns to file
-spec export_patterns(file:filename()) -> ok | {error, term()}.
export_patterns(Filename) ->
    gen_server:call(?MODULE, {export_patterns, Filename}).

%% @doc Import patterns from file
-spec import_patterns(file:filename()) -> {ok, pos_integer()} | {error, term()}.
import_patterns(Filename) ->
    gen_server:call(?MODULE, {import_patterns, Filename}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init(Opts) ->
    ?LOG_INFO("Starting adaptive learning system with opts: ~p", [Opts]),

    %% Parse configuration
    Config = parse_config(Opts),

    %% Initialize HNSW index
    HNSWIndex = initialize_hnsw_index(Config),

    %% Initialize state
    State = #learning_state{
        trajectories = #{},
        patterns = #{},
        hnsw_index = HNSWIndex,
        replay_buffer = initialize_replay_buffer(Config),
        anomaly_detector = initialize_anomaly_detector(Config),
        optimization_state = initialize_optimization_state(Config),
        consolidation_state = initialize_consolidation_state(Config),
        recommendation_engine = initialize_recommendation_engine(),
        metrics = initialize_metrics()
    },

    %% Start consolidation timer
    ConsolidationInterval = maps:get(consolidation_interval_ms, Config, ?CONSOLIDATION_INTERVAL_MS),
    {ok, _ConsolRef} = timer:send_interval(ConsolidationInterval, consolidate_patterns),

    %% Start optimization timer
    OptimizationInterval = maps:get(optimization_interval_ms, Config, ?OPTIMIZATION_INTERVAL_MS),
    {ok, _OptRef} = timer:send_interval(OptimizationInterval, optimize_policies),

    ?LOG_INFO("Adaptive learning system initialized"),
    {ok, State}.

handle_call({start_trajectory, SessionId, AgentType, TaskDesc, Metadata},
            _From, State) ->
    TrajectoryId = ?GENERATE_TRAJECTORY_ID,
    StartTime = erlang:system_time(microsecond),

    Trajectory = #trajectory{
        id = TrajectoryId,
        session_id = SessionId,
        agent_type = AgentType,
        task_description = TaskDesc,
        start_time = StartTime,
        steps = [],
        verdict = undefined,
        reward = undefined,
        metadata = Metadata,
        embedding = undefined
    },

    NewTrajectories = maps:put(SessionId, Trajectory, State#learning_state.trajectories),
    NewMetrics = increment_metric(trajectories_started, State#learning_state.metrics),

    {reply, {ok, TrajectoryId}, State#learning_state{trajectories = NewTrajectories,
                                                     metrics = NewMetrics}};

handle_call({find_similar, TaskDesc, AgentType, K}, _From, State) ->
    %% Generate embedding for query
    QueryEmbedding = generate_embedding(TaskDesc, AgentType),

    %% Search HNSW index
    {ok, Matches} = erlmcp_hnsw:search(State#learning_state.hnsw_index,
                                        QueryEmbedding, K),

    %% Get full pattern details
    PatternMatches = lists:map(fun({PatternId, Similarity}) ->
        case maps:get(PatternId, State#learning_state.patterns, undefined) of
            undefined ->
                #pattern_match{pattern_id = PatternId,
                               similarity = Similarity,
                               reward = 0.0,
                               confidence = 0.0,
                               pattern = undefined};
            Pattern ->
                #pattern_match{pattern_id = PatternId,
                               similarity = Similarity,
                               reward = Pattern#pattern.avg_reward,
                               confidence = calculate_confidence(Pattern),
                               pattern = Pattern}
        end
    end, Matches),

    {reply, {ok, PatternMatches}, State};

handle_call({get_pattern, PatternId}, _From, State) ->
    case maps:get(PatternId, State#learning_state.patterns, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Pattern -> {reply, {ok, Pattern}, State}
    end;

handle_call(get_all_patterns, _From, State) ->
    Patterns = maps:values(State#learning_state.patterns),
    {reply, {ok, Patterns}, State};

handle_call({get_recommendations, Limit}, _From, State) ->
    Recommendations = State#learning_state.recommendation_engine#recommendation_engine.active_recommendations,
    LimitedRecs = lists:sublist(Recommendations, Limit),
    {reply, {ok, LimitedRecs}, State};

handle_call({set_policy, PolicyName, Parameters}, _From, State) ->
    Policy = #optimization_policy{
        name = PolicyName,
        parameters = Parameters,
        performance_score = 0.0,
        last_updated = erlang:system_time(millisecond),
        update_count = 0
    },
    NewOptState = (State#learning_state.optimization_state)#optimization_state{
        policies = maps:put(PolicyName, Policy, State#learning_state.optimization_state#optimization_state.policies)
    },
    {reply, ok, State#learning_state{optimization_state = NewOptState}};

handle_call({get_anomalies, Limit}, _From, State) ->
    %% Return recent anomalies from detector
    Anomalies = erlmcp_anomaly_detector:get_recent_anomalies(
        State#learning_state.anomaly_detector, Limit),
    {reply, {ok, Anomalies}, State};

handle_call(get_metrics, _From, State) ->
    Metrics = State#learning_state.metrics,
    {reply, {ok, Metrics}, State};

handle_call(reset_metrics, _From, State) ->
    {reply, ok, State#learning_state{metrics = initialize_metrics()}};

handle_call({export_patterns, Filename}, _From, State) ->
    Patterns = maps:values(State#learning_state.patterns),
    case export_patterns_to_file(Patterns, Filename) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({import_patterns, Filename}, _From, State) ->
    case import_patterns_from_file(Filename) of
        {ok, ImportedPatterns} ->
            %% Merge into state
            NewPatterns = lists:foldl(fun(Pattern, Acc) ->
                maps:put(Pattern#pattern.id, Pattern, Acc)
            end, State#learning_state.patterns, ImportedPatterns),

            %% Update HNSW index
            NewIndex = lists:foldl(fun(Pattern, Index) ->
                erlmcp_hnsw:insert(Index, Pattern#pattern.id, Pattern#pattern.embedding)
            end, State#learning_state.hnsw_index, ImportedPatterns),

            NewMetrics = maps:update_with(patterns_imported,
                                         fun(V) -> V + length(ImportedPatterns) end,
                                         length(ImportedPatterns),
                                         State#learning_state.metrics),

            {reply, {ok, length(ImportedPatterns)}, State#learning_state{
                patterns = NewPatterns,
                hnsw_index = NewIndex,
                metrics = NewMetrics
            }};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({record_step, SessionId, Operation, Outcome, DurationMs, Metrics}, State) ->
    NewState = case maps:get(SessionId, State#learning_state.trajectories, undefined) of
        undefined ->
            ?LOG_WARNING("Recording step for unknown trajectory: ~p", [SessionId]),
            State;
        Trajectory ->
            StepId = base64:encode(crypto:strong_rand_bytes(8)),
            Timestamp = erlang:system_time(millisecond),

            Step = #trajectory_step{
                step_id = StepId,
                operation = Operation,
                timestamp = Timestamp,
                outcome = Outcome,
                duration_ms = DurationMs,
                metrics = Metrics,
                error_reason = get_error_reason(Outcome),
                metadata = #{}
            },

            NewTrajectory = Trajectory#trajectory{
                steps = Trajectory#trajectory.steps ++ [Step]
            },
            NewTrajectories = maps:put(SessionId, NewTrajectory, State#learning_state.trajectories),
            State#learning_state{trajectories = NewTrajectories}
    end,
    {noreply, NewState};

handle_cast({end_trajectory, SessionId, Verdict, Reward, Metadata}, State) ->
    NewState = case maps:get(SessionId, State#learning_state.trajectories, undefined) of
        undefined ->
            ?LOG_WARNING("Ending unknown trajectory: ~p", [SessionId]),
            State;
        Trajectory ->
            EndTime = erlang:system_time(microsecond),

            CompletedTrajectory = Trajectory#trajectory{
                end_time = EndTime,
                verdict = Verdict,
                reward = Reward,
                metadata = maps:merge(Trajectory#trajectory.metadata, Metadata),
                embedding = generate_embedding(Trajectory#trajectory.task_description,
                                              Trajectory#trajectory.agent_type)
            },

            %% Process through learning pipeline
            StateAfterProcessing = process_completed_trajectory(CompletedTrajectory, State),

            %% Remove from active trajectories
            NewTrajectories = maps:remove(SessionId, StateAfterProcessing#learning_state.trajectories),
            StateAfterProcessing#learning_state{trajectories = NewTrajectories}
    end,
    {noreply, NewState};

handle_cast(force_consolidation, State) ->
    ?LOG_INFO("Forcing pattern consolidation"),
    NewState = consolidate_patterns(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(consolidate_patterns, State) ->
    ?LOG_DEBUG("Periodic pattern consolidation"),
    NewState = consolidate_patterns(State),
    {noreply, NewState};

handle_info(optimize_policies, State) ->
    ?LOG_DEBUG("Periodic policy optimization"),
    NewState = optimize_policies(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ?LOG_INFO("Adaptive learning system terminating"),
    %% Persist state before shutdown
    persist_state(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @private Parse configuration options
-spec parse_config(map()) -> config().
parse_config(Opts) ->
    #{
        embedding_dim => maps:get(embedding_dim, Opts, 256),
        distance_metric => maps:get(distance_metric, Opts, cosine),
        replay_buffer_size => maps:get(replay_buffer_size, Opts, ?REPLAY_BUFFER_SIZE),
        consolidation_interval_ms => maps:get(consolidation_interval_ms, Opts, ?CONSOLIDATION_INTERVAL_MS),
        optimization_interval_ms => maps:get(optimization_interval_ms, Opts, ?OPTIMIZATION_INTERVAL_MS),
        anomaly_sigma_threshold => maps:get(anomaly_sigma_threshold, Opts, ?ANOMALY_SIGMA_THRESHOLD)
    }.

%% @private Initialize HNSW index
-spec initialize_hnsw_index(config()) -> #hnsw_index{}.
initialize_hnsw_index(Config) ->
    Dim = maps:get(embedding_dim, Config),
    Metric = maps:get(distance_metric, Config),
    erlmcp_hnsw:new(Dim, Metric, #{
        m => ?HNSW_M_DEFAULT,
        ef_construction => ?HNSW_EF_CONSTRUCTION,
        ef_search => ?HNSW_EF_SEARCH,
        max_elements => ?HNSW_MAX_ELEMENTS
    }).

%% @private Initialize replay buffer
-spec initialize_replay_buffer(config()) -> #replay_buffer{}.
initialize_replay_buffer(Config) ->
    Capacity = maps:get(replay_buffer_size, Config, ?REPLAY_BUFFER_SIZE),
    #replay_buffer{
        capacity = Capacity,
        experiences = [],
        priorities = #{},
        sampling_distribution = prioritized
    }.

%% @private Initialize anomaly detector
-spec initialize_anomaly_detector(config()) -> #anomaly_detector{}.
initialize_anomaly_detector(Config) ->
    SigmaThreshold = maps:get(anomaly_sigma_threshold, Config, ?ANOMALY_SIGMA_THRESHOLD),
    #anomaly_detector{
        baselines = #{},
        window_size = ?ANOMALY_WINDOW_SIZE,
        sigma_threshold = SigmaThreshold,
        min_observations = ?ANOMALY_MIN_OBSERVATIONS,
        alert_handlers = []
    }.

%% @private Initialize optimization state
-spec initialize_optimization_state(config()) -> #optimization_state{}.
initialize_optimization_state(_Config) ->
    #optimization_state{
        policies = #{},
        current_performance = 0.5,
        baseline_performance = 0.5,
        optimization_interval = ?OPTIMIZATION_INTERVAL_MS,
        learning_rate = ?OPTIMIZATION_LEARNING_RATE,
        convergence_threshold = 0.001
    }.

%% @private Initialize consolidation state
-spec initialize_consolidation_state(config()) -> #consolidation_state{}.
initialize_consolidation_state(_Config) ->
    CheckpointId = ?GENERATE_PATTERN_ID,
    #consolidation_state{
        current_checkpoint = #model_checkpoint{
            id = CheckpointId,
            parameters = #{},
            fisher_info = [],
            performance = 0.0,
            task_count = 0,
            created_at = erlang:system_time(millisecond)
        },
        ewc_lambda = ?EWC_LAMBDA,
        consolidation_interval = ?CONSOLIDATION_INTERVAL_MS,
        last_consolidation = erlang:system_time(millisecond),
        consolidated_patterns = []
    }.

%% @private Initialize recommendation engine
-spec initialize_recommendation_engine() -> #recommendation_engine{}.
initialize_recommendation_engine() ->
    #recommendation_engine{
        active_recommendations = [],
        pattern_index = undefined,
        anomaly_detector = undefined,
        optimizer = undefined,
        max_recommendations = 10
    }.

%% @private Initialize metrics
-spec initialize_metrics() -> map().
initialize_metrics() ->
    #{
        trajectories_started => 0,
        trajectories_completed => 0,
        patterns_learned => 0,
        patterns_promoted => 0,
        patterns_demoted => 0,
        anomalies_detected => 0,
        optimizations_applied => 0,
        consolidations_performed => 0,
        total_reward => 0.0,
        avg_reward => 0.0,
        patterns_imported => 0,
        uptime_seconds => 0,
        last_update => erlang:system_time(millisecond)
    }.

%% @private Increment a metric
-spec increment_metric(atom(), map()) -> map().
increment_metric(Key, Metrics) ->
    maps:update_with(Key, fun(V) -> V + 1 end, 1, Metrics).

%% @private Process a completed trajectory through learning pipeline
-spec process_completed_trajectory(#trajectory{}, #learning_state{}) -> #learning_state{}.
process_completed_trajectory(Trajectory, State) ->
    %% Step 1: Store experience in replay buffer
    State1 = store_experience(Trajectory, State),

    %% Step 2: Check for anomalies
    State2 = check_for_anomalies(Trajectory, State1),

    %% Step 3: Extract and distill pattern if successful
    State3 = case Trajectory#trajectory.verdict of
        success ->
            extract_and_store_pattern(Trajectory, State2);
        _ ->
            State2
    end,

    %% Step 4: Update metrics
    NewMetrics = update_completion_metrics(Trajectory, State3#learning_state.metrics),
    State3#learning_state{metrics = NewMetrics}.

%% @private Store trajectory experience
-spec store_experience(#trajectory{}, #learning_state{}) -> #learning_state{}.
store_experience(Trajectory, State) ->
    Experience = #experience{
        id = ?GENERATE_PATTERN_ID,
        trajectory_id = Trajectory#trajectory.id,
        state = extract_state_features(Trajectory),
        action = extract_action_sequence(Trajectory),
        reward = Trajectory#trajectory.reward,
        next_state = #{},  % Terminal state
        done = true,
        priority = calculate_priority(Trajectory),
        timestamp = erlang:system_time(millisecond)
    },

    Buffer = State#learning_state.replay_buffer,
    NewBuffer = erlmcp_replay_buffer:add(Buffer, Experience),

    State#learning_state{replay_buffer = NewBuffer}.

%% @private Extract state features from trajectory
-spec extract_state_features(#trajectory{}) -> feature_map().
extract_state_features(Trajectory) ->
    #{
        agent_type => Trajectory#trajectory.agent_type,
        task_length => byte_size(Trajectory#trajectory.task_description),
        step_count => length(Trajectory#trajectory.steps),
        total_duration => case Trajectory#trajectory.end_time of
            undefined -> 0;
            EndTime -> EndTime - Trajectory#trajectory.start_time
        end
    }.

%% @private Extract action sequence from trajectory
-spec extract_action_sequence(#trajectory{}) -> binary().
extract_action_sequence(Trajectory) ->
    Actions = [S#trajectory_step.operation || S <- Trajectory#trajectory.steps],
    list_to_binary(lists:join(<<",">>, Actions)).

%% @private Calculate priority for prioritized replay
-spec calculate_priority(#trajectory{}) -> float().
calculate_priority(Trajectory) ->
    %% Higher priority for:
    %% - High rewards (success)
    %% - Low rewards (to learn from failures)
    %% - Rare events
    Reward = Trajectory#trajectory.reward,
    abs(Reward - 0.5) * 2.0.  % 0.0 - 1.0 priority

%% @private Check for anomalies in trajectory
-spec check_for_anomalies(#trajectory{}, #learning_state{}) -> #learning_state{}.
check_for_anomalies(Trajectory, State) ->
    Detector = State#learning_state.anomaly_detector,
    Features = extract_state_features(Trajectory),

    {Anomalies, NewDetector} = erlmcp_anomaly_detector:check(Detector, Features),

    NewMetrics = case Anomalies of
        [] -> State#learning_state.metrics;
        _ ->
            %% Update anomaly count
            maps:update_with(anomalies_detected, fun(V) -> V + length(Anomalies) end,
                            length(Anomalies), State#learning_state.metrics)
    end,

    State#learning_state{
        anomaly_detector = NewDetector,
        metrics = NewMetrics
    }.

%% @private Extract and store pattern from successful trajectory
-spec extract_and_store_pattern(#trajectory{}, #learning_state{}) -> #learning_state{}.
extract_and_store_pattern(Trajectory, State) ->
    %% Check if reward meets threshold
    case Trajectory#trajectory.reward >= ?PATTERN_MIN_REWARD of
        false ->
            State;
        true ->
            TaskSignature = normalize_task_signature(Trajectory#trajectory.task_description),
            Approach = determine_approach(Trajectory),
            Steps = extract_step_signatures(Trajectory),
            PatternId = pattern_id(TaskSignature, Approach),

            Pattern = #pattern{
                id = PatternId,
                task_signature = TaskSignature,
                agent_type = Trajectory#trajectory.agent_type,
                approach = Approach,
                steps = Steps,
                success_count = 1,
                failure_count = 0,
                avg_reward = Trajectory#trajectory.reward,
                reward_variance = 0.0,
                last_used = erlang:system_time(millisecond),
                created_at = erlang:system_time(millisecond),
                embedding = Trajectory#trajectory.embedding,
                metadata = #{
                    trajectory_count => 1,
                    first_seen => erlang:system_time(millisecond)
                }
            },

            %% Merge or insert pattern
            {NewPatterns, NewIndex} = case maps:get(PatternId, State#learning_state.patterns, undefined) of
                undefined ->
                    %% New pattern - add to HNSW
                    NewHNSW = erlmcp_hnsw:insert(State#learning_state.hnsw_index,
                                                 PatternId, Pattern#pattern.embedding),
                    {maps:put(PatternId, Pattern, State#learning_state.patterns), NewHNSW};
                ExistingPattern ->
                    %% Update existing pattern
                    UpdatedPattern = update_pattern(ExistingPattern, Trajectory),
                    {maps:put(PatternId, UpdatedPattern, State#learning_state.patterns),
                     State#learning_state.hnsw_index}
            end,

            NewMetrics = maps:update_with(patterns_learned, fun(V) -> V + 1 end,
                                        1, State#learning_state.metrics),

            State#learning_state{
                patterns = NewPatterns,
                hnsw_index = NewIndex,
                metrics = NewMetrics
            }
    end.

%% @private Update existing pattern with new trajectory data
-spec update_pattern(#pattern{}, #trajectory{}) -> #pattern{}.
update_pattern(Pattern, Trajectory) ->
    NewSuccessCount = Pattern#pattern.success_count + 1,
    TotalCount = NewSuccessCount + Pattern#pattern.failure_count,

    %% Update average reward using running average
    NewAvgReward = (Pattern#pattern.avg_reward * Pattern#pattern.success_count +
                    Trajectory#trajectory.reward) / NewSuccessCount,

    Pattern#pattern{
        success_count = NewSuccessCount,
        avg_reward = NewAvgReward,
        last_used = erlang:system_time(millisecond),
        metadata = maps:update_with(trajectory_count, fun(V) -> V + 1 end,
                                   1, Pattern#pattern.metadata)
    }.

%% @private Normalize task signature
-spec normalize_task_signature(binary()) -> binary().
normalize_task_signature(Task) ->
    %% Lowercase, remove extra whitespace, normalize
    Normalized = string:lowercase(binary_to_list(Task)),
    Trimmed = string:trim(Normalized),
    list_to_binary(re:replace(Trimmed, "\\s+", " ", [global, {return, list}])).

%% @private Determine approach from trajectory
-spec determine_approach(#trajectory{}) -> binary().
determine_approach(Trajectory) ->
    %% Heuristic: use operation sequence as approach signature
    Operations = [S#trajectory_step.operation || S <- Trajectory#trajectory.steps],
    ApproachHash = crypto:hash(md5, term_to_binary(Operations)),
    <<"approach_", (base64:encode(ApproachHash))/binary, "_", (byte_size(Operations))/binary>>.

%% @private Extract step signatures
-spec extract_step_signatures(#trajectory{}) -> [binary()].
extract_step_signatures(Trajectory) ->
    [S#trajectory_step.operation || S <- Trajectory#trajectory.steps].

%% @private Generate pattern ID
-spec pattern_id(binary(), binary()) -> binary().
pattern_id(TaskSignature, Approach) ->
    PatternBin = <<TaskSignature/binary, ":", Approach/binary>>,
    crypto:hash(md5, PatternBin).

%% @private Generate embedding for task
-spec generate_embedding(binary(), agent_type()) -> embedding_vector().
generate_embedding(Task, AgentType) ->
    %% Simple embedding generation (in production, use actual embedding model)
    %% For now, create a fixed-size vector from hash
    Dim = 256,  % Default embedding dimension
    Hash = crypto:hash(sha256, <<AgentType/binary, ":", Task/binary>>),

    %% Expand hash to full embedding dimension
    <<HashBin:32/binary, _/binary>> = Hash,
    Expanded = expand_to_dim(HashBin, Dim),

    %% Normalize to unit vector
    normalize_vector(Expanded).

%% @private Expand binary to embedding dimension
-spec expand_to_dim(binary(), pos_integer()) -> binary(float()).
expand_to_dim(Bin, TargetDim) ->
    BinSize = byte_size(Bin),
    FloatsNeeded = TargetDim,

    %% Convert bytes to floats and repeat/trim to reach target dimension
    Floats = [binary_to_float(<<B:8, 0:24>>) || <<B>> <= Bin],
    Padded = lists:sublist(cycle_list(Floats), FloatsNeeded),
    list_to_binary([<<F:64/float-native>> || F <- Padded]).

%% @private Cycle a list infinitely
-spec cycle_list(list()) -> list().
cycle_list([]) -> [];
cycle_list(L) -> lists:append(L, cycle_list(L)).

%% @private Normalize vector to unit length
-spec normalize_vector(binary(float())) -> binary(float()).
normalize_vector(VectorBin) ->
    Floats = [F || <<F:64/float-native>> <= VectorBin],
    Norm = math:sqrt(lists:sum([F * F || F <- Floats])),
    Normalized = [F / Norm || F <- Floats],
    list_to_binary([<<F:64/float-native>> || F <- Normalized]).

%% @private Calculate confidence score for pattern
-spec calculate_confidence(#pattern{}) -> float().
calculate_confidence(Pattern) ->
    TotalCount = Pattern#pattern.success_count + Pattern#pattern.failure_count,
    case TotalCount of
        0 -> 0.0;
        _ ->
            SuccessRate = Pattern#pattern.success_count / TotalCount,
            %% Combine success rate with reward
            (SuccessRate + Pattern#pattern.avg_reward) / 2
    end.

%% @private Get error reason from outcome
-spec get_error_reason(outcome()) -> term() | undefined.
get_error_reason({error, Reason}) -> Reason;
get_error_reason(failure) -> operation_failed;
get_error_reason(_) -> undefined.

%% @private Update completion metrics
-spec update_completion_metrics(#trajectory{}, map()) -> map().
update_completion_metrics(Trajectory, Metrics) ->
    Metrics1 = increment_metric(trajectories_completed, Metrics),

    TotalReward = maps:get(total_reward, Metrics1, 0.0),
    Completed = maps:get(trajectories_completed, Metrics1, 1),
    NewTotalReward = TotalReward + Trajectory#trajectory.reward,
    NewAvgReward = NewTotalReward / Completed,

    Metrics1#{
        total_reward => NewTotalReward,
        avg_reward => NewAvgReward,
        last_update => erlang:system_time(millisecond)
    }.

%% @private Consolidate patterns
-spec consolidate_patterns(#learning_state{}) -> #learning_state{}.
consolidate_patterns(State) ->
    ?LOG_INFO("Consolidating learned patterns"),

    %% Apply EWC-style consolidation to prevent catastrophic forgetting
    Patterns = maps:values(State#learning_state.patterns),

    %% Promote high-performing patterns
    {Promoted, Demoted} = lists:partition(fun(P) ->
        P#pattern.avg_reward >= ?PROMOTION_THRESHOLD
    end, Patterns),

    %% Update consolidation state
    NewConsolState = (State#learning_state.consolidation_state)#consolidation_state{
        last_consolidation = erlang:system_time(millisecond),
        consolidated_patterns = [P#pattern.id || P <- Promoted]
    },

    NewMetrics = State#learning_state.metrics#{
        patterns_promoted => maps:get(patterns_promoted, State#learning_state.metrics, 0) + length(Promoted),
        patterns_demoted => maps:get(patterns_demoted, State#learning_state.metrics, 0) + length(Demoted),
        consolidations_performed => maps:get(consolidations_performed, State#learning_state.metrics, 0) + 1
    },

    State#learning_state{
        consolidation_state = NewConsolState,
        metrics = NewMetrics
    }.

%% @private Optimize policies based on learned patterns
-spec optimize_policies(#learning_state{}) -> #learning_state{}.
optimize_policies(State) ->
    ?LOG_DEBUG("Optimizing policies based on learned patterns"),

    %% Generate recommendations from current state
    Recommendations = generate_recommendations(State),

    NewEngine = State#learning_state.recommendation_engine#recommendation_engine{
        active_recommendations = Recommendations
    },

    NewMetrics = increment_metric(optimizations_applied, State#learning_state.metrics),

    State#learning_state{
        recommendation_engine = NewEngine,
        metrics = NewMetrics
    }.

%% @private Generate recommendations from learned patterns
-spec generate_recommendations(#learning_state{}) -> [#recommendation{}].
generate_recommendations(State) ->
    Patterns = maps:values(State#learning_state.patterns),
    Anomalies = erlmcp_anomaly_detector:get_recent_anomalies(
        State#learning_state.anomaly_detector, 10),

    %% Pattern-based recommendations
    PatternRecs = [create_pattern_recommendation(P) || P <- Patterns,
                      P#pattern.avg_reward >= ?PROMOTION_THRESHOLD],

    %% Anomaly-based recommendations
    AnomalyRecs = [create_anomaly_recommendation(A) || A <- Anomalies,
                      A#anomaly.severity =:= high orelse A#anomaly.severity =:= critical],

    %% Sort by priority and limit
    AllRecs = lists:keysort(#recommendation.priority, PatternRecs ++ AnomalyRecs),
    lists:sublist(AllRecs, 10).

%% @private Create recommendation from pattern
-spec create_pattern_recommendation(#pattern{}) -> #recommendation{}.
create_pattern_recommendation(Pattern) ->
    Priority = case Pattern#pattern.avg_reward of
        R when R >= 0.9 -> critical;
        R when R >= 0.8 -> high;
        R when R >= 0.7 -> medium;
        _ -> low
    end,

    #recommendation{
        id = ?GENERATE_PATTERN_ID,
        category = pattern_selection,
        priority = Priority,
        title = <<"High-performing pattern: ", Pattern#pattern.approach/binary>>,
        description = io_lib:format("Pattern ~s has ~.2f average reward across ~p uses",
                                   [Pattern#pattern.approach, Pattern#pattern.avg_reward,
                                    Pattern#pattern.success_count]),
        action = #{
            type => use_pattern,
            pattern_id => Pattern#pattern.id,
            agent_type => Pattern#pattern.agent_type
        },
        expected_outcome = <<"Improved success rate for similar tasks">>,
        confidence = calculate_confidence(Pattern),
        evidence = [Pattern#pattern.id],
        expires_at => erlang:system_time(millisecond) + 3600000  % 1 hour
    }.

%% @private Create recommendation from anomaly
-spec create_anomaly_recommendation(#anomaly{}) -> #recommendation{}.
create_anomaly_recommendation(Anomaly) ->
    #recommendation{
        id = ?GENERATE_PATTERN_ID,
        category = error_prevention,
        priority = Anomaly#anomaly.severity,
        title = <<"Anomaly detected: ", Anomaly#anomaly.metric_name/binary>>,
        description = io_lib:format("Metric ~s deviated by ~.2f sigma (~.2f from expected ~.2f)",
                                   [Anomaly#anomaly.metric_name, Anomaly#anomaly.sigma_score,
                                    Anomaly#anomaly.observed_value, Anomaly#anomaly.expected_value]),
        action = #{
            type => investigate_anomaly,
            metric => Anomaly#anomaly.metric_name,
            severity => Anomaly#anomaly.severity
        },
        expected_outcome = <<"Prevent potential failure by addressing anomaly">>,
        confidence = min(1.0, Anomaly#anomaly.sigma_score / 5.0),
        evidence = [Anomaly#anomaly.id],
        expires_at => erlang:system_time(millisecond) + 1800000  % 30 minutes
    }.

%% @private Persist state to disk
-spec persist_state(#learning_state{}) -> ok.
persist_state(_State) ->
    %% In production, persist to AgentDB or SQLite
    ok.

%% @private Export patterns to file
-spec export_patterns_to_file([#pattern{}], file:filename()) -> ok | {error, term()}.
export_patterns_to_file(Patterns, Filename) ->
    try
        JSON = lists:map(fun pattern_to_json/1, Patterns),
        file:write_file(Filename, jsx:encode(JSON)),
        ok
    catch
        Error:_ -> {error, Error}
    end.

%% @private Convert pattern to JSON map
-spec pattern_to_json(#pattern{}) -> map().
pattern_to_json(P) ->
    #{
        id => P#pattern.id,
        task_signature => P#pattern.task_signature,
        agent_type => P#pattern.agent_type,
        approach => P#pattern.approach,
        steps => P#pattern.steps,
        success_count => P#pattern.success_count,
        failure_count => P#pattern.failure_count,
        avg_reward => P#pattern.avg_reward,
        reward_variance => P#pattern.reward_variance,
        created_at => P#pattern.created_at,
        metadata => P#pattern.metadata
    }.

%% @private Import patterns from file
-spec import_patterns_from_file(file:filename()) -> {ok, [#pattern{}]} | {error, term()}.
import_patterns_from_file(Filename) ->
    try
        {ok, Content} = file:read_file(Filename),
        JSON = jsx:decode(Content, [return_maps]),
        Patterns = lists:map(fun json_to_pattern/1, JSON),
        {ok, Patterns}
    catch
        Error:_ -> {error, Error}
    end.

%% @private Convert JSON map to pattern
-spec json_to_pattern(map()) -> #pattern{}.
json_to_pattern(J) ->
    #pattern{
        id = maps:get(<<"id">>, J),
        task_signature = maps:get(<<"task_signature">>, J),
        agent_type = maps:get(<<"agent_type">>, J),
        approach = maps:get(<<"approach">>, J),
        steps = maps:get(<<"steps">>, J),
        success_count = maps:get(<<"success_count">>, J),
        failure_count = maps:get(<<"failure_count">>, J),
        avg_reward = maps:get(<<"avg_reward">>, J),
        reward_variance = maps:get(<<"reward_variance">>, J),
        created_at = maps:get(<<"created_at">>, J),
        embedding = generate_embedding(maps:get(<<"task_signature">>, J),
                                    maps:get(<<"agent_type">>, J)),
        metadata = maps:get(<<"metadata">>, J, #{})
    }.
