%%%-------------------------------------------------------------------
%%% @doc
%%% Adaptive Learning System Records and Constants for erlmcp v3
%%%
%%% This header defines the data structures for the 4-step intelligence pipeline:
%%% RETRIEVE -> JUDGE -> DISTILL -> CONSOLIDATE
%%%
%%% Implements ReasoningBank adaptive learning patterns for MCP operations.
%%% @end
%%%-------------------------------------------------------------------

-ifndef(ERLMCP_LEARNING_HRL).
-define(ERLMCP_LEARNING_HRL, 1).

%%%====================================================================
%%% Learning Pipeline Constants
%%%====================================================================

%% HNSW Index Configuration
-define(HNSW_M_DEFAULT, 16).           % Connection count per layer
-define(HNSW_EF_CONSTRUCTION, 200).    % Index build parameter
-define(HNSW_EF_SEARCH, 50).           % Search parameter
-define(HNSW_MAX_ELEMENTS, 1000000).   % Maximum patterns in index

%% Pattern Storage
-define(PATTERN_TTL_SECONDS, 86400).   % 24 hours default TTL
-define(PATTERN_MIN_REWARD, 0.5).      % Minimum reward to store
-define(PROMOTION_THRESHOLD, 0.8).     % Reward threshold for promotion
-define(DEMOTION_THRESHOLD, 0.3).      % Reward threshold for demotion

%% Experience Replay
-define(REPLAY_BUFFER_SIZE, 10000).    % Max experiences in buffer
-define(REPLAY_BATCH_SIZE, 32).        % Experiences per training batch
-define(REPLAY_SAMPLING_STRATEGY, prioritized). % uniform | prioritized

%% Anomaly Detection
-define(ANOMALY_WINDOW_SIZE, 100).     % Observations for baseline
-define(ANOMALY_SIGMA_THRESHOLD, 3.0). % Standard deviations for anomaly
-define(ANOMALY_MIN_OBSERVATIONS, 30). % Minimum observations before detection

%% Adaptive Optimization
-define(OPTIMIZATION_INTERVAL_MS, 60000). % Check every minute
-define(OPTIMIZATION_MIN_SAMPLES, 100).   % Minimum samples for optimization
-define(OPTIMIZATION_LEARNING_RATE, 0.01). % Default learning rate

%% Consolidation
-define(CONSOLIDATION_INTERVAL_MS, 300000). % Every 5 minutes
-define(EWC_LAMBDA, 1000).             % Elastic weight consolidation lambda
-define(CONSOLIDATION_BATCH_SIZE, 128).

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type pattern_id() :: binary().
-type trajectory_id() :: binary().
-type session_id() :: binary().
-type embedding_vector() :: binary<float>().  % Float32 binary format
-type reward_value() :: float() | number().
-type verdict() :: success | failure | partial.
-type outcome() :: verdict() | {verdict(), reward_value()}.
-type operation_name() :: binary().
-type agent_type() :: binary() | atom().
-type feature_map() :: #{binary() => number()}.

-type embedding_dim() :: 128 | 256 | 384 | 512 | 768.
-type distance_metric() :: cosine | euclidean | dot_product.
-type sampling_strategy() :: uniform | prioritized | diversity | recency.

%%%====================================================================
%%% Trajectory Tracking Records
%%%====================================================================

%% Trajectory represents a complete operational session
-record(trajectory,
        {id :: trajectory_id(),
         session_id :: session_id(),
         agent_type :: agent_type(),
         task_description :: binary() | undefined,
         start_time :: integer(),           % microseconds since epoch
         end_time :: integer() | undefined, % microseconds since epoch
         steps = [] :: [trajectory_step()], % Ordered sequence of steps
         verdict :: verdict() | undefined,
         reward :: reward_value() | undefined,
         metadata :: map(),                  % Arbitrary metadata
         embedding :: embedding_vector() | undefined}).

%% Individual step within a trajectory
-record(trajectory_step,
        {step_id :: binary(),
         operation :: operation_name(),
         timestamp :: integer(),
         outcome :: success | failure,
         duration_ms :: non_neg_integer(),
         metrics :: feature_map(),
         error_reason :: term() | undefined,
         metadata :: map()}).

%% Trajectory verdict with confidence
-record(verdict,
        {outcome :: verdict(),
         confidence :: float(),           % 0.0 - 1.0
         reason :: binary() | undefined,
         reward :: reward_value(),
         metadata :: map()}).

%%%====================================================================
%%% Pattern Records
%%%====================================================================

%% Learned pattern extracted from trajectories
-record(pattern,
        {id :: pattern_id(),
         task_signature :: binary(),       % Normalized task description
         agent_type :: agent_type(),
         approach :: binary(),             % Strategy/pattern name
         steps :: [binary()],              % Abstract step sequence
         success_count = 0 :: non_neg_integer(),
         failure_count = 0 :: non_neg_integer(),
         avg_reward = 0.0 :: float(),
         reward_variance = 0.0 :: float(),
         last_used :: integer() | undefined,
         created_at :: integer(),
         embedding :: embedding_vector(),
         metadata :: map()}).

%% Pattern match result from HNSW search
-record(pattern_match,
        {pattern_id :: pattern_id(),
         similarity :: float(),            % 0.0 - 1.0
         reward :: reward_value(),
         confidence :: float(),
         pattern :: #pattern{} | undefined}).

%%%====================================================================
%%% HNSW Index Records
%%%====================================================================

%% HNSW layer configuration
-record(hnsw_layer,
        {level :: non_neg_integer(),
         m :: pos_integer(),              % Connections per node
         max_elements :: pos_integer(),
         element_count = 0 :: non_neg_integer(),
         data :: map()}).                  % node_id -> neighbor_ids

%% HNSW index structure
-record(hnsw_index,
        {m :: pos_integer(),
         ef_construction :: pos_integer(),
         ef_search :: pos_integer(),
         dim :: embedding_dim(),
         distance_metric :: distance_metric(),
         entry_point :: binary() | undefined,
         layers :: [#hnsw_layer{}],
         element_count = 0 :: non_neg_integer(),
         max_elements :: pos_integer()}).

%% HNSW search context
-record(hnsw_search,
        {query :: embedding_vector(),
         ef :: pos_integer(),
         candidates :: [{binary(), float()}], % {id, distance}
         visited :: sets:set(binary()),
         results :: [{pattern_id(), float()}]}).

%%%====================================================================
%%% Experience Replay Records
%%%====================================================================

%% Single experience for replay
-record(experience,
        {id :: binary(),
         trajectory_id :: trajectory_id(),
         state :: feature_map(),           % Input state
         action :: binary(),               % Action taken
         reward :: reward_value(),         % Reward received
         next_state :: feature_map(),      % Resulting state
         done :: boolean(),                % Episode complete?
         priority :: float(),              % For prioritized replay
         timestamp :: integer()}).

%% Experience replay buffer
-record(replay_buffer,
        {capacity :: pos_integer(),
         experiences = [] :: [#experience{}],
         priorities :: #{binary() => float()},
         sampling_distribution :: uniform | prioritized,
         total_samples = 0 :: non_neg_integer()}).

%% Replay batch for training
-record(replay_batch,
        {states :: [feature_map()],
         actions :: [binary()],
         rewards :: [reward_value()],
         next_states :: [feature_map()],
         dones :: [boolean()],
         indices :: [binary()]}).          % Experience IDs for updates

%%%====================================================================
%%% Anomaly Detection Records
%%%====================================================================

%% Statistical baseline for anomaly detection
-record(statistical_baseline,
        {metric_name :: binary(),
         mean :: float(),
         variance :: float(),
         std_dev :: float(),
         min :: float(),
         max :: float(),
         sample_count :: non_neg_integer(),
         last_updated :: integer()}).

%% Anomaly detection result
-record(anomaly,
        {id :: binary(),
        severity :: low | medium | high | critical,
        metric_name :: binary(),
        observed_value :: float(),
        expected_value :: float(),
        deviation :: float(),
        sigma_score :: float(),
        timestamp :: integer(),
        context :: map()}).

%% Anomaly detector state
-record(anomaly_detector,
        {baselines :: #{binary() => #statistical_baseline{}},
         window_size :: pos_integer(),
         sigma_threshold :: float(),
         min_observations :: pos_integer(),
         alert_handlers :: [pid()]})  % PIDs to notify of anomalies

%%%====================================================================
%%% Adaptive Optimization Records
%%%====================================================================

%% Optimization policy
-record(optimization_policy,
        {name :: binary(),
         parameters :: map(),
         performance_score :: float(),
         last_updated :: integer(),
         update_count :: non_neg_integer()}).

%% Optimization recommendation
-record(optimization_recommendation,
        {id :: binary(),
         type :: parameter_tuning | resource_allocation | strategy_change,
         target :: binary(),               % Component or system
         current_value :: term(),
         recommended_value :: term(),
         expected_improvement :: float(),   % 0.0 - 1.0
         confidence :: float(),            % 0.0 - 1.0
         reason :: binary(),
         estimated_impact :: map()}).

%% Optimization state
-record(optimization_state,
        {policies :: #{binary() => #optimization_policy{}},
         current_performance :: float(),
         baseline_performance :: float(),
         optimization_interval :: pos_integer(),
         learning_rate :: float(),
         convergence_threshold :: float()}).

%%%====================================================================
%%% Consolidation Records (EWC - Elastic Weight Consolidation)
%%%====================================================================

%% Fisher information matrix entry
-record(fisher_info,
        {parameter_id :: binary(),
         value :: float(),
         importance :: float()}).

%% Consolidated model checkpoint
-record(model_checkpoint,
        {id :: binary(),
         parameters :: #{binary() => float()},
         fisher_info :: [#fisher_info{}],
         performance :: float(),
         task_count :: non_neg_integer(),
         created_at :: integer()}).

%% Consolidation state
-record(consolidation_state,
        {current_checkpoint :: #model_checkpoint{},
         ewc_lambda :: float(),
         consolidation_interval :: pos_integer(),
         last_consolidation :: integer(),
         consolidated_patterns :: [pattern_id()]}).

%%%====================================================================
%%% Recommendation Engine Records
%%%====================================================================

%% Recommendation result
-record(recommendation,
        {id :: binary(),
         category :: pattern_selection | parameter_optimization | error_prevention,
         priority :: low | medium | high | critical,
         title :: binary(),
         description :: binary(),
         action :: binary() | map(),
         expected_outcome :: binary(),
         confidence :: float(),
         evidence :: [binary()],           % Supporting trajectory/pattern IDs
         expires_at :: integer() | undefined}).

%% Recommendation engine state
-record(recommendation_engine,
        {active_recommendations :: [#recommendation{}],
         pattern_index :: pid(),           % Pid of HNSW index process
         anomaly_detector :: pid(),
         optimizer :: pid(),
         max_recommendations = 10 :: pos_integer()}).

%%%====================================================================
%%% Learning System State
%%%====================================================================

%% Main learning system state
-record(learning_state,
        {trajectories :: #{trajectory_id() => #trajectory{}},
         patterns :: #{pattern_id() => #pattern{}},
         hnsw_index :: #hnsw_index{},
         replay_buffer :: #replay_buffer{},
         anomaly_detector :: #anomaly_detector{},
         optimization_state :: #optimization_state{},
         consolidation_state :: #consolidation_state{},
         recommendation_engine :: #recommendation_engine{},
         metrics :: map()}).

%%%====================================================================
%%% Event Records
%%%====================================================================

%% Learning events for telemetry
-record(learning_event,
        {type :: trajectory_start | trajectory_end | pattern_learned |
                 anomaly_detected | optimization_applied | consolidation_complete,
         timestamp :: integer(),
         data :: map()}).

%% Pattern search query
-record(pattern_query,
        {task_signature :: binary() | undefined,
         agent_type :: agent_type() | undefined,
         embedding :: embedding_vector() | undefined,
         features :: feature_map() | undefined,
         k = 10 :: pos_integer(),
         min_reward = 0.0 :: float(),
         filters :: map()}).

%%%====================================================================
%%% Exported Types
%%%====================================================================

-export_type([
    pattern_id/0,
    trajectory_id/0,
    session_id/0,
    embedding_vector/0,
    reward_value/0,
    verdict/0,
    outcome/0,
    operation_name/0,
    agent_type/0,
    feature_map/0,
    embedding_dim/0,
    distance_metric/0,
    sampling_strategy/0
]).

%%%====================================================================
%%% Helper Macros
%%%====================================================================

%% Generate unique pattern ID
-define(GENERATE_PATTERN_ID,
        base64:encode(crypto:strong_rand_bytes(12))).

%% Generate unique trajectory ID
-define(GENERATE_TRAJECTORY_ID,
        base64:encode(crypto:strong_rand_bytes(16))).

%% Reward normalization macros
-define(REWARD_TO_BINARY(R),
        float_to_binary(R, [{decimals, 4}, compact])).

-define(BINARY_TO_REWARD(B),
        binary_to_float(B)).

%% Embedding dimension validation
-define(VALIDATE_EMBEDDING_DIM(D),
        ((D =:= 128) orelse (D =:= 256) orelse (D =:= 384) orelse
         (D =:= 512) orelse (D =:= 768))).

%% Similarity threshold macros
-define(SIMILARITY_HIGH, 0.9).
-define(SIMILARITY_MEDIUM, 0.7).
-define(SIMILARITY_LOW, 0.5).

%% Learning phase markers
-define(PHASE_RETRIEVE, retrieve).
-define(PHASE_JUDGE, judge).
-define(PHASE_DISTILL, distill).
-define(PHASE_CONSOLIDATE, consolidate).

-endif.
