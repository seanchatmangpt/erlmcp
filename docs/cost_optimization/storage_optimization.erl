%%====================================================================
%% erlmcp Storage Optimization Implementation
%%====================================================================
%%
%% This module implements comprehensive storage optimization for erlmcp v3,
%% including tiered storage, lifecycle management, and compression.
%%

-module(erlmcp_storage_optimizer).

-behaviour(gen_server).

-export([start_link/0, configure_tiered_storage/1, implement_retention_policy/2,
         enable_compression/1, optimize_data_layout/0, analyze_storage_costs/0,
         get_storage_recommendations/0, archive_data/1, delete_expired_data/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records for storage configuration
-record(storage_tier,
        {name :: binary(),
         type :: hot | warm | cold | archive,
         storage_type :: ssd | hdd | object | glacier,
         compression :: boolean(),
         encryption :: boolean(),
         retention_days :: integer(),
         replication :: integer(),
         cost_per_gb :: float(),
         access_pattern :: frequent | occasional | rare}).

-record(storage_object,
        {id :: binary(),
         data :: binary(),
         metadata :: map(),
         created_at :: integer(),
         last_accessed :: integer(),
        size :: integer(),
        tier :: binary(),
        compression :: binary(),
        checksum :: binary()}).

-record(lifecycle_policy,
        {name :: binary(),
         conditions :: [map()],
         actions :: [map()],
          enabled :: boolean()}).

-record(retention_rule,
        {name :: binary(),
          rule_type :: time | size | age,
          condition :: map(),
          action :: archive | delete | tier,
          target_tier :: binary() | undefined,
          enabled :: boolean()}).

%% Records for storage analysis
-record(storage_analysis,
        {total_size :: integer(),
        total_cost :: float(),
        by_tier :: map(),
        by_category :: map(),
        growth_rate :: float(),
        savings_potential :: float(),
        recommendations :: [binary()]}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec configure_tiered_storage([#storage_tier{}]) -> ok.
configure_tiered_storage(Tiers) ->
    gen_server:cast(?MODULE, {configure_tiered_storage, Tiers}).

-spec implement_retention_policy(binary(), [map()]) -> ok.
implement_retention_policy(PolicyName, Rules) ->
    gen_server:call(?MODULE, {implement_retention_policy, PolicyName, Rules}, 10000).

-spec enable_compression(binary()) -> ok.
enable_compression(TierName) ->
    gen_server:cast(?MODULE, {enable_compression, TierName}).

-spec optimize_data_layout() -> ok.
optimize_data_layout() ->
    gen_server:cast(?MODULE, optimize_data_layout).

-spec analyze_storage_costs() -> #storage_analysis{}.
analyze_storage_costs() ->
    gen_server:call(?MODULE, analyze_storage_costs, 5000).

-spec get_storage_recommendations() -> [binary()].
get_storage_recommendations() ->
    gen_server:call(?MODULE, get_storage_recommendations, 5000).

-spec archive_data(binary()) -> ok | {error, term()}.
archive_data(ObjectId) ->
    gen_server:call(?MODULE, {archive_data, ObjectId}, 5000).

-spec delete_expired_data(binary()) -> integer().
delete_expired_data(PolicyName) ->
    gen_server:call(?MODULE, {delete_expired_data, PolicyName}, 10000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, map()}.
init([]) ->
    %% Initialize storage tiers
    StorageTiers = load_storage_tiers(),

    %% Initialize lifecycle policies
    LifecyclePolicies = load_lifecycle_policies();

    %% Initialize retention rules
    RetentionRules = load_retention_rules();

    %% Start storage monitoring
    schedule_storage_analysis();

    %% Start lifecycle execution
    schedule_lifecycle_execution();

    State = #{
        storage_tiers => StorageTiers,
        lifecycle_policies => LifecyclePolicies,
        retention_rules => RetentionRules,
        last_analysis => undefined,
        storage_cache => #{},
        analysis_interval => 86400000,  % 24 hours
        lifecycle_interval => 3600000   % 1 hour
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({implement_retention_policy, PolicyName, Rules}, _From, State) ->
    %% Create lifecycle policy
    Policy = create_lifecycle_policy(PolicyName, Rules),

    %% Validate policy
    case validate_lifecycle_policy(Policy) of
        valid ->
            %% Store policy
            NewPolicies = maps:put(PolicyName, Policy, State#lifecycle_policies),
            NewState = State#{
                lifecycle_policies => NewPolicies
            },

            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(analyze_storage_costs, _From, State) ->
    %% Generate storage analysis
    Analysis = generate_storage_analysis(State),

    {reply, Analysis, State};

handle_call(get_storage_recommendations, _From, State) ->
    %% Generate storage recommendations
    Recommendations = generate_storage_recommendations(State),

    {reply, Recommendations, State};

handle_call({archive_data, ObjectId}, _From, State) ->
    %% Archive data object
    case archive_data_object(ObjectId, State) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete_expired_data, PolicyName}, _From, State) ->
    %% Delete expired data
    DeletedCount = delete_expired_data_objects(PolicyName, State),

    {reply, DeletedCount, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast({configure_tiered_storage, Tiers}, State) ->
    %% Configure storage tiers
    ConfiguredTiers = lists:map(fun(Tier) ->
        configure_storage_tier(Tier)
    end, Tiers),

    %% Store configured tiers
    NewTiers = lists:foldl(fun(Tier, Acc) ->
        maps:put(Tier#storage_tier.name, Tier, Acc)
    end, #{}, ConfiguredTiers),

    NewState = State#{
        storage_tiers => NewTiers
    },

    {noreply, NewState};

handle_cast({enable_compression, TierName}, State) ->
    %% Enable compression for tier
    Tier = maps:get(TierName, State#storage_tiers, undefined),

    case Tier of
        undefined ->
            %% Tier not found
            {noreply, State};
        _ ->
            %% Enable compression
            UpdatedTier = Tier#storage_tier{compression = true},

            %% Update tier
            NewTiers = maps:put(TierName, UpdatedTier, State#storage_tiers),
            NewState = State#{
                storage_tiers => NewTiers
            },

            {noreply, NewState}
    end;

handle_cast(optimize_data_layout, State) ->
    %% Optimize data layout
    case optimize_data_distribution(State) of
        ok ->
            %% Log optimization success
            log_info("Data layout optimization completed successfully");
        {error, Reason} ->
            log_error("Data layout optimization failed: ~p", [Reason])
    end,

    %% Schedule next optimization
    schedule_data_optimization(),

    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(storage_analysis, State) ->
    %% Perform storage analysis
    Analysis = generate_storage_analysis(State),

    %% Store analysis
    store_storage_analysis(Analysis),

    %% Generate recommendations
    Recommendations = generate_storage_recommendations(State),

    %% Store recommendations
    store_storage_recommendations(Recommendations),

    %% Schedule next analysis
    schedule_storage_analysis(),

    {noreply, State};

handle_info(lifecycle_execution, State) ->
    %% Execute lifecycle policies
    execute_lifecycle_policies(State),

    %% Schedule next execution
    schedule_lifecycle_execution(),

    {noreply, State};

handle_info(data_optimization, State) ->
    %% Trigger data optimization cast
    ?MODULE:optimize_data_layout(),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    %% Clean up resources
    cleanup_storage_resources(),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Load storage tiers
load_storage_tiers() ->
    %% Load from configuration
    TiersConfig = get_config(storage_tiers, []),

    %% Convert to records
    lists:map(fun tier_to_record/1, TiersConfig).

%% Load lifecycle policies
load_lifecycle_policies() ->
    %% Query database
    Policies = erlmcp_database:find(#{
        collection => lifecycle_policies,
        filter => #{enabled => true}
    }),

    %% Convert to records
    lists:map(fun policy_to_record/1, Policies).

%% Load retention rules
load_retention_rules() ->
    %% Query database
    Rules = erlmcp_database:find(#{
        collection => retention_rules,
        filter => #{enabled => true}
    }),

    %% Convert to records
    lists:map(fun rule_to_record/1, Rules).

%% Configure storage tier
configure_storage_tier(Tier) ->
    %% Validate tier configuration
    case validate_storage_tier(Tier) of
        valid ->
            %% Store tier configuration
            store_tier_configuration(Tier),

            %% Initialize storage backend
            initialize_storage_backend(Tier),

            Tier;
        {error, Reason} ->
            {error, Reason}
    end.

%% Create lifecycle policy
create_lifecycle_policy(PolicyName, Rules) ->
    %% Create policy record
    Policy = #lifecycle_policy{
        name = PolicyName,
        conditions = lists:map(fun rule_to_condition/1, Rules),
        actions = lists:map(fun rule_to_action/1, Rules),
        enabled = true
    },

    %% Store policy
    erlmcp_database:insert(#{
        collection => lifecycle_policies,
        document => Policy
    }),

    Policy.

%% Validate lifecycle policy
validate_lifecycle_policy(Policy) ->
    %% Validate policy structure
    case Policy#lifecycle_policy.name of
        undefined ->
            {error, invalid_policy_name};
        _ ->
            %% Validate rules
            ValidRules = lists:all(fun validate_retention_rule/1,
                                  Policy#lifecycle_policy.conditions),
            case ValidRules of
                true ->
                    valid;
                false ->
                    {error, invalid_rules}
            end
    end.

%% Validate retention rule
validate_retention_rule(Rule) ->
    %% Check rule structure
    case maps:is_key(type, Rule) andalso maps:is_key(action, Rule) of
        true ->
            %% Validate rule type
            RuleType = maps:get(type, Rule),
            case RuleType of
                time ->
                    maps:is_key(days, Rule);
                size ->
                    maps:is_key(max_size, Rule);
                age ->
                    maps:is_key(older_than, Rule);
                _ ->
                    false
            end;
        false ->
            false
    end.

%% Generate storage analysis
generate_storage_analysis(State) ->
    %% Get storage metrics
    StorageMetrics = collect_storage_metrics(State),

    %% Calculate total size
    TotalSize = calculate_total_size(StorageMetrics);

    %% Calculate total cost
    TotalCost = calculate_total_cost(StorageMetrics, State);

    %% Group by tier
    ByTier = group_storage_by_tier(StorageMetrics, State);

    %% Group by category
    ByCategory = group_storage_by_category(StorageMetrics);

    %% Calculate growth rate
    GrowthRate = calculate_growth_rate(StorageMetrics);

    %% Calculate savings potential
    SavingsPotential = calculate_savings_potential(StorageMetrics, State);

    %% Generate recommendations
    Recommendations = generate_storage_recommendations(State);

    #storage_analysis{
        total_size = TotalSize,
        total_cost = TotalCost,
        by_tier = ByTier,
        by_category = ByCategory,
        growth_rate = GrowthRate,
        savings_potential = SavingsPotential,
        recommendations = Recommendations
    }.

%% Collect storage metrics
collect_storage_metrics(State) ->
    %% Get all storage objects
    Objects = get_all_storage_objects();

    %% Convert to records
    lists:map(fun object_to_record/1, Objects).

%% Get all storage objects
get_all_storage_objects() ->
    %% Query storage database
    Objects = erlmcp_database:find(#{
        collection => storage_objects,
        sort => #{created_at => -1},
        limit => 10000
    }),

    Objects.

%% Calculate total size
calculate_total_size(StorageMetrics) ->
    lists:sum([M#storage_object.size || M <- StorageMetrics]).

%% Calculate total cost
calculate_total_cost(StorageMetrics, State) ->
    %% Group objects by tier
    ObjectsByTier = lists:foldl(fun(Object, Acc) ->
        Tier = Object#storage_object.tier,
        maps:update(Tier, [Object | maps:get(Tier, Acc, [])], Acc)
    end, #{}, StorageMetrics);

    %% Calculate cost for each tier
    Costs = lists:map(fun({TierName, Objects}) ->
        Tier = maps:get(TierName, State#storage_tiers, undefined),
        if
            Tier =/= undefined ->
                TierCost = Tier#storage_tier.cost_per_gb,
                TotalGB = lists:sum([O#storage_object.size || O <- Objects]) / (1024 * 1024 * 1024),
                TotalGB * TierCost;
            true ->
                0.0
        end
    end, maps:to_list(ObjectsByTier));

    lists:sum(Costs).

%% Group storage by tier
group_storage_by_tier(StorageMetrics, State) ->
    Groups = lists:foldl(fun(Object, Acc) ->
        Tier = Object#storage_object.tier,
        TierInfo = maps:get(Tier, State#storage_tiers, undefined),
        if
            TierInfo =/= undefined ->
                TierType = TierInfo#storage_tier.type,
                maps:update(TierType, [Object | maps:get(TierType, Acc, [])], Acc);
            true ->
                Acc
        end
    end, #{}, StorageMetrics);

    %% Convert to size calculations
    lists:map(fun({TierType, Objects}) ->
        Size = lists:sum([O#storage_object.size || O <- Objects]),
        #{type => TierType, count => length(Objects), size => Size}
    end, maps:to_list(Groups)).

%% Group storage by category
group_storage_by_category(StorageMetrics) ->
    Groups = lists:foldl(fun(Object, Acc) ->
        Category = get_object_category(Object),
        maps:update(Category, [Object | maps:get(Category, Acc, [])], Acc)
    end, #{}, StorageMetrics);

    %% Convert to size calculations
    lists:map(fun({Category, Objects}) ->
        Size = lists:sum([O#storage_object.size || O <- Objects]),
        #{category => Category, count => length(Objects), size => Size}
    end, maps:to_list(Groups)).

%% Calculate growth rate
calculate_growth_rate(StorageMetrics) ->
    %% Group by month
    MonthlyData = group_by_month(StorageMetrics);

    %% Calculate monthly growth
    case length(MonthlyData) >= 2 of
        true ->
            Recent = lists:last(MonthlyData),
            Previous = lists:nth(length(MonthlyData) - 1, MonthlyData),
            if
                Previous > 0 ->
                    (Recent - Previous) / Previous;
                true ->
                    0.0
            end;
        false ->
            0.0
    end.

%% Calculate savings potential
calculate_savings_potential(StorageMetrics, State) ->
    %% Analyze cold data
    ColdData = lists:filter(fun(Object) ->
        TierInfo = maps:get(Object#storage_object.tier, State#storage_tiers, undefined),
        TierInfo =/= undefined andalso TierInfo#storage_tier.type =:= cold
    end, StorageMetrics);

    %% Calculate cost of moving to cheaper storage
    CurrentCost = calculate_total_cost(ColdData, State);

    %% Calculate potential savings with optimal tiering
    OptimalCost = calculate_optimal_cost(ColdData, State);

    CurrentCost - OptimalCost.

%% Generate storage recommendations
generate_storage_recommendations(State) ->
    Recommendations = [],

    %% Analyze cost by tier
    CostByTier = analyze_cost_by_tier(State),
    Recommendations = generate_tier_recommendations(CostByTier, Recommendations);

    %% Analyze access patterns
    AccessPatterns = analyze_access_patterns(State),
    Recommendations = generate_access_recommendations(AccessPatterns, Recommendations);

    %% Analyze compression opportunities
    CompressionOpportunities = analyze_compression_opportunities(State),
    Recommendations = generate_compression_recommendations(CompressionOpportunities, Recommendations);

    %% Analyze lifecycle opportunities
    LifecycleOpportunities = analyze_lifecycle_opportunities(State),
    Recommendations = generate_lifecycle_recommendations(LifecycleOpportunities, Recommendations);

    Recommendations.

%% Archive data object
archive_data_object(ObjectId, State) ->
    %% Get object
    Object = get_storage_object(ObjectId),
    if
        Object =:= undefined ->
            {error, object_not_found};
        true ->
            %% Determine target tier
            TargetTier = determine_archive_tier(Object, State),

            %% Move object to archive tier
            case move_object_to_tier(Object, TargetTier) of
                ok ->
                    %% Update object record
                    update_storage_object(ObjectId, #{tier => TargetTier}),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% Delete expired data objects
delete_expired_data_objects(PolicyName, State) ->
    %% Get lifecycle policy
    Policy = maps:get(PolicyName, State#lifecycle_policies, undefined),

    case Policy of
        undefined ->
            0;
        _ ->
            %% Get retention rules
            Rules = Policy#lifecycle_policy.rules,

            %% Find expired objects
            ExpiredObjects = find_expired_objects(Rules, State),

            %% Delete expired objects
            lists:foreach(fun(Object) ->
                delete_storage_object(Object#storage_object.id)
            end, ExpiredObjects),

            length(ExpiredObjects)
    end.

%% Execute lifecycle policies
execute_lifecycle_policies(State) ->
    %% Get active policies
    ActivePolicies = [P || P <- maps:values(State#lifecycle_policies),
                          P#lifecycle_policy.enabled =:= true];

    %% Execute each policy
    lists:foreach(fun execute_lifecycle_policy/1, ActivePolicies).

%% Execute single lifecycle policy
execute_lifecycle_policy(Policy) ->
    %% Get retention rules
    Rules = Policy#lifecycle_policy.rules,

    %% Execute each rule
    lists:foreach(fun execute_retention_rule/1, Rules).

%% Execute retention rule
execute_retention_rule(Rule) ->
    %% Find objects matching rule
    MatchingObjects = find_objects_matching_rule(Rule);

    %% Execute action
    case Rule#retention_rule.action of
        archive ->
            %% Archive objects
            lists:foreach(fun(Object) ->
                archive_data(Object#storage_object.id)
            end, MatchingObjects);
        delete ->
            %% Delete objects
            lists:foreach(fun(Object) ->
                delete_storage_object(Object#storage_object.id)
            end, MatchingObjects);
        tier ->
            %% Move to different tier
            TargetTier = Rule#retention_rule.target_tier,
            lists:foreach(fun(Object) ->
                move_object_to_tier(Object, TargetTier)
            end, MatchingObjects);
        _ ->
            ok
    end.

%% Find objects matching retention rule
find_objects_matching_rule(Rule) ->
    %% Build query
    Query = build_retention_query(Rule);

    %% Query database
    Objects = erlmcp_database:find(Query);

    Objects.

%% Build retention query
build_retention_query(Rule) ->
    Query = #{collection => storage_objects},

    case Rule#retention_rule.rule_type of
        time ->
            Days = Rule#retention_rule.condition#{days},
            Cutoff = os:system_time(millisecond) - (Days * 86400000),
            Query#{filter => #{created_at => #{'$lt' => Cutoff}}};
        size ->
            MaxSize = Rule#retention_rule.condition#{max_size},
            Query#{filter => #{size => #{'$lte' => MaxSize}}};
        age ->
            OlderThan = Rule#retention_rule.condition#{older_than},
            Cutoff = os:system_time(millisecond) - (OlderThan * 86400000),
            Query#{filter => #{last_accessed => #{'$lt' => Cutoff}}}
    end.

%% Storage optimization functions
optimize_data_distribution(State) ->
    %% Get storage distribution
    Distribution = analyze_storage_distribution(State);

    %% Identify imbalances
    Imbalances = identify_storage_imbalances(Distribution);

    %% Rebalance data
    rebalance_storage_data(Imbalances, State).

analyze_storage_distribution(State) ->
    %% Get all storage objects
    Objects = get_all_storage_objects();

    %% Group by various dimensions
    Groups = group_objects_by_dimensions(Objects);

    Groups.

identify_storage_imbalances(Distribution) ->
    %% Find imbalances
    Imbalances = [],

    %% Check for uneven distribution across tiers
    Imbalances = check_tier_distribution(Distribution, Imbalances);

    %% Check for uneven distribution across categories
    Imbalances = check_category_distribution(Distribution, Imbalances);

    %% Check for hot/cold imbalance
    Imbalances = check_hot_cold_distribution(Distribution, Imbalances);

    Imbalances.

rebalance_storage_data(Imbalances, State) ->
    %% Rebalance each imbalance
    lists:foreach(fun rebalance_imbalance/1, Imbalances).

rebalance_imbalance(Imbalance) ->
    case Imbalance of
        {tier_imbalance, TierName, Count} ->
            %% Move objects to better tier
            rebalance_tier(TierName, Count);
        {category_imbalance, CategoryName, Count} ->
            rebalance_category(CategoryName, Count);
        {hot_cold_imbalance, HotCount, ColdCount} ->
            rebalance_hot_cold(HotCount, ColdCount)
    end.

rebalance_tier(TierName, Count) ->
    %% Get objects in tier
    Objects = get_objects_in_tier(TierName, Count);

    %% Determine better tier
    BetterTier = determine_better_tier(TierName);

    %% Move objects
    lists:foreach(fun(Object) ->
        move_object_to_tier(Object, BetterTier)
    end, Objects).

rebalance_category(CategoryName, Count) ->
    %% Get objects in category
    Objects = get_objects_in_category(CategoryName, Count);

    %% Optimize category distribution
    optimize_category_distribution(Objects).

rebalance_hot_cold(HotCount, ColdCount) ->
    %% Balance hot/cold data
    OptimalRatio = 0.7,  % 70% hot, 30% cold
    CurrentRatio = HotCount / (HotCount + ColdCount),

    if
        CurrentRatio < OptimalRatio ->
            %% Move some data to hot tier
            MoveCount = round((OptimalRatio - CurrentRatio) * (HotCount + ColdCount)),
            move_to_hot_tier(MoveCount);
        CurrentRatio > OptimalRatio ->
            %% Move some data to cold tier
            MoveCount = round((CurrentRatio - OptimalRatio) * (HotCount + ColdCount)),
            move_to_cold_tier(MoveCount);
        true ->
            ok
    end.

move_to_hot_tier(Count) ->
    %% Get cold objects
    Objects = get_cold_objects(Count);

    %% Move to hot tier
    lists:foreach(fun(Object) ->
        move_object_to_tier(Object, hot)
    end, Objects).

move_to_cold_tier(Count) ->
    %% Get warm objects
    Objects = get_warm_objects(Count);

    %% Move to cold tier
    lists:foreach(fun(Object) ->
        move_object_to_tier(Object, cold)
    end, Objects).

%% Compression functions
enable_compression_for_tier(TierName) ->
    %% Get objects in tier
    Objects = get_objects_in_tier(TierName, 1000);  % Batch of 1000

    %% Compress objects
    lists:foreach(fun compress_object/1, Objects).

compress_object(Object) ->
    %% Compress data
    CompressedData = compress_data(Object#storage_object.data),

    %% Update object
    update_storage_object(Object#storage_object.id, #{
        data => CompressedData,
        compression => gzip
    }).

compress_data(Data) ->
    %% Use zlib for compression
    zlib:gzip(Data).

decompress_data(Data, Compression) ->
    case Compression of
        gzip ->
            zlib:gunzip(Data);
        _ ->
            Data
    end.

%% Cost analysis functions
analyze_cost_by_tier(State) ->
    %% Get storage metrics
    Metrics = collect_storage_metrics(State);

    Group by tier
    Grouped = lists:foldl(fun(Object, Acc) ->
        Tier = Object#storage_object.tier,
        Size = Object#storage_object.size,
        Current = maps:get(Tier, Acc, 0),
        maps:put(Tier, Current + Size, Acc)
    end, #{}, Metrics);

    %% Calculate costs
    lists:map(fun({Tier, Size}) ->
        TierInfo = maps:get(Tier, State#storage_tiers, undefined),
        if
            TierInfo =/= undefined ->
                Cost = (Size / (1024 * 1024 * 1024)) * TierInfo#storage_tier.cost_per_gb;
            true ->
                Cost = 0.0
        end,
        #{tier => Tier, size => Size, cost => Cost}
    end, maps:to_list(Grouped)).

generate_tier_recommendations(CostByTier, Recommendations) ->
    %% Find expensive tiers
    ExpensiveTiers = lists:filter(fun(Tier) ->
        Tier#tier.cost > 1000  % $1000 per month
    end, CostByTier);

    %% Generate recommendations
    lists:foldl(fun(Tier, Acc) ->
        Recommendation = generate_tier_recommendation(Tier),
        [Recommendation | Acc]
    end, Recommendations, ExpensiveTiers).

generate_tier_recommendation(Tier) ->
    TierName = Tier#tier.tier,
    Cost = Tier#tier.cost,

    io_lib:format("Optimize ~p tier storage saving ~p per month",
                 [TierName, Cost]).

analyze_access_patterns(State) ->
    %% Get access patterns
    AccessPatterns = get_access_patterns();

    %% Analyze patterns
    AnalyzedPatterns = analyze_patterns(AccessPatterns);

    AnalyzedPatterns.

generate_access_recommendations(AccessPatterns, Recommendations) ->
    %% Find inefficient access patterns
    Inefficient = lists:filter(fun(Pattern) ->
        Pattern#pattern.efficiency < 0.5
    end, AccessPatterns);

    %% Generate recommendations
    lists:foldl(fun(Pattern, Acc) ->
        Recommendation = generate_access_recommendation(Pattern),
        [Recommendation | Acc]
    end, Recommendations, Inefficient).

generate_access_recommendation(Pattern) ->
    io_lib:format("Reorganize ~p data to improve access efficiency",
                 [Pattern#pattern.category]).

analyze_compression_opportunities(State) ->
    %% Get uncompressed data
    Uncompressed = get_uncompressed_data();

    Calculate compression ratios
    Opportunities = lists:map(fun(Object) ->
        CompressionRatio = calculate_compression_ratio(Object),
        if
            CompressionRatio > 0.2 ->  % 20% savings potential
                #{object => Object, ratio => CompressionRatio};
            true ->
                undefined
        end
    end, Uncompressed);

    lists:filter(fun(O) -> O =/= undefined end, Opportunities).

generate_compression_recommendations(Opportunities, Recommendations) ->
    %% Sort by potential savings
    Sorted = lists:sort(fun(O1, O2) ->
        O1#opportunity.ratio > O2#opportunity.ratio
    end, Opportunities);

    %% Generate recommendations
    lists:foldl(fun(Opportunity, Acc) ->
        Recommendation = generate_compression_recommendation(Opportunity),
        [Recommendation | Acc]
    end, Recommendations, Sorted).

generate_compression_recommendation(Opportunity) ->
    io_lib:format("Compress ~p saving ~p%",
                 [Opportunity#opportunity.object#storage_object.id,
                  round(Opportunity#opportunity.ratio * 100)]).

analyze_lifecycle_opportunities(State) ->
    %% Get lifecycle policies
    Policies = maps:values(State#lifecycle_policies);

    %% Analyze each policy
    Opportunities = lists:map(fun analyze_lifecycle_policy/1, Policies);

    lists:flatten(Opportunities).

generate_lifecycle_recommendations(Opportunities, Recommendations) ->
    %% Generate recommendations
    lists:foldl(fun(Opportunity, Acc) ->
        Recommendation = generate_lifecycle_recommendation(Opportunity),
        [Recommendation | Acc]
    end, Recommendations, Opportunities).

generate_lifecycle_recommendation(Opportunity) ->
    io_lib:format("Implement retention policy for ~p",
                 [Opportunity#opportunity.name]).

%% Helper functions
get_object_category(Object) ->
    %% Extract category from metadata
    case maps:get(category, Object#storage_object.metadata, undefined) of
        undefined ->
            "uncategorized";
        Category ->
            Category
    end.

determine_archive_tier(Object, State) ->
    %% Determine optimal archive tier based on object characteristics
    Category = get_object_category(Object);
    Size = Object#storage_object.size;
    Age = os:system_time(millisecond) - Object#storage_object.created_at;

    case Category of
        "logs" when Age > 86400000000 ->  % 30 days
            "cold";
        "metrics" when Age > 259200000000 ->  % 30 days
            "cold";
        "traces" when Age > 604800000000 ->  % 7 days
            "cold";
        _ when Size > 1024 * 1024 * 1024 ->  % 1GB
            "cold";
        _ ->
            "warm"
    end.

move_object_to_tier(Object, TargetTier) ->
    %% Get target tier configuration
    TierConfig = get_tier_config(TargetTier);

    %% Move object to new tier
    NewObject = Object#storage_object{tier = TargetTier};

    %% Update storage
    update_storage_object(Object#storage_object.id, NewObject).

get_tier_config(TierName) ->
    %% Get tier configuration
    case application:get_env(erlmcp, storage_tiers) of
        {ok, Tiers} ->
            case lists:keyfind(TierName, 1, Tiers) of
                {TierName, Config} -> Config;
                false -> undefined
            end;
        undefined ->
            undefined
    end.

group_by_month(StorageMetrics) ->
    %% Group objects by month
    Monthly = lists:foldl(fun(Object, Acc) ->
        Month = Object#storage_object.created_at div 2592000000,  % 30 days
        Size = Object#storage_object.size,
        Current = maps:get(Month, Acc, 0),
        maps:put(Month, Current + Size, Acc)
    end, #{}, StorageMetrics);

    %% Convert to list
    lists:sort(fun({M1, _}, {M2, _}) -> M1 < M2 end, maps:to_list(Monthly)).

group_objects_by_dimensions(Objects) ->
    %% Group by various dimensions
    Groups = #{},

    %% Group by tier
    Groups = lists:foldl(fun(Object, Acc) ->
        Tier = Object#storage_object.tier,
        maps:update(Tier, [Object | maps:get(Tier, Acc, [])], Acc)
    end, Groups, Objects);

    %% Group by category
    Groups = lists:foldl(fun(Object, Acc) ->
        Category = get_object_category(Object),
        maps:update(Category, [Object | maps:get(Category, Acc, [])], Acc)
    end, Groups, Objects);

    %% Group by size
    Groups = lists:foldl(fun(Object, Acc) ->
        SizeBucket = get_size_bucket(Object#storage_object.size),
        maps:update(SizeBucket, [Object | maps:get(SizeBucket, Acc, [])], Acc)
    end, Groups, Objects);

    Groups.

get_size_bucket(Size) ->
    if
        Size < 1024 * 1024 -> "small";
        Size < 1024 * 1024 * 1024 -> "medium";
        Size < 1024 * 1024 * 1024 * 10 -> "large";
        true -> "huge"
    end.

check_tier_distribution(Distribution, Imbalances) ->
    %% Check if any tier has too much data
    TierCounts = maps:get(by_tier, Distribution, #{}),

    lists:foldl(fun({Tier, Objects}, Acc) ->
        Count = length(Objects),
        if
            Count > 10000 ->  % More than 10,000 objects
                [{tier_imbalance, Tier, Count} | Acc];
            true ->
                Acc
        end
    end, Imbalances, maps:to_list(TierCounts)).

check_category_distribution(Distribution, Imbalances) ->
    %% Check if any category has too much data
    CategoryCounts = maps:get(by_category, Distribution, #{}),

    lists:foldl(fun({Category, Objects}, Acc) ->
        Count = length(Objects),
        if
            Count > 50000 ->  % More than 50,000 objects
                [{category_imbalance, Category, Count} | Acc];
            true ->
                Acc
        end
    end, Imbalances, maps:to_list(CategoryCounts)).

check_hot_cold_distribution(Distribution, Imbalances) ->
    %% Check hot/cold ratio
    HotObjects = maps:get(hot, Distribution, []),
    ColdObjects = maps:get(cold, Distribution, []),

    HotCount = length(HotObjects),
    ColdCount = length(ColdObjects);

    Total = HotCount + ColdCount,
    if
        Total > 0 ->
            HotRatio = HotCount / Total,
            if
                HotRatio < 0.3 ->
                    [{hot_cold_imbalance, HotCount, ColdCount} | Imbalances];
                HotRatio > 0.8 ->
                    [{hot_cold_imbalance, HotCount, ColdCount} | Imbalances];
                true ->
                    Imbalances
            end;
        true ->
            Imbalances
    end.

determine_better_tier(CurrentTier) ->
    %% Determine better tier based on access patterns
    case CurrentTier of
        "hot" -> "warm";
        "warm" -> "cold";
        "cold" -> "archive";
        _ -> "warm"
    end.

get_objects_in_tier(TierName, Count) ->
    %% Get objects in tier
    Query = #{
        collection => storage_objects,
        filter => #{tier => TierName},
        sort => #{last_accessed => -1},
        limit => Count
    },

    Objects = erlmcp_database:find(Query),
    Objects.

get_objects_in_category(CategoryName, Count) ->
    %% Get objects in category
    Query = #{
        collection => storage_objects,
        filter => #{metadata => #{category => CategoryName}},
        sort => #{last_accessed => -1},
        limit => Count
    },

    Objects = erlmcp_database:find(Query),
    Objects.

get_cold_objects(Count) ->
    %% Get cold objects
    Query = #{
        collection => storage_objects,
        filter => #{tier => "cold"},
        sort => #{last_accessed => 1},  % Least recently accessed
        limit => Count
    },

    Objects = erlmcp_database:find(Query),
    Objects.

get_warm_objects(Count) ->
    %% Get warm objects
    Query = #{
        collection => storage_objects,
        filter => #{tier => "warm"},
        sort => #{last_accessed => 1},  % Least recently accessed
        limit => Count
    },

    Objects = erlmcp_database:find(Query),
    Objects.

calculate_compression_ratio(Object) ->
    %% Calculate compression ratio
    OriginalSize = Object#storage_object.size;
    CompressedSize = size(compress_data(Object#storage_object.data));

    1.0 - (CompressedSize / OriginalSize).

get_access_patterns() ->
    %% Get access pattern data
    Patterns = erlmcp_database:find(#{
        collection => access_patterns,
        sort => #{timestamp => -1},
        limit => 10000
    }),

    Patterns.

analyze_patterns(Patterns) ->
    %% Analyze access patterns
    Analyzed = [],

    %% Group by category
    ByCategory = lists:foldl(fun(Pattern, Acc) ->
        Category = maps:get(category, Pattern),
        maps:update(Category, [Pattern | maps:get(Category, Acc, [])], Acc)
    end, #{}, Patterns);

    %% Calculate efficiency for each category
    lists:map(fun({Category, CategoryPatterns}) ->
        Efficiency = calculate_access_efficiency(CategoryPatterns),
        #pattern{category => Category, efficiency => Efficiency}
    end, maps:to_list(ByCategory)).

calculate_access_efficiency(Patterns) ->
    %% Calculate access efficiency score
    TotalAccess = length(Patterns);
    UniqueAccess = length(lists:usort([maps:get(object_id, P) || P <- Patterns]));

    if
        TotalAccess > 0 ->
            UniqueAccess / TotalAccess;
        true ->
            0.0
    end.

get_uncompressed_data() ->
    %% Get uncompressed data objects
    Query = #{
        collection => storage_objects,
        filter => #{compression => undefined}
    },

    Objects = erlmcp_database:find(Query),
    Objects.

calculate_optimal_cost(StorageMetrics, State) ->
    %% Calculate cost with optimal tiering
    lists:sum([calculate_optimal_object_cost(Object, State) || Object <- StorageMetrics]).

calculate_optimal_object_cost(Object, State) ->
    %% Calculate optimal cost for object
    Size = Object#storage_object.size;
    AccessFrequency = get_access_frequency(Object);

    case AccessFrequency of
        high ->
            Tier = "hot";
        medium ->
            Tier = "warm";
        low ->
            Tier = "cold";
        _ ->
            Tier = "warm"
    end,

    TierInfo = maps:get(Tier, State#storage_tiers, undefined),
    if
        TierInfo =/= undefined ->
            (Size / (1024 * 1024 * 1024)) * TierInfo#storage_tier.cost_per_gb;
        true ->
            0.0
    end.

get_access_frequency(Object) ->
    %% Calculate access frequency
    RecentAccess = get_recent_access(Object#storage_object.id),
    if
        RecentAccess > 1000 -> high;
        RecentAccess > 100 -> medium;
        RecentAccess > 10 -> low;
        true -> very_low
    end.

get_recent_access(ObjectId) ->
    %% Get recent access count
    Query = #{
        collection => access_patterns,
        filter => #{object_id => ObjectId, timestamp => #{'$gt' => os:system_time(millisecond) - 864000000}}
    },

    Patterns = erlmcp_database:find(Query),
    length(Patterns).

%% Storage management functions
get_storage_object(ObjectId) ->
    %% Get storage object
    Query = #{
        collection => storage_objects,
        filter => #{id => ObjectId},
        limit => 1
    },

    case erlmcp_database:find(Query) of
        [Object] -> Object;
        _ -> undefined
    end.

update_storage_object(ObjectId, Updates) ->
    %% Update storage object
    erlmcp_database:update(#{
        collection => storage_objects,
        filter => #{id => ObjectId},
        updates => Updates
    }).

delete_storage_object(ObjectId) ->
    %% Delete storage object
    erlmcp_database:delete(#{
        collection => storage_objects,
        filter => #{id => ObjectId}
    }).

store_storage_analysis(Analysis) ->
    %% Store analysis in database
    erlmcp_database:insert(#{
        collection => storage_analysis,
        document => Analysis
    }).

store_storage_recommendations(Recommendations) ->
    %% Store recommendations in database
    erlmcp_database:insert(#{
        collection => storage_recommendations,
        document => #{recommendations => Recommendations}
    }).

schedule_storage_analysis() ->
    erlang:send_after(?MODULE, storage_analysis, get_env(analysis_interval, 86400000)).

schedule_lifecycle_execution() ->
    erlang:send_after(?MODULE, lifecycle_execution, get_env(lifecycle_interval, 3600000)).

schedule_data_optimization() ->
    erlang:send_after(?MODULE, data_optimization, get_env(optimization_interval, 604800000)).

%% Helper functions
get_config(Key, Default) ->
    case application:get_env(erlmcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

log_info(Format, Args) ->
    erlmcp_logger:info(Format, Args).

log_error(Format, Args) ->
    erlmcp_logger:error(Format, Args).

%% Cleanup functions
cleanup_storage_resources() ->
    %% Clean up temporary storage resources
    ok.

store_tier_configuration(Tier) ->
    %% Store tier configuration
    erlmcp_database:insert(#{
        collection => storage_tiers,
        document => Tier
    }).

initialize_storage_backend(Tier) ->
    %% Initialize storage backend
    case Tier#storage_tier.type of
        hot ->
            initialize_hot_storage(Tier);
        warm ->
            initialize_warm_storage(Tier);
        cold ->
            initialize_cold_storage(Tier);
        archive ->
            initialize_archive_storage(Tier)
    end.

initialize_hot_storage(Tier) ->
    %% Initialize hot storage (SSD)
    ok.

initialize_warm_storage(Tier) ->
    %% Initialize warm storage (HDD)
    ok.

initialize_cold_storage(Tier) ->
    %% Initialize cold storage (Object)
    ok.

initialize_archive_storage(Tier) ->
    %% Initialize archive storage (Glacier)
    ok.

object_to_record(Object) ->
    #storage_object{
        id = maps:get(id, Object),
        data = maps:get(data, Object),
        metadata = maps:get(metadata, Object),
        created_at = maps:get(created_at, Object),
        last_accessed = maps:get(last_accessed, Object),
        size = maps:get(size, Object),
        tier = maps:get(tier, Object),
        compression = maps:get(compression, Object),
        checksum = maps:get(checksum, Object)
    }.

tier_to_record(Tier) ->
    #storage_tier{
        name = maps:get(name, Tier),
        type = maps:get(type, Tier),
        storage_type = maps:get(storage_type, Tier),
        compression = maps:get(compression, Tier),
        encryption = maps:get(encryption, Tier),
        retention_days = maps:get(retention_days, Tier),
        replication = maps:get(replication, Tier),
        cost_per_gb = maps:get(cost_per_gb, Tier),
        access_pattern = maps:get(access_pattern, Tier)
    }.

policy_to_record(Policy) ->
    #lifecycle_policy{
        name = maps:get(name, Policy),
        conditions = maps:get(conditions, Policy, []),
        actions = maps:get(actions, Policy, []),
        enabled = maps:get(enabled, Policy, false)
    }.

rule_to_record(Rule) ->
    #retention_rule{
        name = maps:get(name, Rule),
        rule_type = maps:get(type, Rule),
        condition = maps:get(condition, Rule),
        action = maps:get(action, Rule),
        target_tier = maps:get(target_tier, Rule),
        enabled = maps:get(enabled, Rule, false)
    }.