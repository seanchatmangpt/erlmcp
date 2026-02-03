%%====================================================================
%% erlmcp License Management and Optimization Implementation
%%====================================================================
%%
%% This module implements comprehensive license management for erlmcp v3,
 including allocation, optimization, and compliance tracking.
%%

-module(erlmcp_license_manager).

-behaviour(gen_server).

-export([start_link/0, create_license_pool/2, allocate_license/2,
         release_license/2, get_license_usage/0, optimize_license_allocation/0,
         generate_compliance_report/0, track_license_costs/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records for license management
-record(license_pool,
        {id :: binary(),
         name :: binary(),
         product :: binary(),
         version :: binary(),
         total_licenses :: integer(),
         used_licenses :: integer(),
         available_licenses :: integer(),
         license_type :: commercial | open_source | custom,
         cost_per_license :: float(),
         auto_renewal :: boolean(),
         expiration_date :: integer() | undefined,
         allocation_strategy :: static | dynamic | priority,
         constraints :: [map()],
          metadata :: map()}).

-record(license_allocation,
        {id :: binary(),
         pool_id :: binary(),
         licensee :: binary(),
         allocated_count :: integer(),
         start_time :: integer(),
         end_time :: integer(),
        status :: active | expired | suspended,
        usage_metrics :: [map()]}).

-record(license_usage,
        {allocation_id :: binary(),
         timestamp :: integer(),
         service :: binary(),
         requests :: integer(),
        successful_requests :: integer(),
        failed_requests :: integer(),
        cost :: float()}).

-record(compliance_rule,
        {id :: binary(),
         name :: binary(),
         condition :: map(),
        action :: alert | revoke | throttle,
        severity :: critical | high | medium | low,
        enabled :: boolean()}).

-record(license_cost,
        {pool_id :: binary(),
         period :: daily | weekly | monthly | quarterly,
         total_cost :: float(),
         cost_per_license :: float(),
         cost_savings :: float(),
         utilization_rate :: float(),
        cost_efficiency :: float()}).

%% Records for license analysis
-record(license_analysis,
        {total_licenses :: integer(),
         total_cost :: float(),
        utilization_rate :: float(),
        cost_per_active_user :: float(),
        savings_opportunities :: [map()],
        compliance_status :: compliant | non_compliant | warning,
        recommendations :: [binary()]}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec create_license_pool(binary(), map()) -> ok | {error, term()}.
create_license_pool(PoolId, Config) ->
    gen_server:call(?MODULE, {create_license_pool, PoolId, Config}, 10000).

-spec allocate_license(binary(), map()) -> {ok, binary()} | {error, term()}.
allocate_license(PoolId, AllocationRequest) ->
    gen_server:call(?MODULE, {allocate_license, PoolId, AllocationRequest}, 5000).

-spec release_license(binary(), binary()) -> ok | {error, term()}.
release_license(PoolId, AllocationId) ->
    gen_server:call(?MODULE, {release_license, PoolId, AllocationId}, 5000).

-spec get_license_usage() -> map().
get_license_usage() ->
    gen_server:call(?MODULE, get_license_usage, 5000).

-spec optimize_license_allocation() -> ok.
optimize_license_allocation() ->
    gen_server:cast(?MODULE, optimize_license_allocation).

-spec generate_compliance_report() -> map().
generate_compliance_report() ->
    gen_server:call(?MODULE, generate_compliance_report, 10000).

-spec track_license_costs(binary(), float()) -> ok.
track_license_costs(PoolId, Cost) ->
    gen_server:cast(?MODULE, {track_license_costs, PoolId, Cost}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, map()}.
init([]) ->
    %% Initialize license pools
    LicensePools = load_license_pools();

    %% Initialize license allocations
    LicenseAllocations = load_license_allocations();

    %% Initialize compliance rules
    ComplianceRules = load_compliance_rules();

    %% Start license monitoring
    schedule_license_monitoring();

    %% Start optimization
    schedule_license_optimization();

    %% Start cost tracking
    schedule_cost_tracking();

    State = #{
        license_pools => LicensePools,
        license_allocations => LicenseAllocations,
        compliance_rules => ComplianceRules,
        license_usage => [],
        license_costs => [],
        last_optimization => undefined,
        monitoring_interval => 30000,  % 30 seconds
        optimization_interval => 300000,  % 5 minutes
        cost_tracking_interval => 3600000  % 1 hour
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, map()) -> {reply, term(), map()}.
handle_call({create_license_pool, PoolId, Config}, _From, State) ->
    %% Create license pool
    Pool = create_license_pool_record(PoolId, Config);

    %% Validate pool
    case validate_license_pool(Pool) of
        valid ->
            %% Store pool
            NewPools = maps:put(PoolId, Pool, State#license_pools),
            NewState = State#{
                license_pools => NewPools
            },

            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({allocate_license, PoolId, AllocationRequest}, _From, State) ->
    %% Get license pool
    Pool = maps:get(PoolId, State#license_pools, undefined);

    case Pool of
        undefined ->
            {reply, {error, pool_not_found}, State};
        _ ->
            %% Check availability
            Available = Pool#license_pool.available_licenses,
            Requested = maps:get(count, AllocationRequest, 1),

            case Available >= Requested of
                true ->
                    %% Allocate license
                    AllocationId = generate_allocation_id(),
                    Allocation = create_license_allocation(
                        PoolId, AllocationId, AllocationRequest
                    ),

                    %% Update pool
                    UpdatedPool = Pool#license_pool{
                        used_licenses = Pool#license_pool.used_licenses + Requested,
                        available_licenses = Available - Requested
                    },

                    NewPools = maps:put(PoolId, UpdatedPool, State#license_pools),
                    NewAllocations = maps:put(AllocationId, Allocation, State#license_allocations),
                    NewState = State#{
                        license_pools => NewPools,
                        license_allocations => NewAllocations
                    },

                    {reply, {ok, AllocationId}, NewState};
                false ->
                    {reply, {error, insufficient_licenses}, State}
            end
    end;

handle_call({release_license, PoolId, AllocationId}, _From, State) ->
    %% Get allocation
    Allocation = maps:get(AllocationId, State#license_allocations, undefined);

    case Allocation of
        undefined ->
            {reply, {error, allocation_not_found}, State};
        _ ->
            %% Check if allocation belongs to pool
            case Allocation#license_allocation.pool_id =:= PoolId of
                true ->
                    %% Release license
                    ReleasedCount = Allocation#license_allocation.allocated_count;

                    %% Get pool
                    Pool = maps:get(PoolId, State#license_pools, undefined);

                    %% Update pool
                    UpdatedPool = Pool#license_pool{
                        used_licenses = Pool#license_pool.used_licenses - ReleasedCount,
                        available_licenses = Pool#license_pool.available_licenses + ReleasedCount
                    },

                    %% Update allocation
                    UpdatedAllocation = Allocation#license_allocation{
                        status => expired,
                        end_time => os:system_time(millisecond)
                    };

                    NewPools = maps:put(PoolId, UpdatedPool, State#license_pools);
                    NewAllocations = maps:put(AllocationId, UpdatedAllocation, State#license_allocations),
                    NewState = State#{
                        license_pools => NewPools,
                        license_allocations => NewAllocations
                    },

                    {reply, ok, NewState};
                false ->
                    {reply, {error, pool_mismatch}, State}
            end
    end;

handle_call(get_license_usage, _From, State) ->
    %% Generate license usage report
    UsageReport = generate_license_usage_report(State),

    {reply, UsageReport, State};

handle_call(generate_compliance_report, _From, State) ->
    %% Generate compliance report
    ComplianceReport = generate_compliance_report(State),

    {reply, ComplianceReport, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(optimize_license_allocation, State) ->
    %% Optimize license allocation
    case optimize_license_allocations(State) of
        ok ->
            %% Log optimization success
            log_info("License allocation optimization completed successfully");
        {error, Reason} ->
            log_error("License allocation optimization failed: ~p", [Reason])
    end,

    %% Schedule next optimization
    schedule_license_optimization(),

    {noreply, State};

handle_cast({track_license_costs, PoolId, Cost}, State) ->
    %% Track license costs
    CostRecord = create_license_cost_record(PoolId, Cost);

    %% Store cost
    NewCosts = [CostRecord | State#license_costs],
    NewState = State#{
        license_costs => NewCosts
    },

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(license_monitoring, State) ->
    %% Monitor license usage
    monitor_license_usage(State);

    %% Check compliance
    check_compliance(State);

    %% Schedule next monitoring
    schedule_license_monitoring(),

    {noreply, State};

handle_info(license_optimization, State) ->
    %% Trigger optimization cast
    ?MODULE:optimize_license_allocation(),

    {noreply, State};

handle_info(cost_tracking, State) ->
    %% Calculate license costs
    CostAnalysis = calculate_license_costs(State);

    Store cost analysis
    store_cost_analysis(CostAnalysis);

    Schedule next cost tracking
    schedule_cost_tracking(),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    %% Clean up license resources
    cleanup_license_resources(),
    ok.

-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Load license pools
load_license_pools() ->
    %% Query license pools
    Pools = erlmcp_database:find(#{
        collection => license_pools,
        filter => #{active => true}
    });

    %% Convert to records
    lists:map(fun pool_to_record/1, Pools).

%% Load license allocations
load_license_allocations() ->
    %% Query license allocations
    Allocations = erlmcp_database:find(#{
        collection => license_allocations,
        filter => #{status => active}
    });

    %% Convert to records
    lists:map(fun allocation_to_record/1, Allocations).

%% Load compliance rules
load_compliance_rules() ->
    %% Query compliance rules
    Rules = erlmcp_database:find(#{
        collection => compliance_rules,
        filter => #{enabled => true}
    });

    %% Convert to records
    lists:map(fun rule_to_record/1, Rules).

%% Create license pool record
create_license_pool_record(PoolId, Config) ->
    TotalLicenses = maps:get(total_licenses, Config),
    CostPerLicense = maps:get(cost_per_license, Config, 0.0),

    Pool = #license_pool{
        id = PoolId,
        name = maps:get(name, Config, PoolId),
        product = maps:get(product, Config),
        version = maps:get(version, Config, "latest"),
        total_licenses = TotalLicenses,
        used_licenses = 0,
        available_licenses = TotalLicenses,
        license_type = maps.get(license_type, Config, commercial),
        cost_per_license = CostPerLicense,
        auto_renewal = maps.get(auto_renewal, Config, false),
        expiration_date = maps.get(expiration_date, Config),
        allocation_strategy = maps.get(allocation_strategy, Config, static),
        constraints = maps.get(constraints, Config, []),
        metadata = maps.get(metadata, Config, #{})
    },

    %% Store in database
    erlmcp_database:insert(#{
        collection => license_pools,
        document => Pool
    }),

    Pool.

%% Create license allocation record
create_license_allocation(PoolId, AllocationId, Request) ->
    Licensee = maps:get(licensee, Request),
    Count = maps:get(count, Request, 1),
    Duration = maps.get(duration_hours, Request, 24),

    Allocation = #license_allocation{
        id = AllocationId,
        pool_id = PoolId,
        licensee = Licensee,
        allocated_count = Count,
        start_time = os:system_time(millisecond),
        end_time = os:system_time(millisecond) + (Duration * 3600000),
        status = active,
        usage_metrics = []
    },

    %% Store in database
    erlmcp_database:insert(#{
        collection => license_allocations,
        document => Allocation
    }),

    Allocation.

%% Create license cost record
create_license_cost_record(PoolId, Cost) ->
    Period = determine_cost_period();

    CostRecord = #license_cost{
        pool_id = PoolId,
        period = Period,
        total_cost = Cost,
        cost_per_license = calculate_cost_per_license(PoolId),
        cost_savings = calculate_cost_savings(PoolId),
        utilization_rate = calculate_utilization_rate(PoolId),
        cost_efficiency = calculate_cost_efficiency(PoolId)
    },

    CostRecord.

%% Validate license pool
validate_license_pool(Pool) ->
    %% Validate pool configuration
    case Pool#license_pool.name of
        undefined ->
            {error, invalid_name};
        _ ->
            case Pool#license_pool.total_licenses > 0 of
                true ->
                    case Pool#license_pool.cost_per_license >= 0 of
                        true ->
                            valid;
                        false ->
                            {error, invalid_cost}
                    end;
                false ->
                    {error, invalid_licenses}
            end
    end.

%% Generate license usage report
generate_license_usage_report(State) ->
    %% Get active allocations
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Group by pool
    ByPool = lists:foldl(fun(Allocation, Acc) ->
        PoolId = Allocation#license_allocation.pool_id,
        maps:update(PoolId, [Allocation | maps:get(PoolId, Acc, [])], Acc)
    end, #{}, ActiveAllocations);

    Calculate usage metrics
    PoolUsage = lists:map(fun({PoolId, Allocations}) ->
        Pool = maps:get(PoolId, State#license_pools, undefined),
        case Pool of
            undefined ->
                undefined;
            _ ->
                TotalAllocated = lists:sum([A#license_allocation.allocated_count || A <- Allocations]),
                Utilization = TotalAllocated / Pool#license_pool.total_licenses,

                #pool_usage{
                    pool_id = PoolId,
                    pool_name = Pool#license_pool.name,
                    total_licenses = Pool#license_pool.total_licenses,
                    allocated_licenses = TotalAllocated,
                    utilization_rate = Utilization,
                    cost = TotalAllocated * Pool#license_pool.cost_per_license
                }
        end
    end, maps:to_list(ByPool));

    Filter out undefined
    ValidUsage = lists:filter(fun(U) -> U =/= undefined end, PoolUsage);

    Generate usage report
    #{
        total_pools => length(ValidUsage),
        total_licenses => lists:sum([U#pool_usage.total_licenses || U <- ValidUsage]),
        total_cost => lists:sum([U#pool_usage.cost || U <- ValidUsage]),
        utilization_rate => calculate_overall_utilization(ValidUsage),
        pools => ValidUsage
    }.

%% Generate compliance report
generate_compliance_report(State) ->
    %% Get compliance rules
    Rules = maps:values(State#compliance_rules);

    Check each rule
    ComplianceResults = lists:map(fun check_compliance_rule/1, Rules);

    Determine overall compliance
    OverallCompliance = determine_overall_compliance(ComplianceResults);

    Calculate compliance score
    ComplianceScore = calculate_compliance_score(ComplianceResults);

    Generate recommendations
    Recommendations = generate_compliance_recommendations(State);

    #{
        overall_status => OverallCompliance,
        compliance_score => ComplianceScore,
        total_rules => length(Rules),
        passed_rules => length([R || R <- ComplianceResults, R#compliance_result.passed =:= true]),
        failed_rules => length([R || R <- ComplianceResults, R#compliance_result.passed =:= false]),
        results => ComplianceResults,
        recommendations => Recommendations
    }.

%% Optimize license allocation
optimize_license_allocations(State) ->
    %% Analyze current allocation
    AllocationAnalysis = analyze_license_allocation(State);

    Identify optimization opportunities
    OptimizationOpportunities = identify_optimization_opportunities(AllocationAnalysis);

    Apply optimizations
    case apply_optimizations(OptimizationOpportunities, State) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% Monitor license usage
monitor_license_usage(State) ->
    %% Get active allocations
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Monitor each allocation
    lists:foreach(fun monitor_allocation/1, ActiveAllocations);

    Check for expiring allocations
    check_expiring_allocations(State).

monitor_allocation(Allocation) ->
    %% Get usage metrics
    UsageMetrics = get_usage_metrics(Allocation#license_allocation.id);

    Update usage metrics
    UpdatedAllocation = Allocation#license_allocation{
        usage_metrics = UsageMetrics
    };

    Check usage thresholds
    check_usage_thresholds(UpdatedAllocation);

    Store updated allocation
    erlmcp_database:update(#{
        collection => license_allocations,
        filter => #{id => Allocation#license_allocation.id},
        updates => #{usage_metrics => UsageMetrics}
    }).

check_usage_thresholds(Allocation) ->
    %% Check if usage exceeds thresholds
    case get_usage_thresholds(Allocation#license_allocation.pool_id) of
        undefined ->
            ok;
        Thresholds ->
            check_thresholds(Allocation, Thresholds)
    end.

check_thresholds(Allocation, Thresholds) ->
    %% Check various thresholds
    Usage = calculate_usage(Allocation);

    case maps:is_key(requests, Thresholds) andalso
         Usage#usage.requests > Thresholds#thresholds.requests of
        true ->
            trigger_threshold_alert(Allocation, requests, Usage#usage.requests);
        false ->
            ok
    end,

    case maps:is_key(success_rate, Thresholds) andalso
         Usage#usage.success_rate < Thresholds#thresholds.success_rate of
        true ->
            trigger_threshold_alert(Allocation, success_rate, Usage#usage.success_rate);
        false ->
            ok
    end,

    case maps:is_key(cost, Thresholds) andalso
         Usage#usage.cost > Thresholds#thresholds.cost of
        true ->
            trigger_threshold_alert(Allocation, cost, Usage#usage.cost);
        false ->
            ok
    end.

check_compliance(State) ->
    %% Get compliance rules
    Rules = maps:values(State#compliance_rules);

    Execute each rule
    lists:foreach(fun execute_compliance_rule/1, Rules).

execute_compliance_rule(Rule) ->
    %% Check rule condition
    case evaluate_compliance_condition(Rule) of
        true ->
            %% Execute rule action
            execute_compliance_action(Rule);
        false ->
            ok
    end.

evaluate_compliance_condition(Rule) ->
    %% Evaluate compliance condition
    Condition = Rule#compliance_rule.condition;

    case Condition of
        #{type := license_expiring} ->
            DaysUntilExpiration = maps:get(days_until_expiration, Condition);
            DaysUntilExpiration < 30;
        #{type := high_usage} ->
            Usage = get_license_usage_stats(Condition#{pool_id});
            Usage > 0.9;
        #{type := cost_threshold} ->
            Cost = get_license_cost_stats(Condition#{pool_id});
            Cost > maps:get(threshold, Condition);
        _ ->
            false
    end.

execute_compliance_action(Rule) ->
    %% Execute compliance action
    case Rule#compliance_rule.action of
        alert ->
            send_compliance_alert(Rule);
        revoke ->
            revoke_compliant_licenses(Rule);
        throttle ->
            throttle_license_usage(Rule);
        _ ->
            ok
    end.

send_compliance_alert(Rule) ->
    %% Send compliance alert
    Alert = #{
        type => compliance_violation,
        severity => Rule#compliance_rule.severity,
        message => io_lib:format("Compliance rule ~p triggered", [Rule#compliance_rule.name]),
        rule => Rule
    },

    erlmcp_alert_manager:send_alert(Alert).

revoke_compliant_licenses(Rule) ->
    %% Revoke non-compliant licenses
    Condition = Rule#compliance_rule.condition;

    case maps:is_key(pool_id, Condition) of
        true ->
            PoolId = maps:get(pool_id, Condition);
            Allocations = get_allocations_for_pool(PoolId);

            %% Revoke allocations that don't meet condition
            lists:foreach(fun(Allocation) ->
                revoke_license(Allocation)
            end, Allocations);
        false ->
            ok
    end.

throttle_license_usage(Rule) ->
    %% Throttle license usage
    Condition = Rule#compliance_rule.condition;

    case maps:is_key(pool_id, Condition) of
        true ->
            PoolId = maps:get(pool_id, Condition);
            Limit = maps:get(limit, Condition);

            %% Set usage limit
            set_usage_limit(PoolId, Limit);
        false ->
            ok
    end.

%% License cost tracking
track_license_costs(PoolId, Cost) ->
    %% Track license cost
    CostRecord = #license_cost{
        pool_id = PoolId,
        period = daily,
        total_cost = Cost,
        cost_per_license = calculate_cost_per_license(PoolId),
        cost_savings = calculate_cost_savings(PoolId),
        utilization_rate = calculate_utilization_rate(PoolId),
        cost_efficiency = calculate_cost_efficiency(PoolId)
    };

    %% Store cost record
    erlmcp_database:insert(#{
        collection => license_costs,
        document => CostRecord
    }),

    %% Update cost metrics
    update_cost_metrics(PoolId, Cost).

calculate_license_costs(State) ->
    %% Calculate license costs
    CostRecords = State#license_costs;

    Group by pool
    ByPool = lists:foldl(fun(Cost, Acc) ->
        PoolId = Cost#license_cost.pool_id,
        maps:update(PoolId, [Cost | maps:get(PoolId, Acc, [])], Acc)
    end, #{}, CostRecords);

    Calculate costs for each pool
    PoolCosts = lists:map(fun({PoolId, Costs}) ->
        TotalCost = lists:sum([C#license_cost.total_cost || C <- Costs]),
        AverageCost = TotalCost / length(Costs),

        #pool_cost{
            pool_id = PoolId,
            total_cost = TotalCost,
            average_cost = AverageCost,
            cost_trend = calculate_cost_trend(Costs)
        }
    end, maps:to_list(ByPool));

    Generate cost analysis
    #{
        total_cost => lists:sum([PC#pool_cost.total_cost || PC <- PoolCosts]),
        average_cost_per_pool => lists:sum([PC#pool_cost.average_cost || PC <- PoolCosts]) / length(PoolCosts),
        pools => PoolCosts
    }.

%% Analysis functions
analyze_license_allocation(State) ->
    %% Analyze current license allocation
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Calculate allocation metrics
    TotalAllocated = lists:sum([A#license_allocation.allocated_count || A <- ActiveAllocations]);
    UniqueLicensees = lists:usort([A#license_allocation.licensee || A <- ActiveAllocations]);

    AverageAllocation = if
        length(ActiveAllocations) > 0 ->
            TotalAllocated / length(ActiveAllocations);
        true ->
            0
    end;

    #allocation_analysis{
        total_allocations => length(ActiveAllocations),
        total_allocated => TotalAllocated,
        unique_licensees => length(UniqueLicensees),
        average_allocation => AverageAllocation,
        allocation_distribution => calculate_allocation_distribution(ActiveAllocations)
    }.

identify_optimization_opportunities(AllocationAnalysis) ->
    %% Identify optimization opportunities
    Opportunities = [];

    Check for over-allocated licenses
    Opportunities = check_over_allocation(AllocationAnalysis, Opportunities);

    Check for under-allocated licenses
    Opportunities = check_under_allocation(AllocationAnalysis, Opportunities);

    Check for idle licenses
    Opportunities = check_idle_licenses(AllocationAnalysis, Opportunities);

    Check for cost optimization
    Opportunities = check_cost_optimization(AllocationAnalysis, Opportunities);

    Opportunities.

check_over_allocation(Analysis, Opportunities) ->
    %% Check for over-allocated licenses
    if
        Analysis#allocation_analysis.average_allocation > 10 ->
            Opportunity = #optimization_opportunity{
                type => over_allocation,
                description => "Reduce average allocation from " ++
                             float_to_list(Analysis#allocation_analysis.average_allocation) ++
                             " to 5 licenses per user",
                estimated_savings => calculate_allocation_savings(Analysis, 5)
            },
            [Opportunity | Opportunities];
        true ->
            Opportunities
    end.

check_under_allocation(Analysis, Opportunities) ->
    %% Check for under-allocated licenses
    if
        Analysis#allocation_analysis.average_allocation < 3 ->
            Opportunity = #optimization_opportunity{
                type => under_allocation,
                description => "Increase average allocation from " ++
                             float_to_list(Analysis#allocation_analysis.average_allocation) ++
                             " to 5 licenses per user",
                estimated_cost => calculate_allocation_cost(Analysis, 5)
            },
            [Opportunity | Opportunities];
        true ->
            Opportunities
    end.

check_idle_licenses(Analysis, Opportunities) ->
    %% Check for idle licenses
    IdleAllocations = find_idle_allocations();

    case IdleAllocations of
        [] ->
            Opportunities;
        _ ->
            Opportunity = #optimization_opportunity{
                type => idle_licenses,
                description => "Reclaim " ++ integer_to_list(length(IdleAllocations)) ++
                             " idle licenses",
                estimated_savings => calculate_idle_savings(IdleAllocations)
            },
            [Opportunity | Opportunities]
    end.

check_cost_optimization(Analysis, Opportunities) ->
    %% Check for cost optimization opportunities
    CostAnalysis = calculate_cost_efficiency(Analysis);

    case CostAnalysis#cost_efficiency.efficiency < 0.8 of
        true ->
            Opportunity = #optimization_opportunity{
                type => cost_optimization,
                description => "Optimize license allocation to improve cost efficiency from " ++
                             float_to_list(CostAnalysis#cost_efficiency.efficiency) ++
                             " to 0.9",
                estimated_savings => CostAnalysis#cost_efficiency.potential_savings
            },
            [Opportunity | Opportunities];
        false ->
            Opportunities
    end.

apply_optimizations(Opportunities, State) ->
    %% Apply optimizations
    case Opportunities of
        [] ->
            ok;
        _ ->
            Sort opportunities by priority
            SortedOpportunities = lists:sort(fun(O1, O2) ->
                O1#optimization_opriority.estimated_savings > O2#optimization_opportunity.estimated_savings
            end, Opportunities);

            Apply top opportunities
            apply_top_opportunities(SortedOpportunities, State)
    end.

apply_top_opportunities(Opportunities, State) ->
    %% Apply top optimization opportunities
    case Opportunities of
        [] ->
            ok;
        [Opportunity | Rest] ->
            case apply_optimization(Opportunity, State) of
                ok ->
                    %% Apply remaining opportunities
                    apply_optimizations(Rest, State);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

apply_optimization(Opportunity, State) ->
    %% Apply individual optimization
    case Opportunity#optimization_opportunity.type of
        over_allocation ->
            apply_over_allocation_optimization(Opportunity, State);
        under_allocation ->
            apply_under_allocation_optimization(Opportunity, State);
        idle_licenses ->
            apply_idle_license_optimization(Opportunity, State);
        cost_optimization ->
            apply_cost_optimization(Opportunity, State);
        _ ->
            {error, unknown_optimization}
    end.

apply_over_allocation_optimization(Opportunity, State) ->
    %% Apply over-allocation optimization
    TargetAllocation = Opportunity#optimization_opportunity.target_allocation;

    Get active allocations
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Reduce allocations
    lists:foreach(fun(Allocation) ->
        CurrentCount = Allocation#license_allocation.allocated_count;
        NewCount = max(1, trunc(CurrentCount * 0.5));

        case NewCount < CurrentCount of
            true ->
                reduce_allocation(Allocation, NewCount);
            false ->
                ok
        end
    end, ActiveAllocations),

    ok.

apply_under_allocation_optimization(Opportunity, State) ->
    %% Apply under-allocation optimization
    TargetAllocation = Opportunity#optimization_opportunity.target_allocation;

    Get active allocations
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Increase allocations
    lists:foreach(fun(Allocation) ->
        CurrentCount = Allocation#license_allocation.allocated_count;
        NewCount = min(10, CurrentCount + 2);

        case NewCount > CurrentCount of
            true ->
                increase_allocation(Allocation, NewCount);
            false ->
                ok
        end
    end, ActiveAllocations),

    ok.

apply_idle_license_optimization(Opportunity, State) ->
    %% Apply idle license optimization
    IdleAllocations = find_idle_allocations();

    Reclaim idle licenses
    lists:foreach(fun(Allocation) ->
        reclaim_license(Allocation)
    end, IdleAllocations),

    ok.

apply_cost_optimization(Opportunity, State) ->
    %% Apply cost optimization
    Optimize license allocation
    OptimalAllocation = calculate_optimal_allocation(State);

    Apply optimal allocation
    apply_optimal_allocation(OptimalAllocation, State),

    ok.

%% Helper functions
generate_allocation_id() ->
    erlmcp_utils:generate_id().

determine_cost_period() ->
    %% Determine cost period based on current date
    Now = os:system_time(millisecond),
    case Now div 2592000000 of  % 30 days
        0 -> daily;
        1 -> weekly;
        _ -> monthly
    end.

calculate_cost_per_license(PoolId) ->
    %% Calculate cost per license
    case get_license_pool(PoolId) of
        undefined ->
            0.0;
        Pool ->
            Pool#license_pool.cost_per_license
    end.

calculate_cost_savings(PoolId) ->
    %% Calculate cost savings
    case get_license_pool(PoolId) of
        undefined ->
            0.0;
        Pool ->
            Savings = Pool#license_pool.total_licenses * Pool#license_pool.cost_per_license * 0.2;
            Savings
    end.

calculate_utilization_rate(PoolId) ->
    %% Calculate utilization rate
    case get_license_pool(PoolId) of
        undefined ->
            0.0;
        Pool ->
            if
                Pool#license_pool.total_licenses > 0 ->
                    Pool#license_pool.used_licenses / Pool#license_pool.total_licenses;
                true ->
                    0.0
            end
    end.

calculate_cost_efficiency(PoolId) ->
    %% Calculate cost efficiency
    Utilization = calculate_utilization_rate(PoolId);
    CostPerLicense = calculate_cost_per_license(PoolId);

    Efficiency = case Utilization of
        U when U > 0.8 -> 0.9;
        U when U > 0.5 -> 0.7;
        U when U > 0.3 -> 0.5;
        _ -> 0.3
    end,

    Efficiency.

get_license_pool(PoolId) ->
    %% Get license pool
    case erlmcp_database:find(#{
        collection => license_pools,
        filter => #{id => PoolId}
    }) of
        [Pool] -> Pool;
        _ -> undefined
    end.

get_usage_metrics(AllocationId) ->
    %% Get usage metrics for allocation
    case erlmcp_database:find(#{
        collection => license_usage,
        filter => #{allocation_id => AllocationId},
        sort => #{timestamp => -1},
        limit => 1000
    }) of
        Metrics -> Metrics;
        _ -> []
    end.

calculate_usage(Allocation) ->
    %% Calculate usage from metrics
    Metrics = Allocation#license_allocation.usage_metrics;

    TotalRequests = lists:sum([M#license_usage.requests || M <- Metrics]);
    SuccessfulRequests = lists:sum([M#license_usage.successful_requests || M <- Metrics]);
    FailedRequests = lists:sum([M#license_usage.failed_requests || M <- Metrics]);
    TotalCost = lists:sum([M#license_usage.cost || M <- Metrics]);

    SuccessRate = if
        TotalRequests > 0 ->
            SuccessfulRequests / TotalRequests;
        true ->
            0.0
    end;

    #usage{
        total_requests = TotalRequests,
        successful_requests = SuccessfulRequests,
        failed_requests = FailedRequests,
        success_rate = SuccessRate,
        total_cost = TotalCost
    }.

get_usage_thresholds(PoolId) ->
    %% Get usage thresholds for pool
    case erlmcp_database:find(#{
        collection => usage_thresholds,
        filter => #{pool_id => PoolId}
    }) of
        [Thresholds] -> Thresholds;
        _ -> undefined
    end.

trigger_threshold_alert(Allocation, ThresholdType, Value) ->
    %% Trigger threshold alert
    Alert = #{
        type => license_threshold,
        severity => medium,
        message => io_lib:format("License allocation ~p exceeded ~p threshold: ~p",
                               [Allocation#license_allocation.id, ThresholdType, Value]),
        allocation_id => Allocation#license_allocation.id,
        threshold_type => ThresholdType,
        value => Value
    };

    erlmcp_alert_manager:send_alert(Alert).

get_allocations_for_pool(PoolId) ->
    %% Get allocations for pool
    case erlmcp_database:find(#{
        collection => license_allocations,
        filter => #{pool_id => PoolId, status => active}
    }) of
        Allocations -> Allocations;
        _ -> []
    end.

revoke_license(Allocation) ->
    %% Revoke license allocation
    erlmcp_database:update(#{
        collection => license_allocations,
        filter => #{id => Allocation#license_allocation.id},
        updates => #{status => expired,
                    end_time => os:system_time(millisecond)}
    }),

    %% Update pool
    PoolId = Allocation#license_allocation.pool_id;
    Pool = get_license_pool(PoolId);

    UpdatedPool = Pool#license_pool{
        used_licenses = Pool#license_pool.used_licenses - Allocation#license_allocation.allocated_count,
        available_licenses = Pool#license_pool.available_licenses + Allocation#license_allocation.allocated_count
    };

    erlmcp_database:update(#{
        collection => license_pools,
        filter => #{id => PoolId},
        updates => #{used_licenses => UpdatedPool#license_pool.used_licenses,
                    available_licenses => UpdatedPool#license_pool.available_licenses}
    }).

set_usage_limit(PoolId, Limit) ->
    %% Set usage limit for pool
    erlmcp_database:insert(#{
        collection => usage_limits,
        document => #{pool_id => PoolId, limit => Limit, timestamp => os:system_time(millisecond)}
    }).

check_expiring_allocations(State) ->
    %% Check for expiring allocations
    Now = os:system_time(millisecond);
    ExpirationThreshold = Now + 86400000;  % 24 hours

    Allocations = maps:values(State#license_allocations);

    Expiring = lists:filter(fun(Allocation) ->
        Allocation#license_allocation.end_time =< ExpirationThreshold
    end, Allocations);

    Send expiration notifications
    lists:foreach(fun send_expiration_notification/1, Expiring).

send_expiration_notification(Allocation) ->
    %% Send expiration notification
    Notification = #{
        type => license_expiration,
        severity => medium,
        message => io_lib:format("License allocation ~p expiring soon",
                               [Allocation#license_allocation.id]),
        allocation_id => Allocation#allocation_id,
        expiration_time => Allocation#allocation.end_time
    };

    erlmcp_notification_manager:send_notification(Notification).

schedule_license_monitoring() ->
    erlang:send_after(?MODULE, license_monitoring, get_env(monitoring_interval, 30000)).

schedule_license_optimization() ->
    erlang:send_after(?MODULE, license_optimization, get_env(optimization_interval, 300000)).

schedule_cost_tracking() ->
    erlang:send_after(?MODULE, cost_tracking, get_env(cost_tracking_interval, 3600000)).

get_env(Key, Default) ->
    case application:get_env(erlmcp, Key) of
        {ok, Value} -> Value;
        undefined -> Default
    end.

log_info(Format, Args) ->
    erlmcp_logger:info(Format, Args).

log_error(Format, Args) ->
    erlmcp_logger:error(Format, Args).

cleanup_license_resources() ->
    %% Clean up license resources
    erlmcp_database:delete(#{
        collection => license_pools,
        filter => #{active => false}
    }),

    erlmcp_database:delete(#{
        collection => license_allocations,
        filter => #{status => expired}
    }),

    erlmcp_database:delete(#{
        collection => usage_metrics,
        filter => #{timestamp => #{'$lt' => os:system_time(millisecond) - 2592000000}}  % 30 days
    }).

store_cost_analysis(Analysis) ->
    %% Store cost analysis
    erlmcp_database:insert(#{
        collection => license_cost_analysis,
        document => Analysis
    }).

calculate_overall_utilization(Usage) ->
    if
        length(Usage) > 0 ->
            lists:sum([U#pool_usage.utilization_rate || U <- Usage]) / length(Usage);
        true ->
            0.0
    end.

check_compliance_rule(Rule) ->
    %% Check compliance rule
    Passed = evaluate_compliance_condition(Rule);

    #compliance_result{
        rule_id => Rule#compliance_rule.id,
        rule_name => Rule#compliance_rule.name,
        passed => Passed,
        severity => Rule#compliance_rule.severity,
        timestamp => os:system_time(millisecond)
    }.

determine_overall_compliance(Results) ->
    %% Determine overall compliance status
    FailedRules = [R || R <- Results, R#compliance_result.passed =:= false];

    case FailedRules of
        [] -> compliant;
        _ when length(FailedRules) > 5 -> non_compliant;
        _ -> warning
    end.

calculate_compliance_score(Results) ->
    %% Calculate compliance score
    TotalRules = length(Results);
    PassedRules = length([R || R <- Results, R#compliance_result.passed =:= true]);

    if
        TotalRules > 0 ->
            PassedRules / TotalRules;
        true ->
            0.0
    end.

generate_compliance_recommendations(State) ->
    %% Generate compliance recommendations
    Recommendations = [];

    Check license usage
    Recommendations = check_license_usage_recommendations(State, Recommendations);

    Check license costs
    Recommendations = check_license_cost_recommendations(State, Recommendations);

    Check compliance rules
    Recommendations = check_compliance_rule_recommendations(State, Recommendations);

    Recommendations.

check_license_usage_recommendations(State, Recommendations) ->
    %% Check for license usage recommendations
    UsageReport = generate_license_usage_report(State);

    case UsageReport#utilization_rate > 0.9 of
        true ->
            Recommendation = "Consider increasing license pool size",
            [Recommendation | Recommendations];
        false when UsageReport#utilization_rate < 0.3 ->
            Recommendation = "Consider reducing license pool size",
            [Recommendation | Recommendations];
        true ->
            Recommendations
    end.

check_license_cost_recommendations(State, Recommendations) ->
    %% Check for license cost recommendations
    CostAnalysis = calculate_license_costs(State);

    case CostAnalysis#cost_efficiency.efficiency < 0.7 of
        true ->
            Recommendation = "Optimize license allocation to reduce costs",
            [Recommendation | Recommendations];
        true ->
            Recommendations
    end.

check_compliance_rule_recommendations(State, Recommendations) ->
    %% Check for compliance rule recommendations
    ComplianceReport = generate_compliance_report(State);

    case ComplianceReport#overall_status of
        non_compliant ->
            Recommendation = "Review and fix compliance issues",
            [Recommendation | Recommendations];
        warning ->
            Recommendation = "Monitor compliance issues",
            [Recommendation | Recommendations];
        _ ->
            Recommendations
    end.

reduce_allocation(Allocation, NewCount) ->
    %% Reduce allocation count
    erlmcp_database:update(#{
        collection => license_allocations,
        filter => #{id => Allocation#license_allocation.id},
        updates => #{allocated_count => NewCount}
    }),

    Return any reclaimed licenses to pool
    ReclaimedCount = Allocation#license_allocation.allocated_count - NewCount;
    reclaim_licenses_to_pool(Allocation#license_allocation.pool_id, ReclaimedCount).

increase_allocation(Allocation, NewCount) ->
    %% Increase allocation count
    erlmcp_database:update(#{
        collection => license_allocations,
        filter => #{id => Allocation#license_allocation.id},
        updates => #{allocated_count => NewCount}
    }),

    Allocate additional licenses from pool
    AdditionalCount = NewCount - Allocation#license_allocation.allocated_count;
    allocate_licenses_from_pool(Allocation#license_allocation.pool_id, AdditionalCount).

reclaim_licenses_to_pool(PoolId, Count) ->
    %% Reclaim licenses to pool
    Pool = get_license_pool(PoolId);

    UpdatedPool = Pool#license_pool{
        used_licenses = Pool#license_pool.used_licenses - Count,
        available_licenses = Pool#license_pool.available_licenses + Count
    };

    erlmcp_database:update(#{
        collection => license_pools,
        filter => #{id => PoolId},
        updates => #{used_licenses => UpdatedPool#license_pool.used_licenses,
                    available_licenses => UpdatedPool#license_pool.available_licenses}
    }).

allocate_licenses_from_pool(PoolId, Count) ->
    %% Allocate licenses from pool
    Pool = get_license_pool(PoolId);

    case Pool#license_pool.available_licenses >= Count of
        true ->
            UpdatedPool = Pool#license_pool{
                used_licenses = Pool#license_pool.used_licenses + Count,
                available_licenses = Pool#license_pool.available_licenses - Count
            };

            erlmcp_database:update(#{
                collection => license_pools,
                filter => #{id => PoolId},
                updates => #{used_licenses => UpdatedPool#license_pool.used_licenses,
                            available_licenses => UpdatedPool#license_pool.available_licenses}
            });
        false ->
            %% Request additional licenses
            request_additional_licenses(PoolId, Count)
    end.

request_additional_licenses(PoolId, Count) ->
    %% Request additional licenses
    Request = #{
        pool_id => PoolId,
        additional_count => Count,
        timestamp => os:system_time(millisecond),
        status => pending
    };

    erlmcp_database:insert(#{
        collection => license_requests,
        document => Request
    }),

    log_info("Requested ~p additional licenses for pool ~p", [Count, PoolId]).

calculate_allocation_distribution(Allocations) ->
    %% Calculate allocation distribution
    Distribution = lists:foldl(fun(Allocation, Acc) ->
        Count = Allocation#license_allocation.allocated_count;
        maps:update(Count, maps:get(Count, Acc, 0) + 1, Acc)
    end, #{}, Allocations);

    Distribution.

find_idle_allocations() ->
    %% Find idle license allocations
    IdleThreshold = os:system_time(millisecond) - 604800000;  % 7 days

    Allocations = erlmcp_database:find(#{
        collection => license_allocations,
        filter => #{status => active}
    });

    IdleAllocations = lists:filter(fun(Allocation) ->
        LastUsage = find_last_usage(Allocation#license_allocation.id),
        LastUsage < IdleThreshold
    end, Allocations);

    IdleAllocations.

find_last_usage(AllocationId) ->
    %% Find last usage time for allocation
    case erlmcp_database:find(#{
        collection => license_usage,
        filter => #{allocation_id => AllocationId},
        sort => #{timestamp => -1},
        limit => 1
    }) of
        [Usage] -> Usage#license_usage.timestamp;
        _ -> 0
    end.

calculate_idle_savings(IdleAllocations) ->
    %% Calculate savings from reclaiming idle licenses
    IdleCount = lists:sum([A#license_allocation.allocated_count || A <- IdleAllocations]);
    AverageCost = get_average_license_cost();

    IdleCount * AverageCost.

get_average_license_cost() ->
    %% Get average license cost
    case application:get_env(erlmcp, average_license_cost) of
        {ok, Cost} -> Cost;
        undefined -> 1000.0  $1000 default
    end.

calculate_allocation_savings(Analysis, TargetAllocation) ->
    %% Calculate savings from reducing allocations
    CurrentAverage = Analysis#allocation_analysis.average_allocation;
    Reduction = CurrentAverage - TargetAllocation;
    TotalAllocations = Analysis#allocation_analysis.total_allocations;

    Reduction * TotalAllocations * get_average_license_cost().

calculate_allocation_cost(Analysis, TargetAllocation) ->
    %% Calculate cost from increasing allocations
    CurrentAverage = Analysis#allocation_analysis.average_allocation;
    Increase = TargetAllocation - CurrentAverage;
    TotalAllocations = Analysis#allocation_analysis.total_allocations;

    Increase * TotalAllocations * get_average_license_cost().

calculate_cost_efficiency(Analysis) ->
    %% Calculate cost efficiency
    Utilization = Analysis#allocation_analysis.average_allocation / 10.0;
    Efficiency = case Utilization of
        U when U > 0.8 -> 0.9;
        U when U > 0.5 -> 0.7;
        U when U > 0.3 -> 0.5;
        _ -> 0.3
    end;

    PotentialSavings = (1.0 - Efficiency) * Analysis#allocation_analysis.total_allocated * get_average_license_cost();

    #cost_efficiency{
        efficiency => Efficiency,
        potential_savings => PotentialSavings
    }.

calculate_cost_trend(Costs) ->
    %% Calculate cost trend
    case Costs of
        [_, _ | _] ->
            Recent = lists:sublist(Costs, 3);
        _ ->
            Recent = Costs
    end;

    case length(Recent) > 1 of
        true ->
            First = Recent#license_cost.total_cost;
            Last = lists:last(Recent)#license_cost.total_cost;
            (Last - First) / First;
        false ->
            0.0
    end.

reclaim_license(Allocation) ->
    %% Reclaim license allocation
    revoke_license(Allocation).

update_cost_metrics(PoolId, Cost) ->
    %% Update cost metrics
    erlmcp_metrics:record_metric(<<"license_cost_usd">>, Cost, #{
        <<"pool_id">> => PoolId
    }),

    erlmcp_metrics:record_metric(<<"license_cost_total_usd">>, Cost, #{}).

apply_optimal_allocation(OptimalAllocation, State) ->
    %% Apply optimal allocation
    case OptimalAllocation of
        increase ->
            increase_license_allocation(State);
        decrease ->
            decrease_license_allocation(State);
        redistribute ->
            redistribute_license_allocation(State);
        _ ->
            ok
    end.

increase_license_allocation(State) ->
    %% Increase license allocation
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Increase each allocation
    lists:foreach(fun(Allocation) ->
        CurrentCount = Allocation#license_allocation.allocated_count;
        NewCount = min(CurrentCount + 1, 10);

        case NewCount > CurrentCount of
            true ->
                increase_allocation(Allocation, NewCount);
            false ->
                ok
        end
    end, ActiveAllocations),

    ok.

decrease_license_allocation(State) ->
    %% Decrease license allocation
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status =:= active];

    Decrease each allocation
    lists:foreach(fun(Allocation) ->
        CurrentCount = Allocation#license_allocation.allocated_count;
        NewCount = max(CurrentCount - 1, 1);

        case NewCount < CurrentCount of
            true ->
                reduce_allocation(Allocation, NewCount);
            false ->
                ok
        end
    end, ActiveAllocations),

    ok.

redistribute_license_allocation(State) ->
    %% Redistribute license allocation
    ActiveAllocations = [A || A <- maps:values(State#license_allocations),
                            A#license_allocation.status = := active];

    TotalAllocated = lists:sum([A#license_allocation.allocated_count || A <- ActiveAllocations]);
    OptimalPerAllocation = round(TotalAllocated / length(ActiveAllocations));

    Redistribute each allocation
    lists:foreach(fun(Allocation) ->
        CurrentCount = Allocation#license_allocation.allocated_count;
        NewCount = min(max(OptimalPerAllocation, 1), 10);

        if
            NewCount > CurrentCount ->
                increase_allocation(Allocation, NewCount);
            NewCount < CurrentCount ->
                reduce_allocation(Allocation, NewCount);
            true ->
                ok
        end
    end, ActiveAllocations),

    ok.

calculate_optimal_allocation(State) ->
    %% Calculate optimal allocation
    UsageReport = generate_license_usage_report(State);

    case UsageReport#utilization_rate of
        U when U > 0.9 ->
            increase;
        U when U < 0.3 ->
            decrease;
        _ ->
            redistribute
    end.

%% Conversion functions
pool_to_record(Pool) ->
    #license_pool{
        id = maps:get(id, Pool),
        name = maps:get(name, Pool),
        product = maps:get(product, Pool),
        version = maps:get(version, Pool),
        total_licenses = maps:get(total_licenses, Pool),
        used_licenses = maps:get(used_licenses, Pool),
        available_licenses = maps:get(available_licenses, Pool),
        license_type = maps:get(license_type, Pool),
        cost_per_license = maps:get(cost_per_license, Pool),
        auto_renewal = maps:get(auto_renewal, Pool),
        expiration_date = maps:get(expiration_date, Pool),
        allocation_strategy = maps:get(allocation_strategy, Pool),
        constraints = maps:get(constraints, Pool),
        metadata = maps:get(metadata, Pool)
    }.

allocation_to_record(Allocation) ->
    #license_allocation{
        id = maps:get(id, Allocation),
        pool_id = maps:get(pool_id, Allocation),
        licensee = maps:get(licensee, Allocation),
        allocated_count = maps:get(allocated_count, Allocation),
        start_time = maps:get(start_time, Allocation),
        end_time = maps:get(end_time, Allocation),
        status = maps:get(status, Allocation),
        usage_metrics = maps:get(usage_metrics, Allocation, [])
    }.

rule_to_record(Rule) ->
    #compliance_rule{
        id = maps:get(id, Rule),
        name = maps:get(name, Rule),
        condition = maps:get(condition, Rule),
        action = maps:get(action, Rule),
        severity = maps:get(severity, Rule),
        enabled = maps:get(enabled, Rule)
    }.