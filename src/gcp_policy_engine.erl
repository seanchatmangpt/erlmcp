%%%-------------------------------------------------------------------
%% @doc Governance Policy Engine
%%
%% Evaluates governance policies for GCP operations:
%% - Tier-based access control (automate/assist/do-not-automate)
%% - Resource quotas and rate limiting
%% - Time-based restrictions
%% - Cost controls
%%
%% 80/20: Simple, composable rules that cover most use cases.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_policy_engine).

-include("erlmcp_governance.hrl").
-include("gcp_simulator.hrl").

%% API
-export([
    evaluate/3,
    create_policy/1,
    add_rule/2,
    remove_rule/2
]).

%% Built-in rule constructors
-export([
    %% Access rules
    tier_rule/1,
    operation_whitelist/1,
    operation_blacklist/1,

    %% Resource rules
    max_instances/1,
    max_storage_gb/1,
    max_messages_per_minute/1,

    %% Time rules
    business_hours_only/0,
    no_weekends/0,
    time_window/2,

    %% Cost rules
    max_cost_per_day/1,
    require_approval_above/1
]).

-record(policy, {
    id :: binary(),
    name :: binary(),
    rules = [] :: [rule()],
    default_action = allow :: allow | deny
}).

-type rule() :: {rule_type(), term()}.
-type rule_type() :: tier | whitelist | blacklist | max_instances |
                     max_storage | rate_limit | time_window | cost_limit.

-type eval_result() :: allow | {deny, binary()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Evaluate a policy against an operation context.
-spec evaluate(#policy{}, atom(), map()) -> eval_result().
evaluate(Policy, Operation, Context) ->
    Results = [evaluate_rule(Rule, Operation, Context) || Rule <- Policy#policy.rules],
    IsDenial = fun({deny, _}) -> true; (_) -> false end,
    case lists:any(IsDenial, Results) of
        true ->
            %% Return first denial
            hd([R || R <- Results, IsDenial(R)]);
        false ->
            allow
    end.

%% @doc Create a new policy.
-spec create_policy(map()) -> #policy{}.
create_policy(Opts) ->
    #policy{
        id = maps:get(id, Opts, generate_id()),
        name = maps:get(name, Opts, <<"unnamed">>),
        rules = maps:get(rules, Opts, []),
        default_action = maps:get(default, Opts, allow)
    }.

%% @doc Add a rule to a policy.
-spec add_rule(#policy{}, rule()) -> #policy{}.
add_rule(Policy, Rule) ->
    Policy#policy{rules = [Rule | Policy#policy.rules]}.

%% @doc Remove a rule from a policy.
-spec remove_rule(#policy{}, rule_type()) -> #policy{}.
remove_rule(Policy, RuleType) ->
    NewRules = [R || R <- Policy#policy.rules, element(1, R) =/= RuleType],
    Policy#policy{rules = NewRules}.

%%====================================================================
%% Rule Constructors
%%====================================================================

%% @doc Tier-based access control.
tier_rule(AllowedTiers) when is_list(AllowedTiers) ->
    {tier, AllowedTiers}.

%% @doc Operation whitelist.
operation_whitelist(Operations) when is_list(Operations) ->
    {whitelist, Operations}.

%% @doc Operation blacklist.
operation_blacklist(Operations) when is_list(Operations) ->
    {blacklist, Operations}.

%% @doc Maximum compute instances.
max_instances(Count) when is_integer(Count), Count > 0 ->
    {max_instances, Count}.

%% @doc Maximum storage in GB.
max_storage_gb(GB) when is_number(GB), GB > 0 ->
    {max_storage, GB * 1024 * 1024 * 1024}.

%% @doc Rate limit messages per minute.
max_messages_per_minute(Count) when is_integer(Count), Count > 0 ->
    {rate_limit, {pubsub, Count, 60000}}.

%% @doc Allow only during business hours (9-17 UTC).
business_hours_only() ->
    {time_window, {9, 0}, {17, 0}}.

%% @doc Deny on weekends.
no_weekends() ->
    {no_weekends, true}.

%% @doc Custom time window.
time_window(Start, End) ->
    {time_window, Start, End}.

%% @doc Maximum cost per day in USD.
max_cost_per_day(USD) when is_number(USD), USD > 0 ->
    {cost_limit, {daily, USD}}.

%% @doc Require approval for operations above cost threshold.
require_approval_above(USD) when is_number(USD), USD > 0 ->
    {approval_required, USD}.

%%====================================================================
%% Rule Evaluation
%%====================================================================

evaluate_rule({tier, AllowedTiers}, _Operation, Context) ->
    CurrentTier = maps:get(tier, Context, tier_b_assist),
    case lists:member(CurrentTier, AllowedTiers) of
        true -> allow;
        false -> {deny, <<"Operation tier not allowed">>}
    end;

evaluate_rule({whitelist, Operations}, Operation, _Context) ->
    OpBin = ensure_binary(Operation),
    case lists:member(OpBin, Operations) of
        true -> allow;
        false -> {deny, <<"Operation not in whitelist">>}
    end;

evaluate_rule({blacklist, Operations}, Operation, _Context) ->
    OpBin = ensure_binary(Operation),
    case lists:member(OpBin, Operations) of
        true -> {deny, <<"Operation is blacklisted">>};
        false -> allow
    end;

evaluate_rule({max_instances, Max}, Operation, Context) ->
    case is_compute_operation(Operation) of
        false -> allow;
        true ->
            Current = maps:get(instance_count, Context, 0),
            case Current < Max of
                true -> allow;
                false -> {deny, <<"Maximum instances reached">>}
            end
    end;

evaluate_rule({max_storage, MaxBytes}, Operation, Context) ->
    case is_storage_operation(Operation) of
        false -> allow;
        true ->
            Current = maps:get(storage_bytes, Context, 0),
            case Current < MaxBytes of
                true -> allow;
                false -> {deny, <<"Storage quota exceeded">>}
            end
    end;

evaluate_rule({rate_limit, {Service, Max, WindowMs}}, Operation, Context) ->
    case is_service_operation(Service, Operation) of
        false -> allow;
        true ->
            Key = {Service, rate_limit},
            {Count, LastReset} = maps:get(Key, Context, {0, 0}),
            Now = erlang:system_time(millisecond),
            case Now - LastReset > WindowMs of
                true ->
                    %% Window expired, reset
                    allow;
                false ->
                    case Count < Max of
                        true -> allow;
                        false -> {deny, <<"Rate limit exceeded">>}
                    end
            end
    end;

evaluate_rule({time_window, {StartH, StartM}, {EndH, EndM}}, _Operation, _Context) ->
    {_, {H, M, _}} = calendar:universal_time(),
    StartMinutes = StartH * 60 + StartM,
    EndMinutes = EndH * 60 + EndM,
    CurrentMinutes = H * 60 + M,
    case CurrentMinutes >= StartMinutes andalso CurrentMinutes =< EndMinutes of
        true -> allow;
        false -> {deny, <<"Outside allowed time window">>}
    end;

evaluate_rule({no_weekends, true}, _Operation, _Context) ->
    {Date, _} = calendar:universal_time(),
    DayOfWeek = calendar:day_of_the_week(Date),
    case DayOfWeek of
        6 -> {deny, <<"Operations not allowed on Saturday">>};
        7 -> {deny, <<"Operations not allowed on Sunday">>};
        _ -> allow
    end;

evaluate_rule({cost_limit, {daily, MaxUSD}}, Operation, Context) ->
    TodaySpend = maps:get(today_spend_usd, Context, 0.0),
    OpCost = estimate_operation_cost(Operation, Context),
    case TodaySpend + OpCost =< MaxUSD of
        true -> allow;
        false -> {deny, <<"Daily cost limit exceeded">>}
    end;

evaluate_rule({approval_required, Threshold}, Operation, Context) ->
    OpCost = estimate_operation_cost(Operation, Context),
    HasApproval = maps:get(approved, Context, false),
    case OpCost > Threshold andalso not HasApproval of
        true -> {deny, <<"Approval required for high-cost operation">>};
        false -> allow
    end;

evaluate_rule(_Unknown, _Operation, _Context) ->
    allow.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_id() ->
    base64:encode(crypto:strong_rand_bytes(12)).

ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
ensure_binary(Bin) when is_binary(Bin) -> Bin.

is_compute_operation(Op) ->
    OpBin = ensure_binary(Op),
    binary:match(OpBin, <<"compute.">>) =/= nomatch.

is_storage_operation(Op) ->
    OpBin = ensure_binary(Op),
    binary:match(OpBin, <<"storage.">>) =/= nomatch.

is_service_operation(pubsub, Op) ->
    OpBin = ensure_binary(Op),
    binary:match(OpBin, <<"pubsub.">>) =/= nomatch;
is_service_operation(_, _) ->
    false.

%% Simplified cost estimation (would use actual pricing in production)
estimate_operation_cost(Operation, Context) ->
    OpBin = ensure_binary(Operation),
    case OpBin of
        <<"compute.instances.create">> ->
            MachineType = maps:get(machine_type, Context, <<"e2-micro">>),
            machine_hourly_cost(MachineType) * 24;  %% Assume 1 day
        <<"storage.objects.create">> ->
            Bytes = maps:get(payload_bytes, Context, 0),
            (Bytes / (1024 * 1024 * 1024)) * 0.02;  %% $0.02/GB/month
        _ ->
            0.001  %% Nominal cost for API calls
    end.

machine_hourly_cost(<<"e2-micro">>) -> 0.0084;
machine_hourly_cost(<<"e2-small">>) -> 0.0168;
machine_hourly_cost(<<"e2-medium">>) -> 0.0336;
machine_hourly_cost(<<"e2-standard-2">>) -> 0.0672;
machine_hourly_cost(<<"e2-standard-4">>) -> 0.1344;
machine_hourly_cost(<<"n1-standard-1">>) -> 0.0475;
machine_hourly_cost(<<"n1-standard-2">>) -> 0.0950;
machine_hourly_cost(_) -> 0.05.  %% Default
