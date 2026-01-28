%%%-------------------------------------------------------------------
%% @doc erlmcp_plan - Plan management and verification module
%%
%% Provides functions for:
%% - Loading and validating plan specifications
%% - Checking tier limits and boundaries
%% - Verifying compliance with SLAs
%% - Managing plan state
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plan).

-export([
    current_plan/0,
    show/1,
    list_plans/0,
    get_envelope/1,
    get_limits/1,
    get_features/1,
    check_throughput_limit/2,
    check_connection_limit/2,
    check_message_size/2,
    verify_sla/1
]).


-type tier() :: team | enterprise | gov.
-type plan_spec() :: map().
-type plan_envelope() :: map().
-type plan_limits() :: map().

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc Get currently active plan
-spec current_plan() -> {ok, {tier(), plan_spec()}} | {error, term()}.
current_plan() ->
    Tier = get_active_tier(),
    case erlmcp_plan_docs_generator:load_plan_spec(Tier) of
        Spec when is_map(Spec) ->
            {ok, {Tier, Spec}};
        Error ->
            {error, Error}
    end.

%% @doc Get specification for a specific tier
-spec show(tier()) -> {ok, plan_spec()} | {error, term()}.
show(Tier) when is_atom(Tier) ->
    try
        Spec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
        {ok, Spec}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc List all available plans
-spec list_plans() -> {ok, [tier()]} | {error, term()}.
list_plans() ->
    {ok, [team, enterprise, gov]}.

%% @doc Get envelope for tier
-spec get_envelope(tier()) -> {ok, plan_envelope()} | {error, term()}.
get_envelope(Tier) ->
    case show(Tier) of
        {ok, Spec} ->
            {ok, maps:get(<<"envelope">>, Spec)};
        Error ->
            Error
    end.

%% @doc Get limits for tier
-spec get_limits(tier()) -> {ok, plan_limits()} | {error, term()}.
get_limits(Tier) ->
    case show(Tier) of
        {ok, Spec} ->
            {ok, maps:get(<<"limits">>, Spec)};
        Error ->
            Error
    end.

%% @doc Get features for tier
-spec get_features(tier()) -> {ok, map()} | {error, term()}.
get_features(Tier) ->
    case show(Tier) of
        {ok, Spec} ->
            {ok, maps:get(<<"features">>, Spec)};
        Error ->
            Error
    end.

%% @doc Check if throughput is within tier limit
-spec check_throughput_limit(tier(), non_neg_integer()) -> ok | {error, rate_limit_exceeded}.
check_throughput_limit(Tier, RequestsPerSecond) ->
    case get_envelope(Tier) of
        {ok, Envelope} ->
            Limit = maps:get(<<"throughput_req_s">>, Envelope),
            case RequestsPerSecond =< Limit of
                true -> ok;
                false -> {error, rate_limit_exceeded}
            end;
        Error ->
            Error
    end.

%% @doc Check if connection count is within tier limit
-spec check_connection_limit(tier(), non_neg_integer()) -> ok | {error, connection_limit_exceeded}.
check_connection_limit(Tier, ConnectionCount) ->
    case get_envelope(Tier) of
        {ok, Envelope} ->
            Limit = maps:get(<<"concurrent_connections">>, Envelope),
            case ConnectionCount =< Limit of
                true -> ok;
                false -> {error, connection_limit_exceeded}
            end;
        Error ->
            Error
    end.

%% @doc Check if message size is within tier limit
-spec check_message_size(tier(), pos_integer()) -> ok | {error, payload_too_large}.
check_message_size(Tier, SizeBytes) ->
    case get_limits(Tier) of
        {ok, Limits} ->
            MaxSize = maps:get(<<"max_message_size_bytes">>, Limits),
            case SizeBytes =< MaxSize of
                true -> ok;
                false -> {error, payload_too_large}
            end;
        Error ->
            Error
    end.

%% @doc Verify SLA compliance for tier
-spec verify_sla(tier()) -> {ok, map()} | {error, term()}.
verify_sla(Tier) ->
    case show(Tier) of
        {ok, Spec} ->
            case maps:get(<<"sla">>, Spec, undefined) of
                undefined ->
                    {ok, #{
                        tier => Tier,
                        has_sla => false,
                        message => "Tier does not have explicit SLA"
                    }};
                SLA ->
                    {ok, #{
                        tier => Tier,
                        has_sla => true,
                        sla => SLA
                    }}
            end;
        Error ->
            Error
    end.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec get_active_tier() -> tier().
get_active_tier() ->
    case application:get_env(erlmcp, current_plan, undefined) of
        Tier when is_atom(Tier), Tier =:= team orelse Tier =:= enterprise orelse Tier =:= gov ->
            Tier;
        _ ->
            % Check environment variable
            case os:getenv("ERLMCP_PLAN") of
                "enterprise" -> enterprise;
                "gov" -> gov;
                _ -> team
            end
    end.
