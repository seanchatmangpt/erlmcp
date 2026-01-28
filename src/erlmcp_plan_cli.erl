%%%-------------------------------------------------------------------
%% @doc erlmcp_plan_cli - Plan management commands for CLI
%%
%% Provides commands for:
%% - erlmcp plan list - List all available tiers
%% - erlmcp plan show <tier> - Show tier specification
%% - erlmcp plan status - Show current active plan
%% - erlmcp plan test-refusal <scenario> - Test refusal behavior
%% - erlmcp upgrade plan <from> <to> - Show upgrade path
%%
%% All output is deterministic and machine-readable JSON + human text.
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plan_cli).

-export([
    list_plans/0,
    show_plan/1,
    current_plan/0,
    test_refusal/1,
    upgrade_path/2
]).


-type tier() :: team | enterprise | gov.
-type plan_spec() :: map().

%%%-------------------------------------------------------------------
%% Public API
%%%-------------------------------------------------------------------

%% @doc List all available plans
-spec list_plans() -> {ok, iodata()} | {error, term()}.
list_plans() ->
    try
        Plans = [team, enterprise, gov],
        Output = [
            "Available Plans\n",
            "===============\n\n",
            [format_plan_summary(Plan) || Plan <- Plans],
            "\n",
            "Use: erlmcp plan show <tier> for details\n"
        ],
        {ok, Output}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Show specification for a specific tier
-spec show_plan(tier()) -> {ok, iodata()} | {error, term()}.
show_plan(Tier) when is_atom(Tier) ->
    try
        Spec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
        Output = [
            format_plan_details(Tier, Spec),
            "\n",
            format_as_json(Spec)
        ],
        {ok, Output}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Get current active plan
-spec current_plan() -> {ok, iodata()} | {error, term()}.
current_plan() ->
    try
        % Default to team tier if not configured
        Tier = get_active_tier(),
        Spec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
        Envelope = maps:get(<<"envelope">>, Spec),
        Output = [
            "Current Plan: ", atom_to_list(Tier), "\n",
            "Throughput:   ", integer_to_list(maps:get(<<"throughput_req_s">>, Envelope)), " req/s\n",
            "Connections:  ", integer_to_list(maps:get(<<"concurrent_connections">>, Envelope)), " concurrent\n",
            "Latency SLA:  ", integer_to_list(maps:get(<<"p99_latency_ms">>, Envelope)), " ms (p99)\n",
            "Failover SLA: ", integer_to_list(maps:get(<<"failover_sla_seconds">>, Envelope)), " seconds\n"
        ],
        {ok, Output}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Test refusal behavior for a scenario
-spec test_refusal(atom()) -> {ok, iodata()} | {error, term()}.
test_refusal(Scenario) ->
    try
        Tier = get_active_tier(),
        Spec = erlmcp_plan_docs_generator:load_plan_spec(Tier),
        Refusal = maps:get(<<"refusal_behavior">>, Spec),
        ScenarioKey = atom_to_binary(Scenario),
        case maps:get(ScenarioKey, Refusal, undefined) of
            undefined ->
                {error, {unknown_scenario, Scenario}};
            Behavior ->
                HttpStatus = maps:get(<<"http_status">>, Behavior, "N/A"),
                ErrorCode = maps:get(<<"error_code">>, Behavior, ""),
                Message = maps:get(<<"message">>, Behavior, ""),
                Output = [
                    "{\"status\": ", format_status_code(HttpStatus), ", ",
                    "\"error_code\": \"", binary_to_list(ErrorCode), "\", ",
                    "\"message\": \"", binary_to_list(Message), "\"}"
                ],
                {ok, Output}
        end
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc Show upgrade path from one tier to another
-spec upgrade_path(tier(), tier()) -> {ok, iodata()} | {error, term()}.
upgrade_path(From, To) when is_atom(From), is_atom(To) ->
    try
        FromSpec = erlmcp_plan_docs_generator:load_plan_spec(From),
        ToSpec = erlmcp_plan_docs_generator:load_plan_spec(To),

        FromEnvelope = maps:get(<<"envelope">>, FromSpec),
        ToEnvelope = maps:get(<<"envelope">>, ToSpec),

        FromThroughput = maps:get(<<"throughput_req_s">>, FromEnvelope),
        ToThroughput = maps:get(<<"throughput_req_s">>, ToEnvelope),

        Output = [
            "Upgrade Path: ", atom_to_list(From), " → ", atom_to_list(To), "\n",
            "===============================================\n\n",
            "Current Tier (", atom_to_list(From), "):\n",
            "  Throughput: ", integer_to_list(FromThroughput), " req/s\n\n",
            "New Tier (", atom_to_list(To), "):\n",
            "  Throughput: ", integer_to_list(ToThroughput), " req/s\n",
            "  Improvement: ", format_improvement(FromThroughput, ToThroughput), "\n\n",
            "To upgrade, contact support or run:\n",
            "  erlmcp upgrade plan ", atom_to_list(From), " ", atom_to_list(To), "\n"
        ],
        {ok, Output}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec format_plan_summary(tier()) -> iodata().
format_plan_summary(team) ->
    [
        "1. Team Tier\n",
        "   - 450 req/s throughput\n",
        "   - 128 concurrent connections\n",
        "   - Perfect for startups and POCs\n\n"
    ];
format_plan_summary(enterprise) ->
    [
        "2. Enterprise Tier\n",
        "   - 1500 req/s throughput\n",
        "   - 512 concurrent connections\n",
        "   - Production-grade availability\n\n"
    ];
format_plan_summary(gov) ->
    [
        "3. Government Tier\n",
        "   - 900 req/s throughput\n",
        "   - 256 concurrent connections\n",
        "   - FIPS-140-2 compliance + audit logging\n\n"
    ].

-spec format_plan_details(tier(), plan_spec()) -> iodata().
format_plan_details(Tier, Spec) ->
    Name = maps:get(<<"name">>, Spec),
    Description = maps:get(<<"description">>, Spec),
    Envelope = maps:get(<<"envelope">>, Spec),
    Throughput = maps:get(<<"throughput_req_s">>, Envelope),
    Concurrent = maps:get(<<"concurrent_connections">>, Envelope),
    QueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),
    Latency = maps:get(<<"p99_latency_ms">>, Envelope),
    Failover = maps:get(<<"failover_sla_seconds">>, Envelope),
    [
        "Plan: ", atom_to_list(Tier), "\n",
        "======================================\n\n",
        "Name: ", binary_to_list(Name), "\n",
        "Description: ", binary_to_list(Description), "\n\n",
        "Envelope:\n",
        "  Throughput:         ", integer_to_list(Throughput), " req/s\n",
        "  Connections:        ", integer_to_list(Concurrent), " concurrent\n",
        "  Queue Depth:        ", integer_to_list(QueueDepth), " messages\n",
        "  P99 Latency:        ", integer_to_list(Latency), " ms\n",
        "  Failover SLA:       ", integer_to_list(Failover), " seconds\n\n"
    ].

-spec format_as_json(plan_spec()) -> iodata().
format_as_json(Spec) ->
    jsx:encode(Spec, [space, indent]).

-spec format_improvement(integer(), integer()) -> string().
format_improvement(From, To) ->
    case To > From of
        true ->
            Percent = ((To - From) * 100) div From,
            "+" ++ integer_to_list(Percent) ++ "%";
        false ->
            "-" ++ integer_to_list(((From - To) * 100) div From) ++ "%"
    end.

-spec format_status_code(term()) -> string().
format_status_code(Code) when is_integer(Code) ->
    integer_to_list(Code);
format_status_code("N/A") ->
    "null";
format_status_code(Code) ->
    integer_to_list(erlang:binary_to_integer(Code)).

-spec get_active_tier() -> tier().
get_active_tier() ->
    % Read from configuration or environment
    case application:get_env(erlmcp, current_plan, team) of
        Tier when is_atom(Tier) -> Tier;
        _ -> team
    end.
