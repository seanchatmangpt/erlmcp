%%%-------------------------------------------------------------------
%% @doc
%% SLA Dashboard HTTP Handler
%%
%% Provides HTTP endpoints for real-time SLA monitoring:
%% - GET /metrics/sla/<plan> - Returns comprehensive SLA status
%% - GET /metrics/sla/<plan>/violations - Returns violation history
%% - GET /metrics/sla - Returns all monitored plans status
%%
%% Response includes:
%% - current_throughput_req_s
%% - current_p99_latency_ms
%% - current_failover_s
%% - plan_envelope (target bounds)
%% - compliance_status (PASS/WARN/FAIL)
%% - violations_count
%% - violation_history (last 60 minutes)
%% - metrics_sample_count
%% - last_checked timestamp
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sla_dashboard_handler).

%% Inets httpd callback module
-export([do/1]).

-include_lib("inets/include/httpd.hrl").

%%====================================================================
%% HTTP Handler for SLA Dashboard API
%%====================================================================

-spec do(#mod{}) -> {proceed, list()} | {ok, #mod{}}.
do(ModData) ->
    Method = ModData#mod.method,
    Path = ModData#mod.request_uri,

    case {Method, Path} of
        {"GET", "/metrics/sla"} ->
            handle_all_sla_metrics();
        {"GET", "/metrics/sla/" ++ PlanStr} ->
            Plan = try binary_to_atom(list_to_binary(PlanStr), utf8) catch _:_ -> undefined end,
            handle_plan_sla_metrics(Plan);
        {"GET", "/metrics/sla/" ++ Rest} ->
            case string:split(Rest, "/") of
                [PlanStr, "violations"] ->
                    Plan = try binary_to_atom(list_to_binary(PlanStr), utf8) catch _:_ -> undefined end,
                    handle_plan_violations(Plan);
                _ ->
                    {proceed, [{response, {404, "Not Found"}}]}
            end;
        {_, _} ->
            {proceed, [{response, {404, "Not Found"}}]}
    end.

%%====================================================================
%% Internal Handlers
%%====================================================================

%% @doc Handle GET /metrics/sla - All plans status
-spec handle_all_sla_metrics() -> {proceed, list()}.
handle_all_sla_metrics() ->
    try
        AllPlans = [team, enterprise, gov],
        PlansData = lists:filtermap(fun(Plan) ->
            case erlmcp_plan_sla_monitor_extended:get_sla_dashboard(Plan) of
                {error, _} -> false;
                Dashboard ->
                    {true, #{
                        plan => atom_to_binary(Plan, utf8),
                        data => Dashboard
                    }}
            end
        end, AllPlans),

        Response = #{
            timestamp => erlang:system_time(millisecond),
            plans_monitored => length(PlansData),
            plans => PlansData
        },

        Json = jsx:encode(Response),
        HttpResponse = {response, {200, [
            {content_type, "application/json"},
            {cache_control, "no-cache, must-revalidate"},
            {access_control_allow_origin, "*"}
        ], Json}},
        {proceed, [HttpResponse]}
    catch
        _:_ ->
            error_response(500, "Internal server error")
    end.

%% @doc Handle GET /metrics/sla/<plan> - Specific plan status
-spec handle_plan_sla_metrics(atom() | undefined) -> {proceed, list()}.
handle_plan_sla_metrics(undefined) ->
    error_response(400, "Invalid plan");
handle_plan_sla_metrics(Plan) ->
    try
        case erlmcp_plan_sla_monitor_extended:get_sla_dashboard(Plan) of
            {error, Reason} ->
                ErrorMsg = io_lib:format("Failed to get SLA metrics: ~w", [Reason]),
                error_response(503, ErrorMsg);
            Dashboard ->
                Response = #{
                    timestamp => erlang:system_time(millisecond),
                    plan => atom_to_binary(Plan, utf8),
                    data => Dashboard
                },

                Json = jsx:encode(Response),
                HttpResponse = {response, {200, [
                    {content_type, "application/json"},
                    {cache_control, "no-cache, must-revalidate"},
                    {access_control_allow_origin, "*"}
                ], Json}},
                {proceed, [HttpResponse]}
        end
    catch
        _:_ ->
            error_response(500, "Internal server error")
    end.

%% @doc Handle GET /metrics/sla/<plan>/violations - Violation history
-spec handle_plan_violations(atom() | undefined) -> {proceed, list()}.
handle_plan_violations(undefined) ->
    error_response(400, "Invalid plan");
handle_plan_violations(Plan) ->
    try
        case erlmcp_plan_sla_monitor_extended:get_violation_history(Plan) of
            {error, Reason} ->
                ErrorMsg = io_lib:format("Failed to get violation history: ~w", [Reason]),
                error_response(503, ErrorMsg);
            Violations ->
                Response = #{
                    timestamp => erlang:system_time(millisecond),
                    plan => atom_to_binary(Plan, utf8),
                    violations_count => length(Violations),
                    violations => Violations
                },

                Json = jsx:encode(Response),
                HttpResponse = {response, {200, [
                    {content_type, "application/json"},
                    {cache_control, "no-cache, must-revalidate"},
                    {access_control_allow_origin, "*"}
                ], Json}},
                {proceed, [HttpResponse]}
        end
    catch
        _:_ ->
            error_response(500, "Internal server error")
    end.

%%====================================================================
%% Response Helpers
%%====================================================================

%% @doc Generate error response
-spec error_response(non_neg_integer(), string() | iolist()) -> {proceed, list()}.
error_response(StatusCode, Message) ->
    ErrorJson = jsx:encode(#{
        error => <<"error">>,
        message => iolist_to_binary(Message),
        timestamp => erlang:system_time(millisecond)
    }),

    HttpResponse = {response, {StatusCode, [
        {content_type, "application/json"},
        {access_control_allow_origin, "*"}
    ], ErrorJson}},

    {proceed, [HttpResponse]}.
