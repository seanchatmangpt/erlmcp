%%%-------------------------------------------------------------------
%% @doc
%% HTTP Handler for SLA Dashboard Endpoint
%%
%% Cowboy REST handler for /metrics/sla/<plan> endpoint
%% Returns real-time SLA compliance metrics in JSON format
%%
%% Metrics returned:
%% - current_throughput_req_s
%% - current_p99_latency_ms
%% - current_failover_s
%% - plan_envelope (target bounds)
%% - compliance_status (PASS/WARN/FAIL)
%% - violations_count (total alerts in period)
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sla_http_handler).

%% Cowboy HTTP handler
-export([init/2, allowed_methods/2, content_types_provided/2, to_json/2]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Cowboy REST Handler
%%====================================================================

init(Req, Opts) ->
    {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
    {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
    {[{<<"application/json">>, to_json}], Req, State}.

to_json(Req, State) ->
    % Extract plan from path
    Plan = extract_plan_from_path(Req),

    % Get SLA dashboard data
    case erlmcp_plan_sla_monitor:get_sla_dashboard(Plan) of
        {error, Reason} ->
            ?LOG_WARNING("Failed to get SLA dashboard for ~w: ~w", [Plan, Reason]),
            ErrorJson = jsx:encode(#{
                error => <<"Failed to retrieve SLA metrics">>,
                plan => atom_to_binary(Plan),
                timestamp => erlang:system_time(millisecond)
            }),
            Req2 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
            Req3 = cowboy_req:reply(500, #{}, ErrorJson, Req2),
            {stop, Req3, State};

        Dashboard ->
            % Convert to JSON-compatible format
            JsonData = dashboard_to_json(Dashboard),
            Json = jsx:encode(JsonData),

            % Set CORS headers
            Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req),
            Req3 = cowboy_req:set_resp_header(<<"cache-control">>, <<"no-cache, no-store">>, Req2),
            Req4 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req3),

            {Json, Req4, State}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Extract plan from URL path /metrics/sla/<plan>
-spec extract_plan_from_path(term()) -> atom().
extract_plan_from_path(Req) ->
    Path = cowboy_req:path(Req),
    % Path format: /metrics/sla/<plan>
    Parts = binary:split(Path, <<"/">>, [global]),
    case lists:last(Parts) of
        <<"team">> -> team;
        <<"enterprise">> -> enterprise;
        <<"gov">> -> gov;
        Other ->
            BinStr = binary_to_list(Other),
            try
                list_to_existing_atom(BinStr)
            catch
                error:badarg -> unknown
            end
    end.

%% @doc Convert dashboard map to JSON-serializable format
-spec dashboard_to_json(map()) -> map().
dashboard_to_json(Dashboard) ->
    Plan = maps:get(plan, Dashboard),
    Throughput = maps:get(throughput, Dashboard),
    Latency = maps:get(latency, Dashboard),
    Failover = maps:get(failover, Dashboard),
    OverallStatus = maps:get(overall_status, Dashboard),
    ViolationsCount = maps:get(violations_count, Dashboard),

    #{
        plan => atom_to_binary(Plan),
        compliance_status => atom_to_binary(OverallStatus),
        timestamp => maps:get(timestamp, Dashboard),

        metrics => #{
            throughput => #{
                current_req_s => round(maps:get(current, Throughput, 0)),
                minimum_req_s => maps:get(minimum, Throughput, 0),
                status => atom_to_binary(maps:get(status, Throughput, fail))
            },

            latency => #{
                current_p99_ms => maps:get(current_p99_ms, Latency, 0),
                maximum_ms => maps:get(maximum_ms, Latency, 0),
                status => atom_to_binary(maps:get(status, Latency, fail))
            },

            failover => #{
                current_s => maps:get(current_s, Failover, 0.0),
                maximum_s => maps:get(maximum_s, Failover, 0.0),
                status => atom_to_binary(maps:get(status, Failover, fail))
            }
        },

        plan_envelope => get_plan_envelope_description(Plan),
        violations_count => ViolationsCount,
        sla_window_minutes => maps:get(sla_window_minutes, Dashboard, 60),
        description => maps:get(description, Dashboard, <<"">>)
    }.

%% @doc Get plan envelope description for JSON response
-spec get_plan_envelope_description(atom()) -> map().
get_plan_envelope_description(team) ->
    #{
        name => <<"Team">>,
        min_throughput_req_s => 450,
        max_latency_p99_ms => 150,
        max_failover_s => 5,
        description => <<"Team plan: 450+ req/s, p99 ≤150ms, failover ≤5s">>
    };

get_plan_envelope_description(enterprise) ->
    #{
        name => <<"Enterprise">>,
        min_throughput_req_s => 1500,
        max_latency_p99_ms => 100,
        max_failover_s => 2,
        description => <<"Enterprise plan: 1500+ req/s, p99 ≤100ms, failover ≤2s">>
    };

get_plan_envelope_description(gov) ->
    #{
        name => <<"Government">>,
        min_throughput_req_s => 900,
        max_latency_p99_ms => 80,
        max_failover_s => 1,
        description => <<"Gov plan: 900+ req/s, p99 ≤80ms, failover ≤1s + audit">>
    };

get_plan_envelope_description(_) ->
    #{
        name => <<"Unknown">>,
        description => <<"Unknown plan">>
    }.
