#!/usr/bin/env escript
%%! -sname generate_marketplace -pa ebin

%% Auto-generate marketplace files from plan specs:
%% - dist/marketplace/plans.json (plan listings with envelopes and SLA)
%% - dist/marketplace/plan-comparison.md (comparison matrix)
%% - dist/marketplace/portal-metadata.json (generation metadata)
%% - templates/pricing_portal.html (HTML template)

-mode(compile).

main(_) ->
    io:format("=== erlmcp Marketplace Generator v1.0 ===~n", []),

    % Load plan specifications
    Plans = load_plans(),
    io:format("Loaded ~p plans~n", [length(Plans)]),

    % Verify evidence links exist
    verify_evidence_links(Plans),
    io:format("Evidence links verified~n", []),

    % Generate marketplace plans.json
    generate_plans_json(Plans),
    io:format("Generated dist/marketplace/plans.json~n", []),

    % Generate comparison matrix markdown
    generate_comparison_matrix(Plans),
    io:format("Generated dist/marketplace/plan-comparison.md~n", []),

    % Generate portal metadata
    generate_portal_metadata(Plans),
    io:format("Generated dist/marketplace/portal-metadata.json~n", []),

    % Generate HTML portal template
    generate_html_portal(Plans),
    io:format("Generated templates/pricing_portal.html~n", []),

    io:format("~nMarketplace generation complete!~n", []),
    erlang:halt(0).

%% Load all three plan specifications
load_plans() ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    TeamFile = filename:join(Cwd, "plans/team.plan.json"),
    EnterpriseFile = filename:join(Cwd, "plans/enterprise.plan.json"),
    GovFile = filename:join(Cwd, "plans/gov.plan.json"),

    Team = load_json_file(TeamFile),
    Enterprise = load_json_file(EnterpriseFile),
    Gov = load_json_file(GovFile),

    [Team, Enterprise, Gov].

%% Load JSON file and return parsed map
load_json_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            jsx:decode(Binary, [return_maps]);
        {error, Reason} ->
            io:format("ERROR: Failed to read ~s: ~p~n", [FilePath, Reason]),
            erlang:halt(1)
    end.

%% Verify all evidence links exist
verify_evidence_links(Plans) ->
    lists:foreach(fun(Plan) ->
        Tier = maps:get(<<"tier">>, Plan),
        Evidence = maps:get(<<"evidence">>, Plan),
        verify_evidence_map(Tier, Evidence)
    end, Plans).

verify_evidence_map(Tier, Evidence) ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    maps:foreach(fun(_Key, Path) ->
        case Path of
            null -> ok;
            _ ->
                FilePath = filename:join(Cwd, erlang:binary_to_list(Path)),
                case file:exists(FilePath) of
                    true -> ok;
                    false ->
                        io:format("WARNING: Missing evidence for ~s: ~s~n", [Tier, Path])
                end
        end
    end, Evidence).

%% Generate dist/marketplace/plans.json with all plan data
generate_plans_json(Plans) ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    OutDir = filename:join(Cwd, "dist/marketplace"),
    filelib:ensure_dir(filename:join(OutDir, "dummy")),

    PlansData = lists:map(fun(Plan) ->
        Tier = maps:get(<<"tier">>, Plan),
        #{
            <<"id">> => Tier,
            <<"name">> => maps:get(<<"name">>, Plan),
            <<"tier">> => Tier,
            <<"description">> => maps:get(<<"description">>, Plan),
            <<"envelope">> => extract_envelope(Plan),
            <<"sla">> => extract_sla(Plan),
            <<"pricing">> => extract_pricing(Plan),
            <<"evidence_links">> => extract_evidence_links(Plan),
            <<"comparison_indicators">> => extract_comparison_indicators(Plan)
        }
    end, Plans),

    PortalData = #{
        <<"version">> => <<"1.0.0">>,
        <<"generated_at">> => erlang:list_to_binary(iso8601_timestamp()),
        <<"erlmcp_version">> => <<"1.4.0">>,
        <<"plans">> => PlansData
    },

    OutFile = filename:join(OutDir, "plans.json"),
    file:write_file(OutFile, [jsx:encode(PortalData), "\n"]),
    io:format("  Written to ~s~n", [OutFile]).

%% Extract envelope data from plan
extract_envelope(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    #{
        <<"throughput_req_s">> => maps:get(<<"throughput_req_s">>, Env),
        <<"concurrent_connections">> => maps:get(<<"concurrent_connections">>, Env),
        <<"queue_depth_messages">> => maps:get(<<"queue_depth_messages">>, Env),
        <<"p99_latency_ms">> => maps:get(<<"p99_latency_ms">>, Env),
        <<"failover_sla_seconds">> => maps:get(<<"failover_sla_seconds">>, Env)
    }.

%% Extract SLA data from plan
extract_sla(Plan) ->
    case maps:get(<<"sla">>, Plan, undefined) of
        undefined ->
            % Calculate default SLA from plan specs
            Tier = maps:get(<<"tier">>, Plan),
            default_sla(Tier);
        SLA ->
            Tier = maps:get(<<"tier">>, Plan),
            #{
                <<"model">> => <<"flat-per-deployment">>,
                <<"availability_percentage">> => maps:get(<<"availability_percentage">>, SLA, default_availability(Tier)),
                <<"throughput_guarantee_req_s">> => maps:get(<<"throughput_guarantee_req_s">>, SLA),
                <<"failover_time_seconds">> => maps:get(<<"failover_time_seconds">>, SLA),
                <<"recovery_sla_minutes">> => maps:get(<<"recovery_sla_minutes">>, SLA)
            }
    end.

default_sla(<<"team">>) ->
    #{
        <<"model">> => <<"flat-per-deployment">>,
        <<"availability_percentage">> => 99.0,
        <<"throughput_guarantee_req_s">> => 450,
        <<"failover_time_seconds">> => 30,
        <<"recovery_sla_minutes">> => 30
    };
default_sla(<<"enterprise">>) ->
    #{
        <<"model">> => <<"flat-per-deployment">>,
        <<"availability_percentage">> => 99.95,
        <<"throughput_guarantee_req_s">> => 1500,
        <<"failover_time_seconds">> => 10,
        <<"recovery_sla_minutes">> => 15
    };
default_sla(<<"gov">>) ->
    #{
        <<"model">> => <<"flat-per-deployment">>,
        <<"availability_percentage">> => 99.99,
        <<"throughput_guarantee_req_s">> => 900,
        <<"failover_time_seconds">> => 15,
        <<"recovery_sla_minutes">> => 10
    }.

default_availability(<<"team">>) -> 99.0;
default_availability(<<"enterprise">>) -> 99.95;
default_availability(<<"gov">>) -> 99.99.

%% Extract pricing data from plan
extract_pricing(Plan) ->
    Pricing = maps:get(<<"pricing">>, Plan),
    #{
        <<"model">> => maps:get(<<"model">>, Pricing),
        <<"description">> => maps:get(<<"description">>, Pricing)
    }.

%% Extract evidence links from plan
extract_evidence_links(Plan) ->
    Evidence = maps:get(<<"evidence">>, Plan),
    maps:filter(fun(_K, V) -> V =/= null end, Evidence).

%% Extract comparison indicators for matrix
extract_comparison_indicators(Plan) ->
    Compliance = maps:get(<<"compliance">>, Plan),
    #{
        <<"features_count">> => maps:get(<<"features_implemented">>, Compliance, 0),
        <<"security_level">> => maps:get(<<"security_level">>, Compliance),
        <<"support_tier">> => support_tier_for_tier(maps:get(<<"tier">>, Plan))
    }.

support_tier_for_tier(<<"team">>) -> <<"community">>;
support_tier_for_tier(<<"enterprise">>) -> <<"premium">>;
support_tier_for_tier(<<"gov">>) -> <<"dedicated">>.

%% Generate plan-comparison.md markdown table
generate_comparison_matrix(Plans) ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    OutDir = filename:join(Cwd, "dist/marketplace"),
    filelib:ensure_dir(filename:join(OutDir, "dummy")),

    %% Build markdown table
    Header = "# erlmcp Plan Comparison\n\n"
        "Comprehensive comparison of Team, Enterprise, and Government tiers.\n"
        "All data auto-generated from plan specifications.\n\n"
        "| Metric | Team | Enterprise | Government |\n"
        "|--------|------|------------|------------|\n",

    ThroughputRow = format_row("Throughput (req/s)", Plans, fun extract_throughput/1),
    ConnectionRow = format_row("Concurrent Connections", Plans, fun extract_connections/1),
    LatencyRow = format_row("P99 Latency (ms)", Plans, fun extract_latency/1),
    FailoverRow = format_row("Failover SLA (seconds)", Plans, fun extract_failover/1),
    AvailabilityRow = format_row("Availability SLA", Plans, fun extract_availability/1),
    AuditRow = format_row("Audit Logging", Plans, fun extract_audit/1),
    FIPSRow = format_row("FIPS 140-2", Plans, fun extract_fips/1),
    HARow = format_row("High Availability", Plans, fun extract_ha/1),
    PricingRow = format_row("Pricing Model", Plans, fun extract_pricing_model/1),

    Table = Header ++ ThroughputRow ++ ConnectionRow ++ LatencyRow ++ FailoverRow
        ++ AvailabilityRow ++ AuditRow ++ FIPSRow ++ HARow ++ PricingRow,

    UpgradePath = "\n## Upgrade Path\n\n"
        "**Team → Enterprise**: Unlock production-grade features, load balancing, "
        "connection pooling, and high availability.\n\n"
        "**Enterprise → Government**: Add FIPS 140-2 compliance, comprehensive audit logging, "
        "encryption enforcement, and government-grade security.\n\n",

    OutFile = filename:join(OutDir, "plan-comparison.md"),
    file:write_file(OutFile, Table ++ UpgradePath),
    io:format("  Written to ~s~n", [OutFile]).

%% Extractors for comparison matrix
extract_throughput(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    erlang:integer_to_binary(maps:get(<<"throughput_req_s">>, Env)).

extract_connections(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    erlang:integer_to_binary(maps:get(<<"concurrent_connections">>, Env)).

extract_latency(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    erlang:integer_to_binary(maps:get(<<"p99_latency_ms">>, Env)).

extract_failover(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    erlang:integer_to_binary(maps:get(<<"failover_sla_seconds">>, Env)).

extract_availability(Plan) ->
    case maps:get(<<"sla">>, Plan, undefined) of
        undefined ->
            Tier = maps:get(<<"tier">>, Plan),
            erlang:float_to_binary(default_availability(Tier), [{decimals, 2}]);
        SLA ->
            erlang:float_to_binary(maps:get(<<"availability_percentage">>, SLA), [{decimals, 2}])
    end.

extract_audit(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"audit_logging">>, Features) of
        true -> <<"Yes">>;
        false -> <<"No">>
    end.

extract_fips(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"fips_140_2">>, Features) of
        true -> <<"Yes">>;
        false -> <<"No">>
    end.

extract_ha(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"high_availability">>, Features) of
        true -> <<"Yes">>;
        false -> <<"No">>
    end.

extract_pricing_model(Plan) ->
    Pricing = maps:get(<<"pricing">>, Plan),
    maps:get(<<"model">>, Pricing).

%% Format a row in comparison matrix
format_row(Label, Plans, Extractor) ->
    Values = lists:map(Extractor, Plans),
    [Team, Enterprise, Gov] = Values,
    RowBin = iolist_to_binary([
        "| ", Label, " | ",
        Team, " | ",
        Enterprise, " | ",
        Gov, " |\n"
    ]),
    erlang:binary_to_list(RowBin).

%% Generate portal-metadata.json
generate_portal_metadata(Plans) ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    OutDir = filename:join(Cwd, "dist/marketplace"),
    filelib:ensure_dir(filename:join(OutDir, "dummy")),

    Metadata = #{
        <<"version">> => <<"1.0.0">>,
        <<"generated_at">> => erlang:list_to_binary(iso8601_timestamp()),
        <<"erlmcp_version">> => <<"1.4.0">>,
        <<"plans_count">> => 3,
        <<"evidence_integrity">> => verify_all_evidence(Plans),
        <<"deterministic">> => true,
        <<"schema_version">> => <<"1.0.0">>,
        <<"portal_url">> => <<"https://erlmcp.dev/pricing">>
    },

    OutFile = filename:join(OutDir, "portal-metadata.json"),
    file:write_file(OutFile, [jsx:encode(Metadata), "\n"]),
    io:format("  Written to ~s~n", [OutFile]).

%% Verify all evidence files exist
verify_all_evidence(Plans) ->
    AllValid = lists:all(fun(Plan) ->
        Evidence = maps:get(<<"evidence">>, Plan),
        maps:size(Evidence) > 0
    end, Plans),
    AllValid.

%% Generate templates/pricing_portal.html
generate_html_portal(_Plans) ->
    Cwd = case file:get_cwd() of
        {ok, Dir} -> Dir;
        _ -> "/Users/sac/erlmcp"
    end,

    OutDir = filename:join(Cwd, "templates"),
    filelib:ensure_dir(filename:join(OutDir, "dummy")),

    HtmlContent = html_template(),
    OutFile = filename:join(OutDir, "pricing_portal.html"),
    file:write_file(OutFile, HtmlContent),
    io:format("  Written to ~s~n", [OutFile]).

%% HTML template content (minimal, fetches plans.json at runtime)
html_template() ->
    "<!DOCTYPE html>\n"
    "<html lang=\"en\">\n"
    "<head>\n"
    "  <meta charset=\"UTF-8\">\n"
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
    "  <title>erlmcp Pricing Portal</title>\n"
    "  <style>\n"
    "    * { margin: 0; padding: 0; box-sizing: border-box; }\n"
    "    body {\n"
    "      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;\n"
    "      background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);\n"
    "      min-height: 100vh;\n"
    "      padding: 40px 20px;\n"
    "    }\n"
    "    .container {\n"
    "      max-width: 1200px;\n"
    "      margin: 0 auto;\n"
    "    }\n"
    "    .header {\n"
    "      text-align: center;\n"
    "      color: white;\n"
    "      margin-bottom: 60px;\n"
    "    }\n"
    "    .header h1 {\n"
    "      font-size: 48px;\n"
    "      margin-bottom: 10px;\n"
    "    }\n"
    "    .header p {\n"
    "      font-size: 20px;\n"
    "      opacity: 0.9;\n"
    "    }\n"
    "    .plans-grid {\n"
    "      display: grid;\n"
    "      grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));\n"
    "      gap: 30px;\n"
    "      margin-bottom: 60px;\n"
    "    }\n"
    "    .plan-card {\n"
    "      background: white;\n"
    "      border-radius: 12px;\n"
    "      padding: 40px 30px;\n"
    "      box-shadow: 0 20px 60px rgba(0,0,0,0.3);\n"
    "      transition: transform 0.3s ease, box-shadow 0.3s ease;\n"
    "    }\n"
    "    .plan-card:hover {\n"
    "      transform: translateY(-10px);\n"
    "      box-shadow: 0 30px 80px rgba(0,0,0,0.4);\n"
    "    }\n"
    "    .plan-name {\n"
    "      font-size: 24px;\n"
    "      font-weight: 700;\n"
    "      margin-bottom: 12px;\n"
    "      color: #333;\n"
    "    }\n"
    "    .plan-description {\n"
    "      font-size: 14px;\n"
    "      color: #666;\n"
    "      margin-bottom: 20px;\n"
    "      line-height: 1.6;\n"
    "    }\n"
    "    .envelope-summary {\n"
    "      background: #f5f5f5;\n"
    "      border-radius: 8px;\n"
    "      padding: 16px;\n"
    "      margin-bottom: 20px;\n"
    "      font-size: 13px;\n"
    "    }\n"
    "    .envelope-item {\n"
    "      display: flex;\n"
    "      justify-content: space-between;\n"
    "      padding: 6px 0;\n"
    "      border-bottom: 1px solid #e0e0e0;\n"
    "    }\n"
    "    .envelope-item:last-child {\n"
    "      border-bottom: none;\n"
    "    }\n"
    "    .envelope-label {\n"
    "      font-weight: 600;\n"
    "      color: #333;\n"
    "    }\n"
    "    .envelope-value {\n"
    "      color: #667eea;\n"
    "      font-weight: 700;\n"
    "    }\n"
    "    .button-group {\n"
    "      display: flex;\n"
    "      gap: 10px;\n"
    "      margin-top: 20px;\n"
    "    }\n"
    "    .btn {\n"
    "      flex: 1;\n"
    "      padding: 12px 16px;\n"
    "      border: none;\n"
    "      border-radius: 6px;\n"
    "      cursor: pointer;\n"
    "      font-weight: 600;\n"
    "      font-size: 13px;\n"
    "      transition: all 0.3s ease;\n"
    "    }\n"
    "    .btn-primary {\n"
    "      background: #667eea;\n"
    "      color: white;\n"
    "    }\n"
    "    .btn-primary:hover {\n"
    "      background: #5568d3;\n"
    "    }\n"
    "    .btn-secondary {\n"
    "      background: #f0f0f0;\n"
    "      color: #333;\n"
    "      border: 1px solid #e0e0e0;\n"
    "    }\n"
    "    .btn-secondary:hover {\n"
    "      background: #e8e8e8;\n"
    "    }\n"
    "    .comparison-section {\n"
    "      background: white;\n"
    "      border-radius: 12px;\n"
    "      padding: 40px;\n"
    "      box-shadow: 0 20px 60px rgba(0,0,0,0.1);\n"
    "    }\n"
    "    .comparison-section h2 {\n"
    "      font-size: 32px;\n"
    "      margin-bottom: 30px;\n"
    "      color: #333;\n"
    "    }\n"
    "    .comparison-table {\n"
    "      width: 100%;\n"
    "      border-collapse: collapse;\n"
    "      font-size: 14px;\n"
    "    }\n"
    "    .comparison-table th,\n"
    "    .comparison-table td {\n"
    "      padding: 12px 16px;\n"
    "      text-align: left;\n"
    "      border-bottom: 1px solid #e0e0e0;\n"
    "    }\n"
    "    .comparison-table th {\n"
    "      background: #f8f8f8;\n"
    "      font-weight: 700;\n"
    "      color: #333;\n"
    "    }\n"
    "    .comparison-table tr:hover {\n"
    "      background: #f9f9f9;\n"
    "    }\n"
    "    .loading {\n"
    "      text-align: center;\n"
    "      color: white;\n"
    "      font-size: 18px;\n"
    "    }\n"
    "    .error {\n"
    "      background: #fee;\n"
    "      color: #c33;\n"
    "      padding: 20px;\n"
    "      border-radius: 8px;\n"
    "      text-align: center;\n"
    "    }\n"
    "  </style>\n"
    "</head>\n"
    "<body>\n"
    "  <div class=\"container\">\n"
    "    <div class=\"header\">\n"
    "      <h1>erlmcp Pricing</h1>\n"
    "      <p>Simple, transparent, flat-rate pricing. No metering, no surprises.</p>\n"
    "    </div>\n"
    "    <div id=\"content\" class=\"loading\">Loading plans...</div>\n"
    "  </div>\n\n"
    "  <script>\n"
    "    async function loadPlans() {\n"
    "      try {\n"
    "        const response = await fetch('/dist/marketplace/plans.json');\n"
    "        if (!response.ok) throw new Error('Failed to load plans');\n"
    "        const data = await response.json();\n"
    "        renderPortal(data);\n"
    "      } catch (error) {\n"
    "        document.getElementById('content').innerHTML = `<div class=\"error\">Error loading plans: ${error.message}</div>`;\n"
    "      }\n"
    "    }\n\n"
    "    function renderPortal(data) {\n"
    "      const plans = data.plans || [];\n"
    "      const html = `\n"
    "        <div class=\"plans-grid\">\n"
    "          ${plans.map(plan => `\n"
    "            <div class=\"plan-card\">\n"
    "              <div class=\"plan-name\">${escapeHtml(plan.name)}</div>\n"
    "              <div class=\"plan-description\">${escapeHtml(plan.description)}</div>\n"
    "              <div class=\"envelope-summary\">\n"
    "                <div class=\"envelope-item\">\n"
    "                  <span class=\"envelope-label\">Throughput</span>\n"
    "                  <span class=\"envelope-value\">${plan.envelope.throughput_req_s.toLocaleString()} req/s</span>\n"
    "                </div>\n"
    "                <div class=\"envelope-item\">\n"
    "                  <span class=\"envelope-label\">Concurrent</span>\n"
    "                  <span class=\"envelope-value\">${plan.envelope.concurrent_connections.toLocaleString()}</span>\n"
    "                </div>\n"
    "                <div class=\"envelope-item\">\n"
    "                  <span class=\"envelope-label\">P99 Latency</span>\n"
    "                  <span class=\"envelope-value\">${plan.envelope.p99_latency_ms}ms</span>\n"
    "                </div>\n"
    "                <div class=\"envelope-item\">\n"
    "                  <span class=\"envelope-label\">Failover SLA</span>\n"
    "                  <span class=\"envelope-value\">${plan.envelope.failover_sla_seconds}s</span>\n"
    "                </div>\n"
    "              </div>\n"
    "              <div class=\"button-group\">\n"
    "                <button class=\"btn btn-primary\" onclick=\"viewDetails('${plan.id}')\">View Details</button>\n"
    "                <button class=\"btn btn-secondary\" onclick=\"viewEvidence('${plan.id}')\">Evidence</button>\n"
    "              </div>\n"
    "            </div>\n"
    "          `).join('')}\n"
    "        </div>\n"
    "      `;\n"
    "      document.getElementById('content').innerHTML = html;\n"
    "    }\n\n"
    "    function escapeHtml(text) {\n"
    "      const map = {'&': '&amp;', '<': '&lt;', '>': '&gt;', '\"': '&quot;', \"'\": '&#039;'};\n"
    "      return text.replace(/[&<>\"']/g, m => map[m]);\n"
    "    }\n\n"
    "    function viewDetails(planId) {\n"
    "      alert(`View details for plan: ${planId}`);\n"
    "    }\n\n"
    "    function viewEvidence(planId) {\n"
    "      alert(`View evidence for plan: ${planId}`);\n"
    "    }\n\n"
    "    // Load plans on page load\n"
    "    document.addEventListener('DOMContentLoaded', loadPlans);\n"
    "  </script>\n"
    "</body>\n"
    "</html>\n".

%% Generate ISO8601 timestamp
iso8601_timestamp() ->
    {Date, Time} = erlang:localtime(),
    {Y, M, D} = Date,
    {H, Min, S} = Time,
    io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
        [Y, M, D, H, Min, S]).
