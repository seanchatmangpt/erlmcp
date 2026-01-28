%%%-------------------------------------------------------------------
%%% @doc Auto-generates pricing portal from plan specifications
%%% Produces deterministic output: plans.json, comparison matrix, HTML template
%%%
%%% Usage: erlmcp_portal_generator:generate()
%%%-------------------------------------------------------------------
-module(erlmcp_portal_generator).

-export([generate/0, generate/1]).

-spec generate() -> ok | {error, term()}.
generate() ->
    generate("/Users/sac/erlmcp").

-spec generate(string()) -> ok | {error, term()}.
generate(Cwd) ->
    io:format("=== erlmcp Pricing Portal Generator ===~n", []),

    % Load plan specifications
    case load_plans(Cwd) of
        {ok, Plans} ->
            io:format("Loaded ~p plans~n", [length(Plans)]),

            % Create output directories
            filelib:ensure_dir(filename:join(Cwd, "dist/marketplace/")),
            filelib:ensure_dir(filename:join(Cwd, "templates/")),

            % Generate all artifacts
            generate_plans_json(Cwd, Plans),
            generate_comparison_matrix(Cwd, Plans),
            generate_portal_metadata(Cwd, Plans),
            generate_html_portal(Cwd),

            io:format("~n✓ Portal generation complete!~n", []),
            ok;

        {error, Reason} ->
            io:format("ERROR: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Load all three plan specifications
load_plans(Cwd) ->
    Files = [
        filename:join(Cwd, "plans/team.plan.json"),
        filename:join(Cwd, "plans/enterprise.plan.json"),
        filename:join(Cwd, "plans/gov.plan.json")
    ],

    try
        Plans = [load_json_file(File) || File <- Files],
        {ok, Plans}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% Load and parse JSON file
load_json_file(FilePath) ->
    {ok, Binary} = file:read_file(FilePath),
    jsx:decode(Binary, [return_maps]).

%% Generate dist/marketplace/plans.json
generate_plans_json(Cwd, Plans) ->
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
        <<"generated_at">> => erlang:list_to_binary(iso8601_now()),
        <<"erlmcp_version">> => <<"1.4.0">>,
        <<"plans">> => PlansData
    },

    OutDir = filename:join(Cwd, "dist/marketplace"),
    OutFile = filename:join(OutDir, "plans.json"),
    file:write_file(OutFile, [jsx:encode(PortalData), "\n"]),
    io:format("  Generated ~s~n", [OutFile]).

%% Extract envelope from plan
extract_envelope(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    #{
        <<"throughput_req_s">> => maps:get(<<"throughput_req_s">>, Env),
        <<"concurrent_connections">> => maps:get(<<"concurrent_connections">>, Env),
        <<"queue_depth_messages">> => maps:get(<<"queue_depth_messages">>, Env),
        <<"p99_latency_ms">> => maps:get(<<"p99_latency_ms">>, Env),
        <<"failover_sla_seconds">> => maps:get(<<"failover_sla_seconds">>, Env)
    }.

%% Extract SLA from plan
extract_sla(Plan) ->
    Tier = maps:get(<<"tier">>, Plan),
    case maps:get(<<"sla">>, Plan, undefined) of
        undefined -> default_sla(Tier);
        SLA ->
            #{
                <<"model">> => <<"flat-per-deployment">>,
                <<"availability_percentage">> => maps:get(<<"availability_percentage">>, SLA),
                <<"throughput_guarantee_req_s">> => maps:get(<<"throughput_guarantee_req_s">>, SLA),
                <<"failover_time_seconds">> => maps:get(<<"failover_time_seconds">>, SLA),
                <<"recovery_sla_minutes">> => maps:get(<<"recovery_sla_minutes">>, SLA)
            }
    end.

%% Default SLA values for each tier
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

%% Extract pricing model
extract_pricing(Plan) ->
    Pricing = maps:get(<<"pricing">>, Plan),
    #{
        <<"model">> => maps:get(<<"model">>, Pricing),
        <<"description">> => maps:get(<<"description">>, Pricing)
    }.

%% Extract evidence links
extract_evidence_links(Plan) ->
    Evidence = maps:get(<<"evidence">>, Plan),
    maps:filter(fun(_K, V) -> V =/= null end, Evidence).

%% Extract comparison indicators
extract_comparison_indicators(Plan) ->
    Compliance = maps:get(<<"compliance">>, Plan),
    #{
        <<"features_count">> => maps:get(<<"features_implemented">>, Compliance, 0),
        <<"security_level">> => maps:get(<<"security_level">>, Compliance)
    }.

%% Generate comparison matrix markdown
generate_comparison_matrix(Cwd, Plans) ->
    Rows = [
        format_row("Throughput (req/s)", Plans, fun extract_throughput/1),
        format_row("Concurrent Connections", Plans, fun extract_connections/1),
        format_row("P99 Latency (ms)", Plans, fun extract_latency/1),
        format_row("Failover SLA (sec)", Plans, fun extract_failover/1),
        format_row("Availability %", Plans, fun extract_availability/1),
        format_row("Audit Logging", Plans, fun extract_audit/1),
        format_row("FIPS 140-2", Plans, fun extract_fips/1),
        format_row("High Availability", Plans, fun extract_ha/1)
    ],

    Header = "# erlmcp Plan Comparison\n\n"
        "Comprehensive comparison of Team, Enterprise, and Government tiers.\n"
        "Auto-generated from plan specifications.\n\n"
        "| Metric | Team | Enterprise | Government |\n"
        "|--------|------|-----------|------------|\n",

    Table = Header ++ lists:concat(Rows),

    UpgradePath = "\n## Upgrade Path\n\n"
        "**Team → Enterprise**: Production-grade, load balancing, high availability.\n\n"
        "**Enterprise → Government**: FIPS 140-2, audit logging, encryption.\n\n",

    Content = Table ++ UpgradePath,

    OutDir = filename:join(Cwd, "dist/marketplace"),
    OutFile = filename:join(OutDir, "plan-comparison.md"),
    file:write_file(OutFile, Content),
    io:format("  Generated ~s~n", [OutFile]).

%% Format comparison matrix row
format_row(Label, Plans, Extractor) ->
    [Team, Enterprise, Gov] = lists:map(Extractor, Plans),
    io_lib:format("| ~s | ~s | ~s | ~s |~n", [Label, Team, Enterprise, Gov]).

%% Extractors for comparison matrix
extract_throughput(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    integer_to_list(maps:get(<<"throughput_req_s">>, Env)).

extract_connections(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    integer_to_list(maps:get(<<"concurrent_connections">>, Env)).

extract_latency(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    integer_to_list(maps:get(<<"p99_latency_ms">>, Env)).

extract_failover(Plan) ->
    Env = maps:get(<<"envelope">>, Plan),
    integer_to_list(maps:get(<<"failover_sla_seconds">>, Env)).

extract_availability(Plan) ->
    SLA = extract_sla(Plan),
    float_to_list(maps:get(<<"availability_percentage">>, SLA), [{decimals, 2}]).

extract_audit(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"audit_logging">>, Features) of
        true -> "Yes";
        false -> "No"
    end.

extract_fips(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"fips_140_2">>, Features) of
        true -> "Yes";
        false -> "No"
    end.

extract_ha(Plan) ->
    Features = maps:get(<<"features">>, Plan),
    case maps:get(<<"high_availability">>, Features) of
        true -> "Yes";
        false -> "No"
    end.

%% Generate portal metadata
generate_portal_metadata(Cwd, _Plans) ->
    Metadata = #{
        <<"version">> => <<"1.0.0">>,
        <<"generated_at">> => erlang:list_to_binary(iso8601_now()),
        <<"erlmcp_version">> => <<"1.4.0">>,
        <<"plans_count">> => 3,
        <<"deterministic">> => true,
        <<"schema_version">> => <<"1.0.0">>,
        <<"portal_url">> => <<"https://erlmcp.dev/pricing">>
    },

    OutDir = filename:join(Cwd, "dist/marketplace"),
    OutFile = filename:join(OutDir, "portal-metadata.json"),
    file:write_file(OutFile, [jsx:encode(Metadata), "\n"]),
    io:format("  Generated ~s~n", [OutFile]).

%% Generate HTML portal template
generate_html_portal(Cwd) ->
    HtmlTemplate = html_content(),
    OutDir = filename:join(Cwd, "templates"),
    OutFile = filename:join(OutDir, "pricing_portal.html"),
    file:write_file(OutFile, HtmlTemplate),
    io:format("  Generated ~s~n", [OutFile]).

%% HTML portal template content
html_content() ->
    "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
  <title>erlmcp Pricing Portal</title>
  <style>
    * { margin: 0; padding: 0; box-sizing: border-box; }
    body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); min-height: 100vh; padding: 40px 20px; }
    .container { max-width: 1200px; margin: 0 auto; }
    .header { text-align: center; color: white; margin-bottom: 60px; }
    .header h1 { font-size: 48px; margin-bottom: 10px; }
    .header p { font-size: 20px; opacity: 0.9; }
    .plans-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 30px; margin-bottom: 60px; }
    .plan-card { background: white; border-radius: 12px; padding: 40px 30px; box-shadow: 0 20px 60px rgba(0,0,0,0.3); transition: transform 0.3s ease, box-shadow 0.3s ease; }
    .plan-card:hover { transform: translateY(-10px); box-shadow: 0 30px 80px rgba(0,0,0,0.4); }
    .plan-name { font-size: 24px; font-weight: 700; margin-bottom: 12px; color: #333; }
    .plan-description { font-size: 14px; color: #666; margin-bottom: 20px; line-height: 1.6; }
    .envelope-summary { background: #f5f5f5; border-radius: 8px; padding: 16px; margin-bottom: 20px; font-size: 13px; }
    .envelope-item { display: flex; justify-content: space-between; padding: 6px 0; border-bottom: 1px solid #e0e0e0; }
    .envelope-item:last-child { border-bottom: none; }
    .envelope-label { font-weight: 600; color: #333; }
    .envelope-value { color: #667eea; font-weight: 700; }
    .button-group { display: flex; gap: 10px; margin-top: 20px; }
    .btn { flex: 1; padding: 12px 16px; border: none; border-radius: 6px; cursor: pointer; font-weight: 600; font-size: 13px; }
    .btn-primary { background: #667eea; color: white; }
    .btn-primary:hover { background: #5568d3; }
    .btn-secondary { background: #f0f0f0; color: #333; border: 1px solid #e0e0e0; }
    .btn-secondary:hover { background: #e8e8e8; }
    .loading { text-align: center; color: white; font-size: 18px; }
  </style>
</head>
<body>
  <div class=\"container\">
    <div class=\"header\">
      <h1>erlmcp Pricing</h1>
      <p>Simple, transparent, flat-rate pricing. No metering, no surprises.</p>
    </div>
    <div id=\"content\" class=\"loading\">Loading plans...</div>
  </div>

  <script>
    async function loadPlans() {
      try {
        const response = await fetch('/dist/marketplace/plans.json');
        if (!response.ok) throw new Error('Failed to load plans');
        const data = await response.json();
        renderPortal(data);
      } catch (error) {
        document.getElementById('content').innerHTML = `<div style=\"color: white; text-align: center;\">Error loading plans: \${error.message}</div>`;
      }
    }

    function renderPortal(data) {
      const plans = data.plans || [];
      const html = `
        <div class=\"plans-grid\">
          \${plans.map(plan => `
            <div class=\"plan-card\">
              <div class=\"plan-name\">\${escapeHtml(plan.name)}</div>
              <div class=\"plan-description\">\${escapeHtml(plan.description)}</div>
              <div class=\"envelope-summary\">
                <div class=\"envelope-item\">
                  <span class=\"envelope-label\">Throughput</span>
                  <span class=\"envelope-value\">\${plan.envelope.throughput_req_s.toLocaleString()} req/s</span>
                </div>
                <div class=\"envelope-item\">
                  <span class=\"envelope-label\">Concurrent</span>
                  <span class=\"envelope-value\">\${plan.envelope.concurrent_connections.toLocaleString()}</span>
                </div>
                <div class=\"envelope-item\">
                  <span class=\"envelope-label\">P99 Latency</span>
                  <span class=\"envelope-value\">\${plan.envelope.p99_latency_ms}ms</span>
                </div>
                <div class=\"envelope-item\">
                  <span class=\"envelope-label\">Failover SLA</span>
                  <span class=\"envelope-value\">\${plan.envelope.failover_sla_seconds}s</span>
                </div>
              </div>
              <div class=\"button-group\">
                <button class=\"btn btn-primary\" onclick=\"alert('View details for ' + '\${plan.id}')\">Details</button>
                <button class=\"btn btn-secondary\" onclick=\"alert('View evidence for ' + '\${plan.id}')\">Evidence</button>
              </div>
            </div>
          `).join('')}
        </div>
      `;
      document.getElementById('content').innerHTML = html;
    }

    function escapeHtml(text) {
      const map = {'&': '&amp;', '<': '&lt;', '>': '&gt;', '\"': '&quot;', \"'\": '&#039;'};
      return text.replace(/[&<>\"']/g, m => map[m]);
    }

    document.addEventListener('DOMContentLoaded', loadPlans);
  </script>
</body>
</html>".

%% Generate ISO8601 timestamp
iso8601_now() ->
    {Date, Time} = erlang:localtime(),
    {Y, M, D} = Date,
    {H, Min, S} = Time,
    lists:flatten(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0bZ",
        [Y, M, D, H, Min, S])).
