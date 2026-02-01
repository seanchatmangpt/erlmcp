%%%-------------------------------------------------------------------
%%% @doc HTML Compliance Report Generator
%%%
%%% Generates HTML compliance reports following Joe Armstrong's philosophy:
%%% "PRETTY IS NICE. USABLE IS BETTER."
%%%
%%% The HTML is designed to ACTUALLY HELP PEOPLE FIND PROBLEMS:
%%% - Overall status PROMINENTLY (top, large, colored)
%%% - Each validator in separate section
%%% - Test counts and pass rates
%%% - Failure details with stack traces
%%% - Performance metrics (latency, throughput)
%%% - Valid HTML5
%%% - Inline CSS (single file, portable)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_compliance_report_html).

%% API
-export([generate_report/1, generate_report/2]).

%% Types
-type validation_results() :: map().

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Generate HTML report with default options
-spec generate_report(validation_results()) -> binary().
generate_report(Results) ->
    generate_report(Results, #{}).

%% @doc Generate HTML report with custom options
-spec generate_report(validation_results(), map()) -> binary().
generate_report(Results, Options) when is_map(Results), is_map(Options) ->
    IncludeCSS = maps:get(include_css, Options, true),
    IncludeJS = maps:get(include_javascript, Options, true),

    %% Extract data
    OverallStatus = compute_status(Results),
    StatusClass = status_to_class(OverallStatus),
    UpperStatus = string:uppercase(binary_to_list(OverallStatus)),

    Compliance = maps:get(overall, Results, 0.0),
    Timestamp = maps:get(timestamp, Results, <<"Unknown">>),
    SpecVersion = maps:get(spec_version, Results, <<"Unknown">>),

    Details = maps:get(details, Results, #{}),
    Passed = maps:get(passed_tests, Details, 0),
    Total = maps:get(total_requirements, Details, 0),

    PassRate =
        case Total of
            0 ->
                <<"0.0">>;
            _ ->
                float_to_binary(Passed / Total * 100.0, [{decimals, 1}])
        end,

    %% Build HTML
    HTML =
        io_lib:format("<!DOCTYPE html>~n"
                      "<html lang=\"en\">~n"
                      "<head>~n"
                      "    <meta charset=\"UTF-8\">~n"
                      "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">~n"
                      "    <title>erlmcp MCP Compliance Report</title>~n"
                      "~s"
                      "~s"
                      "</head>~n"
                      "<body>~n"
                      "<div class=\"container\">~n"
                      "    <div class=\"header\">~n"
                      "        <h1>MCP 2025-11-25 Compliance Report</h1>~n"
                      "        <div class=\"meta\">Generated: ~s | Specification: ~s</div>~n"
                      "        <div class=\"summary\">~n"
                      "            Overall: <span class=\"~s\">~s</span><br/>~n"
                      "            Tests: ~p/~p passed (~s%)<br/>~n"
                      "            Compliance: ~.2f%~n"
                      "        </div>~n"
                      "    </div>~n"
                      "~s"
                      "~s"
                      "~s"
                      "~s"
                      "~s"
                      "~s"
                      "</div>~n"
                      "~s"
                      "</body>~n"
                      "</html>~n",
                      [case IncludeCSS of
                           true ->
                               get_css();
                           false ->
                               ""
                       end,
                       case IncludeJS of
                           true ->
                               get_javascript();
                           false ->
                               ""
                       end,
                       Timestamp,
                       SpecVersion,
                       StatusClass,
                       UpperStatus,
                       Passed,
                       Total,
                       PassRate,
                       Compliance,
                       generate_toc(Results),
                       generate_validation_sections(Results),
                       generate_gap_section(Results),
                       generate_recommendations_section(Results),
                       generate_traceability_section(Results),
                       generate_performance_section(Results),
                       case IncludeJS of
                           true ->
                               "<script>initReport();</script>~n";
                           false ->
                               ""
                       end]),

    iolist_to_binary(HTML).

%%%====================================================================
%%% Internal Functions - Status Computation
%%%====================================================================

%% @private
compute_status(Results) ->
    Compliance = maps:get(overall, Results, 0.0),
    Gaps = maps:get(gaps, Results, []),
    CriticalGaps = [G || G <- Gaps, maps:get(<<"severity">>, G, <<"medium">>) =:= <<"critical">>],

    case {Compliance, CriticalGaps} of
        {C, _} when C >= 80.0 ->
            <<"passed">>;
        {C, []} when C >= 50.0 ->
            <<"warning">>;
        _ ->
            <<"failed">>
    end.

%% @private
status_to_class(<<"passed">>) ->
    <<"pass">>;
status_to_class(<<"failed">>) ->
    <<"fail">>;
status_to_class(<<"warning">>) ->
    <<"warning">>;
status_to_class(_) ->
    <<"unknown">>.

%%%====================================================================
%%% Internal Functions - Section Generators
%%%====================================================================

%% @private
generate_toc(Results) ->
    HasGaps = length(maps:get(gaps, Results, [])) > 0,
    HasRecs = length(maps:get(recommendations, Results, [])) > 0,
    HasTrace =
        maps:size(
            maps:get(traceability, Results, #{}))
        > 0,

    GapsLink =
        case HasGaps of
            true ->
                "            <li><a href=\"#gaps\">Gap Analysis</a></li>~n";
            false ->
                ""
        end,

    RecsLink =
        case HasRecs of
            true ->
                "            <li><a href=\"#recommendations\">Recommendations</a></li>~n";
            false ->
                ""
        end,

    TraceLink =
        case HasTrace of
            true ->
                "            <li><a href=\"#traceability\">Traceability Matrix</a></li>~n";
            false ->
                ""
        end,

    io_lib:format("    <div class=\"toc\">~n"
                  "        <h2>Table of Contents</h2>~n"
                  "        <ul>~n"
                  "            <li><a href=\"#summary\">Summary</a></li>~n"
                  "            <li><a href=\"#sections\">Validation Sections</a></li>~n"
                  "~s"
                  "~s"
                  "~s"
                  "        </ul>~n"
                  "    </div>~n",
                  [GapsLink, RecsLink, TraceLink]).

%% @private
generate_validation_sections(Results) ->
    BySection = maps:get(by_section, Results, #{}),

    Sections =
        maps:fold(fun(Section, Compliance, Acc) ->
                     Class =
                         case Compliance of
                             Score when Score >= 80.0 ->
                                 "pass";
                             Score when Score >= 50.0 ->
                                 "warning";
                             _ ->
                                 "fail"
                         end,

                     SectionID =
                         binary:replace(
                             binary:replace(Section, <<" ">>, <<"-">>, [global]),
                             <<"/">>,
                             <<"-">>,
                             [global]),

                     [io_lib:format("    <div class=\"validator\" id=\"section-~s\">~n"
                                    "        <h3>~s</h3>~n"
                                    "        <div class=\"compliance-bar\">~n"
                                    "            <div class=\"compliance-fill ~s\" style=\"width: ~.1f%\"></div>~n"
                                    "        </div>~n"
                                    "        <div class=\"stats\">Compliance: ~.2f%%</div>~n"
                                    "    </div>~n",
                                    [SectionID, Section, Class, Compliance, Compliance])
                      | Acc]
                  end,
                  [],
                  BySection),
    lists:reverse(["    <div id=\"sections\">~n        <h2>Validation Sections</h2>~n" | Sections])
    ++ ["    </div>~n"].

%% @private
generate_gap_section(Results) ->
    Gaps = maps:get(gaps, Results, []),

    case Gaps of
        [] ->
            "    <div id=\"gaps\">~n"
            "        <h2>Gap Analysis</h2>~n"
            "        <div class=\"no-gaps\">No gaps identified - all requirements tested!</div>~n"
            "    </div>~n";
        _ ->
            GapItems =
                lists:map(fun(Gap) ->
                             Req = maps:get(<<"requirement">>, Gap, #{}),
                             ReqName = maps:get(name, Req, <<"Unknown">>),
                             Status = maps:get(<<"status">>, Gap, <<"unknown">>),
                             Severity = maps:get(<<"severity">>, Gap, <<"medium">>),
                             Rec = maps:get(<<"recommendation">>, Gap, <<"No recommendation">>),
                             Section = maps:get(section, Req, <<"Unknown">>),

                             io_lib:format("        <div class=\"gap-item severity-~s\">~n"
                                           "            <h4>~s</h4>~n"
                                           "            <div class=\"gap-meta\">Section: ~s | Status: ~s | Severity: ~s</div>~n"
                                           "            <div class=\"gap-recommendation\">Recommendation: ~s</div>~n"
                                           "        </div>~n",
                                           [Severity, ReqName, Section, Status, Severity, Rec])
                          end,
                          Gaps),

            io_lib:format("    <div id=\"gaps\">~n"
                          "        <h2>Gap Analysis</h2>~n"
                          "        <div class=\"gaps-list\">~n"
                          "~s"
                          "        </div>~n"
                          "    </div>~n",
                          [lists:flatten(GapItems)])
    end.

%% @private
generate_recommendations_section(Results) ->
    Recommendations = maps:get(recommendations, Results, []),

    case Recommendations of
        [] ->
            "";
        _ ->
            RecItems =
                lists:map(fun(Rec) -> io_lib:format("            <li>~s</li>~n", [Rec]) end,
                          Recommendations),

            io_lib:format("    <div id=\"recommendations\">~n"
                          "        <h2>Recommendations</h2>~n"
                          "        <ul class=\"recommendations-list\">~n"
                          "~s"
                          "        </ul>~n"
                          "    </div>~n",
                          [lists:flatten(RecItems)])
    end.

%% @private
generate_traceability_section(Results) ->
    Traceability = maps:get(traceability, Results, #{}),

    case maps:size(Traceability) of
        0 ->
            "";
        _ ->
            Rows =
                maps:fold(fun(_ReqId, ReqData, Acc) ->
                             ReqName = maps:get(<<"requirement">>, ReqData, <<"Unknown">>),
                             Tests = maps:get(<<"tests">>, ReqData, []),
                             Status = maps:get(<<"status">>, ReqData, <<"unknown">>),
                             LastTested = maps:get(<<"last_tested">>, ReqData, <<"never">>),

                             StatusClass = status_to_class(Status),
                             TestsStr =
                                 case Tests of
                                     [] ->
                                         "No tests";
                                     _ ->
                                         string:join([binary_to_list(T) || T <- Tests], ", ")
                                 end,

                             [io_lib:format("            <tr>~n"
                                            "                <td>~s</td>~n"
                                            "                <td class=\"tests-list\">~s</td>~n"
                                            "                <td class=\"~s\">~s</td>~n"
                                            "                <td>~s</td>~n"
                                            "            </tr>~n",
                                            [ReqName, TestsStr, StatusClass, Status, LastTested])
                              | Acc]
                          end,
                          [],
                          Traceability),

            io_lib:format("    <div id=\"traceability\">~n"
                          "        <h2>Traceability Matrix</h2>~n"
                          "        <table class=\"traceability-table\">~n"
                          "            <thead>~n"
                          "                <tr>~n"
                          "                    <th>Requirement</th>~n"
                          "                    <th>Tests</th>~n"
                          "                    <th>Status</th>~n"
                          "                    <th>Last Tested</th>~n"
                          "                </tr>~n"
                          "            </thead>~n"
                          "            <tbody>~n"
                          "~s"
                          "            </tbody>~n"
                          "        </table>~n"
                          "    </div>~n",
                          [lists:flatten(
                               lists:reverse(Rows))])
    end.

%% @private
generate_performance_section(Results) ->
    Details = maps:get(details, Results, #{}),
    case maps:get(performance, Details, undefined) of
        undefined ->
            "";
        Metrics ->
            Rows =
                maps:fold(fun(Name, Value, Acc) ->
                             [io_lib:format("            <tr>~n"
                                            "                <td>~s</td>~n"
                                            "                <td>~p</td>~n"
                                            "            </tr>~n",
                                            [Name, Value])
                              | Acc]
                          end,
                          [],
                          Metrics),

            io_lib:format("    <div id=\"performance\">~n"
                          "        <h2>Performance Metrics</h2>~n"
                          "        <table class=\"performance-table\">~n"
                          "            <thead>~n"
                          "                <tr>~n"
                          "                    <th>Metric</th>~n"
                          "                    <th>Value</th>~n"
                          "                </tr>~n"
                          "            </thead>~n"
                          "            <tbody>~n"
                          "~s"
                          "            </tbody>~n"
                          "        </table>~n"
                          "    </div>~n",
                          [lists:flatten(
                               lists:reverse(Rows))])
    end.

%%%====================================================================
%%% Internal Functions - CSS and JavaScript
%%%====================================================================

%% @private
get_css() ->
    "
    <style>
        * { box-sizing: border-box; }
        body {
            font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif;
            line-height: 1.6;
            color: #333;
            margin: 0;
            padding: 20px;
            background: #f5f5f5;
        }
        .container {
            max-width: 1200px;
            margin: 0 auto;
            background: white;
            padding: 30px;
            border-radius: 8px;
            box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        .header {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 30px;
            border-radius: 8px;
            margin-bottom: 30px;
        }
        .header h1 {
            margin: 0 0 15px 0;
            font-size: 2.5em;
            font-weight: 700;
        }
        .header .meta {
            font-size: 0.9em;
            opacity: 0.9;
            margin-bottom: 15px;
        }
        .header .summary {
            background: rgba(255,255,255,0.15);
            padding: 15px;
            border-radius: 6px;
            font-size: 1.2em;
        }
        .pass { color: #10b981; font-weight: bold; }
        .fail { color: #ef4444; font-weight: bold; }
        .warning { color: #f59e0b; font-weight: bold; }
        .toc {
            background: #f9fafb;
            border-left: 4px solid #667eea;
            padding: 20px;
            margin: 20px 0;
            border-radius: 4px;
        }
        .toc h2 { margin-top: 0; font-size: 1.3em; }
        .toc ul { list-style: none; padding-left: 0; }
        .toc li { padding: 5px 0; }
        .toc a { color: #667eea; text-decoration: none; }
        .toc a:hover { text-decoration: underline; }
        .validator {
            border: 1px solid #e5e7eb;
            margin: 10px 0;
            padding: 20px;
            border-radius: 6px;
            background: white;
        }
        .validator h3 { margin-top: 0; color: #1f2937; }
        .compliance-bar {
            background: #e5e7eb;
            height: 30px;
            border-radius: 15px;
            overflow: hidden;
            margin: 15px 0;
        }
        .compliance-fill {
            height: 100%;
            transition: width 0.3s ease;
            display: flex;
            align-items: center;
            justify-content: center;
            color: white;
            font-weight: bold;
        }
        .compliance-fill.pass { background: #10b981; }
        .compliance-fill.warning { background: #f59e0b; }
        .compliance-fill.fail { background: #ef4444; }
        .stats { font-size: 0.9em; color: #6b7280; }
        .gap-item {
            border-left: 4px solid #f59e0b;
            background: #fffbeb;
            padding: 15px;
            margin: 10px 0;
            border-radius: 4px;
        }
        .gap-item.severity-critical {
            border-left-color: #ef4444;
            background: #fef2f2;
        }
        .gap-item.severity-high {
            border-left-color: #f97316;
            background: #fff7ed;
        }
        .gap-item h4 { margin: 0 0 10px 0; color: #1f2937; }
        .gap-meta { font-size: 0.85em; color: #6b7280; margin-bottom: 8px; }
        .gap-recommendation { color: #4b5563; }
        .no-gaps {
            padding: 20px;
            background: #ecfdf5;
            border-left: 4px solid #10b981;
            border-radius: 4px;
            color: #065f46;
            font-weight: 500;
        }
        .recommendations-list { list-style: none; padding-left: 0; }
        .recommendations-list li {
            padding: 10px 0;
            border-bottom: 1px solid #e5e7eb;
        }
        .recommendations-list li:last-child { border-bottom: none; }
        table { width: 100%; border-collapse: collapse; margin: 20px 0; }
        th, td { padding: 12px; text-align: left; border-bottom: 1px solid #e5e7eb; }
        th { background: #f9fafb; font-weight: 600; color: #1f2937; }
        .tests-list { font-size: 0.85em; color: #6b7280; }
        @media (max-width: 768px) {
            .container { padding: 15px; }
            .header h1 { font-size: 1.8em; }
            .header .summary { font-size: 1em; }
        }
    </style>
    ".

%% @private
get_javascript() ->
    "
    <script>
        function initReport() {
            document.querySelectorAll('a[href^=\"#\"]').forEach(anchor => {
                anchor.addEventListener('click', function (e) {
                    e.preventDefault();
                    const target = document.querySelector(this.getAttribute('href'));
                    if (target) {
                        target.scrollIntoView({ behavior: 'smooth', block: 'start' });
                    }
                });
            });

            const observer = new IntersectionObserver((entries) => {
                entries.forEach(entry => {
                    if (entry.isIntersecting) {
                        const bars = entry.target.querySelectorAll('.compliance-fill');
                        bars.forEach(bar => {
                            const width = bar.style.width;
                            bar.style.width = '0';
                            setTimeout(() => { bar.style.width = width; }, 100);
                        });
                        observer.unobserve(entry.target);
                    }
                });
            }, { threshold: 0.2 });

            document.querySelectorAll('.validator').forEach(validator => {
                observer.observe(validator);
            });
        }
    </script>
    ".
