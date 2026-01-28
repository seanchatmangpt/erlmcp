%%%-------------------------------------------------------------------
%%% @doc TCPS How-to Guide Recipe Library
%%%
%%% Complete library of task-oriented how-to guides for TCPS operations.
%%% Each guide follows the Problem → Solution → Verification pattern.
%%%
%%% Categories:
%%% - Quality Gates: Setting up and managing quality controls
%%% - Kanban: Configuring workflow management
%%% - Andon: Responding to alerts and issues
%%% - Analysis: Performing root cause analysis
%%% - Production: Implementing production leveling
%%% - Receipts: Managing receipt chains
%%% - Monitoring: Observing system metrics
%%% - Integration: Connecting TCPS to CI/CD
%%% - Debugging: Troubleshooting failures
%%% - Performance: Optimizing system performance
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_howto_recipes).

%% API
-export([
    load_all_guides/0,
    get_quality_gates_guide/0,
    get_kanban_config_guide/0,
    get_andon_response_guide/0,
    get_five_whys_guide/0,
    get_heijunka_guide/0,
    get_receipt_chain_guide/0,
    get_monitoring_guide/0,
    get_cicd_integration_guide/0,
    get_debugging_guide/0,
    get_performance_guide/0,
    get_takt_time_guide/0,
    get_jidoka_guide/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec load_all_guides() -> #{atom() => map()}.
load_all_guides() ->
    #{
        quality_gates => get_quality_gates_guide(),
        kanban_config => get_kanban_config_guide(),
        andon_response => get_andon_response_guide(),
        five_whys => get_five_whys_guide(),
        heijunka => get_heijunka_guide(),
        receipt_chain => get_receipt_chain_guide(),
        monitoring => get_monitoring_guide(),
        cicd_integration => get_cicd_integration_guide(),
        debugging => get_debugging_guide(),
        performance => get_performance_guide(),
        takt_time => get_takt_time_guide(),
        jidoka => get_jidoka_guide()
    }.

%%%===================================================================
%%% Guide 1: How to Set Up Quality Gates
%%%===================================================================

-spec get_quality_gates_guide() -> map().
get_quality_gates_guide() ->
    #{
        id => quality_gates,
        title => <<"How to Set Up Quality Gates for Your Project">>,
        category => quality_gates,
        difficulty => beginner,
        duration => 20,
        problem => <<"You need to enforce quality standards and prevent defects from "
                     "entering your codebase. Quality gates automatically check code "
                     "against defined criteria before allowing it to proceed.">>,
        prerequisites => [
            <<"TCPS MCP server installed and running">>,
            <<"Basic understanding of your project's quality requirements">>,
            <<"Access to project configuration files">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Define quality criteria for your project">>,
                command => <<"tcps_quality_gates:define_criteria([\n"
                            "  {test_coverage, 80},\n"
                            "  {lint_errors, 0},\n"
                            "  {type_coverage, 100},\n"
                            "  {security_issues, 0}\n"
                            "]).">>,
                expected_output => <<"{ok, criteria_defined, 4}">>,
                verification => <<"Criteria should be stored in TCPS configuration">>,
                notes => [
                    <<"Start with realistic thresholds based on current state">>,
                    <<"Gradually increase standards over time">>,
                    <<"Test coverage below 80% indicates insufficient testing">>
                ]
            },
            #{
                number => 2,
                description => <<"Configure gate enforcement points">>,
                command => <<"tcps_quality_gates:configure_enforcement([\n"
                            "  {on_commit, true},\n"
                            "  {on_pr, true},\n"
                            "  {on_merge, true},\n"
                            "  {block_on_failure, true}\n"
                            "]).">>,
                expected_output => <<"{ok, enforcement_configured}">>,
                verification => <<"Gates should trigger at specified points">>,
                notes => [
                    <<"Blocking on failure prevents defects from propagating">>,
                    <<"Consider team readiness before enabling blocking">>
                ]
            },
            #{
                number => 3,
                description => <<"Set up automatic quality checks">>,
                command => <<"tcps_quality_gates:add_checks([\n"
                            "  {run_tests, \"rebar3 eunit\"},\n"
                            "  {check_coverage, \"rebar3 cover\"},\n"
                            "  {lint_code, \"rebar3 lint\"},\n"
                            "  {type_check, \"rebar3 dialyzer\"}\n"
                            "]).">>,
                expected_output => <<"{ok, checks_added, 4}">>,
                verification => <<"Each check should execute successfully">>,
                notes => [
                    <<"Use project-specific tool commands">>,
                    <<"Ensure all tools are installed and configured">>,
                    <<"Test each command independently first">>
                ]
            },
            #{
                number => 4,
                description => <<"Configure notification channels">>,
                command => <<"tcps_quality_gates:setup_notifications([\n"
                            "  {slack_webhook, \"https://hooks.slack.com/...\"},\n"
                            "  {email_list, [\"team@example.com\"]},\n"
                            "  {notify_on_failure, true},\n"
                            "  {notify_on_success, false}\n"
                            "]).">>,
                expected_output => <<"{ok, notifications_configured}">>,
                verification => <<"Test notification by triggering a gate">>,
                notes => [
                    <<"Only notify on failures to reduce noise">>,
                    <<"Include detailed error information in notifications">>
                ]
            },
            #{
                number => 5,
                description => <<"Test quality gates with sample code">>,
                command => <<"tcps_quality_gates:run_gate_check(test_commit).">>,
                expected_output => <<"{ok, all_checks_passed, [\n"
                                    "  {test_coverage, 85, passed},\n"
                                    "  {lint_errors, 0, passed},\n"
                                    "  {type_coverage, 100, passed},\n"
                                    "  {security_issues, 0, passed}\n"
                                    "]}">>,
                verification => <<"All criteria should pass or fail as expected">>,
                notes => [
                    <<"Test both passing and failing scenarios">>,
                    <<"Verify blocking behavior works correctly">>
                ]
            }
        ],
        verification => <<"Quality gates are properly configured when:\n"
                         "1. All criteria are defined with appropriate thresholds\n"
                         "2. Enforcement points trigger checks automatically\n"
                         "3. Failing checks block code progression\n"
                         "4. Team receives timely notifications\n"
                         "5. Gates integrate smoothly with existing workflow">>,
        common_pitfalls => [
            <<"Setting thresholds too high initially - Start realistic, increase gradually">>,
            <<"Not testing notification channels - Verify alerts reach the team">>,
            <<"Forgetting to install quality tools - Ensure all tools are available">>,
            <<"Blocking without team buy-in - Communicate standards clearly">>,
            <<"No escape hatch for emergencies - Provide override mechanism with audit trail">>
        ],
        related_guides => [monitoring, debugging, cicd_integration],
        tags => [<<"quality">>, <<"gates">>, <<"automation">>, <<"testing">>, <<"standards">>]
    }.

%%%===================================================================
%%% Guide 2: How to Configure Kanban Buckets and WIP Limits
%%%===================================================================

-spec get_kanban_config_guide() -> map().
get_kanban_config_guide() ->
    #{
        id => kanban_config,
        title => <<"How to Configure Kanban Buckets and WIP Limits">>,
        category => kanban,
        difficulty => beginner,
        duration => 15,
        problem => <<"Your team is experiencing work bottlenecks and context switching. "
                     "Kanban buckets organize work stages and WIP limits prevent overload.">>,
        prerequisites => [
            <<"Understanding of your team's workflow stages">>,
            <<"Knowledge of team capacity">>,
            <<"TCPS Kanban module enabled">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Define workflow buckets (columns)">>,
                command => <<"tcps_kanban:define_buckets([\n"
                            "  {backlog, #{wip_limit => unlimited}},\n"
                            "  {ready, #{wip_limit => 10}},\n"
                            "  {in_progress, #{wip_limit => 5}},\n"
                            "  {review, #{wip_limit => 3}},\n"
                            "  {done, #{wip_limit => unlimited}}\n"
                            "]).">>,
                expected_output => <<"{ok, buckets_defined, 5}">>,
                verification => <<"Buckets appear in TCPS Kanban board">>,
                notes => [
                    <<"Model buckets after your actual workflow">>,
                    <<"Keep bucket names simple and clear">>,
                    <<"Backlog and Done typically have no WIP limits">>
                ]
            },
            #{
                number => 2,
                description => <<"Calculate appropriate WIP limits">>,
                command => <<"WIPLimit = TeamSize * 1.5,\n"
                            "InProgressLimit = round(WIPLimit / 2),\n"
                            "ReviewLimit = round(WIPLimit / 3).">>,
                expected_output => <<"For team of 4: InProgress=3, Review=2">>,
                verification => <<"Limits should prevent overload while maintaining flow">>,
                notes => [
                    <<"Rule of thumb: 1.5x team size for total WIP">>,
                    <<"Adjust based on observed cycle times">>,
                    <<"Lower limits increase focus but may idle resources">>
                ]
            },
            #{
                number => 3,
                description => <<"Apply WIP limits to buckets">>,
                command => <<"tcps_kanban:set_wip_limits([\n"
                            "  {in_progress, 3},\n"
                            "  {review, 2},\n"
                            "  {ready, 8}\n"
                            "]).">>,
                expected_output => <<"{ok, wip_limits_applied}">>,
                verification => <<"System should enforce limits on new work items">>,
                notes => [
                    <<"Start conservative, relax limits if needed">>,
                    <<"Monitor for idle workers or bottlenecks">>
                ]
            },
            #{
                number => 4,
                description => <<"Configure WIP limit policies">>,
                command => <<"tcps_kanban:configure_policies([\n"
                            "  {strict_enforcement, true},\n"
                            "  {alert_on_limit, true},\n"
                            "  {allow_priority_override, false},\n"
                            "  {aging_threshold_days, 3}\n"
                            "]).">>,
                expected_output => <<"{ok, policies_configured}">>,
                verification => <<"Policies should govern WIP limit behavior">>,
                notes => [
                    <<"Strict enforcement prevents limit violations">>,
                    <<"Alert when approaching limits">>,
                    <<"Track aging items to identify blockers">>
                ]
            },
            #{
                number => 5,
                description => <<"Test bucket transitions and limits">>,
                command => <<"tcps_kanban:move_item(item_1, backlog, ready),\n"
                            "tcps_kanban:move_item(item_1, ready, in_progress).">>,
                expected_output => <<"{ok, item_moved, in_progress}">>,
                verification => <<"Item moves between buckets respecting WIP limits">>,
                notes => [
                    <<"Verify limit enforcement blocks excess items">>,
                    <<"Test alert triggering near limits">>
                ]
            },
            #{
                number => 6,
                description => <<"Monitor flow metrics">>,
                command => <<"tcps_kanban:get_flow_metrics().">>,
                expected_output => <<"#{cycle_time => 2.3,\n"
                                    "  throughput => 12,\n"
                                    "  wip => 7,\n"
                                    "  utilization => 0.85}">>,
                verification => <<"Metrics should show balanced flow">>,
                notes => [
                    <<"Look for increasing cycle times (bottleneck indicator)">>,
                    <<"Aim for high throughput with stable WIP">>
                ]
            }
        ],
        verification => <<"Kanban is properly configured when:\n"
                         "1. Buckets match your actual workflow stages\n"
                         "2. WIP limits prevent overload while maintaining flow\n"
                         "3. System enforces limits consistently\n"
                         "4. Metrics show steady throughput and cycle time\n"
                         "5. Team understands and follows Kanban principles">>,
        common_pitfalls => [
            <<"Too many buckets - Keep it simple with 4-6 stages">>,
            <<"WIP limits too high - Defeats the purpose of limiting work">>,
            <<"WIP limits too low - Creates idle time and frustration">>,
            <<"Not measuring flow metrics - Can't improve what you don't measure">>,
            <<"Allowing too many exceptions - Undermines the system">>
        ],
        related_guides => [monitoring, heijunka, performance],
        tags => [<<"kanban">>, <<"wip">>, <<"flow">>, <<"lean">>, <<"workflow">>]
    }.

%%%===================================================================
%%% Guide 3: How to Respond to an Andon Alert
%%%===================================================================

-spec get_andon_response_guide() -> map().
get_andon_response_guide() ->
    #{
        id => andon_response,
        title => <<"How to Respond to an Andon Alert">>,
        category => andon,
        difficulty => beginner,
        duration => 10,
        problem => <<"An Andon alert has been triggered indicating a quality or process issue. "
                     "You need to respond quickly and systematically to resolve it.">>,
        prerequisites => [
            <<"TCPS Andon system configured">>,
            <<"Alert notification received">>,
            <<"Access to TCPS command interface">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Acknowledge the alert immediately">>,
                command => <<"tcps_andon:acknowledge_alert(AlertId).">>,
                expected_output => <<"{ok, alert_acknowledged, {by, UserId, Timestamp}}">>,
                verification => <<"Alert status changes to 'acknowledged'">>,
                notes => [
                    <<"Acknowledge within SLA time (typically 5 minutes)">>,
                    <<"Acknowledgment stops escalation timers">>,
                    <<"Multiple team members can see who acknowledged">>
                ]
            },
            #{
                number => 2,
                description => <<"Retrieve alert details and context">>,
                command => <<"tcps_andon:get_alert_details(AlertId).">>,
                expected_output => <<"#{type => quality_gate_failure,\n"
                                    "  severity => high,\n"
                                    "  source => commit_abc123,\n"
                                    "  failed_check => test_coverage,\n"
                                    "  threshold => 80,\n"
                                    "  actual => 65,\n"
                                    "  timestamp => {{2024,1,15},{14,30,0}}}">>,
                verification => <<"Alert details provide actionable information">>,
                notes => [
                    <<"Note the specific failure reason">>,
                    <<"Check severity to prioritize response">>,
                    <<"Review timestamp to assess urgency">>
                ]
            },
            #{
                number => 3,
                description => <<"Stop the line if severity is critical">>,
                command => <<"case Severity of\n"
                            "  critical -> tcps_andon:stop_line(production);\n"
                            "  high -> continue;\n"
                            "  medium -> continue\n"
                            "end.">>,
                expected_output => <<"{ok, line_stopped, production}">>,
                verification => <<"Production pipeline halts if critical">>,
                notes => [
                    <<"Critical alerts always stop the line">>,
                    <<"High/medium alerts may continue with monitoring">>,
                    <<"Stopping prevents defect propagation">>
                ]
            },
            #{
                number => 4,
                description => <<"Investigate root cause">>,
                command => <<"tcps_andon:get_alert_context(AlertId, [\n"
                            "  recent_changes,\n"
                            "  similar_alerts,\n"
                            "  metrics_trend\n"
                            "]).">>,
                expected_output => <<"#{recent_changes => [commit_abc123, commit_def456],\n"
                                    "  similar_alerts => [alert_789],\n"
                                    "  metrics_trend => declining}">>,
                verification => <<"Context helps identify root cause">>,
                notes => [
                    <<"Look for patterns in recent changes">>,
                    <<"Check if this is a recurring issue">>,
                    <<"Consider metric trends over time">>
                ]
            },
            #{
                number => 5,
                description => <<"Implement immediate fix or workaround">>,
                command => <<"tcps_andon:log_resolution_action(AlertId, [\n"
                            "  {action, revert_commit},\n"
                            "  {target, commit_abc123},\n"
                            "  {reason, \"Insufficient test coverage\"}\n"
                            "]).">>,
                expected_output => <<"{ok, action_logged}">>,
                verification => <<"Action is recorded in alert history">>,
                notes => [
                    <<"Quick fix stops the immediate problem">>,
                    <<"Document what you did">>,
                    <<"Plan for permanent solution separately">>
                ]
            },
            #{
                number => 6,
                description => <<"Verify fix and resume operations">>,
                command => <<"tcps_andon:verify_resolution(AlertId),\n"
                            "tcps_andon:resume_line(production).">>,
                expected_output => <<"{ok, alert_resolved},\n"
                                    "{ok, line_resumed, production}">>,
                verification => <<"System returns to normal operation">>,
                notes => [
                    <<"Run quality checks to confirm fix">>,
                    <<"Resume only after verification">>,
                    <<"Monitor for recurrence">>
                ]
            },
            #{
                number => 7,
                description => <<"Schedule root cause analysis">>,
                command => <<"tcps_andon:schedule_rca(AlertId, [\n"
                            "  {method, five_whys},\n"
                            "  {participants, [team_lead, dev_1, qa_1]},\n"
                            "  {scheduled_time, tomorrow_2pm}\n"
                            "]).">>,
                expected_output => <<"{ok, rca_scheduled}">>,
                verification => <<"RCA session appears in calendar">>,
                notes => [
                    <<"Don't skip RCA for recurring issues">>,
                    <<"Include diverse perspectives">>,
                    <<"Focus on prevention, not blame">>
                ]
            }
        ],
        verification => <<"Alert response is complete when:\n"
                         "1. Alert was acknowledged within SLA\n"
                         "2. Root cause was identified\n"
                         "3. Immediate fix was implemented\n"
                         "4. Fix was verified effective\n"
                         "5. Operations resumed safely\n"
                         "6. RCA scheduled for permanent solution\n"
                         "7. All actions documented in alert history">>,
        common_pitfalls => [
            <<"Ignoring low-severity alerts - Small issues compound into big problems">>,
            <<"Skipping acknowledgment - Causes alert escalation">>,
            <<"Resuming without verification - May reintroduce the problem">>,
            <<"No follow-up RCA - Issue will likely recur">>,
            <<"Poor documentation - Loses valuable learning opportunity">>
        ],
        related_guides => [five_whys, quality_gates, monitoring],
        tags => [<<"andon">>, <<"alerts">>, <<"incident">>, <<"response">>, <<"quality">>]
    }.

%%%===================================================================
%%% Guide 4: How to Conduct a 5 Whys Analysis
%%%===================================================================

-spec get_five_whys_guide() -> map().
get_five_whys_guide() ->
    #{
        id => five_whys,
        title => <<"How to Conduct a 5 Whys Analysis">>,
        category => analysis,
        difficulty => intermediate,
        duration => 30,
        problem => <<"You need to identify the root cause of a recurring problem. "
                     "The 5 Whys technique systematically drills down to find the true cause.">>,
        prerequisites => [
            <<"Clear problem statement">>,
            <<"Relevant stakeholders available">>,
            <<"TCPS analysis tools installed">>,
            <<"Data about the problem occurrence">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Define the problem clearly">>,
                command => <<"tcps_analysis:define_problem([\n"
                            "  {description, \"Test coverage dropped below 80%\"},\n"
                            "  {occurrence, \"3 times in last sprint\"},\n"
                            "  {impact, \"Quality gate failures\"},\n"
                            "  {affected_area, \"backend services\"}\n"
                            "]).">>,
                expected_output => <<"{ok, problem_defined, problem_id_123}">>,
                verification => <<"Problem is specific and measurable">>,
                notes => [
                    <<"Use concrete data, not opinions">>,
                    <<"Focus on one problem at a time">>,
                    <<"State impact in business terms">>
                ]
            },
            #{
                number => 2,
                description => <<"Ask the first Why - What caused this problem?">>,
                command => <<"tcps_analysis:ask_why(problem_id_123, 1, [\n"
                            "  {question, \"Why did test coverage drop?\"},\n"
                            "  {answer, \"New features added without tests\"}\n"
                            "]).">>,
                expected_output => <<"{ok, why_1_recorded}">>,
                verification => <<"Answer is based on facts, not assumptions">>,
                notes => [
                    <<"Verify answer with data">>,
                    <<"Avoid jumping to solutions">>,
                    <<"Stay focused on causation">>
                ]
            },
            #{
                number => 3,
                description => <<"Ask the second Why - What caused that?">>,
                command => <<"tcps_analysis:ask_why(problem_id_123, 2, [\n"
                            "  {question, \"Why were features added without tests?\"},\n"
                            "  {answer, \"Developers rushed to meet sprint deadline\"}\n"
                            "]).">>,
                expected_output => <<"{ok, why_2_recorded}">>,
                verification => <<"Answer digs deeper into cause">>,
                notes => [
                    <<"Each Why should reveal a deeper layer">>,
                    <<"If stuck, gather more information">>
                ]
            },
            #{
                number => 4,
                description => <<"Ask the third Why - What caused that?">>,
                command => <<"tcps_analysis:ask_why(problem_id_123, 3, [\n"
                            "  {question, \"Why did developers rush?\"},\n"
                            "  {answer, \"Too much work committed to sprint\"}\n"
                            "]).">>,
                expected_output => <<"{ok, why_3_recorded}">>,
                verification => <<"Answer reveals process or planning issue">>,
                notes => [
                    <<"Looking for systemic causes">>,
                    <<"People are not the root cause">>
                ]
            },
            #{
                number => 5,
                description => <<"Ask the fourth Why - What caused that?">>,
                command => <<"tcps_analysis:ask_why(problem_id_123, 4, [\n"
                            "  {question, \"Why was too much work committed?\"},\n"
                            "  {answer, \"Sprint planning didn't account for test time\"}\n"
                            "]).">>,
                expected_output => <<"{ok, why_4_recorded}">>,
                verification => <<"Answer identifies planning gap">>,
                notes => [
                    <<"Process gaps often appear at this level">>,
                    <<"Continue if not yet at root cause">>
                ]
            },
            #{
                number => 6,
                description => <<"Ask the fifth Why - What caused that?">>,
                command => <<"tcps_analysis:ask_why(problem_id_123, 5, [\n"
                            "  {question, \"Why wasn't test time accounted for?\"},\n"
                            "  {answer, \"No standard for test time estimation\"}\n"
                            "]).">>,
                expected_output => <<"{ok, why_5_recorded, root_cause_identified}">>,
                verification => <<"Root cause is a fixable systemic issue">>,
                notes => [
                    <<"May need fewer or more than 5 Whys">>,
                    <<"Stop when you reach a fixable root cause">>,
                    <<"Root cause is typically a process or system gap">>
                ]
            },
            #{
                number => 7,
                description => <<"Define corrective actions">>,
                command => <<"tcps_analysis:define_corrective_actions(problem_id_123, [\n"
                            "  {action, \"Create test time estimation standard\"},\n"
                            "  {action, \"Include test time in sprint planning\"},\n"
                            "  {action, \"Add test coverage to DoD checklist\"},\n"
                            "  {owner, team_lead},\n"
                            "  {deadline, two_weeks}\n"
                            "]).">>,
                expected_output => <<"{ok, actions_defined, 3}">>,
                verification => <<"Actions address the root cause">>,
                notes => [
                    <<"Actions should prevent recurrence">>,
                    <<"Assign clear ownership">>,
                    <<"Set realistic deadlines">>
                ]
            },
            #{
                number => 8,
                description => <<"Track action implementation">>,
                command => <<"tcps_analysis:track_actions(problem_id_123).">>,
                expected_output => <<"#{total_actions => 3,\n"
                                    "  completed => 0,\n"
                                    "  in_progress => 2,\n"
                                    "  not_started => 1}">>,
                verification => <<"Progress is visible and monitored">>,
                notes => [
                    <<"Review action status regularly">>,
                    <<"Verify effectiveness after implementation">>
                ]
            }
        ],
        verification => <<"5 Whys analysis is complete when:\n"
                         "1. Problem is clearly defined with data\n"
                         "2. Each Why question drills deeper\n"
                         "3. Root cause is identified (systemic, fixable)\n"
                         "4. Corrective actions target root cause\n"
                         "5. Actions have owners and deadlines\n"
                         "6. Implementation is tracked\n"
                         "7. Analysis is documented for learning">>,
        common_pitfalls => [
            <<"Stopping too early - Missing the true root cause">>,
            <<"Blaming people - Focus on process and systems">>,
            <<"Multiple root causes per Why - Keep focused on primary cause">>,
            <<"No data to support answers - Verify with facts">>,
            <<"Weak corrective actions - Must prevent recurrence">>,
            <<"No follow-up - Actions must be implemented and verified">>
        ],
        related_guides => [andon_response, quality_gates, monitoring],
        tags => [<<"analysis">>, <<"root-cause">>, <<"5-whys">>, <<"problem-solving">>, <<"lean">>]
    }.

%%%===================================================================
%%% Guide 5: How to Implement Heijunka Production Leveling
%%%===================================================================

-spec get_heijunka_guide() -> map().
get_heijunka_guide() ->
    #{
        id => heijunka,
        title => <<"How to Implement Heijunka Production Leveling">>,
        category => production,
        difficulty => advanced,
        duration => 45,
        problem => <<"Work arrives in unpredictable bursts causing resource bottlenecks and idle time. "
                     "Heijunka levels production volume and mix for smooth, predictable flow.">>,
        prerequisites => [
            <<"Historical work data (at least 4 weeks)">>,
            <<"Understanding of work types and mix">>,
            <<"TCPS Heijunka module enabled">>,
            <<"Team capacity analysis completed">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Analyze historical demand patterns">>,
                command => <<"tcps_heijunka:analyze_demand([\n"
                            "  {period_weeks, 8},\n"
                            "  {work_types, [feature, bug, tech_debt]},\n"
                            "  {metrics, [volume, variability, mix]}\n"
                            "]).">>,
                expected_output => <<"#{avg_weekly_volume => 20,\n"
                                    "  variability => 0.35,\n"
                                    "  work_mix => #{feature => 0.6, bug => 0.25, tech_debt => 0.15}}">>,
                verification => <<"Demand data shows patterns and variability">>,
                notes => [
                    <<"Need at least 4 weeks of data">>,
                    <<"High variability indicates need for leveling">>,
                    <<"Track work types separately">>
                ]
            },
            #{
                number => 2,
                description => <<"Calculate target daily production">>,
                command => <<"WeeklyVolume = 20,\n"
                            "DaysPerWeek = 5,\n"
                            "TargetDaily = WeeklyVolume / DaysPerWeek,\n"
                            "tcps_heijunka:set_target_rate(TargetDaily).">>,
                expected_output => <<"{ok, target_rate_set, 4.0}">>,
                verification => <<"Daily target is realistic and achievable">>,
                notes => [
                    <<"Start with average demand">>,
                    <<"Account for team capacity">>,
                    <<"Build in buffer for variability (10-20%)">>
                ]
            },
            #{
                number => 3,
                description => <<"Design heijunka box (scheduling board)">>,
                command => <<"tcps_heijunka:design_box([\n"
                            "  {time_slots_per_day, 8},\n"
                            "  {days_visible, 5},\n"
                            "  {work_types, [feature, bug, tech_debt]},\n"
                            "  {slot_duration_hours, 1}\n"
                            "]).">>,
                expected_output => <<"{ok, heijunka_box_created, {slots, 40}}">>,
                verification => <<"Box provides visual scheduling framework">>,
                notes => [
                    <<"Each slot represents a time period">>,
                    <<"Visualize one week ahead">>,
                    <<"Color-code by work type">>
                ]
            },
            #{
                number => 4,
                description => <<"Level work mix across time periods">>,
                command => <<"tcps_heijunka:level_work_mix([\n"
                            "  {daily_features, 2.4},  % 60% of 4 items\n"
                            "  {daily_bugs, 1.0},      % 25% of 4 items\n"
                            "  {daily_tech_debt, 0.6}  % 15% of 4 items\n"
                            "]).">>,
                expected_output => <<"{ok, mix_leveled, daily_schedule_created}">>,
                verification => <<"Each day has balanced mix of work types">>,
                notes => [
                    <<"Distribute work types evenly">>,
                    <<"Avoid batching same type">>,
                    <<"Maintains skill utilization">>
                ]
            },
            #{
                number => 5,
                description => <<"Implement pitch intervals">>,
                command => <<"tcps_heijunka:set_pitch_interval([\n"
                            "  {duration_hours, 2},\n"
                            "  {items_per_pitch, 1},\n"
                            "  {review_at_pitch_end, true}\n"
                            "]).">>,
                expected_output => <<"{ok, pitch_configured, {pitches_per_day, 4}}">>,
                verification => <<"Work is released in regular intervals">>,
                notes => [
                    <<"Pitch = takt time × pack quantity">>,
                    <<"Regular intervals enable rhythm">>,
                    <<"Review progress at each pitch">>
                ]
            },
            #{
                number => 6,
                description => <<"Configure buffer management">>,
                command => <<"tcps_heijunka:configure_buffers([\n"
                            "  {kanban_buffer_size, 2},\n"
                            "  {safety_buffer_percent, 15},\n"
                            "  {replenishment_trigger, 0.5}\n"
                            "]).">>,
                expected_output => <<"{ok, buffers_configured}">>,
                verification => <<"Buffers protect against variability">>,
                notes => [
                    <<"Kanban buffer prevents starvation">>,
                    <<"Safety buffer absorbs demand spikes">>,
                    <<"Replenish when half-empty">>
                ]
            },
            #{
                number => 7,
                description => <<"Simulate heijunka schedule">>,
                command => <<"tcps_heijunka:simulate_schedule([\n"
                            "  {simulation_days, 10},\n"
                            "  {demand_variability, 0.35},\n"
                            "  {capacity_utilization, 0.85}\n"
                            "]).">>,
                expected_output => <<"#{flow_smoothness => 0.82,\n"
                                    "  buffer_usage => 0.45,\n"
                                    "  schedule_adherence => 0.88,\n"
                                    "  recommendations => [\"Adequate buffers\", \"Good flow\"]}">>,
                verification => <<"Simulation shows smooth flow with manageable variability">>,
                notes => [
                    <<"Flow smoothness > 0.8 is good">>,
                    <<"Buffer usage < 0.7 indicates adequate sizing">>,
                    <<"Adjust parameters based on results">>
                ]
            },
            #{
                number => 8,
                description => <<"Launch heijunka system">>,
                command => <<"tcps_heijunka:activate([\n"
                            "  {start_date, tomorrow},\n"
                            "  {monitoring_enabled, true},\n"
                            "  {alert_on_deviation, true}\n"
                            "]).">>,
                expected_output => <<"{ok, heijunka_activated}">>,
                verification => <<"System begins leveling work automatically">>,
                notes => [
                    <<"Start date should be sprint/week boundary">>,
                    <<"Monitor closely in first 2 weeks">>,
                    <<"Expect adjustment period">>
                ]
            },
            #{
                number => 9,
                description => <<"Monitor leveling effectiveness">>,
                command => <<"tcps_heijunka:get_performance_metrics().">>,
                expected_output => <<"#{flow_variability_reduction => 0.42,\n"
                                    "  throughput_stability => 0.91,\n"
                                    "  resource_utilization => 0.87,\n"
                                    "  customer_satisfaction => 0.89}">>,
                verification => <<"Metrics show improved stability and flow">>,
                notes => [
                    <<"Variability reduction is key metric">>,
                    <<"Stable throughput indicates success">>,
                    <<"Review weekly, adjust monthly">>
                ]
            }
        ],
        verification => <<"Heijunka is successfully implemented when:\n"
                         "1. Work flow is visibly smoother and more predictable\n"
                         "2. Resource utilization is high and stable\n"
                         "3. Customer lead times are consistent\n"
                         "4. Work mix is balanced across time periods\n"
                         "5. Buffers adequately handle demand variability\n"
                         "6. Team operates in sustainable rhythm\n"
                         "7. Metrics show reduced variability">>,
        common_pitfalls => [
            <<"Insufficient historical data - Need at least 4 weeks">>,
            <<"Ignoring work type mix - All work is not equal">>,
            <<"Too small buffers - Cannot absorb variability">>,
            <<"Too large buffers - Hides problems, increases waste">>,
            <<"Rigid adherence to schedule - Allow some flexibility">>,
            <<"No monitoring - Cannot improve without measurement">>,
            <<"Reverting to old habits - Requires discipline to maintain">>
        ],
        related_guides => [kanban_config, takt_time, monitoring, performance],
        tags => [<<"heijunka">>, <<"leveling">>, <<"lean">>, <<"flow">>, <<"production">>]
    }.

%%%===================================================================
%%% Guide 6: How to Generate and Verify Receipt Chains
%%%===================================================================

-spec get_receipt_chain_guide() -> map().
get_receipt_chain_guide() ->
    #{
        id => receipt_chain,
        title => <<"How to Generate and Verify Receipt Chains">>,
        category => receipts,
        difficulty => intermediate,
        duration => 25,
        problem => <<"You need cryptographic proof of work completion and audit trail integrity. "
                     "Receipt chains provide tamper-evident records of all TCPS operations.">>,
        prerequisites => [
            <<"TCPS receipt system initialized">>,
            <<"Understanding of cryptographic hashing">>,
            <<"Work items to track">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Initialize receipt chain">>,
                command => <<"tcps_receipts:init_chain([\n"
                            "  {chain_id, production_v1},\n"
                            "  {hash_algorithm, sha256},\n"
                            "  {genesis_block, true}\n"
                            "]).">>,
                expected_output => <<"{ok, chain_initialized, genesis_hash}">>,
                verification => <<"Genesis block created with initial hash">>,
                notes => [
                    <<"Genesis block anchors the chain">>,
                    <<"Hash algorithm cannot change after init">>,
                    <<"Store genesis hash securely">>
                ]
            },
            #{
                number => 2,
                description => <<"Generate receipt for work item">>,
                command => <<"tcps_receipts:generate([\n"
                            "  {work_item_id, task_123},\n"
                            "  {action, completed},\n"
                            "  {timestamp, erlang:system_time(millisecond)},\n"
                            "  {metadata, #{author => \"dev_1\", duration => 240}}\n"
                            "]).">>,
                expected_output => <<"{ok, receipt_generated, ReceiptHash}">>,
                verification => <<"Receipt contains hash linking to previous receipt">>,
                notes => [
                    <<"Every action generates a receipt">>,
                    <<"Receipts are immutable once created">>,
                    <<"Hash links create the chain">>
                ]
            },
            #{
                number => 3,
                description => <<"Verify receipt integrity">>,
                command => <<"tcps_receipts:verify(ReceiptHash).">>,
                expected_output => <<"{ok, verified, #{valid => true,\n"
                                    "                  position => 42,\n"
                                    "                  prev_hash_valid => true}}">>,
                verification => <<"Receipt hash matches stored content">>,
                notes => [
                    <<"Verification checks hash calculation">>,
                    <<"Confirms link to previous receipt">>,
                    <<"Detects any tampering">>
                ]
            },
            #{
                number => 4,
                description => <<"Verify chain continuity">>,
                command => <<"tcps_receipts:verify_chain([\n"
                            "  {from_receipt, genesis},\n"
                            "  {to_receipt, latest},\n"
                            "  {check_all, true}\n"
                            "]).">>,
                expected_output => <<"{ok, chain_valid, #{length => 150,\n"
                                    "                    broken_links => 0,\n"
                                    "                    tampering_detected => false}}">>,
                verification => <<"Entire chain is intact from genesis to present">>,
                notes => [
                    <<"Verifies all hash links">>,
                    <<"Computationally expensive for long chains">>,
                    <<"Run periodically as health check">>
                ]
            },
            #{
                number => 5,
                description => <<"Query receipt chain history">>,
                command => <<"tcps_receipts:query([\n"
                            "  {work_item_id, task_123},\n"
                            "  {actions, [started, completed]},\n"
                            "  {time_range, last_7_days}\n"
                            "]).">>,
                expected_output => <<"{ok, receipts_found, [\n"
                                    "  #{action => started, timestamp => T1, hash => H1},\n"
                                    "  #{action => completed, timestamp => T2, hash => H2}\n"
                                    "]}">>,
                verification => <<"Complete audit trail for work item">>,
                notes => [
                    <<"Receipts provide tamper-proof history">>,
                    <<"Can prove when actions occurred">>,
                    <<"Supports compliance and auditing">>
                ]
            },
            #{
                number => 6,
                description => <<"Export receipt chain for archival">>,
                command => <<"tcps_receipts:export_chain([\n"
                            "  {format, json},\n"
                            "  {compression, gzip},\n"
                            "  {include_metadata, true},\n"
                            "  {output_file, \"/archives/chain_2024_q1.json.gz\"}\n"
                            "]).">>,
                expected_output => <<"{ok, chain_exported, {receipts, 5420, bytes, 245_000}}">>,
                verification => <<"Exported file contains complete chain">>,
                notes => [
                    <<"Archive chains periodically">>,
                    <<"Store archives in secure location">>,
                    <<"Verify exported chain integrity">>
                ]
            },
            #{
                number => 7,
                description => <<"Detect chain tampering">>,
                command => <<"tcps_receipts:detect_tampering([\n"
                            "  {check_all_receipts, true},\n"
                            "  {verify_signatures, true},\n"
                            "  {check_timestamps, true}\n"
                            "]).">>,
                expected_output => <<"{ok, no_tampering_detected}">>,
                verification => <<"All receipts pass integrity checks">>,
                notes => [
                    <<"Run as scheduled job">>,
                    <<"Alert on any tampering detected">>,
                    <<"Investigate anomalies immediately">>
                ]
            },
            #{
                number => 8,
                description => <<"Generate chain proof for external verification">>,
                command => <<"tcps_receipts:generate_proof([\n"
                            "  {receipt_hash, ReceiptHash},\n"
                            "  {include_merkle_path, true}\n"
                            "]).">>,
                expected_output => <<"{ok, proof_generated, #{merkle_path => [...],\n"
                                    "                        root_hash => RootHash}}">>,
                verification => <<"Proof enables independent verification">>,
                notes => [
                    <<"Merkle path proves receipt in chain">>,
                    <<"External parties can verify without full chain">>,
                    <<"Useful for compliance and audits">>
                ]
            }
        ],
        verification => <<"Receipt chain is working properly when:\n"
                         "1. Every action generates a unique receipt\n"
                         "2. Receipts link cryptographically to previous receipts\n"
                         "3. Chain integrity verification passes\n"
                         "4. No tampering detected in health checks\n"
                         "5. Query operations return accurate history\n"
                         "6. Chain can be exported and archived\n"
                         "7. External proofs verify successfully">>,
        common_pitfalls => [
            <<"Not initializing genesis block - Chain needs anchor">>,
            <<"Skipping receipt generation - Breaks chain continuity">>,
            <<"No periodic verification - Tampering goes undetected">>,
            <<"Missing backups - Risk of data loss">>,
            <<"Insecure storage - Defeats cryptographic guarantees">>,
            <<"No monitoring - Don't know if system is working">>
        ],
        related_guides => [quality_gates, monitoring, debugging],
        tags => [<<"receipts">>, <<"blockchain">>, <<"audit">>, <<"cryptography">>, <<"compliance">>]
    }.

%%%===================================================================
%%% Guide 7: How to Monitor TCPS Metrics
%%%===================================================================

-spec get_monitoring_guide() -> map().
get_monitoring_guide() ->
    #{
        id => monitoring,
        title => <<"How to Monitor TCPS Metrics">>,
        category => monitoring,
        difficulty => beginner,
        duration => 20,
        problem => <<"You need visibility into TCPS system health and performance. "
                     "Proper monitoring helps identify issues early and track improvements.">>,
        prerequisites => [
            <<"TCPS system running">>,
            <<"OTEL observability configured">>,
            <<"Access to metrics dashboard">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Configure metrics collection">>,
                command => <<"tcps_monitoring:configure([\n"
                            "  {collection_interval_seconds, 60},\n"
                            "  {retention_days, 90},\n"
                            "  {metric_categories, [performance, quality, flow, receipts]}\n"
                            "]).">>,
                expected_output => <<"{ok, monitoring_configured}">>,
                verification => <<"Metrics begin collecting at specified interval">>,
                notes => [
                    <<"60-second interval balances detail and overhead">>,
                    <<"90-day retention supports trend analysis">>,
                    <<"Enable all relevant categories">>
                ]
            },
            #{
                number => 2,
                description => <<"View current system metrics">>,
                command => <<"tcps_monitoring:get_current_metrics().">>,
                expected_output => <<"#{performance => #{throughput => 15, latency_p95 => 120},\n"
                                    "  quality => #{gate_pass_rate => 0.94, defect_rate => 0.02},\n"
                                    "  flow => #{cycle_time => 2.1, wip => 8},\n"
                                    "  receipts => #{chain_length => 5420, verification_rate => 1.0}}">>,
                verification => <<"Metrics show current system state">>,
                notes => [
                    <<"Baseline these values for comparison">>,
                    <<"Look for concerning trends">>,
                    <<"Set alert thresholds based on baselines">>
                ]
            },
            #{
                number => 3,
                description => <<"Set up metric alerts">>,
                command => <<"tcps_monitoring:create_alerts([\n"
                            "  {metric, gate_pass_rate, below, 0.90},\n"
                            "  {metric, cycle_time, above, 3.0},\n"
                            "  {metric, defect_rate, above, 0.05},\n"
                            "  {notification, [slack, email]}\n"
                            "]).">>,
                expected_output => <<"{ok, alerts_created, 3}">>,
                verification => <<"Alert system monitors thresholds">>,
                notes => [
                    <<"Set thresholds based on acceptable variance">>,
                    <<"Avoid alert fatigue with too many alerts">>,
                    <<"Test alerts to verify delivery">>
                ]
            },
            #{
                number => 4,
                description => <<"Create monitoring dashboard">>,
                command => <<"tcps_monitoring:create_dashboard([\n"
                            "  {name, \"TCPS Overview\"},\n"
                            "  {widgets, [\n"
                            "    {throughput_chart, realtime},\n"
                            "    {quality_gauge, current},\n"
                            "    {cycle_time_trend, 7_days},\n"
                            "    {alert_list, active}\n"
                            "  ]},\n"
                            "  {refresh_seconds, 30}\n"
                            "]).">>,
                expected_output => <<"{ok, dashboard_created, DashboardUrl}">>,
                verification => <<"Dashboard displays live metrics">>,
                notes => [
                    <<"Display on team monitor">>,
                    <<"Include key metrics only">>,
                    <<"Auto-refresh for real-time view">>
                ]
            },
            #{
                number => 5,
                description => <<"Analyze metric trends">>,
                command => <<"tcps_monitoring:analyze_trends([\n"
                            "  {metrics, [throughput, quality, cycle_time]},\n"
                            "  {period_days, 30},\n"
                            "  {detect_anomalies, true}\n"
                            "]).">>,
                expected_output => <<"#{throughput => {trend, improving, 12_percent},\n"
                                    "  quality => {trend, stable, 2_percent},\n"
                                    "  cycle_time => {trend, degrading, 8_percent},\n"
                                    "  anomalies => [{cycle_time, spike, day_23}]}">>,
                verification => <<"Trends show direction and magnitude of change">>,
                notes => [
                    <<"Improving throughput is good">>,
                    <<"Degrading cycle time needs investigation">>,
                    <<"Anomalies may indicate incidents">>
                ]
            },
            #{
                number => 6,
                description => <<"Generate performance report">>,
                command => <<"tcps_monitoring:generate_report([\n"
                            "  {report_type, weekly_summary},\n"
                            "  {include_charts, true},\n"
                            "  {distribution_list, [\"team@example.com\"]}\n"
                            "]).">>,
                expected_output => <<"{ok, report_generated, ReportPath}">>,
                verification => <<"Report sent to distribution list">>,
                notes => [
                    <<"Weekly cadence keeps team informed">>,
                    <<"Include actionable insights">>,
                    <<"Highlight improvements and concerns">>
                ]
            },
            #{
                number => 7,
                description => <<"Export metrics for external analysis">>,
                command => <<"tcps_monitoring:export_metrics([\n"
                            "  {format, prometheus},\n"
                            "  {endpoint, \"http://prometheus:9090\"},\n"
                            "  {update_interval_seconds, 30}\n"
                            "]).">>,
                expected_output => <<"{ok, metrics_exported}">>,
                verification => <<"External system ingests TCPS metrics">>,
                notes => [
                    <<"Integrate with existing observability stack">>,
                    <<"Prometheus is industry standard">>,
                    <<"Supports advanced querying and alerting">>
                ]
            }
        ],
        verification => <<"Monitoring is effective when:\n"
                         "1. All key metrics are collected continuously\n"
                         "2. Dashboard provides at-a-glance system health\n"
                         "3. Alerts trigger before issues become critical\n"
                         "4. Trends are analyzed to guide improvements\n"
                         "5. Reports keep stakeholders informed\n"
                         "6. Metrics integrate with broader observability\n"
                         "7. Team uses metrics to drive decisions">>,
        common_pitfalls => [
            <<"Too many metrics - Focus on actionable indicators">>,
            <<"No alerts - Reactive instead of proactive">>,
            <<"Alert fatigue - Too sensitive thresholds">>,
            <<"Metrics not visible - Dashboard not displayed">>,
            <<"No trend analysis - Missing gradual degradation">>,
            <<"Ignoring anomalies - Small issues become big problems">>
        ],
        related_guides => [performance, quality_gates, andon_response],
        tags => [<<"monitoring">>, <<"metrics">>, <<"observability">>, <<"otel">>, <<"dashboard">>]
    }.

%%%===================================================================
%%% Guide 8: How to Integrate TCPS with CI/CD
%%%===================================================================

-spec get_cicd_integration_guide() -> map().
get_cicd_integration_guide() ->
    #{
        id => cicd_integration,
        title => <<"How to Integrate TCPS with CI/CD">>,
        category => integration,
        difficulty => intermediate,
        duration => 35,
        problem => <<"Your CI/CD pipeline lacks quality gates and lean principles. "
                     "Integrating TCPS enforces standards and enables continuous improvement.">>,
        prerequisites => [
            <<"Existing CI/CD pipeline">>,
            <<"TCPS MCP server accessible from CI environment">>,
            <<"CI configuration file access">>,
            <<"Understanding of pipeline stages">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Install TCPS in CI environment">>,
                command => <<"# In your CI configuration (e.g., .github/workflows/ci.yml)\n"
                            "- name: Install TCPS\n"
                            "  run: |\n"
                            "    npm install -g @tcps/mcp-server\n"
                            "    tcps init --ci-mode">>,
                expected_output => <<"TCPS installed successfully\n"
                                    "CI mode enabled">>,
                verification => <<"tcps --version returns installed version">>,
                notes => [
                    <<"CI mode disables interactive features">>,
                    <<"Use environment variables for config">>,
                    <<"Cache installation for faster builds">>
                ]
            },
            #{
                number => 2,
                description => <<"Configure TCPS quality gates in pipeline">>,
                command => <<"- name: Run TCPS Quality Gates\n"
                            "  run: |\n"
                            "    tcps gates run --config .tcps/gates.yml\n"
                            "  env:\n"
                            "    TCPS_FAIL_ON_ERROR: true">>,
                expected_output => <<"Quality gates passed: 4/4\n"
                                    "- Test coverage: 85% (>80%) ✓\n"
                                    "- Lint errors: 0 ✓\n"
                                    "- Type coverage: 100% ✓\n"
                                    "- Security issues: 0 ✓">>,
                verification => <<"Pipeline fails if any gate fails">>,
                notes => [
                    <<"Gates run before deployment stages">>,
                    <<"Configuration in version control">>,
                    <<"Fail fast to save CI time">>
                ]
            },
            #{
                number => 3,
                description => <<"Add TCPS metrics collection">>,
                command => <<"- name: Collect TCPS Metrics\n"
                            "  run: |\n"
                            "    tcps metrics collect \\\n"
                            "      --build-id ${{ github.run_id }} \\\n"
                            "      --commit ${{ github.sha }} \\\n"
                            "      --export prometheus">>,
                expected_output => <<"Metrics collected and exported\n"
                                    "Build duration: 125s\n"
                                    "Test duration: 45s\n"
                                    "Gate check duration: 12s">>,
                verification => <<"Metrics appear in monitoring system">>,
                notes => [
                    <<"Track build performance over time">>,
                    <<"Identify slow stages">>,
                    <<"Correlate with code changes">>
                ]
            },
            #{
                number => 4,
                description => <<"Generate TCPS receipt for build">>,
                command => <<"- name: Generate Build Receipt\n"
                            "  run: |\n"
                            "    tcps receipts generate \\\n"
                            "      --action build_completed \\\n"
                            "      --metadata build-metadata.json">>,
                expected_output => <<"Receipt generated: 0x4f8a2b...\n"
                                    "Chain position: 1234\n"
                                    "Previous hash: 0x9c1e5d...">>,
                verification => <<"Receipt verifiable via TCPS API">>,
                notes => [
                    <<"Provides tamper-proof build audit trail">>,
                    <<"Include commit, timestamp, test results">>,
                    <<"Store receipt hash in artifacts">>
                ]
            },
            #{
                number => 5,
                description => <<"Implement Andon cord for pipeline">>,
                command => <<"- name: TCPS Andon Check\n"
                            "  if: failure()\n"
                            "  run: |\n"
                            "    tcps andon trigger \\\n"
                            "      --severity high \\\n"
                            "      --message \"CI pipeline failed\" \\\n"
                            "      --build-url ${{ github.server_url }}/${{ github.repository }}/actions/runs/${{ github.run_id }}">>,
                expected_output => <<"Andon alert triggered: alert_567\n"
                                    "Severity: high\n"
                                    "Notifications sent to: team-channel">>,
                verification => <<"Team receives failure alert immediately">>,
                notes => [
                    <<"Triggers only on failure">>,
                    <<"Include link to failed build">>,
                    <<"Notifies appropriate channels">>
                ]
            },
            #{
                number => 6,
                description => <<"Add Kanban integration for deployment">>,
                command => <<"- name: Update Kanban Board\n"
                            "  if: success()\n"
                            "  run: |\n"
                            "    tcps kanban move \\\n"
                            "      --item ${{ github.event.pull_request.number }} \\\n"
                            "      --from review \\\n"
                            "      --to done">>,
                expected_output => <<"Item PR-123 moved: review → done\n"
                                    "WIP in 'done': 5/unlimited">>,
                verification => <<"Kanban board updates automatically">>,
                notes => [
                    <<"Keeps board in sync with reality">>,
                    <<"Triggers on successful merge">>,
                    <<"Respects WIP limits">>
                ]
            },
            #{
                number => 7,
                description => <<"Configure deployment gates">>,
                command => <<"- name: Pre-Deployment Gates\n"
                            "  run: |\n"
                            "    tcps gates run --profile production \\\n"
                            "      --require-all \\\n"
                            "      --timeout 300">>,
                expected_output => <<"Production gates passed: 6/6\n"
                                    "- Quality gates: ✓\n"
                                    "- Security scan: ✓\n"
                                    "- Performance tests: ✓\n"
                                    "- Smoke tests: ✓\n"
                                    "- Receipt chain valid: ✓\n"
                                    "- Approval received: ✓">>,
                verification => <<"Deployment proceeds only if all gates pass">>,
                notes => [
                    <<"Production profile has stricter criteria">>,
                    <<"All gates must pass (--require-all)">>,
                    <<"Timeout prevents hanging pipelines">>
                ]
            },
            #{
                number => 8,
                description => <<"Generate CI/CD performance report">>,
                command => <<"- name: Generate TCPS Report\n"
                            "  run: |\n"
                            "    tcps report generate \\\n"
                            "      --type ci_cd_summary \\\n"
                            "      --format html \\\n"
                            "      --output reports/tcps-report.html">>,
                expected_output => <<"Report generated: reports/tcps-report.html\n"
                                    "Size: 245 KB\n"
                                    "Includes: metrics, gates, receipts, trends">>,
                verification => <<"Report published as CI artifact">>,
                notes => [
                    <<"Provides visibility into TCPS operations">>,
                    <<"Include in pull request comments">>,
                    <<"Archive for compliance">>
                ]
            }
        ],
        verification => <<"CI/CD integration is complete when:\n"
                         "1. TCPS runs automatically on every pipeline execution\n"
                         "2. Quality gates block defective code\n"
                         "3. Metrics are collected and exported\n"
                         "4. Build receipts provide audit trail\n"
                         "5. Andon alerts notify team of failures\n"
                         "6. Kanban board stays synchronized\n"
                         "7. Production deployments require all gates\n"
                         "8. Reports provide actionable insights">>,
        common_pitfalls => [
            <<"TCPS not in CI PATH - Installation step missing">>,
            <<"Configuration not in version control - Inconsistent behavior">>,
            <<"No failure notifications - Team unaware of issues">>,
            <<"Skipping gates under time pressure - Defeats the purpose">>,
            <<"Metrics not exported - Lost visibility">>,
            <<"No receipt generation - Missing audit trail">>,
            <<"Production gates same as dev - Insufficient rigor">>
        ],
        related_guides => [quality_gates, monitoring, receipt_chain, debugging],
        tags => [<<"ci-cd">>, <<"integration">>, <<"automation">>, <<"gates">>, <<"pipeline">>]
    }.

%%%===================================================================
%%% Guide 9: How to Debug Quality Gate Failures
%%%===================================================================

-spec get_debugging_guide() -> map().
get_debugging_guide() ->
    #{
        id => debugging,
        title => <<"How to Debug Quality Gate Failures">>,
        category => debugging,
        difficulty => intermediate,
        duration => 25,
        problem => <<"A quality gate is failing but the cause is not immediately clear. "
                     "You need systematic debugging to identify and fix the issue.">>,
        prerequisites => [
            <<"Access to TCPS logs">>,
            <<"Failed quality gate notification">>,
            <<"Understanding of gate criteria">>,
            <<"Source code access">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Identify which gate failed">>,
                command => <<"tcps_debug:get_gate_failure_details(LatestFailure).">>,
                expected_output => <<"#{gate_id => test_coverage,\n"
                                    "  status => failed,\n"
                                    "  threshold => 80,\n"
                                    "  actual => 75,\n"
                                    "  commit => \"abc123\",\n"
                                    "  timestamp => {{2024,1,15},{10,30,0}}}">>,
                verification => <<"Clear identification of failing gate and values">>,
                notes => [
                    <<"Note the gap: 75% vs 80% threshold">>,
                    <<"Check commit for recent changes">>,
                    <<"Review timestamp for context">>
                ]
            },
            #{
                number => 2,
                description => <<"Get detailed gate execution logs">>,
                command => <<"tcps_debug:get_gate_logs(test_coverage, [\n"
                            "  {log_level, debug},\n"
                            "  {include_command_output, true}\n"
                            "]).">>,
                expected_output => <<"[DEBUG] Running coverage check: rebar3 cover\n"
                                    "[INFO] Coverage report generated\n"
                                    "[DEBUG] Module coverages:\n"
                                    "  - module_a: 95%\n"
                                    "  - module_b: 85%\n"
                                    "  - module_c: 45%  <-- Below threshold\n"
                                    "[ERROR] Overall coverage: 75% (threshold: 80%)">>,
                verification => <<"Logs reveal which module has low coverage">>,
                notes => [
                    <<"module_c at 45% is dragging down overall">>,
                    <<"Debug level shows detailed breakdown">>,
                    <<"Command output helps verify tool ran correctly">>
                ]
            },
            #{
                number => 3,
                description => <<"Identify uncovered code sections">>,
                command => <<"tcps_debug:analyze_uncovered_code(module_c).">>,
                expected_output => <<"#{total_lines => 200,\n"
                                    "  covered_lines => 90,\n"
                                    "  uncovered_lines => 110,\n"
                                    "  uncovered_functions => [\n"
                                    "    {handle_error, 3, \"Lines 45-68\"},\n"
                                    "    {validate_input, 2, \"Lines 92-115\"},\n"
                                    "    {format_output, 1, \"Lines 134-156\"}\n"
                                    "  ]}">>,
                verification => <<"Specific functions needing tests identified">>,
                notes => [
                    <<"Three functions have no test coverage">>,
                    <<"Line numbers help locate code">>,
                    <<"Prioritize by function importance">>
                ]
            },
            #{
                number => 4,
                description => <<"Check if tests exist but don't execute">>,
                command => <<"tcps_debug:check_test_execution(module_c_tests).">>,
                expected_output => <<"#{tests_defined => 8,\n"
                                    "  tests_run => 5,\n"
                                    "  tests_skipped => 3,\n"
                                    "  skip_reasons => [\n"
                                    "    {test_handle_error, \"Not implemented\"},\n"
                                    "    {test_validate_input, \"Not implemented\"},\n"
                                    "    {test_format_output, \"Flaky test\"}\n"
                                    "  ]}">>,
                verification => <<"Tests exist but are skipped">>,
                notes => [
                    <<"Common issue: tests marked as skip/pending">>,
                    <<"Need to implement or unskip tests">>,
                    <<"Address flaky tests properly">>
                ]
            },
            #{
                number => 5,
                description => <<"Verify test tool configuration">>,
                command => <<"tcps_debug:verify_tool_config(coverage_tool).">>,
                expected_output => <<"#{tool => rebar3_cover,\n"
                                    "  config_file => \"rebar.config\",\n"
                                    "  settings => #{excl_mods => [module_d, module_e]},\n"
                                    "  issues => [{warning, \"module_c not in coverage scope\"}]}">>,
                verification => <<"Tool configuration checked">>,
                notes => [
                    <<"module_c may be excluded from coverage">>,
                    <<"Check exclusion lists">>,
                    <<"Verify tool version compatibility">>
                ]
            },
            #{
                number => 6,
                description => <<"Compare with previous passing state">>,
                command => <<"tcps_debug:compare_with_baseline([\n"
                            "  {current_commit, \"abc123\"},\n"
                            "  {baseline_commit, \"xyz789\"},\n"
                            "  {metric, test_coverage}\n"
                            "]).">>,
                expected_output => <<"#{baseline_coverage => 82,\n"
                                    "  current_coverage => 75,\n"
                                    "  regression => 7,\n"
                                    "  files_changed => [\"module_c.erl\"],\n"
                                    "  new_code_lines => 45,\n"
                                    "  new_code_coverage => 0}">>,
                verification => <<"Identifies regression cause">>,
                notes => [
                    <<"Coverage dropped 7 points">>,
                    <<"45 lines of new code with 0% coverage">>,
                    <<"Need tests for new code in module_c">>
                ]
            },
            #{
                number => 7,
                description => <<"Generate fix recommendations">>,
                command => <<"tcps_debug:get_fix_recommendations(test_coverage_failure).">>,
                expected_output => <<"[\n"
                                    "  {priority, high, \"Write tests for handle_error/3 in module_c\"},\n"
                                    "  {priority, high, \"Write tests for validate_input/2 in module_c\"},\n"
                                    "  {priority, medium, \"Fix or unskip test_format_output\"},\n"
                                    "  {priority, low, \"Consider raising overall threshold to 85%\"}\n"
                                    "]">>,
                verification => <<"Actionable recommendations provided">>,
                notes => [
                    <<"Prioritized list of fixes">>,
                    <<"High priority items will resolve failure">>,
                    <<"Low priority items for continuous improvement">>
                ]
            },
            #{
                number => 8,
                description => <<"Verify fix before committing">>,
                command => <<"tcps_debug:dry_run_gate(test_coverage).">>,
                expected_output => <<"{ok, gate_passed, #{threshold => 80, actual => 83}}">>,
                verification => <<"Gate would pass with current changes">>,
                notes => [
                    <<"Dry run tests locally before pushing">>,
                    <<"Saves CI cycles">>,
                    <<"Confirms fix is adequate">>
                ]
            }
        ],
        verification => <<"Debugging is successful when:\n"
                         "1. Root cause of failure is identified\n"
                         "2. Specific code/tests needing attention are located\n"
                         "3. Tool configuration issues are ruled out\n"
                         "4. Regression source is understood\n"
                         "5. Fix recommendations are clear and prioritized\n"
                         "6. Local dry run confirms fix\n"
                         "7. Gate passes after fix implementation">>,
        common_pitfalls => [
            <<"Assuming tool output is wrong - Usually it's correct">>,
            <<"Not checking for skipped tests - Common coverage killer">>,
            <<"Ignoring tool configuration - Exclusions hide issues">>,
            <<"No baseline comparison - Can't identify regressions">>,
            <<"Rushing the fix - May introduce new problems">>,
            <<"Not verifying locally - Wastes CI resources">>,
            <<"Lowering threshold instead of fixing - Takes the easy way out">>
        ],
        related_guides => [quality_gates, monitoring, cicd_integration],
        tags => [<<"debugging">>, <<"quality-gates">>, <<"testing">>, <<"troubleshooting">>]
    }.

%%%===================================================================
%%% Guide 10: How to Optimize TCPS Performance
%%%===================================================================

-spec get_performance_guide() -> map().
get_performance_guide() ->
    #{
        id => performance,
        title => <<"How to Optimize TCPS Performance">>,
        category => performance,
        difficulty => advanced,
        duration => 40,
        problem => <<"TCPS operations are slower than expected, impacting developer workflow. "
                     "You need to identify bottlenecks and optimize performance.">>,
        prerequisites => [
            <<"TCPS performance monitoring enabled">>,
            <<"Access to OTEL traces and metrics">>,
            <<"Understanding of system architecture">>,
            <<"Profiling tools available">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Establish performance baseline">>,
                command => <<"tcps_performance:benchmark([\n"
                            "  {operations, [gate_check, receipt_gen, kanban_move]},\n"
                            "  {iterations, 100},\n"
                            "  {record_baseline, true}\n"
                            "]).">>,
                expected_output => <<"#{gate_check => #{p50 => 125, p95 => 340, p99 => 580},\n"
                                    "  receipt_gen => #{p50 => 45, p95 => 120, p99 => 210},\n"
                                    "  kanban_move => #{p50 => 30, p95 => 85, p99 => 140}}">>,
                verification => <<"Baseline metrics recorded for comparison">>,
                notes => [
                    <<"Run during typical load conditions">>,
                    <<"P95 and P99 reveal tail latency">>,
                    <<"Store baseline for future comparison">>
                ]
            },
            #{
                number => 2,
                description => <<"Identify slowest operations">>,
                command => <<"tcps_performance:get_slow_operations([\n"
                            "  {threshold_ms, 200},\n"
                            "  {time_period, last_hour}\n"
                            "]).">>,
                expected_output => <<"[\n"
                                    "  {gate_check, 340, \"Quality gate: test_coverage\"},\n"
                                    "  {gate_check, 580, \"Quality gate: test_coverage\"},\n"
                                    "  {receipt_verify, 450, \"Full chain verification\"}\n"
                                    "]">>,
                verification => <<"Operations exceeding threshold identified">>,
                notes => [
                    <<"test_coverage gate is slowest">>,
                    <<"Full chain verification also slow">>,
                    <<"Focus optimization efforts here">>
                ]
            },
            #{
                number => 3,
                description => <<"Analyze operation traces">>,
                command => <<"tcps_performance:get_trace_analysis(gate_check).">>,
                expected_output => <<"#{total_duration => 340,\n"
                                    "  spans => [\n"
                                    "    {run_coverage_tool, 280, 82.3},\n"
                                    "    {parse_coverage_report, 35, 10.3},\n"
                                    "    {compare_threshold, 15, 4.4},\n"
                                    "    {record_result, 10, 2.9}\n"
                                    "  ]}">>,
                verification => <<"Time breakdown reveals bottleneck">>,
                notes => [
                    <<"Coverage tool takes 82% of time">>,
                    <<"Tool execution is the bottleneck">>,
                    <<"Parsing and comparison are reasonable">>
                ]
            },
            #{
                number => 4,
                description => <<"Enable operation caching">>,
                command => <<"tcps_performance:enable_caching([\n"
                            "  {operation, gate_check},\n"
                            "  {cache_key, {commit_sha, gate_type}},\n"
                            "  {ttl_seconds, 3600},\n"
                            "  {cache_backend, redis}\n"
                            "]).">>,
                expected_output => <<"{ok, caching_enabled, gate_check}">>,
                verification => <<"Subsequent identical checks use cached results">>,
                notes => [
                    <<"Same commit + gate = same result">>,
                    <<"1-hour TTL balances freshness and performance">>,
                    <<"Redis enables distributed caching">>
                ]
            },
            #{
                number => 5,
                description => <<"Parallelize independent operations">>,
                command => <<"tcps_performance:configure_parallelization([\n"
                            "  {operation, gate_check},\n"
                            "  {parallel_gates, [test_coverage, lint, type_check, security]},\n"
                            "  {max_parallel, 4}\n"
                            "]).">>,
                expected_output => <<"{ok, parallelization_configured, {gates, 4}}">>,
                verification => <<"Gates run concurrently instead of serially">>,
                notes => [
                    <<"4 gates in parallel vs sequential">>,
                    <<"Total time = slowest gate, not sum">>,
                    <<"Requires adequate CPU resources">>
                ]
            },
            #{
                number => 6,
                description => <<"Optimize receipt chain verification">>,
                command => <<"tcps_performance:optimize_receipts([\n"
                            "  {use_merkle_tree, true},\n"
                            "  {verify_sample_rate, 0.1},\n"
                            "  {cache_chain_state, true}\n"
                            "]).">>,
                expected_output => <<"{ok, receipt_optimization_enabled}">>,
                verification => <<"Receipt operations complete faster">>,
                notes => [
                    <<"Merkle tree reduces verification to O(log n)">>,
                    <<"Sample verification adequate for most cases">>,
                    <<"Caching avoids repeated chain traversal">>
                ]
            },
            #{
                number => 7,
                description => <<"Tune database queries">>,
                command => <<"tcps_performance:analyze_queries([\n"
                            "  {slow_query_threshold_ms, 100},\n"
                            "  {suggest_indexes, true}\n"
                            "]).">>,
                expected_output => <<"#{slow_queries => [\n"
                                    "    {\"SELECT * FROM receipts WHERE work_item_id = ?\", 150}\n"
                                    "  ],\n"
                                    "  index_suggestions => [\n"
                                    "    \"CREATE INDEX idx_receipts_work_item ON receipts(work_item_id)\"\n"
                                    "  ]}">>,
                verification => <<"Missing indexes identified">>,
                notes => [
                    <<"Add suggested indexes">>,
                    <<"Test query performance after indexing">>,
                    <<"Monitor index usage">>
                ]
            },
            #{
                number => 8,
                description => <<"Implement incremental operations">>,
                command => <<"tcps_performance:enable_incremental([\n"
                            "  {operation, test_coverage},\n"
                            "  {strategy, changed_files_only},\n"
                            "  {fallback_to_full, on_error}\n"
                            "]).">>,
                expected_output => <<"{ok, incremental_enabled, test_coverage}">>,
                verification => <<"Coverage checks only changed files">>,
                notes => [
                    <<"Dramatically faster for small changes">>,
                    <<"Falls back to full check on errors">>,
                    <<"Requires reliable change detection">>
                ]
            },
            #{
                number => 9,
                description => <<"Benchmark optimizations">>,
                command => <<"tcps_performance:benchmark([\n"
                            "  {operations, [gate_check, receipt_gen, kanban_move]},\n"
                            "  {iterations, 100},\n"
                            "  {compare_to_baseline, true}\n"
                            "]).">>,
                expected_output => <<"#{gate_check => #{p50 => 35, improvement => 72%},\n"
                                    "  receipt_gen => #{p50 => 25, improvement => 44%},\n"
                                    "  kanban_move => #{p50 => 28, improvement => 7%}}">>,
                verification => <<"Significant performance improvements measured">>,
                notes => [
                    <<"gate_check improved 72% (125ms → 35ms)">>,
                    <<"Caching and parallelization very effective">>,
                    <<"kanban_move was already fast">>
                ]
            },
            #{
                number => 10,
                description => <<"Monitor performance continuously">>,
                command => <<"tcps_performance:enable_continuous_monitoring([\n"
                            "  {alert_on_regression, true},\n"
                            "  {regression_threshold, 0.20},\n"
                            "  {dashboard_update_interval, 60}\n"
                            "]).">>,
                expected_output => <<"{ok, continuous_monitoring_enabled}">>,
                verification => <<"Performance metrics tracked over time">>,
                notes => [
                    <<"Alert if performance degrades >20%">>,
                    <<"Dashboard shows trends">>,
                    <<"Catch regressions early">>
                ]
            }
        ],
        verification => <<"Performance is optimized when:\n"
                         "1. Baseline performance is documented\n"
                         "2. Bottlenecks are identified and addressed\n"
                         "3. Caching reduces redundant work\n"
                         "4. Independent operations run in parallel\n"
                         "5. Database queries are indexed\n"
                         "6. Incremental strategies minimize work\n"
                         "7. Measurable improvements achieved (>50%)\n"
                         "8. Continuous monitoring prevents regressions">>,
        common_pitfalls => [
            <<"Premature optimization - Measure first, then optimize">>,
            <<"Optimizing wrong operations - Focus on actual bottlenecks">>,
            <<"Cache invalidation bugs - Verify cached data correctness">>,
            <<"Over-parallelization - Diminishing returns, increased complexity">>,
            <<"No baseline comparison - Can't prove improvements">>,
            <<"Missing indexes on production - Test in prod-like environment">>,
            <<"Incremental without fallback - Breaks on edge cases">>,
            <<"No regression monitoring - Optimizations degrade over time">>
        ],
        related_guides => [monitoring, debugging, cicd_integration],
        tags => [<<"performance">>, <<"optimization">>, <<"benchmarking">>, <<"caching">>, <<"profiling">>]
    }.

%%%===================================================================
%%% Guide 11: How to Calculate and Set Takt Time
%%%===================================================================

-spec get_takt_time_guide() -> map().
get_takt_time_guide() ->
    #{
        id => takt_time,
        title => <<"How to Calculate and Set Takt Time">>,
        category => production,
        difficulty => intermediate,
        duration => 30,
        problem => <<"Production pace doesn't match customer demand, causing overproduction or delays. "
                     "Takt time aligns production rate with demand for optimal flow.">>,
        prerequisites => [
            <<"Customer demand data">>,
            <<"Available production time data">>,
            <<"TCPS production module enabled">>,
            <<"Understanding of production capacity">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Calculate available production time">>,
                command => <<"WorkDaysPerWeek = 5,\n"
                            "HoursPerDay = 8,\n"
                            "BreakHours = 1,\n"
                            "MeetingHours = 1,\n"
                            "AvailableHours = (HoursPerDay - BreakHours - MeetingHours) * WorkDaysPerWeek,\n"
                            "AvailableSeconds = AvailableHours * 3600.">>,
                expected_output => <<"AvailableSeconds = 108000  % 30 hours = 108000 seconds/week">>,
                verification => <<"Available time accounts for breaks and meetings">>,
                notes => [
                    <<"Use net available time, not gross">>,
                    <<"Include all non-productive time">>,
                    <<"Base on actual working patterns">>
                ]
            },
            #{
                number => 2,
                description => <<"Determine customer demand rate">>,
                command => <<"tcps_takt:analyze_demand([\n"
                            "  {period_weeks, 8},\n"
                            "  {demand_source, [tickets_closed, features_deployed]}\n"
                            "]).">>,
                expected_output => <<"#{average_weekly_demand => 18,\n"
                                    "  demand_variability => 0.22,\n"
                                    "  trend => stable}">>,
                verification => <<"Demand is measurable and relatively stable">>,
                notes => [
                    <<"Use actual customer pull">>,
                    <<"At least 4 weeks of data">>,
                    <<"High variability may need buffer strategy">>
                ]
            },
            #{
                number => 3,
                description => <<"Calculate takt time">>,
                command => <<"AvailableTime = 108000,  % seconds/week\n"
                            "CustomerDemand = 18,     % items/week\n"
                            "TaktTime = AvailableTime / CustomerDemand,\n"
                            "tcps_takt:set_takt_time(TaktTime).">>,
                expected_output => <<"{ok, takt_time_set, 6000}  % 6000 seconds = 100 minutes per item">>,
                verification => <<"Takt time represents available time per customer unit">>,
                notes => [
                    <<"Takt time = Available time / Customer demand">>,
                    <<"100 minutes/item means produce 1 item every 100 min">>,
                    <<"This is the target pace, not cycle time">>
                ]
            },
            #{
                number => 4,
                description => <<"Compare takt time to cycle time">>,
                command => <<"tcps_takt:compare_takt_to_cycle().">>,
                expected_output => <<"#{takt_time => 6000,\n"
                                    "  current_cycle_time => 5400,\n"
                                    "  capacity_status => adequate,\n"
                                    "  utilization => 0.90}">>,
                verification => <<"Cycle time is less than takt time (good)">>,
                notes => [
                    <<"Cycle time < takt time = capacity adequate">>,
                    <<"Cycle time > takt time = cannot meet demand">>,
                    <<"90% utilization is healthy">>
                ]
            },
            #{
                number => 5,
                description => <<"Configure takt time pacing">>,
                command => <<"tcps_takt:configure_pacing([\n"
                            "  {pace_to_takt, true},\n"
                            "  {release_interval, 6000},\n"
                            "  {visual_indicator, true},\n"
                            "  {alert_on_deviation, 0.15}\n"
                            "]).">>,
                expected_output => <<"{ok, pacing_configured}">>,
                verification => <<"System releases work every takt interval">>,
                notes => [
                    <<"Work released every 100 minutes">>,
                    <<"Visual indicator shows takt rhythm">>,
                    <<"Alert if deviation exceeds 15%">>
                ]
            },
            #{
                number => 6,
                description => <<"Set up takt time monitoring">>,
                command => <<"tcps_takt:enable_monitoring([\n"
                            "  {track_adherence, true},\n"
                            "  {dashboard_widget, true},\n"
                            "  {report_daily, true}\n"
                            "]).">>,
                expected_output => <<"{ok, takt_monitoring_enabled}">>,
                verification => <<"Takt adherence visible on dashboard">>,
                notes => [
                    <<"Monitor how well actual pace matches takt">>,
                    <<"Dashboard shows current status">>,
                    <<"Daily reports track trends">>
                ]
            },
            #{
                number => 7,
                description => <<"Analyze takt time adherence">>,
                command => <<"tcps_takt:get_adherence_report(last_week).">>,
                expected_output => <<"#{takt_adherence => 0.88,\n"
                                    "  faster_than_takt => 0.05,\n"
                                    "  slower_than_takt => 0.07,\n"
                                    "  on_pace => 0.88,\n"
                                    "  recommendations => [\"Good adherence\", \"Minor adjustments needed\"]}">>,
                verification => <<"88% adherence indicates good alignment">>,
                notes => [
                    <<">85% adherence is good">>,
                    <<"Some variance is normal">>,
                    <<"Consistent deviation indicates need to recalculate takt">>
                ]
            },
            #{
                number => 8,
                description => <<"Adjust for demand changes">>,
                command => <<"tcps_takt:recalculate_takt([\n"
                            "  {new_demand, 22},\n"
                            "  {reason, \"Increased customer orders\"}\n"
                            "]).">>,
                expected_output => <<"{ok, takt_time_updated, {old, 6000, new, 4909}}">>,
                verification => <<"Takt time adjusts to new demand (22 items/week)">>,
                notes => [
                    <<"Demand increased 22% (18→22 items)">>,
                    <<"Takt decreased 18% (100→82 minutes)">>,
                    <<"Review capacity to ensure feasibility">>
                ]
            }
        ],
        verification => <<"Takt time is properly set when:\n"
                         "1. Based on actual available production time\n"
                         "2. Derived from real customer demand\n"
                         "3. Cycle time is less than takt time\n"
                         "4. Work is released at takt intervals\n"
                         "5. Adherence is monitored continuously\n"
                         "6. Team understands and follows takt pace\n"
                         "7. Takt is recalculated when demand changes">>,
        common_pitfalls => [
            <<"Using gross time instead of net available time">>,
            <<"Basing demand on forecasts instead of actual pull">>,
            <<"Not accounting for demand variability">>,
            <<"Cycle time exceeds takt time (capacity problem)">>,
            <<"Setting and forgetting - Demand changes over time">>,
            <<"No visual takt indicator - Team can't see pace">>,
            <<"Confusing takt time with cycle time - Different concepts">>
        ],
        related_guides => [heijunka, kanban_config, performance, monitoring],
        tags => [<<"takt-time">>, <<"lean">>, <<"production">>, <<"demand">>, <<"pacing">>]
    }.

%%%===================================================================
%%% Guide 12: How to Implement Jidoka Automation with Human Touch
%%%===================================================================

-spec get_jidoka_guide() -> map().
get_jidoka_guide() ->
    #{
        id => jidoka,
        title => <<"How to Implement Jidoka (Automation with Human Touch)">>,
        category => quality_gates,
        difficulty => advanced,
        duration => 50,
        problem => <<"Automated processes lack intelligent error detection and human judgment. "
                     "Jidoka builds quality at the source with smart automation that stops when issues occur.">>,
        prerequisites => [
            <<"Existing automated processes">>,
            <<"Error detection capabilities">>,
            <<"Team availability for intervention">>,
            <<"TCPS Jidoka module enabled">>
        ],
        steps => [
            #{
                number => 1,
                description => <<"Identify processes for jidoka implementation">>,
                command => <<"tcps_jidoka:analyze_processes([\n"
                            "  {criteria, [repetitive, error_prone, quality_critical]},\n"
                            "  {minimum_frequency, 10_per_week}\n"
                            "]).">>,
                expected_output => <<"#{candidates => [\n"
                                    "    {test_execution, score => 9.2},\n"
                                    "    {code_review, score => 8.1},\n"
                                    "    {deployment, score => 7.8}\n"
                                    "  ]}">>,
                verification => <<"High-impact processes identified">>,
                notes => [
                    <<"Focus on repetitive, error-prone processes">>,
                    <<"Scoring helps prioritize">>,
                    <<"Start with highest score">>
                ]
            },
            #{
                number => 2,
                description => <<"Define normal vs abnormal conditions">>,
                command => <<"tcps_jidoka:define_conditions(test_execution, [\n"
                            "  {normal, [\n"
                            "    {all_tests_pass, true},\n"
                            "    {execution_time_under, 300},\n"
                            "    {no_flaky_tests, true}\n"
                            "  ]},\n"
                            "  {abnormal, [\n"
                            "    {any_test_fails, stop_immediately},\n"
                            "    {execution_timeout, alert_and_continue},\n"
                            "    {flaky_test_detected, log_and_continue}\n"
                            "  ]}\n"
                            "]).">>,
                expected_output => <<"{ok, conditions_defined, test_execution}">>,
                verification => <<"Clear criteria for normal and abnormal states">>,
                notes => [
                    <<"Normal = process can continue autonomously">>,
                    <<"Abnormal = requires stopping or human intervention">>,
                    <<"Define response for each abnormal condition">>
                ]
            },
            #{
                number => 3,
                description => <<"Implement automatic error detection">>,
                command => <<"tcps_jidoka:enable_detection(test_execution, [\n"
                            "  {detection_method, real_time_monitoring},\n"
                            "  {check_frequency, per_operation},\n"
                            "  {sensitivity, high}\n"
                            "]).">>,
                expected_output => <<"{ok, detection_enabled, test_execution}">>,
                verification => <<"System detects abnormalities automatically">>,
                notes => [
                    <<"Real-time monitoring catches issues immediately">>,
                    <<"Per-operation checking ensures nothing is missed">>,
                    <<"High sensitivity prevents defects from escaping">>
                ]
            },
            #{
                number => 4,
                description => <<"Configure automatic stopping mechanism">>,
                command => <<"tcps_jidoka:configure_auto_stop([\n"
                            "  {trigger_conditions, [test_failure, timeout]},\n"
                            "  {stop_scope, current_process},\n"
                            "  {notify_team, true},\n"
                            "  {preserve_state, true}\n"
                            "]).">>,
                expected_output => <<"{ok, auto_stop_configured}">>,
                verification => <<"Process stops automatically on abnormality">>,
                notes => [
                    <<"Stopping prevents defect propagation">>,
                    <<"Team notified immediately">>,
                    <<"State preserved for investigation">>
                ]
            },
            #{
                number => 5,
                description => <<"Build in quality checks at the source">>,
                command => <<"tcps_jidoka:add_source_checks(test_execution, [\n"
                            "  {pre_check, validate_test_environment},\n"
                            "  {during_check, monitor_test_execution},\n"
                            "  {post_check, verify_coverage_maintained},\n"
                            "  {fail_fast, true}\n"
                            "]).">>,
                expected_output => <<"{ok, source_checks_added, 3}">>,
                verification => <<"Quality verified at each stage">>,
                notes => [
                    <<"Pre-check prevents starting with bad environment">>,
                    <<"During-check catches issues in real-time">>,
                    <<"Post-check ensures quality maintained">>,
                    <<"Fail fast saves time">>
                ]
            },
            #{
                number => 6,
                description => <<"Enable intelligent error analysis">>,
                command => <<"tcps_jidoka:enable_error_analysis([\n"
                            "  {analyze_patterns, true},\n"
                            "  {suggest_root_cause, true},\n"
                            "  {recommend_fix, true},\n"
                            "  {learn_from_resolutions, true}\n"
                            "]).">>,
                expected_output => <<"{ok, error_analysis_enabled}">>,
                verification => <<"System provides intelligent error insights">>,
                notes => [
                    <<"Pattern recognition identifies recurring issues">>,
                    <<"Root cause suggestions guide investigation">>,
                    <<"Fix recommendations speed resolution">>,
                    <<"Learning improves over time">>
                ]
            },
            #{
                number => 7,
                description => <<"Configure human intervention points">>,
                command => <<"tcps_jidoka:define_intervention_points([\n"
                            "  {point, error_resolution, required => true},\n"
                            "  {point, quality_judgment, required => true},\n"
                            "  {point, process_improvement, required => false},\n"
                            "  {escalation_timeout, 15_minutes}\n"
                            "]).">>,
                expected_output => <<"{ok, intervention_points_defined, 3}">>,
                verification => <<"Clear points where human judgment needed">>,
                notes => [
                    <<"Humans resolve errors automation can't handle">>,
                    <<"Humans make quality judgments">>,
                    <<"Escalate if no response in 15 minutes">>
                ]
            },
            #{
                number => 8,
                description => <<"Test jidoka system with simulated errors">>,
                command => <<"tcps_jidoka:simulate_error(test_execution, [\n"
                            "  {error_type, test_failure},\n"
                            "  {verify_detection, true},\n"
                            "  {verify_stop, true},\n"
                            "  {verify_notification, true}\n"
                            "]).">>,
                expected_output => <<"#{detected => true, detection_time_ms => 45,\n"
                                    "  stopped => true, stop_time_ms => 120,\n"
                                    "  notified => true, notification_channels => [slack, email],\n"
                                    "  state_preserved => true}">>,
                verification => <<"Jidoka system responds correctly to errors">>,
                notes => [
                    <<"Detection within 45ms is excellent">>,
                    <<"Stopped within 120ms prevents further damage">>,
                    <<"Notifications ensure team awareness">>,
                    <<"State preservation enables investigation">>
                ]
            },
            #{
                number => 9,
                description => <<"Implement visual management for jidoka status">>,
                command => <<"tcps_jidoka:create_visual_board([\n"
                            "  {show_processes, all_jidoka_enabled},\n"
                            "  {indicate_status, [running, stopped, investigating]},\n"
                            "  {highlight_abnormal, true},\n"
                            "  {update_realtime, true}\n"
                            "]).">>,
                expected_output => <<"{ok, visual_board_created, BoardUrl}">>,
                verification => <<"Team can see jidoka status at a glance">>,
                notes => [
                    <<"Visual board provides transparency">>,
                    <<"Status colors: green=running, red=stopped, yellow=investigating">>,
                    <<"Real-time updates keep information current">>
                ]
            },
            #{
                number => 10,
                description => <<"Monitor jidoka effectiveness">>,
                command => <<"tcps_jidoka:get_effectiveness_metrics().">>,
                expected_output => <<"#{defects_caught_at_source => 42,\n"
                                    "  defects_escaped => 2,\n"
                                    "  catch_rate => 0.955,\n"
                                    "  avg_detection_time_seconds => 3.2,\n"
                                    "  avg_resolution_time_minutes => 18,\n"
                                    "  false_positive_rate => 0.03,\n"
                                    "  process_improvement_suggestions => 7}">>,
                verification => <<"Jidoka demonstrably improving quality">>,
                notes => [
                    <<"95.5% catch rate is excellent">>,
                    <<"3.2 second detection prevents propagation">>,
                    <<"18 minute resolution is acceptable">>,
                    <<"Low false positive rate (3%) reduces alert fatigue">>,
                    <<"System learning and suggesting improvements">>
                ]
            }
        ],
        verification => <<"Jidoka is successfully implemented when:\n"
                         "1. Normal and abnormal conditions clearly defined\n"
                         "2. Automated detection catches errors immediately\n"
                         "3. Process stops automatically on abnormalities\n"
                         "4. Quality checks built in at the source\n"
                         "5. Intelligent error analysis guides resolution\n"
                         "6. Human judgment applied at critical points\n"
                         "7. Visual management provides transparency\n"
                         "8. Metrics show high defect catch rate\n"
                         "9. System learns and improves over time\n"
                         "10. Defects caught before escaping to customers">>,
        common_pitfalls => [
            <<"Over-automation - Removing necessary human judgment">>,
            <<"Poor error detection - Missing abnormalities">>,
            <<"Not stopping on errors - Defeats the purpose">>,
            <<"No visual management - Team can't see status">>,
            <<"High false positive rate - Causes alert fatigue">>,
            <<"No learning mechanism - Repeats same mistakes">>,
            <<"Slow error detection - Defects propagate">>,
            <<"No human intervention points - Can't handle edge cases">>,
            <<"Insufficient testing - Jidoka doesn't work when needed">>
        ],
        related_guides => [quality_gates, andon_response, monitoring, five_whys],
        tags => [<<"jidoka">>, <<"automation">>, <<"quality">>, <<"lean">>, <<"error-detection">>]
    }.
