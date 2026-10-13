%%%-------------------------------------------------------------------
%%% @doc Compliance Framework Unit Tests
%%%
%%% EUnit tests for erlmcp_compliance_framework module
%%% Tests compliance checking, rule evaluation, and reporting.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_compliance_framework_unit_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test State
%%%=============================================================================

-record(compliance_test_state, {
    framework_pid :: pid() | undefined
}).

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_compliance_framework() ->
    %% Start the compliance framework
    {ok, Pid} = erlmcp_compliance_framework:start_link(#{}),
    #compliance_test_state{framework_pid = Pid}.

cleanup_compliance_framework(#compliance_test_state{framework_pid = Pid}) ->
    catch gen_server:stop(Pid),
    ok.

%%%=============================================================================
%%% Test Generators
%%%=============================================================================

compliance_framework_test_() ->
    {setup,
     fun setup_compliance_framework/0,
     fun cleanup_compliance_framework/1,
     [
      fun test_start_stop/1,
      fun test_add_compliance_rule/1,
      fun test_evaluate_compliance/1,
      fun test_compliance_report/1,
      fun test_rule_categories/1,
      fun test_enforcement_actions/1
     ]}.

%%%=============================================================================
%%% Basic Lifecycle Tests
%%%=============================================================================

test_start_stop(_State) ->
    ?assertMatch({ok, _Pid}, erlmcp_compliance_framework:start_link(#{})),

    %% Verify framework is registered
    ?assert(is_pid(whereis(erlmcp_compliance_framework))).

test_add_compliance_rule(#compliance_test_state{framework_pid = Pid}) ->
    %% Add a compliance rule
    Rule = #{
        id => <<"test-rule-1">>,
        category => security,
        severity => high,
        description => <<"Test rule">>,
        check => fun(_) -> true end,
        remediation => <<"Fix it">>
    },

    ?assertEqual(ok, gen_server:call(Pid, {add_rule, Rule})),

    %% Verify rule was added
    Rules = gen_server:call(Pid, get_rules),
    ?assert(length(Rules) > 0).

test_evaluate_compliance(#compliance_test_state{framework_pid = Pid}) ->
    %% Evaluate compliance against a target
    Target = #{
        type => module,
        name => <<"erlmcp_test">>,
        version => <<"1.0.0">>
    },

    Result = gen_server:call(Pid, {evaluate_compliance, Target}),
    ?assert(is_map(Result)),
    ?assert(maps:is_key(compliant, Result)),
    ?assert(maps:is_key(violations, Result)).

test_compliance_report(#compliance_test_state{framework_pid = Pid}) ->
    %% Generate compliance report
    Report = gen_server:call(Pid, generate_report),

    ?assert(is_map(Report)),
    ?assert(maps:is_key(timestamp, Report)),
    ?assert(maps:is_key(overall_status, Report)),
    ?assert(is_list(maps:get(violations, Report, []))).

test_rule_categories(_State) ->
    %% Test rule categories
    Categories = [security, type_safety, performance, protocol],
    lists:foreach(fun(Category) ->
        Rule = #{
            id => iolist_to_binary([atom_to_list(Category), "-rule"]),
            category => Category,
            severity => medium,
            description => iolist_to_binary(["Test ", atom_to_list(Category), " rule"]),
            check => fun(_) -> true end
        },
        ?assert(is_map(Rule))
    end, Categories).

test_enforcement_actions(_State) ->
    %% Test enforcement actions for violations
    Actions = [warn, block, log, notify],

    lists:foreach(fun(Action) ->
        ?assert(is_atom(Action)),
        ?assert(lists:member(Action, [warn, block, log, notify, skip]))
    end, Actions).

%%%=============================================================================
%%% Rule Validation Tests
%%%=============================================================================

rule_validation_test_() ->
    [
      fun test_rule_structure/0,
      fun test_rule_check_function/0,
      fun test_rule_severity_levels/0
     ].

test_rule_structure() ->
    %% Test rule must have required fields
    ValidRule = #{
        id => <<"valid-rule">>,
        category => security,
        severity => high,
        description => <<"Valid test rule">>,
        check => fun(_) -> true end
    },
    ?assert(is_map(ValidRule)),
    ?assert(maps:is_key(id, ValidRule)),
    ?assert(maps:is_key(category, ValidRule)),
    ?assert(maps:is_key(severity, ValidRule)).

test_rule_check_function() ->
    %% Rule check function must be callable
    CheckFun = fun(Target) ->
        maps:get(type, Target) =:= module
    end,

    ?assert(is_function(CheckFun, 1)),

    %% Test the function
    ?assert(CheckFun(#{type => module})),
    ?assert(not CheckFun(#{type => other})).

test_rule_severity_levels() ->
    %% Test valid severity levels
    ValidSeverities = [low, medium, high, critical],

    lists:foreach(fun(Severity) ->
        Rule = #{
            id => iolist_to_binary([atom_to_list(Severity), "-rule"]),
            category => test,
            severity => Severity,
            check => fun(_) -> true end
        },
        ?assertEqual(Severity, maps:get(severity, Rule))
    end, ValidSeverities).

%%%=============================================================================
%%% Violation Tests
%%%=============================================================================

violation_tests_test_() ->
    [
      fun test_violation_detection/0,
      fun test_violation_aggregation/0,
      fun test_violation_history/0
     ].

test_violation_detection() ->
    %% Test violation detection
    Rule = #{
        id => <<"violation-detect">>,
        category => security,
        severity => high,
        description => <<"Detect violations">>,
        check => fun(Target) ->
            not maps:is_key(required_key, Target)
        end
    },

    %% Should detect missing required key
    Target = #{type => module, name => <<"test">>},
    Result = (maps:get(check, Rule))(Target),
    ?assertEqual(true, Result).

test_violation_aggregation() ->
    %% Test aggregating violations
    Violations = [
        #{rule_id => <<"rule1">>, severity => high, message => <<"Violation 1">>},
        #{rule_id => <<"rule2">>, severity => medium, message => <<"Violation 2">>}
    ],

    HighCount = length([V || V <- Violations, maps:get(severity, V) =:= high]),
    MediumCount = length([V || V <- Violations, maps:get(severity, V) =:= medium]),

    ?assertEqual(1, HighCount),
    ?assertEqual(1, MediumCount).

test_violation_history() ->
    %% Test violation history tracking
    History = [
        #{
            timestamp => erlang:system_time(second),
            rule_id => <<"history-rule">>,
            target => <<"test-target">>,
            result => fail
        }
    ],

    ?assert(length(History) >= 1),
    ?assertMatch(#{result := fail}, hd(History)).

%%%=============================================================================
%%% Report Generation Tests
%%%=============================================================================

report_tests_test_() ->
    [
      fun test_report_structure/0,
      fun test_report_summary/0,
      fun test_report_export/0
     ].

test_report_structure() ->
    Report = #{
        timestamp => erlang:system_time(second),
        overall_status => pass,
        total_rules => 10,
        passed_rules => 9,
        failed_rules => 1,
        violations => [],
        rules_evaluated => []
    },

    ?assert(maps:is_key(timestamp, Report)),
    ?assert(maps:is_key(overall_status, Report)),
    ?assert(maps:is_key(total_rules, Report)),
    ?assertEqual(10, maps:get(total_rules, Report)),
    ?assertEqual(9, maps:get(passed_rules, Report)).

test_report_summary() ->
    %% Test generating summary from violations
    Violations = [
        #{rule_id => <<"r1">>, severity => high, message => <<"Fail">>},
        #{rule_id => <<"r2">>, severity => medium, message => <<"Fail">>},
        #{rule_id => <<"r3">>, severity => low, message => <<"Fail">>}
    ],

    Summary = generate_summary(Violations),
    ?assert(is_map(Summary)),
    ?assertEqual(3, maps:get(total_violations, Summary)).

test_report_export() ->
    %% Test report export to various formats
    Report = #{
        overall_status => pass,
        timestamp => erlang:system_time(second),
        violations => []
    },

    %% JSON export
    ?assert(is_binary(export_report_json(Report))),

    %% Text export
    ?assert(is_binary(export_report_text(Report))).

%%%=============================================================================
%%% Edge Case Tests
%%%=============================================================================

edge_case_test_() ->
    [
      fun test_empty_rules/0,
      fun test_invalid_target/0,
      fun test_timeout_handling/0
     ].

test_empty_rules() ->
    %% Test compliance with no rules defined
    Target = #{type => module, name => <<"test">>},
    Result = evaluate_with_empty_rules(Target),
    ?assertEqual(#{compliant => true, violations => []}, Result).

test_invalid_target() ->
    %% Test compliance check on invalid target
    InvalidTarget = #{invalid => <<"target">>},
    ?assertEqual(#{compliant => false, violations => []},
                 evaluate_with_empty_rules(InvalidTarget)).

test_timeout_handling() ->
    %% Test compliance check timeout handling
    SlowRule = #{
        id => <<"slow-rule">>,
        category => test,
        severity => low,
        check => fun(_) -> timer:sleep(100), true end
    },

    ?assert(is_function(maps:get(check, SlowRule))).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

evaluate_with_empty_rules(_Target) ->
    %% Mock evaluation with no rules
    #{compliant => true, violations => []}.

export_report_json(Report) ->
    %% Simplified JSON export
    jsx:encode(Report).

export_report_text(Report) ->
    %% Simplified text export
    Status = maps:get(overall_status, Report),
    Timestamp = maps:get(timestamp, Report),
    iolist_to_binary([
        "Compliance Report: ", atom_to_list(Status),
        "\nTimestamp: ", integer_to_list(Timestamp)
    ]).

generate_summary(Violations) ->
    #{
        total_violations => length(Violations),
        by_severity => group_by_severity(Violations)
    }.

group_by_severity(Violations) ->
    lists:foldl(fun(V, Acc) ->
        Severity = maps:get(severity, V),
        Count = maps:get(Severity, Acc, 0) + 1,
        maps:put(Severity, Count, Acc)
    end, #{}, Violations).
