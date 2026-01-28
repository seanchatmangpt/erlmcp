%%%-----------------------------------------------------------------------------
%%% @doc erlmcp doctor CLI - EUnit Tests
%%%
%%% Unit tests for erlmcp_cli_doctor diagnostic command.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_cli_doctor_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Doctor Report Generation Tests
%%====================================================================

check_all_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_check_all_returns_report()),
             ?_test(test_check_all_with_text_format()),
             ?_test(test_check_all_with_json_format()),
             ?_test(test_report_contains_12_checks()),
             ?_test(test_all_checks_have_details()),
             ?_test(test_report_has_summary()),
             ?_test(test_report_has_overall_status())
         ]
     end}.

test_check_all_returns_report() ->
    Report = erlmcp_cli_doctor:check_all(text),
    ?assert(is_tuple(Report)),
    ?assertEqual(doctor_report, element(1, Report)).

test_check_all_with_text_format() ->
    Report = erlmcp_cli_doctor:check_all(text),
    ?assertNotEqual(undefined, Report),
    % Report should be a record with checks
    ?assert(is_list(element(4, Report))).

test_check_all_with_json_format() ->
    Report = erlmcp_cli_doctor:check_all(json),
    ?assertNotEqual(undefined, Report),
    % Format doesn't affect report structure, just output
    ?assert(is_list(element(4, Report))).

test_report_contains_12_checks() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    ?assertEqual(12, length(Checks)).

test_all_checks_have_details() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    lists:foreach(fun({Name, Status, Details, Remediation}) ->
        ?assert(is_list(Name) orelse is_binary(Name)),
        ?assert(lists:member(Status, [ok, warning, critical])),
        ?assert(is_list(Details) orelse is_binary(Details)),
        ?assert(is_list(Remediation) orelse is_binary(Remediation))
    end, Checks).

test_report_has_summary() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Summary = element(5, Report),
    ?assert(is_map(Summary)),
    ?assertNotEqual(undefined, maps:get(total, Summary, undefined)),
    ?assertNotEqual(undefined, maps:get(passed, Summary, undefined)),
    ?assertNotEqual(undefined, maps:get(warnings, Summary, undefined)),
    ?assertNotEqual(undefined, maps:get(critical, Summary, undefined)).

test_report_has_overall_status() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Status = element(6, Report),
    ?assert(lists:member(Status, [ok, warning, critical])).

%%====================================================================
%% Exit Code Tests
%%====================================================================

exit_code_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_exit_code_0_for_ok()),
             ?_test(test_exit_code_1_for_warning()),
             ?_test(test_exit_code_2_for_critical())
         ]
     end}.

test_exit_code_0_for_ok() ->
    Report = {doctor_report, calendar:local_time(), "host", [], #{total => 0, passed => 0, warnings => 0, critical => 0}, ok},
    ExitCode = erlmcp_cli_doctor:get_exit_code(Report),
    ?assertEqual(0, ExitCode).

test_exit_code_1_for_warning() ->
    Report = {doctor_report, calendar:local_time(), "host", [], #{total => 1, passed => 0, warnings => 1, critical => 0}, warning},
    ExitCode = erlmcp_cli_doctor:get_exit_code(Report),
    ?assertEqual(1, ExitCode).

test_exit_code_2_for_critical() ->
    Report = {doctor_report, calendar:local_time(), "host", [], #{total => 1, passed => 0, warnings => 0, critical => 1}, critical},
    ExitCode = erlmcp_cli_doctor:get_exit_code(Report),
    ?assertEqual(2, ExitCode).

%%====================================================================
%% Determinism Tests
%%====================================================================

determinism_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_check_all_is_deterministic()),
             ?_test(test_check_all_returns_consistent_check_count())
         ]
     end}.

test_check_all_is_deterministic() ->
    Reports = [erlmcp_cli_doctor:check_all(text) || _ <- lists:seq(1, 5)],

    % Extract check names from each report
    CheckLists = [element(4, R) || R <- Reports],
    CheckNameLists = [[Name || {Name, _, _, _} <- Checks] || Checks <- CheckLists],

    % All should be identical
    FirstNames = lists:nth(1, CheckNameLists),
    lists:foreach(fun(Names) ->
        ?assertEqual(FirstNames, Names)
    end, CheckNameLists).

test_check_all_returns_consistent_check_count() ->
    Reports = [erlmcp_cli_doctor:check_all(text) || _ <- lists:seq(1, 5)],
    CheckCounts = [length(element(4, R)) || R <- Reports],

    % All should be 12
    lists:foreach(fun(Count) ->
        ?assertEqual(12, Count)
    end, CheckCounts).

%%====================================================================
%% Individual Check Verification Tests
%%====================================================================

checks_present_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_erlang_version_check_present()),
             ?_test(test_rebar3_check_present()),
             ?_test(test_file_descriptor_check_present()),
             ?_test(test_tls_environment_check_present()),
             ?_test(test_otel_environment_check_present()),
             ?_test(test_container_runtime_check_present()),
             ?_test(test_tcp_parameters_check_present()),
             ?_test(test_disk_space_check_present())
         ]
     end}.

test_erlang_version_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:member("Erlang/OTP Version", CheckNames)).

test_rebar3_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "rebar3") =/= nomatch end, CheckNames)).

test_file_descriptor_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "File Descriptor") =/= nomatch end, CheckNames)).

test_tls_environment_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "TLS") =/= nomatch end, CheckNames)).

test_otel_environment_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "OTEL") =/= nomatch end, CheckNames)).

test_container_runtime_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "Container") =/= nomatch end, CheckNames)).

test_tcp_parameters_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "TCP") =/= nomatch end, CheckNames)).

test_disk_space_check_present() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    CheckNames = [Name || {Name, _, _, _} <- Checks],
    ?assert(lists:any(fun(Name) -> string:find(Name, "Disk") =/= nomatch end, CheckNames)).

%%====================================================================
%% Summary Statistics Tests
%%====================================================================

summary_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_summary_total_matches_check_count()),
             ?_test(test_summary_stats_are_non_negative()),
             ?_test(test_summary_passed_plus_warnings_plus_critical_equals_total())
         ]
     end}.

test_summary_total_matches_check_count() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = element(4, Report),
    Summary = element(5, Report),

    ?assertEqual(length(Checks), maps:get(total, Summary)).

test_summary_stats_are_non_negative() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Summary = element(5, Report),

    ?assert(maps:get(total, Summary) >= 0),
    ?assert(maps:get(passed, Summary) >= 0),
    ?assert(maps:get(warnings, Summary) >= 0),
    ?assert(maps:get(critical, Summary) >= 0).

test_summary_passed_plus_warnings_plus_critical_equals_total() ->
    Report = erlmcp_cli_doctor:check_all(text),
    Summary = element(5, Report),

    Total = maps:get(total, Summary),
    Passed = maps:get(passed, Summary),
    Warnings = maps:get(warnings, Summary),
    Critical = maps:get(critical, Summary),

    ?assertEqual(Total, Passed + Warnings + Critical).

%%====================================================================
%% CLI Integration Tests
%%====================================================================

cli_integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_run_with_empty_args()),
             ?_test(test_run_returns_ok())
         ]
     end}.

test_run_with_empty_args() ->
    % Should not throw
    ?assert(erlmcp_cli_doctor:run([]) =:= ok).

test_run_returns_ok() ->
    Result = erlmcp_cli_doctor:run([]),
    ?assertEqual(ok, Result).
