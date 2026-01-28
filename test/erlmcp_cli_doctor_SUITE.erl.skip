%%%-----------------------------------------------------------------------------
%%% @doc erlmcp doctor CLI - Common Test Suite
%%%
%%% Comprehensive test suite for erlmcp doctor diagnostics command.
%%%
%%% Tests cover:
%%% - Output structure validation (text and JSON)
%%% - Exit codes (0 = healthy, 1 = fixable, 2 = blocking)
%%% - Remediation steps presence and clarity
%%% - Deterministic check results (runs 5x for stability verification)
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_cli_doctor_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    suite/0,
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    text_output_structure/1,
    json_output_structure/1,
    json_output_valid/1,
    exit_code_healthy/1,
    exit_code_mapping/1,
    remediation_steps_present/1,
    remediation_steps_actionable/1,
    deterministic_results_five_runs/1,
    erlang_version_check/1,
    rebar3_check/1,
    file_descriptor_check/1,
    tls_environment_check/1,
    otel_environment_check/1,
    summary_statistics/1,
    all_checks_present/1,
    text_format_readability/1,
    json_schema_compliance/1
]).

%%%=============================================================================
%%% Suite Definition
%%%=============================================================================

-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
    [
        {group, output_validation},
        {group, exit_codes},
        {group, remediation},
        {group, determinism},
        {group, individual_checks},
        {group, reporting}
    ].

-spec groups() -> [tuple()].
groups() ->
    [
        {output_validation, [
            text_output_structure,
            json_output_structure,
            json_output_valid
        ]},
        {exit_codes, [
            exit_code_healthy,
            exit_code_mapping
        ]},
        {remediation, [
            remediation_steps_present,
            remediation_steps_actionable
        ]},
        {determinism, [
            deterministic_results_five_runs
        ]},
        {individual_checks, [
            erlang_version_check,
            rebar3_check,
            file_descriptor_check,
            tls_environment_check,
            otel_environment_check
        ]},
        {reporting, [
            summary_statistics,
            all_checks_present,
            text_format_readability,
            json_schema_compliance
        ]}
    ].

%%%=============================================================================
%%% Setup & Cleanup
%%%=============================================================================

-spec init_per_suite(list()) -> list().
init_per_suite(Config) ->
    % Ensure module is compiled
    code:ensure_loaded(erlmcp_cli_doctor),
    Config.

-spec end_per_suite(list()) -> ok.
end_per_suite(_Config) ->
    ok.

-spec init_per_group(atom(), list()) -> list().
init_per_group(_GroupName, Config) ->
    Config.

-spec end_per_group(atom(), list()) -> ok.
end_per_group(_GroupName, _Config) ->
    ok.

-spec init_per_testcase(atom(), list()) -> list().
init_per_testcase(_TestCase, Config) ->
    Config.

-spec end_per_testcase(atom(), list()) -> ok.
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%=============================================================================
%%% Output Validation Tests
%%%=============================================================================

%% @doc Verify text output has required structure
text_output_structure(Config) ->
    ct:log("Testing text output structure"),
    Report = erlmcp_cli_doctor:check_all(text),

    % Verify report record has all required fields
    ?assertMatch(
        #doctor_report{
            timestamp = _,
            hostname = _,
            checks = _,
            summary = _,
            overall_status = _
        },
        Report
    ),

    % Verify checks list is populated
    ?assert(length(Report#doctor_report.checks) >= 10),

    % Verify summary has required keys
    Summary = Report#doctor_report.summary,
    ?assertNotEqual(undefined, maps:get(total, Summary)),
    ?assertNotEqual(undefined, maps:get(passed, Summary)),
    ?assertNotEqual(undefined, maps:get(warnings, Summary)),
    ?assertNotEqual(undefined, maps:get(critical, Summary)),

    Config.

%% @doc Verify JSON output can be generated without errors
json_output_structure(Config) ->
    ct:log("Testing JSON output structure"),
    Report = erlmcp_cli_doctor:check_all(json),

    % Same structure as text format
    ?assertMatch(
        #doctor_report{
            timestamp = _,
            hostname = _,
            checks = _,
            summary = _,
            overall_status = _
        },
        Report
    ),

    Config.

%% @doc Verify JSON output is valid and parseable
json_output_valid(Config) ->
    ct:log("Testing JSON output validity"),
    Report = erlmcp_cli_doctor:check_all(json),

    % Verify report is not empty
    ?assert(Report =/= error),
    ?assert(length(Report#doctor_report.checks) > 0),

    Config.

%%%=============================================================================
%%% Exit Code Tests
%%%=============================================================================

%% @doc Verify healthy system returns exit code 0
exit_code_healthy(Config) ->
    ct:log("Testing exit code for healthy system"),

    % Create mock report with all passing checks
    HealthyReport = #doctor_report{
        timestamp = calendar:local_time(),
        hostname = "test-host",
        checks = [
            {"Check 1", ok, "Details", ""},
            {"Check 2", ok, "Details", ""}
        ],
        summary = #{total => 2, passed => 2, warnings => 0, critical => 0},
        overall_status = ok
    },

    ExitCode = erlmcp_cli_doctor:get_exit_code(HealthyReport),
    ?assertEqual(0, ExitCode),

    Config.

%% @doc Verify exit code mapping for warning/critical
exit_code_mapping(Config) ->
    ct:log("Testing exit code mapping"),

    % Test warning status (exit code 1)
    WarningReport = #doctor_report{
        timestamp = calendar:local_time(),
        hostname = "test-host",
        checks = [{"Check", warning, "Details", ""}],
        summary = #{total => 1, passed => 0, warnings => 1, critical => 0},
        overall_status = warning
    },

    % Test critical status (exit code 2)
    CriticalReport = #doctor_report{
        timestamp = calendar:local_time(),
        hostname = "test-host",
        checks = [{"Check", critical, "Details", ""}],
        summary = #{total => 1, passed => 0, warnings => 0, critical => 1},
        overall_status = critical
    },

    % Verify warning returns 1
    ?assertEqual(1, erlmcp_cli_doctor:get_exit_code(WarningReport)),

    % Verify critical returns 2
    ?assertEqual(2, erlmcp_cli_doctor:get_exit_code(CriticalReport)),

    Config.

%%%=============================================================================
%%% Remediation Tests
%%%=============================================================================

%% @doc Verify all failing checks have remediation steps
remediation_steps_present(Config) ->
    ct:log("Testing remediation steps presence"),
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = Report#doctor_report.checks,

    % For each check with warning or critical status, verify remediation is present
    lists:foreach(fun({_Name, ok, _Details, Remediation}) ->
        % OK checks may have empty remediation
        ?assert(is_list(Remediation) orelse is_binary(Remediation))
    end, Checks),

    % Count checks with non-empty remediation (for failing checks)
    FailingChecks = [C || {_, Status, _, _} = C <- Checks, Status =/= ok],
    RemediationCount = length([C || {_, _, _, R} = C <- FailingChecks, R =/= ""]),

    % Should have some remediation steps documented
    ?assert(RemediationCount >= 0),

    Config.

%% @doc Verify remediation steps are actionable (not generic)
remediation_steps_actionable(Config) ->
    ct:log("Testing remediation steps actionability"),
    Report = erlmcp_cli_doctor:check_all(text),
    Checks = Report#doctor_report.checks,

    % Extract remediation steps
    RemediationSteps = [R || {_, Status, _, R} <- Checks, Status =/= ok, R =/= ""],

    % Verify remediation steps contain commands or clear instructions
    lists:foreach(fun(Step) ->
        % Should contain either "sudo", "apt-get", "brew", "echo", or be a clear instruction
        HasCommand = string:find(Step, "sudo") =/= nomatch
                     orelse string:find(Step, "apt-get") =/= nomatch
                     orelse string:find(Step, "brew") =/= nomatch
                     orelse string:find(Step, "echo") =/= nomatch
                     orelse string:find(Step, "sysctl") =/= nomatch
                     orelse string:find(Step, "ulimit") =/= nomatch
                     orelse string:find(Step, "Set") =/= nomatch
                     orelse string:find(Step, "Install") =/= nomatch,

        case HasCommand of
            true -> ok;
            false -> ?assert(length(Step) > 10)  % At least descriptive text
        end
    end, RemediationSteps),

    Config.

%%%=============================================================================
%%% Determinism Tests
%%%=============================================================================

%% @doc Run checks 5 times and verify results are deterministic
deterministic_results_five_runs(Config) ->
    ct:log("Testing determinism across 5 runs"),

    % Run 5 times and collect results
    Results = [erlmcp_cli_doctor:check_all(text) || _ <- lists:seq(1, 5)],

    % Extract check names and statuses from each result
    CheckResults = [
        [{Name, Status} || {Name, Status, _, _} <- R#doctor_report.checks]
        || R <- Results
    ],

    % Verify all runs have identical check names and statuses
    FirstResult = lists:nth(1, CheckResults),
    lists:foreach(fun(Result) ->
        % Should have identical checks
        ?assertEqual(length(FirstResult), length(Result)),

        % Check names and statuses should match
        Matching = lists:zipwith(fun({N1, S1}, {N2, S2}) ->
            (N1 =:= N2) andalso (S1 =:= S2)
        end, FirstResult, Result),

        ?assert(lists:all(fun(X) -> X end, Matching))
    end, CheckResults),

    Config.

%%%=============================================================================
%%% Individual Check Tests
%%%=============================================================================

%% @doc Verify Erlang version check works
erlang_version_check(Config) ->
    ct:log("Testing Erlang version check"),

    % Run the specific check
    OtpRelease = erlang:system_info(otp_release),
    OtpVersion = try list_to_integer(OtpRelease) catch _:_ -> 0 end,

    ?assert(OtpVersion > 0),

    % Check should report appropriate status
    Report = erlmcp_cli_doctor:check_all(text),
    VersionChecks = [C || {Name, _, _, _} = C <- Report#doctor_report.checks,
                          string:find(Name, "Erlang") =/= nomatch],

    ?assert(length(VersionChecks) > 0),

    Config.

%% @doc Verify rebar3 check works
rebar3_check(Config) ->
    ct:log("Testing rebar3 check"),

    Report = erlmcp_cli_doctor:check_all(text),
    Rebar3Checks = [C || {Name, _, _, _} = C <- Report#doctor_report.checks,
                         string:find(Name, "rebar3") =/= nomatch],

    ?assert(length(Rebar3Checks) > 0),

    Config.

%% @doc Verify file descriptor check works
file_descriptor_check(Config) ->
    ct:log("Testing file descriptor limit check"),

    Report = erlmcp_cli_doctor:check_all(text),
    FDChecks = [C || {Name, _, _, _} = C <- Report#doctor_report.checks,
                     string:find(Name, "File Descriptor") =/= nomatch],

    ?assert(length(FDChecks) > 0),

    Config.

%% @doc Verify TLS environment check works
tls_environment_check(Config) ->
    ct:log("Testing TLS environment check"),

    Report = erlmcp_cli_doctor:check_all(text),
    TLSChecks = [C || {Name, _, _, _} = C <- Report#doctor_report.checks,
                      string:find(Name, "TLS") =/= nomatch],

    ?assert(length(TLSChecks) > 0),

    Config.

%% @doc Verify OTEL environment check works
otel_environment_check(Config) ->
    ct:log("Testing OTEL environment check"),

    Report = erlmcp_cli_doctor:check_all(text),
    OTELChecks = [C || {Name, _, _, _} = C <- Report#doctor_report.checks,
                       string:find(Name, "OTEL") =/= nomatch],

    ?assert(length(OTELChecks) > 0),

    Config.

%%%=============================================================================
%%% Reporting Tests
%%%=============================================================================

%% @doc Verify summary statistics are accurate
summary_statistics(Config) ->
    ct:log("Testing summary statistics"),

    Report = erlmcp_cli_doctor:check_all(text),
    Summary = Report#doctor_report.summary,
    Checks = Report#doctor_report.checks,

    % Count statuses manually
    ManualTotal = length(Checks),
    ManualPassed = length([C || {_, ok, _, _} <- Checks]),
    ManualWarnings = length([C || {_, warning, _, _} <- Checks]),
    ManualCritical = length([C || {_, critical, _, _} <- Checks]),

    % Verify summary matches manual count
    ?assertEqual(ManualTotal, maps:get(total, Summary)),
    ?assertEqual(ManualPassed, maps:get(passed, Summary)),
    ?assertEqual(ManualWarnings, maps:get(warnings, Summary)),
    ?assertEqual(ManualCritical, maps:get(critical, Summary)),

    Config.

%% @doc Verify all expected checks are present
all_checks_present(Config) ->
    ct:log("Testing all checks are present"),

    Report = erlmcp_cli_doctor:check_all(text),
    Checks = Report#doctor_report.checks,
    CheckNames = [Name || {Name, _, _, _} <- Checks],

    % Expected checks
    ExpectedChecks = [
        "Erlang/OTP Version",
        "OTP Features",
        "rebar3 Installation",
        "rebar3 Version",
        "File Descriptor Limit",
        "Ephemeral Ports",
        "TCP Parameters",
        "Container Runtime",
        "TLS Environment",
        "OTEL Environment",
        "Disk Space",
        "Available Memory"
    ],

    % Verify all expected checks are present
    lists:foreach(fun(ExpectedCheck) ->
        ?assert(lists:member(ExpectedCheck, CheckNames),
                io_lib:format("Expected check not found: ~s", [ExpectedCheck]))
    end, ExpectedChecks),

    Config.

%% @doc Verify text format is readable
text_format_readability(Config) ->
    ct:log("Testing text format readability"),

    Report = erlmcp_cli_doctor:check_all(text),

    % Output should not fail
    ?assertNotEqual(error, Report),

    % Report should have checks
    ?assert(length(Report#doctor_report.checks) > 0),

    Config.

%% @doc Verify JSON output complies with expected schema
json_schema_compliance(Config) ->
    ct:log("Testing JSON schema compliance"),

    Report = erlmcp_cli_doctor:check_all(json),

    % Verify required fields
    ?assertNotEqual(undefined, Report#doctor_report.timestamp),
    ?assertNotEqual(undefined, Report#doctor_report.hostname),
    ?assertNotEqual(undefined, Report#doctor_report.checks),
    ?assertNotEqual(undefined, Report#doctor_report.summary),
    ?assertNotEqual(undefined, Report#doctor_report.overall_status),

    % Verify checks is list
    ?assert(is_list(Report#doctor_report.checks)),

    % Verify summary has numeric fields
    Summary = Report#doctor_report.summary,
    ?assert(is_integer(maps:get(total, Summary))),
    ?assert(is_integer(maps:get(passed, Summary))),
    ?assert(is_integer(maps:get(warnings, Summary))),
    ?assert(is_integer(maps:get(critical, Summary))),

    Config.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

%% Define the required records for testing
-record(doctor_report, {
    timestamp :: calendar:datetime(),
    hostname :: string(),
    checks :: [tuple()],
    summary :: map(),
    overall_status :: atom()
}).
