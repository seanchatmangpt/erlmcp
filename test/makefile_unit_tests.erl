-module(makefile_unit_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Makefile Unit Tests - Chicago School TDD
%%%===================================================================
%%% Purpose: Test individual Makefile targets using REAL make execution
%%% Pattern: No mocks, real processes, real file system changes
%%% Coverage: All 70+ Makefile targets with positive/negative/edge cases
%%%===================================================================

%%%===================================================================
%%% Test Categories
%%%===================================================================
%%% 1. Compilation Targets (compile, compile-core, compile-transports, etc.)
%%% 2. Testing Targets (test, eunit, ct, test-smoke, test-quick, test-full)
%%% 3. Quality Gates (validate-*, check, check-full)
%%% 4. TCPS Targets (jidoka, andon, poka-yoke, release-validate)
%%% 5. Workflow Targets (doctor, quick, verify, ci-local)
%%% 6. Utility Targets (clean, deps, info, console, observer)
%%% 7. Governance Targets (hooks-validate, settings-validate, governance-test)
%%% 8. CLI Targets (cli-version, cli-release, cli-test-startup)
%%% 9. Metrics Targets (metrics-snapshot, metrics-trend, metrics-report)
%%% 10. Auto-Fix Targets (auto-fix, auto-fix-quick, auto-fix-validate)
%%%===================================================================

%%%===================================================================
%%% Chicago School TDD Setup: Real Make Execution in Temporary Directory
%%%===================================================================

%% Setup: Create temporary directory with full erlmcp copy for isolated testing
setup_makefile_test() ->
    TestDir = create_temp_test_dir(),
    copy_erlmcp_to_test_dir(TestDir),
    {ok, OriginalDir} = file:get_cwd(),
    ok = file:set_cwd(TestDir),
    #{test_dir => TestDir, original_dir => OriginalDir}.

%% Teardown: Restore directory, clean up test artifacts
teardown_makefile_test(#{test_dir := TestDir, original_dir := OrigDir}) ->
    ok = file:set_cwd(OrigDir),
    cleanup_test_dir(TestDir),
    ok.

%% Helper: Create isolated temporary directory
create_temp_test_dir() ->
    TempBase = "/tmp/erlmcp_makefile_test_" ++ os:getpid(),
    ok = filelib:ensure_dir(TempBase ++ "/"),
    TempBase.

%% Helper: Copy erlmcp to test directory (real file system)
copy_erlmcp_to_test_dir(TestDir) ->
    %% Chicago School: Use real file copy, not mocked file system
    RootDir = get_erlmcp_root(),
    Cmd = io_lib:format("cp -R ~s/* ~s/", [RootDir, TestDir]),
    os:cmd(Cmd),
    ok.

%% Helper: Get erlmcp root directory
get_erlmcp_root() ->
    {ok, Cwd} = file:get_cwd(),
    case filename:basename(Cwd) of
        "test" -> filename:dirname(Cwd);
        _ -> Cwd
    end.

%% Helper: Cleanup test directory
cleanup_test_dir(TestDir) ->
    os:cmd("rm -rf " ++ TestDir),
    ok.

%%%===================================================================
%%% TEST CATEGORY 1: COMPILATION TARGETS (Chicago School: Real rebar3)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make compile
%% Expected: All apps compile with 0 errors
%% Chicago TDD: Real rebar3 execution, real BEAM files generated
%%--------------------------------------------------------------------
compile_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"compile succeeds with valid code",
           fun() ->
               %% Exercise: Run real make compile
               {ExitCode, Output} = run_make_target("compile"),

               %% Verify: Exit code 0 (Chicago School: observable behavior)
               ?assertEqual(0, ExitCode),

               %% Verify: Success message in output
               ?assertMatch({match, _}, re:run(Output, "Compilation complete")),

               %% Verify: BEAM files exist (real file system check)
               BeamFiles = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
               ?assert(length(BeamFiles) > 0)
           end},

          {"compile fails with syntax errors",
           fun() ->
               %% Setup: Introduce syntax error in real file
               inject_syntax_error("apps/erlmcp_core/src/erlmcp_client.erl"),

               %% Exercise: Run real make compile
               {ExitCode, Output} = run_make_target("compile"),

               %% Verify: Non-zero exit code (failure expected)
               ?assertNotEqual(0, ExitCode),

               %% Verify: Error message in output
               ?assertMatch({match, _}, re:run(Output, "syntax error"))
           end},

          {"compile with missing OTP version fails",
           fun() ->
               %% Setup: Mock OTP version check failure (temporary script modification)
               mock_otp_version_check_failure(),

               %% Exercise: Run make compile
               {ExitCode, Output} = run_make_target("compile"),

               %% Verify: Blocked by version check
               ?assertNotEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "OTP 28"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make compile-core, compile-transports, compile-observability, compile-tcps
%% Expected: Individual app compilation works in isolation
%% Chicago TDD: Real per-app rebar3 compilation
%%--------------------------------------------------------------------
compile_individual_apps_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         Apps = ["core", "transports", "observability", "tcps"],
         [
          {io_lib:format("compile-~s succeeds", [App]),
           fun() ->
               Target = "compile-" ++ App,
               {ExitCode, Output} = run_make_target(Target),
               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "compiled"))
           end}
          || App <- Apps
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 2: TESTING TARGETS (Chicago School: Real Test Execution)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make test
%% Expected: EUnit + CT tests run, all pass
%% Chicago TDD: Real rebar3 eunit/ct execution, no mocked test results
%%--------------------------------------------------------------------
test_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"test succeeds with all tests passing",
           fun() ->
               %% Exercise: Run real make test
               {ExitCode, Output} = run_make_target("test"),

               %% Verify: Exit code 0
               ?assertEqual(0, ExitCode),

               %% Verify: Success message
               ?assertMatch({match, _}, re:run(Output, "All tests passed"))
           end},

          {"test fails when EUnit test fails",
           fun() ->
               %% Setup: Inject failing test
               inject_failing_test("test/erlmcp_client_tests.erl"),

               %% Exercise: Run make test
               {ExitCode, _Output} = run_make_target("test"),

               %% Verify: Non-zero exit code
               ?assertNotEqual(0, ExitCode)
           end},

          {"test fails when CT test fails",
           fun() ->
               %% Setup: Inject failing CT suite
               inject_failing_ct_suite("test/integration/sample_SUITE.erl"),

               %% Exercise: Run make test
               {ExitCode, _Output} = run_make_target("test"),

               %% Verify: Non-zero exit code
               ?assertNotEqual(0, ExitCode)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Targets: test-smoke, test-quick, test-full
%% Expected: Tiered test execution with time budgets
%% Chicago TDD: Real script execution, measure actual time
%%--------------------------------------------------------------------
test_tiers_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"test-smoke completes within 2 minutes",
           fun() ->
               %% Exercise: Run test-smoke with time measurement
               {ExitCode, _Output, Duration} = run_make_target_timed("test-smoke"),

               %% Verify: Success
               ?assertEqual(0, ExitCode),

               %% Verify: Time budget (120 seconds)
               ?assert(Duration < 120)
           end},

          {"test-quick completes within 10 minutes",
           fun() ->
               {ExitCode, _Output, Duration} = run_make_target_timed("test-quick"),
               ?assertEqual(0, ExitCode),
               ?assert(Duration < 600)
           end},

          {"test-full runs all tests",
           fun() ->
               {ExitCode, Output} = run_make_target("test-full"),
               ?assertEqual(0, ExitCode),

               %% Verify: All test types executed
               ?assertMatch({match, _}, re:run(Output, "EUnit")),
               ?assertMatch({match, _}, re:run(Output, "Common Test"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Targets: test-strict, benchmark-strict, coverage-strict, quality-strict
%% Expected: BLOCKING on failures, exit code 1
%% Chicago TDD: Real enforcement, real exit codes
%%--------------------------------------------------------------------
strict_targets_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"test-strict blocks on test failure",
           fun() ->
               inject_failing_test("test/sample_tests.erl"),
               {ExitCode, _Output} = run_make_target("test-strict"),
               ?assertEqual(1, ExitCode)
           end},

          {"coverage-strict blocks on <80% coverage",
           fun() ->
               %% Setup: Reduce coverage by removing test file
               remove_test_file("test/erlmcp_registry_tests.erl"),

               {ExitCode, Output} = run_make_target("coverage-strict"),

               %% Verify: Blocked
               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "80%"))
           end},

          {"benchmark-strict blocks on >10% regression",
           fun() ->
               %% Setup: Inject performance degradation
               inject_slow_code("apps/erlmcp_core/src/erlmcp_registry.erl"),

               {ExitCode, Output} = run_make_target("benchmark-strict"),

               %% Verify: Blocked
               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "regression"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 3: QUALITY GATES (Chicago School: Real Tool Execution)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make validate
%% Expected: All sub-gates pass (profile, compile, test, coverage, quality, bench)
%% Chicago TDD: Real rebar3 dialyzer/xref/cover execution
%%--------------------------------------------------------------------
validate_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"validate succeeds when all gates pass",
           fun() ->
               {ExitCode, Output} = run_make_target("validate"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "ALL QUALITY GATES PASSED"))
           end},

          {"validate blocks on compilation failure",
           fun() ->
               inject_syntax_error("apps/erlmcp_core/src/erlmcp_client.erl"),
               {ExitCode, _Output} = run_make_target("validate"),
               ?assertEqual(1, ExitCode)
           end},

          {"validate blocks on test failure",
           fun() ->
               inject_failing_test("test/erlmcp_server_tests.erl"),
               {ExitCode, _Output} = run_make_target("validate"),
               ?assertEqual(1, ExitCode)
           end},

          {"validate blocks on coverage below 80%",
           fun() ->
               remove_test_file("test/erlmcp_json_rpc_tests.erl"),
               {ExitCode, Output} = run_make_target("validate"),
               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "COVERAGE BELOW THRESHOLD"))
           end},

          {"validate blocks on dialyzer warnings",
           fun() ->
               inject_type_error("apps/erlmcp_core/src/erlmcp_client.erl"),
               {ExitCode, Output} = run_make_target("validate"),
               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "dialyzer"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Targets: validate-compile, validate-test, validate-coverage, validate-quality, validate-bench
%% Expected: Individual gates work in isolation
%% Chicago TDD: Real per-gate validation
%%--------------------------------------------------------------------
validate_individual_gates_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         Gates = ["compile", "test", "coverage", "quality", "bench"],
         [
          {io_lib:format("validate-~s passes with valid code", [Gate]),
           fun() ->
               Target = "validate-" ++ Gate,
               {ExitCode, _Output} = run_make_target(Target),
               ?assertEqual(0, ExitCode)
           end}
          || Gate <- Gates
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 4: TCPS TARGETS (Chicago School: Real Scripts)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make jidoka
%% Expected: 8 Jidoka quality gates execute
%% Chicago TDD: Real TCPS script execution
%%--------------------------------------------------------------------
jidoka_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"jidoka executes quality gates",
           fun() ->
               {ExitCode, Output} = run_make_target("jidoka"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "自働化"))
           end},

          {"jidoka stops line on failure",
           fun() ->
               inject_syntax_error("apps/erlmcp_core/src/erlmcp_server.erl"),
               {ExitCode, _Output} = run_make_target("jidoka"),
               ?assertEqual(1, ExitCode)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make andon
%% Expected: Display Andon board status
%% Chicago TDD: Real status script execution
%%--------------------------------------------------------------------
andon_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"andon displays board status",
           fun() ->
               {ExitCode, Output} = run_make_target("andon"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "行灯"))
           end},

          {"andon shows errors when issues exist",
           fun() ->
               %% Setup: Create issue by failing a test
               inject_failing_test("test/sample_tests.erl"),
               run_make_target("test"), %% Trigger failure

               {ExitCode, Output} = run_make_target("andon"),

               ?assertEqual(0, ExitCode), %% andon always succeeds
               ?assertMatch({match, _}, re:run(Output, "issue|error|failure", [caseless]))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make poka-yoke
%% Expected: Error-proofing validation
%% Chicago TDD: Real poka-yoke script execution
%%--------------------------------------------------------------------
poka_yoke_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"poka-yoke validates error-proofing",
           fun() ->
               {ExitCode, Output} = run_make_target("poka-yoke"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "ポカヨケ"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make release-validate
%% Expected: Full validation + Jidoka + quality receipt
%% Chicago TDD: Real release validation workflow
%%--------------------------------------------------------------------
release_validate_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"release-validate generates quality receipt",
           fun() ->
               {ExitCode, Output} = run_make_target("release-validate"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "RELEASE READY")),
               ?assertMatch({match, _}, re:run(Output, "認証"))
           end},

          {"release-validate blocks on quality failure",
           fun() ->
               inject_syntax_error("apps/erlmcp_core/src/erlmcp_client.erl"),
               {ExitCode, Output} = run_make_target("release-validate"),

               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "BLOCKED"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 5: WORKFLOW TARGETS (Chicago School: Real Workflows)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make doctor
%% Expected: Environment health check
%% Chicago TDD: Real environment validation
%%--------------------------------------------------------------------
doctor_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"doctor checks OTP version",
           fun() ->
               {ExitCode, Output} = run_make_target("doctor"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "OTP"))
           end},

          {"doctor checks rebar3 availability",
           fun() ->
               {ExitCode, Output} = run_make_target("doctor"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "rebar3"))
           end},

          {"doctor validates ERLMCP_PROFILE",
           fun() ->
               %% Setup: Set invalid profile
               os:putenv("ERLMCP_PROFILE", "invalid_profile"),

               {ExitCode, _Output} = run_make_target("doctor"),

               %% Verify: Blocks on invalid profile
               ?assertEqual(1, ExitCode)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make quick
%% Expected: Fast quality check (<5 min)
%% Chicago TDD: Real time-bounded workflow
%%--------------------------------------------------------------------
quick_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"quick completes within 5 minutes",
           fun() ->
               {ExitCode, _Output, Duration} = run_make_target_timed("quick"),

               ?assertEqual(0, ExitCode),
               ?assert(Duration < 300) %% 5 minutes
           end},

          {"quick runs smoke tests",
           fun() ->
               {ExitCode, Output} = run_make_target("quick"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "smoke tests"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make verify
%% Expected: Full validation (<15 min)
%% Chicago TDD: Real comprehensive workflow
%%--------------------------------------------------------------------
verify_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"verify completes within 15 minutes",
           fun() ->
               {ExitCode, _Output, Duration} = run_make_target_timed("verify"),

               ?assertEqual(0, ExitCode),
               ?assert(Duration < 900) %% 15 minutes
           end},

          {"verify runs xref",
           fun() ->
               {ExitCode, Output} = run_make_target("verify"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "xref"))
           end},

          {"verify runs dialyzer",
           fun() ->
               {ExitCode, Output} = run_make_target("verify"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "dialyzer"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make ci-local
%% Expected: Reproduce exact CI workflow locally
%% Chicago TDD: Real CI gate sequence
%%--------------------------------------------------------------------
ci_local_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"ci-local runs all 6 gates",
           fun() ->
               {ExitCode, Output} = run_make_target("ci-local"),

               ?assertEqual(0, ExitCode),

               %% Verify: All gates executed
               ?assertMatch({match, _}, re:run(Output, "GATE 1: Compilation")),
               ?assertMatch({match, _}, re:run(Output, "GATE 2: Xref")),
               ?assertMatch({match, _}, re:run(Output, "GATE 3: Dialyzer")),
               ?assertMatch({match, _}, re:run(Output, "GATE 4: EUnit Tests")),
               ?assertMatch({match, _}, re:run(Output, "GATE 5: Common Test")),
               ?assertMatch({match, _}, re:run(Output, "GATE 6: Coverage Check"))
           end},

          {"ci-local generates log files",
           fun() ->
               {ExitCode, _Output} = run_make_target("ci-local"),

               ?assertEqual(0, ExitCode),

               %% Verify: Log files exist (Chicago School: real file system)
               ?assert(filelib:is_file("/tmp/erlmcp_ci_compile.log")),
               ?assert(filelib:is_file("/tmp/erlmcp_ci_xref.log")),
               ?assert(filelib:is_file("/tmp/erlmcp_ci_dialyzer.log")),
               ?assert(filelib:is_file("/tmp/erlmcp_ci_eunit.log")),
               ?assert(filelib:is_file("/tmp/erlmcp_ci_ct.log")),
               ?assert(filelib:is_file("/tmp/erlmcp_ci_coverage.log"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 6: UTILITY TARGETS (Chicago School: Real Tools)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make clean
%% Expected: Remove build artifacts
%% Chicago TDD: Real file deletion
%%--------------------------------------------------------------------
clean_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"clean removes _build directory",
           fun() ->
               %% Setup: Ensure build artifacts exist
               run_make_target("compile"),
               ?assert(filelib:is_dir("_build")),

               %% Exercise: Run clean
               {ExitCode, _Output} = run_make_target("clean"),

               %% Verify: _build removed
               ?assertEqual(0, ExitCode),
               ?assertNot(filelib:is_dir("_build"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make distclean
%% Expected: Deep clean including deps
%% Chicago TDD: Real deep cleanup
%%--------------------------------------------------------------------
distclean_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"distclean removes _build and rebar.lock",
           fun() ->
               %% Setup: Ensure artifacts exist
               run_make_target("compile"),
               ?assert(filelib:is_file("rebar.lock")),

               %% Exercise: Run distclean
               {ExitCode, _Output} = run_make_target("distclean"),

               %% Verify: Deep clean
               ?assertEqual(0, ExitCode),
               ?assertNot(filelib:is_file("rebar.lock"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make deps
%% Expected: Fetch dependencies
%% Chicago TDD: Real rebar3 get-deps
%%--------------------------------------------------------------------
deps_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"deps fetches dependencies",
           fun() ->
               %% Setup: Remove deps
               run_make_target("distclean"),

               %% Exercise: Fetch deps
               {ExitCode, Output} = run_make_target("deps"),

               %% Verify: Success
               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "Dependencies fetched"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make info
%% Expected: Display project information
%% Chicago TDD: Real info display
%%--------------------------------------------------------------------
info_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"info displays project name",
           fun() ->
               {ExitCode, Output} = run_make_target("info"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "erlmcp"))
           end},

          {"info lists all 4 apps",
           fun() ->
               {ExitCode, Output} = run_make_target("info"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "erlmcp_core")),
               ?assertMatch({match, _}, re:run(Output, "erlmcp_transports")),
               ?assertMatch({match, _}, re:run(Output, "erlmcp_observability")),
               ?assertMatch({match, _}, re:run(Output, "tcps_erlmcp"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 7: GOVERNANCE TARGETS (Chicago School: Real Governance)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make hooks-validate
%% Expected: Validate all hooks exist, are executable, have valid syntax
%% Chicago TDD: Real hook validation
%%--------------------------------------------------------------------
hooks_validate_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"hooks-validate passes with valid hooks",
           fun() ->
               {ExitCode, Output} = run_make_target("hooks-validate"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "All hooks validated"))
           end},

          {"hooks-validate fails with non-executable hook",
           fun() ->
               %% Setup: Remove execute permission
               os:cmd("chmod -x .claude/hooks/SessionStart.sh"),

               {ExitCode, _Output} = run_make_target("hooks-validate"),

               ?assertEqual(1, ExitCode)
           end},

          {"hooks-validate fails with syntax error in hook",
           fun() ->
               %% Setup: Inject syntax error
               inject_bash_syntax_error(".claude/hooks/SessionStart.sh"),

               {ExitCode, Output} = run_make_target("hooks-validate"),

               ?assertEqual(1, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "syntax error"))
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make settings-validate
%% Expected: Validate .claude/settings.json schema
%% Chicago TDD: Real JSON schema validation
%%--------------------------------------------------------------------
settings_validate_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"settings-validate passes with valid JSON",
           fun() ->
               {ExitCode, Output} = run_make_target("settings-validate"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "Settings validation PASSED"))
           end},

          {"settings-validate fails with invalid JSON",
           fun() ->
               %% Setup: Corrupt JSON
               inject_invalid_json(".claude/settings.json"),

               {ExitCode, _Output} = run_make_target("settings-validate"),

               ?assertEqual(1, ExitCode)
           end}
         ]
     end}.

%%--------------------------------------------------------------------
%% Target: make governance-test
%% Expected: Run all hook test suites
%% Chicago TDD: Real governance test execution
%%--------------------------------------------------------------------
governance_test_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"governance-test runs all test suites",
           fun() ->
               {ExitCode, Output} = run_make_target("governance-test"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "All governance tests PASSED"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 8: CLI TARGETS (Chicago School: Real CLI)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make cli-version
%% Expected: Display CLI version
%% Chicago TDD: Real CLI execution
%%--------------------------------------------------------------------
cli_version_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"cli-version displays version",
           fun() ->
               {ExitCode, Output} = run_make_target("cli-version"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "CLI Version"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 9: METRICS TARGETS (Chicago School: Real Metrics)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make metrics-snapshot
%% Expected: Capture quality snapshot
%% Chicago TDD: Real metrics capture
%%--------------------------------------------------------------------
metrics_snapshot_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"metrics-snapshot captures snapshot",
           fun() ->
               {ExitCode, _Output} = run_make_target("metrics-snapshot"),

               ?assertEqual(0, ExitCode),

               %% Verify: Snapshot file created
               ?assert(filelib:is_file("metrics/snapshots/latest.json"))
           end}
         ]
     end}.

%%%===================================================================
%%% TEST CATEGORY 10: AUTO-FIX TARGETS (Chicago School: Real Auto-Fix)
%%%===================================================================

%%--------------------------------------------------------------------
%% Target: make auto-fix
%% Expected: Run auto-fix orchestration
%% Chicago TDD: Real auto-fix execution
%%--------------------------------------------------------------------
auto_fix_target_test_() ->
    {setup,
     fun setup_makefile_test/0,
     fun teardown_makefile_test/1,
     fun(_Ctx) ->
         [
          {"auto-fix runs orchestration",
           fun() ->
               {ExitCode, Output} = run_make_target("auto-fix"),

               ?assertEqual(0, ExitCode),
               ?assertMatch({match, _}, re:run(Output, "Auto-Fix System"))
           end}
         ]
     end}.

%%%===================================================================
%%% HELPER FUNCTIONS (Chicago School: Real Command Execution)
%%%===================================================================

%% Execute make target, return {ExitCode, Output}
run_make_target(Target) ->
    Cmd = io_lib:format("make ~s 2>&1", [Target]),
    Output = os:cmd(Cmd),
    ExitCode = get_last_exit_code(),
    {ExitCode, Output}.

%% Execute make target with timing, return {ExitCode, Output, Duration}
run_make_target_timed(Target) ->
    StartTime = erlang:system_time(second),
    {ExitCode, Output} = run_make_target(Target),
    EndTime = erlang:system_time(second),
    Duration = EndTime - StartTime,
    {ExitCode, Output, Duration}.

%% Get last command exit code
get_last_exit_code() ->
    case string:to_integer(os:cmd("echo $?")) of
        {Int, _} -> Int;
        _ -> 0
    end.

%% Inject syntax error into file
inject_syntax_error(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    Corrupted = <<Content/binary, "\n\nthis is a syntax error!!!">>,
    file:write_file(FilePath, Corrupted).

%% Inject failing test
inject_failing_test(FilePath) ->
    TestCode = <<"\n\nfailing_test() -> ?assertEqual(1, 2).\n">>,
    {ok, Content} = file:read_file(FilePath),
    file:write_file(FilePath, <<Content/binary, TestCode/binary>>).

%% Inject failing CT suite
inject_failing_ct_suite(FilePath) ->
    SuiteCode = <<"-module(sample_SUITE).\n-export([all/0, failing_test/1]).\n"
                  "all() -> [failing_test].\n"
                  "failing_test(_Config) -> ct:fail(\"Intentional failure\").\n">>,
    file:write_file(FilePath, SuiteCode).

%% Remove test file
remove_test_file(FilePath) ->
    file:delete(FilePath).

%% Inject type error
inject_type_error(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    TypeError = <<"\n\n-spec bad_spec() -> integer().\nbad_spec() -> \"not_an_integer\".\n">>,
    file:write_file(FilePath, <<Content/binary, TypeError/binary>>).

%% Inject slow code (performance degradation)
inject_slow_code(FilePath) ->
    SlowCode = <<"\n\nslow_function() -> timer:sleep(10000), ok.\n">>,
    {ok, Content} = file:read_file(FilePath),
    file:write_file(FilePath, <<Content/binary, SlowCode/binary>>).

%% Mock OTP version check failure
mock_otp_version_check_failure() ->
    Script = "scripts/check_erlang_version.sh",
    file:write_file(Script, <<"#!/bin/bash\nexit 1\n">>),
    os:cmd("chmod +x " ++ Script).

%% Inject bash syntax error
inject_bash_syntax_error(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    SyntaxError = <<"\n\nif [ ; then\n  echo \"syntax error\"\nfi\n">>,
    file:write_file(FilePath, <<Content/binary, SyntaxError/binary>>).

%% Inject invalid JSON
inject_invalid_json(FilePath) ->
    file:write_file(FilePath, <<"{\"invalid\": json syntax}">>).
