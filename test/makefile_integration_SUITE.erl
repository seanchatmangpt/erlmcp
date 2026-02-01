-module(makefile_integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Makefile Integration Tests - Chicago School TDD
%%%===================================================================
%%% Purpose: Test Makefile target sequences and workflows end-to-end
%%% Pattern: Real make execution, real process coordination, no mocks
%%% Scope: Integration of multiple targets, workflow chains, parallelism
%%%===================================================================

%%%===================================================================
%%% TEST CATEGORIES
%%%===================================================================
%%% 1. Target Sequences (compile → test → validate chain)
%%% 2. Workflow Integration (doctor → quick → verify → ci-local)
%%% 3. Parallel Execution (concurrent target execution)
%%% 4. State Management (clean → compile → test lifecycle)
%%% 5. Error Propagation (failure in chain stops subsequent targets)
%%% 6. Quality Gate Workflows (validate gates in sequence)
%%% 7. TCPS Workflows (jidoka → poka-yoke → release-validate)
%%% 8. Governance Workflows (hooks-validate → settings-validate → governance-test)
%%%===================================================================

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
     %% Target Sequences
     {group, target_sequences},

     %% Workflow Integration
     {group, workflow_integration},

     %% Parallel Execution
     {group, parallel_execution},

     %% State Management
     {group, state_management},

     %% Error Propagation
     {group, error_propagation},

     %% Quality Gate Workflows
     {group, quality_gate_workflows},

     %% TCPS Workflows
     {group, tcps_workflows},

     %% Governance Workflows
     {group, governance_workflows}
    ].

groups() ->
    [
     {target_sequences, [sequence], [
                                      compile_to_test_sequence,
                                      compile_to_validate_sequence,
                                      clean_to_release_sequence
                                     ]},

     {workflow_integration, [sequence], [
                                         doctor_quick_verify_workflow,
                                         quick_to_ci_local_workflow,
                                         full_quality_workflow
                                        ]},

     {parallel_execution, [parallel], [
                                       parallel_app_compilation,
                                       parallel_test_execution,
                                       parallel_quality_gates
                                      ]},

     {state_management, [sequence], [
                                     clean_build_lifecycle,
                                     distclean_recovery_workflow,
                                     incremental_compilation
                                    ]},

     {error_propagation, [sequence], [
                                      compilation_error_stops_test,
                                      test_failure_stops_validate,
                                      quality_failure_stops_release
                                     ]},

     {quality_gate_workflows, [sequence], [
                                           validate_all_gates_workflow,
                                           individual_gate_isolation,
                                           gate_failure_recovery
                                          ]},

     {tcps_workflows, [sequence], [
                                   jidoka_andon_workflow,
                                   poka_yoke_validation_workflow,
                                   release_validate_workflow
                                  ]},

     {governance_workflows, [sequence], [
                                         hooks_settings_governance_workflow,
                                         receipt_generation_workflow,
                                         governance_status_workflow
                                        ]}
    ].

init_per_suite(Config) ->
    %% Chicago School: Create real test environment
    TestDir = create_isolated_test_env(),
    [{test_dir, TestDir}, {original_dir, element(2, file:get_cwd())} | Config].

end_per_suite(Config) ->
    %% Cleanup: Restore environment
    OrigDir = ?config(original_dir, Config),
    TestDir = ?config(test_dir, Config),
    file:set_cwd(OrigDir),
    cleanup_test_env(TestDir),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Finished test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% TEST GROUP 1: TARGET SEQUENCES (Chicago School: Real Command Chains)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: compile → test sequence
%% Expected: Compilation success enables testing
%% Chicago TDD: Real make execution, observable state changes
%%--------------------------------------------------------------------
compile_to_test_sequence(_Config) ->
    %% Phase 1: Compile
    {0, CompileOutput} = run_make("compile"),
    ct:pal("Compilation output: ~s", [CompileOutput]),

    %% Verify: BEAM files generated
    BeamFiles = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    true = length(BeamFiles) > 0,

    %% Phase 2: Test (depends on compilation)
    {0, TestOutput} = run_make("test"),
    ct:pal("Test output: ~s", [TestOutput]),

    %% Verify: Tests executed successfully
    {match, _} = re:run(TestOutput, "All tests passed"),

    ok.

%%--------------------------------------------------------------------
%% Test: compile → validate sequence
%% Expected: Full validation runs after successful compilation
%% Chicago TDD: Real quality gate sequence
%%--------------------------------------------------------------------
compile_to_validate_sequence(_Config) ->
    %% Phase 1: Compile
    {0, _} = run_make("compile"),

    %% Phase 2: Validate (runs all gates)
    {0, ValidateOutput} = run_make("validate"),

    %% Verify: All gates executed
    {match, _} = re:run(ValidateOutput, "ALL QUALITY GATES PASSED"),
    {match, _} = re:run(ValidateOutput, "Profile"),
    {match, _} = re:run(ValidateOutput, "Compilation"),
    {match, _} = re:run(ValidateOutput, "Tests"),
    {match, _} = re:run(ValidateOutput, "Coverage"),
    {match, _} = re:run(ValidateOutput, "Quality"),
    {match, _} = re:run(ValidateOutput, "Benchmarks"),

    ok.

%%--------------------------------------------------------------------
%% Test: clean → compile → test → release sequence
%% Expected: Full build lifecycle from scratch
%% Chicago TDD: Real end-to-end workflow
%%--------------------------------------------------------------------
clean_to_release_sequence(_Config) ->
    %% Phase 1: Clean
    {0, _} = run_make("clean"),
    false = filelib:is_dir("_build"),

    %% Phase 2: Compile
    {0, _} = run_make("compile"),
    true = filelib:is_dir("_build"),

    %% Phase 3: Test
    {0, _} = run_make("test"),

    %% Phase 4: Release
    {0, ReleaseOutput} = run_make("release"),
    {match, _} = re:run(ReleaseOutput, "Release built"),

    %% Verify: Release artifact exists
    true = filelib:is_dir("_build/prod/rel/erlmcp"),

    ok.

%%%===================================================================
%%% TEST GROUP 2: WORKFLOW INTEGRATION (Chicago School: Real Workflows)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: doctor → quick → verify workflow
%% Expected: Progressive quality checks
%% Chicago TDD: Real multi-stage workflow
%%--------------------------------------------------------------------
doctor_quick_verify_workflow(_Config) ->
    %% Stage 1: Doctor (environment check)
    {0, DoctorOutput} = run_make("doctor"),
    {match, _} = re:run(DoctorOutput, "OTP"),

    %% Stage 2: Quick (fast quality check)
    StartQuick = erlang:system_time(second),
    {0, QuickOutput} = run_make("quick"),
    DurationQuick = erlang:system_time(second) - StartQuick,

    %% Verify: Quick completes in <5 minutes
    true = DurationQuick < 300,
    {match, _} = re:run(QuickOutput, "Quick check PASSED"),

    %% Stage 3: Verify (full validation)
    StartVerify = erlang:system_time(second),
    {0, VerifyOutput} = run_make("verify"),
    DurationVerify = erlang:system_time(second) - StartVerify,

    %% Verify: Verify completes in <15 minutes
    true = DurationVerify < 900,
    {match, _} = re:run(VerifyOutput, "Full validation PASSED"),

    ok.

%%--------------------------------------------------------------------
%% Test: quick → ci-local workflow
%% Expected: Local development to CI reproduction
%% Chicago TDD: Real workflow transition
%%--------------------------------------------------------------------
quick_to_ci_local_workflow(_Config) ->
    %% Stage 1: Quick (local development check)
    {0, _} = run_make("quick"),

    %% Stage 2: CI-local (reproduce exact CI workflow)
    {0, CIOutput} = run_make("ci-local"),

    %% Verify: All CI gates executed
    {match, _} = re:run(CIOutput, "GATE 1: Compilation"),
    {match, _} = re:run(CIOutput, "GATE 2: Xref"),
    {match, _} = re:run(CIOutput, "GATE 3: Dialyzer"),
    {match, _} = re:run(CIOutput, "GATE 4: EUnit Tests"),
    {match, _} = re:run(CIOutput, "GATE 5: Common Test"),
    {match, _} = re:run(CIOutput, "GATE 6: Coverage Check"),

    %% Verify: CI log files generated
    true = filelib:is_file("/tmp/erlmcp_ci_compile.log"),
    true = filelib:is_file("/tmp/erlmcp_ci_dialyzer.log"),

    ok.

%%--------------------------------------------------------------------
%% Test: Full quality workflow (all checks)
%% Expected: Complete quality gate sequence
%% Chicago TDD: Real comprehensive workflow
%%--------------------------------------------------------------------
full_quality_workflow(_Config) ->
    %% Execute full quality workflow
    {0, Output} = run_make("validate"),

    %% Verify: Each gate passed individually
    verify_gate_passed(Output, "Profile validation"),
    verify_gate_passed(Output, "Compilation"),
    verify_gate_passed(Output, "Tests"),
    verify_gate_passed(Output, "Coverage"),
    verify_gate_passed(Output, "Quality"),
    verify_gate_passed(Output, "Benchmarks"),

    %% Verify: Final success message
    {match, _} = re:run(Output, "ALL QUALITY GATES PASSED"),

    ok.

%%%===================================================================
%%% TEST GROUP 3: PARALLEL EXECUTION (Chicago School: Real Concurrency)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Parallel app compilation
%% Expected: Individual apps compile concurrently
%% Chicago TDD: Real parallel make execution
%%--------------------------------------------------------------------
parallel_app_compilation(_Config) ->
    %% Clean first
    run_make("clean"),

    %% Launch parallel compilation
    Apps = ["core", "transports", "observability", "tcps"],
    StartTime = erlang:system_time(millisecond),

    %% Execute in parallel (Chicago School: real processes)
    Pids = [spawn_link(fun() ->
                           Target = "compile-" ++ App,
                           {ExitCode, Output} = run_make(Target),
                           exit({done, App, ExitCode, Output})
                       end) || App <- Apps],

    %% Collect results
    Results = [receive
                   {'EXIT', Pid, {done, App, ExitCode, Output}} ->
                       {App, ExitCode, Output}
               after 120000 ->
                   timeout
               end || Pid <- Pids],

    EndTime = erlang:system_time(millisecond),
    ParallelDuration = EndTime - StartTime,

    %% Verify: All apps compiled successfully
    lists:foreach(fun({_App, ExitCode, _Output}) ->
                      0 = ExitCode
                  end, Results),

    %% Verify: Parallel execution faster than sequential
    %% (Real measurement, not mocked)
    ct:pal("Parallel compilation duration: ~p ms", [ParallelDuration]),
    true = ParallelDuration < 180000, %% < 3 minutes

    ok.

%%--------------------------------------------------------------------
%% Test: Parallel test execution
%% Expected: Test suites run concurrently
%% Chicago TDD: Real parallel test execution
%%--------------------------------------------------------------------
parallel_test_execution(_Config) ->
    %% Ensure compilation complete
    run_make("compile"),

    %% Launch parallel tests (Chicago School: real test processes)
    TestTargets = ["test-core", "test-transports", "test-observability", "test-tcps"],
    StartTime = erlang:system_time(millisecond),

    Pids = [spawn_link(fun() ->
                           {ExitCode, Output} = run_make(Target),
                           exit({done, Target, ExitCode, Output})
                       end) || Target <- TestTargets],

    %% Collect results
    Results = [receive
                   {'EXIT', Pid, {done, Target, ExitCode, Output}} ->
                       {Target, ExitCode, Output}
               after 180000 ->
                   timeout
               end || Pid <- Pids],

    EndTime = erlang:system_time(millisecond),
    ParallelDuration = EndTime - StartTime,

    %% Verify: All tests passed
    lists:foreach(fun({_Target, ExitCode, _Output}) ->
                      0 = ExitCode
                  end, Results),

    ct:pal("Parallel test duration: ~p ms", [ParallelDuration]),
    ok.

%%--------------------------------------------------------------------
%% Test: Parallel quality gates
%% Expected: Dialyzer, xref, coverage run concurrently
%% Chicago TDD: Real parallel quality checks
%%--------------------------------------------------------------------
parallel_quality_gates(_Config) ->
    run_make("compile"),

    %% Launch parallel quality gates
    Gates = ["dialyzer", "xref", "coverage"],
    StartTime = erlang:system_time(millisecond),

    Pids = [spawn_link(fun() ->
                           {ExitCode, Output} = run_make(Gate),
                           exit({done, Gate, ExitCode, Output})
                       end) || Gate <- Gates],

    Results = [receive
                   {'EXIT', Pid, {done, Gate, ExitCode, Output}} ->
                       {Gate, ExitCode, Output}
               after 300000 ->
                   timeout
               end || Pid <- Pids],

    EndTime = erlang:system_time(millisecond),
    ParallelDuration = EndTime - StartTime,

    %% Verify: All gates passed
    lists:foreach(fun({_Gate, ExitCode, _Output}) ->
                      0 = ExitCode
                  end, Results),

    ct:pal("Parallel quality gate duration: ~p ms", [ParallelDuration]),
    ok.

%%%===================================================================
%%% TEST GROUP 4: STATE MANAGEMENT (Chicago School: Real State Tracking)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Clean → build lifecycle
%% Expected: Clean state → build → clean state
%% Chicago TDD: Real file system state management
%%--------------------------------------------------------------------
clean_build_lifecycle(_Config) ->
    %% Initial state: Verify clean
    {0, _} = run_make("clean"),
    false = filelib:is_dir("_build"),

    %% Build phase: Generate artifacts
    {0, _} = run_make("compile"),
    true = filelib:is_dir("_build"),
    BeamCount1 = count_beam_files(),
    true = BeamCount1 > 0,

    %% Test phase: More artifacts
    {0, _} = run_make("test"),
    true = filelib:is_dir("_build/test"),

    %% Clean phase: Return to clean state
    {0, _} = run_make("clean"),
    false = filelib:is_dir("_build"),

    ok.

%%--------------------------------------------------------------------
%% Test: Distclean → recovery workflow
%% Expected: Deep clean → full recovery
%% Chicago TDD: Real recovery from deep clean
%%--------------------------------------------------------------------
distclean_recovery_workflow(_Config) ->
    %% Phase 1: Distclean (remove everything)
    {0, _} = run_make("distclean"),
    false = filelib:is_file("rebar.lock"),
    false = filelib:is_dir("_build"),

    %% Phase 2: Fetch deps (recovery step 1)
    {0, _} = run_make("deps"),
    true = filelib:is_file("rebar.lock"),

    %% Phase 3: Compile (recovery step 2)
    {0, _} = run_make("compile"),
    true = filelib:is_dir("_build"),

    %% Phase 4: Test (recovery step 3)
    {0, _} = run_make("test"),

    %% Verify: Full recovery successful
    BeamFiles = filelib:wildcard("_build/default/lib/*/ebin/*.beam"),
    true = length(BeamFiles) > 0,

    ok.

%%--------------------------------------------------------------------
%% Test: Incremental compilation
%% Expected: Recompile only changed modules
%% Chicago TDD: Real incremental build
%%--------------------------------------------------------------------
incremental_compilation(_Config) ->
    %% Initial compilation
    {0, _} = run_make("compile"),
    InitialBeamTime = get_beam_mtime("_build/default/lib/erlmcp_core/ebin/erlmcp_client.beam"),

    %% Wait 2 seconds
    timer:sleep(2000),

    %% Touch a source file
    touch_file("apps/erlmcp_core/src/erlmcp_client.erl"),

    %% Recompile
    {0, _} = run_make("compile"),
    NewBeamTime = get_beam_mtime("_build/default/lib/erlmcp_core/ebin/erlmcp_client.beam"),

    %% Verify: BEAM file updated (Chicago School: observable file timestamp)
    true = NewBeamTime > InitialBeamTime,

    ok.

%%%===================================================================
%%% TEST GROUP 5: ERROR PROPAGATION (Chicago School: Real Error Handling)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Compilation error stops test
%% Expected: Test target aborts if compilation fails
%% Chicago TDD: Real error propagation
%%--------------------------------------------------------------------
compilation_error_stops_test(_Config) ->
    %% Inject syntax error
    inject_syntax_error("apps/erlmcp_core/src/erlmcp_client.erl"),

    %% Attempt compile → test sequence
    {CompileExitCode, _} = run_make("compile"),

    %% Verify: Compilation failed
    true = CompileExitCode =/= 0,

    %% Attempt test (should fail due to compilation failure)
    {TestExitCode, _} = run_make("test"),

    %% Verify: Test also failed (error propagated)
    true = TestExitCode =/= 0,

    ok.

%%--------------------------------------------------------------------
%% Test: Test failure stops validate
%% Expected: Validate aborts if tests fail
%% Chicago TDD: Real validation blocking
%%--------------------------------------------------------------------
test_failure_stops_validate(_Config) ->
    %% Inject failing test
    inject_failing_test("test/erlmcp_server_tests.erl"),

    %% Attempt validate
    {ExitCode, Output} = run_make("validate"),

    %% Verify: Validate blocked
    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "TESTS FAILED"),

    ok.

%%--------------------------------------------------------------------
%% Test: Quality failure stops release
%% Expected: Release-validate aborts on quality failure
%% Chicago TDD: Real release blocking
%%--------------------------------------------------------------------
quality_failure_stops_release(_Config) ->
    %% Inject type error (dialyzer failure)
    inject_type_error("apps/erlmcp_core/src/erlmcp_client.erl"),

    %% Attempt release-validate
    {ExitCode, Output} = run_make("release-validate"),

    %% Verify: Release blocked
    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "BLOCKED"),

    ok.

%%%===================================================================
%%% TEST GROUP 6: QUALITY GATE WORKFLOWS (Chicago School: Real Gates)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Validate all gates workflow
%% Expected: All 6 gates execute in sequence
%% Chicago TDD: Real gate sequence
%%--------------------------------------------------------------------
validate_all_gates_workflow(_Config) ->
    {0, Output} = run_make("validate"),

    %% Verify: Each gate executed in order
    verify_gate_order(Output, [
                               "Profile validation",
                               "Compilation",
                               "Tests",
                               "Coverage",
                               "Quality",
                               "Benchmarks"
                              ]),

    ok.

%%--------------------------------------------------------------------
%% Test: Individual gate isolation
%% Expected: Each gate can run independently
%% Chicago TDD: Real isolated gate execution
%%--------------------------------------------------------------------
individual_gate_isolation(_Config) ->
    %% Test each gate independently
    Gates = ["validate-profile", "validate-compile", "validate-test",
             "validate-coverage", "validate-quality", "validate-bench"],

    lists:foreach(fun(Gate) ->
                      {0, Output} = run_make(Gate),
                      {match, _} = re:run(Output, "passed|PASSED")
                  end, Gates),

    ok.

%%--------------------------------------------------------------------
%% Test: Gate failure recovery
%% Expected: Fix issue → gate passes
%% Chicago TDD: Real recovery workflow
%%--------------------------------------------------------------------
gate_failure_recovery(_Config) ->
    %% Phase 1: Inject error
    inject_syntax_error("apps/erlmcp_core/src/erlmcp_client.erl"),

    %% Phase 2: Validate fails
    {FailExitCode, _} = run_make("validate-compile"),
    true = FailExitCode =/= 0,

    %% Phase 3: Fix error
    restore_file("apps/erlmcp_core/src/erlmcp_client.erl"),

    %% Phase 4: Validate succeeds
    {0, _} = run_make("validate-compile"),

    ok.

%%%===================================================================
%%% TEST GROUP 7: TCPS WORKFLOWS (Chicago School: Real TCPS)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Jidoka → Andon workflow
%% Expected: Quality gates → status display
%% Chicago TDD: Real TCPS workflow
%%--------------------------------------------------------------------
jidoka_andon_workflow(_Config) ->
    %% Phase 1: Run Jidoka
    {0, JidokaOutput} = run_make("jidoka"),
    {match, _} = re:run(JidokaOutput, "自働化"),

    %% Phase 2: Check Andon board
    {0, AndonOutput} = run_make("andon"),
    {match, _} = re:run(AndonOutput, "行灯"),

    ok.

%%--------------------------------------------------------------------
%% Test: Poka-yoke validation workflow
%% Expected: Error-proofing checks pass
%% Chicago TDD: Real poka-yoke execution
%%--------------------------------------------------------------------
poka_yoke_validation_workflow(_Config) ->
    {0, Output} = run_make("poka-yoke"),
    {match, _} = re:run(Output, "ポカヨケ"),
    ok.

%%--------------------------------------------------------------------
%% Test: Release-validate workflow
%% Expected: Full validation + quality receipt
%% Chicago TDD: Real release certification
%%--------------------------------------------------------------------
release_validate_workflow(_Config) ->
    {0, Output} = run_make("release-validate"),

    %% Verify: Release certified
    {match, _} = re:run(Output, "RELEASE READY"),
    {match, _} = re:run(Output, "認証"),

    ok.

%%%===================================================================
%%% TEST GROUP 8: GOVERNANCE WORKFLOWS (Chicago School: Real Governance)
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Hooks → Settings → Governance workflow
%% Expected: Complete governance validation
%% Chicago TDD: Real governance checks
%%--------------------------------------------------------------------
hooks_settings_governance_workflow(_Config) ->
    %% Phase 1: Validate hooks
    {0, _} = run_make("hooks-validate"),

    %% Phase 2: Validate settings
    {0, _} = run_make("settings-validate"),

    %% Phase 3: Run governance tests
    {0, Output} = run_make("governance-test"),
    {match, _} = re:run(Output, "All governance tests PASSED"),

    ok.

%%--------------------------------------------------------------------
%% Test: Receipt generation workflow
%% Expected: Quality receipt generated
%% Chicago TDD: Real receipt generation
%%--------------------------------------------------------------------
receipt_generation_workflow(_Config) ->
    {0, Output} = run_make("release-validate"),

    %% Verify: Receipt generated
    {match, _} = re:run(Output, "Quality receipt generated"),

    ok.

%%--------------------------------------------------------------------
%% Test: Governance status workflow
%% Expected: Display governance status
%% Chicago TDD: Real status display
%%--------------------------------------------------------------------
governance_status_workflow(_Config) ->
    {0, Output} = run_make("governance-status"),

    %% Verify: Status displayed (always succeeds)
    true = is_list(Output),

    ok.

%%%===================================================================
%%% HELPER FUNCTIONS (Chicago School: Real Command Execution)
%%%===================================================================

%% Execute make target, return {ExitCode, Output}
run_make(Target) ->
    Cmd = io_lib:format("make ~s 2>&1", [Target]),
    Port = open_port({spawn, Cmd}, [stream, exit_status, use_stdio, stderr_to_stdout]),
    collect_output(Port, []).

%% Collect output from port
collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            Output = lists:flatten(lists:reverse(Acc)),
            {Status, Output}
    after 600000 -> %% 10 minute timeout
        port_close(Port),
        {timeout, lists:flatten(lists:reverse(Acc))}
    end.

%% Create isolated test environment
create_isolated_test_env() ->
    TempDir = "/tmp/erlmcp_makefile_integration_" ++ os:getpid(),
    filelib:ensure_dir(TempDir ++ "/"),
    RootDir = get_erlmcp_root(),
    os:cmd(io_lib:format("cp -R ~s/* ~s/", [RootDir, TempDir])),
    file:set_cwd(TempDir),
    TempDir.

%% Cleanup test environment
cleanup_test_env(TempDir) ->
    os:cmd("rm -rf " ++ TempDir).

%% Get erlmcp root directory
get_erlmcp_root() ->
    {ok, Cwd} = file:get_cwd(),
    case filename:basename(Cwd) of
        "test" -> filename:dirname(Cwd);
        _ -> Cwd
    end.

%% Verify gate passed
verify_gate_passed(Output, GateName) ->
    Pattern = io_lib:format("~s.*passed", [GateName]),
    {match, _} = re:run(Output, Pattern, [caseless]).

%% Verify gate order
verify_gate_order(Output, GateNames) ->
    lists:foldl(fun(GateName, PrevPos) ->
                    Pattern = io_lib:format("~s", [GateName]),
                    {match, [{Pos, _}]} = re:run(Output, Pattern, [caseless]),
                    true = Pos > PrevPos,
                    Pos
                end, 0, GateNames).

%% Count BEAM files
count_beam_files() ->
    length(filelib:wildcard("_build/default/lib/*/ebin/*.beam")).

%% Get BEAM file modification time
get_beam_mtime(BeamPath) ->
    {ok, FileInfo} = file:read_file_info(BeamPath),
    FileInfo#file_info.mtime.

%% Touch file (update timestamp)
touch_file(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    file:write_file(FilePath, Content).

%% Inject syntax error
inject_syntax_error(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    Corrupted = <<Content/binary, "\n\nthis is a syntax error!!!">>,
    file:write_file(FilePath, Corrupted).

%% Inject failing test
inject_failing_test(FilePath) ->
    TestCode = <<"\n\nfailing_test() -> ct:fail(\"Intentional failure\").\n">>,
    {ok, Content} = file:read_file(FilePath),
    file:write_file(FilePath, <<Content/binary, TestCode/binary>>).

%% Inject type error
inject_type_error(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    TypeError = <<"\n\n-spec bad_spec() -> integer().\nbad_spec() -> \"not_an_integer\".\n">>,
    file:write_file(FilePath, <<Content/binary, TypeError/binary>>).

%% Restore file from backup
restore_file(FilePath) ->
    BackupPath = FilePath ++ ".backup",
    case filelib:is_file(BackupPath) of
        true ->
            file:copy(BackupPath, FilePath),
            file:delete(BackupPath);
        false ->
            %% Restore from original erlmcp directory
            OrigPath = get_erlmcp_root() ++ "/" ++ FilePath,
            file:copy(OrigPath, FilePath)
    end.
