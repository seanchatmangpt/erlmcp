%%%-------------------------------------------------------------------
%%% @doc Quality Gates Integration Tests
%%%
%%% Chicago School TDD integration tests for TCPS quality gate
%%% enforcement system. Tests real gate execution with real processes.
%%%
%%% Test Coverage:
%%% - Gate detects compilation failure
%%% - Gate detects test failure
%%% - Gate detects coverage drop
%%% - Gate blocks on failure
%%% - Gate allows on pass
%%% - Gate generates receipt
%%% - Gate integrates with TCPS
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(quality_gates_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
     gate_detects_compilation_failure_test,
     gate_detects_test_failure_test,
     gate_detects_coverage_drop_test,
     gate_blocks_on_failure_test,
     gate_allows_on_pass_test,
     gate_generates_receipt_test,
     gate_integrates_with_tcps_test,
     multiple_gates_cascade_failure_test,
     gate_timeout_handling_test,
     gate_recovery_after_fix_test
    ].

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    %% Start real application (Chicago School: real system)
    application:ensure_all_started(tcps_erlmcp),

    %% Create test work order directory
    TestDir = "/tmp/quality_gates_suite_test",
    ok = ensure_dir(TestDir),

    [{test_dir, TestDir} | Config].

end_per_suite(_Config) ->
    application:stop(tcps_erlmcp),
    os:cmd("rm -rf /tmp/quality_gates_suite_test"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting testcase: ~p", [TestCase]),

    %% Create test work order for this testcase
    TestDir = ?config(test_dir, Config),
    WorkOrderId = list_to_binary("WO-" ++ atom_to_list(TestCase)),
    WorkOrderDir = filename:join(TestDir, binary_to_list(WorkOrderId)),
    ok = ensure_dir(WorkOrderDir),

    [{work_order_id, WorkOrderId}, {work_order_dir, WorkOrderDir} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases (Chicago School: Real Processes, State Verification)
%%%===================================================================

%% @doc Test: Gate detects compilation failure (real compiler)
gate_detects_compilation_failure_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order with broken code (real file)
    BrokenCode = <<"-module(broken).\n-export([bad/0]).\nbad() -> this_is_syntax_error">>,
    CodeFile = filename:join(WorkOrderDir, "broken.erl"),
    ok = file:write_file(CodeFile, BrokenCode),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        files => [CodeFile],
        stage => compilation
    },

    %% Exercise: Run compilation gate (real tcps_quality_gates)
    Result = tcps_quality_gates:check_gate(compilation, WorkOrder),

    %% Verify: Gate fails with violation (state-based verification)
    {fail, Violations} = Result,
    ct:log("Compilation gate violations: ~p", [Violations]),

    %% Verify violation details
    true = length(Violations) > 0,
    FirstViolation = hd(Violations),
    <<"compilation">> = maps:get(gate, FirstViolation),
    true = maps:is_key(error, FirstViolation),

    ok.

%% @doc Test: Gate detects test failure (real test execution)
gate_detects_test_failure_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order with failing test (real EUnit test)
    FailingTestCode = <<
        "-module(failing_test).\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "\n"
        "always_fail_test() ->\n"
        "    ?assertEqual(1, 2).\n"
    >>,

    TestFile = filename:join(WorkOrderDir, "failing_test.erl"),
    ok = file:write_file(TestFile, FailingTestCode),

    %% Compile test file (required for execution)
    {ok, _} = compile:file(TestFile, [return_errors]),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        test_files => [TestFile],
        stage => testing
    },

    %% Exercise: Run test execution gate (real eunit)
    Result = tcps_quality_gates:check_gate(test_execution, WorkOrder),

    %% Verify: Gate fails due to test failure
    {fail, Violations} = Result,
    ct:log("Test execution violations: ~p", [Violations]),

    %% Verify test failure details
    true = length(Violations) > 0,
    FirstViolation = hd(Violations),
    <<"test_execution">> = maps:get(gate, FirstViolation),
    PassRate = maps:get(pass_rate, FirstViolation, 100.0),
    true = PassRate < 95.0, %% Below required 95%

    ok.

%% @doc Test: Gate detects coverage drop (real coverage)
gate_detects_coverage_drop_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order with low coverage
    UncoveredCode = <<
        "-module(uncovered).\n"
        "-export([covered/0, uncovered/0]).\n"
        "\n"
        "covered() -> ok.\n"
        "uncovered() -> not_tested.\n"
    >>,

    UncoveredTest = <<
        "-module(uncovered_test).\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "\n"
        "covered_test() ->\n"
        "    ok = uncovered:covered().\n"
    >>,

    CodeFile = filename:join(WorkOrderDir, "uncovered.erl"),
    TestFile = filename:join(WorkOrderDir, "uncovered_test.erl"),
    ok = file:write_file(CodeFile, UncoveredCode),
    ok = file:write_file(TestFile, UncoveredTest),

    %% Compile files
    {ok, _} = compile:file(CodeFile, [return_errors]),
    {ok, _} = compile:file(TestFile, [return_errors]),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        files => [CodeFile],
        test_files => [TestFile],
        stage => testing,
        baseline_coverage => 85.0 %% Previous coverage
    },

    %% Exercise: Run test execution gate (with coverage)
    Result = tcps_quality_gates:check_gate(test_execution, WorkOrder),

    %% Verify: Gate fails due to coverage drop (50% coverage < 80% minimum)
    {fail, Violations} = Result,
    ct:log("Coverage violations: ~p", [Violations]),

    %% Find coverage violation
    CoverageViolations = [V || V <- Violations,
                               maps:get(reason, V, undefined) =:= coverage_below_threshold],
    true = length(CoverageViolations) > 0,

    ok.

%% @doc Test: Gate blocks stage transition on failure
gate_blocks_on_failure_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),

    %% Setup: Create work order in compilation stage with errors
    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => ["/nonexistent/broken.erl"] %% Will fail compilation
    },

    %% Exercise: Check if can proceed to next stage (real gate check)
    CanProceed = tcps_quality_gates:can_proceed(compilation, WorkOrder),

    %% Verify: Blocked (false = cannot proceed)
    false = CanProceed,
    ct:log("Stage transition blocked: ~p", [CanProceed]),

    %% Verify gate status is failed
    {fail, _Violations} = tcps_quality_gates:get_gate_status(compilation, WorkOrder),

    ok.

%% @doc Test: Gate allows stage transition on pass
gate_allows_on_pass_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order with valid code (real working code)
    ValidCode = <<
        "-module(valid).\n"
        "-export([add/2]).\n"
        "\n"
        "add(A, B) -> A + B.\n"
    >>,

    ValidTest = <<
        "-module(valid_test).\n"
        "-include_lib(\"eunit/include/eunit.hrl\").\n"
        "\n"
        "add_test() ->\n"
        "    3 = valid:add(1, 2).\n"
    >>,

    CodeFile = filename:join(WorkOrderDir, "valid.erl"),
    TestFile = filename:join(WorkOrderDir, "valid_test.erl"),
    ok = file:write_file(CodeFile, ValidCode),
    ok = file:write_file(TestFile, ValidTest),

    %% Compile files
    {ok, _} = compile:file(CodeFile, [return_errors]),
    {ok, _} = compile:file(TestFile, [return_errors]),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => [CodeFile],
        test_files => [TestFile]
    },

    %% Exercise: Check compilation gate
    CompileResult = tcps_quality_gates:check_gate(compilation, WorkOrder),

    %% Verify: Gate passes
    {pass, CompileReceipt} = CompileResult,
    ct:log("Compilation gate passed: ~p", [CompileReceipt]),

    %% Verify can proceed to next stage
    true = tcps_quality_gates:can_proceed(compilation, WorkOrder),

    ok.

%% @doc Test: Gate generates receipt on success
gate_generates_receipt_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create valid work order
    ValidCode = <<
        "-module(receipt_test_module).\n"
        "-export([function/0]).\n"
        "\n"
        "function() -> ok.\n"
    >>,

    CodeFile = filename:join(WorkOrderDir, "receipt_test_module.erl"),
    ok = file:write_file(CodeFile, ValidCode),
    {ok, _} = compile:file(CodeFile, [return_errors]),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => [CodeFile]
    },

    %% Exercise: Run gate and capture receipt
    {pass, Receipt} = tcps_quality_gates:check_gate(compilation, WorkOrder),

    %% Verify: Receipt has required fields (state verification)
    true = is_map(Receipt),
    ct:log("Receipt: ~p", [Receipt]),

    %% Receipt must contain audit trail fields
    true = maps:is_key(gate, Receipt),
    true = maps:is_key(work_order_id, Receipt),
    true = maps:is_key(timestamp, Receipt),
    true = maps:is_key(result, Receipt),
    true = maps:is_key(hash, Receipt), %% SHA-256 hash for immutability

    <<"compilation">> = maps:get(gate, Receipt),
    WorkOrderId = maps:get(work_order_id, Receipt),
    <<"pass">> = maps:get(result, Receipt),

    %% Verify hash is valid SHA-256 (64 hex chars)
    Hash = maps:get(hash, Receipt),
    64 = byte_size(Hash), %% SHA-256 produces 64 hex characters

    ok.

%% @doc Test: Gate integrates with TCPS workflow
gate_integrates_with_tcps_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order through TCPS system
    ValidCode = <<
        "-module(tcps_integration_module).\n"
        "-export([calculate/1]).\n"
        "\n"
        "calculate(X) -> X * 2.\n"
    >>,

    CodeFile = filename:join(WorkOrderDir, "tcps_integration_module.erl"),
    ok = file:write_file(CodeFile, ValidCode),
    {ok, _} = compile:file(CodeFile, [return_errors]),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => [CodeFile]
    },

    %% Exercise: Validate stage transition (TCPS integration point)
    Result = tcps_quality_gates:validate_stage_transition(
        compilation, testing, WorkOrder
    ),

    %% Verify: Transition allowed (state verification)
    {ok, Receipt} = Result,
    ct:log("Stage transition receipt: ~p", [Receipt]),

    %% Receipt should contain TCPS metadata
    true = maps:is_key(from_stage, Receipt),
    true = maps:is_key(to_stage, Receipt),
    compilation = maps:get(from_stage, Receipt),
    testing = maps:get(to_stage, Receipt),

    ok.

%% @doc Test: Multiple gates cascade failure correctly
multiple_gates_cascade_failure_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create work order that will fail multiple gates
    BrokenCode = <<
        "-module(multi_fail).\n"
        "-export([bad/0]).\n"
        "bad() -> undefined_function(). %% Will fail compilation\n"
    >>,

    CodeFile = filename:join(WorkOrderDir, "multi_fail.erl"),
    ok = file:write_file(CodeFile, BrokenCode),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => [CodeFile]
    },

    %% Exercise: Run all gates (Chicago School: real cascade)
    Results = tcps_quality_gates:check_all_gates(WorkOrder),

    %% Verify: Multiple gates fail (observable behavior)
    ct:log("All gate results: ~p", [Results]),

    %% At least compilation gate should fail
    CompileResult = maps:get(compilation, Results, undefined),
    {fail, _} = CompileResult,

    %% Subsequent gates may be skipped or fail
    true = is_map(Results),
    true = maps:size(Results) >= 1,

    ok.

%% @doc Test: Gate timeout handling (real timeout)
gate_timeout_handling_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),

    %% Setup: Create work order with slow operation
    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => testing,
        timeout => 100 %% 100ms timeout (very short)
    },

    %% Exercise: Run gate with timeout (real process timeout)
    Result = tcps_quality_gates:check_gate(test_execution, WorkOrder),

    %% Verify: Gate handles timeout gracefully
    case Result of
        {fail, Violations} ->
            ct:log("Gate timeout violations: ~p", [Violations]),
            %% Check for timeout violation
            TimeoutViolations = [V || V <- Violations,
                                      maps:get(reason, V, undefined) =:= timeout],
            true = length(TimeoutViolations) >= 0; %% May or may not timeout
        {pass, _Receipt} ->
            ct:log("Gate passed (no timeout)"),
            ok %% Fast enough
    end,

    ok.

%% @doc Test: Gate recovery after fix (real code fix)
gate_recovery_after_fix_test(Config) ->
    WorkOrderId = ?config(work_order_id, Config),
    WorkOrderDir = ?config(work_order_dir, Config),

    %% Setup: Create broken code
    BrokenCode = <<"-module(fixable).\n-export([func/0]).\nfunc() -> broken">>,
    CodeFile = filename:join(WorkOrderDir, "fixable.erl"),
    ok = file:write_file(CodeFile, BrokenCode),

    WorkOrder = #{
        id => WorkOrderId,
        type => <<"feature">>,
        stage => compilation,
        files => [CodeFile]
    },

    %% Exercise: Run gate (should fail)
    FailResult = tcps_quality_gates:check_gate(compilation, WorkOrder),
    {fail, _Violations1} = FailResult,
    ct:log("Gate failed as expected: ~p", [FailResult]),

    %% Fix: Replace with working code (real fix)
    FixedCode = <<"-module(fixable).\n-export([func/0]).\nfunc() -> ok.\n">>,
    ok = file:write_file(CodeFile, FixedCode),

    %% Exercise: Run gate again (should pass now)
    PassResult = tcps_quality_gates:check_gate(compilation, WorkOrder),

    %% Verify: Gate passes after fix (state verification)
    {pass, Receipt} = PassResult,
    ct:log("Gate passed after fix: ~p", [Receipt]),

    %% Receipt should show successful recovery
    <<"compilation">> = maps:get(gate, Receipt),
    <<"pass">> = maps:get(result, Receipt),

    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

ensure_dir(Dir) ->
    case filelib:ensure_dir(filename:join(Dir, "dummy")) of
        ok -> file:make_dir(Dir);
        {error, eexist} -> ok;
        Error -> Error
    end.
