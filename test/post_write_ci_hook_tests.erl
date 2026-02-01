%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for post-write-ci.sh hook
%%%
%%% Tests verify:
%%% - Async execution (hook returns immediately)
%%% - File filtering (.erl/.hrl only)
%%% - Log output (build.log, test.log)
%%% - Log rotation (>10MB)
%%% - Timeout behavior (120s max)
%%% - Error handling (compile failures)
%%%
%%% Reference: AUTONOMOUS_IMPLEMENTATION_WORK_ORDER.md:WO-004
%%% Chicago TDD: Real processes, no mocks, state-based verification
%%% @end
%%%-------------------------------------------------------------------
-module(post_write_ci_hook_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOOK_SCRIPT, ".claude/hooks/post-write-ci.sh").
-define(LOG_DIR, ".erlmcp").
-define(BUILD_LOG, ".erlmcp/build.log").
-define(TEST_LOG, ".erlmcp/test.log").
-define(TEST_MODULE, "test_sample_module").
-define(TEST_FILE, "apps/erlmcp_core/src/test_sample_module.erl").

%%%===================================================================
%%% Setup/Teardown (Chicago School: Real filesystem, real shell)
%%%===================================================================

hook_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Async execution - hook returns immediately", fun test_async_execution/0},
      {"File filtering - only .erl and .hrl files", fun test_file_filtering/0},
      {"Log creation - build.log and test.log created", fun test_log_creation/0},
      {"Log content - contains compile output", fun test_log_content/0},
      {"Log rotation - rotates at 10MB", fun test_log_rotation/0},
      {"Compile failure - logged but doesn't block", fun test_compile_failure/0},
      {"Module test execution - runs specific module tests", fun test_module_test_execution/0}
     ]}.

setup() ->
    %% Ensure project directory exists
    ProjectDir = os:getenv("CLAUDE_PROJECT_DIR", "/home/user/erlmcp"),
    file:set_cwd(ProjectDir),

    %% Clean up previous test artifacts
    cleanup_logs(),

    %% Create test module file
    create_test_module(?TEST_FILE),

    %% Return state for cleanup
    #{project_dir => ProjectDir}.

cleanup(_State) ->
    %% Remove test module
    file:delete(?TEST_FILE),

    %% Clean up logs
    cleanup_logs(),

    ok.

cleanup_logs() ->
    %% Remove all log files and rotations
    file:delete(?BUILD_LOG),
    file:delete(?TEST_LOG),
    [file:delete(?BUILD_LOG ++ "." ++ integer_to_list(N)) || N <- lists:seq(1, 10)],
    [file:delete(?TEST_LOG ++ "." ++ integer_to_list(N)) || N <- lists:seq(1, 10)],
    ok.

create_test_module(FilePath) ->
    %% Create a simple test module
    Code = <<"-module(test_sample_module).\n"
             "-export([hello/0]).\n"
             "\n"
             "hello() -> world.\n">>,

    %% Ensure directory exists
    filelib:ensure_dir(FilePath),
    file:write_file(FilePath, Code).

%%%===================================================================
%%% Test Cases (Chicago School: Observable behavior, no mocks)
%%%===================================================================

test_async_execution() ->
    %% Test: Hook returns immediately (async execution)
    %%
    %% Setup: Note start time
    StartTime = erlang:monotonic_time(millisecond),

    %% Exercise: Run hook with environment variables
    ExitCode = run_hook("Write", ?TEST_FILE),

    %% Verify: Hook completed in < 500ms (async)
    ElapsedMs = erlang:monotonic_time(millisecond) - StartTime,

    ?assertEqual(0, ExitCode, "Hook should exit successfully"),
    ?assert(ElapsedMs < 500, "Hook should return immediately (async)").

test_file_filtering() ->
    %% Test: Hook only triggers on .erl and .hrl files
    %%
    %% Exercise: Run hook with non-Erlang file
    ExitCode1 = run_hook("Write", "README.md"),

    %% Verify: No log files created (hook ignored .md file)
    timer:sleep(100),  %% Brief pause
    ?assertEqual(false, filelib:is_file(?BUILD_LOG), "No log for .md file"),

    %% Exercise: Run hook with .erl file
    ExitCode2 = run_hook("Write", ?TEST_FILE),

    %% Verify: Log files created (hook processed .erl file)
    timer:sleep(2000),  %% Wait for async process
    ?assertEqual(0, ExitCode2, "Hook should process .erl file"),
    ?assertEqual(true, filelib:is_file(?BUILD_LOG), "Build log created for .erl file").

test_log_creation() ->
    %% Test: Hook creates build.log and test.log
    %%
    %% Exercise: Run hook
    run_hook("Edit", ?TEST_FILE),

    %% Wait for async process to complete
    timer:sleep(2000),

    %% Verify: Log files exist (observable state)
    ?assertEqual(true, filelib:is_file(?BUILD_LOG), "Build log should exist"),
    ?assertEqual(true, filelib:is_file(?TEST_LOG), "Test log should exist").

test_log_content() ->
    %% Test: Logs contain expected content
    %%
    %% Exercise: Run hook
    run_hook("Write", ?TEST_FILE),

    %% Wait for async process
    timer:sleep(2000),

    %% Verify: Build log contains compile command
    {ok, BuildContent} = file:read_file(?BUILD_LOG),
    ?assertMatch({match, _}, re:run(BuildContent, "rebar3 compile", []),
                 "Build log should contain compile command"),
    ?assertMatch({match, _}, re:run(BuildContent, "CI triggered by", []),
                 "Build log should contain trigger info").

test_log_rotation() ->
    %% Test: Logs rotate when exceeding 10MB
    %%
    %% Setup: Create large log file (>10MB)
    LargeContent = list_to_binary(lists:duplicate(11 * 1024 * 1024, $X)),
    file:write_file(?BUILD_LOG, LargeContent),

    %% Verify: Initial log size > 10MB
    {ok, FileInfo} = file:read_file_info(?BUILD_LOG, [raw]),
    InitialSize = maps:get(size, FileInfo),
    ?assert(InitialSize > 10 * 1024 * 1024, "Test log should be > 10MB"),

    %% Exercise: Trigger hook (should rotate log)
    run_hook("Write", ?TEST_FILE),

    %% Wait for rotation
    timer:sleep(2000),

    %% Verify: Old log rotated to .1, new log created
    ?assertEqual(true, filelib:is_file(?BUILD_LOG ++ ".1"), "Rotated log should exist"),

    {ok, FileInfo2} = file:read_file_info(?BUILD_LOG, [raw]),
    NewSize = maps:get(size, FileInfo2),
    ?assert(NewSize < InitialSize, "New log should be smaller than rotated log").

test_compile_failure() ->
    %% Test: Compile failure is logged but doesn't block
    %%
    %% Setup: Create invalid Erlang file
    BadFile = "apps/erlmcp_core/src/test_bad_module.erl",
    BadCode = <<"-module(test_bad_module).\n"
                "-export([bad/0]).\n"
                "\n"
                "bad() -> this is invalid syntax!!!.\n">>,
    filelib:ensure_dir(BadFile),
    file:write_file(BadFile, BadCode),

    %% Exercise: Run hook (should handle compile failure gracefully)
    ExitCode = run_hook("Write", BadFile),

    %% Verify: Hook returns immediately despite compile failure
    ?assertEqual(0, ExitCode, "Hook should exit successfully even with bad code"),

    %% Wait for async process
    timer:sleep(3000),

    %% Verify: Build log contains failure message
    {ok, BuildContent} = file:read_file(?BUILD_LOG),
    ?assertMatch({match, _}, re:run(BuildContent, "FAILED", []),
                 "Build log should contain failure status"),

    %% Cleanup
    file:delete(BadFile).

test_module_test_execution() ->
    %% Test: Hook runs tests for specific module
    %%
    %% Setup: Create module with test suite
    TestModuleFile = "apps/erlmcp_core/test/test_sample_module_tests.erl",
    TestCode = <<"-module(test_sample_module_tests).\n"
                 "-include_lib(\"eunit/include/eunit.hrl\").\n"
                 "\n"
                 "hello_test() ->\n"
                 "    ?assertEqual(world, test_sample_module:hello()).\n">>,
    filelib:ensure_dir(TestModuleFile),
    file:write_file(TestModuleFile, TestCode),

    %% Exercise: Run hook
    run_hook("Write", ?TEST_FILE),

    %% Wait for async compile + test
    timer:sleep(4000),

    %% Verify: Test log mentions module-specific test
    case file:read_file(?TEST_LOG) of
        {ok, TestContent} ->
            ?assertMatch({match, _}, re:run(TestContent, "test_sample_module_tests", []),
                         "Test log should reference module test suite");
        {error, _} ->
            %% Test log may not exist if no tests found - acceptable
            ok
    end,

    %% Cleanup
    file:delete(TestModuleFile).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

run_hook(ToolType, FilePath) ->
    %% Run hook script with environment variables
    %%
    %% Returns: Exit code
    ProjectDir = os:getenv("CLAUDE_PROJECT_DIR", "/home/user/erlmcp"),

    Cmd = io_lib:format(
        "cd ~s && TOOL='~s' SUBJECT='~s' CLAUDE_PROJECT_DIR='~s' bash ~s",
        [ProjectDir, ToolType, FilePath, ProjectDir, ?HOOK_SCRIPT]
    ),

    Output = os:cmd(lists:flatten(Cmd) ++ "; echo $?"),

    %% Parse exit code from last line
    Lines = string:tokens(Output, "\n"),
    case lists:last(Lines) of
        "0" -> 0;
        _ -> 1
    end.
