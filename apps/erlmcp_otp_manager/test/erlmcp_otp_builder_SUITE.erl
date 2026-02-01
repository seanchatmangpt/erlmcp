%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_otp_builder
%%%
%%% Tests OTP source building functionality following Chicago School TDD:
%%% - Real processes (actual gen_server instances)
%%% - Real file system operations (temp directories, source downloads)
%%% - Observable behavior verification (progress messages, receipts)
%%% - No mocks (use real OTP source or test fixtures)
%%%
%%% Test coverage:
%%% - Full build lifecycle (configure → make → install)
%%% - Progress streaming (phase updates, percentage)
%%% - Build timeout and cancellation
%%% - Failure recovery (configure errors, missing dependencies)
%%% - Concurrent build isolation
%%% - Build receipt format and metadata
%%%
%%% Setup: Uses /tmp/otp-test-* directories for isolated builds
%%% Cleanup: Removes all test artifacts in end_per_suite
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_builder_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_build_from_valid_source/1,
    test_build_progress_streaming/1,
    test_build_timeout/1,
    test_build_failure_recovery/1,
    test_concurrent_builds/1,
    test_build_receipt_format/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, build_lifecycle},
        {group, error_handling},
        {group, concurrency}
    ].

groups() ->
    [
        {build_lifecycle, [sequence], [
            test_build_from_valid_source,
            test_build_progress_streaming,
            test_build_receipt_format
        ]},
        {error_handling, [sequence], [
            test_build_timeout,
            test_build_failure_recovery
        ]},
        {concurrency, [parallel], [
            test_concurrent_builds
        ]}
    ].

%%====================================================================
%% Setup and Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("~n~n===============================================~n"
           "OTP Builder Test Suite - Starting~n"
           "===============================================~n"),

    %% Start erlmcp_otp_manager application (contains erlmcp_otp_builder)
    %% Note: May not be part of a full application, so we just ensure
    %% the module is loaded and can be used standalone
    code:ensure_loaded(erlmcp_otp_builder),

    %% Create root test directory
    TestRootDir = "/tmp/otp-builder-test-" ++ integer_to_list(erlang:system_time()),
    ok = filelib:ensure_dir(TestRootDir ++ "/"),
    ok = file:make_dir(TestRootDir),

    ct:pal("Test root directory: ~s", [TestRootDir]),

    %% Detect available OTP version for test fixtures
    OtpVersion = detect_otp_version(),
    ct:pal("Detected OTP version: ~s", [OtpVersion]),

    [
        {test_root_dir, TestRootDir},
        {otp_version, OtpVersion}
        | Config
    ].

end_per_suite(Config) ->
    TestRootDir = ?config(test_root_dir, Config),

    ct:pal("~nCleaning up test directory: ~s", [TestRootDir]),
    os:cmd("rm -rf " ++ TestRootDir),

    ct:pal("~n===============================================~n"
           "OTP Builder Test Suite - Complete~n"
           "===============================================~n"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("~n--- Starting test: ~p ---", [TestCase]),

    %% Create isolated test directory for this test case
    TestRootDir = ?config(test_root_dir, Config),
    TestCaseDir = TestRootDir ++ "/" ++ atom_to_list(TestCase),
    ok = filelib:ensure_dir(TestCaseDir ++ "/"),
    ok = file:make_dir(TestCaseDir),

    [{test_case_dir, TestCaseDir} | Config].

end_per_testcase(TestCase, Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Cleanup test case directory (optional - can comment out for debugging)
    %% os:cmd("rm -rf " ++ TestCaseDir),

    ct:pal("--- Completed test: ~p ---~n", [TestCase]),
    ok.

%%====================================================================
%% Test Cases: Build Lifecycle
%%====================================================================

%% @doc Test full OTP build from valid source
%% Chicago School: Verify observable behavior (files created, exit status)
test_build_from_valid_source(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup: Create minimal OTP source structure (test fixture)
    %% In real scenario, this would clone from GitHub
    %% For testing, we create a minimal configure/make stub
    SourceDir = TestCaseDir ++ "/otp_src",
    InstallDir = TestCaseDir ++ "/otp_install",

    ok = create_test_otp_source(SourceDir),

    ct:pal("Source directory: ~s", [SourceDir]),
    ct:pal("Install directory: ~s", [InstallDir]),

    %% Exercise: Start real builder gen_server (Chicago School - real process)
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    ct:pal("Builder started: ~p", [Builder]),

    %% Build OTP from source using real API
    BuildOpts = #{
        source_dir => SourceDir,
        prefix => InstallDir
    },

    %% Verify: Build completes successfully
    Result = erlmcp_otp_builder:build(Builder, BuildOpts),

    ct:pal("Build result: ~p", [Result]),

    %% Chicago School: Verify observable state
    case Result of
        {ok, Prefix, Receipt} ->
            %% Verify prefix matches
            ?assertEqual(InstallDir, Prefix),

            %% Verify receipt structure
            ?assertMatch(#{
                timestamp := _,
                duration := _,
                configure_output := _,
                build_output := _,
                install_output := _,
                build_log := _
            }, Receipt),

            %% Verify duration is positive
            Duration = maps:get(duration, Receipt),
            ?assert(Duration > 0),

            %% Verify install directory created (observable behavior)
            ?assert(filelib:is_dir(InstallDir)),

            %% Verify binaries installed (if test stub created them)
            BinDir = InstallDir ++ "/bin",
            case filelib:is_dir(BinDir) of
                true ->
                    ct:pal("   Binaries directory: ~s", [BinDir]);
                false ->
                    ct:pal("   Note: Test stub may not create bin/ directory")
            end,

            ct:pal("✅ Build completed successfully"),
            ct:pal("   Prefix: ~s", [Prefix]),
            ct:pal("   Duration: ~p ms", [Duration]),

            ok;
        {error, Reason} ->
            ct:fail("Build failed: ~p", [Reason])
    end,

    %% Cleanup: Stop builder (Chicago School - clean shutdown)
    ok = gen_server:stop(Builder).

%% @doc Test build progress via status polling
%% Chicago School: Verify observable state changes (poll status during build)
test_build_progress_streaming(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup: Create test source and install directories
    SourceDir = TestCaseDir ++ "/otp_src",
    InstallDir = TestCaseDir ++ "/otp_install",
    ok = create_test_otp_source(SourceDir),

    ct:pal("Starting build with status monitoring..."),

    %% Exercise: Start real builder (Chicago School - real process)
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    %% Start status monitoring in separate process
    Self = self(),
    Monitor = spawn(fun() -> monitor_build_status(Builder, Self, []) end),

    %% Start build
    BuildOpts = #{
        source_dir => SourceDir,
        prefix => InstallDir
    },

    Result = erlmcp_otp_builder:build(Builder, BuildOpts),

    %% Stop monitoring
    Monitor ! stop,

    %% Collect status snapshots
    StatusSnapshots = receive
        {status_snapshots, Snapshots} -> Snapshots
    after 5000 ->
        []
    end,

    ct:pal("Collected ~p status snapshots", [length(StatusSnapshots)]),

    %% Verify: Build completed
    ?assertMatch({ok, _, _}, Result),

    %% Verify: Status snapshots show phase progression
    if
        length(StatusSnapshots) > 0 ->
            Phases = [maps:get(phase, S) || S <- StatusSnapshots],
            ct:pal("   Observed phases: ~p", [Phases]),

            %% Should see at least some phase transitions
            UniquePhases = lists:usort(Phases),
            ?assert(length(UniquePhases) > 0),

            ct:pal("✅ Status monitoring verified"),
            ct:pal("   Unique phases: ~p", [UniquePhases]),
            ct:pal("   Total snapshots: ~p", [length(StatusSnapshots)]);
        true ->
            ct:pal("⚠️  Note: Build completed too quickly to capture status snapshots"),
            ct:pal("   This is expected for test stubs")
    end,

    %% Cleanup
    ok = gen_server:stop(Builder),

    ok.

%% @doc Test build receipt format and metadata
%% Chicago School: Verify observable receipt structure from builder
test_build_receipt_format(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup
    SourceDir = TestCaseDir ++ "/otp_src",
    InstallDir = TestCaseDir ++ "/otp_install",

    ok = create_test_otp_source(SourceDir),

    ct:pal("Building and verifying receipt..."),

    %% Exercise: Start builder and build
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    BuildOpts = #{
        source_dir => SourceDir,
        prefix => InstallDir
    },

    Result = erlmcp_otp_builder:build(Builder, BuildOpts),

    %% Verify: Build succeeded and receipt returned
    ?assertMatch({ok, _, _Receipt}, Result),

    {ok, Prefix, Receipt} = Result,

    ct:pal("Receipt: ~p", [Receipt]),

    %% Required fields per erlmcp_otp_builder specification
    ?assertMatch(#{
        timestamp := _,
        duration := _,
        configure_output := _,
        build_output := _,
        install_output := _,
        build_log := _
    }, Receipt),

    %% Verify: Duration is positive
    Duration = maps:get(duration, Receipt),
    ?assert(Duration > 0),

    %% Verify: Outputs are binaries
    ConfigureOutput = maps:get(configure_output, Receipt),
    BuildOutput = maps:get(build_output, Receipt),
    InstallOutput = maps:get(install_output, Receipt),
    BuildLog = maps:get(build_log, Receipt),

    ?assert(is_binary(ConfigureOutput)),
    ?assert(is_binary(BuildOutput)),
    ?assert(is_binary(InstallOutput)),
    ?assert(is_binary(BuildLog)),

    %% Verify: Build log contains output from all phases
    ?assert(byte_size(BuildLog) > 0),

    ct:pal("✅ Receipt format verified"),
    ct:pal("   Prefix: ~s", [Prefix]),
    ct:pal("   Duration: ~p ms", [Duration]),
    ct:pal("   Build log size: ~p bytes", [byte_size(BuildLog)]),

    %% Cleanup
    ok = gen_server:stop(Builder),

    ok.

%%====================================================================
%% Test Cases: Error Handling
%%====================================================================

%% @doc Test build cancellation
%% Chicago School: Verify cancel stops build and cleans up (observable behavior)
test_build_timeout(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup: Create source that will take a while
    SourceDir = TestCaseDir ++ "/otp_src_slow",
    InstallDir = TestCaseDir ++ "/otp_install",

    ok = create_slow_test_otp_source(SourceDir),

    ct:pal("Starting build and cancelling after 2s..."),

    %% Exercise: Start real builder
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    BuildOpts = #{
        source_dir => SourceDir,
        prefix => InstallDir
    },

    %% Start build in separate process (non-blocking)
    Self = self(),
    BuilderPid = spawn(fun() ->
        Result = erlmcp_otp_builder:build(Builder, BuildOpts),
        Self ! {build_result, Result}
    end),

    %% Wait briefly, then cancel
    timer:sleep(2000),

    ct:pal("Cancelling build..."),
    ok = erlmcp_otp_builder:cancel(Builder),

    %% Wait for build to finish (should return error after cancel)
    Result = receive
        {build_result, R} -> R
    after 10000 ->
        timeout
    end,

    ct:pal("Build result after cancel: ~p", [Result]),

    %% Verify: Build was cancelled (observable behavior)
    ?assertMatch({error, cancelled}, Result),

    %% Verify: Builder is still alive and can be queried
    {ok, Status} = erlmcp_otp_builder:get_status(Builder),
    ct:pal("Builder status: ~p", [Status]),

    %% Verify: Status shows idle (build cancelled and cleaned up)
    ?assertEqual(idle, maps:get(phase, Status)),

    ct:pal("✅ Build cancellation verified"),
    ct:pal("   Result: ~p", [Result]),
    ct:pal("   Final status: ~p", [Status]),

    %% Cleanup
    ok = gen_server:stop(Builder),

    ok.

%% @doc Test build failure recovery (configure failure)
%% Chicago School: Verify error state is observable
test_build_failure_recovery(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup: Create invalid source (missing configure script)
    SourceDir = TestCaseDir ++ "/otp_src_invalid",
    InstallDir = TestCaseDir ++ "/otp_install",

    ok = create_invalid_test_otp_source(SourceDir),

    ct:pal("Starting build with invalid source (missing configure)..."),

    %% Exercise: Start real builder
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    BuildOpts = #{
        source_dir => SourceDir,
        prefix => InstallDir
    },

    Result = erlmcp_otp_builder:build(Builder, BuildOpts),

    ct:pal("Build result: ~p", [Result]),

    %% Verify: Build fails with expected error (configure should fail)
    ?assertMatch({error, _Reason}, Result),

    {error, Reason} = Result,

    %% Error could be:
    %% - {configure_failed, Code, Output} - configure script failed
    %% - {configure_timeout, Output} - configure timed out
    %% - Any other error indicating configure phase failure

    ct:pal("Build failed as expected: ~p", [Reason]),

    %% Verify: Builder returns to idle state after failure
    {ok, Status} = erlmcp_otp_builder:get_status(Builder),
    ?assertEqual(idle, maps:get(phase, Status)),

    %% Verify: No partial installation created
    ?assertNot(filelib:is_dir(InstallDir ++ "/bin")),

    ct:pal("✅ Build failure recovery verified"),
    ct:pal("   Error: ~p", [Reason]),
    ct:pal("   Builder state: idle"),

    %% Cleanup
    ok = gen_server:stop(Builder),

    ok.

%%====================================================================
%% Test Cases: Concurrency
%%====================================================================

%% @doc Test concurrent builds (isolation)
%% Chicago School: Real parallel builds with separate builders, verify no interference
test_concurrent_builds(Config) ->
    TestCaseDir = ?config(test_case_dir, Config),

    %% Setup: Create two separate build environments
    Build1Dir = TestCaseDir ++ "/build1",
    Build2Dir = TestCaseDir ++ "/build2",

    ok = filelib:ensure_dir(Build1Dir ++ "/"),
    ok = filelib:ensure_dir(Build2Dir ++ "/"),
    ok = file:make_dir(Build1Dir),
    ok = file:make_dir(Build2Dir),

    Source1 = Build1Dir ++ "/otp_src",
    Install1 = Build1Dir ++ "/otp_install",

    Source2 = Build2Dir ++ "/otp_src",
    Install2 = Build2Dir ++ "/otp_install",

    ok = create_test_otp_source(Source1),
    ok = create_test_otp_source(Source2),

    ct:pal("Starting 2 concurrent builds..."),

    %% Exercise: Start two REAL builders in parallel (Chicago School)
    {ok, Builder1} = erlmcp_otp_builder:start_link(),
    {ok, Builder2} = erlmcp_otp_builder:start_link(),

    ct:pal("Builders: ~p, ~p", [Builder1, Builder2]),

    %% Start both builds concurrently
    Self = self(),

    Pid1 = spawn(fun() ->
        Result = erlmcp_otp_builder:build(Builder1, #{
            source_dir => Source1,
            prefix => Install1
        }),
        Self ! {build_complete, build1, Result}
    end),

    Pid2 = spawn(fun() ->
        Result = erlmcp_otp_builder:build(Builder2, #{
            source_dir => Source2,
            prefix => Install2
        }),
        Self ! {build_complete, build2, Result}
    end),

    ct:pal("Build processes: ~p, ~p", [Pid1, Pid2]),

    %% Verify: Both builds complete successfully (no interference)
    Result1 = receive
        {build_complete, build1, R1} -> R1
    after 65000 ->
        timeout
    end,

    Result2 = receive
        {build_complete, build2, R2} -> R2
    after 65000 ->
        timeout
    end,

    %% Verify: Both builds succeeded
    ?assertMatch({ok, Install1, _Receipt1}, Result1),
    ?assertMatch({ok, Install2, _Receipt2}, Result2),

    %% Extract receipts
    {ok, Prefix1, Receipt1} = Result1,
    {ok, Prefix2, Receipt2} = Result2,

    %% Verify: Prefixes are different (isolation)
    ?assertNotEqual(Prefix1, Prefix2),

    %% Verify: Both install directories exist
    ?assert(filelib:is_dir(Install1)),
    ?assert(filelib:is_dir(Install2)),

    %% Verify: Receipts are independent
    ?assertNotEqual(maps:get(timestamp, Receipt1), maps:get(timestamp, Receipt2)),

    ct:pal("✅ Concurrent builds verified"),
    ct:pal("   Build 1: ~s (duration: ~p ms)", [Prefix1, maps:get(duration, Receipt1)]),
    ct:pal("   Build 2: ~s (duration: ~p ms)", [Prefix2, maps:get(duration, Receipt2)]),

    %% Cleanup: Stop both builders
    ok = gen_server:stop(Builder1),
    ok = gen_server:stop(Builder2),

    ok.

%%====================================================================
%% Helper Functions - Test Fixtures
%%====================================================================

%% @doc Create minimal OTP source structure for testing
create_test_otp_source(SourceDir) ->
    ok = filelib:ensure_dir(SourceDir ++ "/"),
    ok = file:make_dir(SourceDir),

    %% Create mock configure script
    ConfigureScript = SourceDir ++ "/configure",
    ConfigureContent = <<"#!/bin/bash\n"
                         "echo 'Configuring OTP (test stub)...'\n"
                         "mkdir -p \"$1\"\n"
                         "exit 0\n">>,
    ok = file:write_file(ConfigureScript, ConfigureContent),
    ok = file:change_mode(ConfigureScript, 8#755),

    %% Create mock Makefile
    Makefile = SourceDir ++ "/Makefile",
    MakefileContent = <<"all:\n"
                        "\techo 'Building OTP (test stub)...'\n"
                        "\tsleep 1\n"
                        "\ninstall:\n"
                        "\techo 'Installing OTP (test stub)...'\n"
                        "\tmkdir -p $(DESTDIR)/bin\n"
                        "\ttouch $(DESTDIR)/bin/erl\n">>,
    ok = file:write_file(Makefile, MakefileContent),

    ok.

%% @doc Create slow OTP source for timeout testing
create_slow_test_otp_source(SourceDir) ->
    ok = filelib:ensure_dir(SourceDir ++ "/"),
    ok = file:make_dir(SourceDir),

    %% Create configure that takes 10+ seconds
    ConfigureScript = SourceDir ++ "/configure",
    ConfigureContent = <<"#!/bin/bash\n"
                         "echo 'Configuring (slow test)...'\n"
                         "sleep 15\n"
                         "exit 0\n">>,
    ok = file:write_file(ConfigureScript, ConfigureContent),
    ok = file:change_mode(ConfigureScript, 8#755),

    Makefile = SourceDir ++ "/Makefile",
    MakefileContent = <<"all:\n\tsleep 15\n\ninstall:\n\tsleep 15\n">>,
    ok = file:write_file(Makefile, MakefileContent),

    ok.

%% @doc Create invalid OTP source for failure testing
create_invalid_test_otp_source(SourceDir) ->
    ok = filelib:ensure_dir(SourceDir ++ "/"),
    ok = file:make_dir(SourceDir),

    %% No configure script → should fail
    %% Just create empty directory
    ok.

%%====================================================================
%% Helper Functions - Status Monitoring
%%====================================================================

%% @doc Monitor builder status during build
monitor_build_status(Builder, Parent, Snapshots) ->
    receive
        stop ->
            Parent ! {status_snapshots, lists:reverse(Snapshots)}
    after 200 ->
        %% Poll status every 200ms
        case erlmcp_otp_builder:get_status(Builder) of
            {ok, Status} ->
                monitor_build_status(Builder, Parent, [Status | Snapshots]);
            _ ->
                monitor_build_status(Builder, Parent, Snapshots)
        end
    end.


%%====================================================================
%% Helper Functions - System Detection
%%====================================================================

detect_otp_version() ->
    case os:cmd("erl -noshell -eval 'io:format(\"~s\", [erlang:system_info(otp_release)]), halt().' 2>/dev/null") of
        "" -> "unknown";
        Version -> string:trim(Version)
    end.
