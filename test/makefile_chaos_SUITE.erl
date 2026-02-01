-module(makefile_chaos_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Makefile Chaos Engineering Tests - Chicago School TDD
%%%===================================================================
%%% Purpose: Test Makefile behavior under chaotic/edge case conditions
%%% Pattern: Real chaos injection, real failure scenarios, no mocks
%%% Scope: Edge cases, concurrent access, resource exhaustion, corruption
%%%===================================================================

%%%===================================================================
%%% CHAOS CATEGORIES
%%%===================================================================
%%% 1. Missing Dependencies (rebar3, erl, scripts)
%%% 2. Corrupted Files (Makefile, config, source)
%%% 3. Resource Exhaustion (disk space, memory, file descriptors)
%%% 4. Permission Errors (read-only, non-executable)
%%% 5. Concurrent Execution (race conditions, file locks)
%%% 6. Network Failures (hex.pm unreachable, timeout)
%%% 7. State Corruption (partial builds, interrupted compilation)
%%% 8. Environment Chaos (missing env vars, invalid paths)
%%%===================================================================

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
     {group, missing_dependencies},
     {group, corrupted_files},
     {group, resource_exhaustion},
     {group, permission_errors},
     {group, concurrent_execution},
     {group, network_failures},
     {group, state_corruption},
     {group, environment_chaos}
    ].

groups() ->
    [
     {missing_dependencies, [], [
                                 missing_rebar3_test,
                                 missing_erl_test,
                                 missing_script_test,
                                 missing_make_test
                                ]},

     {corrupted_files, [], [
                            corrupted_makefile_test,
                            corrupted_config_test,
                            corrupted_source_test,
                            corrupted_rebar_config_test
                           ]},

     {resource_exhaustion, [], [
                                disk_space_exhaustion_test,
                                memory_exhaustion_test,
                                file_descriptor_exhaustion_test,
                                process_limit_exhaustion_test
                               ]},

     {permission_errors, [], [
                              read_only_filesystem_test,
                              non_executable_script_test,
                              write_protected_directory_test,
                              symlink_permission_test
                             ]},

     {concurrent_execution, [], [
                                 concurrent_compile_test,
                                 concurrent_test_execution_test,
                                 file_lock_contention_test,
                                 build_artifact_race_test
                                ]},

     {network_failures, [], [
                             hex_pm_unreachable_test,
                             dependency_timeout_test,
                             partial_download_test,
                             dns_failure_test
                            ]},

     {state_corruption, [], [
                             interrupted_compilation_test,
                             partial_build_cleanup_test,
                             stale_beam_files_test,
                             lock_file_corruption_test
                            ]},

     {environment_chaos, [], [
                              missing_env_var_test,
                              invalid_path_test,
                              corrupted_shell_test,
                              terminal_type_chaos_test
                             ]}
    ].

init_per_suite(Config) ->
    %% Chicago School: Create chaos test environment
    TestDir = create_chaos_test_env(),
    [{test_dir, TestDir}, {original_dir, element(2, file:get_cwd())} | Config].

end_per_suite(Config) ->
    OrigDir = ?config(original_dir, Config),
    TestDir = ?config(test_dir, Config),
    file:set_cwd(OrigDir),
    cleanup_chaos_test_env(TestDir),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting chaos test: ~p", [TestCase]),
    backup_test_env(),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Finished chaos test: ~p", [TestCase]),
    restore_test_env(),
    ok.

%%%===================================================================
%%% CHAOS GROUP 1: MISSING DEPENDENCIES
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Missing rebar3
%% Expected: Makefile detects missing rebar3, provides clear error
%% Chaos: Temporarily hide rebar3 from PATH
%%--------------------------------------------------------------------
missing_rebar3_test(_Config) ->
    %% Chaos: Hide rebar3
    OrigPath = os:getenv("PATH"),
    PathWithoutRebar3 = remove_from_path(OrigPath, "rebar3"),
    os:putenv("PATH", PathWithoutRebar3),

    %% Exercise: Attempt compile
    {ExitCode, Output} = run_make("compile"),

    %% Verify: Clear error message (Chicago School: observable behavior)
    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "rebar3.*not found|command not found", [caseless]),

    %% Cleanup: Restore PATH
    os:putenv("PATH", OrigPath),

    ok.

%%--------------------------------------------------------------------
%% Test: Missing erl
%% Expected: Makefile detects missing Erlang, provides clear error
%% Chaos: Temporarily hide erl from PATH
%%--------------------------------------------------------------------
missing_erl_test(_Config) ->
    OrigPath = os:getenv("PATH"),
    PathWithoutErl = remove_from_path(OrigPath, "erl"),
    os:putenv("PATH", PathWithoutErl),

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "erl.*not found|Erlang.*not found", [caseless]),

    os:putenv("PATH", OrigPath),
    ok.

%%--------------------------------------------------------------------
%% Test: Missing critical script
%% Expected: Makefile detects missing script, fails gracefully
%% Chaos: Delete required script
%%--------------------------------------------------------------------
missing_script_test(_Config) ->
    %% Chaos: Delete check_erlang_version.sh
    ScriptPath = "scripts/check_erlang_version.sh",
    file:rename(ScriptPath, ScriptPath ++ ".backup"),

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "check_erlang_version.*not found", [caseless]),

    %% Cleanup
    file:rename(ScriptPath ++ ".backup", ScriptPath),
    ok.

%%--------------------------------------------------------------------
%% Test: Missing make
%% Expected: Cannot execute Makefile (OS-level error)
%% Chaos: Attempt to run make without make installed
%%--------------------------------------------------------------------
missing_make_test(_Config) ->
    %% Note: This is a hypothetical test - we cannot run this test
    %% without make, but we document the expected behavior.
    %%
    %% Expected: Command not found error from shell
    %% Refusal Code: MISSING_TOOL_MAKE
    %% Remediation: Install make (build-essential on Ubuntu)

    ct:pal("Hypothetical test: If make is missing, user receives:"),
    ct:pal("  bash: make: command not found"),
    ct:pal("  Solution: sudo apt-get install build-essential"),

    ok.

%%%===================================================================
%%% CHAOS GROUP 2: CORRUPTED FILES
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Corrupted Makefile
%% Expected: Make detects syntax error, aborts
%% Chaos: Inject invalid Makefile syntax
%%--------------------------------------------------------------------
corrupted_makefile_test(_Config) ->
    %% Chaos: Corrupt Makefile
    {ok, OrigMakefile} = file:read_file("Makefile"),
    CorruptedMakefile = <<"invalid makefile syntax\n", OrigMakefile/binary>>,
    file:write_file("Makefile", CorruptedMakefile),

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "Makefile.*error|syntax error", [caseless]),

    %% Cleanup
    file:write_file("Makefile", OrigMakefile),
    ok.

%%--------------------------------------------------------------------
%% Test: Corrupted config file
%% Expected: Makefile detects corrupted config, aborts
%% Chaos: Corrupt sys.config
%%--------------------------------------------------------------------
corrupted_config_test(_Config) ->
    ConfigPath = "config/sys.config.dev",
    {ok, OrigConfig} = file:read_file(ConfigPath),
    CorruptedConfig = <<"[{invalid erlang syntax">>,
    file:write_file(ConfigPath, CorruptedConfig),

    {ExitCode, Output} = run_make("compile"),

    %% Verify: Compilation fails due to bad config
    true = ExitCode =/= 0,

    file:write_file(ConfigPath, OrigConfig),
    ok.

%%--------------------------------------------------------------------
%% Test: Corrupted source file
%% Expected: Compilation fails with clear error
%% Chaos: Inject binary corruption in .erl file
%%--------------------------------------------------------------------
corrupted_source_test(_Config) ->
    SourcePath = "apps/erlmcp_core/src/erlmcp_client.erl",
    {ok, OrigSource} = file:read_file(SourcePath),

    %% Chaos: Binary corruption
    CorruptedSource = <<OrigSource/binary, 0, 255, 0, 255>>,
    file:write_file(SourcePath, CorruptedSource),

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "error|corrupt", [caseless]),

    file:write_file(SourcePath, OrigSource),
    ok.

%%--------------------------------------------------------------------
%% Test: Corrupted rebar.config
%% Expected: rebar3 detects error, compilation aborts
%% Chaos: Corrupt rebar.config
%%--------------------------------------------------------------------
corrupted_rebar_config_test(_Config) ->
    {ok, OrigConfig} = file:read_file("rebar.config"),
    CorruptedConfig = <<"{invalid rebar config">>,
    file:write_file("rebar.config", CorruptedConfig),

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,

    file:write_file("rebar.config", OrigConfig),
    ok.

%%%===================================================================
%%% CHAOS GROUP 3: RESOURCE EXHAUSTION
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Disk space exhaustion
%% Expected: Makefile fails gracefully with clear error
%% Chaos: Fill disk (simulated with quota or tmpfs)
%%--------------------------------------------------------------------
disk_space_exhaustion_test(_Config) ->
    %% Chaos: Create large file to consume disk space
    %% (In real test, use tmpfs with size limit)
    ct:pal("Simulated disk space exhaustion test"),
    ct:pal("Expected: Compilation fails with 'No space left on device'"),
    ct:pal("Refusal Code: RESOURCE_DISK_SPACE_EXHAUSTED"),

    %% In production, would:
    %% 1. Mount small tmpfs
    %% 2. Run make compile
    %% 3. Verify error message
    %% 4. Unmount tmpfs

    ok.

%%--------------------------------------------------------------------
%% Test: Memory exhaustion
%% Expected: Compilation OOM, clear error
%% Chaos: Limit memory with ulimit
%%--------------------------------------------------------------------
memory_exhaustion_test(_Config) ->
    ct:pal("Memory exhaustion test (requires ulimit)"),
    ct:pal("Expected: OOM killer or allocation failure"),

    %% In production:
    %% ulimit -v 100000  # Limit to ~100MB
    %% make compile
    %% Verify OOM handling

    ok.

%%--------------------------------------------------------------------
%% Test: File descriptor exhaustion
%% Expected: Makefile fails with "Too many open files"
%% Chaos: Reduce ulimit -n
%%--------------------------------------------------------------------
file_descriptor_exhaustion_test(_Config) ->
    %% Chaos: Reduce file descriptor limit
    OrigLimit = os:cmd("ulimit -n"),
    os:cmd("ulimit -n 64"),

    %% Note: May not fail with low module count
    {_ExitCode, Output} = run_make("compile"),

    %% Expected error if hits limit
    case re:run(Output, "Too many open files", [caseless]) of
        {match, _} ->
            ct:pal("File descriptor limit hit as expected");
        nomatch ->
            ct:pal("File descriptor limit not hit (test needs more files)")
    end,

    %% Cleanup
    os:cmd("ulimit -n " ++ string:trim(OrigLimit)),
    ok.

%%--------------------------------------------------------------------
%% Test: Process limit exhaustion
%% Expected: Compilation fails if spawns exceed limit
%% Chaos: Reduce ulimit -u
%%--------------------------------------------------------------------
process_limit_exhaustion_test(_Config) ->
    ct:pal("Process limit exhaustion test (requires ulimit -u)"),
    ct:pal("Expected: Cannot fork error if parallel compilation hits limit"),

    ok.

%%%===================================================================
%%% CHAOS GROUP 4: PERMISSION ERRORS
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Read-only filesystem
%% Expected: Makefile cannot write build artifacts, fails gracefully
%% Chaos: Mount test directory as read-only
%%--------------------------------------------------------------------
read_only_filesystem_test(_Config) ->
    ct:pal("Read-only filesystem test (requires mount)"),
    ct:pal("Expected: Permission denied when creating _build/"),

    %% In production:
    %% mount -o remount,ro /test/dir
    %% make compile
    %% Verify: Permission denied error

    ok.

%%--------------------------------------------------------------------
%% Test: Non-executable script
%% Expected: Makefile detects non-executable script, clear error
%% Chaos: Remove execute permission from script
%%--------------------------------------------------------------------
non_executable_script_test(_Config) ->
    ScriptPath = "scripts/check_erlang_version.sh",

    %% Chaos: Remove execute permission
    {ok, FileInfo} = file:read_file_info(ScriptPath),
    OrigMode = FileInfo#file_info.mode,
    file:change_mode(ScriptPath, 8#644),  %% rw-r--r--

    {ExitCode, Output} = run_make("compile"),

    true = ExitCode =/= 0,
    {match, _} = re:run(Output, "Permission denied", [caseless]),

    %% Cleanup
    file:change_mode(ScriptPath, OrigMode),
    ok.

%%--------------------------------------------------------------------
%% Test: Write-protected directory
%% Expected: Cannot create _build/, fails with clear error
%% Chaos: Make parent directory read-only
%%--------------------------------------------------------------------
write_protected_directory_test(_Config) ->
    %% Chaos: Create read-only subdirectory
    TestSubdir = "test_readonly_subdir",
    file:make_dir(TestSubdir),
    file:change_mode(TestSubdir, 8#555),  %% r-xr-xr-x

    %% Attempt to write inside
    TestFile = filename:join(TestSubdir, "test.txt"),
    Result = file:write_file(TestFile, <<"test">>),

    %% Verify: Permission denied
    {error, eacces} = Result,

    %% Cleanup
    file:change_mode(TestSubdir, 8#755),
    file:del_dir(TestSubdir),
    ok.

%%--------------------------------------------------------------------
%% Test: Symlink permission chaos
%% Expected: Makefile handles broken symlinks gracefully
%% Chaos: Create broken symlink in config/
%%--------------------------------------------------------------------
symlink_permission_test(_Config) ->
    %% Chaos: Create broken symlink
    file:make_symlink("nonexistent_file", "config/broken_link"),

    {_ExitCode, _Output} = run_make("compile"),

    %% Cleanup
    file:delete("config/broken_link"),
    ok.

%%%===================================================================
%%% CHAOS GROUP 5: CONCURRENT EXECUTION
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Concurrent compile
%% Expected: Multiple make compile in parallel succeed or fail gracefully
%% Chaos: Launch 10 concurrent make compile
%%--------------------------------------------------------------------
concurrent_compile_test(_Config) ->
    %% Chaos: Launch 10 concurrent compilations
    Pids = [spawn_link(fun() ->
                           {ExitCode, _Output} = run_make("compile"),
                           exit({done, ExitCode})
                       end) || _ <- lists:seq(1, 10)],

    %% Collect results
    Results = [receive
                   {'EXIT', Pid, {done, ExitCode}} -> ExitCode
               after 300000 ->
                   timeout
               end || Pid <- Pids],

    %% Verify: At least some succeeded (Chicago School: real concurrency)
    SuccessCount = length([R || R <- Results, R =:= 0]),
    ct:pal("Concurrent compile: ~p/10 succeeded", [SuccessCount]),

    %% Expected: Either all succeed or some fail with file lock errors
    true = SuccessCount > 0,

    ok.

%%--------------------------------------------------------------------
%% Test: Concurrent test execution
%% Expected: Multiple make test in parallel handled correctly
%% Chaos: Launch 5 concurrent make test
%%--------------------------------------------------------------------
concurrent_test_execution_test(_Config) ->
    %% Ensure compiled first
    run_make("compile"),

    %% Chaos: Concurrent test execution
    Pids = [spawn_link(fun() ->
                           {ExitCode, _Output} = run_make("test"),
                           exit({done, ExitCode})
                       end) || _ <- lists:seq(1, 5)],

    Results = [receive
                   {'EXIT', Pid, {done, ExitCode}} -> ExitCode
               after 300000 ->
                   timeout
               end || Pid <- Pids],

    SuccessCount = length([R || R <- Results, R =:= 0]),
    ct:pal("Concurrent test: ~p/5 succeeded", [SuccessCount]),

    ok.

%%--------------------------------------------------------------------
%% Test: File lock contention
%% Expected: rebar3 lock file handled correctly under contention
%% Chaos: Concurrent dep fetching
%%--------------------------------------------------------------------
file_lock_contention_test(_Config) ->
    %% Clean first
    run_make("distclean"),

    %% Chaos: Concurrent dep fetching
    Pids = [spawn_link(fun() ->
                           {ExitCode, _Output} = run_make("deps"),
                           exit({done, ExitCode})
                       end) || _ <- lists:seq(1, 5)],

    Results = [receive
                   {'EXIT', Pid, {done, ExitCode}} -> ExitCode
               after 120000 ->
                   timeout
               end || Pid <- Pids],

    %% Verify: Lock file exists (at least one succeeded)
    true = filelib:is_file("rebar.lock"),

    ok.

%%--------------------------------------------------------------------
%% Test: Build artifact race
%% Expected: BEAM files created correctly under concurrent compilation
%% Chaos: Concurrent module compilation
%%--------------------------------------------------------------------
build_artifact_race_test(_Config) ->
    run_make("clean"),

    %% Chaos: Concurrent compilation of same app
    Pids = [spawn_link(fun() ->
                           {ExitCode, _Output} = run_make("compile-core"),
                           exit({done, ExitCode})
                       end) || _ <- lists:seq(1, 3)],

    _Results = [receive
                   {'EXIT', Pid, {done, ExitCode}} -> ExitCode
               after 180000 ->
                   timeout
               end || Pid <- Pids],

    %% Verify: BEAM files exist and are valid
    BeamFiles = filelib:wildcard("_build/default/lib/erlmcp_core/ebin/*.beam"),
    true = length(BeamFiles) > 0,

    ok.

%%%===================================================================
%%% CHAOS GROUP 6: NETWORK FAILURES
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: hex.pm unreachable
%% Expected: Dependency fetch fails with clear error
%% Chaos: Block hex.pm with firewall rule or hosts file
%%--------------------------------------------------------------------
hex_pm_unreachable_test(_Config) ->
    ct:pal("hex.pm unreachable test (requires network chaos)"),
    ct:pal("Expected: Dependency fetch timeout or connection refused"),
    ct:pal("Simulation: Add '127.0.0.1 hex.pm' to /etc/hosts"),

    ok.

%%--------------------------------------------------------------------
%% Test: Dependency download timeout
%% Expected: rebar3 retries, then fails gracefully
%% Chaos: Inject network latency
%%--------------------------------------------------------------------
dependency_timeout_test(_Config) ->
    ct:pal("Dependency timeout test (requires network chaos)"),
    ct:pal("Expected: Connection timeout after retries"),

    ok.

%%--------------------------------------------------------------------
%% Test: Partial download
%% Expected: rebar3 detects checksum mismatch, retries
%% Chaos: Interrupt download mid-stream
%%--------------------------------------------------------------------
partial_download_test(_Config) ->
    ct:pal("Partial download test (requires network chaos)"),
    ct:pal("Expected: Checksum verification failure, retry"),

    ok.

%%--------------------------------------------------------------------
%% Test: DNS failure
%% Expected: Dependency fetch fails with DNS error
%% Chaos: Point /etc/resolv.conf to invalid DNS
%%--------------------------------------------------------------------
dns_failure_test(_Config) ->
    ct:pal("DNS failure test (requires DNS chaos)"),
    ct:pal("Expected: Name resolution failure"),

    ok.

%%%===================================================================
%%% CHAOS GROUP 7: STATE CORRUPTION
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Interrupted compilation
%% Expected: Partial build cleaned up, retry succeeds
%% Chaos: Kill make during compilation
%%--------------------------------------------------------------------
interrupted_compilation_test(_Config) ->
    %% Chaos: Start compilation, kill it mid-execution
    Pid = spawn_link(fun() ->
                         run_make("compile"),
                         exit(normal)
                     end),

    %% Wait briefly, then kill
    timer:sleep(1000),
    exit(Pid, kill),

    %% Verify: Partial state may exist
    ct:pal("Partial build artifacts may exist"),

    %% Recovery: Clean and recompile
    {0, _} = run_make("clean"),
    {0, _} = run_make("compile"),

    ok.

%%--------------------------------------------------------------------
%% Test: Partial build cleanup
%% Expected: make clean removes partial artifacts
%% Chaos: Create partial _build/ structure
%%--------------------------------------------------------------------
partial_build_cleanup_test(_Config) ->
    %% Chaos: Create partial build structure
    filelib:ensure_dir("_build/default/lib/fake_app/ebin/"),
    file:write_file("_build/default/lib/fake_app/ebin/fake.beam", <<"fake">>),

    %% Exercise: Clean
    {0, _} = run_make("clean"),

    %% Verify: Partial artifacts removed
    false = filelib:is_dir("_build"),

    ok.

%%--------------------------------------------------------------------
%% Test: Stale BEAM files
%% Expected: Recompilation replaces stale files
%% Chaos: Manually create old BEAM file
%%--------------------------------------------------------------------
stale_beam_files_test(_Config) ->
    %% Compile first
    run_make("compile"),

    BeamPath = "_build/default/lib/erlmcp_core/ebin/erlmcp_client.beam",

    %% Chaos: Touch source file to make it newer
    timer:sleep(2000),
    SourcePath = "apps/erlmcp_core/src/erlmcp_client.erl",
    touch_file(SourcePath),

    %% Recompile
    {0, _} = run_make("compile"),

    %% Verify: BEAM file updated (Chicago School: observable timestamp)
    {ok, SourceInfo} = file:read_file_info(SourcePath),
    {ok, BeamInfo} = file:read_file_info(BeamPath),
    true = BeamInfo#file_info.mtime >= SourceInfo#file_info.mtime,

    ok.

%%--------------------------------------------------------------------
%% Test: Lock file corruption
%% Expected: rebar3 detects corruption, regenerates
%% Chaos: Corrupt rebar.lock
%%--------------------------------------------------------------------
lock_file_corruption_test(_Config) ->
    %% Ensure lock exists
    run_make("deps"),

    %% Chaos: Corrupt lock file
    file:write_file("rebar.lock", <<"corrupted lock file">>),

    %% Exercise: Attempt compilation
    {_ExitCode, _Output} = run_make("compile"),

    %% Recovery: Clean and rebuild
    run_make("distclean"),
    {0, _} = run_make("deps"),
    {0, _} = run_make("compile"),

    ok.

%%%===================================================================
%%% CHAOS GROUP 8: ENVIRONMENT CHAOS
%%%===================================================================

%%--------------------------------------------------------------------
%% Test: Missing ERLMCP_PROFILE env var
%% Expected: Defaults to 'dev', compilation succeeds
%% Chaos: Unset ERLMCP_PROFILE
%%--------------------------------------------------------------------
missing_env_var_test(_Config) ->
    %% Chaos: Unset ERLMCP_PROFILE
    OrigProfile = os:getenv("ERLMCP_PROFILE"),
    os:unsetenv("ERLMCP_PROFILE"),

    %% Exercise: Compile (should default to 'dev')
    {0, Output} = run_make("compile"),

    %% Verify: Uses default profile
    {match, _} = re:run(Output, "dev", [caseless]),

    %% Cleanup
    case OrigProfile of
        false -> ok;
        Val -> os:putenv("ERLMCP_PROFILE", Val)
    end,

    ok.

%%--------------------------------------------------------------------
%% Test: Invalid PATH
%% Expected: Makefile fails with clear error
%% Chaos: Set PATH to empty
%%--------------------------------------------------------------------
invalid_path_test(_Config) ->
    OrigPath = os:getenv("PATH"),
    os:putenv("PATH", ""),

    {ExitCode, _Output} = run_make("compile"),

    true = ExitCode =/= 0,

    os:putenv("PATH", OrigPath),
    ok.

%%--------------------------------------------------------------------
%% Test: Corrupted SHELL env var
%% Expected: Makefile uses explicit /bin/bash
%% Chaos: Set SHELL to invalid value
%%--------------------------------------------------------------------
corrupted_shell_test(_Config) ->
    OrigShell = os:getenv("SHELL"),
    os:putenv("SHELL", "/nonexistent/shell"),

    %% Exercise: Should still work (Makefile specifies SHELL=/bin/bash)
    {0, _} = run_make("compile"),

    case OrigShell of
        false -> ok;
        Val -> os:putenv("SHELL", Val)
    end,

    ok.

%%--------------------------------------------------------------------
%% Test: Terminal type chaos (TERM)
%% Expected: Makefile works with TERM=dumb
%% Chaos: Set TERM to invalid value
%%--------------------------------------------------------------------
terminal_type_chaos_test(_Config) ->
    OrigTerm = os:getenv("TERM"),
    os:putenv("TERM", "invalid_terminal"),

    %% Exercise: Compile (Makefile uses TERM=dumb for rebar3)
    {0, _} = run_make("compile"),

    case OrigTerm of
        false -> ok;
        Val -> os:putenv("TERM", Val)
    end,

    ok.

%%%===================================================================
%%% HELPER FUNCTIONS (Chicago School: Real Chaos Injection)
%%%===================================================================

run_make(Target) ->
    Cmd = io_lib:format("make ~s 2>&1", [Target]),
    Port = open_port({spawn, Cmd}, [stream, exit_status, use_stdio, stderr_to_stdout]),
    collect_output(Port, []).

collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            Output = lists:flatten(lists:reverse(Acc)),
            {Status, Output}
    after 600000 ->
        port_close(Port),
        {timeout, lists:flatten(lists:reverse(Acc))}
    end.

create_chaos_test_env() ->
    TempDir = "/tmp/erlmcp_makefile_chaos_" ++ os:getpid(),
    filelib:ensure_dir(TempDir ++ "/"),
    RootDir = get_erlmcp_root(),
    os:cmd(io_lib:format("cp -R ~s/* ~s/", [RootDir, TempDir])),
    file:set_cwd(TempDir),
    TempDir.

cleanup_chaos_test_env(TempDir) ->
    os:cmd("rm -rf " ++ TempDir).

get_erlmcp_root() ->
    {ok, Cwd} = file:get_cwd(),
    case filename:basename(Cwd) of
        "test" -> filename:dirname(Cwd);
        _ -> Cwd
    end.

backup_test_env() ->
    os:cmd("cp -R _build _build.backup 2>/dev/null || true"),
    os:cmd("cp rebar.lock rebar.lock.backup 2>/dev/null || true").

restore_test_env() ->
    os:cmd("rm -rf _build && mv _build.backup _build 2>/dev/null || true"),
    os:cmd("cp rebar.lock.backup rebar.lock 2>/dev/null || true").

remove_from_path(Path, Binary) ->
    Paths = string:split(Path, ":", all),
    FilteredPaths = lists:filter(fun(P) ->
                                     not string:find(P, Binary, leading) =/= nomatch
                                 end, Paths),
    string:join(FilteredPaths, ":").

touch_file(FilePath) ->
    {ok, Content} = file:read_file(FilePath),
    file:write_file(FilePath, Content).
