-module(erlmcp_otp_cache_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Comprehensive Test Suite for erlmcp_otp_cache_server
%%%
%%% Purpose: Test OTP version management and caching functionality
%%%
%%% Chicago School TDD:
%%%   - NO mocks (use real gen_server processes)
%%%   - State-based verification (observable behavior via API)
%%%   - Real OTP detection and caching operations
%%%   - Integration testing with file system
%%%
%%% Test Coverage:
%%%   1. Cache initialization and lifecycle
%%%   2. Idempotent ensure_built operations
%%%   3. Version query and comparison
%%%   4. Integrity verification
%%%   5. Version promotion (atomic switching)
%%%   6. Concurrent request handling
%%%   7. Timeout handling
%%%   8. Error recovery scenarios
%%%
%%% Target: ≥80% code coverage (blocking quality gate)
%%% @end
%%%====================================================================

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

otp_cache_server_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_cache_initialization/1,
         fun test_ensure_built_idempotent/1,
         fun test_version_query/1,
         fun test_verify_integrity/1,
         fun test_promote_version/1,
         fun test_concurrent_requests/1,
         fun test_timeout_handling/1,
         fun test_error_recovery/1,
         fun test_cache_directory_structure/1,
         fun test_invalid_version_handling/1,
         fun test_concurrent_promote_requests/1,
         fun test_cache_cleanup/1
     ]}.

%%%====================================================================
%%% Setup and Teardown (Chicago School: real gen_server)
%%%====================================================================

setup() ->
    %% Setup: Create test cache directory
    TestCacheDir = test_cache_dir(),
    ok = filelib:ensure_dir(TestCacheDir ++ "/"),

    %% Start real gen_server (Chicago School: no mocks)
    Config = #{
        cache_dir => TestCacheDir,
        required_version => <<"28.3.1">>,
        verify_timeout => 5000,
        build_timeout => 60000
    },
    {ok, Pid} = erlmcp_otp_cache_server:start_link(Config),

    %% Return server pid for tests
    #{pid => Pid, cache_dir => TestCacheDir}.

cleanup(#{pid := Pid, cache_dir := CacheDir}) ->
    %% Teardown: Stop server (real process cleanup)
    case is_process_alive(Pid) of
        true ->
            ok = erlmcp_otp_cache_server:stop(Pid),
            timer:sleep(100);  %% Allow cleanup to complete
        false ->
            ok
    end,

    %% Clean up test cache directory
    os:cmd("rm -rf " ++ CacheDir),
    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

test_cache_dir() ->
    %% Generate unique test cache directory
    "/tmp/erlmcp_otp_cache_test_" ++ integer_to_list(erlang:unique_integer([positive])).

mock_otp_installation(CacheDir, Version) ->
    %% Create mock OTP directory structure for testing
    %% (Not a mock in the London School sense - this is test data setup)
    VersionDir = filename:join([CacheDir, "otp-" ++ binary_to_list(Version)]),
    BinDir = filename:join([VersionDir, "bin"]),
    ok = filelib:ensure_dir(BinDir ++ "/"),

    %% Create mock erl binary (shell script that returns version)
    ErlScript = filename:join([BinDir, "erl"]),
    ErlContent = io_lib:format(
        "#!/bin/bash\n"
        "if [[ \"$1\" == \"-noshell\" ]]; then\n"
        "    echo \"~s\"\n"
        "fi\n",
        [Version]
    ),
    ok = file:write_file(ErlScript, ErlContent),
    ok = file:change_mode(ErlScript, 8#755),

    VersionDir.

%%%====================================================================
%%% Test Case 1: Cache Initialization
%%%====================================================================

test_cache_initialization(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Exercise: Server started in setup
            %% Verify: Server is alive (Chicago School: real process)
            ?assert(is_process_alive(Pid)),

            %% Verify: Server responds to API calls (state-based verification)
            ?assertMatch({ok, _}, erlmcp_otp_cache_server:status(Pid)),

            %% Verify: Cache directory exists (observable behavior)
            ?assert(filelib:is_dir(CacheDir))
        end),

        ?_test(begin
            %% Verify: Initial state shows no cached versions
            {ok, Status} = erlmcp_otp_cache_server:status(Pid),
            CachedVersions = maps:get(cached_versions, Status, []),
            ?assertEqual([], CachedVersions)
        end),

        ?_test(begin
            %% Verify: Current version returns undefined initially
            ?assertEqual({ok, undefined}, erlmcp_otp_cache_server:current_version(Pid))
        end)
    ].

%%%====================================================================
%%% Test Case 2: Ensure Built Idempotent
%%%====================================================================

test_ensure_built_idempotent(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Setup: Create mock OTP installation
            Version = <<"28.3.1">>,
            _VersionDir = mock_otp_installation(CacheDir, Version),

            %% Exercise: Call ensure_built first time
            {ok, Result1} = erlmcp_otp_cache_server:ensure_built(Pid, Version),
            ?assertMatch(#{version := Version, status := ready}, Result1),

            %% Exercise: Call ensure_built second time (idempotency test)
            {ok, Result2} = erlmcp_otp_cache_server:ensure_built(Pid, Version),
            ?assertMatch(#{version := Version, status := ready}, Result2),

            %% Verify: Results are identical (idempotent behavior)
            ?assertEqual(Result1, Result2),

            %% Verify: No duplicate work done (check via logs or metrics)
            {ok, Status} = erlmcp_otp_cache_server:status(Pid),
            BuildCount = maps:get(build_count, Status, 0),
            ?assert(BuildCount =< 1)  %% Should only build once
        end)
    ].

%%%====================================================================
%%% Test Case 3: Version Query
%%%====================================================================

test_version_query(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Setup: Install mock OTP version
            Version = <<"28.3.1">>,
            _VersionDir = mock_otp_installation(CacheDir, Version),

            %% Exercise: Ensure built and set as current
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version),
            ok = erlmcp_otp_cache_server:set_current(Pid, Version),

            %% Verify: current_version returns correct version (state-based)
            ?assertEqual({ok, Version}, erlmcp_otp_cache_server:current_version(Pid)),

            %% Verify: Status includes version information
            {ok, Status} = erlmcp_otp_cache_server:status(Pid),
            ?assertEqual(Version, maps:get(current_version, Status))
        end),

        ?_test(begin
            %% Test: Query cached versions
            Version1 = <<"28.3.1">>,
            Version2 = <<"28.2.0">>,

            _Dir1 = mock_otp_installation(CacheDir, Version1),
            _Dir2 = mock_otp_installation(CacheDir, Version2),

            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version1),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version2),

            %% Verify: Both versions cached
            {ok, Versions} = erlmcp_otp_cache_server:cached_versions(Pid),
            ?assertEqual(2, length(Versions)),
            ?assert(lists:member(Version1, Versions)),
            ?assert(lists:member(Version2, Versions))
        end)
    ].

%%%====================================================================
%%% Test Case 4: Verify Integrity
%%%====================================================================

test_verify_integrity(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Setup: Install mock OTP
            Version = <<"28.3.1">>,
            _VersionDir = mock_otp_installation(CacheDir, Version),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version),

            %% Exercise: Verify integrity of cached version
            Result = erlmcp_otp_cache_server:verify_integrity(Pid, Version),

            %% Verify: Integrity check passes (observable behavior)
            ?assertEqual({ok, #{version => Version, status => valid}}, Result)
        end),

        ?_test(begin
            %% Test: Integrity check fails for corrupted installation
            Version = <<"28.3.1">>,
            VersionDir = mock_otp_installation(CacheDir, Version),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version),

            %% Corrupt installation by removing erl binary
            ErlBin = filename:join([VersionDir, "bin", "erl"]),
            ok = file:delete(ErlBin),

            %% Exercise: Verify integrity (should fail)
            Result = erlmcp_otp_cache_server:verify_integrity(Pid, Version),

            %% Verify: Integrity check fails
            ?assertMatch({error, {integrity_failure, _}}, Result)
        end),

        ?_test(begin
            %% Test: Verify non-existent version
            NonExistentVersion = <<"99.99.99">>,

            %% Exercise: Verify integrity of non-cached version
            Result = erlmcp_otp_cache_server:verify_integrity(Pid, NonExistentVersion),

            %% Verify: Returns not_found error
            ?assertEqual({error, {not_found, NonExistentVersion}}, Result)
        end)
    ].

%%%====================================================================
%%% Test Case 5: Promote Version (Atomic Switching)
%%%====================================================================

test_promote_version(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Setup: Install two OTP versions
            Version1 = <<"28.3.1">>,
            Version2 = <<"28.2.0">>,

            _Dir1 = mock_otp_installation(CacheDir, Version1),
            _Dir2 = mock_otp_installation(CacheDir, Version2),

            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version1),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version2),

            %% Exercise: Set Version1 as current
            ok = erlmcp_otp_cache_server:set_current(Pid, Version1),
            ?assertEqual({ok, Version1}, erlmcp_otp_cache_server:current_version(Pid)),

            %% Exercise: Promote to Version2 (atomic switch)
            Result = erlmcp_otp_cache_server:promote_version(Pid, Version2),
            ?assertEqual(ok, Result),

            %% Verify: Current version changed atomically (state-based)
            ?assertEqual({ok, Version2}, erlmcp_otp_cache_server:current_version(Pid)),

            %% Verify: Old version still cached (non-destructive)
            {ok, Versions} = erlmcp_otp_cache_server:cached_versions(Pid),
            ?assert(lists:member(Version1, Versions)),
            ?assert(lists:member(Version2, Versions))
        end),

        ?_test(begin
            %% Test: Promote to non-existent version fails
            Version1 = <<"28.3.1">>,
            NonExistent = <<"99.99.99">>,

            _Dir1 = mock_otp_installation(CacheDir, Version1),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version1),
            ok = erlmcp_otp_cache_server:set_current(Pid, Version1),

            %% Exercise: Try to promote to non-existent version
            Result = erlmcp_otp_cache_server:promote_version(Pid, NonExistent),

            %% Verify: Promotion fails
            ?assertMatch({error, {not_found, NonExistent}}, Result),

            %% Verify: Current version unchanged (atomic failure)
            ?assertEqual({ok, Version1}, erlmcp_otp_cache_server:current_version(Pid))
        end)
    ].

%%%====================================================================
%%% Test Case 6: Concurrent Requests (Chicago School: real processes)
%%%====================================================================

test_concurrent_requests(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Setup: Install mock OTP
            Version = <<"28.3.1">>,
            _VersionDir = mock_otp_installation(CacheDir, Version),

            %% Exercise: Spawn 10 concurrent ensure_built requests
            NumProcs = 10,
            ParentPid = self(),

            Pids = [spawn(fun() ->
                Result = erlmcp_otp_cache_server:ensure_built(Pid, Version),
                ParentPid ! {ensure_built_result, self(), Result}
            end) || _ <- lists:seq(1, NumProcs)],

            %% Collect results
            Results = [receive
                {ensure_built_result, P, R} -> R
            after 5000 ->
                timeout
            end || P <- Pids],

            %% Verify: All requests succeeded (no race conditions)
            AllOk = lists:all(fun
                ({ok, #{status := ready}}) -> true;
                (_) -> false
            end, Results),
            ?assert(AllOk),

            %% Verify: Only built once despite concurrent requests (idempotency)
            {ok, Status} = erlmcp_otp_cache_server:status(Pid),
            BuildCount = maps:get(build_count, Status, 0),
            ?assert(BuildCount =< 1)  %% Idempotent under concurrency
        end)
    ].

%%%====================================================================
%%% Test Case 7: Timeout Handling
%%%====================================================================

test_timeout_handling(#{pid := Pid}) ->
    [
        ?_test(begin
            %% Test: Operation completes within timeout
            Version = <<"28.3.1">>,

            %% Exercise: Call with timeout (should succeed or fail cleanly)
            Result = erlmcp_otp_cache_server:ensure_built(Pid, Version, #{timeout => 5000}),

            %% Verify: Either succeeds or returns timeout error (not crash)
            case Result of
                {ok, _} -> ?assert(true);
                {error, timeout} -> ?assert(true);
                {error, _Other} -> ?assert(true)
            end
        end),

        ?_test(begin
            %% Test: Very short timeout triggers timeout error
            Version = <<"99.99.99">>,  %% Non-existent, will take time

            %% Exercise: Call with very short timeout
            Result = erlmcp_otp_cache_server:ensure_built(Pid, Version, #{timeout => 1}),

            %% Verify: Returns timeout or not_found (doesn't crash)
            ?assertMatch({error, _}, Result)
        end)
    ].

%%%====================================================================
%%% Test Case 8: Error Recovery
%%%====================================================================

test_error_recovery(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Test: Recovery from failed build
            Version = <<"28.3.1">>,

            %% Exercise: Try to ensure_built non-existent version
            Result1 = erlmcp_otp_cache_server:ensure_built(Pid, Version),
            ?assertMatch({error, _}, Result1),

            %% Setup: Now create the mock installation
            _VersionDir = mock_otp_installation(CacheDir, Version),

            %% Exercise: Retry ensure_built (recovery test)
            Result2 = erlmcp_otp_cache_server:ensure_built(Pid, Version),
            ?assertMatch({ok, #{status := ready}}, Result2),

            %% Verify: Server recovered from error (Chicago School: observable state)
            {ok, CurrentVersion} = erlmcp_otp_cache_server:current_version(Pid),
            ?assertEqual(Version, CurrentVersion)
        end),

        ?_test(begin
            %% Test: Server survives invalid requests
            InvalidVersion = <<"invalid.version.format">>,

            %% Exercise: Send invalid request
            Result = erlmcp_otp_cache_server:ensure_built(Pid, InvalidVersion),

            %% Verify: Returns error (doesn't crash)
            ?assertMatch({error, _}, Result),

            %% Verify: Server still alive and functional
            ?assert(is_process_alive(Pid)),
            {ok, _Status} = erlmcp_otp_cache_server:status(Pid)
        end)
    ].

%%%====================================================================
%%% Additional Test Cases
%%%====================================================================

test_cache_directory_structure(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Test: Cache directory structure is correct
            Version = <<"28.3.1">>,
            _VersionDir = mock_otp_installation(CacheDir, Version),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version),

            %% Verify: Expected directory structure exists
            VersionDir = filename:join([CacheDir, "otp-28.3.1"]),
            BinDir = filename:join([VersionDir, "bin"]),
            ?assert(filelib:is_dir(VersionDir)),
            ?assert(filelib:is_dir(BinDir)),

            %% Verify: erl binary exists and is executable
            ErlBin = filename:join([BinDir, "erl"]),
            ?assert(filelib:is_file(ErlBin))
        end)
    ].

test_invalid_version_handling(#{pid := Pid}) ->
    [
        ?_test(begin
            %% Test: Invalid version format rejected
            InvalidVersions = [
                <<"">>,
                <<"abc">>,
                <<"1.2.3.4.5">>,
                <<>>
            ],

            lists:foreach(fun(Version) ->
                Result = erlmcp_otp_cache_server:ensure_built(Pid, Version),
                ?assertMatch({error, {invalid_version, _}}, Result)
            end, InvalidVersions)
        end)
    ].

test_concurrent_promote_requests(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Test: Concurrent promote requests handled atomically
            Version1 = <<"28.3.1">>,
            Version2 = <<"28.2.0">>,

            _Dir1 = mock_otp_installation(CacheDir, Version1),
            _Dir2 = mock_otp_installation(CacheDir, Version2),

            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version1),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version2),

            %% Exercise: Spawn concurrent promote requests
            ParentPid = self(),
            _Pids = [spawn(fun() ->
                Result = erlmcp_otp_cache_server:promote_version(
                    Pid,
                    case N rem 2 of 0 -> Version1; 1 -> Version2 end
                ),
                ParentPid ! {promote_result, self(), Result}
            end) || N <- lists:seq(1, 20)],

            %% Collect results
            timer:sleep(100),

            %% Verify: Final state is consistent (atomic promotions)
            {ok, CurrentVersion} = erlmcp_otp_cache_server:current_version(Pid),
            ?assert(CurrentVersion =:= Version1 orelse CurrentVersion =:= Version2)
        end)
    ].

test_cache_cleanup(#{pid := Pid, cache_dir := CacheDir}) ->
    [
        ?_test(begin
            %% Test: Cache cleanup removes old versions
            Version1 = <<"28.3.1">>,
            Version2 = <<"28.2.0">>,

            _Dir1 = mock_otp_installation(CacheDir, Version1),
            _Dir2 = mock_otp_installation(CacheDir, Version2),

            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version1),
            {ok, _} = erlmcp_otp_cache_server:ensure_built(Pid, Version2),

            %% Verify: Both versions cached
            {ok, Versions} = erlmcp_otp_cache_server:cached_versions(Pid),
            ?assertEqual(2, length(Versions)),

            %% Exercise: Clean up old versions
            ok = erlmcp_otp_cache_server:cleanup_old_versions(Pid, #{keep_latest => 1}),

            %% Verify: Only one version remains (observable behavior)
            {ok, VersionsAfter} = erlmcp_otp_cache_server:cached_versions(Pid),
            ?assertEqual(1, length(VersionsAfter))
        end)
    ].
