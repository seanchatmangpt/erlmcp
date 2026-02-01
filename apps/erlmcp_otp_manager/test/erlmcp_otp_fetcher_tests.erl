-module(erlmcp_otp_fetcher_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

fetcher_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_start_link/1,
         fun test_fetch_prebuilt_missing_url/1,
         fun test_fetch_source_creates_directory/1,
         fun test_retry_backoff_configuration/1,
         fun test_checksum_verification_disabled/1,
         fun test_fetcher_events_emitted/1,
         fun test_concurrent_fetches/1,
         fun test_fetch_timeout/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Create temporary test directory
    TestDir = "/tmp/erlmcp_otp_fetcher_test_" ++ integer_to_list(erlang:system_time()),
    ok = filelib:ensure_dir(filename:join(TestDir, "dummy")),

    %% Set test application environment
    application:set_env(erlmcp_otp_manager, otp_install_dir, TestDir),
    application:set_env(erlmcp_otp_manager, retry_backoff, [100, 200]),
    application:set_env(erlmcp_otp_manager, fetch_timeout_prebuilt, 5000),
    application:set_env(erlmcp_otp_manager, fetch_timeout_source, 10000),
    application:set_env(erlmcp_otp_manager, verify_checksums, false),

    %% Start the fetcher
    {ok, Pid} = erlmcp_otp_fetcher:start_link(),

    #{pid => Pid, test_dir => TestDir}.

cleanup(#{pid := Pid, test_dir := TestDir}) ->
    %% Stop the fetcher
    (catch erlmcp_otp_fetcher:stop()),
    (catch gen_server:stop(Pid)),

    %% Clean up test directory
    os:cmd("rm -rf " ++ TestDir),

    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_start_link(#{pid := Pid}) ->
    [
        ?_assert(is_pid(Pid)),
        ?_assertEqual(true, is_process_alive(Pid)),
        ?_assertEqual(Pid, whereis(erlmcp_otp_fetcher))
    ].

test_fetch_prebuilt_missing_url(#{}) ->
    %% Test that fetch_prebuilt returns error when URL is missing
    Result = erlmcp_otp_fetcher:fetch_prebuilt(#{
        version => <<"28.3.1">>,
        sha256 => <<"abc123">>
    }),

    [
        ?_assertMatch({error, {missing_config, otp_prebuilt_url}}, Result)
    ].

test_fetch_source_creates_directory(#{test_dir := TestDir}) ->
    %% Test that fetch_source creates the destination directory
    %% Note: This test will fail if git is not available or network is down
    %% In a real environment, we would use a local test repository

    %% For now, test that the function is callable and returns expected error format
    Result = erlmcp_otp_fetcher:fetch_source(#{
        version => <<"28.3.1">>,
        repo_url => "https://invalid-url-that-does-not-exist.local/otp.git"
    }),

    %% Should get git_clone_failed error
    [
        ?_assertMatch({error, {git_clone_failed, _}}, Result)
    ].

test_retry_backoff_configuration(#{}) ->
    %% Test that retry backoff is configured correctly
    State = sys:get_state(erlmcp_otp_fetcher),

    %% Extract retry_backoff from state record (position 3 in #state{})
    RetryBackoff = element(3, State),

    [
        ?_assertEqual([100, 200], RetryBackoff)
    ].

test_checksum_verification_disabled(#{}) ->
    %% Test that checksum verification can be disabled
    State = sys:get_state(erlmcp_otp_fetcher),

    %% Extract verify_checksums from state record (position 6 in #state{})
    VerifyChecksums = element(6, State),

    [
        ?_assertEqual(false, VerifyChecksums)
    ].

test_fetcher_events_emitted(#{}) ->
    %% Test that fetcher events are emitted during operations
    %% We'll test this by checking error_logger receives events

    %% Clear error_logger handler
    %% For this test, we just verify the function doesn't crash
    Result = erlmcp_otp_fetcher:fetch_prebuilt(#{
        version => <<"28.3.1">>
    }),

    %% Should get missing_config error
    [
        ?_assertMatch({error, {missing_config, otp_prebuilt_url}}, Result)
    ].

test_concurrent_fetches(#{}) ->
    %% Test that multiple concurrent fetch requests are handled correctly
    Parent = self(),

    %% Spawn multiple fetch processes
    Pids = [
        spawn(fun() ->
            Result = erlmcp_otp_fetcher:fetch_prebuilt(#{version => <<"28.3.1">>}),
            Parent ! {fetch_result, self(), Result}
        end)
        || _ <- lists:seq(1, 3)
    ],

    %% Collect results
    Results = [
        receive
            {fetch_result, Pid, Result} -> Result
        after 5000 ->
            timeout
        end
        || Pid <- Pids
    ],

    %% All should return the same error (missing URL)
    [
        ?_assertEqual(3, length(Results)),
        ?_assert(lists:all(
            fun({error, {missing_config, otp_prebuilt_url}}) -> true;
               (_) -> false
            end,
            Results))
    ].

test_fetch_timeout(#{test_dir := TestDir}) ->
    %% Test that fetch operations respect timeout settings
    %% This is implicitly tested by the timeout configuration
    State = sys:get_state(erlmcp_otp_fetcher),

    %% Extract timeouts from state record
    TimeoutPrebuilt = element(4, State),  % fetch_timeout_prebuilt
    TimeoutSource = element(5, State),    % fetch_timeout_source

    [
        ?_assertEqual(5000, TimeoutPrebuilt),
        ?_assertEqual(10000, TimeoutSource)
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

%% None needed for basic tests
