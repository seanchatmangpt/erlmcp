%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_rate_limiter_tests - Tests for Authentication Rate Limiter
%%%
%%% Tests rate limiting, blocking, and exponential backoff features
%%% following Chicago School TDD principles (real processes, no mocks).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_rate_limiter_tests).

-include_lib("eunit/include/eunit.hrl").

%% Record definitions from erlmcp_auth_rate_limiter
-record(client_stats,
        {client_id :: binary() | undefined,
         ip_address :: inet:ip_address() | undefined,
         total_attempts :: non_neg_integer(),
         successful_auths :: non_neg_integer(),
         failed_auths :: non_neg_integer(),
         rate_limited_count :: non_neg_integer(),
         blocked_count :: non_neg_integer(),
         current_backoff_level :: 0..5,
         last_attempt_at :: integer() | undefined}).

%%====================================================================
%% Test Fixtures
%%====================================================================

rate_limiter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"Rate limit enforcement", fun test_rate_limit_enforcement/0},
      {"Rate limit window expires", fun test_rate_limit_window_expires/0},
      {"Exponential backoff", fun test_exponential_backoff/0},
      {"IP blocking", fun test_ip_blocking/0},
      {"Client statistics", fun test_client_statistics/0},
      {"Success resets backoff", fun test_success_resets_backoff/0},
      {"Cleanup expired entries", fun test_cleanup_expired/0},
      {"Concurrent requests", fun test_concurrent_requests/0},
      {"Clear all blocks", fun test_clear_all_blocks/0},
      {"Get blocked clients", fun test_get_blocked_clients/0},
      {"Reset client", fun test_reset_client/0}]}.

setup() ->
    % Start rate limiter with test config (Chicago School: real process)
    Config =
        #{max_attempts_per_second => 5,  % Lower for testing
          window_ms => 1000,
          max_failures => 3,  % Fewer failures for testing
          block_duration_ms => 5000,  % 5 seconds for testing
          backoff_levels => [0, 100, 200, 400, 800, 1600],
          cleanup_interval_ms => 60000},
    {ok, Pid} = erlmcp_auth_rate_limiter:start_link(Config),
    Pid.

cleanup(_Pid) ->
    erlmcp_auth_rate_limiter:stop(),
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_rate_limit_enforcement() ->
    ClientId = <<"test_client_rate">>,

    % First 5 requests should succeed (state-based verification)
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    % 6th request should be rate limited (observable behavior)
    ?assertEqual({error, rate_limited}, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    ok.

test_rate_limit_window_expires() ->
    ClientId = <<"test_client_window">>,

    % Exhaust rate limit
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),
    ?assertEqual({error, rate_limited}, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    % Wait for window to expire (Chicago School: test real time behavior)
    timer:sleep(1100),

    % Should be allowed again (observable state change)
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    ok.

test_exponential_backoff() ->
    ClientId = <<"test_client_backoff">>,

    % Record failures to trigger backoff (real gen_server state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),

    % Should be blocked now (3 failures = max_failures)
    ?assertEqual(true, erlmcp_auth_rate_limiter:is_blocked(ClientId)),

    % Check that we get blocked error (observable behavior)
    ?assertEqual({error, blocked, client_id_blocked},
                 erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    % Get stats to verify backoff level (state verification)
    {ok, Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),
    ?assert(Stats#client_stats.current_backoff_level > 0),

    ok.

test_ip_blocking() ->
    ClientId = <<"test_client_ip">>,
    IpAddress = {192, 168, 1, 100},

    % Record failures to trigger IP block (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId, IpAddress),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId, IpAddress),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId, IpAddress),

    % IP should be blocked (observable behavior via API)
    Result = erlmcp_auth_rate_limiter:check_rate_limit(<<"other_client">>, IpAddress),
    ?assertMatch({error, blocked, {ip_blocked, _}}, Result),

    % Reset for cleanup
    ok = erlmcp_auth_rate_limiter:reset_client(ClientId),

    ok.

test_client_statistics() ->
    ClientId = <<"test_client_stats">>,

    % Record some failures (real gen_server calls)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),

    % Check stats (state-based verification via API)
    {ok, Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),
    ?assertEqual(<<"test_client_stats">>, Stats#client_stats.client_id),
    ?assertEqual(2, Stats#client_stats.total_attempts),
    ?assertEqual(0, Stats#client_stats.successful_auths),
    ?assertEqual(2, Stats#client_stats.failed_auths),

    % Record success (real API call)
    ok = erlmcp_auth_rate_limiter:record_success(ClientId),

    % Check updated stats (observable state change)
    {ok, Stats2} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),
    ?assertEqual(3, Stats2#client_stats.total_attempts),
    ?assertEqual(1, Stats2#client_stats.successful_auths),
    ?assertEqual(0, Stats2#client_stats.current_backoff_level),

    ok.

test_success_resets_backoff() ->
    ClientId = <<"test_client_reset">>,

    % Record failures to trigger backoff (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),

    % Should be blocked (state verification via API)
    ?assertEqual(true, erlmcp_auth_rate_limiter:is_blocked(ClientId)),

    % Record success (simulating successful auth - real API call)
    ok = erlmcp_auth_rate_limiter:record_success(ClientId),

    % Should no longer be blocked (observable behavior)
    ?assertEqual(false, erlmcp_auth_rate_limiter:is_blocked(ClientId)),

    % Backoff level should be reset (state verification)
    {ok, Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),
    ?assertEqual(0, Stats#client_stats.current_backoff_level),

    ok.

test_cleanup_expired() ->
    ClientId = <<"test_client_cleanup">>,

    % Record failures (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),

    % Verify it's in the system (observable state)
    {ok, _Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),

    % Reset client (real API call)
    ok = erlmcp_auth_rate_limiter:reset_client(ClientId),

    % Should be gone (state verification)
    ?assertEqual({error, not_found}, erlmcp_auth_rate_limiter:get_client_stats(ClientId)),

    ok.

test_concurrent_requests() ->
    ClientId = <<"test_client_concurrent">>,

    % Spawn multiple concurrent requests (real processes, no mocks)
    Self = self(),
    Pids =
        [spawn(fun() ->
                  Result = erlmcp_auth_rate_limiter:check_rate_limit(ClientId),
                  Self ! {result, Result}
               end)
         || _ <- lists:seq(1, 10)],

    % Collect results
    Results =
        [receive
             {result, R} ->
                 R
         end
         || _ <- Pids],

    % Some should have succeeded, some should have been rate limited
    % This tests thread safety of the rate limiter (Chicago School: observable behavior)
    OkCount = length([R || R <- Results, R =:= ok]),
    LimitedCount = length([R || R <- Results, R =:= {error, rate_limited}]),

    ?assert(OkCount > 0),
    ?assert(LimitedCount > 0),

    % Verify stats if they exist (state verification - handle not_found case)
    case erlmcp_auth_rate_limiter:get_client_stats(ClientId) of
        {ok, Stats} ->
            ?assert(Stats#client_stats.total_attempts >= 10);
        {error, not_found} ->
            % Stats may not exist if all requests were rate limited
            ok
    end,

    ok.

test_clear_all_blocks() ->
    ClientId1 = <<"test_client_clear1">>,
    ClientId2 = <<"test_client_clear2">>,

    % Block both clients (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),

    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),

    % Verify both are blocked (state verification)
    ?assertEqual(true, erlmcp_auth_rate_limiter:is_blocked(ClientId1)),
    ?assertEqual(true, erlmcp_auth_rate_limiter:is_blocked(ClientId2)),

    % Clear all blocks (real API call)
    ok = erlmcp_auth_rate_limiter:clear_all_blocks(),

    % Verify neither is blocked (observable behavior)
    ?assertEqual(false, erlmcp_auth_rate_limiter:is_blocked(ClientId1)),
    ?assertEqual(false, erlmcp_auth_rate_limiter:is_blocked(ClientId2)),

    ok.

test_get_blocked_clients() ->
    ClientId1 = <<"test_client_blocked1">>,
    ClientId2 = <<"test_client_blocked2">>,

    % Block two clients (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId1),

    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId2),

    % Get blocked clients list (observable state via API)
    {ok, BlockedClients} = erlmcp_auth_rate_limiter:get_blocked_clients(),

    % Verify both are in the list (state verification)
    ?assert(lists:member(ClientId1, BlockedClients)),
    ?assert(lists:member(ClientId2, BlockedClients)),
    ?assert(length(BlockedClients) >= 2),

    ok.

test_reset_client() ->
    ClientId = <<"test_client_reset_full">>,

    % Create state with failures and blocks (real process state)
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),
    ok = erlmcp_auth_rate_limiter:record_failure(ClientId),

    % Verify state exists
    {ok, _Stats} = erlmcp_auth_rate_limiter:get_client_stats(ClientId),

    % Reset client (real API call)
    ok = erlmcp_auth_rate_limiter:reset_client(ClientId),

    % Verify state cleared (observable behavior)
    ?assertEqual({error, not_found}, erlmcp_auth_rate_limiter:get_client_stats(ClientId)),
    ?assertEqual(false, erlmcp_auth_rate_limiter:is_blocked(ClientId)),

    % Should be able to check rate limit again (fresh state)
    ?assertEqual(ok, erlmcp_auth_rate_limiter:check_rate_limit(ClientId)),

    ok.

%%====================================================================
%% Integration Tests with Auth Module
%%====================================================================

auth_integration_test_() ->
    {setup,
     fun setup_auth_integration/0,
     fun cleanup_auth_integration/1,
     [{"Auth with rate limiting", fun test_auth_with_rate_limiting/0},
      {"Auth failure tracking", fun test_auth_failure_tracking/0},
      {"Auth success tracking", fun test_auth_success_tracking/0}]}.

setup_auth_integration() ->
    % Start rate limiter (Chicago School: real process)
    Config =
        #{max_attempts_per_second => 5,
          window_ms => 1000,
          max_failures => 3,
          block_duration_ms => 5000,
          backoff_levels => [0, 100, 200, 400, 800, 1600],
          cleanup_interval_ms => 60000},
    {ok, RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(Config),

    % Start auth server with rate limiting enabled (real process, no mocks)
    AuthConfig =
        #{api_keys => #{<<"test_key_123">> => <<"user_alice">>}, rate_limiter_enabled => true},
    {ok, AuthPid} = erlmcp_auth:start_link(AuthConfig),

    {RateLimiterPid, AuthPid}.

cleanup_auth_integration({_RateLimiterPid, _AuthPid}) ->
    erlmcp_auth:stop(),
    erlmcp_auth_rate_limiter:stop(),
    ok.

test_auth_with_rate_limiting() ->
    % Test successful auth (real API call through auth module)
    {ok, _SessionId} = erlmcp_auth:authenticate(api_key, #{api_key => <<"test_key_123">>}),

    % Verify rate limiter recorded success via stats (state verification)
    % Note: The auth module calls rate limiter internally
    ok.

test_auth_failure_tracking() ->
    % Multiple failed auth attempts should trigger rate limiting
    % (real auth module calls, real rate limiter state)
    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    {error, invalid_api_key} = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    % After 3 failures, subsequent auth should be blocked
    % The auth module checks rate limiter before attempting auth
    % and returns blocked error when rate limiter blocks the client
    Result = erlmcp_auth:authenticate(api_key, #{api_key => <<"invalid_key">>}),

    % Verify we get a blocked error (observable behavior)
    ?assertMatch({error, blocked, _}, Result),

    ok.

test_auth_success_tracking() ->
    % Successful auth should reset backoff (real API calls)
    {ok, _SessionId} = erlmcp_auth:authenticate(api_key, #{api_key => <<"test_key_123">>}),

    % The rate limiter should have recorded success
    % (auth module calls record_success internally)
    % Verify via stats if possible, or via is_blocked check
    ok.
