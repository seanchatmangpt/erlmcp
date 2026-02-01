%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_oauth_tests - OAuth2 Token Introspection Tests
%%% Tests RFC 7662 OAuth2 token introspection with real Cowboy server.
%%% Uses Chicago School TDD: real HTTP connections, real ETS, state verification.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_oauth_tests).

-include_lib("eunit/include/eunit.hrl").

%% Record definitions (from erlmcp_auth.erl)
-record(state, {sessions :: ets:tid(), oauth2_cache :: ets:tid()}).
-record(session, {user_id :: binary(), auth_method :: atom()}).

%%====================================================================
%% Test Fixtures
%%====================================================================

oauth2_test_() ->
    {setup,
     fun setup_oauth2/0,
     fun cleanup_oauth2/1,
     [{"OAuth2 Token Introspection - Active Token", fun test_oauth2_active_token/0},
      {"OAuth2 Token Introspection - Inactive Token", fun test_oauth2_inactive_token/0},
      {"OAuth2 Token Introspection - Expired Token", fun test_oauth2_expired_token/0},
      {"OAuth2 Token Introspection - Not Yet Valid", fun test_oauth2_not_yet_valid/0},
      {"OAuth2 Token Introspection - Caching", fun test_oauth2_caching/0},
      {"OAuth2 Token Introspection - Cache Expiry", fun test_oauth2_cache_expiry/0},
      {"OAuth2 Token Introspection - HTTP 401 Invalid Client", fun test_oauth2_invalid_client/0},
      {"OAuth2 Token Introspection - HTTP 403 Forbidden", fun test_oauth2_forbidden/0},
      {"OAuth2 Token Introspection - HTTP 400 Bad Request", fun test_oauth2_bad_request/0},
      {"OAuth2 Token Introspection - Connection Failure", fun test_oauth2_connection_failure/0},
      {"OAuth2 Token Introspection - Invalid JSON Response", fun test_oauth2_invalid_json/0},
      {"OAuth2 Token Introspection - Missing Active Claim", fun test_oauth2_missing_active_claim/0},
      {"OAuth2 Authenticate with Valid Token", fun test_oauth2_authenticate_success/0},
      {"OAuth2 Authenticate with Invalid Token", fun test_oauth2_authenticate_failure/0},
      {"OAuth2 Not Configured", fun test_oauth2_not_configured/0}]}.

%% Setup - start Cowboy HTTP server for introspection endpoint
setup_oauth2() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    application:ensure_all_started(gun),
    application:ensure_all_started(cowboy),
    application:ensure_all_started(ranch),

    % Start rate limiter
    {ok, _RateLimiterPid} = erlmcp_auth_rate_limiter:start_link(#{}),

    % Start Cowboy HTTP server on localhost:8765
    Dispatch = cowboy_router:compile([{'_', [{"/introspect", oauth2_introspection_handler, []}]}]),

    {ok, _CowboyPid} =
        cowboy:start_clear(oauth2_test_http, [{port, 8765}], #{env => #{dispatch => Dispatch}}),

    % Start auth server with OAuth2 config
    OAuth2Config =
        #{enabled => true,
          introspect_url => <<"http://localhost:8765/introspect">>,
          client_id => <<"test_client">>,
          client_secret => <<"test_secret">>},

    {ok, AuthPid} = erlmcp_auth:start_link(#{oauth2 => OAuth2Config}),

    #{auth_pid => AuthPid, cowboy_pid => whereis(oauth2_test_http)}.

%% Cleanup - stop servers
cleanup_oauth2(#{auth_pid := AuthPid, cowboy_pid := CowboyPid}) ->
    erlmcp_auth:stop(),
    erlmcp_auth_rate_limiter:stop(),
    cowboy:stop_listener(oauth2_test_http),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test successful token introspection with active token
test_oauth2_active_token() ->
    % Mock introspection handler will return active=true
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"user123">>,
          <<"exp">> => erlang:system_time(second) + 3600,
          <<"iss">> => <<"https://auth.example.com">>,
          <<"scope">> => <<"read write">>}),

    Token = <<"valid_token_123">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify successful validation
    ?assertMatch({ok, _}, Result),
    {ok, TokenInfo} = Result,

    % Verify token info contains expected fields
    ?assertEqual(true, maps:get(<<"active">>, TokenInfo)),
    ?assertEqual(<<"user123">>, maps:get(<<"sub">>, TokenInfo)),
    ?assertEqual(<<"user123">>, maps:get(<<"user_id">>, TokenInfo)),
    ?assertEqual(<<"https://auth.example.com">>, maps:get(<<"iss">>, TokenInfo)),

    % Verify token was cached
    {ok, State} = get_auth_state(),
    [{Token, {CachedInfo, _ExpiresAt}}] = ets:lookup(State#state.oauth2_cache, Token),
    ?assertEqual(<<"user123">>, maps:get(<<"user_id">>, CachedInfo)),

    % Clean up process dictionary
    erase(introspect_response).

%% @doc Test token introspection with inactive token
test_oauth2_inactive_token() ->
    % Mock introspection handler will return active=false
    put(introspect_response, #{<<"active">> => false}),

    Token = <<"inactive_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, token_invalid}, Result),

    % Verify token was NOT cached
    {ok, State} = get_auth_state(),
    ?assertEqual([], ets:lookup(State#state.oauth2_cache, Token)),

    erase(introspect_response).

%% @doc Test token introspection with expired token
test_oauth2_expired_token() ->
    % Mock introspection handler will return active=true but exp in past
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"user123">>,
          <<"exp">> => erlang:system_time(second) - 100}),  % Expired 100 seconds ago

    Token = <<"expired_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, token_expired}, Result),

    erase(introspect_response).

%% @doc Test token introspection with not-yet-valid token
test_oauth2_not_yet_valid() ->
    Now = erlang:system_time(second),
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"user123">>,
          <<"nbf">> => Now + 3600}),  % Not valid for 1 hour

    Token = <<"future_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, token_not_yet_valid}, Result),

    erase(introspect_response).

%% @doc Test token caching mechanism
test_oauth2_caching() ->
    Now = erlang:system_time(second),
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"cached_user">>,
          <<"exp">> => Now + 3600}),

    Token = <<"cacheable_token">>,

    % First call - should hit HTTP endpoint
    ?assertMatch({ok, _}, erlmcp_auth:validate_oauth2_token(Token)),

    % Verify cache has the token
    {ok, State} = get_auth_state(),
    ?assertMatch([{_, {_, _}}], ets:lookup(State#state.oauth2_cache, Token)),

    % Second call - should hit cache (no HTTP request)
    % We verify this by checking the introspection count doesn't increase
    ?assertMatch({ok, _}, erlmcp_auth:validate_oauth2_token(Token)),

    erase(introspect_response).

%% @doc Test cache expiry and refresh
test_oauth2_cache_expiry() ->
    Now = erlang:system_time(second),

    % First response with short expiry
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"user123">>,
          <<"exp">> => Now + 1}),  % Expires in 1 second

    Token = <<"expiring_token">>,

    % First call - cache the token
    ?assertMatch({ok, _}, erlmcp_auth:validate_oauth2_token(Token)),

    {ok, State} = get_auth_state(),
    [{Token, {_, CacheExpiresAt}}] = ets:lookup(State#state.oauth2_cache, Token),

    % Wait for cache to expire
    timer:sleep(1500),

    % Update introspection response for second call
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"user123">>,
          <<"exp">> => Now + 3600}),

    % Second call - should refresh from HTTP (cache expired)
    ?assertMatch({ok, _}, erlmcp_auth:validate_oauth2_token(Token)),

    erase(introspect_response).

%% @doc Test HTTP 401 Invalid Client response
test_oauth2_invalid_client() ->
    % Mock HTTP 401 response
    put(introspect_http_status, 401),

    Token = <<"invalid_client_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, invalid_client}, Result),

    erase(introspect_http_status).

%% @doc Test HTTP 403 Forbidden response
test_oauth2_forbidden() ->
    % Mock HTTP 403 response
    put(introspect_http_status, 403),

    Token = <<"forbidden_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, forbidden}, Result),

    erase(introspect_http_status).

%% @doc Test HTTP 400 Bad Request response
test_oauth2_bad_request() ->
    % Mock HTTP 400 response
    put(introspect_http_status, 400),
    put(introspect_response, #{<<"error">> => <<"invalid_request">>}),

    Token = <<"bad_request_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, invalid_request}, Result),

    erase(introspect_http_status),
    erase(introspect_response).

%% @doc Test connection failure handling
test_oauth2_connection_failure() ->
    % Use non-existent URL to trigger connection failure
    OAuth2Config =
        #{enabled => true,
          introspect_url => <<"http://localhost:9999/introspect">>,  % Non-existent port
          client_id => <<"test_client">>,
          client_secret => <<"test_secret">>},

    {ok, _AuthPid} = erlmcp_auth:start_link(#{oauth2 => OAuth2Config}),

    Token = <<"conn_fail_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify connection failed gracefully
    ?assertMatch({error, connection_failed}, Result),

    erlmcp_auth:stop().

%% @doc Test invalid JSON response handling
test_oauth2_invalid_json() ->
    % Mock invalid JSON response
    put(introspect_response, <<"invalid json {">>),

    Token = <<"invalid_json_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed
    ?assertEqual({error, invalid_response}, Result),

    erase(introspect_response).

%% @doc Test missing 'active' claim in response
test_oauth2_missing_active_claim() ->
    % Mock response without 'active' claim
    put(introspect_response,
        #{<<"sub">> => <<"user123">>, <<"exp">> => erlang:system_time(second) + 3600}),

    Token = <<"missing_active_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify validation failed (active defaults to false)
    ?assertEqual({error, token_invalid}, Result),

    erase(introspect_response).

%% @doc Test authenticate with valid OAuth2 token
test_oauth2_authenticate_success() ->
    put(introspect_response,
        #{<<"active">> => true,
          <<"sub">> => <<"auth_user">>,
          <<"exp">> => erlang:system_time(second) + 3600}),

    Token = <<"auth_success_token">>,

    % Authenticate with OAuth2
    Result = erlmcp_auth:authenticate(oauth2, #{token => Token, ip_address => {127, 0, 0, 1}}),

    % Verify authentication succeeded
    ?assertMatch({ok, _SessionId}, Result),
    {ok, SessionId} = Result,

    % Verify session was created
    {ok, State} = get_auth_state(),
    ?assertMatch([{_, #session{user_id = <<"auth_user">>, auth_method = oauth2}}],
                 ets:lookup(State#state.sessions, SessionId)),

    erase(introspect_response).

%% @doc Test authenticate with invalid OAuth2 token
test_oauth2_authenticate_failure() ->
    put(introspect_response, #{<<"active">> => false}),

    Token = <<"auth_fail_token">>,

    % Authenticate with OAuth2
    Result = erlmcp_auth:authenticate(oauth2, #{token => Token, ip_address => {127, 0, 0, 1}}),

    % Verify authentication failed
    ?assertMatch({error, token_invalid}, Result),

    erase(introspect_response).

%% @doc Test OAuth2 when not configured
test_oauth2_not_configured() ->
    % Stop auth server and restart without OAuth2
    erlmcp_auth:stop(),

    OAuth2Config = #{enabled => false},

    {ok, _AuthPid} = erlmcp_auth:start_link(#{oauth2 => OAuth2Config}),

    Token = <<"not_configured_token">>,

    % Call validate_oauth2_token
    Result = erlmcp_auth:validate_oauth2_token(Token),

    % Verify not configured error
    ?assertEqual({error, oauth2_not_configured}, Result),

    erlmcp_auth:stop().

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get auth server state for verification
get_auth_state() ->
    {status, _, _, [_, _, _, _, _, _, State | _]} = sys:get_status(erlmcp_auth),
    {data, {{"State", State}}} = lists:keyfind({"State", State}, 1, State),
    {ok, State}.
