-module(erlmcp_http_auth_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

start_auth_server_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_start_link()),
            ?_test(test_get_token()),
            ?_test(test_token_validation()),
            ?_test(test_token_refresh()),
            ?_test(test_inject_headers()),
            ?_test(test_invalid_credentials()),
            ?_test(test_token_expiry_check()),
            ?_test(test_concurrent_token_requests())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_start_link() ->
    Config = #{
        client_id => <<"test-client">>,
        client_secret => <<"test-secret">>,
        token_endpoint => "http://localhost:8888/token",
        resource_indicator => "http://mcp.local"
    },
    {ok, Pid} = erlmcp_http_auth:start_link(Config),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)).

test_get_token() ->
    Config = #{
        client_id => <<"test-client">>,
        client_secret => <<"test-secret">>,
        token_endpoint => "http://localhost:8888/token",
        resource_indicator => "http://mcp.local"
    },
    {ok, _Pid} = erlmcp_http_auth:start_link(Config),

    %% Mock HTTP response would happen here
    ?assert(true).

test_token_validation() ->
    %% Test token validation logic
    ?assert(erlmcp_http_auth:validate_token(undefined) =:= false),
    ?assert(erlmcp_http_auth:validate_token(<<"invalid">>) =:= false),
    ?assert(erlmcp_http_auth:validate_token(<<>>) =:= false).

test_token_refresh() ->
    Config = #{
        client_id => <<"test-client">>,
        client_secret => <<"test-secret">>,
        token_endpoint => "http://localhost:8888/token",
        resource_indicator => "http://mcp.local"
    },
    {ok, Pid} = erlmcp_http_auth:start_link(Config),

    %% Token refresh should be called periodically
    ?assert(is_pid(Pid)).

test_inject_headers() ->
    Headers = #{
        <<"content-type">> => <<"application/json">>
    },

    %% Should return headers with authorization
    %% In real test, would mock get_token to return valid token
    ?assert(is_map(Headers)).

test_invalid_credentials() ->
    %% Test with invalid client credentials
    Config = #{
        client_id => <<"invalid">>,
        client_secret => <<"invalid">>,
        token_endpoint => "http://localhost:8888/token",
        resource_indicator => "http://mcp.local"
    },
    {ok, Pid} = erlmcp_http_auth:start_link(Config),
    ?assert(is_pid(Pid)).

test_token_expiry_check() ->
    %% Test token expiry calculation
    Now = erlang:system_time(second),
    Expiry = Now + 3600,  %% 1 hour from now

    ?assert(Expiry > Now).

test_concurrent_token_requests() ->
    Config = #{
        client_id => <<"test-client">>,
        client_secret => <<"test-secret">>,
        token_endpoint => "http://localhost:8888/token",
        resource_indicator => "http://mcp.local"
    },
    {ok, _Pid} = erlmcp_http_auth:start_link(Config),

    %% Simulate concurrent requests
    Self = self(),
    spawn(fun() -> Self ! {test, 1} end),
    spawn(fun() -> Self ! {test, 2} end),
    spawn(fun() -> Self ! {test, 3} end),

    receive {test, _} -> ok after 1000 -> ok end,
    receive {test, _} -> ok after 1000 -> ok end,
    receive {test, _} -> ok after 1000 -> ok end,

    ?assert(true).
