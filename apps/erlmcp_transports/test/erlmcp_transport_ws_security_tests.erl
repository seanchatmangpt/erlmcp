%%%-------------------------------------------------------------------
%%% @doc erlmcp_transport_ws_security_tests - Integration Tests for FM-01 + FM-06 (WebSocket)
%%%
%%% Tests origin validation (FM-01) and header validation (FM-06)
%%% integration into WebSocket transport.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_ws_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),
    application:ensure_all_started(jsx),

    %% Start WebSocket transport on test port
    Port = 18080,
    TransportId = <<"test_ws_security">>,
    Config = #{
        port => Port,
        path => "/mcp/ws",
        allowed_origins => [
            <<"http://localhost">>,
            <<"http://localhost:3000">>,
            <<"http://example.com">>
        ]
    },

    {ok, _Pid} = erlmcp_transport_ws:init(TransportId, Config),

    %% Wait for server to be ready
    timer:sleep(100),

    #{port => Port, transport_id => TransportId}.

cleanup(#{port := Port}) ->
    %% Stop Cowboy listener
    cowboy:stop_listener(erlmcp_ws_listener),
    ok.

%%====================================================================
%% FM-01: Origin Validation Tests (DNS Rebinding Protection)
%%====================================================================

origin_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Context) ->
         [
          ?_test(test_valid_origin_ws_upgrade(Context)),
          ?_test(test_invalid_origin_blocked_ws(Context)),
          ?_test(test_dns_rebinding_blocked_ws(Context)),
          ?_test(test_no_origin_allowed_ws(Context))
         ]
     end}.

test_valid_origin_ws_upgrade(#{port := Port}) ->
    %% Valid origin should allow WebSocket upgrade
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get upgrade response
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
            ?assert(true, "WebSocket upgrade successful")
    after 1000 ->
        ?assert(false, "Timeout waiting for WebSocket upgrade")
    end,

    gun:close(ConnPid).

test_invalid_origin_blocked_ws(#{port := Port}) ->
    %% Invalid origin should block WebSocket upgrade
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://evil.com">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get error response, not upgrade
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(403, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_dns_rebinding_blocked_ws(#{port := Port}) ->
    %% Simulated DNS rebinding attack should be blocked
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://attacker.local">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get 403 Forbidden
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(403, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_no_origin_allowed_ws(#{port := Port}) ->
    %% No origin header (local development) should be allowed
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get upgrade response
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
            ?assert(true, "WebSocket upgrade successful without origin")
    after 1000 ->
        ?assert(false, "Timeout waiting for WebSocket upgrade")
    end,

    gun:close(ConnPid).

%%====================================================================
%% FM-06: Header Validation Tests (Protocol Downgrade Protection)
%%====================================================================

header_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Context) ->
         [
          ?_test(test_valid_websocket_headers(Context)),
          ?_test(test_missing_sec_websocket_version(Context)),
          ?_test(test_invalid_websocket_protocol(Context))
         ]
     end}.

test_valid_websocket_headers(#{port := Port}) ->
    %% Valid WebSocket headers should allow upgrade
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get upgrade response
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
            ?assert(true, "Valid headers accepted")
    after 1000 ->
        ?assert(false, "Timeout waiting for upgrade")
    end,

    gun:close(ConnPid).

test_missing_sec_websocket_version(#{port := Port}) ->
    %% Missing required WebSocket headers should be rejected
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    %% Try to upgrade without proper WebSocket headers
    Headers = [
        {<<"origin">>, <<"http://localhost">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/ws", Headers),

    %% Should get error response
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            %% Should be 400 or 426 (Upgrade Required)
            ?assert(Status =:= 400 orelse Status =:= 426)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_invalid_websocket_protocol(#{port := Port}) ->
    %% Invalid WebSocket protocol should be rejected
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"sec-websocket-version">>, <<"8">>}  %% Invalid version
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get error response
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            %% Should be 400 Bad Request
            ?assertEqual(400, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

%%====================================================================
%% Combined Security Tests
%%====================================================================

combined_security_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Context) ->
         [
          ?_test(test_both_validations_pass_ws(Context)),
          ?_test(test_origin_fails_before_headers_ws(Context))
         ]
     end}.

test_both_validations_pass_ws(#{port := Port}) ->
    %% Both origin and headers valid
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost:3000">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get upgrade response
    receive
        {gun_upgrade, ConnPid, StreamRef, [<<"websocket">>], _RespHeaders} ->
            ?assert(true, "Both validations passed")
    after 1000 ->
        ?assert(false, "Timeout waiting for upgrade")
    end,

    gun:close(ConnPid).

test_origin_fails_before_headers_ws(#{port := Port}) ->
    %% Invalid origin should fail BEFORE header validation
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://evil.com">>},
        {<<"sec-websocket-version">>, <<"8">>}  %% Also invalid
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    %% Should get 403 Forbidden (origin validation failure)
    %% NOT 400 (which would be header validation failure)
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(403, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

%%====================================================================
%% Error Response Format Tests
%%====================================================================

error_response_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Context) ->
         [
          ?_test(test_origin_error_json_rpc_format_ws(Context)),
          ?_test(test_header_error_json_rpc_format_ws(Context))
         ]
     end}.

test_origin_error_json_rpc_format_ws(#{port := Port}) ->
    %% Origin validation error should return JSON-RPC format
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://malicious.com">>},
        {<<"sec-websocket-version">>, <<"13">>}
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    receive
        {gun_response, ConnPid, StreamRef, IsFin, 403, RespHeaders} ->
            %% Verify content-type
            ContentType = proplists:get_value(<<"content-type">>, RespHeaders),
            ?assertEqual(<<"application/json">>, ContentType),

            %% Get body
            Body = case IsFin of
                fin -> <<>>;
                nofin ->
                    receive
                        {gun_data, ConnPid, StreamRef, fin, Data} ->
                            Data
                    after 1000 ->
                        <<>>
                    end
            end,

            %% Verify JSON-RPC structure
            Error = jsx:decode(Body, [return_maps]),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Error)),

            ErrorObj = maps:get(<<"error">>, Error),
            ?assertEqual(-32600, maps:get(<<"code">>, ErrorObj)),
            ?assertEqual(<<"Origin validation failed">>, maps:get(<<"message">>, ErrorObj))
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_header_error_json_rpc_format_ws(#{port := Port}) ->
    %% Header validation error should return JSON-RPC format
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"sec-websocket-version">>, <<"8">>}  %% Invalid version
    ],

    StreamRef = gun:ws_upgrade(ConnPid, "/mcp/ws", Headers),

    receive
        {gun_response, ConnPid, StreamRef, IsFin, 400, RespHeaders} ->
            %% Verify content-type
            ContentType = proplists:get_value(<<"content-type">>, RespHeaders),
            ?assertEqual(<<"application/json">>, ContentType),

            %% Get body
            Body = case IsFin of
                fin -> <<>>;
                nofin ->
                    receive
                        {gun_data, ConnPid, StreamRef, fin, Data} ->
                            Data
                    after 1000 ->
                        <<>>
                    end
            end,

            %% Verify it's JSON
            Error = jsx:decode(Body, [return_maps]),
            ?assert(is_map(Error)),
            ?assertEqual(<<"header_validation_failed">>, maps:get(<<"error">>, Error))
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).
