%%%-------------------------------------------------------------------
%%% @doc erlmcp_transport_sse_security_tests - Integration Tests for FM-01 + FM-06
%%%
%%% Tests origin validation (FM-01) and header validation (FM-06)
%%% integration into SSE transport.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sse_security_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start required applications
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),
    application:ensure_all_started(jsx),

    %% Start SSE transport on test port
    Port = 18081,
    TransportId = <<"test_sse_security">>,
    Config = #{
        port => Port,
        path => "/mcp/sse",
        allowed_origins => [
            <<"http://localhost">>,
            <<"http://localhost:3000">>,
            <<"http://example.com">>
        ]
    },

    {ok, _Pid} = erlmcp_transport_sse:init(TransportId, Config),

    %% Wait for server to be ready
    timer:sleep(100),

    #{port => Port, transport_id => TransportId}.

cleanup(#{port := Port}) ->
    %% Stop Cowboy listener
    cowboy:stop_listener(erlmcp_sse_listener),
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
          ?_test(test_valid_origin_localhost(Context)),
          ?_test(test_valid_origin_example_com(Context)),
          ?_test(test_invalid_origin_blocked(Context)),
          ?_test(test_dns_rebinding_attack_blocked(Context)),
          ?_test(test_no_origin_header_allowed(Context))
         ]
     end}.

test_valid_origin_localhost(#{port := Port}) ->
    %% Valid origin should be accepted
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 200 OK with SSE content-type
    receive
        {gun_response, ConnPid, StreamRef, nofin, Status, RespHeaders} ->
            ?assertEqual(200, Status),
            ContentType = proplists:get_value(<<"content-type">>, RespHeaders),
            ?assertEqual(<<"text/event-stream">>, ContentType)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_valid_origin_example_com(#{port := Port}) ->
    %% Valid origin from allowed list should be accepted
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://example.com">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 200 OK
    receive
        {gun_response, ConnPid, StreamRef, nofin, Status, _RespHeaders} ->
            ?assertEqual(200, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_invalid_origin_blocked(#{port := Port}) ->
    %% Invalid origin should be blocked with 403
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://evil.com">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 403 Forbidden
    receive
        {gun_response, ConnPid, StreamRef, IsFin, Status, RespHeaders} ->
            ?assertEqual(403, Status),

            %% Should be JSON-RPC error
            ContentType = proplists:get_value(<<"content-type">>, RespHeaders),
            ?assertEqual(<<"application/json">>, ContentType),

            %% Get response body
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

            %% Verify JSON-RPC error structure
            Error = jsx:decode(Body, [return_maps]),
            ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Error)),
            ErrorObj = maps:get(<<"error">>, Error),
            ?assertEqual(-32600, maps:get(<<"code">>, ErrorObj)),
            ?assertEqual(<<"Origin validation failed">>, maps:get(<<"message">>, ErrorObj))
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_dns_rebinding_attack_blocked(#{port := Port}) ->
    %% Simulated DNS rebinding attack with malicious origin
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://attacker.local">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 403 Forbidden (DNS rebinding blocked)
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(403, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_no_origin_header_allowed(#{port := Port}) ->
    %% No origin header (local development) should be allowed
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 200 OK (no origin allowed for local dev)
    receive
        {gun_response, ConnPid, StreamRef, nofin, Status, _RespHeaders} ->
            ?assertEqual(200, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
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
          ?_test(test_valid_accept_header(Context)),
          ?_test(test_missing_accept_header_rejected(Context)),
          ?_test(test_invalid_accept_header_rejected(Context)),
          ?_test(test_post_valid_content_type(Context)),
          ?_test(test_post_invalid_content_type_rejected(Context))
         ]
     end}.

test_valid_accept_header(#{port := Port}) ->
    %% Valid Accept header for SSE
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 200 OK
    receive
        {gun_response, ConnPid, StreamRef, nofin, Status, _RespHeaders} ->
            ?assertEqual(200, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_missing_accept_header_rejected(#{port := Port}) ->
    %% Missing Accept header should be rejected
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 400 Bad Request
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(400, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_invalid_accept_header_rejected(#{port := Port}) ->
    %% Invalid Accept header should be rejected
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"accept">>, <<"text/html">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 400 Bad Request
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(400, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_post_valid_content_type(#{port := Port}) ->
    %% Valid Content-Type for POST
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"content-type">>, <<"application/json">>}
    ],

    Body = jsx:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"ping">>}),

    StreamRef = gun:post(ConnPid, "/mcp/sse", Headers, Body),

    %% Should get 202 Accepted
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
            ?assertEqual(202, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_post_invalid_content_type_rejected(#{port := Port}) ->
    %% Invalid Content-Type for POST should be rejected
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"content-type">>, <<"text/plain">>}
    ],

    Body = <<"test">>,

    StreamRef = gun:post(ConnPid, "/mcp/sse", Headers, Body),

    %% Should get 400 Bad Request
    receive
        {gun_response, ConnPid, StreamRef, _IsFin, Status, _RespHeaders} ->
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
          ?_test(test_both_validations_pass(Context)),
          ?_test(test_origin_fails_before_headers(Context))
         ]
     end}.

test_both_validations_pass(#{port := Port}) ->
    %% Both origin and headers valid
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost:3000">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

    %% Should get 200 OK
    receive
        {gun_response, ConnPid, StreamRef, nofin, Status, _RespHeaders} ->
            ?assertEqual(200, Status)
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_origin_fails_before_headers(#{port := Port}) ->
    %% Invalid origin should fail BEFORE header validation
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://evil.com">>},
        {<<"accept">>, <<"text/html">>}  %% Also invalid, but shouldn't reach this check
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

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
          ?_test(test_origin_error_has_json_rpc_format(Context)),
          ?_test(test_header_error_has_json_rpc_format(Context))
         ]
     end}.

test_origin_error_has_json_rpc_format(#{port := Port}) ->
    %% Origin validation error should return JSON-RPC format
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://malicious.com">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

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
            ?assertEqual(<<"Origin validation failed">>, maps:get(<<"message">>, ErrorObj)),

            %% Verify data field exists
            Data = maps:get(<<"data">>, ErrorObj),
            ?assert(is_map(Data)),
            ?assertEqual(<<"http://malicious.com">>, maps:get(<<"origin">>, Data))
    after 1000 ->
        ?assert(false, "Timeout waiting for response")
    end,

    gun:close(ConnPid).

test_header_error_has_json_rpc_format(#{port := Port}) ->
    %% Header validation error should return JSON-RPC format
    {ok, ConnPid} = gun:open("localhost", Port),
    {ok, _Protocol} = gun:await_up(ConnPid),

    Headers = [
        {<<"origin">>, <<"http://localhost">>},
        {<<"accept">>, <<"application/json">>}  %% Wrong for SSE
    ],

    StreamRef = gun:get(ConnPid, "/mcp/sse", Headers),

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
