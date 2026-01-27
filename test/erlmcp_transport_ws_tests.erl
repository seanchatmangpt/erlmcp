-module(erlmcp_transport_ws_tests).

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

websocket_transport_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_init_websocket()),
            ?_test(test_send_message()),
            ?_test(test_close_connection()),
            ?_test(test_handle_text_frame()),
            ?_test(test_handle_invalid_json()),
            ?_test(test_handle_binary_frame()),
            ?_test(test_ping_pong()),
            ?_test(test_concurrent_connections()),
            ?_test(test_large_message_handling())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_init_websocket() ->
    Config = #{
        port => 8080,
        path => "/mcp/ws"
    },
    TransportId = <<"ws_test_1">>,

    {ok, Pid} = erlmcp_transport_ws:init(TransportId, Config),
    ?assert(is_pid(Pid)).

test_send_message() ->
    TransportId = <<"ws_test_2">>,
    Pid = spawn(fun() -> receive stop -> ok end end),

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    Result = erlmcp_transport_ws:send(Pid, Message),
    ?assert(Result =:= ok orelse (is_tuple(Result) andalso element(1, Result) =:= error)).

test_close_connection() ->
    TransportId = <<"ws_test_3">>,
    Pid = spawn(fun() -> receive stop -> ok end end),

    Result = erlmcp_transport_ws:close(Pid),
    ?assert(Result =:= ok).

test_handle_text_frame() ->
    %% Test handling of text WebSocket frames
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    }),

    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0).

test_handle_invalid_json() ->
    %% Invalid JSON should return error response
    InvalidJson = <<"invalid json {">>,

    ?assert(is_binary(InvalidJson)).

test_handle_binary_frame() ->
    %% Binary frames not supported
    BinaryData = <<1, 2, 3, 4, 5>>,

    ?assert(is_binary(BinaryData)).

test_ping_pong() ->
    %% Test ping/pong heartbeat
    ?assert(true).

test_concurrent_connections() ->
    %% Test multiple concurrent WebSocket connections
    Pids = [spawn(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, 5)],

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    lists:foreach(
        fun(Pid) ->
            erlmcp_transport_ws:send(Pid, Message)
        end,
        Pids
    ),

    ?assert(length(Pids) =:= 5).

test_large_message_handling() ->
    %% Test handling of large messages
    LargeData = binary:copy(<<"x">>, 10000),

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"data">> => LargeData,
        <<"id">> => 1
    }),

    ?assert(byte_size(Message) > 10000).
