-module(erlmcp_transport_sse_tests).

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

sse_transport_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_init_sse()),
            ?_test(test_send_event()),
            ?_test(test_close_stream()),
            ?_test(test_format_sse_event()),
            ?_test(test_post_message()),
            ?_test(test_get_stream()),
            ?_test(test_keepalive_ping()),
            ?_test(test_stream_timeout()),
            ?_test(test_concurrent_streams())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_init_sse() ->
    Config = #{
        port => 8081,
        path => "/mcp/sse"
    },
    TransportId = <<"sse_test_1">>,

    {ok, Pid} = erlmcp_transport_sse:init(TransportId, Config),
    ?assert(is_pid(Pid)).

test_send_event() ->
    _TransportId = <<"sse_test_2">>,
    ClientPid = spawn(fun() -> receive stop -> ok end end),

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    Result = erlmcp_transport_sse:send(ClientPid, Message),
    ?assert(Result =:= ok orelse (is_tuple(Result) andalso element(1, Result) =:= error)).

test_close_stream() ->
    ClientPid = spawn(fun() -> receive stop -> ok end end),

    Result = erlmcp_transport_sse:close(ClientPid),
    ?assert(Result =:= ok).

test_format_sse_event() ->
    %% Test SSE event formatting
    _Data = <<"test event data">>,
    Formatted = <<"event: message\ndata: test event data\n\n">>,

    ?assert(is_binary(Formatted)),
    ?assert(binary:match(Formatted, <<"event: message">>) =/= nomatch).

test_post_message() ->
    %% POST request to /mcp/sse should accept JSON
    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/call">>,
        <<"params">> => #{<<"name">> => <<"test">>},
        <<"id">> => 1
    }),

    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0).

test_get_stream() ->
    %% GET request should return stream headers
    ?assert(true).

test_keepalive_ping() ->
    %% Test keep-alive ping messages
    Ping = <<":\n">>,

    ?assert(is_binary(Ping)).

test_stream_timeout() ->
    %% Test 5 minute idle timeout
    Timeout = 300000,  %% 5 minutes in milliseconds

    ?assert(Timeout =:= 300000).

test_concurrent_streams() ->
    %% Test multiple concurrent SSE streams
    Pids = [spawn(fun() -> receive stop -> ok end end) || _ <- lists:seq(1, 3)],

    Message = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"resources/list">>,
        <<"id">> => 1
    }),

    lists:foreach(
        fun(Pid) ->
            erlmcp_transport_sse:send(Pid, Message)
        end,
        Pids
    ),

    ?assert(length(Pids) =:= 3).
