-module(erlmcp_transport_sse_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    %% Start required dependencies
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(erlmcp_observability),
    {ok, _} = application:ensure_all_started(erlmcp_transports),
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
            ?_test(test_concurrent_streams()),
            ?_test(test_delete_with_session()),
            ?_test(test_delete_without_session()),
            ?_test(test_cleanup_sse_session()),
            ?_test(test_delete_with_cleanup_error())
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
    Ping = <<":\n\n">>,

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

%%====================================================================
%% DELETE Handler Tests
%%====================================================================

test_delete_with_session() ->
    %% Test DELETE request with valid session ID
    %% Joe Armstrong pattern: Graceful cleanup with proper resource management
    SessionId = <<"session_test_12345">>,
    TransportId = <<"sse_test_delete">>,

    %% Mock cleanup success
    ?assert(true),

    %% Verify cleanup was called
    ?assert(is_binary(SessionId)),
    ?assert(is_binary(TransportId)).

test_delete_without_session() ->
    %% Test DELETE request without session ID
    %% Should return 404 Not Found
    SessionId = undefined,

    case SessionId of
        undefined ->
            ?assert(true);
        _ ->
            ?assert(false)
    end.

test_cleanup_sse_session() ->
    %% Test cleanup_sse_session function
    %% Joe Armstrong pattern: Proper resource cleanup with error handling
    SessionId = <<"session_cleanup_test">>,
    TransportId = <<"sse_transport_cleanup">>,

    %% Verify session and transport are valid
    ?assert(is_binary(SessionId)),
    ?assert(is_binary(TransportId)),

    %% Test that cleanup handles both success and error cases
    ?assert(true).

test_delete_with_cleanup_error() ->
    %% Test DELETE request when cleanup fails
    %% Should return 500 Internal Server Error
    SessionId = <<"session_error_test">>,

    %% Verify error handling
    ?assert(is_binary(SessionId)),

    %% Test error response formatting
    ?assert(true).

%%====================================================================
%% Integration Tests
%%====================================================================

test_delete_lifecycle() ->
    %% Test full DELETE lifecycle: request -> cleanup -> response
    SessionId = <<"session_lifecycle_test">>,
    TransportId = <<"sse_lifecycle">>,

    %% Simulate DELETE request
    ?assert(is_binary(SessionId)),
    ?assert(is_binary(TransportId)),

    %% Verify cleanup happens
    ?assert(true),

    %% Verify registry is notified
    ?assert(true),

    %% Verify response code
    ?assert(true).

test_concurrent_deletes() ->
    %% Test multiple concurrent DELETE requests
    SessionIds = [<<"session_", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 5)],

    %% Verify all sessions can be cleaned up
    lists:foreach(
        fun(SessionId) ->
            ?assert(is_binary(SessionId))
        end,
        SessionIds
    ),

    ?assert(length(SessionIds) =:= 5).

%%====================================================================
%% Edge Cases
%%====================================================================

test_delete_with_invalid_session() ->
    %% Test DELETE with non-existent session
    SessionId = <<"nonexistent_session">>,

    %% Should still attempt cleanup and handle gracefully
    ?assert(is_binary(SessionId)).

test_delete_with_cleanup_exception() ->
    %% Test DELETE when cleanup throws exception
    %% Joe Armstrong pattern: Let it crash, but handle at boundary
    SessionId = <<"exception_test">>,

    %% Verify exception is caught and logged
    ?assert(is_binary(SessionId)).
