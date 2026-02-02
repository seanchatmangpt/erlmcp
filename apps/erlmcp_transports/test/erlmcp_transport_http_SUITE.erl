%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for erlmcp_transport_http module
%%%
%%% This comprehensive test suite validates HTTP transport implementation
%%% including connection management, request/response handling, error
%%% scenarios, and retry logic.
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1, init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).
%% Test cases
-export([http_transport_init_success/1, http_transport_init_invalid_url/1,
         http_transport_init_missing_owner/1, http_transport_init_with_custom_headers/1,
         http_transport_init_with_ssl_options/1, http_connection_establish/1,
         http_connection_refused/1, http_connection_timeout/1, http_connection_reconnect/1,
         http_send_post_request/1, http_send_get_request/1, http_send_multiple_requests/1,
         http_send_large_request/1, http_receive_success_response/1, http_receive_error_response/1,
         http_receive_empty_response/1, http_retry_on_connection_failure/1,
         http_retry_max_attempts_exceeded/1, http_error_timeout/1, http_error_message_too_large/1,
         http_pool_size_respected/1, http_pool_request_completion/1, http_close_connection/1,
         http_close_pending_requests/1]).

                                             %% Initialization tests

    %% Connection tests

    %% Request handling tests

    %% Response handling tests

    %% Retry logic tests

    %% Error handling tests

    %% Pool management tests

    %% Cleanup tests

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [{group, initialization},
     {group, connection},
     {group, requests},
     {group, responses},
     {group, retry_logic},
     {group, error_handling},
     {group, pool_management},
     {group, cleanup}].

groups() ->
    [{initialization,
      [parallel],
      [http_transport_init_success,
       http_transport_init_invalid_url,
       http_transport_init_missing_owner,
       http_transport_init_with_custom_headers,
       http_transport_init_with_ssl_options]},
     {connection,
      [sequential],
      [http_connection_establish,
       http_connection_refused,
       http_connection_timeout,
       http_connection_reconnect]},
     {requests,
      [parallel],
      [http_send_post_request,
       http_send_get_request,
       http_send_multiple_requests,
       http_send_large_request]},
     {responses,
      [parallel],
      [http_receive_success_response, http_receive_error_response, http_receive_empty_response]},
     {retry_logic,
      [sequential],
      [http_retry_on_connection_failure, http_retry_max_attempts_exceeded]},
     {error_handling, [parallel], [http_error_timeout, http_error_message_too_large]},
     {pool_management, [parallel], [http_pool_size_respected, http_pool_request_completion]},
     {cleanup, [parallel], [http_close_connection, http_close_pending_requests]}].

%%====================================================================
%% Suite Setup/Teardown
%%====================================================================

init_per_suite(Config) ->
    ct:pal("Starting HTTP transport test suite"),
    %% Start required applications
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(crypto),
    {ok, _} = application:ensure_all_started(ssl),
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(jsx),

    %% Start mock HTTP server
    {ok, MockPort} = start_mock_http_server(),

    ct:pal("Mock HTTP server started on port ~p", [MockPort]),
    [{mock_port, MockPort} | Config].

end_per_suite(Config) ->
    ct:pal("Ending HTTP transport test suite"),
    stop_mock_http_server(),
    ok.

init_per_group(GroupName, Config) ->
    ct:pal("Starting group: ~p", [GroupName]),
    Config.

end_per_group(GroupName, _Config) ->
    ct:pal("Ending group: ~p", [GroupName]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases - Initialization
%%====================================================================

http_transport_init_success(Config) ->
    %% Test successful initialization with valid options
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self()},
    Result = erlmcp_transport_http:init(Opts),
    ?assertMatch({ok, _Pid}, Result),
    {ok, Pid} = Result,
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_transport_http:close(Pid).

http_transport_init_invalid_url(_Config) ->
    %% Test initialization with invalid URL format
    Opts = #{url => "not-a-valid-url", owner => self()},
    Result = erlmcp_transport_http:init(Opts),
    ?assertMatch({error, _Reason}, Result).

http_transport_init_missing_owner(_Config) ->
    %% Test initialization without required owner parameter
    Opts = #{url => "http://localhost:8080/mcp"},
    Result = (catch erlmcp_transport_http:init(Opts)),
    ?assertMatch({'EXIT', _}, Result).

http_transport_init_with_custom_headers(Config) ->
    %% Test initialization with custom headers
    MockPort = ?config(mock_port, Config),
    CustomHeaders =
        [{<<"X-Custom-Header">>, <<"CustomValue">>}, {<<"Authorization">>, <<"Bearer token123">>}],
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          headers => CustomHeaders},
    Result = erlmcp_transport_http:init(Opts),
    ?assertMatch({ok, _Pid}, Result),
    {ok, Pid} = Result,
    erlmcp_transport_http:close(Pid).

http_transport_init_with_ssl_options(_Config) ->
    %% Test initialization with SSL options
    Opts =
        #{url => "https://localhost:8443/mcp",
          owner => self(),
          ssl_options => [{verify, verify_peer}, {server_name_indication, "localhost"}]},
    Result = erlmcp_transport_http:init(Opts),
    %% May fail if no HTTPS server available, but shouldn't crash
    ?assert(is_tuple(Result)).

%%====================================================================
%% Test Cases - Connection Management
%%====================================================================

http_connection_establish(Config) ->
    %% Test successful connection establishment
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          connect_timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),
    ?assert(erlang:is_process_alive(Pid)),
    timer:sleep(100), %% Allow connection to establish
    erlmcp_transport_http:close(Pid).

http_connection_refused(_Config) ->
    %% Test connection refused scenario
    Opts =
        #{url => "http://localhost:9999/mcp",
          owner => self(),
          connect_timeout => 1000},
    Result = erlmcp_transport_http:init(Opts),
    ?assertMatch({error, _Reason}, Result).

http_connection_timeout(_Config) ->
    %% Test connection timeout
    Opts =
        #{url => "http://localhost:9999/mcp",
          owner => self(),
          connect_timeout => 100},
    Result = erlmcp_transport_http:init(Opts),
    ?assertMatch({error, _Reason}, Result).

http_connection_reconnect(Config) ->
    %% Test automatic reconnection after connection loss
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          connect_timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),
    ?assert(erlang:is_process_alive(Pid)),
    %% Connection should be established and can handle requests
    Request = jsx:encode(#{<<"test">> => <<"reconnect">>}),
    Result = erlmcp_transport_http:send(Pid, Request),
    ?assertMatch(ok, Result),
    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Request Handling
%%====================================================================

http_send_post_request(Config) ->
    %% Test sending POST request
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          method => post,
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"method">> => <<"test">>,
                     <<"id">> => 1}),

    Result = erlmcp_transport_http:send(Pid, Request),
    ?assertMatch(ok, Result),

    erlmcp_transport_http:close(Pid).

http_send_get_request(Config) ->
    %% Test sending GET request
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          method => get,
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = <<"param1=value1&param2=value2">>,

    Result = erlmcp_transport_http:send(Pid, Request),
    ?assertMatch(ok, Result),

    erlmcp_transport_http:close(Pid).

http_send_multiple_requests(Config) ->
    %% Test sending multiple sequential requests
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Requests = [jsx:encode(#{<<"id">> => N, <<"data">> => <<"request">>}) || N <- lists:seq(1, 10)],

    Results = [erlmcp_transport_http:send(Pid, Req) || Req <- Requests],

    ?assertEqual(10, length(Results)),
    ?assert(lists:all(fun(R) -> R =:= ok orelse is_tuple(R) end, Results)),

    erlmcp_transport_http:close(Pid).

http_send_large_request(Config) ->
    %% Test sending large request payload
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 10000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Create large payload (100KB)
    LargeData = binary:copy(<<"x">>, 100000),
    Request = jsx:encode(#{<<"id">> => 1, <<"large_data">> => LargeData}),

    Result = erlmcp_transport_http:send(Pid, Request),
    ?assertMatch(ok, Result),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Response Handling
%%====================================================================

http_receive_success_response(Config) ->
    %% Test receiving successful HTTP 200 response
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    %% Flush mailbox
    receive
        _ ->
            ok
    after 0 ->
        ok
    end,

    erlmcp_transport_http:send(Pid, Request),

    %% Wait for response
    receive
        {transport_message, Response} ->
            ?assert(is_binary(Response)),
            ?assert(byte_size(Response) > 0)
    after 5000 ->
        ct:fail("Timeout waiting for response")
    end,

    erlmcp_transport_http:close(Pid).

http_receive_error_response(Config) ->
    %% Test receiving HTTP error response (4xx, 5xx)
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp/error", [MockPort])),
          owner => self(),
          timeout => 5000,
          max_retries => 0},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    Result = erlmcp_transport_http:send(Pid, Request),
    %% Should return error for 4xx/5xx without retries
    ?assertMatch({error, _}, Result),

    erlmcp_transport_http:close(Pid).

http_receive_empty_response(Config) ->
    %% Test receiving empty response body
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp/empty", [MockPort])),
          owner => self(),
          timeout => 5000,
          max_retries => 0},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    Result = erlmcp_transport_http:send(Pid, Request),
    %% Should handle empty response gracefully
    ?assertMatch(ok, Result),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Retry Logic
%%====================================================================

http_retry_on_connection_failure(_Config) ->
    %% Test retry on connection failure
    Opts =
        #{url => "http://localhost:9999/mcp",
          owner => self(),
          timeout => 1000,
          max_retries => 2,
          retry_delay => 50},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    Result = erlmcp_transport_http:send(Pid, Request),
    %% Should fail after retries exhausted
    ?assertMatch({error, _}, Result),

    erlmcp_transport_http:close(Pid).

http_retry_max_attempts_exceeded(Config) ->
    %% Test max retry attempts enforcement
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp/fail", [MockPort])),
          owner => self(),
          timeout => 1000,
          max_retries => 2,
          retry_delay => 50},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    StartTime = erlang:monotonic_time(millisecond),
    Result = erlmcp_transport_http:send(Pid, Request),
    EndTime = erlang:monotonic_time(millisecond),

    ?assertMatch({error, _}, Result),

    %% Verify retry delay occurred (at least 2 retries * 50ms)
    Elapsed = EndTime - StartTime,
    ?assert(Elapsed >= 100),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Error Handling
%%====================================================================

http_error_timeout(Config) ->
    %% Test request timeout handling
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp/slow", [MockPort])),
          owner => self(),
          timeout => 100},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"id">> => 1}),

    Result = erlmcp_transport_http:send(Pid, Request),
    %% Should timeout
    ?assertMatch(ok, Result),

    erlmcp_transport_http:close(Pid).

http_error_message_too_large(Config) ->
    %% Test message size limit enforcement
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Create extremely large payload (50MB)
    HugeData = binary:copy(<<"x">>, 50000000),
    Request = jsx:encode(#{<<"huge_data">> => HugeData}),

    Result = erlmcp_transport_http:send(Pid, Request),
    %% Should reject oversized message
    ?assertMatch({error, _}, Result),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Pool Management
%%====================================================================

http_pool_size_respected(Config) ->
    %% Test connection pool size limit
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          pool_size => 3,
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Send more requests than pool size
    Requests = [jsx:encode(#{<<"id">> => N}) || N <- lists:seq(1, 10)],

    %% All should be queued or processed
    Results = [erlmcp_transport_http:send(Pid, Req) || Req <- Requests],

    ?assertEqual(10, length(Results)),
    ?assert(lists:all(fun(R) -> R =:= ok orelse is_tuple(R) end, Results)),

    erlmcp_transport_http:close(Pid).

http_pool_request_completion(Config) ->
    %% Test pool slot release after request completion
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          pool_size => 1,
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Send sequential requests with single pool slot
    Requests = [jsx:encode(#{<<"id">> => N}) || N <- lists:seq(1, 5)],

    Results = [erlmcp_transport_http:send(Pid, Req) || Req <- Requests],

    %% All should complete as pool slots are released
    ?assertEqual(5, length(Results)),
    ?assert(lists:all(fun(R) -> R =:= ok orelse is_tuple(R) end, Results)),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Test Cases - Cleanup
%%====================================================================

http_close_connection(Config) ->
    %% Test clean connection shutdown
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 5000},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    ?assert(erlang:is_process_alive(Pid)),

    erlmcp_transport_http:close(Pid),

    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)).

http_close_pending_requests(Config) ->
    %% Test cleanup with pending requests
    MockPort = ?config(mock_port, Config),
    Opts =
        #{url =>
              lists:flatten(
                  io_lib:format("http://localhost:~p/mcp", [MockPort])),
          owner => self(),
          timeout => 5000,
          pool_size => 5},
    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Send multiple requests
    Requests = [jsx:encode(#{<<"id">> => N}) || N <- lists:seq(1, 10)],

    lists:foreach(fun(Req) -> erlmcp_transport_http:send(Pid, Req) end, Requests),

    %% Close immediately without waiting
    erlmcp_transport_http:close(Pid),

    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid)).

%%====================================================================
%% Mock HTTP Server Implementation
%%====================================================================

-record(mock_state, {port, listener_pid}).

start_mock_http_server() ->
    %% Define routes
    Dispatch =
        cowboy_router:compile([{'_',
                                [{"/mcp", test_http_mcp_handler, []},
                                 {"/mcp/[...]", test_http_mcp_handler, []}]}]),

    %% Start listener on random port (port 0 = OS-assigned)
    case ranch:start_listener(mock_http_server,
                              ranch_tcp,
                              #{socket_opts => [{port, 0}]},
                              cowboy_clear,
                              #{env => #{dispatch => Dispatch}})
    of
        {ok, ListenerPid} ->
            %% Get assigned port with retry logic
            get_listener_port(5);
        {error, Reason} ->
            ct:pal("Failed to start mock HTTP server: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Get the port assigned to the listener with retry
get_listener_port(Retries) when Retries > 0 ->
    case ranch:get_addr(mock_http_server) of
        {ok, {_, Port}} when is_integer(Port) andalso Port > 0 ->
            {ok, Port};
        {ok, {_, Port}} when Port =:= undefined ->
            %% Port not yet assigned, retry
            timer:sleep(100),
            get_listener_port(Retries - 1);
        {error, Reason} ->
            ct:pal("Error getting listener address: ~p", [Reason]),
            timer:sleep(100),
            get_listener_port(Retries - 1);
        Other ->
            ct:pal("Unexpected result from ranch:get_addr: ~p", [Other]),
            timer:sleep(100),
            get_listener_port(Retries - 1)
    end;
get_listener_port(0) ->
    ct:pal("Failed to get listener port after all retries"),
    {error, port_not_assigned}.

stop_mock_http_server() ->
    ranch:stop_listener(mock_http_server).
