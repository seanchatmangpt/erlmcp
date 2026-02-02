%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP/2 Transport Client Tests
%%%
%%% Test coverage for Gun-based HTTP/2 client implementation.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http2_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

%% Tests
-export([all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Connection tests
    http2_client_start_test/1,
    http2_client_connect_test/1,
    http2_client_reconnect_test/1,

    %% Request tests
    http2_get_request_test/1,
    http2_post_request_test/1,
    http2_concurrent_streams_test/1,
    http2_flow_control_test/1,

    %% Metrics tests
    http2_metrics_test/1,
    http2_status_test/1,

    %% Error handling tests
    http2_connection_failure_test/1,
    http2_stream_reset_test/1,
    http2_timeout_test/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        {group, connection_tests},
        {group, request_tests},
        {group, metrics_tests},
        {group, error_tests}
    ].

groups() ->
    [
        {connection_tests, [parallel], [
            http2_client_start_test,
            http2_client_connect_test,
            http2_client_reconnect_test
        ]},
        {request_tests, [sequence], [
            http2_get_request_test,
            http2_post_request_test,
            http2_concurrent_streams_test,
            http2_flow_control_test
        ]},
        {metrics_tests, [parallel], [
            http2_metrics_test,
            http2_status_test
        ]},
        {error_tests, [parallel], [
            http2_connection_failure_test,
            http2_stream_reset_test,
            http2_timeout_test
        ]}
    ].

init_per_suite(Config) ->
    %% Start Gun for testing
    application:ensure_all_started(gun),
    Config.

end_per_suite(_Config) ->
    application:stop(gun),
    ok.

init_per_group(connection_tests, Config) ->
    %% Start test HTTP/2 server (using httpbin or local server)
    Config;
init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases: Connection
%%%===================================================================

http2_client_start_test(_Config) ->
    %% Test starting HTTP/2 client
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Verify process is alive
    ?assert(erlang:is_process_alive(Pid)),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_client_connect_test(_Config) ->
    %% Test HTTP/2 connection establishment
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Get Gun connection
    {ok, GunPid} = erlmcp_transport_http2_client:get_connection(Pid),
    ?assert(erlang:is_process_alive(GunPid)),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_client_reconnect_test(_Config) ->
    %% Test reconnection after Gun process crash
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Get Gun connection and kill it
    {ok, GunPid} = erlmcp_transport_http2_client:get_connection(Pid),
    exit(GunPid, kill),

    %% Wait for reconnection (with timeout)
    timer:sleep(1000),

    %% Verify client is still alive (should have reconnected)
    ?assert(erlang:is_process_alive(Pid)),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

%%%===================================================================
%%% Test Cases: Requests
%%%===================================================================

http2_get_request_test(_Config) ->
    %% Test HTTP/2 GET request
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Make GET request
    {ok, Status, _Headers, Body} =
        erlmcp_transport_http2_client:request(Pid, <<"GET">>, <<"/get">>, [], <<>>),

    ?assertEqual(200, Status),
    ?assert(is_binary(Body)),
    ?assert(byte_size(Body) > 0),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_post_request_test(_Config) ->
    %% Test HTTP/2 POST request
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Make POST request
    JsonBody = <<"{\"test\": \"data\"}">>,
    {ok, Status, _Headers, Body} =
        erlmcp_transport_http2_client:request(Pid, <<"POST">>, <<"/post">>, [], JsonBody),

    ?assertEqual(200, Status),
    ?assert(is_binary(Body)),
    ?assert(byte_size(Body) > 0),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_concurrent_streams_test(_Config) ->
    %% Test HTTP/2 multiplexing (multiple concurrent streams)
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 100
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Spawn 50 concurrent requests
    NumRequests = 50,
    Self = self(),

    RequestFun = fun(N) ->
        spawn_link(fun() ->
            Result = erlmcp_transport_http2_client:request(
                Pid, <<"GET">>, <<"/get">>, [], <<>>),
            Self ! {request_result, N, Result}
        end)
    end,

    lists:foreach(RequestFun, lists:seq(1, NumRequests)),

    %% Collect results
    Results = collect_results(NumRequests, []),
    SuccessCount = length([R || {_, {ok, 200, _, _}} <- Results]),

    ?assertEqual(NumRequests, SuccessCount),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_flow_control_test(_Config) ->
    %% Test HTTP/2 flow control (stream limiting)
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 5
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Get status before requests
    {ok, Status1} = erlmcp_transport_http2_client:get_status(Pid),

    %% Spawn 10 concurrent requests (more than max_concurrent_streams)
    NumRequests = 10,
    Self = self(),

    lists:foreach(fun(_) ->
        spawn_link(fun() ->
            Result = erlmcp_transport_http2_client:request(
                Pid, <<"GET">>, <<"/get">>, [], <<>>),
            Self ! {flow_test_result, Result}
        end)
    end, lists:seq(1, NumRequests)),

    %% Collect results (should all succeed, but some may be queued)
    Results = collect_results(NumRequests, []),
    SuccessCount = length([R || {_, {ok, 200, _, _}} <- Results]),

    ?assertEqual(NumRequests, SuccessCount),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

%%%===================================================================
%%% Test Cases: Metrics
%%%===================================================================

http2_metrics_test(_Config) ->
    %% Test metrics collection
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Make some requests
    lists:foreach(fun(_) ->
        erlmcp_transport_http2_client:request(Pid, <<"GET">>, <<"/get">>, [], <<>>)
    end, lists:seq(1, 5)),

    %% Get metrics
    {ok, Metrics} = erlmcp_transport_http2_client:get_metrics(Pid),

    ?assert(maps:get(total_requests, Metrics) >= 5),
    ?assert(maps:get(successful_responses, Metrics) >= 5),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_status_test(_Config) ->
    %% Test status reporting
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Get status
    {ok, Status} = erlmcp_transport_http2_client:get_status(Pid),

    ?assertMatch(#{host := <<"httpbin.org">>}, Status),
    ?assertMatch(#{port := 443}, Status),
    ?assertMatch(#{transport := ssl}, Status),
    ?assertMatch(#{max_concurrent_streams := 10}, Status),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

%%%===================================================================
%%% Test Cases: Error Handling
%%%===================================================================

http2_connection_failure_test(_Config) ->
    %% Test connection failure handling
    Opts = #{
        host => <<"invalid-host-that-does-not-exist-12345.com">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },

    ?assertMatch({error, _}, erlmcp_transport_http2_client:start_link(Opts)).

http2_stream_reset_test(_Config) ->
    %% Test stream reset handling
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10,
        retry => 0
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Make request to endpoint that resets stream (if available)
    %% For now, just test normal case
    {ok, Status, _, _} =
        erlmcp_transport_http2_client:request(Pid, <<"GET">>, <<"/get">>, [], <<>>),

    ?assertEqual(200, Status),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

http2_timeout_test(_Config) ->
    %% Test request timeout
    Opts = #{
        host => <<"httpbin.org">>,
        port => 443,
        transport => ssl,
        max_concurrent_streams => 10
    },
    {ok, Pid} = erlmcp_transport_http2_client:start_link(Opts),

    %% Make request with very short timeout (should fail)
    Result = erlmcp_transport_http2_client:request(
        Pid, <<"GET">>, <<"/delay/10">>, [], <<>>, 1),

    ?assertMatch({error, _}, Result),

    %% Clean up
    erlmcp_transport_http2_client:close(Pid).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

collect_results(0, Acc) ->
    lists:reverse(Acc);
collect_results(Count, Acc) ->
    receive
        {request_result, N, Result} ->
            collect_results(Count - 1, [{N, Result} | Acc]);
        {flow_test_result, Result} ->
            collect_results(Count - 1, [Result | Acc])
    after 10000 ->
        {timeout, Acc}
    end.
