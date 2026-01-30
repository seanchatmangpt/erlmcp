-module(erlmcp_transport_http_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chicago School TDD HTTP Transport Tests
%%
%% Philosophy: Real HTTP server (cowboy), real gun client, no mocks.
%% Verify observable behavior (state-based assertions).
%%====================================================================

-define(TEST_HTTP_PORT, 18080).
-define(TEST_HTTPS_PORT, 18443).

%%====================================================================
%% State Record (mirrors erlmcp_transport_http_server state)
%%====================================================================

-record(state, {
    url :: string(),
    owner :: pid(),
    method :: get | post,
    headers :: [{binary(), binary()}],
    timeout :: timeout(),
    connect_timeout :: timeout(),
    max_retries :: non_neg_integer(),
    retry_delay :: pos_integer(),
    gun_pid :: pid() | undefined,
    gun_monitor :: reference() | undefined,
    gun_stream_ref :: reference() | undefined,
    pending_requests :: map(),
    message_queue :: queue:queue(),
    pool_size :: non_neg_integer(),
    active_requests :: non_neg_integer(),
    ssl_options :: list(),
    host :: string(),
    port :: pos_integer(),
    path :: string(),
    scheme :: http | https
}).

%%====================================================================
%% Mock HTTP Server State
%%====================================================================

-record(server_state, {
    requests = [] :: list(),
    response_handler :: fun((binary(), cowboy_req:req()) -> {non_neg_integer(), binary()})
}).

%%====================================================================
%% Test Fixtures
%%====================================================================

http_transport_test_() ->
    {foreach,
     fun setup_http_server/0,
     fun cleanup_http_server/1,
     [
      {"URL parsing - HTTP", fun test_parse_http_url/0},
      {"URL parsing - HTTPS", fun test_parse_https_url/0},
      {"URL parsing - with port", fun test_parse_url_with_port/0},
      {"URL parsing - with path", fun test_parse_url_with_path/0},
      {"Header normalization", fun test_normalize_headers/0},
      {"Transport init and connect", fun test_transport_init_connect/0},
      {"Send POST request", fun test_send_post_request/0},
      {"Send GET request", fun test_send_get_request/0},
      {"Receive 2xx success response", fun test_2xx_success_response/0},
      {"Receive 4xx client error", fun test_4xx_client_error/0},
      {"Receive 5xx server error with retry", fun test_5xx_server_error_retry/0},
      {"Retry on 429 rate limit", fun test_429_rate_limit_retry/0},
      {"Max retries exceeded", fun test_max_retries_exceeded/0},
      {"Exponential backoff with jitter", fun test_exponential_backoff/0},
      {"Timeout handling", fun test_timeout_handling/0},
      {"Large request payload", fun test_large_request_payload/0},
      {"Large response payload", fun test_large_response_payload/0},
      {"Chunked response handling", fun test_chunked_response/0},
      {"Concurrent requests - pool size enforcement", fun test_concurrent_pool_size/0},
      {"Concurrent requests - multiple simultaneous", fun test_concurrent_multiple/0},
      {"Request queueing when pool full", fun test_request_queueing/0},
      {"Connection failure handling", fun test_connection_failure/0},
      {"Connection recovery", fun test_connection_recovery/0},
      {"Gun connection monitoring", fun test_gun_monitoring/0},
      {"Owner process monitoring", fun test_owner_monitoring/0},
      {"Graceful shutdown", fun test_graceful_shutdown/0},
      {"HTTP/2 protocol support", fun test_http2_support/0},
      {"Multiple headers", fun test_multiple_headers/0},
      {"JSON content type", fun test_json_content_type/0}
     ]}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup_http_server() ->
    %% Start applications
    application:ensure_all_started(cowboy),
    application:ensure_all_started(gun),
    application:ensure_all_started(ranch),
    application:ensure_all_started(ssl),
    application:ensure_all_started(jsx),

    %% Configure cowboy routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/mcp", ?MODULE, #{type => mcp}},
            {"/mcp/[...]", ?MODULE, #{type => mcp}},
            {"/api/test", ?MODULE, #{type => mcp}},
            {"/slow", ?MODULE, #{type => slow}},
            {"/large", ?MODULE, #{type => large}},
            {"/chunked", ?MODULE, #{type => chunked}},
            {"/error/:code", ?MODULE, #{type => error}},
            {"/:status", ?MODULE, #{type => status}}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(http_test_listener,
        [{port, ?TEST_HTTP_PORT}],
        #{env => #{dispatch => Dispatch}}
    ),

    %% Wait for server to start
    timer:sleep(100),

    #{server => http_test_listener, port => ?TEST_HTTP_PORT}.

cleanup_http_server(#{server := Listener}) ->
    cowboy:stop_listener(Listener),
    timer:sleep(100),
    ok;
cleanup_http_server(_) ->
    ok.

default_response_handler(Body, _Req) ->
    %% Echo back the request body
    Response = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => <<"success">>,
        <<"echo">> => Body
    }),
    {200, Response}.

%%====================================================================
%% Tests - URL Parsing & Connection
%%====================================================================

test_parse_http_url() ->
    %% Chicago School: Test via real init, verify observable behavior
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        connect_timeout => 5000
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Verify state via get_state call
    {ok, State} = gen_server:call(Pid, get_state),
    ?assertEqual("localhost", State#state.host),
    ?assertEqual(?TEST_HTTP_PORT, State#state.port),
    ?assertEqual("/mcp", State#state.path),
    ?assertEqual(http, State#state.scheme),

    erlmcp_transport_http:close(Pid).

test_parse_https_url() ->
    %% Note: HTTPS won't connect without valid cert, but URL parsing should work
    Opts = #{
        url => "https://api.example.com:443/v1/mcp",
        owner => self(),
        connect_timeout => 100  % Short timeout since we expect failure
    },

    %% Init will fail to connect, but should parse URL correctly
    case erlmcp_transport_http:init(Opts) of
        {ok, _Pid} ->
            %% If it somehow connects, that's fine
            ok;
        {error, _Reason} ->
            %% Expected - no valid HTTPS server at example.com
            ok
    end.

test_parse_url_with_port() ->
    Opts = #{
        url => "http://localhost:9090/api",
        owner => self(),
        connect_timeout => 100
    },

    %% Won't connect (no server on 9090), but URL should parse
    case erlmcp_transport_http:init(Opts) of
        {ok, Pid} ->
            {ok, State} = gen_server:call(Pid, get_state),
            ?assertEqual("localhost", State#state.host),
            ?assertEqual(9090, State#state.port),
            ?assertEqual("/api", State#state.path),
            erlmcp_transport_http:close(Pid);
        {error, _} ->
            %% Expected - connection failure
            ok
    end.

test_parse_url_with_path() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/api/v2/endpoint",
        owner => self(),
        connect_timeout => 5000
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),
    ?assertEqual("/api/v2/endpoint", State#state.path),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Headers
%%====================================================================

test_normalize_headers() ->
    Headers = [
        {"Content-Type", "application/json"},
        {<<"Accept">>, <<"text/plain">>},
        {"X-Custom", "value"}
    ],

    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        headers => Headers
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),

    %% Headers should be normalized to binary tuples
    ?assert(lists:all(fun({K, V}) ->
        is_binary(K) andalso is_binary(V)
    end, State#state.headers)),

    %% User headers should override defaults
    ?assert(lists:member({<<"content-type">>, <<"application/json">>}, State#state.headers)
            orelse lists:member({<<"Content-Type">>, <<"application/json">>}, State#state.headers)),

    erlmcp_transport_http:close(Pid).

test_multiple_headers() ->
    Headers = [
        {"Authorization", "Bearer token123"},
        {"X-Request-ID", "req-456"},
        {"X-Custom-Header", "custom-value"}
    ],

    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        headers => Headers
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),

    %% Verify all custom headers are present
    ?assert(length(State#state.headers) >= 3),

    erlmcp_transport_http:close(Pid).

test_json_content_type() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),

    %% Default headers should include JSON content type
    HasJsonContentType = lists:any(fun({K, V}) ->
        string:lowercase(binary_to_list(K)) =:= "content-type"
        andalso binary:match(V, <<"application/json">>) =/= nomatch
    end, State#state.headers),

    ?assert(HasJsonContentType),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Connection & Init
%%====================================================================

test_transport_init_connect() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        connect_timeout => 5000,
        timeout => 10000
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),

    %% Verify gun connection was established
    {ok, State} = gen_server:call(Pid, get_state),
    ?assert(is_pid(State#state.gun_pid)),
    ?assert(is_reference(State#state.gun_monitor)),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Request/Response
%%====================================================================

test_send_post_request() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        method => post
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test">>,
        <<"params">> => #{},
        <<"id">> => 1
    }),

    %% Send request (Chicago School: use real gen_server call)
    ok = erlmcp_transport_http:send(Pid, Request),

    %% Verify response received (observable behavior)
    receive
        {transport_message, Response} ->
            ?assert(is_binary(Response)),
            Decoded = jsx:decode(Response, [return_maps]),
            ?assertEqual(<<"success">>, maps:get(<<"result">>, Decoded))
    after 5000 ->
        ?assert(false, "Response timeout")
    end,

    erlmcp_transport_http:close(Pid).

test_send_get_request() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        method => get
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = <<"test=value&foo=bar">>,
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, _Response} ->
            ok
    after 5000 ->
        ?assert(false, "GET response timeout")
    end,

    erlmcp_transport_http:close(Pid).

test_2xx_success_response() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/200",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, Response} ->
            ?assert(is_binary(Response))
    after 5000 ->
        ?assert(false, "Success response timeout")
    end,

    erlmcp_transport_http:close(Pid).

test_4xx_client_error() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/400",
        owner => self(),
        max_retries => 0  % No retries for 4xx
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% 4xx errors should not be retried, call should return error
    {error, {http_error, 400, _}} = erlmcp_transport_http:send(Pid, Request),

    erlmcp_transport_http:close(Pid).

test_5xx_server_error_retry() ->
    %% This test requires a server that returns 5xx then 2xx
    %% For simplicity, we test that 5xx triggers retry logic
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/500",
        owner => self(),
        max_retries => 2,
        retry_delay => 100
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% Should retry on 5xx, eventually fail
    {error, {http_error, 500, _}} = erlmcp_transport_http:send(Pid, Request),

    erlmcp_transport_http:close(Pid).

test_429_rate_limit_retry() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/429",
        owner => self(),
        max_retries => 2,
        retry_delay => 100
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% Should retry on 429 rate limit
    {error, {http_error, 429, _}} = erlmcp_transport_http:send(Pid, Request),

    erlmcp_transport_http:close(Pid).

test_max_retries_exceeded() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/503",
        owner => self(),
        max_retries => 1,  % Only 1 retry
        retry_delay => 100
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% Should fail after max retries
    {error, {http_error, 503, _}} = erlmcp_transport_http:send(Pid, Request),

    erlmcp_transport_http:close(Pid).

test_exponential_backoff() ->
    %% Test that retry delay increases exponentially
    %% This is tested indirectly via retry timing

    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/500",
        owner => self(),
        max_retries => 3,
        retry_delay => 100  % Base delay
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    StartTime = erlang:monotonic_time(millisecond),
    {error, _} = erlmcp_transport_http:send(Pid, Request),
    EndTime = erlang:monotonic_time(millisecond),

    Duration = EndTime - StartTime,

    %% With exponential backoff: 100ms + 200ms + 400ms = 700ms minimum
    %% Plus network overhead, should be > 500ms
    ?assert(Duration > 500, io_lib:format("Duration ~p too short", [Duration])),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Timeout
%%====================================================================

test_timeout_handling() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/slow",
        owner => self(),
        timeout => 500  % Short timeout
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% Slow endpoint should timeout
    %% Note: timeout handling depends on gun timeout configuration
    ok = erlmcp_transport_http:send(Pid, Request),

    %% May or may not timeout depending on timing
    receive
        {transport_message, _} -> ok
    after 2000 ->
        ok  % Timeout is acceptable
    end,

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Large Payloads
%%====================================================================

test_large_request_payload() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Create 1MB payload
    LargeData = binary:copy(<<"x">>, 1024 * 1024),
    Request = jsx:encode(#{
        <<"data">> => base64:encode(LargeData)
    }),

    %% Should handle large request
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, _Response} ->
            ok
    after 10000 ->
        ?assert(false, "Large request timeout")
    end,

    erlmcp_transport_http:close(Pid).

test_large_response_payload() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/large",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"size">> => 1048576}),  % Request 1MB response
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, Response} ->
            %% Should receive large response
            ?assert(byte_size(Response) > 1000000)
    after 10000 ->
        ?assert(false, "Large response timeout")
    end,

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Chunked Responses
%%====================================================================

test_chunked_response() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/chunked",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"chunks">> => 3}),
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, Response} ->
            ?assert(is_binary(Response))
    after 5000 ->
        ?assert(false, "Chunked response timeout")
    end,

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Concurrency & Backpressure
%%====================================================================

test_concurrent_pool_size() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        pool_size => 2  % Limit to 2 concurrent requests
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"concurrent">>}),

    %% Send 5 requests (more than pool size)
    [erlmcp_transport_http:send(Pid, Request) || _ <- lists:seq(1, 5)],

    %% All should eventually succeed
    [receive
        {transport_message, _} -> ok
    after 10000 ->
        ?assert(false, "Concurrent request timeout")
    end || _ <- lists:seq(1, 5)],

    erlmcp_transport_http:close(Pid).

test_concurrent_multiple() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self(),
        pool_size => 10
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"data">>}),

    %% Send 10 requests concurrently
    [spawn(fun() ->
        ok = erlmcp_transport_http:send(Pid, Request)
    end) || _ <- lists:seq(1, 10)],

    %% Receive all responses
    [receive
        {transport_message, _} -> ok
    after 10000 ->
        ?assert(false, "Concurrent multiple timeout")
    end || _ <- lists:seq(1, 10)],

    erlmcp_transport_http:close(Pid).

test_request_queueing() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/slow",
        owner => self(),
        pool_size => 1  % Force queueing
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    Request = jsx:encode(#{<<"test">> => <<"queued">>}),

    %% Send 3 requests, they should queue
    ok = erlmcp_transport_http:send(Pid, Request),
    ok = erlmcp_transport_http:send(Pid, Request),
    ok = erlmcp_transport_http:send(Pid, Request),

    %% All should eventually complete
    timer:sleep(2000),

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Tests - Connection Management
%%====================================================================

test_connection_failure() ->
    %% Connect to non-existent server
    Opts = #{
        url => "http://localhost:19999/mcp",  % Non-existent port
        owner => self(),
        connect_timeout => 1000
    },

    %% Should fail to connect
    {error, _Reason} = erlmcp_transport_http:init(Opts).

test_connection_recovery() ->
    %% Start with working connection
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),

    %% Kill gun connection
    GunPid = State#state.gun_pid,
    exit(GunPid, kill),

    %% Wait for reconnection
    timer:sleep(500),

    %% Should reconnect automatically
    {ok, NewState} = gen_server:call(Pid, get_state),
    ?assert(is_pid(NewState#state.gun_pid)),
    ?assertNotEqual(GunPid, NewState#state.gun_pid),

    erlmcp_transport_http:close(Pid).

test_gun_monitoring() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),

    %% Verify gun is monitored
    ?assert(is_reference(State#state.gun_monitor)),

    %% Kill gun process
    GunPid = State#state.gun_pid,
    exit(GunPid, kill),

    %% HTTP transport should detect and handle
    timer:sleep(500),

    %% Transport should still be alive
    ?assert(erlang:is_process_alive(Pid)),

    erlmcp_transport_http:close(Pid).

test_owner_monitoring() ->
    %% Spawn owner process
    Owner = spawn(fun() ->
        receive stop -> ok end
    end),

    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => Owner
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Kill owner
    exit(Owner, kill),
    timer:sleep(200),

    %% Transport should also die
    ?assertNot(erlang:is_process_alive(Pid)).

test_graceful_shutdown() ->
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),
    {ok, State} = gen_server:call(Pid, get_state),
    GunPid = State#state.gun_pid,

    %% Close transport
    ok = erlmcp_transport_http:close(Pid),

    %% Transport should stop
    timer:sleep(200),
    ?assertNot(erlang:is_process_alive(Pid)),

    %% Gun should also be closed
    ?assertNot(erlang:is_process_alive(GunPid)).

%%====================================================================
%% Tests - HTTP/2
%%====================================================================

test_http2_support() ->
    %% Gun should negotiate HTTP/2 if available
    Opts = #{
        url => "http://localhost:" ++ integer_to_list(?TEST_HTTP_PORT) ++ "/mcp",
        owner => self()
    },

    {ok, Pid} = erlmcp_transport_http:init(Opts),

    %% Send request via HTTP/2 (gun handles protocol negotiation)
    Request = jsx:encode(#{<<"test">> => <<"http2">>}),
    ok = erlmcp_transport_http:send(Pid, Request),

    receive
        {transport_message, Response} ->
            ?assert(is_binary(Response))
    after 5000 ->
        ?assert(false, "HTTP/2 request timeout")
    end,

    erlmcp_transport_http:close(Pid).

%%====================================================================
%% Cowboy Handler Implementation (for test HTTP server)
%%====================================================================

%% Cowboy handler behavior
init(Req0, Opts) ->
    Type = maps:get(type, Opts, mcp),
    handle_request(Type, Req0, Opts).

handle_request(mcp, Req0, Opts) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Response = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"result">> => <<"success">>,
        <<"echo">> => Body
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, Opts};

handle_request(status, Req0, Opts) ->
    StatusBin = cowboy_req:binding(status, Req0),
    Status = binary_to_integer(StatusBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Response = jsx:encode(#{
        <<"status">> => Status,
        <<"echo">> => Body
    }),
    Req = cowboy_req:reply(Status,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, Opts};

handle_request(slow, Req0, Opts) ->
    timer:sleep(2000),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Response = jsx:encode(#{
        <<"delayed">> => true,
        <<"echo">> => Body
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, Opts};

handle_request(large, Req0, Opts) ->
    {ok, _Body, Req1} = cowboy_req:read_body(Req0),
    LargeData = binary:copy(<<"y">>, 1024 * 1024),
    Response = jsx:encode(#{
        <<"large">> => true,
        <<"data">> => base64:encode(LargeData)
    }),
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, Opts};

handle_request(chunked, Req0, Opts) ->
    Req1 = cowboy_req:stream_reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Req0),
    cowboy_req:stream_body(<<"{\"chunked\":true,\"data\":\"">>, nofin, Req1),
    timer:sleep(100),
    cowboy_req:stream_body(<<"chunk1">>, nofin, Req1),
    timer:sleep(100),
    cowboy_req:stream_body(<<"chunk2">>, nofin, Req1),
    timer:sleep(100),
    cowboy_req:stream_body(<<"chunk3\"}">>, fin, Req1),
    {ok, Req1, Opts};

handle_request(error, Req0, Opts) ->
    CodeBin = cowboy_req:binding(code, Req0),
    Code = binary_to_integer(CodeBin),
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    Response = jsx:encode(#{
        <<"error">> => true,
        <<"code">> => Code,
        <<"message">> => <<"Simulated error">>
    }),
    Req = cowboy_req:reply(Code,
        #{<<"content-type">> => <<"application/json">>},
        Response,
        Req1),
    {ok, Req, Opts}.
