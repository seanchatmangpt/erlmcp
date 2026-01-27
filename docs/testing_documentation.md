# Testing Documentation

## Overview

This document provides comprehensive guidance for testing MCP applications and transport implementations using the ErlMCP SDK. The new transport architecture includes built-in testing support and mocking capabilities.

## Testing Architecture

### Test-Friendly Design

The ErlMCP transport architecture is designed with testability in mind:

- **Test Mode Detection**: Automatic detection of test environments
- **Transport Mocking**: Built-in support for simulating transport behavior
- **Isolated Testing**: Each transport can be tested independently
- **Deterministic Behavior**: Predictable behavior in test environments

### Test Environment Detection

The SDK automatically detects test environments using several heuristics:

```erlang
% Automatic test detection
is_test_environment() ->
    case get(test_mode) of
        true -> true;
        _ ->
            case whereis(eunit_proc) of
                undefined -> false;
                _ -> true  % EUnit is running
            end
    end.
```

You can also manually enable test mode:

```erlang
% Manually enable test mode
put(test_mode, true).
```

## Unit Testing

### Testing Server Components

#### Basic Server Test

```erlang
-module(my_server_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    put(test_mode, true),  % Enable test mode
    Capabilities = #{
        resources => #{enabled => true},
        tools => #{enabled => true}
    },
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    Server.

teardown(Server) ->
    ok = erlmcp_server:stop(Server).

server_start_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun(Server) ->
         [
             ?_assert(is_pid(Server)),
             ?_assertEqual({ok, Server}, gen_server:call(Server, ping))
         ]
     end}.
```

#### Testing Resource Handlers

```erlang
resource_handler_test() ->
    put(test_mode, true),
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),
    
    % Add a test resource
    Handler = fun(<<"test://", Path/binary>>) ->
        <<"Content for ", Path/binary>>
    end,
    ok = erlmcp_server:add_resource(Server, <<"test://example">>, Handler),
    
    % Test resource listing
    {ok, Resources} = gen_server:call(Server, list_resources),
    ?assertEqual(1, length(Resources)),
    
    % Test resource reading
    {ok, Content} = gen_server:call(Server, {read_resource, <<"test://example">>}),
    ?assertEqual(<<"Content for example">>, Content),
    
    ok = erlmcp_server:stop(Server).
```

#### Testing Tool Handlers

```erlang
tool_handler_test() ->
    put(test_mode, true),
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),
    
    % Add a test tool with schema
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"input">>]
    },
    Handler = fun(#{<<"input">> := Input}) ->
        <<"Processed: ", Input/binary>>
    end,
    ok = erlmcp_server:add_tool_with_schema(Server, <<"process">>, Handler, Schema),
    
    % Test valid arguments
    Args = #{<<"input">> => <<"test data">>},
    {ok, Result} = gen_server:call(Server, {call_tool, <<"process">>, Args}),
    ?assertEqual(<<"Processed: test data">>, Result),
    
    % Test invalid arguments (missing required field)
    InvalidArgs = #{},
    ?assertMatch({error, {validation_failed, _}}, 
                 gen_server:call(Server, {call_tool, <<"process">>, InvalidArgs})),
    
    ok = erlmcp_server:stop(Server).
```

### Testing Client Components

#### Basic Client Test

```erlang
client_test() ->
    put(test_mode, true),
    
    % Start a mock server first
    {ok, MockServer} = start_mock_server(),
    
    % Start client
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    
    % Test initialization
    ClientInfo = #{name => <<"Test Client">>, version => <<"1.0.0">>},
    {ok, ServerInfo} = erlmcp_client:initialize(Client, ClientInfo),
    ?assertMatch(#{name := _}, ServerInfo),
    
    % Clean up
    ok = erlmcp_client:stop(Client),
    ok = stop_mock_server(MockServer).
```

#### Testing with Message Simulation

```erlang
client_message_simulation_test() ->
    put(test_mode, true),
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    
    % Get transport PID for message simulation
    {ok, Transport} = gen_server:call(Client, get_transport),
    
    % Simulate server response
    ResponseMessage = erlmcp_json_rpc:encode_response(
        1, 
        #{<<"resources">> => [#{<<"uri">> => <<"test://resource">>}]}
    ),
    ok = gen_server:call(Transport, {simulate_input, ResponseMessage}),
    
    % Verify message was processed
    receive
        {mcp_response, _Id, Response} ->
            ?assertMatch(#{<<"resources">> := _}, Response)
    after 1000 ->
        ?assert(false)  % Timeout
    end,
    
    ok = erlmcp_client:stop(Client).
```

## Transport Testing

### STDIO Transport Tests

```erlang
stdio_transport_test() ->
    put(test_mode, true),
    
    % Start STDIO transport in test mode
    {ok, Transport} = erlmcp_transport_stdio:start_link(self()),
    
    % Test sending
    TestMessage = <<"test message">>,
    ok = erlmcp_transport_stdio:send(Transport, TestMessage),
    
    % Test input simulation
    ok = gen_server:call(Transport, {simulate_input, <<"simulated input">>}),
    
    % Verify we received the simulated input
    receive
        {transport_message, <<"simulated input">>} ->
            ok
    after 1000 ->
        ?assert(false)
    end,
    
    % Clean up
    ok = erlmcp_transport_stdio:close(Transport).
```

### TCP Transport Tests

```erlang
tcp_transport_test() ->
    % Start a mock TCP server
    {ok, MockServer} = start_mock_tcp_server(9999),
    
    % Configure TCP transport
    TcpOpts = #{
        host => "localhost",
        port => 9999,
        owner => self(),
        connect_timeout => 1000
    },
    
    % Start transport
    {ok, Transport} = erlmcp_transport_tcp:start_link(TcpOpts),
    
    % Wait for connection
    receive
        {transport_connected, Transport} ->
            ok
    after 2000 ->
        ?assert(false)  % Connection timeout
    end,
    
    % Test sending
    ok = erlmcp_transport_tcp:send(Transport, <<"test data\n">>),
    
    % Verify data was received by mock server
    ?assertEqual(<<"test data">>, get_last_received_data(MockServer)),
    
    % Clean up
    ok = erlmcp_transport_tcp:close(Transport),
    ok = stop_mock_tcp_server(MockServer).
```

### HTTP Transport Tests

```erlang
http_transport_test() ->
    % Start a mock HTTP server using meck or similar
    meck:new(httpc, [unstick, passthrough]),
    meck:expect(httpc, request, 
        fun(post, {_Url, _Headers, "application/json", Body}, _HttpOptions, _Options) ->
            {ok, {{"HTTP/1.1", 200, "OK"}, [], Body}}
        end),
    
    % Configure HTTP transport
    HttpOpts = #{
        url => "http://localhost:8080/mcp",
        owner => self(),
        method => post
    },
    
    % Start transport
    {ok, Transport} = erlmcp_transport_http:start_link(HttpOpts),
    
    % Test sending
    TestData = <<"test request">>,
    ok = gen_server:call(Transport, {send, TestData}),
    
    % Verify HTTP request was made
    ?assert(meck:called(httpc, request, ['_', '_', '_', '_'])),
    
    % Clean up
    meck:unload(httpc),
    gen_server:stop(Transport).
```

## Integration Testing

### End-to-End Server Testing

```erlang
e2e_server_test() ->
    put(test_mode, true),
    
    % Start server with full configuration
    ServerCapabilities = #{
        resources => #{enabled => true},
        tools => #{enabled => true},
        prompts => #{enabled => true}
    },
    {ok, Server} = erlmcp_server:start_link({stdio, []}, ServerCapabilities),
    
    % Add test components
    ok = erlmcp_server:add_resource(Server, <<"config://test">>, 
        fun(_) -> <<"test config data">> end),
    
    ok = erlmcp_server:add_tool(Server, <<"echo">>,
        fun(Args) -> Args end),
    
    ok = erlmcp_server:add_prompt(Server, <<"greeting">>,
        fun(#{<<"name">> := Name}) ->
            [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"Hello ", Name/binary>>}}]
        end),
    
    % Simulate complete MCP session
    {ok, Transport} = gen_server:call(Server, get_transport),
    
    % 1. Initialize
    InitRequest = erlmcp_json_rpc:encode_request(1, <<"initialize">>, #{
        <<"clientInfo">> => #{<<"name">> => <<"Test Client">>, <<"version">> => <<"1.0.0">>},
        <<"protocolVersion">> => <<"2025-06-18">>,
        <<"capabilities">> => #{}
    }),
    ok = gen_server:call(Transport, {simulate_input, InitRequest}),
    
    % 2. List resources
    ListResourcesRequest = erlmcp_json_rpc:encode_request(2, <<"resources/list">>, #{}),
    ok = gen_server:call(Transport, {simulate_input, ListResourcesRequest}),
    
    % 3. Call tool
    CallToolRequest = erlmcp_json_rpc:encode_request(3, <<"tools/call">>, #{
        <<"name">> => <<"echo">>,
        <<"arguments">> => #{<<"message">> => <<"test">>}
    }),
    ok = gen_server:call(Transport, {simulate_input, CallToolRequest}),
    
    % Collect all responses
    Responses = collect_responses(3, []),
    
    % Verify responses
    ?assertEqual(3, length(Responses)),
    
    % Clean up
    ok = erlmcp_server:stop(Server).

collect_responses(0, Acc) ->
    lists:reverse(Acc);
collect_responses(N, Acc) ->
    receive
        {mcp_response, _Id, Response} ->
            collect_responses(N - 1, [Response | Acc])
    after 5000 ->
        ?assert(false)  % Timeout
    end.
```

### Client-Server Integration Test

```erlang
client_server_integration_test() ->
    put(test_mode, true),
    
    % Start server
    {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),
    ok = erlmcp_server:add_resource(Server, <<"test://data">>,
        fun(_) -> <<"integration test data">> end),
    
    % Start client
    {ok, Client} = erlmcp_client:start_link({stdio, []}),
    
    % Set up bidirectional communication
    link_transports(Server, Client),
    
    % Initialize client
    ClientInfo = #{name => <<"Integration Test Client">>, version => <<"1.0.0">>},
    {ok, _ServerInfo} = erlmcp_client:initialize(Client, ClientInfo),
    
    % Test resource operations
    {ok, Resources} = erlmcp_client:list_resources(Client),
    ?assert(length(Resources) > 0),
    
    {ok, Content} = erlmcp_client:read_resource(Client, <<"test://data">>),
    ?assertEqual(<<"integration test data">>, Content),
    
    % Clean up
    ok = erlmcp_client:stop(Client),
    ok = erlmcp_server:stop(Server).
```

## Mock Utilities

### Transport Mock Helper

```erlang
-module(transport_mock).
-export([start/0, stop/1, expect_send/2, verify_send/2]).

start() ->
    spawn_link(fun() -> mock_loop(#{sent => [], expected => []}) end).

stop(MockPid) ->
    MockPid ! stop.

expect_send(MockPid, ExpectedData) ->
    MockPid ! {expect, ExpectedData}.

verify_send(MockPid, ExpectedData) ->
    MockPid ! {verify, ExpectedData, self()},
    receive
        verification_result -> ok
    after 1000 ->
        {error, timeout}
    end.

mock_loop(State) ->
    receive
        {send, Data} ->
            NewSent = [Data | maps:get(sent, State)],
            mock_loop(State#{sent := NewSent});
        {expect, Data} ->
            Expected = maps:get(expected, State),
            mock_loop(State#{expected := [Data | Expected]});
        {verify, Data, From} ->
            Sent = maps:get(sent, State),
            Result = case lists:member(Data, Sent) of
                true -> ok;
                false -> {error, not_sent}
            end,
            From ! verification_result,
            mock_loop(State);
        stop ->
            ok
    end.
```

### Server Mock

```erlang
-module(server_mock).
-export([start/0, add_response/3, stop/1]).

start() ->
    spawn_link(fun() -> server_mock_loop(#{}) end).

add_response(MockPid, Method, Response) ->
    MockPid ! {add_response, Method, Response}.

stop(MockPid) ->
    MockPid ! stop.

server_mock_loop(Responses) ->
    receive
        {add_response, Method, Response} ->
            NewResponses = Responses#{Method => Response},
            server_mock_loop(NewResponses);
        {request, Method, _Params, From} ->
            Response = maps:get(Method, Responses, 
                {error, {method_not_found, Method}}),
            From ! {response, Response},
            server_mock_loop(Responses);
        stop ->
            ok
    end.
```

## Performance Testing

### Load Testing

```erlang
load_test() ->
    NumClients = 100,
    NumRequests = 1000,
    
    % Start server
    {ok, Server} = erlmcp_server:start_link({tcp, #{port => 9999}}, #{}),
    ok = erlmcp_server:add_tool(Server, <<"echo">>,
        fun(Args) -> Args end),
    
    % Start multiple clients
    Clients = start_multiple_clients(NumClients),
    
    % Measure performance
    StartTime = erlang:monotonic_time(millisecond),
    
    % Send concurrent requests
    Pids = lists:map(fun(Client) ->
        spawn(fun() -> send_requests(Client, NumRequests) end)
    end, Clients),
    
    % Wait for completion
    lists:foreach(fun(Pid) ->
        receive
            {Pid, completed} -> ok
        after 30000 ->
            ?assert(false)  % Timeout
        end
    end, Pids),
    
    EndTime = erlang:monotonic_time(millisecond),
    
    % Calculate metrics
    TotalRequests = NumClients * NumRequests,
    Duration = EndTime - StartTime,
    RequestsPerSecond = (TotalRequests * 1000) / Duration,
    
    io:format("Load test completed: ~p requests in ~p ms (~.2f req/sec)~n",
              [TotalRequests, Duration, RequestsPerSecond]),
    
    % Clean up
    lists:foreach(fun(Client) ->
        ok = erlmcp_client:stop(Client)
    end, Clients),
    ok = erlmcp_server:stop(Server).
```

### Memory Leak Testing

```erlang
memory_leak_test() ->
    InitialMemory = erlang:memory(total),
    
    % Run many iterations
    lists:foreach(fun(_) ->
        {ok, Server} = erlmcp_server:start_link({stdio, []}, #{}),
        ok = erlmcp_server:add_resource(Server, <<"test://leak">>,
            fun(_) -> <<"data">> end),
        ok = erlmcp_server:stop(Server)
    end, lists:seq(1, 1000)),
    
    % Force garbage collection
    erlang:garbage_collect(),
    timer:sleep(100),
    
    FinalMemory = erlang:memory(total),
    MemoryIncrease = FinalMemory - InitialMemory,
    
    io:format("Memory increase: ~p bytes~n", [MemoryIncrease]),
    
    % Assert reasonable memory increase (less than 1MB)
    ?assert(MemoryIncrease < 1024 * 1024).
```

## Best Practices

### Test Organization

1. **Separate Unit and Integration Tests**
   - Unit tests in `test/unit/` directory  
   - Integration tests in `test/integration/`
   - End-to-end tests in `test/e2e/`

2. **Use Test Fixtures**
   - Create reusable test setups
   - Common mock utilities
   - Standard test data

3. **Test Coverage**
   - Aim for >80% code coverage
   - Test error conditions
   - Test edge cases

### Test Data Management

```erlang
% test/fixtures/test_data.erl
-module(test_data).
-export([sample_capabilities/0, sample_resources/0, sample_tools/0]).

sample_capabilities() ->
    #{
        resources => #{enabled => true},
        tools => #{enabled => true},
        prompts => #{enabled => false}
    }.

sample_resources() ->
    [
        #{uri => <<"test://config">>, name => <<"Config">>, description => <<"Test config">>},
        #{uri => <<"test://data">>, name => <<"Data">>, description => <<"Test data">>}
    ].

sample_tools() ->
    [
        #{name => <<"echo">>, description => <<"Echo tool">>, 
          input_schema => #{type => <<"object">>}},
        #{name => <<"add">>, description => <<"Add numbers">>,
          input_schema => #{
              type => <<"object">>,
              properties => #{
                  a => #{type => <<"number">>},
                  b => #{type => <<"number">>}
              }
          }}
    ].
```

### Continuous Integration

```yaml
# .github/workflows/test.yml
name: Tests
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        otp: [25, 26, 27]
    steps:
      - uses: actions/checkout@v2
      - uses: erlef/setup-beam@v1
        with:
          otp-version: ${{ matrix.otp }}
          rebar3-version: 3.20.0
      - run: rebar3 compile
      - run: rebar3 eunit
      - run: rebar3 ct
      - run: rebar3 dialyzer
      - run: rebar3 cover
```

This comprehensive testing documentation provides all the tools and patterns needed to thoroughly test MCP applications built with ErlMCP.