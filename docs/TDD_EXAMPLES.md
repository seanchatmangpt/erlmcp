# TDD Examples and Patterns for erlmcp

## Practical Examples of Chicago School TDD Implementation

This document provides concrete examples of TDD patterns used in the erlmcp project, demonstrating Chicago School principles in action.

## Table of Contents

1. [Server Testing Patterns](#server-testing-patterns)
2. [Registry Testing Patterns](#registry-testing-patterns)
3. [Transport Testing Patterns](#transport-testing-patterns)
4. [Client-Server Integration Testing](#client-server-integration-testing)
5. [Property-Based Testing Examples](#property-based-testing-examples)
6. [Stress Testing Examples](#stress-testing-examples)
7. [Test Utilities and Helpers](#test-utilities-and-helpers)

---

## Server Testing Patterns

### Example 1: Resource Management Testing

```erlang
%%% File: apps/erlmcp_core/test/erlmcp_resource_tests.erl
-module(erlmcp_resource_tests).
-include_lib("eunit/include/eunit.hrl").

%% Chicago School TDD: Real processes, state verification
resource_lifecycle_test_() ->
    {setup,
     fun() ->
         %% Start real application dependencies
         application:ensure_all_started(erlmcp_core),
         %% Create real server instance
         {ok, Server} = erlmcp_server:start_link(<<"resource_test_server">>,
                                               #mcp_server_capabilities{
                                                   resources = #mcp_capability{enabled = true}
                                               }),
         Server
     end,
     fun(Server) ->
         %% Stop real server
         erlmcp_server:stop(Server),
         %% Clean up any remaining processes
         application:stop(erlmcp_core)
     end,
     fun(Server) ->
         [
          ?_test(add_and_retrieve_resource(Server)),
          ?_test(update_resource_content(Server)),
          ?_test(delete_resource_cleanup(Server)),
          ?_test(concurrent_resource_access(Server))
         ]
     end
    }.

%% Test observable state, not internal implementation
add_and_retrieve_resource(Server) ->
    %% Arrange: Prepare test data
    Uri = <<"test://resource/add_test">>,
    ExpectedContent = <<"Initial resource content">>,
    Handler = fun(_) -> ExpectedContent end,

    %% Act: Exercise API
    ok = erlmcp_server:add_resource(Server, Uri, Handler),

    %% Assert: Verify observable state
    ?assertEqual({ok, ExpectedContent}, erlmcp_server:get_resource(Server, Uri)).

update_resource_content(Server) ->
    %% Arrange: Create resource first
    Uri = <<"test://resource/update_test">>,
    InitialContent = <<"Initial content">>,
    UpdatedContent = <<"Updated content">>,

    Handler = fun(Content) ->
        case Content of
            initial -> InitialContent;
            updated -> UpdatedContent
        end
    end,

    %% Act: Add and update
    ok = erlmcp_server:add_resource(Server, Uri, Handler),
    ok = erlmcp_server:notify_resource_updated(Server, Uri, #{version => 2}),

    %% Assert: Verify update
    ?assertEqual({ok, UpdatedContent}, erlmcp_server:get_resource(Server, Uri)).

delete_resource_cleanup(Server) ->
    %% Arrange: Create resource to delete
    Uri = <<"test://resource/delete_test">>,
    Handler = fun(_) -> <<"content">> end,
    ok = erlmcp_server:add_resource(Server, Uri, Handler),

    %% Act: Delete resource
    ok = erlmcp_server:delete_resource(Server, Uri),

    %% Assert: Verify cleanup (observable behavior)
    ?assertEqual({error, not_found}, erlmcp_server:get_resource(Server, Uri)).

%% Test concurrent access with real processes
concurrent_resource_access(Server) ->
    %% Arrange: Create resource
    Uri = <<"test://resource/concurrent">>,
    Handler = fun(_) -> <<"shared_content">> end,
    ok = erlmcp_server:add_resource(Server, Uri, Handler),

    %% Act: Spawn multiple real client processes
    ClientCount = 10,
    ClientPids = lists:map(fun(Id) ->
        spawn_link(fun() ->
            %% Each client performs operations
            lists:foreach(fun(_) ->
                ?assertEqual({ok, <<"shared_content">>},
                           erlmcp_server:get_resource(Server, Uri)),
                timer:sleep(10)  % Small delay to test concurrency
            end, lists:seq(1, 5))
        end)
    end, lists:seq(1, ClientCount)),

    %% Wait for all clients to complete
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok end
    end, ClientPids),

    %% Assert: Verify server stability after load
    ?assertEqual({ok, <<"shared_content">>}, erlmcp_server:get_resource(Server, Uri)).
```

### Example 2: Tool Management Testing

```erlang
%% Tool testing with real process behavior
tool_execution_test_() ->
    {spawn,
     fun() ->
         Server = start_test_server(),

         %% Add tool with predictable behavior
         ok = erlmcp_server:add_tool(Server, <<"echo">>,
                                    fun(Args) -> Args end),

         %% Execute tool and verify result
         Args = #{<<"message">> => <<"hello">>, <<"count">> => 1},
         ?assertEqual({ok, Args},
                     erlmcp_server:call_tool(Server, <<"echo">>, Args)),

         %% Test error handling
         ?assertEqual({error, not_found},
                     erlmcp_server:call_tool(Server, <<"nonexistent">>, #{})),

         ok = erlmcp_server:stop(Server)
     end
    }.

%% Test tool validation and schema handling
tool_validation_test_() ->
    {spawn,
     fun() ->
         Server = start_test_server(),

         %% Add tool with schema validation
         Schema = #{
             type => <<"object">>,
             properties => #{
                 required_field => #{type => <<"string">>},
                 optional_field => #{type => <<"number">>}
             },
             required => [<<"required_field">>]
         },

         Handler = fun(Args) ->
             case maps:get(<<"required_field">>, Args) of
                 undefined -> {error, missing_required_field};
                 _ -> {ok, Args}
             end
         end,

         ok = erlmcp_server:add_tool_with_schema(Server, <<"validated_tool">>,
                                             Handler, Schema),

         %% Test valid input
         ValidArgs = #{<<"required_field">> => <<"test">>,
                     <<"optional_field">> => 42},
         ?assertEqual({ok, ValidArgs},
                     erlmcp_server:call_tool(Server, <<"validated_tool">>, ValidArgs)),

         %% Test invalid input (missing required)
         InvalidArgs = #{<<"optional_field">> => 42},
         ?assertEqual({error, missing_required_field},
                     erlmcp_server:call_tool(Server, <<"validated_tool">>, InvalidArgs)),

         ok = erlmcp_server:stop(Server)
     end
    }.
```

---

## Registry Testing Patterns

### Example 1: Server Registration Testing

```erlang
%%% File: apps/erlmcp_core/test/erlmcp_registry_tests.erl
-module(erlmcp_registry_tests).
-include_lib("eunit/include/eunit.hrl").

%% Chicago School: Test registry coordination with real processes
server_registration_test_() ->
    {setup,
     fun() ->
         %% Start real registry
         application:ensure_all_started(gproc),
         %% Create multiple real servers
         {ok, Server1} = erlmcp_server:start_link(<<"reg_test_1">>, Caps),
         {ok, Server2} = erlmcp_server:start_link(<<"reg_test_2">>, Caps),
         [Server1, Server2]
     end,
     fun(Servers) ->
         %% Stop real servers
         [erlmcp_server:stop(Pid) || Pid <- Servers],
         application:stop(gproc)
     end,
     fun(Servers) ->
         [
          ?_test(register_server_uniqueness(Servers)),
          ?_test(server_lookup_operations(Servers)),
          ?_test(server_failure_recovery(Servers))
         ]
     end
    }.

register_server_uniqueness([Server1, Server2]) ->
    %% Test that each server has unique registration
    {ok, {Pid1, _}} = erlmcp_registry:find_server(<<"reg_test_1">>),
    {ok, {Pid2, _}} = erlmcp_registry:find_server(<<"reg_test_2">>),

    %% Verify PIDs match what we started
    ?assertEqual(Server1, Pid1),
    ?assertEqual(Server2, Pid2),
    ?assertNotEqual(Pid1, Pid2).

server_lookup_operations([_]) ->
    %% Test registry lookup operations
    ?assertEqual(ok, erlmcp_registry:register_server(<<"new_server">>, self())),

    {ok, {Self, Config}} = erlmcp_registry:find_server(<<"new_server">>),
    ?assertEqual(self(), Self),
    ?assert(is_map(Config)).

server_failure_recovery([FirstServer | _]) ->
    %% Test graceful handling when server dies
    ?assertEqual({ok, {FirstServer, _}}, erlmcp_registry:find_server(<<"reg_test_1">>)),

    %% Kill server process
    exit(FirstServer, kill),
    timer:sleep(100),  % Allow cleanup

    %% Should return error, not crash
    ?assertEqual({error, not_found}, erlmcp_registry:find_server(<<"reg_test_1">>>)).
```

### Example 2: Transport Registry Testing

```erlang
%% Transport coordination testing
transport_registry_test_() ->
    {spawn,
     fun() ->
         %% Start real registry and test transports
         application:ensure_all_started(erlmcp_core),

         %% Create multiple real transports
         {ok, Transport1} = erlmcp_transport:start_link(<<"trans_test_1">>, stdio,
                                                       #{server_id => <<"server_1">>}),
         {ok, Transport2} = erlmcp_transport:start_link(<<"trans_test_2">>, stdio,
                                                       #{server_id => <<"server_2">>}),

         %% Test transport registration and coordination
         Transports = erlmcp_registry:list_transports(),
         TransportIds = [Id || {Id, _} <- Transports],

         ?assert(lists:member(<<"trans_test_1">>, TransportIds)),
         ?assert(lists:member(<<"trans_test_2">>, TransportIds)),

         %% Test transport-server binding
         ok = erlmcp_registry:bind_transport_to_server(<<"trans_test_1">>, <<"server_1">>),
         ok = erlmcp_registry:bind_transport_to_server(<<"trans_test_2">>, <<"server_2">>),

         %% Verify bindings
         Bindings = erlmcp_registry:get_transport_bindings(),
         ?assert(lists:member({<<"trans_test_1">>, <<"server_1">>}, Bindings)),
         ?assert(lists:member({<<"trans_test_2">>, <<"server_2">>}, Bindings)),

         %% Cleanup
         [erlmcp_transport:stop(Pid) || Pid <- [Transport1, Transport2]],
         application:stop(erlmcp_core)
     end
    }.
```

---

## Transport Testing Patterns

### Example 1: TCP Transport Testing

```erlang
%%% File: apps/erlmcp_transports/test/erlmcp_transport_tcp_tests.erl
-module(erlmcp_transport_tcp_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test real TCP transport behavior
tcp_transport_test_() ->
    {spawn,
     fun() ->
         %% Test with real TCP server
         {ok, ServerSocket} = gen_tcp:listen(0, [binary, {active, false}]),
         {ok, Port} = inet:port(ServerSocket),

         %% Spawn real TCP client
         ClientPid = spawn_link(fun() -> tcp_client_worker(ServerSocket, Port) end),

         %% Wait for connection
         receive
             {connected, Pid} ->
                 ?assertEqual(ClientPid, Pid),
                 %% Test message passing
                 Pid ! {send, <<"test message">>},
                 receive
                     {received, <<"test message">>} -> ok
                 after 1000 ->
                     ct:fail("Message not received")
                 end
         after 5000 ->
             ct:fail("Client connection timeout")
         end,

         %% Cleanup
         gen_tcp:close(ServerSocket),
         exit(ClientPid, normal)
     end
    }.

tcp_client_worker(ServerSocket, Port) ->
    {ok, Socket} = gen_tcp:connect("localhost", Port, [binary, {active, true}]),
    gen_tcp:send(Socket, <<"connect">>),

    %% Handle messages
    receive
        {send, Message} ->
            gen_tcp:send(Socket, Message),
            sender() ! {received, Message}
    end,

    gen_tcp:close(Socket).
```

### Example 2: HTTP Transport Testing

```erlang
%% HTTP transport with real HTTP server
http_transport_test_() ->
    {spawn,
     fun() ->
         %% Start mock HTTP server
         MockServer = spawn_link(fun() -> http_mock_server(8080) end),
         timer:sleep(100),  % Let server start

         %% Test real HTTP client
         {ok, Transport} = erlmcp_transport_http:start_link(<<"http_test">>,
                                                           #{url => "http://localhost:8080/test"}),

         %% Send HTTP request through transport
         Request = #{
             method => <<"POST">>,
             path => <<"/api">>,
             headers => #{<<"content-type">> => <<"application/json">>},
             body => jsx:encode(#{test => <<"data">>})
         },

         ok = erlmcp_transport_http:send(Transport, Request),

         %% Wait for response
         receive
             {http_response, Status, Headers, Body} ->
                 ?assertEqual(200, Status),
                 ?assertEqual(<<"mock response">>, Body)
         after 5000 ->
             ct:fail("HTTP response timeout")
         end,

         %% Cleanup
         erlmcp_transport_http:stop(Transport),
         exit(MockServer, normal)
     end
    }.

http_mock_server(Port) ->
    {ok, ServerSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept_loop(ServerSocket).

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_http_request(Socket) end),
    accept_loop(ListenSocket).

handle_http_request(Socket) ->
    {ok, Data} = gen_tcp:recv(Socket, 0),

    %% Parse HTTP request (simplified)
    Response = <<"HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"response\": \"mock response\"}">>,
    gen_tcp:send(Socket, Response),
    gen_tcp:close(Socket).
```

---

## Client-Server Integration Testing

### Example 1: Full MCP Protocol Flow

```erlang
%%% File: apps/erlmcp_core/test/erlmcp_integration_SUITE.erl
mcp_protocol_flow_test(Config) ->
    ct:pal("Testing complete MCP protocol flow"),

    %% Setup real server with all capabilities
    ServerId = make_test_server_id(100),
    {ok, ServerPid} = erlmcp:start_server(ServerId, #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true},
            tools = #mcp_capability{enabled = true},
            prompts = #mcp_capability{enabled = true}
        }
    }),

    %% Add real MCP components
    ok = erlmcp:add_tool(ServerId, <<"calculator">>, fun(Args) ->
        Op = maps:get(<<"operation">>, Args, <<"add">>),
        A = maps:get(<<"a">>, Args, 0),
        B = maps:get(<<"b">>, Args, 0),
        Result = case Op of
            <<"add">> -> A + B;
            <<"multiply">> -> A * B;
            _ -> {error, unknown_operation}
        end,
        #{result => Result}
    end),

    ok = erlmcp:add_resource(ServerId, <<"file://test.txt">>, fun(_Uri) ->
        #{content => <<"Test file content">>, mimeType => <<"text/plain">>}
    end),

    %% Setup real transport
    TransportId = make_test_transport_id(100),
    {ok, TransportPid} = erlmcp:start_transport(TransportId, stdio,
                                            #{server_id => ServerId}),

    %% Execute complete MCP sequence
    Messages = [
        %% 1. Initialize
        #{id => 1, method ?MCP_METHOD_INITIALIZE,
          params => #{protocol => ?MCP_VERSION, capabilities => #{}}},

        %% 2. List tools
        #{id => 2, method ?MCP_METHOD_TOOLS_LIST},

        %% 3. Call tool
        #{id => 3, method ?MCP_METHOD_TOOLS_CALL,
          params => #{name => <<"calculator">>,
                     arguments => #{operation => <<"add">>, a => 10, b => 20}}},

        %% 4. Read resource
        #{id => 4, method ?MCP_METHOD_RESOURCES_READ,
          params => #{uri => <<"file://test.txt">>}}
    ],

    %% Send real messages
    lists:foreach(fun(Msg) ->
        TransportPid ! {message, jsx:encode(Msg)},
        timer:sleep(100)  % Allow processing
    end, Messages),

    %% Verify all operations completed
    timer:sleep(500),
    ?assert(is_process_alive(ServerPid)),
    ?assert(is_process_alive(TransportPid)),

    %% Cleanup
    erlmcp:stop_transport(TransportId),
    erlmcp:stop_server(ServerId).
```

### Example 2: Subscription Integration Test

```erlang
%% Resource subscription with real process coordination
subscription_integration_test(Config) ->
    ct:pal("Testing resource subscription integration"),

    %% Setup server with subscription capability
    ServerId = make_test_server_id(101),
    {ok, Server} = erlmcp:start_server(ServerId, #{
        capabilities => #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true,
                                      features => #{subscribe => true}}
        }
    }),

    %% Add subscribable resource
    Uri = <<"test://sub/resource">>,
    Handler = fun(_Uri) -> #{content => <<"dynamic">>} end,
    ok = erlmcp:add_resource(Server, Uri, Handler),

    %% Setup subscriber process
    Subscriber = spawn_link(fun() -> subscriber_loop() end),

    %% Subscribe to resource updates
    ok = erlmcp:subscribe_resource(Server, Uri, Subscriber),

    %% Trigger resource update
    UpdateMetadata = #{version => 2, timestamp => erlang:system_time()},
    ok = erlmcp:notify_resource_updated(Server, Uri, UpdateMetadata),

    %% Verify subscriber received notification
    receive
        {resource_update, Uri, Metadata} ->
            ?assertEqual(2, maps:get(version, Metadata))
    after 1000 ->
        ct:fail("Subscription notification not received")
    end,

    %% Cleanup
    exit(Subscriber, normal),
    erlmcp:stop_server(Server).

subscriber_loop() ->
    receive
        {resource_update, Uri, Metadata} ->
            %% Forward to test process
            self() ! {received_update, Uri, Metadata},
            subscriber_loop()
    end.
```

---

## Property-Based Testing Examples

### Example 1: Registry Properties

```erlang
%%% File: apps/erlmcp_core/test/erlmcp_registry_property_tests.erl
-module(erlmcp_registry_property_tests).
-include_lib("proper/include/proper.hrl").

%% Property: Server registration is idempotent
prop_registration_idempotent() ->
    ?FORALL(ServerId, atom(),
        begin
            %% Setup: Clean state
            cleanup_registry(),

            %% Exercise: Register same server multiple times
            {ok, Pid} = erlmcp:start_server(ServerId, #{}),
            ok = erlmcp:register_server(ServerId, Pid),
            ok = erlmcp:register_server(ServerId, Pid),  % Duplicate registration

            %% Verify: Should not create multiple entries
            Servers = erlmcp:list_servers(),
            Count = length([Id || {Id, _} <- Servers, Id =:= ServerId]),

            erlmcp:stop_server(ServerId),
            Count =:= 1
        end).

%% Property: Server lookup is consistent
prop_lookup_consistency() ->
    ?FORALL(Servers, list({atom(), pid()}),
        begin
            %% Setup: Register multiple servers
            lists:foreach(fun({Id, Pid}) ->
                ok = erlmcp:register_server(Id, Pid)
            end, Servers),

            %% Exercise: Lookup each server multiple times
            Results = lists:map(fun({Id, _}) ->
                lists:duplicate(10, erlmcp:find_server(Id))
            end, Servers),

            %% Verify: All lookups return consistent results
            Consistent = lists:all(fun(ResultsForId) ->
                lists:all(fun(R) -> R =:= hd(ResultsForId) end, ResultsForId)
            end, Results),

            %% Cleanup
            lists:forEach(fun({Id, _}) -> erlmcp:unregister_server(Id) end, Servers),
            Consistent
        end).

%% Property: Registry handles concurrent registration
prop_concurrent_registration() ->
    ?FORALL(Count, range(1, 100),
        begin
            %% Setup: Clean state
            cleanup_registry(),

            %% Exercise: Concurrent registration
            Pids = lists:map(fun(I) ->
                spawn_link(fun() ->
                    ServerId = list_to_atom("server_" ++ integer_to_list(I)),
                    Pid = spawn(fun() -> server_loop() end),
                    ok = erlmcp:register_server(ServerId, Pid),
                    erlang:unlink(Pid)
                end)
            end, lists:seq(1, Count)),

            %% Wait for completion
            lists:foreach(fun(Pid) ->
                Ref = monitor(process, Pid),
                receive {'DOWN', Ref, process, Pid, _} -> ok end
            end, Pids),

            %% Verify: All servers registered
            RegisteredServers = erlmcp:list_servers(),
            RegisteredCount = length(RegisteredServers),

            %% Cleanup
            lists:forEach(fun({Id, _}) -> erlmcp:unregister_server(Id) end, RegisteredServers),

            %% Should have registered all servers without duplicates
            RegisteredCount =:= Count
        end).

%% Property: Resource roundtrip consistency
prop_resource_roundtrip() ->
    ?FORALL({Uri, Content}, {binary(), binary()},
        begin
            %% Setup: Create server and add resource
            Server = start_test_server(),
            Handler = fun(_) -> Content end,
            ok = erlmcp:add_resource(Server, Uri, Handler),

            %% Exercise: Retrieve resource multiple times
            Results = lists:map(fun(_) ->
                erlmcp:get_resource(Server, Uri)
            end, lists:seq(1, 10)),

            %% Verify: All retrievals are consistent
            Consistent = lists:all(fun(R) -> R =:= {ok, Content} end, Results),

            %% Cleanup
            erlmcp:stop_server(Server),
            Consistent
        end).
```

### Example 2: Transport Properties

```erlang
%% Transport property testing
module(erlmcp_transport_property_tests).

%% Property: Message delivery preserves content
prop_message_delivery_integrity() ->
    ?FORALL(Message, binary(),
        begin
            %% Setup: Create transport pair
            {ok, Sender} = create_test_transport_pair(),

            %% Exercise: Send message
            ok = erlmcp_transport:send(Sender, Message),

            %% Verify: Message content preserved
            Received = wait_for_message(1000),
            Received =:= Message

            %% Cleanup
            erlmcp_transport:stop(Sender)
        end).

%% Property: Transport handles large messages
prop_large_message_handling() ->
    ?FORALL(Size, range(1000, 10000),
        begin
            %% Generate large binary
            LargeMessage = crypto:strong_rand_bytes(Size),

            %% Test transport can handle it
            Transport = start_test_transport(),
            Result = erlmcp_transport:send(Transport, LargeMessage),

            %% Verify no crashes and size preserved
            case Result of
                ok -> true;
                {error, too_large} -> Size =< 8192;  % Max size limit
                _ -> false
            end
        end).

%% Shrinking for error conditions
prop_error_conditions_shrink() ->
    ?SUCHTHAT(Input,
        binary(),
        byte_size(Input) > 10000,  % Large input that might cause errors
        begin
            TestResult = erlmcp_transport:send(Transport, Input),
            % Should shrink to minimal failing case
            case TestResult of
                {error, Reason} -> Reason;
                _ -> ok
            end
        end).
```

---

## Stress Testing Examples

### Example 1: Registry Performance Test

```erlang
%%% File: bench/erlmcp_bench_registry.erl
-module(erlmcp_bench_registry).

-compile([export_all]).

%% High-throughput registry benchmark
run_high_throughput_test() ->
    ct:pal("Running registry high-throughput test"),

    %% Configuration
    Servers = 50,
    OperationsPerServer = 1000,
    Workers = 10,

    %% Start test environment
    application:ensure_all_started(erlmcp_core),

    %% Spawn concurrent workers
    StartTime = erlang:monotonic_time(microsecond),

    WorkerPids = lists:map(fun(WorkerId) ->
        spawn_link(fun() ->
            registry_worker(WorkerId, OperationsPerServer div Workers)
        end)
    end, lists:seq(1, Workers)),

    %% Wait for completion
    lists:foreach(fun(Pid) ->
        Ref = monitor(process, Pid),
        receive {'DOWN', Ref, process, Pid, _} -> ok end
    end, WorkerPids),

    EndTime = erlang:monotonic_time(microsecond),
    Duration = (EndTime - StartTime) div 1000000,  % Convert to seconds
    TotalOps = Servers * OperationsPerServer,
    Throughput = TotalOps / Duration,

    ct:pal("Registry Performance: ~p ops in ~ps = ~p ops/sec",
           [TotalOps, Duration, Throughput]),

    %% Validate performance
    ?assert(Throughput > 1000, "Should handle at least 1000 ops/sec"),

    application:stop(erlmcp_core).

registry_worker(WorkerId, OperationCount) ->
    %% Create test servers
    ServerIds = [list_to_atom("server_" ++ integer_to_list(I))
                 || I <- lists:seq(WorkerId * 10, WorkerId * 10 + 9)],

    %% Register servers
    lists:foreach(fun(ServerId) ->
        ServerPid = spawn(fun() -> server_process() end),
        erlmcp:register_server(ServerId, ServerPid)
    end, ServerIds),

    %% Perform registry operations
    lists:foreach(fun(_) ->
        %% Random operations
        OpType = rand:uniform(3),
        case OpType of
            1 ->  % Registration
                Id = list_to_atom("temp_" ++ integer_to_binary(rand:uniform(1000))),
                Pid = spawn(fun() -> server_process() end),
                erlmcp:register_server(Id, Pid),
                erlmcp:unregister_server(Id);
            2 ->  % Lookup
                ServerId = lists:nth(rand:uniform(10), ServerIds),
                erlmcp:find_server(ServerId);
            3 ->  % List
                erlmcp:list_servers()
        end
    end, lists:seq(1, OperationCount)),

    %% Cleanup
    lists:forEach(fun(Id) -> erlmcp:unregister_server(Id) end, ServerIds).
```

### Example 2: Memory Exhaustion Test

```erlang
%% Memory pressure test
run_memory_exhaustion_test() ->
    ct:pal("Running memory exhaustion test"),

    InitialMemory = erlang:memory(total),
    ct:pal("Initial memory: ~pMB", [InitialMemory div 1024 div 1024]),

    %% Create processes until memory pressure
    Processes = create_memory_heavy_processes(1000),

    PeakMemory = erlang:memory(total),
    ct:pal("Peak memory: ~pMB", [PeakMemory div 1024 div 1024]),

    %% Test system stability under memory pressure
    TestOps = run_memory_operations(Processes),

    %% Cleanup
    [exit(Pid, normal) || Pid <- Processes],
    timer:sleep(1000),  % Allow GC

    FinalMemory = erlang:memory(total),
    ct:pal("Final memory: ~pMB", [FinalMemory div 1024 div 1024]),

    %% Validate memory cleanup
    MemoryRecovery = (PeakMemory - FinalMemory) div 1024 div 1024,
    ct:pal("Memory recovered: ~pMB", [MemoryRecovery]),

    ?assert(MemoryRecovery > 100, "Should recover significant memory").

create_memory_heavy_processes(Count) ->
    lists:map(fun(I) ->
        spawn_link(fun() -> memory_heavy_process(I) end)
    end, lists:seq(1, Count)).

memory_heavy_process(Id) ->
    %% Create large data structures
    LargeData = lists:foldl(fun(_, Acc) ->
        [crypto:strong_rand_bytes(1024) | Acc]
    end, [], lists:seq(1, 100)),

    %% Simulate work
    receive
        {do_work, Count} ->
            lists:foreach(fun(_) ->
                %% Process some data
                lists:map(fun(Bin) -> binary:part(Bin, {0, 100}) end, LargeData),
                timer:sleep(10)
            end, lists:seq(1, Count))
    after 5000 ->
        %% Periodic work
        memory_heavy_process(Id)
    end.

run_memory_operations(Processes) ->
    %% Send work to all processes
    lists:foreach(fun(Pid) ->
        Pid ! {do_work, 10}
    end, Processes),

    %% Monitor memory usage
    monitor_memory_usage(30, Processes, 0).

monitor_memory_usage(0, _, TotalOps) -> TotalOps;
monitor_memory_usage(Count, Processes, TotalOps) ->
    CurrentMemory = erlang:memory(total),
    ct:pal("Memory: ~pMB", [CurrentMemory div 1024 div 1024]),

    %% Continue operations
    lists:foreach(fun(Pid) ->
        Pid ! {do_work, 1}
    end, Processes),

    timer:sleep(1000),
    monitor_memory_usage(Count - 1, Processes, TotalOps + length(Processes)).
```

---

## Test Utilities and Helpers

### Example 1: Test Setup Utilities

```erlang
%%% File: apps/erlmcp_core/test/test_utils.erl
-module(test_utils).
-export([start_test_server/1, start_test_transport/2,
         mock_external_services/0, cleanup_test_state/1]).

%% Start test server with specified capabilities
start_test_server(ServerId) ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    Pid.

%% Start test transport with real configuration
start_test_transport(TransportId, ServerId) ->
    Config = #{server_id => ServerId, test_mode => true},
    {ok, Pid} = erlmcp_transport:start_link(TransportId, stdio, Config),
    Pid.

%% Mock external services (HTTP, file system, etc.)
mock_external_services() ->
    %% Mock HTTP client
    meck:new(httpc, [passthrough]),
    meck:expect(httpc, request, fun(Method, Url, _, _) ->
        case Url of
            "http://test.example.com" ->
                {ok, {{200, "OK"}, [], <<"mock response">>}};
            _ ->
                meck:passthrough([Method, Url, _, _])
        end
    end),

    %% Mock file operations
    meck:new(file, [unstick]),
    meck:expect(file, read_file, fun(Path) ->
        case Path of
            "/test/file.txt" -> {ok, <<"mock file content">>};
            _ -> meck:passthrough([Path])
        end
    end).

%% Clean up test state
cleanup_test_state(ServerPids) ->
    %% Stop all test servers
    lists:foreach(fun(Pid) ->
        erlmcp_server:stop(Pid)
    end, ServerPids),

    %% Clean up any remaining processes
    application:stop(erlmcp_core),

    %% Clean up mocks
    meck:unload(),

    %% Force garbage collection
    garbage_collect().
```

### Example 2: Test Data Generators

```erlang
%%% File: apps/erlmcp_core/test/test_data_generators.erl
-module(test_data_generators).

-export([generate_test_uri/0, generate_test_content/0,
         generate_test_arguments/0, generate_test_config/0]).

%% Generate valid test URIs
generate_test_uri() ->
    Schemes = [<<"file://">>, <<"http://">>, <<"https://">>, <<"test://">>],
    Paths = [<<"/path/to/file.txt">>, <<"/api/resource/123">>, <<"/data">>],

    Scheme = lists:nth(rand:uniform(length(Schemes)), Schemes),
    Path = lists:nth(rand:uniform(length(Paths)), Paths),

    <<Scheme/binary, Path/binary>>.

%% Generate test content of various types
generate_test_content() ->
    ContentType = rand:uniform(4),
    case ContentType of
        1 -> {text, crypto:strong_rand_bytes(100)};
        2 -> {json, jsx:encode(#{key => <<"value">>, number => 42})};
        3 -> {binary, crypto:strong_rand_bytes(1024)};
        4 -> {empty, <<>>}
    end.

%% Generate test tool arguments
generate_test_arguments() ->
    Args = maps:from_list([
        {<<"string_arg">>, generate_string()},
        {<<"number_arg">>, rand:uniform(1000)},
        {<<"boolean_arg">>, rand:uniform(2) =:= 1},
        {<<"list_arg">>, generate_list()},
        {<<"map_arg">>, generate_map()}
    ]),
    Args.

generate_string() ->
    Length = rand:uniform(50),
    list_to_binary([rand:uniform(255) || _ <- lists:seq(1, Length)]).

generate_list() ->
    Length = rand:uniform(10),
    [generate_string() || _ <- lists:seq(1, Length)].

generate_map() ->
    Keys = [<<"key1">>, <<"key2">>, <<"key3">>],
    maps:from_list([{K, generate_string()} || K <- Keys]).

%% Generate test configuration
generate_test_config() ->
    #{
        server_name => generate_string(),
        timeout => rand:uniform(10000),
        max_connections => rand:uniform(1000),
        capabilities => generate_capabilities(),
        transport => generate_transport_config()
    }.

generate_capabilities() ->
    #{
        resources => #{enabled => rand:uniform(2) =:= 1},
        tools => #{enabled => rand:uniform(2) =:= 1},
        prompts => #{enabled => rand:uniform(2) =:= 1}
    }.

generate_transport_config() ->
    TransportType = rand:uniform(3),
    case TransportType of
        1 -> #{type => stdio, buffer_size => rand:uniform(1024)};
        2 -> #{type => tcp, host => <<"localhost">>, port => rand:uniform(9000, 9999)};
        3 -> #{type => http, url => <<"http://localhost:8080">>}
    end.
```

### Example 3: Test Assertion Helpers

```erlang
%%% File: apps/erlmcp_core/test/test_assertions.erl
-module(test_assertions).

-export([assert_server_state/2, assert_message_format/2,
         assert_performance_within_range/3, assert_no_memory_leak/1]).

%% Assert server is in expected state
assert_server_state(ServerPid, ExpectedState) ->
    %% Get server state through API (not internal inspection)
    State = erlmcp_server:get_state(ServerPid),
    ?assertMatch(ExpectedState, State).

%% Assert message follows expected format
assert_message_format(Message, ExpectedType) ->
    case jsx:is_json(Message) of
        true ->
            Json = jsx:decode(Message),
            ?assertMatch(#{<<"jsonrpc">> := ?JSONRPC_VERSION,
                         <<"id">> := _,
                         <<"method">> := ExpectedType}, Json);
        false ->
            ?assert(false, "Message should be valid JSON")
    end.

%% Assert operation performance is within expected range
assert_performance_within_range(Operation, Timeout, MinOps) ->
    StartTime = erlang:monotonic_time(microsecond),

    %% Perform operation multiple times
    lists:foreach(fun(_) ->
        Operation()
    end, lists:seq(1, MinOps)),

    EndTime = erlang:monotonic_time(microsecond),
    Duration = (EndTime - StartTime) div 1000,  % milliseconds

    ?assert(Duration =< Timeout,
            io_lib:format("Operation took ~pms, expected <= ~pms",
                         [Duration, Timeout])).

%% Assert no memory leak after test
assert_no_memory_leak(TestProcess) ->
    %% Get memory before cleanup
    BeforeMemory = erlang:memory(total),

    %% Cleanup test process
    exit(TestProcess, normal),
    timer:sleep(1000),  % Allow cleanup

    %% Get memory after cleanup
    AfterMemory = erlang:memory(total),

    MemoryLeak = BeforeMemory - AfterMemory,
    ?assert(MemoryLeak < 1024 * 1024,  % Less than 1MB leak
             io_lib:format("Memory leak detected: ~p bytes", [MemoryLeak])).
```

## Conclusion

These examples demonstrate the Chicago School TDD approach in practice, focusing on:

1. **Real process collaboration** rather than mocking
2. **Observable state verification** rather than internal inspection
3. **Integration testing** of components working together
4. **Property-based testing** for edge cases and invariants
5. **Performance validation** under realistic load

The patterns shown can be adapted for other Erlang/OTP projects and provide a solid foundation for comprehensive test coverage while maintaining the efficiency and reliability expected from Erlang systems.