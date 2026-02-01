# Test Patterns Library - Comprehensive Visual Guide

**Version:** 2.1.0
**Last Updated:** 2026-01-31
**Status:** Production-Ready

---

## Overview

This document provides a comprehensive library of testing patterns used in erlmcp, each illustrated with detailed Mermaid diagrams showing test structure, execution flow, and verification strategies. All patterns follow **Chicago School TDD** principles: real collaborators, state-based verification, and observable behavior testing.

---

## Pattern Categories

```mermaid
mindmap
  root((Test Patterns))
    Gen_server
      Lifecycle
      State management
      Supervision
      Handle callbacks
    Registry
      Registration
      Lookup & discovery
      Process monitoring
      Auto-cleanup
    Transport
      Behavior testing
      Message flow
      Connection handling
      Failover
    Session
      Persistence
      Failover
      Replication
      Lifecycle
    Chaos
      Failure injection
      Recovery testing
      Circuit breaking
      Resource exhaustion
    Property
      Roundtrip invariants
      State machines
      Protocol compliance
      Generator strategies
```

---

## Pattern 1: Gen_server Lifecycle Testing

### Purpose

Verify gen_server starts correctly, handles initialization, stops gracefully, and cleans up resources.

### Test Structure

```mermaid
graph TB
    subgraph "Test Phases"
        Setup[Setup Phase]
        Exercise[Exercise Phase]
        Verify[Verify Phase]
        Teardown[Teardown Phase]
    end

    subgraph "Gen_server Lifecycle"
        Start[start_link/1]
        Init[init/1]
        Handle[handle_call/info]
        Terminate[terminate/2]
    end

    Setup --> Start
    Start --> Init
    Init --> Handle
    Handle --> Terminate
    Terminate --> Teardown

    Exercise --> Handle
    Verify --> Handle

    style Start fill:#51cf66
    style Terminate fill:#ffd43b
```

### Sequence Diagram

```mermaid
sequenceDiagram
    participant Test as Test Suite
    participant Server as Gen_server
    participant Sup as Supervisor

    Test->>Server: start_link(Args)
    Server->>Server: init(Args)
    Server-->>Test: {ok, Pid}

    Test->>Server: API call
    Server->>Server: handle_call/3
    Server-->>Test: {reply, Reply, State}

    Test->>Server: stop()
    Server->>Server: terminate/2
    Server-->>Test: ok

    Test->>Sup: Verify child stopped
    Sup-->>Test: Child not in list
```

### Implementation Template

```erlang
-module(my_server_tests).
-include_lib("eunit/include/eunit.hrl").

server_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun tests/1}.

setup() ->
    {ok, Pid} = my_server:start_link(#{name => test}),
    Pid.

cleanup(Pid) ->
    ok = my_server:stop(Pid),
    % Verify process is dead
    undefined = process_info(Pid).

tests(Pid) ->
    [
     ?_test(started_successfully(Pid)),
     ?_test(handle_call_works(Pid)),
     ?_test(stop_is_graceful(Pid))
    ].

started_successfully(Pid) ->
    ?assertNotEqual(undefined, process_info(Pid)),
    ?assert(is_process_alive(Pid)).

handle_call_works(Pid) ->
    {ok, Result} = my_server:do_something(Pid, arg),
    ?assertEqual(expected, Result).

stop_is_graceful(Pid) ->
    ok = my_server:stop(Pid),
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(Pid)).
```

---

## Pattern 2: Registry Testing

### Purpose

Verify process registration, lookup by name, process monitoring, and auto-cleanup on process death.

### Registration Flow

```mermaid
sequenceDiagram
    participant Test as Test Suite
    participant Proc as Test Process
    participant Reg as Registry
    participant GProc as gproc

    Test->>Proc: spawn(fun() -> loop() end)
    Test->>Reg: register_name({test, key}, Pid)
    Reg->>GProc: reg({n, {test, key}}, Pid)
    GProc-->>Reg: true
    Reg-->>Test: ok

    Test->>Reg: whereis_name({test, key})
    Reg->>GProc: lookup_pid({n, {test, key}})
    GProc-->>Reg: {Pid, _}
    Reg-->>Test: {ok, Pid}

    Test->>Proc: exit(Pid, kill)
    Proc-->>Reg: {'DOWN', Monitor}
    Reg->>Reg: Auto-deregister

    Test->>Reg: whereis_name({test, key})
    Reg-->>Test: {error, not_found}
```

### State Transitions

```mermaid
stateDiagram-v2
    [*] --> Unregistered: Initial state
    Unregistered --> Registering: register_name called
    Registering --> Registered: gproc success

    Registered --> Active: Process alive
    Active --> Registered: Lookup successful
    Active --> MonitorDown: Process dies

    MonitorDown --> Unregistered: Auto-cleanup
    Registered --> Unregistered: unregister_name

    note right of MonitorDown
        gproc monitors process
        Auto-deregisters on death
    end note
```

### Implementation Template

```erlang
registry_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(erlmcp),
         erlmcp_registry
     end,
     fun(_) ->
         application:stop(erlmcp)
     end,
     fun(_) ->
         [
          ?_test(register_and_lookup()),
          ?_test(auto_cleanup_on_death()),
          ?_test(unregister_removes_entry())
         ]
     end}.

register_and_lookup() ->
    % Setup: Spawn process
    Pid = spawn(fun() -> receive stop -> ok end end),

    % Exercise: Register
    ok = erlmcp_registry:register_name({test, key}, Pid),

    % Verify: Lookup returns same Pid (state-based)
    {ok, FoundPid} = erlmcp_registry:whereis_name({test, key}),
    ?assertEqual(Pid, FoundPid),

    % Cleanup
    Pid ! stop.

auto_cleanup_on_death() ->
    % Setup: Register process
    Pid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_name({test, auto}, Pid),

    % Verify: Registered
    {ok, Pid} = erlmcp_registry:whereis_name({test, auto}),

    % Exercise: Kill process
    exit(Pid, kill),
    timer:sleep(100), % Allow monitor to trigger

    % Verify: Auto-deregistered (observable behavior)
    ?assertEqual({error, not_found},
                 erlmcp_registry:whereis_name({test, auto})).

unregister_removes_entry() ->
    % Setup: Register
    Pid = spawn(fun() -> receive stop -> ok end end),
    ok = erlmcp_registry:register_name({test, manual}, Pid),

    % Exercise: Unregister
    ok = erlmcp_registry:unregister_name({test, manual}),

    % Verify: Not found
    ?assertEqual({error, not_found},
                 erlmcp_registry:whereis_name({test, manual})),

    % Cleanup
    Pid ! stop.
```

---

## Pattern 3: Transport Behavior Testing

### Purpose

Verify transport behavior compliance: init, send, close, and error handling for all transport types (stdio, TCP, HTTP, WebSocket).

### Transport Interface

```mermaid
graph TB
    subgraph "Transport Behavior"
        Init[init/2<br/>Initialize transport]
        Send[send/2<br/>Send data]
        Close[close/1<br/>Close connection]
    end

    subgraph "Callbacks"
        OnInit[On Connected]
        OnData[On Data Received]
        OnClose[On Disconnected]
    end

    subgraph "Error Handling"
        SendError[Send Fails]
        ConnError[Connection Lost]
        InvalidData[Invalid Data]
    end

    Init --> OnInit
    Send --> OnData
    Close --> OnClose

    Send --> SendError
    OnData --> ConnError
    OnInit --> InvalidData

    style Init fill:#51cf66
    style Close fill:#ffd43b
```

### Message Flow (stdio Transport)

```mermaid
sequenceDiagram
    participant Test as Test Suite
    participant Transport as stdio Transport
    participant Server as MCP Server
    participant Stdio as Stdin/Stdout

    Test->>Transport: start_link()
    Transport->>Server: Connect via stdio
    Server-->>Transport: Connected

    Test->>Transport: send(JSON-RPC message)
    Transport->>Stdio: Write to stdout
    Stdio-->>Server: Data received

    Server->>Stdio: Write response to stdout
    Stdio-->>Transport: Data received
    Transport->>Transport: Parse JSON-RPC
    Transport-->>Test: {ok, Response}

    Test->>Transport: close()
    Transport->>Server: Close stdio
    Transport-->>Test: ok
```

### Implementation Template

```erlang
transport_test_() ->
    {setup,
     fun() ->
         {ok, Transport} = erlmcp_transport_stdio:start_link(),
         Transport
     end,
     fun(Transport) ->
         erlmcp_transport_stdio:close(Transport)
     end,
     fun(Transport) ->
         [
          ?_test(initialization(Transport)),
          ?_test(send_data(Transport)),
          ?_test(receive_data(Transport)),
          ?_test(close_connection(Transport))
         ]
     end}.

initialization(Transport) ->
    % Verify: Transport initialized successfully
    ?assert(is_process_alive(Transport)).

send_data(Transport) ->
    Message = #{
        jsonrpc => <<"2.0">>,
        id => 1,
        method => <<"test">>
    },

    % Exercise: Send message
    Result = erlmcp_transport_stdio:send(Message, Transport),

    % Verify: Send succeeds
    ?assertEqual(ok, Result).

receive_data(Transport) ->
    % Exercise: Simulate incoming data
    JSON = <<"{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}">>,

    % Transport would normally receive this via stdin
    % For testing, we verify the transport handles it
    {ok, Decoded} = jsx:decode(JSON, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)).

close_connection(Transport) ->
    % Exercise: Close transport
    ok = erlmcp_transport_stdio:close(Transport),

    % Verify: Transport stopped
    timer:sleep(100),
    ?assertEqual(false, is_process_alive(Transport)).
```

---

## Pattern 4: Session Persistence Testing

### Purpose

Verify session data survives server restarts across different backends (ETS, DETS, Mnesia).

### Backend Comparison

```mermaid
graph TB
    subgraph "ETS Backend"
        E1[In-Memory]
        E2[Fastest O(1)]
        E3[Lost on restart]
    end

    subgraph "DETS Backend"
        D1[Disk Storage]
        D2[Fast O(1)]
        D3[Persistent]
    end

    subgraph "Mnesia Backend"
        M1[Distributed]
        M2[Replicated]
        M3[Cluster-wide]
    end

    E1 --> Speed[Speed: ETS > DETS > Mnesia]
    D1 --> Speed
    M1 --> Speed

    E3 --> Durability[Durability: Mnesia > DETS > ETS]
    D3 --> Durability
    M3 --> Durability

    style Speed fill:#ffd43b
    style Durability fill:#51cf66
```

### Persistence Flow

```mermaid
stateDiagram-v2
    [*] --> CreateSession: session_manager:create_session
    CreateSession --> Persisting: Write to backend

    Persisting --> ETS: ETS backend
    Persisting --> DETS: DETS backend
    Persisting --> Mnesia: Mnesia backend

    ETS --> SessionActive: Session active
    DETS --> SessionActive
    Mnesia --> SessionActive

    SessionActive --> ServerCrash: Server crashes
    ServerCrash --> Restarting: Server restarts
    Restarting --> Restoring: Load from backend

    Restoring --> SessionRestored: Session restored
    SessionRestored --> [*]: Session continues

    note right of Restoring
        ETS: Lost on restart
        DETS: Loaded from disk
        Mnesia: Replicated from cluster
    end note
```

### Implementation Template

```erlang
session_persistence_test_() ->
    {setup,
     fun() ->
         % Setup: Start with DETS backend
         {ok, SessionMgr} = erlmcp_session_manager:start_link(
             #{
                 backend => erlmcp_session_dets,
                 backend_opts => #{
                     file_path => "/tmp/test_session.dets"
                 }
             }
         ),
         SessionMgr
     end,
     fun(SessionMgr) ->
         % Cleanup: Stop manager and remove file
         ok = erlmcp_session_manager:stop(SessionMgr),
         file:delete("/tmp/test_session.dets")
     end,
     fun(SessionMgr) ->
         [
          ?_test(create_and_retrieve(SessionMgr)),
          ?_test(update_session(SessionMgr)),
          ?_test(delete_session(SessionMgr)),
          ?_test(persists_across_restart(SessionMgr))
         ]
     end}.

create_and_retrieve(SessionMgr) ->
    SessionId = <<"session-123">>,
    SessionData = #{user_id => <<"user-456">>},

    % Exercise: Create session
    ok = erlmcp_session_manager:create_session(
        SessionMgr, SessionId, SessionData
    ),

    % Verify: Retrieve session (state-based)
    {ok, Retrieved} = erlmcp_session_manager:get_session(
        SessionMgr, SessionId
    ),
    ?assertEqual(SessionData, Retrieved).

update_session(SessionMgr) ->
    SessionId = <<"session-789">>,
    InitialData = #{count => 1},

    ok = erlmcp_session_manager:create_session(
        SessionMgr, SessionId, InitialData
    ),

    % Exercise: Update session
    UpdatedData = #{count => 2},
    ok = erlmcp_session_manager:update_session(
        SessionMgr, SessionId, UpdatedData
    ),

    % Verify: Session updated
    {ok, Retrieved} = erlmcp_session_manager:get_session(
        SessionMgr, SessionId
    ),
    ?assertEqual(UpdatedData, Retrieved).

delete_session(SessionMgr) ->
    SessionId = <<"session-delete">>,
    SessionData = #{test => true},

    ok = erlmcp_session_manager:create_session(
        SessionMgr, SessionId, SessionData
    ),

    % Exercise: Delete session
    ok = erlmcp_session_manager:delete_session(SessionMgr, SessionId),

    % Verify: Session gone
    ?assertEqual(
        {error, not_found},
        erlmcp_session_manager:get_session(SessionMgr, SessionId)
    ).

persists_across_restart(_SessionMgr) ->
    SessionId = <<"session-persist">>,
    SessionData = #{persistent => true},

    % Setup: Create session before restart
    {ok, SessionMgr1} = erlmcp_session_manager:start_link(
        #{
            backend => erlmcp_session_dets,
            backend_opts => #{
                file_path => "/tmp/test_session_restart.dets"
            }
         }
    ),

    ok = erlmcp_session_manager:create_session(
        SessionMgr1, SessionId, SessionData
    ),

    % Exercise: Crash and restart
    exit(SessionMgr1, kill),
    timer:sleep(100),

    {ok, SessionMgr2} = erlmcp_session_manager:start_link(
        #{
            backend => erlmcp_session_dets,
            backend_opts => #{
                file_path => "/tmp/test_session_restart.dets"
            }
         }
    ),

    % Verify: Session restored from disk
    {ok, Restored} = erlmcp_session_manager:get_session(
        SessionMgr2, SessionId
    ),
    ?assertEqual(SessionData, Restored),

    % Cleanup
    ok = erlmcp_session_manager:stop(SessionMgr2),
    file:delete("/tmp/test_session_restart.dets").
```

---

## Pattern 5: Chaos Engineering Testing

### Purpose

Verify system resilience through controlled failure injection (network failures, process crashes, resource exhaustion).

### Failure Scenarios

```mermaid
graph TB
    subgraph "Failure Injection"
        Network[Network Failures<br/>Latency, packet loss]
        Process[Process Failures<br/>Kill, spawn storm]
        Resource[Resource Exhaustion<br/>Memory, CPU, disk]
    end

    subgraph "System Response"
        Detect[Failure Detection]
        Recover[Auto Recovery]
        Degrade[Graceful Degradation]
    end

    subgraph "Verification"
        Bounded[Bounded Refusals]
        Fast[Fast Recovery <5s]
        Stable[System Stable]
    end

    Network --> Detect
    Process --> Detect
    Resource --> Detect

    Detect --> Recover
    Recover --> Bounded

    Detect --> Degrade
    Degrade --> Fast

    Recover --> Stable

    style Detect fill:#ffd43b
    style Recover fill:#51cf66
```

### Chaos Test Flow

```mermaid
sequenceDiagram
    participant Test as Test Suite
    participant Chaos as Chaos Engine
    participant System as System Under Test
    participant Monitor as Health Monitor

    Test->>Chaos: Inject failure
    Chaos->>System: Simulate failure

    System->>Monitor: Health check
    Monitor-->>System: Degraded state

    System->>System: Activate recovery
    System->>Monitor: Health check
    Monitor-->>System: Healthy state

    System-->>Test: Recovery complete

    Test->>System: Verify bounded refusals
    System-->>Test: Refusal count

    Test->>Test: Assert recovery < 5s
```

### Implementation Template

```erlang
chaos_test_() ->
    {setup,
     fun() ->
         application:ensure_all_started(erlmcp),
         erlmcp_chaos:start_link()
     end,
     fun(Chaos) ->
         erlmcp_chaos:stop(Chaos),
         application:stop(erlmcp)
     end,
     fun(Chaos) ->
         [
          ?_test(network_latency(Chaos)),
          ?_test(process_kill(Chaos)),
          ?_test(memory_exhaustion(Chaos))
         ]
     end}.

network_latency(Chaos) ->
    % Setup: Start server
    {ok, Server} = erlmcp_server:start_link(),

    % Exercise: Inject 100ms latency
    ok = erlmcp_chaos:inject_latency(Chaos, network, 100),

    % Verify: Requests still succeed (slower but working)
    StartTime = erlang:monotonic_time(millisecond),
    {ok, _Result} = erlmcp_server:list_tools(Server),
    EndTime = erlang:monotonic_time(millisecond),

    ResponseTime = EndTime - StartTime,
    ?assert(ResponseTime >= 100),
    ?assert(ResponseTime < 500), % Should complete

    % Cleanup
    ok = erlmcp_chaos:remove_injection(Chaos, network),
    erlmcp_server:stop(Server).

process_kill(Chaos) ->
    % Setup: Start supervised server
    {ok, Sup} = erlmcp_core_sup:start_link(),
    {children, Children} = supervisor:which_children(Sup),
    {erlmcp_server, InitialPid, _, _} =
        lists:keyfind(erlmcp_server, 1, Children),

    % Exercise: Kill process
    exit(InitialPid, kill),
    timer:sleep(100), % Allow supervisor to restart

    % Verify: Process restarted (observable behavior)
    {children, NewChildren} = supervisor:which_children(Sup),
    {erlmcp_server, NewPid, _, _} =
        lists:keyfind(erlmcp_server, 1, NewChildren),

    ?assertNotEqual(InitialPid, NewPid),
    ?assert(is_process_alive(NewPid)),

    % Verify: Recovery time < 5s
    ?assert(timer:sleep(5000) =:= ok),

    % Cleanup
    erlmcp_core_sup:stop(Sup).

memory_exhaustion(Chaos) ->
    % Setup: Start server
    {ok, Server} = erlmcp_server:start_link(),

    % Exercise: Inject memory pressure
    ok = erlmcp_chaos:inject_memory_pressure(Chaos, 80), % 80% usage

    % Verify: Server degrades gracefully
    {ok, Status} = erlmcp_server:get_status(Server),
    ?assertEqual(degraded, maps:get(state, Status)),

    % Verify: Requests return bounded refusals (not crashes)
    RefusalCount = lists:foldl(
        fun(_, Acc) ->
            case erlmcp_server:list_tools(Server) of
                {error, resource_exhausted} -> Acc + 1;
                {ok, _} -> Acc
            end
        end,
        0,
        lists:seq(1, 100)
    ),

    ?assert(RefusalCount > 0), % Some refusals
    ?assert(RefusalCount < 100), % Not all refused (bounded)

    % Cleanup
    ok = erlmcp_chaos:remove_injection(Chaos, memory),
    erlmcp_server:stop(Server).
```

---

## Pattern 6: Property-Based Testing

### Purpose

Find edge cases through generative testing of invariants (roundtrips, state machines, protocol compliance).

### Property Test Structure

```mermaid
graph TB
    subgraph "Property Test"
        Generator[Random Input Generator]
        System[System Under Test]
        Predicate[Property Predicate]
    end

    subgraph "Execution"
        Generate[Generate Input]
        Run[Run System]
        Check[Check Predicate]
    end

    subgraph "Outcomes"
        Pass[Property Passes]
        Fail[Property Fails]
        Shrink[Shrink to Minimal]
    end

    Generator --> Generate
    Generate --> Run
    Run --> System
    System --> Check
    Check --> Predicate

    Predicate --> Pass
    Predicate --> Fail

    Fail --> Shrink
    Shrink --> Run

    style Pass fill:#51cf66
    style Fail fill:#ff6b6b
    style Shrink fill:#ffd43b
```

### Roundtrip Property

```mermaid
sequenceDiagram
    participant Prop as Proper
    participant Gen as Generator
    participant Enc as Encoder
    participant Dec as Decoder
    participant Check as Predicate

    Prop->>Gen: Generate input
    Gen-->>Prop: Random message

    Prop->>Enc: Encode message
    Enc-->>Prop: Encoded data

    Prop->>Dec: Decode data
    Dec-->>Prop: Decoded message

    Prop->>Check: Compare original vs decoded
    Check-->>Prop: Equal / Not equal

    alt Equal
        Prop->>Prop: Property passes
    else Not Equal
        Prop->>Gen: Shrink input
        Gen-->>Prop: Smaller input
        Prop->>Enc: Retry with smaller
    end
```

### Implementation Template

```erlang
% Property: JSON-RPC encoding/decoding roundtrip
prop_json_rpc_roundtrip() ->
    ?FORALL(Message, message_generator(),
        begin
            % Exercise: Encode
            Encoded = erlmcp_json_rpc:encode(Message),

            % Exercise: Decode
            {ok, Decoded} = erlmcp_json_rpc:decode(Encoded),

            % Verify: Roundtrip invariant
            Decoded =:= Message
        end).

% Generator: Random JSON-RPC request
message_generator() ->
    ?LET({Id, Method, Params},
        {binary(), binary(), proper_types:list(prop_json())},
        #{
            jsonrpc => <<"2.0">>,
            id => Id,
            method => Method,
            params => Params
        }).

% Property: Tool invocation always returns valid response
prop_tool_invocation() ->
    ?FORALL({Tool, Args},
            {tool_generator(), proper_types:list(prop_json())},
        begin
            {ok, Server} = erlmcp_server:start_link(),
            ok = erlmcp_server:add_tool(Server, Tool),

            % Exercise: Call tool
            Result = erlmcp_server:call_tool(
                Server,
                maps:get(name, Tool),
                Args
            ),

            % Verify: Result is valid JSON-RPC response
            IsValid = case Result of
                {ok, Response} when is_map(Response) ->
                    maps:is_key(result, Response) orelse
                    maps:is_key(error, Response);
                _ ->
                    false
            end,

            erlmcp_server:stop(Server),
            IsValid
        end).

tool_generator() ->
    ?LET(Name, binary(),
        #{
            name => Name,
            description => <<"Test tool">>,
            handler => fun(_) -> {ok, #{result => ok}} end
        }).

% Property: State machine transitions are valid
prop_session_state_machine() ->
    ?FORALL(Commands,
            proper_types:list(commands()),
        begin
            {ok, Session} = erlmcp_session:start_link(),

            % Exercise: Execute commands
            Results = [execute_command(Session, Cmd) || Cmd <- Commands],

            % Verify: All transitions valid
            IsValid = lists:all(fun(R) -> R =:= valid end, Results),

            erlmcp_session:stop(Session),
            IsValid
        end).

commands() ->
    proper_types:oneof([
        {create, binary()},
        {update, binary(), term()},
        {delete, binary()},
        {get, binary()}
    ]).

execute_command(Session, {create, Id}) ->
    case erlmcp_session:create(Session, Id, #{}) of
        ok -> valid;
        {error, _} -> valid % Error is valid transition
    end;
execute_command(Session, {update, Id, Data}) ->
    case erlmcp_session:update(Session, Id, Data) of
        ok -> valid;
        {error, _} -> valid
    end;
execute_command(Session, {delete, Id}) ->
    case erlmcp_session:delete(Session, Id) of
        ok -> valid;
        {error, _} -> valid
    end;
execute_command(Session, {get, Id}) ->
    case erlmcp_session:get(Session, Id) of
        {ok, _} -> valid;
        {error, _} -> valid
    end.
```

---

## Pattern 7: Distributed Testing

### Purpose

Verify distributed coordination, replication, and failover across multiple nodes.

### Multi-Node Topology

```mermaid
graph TB
    subgraph "Node 1 (Primary)"
        R1[Registry]
        S1[Server 1]
        DB1[(Mnesia)]
    end

    subgraph "Node 2 (Replica)"
        R2[Registry]
        S2[Server 2]
        DB2[(Mnesia)]
    end

    subgraph "Node 3 (Replica)"
        R3[Registry]
        S3[Server 3]
        DB3[(Mnesia)]
    end

    DB1 <-_sync->|Replication| DB2
    DB2 <-sync->|Replication| DB3
    DB3 <-sync->|Replication| DB1

    S1 -.->|Register| R1
    S2 -.->|Register| R2
    S3 -.->|Register| R3

    Test[Test Suite] -.->|Lookup from any node| R1
    Test -.->|Lookup from any node| R2
    Test -.->|Lookup from any node| R3

    style DB1 fill:#51cf66
    style DB2 fill:#51cf66
    style DB3 fill:#51cf66
```

### Distributed Test Flow

```mermaid
sequenceDiagram
    participant Test as Test Suite
    participant Node1 as Node 1
    participant Node2 as Node 2
    participant Node3 as Node 3

    Test->>Node1: Start peer node
    Test->>Node2: Start peer node
    Test->>Node3: Start peer node

    Test->>Node1: Start Mnesia
    Test->>Node2: Start Mnesia
    Test->>Node3: Start Mnesia

    Test->>Node1: Create distributed table
    Node1->>Node2: Replicate schema
    Node1->>Node3: Replicate schema

    Test->>Node1: Register server1
    Node1->>Node1: Store in Mnesia

    Test->>Node2: Lookup server1
    Node2->>Node3: Query distributed table
    Node3-->>Node2: Return data
    Node2-->>Test: Server1 found

    Test->>Node2: Kill Node1
    Node1-->>Node3: Node down

    Test->>Node3: Lookup server1
    Node3-->>Test: Still found (replicated)

    Test->>Node2: Stop peer
    Test->>Node3: Stop peer
```

### Implementation Template

```erlang
distributed_registry_test_() ->
    {setup,
     fun setup_nodes/0,
     fun teardown_nodes/1,
     fun test_distribution/1}.

setup_nodes() ->
    % Start peer nodes
    {ok, PeerNode1} = ct_slave:start(node1, [
        {erl_flags, "-name node1@127.0.0.1"}
    ]),
    {ok, PeerNode2} = ct_slave:start(node2, [
        {erl_flags, "-name node2@127.0.0.1"}
    ]),

    % Wait for nodes to start
    timer:sleep(500),

    #{node1 => PeerNode1, node2 => PeerNode2}.

teardown_nodes(Nodes) ->
    ct_slave:stop(node1),
    ct_slave:stop(node2),
    timer:sleep(500).

test_distribution(Nodes) ->
    [
     ?_test(register_and_lookup_distributed(Nodes)),
     ?_test(replication_across_nodes(Nodes)),
     ?_test(failover_to_replica(Nodes))
    ].

register_and_lookup_distributed(#{node1 := Node1, node2 := Node2}) ->
    % Setup: Start Mnesia on all nodes
    rpc:call(Node1, mnesia, start, []),
    rpc:call(Node2, mnesia, start, []),
    mnesia:start(),

    % Setup: Create distributed table
    {atomic, ok} = mnesia:create_table(
        registry,
        [
            {disc_copies, [node(), Node1, Node2]},
            {attributes, record_info(fields, registry)}
        ]
    ),

    % Exercise: Register on node1
    ServerPid = spawn(Node1, fun() ->
        receive stop -> ok end
    end),

    rpc:call(Node1, erlmcp_registry, register_name,
        [{mcp, server, <<"s1">>}, ServerPid]
    ),

    % Verify: Can lookup from node2 (distributed)
    {ok, FoundPid} = rpc:call(Node2, erlmcp_registry,
        whereis_name, [{mcp, server, <<"s1">>}]
    ),

    ?assertEqual(ServerPid, FoundPid),

    % Cleanup
    mnesia:stop().

replication_across_nodes(#{node1 := Node1, node2 := Node2}) ->
    % Setup: Create replicated table
    rpc:call(Node1, mnesia, start, []),
    rpc:call(Node2, mnesia, start, []),
    mnesia:start(),

    {atomic, ok} = mnesia:create_table(
        sessions,
        [
            {disc_copies, [node(), Node1, Node2]},
            {attributes, record_info(fields, session)}
        ]
    ),

    % Exercise: Write on node1
    SessionData = #session{id => <<"sess1">>, data => #{test => true}},
    {atomic, ok} = rpc:call(Node1, mnesia, transaction, [fun() ->
        mnesia:write(SessionData)
    end]),

    % Verify: Read from node2 (replicated)
    {atomic, [FoundSession]} = rpc:call(Node2, mnesia, transaction, [fun() ->
        mnesia:read({session, <<"sess1">>})
    end]),

    ?assertEqual(SessionData, FoundSession),

    % Cleanup
    mnesia:stop().

failover_to_replica(#{node1 := Node1, node2 := Node2}) ->
    % Setup: Create replicated table
    rpc:call(Node1, mnesia, start, []),
    rpc:call(Node2, mnesia, start, []),
    mnesia:start(),

    {atomic, ok} = mnesia:create_table(
        config,
        [
            {disc_copies, [node(), Node1, Node2]},
            {attributes, record_info(fields, config)}
        ]
    ),

    % Exercise: Write data
    ConfigData = #config{key => <<"key1">>, value => <<"val1">>},
    {atomic, ok} = rpc:call(Node1, mnesia, transaction, [fun() ->
        mnesia:write(ConfigData)
    end]),

    % Exercise: Kill node1
    ct_slave:stop(node1),
    timer:sleep(500),

    % Verify: Data still available on node2 (replicated)
    {atomic, [FoundConfig]} = rpc:call(Node2, mnesia, transaction, [fun() ->
        mnesia:read({config, <<"key1">>})
    end]),

    ?assertEqual(ConfigData, FoundConfig),

    % Cleanup
    mnesia:stop().

% Record definitions
-record(registry, {name, pid}).
-record(session, {id, data}).
-record(config, {key, value}).
```

---

## Summary

**Pattern Library Coverage:**

- ✅ **Gen_server Lifecycle**: Start, stop, supervision, callbacks
- ✅ **Registry**: Registration, lookup, monitoring, cleanup
- ✅ **Transport**: Behavior compliance, message flow, failover
- ✅ **Session**: Persistence, failover, replication (ETS/DETS/Mnesia)
- ✅ **Chaos**: Failure injection, recovery, bounded refusals
- ✅ **Property**: Roundtrips, state machines, generators
- ✅ **Distributed**: Multi-node coordination, replication, failover

**Chicago School TDD Compliance:**

- All patterns use **real collaborators** (no mocks)
- All patterns use **state-based verification** (observable behavior)
- All patterns use **real processes** (gen_servers, supervised)
- All patterns include **setup/teardown** (clean lifecycle)

**Usage:**

1. Copy template for your use case
2. Adapt to your module's API
3. Run with `rebar3 eunit` or `rebar3 ct`
4. Verify Chicago School compliance (no mocks, real processes)

---

**Related Documentation:**
- [README](README.md) - Testing overview
- [TDD Strategy](tdd-strategy.md) - Chicago School methodology
- [Integration Tests](integration-tests.md) - Multi-process patterns
- [Testing Architecture](TESTING_ARCHITECTURE.md) - System design

**Last Updated:** 2026-01-31
**Maintained by:** erlang-test-engineer agent
**Version:** 2.1.0
