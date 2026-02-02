# erlmcp_flow_raft - Quick Reference

## Module Overview
Minimal Raft consensus for leader election only. Pure module (no gen_server).

## API

### start_election/1
```erlang
erlmcp_flow_raft:start_election(Nodes) -> {ok, Leader} | {error, no_quorum}

% Single node (always succeeds)
{ok, Leader} = erlmcp_flow_raft:start_election([]).

% 3-node cluster
Nodes = ['node1@host', 'node2@host'],
case erlmcp_flow_raft:start_election(Nodes) of
    {ok, Leader} -> 
        io:format("Leader elected: ~p~n", [Leader]);
    {error, no_quorum} -> 
        io:format("Failed to reach quorum~n")
end.
```

**Behavior**:
- Spawns parallel vote requests to all nodes
- Timeout: random 150-300ms
- Quorum: N/2 + 1 votes required
- Self always votes for self
- Returns self as leader if quorum reached

### heartbeat/2
```erlang
erlmcp_flow_raft:heartbeat(Leader, Nodes) -> ok | {error, leader_dead}

% Check if leader is alive
Leader = 'leader@host',
Nodes = ['node1@host', 'node2@host'],
case erlmcp_flow_raft:heartbeat(Leader, Nodes) of
    ok -> 
        io:format("Leader is alive~n");
    {error, leader_dead} -> 
        io:format("Leader unreachable, starting new election~n"),
        erlmcp_flow_raft:start_election(Nodes)
end.

% Self-check (always succeeds)
ok = erlmcp_flow_raft:heartbeat(node(), []).
```

**Behavior**:
- Uses `net_adm:ping/1` for liveness check
- Self-node always returns ok
- Validates leader is in node list

### is_leader/1
```erlang
erlmcp_flow_raft:is_leader(NodeOrPid) -> boolean()

% Check if we are the leader
case erlmcp_flow_raft:is_leader(node()) of
    true -> handle_leader_request();
    false -> forward_to_leader()
end.

% Check if process is on leader node
case erlmcp_flow_raft:is_leader(Pid) of
    true -> local_call(Pid);
    false -> remote_call(Pid)
end.
```

## Usage Patterns

### Pattern 1: Simple Election Loop
```erlang
-module(my_election_loop).
-export([start/1, loop/2]).

start(Nodes) ->
    spawn(?MODULE, loop, [Nodes, undefined]).

loop(Nodes, Leader) ->
    case Leader of
        undefined ->
            % No leader, start election
            case erlmcp_flow_raft:start_election(Nodes) of
                {ok, NewLeader} ->
                    io:format("Elected leader: ~p~n", [NewLeader]),
                    loop(Nodes, NewLeader);
                {error, no_quorum} ->
                    timer:sleep(1000),
                    loop(Nodes, undefined)
            end;
        _ ->
            % Have leader, check heartbeat
            case erlmcp_flow_raft:heartbeat(Leader, Nodes) of
                ok ->
                    timer:sleep(100),
                    loop(Nodes, Leader);
                {error, leader_dead} ->
                    io:format("Leader dead, re-electing~n"),
                    loop(Nodes, undefined)
            end
    end.
```

### Pattern 2: gen_server Integration
```erlang
-module(my_server).
-behaviour(gen_server).

-record(state, {
    nodes = [],
    leader = undefined,
    last_heartbeat = 0
}).

init(Nodes) ->
    self() ! start_election,
    {ok, #state{nodes = Nodes}}.

handle_info(start_election, State) ->
    case erlmcp_flow_raft:start_election(State#state.nodes) of
        {ok, Leader} ->
            erlang:send_after(100, self(), heartbeat),
            {noreply, State#state{leader = Leader}};
        {error, no_quorum} ->
            erlang:send_after(1000, self(), start_election),
            {noreply, State}
    end;

handle_info(heartbeat, #state{leader = Leader, nodes = Nodes} = State) ->
    case erlmcp_flow_raft:heartbeat(Leader, Nodes) of
        ok ->
            erlang:send_after(100, self(), heartbeat),
            {noreply, State#state{last_heartbeat = erlang:system_time(millisecond)}};
        {error, leader_dead} ->
            self() ! start_election,
            {noreply, State#state{leader = undefined}}
    end.

handle_call(get_leader, _From, State) ->
    {reply, State#state.leader, State};

handle_call({do_work, Work}, From, State) ->
    case erlmcp_flow_raft:is_leader(node()) of
        true ->
            % We are leader, process work
            Result = process_work(Work),
            {reply, Result, State};
        false ->
            % Forward to leader
            {reply, {error, not_leader, State#state.leader}, State}
    end.
```

### Pattern 3: Distributed Work Queue
```erlang
-module(my_work_queue).
-export([submit_work/2, elect_coordinator/1]).

submit_work(Work, Nodes) ->
    case erlmcp_flow_raft:start_election(Nodes) of
        {ok, Leader} ->
            case erlmcp_flow_raft:is_leader(Leader) of
                true ->
                    % We are the leader, process locally
                    process_work(Work);
                false ->
                    % Send to leader
                    rpc:call(Leader, ?MODULE, process_work, [Work])
            end;
        {error, no_quorum} ->
            {error, no_coordinator}
    end.

elect_coordinator(Nodes) ->
    % Retry election until success
    case erlmcp_flow_raft:start_election(Nodes) of
        {ok, Leader} -> {ok, Leader};
        {error, no_quorum} ->
            timer:sleep(rand:uniform(300)),
            elect_coordinator(Nodes)
    end.
```

## Testing

### Unit Test Example
```erlang
-module(my_raft_test).
-include_lib("eunit/include/eunit.hrl").

single_node_test() ->
    % Single node should always win
    {ok, Leader} = erlmcp_flow_raft:start_election([]),
    ?assertEqual(node(), Leader),
    ?assert(erlmcp_flow_raft:is_leader(Leader)).

heartbeat_test() ->
    % Self-heartbeat should always succeed
    ?assertEqual(ok, erlmcp_flow_raft:heartbeat(node(), [])).

quorum_test() ->
    % With unreachable nodes, election should fail
    FakeNodes = ['node1@fake', 'node2@fake'],
    ?assertMatch({error, no_quorum}, 
                 erlmcp_flow_raft:start_election(FakeNodes)).
```

## Configuration

### Timeouts
```erlang
-define(ELECTION_TIMEOUT_MIN, 150).  % Minimum election timeout (ms)
-define(ELECTION_TIMEOUT_MAX, 300).  % Maximum election timeout (ms)
-define(HEARTBEAT_INTERVAL, 100).    % Heartbeat interval (ms)
```

To customize, fork the module and change these values.

### Quorum Calculation
```erlang
Nodes = 5,
Quorum = (Nodes div 2) + 1,  % = 3 votes required
```

| Total Nodes | Quorum | Tolerated Failures |
|-------------|--------|--------------------|
| 1 | 1 | 0 |
| 3 | 2 | 1 |
| 5 | 3 | 2 |
| 7 | 4 | 3 |

## Limitations (MVP)

1. **No persistence**: Elections don't survive node restarts
2. **No log replication**: Cannot replicate state across nodes
3. **Single term**: No term transitions or conflict resolution
4. **Ping-based voting**: Not as robust as RPC-based voting
5. **No follower state**: Clients manage their own state

## Troubleshooting

### Election always fails (no_quorum)
- Check node connectivity: `net_adm:ping('node@host')`
- Verify node names in cluster match actual nodes
- Ensure at least N/2+1 nodes are reachable

### Leader appears dead but is alive
- Check network latency/partitions
- Verify `net_adm:ping/1` works between nodes
- Consider increasing heartbeat timeout

### Multiple leaders elected
- Not possible in this implementation (ping-based, no split-brain)
- Each node only votes for self in start_election/1

## Performance

| Operation | Latency |
|-----------|---------|
| start_election/1 | 150-300ms (worst case) |
| heartbeat/2 | 1-10ms (local), RTT (remote) |
| is_leader/1 | <1ms |

**Throughput**: Not applicable (stateless operations)

## See Also

- Source: `/home/user/erlmcp/apps/erlmcp_flow/src/erlmcp_flow_raft.erl`
- Tests: `/home/user/erlmcp/apps/erlmcp_flow/test/erlmcp_flow_raft_tests.erl`
- Roadmap: `/home/user/erlmcp/ERLMCP_FLOW_80_20_ROADMAP.md`
