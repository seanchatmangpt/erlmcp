# Consensus POC - Quick Start Guide

## One-Line Demo

```bash
rebar3 shell --eval "erlmcp_consensus_poc:run_demo()."
```

## API Cheat Sheet

```erlang
%% Start a node
{ok, Pid} = erlmcp_consensus_poc:start_link(NodeId).

%% Execute tool (routes to leader automatically)
{ok, Result} = erlmcp_consensus_poc:execute_tool(Pid, ToolName, Args).

%% Get status
Status = erlmcp_consensus_poc:get_status(Pid).
%% Returns: #{node_id, role, leader_pid, execution_count, audit_log_size}

%% Stop node
ok = erlmcp_consensus_poc:stop(Pid).
```

## Tool Examples

```erlang
%% Math operations
execute_tool(Pid, <<"add">>, #{<<"a">> => 1, <<"b">> => 2}).        % → {ok, 3}
execute_tool(Pid, <<"multiply">>, #{<<"x">> => 3, <<"y">> => 4}).  % → {ok, 12}
execute_tool(Pid, <<"divide">>, #{<<"a">> => 10, <<"b">> => 2}).   % → {ok, 5.0}
execute_tool(Pid, <<"subtract">>, #{<<"a">> => 20, <<"b">> => 5}). % → {ok, 15}

%% String operations
execute_tool(Pid, <<"concat">>, #{<<"s1">> => <<"hello">>, <<"s2">> => <<"world">>}).
% → {ok, <<"hello world">>}
```

## Quick Test

```erlang
%% Start 3 nodes
{ok, N1} = erlmcp_consensus_poc:start_link(node1),
{ok, N2} = erlmcp_consensus_poc:start_link(node2),
{ok, N3} = erlmcp_consensus_poc:start_link(node3),
timer:sleep(200). % Let election settle

%% Execute from any node (routes to leader)
{ok, 42} = erlmcp_consensus_poc:execute_tool(N1, <<"add">>, #{<<"a">> => 40, <<"b">> => 2}).
{ok, 42} = erlmcp_consensus_poc:execute_tool(N2, <<"add">>, #{<<"a">> => 40, <<"b">> => 2}).
{ok, 42} = erlmcp_consensus_poc:execute_tool(N3, <<"add">>, #{<<"a">> => 40, <<"b">> => 2}).

%% Check which is leader
S1 = erlmcp_consensus_poc:get_status(N1),
S2 = erlmcp_consensus_poc:get_status(N2),
S3 = erlmcp_consensus_poc:get_status(N3),
[maps:get(role, S) || S <- [S1, S2, S3]]. % → [leader, follower, follower] (order varies)

%% Cleanup
erlmcp_consensus_poc:stop(N1),
erlmcp_consensus_poc:stop(N2),
erlmcp_consensus_poc:stop(N3).
```

## Test Failover

```erlang
%% Start nodes
{ok, N1} = erlmcp_consensus_poc:start_link(node1),
{ok, N2} = erlmcp_consensus_poc:start_link(node2),
{ok, N3} = erlmcp_consensus_poc:start_link(node3),
timer:sleep(200).

%% Find leader
Leader = case erlmcp_consensus_poc:get_status(N1) of
    #{role := leader} -> N1;
    _ -> case erlmcp_consensus_poc:get_status(N2) of
        #{role := leader} -> N2;
        _ -> N3
    end
end.

%% Kill leader
erlmcp_consensus_poc:stop(Leader),
timer:sleep(300). % Wait for re-election

%% Execute tool still works
Remaining = lists:delete(Leader, [N1, N2, N3]),
[First | _] = Remaining,
{ok, 100} = erlmcp_consensus_poc:execute_tool(First, <<"add">>, #{<<"a">> => 50, <<"b">> => 50}).

%% Cleanup
[erlmcp_consensus_poc:stop(N) || N <- Remaining].
```

## Run Tests

```bash
rebar3 eunit --module=erlmcp_consensus_poc_tests
```

## Files

- **Implementation**: `apps/erlmcp_core/src/poc/erlmcp_consensus_poc.erl`
- **Tests**: `apps/erlmcp_core/test/erlmcp_consensus_poc_tests.erl`
- **Full docs**: `apps/erlmcp_core/src/poc/README_CONSENSUS_POC.md`
- **This guide**: `apps/erlmcp_core/src/poc/QUICK_START.md`

## What It Demonstrates

✅ Leader election using Erlang global registry
✅ Exactly-once tool execution (all on leader)
✅ Request forwarding (follower → leader)
✅ Automatic failover on leader death
✅ Audit log of all executions

## Pattern for Production

This POC shows the **pattern** you'd use with ra/raft:

- POC uses `global:register_name/2` → Production uses Raft consensus
- POC uses in-memory log → Production uses replicated durable log
- POC uses process monitoring → Production uses Raft heartbeat
- POC shows the coordination pattern → Production scales it

See `README_CONSENSUS_POC.md` for complete details.
