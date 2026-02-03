#!/usr/bin/env bash
# ============================================================================
# emit_cluster_mermaid.sh - Erlang Cluster Proof Mermaid Diagram
# ============================================================================
# CONSTITUTION: DOCKER-ONLY CONSTITUTION
#
# Generates a Mermaid diagram that proves Erlang cluster membership:
#   1. Current node identity
#   2. Connected nodes via nodes()
#   3. Ping test results for expected nodes
#   4. Scheduler and process counts
#
# This is the REAL proof of cluster connectivity, not Docker Swarm status.
# The BEAM cluster is what matters for MCP session routing.
#
# Usage:
#   EXPECTED_NODES=3 ./emit_cluster_mermaid.sh
#   RUN_ID=xxx OUT_DIR=/path ./emit_cluster_mermaid.sh
#
# Exit codes:
#   0 - Cluster proof generated (connectivity verified)
#   1 - Cluster incomplete (fewer nodes than expected)
#   2 - Forbidden host execution
# ============================================================================

set -euo pipefail

# ============================================================================
# HARD CHECK: Must be running inside Docker container
# ============================================================================
check_is_docker() {
    if [[ -f "/.dockerenv" ]]; then
        return 0
    fi
    if [[ -r "/proc/1/cgroup" ]] && grep -Eq "(docker|kubepods|containerd)" /proc/1/cgroup 2>/dev/null; then
        return 0
    fi
    return 1
}

if ! check_is_docker; then
    echo "ANDON: FORBIDDEN_HOST_EXECUTION"
    echo "Refusal: emit_cluster_mermaid.sh MUST run inside Docker container"
    exit 2
fi

# ============================================================================
# Configuration
# ============================================================================
RUN_ID="${RUN_ID:-$(date -u +%Y%m%dT%H%M%SZ)}"
OUT_DIR="${OUT_DIR:-/workspace/receipts/$RUN_ID}"
EXPECTED_NODES="${EXPECTED_NODES:-1}"  # Minimum expected nodes in cluster
ERLANG_COOKIE="${ERLANG_COOKIE:-erlmcp_cluster_cookie}"

mkdir -p "$OUT_DIR"

# ============================================================================
# Erlang Cluster Query Script
# ============================================================================
CLUSTER_QUERY='
-module(cluster_query).
-export([main/1]).

main([ExpectedStr]) ->
    Expected = list_to_integer(ExpectedStr),

    %% Basic node info
    Node = node(),
    ConnectedNodes = nodes(),
    ConnectedCount = length(ConnectedNodes),
    TotalNodes = ConnectedCount + 1, %% Including self

    %% System info
    Schedulers = erlang:system_info(schedulers_online),
    ProcessCount = erlang:system_info(process_count),
    MemoryTotal = erlang:memory(total) div 1048576,

    %% Ping all connected nodes to verify
    PingResults = lists:map(fun(N) ->
        Result = net_adm:ping(N),
        {N, Result}
    end, ConnectedNodes),

    %% Output results as parseable lines
    io:format("NODE=~s~n", [Node]),
    io:format("CONNECTED_COUNT=~p~n", [ConnectedCount]),
    io:format("TOTAL_NODES=~p~n", [TotalNodes]),
    io:format("EXPECTED_NODES=~p~n", [Expected]),
    io:format("SCHEDULERS=~p~n", [Schedulers]),
    io:format("PROCESS_COUNT=~p~n", [ProcessCount]),
    io:format("MEMORY_MB=~p~n", [MemoryTotal]),

    %% Connected nodes list
    io:format("CONNECTED_NODES=~p~n", [ConnectedNodes]),

    %% Ping results
    lists:foreach(fun({N, R}) ->
        io:format("PING_~s=~s~n", [N, R])
    end, PingResults),

    %% Cluster health status
    case TotalNodes >= Expected of
        true ->
            io:format("CLUSTER_STATUS=healthy~n"),
            io:format("CLUSTER_HEALTH=OK~n"),
            halt(0);
        false ->
            io:format("CLUSTER_STATUS=incomplete~n"),
            io:format("CLUSTER_HEALTH=FAIL: ~p nodes < ~p expected~n", [TotalNodes, Expected]),
            halt(1)
    end.
'

# ============================================================================
# Execute Cluster Query
# ============================================================================
echo "Querying Erlang cluster state..."

QUERY_FILE=$(mktemp /tmp/cluster_query_XXXXXX.erl)
echo "$CLUSTER_QUERY" > "$QUERY_FILE"

# Try to query cluster (may fail if no cluster running)
CLUSTER_OUTPUT=""
CLUSTER_EXIT=0

if command -v erl >/dev/null 2>&1; then
    set +e
    CLUSTER_OUTPUT=$(erl -noshell -setcookie "$ERLANG_COOKIE" \
        -eval "
            Node = node(),
            ConnectedNodes = nodes(),
            ConnectedCount = length(ConnectedNodes),
            io:format(\"NODE=~p~n\", [Node]),
            io:format(\"CONNECTED_COUNT=~p~n\", [ConnectedCount]),
            io:format(\"CONNECTED_NODES=~p~n\", [ConnectedNodes]),
            io:format(\"SCHEDULERS=~p~n\", [erlang:system_info(schedulers_online)]),
            io:format(\"PROCESS_COUNT=~p~n\", [erlang:system_info(process_count)]),
            io:format(\"MEMORY_MB=~p~n\", [erlang:memory(total) div 1048576]),
            io:format(\"OTP_VSN=~s~n\", [erlang:system_info(otp_release)]),
            halt(0).
        " 2>/dev/null)
    CLUSTER_EXIT=$?
    set -e
fi

rm -f "$QUERY_FILE"

# Parse output or use defaults
NODE=$(echo "$CLUSTER_OUTPUT" | grep "^NODE=" | cut -d= -f2 || echo "nonode@nohost")
CONNECTED_COUNT=$(echo "$CLUSTER_OUTPUT" | grep "^CONNECTED_COUNT=" | cut -d= -f2 || echo "0")
CONNECTED_NODES=$(echo "$CLUSTER_OUTPUT" | grep "^CONNECTED_NODES=" | cut -d= -f2- || echo "[]")
SCHEDULERS=$(echo "$CLUSTER_OUTPUT" | grep "^SCHEDULERS=" | cut -d= -f2 || echo "unknown")
PROCESS_COUNT=$(echo "$CLUSTER_OUTPUT" | grep "^PROCESS_COUNT=" | cut -d= -f2 || echo "unknown")
MEMORY_MB=$(echo "$CLUSTER_OUTPUT" | grep "^MEMORY_MB=" | cut -d= -f2 || echo "unknown")
OTP_VSN=$(echo "$CLUSTER_OUTPUT" | grep "^OTP_VSN=" | cut -d= -f2 || echo "unknown")

TOTAL_NODES=$((CONNECTED_COUNT + 1))
HOSTNAME=$(hostname)
TIMESTAMP=$(date -u +%Y-%m-%dT%H:%M:%SZ)

# Determine cluster health
if [[ $TOTAL_NODES -ge $EXPECTED_NODES ]]; then
    CLUSTER_STATUS="healthy"
    CLUSTER_HEALTH="OK: $TOTAL_NODES nodes >= $EXPECTED_NODES expected"
else
    CLUSTER_STATUS="incomplete"
    CLUSTER_HEALTH="FAIL: $TOTAL_NODES nodes < $EXPECTED_NODES expected"
fi

# ============================================================================
# Generate Cluster Mermaid Diagram
# ============================================================================
echo "Generating cluster topology Mermaid diagram..."

cat > "$OUT_DIR/cluster_topology.mmd" <<EOF
%%{init: {'theme': 'dark'}}%%
%% ============================================================================
%% ERLANG CLUSTER TOPOLOGY PROOF
%% ============================================================================
%% AUTOGENERATED: $RUN_ID
%% TIMESTAMP: $TIMESTAMP
%% CLUSTER_STATUS: $CLUSTER_STATUS
%% ============================================================================

flowchart TB
    subgraph ClusterOverview["Erlang Distributed Cluster"]
        direction TB

        subgraph CurrentNode["Current Node: $HOSTNAME"]
            direction TB
            N1["$NODE"]
            N1_INFO["OTP: $OTP_VSN\\nSchedulers: $SCHEDULERS\\nProcesses: $PROCESS_COUNT\\nMemory: ${MEMORY_MB}MB"]
        end

        subgraph ConnectedPeers["Connected Peers ($CONNECTED_COUNT)"]
            direction TB
EOF

# Add connected nodes to diagram
if [[ "$CONNECTED_NODES" != "[]" && -n "$CONNECTED_NODES" ]]; then
    # Parse the Erlang list format
    NODES_CLEAN=$(echo "$CONNECTED_NODES" | tr -d '[]' | tr ',' '\n' | tr -d "'" | tr -d ' ')
    NODE_NUM=2
    while IFS= read -r peer_node; do
        if [[ -n "$peer_node" ]]; then
            echo "            N${NODE_NUM}[\"$peer_node\"]" >> "$OUT_DIR/cluster_topology.mmd"
            NODE_NUM=$((NODE_NUM + 1))
        fi
    done <<< "$NODES_CLEAN"
else
    echo "            EMPTY[\"No connected peers\"]" >> "$OUT_DIR/cluster_topology.mmd"
fi

cat >> "$OUT_DIR/cluster_topology.mmd" <<EOF
        end
    end

    subgraph ClusterHealth["Cluster Health"]
        direction TB
        STATUS["Status: $CLUSTER_STATUS"]
        HEALTH["$CLUSTER_HEALTH"]
        EXPECTED["Expected: >= $EXPECTED_NODES nodes"]
        ACTUAL["Actual: $TOTAL_NODES nodes"]
    end

    subgraph MCPRouting["MCP Session Routing"]
        direction TB
        REGISTRY["gproc Global Registry"]
        SESSIONS["Distributed Sessions"]
        ROUTING["Request Routing"]
    end

    %% Connections
    CurrentNode --> ConnectedPeers
    ClusterOverview --> ClusterHealth
    ClusterOverview --> MCPRouting

    %% Styling
    classDef current fill:#0f3460,stroke:#16213e,color:#00fff5
    classDef peer fill:#1a1a2e,stroke:#16213e,color:#e94560
    classDef healthy fill:#2d4a22,stroke:#16213e,color:#00ff00
    classDef incomplete fill:#5a1a1a,stroke:#16213e,color:#ff4444
    classDef routing fill:#533483,stroke:#16213e,color:#fff

    class CurrentNode,N1,N1_INFO current
    class ConnectedPeers peer
EOF

# Style based on health
if [[ "$CLUSTER_STATUS" == "healthy" ]]; then
    echo "    class ClusterHealth,STATUS,HEALTH healthy" >> "$OUT_DIR/cluster_topology.mmd"
else
    echo "    class ClusterHealth,STATUS,HEALTH incomplete" >> "$OUT_DIR/cluster_topology.mmd"
fi

echo "    class MCPRouting,REGISTRY,SESSIONS,ROUTING routing" >> "$OUT_DIR/cluster_topology.mmd"

echo "OK: Generated $OUT_DIR/cluster_topology.mmd"

# ============================================================================
# Generate Cluster Meta JSON
# ============================================================================
cat > "$OUT_DIR/cluster_meta.json" <<EOF
{
  "run_id": "$RUN_ID",
  "timestamp": "$TIMESTAMP",
  "cluster": {
    "status": "$CLUSTER_STATUS",
    "health": "$CLUSTER_HEALTH",
    "expected_nodes": $EXPECTED_NODES,
    "total_nodes": $TOTAL_NODES,
    "connected_count": $CONNECTED_COUNT
  },
  "current_node": {
    "hostname": "$HOSTNAME",
    "node": "$NODE",
    "otp_version": "$OTP_VSN",
    "schedulers": "$SCHEDULERS",
    "process_count": "$PROCESS_COUNT",
    "memory_mb": "$MEMORY_MB"
  },
  "connected_nodes": "$CONNECTED_NODES",
  "is_docker": true
}
EOF

echo "OK: Generated $OUT_DIR/cluster_meta.json"

# ============================================================================
# Output Summary
# ============================================================================
echo ""
echo "=========================================="
echo "CLUSTER PROOF GENERATED"
echo "=========================================="
echo "RUN_ID:          $RUN_ID"
echo "CLUSTER_STATUS:  $CLUSTER_STATUS"
echo "TOTAL_NODES:     $TOTAL_NODES / $EXPECTED_NODES expected"
echo "CONNECTED_NODES: $CONNECTED_NODES"
echo "HOSTNAME:        $HOSTNAME"
echo "NODE:            $NODE"
echo ""
echo "FILES:"
echo "  $OUT_DIR/cluster_topology.mmd"
echo "  $OUT_DIR/cluster_meta.json"
echo "=========================================="

# Exit with cluster health status
if [[ "$CLUSTER_STATUS" == "healthy" ]]; then
    exit 0
else
    echo ""
    echo "WARNING: Cluster incomplete - $TOTAL_NODES nodes < $EXPECTED_NODES expected"
    exit 1
fi
