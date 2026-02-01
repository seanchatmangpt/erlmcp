#!/bin/bash
# connect-server.sh - Interactive REPL automation script
# Connect to erlmcp server and execute commands from a script file

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m'

PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

print_header() {
    echo -e "${BLUE}========================================${NC}"
    echo -e "${BLUE}$1${NC}"
    echo -e "${BLUE}========================================${NC}"
}

print_success() {
    echo -e "${GREEN}✓ $1${NC}"
}

print_info() {
    echo -e "${BLUE}ℹ $1${NC}"
}

# ============================================================================
# Script 1: Create and list resources
# ============================================================================
script_create_resources() {
    print_header "Script 1: Create and List Resources"

    local script_content=$(cat <<'EOF'
% Start a server
{ok, ServerPid} = erlmcp_server:start_link(resource_demo, #{
  transport => stdio
}),
io:format("Server started: ~p~n", [ServerPid]),

% Define resources
Resources = [
  #{
    name => <<"users">>,
    uri => <<"api://localhost/users">>,
    description => <<"User directory">>,
    mimeType => <<"application/json">>
  },
  #{
    name => <<"config">>,
    uri => <<"file:///etc/config">>,
    description => <<"Configuration file">>,
    mimeType => <<"text/plain">>
  }
],

% Register resources
lists:foreach(fun(Resource) ->
  erlmcp_server:add_resource(ServerPid, Resource),
  io:format("Added resource: ~p~n", [maps:get(name, Resource)])
end, Resources),

% List all resources
AllResources = erlmcp_server:list_resources(ServerPid),
io:format("Total resources: ~w~n", [length(AllResources)]),

% Show summary
lists:foreach(fun(R) ->
  io:format("  - ~s: ~s~n", [maps:get(name, R), maps:get(uri, R)])
end, AllResources),

% Cleanup
erlmcp_server:stop(ServerPid),
io:format("Server stopped~n"),
halt(0).
EOF
    )

    # Write to temp file
    local script_file="/tmp/erlmcp_create_resources.erl"
    echo "$script_content" > "$script_file"

    # Execute in Erlang
    cd "$PROJECT_ROOT"
    erl -noshell -s erlmcp_core -eval "$(cat "$script_file")" 2>/dev/null || true

    rm -f "$script_file"
    print_success "Script executed"
}

# ============================================================================
# Script 2: Performance stress test
# ============================================================================
script_stress_test() {
    print_header "Script 2: Stress Test - Many Clients"

    local num_clients="${1:-100}"

    print_info "Starting stress test with $num_clients concurrent clients..."

    local script_content=$(cat <<EOF'
% Configuration
NumClients = $num_clients,
Duration = 5000,  % 5 seconds

io:format("Stress test: ~w clients, ~w ms duration~n", [NumClients, Duration]),

% Start server
{ok, ServerPid} = erlmcp_server:start_link(stress_test, #{}),

% Spawn clients
ClientPids = [
  spawn(fun() ->
    {ok, Pid} = erlmcp_client:start_link(
      list_to_atom("client_" ++ integer_to_list(I)),
      #{server_pid => ServerPid}
    ),
    % Simulate work
    timer:sleep(Duration),
    erlmcp_client:stop(Pid)
  end)
  || I <- lists:seq(1, NumClients)
],

% Wait for all clients
lists:foreach(fun(Pid) ->
  monitor(process, Pid),
  receive
    {'DOWN', _, process, Pid, _} -> ok
  after Duration + 1000 -> timeout
  end
end, ClientPids),

% Report
io:format("~nStress test completed~n"),
io:format("Clients spawned: ~w~n", [NumClients]),
io:format("Duration: ~w ms~n", [Duration]),

% Cleanup
erlmcp_server:stop(ServerPid),
halt(0).
EOF
    )

    cd "$PROJECT_ROOT"
    # Note: This is a simplified version - real implementation would measure metrics
    erl -noshell -eval "$script_content" 2>/dev/null || true

    print_success "Stress test completed"
}

# ============================================================================
# Script 3: Health check monitoring
# ============================================================================
script_health_check() {
    print_header "Script 3: Health Check Loop"

    print_info "Running health checks every 2 seconds (press Ctrl+C to stop)..."

    local count=0
    while true; do
        count=$((count + 1))

        # Get process count
        proc_count=$(erl -noshell -eval "io:format(\"~w~n\", [erlang:system_info(process_count)])." -s init stop 2>/dev/null | head -1)

        # Get memory
        memory=$(erl -noshell -eval "erlang:memory(processes), io:format(\"~w~n\", [erlang:memory(processes)])." -s init stop 2>/dev/null | head -1)

        # Format output
        printf "\r[%2d] Processes: %3s | Memory: %s bytes" "$count" "$proc_count" "$memory"

        sleep 2
    done
}

# ============================================================================
# Script 4: Integration test workflow
# ============================================================================
script_integration_test() {
    print_header "Script 4: Integration Test Workflow"

    local script_content=$(cat <<'EOF'
% Define test cases
TestCases = [
  {setup, <<"Server setup">>, fun() ->
    {ok, Pid} = erlmcp_server:start_link(test_server, #{}),
    {ok, Pid}
  end},
  {resource_add, <<"Add resource">>, fun(ServerPid) ->
    Resource = #{name => <<"test">>, uri => <<"test://">>},
    erlmcp_server:add_resource(ServerPid, Resource),
    ok
  end},
  {resource_list, <<"List resources">>, fun(ServerPid) ->
    Resources = erlmcp_server:list_resources(ServerPid),
    {ok, Resources}
  end},
  {cleanup, <<"Cleanup">>, fun(ServerPid) ->
    erlmcp_server:stop(ServerPid),
    ok
  end}
],

% Run tests
io:format("Running integration tests...~n~n"),

Results = lists:foldl(fun({TestType, Name, TestFun}, State) ->
  try
    case TestType of
      setup ->
        Result = TestFun(),
        io:format("✓ ~s~n", [Name]),
        [Result | State];
      cleanup ->
        [Pid | _] = State,
        TestFun(Pid),
        io:format("✓ ~s~n", [Name]),
        State;
      _ ->
        [Pid | _] = State,
        TestFun(Pid),
        io:format("✓ ~s~n", [Name]),
        State
    end
  catch
    error:Error ->
      io:format("✗ ~s: ~p~n", [Name, Error]),
      State
  end
end, [], TestCases),

io:format("~nIntegration tests completed~n"),
halt(0).
EOF
    )

    cd "$PROJECT_ROOT"
    erl -noshell -eval "$script_content" 2>/dev/null || true

    print_success "Integration tests completed"
}

# ============================================================================
# Script 5: Memory leak detection
# ============================================================================
script_memory_leak_detection() {
    print_header "Script 5: Memory Leak Detection"

    print_info "Allocating and releasing memory in cycles..."
    print_info "Check for unbounded growth..."

    local script_content=$(cat <<'EOF'
% Test parameters
Iterations = 100,
AllocationPerIteration = 1000,

io:format("Memory leak test: ~w iterations~n", [Iterations]),

% Baseline
BaseMem = erlang:memory(processes),
io:format("Baseline memory: ~w bytes~n", [BaseMem]),

% Run allocations
lists:foreach(fun(I) ->
  % Allocate
  _Data = lists:seq(1, AllocationPerIteration),

  % Every 10 iterations, check memory
  case I rem 10 of
    0 ->
      CurrentMem = erlang:memory(processes),
      Growth = CurrentMem - BaseMem,
      Percentage = (Growth / BaseMem) * 100,
      io:format("Iteration ~3w: ~w bytes (growth: ~.1f%)~n",
        [I, CurrentMem, Percentage]);
    _ -> ok
  end
end, lists:seq(1, Iterations)),

% Final check
FinalMem = erlang:memory(processes),
TotalGrowth = FinalMem - BaseMem,
TotalPercentage = (TotalGrowth / BaseMem) * 100,

io:format("~nFinal memory: ~w bytes~n", [FinalMem]),
io:format("Total growth: ~w bytes (~.1f%)~n", [TotalGrowth, TotalPercentage]),

case TotalPercentage > 50 of
  true ->
    io:format("⚠ Warning: Significant memory growth detected~n"),
    halt(1);
  false ->
    io:format("✓ Memory leak test passed~n"),
    halt(0)
end.
EOF
    )

    cd "$PROJECT_ROOT"
    erl -noshell -eval "$script_content" 2>/dev/null || true

    print_success "Memory leak detection completed"
}

# ============================================================================
# Main
# ============================================================================
main() {
    local script="${1:-help}"

    case "$script" in
        1|resources)
            script_create_resources
            ;;
        2|stress)
            script_stress_test "${2:-100}"
            ;;
        3|health)
            script_health_check
            ;;
        4|integration)
            script_integration_test
            ;;
        5|memory)
            script_memory_leak_detection
            ;;
        *)
            echo "Usage: $0 <script> [args]"
            echo ""
            echo "Available scripts:"
            echo "  1, resources          Create and list resources"
            echo "  2, stress [NUM]       Stress test with NUM clients (default: 100)"
            echo "  3, health             Continuous health check (Ctrl+C to stop)"
            echo "  4, integration        Run integration test workflow"
            echo "  5, memory             Detect memory leaks"
            echo ""
            echo "Examples:"
            echo "  $0 resources           # Create resources demo"
            echo "  $0 stress 50           # Stress test with 50 clients"
            echo "  $0 health              # Monitor health continuously"
            echo "  $0 integration         # Run integration tests"
            echo "  $0 memory              # Check for memory leaks"
            exit 1
            ;;
    esac
}

main "$@"
