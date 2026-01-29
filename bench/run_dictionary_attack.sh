#!/bin/bash
set -e

# Dictionary Attack Stress Test #15
# This test simulates rapid reconnections with random IDs to find DoS vulnerabilities

echo "=== DICTIONARY ATTACK STRESS TEST #15 ==="
echo "Attack: Rapid Reconnect with Random IDs"
echo "Objective: Find rate limits, lockout behavior, or crash from auth overload"
echo ""

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
PORT=${1:-10015}
RATE=${2:-1000}  # connections per second
CREDENTIAL_TYPE=${3:-mixed}  # valid, invalid, mixed, same_credential

echo "Configuration:"
echo "  Port: $PORT"
echo "  Rate: $RATE connections/sec"
echo "  Credential Type: $CREDENTIAL_TYPE"
echo ""

# Check if rebar3 is available
if ! command -v rebar3 &> /dev/null; then
    echo -e "${RED}Error: rebar3 not found${NC}"
    echo "Please install rebar3: https://rebar3.org/"
    exit 1
fi

# Compile the project
echo "Compiling project..."
cd "$PROJECT_ROOT"
TERM=dumb rebar3 compile
if [ $? -ne 0 ]; then
    echo -e "${RED}Compilation failed${NC}"
    exit 1
fi
echo -e "${GREEN}Compilation successful${NC}"
echo ""

# Check if we can start an Erlang node
echo "Starting Erlang node for attack..."
erl -pa _build/default/lib/*/ebin -noshell -sname dictionary_attack -setcookie erlmcp -eval "
    % Compile the benchmark module
    case c('$SCRIPT_DIR/erlmcp_bench_dictionary_attack.erl', [{i, \"$PROJECT_ROOT/apps/erlmcp_core/include\"}]) of
        {ok, Module} ->
            io:format('Module compiled: ~p~n', [Module]),
            
            % Start required applications
            application:ensure_all_started(kernel),
            application:ensure_all_started(stdlib),
            application:ensure_all_started(crypto),
            application:ensure_all_started(jsx),
            application:ensure_all_started(gproc),
            application:ensure_all_started(ranch),
            application:ensure_all_started(gun),
            application:ensure_all_started(poolboy),
            
            io:format('Applications started~n'),
            
            % Run the attack
            io:format('~n~n=== STARTING DICTIONARY ATTACK ===~n~n'),
            
            Config = #{
                total_attempts => 1000000,
                connect_rate_per_sec => $RATE,
                port => $PORT,
                credential_type => '$CREDENTIAL_TYPE',
                report_interval => 10000
            },
            
            case erlmcp_bench_dictionary_attack:run(Config) of
                {ok, Result} ->
                    io:format('~n~n=== ATTACK COMPLETE ===~n~n'),
                    Report = erlmcp_bench_dictionary_attack:generate_report(Result),
                    io:format('~s~n', [Report]),
                    
                    % Save results to file
                    Timestamp = integer_to_list(erlang:system_time(second)),
                    Filename = \"$SCRIPT_DIR/results/dictionary_attack_\" ++ atom_to_list('$CREDENTIAL_TYPE') ++ \"_\" ++ Timestamp ++ \".txt\",
                    file:write_file(Filename, Report),
                    io:format('~nResults saved to: ~s~n', [Filename]),
                    
                    halt(0);
                {error, Reason} ->
                    io:format('Attack failed: ~p~n', [Reason]),
                    halt(1)
            end;
        {error, Reason, Errors} ->
            io:format('Compilation failed: ~p~nErrors: ~p~n', [Reason, Errors]),
            halt(1)
    end
"
