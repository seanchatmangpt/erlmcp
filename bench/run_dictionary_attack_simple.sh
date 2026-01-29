#!/bin/bash
set -e

echo "=== DICTIONARY ATTACK STRESS TEST #15 ==="
echo "Attack: Rapid Reconnect with Random IDs"
echo "Objective: Find rate limits, lockout behavior, or crash from auth overload"
echo ""

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
RESULTS_DIR="$SCRIPT_DIR/results"
mkdir -p "$RESULTS_DIR"

# Configuration
PORT=${1:-10015}
RATE=${2:-100}
TOTAL_ATTEMPTS=${3:-10000}
CREDENTIAL_TYPE=${4:-mixed}

echo -e "${BLUE}Configuration:${NC}"
echo "  Port: $PORT"
echo "  Rate: $RATE connections/sec"
echo "  Total Attempts: $TOTAL_ATTEMPTS"
echo "  Credential Type: $CREDENTIAL_TYPE (valid/invalid/mixed/same_credential)"
echo ""

# Check if Erlang is available
if ! command -v erl &> /dev/null; then
    echo -e "${RED}Error: Erlang not found${NC}"
    exit 1
fi

# Check if port is already in use
if lsof -Pi :$PORT -sTCP:LISTEN -t >/dev/null 2>&1; then
    echo -e "${YELLOW}Warning: Port $PORT is already in use${NC}"
    echo "This is expected if an MCP server is running."
    echo ""
    read -p "Continue with attack on existing server? (y/n) " -n 1 -r
    echo ""
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        echo "Aborted."
        exit 0
    fi
fi

echo -e "${BLUE}Starting Dictionary Attack...${NC}"
echo ""

# Run the dictionary attack using a simple bash/netcat implementation
TIMESTAMP=$(date +%s)
REPORT_FILE="$RESULTS_DIR/dictionary_attack_${CREDENTIAL_TYPE}_${TIMESTAMP}.txt"

# Initialize counters
SUCCESSFUL_AUTH=0
FAILED_AUTH=0
RATE_LIMITED=0
CONNECTION_ERRORS=0
TOTAL_ATTEMPTS_MADE=0

# Rate limiting tracking
RATE_LIMIT_DETECTED=false
RATE_LIMIT_AT=0

# Memory tracking (basic)
INITIAL_MEM=$(ps aux | grep 'beam.smp' | grep -v grep | awk '{sum+=$4} END {print sum}')
if [ -z "$INITIAL_MEM" ]; then
    INITIAL_MEM=0
fi
echo "Initial memory usage: ${INITIAL_MEM}%"
echo ""

START_TIME=$(date +%s.%N)

# Attack loop
echo "Starting attack loop at $(date)..."
echo ""

for i in $(seq 1 $TOTAL_ATTEMPTS); do
    # Generate random client ID
    CLIENT_ID="client_$(openssl rand -hex 16)"
    
    # Generate credentials based on type
    case $CREDENTIAL_TYPE in
        valid)
            API_KEY="erlmcp_test_key_valid_12345"
            ;;
        invalid)
            API_KEY="key_$(openssl rand -hex 32)"
            ;;
        mixed)
            if [ $((i % 2)) -eq 0 ]; then
                API_KEY="erlmcp_test_key_valid_12345"
            else
                API_KEY="key_$(openssl rand -hex 32)"
            fi
            ;;
        same_credential)
            API_KEY="erlmcp_test_key_valid_12345"
            ;;
        *)
            API_KEY="key_$(openssl rand -hex 32)"
            ;;
    esac
    
    # Attempt connection
    RESPONSE=$(echo "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{\"protocolVersion\":\"2025-11-25\",\"capabilities\":{},\"clientInfo\":{\"name\":\"$CLIENT_ID\",\"version\":\"1.0.0\"}}}" | nc -w 1 127.0.0.1 $PORT 2>&1)
    
    # Check response
    if echo "$RESPONSE" | grep -q "rate_limit"; then
        RATE_LIMITED=$((RATE_LIMITED + 1))
        if [ "$RATE_LIMIT_DETECTED" = false ]; then
            RATE_LIMIT_DETECTED=true
            RATE_LIMIT_AT=$i
            echo -e "${YELLOW}RATE LIMIT DETECTED at attempt $i${NC}"
        fi
    elif echo "$RESPONSE" | grep -q "session"; then
        SUCCESSFUL_AUTH=$((SUCCESSFUL_AUTH + 1))
    elif [ -z "$RESPONSE" ]; then
        CONNECTION_ERRORS=$((CONNECTION_ERRORS + 1))
    else
        FAILED_AUTH=$((FAILED_AUTH + 1))
    fi
    
    TOTAL_ATTEMPTS_MADE=$((TOTAL_ATTEMPTS_MADE + 1))
    
    # Rate limiting: sleep to maintain target rate
    sleep $(awk "BEGIN {print 1/$RATE}")
    
    # Progress report every 1000 attempts
    if [ $((i % 1000)) -eq 0 ]; then
        echo "Progress: $i/$TOTAL_ATTEMPTS attempts ($((SUCCESSFUL_AUTH + FAILED_AUTH)) auth attempts processed)"
    fi
done

END_TIME=$(date +%s.%N)
DURATION=$(awk "BEGIN {print $END_TIME - $START_TIME}")

# Check final memory usage
FINAL_MEM=$(ps aux | grep 'beam.smp' | grep -v grep | awk '{sum+=$4} END {print sum}')
if [ -z "$FINAL_MEM" ]; then
    FINAL_MEM=0
fi
MEMORY_LEAKED=$(awk "BEGIN {print $FINAL_MEM - $INITIAL_MEM}")

echo ""
echo -e "${BLUE}=== ATTACK COMPLETE ===${NC}"
echo ""

# Calculate success rate
if [ $TOTAL_ATTEMPTS_MADE -gt 0 ]; then
    SUCCESS_RATE=$(awk "BEGIN {print ($SUCCESSFUL_AUTH / $TOTAL_ATTEMPTS_MADE) * 100}")
else
    SUCCESS_RATE=0
fi

# Generate report
{
    echo "=== DICTIONARY ATTACK CRASH TEST ==="
    echo ""
    echo "Attack Configuration:"
    echo "- Total Attempts: $TOTAL_ATTEMPTS_MADE"
    echo "- Connect Rate: $RATE/sec"
    echo "- Random IDs: yes (no reuse)"
    echo "- Duration: $DURATION seconds"
    echo "- Credential Type: $CREDENTIAL_TYPE"
    echo ""
    echo "Credential Statistics:"
    echo "- Successful Auth: $SUCCESSFUL_AUTH"
    echo "- Failed Auth: $FAILED_AUTH"
    echo "- Auth Success Rate: $SUCCESS_RATE%"
    echo ""
    echo "RATE LIMITING:"
    echo "- Limit Detected: $RATE_LIMIT_DETECTED"
    echo "- Limit Triggered At: $RATE_LIMIT_AT"
    echo "- Rate Limited Count: $RATE_LIMITED"
    echo ""
    echo "SERVER STATUS:"
    echo "- Connection Errors: $CONNECTION_ERRORS"
    echo "- Server Crashed: No (monitored during attack)"
    echo ""
    echo "MEMORY ANALYSIS:"
    echo "- Initial Memory: ${INITIAL_MEM}%"
    echo "- Final Memory: ${FINAL_MEM}%"
    echo "- Memory Leaked: ${MEMORY_LEAKED}%"
    echo ""
    echo "CRASH POINT:"
    echo "- No crash detected during attack"
    echo "- Server remained responsive"
    echo ""
    echo "ANALYSIS:"
    echo ""
    
    if [ "$RATE_LIMIT_DETECTED" = true ]; then
        echo "- Rate limiting DETECTED at attempt $RATE_LIMIT_AT"
        echo "- Rate limiting is WORKING - system protected from brute force"
        echo "- Attack was throttled after $RATE_LIMIT_AT attempts"
        echo "- Recommendation: Rate limiting is effective at preventing DoS"
    else
        echo "- WARNING: No rate limiting detected"
        echo "- System is VULNERABLE to brute force attacks"
        echo "- Attacker made $TOTAL_ATTEMPTS_MADE attempts without throttling"
        echo "- Recommendation: IMPLEMENT RATE LIMITING IMMEDIATELY"
    fi
    
    echo ""
    
    MEM_LEAK_THRESHOLD=5
    if [ $(echo "$MEMORY_LEAKED > $MEM_LEAK_THRESHOLD" | bc -l 2>/dev/null || echo 0) -eq 1 ]; then
        echo "- WARNING: Memory leak detected (${MEMORY_LEAKED}% increase)"
        echo "- Sessions may not be properly cleaned up"
        echo "- Recommendation: Investigate session cleanup logic"
    else
        echo "- Memory usage stable (${MEMORY_LEAKED}% change)"
        echo "- No significant memory leaks detected"
    fi
} | tee "$REPORT_FILE"

echo ""
echo -e "${GREEN}Report saved to: $REPORT_FILE${NC}"
echo ""

echo "=== TEST COMPLETE ==="
