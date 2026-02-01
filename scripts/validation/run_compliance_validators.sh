#!/usr/bin/env bash
###===================================================================
### run_compliance_validators.sh - MCP Compliance Validation Orchestrator
###===================================================================
###
### Orchestrates all 4 compliance validators and generates unified report.
###
### Usage:
###   ./scripts/validation/run_compliance_validators.sh [OPTIONS]
###
### Options:
###   --quick       Quick validation (protocol + transport only)
###   --full        Full validation (all 4 validators + stress tests)
###   --output=DIR  Output directory (default: reports/compliance/latest)
###   --format=FMT  Output format: json, html, markdown (default: json)
###   --archive     Archive results with timestamp
###   --help        Show this help message
###
### Exit Codes:
###   0 - All validators passed (compliance >= threshold)
###   1 - Validation failed or errors occurred
###   2 - Compliance below threshold (non-blocking warning)
###
### Example:
###   ./scripts/validation/run_compliance_validators.sh --full --output=reports/compliance/latest
###   ./scripts/validation/run_compliance_validators.sh --quick --format=html
###
###===================================================================

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$PROJECT_ROOT"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

# Default configuration
MODE="standard"
OUTPUT_DIR="reports/compliance/latest"
OUTPUT_FORMAT="json"
ARCHIVE_ENABLED=false
CONFIG_FILE=".claude/config/compliance_config.json"

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --quick)
            MODE="quick"
            shift
            ;;
        --full)
            MODE="full"
            shift
            ;;
        --output=*)
            OUTPUT_DIR="${1#*=}"
            shift
            ;;
        --format=*)
            OUTPUT_FORMAT="${1#*=}"
            shift
            ;;
        --archive)
            ARCHIVE_ENABLED=true
            shift
            ;;
        --help)
            grep "^###" "$0" | sed 's/^### \?//'
            exit 0
            ;;
        *)
            echo -e "${RED}Error: Unknown option: $1${NC}"
            exit 1
            ;;
    esac
done

# Load configuration
if [ -f "$CONFIG_FILE" ]; then
    echo -e "${BLUE}Loading configuration from $CONFIG_FILE${NC}"
    COMPLIANCE_THRESHOLD=$(jq -r '.thresholds.overall_compliance_percent // 80.0' "$CONFIG_FILE")
    ARCHIVE_RETENTION_DAYS=$(jq -r '.archive.retention_days // 90' "$CONFIG_FILE")
    ARCHIVE_MAX_COUNT=$(jq -r '.archive.max_archives // 50' "$CONFIG_FILE")
else
    echo -e "${YELLOW}Warning: Config file not found, using defaults${NC}"
    COMPLIANCE_THRESHOLD=80.0
    ARCHIVE_RETENTION_DAYS=90
    ARCHIVE_MAX_COUNT=50
fi

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  MCP Compliance Validation${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${CYAN}Mode:${NC}              $MODE"
echo -e "${CYAN}Output Directory:${NC}  $OUTPUT_DIR"
echo -e "${CYAN}Output Format:${NC}     $OUTPUT_FORMAT"
echo -e "${CYAN}Archive:${NC}           $ARCHIVE_ENABLED"
echo -e "${CYAN}Threshold:${NC}         ${COMPLIANCE_THRESHOLD}%"
echo ""

# Create output directory
mkdir -p "$OUTPUT_DIR"
TEMP_DIR="$OUTPUT_DIR/temp_$$"
mkdir -p "$TEMP_DIR"

# Ensure cleanup on exit
trap "rm -rf '$TEMP_DIR'" EXIT

# Ensure project is compiled
echo -e "${BLUE}[1/6] Compiling project...${NC}"
if TERM=dumb rebar3 compile > "$TEMP_DIR/compile.log" 2>&1; then
    echo -e "${GREEN}✓ Compilation successful${NC}"
else
    echo -e "${RED}✗ Compilation failed${NC}"
    cat "$TEMP_DIR/compile.log"
    exit 1
fi

# Determine which validators to run based on mode
case $MODE in
    quick)
        VALIDATORS=("protocol" "transport")
        echo -e "${CYAN}Running quick validation (protocol + transport)${NC}"
        ;;
    full)
        VALIDATORS=("protocol" "transport" "security" "performance")
        echo -e "${CYAN}Running full validation (all validators)${NC}"
        ;;
    *)
        VALIDATORS=("protocol" "transport" "security" "performance")
        echo -e "${CYAN}Running standard validation (all validators)${NC}"
        ;;
esac

# Run validators
echo ""
echo -e "${BLUE}[2/6] Running validators...${NC}"

VALIDATOR_RESULTS=()
VALIDATOR_EXIT_CODES=()

for validator in "${VALIDATORS[@]}"; do
    echo -e "${CYAN}  → Running ${validator} validator...${NC}"

    VALIDATOR_OUTPUT="$TEMP_DIR/${validator}_validator.json"

    # Run validator via Erlang
    if erl -pa _build/default/lib/*/ebin -noshell -eval "
        application:ensure_all_started(erlmcp_core),
        application:ensure_all_started(erlmcp_transports),
        application:ensure_all_started(erlmcp_validation),

        ValidatorModule = erlmcp_${validator}_validator,

        io:format(\"    Starting ~s...~n\", [ValidatorModule]),

        case ValidatorModule:start_link() of
            {ok, _Pid} -> ok;
            {error, {already_started, _}} -> ok;
            {error, Reason} ->
                io:format(\"    Failed to start validator: ~p~n\", [Reason]),
                halt(1)
        end,

        Result = case ValidatorModule:run() of
            {ok, R} -> R;
            R when is_map(R) -> R;
            Other ->
                io:format(\"    Unexpected result format: ~p~n\", [Other]),
                #{status => error, reason => unexpected_format, details => Other}
        end,

        Json = jsx:encode(Result, [{space, 1}, {indent, 2}]),
        file:write_file(\"$VALIDATOR_OUTPUT\", Json),

        io:format(\"    Completed ~s~n\", [ValidatorModule]),
        halt(0).
    " 2>&1 | tee "$TEMP_DIR/${validator}_validator.log"; then
        echo -e "${GREEN}    ✓ ${validator} validator completed${NC}"
        VALIDATOR_RESULTS+=("$VALIDATOR_OUTPUT")
        VALIDATOR_EXIT_CODES+=(0)
    else
        EXIT_CODE=$?
        echo -e "${YELLOW}    ⚠ ${validator} validator had issues (exit code: $EXIT_CODE)${NC}"
        # Create minimal error result
        echo "{\"status\":\"error\",\"validator\":\"${validator}\",\"exit_code\":$EXIT_CODE}" > "$VALIDATOR_OUTPUT"
        VALIDATOR_RESULTS+=("$VALIDATOR_OUTPUT")
        VALIDATOR_EXIT_CODES+=($EXIT_CODE)
    fi
done

# Combine validator results
echo ""
echo -e "${BLUE}[3/6] Combining validator results...${NC}"

COMBINED_RESULTS="$TEMP_DIR/combined_results.json"

# Build combined JSON structure
jq -n --slurpfile protocol "$TEMP_DIR/protocol_validator.json" \
      --slurpfile transport "$TEMP_DIR/transport_validator.json" \
      $([ -f "$TEMP_DIR/security_validator.json" ] && echo "--slurpfile security '$TEMP_DIR/security_validator.json'") \
      $([ -f "$TEMP_DIR/performance_validator.json" ] && echo "--slurpfile performance '$TEMP_DIR/performance_validator.json'") \
      '{
        spec_version: "2025-11-25",
        timestamp: (now | strftime("%Y-%m-%dT%H:%M:%SZ")),
        mode: $ENV.MODE,
        validators: {
          protocol: ($protocol[0] // {}),
          transport: ($transport[0] // {}),
          security: ($security[0] // {}),
          performance: ($performance[0] // {})
        }
      }' > "$COMBINED_RESULTS" 2>/dev/null || {
        # Fallback: create combined results without optional validators
        jq -n --slurpfile protocol "$TEMP_DIR/protocol_validator.json" \
              --slurpfile transport "$TEMP_DIR/transport_validator.json" \
              '{
                spec_version: "2025-11-25",
                timestamp: (now | strftime("%Y-%m-%dT%H:%M:%SZ")),
                mode: "'"$MODE"'",
                validators: {
                  protocol: ($protocol[0] // {}),
                  transport: ($transport[0] // {})
                }
              }' > "$COMBINED_RESULTS"
      }

echo -e "${GREEN}✓ Results combined${NC}"

# Calculate compliance score
echo ""
echo -e "${BLUE}[4/6] Calculating compliance score...${NC}"

COMPLIANCE_SCORE=$(erl -pa _build/default/lib/*/ebin -noshell -eval "
    application:ensure_all_started(erlmcp_validation),

    {ok, CombinedBin} = file:read_file(\"$COMBINED_RESULTS\"),
    Combined = jsx:decode(CombinedBin, [return_maps]),

    Validators = maps:get(<<\"validators\">>, Combined, #{}),

    % Calculate overall compliance
    ValidatorScores = maps:fold(fun(Name, Result, Acc) ->
        case maps:get(<<\"compliance_score\">>, Result, undefined) of
            undefined ->
                case maps:get(<<\"status\">>, Result, <<\"unknown\">>) of
                    <<\"passed\">> -> [100.0 | Acc];
                    <<\"error\">> -> [0.0 | Acc];
                    _ -> [50.0 | Acc]
                end;
            Score when is_number(Score) -> [Score | Acc];
            _ -> [0.0 | Acc]
        end
    end, [], Validators),

    ComplianceScore = case ValidatorScores of
        [] -> 0.0;
        Scores -> lists:sum(Scores) / length(Scores)
    end,

    io:format(\"~.2f\", [ComplianceScore]),
    halt(0).
" 2>/dev/null || echo "0.0")

echo -e "${CYAN}Compliance Score: ${COMPLIANCE_SCORE}%${NC}"

# Determine pass/fail
if awk "BEGIN {exit !($COMPLIANCE_SCORE >= $COMPLIANCE_THRESHOLD)}"; then
    COMPLIANCE_STATUS="passed"
    STATUS_COLOR=$GREEN
    STATUS_SYMBOL="✓"
else
    COMPLIANCE_STATUS="warning"
    STATUS_COLOR=$YELLOW
    STATUS_SYMBOL="⚠"
fi

echo -e "${STATUS_COLOR}${STATUS_SYMBOL} Compliance: $COMPLIANCE_STATUS (threshold: ${COMPLIANCE_THRESHOLD}%)${NC}"

# Generate final report
echo ""
echo -e "${BLUE}[5/6] Generating compliance report...${NC}"

REPORT_FILE="$OUTPUT_DIR/mcp_compliance.$OUTPUT_FORMAT"

case $OUTPUT_FORMAT in
    json)
        # Add compliance metadata to combined results
        jq --arg score "$COMPLIANCE_SCORE" \
           --arg status "$COMPLIANCE_STATUS" \
           --arg threshold "$COMPLIANCE_THRESHOLD" \
           '. + {
             compliance: {
               score: ($score | tonumber),
               status: $status,
               threshold: ($threshold | tonumber)
             }
           }' "$COMBINED_RESULTS" > "$REPORT_FILE"
        echo -e "${GREEN}✓ JSON report generated: $REPORT_FILE${NC}"
        ;;
    html)
        # Generate HTML report via Erlang
        erl -pa _build/default/lib/*/ebin -noshell -eval "
            application:ensure_all_started(erlmcp_validation),
            {ok, Bin} = file:read_file(\"$COMBINED_RESULTS\"),
            Data = jsx:decode(Bin, [return_maps]),
            {ok, Html} = erlmcp_compliance_report_html:generate_report(html, Data),
            file:write_file(\"$REPORT_FILE\", Html),
            halt(0).
        " 2>&1 | tee "$TEMP_DIR/html_gen.log" || {
            echo -e "${YELLOW}⚠ HTML generation failed, falling back to JSON${NC}"
            cp "$COMBINED_RESULTS" "$OUTPUT_DIR/mcp_compliance.json"
            REPORT_FILE="$OUTPUT_DIR/mcp_compliance.json"
        }
        echo -e "${GREEN}✓ HTML report generated: $REPORT_FILE${NC}"
        ;;
    markdown)
        # Generate markdown report
        {
            echo "# MCP Compliance Report"
            echo ""
            echo "**Generated:** $(date -u +"%Y-%m-%dT%H:%M:%SZ")"
            echo "**Mode:** $MODE"
            echo "**Compliance Score:** ${COMPLIANCE_SCORE}%"
            echo "**Status:** $COMPLIANCE_STATUS"
            echo ""
            echo "## Validator Results"
            echo ""
            for validator in "${VALIDATORS[@]}"; do
                if [ -f "$TEMP_DIR/${validator}_validator.json" ]; then
                    echo "### ${validator^} Validator"
                    echo ""
                    STATUS=$(jq -r '.status // "unknown"' "$TEMP_DIR/${validator}_validator.json")
                    echo "- **Status:** $STATUS"
                    if [ "$STATUS" != "error" ]; then
                        jq -r 'to_entries[] | "- **\(.key):** \(.value)"' "$TEMP_DIR/${validator}_validator.json" | head -10
                    fi
                    echo ""
                fi
            done
        } > "$REPORT_FILE"
        echo -e "${GREEN}✓ Markdown report generated: $REPORT_FILE${NC}"
        ;;
    *)
        echo -e "${RED}✗ Unknown format: $OUTPUT_FORMAT${NC}"
        exit 1
        ;;
esac

# Archive results if requested
echo ""
echo -e "${BLUE}[6/6] Archiving results...${NC}"

if [ "$ARCHIVE_ENABLED" = true ]; then
    ARCHIVE_DIR="reports/compliance/archive"
    mkdir -p "$ARCHIVE_DIR"

    TIMESTAMP=$(date -u +"%Y%m%d_%H%M%S")
    ARCHIVE_SUBDIR="$ARCHIVE_DIR/compliance_$TIMESTAMP"
    mkdir -p "$ARCHIVE_SUBDIR"

    # Copy all results to archive
    cp "$REPORT_FILE" "$ARCHIVE_SUBDIR/"
    cp "$TEMP_DIR"/*.json "$ARCHIVE_SUBDIR/" 2>/dev/null || true
    cp "$TEMP_DIR"/*.log "$ARCHIVE_SUBDIR/" 2>/dev/null || true

    echo -e "${GREEN}✓ Results archived to: $ARCHIVE_SUBDIR${NC}"

    # Cleanup old archives
    echo -e "${CYAN}  Cleaning up old archives...${NC}"

    # Remove archives older than retention days
    find "$ARCHIVE_DIR" -type d -name "compliance_*" -mtime +$ARCHIVE_RETENTION_DAYS -exec rm -rf {} \; 2>/dev/null || true

    # Keep only max count of recent archives
    ARCHIVE_COUNT=$(find "$ARCHIVE_DIR" -type d -name "compliance_*" | wc -l)
    if [ "$ARCHIVE_COUNT" -gt "$ARCHIVE_MAX_COUNT" ]; then
        EXCESS=$((ARCHIVE_COUNT - ARCHIVE_MAX_COUNT))
        find "$ARCHIVE_DIR" -type d -name "compliance_*" -printf '%T+ %p\n' | sort | head -n $EXCESS | cut -d' ' -f2- | xargs rm -rf
        echo -e "${CYAN}  Removed $EXCESS old archives${NC}"
    fi
else
    echo -e "${CYAN}Archiving disabled (use --archive to enable)${NC}"
fi

# Summary
echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo -e "${CYAN}Validators Run:${NC}     ${#VALIDATORS[@]}"
echo -e "${CYAN}Compliance Score:${NC}   ${COMPLIANCE_SCORE}%"
echo -e "${CYAN}Threshold:${NC}          ${COMPLIANCE_THRESHOLD}%"
echo -e "${CYAN}Status:${NC}             ${STATUS_COLOR}${COMPLIANCE_STATUS}${NC}"
echo -e "${CYAN}Report Location:${NC}    $REPORT_FILE"
echo ""

# Exit with appropriate code
if [ "$COMPLIANCE_STATUS" = "passed" ]; then
    echo -e "${GREEN}✓ All compliance checks passed!${NC}"
    exit 0
elif [ "$COMPLIANCE_STATUS" = "warning" ]; then
    echo -e "${YELLOW}⚠ Compliance below threshold (non-blocking warning)${NC}"
    exit 2
else
    echo -e "${RED}✗ Compliance validation failed${NC}"
    exit 1
fi
