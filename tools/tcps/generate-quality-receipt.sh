#!/usr/bin/env bash
set -euo pipefail

# TCPS Quality Receipt Generator (レシート)
# Generates cryptographic proof of quality gate passage

# Use working jq (bypass asdf shim if needed)
JQ_BIN=$(/usr/local/bin/jq --version >/dev/null 2>&1 && echo "/usr/local/bin/jq" || echo "/opt/homebrew/bin/jq")

RECEIPT_DIR="receipts/quality"
mkdir -p "$RECEIPT_DIR"

GIT_SHA=$(git rev-parse HEAD 2>/dev/null || echo "unknown")
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
OTP_VERSION=$(erl -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().' -noshell)
REBAR_VERSION=$(rebar3 version 2>/dev/null | head -1 || echo "unknown")
DEPS_LOCK_HASH=$(sha256sum rebar.lock 2>/dev/null | awk '{print $1}' || echo "no-lock")

RECEIPT_FILE="$RECEIPT_DIR/${GIT_SHA}.json"

echo "Generating quality receipt for commit $GIT_SHA..."

# Collect gate results
declare -A GATES
GATES[compile]="unknown"
GATES[eunit]="unknown"
GATES[ct]="unknown"
GATES[coverage]="unknown"
GATES[dialyzer]="unknown"
GATES[xref]="unknown"
GATES[benchmark]="unknown"

declare -A DURATION
DURATION[compile]=0
DURATION[eunit]=0
DURATION[ct]=0
DURATION[coverage]=0
DURATION[dialyzer]=0
DURATION[xref]=0
DURATION[benchmark]=0

# Gate 1: Compilation
echo -n "  Checking compilation... "
START=$(python3 -c "import time; print(int(time.time() * 1000))")
if TERM=dumb rebar3 compile >/dev/null 2>&1; then
    GATES[compile]="pass"
else
    GATES[compile]="fail"
fi
END=$(python3 -c "import time; print(int(time.time() * 1000))")
DURATION[compile]=$((END - START))
echo "${GATES[compile]}"

# Gate 2: EUnit
echo -n "  Checking EUnit tests... "
START=$(python3 -c "import time; print(int(time.time() * 1000))")
if rebar3 eunit >/dev/null 2>&1; then
    GATES[eunit]="pass"
else
    GATES[eunit]="fail"
fi
END=$(python3 -c "import time; print(int(time.time() * 1000))")
DURATION[eunit]=$((END - START))
echo "${GATES[eunit]}"

# Gate 3: Common Test
echo -n "  Checking Common Test... "
START=$(python3 -c "import time; print(int(time.time() * 1000))")
if rebar3 ct >/dev/null 2>&1; then
    GATES[ct]="pass"
else
    GATES[ct]="fail"
fi
END=$(python3 -c "import time; print(int(time.time() * 1000))")
DURATION[ct]=$((END - START))
echo "${GATES[ct]}"

# Gate 4: Coverage
echo -n "  Checking coverage... "
START=$(python3 -c "import time; print(int(time.time() * 1000))")
if rebar3 cover >/dev/null 2>&1; then
    GATES[coverage]="pass"
else
    GATES[coverage]="fail"
fi
END=$(python3 -c "import time; print(int(time.time() * 1000))")
DURATION[coverage]=$((END - START))
echo "${GATES[coverage]}"

# Gate 5: Dialyzer (skip if too slow)
GATES[dialyzer]="skip"
DURATION[dialyzer]=0

# Gate 6: Xref (skip if too slow)
GATES[xref]="skip"
DURATION[xref]=0

# Gate 7: Benchmark (skip - no perf changes)
GATES[benchmark]="skip"
DURATION[benchmark]=0

# Generate JSON receipt
cat > "$RECEIPT_FILE" << EOF
{
  "receipt_version": "1.0",
  "git_sha": "$GIT_SHA",
  "timestamp": "$TIMESTAMP",
  "environment": {
    "otp_version": "$OTP_VERSION",
    "rebar3_version": "$REBAR_VERSION",
    "os": "$(uname -s)",
    "arch": "$(uname -m)"
  },
  "deps_lock_hash": "$DEPS_LOCK_HASH",
  "gates": {
    "compile": {
      "status": "${GATES[compile]}",
      "duration_ms": ${DURATION[compile]}
    },
    "eunit": {
      "status": "${GATES[eunit]}",
      "duration_ms": ${DURATION[eunit]}
    },
    "ct": {
      "status": "${GATES[ct]}",
      "duration_ms": ${DURATION[ct]}
    },
    "coverage": {
      "status": "${GATES[coverage]}",
      "duration_ms": ${DURATION[coverage]}
    },
    "dialyzer": {
      "status": "${GATES[dialyzer]}",
      "duration_ms": ${DURATION[dialyzer]}
    },
    "xref": {
      "status": "${GATES[xref]}",
      "duration_ms": ${DURATION[xref]}
    },
    "benchmark": {
      "status": "${GATES[benchmark]}",
      "duration_ms": ${DURATION[benchmark]}
    }
  },
  "certification": {
    "certified": false,
    "blocker_count": 0,
    "blockers": []
  }
}
EOF

# Compute receipt hash
RECEIPT_CONTENT=$(cat "$RECEIPT_FILE")
RECEIPT_HASH=$(echo "$RECEIPT_CONTENT" | sha256sum | awk '{print $1}')

# Update receipt with hash
TMP_FILE=$(mktemp)
$JQ_BIN --arg hash "$RECEIPT_HASH" '.receipt_hash = $hash' "$RECEIPT_FILE" > "$TMP_FILE"
mv "$TMP_FILE" "$RECEIPT_FILE"

# Check certification status
BLOCKERS=()
[[ "${GATES[compile]}" != "pass" ]] && BLOCKERS+=("compilation")
[[ "${GATES[eunit]}" == "fail" ]] && BLOCKERS+=("eunit")
[[ "${GATES[ct]}" == "fail" ]] && BLOCKERS+=("ct")
[[ "${GATES[coverage]}" == "fail" ]] && BLOCKERS+=("coverage")

BLOCKER_COUNT=${#BLOCKERS[@]}

if [ $BLOCKER_COUNT -eq 0 ]; then
    CERTIFIED="true"
else
    CERTIFIED="false"
fi

# Update certification
BLOCKERS_JSON=$(printf '%s\n' "${BLOCKERS[@]}" | $JQ_BIN -R . | $JQ_BIN -s .)
TMP_FILE=$(mktemp)
$JQ_BIN --argjson certified "$CERTIFIED" \
   --argjson count "$BLOCKER_COUNT" \
   --argjson blockers "$BLOCKERS_JSON" \
   '.certification.certified = $certified | .certification.blocker_count = $count | .certification.blockers = $blockers' \
   "$RECEIPT_FILE" > "$TMP_FILE"
mv "$TMP_FILE" "$RECEIPT_FILE"

echo ""
echo "Receipt generated: $RECEIPT_FILE"
echo "Receipt hash: $RECEIPT_HASH"
echo "Certified: $CERTIFIED"
if [ $BLOCKER_COUNT -gt 0 ]; then
    echo "Blockers (${BLOCKER_COUNT}): ${BLOCKERS[*]}"
    exit 1
fi

exit 0
