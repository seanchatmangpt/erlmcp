# MCP Implementation Deployment Pipeline Design

**Version:** 1.0.0
**Created:** 2026-02-02
**Status:** DESIGN SPECIFICATION
**Target:** erlmcp v2.1.0 → v3.0.0 MCP Rollout

---

## Executive Summary

This document specifies the enhanced CI/CD pipeline for the MCP implementation rollout, supporting gradual feature deployment, canary releases, and zero-downtime upgrades across a 30-38 week implementation timeline.

**Key Enhancements:**
- **550+ new tests** integrated into existing 6-gate quality system
- **Feature flag infrastructure** for gradual rollout (per-phase, per-feature)
- **Canary deployment** with automated health checks and rollback
- **Zero-downtime upgrades** via hot code loading and blue-green deployment
- **Database migration pipeline** (ETS → DETS → Mnesia)
- **Multi-environment strategy** (dev, test, staging, canary, production)

---

## I. CI/CD ENHANCEMENTS FOR 550+ NEW TESTS

### A. Test Suite Expansion Strategy

#### 1. Current Baseline (v2.1.0)
```
Total Tests: 302
├── EUnit: 269 tests
├── CT Suites: 33 suites
├── Proper: ~10 property tests
└── Coverage: 75% overall, 85% core

CI Runtime: ~4 minutes (parallel gates)
Quality Gates: 6 (compile, xref, dialyzer, eunit, ct, coverage≥80%)
```

#### 2. Target State (v3.0.0)
```
Total Tests: 850+
├── EUnit: 350 tests (+81 tests)
├── CT Suites: 60 suites (+27 suites)
├── Proper: 60 property tests (+50 tests)
├── Compliance: 30 spec tests (+25 tests)
├── Benchmarks: 30 perf tests (+15 tests)
└── Chaos: 20 resilience tests (+15 tests)

CI Runtime Target: ≤8 minutes (with optimizations)
Quality Gates: 8 (add compliance, benchmark regression)
Coverage Target: 85% overall, 90% core
```

#### 3. Phased Test Integration

**Phase 1: Sampling + Core Fixes (8-10 weeks)**
```yaml
New Tests: +150
├── EUnit: +30 (sampling_tests, message_priority_tests)
├── CT: +10 (sampling_integration_SUITE, server_enhanced_SUITE)
├── Proper: +15 (sampling_proper_tests, icon_cache_proper_tests)
└── Compliance: +10 (mcp_sampling_compliance_tests)

CI Impact: +90 seconds
Gate Changes: Add sampling compliance gate (non-blocking initially)
```

**Phase 2: Tasks + Elicitation (10-12 weeks)**
```yaml
New Tests: +180
├── EUnit: +35 (tasks_api_tests, elicitation_tests, context_metadata_tests)
├── CT: +12 (tasks_integration_SUITE, elicitation_SUITE)
├── Proper: +20 (tasks_proper_tests, elicitation_proper_tests)
└── Compliance: +10 (tasks_compliance_tests, elicitation_compliance_tests)

CI Impact: +120 seconds
Gate Changes: Promote sampling gate to blocking, add tasks/elicitation gates (non-blocking)
```

**Phase 3: Optimizations (8-10 weeks)**
```yaml
New Tests: +120
├── Benchmarks: +15 (streaming_bench, parallel_bench, SONA_bench)
├── CT: +5 (performance_regression_SUITE)
├── Proper: +10 (optimization_proper_tests)
└── Chaos: +15 (backpressure_chaos, circuit_breaker_chaos)

CI Impact: +60 seconds (benchmarks run conditionally)
Gate Changes: Add performance regression gate (block on >10% degradation)
```

**Phase 4: Advanced Features (4-6 weeks)**
```yaml
New Tests: +100
├── EUnit: +16 (advanced_features_tests)
├── CT: +5 (advanced_integration_SUITE)
├── Proper: +5 (advanced_proper_tests)
└── Compliance: +5 (full_spec_compliance_tests)

CI Impact: +30 seconds
Gate Changes: Promote all compliance gates to blocking, final spec certification
```

### B. Enhanced CI/CD Pipeline Architecture

#### 1. Multi-Stage Pipeline Design

```yaml
# .github/workflows/mcp-ci-enhanced.yml

name: MCP Enhanced CI/CD

on:
  push:
    branches: [main, 'release/**', 'mcp/**']
  pull_request:
    branches: [main, 'release/**']

env:
  OTP_VERSION: 28
  REBAR3_VERSION: 3.25
  FEATURE_FLAGS_CONFIG: config/feature_flags.json

jobs:
  # ========================================================================
  # STAGE 1: FAST FEEDBACK (Target: 2 min)
  # ========================================================================
  fast-feedback:
    name: Fast Feedback Loop
    runs-on: ubuntu-22.04
    timeout-minutes: 5
    outputs:
      changed_apps: ${{ steps.detect-changes.outputs.apps }}
      test_tier: ${{ steps.detect-changes.outputs.tier }}

    steps:
    - name: Checkout
      uses: actions/checkout@v4
      with:
        fetch-depth: 0  # Full history for change detection

    - name: Detect changed applications
      id: detect-changes
      run: |
        # Smart change detection for incremental testing
        CHANGED_FILES=$(git diff --name-only origin/main...HEAD)

        # Determine which apps changed
        CHANGED_APPS=""
        echo "$CHANGED_FILES" | grep "^apps/erlmcp_core" && CHANGED_APPS="$CHANGED_APPS core"
        echo "$CHANGED_FILES" | grep "^apps/erlmcp_transports" && CHANGED_APPS="$CHANGED_APPS transports"
        echo "$CHANGED_FILES" | grep "^apps/erlmcp_observability" && CHANGED_APPS="$CHANGED_APPS observability"
        echo "$CHANGED_FILES" | grep "^apps/erlmcp_validation" && CHANGED_APPS="$CHANGED_APPS validation"

        # Determine test tier (smoke/quick/full)
        if echo "$CHANGED_FILES" | grep -E "(test/|_tests\.erl|_SUITE\.erl)"; then
          echo "tier=full" >> $GITHUB_OUTPUT
        elif [ -n "$CHANGED_APPS" ]; then
          echo "tier=quick" >> $GITHUB_OUTPUT
        else
          echo "tier=smoke" >> $GITHUB_OUTPUT
        fi

        echo "apps=$CHANGED_APPS" >> $GITHUB_OUTPUT
        echo "Changed apps: $CHANGED_APPS"

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ env.OTP_VERSION }}
        rebar3-version: ${{ env.REBAR3_VERSION }}

    - name: Cache build
      uses: actions/cache@v3
      with:
        path: |
          _build
          ~/.cache/rebar3
        key: otp${{ env.OTP_VERSION }}-${{ hashFiles('rebar.lock') }}
        restore-keys: |
          otp${{ env.OTP_VERSION }}-

    - name: Fast compile check
      run: TERM=dumb rebar3 compile

    - name: Smoke tests (changed apps only)
      run: |
        if [ -n "${{ steps.detect-changes.outputs.apps }}" ]; then
          for app in ${{ steps.detect-changes.outputs.apps }}; do
            echo "Running smoke tests for $app..."
            rebar3 eunit --app=erlmcp_$app --suite=*_smoke_*
          done
        else
          echo "No app changes, running core smoke tests"
          rebar3 eunit --module=erlmcp_json_rpc_tests
        fi

  # ========================================================================
  # STAGE 2: QUALITY GATES (Target: 6 min, parallel)
  # ========================================================================
  quality-gates:
    name: Quality Gates
    needs: fast-feedback
    runs-on: ubuntu-22.04
    timeout-minutes: 15
    strategy:
      fail-fast: false
      matrix:
        gate: [compile, xref, dialyzer, eunit, ct, coverage, compliance, benchmarks]

    steps:
    - name: Checkout
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ env.OTP_VERSION }}
        rebar3-version: ${{ env.REBAR3_VERSION }}

    - name: Restore cache
      uses: actions/cache@v3
      with:
        path: |
          _build
          ~/.cache/rebar3
        key: otp${{ env.OTP_VERSION }}-${{ hashFiles('rebar.lock') }}

    - name: Run gate - ${{ matrix.gate }}
      id: gate
      run: |
        case "${{ matrix.gate }}" in
          compile)
            TERM=dumb rebar3 compile
            ;;
          xref)
            rebar3 xref
            ;;
          dialyzer)
            rebar3 dialyzer
            ;;
          eunit)
            # Incremental EUnit based on changes
            if [ "${{ needs.fast-feedback.outputs.test_tier }}" = "full" ]; then
              rebar3 eunit --cover
            else
              # Run only changed app tests
              for app in ${{ needs.fast-feedback.outputs.changed_apps }}; do
                rebar3 eunit --app=erlmcp_$app --cover
              done
            fi
            ;;
          ct)
            # Selective CT suites
            if [ "${{ needs.fast-feedback.outputs.test_tier }}" = "full" ]; then
              rebar3 ct --cover
            else
              echo "Skipping CT for quick tier"
            fi
            ;;
          coverage)
            rebar3 cover --verbose
            ./scripts/check_coverage_threshold.sh 80
            ;;
          compliance)
            # MCP spec compliance tests
            if [ -f ./bin/erlmcp-validate ]; then
              ./bin/erlmcp-validate run --compliance --phase=${{ github.event.pull_request.labels.*.name || 'phase1' }}
            else
              echo "Compliance validator not built, skipping"
            fi
            ;;
          benchmarks)
            # Quick regression check (skip on PRs unless performance label)
            if [[ "${{ github.event_name }}" == "push" ]] || [[ "${{ contains(github.event.pull_request.labels.*.name, 'performance') }}" == "true" ]]; then
              ./scripts/bench/regression_check.sh --quick
            else
              echo "Skipping benchmarks for PR (add 'performance' label to run)"
            fi
            ;;
        esac

    - name: Upload coverage
      if: matrix.gate == 'coverage'
      uses: actions/upload-artifact@v3
      with:
        name: coverage-report
        path: _build/test/cover/

    - name: Upload compliance report
      if: matrix.gate == 'compliance'
      uses: actions/upload-artifact@v3
      with:
        name: compliance-report
        path: _build/test/compliance/

  # ========================================================================
  # STAGE 3: INTEGRATION TESTS (Target: 4 min)
  # ========================================================================
  integration:
    name: Integration Tests
    needs: quality-gates
    runs-on: ubuntu-22.04
    timeout-minutes: 10
    services:
      # Mnesia cluster for integration tests
      mnesia-node1:
        image: erlang:28-alpine
        ports:
          - 4369:4369
      mnesia-node2:
        image: erlang:28-alpine
        ports:
          - 4370:4369

    steps:
    - name: Checkout
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ env.OTP_VERSION }}
        rebar3-version: ${{ env.REBAR3_VERSION }}

    - name: Run integration test suites
      run: |
        # Phase-specific integration tests
        PHASE="${{ github.event.pull_request.labels.*.name || 'phase1' }}"

        case "$PHASE" in
          phase1|sampling)
            rebar3 ct --suite=sampling_integration_SUITE,server_enhanced_SUITE
            ;;
          phase2|tasks|elicitation)
            rebar3 ct --suite=tasks_integration_SUITE,elicitation_SUITE
            ;;
          phase3|optimization)
            rebar3 ct --suite=performance_regression_SUITE,streaming_SUITE
            ;;
          phase4|advanced)
            rebar3 ct --suite=advanced_integration_SUITE,full_spec_SUITE
            ;;
          *)
            rebar3 ct --dir=test/integration
            ;;
        esac

    - name: Upload CT logs
      if: always()
      uses: actions/upload-artifact@v3
      with:
        name: ct-logs
        path: _build/test/logs/

  # ========================================================================
  # STAGE 4: FEATURE FLAG VALIDATION
  # ========================================================================
  feature-flags:
    name: Feature Flag Validation
    needs: integration
    runs-on: ubuntu-22.04
    timeout-minutes: 8

    strategy:
      matrix:
        feature_set:
          - { name: "baseline", flags: "all_disabled" }
          - { name: "phase1", flags: "sampling:enabled" }
          - { name: "phase2", flags: "sampling:enabled,tasks:enabled,elicitation:enabled" }
          - { name: "phase3", flags: "sampling:enabled,tasks:enabled,elicitation:enabled,optimizations:enabled" }
          - { name: "phase4", flags: "all_enabled" }

    steps:
    - name: Checkout
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ env.OTP_VERSION }}
        rebar3-version: ${{ env.REBAR3_VERSION }}

    - name: Configure feature flags
      run: |
        echo "Testing with feature set: ${{ matrix.feature_set.name }}"
        export ERLMCP_FEATURE_FLAGS="${{ matrix.feature_set.flags }}"
        echo "ERLMCP_FEATURE_FLAGS=$ERLMCP_FEATURE_FLAGS" >> $GITHUB_ENV

    - name: Run smoke tests with feature flags
      run: |
        # Run subset of tests to validate feature flag behavior
        rebar3 eunit --suite=feature_flag_tests
        rebar3 ct --suite=feature_flag_integration_SUITE

    - name: Validate feature flag isolation
      run: |
        # Ensure disabled features don't leak into API
        ./scripts/validate_feature_isolation.sh "${{ matrix.feature_set.flags }}"

  # ========================================================================
  # STAGE 5: BUILD ARTIFACTS
  # ========================================================================
  build-artifacts:
    name: Build Release Artifacts
    needs: [quality-gates, integration, feature-flags]
    runs-on: ubuntu-22.04
    timeout-minutes: 10

    steps:
    - name: Checkout
      uses: actions/checkout@v4

    - name: Setup Erlang/OTP
      uses: erlef/setup-beam@v1
      with:
        otp-version: ${{ env.OTP_VERSION }}
        rebar3-version: ${{ env.REBAR3_VERSION }}

    - name: Build production release
      run: rebar3 as prod release

    - name: Generate release metadata
      run: |
        cat > _build/prod/rel/erlmcp/release_metadata.json << EOF
        {
          "version": "$(grep '{vsn,' apps/erlmcp_core/src/erlmcp_core.app.src | sed 's/.*"\(.*\)".*/\1/')",
          "git_sha": "${{ github.sha }}",
          "build_date": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
          "feature_flags": $(cat config/feature_flags.json),
          "otp_version": "${{ env.OTP_VERSION }}",
          "compliance_level": "$(./bin/erlmcp-validate --version | grep 'Compliance' || echo 'unknown')"
        }
        EOF

    - name: Create release tarball
      run: |
        cd _build/prod/rel
        tar -czf erlmcp-${{ github.sha }}.tar.gz erlmcp/
        sha256sum erlmcp-${{ github.sha }}.tar.gz > erlmcp-${{ github.sha }}.tar.gz.sha256

    - name: Upload release artifact
      uses: actions/upload-artifact@v3
      with:
        name: erlmcp-release
        path: |
          _build/prod/rel/erlmcp-${{ github.sha }}.tar.gz
          _build/prod/rel/erlmcp-${{ github.sha }}.tar.gz.sha256
        retention-days: 30

  # ========================================================================
  # STAGE 6: GATE SUMMARY
  # ========================================================================
  gate-summary:
    name: Quality Gate Summary
    needs: [fast-feedback, quality-gates, integration, feature-flags, build-artifacts]
    runs-on: ubuntu-22.04
    if: always()

    steps:
    - name: Generate summary
      run: |
        cat << 'EOF' >> $GITHUB_STEP_SUMMARY
        # MCP CI/CD Pipeline Summary

        ## Pipeline Results

        | Stage | Status | Duration |
        |-------|--------|----------|
        | Fast Feedback | ${{ needs.fast-feedback.result }} | ~2 min |
        | Quality Gates | ${{ needs.quality-gates.result }} | ~6 min |
        | Integration Tests | ${{ needs.integration.result }} | ~4 min |
        | Feature Flag Validation | ${{ needs.feature-flags.result }} | ~8 min |
        | Build Artifacts | ${{ needs.build-artifacts.result }} | ~2 min |

        **Total Runtime:** ~20 minutes (parallel execution)

        ## Changed Components
        - **Apps:** ${{ needs.fast-feedback.outputs.changed_apps || 'none' }}
        - **Test Tier:** ${{ needs.fast-feedback.outputs.test_tier }}

        ## Next Steps
        - ✅ All gates passed → Ready for merge
        - ⚠️ Warnings present → Review before merge
        - ❌ Gates failed → Fix and re-push

        ## Artifacts
        - Coverage Report: [Download](../artifacts/coverage-report)
        - Compliance Report: [Download](../artifacts/compliance-report)
        - Release Tarball: [Download](../artifacts/erlmcp-release)
        EOF

    - name: Check blocking gates
      run: |
        BLOCKING_FAILED=false

        # Check each blocking gate
        if [ "${{ needs.quality-gates.result }}" != "success" ]; then
          echo "::error::Quality gates FAILED - merge BLOCKED"
          BLOCKING_FAILED=true
        fi

        if [ "${{ needs.integration.result }}" != "success" ]; then
          echo "::error::Integration tests FAILED - merge BLOCKED"
          BLOCKING_FAILED=true
        fi

        if [ "$BLOCKING_FAILED" = "true" ]; then
          exit 1
        fi

        echo "✅ All blocking gates PASSED"
```

### C. Incremental Testing Strategy

#### 1. Smart Change Detection

**Algorithm:**
```bash
#!/bin/bash
# scripts/ci/detect_test_scope.sh

# Input: Git diff between base and HEAD
BASE_REF="${1:-origin/main}"
HEAD_REF="${2:-HEAD}"

# Analyze changed files
CHANGED_FILES=$(git diff --name-only "$BASE_REF...$HEAD_REF")

# Initialize test scope
TEST_SCOPE="smoke"  # Default: minimal
CHANGED_APPS=()

# Detect app changes
for app in erlmcp_core erlmcp_transports erlmcp_observability erlmcp_validation; do
  if echo "$CHANGED_FILES" | grep -q "apps/$app/src"; then
    CHANGED_APPS+=("$app")
    TEST_SCOPE="quick"  # Upgrade to quick tests
  fi
done

# Detect test file changes
if echo "$CHANGED_FILES" | grep -qE "(test/|_tests\.erl|_SUITE\.erl)"; then
  TEST_SCOPE="full"  # Upgrade to full test suite
fi

# Detect critical path changes (always run full suite)
CRITICAL_PATHS=(
  "apps/erlmcp_core/src/erlmcp_server.erl"
  "apps/erlmcp_core/src/erlmcp_client.erl"
  "apps/erlmcp_core/src/erlmcp_json_rpc.erl"
  "apps/erlmcp_core/src/erlmcp_registry.erl"
)

for path in "${CRITICAL_PATHS[@]}"; do
  if echo "$CHANGED_FILES" | grep -q "$path"; then
    TEST_SCOPE="full"
    break
  fi
done

# Output results
echo "TEST_SCOPE=$TEST_SCOPE"
echo "CHANGED_APPS=${CHANGED_APPS[*]}"

# Generate test commands
case "$TEST_SCOPE" in
  smoke)
    echo "TEST_CMD=rebar3 eunit --module=erlmcp_json_rpc_tests"
    ;;
  quick)
    echo "TEST_CMD=rebar3 eunit --app=${CHANGED_APPS[*]// /,}"
    ;;
  full)
    echo "TEST_CMD=rebar3 do eunit --cover, ct --cover"
    ;;
esac
```

**Benefits:**
- **50% CI time reduction** for minor changes
- **Early failure detection** (2 min vs 20 min)
- **Resource optimization** (pay only for what changed)

#### 2. Test Sharding for Parallel Execution

**Strategy:**
```yaml
# Shard large test suites across multiple runners

strategy:
  matrix:
    shard: [1, 2, 3, 4]  # 4 parallel shards

steps:
  - name: Run EUnit shard ${{ matrix.shard }}
    run: |
      # Divide tests into 4 equal shards
      TOTAL_MODULES=$(rebar3 eunit --list | wc -l)
      SHARD_SIZE=$((TOTAL_MODULES / 4))
      START=$(((${{ matrix.shard }} - 1) * SHARD_SIZE + 1))
      END=$((${{ matrix.shard }} * SHARD_SIZE))

      rebar3 eunit --module=$(rebar3 eunit --list | sed -n "${START},${END}p" | tr '\n' ',')
```

**Expected Speedup:**
- EUnit: 4 min → 1 min (4x parallelism)
- CT: 8 min → 2 min (4x parallelism)
- **Total CI: 20 min → 8 min** (target achieved)

---

## II. FEATURE FLAG INFRASTRUCTURE

### A. Architecture Design

#### 1. Feature Flag Service

**Module:** `erlmcp_feature_flags.erl`

```erlang
-module(erlmcp_feature_flags).
-behaviour(gen_server).

%% API
-export([start_link/0, is_enabled/1, is_enabled/2, enable/1, disable/1,
         get_all/0, reload/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    flags :: #{binary() => flag_config()},
    reload_timer :: reference()
}).

-type flag_config() :: #{
    enabled := boolean(),
    rollout_percentage := 0..100,
    environments := [atom()],
    conditions := [condition()],
    metadata := map()
}.

-type condition() ::
    {user_id, binary()} |
    {session_type, atom()} |
    {custom, fun((map()) -> boolean())}.

%%% API Functions

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Check if feature is enabled for context
-spec is_enabled(binary(), map()) -> boolean().
is_enabled(FeatureName, Context) ->
    gen_server:call(?MODULE, {is_enabled, FeatureName, Context}).

%% Check if feature is globally enabled
-spec is_enabled(binary()) -> boolean().
is_enabled(FeatureName) ->
    is_enabled(FeatureName, #{}).

%% Runtime enable/disable (testing only)
-spec enable(binary()) -> ok.
enable(FeatureName) ->
    gen_server:call(?MODULE, {enable, FeatureName}).

-spec disable(binary()) -> ok.
disable(FeatureName) ->
    gen_server:call(?MODULE, {disable, FeatureName}).

%% Get all flag states
-spec get_all() -> #{binary() => boolean()}.
get_all() ->
    gen_server:call(?MODULE, get_all).

%% Reload flags from config
-spec reload() -> ok.
reload() ->
    gen_server:cast(?MODULE, reload).

%%% gen_server callbacks

init([]) ->
    % Load flags from config file
    ConfigFile = application:get_env(erlmcp_core, feature_flags_config,
                                      "config/feature_flags.json"),
    {ok, Flags} = load_flags(ConfigFile),

    % Set up periodic reload (every 60s)
    Timer = erlang:send_after(60000, self(), reload),

    {ok, #state{flags = Flags, reload_timer = Timer}}.

handle_call({is_enabled, FeatureName, Context}, _From, State) ->
    Result = evaluate_flag(FeatureName, Context, State#state.flags),
    {reply, Result, State};

handle_call({enable, FeatureName}, _From, State) ->
    NewFlags = update_flag(FeatureName, #{enabled => true}, State#state.flags),
    {reply, ok, State#state{flags = NewFlags}};

handle_call({disable, FeatureName}, _From, State) ->
    NewFlags = update_flag(FeatureName, #{enabled => false}, State#state.flags),
    {reply, ok, State#state{flags = NewFlags}};

handle_call(get_all, _From, State) ->
    Result = maps:map(fun(_K, V) -> maps:get(enabled, V, false) end,
                       State#state.flags),
    {reply, Result, State}.

handle_cast(reload, State) ->
    ConfigFile = application:get_env(erlmcp_core, feature_flags_config,
                                      "config/feature_flags.json"),
    case load_flags(ConfigFile) of
        {ok, NewFlags} ->
            logger:info("Feature flags reloaded: ~p", [maps:keys(NewFlags)]),
            {noreply, State#state{flags = NewFlags}};
        {error, Reason} ->
            logger:warning("Failed to reload flags: ~p", [Reason]),
            {noreply, State}
    end.

handle_info(reload, State) ->
    % Periodic reload
    gen_server:cast(self(), reload),
    Timer = erlang:send_after(60000, self(), reload),
    {noreply, State#state{reload_timer = Timer}}.

%%% Internal Functions

load_flags(ConfigFile) ->
    case file:read_file(ConfigFile) of
        {ok, Data} ->
            {ok, jsx:decode(Data, [return_maps])};
        {error, Reason} ->
            logger:error("Failed to load feature flags from ~s: ~p",
                         [ConfigFile, Reason]),
            {error, Reason}
    end.

evaluate_flag(FeatureName, Context, Flags) ->
    case maps:get(FeatureName, Flags, undefined) of
        undefined ->
            false;  % Unknown flags default to disabled
        FlagConfig ->
            % Check global enabled
            case maps:get(enabled, FlagConfig, false) of
                false -> false;
                true ->
                    % Check environment
                    Env = application:get_env(erlmcp_core, environment, dev),
                    EnvAllowed = lists:member(Env, maps:get(environments, FlagConfig, [Env])),

                    % Check rollout percentage
                    RolloutPct = maps:get(rollout_percentage, FlagConfig, 100),
                    InRollout = check_rollout(Context, RolloutPct),

                    % Check conditions
                    Conditions = maps:get(conditions, FlagConfig, []),
                    ConditionsMet = check_conditions(Conditions, Context),

                    EnvAllowed andalso InRollout andalso ConditionsMet
            end
    end.

check_rollout(Context, 100) ->
    true;
check_rollout(Context, Percentage) when Percentage >= 0, Percentage =< 100 ->
    % Consistent hashing based on session_id
    SessionId = maps:get(session_id, Context, <<>>),
    Hash = erlang:phash2(SessionId, 100),
    Hash < Percentage;
check_rollout(_, _) ->
    false.

check_conditions([], _Context) ->
    true;
check_conditions([{user_id, UserId} | Rest], Context) ->
    case maps:get(user_id, Context, undefined) of
        UserId -> check_conditions(Rest, Context);
        _ -> false
    end;
check_conditions([{session_type, Type} | Rest], Context) ->
    case maps:get(session_type, Context, undefined) of
        Type -> check_conditions(Rest, Context);
        _ -> false
    end;
check_conditions([{custom, Fun} | Rest], Context) ->
    case Fun(Context) of
        true -> check_conditions(Rest, Context);
        false -> false
    end.

update_flag(FeatureName, Updates, Flags) ->
    ExistingConfig = maps:get(FeatureName, Flags, #{}),
    NewConfig = maps:merge(ExistingConfig, Updates),
    maps:put(FeatureName, NewConfig, Flags).
```

#### 2. Feature Flag Configuration

**File:** `config/feature_flags.json`

```json
{
  "mcp_sampling": {
    "enabled": true,
    "rollout_percentage": 100,
    "environments": ["dev", "test", "staging", "production"],
    "conditions": [],
    "metadata": {
      "phase": 1,
      "description": "MCP 2025-11-25 sampling support",
      "tracking_issue": "https://github.com/org/erlmcp/issues/100"
    }
  },
  "mcp_tasks": {
    "enabled": false,
    "rollout_percentage": 0,
    "environments": ["dev", "test"],
    "conditions": [],
    "metadata": {
      "phase": 2,
      "description": "Experimental Tasks API",
      "tracking_issue": "https://github.com/org/erlmcp/issues/101"
    }
  },
  "mcp_elicitation": {
    "enabled": false,
    "rollout_percentage": 0,
    "environments": ["dev", "test"],
    "conditions": [],
    "metadata": {
      "phase": 2,
      "description": "Elicitation for dynamic inputs",
      "tracking_issue": "https://github.com/org/erlmcp/issues/102"
    }
  },
  "mcp_streaming_optimizations": {
    "enabled": false,
    "rollout_percentage": 0,
    "environments": ["dev"],
    "conditions": [],
    "metadata": {
      "phase": 3,
      "description": "Streaming and parallel optimizations",
      "tracking_issue": "https://github.com/org/erlmcp/issues/103"
    }
  },
  "mcp_sona_hybrid": {
    "enabled": false,
    "rollout_percentage": 0,
    "environments": ["dev"],
    "conditions": [],
    "metadata": {
      "phase": 4,
      "description": "SONA hybrid architecture (Erlang + Rust)",
      "tracking_issue": "https://github.com/org/erlmcp/issues/104"
    }
  }
}
```

#### 3. Gradual Rollout Strategy

**Phase 1: Sampling (Weeks 1-10)**
```json
{
  "mcp_sampling": {
    "week_1": {"enabled": true, "rollout_percentage": 10, "environments": ["dev"]},
    "week_2": {"enabled": true, "rollout_percentage": 25, "environments": ["dev"]},
    "week_4": {"enabled": true, "rollout_percentage": 100, "environments": ["dev", "test"]},
    "week_6": {"enabled": true, "rollout_percentage": 10, "environments": ["staging"]},
    "week_8": {"enabled": true, "rollout_percentage": 50, "environments": ["staging"]},
    "week_10": {"enabled": true, "rollout_percentage": 100, "environments": ["production"]}
  }
}
```

**Rollout Gates:**
1. **Dev → Test:** All EUnit + CT pass, coverage ≥80%
2. **Test → Staging:** Compliance tests pass, no regressions
3. **Staging → Canary:** Health checks pass for 48 hours
4. **Canary → Production:** Error rate <0.1%, latency P95 <10ms

---

## III. CANARY DEPLOYMENT PROCESS

### A. Canary Deployment Architecture

```
Production Traffic (100%)
         │
         ├─────────────────────────────────────┐
         │                                     │
    Traffic Split                         Traffic Split
    (e.g., 95% / 5%)                      (e.g., 90% / 10%)
         │                                     │
    ┌────┴────┐                           ┌────┴────┐
    │         │                           │         │
 Stable    Canary                      Stable    Canary
 (v2.1.0)  (v2.2.0)                    (v2.1.0)  (v2.2.0)
    │         │                           │         │
    └─────────┴────────> Metrics ────────┴─────────┘
                            │
                    ┌───────┴───────┐
                    │  Auto-Decision │
                    │   Engine       │
                    └───────┬────────┘
                            │
              ┌─────────────┼─────────────┐
              │             │             │
         Promote       Monitor        Rollback
         to 100%       (Continue)     to Stable
```

### B. Canary Deployment Workflow

**File:** `.github/workflows/canary-deployment.yml`

```yaml
name: Canary Deployment

on:
  workflow_dispatch:
    inputs:
      version:
        description: 'Release version to deploy'
        required: true
      canary_percentage:
        description: 'Initial canary traffic percentage'
        required: false
        default: '5'
      duration_minutes:
        description: 'Canary observation duration (minutes)'
        required: false
        default: '60'

env:
  CANARY_NAMESPACE: erlmcp-canary
  STABLE_NAMESPACE: erlmcp
  HEALTH_CHECK_INTERVAL: 30  # seconds
  ERROR_RATE_THRESHOLD: 0.001  # 0.1%
  LATENCY_P95_THRESHOLD: 10  # milliseconds

jobs:
  # ========================================================================
  # STEP 1: Pre-deployment Validation
  # ========================================================================
  pre-deployment:
    name: Pre-deployment Validation
    runs-on: ubuntu-22.04
    outputs:
      baseline_metrics: ${{ steps.capture-baseline.outputs.metrics }}

    steps:
    - name: Checkout
      uses: actions/checkout@v4
      with:
        ref: ${{ github.event.inputs.version }}

    - name: Validate release artifact
      run: |
        # Verify release tarball exists and checksum matches
        TARBALL="erlmcp-${{ github.event.inputs.version }}.tar.gz"
        if [ ! -f "$TARBALL" ]; then
          echo "::error::Release tarball not found: $TARBALL"
          exit 1
        fi

        sha256sum -c "$TARBALL.sha256" || exit 1
        echo "✅ Release artifact validated"

    - name: Capture baseline metrics
      id: capture-baseline
      run: |
        # Query Prometheus for current production metrics
        PROMETHEUS_URL="${{ secrets.PROMETHEUS_URL }}"

        # Capture 1-hour baseline
        ERROR_RATE=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(erlmcp_requests_failed_total[1h])")
        LATENCY_P95=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[1h]))")
        THROUGHPUT=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(erlmcp_requests_total[1h])")

        METRICS=$(cat <<EOF
        {
          "error_rate": $ERROR_RATE,
          "latency_p95": $LATENCY_P95,
          "throughput": $THROUGHPUT,
          "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
        }
        EOF
        )

        echo "metrics=$METRICS" >> $GITHUB_OUTPUT
        echo "Baseline metrics captured"

    - name: Run pre-deployment smoke tests
      run: |
        # Build release and run smoke tests
        rebar3 as prod release

        # Start release in test mode
        _build/prod/rel/erlmcp/bin/erlmcp daemon
        sleep 5

        # Run smoke tests
        ./scripts/smoke_test.sh http://localhost:8080

        # Stop release
        _build/prod/rel/erlmcp/bin/erlmcp stop

  # ========================================================================
  # STEP 2: Deploy Canary
  # ========================================================================
  deploy-canary:
    name: Deploy Canary Release
    needs: pre-deployment
    runs-on: ubuntu-22.04
    environment:
      name: canary
      url: https://canary.erlmcp.example.com

    steps:
    - name: Setup kubectl
      uses: azure/setup-kubectl@v3

    - name: Configure kubectl
      run: |
        echo "${{ secrets.KUBE_CONFIG }}" | base64 -d > $HOME/.kube/config
        kubectl config use-context production

    - name: Create canary deployment
      run: |
        # Create canary namespace if not exists
        kubectl create namespace ${{ env.CANARY_NAMESPACE }} --dry-run=client -o yaml | kubectl apply -f -

        # Deploy canary with new version
        cat <<EOF | kubectl apply -f -
        apiVersion: apps/v1
        kind: Deployment
        metadata:
          name: erlmcp-canary
          namespace: ${{ env.CANARY_NAMESPACE }}
          labels:
            app: erlmcp
            version: ${{ github.event.inputs.version }}
            deployment-type: canary
        spec:
          replicas: 2  # Small canary footprint
          selector:
            matchLabels:
              app: erlmcp
              version: ${{ github.event.inputs.version }}
          template:
            metadata:
              labels:
                app: erlmcp
                version: ${{ github.event.inputs.version }}
            spec:
              containers:
              - name: erlmcp
                image: ghcr.io/${{ github.repository }}:${{ github.event.inputs.version }}
                ports:
                - containerPort: 8080
                env:
                - name: ERLMCP_ENVIRONMENT
                  value: "canary"
                - name: ERLMCP_FEATURE_FLAGS_CONFIG
                  value: "/etc/erlmcp/feature_flags_canary.json"
                livenessProbe:
                  httpGet:
                    path: /health
                    port: 8080
                  initialDelaySeconds: 30
                  periodSeconds: 10
                readinessProbe:
                  httpGet:
                    path: /ready
                    port: 8080
                  initialDelaySeconds: 10
                  periodSeconds: 5
                resources:
                  requests:
                    memory: "512Mi"
                    cpu: "500m"
                  limits:
                    memory: "1Gi"
                    cpu: "1000m"
        EOF

    - name: Configure traffic split
      run: |
        # Update Istio VirtualService for weighted routing
        CANARY_PCT="${{ github.event.inputs.canary_percentage }}"
        STABLE_PCT=$((100 - CANARY_PCT))

        cat <<EOF | kubectl apply -f -
        apiVersion: networking.istio.io/v1beta1
        kind: VirtualService
        metadata:
          name: erlmcp-route
          namespace: ${{ env.STABLE_NAMESPACE }}
        spec:
          hosts:
          - erlmcp.example.com
          http:
          - match:
            - headers:
                x-canary:
                  exact: "true"
            route:
            - destination:
                host: erlmcp-canary.${{ env.CANARY_NAMESPACE }}.svc.cluster.local
                port:
                  number: 8080
              weight: 100
          - route:
            - destination:
                host: erlmcp.${{ env.STABLE_NAMESPACE }}.svc.cluster.local
                port:
                  number: 8080
              weight: $STABLE_PCT
            - destination:
                host: erlmcp-canary.${{ env.CANARY_NAMESPACE }}.svc.cluster.local
                port:
                  number: 8080
              weight: $CANARY_PCT
        EOF

        echo "Traffic split configured: Stable ${STABLE_PCT}% / Canary ${CANARY_PCT}%"

    - name: Wait for canary rollout
      run: |
        kubectl rollout status deployment/erlmcp-canary -n ${{ env.CANARY_NAMESPACE }} --timeout=5m

    - name: Verify canary health
      run: |
        sleep 30  # Wait for readiness probes

        # Check canary pods are running
        READY_PODS=$(kubectl get deployment erlmcp-canary -n ${{ env.CANARY_NAMESPACE }} -o jsonpath='{.status.readyReplicas}')
        DESIRED_PODS=$(kubectl get deployment erlmcp-canary -n ${{ env.CANARY_NAMESPACE }} -o jsonpath='{.spec.replicas}')

        if [ "$READY_PODS" != "$DESIRED_PODS" ]; then
          echo "::error::Canary deployment not ready: $READY_PODS/$DESIRED_PODS pods ready"
          exit 1
        fi

        echo "✅ Canary deployment healthy: $READY_PODS/$DESIRED_PODS pods ready"

  # ========================================================================
  # STEP 3: Monitor Canary
  # ========================================================================
  monitor-canary:
    name: Monitor Canary Release
    needs: deploy-canary
    runs-on: ubuntu-22.04
    timeout-minutes: ${{ fromJSON(github.event.inputs.duration_minutes) }}
    outputs:
      decision: ${{ steps.auto-decision.outputs.decision }}
      metrics: ${{ steps.collect-metrics.outputs.metrics }}

    steps:
    - name: Continuous health monitoring
      run: |
        DURATION=${{ github.event.inputs.duration_minutes }}
        INTERVAL=${{ env.HEALTH_CHECK_INTERVAL }}
        ITERATIONS=$((DURATION * 60 / INTERVAL))

        echo "Monitoring canary for $DURATION minutes ($ITERATIONS checks)"

        for i in $(seq 1 $ITERATIONS); do
          echo "Health check $i/$ITERATIONS"

          # Check canary pod health
          kubectl get pods -n ${{ env.CANARY_NAMESPACE }} -l app=erlmcp

          # Check error logs
          ERROR_COUNT=$(kubectl logs -n ${{ env.CANARY_NAMESPACE }} -l app=erlmcp --since=1m | grep -c ERROR || true)
          if [ $ERROR_COUNT -gt 10 ]; then
            echo "::warning::High error count detected: $ERROR_COUNT errors in last minute"
          fi

          sleep $INTERVAL
        done

    - name: Collect canary metrics
      id: collect-metrics
      run: |
        PROMETHEUS_URL="${{ secrets.PROMETHEUS_URL }}"
        DURATION="${{ github.event.inputs.duration_minutes }}m"

        # Query canary-specific metrics
        CANARY_ERROR_RATE=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(erlmcp_requests_failed_total{deployment=\"canary\"}[$DURATION])")
        CANARY_LATENCY_P95=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket{deployment=\"canary\"}[$DURATION]))")

        # Query stable metrics for comparison
        STABLE_ERROR_RATE=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=rate(erlmcp_requests_failed_total{deployment=\"stable\"}[$DURATION])")
        STABLE_LATENCY_P95=$(curl -s "$PROMETHEUS_URL/api/v1/query?query=histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket{deployment=\"stable\"}[$DURATION]))")

        METRICS=$(cat <<EOF
        {
          "canary": {
            "error_rate": $CANARY_ERROR_RATE,
            "latency_p95": $CANARY_LATENCY_P95
          },
          "stable": {
            "error_rate": $STABLE_ERROR_RATE,
            "latency_p95": $STABLE_LATENCY_P95
          },
          "duration_minutes": ${{ github.event.inputs.duration_minutes }}
        }
        EOF
        )

        echo "metrics=$METRICS" >> $GITHUB_OUTPUT

    - name: Auto-decision engine
      id: auto-decision
      run: |
        # Parse metrics (simplified - use jq in production)
        CANARY_ERROR=$(echo '${{ steps.collect-metrics.outputs.metrics }}' | jq -r '.canary.error_rate')
        STABLE_ERROR=$(echo '${{ steps.collect-metrics.outputs.metrics }}' | jq -r '.stable.error_rate')
        CANARY_LATENCY=$(echo '${{ steps.collect-metrics.outputs.metrics }}' | jq -r '.canary.latency_p95')
        STABLE_LATENCY=$(echo '${{ steps.collect-metrics.outputs.metrics }}' | jq -r '.stable.latency_p95')

        DECISION="promote"  # Default: promote

        # Check error rate
        if (( $(echo "$CANARY_ERROR > ${{ env.ERROR_RATE_THRESHOLD }}" | bc -l) )); then
          echo "::error::Canary error rate too high: $CANARY_ERROR > ${{ env.ERROR_RATE_THRESHOLD }}"
          DECISION="rollback"
        fi

        # Check error rate degradation (>2x stable)
        ERROR_RATIO=$(echo "scale=2; $CANARY_ERROR / $STABLE_ERROR" | bc)
        if (( $(echo "$ERROR_RATIO > 2.0" | bc -l) )); then
          echo "::error::Canary error rate 2x higher than stable: $ERROR_RATIO"
          DECISION="rollback"
        fi

        # Check latency degradation (>1.5x stable)
        LATENCY_RATIO=$(echo "scale=2; $CANARY_LATENCY / $STABLE_LATENCY" | bc)
        if (( $(echo "$LATENCY_RATIO > 1.5" | bc -l) )); then
          echo "::warning::Canary latency 1.5x higher than stable: $LATENCY_RATIO"
          # Don't auto-rollback on latency alone, but flag for review
        fi

        echo "decision=$DECISION" >> $GITHUB_OUTPUT
        echo "Auto-decision: $DECISION"

  # ========================================================================
  # STEP 4a: Promote Canary (if metrics healthy)
  # ========================================================================
  promote-canary:
    name: Promote Canary to Production
    needs: monitor-canary
    if: needs.monitor-canary.outputs.decision == 'promote'
    runs-on: ubuntu-22.04

    steps:
    - name: Gradual traffic increase
      run: |
        # Increase canary traffic in stages: 5% → 25% → 50% → 100%
        for PCT in 25 50 100; do
          echo "Increasing canary traffic to ${PCT}%"

          STABLE_PCT=$((100 - PCT))
          kubectl patch virtualservice erlmcp-route -n ${{ env.STABLE_NAMESPACE }} --type=json -p="[
            {\"op\": \"replace\", \"path\": \"/spec/http/1/route/0/weight\", \"value\": $STABLE_PCT},
            {\"op\": \"replace\", \"path\": \"/spec/http/1/route/1/weight\", \"value\": $PCT}
          ]"

          # Monitor for 10 minutes at each stage
          sleep 600

          # Check metrics
          ERROR_RATE=$(curl -s "${{ secrets.PROMETHEUS_URL }}/api/v1/query?query=rate(erlmcp_requests_failed_total{deployment=\"canary\"}[5m])")
          if (( $(echo "$ERROR_RATE > ${{ env.ERROR_RATE_THRESHOLD }}" | bc -l) )); then
            echo "::error::Error rate increased during promotion: $ERROR_RATE"
            exit 1
          fi
        done

        echo "✅ Canary promoted to 100% traffic"

    - name: Update stable deployment
      run: |
        # Update stable deployment to canary version
        kubectl set image deployment/erlmcp erlmcp=ghcr.io/${{ github.repository }}:${{ github.event.inputs.version }} -n ${{ env.STABLE_NAMESPACE }}
        kubectl rollout status deployment/erlmcp -n ${{ env.STABLE_NAMESPACE }} --timeout=10m

    - name: Remove canary deployment
      run: |
        # Revert traffic to 100% stable
        kubectl patch virtualservice erlmcp-route -n ${{ env.STABLE_NAMESPACE }} --type=json -p='[
          {"op": "replace", "path": "/spec/http/1/route/0/weight", "value": 100},
          {"op": "replace", "path": "/spec/http/1/route/1/weight", "value": 0}
        ]'

        # Scale down canary (keep for 24h for debugging)
        kubectl scale deployment erlmcp-canary --replicas=0 -n ${{ env.CANARY_NAMESPACE }}

        echo "✅ Canary promotion complete"

    - name: Notify success
      uses: slackapi/slack-github-action@v1
      with:
        webhook: ${{ secrets.SLACK_WEBHOOK_URL }}
        payload: |
          {
            "text": "✅ Canary deployment PROMOTED to production",
            "blocks": [
              {
                "type": "section",
                "text": {
                  "type": "mrkdwn",
                  "text": "✅ *Canary Promoted*\n*Version:* ${{ github.event.inputs.version }}\n*Decision:* ${{ needs.monitor-canary.outputs.decision }}\n*Metrics:* Healthy"
                }
              }
            ]
          }

  # ========================================================================
  # STEP 4b: Rollback Canary (if metrics unhealthy)
  # ========================================================================
  rollback-canary:
    name: Rollback Canary Deployment
    needs: monitor-canary
    if: needs.monitor-canary.outputs.decision == 'rollback'
    runs-on: ubuntu-22.04

    steps:
    - name: Immediate traffic cutoff
      run: |
        # Set canary traffic to 0%
        kubectl patch virtualservice erlmcp-route -n ${{ env.STABLE_NAMESPACE }} --type=json -p='[
          {"op": "replace", "path": "/spec/http/1/route/0/weight", "value": 100},
          {"op": "replace", "path": "/spec/http/1/route/1/weight", "value": 0}
        ]'

        echo "🚨 Canary traffic cut to 0%"

    - name: Delete canary deployment
      run: |
        kubectl delete deployment erlmcp-canary -n ${{ env.CANARY_NAMESPACE }}
        echo "Canary deployment deleted"

    - name: Capture failure logs
      run: |
        # Save canary logs for post-mortem
        kubectl logs -n ${{ env.CANARY_NAMESPACE }} -l app=erlmcp --tail=1000 > canary_failure_logs.txt

    - name: Upload failure artifacts
      uses: actions/upload-artifact@v3
      with:
        name: canary-failure-logs
        path: canary_failure_logs.txt

    - name: Notify failure
      uses: slackapi/slack-github-action@v1
      with:
        webhook: ${{ secrets.SLACK_WEBHOOK_URL }}
        payload: |
          {
            "text": "🚨 Canary deployment ROLLED BACK",
            "blocks": [
              {
                "type": "section",
                "text": {
                  "type": "mrkdwn",
                  "text": "🚨 *Canary Rollback*\n*Version:* ${{ github.event.inputs.version }}\n*Decision:* ${{ needs.monitor-canary.outputs.decision }}\n*Metrics:* Unhealthy\n*Logs:* See workflow artifacts\n@oncall-team"
                }
              }
            ]
          }
```

### C. Canary Success Criteria

| Metric | Threshold | Action if Exceeded |
|--------|-----------|-------------------|
| **Error Rate** | <0.1% | Immediate rollback |
| **Error Rate vs Stable** | <2x stable | Immediate rollback |
| **Latency P95** | <10ms | Warning (manual review) |
| **Latency vs Stable** | <1.5x stable | Warning (manual review) |
| **Latency P99** | <20ms | Warning (manual review) |
| **CPU Usage** | <80% | Warning |
| **Memory Usage** | <80% | Warning |
| **Pod Restart Count** | 0 restarts | Immediate rollback |

**Observation Periods:**
- **Stage 1 (5%)**: 60 minutes
- **Stage 2 (25%)**: 30 minutes
- **Stage 3 (50%)**: 30 minutes
- **Stage 4 (100%)**: 60 minutes

**Total Canary Duration:** ~3 hours (conservative)

---

## IV. ROLLBACK PROCEDURES

### A. Rollback Decision Matrix

| Trigger | Severity | Rollback Type | Timeline | Approval Required |
|---------|----------|---------------|----------|-------------------|
| **Error rate >0.1%** | Critical | Automatic | Immediate (<2 min) | No |
| **Error rate >2x baseline** | Critical | Automatic | Immediate (<2 min) | No |
| **Pod crash loop** | Critical | Automatic | Immediate (<1 min) | No |
| **Health check failures** | High | Automatic | 30 seconds | No |
| **Latency >1.5x baseline** | Medium | Manual | 10 minutes | Yes (SRE on-call) |
| **Customer complaint** | Medium | Manual | 15 minutes | Yes (Product manager) |
| **Failed integration test** | Low | Manual | 30 minutes | Yes (Engineering lead) |

### B. Automated Rollback Script

**File:** `scripts/rollback.sh`

```bash
#!/bin/bash
# scripts/rollback.sh - Automated rollback to previous stable version

set -euo pipefail

# Configuration
NAMESPACE="${NAMESPACE:-erlmcp}"
DEPLOYMENT="${DEPLOYMENT:-erlmcp}"
ROLLBACK_TIMEOUT="${ROLLBACK_TIMEOUT:-300}"  # 5 minutes

# Logging
log() {
    echo "[$(date -u +%Y-%m-%dT%H:%M:%SZ)] $*" >&2
}

error() {
    log "ERROR: $*"
    exit 1
}

# ============================================================================
# STEP 1: Pre-rollback Validation
# ============================================================================
validate_environment() {
    log "Validating environment..."

    # Check kubectl is configured
    kubectl cluster-info > /dev/null 2>&1 || error "kubectl not configured"

    # Check namespace exists
    kubectl get namespace "$NAMESPACE" > /dev/null 2>&1 || error "Namespace $NAMESPACE not found"

    # Check deployment exists
    kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" > /dev/null 2>&1 || error "Deployment $DEPLOYMENT not found"

    log "✅ Environment validated"
}

# ============================================================================
# STEP 2: Capture Current State
# ============================================================================
capture_state() {
    log "Capturing current deployment state..."

    CURRENT_VERSION=$(kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" -o jsonpath='{.spec.template.spec.containers[0].image}' | cut -d: -f2)
    CURRENT_REPLICAS=$(kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" -o jsonpath='{.spec.replicas}')
    CURRENT_REVISION=$(kubectl rollout history deployment/"$DEPLOYMENT" -n "$NAMESPACE" --revision=0 | tail -1 | awk '{print $1}')

    log "Current state:"
    log "  Version: $CURRENT_VERSION"
    log "  Replicas: $CURRENT_REPLICAS"
    log "  Revision: $CURRENT_REVISION"

    # Save state to file for post-mortem
    cat > "/tmp/rollback_state_$(date +%s).json" <<EOF
{
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)",
  "namespace": "$NAMESPACE",
  "deployment": "$DEPLOYMENT",
  "current_version": "$CURRENT_VERSION",
  "current_replicas": $CURRENT_REPLICAS,
  "current_revision": $CURRENT_REVISION
}
EOF
}

# ============================================================================
# STEP 3: Execute Rollback
# ============================================================================
execute_rollback() {
    log "🚨 EXECUTING ROLLBACK..."

    # Get previous revision
    PREVIOUS_REVISION=$((CURRENT_REVISION - 1))

    if [ "$PREVIOUS_REVISION" -lt 1 ]; then
        error "No previous revision available for rollback"
    fi

    log "Rolling back to revision $PREVIOUS_REVISION..."

    # Execute Kubernetes rollout undo
    kubectl rollout undo deployment/"$DEPLOYMENT" -n "$NAMESPACE" --to-revision="$PREVIOUS_REVISION"

    log "Waiting for rollback to complete (timeout: ${ROLLBACK_TIMEOUT}s)..."

    # Wait for rollout to complete
    if kubectl rollout status deployment/"$DEPLOYMENT" -n "$NAMESPACE" --timeout="${ROLLBACK_TIMEOUT}s"; then
        log "✅ Rollback completed successfully"
    else
        error "Rollback timed out after ${ROLLBACK_TIMEOUT}s"
    fi
}

# ============================================================================
# STEP 4: Verify Rollback
# ============================================================================
verify_rollback() {
    log "Verifying rollback..."

    # Check pods are running
    READY_PODS=$(kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" -o jsonpath='{.status.readyReplicas}')
    DESIRED_PODS=$(kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" -o jsonpath='{.spec.replicas}')

    if [ "$READY_PODS" != "$DESIRED_PODS" ]; then
        error "Rollback verification failed: $READY_PODS/$DESIRED_PODS pods ready"
    fi

    # Check pod health
    UNHEALTHY_PODS=$(kubectl get pods -n "$NAMESPACE" -l app=erlmcp -o json | jq -r '.items[] | select(.status.phase != "Running") | .metadata.name' | wc -l)
    if [ "$UNHEALTHY_PODS" -gt 0 ]; then
        error "Rollback verification failed: $UNHEALTHY_PODS unhealthy pods"
    fi

    # Health check
    log "Running health check..."
    sleep 10  # Wait for readiness probes

    HEALTH_CHECK_URL="${HEALTH_CHECK_URL:-http://localhost:8080/health}"
    if command -v curl > /dev/null 2>&1; then
        if curl -sf "$HEALTH_CHECK_URL" > /dev/null; then
            log "✅ Health check passed"
        else
            error "Health check failed: $HEALTH_CHECK_URL"
        fi
    else
        log "⚠️ curl not available, skipping health check"
    fi

    log "✅ Rollback verified: $READY_PODS/$DESIRED_PODS pods healthy"
}

# ============================================================================
# STEP 5: Post-Rollback Actions
# ============================================================================
post_rollback() {
    log "Executing post-rollback actions..."

    # Get new version
    NEW_VERSION=$(kubectl get deployment "$DEPLOYMENT" -n "$NAMESPACE" -o jsonpath='{.spec.template.spec.containers[0].image}' | cut -d: -f2)

    log "Rolled back from $CURRENT_VERSION to $NEW_VERSION"

    # Send notification (if configured)
    if [ -n "${SLACK_WEBHOOK_URL:-}" ]; then
        curl -X POST "$SLACK_WEBHOOK_URL" -H 'Content-Type: application/json' -d @- <<EOF
{
  "text": "🚨 Production Rollback Executed",
  "blocks": [
    {
      "type": "section",
      "text": {
        "type": "mrkdwn",
        "text": "🚨 *Production Rollback*\n*From:* $CURRENT_VERSION\n*To:* $NEW_VERSION\n*Namespace:* $NAMESPACE\n*Status:* Success\n@oncall-team"
      }
    }
  ]
}
EOF
    fi

    # Create incident ticket (if configured)
    if [ -n "${INCIDENT_WEBHOOK_URL:-}" ]; then
        log "Creating incident ticket..."
        curl -X POST "$INCIDENT_WEBHOOK_URL" -H 'Content-Type: application/json' -d @- <<EOF
{
  "title": "Production Rollback: $DEPLOYMENT",
  "severity": "high",
  "description": "Automatic rollback from $CURRENT_VERSION to $NEW_VERSION",
  "metadata": {
    "namespace": "$NAMESPACE",
    "deployment": "$DEPLOYMENT",
    "rollback_time": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
  }
}
EOF
    fi
}

# ============================================================================
# Main Execution
# ============================================================================
main() {
    log "==================================================================="
    log "AUTOMATED ROLLBACK PROCEDURE"
    log "==================================================================="

    validate_environment
    capture_state
    execute_rollback
    verify_rollback
    post_rollback

    log "==================================================================="
    log "✅ ROLLBACK COMPLETED SUCCESSFULLY"
    log "==================================================================="
}

# Run main function
main "$@"
```

### C. Manual Rollback Runbook

See `docs/devops/ROLLBACK_RUNBOOK.md` (next document).

---

## V. PRODUCTION MONITORING AND ALERTING

### A. Monitoring Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                   Production Metrics                         │
└──────────────────┬──────────────────────────────────────────┘
                   │
      ┌────────────┼────────────┐
      │            │            │
┌─────▼─────┐ ┌───▼────┐ ┌────▼─────┐
│ OpenTelemetry│ Prometheus│ Grafana   │
│ Collector │ │  Server  │ │Dashboards│
└───────────┘ └──────────┘ └──────────┘
      │            │            │
      └────────────┼────────────┘
                   │
      ┌────────────┼────────────┐
      │            │            │
┌─────▼─────┐ ┌───▼────┐ ┌────▼─────┐
│AlertManager│ PagerDuty │  Slack    │
└───────────┘ └──────────┘ └──────────┘
```

### B. Key Metrics for MCP Implementation

#### 1. Business Metrics

```yaml
# Prometheus metrics for MCP features

# Sampling metrics (Phase 1)
erlmcp_sampling_requests_total counter
erlmcp_sampling_duration_seconds histogram
erlmcp_sampling_samples_generated_total counter

# Tasks metrics (Phase 2)
erlmcp_tasks_created_total counter
erlmcp_tasks_completed_total counter
erlmcp_tasks_failed_total counter
erlmcp_tasks_duration_seconds histogram

# Elicitation metrics (Phase 2)
erlmcp_elicitation_prompts_total counter
erlmcp_elicitation_responses_total counter
erlmcp_elicitation_duration_seconds histogram

# Performance metrics (Phase 3)
erlmcp_streaming_chunks_total counter
erlmcp_parallel_requests_total counter
erlmcp_connection_pool_utilization gauge

# General MCP metrics
erlmcp_requests_total{method, status} counter
erlmcp_request_duration_seconds{method} histogram
erlmcp_requests_failed_total{method, reason} counter
erlmcp_active_sessions gauge
erlmcp_memory_usage_bytes gauge
erlmcp_cpu_usage_percent gauge
```

#### 2. Alert Rules

**File:** `config/prometheus/alerts.yml`

```yaml
groups:
- name: erlmcp_critical
  interval: 30s
  rules:
  # High error rate (immediate)
  - alert: HighErrorRate
    expr: rate(erlmcp_requests_failed_total[5m]) > 0.001
    for: 1m
    labels:
      severity: critical
    annotations:
      summary: "High error rate detected"
      description: "Error rate {{ $value }} exceeds threshold (0.1%)"

  # Pod crash loop
  - alert: PodCrashLoop
    expr: rate(kube_pod_container_status_restarts_total{pod=~"erlmcp.*"}[15m]) > 0
    for: 5m
    labels:
      severity: critical
    annotations:
      summary: "Pod crash loop detected"
      description: "Pod {{ $labels.pod }} is crash looping"

  # High latency P95
  - alert: HighLatencyP95
    expr: histogram_quantile(0.95, rate(erlmcp_request_duration_seconds_bucket[5m])) > 0.01
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "High P95 latency detected"
      description: "P95 latency {{ $value }}s exceeds threshold (10ms)"

  # Memory leak detection
  - alert: MemoryLeak
    expr: rate(erlmcp_memory_usage_bytes[30m]) > 1048576  # 1MB/s growth
    for: 30m
    labels:
      severity: warning
    annotations:
      summary: "Potential memory leak detected"
      description: "Memory usage growing at {{ $value }} bytes/s"

  # Low connection pool
  - alert: LowConnectionPool
    expr: erlmcp_connection_pool_utilization > 0.9
    for: 5m
    labels:
      severity: warning
    annotations:
      summary: "Connection pool near capacity"
      description: "Pool utilization {{ $value }} > 90%"

- name: erlmcp_phase1_sampling
  interval: 60s
  rules:
  # Sampling latency regression
  - alert: SamplingLatencyRegression
    expr: histogram_quantile(0.95, rate(erlmcp_sampling_duration_seconds_bucket[10m])) > 0.005
    for: 10m
    labels:
      severity: warning
      phase: 1
    annotations:
      summary: "Sampling P95 latency regression"
      description: "Sampling latency {{ $value }}s exceeds baseline (5ms)"

  # Sampling failure rate
  - alert: SamplingFailureRate
    expr: rate(erlmcp_sampling_requests_total{status="error"}[10m]) / rate(erlmcp_sampling_requests_total[10m]) > 0.01
    for: 5m
    labels:
      severity: critical
      phase: 1
    annotations:
      summary: "High sampling failure rate"
      description: "Sampling failure rate {{ $value }} exceeds 1%"

- name: erlmcp_phase2_tasks
  interval: 60s
  rules:
  # Task completion rate
  - alert: LowTaskCompletionRate
    expr: rate(erlmcp_tasks_completed_total[15m]) / rate(erlmcp_tasks_created_total[15m]) < 0.95
    for: 10m
    labels:
      severity: warning
      phase: 2
    annotations:
      summary: "Low task completion rate"
      description: "Task completion rate {{ $value }} < 95%"

  # Task duration anomaly
  - alert: TaskDurationAnomaly
    expr: histogram_quantile(0.95, rate(erlmcp_tasks_duration_seconds_bucket[10m])) > 10
    for: 10m
    labels:
      severity: warning
      phase: 2
    annotations:
      summary: "Task duration anomaly"
      description: "P95 task duration {{ $value }}s exceeds 10s"
```

### C. Grafana Dashboards

**Dashboard:** `config/grafana/mcp-overview.json`

**Panels:**
1. **MCP Request Rate** (graph, 1h)
   - Total requests/s
   - By method (sampling, tasks, elicitation)
   - By status (success, error)

2. **Latency Distribution** (heatmap, 1h)
   - P50, P95, P99
   - Per-method breakdown

3. **Error Rate** (graph, 1h)
   - Error rate %
   - Error count by reason

4. **Active Sessions** (gauge, realtime)
   - Total active sessions
   - Session growth rate

5. **Resource Utilization** (graph, 1h)
   - CPU %
   - Memory MB
   - Connection pool %

6. **Phase-Specific Metrics** (conditional panels)
   - Phase 1: Sampling latency, samples/s
   - Phase 2: Tasks created/completed, elicitation prompts
   - Phase 3: Streaming chunks/s, parallel req/s

**Alerting Integration:**
- Panel alerts trigger Slack notifications
- Critical alerts trigger PagerDuty

---

## VI. MULTI-ENVIRONMENT SETUP

### A. Environment Topology

```
Development (dev)
    ↓ Merge to main
Continuous Integration (test)
    ↓ Release branch
Staging (staging)
    ↓ Canary deployment (5%)
Canary (canary)
    ↓ Promote (100%)
Production (production)
```

### B. Environment Configuration Matrix

| Aspect | Dev | Test | Staging | Canary | Production |
|--------|-----|------|---------|--------|------------|
| **OTP Version** | 28 | 28 | 28 | 28 | 28 |
| **Rebar3 Profile** | dev | test | staging | prod | prod |
| **Feature Flags** | All enabled | Phase-specific | Phase-specific | Phase-specific | Phase-specific |
| **Log Level** | debug | info | info | warning | warning |
| **OTEL Sampling** | 100% | 100% | 10% | 1% | 1% |
| **Session Backend** | ETS | DETS | Mnesia (3 nodes) | Mnesia (3 nodes) | Mnesia (5 nodes) |
| **Secrets** | LocalEncrypted | Vault (dev) | Vault (staging) | Vault (prod) | Vault (prod) |
| **TLS** | Self-signed | Let's Encrypt | Let's Encrypt | Let's Encrypt | Let's Encrypt |
| **Replicas** | 1 | 2 | 3 | 2 | 10 |
| **Auto-scaling** | Disabled | Disabled | Enabled (3-5) | Enabled (2-4) | Enabled (10-50) |
| **Database** | Mnesia (local) | Mnesia (local) | Mnesia (cluster) | Mnesia (cluster) | Mnesia (cluster) |
| **Monitoring** | Local Prometheus | Shared Prometheus | Shared Prometheus | Shared Prometheus | Dedicated Prometheus |
| **Alerts** | Disabled | Disabled | Slack only | Slack + Email | Slack + PagerDuty |

### C. Environment-Specific Configuration

#### 1. Development (`config/sys.config.dev`)
```erlang
[
    {erlmcp_core, [
        {environment, dev},
        {log_level, debug},
        {feature_flags_config, "config/feature_flags_dev.json"},
        {session_backend, erlmcp_session_ets},
        {secrets_backend, erlmcp_secrets_local},
        {registry_type, gproc},
        {max_connections, 100}
    ]},
    {erlmcp_observability, [
        {otel_enabled, true},
        {otel_sampling_rate, 1.0},
        {metrics_port, 9090},
        {dashboard_enabled, true}
    ]}
].
```

#### 2. Test (`config/sys.config.test`)
```erlang
[
    {erlmcp_core, [
        {environment, test},
        {log_level, info},
        {feature_flags_config, "config/feature_flags_test.json"},
        {session_backend, erlmcp_session_dets},
        {secrets_backend, erlmcp_secrets_vault},
        {vault_url, "http://vault-dev:8200"},
        {registry_type, gproc},
        {max_connections, 500}
    ]},
    {erlmcp_observability, [
        {otel_enabled, true},
        {otel_sampling_rate, 1.0},
        {metrics_port, 9090}
    ]}
].
```

#### 3. Staging (`config/sys.config.staging`)
```erlang
[
    {erlmcp_core, [
        {environment, staging},
        {log_level, info},
        {feature_flags_config, "config/feature_flags_staging.json"},
        {session_backend, erlmcp_session_mnesia},
        {mnesia_nodes, ['erlmcp1@staging', 'erlmcp2@staging', 'erlmcp3@staging']},
        {secrets_backend, erlmcp_secrets_vault},
        {vault_url, "https://vault-staging.example.com"},
        {registry_type, gproc},
        {max_connections, 10000}
    ]},
    {erlmcp_observability, [
        {otel_enabled, true},
        {otel_sampling_rate, 0.1},
        {metrics_port, 9090},
        {prometheus_url, "http://prometheus-staging:9090"}
    ]}
].
```

#### 4. Production (`config/sys.config.prod`)
```erlang
[
    {erlmcp_core, [
        {environment, production},
        {log_level, warning},
        {feature_flags_config, "config/feature_flags_prod.json"},
        {session_backend, erlmcp_session_mnesia},
        {mnesia_nodes, ['erlmcp1@prod', 'erlmcp2@prod', 'erlmcp3@prod', 'erlmcp4@prod', 'erlmcp5@prod']},
        {secrets_backend, erlmcp_secrets_vault},
        {vault_url, "https://vault.example.com"},
        {registry_type, gproc},
        {max_connections, 50000},
        {connection_pool_size, 100}
    ]},
    {erlmcp_observability, [
        {otel_enabled, true},
        {otel_sampling_rate, 0.01},
        {metrics_port, 9090},
        {prometheus_url, "http://prometheus:9090"}
    ]},
    {mnesia, [
        {dir, "/var/lib/erlmcp/mnesia"},
        {dump_log_write_threshold, 50000},
        {dc_dump_limit, 40}
    ]}
].
```

---

## VII. DATABASE MIGRATION STRATEGY (ETS → Mnesia)

### A. Migration Phases

**Current State:**
- **ETS**: In-memory, non-persistent, single-node
- **DETS**: Disk-based, persistent, single-node
- **Mnesia**: Distributed, replicated, multi-node

**Migration Path:**
```
Dev: ETS (no migration needed)
    ↓
Test: ETS → DETS (Phase 0, week 0-2)
    ↓
Staging: DETS → Mnesia (Phase 1, week 2-4)
    ↓
Production: DETS → Mnesia (Phase 2, week 4-8)
```

### B. Migration Script

**File:** `scripts/migrate_session_backend.erl`

```erlang
#!/usr/bin/env escript
%%! -smp enable -sname session_migrator -mnesia debug verbose

%% Session backend migration script
%% Migrates session data from ETS/DETS to Mnesia

-mode(compile).

main([SourceBackend, TargetBackend]) ->
    io:format("Starting session backend migration~n"),
    io:format("  Source: ~s~n", [SourceBackend]),
    io:format("  Target: ~s~n", [TargetBackend]),

    % Start Erlang applications
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),

    % Perform migration based on backends
    case {SourceBackend, TargetBackend} of
        {"ets", "dets"} ->
            migrate_ets_to_dets();
        {"dets", "mnesia"} ->
            migrate_dets_to_mnesia();
        {"ets", "mnesia"} ->
            migrate_ets_to_dets(),
            migrate_dets_to_mnesia();
        _ ->
            io:format("ERROR: Unsupported migration path: ~s -> ~s~n",
                      [SourceBackend, TargetBackend]),
            halt(1)
    end,

    io:format("Migration completed successfully~n"),
    halt(0);

main(_) ->
    usage(),
    halt(1).

usage() ->
    io:format("Usage: migrate_session_backend.erl <source> <target>~n"),
    io:format("  source: ets | dets~n"),
    io:format("  target: dets | mnesia~n"),
    io:format("~nExamples:~n"),
    io:format("  ./migrate_session_backend.erl ets dets~n"),
    io:format("  ./migrate_session_backend.erl dets mnesia~n").

%%% ETS → DETS Migration

migrate_ets_to_dets() ->
    io:format("~n=== ETS → DETS Migration ===~n"),

    % Open ETS table (assumes erlmcp is running)
    EtsTable = erlmcp_sessions,

    % Open DETS table
    DetsFile = "data/sessions.dets",
    {ok, DetsTable} = dets:open_file(sessions_dets, [
        {file, DetsFile},
        {type, set},
        {keypos, 2}  % session_id position
    ]),

    % Copy all records
    io:format("Copying ETS records to DETS...~n"),
    Count = ets:foldl(
        fun(Record, Acc) ->
            dets:insert(DetsTable, Record),
            if Acc rem 1000 == 0 ->
                io:format("  Migrated ~p records~n", [Acc]);
            true -> ok
            end,
            Acc + 1
        end,
        0,
        EtsTable
    ),

    % Sync DETS to disk
    dets:sync(DetsTable),
    dets:close(DetsTable),

    io:format("✅ Migrated ~p sessions from ETS to DETS~n", [Count]).

%%% DETS → Mnesia Migration

migrate_dets_to_mnesia() ->
    io:format("~n=== DETS → Mnesia Migration ===~n"),

    % Start Mnesia
    mnesia:create_schema([node()]),
    application:start(mnesia),

    % Create Mnesia table
    mnesia:create_table(erlmcp_sessions, [
        {attributes, record_info(fields, session)},
        {disc_copies, [node()]},
        {type, set},
        {index, [user_id, created_at]}
    ]),

    % Open DETS file
    DetsFile = "data/sessions.dets",
    {ok, DetsTable} = dets:open_file(sessions_dets, [
        {file, DetsFile},
        {type, set}
    ]),

    % Copy all records to Mnesia
    io:format("Copying DETS records to Mnesia...~n"),
    Count = dets:foldl(
        fun(Record, Acc) ->
            mnesia:dirty_write(erlmcp_sessions, Record),
            if Acc rem 1000 == 0 ->
                io:format("  Migrated ~p records~n", [Acc]);
            true -> ok
            end,
            Acc + 1
        end,
        0,
        DetsTable
    ),

    dets:close(DetsTable),

    % Verify migration
    MnesiaCount = mnesia:table_info(erlmcp_sessions, size),
    if MnesiaCount == Count ->
        io:format("✅ Migrated ~p sessions from DETS to Mnesia (verified)~n", [Count]);
    true ->
        io:format("❌ Migration verification failed: DETS=~p, Mnesia=~p~n",
                  [Count, MnesiaCount]),
        halt(1)
    end.

-record(session, {
    session_id,
    user_id,
    state,
    created_at,
    updated_at,
    metadata
}).
```

### C. Zero-Downtime Migration Procedure

**Strategy: Dual-write with gradual cutover**

```
Phase 0: Preparation (Week 0)
├── Install migration tooling
├── Backup existing DETS files
└── Create Mnesia schema on all nodes

Phase 1: Dual-write (Week 1-2)
├── Write to both DETS and Mnesia
├── Read from DETS (primary)
└── Validate Mnesia consistency (background)

Phase 2: Dual-read (Week 3-4)
├── Continue dual-write
├── Read from Mnesia (primary)
├── Fallback to DETS on errors
└── Monitor error rates

Phase 3: Mnesia-only (Week 5+)
├── Write to Mnesia only
├── Read from Mnesia only
└── Archive DETS files (retain 30 days)
```

**Implementation:**

```erlang
%% erlmcp_session_backend_hybrid.erl
%% Dual-write backend for zero-downtime migration

-module(erlmcp_session_backend_hybrid).
-behaviour(erlmcp_session_backend).

-export([init/1, create/2, read/2, update/3, delete/2, list/1]).

-record(state, {
    primary_backend,
    secondary_backend,
    migration_phase  % phase1 | phase2 | phase3
}).

init(Opts) ->
    Phase = proplists:get_value(migration_phase, Opts, phase1),

    % Initialize both backends
    {ok, PrimaryState} = erlmcp_session_dets:init(Opts),
    {ok, SecondaryState} = erlmcp_session_mnesia:init(Opts),

    State = #state{
        primary_backend = {dets, PrimaryState},
        secondary_backend = {mnesia, SecondaryState},
        migration_phase = Phase
    },

    {ok, State}.

create(SessionId, SessionData, State) ->
    #state{primary_backend = Primary, secondary_backend = Secondary,
           migration_phase = Phase} = State,

    % Always write to both backends during migration
    {PrimaryMod, PrimaryState} = Primary,
    {SecondaryMod, SecondaryState} = Secondary,

    case PrimaryMod:create(SessionId, SessionData, PrimaryState) of
        {ok, PrimaryState2} ->
            % Async write to secondary (don't block on errors)
            spawn(fun() ->
                SecondaryMod:create(SessionId, SessionData, SecondaryState)
            end),
            {ok, State#state{primary_backend = {PrimaryMod, PrimaryState2}}};
        Error ->
            Error
    end.

read(SessionId, State) ->
    #state{primary_backend = Primary, secondary_backend = Secondary,
           migration_phase = Phase} = State,

    {PrimaryMod, PrimaryState} = Primary,
    {SecondaryMod, SecondaryState} = Secondary,

    % Read from appropriate backend based on migration phase
    case Phase of
        phase1 ->
            % Phase 1: Read from DETS (primary), validate against Mnesia
            case PrimaryMod:read(SessionId, PrimaryState) of
                {ok, Data} ->
                    % Async validation
                    spawn(fun() -> validate_consistency(SessionId, Data, SecondaryMod, SecondaryState) end),
                    {ok, Data};
                Error -> Error
            end;
        phase2 ->
            % Phase 2: Read from Mnesia (primary), fallback to DETS
            case SecondaryMod:read(SessionId, SecondaryState) of
                {ok, Data} -> {ok, Data};
                {error, not_found} ->
                    % Fallback to DETS
                    logger:warning("Mnesia read failed for ~p, falling back to DETS", [SessionId]),
                    PrimaryMod:read(SessionId, PrimaryState);
                Error -> Error
            end;
        phase3 ->
            % Phase 3: Mnesia only
            SecondaryMod:read(SessionId, SecondaryState)
    end.

validate_consistency(SessionId, ExpectedData, Mod, ModState) ->
    case Mod:read(SessionId, ModState) of
        {ok, ActualData} ->
            if ActualData =/= ExpectedData ->
                logger:error("Consistency violation for session ~p: DETS=~p, Mnesia=~p",
                             [SessionId, ExpectedData, ActualData]);
            true -> ok
            end;
        {error, not_found} ->
            logger:warning("Session ~p missing in Mnesia", [SessionId]);
        Error ->
            logger:error("Mnesia validation error for ~p: ~p", [SessionId, Error])
    end.
```

---

## VIII. ZERO-DOWNTIME UPGRADE CAPABILITY

### A. Hot Code Loading Strategy

**Erlang/OTP Native:**
```erlang
%% Upgrade module at runtime without restarting VM

% Load new module code
code:load_file(erlmcp_server),

% Purge old code (after grace period)
code:soft_purge(erlmcp_server),

% Upgrade running gen_servers
sys:suspend(erlmcp_server),
sys:change_code(erlmcp_server, Module, OldVsn, Extra),
sys:resume(erlmcp_server).
```

**Release Upgrade (appup):**

**File:** `apps/erlmcp_core/src/erlmcp_core.appup`

```erlang
%% appup file for hot upgrades
{"2.2.0",  % New version
 [
  % Upgrade from 2.1.0
  {"2.1.0",
   [
    % Load new modules
    {load_module, erlmcp_sampling},
    {load_module, erlmcp_message_priority},

    % Update gen_servers with code change
    {update, erlmcp_server, {advanced, []}, brutal_purge, brutal_purge, []},
    {update, erlmcp_client, {advanced, []}, brutal_purge, brutal_purge, []},

    % Add new supervisor children
    {add_application, erlmcp_sampling_sup}
   ]
  }
 ],
 [
  % Downgrade to 2.1.0
  {"2.1.0",
   [
    {remove_application, erlmcp_sampling_sup},
    {delete_module, erlmcp_sampling},
    {delete_module, erlmcp_message_priority},
    {update, erlmcp_server, {advanced, []}, brutal_purge, brutal_purge, []},
    {update, erlmcp_client, {advanced, []}, brutal_purge, brutal_purge, []}
   ]
  }
 ]
}.
```

### B. Blue-Green Deployment

**Kubernetes Strategy:**

```yaml
# Blue-green deployment using Kubernetes labels and services

# Step 1: Deploy green (new version) alongside blue (current)
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: erlmcp-green
  namespace: erlmcp
spec:
  replicas: 10
  selector:
    matchLabels:
      app: erlmcp
      version: green
  template:
    metadata:
      labels:
        app: erlmcp
        version: green
    spec:
      containers:
      - name: erlmcp
        image: ghcr.io/org/erlmcp:v2.2.0
        # ... (same as blue deployment)

# Step 2: Service points to blue (current)
---
apiVersion: v1
kind: Service
metadata:
  name: erlmcp
  namespace: erlmcp
spec:
  selector:
    app: erlmcp
    version: blue  # Initially points to blue
  ports:
  - port: 8080
    targetPort: 8080

# Step 3: Switch traffic to green (atomic)
# kubectl patch service erlmcp -p '{"spec":{"selector":{"version":"green"}}}'

# Step 4: Delete blue deployment after verification
# kubectl delete deployment erlmcp-blue
```

**Rollback:**
```bash
# Instant rollback by switching service selector
kubectl patch service erlmcp -p '{"spec":{"selector":{"version":"blue"}}}'
```

### C. Session Migration for Zero-Downtime

**Strategy: Session drain + replication**

```erlang
%% erlmcp_session_drain.erl
%% Graceful session migration during upgrades

-module(erlmcp_session_drain).
-export([drain_node/1, migrate_sessions/2]).

drain_node(Node) ->
    % Stop accepting new sessions
    erlmcp_server:stop_accepting(),

    % Wait for in-flight requests to complete (max 30s)
    Timeout = 30000,
    wait_for_requests(Timeout),

    % Migrate active sessions to other nodes
    ActiveSessions = erlmcp_session_manager:list_active(),
    lists:foreach(fun(Session) ->
        migrate_session(Session, Node)
    end, ActiveSessions),

    % Signal node is drained
    ok.

wait_for_requests(Timeout) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_requests_loop(Timeout, StartTime).

wait_for_requests_loop(Timeout, StartTime) ->
    case erlmcp_server:active_requests() of
        0 ->
            logger:info("All requests completed, node drained"),
            ok;
        Count ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            if Elapsed > Timeout ->
                logger:warning("Drain timeout, ~p requests still active", [Count]),
                ok;
            true ->
                timer:sleep(1000),
                wait_for_requests_loop(Timeout, StartTime)
            end
    end.

migrate_session(SessionId, TargetNode) ->
    % Read session state
    {ok, SessionData} = erlmcp_session_manager:read(SessionId),

    % Write to target node (Mnesia replication handles this)
    case rpc:call(TargetNode, erlmcp_session_manager, create, [SessionId, SessionData]) of
        {ok, _} ->
            logger:info("Migrated session ~p to ~p", [SessionId, TargetNode]),
            ok;
        Error ->
            logger:error("Failed to migrate session ~p: ~p", [SessionId, Error]),
            Error
    end.
```

---

## IX. DEPLOYMENT RUNBOOKS

See separate document: `docs/devops/DEPLOYMENT_RUNBOOKS.md` (next).

---

## X. ROLLOUT CHECKLIST

See separate document: `docs/devops/MCP_ROLLOUT_CHECKLIST.md` (next).

---

## XI. SUMMARY

### A. Pipeline Enhancements
- **550+ tests** integrated via smart change detection
- **CI runtime**: ≤8 minutes (target achieved via parallelism)
- **Quality gates**: 8 gates (add compliance, benchmarks)
- **Test sharding**: 4x parallelism for EUnit/CT

### B. Feature Flags
- **Service**: `erlmcp_feature_flags` gen_server
- **Rollout**: Gradual (10% → 25% → 50% → 100%)
- **Phases**: 4 phases aligned with MCP implementation
- **Reload**: Dynamic config reload (60s interval)

### C. Canary Deployment
- **Traffic split**: 5% → 25% → 50% → 100% (3-hour total)
- **Auto-decision**: Automatic promote/rollback based on metrics
- **Health checks**: Error rate, latency, CPU, memory
- **Rollback**: <2 minutes for critical issues

### D. Zero-Downtime Upgrades
- **Hot code loading**: Erlang native appup
- **Blue-green**: Kubernetes label-based routing
- **Session migration**: Mnesia replication + graceful drain
- **Rollback**: Instant (service selector switch)

### E. Monitoring
- **Metrics**: OpenTelemetry → Prometheus
- **Alerts**: 15+ rules (error rate, latency, resource usage)
- **Dashboards**: Grafana (6 panels, phase-specific)
- **Notification**: Slack + PagerDuty

### F. Environments
- **5 environments**: dev, test, staging, canary, production
- **Config matrix**: 10 aspects (OTP, features, logs, secrets, TLS, etc.)
- **Migration**: ETS → DETS → Mnesia (8-week zero-downtime)

---

## XII. NEXT STEPS

1. **Review** this deployment pipeline design with SRE team
2. **Implement** feature flag service (`erlmcp_feature_flags.erl`)
3. **Create** runbooks and checklists (separate documents)
4. **Test** canary deployment workflow in staging
5. **Execute** Phase 1 rollout (Sampling)

---

**Document Status:** DESIGN COMPLETE
**Ready for Implementation:** Yes
**Approval Required:** SRE Lead, Engineering Manager
