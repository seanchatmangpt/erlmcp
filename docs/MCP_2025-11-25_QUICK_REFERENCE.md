# MCP 2025-11-25 Implementation Quick Reference

**Purpose**: Quick reference guide for implementing MCP 2025-11-25 features in erlmcp
**Timeline**: 10 weeks (2.5 months)
**Methodology**: Chicago School TDD + SPARC

---

## Quick Navigation

- [Weekly Schedule](#weekly-schedule)
- [Phase 4a: Task Management](#phase-4a-task-management)
- [Phase 4b: Completion API](#phase-4b-completion-api)
- [Phase 4c: Elicitation](#phase-4c-elicitation)
- [Phase 4d: Integration](#phase-4d-integration)
- [Phase 5: Quality Gates](#phase-5-quality-gates)
- [Quality Gate Commands](#quality-gate-commands)
- [Success Criteria](#success-criteria)

---

## Weekly Schedule

| Week | Phase | Focus | Deliverables | Quality Gates |
|------|-------|-------|--------------|---------------|
| 1-2 | 4a | Task Management | erlmcp_task_manager.erl + tests | Compile, EUnit, Benchmarks |
| 3-4 | 4b | Completion API | erlmcp_completion.erl + tests | Compile, EUnit, Benchmarks |
| 5-6 | 4c | Elicitation | erlmcp_elicitation.erl + tests | Compile, EUnit, Benchmarks |
| 7-8 | 4d | Integration | Server/client integration + tests | Compile, EUnit, CT, Dialyzer, Xref |
| 9-10 | 5 | Quality Gates | Validation + release | ALL quality gates |

**Parallel Execution**: Phases 4b and 4c can run concurrently with different agents.

---

## Phase 4a: Task Management

**Duration**: 2 weeks
**Module**: `erlmcp_task_manager.erl`
**Agent**: erlang-otp-developer

### Key Files

```
apps/erlmcp_core/src/erlmcp_task_manager.erl       (new)
apps/erlmcp_core/src/erlmcp_task_manager_sup.erl    (new)
apps/erlmcp_core/test/erlmcp_task_manager_tests.erl (new)
apps/erlmcp_core/test/erlmcp_task_manager_SUITE.erl (new)
bench/erlmcp_bench_task_manager.erl                (new)
```

### API Summary

```erlang
% Task creation
-spec create_task(handler(), metadata()) -> {ok, task_id()} | {error, term()}.

% Task retrieval
-spec get_task(task_id()) -> {ok, task()} | {error, not_found}.

% Task listing (pagination)
-spec list_tasks(cursor(), limit()) -> {[task()], next_cursor()}.

% Task execution
-spec execute_task(task_id()) -> ok | {error, term()}.

% Task cancellation
-spec cancel_task(task_id()) -> ok | {error, term()}.
```

### Performance Targets

- Task creation: **<10ms (p99)**
- Task retrieval: **<50ms (p99)**
- Task cancellation: **<10ms (p99)**

### Daily Tasks (Week 1-2)

**Day 1-2**: Module skeleton + basic tests
**Day 3-4**: Task creation algorithm
**Day 5-6**: Task state machine
**Day 7-8**: Task listing + pagination
**Day 9-10**: Task cancellation + integration

### Quality Gates (End of Week 2)

```bash
TERM=dumb rebar3 compile                          # 0 errors, 0 warnings
rebar3 eunit --module=erlmcp_task_manager_tests   # 100% pass, ≥80% coverage
rebar3 ct --suite=erlmcp_task_manager_SUITE      # 100% pass
rebar3 dialyzer -M erlmcp_task_manager            # 0 warnings
erlmcp_bench_task_manager:run(task_creation_1000) # <10ms p99
```

---

## Phase 4b: Completion API

**Duration**: 2 weeks
**Module**: `erlmcp_completion.erl`
**Agent**: erlang-otp-developer

### Key Files

```
apps/erlmcp_core/src/erlmcp_completion.erl       (new)
apps/erlmcp_core/src/erlmcp_completion_sup.erl    (new)
apps/erlmcp_core/test/erlmcp_completion_tests.erl (new)
apps/erlmcp_core/test/erlmcp_completion_SUITE.erl (new)
bench/erlmcp_bench_completion.erl                (new)
```

### API Summary

```erlang
% Resource completion (file paths)
-spec complete_resource(partial_path(), context()) -> [{completion(), score()}].

% Command completion (tool names)
-spec complete_command(partial_name(), context()) -> [{completion(), score()}].

% Cache management
-spec invalidate_cache(type()) -> ok.
-spec check_cache(key()) -> {ok, cached} | {error, not_cached}.
```

### Performance Targets

- Completion generation: **<100ms (p99)**
- Cache hit rate: **≥50%**
- Ranking accuracy: **≥70%**

### Daily Tasks (Week 3-4)

**Day 1-2**: Module skeleton + basic tests
**Day 3-4**: Resource completion (file paths)
**Day 5-6**: Command completion (tool names)
**Day 7-8**: Ranking algorithm optimization
**Day 9-10**: Integration and caching

### Quality Gates (End of Week 4)

```bash
TERM=dumb rebar3 compile                          # 0 errors, 0 warnings
rebar3 eunit --module=erlmcp_completion_tests     # 100% pass, ≥80% coverage
rebar3 ct --suite=erlmcp_completion_SUITE        # 100% pass
rebar3 dialyzer -M erlmcp_completion              # 0 warnings
erlmcp_bench_completion:run(completion_1000)     # <100ms p99
```

---

## Phase 4c: Elicitation

**Duration**: 2 weeks
**Module**: `erlmcp_elicitation.erl`
**Agent**: erlang-otp-developer

### Key Files

```
apps/erlmcp_core/src/erlmcp_elicitation.erl       (new)
apps/erlmcp_core/src/erlmcp_elicitation_sup.erl    (new)
apps/erlmcp_core/test/erlmcp_elicitation_tests.erl (new)
apps/erlmcp_core/test/erlmcp_elicitation_SUITE.erl (new)
bench/erlmcp_bench_elicitation.erl                (new)
```

### API Summary

```erlang
% Elicitation creation
-spec create_elicitation(client_id(), resource(), permissions()) ->
    {ok, url()} | {error, term()}.

% Elicitation validation
-spec validate_elicitation(elicitation_id(), signature()) ->
    {ok, valid} | {error, term()}.

% Elicitation approval
-spec approve_elicitation(elicitation_id()) -> ok | {error, term()}.

% Elicitation rejection
-spec reject_elicitation(elicitation_id()) -> ok | {error, term()}.
```

### Performance Targets

- Elicitation creation: **<50ms (p99)**
- Signature verification: **<5ms (p99)**
- Timer cleanup: **<10ms (p99)**

### Daily Tasks (Week 5-6)

**Day 1-2**: Module skeleton + basic tests
**Day 3-4**: URL generation + signature
**Day 5-6**: URL validation
**Day 7-8**: Expiry timer management
**Day 9-10**: Client notifications + integration

### Quality Gates (End of Week 6)

```bash
TERM=dumb rebar3 compile                          # 0 errors, 0 warnings
rebar3 eunit --module=erlmcp_elicitation_tests    # 100% pass, ≥80% coverage
rebar3 ct --suite=erlmcp_elicitation_SUITE       # 100% pass
rebar3 dialyzer -M erlmcp_elicitation             # 0 warnings
erlmcp_bench_elicitation:run(elicitation_create)  # <50ms p99
```

---

## Phase 4d: Integration

**Duration**: 2 weeks
**Focus**: Integrate new modules with existing system
**Agents**: erlang-otp-developer + erlang-test-engineer

### Key Files to Modify

```
apps/erlmcp_core/src/erlmcp_server.erl   (modify)
apps/erlmcp_core/src/erlmcp_client.erl   (modify)
apps/erlmcp_core/src/erlmcp_json_rpc.erl (modify)
apps/erlmcp_core/include/erlmcp.hrl      (modify)
```

### Daily Tasks (Week 7-8)

**Day 1-3**: Server handler integration
- Add JSON-RPC handlers for tasks, completion, elicitation
- Update capability negotiation
- Write server integration tests

**Day 4-6**: Client API integration
- Add client API functions
- Add notification handlers
- Write client integration tests

**Day 7-9**: JSON-RPC message encoding
- Add message encoders/decoders
- Write encoding tests

**Day 10**: Backward compatibility testing
- Verify existing features still work
- Test mixed old + new workflows
- Write backward compatibility tests

### Quality Gates (End of Week 8)

```bash
TERM=dumb rebar3 compile            # 0 errors, 0 warnings
rebar3 eunit                        # 100% pass, ≥80% coverage
rebar3 ct                           # 100% pass
rebar3 dialyzer                     # 0 warnings
rebar3 xref                         # 0 issues
rebar3 ct --suite=erlmcp_backward_compat_SUITE  # 100% pass
```

---

## Phase 5: Quality Gates

**Duration**: 2 weeks
**Focus**: Quality validation, release preparation
**Agents**: erlang-test-engineer + code-reviewer + performance-benchmarker

### Daily Tasks (Week 9-10)

**Day 1**: Gate 1 - Compilation
**Day 1-2**: Gate 2 - Unit Tests
**Day 2-3**: Gate 3 - Integration Tests
**Day 3-4**: Gate 4 - Type Checking
**Day 4**: Gate 5 - Cross-Reference
**Day 5-6**: Gate 6 - Performance Benchmarks
**Day 6-7**: Documentation Completion
**Day 7-8**: Code Review
**Day 9**: Release Preparation
**Day 10**: Final Validation

### Quality Gate Commands (All Phases)

See [Quality Gate Commands](#quality-gate-commands) section below.

---

## Quality Gate Commands

### Gate 1: Compilation

```bash
TERM=dumb rebar3 compile
```

**Expected**: 0 errors, 0 warnings

### Gate 2: Unit Tests

```bash
rebar3 eunit
```

**Expected**: 100% pass rate, ≥80% coverage

### Gate 3: Integration Tests

```bash
rebar3 ct
```

**Expected**: 100% pass rate

### Gate 4: Type Checking

```bash
rebar3 dialyzer
```

**Expected**: 0 warnings

### Gate 5: Cross-Reference

```bash
rebar3 xref
```

**Expected**: 0 undefined functions, 0 unused calls

### Gate 6: Performance Benchmarks

```bash
make benchmark-quick
```

**Expected**: <10% regression from baseline

---

## Success Criteria

### Functional Completeness

- [ ] All 6 MCP gaps implemented
- [ ] 100% API contract coverage
- [ ] Backward compatibility maintained
- [ ] 0 critical bugs

### Code Quality

- [ ] ≥80% test coverage
- [ ] 0 Dialyzer warnings
- [ ] 0 Xref issues
- [ ] All code reviewed and approved

### Performance

- [ ] Task creation < 10ms (p99)
- [ ] Task retrieval < 50ms (p99)
- [ ] Completion generation < 100ms (p99)
- [ ] Elicitation creation < 50ms (p99)
- [ ] <10% performance regression

### Reliability

- [ ] 0 critical bugs in production
- [ ] 100% test pass rate
- [ ] Graceful failure handling
- [ ] MTBF ≥ 24 hours

---

## Quick Commands

### Development Workflow

```bash
# Start development shell
make console

# Compile
TERM=dumb rebar3 compile

# Run unit tests
rebar3 eunit

# Run integration tests
rebar3 ct

# Run specific test module
rebar3 eunit --module=erlmcp_task_manager_tests

# Run specific test suite
rebar3 ct --suite=erlmcp_task_manager_SUITE

# Type checking
rebar3 dialyzer

# Cross-reference
rebar3 xref

# Performance benchmarks
make benchmark-quick

# Full test suite
make check
```

### File Templates

#### Gen Server Template

```erlang
-module(erlmcp_feature).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

#### EUnit Test Template

```erlang
-module(erlmcp_feature_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Generators
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_feature:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

feature_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
          ?_assert(true)
         ]
     end}.
```

#### Common Test Suite Template

```erlang
-module(erlmcp_feature_SUITE).
-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [feature_test].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

feature_test(_Config) ->
    ?assert(true).
```

---

## Resources

### Documentation

- Full Implementation Plan: `docs/MCP_2025-11-25_IMPLEMENTATION_PLAN.md`
- Specification: `docs/MCP_2025-11-25_SPECIFICATION_GAPS.md`
- Pseudocode: `docs/MCP_2025-11-25_PSEUDOCODE_DESIGN.md`
- Architecture: `docs/MCP_2025-11-25_ARCHITECTURE_DESIGN.md`

### SPARC Commands

```bash
# Run SPARC specification
sparc run spec

# Run SPARC pseudocode
sparc run pseudocode

# Run SPARC architecture
sparc run architect

# Run SPARC TDD
sparc tdd

# Run SPARC integration
sparc run integration
```

### Agent Commands

```bash
# Spawn erlang-otp-developer agent
/swarm spawn erlang-otp-developer

# Spawn erlang-test-engineer agent
/swarm spawn erlang-test-engineer

# Spawn code-reviewer agent
/swarm spawn code-reviewer
```

---

## Troubleshooting

### Common Issues

**Issue**: Compilation fails with "undefined function"
**Solution**: Run `rebar3 clean` and recompile

**Issue**: Tests fail with "process not alive"
**Solution**: Check supervision tree, ensure process is started

**Issue**: Dialyzer warnings
**Solution**: Add `-spec` attributes to all public functions

**Issue**: Performance regression
**Solution**: Run profiler, optimize hot paths

### Getting Help

1. Check implementation plan: `docs/MCP_2025-11-25_IMPLEMENTATION_PLAN.md`
2. Check SPARC documentation: `docs/SPARC_INTEGRATION_README.md`
3. Ask in team chat
4. Create GitHub issue

---

**Version**: 1.0
**Last Updated**: 2026-01-29
**Status**: Ready for Implementation
