# OTP Compliance Quick Reference Card

**For:** erlmcp-flow developers
**Version:** 1.0.0
**Full Guide:** `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md`

---

## TL;DR - OTP Compliance Checklist

Before committing OTP code:

```
✅ All 6 gen_server callbacks exported
✅ init/1 never blocks (< 100ms)
✅ All processes supervised (no spawn)
✅ Messages are typed tuples/records
✅ Timeouts ≥ 5000ms
✅ Let-it-crash (no defensive programming)
✅ Real processes in tests (no mocks)
✅ No sys:get_status in tests
✅ Type specs for all exported functions
✅ Documentation complete
```

---

## Quick Patterns

### 1. gen_server Template

```erlang
-module(my_server).
-behaviour(gen_server).

%% API
-export([start_link/1, do_something/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {
    data :: map()
}).

%%% API
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec do_something(pid(), term()) -> ok | {error, term()}.
do_something(Server, Args) ->
    gen_server:call(Server, {do_something, Args}, 5000).

%%% Callbacks

%% Non-blocking init
init(Opts) ->
    %% Fast return
    {ok, #state{data = #{}}, {continue, initialize}}.

%% Async initialization
handle_continue(initialize, State) ->
    %% Safe to block here
    Data = load_data(),
    {noreply, State#state{data = Data}}.

%% Synchronous (request-response)
handle_call({do_something, Args}, _From, State) ->
    Result = process(Args, State),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Asynchronous (fire-and-forget)
handle_cast({notify, Event}, State) ->
    NewState = handle_event(Event, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Internal messages
handle_info(timeout, State) ->
    NewState = handle_timeout(State),
    {noreply, NewState};
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    NewState = cleanup_monitor(Ref, Pid, State),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

%% Graceful cleanup
terminate(_Reason, #state{data = Data}) ->
    cleanup(Data),
    ok.

%% Hot code upgrade
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

### 2. Supervisor Template

```erlang
-module(my_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, [Args]).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => worker,
        start => {my_server, start_link, []},
        restart => transient,  % Restart only on abnormal exit
        shutdown => 5000,
        type => worker,
        modules => [my_server]
    },

    {ok, {SupFlags, [ChildSpec]}}.
```

### 3. Non-Blocking init/1

```erlang
%% ❌ WRONG
init(Opts) ->
    {ok, Conn} = connect_db(Opts),  % BLOCKS!
    {ok, #state{conn = Conn}}.

%% ✅ CORRECT - Pattern 1 (OTP 21+)
init(Opts) ->
    {ok, #state{opts = Opts}, {continue, initialize}}.

handle_continue(initialize, State = #state{opts = Opts}) ->
    {ok, Conn} = connect_db(Opts),
    {noreply, State#state{conn = Conn}}.

%% ✅ CORRECT - Pattern 2 (OTP 20)
init(Opts) ->
    self() ! initialize,
    {ok, #state{opts = Opts}}.

handle_info(initialize, State = #state{opts = Opts}) ->
    {ok, Conn} = connect_db(Opts),
    {noreply, State#state{conn = Conn}}.
```

### 4. Typed Messages

```erlang
%% ❌ WRONG
handle_info({update, Data}, State) ->  % Ambiguous
    {noreply, State}.

%% ✅ CORRECT
-type server_msg() ::
    {resource_updated, uri(), metadata()} |
    {tool_added, name(), handler()} |
    timeout.

handle_info({resource_updated, Uri, Meta}, State) ->
    NewState = update_resource(Uri, Meta, State),
    {noreply, NewState};
handle_info({tool_added, Name, Handler}, State) ->
    NewState = register_tool(Name, Handler, State),
    {noreply, NewState};
handle_info(timeout, State) ->
    {noreply, handle_timeout(State)}.
```

### 5. Let-It-Crash

```erlang
%% ❌ WRONG - Defensive programming
handle_call({process, Data}, _From, State) ->
    try
        Result = process_data(Data),
        {reply, {ok, Result}, State}
    catch
        error:Reason ->
            {reply, {error, Reason}, State}  % Hiding error
    end.

%% ✅ CORRECT - Let it crash
handle_call({process, Data}, _From, State) ->
    Result = process_data(Data),  % Crashes if invalid
    {reply, Result, State}.

%% ✅ CORRECT - Catch only expected errors
handle_call({safe_lookup, Key}, _From, State = #state{data = Data}) ->
    case maps:get(Key, Data, not_found) of
        not_found -> {reply, {error, not_found}, State};
        Value -> {reply, {ok, Value}, State}
    end.
```

### 6. Monitor Pattern

```erlang
%% Monitor for resource cleanup
start_worker(Args) ->
    Pid = spawn(fun() -> worker_loop(Args) end),
    Ref = erlang:monitor(process, Pid),
    {ok, Pid, Ref}.

%% Handle worker death
handle_info({'DOWN', Ref, process, Pid, _Reason}, State) ->
    NewState = cleanup_worker(Ref, Pid, State),
    {noreply, NewState}.

%% Demonitor when done
cleanup(Ref) ->
    erlang:demonitor(Ref, [flush]).
```

### 7. Test Pattern (Chicago TDD)

```erlang
%% ❌ WRONG - Mock usage
test_with_mock() ->
    meck:new(erlmcp_registry),
    meck:expect(erlmcp_registry, lookup, fun(_) -> {ok, self()} end),
    %% test
    meck:unload(erlmcp_registry).

%% ✅ CORRECT - Real processes
test_with_real_processes() ->
    {ok, Reg} = erlmcp_registry:start_link(),
    {ok, Server} = erlmcp_server:start_link(id, #{}),

    %% Test observable behavior
    ok = erlmcp_server:add_tool(Server, <<"tool">>, Handler),
    {ok, Tools} = erlmcp_server:list_tools(Server),
    ?assert(lists:member(<<"tool">>, Tools)),

    %% Cleanup
    erlmcp_server:stop(Server),
    erlmcp_registry:stop(Reg).
```

---

## Code Review Quick Checklist

Copy this to your PR description:

```markdown
## OTP Compliance Checklist

### gen_server/gen_statem
- [ ] All 6 callbacks exported (gen_server) or 7 (gen_statem)
- [ ] init/1 returns in < 100ms (no blocking)
- [ ] {continue, ...} or self() ! msg for async init
- [ ] Proper message handling (call/cast/info)
- [ ] Graceful terminate/2

### Supervision
- [ ] All processes supervised (no bare spawn)
- [ ] Restart strategy appropriate (permanent/transient/temporary)
- [ ] Intensity/period configured
- [ ] Shutdown timeouts set

### Messages & Errors
- [ ] All messages typed (records/tagged tuples)
- [ ] Timeouts ≥ 5000ms
- [ ] Let-it-crash (no defensive code)
- [ ] Monitors for cleanup

### Testing
- [ ] Real erlmcp processes (no mocks)
- [ ] No sys:get_status/get_state
- [ ] Observable behavior testing
- [ ] Proper cleanup

### Documentation
- [ ] Module @doc header
- [ ] All exports have -spec
- [ ] Custom types defined
- [ ] @doc for public API

### Quality Gates
- [ ] Compiles (0 errors)
- [ ] Tests pass (0 failures)
- [ ] Coverage ≥ 80%
- [ ] No dialyzer warnings
- [ ] No xref undefined calls
```

---

## Anti-Patterns Detection

```bash
# Check for common violations

# Unsupervised spawn
grep -r "spawn(fun()" apps/*/src/

# Mock usage in tests
grep -r "meck:new\|meck:expect" apps/*/test/

# State inspection
grep -r "sys:get_status\|sys:get_state" apps/*/test/

# Short timeouts
grep -r "gen_server:call.*[0-9]\{1,3\})" apps/*/src/

# Defensive error handling
grep -r "try.*catch.*error" apps/*/src/ | grep -v "%"
```

---

## Restart Strategies

| Strategy | When | Example |
|----------|------|---------|
| `permanent` | Must always run | Registry, core services |
| `transient` | Restart on crash only | Workers, connections |
| `temporary` | Never restart | One-off tasks |

| Supervision | When | Children |
|-------------|------|----------|
| `one_for_one` | Independent | Most common |
| `one_for_all` | Tightly coupled | Rare |
| `simple_one_for_one` | Dynamic pool | Workers |
| `rest_for_one` | Pipeline | Dependencies |

---

## Timeouts

| Operation | Minimum | Recommended |
|-----------|---------|-------------|
| gen_server:call | 5000ms | 5000-30000ms |
| Database ops | 10000ms | 30000ms |
| Batch processing | 30000ms | 60000ms |
| Indefinite | infinity | Use sparingly |

---

## Type Specs

```erlang
%% Basic types
-type server() :: pid().
-type result() :: {ok, term()} | {error, term()}.
-type callback() :: fun((term()) -> result()).

%% Export types
-export_type([server/0, result/0, callback/0]).

%% Function specs
-spec start_link(opts()) -> {ok, pid()} | {error, term()}.
-spec process(server(), data(), timeout()) -> result().
```

---

## Common Violations & Fixes

| Violation | Fix | Section |
|-----------|-----|---------|
| Blocking init/1 | Use {continue, ...} | 1.2 |
| Unsupervised spawn | Use supervisor:start_child | 2.1 |
| Mock in tests | Use real processes | 5.1 |
| sys:get_status | Test observable behavior | 5.2 |
| Swallowing errors | Let it crash | 4.1 |
| Timeout < 5s | Use ≥ 5000ms | 3.2 |
| Unbounded queue | Add queue limit | 3.3 |
| Missing -spec | Add type spec | 6.2 |

---

## Quick Commands

```bash
# Validate OTP compliance
make validate-otp              # Run OTP compliance checks

# Check specific patterns
make check-supervision         # Verify supervision trees
make check-blocking-init       # Find blocking init/1
make check-unsupervised        # Find unsupervised spawn

# Anti-pattern detection
./.github/scripts/chicago-tdd-scan.sh    # Chicago TDD
./scripts/otp-compliance-scan.sh         # OTP patterns
```

---

## Emergency Fixes

### "Process not supervised" error

```erlang
%% Before (WRONG)
spawn(fun() -> worker() end)

%% After (CORRECT)
supervisor:start_child(my_sup, [Args])
```

### "init/1 timeout" error

```erlang
%% Before (WRONG)
init(Opts) ->
    {ok, Conn} = slow_connect(Opts),
    {ok, #state{conn = Conn}}.

%% After (CORRECT)
init(Opts) ->
    {ok, #state{opts = Opts}, {continue, connect}}.

handle_continue(connect, State) ->
    {ok, Conn} = slow_connect(State#state.opts),
    {noreply, State#state{conn = Conn}}.
```

### "gen_server call timeout" error

```erlang
%% Before (WRONG)
gen_server:call(Server, Request)  % Default 5s

%% After (CORRECT)
gen_server:call(Server, Request, 30000)  % 30s for slow ops
```

---

## Resources

**Full Documentation:**
- `docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md` - Complete guide
- `docs/ERLMCP_FLOW_QUALITY_STANDARDS.md` - Quality standards
- `CLAUDE.md` - Project specification

**Quick Checks:**
```bash
# View this reference
cat docs/OTP_COMPLIANCE_QUICK_REFERENCE.md

# View full checklist
cat docs/ERLMCP_FLOW_OTP_COMPLIANCE_CHECKLIST.md

# Check code
make validate
```

---

**CODE LIKE A JOE ARMSTRONG AGI SWARM!!!**
