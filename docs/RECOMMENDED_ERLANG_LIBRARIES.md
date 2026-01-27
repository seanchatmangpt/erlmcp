# Recommended Erlang Libraries to Close MCP Protocol Gaps

**Analysis Date**: 2026-01-27
**erlmcp Version**: 0.6.0
**Target**: Close remaining 27.5% protocol coverage gap

---

## Overview

This document recommends the most powerful and battle-tested Erlang libraries to implement the missing MCP protocol features identified in the [MCP Protocol Coverage Analysis](./MCP_PROTOCOL_COVERAGE_ANALYSIS.md).

**Current Coverage**: 72.5% → **Target Coverage**: 95%+

---

## 1. OAuth 2.0 & Authorization (0% → 90%)

### Primary Recommendation: `kivra/oauth2` + `kivra/oauth2_client`

**Why**: Industry-standard OAuth2 implementation, RFC 6749 compliant, production-proven

#### Server-Side: [kivra/oauth2](https://github.com/kivra/oauth2)
```erlang
{deps, [
    {oauth2, "2.1.0"}
]}.
```

**Features**:
- ✅ Full RFC 6749 server implementation
- ✅ All grant types (authorization code, client credentials, password, implicit)
- ✅ Token generation and validation
- ✅ Refresh token support
- ✅ Pluggable authentication & persistence
- ✅ MIT licensed

**Use For**:
- Implementing OAuth2 authorization server
- Token generation and validation
- Secure HTTP transport authentication

#### Client-Side: [kivra/oauth2_client](https://github.com/kivra/oauth2_client)
```erlang
{deps, [
    {oauth2_client, "1.0.0"}
]}.
```

**Features**:
- ✅ Automatic token refresh
- ✅ REST client wrapper
- ✅ Multiple grant types
- ✅ Azure AD support
- ✅ Handles token expiration gracefully

**Use For**:
- MCP client authentication
- Consuming OAuth2-protected MCP servers
- Resource Indicators (RFC 8707) implementation

**Implementation Effort**: Medium (2-3 weeks)
**Priority**: 🔴 **CRITICAL** (Required by June 2025 spec)

**References**:
- [OAuth Libraries for Erlang](https://oauth.net/code/erlang/)
- [kivra/oauth2 GitHub](https://github.com/kivra/oauth2)
- [kivra/oauth2_client GitHub](https://github.com/kivra/oauth2_client)

---

## 2. WebSocket Transport (0% → 100%)

### Primary Recommendation: `cowboy` (Server) + `gun` (Client)

**Why**: Industry-standard, high-performance, mature ecosystem from Nine Nines

#### Server-Side: [cowboy](https://github.com/ninenines/cowboy)
```erlang
{deps, [
    {cowboy, "2.10.0"}  % Already in project!
]}.
```

**Features**:
- ✅ WebSocket support (RFC 6455)
- ✅ HTTP/1.1, HTTP/2
- ✅ Small, fast, modular
- ✅ Battle-tested in production
- ✅ Excellent documentation
- ✅ Already used for TCPS dashboard!

**Use For**:
- MCP server WebSocket transport
- Browser-friendly bidirectional communication
- Replace or augment HTTP transport

**Example**:
```erlang
% Cowboy WebSocket handler
-module(erlmcp_websocket_handler).
-behaviour(cowboy_websocket).

init(Req, State) ->
    {cowboy_websocket, Req, State}.

websocket_handle({text, Msg}, State) ->
    % Parse JSON-RPC message
    JsonRpcMsg = jsx:decode(Msg),
    % Handle MCP request
    Response = erlmcp_server:handle_request(JsonRpcMsg),
    {reply, {text, jsx:encode(Response)}, State}.
```

#### Client-Side: [gun](https://github.com/ninenines/gun)
```erlang
{deps, [
    {gun, "2.0.1"}  % Already in project!
]}.
```

**Features**:
- ✅ WebSocket client (RFC 6455)
- ✅ HTTP/1.1, HTTP/2 client
- ✅ Asynchronous, always-connected
- ✅ Automatic reconnection
- ✅ Built-in connection pooling
- ✅ Already used for HTTP transport!

**Use For**:
- MCP client WebSocket connections
- Browser-based MCP clients
- Bidirectional persistent connections

**Implementation Effort**: Low (1-2 weeks) - Already have both libraries!
**Priority**: 🟡 **HIGH** (Browser compatibility)

**References**:
- [Nine Nines Cowboy](https://ninenines.eu/)
- [Gun Function Reference](https://ninenines.eu/docs/en/gun/2.0/manual/)
- [Cowboy WebSocket Tutorial](https://marcelog.github.io/articles/erlang_websocket_server_cowboy_tutorial.html)
- [Gun GitHub](https://github.com/ninenines/gun)

---

## 3. SSE (Server-Sent Events) Transport (0% → 90%)

### Primary Recommendation: `erlang-sse` (Cowboy-based)

**Why**: Cowboy integration, W3C compliant, topic-based events

#### [erlang-sse](https://github.com/linearregression/erlang-sse)
```erlang
{deps, [
    {erlang_sse, {git, "https://github.com/linearregression/erlang-sse.git", {branch, "master"}}}
]}.
```

**Features**:
- ✅ W3C Server-Sent Events spec compliant
- ✅ Built on Cowboy (already in project!)
- ✅ Topic-based event sources
- ✅ ATOM syndication format
- ✅ Command-line tools for testing
- ✅ Lightweight process model

**Use For**:
- MCP server → client notifications
- Browser-based MCP clients (unidirectional)
- Real-time updates without WebSocket overhead

**Alternative**: [mod_sse](https://github.com/dozzie/mod_sse) for inets/httpd

**Example**:
```erlang
% SSE endpoint for MCP notifications
init(Req, State) ->
    Headers = #{
        <<"content-type">> => <<"text/event-stream">>,
        <<"cache-control">> => <<"no-cache">>,
        <<"connection">> => <<"keep-alive">>
    },
    Req2 = cowboy_req:stream_reply(200, Headers, Req),

    % Send MCP notifications as SSE events
    send_sse_event(Req2, <<"notifications/resources/updated">>, #{
        <<"uri">> => <<"file:///data/config.json">>
    }),

    {ok, Req2, State}.

send_sse_event(Req, EventType, Data) ->
    Event = [
        <<"event: ">>, EventType, <<"\n">>,
        <<"data: ">>, jsx:encode(Data), <<"\n\n">>
    ],
    cowboy_req:stream_body(Event, nofin, Req).
```

**Implementation Effort**: Low (1-2 weeks)
**Priority**: 🟢 **MEDIUM** (Nice-to-have for browser clients)

**References**:
- [erlang-sse GitHub](https://github.com/linearregression/erlang-sse)
- [Erlang & SSE Made for Each Other](https://drupalsun.com/romain-gauthier/2012/01/05/erlang-server-sent-events-made-each-other)
- [mod_sse GitHub](https://github.com/dozzie/mod_sse)

---

## 4. Task Queue & Job Processing (0% → 95%)

### Primary Recommendation: `jobs` (Load Regulation)

**Why**: OTP-native, proven in production, perfect for MCP task management

#### [uwiger/jobs](https://github.com/uwiger/jobs)
```erlang
{deps, [
    {jobs, "0.10.0"}
]}.
```

**Features**:
- ✅ Load regulation and rate limiting
- ✅ Configurable queues with throughput control
- ✅ Credit pool management
- ✅ Feedback compensation
- ✅ OTP supervision tree integration
- ✅ Production-proven (Ulf Wiger library)

**Use For**:
- MCP Tasks API (`tasks/get`, `tasks/cancel`, `tasks/list`)
- Long-running tool execution
- Task-augmented sampling
- Rate-limited operations

**Example**:
```erlang
% Configure job queue for MCP tasks
jobs:add_queue(mcp_tasks, [
    {regulators, [
        {rate, [{limit, 100}]},  % 100 tasks/sec max
        {counter, [{limit, 1000}]}  % 1000 concurrent tasks max
    ]},
    {type, fifo}
]).

% Submit MCP task
TaskId = jobs:run(mcp_tasks, fun() ->
    % Long-running tool execution
    erlmcp_tools:call_tool(ToolName, Args)
end).

% Get task status
case jobs:info(TaskId) of
    {ok, Status} -> Status;
    undefined -> not_found
end.
```

### Alternative: Native OTP + ETS

**For simpler cases**, build on top of OTP's `gen_server` + `ets`:

```erlang
% Minimal task manager
-module(erlmcp_task_manager).
-behaviour(gen_server).

% Store tasks in ETS
init([]) ->
    Tid = ets:new(mcp_tasks, [set, public, named_table,
                               {read_concurrency, true}]),
    {ok, #{tasks => Tid}}.

% Create task
handle_call({create_task, Fun}, _From, State) ->
    TaskId = generate_task_id(),
    Pid = spawn_link(fun() -> execute_task(TaskId, Fun) end),
    ets:insert(mcp_tasks, {TaskId, #{
        pid => Pid,
        status => running,
        created => erlang:timestamp()
    }}),
    {reply, {ok, TaskId}, State}.

% Cancel task
handle_call({cancel_task, TaskId}, _From, State) ->
    case ets:lookup(mcp_tasks, TaskId) of
        [{TaskId, #{pid := Pid}}] ->
            exit(Pid, cancelled),
            ets:update_element(mcp_tasks, TaskId,
                               {2, #{status => cancelled}}),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end.
```

**Implementation Effort**: Low-Medium (1-3 weeks depending on approach)
**Priority**: 🔴 **CRITICAL** (Required for 2025-11-25 spec)

**References**:
- [jobs GitHub](https://github.com/uwiger/jobs)
- [Erlang Job Queue Libraries](https://hex.pm/packages?search=job+queue)
- [Background Processing with GenServer](https://hackernoon.com/background-processing-using-elixir-genserver-and-the-erlang-queue-class-8d476d4942c2)

---

## 5. Form Rendering & Elicitation (0% → 80%)

### Primary Recommendation: Build on existing `bbmustache` + HTML forms

**Why**: Already in project, simple and powerful templating

#### [bbmustache](https://github.com/soranoba/bbmustache) (Already included!)
```erlang
{deps, [
    {bbmustache, "1.12.2"}  % Already in project!
]}.
```

**Features**:
- ✅ Mustache template engine
- ✅ Fast rendering
- ✅ Logic-less templates
- ✅ Already used for TCPS artifacts

**Use For**:
- Rendering HTML forms for elicitation
- Dynamic UI generation
- Template-based responses

**Example**:
```erlang
% Elicitation form template
FormTemplate = <<"
<form id='{{form_id}}'>
  <h3>{{title}}</h3>
  <p>{{description}}</p>
  {{#fields}}
  <div>
    <label for='{{id}}'>{{label}}</label>
    <input type='{{type}}' id='{{id}}' name='{{id}}'
           {{#required}}required{{/required}} />
  </div>
  {{/fields}}
  <button type='submit'>Submit</button>
</form>
">>.

% Render form
FormHtml = bbmustache:render(FormTemplate, #{
    form_id => <<"user_info">>,
    title => <<"User Information">>,
    description => <<"Please provide your details">>,
    fields => [
        #{id => <<"name">>, label => <<"Name">>,
          type => <<"text">>, required => true},
        #{id => <<"email">>, label => <<"Email">>,
          type => <<"email">>, required => true}
    ]
}).

% Return as elicitation response
#{
    <<"jsonrpc">> => <<"2.0">>,
    <<"id">> => RequestId,
    <<"result">> => #{
        <<"type">> => <<"form">>,
        <<"content">> => FormHtml,
        <<"elicitation_id">> => generate_elicitation_id()
    }
}.
```

**Implementation Effort**: Low (1 week)
**Priority**: 🟡 **HIGH** (New 2025-11-25 spec feature)

**References**:
- [bbmustache GitHub](https://github.com/soranoba/bbmustache)

---

## 6. Icon & Metadata Handling (0% → 90%)

### Primary Recommendation: Build with existing HTTP libraries

**No new dependencies needed!** Use existing `gun` for fetching + validation

**Implementation**:
```erlang
% Icon metadata structure
-record(icon, {
    src :: binary(),           % URI (https:// or data:)
    mime_type :: binary(),     % image/png, image/svg+xml, etc
    sizes :: [binary()],       % [<<"48x48">>, <<"96x96">>]
    theme :: light | dark      % Optional theme
}).

% Icon validation
validate_icon_uri(<<"https://", _/binary>> = Uri) ->
    % Verify HTTPS
    % Check same-origin with server
    % Fetch and validate MIME type
    {ok, Uri};
validate_icon_uri(<<"data:", _/binary>> = DataUri) ->
    % Parse data URI
    % Validate base64 encoding
    % Check MIME type
    {ok, DataUri};
validate_icon_uri(_) ->
    {error, invalid_scheme}.

% Secure icon fetching
fetch_icon(IconUri, ServerOrigin) ->
    % Verify same origin
    case uri_string:parse(IconUri) of
        #{host := Host} when Host == ServerOrigin ->
            % Fetch without credentials
            gun:open(Host, 443, #{
                transport => tls,
                protocols => [http2, http]
            }),
            gun:get(ConnPid, Path, [
                {<<"accept">>, <<"image/*">>}
            ]),
            % Receive and validate content
            receive_and_validate_icon();
        _ ->
            {error, origin_mismatch}
    end.
```

**Implementation Effort**: Low (1 week)
**Priority**: 🟢 **MEDIUM** (UX enhancement)

---

## 7. Autocomplete & Argument Completion (0% → 85%)

### Primary Recommendation: Custom implementation with pattern matching

**No dependencies needed!** Pure Erlang pattern matching

**Implementation**:
```erlang
% Completion for tool arguments
-module(erlmcp_completion).

complete_argument(ToolName, ArgumentName, Partial) ->
    case erlmcp_registry:get_tool(ToolName) of
        {ok, Tool} ->
            Schema = maps:get(<<"inputSchema">>, Tool),
            ArgSchema = get_argument_schema(Schema, ArgumentName),
            generate_completions(ArgSchema, Partial);
        error ->
            {error, tool_not_found}
    end.

generate_completions(#{<<"enum">> := Enum}, Partial) ->
    % Enum completion
    Matches = [V || V <- Enum,
                    binary:match(V, Partial) =/= nomatch],
    #{<<"values">> => Matches, <<"hasMore">> => false};

generate_completions(#{<<"type">> := <<"string">>,
                       <<"pattern">> := _}, Partial) ->
    % Pattern-based suggestions
    #{<<"values">> => suggest_from_history(Partial),
      <<"hasMore">> => true};

generate_completions(_, _) ->
    #{<<"values">> => [], <<"hasMore">> => false}.
```

**Implementation Effort**: Low (1 week)
**Priority**: 🟢 **MEDIUM** (UX enhancement)

---

## 8. Advanced Observability (Already 72% with OpenTelemetry)

### Complement with: `recon` (Production Debugging)

#### [recon](https://github.com/ferd/recon)
```erlang
{deps, [
    {recon, "2.5.3"}  % May already be in dev profile
]}.
```

**Features**:
- ✅ Production-safe debugging
- ✅ Process inspection
- ✅ Memory analysis
- ✅ Performance profiling
- ✅ No production overhead

**Use For**:
- Debugging MCP task execution
- Memory leak detection
- Performance bottleneck analysis
- Production monitoring

**Implementation Effort**: None (add to dev dependencies)
**Priority**: 🟢 **LOW** (Already have OpenTelemetry)

---

## Implementation Priority Matrix

| Gap | Library | Effort | Priority | Impact | Dependencies |
|-----|---------|--------|----------|--------|--------------|
| **OAuth 2.0** | kivra/oauth2 + oauth2_client | Medium | 🔴 CRITICAL | Security compliance | None |
| **Tasks API** | jobs or native OTP | Low-Med | 🔴 CRITICAL | Core feature | None |
| **WebSocket** | cowboy + gun | Low | 🟡 HIGH | Browser support | ✅ Already have! |
| **Elicitation** | bbmustache | Low | 🟡 HIGH | Core feature | ✅ Already have! |
| **Roots** | Native OTP | Low | 🟡 HIGH | Security | None |
| **SSE** | erlang-sse | Low | 🟢 MEDIUM | Browser support | cowboy ✅ |
| **Icons** | Native + gun | Low | 🟢 MEDIUM | UX | gun ✅ |
| **Completion** | Native | Low | 🟢 MEDIUM | UX | None |

---

## Recommended Dependencies Update

### Add to `rebar.config`:

```erlang
{deps, [
    % ... existing deps ...

    % OAuth 2.0 (CRITICAL)
    {oauth2, "2.1.0"},
    {oauth2_client, "1.0.0"},

    % Task processing (CRITICAL)
    {jobs, "0.10.0"},

    % SSE transport (if needed)
    {erlang_sse, {git, "https://github.com/linearregression/erlang-sse.git",
                  {branch, "master"}}}
]}.

{profiles, [
    {dev, [
        {deps, [
            {recon, "2.5.3"},
            {observer_cli, "1.7.4"}  % Already have
        ]}
    ]}
]}.
```

**Note**: WebSocket, elicitation, icons, and completion don't require new dependencies - use existing libraries!

---

## Implementation Phases with Libraries

### Phase 1: Security & Auth (4-6 weeks)
**Libraries**: `kivra/oauth2`, `kivra/oauth2_client`
- [ ] Add OAuth2 dependencies
- [ ] Implement authorization server
- [ ] Implement client authentication
- [ ] Add Resource Indicators (RFC 8707)
- [ ] Token management and refresh

### Phase 2: Task Management (2-3 weeks)
**Libraries**: `uwiger/jobs` or native OTP
- [ ] Add jobs dependency (or build native)
- [ ] Implement `tasks/*` methods
- [ ] Task lifecycle management
- [ ] Progress tracking
- [ ] Cancellation support

### Phase 3: Enhanced Transports (3-4 weeks)
**Libraries**: `cowboy` (✅), `gun` (✅), `erlang-sse`
- [ ] WebSocket server (cowboy)
- [ ] WebSocket client (gun)
- [ ] SSE transport (erlang-sse)
- [ ] Transport tests

### Phase 4: Client Features (2-3 weeks)
**Libraries**: `bbmustache` (✅), native OTP
- [ ] Elicitation forms (bbmustache)
- [ ] Roots validation (native)
- [ ] Completion (native)

### Phase 5: UI & Polish (1-2 weeks)
**Libraries**: Native + `gun` (✅)
- [ ] Icon metadata support
- [ ] Icon validation and fetching
- [ ] Security hardening

**Total**: 12-18 weeks to reach 95%+ coverage

---

## Cost-Benefit Analysis

### Maximum Leverage (Already Have These!)
- ✅ **cowboy**: WebSocket server + HTTP/2
- ✅ **gun**: WebSocket client + HTTP/2
- ✅ **bbmustache**: Forms and templating
- ✅ **jsx**: JSON processing
- ✅ **jesse**: JSON Schema validation

**Savings**: ~6-8 weeks of implementation time!

### Must Add (High ROI)
- **kivra/oauth2**: Industry-standard OAuth2 (saves 4-6 weeks vs custom)
- **jobs**: Production-proven task queue (saves 2-3 weeks vs custom)

### Optional (Low Dependency Cost)
- **erlang-sse**: Simple SSE support (saves 1-2 weeks)
- **recon**: Production debugging (saves debugging hours)

---

## Alternative Approaches

### If Minimizing Dependencies

Build everything with **OTP primitives only**:
- Tasks: `gen_server` + `ets` + `supervisor`
- WebSocket: `cowboy_websocket` (already have)
- OAuth: Custom JWT validation (risky, not recommended)
- Forms: HTML strings (no templates)

**Tradeoff**: More code to maintain, potential bugs, longer development time

### If Using Elixir Ecosystem

Many more options available (all run on Erlang VM):
- `oban`: Superior task queue with PostgreSQL
- `ueberauth/oauth2`: Modern OAuth2 client
- `phoenix`: WebSocket + channels

**Tradeoff**: Mixing languages, larger dependencies

---

## Conclusion

### Best Path Forward

1. **Add minimal dependencies**: `kivra/oauth2`, `kivra/oauth2_client`, `uwiger/jobs`
2. **Leverage existing libraries**: cowboy, gun, bbmustache all support missing features!
3. **Build native for simple features**: Icons, completions, roots validation

**Result**: Reach 95%+ MCP coverage with only **3 new major dependencies** and 12-18 weeks effort.

### Why These Libraries

- ✅ **Battle-tested** in production Erlang systems
- ✅ **Actively maintained** with recent updates
- ✅ **Well-documented** with examples
- ✅ **OTP-native** design patterns
- ✅ **MIT/Apache licensed** (permissive)
- ✅ **Community-trusted** (Ulf Wiger, Nine Nines, Kivra)

---

## References

### OAuth & Security
- [OAuth Libraries for Erlang](https://oauth.net/code/erlang/)
- [kivra/oauth2 GitHub](https://github.com/kivra/oauth2)
- [kivra/oauth2_client GitHub](https://github.com/kivra/oauth2_client)

### WebSocket & Real-Time
- [Nine Nines Cowboy](https://ninenines.eu/)
- [Gun GitHub](https://github.com/ninenines/gun)
- [Cowboy WebSocket Tutorial](https://marcelog.github.io/articles/erlang_websocket_server_cowboy_tutorial.html)
- [erlang-sse GitHub](https://github.com/linearregression/erlang-sse)
- [Erlang & SSE](https://drupalsun.com/romain-gauthier/2012/01/05/erlang-server-sent-events-made-each-other)

### Task Processing
- [jobs GitHub](https://github.com/uwiger/jobs)
- [Erlang Job Queue Libraries](https://hex.pm/packages?search=job+queue)
- [Background Processing Tutorial](https://hackernoon.com/background-processing-using-elixir-genserver-and-the-erlang-queue-class-8d476d4942c2)

### Templates & UI
- [bbmustache GitHub](https://github.com/soranoba/bbmustache)

### Debugging & Monitoring
- [recon GitHub](https://github.com/ferd/recon)

---

**Document Version**: 1.0
**Last Updated**: 2026-01-27
**Next Review**: After Phase 1 implementation
