# erlmcp Architecture Refactoring Plan - v1.0.0

## Executive Summary

This document presents a comprehensive refactoring plan to address architectural debt in erlmcp, focusing on decomposing god objects and establishing hierarchical supervision trees. The refactoring will improve maintainability, testability, and OTP compliance while maintaining backward compatibility.

**Key Issues:**
- `erlmcp_server.erl` (2,040 lines) - Violates Single Responsibility Principle
- `erlmcp_capabilities.erl` (1,253 lines) - Mixed concerns (extraction, negotiation, validation)
- `erlmcp_core_sup.erl` (16 direct children) - Flat supervision tree limits fault isolation

**Target State:**
- All modules ≤500 lines (average ~400 lines)
- Hierarchical supervision with 3-tier bulkhead pattern
- Clear module boundaries following OTP behaviors
- Zero breaking changes to public API

---

## 1. Current Architecture (C4 Component Diagram)

### 1.1 Monolithic erlmcp_server Structure

```plantuml
@startuml current_server_structure
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Current erlmcp_server Architecture (2,040 lines)

Container_Boundary(server, "erlmcp_server.erl") {
    Component(gen_server, "gen_server callbacks", "Erlang", "init/1, handle_call/3, handle_cast/2, handle_info/2")
    Component(resources, "Resource Management", "Erlang", "add/delete resources, templates, subscriptions")
    Component(tools, "Tool Management", "Erlang", "add/delete tools, schema validation")
    Component(prompts, "Prompt Management", "Erlang", "add/delete prompts, arguments")
    Component(notifications, "Notification Handling", "Erlang", "progress, resource updates, handlers")
    Component(phase, "Phase Tracking", "Erlang", "Initialization state machine")
    Component(state, "State Management", "Erlang", "2,000+ line state record")

    Rel(gen_server, resources, "manages")
    Rel(gen_server, tools, "manages")
    Rel(gen_server, prompts, "manages")
    Rel(gen_server, notifications, "manages")
    Rel(gen_server, phase, "tracks")
    Rel(gen_server, state, "maintains")
}

Component_Ext(registry, "erlmcp_registry", "Process registry")
Component_Ext(capabilities, "erlmcp_capabilities", "1,253 lines")
Component_Ext(json_rpc, "erlmcp_json_rpc", "Protocol codec")

Rel(server, registry, "registers with")
Rel(server, capabilities, "uses")
Rel(server, json_rpc, "encodes/decodes")

note right of server
  **Problems:**
  - 2,040 lines (4-5x recommended size)
  - 35 exported functions
  - Mixed responsibilities
  - Difficult to test in isolation
  - Poor fault isolation
  - Hard to understand/maintain
end note

@enduml
```

### 1.2 Monolithic erlmcp_capabilities Structure

```plantuml
@startuml current_capabilities_structure
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Current erlmcp_capabilities Architecture (1,253 lines)

Container_Boundary(caps, "erlmcp_capabilities.erl") {
    Component(extraction, "Capability Extraction", "Erlang", "Client/server capability parsing")
    Component(negotiation, "Capability Negotiation", "Erlang", "Merge, negotiate, flags")
    Component(validation, "Capability Validation", "Erlang", "Protocol version, structures")
    Component(conversion, "Map Conversion", "Erlang", "Record ↔ Map transformations")
    Component(features, "Feature Management", "Erlang", "Enable/disable, check features")
    Component(degradation, "Graceful Degradation", "Erlang", "Fallback strategies")
    Component(registry, "Capability Registry", "Erlang", "Required/optional capabilities")
    Component(gen_server, "gen_server State", "Erlang", "Persistent capability state")

    Rel(extraction, conversion, "uses")
    Rel(negotiation, validation, "validates")
    Rel(negotiation, features, "manages")
    Rel(negotiation, degradation, "applies")
    Rel(registry, validation, "enforces")
    Rel(gen_server, extraction, "stores")
}

note right of caps
  **Problems:**
  - 1,253 lines (2-3x recommended size)
  - 51 exported functions
  - Mixed concerns (parsing, logic, storage)
  - Difficult to test independently
  - Unclear module boundaries
end note

@enduml
```

### 1.3 Flat erlmcp_core_sup Structure

```plantuml
@startuml current_supervision_tree
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Current Supervision Tree (Flat - 16 Workers)

Component(core_sup, "erlmcp_core_sup", "one_for_one", "244 lines, 16 children")

Component(registry, "erlmcp_registry", "worker")
Component(reload_sup, "erlmcp_reload_sup", "supervisor")
Component(session_mgr, "erlmcp_session_manager", "worker")
Component(hooks, "erlmcp_hooks", "worker")
Component(subscriptions, "erlmcp_resource_subscriptions", "worker")
Component(sse_store, "erlmcp_sse_event_store", "worker")
Component(icon_cache, "erlmcp_icon_cache", "worker")
Component(cache, "erlmcp_cache", "worker")
Component(replicator, "erlmcp_session_replicator", "worker")
Component(failover, "erlmcp_session_failover", "worker")
Component(conn_limiter, "erlmcp_connection_limiter", "worker")
Component(conn_monitor, "erlmcp_connection_monitor", "worker")
Component(cpu_quota, "erlmcp_cpu_quota", "worker")
Component(cancellation, "erlmcp_cancellation", "worker")
Component(pagination, "erlmcp_pagination", "worker")
Component(notif_sup, "erlmcp_notification_handler_sup", "supervisor")

Rel(core_sup, registry, "supervises")
Rel(core_sup, reload_sup, "supervises")
Rel(core_sup, session_mgr, "supervises")
Rel(core_sup, hooks, "supervises")
Rel(core_sup, subscriptions, "supervises")
Rel(core_sup, sse_store, "supervises")
Rel(core_sup, icon_cache, "supervises")
Rel(core_sup, cache, "supervises")
Rel(core_sup, replicator, "supervises")
Rel(core_sup, failover, "supervises")
Rel(core_sup, conn_limiter, "supervises")
Rel(core_sup, conn_monitor, "supervises")
Rel(core_sup, cpu_quota, "supervises")
Rel(core_sup, cancellation, "supervises")
Rel(core_sup, pagination, "supervises")
Rel(core_sup, notif_sup, "supervises")

note bottom of core_sup
  **Problems:**
  - 16 direct children (violates OTP guidelines: max 5-7)
  - No fault isolation bulkheads
  - Unrelated components under same supervisor
  - Single point of failure for core services
  - Difficult to reason about failure scenarios
  - Poor scalability (restart intensity)
end note

@enduml
```

---

## 2. Proposed Architecture (C4 Component Diagram)

### 2.1 Modular erlmcp_server Structure (5 Modules)

```plantuml
@startuml proposed_server_structure
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Proposed erlmcp_server Architecture (5 Focused Modules)

Container_Boundary(api, "Public API Layer") {
    Component(server, "erlmcp_server", "gen_server", "Core gen_server callbacks + delegation (400 lines)")
}

Container_Boundary(domain, "Domain Logic Layer") {
    Component(tools, "erlmcp_server_tools", "Module", "Tool management + validation (350 lines)")
    Component(resources, "erlmcp_server_resources", "Module", "Resource + template management (380 lines)")
    Component(prompts, "erlmcp_server_prompts", "Module", "Prompt + argument management (320 lines)")
    Component(notifications, "erlmcp_server_notifications", "Module", "Progress + notifications (290 lines)")
}

Container_Boundary(state, "State Management") {
    Component(state_mgr, "erlmcp_server_state", "Module", "State record + transitions (300 lines)")
}

Rel(server, tools, "delegates tool operations")
Rel(server, resources, "delegates resource operations")
Rel(server, prompts, "delegates prompt operations")
Rel(server, notifications, "delegates notifications")
Rel(server, state_mgr, "manages state")

Rel(tools, state_mgr, "updates state")
Rel(resources, state_mgr, "updates state")
Rel(prompts, state_mgr, "updates state")
Rel(notifications, state_mgr, "reads state")

Component_Ext(capabilities, "erlmcp_capabilities (refactored)", "Capability system")
Component_Ext(registry, "erlmcp_registry", "Process registry")

Rel(server, capabilities, "negotiates capabilities")
Rel(server, registry, "registers server")

note right of server
  **Benefits:**
  - Clear separation of concerns
  - Each module ~300-400 lines
  - Easy to test in isolation
  - Better fault isolation
  - Improved maintainability
  - Single Responsibility Principle
end note

@enduml
```

### 2.2 Modular erlmcp_capabilities Structure (3 Modules)

```plantuml
@startuml proposed_capabilities_structure
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Proposed erlmcp_capabilities Architecture (3 Focused Modules)

Container_Boundary(public, "Public API") {
    Component(facade, "erlmcp_capabilities", "Facade", "Public API + delegation (250 lines)")
}

Container_Boundary(domain, "Domain Logic") {
    Component(negotiation, "erlmcp_capability_negotiation", "Module", "Negotiation + merging (420 lines)")
    Component(validation, "erlmcp_capability_validation", "Module", "Validation + protocol checks (380 lines)")
    Component(registry, "erlmcp_capability_registry", "gen_server", "Required/optional tracking (350 lines)")
}

Rel(facade, negotiation, "delegates negotiation")
Rel(facade, validation, "delegates validation")
Rel(facade, registry, "queries registry")

Rel(negotiation, validation, "validates before merge")
Rel(negotiation, registry, "checks requirements")

note right of facade
  **Benefits:**
  - Clear domain boundaries
  - Testable negotiation logic
  - Reusable validation
  - Persistent registry state
  - ~350-420 lines per module
end note

@enduml
```

### 2.3 Hierarchical erlmcp_core_sup Structure (3-Tier)

```plantuml
@startuml proposed_supervision_tree
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

LAYOUT_WITH_LEGEND()

title Proposed Hierarchical Supervision Tree (3-Tier Bulkhead)

Component(core_sup, "erlmcp_core_sup", "one_for_one", "Root supervisor - 4 children")

Component(foundation_sup, "erlmcp_foundation_sup", "one_for_all", "Registry + core infrastructure")
Component(session_sup, "erlmcp_session_sup", "one_for_one", "Session management")
Component(resource_sup, "erlmcp_resource_sup", "one_for_one", "Resource services")
Component(protection_sup, "erlmcp_protection_sup", "one_for_one", "Rate limiting + quotas")

Rel(core_sup, foundation_sup, "supervises (TIER 1)")
Rel(core_sup, session_sup, "supervises (TIER 2)")
Rel(core_sup, resource_sup, "supervises (TIER 2)")
Rel(core_sup, protection_sup, "supervises (TIER 3)")

' Foundation Tier (TIER 1 - Critical)
Component(registry, "erlmcp_registry", "worker")
Component(capabilities, "erlmcp_capabilities", "worker")
Component(reload_sup_child, "erlmcp_reload_sup", "supervisor")

Rel(foundation_sup, registry, "supervises")
Rel(foundation_sup, capabilities, "supervises")
Rel(foundation_sup, reload_sup_child, "supervises")

' Session Tier (TIER 2)
Component(session_mgr, "erlmcp_session_manager", "worker")
Component(replicator, "erlmcp_session_replicator", "worker")
Component(failover, "erlmcp_session_failover", "worker")
Component(hooks, "erlmcp_hooks", "worker")

Rel(session_sup, session_mgr, "supervises")
Rel(session_sup, replicator, "supervises")
Rel(session_sup, failover, "supervises")
Rel(session_sup, hooks, "supervises")

' Resource Tier (TIER 2)
Component(subscriptions, "erlmcp_resource_subscriptions", "worker")
Component(sse_store, "erlmcp_sse_event_store", "worker")
Component(icon_cache, "erlmcp_icon_cache", "worker")
Component(cache, "erlmcp_cache", "worker")
Component(notif_sup, "erlmcp_notification_handler_sup", "supervisor")

Rel(resource_sup, subscriptions, "supervises")
Rel(resource_sup, sse_store, "supervises")
Rel(resource_sup, icon_cache, "supervises")
Rel(resource_sup, cache, "supervises")
Rel(resource_sup, notif_sup, "supervises")

' Protection Tier (TIER 3)
Component(conn_limiter, "erlmcp_connection_limiter", "worker")
Component(conn_monitor, "erlmcp_connection_monitor", "worker")
Component(cpu_quota, "erlmcp_cpu_quota", "worker")
Component(cancellation, "erlmcp_cancellation", "worker")
Component(pagination, "erlmcp_pagination", "worker")

Rel(protection_sup, conn_limiter, "supervises")
Rel(protection_sup, conn_monitor, "supervises")
Rel(protection_sup, cpu_quota, "supervises")
Rel(protection_sup, cancellation, "supervises")
Rel(protection_sup, pagination, "supervises")

note bottom of core_sup
  **Benefits:**
  - Hierarchical bulkhead isolation
  - TIER 1: Critical (registry fails → restart all)
  - TIER 2: Domain services (isolated failures)
  - TIER 3: Protection (independent restart)
  - Max 5 children per supervisor
  - Clear failure domains
  - Improved restart strategies
end note

@enduml
```

---

## 3. Dependency Graphs (Before/After)

### 3.1 Current Module Dependencies

```plantuml
@startuml current_dependencies
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

title Current Module Dependencies (Circular & Tangled)

component "erlmcp_server\n(2,040 lines)" as server #FFB6C1
component "erlmcp_capabilities\n(1,253 lines)" as caps #FFB6C1
component "erlmcp_registry\n(503 lines)" as registry
component "erlmcp_json_rpc\n(469 lines)" as json_rpc
component "erlmcp_client\n(778 lines)" as client
component "erlmcp_change_notifier" as notifier
component "erlmcp_uri_validator" as validator

server --> caps : "negotiation"
server --> registry : "registration"
server --> json_rpc : "encoding"
server --> notifier : "notifications"
server --> validator : "validation"

caps --> json_rpc : "encoding"
caps --> registry : "storage"

client --> caps : "negotiation"
client --> registry : "registration"
client --> json_rpc : "encoding"

registry --> caps : "capability checks"
caps --> registry : "capability storage"

note right of server
  **Problems:**
  - Circular dependency (registry ↔ capabilities)
  - God objects tightly coupled
  - Difficult to test in isolation
  - High change ripple effect
end note

@enduml
```

### 3.2 Proposed Module Dependencies

```plantuml
@startuml proposed_dependencies
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Component.puml

title Proposed Module Dependencies (Layered & Acyclic)

package "Application Layer" {
    component "erlmcp_server\n(400 lines)" as server #90EE90
    component "erlmcp_capabilities\n(250 lines)" as caps_facade #90EE90
}

package "Domain Layer" {
    component "erlmcp_server_tools\n(350 lines)" as tools #87CEEB
    component "erlmcp_server_resources\n(380 lines)" as resources #87CEEB
    component "erlmcp_server_prompts\n(320 lines)" as prompts #87CEEB
    component "erlmcp_server_notifications\n(290 lines)" as notifications #87CEEB
    component "erlmcp_server_state\n(300 lines)" as state #87CEEB

    component "erlmcp_capability_negotiation\n(420 lines)" as negotiation #87CEEB
    component "erlmcp_capability_validation\n(380 lines)" as validation #87CEEB
    component "erlmcp_capability_registry\n(350 lines)" as cap_registry #87CEEB
}

package "Infrastructure Layer" {
    component "erlmcp_registry" as registry #FFE4B5
    component "erlmcp_json_rpc" as json_rpc #FFE4B5
    component "erlmcp_change_notifier" as notifier #FFE4B5
    component "erlmcp_uri_validator" as validator #FFE4B5
}

' Application → Domain
server --> tools
server --> resources
server --> prompts
server --> notifications
server --> state

caps_facade --> negotiation
caps_facade --> validation
caps_facade --> cap_registry

' Domain → Domain
tools --> state
resources --> state
prompts --> state
notifications --> state

negotiation --> validation
negotiation --> cap_registry

' Domain → Infrastructure
server --> registry
server --> json_rpc
caps_facade --> json_rpc

resources --> validator
tools --> validator

' Infrastructure (no upward dependencies)

note right of server
  **Benefits:**
  - Acyclic dependencies (top-down)
  - Clear layering
  - Easy to test (bottom-up)
  - Low coupling
  - High cohesion
end note

@enduml
```

---

## 4. Refactoring Sequence Diagram

```plantuml
@startuml refactoring_sequence
!include https://raw.githubusercontent.com/plantuml-stdlib/C4-PlantUML/master/C4_Dynamic.puml

title Refactoring Sequence (6 Phases - Zero Downtime)

participant "Current\nerlmcp_server" as old_server
participant "erlmcp_server_tools\n(NEW)" as tools
participant "erlmcp_server_resources\n(NEW)" as resources
participant "erlmcp_server_prompts\n(NEW)" as prompts
participant "erlmcp_server_notifications\n(NEW)" as notifications
participant "erlmcp_server_state\n(NEW)" as state
participant "New\nerlmcp_server" as new_server

== Phase 1: Extract State Management ==
old_server -> state : 1. Extract state record + transitions
note right: Create erlmcp_server_state.erl\nMove #state{} record\nMove state helper functions

== Phase 2: Extract Domain Modules ==
old_server -> tools : 2.1 Extract tool management
note right: Move add_tool*/delete_tool\nMove tool validation\nMove schema handling

old_server -> resources : 2.2 Extract resource management
note right: Move add_resource*/delete_resource\nMove template handling\nMove subscription logic

old_server -> prompts : 2.3 Extract prompt management
note right: Move add_prompt*/delete_prompt\nMove argument handling

old_server -> notifications : 2.4 Extract notifications
note right: Move notify_*/report_progress\nMove handler registration

== Phase 3: Create Facade ==
tools -> state : 3.1 Domain modules use state
resources -> state : 3.2 Domain modules use state
prompts -> state : 3.3 Domain modules use state
notifications -> state : 3.4 Domain modules use state

== Phase 4: Refactor gen_server Callbacks ==
old_server -> new_server : 4. Delegate to domain modules
new_server -> tools : handle_call({add_tool, ...})
new_server -> resources : handle_call({add_resource, ...})
new_server -> prompts : handle_call({add_prompt, ...})
new_server -> notifications : handle_cast({notify, ...})

== Phase 5: Test & Verify ==
note over new_server
  Run full test suite
  Verify API compatibility
  Performance benchmarks
  No breaking changes
end note

== Phase 6: Deprecate Old Implementation ==
note over old_server
  Mark old implementation as deprecated
  Update documentation
  Migration guide for internal callers
end note

@enduml
```

---

## 5. Implementation Plan

### 5.1 erlmcp_server Refactoring (2,040 → 5×400 lines)

#### Phase 1: Extract State Management (Week 1)

**Create:** `apps/erlmcp_core/src/erlmcp_server_state.erl` (~300 lines)

```erlang
-module(erlmcp_server_state).
-export([
    new/2,
    get_resources/1, set_resources/2,
    get_tools/1, set_tools/2,
    get_prompts/1, set_prompts/2,
    get_phase/1, set_phase/2,
    transition_to_ready/1,
    is_initialized/1
]).

-include("erlmcp.hrl").

%% State record (moved from erlmcp_server)
-record(state, {
    server_id :: server_id(),
    phase = ?MCP_PHASE_INITIALIZATION :: mcp_server_phase(),
    init_timeout_ref :: reference() | undefined,
    init_timeout_ms = ?MCP_DEFAULT_INIT_TIMEOUT_MS :: pos_integer(),
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    resources = #{} :: #{binary() => {#mcp_resource{}, resource_handler()}},
    resource_templates = #{} :: #{binary() => {#mcp_resource_template{}, resource_handler()}},
    tools = #{} :: #{binary() => {#mcp_tool{}, tool_handler(), map() | undefined}},
    prompts = #{} :: #{binary() => {#mcp_prompt{}, prompt_handler()}},
    subscriptions = #{} :: #{binary() => sets:set(pid())},
    progress_tokens = #{} :: #{binary() | integer() => #mcp_progress_notification{}},
    notifier_pid :: pid() | undefined,
    initialized = false :: boolean(),
    last_tools_notification :: integer() | undefined,
    roots = #{} :: map(),
    notification_handlers = #{} :: #{binary() => {pid(), reference()}}
}).

-type state() :: #state{}.
-export_type([state/0]).

%% State constructors and accessors
new(ServerId, Capabilities) ->
    #state{
        server_id = ServerId,
        capabilities = Capabilities
    }.

get_resources(#state{resources = Resources}) -> Resources.
set_resources(State, Resources) -> State#state{resources = Resources}.

get_tools(#state{tools = Tools}) -> Tools.
set_tools(State, Tools) -> State#state{tools = Tools}.

get_prompts(#state{prompts = Prompts}) -> Prompts.
set_prompts(State, Prompts) -> State#state{prompts = Prompts}.

get_phase(#state{phase = Phase}) -> Phase.
set_phase(State, Phase) -> State#state{phase = Phase}.

transition_to_ready(State) ->
    State#state{phase = ?MCP_PHASE_READY, initialized = true}.

is_initialized(#state{initialized = Init}) -> Init.
```

**Tests:** `test/erlmcp_server_state_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_server_state module (Phase 1/6)"`

---

#### Phase 2: Extract Tool Management (Week 2)

**Create:** `apps/erlmcp_core/src/erlmcp_server_tools.erl` (~350 lines)

```erlang
-module(erlmcp_server_tools).
-export([
    add_tool/3,
    add_tool_with_schema/4,
    add_tool_with_description/4,
    add_tool_full/5,
    delete_tool/2,
    list_tools/1,
    get_tool/2,
    execute_tool/3,
    validate_tool_name/1,
    validate_tool_schema/1
]).

-include("erlmcp.hrl").

%% Add tool with handler (basic)
-spec add_tool(erlmcp_server_state:state(), binary(), tool_handler()) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_tool(State, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    case validate_tool_name(Name) of
        ok ->
            Tool = #mcp_tool{
                name = Name,
                description = <<"Tool: ", Name/binary>>
            },
            Tools = erlmcp_server_state:get_tools(State),
            NewTools = maps:put(Name, {Tool, Handler, undefined}, Tools),
            NewState = erlmcp_server_state:set_tools(State, NewTools),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Add tool with JSON Schema validation
-spec add_tool_with_schema(erlmcp_server_state:state(), binary(), tool_handler(), map()) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_tool_with_schema(State, Name, Handler, Schema) ->
    case validate_tool_schema(Schema) of
        ok ->
            Tool = #mcp_tool{
                name = Name,
                description = <<"Tool: ", Name/binary>>,
                input_schema = Schema
            },
            Tools = erlmcp_server_state:get_tools(State),
            NewTools = maps:put(Name, {Tool, Handler, Schema}, Tools),
            NewState = erlmcp_server_state:set_tools(State, NewTools),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Delete tool
-spec delete_tool(erlmcp_server_state:state(), binary()) ->
    {ok, erlmcp_server_state:state()} | {error, not_found}.
delete_tool(State, Name) ->
    Tools = erlmcp_server_state:get_tools(State),
    case maps:is_key(Name, Tools) of
        true ->
            NewTools = maps:remove(Name, Tools),
            NewState = erlmcp_server_state:set_tools(State, NewTools),
            {ok, NewState};
        false ->
            {error, not_found}
    end.

%% List all tools
-spec list_tools(erlmcp_server_state:state()) -> [#mcp_tool{}].
list_tools(State) ->
    Tools = erlmcp_server_state:get_tools(State),
    [Tool || {Tool, _Handler, _Schema} <- maps:values(Tools)].

%% Validation helpers
validate_tool_name(Name) when byte_size(Name) > 0, byte_size(Name) =< 1000 -> ok;
validate_tool_name(_) -> {error, invalid_tool_name}.

validate_tool_schema(Schema) when is_map(Schema) ->
    %% TODO: Add JSON Schema validation via jesse
    ok;
validate_tool_schema(_) -> {error, invalid_schema}.
```

**Tests:** `test/erlmcp_server_tools_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_server_tools module (Phase 2/6)"`

---

#### Phase 3: Extract Resource Management (Week 2)

**Create:** `apps/erlmcp_core/src/erlmcp_server_resources.erl` (~380 lines)

```erlang
-module(erlmcp_server_resources).
-export([
    add_resource/3,
    add_resource_template/4,
    delete_resource/2,
    list_resources/1,
    get_resource/2,
    subscribe_resource/3,
    unsubscribe_resource/2,
    list_subscriptions/2,
    validate_resource_uri/1
]).

-include("erlmcp.hrl").

%% Add resource with handler
-spec add_resource(erlmcp_server_state:state(), binary(), resource_handler()) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_resource(State, Uri, Handler) when is_binary(Uri), is_function(Handler, 1) ->
    case erlmcp_uri_validator:validate_resource_uri_on_registration(Uri) of
        ok ->
            Resource = #mcp_resource{
                uri = Uri,
                name = Uri,
                mime_type = ?MCP_MIME_TEXT_PLAIN
            },
            Resources = erlmcp_server_state:get_resources(State),
            NewResources = maps:put(Uri, {Resource, Handler}, Resources),
            NewState = erlmcp_server_state:set_resources(State, NewResources),
            {ok, NewState};
        {error, {ErrorType, ErrorMsg}} ->
            {error, {?JSONRPC_INVALID_PARAMS, ErrorMsg, #{
                <<"error_type">> => atom_to_binary(ErrorType, utf8),
                <<"uri">> => Uri
            }}}
    end.

%% Add resource template with URI template
-spec add_resource_template(erlmcp_server_state:state(), binary(), binary(), resource_handler()) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_resource_template(State, UriTemplate, Name, Handler) ->
    case erlmcp_uri_validator:validate_uri_template(UriTemplate) of
        ok ->
            Template = #mcp_resource_template{
                uri_template = UriTemplate,
                name = Name,
                mime_type = ?MCP_MIME_TEXT_PLAIN
            },
            Templates = erlmcp_server_state:get_resource_templates(State),
            NewTemplates = maps:put(UriTemplate, {Template, Handler}, Templates),
            NewState = erlmcp_server_state:set_resource_templates(State, NewTemplates),
            {ok, NewState};
        {error, {ErrorType, ErrorMsg}} ->
            {error, {?JSONRPC_INVALID_PARAMS, ErrorMsg, #{
                <<"error_type">> => atom_to_binary(ErrorType, utf8),
                <<"uri_template">> => UriTemplate
            }}}
    end.

%% Subscribe to resource changes
-spec subscribe_resource(erlmcp_server_state:state(), binary(), pid()) ->
    {ok, erlmcp_server_state:state()}.
subscribe_resource(State, Uri, SubscriberPid) ->
    Subscriptions = erlmcp_server_state:get_subscriptions(State),
    Subscribers = maps:get(Uri, Subscriptions, sets:new([{version, 2}])),
    NewSubscribers = sets:add_element(SubscriberPid, Subscribers),
    NewSubscriptions = maps:put(Uri, NewSubscribers, Subscriptions),
    NewState = erlmcp_server_state:set_subscriptions(State, NewSubscriptions),
    {ok, NewState}.
```

**Tests:** `test/erlmcp_server_resources_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_server_resources module (Phase 3/6)"`

---

#### Phase 4: Extract Prompt Management (Week 3)

**Create:** `apps/erlmcp_core/src/erlmcp_server_prompts.erl` (~320 lines)

```erlang
-module(erlmcp_server_prompts).
-export([
    add_prompt/3,
    add_prompt_with_args/4,
    add_prompt_with_args_and_schema/5,
    delete_prompt/2,
    list_prompts/1,
    get_prompt/2,
    execute_prompt/3,
    validate_prompt_name/1,
    validate_prompt_arguments/1
]).

-include("erlmcp.hrl").

%% Add prompt with handler
-spec add_prompt(erlmcp_server_state:state(), binary(), prompt_handler()) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_prompt(State, Name, Handler) when is_binary(Name), is_function(Handler, 1) ->
    case validate_prompt_name(Name) of
        ok ->
            Prompt = #mcp_prompt{
                name = Name,
                description = <<"Prompt: ", Name/binary>>
            },
            Prompts = erlmcp_server_state:get_prompts(State),
            NewPrompts = maps:put(Name, {Prompt, Handler}, Prompts),
            NewState = erlmcp_server_state:set_prompts(State, NewPrompts),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Add prompt with arguments
-spec add_prompt_with_args(erlmcp_server_state:state(), binary(), prompt_handler(), [#mcp_prompt_argument{}]) ->
    {ok, erlmcp_server_state:state()} | {error, term()}.
add_prompt_with_args(State, Name, Handler, Arguments) ->
    case validate_prompt_arguments(Arguments) of
        ok ->
            Prompt = #mcp_prompt{
                name = Name,
                description = <<"Prompt: ", Name/binary>>,
                arguments = Arguments
            },
            Prompts = erlmcp_server_state:get_prompts(State),
            NewPrompts = maps:put(Name, {Prompt, Handler}, Prompts),
            NewState = erlmcp_server_state:set_prompts(State, NewPrompts),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Validation helpers
validate_prompt_name(Name) when byte_size(Name) > 0, byte_size(Name) =< 1000 -> ok;
validate_prompt_name(_) -> {error, invalid_prompt_name}.

validate_prompt_arguments(Args) when is_list(Args) ->
    %% TODO: Validate argument records
    ok;
validate_prompt_arguments(_) -> {error, invalid_arguments}.
```

**Tests:** `test/erlmcp_server_prompts_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_server_prompts module (Phase 4/6)"`

---

#### Phase 5: Extract Notification Handling (Week 3)

**Create:** `apps/erlmcp_core/src/erlmcp_server_notifications.erl` (~290 lines)

```erlang
-module(erlmcp_server_notifications).
-export([
    report_progress/4,
    notify_resource_updated/3,
    notify_resources_changed/1,
    notify_tools_changed/1,
    notify_prompts_changed/1,
    register_notification_handler/3,
    unregister_notification_handler/2,
    unregister_all_handlers/1,
    list_notification_handlers/1
]).

-include("erlmcp.hrl").

%% Report progress for long-running operations
-spec report_progress(erlmcp_server_state:state(), binary() | integer(), integer(), integer()) ->
    ok.
report_progress(State, ProgressToken, Progress, Total) ->
    Notification = #mcp_progress_notification{
        progress_token = ProgressToken,
        progress = Progress,
        total = Total
    },
    NotifierPid = erlmcp_server_state:get_notifier_pid(State),
    case NotifierPid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            erlmcp_change_notifier:notify_progress(Pid, Notification),
            ok
    end.

%% Notify resource updated
-spec notify_resource_updated(erlmcp_server_state:state(), binary(), #mcp_resource{}) ->
    ok.
notify_resource_updated(State, Uri, Resource) ->
    NotifierPid = erlmcp_server_state:get_notifier_pid(State),
    case NotifierPid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            erlmcp_change_notifier:notify_resource_updated(Pid, Uri, Resource),
            ok
    end.

%% Notify resources list changed
-spec notify_resources_changed(erlmcp_server_state:state()) -> ok.
notify_resources_changed(State) ->
    NotifierPid = erlmcp_server_state:get_notifier_pid(State),
    case NotifierPid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            erlmcp_change_notifier:notify_list_changed(Pid, resources),
            ok
    end.

%% Notify tools list changed (with rate limiting)
-spec notify_tools_changed(erlmcp_server_state:state()) ->
    {ok, erlmcp_server_state:state()}.
notify_tools_changed(State) ->
    Now = erlang:system_time(millisecond),
    LastNotification = erlmcp_server_state:get_last_tools_notification(State),
    RateLimit = 1000, % 1 second rate limit

    case LastNotification of
        undefined ->
            send_tools_notification(State, Now);
        Timestamp when (Now - Timestamp) >= RateLimit ->
            send_tools_notification(State, Now);
        _ ->
            %% Rate limited, skip notification
            {ok, State}
    end.

send_tools_notification(State, Timestamp) ->
    NotifierPid = erlmcp_server_state:get_notifier_pid(State),
    case NotifierPid of
        undefined -> ok;
        Pid when is_pid(Pid) ->
            erlmcp_change_notifier:notify_list_changed(Pid, tools)
    end,
    NewState = erlmcp_server_state:set_last_tools_notification(State, Timestamp),
    {ok, NewState}.

%% Register notification handler
-spec register_notification_handler(erlmcp_server_state:state(), binary(), pid()) ->
    {ok, erlmcp_server_state:state()}.
register_notification_handler(State, Method, HandlerPid) ->
    MonitorRef = erlang:monitor(process, HandlerPid),
    Handlers = erlmcp_server_state:get_notification_handlers(State),
    NewHandlers = maps:put(Method, {HandlerPid, MonitorRef}, Handlers),
    NewState = erlmcp_server_state:set_notification_handlers(State, NewHandlers),
    {ok, NewState}.
```

**Tests:** `test/erlmcp_server_notifications_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_server_notifications module (Phase 5/6)"`

---

#### Phase 6: Refactor erlmcp_server gen_server (Week 4)

**Modify:** `apps/erlmcp_core/src/erlmcp_server.erl` (2,040 → 400 lines)

```erlang
-module(erlmcp_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports (UNCHANGED - backward compatibility)
-export([
    start_link/2,
    add_resource/3,
    add_resource_template/4,
    add_tool/3,
    add_tool_with_description/4,
    add_tool_with_schema/4,
    add_tool_full/5,
    add_prompt/3,
    add_prompt_with_args/4,
    add_prompt_with_args_and_schema/5,
    delete_resource/2,
    delete_tool/2,
    delete_prompt/2,
    subscribe_resource/3,
    unsubscribe_resource/2,
    report_progress/4,
    notify_resource_updated/3,
    notify_resources_changed/1,
    register_notification_handler/3,
    unregister_notification_handler/2,
    unregister_all_handlers/1,
    stop/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API Functions (Delegate to Domain Modules)
%%====================================================================

-spec add_resource(server(), binary(), resource_handler()) -> ok.
add_resource(Server, Uri, Handler) ->
    gen_server:call(Server, {add_resource, Uri, Handler}).

-spec add_tool(server(), binary(), tool_handler()) -> ok.
add_tool(Server, Name, Handler) ->
    gen_server:call(Server, {add_tool, Name, Handler}).

-spec add_prompt(server(), binary(), prompt_handler()) -> ok.
add_prompt(Server, Name, Handler) ->
    gen_server:call(Server, {add_prompt, Name, Handler}).

-spec report_progress(server(), binary() | integer(), integer(), integer()) -> ok.
report_progress(Server, ProgressToken, Progress, Total) ->
    gen_server:cast(Server, {report_progress, ProgressToken, Progress, Total}).

%%====================================================================
%% gen_server Callbacks (Delegate to Domain Modules)
%%====================================================================

init([ServerId, Capabilities]) ->
    process_flag(trap_exit, true),

    %% Initialize notifier
    NotifierPid = case erlmcp_change_notifier:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end,

    %% Create initial state
    State = erlmcp_server_state:new(ServerId, Capabilities),
    State2 = erlmcp_server_state:set_notifier_pid(State, NotifierPid),

    logger:info("Starting MCP server ~p", [ServerId]),
    {ok, State2}.

%% Tool management (delegate to erlmcp_server_tools)
handle_call({add_tool, Name, Handler}, _From, State) ->
    case erlmcp_server_tools:add_tool(State, Name, Handler) of
        {ok, NewState} ->
            {ok, NewState2} = erlmcp_server_notifications:notify_tools_changed(NewState),
            {reply, ok, NewState2};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({add_tool_with_schema, Name, Handler, Schema}, _From, State) ->
    case erlmcp_server_tools:add_tool_with_schema(State, Name, Handler, Schema) of
        {ok, NewState} ->
            {ok, NewState2} = erlmcp_server_notifications:notify_tools_changed(NewState),
            {reply, ok, NewState2};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Resource management (delegate to erlmcp_server_resources)
handle_call({add_resource, Uri, Handler}, _From, State) ->
    case erlmcp_server_resources:add_resource(State, Uri, Handler) of
        {ok, NewState} ->
            erlmcp_server_notifications:notify_resources_changed(NewState),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

%% Prompt management (delegate to erlmcp_server_prompts)
handle_call({add_prompt, Name, Handler}, _From, State) ->
    case erlmcp_server_prompts:add_prompt(State, Name, Handler) of
        {ok, NewState} ->
            erlmcp_server_notifications:notify_prompts_changed(NewState),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% Notification handling (delegate to erlmcp_server_notifications)
handle_cast({report_progress, ProgressToken, Progress, Total}, State) ->
    erlmcp_server_notifications:report_progress(State, ProgressToken, Progress, Total),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

**Tests:** Update `test/erlmcp_server_tests.erl` (verify API compatibility)

**Git:** `git commit -m "refactor: Refactor erlmcp_server to delegate to domain modules (Phase 6/6)"`

---

### 5.2 erlmcp_capabilities Refactoring (1,253 → 3×400 lines)

#### Phase 1: Extract Capability Negotiation (Week 5)

**Create:** `apps/erlmcp_core/src/erlmcp_capability_negotiation.erl` (~420 lines)

```erlang
-module(erlmcp_capability_negotiation).
-export([
    negotiate_capabilities/2,
    negotiate_capability_enhanced/3,
    merge_capability/3,
    apply_graceful_degradation/2,
    apply_graceful_degradation_enhanced/2,
    negotiate_subscribe_flag/2,
    client_supports_tools_list_changed/1
]).

-include("erlmcp.hrl").

%% Negotiate capabilities between client and server
-spec negotiate_capabilities(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    {ok, #mcp_server_capabilities{}} | {error, term()}.
negotiate_capabilities(ClientCaps, ServerCaps) ->
    %% Negotiate each capability domain
    NegotiatedResources = negotiate_resources(ClientCaps, ServerCaps),
    NegotiatedTools = negotiate_tools(ClientCaps, ServerCaps),
    NegotiatedPrompts = negotiate_prompts(ClientCaps, ServerCaps),
    NegotiatedLogging = negotiate_logging(ClientCaps, ServerCaps),
    NegotiatedSampling = negotiate_sampling(ClientCaps, ServerCaps),
    NegotiatedRoots = negotiate_roots(ClientCaps, ServerCaps),

    NegotiatedCaps = #mcp_server_capabilities{
        resources = NegotiatedResources,
        tools = NegotiatedTools,
        prompts = NegotiatedPrompts,
        logging = NegotiatedLogging,
        sampling = NegotiatedSampling,
        roots = NegotiatedRoots,
        experimental = ServerCaps#mcp_server_capabilities.experimental
    },

    {ok, NegotiatedCaps}.

%% Negotiate tools capability with listChanged feature flag
negotiate_tools(ClientCaps, ServerCaps) ->
    ClientTools = ClientCaps#mcp_client_capabilities.tools,
    ServerTools = ServerCaps#mcp_server_capabilities.tools,

    %% Server supports tools if it has any
    ServerSupportsTools = ServerTools#mcp_capability.enabled,

    %% Client supports listChanged notification
    ClientSupportsListChanged = ClientTools#mcp_tools_capability.listChanged,

    %% Negotiate: both must support for feature to be enabled
    ListChangedEnabled = ServerSupportsTools andalso ClientSupportsListChanged,

    #mcp_tools_capability{
        listChanged = ListChangedEnabled
    }.

%% Check if client supports tools/list_changed notification
-spec client_supports_tools_list_changed(#mcp_client_capabilities{}) -> boolean().
client_supports_tools_list_changed(#mcp_client_capabilities{tools = Tools}) ->
    Tools#mcp_tools_capability.listChanged.

%% Apply graceful degradation for unsupported capabilities
-spec apply_graceful_degradation(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    {ok, #mcp_server_capabilities{}}.
apply_graceful_degradation(ClientCaps, ServerCaps) ->
    %% Disable server features not supported by client
    DegradedCaps = ServerCaps#mcp_server_capabilities{
        resources = degrade_resources(ClientCaps, ServerCaps),
        tools = degrade_tools(ClientCaps, ServerCaps),
        prompts = degrade_prompts(ClientCaps, ServerCaps)
    },
    {ok, DegradedCaps}.

degrade_tools(ClientCaps, ServerCaps) ->
    ClientTools = ClientCaps#mcp_client_capabilities.tools,
    ServerTools = ServerCaps#mcp_server_capabilities.tools,

    case ClientTools#mcp_tools_capability.listChanged of
        false ->
            %% Disable listChanged if client doesn't support it
            #mcp_tools_capability{listChanged = false};
        true ->
            ServerTools
    end.
```

**Tests:** `test/erlmcp_capability_negotiation_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_capability_negotiation module"`

---

#### Phase 2: Extract Capability Validation (Week 5)

**Create:** `apps/erlmcp_core/src/erlmcp_capability_validation.erl` (~380 lines)

```erlang
-module(erlmcp_capability_validation).
-export([
    validate_protocol_version/1,
    validate_capability_structures/2,
    validate_client_capability_record/1,
    validate_server_capability_record/1,
    validate_model_preferences/1,
    validate_flag/3,
    supports_flag/2,
    validate_capability_dependencies/1,
    is_capability_required/1,
    is_capability_optional/1
]).

-include("erlmcp.hrl").

%% Validate MCP protocol version
-spec validate_protocol_version(binary()) -> ok | {error, unsupported_version}.
validate_protocol_version(<<"2025-11-25">>) -> ok;
validate_protocol_version(<<"2024-11-05">>) -> ok;
validate_protocol_version(_) -> {error, unsupported_version}.

%% Validate capability structures
-spec validate_capability_structures(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    ok | {error, term()}.
validate_capability_structures(ClientCaps, ServerCaps) ->
    case validate_client_capability_record(ClientCaps) of
        ok ->
            validate_server_capability_record(ServerCaps);
        Error ->
            Error
    end.

%% Validate client capability record
-spec validate_client_capability_record(#mcp_client_capabilities{}) -> ok | {error, term()}.
validate_client_capability_record(#mcp_client_capabilities{
    roots = Roots,
    sampling = Sampling,
    tools = Tools
}) ->
    %% Validate roots capability
    case validate_capability(Roots) of
        ok ->
            %% Validate sampling capability
            case validate_capability(Sampling) of
                ok ->
                    %% Validate tools capability
                    validate_tools_capability(Tools);
                Error -> Error
            end;
        Error -> Error
    end;
validate_client_capability_record(_) ->
    {error, invalid_client_capabilities}.

%% Validate server capability record
-spec validate_server_capability_record(#mcp_server_capabilities{}) -> ok | {error, term()}.
validate_server_capability_record(#mcp_server_capabilities{
    resources = Resources,
    tools = Tools,
    prompts = Prompts,
    logging = Logging
}) ->
    Validations = [
        validate_capability(Resources),
        validate_tools_capability(Tools),
        validate_capability(Prompts),
        validate_capability(Logging)
    ],

    case lists:all(fun(R) -> R =:= ok end, Validations) of
        true -> ok;
        false -> {error, invalid_server_capabilities}
    end;
validate_server_capability_record(_) ->
    {error, invalid_server_capabilities}.

%% Validate generic capability record
validate_capability(#mcp_capability{enabled = Enabled}) when is_boolean(Enabled) -> ok;
validate_capability(_) -> {error, invalid_capability}.

%% Validate tools capability (with listChanged flag)
validate_tools_capability(#mcp_tools_capability{listChanged = ListChanged})
  when is_boolean(ListChanged) -> ok;
validate_tools_capability(_) -> {error, invalid_tools_capability}.

%% Validate model preferences (sampling capability)
-spec validate_model_preferences(map()) -> ok | {error, term()}.
validate_model_preferences(Prefs) when is_map(Prefs) ->
    %% TODO: Validate model preference structure
    ok;
validate_model_preferences(_) ->
    {error, invalid_model_preferences}.

%% Check if capability supports a specific flag
-spec supports_flag(#mcp_capability{} | #mcp_tools_capability{}, atom()) -> boolean().
supports_flag(#mcp_tools_capability{listChanged = true}, listChanged) -> true;
supports_flag(#mcp_tools_capability{}, listChanged) -> false;
supports_flag(_, _) -> false.

%% Validate flag value
-spec validate_flag(atom(), any(), #mcp_capability{}) -> ok | {error, term()}.
validate_flag(listChanged, Value, _Cap) when is_boolean(Value) -> ok;
validate_flag(_, _, _) -> {error, invalid_flag}.

%% Check if capability is required
-spec is_capability_required(atom()) -> boolean().
is_capability_required(roots) -> false;
is_capability_required(sampling) -> false;
is_capability_required(tools) -> false;
is_capability_required(_) -> false.

%% Check if capability is optional
-spec is_capability_optional(atom()) -> boolean().
is_capability_optional(Cap) -> not is_capability_required(Cap).

%% Validate capability dependencies
-spec validate_capability_dependencies(#mcp_server_capabilities{}) -> ok | {error, term()}.
validate_capability_dependencies(_Caps) ->
    %% TODO: Implement dependency validation
    ok.
```

**Tests:** `test/erlmcp_capability_validation_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_capability_validation module"`

---

#### Phase 3: Extract Capability Registry (Week 6)

**Create:** `apps/erlmcp_core/src/erlmcp_capability_registry.erl` (~350 lines)

```erlang
-module(erlmcp_capability_registry).
-behaviour(gen_server).

-export([
    start_link/0,
    get_required_capabilities/0,
    get_optional_capabilities/0,
    get_capability_dependencies/1,
    get_negotiated_capabilities/0,
    set_negotiated_capabilities/1,
    reset_negotiated_capabilities/0,
    get_capability_flags/2,
    set_capability_flag/4,
    get_capability_description/1,
    get_feature_description/2
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(state, {
    negotiated_capabilities :: #mcp_server_capabilities{} | undefined,
    capability_metadata :: map()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_required_capabilities() ->
    gen_server:call(?MODULE, get_required_capabilities).

get_optional_capabilities() ->
    gen_server:call(?MODULE, get_optional_capabilities).

get_negotiated_capabilities() ->
    gen_server:call(?MODULE, get_negotiated_capabilities).

set_negotiated_capabilities(Caps) ->
    gen_server:call(?MODULE, {set_negotiated_capabilities, Caps}).

reset_negotiated_capabilities() ->
    gen_server:call(?MODULE, reset_negotiated_capabilities).

get_capability_description(CapName) ->
    gen_server:call(?MODULE, {get_capability_description, CapName}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    Metadata = #{
        resources => #{
            description => <<"Server resource management capability">>,
            required => false,
            features => []
        },
        tools => #{
            description => <<"Server tool execution capability">>,
            required => false,
            features => [
                #{name => listChanged, description => <<"Notify clients of tool list changes">>}
            ]
        },
        prompts => #{
            description => <<"Server prompt template capability">>,
            required => false,
            features => []
        },
        logging => #{
            description => <<"Client logging capability">>,
            required => false,
            features => []
        },
        sampling => #{
            description => <<"Server sampling capability">>,
            required => false,
            features => []
        },
        roots => #{
            description => <<"Client roots capability">>,
            required => false,
            features => []
        }
    },

    State = #state{
        negotiated_capabilities = undefined,
        capability_metadata = Metadata
    },

    {ok, State}.

handle_call(get_required_capabilities, _From, State) ->
    Required = [roots, sampling],  % MCP 2025-11-25 required capabilities
    {reply, Required, State};

handle_call(get_optional_capabilities, _From, State) ->
    Optional = [resources, tools, prompts, logging],
    {reply, Optional, State};

handle_call(get_negotiated_capabilities, _From, State) ->
    {reply, State#state.negotiated_capabilities, State};

handle_call({set_negotiated_capabilities, Caps}, _From, State) ->
    {reply, ok, State#state{negotiated_capabilities = Caps}};

handle_call(reset_negotiated_capabilities, _From, State) ->
    {reply, ok, State#state{negotiated_capabilities = undefined}};

handle_call({get_capability_description, CapName}, _From, State) ->
    Metadata = State#state.capability_metadata,
    Description = case maps:get(CapName, Metadata, undefined) of
        undefined -> undefined;
        CapMeta -> maps:get(description, CapMeta, undefined)
    end,
    {reply, Description, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
```

**Tests:** `test/erlmcp_capability_registry_tests.erl` (100% coverage)

**Git:** `git commit -m "refactor: Extract erlmcp_capability_registry module"`

---

#### Phase 4: Refactor erlmcp_capabilities Facade (Week 6)

**Modify:** `apps/erlmcp_core/src/erlmcp_capabilities.erl` (1,253 → 250 lines)

```erlang
-module(erlmcp_capabilities).

%% Public API (UNCHANGED - backward compatibility)
-export([
    extract_client_capabilities/1,
    extract_server_capabilities/1,
    capability_to_map/1,
    map_to_capability/1,
    validate_protocol_version/1,
    validate_capability_structures/2,
    negotiate_capabilities/2,
    client_supports_tools_list_changed/1,
    %% ... (all other exports unchanged)
]).

-include("erlmcp.hrl").

%%====================================================================
%% API Functions (Delegate to Domain Modules)
%%====================================================================

%% Extraction (local functions - lightweight parsing)
-spec extract_client_capabilities(map()) -> #mcp_client_capabilities{}.
extract_client_capabilities(Params) when is_map(Params) ->
    CapsMap = maps:get(<<"capabilities">>, Params, #{}),
    #mcp_client_capabilities{
        roots = extract_roots_client_capability(CapsMap),
        sampling = extract_sampling_client_capability(CapsMap),
        tools = extract_tools_client_capability(CapsMap),
        experimental = maps:get(<<"experimental">>, CapsMap, undefined)
    }.

%% Validation (delegate to validation module)
-spec validate_protocol_version(binary()) -> ok | {error, term()}.
validate_protocol_version(Version) ->
    erlmcp_capability_validation:validate_protocol_version(Version).

-spec validate_capability_structures(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    ok | {error, term()}.
validate_capability_structures(ClientCaps, ServerCaps) ->
    erlmcp_capability_validation:validate_capability_structures(ClientCaps, ServerCaps).

%% Negotiation (delegate to negotiation module)
-spec negotiate_capabilities(#mcp_client_capabilities{}, #mcp_server_capabilities{}) ->
    {ok, #mcp_server_capabilities{}} | {error, term()}.
negotiate_capabilities(ClientCaps, ServerCaps) ->
    erlmcp_capability_negotiation:negotiate_capabilities(ClientCaps, ServerCaps).

-spec client_supports_tools_list_changed(#mcp_client_capabilities{}) -> boolean().
client_supports_tools_list_changed(ClientCaps) ->
    erlmcp_capability_negotiation:client_supports_tools_list_changed(ClientCaps).

%% Registry (delegate to registry module)
-spec get_required_capabilities() -> [atom()].
get_required_capabilities() ->
    erlmcp_capability_registry:get_required_capabilities().

-spec get_optional_capabilities() -> [atom()].
get_optional_capabilities() ->
    erlmcp_capability_registry:get_optional_capabilities().

%%====================================================================
%% Internal Functions (Lightweight Extraction)
%%====================================================================

extract_roots_client_capability(CapsMap) ->
    case maps:get(<<"roots">>, CapsMap, undefined) of
        undefined -> #mcp_capability{enabled = false};
        CapMap when is_map(CapMap), map_size(CapMap) > 0 ->
            #mcp_capability{enabled = true};
        _ -> #mcp_capability{enabled = false}
    end.

extract_sampling_client_capability(CapsMap) ->
    case maps:get(<<"sampling">>, CapsMap, undefined) of
        undefined -> #mcp_capability{enabled = false};
        CapMap when is_map(CapMap) -> #mcp_capability{enabled = true};
        _ -> #mcp_capability{enabled = false}
    end.

extract_tools_client_capability(CapsMap) ->
    case maps:get(<<"tools">>, CapsMap, undefined) of
        undefined ->
            #mcp_tools_capability{listChanged = false};
        CapMap when is_map(CapMap) ->
            #mcp_tools_capability{
                listChanged = maps:get(<<"listChanged">>, CapMap, false)
            };
        _ ->
            #mcp_tools_capability{listChanged = false}
    end.

%% Map conversion functions remain local (lightweight transformations)
capability_to_map(#mcp_server_capabilities{} = Caps) ->
    %% ... (keep existing implementation)
    ok.

map_to_capability(Map) when is_map(Map) ->
    %% ... (keep existing implementation)
    ok.
```

**Tests:** Update `test/erlmcp_capabilities_tests.erl` (verify API compatibility)

**Git:** `git commit -m "refactor: Refactor erlmcp_capabilities to facade pattern"`

---

### 5.3 erlmcp_core_sup Hierarchical Supervision (Week 7-8)

#### Phase 1: Create Tier 1 Foundation Supervisor

**Create:** `apps/erlmcp_core/src/erlmcp_foundation_sup.erl` (~120 lines)

```erlang
-module(erlmcp_foundation_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% TIER 1: Foundation Infrastructure Supervisor
%% Strategy: one_for_all - registry failure restarts all foundation
%% Critical services: registry, capabilities, reload
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_all,  % Registry failure → restart all
        intensity => 3,
        period => 60
    },

    ChildSpecs = [
        %% Registry (critical - all services depend on this)
        #{
            id => erlmcp_registry,
            start => {erlmcp_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_registry]
        },

        %% Capability registry (critical - negotiation depends on this)
        #{
            id => erlmcp_capability_registry,
            start => {erlmcp_capability_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_capability_registry]
        },

        %% Hot reload supervisor (critical - code upgrade)
        #{
            id => erlmcp_reload_sup,
            start => {erlmcp_reload_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_reload_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Git:** `git commit -m "refactor: Create erlmcp_foundation_sup (TIER 1)"`

---

#### Phase 2: Create Tier 2 Session Supervisor

**Create:** `apps/erlmcp_core/src/erlmcp_session_sup.erl` (~140 lines)

```erlang
-module(erlmcp_session_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% TIER 2: Session Management Supervisor
%% Strategy: one_for_one - session failures are isolated
%% Services: session manager, replication, failover, hooks
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent session components
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_session_manager,
            start => {erlmcp_session_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_manager]
        },

        #{
            id => erlmcp_session_replicator,
            start => {erlmcp_session_replicator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_replicator]
        },

        #{
            id => erlmcp_session_failover,
            start => {erlmcp_session_failover, start_link, [node()]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_session_failover]
        },

        #{
            id => erlmcp_hooks,
            start => {erlmcp_hooks, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_hooks]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Git:** `git commit -m "refactor: Create erlmcp_session_sup (TIER 2)"`

---

#### Phase 3: Create Tier 2 Resource Supervisor

**Create:** `apps/erlmcp_core/src/erlmcp_resource_mgmt_sup.erl` (~160 lines)

```erlang
-module(erlmcp_resource_mgmt_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% TIER 2: Resource Management Supervisor
%% Strategy: one_for_one - resource failures are isolated
%% Services: subscriptions, SSE, cache, icons, notifications
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent resource components
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_resource_subscriptions,
            start => {erlmcp_resource_subscriptions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_resource_subscriptions]
        },

        #{
            id => erlmcp_sse_event_store,
            start => {erlmcp_sse_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_sse_event_store]
        },

        #{
            id => erlmcp_icon_cache,
            start => {erlmcp_icon_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_icon_cache]
        },

        #{
            id => erlmcp_cache,
            start => {erlmcp_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cache]
        },

        #{
            id => erlmcp_notification_handler_sup,
            start => {erlmcp_notification_handler_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_notification_handler_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Git:** `git commit -m "refactor: Create erlmcp_resource_mgmt_sup (TIER 2)"`

---

#### Phase 4: Create Tier 3 Protection Supervisor

**Create:** `apps/erlmcp_core/src/erlmcp_protection_sup.erl` (~160 lines)

```erlang
-module(erlmcp_protection_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% TIER 3: Protection & Rate Limiting Supervisor
%% Strategy: one_for_one - protection failures don't affect core
%% Services: connection limiter, monitor, CPU quota, cancellation, pagination
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Independent protection components
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_connection_limiter,
            start => {erlmcp_connection_limiter, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_limiter]
        },

        #{
            id => erlmcp_connection_monitor,
            start => {erlmcp_connection_monitor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_connection_monitor]
        },

        #{
            id => erlmcp_cpu_quota,
            start => {erlmcp_cpu_quota, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cpu_quota]
        },

        #{
            id => erlmcp_cancellation,
            start => {erlmcp_cancellation, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_cancellation]
        },

        #{
            id => erlmcp_pagination,
            start => {erlmcp_pagination, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_pagination]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
```

**Git:** `git commit -m "refactor: Create erlmcp_protection_sup (TIER 3)"`

---

#### Phase 5: Refactor erlmcp_core_sup

**Modify:** `apps/erlmcp_core/src/erlmcp_core_sup.erl` (244 → 100 lines)

```erlang
-module(erlmcp_core_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% v2.0.0: Hierarchical Core Infrastructure Supervisor
%%
%% 3-Tier Bulkhead Architecture:
%% - TIER 1: Foundation (registry, capabilities) - one_for_all
%% - TIER 2: Domain Services (sessions, resources) - one_for_one
%% - TIER 3: Protection (rate limits, quotas) - one_for_one
%%
%% Strategy: one_for_one - tier failures are isolated
%% ====================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Tier failures are isolated
        intensity => 3,
        period => 60
    },

    ClusterEnabled = application:get_env(erlmcp_core, cluster_enabled, false),

    BaseChildSpecs = [
        %% TIER 1: Foundation (Critical Infrastructure)
        #{
            id => erlmcp_foundation_sup,
            start => {erlmcp_foundation_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_foundation_sup]
        },

        %% TIER 2: Session Management
        #{
            id => erlmcp_session_sup,
            start => {erlmcp_session_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_session_sup]
        },

        %% TIER 2: Resource Management
        #{
            id => erlmcp_resource_mgmt_sup,
            start => {erlmcp_resource_mgmt_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_resource_mgmt_sup]
        },

        %% TIER 3: Protection & Rate Limiting
        #{
            id => erlmcp_protection_sup,
            start => {erlmcp_protection_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_protection_sup]
        }
    ],

    ChildSpecs = case ClusterEnabled of
        true ->
            [
                #{
                    id => erlmcp_cluster_sup,
                    start => {erlmcp_cluster_sup, start_link, []},
                    restart => permanent,
                    shutdown => infinity,
                    type => supervisor,
                    modules => [erlmcp_cluster_sup]
                }
                | BaseChildSpecs
            ];
        false ->
            BaseChildSpecs
    end,

    {ok, {SupFlags, ChildSpecs}}.
```

**Git:** `git commit -m "refactor: Restructure erlmcp_core_sup to hierarchical 3-tier architecture"`

---

## 6. Migration Strategy (Backward Compatibility)

### 6.1 API Compatibility Guarantees

**Public API Stability:**
- All exported functions in `erlmcp_server` remain unchanged
- All exported functions in `erlmcp_capabilities` remain unchanged
- Internal function signatures may change (private functions)
- Test suites run without modification

**Example - erlmcp_server API Compatibility:**

```erlang
%% OLD (v1.x)
erlmcp_server:add_tool(ServerPid, <<"my_tool">>, Handler).

%% NEW (v2.x) - SAME API
erlmcp_server:add_tool(ServerPid, <<"my_tool">>, Handler).

%% Implementation changes from direct gen_server:call to delegation:
%% Before: handle_call implements logic directly (2,040 lines)
%% After: handle_call delegates to erlmcp_server_tools (400 lines)
```

### 6.2 Phased Rollout Strategy

**Week 1-2: Infrastructure (No Breaking Changes)**
- Extract `erlmcp_server_state.erl`
- Extract `erlmcp_server_tools.erl`
- Tests: 100% coverage for new modules
- CI/CD: Green builds required

**Week 3-4: Domain Logic (No Breaking Changes)**
- Extract `erlmcp_server_resources.erl`
- Extract `erlmcp_server_prompts.erl`
- Extract `erlmcp_server_notifications.erl`
- Tests: Full integration tests pass

**Week 5-6: Capabilities Refactoring (No Breaking Changes)**
- Extract `erlmcp_capability_negotiation.erl`
- Extract `erlmcp_capability_validation.erl`
- Extract `erlmcp_capability_registry.erl`
- Refactor `erlmcp_capabilities.erl` to facade pattern
- Tests: API compatibility verified

**Week 7-8: Supervision Tree (No Breaking Changes)**
- Create tier supervisors (foundation, session, resource, protection)
- Refactor `erlmcp_core_sup.erl`
- Tests: Supervision tree tests
- Observer: Visualize new hierarchy

**Week 9: Integration & Performance Testing**
- Full test suite (EUnit, CT, PropEr)
- Benchmarks (no regression > 10%)
- Dialyzer (0 warnings)
- Xref (0 warnings)

**Week 10: Documentation & Release**
- Update architecture.md
- Migration guide
- Release notes
- v2.0.0 release

### 6.3 Deprecation Timeline

**v2.0.0 (Week 10):**
- All refactored modules released
- Public API unchanged
- Internal implementations delegated
- Deprecation warnings: NONE (no deprecated functions)

**v2.1.0 (Month 4):**
- Optional: Mark internal direct calls as deprecated
- Migration guide for internal code

**v3.0.0 (Month 12):**
- Optional: Remove deprecated internal APIs (if any)
- Major version bump (semantic versioning)

### 6.4 Testing Strategy

**Unit Tests (Chicago School - No Mocks):**
```erlang
%% test/erlmcp_server_tools_tests.erl
add_tool_test() ->
    State = erlmcp_server_state:new(test_server, #mcp_server_capabilities{}),
    Handler = fun(_Args) -> {ok, <<"result">>} end,

    {ok, NewState} = erlmcp_server_tools:add_tool(State, <<"test_tool">>, Handler),
    Tools = erlmcp_server_tools:list_tools(NewState),

    ?assertEqual(1, length(Tools)),
    [Tool] = Tools,
    ?assertEqual(<<"test_tool">>, Tool#mcp_tool.name).
```

**Integration Tests (Real Processes):**
```erlang
%% test/erlmcp_server_SUITE.erl
add_tool_integration_test(_Config) ->
    {ok, ServerPid} = erlmcp_server:start_link(test_server, #mcp_server_capabilities{
        tools = #mcp_tools_capability{listChanged = true}
    }),

    Handler = fun(_Args) -> {ok, <<"result">>} end,
    ok = erlmcp_server:add_tool(ServerPid, <<"test_tool">>, Handler),

    %% Verify tool is registered
    %% Verify notification sent (if client subscribed)

    erlmcp_server:stop(ServerPid).
```

**Property-Based Tests (PropEr):**
```erlang
%% test/prop_erlmcp_server_tools.erl
prop_add_delete_tool() ->
    ?FORALL({Name, Handler}, {tool_name(), tool_handler()},
        begin
            State = erlmcp_server_state:new(test, #mcp_server_capabilities{}),
            {ok, State2} = erlmcp_server_tools:add_tool(State, Name, Handler),
            {ok, State3} = erlmcp_server_tools:delete_tool(State2, Name),

            erlmcp_server_tools:list_tools(State3) =:= []
        end).
```

### 6.5 Performance Validation

**Benchmark Baseline (Before Refactoring):**
```bash
$ make benchmark-quick
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
  Registry: 553K msg/s
  Server ops: 242K msg/s (add_tool, add_resource, add_prompt)
```

**Benchmark Target (After Refactoring):**
```bash
$ make benchmark-quick
erlmcp_bench_core_ops:run(<<"core_ops_100k">>)
  Registry: 553K msg/s (no change)
  Server ops: ≥217K msg/s (acceptable if ≥90% baseline, -10% max regression)
```

**Regression Threshold: <10% performance loss**

### 6.6 Rollback Plan

**If Critical Issues Found:**

1. **Identify Issue Severity:**
   - P0: Crashes, data loss, security → Immediate rollback
   - P1: Performance regression >10% → Rollback + investigate
   - P2: Non-critical bugs → Fix forward

2. **Rollback Procedure:**
   ```bash
   # Revert to v1.x release
   git revert <refactoring-commits>
   rebar3 compile
   rebar3 eunit
   rebar3 release
   ```

3. **Post-Rollback:**
   - Root cause analysis
   - Fix in feature branch
   - Re-test with full suite
   - Gradual re-deployment

---

## 7. Success Metrics

### 7.1 Code Quality Metrics

| Metric | Before | Target | Measurement |
|--------|--------|--------|-------------|
| **erlmcp_server LOC** | 2,040 | ≤500 | `wc -l erlmcp_server.erl` |
| **erlmcp_capabilities LOC** | 1,253 | ≤250 | `wc -l erlmcp_capabilities.erl` |
| **Avg module size** | 650 | ≤400 | `find . -name "*.erl" \| xargs wc -l \| awk '{sum+=$1} END {print sum/NR}'` |
| **Max supervisor children** | 16 | ≤5 | Manual review of supervision tree |
| **Supervision tiers** | 1 (flat) | 3 (hierarchical) | Observer visualization |
| **Module coupling** | High (circular) | Low (acyclic) | Xref analysis |

### 7.2 OTP Compliance Metrics

| Metric | Before | Target | Measurement |
|--------|--------|--------|-------------|
| **Single Responsibility** | Violated (god objects) | Compliant | Manual review |
| **Supervision depth** | 1 (flat) | 3 (hierarchical) | Observer |
| **Fault isolation** | Poor (16 workers) | Good (bulkheads) | Failure injection tests |
| **Restart strategies** | Generic (one_for_one) | Optimized (one_for_all TIER1) | Supervisor specs |

### 7.3 Testing Metrics

| Metric | Before | Target | Measurement |
|--------|--------|--------|-------------|
| **Test coverage** | ~80% | ≥85% | `rebar3 cover` |
| **Unit tests** | Mixed | 100% domain modules | EUnit reports |
| **Integration tests** | Partial | Full API coverage | CT reports |
| **Property tests** | None | Core operations | PropEr |

### 7.4 Performance Metrics

| Metric | Before | Target | Measurement |
|--------|--------|--------|-------------|
| **Server ops throughput** | 242K msg/s | ≥217K msg/s (-10% max) | `erlmcp_bench_core_ops` |
| **Latency p95** | <5ms | ≤5.5ms (-10% max) | Benchmark reports |
| **Memory per server** | ~50KB | ≤55KB (-10% max) | `recon:proc_count(memory, 10)` |

---

## 8. Risks & Mitigations

### Risk 1: Performance Regression from Module Delegation

**Risk:** Adding delegation layer increases call depth, potential latency.

**Likelihood:** Medium
**Impact:** Medium
**Mitigation:**
- Inline critical hot paths if regression >10%
- Use macro delegation for zero-cost abstraction
- Benchmark continuously during refactoring
- Accept <10% regression if code quality gain is significant

**Acceptance Criteria:** <10% throughput regression, <10% latency increase

---

### Risk 2: Supervision Tree Migration Instability

**Risk:** Hierarchical supervision introduces new failure modes.

**Likelihood:** Low
**Impact:** High
**Mitigation:**
- Extensive supervision tree testing (failure injection)
- Observer visualization before/after
- Gradual migration (one tier at a time)
- Rollback plan ready

**Acceptance Criteria:** All failure scenarios handled gracefully, <5s recovery

---

### Risk 3: Breaking Internal API Contracts

**Risk:** Internal callers depend on private functions.

**Likelihood:** Low
**Impact:** Medium
**Mitigation:**
- Grep codebase for all function calls before refactoring
- Public API remains unchanged (zero breaking changes)
- Deprecation warnings for internal APIs (if needed)
- Full regression test suite

**Acceptance Criteria:** Zero test failures, zero compilation errors

---

### Risk 4: Timeline Slippage (10 Weeks Aggressive)

**Risk:** Refactoring takes longer than estimated.

**Likelihood:** Medium
**Impact:** Low
**Mitigation:**
- Phased delivery (can stop after any phase)
- Parallel work (capabilities + server refactoring)
- Buffer time in Week 9-10
- Minimum viable refactoring: Just server + capabilities (skip supervision)

**Acceptance Criteria:** At least server refactoring complete by Week 6

---

## 9. Conclusion

This refactoring plan addresses critical architectural debt in erlmcp by:

1. **Decomposing God Objects:**
   - `erlmcp_server`: 2,040 → 5×400 lines (tools, resources, prompts, notifications, state)
   - `erlmcp_capabilities`: 1,253 → 3×400 lines (negotiation, validation, registry)

2. **Establishing Hierarchical Supervision:**
   - Flat 16-worker supervisor → 3-tier bulkhead architecture
   - TIER 1: Foundation (one_for_all) - registry, capabilities
   - TIER 2: Domain Services (one_for_one) - sessions, resources
   - TIER 3: Protection (one_for_one) - rate limits, quotas

3. **Maintaining Backward Compatibility:**
   - Zero breaking changes to public API
   - Gradual migration over 10 weeks
   - Rollback plan for critical issues
   - <10% performance regression threshold

4. **Improving Testability:**
   - Each module ≤500 lines (easy to test)
   - Clear separation of concerns (unit testable)
   - Property-based tests for core operations

**Next Steps:**
1. Review this plan with team
2. Approve timeline and resources
3. Create feature branch `refactor/god-objects-v2`
4. Begin Phase 1: Extract erlmcp_server_state (Week 1)

**Document Version:** v1.0.0
**Author:** Erlang Architect Agent
**Date:** 2026-01-30
**Status:** Proposed (Awaiting Approval)

---

## Appendix A: File Checklist

### New Files Created (16 total)

**erlmcp_server refactoring (5 modules):**
- `apps/erlmcp_core/src/erlmcp_server_state.erl` (~300 lines)
- `apps/erlmcp_core/src/erlmcp_server_tools.erl` (~350 lines)
- `apps/erlmcp_core/src/erlmcp_server_resources.erl` (~380 lines)
- `apps/erlmcp_core/src/erlmcp_server_prompts.erl` (~320 lines)
- `apps/erlmcp_core/src/erlmcp_server_notifications.erl` (~290 lines)

**erlmcp_capabilities refactoring (3 modules):**
- `apps/erlmcp_core/src/erlmcp_capability_negotiation.erl` (~420 lines)
- `apps/erlmcp_core/src/erlmcp_capability_validation.erl` (~380 lines)
- `apps/erlmcp_core/src/erlmcp_capability_registry.erl` (~350 lines)

**Supervision tree refactoring (4 supervisors):**
- `apps/erlmcp_core/src/erlmcp_foundation_sup.erl` (~120 lines)
- `apps/erlmcp_core/src/erlmcp_session_sup.erl` (~140 lines)
- `apps/erlmcp_core/src/erlmcp_resource_mgmt_sup.erl` (~160 lines)
- `apps/erlmcp_core/src/erlmcp_protection_sup.erl` (~160 lines)

**Test files (4 new test suites):**
- `test/erlmcp_server_state_tests.erl`
- `test/erlmcp_server_tools_tests.erl`
- `test/erlmcp_server_resources_tests.erl`
- `test/erlmcp_server_prompts_tests.erl`
- `test/erlmcp_server_notifications_tests.erl`
- `test/erlmcp_capability_negotiation_tests.erl`
- `test/erlmcp_capability_validation_tests.erl`
- `test/erlmcp_capability_registry_tests.erl`

### Modified Files (2 total)

- `apps/erlmcp_core/src/erlmcp_server.erl` (2,040 → 400 lines)
- `apps/erlmcp_core/src/erlmcp_capabilities.erl` (1,253 → 250 lines)
- `apps/erlmcp_core/src/erlmcp_core_sup.erl` (244 → 100 lines)

### Total LOC Impact

**Before:**
- erlmcp_server: 2,040
- erlmcp_capabilities: 1,253
- erlmcp_core_sup: 244
- **Total: 3,537 lines**

**After:**
- 16 new modules: ~4,470 lines (well-structured, focused)
- 3 refactored modules: ~750 lines (facades)
- **Total: 5,220 lines (+1,683 lines)**

**Why LOC increased:**
- Explicit module boundaries (imports, exports)
- Documentation and type specs
- Improved error handling
- Clear separation of concerns

**Quality improvement:**
- Average module size: 650 → 326 lines (-50%)
- Max module size: 2,040 → 420 lines (-79%)
- Testability: Low → High
- Maintainability: Poor → Excellent
