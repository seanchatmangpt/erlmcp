# MCP 2025-11-25 Specification Compliance - Architecture Design

**Document Version:** 1.0.0  
**Date:** 2026-01-30  
**Status:** Design Proposal  
**Author:** Erlang Architect Agent  

---

## Executive Summary

This document provides a comprehensive architectural design for achieving full MCP 2025-11-25 specification compliance in erlmcp. The design focuses on six critical areas: protocol version negotiation, capability discovery and advertisement, state management for MCP lifecycle, request/notification/response handling patterns, error propagation with JSON-RPC error codes, and backwards compatibility strategy.

**Key Design Principles:**
- OTP-first design (gen_server, supervision trees)
- Transport abstraction maintained
- Backwards compatibility with 2024-11-05 protocol version
- Graceful degradation for unsupported features
- Let-it-crash philosophy with proper supervision
- Zero-downtime protocol upgrades

---

## Table of Contents

1. [Protocol Version Negotiation System](#1-protocol-version-negotiation-system)
2. [Capability Discovery and Advertisement](#2-capability-discovery-and-advertisement-architecture)
3. [State Management for MCP Lifecycle](#3-state-management-for-mcp-lifecycle)
4. [Request/Notification/Response Handling](#4-requestnotificationresponse-handling-patterns)
5. [Error Propagation and Mappings](#5-error-propagation-and-json-rpc-error-code-mappings)
6. [Backwards Compatibility Strategy](#6-backwards-compatibility-strategy)
7. [Supervision Tree Architecture](#7-supervision-tree-architecture)
8. [Implementation Roadmap](#8-implementation-roadmap)
9. [Testing Strategy](#9-testing-strategy)
10. [Performance Considerations](#10-performance-considerations)
11. [Security Considerations](#11-security-considerations)
12. [Observability](#12-observability-and-monitoring)

---

## 1. Protocol Version Negotiation System

### 1.1 Current State Analysis

**Strengths:**
- Protocol version defined as macro: `?MCP_VERSION` = `<<"2025-11-25">>`
- `erlmcp_capabilities:validate_protocol_version/1` exists
- Supports both `2024-11-05` and `2025-11-25`

**Gaps:**
- No version negotiation logic during initialization
- No client/server version mismatch handling
- No version-specific feature enablement
- Hardcoded version in initialization requests

### 1.2 Proposed Module: erlmcp_protocol_version

```erlang
%%% Purpose: Centralized protocol version management and negotiation
%%% Location: apps/erlmcp_core/src/erlmcp_protocol_version.erl
%%% Behavior: gen_server (stateful version registry)

-record(protocol_version_state, {
    supported_versions = [<<"2025-11-25">>, <<"2024-11-05">>] :: [binary()],
    preferred_version = <<"2025-11-25">> :: binary(),
    negotiated_version :: binary() | undefined,
    fallback_enabled = true :: boolean(),
    version_features = #{} :: #{binary() => [atom()]}
}).

%%% API Functions:
-spec get_supported_versions() -> [binary()].
-spec negotiate_version(ClientVersion :: binary(), ServerVersions :: [binary()]) ->
    {ok, binary()} | {error, {unsupported_version, binary()}}.
-spec get_version_features(binary()) -> [atom()].
-spec version_supports_feature(binary(), atom()) -> boolean().
```

**Version Feature Matrix:**
```erlang
%% Features by protocol version
-define(VERSION_FEATURES, #{
    <<"2025-11-25">> => [
        ping,                    % ping/pong keepalive
        completions,             % Completion API
        elicitation,             % URL elicitation
        tasks,                   % Task management
        message_notifications,   % notifications/message
        pagination_cursors,      % Cursor-based pagination
        request_cancellation     % requests/cancel
    ],
    <<"2024-11-05">> => [
        resources,
        tools,
        prompts,
        logging,
        sampling,
        roots
    ]
}).
```

### 1.3 Initialization Flow with Version Negotiation

```
CLIENT                                    SERVER
  |                                         |
  |-------- initialize (version) --------->|
  |  {                                      |
  |    "protocolVersion": "2025-11-25"     | 1. Validate client version
  |  }                                      | 2. Check supported versions
  |                                         | 3. Select best match
  |<------ initialize result --------------|
  |  {                                      |
  |    "protocolVersion": "2025-11-25",    | 4. Return negotiated version
  |    "capabilities": {...}                | 5. Enable version-specific features
  |  }                                      |
  |                                         |
  |-------- initialized notification ----->| 6. Client confirms initialization
  |                                         |
  |<======= MCP 2025-11-25 Mode ==========>|
```

**Supervision Integration:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_protocol_version (NEW - permanent worker)
│   Child Spec: #{
│     id => erlmcp_protocol_version,
│     start => {erlmcp_protocol_version, start_link, []},
│     restart => permanent,
│     shutdown => 5000,
│     type => worker
│   }
└── ...
```

**Rationale:**
- Explicit version negotiation prevents silent feature mismatches
- Server selects best compatible version from client request
- Client confirms negotiated version via `initialized` notification
- Fallback to older version if newer version not supported

---

## 2. Capability Discovery and Advertisement Architecture

### 2.1 Current Implementation Review

**Strengths:**
- `erlmcp_capabilities` module exists with comprehensive API
- Capability records defined for all MCP capabilities
- Feature flags for `listChanged` and `subscribe`
- Graceful degradation logic implemented

**Gaps:**
- No dynamic capability querying during session
- No capability update notifications
- No capability versioning
- Limited experimental capability support

### 2.2 Enhanced Capability Manager

**Enhanced State Record:**
```erlang
-record(capability_manager_state, {
    server_capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    protocol_version :: binary() | undefined,
    negotiated_capabilities :: #mcp_server_capabilities{} | undefined,
    capability_registry = #{} :: #{atom() => capability_info()},
    experimental_features = #{} :: #{binary() => map()},
    capability_change_subscribers = [] :: [pid()]  % NEW
}).

-record(capability_info, {
    name :: atom(),
    version :: binary(),
    required :: boolean(),
    dependencies :: [atom()],
    description :: binary(),
    feature_flags :: [atom()]
}).
```

**New API Functions:**
```erlang
%% Dynamic Capability Discovery
-spec discover_capabilities(pid()) -> {ok, #mcp_server_capabilities{}} | {error, term()}.
-spec query_capability(atom()) -> {ok, capability_info()} | {error, not_found}.

%% Capability Change Notifications (NEW)
-spec subscribe_to_capability_changes(pid()) -> ok.
-spec notify_capability_change(atom(), enabled | disabled) -> ok.

%% Experimental Features (ENHANCED)
-spec register_experimental_feature(binary(), map()) -> ok | {error, term()}.
-spec list_experimental_features() -> [{binary(), map()}].
```

### 2.3 Capability Lifecycle

```
INITIALIZATION PHASE:
1. Client sends capabilities in initialize request
2. Server validates and negotiates capabilities
3. Server sends negotiated capabilities in initialize response
4. Client sends "initialized" notification (confirms)

OPERATIONAL PHASE:
5. Capabilities are immutable during session
6. Dynamic feature queries allowed via capability_info()
7. Experimental features can be discovered

SHUTDOWN PHASE:
8. Capabilities persist until connection closed
```

**Rationale:**
- Capabilities negotiated once during initialization (immutable)
- Dynamic discovery allows inspection without re-negotiation
- Experimental features enable safe feature testing
- Capability change subscribers for monitoring (not re-negotiation)

---

## 3. State Management for MCP Lifecycle

### 3.1 Client State Machine

**Phase Definitions:**
```erlang
-type client_phase() ::
    pre_initialization   % Initial state before initialize
  | initializing         % Sent initialize, awaiting response
  | initialized          % Received response, sent initialized notification
  | error                % Initialization failed
  | closed.              % Connection closed

-define(VALID_PHASE_TRANSITIONS, #{
    pre_initialization => [initializing, closed],
    initializing => [initialized, error, closed],
    initialized => [closed],
    error => [closed],
    closed => []
}).
```

**Enhanced Client State Record:**
```erlang
-record(client_state, {
    %% Existing fields
    transport :: module(),
    transport_state :: term(),
    phase = pre_initialization :: client_phase(),
    capabilities :: #mcp_server_capabilities{} | undefined,
    request_id = 1 :: pos_integer(),
    pending_requests = #{} :: #{pos_integer() => {atom(), pid(), erlang:timestamp()}},

    %% NEW: Phase management
    phase_history = [] :: [{client_phase(), erlang:timestamp()}],
    phase_transition_callbacks = #{} :: #{client_phase() => [fun()]},
    initialization_timeout = 30000 :: timeout(),
    initialization_timer :: reference() | undefined,

    %% NEW: Lifecycle tracking
    session_id :: binary() | undefined,
    session_start_time :: erlang:timestamp() | undefined,
    last_activity :: erlang:timestamp() | undefined,
    shutdown_requested = false :: boolean()
}).
```

**Phase Transition Validation:**
```erlang
-spec validate_phase_transition(client_phase(), client_phase()) -> 
    ok | {error, invalid_transition}.
validate_phase_transition(From, To) ->
    case maps:get(From, ?VALID_PHASE_TRANSITIONS, []) of
        AllowedTransitions when is_list(AllowedTransitions) ->
            case lists:member(To, AllowedTransitions) of
                true -> ok;
                false -> {error, {invalid_transition, From, To}}
            end;
        _ ->
            {error, {unknown_phase, From}}
    end.

-spec transition_phase(client_state(), client_phase()) ->
    {ok, client_state()} | {error, term()}.
transition_phase(State = #client_state{phase = CurrentPhase}, NewPhase) ->
    case validate_phase_transition(CurrentPhase, NewPhase) of
        ok ->
            NewState = State#client_state{
                phase = NewPhase,
                phase_history = [{NewPhase, erlang:timestamp()} | State#client_state.phase_history],
                last_activity = erlang:timestamp()
            },
            execute_phase_callbacks(NewState, CurrentPhase, NewPhase),
            {ok, NewState};
        {error, Reason} ->
            logger:error("Invalid phase transition from ~p to ~p: ~p",
                        [CurrentPhase, NewPhase, Reason]),
            {error, Reason}
    end.
```

### 3.2 Server State Machine

**Phase Definitions:**
```erlang
-type server_phase() ::
    initialization       % Awaiting initialize request
  | initialized          % Received initialize, sent response, awaiting initialized notification
  | ready                % Received initialized notification, fully operational
  | shutting_down        % Shutdown initiated
  | closed.              % Connection closed

-define(SERVER_PHASE_TRANSITIONS, #{
    initialization => [initialized, closed],
    initialized => [ready, closed],
    ready => [shutting_down, closed],
    shutting_down => [closed],
    closed => []
}).
```

**Enhanced Server State Record:**
```erlang
-record(server_state, {
    %% Existing fields
    server_id :: atom(),
    phase = initialization :: server_phase(),
    capabilities :: #mcp_server_capabilities{},
    client_capabilities :: #mcp_client_capabilities{} | undefined,
    resources = #{} :: #{binary() => {#mcp_resource{}, resource_handler()}},
    tools = #{} :: #{binary() => {#mcp_tool{}, tool_handler(), map() | undefined}},

    %% NEW: Phase management
    phase_history = [] :: [{server_phase(), erlang:timestamp()}],
    initialization_timer :: reference() | undefined,
    initialization_timeout = 30000 :: timeout(),
    ready_time :: erlang:timestamp() | undefined,

    %% NEW: Lifecycle tracking
    session_id :: binary() | undefined,
    protocol_version :: binary() | undefined,
    initialized_notification_received = false :: boolean(),
    shutdown_reason :: term() | undefined
}).
```

**Initialization Timeout Handling:**
```erlang
%% In init/1
init([ServerId, Capabilities, Options]) ->
    Timeout = maps:get(initialization_timeout, Options, 30000),
    TimerRef = erlang:send_after(Timeout, self(), initialization_timeout),
    State = #server_state{
        server_id = ServerId,
        phase = initialization,
        capabilities = Capabilities,
        initialization_timer = TimerRef,
        initialization_timeout = Timeout
    },
    {ok, State}.

%% Handle timeout
handle_info(initialization_timeout, State = #server_state{phase = Phase})
  when Phase =:= initialization; Phase =:= initialized ->
    logger:error("Server initialization timeout after ~pms. Phase: ~p",
                [State#server_state.initialization_timeout, Phase]),
    {stop, {shutdown, initialization_timeout}, State#server_state{phase = closed}};

handle_info(initialization_timeout, State) ->
    {noreply, State}.
```

### 3.3 State Transition Diagrams

```
CLIENT STATE MACHINE:

┌─────────────────────┐
│ pre_initialization  │ ──────┐
└─────────────────────┘       │
          │                    │
          │ initialize()       │
          ▼                    │
┌─────────────────────┐       │
│   initializing      │       │ error/close
└─────────────────────┘       │
          │                    │
          │ init response      │
          ▼                    │
┌─────────────────────┐       │
│   initialized       │◄──────┤
└─────────────────────┘       │
          │                    │
          │ stop()             │
          ▼                    ▼
┌─────────────────────┐   ┌───────┐
│      closed         │◄──│ error │
└─────────────────────┘   └───────┘


SERVER STATE MACHINE:

┌─────────────────────┐
│   initialization    │
└─────────────────────┘
          │
          │ initialize request
          ▼
┌─────────────────────┐
│   initialized       │
└─────────────────────┘
          │
          │ initialized notification
          ▼
┌─────────────────────┐
│       ready         │
└─────────────────────┘
          │
          │ shutdown
          ▼
┌─────────────────────┐
│  shutting_down      │
└─────────────────────┘
          │
          ▼
┌─────────────────────┐
│      closed         │
└─────────────────────┘
```

**Rationale:**
- Explicit phase tracking prevents invalid operations
- Phase transitions validated before execution
- Timeout enforcement prevents hung initialization
- History tracking for debugging and auditing

---

## 4. Request/Notification/Response Handling Patterns

### 4.1 Enhanced Message Context

```erlang
%% NEW: Message context for routing and tracking
-record(json_rpc_context, {
    direction :: client_to_server | server_to_client,
    transport_id :: atom(),
    session_id :: binary() | undefined,
    received_at :: erlang:timestamp(),
    trace_id :: binary() | undefined,
    parent_span_id :: binary() | undefined
}).

%% Enhanced request envelope
-record(json_rpc_request_envelope, {
    request :: #json_rpc_request{},
    context :: #json_rpc_context{},
    timeout :: timeout(),
    retry_count = 0 :: non_neg_integer()
}).
```

### 4.2 Request Manager (NEW)

```erlang
%%% Module: erlmcp_request_manager
%%% Purpose: Centralized request lifecycle management
%%% Location: apps/erlmcp_core/src/erlmcp_request_manager.erl
%%% Behavior: gen_server

-record(request_manager_state, {
    pending_requests = #{} :: #{request_id() => request_info()},
    request_timeouts = #{} :: #{request_id() => reference()},
    default_timeout = 5000 :: timeout(),
    max_pending = 1000 :: pos_integer(),
    request_counter = 0 :: non_neg_integer(),
    request_metrics = #{} :: #{binary() => request_metrics()}
}).

-record(request_info, {
    request_id :: request_id(),
    method :: binary(),
    requester_pid :: pid(),
    started_at :: erlang:timestamp(),
    timeout_ref :: reference(),
    retry_count = 0 :: non_neg_integer(),
    context :: #json_rpc_context{} | undefined
}).

%%% API:
-spec register_request(request_id(), binary(), pid(), timeout()) -> ok | {error, term()}.
-spec complete_request(request_id(), {ok, term()} | {error, term()}) -> ok.
-spec cancel_request(request_id()) -> ok | {error, not_found()}.
-spec get_pending_requests() -> [request_info()].
```

### 4.3 Notification Dispatcher (NEW)

```erlang
%%% Module: erlmcp_notification_dispatcher
%%% Purpose: Route notifications to registered handlers
%%% Location: apps/erlmcp_core/src/erlmcp_notification_dispatcher.erl
%%% Behavior: gen_server

-record(notification_dispatcher_state, {
    handlers = #{} :: #{binary() => [notification_handler()]},
    wildcard_handlers = [] :: [notification_handler()],
    notification_queue = queue:new() :: queue:queue(notification()),
    max_queue_size = 1000 :: pos_integer(),
    dropped_count = 0 :: non_neg_integer()
}).

-type notification_handler() ::
    {module(), atom()}           % MFA callback
  | fun((binary(), map()) -> ok) % Function
  | pid().                        % Process mailbox

%%% API:
-spec register_handler(binary(), notification_handler()) -> ok.
-spec dispatch_notification(binary(), map(), #json_rpc_context{}) -> ok.
```

### 4.4 Response Pattern Enforcement

```erlang
%% Client-side response handling
handle_info({transport_message, Data}, State) ->
    case erlmcp_json_rpc:decode_message(Data) of
        {ok, #json_rpc_response{id = Id, result = Result, error = undefined}} ->
            erlmcp_request_manager:complete_request(Id, {ok, Result}),
            handle_success_response(Id, Result, State);

        {ok, #json_rpc_response{id = Id, error = Error}} ->
            erlmcp_request_manager:complete_request(Id, {error, Error}),
            handle_error_response(Id, Error, State);

        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            Context = build_context(State),
            erlmcp_notification_dispatcher:dispatch_notification(Method, Params, Context),
            {noreply, State};

        {error, Reason} ->
            logger:error("Failed to decode JSON-RPC message: ~p", [Reason]),
            {noreply, State}
    end.
```

**Supervision Integration:**
```
erlmcp_core_sup (one_for_one)
├── erlmcp_request_manager (NEW)
│   Child Spec: #{
│     id => erlmcp_request_manager,
│     start => {erlmcp_request_manager, start_link, []},
│     restart => permanent,
│     shutdown => 10000,
│     type => worker
│   }
├── erlmcp_notification_dispatcher (NEW)
│   Child Spec: #{
│     id => erlmcp_notification_dispatcher,
│     start => {erlmcp_notification_dispatcher, start_link, []},
│     restart => permanent,
│     shutdown => 5000,
│     type => worker
│   }
└── ...
```

**Rationale:**
- Centralized request tracking prevents orphaned requests
- Timeout management ensures timely failure detection
- Notification dispatcher enables pub/sub pattern
- Context tracking supports distributed tracing

---

## 5. Error Propagation and JSON-RPC Error Code Mappings

### 5.1 Current Coverage Review

**Strengths:**
- Comprehensive error codes defined in `erlmcp.hrl`
- JSON-RPC 2.0 standard errors mapped
- MCP-specific errors in range `-32001` to `-32099`

**Gaps:**
- No error translation layer (Erlang errors → JSON-RPC errors)
- No error context preservation
- Limited error data payloads

### 5.2 Error Handler Module (NEW)

```erlang
%%% Module: erlmcp_error_handler
%%% Purpose: Translate Erlang errors to JSON-RPC error responses
%%% Location: apps/erlmcp_core/src/erlmcp_error_handler.erl
%%% Behavior: None (utility module)

-record(mcp_error_context, {
    request_id :: json_rpc_id() | undefined,
    method :: binary() | undefined,
    phase :: client_phase() | server_phase() | undefined,
    stacktrace :: list() | undefined,
    additional_data :: map() | undefined
}).

%%% API:
-spec translate_error(term()) -> {integer(), binary(), map() | undefined}.
-spec translate_error_with_context(term(), #mcp_error_context{}) ->
    #json_rpc_response{}.
-spec build_error_response(json_rpc_id(), integer(), binary(), map()) ->
    #json_rpc_response{}.
-spec is_recoverable_error(integer()) -> boolean().
-spec suggest_recovery_action(integer()) -> retry | reconnect | abort | ignore.
```

### 5.3 Error Translation Rules

```erlang
%% Erlang Error → JSON-RPC Error Mapping
translate_error(timeout) ->
    {?MCP_ERROR_TIMEOUT, ?MCP_MSG_TIMEOUT, undefined};

translate_error({resource_not_found, Uri}) ->
    {?MCP_ERROR_RESOURCE_NOT_FOUND, ?MCP_MSG_RESOURCE_NOT_FOUND,
     #{<<"uri">> => Uri}};

translate_error({tool_not_found, Name}) ->
    {?MCP_ERROR_TOOL_NOT_FOUND, ?MCP_MSG_TOOL_NOT_FOUND,
     #{<<"tool">> => Name}};

translate_error({capability_not_supported, Cap}) ->
    {?MCP_ERROR_CAPABILITY_NOT_SUPPORTED, ?MCP_MSG_CAPABILITY_NOT_SUPPORTED,
     #{<<"capability">> => atom_to_binary(Cap)}};

translate_error({not_initialized, Phase, Message}) ->
    {?MCP_ERROR_NOT_INITIALIZED, Message,
     #{<<"currentPhase">> => atom_to_binary(Phase)}};

translate_error({unsupported_protocol_version, Version}) ->
    {?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION,
     ?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION,
     #{<<"requestedVersion">> => Version,
       <<"supportedVersions">> => erlmcp_protocol_version:get_supported_versions()}};

translate_error(Other) ->
    logger:warning("Unmapped error: ~p", [Other]),
    {?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR,
     #{<<"erlangError">> => format_term(Other)}}.
```

### 5.4 Error Recovery Strategies

```erlang
%% Determine if error is recoverable
-spec is_recoverable_error(integer()) -> boolean().
is_recoverable_error(?MCP_ERROR_TIMEOUT) -> true;
is_recoverable_error(?MCP_ERROR_RATE_LIMITED) -> true;
is_recoverable_error(?MCP_ERROR_TRANSPORT_ERROR) -> true;
is_recoverable_error(?MCP_ERROR_RESOURCE_NOT_FOUND) -> false;
is_recoverable_error(_) -> false.

%% Suggest recovery action
-spec suggest_recovery_action(integer()) -> retry | reconnect | abort | ignore.
suggest_recovery_action(?MCP_ERROR_TIMEOUT) -> retry;
suggest_recovery_action(?MCP_ERROR_RATE_LIMITED) -> retry;
suggest_recovery_action(?MCP_ERROR_TRANSPORT_ERROR) -> reconnect;
suggest_recovery_action(?MCP_ERROR_NOT_INITIALIZED) -> reconnect;
suggest_recovery_action(_) -> abort.
```

**Rationale:**
- Consistent error mapping prevents protocol violations
- Rich error data aids debugging
- Recovery strategies enable resilience

---

## 6. Backwards Compatibility Strategy

### 6.1 Version Detection and Fallback

**Strategy: Graceful Degradation**
```
1. Client sends preferred protocol version in initialize
2. Server responds with negotiated version
3. If negotiated version < preferred version:
   - Disable features not available in older version
   - Log warning about feature unavailability
4. If negotiated version not supported:
   - Return error with supported versions list
```

**Implementation:**
```erlang
handle_call({initialize, Params}, From, State) ->
    ClientVersion = maps:get(<<"protocolVersion">>, Params, <<"2024-11-05">>),
    case erlmcp_protocol_version:negotiate_version(ClientVersion,
                                                    erlmcp_protocol_version:get_supported_versions()) of
        {ok, NegotiatedVersion} ->
            ServerCaps = apply_version_restrictions(State#server_state.capabilities, NegotiatedVersion),
            ClientCaps = erlmcp_capabilities:extract_client_capabilities(Params),
            FinalCaps = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
            Response = build_initialize_response(NegotiatedVersion, FinalCaps),
            {reply, {ok, Response}, State#server_state{
                protocol_version = NegotiatedVersion,
                client_capabilities = ClientCaps,
                capabilities = FinalCaps
            }};

        {error, {unsupported_version, _}} ->
            ErrorData = #{
                <<"requestedVersion">> => ClientVersion,
                <<"supportedVersions">> => erlmcp_protocol_version:get_supported_versions()
            },
            Error = erlmcp_error_handler:build_error_response(
                maps:get(<<"id">>, Params, null),
                ?MCP_ERROR_UNSUPPORTED_PROTOCOL_VERSION,
                ?MCP_MSG_UNSUPPORTED_PROTOCOL_VERSION,
                ErrorData
            ),
            {reply, {error, Error}, State}
    end.
```

### 6.2 Feature Flag Matrix

```erlang
%% Apply version-specific feature restrictions
-spec apply_version_restrictions(#mcp_server_capabilities{}, binary()) ->
    #mcp_server_capabilities{}.
apply_version_restrictions(Caps, <<"2024-11-05">>) ->
    %% Disable features not available in 2024-11-05
    Caps#mcp_server_capabilities{
        experimental = disable_experimental_features(
            Caps#mcp_server_capabilities.experimental,
            [<<"completions">>, <<"elicitation">>, <<"tasks">>])
    };
apply_version_restrictions(Caps, <<"2025-11-25">>) ->
    Caps;
apply_version_restrictions(Caps, UnknownVersion) ->
    logger:warning("Unknown protocol version: ~p. Applying maximum restrictions.", 
                   [UnknownVersion]),
    apply_version_restrictions(Caps, <<"2024-11-05">>).
```

### 6.3 Method Availability by Version

```erlang
%% Check if method is available in negotiated version
-spec is_method_available(binary(), binary()) -> boolean().
is_method_available(<<"ping">>, <<"2025-11-25">>) -> true;
is_method_available(<<"completion/complete">>, <<"2025-11-25">>) -> true;
is_method_available(<<"elicitation/create">>, <<"2025-11-25">>) -> true;
is_method_available(<<"tasks/create">>, <<"2025-11-25">>) -> true;
is_method_available(<<"ping">>, _) -> false;
is_method_available(<<"completion/complete">>, _) -> false;
%% Core methods available in all versions
is_method_available(<<"initialize">>, _) -> true;
is_method_available(<<"resources/list">>, _) -> true;
is_method_available(<<"tools/call">>, _) -> true;
is_method_available(_, _) -> false.
```

**Rationale:**
- Graceful degradation prevents hard failures
- Clear version communication aids debugging
- Feature flags enable selective disablement

---

## 7. Supervision Tree Architecture

### 7.1 Enhanced Core Supervision Tree

```
erlmcp_sup (one_for_one)
│
├── erlmcp_core_sup (one_for_one) ──────────┐ TIER 1: PROTOCOL CORE
│   ├── erlmcp_protocol_version (NEW)       │ Permanent, 5s shutdown
│   ├── erlmcp_registry                     │ Permanent, 5s shutdown
│   ├── erlmcp_capabilities                 │ Permanent, 5s shutdown
│   ├── erlmcp_request_manager (NEW)        │ Permanent, 10s shutdown
│   ├── erlmcp_notification_dispatcher (NEW)│ Permanent, 5s shutdown
│   ├── erlmcp_session_manager              │ Permanent
│   ├── erlmcp_task_manager                 │ Permanent
│   └── ...                                 │
│                                           │
├── erlmcp_server_sup (simple_one_for_one) │ TIER 2: DYNAMIC SERVERS
│   └── [Dynamic erlmcp_server instances]  │ Temporary
│                                           │
├── erlmcp_client_sup (simple_one_for_one) │ TIER 3: DYNAMIC CLIENTS
│   └── [Dynamic erlmcp_client instances]  │ Temporary
│                                           │
└── erlmcp_observability_sup (one_for_one) │ TIER 4: OBSERVABILITY
    └── [Metrics, traces, etc.]            │ Isolated
```

### 7.2 Child Specs for New Components

**erlmcp_protocol_version:**
```erlang
#{
    id => erlmcp_protocol_version,
    start => {erlmcp_protocol_version, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_protocol_version]
}
```

**erlmcp_request_manager:**
```erlang
#{
    id => erlmcp_request_manager,
    start => {erlmcp_request_manager, start_link, []},
    restart => permanent,
    shutdown => 10000,
    type => worker,
    modules => [erlmcp_request_manager]
}
```

**erlmcp_notification_dispatcher:**
```erlang
#{
    id => erlmcp_notification_dispatcher,
    start => {erlmcp_notification_dispatcher, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_notification_dispatcher]
}
```

### 7.3 Failure Isolation

| Component Crash | Restart Scope | Recovery Time | Impact |
|-----------------|---------------|---------------|--------|
| protocol_version | Component only | ~500ms | New sessions fail until restart |
| request_manager | Component only | ~1s | Pending requests timeout |
| notification_dispatcher | Component only | ~500ms | Notifications lost during restart |
| capabilities | Component only | ~500ms | Capability queries fail temporarily |
| Server instance | Instance only | ~2s | Single connection dropped |
| Client instance | Instance only | ~2s | Single client disconnected |

**Rationale:**
- `one_for_one` strategy prevents cascading failures
- Permanent workers for critical infrastructure
- Temporary workers for dynamic connections

---

## 8. Implementation Roadmap

### Phase 1: Foundation (Week 1-2)
- ✅ `erlmcp_protocol_version` module
- ✅ Enhanced `erlmcp_capabilities`
- ✅ `erlmcp_error_handler` utility
- ✅ Unit tests for version negotiation

### Phase 2: State Management (Week 3-4)
- ✅ Enhanced client state machine
- ✅ Enhanced server state machine
- ✅ Initialization timeout enforcement
- ✅ Integration tests for lifecycle

### Phase 3: Request/Notification (Week 5-6)
- ✅ `erlmcp_request_manager` gen_server
- ✅ `erlmcp_notification_dispatcher` gen_server
- ✅ Request correlation and timeout
- ✅ Performance tests

### Phase 4: Backwards Compatibility (Week 7-8)
- ✅ Version-specific feature flags
- ✅ Method availability checks
- ✅ Compatibility test suite
- ✅ Migration guide

### Phase 5: Integration (Week 9-10)
- ✅ End-to-end integration tests
- ✅ Chaos testing
- ✅ Performance benchmarking
- ✅ Documentation

---

## 9. Testing Strategy

### 9.1 Unit Tests
- Version negotiation (property-based)
- Phase transitions (property-based)
- Error translation (exhaustive)

### 9.2 Integration Tests
- Full initialization sequence
- Timeout scenarios
- Phase enforcement
- Graceful shutdown

### 9.3 Chaos Tests
- Request manager crash
- Notification dispatcher crash
- Protocol version manager crash

---

## 10. Performance Considerations

| Metric | Target |
|--------|--------|
| Version negotiation latency | < 1ms |
| Capability negotiation latency | < 5ms |
| Phase transition latency | < 100μs |
| Request correlation latency | < 50μs |
| Error translation latency | < 10μs |
| Notification dispatch latency | < 100μs |

---

## 11. Security Considerations

### 11.1 Request ID Overflow Protection
- Thresholds at 80%, 90%, 96% of max
- Automatic reconnection when reaching reserved threshold

### 11.2 Input Validation
- Protocol version format validation
- Capability structure validation

### 11.3 DoS Protection
- Per-connection request limits
- Notification queue size limits
- Request history TTL

---

## 12. Observability and Monitoring

### 12.1 Metrics
- `mcp.protocol.version.negotiated` (histogram)
- `mcp.phase.transitions` (counter)
- `mcp.requests.pending` (gauge)
- `mcp.errors.total` (counter by code)

### 12.2 Structured Logging
```erlang
logger:info(#{
    event => protocol_negotiation,
    client_version => <<"2025-11-25">>,
    negotiated_version => <<"2025-11-25">>,
    session_id => SessionId
}).
```

### 12.3 Distributed Tracing
- OpenTelemetry span injection
- Context propagation via `#json_rpc_context{}`

---

## Summary and Next Steps

### Key Architectural Decisions

| Decision | Rationale | Trade-offs |
|----------|-----------|------------|
| Centralized version negotiation | Single source of truth | Additional gen_server |
| Immutable capabilities | Prevents confusion | Cannot add features dynamically |
| Explicit phase machines | Prevents invalid transitions | More verbose |
| Centralized request tracking | Unified timeout/metrics | Single point of failure (mitigated) |
| Error translation layer | Consistent responses | Translation overhead |
| Backwards compatibility via flags | Supports multiple versions | Code complexity |

### Implementation Priority

**Must-Have:**
1. Protocol version negotiation
2. Phase state machines
3. Error translation

**Should-Have:**
1. Request manager
2. Notification dispatcher
3. Backwards compatibility

**Nice-to-Have:**
1. Dynamic capability updates
2. Advanced metrics
3. Chaos testing suite

### Success Metrics

**Compliance:**
- ✅ 100% MCP 2025-11-25 protocol compliance
- ✅ Backwards compatible with 2024-11-05

**Performance:**
- ✅ No regression in throughput
- ✅ < 10% increase in latency

**Reliability:**
- ✅ Zero protocol violations
- ✅ 100% test coverage

---

**Document Control:**
- **Version:** 1.0.0
- **Author:** Erlang Architect Agent
- **Last Updated:** 2026-01-30

