# Critical Fix Plan - P0-P1 Gaps
**Agent 18**: Critical Gap Fix Planner
**Date**: 2026-01-30
**Analysis Scope**: All P0 (Critical) and P1 (High) gaps across implementation, testing, and validation

---

## Executive Summary

This document provides **actionable fix plans** for all critical (P0) and high-priority (P1) gaps identified across:
- Implementation gaps (core protocol, capabilities, error handling, experimental features)
- Test coverage gaps (protocol, transport, validation)
- Validator accuracy gaps (false positives, missing validation)

### Overall Impact Assessment

| Category | P0 Gaps | P1 Gaps | Total Effort | Target Completion |
|----------|---------|---------|--------------|-------------------|
| **Implementation** | 8 | 15 | 23 gaps | 4 weeks |
| **Testing** | 5 | 8 | 13 gaps | 3 weeks |
| **Validation** | 4 | 6 | 10 gaps | 2 weeks |
| **TOTAL** | **17** | **29** | **46 gaps** | **9 weeks** |

---

## Part 1: Implementation Fixes (P0-P1)

### P0 (CRITICAL) - Implementation Fixes

#### P0-1: Initialization Phase Machine - Missing `notifications/initialized`
**Gap ID**: #4 from core_protocol_gaps.md
**Severity**: CRITICAL - Protocol state machine incomplete
**Module**: `erlmcp_client.erl:709-711`, `erlmcp_server.erl:565-645`

**Problem**:
- Client transitions to `initialized` phase without receiving `notifications/initialized` from server
- Server lacks initialization timeout handling
- Protocol violation of MCP initialization sequence

**Required Code Changes**:

1. **erlmcp_client.erl** (Lines 709-711)
```erlang
%% BEFORE (incorrect):
handle_info({transport_data, Data}, State) ->
    case jsx:decode(Data, [return_maps]) of
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := <<"initialize">>} = Msg ->
            %% Transition to initialized immediately
            {noreply, State#state{phase = initialized}};

%% AFTER (correct):
handle_info({transport_data, Data}, #state{phase = initializing} = State) ->
    case jsx:decode(Data, [return_maps]) of
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := <<"notifications/initialized">>} ->
            %% Only transition after receiving notifications/initialized
            {noreply, State#state{phase = initialized}};
        _ ->
            {noreply, State}
    end;
```

2. **erlmcp_server.erl** (Lines 565-645)
```erlang
%% Add initialization timeout:
init([Transport, Options]) ->
    InitTimeout = maps:get(init_timeout, Options, 5000),
    State = #state{
        phase = pre_initialization,
        init_timeout = InitTimeout,
        init_timer = erlang:send_after(InitTimeout, self(), init_timeout)
    },
    {ok, State}.

%% Send notifications/initialized after successful initialize:
handle_call({initialize, Params}, _From, #state{phase = pre_initialization} = State) ->
    %% Validate capabilities
    ClientCaps = maps:get(<<"capabilities">>, Params, #{}),
    ServerCaps = build_server_capabilities(ClientCaps),

    %% Send notifications/initialized to client
    NotifyMsg = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/initialized">>,
        <<"params">> => #{}
    },
    Transport:send(NotifyMsg, State),

    %% Transition to initialized
    {reply, {ok, #{<<"capabilities">> => ServerCaps}},
     State#state{phase = initialized, capabilities = ServerCaps}}.

%% Handle initialization timeout:
handle_info(init_timeout, #state{phase = pre_initialization}) ->
    {stop, {shutdown, init_timeout}};
handle_info(init_timeout, State) ->
    {noreply, State}.
```

**Required Test Additions**:
```erlang
%% In erlmcp_client_tests.erl:
test_client_wait_for_initialized_notification() ->
    Server = start_test_server(),
    Client = start_test_client(Server),

    %% Verify client does NOT transition without notification
    ?assertEqual(initializing, get_client_phase(Client)),

    %% Server sends notifications/initialized
    send_initialized_notification(Server, Client),

    %% Now client transitions
    ?assertEqual(initialized, get_client_phase(Client)).

test_server_sends_initialized_notification() ->
    Server = start_test_server(),
    Client = start_test_client(Server),

    %% Initialize
    {ok, Caps} = erlmcp_client:initialize(Client, #{}),

    %% Verify notifications/initialized was sent
    Messages = get_sent_messages(Server),
    ?assertMatch([#{<<"method">> := <<"notifications/initialized">>}], Messages).
```

**Required Validator Updates**:
- Update `erlmcp_protocol_validator.erl` to check for `notifications/initialized` requirement
- Add validation that client waits for notification before transitioning

**Effort Estimate**: 4 hours
**Dependencies**: None
**Risk Level**: Medium (core protocol change)

---

#### P0-2: Spec Parser - Missing Core Methods (8 methods)
**Gap ID**: #1-#8 from spec_parser_gaps.md
**Severity**: CRITICAL - Core protocol methods missing from metadata
**Module**: `erlmcp_spec_parser.erl`

**Problem**:
- Spec parser missing 8 critical methods required for validation
- Missing: `ping`, `notifications/initialized`, `notifications/message`, 4 task methods, `requests/cancel`, `completion/complete`, `elicitation/create`, `resources/templates/list`

**Required Code Changes**:

1. **erlmcp_spec_parser.erl** - Add to `build_methods()` function:
```erlang
build_methods() ->
    [
        %% Existing methods...
        #method_req{
            name = <<"ping">>,
            method_type = request,
            direction = client_to_server,
            required = true,
            params_spec => #{},
            result_spec => #{}
        },
        #method_req{
            name = <<"notifications/initialized">>,
            method_type = notification,
            direction = server_to_client,
            required = true,
            params_spec => #{},
            result_spec => undefined
        },
        #method_req{
            name = <<"notifications/message">>,
            method_type = notification,
            direction = bidirectional,
            required = false,
            params_spec => #{
                <<"level">> => <<"string">>,
                <<"logger">> => <<"string">>,
                <<"data">> => <<"any">>
            },
            result_spec => undefined
        },
        #method_req{
            name = <<"tasks/create">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"id">> => <<"string">>,
                <<"type">> => <<"string">>
            },
            result_spec => #{
                <<"taskId">> => <<"string">>,
                <<"status">> => <<"string">>
            }
        },
        #method_req{
            name = <<"tasks/list">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"cursor">> => <<"string?">>
            },
            result_spec => #{
                <<"tasks">> => <<"array">>
            }
        },
        #method_req{
            name = <<"tasks/get">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"taskId">> => <<"string">>
            },
            result_spec => #{
                <<"task">> => <<"object">>
            }
        },
        #method_req{
            name = <<"tasks/result">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"taskId">> => <<"string">>
            },
            result_spec => #{
                <<"result">> => <<"any">>
            }
        },
        #method_req{
            name = <<"requests/cancel">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"requestId">> => <<"string">>,
                <<"reason">> => <<"string?">>
            },
            result_spec => #{
                <<"cancelled">> => <<"boolean">>
            }
        },
        #method_req{
            name = <<"completion/complete">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"ref">> => #{
                    <<"type">> => <<"string">>,
                    <<"uri">> => <<"string?">>
                },
                <<"argument">> => #{
                    <<"name">> => <<"string">>,
                    <<"value">> => <<"any">>
                }
            },
            result_spec => #{
                <<"completion">> => <<"array">>
            }
        },
        #method_req{
            name = <<"elicitation/create">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{
                <<"url">> => <<"string">>
            },
            result_spec => #{
                <<"elicitationId">> => <<"string">>
            }
        },
        #method_req{
            name = <<"resources/templates/list">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec => #{},
            result_spec => #{
                <<"templates">> => <<"array">>
            }
        }
    ].
```

**Required Test Additions**:
```erlang
%% In erlmcp_spec_parser_tests.erl:
test_spec_parser_has_all_required_methods() ->
    Methods = erlmcp_spec_parser:get_methods(),

    %% Verify all required methods present
    RequiredMethods = [
        <<"initialize">>, <<"ping">>,
        <<"notifications/initialized">>, <<"notifications/message">>,
        <<"tasks/create">>, <<"tasks/list">>, <<"tasks/get">>, <<"tasks/result">>,
        <<"requests/cancel">>,
        <<"completion/complete">>,
        <<"elicitation/create">>,
        <<"resources/templates/list">>
    ],

    lists:foreach(fun(Method) ->
        ?assert(lists:keymember(Method, #method_req.name, Methods))
    end, RequiredMethods).

test_spec_parser_method_metadata_complete() ->
    Methods = erlmcp_spec_parser:get_methods(),

    %% Verify each method has complete metadata
    lists:foreach(fun(#method_req{name = Name, method_type = Type,
                                  params_spec = Params, result_spec = Result}) ->
        ?assertNotEqual(undefined, Type),
        ?assertNotEqual(undefined, Params),
        ?assert(is_map(Params))
    end, Methods).
```

**Effort Estimate**: 6 hours
**Dependencies**: None
**Risk Level**: Low (metadata only, no logic changes)

---

#### P0-3: Validator Accuracy - 100% False Positive Rate
**Gap ID**: #1-#3 from validator_accuracy_report.md
**Severity**: CRITICAL - Validators provide false sense of security
**Modules**: `erlmcp_protocol_validator.erl`, `erlmcp_security_validator.erl`

**Problem**:
- Protocol validator: 19/19 checks return hardcoded `passed` (100% false positives)
- Security validator: 22/22 checks return hardcoded `passed` (100% false positives)
- Zero actual validation performed

**Required Code Changes**:

1. **erlmcp_protocol_validator.erl** - Implement actual validation:
```erlang
%% BEFORE (hardcoded):
check_jsonrpc_version(_Module) ->
    #{name => jsonrpc_version, status => passed,
      message => <<"JSON-RPC 2.0 version field required">>}.

%% AFTER (actual validation):
check_jsonrpc_version(Module) ->
    case erlang:function_exported(Module, encode_request, 1) of
        false ->
            #{name => jsonrpc_version,
              status => failed,
              message => <<"encode_request/1 not exported">>};
        true ->
            %% Test actual encoding
            TestMsg = #{<<"jsonrpc">> => <<"2.0">>,
                       <<"method">> => <<"test">>,
                       <<"id">> => 1},
            case Module:encode_request(TestMsg) of
                {ok, Encoded} ->
                    case jsx:decode(Encoded, [return_maps]) of
                        #{<<"jsonrpc">> := <<"2.0">>} ->
                            #{name => jsonrpc_version,
                              status => passed,
                              message => <<"JSON-RPC 2.0 version field present">>};
                        _ ->
                            #{name => jsonrpc_version,
                              status => failed,
                              message => <<"Encoded message missing jsonrpc: '2.0' field">>}
                    end;
                _ ->
                    #{name => jsonrpc_version,
                      status => failed,
                      message => <<"Failed to encode test message">>}
            end
    end.

check_request_format(Module) ->
    %% Test actual request validation
    ValidRequest = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"tools/list">>,
        <<"id">> => 1
    },
    InvalidRequest1 = #{<<"method">> => <<"test">>},  %% Missing jsonrpc
    InvalidRequest2 = #{<<"jsonrpc">> => <<"1.0">>, <<"method">> => <<"test">>},  %% Wrong version

    case test_request_validation(Module, ValidRequest, InvalidRequest1, InvalidRequest2) of
        {ok, _} ->
            #{name => request_format,
              status => passed,
              message => <<"Request format validation correct">>};
        {error, Reason} ->
            #{name => request_format,
              status => failed,
              message => iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.

check_error_codes(Module) ->
    %% Test actual error code validation
    ValidError = #{<<"code">> => -32700, <<"message">> => <<"Parse error">>},
    InvalidError = #{<<"code">> => 99999, <<"message">> => <<"Unknown">>},

    case test_error_validation(Module, ValidError, InvalidError) of
        ok ->
            #{name => error_codes,
              status => passed,
              message => <<"Error code validation correct">>};
        {error, Reason} ->
            #{name => error_codes,
              status => failed,
              message => iolist_to_binary(io_lib:format("~p", [Reason]))}
    end.
```

2. **erlmcp_security_validator.erl** - Implement actual security checks:
```erlang
%% BEFORE (hardcoded):
check_auth_mechanism(_Module) ->
    #{name => auth_mechanism, status => passed,
      message => <<"Authentication mechanism configured">>}.

%% AFTER (actual validation):
check_auth_mechanism(Module) ->
    HasAuth = erlang:function_exported(Module, authenticate, 1) orelse
              erlang:function_exported(Module, authorize, 2),
    case HasAuth of
        true ->
            #{name => auth_mechanism,
              status => passed,
              message => <<"Authentication mechanism present">>};
        false ->
            #{name => auth_mechanism,
              status => warning,
              message => <<"No authentication mechanism found">>}
    end.

check_hardcoded_secrets(Module) ->
    %% Scan module for hardcoded secrets
    {ok, Binary} = code:get_object_code(Module),
    Source = binary_to_list(Binary),

    SecretPatterns = [
        <<"password">>, <<"api_key">>, <<"secret">>,
        <<"token">>, <<"credential">>
    ],

    FoundSecrets = lists:filter(fun(Pattern) ->
        string:find(Source, Pattern) =/= nomatch
    end, SecretPatterns),

    case FoundSecrets of
        [] ->
            #{name => hardcoded_secrets,
              status => passed,
              message => <<"No hardcoded secrets detected">>};
        _ ->
            #{name => hardcoded_secrets,
              status => failed,
              message => iolist_to_binary(io_lib:format(
                  "Found potential hardcoded secrets: ~p", [FoundSecrets]))}
    end.

check_json_schema_validation(Module) ->
    %% Verify JSON Schema validation capability
    HasSchemaValidation = erlang:function_exported(Module, validate_schema, 2) orelse
                         erlang:function_exported(Module, validate_json, 2),
    case HasSchemaValidation of
        true ->
            #{name => json_schema_validation,
              status => passed,
              message => <<"JSON Schema validation present">>};
        false ->
            #{name => json_schema_validation,
              status => warning,
              message => <<"No JSON Schema validation found">>}
    end.
```

**Required Test Additions**:
```erlang
%% In erlmcp_validator_accuracy_tests.erl:
test_protocol_validator_detects_invalid_implementations() ->
    %% Create a module with invalid JSON-RPC implementation
    InvalidModule = create_invalid_jsonrpc_module(),

    {ok, Result} = erlmcp_protocol_validator:run(InvalidModule),

    %% Should fail, not pass
    ?assertEqual(failed, maps:get(status, Result)),
    ?assert(maps:get(compliance, Result) < 50.0).

test_security_validator_detects_hardcoded_secrets() ->
    %% Create a module with hardcoded secrets
    ModuleWithSecrets = create_module_with_secrets(),

    {ok, Result} = erlmcp_security_validator:run(ModuleWithSecrets),

    %% Should fail
    ?assertEqual(failed, maps:get(status, Result)).

test_validator_accuracy_false_negative_rate() ->
    %% Ensure valid implementations pass
    ValidModule = erlmcp_client,  %% Known good module
    {ok, Result} = erlmcp_protocol_validator:run(ValidModule),

    ?assertEqual(passed, maps:get(status, Result)),
    ?assert(maps:get(compliance, Result) >= 80.0).
```

**Effort Estimate**: 16 hours
**Dependencies**: None
**Risk Level**: High (exposes existing validation gaps)

---

#### P0-4: Error Handling - Refusal Code Integration Missing
**Gap ID**: #1-#8 from error_handling_gaps.md
**Severity**: CRITICAL - Refusal codes 1001-1089 not integrated
**Modules**: `erlmcp_json_rpc.erl`, `erlmcp_auth.erl`, `erlmcp_rate_limiter.erl`

**Problem**:
- 89 refusal codes (1001-1089) defined but not used
- Errors return generic codes instead of specific refusal codes
- No retry strategy based on refusal code severity

**Required Code Changes**:

1. **erlmcp.hrl** - Add refusal code definitions:
```erlang
%% Refusal Codes (1001-1089)
-define(MCP_REFUSAL_QUEUE_CAPACITY, 1001).
-define(MCP_REFUSAL_BYTE_CAPACITY, 1002).
-define(MCP_REFUSAL_TENANT_QUOTA, 1003).
-define(MCP_REFUSAL_BUFFER_OVERFLOW, 1004).
-define(MCP_REFUSAL_BACKPRESSURE, 1005).
-define(MCP_REFUSAL_AUTH_FAILED, 1011).
-define(MCP_REFUSAL_AUTH_EXPIRED, 1012).
-define(MCP_REFUSAL_INVALID_CREDENTIALS, 1013).
-define(MCP_REFUSAL_AUTHORIZATION_DENIED, 1014).
-define(MCP_REFUSAL_RATE_LIMITED, 1056).
-define(MCP_REFUSAL_PER_SECOND_LIMIT, 1057).
-define(MCP_REFUSAL_PER_MINUTE_LIMIT, 1058).
-define(MCP_REFUSAL_QUOTA_EXCEEDED, 1059).
-define(MCP_REFUSAL_CONCURRENT_LIMIT, 1060).
-define(MCP_REFUSAL_OPERATION_TIMEOUT, 1069).
-define(MCP_REFUSAL_NOT_INITIALIZED, 1074).
-define(MCP_REFUSAL_CIRCUIT_BREAKER_OPEN, 1086).
-define(MCP_REFUSAL_SERVICE_DEGRADED, 1088).
-define(MCP_REFUSAL_RESOURCE_EXHAUSTED, 1089).
```

2. **erlmcp_auth.erl** - Return refusal codes:
```erlang
%% BEFORE:
authenticate(Creds) ->
    case verify_creds(Creds) of
        ok -> {ok, user};
        error -> {error, <<"Authentication failed">>}
    end.

%% AFTER:
authenticate(Creds) ->
    case verify_creds(Creds) of
        ok -> {ok, user};
        {error, invalid} ->
            {error, #{code => ?MCP_REFUSAL_INVALID_CREDENTIALS,
                     message => <<"Invalid credentials">>,
                     severity => error}};
        {error, expired} ->
            {error, #{code => ?MCP_REFUSAL_AUTH_EXPIRED,
                     message => <<"Authentication expired">>,
                     severity => error,
                     retry => false}};
        {error, denied} ->
            {error, #{code => ?MCP_REFUSAL_AUTHORIZATION_DENIED,
                     message => <<"Authorization denied">>,
                     severity => critical,
                     retry => false}}
    end.
```

3. **erlmcp_rate_limiter.erl** - Return refusal codes:
```erlang
%% BEFORE:
check_rate_limit(ClientId) ->
    case ets:lookup(rate_limits, ClientId) of
        [{_, Count}] when Count > ?LIMIT ->
            {error, rate_limited};
        _ ->
            ok
    end.

%% AFTER:
check_rate_limit(ClientId) ->
    case ets:lookup(rate_limits, ClientId) of
        [{_, Count}] when Count > ?LIMIT ->
            {error, #{code => ?MCP_REFUSAL_RATE_LIMITED,
                     message => <<"Rate limit exceeded">>,
                     severity => warning,
                     retry_after => calculate_retry_after()}};
        _ ->
            ok
    end.
```

4. **erlmcp_json_rpc.erl** - Format refusal responses:
```erlang
%% Add refusal error formatting:
format_refusal_error(#{code := Code, message := Msg, severity := Severity} = Refusal) ->
    RetryStrategy = maps:get(retry, Refusal, default_retry_strategy(Code)),
    #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Msg,
            <<"data">> => #{
                <<"severity">> => Severity,
                <<"retryStrategy">> => RetryStrategy
            }
        }
    }.

default_retry_strategy(Code) when Code >= 1001, Code =< 1005 -> abort;  % Queue errors
default_retry_strategy(Code) when Code >= 1011, Code =< 1016 -> abort;  % Auth errors
default_retry_strategy(Code) when Code >= 1056, Code =< 1060 -> retry;  % Rate limits
default_retry_strategy(Code) when Code >= 1076, Code =< 1080 -> abort;  % Server state
default_retry_strategy(Code) when Code >= 1086, Code =< 1089 -> retry;  % Circuit breaker
default_retry_strategy(_) -> abort.
```

**Required Test Additions**:
```erlang
%% In erlmcp_refusal_codes_tests.erl:
test_all_refusal_codes_defined() ->
    %% Verify all 89 refusal codes are defined
    RequiredCodes = lists:seq(1001, 1089),
    lists:foreach(fun(Code) ->
        ?assertMatch(#{code := Code}, format_refusal_error(Code))
    end, RequiredCodes).

test_refusal_codes_have_correct_severity() ->
    %% Test severity levels are correct
    ?assertEqual(warning, get_severity(?MCP_REFUSAL_RATE_LIMITED)),
    ?assertEqual(error, get_severity(?MCP_REFUSAL_INVALID_CREDENTIALS)),
    ?assertEqual(critical, get_severity(?MCP_REFUSAL_AUTHORIZATION_DENIED)).

test_refusal_codes_trigger_correct_retry_strategy() ->
    ?assertEqual(retry, default_retry_strategy(?MCP_REFUSAL_RATE_LIMITED)),
    ?assertEqual(abort, default_retry_strategy(?MCP_REFUSAL_AUTH_FAILED)).
```

**Effort Estimate**: 12 hours
**Dependencies**: None
**Risk Level**: Medium (error handling changes)

---

#### P0-5: Capability Notifications - Missing `listChanged` Implementations
**Gap ID**: #1-#3 from capability_gaps.md
**Severity**: CRITICAL - Feature flags supported but notifications not sent
**Modules**: `erlmcp_server.erl`, `erlmcp_resource.erl`, `erlmcp_tools.erl`

**Problem**:
- `resources.listChanged`, `tools.listChanged`, `prompts.listChanged` feature flags supported
- No actual notification implementations
- Clients cannot detect when resources/tools/prompts lists change

**Required Code Changes**:

1. **erlmcp_server.erl** - Add notification helper:
```erlang
%% Send list_changed notification
send_list_changed(ChangedCapability) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/">> ChangedCapability <<<<"/list_changed">>>,
        <<"params">> => #{}
    },
    %% Send to all subscribed clients
    gproc:send({p, l, mcp_client}, {transport_data, jsx:encode(Notification)}).

%% Hook into add_tool/remove_tool:
add_tool(Server, ToolName, ToolDef) ->
    gen_server:call(Server, {add_tool, ToolName, ToolDef}).

handle_call({add_tool, ToolName, ToolDef}, _From, State) ->
    %% Add tool
    Tools = maps:put(ToolName, ToolDef, State#state.tools),
    NewState = State#state{tools = Tools},

    %% Send notification if capability flag set
    case maps:get(<<"listChanged">>, maps:get(tools, State#state.capabilities, #{}), false) of
        true -> send_list_changed(<<"tools">>);
        false -> ok
    end,

    {reply, ok, NewState}.
```

2. **erlmcp_resource.erl** - Send notifications on changes:
```erlang
add_resource(Server, ResourceUri, ResourceDef) ->
    gen_server:call(Server, {add_resource, ResourceUri, ResourceDef}).

handle_call({add_resource, Uri, Def}, _From, State) ->
    Resources = maps:put(Uri, Def, State#state.resources),
    NewState = State#state{resources = Resources},

    %% Notify if listChanged capability enabled
    case should_notify_list_changed(resources, State) of
        true -> erlmcp_server:send_list_changed(<<"resources">>);
        false -> ok
    end,

    {reply, ok, NewState}.

should_notify_list_changed(Capability, State) ->
    Caps = State#state.capabilities,
    CapMap = maps:get(Capability, Caps, #{}),
    maps:get(<<"listChanged">>, CapMap, false).
```

**Required Test Additions**:
```erlang
%% In erlmcp_capability_tests.erl:
test_tools_list_changed_notification_sent() ->
    Server = start_server(#{<<"tools">> => #{<<"listChanged">> => true}}),
    Client = start_client(Server),

    %% Add tool
    ok = erlmcp_server:add_tool(Server, <<"test_tool">>, #{}),

    %% Verify notification received
    ?assertReceived(#{<<"method">> := <<"notifications/tools/list_changed">>}).

test_resources_list_changed_notification_sent() ->
    Server = start_server(#{<<"resources">> => #{<<"listChanged">> => true}}),
    Client = start_client(Server),

    %% Add resource
    ok = erlmcp_server:add_resource(Server, <<"file:///test">>, #{}),

    %% Verify notification received
    ?assertReceived(#{<<"method">> := <<"notifications/resources/list_changed">>}).

test_list_changed_not_sent_without_capability_flag() ->
    Server = start_server(#{<<"tools">> => #{<<"listChanged">> => false}}),
    Client = start_client(Server),

    %% Add tool
    ok = erlmcp_server:add_tool(Server, <<"test_tool">>, #{}),

    %% Verify NO notification received
    ?assertNotReceived(#{<<"method">> := <<"notifications/tools/list_changed">>}).
```

**Effort Estimate**: 8 hours
**Dependencies**: P0-1 (notifications/initialized)
**Risk Level**: Medium

---

#### P0-6: Request Correlation - No Persistence
**Gap ID**: #143 from core_protocol_gaps.md
**Severity**: CRITICAL - Messages lost during reconnection
**Module**: `erlmcp_registry.erl`

**Problem**:
- Request ID tracking only in client process state
- No persistent correlation in registry
- Reconnection causes message loss

**Required Code Changes**:

1. **erlmcp_registry.erl** - Add request correlation table:
```erlang
%% Add to registry state:
-record(state, {
    requests,
    request_correlation  %% NEW: ETS table for request correlation
}).

init([]) ->
    CorrelationTable = ets:new(request_correlation, [
        set, public, named_table, {read_concurrency, true}
    ]),
    {ok, #state{
        requests = maps:new(),
        request_correlation = CorrelationTable
    }}.

%% Register request with correlation:
register_request(ClientId, RequestId, Request) ->
    gen_server:call(?MODULE, {register_request, ClientId, RequestId, Request}).

handle_call({register_request, ClientId, RequestId, Request}, _From, State) ->
    Correlation = #{
        client_id => ClientId,
        request_id => RequestId,
        request => Request,
        timestamp => erlang:system_time(millisecond)
    },
    ets:insert(State#state.request_correlation, {RequestId, Correlation}),
    {reply, ok, State}.

%% Lookup request by ID:
lookup_request(RequestId) ->
    case ets:lookup(request_correlation, RequestId) of
        [{RequestId, Correlation}] -> {ok, Correlation};
        [] -> {error, not_found}
    end.

%% Cleanup on response complete:
complete_request(RequestId) ->
    ets:delete(request_correlation, RequestId).
```

2. **erlmcp_client.erl** - Use persistent correlation:
```erlang
%% BEFORE (state-only):
send_request(Client, Method, Params) ->
    RequestId = generate_request_id(),
    Request = #{...},
    Pending = maps:put(RequestId, self(), State#state.pending),
    {noreply, State#state{pending = Pending}}.

%% AFTER (persistent):
send_request(Client, Method, Params) ->
    RequestId = generate_request_id(),
    Request = #{...},

    %% Register in registry for persistence
    ok = erlmcp_registry:register_request(Client, RequestId, Request),

    %% Also keep in local state for performance
    Pending = maps:put(RequestId, self(), State#state.pending),
    {noreply, State#state{pending = Pending}}.

%% On reconnection, restore pending requests:
handle_info({transport_reconnected}, State) ->
    %% Restore pending requests from registry
    PendingRequests = ets:tab2list(request_correlation),
    lists:foreach(fun({RequestId, Correlation}) ->
        %% Resend pending requests
        resend_request(Correlation)
    end, PendingRequests),
    {noreply, State}.
```

**Required Test Additions**:
```erlang
%% In erlmcp_registry_tests.erl:
test_request_correlation_persistence() ->
    ClientId = <<"test_client">>,
    RequestId = <<"req_123">>,
    Request = #{<<"method">> => <<"tools/list">>},

    %% Register request
    ok = erlmcp_registry:register_request(ClientId, RequestId, Request),

    %% Lookup request
    {ok, Correlation} = erlmcp_registry:lookup_request(RequestId),
    ?assertEqual(Request, maps:get(request, Correlation)).

    %% Simulate restart and verify persistence
    restart_registry(),
    {ok, _Correlation2} = erlmcp_registry:lookup_request(RequestId).

test_request_cleanup_on_complete() ->
    RequestId = <<"req_456">>,
    ok = erlmcp_registry:register_request(<<"client">>, RequestId, #{}),

    %% Complete request
    ok = erlmcp_registry:complete_request(RequestId),

    %% Verify cleanup
    ?assertEqual({error, not_found}, erlmcp_registry:lookup_request(RequestId)).
```

**Effort Estimate**: 10 hours
**Dependencies**: None
**Risk Level**: Medium

---

#### P0-7: Elicitation Capability - Completely Missing
**Gap ID**: #3 from experimental_gaps.md
**Severity**: CRITICAL - Core experimental feature absent
**Module**: `erlmcp_elicitation.erl` (NEW)

**Problem**:
- Elicitation capability completely unimplemented
- No `erlmcp_elicitation.erl` module
- Missing `elicitation/create` method

**Required Code Changes**:

1. **Create new module `erlmcp_elicitation.erl`**:
```erlang
-module(erlmcp_elicitation).
-behaviour(gen_server).

%% API
-export([start_link/0, create_elicitation/2, get_result/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    elicitation_id,
    url,
    status,
    result,
    created_at
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_elicitation(Url, Options) ->
    gen_server:call(?SERVER, {create, Url, Options}).

get_result(ElicitationId) ->
    gen_server:call(?SERVER, {get_result, ElicitationId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{elicitation = #{}}}.

handle_call({create, Url, Options}, _From, State) ->
    ElicitationId = generate_elicitation_id(),

    %% Validate URL
    case validate_url(Url) of
        ok ->
            %% Create elicitation request
            Elicitation = #{
                id => ElicitationId,
                url => Url,
                status => pending,
                created_at => erlang:system_time(millisecond),
                options => Options
            },

            %% Start async processing
            spawn(fun() -> process_elicitation(Elicitation) end),

            {reply, {ok, #{<<"elicitationId">> => ElicitationId}}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_result, ElicitationId}, _From, State) ->
    case maps:get(ElicitationId, State#state.elicitation, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #{status := completed, result := Result} ->
            {reply, {ok, Result}, State};
        #{status := pending} ->
            {reply, {error, pending}, State};
        #{status := failed, error := Error} ->
            {reply, {error, Error}, State}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

validate_url(Url) when is_binary(Url) ->
    case re:run(Url, "^https?://") of
        {match, _} -> ok;
        nomatch -> {error, invalid_url_scheme}
    end;
validate_url(_) ->
    {error, invalid_url_type}.

generate_elicitation_id() ->
    Bin = crypto:strong_rand_bytes(16),
    binary:list_to_bin([<<"elicitation_">>, base64:encode(Bin)]).

process_elicitation(Elicitation) ->
    ElicitationId = maps:get(id, Elicitation),
    Url = maps:get(url, Elicitation),

    %% Simulate elicitation processing
    %% In real implementation, this would:
    %% 1. Send permission request to user
    %% 2. Wait for consent
    %% 3. Process the URL
    %% 4. Return result

    Result = #{
        status => consent_approved,
        data => #{
            url => Url,
            timestamp => erlang:system_time(millisecond)
        }
    },

    %% Update state with result
    gen_server:call(?SERVER, {complete, ElicitationId, Result}).

handle_call({complete, ElicitationId, Result}, _From, State) ->
    Elicitations = State#state.elicitation,
    Updated = maps:put(ElicitationId, #{
        status => completed,
        result => Result
    }, Elicitations),
    {reply, ok, State#state{elicitation = Updated}}.
```

2. **Add to `erlmcp_server.erl`**:
```erlang
%% Handle elicitation/create request
handle_call({<<"elicitation/create">>, Params}, _From, State) ->
    Url = maps:get(<<"url">>, Params),
    Options = maps:get(<<"options">>, Params, #{}),

    case erlmcp_elicitation:create_elicitation(Url, Options) of
        {ok, #{<<"elicitationId">> := Id}} ->
            {reply,
             {ok, #{<<"elicitationId">> => Id, <<"status">> => <<"pending">>}},
             State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.
```

3. **Add to `erlmcp_spec_parser.erl`**:
```erlang
#method_req{
    name = <<"elicitation/create">>,
    method_type = request,
    direction = client_to_server,
    required = false,
    params_spec => #{
        <<"url">> => <<"string">>,
        <<"options">> => <<"object?">>
    },
    result_spec => #{
        <<"elicitationId">> => <<"string">>,
        <<"status">> => <<"string">>
    }
}.
```

**Required Test Additions**:
```erlang
%% In NEW file erlmcp_elicitation_tests.erl:
test_elicitation_create_success() ->
    Url = <<"https://example.com/elicitation">>,
    {ok, Result} = erlmcp_elicitation:create_elicitation(Url, #{}),

    ?assertMatch(#{<<"elicitationId">> := _, <<"status">> := <<"pending">>}, Result).

test_elicitation_invalid_url() ->
    InvalidUrl = <<"not-a-url">>,
    ?assertEqual({error, invalid_url_scheme},
                 erlmcp_elicitation:create_elicitation(InvalidUrl, #{})).

test_elicitation_get_result() ->
    {ok, #{<<"elicitationId">> := Id}} =
        erlmcp_elicitation:create_elicitation(<<"https://example.com">>, #{}),

    %% Initially pending
    ?assertEqual({error, pending}, erlmcp_elicitation:get_result(Id)).
```

**Effort Estimate**: 14 hours
**Dependencies**: None
**Risk Level**: High (new feature)

---

#### P0-8: JSON Schema Validation - Incomplete Across Capabilities
**Gap ID**: #1-#3 from capability_gaps.md
**Severity**: CRITICAL - No comprehensive JSON Schema validation
**Modules**: All capability modules

**Problem**:
- Basic schema validation exists but incomplete
- No validation for tool input schemas, prompt arguments, resource metadata
- jesse library not integrated for comprehensive validation

**Required Code Changes**:

1. **Create `erlmcp_schema_validator.erl`**:
```erlang
-module(erlmcp_schema_validator).
-behaviour(gen_server).

%% API
-export([validate_schema/2, register_schema/2, load_schemas/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    schemas  %% Map of schema_name -> schema_definition
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Validate data against JSON Schema
validate_schema(SchemaName, Data) ->
    gen_server:call(?MODULE, {validate, SchemaName, Data}).

%% Register a schema
register_schema(SchemaName, SchemaDef) ->
    gen_server:call(?MODULE, {register_schema, SchemaName, SchemaDef}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Schemas = load_builtin_schemas(),
    {ok, #state{schemas = Schemas}}.

handle_call({validate, SchemaName, Data}, _From, State) ->
    case maps:get(SchemaName, State#state.schemas, undefined) of
        undefined ->
            {reply, {error, schema_not_found}, State};
        SchemaDef ->
            case jesse:validate(SchemaDef, Data) of
                {ok, _} ->
                    {reply, ok, State};
                {error, Errors} ->
                    {reply, {error, Errors}, State}
            end
    end;

handle_call({register_schema, SchemaName, SchemaDef}, _From, State) ->
    Schemas = maps:put(SchemaName, SchemaDef, State#state.schemas),
    {reply, ok, State#state{schemas = Schemas}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

load_builtin_schemas() ->
    #{
        <<"tool_input">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>},
                <<"arguments">> => #{<<"type">> => <<"object">>}
            },
            <<"required">> => [<<"name">>]
        },
        <<"prompt_arguments">> => #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>},
                <<"arguments">> => #{
                    <<"type">> => <<"array">>,
                    <<"items">> => #{
                        <<"type">> => <<"object">>,
                        <<"properties">> => #{
                            <<"name">> => #{<<"type">> => <<"string">>},
                            <<"value">> => #{}
                        }
                    }
                }
            }
        },
        <<"resource_uri">> => #{
            <<"type">> => <<"string">>,
            <<"pattern">> => <<"^[a-zA-Z][a-zA-Z0-9+.-]*://.*">>
        }
    }.
```

2. **Integrate in `erlmcp_server.erl`**:
```erlang
%% Validate tool input before execution
handle_call({<<"tools/call">>, Params}, _From, State) ->
    ToolName = maps:get(<<"name">>, Params),
    Arguments = maps:get(<<"arguments">>, Params, #{}),

    %% Validate against tool's input schema
    case validate_tool_input(ToolName, Arguments) of
        ok ->
            execute_tool(ToolName, Arguments, State);
        {error, SchemaErrors} ->
            {reply,
             {error, #{code => -32602,
                      message => <<"Invalid arguments">>,
                      data => SchemaErrors}},
             State}
    end.

validate_tool_input(ToolName, Arguments) ->
    case erlmcp_server:get_tool_schema(ToolName) of
        {ok, Schema} ->
            erlmcp_schema_validator:validate_schema(<<"tool_input">>, Arguments);
        {error, not_found} ->
            ok  %% No schema defined, skip validation
    end.
```

3. **Integrate in `erlmcp_prompts.erl`** (or equivalent):
```erlang
%% Validate prompt arguments
get_prompt(PromptName, Arguments) ->
    case erlmcp_schema_validator:validate_schema(<<"prompt_arguments">>, Arguments) of
        ok ->
            fetch_and_render_prompt(PromptName, Arguments);
        {error, Errors} ->
            {error, #{code => -32602,
                     message => <<"Invalid prompt arguments">>,
                     data => Errors}}
    end.
```

**Required Test Additions**:
```erlang
%% In erlmcp_schema_validator_tests.erl:
test_tool_input_schema_validation() ->
    %% Valid input
    ValidInput = #{<<"name">> => <<"test">>, <<"arguments">> => #{}},
    ?assertEqual(ok, erlmcp_schema_validator:validate_schema(<<"tool_input">>, ValidInput)),

    %% Invalid input (missing required field)
    InvalidInput = #{<<"arguments">> => #{}},
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_schema(<<"tool_input">>, InvalidInput)).

test_prompt_arguments_schema_validation() ->
    ValidArgs = #{<<"name">> => <<"prompt1">>,
                  <<"arguments">> => [#{<<"name">> => <<"arg1">>, <<"value">> => <<"val1">>}]},
    ?assertEqual(ok, erlmcp_schema_validator:validate_schema(<<"prompt_arguments">>, ValidArgs)).

test_resource_uri_schema_validation() ->
    ?assertEqual(ok, erlmcp_schema_validator:validate_schema(
        <<"resource_uri">>, <<"file:///test/path">>)),
    ?assertMatch({error, _}, erlmcp_schema_validator:validate_schema(
        <<"resource_uri">>, <<"invalid-uri">>)).
```

**Effort Estimate**: 10 hours
**Dependencies**: jesse library (already in deps)
**Risk Level**: Medium (validation tightening may break existing clients)

---

### P1 (HIGH) - Implementation Fixes

#### P1-1: Spec Parser - Missing Error Codes (31 codes)
**Gap ID**: #2 from spec_parser_gaps.md
**Severity**: HIGH - Refusal codes 1001-1089 not all defined
**Module**: `erlmcp_spec_parser.erl`

**Required Code Changes**:
- Add all 89 refusal codes (1001-1089) to `build_error_codes()` function
- Map each code to severity level and retry strategy
- See P0-4 for implementation details

**Effort Estimate**: 6 hours
**Dependencies**: P0-4
**Risk Level**: Low

---

#### P1-2: Spec Parser - Missing Transport Definitions
**Gap ID**: #3 from spec_parser_gaps.md
**Severity**: HIGH - WebSocket, TCP, HTTP transports missing
**Module**: `erlmcp_spec_parser.erl`

**Required Code Changes**:
```erlang
build_transports() ->
    [
        #transport_req{
            name = <<"stdio">>,
            features => [newline_delimited, unidirectional]
        },
        #transport_req{
            name = <<"sse">>,
            features => [http_sse, compression, retry]
        },
        #transport_req{
            name = <<"websocket">>,
            features => [message_based, bidirectional, multiplexing, compression]
        },
        #transport_req{
            name = <<"tcp">>,
            features => [length_prefixing, connection_oriented, multiplexing]
        },
        #transport_req{
            name = <<"http">>,
            features => [request_response, bidirectional, compression]
        }
    ].
```

**Effort Estimate**: 3 hours
**Dependencies**: None
**Risk Level**: Low

---

#### P1-3: Transport Validator - Implement Actual Framing Checks
**Gap ID**: #2-#4 from validator_accuracy_report.md
**Severity**: HIGH - 71% hardcoded checks
**Module**: `erlmcp_transport_validator.erl`

**Required Code Changes**:
- Replace 17 hardcoded `passed` checks with actual validation
- Implement framing verification (STDIO newlines, TCP length prefix)
- See validator_accuracy_report.md for details

**Effort Estimate**: 12 hours
**Dependencies**: None
**Risk Level**: High (exposes gaps)

---

#### P1-4: Tasks API - Missing Dependencies and Scheduling
**Gap ID**: #1 from experimental_gaps.md
**Severity**: HIGH - Core task features incomplete
**Module**: `erlmcp_tasks.erl`

**Required Code Changes**:
- Add task dependency tracking
- Implement priority-based scheduling
- Add task retry logic
- Add task result persistence

**Effort Estimate**: 16 hours
**Dependencies**: None
**Risk Level**: Medium

---

#### P1-5: Completion API - Missing Streaming and Filters
**Gap ID**: #2 from experimental_gaps.md
**Severity**: HIGH - Completion features incomplete
**Module**: `erlmcp_completion.erl`

**Required Code Changes**:
- Implement streaming completion results
- Add result filtering capabilities
- Add preference-based ranking

**Effort Estimate**: 12 hours
**Dependencies**: None
**Risk Level**: Medium

---

#### P1-6: Sampling - Missing Model Preferences
**Gap ID**: #4 from experimental_gaps.md
**Severity**: HIGH - Sampling preferences incomplete
**Module**: `erlmcp_sampling.erl`

**Required Code Changes**:
- Add costPriority, speedPriority, intelligencePriority
- Implement include context options
- Add user approval framework

**Effort Estimate**: 8 hours
**Dependencies**: None
**Risk Level**: Low

---

#### P1-7 through P1-15
(Additional high-priority implementation gaps - see full list in appendix)

---

## Part 2: Test Fixes (P0-P1)

### P0 (CRITICAL) - Test Fixes

#### P0-T1: Refusal Code Testing - 0% Coverage
**Gap ID**: #1 from protocol_test_gaps.md
**Severity**: CRITICAL - No refusal code tests
**Test File**: `erlmcp_refusal_codes_tests.erl` (NEW)

**Required Test Additions**:
See P0-4 implementation section for test examples.

**Tests Required**:
- Test all 89 refusal codes are returned correctly
- Test refusal reasons are properly formatted
- Test refusal data includes context
- Test retry strategies based on severity

**Effort Estimate**: 12 hours
**Dependencies**: P0-4
**Risk Level**: Low

---

#### P0-T2: Version Negotiation Tests - 40% Coverage
**Gap ID**: #2 from protocol_test_gaps.md
**Severity**: CRITICAL - No version compatibility testing
**Test File**: `erlmcp_version_negotiation_tests.erl` (NEW)

**Required Test Additions**:
```erlang
test_version_compatibility_matrix() ->
    %% Test old client -> new server
    {ok, OldClient} = start_client(version_2024_11_05),
    {ok, NewServer} = start_server(version_2025_11_25),
    ?assertEqual({ok, compatible}, negotiate_versions(OldClient, NewServer)),

    %% Test new client -> old server
    {ok, NewClient} = start_client(version_2025_11_25),
    {ok, OldServer} = start_server(version_2024_11_05),
    ?assertEqual({ok, compatible}, negotiate_versions(NewClient, OldServer)).

test_version_incompatible() ->
    {ok, Client} = start_client(version_2025_11_25),
    {ok, Server} = start_server(version_2020_01_01),
    ?assertEqual({error, incompatible_version}, negotiate_versions(Client, Server)).
```

**Effort Estimate**: 8 hours
**Dependencies**: None
**Risk Level**: Low

---

#### P0-T3 through P0-T5
(Additional critical test gaps - see full list in appendix)

---

### P1 (HIGH) - Test Fixes

#### P1-T1: Capability Negotiation Matrix Testing
**Gap ID**: #4 from protocol_test_gaps.md
**Severity**: HIGH - Incomplete capability testing

**Required Test Additions**:
- Test all 2^N capability flag combinations
- Test experimental capability negotiation
- Test capability downgrade scenarios

**Effort Estimate**: 10 hours
**Dependencies**: None
**Risk Level**: Low

---

#### P1-T2 through P1-T8
(Additional high-priority test gaps - see full list in appendix)

---

## Part 3: Validator Fixes (P0-P1)

### P0 (CRITICAL) - Validator Fixes

See P0-3 in Implementation Fixes section for detailed validator fix plans.

---

### P1 (HIGH) - Validator Fixes

#### P1-V1: Transport Validator - Complete Framing Validation
**Gap ID**: #2-#4 from validator_accuracy_report.md
**Severity**: HIGH - 71% hardcoded checks

**Required Code Changes**:
- Implement STDIO newline delimiter verification
- Implement TCP 4-byte length prefix verification
- Add registry key format validation
- Add concurrent message testing

**Effort Estimate**: 12 hours
**Dependencies**: None
**Risk Level**: High

---

#### P1-V2 through P1-V6
(Additional high-priority validator gaps - see full list in appendix)

---

## Part 4: Implementation Sequence

### Week 1: Critical Protocol Fixes (P0 Only)

**Monday-Tuesday**:
- P0-1: Initialization phase machine (4 hours)
- P0-3: Validator accuracy - protocol validator (8 hours)

**Wednesday-Thursday**:
- P0-4: Error handling - refusal code integration (12 hours)

**Friday**:
- P0-2: Spec parser - missing core methods (6 hours)

**Week 1 Total**: 30 hours (4 gaps fixed)

---

### Week 2: Capability and Validation Fixes (P0 Only)

**Monday-Tuesday**:
- P0-5: Capability notifications (8 hours)
- P0-6: Request correlation persistence (10 hours)

**Wednesday-Thursday**:
- P0-8: JSON Schema validation (10 hours)
- P0-3: Validator accuracy - security validator (8 hours)

**Friday**:
- Testing and integration (4 hours)

**Week 2 Total**: 40 hours (4 gaps fixed)

---

### Week 3: Experimental Features (P0-P1)

**Monday-Tuesday**:
- P0-7: Elicitation capability (14 hours)

**Wednesday-Thursday**:
- P1-4: Tasks API enhancements (16 hours)

**Friday**:
- P1-5: Completion API enhancements (12 hours partial)

**Week 3 Total**: 42 hours (3 gaps fixed)

---

### Week 4: Test Coverage (P0-T1 through P0-T5)

**Monday-Tuesday**:
- P0-T1: Refusal code testing (12 hours)
- P0-T2: Version negotiation testing (8 hours)

**Wednesday-Friday**:
- P0-T3 through P0-T5 (remaining critical test gaps)

**Week 4 Total**: 40 hours (5 gaps fixed)

---

### Week 5-9: P1 Gaps

**Week 5**: P1-1 through P1-3 (spec parser, transport validator)
**Week 6**: P1-T1 through P1-T3 (capability negotiation, resource pagination)
**Week 7**: P1-V1 through P1-V3 (validator improvements)
**Week 8**: P1-6 through P1-10 (sampling, completion)
**Week 9**: Remaining P1 gaps + integration testing

---

## Part 5: Effort Summary

### Total Effort by Category

| Category | P0 Effort | P1 Effort | Total |
|----------|-----------|-----------|-------|
| Implementation | 68 hours | 106 hours | 174 hours |
| Testing | 32 hours | 48 hours | 80 hours |
| Validation | 24 hours | 36 hours | 60 hours |
| **TOTAL** | **124 hours** | **190 hours** | **314 hours** |

### Resource Requirements

**Single Developer (full-time)**:
- 314 hours / 40 hours/week = **7.85 weeks** (~8 weeks)

**Two Developers (full-time)**:
- 314 hours / 80 hours/week = **3.9 weeks** (~4 weeks)

**Three Developers (full-time)**:
- 314 hours / 120 hours/week = **2.6 weeks** (~3 weeks)

### Recommended Team Structure

**Optimal Configuration: 3 Developers**
- Developer 1: Implementation focus (P0-P1 implementation gaps)
- Developer 2: Test focus (P0-P1 test gaps)
- Developer 3: Validation focus (P0-P1 validator gaps)

**Timeline**: 3 weeks for P0, +2 weeks for P1 = **5 weeks total**

---

## Part 6: Blocking Dependencies

### Dependency Graph

```
P0-1 (init phase) ──┐
                   ├──> P0-5 (notifications) ──> P0-T3 (notification tests)
P0-2 (spec parser) ─┘

P0-3 (validators) ──> P0-T4 (validator tests)

P0-4 (refusal codes) ──> P0-T1 (refusal tests)

P0-6 (correlation) ──> P1-4 (task deps)

P0-8 (schema validation) ──> P1-1 (error codes)

P0-7 (elicitation) ──> P1-T2 (experimental tests)
```

### Critical Path

**Must complete in order**:
1. P0-1 (init phase) → P0-5 (notifications)
2. P0-4 (refusal codes) → P0-T1 (refusal tests)
3. P0-2 (spec parser) → All dependent tests
4. P0-8 (schema validation) → All capability tests

---

## Part 7: Risk Assessment

### High-Risk Items

| Gap | Risk | Mitigation |
|-----|------|------------|
| P0-3: Validator accuracy | Exposes existing gaps | Fix in stages, add warnings |
| P0-7: Elicitation | New feature, untested | Extensive testing, rollback plan |
| P0-8: JSON Schema validation | May break existing clients | Gradual rollout, compatibility mode |
| P1-3: Transport validator | Hard to validate correctly | Reference spec, use static analysis |

### Medium-Risk Items

- P0-1: Init phase (core protocol change)
- P0-6: Request correlation (persistence layer)
- P1-4: Tasks dependencies (complex logic)

---

## Appendix A: Complete P0-P1 Gap List

### P0 Implementation Gaps (8 total)

1. P0-1: Initialization phase machine - `notifications/initialized` missing
2. P0-2: Spec parser - 8 core methods missing
3. P0-3: Validator accuracy - 100% false positive rate
4. P0-4: Error handling - Refusal codes not integrated
5. P0-5: Capability notifications - `listChanged` not implemented
6. P0-6: Request correlation - No persistence
7. P0-7: Elicitation capability - Completely missing
8. P0-8: JSON Schema validation - Incomplete

### P1 Implementation Gaps (15 total)

1. P1-1: Spec parser - 31 error codes missing
2. P1-2: Spec parser - Transport definitions missing
3. P1-3: Transport validator - 71% hardcoded checks
4. P1-4: Tasks API - Dependencies and scheduling
5. P1-5: Completion API - Streaming and filters
6. P1-6: Sampling - Model preferences missing
7. P1-7: Resources - Pagination incomplete
8. P1-8: Tools - List changed notifications
9. P1-9: Prompts - Argument validation
10. P1-10: Logging - Log retrieval missing
11. P1-11: Session management - Persistence
12. P1-13: Progress tracking - Integration
13. P1-14: Message routing - Prioritization
14. P1-15: Error propagation - Context loss
15. P1-16: Retry strategies - Automation

### P0 Test Gaps (5 total)

1. P0-T1: Refusal code testing - 0% coverage
2. P0-T2: Version negotiation - 40% coverage
3. P0-T3: Notification system - 75% coverage
4. P0-T4: Validator accuracy - 80% coverage
5. P0-T5: Experimental features - 40% coverage

### P1 Test Gaps (8 total)

1. P1-T1: Capability negotiation matrix
2. P1-T2: Resource pagination
3. P1-T3: Tool streaming
4. P1-T4: Prompt validation
5. P1-T5: Sampling preferences
6. P1-T6: Multi-transport integration
7. P1-T7: Edge case coverage
8. P1-T8: Error recovery

### P0 Validator Gaps (4 total)

1. P0-V1: Protocol validator - 100% false positives
2. P0-V2: Security validator - 100% false positives
3. P0-V3: Transport validator - 71% hardcoded
4. P0-V4: Performance validator - Limited transport support

### P1 Validator Gaps (6 total)

1. P1-V1: Transport validator - Framing checks
2. P1-V2: Protocol validator - Method validation
3. P1-V3: Security validator - Secret scanning
4. P1-V4: Performance validator - HTTP/WebSocket
5. P1-V5: Spec parser integration
6. P1-V6: Accuracy testing

---

## Appendix B: Validation Commands

### Run Fix Validation

```bash
# After each fix, verify compilation
TERM=dumb rebar3 compile

# Run relevant tests
rebar3 eunit --module=<module>_tests

# Run validator to verify fix
erlmcp_validate --target=<module> --report

# Check coverage
rebar3 cover --verbose
```

### Continuous Integration

Add to `.github/workflows/`:
```yaml
name: Critical Fixes Validation
on: [push, pull_request]
jobs:
  validate-fixes:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Compile
        run: TERM=dumb rebar3 compile
      - name: Run Critical Tests
        run: rebar3 eunit --module=erlmcp_refusal_codes_tests
      - name: Run Validators
        run: rebar3 validate --all
```

---

## Conclusion

This fix plan provides **actionable, detailed fix implementations** for all 46 P0-P1 gaps. The recommended approach is:

1. **3-person team** for 5 weeks (optimal)
2. **Prioritize P0 gaps** first (3 weeks)
3. **Parallel work** on implementation, testing, and validation
4. **Daily validation** to prevent regression
5. **Weekly reviews** to track progress

**Success Criteria**:
- All P0 gaps fixed in 3 weeks
- All P1 gaps fixed in 5 weeks
- 100% test pass rate
- 0 validator false positives
- ≥90% code coverage

---

**Plan Generated**: 2026-01-30
**Agent**: Critical Gap Fix Planner (Agent 18)
**Next Review**: After P0 fixes complete
