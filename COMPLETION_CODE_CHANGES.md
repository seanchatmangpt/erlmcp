# MCP Completion Integration - Code Changes

## Summary
Integration of erlmcp_completion module into erlmcp server for MCP 2025-11-25 spec compliance.

---

## Change 1: Supervision Tree Integration

**File**: `apps/erlmcp_core/src/erlmcp_core_sup.erl`
**Location**: Lines 209-219

```erlang
%% ================================================================
%% COMPLETION: Argument completion per MCP 2025-11-25 spec
%% ================================================================
#{
    id => erlmcp_completion,
    start => {erlmcp_completion, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [erlmcp_completion]
},
```

**Purpose**: Add erlmcp_completion as a supervised worker in the core supervision tree.

---

## Change 2: Request Handler

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Location**: Lines 1335-1423

```erlang
%% Completion/complete endpoint - MCP 2025-11-25 spec argument completion
handle_request(Id, ?MCP_METHOD_COMPLETION_COMPLETE, Params, TransportId, #state{server_id = ServerId} = State) ->
    SpanCtx = erlmcp_tracing:start_server_span(<<"server.handle_completion_complete">>, ServerId),
    try
        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"request_id">> => Id,
            <<"transport_id">> => TransportId,
            <<"method">> => ?MCP_METHOD_COMPLETION_COMPLETE
        }),

        %% Extract ref (tool/resource/prompt reference)
        Ref = maps:get(<<"ref">>, Params, undefined),

        %% Extract argument structure
        ArgumentMap = maps:get(<<"argument">>, Params, undefined),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"completion.ref">> => Ref,
            <<"completion.argument">> => ArgumentMap
        }),

        case {Ref, ArgumentMap} of
            {undefined, _} ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_ref, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    <<"Missing required parameter 'ref'">>),
                {noreply, State};
            {_, undefined} ->
                erlmcp_tracing:record_error_details(SpanCtx, missing_argument, undefined),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    <<"Missing required parameter 'argument'">>),
                {noreply, State};
            {RefValue, ArgValue} when is_binary(RefValue), is_map(ArgValue) ->
                %% Build completion context from server state
                Context = build_completion_context(RefValue, State),

                %% Call completion service
                case erlang:whereis(erlmcp_completion) of
                    undefined ->
                        erlmcp_tracing:record_error_details(SpanCtx, completion_service_unavailable, undefined),
                        logger:warning("Completion service not available"),
                        send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR,
                            <<"Completion service unavailable">>),
                        {noreply, State};
                    CompletionPid ->
                        case erlmcp_completion:complete(CompletionPid, RefValue, ArgValue, Context) of
                            {ok, Result} ->
                                %% Transform result to MCP format
                                Response = format_completion_response(Result),
                                erlmcp_tracing:set_attributes(SpanCtx, #{
                                    <<"completion.total">> => maps:get(total, Result, 0),
                                    <<"completion.hasMore">> => maps:get(hasMore, Result, false)
                                }),
                                erlmcp_tracing:set_status(SpanCtx, ok),
                                send_response_via_registry(State, TransportId, Id, Response),
                                {noreply, State};
                            {error, {completion_ref_not_found, _Code, Message}} ->
                                erlmcp_tracing:record_error_details(SpanCtx, completion_ref_not_found, RefValue),
                                send_error_via_registry(State, TransportId, Id, ?JSONRPC_METHOD_NOT_FOUND, Message),
                                {noreply, State};
                            {error, {completion_rate_limited, Code, Message}} ->
                                erlmcp_tracing:record_error_details(SpanCtx, completion_rate_limited, RefValue),
                                send_error_via_registry(State, TransportId, Id, Code, Message),
                                {noreply, State};
                            {error, {completion_handler_failed, Code, Message}} ->
                                erlmcp_tracing:record_error_details(SpanCtx, completion_handler_failed, RefValue),
                                send_error_via_registry(State, TransportId, Id, Code, Message),
                                {noreply, State};
                            {error, Reason} ->
                                erlmcp_tracing:record_error_details(SpanCtx, completion_failed, Reason),
                                logger:error("Completion failed: ~p", [Reason]),
                                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR,
                                    <<"Completion request failed">>),
                                {noreply, State}
                        end
                end;
            _ ->
                erlmcp_tracing:record_error_details(SpanCtx, invalid_params, {Ref, ArgumentMap}),
                send_error_via_registry(State, TransportId, Id, ?JSONRPC_INVALID_PARAMS,
                    <<"Invalid parameters: 'ref' must be binary, 'argument' must be map">>),
                {noreply, State}
        end
    catch
        Class:Reason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, Reason, Stacktrace),
            logger:error("Completion request crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            send_error_via_registry(State, TransportId, Id, ?JSONRPC_INTERNAL_ERROR, ?JSONRPC_MSG_INTERNAL_ERROR),
            {noreply, State}
    after
        erlmcp_tracing:end_span(SpanCtx)
    end;
```

**Purpose**: Handle completion/complete JSON-RPC requests with full validation and error handling.

---

## Change 3: Helper Functions

**File**: `apps/erlmcp_core/src/erlmcp_server.erl`
**Location**: Lines 2824-2883

```erlang
%%====================================================================
%% Internal functions - Completion Support (MCP 2025-11-25)
%%====================================================================

%% @doc Build completion context from server state
%% Provides context about available tools, resources, prompts for completion
-spec build_completion_context(binary(), state()) -> map().
build_completion_context(Ref, State) ->
    %% Determine the type of reference based on registered items
    Type = case {maps:is_key(Ref, State#state.tools),
                 maps:is_key(Ref, State#state.resources),
                 maps:is_key(Ref, State#state.prompts)} of
        {true, _, _} -> <<"tool">>;
        {_, true, _} -> <<"resource">>;
        {_, _, true} -> <<"prompt">>;
        _ -> <<"unknown">>
    end,

    %% Build context with available information
    #{
        <<"type">> => Type,
        <<"tools">> => maps:keys(State#state.tools),
        <<"resources">> => maps:keys(State#state.resources),
        <<"prompts">> => maps:keys(State#state.prompts)
    }.

%% @doc Format completion response to MCP format
%% Transforms internal completion result to MCP 2025-11-25 spec format
-spec format_completion_response(map()) -> map().
format_completion_response(Result) ->
    Completions = maps:get(completions, Result, []),
    HasMore = maps:get(hasMore, Result, false),
    Total = maps:get(total, Result, length(Completions)),

    %% Transform completion items to ensure correct format
    FormattedCompletions = lists:map(fun format_completion_item/1, Completions),

    #{
        <<"completion">> => #{
            <<"values">> => FormattedCompletions,
            <<"total">> => Total,
            <<"hasMore">> => HasMore
        }
    }.

%% @doc Format individual completion item
-spec format_completion_item(map()) -> map().
format_completion_item(Item) ->
    Value = maps:get(value, Item, maps:get(<<"value">>, Item, <<>>)),
    Label = case maps:get(label, Item, maps:get(<<"label">>, Item, undefined)) of
        undefined -> undefined;
        L -> L
    end,

    BaseItem = #{<<"value">> => Value},

    case Label of
        undefined -> BaseItem;
        _ -> BaseItem#{<<"label">> => Label}
    end.
```

**Purpose**: Provide helper functions for context building and MCP format transformation.

---

## Integration Flow

```
JSON-RPC Request
    ↓
erlmcp_server:handle_request(?MCP_METHOD_COMPLETION_COMPLETE, ...)
    ↓
Validate params (ref, argument)
    ↓
build_completion_context(Ref, State)
    ↓
erlmcp_completion:complete(Pid, Ref, Argument, Context)
    ├─→ Check cache (ETS lookup)
    ├─→ Check rate limit
    ├─→ Call registered handler
    ├─→ Rank results (Jaro-Winkler)
    └─→ Cache result (TTL 30s)
    ↓
format_completion_response(Result)
    ↓
send_response_via_registry(...)
    ↓
JSON-RPC Response
```

---

## Key Design Decisions

1. **Service Location**: Used `erlang:whereis(erlmcp_completion)` for graceful degradation if service not available
2. **Context Building**: Extract server state (tools, resources, prompts) to provide completion handlers with context
3. **Error Mapping**: Map internal erlmcp_completion errors to proper MCP error codes
4. **Tracing**: Full OpenTelemetry integration for observability
5. **Format Transformation**: Handle both atom and binary keys for robustness

---

## Files Modified

- `apps/erlmcp_core/src/erlmcp_core_sup.erl`: +9 lines
- `apps/erlmcp_core/src/erlmcp_server.erl`: +149 lines

**Total**: 158 lines added

---
**Status**: INTEGRATION COMPLETE ✓
**Compilation**: PENDING (requires Erlang environment)
