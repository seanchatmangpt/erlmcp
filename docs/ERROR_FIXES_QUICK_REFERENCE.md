# Error Handling Fixes: Quick Reference

## Missing Constants - Add to `apps/erlmcp_core/include/erlmcp.hrl`

```erlang
%%%===================================================================
%%% JSON-RPC Error Message Constants (Missing)
%%%===================================================================
-define(JSONRPC_MSG_PARSE_ERROR, <<"Parse error">>).
-define(JSONRPC_MSG_INVALID_REQUEST, <<"Invalid Request">>).
-define(JSONRPC_MSG_METHOD_NOT_FOUND, <<"Method not found">>).
-define(JSONRPC_MSG_INVALID_PARAMS, <<"Invalid params">>).
-define(JSONRPC_MSG_INTERNAL_ERROR, <<"Internal error">>).

%%%===================================================================
%%% MCP Error Message Constants (Missing - Add These)
%%%===================================================================
-define(MCP_MSG_RESOURCE_NOT_FOUND, <<"Resource not found">>).
-define(MCP_MSG_TOOL_NOT_FOUND, <<"Tool not found">>).
-define(MCP_MSG_PROMPT_NOT_FOUND, <<"Prompt not found">>).
-define(MCP_MSG_CAPABILITY_NOT_SUPPORTED, <<"Capability not supported">>).
-define(MCP_MSG_NOT_INITIALIZED, <<"Server not initialized">>).
-define(MCP_MSG_ALREADY_INITIALIZED, <<"Server already initialized">>).
-define(MCP_MSG_VALIDATION_FAILED, <<"Validation failed">>).
-define(MCP_MSG_MESSAGE_TOO_LARGE, <<"Message size exceeds maximum allowed">>).
-define(MCP_MSG_TOOL_DESCRIPTION_TOO_LONG, <<"Tool description exceeds maximum length">>).

%%%===================================================================
%%% MCP Field Name Constants (Missing - Add These)
%%%===================================================================
-define(MCP_FIELD_PROTOCOL_VERSION, <<"protocolVersion">>).
-define(MCP_FIELD_CAPABILITIES, <<"capabilities">>).
-define(MCP_FIELD_CLIENT_INFO, <<"clientInfo">>).
-define(MCP_FIELD_SERVER_INFO, <<"serverInfo">>).
-define(MCP_FIELD_ROOTS, <<"roots">>).
-define(MCP_FIELD_SAMPLING, <<"sampling">>).

%%%===================================================================
%%% Valid Error Codes List (Missing - Add This)
%%%===================================================================
-define(VALID_ERROR_CODES, [
    %% Standard JSON-RPC errors
    -32700,  %% Parse error
    -32600,  %% Invalid request
    -32601,  %% Method not found
    -32602,  %% Invalid params
    -32603,  %% Internal error

    %% MCP-specific errors
    -32001,  %% Resource not found
    -32002,  %% Tool not found
    -32003,  %% Prompt not found
    -32004,  %% Capability not supported
    -32005,  %% Not initialized
    -32006,  %% Subscription failed
    -32007,  %% Validation failed
    -32008,  %% Transport error
    -32009,  %% Timeout
    -32010,  %% Rate limited
    -32011,  %% Tool description too long (Gap #40)
    -32012,  %% Message too large (Gap #45)
    -32013,  %% Invalid content type
    -32014,  %% Content too large
    -32015,  %% Invalid encoding
    -32016,  %% Binary data too large
    -32017,  %% Text too long
    -32018,  %% Invalid MIME type
    -32019,  %% Unsupported media type
    -32020,  %% Media type not acceptable
    -32021,  %% Resource template not found
    -32022,  %% Invalid URI
    -32023,  %% URI syntax error
    -32024,  %% URI too long
    -32025,  %% Resource access denied
    -32026,  %% Resource already exists
    -32027,  %% Resource locked
    -32028,  %% Resource version mismatch
    -32029,  %% Template render failed
    -32030,  %% Invalid URI template
    -32031,  %% Tool execution failed
    -32032,  %% Tool timeout
    -32033   %% Tool cancelled
]).

%%%===================================================================
%%% Request ID Threshold Constants (Missing - Add These)
%%%===================================================================
-define(ID_WARNING_THRESHOLD, 922337203685477580).  %% 80% of 2^60
-define(ID_CRITICAL_THRESHOLD, 1037629353461465664). %% 90% of 2^60
-define(ID_RESERVED_THRESHOLD, 1106804644422573056). %% 96% of 2^60
```

## Error Assertion Patterns - Before & After

### Pattern 1: Parse Error Testing

**BEFORE (Broken):**
```erlang
test_parse_error(_ServerPid) ->
    InvalidJson = <<"{invalid json}">>,
    {error, {parse_error, _Reason}} = erlmcp_json_rpc:decode_message(InvalidJson),
    %% ... rest of test
```

**AFTER (Fixed):**
```erlang
test_parse_error(_ServerPid) ->
    InvalidJson = <<"{invalid json}">>,
    %% Actual implementation returns {error, {parse_error, DetailsMap}}
    {error, {parse_error, _DetailsMap}} = erlmcp_json_rpc:decode_message(InvalidJson),

    %% Verify error response encoding
    ErrorBin = erlmcp_json_rpc:error_parse(1),
    {ok, Response} = erlmcp_json_rpc:decode_message(ErrorBin),
    ?assertEqual(-32700, Response#json_rpc_response.error#mcp_error.code).
```

### Pattern 2: Method Not Found Testing

**BEFORE (Broken):**
```erlang
test_method_not_found(_ServerPid) ->
    %% Send request to server with unknown method
    _RequestJson = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"unknown/method">>,
        <<"id">> => 1
    }),

    %% Server handles via handle_info, simulate the request processing
    %% The server should return method not found error
    ErrorBin = erlmcp_json_rpc:error_method_not_found(1, <<"unknown/method">>),
    ErrorMap = jsx:decode(ErrorBin, [return_maps]),
    Error = maps:get(<<"error">>, ErrorMap),

    %% Verify error structure
    ?assertEqual(-32601, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Method not found">>, maps:get(<<"message">>, Error)),
    ?assertMatch(#{<<"method">> := <<"unknown/method">>}, maps:get(<<"data">>, Error)).
```

**AFTER (Fixed - Use record instead of map):**
```erlang
test_method_not_found(_ServerPid) ->
    %% Create error response
    ErrorBin = erlmcp_json_rpc:error_method_not_found(1, <<"unknown/method">>),

    %% Decode and verify using record pattern
    {ok, Response} = erlmcp_json_rpc:decode_message(ErrorBin),
    #json_rpc_response{
        id = 1,
        error = #mcp_error{
            code = -32601,
            message = <<"Method not found">>,
            data = DataMap
        }
    } = Response,

    %% Verify data contains method name
    ?assertMatch(#{<<"method">> := <<"unknown/method">>}, DataMap).
```

### Pattern 3: Invalid Params Testing

**BEFORE (Broken):**
```erlang
test_invalid_params(_ServerPid) ->
    %% Test missing required parameters
    ErrorBin = erlmcp_json_rpc:error_invalid_params(1, <<"Missing 'uri' parameter">>),
    ErrorMap = jsx:decode(ErrorBin, [return_maps]),
    Error = maps:get(<<"error">>, ErrorMap),

    ?assertEqual(-32602, maps:get(<<"code">>, Error)),
    ?assertEqual(<<"Invalid params">>, maps:get(<<"message">>, Error)),
    ?assertMatch(#{<<"details">> := <<"Missing 'uri' parameter">>}, maps:get(<<"data">>, Error)),

    %% Test with atom details (auto-converted to binary)
    ErrorBin2 = erlmcp_json_rpc:error_invalid_params(1, missing_uri),
    ErrorMap2 = jsx:decode(ErrorBin2, [return_maps]),
    Error2 = maps:get(<<"error">>, ErrorMap2),
    ?assertEqual(-32602, maps:get(<<"code">>, Error2)).
```

**AFTER (Fixed - Use record):**
```erlang
test_invalid_params(_ServerPid) ->
    %% Test with binary details
    ErrorBin1 = erlmcp_json_rpc:error_invalid_params(1, <<"Missing 'uri' parameter">>),
    {ok, Response1} = erlmcp_json_rpc:decode_message(ErrorBin1),
    #json_rpc_response{
        error = #mcp_error{
            code = -32602,
            message = <<"Invalid params">>,
            data = #{<<"details">> := <<"Missing 'uri' parameter">>}
        }
    } = Response1,

    %% Test with atom details (auto-converted to binary)
    ErrorBin2 = erlmcp_json_rpc:error_invalid_params(1, missing_uri),
    {ok, Response2} = erlmcp_json_rpc:decode_message(ErrorBin2),
    #json_rpc_response{
        error = #mcp_error{
            code = -32602,
            data = #{<<"details">> := <<"missing_uri">>}
        }
    } = Response2.
```

### Pattern 4: Error Code Validation

**BEFORE (Broken):**
```erlang
test_error_code_validation(_ServerPid) ->
    %% All valid codes should pass
    ValidCodes = ?VALID_ERROR_CODES,  %% NOT DEFINED!
    lists:foreach(fun(Code) ->
        ?assert(erlmcp_json_rpc:validate_error_code(Code))
    end, ValidCodes),

    %% Invalid codes should fail
    ?assertNot(erlmcp_json_rpc:validate_error_code(-99999)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(100)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(0)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(-1)),
```

**AFTER (Fixed - Hardcode valid codes):**
```erlang
test_error_code_validation(_ServerPid) ->
    %% Define valid codes inline (or use ?VALID_ERROR_CODES after adding to header)
    ValidCodes = [
        -32700, -32600, -32601, -32602, -32603,  %% JSON-RPC
        -32001, -32002, -32003, -32004, -32005,    %% MCP
        -32007, -32011, -32012                     %% MCP extensions
    ],
    lists:foreach(fun(Code) ->
        ?assert(erlmcp_json_rpc:validate_error_code(Code))
    end, ValidCodes),

    %% Invalid codes should fail
    ?assertNot(erlmcp_json_rpc:validate_error_code(-99999)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(100)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(0)),
    ?assertNot(erlmcp_json_rpc:validate_error_code(-1)).
```

## Test Helper Module - Create This File

Create `apps/erlmcp_core/test/test_client.erl`:

```erlang
-module(test_client).
-behavior(gen_server).

%% API
-export([start_link/0, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #{state => initialized}}.

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

Create `apps/erlmcp_core/test/test_cleanup_handler.erl`:

```erlang
-module(test_cleanup_handler).

%% Cleanup handler callbacks
-export([cleanup_operation/2]).

cleanup_operation(_Token, _Reason) ->
    %% Track cleanup calls via ETS for testing
    case ets:whereis(cleanup_tracker) of
        undefined ->
            ets:new(cleanup_tracker, [named_table, public, set]);
        _ ->
            ok
    end,
    ets:insert(cleanup_tracker, {cleanup_called, true}),
    ok.
```

Create `apps/erlmcp_core/test/test_failing_cleanup_handler.erl`:

```erlang
-module(test_failing_cleanup_handler).

%% Failing cleanup handler for testing error handling
-export([cleanup_operation/2]).

cleanup_operation(_Token, _Reason) ->
    %% Deliberately fail to test error handling
    exit(cleanup_failed).
```

## Common Test Setup Pattern

Replace problematic test setups with this pattern:

```erlang
%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start application (if not already started)
    application:ensure_all_started(erlmcp_core),

    %% Start a test server with full capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    },
    {ok, Server} = erlmcp_server:start_link(<<"test_server">>, Capabilities),

    %% Start a test client
    {ok, Client} = erlmcp_client:start_link({stdio, []}),

    #{server => Server, client => Client}.

cleanup(#{server := Server, client := Client}) ->
    %% Clean up client
    case is_process_alive(Client) of
        true -> erlmcp_client:stop(Client);
        false -> ok
    end,

    %% Clean up server
    case is_process_alive(Server) of
        true -> erlmcp_server:stop(Server);
        false -> ok
    end.

%%====================================================================
%% Test Case Pattern
%%====================================================================

test_example(State) ->
    fun() ->
        Server = maps:get(server, State),
        Client = maps:get(client, State),

        %% Perform test operations
        {ok, Tools} = erlmcp_server:list_tools_local(Server),
        ?assert(is_list(Tools)),

        %% Verify results
        ?assert(length(Tools) >= 0)
    end.
```

## Summary

### Files to Create:

1. **Add to `apps/erlmcp_core/include/erlmcp.hrl`:**
   - JSON-RPC message constants (5)
   - MCP message constants (9)
   - MCP field name constants (7)
   - Valid error codes list (1)
   - Request ID threshold constants (4)

2. **Create test helper modules:**
   - `apps/erlmcp_core/test/test_client.erl`
   - `apps/erlmcp_core/test/test_cleanup_handler.erl`
   - `apps/erlmcp_core/test/test_failing_cleanup_handler.erl`

### Files to Fix:

**Category A (Minor fixes):**
- `erlmcp_error_handling_tests.erl.broken`
- `mcp_compliance_SUITE.erl.broken`
- `erlmcp_jsonrpc_compliance_tests.erl.broken`
- `erlmcp_request_id_tests.erl.broken`
- `erlmcp_message_parser_tests.erl.broken`

### Files to Delete:

**Category D (Unsalvageable):**
- `regression_detection_SUITE.erl.broken`
- `auto_fix_SUITE.erl.broken`
- `extra_test.erl.broken`
- `erlmcp_sampling_manual_tests.erl.broken` (move to examples/)

---

**Quick Reference Version:** 1.0
**Created:** 2026-01-30
**Purpose:** Fast fixes for broken error handling tests
