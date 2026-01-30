# API Migration Guide for Broken Tests

**Purpose:** Quick reference for updating broken tests to use current APIs
**Updated:** 2026-01-30

## JSON-RPC Module API Changes

### Core Encoding Functions

#### encode_request (NO CHANGE)
```erlang
%% API:
erlmcp_json_rpc:encode_request(Id, Method, Params) -> binary()

%% Usage (unchanged):
Encoded = erlmcp_json_rpc:encode_request(
    1,
    <<"initialize">>,
    #{version => <<"1.0">>}
)
```

#### encode_response (NO CHANGE)
```erlang
%% API:
erlmcp_json_rpc:encode_response(Id, Result) -> binary()

%% Usage (unchanged):
Encoded = erlmcp_json_rpc:encode_response(1, #{status => ok})
```

#### encode_error → encode_error_response (RENAMED)
```erlang
%% OLD API (broken tests):
erlmcp_json_rpc:encode_error(Id, Code, Message, Data)

%% NEW API (current):
erlmcp_json_rpc:encode_error_response(Id, Code, Message, Data)
erlmcp_json_rpc:encode_error_response(Id, Code, Message)  %% Data = undefined

%% Migration:
%% Replace all: encode_error → encode_error_response
%% Example:
Old: erlmcp_json_rpc:encode_error(Id, Code, Msg, Data)
New: erlmcp_json_rpc:encode_error_response(Id, Code, Msg, Data)
```

#### encode_notification (NO CHANGE)
```erlang
%% API:
erlmcp_json_rpc:encode_notification(Method, Params) -> binary()

%% Usage (unchanged):
Encoded = erlmcp_json_rpc:encode_notification(
    <<"notifications/message">>,
    #{level => info, data => <<"hello">>}
)
```

### Core Decoding Functions

#### decode → decode_message (RENAMED)
```erlang
%% OLD API (broken tests):
erlmcp_json_rpc:decode(Binary) -> {ok, Message} | {error, Reason}

%% NEW API (current):
erlmcp_json_rpc:decode_message(Binary) -> {ok, Message} | {error, Reason}
erlmcp_json_rpc:decode_message(Binary, TransportType) -> {ok, Message} | {error, Reason}

%% Migration:
%% Replace all: decode( → decode_message(
%% Example:
Old: {ok, Msg} = erlmcp_json_rpc:decode(Json)
New: {ok, Msg} = erlmcp_json_rpc:decode_message(Json)
```

#### decode_batch (NO CHANGE)
```erlang
%% API:
erlmcp_json_rpc:decode_batch(Binary) -> {ok, [Message]} | {error, Reason}

%% Usage (unchanged):
{ok, Requests} = erlmcp_json_rpc:decode_batch(JsonArray)
```

### Error Record Changes

#### Error Objects (MAPS, not RECORDS)
```erlang
%% OLD PATTERN (broken tests):
#error_record{code = Code, message = Message, data = Data}

%% NEW PATTERN (current):
#{
    error => #{
        code => Code,
        message => Message,
        data => Data  %% Optional
    }
}

%% OR:
%% Decoded error responses now contain:
%% #mcp_error{} records (internal)
%% OR maps (public API)

%% Migration Pattern:
Old: case Error of
        #mcp_error{code = C, message = M, data = D} -> ...
    end

New: case Error of
        #{code := C, message := M, data := D} -> ...;
        #{code := C, message := M} -> ...  %% data optional
    end
```

### Batch Encoding

#### encode_batch (NO CHANGE)
```erlang
%% API:
erlmcp_json_rpc:encode_batch([Message]) -> binary()

%% Usage (unchanged):
Batch = [
    #json_rpc_request{id = 1, method = <<"foo">>, params = #{}},
    #json_rpc_request{id = 2, method = <<"bar">>, params = #{}}
],
Encoded = erlmcp_json_rpc:encode_batch(Batch)
```

### Convenience Functions

#### Error Creation Helpers (NEW)
```erlang
%% Pre-defined error creators (add to current tests):
erlmcp_json_rpc:error_method_not_found(Id, Data)
erlmcp_json_rpc:error_invalid_params(Id, Data)
erlmcp_json_rpc:error_resource_not_found(Id, Data)
erlmcp_json_rpc:error_tool_not_found(Id, Data)
erlmcp_json_rpc:error_prompt_not_found(Id, Data)
erlmcp_json_rpc:error_capability_not_supported(Id, Data)
erlmcp_json_rpc:error_not_initialized(Id)
erlmcp_json_rpc:error_validation_failed(Id, Data)
erlmcp_json_rpc:error_message_too_large(Id, Data)
erlmcp_json_rpc:error_internal(Id)
erlmcp_json_rpc:error_parse(Id)
```

---

## Transport Module API Changes

### TCP Transport (ranch-based)

#### Old Pattern (custom gen_server)
```erlang
%% OLD:
{ok, Pid} = erlmcp_transport_tcp:start_link(Port, Options)
%% Direct gen_server calls
```

#### New Pattern (ranch + pool_manager)
```erlang
%% NEW:
{ok, Pid} = erlmcp_transport_tcp:start_link(#{port => Port, ...})
%% Ranch handles connections
%% Pool manager handles connection pooling
```

### Connection Lifecycle

#### Starting Transport
```erlang
%% Current API:
Config = #{
    port => 0,  %% Random port for testing
    transport_opts => #{},
    pool_config => #{
        size => 10,
        max_overflow => 20
    }
},
{ok, Pid} = erlmcp_transport_tcp:start_link(Config).

%% Get actual port:
{ok, Port} = erlmcp_transport_tcp:get_port(Pid).
```

#### Creating Connections
```erlang
%% Use pool manager:
{ok, Conn} = erlmcp_pool_manager:checkout(PoolName),
%% Use connection...
ok = erlmcp_pool_manager:checkin(PoolName, Conn).
```

---

## Request ID Changes

### Old Pattern (erlmcp_request_id module)
```erlang
%% OLD (deleted):
NextId = erlmcp_request_id:next_id(CurrentId)
%% Custom overflow handling
```

### New Pattern (integrated into client)
```erlang
%% NEW (integrated):
%% Client handles request IDs internally
%% No separate module needed

%% Testing approach:
%% Test correlation, not IDs
```

---

## Test Pattern Changes

### Chicago School TDD Patterns

#### OLD (London School - MOCKS)
```erlang
%% BAD: Mocking collaborators
meck:new(erlmcp_json_rpc),
meck:expect(erlmcp_json_rpc, encode_request, fun(_, _, _) -> <<"fake">> end),
%% ... test ...
meck:unload(erlmcp_json_rpc)
```

#### NEW (Chicago School - REAL PROCESSES)
```erlang
%% GOOD: Real processes
{ok, ServerPid} = erlmcp_server:start_link(Config),
{ok, ClientPid} = erlmcp_client:start_link(ClientConfig),

%% Real interaction
{ok, Response} = erlmcp_client:call_tool(ClientPid, <<"tool_name">>, #{}),

%% State-based verification
?assertMatch(#{result := _}, Response),

%% Teardown
gen_server:stop(ClientPid),
gen_server:stop(ServerPid).
```

### Setup/Teardown Patterns

#### Standard Fixture
```erlang
module_test_() ->
    {foreach,
     fun setup/0,      %% Setup for each test
     fun cleanup/1,    %% Cleanup after each test
     [
        fun test_case_1/1,
        fun test_case_2/1
     ]}.

setup() ->
    {ok, Pid} = module:start_link(test_config),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid),
    timer:sleep(50).  %% Let cleanup complete
```

#### Shared Setup (once per suite)
```erlang
module_test_() ->
    {setup,
     fun setup_all/0,      %% Setup once for all tests
     fun cleanup_all/1,    %% Cleanup after all tests
     [
        fun test_case_1/0,
        fun test_case_2/0
     ]}.

setup_all() ->
    application:ensure_all_started(erlmcp),
    Config = #{}.

cleanup_all(_Config) ->
    application:stop(erlmcp).
```

---

## Common Migration Patterns

### Pattern 1: Function Renames
```erlang
%% Find and replace:
encode_error → encode_error_response
decode(       → decode_message(
decode(Json,  → decode_message(Json,

%% Use rebar3 format to verify after changes
```

### Pattern 2: Error Record to Map
```erlang
%% OLD:
#error{code = C, message = M}

%% NEW:
#{code := C, message := M}

%% Search pattern:
#error{ → #{
```

### Pattern 3: Mock to Real Process
```erlang
%% OLD:
meck:new(Module),
meck:expect(Module, func, fun(...) -> ... end),

%% NEW:
{ok, Pid} = Module:start_link(Config),
%% Use real API
Result = Module:func(Pid, Args),
gen_server:stop(Pid),
```

---

## Verification Checklist

After migrating tests, verify:

- [ ] All function calls use current API names
- [ ] Error records use map pattern matching
- [ ] No mock objects (meck, etc.) - use real processes
- [ ] Tests verify observable state, not internal calls
- [ ] Setup/teardown properly clean up processes
- [ ] Tests compile without warnings
- [ ] All tests pass: `rebar3 eunit --module=module_tests`
- [ ] Coverage check: `rebar3 cover --verbose`

---

## Quick Reference Commands

```bash
# Update all encode_error calls:
find apps/ -name "*.erl" -exec sed -i.bak 's/encode_error/encode_error_response/g' {} \;

# Update all decode calls (careful - may have other decode functions):
find apps/ -name "*_tests.erl" -exec sed -i.bak 's/erlmcp_json_rpc:decode(/erlmcp_json_rpc:decode_message(/g' {} \;

# Format all files:
rebar3 format

# Run specific test:
rebar3 eunit --module=erlmcp_json_rpc_tests

# Run with coverage:
rebar3 cover --verbose
```

---

## Migration Example: erlmcp_json_rpc_proper_tests

### Before (broken):
```erlang
prop_encode_decode_error_response_roundtrip() ->
    ?FORALL(Response, error_response(),
        begin
            ErrorMap = maps:get(error, Response),
            Encoded = erlmcp_json_rpc:encode_error(  %% WRONG
                maps:get(id, Response),
                maps:get(code, ErrorMap),
                maps:get(message, ErrorMap),
                maps:get(data, ErrorMap, undefined)
            ),
            case erlmcp_json_rpc:decode(Encoded) of  %% WRONG
                {ok, Decoded} ->
                    case maps:get(error, Decoded) of
                        #mcp_error{code = Code, ...} ->  %% WRONG
                            Code =:= maps:get(code, ErrorMap);
                        _ -> false
                    end;
                _ -> false
            end
        end).
```

### After (fixed):
```erlang
prop_encode_decode_error_response_roundtrip() ->
    ?FORALL(Response, error_response(),
        begin
            ErrorMap = maps:get(error, Response),
            Encoded = erlmcp_json_rpc:encode_error_response(  %% FIXED
                maps:get(id, Response),
                maps:get(code, ErrorMap),
                maps:get(message, ErrorMap),
                maps:get(data, ErrorMap, undefined)
            ),
            case erlmcp_json_rpc:decode_message(Encoded) of  %% FIXED
                {ok, Decoded} ->
                    case maps:get(error, Decoded) of
                        #{code := Code, message := Message, data := Data} ->  %% FIXED
                            Code =:= maps:get(code, ErrorMap) andalso
                            Message =:= maps:get(message, ErrorMap) andalso
                            Data =:= maps:get(data, ErrorMap, undefined);
                        _ -> false
                    end;
                _ -> false
            end
        end).
```

---

**Last Updated:** 2026-01-30
**For:** erlmcp v0.6.0
**Pattern:** Chicago School TDD
