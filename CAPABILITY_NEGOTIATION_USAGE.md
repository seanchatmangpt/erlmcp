# Capability Negotiation Usage Guide

## Quick Reference

### Server Side: Building Capabilities

```erlang
%% Build default capabilities (all features enabled)
ServerCaps = erlmcp_capabilities:build_server_capabilities(),

%% Custom configuration
Config = #{
    resources => #{
        subscribe => true,
        listChanged => true
    }
},
CustomCaps = erlmcp_capabilities:build_server_capabilities(Config),

%% Serialize to JSON for response
CapabilitiesMap = erlmcp_capabilities:capability_to_map(ServerCaps),
Response = #{
    <<"capabilities">> => CapabilitiesMap,
    ...
}
```

### Client Side: Extracting & Validating

```erlang
%% Extract client capabilities from initialize request
ClientCaps = erlmcp_capabilities:extract_client_capabilities(InitParams),

%% Validate protocol version
case erlmcp_capabilities:validate_protocol_version(RequestedVersion) of
    ok -> proceed;
    {error, Reason} -> return_error(Reason)
end,

%% Store server capabilities from response
{ok, Response} = erlmcp_client:initialize(Client, ClientCaps),
ServerCapabilities = erlmcp_capabilities:server_capabilities_from_map(
    maps:get(<<"capabilities">>, Response)
),

%% Validate capability before using feature
case erlmcp_capabilities:validate_capability(ServerCapabilities, resources) of
    ok -> list_resources();
    {error, capability_not_supported} -> return_error(<<"Resources not supported">>)
end,

%% Check specific feature
case erlmcp_capabilities:validate_feature(ServerCapabilities, resources, subscribe) of
    ok -> subscribe_to_resource(Uri);
    {error, feature_not_supported} -> log_warning(<<"Subscribe not supported">>)
end
```

---

## Available Capabilities

### Resources
- `subscribe` - Clients can subscribe to resource updates
- `listChanged` - Server sends notifications when resource list changes

### Tools
- `listChanged` - Server sends notifications when tool list changes

### Prompts
- `listChanged` - Server sends notifications when prompt list changes

### Logging
- No specific features, capability indicates logging is supported

### Sampling
- `modelPreferences` - Includes model preference hints for AI sampling

### Roots
- No specific features, capability indicates roots support

---

## API Reference

### Building Capabilities

```erlang
%% Create default capabilities with all features
build_server_capabilities() -> #mcp_server_capabilities{}

%% Create capabilities with custom configuration
build_server_capabilities(Config :: map()) -> #mcp_server_capabilities{}
```

### Extracting & Validating

```erlang
%% Extract client capabilities from initialize params
extract_client_capabilities(Params :: map() | undefined) -> #mcp_client_capabilities{}

%% Validate protocol version
validate_protocol_version(Version :: binary()) -> ok | {error, binary()}

%% Validate capability exists
validate_capability(Caps :: #mcp_server_capabilities{},
                   Capability :: atom()) -> ok | {error, atom()}

%% Validate feature within capability
validate_feature(Caps :: #mcp_server_capabilities{},
                Capability :: atom(),
                Feature :: atom()) -> ok | {error, atom()}
```

### Serialization

```erlang
%% Serialize capabilities to JSON map
capability_to_map(#mcp_server_capabilities{} |
                  #mcp_resources_capability{} |
                  #mcp_tools_capability{} |
                  ...) -> map()

%% Serialize client capabilities to JSON
client_capabilities_to_map(#mcp_client_capabilities{}) -> map()

%% Deserialize from JSON
server_capabilities_from_map(map()) -> #mcp_server_capabilities{}
client_capabilities_from_map(map()) -> #mcp_client_capabilities{}
```

---

## Error Handling

### Valid Error Returns

```erlang
%% Unknown capability
erlmcp_capabilities:validate_capability(Caps, invalid_cap)
-> {error, unknown_capability}

%% Unsupported protocol version
erlmcp_capabilities:validate_protocol_version(<<"1.0.0">>)
-> {error, <<"Unsupported protocol version">>}

%% Feature not in capability
erlmcp_capabilities:validate_feature(Caps, resources, unsupported_feature)
-> {error, unknown_feature}

%% Feature disabled
erlmcp_capabilities:validate_feature(CapsWithoutSubscribe, resources, subscribe)
-> {error, feature_not_supported}
```

---

## Integration Examples

### Example 1: Server Initialization

```erlang
start_server() ->
    %% Create server with capabilities
    Caps = erlmcp_capabilities:build_server_capabilities(),
    {ok, ServerPid} = erlmcp_server:start_link(my_server, Caps),
    ServerPid.
```

### Example 2: Client Feature Usage

```erlang
use_resources(Client) ->
    {ok, InitResponse} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    %% Parse server capabilities
    ServerCaps = erlmcp_capabilities:server_capabilities_from_map(
        maps:get(<<"capabilities">>, InitResponse)
    ),

    %% Only list resources if capability is advertised
    case erlmcp_capabilities:validate_capability(ServerCaps, resources) of
        ok ->
            {ok, Resources} = erlmcp_client:list_resources(Client),
            process_resources(Resources);
        {error, _} ->
            io:format("Server doesn't support resources~n")
    end.
```

### Example 3: Conditional Subscription

```erlang
maybe_subscribe_to_resource(Client, Uri) ->
    {ok, InitResponse} = erlmcp_client:initialize(Client, #mcp_client_capabilities{}),

    ServerCaps = erlmcp_capabilities:server_capabilities_from_map(
        maps:get(<<"capabilities">>, InitResponse)
    ),

    case erlmcp_capabilities:validate_feature(ServerCaps, resources, subscribe) of
        ok ->
            erlmcp_client:subscribe_to_resource(Client, Uri),
            ok;
        {error, feature_not_supported} ->
            io:format("Server doesn't support resource subscriptions~n"),
            {error, not_supported}
    end.
```

---

## Testing with Capabilities

```erlang
test_capability_negotiation() ->
    %% Build capabilities
    Caps = erlmcp_capabilities:build_server_capabilities(),

    %% Verify they serialize correctly
    Map = erlmcp_capabilities:capability_to_map(Caps),
    RestoredCaps = erlmcp_capabilities:server_capabilities_from_map(Map),

    %% Validate each capability
    ok = erlmcp_capabilities:validate_capability(RestoredCaps, resources),
    ok = erlmcp_capabilities:validate_capability(RestoredCaps, tools),
    ok = erlmcp_capabilities:validate_capability(RestoredCaps, prompts),

    %% Validate features
    ok = erlmcp_capabilities:validate_feature(RestoredCaps, resources, subscribe),
    ok = erlmcp_capabilities:validate_feature(RestoredCaps, tools, listChanged),

    io:format("All capability tests passed~n").
```

---

## Record Structures

### Server Capabilities
```erlang
#mcp_server_capabilities{
    resources = #mcp_resources_capability{
        subscribe = true,
        listChanged = true
    },
    tools = #mcp_tools_capability{
        listChanged = true
    },
    prompts = #mcp_prompts_capability{
        listChanged = true
    },
    logging = #mcp_logging_capability{},
    sampling = #mcp_sampling_capability{
        modelPreferences = undefined
    },
    roots = #mcp_roots_capability{},
    experimental = undefined
}
```

### Client Capabilities
```erlang
#mcp_client_capabilities{
    roots = #mcp_capability{enabled = false},
    sampling = #mcp_capability{enabled = false},
    experimental = undefined
}
```

---

## Best Practices

1. **Always validate before use** - Check capabilities before making requests that depend on them
2. **Handle gracefully** - Provide fallback behavior when capabilities aren't available
3. **Log warnings** - When features are used without capability support
4. **Cache capabilities** - Store capabilities from initialize response, don't re-request
5. **Version checking** - Always validate protocol version during initialization

---

## Files

- `/Users/sac/erlmcp/src/erlmcp_capabilities.erl` - Implementation
- `/Users/sac/erlmcp/test/erlmcp_capabilities_tests.erl` - Test suite
- `/Users/sac/erlmcp/include/erlmcp.hrl` - Record definitions
