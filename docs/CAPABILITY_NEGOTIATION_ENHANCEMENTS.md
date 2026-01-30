# Capability Negotiation Enhancements - Task #209

## Summary

Enhanced the capability negotiation module (`erlmcp_capabilities.erl`) with comprehensive feature flag handling, graceful degradation, and improved client capability extraction.

## Changes Made

### 1. Enhanced Client Capability Extraction

**New Functions:**
- `extract_roots_client_capability/1` - Properly extracts roots capability with feature flag detection
- `extract_sampling_client_capability/1` - Extracts sampling capability with model preferences

**Improvements:**
- Better handling of capability maps with size checking
- Proper detection of enabled/disabled states
- Support for feature flags in client capabilities

### 2. Graceful Degradation

**New Functions:**
- `negotiate_experimental/2` - Negotiates experimental features using intersection
- `apply_graceful_degradation/2` - Applies degradation based on client capabilities

**Behavior:**
- Disables `listChanged` feature on resources when client doesn't support roots
- Returns intersection of experimental features
- Maintains backward compatibility with older clients

### 3. Feature Flag Management

**New Functions:**
- `is_feature_enabled/3` - Check if a feature flag is enabled
- `disable_feature/3` - Disable a specific feature flag
- `enable_feature/3` - Enable a specific feature flag
- `get_enabled_features/2` - Get list of all enabled features

**Supported Features:**
- `resources:subscribe` - Resource subscription support
- `resources:listChanged` - Resource list change notifications
- `tools:listChanged` - Tool list change notifications
- `prompts:listChanged` - Prompt list change notifications
- `roots:listChanged` - Roots list change notifications

### 4. Improved Capability Negotiation

**Enhanced `negotiate_capability/3`:**
- Checks client roots capability before enabling resources features
- Disables `listChanged` when client doesn't support roots
- Properly handles sampling capability with client preferences
- Returns empty roots capability when client doesn't support it

**Example:**
```erlang
%% Client without roots support
ClientCaps = #mcp_client_capabilities{
    roots = #mcp_capability{enabled = false},
    sampling = #mcp_capability{enabled = false}
},

%% Server with full capabilities
ServerCaps = #mcp_server_capabilities{
    resources = #mcp_resources_capability{
        subscribe = true,
        listChanged = true
    },
    ...
},

%% Negotiated result disables listChanged
Negotiated = erlmcp_capabilities:negotiate_capabilities(ClientCaps, ServerCaps),
%% Result: resources#mcp_resources_capability{subscribe = true, listChanged = false}
```

### 5. Enhanced Error Formatting

**New Error Type:**
- `{feature_flag_disabled, Capability, Feature}` - Indicates a feature is disabled

**Example:**
```erlang
erlmcp_capabilities:format_capability_error(
    {feature_flag_disabled, resources, listChanged}
).
%% Returns: <<"Feature 'listChanged' is disabled for capability 'resources'">>
```

## API Additions

### Exported Functions

```erlang
%% Feature flag management
is_feature_enabled(Caps, Capability, Feature) -> boolean()
disable_feature(Caps, Capability, Feature) -> Caps
enable_feature(Caps, Capability, Feature) -> Caps
get_enabled_features(Caps, Capability) -> [Feature]

%% Graceful degradation
apply_graceful_degradation(ClientCaps, ServerCaps) -> ServerCaps
negotiate_experimental(ClientCaps, ServerCaps) -> ExperimentalMap | undefined

%% Enhanced extraction
extract_roots_client_capability(CapsMap) -> #mcp_capability{}
extract_sampling_client_capability(CapsMap) -> #mcp_capability{}
```

## Usage Examples

### Check Feature Support

```erlang
%% Check if subscribe is enabled for resources
case erlmcp_capabilities:is_feature_enabled(Caps, resources, subscribe) of
    true -> enable_resource_subscriptions();
    false -> log_no_subscribe_support()
end.
```

### Get All Enabled Features

```erlang
%% Get all enabled features for resources
Features = erlmcp_capabilities:get_enabled_features(Caps, resources),
%% Returns: [subscribe, listChanged] or [] or [subscribe] etc.
```

### Disable Feature for Client

```erlang
%% Disable listChanged if client doesn't support it
Caps1 = erlmcp_capabilities:disable_feature(Caps, resources, listChanged).
```

### Apply Graceful Degradation

```erlang
%% Automatic degradation during negotiation
NegotiatedCaps = erlmcp_capabilities:negotiate_capabilities(
    ClientCaps,
    ServerCaps
).
%% Automatically applies apply_graceful_degradation/2 internally
```

## Compatibility

### Backward Compatibility
- All existing functions maintained
- Default behavior unchanged for basic capability negotiation
- Feature flags default to `false` when not specified

### MCP Protocol Versions
- Supports `2024-11-05` and `2025-11-25`
- Graceful degradation for clients with partial feature support

## Testing Recommendations

While tests were not written as part of this task, here are key test scenarios:

1. **Feature Flag Extraction**
   - Client with roots capability
   - Client without roots capability
   - Client with sampling capability

2. **Capability Negotiation**
   - Full feature support on both sides
   - Client without roots support
   - Client with partial feature support

3. **Graceful Degradation**
   - Disable listChanged when roots not supported
   - Experimental feature intersection
   - Sampling preference merging

4. **Feature Management**
   - Enable/disable individual features
   - Query enabled features
   - Check feature support

## Files Modified

- `/Users/sac/erlmcp/apps/erlmcp_core/src/erlmcp_capabilities.erl` - Enhanced with new features

## Related Tasks

- Task #144: Implement capability negotiation
- Task #152: Implement capability negotiation for erlmcp
- Task #209: Complete capability negotiation (this task)

## Notes

- No breaking changes to existing API
- All new functions are additive
- Compilation successful for the capabilities module
- Ready for integration and testing
