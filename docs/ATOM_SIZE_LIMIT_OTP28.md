# OTP 28 Atom Size Limit - MCP Internationalization

## Overview

**OTP 28 Innovation**: Atom size limit changed from **255 bytes** to **255 characters**.

This change enables longer international atom names with multibyte UTF-8 characters, which is critical for MCP protocol internationalization.

### Key Changes

| OTP Version | Limit | Unit | Example |
|-------------|-------|------|---------|
| OTP < 28 | 255 | bytes | `"ツ"` = 3 bytes ✓ |
| OTP 28+ | 255 | characters | `"ツ"` = 1 char ✓ |

## Impact on MCP Protocol

### Use Cases

1. **Tool Names**: International tool names in Japanese, Arabic, emoji
2. **Resource URIs**: UTF-8 resource identifiers
3. **Metadata Keys**: International metadata keys
4. **Error Messages**: International error descriptions

### Examples

```erlang
% Japanese tool name (3 bytes per character)
<<"ツール名">>  % 12 bytes, 4 chars - Works in OTP 28!

% Arabic tool name (variable length UTF-8)
<<"الأداة">>  % 10 bytes, 5 chars

% Emoji in tool names
<<"🔧_tool_🚀">>  % 14 bytes, 8 chars

% Mixed languages
<<"tool_日本語_🔧_الأداة">>  % 30 bytes, 14 chars
```

## erlmcp_atoms API

### Core Functions

#### `binary_to_atom_safe/1,2`

Safely convert binary to atom with validation.

```erlang
% Safe conversion with byte size check (255 bytes)
erlmcp_atoms:binary_to_atom_safe(<<"tool_name">>).
% => 'tool_name'

% With encoding option
erlmcp_atoms:binary_to_atom_safe(<<"tool">>, utf8).
% => 'tool'

% Error cases
erlmcp_atoms:binary_to_atom_safe(<<>>).
% => {error, empty}

erlmcp_atoms:binary_to_atom_safe(<<0:256/unit:8>>).
% => {error, too_long}
```

#### `tool_name_to_atom/1`

OTP 28 aware tool name conversion (255 characters).

```erlang
% International tool names
erlmcp_atoms:tool_name_to_atom(<<"ツール名">>).
% => 'ツール名'

erlmcp_atoms:tool_name_to_atom(<<"🔧_tool_🚀">>).
% => '🔧_tool_🚀'

% Too long (256+ characters)
erlmcp_atoms:tool_name_to_atom(list_to_binary(lists:duplicate(256, $a))).
% => ** error: tool_name_too_long
```

#### `resource_name_to_atom/1`

Convert resource names/URIs to atoms.

```erlang
% Simple resource name
erlmcp_atoms:resource_name_to_atom(<<"my_resource">>).
% => {ok, 'my_resource'}

% Resource URI
erlmcp_atoms:resource_name_to_atom(<<"file:///path/to/resource">>).
% => {ok, 'file:///path/to/resource'}
```

#### `namespaced_atom/2`

Create namespaced atoms with "$" separator.

```erlang
% Create namespaced atom
erlmcp_atoms:namespaced_atom(<<"mcp">>, <<"tool">>).
% => 'mcp$tool'

% International namespace
erlmcp_atoms:namespaced_atom(<<"mcp">>, <<"ツール">>).
% => 'mcp$ツール'
```

#### `existing_atom/1`

Look up existing atom (prevents atom leak).

```erlang
% Atom exists
erlmcp_atoms:existing_atom(<<"existing_tool">>).
% => 'existing_tool'

% Atom doesn't exist
erlmcp_atoms:existing_atom(<<"nonexistent">>).
% => {error, not_found}
```

#### `char_length_check/1`

Check character length (OTP 28).

```erlang
% Within limit
erlmcp_atoms:char_length_check(<<"test">>).
% => ok

% Empty
erlmcp_atoms:char_length_check(<<>>).
% => {error, empty}

% Too long (256+ characters)
erlmcp_atoms:char_length_check(list_to_binary(lists:duplicate(256, $a))).
% => {error, too_long}
```

#### `is_safe_atom/1`

Check if atom is safe (not reserved).

```erlang
% Reserved atoms (unsafe)
erlmcp_atoms:is_safe_atom(undefined).
% => false

erlmcp_atoms:is_safe_atom(true).
% => false

% User atoms (safe)
erlmcp_atoms:is_safe_atom('my_tool').
% => true

erlmcp_atoms:is_safe_atom('ツール').
% => true
```

#### `validate_binary/1`

Validate binary for atom conversion.

```erlang
% Valid
erlmcp_atoms:validate_binary(<<"test">>).
% => ok

% Empty
erlmcp_atoms:validate_binary(<<>>).
% => {error, empty}

% Too long
erlmcp_atoms:validate_binary(<<0:256/unit:8>>).
% => {error, too_long}

% Invalid UTF-8
erlmcp_atoms:validate_binary(<<255, 254>>).
% => {error, invalid_utf8}
```

#### `get_atom_limit/0`

Get current atom limit.

```erlang
% OTP 28+
erlmcp_atoms:get_atom_limit().
% => 255 (characters)

% OTP < 28
erlmcp_atoms:get_atom_limit().
% => 255 (bytes)
```

## Usage Patterns

### Pattern 1: Tool Registration

```erlang
% Register tool with international name
register_tool(<<"ツール名">>, Config) ->
    ToolAtom = erlmcp_atoms:tool_name_to_atom(<<"ツール名">>),
    % Use ToolAtom for registration
    ets:insert(tool_table, {ToolAtom, Config}).
```

### Pattern 2: Resource URIs

```erlang
% Convert resource URI to atom
ResourceAtom = erlmcp_atoms:resource_name_to_atom(URI),
case ResourceAtom of
    {ok, Atom} -> ets:insert(resources, {Atom, Config});
    {error, Reason} -> logger:error("Invalid resource URI: ~p", [Reason])
end.
```

### Pattern 3: Namespace Prefix

```erlang
% Create namespaced atom
Namespaced = erlmcp_atoms:namespaced_atom(<<"mcp">>, ToolName),
case Namespaced of
    Atom when is_atom(Atom) -> ets:insert(tools, {Atom, Config});
    {error, Reason} -> logger:error("Invalid tool name: ~p", [Reason])
end.
```

### Pattern 4: Safe Conversion with Error Handling

```erlang
% Safe conversion with validation
case erlmcp_atoms:binary_to_atom_safe(UserInput) of
    Atom when is_atom(Atom) ->
        {ok, Atom};
    {error, too_long} ->
        {error, "Tool name exceeds 255 characters"};
    {error, invalid_utf8} ->
        {error, "Tool name contains invalid UTF-8"};
    {error, empty} ->
        {error, "Tool name cannot be empty"}
end.
```

## Migration Guide

### From OTP 27 to OTP 28

#### Before (OTP 27)

```erlang
% Byte-based limit (unsafe for multibyte UTF-8)
binary_to_atom(<<"ツール">>, utf8).
% 3 chars = 9 bytes ✓

% Problem: Long Japanese names fail
binary_to_atom(<<"ツール名前確認_長い名前">>, utf8).
% May exceed 255 bytes even if < 255 chars!
```

#### After (OTP 28)

```erlang
% Character-based limit (safe for multibyte UTF-8)
erlmcp_atoms:tool_name_to_atom(<<"ツール名前確認_長い名前">>).
% 14 chars < 255 ✓

% Still works with ASCII
erlmcp_atoms:tool_name_to_atom(<<"tool">>).
% => 'tool'
```

### Backward Compatibility

The API is backward compatible with OTP < 28:

```erlang
% Works on both OTP 27 and OTP 28
case erlmcp_atoms:binary_to_atom_safe(Bin) of
    Atom when is_atom(Atom) -> ok;
    {error, too_long} -> error
end.

% On OTP 27: Uses byte_size (255 bytes)
% On OTP 28: Uses string:length (255 characters)
```

## Safety Best Practices

### 1. Always Use Safe Conversion

```erlang
% ✅ GOOD: Safe conversion
case erlmcp_atoms:binary_to_atom_safe(UserInput) of
    Atom when is_atom(Atom) -> ok;
    {error, Reason} -> handle_error(Reason)
end.

% ❌ BAD: Unsafe conversion
Atom = binary_to_atom(UserInput, utf8).
% May crash or cause atom leak!
```

### 2. Validate Before Conversion

```erlang
% ✅ GOOD: Validate first
case erlmcp_atoms:validate_binary(Bin) of
    ok -> erlmcp_atoms:binary_to_atom_safe(Bin);
    {error, Reason} -> {error, Reason}
end.
```

### 3. Use Existing Atom Lookup

```erlang
% ✅ GOOD: Reuse existing atoms
case erlmcp_atoms:existing_atom(Bin) of
    Atom when is_atom(Atom) -> ok;
    {error, not_found} -> create_new_atom(Bin)
end.

% ❌ BAD: Always creates new atoms
binary_to_atom(Bin, utf8).  % Atom leak!
```

### 4. Check for Reserved Atoms

```erlang
% ✅ GOOD: Check safety
Atom = erlmcp_atoms:tool_name_to_atom(Bin),
case erlmcp_atoms:is_safe_atom(Atom) of
    true -> ok;
    false -> {error, reserved_atom}
end.
```

## Testing Strategies

### Unit Tests

```erlang
% Test international characters
japanese_test() ->
    Atom = erlmcp_atoms:tool_name_to_atom(<<"ツール名">>),
    ?assert(is_atom(Atom)),
    ?assert(erlmcp_atoms:is_safe_atom(Atom)).

arabic_test() ->
    Atom = erlmcp_atoms:tool_name_to_atom(<<"الأداة">>),
    ?assert(is_atom(Atom)).

emoji_test() ->
    Atom = erlmcp_atoms:tool_name_to_atom(<<"🔧_tool_🚀">>),
    ?assert(is_atom(Atom)).
```

### Property Tests

```erlang
% PropEr: Character length invariant
prop_char_length_ok() ->
    ?FORALL(Bin, unicode_binary(),
        begin
            case erlmcp_atoms:char_length_check(Bin) of
                ok -> true;
                {error, too_long} -> string:length(Bin) > 255;
                _ -> true
            end
        end).
```

### Integration Tests

```erlang
% Test tool registration with international names
register_international_tool_test() ->
    ToolName = <<"ツール名">>,
    {ok, _} = erlmcp_server:add_tool(ToolName, Config),
    {ok, Tool} = erlmcp_server:get_tool(ToolName),
    ?assertEqual(ToolName, maps:get(<<"name">>, Tool)).
```

## Performance Considerations

### Character Length Calculation

```erlang
% OTP 28: Uses string:length/1 (Unicode-aware)
% Cost: O(n) where n = number of characters
Chars = string:length(Binary).

% Optimization: Cache frequently used names
% Use ETS table for atom lookup
```

### Atom Leak Prevention

```erlang
% Always try existing atom first
try
    binary_to_existing_atom(Binary, utf8)
catch
    error:badarg ->
        % Create only if doesn't exist
        binary_to_atom(Binary, utf8)
end.
```

## Troubleshooting

### Common Errors

#### `tool_name_too_long`

```erlang
% Cause: Tool name exceeds 255 characters
% Solution: Shorten name or use abbreviation
```

#### `{error, invalid_utf8}`

```erlang
% Cause: Invalid UTF-8 sequence
% Solution: Validate input before conversion
case unicode:characters_to_binary(Bin, utf8, utf8) of
    {ok, Valid} -> erlmcp_atoms:binary_to_atom_safe(Valid);
    {error, _, _} -> {error, invalid_utf8}
end.
```

#### `system_limit` (Atom Table Full)

```erlang
% Cause: Too many atoms (typically 1M limit)
% Solution: Use binary references instead of atoms
% or implement atom garbage collection
```

## Examples

### Example 1: International Tool Registry

```erlang
-module(international_tool_registry).
-export([register_tool/2, lookup_tool/1]).

register_tool(Name, Config) when is_binary(Name) ->
    % Convert to atom (OTP 28 aware)
    Atom = erlmcp_atoms:tool_name_to_atom(Name),
    ets:insert(tools, {Atom, Config}),
    {ok, Atom}.

lookup_tool(Name) when is_binary(Name) ->
    % Look up existing atom
    case erlmcp_atoms:existing_atom(Name) of
        Atom when is_atom(Atom) ->
            case ets:lookup(tools, Atom) of
                [{Atom, Config}] -> {ok, Config};
                [] -> {error, not_found}
            end;
        {error, not_found} ->
            {error, not_found}
    end.
```

### Example 2: Resource URI Validation

```erlang
-module(resource_validator).
-export([validate_uri/1]).

validate_uri(URI) when is_binary(URI) ->
    % Check character length
    case erlmcp_atoms:char_length_check(URI) of
        ok ->
            % Check for valid URI characters
            case validate_uri_chars(URI) of
                ok -> {ok, URI};
                {error, Reason} -> {error, Reason}
            end;
        {error, too_long} ->
            {error, uri_too_long}
    end.

validate_uri_chars(URI) ->
    % Check for valid URI characters (ASCII + reserved)
    case re:run(URI, <<"^[a-zA-Z0-9\\-._~:/?#[\\]@!$&'()*+,;=]+$">>) of
        {match, _} -> ok;
        nomatch -> {error, invalid_uri_chars}
    end.
```

## References

- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principles/versions.html)
- [Erlang Efficiency Guide: Atoms](https://www.erlang.org/doc/efficiency_guide/advanced.html)
- [MCP Specification: Internationalization](https://modelcontextprotocol.io/)
- [RFC 3629: UTF-8](https://datatracker.ietf.org/doc/html/rfc3629)

## See Also

- `erlmcp_registry.erl` - Tool and resource registration
- `erlmcp_json_rpc.erl` - JSON-RPC protocol with UTF-8 support
- `erlmcp_mcp_types.erl` - Nominal types for type safety
