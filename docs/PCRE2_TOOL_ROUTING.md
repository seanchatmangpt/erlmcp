# OTP 28 PCRE2 Tool Routing

## Overview

erlmcp uses **PCRE2 10.45** (upgraded from PCRE in OTP 28) to detect and route tool invocations from AI model output. This enables natural language tool calls without requiring structured JSON-RPC `tools/call` messages.

## PCRE2 Features

### What's New in OTP 28

| Feature | Description | Example |
|---------|-------------|---------|
| **Possessive Quantifiers** | Prevent backtracking for performance | `\w*+` |
| **Lookahead/Lookbehind** | Zero-width assertions | `(?=pattern)`, `(?<=pattern)` |
| **Atomic Groups** | Prevent catastrophic backtracking | `(?>pattern)` |
| **Conditional Patterns** | If-then-else in regex | `(?(condition)then|else)` |
| **Subroutine Calls** | Recursion in patterns | `(?P>name)` |
| **Unicode 15** | Full Unicode 15 support | `\p{Emoji}` |
| **Named Captures** | Readable capture groups | `(?<name>pattern)` |

## Built-in Tool Patterns

### 1. Calculator Pattern

**Pattern**:
```erlang
<<"calculate\\(\\s*(?<expr>[^)]+)\\s*\\)"/utf8>>
```

**Matches**:
```
calculate(2 + 2)              → expr: "2 + 2"
calculate(  10 * 5 + 3  )      → expr: "10 * 5 + 3"
calculate(-5 + 10)             → expr: "-5 + 10"
calculate((2 + 3) * 4)         → expr: "(2 + 3) * 4"
```

**Handler**: `erlmcp_tool_calculator`

### 2. Search Pattern

**Pattern**:
```erlang
<<"search\\s+for\\s+(?<query>[^.!?]+)[.!?]?"/utf8>>
```

**Matches**:
```
search for Erlang patterns          → query: "Erlang patterns"
search for OTP supervisors!         → query: "OTP supervisors"
search for gen_server callbacks     → query: "gen_server callbacks"
search for 天気 in 日本              → query: "天気 in 日本"
```

**Handler**: `erlmcp_tool_search`

### 3. Code Execution Pattern

**Pattern**:
```erlang
<<"execute\\s+(?<lang>\\w+)\\s*:\\s*(?<code>[^]+?)(?=\\s*(?:execute|search|calculate|$))"/utf8>>
```

**Matches**:
```
execute erlang: io:format("Hello")         → lang: "erlang", code: "io:format(\"Hello\")"
execute python: print('world')             → lang: "python", code: "print('world')"
execute javascript: console.log('test')    → lang: "javascript", code: "console.log('test')"
```

**Handler**: `erlmcp_tool_execute`

## Usage

### Detecting Tool Calls

```erlang
%% Simple detection with built-in patterns
Text = <<"calculate(2 + 2)">>,
case erlmcp_tool_router:detect_tool_call(Text) of
    {ok, HandlerModule, Args, Captures} ->
        %% Execute tool: HandlerModule:handle(Args, Captures)
        ok;
    not_found ->
        %% No tool invocation detected
        no_tool
end.
```

### Routing to Tools

```erlang
%% Direct routing by tool name
case erlmcp_tool_router:route_to_tool(<<"calculator">>, <<"2 + 2">>) of
    {ok, Module, Args} ->
        Module:handle(Args);
    {error, {unknown_tool, Name}} ->
        {error, unknown_tool}
end.
```

### Custom Patterns

```erlang
%% Add weather pattern
Pattern = <<"weather\\s+in\\s+(?<city>[A-Za-z\\s]+)">>,
ok = erlmcp_tool_router:add_pattern(<<"weather">>, Pattern, erlmcp_tool_weather),

%% Detect weather queries
Text = <<"weather in San Francisco">>,
{ok, erlmcp_tool_weather, _, Captures} = erlmcp_tool_router:detect_tool_call(Text),
City = maps:get(<<"city">>, Captures),
%% City = <<"San Francisco">>
```

### Extracting Named Captures

```erlang
%% Extract arguments from pattern match
Pattern = <<"calculate\\((?<expr>[^)]+)\\)">>,
Text = <<"calculate(10 * 5)">>,

{ok, Captures} = erlmcp_tool_router:extract_args(Text, Pattern),
Expr = maps:get(<<"expr">>, Captures),
%% Expr = <<"10 * 5">>
```

## Advanced PCRE2 Patterns

### Possessive Quantifiers

Prevent backtracking for performance-critical patterns:

```erlang
%% Possessive: matches "aaaa" but won't backtrack for "aaaab"
Pattern = <<"\\w*+(?<value>\\d+)">>,
```

### Lookahead Assertions

Match only if followed by pattern (without consuming):

```erlang
%% Positive lookahead: word followed by digit
Pattern = <<"\\w+\\s+(?=\\d+)">>,

%% Negative lookahead: word NOT followed by digit
Pattern = <<"\\w+\\s+(?!\\d+)">>,
```

### Lookbehind Assertions

Match only if preceded by pattern (without consuming):

```erlang
%% Positive lookbehind: digit at end
Pattern = <<"(?<number>\\d+)\\s*(?<=\\d)$">>,

%% Negative lookbehind: not preceded by digit
Pattern = <<"(?<text>\\w+)\\s*(?<!\\d)">>,
```

### Atomic Groups

Prevent catastrophic backtracking in complex patterns:

```erlang
%% Atomic: won't backtrack inside group
Pattern = <<"(?>(?<text>\\w+\\s*)+)+">>,
```

### Conditional Patterns

If-then-else logic in regex:

```erlang
%% If next char is digit, capture digits, else capture words
Pattern = <<"(?(?=\\d)(?<digits>\\d+)|(?<words>\\w+))">>,
```

### Recursion (Subroutine Calls)

Match nested structures:

```erlang
%% Match nested parentheses (up to PCRE2 recursion limit)
Pattern = <<"\\((?>[^()]++|(?R))*\\)">>,
```

## Pattern Validation

### Validate Before Adding

```erlang
case erlmcp_tool_router:validate_pattern(Pattern) of
    ok ->
        erlmcp_tool_router:add_pattern(Name, Pattern, Handler);
    {error, {invalid_pattern, Reason}} ->
        {error, invalid_pattern}
end.
```

### Compile Pattern

```erlang
case erlmcp_tool_router:compile_pattern(Pattern) of
    {ok, MP} ->
        %% Use compiled pattern for repeated matching
        re:run(Text, MP, [unicode]);
    {error, Reason} ->
        {error, compile_failed}
end.
```

## Unicode Support

All patterns use the `unicode` option for full Unicode 15 support:

```erlang
%% Unicode text detection
Text = <<"search for 天気 patterns in 日本"/utf8>>,
{ok, Handler, Args, Captures} = erlmcp_tool_router:detect_tool_call(Text),
```

## Performance Considerations

### Pattern Ordering

Patterns are tried in order; most specific patterns should be first:

```erlang
%% WRONG: Generic pattern catches everything
[?GENERIC_TOOL_PATTERN, ?CALCULATOR_PATTERN]

%% CORRECT: Specific patterns first
[?CALCULATOR_PATTERN, ?SEARCH_PATTERN, ?CODE_EXEC_PATTERN, ?GENERIC_TOOL_PATTERN]
```

### Compiled Patterns

ETS table stores pre-compiled patterns for performance:

```erlang
%% Pattern is compiled once when added
ok = erlmcp_tool_router:add_pattern(Name, Pattern, Handler),
```

### Possessive Quantifiers

Use possessive quantifiers (`*+`, `++`, `?+`, `{n,m}+`) to prevent unnecessary backtracking:

```erlang
%% GOOD: Possessive (no backtracking)
<<"\\w*+(?<value>\\d+)">>

%% AVOID: Greedy (may backtrack)
<<"\\w*(?<value>\\d+)">>
```

## Integration with erlmcp_server

### Automatic Detection

The tool router integrates with `erlmcp_server` to detect natural language tool calls:

```erlang
%% In erlmcp_server.erl handle_request/4
handle_request(Id, ?MCP_METHOD_TOOLS_CALL, Params, TransportId, State) ->
    Name = maps:get(<<"name">>, Params, <<>>),
    Arguments = maps:get(<<"arguments">>, Params, #{}),

    %% Check if arguments contain natural language tool call
    case erlmcp_tool_router:detect_tool_call(Arguments) of
        {ok, HandlerModule, Args, Captures} ->
            %% Route to detected tool
            execute_tool(HandlerModule, Args, Captures, Id, TransportId, State);
        not_found ->
            %% Fallback to structured tool call
            execute_structured_tool(Name, Arguments, Id, TransportId, State)
    end.
```

## Testing

### EUnit Tests

```bash
# Run tool router tests
rebar3 eunit --module=erlmcp_tool_router_tests
```

### Test Coverage

- Calculator pattern matching
- Search pattern matching (including Unicode)
- Code execution pattern matching
- Custom pattern management
- PCRE2 advanced features (lookahead, lookbehind, possessive)
- Edge cases (nested parentheses, special characters)
- Performance benchmarks

## Examples

### Example 1: Weather Tool

```erlang
%% Define pattern
Pattern = <<"weather\\s+in\\s+(?<city>[A-Za-z\\s]+)">>,
ok = erlmcp_tool_router:add_pattern(<<"weather">>, Pattern, erlmcp_tool_weather),

%% Implement handler
-module(erlmcp_tool_weather).
-export([handle/2]).

handle(Args, Captures) ->
    City = maps:get(<<"city">>, Captures),
    %% Call weather API
    {ok, WeatherData} = weather_api:get(City),
    {ok, #{<<"temperature">> => Temp, <<"conditions">> => Cond}} = WeatherData,
    {ok, #{<<"result">> => io_lib:format("~s: ~p°F, ~s", [City, Temp, Cond])}}.
```

### Example 2: Database Query Tool

```erlang
%% Advanced pattern with SQL
Pattern = <<"query\\s+(?<select>SELECT\\s+.+?)\\s+from\\s+(?<table>\\w+)(?:\\s+where\\s+(?<where>.+?))?$">>,
ok = erlmcp_tool_router:add_pattern(<<"query">>, Pattern, erlmcp_tool_query),

%% Matches:
%% query select * from users where id = 1
%%   → select: "select *", table: "users", where: "id = 1"
```

### Example 3: File Operations

```erlang
%% File read/write patterns
ReadPattern = <<"read\\s+file\\s+(?<path>[^\\s]+)">>,
WritePattern = <<"write\\s+file\\s+(?<path>[^\\s]+)\\s+(?<content>.+)$">>,

ok = erlmcp_tool_router:add_pattern(<<"read_file">>, ReadPattern, erlmcp_tool_file),
ok = erlmcp_tool_router:add_pattern(<<"write_file">>, WritePattern, erlmcp_tool_file),
```

## Troubleshooting

### Pattern Not Matching

1. **Check pattern syntax**:
   ```erlang
   erlmcp_tool_router:validate_pattern(Pattern).
   ```

2. **Verify Unicode option**:
   ```erlang
   %% Ensure pattern ends with /utf8
   <<"pattern"/utf8>>
   ```

3. **Test with exact text**:
   ```erlang
   erlmcp_tool_router:extract_args(Text, Pattern).
   ```

### Performance Issues

1. **Use possessive quantifiers**: Replace `*` with `*+`
2. **Order patterns**: Most specific first
3. **Avoid catastrophic backtracking**: Use atomic groups `(?>...)`

## References

- [PCRE2 Documentation](https://www.pcre.org/current/doc/html/)
- [Erlang re Module](https://www.erlang.org/doc/man/re.html)
- [OTP 28 Release Notes](https://www.erlang.org/doc/system_principals/versions.html#otp-28)
- [MCP Protocol 2025-11-25](https://spec.modelcontextprotocol.io/specification/2025-11-25/)
