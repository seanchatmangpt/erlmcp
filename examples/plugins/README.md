# erlmcp Plugin Examples

Example plugins demonstrating how to extend erlmcp with custom functionality.

## Plugin Files

### Data Transformers

**`formatter-json-to-csv.erl`** - JSON to CSV transformer plugin

A complete working plugin that demonstrates:
- Implementing erlmcp_plugin_transformer behavior
- Plugin lifecycle (start/stop)
- Data validation
- Error handling
- CSV escaping and quoting
- Comprehensive tests

**Features**:
- Converts JSON array of objects to CSV
- Handles special characters (quotes, commas, newlines)
- Configurable delimiters and quote characters
- Type conversion for numbers, atoms, binary strings
- Automatic header extraction

**Usage**:
```erlang
% Load and start
c(formatter_json_to_csv).
formatter_json_to_csv:start().

% Use the plugin
Data = [
  #{name => "Alice", age => 30, city => "NYC"},
  #{name => "Bob", age => 25, city => "Boston"}
],
{ok, CSV} = erlmcp_plugin_registry:call(
  json_to_csv,
  transform,
  [Data, #{}]
).

% Output: CSV string
```

**Run tests**:
```erlang
formatter_json_to_csv:test_basic().
formatter_json_to_csv:test_with_special_chars().
formatter_json_to_csv:test_error_handling().
```

---

### Validators

**`validator-custom.erl`** - Custom security validator plugin

A complete working plugin demonstrating:
- Implementing erlmcp_plugin_validator behavior
- Security checks (SQL injection, XSS, path traversal)
- Format validation (email, URL)
- Extensible check architecture
- Comprehensive test suite

**Features**:
- SQL injection detection
- XSS (cross-site scripting) attack detection
- Path traversal attack detection
- Email format validation
- URL format validation
- Case-insensitive pattern matching
- Support for binary, list, and atom inputs

**Usage**:
```erlang
% Load and start
c(validator_custom).
validator_custom:start().

% Validate data
{ok} = erlmcp_plugin_registry:call(
  security_check,
  validate,
  [<<"user@example.com">>, #{checks => [email_format]}]
).

% Or detect attacks
{error, Violations} = erlmcp_plugin_registry:call(
  security_check,
  validate,
  [<<"'; DROP TABLE users; --">>, #{checks => [sql_injection]}]
).
```

**Run tests**:
```erlang
validator_custom:test_sql_injection().
validator_custom:test_xss().
validator_custom:test_path_traversal().
validator_custom:test_email_validation().
validator_custom:test_url_validation().
validator_custom:run_all_tests().
```

---

## Learning Path

### 1. Understand Plugin Architecture (5 min)

Read: [PLUGIN_DEVELOPMENT_GUIDE.md](../../docs/PLUGIN_DEVELOPMENT_GUIDE.md)
- Plugin types and behaviors
- Plugin registration system
- Lifecycle hooks

### 2. Study Transformer Plugin (10 min)

Read: `formatter-json-to-csv.erl` comments
- Behavior callback implementation
- Error handling patterns
- Value escaping logic
- Test structure

### 3. Study Validator Plugin (10 min)

Read: `validator-custom.erl` comments
- Multiple check implementations
- Pattern matching and detection
- Format validation
- Test patterns

### 4. Run Tests (5 min)

```erlang
% Start Erlang
erlmcp start

% Load and test transformer
c(formatter_json_to_csv).
formatter_json_to_csv:run_tests().

% Load and test validator
c(validator_custom).
validator_custom:run_all_tests().
```

### 5. Create Your Own (30+ min)

Use the examples as templates:
1. Copy one of the example plugins
2. Modify behavior and callbacks
3. Update tests
4. Register and use

---

## Quick Start

### Load an example plugin:

```bash
cd erlmcp
erlmcp start
```

Then in the REPL:

```erlang
% Load transformer
c(examples/plugins/formatter_json_to_csv).
formatter_json_to_csv:start().

% Test it
Data = [#{name => "Test"}],
{ok, CSV} = erlmcp_plugin_registry:call(json_to_csv, transform, [Data, #{}]).

% Or load validator
c(examples/plugins/validator_custom).
validator_custom:start().

% Test security checks
{error, Violations} = erlmcp_plugin_registry:call(
  security_check,
  validate,
  [<<"<script>alert(1)</script>">>, #{checks => [xss]}]
).
```

---

## Plugin Development Workflow

### Step 1: Choose a behavior

```erlang
% Copy an example and modify
cp formatter-json-to-csv.erl my-transformer.erl
```

### Step 2: Implement callbacks

```erlang
-behaviour(erlmcp_plugin_transformer).

-export([
  name/0,
  format/0,
  transform/2,
  start/0,
  stop/0
]).
```

### Step 3: Write tests

```erlang
% Add test functions
test_basic() -> ...
test_edge_cases() -> ...
test_error_handling() -> ...

run_tests() ->
  test_basic(),
  test_edge_cases(),
  test_error_handling().
```

### Step 4: Test in REPL

```erlang
c(my_transformer).
my_transformer:run_tests().
my_transformer:start().
```

### Step 5: Integrate

```erlang
% Use with registry
erlmcp_plugin_registry:call(
  my_plugin,
  method,
  [Args]
).
```

---

## Plugin Categories

### Current Examples

| Plugin | Type | Purpose |
|--------|------|---------|
| formatter-json-to-csv | Transformer | Convert JSON arrays to CSV |
| validator-custom | Validator | Security & format validation |

### Ideas for More Plugins

**Transformers**:
- XML to JSON
- CSV to JSON
- YAML to JSON
- MessagePack to JSON
- Protocol Buffer to JSON

**Validators**:
- GDPR compliance checker
- HIPAA compliance checker
- Custom business rule validator
- JSON Schema validator
- OpenAPI validator

**Resources**:
- Database connector
- REST API connector
- File system handler
- Cache handler
- Message queue handler

**Middleware**:
- Request logger
- Response formatter
- Rate limiter
- Cache wrapper
- Metrics collector

---

## Testing

### Unit Tests

Each plugin includes unit tests following this pattern:

```erlang
test_case_name() ->
  io:format("Test: Case name~n"),

  % Arrange
  Input = ...,
  Expected = ...,

  % Act
  {Result, Output} = function_under_test(Input),

  % Assert
  case Output of
    Expected -> io:format("✓ Passed~n");
    _ -> io:format("✗ Failed~n")
  end.
```

### Running Tests

```erlang
% Run individual test
formatter_json_to_csv:test_basic().

% Run all tests
formatter_json_to_csv:run_tests().
validator_custom:run_all_tests().

% Or use EUnit
rebar3 eunit --module=my_plugin_tests
```

---

## Common Patterns

### Error Handling

```erlang
transform(Input, Options) ->
  try
    validate_input(Input),
    process(Input, Options)
  catch
    error:{validation, Reason} ->
      {error, {invalid_input, Reason}};
    error:Reason ->
      {error, {processing_error, Reason}}
  end.
```

### Configuration via Options

```erlang
transform(Input, Options) ->
  Mode = maps:get(mode, Options, auto),
  Delimiter = maps:get(delimiter, Options, <<",">>),
  IncludeHeaders = maps:get(headers, Options, true),
  ...
end.
```

### Type Conversion

```erlang
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_number(V) -> list_to_binary(integer_to_list(V));
to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).
```

### Validation Pattern

```erlang
validate_input(Input) when is_list(Input) ->
  case Input of
    [] -> throw(error({invalid_input, empty}));
    [H|_] when is_map(H) -> ok;
    _ -> throw(error({invalid_input, not_maps}))
  end;
validate_input(_) ->
  throw(error({invalid_input, not_list})).
```

---

## Next Steps

1. **Read** the [PLUGIN_DEVELOPMENT_GUIDE.md](../../docs/PLUGIN_DEVELOPMENT_GUIDE.md) for comprehensive reference
2. **Study** the example plugins in this directory
3. **Run** the tests to see them in action
4. **Create** your own plugin following the same patterns
5. **Share** your plugins with the community

---

## See Also

- [Plugin Development Guide](../../docs/PLUGIN_DEVELOPMENT_GUIDE.md) - Complete plugin reference
- [CLI Interactive Guide](../../docs/CLI_INTERACTIVE_GUIDE.md) - REPL examples
- [CLI Reference](../../docs/CLI_REFERENCE.md) - Command reference
- [Diagnostics Guide](../../docs/DIAGNOSTICS_GUIDE.md) - Advanced profiling
