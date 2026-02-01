# erlmcp Plugin Development Guide

Learn how to extend erlmcp with custom plugins. Build data transformers, validators, integrations, and more.

**Table of Contents**:
- [Plugin Architecture](#plugin-architecture)
- [Creating Your First Plugin](#creating-your-first-plugin)
- [Plugin Behaviors](#plugin-behaviors)
- [Plugin Lifecycle](#plugin-lifecycle)
- [Example: JSON-to-CSV Transformer](#example-json-to-csv-transformer)
- [Example: Custom Validator](#example-custom-validator)
- [Testing Plugins](#testing-plugins)
- [Publishing Plugins](#publishing-plugins)
- [Plugin Best Practices](#plugin-best-practices)

## Plugin Architecture

### Plugin types

erlmcp supports several plugin categories:

| Type | Purpose | Example |
|------|---------|---------|
| Transformer | Convert data formats | JSON→CSV, XML→JSON |
| Validator | Custom validation rules | Security checks, format validation |
| Transport | Custom protocol support | Custom framing, binary protocols |
| Middleware | Intercept/modify requests | Logging, metrics, auth |
| Resource | Custom resource handlers | Database, file system, APIs |
| Tool | Custom tool implementations | Shell commands, integrations |

### Plugin structure

```
my-plugin/
├── src/
│   ├── my_plugin.erl           # Main module (implements behavior)
│   └── my_plugin_helper.erl    # Helper module (optional)
├── test/
│   └── my_plugin_tests.erl    # Plugin tests
├── examples/
│   └── usage_example.erl       # Usage example
└── README.md                   # Plugin documentation
```

### Plugin registration

Plugins are registered with the plugin registry:

```erlang
% Register a plugin
erlmcp_plugin_registry:register(
  my_transformer,          % Plugin name
  erlmcp_plugin_transformer, % Behavior module
  my_plugin                  % Implementation module
).

% Use the plugin
erlmcp_plugin_registry:call(my_transformer, transform, [Data]).
```

---

## Creating Your First Plugin

### Step 1: Define the plugin behavior

```erlang
% file: erlmcp_plugin_transformer.erl
-module(erlmcp_plugin_transformer).

% Behavior definition
-callback transform(Input :: term(), Options :: map()) ->
  {ok, Output :: term()} | {error, Reason :: term()}.

-callback format() -> atom().  % Output format: csv, json, xml, etc.

-callback name() -> atom().    % Plugin name
```

### Step 2: Implement the behavior

```erlang
% file: my_json_csv_transformer.erl
-module(my_json_csv_transformer).

% Declare behavior implementation
-behaviour(erlmcp_plugin_transformer).

% Export all callback functions
-export([transform/2, format/0, name/0]).

% Callback implementation 1: Plugin metadata
name() ->
  json_to_csv.

format() ->
  csv.

% Callback implementation 2: Main transformation logic
transform(Input, _Options) ->
  try
    % Input is a list of maps (JSON array of objects)
    Rows = [convert_row(Row) || Row <- Input],
    % Convert to CSV
    {ok, format_csv(Rows)}
  catch
    error:Reason ->
      {error, {transform_failed, Reason}}
  end.

% Helper: convert single row
convert_row(Row) when is_map(Row) ->
  maps:values(Row);
convert_row(Row) when is_list(Row) ->
  Row;
convert_row(Other) ->
  [Other].

% Helper: format as CSV
format_csv(Rows) ->
  string:join([row_to_csv(Row) || Row <- Rows], "\n").

row_to_csv(Row) ->
  string:join([format_value(V) || V <- Row], ",").

format_value(V) when is_number(V) -> integer_to_list(V);
format_value(V) when is_atom(V) -> atom_to_list(V);
format_value(V) when is_binary(V) -> binary_to_list(V);
format_value(V) -> lists:flatten(io_lib:format("~p", [V])).
```

### Step 3: Create the plugin module

```erlang
% file: my_plugin.erl
-module(my_plugin).

% Public API
-export([start/0, stop/0]).

% Start hook (register plugin on startup)
start() ->
  erlmcp_plugin_registry:register(
    json_to_csv,                    % Name
    erlmcp_plugin_transformer,      % Behavior
    my_json_csv_transformer         % Implementation
  ),
  {ok, loaded}.

% Stop hook (cleanup on shutdown)
stop() ->
  erlmcp_plugin_registry:unregister(json_to_csv),
  ok.
```

### Step 4: Load the plugin

```erlang
% In Erlang shell
1> c(my_json_csv_transformer).
{ok, my_json_csv_transformer}

2> c(my_plugin).
{ok, my_plugin}

3> my_plugin:start().
{ok, loaded}

% Test it
4> Data = [
4>   #{name => "Alice", age => 30, city => "New York"},
4>   #{name => "Bob", age => 25, city => "Boston"}
4> ].

5> erlmcp_plugin_registry:call(json_to_csv, transform, [Data, #{}]).
{ok, "alice,30,new york\nbob,25,boston"}
```

---

## Plugin Behaviors

### Transformer behavior

**Purpose**: Convert between data formats

```erlang
-module(erlmcp_plugin_transformer).
-callback transform(Input :: term(), Options :: map()) ->
  {ok, Output :: term()} | {error, Reason :: term()}.
```

**Example use cases**:
- JSON ↔ CSV
- XML ↔ JSON
- Binary ↔ Text
- Format validation

### Validator behavior

**Purpose**: Validate data against custom rules

```erlang
-module(erlmcp_plugin_validator).
-callback validate(Data :: term(), Rules :: map()) ->
  {ok} | {error, Violations :: list()}.
```

**Example use cases**:
- Custom format validation
- Business rule checking
- Security validation

### Resource handler behavior

**Purpose**: Provide custom resource implementations

```erlang
-module(erlmcp_plugin_resource).
-callback read(Uri :: binary()) ->
  {ok, Content :: binary()} | {error, Reason :: term()}.

-callback write(Uri :: binary(), Content :: binary()) ->
  {ok} | {error, Reason :: term()}.

-callback delete(Uri :: binary()) ->
  {ok} | {error, Reason :: term()}.
```

**Example use cases**:
- Database connections
- HTTP API clients
- File system access

### Middleware behavior

**Purpose**: Intercept and modify requests/responses

```erlang
-module(erlmcp_plugin_middleware).
-callback process_request(Request :: map()) ->
  {ok, ModifiedRequest :: map()} | {error, Reason :: term()}.

-callback process_response(Response :: map()) ->
  {ok, ModifiedResponse :: map()} | {error, Reason :: term()}.
```

**Example use cases**:
- Request logging
- Rate limiting
- Authentication injection
- Response formatting

---

## Plugin Lifecycle

### Plugin initialization

```erlang
% 1. Module is loaded into Erlang VM
code:ensure_loaded(my_plugin).

% 2. Behavior is registered
erlmcp_plugin_registry:register(my_plugin, behavior, module).

% 3. Plugin hook is called (if available)
my_plugin:start().

% 4. Plugin is available for use
erlmcp_plugin_registry:call(my_plugin, method, [Args]).
```

### Plugin shutdown

```erlang
% 1. Plugin is unregistered
erlmcp_plugin_registry:unregister(my_plugin).

% 2. Cleanup hook is called
my_plugin:stop().

% 3. Module remains loaded (or can be purged)
code:purge(my_plugin).
```

### Lifecycle hooks

Implement optional hooks for lifecycle events:

```erlang
% file: my_plugin.erl
-module(my_plugin).

% Optional: Called on initialization
-export([on_load/0]).
on_load() ->
  io:format("Plugin loaded~n"),
  ok.

% Optional: Called on shutdown
-export([on_unload/0]).
on_unload() ->
  io:format("Plugin unloaded~n"),
  ok.

% Optional: Called periodically for health checks
-export([on_health_check/0]).
on_health_check() ->
  {ok, {status, healthy}}.
```

---

## Example: JSON-to-CSV Transformer

### Full working example

```erlang
% file: json_to_csv_transformer.erl
-module(json_to_csv_transformer).
-behaviour(erlmcp_plugin_transformer).

-export([transform/2, format/0, name/0]).
-export([start/0, stop/0]).

% ==================== Public API ====================

start() ->
  erlmcp_plugin_registry:register(
    json_to_csv,
    erlmcp_plugin_transformer,
    json_to_csv_transformer
  ),
  io:format("JSON→CSV transformer loaded~n"),
  {ok, loaded}.

stop() ->
  erlmcp_plugin_registry:unregister(json_to_csv),
  io:format("JSON→CSV transformer unloaded~n"),
  ok.

% ==================== Behavior Implementation ====================

name() ->
  <<"json_to_csv">>.

format() ->
  csv.

transform(Input, Options) ->
  try
    % Validate input
    validate_input(Input),

    % Extract headers if needed
    Headers = case maps:get(headers, Options, auto) of
      auto -> extract_headers(Input);
      explicit -> maps:get(header_list, Options, [])
    end,

    % Convert rows
    Rows = [map_to_values(Row, Headers) || Row <- Input],

    % Format as CSV
    CSV = format_csv([Headers | Rows]),

    {ok, CSV}
  catch
    error:{invalid_input, Reason} ->
      {error, {invalid_input, Reason}};
    error:Other ->
      {error, {transform_error, Other}}
  end.

% ==================== Helper Functions ====================

validate_input(Input) when is_list(Input) ->
  case Input of
    [] -> throw(error({invalid_input, empty_list}));
    [First | _] when not is_map(First) ->
      throw(error({invalid_input, non_map_in_list}));
    _ -> ok
  end;
validate_input(_) ->
  throw(error({invalid_input, not_a_list})).

extract_headers([First | _]) when is_map(First) ->
  maps:keys(First);
extract_headers(_) ->
  [].

map_to_values(Row, Headers) when is_map(Row) ->
  [escape_value(maps:get(H, Row, "")) || H <- Headers];
map_to_values(Row, _) when is_list(Row) ->
  [escape_value(V) || V <- Row].

escape_value(V) when is_binary(V) ->
  escape_csv(V);
escape_value(V) when is_list(V) ->
  escape_csv(list_to_binary(V));
escape_value(V) when is_number(V) ->
  integer_to_list(V);
escape_value(V) ->
  escape_csv(term_to_binary(V)).

escape_csv(Value) ->
  case binary:match(Value, [<<$">>, <<$,>>, <<$\n>>]) of
    nomatch -> Value;
    _ -> <<"\"", (binary:replace(Value, <<"\"">>, <<"\"\"">>, [global]))/binary, "\"">>
  end.

format_csv([]) -> <<>>;
format_csv(Rows) ->
  RowStrings = [row_to_binary(Row) || Row <- Rows],
  binary:list_to_bin(string:join(RowStrings, "\n")).

row_to_binary(Row) ->
  Values = [
    case V of
      B when is_binary(B) -> B;
      N when is_number(N) -> list_to_binary(integer_to_list(N));
      L when is_list(L) -> list_to_binary(L);
      _ -> <<"NULL">>
    end
    || V <- Row
  ],
  binary:list_to_bin(string:join(Values, ",")).
```

### Usage

```erlang
% In REPL
1> c(json_to_csv_transformer).
{ok, json_to_csv_transformer}

2> json_to_csv_transformer:start().
JSON→CSV transformer loaded
{ok, loaded}

% Test data
3> Data = [
3>   #{name => <<"Alice">>, age => 30, salary => 80000},
3>   #{name => <<"Bob">>, age => 25, salary => 75000},
3>   #{name => <<"Carol">>, age => 35, salary => 95000}
3> ].

% Transform to CSV
4> {ok, CSV} = erlmcp_plugin_registry:call(
4>   json_to_csv,
4>   transform,
4>   [Data, #{headers => auto}]
4> ).

5> io:format("~s~n", [CSV]).
Alice,30,80000
Bob,25,75000
Carol,35,95000
```

---

## Example: Custom Validator

### Security validator plugin

```erlang
% file: security_validator.erl
-module(security_validator).
-behaviour(erlmcp_plugin_validator).

-export([validate/2]).
-export([start/0, stop/0]).

start() ->
  erlmcp_plugin_registry:register(
    security_check,
    erlmcp_plugin_validator,
    security_validator
  ),
  io:format("Security validator loaded~n"),
  {ok, loaded}.

stop() ->
  erlmcp_plugin_registry:unregister(security_check),
  ok.

validate(Data, Rules) ->
  Checks = maps:get(checks, Rules, [
    sql_injection,
    xss,
    path_traversal
  ]),

  Violations = [
    {Check, Reason}
    || Check <- Checks,
       {violation, Reason} <- [run_check(Check, Data)]
  ],

  case Violations of
    [] -> {ok};
    _ -> {error, Violations}
  end.

run_check(sql_injection, Data) ->
  Keywords = [
    <<"union">>, <<"select">>, <<"drop">>,
    <<"insert">>, <<"update">>, <<"delete">>
  ],
  case has_any_keyword(Data, Keywords) of
    true -> {violation, "SQL injection attempt detected"};
    false -> {ok}
  end;

run_check(xss, Data) ->
  Tags = [<<"<script">>, <<"onclick">>, <<"onerror">>],
  case has_any_keyword(Data, Tags) of
    true -> {violation, "XSS attempt detected"};
    false -> {ok}
  end;

run_check(path_traversal, Data) ->
  Patterns = [<<"../">>, <<"..\\">>, <<"..">>, null],
  case has_any_keyword(Data, Patterns) of
    true -> {violation, "Path traversal attempt detected"};
    false -> {ok}
  end;

run_check(_, _) ->
  {ok}.

has_any_keyword(Data, Keywords) ->
  DataBin = to_binary(Data),
  DataLower = string:lowercase(DataBin),
  lists:any(
    fun(Keyword) ->
      binary:match(DataLower, string:lowercase(Keyword)) =/= nomatch
    end,
    Keywords
  ).

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(N) when is_number(N) -> list_to_binary(integer_to_list(N));
to_binary(X) -> list_to_binary(io_lib:format("~p", [X])).
```

---

## Testing Plugins

### Unit testing

```erlang
% file: json_to_csv_transformer_tests.erl
-module(json_to_csv_transformer_tests).

-include_lib("eunit/include/eunit.hrl").

transform_basic_test() ->
  Data = [
    #{name => <<"Alice">>, age => 30},
    #{name => <<"Bob">>, age => 25}
  ],
  {ok, CSV} = json_to_csv_transformer:transform(Data, #{}),
  ?assertMatch(_, CSV).

transform_with_empty_test() ->
  Data = [
    #{name => <<"Alice">>, value => ""},
    #{name => <<"Bob">>, value => "test"}
  ],
  {ok, CSV} = json_to_csv_transformer:transform(Data, #{}),
  ?assertMatch(_, CSV).

escape_quotes_test() ->
  Data = [
    #{name => <<"Alice \"The Great\"">>}
  ],
  {ok, CSV} = json_to_csv_transformer:transform(Data, #{}),
  ?assert(binary:match(CSV, <<"\"Alice")) =/= nomatch).

invalid_input_test() ->
  ?assertMatch(
    {error, {invalid_input, _}},
    json_to_csv_transformer:transform(not_a_list, #{})
  ).
```

Run tests:
```bash
rebar3 eunit --module=json_to_csv_transformer_tests
```

### Integration testing

```erlang
% file: plugin_integration_tests.erl
-module(plugin_integration_tests).

-include_lib("eunit/include/eunit.hrl").

plugin_lifecycle_test() ->
  % Load plugin
  c(json_to_csv_transformer),
  {ok, _} = json_to_csv_transformer:start(),

  % Verify registration
  ?assertMatch(
    json_to_csv,
    erlmcp_plugin_registry:lookup(json_to_csv)
  ),

  % Use plugin
  Data = [#{name => <<"Test">>}],
  {ok, _} = erlmcp_plugin_registry:call(
    json_to_csv,
    transform,
    [Data, #{}]
  ),

  % Cleanup
  json_to_csv_transformer:stop().
```

---

## Publishing Plugins

### Package structure

```
json-to-csv-plugin/
├── src/
│   └── json_to_csv_transformer.erl
├── test/
│   └── json_to_csv_transformer_tests.erl
├── examples/
│   └── basic_usage.erl
├── README.md
├── CHANGELOG.md
├── LICENSE
└── rebar.config
```

### rebar.config for plugin

```erlang
{erl_opts, [debug_info]}.

{deps, [
  {erlmcp, {git, "https://github.com/user/erlmcp.git", {tag, "v3.0.0"}}}
]}.

{profiles, [
  {test, [
    {erl_opts, [debug_info]}
  ]}
]}.
```

### Documentation template

```markdown
# JSON-to-CSV Transformer Plugin

Transform JSON array of objects to CSV format.

## Installation

Add to rebar.config:
```erlang
{deps, [
  {json_csv_plugin, {git, "https://github.com/user/json-csv-plugin.git"}}
]}
```

## Usage

```erlang
% Register
json_to_csv_transformer:start().

% Use
Data = [#{name => "Alice", age => 30}],
{ok, CSV} = erlmcp_plugin_registry:call(
  json_to_csv,
  transform,
  [Data, #{}]
).
```

## Configuration

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| headers | auto\|explicit | auto | How to handle headers |
| delimiter | binary | "," | CSV delimiter |
| quote | binary | "\"" | Quote character |
```

---

## Plugin Best Practices

### Design principles

1. **Single responsibility** - Each plugin does one thing well
2. **Error handling** - Return `{error, Reason}` instead of throwing
3. **Configuration** - Accept options map for customization
4. **Validation** - Validate input and options
5. **Documentation** - Include usage examples
6. **Testing** - Test all error paths and edge cases

### Performance considerations

```erlang
% Avoid: Building large intermediate lists
transform_slow(Items, _) ->
  [process(Item) || Item <- Items],  % Two passes
  [format(Item) || Item <- Items].

% Prefer: Single pass
transform_fast(Items, _) ->
  [format(process(Item)) || Item <- Items].

% Avoid: Spawning processes for each item
transform_very_slow(Items, _) ->
  [spawn(?MODULE, process, [Item]) || Item <- Items].

% Prefer: Batching
transform_better(Items, _) ->
  batch_process(Items, 1000).
```

### Error handling pattern

```erlang
transform(Input, Options) ->
  try
    % Validate inputs
    {ok, ValidInput} = validate_input(Input),
    {ok, ValidOpts} = validate_options(Options),

    % Process
    process(ValidInput, ValidOpts)

  catch
    error:{validation, Reason} ->
      {error, {invalid_input, Reason}};
    error:Reason ->
      {error, {processing_error, Reason}};
    throw:Error ->
      {error, Error}
  end.
```

---

## See Also

- [CLI_REFERENCE.md](CLI_REFERENCE.md) - CLI command reference
- [API_REFERENCE.md](api-reference.md) - Plugin API documentation
- [examples/plugins/](../examples/plugins/) - Example plugins
