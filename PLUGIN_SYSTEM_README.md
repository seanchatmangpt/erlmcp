# erlmcp Plugin System

## Overview

The erlmcp plugin system enables extensibility of the CLI through custom validators, formatters, exporters, commands, and middleware. The system follows Erlang/OTP best practices with full supervision, process isolation, and let-it-crash fault tolerance.

## Architecture

### Supervision Tree

```
erlmcp_core_sup (one_for_one)
├── ... (existing children)
└── erlmcp_plugin_sup (one_for_one)
    ├── erlmcp_plugin_registry (gen_server) - Plugin metadata & routing (gproc)
    ├── erlmcp_plugin_manager (gen_server) - Plugin lifecycle management
    └── erlmcp_plugin_worker_sup (simple_one_for_one) - Dynamic plugin instances
```

### Plugin Types

1. **Validators** (`erlmcp_plugin_validator`) - Custom spec validators
2. **Formatters** (`erlmcp_plugin_formatter`) - Output formatters (CSV, XML, AVRO)
3. **Exporters** (`erlmcp_plugin_exporter`) - Export to external systems (S3, Kafka)
4. **Commands** (`erlmcp_plugin_command`) - New CLI commands
5. **Middleware** (`erlmcp_plugin_middleware`) - Request/response interceptors

### Key Features

- **Process Isolation**: Each plugin runs in its own gen_server process
- **Fault Tolerance**: Plugin crash ≠ system crash (let-it-crash)
- **Hot Reload**: Plugins can be loaded/unloaded without system restart
- **Discovery**: Automatic plugin discovery from filesystem
- **Routing**: O(log N) plugin lookups via gproc
- **Security**: Basic sandboxing via Erlang VM process isolation

## Plugin Development

### Creating a Plugin

Use the make target to scaffold a new plugin:

```bash
make create-plugin-scaffold
# Follow the prompts to specify name and type
```

Or manually create a plugin module:

```erlang
-module(erlmcp_plugin_my_formatter).
-behaviour(erlmcp_plugin_formatter).

-export([metadata/0, init/1, format/2, supports_format/0, terminate/2]).

metadata() ->
    #{
        name => <<"my_formatter">>,
        version => <<"1.0.0">>,
        type => formatter,
        description => <<"Custom output formatter">>,
        author => <<"Your Name">>
    }.

init(Opts) ->
    {ok, #{}}.

format(Data, State) ->
    FormattedData = convert_to_format(Data),
    {ok, FormattedData, State}.

supports_format() ->
    custom.

terminate(_Reason, _State) ->
    ok.

%% Internal functions
convert_to_format(Data) ->
    %% Your formatting logic
    jsx:encode(Data).
```

### Plugin Behavior Callbacks

#### Base Plugin (all types)

- `metadata/0` - Returns plugin metadata (name, version, type, description, author)
- `init/1` - Initialize plugin with options, returns `{ok, State}` or `{error, Reason}`
- `terminate/2` - Cleanup (optional)

#### Type-Specific Callbacks

**Validator**:
- `validate(Data, State)` - Returns `{ok, Result, NewState}` or `{error, Reason}`
- `get_schema/0` - Returns JSON schema

**Formatter**:
- `format(Data, State)` - Returns `{ok, Formatted, NewState}` or `{error, Reason}`
- `supports_format/0` - Returns format atom (e.g., `csv`, `xml`, `avro`)

**Exporter**:
- `export(Data, State)` - Returns `{ok, Result, NewState}` or `{error, Reason}`
- `get_config_schema/0` - Returns configuration schema

**Command**:
- `execute(Args, State)` - Returns `{ok, Result, NewState}` or `{error, Reason}`
- `help/0` - Returns help text binary

**Middleware**:
- `pre_execute(Request, State)` - Returns `{ok, ModifiedRequest, NewState}`
- `post_execute(Response, State)` - Returns `{ok, ModifiedResponse, NewState}`

## Plugin Installation

### Method 1: User Directory (Recommended)

```bash
# Compile your plugin
erlc -o ~/.erlmcp/plugins/ebin my_plugin.erl

# Or use the make target
make compile-plugin PLUGIN=my_formatter
```

### Method 2: Application Priv Directory

```bash
erlc -o apps/erlmcp_core/priv/plugins my_plugin.erl
```

### Method 3: Custom Directory via sys.config

```erlang
[
  {erlmcp_core, [
    {plugin_dirs, [
      "/opt/custom/plugins",
      "/usr/local/share/erlmcp/plugins"
    ]},
    {plugin_auto_discover, true}
  ]}
].
```

## Plugin Management

### Makefile Targets

```bash
# Create a new plugin scaffold
make create-plugin-scaffold

# Compile a plugin
make compile-plugin PLUGIN=my_formatter

# Run plugin system tests
make test-plugins

# Load a plugin dynamically
make load-plugin PLUGIN=my_formatter

# List loaded plugins
make list-plugins

# Build example AVRO formatter plugin
make example-avro-plugin
```

### Programmatic API

```erlang
%% Discover and load all plugins
{ok, Modules} = erlmcp_plugin_manager:discover_and_load_plugins().

%% Load specific plugin
{ok, Pid} = erlmcp_plugin_manager:load_plugin(erlmcp_plugin_my_formatter).

%% Load plugin with options
{ok, Pid} = erlmcp_plugin_manager:load_plugin(
    erlmcp_plugin_my_formatter,
    #{compression => true}
).

%% Unload plugin
ok = erlmcp_plugin_manager:unload_plugin(erlmcp_plugin_my_formatter).

%% Reload plugin (hot reload)
{ok, Pid} = erlmcp_plugin_manager:reload_plugin(erlmcp_plugin_my_formatter).

%% List loaded plugins
{ok, Modules} = erlmcp_plugin_manager:list_loaded_plugins().

%% Get plugin info
{ok, PluginInfo} = erlmcp_plugin_registry:get_plugin(erlmcp_plugin_my_formatter).

%% List plugins by type
{ok, Formatters} = erlmcp_plugin_registry:list_plugins_by_type(formatter).

%% Execute pre-command hooks (middleware)
{ok, ModifiedRequest} = erlmcp_plugin_manager:execute_pre_command_hooks(Request).

%% Execute post-command hooks (middleware)
{ok, ModifiedResponse} = erlmcp_plugin_manager:execute_post_command_hooks(Request, Response).

%% Call plugin function directly
Result = erlmcp_plugin_manager:call_plugin(
    erlmcp_plugin_my_formatter,
    format,
    [Data]
).
```

## Example Plugin: AVRO Formatter

See `/home/user/erlmcp/examples/plugins/erlmcp_plugin_avro_formatter.erl` for a complete working example.

This plugin demonstrates:
- Converting JSON to Apache AVRO binary format
- Schema definition
- Compression support
- State management

Build and use:

```bash
# Build the example plugin
make example-avro-plugin

# Copy to user plugins directory
cp _build/default/plugins/erlmcp_plugin_avro_formatter.beam ~/.erlmcp/plugins/

# Use in CLI (after integration with validate CLI)
erlmcp_validate report --format avro
```

## Plugin Discovery

Plugins are discovered from these paths (in order):

1. `~/.erlmcp/plugins/*.beam` (user plugins)
2. Custom directories from sys.config `{plugin_dirs, [...]}`
3. Application priv dir: `apps/erlmcp_core/priv/plugins/*.beam`
4. `/usr/local/lib/erlmcp/plugins/*.beam` (system-wide)
5. `/opt/erlmcp/plugins/*.beam` (alternative system location)

## Testing

### Unit Tests (EUnit)

Comprehensive EUnit tests in `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_plugin_tests.erl`:

```bash
# Run plugin system tests
rebar3 eunit --module=erlmcp_plugin_tests

# Or via make
make test-plugins
```

Tests cover:
- Plugin loader validation
- Plugin registry registration
- Plugin manager lifecycle
- Plugin worker isolation
- Plugin discovery
- Hook execution

### Integration Tests

Create mock plugins for testing:

```erlang
%% In your test module
Module = create_mock_formatter_plugin(),
{ok, WorkerPid} = erlmcp_plugin_manager:load_plugin(Module, #{}),
{ok, Formatted} = erlmcp_plugin_worker:call_function(WorkerPid, format, [Data]),
ok = erlmcp_plugin_manager:unload_plugin(Module).
```

## Security

### Process Isolation

Each plugin runs in an isolated gen_server process. Plugin crashes do not affect:
- Other plugins
- The plugin manager
- The core system

### Sandboxing

Basic sandboxing via Erlang VM process isolation:
- Each plugin has its own process dictionary
- No shared state between plugins
- Supervisor restart strategies prevent cascading failures

### Future Enhancements

Potential security enhancements (not yet implemented):
- Plugin signing (GPG signatures)
- Permission system (file access, network access)
- Resource limits (memory, CPU quotas)
- Audit logging for plugin operations

## Implementation Modules

### Core Modules

- **erlmcp_plugin** - Base plugin behavior definition
- **erlmcp_plugin_validator** - Validator plugin behavior
- **erlmcp_plugin_formatter** - Formatter plugin behavior
- **erlmcp_plugin_exporter** - Exporter plugin behavior
- **erlmcp_plugin_command** - Command plugin behavior
- **erlmcp_plugin_middleware** - Middleware plugin behavior

### Infrastructure Modules

- **erlmcp_plugin_manager** - Plugin lifecycle orchestration (gen_server)
- **erlmcp_plugin_registry** - Plugin tracking and routing (gen_server, gproc)
- **erlmcp_plugin_loader** - Dynamic code loading and validation
- **erlmcp_plugin_worker** - Individual plugin instance (gen_server)

### Supervisors

- **erlmcp_plugin_sup** - Main plugin supervisor (one_for_one)
- **erlmcp_plugin_worker_sup** - Dynamic worker supervisor (simple_one_for_one)

## Files Created

### Source Files
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_validator.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_formatter.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_exporter.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_command.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_middleware.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_manager.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_registry.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_loader.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_worker.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_sup.erl`
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_plugin_worker_sup.erl`

### Test Files
- `/home/user/erlmcp/apps/erlmcp_core/test/erlmcp_plugin_tests.erl`

### Example Plugins
- `/home/user/erlmcp/examples/plugins/erlmcp_plugin_avro_formatter.erl`

### Scripts
- `/home/user/erlmcp/scripts/dev/create_plugin_scaffold.sh`

### Modified Files
- `/home/user/erlmcp/apps/erlmcp_core/src/erlmcp_core_sup.erl` (added plugin supervisor)
- `/home/user/erlmcp/Makefile` (added plugin targets)

## Architecture Decisions

### Supervision Strategy

**erlmcp_plugin_sup (one_for_one)**:
- Registry, manager, and worker supervisor fail independently
- Registry failure doesn't affect manager or workers
- Manager failure doesn't affect running workers (workers continue until terminated)

**erlmcp_plugin_worker_sup (simple_one_for_one)**:
- Each plugin worker is dynamically created
- Plugin crashes are isolated (transient restart)
- Supervisor manages all plugin instances

### Routing via gproc

Plugin registry uses gproc for O(log N) lookups:
- Plugins registered by name: `{n, l, {plugin, Name}}`
- Plugins registered by type: `{p, l, {plugin_type, Type}}`
- Fast lookups without ETS scans

### Let-it-Crash Philosophy

Plugin failures are handled gracefully:
1. Plugin worker crashes
2. Supervisor detects crash via monitor
3. Registry automatically unregisters dead plugin
4. Manager detects via DOWN message
5. Plugin can be reloaded without system restart

## Future Enhancements

1. **CT Integration Tests**: Full integration tests with example plugins
2. **Plugin Dependencies**: Dependency resolution and loading order
3. **Plugin Versioning**: Semantic versioning and compatibility checks
4. **Plugin Marketplace**: Central registry for community plugins
5. **Plugin CLI Integration**: Direct CLI command registration
6. **Hot Code Upgrade**: Support for code:purge and reload
7. **Plugin Metrics**: OTEL integration for plugin performance tracking
8. **Plugin Permissions**: Fine-grained permission system
9. **Plugin Signing**: GPG signature verification

## Troubleshooting

### Plugin won't load

```erlang
%% Check plugin validation
erlmcp_plugin_loader:validate_plugin(YourPlugin).

%% Common issues:
%% - Missing metadata/0 function
%% - Missing required callbacks
%% - Invalid metadata format
%% - Module not in code path
```

### Plugin crashes on execution

```erlang
%% Plugin errors are isolated
%% Check logs for crash reason
%% Plugin worker will be restarted by supervisor
```

### Plugin not discovered

```erlang
%% Check plugin paths
Paths = erlmcp_plugin_loader:get_plugin_paths().

%% Verify .beam file in path
%% Check file permissions
```

## Contributing

When contributing plugins:

1. Follow behavior contract exactly
2. Implement all required callbacks
3. Add comprehensive tests
4. Document usage in plugin module docs
5. Follow OTP design principles
6. Use gen_server for stateful plugins
7. Never use mocks in tests (use real processes)

## License

Same as erlmcp project.

## Contact

For plugin development questions or contributions, see main erlmcp README.
