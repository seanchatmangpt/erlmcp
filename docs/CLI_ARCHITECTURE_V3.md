# erlmcp CLI Architecture v3.0 - Bleeding Edge Design

**Status**: Design Document (Not Implemented)
**Version**: 3.0.0
**Target**: State-of-the-art CLI with REPL, plugins, and advanced observability
**Author**: Plan Designer Agent
**Date**: 2026-02-01

---

## Executive Summary

This document specifies a comprehensive, modern CLI architecture for erlmcp that unifies all command-line operations under a single `erlmcp` command with:

- **Unified Command Interface**: Single entry point (`erlmcp`) with hierarchical subcommands
- **Interactive REPL Mode**: Full-featured shell with history, completion, and multi-connection support
- **Plugin System**: Extensible architecture for custom validators, formatters, and exporters
- **Rich Observability**: Real-time metrics, tracing, profiling integrated into CLI
- **Smart UX**: Fuzzy completion, suggestions, inline docs, configuration profiles
- **Performance**: <100ms startup, <50MB idle memory, lazy-loaded plugins

---

## Table of Contents

1. [CLI Structure](#1-cli-structure)
2. [Interactive Mode](#2-interactive-mode)
3. [Plugin System](#3-plugin-system)
4. [Observability Layer](#4-observability-layer)
5. [Smart Features](#5-smart-features)
6. [Performance Targets](#6-performance-targets)
7. [File Layout](#7-file-layout)
8. [Module Dependency Graph](#8-module-dependency-graph)
9. [API Contracts](#9-api-contracts)
10. [Example Command Flows](#10-example-command-flows)
11. [Migration Path](#11-migration-path)
12. [Implementation Phases](#12-implementation-phases)

---

## 1. CLI Structure

### 1.1 Command Taxonomy

```
erlmcp
├── validate          # Validation commands (MCP spec compliance)
│   ├── spec          # Validate against MCP specification
│   ├── server <url>  # Validate running MCP server
│   ├── message       # Validate JSON-RPC message
│   └── transport     # Validate transport implementation
│       ├── stdio
│       ├── tcp
│       ├── http
│       ├── ws
│       └── sse
├── run               # Server/client management
│   ├── server        # Start MCP server
│   │   ├── --transport <type>
│   │   ├── --port <port>
│   │   └── --config <file>
│   ├── client        # Start MCP client
│   │   └── --connect <url>
│   └── repl          # Interactive REPL mode
├── test              # Testing commands
│   ├── unit          # Run EUnit tests
│   ├── ct            # Run Common Test suites
│   ├── property      # Run PropEr tests
│   ├── benchmark     # Run benchmarks
│   │   ├── quick
│   │   ├── full
│   │   └── stress
│   └── coverage      # Generate coverage reports
├── deploy            # Deployment operations
│   ├── release       # Create release
│   ├── upgrade       # Hot code upgrade
│   ├── cluster       # Cluster management
│   │   ├── join
│   │   ├── leave
│   │   └── status
│   └── docker        # Docker operations
├── observe           # Observability commands
│   ├── metrics       # View metrics
│   │   ├── live      # Live metrics stream
│   │   ├── export    # Export to file/service
│   │   └── query     # Query historical metrics
│   ├── traces        # Distributed tracing
│   │   ├── live
│   │   └── search
│   ├── dashboard     # Start web dashboard
│   │   ├── --port <port>
│   │   └── --watch
│   ├── health        # Health checks
│   ├── top           # Process viewer (like htop)
│   └── profile       # Profiler
│       ├── cpu
│       ├── memory
│       └── message-queue
├── debug             # Debugging utilities
│   ├── inspect       # Inspect process/state
│   ├── trace         # Debug trace
│   ├── crash-dump    # Analyze crash dumps
│   └── remote-shell  # Remote observer shell
├── plugin            # Plugin management
│   ├── list          # List installed plugins
│   ├── install       # Install plugin
│   ├── uninstall     # Uninstall plugin
│   ├── enable        # Enable plugin
│   ├── disable       # Disable plugin
│   └── info          # Plugin information
├── config            # Configuration management
│   ├── get           # Get config value
│   ├── set           # Set config value
│   ├── list          # List all config
│   ├── profile       # Manage profiles
│   │   ├── list
│   │   ├── create
│   │   ├── switch
│   │   └── delete
│   └── validate      # Validate config file
└── util              # Utility commands
    ├── gen           # Code generation
    │   ├── server
    │   ├── client
    │   ├── transport
    │   └── plugin
    ├── docs          # Documentation
    │   ├── api
    │   ├── protocol
    │   └── examples
    ├── completion    # Shell completion
    │   ├── bash
    │   ├── zsh
    │   └── fish
    ├── version       # Version info
    └── doctor        # System diagnostics
```

### 1.2 Global Flags

All commands support these global flags:

```erlang
-define(GLOBAL_FLAGS, #{
    %% Output control
    verbose => #{
        short => $v,
        long => "verbose",
        help => "Verbose output (can be repeated: -vv, -vvv)",
        type => counter,
        default => 0
    },
    quiet => #{
        short => $q,
        long => "quiet",
        help => "Suppress output except errors",
        type => boolean,
        default => false
    },
    output => #{
        short => $o,
        long => "output",
        help => "Output format: text|json|yaml|table|csv",
        type => {enum, [text, json, yaml, table, csv]},
        default => text
    },

    %% Configuration
    profile => #{
        short => $p,
        long => "profile",
        help => "Configuration profile (dev|test|staging|prod)",
        type => {enum, [dev, test, staging, prod]},
        default => dev
    },
    config => #{
        short => $c,
        long => "config",
        help => "Path to configuration file",
        type => file,
        default => "~/.erlmcp/config.yaml"
    },

    %% Observability
    trace => #{
        long => "trace",
        help => "Enable distributed tracing",
        type => boolean,
        default => false
    },
    profile_perf => #{
        long => "profile",
        help => "Enable performance profiling",
        type => boolean,
        default => false
    },
    benchmark => #{
        long => "benchmark",
        help => "Collect benchmark metrics",
        type => boolean,
        default => false
    },

    %% Advanced
    json => #{
        long => "json",
        help => "JSON output (shorthand for --output json)",
        type => boolean,
        default => false
    },
    no_color => #{
        long => "no-color",
        help => "Disable colored output",
        type => boolean,
        default => false
    },
    timeout => #{
        long => "timeout",
        help => "Command timeout in seconds",
        type => integer,
        default => 300
    },

    %% Help
    help => #{
        short => $h,
        long => "help",
        help => "Show help message",
        type => boolean,
        default => false
    },
    examples => #{
        long => "examples",
        help => "Show command examples",
        type => boolean,
        default => false
    }
}).
```

---

## 2. Interactive Mode

### 2.1 REPL Architecture

The REPL (Read-Eval-Print Loop) provides a full-featured interactive shell for erlmcp operations.

#### 2.1.1 REPL Features

```
erlmcp repl

Welcome to erlmcp REPL v3.0.0
Type 'help' for commands, 'exit' to quit

erlmcp> connect stdio://myserver
✓ Connected to stdio://myserver [session: a4f3d2c1]

erlmcp> tools list
Available tools (3):
  - calculator      Arithmetic operations
  - weather         Get weather data
  - file_ops        File operations

erlmcp> tools call calculator '{"op": "add", "a": 5, "b": 3}'
Result: 8

erlmcp> metrics live --filter throughput,latency
┌─────────────┬────────────┬─────────────┐
│ Metric      │ Value      │ Trend       │
├─────────────┼────────────┼─────────────┤
│ Throughput  │ 42.3k/s    │ ↑ +5%       │
│ Latency P95 │ 23ms       │ ↓ -2%       │
└─────────────┴────────────┴─────────────┘

erlmcp> exit
Goodbye!
```

#### 2.1.2 REPL State Management

```erlang
-record(repl_state, {
    %% Session management
    connections :: #{conn_id() => connection()},
    active_conn :: conn_id() | undefined,

    %% History
    history :: [string()],
    history_idx :: integer(),
    history_max :: integer(),
    history_file :: file:filename(),

    %% Completion
    completion_cache :: #{prefix() => [completion()]},
    completion_timestamp :: integer(),

    %% Context
    context :: #{
        current_profile => atom(),
        env_vars => map(),
        aliases => map(),
        plugins => [plugin_id()]
    },

    %% Display
    output_format :: text | json | yaml | table,
    color_enabled :: boolean(),
    pager_enabled :: boolean(),

    %% Metrics
    metrics_subscription :: pid() | undefined,
    trace_enabled :: boolean()
}).

-record(connection, {
    id :: binary(),
    url :: string(),
    transport :: stdio | tcp | http | ws | sse,
    pid :: pid(),
    capabilities :: map(),
    tools :: [tool()],
    resources :: [resource()],
    session_info :: map(),
    stats :: #{
        requests_sent => integer(),
        responses_received => integer(),
        errors => integer(),
        last_activity => integer()
    }
}).
```

#### 2.1.3 Command History

- **Persistent history**: Saved to `~/.erlmcp/history`
- **Search**: Ctrl-R for reverse search
- **Navigation**: Up/Down arrows, Ctrl-P/Ctrl-N
- **Expansion**: `!!` (last command), `!n` (command n), `!-n` (n commands ago)

```erlang
%% History API
-spec add_to_history(string(), repl_state()) -> repl_state().
-spec search_history(string(), repl_state()) -> [string()].
-spec save_history(repl_state()) -> ok | {error, term()}.
-spec load_history() -> {ok, [string()]} | {error, term()}.
```

#### 2.1.4 Auto-completion

Multi-level completion with fuzzy matching:

```erlang
-record(completion, {
    text :: string(),           % Completion text
    display :: string(),        % Display text (may include color codes)
    description :: string(),    % Short description
    type :: command | flag | arg | plugin | custom,
    priority :: integer(),      % Sort priority (higher = earlier)
    metadata :: map()           % Additional data
}).

%% Completion API
-spec complete(string(), repl_state()) -> [completion()].
-spec complete_command(string()) -> [completion()].
-spec complete_flag(string(), string()) -> [completion()].
-spec complete_arg(string(), string(), pos_integer()) -> [completion()].
```

**Completion Examples**:

```
erlmcp> val<TAB>
validate

erlmcp> validate <TAB>
spec    server    message    transport

erlmcp> validate server std<TAB>
stdio://

erlmcp> observe metrics --fi<TAB>
--filter

erlmcp> plugin ins<TAB>
install

erlmcp> plugin install erl<TAB>
erlmcp-validator-custom    erlmcp-exporter-prometheus
```

### 2.2 Multi-connection Support

The REPL can manage multiple simultaneous connections:

```
erlmcp> connect stdio://server1 --alias s1
✓ Connected to stdio://server1 [session: abc123, alias: s1]

erlmcp> connect http://server2:8080 --alias s2
✓ Connected to http://server2:8080 [session: def456, alias: s2]

erlmcp> connections list
Active connections (2):
  * s1 (stdio://server1)     [session: abc123, tools: 5]
    s2 (http://server2:8080) [session: def456, tools: 3]

erlmcp> use s2
✓ Switched to s2

erlmcp[s2]> tools list
Available tools (3):
  - calculator
  - weather
  - file_ops

erlmcp[s2]> use s1
✓ Switched to s1

erlmcp[s1]> disconnect s2
✓ Disconnected from s2

erlmcp[s1]> connections list
Active connections (1):
  * s1 (stdio://server1) [session: abc123, tools: 5]
```

### 2.3 REPL Commands

Built-in commands (not subcommands):

```erlang
-define(REPL_COMMANDS, #{
    %% Connection management
    connect => #{
        syntax => "connect <url> [--alias <name>]",
        help => "Connect to MCP server",
        examples => [
            "connect stdio://myserver",
            "connect tcp://localhost:8080 --alias dev",
            "connect http://api.example.com --alias prod"
        ]
    },
    disconnect => #{
        syntax => "disconnect [<alias|id>]",
        help => "Disconnect from server (current if no arg)",
        examples => [
            "disconnect",
            "disconnect dev",
            "disconnect abc123"
        ]
    },
    use => #{
        syntax => "use <alias|id>",
        help => "Switch active connection",
        examples => ["use dev", "use abc123"]
    },
    connections => #{
        syntax => "connections list",
        help => "List all connections",
        examples => ["connections list"]
    },

    %% Tools
    tools => #{
        syntax => "tools <list|call|describe>",
        help => "Manage tools",
        examples => [
            "tools list",
            "tools describe calculator",
            "tools call calculator '{\"op\":\"add\",\"a\":5,\"b\":3}'"
        ]
    },

    %% Resources
    resources => #{
        syntax => "resources <list|read|subscribe>",
        help => "Manage resources",
        examples => [
            "resources list",
            "resources read file://data.json",
            "resources subscribe file://logs/*.log"
        ]
    },

    %% Metrics
    metrics => #{
        syntax => "metrics <live|export|query>",
        help => "View metrics",
        examples => [
            "metrics live",
            "metrics live --filter throughput,latency",
            "metrics export --format prometheus",
            "metrics query 'throughput > 1000'"
        ]
    },

    %% History
    history => #{
        syntax => "history [<n>|clear|search <term>]",
        help => "Command history",
        examples => [
            "history",
            "history 10",
            "history clear",
            "history search connect"
        ]
    },

    %% Configuration
    set => #{
        syntax => "set <key> <value>",
        help => "Set REPL configuration",
        examples => [
            "set output json",
            "set color false",
            "set timeout 60"
        ]
    },
    get => #{
        syntax => "get <key>",
        help => "Get REPL configuration",
        examples => ["get output", "get timeout"]
    },

    %% Aliases
    alias => #{
        syntax => "alias <name> <command>",
        help => "Create command alias",
        examples => [
            "alias ll 'tools list'",
            "alias calc 'tools call calculator'"
        ]
    },
    unalias => #{
        syntax => "unalias <name>",
        help => "Remove command alias",
        examples => ["unalias ll"]
    },

    %% Utility
    clear => #{
        syntax => "clear",
        help => "Clear screen",
        examples => ["clear"]
    },
    help => #{
        syntax => "help [<command>]",
        help => "Show help",
        examples => ["help", "help connect", "help tools"]
    },
    exit => #{
        syntax => "exit",
        help => "Exit REPL",
        examples => ["exit"]
    }
}).
```

---

## 3. Plugin System

### 3.1 Plugin Architecture

Plugins extend erlmcp functionality through a well-defined behavior interface.

#### 3.1.1 Plugin Discovery Mechanism

```erlang
%% Plugin discovery paths (in order of precedence)
-define(PLUGIN_PATHS, [
    "~/.erlmcp/plugins",           % User plugins
    "/etc/erlmcp/plugins",         % System plugins
    "./plugins",                   % Project-local plugins
    {env, "ERLMCP_PLUGIN_PATH"}   % Environment variable
]).

%% Plugin metadata (plugin.manifest)
-record(plugin_manifest, {
    id :: binary(),                          % Unique plugin ID
    name :: binary(),                        % Display name
    version :: binary(),                     % Semver version
    description :: binary(),                 % Short description
    author :: binary(),                      % Author name
    license :: binary(),                     % SPDX license
    homepage :: binary(),                    % URL
    erlmcp_version :: binary(),             % Required erlmcp version
    dependencies :: [dependency()],          % Plugin dependencies
    capabilities :: [capability()],          % What the plugin provides
    entry_point :: module(),                 % Main module
    hooks :: [hook()],                       % Hook registrations
    config_schema :: json_schema()           % Configuration schema
}).

-type capability() ::
    {validator, validator_type()} |
    {formatter, format_type()} |
    {exporter, exporter_type()} |
    {command, command_spec()} |
    {transport, transport_name()}.

%% Plugin loading
-spec discover_plugins() -> {ok, [plugin_manifest()]} | {error, term()}.
-spec load_plugin(plugin_id()) -> {ok, plugin()} | {error, term()}.
-spec unload_plugin(plugin_id()) -> ok | {error, term()}.
-spec reload_plugin(plugin_id()) -> ok | {error, term()}.
```

#### 3.1.2 Plugin Behavior Interface

```erlang
-module(erlmcp_plugin).

%% Behavior callbacks
-callback init(Config :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback handle_command(Command :: string(), Args :: [string()], State :: term()) ->
    {ok, Result :: term(), NewState :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback handle_hook(Hook :: atom(), Data :: term(), State :: term()) ->
    {ok, Result :: term(), NewState :: term()} |
    {error, Reason :: term(), State :: term()}.

-callback validate(Type :: atom(), Data :: term(), State :: term()) ->
    {ok, ValidationResult :: map()} |
    {error, ValidationErrors :: [term()]}.

-callback format(Format :: atom(), Data :: term(), State :: term()) ->
    {ok, FormattedData :: binary()} |
    {error, Reason :: term()}.

-callback export(Destination :: term(), Data :: term(), State :: term()) ->
    ok | {error, Reason :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> ok.

-callback info() -> plugin_info().

-optional_callbacks([
    handle_command/3,
    handle_hook/3,
    validate/3,
    format/3,
    export/3
]).

-type plugin_info() :: #{
    name := binary(),
    version := binary(),
    description := binary(),
    capabilities := [capability()]
}.
```

### 3.2 Example Plugins

#### 3.2.1 Custom Validator Plugin

```erlang
-module(erlmcp_plugin_custom_validator).
-behaviour(erlmcp_plugin).

-export([
    init/1,
    validate/3,
    terminate/2,
    info/0
]).

info() ->
    #{
        name => <<"Custom Business Logic Validator">>,
        version => <<"1.0.0">>,
        description => <<"Validates MCP messages against custom business rules">>,
        capabilities => [{validator, custom_business_logic}]
    }.

init(Config) ->
    Rules = maps:get(rules, Config, []),
    {ok, #{rules => Rules}}.

validate(custom_business_logic, Message, #{rules := Rules} = State) ->
    case apply_rules(Rules, Message) of
        {ok, _} ->
            {ok, #{
                status => passed,
                message => <<"All business rules passed">>,
                rules_checked => length(Rules)
            }};
        {error, Violations} ->
            {error, [
                #{
                    status => failed,
                    violations => Violations
                }
            ]}
    end.

terminate(_Reason, _State) ->
    ok.

apply_rules(Rules, Message) ->
    %% Custom validation logic
    ok.
```

#### 3.2.2 Prometheus Exporter Plugin

```erlang
-module(erlmcp_plugin_prometheus_exporter).
-behaviour(erlmcp_plugin).

-export([
    init/1,
    export/3,
    terminate/2,
    info/0
]).

info() ->
    #{
        name => <<"Prometheus Exporter">>,
        version => <<"1.0.0">>,
        description => <<"Export metrics in Prometheus format">>,
        capabilities => [{exporter, prometheus}]
    }.

init(Config) ->
    Port = maps:get(port, Config, 9100),
    {ok, ListenerPid} = start_http_server(Port),
    {ok, #{listener_pid => ListenerPid, port => Port}}.

export(prometheus, Metrics, State) ->
    PrometheusFormat = format_as_prometheus(Metrics),
    broadcast_to_clients(PrometheusFormat, State),
    ok.

terminate(_Reason, #{listener_pid := Pid}) ->
    stop_http_server(Pid),
    ok.

format_as_prometheus(Metrics) ->
    %% Convert to Prometheus text format
    <<"# HELP erlmcp_requests_total Total requests\n",
      "# TYPE erlmcp_requests_total counter\n",
      "erlmcp_requests_total 12345\n">>.

start_http_server(Port) ->
    %% Start Cowboy listener
    {ok, self()}.

stop_http_server(Pid) ->
    ok.

broadcast_to_clients(_Data, _State) ->
    ok.
```

#### 3.2.3 Custom Formatter Plugin

```erlang
-module(erlmcp_plugin_markdown_formatter).
-behaviour(erlmcp_plugin).

-export([
    init/1,
    format/3,
    terminate/2,
    info/0
]).

info() ->
    #{
        name => <<"Markdown Formatter">>,
        version => <<"1.0.0">>,
        description => <<"Format output as GitHub Flavored Markdown">>,
        capabilities => [{formatter, markdown}]
    }.

init(_Config) ->
    {ok, #{}}.

format(markdown, Data, State) when is_map(Data) ->
    Markdown = maps:fold(fun(K, V, Acc) ->
        [Acc, "- **", format_key(K), "**: ", format_value(V), "\n"]
    end, [], Data),
    {ok, iolist_to_binary(Markdown)};
format(markdown, Data, State) when is_list(Data) ->
    %% Format as markdown table
    {ok, format_table_as_markdown(Data)}.

terminate(_Reason, _State) ->
    ok.

format_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
format_key(K) when is_binary(K) -> K.

format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) when is_float(V) -> float_to_binary(V);
format_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_value(V) when is_binary(V) -> V;
format_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).

format_table_as_markdown(Rows) ->
    %% Generate markdown table
    <<"| Column1 | Column2 |\n|---------|---------|">>.
```

### 3.3 Plugin Security Model

```erlang
-record(plugin_security, {
    sandbox :: enabled | disabled,
    permissions :: [permission()],
    resource_limits :: #{
        max_memory => bytes(),
        max_cpu_percent => float(),
        max_processes => integer(),
        max_file_descriptors => integer()
    },
    allowed_modules :: [module()],
    allowed_applications :: [atom()],
    network_access :: #{
        allowed_hosts => [string()],
        allowed_ports => [inet:port_number()]
    }
}).

-type permission() ::
    read_config |
    write_config |
    network_access |
    file_read |
    file_write |
    process_spawn |
    port_open |
    ets_access.

%% Security API
-spec check_permission(plugin_id(), permission()) -> ok | {error, denied}.
-spec enforce_resource_limits(plugin_id()) -> ok.
-spec sandbox_execute(plugin_id(), fun()) -> {ok, Result} | {error, term()}.
```

### 3.4 Plugin Management Commands

```bash
# List plugins
erlmcp plugin list
erlmcp plugin list --enabled
erlmcp plugin list --disabled

# Install plugin
erlmcp plugin install prometheus-exporter
erlmcp plugin install ./my-plugin
erlmcp plugin install https://github.com/user/erlmcp-plugin-xyz

# Plugin info
erlmcp plugin info prometheus-exporter

# Enable/disable
erlmcp plugin enable prometheus-exporter
erlmcp plugin disable prometheus-exporter

# Configure
erlmcp plugin config prometheus-exporter --set port=9100
erlmcp plugin config prometheus-exporter --get port

# Uninstall
erlmcp plugin uninstall prometheus-exporter
```

---

## 4. Observability Layer

### 4.1 Structured Output Formats

All commands support multiple output formats via `--output` flag:

```erlang
-define(OUTPUT_FORMATS, #{
    text => #{
        handler => fun format_text/1,
        content_type => <<"text/plain">>,
        extension => ".txt"
    },
    json => #{
        handler => fun format_json/1,
        content_type => <<"application/json">>,
        extension => ".json"
    },
    yaml => #{
        handler => fun format_yaml/1,
        content_type => <<"application/yaml">>,
        extension => ".yaml"
    },
    table => #{
        handler => fun format_table/1,
        content_type => <<"text/plain">>,
        extension => ".txt"
    },
    csv => #{
        handler => fun format_csv/1,
        content_type => <<"text/csv">>,
        extension => ".csv"
    }
}).
```

**Example Outputs**:

```bash
# Text (default)
erlmcp observe metrics --output text
Metrics:
  Throughput: 42.3k/s
  Latency P95: 23ms
  Error Rate: 0.01%

# JSON
erlmcp observe metrics --output json
{
  "metrics": {
    "throughput": 42300,
    "latency_p95": 23,
    "error_rate": 0.01
  },
  "timestamp": "2026-02-01T12:34:56Z"
}

# YAML
erlmcp observe metrics --output yaml
metrics:
  throughput: 42300
  latency_p95: 23
  error_rate: 0.01
timestamp: 2026-02-01T12:34:56Z

# Table
erlmcp observe metrics --output table
┌────────────┬─────────┐
│ Metric     │ Value   │
├────────────┼─────────┤
│ Throughput │ 42.3k/s │
│ Latency95  │ 23ms    │
│ ErrorRate  │ 0.01%   │
└────────────┴─────────┘

# CSV
erlmcp observe metrics --output csv
metric,value,unit
throughput,42300,msg/s
latency_p95,23,ms
error_rate,0.01,percent
```

### 4.2 Tracing Integration

```bash
# Enable tracing for a command
erlmcp validate server http://api.example.com --trace

# Trace output (shown alongside command output)
TRACE [2026-02-01T12:34:56.123Z] erlmcp_client:init/1 started
TRACE [2026-02-01T12:34:56.145Z] erlmcp_transport_http:connect -> {ok, Connected}
TRACE [2026-02-01T12:34:56.167Z] erlmcp_client:initialize -> {jsonrpc: "2.0", method: "initialize"}
TRACE [2026-02-01T12:34:56.234Z] Response received: {result: {...}}
TRACE [2026-02-01T12:34:56.256Z] Validation completed: passed

# Export traces
erlmcp validate server http://api.example.com --trace --trace-export jaeger

# Trace query in REPL
erlmcp> traces search 'duration > 100ms'
Found 3 traces:
  - [234ms] erlmcp_client:call_tool (trace_id: abc123)
  - [156ms] erlmcp_transport_http:send (trace_id: def456)
  - [123ms] erlmcp_server:handle_request (trace_id: ghi789)
```

### 4.3 Real-time Metrics

```bash
# Live metrics stream (updates every 1s)
erlmcp observe metrics live

┌─────────────────────┬──────────┬─────────┬─────────┐
│ Metric              │ Current  │ Avg 1m  │ Trend   │
├─────────────────────┼──────────┼─────────┼─────────┤
│ Requests/s          │ 1,234    │ 1,150   │ ↑ +7%   │
│ Latency P50         │ 12ms     │ 14ms    │ ↓ -14%  │
│ Latency P95         │ 45ms     │ 52ms    │ ↓ -13%  │
│ Latency P99         │ 89ms     │ 95ms    │ ↓ -6%   │
│ Error Rate          │ 0.02%    │ 0.03%   │ ↓ -33%  │
│ Active Connections  │ 42       │ 38      │ ↑ +11%  │
│ Memory (heap)       │ 234MB    │ 220MB   │ ↑ +6%   │
│ Process Count       │ 156      │ 145     │ ↑ +8%   │
└─────────────────────┴──────────┴─────────┴─────────┘

Press 'q' to quit, 'r' to refresh, 'f' to filter

# Filter metrics
erlmcp observe metrics live --filter throughput,latency

# Export to file
erlmcp observe metrics live --export metrics.json

# Export to Prometheus
erlmcp observe metrics live --export prometheus --port 9100

# Query historical metrics (requires metrics storage)
erlmcp observe metrics query 'throughput > 1000 AND error_rate < 0.01' --since '1h ago'
```

### 4.4 OTEL Integration

```erlang
%% OpenTelemetry configuration for CLI
-record(otel_config, {
    enabled :: boolean(),
    service_name :: binary(),
    service_version :: binary(),
    exporter :: #{
        type => jaeger | zipkin | otlp | honeycomb | datadog,
        endpoint => string(),
        headers => map(),
        batch_size => integer(),
        timeout => integer()
    },
    sampler :: #{
        type => always_on | always_off | trace_id_ratio | parent_based,
        ratio => float()  % For trace_id_ratio
    },
    resource_attributes :: map()
}).

%% Automatic instrumentation for CLI commands
-spec instrument_command(Command :: atom(), Args :: list()) ->
    {ok, Result :: term(), Metrics :: map()}.

%% Example: Automatically traced command
erlmcp validate server http://api.example.com --trace --trace-export jaeger
%% This will:
%% 1. Create a root span for the command
%% 2. Create child spans for each validation step
%% 3. Export to Jaeger
%% 4. Show trace ID in output
```

### 4.5 Performance Profiling

```bash
# CPU profiling
erlmcp test benchmark quick --profile cpu
Running benchmark...
Completed in 5.2s

CPU Profile:
  erlmcp_json_rpc:encode/1          28.4%  (1,234ms)
  erlmcp_transport_tcp:send/2       18.7%  (812ms)
  erlmcp_client:handle_response/2   15.3%  (665ms)
  erlmcp_registry:lookup/1          12.1%  (526ms)
  Other                             25.5%  (1,108ms)

Profile saved to: /tmp/erlmcp-cpu-profile-20260201-123456.fprof

# Memory profiling
erlmcp run server --profile memory --duration 60s
Memory Profile (60s sample):
  Total allocated: 2.4GB
  Peak heap: 156MB

  Top allocators:
    erlmcp_session_ets               42.3%  (1.01GB)
    erlmcp_message_handler           23.1%  (554MB)
    erlmcp_transport_pool            15.8%  (379MB)

Profile saved to: /tmp/erlmcp-memory-profile-20260201-123456.memprof

# Message queue profiling
erlmcp observe profile message-queue --top 10
Top 10 processes by message queue length:
  1. erlmcp_server (12,345 messages)  [pid: <0.234.0>]
  2. erlmcp_client (5,678 messages)   [pid: <0.567.0>]
  3. erlmcp_registry (2,345 messages) [pid: <0.890.0>]
  ...
```

---

## 5. Smart Features

### 5.1 Command Suggestions ("Did you mean?")

```erlang
-module(erlmcp_cli_suggestions).

%% Levenshtein distance-based fuzzy matching
-spec suggest_command(InvalidCommand :: string()) -> [suggestion()].

suggest_command("validat") ->
    [
        #{command => "validate", distance => 1, confidence => 0.95},
        #{command => "valid", distance => 2, confidence => 0.75}
    ];
suggest_command("obserb") ->
    [
        #{command => "observe", distance => 1, confidence => 0.90}
    ].
```

**Example**:

```bash
$ erlmcp validat server
Error: Unknown command 'validat'

Did you mean?
  - validate (95% match)
  - valid (75% match)

Run 'erlmcp --help' for available commands.
```

### 5.2 Fuzzy Completion

```erlang
%% Fuzzy matching for completion
-spec fuzzy_complete(Prefix :: string(), Candidates :: [string()]) ->
    [completion()].

fuzzy_complete("vldt", Candidates) ->
    %% Matches "validate" with score
    [
        #{text => "validate", score => 0.85},
        #{text => "valid", score => 0.60}
    ].
```

**Example**:

```bash
erlmcp> vldt<TAB>
validate (85% match)
valid (60% match)

erlmcp> obsr<TAB>
observe (90% match)
```

### 5.3 Inline Documentation

All commands support `--help` and `--examples`:

```bash
# Command help
erlmcp validate --help

Usage: erlmcp validate <subcommand> [options]

Validate MCP servers, messages, and transports for spec compliance.

Subcommands:
  spec              Validate against MCP specification
  server <url>      Validate running MCP server
  message           Validate JSON-RPC message from stdin or file
  transport <type>  Validate transport implementation

Global Options:
  -v, --verbose     Verbose output
  -o, --output      Output format (text|json|yaml|table)
  --trace           Enable distributed tracing
  -h, --help        Show this help
  --examples        Show command examples

Run 'erlmcp validate <subcommand> --help' for subcommand details.

# Examples
erlmcp validate --examples

Examples:

  # Validate MCP specification compliance
  erlmcp validate spec

  # Validate a running server
  erlmcp validate server stdio://myserver
  erlmcp validate server http://localhost:8080

  # Validate a JSON-RPC message
  echo '{"jsonrpc":"2.0","method":"ping"}' | erlmcp validate message
  erlmcp validate message --file request.json

  # Validate transport implementation
  erlmcp validate transport stdio
  erlmcp validate transport tcp --verbose

  # Generate compliance report
  erlmcp validate spec --output json > compliance-report.json

For more information, visit: https://erlmcp.org/docs/cli/validate
```

### 5.4 Configuration Profiles

```yaml
# ~/.erlmcp/config.yaml

# Default profile
default:
  output_format: text
  verbose: false
  timeout: 300
  color: true
  plugins:
    - prometheus-exporter
    - custom-validator

# Development profile
dev:
  inherits: default
  verbose: true
  trace: true
  metrics:
    enabled: true
    export: prometheus
    port: 9100
  server:
    transport: stdio
    log_level: debug

# Staging profile
staging:
  inherits: default
  output_format: json
  trace: true
  otel:
    enabled: true
    exporter:
      type: jaeger
      endpoint: http://jaeger:14268
  server:
    transport: http
    port: 8080
    log_level: info

# Production profile
prod:
  inherits: default
  output_format: json
  verbose: false
  trace: false
  otel:
    enabled: true
    exporter:
      type: datadog
      endpoint: https://datadog.example.com
  server:
    transport: http
    port: 8080
    log_level: warning
  plugins:
    - prometheus-exporter
    - datadog-exporter
```

**Usage**:

```bash
# Use specific profile
erlmcp --profile dev run server
erlmcp --profile prod validate server http://api.example.com

# Set default profile
erlmcp config profile set dev

# List profiles
erlmcp config profile list

# Create new profile
erlmcp config profile create my-profile --from dev

# Edit profile
erlmcp config profile edit my-profile
```

### 5.5 Smart Defaults

```erlang
%% Smart defaults based on context
-spec smart_defaults(Command :: atom(), Context :: map()) -> map().

smart_defaults(validate, #{profile := dev}) ->
    #{verbose => true, trace => true};
smart_defaults(validate, #{profile := prod}) ->
    #{verbose => false, output => json};
smart_defaults(run, #{profile := test}) ->
    #{transport => stdio, timeout => 30}.
```

---

## 6. Performance Targets

### 6.1 Startup Performance

| Metric | Target | Measurement |
|--------|--------|-------------|
| Cold start (no cache) | <150ms | Time from exec to first prompt |
| Warm start (with cache) | <50ms | With code/beam cache |
| REPL start | <100ms | Time to interactive prompt |
| Plugin discovery | <20ms | Scan all plugin paths |
| Command parse | <5ms | Parse and validate command |

**Optimization Strategies**:

```erlang
%% 1. Lazy module loading
-spec load_module_lazy(Module :: module()) -> ok.
load_module_lazy(Module) ->
    %% Only load when first used
    code:ensure_loaded(Module).

%% 2. Persistent cache
-spec cache_startup_data() -> ok.
cache_startup_data() ->
    %% Cache:
    %% - Plugin manifests
    %% - Command tree
    %% - Completion data
    %% - Configuration
    persistent_term:put(erlmcp_cache, CacheData).

%% 3. Incremental compilation
%% Use rebar3's incremental compilation

%% 4. Beam file cache
%% Pre-compile critical paths
```

### 6.2 Memory Footprint

| State | Target | Measurement |
|-------|--------|-------------|
| Idle (no connections) | <50MB | RSS after startup |
| Single connection | <75MB | RSS with one active connection |
| REPL (no activity) | <60MB | RSS in interactive mode |
| Per-plugin overhead | <5MB | Additional RSS per loaded plugin |

**Memory Optimization**:

```erlang
%% 1. Shared term storage
persistent_term:put(shared_config, Config).

%% 2. Binary reference counting
%% Use binaries for large data

%% 3. ETS for ephemeral data
ets:new(cli_cache, [set, public, {read_concurrency, true}]).

%% 4. Hibernate unused gen_servers
gen_server:cast(self(), hibernate).
```

### 6.3 Command Execution Performance

| Operation | Target | Measurement |
|-----------|--------|-------------|
| Simple command (help) | <10ms | End-to-end |
| Validation command | <500ms | Including network |
| Metrics query | <100ms | Local metrics |
| Plugin execution | <200ms | Average plugin call |
| REPL command | <50ms | Internal command |

### 6.4 Plugin Loading

| Operation | Target | Strategy |
|-----------|--------|----------|
| Plugin discovery | <20ms | Parallel scan + cache |
| Plugin load (lazy) | <100ms | On-demand loading |
| Plugin unload | <50ms | Graceful shutdown |
| Plugin hot reload | <200ms | Code reload |

```erlang
%% Lazy plugin loading
-spec load_plugin_lazy(plugin_id()) -> ok.
load_plugin_lazy(PluginId) ->
    spawn(fun() ->
        %% Load in background
        case load_plugin(PluginId) of
            {ok, _} -> ok;
            {error, Reason} ->
                log_error("Failed to load plugin ~s: ~p", [PluginId, Reason])
        end
    end),
    ok.
```

### 6.5 Benchmarking

```bash
# Built-in benchmarking
erlmcp util benchmark cli

CLI Performance Benchmark
=========================

Startup Performance:
  Cold start:        87ms   ✓ (target: <150ms)
  Warm start:        42ms   ✓ (target: <50ms)
  REPL start:        65ms   ✓ (target: <100ms)

Memory Footprint:
  Idle:              45MB   ✓ (target: <50MB)
  Single connection: 68MB   ✓ (target: <75MB)
  REPL (idle):       52MB   ✓ (target: <60MB)

Command Execution:
  help:              5ms    ✓ (target: <10ms)
  validate spec:     234ms  ✓ (target: <500ms)
  metrics query:     78ms   ✓ (target: <100ms)

All targets met! ✓
```

---

## 7. File Layout

### 7.1 Directory Structure

```
erlmcp/
├── apps/
│   ├── erlmcp_core/
│   ├── erlmcp_transports/
│   ├── erlmcp_observability/
│   ├── erlmcp_validation/
│   └── erlmcp_cli/                    # NEW: CLI application
│       ├── src/
│       │   ├── erlmcp_cli.erl                      # Main entry point
│       │   ├── erlmcp_cli_app.erl                  # OTP application
│       │   ├── erlmcp_cli_sup.erl                  # Supervisor
│       │   │
│       │   ├── core/
│       │   │   ├── erlmcp_cli_parser.erl           # Command parser
│       │   │   ├── erlmcp_cli_dispatcher.erl       # Command dispatcher
│       │   │   ├── erlmcp_cli_config.erl           # Configuration manager
│       │   │   ├── erlmcp_cli_output.erl           # Output formatter
│       │   │   └── erlmcp_cli_error.erl            # Error handler
│       │   │
│       │   ├── repl/
│       │   │   ├── erlmcp_repl.erl                 # REPL server
│       │   │   ├── erlmcp_repl_state.erl           # State management
│       │   │   ├── erlmcp_repl_history.erl         # Command history
│       │   │   ├── erlmcp_repl_completion.erl      # Auto-completion
│       │   │   ├── erlmcp_repl_connection.erl      # Connection manager
│       │   │   └── erlmcp_repl_commands.erl        # Built-in commands
│       │   │
│       │   ├── commands/
│       │   │   ├── erlmcp_cmd_validate.erl         # Validate commands
│       │   │   ├── erlmcp_cmd_run.erl              # Run commands
│       │   │   ├── erlmcp_cmd_test.erl             # Test commands
│       │   │   ├── erlmcp_cmd_deploy.erl           # Deploy commands
│       │   │   ├── erlmcp_cmd_observe.erl          # Observe commands
│       │   │   ├── erlmcp_cmd_debug.erl            # Debug commands
│       │   │   ├── erlmcp_cmd_plugin.erl           # Plugin commands
│       │   │   ├── erlmcp_cmd_config.erl           # Config commands
│       │   │   └── erlmcp_cmd_util.erl             # Utility commands
│       │   │
│       │   ├── plugins/
│       │   │   ├── erlmcp_plugin.erl               # Plugin behavior
│       │   │   ├── erlmcp_plugin_manager.erl       # Plugin manager
│       │   │   ├── erlmcp_plugin_loader.erl        # Plugin loader
│       │   │   ├── erlmcp_plugin_sandbox.erl       # Security sandbox
│       │   │   ├── erlmcp_plugin_registry.erl      # Plugin registry
│       │   │   └── erlmcp_plugin_hooks.erl         # Hook system
│       │   │
│       │   ├── formatters/
│       │   │   ├── erlmcp_formatter_text.erl       # Text formatter
│       │   │   ├── erlmcp_formatter_json.erl       # JSON formatter
│       │   │   ├── erlmcp_formatter_yaml.erl       # YAML formatter
│       │   │   ├── erlmcp_formatter_table.erl      # Table formatter
│       │   │   └── erlmcp_formatter_csv.erl        # CSV formatter
│       │   │
│       │   ├── utils/
│       │   │   ├── erlmcp_cli_suggestions.erl      # Command suggestions
│       │   │   ├── erlmcp_cli_fuzzy.erl            # Fuzzy matching
│       │   │   ├── erlmcp_cli_color.erl            # Color output
│       │   │   ├── erlmcp_cli_table.erl            # Table rendering
│       │   │   ├── erlmcp_cli_progress.erl         # Progress bars
│       │   │   └── erlmcp_cli_pager.erl            # Output pager
│       │   │
│       │   └── observability/
│       │       ├── erlmcp_cli_metrics.erl          # CLI metrics
│       │       ├── erlmcp_cli_tracer.erl           # CLI tracing
│       │       └── erlmcp_cli_profiler.erl         # CLI profiling
│       │
│       ├── test/
│       │   ├── erlmcp_cli_tests.erl
│       │   ├── erlmcp_repl_tests.erl
│       │   ├── erlmcp_plugin_tests.erl
│       │   └── erlmcp_cli_performance_tests.erl
│       │
│       ├── include/
│       │   ├── erlmcp_cli.hrl                      # CLI records/macros
│       │   └── erlmcp_plugin.hrl                   # Plugin interface
│       │
│       └── priv/
│           ├── templates/                          # Code gen templates
│           │   ├── server.erl.tmpl
│           │   ├── client.erl.tmpl
│           │   ├── transport.erl.tmpl
│           │   └── plugin.erl.tmpl
│           ├── completions/                        # Shell completions
│           │   ├── erlmcp.bash
│           │   ├── erlmcp.zsh
│           │   └── erlmcp.fish
│           └── schemas/
│               ├── config.schema.json              # Config schema
│               └── plugin.schema.json              # Plugin manifest schema
│
├── plugins/                                        # Example plugins
│   ├── erlmcp_plugin_prometheus/
│   ├── erlmcp_plugin_custom_validator/
│   └── erlmcp_plugin_markdown_formatter/
│
└── scripts/
    └── build-cli.sh                                # CLI build script
```

### 7.2 New Modules

| Module | Lines (est.) | Purpose |
|--------|--------------|---------|
| `erlmcp_cli.erl` | 150 | Main escript entry point |
| `erlmcp_cli_app.erl` | 50 | OTP application |
| `erlmcp_cli_sup.erl` | 80 | Supervisor |
| `erlmcp_cli_parser.erl` | 400 | Command line parser |
| `erlmcp_cli_dispatcher.erl` | 200 | Command dispatcher |
| `erlmcp_cli_config.erl` | 300 | Configuration manager |
| `erlmcp_cli_output.erl` | 250 | Output formatting |
| `erlmcp_repl.erl` | 600 | REPL server |
| `erlmcp_repl_state.erl` | 200 | REPL state |
| `erlmcp_repl_history.erl` | 250 | Command history |
| `erlmcp_repl_completion.erl` | 400 | Auto-completion |
| `erlmcp_repl_connection.erl` | 300 | Connection manager |
| `erlmcp_cmd_validate.erl` | 500 | Validate commands |
| `erlmcp_cmd_run.erl` | 400 | Run commands |
| `erlmcp_cmd_test.erl` | 300 | Test commands |
| `erlmcp_cmd_observe.erl` | 500 | Observe commands |
| `erlmcp_cmd_plugin.erl` | 350 | Plugin commands |
| `erlmcp_plugin_manager.erl` | 600 | Plugin management |
| `erlmcp_plugin_loader.erl` | 400 | Plugin loading |
| `erlmcp_plugin_sandbox.erl` | 500 | Security sandbox |
| `erlmcp_formatter_*.erl` | 200 each | Output formatters (5 modules) |
| `erlmcp_cli_suggestions.erl` | 200 | Command suggestions |
| `erlmcp_cli_fuzzy.erl` | 150 | Fuzzy matching |
| `erlmcp_cli_table.erl` | 300 | Table rendering |
| **Total** | **~8,030** | 26 new modules |

### 7.3 Integration Points

```erlang
%% Integration with existing modules

%% 1. Validation CLI wraps existing validators
-module(erlmcp_cmd_validate).
-export([execute/2]).

execute(spec, _Args) ->
    erlmcp_validate_cli:validate_spec().

%% 2. Observe CLI wraps dashboard/metrics
-module(erlmcp_cmd_observe).
-export([execute/2]).

execute({metrics, live}, _Args) ->
    {ok, Pid} = erlmcp_dashboard_server:start_link(),
    stream_metrics(Pid).

%% 3. Run CLI wraps server/client
-module(erlmcp_cmd_run).
-export([execute/2]).

execute(server, #{transport := Transport, port := Port}) ->
    erlmcp_server:start_link([{transport, Transport}, {port, Port}]).
```

### 7.4 Dependencies

**New Dependencies** (to add to `rebar.config`):

```erlang
{deps, [
    % ... existing deps ...

    % CLI-specific deps
    {getopt, "1.0.2"},           % Command-line parsing
    {yamler, "0.8.0"},           % YAML support
    {erltable, "0.1.0"},         % Table rendering
    {fuzzy, "1.0.0"}             % Fuzzy matching (or implement custom)
]}.
```

---

## 8. Module Dependency Graph

```
                                    ┌─────────────────┐
                                    │  erlmcp_cli     │ (Main entry)
                                    │  (escript)      │
                                    └────────┬────────┘
                                             │
                         ┌───────────────────┼───────────────────┐
                         │                   │                   │
                    ┌────▼─────┐      ┌─────▼──────┐     ┌─────▼─────┐
                    │  Parser  │      │ Dispatcher │     │  Config   │
                    └────┬─────┘      └─────┬──────┘     └─────┬─────┘
                         │                  │                   │
                         └──────────┬───────┴───────────────────┘
                                    │
                    ┌───────────────┼───────────────┐
                    │               │               │
            ┌───────▼───────┐  ┌───▼────┐   ┌─────▼─────┐
            │  REPL Server  │  │Commands│   │  Plugins  │
            └───┬───────┬───┘  └───┬────┘   └─────┬─────┘
                │       │          │              │
        ┌───────┘       └──┐       │       ┌──────┴──────┐
        │                  │       │       │             │
    ┌───▼────┐      ┌──────▼───┐  │   ┌───▼──────┐  ┌───▼────────┐
    │History │      │Completion│  │   │ Manager  │  │ Sandbox    │
    └────────┘      └──────────┘  │   └──────────┘  └────────────┘
                                  │
                    ┌─────────────┼─────────────┬──────────────┐
                    │             │             │              │
               ┌────▼────┐   ┌────▼────┐   ┌────▼────┐   ┌────▼────┐
               │Validate │   │  Run    │   │ Observe │   │  Test   │
               └────┬────┘   └────┬────┘   └────┬────┘   └────┬────┘
                    │             │             │             │
            ┌───────┴──────┐      │     ┌───────┴──────┐      │
            │              │      │     │              │      │
    ┌───────▼────┐  ┌──────▼──┐  │  ┌──▼─────┐  ┌─────▼───┐  │
    │Spec Parser │  │Protocol │  │  │Metrics │  │Dashboard│  │
    └────────────┘  │Validator│  │  │ Server │  │ Server  │  │
                    └─────────┘  │  └────────┘  └─────────┘  │
                                 │                            │
                         ┌───────▼─────┐            ┌─────────▼──────┐
                         │   Server    │            │  EUnit/CT      │
                         │   Client    │            └────────────────┘
                         └─────────────┘
                                 │
                    ┌────────────┼────────────┐
                    │            │            │
            ┌───────▼──────┐ ┌───▼────┐ ┌────▼─────┐
            │  Transport   │ │Registry│ │ Session  │
            │  (stdio/tcp/ │ └────────┘ └──────────┘
            │   http/ws)   │
            └──────────────┘

Legend:
  ━━━  Strong dependency (required)
  ───  Weak dependency (optional/runtime)
```

---

## 9. API Contracts

### 9.1 Command Behavior

```erlang
-module(erlmcp_command).

%% Command behavior for all CLI commands
-callback execute(Args :: map()) ->
    {ok, Result :: term()} |
    {error, Reason :: term()}.

-callback help() -> string().

-callback examples() -> [example()].

-optional_callbacks([help/0, examples/0]).

-type example() :: #{
    description := string(),
    command := string(),
    expected_output => string()
}.
```

### 9.2 Plugin Behavior (Detailed)

```erlang
-module(erlmcp_plugin).

%% Plugin lifecycle
-callback init(Config :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> ok.

%% Plugin information
-callback info() -> plugin_info().

-type plugin_info() :: #{
    name := binary(),
    version := binary(),
    description := binary(),
    author := binary(),
    license := binary(),
    capabilities := [capability()]
}.

%% Command handling (if plugin provides commands)
-callback handle_command(Command :: string(), Args :: [string()], State :: term()) ->
    {ok, Result :: term(), NewState :: term()} |
    {error, Reason :: term(), State :: term()}.

%% Hook handling (if plugin registers hooks)
-callback handle_hook(Hook :: atom(), Data :: term(), State :: term()) ->
    {ok, Result :: term(), NewState :: term()} |
    {error, Reason :: term(), State :: term()}.

%% Validation (if plugin provides validators)
-callback validate(Type :: atom(), Data :: term(), State :: term()) ->
    {ok, ValidationResult :: map()} |
    {error, ValidationErrors :: [term()]}.

%% Formatting (if plugin provides formatters)
-callback format(Format :: atom(), Data :: term(), State :: term()) ->
    {ok, FormattedData :: binary()} |
    {error, Reason :: term()}.

%% Exporting (if plugin provides exporters)
-callback export(Destination :: term(), Data :: term(), State :: term()) ->
    ok | {error, Reason :: term()}.

%% Configuration schema (if plugin accepts configuration)
-callback config_schema() -> json_schema().

-optional_callbacks([
    handle_command/3,
    handle_hook/3,
    validate/3,
    format/3,
    export/3,
    config_schema/0
]).
```

### 9.3 Formatter Behavior

```erlang
-module(erlmcp_formatter).

-callback format(Data :: term(), Options :: map()) ->
    {ok, FormattedBinary :: binary()} |
    {error, Reason :: term()}.

-callback content_type() -> binary().

-callback file_extension() -> string().
```

### 9.4 REPL Command Behavior

```erlang
-module(erlmcp_repl_command).

-callback execute(Args :: [string()], State :: repl_state()) ->
    {ok, Output :: iodata(), NewState :: repl_state()} |
    {error, Reason :: term(), State :: repl_state()}.

-callback help() -> string().

-callback complete(Prefix :: string(), State :: repl_state()) ->
    [completion()].

-optional_callbacks([help/0, complete/2]).
```

---

## 10. Example Command Flows

### 10.1 Flow: `erlmcp validate server http://api.example.com`

```
1. User executes command
   └─> erlmcp_cli:main/1
       │
2. Parse command line
   └─> erlmcp_cli_parser:parse/1
       │   Input: ["validate", "server", "http://api.example.com"]
       │   Output: #{command => validate,
       │              subcommand => server,
       │              args => #{"url" => "http://api.example.com"},
       │              flags => #{}}
       │
3. Load configuration (profile, global flags)
   └─> erlmcp_cli_config:load/1
       │   Profile: dev (default)
       │   Merged with global flags
       │
4. Dispatch to command handler
   └─> erlmcp_cli_dispatcher:dispatch/2
       │   Route: validate/server
       │   Handler: erlmcp_cmd_validate:execute/2
       │
5. Execute validation
   └─> erlmcp_cmd_validate:execute(server, #{url => "..."})
       │
       ├─> erlmcp_validate_cli:validate_running_server/2
       │   │
       │   ├─> Start validators
       │   │   └─> erlmcp_protocol_validator:run/1
       │   │   └─> erlmcp_transport_validator:run/1
       │   │   └─> erlmcp_security_validator:run/1
       │   │
       │   ├─> Collect results
       │   │
       │   └─> Return validation report
       │
6. Format output
   └─> erlmcp_cli_output:format/2
       │   Format: text (from config/flags)
       │   Output: Formatted validation report
       │
7. Print to stdout
   └─> io:format/2
       │
8. Exit with code
   └─> erlang:halt/1
       │   Code: 0 (success) or 1 (failure)
```

### 10.2 Flow: `erlmcp repl` (Interactive Session)

```
1. User executes: erlmcp repl
   └─> erlmcp_cli:main/1
       │
2. Parse command
   └─> erlmcp_cli_parser:parse/1
       │   Output: #{command => repl}
       │
3. Start REPL server
   └─> erlmcp_repl:start/1
       │
       ├─> Initialize state
       │   └─> erlmcp_repl_state:init/1
       │       │   - Load history from ~/.erlmcp/history
       │       │   - Load configuration
       │       │   - Initialize completion cache
       │
       ├─> Print welcome message
       │
       └─> Enter read-eval-print loop
           │
           Loop:
           ├─> Read command
           │   └─> io:get_line("erlmcp> ")
           │       │   - Handle arrow keys (history navigation)
           │       │   - Handle Tab (completion)
           │       │   - Handle Ctrl-R (search)
           │
           ├─> Parse command
           │   └─> erlmcp_repl_commands:parse/1
           │       │   Example: "connect stdio://myserver --alias s1"
           │       │   Output: #{cmd => connect, args => [...]}
           │
           ├─> Execute command
           │   └─> erlmcp_repl_commands:execute/2
           │       │
           │       Case: connect command
           │       └─> erlmcp_repl_connection:connect/2
           │           │   - Start MCP client
           │           │   - Store in state
           │           │   - Set as active connection
           │
           ├─> Add to history
           │   └─> erlmcp_repl_history:add/2
           │
           ├─> Print result
           │   └─> erlmcp_cli_output:format/2
           │
           └─> Repeat loop until 'exit' command

4. Exit REPL
   └─> erlmcp_repl:stop/1
       │   - Save history
       │   - Close all connections
       │   - Cleanup resources
```

### 10.3 Flow: `erlmcp observe metrics live --trace`

```
1. Parse command
   └─> #{command => observe,
         subcommand => metrics,
         action => live,
         flags => #{trace => true}}
   │
2. Setup tracing (due to --trace flag)
   └─> erlmcp_cli_tracer:enable/1
       │   - Initialize OpenTelemetry
       │   - Create root span for command
       │
3. Execute observe/metrics/live
   └─> erlmcp_cmd_observe:execute({metrics, live}, #{trace => true})
       │
       ├─> Subscribe to metrics
       │   └─> erlmcp_metrics_server:subscribe/1
       │       │   Returns: stream_pid
       │
       ├─> Setup display
       │   └─> erlmcp_cli_table:init/1
       │       │   - Initialize table renderer
       │       │   - Setup terminal for live updates
       │
       └─> Enter display loop
           │
           Loop (every 1s):
           ├─> Receive metrics
           │   └─> receive {metrics, Data} -> Data end
           │
           ├─> Create span (if tracing)
           │   └─> otel:with_span("render_metrics", fun() -> ... end)
           │
           ├─> Render table
           │   └─> erlmcp_cli_table:render/2
           │       │   - Calculate trends
           │       │   - Format values
           │       │   - Add colors
           │       │   - Print to terminal
           │
           ├─> Check for user input
           │   └─> Check for 'q' (quit), 'r' (refresh), 'f' (filter)
           │
           └─> Repeat until user quits

4. Cleanup
   └─> Unsubscribe from metrics
       └─> Restore terminal
           └─> Export traces (if --trace)
               └─> erlmcp_cli_tracer:export/1
```

### 10.4 Flow: Plugin Installation

```
1. User: erlmcp plugin install prometheus-exporter
   │
2. Parse command
   └─> #{command => plugin, subcommand => install,
         args => ["prometheus-exporter"]}
   │
3. Execute plugin install
   └─> erlmcp_cmd_plugin:execute(install, #{name => "prometheus-exporter"})
       │
       ├─> Resolve plugin source
       │   └─> erlmcp_plugin_manager:resolve/1
       │       │   Options:
       │       │   - Local path: ./plugins/prometheus-exporter
       │       │   - Git repo: https://github.com/user/erlmcp-plugin-prometheus
       │       │   - Registry: erlmcp registry (future)
       │       │
       │       │   Result: {git, "https://github.com/erlmcp/erlmcp-plugin-prometheus"}
       │
       ├─> Download plugin
       │   └─> erlmcp_plugin_manager:download/2
       │       │   - Clone git repo to /tmp/erlmcp-plugin-xyz
       │       │   - Verify signature (if available)
       │
       ├─> Validate plugin
       │   └─> erlmcp_plugin_manager:validate/1
       │       │
       │       ├─> Load manifest (plugin.manifest)
       │       │   └─> Parse and validate against schema
       │       │
       │       ├─> Check erlmcp version compatibility
       │       │
       │       ├─> Verify dependencies
       │       │
       │       └─> Run security scan
       │           └─> Check for dangerous patterns
       │
       ├─> Compile plugin
       │   └─> rebar3:compile/1
       │       │   - Compile .erl files
       │       │   - Generate .beam files
       │
       ├─> Install plugin
       │   └─> erlmcp_plugin_manager:install/2
       │       │   - Copy to ~/.erlmcp/plugins/prometheus-exporter/
       │       │   - Register in plugin registry
       │       │   - Update config
       │
       └─> Enable plugin (optional, auto-enable by default)
           └─> erlmcp_plugin_manager:enable/1
               │   - Load plugin module
               │   - Call init/1
               │   - Register capabilities
               │   - Register hooks

5. Output success message
   └─> "✓ Plugin 'prometheus-exporter' installed successfully"
```

---

## 11. Migration Path

### 11.1 Phase 0: Compatibility Layer (Week 1)

**Goal**: Ensure existing `erlmcp_validate_cli` continues to work.

```erlang
%% Create wrapper that delegates to new CLI
-module(erlmcp_validate_cli_compat).

validate_spec() ->
    %% Delegate to new CLI
    erlmcp_cli:main(["validate", "spec"]).

validate_transport(Transport) ->
    erlmcp_cli:main(["validate", "transport", atom_to_list(Transport)]).
```

**Changes**:
- Keep `erlmcp_validate_cli.erl` as-is
- Add new `erlmcp_cli` app alongside
- Add compatibility shim if needed

**Testing**:
```bash
# Old way still works
./scripts/build-escript.sh

# New way also works
rebar3 escriptize  # builds new erlmcp CLI
./erlmcp validate spec
```

### 11.2 Phase 1: Core CLI Infrastructure (Week 2-3)

**Deliverables**:
- `erlmcp_cli` app structure
- Basic command parser
- Command dispatcher
- Output formatters (text, json)
- Migrate `validate` commands to new structure

**Migration Steps**:

1. **Create app structure**:
```bash
mkdir -p apps/erlmcp_cli/src/{core,commands,formatters,utils}
```

2. **Implement core modules**:
   - `erlmcp_cli.erl` - Main entry point
   - `erlmcp_cli_parser.erl` - Command parser
   - `erlmcp_cli_dispatcher.erl` - Dispatcher
   - `erlmcp_cli_output.erl` - Output formatting

3. **Migrate validate commands**:
   - Extract logic from `erlmcp_validate_cli.erl`
   - Create `erlmcp_cmd_validate.erl`
   - Implement subcommands: spec, server, message, transport

4. **Testing**:
```bash
# Run existing tests
rebar3 eunit --module=erlmcp_validate_cli_tests

# Run new CLI tests
rebar3 eunit --module=erlmcp_cmd_validate_tests

# Integration test
./erlmcp validate spec --output json
```

### 11.3 Phase 2: REPL Mode (Week 4-5)

**Deliverables**:
- REPL server with basic functionality
- Command history
- Auto-completion (basic)
- Multi-connection support

**Implementation**:
1. Implement `erlmcp_repl.erl`
2. Add history management
3. Add basic completion
4. Add connection management

**Testing**:
```bash
./erlmcp repl
erlmcp> connect stdio://myserver
erlmcp> tools list
erlmcp> exit
```

### 11.4 Phase 3: Plugin System (Week 6-7)

**Deliverables**:
- Plugin behavior and manager
- Plugin discovery
- Example plugins (prometheus exporter, custom validator)
- Security sandbox (basic)

**Implementation**:
1. Define plugin behavior
2. Implement plugin manager
3. Create example plugins
4. Add `plugin` commands

**Testing**:
```bash
./erlmcp plugin list
./erlmcp plugin install ./plugins/erlmcp_plugin_prometheus
./erlmcp plugin enable prometheus-exporter
./erlmcp observe metrics export --format prometheus
```

### 11.5 Phase 4: Advanced Features (Week 8-10)

**Deliverables**:
- Fuzzy completion
- Command suggestions
- Advanced output formats (table, yaml, csv)
- Configuration profiles
- Performance optimization

### 11.6 Phase 5: Observability Integration (Week 11-12)

**Deliverables**:
- `observe` commands fully integrated
- Live metrics dashboard
- Tracing integration
- Profiling tools
- `debug` commands

### 11.7 Phase 6: Polish & Documentation (Week 13-14)

**Deliverables**:
- Comprehensive documentation
- Shell completion scripts
- Inline help and examples
- Performance benchmarks
- Migration guide

---

## 12. Implementation Phases

### 12.1 MVP (Minimum Viable Product) - 4 weeks

**Scope**:
- Core CLI infrastructure
- Basic command parser and dispatcher
- `validate` commands (migrated from existing CLI)
- `run server` and `run client` commands
- Text and JSON output formats
- Basic error handling

**Excluded from MVP**:
- REPL mode
- Plugin system
- Advanced formatters
- Smart features (fuzzy completion, suggestions)
- Advanced observability

**Success Criteria**:
- All existing `erlmcp_validate_cli` functionality works via new CLI
- `erlmcp validate spec` passes all tests
- Startup time <150ms
- Help system works
- Exit codes correct

**Deliverables**:
```
apps/erlmcp_cli/
├── src/
│   ├── erlmcp_cli.erl
│   ├── core/
│   │   ├── erlmcp_cli_parser.erl
│   │   ├── erlmcp_cli_dispatcher.erl
│   │   ├── erlmcp_cli_config.erl
│   │   └── erlmcp_cli_output.erl
│   ├── commands/
│   │   ├── erlmcp_cmd_validate.erl
│   │   └── erlmcp_cmd_run.erl
│   └── formatters/
│       ├── erlmcp_formatter_text.erl
│       └── erlmcp_formatter_json.erl
└── test/
    └── erlmcp_cli_tests.erl
```

### 12.2 Phase 2: REPL & Connections - 3 weeks

**Scope**:
- REPL server
- Command history
- Basic auto-completion
- Multi-connection management
- REPL built-in commands

**Success Criteria**:
- REPL starts in <100ms
- History persists across sessions
- Can manage multiple connections
- Tab completion works for commands

**Deliverables**:
```
apps/erlmcp_cli/src/repl/
├── erlmcp_repl.erl
├── erlmcp_repl_state.erl
├── erlmcp_repl_history.erl
├── erlmcp_repl_completion.erl
├── erlmcp_repl_connection.erl
└── erlmcp_repl_commands.erl
```

### 12.3 Phase 3: Plugin System - 3 weeks

**Scope**:
- Plugin behavior and interface
- Plugin manager and loader
- Plugin discovery
- Security sandbox (basic)
- 3 example plugins
- `plugin` commands

**Success Criteria**:
- Can install, enable, disable, uninstall plugins
- Plugins can extend commands, validators, formatters
- Security sandbox prevents unauthorized operations
- Plugin discovery <20ms

**Deliverables**:
```
apps/erlmcp_cli/src/plugins/
├── erlmcp_plugin.erl
├── erlmcp_plugin_manager.erl
├── erlmcp_plugin_loader.erl
├── erlmcp_plugin_sandbox.erl
└── erlmcp_plugin_registry.erl

plugins/
├── erlmcp_plugin_prometheus/
├── erlmcp_plugin_custom_validator/
└── erlmcp_plugin_markdown_formatter/
```

### 12.4 Phase 4: Smart Features - 2 weeks

**Scope**:
- Fuzzy completion
- Command suggestions ("Did you mean?")
- Advanced formatters (YAML, Table, CSV)
- Configuration profiles
- Inline documentation and examples

**Success Criteria**:
- Fuzzy matching suggests correct command
- Typos show helpful suggestions
- Table output renders correctly
- Config profiles work seamlessly

**Deliverables**:
```
apps/erlmcp_cli/src/utils/
├── erlmcp_cli_suggestions.erl
├── erlmcp_cli_fuzzy.erl
├── erlmcp_cli_table.erl
└── erlmcp_cli_color.erl

apps/erlmcp_cli/src/formatters/
├── erlmcp_formatter_yaml.erl
├── erlmcp_formatter_table.erl
└── erlmcp_formatter_csv.erl
```

### 12.5 Phase 5: Observability - 2 weeks

**Scope**:
- `observe` command suite
- Live metrics dashboard
- Tracing integration
- Profiling tools
- `debug` commands

**Success Criteria**:
- Live metrics update every 1s
- Tracing exports to Jaeger/Zipkin
- CPU/memory profiling works
- Health checks execute quickly

**Deliverables**:
```
apps/erlmcp_cli/src/commands/
├── erlmcp_cmd_observe.erl
└── erlmcp_cmd_debug.erl

apps/erlmcp_cli/src/observability/
├── erlmcp_cli_metrics.erl
├── erlmcp_cli_tracer.erl
└── erlmcp_cli_profiler.erl
```

### 12.6 Phase 6: Polish & Production - 2 weeks

**Scope**:
- Performance optimization
- Comprehensive documentation
- Shell completion scripts
- Migration guide
- Benchmarks and performance tests
- Final testing and bug fixes

**Success Criteria**:
- All performance targets met
- Documentation complete
- Shell completions work (bash, zsh, fish)
- Migration guide validated

**Deliverables**:
```
docs/
├── CLI_USER_GUIDE.md
├── CLI_PLUGIN_DEVELOPMENT.md
└── CLI_MIGRATION_GUIDE.md

apps/erlmcp_cli/priv/completions/
├── erlmcp.bash
├── erlmcp.zsh
└── erlmcp.fish

apps/erlmcp_cli/test/
└── erlmcp_cli_performance_tests.erl
```

---

## Summary

This architecture provides a **bleeding-edge CLI** for erlmcp with:

✅ **Unified Command Structure**: Hierarchical, intuitive, extensible
✅ **Interactive REPL**: Full-featured shell with history, completion, multi-connection
✅ **Plugin System**: Secure, extensible, with example implementations
✅ **Rich Observability**: Metrics, tracing, profiling deeply integrated
✅ **Smart UX**: Fuzzy completion, suggestions, inline docs, profiles
✅ **Performance**: <100ms startup, <50MB idle, lazy-loaded plugins
✅ **Clean Architecture**: Well-structured, testable, maintainable

**Total Estimated Effort**: 16 weeks (4 months) for full implementation

**Lines of Code**: ~8,000 new lines across 26 modules

**Integration**: Seamless with existing erlmcp ecosystem

**Migration**: Incremental, backward-compatible

---

**Next Steps**: Review this design, adjust priorities, and begin MVP implementation (Phase 12.1).

