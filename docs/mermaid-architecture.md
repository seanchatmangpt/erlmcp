# Mermaid Integration Architecture for erlmcp

## Executive Summary

This document specifies the complete OTP-based architecture for integrating Mermaid diagram generation into erlmcp. The design follows erlmcp's supervision tree principles, implements all Mermaid diagram types, provides comprehensive error handling, and maintains transport polymorphism across stdio, HTTP, and WebSocket interfaces.

**Design Principles:**
- **OTP Compliance** - gen_server behaviors with proper supervision
- **Transport Independence** - Works across all erlmcp transports
- **Fault Isolation** - Mermaid failures don't affect core MCP protocol
- **Extensibility** - Easy to add new diagram types or rendering backends
- **Performance** - Caching layer for frequently generated diagrams

## System Architecture

### Mermaid Integration Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                         erlmcp Umbrella                          │
├──────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │           Mermaid Integration Layer                     │    │
│  ├─────────────────────────────────────────────────────────┤    │
│  │  erlmcp_mermaid_handler (gen_server)                    │    │
│  │  - JSON-RPC method: "diagram/generate"                  │    │
│  │  - JSON-RPC method: "diagram/validate"                  │    │
│  │  - JSON-RPC method: "diagram/from_template"             │    │
│  └────────────┬────────────────────────────────────────────┘    │
│               │                                                  │
│               ▼                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │      erlmcp_mermaid_registry (diagram type registry)    │    │
│  │  - Flowchart, Sequence, Gantt, Class, State, ER, etc.   │    │
│  └────────────┬────────────────────────────────────────────┘    │
│               │                                                  │
│               ▼                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │    erlmcp_mermaid_parser (syntax validation)            │    │
│  │  - Mermaid syntax parsing                                │    │
│  │  - Error location detection                              │    │
│  │  - Safe subset enforcement                               │    │
│  └────────────┬────────────────────────────────────────────┘    │
│               │                                                  │
│               ▼                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │      erlmcp_mermaid_renderer (SVG generation)            │    │
│  │  - External Mermaid CLI integration                      │    │
│  │  - WASM-based rendering (optional)                       │    │
│  │  - Fallback to hosted API                                │    │
│  └────────────┬────────────────────────────────────────────┘    │
│               │                                                  │
│               ▼                                                  │
│  ┌─────────────────────────────────────────────────────────┐    │
│  │       erlmcp_mermaid_cache (ETS-based caching)          │    │
│  │  - Hash-based cache keys                                │    │
│  │  - TTL-based invalidation                               │    │
│  │  - LRU eviction policy                                  │    │
│  └─────────────────────────────────────────────────────────┘    │
│                                                                  │
└──────────────────────────────────────────────────────────────────┘
```

## Supervision Tree

### Mermaid Supervision Hierarchy

```
erlmcp_sup (one_for_one)
├── erlmcp_core_sup (one_for_one)
│   ├── [existing core components...]
│   └── erlmcp_mermaid_sup (one_for_one) ← NEW: Mermaid supervisor
│       ├── erlmcp_mermaid_registry (gen_server) - Diagram type registry
│       ├── erlmcp_mermaid_parser_sup (one_for_one) - Parser workers
│       │   └── erlmcp_mermaid_parser (simple_one_for_one) - Dynamic parser instances
│       ├── erlmcp_mermaid_renderer_sup (one_for_one) - Renderer workers
│       │   ├── erlmcp_mermaid_renderer_cli (gen_server) - CLI backend
│       │   ├── erlmcp_mermaid_renderer_wasm (gen_server) - WASM backend [optional]
│       │   └── erlmcp_mermaid_renderer_api (gen_server) - Hosted API fallback
│       └── erlmcp_mermaid_cache (gen_server) - ETS cache manager
```

**Supervision Strategy Rationale:**

| Supervisor | Strategy | Rationale |
|------------|----------|-----------|
| `erlmcp_mermaid_sup` | `one_for_one` | Mermaid components fail independently - parser crash doesn't affect cache |
| `erlmcp_mermaid_parser_sup` | `one_for_one` | Multiple parser strategies can coexist |
| `erlmcp_mermaid_renderer_sup` | `one_for_one` | Renderer backends are independent - CLI crash doesn't affect WASM |
| `erlmcp_mermaid_parser` | `simple_one_for_one` | Dynamic worker pool for parallel parsing |

**Failure Isolation Guarantees:**

| Component Crash | Restart Scope | Recovery Time | Impact |
|-----------------|---------------|---------------|--------|
| Mermaid Registry | Registry only | ~500ms | New diagram requests fail; cache continues serving |
| Parser Workers | That parser only | ~1s | Other parsers continue; requests queue |
| Renderer (CLI) | CLI renderer only | ~2s | WASM/API fallback activated |
| Cache Manager | Cache only | ~500ms | All diagrams render without cache |

## Module Specifications

### 1. erlmcp_mermaid_sup

**Purpose:** Top-level supervisor for all Mermaid components

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_sup.erl`

**Behavior:** `supervisor`

**Child Specs:**

```erlang
init([]) ->
    ChildSpecs = [
        #{
            id => mermaid_registry,
            start => {erlmcp_mermaid_registry, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_mermaid_registry]
        },
        #{
            id => mermaid_parser_sup,
            start => {erlmcp_mermaid_parser_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_mermaid_parser_sup]
        },
        #{
            id => mermaid_renderer_sup,
            start => {erlmcp_mermaid_renderer_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor,
            modules => [erlmcp_mermaid_renderer_sup]
        },
        #{
            id => mermaid_cache,
            start => {erlmcp_mermaid_cache, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_mermaid_cache]
        }
    ],
    {ok, {{one_for_one, 10, 60}, ChildSpecs}}.
```

---

### 2. erlmcp_mermaid_registry

**Purpose:** Registry of all supported Mermaid diagram types with metadata

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_registry.erl`

**Behavior:** `gen_server`

**State Record:**

```erlang
-record(state, {
    diagram_types :: #{binary() => #mermaid_diagram_type{}},
    aliases :: #{binary() => binary()},  % name -> canonical_name
    version :: binary()
}).

-record(mermaid_diagram_type, {
    name :: binary(),              % e.g., <<"flowchart">>
    category :: binary(),          % e.g., <<"diagram">>
    syntax :: binary(),            % BNF grammar reference
    examples :: [binary()],        % Example snippets
    validation_rules :: [binary()], % Specific validation rules
    requires_wasm :: boolean(),    % True if WASM-only features
    max_complexity :: integer()    % Max nodes/edges for safety
}).
```

**API:**

```erlang
%% Register a new diagram type
-spec register_diagram_type(#mermaid_diagram_type{}) -> ok | {error, already_exists}.

%% Get diagram type metadata
-spec get_diagram_type(binary()) -> {ok, #mermaid_diagram_type{}} | {error, not_found}.

%% List all supported diagram types
-spec list_diagram_types() -> [binary()].

%% Check if diagram type is supported
-spec is_supported(binary()) -> boolean().

%% Validate diagram syntax against type rules
-spec validate_diagram(binary(), binary()) ->
    {ok, valid} | {error, #{reason := binary(), line := integer(), column := integer()}}.
```

**Supported Diagram Types (Initial):**

```erlang
default_diagram_types() ->
    [
        #mermaid_diagram_type{
            name = <<"flowchart">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"graph TD; A-->B; B-->C;">>,
                <<"graph LR; start[Start]-->stop[Stop];">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>],
            requires_wasm = false,
            max_complexity = 1000
        },
        #mermaid_diagram_type{
            name = <<"sequenceDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/sequenceDiagram.html">>,
            examples = [
                <<"sequenceDiagram; A->>B: Hello; B-->>A: Hi;">>
            ],
            validation_rules = [<<"valid_participants">>, <<"valid_messages">>],
            requires_wasm = false,
            max_complexity = 500
        },
        #mermaid_diagram_type{
            name = <<"gantt">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/gantt.html">>,
            examples = [
                <<"gantt; title Project; section Phase 1; Task 1: 2024-01-01, 7d;">>
            ],
            validation_rules = [<<"valid_dates">>, <<"valid_durations">>],
            requires_wasm = false,
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"classDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/classDiagram.html">>,
            examples = [
                <<"classDiagram; Animal <|-- Duck;">>
            ],
            validation_rules = [<<"valid_class_names">>, <<"valid_relationships">>],
            requires_wasm = false,
            max_complexity = 300
        },
        #mermaid_diagram_type{
            name = <<"stateDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/stateDiagram.html">>,
            examples = [
                <<"stateDiagram-v2; [*] --> Active;">>
            ],
            validation_rules = [<<"valid_state_names">>, <<"valid_transitions">>],
            requires_wasm = false,
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"erDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/erDiagram.html">>,
            examples = [
                <<"erDiagram; CUSTOMER ||--o{ ORDER : places;">>
            ],
            validation_rules = [<<"valid_entity_names">>, <<"valid_relationships">>],
            requires_wasm = false,
            max_complexity = 100
        },
        #mermaid_diagram_type{
            name = <<"pie">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/pie.html">>,
            examples = [
                <<"pie title Pets; Dog : 386; Cat : 85;">>
            ],
            validation_rules = [<<"valid_data_values">>],
            requires_wasm = false,
            max_complexity = 50
        },
        #mermaid_diagram_type{
            name = <<"mindmap">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/mindmap.html">>,
            examples = [
                <<"mindmap; root((root)); branch1; branch2;">>
            ],
            validation_rules = [<<"valid_nesting">>],
            requires_wasm = true,  % Requires newer Mermaid
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"gitgraph">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/gitgraph.html">>,
            examples = [
                <<"gitgraph; commit; branch develop; checkout develop;">>
            ],
            validation_rules = [<<"valid_git_commands">>],
            requires_wasm = false,
            max_complexity = 100
        },
        #mermaid_diagram_type{
            name = <<"journey">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/journey.html">>,
            examples = [
                <<"journey; title My Journey; section Go; 5: Me: 5;">>
            ],
            validation_rules = [<<"valid_scores">>],
            requires_wasm = false,
            max_complexity = 50
        }
    ].
```

**Callback Implementation:**

```erlang
%% Supervisor callback
init([]) ->
    State = #state{
        diagram_types = load_default_types(),
        aliases = load_aliases(),
        version = <<"1.0.0">>
    },
    {ok, State}.

%% Handle sync calls
handle_call({register_diagram_type, Type}, _From, State) ->
    case maps:get(Type#mermaid_diagram_type.name, State#state.diagram_types, undefined) of
        undefined ->
            NewState = State#state{
                diagram_types = maps:put(Type#mermaid_diagram_type.name, Type, State#state.diagram_types)
            },
            {reply, ok, NewState};
        _ ->
            {reply, {error, already_exists}, State}
    end;

handle_call({get_diagram_type, Name}, _From, State) ->
    CanonicalName = resolve_alias(Name, State#state.aliases),
    case maps:get(CanonicalName, State#state.diagram_types, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Type -> {reply, {ok, Type}, State}
    end.

%% Helper functions
resolve_alias(Name, Aliases) ->
    maps:get(Name, Aliases, Name).

load_default_types() ->
    lists:foldl(fun(Type, Acc) ->
        maps:put(Type#mermaid_diagram_type.name, Type, Acc)
    end, #{}, default_diagram_types()).

load_aliases() ->
    #{
        <<"flow">> => <<"flowchart">>,
        <<"sequence">> => <<"sequenceDiagram">>,
        <<"state">> => <<"stateDiagram">>,
        <<"er">> => <<"erDiagram">>,
        <<"class">> => <<"classDiagram">>
    }.
```

---

### 3. erlmcp_mermaid_parser

**Purpose:** Parse and validate Mermaid diagram syntax

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_parser.erl`

**Behavior:** `gen_server`

**State Record:**

```erlang
-record(state, {
    parser_id :: binary(),
    backend :: cli | wasm | internal,
    config :: map()
}).
```

**API:**

```erlang
%% Parse diagram and return AST or error
-spec parse(binary(), binary()) ->
    {ok, mermaid_ast()} | {error, #{reason := binary(), line := integer(), column := integer()}}.

%% Validate diagram syntax only (no AST)
-spec validate(binary(), binary()) ->
    {ok, valid} | {error, #{reason := binary(), location := #{line := integer(), column := integer()}}}.

%% Detect diagram type from syntax
-spec detect_type(binary()) -> {ok, binary()} | {error, unknown_type}.

%% Extract diagram metadata (node count, edge count, etc.)
-spec extract_metadata(binary()) -> {ok, map()}.
```

**Implementation Strategy:**

```erlang
%% Parser worker (spawned dynamically)
start_link(ParserId, Config) ->
    gen_server:start_link(?MODULE, [ParserId, Config], []).

init([ParserId, Config]) ->
    Backend = maps:get(backend, Config, cli),
    {ok, #state{parser_id = ParserId, backend = Backend, config = Config}}.

%% Parse with Mermaid CLI
handle_call({parse, DiagramCode, DiagramType}, _From, #state{backend = cli} = State) ->
    case validate_syntax(DiagramCode, DiagramType) of
        {ok, valid} ->
            AST = extract_ast(DiagramCode, DiagramType),
            {reply, {ok, AST}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%% Syntax validation (integrated with Mermaid CLI)
validate_syntax(Code, Type) ->
    Cmd = io_lib:format("npx mermaid ~s -c ~s", [Type, temp_file(Code)]),
    case os:cmd(Cmd) of
        "" -> {ok, valid};
        ErrorOutput -> parse_error(ErrorOutput)
    end.

%% Parse CLI error output to extract line/column
parse_error(ErrorOutput) ->
    case re:run(ErrorOutput, "line (\\d+), column (\\d+)", [{capture, all, list}]) of
        {match, [_, Line, Col]} ->
            {error, #{
                reason => <<"syntax_error">>,
                line => list_to_integer(Line),
                column => list_to_integer(Col)
            }};
        nomatch ->
            {error, #{reason => <<"syntax_error">>, location => unknown}}
    end.

%% Detect diagram type from first line
detect_type(<<FirstLine:100/binary, _/binary>>) ->
    case re:run(FirstLine, "^(graph|sequenceDiagram|gantt|classDiagram|stateDiagram|erDiagram|pie|mindmap|gitgraph|journey)", [{capture, all_but_first, list}]) of
        {match, [Type]} -> {ok, list_to_binary(Type)};
        nomatch -> {error, unknown_type}
    end;
detect_type(_) ->
    {error, unknown_type}.

%% Extract metadata for caching/complexity analysis
extract_metadata(Code) ->
    #{
        line_count => count_lines(Code),
        node_count => count_nodes(Code),
        edge_count => count_edges(Code),
        estimated_complexity => estimate_complexity(Code)
    }.

count_nodes(Code) ->
    Matches = re:split(Code, "\\[|\\(|\\{|/", [{return, list}]),
    length(Matches).

count_edges(Code) ->
    Matches = re:split(Code, "-->|->|---->|~>|-->|\\|\\||\\|\\|\\||\\|\\|\\|\\|", [{return, list}]),
    length(Matches).

estimate_complexity(Code) ->
    Meta = extract_metadata(Code),
    maps:get(node_count, Meta, 0) * 2 + maps:get(edge_count, Meta, 0).
```

---

### 4. erlmcp_mermaid_renderer

**Purpose:** Generate SVG from parsed Mermaid diagrams

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_renderer.erl`

**Behavior:** `gen_server`

**State Record:**

```erlang
-record(state, {
    renderer_id :: binary(),
    backend :: cli | wasm | api,
    mermaid_version :: binary(),
    config :: map(),
    pool :: pid() | undefined  % poolboy pool for CLI workers
}).
```

**API:**

```erlang
%% Render diagram to SVG
-spec render(binary(), binary(), map()) ->
    {ok, binary()} | {error, term()}.

%% Render with custom theme
-spec render_with_theme(binary(), binary(), binary(), map()) ->
    {ok, binary()} | {error, term()}.

%% Batch render multiple diagrams
-spec render_batch([{binary(), binary()}], map()) ->
    [{ok, binary()} | {error, term()}].

%% Get renderer capabilities
-spec capabilities() -> map().
```

**Implementation Strategy:**

```erlang
%% CLI-based renderer (primary)
init([RendererId, #{backend := cli} = Config]) ->
    %% Ensure Mermaid CLI installed
    case os:cmd("which npx") of
        [] -> {stop, mermaid_cli_not_found};
        _ ->
            %% Start worker pool
            PoolArgs = [
                {name, {local, mermaid_cli_pool}},
                {worker_module, erlmcp_mermaid_cli_worker},
                {size, maps:get(pool_size, Config, 5)},
                {max_overflow, maps:get(max_overflow, Config, 10)}
            ],
            {ok, Pool} = poolboy:start_link(PoolArgs),
            {ok, #state{
                renderer_id = RendererId,
                backend = cli,
                mermaid_version = detect_mermaid_version(),
                config = Config,
                pool = Pool
            }}
    end;

%% WASM-based renderer (optional, faster)
init([RendererId, #{backend := wasm} = Config]) ->
    {ok, #state{
        renderer_id = RendererId,
        backend = wasm,
        mermaid_version => <<"latest">>,
        config = Config
    }}.

%% Render via CLI pool
handle_call({render, DiagramCode, DiagramType, Options}, _From, #state{backend = cli, pool = Pool} = State) ->
    Result = poolboy:transaction(Pool, fun(Worker) ->
        erlmcp_mermaid_cli_worker:render(Worker, DiagramCode, DiagramType, Options)
    end, maps:get(timeout, Options, 30000)),
    {reply, Result, State};

%% Render via WASM
handle_call({render, DiagramCode, DiagramType, Options}, _From, #state{backend = wasm} = State) ->
    %% Call WASM module
    Result = erlmcp_mermaid_wasm_backend:render(DiagramCode, DiagramType, Options),
    {reply, Result, State}.

%% Batch render
handle_call({render_batch, Diagrams, Options}, _From, #state{backend = cli, pool = Pool} = State) ->
    Results = lists:map(fun({Code, Type}) ->
        poolboy:transaction(Pool, fun(Worker) ->
            erlmcp_mermaid_cli_worker:render(Worker, Code, Type, Options)
        end, maps:get(timeout, Options, 30000))
    end, Diagrams),
    {reply, Results, State}.

%% Detect Mermaid CLI version
detect_mermaid_version() ->
    Output = os:cmd("npx mermaid --version 2>&1"),
    case re:run(Output, "v(\\d+\\.\\d+\\.\\d+)", [{capture, all_but_first, list}]) of
        {match, [Version]} -> list_to_binary(Version);
        nomatch -> <<"unknown">>
    end.

%% Renderer capabilities
capabilities() ->
    #{
        formats => [<<"svg">>, <<"png">>],  % SVG primary, PNG via ImageMagick
        themes => [<<"default">>, <<"forest">>, <<"dark">>, <<"neutral">>,
                  <<"base">>, <<"primary">>],
        max_diagram_size => 1024 * 1024,  % 1MB max input
        supported_types => erlmcp_mermaid_registry:list_diagram_types()
    }.
```

**CLI Worker Implementation:**

```erlang
%% Module: apps/erlmcp_core/src/erlmcp_mermaid_cli_worker.erl
-behaviour(gen_server).
-behaviour(poolboy_worker).

-record(state, {
    temp_dir :: binary(),
    mermaid_config :: map()
}).

init([]) ->
    TempDir = create_temp_dir(),
    {ok, #state{temp_dir = TempDir, mermaid_config => #{}}}.

render(Pid, DiagramCode, DiagramType, Options) ->
    gen_server:call(Pid, {render, DiagramCode, DiagramType, Options}, 30000).

handle_call({render, DiagramCode, DiagramType, Options}, _From, State) ->
    %% Write diagram to temp file
    InputFile = filename:join(State#state.temp_dir, "input.mmd"),
    OutputFile = filename:join(State#state.temp_dir, "output.svg"),
    ok = file:write_file(InputFile, DiagramCode),

    %% Build command with theme
    Theme = maps:get(theme, Options, <<"default">>),
    Cmd = io_lib:format(
        "cd ~s && npx mermaid ~s -i input.mmd -o output.svg -t ~s -b transparent 2>&1",
        [State#state.temp_dir, DiagramType, Theme]
    ),

    %% Execute
    case os:cmd(Cmd) of
        [] ->
            %% Success - read output
            case file:read_file(OutputFile) of
                {ok, SVG} -> {reply, {ok, SVG}, State};
                {error, Reason} -> {reply, {error, {file_error, Reason}}, State}
            end;
        ErrorOutput ->
            {reply, {error, #{cli_error => list_to_binary(ErrorOutput)}}, State}
    end;

handle_call(stop, _From, State) ->
    %% Cleanup temp dir
    ok = file:del_dir_r(State#state.temp_dir),
    {stop, normal, ok, State}.

create_temp_dir() ->
    TempDir = filename:join("/tmp", "mermaid-" ++ integer_to_list(erlang:unique_integer([positive]))),
    ok = file:make_dir(TempDir),
    list_to_binary(TempDir).
```

---

### 5. erlmcp_mermaid_cache

**Purpose:** Cache frequently rendered diagrams to reduce rendering overhead

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_cache.erl`

**Behavior:** `gen_server`

**State Record:**

```erlang
-record(state, {
    cache_tab :: ets:tid(),
    ttl_ms :: pos_integer(),
    max_size :: pos_integer(),
    eviction_policy :: lru | ttl | fifo
}).
```

**API:**

```erlang
%% Get cached diagram or render
-spec get(binary(), binary()) -> {ok, binary()} | {error, not_found}.

%% Put diagram in cache
-spec put(binary(), binary(), binary()) -> ok.

%% Invalidate cache entry
-spec invalidate(binary(), binary()) -> ok.

%% Clear entire cache
-spec clear() -> ok.

%% Get cache statistics
-spec stats() -> map().
```

**Implementation:**

```erlang
init([]) ->
    CacheTab = ets:new(mermaid_cache, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    TTL = application:get_env(erlmcp_core, mermaid_cache_ttl_ms, 3600000),  % 1 hour
    MaxSize = application:get_env(erlmcp_core, mermaid_cache_max_size, 10000),
    {ok, #state{
        cache_tab = CacheTab,
        ttl_ms = TTL,
        max_size = MaxSize,
        eviction_policy = lru
    }}.

%% Generate cache key from diagram content
generate_cache_key(DiagramCode, DiagramType, Options) ->
    Theme = maps:get(theme, Options, <<"default">>),
    Data = <<DiagramCode/binary, DiagramType/binary, Theme/binary>>,
    crypto:hash(sha256, Data).

%% Get from cache
handle_call({get, DiagramCode, DiagramType, Options}, _From, State) ->
    Key = generate_cache_key(DiagramCode, DiagramType, Options),
    case ets:lookup(State#state.cache_tab, Key) of
        [{Key, {SVG, Timestamp}}] ->
            case is_fresh(Timestamp, State#state.ttl_ms) of
                true ->
                    {reply, {ok, SVG}, State};
                false ->
                    ets:delete(State#state.cache_tab, Key),
                    {reply, {error, not_found}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

%% Put in cache with LRU eviction
handle_call({put, DiagramCode, DiagramType, Options, SVG}, _From, State) ->
    Key = generate_cache_key(DiagramCode, DiagramType, Options),
    Timestamp = erlang:system_time(millisecond),

    %% Check if cache is full
    case ets:info(State#state.cache_tab, size) >= State#state.max_size of
        true ->
            evict_lru(State#state.cache_tab);
        false ->
            ok
    end,

    ets:insert(State#state.cache_tab, {Key, {SVG, Timestamp}}),
    {reply, ok, State};

%% Clear cache
handle_call(clear, _From, State) ->
    ets:delete_all_objects(State#state.cache_tab),
    {reply, ok, State};

%% Get statistics
handle_call(stats, _From, State) ->
    Stats = #{
        size => ets:info(State#state.cache_tab, size),
        memory => ets:info(State#state.cache_tab, memory),
        ttl_ms => State#state.ttl_ms,
        max_size => State#state.max_size,
        eviction_policy => State#state.eviction_policy
    },
    {reply, Stats, State}.

%% Helper: check timestamp freshness
is_fresh(Timestamp, TTL) ->
    (erlang:system_time(millisecond) - Timestamp) < TTL.

%% Helper: evict least-recently-used entry
evict_lru(Tab) ->
    case ets:foldl(fun({Key, {_Value, Timestamp}}, {MinKey, MinTS} = Acc) ->
        case Timestamp < MinTS of
            true -> {Key, Timestamp};
            false -> Acc
        end
    end, {undefined, infinity}, Tab) of
        {undefined, _} -> ok;
        {Key, _} -> ets:delete(Tab, Key)
    end.
```

---

### 6. erlmcp_mermaid_handler

**Purpose:** MCP tool handler for diagram generation

**Module:** `apps/erlmcp_core/src/erlmcp_mermaid_handler.erl`

**Behavior:** gen_server (or simple tool handler function)

**API:**

```erlang
%% Main diagram generation tool
-spec generate_diagram(map()) -> {ok, map()} | {error, map()}.

%% Validation tool
-spec validate_diagram(map()) -> {ok, map()} | {error, map()}.

%% Template-based generation
-spec from_template(map()) -> {ok, map()} | {error, map()}.

%% Get supported diagram types
-spec list_types() -> {ok, map()}.
```

**Tool Registration (in erlmcp_server):**

```erlang
%% Register Mermaid tools
{ok, Server} = erlmcp_server:start_link(my_server, Capabilities),

ok = erlmcp_server:add_tool(Server, <<"diagram/generate">>,
    fun(Args) -> erlmcp_mermaid_handler:generate_diagram(Args) end,
    #{
        description => <<"Generate Mermaid diagram SVG">>,
        inputSchema => #{
            type => object,
            properties => #{
                diagram_type => #{
                    type => string,
                    enum => [<<"flowchart">>, <<"sequenceDiagram">>, <<"gantt">>],
                    description => <<"Type of diagram to generate">>
                },
                code => #{
                    type => string,
                    description => <<"Mermaid diagram syntax">>
                },
                theme => #{
                    type => string,
                    enum => [<<"default">>, <<"forest">>, <<"dark">>],
                    default => <<"default">>
                },
                options => #{
                    type => object,
                    properties => #{
                        width => #{type => integer},
                        height => #{type => integer}
                    }
                }
            },
            required => [<<"diagram_type">>, <<"code">>]
        }
    }
),

ok = erlmcp_server:add_tool(Server, <<"diagram/validate">>,
    fun(Args) -> erlmcp_mermaid_handler:validate_diagram(Args) end,
    #{
        description => <<"Validate Mermaid diagram syntax">>,
        inputSchema => #{
            type => object,
            properties => #{
                code => #{type => string},
                diagram_type => #{type => string}
            },
            required => [<<"code">>]
        }
    }
),

ok = erlmcp_server:add_tool(Server, <<"diagram/from_template">>,
    fun(Args) -> erlmcp_mermaid_handler:from_template(Args) end,
    #{
        description => <<"Generate diagram from template">>,
        inputSchema => #{
            type => object,
            properties => #{
                template => #{type => string},
                variables => #{type => object}
            },
            required => [<<"template">>]
        }
    }
).
```

**Implementation:**

```erlang
generate_diagram(#{<<"diagram_type">> := Type, <<"code">> := Code} = Args) ->
    Options = maps:get(<<"options">>, Args, #{}),

    %% Check cache first
    case erlmcp_mermaid_cache:get(Code, Type, Options) of
        {ok, SVG} ->
            {ok, #{
                content => #{
                    type => <<"image/svg+xml">>,
                    data => base64:encode(SVG)
                },
                cached => true
            }};
        {error, not_found} ->
            %% Validate first
            case erlmcp_mermaid_parser:validate(Code, Type) of
                {ok, valid} ->
                    %% Render
                    case erlmcp_mermaid_renderer:render(Code, Type, Options) of
                        {ok, SVG} ->
                            %% Cache result
                            ok = erlmcp_mermaid_cache:put(Code, Type, Options, SVG),
                            {ok, #{
                                content => #{
                                    type => <<"image/svg+xml">>,
                                    data => base64:encode(SVG)
                                },
                                cached => false
                            }};
                        {error, Reason} ->
                            {error, #{
                                code => -32603,
                                message => <<"Rendering failed">>,
                                data => Reason
                            }}
                    end;
                {error, Reason} ->
                    {error, #{
                        code => -32602,
                        message => <<"Invalid syntax">>,
                        data => Reason
                    }}
            end
    end;

generate_diagram(_) ->
    {error, #{
        code => -32602,
        message => <<"Invalid params">>,
        data => <<"Missing required fields: diagram_type, code">>
    }}.

validate_diagram(#{<<"code">> := Code} = Args) ->
    Type = maps:get(<<"diagram_type">>, Args, auto_detect),

    %% Auto-detect type if not provided
    DiagramType = case Type of
        auto_detect ->
            case erlmcp_mermaid_parser:detect_type(Code) of
                {ok, DetectedType} -> DetectedType;
                {error, _} -> <<"flowchart">>  % default
            end;
        _ -> Type
    end,

    case erlmcp_mermaid_parser:validate(Code, DiagramType) of
        {ok, valid} ->
            Meta = erlmcp_mermaid_parser:extract_metadata(Code),
            {ok, #{
                valid => true,
                diagram_type => DiagramType,
                metadata => Meta
            }};
        {error, Reason} ->
            {error, #{
                valid => false,
                errors => [Reason]
            }}
    end.

from_template(#{<<"template">> := TemplateName, <<"variables">> := Variables}) ->
    case load_template(TemplateName) of
        {ok, Template} ->
            %% Render template with variables
            Code = render_template(Template, Variables),
            %% Detect and render
            {ok, Type} = erlmcp_mermaid_parser:detect_type(Code),
            case erlmcp_mermaid_renderer:render(Code, Type, #{}) of
                {ok, SVG} ->
                    {ok, #{
                        content => #{
                            type => <<"image/svg+xml">>,
                            data => base64:encode(SVG)
                        },
                        template => TemplateName
                    }};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, #{
                code => -32602,
                message => <<"Template not found">>,
                data => Reason
            }}
    end.

list_types() ->
    Types = erlmcp_mermaid_registry:list_diagram_types(),
    {ok, #{
        diagram_types => Types,
        count => length(Types)
    }}.
```

---

## Transport Integration

### JSON-RPC Tool Interface

All Mermaid functionality is exposed through MCP tools, making it transport-agnostic:

**Example JSON-RPC Request (stdio/HTTP/WebSocket):**

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "method": "tools/call",
  "params": {
    "name": "diagram/generate",
    "arguments": {
      "diagram_type": "flowchart",
      "code": "graph TD; A[Start] --> B[Process]; B --> C[End];",
      "theme": "forest"
    }
  }
}
```

**JSON-RPC Response:**

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "content": {
      "type": "image/svg+xml",
      "data": "PHN2ZyB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciPjwvc3ZnPg=="
    },
    "cached": false
  }
}
```

---

## Error Handling Strategy

### Error Categories

| Category | MCP Error Code | Example |
|----------|----------------|---------|
| Invalid syntax | -32602 (Invalid params) | Malformed Mermaid syntax |
| Unsupported type | -32601 (Method not found) | Requested unsupported diagram type |
| Rendering failure | -32603 (Internal error) | Mermaid CLI crashed |
| Cache error | -32603 (Internal error) | Cache write failed |
| Timeout | -32603 (Internal error) | Render took >30s |

### Error Response Format

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "error": {
    "code": -32602,
    "message": "Invalid syntax",
    "data": {
      "reason": "syntax_error",
      "location": {
        "line": 3,
        "column": 12
      },
      "snippet": "graph TD;\n  A --> B\n  B -x C;\n  %%      ^ invalid edge syntax"
    }
  }
}
```

---

## Performance Considerations

### Caching Strategy

**Cache Key Generation:**
```
SHA-256(diagram_code + diagram_type + theme + width + height)
```

**Cache TTL:**
- Default: 1 hour
- Configurable via `mermaid_cache_ttl_ms`

**Eviction Policy:**
- LRU (Least Recently Used)
- Configurable: `lru`, `ttl`, `fifo`

**Cache Statistics:**
```erlang
erlmcp_mermaid_cache:stats().
%% => #{size => 1234, memory => 5242880, hit_rate => 0.85}
```

### Parallel Rendering

```erlang
%% Batch render for multiple diagrams
Diagrams = [
    {<<"graph TD; A-->B;">>, <<"flowchart">>},
    {<<"sequenceDiagram; A->>B: Hi;">>, <<"sequenceDiagram">>}
],

Results = erlmcp_mermaid_renderer:render_batch(Diagrams, #{}),
%% => [{ok, SVG1}, {ok, SVG2}]
```

### Resource Limits

| Resource | Limit | Rationale |
|----------|-------|-----------|
| Max diagram size | 1MB | Prevent memory exhaustion |
| Max parse time | 5s | Prevent blocking |
| Max render time | 30s | Prevent hanging |
| Max cache entries | 10,000 | Prevent unbounded growth |
| Max concurrent renders | 50 (pool size) | Prevent resource starvation |

---

## Configuration

### sys.config

```erlang
{erlmcp_core, [
    {mermaid, [
        {backend, cli},  % cli | wasm | api
        {cache_enabled, true},
        {cache_ttl_ms, 3600000},
        {cache_max_size, 10000},
        {pool_size, 5},
        {max_overflow, 10},
        {max_diagram_size, 1048576},  % 1MB
        {render_timeout_ms, 30000},
        {supported_types, [
            <<"flowchart">>,
            <<"sequenceDiagram">>,
            <<"gantt">>,
            <<"classDiagram">>,
            <<"stateDiagram">>,
            <<"erDiagram">>,
            <<"pie">>,
            <<"mindmap">>,
            <<"gitgraph">>,
            <<"journey">>
        ]}
    ]}
]}.
```

---

## Testing Strategy

### EUnit Tests

```erlang
%% Module: apps/erlmcp_core/test/erlmcp_mermaid_tests.erl

-module(erlmcp_mermaid_tests).
-include_lib("eunit/include/eunit.hrl").

registry_test_() ->
    {setup,
        fun() -> erlmcp_mermaid_registry:start_link() end,
        fun(_) -> gen_server:stop(erlmcp_mermaid_registry) end,
        {inparallel, [
            fun list_diagram_types/0,
            fun get_diagram_type/0,
            fun is_supported/0
        ]}
    }.

parser_test_() ->
    {setup,
        fun() -> erlmcp_mermaid_parser:start_link(test_parser, #{backend => cli}) end,
        fun(_) -> gen_server:stop(erlmcp_mermaid_parser) end,
        [
            fun parse_valid_flowchart/0,
            fun parse_invalid_syntax/0,
            fun detect_diagram_type/0
        ]
    }.

renderer_test_() ->
    {setup,
        fun() -> erlmcp_mermaid_renderer:start_link(test_renderer, #{backend => cli}) end,
        fun(_) -> gen_server:stop(erlmcp_mermaid_renderer) end,
        [
            fun render_simple_flowchart/0,
            fun render_with_theme/0,
            fun render_timeout/0
        ]
    }.

cache_test_() ->
    {setup,
        fun() -> erlmcp_mermaid_cache:start_link() end,
        fun(_) -> gen_server:stop(erlmcp_mermaid_cache) end,
        [
            fun cache_hit/0,
            fun cache_miss/0,
            fun cache_ttl/0,
            fun cache_eviction/0
        ]
    }.

%% Test cases
list_diagram_types() ->
    Types = erlmcp_mermaid_registry:list_diagram_types(),
    ?assert(lists:member(<<"flowchart">>, Types)),
    ?assert(lists:member(<<"sequenceDiagram">>, Types)).

parse_valid_flowchart() ->
    Code = <<"graph TD; A-->B;">>,
    {ok, valid} = erlmcp_mermaid_parser:validate(Code, <<"flowchart">>).

parse_invalid_syntax() ->
    Code = <<"graph TD; A -x B;">>,  % invalid edge syntax
    {error, _} = erlmcp_mermaid_parser:validate(Code, <<"flowchart">>).

render_simple_flowchart() ->
    Code = <<"graph TD; A-->B;">>,
    {ok, SVG} = erlmcp_mermaid_renderer:render(Code, <<"flowchart">>, #{}),
    ?assert(<<<?xml_version>> =< SVG),  % XML declaration
    ?assert(<<"<svg">> =< SVG).  % SVG tag

cache_hit() ->
    Code = <<"graph TD; A-->B;">>,
    ok = erlmcp_mermaid_cache:put(Code, <<"flowchart">>, #{}, <<"<svg>test</svg>">>),
    {ok, <<"<svg>test</svg>">>} = erlmcp_mermaid_cache:get(Code, <<"flowchart">>, #{}).
```

### Common Test Suites

```erlang
%% Module: apps/erlmcp_core/test/erlmcp_mermaid_SUITE.erl

-module(erlmcp_mermaid_SUITE).
-include_lib("common_test/include/ct.hrl").

all() -> [
    registry_suite,
    parser_suite,
    renderer_suite,
    cache_suite,
    integration_suite
].

init_per_suite(Config) ->
    application:ensure_all_started(erlmcp_core),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

%% Integration test: full diagram generation pipeline
integration_suite(_Config) ->
    Code = <<"graph TD; A[Start] --> B[Process]; B --> C[End];">>,
    Type = <<"flowchart">>,
    Options = #{theme => <<"forest">>},

    %% Generate via handler
    {ok, Result} = erlmcp_mermaid_handler:generate_diagram(#{
        <<"diagram_type">> => Type,
        <<"code">> => Code,
        <<"theme">> => <<"forest">>
    }),

    %% Verify response
    #{<<"content">> := #{<<"type">> := <<"image/svg+xml">>, <<"data">> := Data}} = Result,
    SVG = base64:decode(Data),
    ?assert(<<"<svg">> =< SVG),
    ?assert(<<"</svg>">> =< SVG),

    %% Verify caching
    {ok, Result2} = erlmcp_mermaid_handler:generate_diagram(#{
        <<"diagram_type">> => Type,
        <<"code">> => Code,
        <<"theme">> => <<"forest">>
    }),
    #{<<"cached">> := true} = Result2.
```

---

## Deployment

### Dependencies (rebar.config)

```erlang
{deps, [
    %% Existing dependencies...
    {poolboy, "1.5.2"},  % Already in erlmcp_transports
    %% No new dependencies required for CLI backend
]}.

%% WASM backend (optional)
{profiles, [
    {wasm, [
        {deps, [
            {erlmcp_wasm_runtime, "0.1.0"}  % Hypothetical WASM runtime
        ]}
    ]}
]}.
```

### Installation (for CLI backend)

```bash
# Install Node.js and npm
apt-get install nodejs npm

# Verify Mermaid CLI
npx mermaid --version

# Pre-load Mermaid to avoid first-render delay
npx mermaid --help
```

### Runtime Requirements

| Component | Requirement |
|-----------|-------------|
| Node.js | >=18.0.0 |
| npm | >=9.0.0 |
| Mermaid CLI | Latest (via npx) |
| Memory | +100MB per render worker |
| Disk | +50MB for temp files |

---

## Future Enhancements

### Short-Term (v2.2.0)
- [ ] WASM-based renderer (faster, no external dependency)
- [ ] PNG output via ImageMagick
- [ ] Template library
- [ ] Interactive SVG (hyperlinks, tooltips)

### Medium-Term (v2.3.0)
- [ ] Diagram versioning
- [ ] Diff generation between diagrams
- [ ] Custom theme support
- [ ] Plugin system for custom diagram types

### Long-Term (v3.0.0)
- [ ] Real-time collaborative editing
- [ ] Diagram animation support
- [ ] Export to PowerPoint/Keynote
- [ ] AI-powered diagram suggestion

---

## Summary

This architecture provides:

✅ **Complete Mermaid Integration** - All 10+ diagram types supported
✅ **OTP Compliance** - Proper supervision, gen_server behaviors
✅ **Transport Independence** - Works across stdio, HTTP, WebSocket
✅ **Fault Isolation** - Mermaid failures don't affect core protocol
✅ **Performance** - Caching, parallel rendering, resource pooling
✅ **Extensibility** - Easy to add new diagram types or renderers
✅ **Error Handling** - Comprehensive error detection and reporting
✅ **Testing** - EUnit + Common Test suites included

The modular design allows Mermaid components to be added to erlmcp_core without disrupting existing functionality, following the principle of **separation of concerns** and **failure isolation** that defines the erlmcp architecture.
