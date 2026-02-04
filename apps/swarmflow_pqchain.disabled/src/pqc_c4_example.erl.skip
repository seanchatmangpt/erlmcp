%%% @doc Examples and utilities for working with the C4 architecture model
%%%
%%% This module demonstrates how to use pqc_c4_model to:
%%% - Query the architecture model
%%% - Generate diagrams in various formats
%%% - Export documentation
%%% - Validate the model
%%%
%%% Usage:
%%%   pqc_c4_example:generate_all_diagrams().
%%%   pqc_c4_example:print_architecture_summary().
%%%   pqc_c4_example:list_containers().
%%%
%%% @end
-module(pqc_c4_example).

%% API
-export([
    generate_all_diagrams/0,
    generate_all_diagrams/1,
    print_architecture_summary/0,
    list_containers/0,
    list_components/1,
    list_relationships/0,
    validate_architecture/0
]).

-define(OUTPUT_DIR, "doc/diagrams").

%%% ============================================================================
%%% Diagram Generation
%%% ============================================================================

%% @doc Generate all diagram formats to the default output directory
-spec generate_all_diagrams() -> ok | {error, term()}.
generate_all_diagrams() ->
    generate_all_diagrams(?OUTPUT_DIR).

%% @doc Generate all diagram formats to the specified output directory
-spec generate_all_diagrams(OutputDir :: string()) -> ok | {error, term()}.
generate_all_diagrams(OutputDir) ->
    io:format("Generating C4 architecture diagrams to ~s...~n", [OutputDir]),

    %% Ensure output directory exists
    ok = filelib:ensure_dir(filename:join(OutputDir, "dummy")),

    %% Generate PlantUML diagrams
    io:format("  - Generating PlantUML diagrams...~n"),
    ok = generate_plantuml_diagrams(OutputDir),

    %% Generate Structurizr DSL
    io:format("  - Generating Structurizr DSL...~n"),
    ok = generate_structurizr_dsl(OutputDir),

    %% Generate Mermaid diagrams
    io:format("  - Generating Mermaid diagrams...~n"),
    ok = generate_mermaid_diagrams(OutputDir),

    io:format("~nDiagrams generated successfully!~n"),
    io:format("~nTo render PlantUML diagrams:~n"),
    io:format("  cd ~s && plantuml *.puml~n", [OutputDir]),
    io:format("~nTo view Structurizr DSL:~n"),
    io:format("  Upload workspace.dsl to https://structurizr.com/~n"),
    io:format("  or run: docker run -p 8080:8080 -v $PWD/~s:/usr/local/structurizr structurizr/lite~n", [OutputDir]),
    io:format("~nMermaid diagrams can be embedded in Markdown or viewed at https://mermaid.live/~n~n"),

    ok.

-spec generate_plantuml_diagrams(OutputDir :: string()) -> ok.
generate_plantuml_diagrams(OutputDir) ->
    %% Context diagram
    ContextFile = filename:join(OutputDir, "01-context.puml"),
    ContextDiagram = pqc_c4_model:to_plantuml(context),
    ok = file:write_file(ContextFile, ContextDiagram),
    io:format("    ✓ ~s~n", [ContextFile]),

    %% Container diagram
    ContainerFile = filename:join(OutputDir, "02-container.puml"),
    ContainerDiagram = pqc_c4_model:to_plantuml(container),
    ok = file:write_file(ContainerFile, ContainerDiagram),
    io:format("    ✓ ~s~n", [ContainerFile]),

    %% Component diagram
    ComponentFile = filename:join(OutputDir, "03-component-consensus.puml"),
    ComponentDiagram = pqc_c4_model:to_plantuml(component),
    ok = file:write_file(ComponentFile, ComponentDiagram),
    io:format("    ✓ ~s~n", [ComponentFile]),

    ok.

-spec generate_structurizr_dsl(OutputDir :: string()) -> ok.
generate_structurizr_dsl(OutputDir) ->
    DSLFile = filename:join(OutputDir, "workspace.dsl"),
    DSL = pqc_c4_model:to_structurizr_dsl(),
    ok = file:write_file(DSLFile, DSL),
    io:format("    ✓ ~s~n", [DSLFile]),
    ok.

-spec generate_mermaid_diagrams(OutputDir :: string()) -> ok.
generate_mermaid_diagrams(OutputDir) ->
    %% Context diagram
    ContextFile = filename:join(OutputDir, "context.mmd"),
    ContextDiagram = pqc_c4_model:to_mermaid(context),
    ok = file:write_file(ContextFile, ContextDiagram),
    io:format("    ✓ ~s~n", [ContextFile]),

    %% Container diagram
    ContainerFile = filename:join(OutputDir, "container.mmd"),
    ContainerDiagram = pqc_c4_model:to_mermaid(container),
    ok = file:write_file(ContainerFile, ContainerDiagram),
    io:format("    ✓ ~s~n", [ContainerFile]),

    ok.

%%% ============================================================================
%%% Architecture Queries
%%% ============================================================================

%% @doc Print a summary of the architecture
-spec print_architecture_summary() -> ok.
print_architecture_summary() ->
    Context = pqc_c4_model:context(),
    Containers = pqc_c4_model:containers(),
    Relationships = pqc_c4_model:relationships(),

    io:format("~n=== SwarmFlow PQChain Architecture Summary ===~n~n"),

    %% System info
    io:format("System: ~s~n", [element(3, Context)]),
    io:format("Description: ~s~n~n", [element(4, Context)]),

    %% Statistics
    io:format("Statistics:~n"),
    io:format("  - Containers: ~p~n", [length(Containers)]),

    ComponentCount = lists:sum([
        length(element(6, C)) || C <- Containers
    ]),
    io:format("  - Components: ~p~n", [ComponentCount]),
    io:format("  - Relationships: ~p~n~n", [length(Relationships)]),

    %% Architecture pattern
    io:format("Architecture Pattern: One Kernel, Two Faces~n"),
    io:format("  - Case Kernel: Erlang/OTP runtime~n"),
    io:format("  - A2A Face: Agent-to-Agent protocol (Google A2A)~n"),
    io:format("  - MCP Face: Model Context Protocol (Anthropic MCP)~n~n"),

    %% Key technologies
    io:format("Key Technologies:~n"),
    io:format("  - Post-Quantum Crypto: ML-KEM, ML-DSA, SLH-DSA (NIST FIPS 203/204/205)~n"),
    io:format("  - Consensus: Byzantine Fault Tolerant (gen_statem)~n"),
    io:format("  - Smart Contracts: Workflow Nets (SwarmFlow OS)~n"),
    io:format("  - P2P Network: ML-KEM encrypted channels~n"),
    io:format("  - State Storage: Mnesia/RocksDB with Merkle Patricia tries~n~n"),

    ok.

%% @doc List all containers with their technologies
-spec list_containers() -> ok.
list_containers() ->
    Containers = pqc_c4_model:containers(),

    io:format("~n=== Containers ===~n~n"),

    lists:foreach(
        fun(Container) ->
            Id = element(2, Container),
            Name = element(3, Container),
            Tech = element(4, Container),
            Components = element(6, Container),

            io:format("~s~n", [Name]),
            io:format("  ID: ~s~n", [Id]),
            io:format("  Technology: ~s~n", [Tech]),
            io:format("  Components: ~p~n~n", [length(Components)])
        end,
        Containers
    ),

    ok.

%% @doc List components for a specific container
-spec list_components(ContainerId :: binary()) -> ok | {error, not_found}.
list_components(ContainerId) ->
    case pqc_c4_model:components(ContainerId) of
        {error, not_found} ->
            io:format("Container not found: ~s~n", [ContainerId]),
            {error, not_found};
        Components ->
            io:format("~n=== Components in ~s ===~n~n", [ContainerId]),

            lists:foreach(
                fun(Component) ->
                    Name = element(3, Component),
                    Tech = element(4, Component),
                    Desc = element(5, Component),
                    Responsibilities = element(6, Component),

                    io:format("~s (~s)~n", [Name, Tech]),
                    io:format("  ~s~n", [Desc]),
                    io:format("  Responsibilities:~n"),
                    lists:foreach(
                        fun(R) ->
                            io:format("    - ~s~n", [R])
                        end,
                        Responsibilities
                    ),
                    io:format("~n")
                end,
                Components
            ),

            ok
    end.

%% @doc List all relationships
-spec list_relationships() -> ok.
list_relationships() ->
    Relationships = pqc_c4_model:relationships(),

    io:format("~n=== Relationships ===~n~n"),

    %% Group by source
    Grouped = lists:foldl(
        fun(Rel, Acc) ->
            Source = element(2, Rel),
            maps:update_with(Source, fun(Rels) -> [Rel | Rels] end, [Rel], Acc)
        end,
        #{},
        Relationships
    ),

    %% Print grouped relationships
    lists:foreach(
        fun({Source, Rels}) ->
            io:format("~s:~n", [Source]),
            lists:foreach(
                fun(Rel) ->
                    Target = element(3, Rel),
                    Desc = element(4, Rel),
                    Tech = element(5, Rel),
                    Bidir = element(6, Rel),

                    Arrow = case Bidir of
                        true -> <<"<-->">>;
                        false -> <<"--->">>
                    end,

                    TechStr = case Tech of
                        undefined -> <<"">>;
                        T -> [<<" (">>, T, <<")">>]
                    end,

                    io:format("  ~s ~s: ~s~s~n", [Arrow, Target, Desc, TechStr])
                end,
                lists:reverse(Rels)
            ),
            io:format("~n")
        end,
        lists:sort(maps:to_list(Grouped))
    ),

    ok.

%%% ============================================================================
%%% Validation
%%% ============================================================================

%% @doc Validate the architecture model and print results
-spec validate_architecture() -> ok | {error, [term()]}.
validate_architecture() ->
    io:format("~nValidating C4 architecture model...~n"),

    case pqc_c4_model:validate_model() of
        ok ->
            io:format("✓ Model is valid~n"),
            io:format("  - All IDs are unique~n"),
            io:format("  - All relationships reference valid elements~n"),
            io:format("  - All containers have components~n~n"),
            ok;
        {error, Errors} ->
            io:format("✗ Model validation failed:~n"),
            lists:foreach(
                fun(Error) ->
                    io:format("  - ~p~n", [Error])
                end,
                Errors
            ),
            io:format("~n"),
            {error, Errors}
    end.

%%% ============================================================================
%%% Example Usage
%%% ============================================================================

%% Example session:
%%
%% 1> pqc_c4_example:print_architecture_summary().
%% === SwarmFlow PQChain Architecture Summary ===
%% ...
%%
%% 2> pqc_c4_example:list_containers().
%% === Containers ===
%% ...
%%
%% 3> pqc_c4_example:list_components(<<"consensus_engine">>).
%% === Components in consensus_engine ===
%% ...
%%
%% 4> pqc_c4_example:validate_architecture().
%% Validating C4 architecture model...
%% ✓ Model is valid
%% ...
%%
%% 5> pqc_c4_example:generate_all_diagrams().
%% Generating C4 architecture diagrams to doc/diagrams...
%% ...
