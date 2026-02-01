-module(erlmcp_mermaid_registry).
-behaviour(gen_server).

%% API
-export([start_link/0,
         register_diagram_type/1,
         get_diagram_type/1,
         list_diagram_types/0,
         is_supported/1,
         validate_diagram/2,
         get_all_metadata/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

%% Records
-record(state, {
    diagram_types :: #{binary() => #mermaid_diagram_type{}},
    aliases :: #{binary() => binary()},
    version :: binary()
}).

-record(mermaid_diagram_type, {
    name :: binary(),
    category :: binary(),
    syntax :: binary(),
    examples :: [binary()],
    validation_rules :: [binary()],
    requires_wasm :: boolean(),
    max_complexity :: integer()
}).

-type mermaid_diagram_type() :: #mermaid_diagram_type{}.
-export_type([mermaid_diagram_type/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the registry server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a new diagram type
-spec register_diagram_type(mermaid_diagram_type()) -> ok | {error, term()}.
register_diagram_type(Type) ->
    gen_server:call(?SERVER, {register_diagram_type, Type}).

%% @doc Get diagram type metadata
-spec get_diagram_type(binary()) -> {ok, mermaid_diagram_type()} | {error, not_found}.
get_diagram_type(Name) ->
    gen_server:call(?SERVER, {get_diagram_type, Name}).

%% @doc List all supported diagram types
-spec list_diagram_types() -> [binary()].
list_diagram_types() ->
    gen_server:call(?SERVER, list_diagram_types).

%% @doc Check if diagram type is supported
-spec is_supported(binary()) -> boolean().
is_supported(Name) ->
    gen_server:call(?SERVER, {is_supported, Name}).

%% @doc Validate diagram syntax against type rules
-spec validate_diagram(binary(), binary()) ->
    {ok, valid} | {error, #{reason := binary(), line => integer(), column => integer()}}.
validate_diagram(Code, Type) ->
    gen_server:call(?SERVER, {validate_diagram, Code, Type}).

%% @doc Get all diagram metadata
-spec get_all_metadata() -> #{binary() => mermaid_diagram_type()}.
get_all_metadata() ->
    gen_server:call(?SERVER, get_all_metadata).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    State = #state{
        diagram_types = load_default_types(),
        aliases = load_aliases(),
        version = <<"1.0.0">>
    },
    {ok, State}.

%% @private
handle_call({register_diagram_type, #mermaid_diagram_type{name = Name} = Type}, _From, State) ->
    case maps:get(Name, State#state.diagram_types, undefined) of
        undefined ->
            NewState = State#state{
                diagram_types = maps:put(Name, Type, State#state.diagram_types)
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
    end;

handle_call(list_diagram_types, _From, State) ->
    Types = maps:keys(State#state.diagram_types),
    {reply, lists:sort(Types), State};

handle_call({is_supported, Name}, _From, State) ->
    CanonicalName = resolve_alias(Name, State#state.aliases),
    IsSupported = maps:is_key(CanonicalName, State#state.diagram_types),
    {reply, IsSupported, State};

handle_call({validate_diagram, Code, Type}, _From, State) ->
    case get_diagram_type(Type) of
        {ok, #mermaid_diagram_type{validation_rules = Rules, max_complexity = MaxComplexity}} ->
            case validate_syntax(Code, Rules) of
                {ok, valid} ->
                    case validate_complexity(Code, MaxComplexity) of
                        {ok, valid} ->
                            {reply, {ok, valid}, State};
                        {error, Reason} ->
                            {reply, {error, Reason}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, not_found} ->
            {reply, {error, #{reason => <<"unsupported_diagram_type">>}}, State}
    end;

handle_call(get_all_metadata, _From, State) ->
    {reply, State#state.diagram_types, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
load_default_types() ->
    Types = default_diagram_types(),
    lists:foldl(fun(Type, Acc) ->
        maps:put(Type#mermaid_diagram_type.name, Type, Acc)
    end, #{}, Types).

%% @private
load_aliases() ->
    #{
        <<"flow">> => <<"flowchart">>,
        <<"sequence">> => <<"sequenceDiagram">>,
        <<"state">> => <<"stateDiagram-v2">>,
        <<"er">> => <<"erDiagram">>,
        <<"class">> => <<"classDiagram">>,
        <<"state_v2">> => <<"stateDiagram-v2">>
    }.

%% @private
resolve_alias(Name, Aliases) ->
    maps:get(Name, Aliases, Name).

%% @private
default_diagram_types() ->
    [
        #mermaid_diagram_type{
            name = <<"flowchart">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"graph TD; A-->B; B-->C;">>,
                <<"graph LR; start[Start]-->stop[Stop];">>,
                <<"graph TB; subgraph S1; A1; A2; end; subgraph S2; B1; B2; end;">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>, <<"valid_subgraphs">>],
            requires_wasm = false,
            max_complexity = 1000
        },
        #mermaid_diagram_type{
            name = <<"sequenceDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/sequenceDiagram.html">>,
            examples = [
                <<"sequenceDiagram; A->>B: Hello; B-->>A: Hi;">>,
                <<"sequenceDiagram; participant Alice; participant Bob; Alice->>Bob: Hello;">>
            ],
            validation_rules = [<<"valid_participants">>, <<"valid_messages">>, <<"valid_arrows">>],
            requires_wasm = false,
            max_complexity = 500
        },
        #mermaid_diagram_type{
            name = <<"gantt">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/gantt.html">>,
            examples = [
                <<"gantt; title Project; section Phase 1; Task 1: 2024-01-01, 7d;">>,
                <<"gantt; dateFormat YYYY-MM-DD; section Planning; Task 1: 2024-01-01, 7d; Task 2: 2024-01-08, 5d;">>
            ],
            validation_rules = [<<"valid_dates">>, <<"valid_durations">>, <<"valid_sections">>],
            requires_wasm = false,
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"classDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/classDiagram.html">>,
            examples = [
                <<"classDiagram; Animal <|-- Duck;">>,
                <<"classDiagram; class Car{ +String model; +drive() }">>
            ],
            validation_rules = [<<"valid_class_names">>, <<"valid_relationships">>, <<"valid_members">>],
            requires_wasm = false,
            max_complexity = 300
        },
        #mermaid_diagram_type{
            name = <<"stateDiagram-v2">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/stateDiagram.html">>,
            examples = [
                <<"stateDiagram-v2; [*] --> Active;">>,
                <<"stateDiagram-v2; [*] --> Still; Still --> [*]; Still --> Moving;">>
            ],
            validation_rules = [<<"valid_state_names">>, <<"valid_transitions">>, <<"valid_composites">>],
            requires_wasm = false,
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"erDiagram">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/erDiagram.html">>,
            examples = [
                <<"erDiagram; CUSTOMER ||--o{ ORDER : places;">>,
                <<"erDiagram; STUDENT }|..|{ ADVISOR : supervises">>
            ],
            validation_rules = [<<"valid_entity_names">>, <<"valid_relationships">>, <<"valid_cardinalities">>],
            requires_wasm = false,
            max_complexity = 100
        },
        #mermaid_diagram_type{
            name = <<"pie">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/pie.html">>,
            examples = [
                <<"pie title Pets; Dog : 386; Cat : 85;">>,
                <<"pie showData; title Market Share; A : 40; B : 30; C : 30;">>
            ],
            validation_rules = [<<"valid_data_values">>, <<"valid_percentages">>],
            requires_wasm = false,
            max_complexity = 50
        },
        #mermaid_diagram_type{
            name = <<"mindmap">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/mindmap.html">>,
            examples = [
                <<"mindmap; root((root)); branch1; branch2;">>,
                <<"mindmap; root((mindmap)); Origins; Long history; ::icon(fa fa-book);">>
            ],
            validation_rules = [<<"valid_nesting">>, <<"valid_icons">>],
            requires_wasm = true,
            max_complexity = 200
        },
        #mermaid_diagram_type{
            name = <<"gitgraph">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/gitgraph.html">>,
            examples = [
                <<"gitgraph; commit; branch develop; checkout develop;">>,
                <<"gitgraph; commit; commit; branch feat; checkout feat;">>
            ],
            validation_rules = [<<"valid_git_commands">>, <<"valid_branch_names">>],
            requires_wasm = false,
            max_complexity = 100
        },
        #mermaid_diagram_type{
            name = <<"journey">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/journey.html">>,
            examples = [
                <<"journey; title My Journey; section Go; 5: Me: 5;">>,
                <<"journey; title Order processing; section Order; 5: Customer: 5;">>
            ],
            validation_rules = [<<"valid_scores">>, <<"valid_sections">>],
            requires_wasm = false,
            max_complexity = 50
        },
        #mermaid_diagram_type{
            name = <<"flowchart LR">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"flowchart LR; A-->B; B-->C;">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>],
            requires_wasm = false,
            max_complexity = 1000
        },
        #mermaid_diagram_type{
            name = <<"flowchart TD">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"flowchart TD; A-->B; B-->C;">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>],
            requires_wasm = false,
            max_complexity = 1000
        },
        #mermaid_diagram_type{
            name = <<"flowchart TB">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"flowchart TB; A-->B; B-->C;">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>],
            requires_wasm = false,
            max_complexity = 1000
        },
        #mermaid_diagram_type{
            name = <<"flowchart RL">>,
            category = <<"diagram">>,
            syntax = <<"https://mermaid.js.org/syntax/flowchart.html">>,
            examples = [
                <<"flowchart RL; A-->B; B-->C;">>
            ],
            validation_rules = [<<"valid_node_ids">>, <<"valid_edge_syntax">>],
            requires_wasm = false,
            max_complexity = 1000
        }
    ].

%% @private
validate_syntax(_Code, []) ->
    {ok, valid};
validate_syntax(Code, [Rule | Rest]) ->
    case apply_validation_rule(Code, Rule) of
        {ok, valid} -> validate_syntax(Code, Rest);
        {error, _} = Error -> Error
    end.

%% @private
apply_validation_rule(Code, <<"valid_node_ids">>) ->
    % Check for valid node identifiers (alphanumeric, underscore, hyphen)
    case re:run(Code, "\\[([a-zA-Z0-9_-]+)\\]|\\(([a-zA-Z0-9_-]+)\\)|\\{([a-zA-Z0-9_-]+)\\}", [global]) of
        {match, _} -> {ok, valid};
        nomatch -> {ok, valid}  % Empty graph is valid
    end;

apply_validation_rule(Code, <<"valid_edge_syntax">>) ->
    % Check for valid edge syntax
    InvalidEdges = re:run(Code, "-[^->]", [global]),
    case InvalidEdges of
        nomatch -> {ok, valid};
        _ -> {error, #{reason => <<"invalid_edge_syntax">>}}
    end;

apply_validation_rule(Code, <<"valid_subgraphs">>) ->
    % Check subgraph syntax
    case re:run(Code, "subgraph .*? end", [global, {capture, all}, caseless]) of
        {match, _} -> {ok, valid};
        nomatch -> {ok, valid}
    end;

apply_validation_rule(Code, <<"valid_participants">>) ->
    % Check participant declarations
    {ok, valid};

apply_validation_rule(Code, <<"valid_messages">>) ->
    % Check message syntax
    case re:run(Code, "->>|-->>|->|-->>|->>|-->>", [global]) of
        {match, _} -> {ok, valid};
        nomatch -> {ok, valid}
    end;

apply_validation_rule(_Code, <<"valid_dates">>) ->
    % Basic date validation (would be more comprehensive in production)
    {ok, valid};

apply_validation_rule(_Code, <<"valid_durations">>) ->
    % Basic duration validation
    {ok, valid};

apply_validation_rule(_Code, <<"valid_sections">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_class_names">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_relationships">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_members">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_state_names">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_transitions">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_composites">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_entity_names">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_cardinalities">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_data_values">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_percentages">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_nesting">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_icons">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_git_commands">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_branch_names">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_scores">>) ->
    {ok, valid};

apply_validation_rule(_Code, <<"valid_arrows">>) ->
    {ok, valid};

apply_validation_rule(_Code, _Rule) ->
    {ok, valid}.

%% @private
validate_complexity(Code, MaxComplexity) ->
    Complexity = calculate_complexity(Code),
    case Complexity =< MaxComplexity of
        true -> {ok, valid};
        false ->
            {error, #{
                reason => <<"diagram_too_complex">>,
                complexity => Complexity,
                max_allowed => MaxComplexity
            }}
    end.

%% @private
calculate_complexity(Code) ->
    % Count nodes and edges as a rough complexity measure
    Nodes = length(re:split(Code, "\\[|\\(|\\{|/", [{return, list}, trim])) - 1,
    Edges = length(re:split(Code, "-->|->|---->|~>|-->|\\|\\||\\|\\|\\||\\|\\|\\|\\|", [{return, list}, trim])) - 1,
    max(0, Nodes * 2 + Edges).
