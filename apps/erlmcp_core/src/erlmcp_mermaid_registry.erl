-module(erlmcp_mermaid_registry).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    register_type/2,
    register_types/1,
    unregister_type/1,
    get_type/1,
    list_types/0,
    list_types_by_category/1,
    validate_diagram/2,
    get_parser/1,
    get_renderer/1,
    convert_type/2,
    get_type_metadata/1,
    get_compatible_types/1,
    check_type_support/2,
    get_type_inheritance/1,
    get_all_metadata/0,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type diagram_type() :: binary().
-type category() :: flowchart | sequence | class | state | er | gantt | pie | relationship | mindmap | timeline | git | user | architecture.
-type parser_module() :: atom().
-type renderer_module() :: atom().
-type validation_rules() :: map().
-type metadata() :: map().

-export_type([diagram_type/0, category/0, parser_module/0, renderer_module/0, validation_rules/0, metadata/0]).

%% Diagram Type Record
-record(diagram_type, {
    name :: diagram_type(),
    category :: category(),
    parser_module :: parser_module() | undefined,
    renderer_module :: renderer_module() | undefined,
    validation_rules :: validation_rules(),
    metadata :: metadata(),
    parent_type :: diagram_type() | undefined,
    aliases :: [binary()]
}).

%% State Record
-record(state, {
    types = #{} :: #{diagram_type() => #diagram_type{}},
    categories = #{} :: #{category() => [diagram_type()]},
    parsers = #{} :: #{parser_module() => [diagram_type()]},
    renderers = #{} :: #{renderer_module() => [diagram_type()]}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register_type(diagram_type(), metadata()) -> ok | {error, term()}.
register_type(TypeName, Metadata) when is_binary(TypeName), is_map(Metadata) ->
    gen_server:call(?MODULE, {register_type, TypeName, Metadata}).

-spec register_types([metadata()]) -> ok | {error, term()}.
register_types(TypesMetadata) when is_list(TypesMetadata) ->
    gen_server:call(?MODULE, {register_types, TypesMetadata}).

-spec unregister_type(diagram_type()) -> ok | {error, not_found}.
unregister_type(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {unregister_type, TypeName}).

-spec get_type(diagram_type()) -> {ok, #diagram_type{}} | {error, not_found}.
get_type(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_type, TypeName}).

-spec list_types() -> {ok, [diagram_type()]}.
list_types() ->
    gen_server:call(?MODULE, list_types).

-spec list_types_by_category(category()) -> {ok, [diagram_type()]} | {error, {invalid_category, category()}}.
list_types_by_category(Category) ->
    gen_server:call(?MODULE, {list_types_by_category, Category}).

-spec validate_diagram(diagram_type(), binary()) -> ok | {error, term()}.
validate_diagram(TypeName, DiagramCode) when is_binary(TypeName), is_binary(DiagramCode) ->
    gen_server:call(?MODULE, {validate_diagram, TypeName, DiagramCode}, 10000).

-spec get_parser(diagram_type()) -> {ok, parser_module()} | {error, not_found}.
get_parser(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_parser, TypeName}).

-spec get_renderer(diagram_type()) -> {ok, renderer_module()} | {error, not_found}.
get_renderer(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_renderer, TypeName}).

-spec convert_type(diagram_type(), diagram_type()) -> {ok, binary()} | {error, term()}.
convert_type(SourceType, TargetType) when is_binary(SourceType), is_binary(TargetType) ->
    gen_server:call(?MODULE, {convert_type, SourceType, TargetType}, 30000).

-spec get_type_metadata(diagram_type()) -> {ok, metadata()} | {error, not_found}.
get_type_metadata(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_type_metadata, TypeName}).

-spec get_compatible_types(diagram_type()) -> {ok, [diagram_type()]} | {error, not_found}.
get_compatible_types(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_compatible_types, TypeName}).

-spec check_type_support(diagram_type(), atom()) -> boolean() | {error, not_found}.
check_type_support(TypeName, Feature) when is_binary(TypeName), is_atom(Feature) ->
    gen_server:call(?MODULE, {check_type_support, TypeName, Feature}).

-spec get_type_inheritance(diagram_type()) -> {ok, [diagram_type()]} | {error, not_found}.
get_type_inheritance(TypeName) when is_binary(TypeName) ->
    gen_server:call(?MODULE, {get_type_inheritance, TypeName}).

-spec get_all_metadata() -> {ok, #{diagram_type() => metadata()}}.
get_all_metadata() ->
    gen_server:call(?MODULE, get_all_metadata).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state(), {continue, initialize_types}}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting Mermaid diagram type registry (async initialization)"),
    {ok, #state{}, {continue, initialize_types}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({register_type, TypeName, Metadata}, _From, State) ->
    case validate_metadata(Metadata) of
        ok ->
            DiagramType = build_diagram_type(TypeName, Metadata),
            NewState = store_type(DiagramType, State),
            logger:info("Registered Mermaid diagram type: ~s (~p)", [TypeName, DiagramType#diagram_type.category]),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({register_types, TypesMetadata}, _From, State) ->
    case validate_all_metadata(TypesMetadata) of
        ok ->
            {DiagramTypes, NewState} = lists:mapfoldl(
                fun(Metadata, AccState) ->
                    TypeName = maps:get(name, Metadata),
                    DiagramType = build_diagram_type(TypeName, Metadata),
                    {DiagramType, store_type(DiagramType, AccState)}
                end,
                State,
                TypesMetadata
            ),
            logger:info("Registered ~p Mermaid diagram types", [length(DiagramTypes)]),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({unregister_type, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        DiagramType ->
            NewState = remove_type(DiagramType, State),
            logger:info("Unregistered Mermaid diagram type: ~s", [TypeName]),
            {reply, ok, NewState}
    end;

handle_call({get_type, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        DiagramType ->
            {reply, {ok, DiagramType}, State}
    end;

handle_call(list_types, _From, State) ->
    Types = maps:keys(State#state.types),
    {reply, {ok, lists:sort(Types)}, State};

handle_call({list_types_by_category, Category}, _From, State) ->
    case maps:get(Category, State#state.categories, undefined) of
        undefined ->
            {reply, {error, {invalid_category, Category}}, State};
        Types ->
            {reply, {ok, Types}, State}
    end;

handle_call({validate_diagram, TypeName, DiagramCode}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, {unknown_type, TypeName}}, State};
        DiagramType ->
            Rules = DiagramType#diagram_type.validation_rules,
            Result = apply_validation_rules(Rules, DiagramCode),
            {reply, Result, State}
    end;

handle_call({get_parser, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #diagram_type{parser_module = undefined} ->
            {reply, {error, {no_parser, TypeName}}, State};
        #diagram_type{parser_module = Parser} ->
            {reply, {ok, Parser}, State}
    end;

handle_call({get_renderer, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #diagram_type{renderer_module = undefined} ->
            {reply, {error, {no_renderer, TypeName}}, State};
        #diagram_type{renderer_module = Renderer} ->
            {reply, {ok, Renderer}, State}
    end;

handle_call({convert_type, SourceType, TargetType}, _From, State) ->
    case {maps:get(SourceType, State#state.types, undefined),
          maps:get(TargetType, State#state.types, undefined)} of
        {#diagram_type{} = Source, #diagram_type{} = Target} ->
            Result = check_conversion_compatibility(Source, Target),
            {reply, Result, State};
        {undefined, _} ->
            {reply, {error, {unknown_source_type, SourceType}}, State};
        {_, undefined} ->
            {reply, {error, {unknown_target_type, TargetType}}, State}
    end;

handle_call({get_type_metadata, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        DiagramType ->
            {reply, {ok, DiagramType#diagram_type.metadata}, State}
    end;

handle_call({get_compatible_types, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        DiagramType ->
            Category = DiagramType#diagram_type.category,
            Compatible = maps:get(Category, State#state.categories, []),
            {reply, {ok, Compatible}, State}
    end;

handle_call({check_type_support, TypeName, Feature}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #diagram_type{metadata = Metadata} ->
            SupportedFeatures = maps:get(features, Metadata, #{}),
            Result = maps:get(Feature, SupportedFeatures, false),
            {reply, Result, State}
    end;

handle_call({get_type_inheritance, TypeName}, _From, State) ->
    case maps:get(TypeName, State#state.types, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #diagram_type{parent_type = undefined} ->
            {reply, {ok, [TypeName]}, State};
        #diagram_type{parent_type = Parent} ->
            Inheritance = build_inheritance_chain(TypeName, Parent, State),
            {reply, {ok, Inheritance}, State}
    end;

handle_call(get_all_metadata, _From, State) ->
    AllMetadata = maps:map(
        fun(_TypeName, DiagramType) ->
            DiagramType#diagram_type.metadata
        end,
        State#state.types
    ),
    {reply, {ok, AllMetadata}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_continue(term(), state()) -> {noreply, state()}.
handle_continue(initialize_types, State) ->
    logger:info("Initializing Mermaid diagram types"),
    InitializedState = initialize_builtin_types(State),
    logger:info("Mermaid registry initialized with ~p diagram types", [
        maps:size(InitializedState#state.types)
    ]),
    {noreply, InitializedState};

handle_continue(_Continue, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Mermaid diagram type registry terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate metadata structure
-spec validate_metadata(metadata()) -> ok | {error, term()}.
validate_metadata(Metadata) ->
    RequiredFields = [name, category],
    case lists:all(fun(F) -> maps:is_key(F, Metadata) end, RequiredFields) of
        true ->
            case maps:get(name, Metadata) of
                Name when is_binary(Name), byte_size(Name) > 0 -> ok;
                _ -> {error, {invalid_metadata, name_required}}
            end;
        false ->
            {error, {invalid_metadata, missing_required_fields}}
    end.

%% @doc Validate list of metadata
-spec validate_all_metadata([metadata()]) -> ok | {error, term()}.
validate_all_metadata(MetadataList) ->
    ValidateFun = fun(Metadata, Acc) ->
        case validate_metadata(Metadata) of
            ok -> Acc;
            Error -> Error
        end
    end,
    lists:foldl(ValidateFun, ok, MetadataList).

%% @doc Build diagram type record from metadata
-spec build_diagram_type(diagram_type(), metadata()) -> #diagram_type{}.
build_diagram_type(Name, Metadata) ->
    #diagram_type{
        name = Name,
        category = maps:get(category, Metadata),
        parser_module = maps:get(parser_module, Metadata, undefined),
        renderer_module = maps:get(renderer_module, Metadata, undefined),
        validation_rules = maps:get(validation_rules, Metadata, #{}),
        metadata = Metadata,
        parent_type = maps:get(parent_type, Metadata, undefined),
        aliases = maps:get(aliases, Metadata, [])
    }.

%% @doc Store diagram type in state
-spec store_type(#diagram_type{}, state()) -> state().
store_type(#diagram_type{} = DiagramType, State) ->
    TypeName = DiagramType#diagram_type.name,
    Category = DiagramType#diagram_type.category,

    % Store in types map
    NewTypes = maps:put(TypeName, DiagramType, State#state.types),

    % Update category index
    NewCategories = maps:update_with(
        Category,
        fun(Types) -> lists:usort([TypeName | Types]) end,
        [TypeName],
        State#state.categories
    ),

    % Update parser index
    NewParsers = case DiagramType#diagram_type.parser_module of
        undefined -> State#state.parsers;
        Parser ->
            maps:update_with(
                Parser,
                fun(Types) -> lists:usort([TypeName | Types]) end,
                [TypeName],
                State#state.parsers
            )
    end,

    % Update renderer index
    NewRenderers = case DiagramType#diagram_type.renderer_module of
        undefined -> State#state.renderers;
        Renderer ->
            maps:update_with(
                Renderer,
                fun(Types) -> lists:usort([TypeName | Types]) end,
                [TypeName],
                State#state.renderers
            )
    end,

    State#state{
        types = NewTypes,
        categories = NewCategories,
        parsers = NewParsers,
        renderers = NewRenderers
    }.

%% @doc Remove diagram type from state
-spec remove_type(#diagram_type{}, state()) -> state().
remove_type(#diagram_type{} = DiagramType, State) ->
    TypeName = DiagramType#diagram_type.name,
    Category = DiagramType#diagram_type.category,

    % Remove from types map
    NewTypes = maps:remove(TypeName, State#state.types),

    % Update category index
    NewCategories = maps:update_with(
        Category,
        fun(Types) -> lists:delete(TypeName, Types) end,
        State#state.categories
    ),

    % Update parser index
    NewParsers = case DiagramType#diagram_type.parser_module of
        undefined -> State#state.parsers;
        Parser ->
            maps:update_with(
                Parser,
                fun(Types) -> lists:delete(TypeName, Types) end,
                State#state.parsers
            )
    end,

    % Update renderer index
    NewRenderers = case DiagramType#diagram_type.renderer_module of
        undefined -> State#state.renderers;
        Renderer ->
            maps:update_with(
                Renderer,
                fun(Types) -> lists:delete(TypeName, Types) end,
                State#state.renderers
            )
    end,

    State#state{
        types = NewTypes,
        categories = NewCategories,
        parsers = NewParsers,
        renderers = NewRenderers
    }.

%% @doc Apply validation rules to diagram code
-spec apply_validation_rules(validation_rules(), binary()) -> ok | {error, term()}.
apply_validation_rules(Rules, Code) ->
    % Check minimum length
    case maps:get(min_length, Rules, undefined) of
        MinLen when is_integer(MinLen), byte_size(Code) < MinLen ->
            {error, {code_too_short, MinLen}};
        _ ->
            % Check maximum length
            case maps:get(max_length, Rules, undefined) of
                MaxLen when is_integer(MaxLen), byte_size(Code) > MaxLen ->
                    {error, {code_too_long, MaxLen}};
                _ ->
                    % Check required keywords
                    case maps:get(required_keywords, Rules, undefined) of
                        undefined -> ok;
                        Keywords when is_list(Keywords) ->
                            case lists:all(fun(KW) ->
                                binary:match(Code, KW) =/= nomatch
                            end, Keywords) of
                                true -> ok;
                                false -> {error, {missing_required_keywords, Keywords}}
                            end
                    end
            end
    end.

%% @doc Check conversion compatibility between types
-spec check_conversion_compatibility(#diagram_type{}, #diagram_type{}) ->
    {ok, binary()} | {error, term()}.
check_conversion_compatibility(Source, Target) ->
    SourceCategory = Source#diagram_type.category,
    TargetCategory = Target#diagram_type.category,

    case {SourceCategory, TargetCategory} of
        {Same, Same} ->
            {ok, <<"Direct conversion supported within same category">>};
        {flowchart, sequence} ->
            {ok, <<"Flowchart to sequence conversion (limited)">>};
        {class, flowchart} ->
            {ok, <<"Class to flowchart conversion">>};
        {state, flowchart} ->
            {ok, <<"State to flowchart conversion">>};
        {_, _} ->
            {error, {incompatible_types, SourceCategory, TargetCategory}}
    end.

%% @doc Build inheritance chain for a type
-spec build_inheritance_chain(diagram_type(), diagram_type(), state()) -> [diagram_type()].
build_inheritance_chain(TypeName, ParentName, State) ->
    case maps:get(ParentName, State#state.types, undefined) of
        undefined ->
            [TypeName];
        #diagram_type{parent_type = undefined} ->
            [TypeName, ParentName];
        #diagram_type{parent_type = GrandParent} ->
            [TypeName | build_inheritance_chain(ParentName, GrandParent, State)]
    end.

%% @doc Initialize built-in Mermaid diagram types
-spec initialize_builtin_types(state()) -> state().
initialize_builtin_types(State) ->
    BuiltinTypes = get_builtin_type_definitions(),
    lists:foldl(
        fun(Metadata, AccState) ->
            TypeName = maps:get(name, Metadata),
            DiagramType = build_diagram_type(TypeName, Metadata),
            store_type(DiagramType, AccState)
        end,
        State,
        BuiltinTypes
    ).

%% @doc Get built-in Mermaid diagram type definitions
-spec get_builtin_type_definitions() -> [metadata()].
get_builtin_type_definitions() ->
    [
        %% Flowchart Category
        #{
            name => <<"flowchart">>,
            category => flowchart,
            description => <<"Flowchart diagram with nodes and edges">>,
            parser_module => erlmcp_mermaid_parser_flowchart,
            renderer_module => erlmcp_mermaid_renderer_flowchart,
            validation_rules => #{
                min_length => 10,
                max_length => 1000000,
                required_keywords => [<<"graph">>, <<"--">>]
            },
            aliases => [<<"flow">>, <<"graph">>],
            features => #{
                subgraphs => true,
                styling => true,
                icons => true,
                links => true
            },
            documentation => #{
                syntax => <<"graph TD\n    A[Start] --> B[Process]\n    B --> C[End]">>,
                examples => [
                    <<"graph LR\n    A-->B\n    B-->C">>,
                    <<"graph TB\n    subgraph One\n    A1-->A2\n    end">>
                ]
            }
        },
        #{
            name => <<"flowchart_td">>,
            category => flowchart,
            description => <<"Top-down flowchart">>,
            parser_module => erlmcp_mermaid_parser_flowchart,
            renderer_module => erlmcp_mermaid_renderer_flowchart,
            parent_type => <<"flowchart">>,
            validation_rules => #{
                min_length => 10,
                required_keywords => [<<"graph TD">>]
            },
            aliases => [<<"td">>],
            features => #{
                subgraphs => true,
                styling => true
            }
        },
        #{
            name => <<"flowchart_lr">>,
            category => flowchart,
            description => <<"Left-right flowchart">>,
            parser_module => erlmcp_mermaid_parser_flowchart,
            renderer_module => erlmcp_mermaid_renderer_flowchart,
            parent_type => <<"flowchart">>,
            validation_rules => #{
                min_length => 10,
                required_keywords => [<<"graph LR">>]
            },
            aliases => [<<"lr">>],
            features => #{
                subgraphs => true,
                styling => true
            }
        },

        %% Sequence Diagram Category
        #{
            name => <<"sequence">>,
            category => sequence,
            description => <<"Sequence diagram showing interactions">>,
            parser_module => erlmcp_mermaid_parser_sequence,
            renderer_module => erlmcp_mermaid_renderer_sequence,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"sequenceDiagram">>, <<"->>">>]
            },
            aliases => [<<"seq">>, <<"seqdiag">>],
            features => #{
                participants => true,
                messages => true,
                loops => true,
                alt_blocks => true,
                par_blocks => true
            },
            documentation => #{
                syntax => <<"sequenceDiagram\n    Alice->>Bob: Hello\n    Bob-->>Alice: Hi">>,
                examples => [
                    <<"sequenceDiagram\n    A->>B: Request\n    B-->>A: Response">>
                ]
            }
        },

        %% Class Diagram Category
        #{
            name => <<"class">>,
            category => class,
            description => <<"Class diagram for software design">>,
            parser_module => erlmcp_mermaid_parser_class,
            renderer_module => erlmcp_mermaid_renderer_class,
            validation_rules => #{
                min_length => 30,
                required_keywords => [<<"classDiagram">>]
            },
            aliases => [<<"classdiag">>, <<"uml">>],
            features => #{
                classes => true,
                relationships => true,
                methods => true,
                properties => true,
                generics => true
            },
            documentation => #{
                syntax => <<"classDiagram\n    class Animal{\n        +String name\n    }">>
            }
        },

        %% State Diagram Category
        #{
            name => <<"state">>,
            category => state,
            description => <<"State machine diagram">>,
            parser_module => erlmcp_mermaid_parser_state,
            renderer_module => erlmcp_mermaid_renderer_state,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"stateDiagram">>]
            },
            aliases => [<<"statemachine">>, <<"stm">>],
            features => #{
                states => true,
                transitions => true,
                composite_states => true,
                concurrent_states => true
            }
        },

        %% Entity Relationship Category
        #{
            name => <<"er">>,
            category => er,
            description => <<"Entity relationship diagram">>,
            parser_module => erlmcp_mermaid_parser_er,
            renderer_module => erlmcp_mermaid_renderer_er,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"erDiagram">>]
            },
            aliases => [<<"entity_relationship">>, <<"erd">>],
            features => #{
                entities => true,
                relationships => true,
                cardinality => true
            }
        },

        %% Gantt Chart Category
        #{
            name => <<"gantt">>,
            category => gantt,
            description => <<"Gantt chart for project planning">>,
            parser_module => erlmcp_mermaid_parser_gantt,
            renderer_module => erlmcp_mermaid_renderer_gantt,
            validation_rules => #{
                min_length => 30,
                required_keywords => [<<"gantt">>]
            },
            aliases => [<<"project">>, <<"timeline">>],
            features => #{
                tasks => true,
                milestones => true,
                dependencies => true,
                sections => true
            }
        },

        %% Pie Chart Category
        #{
            name => <<"pie">>,
            category => pie,
            description => <<"Pie chart for data visualization">>,
            parser_module => erlmcp_mermaid_parser_pie,
            renderer_module => erlmcp_mermaid_renderer_pie,
            validation_rules => #{
                min_length => 15,
                required_keywords => [<<"pie">>]
            },
            aliases => [<<"piechart">>],
            features => #{
                data_labels => true,
                show_percentages => true
            }
        },

        %% Relationship Diagram Category
        #{
            name => <<"relationship">>,
            category => relationship,
            description => <<"Relationship diagram">>,
            parser_module => erlmcp_mermaid_parser_relationship,
            renderer_module => erlmcp_mermaid_renderer_relationship,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"relationship">>]
            },
            features => #{
                entities => true,
                connections => true
            }
        },

        %% Mindmap Category
        #{
            name => <<"mindmap">>,
            category => mindmap,
            description => <<"Mind map for brainstorming">>,
            parser_module => erlmcp_mermaid_parser_mindmap,
            renderer_module => erlmcp_mermaid_renderer_mindmap,
            validation_rules => #{
                min_length => 15,
                required_keywords => [<<"mindmap">>]
            },
            aliases => [<<"brainstorm">>],
            features => #{
                branches => true,
                nesting => true,
                icons => true
            }
        },

        %% Timeline Category
        #{
            name => <<"timeline">>,
            category => timeline,
            description => <<"Timeline visualization">>,
            parser_module => erlmcp_mermaid_parser_timeline,
            renderer_module => erlmcp_mermaid_renderer_timeline,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"timeline">>]
            },
            aliases => [<<"chronology">>],
            features => #{
                events => true,
                dates => true,
                sections => true
            }
        },

        %% Git Category
        #{
            name => <<"git">>,
            category => git,
            description => <<"Git graph visualization">>,
            parser_module => erlmcp_mermaid_parser_git,
            renderer_module => erlmcp_mermaid_renderer_git,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"gitGraph">>]
            },
            aliases => [<<"gitgraph">>],
            features => #{
                commits => true,
                branches => true,
                merges => true,
                checkout => true
            }
        },

        %% User Journey Category
        #{
            name => <<"journey">>,
            category => user,
            description => <<"User journey map">>,
            parser_module => erlmcp_mermaid_parser_journey,
            renderer_module => erlmcp_mermaid_renderer_journey,
            validation_rules => #{
                min_length => 30,
                required_keywords => [<<"journey">>]
            },
            aliases => [<<"user_journey">>, <<"userjourney">>],
            features => #{
                tasks => true,
                sections => true,
                actors => true
            }
        },

        %% Architecture Category
        #{
            name => <<"c4">>,
            category => architecture,
            description => <<"C4 architecture diagram">>,
            parser_module => erlmcp_mermaid_parser_c4,
            renderer_module => erlmcp_mermaid_renderer_c4,
            validation_rules => #{
                min_length => 30,
                required_keywords => [<<"C4">>]
            },
            aliases => [<<"architecture">>, <<"c4context">>],
            features => #{
                contexts => true,
                containers => true,
                components => true,
                relationships => true
            }
        },
        #{
            name => <<"architecture_context">>,
            category => architecture,
            description => <<"C4 context diagram">>,
            parser_module => erlmcp_mermaid_parser_c4,
            renderer_module => erlmcp_mermaid_renderer_c4,
            parent_type => <<"c4">>,
            validation_rules => #{
                required_keywords => [<<"context">>]
            },
            aliases => [<<"context">>]
        },
        #{
            name => <<"architecture_container">>,
            category => architecture,
            description => <<"C4 container diagram">>,
            parser_module => erlmcp_mermaid_parser_c4,
            renderer_module => erlmcp_mermaid_renderer_c4,
            parent_type => <<"c4">>,
            validation_rules => #{
                required_keywords => [<<"container">>]
            },
            aliases => [<<"container">>]
        },

        %% Block Diagram Category
        #{
            name => <<"block">>,
            category => architecture,
            description => <<"Block diagram">>,
            parser_module => erlmcp_mermaid_parser_block,
            renderer_module => erlmcp_mermaid_renderer_block,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"block">>]
            },
            features => #{
                blocks => true,
                arrows => true,
                labels => true
            }
        },

        %% Network Category
        #{
            name => <<"network">>,
            category => architecture,
            description => <<"Network diagram">>,
            parser_module => erlmcp_mermaid_parser_network,
            renderer_module => erlmcp_mermaid_renderer_network,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"network">>]
            },
            features => #{
                nodes => true,
                connections => true,
                subnets => true
            }
        },

        %% Requirement Category
        #{
            name => <<"requirement">>,
            category => user,
            description => <<"Requirement diagram">>,
            parser_module => erlmcp_mermaid_parser_requirement,
            renderer_module => erlmcp_mermaid_renderer_requirement,
            validation_rules => #{
                min_length => 30,
                required_keywords => [<<"requirementDiagram">>]
            },
            aliases => [<<"req">>, <<"requirements">>],
            features => #{
                requirements => true,
                relationships => true,
                elements => true
            }
        },

        %% JSON Data Tree Category
        #{
            name => <<"json">>,
            category => architecture,
            description => <<"JSON data tree visualization">>,
            parser_module => erlmcp_mermaid_parser_json,
            renderer_module => erlmcp_mermaid_renderer_json,
            validation_rules => #{
                min_length => 15,
                required_keywords => [<<"json">>]
            },
            features => #{
                nested_data => true,
                arrays => true,
                objects => true
            }
        },

        %% YAML Data Tree Category
        #{
            name => <<"yaml">>,
            category => architecture,
            description => <<"YAML data tree visualization">>,
            parser_module => erlmcp_mermaid_parser_yaml,
            renderer_module => erlmcp_mermaid_renderer_yaml,
            validation_rules => #{
                min_length => 15,
                required_keywords => [<<"yaml">>]
            },
            features => #{
                nested_data => true,
                sequences => true,
                mappings => true
            }
        },

        %% XY Chart Category
        #{
            name => <<"xychart">>,
            category => pie,
            description => <<"XY chart for data visualization">>,
            parser_module => erlmcp_mermaid_parser_xychart,
            renderer_module => erlmcp_mermaid_renderer_xychart,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"xychart-beta">>]
            },
            aliases => [<<"xy">>],
            features => #{
                x_axis => true,
                y_axis => true,
                series => true
            }
        },

        %% Sankey Diagram Category
        #{
            name => <<"sankey">>,
            category => pie,
            description => <<"Sankey diagram for flow visualization">>,
            parser_module => erlmcp_mermaid_parser_sankey,
            renderer_module => erlmcp_mermaid_renderer_sankey,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"sankey-beta">>]
            },
            features => #{
                flows => true,
                nodes => true,
                quantities => true
            }
        },

        %% Packet Diagram Category
        #{
            name => <<"packet">>,
            category => architecture,
            description => <<"Packet diagram for network protocols">>,
            parser_module => erlmcp_mermaid_parser_packet,
            renderer_module => erlmcp_mermaid_renderer_packet,
            validation_rules => #{
                min_length => 15,
                required_keywords => [<<"packet">>]
            },
            features => #{
                layers => true,
                headers => true,
                payloads => true
            }
        },

        %% Decision Tree Category
        #{
            name => <<"decision">>,
            category => flowchart,
            description => <<"Decision tree diagram">>,
            parser_module => erlmcp_mermaid_parser_decision,
            renderer_module => erlmcp_mermaid_renderer_decision,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"graph">>]
            },
            parent_type => <<"flowchart">>,
            features => #{
                decisions => true,
                outcomes => true,
                conditions => true
            }
        },

        %% Organizational Chart Category
        #{
            name => <<"org">>,
            category => architecture,
            description => <<"Organizational chart">>,
            parser_module => erlmcp_mermaid_parser_org,
            renderer_module => erlmcp_mermaid_renderer_org,
            validation_rules => #{
                min_length => 20,
                required_keywords => [<<"graph">>]
            },
            parent_type => <<"flowchart">>,
            aliases => [<<"orgchart">>, <<"organization">>],
            features => #{
                managers => true,
                subordinates => true,
                departments => true
            }
        },

        %% Skateboard Category
        #{
            name => <<"sketch">>,
            category => architecture,
            description => <<"Sketch/hand-drawn style diagram">>,
            parser_module => erlmcp_mermaid_parser_sketch,
            renderer_module => erlmcp_mermaid_renderer_sketch,
            validation_rules => #{
                min_length => 15
            },
            aliases => [<<"handdrawn">>],
            features => #{
                rough_edges => true,
                custom_style => true
            }
        }
    ].
