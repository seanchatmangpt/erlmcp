-module(memory_server).
-behaviour(gen_server).

-include("memory_server.hrl").

%% API
-export([start_link/0, start_link/1]).

%% Public API for operations
-export([
    create_entities/1,
    create_relations/1,
    add_observations/2,
    delete_entities/1,
    delete_observations/2,
    delete_relations/1,
    read_graph/0,
    search_nodes/1,
    open_nodes/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_SAVE_INTERVAL, 5000).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Config], []).

%% Public API functions
create_entities(Entities) ->
    gen_server:call(?SERVER, {create_entities, Entities}).

create_relations(Relations) ->
    gen_server:call(?SERVER, {create_relations, Relations}).

add_observations(EntityName, Observations) ->
    gen_server:call(?SERVER, {add_observations, EntityName, Observations}).

delete_entities(EntityNames) ->
    gen_server:call(?SERVER, {delete_entities, EntityNames}).

delete_observations(EntityName, Observations) ->
    gen_server:call(?SERVER, {delete_observations, EntityName, Observations}).

delete_relations(Relations) ->
    gen_server:call(?SERVER, {delete_relations, Relations}).

read_graph() ->
    gen_server:call(?SERVER, read_graph).

search_nodes(Query) ->
    gen_server:call(?SERVER, {search_nodes, Query}).

open_nodes(EntityNames) ->
    gen_server:call(?SERVER, {open_nodes, EntityNames}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Config]) ->
    process_flag(trap_exit, true),
    
    %% Get configuration
    MemoryFilePath = maps:get(memory_file_path, Config, 
                              application:get_env(memory_server, memory_file_path, "memory.json")),
    AutoSave = maps:get(auto_save, Config,
                        application:get_env(memory_server, auto_save, true)),
    SaveInterval = maps:get(save_interval, Config,
                            application:get_env(memory_server, save_interval, ?DEFAULT_SAVE_INTERVAL)),
    
    %% Convert to binary if needed
    FilePath = ensure_binary(MemoryFilePath),
    
    %% Load existing graph
    KnowledgeGraph = load_graph(FilePath),
    
    %% Schedule auto-save if enabled
    SaveTimer = case AutoSave of
        true -> 
            erlang:send_after(SaveInterval, self(), auto_save);
        false -> 
            undefined
    end,
    
    State = #state{
        knowledge_graph = KnowledgeGraph,
        memory_file_path = FilePath,
        mcp_server = undefined,
        save_timer = SaveTimer,
        auto_save = AutoSave,
        save_interval = SaveInterval,
        dirty = false
    },
    
    logger:info("Memory Server started with file: ~s", [FilePath]),
    
    {ok, State}.

handle_call({create_entities, Entities}, _From, State) ->
    {Result, NewState} = create_entities_impl(Entities, State),
    {reply, Result, mark_dirty(NewState)};

handle_call({create_relations, Relations}, _From, State) ->
    {Result, NewState} = create_relations_impl(Relations, State),
    {reply, Result, mark_dirty(NewState)};

handle_call({add_observations, EntityName, Observations}, _From, State) ->
    {Result, NewState} = add_observations_impl(EntityName, Observations, State),
    {reply, Result, mark_dirty(NewState)};

handle_call({delete_entities, EntityNames}, _From, State) ->
    {Result, NewState} = delete_entities_impl(EntityNames, State),
    {reply, Result, mark_dirty(NewState)};

handle_call({delete_observations, EntityName, Observations}, _From, State) ->
    {Result, NewState} = delete_observations_impl(EntityName, Observations, State),
    {reply, Result, mark_dirty(NewState)};

handle_call({delete_relations, Relations}, _From, State) ->
    {Result, NewState} = delete_relations_impl(Relations, State),
    {reply, Result, mark_dirty(NewState)};

handle_call(read_graph, _From, State) ->
    Result = read_graph_impl(State),
    {reply, Result, State};

handle_call({search_nodes, Query}, _From, State) ->
    Result = search_nodes_impl(Query, State),
    {reply, Result, State};

handle_call({open_nodes, EntityNames}, _From, State) ->
    Result = open_nodes_impl(EntityNames, State),
    {reply, Result, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State}.

handle_cast(save_now, State) ->
    NewState = save_if_dirty(State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(auto_save, #state{auto_save = true, save_interval = Interval} = State) ->
    %% Save if dirty
    NewState = save_if_dirty(State),
    
    %% Schedule next save
    Timer = erlang:send_after(Interval, self(), auto_save),
    
    {noreply, NewState#state{save_timer = Timer}};

handle_info(auto_save, State) ->
    %% Auto-save disabled, ignore
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel save timer
    case State#state.save_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,
    
    %% Save final state if dirty
    save_if_dirty(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - State Management
%%====================================================================

mark_dirty(State) ->
    State#state{dirty = true}.

save_if_dirty(#state{dirty = false} = State) ->
    State;
save_if_dirty(#state{dirty = true} = State) ->
    case save_graph(State#state.knowledge_graph, State#state.memory_file_path) of
        ok ->
            State#state{dirty = false};
        {error, _Reason} ->
            logger:error("Failed to save knowledge graph: ~p", [_Reason]),
            State
    end.

ensure_binary(Path) when is_binary(Path) -> Path;
ensure_binary(Path) when is_list(Path) -> list_to_binary(Path);
ensure_binary(Path) -> iolist_to_binary(io_lib:format("~p", [Path])).

%%====================================================================
%% File I/O Functions
%%====================================================================

load_graph(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
            parse_graph_lines(Lines);
        {error, enoent} ->
            logger:info("Memory file not found, starting with empty graph"),
            #knowledge_graph{entities = [], relations = []};
        {error, Reason} ->
            logger:error("Failed to load graph from ~s: ~p", [FilePath, Reason]),
            #knowledge_graph{entities = [], relations = []}
    end.

parse_graph_lines(Lines) ->
    parse_graph_lines(Lines, [], []).

parse_graph_lines([], Entities, Relations) ->
    #knowledge_graph{
        entities = lists:reverse(Entities), 
        relations = lists:reverse(Relations)
    };
parse_graph_lines([Line | Rest], Entities, Relations) ->
    case Line of
        <<>> ->
            parse_graph_lines(Rest, Entities, Relations);
        _ ->
            try jsx:decode(Line, [return_maps]) of
                #{<<"type">> := <<"entity">>, <<"name">> := Name, 
                  <<"entityType">> := Type} = EntityMap ->
                    Observations = maps:get(<<"observations">>, EntityMap, []),
                    Entity = #entity{
                        name = Name, 
                        entity_type = Type, 
                        observations = Observations
                    },
                    parse_graph_lines(Rest, [Entity | Entities], Relations);
                #{<<"type">> := <<"relation">>, <<"from">> := From, 
                  <<"to">> := To, <<"relationType">> := RelType} ->
                    Relation = #relation{
                        from = From, 
                        to = To, 
                        relation_type = RelType
                    },
                    parse_graph_lines(Rest, Entities, [Relation | Relations]);
                _ ->
                    logger:warning("Skipping invalid line in memory file: ~s", [Line]),
                    parse_graph_lines(Rest, Entities, Relations)
            catch
                error:badarg ->
                    logger:warning("Failed to parse JSON line: ~s", [Line]),
                    parse_graph_lines(Rest, Entities, Relations)
            end
    end.

save_graph(KnowledgeGraph, FilePath) ->
    Lines = format_graph_lines(KnowledgeGraph),
    Data = case Lines of
        [] -> <<>>;
        _ -> iolist_to_binary([lists:join(<<"\n">>, Lines), <<"\n">>])
    end,
    
    %% Write to temporary file first
    TempFile = <<FilePath/binary, ".tmp">>,
    case file:write_file(TempFile, Data) of
        ok ->
            %% Atomic rename
            case file:rename(TempFile, FilePath) of
                ok -> 
                    logger:debug("Knowledge graph saved to ~s", [FilePath]),
                    ok;
                {error, _Reason} = Error ->
                    file:delete(TempFile),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

format_graph_lines(#knowledge_graph{entities = Entities, relations = Relations}) ->
    EntityLines = [jsx:encode(#{
        <<"type">> => <<"entity">>,
        <<"name">> => Entity#entity.name,
        <<"entityType">> => Entity#entity.entity_type,
        <<"observations">> => Entity#entity.observations
    }) || Entity <- Entities],
    
    RelationLines = [jsx:encode(#{
        <<"type">> => <<"relation">>,
        <<"from">> => Relation#relation.from,
        <<"to">> => Relation#relation.to,
        <<"relationType">> => Relation#relation.relation_type
    }) || Relation <- Relations],
    
    EntityLines ++ RelationLines.

%%====================================================================
%% Implementation Functions
%%====================================================================

create_entities_impl(EntitiesJson, State) ->
    try
        Entities = [#entity{
            name = maps:get(<<"name">>, EntityMap),
            entity_type = maps:get(<<"entityType">>, EntityMap),
            observations = maps:get(<<"observations">>, EntityMap, [])
        } || EntityMap <- EntitiesJson],
        
        KnowledgeGraph = State#state.knowledge_graph,
        ExistingNames = [E#entity.name || E <- KnowledgeGraph#knowledge_graph.entities],
        
        %% Filter out duplicates
        NewEntities = [E || E <- Entities, not lists:member(E#entity.name, ExistingNames)],
        
        UpdatedEntities = KnowledgeGraph#knowledge_graph.entities ++ NewEntities,
        UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, length(NewEntities)}, NewState}
    catch
        Class:Error:Stack ->
            logger:error("Failed to create entities: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

create_relations_impl(RelationsJson, State) ->
    try
        Relations = [#relation{
            from = maps:get(<<"from">>, RelationMap),
            to = maps:get(<<"to">>, RelationMap),
            relation_type = maps:get(<<"relationType">>, RelationMap)
        } || RelationMap <- RelationsJson],
        
        KnowledgeGraph = State#state.knowledge_graph,
        ExistingRelations = KnowledgeGraph#knowledge_graph.relations,
        
        %% Filter out duplicates
        NewRelations = [R || R <- Relations, not relation_exists(R, ExistingRelations)],
        
        UpdatedRelations = ExistingRelations ++ NewRelations,
        UpdatedGraph = KnowledgeGraph#knowledge_graph{relations = UpdatedRelations},
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, length(NewRelations)}, NewState}
    catch
        Class:Error:Stack ->
            logger:error("Failed to create relations: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

add_observations_impl(EntityName, Observations, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        Entities = KnowledgeGraph#knowledge_graph.entities,
        
        case lists:keyfind(EntityName, #entity.name, Entities) of
            false ->
                {{error, entity_not_found}, State};
            Entity ->
                ExistingObs = Entity#entity.observations,
                NewObs = [Obs || Obs <- Observations, not lists:member(Obs, ExistingObs)],
                UpdatedObs = ExistingObs ++ NewObs,
                UpdatedEntity = Entity#entity{observations = UpdatedObs},
                
                UpdatedEntities = lists:keyreplace(EntityName, #entity.name, 
                                                   Entities, UpdatedEntity),
                UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
                
                NewState = State#state{knowledge_graph = UpdatedGraph},
                {{ok, length(NewObs)}, NewState}
        end
    catch
        Class:Error:Stack ->
            logger:error("Failed to add observations: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

delete_entities_impl(EntityNames, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        Entities = KnowledgeGraph#knowledge_graph.entities,
        Relations = KnowledgeGraph#knowledge_graph.relations,
        
        %% Delete entities
        {RemainingEntities, DeletedCount} = lists:foldl(fun(Name, {Acc, Count}) ->
            case lists:keymember(Name, #entity.name, Acc) of
                true ->
                    {lists:keydelete(Name, #entity.name, Acc), Count + 1};
                false ->
                    {Acc, Count}
            end
        end, {Entities, 0}, EntityNames),
        
        %% Delete related relations
        RemainingRelations = [R || R <- Relations, 
            not lists:member(R#relation.from, EntityNames) andalso 
            not lists:member(R#relation.to, EntityNames)],
        
        UpdatedGraph = KnowledgeGraph#knowledge_graph{
            entities = RemainingEntities,
            relations = RemainingRelations
        },
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, DeletedCount}, NewState}
    catch
        Class:Error:Stack ->
            logger:error("Failed to delete entities: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

delete_observations_impl(EntityName, Observations, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        Entities = KnowledgeGraph#knowledge_graph.entities,
        
        case lists:keyfind(EntityName, #entity.name, Entities) of
            false ->
                {{error, entity_not_found}, State};
            Entity ->
                ExistingObs = Entity#entity.observations,
                {RemainingObs, DeletedCount} = lists:foldl(fun(Obs, {Acc, Count}) ->
                    case lists:member(Obs, Acc) of
                        true ->
                            {lists:delete(Obs, Acc), Count + 1};
                        false ->
                            {Acc, Count}
                    end
                end, {ExistingObs, 0}, Observations),
                
                UpdatedEntity = Entity#entity{observations = RemainingObs},
                UpdatedEntities = lists:keyreplace(EntityName, #entity.name, 
                                                   Entities, UpdatedEntity),
                UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
                
                NewState = State#state{knowledge_graph = UpdatedGraph},
                {{ok, DeletedCount}, NewState}
        end
    catch
        Class:Error:Stack ->
            logger:error("Failed to delete observations: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

delete_relations_impl(RelationsJson, State) ->
    try
        Relations = [#relation{
            from = maps:get(<<"from">>, RelationMap),
            to = maps:get(<<"to">>, RelationMap),
            relation_type = maps:get(<<"relationType">>, RelationMap)
        } || RelationMap <- RelationsJson],
        
        KnowledgeGraph = State#state.knowledge_graph,
        ExistingRelations = KnowledgeGraph#knowledge_graph.relations,
        
        {RemainingRelations, DeletedCount} = lists:foldl(fun(Rel, {Acc, Count}) ->
            case relation_exists(Rel, Acc) of
                true ->
                    {remove_relation(Rel, Acc), Count + 1};
                false ->
                    {Acc, Count}
            end
        end, {ExistingRelations, 0}, Relations),
        
        UpdatedGraph = KnowledgeGraph#knowledge_graph{relations = RemainingRelations},
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, DeletedCount}, NewState}
    catch
        Class:Error:Stack ->
            logger:error("Failed to delete relations: ~p:~p~n~p", [Class, Error, Stack]),
            {{error, Error}, State}
    end.

read_graph_impl(State) ->
    try
        GraphJson = knowledge_graph_to_json(State#state.knowledge_graph),
        {ok, GraphJson}
    catch
        Class:Error:Stack ->
            logger:error("Failed to read graph: ~p:~p~n~p", [Class, Error, Stack]),
            {error, Error}
    end.

search_nodes_impl(Query, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        LowerQuery = string:lowercase(binary_to_list(Query)),
        
        MatchingEntities = [entity_to_json(E) || E <- KnowledgeGraph#knowledge_graph.entities,
            entity_matches_query(E, LowerQuery)],
        
        MatchingRelations = [relation_to_json(R) || R <- KnowledgeGraph#knowledge_graph.relations,
            relation_matches_query(R, LowerQuery)],
        
        Results = #{
            <<"entities">> => MatchingEntities,
            <<"relations">> => MatchingRelations
        },
        
        {ok, Results}
    catch
        Class:Error:Stack ->
            logger:error("Failed to search nodes: ~p:~p~n~p", [Class, Error, Stack]),
            {error, Error}
    end.

open_nodes_impl(EntityNames, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        Entities = KnowledgeGraph#knowledge_graph.entities,
        Relations = KnowledgeGraph#knowledge_graph.relations,
        
        FoundEntities = [entity_to_json(E) || E <- Entities,
            lists:member(E#entity.name, EntityNames)],
        
        RelatedRelations = [relation_to_json(R) || R <- Relations,
            lists:member(R#relation.from, EntityNames) orelse
            lists:member(R#relation.to, EntityNames)],
        
        Results = #{
            <<"entities">> => FoundEntities,
            <<"relations">> => RelatedRelations
        },
        
        {ok, Results}
    catch
        Class:Error:Stack ->
            logger:error("Failed to open nodes: ~p:~p~n~p", [Class, Error, Stack]),
            {error, Error}
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

relation_exists(#relation{from = From, to = To, relation_type = Type}, Relations) ->
    lists:any(fun(#relation{from = F, to = T, relation_type = Ty}) ->
        F =:= From andalso T =:= To andalso Ty =:= Type
    end, Relations).

remove_relation(#relation{from = From, to = To, relation_type = Type}, Relations) ->
    lists:filter(fun(#relation{from = F, to = T, relation_type = Ty}) ->
        not (F =:= From andalso T =:= To andalso Ty =:= Type)
    end, Relations).

entity_matches_query(#entity{name = Name, entity_type = Type, observations = Obs}, Query) ->
    string:find(string:lowercase(binary_to_list(Name)), Query) =/= nomatch orelse
    string:find(string:lowercase(binary_to_list(Type)), Query) =/= nomatch orelse
    lists:any(fun(O) ->
        string:find(string:lowercase(binary_to_list(O)), Query) =/= nomatch
    end, Obs).

relation_matches_query(#relation{from = From, to = To, relation_type = Type}, Query) ->
    string:find(string:lowercase(binary_to_list(From)), Query) =/= nomatch orelse
    string:find(string:lowercase(binary_to_list(To)), Query) =/= nomatch orelse
    string:find(string:lowercase(binary_to_list(Type)), Query) =/= nomatch.

knowledge_graph_to_json(#knowledge_graph{entities = Entities, relations = Relations}) ->
    #{
        <<"entities">> => [entity_to_json(E) || E <- Entities],
        <<"relations">> => [relation_to_json(R) || R <- Relations]
    }.

entity_to_json(#entity{name = Name, entity_type = Type, observations = Obs}) ->
    #{
        <<"name">> => Name,
        <<"entityType">> => Type,
        <<"observations">> => Obs
    }.

relation_to_json(#relation{from = From, to = To, relation_type = Type}) ->
    #{
        <<"from">> => From,
        <<"to">> => To,
        <<"relationType">> => Type
    }.