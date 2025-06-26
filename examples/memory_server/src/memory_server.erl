-module(memory_server).
-behaviour(gen_server).

-include("../include/memory_server.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    
    MemoryFilePath = case application:get_env(memory_server, memory_file_path) of
        {ok, Path} -> list_to_binary(Path);
        undefined -> 
            case os:getenv("MEMORY_FILE_PATH") of
                false -> <<"memory.json">>;
                EnvPath -> list_to_binary(EnvPath)
            end
    end,
    
    KnowledgeGraph = load_graph(MemoryFilePath),
    
    stdio_handler:start(),
    
    State = #state{
        knowledge_graph = KnowledgeGraph,
        memory_file_path = MemoryFilePath,
        mcp_server = self()
    },
    
    {ok, State}.

handle_call({create_entities, Entities}, _From, State) ->
    {Result, NewState} = create_entities_impl(Entities, State),
    {reply, Result, NewState};

handle_call({create_relations, Relations}, _From, State) ->
    {Result, NewState} = create_relations_impl(Relations, State),
    {reply, Result, NewState};

handle_call({add_observations, EntityName, Observations}, _From, State) ->
    {Result, NewState} = add_observations_impl(EntityName, Observations, State),
    {reply, Result, NewState};

handle_call({delete_entities, EntityNames}, _From, State) ->
    {Result, NewState} = delete_entities_impl(EntityNames, State),
    {reply, Result, NewState};

handle_call({delete_observations, EntityName, Observations}, _From, State) ->
    {Result, NewState} = delete_observations_impl(EntityName, Observations, State),
    {reply, Result, NewState};

handle_call({delete_relations, Relations}, _From, State) ->
    {Result, NewState} = delete_relations_impl(Relations, State),
    {reply, Result, NewState};

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

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%====================================================================
%% File I/O Functions
%%====================================================================

load_graph(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Data} ->
            Lines = binary:split(Data, <<"\n">>, [global, trim_all]),
            parse_graph_lines(Lines);
        {error, enoent} ->
            #knowledge_graph{entities = [], relations = []};
        {error, Reason} ->
            logger:error("Failed to load graph from ~s: ~p", [FilePath, Reason]),
            #knowledge_graph{entities = [], relations = []}
    end.

parse_graph_lines(Lines) ->
    parse_graph_lines(Lines, [], []).

parse_graph_lines([], Entities, Relations) ->
    #knowledge_graph{entities = lists:reverse(Entities), relations = lists:reverse(Relations)};
parse_graph_lines([Line | Rest], Entities, Relations) ->
    case Line of
        <<>> ->
            parse_graph_lines(Rest, Entities, Relations);
        _ ->
            try jsx:decode(Line, [return_maps]) of
                #{<<"type">> := <<"entity">>, <<"name">> := Name, <<"entityType">> := Type} = EntityMap ->
                    Observations = maps:get(<<"observations">>, EntityMap, []),
                    Entity = #entity{name = Name, entity_type = Type, observations = Observations},
                    parse_graph_lines(Rest, [Entity | Entities], Relations);
                #{<<"type">> := <<"relation">>, <<"from">> := From, <<"to">> := To, <<"relationType">> := RelType} ->
                    Relation = #relation{from = From, to = To, relation_type = RelType},
                    parse_graph_lines(Rest, Entities, [Relation | Relations]);
                _ ->
                    parse_graph_lines(Rest, Entities, Relations)
            catch
                _:_ ->
                    parse_graph_lines(Rest, Entities, Relations)
            end
    end.

save_graph(KnowledgeGraph, FilePath) ->
    Lines = format_graph_lines(KnowledgeGraph),
    Data = case Lines of
        [] -> <<>>;
        _ -> iolist_to_binary([string:join([binary_to_list(Line) || Line <- Lines], "\n"), "\n"])
    end,
    file:write_file(FilePath, Data).

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
        
        NewEntities = [E || E <- Entities, not lists:member(E#entity.name, ExistingNames)],
        
        UpdatedEntities = KnowledgeGraph#knowledge_graph.entities ++ NewEntities,
        UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
        
        save_graph(UpdatedGraph, State#state.memory_file_path),
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, length(NewEntities)}, NewState}
    catch
        _:Error ->
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
        
        NewRelations = [R || R <- Relations, not relation_exists(R, ExistingRelations)],
        
        UpdatedRelations = ExistingRelations ++ NewRelations,
        UpdatedGraph = KnowledgeGraph#knowledge_graph{relations = UpdatedRelations},
        
        save_graph(UpdatedGraph, State#state.memory_file_path),
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, length(NewRelations)}, NewState}
    catch
        _:Error ->
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
                
                UpdatedEntities = lists:keyreplace(EntityName, #entity.name, Entities, UpdatedEntity),
                UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
                
                save_graph(UpdatedGraph, State#state.memory_file_path),
                
                NewState = State#state{knowledge_graph = UpdatedGraph},
                {{ok, length(NewObs)}, NewState}
        end
    catch
        _:Error ->
            {{error, Error}, State}
    end.

delete_entities_impl(EntityNames, State) ->
    try
        KnowledgeGraph = State#state.knowledge_graph,
        Entities = KnowledgeGraph#knowledge_graph.entities,
        Relations = KnowledgeGraph#knowledge_graph.relations,
        
        {RemainingEntities, DeletedCount} = lists:foldl(fun(Name, {Acc, Count}) ->
            case lists:keymember(Name, #entity.name, Acc) of
                true ->
                    {lists:keydelete(Name, #entity.name, Acc), Count + 1};
                false ->
                    {Acc, Count}
            end
        end, {Entities, 0}, EntityNames),
        
        RemainingRelations = [R || R <- Relations, 
            not lists:member(R#relation.from, EntityNames) andalso 
            not lists:member(R#relation.to, EntityNames)],
        
        UpdatedGraph = KnowledgeGraph#knowledge_graph{
            entities = RemainingEntities,
            relations = RemainingRelations
        },
        
        save_graph(UpdatedGraph, State#state.memory_file_path),
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, DeletedCount}, NewState}
    catch
        _:Error ->
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
                UpdatedEntities = lists:keyreplace(EntityName, #entity.name, Entities, UpdatedEntity),
                UpdatedGraph = KnowledgeGraph#knowledge_graph{entities = UpdatedEntities},
                
                save_graph(UpdatedGraph, State#state.memory_file_path),
                
                NewState = State#state{knowledge_graph = UpdatedGraph},
                {{ok, DeletedCount}, NewState}
        end
    catch
        _:Error ->
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
        
        save_graph(UpdatedGraph, State#state.memory_file_path),
        
        NewState = State#state{knowledge_graph = UpdatedGraph},
        {{ok, DeletedCount}, NewState}
    catch
        _:Error ->
            {{error, Error}, State}
    end.

read_graph_impl(State) ->
    try
        GraphJson = knowledge_graph_to_json(State#state.knowledge_graph),
        {ok, GraphJson}
    catch
        _:Error ->
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
        _:Error ->
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
        _:Error ->
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
