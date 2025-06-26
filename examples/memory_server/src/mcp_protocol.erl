-module(mcp_protocol).

-export([
    handle_initialize/1,
    handle_list_tools/0,
    handle_call_tool/2,
    handle_list_resources/0,
    handle_read_resource/1,
    send_response/2,
    send_error/3
]).

-include("../include/memory_server.hrl").

handle_initialize(_Params) ->
    Capabilities = #{
        <<"tools">> => #{},
        <<"resources">> => #{},
        <<"prompts">> => #{}
    },
    
    ServerInfo = #{
        <<"name">> => <<"memory-server">>,
        <<"version">> => <<"1.0.0">>
    },
    
    #{
        <<"protocolVersion">> => <<"2025-06-18">>,
        <<"capabilities">> => Capabilities,
        <<"serverInfo">> => ServerInfo
    }.

handle_list_tools() ->
    Tools = [
        #{
            <<"name">> => <<"create_entities">>,
            <<"description">> => <<"Create new entities in the knowledge graph">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entities">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"name">> => #{<<"type">> => <<"string">>},
                                <<"entityType">> => #{<<"type">> => <<"string">>},
                                <<"observations">> => #{
                                    <<"type">> => <<"array">>,
                                    <<"items">> => #{<<"type">> => <<"string">>}
                                }
                            },
                            <<"required">> => [<<"name">>, <<"entityType">>]
                        }
                    }
                },
                <<"required">> => [<<"entities">>]
            }
        },
        #{
            <<"name">> => <<"create_relations">>,
            <<"description">> => <<"Create new relations between entities">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"relations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"from">> => #{<<"type">> => <<"string">>},
                                <<"to">> => #{<<"type">> => <<"string">>},
                                <<"relationType">> => #{<<"type">> => <<"string">>}
                            },
                            <<"required">> => [<<"from">>, <<"to">>, <<"relationType">>]
                        }
                    }
                },
                <<"required">> => [<<"relations">>]
            }
        },
        #{
            <<"name">> => <<"add_observations">>,
            <<"description">> => <<"Add observations to existing entities">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entityName">> => #{<<"type">> => <<"string">>},
                    <<"observations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                },
                <<"required">> => [<<"entityName">>, <<"observations">>]
            }
        },
        #{
            <<"name">> => <<"delete_entities">>,
            <<"description">> => <<"Delete entities and their relations">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entityNames">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                },
                <<"required">> => [<<"entityNames">>]
            }
        },
        #{
            <<"name">> => <<"delete_observations">>,
            <<"description">> => <<"Delete specific observations from entities">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entityName">> => #{<<"type">> => <<"string">>},
                    <<"observations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                },
                <<"required">> => [<<"entityName">>, <<"observations">>]
            }
        },
        #{
            <<"name">> => <<"delete_relations">>,
            <<"description">> => <<"Delete specific relations">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"relations">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{
                            <<"type">> => <<"object">>,
                            <<"properties">> => #{
                                <<"from">> => #{<<"type">> => <<"string">>},
                                <<"to">> => #{<<"type">> => <<"string">>},
                                <<"relationType">> => #{<<"type">> => <<"string">>}
                            },
                            <<"required">> => [<<"from">>, <<"to">>, <<"relationType">>]
                        }
                    }
                },
                <<"required">> => [<<"relations">>]
            }
        },
        #{
            <<"name">> => <<"read_graph">>,
            <<"description">> => <<"Read the entire knowledge graph">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"additionalProperties">> => false
            }
        },
        #{
            <<"name">> => <<"search_nodes">>,
            <<"description">> => <<"Search for entities and relations">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"query">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"query">>]
            }
        },
        #{
            <<"name">> => <<"open_nodes">>,
            <<"description">> => <<"Get specific entities and their relations">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"entityNames">> => #{
                        <<"type">> => <<"array">>,
                        <<"items">> => #{<<"type">> => <<"string">>}
                    }
                },
                <<"required">> => [<<"entityNames">>]
            }
        }
    ],
    #{<<"tools">> => Tools}.

handle_call_tool(ToolName, Arguments) ->
    case ToolName of
        <<"create_entities">> ->
            create_entities_tool(Arguments);
        <<"create_relations">> ->
            create_relations_tool(Arguments);
        <<"add_observations">> ->
            add_observations_tool(Arguments);
        <<"delete_entities">> ->
            delete_entities_tool(Arguments);
        <<"delete_observations">> ->
            delete_observations_tool(Arguments);
        <<"delete_relations">> ->
            delete_relations_tool(Arguments);
        <<"read_graph">> ->
            read_graph_tool(Arguments);
        <<"search_nodes">> ->
            search_nodes_tool(Arguments);
        <<"open_nodes">> ->
            open_nodes_tool(Arguments);
        _ ->
            {error, <<"Unknown tool: ", ToolName/binary>>}
    end.

handle_list_resources() ->
    Resources = [
        #{
            <<"uri">> => <<"memory://graph">>,
            <<"name">> => <<"Knowledge Graph">>,
            <<"description">> => <<"Complete knowledge graph with entities and relations">>,
            <<"mimeType">> => <<"application/json">>
        }
    ],
    #{<<"resources">> => Resources}.

handle_read_resource(Uri) ->
    case Uri of
        <<"memory://graph">> ->
            get_memory_graph_handler();
        _ ->
            {error, <<"Unknown resource: ", Uri/binary>>}
    end.

send_response(Id, Result) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => Result
    },
    io:format("~s~n", [jsx:encode(Response)]).

send_error(Id, Code, Message) ->
    Error = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    },
    io:format("~s~n", [jsx:encode(Error)]).

create_entities_tool(#{<<"entities">> := EntitiesJson}) ->
    case gen_server:call(memory_server, {create_entities, EntitiesJson}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Created ~p entities", [Count]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

create_relations_tool(#{<<"relations">> := RelationsJson}) ->
    case gen_server:call(memory_server, {create_relations, RelationsJson}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Created ~p relations", [Count]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

add_observations_tool(#{<<"entityName">> := EntityName, <<"observations">> := Observations}) ->
    case gen_server:call(memory_server, {add_observations, EntityName, Observations}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Added ~p observations to ~s", [Count, EntityName]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

delete_entities_tool(#{<<"entityNames">> := EntityNames}) ->
    case gen_server:call(memory_server, {delete_entities, EntityNames}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Deleted ~p entities", [Count]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

delete_observations_tool(#{<<"entityName">> := EntityName, <<"observations">> := Observations}) ->
    case gen_server:call(memory_server, {delete_observations, EntityName, Observations}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Deleted ~p observations from ~s", [Count, EntityName]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

delete_relations_tool(#{<<"relations">> := RelationsJson}) ->
    case gen_server:call(memory_server, {delete_relations, RelationsJson}) of
        {ok, Count} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => iolist_to_binary(io_lib:format("Deleted ~p relations", [Count]))
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

read_graph_tool(_Arguments) ->
    case gen_server:call(memory_server, read_graph) of
        {ok, GraphJson} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => jsx:encode(GraphJson)
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

search_nodes_tool(#{<<"query">> := Query}) ->
    case gen_server:call(memory_server, {search_nodes, Query}) of
        {ok, Results} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => jsx:encode(Results)
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

open_nodes_tool(#{<<"entityNames">> := EntityNames}) ->
    case gen_server:call(memory_server, {open_nodes, EntityNames}) of
        {ok, Results} ->
            [#{
                <<"type">> => <<"text">>,
                <<"text">> => jsx:encode(Results)
            }];
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Error: ~p", [Reason]))}
    end.

get_memory_graph_handler() ->
    {ok, State} = gen_server:call(memory_server, get_state),
    GraphJson = knowledge_graph_to_json(State#state.knowledge_graph),
    [#{
        <<"type">> => <<"text">>,
        <<"text">> => jsx:encode(GraphJson)
    }].

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
