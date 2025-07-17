-module(memory_server_mcp).
-behaviour(gen_server).

-include_lib("erlmcp/include/erlmcp.hrl").
-include("memory_server.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State
-record(mcp_state, {
    mcp_server :: pid(),
    transport_config :: term()
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link(TransportConfig) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [TransportConfig], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([TransportConfig]) ->
    process_flag(trap_exit, true),
    
    %% Initialize MCP server capabilities
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_capability{name = <<"tools">>, enabled = true}
    },
    
    %% Start the MCP server
    {ok, McpServer} = erlmcp_server:start_link(TransportConfig, Capabilities),
    
    %% Setup all tools
    ok = setup_tools(McpServer),
    
    State = #mcp_state{
        mcp_server = McpServer,
        transport_config = TransportConfig
    },
    
    logger:info("Memory Server MCP interface started with ~p transport", 
                [element(1, TransportConfig)]),
    
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, #mcp_state{mcp_server = Pid} = State) ->
    logger:error("MCP server process died: ~p", [Reason]),
    {stop, {mcp_server_died, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Tool Setup
%%====================================================================

setup_tools(McpServer) ->
    ToolDefinitions = [
        {<<"create_entities">>, fun handle_create_entities/1, create_entities_schema()},
        {<<"create_relations">>, fun handle_create_relations/1, create_relations_schema()},
        {<<"add_observations">>, fun handle_add_observations/1, add_observations_schema()},
        {<<"delete_entities">>, fun handle_delete_entities/1, delete_entities_schema()},
        {<<"delete_observations">>, fun handle_delete_observations/1, delete_observations_schema()},
        {<<"delete_relations">>, fun handle_delete_relations/1, delete_relations_schema()},
        {<<"read_graph">>, fun handle_read_graph/1, read_graph_schema()},
        {<<"search_nodes">>, fun handle_search_nodes/1, search_nodes_schema()},
        {<<"open_nodes">>, fun handle_open_nodes/1, open_nodes_schema()}
    ],
    
    lists:foreach(fun({Name, Handler, Schema}) ->
        erlmcp_server:add_tool_with_schema(McpServer, Name, Handler, Schema)
    end, ToolDefinitions),
    
    ok.

%%====================================================================
%% Tool Handlers
%%====================================================================

handle_create_entities(#{<<"entities">> := EntitiesJson}) ->
    case memory_server:create_entities(EntitiesJson) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Created ~p entities", [Count])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_create_relations(#{<<"relations">> := RelationsJson}) ->
    case memory_server:create_relations(RelationsJson) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Created ~p relations", [Count])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_add_observations(#{<<"entityName">> := EntityName, <<"observations">> := Observations}) ->
    case memory_server:add_observations(EntityName, Observations) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Added ~p observations to ~s", 
                                                      [Count, EntityName])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_delete_entities(#{<<"entityNames">> := EntityNames}) ->
    case memory_server:delete_entities(EntityNames) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Deleted ~p entities", [Count])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_delete_observations(#{<<"entityName">> := EntityName, <<"observations">> := Observations}) ->
    case memory_server:delete_observations(EntityName, Observations) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Deleted ~p observations from ~s", 
                                                      [Count, EntityName])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_delete_relations(#{<<"relations">> := RelationsJson}) ->
    case memory_server:delete_relations(RelationsJson) of
        {ok, Count} ->
            [#mcp_content{
                type = <<"text">>,
                text = iolist_to_binary(io_lib:format("Deleted ~p relations", [Count])),
                mime_type = <<"text/plain">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_read_graph(_Arguments) ->
    case memory_server:read_graph() of
        {ok, GraphJson} ->
            [#mcp_content{
                type = <<"text">>,
                text = jsx:encode(GraphJson),
                mime_type = <<"application/json">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_search_nodes(#{<<"query">> := Query}) ->
    case memory_server:search_nodes(Query) of
        {ok, Results} ->
            [#mcp_content{
                type = <<"text">>,
                text = jsx:encode(Results),
                mime_type = <<"application/json">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

handle_open_nodes(#{<<"entityNames">> := EntityNames}) ->
    case memory_server:open_nodes(EntityNames) of
        {ok, Results} ->
            [#mcp_content{
                type = <<"text">>,
                text = jsx:encode(Results),
                mime_type = <<"application/json">>
            }];
        {error, Reason} ->
            throw({error, format_error(Reason)})
    end.

%%====================================================================
%% Tool Schemas
%%====================================================================

create_entities_schema() ->
    #{
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
    }.

create_relations_schema() ->
    #{
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
    }.

add_observations_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"entityName">> => #{<<"type">> => <<"string">>},
            <<"observations">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>}
            }
        },
        <<"required">> => [<<"entityName">>, <<"observations">>]
    }.

delete_entities_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"entityNames">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>}
            }
        },
        <<"required">> => [<<"entityNames">>]
    }.

delete_observations_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"entityName">> => #{<<"type">> => <<"string">>},
            <<"observations">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>}
            }
        },
        <<"required">> => [<<"entityName">>, <<"observations">>]
    }.

delete_relations_schema() ->
    #{
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
    }.

read_graph_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{},
        <<"additionalProperties">> => false
    }.

search_nodes_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"query">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"query">>]
    }.

open_nodes_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"entityNames">> => #{
                <<"type">> => <<"array">>,
                <<"items">> => #{<<"type">> => <<"string">>}
            }
        },
        <<"required">> => [<<"entityNames">>]
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).