%%% @doc Registry-based MCP Advisor Provider
%%%
%%% Discovers and searches MCP servers registered in the local erlmcp registry.
%%% This provider enables discovery of locally running MCP servers.
%%%
%%% @end
-module(erlmcp_advisor_provider_registry).

-behaviour(erlmcp_advisor_provider).

-include("erlmcp.hrl").

%% Behavior callbacks
-export([
    search/2,
    info/0
]).

%% Optional callbacks
-export([
    init/1,
    health/0,
    cleanup/0
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type search_options() :: erlmcp_advisor_provider:search_options().
-type search_result() :: erlmcp_advisor_provider:search_result().

%%====================================================================
%% Behavior Callbacks
%%====================================================================

%% @doc Search for MCP servers in the registry
-spec search(binary(), search_options()) -> {ok, [search_result()]} | {error, term()}.
search(Query, Options) when is_binary(Query), is_map(Options) ->
    try
        Limit = maps:get(limit, Options, 10),
        MinSimilarity = maps:get(min_similarity, Options, 0.3),

        %% Get all registered servers
        Servers = get_registered_servers(),

        %% Score and filter servers
        Scored = score_servers(Query, Servers),
        Filtered = [S || S <- Scored, maps:get(similarity, S, 0.0) >= MinSimilarity],

        %% Sort by similarity descending
        Sorted = lists:sort(fun(A, B) ->
            maps:get(similarity, A, 0.0) >= maps:get(similarity, B, 0.0)
        end, Filtered),

        %% Apply limit
        Results = lists:sublist(Sorted, Limit),

        {ok, Results}
    catch
        Type:Error:Stack ->
            logger:error("Registry search failed", #{
                type => Type,
                error => Error,
                stack => Stack
            }),
            {error, {search_failed, Error}}
    end.

%% @doc Provider information
-spec info() -> erlmcp_advisor_provider:provider_info().
info() ->
    #{
        name => <<"Registry Provider">>,
        version => <<"1.0.0">>,
        description => <<"Discovers MCP servers from the local erlmcp registry">>,
        capabilities => [local_discovery, real_time]
    }.

%% @doc Initialize the provider
-spec init(map()) -> ok | {error, term()}.
init(_Config) ->
    %% Ensure registry is available
    case whereis(erlmcp_registry) of
        undefined -> {error, registry_not_started};
        Pid when is_pid(Pid) -> ok
    end.

%% @doc Health check
-spec health() -> ok | {error, term()}.
health() ->
    case whereis(erlmcp_registry) of
        undefined -> {error, registry_down};
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true -> ok;
                false -> {error, registry_dead}
            end
    end.

%% @doc Cleanup
-spec cleanup() -> ok.
cleanup() ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get all registered MCP servers
-spec get_registered_servers() -> [map()].
get_registered_servers() ->
    try
        LocalServers = erlmcp_registry:list_servers(local),
        lists:map(fun({ServerId, {_Pid, Config}}) ->
            server_to_searchable(ServerId, Config)
        end, LocalServers)
    catch
        _:_ ->
            []
    end.

%% @private Convert server info to searchable format
-spec server_to_searchable(atom(), map()) -> map().
server_to_searchable(ServerId, Config) ->
    Capabilities = maps:get(capabilities, Config, #mcp_server_capabilities{}),

    %% Build description from capabilities
    Description = build_description(ServerId, Capabilities, Config),

    #{
        id => atom_to_binary(ServerId, utf8),
        title => atom_to_binary(ServerId, utf8),
        description => Description,
        server_id => ServerId,
        capabilities => capabilities_to_map(Capabilities),
        config => Config,
        metadata => #{
            source => registry,
            local => true,
            pid => maps:get(pid, Config, undefined)
        }
    }.

%% @private Build description from server capabilities
-spec build_description(atom(), #mcp_server_capabilities{}, map()) -> binary().
build_description(ServerId, Capabilities, Config) ->
    Parts = [
        <<"MCP server: ">>, atom_to_binary(ServerId, utf8)
    ],

    %% Add capability descriptions
    CapParts = capability_descriptions(Capabilities),

    %% Add any custom description from config
    CustomDesc = maps:get(description, Config, <<>>),

    iolist_to_binary([Parts, <<". ">>, CapParts, <<" ">>, CustomDesc]).

%% @private Generate capability descriptions
-spec capability_descriptions(#mcp_server_capabilities{}) -> iolist().
capability_descriptions(#mcp_server_capabilities{} = Caps) ->
    Parts = [],

    Parts2 = case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{subscribe = true} ->
            [<<"Provides subscribable resources.">> | Parts];
        #mcp_resources_capability{} ->
            [<<"Provides resources.">> | Parts];
        _ -> Parts
    end,

    Parts3 = case Caps#mcp_server_capabilities.tools of
        #mcp_tools_capability{listChanged = true} ->
            [<<"Offers tools with change notifications.">> | Parts2];
        #mcp_tools_capability{} ->
            [<<"Offers tools.">> | Parts2];
        _ -> Parts2
    end,

    Parts4 = case Caps#mcp_server_capabilities.prompts of
        #mcp_prompts_capability{listChanged = true} ->
            [<<"Provides prompts with change notifications.">> | Parts3];
        #mcp_prompts_capability{} ->
            [<<"Provides prompts.">> | Parts3];
        _ -> Parts3
    end,

    Parts5 = case Caps#mcp_server_capabilities.sampling of
        #mcp_sampling_capability{} ->
            [<<"Supports sampling.">> | Parts4];
        _ -> Parts4
    end,

    lists:join(<<" ">>, lists:reverse(Parts5)).

%% @private Convert capabilities record to map
-spec capabilities_to_map(#mcp_server_capabilities{}) -> map().
capabilities_to_map(#mcp_server_capabilities{} = Caps) ->
    #{
        resources => Caps#mcp_server_capabilities.resources /= undefined,
        tools => Caps#mcp_server_capabilities.tools /= undefined,
        prompts => Caps#mcp_server_capabilities.prompts /= undefined,
        logging => Caps#mcp_server_capabilities.logging /= undefined,
        sampling => Caps#mcp_server_capabilities.sampling /= undefined
    }.

%% @private Score servers against query
-spec score_servers(binary(), [map()]) -> [search_result()].
score_servers(Query, Servers) ->
    lists:map(fun(Server) ->
        score_server(Query, Server)
    end, Servers).

%% @private Score a single server against query
-spec score_server(binary(), map()) -> search_result().
score_server(Query, Server) ->
    Title = maps:get(title, Server, <<>>),
    Description = maps:get(description, Server, <<>>),

    %% Compute similarity scores
    TitleSim = erlmcp_advisor_provider:compute_text_similarity(Query, Title),
    DescSim = erlmcp_advisor_provider:compute_text_similarity(Query, Description),

    %% Weighted combination (title matches are more important)
    Similarity = (TitleSim * 0.6) + (DescSim * 0.4),

    %% Build result
    erlmcp_advisor_provider:normalize_result(Server#{
        similarity => Similarity
    }).
