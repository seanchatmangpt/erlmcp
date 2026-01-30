%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Public API
%%%
%%% This module provides a simplified public API for ErlMCP.
%%% It wraps the internal modules (erlmcp_server, erlmcp_client,
%%% erlmcp_registry) to provide a clean interface for users.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp).

%% Public API
-export([
    %% Server management
    start_server/2,
    stop_server/1,
    list_servers/0,
    get_server_config/1,
    update_server_config/2,
    %% Tools management
    add_tool/3,
    remove_tool/2,
    %% Resources management
    add_resource/3,
    remove_resource/2,
    %% Prompts management
    add_prompt/3,
    remove_prompt/2,
    %% Transport management
    start_transport/3,
    stop_transport/1,
    list_transports/0,
    bind_transport_to_server/2,
    get_transport_bindings/0,
    get_transport_config/1,
    update_transport_config/2,
    validate_transport_config/2,
    %% Registry operations
    find_server/1
]).

%%--------------------------------------------------------------------
%% @doc Start a new MCP server with the given ID and configuration
%% @end
%%--------------------------------------------------------------------
-spec start_server(ServerId :: atom(), Config :: map()) ->
    {ok, pid()} | {error, term()}.
start_server(ServerId, Config) ->
    erlmcp_server:start_link(ServerId, Config).

%%--------------------------------------------------------------------
%% @doc Stop an MCP server
%% @end
%%--------------------------------------------------------------------
-spec stop_server(ServerId :: atom()) -> ok | {error, term()}.
stop_server(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:stop(Pid, normal, 5000);
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc List all registered servers
%% @end
%%--------------------------------------------------------------------
-spec list_servers() -> [{atom(), pid()}].
list_servers() ->
    case erlmcp_registry:list_servers() of
        Servers when is_list(Servers) ->
            [{Id, Pid} || {Id, Pid, _Config} <- Servers];
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @doc Get server configuration
%% @end
%%--------------------------------------------------------------------
-spec get_server_config(ServerId :: atom()) ->
    {ok, map()} | {error, term()}.
get_server_config(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {_Pid, Config}} -> {ok, Config};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Update server configuration
%% @end
%%--------------------------------------------------------------------
-spec update_server_config(ServerId :: atom(), Config :: map()) ->
    ok | {error, term()}.
update_server_config(ServerId, Config) ->
    erlmcp_registry:update_server(ServerId, Config).

%%--------------------------------------------------------------------
%% @doc Add a tool to a server
%% @end
%%--------------------------------------------------------------------
-spec add_tool(ServerId :: atom(), ToolName :: binary(),
               Handler :: function()) -> ok | {error, term()}.
add_tool(ServerId, ToolName, Handler) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {add_tool, ToolName, Handler});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Remove a tool from a server
%% @end
%%--------------------------------------------------------------------
-spec remove_tool(ServerId :: atom(), ToolName :: binary()) ->
    ok | {error, term()}.
remove_tool(ServerId, ToolName) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {remove_tool, ToolName});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Add a resource to a server
%% @end
%%--------------------------------------------------------------------
-spec add_resource(ServerId :: atom(), Uri :: binary(),
                   Handler :: function()) -> ok | {error, term()}.
add_resource(ServerId, Uri, Handler) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {add_resource, Uri, Handler});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Remove a resource from a server
%% @end
%%--------------------------------------------------------------------
-spec remove_resource(ServerId :: atom(), Uri :: binary()) ->
    ok | {error, term()}.
remove_resource(ServerId, Uri) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {remove_resource, Uri});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Add a prompt to a server
%% @end
%%--------------------------------------------------------------------
-spec add_prompt(ServerId :: atom(), PromptName :: binary(),
                 Handler :: function()) -> ok | {error, term()}.
add_prompt(ServerId, PromptName, Handler) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {add_prompt, PromptName, Handler});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Remove a prompt from a server
%% @end
%%--------------------------------------------------------------------
-spec remove_prompt(ServerId :: atom(), PromptName :: binary()) ->
    ok | {error, term()}.
remove_prompt(ServerId, PromptName) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {remove_prompt, PromptName});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Start a transport with the given type and configuration
%% @end
%%--------------------------------------------------------------------
-spec start_transport(TransportId :: atom(), Type :: atom(),
                      Config :: map()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    erlmcp_transport_sup:start_child(TransportId, Type, Config).

%%--------------------------------------------------------------------
%% @doc Stop a transport
%% @end
%%--------------------------------------------------------------------
-spec stop_transport(TransportId :: atom()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} ->
            gen_server:stop(Pid, normal, 5000);
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc List all registered transports
%% @end
%%--------------------------------------------------------------------
-spec list_transports() -> [{atom(), pid()}].
list_transports() ->
    case erlmcp_registry:list_transports() of
        Transports when is_list(Transports) ->
            [{Id, Pid} || {Id, Pid, _Type, _Config} <- Transports];
        _ -> []
    end.

%%--------------------------------------------------------------------
%% @doc Bind a transport to a server
%% @end
%%--------------------------------------------------------------------
-spec bind_transport_to_server(TransportId :: atom(), ServerId :: atom()) ->
    ok | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _Config}} ->
            gen_server:call(Pid, {bind_to_server, ServerId});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Get all transport bindings
%% @end
%%--------------------------------------------------------------------
-spec get_transport_bindings() ->
    [{atom(), atom()}] | {error, term()}.
get_transport_bindings() ->
    try
        %% For now, return empty list - this would need to be implemented in registry
        []
    catch
        _:_ -> {error, registry_unavailable}
    end.

%%--------------------------------------------------------------------
%% @doc Get transport configuration
%% @end
%%--------------------------------------------------------------------
-spec get_transport_config(TransportId :: atom()) ->
    {ok, map()} | {error, term()}.
get_transport_config(TransportId) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {_Pid, Config}} -> {ok, Config};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Update transport configuration
%% @end
%%--------------------------------------------------------------------
-spec update_transport_config(TransportId :: atom(), Config :: map()) ->
    ok | {error, term()}.
update_transport_config(TransportId, Config) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {Pid, _OldConfig}} ->
            gen_server:call(Pid, {update_config, Config});
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Validate transport configuration
%% @end
%%--------------------------------------------------------------------
-spec validate_transport_config(Type :: atom(), Config :: map()) ->
    ok | {error, term()}.
validate_transport_config(stdio, _Config) ->
    ok;
validate_transport_config(tcp, Config) ->
    PortResult = case maps:get(port, Config, undefined) of
        undefined -> {error, missing_port};
        Port when is_integer(Port), Port >= 1, Port =< 65535 -> ok;
        _ -> {error, invalid_port}
    end,
    case PortResult of
        {error, _} = Error -> Error;
        ok ->
            case maps:get(host, Config, undefined) of
                undefined -> {error, missing_host};
                _ -> ok
            end
    end;
validate_transport_config(http, Config) ->
    case maps:get(url, Config, undefined) of
        undefined -> {error, missing_url};
        Url when is_binary(Url) -> ok;
        _ -> {error, invalid_url}
    end;
validate_transport_config(Type, _Config) ->
    {error, {unknown_transport_type, Type}}.

%%--------------------------------------------------------------------
%% @doc Find a server by ID
%% @end
%%--------------------------------------------------------------------
-spec find_server(ServerId :: atom()) ->
    {ok, {pid(), map()}} | {error, not_found}.
find_server(ServerId) ->
    erlmcp_registry:find_server(ServerId).
