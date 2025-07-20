-module(erlmcp).

-include("erlmcp.hrl").

%% Application management API
-export([
    start_server/1, stop_server/1, list_servers/0,
    start_transport/2, start_transport/3, stop_transport/1, list_transports/0,
    bind_transport_to_server/2, unbind_transport/1
]).

%% Server operations API
-export([
    add_resource/3, add_resource/4,
    add_tool/3, add_tool/4,
    add_prompt/3, add_prompt/4
]).

%% Configuration API
-export([
    get_server_config/1, update_server_config/2,
    get_transport_config/1, update_transport_config/2
]).

%% Legacy compatibility for stdio server
-export([
    start_stdio_server/0, start_stdio_server/1, stop_stdio_server/0
]).

%% Convenience functions
-export([
    start_stdio_setup/2, start_tcp_setup/3, 
    setup_server_components/2, quick_stdio_server/3
]).

%% Types
-type server_id() :: atom().
-type transport_id() :: atom().
-type transport_type() :: stdio | tcp | http.

-export_type([server_id/0, transport_id/0, transport_type/0]).

%%====================================================================
%% Application Management API
%%====================================================================

-spec start_server(server_id()) -> {ok, pid()} | {error, term()}.
start_server(ServerId) ->
    start_server_with_config(ServerId, #{}).

-spec start_server_with_config(server_id(), map()) -> {ok, pid()} | {error, term()}.
start_server_with_config(ServerId, Config) ->
    % For now, use a simple approach until full supervision is implemented
    % Ensure default capabilities if not provided
    DefaultCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    DefaultConfig = #{capabilities => DefaultCaps},
    FinalConfig = maps:merge(DefaultConfig, Config),
    
    % Try the new supervisor first, fall back to creating directly
    case whereis(erlmcp_server_sup) of
        undefined ->
            % Supervisor not available, create server directly for now
            case erlmcp_server_new:start_link(ServerId, FinalConfig) of
                {ok, ServerPid} ->
                    % Register with registry if available
                    case whereis(erlmcp_registry) of
                        undefined -> {ok, ServerPid};
                        _ -> 
                            case erlmcp_registry:register_server(ServerId, ServerPid, FinalConfig) of
                                ok -> {ok, ServerPid};
                                Error -> Error
                            end
                    end;
                Error -> Error
            end;
        _Pid ->
            % Use supervisor
            case erlmcp_server_sup:start_child(ServerId, FinalConfig) of
                {ok, ServerPid} ->
                    case erlmcp_registry:register_server(ServerId, ServerPid, FinalConfig) of
                        ok -> {ok, ServerPid};
                        Error -> Error
                    end;
                Error -> Error
            end
    end.

-spec stop_server(server_id()) -> ok | {error, term()}.
stop_server(ServerId) ->
    % Try registry first, fall back to direct approach
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_server(ServerId) of
                {ok, {ServerPid, _Config}} ->
                    erlmcp_registry:unregister_server(ServerId),
                    case is_process_alive(ServerPid) of
                        true -> 
                            erlmcp_server_new:stop(ServerPid),
                            ok;
                        false -> ok
                    end;
                {error, not_found} ->
                    ok
            end
    end.

-spec list_servers() -> [{server_id(), {pid(), map()}}].
list_servers() ->
    erlmcp_registry:list_servers().

-spec start_transport(transport_id(), transport_type()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type) ->
    start_transport(TransportId, Type, #{}).

-spec start_transport(transport_id(), transport_type(), map()) -> {ok, pid()} | {error, term()}.
start_transport(TransportId, Type, Config) ->
    % For now, create transport directly until full supervision is implemented
    case Type of
        stdio ->
            case erlmcp_transport_stdio_new:start_link(TransportId, Config) of
                {ok, TransportPid} ->
                    TransportConfig = Config#{type => Type},
                    case whereis(erlmcp_registry) of
                        undefined -> {ok, TransportPid};
                        _ -> 
                            case erlmcp_registry:register_transport(TransportId, TransportPid, TransportConfig) of
                                ok -> {ok, TransportPid};
                                Error -> Error
                            end
                    end;
                Error -> Error
            end;
        tcp ->
            {error, {transport_not_implemented, tcp}};
        http ->
            {error, {transport_not_implemented, http}};
        _ ->
            {error, {unknown_transport_type, Type}}
    end.

-spec stop_transport(transport_id()) -> ok | {error, term()}.
stop_transport(TransportId) ->
    case whereis(erlmcp_registry) of
        undefined ->
            {error, registry_not_available};
        _ ->
            case erlmcp_registry:find_transport(TransportId) of
                {ok, {TransportPid, _Config}} ->
                    erlmcp_registry:unregister_transport(TransportId),
                    case is_process_alive(TransportPid) of
                        true -> 
                            erlmcp_transport_stdio_new:close(TransportPid),
                            ok;
                        false -> ok
                    end;
                {error, not_found} ->
                    ok
            end
    end.

-spec list_transports() -> [{transport_id(), {pid(), map()}}].
list_transports() ->
    erlmcp_registry:list_transports().

-spec bind_transport_to_server(transport_id(), server_id()) -> ok | {error, term()}.
bind_transport_to_server(TransportId, ServerId) ->
    erlmcp_registry:bind_transport_to_server(TransportId, ServerId).

-spec unbind_transport(transport_id()) -> ok.
unbind_transport(TransportId) ->
    erlmcp_registry:unbind_transport(TransportId).

%%====================================================================
%% Server Operations API
%%====================================================================

-spec add_resource(server_id(), binary(), fun()) -> ok | {error, term()}.
add_resource(ServerId, Uri, Handler) ->
    add_resource(ServerId, Uri, Handler, #{}).

-spec add_resource(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_resource(ServerId, Uri, Handler, Options) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            case maps:get(template, Options, false) of
                true ->
                    Name = maps:get(name, Options, Uri),
                    erlmcp_server_new:add_resource_template(ServerPid, Uri, Name, Handler);
                false ->
                    erlmcp_server_new:add_resource(ServerPid, Uri, Handler)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec add_tool(server_id(), binary(), fun()) -> ok | {error, term()}.
add_tool(ServerId, Name, Handler) ->
    add_tool(ServerId, Name, Handler, #{}).

-spec add_tool(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_tool(ServerId, Name, Handler, Options) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            case maps:get(schema, Options, undefined) of
                undefined ->
                    erlmcp_server_new:add_tool(ServerPid, Name, Handler);
                Schema ->
                    erlmcp_server_new:add_tool_with_schema(ServerPid, Name, Handler, Schema)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec add_prompt(server_id(), binary(), fun()) -> ok | {error, term()}.
add_prompt(ServerId, Name, Handler) ->
    add_prompt(ServerId, Name, Handler, #{}).

-spec add_prompt(server_id(), binary(), fun(), map()) -> ok | {error, term()}.
add_prompt(ServerId, Name, Handler, Options) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, _Config}} ->
            case maps:get(arguments, Options, undefined) of
                undefined ->
                    erlmcp_server_new:add_prompt(ServerPid, Name, Handler);
                Arguments ->
                    erlmcp_server_new:add_prompt_with_args(ServerPid, Name, Handler, Arguments)
            end;
        {error, not_found} ->
            {error, server_not_found}
    end.

%%====================================================================
%% Configuration API
%%====================================================================

-spec get_server_config(server_id()) -> {ok, map()} | {error, term()}.
get_server_config(ServerId) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {_ServerPid, Config}} ->
            {ok, Config};
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec update_server_config(server_id(), map()) -> ok | {error, term()}.
update_server_config(ServerId, NewConfig) ->
    case erlmcp_registry:find_server(ServerId) of
        {ok, {ServerPid, OldConfig}} ->
            UpdatedConfig = maps:merge(OldConfig, NewConfig),
            % Re-register with updated config
            erlmcp_registry:register_server(ServerId, ServerPid, UpdatedConfig);
        {error, not_found} ->
            {error, server_not_found}
    end.

-spec get_transport_config(transport_id()) -> {ok, map()} | {error, term()}.
get_transport_config(TransportId) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {_TransportPid, Config}} ->
            {ok, Config};
        {error, not_found} ->
            {error, transport_not_found}
    end.

-spec update_transport_config(transport_id(), map()) -> ok | {error, term()}.
update_transport_config(TransportId, NewConfig) ->
    case erlmcp_registry:find_transport(TransportId) of
        {ok, {TransportPid, OldConfig}} ->
            UpdatedConfig = maps:merge(OldConfig, NewConfig),
            % Re-register with updated config
            erlmcp_registry:register_transport(TransportId, TransportPid, UpdatedConfig);
        {error, not_found} ->
            {error, transport_not_found}
    end.

%%====================================================================
%% Legacy Compatibility API
%%====================================================================

-spec start_stdio_server() -> {ok, pid()} | {error, term()}.
start_stdio_server() ->
    start_stdio_server(#{}).

-spec start_stdio_server(map()) -> {ok, pid()} | {error, term()}.
start_stdio_server(Options) ->
    % Use the legacy approach for now, but with registry if available
    case erlmcp_stdio_server:start_link(Options) of
        {ok, ServerPid} ->
            % Register as default stdio server if registry is available
            case whereis(erlmcp_registry) of
                undefined -> {ok, ServerPid};
                _ ->
                    ServerConfig = #{
                        capabilities => #mcp_server_capabilities{
                            resources = #mcp_capability{enabled = true},
                            tools = #mcp_capability{enabled = true},
                            prompts = #mcp_capability{enabled = true}
                        },
                        options => Options
                    },
                    case erlmcp_registry:register_server(default_stdio_server, ServerPid, ServerConfig) of
                        ok -> {ok, ServerPid};
                        {error, already_registered} -> {ok, ServerPid};
                        Error -> Error
                    end
            end;
        Error -> Error
    end.

-spec stop_stdio_server() -> ok.
stop_stdio_server() ->
    % Try registry approach first
    case whereis(erlmcp_registry) of
        undefined ->
            % Fall back to legacy approach
            case whereis(erlmcp_stdio_server) of
                undefined -> ok;
                _Pid -> 
                    erlmcp_stdio_server:stop(),
                    ok
            end;
        _ ->
            % Use registry to stop both server and transport
            _ = stop_transport(default_stdio_transport),
            _ = stop_server(default_stdio_server),
            % Also stop legacy server if it exists
            case whereis(erlmcp_stdio_server) of
                undefined -> ok;
                _Pid -> erlmcp_stdio_server:stop()
            end,
            ok
    end.

%%====================================================================
%% Convenience Functions
%%====================================================================

%% Create a complete MCP server setup with stdio transport
-spec start_stdio_setup(server_id(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_stdio_setup(ServerId, Config) ->
    case start_server_with_config(ServerId, Config) of
        {ok, ServerPid} ->
            TransportId = binary_to_atom(<<(atom_to_binary(ServerId))/binary, "_stdio">>, utf8),
            TransportConfig = #{server_id => ServerId},
            case start_transport(TransportId, stdio, TransportConfig) of
                {ok, TransportPid} ->
                    {ok, #{server => ServerPid, transport => TransportPid}};
                {error, TransportError} ->
                    _ = stop_server(ServerId),
                    {error, TransportError}
            end;
        {error, _} = ServerError ->
            ServerError
    end.

%% Create a complete MCP server setup with TCP transport
-spec start_tcp_setup(server_id(), map(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
start_tcp_setup(ServerId, ServerConfig, TcpConfig) ->
    case start_server_with_config(ServerId, ServerConfig) of
        {ok, ServerPid} ->
            TransportId = binary_to_atom(<<(atom_to_binary(ServerId))/binary, "_tcp">>, utf8),
            TransportConfig = maps:merge(#{server_id => ServerId}, TcpConfig),
            case start_transport(TransportId, tcp, TransportConfig) of
                {ok, TransportPid} ->
                    {ok, #{server => ServerPid, transport => TransportPid}};
                {error, TransportError} ->
                    _ = stop_server(ServerId),
                    {error, TransportError}
            end;
        {error, _} = ServerError ->
            ServerError
    end.

%% Batch add multiple resources, tools, and prompts
-spec setup_server_components(server_id(), map()) -> ok | {error, term()}.
setup_server_components(ServerId, Components) ->
    Resources = maps:get(resources, Components, []),
    Tools = maps:get(tools, Components, []),
    Prompts = maps:get(prompts, Components, []),
    
    try
        % Add resources
        lists:foreach(fun
            ({Uri, Handler}) ->
                ok = add_resource(ServerId, Uri, Handler);
            ({Uri, Handler, Options}) ->
                ok = add_resource(ServerId, Uri, Handler, Options)
        end, Resources),
        
        % Add tools
        lists:foreach(fun
            ({Name, Handler}) ->
                ok = add_tool(ServerId, Name, Handler);
            ({Name, Handler, Options}) ->
                ok = add_tool(ServerId, Name, Handler, Options)
        end, Tools),
        
        % Add prompts
        lists:foreach(fun
            ({Name, Handler}) ->
                ok = add_prompt(ServerId, Name, Handler);
            ({Name, Handler, Options}) ->
                ok = add_prompt(ServerId, Name, Handler, Options)
        end, Prompts),
        
        ok
    catch
        error:{badmatch, {error, Reason}} ->
            {error, Reason};
        Class:Exception ->
            {error, {Class, Exception}}
    end.

%% Quick way to create a complete stdio MCP server with components
-spec quick_stdio_server(server_id(), map(), map()) -> {ok, #{server => pid(), transport => pid()}} | {error, term()}.
quick_stdio_server(ServerId, ServerConfig, Components) ->
    case start_stdio_setup(ServerId, ServerConfig) of
        {ok, Result} ->
            case setup_server_components(ServerId, Components) of
                ok ->
                    {ok, Result};
                {error, SetupError} ->
                    _ = stop_server(ServerId),
                    _ = stop_transport(binary_to_atom(<<(atom_to_binary(ServerId))/binary, "_stdio">>, utf8)),
                    {error, SetupError}
            end;
        {error, _} = Error ->
            Error
    end.