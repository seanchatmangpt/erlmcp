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
    find_server/1,
    %% Introspection API (Armstrong-style)
    status/0,
    session/1,
    streams/1,
    tasks/0,
    queues/0,
    health/0,
    sessions/0,
    servers/0,
    transports/0,
    help/0
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

%%====================================================================
%% Armstrong-Style Introspection API
%% Shell convenience functions for live system interrogation
%%====================================================================

%% @doc Get overall system status
%% Returns health, session count, TPS, memory usage
-spec status() -> map().
status() ->
    erlmcp_introspect:status().

%% @doc Dump detailed session information
%% Returns session state, capabilities, in-flight requests, subscriptions
-spec session(SessionId :: binary()) -> {ok, map()} | {error, not_found}.
session(SessionId) when is_binary(SessionId) ->
    erlmcp_introspect:session_dump(SessionId).

%% @doc Get stream/subscription information for a session
%% Returns active SSE subscriptions, last event IDs, buffer depths
-spec streams(SessionId :: binary()) -> {ok, [map()]} | {error, not_found}.
streams(SessionId) when is_binary(SessionId) ->
    erlmcp_introspect:streams(SessionId).

%% @doc Get task status across the system
%% Returns tasks by status, latencies, stuck detectors
-spec tasks() -> map().
tasks() ->
    erlmcp_introspect:tasks().

%% @doc Get mailbox queue depths
%% Returns top 10 processes by queue depth, grouped by type
-spec queues() -> map().
queues() ->
    erlmcp_introspect:queues().

%% @doc Run immediate health check
%% Returns health status and detailed metrics
-spec health() -> {erlmcp_introspect:health_status(), map()}.
health() ->
    erlmcp_introspect:health_check().

%% @doc Show help for introspection commands
-spec help() -> ok.
help() ->
    io:format("~n"),
    io:format("erlmcp Shell Introspection Commands~n"),
    io:format("=====================================~n"),
    io:format("~n"),
    io:format("  erlmcp:status().      - Overall system health and metrics~n"),
    io:format("  erlmcp:session(Id).   - Detailed session dump~n"),
    io:format("  erlmcp:streams(Id).   - Stream subscriptions for session~n"),
    io:format("  erlmcp:tasks().       - Active tasks and their status~n"),
    io:format("  erlmcp:queues().      - Message queue depths (find bottlenecks)~n"),
    io:format("  erlmcp:health().      - Run health check now~n"),
    io:format("~n"),
    io:format("Additional Commands:~n"),
    io:format("~n"),
    io:format("  erlmcp:sessions().    - List all sessions~n"),
    io:format("  erlmcp:servers().     - List all MCP servers~n"),
    io:format("  erlmcp:transports().  - List all transports~n"),
    io:format("  erlmcp:help().        - Show this help~n"),
    io:format("~n"),
    io:format("Examples:~n"),
    io:format("~n"),
    io:format("  %% Check system health~n"),
    io:format("  1> erlmcp:status().~n"),
    io:format("~n"),
    io:format("  %% List all sessions~n"),
    io:format("  2> erlmcp:sessions().~n"),
    io:format("~n"),
    io:format("  %% Inspect a specific session~n"),
    io:format("  3> erlmcp:session(<<\"session-123\">>).~n"),
    io:format("~n"),
    io:format("  %% Find message queue bottlenecks~n"),
    io:format("  4> erlmcp:queues().~n"),
    io:format("~n"),
    ok.

%% @doc List all active sessions
%% Convenience wrapper around session manager
-spec sessions() -> [map()].
sessions() ->
    case whereis(erlmcp_session_manager) of
        undefined ->
            io:format("Session manager not running~n"),
            [];
        _Pid ->
            case erlmcp_session_manager:list_sessions() of
                Sessions when is_list(Sessions) ->
                    io:format("~nActive Sessions (~p total):~n", [length(Sessions)]),
                    io:format("~s~n", [lists:duplicate(60, $=)]),
                    lists:foreach(
                        fun(Session) ->
                            SessionId = maps:get(id, Session, <<"unknown">>),
                            CreatedAt = maps:get(created_at, Session, 0),
                            LastAccessed = maps:get(last_accessed, Session, 0),
                            Now = erlang:system_time(millisecond),
                            Age = Now - CreatedAt,
                            Idle = Now - LastAccessed,
                            io:format("  ~s~n", [SessionId]),
                            io:format("    Age: ~p ms, Idle: ~p ms~n", [Age, Idle])
                        end,
                        Sessions
                    ),
                    io:format("~n"),
                    Sessions;
                Error ->
                    io:format("Error listing sessions: ~p~n", [Error]),
                    []
            end
    end.

%% @doc List all MCP servers
%% Convenience wrapper around registry
-spec servers() -> [{binary(), {pid(), map()}}].
servers() ->
    case whereis(erlmcp_registry) of
        undefined ->
            io:format("Registry not running~n"),
            [];
        _Pid ->
            Servers = erlmcp_registry:list_servers(),
            io:format("~nMCP Servers (~p total):~n", [length(Servers)]),
            io:format("~s~n", [lists:duplicate(60, $=)]),
            lists:foreach(
                fun({ServerId, {Pid, Config}}) ->
                    QueueLen = case erlang:process_info(Pid, message_queue_len) of
                        {message_queue_len, Len} -> Len;
                        undefined -> 0
                    end,
                    Caps = maps:get(capabilities, Config, #{}),
                    io:format("  ~p~n", [ServerId]),
                    io:format("    Pid: ~p, Queue: ~p~n", [Pid, QueueLen]),
                    io:format("    Capabilities: ~p~n", [Caps])
                end,
                Servers
            ),
            io:format("~n"),
            Servers
    end.

%% @doc List all transports
%% Convenience wrapper around registry
-spec transports() -> [{binary(), {pid(), map()}}].
transports() ->
    case whereis(erlmcp_registry) of
        undefined ->
            io:format("Registry not running~n"),
            [];
        _Pid ->
            Transports = erlmcp_registry:list_transports(),
            io:format("~nTransports (~p total):~n", [length(Transports)]),
            io:format("~s~n", [lists:duplicate(60, $=)]),
            lists:foreach(
                fun({TransportId, {Pid, Config}}) ->
                    Type = maps:get(type, Config, unknown),
                    QueueLen = case erlang:process_info(Pid, message_queue_len) of
                        {message_queue_len, Len} -> Len;
                        undefined -> 0
                    end,
                    io:format("  ~p~n", [TransportId]),
                    io:format("    Type: ~p, Pid: ~p, Queue: ~p~n", [Type, Pid, QueueLen])
                end,
                Transports
            ),
            io:format("~n"),
            Transports
    end.
