%%%-------------------------------------------------------------------
%%% @doc
%%% Component Health Check Implementations
%%%
%%% Provides real health check implementations for core erlmcp components.
%%% Replaces stub health_check() functions with actual status reporting.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_component_health).

-export([
    client_health/1,
    server_health/1,
    registry_health/0,
    session_manager_health/0
]).

-include_lib("kernel/include/logger.hrl").

-type health_status() :: healthy | degraded | unhealthy | unknown.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Health check for erlmcp_client process
-spec client_health(pid()) -> {health_status(), map()}.
client_health(ClientPid) when is_pid(ClientPid) ->
    try
        % Check if process is alive
        case erlang:is_process_alive(ClientPid) of
            false ->
                {unhealthy, #{reason => process_dead}};
            true ->
                % Get process info
                case erlang:process_info(ClientPid, [message_queue_len, memory, reductions]) of
                    undefined ->
                        {unhealthy, #{reason => process_info_failed}};
                    ProcessInfo ->
                        QueueLen = proplists:get_value(message_queue_len, ProcessInfo, 0),
                        Memory = proplists:get_value(memory, ProcessInfo, 0),
                        Reductions = proplists:get_value(reductions, ProcessInfo, 0),
                        
                        % Try to get client state
                        Status = try
                            case gen_server:call(ClientPid, get_pid, 1000) of
                                Pid when is_pid(Pid) ->
                                    % Get pending request count
                                    case gen_server:call(ClientPid, {get_state_info}, 1000) of
                                        {ok, StateInfo} ->
                                            PendingCount = maps:get(pending_count, StateInfo, 0),
                                            
                                            % Determine health based on metrics
                                            if
                                                QueueLen > 1000 -> degraded;
                                                PendingCount > 100 -> degraded;
                                                Memory > 100000000 -> degraded; % 100MB
                                                true -> healthy
                                            end;
                                        _ ->
                                            healthy
                                    end;
                                _ ->
                                    degraded
                            end
                        catch
                            _:_ -> degraded
                        end,
                        
                        Metrics = #{
                            queue_length => QueueLen,
                            memory_bytes => Memory,
                            reductions => Reductions,
                            status => Status
                        },
                        
                        {Status, Metrics}
                end
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Client health check failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {unhealthy, #{reason => {exception, Class, Reason}}}
    end;
client_health(_) ->
    {unhealthy, #{reason => invalid_pid}}.

%% @doc Health check for erlmcp_server process
-spec server_health(pid()) -> {health_status(), map()}.
server_health(ServerPid) when is_pid(ServerPid) ->
    try
        case erlang:is_process_alive(ServerPid) of
            false ->
                {unhealthy, #{reason => process_dead}};
            true ->
                case erlang:process_info(ServerPid, [message_queue_len, memory]) of
                    undefined ->
                        {unhealthy, #{reason => process_info_failed}};
                    ProcessInfo ->
                        QueueLen = proplists:get_value(message_queue_len, ProcessInfo, 0),
                        Memory = proplists:get_value(memory, ProcessInfo, 0),
                        
                        % Try to get server state
                        Status = try
                            case gen_server:call(ServerPid, get_pid, 1000) of
                                Pid when is_pid(Pid) ->
                                    % Try to get tool/resource/prompt counts
                                    case gen_server:call(ServerPid, {get_state_info}, 1000) of
                                        {ok, StateInfo} ->
                                            ToolCount = maps:get(tool_count, StateInfo, 0),
                                            ResourceCount = maps:get(resource_count, StateInfo, 0),
                                            
                                            if
                                                QueueLen > 1000 -> degraded;
                                                Memory > 100000000 -> degraded;
                                                true -> healthy
                                            end;
                                        _ ->
                                            healthy
                                    end;
                                _ ->
                                    degraded
                            end
                        catch
                            _:_ -> degraded
                        end,
                        
                        Metrics = #{
                            queue_length => QueueLen,
                            memory_bytes => Memory,
                            status => Status
                        },
                        
                        {Status, Metrics}
                end
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Server health check failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {unhealthy, #{reason => {exception, Class, Reason}}}
    end;
server_health(_) ->
    {unhealthy, #{reason => invalid_pid}}.

%% @doc Health check for erlmcp_registry (singleton process)
-spec registry_health() -> {health_status(), map()}.
registry_health() ->
    try
        case whereis(erlmcp_registry) of
            undefined ->
                {unhealthy, #{reason => not_registered}};
            Pid when is_pid(Pid) ->
                case erlang:is_process_alive(Pid) of
                    false ->
                        {unhealthy, #{reason => process_dead}};
                    true ->
                        case erlang:process_info(Pid, [message_queue_len, memory]) of
                            undefined ->
                                {unhealthy, #{reason => process_info_failed}};
                            ProcessInfo ->
                                QueueLen = proplists:get_value(message_queue_len, ProcessInfo, 0),
                                Memory = proplists:get_value(memory, ProcessInfo, 0),
                                
                                % Test registry responsiveness
                                Status = try
                                    case gen_server:call(Pid, get_pid, 1000) of
                                        P when is_pid(P) ->
                                            % Get server and transport counts
                                            Servers = gen_server:call(Pid, list_servers, 1000),
                                            Transports = gen_server:call(Pid, list_transports, 1000),
                                            
                                            ServerCount = length(Servers),
                                            TransportCount = length(Transports),
                                            
                                            % Check gproc health
                                            GprocAlive = erlang:whereis(gproc) =/= undefined,
                                            
                                            if
                                                not GprocAlive -> unhealthy;
                                                QueueLen > 1000 -> degraded;
                                                Memory > 50000000 -> degraded;
                                                true -> healthy
                                            end;
                                        _ ->
                                            degraded
                                    end
                                catch
                                    _:_ -> degraded
                                end,
                                
                                Metrics = #{
                                    queue_length => QueueLen,
                                    memory_bytes => Memory,
                                    gproc_alive => erlang:whereis(gproc) =/= undefined,
                                    status => Status
                                },
                                
                                {Status, Metrics}
                        end
                end
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Registry health check failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {unhealthy, #{reason => {exception, Class, Reason}}}
    end.

%% @doc Health check for erlmcp_session_manager (singleton process)
-spec session_manager_health() -> {health_status(), map()}.
session_manager_health() ->
    try
        case whereis(erlmcp_session_manager) of
            undefined ->
                {unhealthy, #{reason => not_registered}};
            Pid when is_pid(Pid) ->
                case erlang:is_process_alive(Pid) of
                    false ->
                        {unhealthy, #{reason => process_dead}};
                    true ->
                        case erlang:process_info(Pid, [message_queue_len, memory]) of
                            undefined ->
                                {unhealthy, #{reason => process_info_failed}};
                            ProcessInfo ->
                                QueueLen = proplists:get_value(message_queue_len, ProcessInfo, 0),
                                Memory = proplists:get_value(memory, ProcessInfo, 0),
                                
                                % Test session manager responsiveness
                                Status = try
                                    case gen_server:call(Pid, list_sessions, 1000) of
                                        Sessions when is_list(Sessions) ->
                                            SessionCount = length(Sessions),
                                            
                                            % Check ETS table health
                                            ETSHealth = case ets:info(erlmcp_sessions) of
                                                undefined -> false;
                                                _ -> true
                                            end,
                                            
                                            if
                                                not ETSHealth -> unhealthy;
                                                QueueLen > 1000 -> degraded;
                                                SessionCount > 100000 -> degraded;
                                                Memory > 500000000 -> degraded; % 500MB
                                                true -> healthy
                                            end;
                                        _ ->
                                            degraded
                                    end
                                catch
                                    _:_ -> degraded
                                end,
                                
                                Metrics = #{
                                    queue_length => QueueLen,
                                    memory_bytes => Memory,
                                    ets_table_exists => ets:info(erlmcp_sessions) =/= undefined,
                                    status => Status
                                },
                                
                                {Status, Metrics}
                        end
                end
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Session manager health check failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {unhealthy, #{reason => {exception, Class, Reason}}}
    end.
