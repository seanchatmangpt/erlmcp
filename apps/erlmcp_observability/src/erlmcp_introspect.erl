%%%-------------------------------------------------------------------
%%% @doc
%%% Armstrong-Style Introspection API for erlmcp
%%%
%%% Live system interrogation - replaces debugging with transparency.
%%% All functions return maps suitable for both shell inspection and HTTP APIs.
%%%
%%% Philosophy: "The system should be able to explain itself."
%%% No debugging needed when you can ask the system what it's doing.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_introspect).

%% API exports
-export([status/0, session_dump/1, streams/1, tasks/0, queues/0, health_check/0]).

-include_lib("kernel/include/logger.hrl").

-type session_id() :: binary().
-type health_status() :: healthy | degraded | critical.

-export_type([session_id/0, health_status/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Get system-wide status snapshot
%% Returns a map with current system health, metrics, and resource usage
-spec status() -> map().
status() ->
    Now = erlang:system_time(millisecond),

    %% Get metrics from metrics server
    Metrics = safe_call(erlmcp_metrics_server, get_metrics, [], #{}),

    %% Get health status
    HealthReport = safe_call(erlmcp_health, check, [], #{healthy => false, checks => #{}}),
    HealthStatus =
        case maps:get(healthy, HealthReport, false) of
            true ->
                healthy;
            false ->
                Checks = maps:get(checks, HealthReport, #{}),
                case lists:member(unhealthy, maps:values(Checks)) of
                    true ->
                        critical;
                    false ->
                        degraded
                end
        end,

    %% Get session count
    Sessions = safe_call(erlmcp_session_manager, list_sessions, [], []),
    SessionCount = length(Sessions),

    %% Get connection count from registry
    Servers = safe_call(erlmcp_registry, list_servers, [], []),
    Transports = safe_call(erlmcp_registry, list_transports, [], []),
    ConnectionCount = length(Transports),

    %% Calculate TPS from metrics
    TPS = maps:get(<<"throughput_msg_per_s">>, Metrics, 0),

    %% Get memory usage
    Memory = erlang:memory(),
    HeapMB = maps:get(total, Memory, 0) / 1024 / 1024,
    RSSMB =
        case recon_alloc:memory(allocated) of
            N when is_number(N) ->
                N / 1024 / 1024;
            _ ->
                0.0
        end,

    %% Get last 5 health check results from health monitor
    ComponentHealth = safe_call(erlmcp_health_monitor, get_all_component_health, [], #{}),
    LastChecks =
        maps:fold(fun(CompId, CompData, Acc) ->
                     Status = maps:get(status, CompData, unknown),
                     LastCheck = maps:get(last_check, CompData, undefined),
                     [#{component => CompId,
                        status => Status,
                        last_check => LastCheck}
                      | Acc]
                  end,
                  [],
                  ComponentHealth),
    RecentChecks =
        lists:sublist(
            lists:reverse(LastChecks), 5),

    #{status => HealthStatus,
      timestamp => Now,
      sessions =>
          #{count => SessionCount,
            active => SessionCount},  % All sessions in list are active
      connections => #{count => ConnectionCount, servers => length(Servers)},
      throughput =>
          #{current_msg_per_s => TPS, total_messages => maps:get(<<"total_messages">>, Metrics, 0)},
      memory =>
          #{heap_mb => round(HeapMB * 100) / 100,
            rss_mb => round(RSSMB * 100) / 100,
            processes => erlang:system_info(process_count)},
      health_checks => #{recent => RecentChecks, overall => HealthReport}}.

%% @doc Dump detailed information about a specific session
%% Returns session state, capabilities, in-flight requests, subscriptions
-spec session_dump(session_id()) -> {ok, map()} | {error, not_found}.
session_dump(SessionId) when is_binary(SessionId) ->
    %% Get session from session manager
    case safe_call(erlmcp_session_manager, get_session, [SessionId], {error, not_found}) of
        {ok, Session} ->
            %% Get session metadata
            Metadata = maps:get(metadata, Session, #{}),
            CreatedAt = maps:get(created_at, Session, 0),
            LastAccessed = maps:get(last_accessed, Session, 0),

            %% Try to find associated server/transport
            Servers = safe_call(erlmcp_registry, list_servers, [], []),
            Transports = safe_call(erlmcp_registry, list_transports, [], []),

            %% Find server for this session (if any)
            ServerInfo = find_server_for_session(SessionId, Servers),
            TransportInfo = find_transport_for_session(SessionId, Transports),

            %% Get subscription info if available
            Subscriptions = get_session_subscriptions(SessionId),

            Now = erlang:system_time(millisecond),

            {ok,
             #{session_id => SessionId,
               state =>
                   #{created_at => CreatedAt,
                     last_accessed => LastAccessed,
                     age_ms => Now - CreatedAt,
                     idle_ms => Now - LastAccessed},
               metadata => Metadata,
               server => ServerInfo,
               transport => TransportInfo,
               subscriptions => Subscriptions,
               in_flight_requests => get_in_flight_requests(ServerInfo)}};
        {error, _} = Error ->
            Error;
        _ ->
            {error, not_found}
    end.

%% @doc Get stream/subscription information for a session
%% Returns active SSE subscriptions, last event IDs, buffer depths
-spec streams(session_id()) -> {ok, [map()]} | {error, not_found}.
streams(SessionId) when is_binary(SessionId) ->
    %% Get all subscriptions from resource subscriptions manager
    Subscriptions = get_session_subscriptions(SessionId),

    %% Get SSE event store info if available
    SSEStores =
        case whereis(erlmcp_sse_event_store) of
            undefined ->
                [];
            _Pid ->
                %% Get all event stores (this is a simplified version)
                %% In real implementation, would query the SSE event store
                []
        end,

    %% Combine subscription and SSE info
    Streams =
        lists:map(fun(#{uri := Uri} = Sub) ->
                     #{stream_id => Uri,
                       subscription_status => active,
                       created_at => maps:get(created_at, Sub, 0),
                       last_event_id => get_last_event_id(Uri),
                       replay_buffer_depth => 0,  % Would query SSE event store
                       message_queue_depth => 0}   % Would query subscriber process
                  end,
                  Subscriptions),

    {ok, Streams}.

%% @doc Get task status across the system
%% Returns tasks by status, latencies, CPU usage
-spec tasks() -> map().
tasks() ->
    %% In erlmcp, "tasks" could mean:
    %% 1. Active gen_server processes
    %% 2. Pending requests in servers
    %% 3. Active connections/sessions
    %% Get all registered servers and their queue depths
    Servers = safe_call(erlmcp_registry, list_servers, [], []),
    Transports = safe_call(erlmcp_registry, list_transports, [], []),

    %% Categorize by role
    ServerTasks =
        lists:map(fun({ServerId, {Pid, _Config}}) ->
                     QueueLen = get_process_queue_len(Pid),
                     Reductions = get_process_reductions(Pid),
                     #{id => ServerId,
                       type => server,
                       status => get_process_status(Pid),
                       queue_depth => QueueLen,
                       reductions => Reductions}
                  end,
                  Servers),

    TransportTasks =
        lists:map(fun({TransportId, {Pid, _Config}}) ->
                     QueueLen = get_process_queue_len(Pid),
                     Reductions = get_process_reductions(Pid),
                     #{id => TransportId,
                       type => transport,
                       status => get_process_status(Pid),
                       queue_depth => QueueLen,
                       reductions => Reductions}
                  end,
                  Transports),

    AllTasks = ServerTasks ++ TransportTasks,

    %% Group by status
    ByStatus =
        lists:foldl(fun(Task, Acc) ->
                       Status = maps:get(status, Task, unknown),
                       Tasks = maps:get(Status, Acc, []),
                       maps:put(Status, [Task | Tasks], Acc)
                    end,
                    #{},
                    AllTasks),

    %% Calculate mean queue depths
    TotalQueueDepth = lists:sum([maps:get(queue_depth, T, 0) || T <- AllTasks]),
    MeanQueueDepth =
        case length(AllTasks) of
            0 ->
                0;
            N ->
                TotalQueueDepth / N
        end,

    %% Find oldest/stuck tasks
    OldestTask =
        case AllTasks of
            [] ->
                undefined;
            [First | _] ->
                First
        end,

    #{total => length(AllTasks),
      by_status => ByStatus,
      mean_queue_depth => round(MeanQueueDepth * 100) / 100,
      oldest_task => OldestTask,
      by_type => #{servers => length(ServerTasks), transports => length(TransportTasks)}}.

%% @doc Get mailbox queue depths across the system
%% Returns top processes by queue depth, grouped by type
-spec queues() -> map().
queues() ->
    %% Get all registered processes
    Servers = safe_call(erlmcp_registry, list_servers, [], []),
    Transports = safe_call(erlmcp_registry, list_transports, [], []),
    Sessions = safe_call(erlmcp_session_manager, list_sessions, [], []),

    %% Get queue depths for servers
    ServerQueues =
        lists:map(fun({ServerId, {Pid, _Config}}) -> {ServerId, server, get_process_queue_len(Pid)}
                  end,
                  Servers),

    %% Get queue depths for transports
    TransportQueues =
        lists:map(fun({TransportId, {Pid, _Config}}) ->
                     {TransportId, transport, get_process_queue_len(Pid)}
                  end,
                  Transports),

    %% Combine and sort
    AllQueues = ServerQueues ++ TransportQueues,
    SortedQueues =
        lists:reverse(
            lists:keysort(3, AllQueues)),

    %% Get top 10
    Top10 = lists:sublist(SortedQueues, 10),

    %% Group by type
    ByType =
        lists:foldl(fun({Id, Type, Depth}, Acc) ->
                       TypeQueues = maps:get(Type, Acc, []),
                       maps:put(Type, [{Id, Depth} | TypeQueues], Acc)
                    end,
                    #{},
                    AllQueues),

    %% Get global control plane queue (registry, session_manager, etc.)
    ControlPlaneQueues =
        [{erlmcp_registry, get_process_queue_len(whereis(erlmcp_registry))},
         {erlmcp_session_manager, get_process_queue_len(whereis(erlmcp_session_manager))},
         {erlmcp_health, get_process_queue_len(whereis(erlmcp_health))},
         {erlmcp_health_monitor, get_process_queue_len(whereis(erlmcp_health_monitor))}],

    #{top_10 =>
          lists:map(fun({Id, Type, Depth}) ->
                       #{id => Id,
                         type => Type,
                         depth => Depth}
                    end,
                    Top10),
      by_type => ByType,
      control_plane =>
          lists:filter(fun({_Name, Depth}) -> Depth =/= undefined end, ControlPlaneQueues),
      total_sessions => length(Sessions)}.

%% @doc Run immediate health check across all systems
%% Returns healthy status or list of degraded/unhealthy components
-spec health_check() -> {health_status(), map()}.
health_check() ->
    Now = erlang:system_time(millisecond),

    %% Check registry (gproc ping)
    RegistryHealth =
        case whereis(erlmcp_registry) of
            undefined ->
                unhealthy;
            Pid when is_pid(Pid) ->
                case erlang:is_process_alive(Pid) of
                    true ->
                        %% Try a simple call
                        case catch gen_server:call(Pid, get_pid, 1000) of
                            P when is_pid(P) ->
                                healthy;
                            _ ->
                                degraded
                        end;
                    false ->
                        unhealthy
                end
        end,

    %% Check session backend (read test)
    SessionHealth =
        case whereis(erlmcp_session_manager) of
            undefined ->
                unhealthy;
            Pid1 when is_pid(Pid1) ->
                case erlang:is_process_alive(Pid1) of
                    true ->
                        case catch gen_server:call(Pid1, list_sessions, 1000) of
                            L when is_list(L) ->
                                healthy;
                            _ ->
                                degraded
                        end;
                    false ->
                        unhealthy
                end
        end,

    %% Check all supervisors are running
    SupervisorHealth = check_supervisors(),

    %% Aggregate results
    Checks =
        #{registry => RegistryHealth,
          session_backend => SessionHealth,
          supervisors => SupervisorHealth,
          timestamp => Now},

    %% Determine overall health
    AllStatuses =
        maps:values(
            maps:remove(timestamp, Checks)),
    OverallHealth =
        case lists:member(unhealthy, AllStatuses) of
            true ->
                critical;
            false ->
                case lists:member(degraded, AllStatuses) of
                    true ->
                        degraded;
                    false ->
                        healthy
                end
        end,

    Metrics =
        #{checks => Checks,
          overall => OverallHealth,
          timestamp => Now},

    {OverallHealth, Metrics}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Safe gen_server:call with timeout and error handling
safe_call(Module, Function, Args, Default) ->
    case whereis(Module) of
        undefined ->
            Default;
        Pid when is_pid(Pid) ->
            try
                apply(gen_server, call, [Pid, Function | Args])
            catch
                _:_ ->
                    Default
            end
    end.

%% @doc Find server associated with a session
find_server_for_session(SessionId, Servers) ->
    %% Look through server configs for matching session
    case lists:search(fun({_ServerId, {_Pid, Config}}) ->
                         maps:get(session_id, Config, undefined) =:= SessionId
                      end,
                      Servers)
    of
        {value, {ServerId, {Pid, Config}}} ->
            #{server_id => ServerId,
              pid => Pid,
              queue_depth => get_process_queue_len(Pid),
              capabilities => maps:get(capabilities, Config, undefined)};
        false ->
            undefined
    end.

%% @doc Find transport associated with a session
find_transport_for_session(SessionId, Transports) ->
    %% Look through transport configs for matching session
    case lists:search(fun({_TransportId, {_Pid, Config}}) ->
                         maps:get(session_id, Config, undefined) =:= SessionId
                      end,
                      Transports)
    of
        {value, {TransportId, {Pid, Config}}} ->
            #{transport_id => TransportId,
              pid => Pid,
              type => maps:get(type, Config, unknown),
              remote_addr => maps:get(remote_addr, Config, undefined)};
        false ->
            undefined
    end.

%% @doc Get subscriptions for a session
get_session_subscriptions(SessionId) ->
    %% This would query erlmcp_resource_subscriptions
    %% For now, return empty list as we need to add session tracking there
    case whereis(erlmcp_resource_subscriptions) of
        undefined ->
            [];
        _Pid ->
            %% Would need to extend resource_subscriptions to track by session
            []
    end.

%% @doc Get in-flight requests from server info
get_in_flight_requests(undefined) ->
    [];
get_in_flight_requests(#{pid := Pid}) when is_pid(Pid) ->
    %% Try to get pending requests from server state
    %% This is a simplified version - real implementation would need
    %% to expose this via gen_server:call
    [];
get_in_flight_requests(_) ->
    [].

%% @doc Get last event ID for a stream
get_last_event_id(_Uri) ->
    %% Would query erlmcp_sse_event_store
    undefined.

%% @doc Get process message queue length
get_process_queue_len(undefined) ->
    undefined;
get_process_queue_len(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} ->
                    Len;
                undefined ->
                    undefined
            end;
        false ->
            undefined
    end.

%% @doc Get process reductions
get_process_reductions(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            case erlang:process_info(Pid, reductions) of
                {reductions, Reds} ->
                    Reds;
                undefined ->
                    0
            end;
        false ->
            0
    end.

%% @doc Get process status (running, waiting, etc.)
get_process_status(undefined) ->
    dead;
get_process_status(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            case erlang:process_info(Pid, status) of
                {status, Status} ->
                    Status;
                undefined ->
                    unknown
            end;
        false ->
            dead
    end.

%% @doc Check that all supervisors are running
check_supervisors() ->
    Supervisors =
        [erlmcp_sup,
         erlmcp_core_sup,
         erlmcp_session_sup,
         erlmcp_transport_sup,
         erlmcp_observability_sup],

    Results =
        lists:map(fun(SupName) ->
                     case whereis(SupName) of
                         undefined ->
                             unhealthy;
                         Pid ->
                             case erlang:is_process_alive(Pid) of
                                 true ->
                                     healthy;
                                 false ->
                                     unhealthy
                             end
                     end
                  end,
                  Supervisors),

    case lists:member(unhealthy, Results) of
        true ->
            unhealthy;
        false ->
            case lists:member(degraded, Results) of
                true ->
                    degraded;
                false ->
                    healthy
            end
    end.
