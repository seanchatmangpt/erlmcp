%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_transport_sup - Transport Layer Supervisor
%%%
%%% Supervisor for all transport workers (stdio, tcp, http, ws, sse).
%%%
%%% Ontology: ggen/ontology/instances/transport_supervisor.ttl
%%%
%%% Generated from ggen methodology - Transport Supervisor instance
%%%
%%% Supervision Strategy: one_for_one
%%% - Each transport worker restarts independently on failure
%%% - stdio: temporary (single-use, don't restart)
%%% - tcp/http/ws/sse: transient (restart on abnormal exit only)
%%% - health_monitor: permanent (always restart - critical for monitoring)
%%%
%%% Restart Intensity: 5 restarts per 60 seconds
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/3, stop_accepting/0]).
-export([which_transports/0, transport_status/0]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type transport_type() :: stdio | tcp | http | ws | sse.
-type transport_id() :: atom().
-type transport_config() :: map().

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(transport_id(), transport_type(), transport_config()) ->
    {ok, pid()} | {error, term()}.
start_child(TransportId, Type, Config) ->
    ?LOG_INFO(#{what => transport_supervisor_start_child,
                transport_id => TransportId,
                transport_type => Type,
                config => Config}),

    case transport_module(Type) of
        {ok, Module} ->
            ChildSpec = build_child_spec(TransportId, Module, Type, Config),
            case supervisor:start_child(?MODULE, ChildSpec) of
                {ok, Pid} = Result ->
                    ?LOG_INFO(#{what => transport_child_started,
                                transport_id => TransportId,
                                transport_type => Type,
                                pid => Pid}),
                    Result;
                {ok, Pid, _Info} = Result ->
                    ?LOG_INFO(#{what => transport_child_started,
                                transport_id => TransportId,
                                transport_type => Type,
                                pid => Pid}),
                    Result;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{what => transport_child_start_failed,
                                 transport_id => TransportId,
                                 transport_type => Type,
                                 reason => Reason}),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{what => transport_module_resolution_failed,
                         transport_id => TransportId,
                         transport_type => Type,
                         reason => Reason}),
            Error
    end.

-spec which_transports() -> [{transport_id(), pid() | undefined, transport_type()}].
which_transports() ->
    Children = supervisor:which_children(?MODULE),
    lists:filtermap(
        fun({Id, Pid, _Type, _Modules}) ->
            case transport_type_from_id(Id) of
                {ok, TransportType} when is_pid(Pid) ->
                    {true, {Id, Pid, TransportType}};
                _ ->
                    false
            end
        end,
        Children
    ).

-spec transport_status() -> #{transport_id() => #{pid := pid() | undefined,
                                                   status => running | restarting | stopped}}.
transport_status() ->
    Children = supervisor:which_children(?MODULE),
    lists:foldl(
        fun({Id, Pid, _Type, _Modules}, Acc) ->
            Status = case Pid of
                undefined -> stopped;
                P when is_pid(P) ->
                    case process_info(P, status) of
                        {status, running} -> running;
                        {status, _} -> restarting;
                        _ -> stopped
                    end;
                _ -> stopped
            end,
            Acc#{Id => #{pid => Pid, status => Status}}
        end,
        #{},
        Children
    ).

%% @doc Stop accepting new connections (graceful shutdown preparation)
-spec stop_accepting() -> ok.
stop_accepting() ->
    ?LOG_INFO(#{what => transport_supervisor_stop_accepting}),
    try
        Children = supervisor:which_children(?MODULE),
        lists:foreach(fun({Id, Pid, Type, Modules}) ->
            case Type of
                worker when is_pid(Pid) ->
                    ?LOG_DEBUG(#{what => telling_transport_to_stop_accepting,
                                  transport_id => Id,
                                  pid => Pid}),
                    try
                        Pid ! stop_accepting,
                        ok
                    catch
                        _:_ -> ok
                    end;
                _ ->
                    ok
            end
        end, Children),
        ?LOG_INFO(#{what => transport_supervisor_stop_accepting_complete}),
        ok
    catch
        _:Error ->
            ?LOG_ERROR(#{what => transport_supervisor_stop_accepting_failed,
                         error => Error}),
            ok
    end.

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO(#{what => transport_supervisor_init}),

    %% Supervision flags from ontology: one_for_one, intensity=5, period=60
    SupFlags =
        #{strategy => one_for_one,
          intensity => 5,
          period => 60},

    %% Child specs from ontology instance definition
    %% Health monitor is permanent - always restart (critical for monitoring)
    %% Transports are added dynamically via start_child/3
    ChildSpecs = [
        health_monitor_child_spec()
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Build health monitor child spec (permanent - always restart)
-spec health_monitor_child_spec() -> supervisor:child_spec().
health_monitor_child_spec() ->
    #{id => erlmcp_transport_health,
      start => {erlmcp_transport_health, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [erlmcp_transport_health]}.

%% @doc Resolve transport type to module name.
-spec transport_module(transport_type()) -> {ok, module()} | {error, unknown_transport_type}.
transport_module(stdio) -> {ok, erlmcp_transport_stdio};
transport_module(tcp) -> {ok, erlmcp_transport_tcp};
transport_module(http) -> {ok, erlmcp_transport_http};
transport_module(ws) -> {ok, erlmcp_transport_ws};
transport_module(sse) -> {ok, erlmcp_transport_sse};
transport_module(Type) ->
    ?LOG_WARNING(#{what => unknown_transport_type, transport_type => Type}),
    {error, unknown_transport_type}.

%% @doc Infer transport type from child ID.
-spec transport_type_from_id(atom()) -> {ok, transport_type()} | {error, unknown}.
transport_type_from_id(erlmcp_transport_stdio) -> {ok, stdio};
transport_type_from_id(erlmcp_transport_tcp) -> {ok, tcp};
transport_type_from_id(erlmcp_transport_http) -> {ok, http};
transport_type_from_id(erlmcp_transport_ws) -> {ok, ws};
transport_type_from_id(erlmcp_transport_sse) -> {ok, sse};
transport_type_from_id(erlmcp_transport_health) -> {ok, health};
transport_type_from_id(_) -> {error, unknown}.

%% @doc Build child specification with transport-specific restart strategy.
%% From ontology: stdio is temporary, tcp/http/ws/sse are transient.
-spec build_child_spec(transport_id(), module(), transport_type(), transport_config()) ->
    supervisor:child_spec().
build_child_spec(TransportId, Module, Type, Config) ->
    #{id => TransportId,
      start => {Module, start_link, [TransportId, Config]},
      restart => restart_strategy(Type),
      shutdown => shutdown_timeout(Type),
      type => worker,
      modules => [Module]}.

%% @doc Determine restart strategy based on transport type.
%% stdio: temporary (single-use, don't restart)
%% tcp/http/ws/sse: transient (restart if abnormal exit)
-spec restart_strategy(transport_type()) -> temporary | transient.
restart_strategy(stdio) -> temporary;  % stdio is typically one-shot
restart_strategy(tcp) -> transient;     % tcp should restart on failures
restart_strategy(http) -> transient;    % http should restart on failures
restart_strategy(ws) -> transient;      % ws should restart on failures
restart_strategy(sse) -> transient.     % sse should restart on failures

%% @doc Determine shutdown timeout based on transport type.
%% stdio: quick shutdown (2s)
%% tcp/http/ws/sse: graceful shutdown (5s)
-spec shutdown_timeout(transport_type()) -> pos_integer().
shutdown_timeout(stdio) -> 2000;  % stdio should shut down quickly
shutdown_timeout(tcp) -> 5000;    % tcp needs time for connection cleanup
shutdown_timeout(http) -> 5000;   % http needs time for request completion
shutdown_timeout(ws) -> 5000;     % ws needs time for connection cleanup
shutdown_timeout(sse) -> 5000.    % sse needs time for stream cleanup
