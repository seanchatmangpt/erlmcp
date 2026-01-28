-module(erlmcp_transport_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(TransportId, Type, Config) ->
    ?LOG_INFO(#{
        what => transport_supervisor_start_child,
        transport_id => TransportId,
        transport_type => Type,
        config => Config
    }),

    case transport_module(Type) of
        {ok, Module} ->
            ChildSpec = build_child_spec(TransportId, Module, Type, Config),
            case supervisor:start_child(?MODULE, ChildSpec) of
                {ok, Pid} = Result ->
                    ?LOG_INFO(#{
                        what => transport_child_started,
                        transport_id => TransportId,
                        transport_type => Type,
                        pid => Pid
                    }),
                    Result;
                {error, Reason} = Error ->
                    ?LOG_ERROR(#{
                        what => transport_child_start_failed,
                        transport_id => TransportId,
                        transport_type => Type,
                        reason => Reason
                    }),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR(#{
                what => transport_module_resolution_failed,
                transport_id => TransportId,
                transport_type => Type,
                reason => Reason
            }),
            Error
    end.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO(#{what => transport_supervisor_init}),

    SupFlags = #{
        strategy => one_for_one,  % Transport failures are isolated
        intensity => 5,
        period => 60
    },

    % Start with empty child specs - transports are added dynamically
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Resolve transport type to module name.
-spec transport_module(atom()) -> {ok, module()} | {error, unknown_transport_type}.
transport_module(stdio) -> {ok, erlmcp_transport_stdio};
transport_module(tcp) -> {ok, erlmcp_transport_tcp};
transport_module(http) -> {ok, erlmcp_transport_http};
transport_module(ws) -> {ok, erlmcp_transport_ws};
transport_module(sse) -> {ok, erlmcp_transport_sse};
transport_module(Type) ->
    ?LOG_WARNING(#{
        what => unknown_transport_type,
        transport_type => Type
    }),
    {error, unknown_transport_type}.

%% @doc Build child specification with transport-specific restart strategy.
-spec build_child_spec(atom(), module(), atom(), map()) -> supervisor:child_spec().
build_child_spec(TransportId, Module, Type, Config) ->
    #{
        id => TransportId,
        start => {Module, start_link, [TransportId, Config]},
        restart => restart_strategy(Type),
        shutdown => shutdown_timeout(Type),
        type => worker,
        modules => [Module]
    }.

%% @doc Determine restart strategy based on transport type.
%% stdio: temporary (single-use, don't restart)
%% tcp/http/ws/sse: transient (restart if abnormal exit)
-spec restart_strategy(atom()) -> temporary | transient | permanent.
restart_strategy(stdio) -> temporary;  % stdio is typically one-shot
restart_strategy(tcp) -> transient;    % tcp should restart on failures
restart_strategy(http) -> transient;   % http should restart on failures
restart_strategy(ws) -> transient;     % ws should restart on failures
restart_strategy(sse) -> transient;    % sse should restart on failures
restart_strategy(_) -> temporary.      % default to temporary for unknown

%% @doc Determine shutdown timeout based on transport type.
%% stdio: quick shutdown (2s)
%% tcp/http/ws/sse: graceful shutdown (5s)
-spec shutdown_timeout(atom()) -> non_neg_integer().
shutdown_timeout(stdio) -> 2000;  % stdio should shut down quickly
shutdown_timeout(tcp) -> 5000;    % tcp needs time for connection cleanup
shutdown_timeout(http) -> 5000;   % http needs time for request completion
shutdown_timeout(ws) -> 5000;     % ws needs time for connection cleanup
shutdown_timeout(sse) -> 5000;    % sse needs time for stream cleanup
shutdown_timeout(_) -> 5000.      % default to 5s for unknown
