-module(erlmcp_transport_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/3, stop_child/1, get_child_status/1, transport_module/1]).
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    ?LOG_INFO("Starting transport supervisor"),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(TransportId, Type, Config) ->
    ?LOG_INFO("Starting transport child: ~p type: ~p", [TransportId, Type]),
    
    try
        % Determine the appropriate transport module with enhanced error handling
        Module = transport_module(Type),
        
        ChildSpec = #{
            id => TransportId,
            start => {Module, start_link, [TransportId, Config]},
            restart => permanent,  % Enhanced: Use permanent restart for better reliability
            shutdown => 10000,     % Enhanced: Increased shutdown timeout
            type => worker,
            modules => [Module]
        },
        
        case supervisor:start_child(?MODULE, ChildSpec) of
            {ok, Pid} = Result ->
                ?LOG_INFO("Successfully started transport ~p with PID ~p", [TransportId, Pid]),
                % Initialize health monitoring
                schedule_health_check(TransportId, Pid),
                Result;
            {error, {already_started, Pid}} ->
                ?LOG_WARNING("Transport ~p already started with PID ~p", [TransportId, Pid]),
                {ok, Pid};
            {error, Reason} = Error ->
                ?LOG_ERROR("Failed to start transport ~p: ~p", [TransportId, Reason]),
                Error
        end
    catch
        error:StartupError ->
            ?LOG_ERROR("Exception starting transport ~p: ~p", [TransportId, StartupError]),
            {error, {startup_exception, StartupError}};
        Class:ExceptionReason:Stacktrace ->
            ?LOG_ERROR("Unexpected error starting transport ~p: ~p:~p~n~p", 
                      [TransportId, Class, ExceptionReason, Stacktrace]),
            {error, {unexpected_error, {Class, ExceptionReason}}}
    end.

-spec stop_child(atom()) -> ok | {error, term()}.
stop_child(TransportId) ->
    ?LOG_INFO("Stopping transport child: ~p", [TransportId]),
    case supervisor:terminate_child(?MODULE, TransportId) of
        ok ->
            case supervisor:delete_child(?MODULE, TransportId) of
                ok ->
                    ?LOG_INFO("Successfully stopped and removed transport ~p", [TransportId]),
                    ok;
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to delete transport ~p: ~p", [TransportId, Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to terminate transport ~p: ~p", [TransportId, Reason]),
            Error
    end.

-spec get_child_status(atom()) -> {ok, running | stopped} | {error, not_found}.
get_child_status(TransportId) ->
    case supervisor:which_children(?MODULE) of
        Children when is_list(Children) ->
            case lists:keyfind(TransportId, 1, Children) of
                {TransportId, Pid, worker, _} when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true -> {ok, running};
                        false -> {ok, stopped}
                    end;
                {TransportId, undefined, worker, _} ->
                    {ok, stopped};
                false ->
                    {error, not_found}
            end;
        Error ->
            ?LOG_ERROR("Failed to get children list: ~p", [Error]),
            {error, supervisor_error}
    end.

%% Enhanced transport module resolution with validation
-spec transport_module(atom()) -> module().
transport_module(stdio) -> 
    erlmcp_transport_stdio_new;  % Keep current name for now (Phase 6 will rename)
transport_module(tcp) -> 
    erlmcp_transport_tcp_new;
transport_module(http) -> 
    erlmcp_transport_http_new;
transport_module(Type) ->
    ?LOG_ERROR("Unknown transport type: ~p", [Type]),
    error({unknown_transport_type, Type}).

%%====================================================================
%% Internal Functions
%%====================================================================

%% Health monitoring - schedule periodic checks
-spec schedule_health_check(atom(), pid()) -> ok.
schedule_health_check(TransportId, Pid) ->
    spawn(fun() -> health_monitor_loop(TransportId, Pid) end),
    ok.

%% Health monitoring loop
-spec health_monitor_loop(atom(), pid()) -> ok.
health_monitor_loop(TransportId, Pid) ->
    timer:sleep(30000), % Check every 30 seconds
    case is_process_alive(Pid) of
        true ->
            % Transport is healthy, continue monitoring
            health_monitor_loop(TransportId, Pid);
        false ->
            ?LOG_WARNING("Transport ~p (PID ~p) is no longer alive, health monitoring stopped", 
                        [TransportId, Pid])
    end.

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    ?LOG_INFO("Initializing transport supervisor"),
    
    % Enhanced supervisor flags for better reliability
    SupFlags = #{
        strategy => one_for_one,  % Transport failures are isolated
        intensity => 10,          % Enhanced: Increased restart intensity
        period => 60,             % Period in seconds
        auto_shutdown => never    % Enhanced: Never auto-shutdown
    },
    
    % Start with empty child specs - transports are added dynamically
    ChildSpecs = [],
    
    ?LOG_INFO("Transport supervisor initialized with enhanced configuration"),
    {ok, {SupFlags, ChildSpecs}}.
