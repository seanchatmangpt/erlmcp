%%%-------------------------------------------------------------------
%%% @doc ErlMCP Monitor Supervisor
%%% Supervises monitoring components for fault tolerance.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_monitor_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_link/1,
    stop_monitoring/0,
    restart_component/1,
    get_component_status/0
]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the supervisor with configuration
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

%% @doc Stop monitoring
-spec stop_monitoring() -> ok.
stop_monitoring() ->
    case whereis(?SERVER) of
        undefined -> ok;
        _Pid -> 
            exit(whereis(?SERVER), shutdown),
            ok
    end.

%% @doc Restart a specific component
-spec restart_component(Component :: atom()) -> ok | {error, term()}.
restart_component(Component) ->
    supervisor:restart_child(?SERVER, Component).

%% @doc Get status of all components
-spec get_component_status() -> [{atom(), pid() | undefined, atom()}].
get_component_status() ->
    case whereis(?SERVER) of
        undefined ->
            [{supervisor, undefined, not_running}];
        _Pid ->
            Children = supervisor:which_children(?SERVER),
            [{Id, Pid, Type} || {Id, Pid, Type, _Modules} <- Children]
    end.

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([Config]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    
    % Define child specifications
    ChildSpecs = [
        % Main monitor process
        #{
            id => erlmcp_monitor,
            start => {erlmcp_monitor, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_monitor]
        },
        
        % Dashboard process (if enabled)
        #{
            id => erlmcp_monitor_dashboard,
            start => {erlmcp_monitor_dashboard, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_monitor_dashboard]
        },
        
        % Alert manager process
        #{
            id => erlmcp_alert_manager,
            start => {erlmcp_alert_manager, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_alert_manager]
        },
        
        % Metrics collector process
        #{
            id => erlmcp_metrics_collector,
            start => {erlmcp_metrics_collector, start_link, [Config]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_metrics_collector]
        }
    ],
    
    % Filter child specs based on configuration
    FilteredChildSpecs = filter_child_specs(ChildSpecs, Config),
    
    {ok, {SupFlags, FilteredChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Filter child specifications based on configuration
filter_child_specs(ChildSpecs, Config) ->
    lists:filter(
        fun(#{id := Id}) ->
            case Id of
                erlmcp_monitor_dashboard ->
                    maps:get(dashboard_enabled, Config, true);
                erlmcp_alert_manager ->
                    maps:get(alerts_enabled, Config, true);
                erlmcp_metrics_collector ->
                    maps:get(metrics_enabled, Config, true);
                _ ->
                    true
            end
        end,
        ChildSpecs
    ).