-module(erlmcp_port_sup).

-behaviour(supervisor).

%% Port Driver Supervisor for MCP External Tool Integration
%%
%% This supervisor manages port-based tool workers with proper isolation
%% and restart strategies. Uses one_for_one strategy for port independence.
%%
%% Strategy: one_for_one
%% - Each port worker restarts independently on failure
%% - Failed ports don't affect other ports
%% - Max restart intensity: 5 per 60 seconds

%% API exports
-export([start_link/0,
         start_port_worker/2,
         start_python_bridge/1,
         stop_port_worker/1,
         list_port_workers/0]).

%% supervisor callback
-export([init/1]).

-include("erlmcp.hrl").

%%====================================================================
%% Macros
%%====================================================================

-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start port supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start port worker with command and args
-spec start_port_worker(atom(), {binary() | string(), [binary() | string()]}) ->
    {ok, pid()} | {error, term()}.
start_port_worker(WorkerId, {Command, Args}) ->
    ChildSpec = #{
        id => WorkerId,
        start => {erlmcp_port_tool, start_link, [#{}]},
        restart => transient,     % Don't restart if exits normally
        shutdown => 5000,        % Time to close port gracefully
        type => worker,
        modules => [erlmcp_port_tool]
    },
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, Pid} ->
            %% Start the port
            case erlmcp_port_tool:start_port(Pid, {Command, Args}) of
                {ok, _Port} ->
                    {ok, Pid};
                {error, Reason} ->
                    %% Port failed to start - terminate worker
                    supervisor:terminate_child(?MODULE, WorkerId),
                    supervisor:delete_child(?MODULE, WorkerId),
                    {error, {port_start_failed, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start Python bridge worker
-spec start_python_bridge(atom()) -> {ok, pid()} | {error, term()}.
start_python_bridge(BridgeId) ->
    ChildSpec = #{
        id => BridgeId,
        start => {erlmcp_python_bridge, start_link, [#{}]},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_python_bridge]
    },
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc Stop port worker
-spec stop_port_worker(atom()) -> ok | {error, term()}.
stop_port_worker(WorkerId) ->
    case supervisor:terminate_child(?MODULE, WorkerId) of
        ok ->
            supervisor:delete_child(?MODULE, WorkerId);
        {error, not_found} ->
            ok;
        Error ->
            Error
    end.

%% @doc List all active port workers
-spec list_port_workers() -> [{atom(), pid() | undefined, term()}].
list_port_workers() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(fun({Id, Pid, _Type, Modules}) ->
                 {Id, Pid, Modules}
              end, Children).

%%====================================================================
%% supervisor callbacks
%%====================================================================

%% @doc Initialize port supervisor
%% Strategy: one_for_one for port isolation
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => ?MAX_RESTART,
        period => ?MAX_TIME
    },
    {ok, {SupFlags, []}}.
