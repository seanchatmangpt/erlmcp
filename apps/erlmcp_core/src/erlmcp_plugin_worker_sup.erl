%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Worker Supervisor - Manages plugin worker processes
%%%
%%% Strategy: simple_one_for_one
%%% - Dynamic worker creation
%%% - Each plugin runs in isolated process
%%% - Plugin crash doesn't affect others
%%%
%%% Restart strategy: transient
%%% - Normal exits are not restarted
%%% - Abnormal crashes are restarted
%%% - Ensures plugin failures are isolated
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_plugin/2, stop_plugin/1]).
%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a plugin worker
-spec start_plugin(module(), map()) -> {ok, pid()} | {error, term()}.
start_plugin(Module, Opts) ->
    supervisor:start_child(?SERVER, [Module, Opts]).

%% @doc Stop a plugin worker
-spec stop_plugin(pid()) -> ok.
stop_plugin(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%====================================================================
%% supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,  % Dynamic workers
          intensity => 5,                  % Max 5 restarts
          period => 60},                     % In 60 seconds

    ChildSpec =
        #{id => erlmcp_plugin_worker,
          start => {erlmcp_plugin_worker, start_link, []},
          restart => transient,            % Don't restart on normal exit
          shutdown => 5000,
          type => worker,
          modules => [erlmcp_plugin_worker]},

    {ok, {SupFlags, [ChildSpec]}}.
