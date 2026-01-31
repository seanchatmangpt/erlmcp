-module(erlmcp_recovery_test_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

start_child(ChildId, Args) ->
    supervisor:start_child(?MODULE, [ChildId, Args]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    {ok, {SupFlags, []}}.
