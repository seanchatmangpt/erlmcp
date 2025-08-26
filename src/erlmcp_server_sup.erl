-module(erlmcp_server_sup).
-behaviour(supervisor).

-export([start_link/0, start_child/2]).
-export([init/1]).

-include("erlmcp.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(atom(), map()) -> {ok, pid()} | {error, term()}.
start_child(ServerId, Config) ->
    % For simple_one_for_one, we just pass the arguments
    supervisor:start_child(?MODULE, [ServerId, Config]).

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic server instances
        intensity => 5,
        period => 60
    },
    
    % Template child spec for server instances
    ChildSpecs = [
        #{
            id => erlmcp_server_new,
            start => {erlmcp_server_new, start_link, []},
            restart => temporary,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_server_new]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
