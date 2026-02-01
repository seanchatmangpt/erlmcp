%%%====================================================================
%%% @doc Mermaid Renderer Supervisor
%%%
%%% Supervises Mermaid diagram renderer processes using simple_one_for_one
%%% strategy. Each renderer gets its own supervised process with proper
%%% isolation and error recovery.
%%%
%%% Supervision Strategy: simple_one_for_one
%%% - Dynamic worker processes (one per renderer instance)
%%% - Automatic restart on crashes
%%% - Isolated failure domains
%%%
%%% @end
%%%====================================================================
-module(erlmcp_mermaid_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_renderer/0, start_renderer/1]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a Mermaid renderer with default configuration
-spec start_renderer() -> {ok, pid()} | {error, term()}.
start_renderer() ->
    start_renderer(#{}).

%% @doc Start a Mermaid renderer with custom configuration
-spec start_renderer(map()) -> {ok, pid()} | {error, term()}.
start_renderer(Config) ->
    supervisor:start_child(?MODULE, [Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => erlmcp_mermaid_renderer,
        start => {erlmcp_mermaid_renderer, start_link, []},
        restart => permanent,  % Always restart on crash
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_mermaid_renderer]
    },

    {ok, {SupFlags, [ChildSpec]}}.
