%%%-------------------------------------------------------------------
%%% @doc A2A Stream Supervisor
%%%
%%% Manages dynamic stream processes using simple_one_for_one strategy.
%%% Each stream handles real-time updates for a subscribed task.
%%%
%%% Strategy: simple_one_for_one - dynamic stream instances
%%% Restart: temporary - streams are not restarted on crash
%%% Shutdown: 5000ms - graceful cleanup of stream state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_stream_sup).

-behaviour(supervisor).

-include("erlmcp_a2a.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the stream supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @doc Initializes the stream supervisor.
%% Uses simple_one_for_one strategy for dynamic stream spawning.
%%
%% Streams are temporary - if they crash, they are not restarted.
%% The client must re-subscribe to get a new stream.
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,  % Dynamic stream instances
          intensity => 10,                  % Allow more restarts for streams
          period => 60},

    %% Template child spec for stream instances
    %% For simple_one_for_one, start args are appended when starting child
    ChildSpecs = [
        #{id => erlmcp_a2a_stream,
          start => {erlmcp_a2a_stream, start_link, []},
          restart => temporary,  % Streams should not restart on crash
          shutdown => 5000,      % Graceful shutdown timeout
          type => worker,
          modules => [erlmcp_a2a_stream]}
    ],

    {ok, {SupFlags, ChildSpecs}}.
