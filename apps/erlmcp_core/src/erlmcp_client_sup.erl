-module(erlmcp_client_sup).
-behaviour(supervisor).

-export([start_link/0, start_client/1, start_client/2]).
-export([init/1]).

-include("erlmcp.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the client supervisor.
%% This supervisor manages dynamically created client processes using
%% the simple_one_for_one strategy.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new client process with transport options only.
%% Equivalent to start_client(TransportOpts, #{}).
-spec start_client(erlmcp_client:transport_opts()) ->
    {ok, pid()} | {error, term()}.
start_client(TransportOpts) ->
    start_client(TransportOpts, #{}).

%% @doc Starts a new client process with transport and client options.
%% The client process is supervised by this supervisor with a temporary
%% restart strategy, meaning it will not be restarted on crash.
%%
%% TransportOpts: {stdio, list()} | {tcp, map()} | {http, map()}
%% ClientOpts: #{strict_mode => boolean(), timeout => timeout(), ...}
-spec start_client(erlmcp_client:transport_opts(), erlmcp_client:client_opts()) ->
    {ok, pid()} | {error, term()}.
start_client(TransportOpts, ClientOpts) ->
    % For simple_one_for_one, we pass the arguments that will be appended
    % to the start function's argument list from the child spec template
    supervisor:start_child(?MODULE, [TransportOpts, ClientOpts]).

%%====================================================================
%% supervisor callbacks
%%====================================================================

%% @doc Initializes the client supervisor.
%% Uses simple_one_for_one strategy for dynamic client spawning.
%%
%% Strategy: simple_one_for_one - allows dynamic addition of children
%% Restart: temporary - clients are not restarted on crash (process-per-connection)
%% Shutdown: 5000ms - allows graceful cleanup of client state
%% Intensity/Period: 5 restarts per 60 seconds
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,  % Dynamic client instances
        intensity => 5,
        period => 60
    },

    % Template child spec for client instances
    % For simple_one_for_one, the start args are placeholder values.
    % When start_client/2 is called, the provided args are appended to
    % the start function's argument list.
    ChildSpecs = [
        #{
            id => erlmcp_client,
            start => {erlmcp_client, start_link, [undefined, #{}]},
            restart => temporary,  % Clients should not restart on crash
            shutdown => 5000,      % Graceful shutdown timeout
            type => worker,
            modules => [erlmcp_client]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
