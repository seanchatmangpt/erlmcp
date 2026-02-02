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

%% @doc OTP 28 Supervisor Auto-Hibernation - Disabled
%%
%% Dynamic supervisors with simple_one_for_one strategy should NOT
%% auto-hibernate because:
%%
%% 1. Frequent child restarts: MCP servers start/stop frequently
%% 2. Wake overhead: Hibernation wake adds latency to child operations
%% 3. Minimal benefit: Dynamic supervisors are already memory-efficient
%%
%% Static supervisors (erlmcp_sup) DO hibernate after 1s idle.
%%
%% See: docs/SUPERVISOR_HIBERNATION_OTP28.md
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => simple_one_for_one,  % Dynamic server instances
          intensity => 5,
          period => 60,
          auto_hibernation => false  % Explicitly disable for dynamic supervisor
         },

    % Template child spec for server instances
    % CRITICAL FIX: For simple_one_for_one, start args MUST be a list.
    % The args passed to start_child/2 are appended to this list.
    % We provide placeholder args that will be replaced.
    ChildSpecs =
        [#{id => erlmcp_server,
           start => {erlmcp_server, start_link, [undefined, #{}]},
           restart => temporary,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_server]}],

    {ok, {SupFlags, ChildSpecs}}.
