%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_sup - CLI Subsystem Supervisor
%%%
%%% Manages CLI infrastructure with 3-tier supervision tree.
%%% Failures in CLI do not affect core MCP protocol operation.
%%%
%%% Supervision Strategy: one_for_all
%%% - CLI registry crash: restart entire subsystem
%%% - CLI config crash: restart entire subsystem
%%% - CLI secrets crash: restart entire subsystem
%%% - Session supervisor: simple_one_for_one per-connection
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the CLI supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the CLI supervisor with options
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Opts).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the supervisor with CLI workers
%% Strategy: one_for_all - CLI failures restart the entire subsystem
%% Session supervisor uses simple_one_for_one for per-connection isolation
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Opts) ->
    SupFlags =
        #{strategy => one_for_all,
          intensity => 5,       % Max 5 restarts
          period => 60},        % Within 60 seconds

    %% Child specifications
    Children =
        [%% CLI Registry - Command registry and routing
         %% Must start before CLI sessions that use it
         #{id => erlmcp_cli_registry,
           start => {erlmcp_cli_registry, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cli_registry]},
         %% CLI Configuration Manager
         #{id => erlmcp_cli_config,
           start => {erlmcp_cli_config, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cli_config]},
         %% CLI Secrets Manager
         #{id => erlmcp_cli_secrets,
           start => {erlmcp_cli_secrets, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cli_secrets]},
         %% CLI Session Supervisor - Per-connection isolation
         %% Strategy: simple_one_for_one for dynamic session management
         #{id => erlmcp_cli_session_sup,
           start => {erlmcp_cli_session_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_cli_session_sup]},
         %% CLI Transport Supervisor
         #{id => erlmcp_cli_transport_sup,
           start => {erlmcp_cli_transport_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_cli_transport_sup]},
         %% CLI Metrics Integration
         #{id => erlmcp_cli_metrics,
           start => {erlmcp_cli_metrics, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_cli_metrics]}],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================