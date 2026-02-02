-module(erlmcp_session_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Session Infrastructure Supervisor
%%
%% Manages session-related infrastructure and management
%% Session management is critical but isolated from other components
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags =
        #{strategy => one_for_one,  % Each session component fails independently
          intensity => 5,
          period => 60},

    ChildSpecs =
        %% ================================================================
        %% SESSION MANAGEMENT: Core session lifecycle and persistence
        %% ================================================================
        [#{id => erlmcp_session_manager,
           start => {erlmcp_session_manager, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_session_manager]},
         %% ================================================================
        %% SESSION REPLICATION: Distributed session replication
        %% ================================================================
        #{id => erlmcp_session_replicator,
           start => {erlmcp_session_replicator, start_link, []},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_session_replicator]},
         %% ================================================================
        %% SESSION FAILOVER: Session failover for high availability
        %% ================================================================
        #{id => erlmcp_session_failover,
           start => {erlmcp_session_failover, start_link, [node()]},
           restart => permanent,
           shutdown => 5000,
           type => worker,
           modules => [erlmcp_session_failover]},
         %% ================================================================
        %% NOTIFICATION HANDLERS: Supervised notification processing
        %% ================================================================
        #{id => erlmcp_notification_handler_sup,
           start => {erlmcp_notification_handler_sup, start_link, []},
           restart => permanent,
           shutdown => infinity,
           type => supervisor,
           modules => [erlmcp_notification_handler_sup]}],

    {ok, {SupFlags, ChildSpecs}}.