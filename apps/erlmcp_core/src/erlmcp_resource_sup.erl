-module(erlmcp_resource_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%%====================================================================
%% Resource Supervisor
%%
%% Supervises MCP resource management and protocol features:
%% - Resource subscriptions
%% - SSE event storage
%% - Completion, elicitation
%% - Roots and apps servers
%% - Notification handlers
%%
%% Strategy: one_for_one - independent MCP features
%% ====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% ================================================================
        %% RESOURCE SUBSCRIPTIONS: Manage resource subscription lifecycle
        %% ================================================================
        #{
            id => erlmcp_resource_subscriptions,
            start => {erlmcp_resource_subscriptions, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_resource_subscriptions]
        },

        %% ================================================================
        %% SSE EVENT STORE: Server-Sent Events storage for subscriptions
        %% ================================================================
        #{
            id => erlmcp_sse_event_store,
            start => {erlmcp_sse_event_store, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_sse_event_store]
        },

        %% ================================================================
        %% COMPLETION: Argument completion per MCP 2025-11-25 spec
        %% ================================================================
        #{
            id => erlmcp_completion,
            start => {erlmcp_completion, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_completion]
        },

        %% ================================================================
        %% ELICITATION: User input elicitation (inline, url, terminal)
        %% ================================================================
        #{
            id => erlmcp_elicitation,
            start => {erlmcp_elicitation, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_elicitation]
        },

        %% ================================================================
        %% ROOTS: Root directory management for MCP servers
        %% ================================================================
        #{
            id => erlmcp_roots_server,
            start => {erlmcp_roots_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_roots_server]
        },

        %% ================================================================
        %% APPS: Application lifecycle management and sandboxing
        %% ================================================================
        #{
            id => erlmcp_apps_server,
            start => {erlmcp_apps_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_apps_server]
        },

        %% ================================================================
        %% NOTIFICATION HANDLERS: Supervised notification processing (RPN 168)
        %% ================================================================
        #{
            id => erlmcp_notification_handler_sup,
            start => {erlmcp_notification_handler_sup, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [erlmcp_notification_handler_sup]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
