%%% @doc MCP Advisor Supervisor
%%%
%%% Supervises the advisor service components:
%%% - erlmcp_advisor: Main advisor gen_server
%%% - erlmcp_advisor_mcp_server: MCP protocol integration
%%%
%%% Uses one_for_all strategy since MCP server depends on advisor.
%%%
%%% @end
-module(erlmcp_advisor_sup).

-behaviour(supervisor).

%% API exports
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_all,  %% If advisor fails, restart MCP server too
        intensity => 3,
        period => 60
    },

    %% Only start advisor if enabled
    ChildSpecs = case application:get_env(erlmcp_core, enable_advisor, true) of
        true ->
            [
                %% Main advisor service
                #{
                    id => erlmcp_advisor,
                    start => {erlmcp_advisor, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [erlmcp_advisor]
                }
                %% Note: MCP server is optional and can be started separately
                %% if transport integration is needed
            ];
        false ->
            []
    end,

    {ok, {SupFlags, ChildSpecs}}.
