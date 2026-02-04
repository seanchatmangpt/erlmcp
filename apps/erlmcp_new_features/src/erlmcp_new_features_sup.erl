-module(erlmcp_new_features_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SUP, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?SUP}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => erlmcp_mcp_proxy_relay,
            start => {erlmcp_mcp_proxy_relay, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_mcp_proxy_relay]
        },
        #{
            id => erlmcp_batch_processor,
            start => {erlmcp_batch_processor, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_batch_processor]
        },
        #{
            id => erlmcp_json_schema_validator,
            start => {erlmcp_json_schema_validator, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_json_schema_validator]
        },
        #{
            id => erlmcp_event_bus,
            start => {erlmcp_event_bus, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_event_bus]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
