-module(erlmcp_metrics_http_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(8080).

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Port]).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([pos_integer()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Port]) ->
    ?LOG_INFO("Starting metrics HTTP server on port ~B~n", [Port]),

    %% Metrics HTTP server worker
    MetricsHttpSpec = #{
        id => erlmcp_metrics_http_worker,
        start => {erlmcp_metrics_http_worker, start_link, [Port]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_metrics_http_worker]
    },

    {ok, {
        #{strategy => one_for_one, intensity => 10, period => 60},
        [MetricsHttpSpec]
    }}.
