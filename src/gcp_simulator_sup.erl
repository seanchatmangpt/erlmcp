%%%-------------------------------------------------------------------
%% @doc GCP Simulator Supervisor
%%
%% Supervises all GCP service simulators:
%% - Cloud Storage
%% - Pub/Sub
%% - IAM
%% - Compute Engine
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_simulator_sup).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    stop/0,
    start_service/1,
    stop_service/1,
    list_services/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid ->
            exit(Pid, shutdown),
            ok
    end.

%% @doc Start a specific service.
-spec start_service(atom()) -> {ok, pid()} | {error, term()}.
start_service(Service) ->
    ChildSpec = child_spec(Service),
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc Stop a specific service.
-spec stop_service(atom()) -> ok | {error, term()}.
stop_service(Service) ->
    case supervisor:terminate_child(?SERVER, Service) of
        ok -> supervisor:delete_child(?SERVER, Service);
        Error -> Error
    end.

%% @doc List running services.
-spec list_services() -> [atom()].
list_services() ->
    Children = supervisor:which_children(?SERVER),
    [Id || {Id, Pid, _, _} <- Children, is_pid(Pid)].

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    Children = [
        child_spec(gcp_storage_sim),
        child_spec(gcp_pubsub_sim),
        child_spec(gcp_iam_sim),
        child_spec(gcp_compute_sim)
    ],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================

child_spec(gcp_storage_sim) ->
    #{
        id => gcp_storage_sim,
        start => {gcp_storage_sim, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [gcp_storage_sim]
    };

child_spec(gcp_pubsub_sim) ->
    #{
        id => gcp_pubsub_sim,
        start => {gcp_pubsub_sim, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [gcp_pubsub_sim]
    };

child_spec(gcp_iam_sim) ->
    #{
        id => gcp_iam_sim,
        start => {gcp_iam_sim, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [gcp_iam_sim]
    };

child_spec(gcp_compute_sim) ->
    #{
        id => gcp_compute_sim,
        start => {gcp_compute_sim, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [gcp_compute_sim]
    }.
