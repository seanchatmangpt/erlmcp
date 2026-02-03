%% @private
%% Site Replication Supervisor
%% Manages replication services for a specific site
-module(erlmcp_site_replication_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_replication/2, get_status/1, add_target/1, remove_target/1]).

%% Internal exports
-export([init/1]).

%% Records
-record(replication_config, {
    site_id :: binary(),
    mode :: sync | async | hybrid,
    compression :: boolean(),
    encryption :: boolean(),
    batch_size :: pos_integer(),
    interval :: pos_integer(),
    targets :: [binary()],
    bandwidth_limit :: pos_integer() | infinity
}).

-record(replication_metrics, {
    total_replicated :: non_neg_integer(),
    failed_replications :: non_neg_integer(),
    average_lag :: float(),
    last_replication :: integer() | undefined,
    queue_size :: non_neg_integer()
}).

-define(SERVER, ?MODULE).
-define(REPLICATION_WORKERS, 5).
-define(MAX_RETRIES, 3).
-define(BATCH_TIMEOUT, 5000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_replication(binary(), map()) -> ok | {error, term()}.
start_replication(SiteId, Config) ->
    case supervisor:start_child(?SERVER, make_replication_worker_spec(SiteId)) of
        {ok, WorkerPid} ->
            %% Initialize replication for this site
            erlmcp_replication_worker:start_site(SiteId, Config),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_status(binary()) -> {ok, map()} | {error, term()}.
get_status(SiteId) ->
    case supervisor:which_children(?SERVER) of
        Children when is_list(Children) ->
            case lists:keyfind(SiteId, 1, Children) of
                {SiteId, Pid, _, _} when is_pid(Pid) ->
                    erlmcp_replication_worker:get_status(Pid);
                _ ->
                    {error, not_found}
            end;
        _ ->
            {error, not_found}
    end.

-spec add_target(binary()) -> ok.
add_target(TargetSiteId) ->
    %% Add replication target to all workers
    Workers = get_all_workers(),
    lists:foreach(fun(WorkerPid) ->
        erlmcp_replication_worker:add_target(WorkerPid, TargetSiteId)
    end, Workers).

-spec remove_target(binary()) -> ok.
remove_target(TargetSiteId) ->
    %% Remove replication target from all workers
    Workers = get_all_workers(),
    lists:foreach(fun(WorkerPid) ->
        erlmcp_replication_worker.remove_target(WorkerPid, TargetSiteId)
    end, Workers).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Replication Supervisor Strategy
    %% - one_for_one: Individual replication worker failures don't affect others
    %% - Max restarts: 3 per 60 seconds
    %% - Max time: 60 seconds
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 60
    },

    ChildSpecs = [
        %% Multiple replication workers for parallel processing
        #{id => erlmcp_replication_worker_1,
          start => {erlmcp_replication_worker, start_link, [1]},
          restart => permanent,
          shutdown => 10000,
          type => worker,
          modules => [erlmcp_replication_worker]},

        #{id => erlmcp_replication_worker_2,
          start => {erlmcp_replication_worker, start_link, [2]},
          restart => permanent,
          shutdown => 10000,
          type => worker,
          modules => [erlmcp_replication_worker]},

        #{id => erlmcp_replication_worker_3,
          start => {erlmcp_replication_worker, start_link, [3]},
          restart => permanent,
          shutdown => 10000,
          type => worker,
          modules => [erlmcp_replicationworker]},

        #{id => erlmcp_replication_worker_4,
          start => {erlmcp_replication_worker, start_link, [4]},
          restart => permanent,
          shutdown => 10000,
          type => worker,
          modules => [erlmcp_replication_worker]},

        #{id => erlmcp_replication_worker_5,
          start => {erlmcp_replication_worker, start_link, [5]},
          restart => permanent,
          shutdown => 10000,
          type => worker,
          modules => [erlmcp_replication_worker]}
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%====================================================================
%% Internal Functions
%%====================================================================

make_replication_worker_spec(SiteId) ->
    %% Generate worker spec for a specific site
    Id = list_to_atom("replication_" ++ binary_to_list(SiteId)),
    #{id => Id,
      start => {erlmcp_replication_worker, start_link, [SiteId]},
      restart => permanent,
      shutdown => 10000,
      type => worker,
      modules => [erlmcp_replication_worker]}.

get_all_workers() ->
    %% Get all replication worker PIDs
    Workers = supervisor:which_children(?SERVER),
    lists:filtermap(fun({_, Pid, _, worker}) when is_pid(Pid) ->
        {true, Pid};
        _ ->
            false
    end, Workers).

%%====================================================================
%% Debug Functions
%%====================================================================

-spec get_all_active_replications() -> [binary()].
get_all_active_replications() ->
    %% Get all active replication sessions
    Workers = get_all_workers(),
    lists:foldl(fun(WorkerPid, Acc) ->
        case erlmcp_replication_worker.get_replicated_sites(WorkerPid) of
            {ok, Sites} ->
                Acc ++ Sites;
            _ ->
                Acc
        end
    end, [], Workers).