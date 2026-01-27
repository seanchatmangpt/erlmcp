%% @doc Adapter for poolboy worker lifecycle management
%% Allows poolboy to work with arbitrary worker modules

-module(erlmcp_poolboy_worker_adapter).

%% API for poolboy
-export([start_link/1]).

%% @doc Start a worker - poolboy calls this function
%% Args: [ActualWorkerModule, ActualWorkerArgs]
-spec start_link([module() | list()]) -> {ok, pid()} | {error, term()}.
start_link([WorkerModule, WorkerArgs]) ->
    apply(WorkerModule, start_link, WorkerArgs).

