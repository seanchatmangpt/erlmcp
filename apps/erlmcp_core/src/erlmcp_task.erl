-module(erlmcp_task).

-include("erlmcp.hrl").

%% API exports
-export([
    create/2,
    create/3,
    get_task/1,
    update_status/2,
    list_tasks/0,
    cancel_task/1
]).

%% Types
-type task_id() :: binary().
-type task_status() :: queued | running | completed | failed | cancelled | cancel_requested.
-type task() :: #{
    id := task_id(),
    name := binary(),
    status := task_status(),
    created_at := integer(),
    updated_at := integer(),
    result => term(),
    error => term()
}.

-export_type([task_id/0, task_status/0, task/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec create(binary(), map()) -> task().
create(Name, Params) ->
    create(Name, Params, queued).

-spec create(binary(), map(), task_status()) -> task().
create(Name, _Params, Status) ->
    #{
        id => generate_task_id(),
        name => Name,
        status => Status,
        created_at => erlang:system_time(millisecond),
        updated_at => erlang:system_time(millisecond)
    }.

-spec get_task(task_id()) -> {ok, task()} | {error, not_found}.
get_task(_TaskId) ->
    %% TODO: Implement persistent task storage
    {error, not_found}.

-spec update_status(task_id(), task_status()) -> ok | {error, term()}.
update_status(_TaskId, _Status) ->
    %% TODO: Implement task status updates
    ok.

-spec list_tasks() -> [task()].
list_tasks() ->
    %% TODO: Implement task listing
    [].

-spec cancel_task(task_id()) -> ok | {error, term()}.
cancel_task(_TaskId) ->
    %% TODO: Implement task cancellation
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_task_id() -> task_id().
generate_task_id() ->
    Rand = crypto:strong_rand_bytes(16),
    binary:encode_hex(Rand).
