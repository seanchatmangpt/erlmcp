%%%-------------------------------------------------------------------
%%% @doc A2A Task Store — DETS-backed durable persistence
%%%
%%% Durable backing for the A2A task manager's task_table and
%%% push_config_table. One DETS set file, keys at keypos 1:
%%%
%%%   {TaskId, {task / push-config entries}, ...}
%%%
%%% Storage layout (keypos 1 set):
%%%   {TaskId, {task, Task}}                  — persisted task
%%%   {{push, TaskId, ConfigId}, PushConfig}  — persisted push config
%%%
%%% Contract:
%%% - open/repair on init: a corrupt file is quarantined to
%%%   <path>.corrupt-<unique> with a logged refusal, then a fresh file
%%%   is opened — never a crash;
%%% - write-through on put/delete; a DETS write failure is logged,
%%%   never fatal (in-memory ETS remains the serving layer);
%%% - load-on-start returns persisted tasks and push configs so the
%%%   caller can re-seed its ETS tables.
%%%
%%% Pattern follows erlmcp_session_dets.erl (dets:open_file with
%%% auto_save, first-element keys, plain dets ops).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_task_store_dets).

%% API
-export([init/1, close/1, put_task/3, delete_task/2, put_push_config/4,
         delete_push_config/3, load/1, info/1]).

-type task_id() :: binary().
-type config_id() :: binary().
-type path() :: file:filename_all().
-type store() :: #{table := atom(), path := path()}.

-export_type([store/0]).

-define(TABLE, erlmcp_a2a_task_store_dets_tab).

%%====================================================================
%% API
%%====================================================================

%% @doc Open (or repair) the DETS file. Opts (required key):
%% #{dets_path := Path, auto_save => Ms (default 10000)}.
-spec init(map()) -> {ok, store()}.
init(Opts) when is_map(Opts) ->
    Path = maps:get(dets_path, Opts),
    AutoSave = maps:get(auto_save, Opts, 10000),
    case open(Path, AutoSave) of
        {ok, _} = Ok ->
            Ok;
        {error, Reason} ->
            logger:error(
              "erlmcp_a2a_task_store_dets: open failed for ~ts (~0p); "
              "refusing file, quarantining, starting fresh", [Path, Reason]),
            Quarantine = quarantine_path(Path),
            _ = file:rename(Path, Quarantine),
            {ok, _} = open(Path, AutoSave)
    end.

%% @doc Close the store (sync + close). Safe on undefined.
-spec close(store() | undefined) -> ok.
close(undefined) ->
    ok;
close(#{table := State}) ->
    catch dets:sync(State),
    catch dets:close(State),
    ok.

%% @doc Write-through a task. Returns ok even on DETS failure (logged).
-spec put_task(task_id(), term(), store()) -> ok.
put_task(TaskId, Task, #{table := Table}) ->
    write(Table, {TaskId, {task, Task}}).

%% @doc Delete a persisted task by id.
-spec delete_task(task_id(), store()) -> ok.
delete_task(TaskId, #{table := ETS_TABLE}) ->
    case dets:delete(ETS_TABLE, TaskId) of
        ok -> ok;
        {error, Reason} ->
            logger:error("erlmcp_a2a_task_store_dets: delete_task ~0p failed: ~0p",
                         [TaskId, Reason]),
            ok
    end.

%% @doc Write-through a push config (keyed by task id + config id).
-spec put_push_config(task_id(), config_id(), term(), store()) -> ok.
put_push_config(TaskId, ConfigId, PushConfig, #{table := Table}) ->
    write(Table, {{push, TaskId, ConfigId}, PushConfig}).

%% @doc Delete a persisted push config by task id + config id.
-spec delete_push_config(task_id(), config_id(), store()) -> ok.
delete_push_config(TaskId, ConfigId, #{table := Table}) ->
    case dets:delete(Table, {push, TaskId, ConfigId}) of
        ok -> ok;
        {error, Reason} ->
            logger:error("erlmcp_a2a_task_store_dets: delete_push_config ~0p/~0p failed: ~0p",
                         [TaskId, ConfigId, Reason]),
            ok
    end.

%% @doc Load all persisted tasks and push configs:
%% #{tasks => [{TaskId, Task}], push_configs => [{TaskId, ConfigId, Config}]}.
-spec load(store()) -> #{tasks := [{task_id(), term()}],
                         push_configs := [{task_id(), config_id(), term()}]}.
load(#{table := Table}) ->
    Empty = #{tasks => [], push_configs => []},
    Fold = fun({TaskId, {task, Task}}, Acc) ->
               Tasks = [{TaskId, Task} | maps:get(tasks, Acc)],
               maps:put(tasks, Tasks, Acc);
              ({{push, TaskId, ConfigId}, Config}, Acc) ->
               PushConfigs = [{TaskId, ConfigId, Config} | maps:get(push_configs, Acc)],
               maps:put(push_configs, PushConfigs, Acc);
              (_Other, Acc) ->
               Acc
           end,
    try dets:foldr(Fold, Empty, Table) of
        M when is_map(M) -> M;
        {error, Reason} ->
            logger:error("erlmcp a2a load fold failed: ~0p", [Reason]),
            Empty
    catch
        _:_ -> Empty
    end.

%% @doc Introspection: #{path => Path, size => N}.
-spec info(store()) -> #{path := path(), size := non_neg_integer()}.
info(#{table := Table, path := Path}) ->
    Size = case dets:info(Table, size) of
        N when is_integer(N) -> N;
        _ -> 0
    end,
    #{path => Path, size => Size}.

%%====================================================================
%% Internal
%%====================================================================

-spec write(atom(), tuple()) -> ok.
write(Table, Entry) ->
    case dets:insert(Table, Entry) of
        ok -> ok;
        {error, Reason} ->
            logger:error("erlmcp_a2a_task_store_dets: insert failed: ~0p", [Reason]),
            ok
    end.

-spec open(path(), pos_integer()) -> {ok, store()} | {error, term()}.
open(Path, AutoSave) ->
    %% Re-assert name ownership: a previous incarnation may still be
    %% open on this node if the caller skipped close/1.
    catch dets:close(?TABLE),
    case dets:open_file(?TABLE, [{file, Path}, {type, set}, {access, read_write},
                                 {auto_save, AutoSave}]) of
        {ok, ?TABLE} -> {ok, #{table => ?TABLE, path => Path}};
        {error, Reason} -> {error, Reason}
    end.

-spec quarantine_path(path()) -> path().
quarantine_path(Path) ->
    Unique = erlang:unique_integer([positive]),
    unicode:characters_to_binary(io_lib:format("~ts.corrupt-~b", [Path, Unique])).
