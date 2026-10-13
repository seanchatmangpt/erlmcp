%% @doc eunit tests for the A2A DETS task store and the task manager's
%% durable mode. Real processes and real DETS files throughout — no
%% mocks. Each test uses a unique tmp path and cleans up on exit.
-module(erlmcp_a2a_task_store_dets_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include("erlmcp_a2a.hrl").

%%====================================================================
%% Test generators
%%====================================================================

erlmcp_a2a_task_store_dets_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_store_roundtrip/1,
      fun test_delete_survives_restart/1,
      fun test_push_config_roundtrip/1,
      fun test_corrupt_file_repair/1,
      fun test_default_off_is_in_memory/1]}.

%%====================================================================
%% Fixtures
%%====================================================================

setup() ->
    Dir = filename:join(evacuate_tmp(), "erlmcp_a2a_dets_test_" ++
                                           integer_to_list(erlang:unique_integer([positive]))),
    file:make_dir(Dir),
    Dir.

cleanup(Dir) ->
    %% Cleanup law: remove every artifact this test minted
    [catch file:delete(F) || F <- filelib:wildcard(filename:join(Dir, "*"))],
    catch file:del_dir(Dir),
    ok.

tmp_path(Dir) ->
    filename:join(Dir, "a2a_tasks.dets").

evacuate_tmp() ->
    "/tmp".

%%====================================================================
%% Manager lifecycle helpers (real gen_server start/stop)
%%====================================================================

start_manager(Dir) ->
    %% Kill any stray manager from a prior test first
    catch gen_server:stop(erlmcp_a2a_task_manager, normal, 1000),
    {ok, Pid} = erlmcp_a2a_task_manager:start_link(#{dets_path => tmp_path(Dir)}),
    %% Synchronize on handle_continue(initialize_tables) completion
    %% (table + DETS init runs asynchronously after init/1 returns)
    _ = erlmcp_a2a_task_manager:get_stats(),
    Pid.

stop_manager() ->
    ok = gen_server:stop(erlmcp_a2a_task_manager, normal, 5000).

sample_message() ->
    #a2a_message{message_id = <<"msg-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
                 role = user,
                 parts = [#a2a_part{text = <<"hello">>}]}.

sample_push_config(ConfigId) ->
    #a2a_task_push_notification_config{
       id = ConfigId,
       task_id = <<"task">>,
       push_notification_config =
           #a2a_push_notification_config{id = ConfigId, url = <<"https://example.test/hook">>}}.

%%====================================================================
%% Tests
%%====================================================================

%% Put task through the manager, stop, restart with the same path,
%% task readable (the C26 falsifier precondition).
test_store_roundtrip(Dir) ->
    fun() ->
        Pid = start_manager(Dir),
        ?assert(is_process_alive(Pid)),

        CtxId = <<"ctx-roundtrip">>,
        {ok, Task} = erlmcp_a2a_task_manager:create_task(CtxId, sample_message()),
        TaskId = Task#a2a_task.id,

        %% Force flush so the restart cannot rely on auto_save timing
        ok = stop_manager(),

        Pid2 = start_manager(Dir),
        ?assert(is_process_alive(Pid2)),

        {ok, Restored} = erlmcp_a2a_task_manager:get_task(TaskId),
        ?assertEqual(TaskId, Restored#a2a_task.id),
        ?assertEqual(CtxId, Restored#a2a_task.context_id),
        #{tasks := Tasks} = erlmcp_a2a_task_manager:list_tasks(),
        ?assertEqual(1, length(Tasks)),

        ok = stop_manager()
    end.

%% Delete through the manager, restart, task must be gone.
test_delete_survives_restart(Dir) ->
    fun() ->
        _Pid = start_manager(Dir),

        {ok, Task1} = erlmcp_a2a_task_manager:create_task(<<"ctx-del">>, sample_message()),
        {ok, Task2} = erlmcp_a2a_task_manager:create_task(<<"ctx-del">>, sample_message()),

        {ok, 2} = erlmcp_a2a_task_manager:delete_context(<<"ctx-del">>),
        {ok, Survivor0} = erlmcp_a2a_task_manager:create_task(<<"ctx-keep">>, sample_message()),
        ok = stop_manager(),

        _Pid2 = start_manager(Dir),
        ?assertEqual({error, not_found},
                     erlmcp_a2a_task_manager:get_task(Task1#a2a_task.id)),
        ?assertEqual({error, not_found},
                     erlmcp_a2a_task_manager:get_task(Task2#a2a_task.id)),
        {ok, Survivor} = erlmcp_a2a_task_manager:get_task(Survivor0#a2a_task.id),
        ?assertEqual(<<"ctx-keep">>, Survivor#a2a_task.context_id),
        ok = stop_manager()
    end.

%% Push config write-through + restore.
test_push_config_roundtrip(Dir) ->
    fun() ->
        _Pid = start_manager(Dir),
        {ok, Task} = erlmcp_a2a_task_manager:create_task(<<"ctx-push">>, sample_message()),
        TaskId = Task#a2a_task.id,
        ConfigId = <<"cfg-1">>,
        ok = erlmcp_a2a_task_manager:register_push_config(TaskId, sample_push_config(ConfigId)),
        ?assertMatch([_], erlmcp_a2a_task_manager:get_push_configs(TaskId)),

        %% Delete path survives too: register a second, unregister it.
        ConfigId2 = <<"cfg-2">>,
        ok = erlmcp_a2a_task_manager:register_push_config(TaskId, sample_push_config(ConfigId2)),
        ok = erlmcp_a2a_task_manager:unregister_push_config(TaskId, ConfigId2),
        ?assertEqual(1, length(erlmcp_a2a_task_manager:get_push_configs(TaskId))),
        ok = stop_manager(),

        _Pid2 = start_manager(Dir),
        {ok, Restored} = erlmcp_a2a_task_manager:get_task(TaskId),
        ?assertEqual(TaskId, Restored#a2a_task.id),
        Configs = erlmcp_a2a_task_manager:get_push_configs(TaskId),
        ?assertEqual(1, length(Configs)),
        ?assertEqual(ConfigId, (hd(Configs))#a2a_task_push_notification_config.id),
        ok = stop_manager()
    end.

%% Corrupt DETS file: manager starts cleanly, old file quarantined,
%% fresh store works. Logged refusal, never a crash.
test_corrupt_file_repair(Dir) ->
    fun() ->
        Path = tmp_path(Dir),
        ok = file:write_file(Path, <<"this is not a dets file, just garbage bytes">>),
        CorruptSize = size_of(Path),

        Pid = start_manager(Dir),
        ?assert(is_process_alive(Pid)),

        %% Old file quarantined (renamed with .corrupt- suffix)
        Quarantined = filelib:wildcard(filename:join(Dir, "*.corrupt-*")),
        ?assertEqual(1, length(Quarantined)),
        ?assertEqual(CorruptSize, size_of(hd(Quarantined))),
        %% Path now holds a fresh, valid DETS file (recreated on repair)

        %% Fresh store is usable
        {ok, Task} = erlmcp_a2a_task_manager:create_task(<<"ctx-corrupt">>, sample_message()),
        ok = stop_manager(),

        _Pid2 = start_manager(Dir),
        {ok, _} = erlmcp_a2a_task_manager:get_task(Task#a2a_task.id),
        ok = stop_manager()
    end.

%% Default (no dets_path): byte-identical legacy behavior — works,
%% nothing persisted, no .dets file created.
test_default_off_is_in_memory(Dir) ->
    fun() ->
        catch gen_server:stop(erlmcp_a2a_task_manager, normal, 1000),
        {ok, Pid} = erlmcp_a2a_task_manager:start_link(#{}),
        ?assert(is_process_alive(Pid)),

        {ok, Task} = erlmcp_a2a_task_manager:create_task(<<"ctx-mem">>, sample_message()),
        {ok, Task} = erlmcp_a2a_task_manager:get_task(Task#a2a_task.id),

        %% Store-level API confirms nothing was opened on the tmp path
        ?assertEqual(0, length(filelib:wildcard(filename:join(Dir, "*")))),
        ok = stop_manager()
    end.

size_of(Path) ->
    {ok, #file_info{size = Size}} = file:read_file_info(Path),
    Size.
