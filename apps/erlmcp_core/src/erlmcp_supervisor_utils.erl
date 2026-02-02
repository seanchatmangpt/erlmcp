%%%-------------------------------------------------------------------
%%% @doc erlmcp_supervisor_utils - Supervisor Introspection Utilities
%%%
%%% Provides runtime analysis of OTP supervision trees for:
%%% - Health scoring and status monitoring
%%% - Process tree visualization
%%% - JSON export for external tooling
%%% - Performance metrics collection
%%%
%%% Joe Armstrong Philosophy:
%%% "The supervision tree is the architecture of your system.
%%%  Make it visible. Make it inspectable. Make it understandable."
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_supervisor_utils).

%% API exports
-export([get_children_status/1, get_supervision_tree/1, get_supervision_tree_flat/1,
         count_processes/1, calculate_health_score/1, export_to_json/1, export_to_json_pretty/1,
         get_process_metrics/1, get_tree_metrics/1, find_unhealthy_processes/1,
         get_restart_statistics/1, validate_supervision_tree/1]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Types
%%%===================================================================

-type sup_ref() :: atom() | pid().
-type health_score() :: float(). % 0.0 (critical) to 1.0 (healthy)
-type child_status() ::
    #{id => term(),
      pid => pid() | undefined | restarting,
      type => worker | supervisor,
      modules => [module()],
      status => child_process_status()}.
-type child_process_status() ::
    not_started |
    restarting |
    dead |
    #{status => atom(),
      queue_len => non_neg_integer(),
      memory => non_neg_integer()} |
    #{child_count => non_neg_integer()}.
-type supervision_tree() ::
    #{supervisor => sup_ref(),
      pid => pid() | undefined,
      health_score => health_score(),
      children => [child_node()]}.
-type child_node() ::
    #{id => term(),
      type => worker | supervisor,
      pid => pid() | undefined,
      modules => [module()],
      state => term(),
      tree => supervision_tree() | undefined}.
-type tree_metrics() ::
    #{total_supervisors => non_neg_integer(),
      total_workers => non_neg_integer(),
      total_processes => non_neg_integer(),
      total_memory_bytes => non_neg_integer(),
      max_depth => non_neg_integer(),
      health_score => health_score(),
      unhealthy_count => non_neg_integer()}.

-export_type([sup_ref/0, health_score/0, child_status/0, supervision_tree/0, tree_metrics/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Get status of all direct children of a supervisor
-spec get_children_status(sup_ref()) -> [child_status()].
get_children_status(SupRef) ->
    try
        Children = supervisor:which_children(SupRef),
        [#{id => Id,
           pid => Pid,
           type => Type,
           modules => Mods,
           status => get_child_status(Pid, Type)}
         || {Id, Pid, Type, Mods} <- Children]
    catch
        _:Reason ->
            ?LOG_WARNING("Failed to get children status for ~p: ~p", [SupRef, Reason]),
            []
    end.

%% @doc Get complete supervision tree recursively
-spec get_supervision_tree(sup_ref()) -> supervision_tree().
get_supervision_tree(SupRef) ->
    try
        Pid = resolve_supervisor_pid(SupRef),
        Children = supervisor:which_children(Pid),
        HealthScore = calculate_health_score(SupRef),
        #{supervisor => SupRef,
          pid => Pid,
          health_score => HealthScore,
          children => [format_child(Child) || Child <- Children]}
    catch
        _:Reason ->
            ?LOG_WARNING("Failed to get supervision tree for ~p: ~p", [SupRef, Reason]),
            #{supervisor => SupRef,
              pid => undefined,
              health_score => 0.0,
              children => [],
              error => Reason}
    end.

%% @doc Get flattened supervision tree (all processes in list)
-spec get_supervision_tree_flat(sup_ref()) -> [#{pid := pid(), supervisor := sup_ref()}].
get_supervision_tree_flat(SupRef) ->
    Tree = get_supervision_tree(SupRef),
    flatten_tree(Tree, []).

%% @doc Count total processes in supervision tree
-spec count_processes(sup_ref()) -> non_neg_integer().
count_processes(SupRef) ->
    count_processes_internal(SupRef, 0).

%% @doc Calculate health score for supervision tree (0.0 = critical, 1.0 = healthy)
%% Scoring criteria:
%% - All processes alive and running: 1.0
%% - Dead/restarting processes: -0.1 per process
%% - High message queue (>100): -0.05 per process
%% - High memory (>100MB): -0.02 per process
-spec calculate_health_score(sup_ref()) -> health_score().
calculate_health_score(SupRef) ->
    try
        Children = get_children_status(SupRef),
        TotalChildren = length(Children),

        if TotalChildren =:= 0 ->
               1.0; % Empty supervisor is healthy
           true ->
               Penalties =
                   lists:foldl(fun(Child, Acc) -> Acc + calculate_child_penalty(Child) end,
                               0.0,
                               Children),

               % Start at 1.0 and subtract penalties
               Score = max(0.0, 1.0 - Penalties / TotalChildren),

               % Recursively check child supervisors
               ChildScores =
                   [calculate_health_score(Pid)
                    || #{type := supervisor, pid := Pid} <- Children, is_pid(Pid)],

               case ChildScores of
                   [] ->
                       Score;
                   _ ->
                       (Score + lists:sum(ChildScores)) / (1 + length(ChildScores))
               end
        end
    catch
        _:_ ->
            0.0
    end.

%% @doc Export supervision tree to JSON
-spec export_to_json(sup_ref()) -> binary().
export_to_json(SupRef) ->
    Tree = get_supervision_tree(SupRef),
    Metrics = get_tree_metrics(SupRef),

    Export =
        #{<<"tree">> => serialize_tree(Tree),
          <<"metrics">> => serialize_metrics(Metrics),
          <<"timestamp">> => erlang:system_time(second)},

    erlmcp_json_native:encode(Export).

%% @doc Export supervision tree to pretty-printed JSON
-spec export_to_json_pretty(sup_ref()) -> binary().
export_to_json_pretty(SupRef) ->
    Tree = get_supervision_tree(SupRef),
    Metrics = get_tree_metrics(SupRef),

    Export =
        #{<<"tree">> => serialize_tree(Tree),
          <<"metrics">> => serialize_metrics(Metrics),
          <<"timestamp">> => erlang:system_time(second)},

    %% Note: Native JSON doesn't support pretty print options
    %% For formatted output, use external tooling or post-process
    erlmcp_json_native:encode(Export).

%% @doc Get performance metrics for a single process
-spec get_process_metrics(pid()) -> map() | {error, term()}.
get_process_metrics(Pid) when is_pid(Pid) ->
    case process_info(Pid,
                      [memory,
                       message_queue_len,
                       reductions,
                       status,
                       total_heap_size,
                       heap_size,
                       stack_size,
                       registered_name])
    of
        undefined ->
            {error, dead};
        Info ->
            #{memory_bytes => proplists:get_value(memory, Info, 0),
              message_queue_len => proplists:get_value(message_queue_len, Info, 0),
              reductions => proplists:get_value(reductions, Info, 0),
              status => proplists:get_value(status, Info, unknown),
              total_heap_size => proplists:get_value(total_heap_size, Info, 0),
              heap_size => proplists:get_value(heap_size, Info, 0),
              stack_size => proplists:get_value(stack_size, Info, 0),
              registered_name => proplists:get_value(registered_name, Info, undefined)}
    end;
get_process_metrics(_) ->
    {error, not_pid}.

%% @doc Get aggregate metrics for entire supervision tree
-spec get_tree_metrics(sup_ref()) -> tree_metrics().
get_tree_metrics(SupRef) ->
    Tree = get_supervision_tree(SupRef),
    Flat = get_supervision_tree_flat(SupRef),

    {SupervisorCount, WorkerCount, TotalMemory} =
        lists:foldl(fun(#{pid := Pid}, {Sups, Workers, Mem}) ->
                       case get_process_metrics(Pid) of
                           {error, _} ->
                               {Sups, Workers, Mem};
                           Metrics ->
                               IsSupervisor = is_supervisor(Pid),
                               NewSups =
                                   if IsSupervisor ->
                                          Sups + 1;
                                      true ->
                                          Sups
                                   end,
                               NewWorkers =
                                   if IsSupervisor ->
                                          Workers;
                                      true ->
                                          Workers + 1
                                   end,
                               NewMem = Mem + maps:get(memory_bytes, Metrics, 0),
                               {NewSups, NewWorkers, NewMem}
                       end
                    end,
                    {0, 0, 0},
                    Flat),

    UnhealthyCount = length(find_unhealthy_processes(SupRef)),
    MaxDepth = calculate_max_depth(Tree, 0),

    #{total_supervisors => SupervisorCount,
      total_workers => WorkerCount,
      total_processes => SupervisorCount + WorkerCount,
      total_memory_bytes => TotalMemory,
      max_depth => MaxDepth,
      health_score => maps:get(health_score, Tree, 0.0),
      unhealthy_count => UnhealthyCount}.

%% @doc Find all unhealthy processes in supervision tree
-spec find_unhealthy_processes(sup_ref()) -> [#{pid := pid(), reason := term()}].
find_unhealthy_processes(SupRef) ->
    Children = get_children_status(SupRef),

    Unhealthy =
        lists:filtermap(fun(Child) ->
                           case maps:get(status, Child) of
                               dead ->
                                   {true, #{pid => maps:get(pid, Child), reason => dead}};
                               restarting ->
                                   {true, #{pid => maps:get(pid, Child), reason => restarting}};
                               not_started ->
                                   {true, #{pid => maps:get(pid, Child), reason => not_started}};
                               Status when is_map(Status) ->
                                   QueueLen = maps:get(queue_len, Status, 0),
                                   Memory = maps:get(memory, Status, 0),
                                   if QueueLen > 1000 ->
                                          {true,
                                           #{pid => maps:get(pid, Child),
                                             reason => high_queue,
                                             queue_len => QueueLen}};
                                      Memory > 100000000 ->
                                          {true,
                                           #{pid => maps:get(pid, Child),
                                             reason => high_memory,
                                             memory => Memory}};
                                      true ->
                                          false
                                   end;
                               _ ->
                                   false
                           end
                        end,
                        Children),

    % Recursively check child supervisors
    ChildUnhealthy =
        lists:flatmap(fun(Child) ->
                         case {maps:get(type, Child), maps:get(pid, Child)} of
                             {supervisor, Pid} when is_pid(Pid) ->
                                 find_unhealthy_processes(Pid);
                             _ ->
                                 []
                         end
                      end,
                      Children),

    Unhealthy ++ ChildUnhealthy.

%% @doc Get restart statistics for supervisor
-spec get_restart_statistics(sup_ref()) ->
                                #{intensity := non_neg_integer(),
                                  period := non_neg_integer(),
                                  restarts := non_neg_integer(),
                                  active_children := non_neg_integer(),
                                  specs := non_neg_integer(),
                                  supervisors := non_neg_integer(),
                                  workers := non_neg_integer()}.
get_restart_statistics(SupRef) ->
    try
        Pid = resolve_supervisor_pid(SupRef),
        supervisor:count_children(Pid)
    catch
        _:_ ->
            #{intensity => 0,
              period => 0,
              restarts => 0,
              active_children => 0,
              specs => 0,
              supervisors => 0,
              workers => 0}
    end.

%% @doc Validate supervision tree structure
%% Returns {ok, Report} or {error, Violations}
-spec validate_supervision_tree(sup_ref()) ->
                                   {ok, #{valid := true, checks := [atom()]}} |
                                   {error,
                                    #{valid := false,
                                      violations := [#{check := atom(), reason := term()}]}}.
validate_supervision_tree(SupRef) ->
    Checks =
        [{supervisor_alive, fun() -> is_supervisor_alive(SupRef) end},
         {no_dead_children, fun() -> check_no_dead_children(SupRef) end},
         {no_high_queues, fun() -> check_no_high_queues(SupRef) end},
         {health_score_ok, fun() -> calculate_health_score(SupRef) > 0.5 end}],

    Violations =
        lists:filtermap(fun({CheckName, CheckFun}) ->
                           try
                               case CheckFun() of
                                   true ->
                                       false;
                                   false ->
                                       {true, #{check => CheckName, reason => failed}};
                                   {false, Reason} ->
                                       {true, #{check => CheckName, reason => Reason}}
                               end
                           catch
                               _:Error ->
                                   {true, #{check => CheckName, reason => Error}}
                           end
                        end,
                        Checks),

    case Violations of
        [] ->
            {ok, #{valid => true, checks => [Name || {Name, _} <- Checks]}};
        _ ->
            {error, #{valid => false, violations => Violations}}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Get status of a child process
-spec get_child_status(pid() | undefined | restarting, worker | supervisor) ->
                          child_process_status().
get_child_status(undefined, _) ->
    not_started;
get_child_status(restarting, _) ->
    restarting;
get_child_status(Pid, worker) when is_pid(Pid) ->
    case process_info(Pid, [status, message_queue_len, memory]) of
        undefined ->
            dead;
        Info ->
            #{status => proplists:get_value(status, Info),
              queue_len => proplists:get_value(message_queue_len, Info),
              memory => proplists:get_value(memory, Info)}
    end;
get_child_status(Pid, supervisor) when is_pid(Pid) ->
    try
        #{child_count => length(supervisor:which_children(Pid))}
    catch
        _:_ ->
            dead
    end.

%% @private Format a child for tree representation
-spec format_child({term(), pid() | undefined | restarting, worker | supervisor, [module()]}) ->
                      child_node().
format_child({Id, Pid, supervisor, Mods}) when is_pid(Pid) ->
    #{id => Id,
      type => supervisor,
      pid => Pid,
      modules => Mods,
      state => unavailable,
      tree => get_supervision_tree(Pid)};
format_child({Id, Pid, worker, Mods}) ->
    #{id => Id,
      type => worker,
      pid => Pid,
      modules => Mods,
      state => safe_get_state(Pid),
      tree => undefined};
format_child({Id, undefined, Type, Mods}) ->
    #{id => Id,
      type => Type,
      pid => undefined,
      modules => Mods,
      state => not_started,
      tree => undefined};
format_child({Id, restarting, Type, Mods}) ->
    #{id => Id,
      type => Type,
      pid => restarting,
      modules => Mods,
      state => restarting,
      tree => undefined}.

%% @private Safely get process state with timeout
-spec safe_get_state(pid() | undefined | restarting) -> term().
safe_get_state(Pid) when is_pid(Pid) ->
    try
        sys:get_state(Pid, 1000)
    catch
        _:_ ->
            unavailable
    end;
safe_get_state(_) ->
    unavailable.

%% @private Count processes recursively
-spec count_processes_internal(sup_ref(), non_neg_integer()) -> non_neg_integer().
count_processes_internal(SupRef, Acc) ->
    try
        Pid = resolve_supervisor_pid(SupRef),
        Children = supervisor:which_children(Pid),
        lists:foldl(fun ({_, ChildPid, supervisor, _}, A) when is_pid(ChildPid) ->
                            count_processes_internal(ChildPid, A + 1);
                        ({_, ChildPid, worker, _}, A) when is_pid(ChildPid) ->
                            A + 1;
                        (_, A) ->
                            A
                    end,
                    Acc,
                    Children)
    catch
        _:_ ->
            Acc
    end.

%% @private Calculate penalty for child in health score
-spec calculate_child_penalty(child_status()) -> float().
calculate_child_penalty(#{status := dead}) ->
    0.5; % Dead process is critical
calculate_child_penalty(#{status := restarting}) ->
    0.3; % Restarting is concerning
calculate_child_penalty(#{status := not_started}) ->
    0.4; % Not started is problematic
calculate_child_penalty(#{status := Status}) when is_map(Status) ->
    QueueLen = maps:get(queue_len, Status, 0),
    Memory = maps:get(memory, Status, 0),

    QueuePenalty =
        if QueueLen > 1000 ->
               0.3;
           QueueLen > 100 ->
               0.1;
           true ->
               0.0
        end,

    MemoryPenalty =
        if Memory > 100000000 ->
               0.2; % >100MB
           Memory > 50000000 ->
               0.1;  % >50MB
           true ->
               0.0
        end,

    QueuePenalty + MemoryPenalty;
calculate_child_penalty(_) ->
    0.0.

%% @private Flatten supervision tree to list of processes
-spec flatten_tree(supervision_tree(), [#{pid := pid(), supervisor := sup_ref()}]) ->
                      [#{pid := pid(), supervisor := sup_ref()}].
flatten_tree(#{supervisor := SupRef,
               pid := Pid,
               children := Children},
             Acc)
    when is_pid(Pid) ->
    Entry = #{pid => Pid, supervisor => SupRef},

    ChildEntries =
        lists:flatmap(fun(Child) ->
                         case Child of
                             #{tree := Tree} when is_map(Tree) ->
                                 flatten_tree(Tree, []);
                             #{pid := ChildPid} when is_pid(ChildPid) ->
                                 [#{pid => ChildPid, supervisor => SupRef}];
                             _ ->
                                 []
                         end
                      end,
                      Children),

    [Entry | ChildEntries] ++ Acc;
flatten_tree(_, Acc) ->
    Acc.

%% @private Serialize supervision tree to JSON-compatible map
-spec serialize_tree(supervision_tree()) -> map().
serialize_tree(#{supervisor := SupRef,
                 pid := Pid,
                 health_score := Score,
                 children := Children} =
                   Tree) ->
    #{<<"supervisor">> => atom_to_binary(SupRef, utf8),
      <<"pid">> => serialize_pid(Pid),
      <<"health_score">> => round(Score * 100) / 100,
      <<"children">> => [serialize_child(C) || C <- Children],
      <<"error">> => maps:get(error, Tree, null)}.

%% @private Serialize child node to JSON-compatible map
-spec serialize_child(child_node()) -> map().
serialize_child(#{id := Id,
                  type := Type,
                  pid := Pid,
                  modules := Mods,
                  tree := Tree}) ->
    Base =
        #{<<"id">> => serialize_id(Id),
          <<"type">> => atom_to_binary(Type, utf8),
          <<"pid">> => serialize_pid(Pid),
          <<"modules">> => [atom_to_binary(M, utf8) || M <- Mods]},

    case Tree of
        undefined ->
            Base;
        T when is_map(T) ->
            Base#{<<"tree">> => serialize_tree(T)}
    end.

%% @private Serialize metrics to JSON-compatible map
-spec serialize_metrics(tree_metrics()) -> map().
serialize_metrics(Metrics) ->
    #{<<"total_supervisors">> => maps:get(total_supervisors, Metrics),
      <<"total_workers">> => maps:get(total_workers, Metrics),
      <<"total_processes">> => maps:get(total_processes, Metrics),
      <<"total_memory_bytes">> => maps:get(total_memory_bytes, Metrics),
      <<"total_memory_mb">> => round(maps:get(total_memory_bytes, Metrics) / 1048576 * 100) / 100,
      <<"max_depth">> => maps:get(max_depth, Metrics),
      <<"health_score">> => round(maps:get(health_score, Metrics) * 100) / 100,
      <<"unhealthy_count">> => maps:get(unhealthy_count, Metrics)}.

%% @private Serialize PID to binary
-spec serialize_pid(pid() | undefined | restarting) -> binary() | null.
serialize_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
serialize_pid(undefined) ->
    null;
serialize_pid(restarting) ->
    <<"restarting">>.

%% @private Serialize ID to binary
-spec serialize_id(term()) -> binary().
serialize_id(Id) when is_atom(Id) ->
    atom_to_binary(Id, utf8);
serialize_id(Id) when is_binary(Id) ->
    Id;
serialize_id(Id) ->
    list_to_binary(io_lib:format("~p", [Id])).

%% @private Calculate maximum depth of supervision tree
-spec calculate_max_depth(supervision_tree(), non_neg_integer()) -> non_neg_integer().
calculate_max_depth(#{children := Children}, CurrentDepth) ->
    ChildDepths =
        [calculate_max_depth(Tree, CurrentDepth + 1) || #{tree := Tree} <- Children, is_map(Tree)],

    case ChildDepths of
        [] ->
            CurrentDepth;
        _ ->
            lists:max(ChildDepths)
    end.

%% @private Resolve supervisor reference to PID
-spec resolve_supervisor_pid(sup_ref()) -> pid().
resolve_supervisor_pid(Pid) when is_pid(Pid) ->
    Pid;
resolve_supervisor_pid(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined ->
            error({supervisor_not_running, Name});
        Pid ->
            Pid
    end.

%% @private Check if supervisor is alive
-spec is_supervisor_alive(sup_ref()) -> boolean().
is_supervisor_alive(SupRef) ->
    try
        Pid = resolve_supervisor_pid(SupRef),
        is_process_alive(Pid)
    catch
        _:_ ->
            false
    end.

%% @private Check if process is a supervisor
-spec is_supervisor(pid()) -> boolean().
is_supervisor(Pid) ->
    try
        _ = supervisor:which_children(Pid),
        true
    catch
        _:_ ->
            false
    end.

%% @private Check for dead children
-spec check_no_dead_children(sup_ref()) -> boolean() | {false, term()}.
check_no_dead_children(SupRef) ->
    Children = get_children_status(SupRef),
    DeadChildren = [Id || #{id := Id, status := dead} <- Children],

    case DeadChildren of
        [] ->
            true;
        _ ->
            {false, {dead_children, DeadChildren}}
    end.

%% @private Check for high message queues
-spec check_no_high_queues(sup_ref()) -> boolean() | {false, term()}.
check_no_high_queues(SupRef) ->
    Children = get_children_status(SupRef),
    HighQueues =
        [{Id, QueueLen}
         || #{id := Id, status := Status} <- Children,
            is_map(Status),
            (QueueLen = maps:get(queue_len, Status, 0)) > 1000],

    case HighQueues of
        [] ->
            true;
        _ ->
            {false, {high_queues, HighQueues}}
    end.
