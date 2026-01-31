%%%-------------------------------------------------------------------
%%% @doc supervisor_utils_example - Usage Examples for erlmcp_supervisor_utils
%%%
%%% This module demonstrates how to use the supervisor introspection
%%% utilities in production applications.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(supervisor_utils_example).

-export([
    example_basic_status/0,
    example_health_monitoring/0,
    example_json_export/0,
    example_metrics_dashboard/0,
    example_find_problems/0,
    example_validation/0,
    example_monitoring_loop/0
]).

%%%===================================================================
%%% Basic Usage Examples
%%%===================================================================

%% @doc Example 1: Get basic status of erlmcp_sup children
example_basic_status() ->
    io:format("=== Basic Supervisor Status ===~n"),

    %% Get direct children status
    Children = erlmcp_supervisor_utils:get_children_status(erlmcp_sup),

    io:format("Total children: ~p~n", [length(Children)]),

    %% Print each child's status
    lists:foreach(fun(Child) ->
        #{id := Id, type := Type, pid := Pid, status := Status} = Child,
        io:format("  ~p (~p): ~p - ~p~n", [Id, Type, Pid, Status])
    end, Children),

    ok.

%% @doc Example 2: Monitor health scores over time
example_health_monitoring() ->
    io:format("=== Health Score Monitoring ===~n"),

    Supervisors = [erlmcp_sup, erlmcp_core_sup, erlmcp_server_sup],

    lists:foreach(fun(SupRef) ->
        case whereis(SupRef) of
            undefined ->
                io:format("~p: NOT RUNNING~n", [SupRef]);
            _ ->
                Score = erlmcp_supervisor_utils:calculate_health_score(SupRef),
                Status = case Score of
                    S when S >= 0.9 -> "HEALTHY";
                    S when S >= 0.7 -> "DEGRADED";
                    S when S >= 0.5 -> "WARNING";
                    _ -> "CRITICAL"
                end,
                io:format("~p: ~.2f (~s)~n", [SupRef, Score, Status])
        end
    end, Supervisors),

    ok.

%% @doc Example 3: Export supervision tree to JSON file
example_json_export() ->
    io:format("=== JSON Export ===~n"),

    %% Export erlmcp_sup tree to JSON
    Json = erlmcp_supervisor_utils:export_to_json_pretty(erlmcp_sup),

    %% Write to file
    Filename = "/tmp/erlmcp_supervision_tree.json",
    ok = file:write_file(Filename, Json),

    io:format("Exported supervision tree to: ~s~n", [Filename]),
    io:format("Size: ~p bytes~n", [byte_size(Json)]),

    %% Parse and display metrics
    Decoded = jsx:decode(Json, [return_maps]),
    Metrics = maps:get(<<"metrics">>, Decoded),

    io:format("~nMetrics:~n"),
    io:format("  Total processes: ~p~n", [maps:get(<<"total_processes">>, Metrics)]),
    io:format("  Total memory: ~.2f MB~n", [maps:get(<<"total_memory_mb">>, Metrics)]),
    io:format("  Health score: ~p~n", [maps:get(<<"health_score">>, Metrics)]),
    io:format("  Max depth: ~p~n", [maps:get(<<"max_depth">>, Metrics)]),

    ok.

%% @doc Example 4: Build a metrics dashboard
example_metrics_dashboard() ->
    io:format("=== Metrics Dashboard ===~n~n"),

    %% Get comprehensive metrics
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup),

    #{
        total_supervisors := Sups,
        total_workers := Workers,
        total_processes := Total,
        total_memory_bytes := MemBytes,
        max_depth := Depth,
        health_score := Score,
        unhealthy_count := Unhealthy
    } = Metrics,

    %% Display dashboard
    io:format("┌─────────────────────────────────────────┐~n"),
    io:format("│  ERLMCP SUPERVISION TREE METRICS        │~n"),
    io:format("├─────────────────────────────────────────┤~n"),
    io:format("│  Supervisors:     ~6w                  │~n", [Sups]),
    io:format("│  Workers:         ~6w                  │~n", [Workers]),
    io:format("│  Total Processes: ~6w                  │~n", [Total]),
    io:format("│  Memory (MB):     ~6.2f                  │~n", [MemBytes / 1048576]),
    io:format("│  Max Depth:       ~6w                  │~n", [Depth]),
    io:format("│  Health Score:    ~6.2f                  │~n", [Score]),
    io:format("│  Unhealthy:       ~6w                  │~n", [Unhealthy]),
    io:format("└─────────────────────────────────────────┘~n"),

    ok.

%% @doc Example 5: Find and report problematic processes
example_find_problems() ->
    io:format("=== Problem Detection ===~n"),

    %% Find unhealthy processes in the tree
    Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup),

    case Problems of
        [] ->
            io:format("✓ No problems detected - all processes healthy~n");
        _ ->
            io:format("⚠ Found ~p problematic processes:~n", [length(Problems)]),
            lists:foreach(fun(Problem) ->
                #{pid := Pid, reason := Reason} = Problem,
                io:format("  - ~p: ~p", [Pid, Reason]),
                case Problem of
                    #{queue_len := QLen} ->
                        io:format(" (queue: ~p)", [QLen]);
                    #{memory := Mem} ->
                        io:format(" (memory: ~.2f MB)", [Mem / 1048576]);
                    _ ->
                        ok
                end,
                io:format("~n")
            end, Problems)
    end,

    ok.

%% @doc Example 6: Validate supervision tree
example_validation() ->
    io:format("=== Supervision Tree Validation ===~n"),

    case erlmcp_supervisor_utils:validate_supervision_tree(erlmcp_sup) of
        {ok, #{valid := true, checks := Checks}} ->
            io:format("✓ VALIDATION PASSED~n"),
            io:format("  Checks performed: ~p~n", [Checks]);
        {error, #{valid := false, violations := Violations}} ->
            io:format("✗ VALIDATION FAILED~n"),
            lists:foreach(fun(#{check := Check, reason := Reason}) ->
                io:format("  - ~p: ~p~n", [Check, Reason])
            end, Violations)
    end,

    ok.

%% @doc Example 7: Continuous monitoring loop
example_monitoring_loop() ->
    io:format("=== Starting Continuous Monitoring (10 iterations) ===~n"),

    monitor_loop(10).

monitor_loop(0) ->
    io:format("Monitoring complete.~n"),
    ok;
monitor_loop(N) ->
    %% Get current health score
    Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),

    %% Get problem count
    Problems = erlmcp_supervisor_utils:find_unhealthy_processes(erlmcp_sup),
    ProblemCount = length(Problems),

    %% Display
    Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    io:format("[~s] Health: ~.2f, Problems: ~p~n",
              [Timestamp, Score, ProblemCount]),

    %% Alert if critical
    if
        Score < 0.5 ->
            io:format("  ⚠️  CRITICAL: Health score below 0.5!~n");
        ProblemCount > 0 ->
            io:format("  ⚠️  WARNING: ~p unhealthy processes detected~n",
                      [ProblemCount]);
        true ->
            ok
    end,

    %% Wait 5 seconds
    timer:sleep(5000),

    monitor_loop(N - 1).

%%%===================================================================
%%% Production Integration Examples
%%%===================================================================

%% @doc Example: Integration with erlmcp_health_monitor
-spec register_with_health_monitor() -> ok.
register_with_health_monitor() ->
    %% Register health check function with erlmcp_health_monitor
    HealthCheckFun = fun() ->
        Score = erlmcp_supervisor_utils:calculate_health_score(erlmcp_sup),
        if
            Score >= 0.9 -> healthy;
            Score >= 0.7 -> degraded;
            Score >= 0.5 -> {degraded, {score, Score}};
            true -> {unhealthy, {score, Score}}
        end
    end,

    erlmcp_health_monitor:register_component(
        erlmcp_supervision_tree,
        self(),
        HealthCheckFun
    ),

    ok.

%% @doc Example: Export metrics to Prometheus format
-spec export_to_prometheus() -> iolist().
export_to_prometheus() ->
    Metrics = erlmcp_supervisor_utils:get_tree_metrics(erlmcp_sup),

    [
        "# HELP erlmcp_supervisors Total number of supervisors\n",
        "# TYPE erlmcp_supervisors gauge\n",
        io_lib:format("erlmcp_supervisors ~p~n", [maps:get(total_supervisors, Metrics)]),

        "# HELP erlmcp_workers Total number of workers\n",
        "# TYPE erlmcp_workers gauge\n",
        io_lib:format("erlmcp_workers ~p~n", [maps:get(total_workers, Metrics)]),

        "# HELP erlmcp_health_score Supervision tree health score\n",
        "# TYPE erlmcp_health_score gauge\n",
        io_lib:format("erlmcp_health_score ~.2f~n", [maps:get(health_score, Metrics)]),

        "# HELP erlmcp_unhealthy_processes Number of unhealthy processes\n",
        "# TYPE erlmcp_unhealthy_processes gauge\n",
        io_lib:format("erlmcp_unhealthy_processes ~p~n", [maps:get(unhealthy_count, Metrics)])
    ].

%% @doc Example: Alert on supervision tree degradation
-spec check_and_alert() -> ok.
check_and_alert() ->
    case erlmcp_supervisor_utils:validate_supervision_tree(erlmcp_sup) of
        {ok, _} ->
            ok;
        {error, #{violations := Violations}} ->
            %% Send alert via erlmcp_health_monitor
            Message = io_lib:format("Supervision tree validation failed: ~p", [Violations]),
            erlmcp_health_monitor:report_degradation(erlmcp_supervision_tree),
            logger:error("~s", [Message]),
            ok
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Pretty-print supervision tree to console
-spec pretty_print_tree(atom()) -> ok.
pretty_print_tree(SupRef) ->
    Tree = erlmcp_supervisor_utils:get_supervision_tree(SupRef),
    pretty_print_tree(Tree, 0).

pretty_print_tree(#{supervisor := Sup, children := Children, health_score := Score}, Indent) ->
    Prefix = lists:duplicate(Indent * 2, $ ),
    io:format("~s└─ ~p (health: ~.2f)~n", [Prefix, Sup, Score]),

    lists:foreach(fun(Child) ->
        #{id := Id, type := Type, tree := SubTree} = Child,
        ChildPrefix = lists:duplicate((Indent + 1) * 2, $ ),
        io:format("~s├─ ~p (~p)~n", [ChildPrefix, Id, Type]),

        case SubTree of
            undefined -> ok;
            Tree when is_map(Tree) ->
                pretty_print_tree(Tree, Indent + 2)
        end
    end, Children).
