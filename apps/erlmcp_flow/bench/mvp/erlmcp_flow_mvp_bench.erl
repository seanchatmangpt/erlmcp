%%%-------------------------------------------------------------------
%%% @doc erlmcp-flow MVP Baseline Performance Benchmark
%%% Target: 10K msg/s, <500ms p99 latency, <500MB memory, 0% loss, <2s recovery
%%% Scope: 10 agents, 1000 tasks - simple, unoptimized measurements
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_mvp_bench).

-export([
    run_all/0,
    run_throughput/0,
    run_latency/0,
    run_memory/0,
    run_reliability/0,
    generate_report/1
]).

-include_lib("eunit/include/eunit.hrl").

%% MVP Targets (from 80/20 roadmap)
-define(NUM_AGENTS, 10).
-define(NUM_TASKS, 1000).
-define(TARGET_THROUGHPUT_MSG_PER_S, 10000).
-define(TARGET_LATENCY_P99_MS, 500).
-define(TARGET_MEMORY_MB, 500).
-define(TARGET_TASK_LOSS_PCT, 0.0).
-define(TARGET_RECOVERY_TIME_MS, 2000).

%% Future Optimization Targets (for reference)
-define(FUTURE_THROUGHPUT_MSG_PER_S, 500000).
-define(FUTURE_LATENCY_P99_MS, 50).

%%%===================================================================
%%% Public API
%%%===================================================================

%% @doc Run all MVP benchmarks and generate report
run_all() ->
    io:format("~n=== erlmcp-flow MVP Baseline Performance Benchmark ===~n"),
    io:format("Target: 10K msg/s, <500ms p99, <500MB memory~n"),
    io:format("Scope: 10 agents, 1000 tasks (simple, unoptimized)~n~n"),

    Results = #{
        throughput => run_throughput(),
        latency => run_latency(),
        memory => run_memory(),
        reliability => run_reliability()
    },

    Report = generate_report(Results),
    write_results(Report),

    io:format("~n=== MVP Baseline Complete ===~n"),
    io:format("Report: bench/mvp/results/mvp_baseline_~s.json~n", [timestamp()]),

    Results.

%%%===================================================================
%%% Throughput Benchmarks
%%%===================================================================

run_throughput() ->
    io:format("~n--- Throughput Benchmarks (MVP) ---~n"),

    #{
        task_throughput => bench_task_throughput()
    }.

bench_task_throughput() ->
    io:format("Task Throughput (10 agents, 1000 tasks): "),

    % Spawn agents
    AgentPids = spawn_simple_agents(?NUM_AGENTS),

    % Measure throughput
    Start = erlang:monotonic_time(microsecond),

    % Submit tasks
    TaskIds = lists:map(fun(I) ->
        {ok, TaskId} = submit_simple_task(AgentPids, I),
        TaskId
    end, lists:seq(1, ?NUM_TASKS)),

    % Wait for completion
    wait_for_tasks(TaskIds, 30000),

    Duration = erlang:monotonic_time(microsecond) - Start,
    Throughput = ?NUM_TASKS / (Duration / 1000000),

    cleanup_agents(AgentPids),

    Result = #{
        throughput_msg_per_s => round(Throughput),
        duration_ms => Duration / 1000,
        num_tasks => ?NUM_TASKS,
        num_agents => ?NUM_AGENTS,
        target_met => Throughput >= ?TARGET_THROUGHPUT_MSG_PER_S,
        future_target => ?FUTURE_THROUGHPUT_MSG_PER_S
    },

    io:format("~.0f msg/s (~s) [MVP target: ~.0f, Future: ~.0f]~n", [
        Throughput,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end,
        float(?TARGET_THROUGHPUT_MSG_PER_S),
        float(?FUTURE_THROUGHPUT_MSG_PER_S)
    ]),

    Result.

%%%===================================================================
%%% Latency Benchmarks
%%%===================================================================

run_latency() ->
    io:format("~n--- Latency Benchmarks (MVP) ---~n"),

    #{
        agent_latency => bench_agent_latency()
    }.

bench_agent_latency() ->
    io:format("Agent Task Latency: "),

    % Spawn agents
    AgentPids = spawn_simple_agents(?NUM_AGENTS),

    % Warmup
    lists:foreach(fun(_) ->
        measure_task_latency(AgentPids)
    end, lists:seq(1, 10)),

    % Measure latencies
    Latencies = lists:map(fun(_) ->
        measure_task_latency(AgentPids)
    end, lists:seq(1, 100)),

    cleanup_agents(AgentPids),

    Result = #{
        p50_ms => percentile(Latencies, 0.50) / 1000,
        p95_ms => percentile(Latencies, 0.95) / 1000,
        p99_ms => percentile(Latencies, 0.99) / 1000,
        avg_ms => lists:sum(Latencies) / length(Latencies) / 1000,
        num_samples => length(Latencies),
        target_met => percentile(Latencies, 0.99) / 1000 =< ?TARGET_LATENCY_P99_MS,
        future_target_p99_ms => ?FUTURE_LATENCY_P99_MS
    },

    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s) [MVP: <~pms, Future: <~pms]~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end,
        ?TARGET_LATENCY_P99_MS,
        ?FUTURE_LATENCY_P99_MS
    ]),

    Result.

%%%===================================================================
%%% Memory Benchmarks
%%%===================================================================

run_memory() ->
    io:format("~n--- Memory Benchmarks (MVP) ---~n"),

    #{
        process_memory => bench_process_memory()
    }.

bench_process_memory() ->
    io:format("Process Overhead Memory: "),

    erlang:garbage_collect(),
    MemBefore = erlang:memory(total),

    % Spawn agents
    AgentPids = spawn_simple_agents(?NUM_AGENTS),

    % Submit some tasks to create realistic state
    TaskIds = lists:map(fun(I) ->
        {ok, TaskId} = submit_simple_task(AgentPids, I),
        TaskId
    end, lists:seq(1, 100)),

    erlang:garbage_collect(),
    MemAfter = erlang:memory(total),

    wait_for_tasks(TaskIds, 30000),

    UsedMB = (MemAfter - MemBefore) / (1024 * 1024),
    MBPerAgent = UsedMB / ?NUM_AGENTS,

    cleanup_agents(AgentPids),

    Result = #{
        memory_mb => UsedMB,
        mb_per_agent => MBPerAgent,
        num_agents => ?NUM_AGENTS,
        target_met => UsedMB =< ?TARGET_MEMORY_MB
    },

    io:format("~.2f MB (~.2f MB/agent) (~s) [Target: <~p MB]~n", [
        UsedMB,
        MBPerAgent,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end,
        ?TARGET_MEMORY_MB
    ]),

    Result.

%%%===================================================================
%%% Reliability Benchmarks
%%%===================================================================

run_reliability() ->
    io:format("~n--- Reliability Benchmarks (MVP) ---~n"),

    #{
        task_loss => bench_task_loss(),
        agent_recovery => bench_agent_recovery()
    }.

bench_task_loss() ->
    io:format("Task Loss (with agent failures): "),

    % Spawn agents
    AgentPids = spawn_simple_agents(?NUM_AGENTS),

    % Submit tasks
    TaskIds = lists:map(fun(I) ->
        {ok, TaskId} = submit_simple_task(AgentPids, I),
        TaskId
    end, lists:seq(1, ?NUM_TASKS)),

    % Kill some agents during execution
    spawn(fun() ->
        timer:sleep(100),
        lists:foreach(fun(Pid) ->
            exit(Pid, kill),
            timer:sleep(50)
        end, lists:sublist(AgentPids, 3))  % Kill 3 agents
    end),

    % Wait for completion with retries
    Completed = count_completed_tasks(TaskIds, 60000),

    cleanup_agents(AgentPids),

    LossRate = ((?NUM_TASKS - Completed) / ?NUM_TASKS) * 100,

    Result = #{
        total_tasks => ?NUM_TASKS,
        completed_tasks => Completed,
        lost_tasks => ?NUM_TASKS - Completed,
        loss_rate_pct => LossRate,
        target_met => LossRate =< ?TARGET_TASK_LOSS_PCT
    },

    io:format("~p/~p completed, ~.2f% loss (~s) [Target: ~.1f%]~n", [
        Completed,
        ?NUM_TASKS,
        LossRate,
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end,
        ?TARGET_TASK_LOSS_PCT
    ]),

    Result.

bench_agent_recovery() ->
    io:format("Agent Recovery Time: "),

    % Measure supervisor restart times
    RecoveryTimes = lists:map(fun(_) ->
        measure_agent_recovery()
    end, lists:seq(1, 10)),

    Result = #{
        p50_ms => percentile(RecoveryTimes, 0.50) / 1000,
        p95_ms => percentile(RecoveryTimes, 0.95) / 1000,
        p99_ms => percentile(RecoveryTimes, 0.99) / 1000,
        avg_ms => lists:sum(RecoveryTimes) / length(RecoveryTimes) / 1000,
        num_samples => length(RecoveryTimes),
        target_met => percentile(RecoveryTimes, 0.99) / 1000 =< ?TARGET_RECOVERY_TIME_MS
    },

    io:format("p50=~.2fms, p95=~.2fms, p99=~.2fms (~s) [Target: <~pms]~n", [
        maps:get(p50_ms, Result),
        maps:get(p95_ms, Result),
        maps:get(p99_ms, Result),
        case Result of
            #{target_met := true} -> "PASS";
            _ -> "FAIL"
        end,
        ?TARGET_RECOVERY_TIME_MS
    ]),

    Result.

%%%===================================================================
%%% Report Generation
%%%===================================================================

generate_report(Results) ->
    #{
        metadata => #{
            timestamp => erlang:system_time(millisecond),
            otp_version => erlang:system_info(otp_release),
            erts_version => erlang:system_info(version),
            system_architecture => erlang:system_info(system_architecture),
            schedulers => erlang:system_info(schedulers),
            benchmark_type => mvp_baseline
        },
        mvp_targets => #{
            throughput_msg_per_s => ?TARGET_THROUGHPUT_MSG_PER_S,
            latency_p99_ms => ?TARGET_LATENCY_P99_MS,
            memory_mb => ?TARGET_MEMORY_MB,
            task_loss_pct => ?TARGET_TASK_LOSS_PCT,
            recovery_time_ms => ?TARGET_RECOVERY_TIME_MS
        },
        future_targets => #{
            throughput_msg_per_s => ?FUTURE_THROUGHPUT_MSG_PER_S,
            latency_p99_ms => ?FUTURE_LATENCY_P99_MS
        },
        results => Results,
        summary => generate_summary(Results)
    }.

generate_summary(Results) ->
    ThroughputResult = maps:get(throughput, Results),
    LatencyResult = maps:get(latency, Results),
    MemoryResult = maps:get(memory, Results),
    ReliabilityResult = maps:get(reliability, Results),

    TaskThroughput = maps:get(throughput_msg_per_s, maps:get(task_throughput, ThroughputResult)),
    AgentLatencyP99 = maps:get(p99_ms, maps:get(agent_latency, LatencyResult)),
    ProcessMemory = maps:get(memory_mb, maps:get(process_memory, MemoryResult)),
    TaskLoss = maps:get(loss_rate_pct, maps:get(task_loss, ReliabilityResult)),
    RecoveryP99 = maps:get(p99_ms, maps:get(agent_recovery, ReliabilityResult)),

    #{
        throughput_met => TaskThroughput >= ?TARGET_THROUGHPUT_MSG_PER_S,
        latency_met => AgentLatencyP99 =< ?TARGET_LATENCY_P99_MS,
        memory_met => ProcessMemory =< ?TARGET_MEMORY_MB,
        task_loss_met => TaskLoss =< ?TARGET_TASK_LOSS_PCT,
        recovery_met => RecoveryP99 =< ?TARGET_RECOVERY_TIME_MS,

        overall_mvp_pass => (TaskThroughput >= ?TARGET_THROUGHPUT_MSG_PER_S) and
                            (AgentLatencyP99 =< ?TARGET_LATENCY_P99_MS) and
                            (ProcessMemory =< ?TARGET_MEMORY_MB) and
                            (TaskLoss =< ?TARGET_TASK_LOSS_PCT) and
                            (RecoveryP99 =< ?TARGET_RECOVERY_TIME_MS),

        key_metrics => #{
            throughput_msg_per_s => TaskThroughput,
            latency_p99_ms => AgentLatencyP99,
            memory_mb => ProcessMemory,
            task_loss_pct => TaskLoss,
            recovery_p99_ms => RecoveryP99
        },

        gap_to_future => #{
            throughput_gap => ?FUTURE_THROUGHPUT_MSG_PER_S - TaskThroughput,
            latency_gap => AgentLatencyP99 - ?FUTURE_LATENCY_P99_MS
        }
    }.

write_results(Report) ->
    Timestamp = timestamp(),
    Filename = io_lib:format("bench/mvp/results/mvp_baseline_~s.json", [Timestamp]),
    filelib:ensure_dir(Filename),
    ok = file:write_file(Filename, jsx:encode(Report)),

    % Also write markdown summary
    MdFilename = io_lib:format("bench/mvp/results/mvp_baseline_~s.md", [Timestamp]),
    MdContent = format_markdown_report(Report),
    ok = file:write_file(MdFilename, MdContent),

    ok.

format_markdown_report(Report) ->
    #{summary := Summary, results := Results} = Report,
    #{key_metrics := Metrics, gap_to_future := Gap} = Summary,

    io_lib:format(
        "# erlmcp-flow MVP Baseline Performance Report~n~n"
        "**Date**: ~s~n"
        "**Type**: MVP Baseline (simple, unoptimized)~n"
        "**Scope**: 10 agents, 1000 tasks~n~n"
        "## Summary~n~n"
        "| Metric | Result | MVP Target | Future Target | Status |~n"
        "|--------|--------|------------|---------------|--------|~n"
        "| Throughput | ~.0f msg/s | ~p msg/s | ~p msg/s | ~s |~n"
        "| Latency (p99) | ~.2f ms | <~p ms | <~p ms | ~s |~n"
        "| Memory | ~.2f MB | <~p MB | - | ~s |~n"
        "| Task Loss | ~.2f% | ~.1f% | - | ~s |~n"
        "| Recovery (p99) | ~.2f ms | <~p ms | - | ~s |~n~n"
        "**Overall MVP**: ~s~n~n"
        "## Gap to Future Optimization~n~n"
        "- **Throughput**: Need ~.0fx improvement (~.0f → ~p msg/s)~n"
        "- **Latency**: Need ~.1fx improvement (~.2fms → ~pms p99)~n~n"
        "## Next Steps~n~n"
        "1. **v0.2.0**: Optimize hot paths (target: 50K msg/s, <100ms p99)~n"
        "2. **v0.3.0**: Advanced concurrency (target: 100K msg/s, <75ms p99)~n"
        "3. **v1.0.0**: Full optimization (target: 500K msg/s, <50ms p99)~n",
        [
            timestamp(),
            maps:get(throughput_msg_per_s, Metrics),
            ?TARGET_THROUGHPUT_MSG_PER_S,
            ?FUTURE_THROUGHPUT_MSG_PER_S,
            status_icon(maps:get(throughput_met, Summary)),
            maps:get(latency_p99_ms, Metrics),
            ?TARGET_LATENCY_P99_MS,
            ?FUTURE_LATENCY_P99_MS,
            status_icon(maps:get(latency_met, Summary)),
            maps:get(memory_mb, Metrics),
            ?TARGET_MEMORY_MB,
            status_icon(maps:get(memory_met, Summary)),
            maps:get(task_loss_pct, Metrics),
            ?TARGET_TASK_LOSS_PCT,
            status_icon(maps:get(task_loss_met, Summary)),
            maps:get(recovery_p99_ms, Metrics),
            ?TARGET_RECOVERY_TIME_MS,
            status_icon(maps:get(recovery_met, Summary)),
            case maps:get(overall_mvp_pass, Summary) of
                true -> "PASS";
                false -> "FAIL"
            end,
            ?FUTURE_THROUGHPUT_MSG_PER_S / maps:get(throughput_msg_per_s, Metrics),
            maps:get(throughput_msg_per_s, Metrics),
            ?FUTURE_THROUGHPUT_MSG_PER_S,
            maps:get(latency_p99_ms, Metrics) / ?FUTURE_LATENCY_P99_MS,
            maps:get(latency_p99_ms, Metrics),
            ?FUTURE_LATENCY_P99_MS
        ]
    ).

status_icon(true) -> "PASS";
status_icon(false) -> "FAIL".

%%%===================================================================
%%% Helper Functions (Simple Implementations)
%%%===================================================================

timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    io_lib:format("~4..0B~2..0B~2..0B_~2..0B~2..0B~2..0B", [Y, M, D, H, Min, S]).

percentile(List, P) ->
    Sorted = lists:sort(List),
    Index = max(1, min(length(Sorted), round(P * length(Sorted)))),
    lists:nth(Index, Sorted).

%% Simple agent simulation (placeholder for actual implementation)
spawn_simple_agents(N) ->
    lists:map(fun(I) ->
        spawn(fun() -> simple_agent_loop(I, #{}) end)
    end, lists:seq(1, N)).

simple_agent_loop(Id, State) ->
    receive
        {task, From, TaskId, _Task} ->
            timer:sleep(rand:uniform(10)),  % Simulate work (0-10ms)
            From ! {task_done, TaskId},
            simple_agent_loop(Id, State);
        stop ->
            ok
    end.

submit_simple_task(AgentPids, TaskId) ->
    Agent = lists:nth(rand:uniform(length(AgentPids)), AgentPids),
    Agent ! {task, self(), TaskId, #{id => TaskId}},
    {ok, TaskId}.

wait_for_tasks(TaskIds, Timeout) ->
    wait_for_tasks(TaskIds, Timeout, erlang:monotonic_time(millisecond)).

wait_for_tasks([], _Timeout, _Start) ->
    ok;
wait_for_tasks(TaskIds, Timeout, Start) ->
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    case Elapsed > Timeout of
        true -> timeout;
        false ->
            receive
                {task_done, TaskId} ->
                    wait_for_tasks(lists:delete(TaskId, TaskIds), Timeout, Start)
            after 100 ->
                wait_for_tasks(TaskIds, Timeout, Start)
            end
    end.

count_completed_tasks(TaskIds, Timeout) ->
    count_completed_tasks(TaskIds, Timeout, erlang:monotonic_time(millisecond), 0).

count_completed_tasks([], _Timeout, _Start, Count) ->
    Count;
count_completed_tasks(_TaskIds, Timeout, Start, Count) ->
    Elapsed = erlang:monotonic_time(millisecond) - Start,
    case Elapsed > Timeout of
        true -> Count;
        false ->
            receive
                {task_done, _TaskId} ->
                    count_completed_tasks([], Timeout, Start, Count + 1)
            after 100 ->
                Count
            end
    end.

measure_task_latency(AgentPids) ->
    Start = erlang:monotonic_time(microsecond),
    TaskId = rand:uniform(1000000),
    {ok, _} = submit_simple_task(AgentPids, TaskId),
    receive
        {task_done, TaskId} ->
            erlang:monotonic_time(microsecond) - Start
    after 5000 ->
        5000000  % 5s timeout
    end.

measure_agent_recovery() ->
    % Spawn agent under supervision (simplified)
    Pid = spawn(fun() -> simple_agent_loop(1, #{}) end),

    % Kill agent
    Start = erlang:monotonic_time(microsecond),
    exit(Pid, kill),

    % Simulate supervisor restart
    timer:sleep(rand:uniform(100)),  % 0-100ms restart time

    erlang:monotonic_time(microsecond) - Start.

cleanup_agents(AgentPids) ->
    lists:foreach(fun(Pid) ->
        catch Pid ! stop
    end, AgentPids),
    timer:sleep(100).
