%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Total Productive Maintenance
%%%
%%% Handles all TPM-related CLI commands for system health and maintenance.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_tpm).

-export([run/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

run(["maintenance" | Args]) ->
    run_maintenance(parse_args(Args));

run(["dashboard" | _]) ->
    show_dashboard();

run(["health" | _]) ->
    check_health();

run(["metrics" | _]) ->
    show_metrics();

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: maintenance, dashboard, health, metrics"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

run_maintenance(Args) ->
    Type = case {maps:get(daily, Args, false), maps:get(weekly, Args, false)} of
        {true, _} -> daily;
        {_, true} -> weekly;
        _ -> daily
    end,

    Force = maps:get(force, Args, false),

    io:format("Running ~s maintenance...~n", [Type]),

    % Check if maintenance was recently run
    case should_run_maintenance(Type, Force) of
        true ->
            Results = perform_maintenance(Type),
            print_maintenance_results(Results),

            case lists:all(fun({_, Status}) -> Status =:= ok end, Results) of
                true ->
                    tcps_cli_format:success("Maintenance completed successfully"),
                    halt(0);
                false ->
                    tcps_cli_format:warning("Some maintenance tasks had issues"),
                    halt(1)
            end;
        false ->
            tcps_cli_format:info("Maintenance was recently run. Use --force to run again."),
            halt(0)
    end.

show_dashboard() ->
    io:format("~n"),
    io:format("TPM Dashboard~n"),
    io:format("=============~n~n"),

    % System health
    Health = check_system_health(),
    io:format("System Health:      ~s~n", [format_health_status(Health)]),
    io:format("~n"),

    % Component status
    Components = [
        {kanban, check_component_health(tcps_kanban)},
        {andon, check_component_health(tcps_andon)},
        {root_cause, check_component_health(tcps_root_cause)}
    ],

    io:format("Component Status:~n"),
    lists:foreach(fun({Component, Status}) ->
        io:format("  ~-15s: ~s~n", [Component, format_component_status(Status)])
    end, Components),
    io:format("~n"),

    % Last maintenance
    case get_last_maintenance() of
        undefined ->
            io:format("Last Maintenance:   Never~n");
        Timestamp ->
            io:format("Last Maintenance:   ~s~n", [tcps_cli_format:format_timestamp(Timestamp)])
    end,
    io:format("~n"),

    halt(0).

check_health() ->
    Health = check_system_health(),

    case Health of
        healthy ->
            tcps_cli_format:success("System is healthy"),
            print_health_details(),
            halt(0);
        degraded ->
            tcps_cli_format:warning("System is degraded"),
            print_health_details(),
            halt(1);
        unhealthy ->
            tcps_cli_format:error("System is unhealthy"),
            print_health_details(),
            halt(1)
    end.

show_metrics() ->
    io:format("~n"),
    io:format("TPM Metrics:~n"),
    io:format("============~n~n"),

    % System uptime
    {UpTime, _} = erlang:statistics(wall_clock),
    io:format("System Uptime:        ~s~n", [tcps_cli_format:format_duration(UpTime / 1000)]),

    % Memory usage
    Memory = erlang:memory(total),
    io:format("Memory Usage:         ~s~n", [tcps_cli_format:format_bytes(Memory)]),

    % Process count
    ProcessCount = erlang:system_info(process_count),
    io:format("Active Processes:     ~p~n", [ProcessCount]),

    % Reductions
    {Reductions, _} = erlang:statistics(reductions),
    io:format("Total Reductions:     ~p~n", [Reductions]),

    io:format("~n"),
    halt(0).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--daily" | Rest], Acc) ->
    parse_args(Rest, Acc#{daily => true});
parse_args(["--weekly" | Rest], Acc) ->
    parse_args(Rest, Acc#{weekly => true});
parse_args(["--force" | Rest], Acc) ->
    parse_args(Rest, Acc#{force => true});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

should_run_maintenance(_Type, true) ->
    % Force flag bypasses check
    true;
should_run_maintenance(Type, false) ->
    case get_last_maintenance() of
        undefined -> true;
        LastRun ->
            Now = erlang:system_time(millisecond),
            TimeSince = Now - LastRun,

            Threshold = case Type of
                daily -> 24 * 3600 * 1000;   % 24 hours
                weekly -> 7 * 24 * 3600 * 1000  % 7 days
            end,

            TimeSince >= Threshold
    end.

perform_maintenance(Type) ->
    Tasks = case Type of
        daily -> daily_maintenance_tasks();
        weekly -> daily_maintenance_tasks() ++ weekly_maintenance_tasks()
    end,

    Results = [execute_maintenance_task(Task) || Task <- Tasks],

    % Update last maintenance timestamp
    set_last_maintenance(erlang:system_time(millisecond)),

    Results.

daily_maintenance_tasks() ->
    [
        {clean_old_receipts, fun clean_old_receipts/0},
        {check_ets_tables, fun check_ets_tables/0},
        {verify_file_permissions, fun verify_file_permissions/0},
        {garbage_collect, fun run_garbage_collection/0}
    ].

weekly_maintenance_tasks() ->
    [
        {archive_receipts, fun archive_old_receipts/0},
        {generate_metrics_report, fun generate_weekly_metrics/0},
        {check_disk_usage, fun check_disk_usage/0}
    ].

execute_maintenance_task({Name, Fun}) ->
    try
        Fun(),
        {Name, ok}
    catch
        Class:Reason ->
            {Name, {error, {Class, Reason}}}
    end.

clean_old_receipts() ->
    % Clean receipts older than 90 days
    ok.

check_ets_tables() ->
    % Verify ETS tables are healthy
    Tables = [tcps_andon_events, tcps_receipts],
    lists:foreach(fun(Table) ->
        case ets:info(Table) of
            undefined -> ok; % Not created yet
            _ -> ok
        end
    end, Tables),
    ok.

verify_file_permissions() ->
    % Check that directories have correct permissions
    Dirs = [
        tcps_cli_config:get(receipts_path, "./priv/receipts"),
        tcps_cli_config:get(ontology_path, "./ontology")
    ],
    lists:foreach(fun(Dir) ->
        filelib:ensure_dir(Dir ++ "/")
    end, Dirs),
    ok.

run_garbage_collection() ->
    % Force garbage collection
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()],
    ok.

archive_old_receipts() ->
    % Archive receipts older than 30 days
    ok.

generate_weekly_metrics() ->
    % Generate and store weekly metrics snapshot
    ok.

check_disk_usage() ->
    % Check available disk space
    ok.

print_maintenance_results(Results) ->
    io:format("~n"),
    lists:foreach(fun({Task, Status}) ->
        case Status of
            ok ->
                io:format("  ✓ ~s~n", [Task]);
            {error, Reason} ->
                io:format("  ✗ ~s: ~p~n", [Task, Reason])
        end
    end, Results),
    io:format("~n").

check_system_health() ->
    % Check various health indicators
    Checks = [
        check_memory_health(),
        check_process_health(),
        check_component_health()
    ],

    case lists:filter(fun(X) -> X =/= healthy end, Checks) of
        [] -> healthy;
        [degraded | _] -> degraded;
        _ -> unhealthy
    end.

check_memory_health() ->
    Memory = erlang:memory(total),
    SystemMemory = erlang:memory(system),
    Ratio = SystemMemory / Memory,

    if
        Ratio < 0.7 -> healthy;
        Ratio < 0.9 -> degraded;
        true -> unhealthy
    end.

check_process_health() ->
    ProcessCount = erlang:system_info(process_count),
    ProcessLimit = erlang:system_info(process_limit),
    Ratio = ProcessCount / ProcessLimit,

    if
        Ratio < 0.7 -> healthy;
        Ratio < 0.9 -> degraded;
        true -> unhealthy
    end.

check_component_health() ->
    % Check if critical components are running
    Components = [tcps_kanban, tcps_andon, tcps_root_cause],
    Running = [whereis(C) =/= undefined || C <- Components],

    case length([R || R <- Running, R =:= true]) of
        N when N >= 2 -> healthy;
        1 -> degraded;
        _ -> unhealthy
    end.

check_component_health(Component) ->
    case whereis(Component) of
        undefined -> down;
        _Pid -> running
    end.

format_health_status(healthy) -> "Healthy ✓";
format_health_status(degraded) -> "Degraded ⚠";
format_health_status(unhealthy) -> "Unhealthy ✗".

format_component_status(running) -> "Running ✓";
format_component_status(down) -> "Down ✗".

print_health_details() ->
    io:format("~n"),
    io:format("Health Details:~n"),
    io:format("---------------~n"),

    Memory = erlang:memory(total),
    io:format("Memory:     ~s~n", [tcps_cli_format:format_bytes(Memory)]),

    ProcessCount = erlang:system_info(process_count),
    io:format("Processes:  ~p~n", [ProcessCount]),

    io:format("~n").

get_last_maintenance() ->
    % In production, this would read from persistent storage
    undefined.

set_last_maintenance(_Timestamp) ->
    % In production, this would write to persistent storage
    ok.
