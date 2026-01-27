%%%-----------------------------------------------------------------------------
%%% @doc Health check endpoint for erlmcp v0.6.0 + TCPS
%%% @author erlmcp team
%%% @copyright 2026
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_health).

-export([
    check/0,
    readiness/0,
    liveness/0,
    startup/0,
    detailed_check/0
]).

-type health_status() :: healthy | unhealthy | degraded.
-type check_result() :: {ok, #{status => health_status(), checks => list()}} |
                        {error, #{status => health_status(), checks => list()}}.

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Comprehensive health check (all subsystems)
-spec check() -> check_result().
check() ->
    Checks = [
        check_gproc(),
        check_poolboy(),
        check_tcps(),
        check_disk_space(),
        check_memory(),
        check_database(),
        check_transport()
    ],
    aggregate_results(Checks).

%% @doc Readiness probe - is the system ready to accept traffic?
-spec readiness() -> check_result().
readiness() ->
    Checks = [
        check_gproc(),
        check_database(),
        check_transport()
    ],
    aggregate_results(Checks).

%% @doc Liveness probe - is the system alive?
-spec liveness() -> check_result().
liveness() ->
    Checks = [
        check_vm(),
        check_memory()
    ],
    aggregate_results(Checks).

%% @doc Startup probe - has the system finished starting?
-spec startup() -> check_result().
startup() ->
    Checks = [
        check_application_started(),
        check_supervisors(),
        check_gproc()
    ],
    aggregate_results(Checks).

%% @doc Detailed health check with metrics
-spec detailed_check() -> map().
detailed_check() ->
    {Status, Checks} = case check() of
        {ok, #{checks := C}} -> {healthy, C};
        {error, #{checks := C}} -> {unhealthy, C}
    end,

    #{
        status => Status,
        timestamp => erlang:system_time(millisecond),
        version => "0.6.0",
        checks => Checks,
        metrics => get_metrics(),
        system_info => get_system_info()
    }.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Check if gproc is running
check_gproc() ->
    try
        case whereis(gproc) of
            undefined ->
                {error, #{
                    name => gproc,
                    status => unhealthy,
                    message => <<"gproc not running">>
                }};
            Pid when is_pid(Pid) ->
                case erlang:is_process_alive(Pid) of
                    true ->
                        {ok, #{
                            name => gproc,
                            status => healthy,
                            message => <<"gproc running">>
                        }};
                    false ->
                        {error, #{
                            name => gproc,
                            status => unhealthy,
                            message => <<"gproc process dead">>
                        }}
                end
        end
    catch
        _:Error ->
            {error, #{
                name => gproc,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("gproc check failed: ~p", [Error]))
            }}
    end.

%% @private Check poolboy status
check_poolboy() ->
    try
        % Check if poolboy application is running
        case application:get_application(poolboy) of
            {ok, poolboy} ->
                {ok, #{
                    name => poolboy,
                    status => healthy,
                    message => <<"poolboy running">>
                }};
            undefined ->
                {error, #{
                    name => poolboy,
                    status => unhealthy,
                    message => <<"poolboy not loaded">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => poolboy,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("poolboy check failed: ~p", [Error]))
            }}
    end.

%% @private Check TCPS subsystem
check_tcps() ->
    try
        % Check if TCPS modules are loaded
        case code:is_loaded(tcps_coordinator) of
            {file, _} ->
                {ok, #{
                    name => tcps,
                    status => healthy,
                    message => <<"TCPS coordinator loaded">>
                }};
            false ->
                {ok, #{
                    name => tcps,
                    status => degraded,
                    message => <<"TCPS coordinator not loaded (optional)">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => tcps,
                status => degraded,
                message => iolist_to_binary(io_lib:format("TCPS check failed: ~p", [Error]))
            }}
    end.

%% @private Check disk space
check_disk_space() ->
    try
        DataDir = application:get_env(erlmcp, data_dir, "/var/lib/erlmcp"),
        case filelib:is_dir(DataDir) of
            true ->
                % Simple check - just ensure directory is accessible
                {ok, #{
                    name => disk_space,
                    status => healthy,
                    message => <<"Disk accessible">>
                }};
            false ->
                {ok, #{
                    name => disk_space,
                    status => degraded,
                    message => <<"Data directory not found">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => disk_space,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Disk check failed: ~p", [Error]))
            }}
    end.

%% @private Check memory usage
check_memory() ->
    try
        Memory = erlang:memory(),
        Total = proplists:get_value(total, Memory),
        MaxHeap = case application:get_env(erlmcp, max_heap_size) of
            {ok, Size} -> Size;
            undefined -> 4294967296  % 4GB default
        end,

        UsagePercent = (Total / MaxHeap) * 100,

        if
            UsagePercent < 80 ->
                {ok, #{
                    name => memory,
                    status => healthy,
                    message => <<"Memory usage normal">>,
                    usage_percent => UsagePercent
                }};
            UsagePercent < 90 ->
                {ok, #{
                    name => memory,
                    status => degraded,
                    message => <<"Memory usage high">>,
                    usage_percent => UsagePercent
                }};
            true ->
                {error, #{
                    name => memory,
                    status => unhealthy,
                    message => <<"Memory usage critical">>,
                    usage_percent => UsagePercent
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => memory,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Memory check failed: ~p", [Error]))
            }}
    end.

%% @private Check database connectivity
check_database() ->
    try
        % Check if database connection pool is available
        case application:get_env(erlmcp, tcps) of
            {ok, Config} when is_map(Config) ->
                case maps:get(persistence, Config, #{}) of
                    #{enabled := true} ->
                        % TODO: Implement actual database connectivity check
                        {ok, #{
                            name => database,
                            status => healthy,
                            message => <<"Database connectivity not fully implemented">>
                        }};
                    _ ->
                        {ok, #{
                            name => database,
                            status => healthy,
                            message => <<"Database persistence disabled">>
                        }}
                end;
            _ ->
                {ok, #{
                    name => database,
                    status => healthy,
                    message => <<"Database not configured">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => database,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Database check failed: ~p", [Error]))
            }}
    end.

%% @private Check transport layer
check_transport() ->
    try
        % Check if transport supervisor is running
        case whereis(erlmcp_transport_sup) of
            undefined ->
                {ok, #{
                    name => transport,
                    status => degraded,
                    message => <<"Transport supervisor not running">>
                }};
            Pid when is_pid(Pid) ->
                {ok, #{
                    name => transport,
                    status => healthy,
                    message => <<"Transport layer operational">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => transport,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Transport check failed: ~p", [Error]))
            }}
    end.

%% @private Check if VM is responsive
check_vm() ->
    try
        % Simple liveness check - if we can execute this, VM is alive
        {ok, #{
            name => vm,
            status => healthy,
            message => <<"VM responsive">>
        }}
    catch
        _:Error ->
            {error, #{
                name => vm,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("VM check failed: ~p", [Error]))
            }}
    end.

%% @private Check if application is started
check_application_started() ->
    try
        case application:get_application(erlmcp) of
            {ok, erlmcp} ->
                case application:get_key(erlmcp, vsn) of
                    {ok, Version} ->
                        {ok, #{
                            name => application,
                            status => healthy,
                            message => <<"Application started">>,
                            version => list_to_binary(Version)
                        }};
                    undefined ->
                        {ok, #{
                            name => application,
                            status => healthy,
                            message => <<"Application started (version unknown)">>
                        }}
                end;
            undefined ->
                {error, #{
                    name => application,
                    status => unhealthy,
                    message => <<"Application not started">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => application,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Application check failed: ~p", [Error]))
            }}
    end.

%% @private Check supervisors
check_supervisors() ->
    try
        case whereis(erlmcp_sup) of
            undefined ->
                {error, #{
                    name => supervisors,
                    status => unhealthy,
                    message => <<"Main supervisor not running">>
                }};
            Pid when is_pid(Pid) ->
                {ok, #{
                    name => supervisors,
                    status => healthy,
                    message => <<"Supervisors running">>
                }}
        end
    catch
        _:Error ->
            {error, #{
                name => supervisors,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Supervisor check failed: ~p", [Error]))
            }}
    end.

%% @private Aggregate check results
aggregate_results(Checks) ->
    HealthyCount = length([ok || {ok, _} <- Checks]),
    UnhealthyCount = length([error || {error, _} <- Checks]),
    TotalCount = length(Checks),

    CheckDetails = lists:map(fun
        ({ok, Check}) -> Check;
        ({error, Check}) -> Check
    end, Checks),

    if
        UnhealthyCount == 0 ->
            {ok, #{
                status => healthy,
                checks => CheckDetails,
                summary => #{
                    total => TotalCount,
                    healthy => HealthyCount,
                    unhealthy => UnhealthyCount
                }
            }};
        UnhealthyCount < TotalCount ->
            {error, #{
                status => degraded,
                checks => CheckDetails,
                summary => #{
                    total => TotalCount,
                    healthy => HealthyCount,
                    unhealthy => UnhealthyCount
                }
            }};
        true ->
            {error, #{
                status => unhealthy,
                checks => CheckDetails,
                summary => #{
                    total => TotalCount,
                    healthy => HealthyCount,
                    unhealthy => UnhealthyCount
                }
            }}
    end.

%% @private Get system metrics
get_metrics() ->
    Memory = erlang:memory(),
    #{
        memory_total => proplists:get_value(total, Memory),
        memory_processes => proplists:get_value(processes, Memory),
        memory_system => proplists:get_value(system, Memory),
        memory_atom => proplists:get_value(atom, Memory),
        memory_binary => proplists:get_value(binary, Memory),
        memory_ets => proplists:get_value(ets, Memory),
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        port_count => erlang:system_info(port_count),
        port_limit => erlang:system_info(port_limit),
        run_queue => erlang:statistics(run_queue),
        uptime_ms => erlang:statistics(wall_clock)
    }.

%% @private Get system information
get_system_info() ->
    #{
        otp_release => list_to_binary(erlang:system_info(otp_release)),
        erts_version => list_to_binary(erlang:system_info(version)),
        system_architecture => list_to_binary(erlang:system_info(system_architecture)),
        schedulers => erlang:system_info(schedulers),
        schedulers_online => erlang:system_info(schedulers_online),
        logical_processors => erlang:system_info(logical_processors),
        node_name => atom_to_binary(node(), utf8)
    }.
