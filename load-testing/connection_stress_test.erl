%% @doc Connection Stress Testing for erlmcp v3
%% Implements horizontal scaling validation and connection stress testing
%%
%% Features:
%% - Maximum connection capacity testing
%% - Connection pool management
%% - Horizontal scaling validation
%% - Connection leak detection
%% - Performance under high connection load

-module(erlmcp_connection_stress_test).
-author("erlmcp-load-test-team").
-behaviour(gen_server).

%% API exports
-export([start_link/1, run_stress_test/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include
-include_lib("kernel/include/logger.hrl").
-include_lib("stdlib/include/assert.hrl").

%% Records
-record(connection_stats, {
    total_connections :: integer(),
    active_connections :: integer(),
    failed_connections :: integer(),
    avg_connection_time :: float(),
    max_connection_time :: integer(),
    connection_errors :: map()
}).

-record(stress_test_config, {
    max_connections :: integer(),
    connection_rate :: integer(), % connections per second
    test_duration :: integer(), % milliseconds
    endpoints :: list(),
    auth_config :: map(),
    retry_policy :: {integer(), integer()}, % max_retries, backoff_ms
    monitoring_interval :: integer()
}).

-record(stress_test_state, {
    config :: #stress_test_config{},
    stats :: #connection_stats{},
    start_time :: integer(),
    end_time :: integer(),
    active_connections :: list(),
    monitors :: list(),
    test_results :: map()
}).

%% ============================================================================
%% API FUNCTIONS
%% ============================================================================

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

run_stress_test(TestId, Config) ->
    gen_server:call(?MODULE, {run_stress_test, TestId, Config}, infinity).

stop() ->
    gen_server:call(?MODULE, stop).

%% ============================================================================
%% GEN_SERVER CALLBACKS
%% ============================================================================

init(Config) ->
    process_flag(trap_exit, true),
    ?LOG_INFO("Connection stress test starting with config: ~p", [Config]),

    %% Initialize state
    State = #stress_test_state{
        config = Config,
        stats = #connection_stats{
            total_connections = 0,
            active_connections = 0,
            failed_connections = 0,
            avg_connection_time = 0.0,
            max_connection_time = 0,
            connection_errors = maps:new()
        },
        start_time = erlang:system_time(millisecond),
        active_connections = [],
        monitors = [],
        test_results = maps:new()
    },

    %% Start monitoring
    erlang:send_after(Config#monitoring_interval, self(), collect_metrics),

    {ok, State}.

handle_call({run_stress_test, TestId, TestConfig}, _From, State) ->
    %% Validate configuration
    case validate_test_config(TestConfig) of
        {ok, ValidConfig} ->
            %% Start stress test
            NewState = start_stress_test(TestId, ValidConfig, State),
            {reply, {ok, test_started}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_stats, _From, State) ->
    {reply, {ok, State#stats}, State};

handle_call(get_test_status, _From, State) ->
    Status = determine_test_status(State),
    {reply, {ok, Status}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State) ->
    %% Collect and log metrics
    Metrics = collect_connection_metrics(State),
    ?LOG_INFO("Stress test metrics: ~p", [Metrics]),

    %% Check if test should continue
    case should_continue_test(State) of
        true ->
            %% Schedule next metrics collection
            NextInterval = State#config#monitoring_interval,
            erlang:send_after(NextInterval, self(), collect_metrics),
            {noreply, State};
        false ->
            %% Test completed
            FinalState = complete_test(State),
            {stop, normal, FinalState}
    end;

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    %% Handle connection process death
    handle_connection_death(Pid, Reason, State);

handle_info({connection_created, Pid, ConnectionTime}, State) ->
    %% Track new connection
    NewState = track_new_connection(Pid, ConnectionTime, State),
    {noreply, NewState};

handle_info({connection_failed, Pid, Reason, ConnectionTime}, State) ->
    %% Track failed connection
    NewState = track_failed_connection(Pid, Reason, ConnectionTime, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup resources
    cleanup_connections(State),
    ?LOG_INFO("Connection stress test terminated", []),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ============================================================================
%% PRIVATE FUNCTIONS
%% ============================================================================

validate_test_config(Config) ->
    %% Validate stress test configuration
    RequiredFields = [max_connections, connection_rate, test_duration, endpoints],

    case lists:all(fun(Field) -> maps:is_key(Field, Config) end, RequiredFields) of
        true ->
            %% Additional validation
            case Config#max_connections > 0 andalso Config#connection_rate > 0 of
                true ->
                    {ok, Config};
                false ->
                    {error, invalid_connection_parameters}
            end;
        false ->
            {error, missing_required_fields}
    end.

start_stress_test(TestId, Config, State) ->
    ?LOG_INFO("Starting stress test: ~p with ~p connections", [TestId, Config#max_connections]),

    %% Initialize test
    NewState = State#stress_test_state{
        config = Config,
        start_time = erlang:system_time(millisecond),
        test_results = #{
            test_id => TestId,
            start_time => erlang:system_time(millisecond),
            status => running,
            phases => []
        }
    },

    %% Start connections gradually
    start_connections_gracefully(Config#connection_rate, NewState).

start_connections_gracefully(ConnectionsPerSecond, State) ->
    %% Calculate connection interval
    Interval = 1000 div ConnectionsPerSecond,

    %% Start initial batch
    State1 = start_connection_batch(ConnectionsPerSecond, State),

    %% Schedule next batch
    erlang:send_after(Interval, self(), {start_connection_batch, ConnectionsPerSecond}).

start_connection_batch(ConnectionCount, State) ->
    %% Start a batch of connections
    {NewConnections, NewState} = lists:foldl(fun(_, {Acc, StateAcc}) ->
        case create_connection(StateAcc) of
            {ok, Pid} ->
                {[Pid | Acc], StateAcc#stress_test_state{
                    active_connections = [Pid | StateAcc#stress_test_state.active_connections],
                    stats = StateAcc#stress_test_state.stats#connection_stats{
                        total_connections = StateAcc#stress_test_state.stats#connection_stats.total_connections + 1,
                        active_connections = StateAcc#stress_test_state.stats#active_connections + 1
                    }
                }};
            {error, Reason} ->
                ?LOG_WARNING("Failed to create connection: ~p", [Reason]),
                {Acc, StateAcc#stress_test_state{
                    stats = StateAcc#stress_test_state.stats#connection_stats{
                        failed_connections = StateAcc#stress_test_state.stats#failed_connections + 1,
                        connection_errors = maps:put(error, Reason,
                            StateAcc#stress_test_state.stats#connection_errors)
                    }
                }}
        end
    end, {[], State}, lists:seq(1, ConnectionCount)),

    {NewConnections, NewState}.

create_connection(State) ->
    %% Create a new connection to erlmcp server
    Config = State#stress_test_state.config,

    try
        %% Establish connection with authentication
        Connection = erlmcp_transport:connect(Config#endpoints, Config#auth_config),

        %% Create monitoring process for this connection
        MonitorPid = spawn_link(fun() ->
            connection_monitor(Connection, State)
        end),

        %% Start the connection
        erlmcp_transport:start(Connection),

        {ok, MonitorPid}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

connection_monitor(Connection, State) ->
    %% Monitor connection performance
    MonitorRef = erlang:monitor(process, Connection),

    connection_monitor_loop(Connection, MonitorRef, State, 0, 0).

connection_monitor_loop(Connection, MonitorRef, State, RequestCount, ErrorCount) ->
    receive
        {'DOWN', MonitorRef, process, _Pid, Reason} ->
            %% Connection died
            erlang:send_after(0, self(), {connection_failed, self(), Reason,
                erlang:system_time(millisecond)});

        {monitor_metrics, Metrics} ->
            %% Process metrics
            NewState = update_connection_metrics(Metrics, State),
            connection_monitor_loop(Connection, MonitorRef, NewState, RequestCount, ErrorCount);

        {monitor_request, RequestTime} ->
            %% Track request timing
            NewRequestCount = RequestCount + 1,
            connection_monitor_loop(Connection, MonitorRef, State, NewRequestCount, ErrorCount);

        {monitor_error, Error} ->
            %% Track errors
            NewErrorCount = ErrorCount + 1,
            connection_monitor_loop(Connection, MonitorRef, State, RequestCount, NewErrorCount);

        stop ->
            %% Stop monitoring
            ok
    end.

track_new_connection(Pid, ConnectionTime, State) ->
    %% Track a new successful connection
    NewStats = State#stress_test_state.stats#connection_stats{
        active_connections = State#stress_test_state.stats#connection_stats.active_connections + 1,
        avg_connection_time = calculate_avg_connection_time(
            State#stress_test_state.stats#connection_stats.avg_connection_time,
            State#stress_test_state.stats#connection_stats.total_connections,
            ConnectionTime
        ),
        max_connection_time = max(State#stress_test_state.stats#connection_stats.max_connection_time,
                                 ConnectionTime)
    },

    State#stress_test_state{
        stats = NewStats
    }.

track_failed_connection(Pid, Reason, ConnectionTime, State) ->
    %% Track a failed connection
    NewStats = State#stress_test_state.stats#connection_stats{
        failed_connections = State#stress_test_state.stats#failed_connections + 1,
        connection_errors = maps:put(Reason,
            maps:get(Reason, State#stress_test_state.stats#connection_errors, 0) + 1,
            State#stress_test_state.stats#connection_errors)
    },

    State#stress_test_state{
        stats = NewStats
    }.

handle_connection_death(Pid, Reason, State) ->
    %% Handle connection process death
    NewState = case lists:member(Pid, State#stress_test_state.active_connections) of
        true ->
            %% Active connection died
            NewActiveConnections = lists:delete(Pid, State#stress_test_state.active_connections),
            NewStats = State#stress_test_state.stats#connection_stats{
                active_connections = State#stress_test_state.stats#connection_stats.active_connections - 1
            },

            %% Log the death
            ?LOG_INFO("Connection ~p died: ~p", [Pid, Reason]),

            State#stress_test_state{
                active_connections = NewActiveConnections,
                stats = NewStats
            };
        false ->
            %% Monitor died
            NewMonitors = lists:delete(Pid, State#stress_test_state.monitors),
            State#stress_test_state{
                monitors = NewMonitors
            }
    end,

    %% Check if we need to replace dead connections
    case should_replace_connections(State) of
        true ->
            {Replacements, ReplacedState} = start_replacement_connections(
                length(State#stress_test_state.active_connections),
                NewState
            ),
            ?LOG_INFO("Started ~p replacement connections", [Replacements]),
            ReplacedState;
        false ->
            NewState
    end.

start_replacement_connections(ConnectionCount, State) ->
    %% Replace dead connections
    start_connection_batch(ConnectionCount, State).

should_replace_connections(State) ->
    %% Determine if we should replace dead connections
    CurrentActive = State#stress_test_state.stats#connection_stats.active_connections,
    TargetConnections = State#stress_test_state.config#max_connections,

    case State#stress_test_state.config#connection_rate > 0 of
        true ->
            %% Check if we're in the ramp-up phase
            ElapsedTime = erlang:system_time(millisecond) - State#stress_test_state.start_time,
            MaxConnections = min(State#stress_test_state.config#max_connections,
                                floor(TargetConnections * (ElapsedTime /
                                    State#stress_test_state.config#test_duration)));
        false ->
            MaxConnections = TargetConnections
    end,

    CurrentActive < MaxConnections * 0.8. % Replace if below 80% of target

should_continue_test(State) ->
    %% Check if test should continue
    CurrentTime = erlang:system_time(millisecond),
    ElapsedTime = CurrentTime - State#stress_test_state.start_time;
    TestDuration = State#stress_test_state.config#test_duration;

    case ElapsedTime < TestDuration of
        true ->
            %% Check if we still need more connections
            CurrentConnections = State#stress_test_state.stats#active_connections;
            TargetConnections = State#stress_test_state.config#max_connections;
            CurrentConnections < TargetConnections;
        false ->
            false
    end.

collect_connection_metrics(State) ->
    %% Collect connection metrics
    Stats = State#stress_test_state.stats,

    Metrics = #{
        timestamp => erlang:system_time(millisecond),
        total_connections => Stats#connection_stats.total_connections,
        active_connections => Stats#connection_stats.active_connections,
        failed_connections => Stats#connection_stats.failed_connections,
        avg_connection_time => Stats#connection_stats.avg_connection_time,
        max_connection_time => Stats#connection_stats.max_connection_time,
        connection_errors => Stats#connection_stats.connection_errors,
        cpu_usage => get_cpu_usage(),
        memory_usage => get_memory_usage(),
        connection_rate => calculate_connection_rate(State)
    },

    %% Update test results
    UpdatedResults = State#stress_test_state.test_results#{
        metrics => Metrics,
        current_phase => determine_test_phase(State)
    },

    State#stress_test_state{
        test_results = UpdatedResults
    }.

calculate_connection_rate(State) ->
    %% Calculate current connection establishment rate
    Stats = State#stress_test_state.stats;
    TotalConnections = Stats#connection_stats.total_connections;
    ElapsedTime = erlang:system_time(millisecond) - State#stress_test_state.start_time;

    case ElapsedTime > 0 of
        true ->
            (TotalConnections * 1000) / ElapsedTime;
        false ->
            0
    end.

calculate_avg_connection_time(CurrentAvg, CurrentCount, NewTime) ->
    %% Calculate new average connection time
    case CurrentCount of
        0 ->
            NewTime;
        _ ->
            (CurrentAvg * CurrentCount + NewTime) / (CurrentCount + 1)
    end.

determine_test_phase(State) ->
    %% Determine current test phase
    ElapsedTime = erlang:system_time(millisecond) - State#stress_test_state.start_time;
    TestDuration = State#stress_test_state.config#test_duration;

    case ElapsedTime / TestDuration of
        Ratio when Ratio < 0.1 ->
            ramp_up;
        Ratio when Ratio < 0.5 ->
            steady_state;
        Ratio when Ratio < 0.9 ->
                peak_load;
        _ ->
                cool_down
    end.

determine_test_status(State) ->
    %% Determine overall test status
    case State#stress_test_state.test_results#status of
        running ->
            #{
                status => running,
                phase => determine_test_phase(State),
                connections => State#stress_test_state.stats#connection_stats.active_connections,
                target => State#stress_test_state.config#max_connections,
                progress => (erlang:system_time(millisecond) - State#stress_test_state.start_time) /
                          State#stress_test_state.config#test_duration * 100
            };
        completed ->
            State#stress_test_state.test_results#status;
        failed ->
            State#stress_test_state.test_results#status
    end.

complete_test(State) ->
    %% Complete the stress test
    EndTime = erlang:system_time(millisecond);
    ElapsedTime = EndTime - State#stress_test_state.start_time;

    %% Generate final report
    FinalResults = State#stress_test_state.test_results#{
        end_time => EndTime,
        duration => ElapsedTime,
        final_stats => State#stress_test_state.stats,
        summary => generate_test_summary(State),
        recommendations => generate_test_recommendations(State)
    },

    %% Save results
    ReportFile = "/Users/sac/erlmcp/load-testing/stress_test_" ++
                 integer_to_list(StartTime) ++ "_completed.json",
    file:write_file(ReportFile, jsx:encode(FinalResults)),

    ?LOG_INFO("Stress test completed. Report saved to: ~p", [ReportFile]),

    State#stress_test_state{
        end_time => EndTime,
        test_results = FinalResults#{
            status => completed
        }
    }.

generate_test_summary(State) ->
    %% Generate test summary
    Stats = State#stress_test_state.stats;
    ElapsedTime = State#stress_test_state.end_time - State#stress_test_state.start_time;

    #{
        total_connections => Stats#connection_stats.total_connections,
        successful_connections => Stats#connection_stats.active_connections,
        failed_connections => Stats#connection_stats.failed_connections,
        success_rate => calculate_success_rate(Stats),
        avg_connection_time => Stats#connection_stats.avg_connection_time,
        max_connection_time => Stats#connection_stats.max_connection_time,
        errors_by_type => Stats#connection_stats.connection_errors,
        test_duration => ElapsedTime
    }.

generate_test_recommendations(State) ->
    %% Generate test recommendations
    Stats = State#stress_test_state.stats;
    SuccessRate = calculate_success_rate(Stats);

    Recommendations = case SuccessRate of
        Rate when Rate < 0.9 ->
            [#{recommendation => "Review connection handling code", priority => high}];
        Rate when Rate < 0.95 ->
            [#{recommendation => "Monitor for memory leaks", priority => medium}];
        _ ->
            [#{recommendation => "Test with higher connection limits", priority => low}]
    end,

    Recommendations.

calculate_success_rate(Stats) ->
    case Stats#connection_stats.total_connections of
        0 ->
            1.0;
        Total ->
            Successful = Stats#connection_stats.active_connections;
            Successful / Total
    end.

cleanup_connections(State) ->
    %% Cleanup all connections
    lists:foreach(fun(Pid) ->
        erlang:exit(Pid, shutdown)
    end, State#stress_test_state.active_connections),

    lists:foreach(fun(MonitorPid) ->
        erlang:exit(MonitorPid, shutdown)
    end, State#stress_test_state.monitors),

    ok.

update_connection_metrics(Metrics, State) ->
    %% Update connection metrics
    State#stress_test_state{
        test_results = State#stress_test_state.test_results#{
            metrics => Metrics
        }
    }.

get_cpu_usage() ->
    %% Get current CPU usage
    {ok, CpuData} = os:type(),
    case CpuData of
        {unix, linux} ->
            get_linux_cpu_usage();
        {unix, darwin} ->
            get_darwin_cpu_usage();
        _ ->
            0.0
    end.

get_linux_cpu_usage() ->
    %% Linux CPU usage implementation
    case file:read_file("/proc/loadavg") of
        {ok, Content} ->
            %% Parse load average
            [Load1] = lists:sublist(string:split(Content, " ", all), 1),
            case float(Load1) of
                Load when Load > 0 ->
                    min(Load * 100, 100.0);
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_darwin_cpu_usage() ->
    %% macOS CPU usage implementation
    case os:cmd("top -l 1 -n 0 | grep 'CPU usage'") of
        "CPU usage: " ++ Rest ->
            case string:split(Rest, "%", all) of
                [LoadStr | _] ->
                    case float(string:trim(LoadStr)) of
                        Load when Load > 0 ->
                            Load;
                        _ ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

get_memory_usage() ->
    %% Get current memory usage
    case os:type() of
        {unix, linux} ->
            get_linux_memory_usage();
        {unix, darwin} ->
            get_darwin_memory_usage();
        _ ->
            0.0
    end.

get_linux_memory_usage() ->
    %% Linux memory usage implementation
    case file:read_file("/proc/meminfo") of
        {ok, Content} ->
            Lines = string:split(Content, "\n", all),
            Total = extract_mem_value(Lines, "MemTotal:");
            Available = extract_mem_value(Lines, "MemAvailable:");

            case Total of
                0 ->
                    0.0;
                _ ->
                    (Total - Available) / Total * 100
            end;
        _ ->
            0.0
    end.

get_darwin_memory_usage() ->
    %% macOS memory usage implementation
    case os:cmd("vm_stat | grep 'Pages free'") of
        "Pages free: " ++ FreeStr ->
            Free = list_to_integer(string:trim(FreeStr)),
            case os:cmd("vm_stat | grep 'Pages active'") of
                "Pages active: " ++ ActiveStr ->
                    Active = list_to_integer(string:trim(ActiveStr)),
                    Total = Free + Active + 1024, % Approximate
                    if
                        Total > 0 ->
                            Active / Total * 100;
                        true ->
                            0.0
                    end;
                _ ->
                    0.0
            end;
        _ ->
            0.0
    end.

extract_mem_value(Lines, Key) ->
    %% Extract memory value from /proc/meminfo
    case lists:foldl(fun(Line, Acc) ->
        case string:find(Line, Key) of
            nomatch ->
                Acc;
            _ ->
                case string:split(Line, " ", all) of
                    [_, ValueStr | _] ->
                        list_to_integer(ValueStr) * 1024; % Convert to KB
                    _ ->
                        Acc
                end
        end
    end, 0, Lines) of
        Value when Value > 0 ->
            Value;
        _ ->
            0
    end.