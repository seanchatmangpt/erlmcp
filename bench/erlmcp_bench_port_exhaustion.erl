%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_port_exhaustion - Destructive Port Exhaustion Stress Test
%%%
%%% CRITICAL: This is a DESTRUCTIVE test that exhausts OS TCP ports.
%%% DO NOT run on production systems.
%%%
%%% Objective:
%%% - Open TCP ports from 1024 -> 65535 until port exhaustion
%%% - One connection per port, keep all open (no close)
%%% - Find exact OS port limit
%%% - Measure server resilience under port exhaustion
%%%
%%% Test Protocol:
%%% 1. Spawn MCP server on port 10008
%%% 2. Open connections on ports 1024 -> 65535 (client ports)
%%% 3. Monitor: inet:getstat(), available ports, fd usage
%%% 4. Continue until eaddrinuse or OS refuses connection
%%% 5. Document: last successful port, total connections, time to exhaust
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_port_exhaustion).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    run_all/0,
    run_client_exhaustion/0,
    run_server_exhaustion/0,
    run_rapid_reuse_test/0,
    run_so_reuseaddr_test/0,
    generate_report/1
]).

%% Records
-record(port_exhaustion_config, {
    server_port = 10008 :: inet:port_number(),
    start_port = 1024 :: inet:port_number(),
    end_port = 65535 :: inet:port_number(),
    client_bind_ports = false :: boolean(),
    keep_connections = true :: boolean(),
    report_interval = 1000 :: pos_integer(),
    timeout_ms = 30000 :: pos_integer()
}).

-record(port_exhaustion_result, {
    test_id :: binary(),
    test_type :: binary(),
    
    %% Port range
    start_port :: inet:port_number(),
    end_port :: inet:port_number(),
    total_attempted :: non_neg_integer(),
    
    %% Results
    successful_connections :: non_neg_integer(),
    failed_connections :: non_neg_integer(),
    last_successful_port :: inet:port_number() | undefined,
    first_failure_port :: inet:port_number() | undefined,
    
    %% Timing
    duration_s :: float(),
    time_to_exhaust_s :: float() | undefined,
    avg_connection_time_ms :: float(),
    
    %% System limits
    ephemeral_port_range :: {inet:port_number(), inet:port_number()},
    file_descriptor_limit :: non_neg_integer(),
    fds_used_at_limit :: non_neg_integer(),
    
    %% Errors
    final_error :: term(),
    error_breakdown :: map(),
    
    %% Server status
    server_survived :: boolean(),
    server_accepting :: boolean(),
    process_count_at_limit :: non_neg_integer(),
    
    %% System info
    os_type :: {atom(), atom()},
    erlang_version :: string(),
    total_memory_mib :: float(),
    
    %% Metrics
    scope :: binary(),
    timestamp :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all port exhaustion tests
-spec run_all() -> {ok, [map()]} | {error, term()}.
run_all() ->
    logger:warning("=== STARTING DESTRUCTIVE PORT EXHAUSTION TESTS ==="),
    logger:warning("This test will exhaust OS TCP ports"),
    logger:warning("DO NOT run on production systems"),
    
    Results = [
        run_client_exhaustion(),
        run_server_exhaustion(),
        run_rapid_reuse_test(),
        run_so_reuseaddr_test()
    ],
    
    logger:info("=== ALL PORT EXHAUSTION TESTS COMPLETE ==="),
    {ok, Results}.

%% @doc Run client-side port exhaustion test
%% This exhausts the ephemeral port range by opening many client connections
-spec run_client_exhaustion() -> map().
run_client_exhaustion() ->
    logger:warning("=== CLIENT PORT EXHAUSTION TEST ==="),
    logger:warning("Exhausting ephemeral port range (49152-65535 on macOS)"),
    
    Config = #port_exhaustion_config{
        server_port = 10008,
        start_port = 1024,
        end_port = 65535,
        client_bind_ports = false,  % Let OS assign ephemeral ports
        keep_connections = true,
        report_interval = 500,
        timeout_ms = 60000
    },
    
    run_test(<<"client_exhaustion">>, Config).

%% @doc Run server-side port exhaustion test
%% This exhausts server ports by trying to bind to all ports
-spec run_server_exhaustion() -> map().
run_server_exhaustion() ->
    logger:warning("=== SERVER PORT EXHAUSTION TEST ==="),
    logger:warning("Attempting to bind to ports 1024-65535"),
    
    Config = #port_exhaustion_config{
        server_port = 10008,
        start_port = 1024,
        end_port = 65535,
        client_bind_ports = true,  % Explicitly bind to each port
        keep_connections = true,
        report_interval = 1000,
        timeout_ms = 120000
    },
    
    run_test(<<"server_exhaustion">>, Config).

%% @doc Test rapid open/close port reuse
-spec run_rapid_reuse_test() -> map().
run_rapid_reuse_test() ->
    logger:warning("=== RAPID OPEN/CLOSE PORT REUSE TEST ==="),
    
    StartTime = erlang:monotonic_time(millisecond),
    
    % Try to open and close 10000 connections rapidly
    TargetPort = 10008,
    Iterations = 10000,
    
    Results = open_close_rapid(TargetPort, Iterations, 0, 0),
    
    EndTime = erlang:monotonic_time(millisecond),
    DurationS = (EndTime - StartTime) / 1000,
    
    #{ 
        test_type => <<"rapid_open_close">>,
        target_port => TargetPort,
        iterations => Iterations,
        successful_opens => element(1, Results),
        failed_opens => element(2, Results),
        duration_s => DurationS,
        ops_per_second => Iterations / DurationS,
        scope => <<"port_reuse">>,
        timestamp => erlang:system_time(second)
    }.

%% @doc Test SO_REUSEADDR socket option
-spec run_so_reuseaddr_test() -> map().
run_so_reuseaddr_test() ->
    logger:warning("=== SO_REUSEADDR TEST ==="),
    
    Port = 19999,
    
    % Open first socket
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, false}]) of
        {ok, Socket1} ->
            logger:info("First socket bound to port ~p", [Port]),
            
            % Try to bind second socket without SO_REUSEADDR (should fail)
            Result1 = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, false}]),
            
            % Close first socket
            gen_tcp:close(Socket1),
            timer:sleep(100),  % Wait for OS to release port
            
            % Try to bind again (should succeed)
            Result2 = gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, false}]),
            case Result2 of
                {ok, Socket2} ->
                    gen_tcp:close(Socket2),
                    ok;
                _ ->
                    ok
            end,
            
            #{ 
                test_type => <<"so_reuseaddr">>,
                test_port => Port,
                first_bind_success => true,
                second_bind_without_reuse => 
                    case Result1 of
                        {ok, _} -> true;  % Unexpected success
                        {error, _} -> false
                    end,
                second_bind_after_close =>
                    case Result2 of
                        {ok, _} -> true;
                        {error, _} -> false
                    end,
                scope => <<"socket_options">>,
                timestamp => erlang:system_time(second)
            };
        {error, Reason} ->
            logger:error("Failed to bind first socket: ~p", [Reason]),
            #{ 
                test_type => <<"so_reuseaddr">>,
                test_port => Port,
                first_bind_success => false,
                error => Reason,
                scope => <<"socket_options">>,
                timestamp => erlang:system_time(second)
            }
    end.

%% @doc Run port exhaustion test with config
-spec run_test(binary(), #port_exhaustion_config{}) -> map().
run_test(TestType, Config) when is_record(Config, port_exhaustion_config) ->
    logger:info("Starting test: ~p", [TestType]),
    logger:info("Config: ~p", [Config]),
    
    % Start server if needed
    {ok, ServerPid} = start_test_server(Config#port_exhaustion_config.server_port),
    timer:sleep(500),  % Let server initialize
    
    % Get initial system stats
    EphemeralRange = get_ephemeral_port_range(),
    FDLimit = get_fd_limit(),
    
    % Start port exhaustion
    StartTime = erlang:monotonic_time(millisecond),
    StartTimeSec = erlang:system_time(second),
    
    % Create ETS table for tracking connections
    ets:new(port_connections, [named_table, public, set]),
    ets:insert(port_connections, {sockets, []}),
    ets:insert(port_connections, {success_count, 0}),
    ets:insert(port_connections, {fail_count, 0}),
    
    % Run the exhaustion loop
    ExhaustionResult = run_exhaustion_loop(Config),
    
    EndTime = erlang:monotonic_time(millisecond),
    DurationS = (EndTime - StartTime) / 1000,
    
    % Collect final stats
    SuccessCount = ets:lookup_element(port_connections, success_count, 2),
    FailCount = ets:lookup_element(port_connections, fail_count, 2),
    FinalFDs = get_fd_count(),
    
    % Close all sockets
    Sockets = ets:lookup_element(port_connections, sockets, 2),
    close_all_sockets(Sockets),
    ets:delete(port_connections),
    
    % Check server status
    ServerSurvived = is_process_alive(ServerPid),
    ServerAccepting = check_server_accepting(ServerPid, Config#port_exhaustion_config.server_port),
    
    % Get process count
    ProcessCount = erlang:system_info(process_count),
    
    % Build result
    Result = #port_exhaustion_result{
        test_id = <<TestType/binary, "_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        test_type = TestType,
        start_port = Config#port_exhaustion_config.start_port,
        end_port = Config#port_exhaustion_config.end_port,
        total_attempted = SuccessCount + FailCount,
        successful_connections = SuccessCount,
        failed_connections = FailCount,
        last_successful_port = maps:get(last_success_port, ExhaustionResult, undefined),
        first_failure_port = maps:get(first_fail_port, ExhaustionResult, undefined),
        duration_s = DurationS,
        time_to_exhaust_s = maps:get(time_to_exhaust, ExhaustionResult, undefined),
        avg_connection_time_ms = 
            case SuccessCount of
                0 -> 0;
                _ -> DurationS * 1000 / SuccessCount
            end,
        ephemeral_port_range = EphemeralRange,
        file_descriptor_limit = FDLimit,
        fds_used_at_limit = FinalFDs,
        final_error = maps:get(final_error, ExhaustionResult, undefined),
        error_breakdown = maps:get(error_breakdown, ExhaustionResult, #{}),
        server_survived = ServerSurvived,
        server_accepting = ServerAccepting,
        process_count_at_limit = ProcessCount,
        os_type = os:type(),
        erlang_version = erlang:system_info(otp_release) ++ "/" ++ erlang:system_info(version),
        total_memory_mib = erlang:memory(total) / 1024 / 1024,
        scope = <<"port_exhaustion">>,
        timestamp = StartTimeSec
    },
    
    % Stop server
    stop_test_server(ServerPid),
    
    % Generate and display report
    Report = generate_report(Result),
    logger:info("~n~s", [Report]),
    
    % Return as map
    result_to_map(Result).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run the main port exhaustion loop
-spec run_exhaustion_loop(#port_exhaustion_config{}) -> map().
run_exhaustion_loop(Config) ->
    StartPort = Config#port_exhaustion_config.start_port,
    EndPort = Config#port_exhaustion_config.end_port,
    ServerPort = Config#port_exhaustion_config.server_port,
    Timeout = Config#port_exhaustion_config.timeout_ms,
    
    StartTime = erlang:monotonic_time(millisecond),
    
    case Config#port_exhaustion_config.client_bind_ports of
        false ->
            % Client mode: Let OS assign ephemeral ports
            exhaust_ephemeral_ports(ServerPort, Timeout, StartTime);
        true ->
            % Server mode: Explicitly bind to each port
            exhaust_bound_ports(StartPort, EndPort, StartTime)
    end.

%% @doc Exhaust ephemeral ports by opening many client connections
-spec exhaust_ephemeral_ports(inet:port_number(), pos_integer(), integer()) -> map().
exhaust_ephemeral_ports(ServerPort, Timeout, StartTime) ->
    TargetIterations = 70000,  % More than max ports
    exhaust_ephemeral_loop(ServerPort, TargetIterations, 0, Timeout, StartTime).

exhaust_ephemeral_loop(_ServerPort, MaxIterations, Count, Timeout, StartTime) 
  when Count >= MaxIterations ->
    #{
        final_error => max_iterations_reached,
        time_to_exhaust => (erlang:monotonic_time(millisecond) - StartTime) / 1000
    };
exhaust_ephemeral_loop(ServerPort, MaxIterations, Count, Timeout, StartTime) ->
    CurrentTime = erlang:monotonic_time(millisecond),
    Elapsed = CurrentTime - StartTime,
    
    case Elapsed > Timeout of
        true ->
            #{
                final_error => timeout,
                time_to_exhaust => Elapsed / 1000
            };
        false ->
            % Try to open connection
            case gen_tcp:connect("localhost", ServerPort, [
                binary, 
                {active, false},
                {packet, 0}
            ], 1000) of
                {ok, Socket} ->
                    % Store socket
                    {Sockets, Succ} = case ets:lookup(port_connections, sockets) of
                        [{sockets, SockList}] -> {SockList, ets:lookup_element(port_connections, success_count, 2)};
                        _ -> {[], 0}
                    end,
                    ets:insert(port_connections, {sockets, [Socket | Sockets]}),
                    ets:insert(port_connections, {success_count, Succ + 1}),
                    
                    % Report progress
                    case (Succ + 1) rem 1000 of
                        0 ->
                            logger:info("Opened ~p connections...", [Succ + 1]),
                            print_progress(Succ + 1, Elapsed);
                        _ ->
                            ok
                    end,
                    
                    exhaust_ephemeral_loop(ServerPort, MaxIterations, Count + 1, Timeout, StartTime);
                {error, Reason} ->
                    {_, Fail} = case ets:lookup(port_connections, fail_count) of
                        [{fail_count, F}] -> {fail_count, F};
                        _ -> {fail_count, 0}
                    end,
                    ets:insert(port_connections, {fail_count, Fail + 1}),
                    
                    logger:warning("Connection failed at count ~p: ~p", [Count, Reason]),
                    
                    % Update error breakdown
                    OldBreakdown = #{},
                    NewBreakdown = maps:update_with(Reason, fun(V) -> V + 1 end, 1, OldBreakdown),
                    
                    % Check if we've hit exhaustion
                    case is_exhaustion_error(Reason) of
                        true ->
                            logger:error("Port exhaustion detected at ~p connections: ~p", [Count, Reason]),
                            #{
                                failed => Fail + 1,
                                first_fail_port => Count,
                                final_error => Reason,
                                error_breakdown => NewBreakdown,
                                time_to_exhaust => Elapsed / 1000
                            };
                        false ->
                            exhaust_ephemeral_loop(ServerPort, MaxIterations, Count + 1, Timeout, StartTime)
                    end
            end
    end.

%% @doc Exhaust ports by explicitly binding to each port
-spec exhaust_bound_ports(inet:port_number(), inet:port_number(), integer()) -> map().
exhaust_bound_ports(StartPort, EndPort, StartTime) ->
    exhaust_bound_loop(StartPort, EndPort, StartTime).

exhaust_bound_loop(CurrentPort, EndPort, StartTime) when CurrentPort > EndPort ->
    #{
        final_error => all_ports_exhausted,
        time_to_exhaust => (erlang:monotonic_time(millisecond) - StartTime) / 1000
    };
exhaust_bound_loop(CurrentPort, EndPort, StartTime) ->
    % Try to bind to port
    case gen_tcp:listen(CurrentPort, [binary, {active, false}, {reuseaddr, false}]) of
        {ok, Socket} ->
            {Sockets, Succ} = case ets:lookup(port_connections, sockets) of
                [{sockets, SockList}] -> {SockList, ets:lookup_element(port_connections, success_count, 2)};
                _ -> {[], 0}
            end,
            ets:insert(port_connections, {sockets, [Socket | Sockets]}),
            ets:insert(port_connections, {success_count, Succ + 1}),
            
            case Succ rem 1000 of
                0 ->
                    Elapsed = (erlang:monotonic_time(millisecond) - StartTime) / 1000,
                    logger:info("Bound ~p ports (current: ~p)...", [Succ + 1, CurrentPort]),
                    print_progress(Succ + 1, Elapsed * 1000);
                _ ->
                    ok
            end,
            
            exhaust_bound_loop(CurrentPort + 1, EndPort, StartTime);
        {error, Reason} ->
            {_, Fail} = case ets:lookup(port_connections, fail_count) of
                [{fail_count, F}] -> {fail_count, F};
                _ -> {fail_count, 0}
            end,
            ets:insert(port_connections, {fail_count, Fail + 1}),
            
            logger:warning("Bind failed on port ~p: ~p", [CurrentPort, Reason]),
            
            OldBreakdown = #{},
            NewBreakdown = maps:update_with(Reason, fun(V) -> V + 1 end, 1, OldBreakdown),
            
            Elapsed = (erlang:monotonic_time(millisecond) - StartTime) / 1000,
            
            #{
                failed => Fail + 1,
                first_fail_port => CurrentPort,
                final_error => Reason,
                error_breakdown => NewBreakdown,
                time_to_exhaust => Elapsed
            }
    end.

%% @doc Rapid open/close test
-spec open_close_rapid(inet:port_number(), non_neg_integer(), non_neg_integer(), non_neg_integer()) -> 
    {non_neg_integer(), non_neg_integer()}.
open_close_rapid(_Port, 0, Success, Fail) ->
    {Success, Fail};
open_close_rapid(Port, Iterations, Success, Fail) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}], 100) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            open_close_rapid(Port, Iterations - 1, Success + 1, Fail);
        {error, _Reason} ->
            open_close_rapid(Port, Iterations - 1, Success, Fail + 1)
    end.

%% @doc Check if error indicates port exhaustion
-spec is_exhaustion_error(term()) -> boolean().
is_exhaustion_error(eaddrnotavail) -> true;
is_exhaustion_error(eaddrinuse) -> true;
is_exhaustion_error(emfile) -> true;  % Too many open files
is_exhaustion_error(enfile) -> true;  % File table overflow
is_exhaustion_error(_) -> false.

%% @doc Start test server
-spec start_test_server(inet:port_number()) -> {ok, pid()}.
start_test_server(Port) ->
    % Simple TCP echo server for testing
    spawn(fun() -> test_server_loop(Port) end),
    timer:sleep(100),
    {ok, self()}.

%% @doc Stop test server
-spec stop_test_server(pid()) -> ok.
stop_test_server(_Pid) ->
    ok.

%% @doc Test server loop
-spec test_server_loop(inet:port_number()) -> no_return().
test_server_loop(Port) ->
    case gen_tcp:listen(Port, [binary, {active, false}, {reuseaddr, true}, {packet, 0}]) of
        {ok, ListenSocket} ->
            logger:info("Test server listening on port ~p", [Port]),
            accept_loop(ListenSocket);
        {error, Reason} ->
            logger:error("Test server failed to listen: ~p", [Reason]),
            exit({listen_failed, Reason})
    end.

%% @doc Accept connections loop
-spec accept_loop(gen_tcp:socket()) -> no_return().
accept_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            spawn(fun() -> handle_client(Socket) end),
            accept_loop(ListenSocket);
        {error, timeout} ->
            accept_loop(ListenSocket);
        {error, Reason} ->
            logger:error("Accept failed: ~p", [Reason]),
            accept_loop(ListenSocket)
    end.

%% @doc Handle client connection
-spec handle_client(gen_tcp:socket()) -> ok.
handle_client(Socket) ->
    receive
        {tcp, Socket, Data} ->
            gen_tcp:send(Socket, Data),
            handle_client(Socket);
        {tcp_closed, Socket} ->
            ok
    after 30000 ->
        gen_tcp:close(Socket)
    end.

%% @doc Check if server is accepting connections
-spec check_server_accepting(pid(), inet:port_number()) -> boolean().
check_server_accepting(_Pid, Port) ->
    case gen_tcp:connect("localhost", Port, [binary, {active, false}], 1000) of
        {ok, Socket} ->
            gen_tcp:close(Socket),
            true;
        {error, _} ->
            false
    end.

%% @doc Close all sockets in list
-spec close_all_sockets([gen_tcp:socket()]) -> ok.
close_all_sockets([]) ->
    ok;
close_all_sockets([Socket | Rest]) ->
    catch gen_tcp:close(Socket),
    close_all_sockets(Rest).

%% @doc Get ephemeral port range
-spec get_ephemeral_port_range() -> {inet:port_number(), inet:port_number()}.
get_ephemeral_port_range() ->
    case os:type() of
        {unix, darwin} ->
            % macOS
            case catch begin
                First = list_to_integer(os:cmd("sysctl -n net.inet.ip.portrange.first") -- "\n"),
                Last = list_to_integer(os:cmd("sysctl -n net.inet.ip.portrange.last") -- "\n"),
                {First, Last}
            end of
                {'EXIT', _} -> {49152, 65535};  % Default macOS range
                Range -> Range
            end;
        {unix, linux} ->
            % Linux: read from /proc/sys/net/ipv4/ip_local_port_range
            case file:read_file("/proc/sys/net/ipv4/ip_local_port_range") of
                {ok, Data} ->
                    [FirstStr, LastStr] = string:split(
                        string:trim(binary_to_list(Data)), " ", trailing),
                    {list_to_integer(FirstStr), list_to_integer(LastStr)};
                {error, _} ->
                    {32768, 60999}  % Default Linux range
            end;
        _ ->
            {49152, 65535}  % Common default
    end.

%% @doc Get file descriptor limit
-spec get_fd_limit() -> non_neg_integer().
get_fd_limit() ->
    case os:type() of
        {unix, _} ->
            case catch begin
                list_to_integer(os:cmd("ulimit -n") -- "\n")
            end of
                {'EXIT', _} -> 1024;  % Default
                Limit when is_integer(Limit) -> Limit;
                _ -> 1024
            end;
        _ ->
            1024
    end.

%% @doc Get current file descriptor count
-spec get_fd_count() -> non_neg_integer().
get_fd_count() ->
    case os:type() of
        {unix, darwin} ->
            % macOS: count entries in /dev/fd
            case catch begin
                Length = length(filelib:wildcard("*", "/dev/fd")),
                Length - 1  % Subtract 1 for the dir itself
            end of
                {'EXIT', _} -> 0;
                Count -> Count
            end;
        {unix, linux} ->
            % Linux: count entries in /proc/self/fd
            case catch begin
                length(filelib:wildcard("*", "/proc/self/fd"))
            end of
                {'EXIT', _} -> 0;
                Count -> Count
            end;
        _ ->
            0
    end.

%% @doc Print progress
-spec print_progress(non_neg_integer(), integer()) -> ok.
print_progress(Count, ElapsedMs) ->
    Rate = case ElapsedMs of
        0 -> 0;
        _ -> Count * 1000 / ElapsedMs
    end,
    logger:info("  Progress: ~p connections, ~.2f conn/sec", [Count, Rate]).

%% @doc Generate report
-spec generate_report(#port_exhaustion_result{}) -> string().
generate_report(Result) ->
    io_lib:format("
=================================================================
               PORT EXHAUSTION CRASH TEST REPORT
=================================================================

Test ID: ~s
Test Type: ~s
Timestamp: ~p

PORT RANGE:
- Start Port: ~p
- End Port: ~p
- Total Attempted: ~p

OPENING PROGRESS:
- Successful Connections: ~p
- Failed Connections: ~p
- Last Successful Port: ~p
- First Failure Port: ~p

TIMING:
- Total Duration: ~.2f seconds
- Time to Exhaust: ~p
- Avg Connection Time: ~.3f ms

SYSTEM LIMITS:
- Ephemeral Port Range: ~p
- File Descriptor Limit: ~p
- FDs Used at Limit: ~p
- OS Type: ~p
- Erlang Version: ~s
- Total Memory: ~.2f MiB

BREAKING POINT:
- Final Error: ~p
- Error Breakdown: ~p

SERVER STATUS:
- Server Survived: ~p
- Server Accepting: ~p
- Process Count at Limit: ~p

SCOPE: ~s

=================================================================
ANALYSIS:
The system reached port exhaustion when attempting to open ~p connections.
Primary limiting factor: ~p

To mitigate port exhaustion:
1. Use connection pooling to reuse connections
2. Enable SO_REUSEADDR for server sockets (with caution)
3. Increase ephemeral port range (OS-specific)
4. Increase file descriptor limits (ulimit -n)
5. Use multiple IP addresses for more client ports
6. Implement proper connection lifecycle management

=================================================================
",
        [
            Result#port_exhaustion_result.test_id,
            Result#port_exhaustion_result.test_type,
            Result#port_exhaustion_result.timestamp,
            Result#port_exhaustion_result.start_port,
            Result#port_exhaustion_result.end_port,
            Result#port_exhaustion_result.total_attempted,
            Result#port_exhaustion_result.successful_connections,
            Result#port_exhaustion_result.failed_connections,
            Result#port_exhaustion_result.last_successful_port,
            Result#port_exhaustion_result.first_failure_port,
            Result#port_exhaustion_result.duration_s,
            Result#port_exhaustion_result.time_to_exhaust_s,
            Result#port_exhaustion_result.avg_connection_time_ms,
            Result#port_exhaustion_result.ephemeral_port_range,
            Result#port_exhaustion_result.file_descriptor_limit,
            Result#port_exhaustion_result.fds_used_at_limit,
            Result#port_exhaustion_result.os_type,
            Result#port_exhaustion_result.erlang_version,
            Result#port_exhaustion_result.total_memory_mib,
            Result#port_exhaustion_result.final_error,
            Result#port_exhaustion_result.error_breakdown,
            Result#port_exhaustion_result.server_survived,
            Result#port_exhaustion_result.server_accepting,
            Result#port_exhaustion_result.process_count_at_limit,
            Result#port_exhaustion_result.scope,
            Result#port_exhaustion_result.successful_connections,
            Result#port_exhaustion_result.final_error
        ]
    ).

%% @doc Convert result record to map
-spec result_to_map(#port_exhaustion_result{}) -> map().
result_to_map(R) ->
    #{
        test_id => R#port_exhaustion_result.test_id,
        test_type => R#port_exhaustion_result.test_type,
        start_port => R#port_exhaustion_result.start_port,
        end_port => R#port_exhaustion_result.end_port,
        total_attempted => R#port_exhaustion_result.total_attempted,
        successful_connections => R#port_exhaustion_result.successful_connections,
        failed_connections => R#port_exhaustion_result.failed_connections,
        last_successful_port => R#port_exhaustion_result.last_successful_port,
        first_failure_port => R#port_exhaustion_result.first_failure_port,
        duration_s => R#port_exhaustion_result.duration_s,
        time_to_exhaust_s => R#port_exhaustion_result.time_to_exhaust_s,
        avg_connection_time_ms => R#port_exhaustion_result.avg_connection_time_ms,
        ephemeral_port_range => R#port_exhaustion_result.ephemeral_port_range,
        file_descriptor_limit => R#port_exhaustion_result.file_descriptor_limit,
        fds_used_at_limit => R#port_exhaustion_result.fds_used_at_limit,
        final_error => R#port_exhaustion_result.final_error,
        error_breakdown => R#port_exhaustion_result.error_breakdown,
        server_survived => R#port_exhaustion_result.server_survived,
        server_accepting => R#port_exhaustion_result.server_accepting,
        process_count_at_limit => R#port_exhaustion_result.process_count_at_limit,
        os_type => R#port_exhaustion_result.os_type,
        erlang_version => R#port_exhaustion_result.erlang_version,
        total_memory_mib => R#port_exhaustion_result.total_memory_mib,
        scope => R#port_exhaustion_result.scope,
        timestamp => R#port_exhaustion_result.timestamp
    }.
