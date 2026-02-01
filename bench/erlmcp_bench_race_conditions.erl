%%%-------------------------------------------------------------------
%%% @doc erlmcp_bench_race_conditions - Race Condition Bombardment Test
%%%
%%% DESTRUCTIVE TEST #12: Extreme race condition testing to identify
%%% corruption bugs, lost updates, and read-write conflicts in the
%%% erlmcp system under concurrent load.
%%%
%%% Test Protocol:
%%% 1. Spawn MCP server with shared ETS counter resource
%%% 2. Launch 10K→100K concurrent client processes
%%% 3. Each client performs 1K operations (10M→100M total ops)
%%% 4. Mix: 70% increment, 20% read, 10% set
%%% 5. No locking, no transactions - pure race chaos
%%% 6. Measure corruption, lost updates, inconsistent reads
%%%
%%% Validates:
%%% - ETS table integrity under extreme contention
%%% - Lost update anomaly detection
%%% - Read-write conflict patterns
%%% - Data corruption detection
%%% - Process crash recovery
%%% - Atomicity violations
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_race_conditions).

-behaviour(gen_server).

%% API
-export([
    run_all_tests/0,
    run_all_tests/1,
    run_test/1,
    run_test/2,
    tests/0,
    generate_report/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Test worker functions
-export([
    race_worker/3,
    read_write_worker/3,
    mixed_ops_worker/3
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type test_id() :: binary().
-type race_result() :: #{
    workload_id := binary(),
    benchmark := binary(),
    test := binary(),
    concurrent_clients := non_neg_integer(),
    operations_per_client := non_neg_integer(),
    total_operations := non_neg_integer(),
    expected_final_value := non_neg_integer(),
    actual_final_value := non_neg_integer(),
    lost_updates := non_neg_integer(),
    update_success_rate := float(),
    negative_values_read := non_neg_integer(),
    impossible_values := non_neg_integer(),
    inconsistent_reads := non_neg_integer(),
    ets_corruption := boolean(),
    client_crashes := non_neg_integer(),
    server_crashes := non_neg_integer(),
    ets_errors := non_neg_integer(),
    test_duration_s := float(),
    ops_per_second := float(),
    corruption_patterns := [binary()],
    crash_triggers := [binary()],
    test_passed := boolean(),
    scope := binary(),
    timestamp := integer()
}.

-export_type([race_result/0]).

-record(state, {
    ets_table :: ets:tid(),
    clients_started = 0 :: non_neg_integer(),
    clients_completed = 0 :: non_neg_integer(),
    operations_total = 0 :: non_neg_integer(),
    errors_detected = [] :: [term()],
    read_anomalies = [] :: term(),
    start_time :: erlang:timestamp(),
    test_id :: binary()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Run all race condition tests with default config
-spec run_all_tests() -> {ok, [race_result()]} | {error, term()}.
run_all_tests() ->
    run_all_tests(#{}).

%% @doc Run all race condition tests with custom config
-spec run_all_tests(map()) -> {ok, [race_result()]} | {error, term()}.
run_all_tests(Config) ->
    Tests = tests(),
    logger:info("Running ~p race condition tests", [length(Tests)]),

    Results = lists:map(
        fun(#{id := TestId}) ->
            case run_test(TestId, Config) of
                {ok, Result} -> Result;
                {error, Reason} ->
                    logger:error("Test ~p failed: ~p", [TestId, Reason]),
                    create_error_result(TestId, Reason)
            end
        end,
        Tests
    ),

    generate_report(Results),
    {ok, Results}.

%% @doc Run a specific race condition test
-spec run_test(test_id()) -> {ok, race_result()} | {error, term()}.
run_test(TestId) ->
    run_test(TestId, #{}).

%% @doc Run a specific test with custom config
-spec run_test(test_id(), map()) -> {ok, race_result()} | {error, term()}.
run_test(TestId, Config) ->
    logger:info("Starting race condition test: ~s", [TestId]),

    case lists:keyfind(TestId, 1, tests()) of
        {TestId, TestFun, _} ->
            case TestFun(Config) of
                {ok, Result} ->
                    logger:info("Test ~s completed", [TestId]),
                    {ok, Result};
                {error, Reason} ->
                    logger:error("Test ~s failed: ~p", [TestId, Reason]),
                    {error, Reason}
            end;
        false ->
            {error, {unknown_test, TestId}}
    end.

%% @doc List all available race condition tests
-spec tests() -> [{test_id(), function(), map()}].
tests() ->
    [
        {<<"race_basic_10k">>, fun test_race_basic_10k/1, #{
            description => <<"10K clients, 1K ops each, pure race conditions">>,
            severity => high
        }},
        {<<"race_extreme_100k">>, fun test_race_extreme_100k/1, #{
            description => <<"100K clients, 1K ops each, extreme contention">>,
            severity => critical
        }},
        {<<"race_read_write_conflicts">>, fun test_race_read_write_conflicts/1, #{
            description => <<"Read-write conflict bombardment">>,
            severity => high
        }},
        {<<"race_increment_storm">>, fun test_race_increment_storm/1, #{
            description => <<"Pure increment storm (no reads)">>,
            severity => high
        }},
        {<<"race_set_corruption">>, fun test_race_set_corruption/1, #{
            description => <<"Concurrent set operations for corruption">>,
            severity => critical
        }}
    ].

%% @doc Generate comprehensive race condition report
-spec generate_report([race_result()]) -> ok.
generate_report(Results) ->
    ReportFile = filename:join([code:priv_dir(erlmcp), "..", "bench", "results",
                                "race_conditions_report_" ++ integer_to_list(os:system_time(millisecond)) ++ ".json"]),

    Report = #{
        <<"benchmark">> => <<"race_conditions">>,
        <<"timestamp">> => erlang:system_time(second),
        <<"total_tests">> => length(Results),
        <<"passed">> => length([R || R <- Results, maps:get(test_passed, R, false)]),
        <<"failed">> => length([R || R <- Results, not maps:get(test_passed, R, true)]),
        <<"results">> => Results
    },

    Json = jsx:encode(Report, [space, indent]),
    ok = file:write_file(ReportFile, Json),

    logger:info("Race condition report generated: ~s", [ReportFile]),
    print_summary(Results).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([TestId]) ->
    Table = ets:new(race_counter, [public, named_table, {read_concurrency, true}, {write_concurrency, true}]),
    ets:insert(Table, {count, 0}),
    ets:insert(Table, {negative_reads, 0}),
    ets:insert(Table, {impossible_reads, 0}),
    ets:insert(Table, {inconsistent_reads, 0}),

    {ok, #state{
        ets_table = Table,
        start_time = erlang:timestamp(),
        test_id = TestId
    }}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{ets_table = Table}) ->
    catch ets:delete(Table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Test Implementations
%%====================================================================

%% @doc Basic race condition test: 10K clients, 1K ops each
test_race_basic_10k(Config) ->
    run_race_test(<<"race_basic_10k">>, 10000, 1000, #{}, Config).

%% @doc Extreme race condition test: 100K clients, 1K ops each
test_race_extreme_100k(Config) ->
    run_race_test(<<"race_extreme_100k">>, 100000, 1000, #{}, Config).

%% @doc Read-write conflict bombardment
test_race_read_write_conflicts(Config) ->
    run_race_test(<<"race_read_write_conflicts">>, 50000, 500, #{
        read_percent => 40,
        write_percent => 60
    }, Config).

%% @doc Pure increment storm (no reads, maximum contention)
test_race_increment_storm(Config) ->
    run_race_test(<<"race_increment_storm">>, 20000, 2000, #{
        increment_percent => 100,
        read_percent => 0,
        set_percent => 0
    }, Config).

%% @doc Concurrent set operations for corruption testing
test_race_set_corruption(Config) ->
    run_race_test(<<"race_set_corruption">>, 10000, 1000, #{
        increment_percent => 30,
        read_percent => 20,
        set_percent => 50
    }, Config).

%%====================================================================
%% Core Race Test Engine
%%====================================================================

run_race_test(TestId, NumClients, OpsPerClient, MixConfig, Config) ->
    logger:info("Starting race test: ~s with ~p clients, ~p ops each",
                [TestId, NumClients, OpsPerClient]),

    % Start gen_server to manage test
    {ok, Pid} = gen_server:start(?MODULE, [TestId], []),

    % Get operation mix
    IncrementPct = maps:get(increment_percent, MixConfig, 70),
    ReadPct = maps:get(read_percent, MixConfig, 20),
    SetPct = maps:get(set_percent, MixConfig, 10),

    % Spawn all clients concurrently
    StartTime = erlang:monotonic_time(microsecond),

    ClientPids = lists:map(
        fun(I) ->
            spawn_monitor(?MODULE, mixed_ops_worker, [Pid, OpsPerClient,
                {IncrementPct, ReadPct, SetPct}])
        end,
        lists:seq(1, NumClients)
    ),

    % Wait for all clients to complete
    Results = wait_for_clients(ClientPids, []),

    EndTime = erlang:monotonic_time(microsecond),
    DurationUs = EndTime - StartTime,
    DurationS = DurationUs / 1000000,

    % Collect final metrics
    gen_server:stop(Pid),

    % Get final counter value
    FinalCount = case ets:lookup(race_counter, count) of
        [{count, Val}] -> Val;
        _ -> error
    end,

    % Get anomaly counters
    [{_, NegativeReads}] = ets:lookup(race_counter, negative_reads),
    [{_, ImpossibleReads}] = ets:lookup(race_counter, impossible_reads),
    [{_, InconsistentReads}] = ets:lookup(race_counter, inconsistent_reads),

    % Calculate expected value (based on increment operations)
    TotalOps = NumClients * OpsPerClient,
    ExpectedIncrements = round(TotalOps * (IncrementPct / 100)),
    ExpectedValue = ExpectedIncrements,

    % Calculate lost updates
    LostUpdates = case FinalCount of
        error -> ExpectedValue;
        Val when Val >= 0 -> max(0, ExpectedValue - Val);
        _ -> ExpectedValue
    end,

    UpdateSuccessRate = case LostUpdates of
        0 -> 100.0;
        _ -> (ExpectedValue - LostUpdates) / ExpectedValue * 100
    end,

    % Count crashes and errors
    {ClientCrashes, ServerCrashes, EtsErrors} = analyze_results(Results),

    % Detect corruption patterns
    CorruptionPatterns = detect_corruption(Results, FinalCount),

    % Build result
    Result = #{
        workload_id => TestId,
        benchmark => <<"race_conditions">>,
        test => TestId,
        concurrent_clients => NumClients,
        operations_per_client => OpsPerClient,
        total_operations => TotalOps,
        expected_final_value => ExpectedValue,
        actual_final_value => FinalCount,
        lost_updates => LostUpdates,
        update_success_rate => UpdateSuccessRate,
        negative_values_read => NegativeReads,
        impossible_values => ImpossibleReads,
        inconsistent_reads => InconsistentReads,
        ets_corruption => FinalCount =:= error orelse FinalCount < 0,
        client_crashes => ClientCrashes,
        server_crashes => ServerCrashes,
        ets_errors => EtsErrors,
        test_duration_s => DurationS,
        ops_per_second => TotalOps / DurationS,
        corruption_patterns => CorruptionPatterns,
        crash_triggers => [],
        test_passed => LostUpdates =:= 0 andalso FinalCount >= 0,
        scope => <<"race_condition_bombardment">>,
        timestamp => erlang:system_time(second)
    },

    {ok, Result}.

%% @doc Worker process that performs mixed operations
mixed_ops_worker(ServerPid, NumOps, {IncPct, ReadPct, SetPct}) ->
    random:seed(os:timestamp()),

    Results = lists:map(
        fun(_) ->
            Op = random_op(IncPct, ReadPct, SetPct),
            perform_race_op(race_counter, Op)
        end,
        lists:seq(1, NumOps)
    ),

    ServerPid ! {worker_complete, length(Results)},
    exit(normal).

%% @doc Perform a race condition operation
perform_race_op(Table, increment) ->
    try
        % Read current value
        [{count, Current}] = ets:lookup(Table, count),

        % Check for negative values (corruption indicator)
        if Current < 0 ->
            ets:update_counter(Table, negative_reads, {2, 1});
           true -> ok
        end,

        % Increment WITHOUT ATOMICITY (classic lost update bug)
        NewVal = Current + 1,
        ets:insert(Table, {count, NewVal}),
        {ok, increment, NewVal}
    catch
        _:Error -> {error, increment, Error}
    end;

perform_race_op(Table, read) ->
    try
        [{count, Val}] = ets:lookup(Table, count),

        % Detect read anomalies
        if Val < 0 ->
            ets:update_counter(Table, negative_reads, {2, 1});
           true -> ok
        end,

        % Check for impossible values
        if Val > 1000000000 ->
            ets:update_counter(Table, impossible_reads, {2, 1});
           true -> ok
        end,

        {ok, read, Val}
    catch
        _:Error -> {error, read, Error}
    end;

perform_race_op(Table, set) ->
    try
        % Set to random value (chaos operation)
        RandomVal = random:uniform(1000),
        ets:insert(Table, {count, RandomVal}),
        {ok, set, RandomVal}
    catch
        _:Error -> {error, set, Error}
    end.

%% @doc Generate random operation based on percentages
random_op(IncPct, ReadPct, SetPct) ->
    R = random:uniform(100),
    if R =< IncPct -> increment;
       R =< IncPct + ReadPct -> read;
       true -> set
    end.

%% @doc Wait for all clients to complete
wait_for_clients([], Acc) ->
    lists:flatten(Acc);
wait_for_clients([{Pid, Ref} | Rest], Acc) ->
    receive
        {'DOWN', Ref, process, Pid, normal} ->
            wait_for_clients(Rest, Acc);
        {'DOWN', Ref, process, Pid, Reason} ->
            wait_for_clients(Rest, [{crash, Reason} | Acc])
    after 60000 ->
        exit(Pid, kill),
        wait_for_clients(Rest, [{timeout, Pid} | Acc])
    end.

%% @doc Analyze results for crashes and errors
analyze_results(Results) ->
    ClientCrashes = length([R || R <- Results, element(1, R) =:= crash]),
    ServerCrashes = length([R || R <- Results, element(1, R) =:= server_crash]),
    EtsErrors = length([R || R <- Results, element(1, R) =:= ets_error]),
    {ClientCrashes, ServerCrashes, EtsErrors}.

%% @doc Detect corruption patterns
detect_corruption(_Results, FinalValue) ->
    Patterns = [],
    case FinalValue of
        error -> [<<"ETS corruption detected">> | Patterns];
        Val when Val < 0 -> [<<"Negative counter value">> | Patterns];
        _ -> Patterns
    end.

%%====================================================================
%% Alternative Worker Implementations
%%====================================================================

%% @doc Pure race worker (increment only)
race_worker(_Table, 0, _Acc) ->
    ok;
race_worker(Table, N, Acc) ->
    % Non-atomic increment (classic lost update)
    [{count, Current}] = ets:lookup(Table, count),
    NewVal = Current + 1,
    ets:insert(Table, {count, NewVal}),
    race_worker(Table, N - 1, NewVal).

%% @doc Read-write worker (50/50 mix)
read_write_worker(Table, N, _Acc) ->
    read_write_worker_loop(Table, N).

read_write_worker_loop(_Table, 0) ->
    ok;
read_write_worker_loop(Table, N) ->
    case random:uniform(2) of
        1 ->
            % Read
            [{count, _}] = ets:lookup(Table, count);
        2 ->
            % Write (NON-IDEMPOTENT: Intentional race condition for testing)
            % This demonstrates the classic read-modify-write race condition.
            % DO NOT use this pattern in production - use ets:update_counter/3 instead.
            [{count, Current}] = ets:lookup(Table, count),
            ets:insert(Table, {count, Current + 1})  % RACE CONDITION: Not atomic!
    end,
    read_write_worker_loop(Table, N - 1).

%%====================================================================
%% Reporting
%%====================================================================

print_summary(Results) ->
    io:format("~n=== RACE CONDITION BOMBARDMENT RESULTS ===~n~n", []),

    lists:foreach(
        fun(Result) ->
            TestId = maps:get(test, Result),
            Clients = maps:get(concurrent_clients, Result),
            Ops = maps:get(total_operations, Result),
            Expected = maps:get(expected_final_value, Result),
            Actual = maps:get(actual_final_value, Result),
            Lost = maps:get(lost_updates, Result),
            NegReads = maps:get(negative_values_read, Result),
            Corruption = maps:get(ets_corruption, Result),

            io:format("Test: ~s~n", [TestId]),
            io:format("  Clients: ~p, Total Ops: ~p~n", [Clients, Ops]),
            io:format("  Expected: ~p, Actual: ~p~n", [Expected, Actual]),
            io:format("  Lost Updates: ~p (~.2f%)~n", [Lost,
                (Lost / Expected * 100)]),
            io:format("  Negative Reads: ~p~n", [NegReads]),
            io:format("  ETS Corruption: ~p~n", [Corruption]),
            io:format("~n", [])
        end,
        Results
    ),

    TotalPassed = length([R || R <- Results, maps:get(test_passed, R, false)]),
    TotalTests = length(Results),
    io:format("Summary: ~p/~p tests passed~n", [TotalPassed, TotalTests]).

%% @doc Create error result for failed tests
create_error_result(TestId, Reason) ->
    #{
        workload_id => TestId,
        benchmark => <<"race_conditions">>,
        test => TestId,
        error => Reason,
        test_passed => false,
        timestamp => erlang:system_time(second)
    }.
