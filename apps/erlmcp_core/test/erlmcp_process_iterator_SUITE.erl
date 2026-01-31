%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 28 Process Iterator Test Suite
%%%
%%% Tests the new process iterator API introduced in OTP 28:
%%% - erlang:processes_iterator/0 - O(1) memory usage
%%% - erlang:process_next/1 - iterator advancement
%%% - Backward compatibility with OTP 25-27
%%%
%%% Chicago School TDD:
%%% - Real process spawning (100+ processes)
%%% - Observable behavior: memory usage, completeness, ordering
%%% - No mocks or fakes
%%% - Graceful fallback for OTP 25-27
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_process_iterator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    test_iterator_api_available/1,
    test_iterator_memory_constant/1,
    test_iterator_completeness/1,
    test_iterator_ordering/1,
    test_process_next_termination/1,
    test_fallback_available_otp27/1,
    test_concurrent_iteration/1
]).

-define(TEST_PROCESS_COUNT, 100).
-define(LARGE_PROCESS_COUNT, 1000).
-define(MEMORY_SAMPLE_SIZE, 10).

%%====================================================================
%% Suite Configuration
%%====================================================================

all() ->
    [
        {group, otp28_features},
        {group, compatibility},
        {group, stress_tests}
    ].

groups() ->
    [
        {otp28_features, [sequence], [
            test_iterator_api_available,
            test_iterator_memory_constant,
            test_iterator_completeness,
            test_iterator_ordering,
            test_process_next_termination
        ]},
        {compatibility, [sequence], [
            test_fallback_available_otp27
        ]},
        {stress_tests, [sequence], [
            test_concurrent_iteration
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting OTP 28 Process Iterator Test Suite"),

    % Detect OTP version
    OTPRelease = erlang:system_info(otp_release),
    OTPVersion = list_to_integer(OTPRelease),

    ct:pal("OTP Release: ~s (Version: ~p)", [OTPRelease, OTPVersion]),

    % Check if iterator API is available (OTP 28+)
    HasIterator = has_process_iterator_api(),
    ct:pal("Process iterator API available: ~p", [HasIterator]),

    [{otp_version, OTPVersion}, {has_iterator, HasIterator} | Config].

end_per_suite(_Config) ->
    ct:pal("Process Iterator Test Suite completed"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),

    % Spawn test processes for each test case
    ProcessCount = ?TEST_PROCESS_COUNT,
    Processes = spawn_test_processes(ProcessCount),

    % Store process PIDs in config
    [{test_processes, Processes} | Config].

end_per_testcase(TestCase, Config) ->
    ct:pal("Ending test case: ~p", [TestCase]),

    % Cleanup: terminate all test processes
    Processes = proplists:get_value(test_processes, Config, []),
    cleanup_processes(Processes),

    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test that process iterator API is available on OTP 28+
test_iterator_api_available(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),
    OTPVersion = proplists:get_value(otp_version, Config),

    ct:pal("Testing iterator API availability (OTP ~p)", [OTPVersion]),

    if
        OTPVersion >= 28 ->
            % OTP 28+: iterator must be available
            ?assert(HasIterator),

            % Verify we can create an iterator
            Iterator = erlang:processes_iterator(),
            ?assert(is_reference(Iterator) orelse Iterator =:= done),

            ct:pal("Iterator created successfully: ~p", [Iterator]);

        true ->
            % OTP 25-27: iterator should not be available
            ?assertNot(HasIterator),
            ct:pal("Iterator API not available (expected for OTP <28)")
    end.

%% @doc Test that iterator uses O(1) memory vs O(N) for processes/0
test_iterator_memory_constant(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),

    case HasIterator of
        false ->
            {skip, "Process iterator not available (OTP <28)"};

        true ->
            ct:pal("Testing iterator memory usage vs processes/0"),

            % Spawn many processes to amplify memory difference
            LargeProcessSet = spawn_test_processes(?LARGE_PROCESS_COUNT),

            try
                % Measure memory for processes/0 (O(N) - creates full list)
                MemBefore1 = erlang:memory(total),
                _AllProcs = erlang:processes(),  % Creates list of all processes
                MemAfter1 = erlang:memory(total),
                ProcessesMemory = MemAfter1 - MemBefore1,

                % Force garbage collection to clear the list
                erlang:garbage_collect(),
                timer:sleep(100),

                % Measure memory for iterator (O(1) - just iterator reference)
                MemBefore2 = erlang:memory(total),
                _Iterator = erlang:processes_iterator(),  % Just creates iterator
                MemAfter2 = erlang:memory(total),
                IteratorMemory = MemAfter2 - MemBefore2,

                ct:pal("Memory usage comparison:"),
                ct:pal("  processes/0: ~p bytes (O(N))", [ProcessesMemory]),
                ct:pal("  iterator: ~p bytes (O(1))", [IteratorMemory]),

                % Iterator should use significantly less memory
                % (processes/0 creates list of PIDs, iterator is just a reference)
                ?assert(IteratorMemory < ProcessesMemory),

                % Iterator memory should be roughly constant (just reference size)
                % Should be < 1KB regardless of process count
                ?assert(IteratorMemory < 1024)

            after
                cleanup_processes(LargeProcessSet)
            end
    end.

%% @doc Test that iterator returns all processes
test_iterator_completeness(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),
    TestProcesses = proplists:get_value(test_processes, Config),

    case HasIterator of
        false ->
            {skip, "Process iterator not available (OTP <28)"};

        true ->
            ct:pal("Testing iterator completeness"),

            % Get all processes using iterator
            IteratedPids = collect_all_processes_via_iterator(),

            % Get all processes using processes/0
            AllPids = erlang:processes(),

            ct:pal("Iterator found ~p processes", [length(IteratedPids)]),
            ct:pal("processes/0 found ~p processes", [length(AllPids)]),

            % Both methods should find same count
            ?assertEqual(length(AllPids), length(IteratedPids)),

            % All test processes should be in the iterated list
            lists:foreach(fun(Pid) ->
                ?assert(lists:member(Pid, IteratedPids))
            end, TestProcesses)
    end.

%% @doc Test that iterator ordering is consistent
test_iterator_ordering(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),

    case HasIterator of
        false ->
            {skip, "Process iterator not available (OTP <28)"};

        true ->
            ct:pal("Testing iterator ordering consistency"),

            % Iterate multiple times
            Iteration1 = collect_all_processes_via_iterator(),
            Iteration2 = collect_all_processes_via_iterator(),

            % Order should be consistent across iterations
            % (if no processes spawn/die between iterations)
            ?assertEqual(Iteration1, Iteration2)
    end.

%% @doc Test that process_next/1 properly terminates with 'done'
test_process_next_termination(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),

    case HasIterator of
        false ->
            {skip, "Process iterator not available (OTP <28)"};

        true ->
            ct:pal("Testing iterator termination"),

            % Create iterator
            Iterator = erlang:processes_iterator(),

            % Iterate until done
            Result = iterate_until_done(Iterator, 0),

            ct:pal("Iterator terminated after ~p processes", [Result]),

            % Should have counted some processes
            ?assert(Result > 0)
    end.

%% @doc Test that fallback to processes/0 works on OTP 25-27
test_fallback_available_otp27(Config) ->
    ct:pal("Testing fallback mechanism"),

    % Use our helper that works on all OTP versions
    AllPids = get_all_processes_compat(),

    ct:pal("Found ~p processes using compat function", [length(AllPids)]),

    % Should return a list
    ?assert(is_list(AllPids)),

    % Should contain at least our test processes
    TestProcesses = proplists:get_value(test_processes, Config),
    lists:foreach(fun(Pid) ->
        ?assert(lists:member(Pid, AllPids))
    end, TestProcesses).

%% @doc Test concurrent iteration from multiple processes
test_concurrent_iteration(Config) ->
    HasIterator = proplists:get_value(has_iterator, Config),

    case HasIterator of
        false ->
            {skip, "Process iterator not available (OTP <28)"};

        true ->
            ct:pal("Testing concurrent iteration"),

            % Spawn 10 concurrent iterators
            Self = self(),
            IteratorProcs = [spawn_link(fun() ->
                Pids = collect_all_processes_via_iterator(),
                Self ! {iterator_done, N, length(Pids)}
            end) || N <- lists:seq(1, 10)],

            % Collect results
            Results = [receive
                {iterator_done, N, Count} -> {N, Count}
            after 5000 ->
                timeout
            end || _ <- IteratorProcs],

            ct:pal("Concurrent iteration results: ~p", [Results]),

            % All should have completed (no timeouts)
            ?assertEqual(10, length([R || R = {_, _} <- Results])),

            % All should have found roughly the same number of processes
            Counts = [Count || {_, Count} <- Results],
            MaxCount = lists:max(Counts),
            MinCount = lists:min(Counts),

            % Allow small variance due to processes spawning/dying
            ?assert(MaxCount - MinCount < 100)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Check if process iterator API is available
has_process_iterator_api() ->
    % Try to call processes_iterator/0
    try
        erlang:processes_iterator(),
        true
    catch
        error:undef -> false;
        _:_ -> true  % API exists but might have failed for other reason
    end.

%% @doc Spawn test processes that wait for shutdown signal
spawn_test_processes(Count) ->
    [spawn_link(fun() -> test_process_loop() end) || _ <- lists:seq(1, Count)].

%% @doc Test process loop - waits for shutdown message
test_process_loop() ->
    receive
        shutdown -> ok
    after 60000 ->  % Timeout after 1 minute
        ok
    end.

%% @doc Cleanup test processes
cleanup_processes(Processes) ->
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true -> Pid ! shutdown;
            false -> ok
        end
    end, Processes),

    % Wait for processes to terminate
    timer:sleep(100).

%% @doc Collect all processes using iterator
collect_all_processes_via_iterator() ->
    Iterator = erlang:processes_iterator(),
    collect_processes_recursive(Iterator, []).

collect_processes_recursive(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} when is_pid(Pid) ->
            collect_processes_recursive(NewIterator, [Pid | Acc]);
        done ->
            lists:reverse(Acc)
    end.

%% @doc Iterate until done and count processes
iterate_until_done(Iterator, Count) ->
    case erlang:process_next(Iterator) of
        {_Pid, NewIterator} ->
            iterate_until_done(NewIterator, Count + 1);
        done ->
            Count
    end.

%% @doc Get all processes with compatibility for OTP 25-27
get_all_processes_compat() ->
    case has_process_iterator_api() of
        true ->
            % OTP 28+: use iterator
            collect_all_processes_via_iterator();
        false ->
            % OTP 25-27: use processes/0
            erlang:processes()
    end.
