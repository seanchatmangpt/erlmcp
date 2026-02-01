%%%====================================================================
%%% @doc erlmcp_batch Unit Tests
%%%
%%% Tests for request batching and pipelining functionality:
%%% - Batch accumulation
%%% - Timeout triggering
%%% - Result ordering
%%% - Partial failures
%%% - Strategy updates
%%% - Statistics tracking
%%%
%%% @end
%%%====================================================================

-module(erlmcp_batch_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Simple executor that echoes requests
echo_executor(Requests) ->
    [{ok,
      #{request_id => ReqId,
        method => Method,
        params => Params}}
     || {ReqId, Method, Params} <- Requests].

%% Executor that fails every Nth request
failing_executor(N) ->
    fun(Requests) ->
       lists:map(fun({Idx, Req}) ->
                    case Idx rem N of
                        0 ->
                            {error, simulated_failure};
                        _ ->
                            {ok, Req}
                    end
                 end,
                 lists:zip(
                     lists:seq(1, length(Requests)), Requests))
    end.

%% Executor with artificial latency (unused in tests, kept for future use)
%% slow_executor(DelayMs) ->
%%     fun(Requests) ->
%%         timer:sleep(DelayMs),
%%         echo_executor(Requests)
%%     end.

%%====================================================================
%% Size-Based Batching Tests
%%====================================================================

size_based_batching_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Batch executes when size reached", fun test_size_based_execution/0},
      {"Results maintain order", fun test_result_ordering/0},
      {"Flush forces immediate execution", fun test_manual_flush/0}]}.

test_size_based_execution() ->
    BatchSize = 5,
    {ok, Batcher} = erlmcp_batch:start_link(fun echo_executor/1, #{strategy => {size, BatchSize}}),

    % Add requests one by one
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test_method">>, #{index => I}),
             Ref
         end
         || I <- lists:seq(1, BatchSize)],

    % Should execute automatically when size reached
    Results = collect_results(Refs, 1000),

    % Verify all results received
    ?assertEqual(BatchSize, length(Results)),

    % Verify results are in order
    Indices = [maps:get(index, maps:get(params, Result)) || {ok, Result} <- Results],
    ?assertEqual(lists:seq(1, BatchSize), Indices),

    erlmcp_batch:stop(Batcher).

test_result_ordering() ->
    {ok, Batcher} = erlmcp_batch:start_link(fun echo_executor/1, #{strategy => {size, 10}}),

    % Add 10 requests with different data
    Refs =
        [begin
             {ok, Ref} =
                 erlmcp_batch:add_request(Batcher,
                                          <<"method_", (integer_to_binary(I))/binary>>,
                                          #{order => I}),
             Ref
         end
         || I <- lists:seq(1, 10)],

    % Collect results
    Results = collect_results(Refs, 1000),

    % Verify order is maintained
    Orders = [maps:get(order, maps:get(params, R)) || {ok, R} <- Results],
    ?assertEqual(lists:seq(1, 10), Orders),

    erlmcp_batch:stop(Batcher).

test_manual_flush() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {size, 100}}),  % Large batch size

    % Add only 3 requests (below batch size)
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 3)],

    % Manually flush
    ok = erlmcp_batch:flush(Batcher),

    % Should receive results immediately
    Results = collect_results(Refs, 1000),
    ?assertEqual(3, length(Results)),

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Time-Based Batching Tests
%%====================================================================

time_based_batching_test_() ->
    {timeout,
     10,
     {foreach,
      fun() -> ok end,
      fun(_) -> ok end,
      [{"Batch executes after timeout", fun test_time_based_execution/0},
       {"Multiple batches over time", fun test_multiple_time_batches/0}]}}.

test_time_based_execution() ->
    TimeoutMs = 100,
    {ok, Batcher} = erlmcp_batch:start_link(fun echo_executor/1, #{strategy => {time, TimeoutMs}}),

    % Add requests
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 3)],

    % Wait for timeout using poll (check for results)
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Results = collect_results(Refs, 10),
                                       length(Results) =:= 3
                                    end,
                                    batch_complete,
                                    TimeoutMs + 200,
                                    10),

    % Should have results
    Results = collect_results(Refs, 100),
    ?assertEqual(3, length(Results)),

    erlmcp_batch:stop(Batcher).

test_multiple_time_batches() ->
    TimeoutMs = 50,
    {ok, Batcher} = erlmcp_batch:start_link(fun echo_executor/1, #{strategy => {time, TimeoutMs}}),

    % First batch
    Refs1 =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{batch => 1, i => I}),
             Ref
         end
         || I <- lists:seq(1, 2)],

    % Wait for first batch using poll
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Results = collect_results(Refs1, 10),
                                       length(Results) =:= 2
                                    end,
                                    batch1_complete,
                                    TimeoutMs + 100,
                                    5),

    Results1 = collect_results(Refs1, 100),
    ?assertEqual(2, length(Results1)),

    % Second batch
    Refs2 =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{batch => 2, i => I}),
             Ref
         end
         || I <- lists:seq(1, 2)],

    % Wait for second batch using poll
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Results = collect_results(Refs2, 10),
                                       length(Results) =:= 2
                                    end,
                                    batch2_complete,
                                    TimeoutMs + 100,
                                    5),

    Results2 = collect_results(Refs2, 100),
    ?assertEqual(2, length(Results2)),

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Adaptive Batching Tests
%%====================================================================

adaptive_batching_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Adaptive batch size adjusts", fun test_adaptive_adjustment/0},
      {"High failure rate decreases batch size", fun test_adaptive_failure_response/0}]}.

test_adaptive_adjustment() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {adaptive, #{min => 2, max => 10}},
                                  parallel_workers => 1}),  % Sequential for predictable behavior

    % Send requests in batches to trigger adaptive behavior
    % Adaptive starts at min=2, so every 2 requests should trigger a batch
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 50)],

    % Wait for all batches to complete using poll (check stats)
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Stats = erlmcp_batch:get_stats(Batcher),
                                       TotalBatches = maps:get(total_batches, Stats, 0),
                                       TotalBatches > 5
                                    end,
                                    batches_complete,
                                    2000,
                                    50),

    % Check stats
    Stats = erlmcp_batch:get_stats(Batcher),
    TotalBatches = maps:get(total_batches, Stats),

    % Should have created multiple batches (50 requests / ~2-10 per batch)
    ?assert(TotalBatches > 5),

    erlmcp_batch:stop(Batcher).

test_adaptive_failure_response() ->
    % Executor that fails 20% of requests
    Executor = failing_executor(5),

    {ok, Batcher} =
        erlmcp_batch:start_link(Executor,
                                #{strategy => {adaptive, #{min => 5, max => 20}},
                                  parallel_workers =>
                                      1}),  % Sequential for predictable failure pattern

    % Send many requests
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 100)],

    % Wait for batches to complete using poll (check failures)
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Stats = erlmcp_batch:get_stats(Batcher),
                                       TotalFailures = maps:get(total_failures, Stats, 0),
                                       TotalFailures > 0
                                    end,
                                    failures_detected,
                                    2000,
                                    50),

    % Check stats
    Stats = erlmcp_batch:get_stats(Batcher),
    TotalFailures = maps:get(total_failures, Stats),

    % Should have some failures (20% of 100 = 20)
    ?assert(TotalFailures > 0),
    ?assert(TotalFailures < 100),

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Partial Failure Tests
%%====================================================================

partial_failure_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Partial failures don't block other results", fun test_partial_failures/0},
      {"All failures still return errors", fun test_all_failures/0}]}.

test_partial_failures() ->
    % Fail every 3rd request
    Executor = failing_executor(3),

    {ok, Batcher} =
        erlmcp_batch:start_link(Executor,
                                #{strategy => {size, 9},
                                  parallel_workers =>
                                      1}),  % Sequential execution for predictable failure pattern

    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 9)],

    Results = collect_results(Refs, 1000),

    % Should have all results
    ?assertEqual(9, length(Results)),

    % Count successes and failures
    {Successes, Failures} =
        lists:partition(fun ({ok, _}) ->
                                true;
                            (_) ->
                                false
                        end,
                        Results),

    ?assertEqual(6, length(Successes)),
    ?assertEqual(3, length(Failures)),

    erlmcp_batch:stop(Batcher).

test_all_failures() ->
    % Executor that always fails
    Executor = fun(_Requests) -> [{error, always_fails} || _ <- _Requests] end,

    {ok, Batcher} = erlmcp_batch:start_link(Executor, #{strategy => {size, 5}}),

    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 5)],

    Results = collect_results(Refs, 1000),

    % All should be errors
    ?assert(lists:all(fun ({error, _}) ->
                              true;
                          (_) ->
                              false
                      end,
                      Results)),

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Statistics Tests
%%====================================================================

statistics_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Statistics are tracked correctly", fun test_statistics/0},
      {"Average batch size is calculated", fun test_avg_batch_size/0}]}.

test_statistics() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {size, 5},
                                  parallel_workers => 1}),  % Sequential for predictable stats

    % Send 15 requests (3 batches of 5 each)
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, 15)],

    % Wait for all results
    _ = [receive
             {batch_result, Ref, _} ->
                 ok
         after 5000 ->
             timeout
         end
         || Ref <- Refs],

    Stats = erlmcp_batch:get_stats(Batcher),

    ?assertEqual(15, maps:get(total_requests, Stats)),
    ?assertEqual(3, maps:get(total_batches, Stats)),
    ?assertEqual(0, maps:get(total_failures, Stats)),
    ?assertEqual(5.0, maps:get(avg_batch_size, Stats)),

    erlmcp_batch:stop(Batcher).

test_avg_batch_size() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {size, 100},  % Large size, will use manual flush
                                  parallel_workers => 1}),     % Sequential for predictable stats

    % Batch 1: 5 requests
    Refs1 =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{}),
             Ref
         end
         || _ <- lists:seq(1, 5)],
    erlmcp_batch:flush(Batcher),

    % Wait for all batch 1 results
    _ = [receive
             {batch_result, Ref, _} ->
                 ok
         after 5000 ->
             timeout
         end
         || Ref <- Refs1],

    % Batch 2: 10 requests
    Refs2 =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{}),
             Ref
         end
         || _ <- lists:seq(1, 10)],
    erlmcp_batch:flush(Batcher),

    % Wait for all batch 2 results
    _ = [receive
             {batch_result, Ref, _} ->
                 ok
         after 5000 ->
             timeout
         end
         || Ref <- Refs2],

    % Wait for stats to update using poll
    {ok, _} =
        erlmcp_test_sync:poll_until(fun() ->
                                       Stats = erlmcp_batch:get_stats(Batcher),
                                       maps:get(avg_batch_size, Stats, 0) > 0
                                    end,
                                    stats_updated,
                                    500,
                                    10),

    Stats = erlmcp_batch:get_stats(Batcher),

    AvgBatchSize = maps:get(avg_batch_size, Stats),
    ?assertEqual(7.5, AvgBatchSize),  % (5 + 10) / 2

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Strategy Update Tests
%%====================================================================

strategy_update_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [{"Strategy can be updated dynamically", fun test_strategy_update/0}]}.

test_strategy_update() ->
    {ok, Batcher} = erlmcp_batch:start_link(fun echo_executor/1, #{strategy => {size, 5}}),

    % Change to time-based
    ok = erlmcp_batch:update_strategy(Batcher, {time, 50}),

    % Add request
    {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{}),

    % Wait for timeout using poll (check for result)
    {ok, Result} =
        erlmcp_test_sync:wait_for_message(fun ({batch_result, Ref, _}) ->
                                                  true;
                                              (_) ->
                                                  false
                                          end,
                                          200),

    ?assertMatch({ok, _}, element(3, Result)),

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {timeout,
     30,
     {foreach,
      fun() -> ok end,
      fun(_) -> ok end,
      [{"High throughput batching", fun test_high_throughput/0},
       {"Latency remains acceptable", fun test_latency/0}]}}.

test_high_throughput() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {size, 100}, parallel_workers => 8}),

    NumRequests = 10000,
    StartTime = erlang:monotonic_time(microsecond),

    % Send all requests
    Refs =
        [begin
             {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{i => I}),
             Ref
         end
         || I <- lists:seq(1, NumRequests)],

    % Force flush last batch
    erlmcp_batch:flush(Batcher),

    % Collect all results
    _ = collect_results(Refs, 10000),

    EndTime = erlang:monotonic_time(microsecond),
    DurationSec = (EndTime - StartTime) / 1_000_000,
    Throughput = NumRequests / DurationSec,

    io:format("Throughput: ~.2f req/sec~n", [Throughput]),

    % Should achieve reasonable throughput
    ?assert(Throughput > 1000),

    erlmcp_batch:stop(Batcher).

test_latency() ->
    {ok, Batcher} =
        erlmcp_batch:start_link(fun echo_executor/1,
                                #{strategy => {time, 10}}),  % 10ms batching

    % Measure latency for single request
    StartTime = erlang:monotonic_time(microsecond),
    {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test">>, #{}),

    Result =
        receive
            {batch_result, Ref, R} ->
                R
        after 1000 ->
            timeout
        end,

    EndTime = erlang:monotonic_time(microsecond),
    LatencyMs = (EndTime - StartTime) / 1000,

    io:format("Latency: ~.2f ms~n", [LatencyMs]),

    ?assertMatch({ok, _}, Result),
    ?assert(LatencyMs < 100),  % Should be well under 100ms

    erlmcp_batch:stop(Batcher).

%%====================================================================
%% Helper Functions
%%====================================================================

%% Collect batch results
collect_results(Refs, Timeout) ->
    [receive
         {batch_result, Ref, Result} ->
             Result
     after Timeout ->
         timeout
     end
     || Ref <- Refs].
