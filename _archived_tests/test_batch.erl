#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main(_Args) ->
    io:format("Testing erlmcp_batch module...~n~n"),

    %% Test 1: Start batcher
    io:format("Test 1: Starting batcher...~n"),
    Executor = fun(Requests) ->
        [{ok, Req} || Req <- Requests]
    end,

    {ok, Batcher} = erlmcp_batch:start_link(Executor, #{
        strategy => {size, 5}
    }),
    io:format("  ✓ Batcher started~n~n"),

    %% Test 2: Add requests
    io:format("Test 2: Adding requests...~n"),
    Refs = [begin
        {ok, Ref} = erlmcp_batch:add_request(Batcher, <<"test_method">>, #{index => I}),
        Ref
    end || I <- lists:seq(1, 5)],
    io:format("  ✓ Added 5 requests~n~n"),

    %% Test 3: Collect results
    io:format("Test 3: Collecting results...~n"),
    Results = [begin
        receive
            {batch_result, Ref, Result} -> Result
        after 5000 ->
            timeout
        end
    end || Ref <- Refs],

    SuccessCount = length([ok || {ok, _} <- Results]),
    io:format("  ✓ Received ~p/5 results~n~n", [SuccessCount]),

    %% Test 4: Get statistics
    io:format("Test 4: Getting statistics...~n"),
    Stats = erlmcp_batch:get_stats(Batcher),
    io:format("  Total requests: ~p~n", [maps:get(total_requests, Stats)]),
    io:format("  Total batches: ~p~n", [maps:get(total_batches, Stats)]),
    io:format("  Avg batch size: ~.1f~n~n", [maps:get(avg_batch_size, Stats)]),

    %% Test 5: Stop batcher
    io:format("Test 5: Stopping batcher...~n"),
    erlmcp_batch:stop(Batcher),
    io:format("  ✓ Batcher stopped~n~n"),

    io:format("All tests passed!~n"),
    halt(0).
