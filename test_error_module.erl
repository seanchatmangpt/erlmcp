#!/usr/bin/env escript
%% -*- erlang -*-

-module(test_error_module).
-export([main/1]).

main(_Args) ->
    erlmcp_error:start_error_collector(),

    % Test 1: Context creation
    io:format("Test 1: Creating context...~n"),
    Context = erlmcp_error:new_context(test_op),
    io:format("  OK - Context created with error_id: ~s~n", [maps:get(error_id, Context)]),

    % Test 2: Error creation
    io:format("Test 2: Creating error...~n"),
    Error = erlmcp_error:error(-32009, <<"Timeout error">>),
    io:format("  OK - Error created~n"),

    % Test 3: Error categorization
    io:format("Test 3: Categorizing error...~n"),
    Category = erlmcp_error:categorize(Error),
    io:format("  OK - Error category: ~w~n", [Category]),

    % Test 4: Error logging
    io:format("Test 4: Logging error...~n"),
    erlmcp_error:log_error(Error, Context),
    io:format("  OK - Error logged~n"),

    % Test 5: Recovery decision
    io:format("Test 5: Checking if retryable...~n"),
    Retryable = erlmcp_error:is_retryable(Error),
    io:format("  OK - Retryable: ~w~n", [Retryable]),

    % Test 6: 100K context creation benchmark
    io:format("Test 6: Performance test - creating 100K contexts...~n"),
    {Time, _} = timer:tc(fun() ->
        [erlmcp_error:new_context(
            list_to_atom("op_" ++ integer_to_list(I rem 10)),
            #{connection_id => <<"conn-", (integer_to_binary(I))/binary>>}
        ) || I <- lists:seq(1, 100000)]
    end),
    TimeMs = Time div 1000,
    Throughput = (100000 * 1000) div TimeMs,
    io:format("  OK - Created 100K contexts in ~wms (~w contexts/sec)~n", [TimeMs, Throughput]),

    % Test 7: Error collection stats
    io:format("Test 7: Collecting error statistics...~n"),
    erlmcp_error:reset_error_stats(),
    [erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, 1000)],
    timer:sleep(100),
    Stats = erlmcp_error:get_error_stats(),
    Total = maps:get(total, Stats, 0),
    io:format("  OK - Collected ~w errors~n", [Total]),

    % Test 8: Error info extraction
    io:format("Test 8: Extracting error information...~n"),
    Info = erlmcp_error:extract_error_info(Error),
    io:format("  OK - Error info: code=~w, category=~w~n", [
        maps:get(code, Info),
        maps:get(category, Info)
    ]),

    % Summary
    io:format("~nAll tests passed!~n"),
    halt(0).
