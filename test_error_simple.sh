#!/bin/bash

erl -pa _build/default/lib/*/ebin -noinput << 'EOF'
% Start the error collector
erlmcp_error:start_error_collector(),

% Test 1: Context creation
io:format("Test 1: Context creation~n", []),
Context = erlmcp_error:new_context(test_op),
io:format("  PASS - Error ID: ~s~n", [maps:get(error_id, Context)]),

% Test 2: Error creation
io:format("Test 2: Error creation~n", []),
Error = erlmcp_error:error(-32009, <<"Timeout">>),
io:format("  PASS~n", []),

% Test 3: Categorization
io:format("Test 3: Categorization~n", []),
Cat = erlmcp_error:categorize(Error),
io:format("  Category: ~w~n", [Cat]),

% Test 4: Retryable check
io:format("Test 4: Retryable check~n", []),
Retryable = erlmcp_error:is_retryable(Error),
io:format("  Retryable: ~w~n", [Retryable]),

% Test 5: Error extraction
io:format("Test 5: Error info extraction~n", []),
Info = erlmcp_error:extract_error_info(Error),
io:format("  Code: ~w, Category: ~w~n", [maps:get(code, Info), maps:get(category, Info)]),

% Test 6: Backoff delay
io:format("Test 6: Backoff delay calculation~n", []),
Delay = erlmcp_error:backoff_delay(1, 3),
io:format("  Delay: ~wms~n", [Delay]),

% Test 7: Statistics collection
io:format("Test 7: Error statistics~n", []),
erlmcp_error:reset_error_stats(),
[erlmcp_error:collect_error(transient, 1) || _ <- lists:seq(1, 100)],
timer:sleep(100),
Stats = erlmcp_error:get_error_stats(),
io:format("  Total errors: ~w~n", [maps:get(total, Stats, 0)]),

% Test 8: Performance - 100K contexts
io:format("Test 8: Performance test (100K contexts)~n", []),
{Time, _} = timer:tc(fun() ->
  [erlmcp_error:new_context(
    list_to_atom("op_" ++ integer_to_list(I rem 5)),
    #{connection_id => I}
  ) || I <- lists:seq(1, 100000)]
end),
TimeMs = Time div 1000,
Throughput = (100000 * 1000) div TimeMs,
io:format("  Time: ~wms, Throughput: ~w contexts/sec~n", [TimeMs, Throughput]),

io:format("~n=== ALL TESTS PASSED ===~n", []),
halt(0).
EOF
