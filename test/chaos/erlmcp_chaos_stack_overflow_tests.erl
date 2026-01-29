%%%-------------------------------------------------------------------
%%% @doc Stack overflow stress test - find recursion limits
%%% Created: 2025-01-29
%%% Purpose: Crash test with 1M depth recursion
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_stack_overflow_tests).
-author("stress_test").

-include_lib("eunit/include/eunit.hrl").

%% Test exports
-export([
    stack_overflow_suite/0,
    test_tail_recursion/1,
    test_non_tail_recursion/1,
    test_mutual_recursion/1,
    find_stack_limit/0
]).

%%%===================================================================
%%% Test Suite
%%%===================================================================

stack_overflow_suite() ->
    {"Stack Overflow Crash Tests", [
        {"tail_recursion_10", fun() -> test_tail_recursion(10) end},
        {"tail_recursion_100", fun() -> test_tail_recursion(100) end},
        {"tail_recursion_1000", fun() -> test_tail_recursion(1000) end},
        {"tail_recursion_10000", fun() -> test_tail_recursion(10000) end},
        {"tail_recursion_100000", fun() -> test_tail_recursion(100000) end},
        {"tail_recursion_1000000", fun() -> test_tail_recursion(1000000) end},
        
        {"non_tail_recursion_10", fun() -> test_non_tail_recursion(10) end},
        {"non_tail_recursion_100", fun() -> test_non_tail_recursion(100) end},
        {"non_tail_recursion_1000", fun() -> test_non_tail_recursion(1000) end},
        {"non_tail_recursion_10000", fun() -> test_non_tail_recursion(10000) end},
        {"non_tail_recursion_100000", fun() -> test_non_tail_recursion(100000) end},
        {"non_tail_recursion_1000000", fun() -> test_non_tail_recursion(1000000) end},
        
        {"mutual_recursion_10", fun() -> test_mutual_recursion(10) end},
        {"mutual_recursion_100", fun() -> test_mutual_recursion(100) end},
        {"mutual_recursion_1000", fun() -> test_mutual_recursion(1000) end},
        {"mutual_recursion_10000", fun() -> test_mutual_recursion(10000) end},
        {"mutual_recursion_100000", fun() -> test_mutual_recursion(100000) end},
        {"mutual_recursion_1000000", fun() -> test_mutual_recursion(1000000) end},
        
        {"find_stack_limit", fun find_stack_limit/0}
    ]}.

%%%===================================================================
%%% Tail Recursion Tests (should work at any depth)
%%%===================================================================

test_tail_recursion(Depth) ->
    io:format("~n=== TAIL RECURSION TEST: Depth ~p ===~n", [Depth]),
    Start = erlang:monotonic_time(microsecond),
    
    case catch tail_recurse(Depth, Depth) of
        {'EXIT', {Class, Reason, Stack}} ->
            Elapsed = erlang:monotonic_time(microsecond) - Start,
            io:format("CRASH at depth ~p after ~p us:~n", [Depth, Elapsed]),
            io:format("  Class: ~p~n", [Class]),
            io:format("  Reason: ~p~n", [Reason]),
            io:format("  Stack: ~p~n", [Stack]),
            {crashed, Depth, Class, Reason};
        Result ->
            Elapsed = erlang:monotonic_time(microsecond) - Start,
            io:format("SUCCESS: Tail recursion to depth ~p in ~p us~n", [Depth, Elapsed]),
            io:format("  Result: ~p~n", [Result]),
            io:format("  Rate: ~p depth/sec~n", [Depth * 1000000 div Elapsed]),
            {success, Depth, Elapsed}
    end.

%% Tail-recursive function (stack should not grow)
tail_recurse(0, Original) -> {reached, Original};
tail_recurse(N, Original) when N > 0 -> tail_recurse(N - 1, Original).

%%%===================================================================
%%% Non-Tail Recursion Tests (stack grows, should crash)
%%%===================================================================

test_non_tail_recursion(Depth) ->
    io:format("~n=== NON-TAIL RECURSION TEST: Depth ~p ===~n", [Depth]),
    Start = erlang:monotonic_time(microsecond),
    
    Process = spawn(fun() ->
        case catch not_tail_recurse(Depth) of
            {'EXIT', {Class, Reason, Stack}} ->
                Elapsed = erlang:monotonic_time(microsecond) - Start,
                io:format("CRASH at depth ~p after ~p us:~n", [Depth, Elapsed]),
                io:format("  Class: ~p~n", [Class]),
                io:format("  Reason: ~p~n", [Reason]),
                io:format("  Stack: ~p~n", [Stack]),
                exit({crashed, Depth, Class, Reason});
            Result ->
                Elapsed = erlang:monotonic_time(microsecond) - Start,
                io:format("SUCCESS: Non-tail recursion to depth ~p in ~p us~n", [Depth, Elapsed]),
                io:format("  Result: ~p~n", [Result]),
                exit({success, Depth, Elapsed})
        end
    end),
    
    receive
        {'EXIT', Process, Result} -> Result
    after 
        30000 -> timeout
    end.

%% Non-tail-recursive function (stack grows with each call)
not_tail_recurse(0) -> reached;
not_tail_recurse(N) when N > 0 -> 
    Result = not_tail_recurse(N - 1),
    Result + 1.

%%%===================================================================
%%% Mutual Recursion Tests (A calls B, B calls A)
%%%===================================================================

test_mutual_recursion(Depth) ->
    io:format("~n=== MUTUAL RECURSION TEST: Depth ~p ===~n", [Depth]),
    Start = erlang:monotonic_time(microsecond),
    
    Process = spawn(fun() ->
        case catch mutual_recurse_a(Depth) of
            {'EXIT', {Class, Reason, Stack}} ->
                Elapsed = erlang:monotonic_time(microsecond) - Start,
                io:format("CRASH at depth ~p after ~p us:~n", [Depth, Elapsed]),
                io:format("  Class: ~p~n", [Class]),
                io:format("  Reason: ~p~n", [Reason]),
                io:format("  Stack: ~p~n", [Stack]),
                exit({crashed, Depth, Class, Reason});
            Result ->
                Elapsed = erlang:monotonic_time(microsecond) - Start,
                io:format("SUCCESS: Mutual recursion to depth ~p in ~p us~n", [Depth, Elapsed]),
                io:format("  Result: ~p~n", [Result]),
                exit({success, Depth, Elapsed})
        end
    end),
    
    receive
        {'EXIT', Process, Result} -> Result
    after 
        30000 -> timeout
    end.

%% Mutual recursion - function A
mutual_recurse_a(0) -> reached;
mutual_recurse_a(N) when N > 0 -> mutual_recurse_b(N - 1).

%% Mutual recursion - function B (non-tail, stack grows)
mutual_recurse_b(0) -> reached;
mutual_recurse_b(N) when N > 0 -> 
    Result = mutual_recurse_a(N - 1),
    Result.

%%%===================================================================
%%% Binary Search for Stack Limit
%%%===================================================================

find_stack_limit() ->
    io:format("~n=== BINARY SEARCH FOR STACK LIMIT ===~n"),
    binary_search_limit(1, 1000000).

binary_search_limit(Low, High) when High < Low ->
    io:format("~nStack limit found between ~p and ~p~n", [Low, High + 1]),
    {limit_found, Low};
binary_search_limit(Low, High) ->
    Mid = (Low + High) div 2,
    io:format("~nTesting depth ~p (range: ~p-~p)~n", [Mid, Low, High]),
    
    case test_depth_safe(Mid) of
        success ->
            io:format("  SUCCESS at ~p, trying higher~n", [Mid]),
            binary_search_limit(Mid + 1, High);
        crash ->
            io:format("  CRASH at ~p, trying lower~n", [Mid]),
            binary_search_limit(Low, Mid - 1)
    end.

test_depth_safe(Depth) ->
    Process = spawn(fun() ->
        case catch not_tail_recurse(Depth) of
            {'EXIT', _} -> exit(crash);
            _ -> exit(success)
        end
    end),
    
    receive
        {'EXIT', Process, Result} -> Result
    after 
        5000 -> timeout
    end.

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

stack_overflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_tail_recursion(10)),
      ?_test(test_tail_recursion(100)),
      ?_test(test_tail_recursion(1000)),
      ?_test(test_non_tail_recursion(10)),
      ?_test(test_non_tail_recursion(100)),
      ?_test(test_mutual_recursion(10)),
      ?_test(test_mutual_recursion(100))
     ]}.

setup() ->
    io:format("~n=== STACK OVERFLOW TEST SUITE STARTED ===~n"),
    ok.

cleanup(_) ->
    io:format("~n=== STACK OVERFLOW TEST SUITE COMPLETED ===~n"),
    ok.
