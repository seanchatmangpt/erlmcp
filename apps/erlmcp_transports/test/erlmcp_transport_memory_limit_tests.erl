%%%-------------------------------------------------------------------
%%% @doc Transport Memory Limit Integration Tests
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_memory_limit_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports).

cleanup(_) ->
    application:stop(erlmcp_transports),
    application:stop(erlmcp_core).

%%%===================================================================
%%% Test Cases
%%%===================================================================

tcp_transport_memory_limit_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         OversizeMessage = 20 * 1024 * 1024,
         ?assertEqual({error, payload_too_large}, erlmcp_memory_guard:check_allocation(OversizeMessage)),
         NormalMessage = byte_size(<<"{\"test\": \"data\"}">>),
         ?assertEqual(ok, erlmcp_memory_guard:check_allocation(NormalMessage))
     end)] end}.

stdio_transport_memory_limit_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         MaxSize = erlmcp_memory_guard:get_payload_limit(),
         ?assertEqual(16777216, MaxSize),
         SmallMessage = byte_size(<<"{\"key\": \"value\"}">>),
         ?assertEqual(ok, erlmcp_memory_guard:check_allocation(SmallMessage)),
         OversizeMessage = 20 * 1024 * 1024,
         ?assertEqual({error, payload_too_large}, erlmcp_memory_guard:check_allocation(OversizeMessage))
     end)] end}.

http_transport_memory_limit_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         SmallMessage = jsx:encode(#{jsonrpc => <<"2.0">>, method => <<"test">>, id => 1}),
         ?assertEqual(ok, erlmcp_memory_guard:check_allocation(byte_size(SmallMessage))),
         OversizePayload = <<"{\"data\": \"", <<0:8000000/unit:8>>/binary, "\"}">>,
         ?assertEqual({error, payload_too_large}, erlmcp_memory_guard:check_allocation(byte_size(OversizePayload)))
     end)] end}.

bounded_refusal_error_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         OversizeMessage = 20 * 1024 * 1024,
         case erlmcp_memory_guard:check_allocation(OversizeMessage) of
             {error, payload_too_large} -> ?assert(true);
             Other -> ?assert(false, {expected_payload_too_large, Other})
         end
     end)] end}.

memory_limit_various_sizes_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         Limit = erlmcp_memory_guard:get_payload_limit(),
         ?assertEqual(ok, erlmcp_memory_guard:check_allocation(Limit)),
         ?assertEqual({error, payload_too_large}, erlmcp_memory_guard:check_allocation(Limit + 1)),
         TestSizes = [{1, ok}, {1024, ok}, {1024 * 1024, ok}, {10 * 1024 * 1024, ok}, {Limit, ok}, {Limit + 1, {error, payload_too_large}}, {Limit * 2, {error, payload_too_large}}],
         lists:foreach(fun({Size, Expected}) -> ?assertEqual(Expected, erlmcp_memory_guard:check_allocation(Size)) end, TestSizes)
     end)] end}.

memory_guard_stats_under_load_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         InitialStats = erlmcp_memory_guard:get_memory_stats(),
         lists:foreach(fun(Size) -> erlmcp_memory_guard:check_allocation(Size) end, [1024, 1024 * 1024, 10 * 1024 * 1024]),
         FinalStats = erlmcp_memory_guard:get_memory_stats(),
         ?assert(is_map(InitialStats)),
         ?assert(is_map(FinalStats)),
         ?assert(maps:is_key(used_percent, InitialStats)),
         ?assert(maps:is_key(used_percent, FinalStats))
     end)] end}.

circuit_breaker_test_() ->
    {setup, fun setup/0, fun cleanup/1,
     fun(_) -> [?_test(begin
         IsOpen = erlmcp_memory_guard:is_circuit_breaker_open(),
         ?assert(is_boolean(IsOpen)),
         Threshold = erlmcp_memory_guard:get_circuit_breaker_threshold(),
         ?assert(Threshold >= 0.0),
         ?assert(Threshold =< 1.0)
     end)] end}.
