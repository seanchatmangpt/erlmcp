-module(erlmcp_batch_processor_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    case whereis(erlmcp_batch_processor) of
        undefined -> {ok, Pid} = erlmcp_batch_processor:start_link(), Pid;
        Pid -> Pid
    end.

cleanup(_Pid) ->
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

batch_processor_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun configure_test/0,
      fun get_config_test/0,
      fun process_batch_single_test/0,
      fun process_batch_multiple_test/0,
      fun process_batch_empty_test/0,
      fun get_metrics_test/0
     ]}.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

configure_test() ->
    {"configure updates configuration", fun() ->
        ?assertEqual(ok, erlmcp_batch_processor:configure(max_batch_size, 50)),
        ?assertEqual(ok, erlmcp_batch_processor:configure(batch_timeout, 2000)),
        ?assertEqual(ok, erlmcp_batch_processor:configure(max_concurrency, 5))
    end}.

get_config_test() ->
    {"get_config returns current configuration", fun() ->
        Config = erlmcp_batch_processor:get_config(),
        ?assert(is_map(Config)),
        ?assert(maps:is_key(max_batch_size, Config)),
        ?assert(maps:is_key(batch_timeout, Config)),
        ?assert(maps:is_key(max_concurrency, Config)),
        ?assertNotEqual(undefined, maps:get(max_batch_size, Config, undefined))
    end}.

process_batch_single_test() ->
    {"process_batch processes single item", fun() ->
        Batch = [#{id => 1, method => <<"test">>}],
        Result = erlmcp_batch_processor:process_batch(Batch),
        ?assertMatch({ok, #{success := _, errors := _}}, Result)
    end}.

process_batch_multiple_test() ->
    {"process_batch processes multiple items", fun() ->
        Batch = [#{id => N, method => <<"batch_test">>} || N <- lists:seq(1, 10)],
        Result = erlmcp_batch_processor:process_batch(Batch),
        ?assertMatch({ok, #{success := _, errors := _}}, Result)
    end}.

process_batch_empty_test() ->
    {"process_batch rejects empty batch", fun() ->
        Result = erlmcp_batch_processor:process_batch([]),
        ?assertEqual({error, empty_batch}, Result)
    end}.

get_metrics_test() ->
    {"get_metrics returns processing statistics", fun() ->
        Metrics = erlmcp_batch_processor:get_metrics(),
        ?assert(is_map(Metrics)),
        ?assert(maps:is_key(batches_processed, Metrics)),
        ?assert(maps:is_key(items_processed, Metrics)),
        ?assert(maps:is_key(errors, Metrics))
    end}.
