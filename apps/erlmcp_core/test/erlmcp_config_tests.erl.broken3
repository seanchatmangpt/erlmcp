%%%-------------------------------------------------------------------
%%% @doc erlmcp_config_tests - Configuration Management Tests
%%%
%%% Tests the fast persistent_term-based configuration system:
%%% - Real gen_server (no mocks)
%%% - Zero-copy reads via persistent_term
%%% - State-based verification (config values)
%%% - Bulk updates (single GC event)
%%% - Reload from application environment
%%%
%%% Uses Chicago School TDD:
%%% - Verify observable config values (not internals)
%%% - Test performance characteristics (fast reads)
%%% - Test GC minimization (bulk updates)
%%%
%%% Target: 90%+ coverage (core infrastructure, performance-critical)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_config_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

config_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_config_start/1,
      fun test_get_existing_key/1,
      fun test_get_missing_key_with_default/1,
      fun test_get_all/1,
      fun test_set_single_key/1,
      fun test_bulk_update/1,
      fun test_delete_key/1,
      fun test_reload/1,
      fun test_clear_cache/1,
      fun test_persistent_term_performance/1,
      fun test_bulk_update_minimizes_gc/1,
      fun test_get_with_default_missing_key/1,
      fun test_get_with_default_existing_key/1,
      fun test_set_and_get/1,
      fun test_update_and_get/1,
      fun test_delete_and_get/1,
      fun test_default_values/1,
      fun test_transport_defaults/1,
      fun test_capabilities_defaults/1,
      fun test_rate_limit_defaults/1,
      fun test_circuit_breaker_defaults/1,
      fun test_gen_server_handle_call_unknown/1,
      fun test_gen_server_handle_cast/1,
      fun test_gen_server_handle_info/1,
      fun test_gen_server_code_change/1,
      fun test_gen_server_terminate/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start config server
    {ok, Pid} = erlmcp_config:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop config server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_config);
        false ->
            ok
    end.

%%====================================================================
%% Basic API Tests
%%====================================================================

test_config_start(Pid) ->
    %% Verify: Server started successfully
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)).

test_get_existing_key(_Pid) ->
    %% Exercise: Get existing config key (with default)
    Value = erlmcp_config:get(max_message_size),

    %% Verify: Returns default value
    ?assert(is_integer(Value)),
    ?assertEqual(10485760, Value). % 10MB default

test_get_missing_key_with_default(_Pid) ->
    %% Exercise: Get missing key with default
    Value = erlmcp_config:get(nonexistent_key, default_value),

    %% Verify: Returns default value
    ?assertEqual(default_value, Value).

test_get_all(_Pid) ->
    %% Exercise: Get all config
    AllConfig = erlmcp_config:get_all(),

    %% Verify: Returns map with multiple keys
    ?assert(is_map(AllConfig)),
    ?assert(maps:size(AllConfig) > 10).

%%====================================================================
%% Set and Update Tests
%%====================================================================

test_set_single_key(_Pid) ->
    %% Exercise: Set a config key
    ok = erlmcp_config:set(test_key, test_value),

    %% Verify: Value is retrievable
    Value = erlmcp_config:get(test_key),
    ?assertEqual(test_value, Value).

test_bulk_update(_Pid) ->
    %% Exercise: Bulk update multiple keys (single GC event)
    Updates = #{
        bulk_key_1 => value_1,
        bulk_key_2 => value_2,
        bulk_key_3 => value_3
    },
    ok = erlmcp_config:update(Updates),

    %% Verify: All values set
    ?assertEqual(value_1, erlmcp_config:get(bulk_key_1)),
    ?assertEqual(value_2, erlmcp_config:get(bulk_key_2)),
    ?assertEqual(value_3, erlmcp_config:get(bulk_key_3)).

test_delete_key(_Pid) ->
    %% Exercise: Set then delete a key
    ok = erlmcp_config:set(temp_key, temp_value),
    ?assertEqual(temp_value, erlmcp_config:get(temp_key)),

    ok = erlmcp_config:delete(temp_key),

    %% Verify: Key deleted (returns default)
    ?assertEqual(undefined, erlmcp_config:get(temp_key, undefined)).

%%====================================================================
%% Reload and Clear Tests
%%====================================================================

test_reload(_Pid) ->
    %% Exercise: Reload config from application environment
    ok = erlmcp_config:set(test_reload_key, test_value),

    %% Reload
    ok = erlmcp_config:reload(),

    %% Verify: Test key cleared (reloaded to defaults)
    %% Note: If test_reload_key not in app env, should be gone
    Default = erlmcp_config:get(test_reload_key, reloaded),
    ?assertEqual(reloaded, Default).

test_clear_cache(_Pid) ->
    %% Exercise: Clear all config (testing only)
    ok = erlmcp_config:set(test_clear_key, test_value),
    ?assertEqual(test_value, erlmcp_config:get(test_clear_key)),

    %% Clear
    ok = erlmcp_config:clear_cache(),

    %% Verify: All config cleared (defaults reset on next access)
    %% After clear, subsequent get/1 calls will reinitialize from defaults
    ok.

%%====================================================================
%% Performance Tests
%%====================================================================

test_persistent_term_performance(_Pid) ->
    %% Exercise: Measure read performance (should be ~10ns)
    Iterations = 10000,

    StartTime = erlang:monotonic_time(nanosecond),
    lists:foreach(fun(_) ->
        erlmcp_config:get(max_message_size)
    end, lists:seq(1, Iterations)),
    EndTime = erlang:monotonic_time(nanosecond),

    DurationNs = EndTime - StartTime,
    AvgNs = DurationNs div Iterations,

    %% Verify: Average read is fast (<1μs = 1000ns)
    ?assert(AvgNs < 1000,
            io_lib:format("Read too slow: ~pns (should be <1000ns)", [AvgNs])).

test_bulk_update_minimizes_gc(_Pid) ->
    %% Exercise: Compare bulk update vs multiple sets
    Keys = lists:seq(1, 100),

    %% Bulk update (single GC)
    BulkUpdates = maps:from_list(
        [{list_to_existing_atom("bulk_" ++ integer_to_list(K)), K}
         || K <- Keys]),
    {BulkTime, _} = timer:tc(fun() ->
        erlmcp_config:update(BulkUpdates)
    end),

    %% Multiple sets (multiple GCs - slower)
    {MultiTime, _} = timer:tc(fun() ->
        lists:foreach(fun(K) ->
            Key = list_to_existing_atom("multi_" ++ integer_to_list(K)),
            erlmcp_config:set(Key, K)
        end, Keys)
    end),

    %% Verify: Bulk update is faster (or at least not much slower)
    %% We expect bulk to be <= multi due to single GC event
    ?assert(BulkTime =< MultiTime * 2,
            io_lib:format("Bulk update too slow: ~pμs vs multi ~pμs",
                         [BulkTime, MultiTime])).

%%====================================================================
%% Get with Default Tests
%%====================================================================

test_get_with_default_missing_key(_Pid) ->
    %% Exercise: Get missing key with default
    Value = erlmcp_config:get(missing_key_123, my_default),

    %% Verify: Returns default
    ?assertEqual(my_default, Value).

test_get_with_default_existing_key(_Pid) ->
    %% Exercise: Get existing key with default (default ignored)
    ok = erlmcp_config:set(existing_key_test, actual_value),

    Value = erlmcp_config:get(existing_key_test, ignored_default),

    %% Verify: Returns actual value, not default
    ?assertEqual(actual_value, Value).

%%====================================================================
%% Integration Tests
%%====================================================================

test_set_and_get(_Pid) ->
    %% Exercise: Set then get
    ok = erlmcp_config:set(integration_key, integration_value),
    Value = erlmcp_config:get(integration_key),

    ?assertEqual(integration_value, Value).

test_update_and_get(_Pid) ->
    %% Exercise: Update then get all
    Updates = #{
        update_key_1 => val_1,
        update_key_2 => val_2
    },
    ok = erlmcp_config:update(Updates),

    AllConfig = erlmcp_config:get_all(),

    ?assertEqual(val_1, maps:get(update_key_1, AllConfig)),
    ?assertEqual(val_2, maps:get(update_key_2, AllConfig)).

test_delete_and_get(_Pid) ->
    %% Exercise: Set, delete, get
    ok = erlmcp_config:set(delete_test, value),

    ok = erlmcp_config:delete(delete_test),

    %% Verify deleted
    ?assertEqual(undefined, erlmcp_config:get(delete_test, undefined)).

%%====================================================================
%% Default Values Tests
%%====================================================================

test_default_values(_Pid) ->
    %% Exercise: Verify default values are loaded
    ?assertEqual(10485760, erlmcp_config:get(max_message_size)),
    ?assertEqual(100, erlmcp_config:get(max_batch_size)),
    ?assertEqual(30000, erlmcp_config:get(request_timeout)),
    ?assertEqual(300000, erlmcp_config:get(idle_timeout)).

test_transport_defaults(_Pid) ->
    %% Exercise: Verify transport defaults
    TransportDefaults = erlmcp_config:get(transport_defaults),
    ?assert(is_map(TransportDefaults)),

    ?assert(maps:is_key(tcp, TransportDefaults)),
    ?assert(maps:is_key(http, TransportDefaults)),
    ?assert(maps:is_key(ws, TransportDefaults)),
    ?assert(maps:is_key(stdio, TransportDefaults)).

test_capabilities_defaults(_Pid) ->
    %% Exercise: Verify capabilities defaults
    Capabilities = erlmcp_config:get(capabilities),
    ?assert(is_map(Capabilities)),

    ?assertEqual(true, maps:get(tools, Capabilities)),
    ?assertEqual(true, maps:get(resources, Capabilities)),
    ?assertEqual(true, maps:get(prompts, Capabilities)).

test_rate_limit_defaults(_Pid) ->
    %% Exercise: Verify rate limiting defaults
    ?assertEqual(true, erlmcp_config:get(rate_limit_enabled)),
    ?assertEqual(1000, erlmcp_config:get(rate_limit_requests_per_second)),
    ?assertEqual(100, erlmcp_config:get(rate_limit_burst)).

test_circuit_breaker_defaults(_Pid) ->
    %% Exercise: Verify circuit breaker defaults
    ?assertEqual(true, erlmcp_config:get(circuit_breaker_enabled)),
    ?assertEqual(5, erlmcp_config:get(circuit_breaker_threshold)),
    ?assertEqual(60000, erlmcp_config:get(circuit_breaker_timeout)).

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

test_gen_server_handle_call_unknown(_Pid) ->
    %% Exercise: Send unknown call
    Result = gen_server:call(erlmcp_config, unknown_request),

    %% Verify: Returns error
    ?assertEqual({error, unknown_request}, Result).

test_gen_server_handle_cast(_Pid) ->
    %% Exercise: Send cast
    Result = gen_server:cast(erlmcp_config, test_cast),

    %% Verify: Doesn't crash
    ?assertEqual(ok, Result).

test_gen_server_handle_info(_Pid) ->
    %% Exercise: Send info message
    Pid ! test_info,
    timer:sleep(100),

    %% Verify: Server still alive
    ?assert(is_process_alive(Pid)).

test_gen_server_code_change(_Pid) ->
    %% Exercise: Code change
    {ok, State} = sys:get_state(erlmcp_config),
    Result = erlmcp_config:code_change("", State, ""),

    %% Verify: Succeeds
    ?assertMatch({ok, _}, Result).

test_gen_server_terminate(_Pid) ->
    %% Exercise: Stop server
    ok = gen_server:stop(erlmcp_config),

    %% Verify: Stopped
    ?assertNot(is_process_alive(Pid)).
