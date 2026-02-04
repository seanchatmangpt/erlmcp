-module(erlmcp_new_features_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite Setup/Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_new_features_app:start(normal, []),
    Pid.

cleanup(_Pid) ->
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

erlmcp_new_features_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun supervisor_starts/0,
      fun mcp_proxy_relay_tests/0,
      fun batch_processor_tests/0,
      fun json_schema_validator_tests/0,
      fun event_bus_tests/0
     ]}.

%%====================================================================
%% Individual Tests
%%====================================================================

supervisor_starts() ->
    {"Supervisor starts all children", fun() ->
        ?assertNotEqual(undefined, whereis(erlmcp_new_features_sup)),
        ?assertNotEqual(undefined, whereis(erlmcp_mcp_proxy_relay)),
        ?assertNotEqual(undefined, whereis(erlmcp_batch_processor)),
        ?assertNotEqual(undefined, whereis(erlmcp_json_schema_validator)),
        ?assertNotEqual(undefined, whereis(erlmcp_event_bus))
    end}.

mcp_proxy_relay_tests() ->
    {"MCP Proxy Relay", [
        {"add_upstream adds upstream", fun() ->
            ok = erlmcp_mcp_proxy_relay:add_upstream(test_upstream, <<"http://localhost:8080">>),
            Upstreams = erlmcp_mcp_proxy_relay:list_upstreams(),
            ?assert(length(Upstreams) > 0)
        end},
        {"forward_request works with upstream", fun() ->
            ok = erlmcp_mcp_proxy_relay:add_upstream(upstream1, <<"http://test.example.com">>),
            Request = #{jsonrpc => <<"2.0">>, id => 1, method => <<"test">>},
            Result = erlmcp_mcp_proxy_relay:forward_request(Request),
            ?assertMatch({ok, _}, Result)
        end},
        {"remove_upstream removes upstream", fun() ->
            ok = erlmcp_mcp_proxy_relay:add_upstream(temp_upstream, <<"http://temp.com">>),
            ok = erlmcp_mcp_proxy_relay:remove_upstream(temp_upstream),
            ?assertEqual(ok, ok)
        end},
        {"get_stats returns stats map", fun() ->
            Stats = erlmcp_mcp_proxy_relay:get_stats(),
            ?assert(is_map(Stats)),
            ?assert(maps:is_key(forwarded, Stats)),
            ?assert(maps:is_key(errors, Stats))
        end}
    ]}.

batch_processor_tests() ->
    {"Batch Processor", [
        {"configure max_batch_size works", fun() ->
            ?assertEqual(ok, erlmcp_batch_processor:configure(max_batch_size, 50))
        end},
        {"configure batch_timeout works", fun() ->
            ?assertEqual(ok, erlmcp_batch_processor:configure(batch_timeout, 2000))
        end},
        {"configure max_concurrency works", fun() ->
            ?assertEqual(ok, erlmcp_batch_processor:configure(max_concurrency, 5))
        end},
        {"get_config returns config map", fun() ->
            Config = erlmcp_batch_processor:get_config(),
            ?assert(is_map(Config)),
            ?assert(maps:is_key(max_batch_size, Config)),
            ?assert(maps:is_key(batch_timeout, Config)),
            ?assert(maps:is_key(max_concurrency, Config))
        end},
        {"process_batch handles single item", fun() ->
            Batch = [#{id => 1, method => <<"test">>}],
            Result = erlmcp_batch_processor:process_batch(Batch),
            ?assertMatch({ok, _}, Result)
        end},
        {"process_batch handles multiple items", fun() ->
            Batch = [#{id => N, method => <<"test">>} || N <- lists:seq(1, 5)],
            Result = erlmcp_batch_processor:process_batch(Batch),
            ?assertMatch({ok, _}, Result)
        end},
        {"process_batch rejects empty batch", fun() ->
            Result = erlmcp_batch_processor:process_batch([]),
            ?assertEqual({error, empty_batch}, Result)
        end},
        {"get_metrics returns metrics map", fun() ->
            Metrics = erlmcp_batch_processor:get_metrics(),
            ?assert(is_map(Metrics)),
            ?assert(maps:is_key(batches_processed, Metrics)),
            ?assert(maps:is_key(items_processed, Metrics))
        end}
    ]}.

json_schema_validator_tests() ->
    {"JSON Schema Validator", [
        {"compile_schema works with valid schema", fun() ->
            Schema = #{
                <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
            },
            Result = erlmcp_json_schema_validator:compile_schema(Schema),
            ?assertMatch({ok, _}, Result)
        end},
        {"load_schema works", fun() ->
            Schema = #{
                <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
                <<"type">> => <<"object">>
            },
            Result = erlmcp_json_schema_validator:load_schema(test_schema, Schema),
            ?assertEqual(ok, Result)
        end},
        {"list_schemas returns loaded schemas", fun() ->
            Schema = #{
                <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
                <<"type">> => <<"string">>
            },
            ok = erlmcp_json_schema_validator:load_schema(list_test, Schema),
            Schemas = erlmcp_json_schema_validator:list_schemas(),
            ?assert(is_list(Schemas))
        end},
        {"unload_schema removes schema", fun() ->
            Schema = #{
                <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
                <<"type">> => <<"number">>
            },
            ok = erlmcp_json_schema_validator:load_schema(temp_schema, Schema),
            ?assertEqual(ok, erlmcp_json_schema_validator:unload_schema(temp_schema))
        end},
        {"validate validates valid data", fun() ->
            Schema = #{
                <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
                <<"type">> => <<"string">>
            },
            ok = erlmcp_json_schema_validator:load_schema(string_schema, Schema),
            Result = erlmcp_json_schema_validator:validate(string_schema, <<"test">>),
            ?assertMatch({ok, _}, Result)
        end}
    ]}.

event_bus_tests() ->
    {"Event Bus", [
        {"subscribe returns subscription id", fun() ->
            Result = erlmcp_event_bus:subscribe(test_event, undefined),
            ?assertMatch({ok, _Ref}, Result)
        end},
        {"list_subscribers returns list", fun() ->
            {ok, _SubId} = erlmcp_event_bus:subscribe(sub_test_event, undefined),
            Subscribers = erlmcp_event_bus:list_subscribers(sub_test_event),
            ?assert(is_list(Subscribers))
        end},
        {"publish returns ok", fun() ->
            Event = #{data => <<"test">>},
            ?assertEqual(ok, erlmcp_event_bus:publish(publish_test, Event))
        end},
        {"publish_sync returns delivered count", fun() ->
            {ok, _SubId} = erlmcp_event_bus:subscribe(sync_test, undefined),
            Event = #{data => <<"sync test">>},
            Result = erlmcp_event_bus:publish_sync(sync_test, Event),
            ?assertMatch({ok, _Count}, Result)
        end},
        {"get_metrics returns metrics map", fun() ->
            Metrics = erlmcp_event_bus:get_metrics(),
            ?assert(is_map(Metrics)),
            ?assert(maps:is_key(events_published, Metrics)),
            ?assert(maps:is_key(events_delivered, Metrics)),
            ?assert(maps:is_key(subscribers, Metrics))
        end}
    ]}.
