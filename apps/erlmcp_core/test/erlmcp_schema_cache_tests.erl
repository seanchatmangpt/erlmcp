-module(erlmcp_schema_cache_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    % Start the schema cache server
    {ok, Pid} = erlmcp_schema_cache:start_link(),
    Pid.

cleanup(Pid) ->
    % Stop the server and clear cache
    gen_server:stop(Pid),
    erlmcp_schema_cache:clear_all().

schema_cache_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun test_cache_and_get_schema/1,
        fun test_cache_multiple_schemas/1,
        fun test_has_schema/1,
        fun test_validate_object/1,
        fun test_validate_array/1,
        fun test_validate_primitives/1,
        fun test_validate_type_mismatch/1,
        fun test_invalidate_schema/1,
        fun test_clear_all/1,
        fun test_list_schemas/1,
        fun test_get_stats/1,
        fun test_cache_hit_miss_tracking/1,
        fun test_validation_counter/1,
        fun test_missing_schema/1,
        fun test_invalid_schema_structure/1,
        fun test_concurrent_access/1,
        fun test_zero_copy_schema_access/1
    ]}.

%%====================================================================
%% Test Cases
%%====================================================================

test_cache_and_get_schema(_Pid) ->
    ?_test(begin
        Schema = #{
            <<"type">> => <<"object">>,
            <<"properties">> =>
                #{
                    <<"name">> => #{<<"type">> => <<"string">>}
                }
        },

        ok = erlmcp_schema_cache:cache_schema(test_schema, Schema),
        {ok, Retrieved} = erlmcp_schema_cache:get_schema(test_schema),
        ?assertEqual(Schema, Retrieved)
    end).

test_cache_multiple_schemas(_Pid) ->
    ?_test(begin
        Schema1 = #{<<"type">> => <<"object">>},
        Schema2 = #{<<"type">> => <<"array">>},
        Schema3 = #{<<"type">> => <<"string">>},

        ok = erlmcp_schema_cache:cache_schema(schema1, Schema1),
        ok = erlmcp_schema_cache:cache_schema(schema2, Schema2),
        ok = erlmcp_schema_cache:cache_schema(schema3, Schema3),

        {ok, S1} = erlmcp_schema_cache:get_schema(schema1),
        {ok, S2} = erlmcp_schema_cache:get_schema(schema2),
        {ok, S3} = erlmcp_schema_cache:get_schema(schema3),

        ?assertEqual(Schema1, S1),
        ?assertEqual(Schema2, S2),
        ?assertEqual(Schema3, S3)
    end).

test_has_schema(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(exists, Schema),

        ?assert(erlmcp_schema_cache:has_schema(exists)),
        ?assertNot(erlmcp_schema_cache:has_schema(does_not_exist))
    end).

test_validate_object(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(object_schema, Schema),

        % Valid object
        ?assertEqual(ok, erlmcp_schema_cache:validate(object_schema, #{key => value})),

        % Invalid (not an object)
        ?assertMatch(
            {error, {type_mismatch, expected_object}},
            erlmcp_schema_cache:validate(object_schema, [1, 2, 3])
        )
    end).

test_validate_array(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"array">>},
        ok = erlmcp_schema_cache:cache_schema(array_schema, Schema),

        % Valid array
        ?assertEqual(ok, erlmcp_schema_cache:validate(array_schema, [1, 2, 3])),

        % Invalid (not an array)
        ?assertMatch(
            {error, {type_mismatch, expected_array}},
            erlmcp_schema_cache:validate(array_schema, #{})
        )
    end).

test_validate_primitives(_Pid) ->
    ?_test(begin
        % String schema
        StringSchema = #{<<"type">> => <<"string">>},
        ok = erlmcp_schema_cache:cache_schema(string_schema, StringSchema),
        ?assertEqual(ok, erlmcp_schema_cache:validate(string_schema, <<"test">>)),
        ?assertMatch(
            {error, {type_mismatch, expected_string}},
            erlmcp_schema_cache:validate(string_schema, 123)
        ),

        % Number schema
        NumberSchema = #{<<"type">> => <<"number">>},
        ok = erlmcp_schema_cache:cache_schema(number_schema, NumberSchema),
        ?assertEqual(ok, erlmcp_schema_cache:validate(number_schema, 42)),
        ?assertEqual(ok, erlmcp_schema_cache:validate(number_schema, 3.14)),

        % Integer schema
        IntSchema = #{<<"type">> => <<"integer">>},
        ok = erlmcp_schema_cache:cache_schema(int_schema, IntSchema),
        ?assertEqual(ok, erlmcp_schema_cache:validate(int_schema, 42)),
        ?assertMatch(
            {error, {type_mismatch, expected_integer}},
            erlmcp_schema_cache:validate(int_schema, 3.14)
        ),

        % Boolean schema
        BoolSchema = #{<<"type">> => <<"boolean">>},
        ok = erlmcp_schema_cache:cache_schema(bool_schema, BoolSchema),
        ?assertEqual(ok, erlmcp_schema_cache:validate(bool_schema, true)),
        ?assertEqual(ok, erlmcp_schema_cache:validate(bool_schema, false))
    end).

test_validate_type_mismatch(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"string">>},
        ok = erlmcp_schema_cache:cache_schema(strict_schema, Schema),

        ?assertMatch(
            {error, {type_mismatch, expected_string}},
            erlmcp_schema_cache:validate(strict_schema, 123)
        ),
        ?assertMatch(
            {error, {type_mismatch, expected_string}},
            erlmcp_schema_cache:validate(strict_schema, #{})
        )
    end).

test_invalidate_schema(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(temp_schema, Schema),

        ?assert(erlmcp_schema_cache:has_schema(temp_schema)),

        ok = erlmcp_schema_cache:invalidate(temp_schema),

        ?assertNot(erlmcp_schema_cache:has_schema(temp_schema)),
        ?assertEqual({error, not_found}, erlmcp_schema_cache:get_schema(temp_schema))
    end).

test_clear_all(_Pid) ->
    ?_test(begin
        Schema1 = #{<<"type">> => <<"object">>},
        Schema2 = #{<<"type">> => <<"array">>},

        ok = erlmcp_schema_cache:cache_schema(schema_a, Schema1),
        ok = erlmcp_schema_cache:cache_schema(schema_b, Schema2),

        ?assert(erlmcp_schema_cache:has_schema(schema_a)),
        ?assert(erlmcp_schema_cache:has_schema(schema_b)),

        ok = erlmcp_schema_cache:clear_all(),

        ?assertNot(erlmcp_schema_cache:has_schema(schema_a)),
        ?assertNot(erlmcp_schema_cache:has_schema(schema_b))
    end).

test_list_schemas(_Pid) ->
    ?_test(begin
        Schema1 = #{<<"type">> => <<"object">>},
        Schema2 = #{<<"type">> => <<"array">>},
        Schema3 = #{<<"type">> => <<"string">>},

        ok = erlmcp_schema_cache:cache_schema(list_test_1, Schema1),
        ok = erlmcp_schema_cache:cache_schema(list_test_2, Schema2),
        ok = erlmcp_schema_cache:cache_schema(list_test_3, Schema3),

        Schemas = erlmcp_schema_cache:list_schemas(),
        ?assert(lists:member(list_test_1, Schemas)),
        ?assert(lists:member(list_test_2, Schemas)),
        ?assert(lists:member(list_test_3, Schemas))
    end).

test_get_stats(_Pid) ->
    ?_test(begin
        Stats1 = erlmcp_schema_cache:get_stats(),
        ?assertEqual(0, maps:get(cache_hits, Stats1)),
        ?assertEqual(0, maps:get(cache_misses, Stats1)),
        ?assertEqual(0, maps:get(validations, Stats1)),

        % Cache a schema and access it
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(stats_test, Schema),

        % Trigger a hit
        {ok, _} = erlmcp_schema_cache:get_schema(stats_test),

        % Trigger a miss
        {error, not_found} = erlmcp_schema_cache:get_schema(nonexistent),

        % Trigger validations
        ok = erlmcp_schema_cache:validate(stats_test, #{}),
        ok = erlmcp_schema_cache:validate(stats_test, #{}),

        % Check stats
        timer:sleep(100),
        Stats2 = erlmcp_schema_cache:get_stats(),
        ?assert(maps:get(cache_hits, Stats2) >= 1),
        ?assert(maps:get(cache_misses, Stats2) >= 1),
        ?assert(maps:get(validations, Stats2) >= 2)
    end).

test_cache_hit_miss_tracking(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(hit_miss_test, Schema),

        % Multiple hits
        {ok, _} = erlmcp_schema_cache:get_schema(hit_miss_test),
        {ok, _} = erlmcp_schema_cache:get_schema(hit_miss_test),
        {ok, _} = erlmcp_schema_cache:get_schema(hit_miss_test),

        % Multiple misses
        {error, not_found} = erlmcp_schema_cache:get_schema(miss1),
        {error, not_found} = erlmcp_schema_cache:get_schema(miss2),

        timer:sleep(100),
        Stats = erlmcp_schema_cache:get_stats(),
        HitRate = maps:get(hit_rate, Stats),

        % Should have hit rate of 3/5 = 0.6
        ?assert(HitRate >= 0.5),
        ?assert(HitRate =< 1.0)
    end).

test_validation_counter(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(validation_test, Schema),

        % Perform multiple validations
        ok = erlmcp_schema_cache:validate(validation_test, #{}),
        ok = erlmcp_schema_cache:validate(validation_test, #{key => value}),
        {error, _} = erlmcp_schema_cache:validate(validation_test, []),

        timer:sleep(100),
        Stats = erlmcp_schema_cache:get_stats(),
        ?assert(maps:get(validations, Stats) >= 3)
    end).

test_missing_schema(_Pid) ->
    ?_test(begin
        ?assertEqual({error, not_found}, erlmcp_schema_cache:get_schema(missing)),
        ?assertEqual({error, not_found}, erlmcp_schema_cache:validate(missing, #{}))
    end).

test_invalid_schema_structure(_Pid) ->
    ?_test(begin
        % Schema without required "type" field
        InvalidSchema = #{<<"properties">> => #{}},

        ?assertMatch(
            {error, missing_required_fields},
            erlmcp_schema_cache:cache_schema(invalid, InvalidSchema)
        )
    end).

test_concurrent_access(_Pid) ->
    ?_test(begin
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(concurrent_test, Schema),

        % Spawn multiple concurrent readers
        Parent = self(),
        Readers = [
            spawn_link(fun() ->
                Result = erlmcp_schema_cache:get_schema(concurrent_test),
                Parent ! {read, self(), Result}
            end)
         || _ <- lists:seq(1, 50)
        ],

        % Collect results
        Results = [
            receive
                {read, Pid, Result} -> Result
            after 5000 -> timeout
            end
         || Pid <- Readers
        ],

        % All should succeed
        ?assertEqual(50, length(Results)),
        ?assert(lists:all(fun({ok, S}) -> S =:= Schema; (_) -> false end, Results))
    end).

test_zero_copy_schema_access(_Pid) ->
    ?_test(begin
        % Create a large schema
        LargeSchema = #{
            <<"type">> => <<"object">>,
            <<"properties">> =>
                maps:from_list([
                    {
                        list_to_binary("field_" ++ integer_to_list(N)),
                        #{<<"type">> => <<"string">>}
                    }
                 || N <- lists:seq(1, 1000)
                ])
        },

        ok = erlmcp_schema_cache:cache_schema(large_schema, LargeSchema),

        % Multiple reads should be zero-copy
        {ok, Schema1} = erlmcp_schema_cache:get_schema(large_schema),
        {ok, Schema2} = erlmcp_schema_cache:get_schema(large_schema),
        {ok, Schema3} = erlmcp_schema_cache:get_schema(large_schema),

        ?assertEqual(Schema1, Schema2),
        ?assertEqual(Schema2, Schema3),
        ?assertEqual(LargeSchema, Schema1)
    end).

%%====================================================================
%% Performance Benchmark Tests
%%====================================================================

performance_test_() ->
    {timeout, 30, fun test_schema_lookup_performance/0}.

test_schema_lookup_performance() ->
    {ok, Pid} = erlmcp_schema_cache:start_link(),
    try
        Schema = #{<<"type">> => <<"object">>},
        ok = erlmcp_schema_cache:cache_schema(perf_test, Schema),

        % Benchmark 1M lookups
        Iterations = 1000000,
        StartTime = erlang:monotonic_time(microsecond),

        [erlmcp_schema_cache:get_schema(perf_test) || _ <- lists:seq(1, Iterations)],

        EndTime = erlang:monotonic_time(microsecond),
        ElapsedUs = EndTime - StartTime,
        LookupsPerSec = (Iterations * 1000000) div ElapsedUs,

        logger:info("persistent_term schema lookup performance: ~w lookups/sec (~.2f μs/lookup)", [
            LookupsPerSec, ElapsedUs / Iterations
        ]),

        % Should achieve >1M lookups/sec
        ?assert(LookupsPerSec > 500000)
    after
        gen_server:stop(Pid),
        erlmcp_schema_cache:clear_all()
    end.
