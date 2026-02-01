-module(erlmcp_schema_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%% Helper record to access schema fields in tests
-record(schema, {name, version, type, definition, created_at}).

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

schema_registry_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Schema registration", fun test_schema_registration/0},
         {"Schema retrieval", fun test_schema_retrieval/0},
         {"Schema versioning", fun test_schema_versioning/0},
         {"Schema validation", fun test_schema_validation/0},
         {"Compatibility checking", fun test_compatibility_checking/0},
         {"Schema deletion", fun test_schema_deletion/0},
         {"Concurrent operations", fun test_concurrent_operations/0}]
     end}.

setup() ->
    % Start required applications
    application:ensure_all_started(jsx),
    application:ensure_all_started(jesse),
    application:ensure_all_started(poolboy),
    application:ensure_all_started(gproc),

    % Start schema registry
    {ok, Pid} = erlmcp_schema_registry:start_link(),
    Pid.

cleanup(_Pid) ->
    erlmcp_schema_registry:stop(),
    ok.

%%====================================================================
%% Tests
%%====================================================================

test_schema_registration() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"name">> => #{<<"type">> => <<"string">>},
                <<"age">> => #{<<"type">> => <<"integer">>}},
          <<"required">> => [<<"name">>]},

    % Register schema
    ?assertEqual(ok, erlmcp_schema_registry:register(<<"user">>, {1, 0, 0}, tool, Schema)),

    % Verify it exists
    {ok, Retrieved} = erlmcp_schema_registry:get(<<"user">>, {1, 0, 0}),
    ?assertEqual(<<"user">>, Retrieved#schema.name),
    ?assertEqual({1, 0, 0}, Retrieved#schema.version),
    ?assertEqual(tool, Retrieved#schema.type).

test_schema_retrieval() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> => #{<<"id">> => #{<<"type">> => <<"integer">>}}},

    % Register schema
    erlmcp_schema_registry:register(<<"product">>, {1, 0, 0}, resource, Schema),

    % Get by name and version
    {ok, Retrieved} = erlmcp_schema_registry:get(<<"product">>, {1, 0, 0}),
    ?assertEqual(resource, Retrieved#schema.type),

    % Get latest version
    {ok, Latest} = erlmcp_schema_registry:get_latest(<<"product">>),
    ?assertEqual({1, 0, 0}, Latest#schema.version),

    % Get non-existent schema
    ?assertEqual({error, not_found}, erlmcp_schema_registry:get(<<"nonexistent">>, {1, 0, 0})).

test_schema_versioning() ->
    BaseSchema =
        #{<<"type">> => <<"object">>,
          <<"properties">> => #{<<"name">> => #{<<"type">> => <<"string">>}},
          <<"required">> => [<<"name">>]},

    % Register v1.0.0
    erlmcp_schema_registry:register(<<"person">>, {1, 0, 0}, BaseSchema),

    % Register v1.1.0 (compatible change)
    V110Schema =
        maps:put(<<"properties">>,
                 maps:merge(
                     maps:get(<<"properties">>, BaseSchema),
                     #{<<"email">> => #{<<"type">> => <<"string">>}}),
                 BaseSchema),
    erlmcp_schema_registry:register(<<"person">>, {1, 1, 0}, V110Schema),

    % Register v2.0.0 (breaking change)
    V200Schema = maps:put(<<"required">>, [<<"name">>, <<"email">>], V110Schema),
    erlmcp_schema_registry:register(<<"person">>, {2, 0, 0}, V200Schema),

    % List all versions
    {ok, Versions} = erlmcp_schema_registry:list_versions(<<"person">>),
    ?assertEqual(3, length(Versions)),
    ?assert(lists:member({1, 0, 0}, Versions)),
    ?assert(lists:member({1, 1, 0}, Versions)),
    ?assert(lists:member({2, 0, 0}, Versions)),

    % Get latest should return v2.0.0
    {ok, Latest} = erlmcp_schema_registry:get_latest(<<"person">>),
    ?assertEqual({2, 0, 0}, Latest#schema.version).

test_schema_validation() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> =>
              #{<<"name">> => #{<<"type">> => <<"string">>},
                <<"age">> =>
                    #{<<"type">> => <<"integer">>,
                      <<"minimum">> => 0,
                      <<"maximum">> => 120}},
          <<"required">> => [<<"name">>]},

    erlmcp_schema_registry:register(<<"user_validation">>, {1, 0, 0}, Schema),

    % Valid data
    ValidData = #{<<"name">> => <<"John">>, <<"age">> => 30},
    ?assertEqual(ok, erlmcp_schema_registry:validate(<<"user_validation">>, {1, 0, 0}, ValidData)),

    % Missing required field
    InvalidData1 = #{<<"age">> => 30},
    Result1 = erlmcp_schema_registry:validate(<<"user_validation">>, {1, 0, 0}, InvalidData1),
    ?assertMatch({error, _}, Result1),

    % Invalid type
    InvalidData2 = #{<<"name">> => <<"Jane">>, <<"age">> => <<"thirty">>},
    Result2 = erlmcp_schema_registry:validate(<<"user_validation">>, {1, 0, 0}, InvalidData2),
    ?assertMatch({error, _}, Result2),

    % Out of range
    InvalidData3 = #{<<"name">> => <<"Bob">>, <<"age">> => 150},
    Result3 = erlmcp_schema_registry:validate(<<"user_validation">>, {1, 0, 0}, InvalidData3),
    ?assertMatch({error, _}, Result3).

test_compatibility_checking() ->
    V1Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> => #{<<"name">> => #{<<"type">> => <<"string">>}},
          <<"required">> => [<<"name">>]},

    % v2 adds optional field (compatible)
    V2Schema =
        maps:put(<<"properties">>,
                 maps:merge(
                     maps:get(<<"properties">>, V1Schema),
                     #{<<"email">> => #{<<"type">> => <<"string">>}}),
                 V1Schema),

    % v3 makes email required (breaking change)
    V3Schema = maps:put(<<"required">>, [<<"name">>, <<"email">>], V2Schema),

    erlmcp_schema_registry:register(<<"compat_test">>, {1, 0, 0}, V1Schema),
    erlmcp_schema_registry:register(<<"compat_test">>, {2, 0, 0}, V2Schema),
    erlmcp_schema_registry:register(<<"compat_test">>, {3, 0, 0}, V3Schema),

    % v1 -> v2 should be compatible
    {ok, compatible} =
        erlmcp_schema_registry:check_compatibility(<<"compat_test">>, {1, 0, 0}, {2, 0, 0}),

    % v2 -> v3 should be breaking change
    {ok, breaking_change} =
        erlmcp_schema_registry:check_compatibility(<<"compat_test">>, {2, 0, 0}, {3, 0, 0}).

test_schema_deletion() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> => #{<<"id">> => #{<<"type">> => <<"integer">>}}},

    erlmcp_schema_registry:register(<<"deletable">>, {1, 0, 0}, Schema),

    % Verify it exists
    ?assertMatch({ok, _}, erlmcp_schema_registry:get(<<"deletable">>, {1, 0, 0})),

    % Delete it
    ?assertEqual(ok, erlmcp_schema_registry:delete(<<"deletable">>, {1, 0, 0})),

    % Verify it's gone
    ?assertEqual({error, not_found}, erlmcp_schema_registry:get(<<"deletable">>, {1, 0, 0})),

    % Delete non-existent schema
    ?assertEqual({error, not_found}, erlmcp_schema_registry:delete(<<"nonexistent">>, {1, 0, 0})).

test_concurrent_operations() ->
    Schema =
        #{<<"type">> => <<"object">>,
          <<"properties">> => #{<<"value">> => #{<<"type">> => <<"integer">>}}},

    erlmcp_schema_registry:register(<<"concurrent">>, {1, 0, 0}, Schema),

    % Spawn multiple processes doing concurrent validations
    NumProcesses = 100,
    Self = self(),

    _Processes =
        lists:map(fun(N) ->
                     spawn(fun() ->
                              ValidData = #{<<"value">> => N},
                              Result =
                                  erlmcp_schema_registry:validate(<<"concurrent">>,
                                                                  {1, 0, 0},
                                                                  ValidData),
                              Self ! {result, N, Result}
                           end)
                  end,
                  lists:seq(1, NumProcesses)),

    % Collect results
    Results =
        lists:map(fun(N) ->
                     receive
                         {result, N, Result} ->
                             Result
                     after 5000 ->
                         timeout
                     end
                  end,
                  lists:seq(1, NumProcesses)),

    % All should succeed
    ?assertEqual(NumProcesses, length(lists:filter(fun(R) -> R =:= ok end, Results))).

%%====================================================================
%% Utility Functions
%%====================================================================
