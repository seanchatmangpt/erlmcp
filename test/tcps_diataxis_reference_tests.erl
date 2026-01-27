%%%-------------------------------------------------------------------
%%% @doc Unit tests for tcps_diataxis_reference module
%%% Comprehensive test coverage for reference documentation retrieval.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_reference_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

reference_test_() ->
    [
     {"Get reference by ID", fun test_get_reference/0},
     {"Get non-existent reference", fun test_get_nonexistent_reference/0},
     {"List all references", fun test_list_references/0},
     {"List references by type", fun test_list_by_type/0},
     {"Search references", fun test_search_references/0},
     {"Generate API reference", fun test_generate_api_reference/0},
     {"Generate config reference", fun test_generate_config_reference/0},
     {"Get module reference", fun test_get_module_reference/0},
     {"Get function reference", fun test_get_function_reference/0},
     {"Validate reference structure", fun test_validate_reference/0},
     {"Reference has all required keys", fun test_reference_keys/0},
     {"Function references well-formed", fun test_function_references/0},
     {"Type coverage", fun test_type_coverage/0}
    ].

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_get_reference() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    ?assertEqual(<<"tcps_work_order API Reference">>, maps:get(title, Ref)),
    ?assertEqual(api, maps:get(type, Ref)),
    ?assertEqual(tcps_work_order, maps:get(module, Ref)),
    ?assert(maps:is_key(functions, Ref)).

test_get_nonexistent_reference() ->
    ?assertEqual({error, not_found}, tcps_diataxis_reference:get_reference(<<"nonexistent">>)).

test_list_references() ->
    References = tcps_diataxis_reference:list_references(),
    ?assert(length(References) >= 5),
    ?assert(lists:all(fun(R) -> maps:is_key(id, R) end, References)),
    ?assert(lists:all(fun(R) -> maps:is_key(title, R) end, References)),
    ?assert(lists:all(fun(R) -> maps:is_key(type, R) end, References)).

test_list_by_type() ->
    ApiRefs = tcps_diataxis_reference:list_by_type(api),
    ProtocolRefs = tcps_diataxis_reference:list_by_type(protocol),
    FormatRefs = tcps_diataxis_reference:list_by_type(format),
    CliRefs = tcps_diataxis_reference:list_by_type(cli),

    ?assertEqual(2, length(ApiRefs)),
    ?assertEqual(1, length(ProtocolRefs)),
    ?assertEqual(1, length(FormatRefs)),
    ?assertEqual(1, length(CliRefs)),

    ?assert(lists:all(fun(R) -> maps:get(type, R) =:= api end, ApiRefs)),
    ?assert(lists:all(fun(R) -> maps:get(type, R) =:= protocol end, ProtocolRefs)).

test_search_references() ->
    Results1 = tcps_diataxis_reference:search_references(<<"quality">>),
    ?assert(length(Results1) >= 1),

    Results2 = tcps_diataxis_reference:search_references(<<"mcp">>),
    ?assert(length(Results2) >= 1),

    Results3 = tcps_diataxis_reference:search_references(<<"work order">>),
    ?assert(length(Results3) >= 1),

    % Case insensitive
    Results4 = tcps_diataxis_reference:search_references(<<"QUALITY">>),
    ?assertEqual(length(Results1), length(Results4)).

test_generate_api_reference() ->
    {ok, Ref} = tcps_diataxis_reference:generate_api_reference(lists),
    ?assertEqual(api, maps:get(type, Ref)),
    ?assertEqual(<<"lists">>, maps:get(id, Ref)),
    ?assert(maps:is_key(functions, Ref)),

    Functions = maps:get(functions, Ref),
    ?assert(length(Functions) > 0),

    % Check function structure
    FirstFunc = hd(Functions),
    ?assert(maps:is_key(name, FirstFunc)),
    ?assert(maps:is_key(arity, FirstFunc)),
    ?assert(maps:is_key(signature, FirstFunc)).

test_generate_config_reference() ->
    {ok, Ref} = tcps_diataxis_reference:generate_config_reference(),
    ?assertEqual(<<"TCPS Configuration Reference">>, maps:get(title, Ref)),
    ?assertEqual(config, maps:get(type, Ref)),
    ?assert(maps:is_key(examples, Ref)),

    Examples = maps:get(examples, Ref),
    ?assert(length(Examples) > 0).

test_get_module_reference() ->
    {ok, Ref} = tcps_diataxis_reference:get_module_reference(tcps_work_order),
    ?assertEqual(<<"tcps_work_order API Reference">>, maps:get(title, Ref)),

    % Try dynamic generation for unlisted module
    {ok, ListsRef} = tcps_diataxis_reference:get_module_reference(lists),
    ?assertEqual(api, maps:get(type, ListsRef)).

test_get_function_reference() ->
    {ok, FuncRef} = tcps_diataxis_reference:get_function_reference(tcps_work_order, {create, 1}),
    ?assertEqual(<<"create">>, maps:get(name, FuncRef)),
    ?assertEqual(1, maps:get(arity, FuncRef)),
    ?assert(maps:is_key(signature, FuncRef)),
    ?assert(maps:is_key(parameters, FuncRef)),
    ?assert(maps:is_key(returns, FuncRef)).

test_validate_reference() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    ?assertEqual(ok, tcps_diataxis_reference:validate_reference(Ref)),

    InvalidRef = #{
        id => <<"test">>,
        title => <<"Test">>
        % Missing required keys
    },
    ?assertEqual({error, missing_required_keys},
                 tcps_diataxis_reference:validate_reference(InvalidRef)).

test_reference_keys() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    RequiredKeys = [id, type, title, description, version],
    lists:foreach(
        fun(Key) ->
            ?assert(maps:is_key(Key, Ref))
        end,
        RequiredKeys
    ).

test_function_references() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    Functions = maps:get(functions, Ref),

    ?assert(length(Functions) >= 3),

    lists:foreach(
        fun(Func) ->
            ?assert(maps:is_key(name, Func)),
            ?assert(maps:is_key(arity, Func)),
            ?assert(maps:is_key(signature, Func)),
            ?assert(maps:is_key(parameters, Func)),
            ?assert(maps:is_key(returns, Func)),
            ?assert(maps:is_key(description, Func)),
            ?assert(maps:is_key(examples, Func)),
            ?assert(maps:is_key(since, Func))
        end,
        Functions
    ).

test_type_coverage() ->
    AllTypes = [api, protocol, format, cli, config],

    References = tcps_diataxis_reference:list_references(),

    lists:foreach(
        fun(Type) ->
            WithType = lists:filter(
                fun(R) -> maps:get(type, R) =:= Type end,
                References
            ),
            % At least some references of each type exist (except config which is generated)
            if
                Type =:= config -> ok;
                true -> ?assert(length(WithType) > 0)
            end
        end,
        AllTypes
    ).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    [
     {"Function parameter validation", fun test_function_parameters/0},
     {"Examples are executable", fun test_examples_format/0},
     {"Cross-references", fun test_cross_references/0}
    ].

test_function_parameters() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    Functions = maps:get(functions, Ref),

    CreateFunc = lists:keyfind(<<"create">>, 1, [{maps:get(name, F), F} || F <- Functions]),
    {_, Create} = CreateFunc,

    Params = maps:get(parameters, Create),
    ?assert(length(Params) > 0),

    % Check parameter structure
    FirstParam = hd(Params),
    ?assert(maps:is_key(name, FirstParam)),
    ?assert(maps:is_key(type, FirstParam)),
    ?assert(maps:is_key(required, FirstParam)),
    ?assert(maps:is_key(description, FirstParam)).

test_examples_format() ->
    {ok, Ref} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    Examples = maps:get(examples, Ref),

    ?assert(length(Examples) > 0),
    ?assert(lists:all(fun(E) -> is_binary(E) andalso byte_size(E) > 0 end, Examples)).

test_cross_references() ->
    {ok, WORef} = tcps_diataxis_reference:get_reference(<<"tcps_work_order">>),
    SeeAlso = maps:get(see_also, WORef),

    % All cross-referenced items should exist
    lists:foreach(
        fun(RefId) ->
            ?assertMatch({ok, _}, tcps_diataxis_reference:get_reference(RefId))
        end,
        SeeAlso
    ).
