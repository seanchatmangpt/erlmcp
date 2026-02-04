-module(erlmcp_json_schema_validator_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

setup() ->
    case whereis(erlmcp_json_schema_validator) of
        undefined -> {ok, Pid} = erlmcp_json_schema_validator:start_link(), Pid;
        Pid -> Pid
    end.

cleanup(_Pid) ->
    ok.

%%%===================================================================
%%% Test Generators
%%%===================================================================

json_schema_validator_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun compile_schema_valid_test/0,
      fun compile_schema_invalid_test/0,
      fun load_schema_test/0,
      fun unload_schema_test/0,
      fun list_schemas_test/0,
      fun validate_valid_data_test/0,
      fun validate_invalid_data_test/0
     ]}.

%%%===================================================================
%%% Individual Tests
%%%===================================================================

compile_schema_valid_test() ->
    {"compile_schema compiles valid schema", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"name">> => #{<<"type">> => <<"string">>},
                <<"age">> => #{<<"type">> => <<"integer">>}
            }
        },
        Result = erlmcp_json_schema_validator:compile_schema(Schema),
        ?assertMatch({ok, _}, Result)
    end}.

compile_schema_invalid_test() ->
    {"compile_schema rejects invalid schema", fun() ->
        Schema = #{<<"invalid">> => <<"schema">>},
        Result = erlmcp_json_schema_validator:compile_schema(Schema),
        ?assertMatch({error, _}, Result)
    end}.

load_schema_test() ->
    {"load_schema loads schema", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"string">>
        },
        Result = erlmcp_json_schema_validator:load_schema(load_test, Schema),
        ?assertEqual(ok, Result)
    end}.

unload_schema_test() ->
    {"unload_schema removes schema", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"number">>
        },
        ok = erlmcp_json_schema_validator:load_schema(unload_test, Schema),
        ?assertEqual(ok, erlmcp_json_schema_validator:unload_schema(unload_test))
    end}.

list_schemas_test() ->
    {"list_schemas returns schema names", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"boolean">>
        },
        ok = erlmcp_json_schema_validator:load_schema(list_test, Schema),
        Schemas = erlmcp_json_schema_validator:list_schemas(),
        ?assert(is_list(Schemas)),
        ?assert(lists:member(list_test, Schemas))
    end}.

validate_valid_data_test() ->
    {"validate accepts valid data", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"string">>
        },
        ok = erlmcp_json_schema_validator:load_schema(string_test, Schema),
        Result = erlmcp_json_schema_validator:validate(string_test, <<"hello">>),
        ?assertMatch({ok, _}, Result)
    end}.

validate_invalid_data_test() ->
    {"validate rejects invalid data", fun() ->
        Schema = #{
            <<"$schema">> => <<"http://json-schema.org/draft-07/schema#">>,
            <<"type">> => <<"number">>
        },
        ok = erlmcp_json_schema_validator:load_schema(number_test, Schema),
        Result = erlmcp_json_schema_validator:validate(number_test, <<"not a number">>),
        ?assertMatch({error, _}, Result)
    end}.
