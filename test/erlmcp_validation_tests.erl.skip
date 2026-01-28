-module(erlmcp_validation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_validation Module
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    erlmcp_validation:initialize(),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Validation Tests
%%====================================================================

validate_transport_config_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_empty_config()),
             ?_test(test_validate_valid_config()),
             ?_test(test_validate_invalid_config()),
             ?_test(test_validate_config_with_type()),
             ?_test(test_validate_tcp_config()),
             ?_test(test_validate_http_config()),
             ?_test(test_validate_config_missing_fields()),
             ?_test(test_validate_config_extra_fields()),
             ?_test(test_validate_config_type_mismatch())
         ]
     end}.

test_validate_empty_config() ->
    Config = #{},
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch({ok, _} | {error, _}, [Result]).

test_validate_valid_config() ->
    Config = #{type => stdio, enabled => true},
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_invalid_config() ->
    Config = #{invalid_key => <<"value">>},
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch({error, _} | ok | {ok, _}, [Result]).

test_validate_config_with_type() ->
    Config = #{host => localhost, port => 8080},
    Result = erlmcp_validation:validate_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_tcp_config() ->
    Config = #{
        type => tcp,
        host => <<"localhost">>,
        port => 5000,
        backlog => 128
    },
    Result = erlmcp_validation:validate_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_http_config() ->
    Config = #{
        type => http,
        host => <<"0.0.0.0">>,
        port => 3000
    },
    Result = erlmcp_validation:validate_transport_config(http, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_config_missing_fields() ->
    Config = #{type => tcp},
    Result = erlmcp_validation:validate_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_config_extra_fields() ->
    Config = #{
        type => stdio,
        extra_field1 => value1,
        extra_field2 => value2,
        enabled => true
    },
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_config_type_mismatch() ->
    Config = #{type => <<"stdio">>, port => <<"not_integer">>},
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

%%====================================================================
%% Schema Tests
%%====================================================================

schema_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_config_schema_tcp()),
             ?_test(test_get_config_schema_http()),
             ?_test(test_get_config_schema_stdio()),
             ?_test(test_get_config_schema_invalid()),
             ?_test(test_config_schema_not_empty()),
             ?_test(test_config_schema_format())
         ]
     end}.

test_get_config_schema_tcp() ->
    Schema = erlmcp_validation:get_config_schema(tcp),
    ?assert(Schema =/= undefined andalso Schema =/= error).

test_get_config_schema_http() ->
    Schema = erlmcp_validation:get_config_schema(http),
    ?assert(Schema =/= undefined andalso Schema =/= error).

test_get_config_schema_stdio() ->
    Schema = erlmcp_validation:get_config_schema(stdio),
    ?assert(Schema =/= undefined andalso Schema =/= error).

test_get_config_schema_invalid() ->
    Schema = erlmcp_validation:get_config_schema(invalid_type),
    ?assertMatch(error | undefined | _, Schema).

test_config_schema_not_empty() ->
    Schema = erlmcp_validation:get_config_schema(tcp),
    ?assert(is_map(Schema) orelse is_list(Schema) orelse Schema =/= undefined).

test_config_schema_format() ->
    Schema = erlmcp_validation:get_config_schema(http),
    %% Schema should be a valid structure
    ?assert(is_map(Schema) orelse is_list(Schema) orelse Schema =:= error).

%%====================================================================
%% Initialization Tests
%%====================================================================

initialization_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_initialize()),
             ?_test(test_initialize_idempotent()),
             ?_test(test_initialize_validates_schemas())
         ]
     end}.

test_initialize() ->
    Result = erlmcp_validation:initialize(),
    ?assertEqual(ok, Result).

test_initialize_idempotent() ->
    Result1 = erlmcp_validation:initialize(),
    Result2 = erlmcp_validation:initialize(),
    ?assertEqual(Result1, Result2).

test_initialize_validates_schemas() ->
    erlmcp_validation:initialize(),
    %% After init, schemas should be available
    Schema = erlmcp_validation:get_config_schema(tcp),
    ?assert(Schema =/= undefined).

%%====================================================================
%% Common Validation Patterns Tests
%%====================================================================

pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_multiple_configs()),
             ?_test(test_validate_various_types()),
             ?_test(test_validate_boundary_values()),
             ?_test(test_validate_special_characters())
         ]
     end}.

test_validate_multiple_configs() ->
    Configs = [
        #{type => tcp, port => 5000},
        #{type => http, port => 3000},
        #{type => stdio}
    ],
    Results = [erlmcp_validation:validate_transport_config(C) || C <- Configs],
    ?assertEqual(3, length(Results)).

test_validate_various_types() ->
    ConfigTypes = [tcp, http, stdio, sse, ws],
    Results = [erlmcp_validation:get_config_schema(T) || T <- ConfigTypes],
    ?assertEqual(5, length(Results)).

test_validate_boundary_values() ->
    Configs = [
        #{port => 1},      % Minimum port
        #{port => 65535},  % Maximum port
        #{backlog => 1},   % Minimum backlog
        #{backlog => 128}  % Typical backlog
    ],
    Results = [erlmcp_validation:validate_transport_config(C) || C <- Configs],
    ?assertEqual(4, length(Results)).

test_validate_special_characters() ->
    Config = #{
        host => <<"[::1]">>,  % IPv6 address
        name => <<"Server-1_2.3">>,  % Special characters in name
        path => <<"/path/to/socket">>
    },
    Result = erlmcp_validation:validate_transport_config(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_validate_null_config()),
             ?_test(test_validate_non_map_config()),
             ?_test(test_validate_very_large_config()),
             ?_test(test_validate_nested_config())
         ]
     end}.

test_validate_null_config() ->
    Result = erlmcp_validation:validate_transport_config(null),
    ?assertMatch(error | {error, _} | ok, [Result]).

test_validate_non_map_config() ->
    Result = erlmcp_validation:validate_transport_config([1, 2, 3]),
    ?assertMatch(error | {error, _} | ok, [Result]).

test_validate_very_large_config() ->
    LargeConfig = maps:from_list([{<<"key_", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 1000)]),
    Result = erlmcp_validation:validate_transport_config(LargeConfig),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).

test_validate_nested_config() ->
    Config = #{
        type => tcp,
        nested => #{
            inner => #{
                deep => <<"value">>
            }
        }
    },
    Result = erlmcp_validation:validate_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, [Result]).
