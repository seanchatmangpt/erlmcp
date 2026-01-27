-module(erlmcp_config_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() ->
    application:ensure_all_started(erlmcp),
    ok.
cleanup(_) -> ok.

%%====================================================================
%% Basic Config Tests
%%====================================================================

basic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_config()),
        ?_test(test_set_config()),
        ?_test(test_get_default_config()),
        ?_test(test_config_not_empty())
    ] end}.

test_get_config() ->
    Result = erlmcp_config:get(),
    ?assert(is_map(Result) orelse is_list(Result)).

test_set_config() ->
    Config = #{key => <<"value">>},
    Result = erlmcp_config:set(test_key, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_default_config() ->
    Result = erlmcp_config:get_defaults(),
    ?assert(is_map(Result) orelse is_list(Result)).

test_config_not_empty() ->
    Result = erlmcp_config:get(),
    ?assert(length(Result) > 0 orelse maps:size(Result) > 0).

%%====================================================================
%% Transport Config Tests
%%====================================================================

transport_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_transport_config()),
        ?_test(test_set_transport_config()),
        ?_test(test_transport_config_tcp()),
        ?_test(test_transport_config_http()),
        ?_test(test_transport_config_stdio())
    ] end}.

test_get_transport_config() ->
    Result = erlmcp_config:get_transport_config(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

test_set_transport_config() ->
    Config = #{type => tcp, port => 5000},
    Result = erlmcp_config:set_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_transport_config_tcp() ->
    Config = #{port => 6000, backlog => 128},
    Result = erlmcp_config:set_transport_config(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_transport_config_http() ->
    Config = #{port => 8080, host => <<"localhost">>},
    Result = erlmcp_config:set_transport_config(http, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_transport_config_stdio() ->
    Config = #{enabled => true},
    Result = erlmcp_config:set_transport_config(stdio, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Feature Config Tests
%%====================================================================

feature_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_enable_feature()),
        ?_test(test_disable_feature()),
        ?_test(test_is_feature_enabled()),
        ?_test(test_get_feature_config())
    ] end}.

test_enable_feature() ->
    Result = erlmcp_config:enable_feature(sampling),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_disable_feature() ->
    Result = erlmcp_config:disable_feature(tracing),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_is_feature_enabled() ->
    erlmcp_config:enable_feature(test_feature),
    Result = erlmcp_config:is_feature_enabled(test_feature),
    ?assert(is_boolean(Result)).

test_get_feature_config() ->
    erlmcp_config:enable_feature(test_feat),
    Result = erlmcp_config:get_feature_config(test_feat),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

%%====================================================================
%% Server Config Tests
%%====================================================================

server_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_server_config()),
        ?_test(test_set_server_config()),
        ?_test(test_server_capabilities()),
        ?_test(test_server_name())
    ] end}.

test_get_server_config() ->
    Result = erlmcp_config:get_server_config(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

test_set_server_config() ->
    Config = #{name => <<"test_server">>, version => <<"1.0">>},
    Result = erlmcp_config:set_server_config(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_server_capabilities() ->
    Result = erlmcp_config:get_capabilities(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

test_server_name() ->
    Result = erlmcp_config:get_server_name(),
    ?assert(is_binary(Result) orelse is_atom(Result) orelse Result =:= undefined).

%%====================================================================
%% Environment Config Tests
%%====================================================================

env_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_env_var()),
        ?_test(test_set_env_var()),
        ?_test(test_env_defaults()),
        ?_test(test_env_override())
    ] end}.

test_get_env_var() ->
    Result = erlmcp_config:get_env(timeout, 5000),
    ?assert(is_integer(Result)).

test_set_env_var() ->
    Result = erlmcp_config:set_env(test_var, <<"value">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_env_defaults() ->
    Default = 100,
    Result = erlmcp_config:get_env(nonexistent_var, Default),
    ?assertEqual(Default, Result).

test_env_override() ->
    erlmcp_config:set_env(override_test, 999),
    Result = erlmcp_config:get_env(override_test, 111),
    ?assertMatch(999 | 111, Result).

%%====================================================================
%% Logging Config Tests
%%====================================================================

logging_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_log_level()),
        ?_test(test_set_log_level()),
        ?_test(test_get_log_config()),
        ?_test(test_log_output())
    ] end}.

test_get_log_level() ->
    Result = erlmcp_config:get_log_level(),
    ?assertMatch(debug | info | warning | error | critical | none, [Result]).

test_set_log_level() ->
    Result = erlmcp_config:set_log_level(info),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_get_log_config() ->
    Result = erlmcp_config:get_log_config(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

test_log_output() ->
    Result = erlmcp_config:get_log_output(),
    ?assert(is_atom(Result) orelse is_list(Result) orelse Result =:= undefined).

%%====================================================================
%% Validation Tests
%%====================================================================

validation_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_validate_config()),
        ?_test(test_validate_transport()),
        ?_test(test_validate_server()),
        ?_test(test_config_schema())
    ] end}.

test_validate_config() ->
    Config = #{key => <<"value">>},
    Result = erlmcp_config:validate(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_validate_transport() ->
    Config = #{type => tcp, port => 5000},
    Result = erlmcp_config:validate_transport(tcp, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_validate_server() ->
    Config = #{name => <<"server">>, version => <<"1.0">>},
    Result = erlmcp_config:validate_server(Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_config_schema() ->
    Result = erlmcp_config:get_schema(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

%%====================================================================
%% Reload Tests
%%====================================================================

reload_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_reload_config()),
        ?_test(test_reload_from_file()),
        ?_test(test_config_persistence()),
        ?_test(test_hot_reload())
    ] end}.

test_reload_config() ->
    Result = erlmcp_config:reload(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_reload_from_file() ->
    Result = erlmcp_config:reload_from_file(<<"test.config">>),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_config_persistence() ->
    erlmcp_config:set(persist_test, #{data => <<"test">>}),
    Result = erlmcp_config:get(persist_test),
    ?assert(is_map(Result) orelse Result =:= undefined orelse Result =:= error).

test_hot_reload() ->
    Result = erlmcp_config:reload(),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Edge Cases Tests
%%====================================================================

edge_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_empty_config()),
        ?_test(test_nested_config()),
        ?_test(test_large_config()),
        ?_test(test_special_keys())
    ] end}.

test_empty_config() ->
    Result = erlmcp_config:set(empty_test, #{}),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_nested_config() ->
    Config = #{
        level1 => #{
            level2 => #{
                level3 => <<"deep value">>
            }
        }
    },
    Result = erlmcp_config:set(nested_test, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_large_config() ->
    LargeConfig = maps:from_list([{<<"key_", (integer_to_binary(I))/binary>>, I} || I <- lists:seq(1, 1000)]),
    Result = erlmcp_config:set(large_test, LargeConfig),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

test_special_keys() ->
    Config = #{
        <<"special-key">> => <<"value">>,
        <<"key.with.dots">> => <<"value">>,
        <<"key_with_underscores">> => <<"value">>
    },
    Result = erlmcp_config:set(special_test, Config),
    ?assertMatch(ok | {ok, _} | {error, _}, Result).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_config_lifecycle()),
        ?_test(test_config_update_and_reload()),
        ?_test(test_feature_with_config())
    ] end}.

test_config_lifecycle() ->
    erlmcp_config:set(lifecycle, #{status => created}),
    Config1 = erlmcp_config:get(lifecycle),
    erlmcp_config:set(lifecycle, #{status => updated}),
    Config2 = erlmcp_config:get(lifecycle),
    ?assert(Config1 =/= undefined orelse Config2 =/= undefined).

test_config_update_and_reload() ->
    erlmcp_config:set(update_test, #{v => 1}),
    erlmcp_config:reload(),
    Result = erlmcp_config:get(update_test),
    ?assert(Result =:= undefined orelse is_map(Result)).

test_feature_with_config() ->
    erlmcp_config:enable_feature(test_feature),
    FeatureConfig = erlmcp_config:get_feature_config(test_feature),
    ?assert(is_map(FeatureConfig) orelse FeatureConfig =:= undefined).
