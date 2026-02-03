%%%-------------------------------------------------------------------
%%% @doc
%%% Feature Flags and ConfigMap Hot-Reload Tests
%%%
%%% Test suite for configuration management with hot-reload support.
%%% Tests include:
%%% - Feature flag loading and parsing
%%% - Hot-reload triggering
%%% - Rollout evaluation
%%% - Environment-specific overrides
%%% - Configuration validation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_feature_flags_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(TEST_FLAGS, #{
    <<"observability.metrics.enabled">> => #{
        name => <<"observability.metrics.enabled">>,
        category => observability,
        state => enabled,
        value => true,
        rollout_percentage => 100
    },
    <<"experimental.new_api">> => #{
        name => <<"experimental.new_api">>,
        category => experimental,
        state => rollout,
        value => false,
        rollout_percentage => 25,
        rollout_strategy => canary
    },
    <<"auth.rate_limit">> => #{
        name => <<"auth.rate_limit">>,
        category => security,
        state => enabled,
        value => true
    }
}).

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    % Create ETS table for tests
    ets:new(erlmcp_feature_flags, [
        named_table,
        set,
        public,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    % Cache test flags
    ets:insert(erlmcp_feature_flags, {flags, ?TEST_FLAGS}),
    ok.

cleanup(_) ->
    % Clean up ETS table
    catch ets:delete(erlmcp_feature_flags),
    ok.

%%====================================================================
%% Feature Flag Tests
%%====================================================================

flag_get_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
               Flag = erlmcp_feature_flags:get_flag(<<"observability.metrics.enabled">>),
               ?assertEqual(true, Flag)
           end),
      ?_test(begin
               Flag = erlmcp_feature_flags:get_flag(<<"nonexistent.flag">>),
               ?assertEqual(undefined, Flag)
           end),
      ?_test(begin
               Flag = erlmcp_feature_flags:get_flag(<<"nonexistent.flag">>, true),
               ?assertEqual(true, Flag)
           end)
     ]}.

flag_is_enabled_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
               ?assert(erlmcp_feature_flags:is_enabled(<<"observability.metrics.enabled">>))
           end),
      ?_test(begin
               ?assertNot(erlmcp_feature_flags:is_enabled(<<"experimental.new_api">>))
           end),
      ?_test(begin
               ?assert(erlmcp_feature_flags:is_enabled(<<"auth.rate_limit">>))
           end)
     ]}.

flag_rollout_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
               % Test rollout with different identifiers
               Results = [
                   erlmcp_feature_flags:evaluate_rollout(
                       <<"experimental.new_api">>,
                       <<"user-", (integer_to_binary(N))/binary>>
                   )
                || N <- lists:seq(1, 100)
               ],
               % Approximately 25% should be true due to rollout_percentage
               TrueCount = length([R || R <- Results, R =:= true]),
               ?assert(TrueCount >= 15 andalso TrueCount =< 35)
           end)
     ]}.

flag_set_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
               ?assertEqual(ok, erlmcp_feature_flags:set_flag(<<"test.flag">>, true)),
               ?assertEqual(true, erlmcp_feature_flags:get_flag(<<"test.flag">>))
           end),
      ?_test(begin
               ?assertEqual(ok, erlmcp_feature_flags:set_flag(
                   <<"test.flag2">>, false, #{state => disabled}
               )),
               ?assertEqual(false, erlmcp_feature_flags:get_flag(<<"test.flag2">>))
           end)
     ]}.

flag_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
               AllFlags = erlmcp_feature_flags:list_flags(),
               ?assertEqual(3, maps:size(AllFlags))
           end),
      ?_test(begin
               ExperimentalFlags = erlmcp_feature_flags:list_flags(experimental),
               ?assertEqual(1, length(ExperimentalFlags)),
               ?assertMatch(
                   #{name := <<"experimental.new_api">>},
                   hd(ExperimentalFlags)
               )
           end)
     ]}.

%%====================================================================
%% Configuration Manager Tests
%%====================================================================

config_get_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     [
      ?_test(begin
               ?assertEqual(ok, erlmcp_config_manager:start_link()),
               Config = erlmcp_config_manager:get_config(),
               ?assert(is_map(Config)),
               ?assert(maps:is_key(log_level, Config))
           end),
      ?_test(begin
               LogLevel = erlmcp_config_manager:get_config([log_level]),
               ?assertEqual(info, LogLevel)
           end),
      ?_test(begin
               Timeout = erlmcp_config_manager:get_config([server, request_timeout], 5000),
               ?assertEqual(30000, Timeout)
           end)
     ]}.

config_set_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     [
      ?_test(begin
               ?assertEqual(ok, erlmcp_config_manager:start_link()),
               ?assertEqual(ok, erlmcp_config_manager:set_config([test_key], test_value)),
               ?assertEqual(test_value, erlmcp_config_manager:get_config([test_key]))
           end),
      ?_test(begin
               ?assertEqual(ok, erlmcp_config_manager:start_link()),
               ?assertEqual(ok, erlmcp_config_manager:set_config(
                   [nested, key], nested_value
               )),
               ?assertEqual(nested_value, erlmcp_config_manager:get_config([nested, key]))
           end)
     ]}.

%%====================================================================
%% Hot-Reload Tests
%%====================================================================

hot_reload_test_() ->
    {setup,
     fun setup_config/0,
     fun cleanup_config/1,
     [
      ?_test(begin
               ?assertEqual(ok, erlmcp_config_manager:start_link()),

               % Get initial config
               Config1 = erlmcp_config_manager:get_config(),

               % Trigger reload
               ?assertEqual(ok, erlmcp_config_manager:reload_config()),

               % Config should still be valid
               Config2 = erlmcp_config_manager:get_config(),
               ?assert(is_map(Config2))
           end)
     ]}.

%%====================================================================
%% Environment-Specific Config Tests
%%====================================================================

env_override_test_() ->
    [
      ?_test(begin
               % Development environment
               os:putenv("ERLMCP_ENV", "development"),
               ?assertEqual(debug, get_log_level("development"))
           end),
      ?_test(begin
               % Production environment
               os:putenv("ERLMCP_ENV", "production"),
               ?assertEqual(info, get_log_level("production"))
           end)
    ].

%%====================================================================
%% Validation Tests
%%====================================================================

validation_test_() ->
    ValidConfig = #{
        <<"log_level">> => info,
        <<"log_format">> => json,
        <<"server">> => #{
            <<"max_subscriptions_per_resource">> => 5000,
            <<"request_timeout">> => 30000
        }
    },

    InvalidConfig = #{
        <<"log_level">> => invalid_value,
        <<"server">> => #{
            <<"max_subscriptions_per_resource">> => -1
        }
    },

    [
      ?_test(begin
               ?assertEqual(ok, erlmcp_config_manager:validate_config(ValidConfig))
           end),
      ?_test(begin
               {error, Errors} = erlmcp_config_manager:validate_config(InvalidConfig),
               ?assert(length(Errors) > 0)
           end)
    ].

%%====================================================================
%% Internal Helpers
%%====================================================================

setup_config() ->
    {ok, Pid} = erlmcp_config_manager:start_link(),
    Pid.

cleanup_config(_) ->
    gen_server:stop(erlmcp_config_manager).

get_log_level(Env) ->
    case Env of
        development -> debug;
        staging -> info;
        production -> info;
        _ -> info
    end.
