%% @doc Comprehensive tests for erlmcp_profile_manager
%%
%% Tests cover:
%% - Profile listing and discovery
%% - Profile content retrieval
%% - Profile validation
%% - Profile application (runtime and file modes)
%% - Configuration merging
%% - Error handling

-module(erlmcp_profile_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%% ============================================================================
%% Test Suite
%% ============================================================================

%% List profiles test
list_profiles_test() ->
    {ok, Profiles} = erlmcp_profile_manager:list_profiles(),
    ?assert(is_list(Profiles)),
    ?assert(length(Profiles) >= 3),

    ProfileNames = lists:map(fun({Name, _Desc}) -> Name end, Profiles),
    ?assert(lists:member(dev, ProfileNames)),
    ?assert(lists:member(prod, ProfileNames)),
    ?assert(lists:member(gov, ProfileNames)),

    % Verify descriptions exist
    lists:foreach(fun({_Name, Desc}) ->
        ?assert(is_list(Desc)),
        ?assert(length(Desc) > 0)
    end, Profiles).

%% Show profile test
show_profile_dev_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(dev),
    ?assert(is_list(Config)),
    ?assert(length(Config) > 0),

    % Check for erlmcp application config
    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    ?assert(ErlmcpConfig =/= undefined),

    % Check dev-specific settings
    LogLevel = proplists:get_value(log_level, ErlmcpConfig),
    ?assertEqual(debug, LogLevel),

    % Circuit breaker should be disabled in dev
    CircuitBreaker = proplists:get_value(circuit_breaker, ErlmcpConfig),
    CBEnabled = maps:get(enabled, CircuitBreaker),
    ?assertEqual(false, CBEnabled).

show_profile_prod_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(prod),
    ?assert(is_list(Config)),

    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    ?assert(ErlmcpConfig =/= undefined),

    % Check prod-specific settings
    LogLevel = proplists:get_value(log_level, ErlmcpConfig),
    ?assertEqual(info, LogLevel),

    % Circuit breaker should be enabled in prod
    CircuitBreaker = proplists:get_value(circuit_breaker, ErlmcpConfig),
    CBEnabled = maps:get(enabled, CircuitBreaker),
    ?assertEqual(true, CBEnabled),

    % HTTPS should be required in prod
    HTTPSConfig = proplists:get_value(https_config, ErlmcpConfig),
    HTTPSEnabled = proplists:get_value(enabled, HTTPSConfig),
    ?assertEqual(true, HTTPSEnabled).

show_profile_gov_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(gov),
    ?assert(is_list(Config)),

    ErlmcpConfig = proplists:get_value(erlmcp, Config),
    ?assert(ErlmcpConfig =/= undefined),

    % Check gov-specific settings
    LogLevel = proplists:get_value(log_level, ErlmcpConfig),
    ?assertEqual(debug, LogLevel),

    % Audit logging should be enabled
    AuditLogging = proplists:get_value(audit_logging, ErlmcpConfig),
    AuditEnabled = proplists:get_value(enabled, AuditLogging),
    ?assertEqual(true, AuditEnabled),

    % Rate limiting should be strict
    RateLimiting = proplists:get_value(rate_limiting, ErlmcpConfig),
    MaxMsgPerSec = maps:get(max_messages_per_sec, RateLimiting),
    ?assertEqual(50, MaxMsgPerSec).

show_profile_nonexistent_test() ->
    Result = erlmcp_profile_manager:show_profile(nonexistent),
    ?assertEqual({error, {profile_not_found, nonexistent}}, Result).

%% Validate profile tests
validate_profile_dev_test() ->
    ?assertEqual(ok, erlmcp_profile_manager:validate_profile(dev)).

validate_profile_prod_test() ->
    ?assertEqual(ok, erlmcp_profile_manager:validate_profile(prod)).

validate_profile_gov_test() ->
    ?assertEqual(ok, erlmcp_profile_manager:validate_profile(gov)).

validate_profile_nonexistent_test() ->
    Result = erlmcp_profile_manager:validate_profile(nonexistent),
    ?assertEqual({error, {profile_not_found, nonexistent}}, Result).

%% Get profile path tests
get_profile_path_dev_test() ->
    {ok, Path} = erlmcp_profile_manager:get_profile_path(dev),
    ?assert(is_list(Path)),
    ?assert(filelib:is_file(Path)),
    ?assert(string:str(Path, "dev.config") > 0).

get_profile_path_prod_test() ->
    {ok, Path} = erlmcp_profile_manager:get_profile_path(prod),
    ?assert(is_list(Path)),
    ?assert(filelib:is_file(Path)),
    ?assert(string:str(Path, "prod.config") > 0).

get_profile_path_gov_test() ->
    {ok, Path} = erlmcp_profile_manager:get_profile_path(gov),
    ?assert(is_list(Path)),
    ?assert(filelib:is_file(Path)),
    ?assert(string:str(Path, "gov.config") > 0).

get_profile_path_nonexistent_test() ->
    Result = erlmcp_profile_manager:get_profile_path(nonexistent),
    ?assertEqual({error, {profile_not_found, nonexistent}}, Result).

%% Apply profile tests (runtime)
apply_profile_runtime_dev_test() ->
    % Save original env
    OrigLogLevel = application:get_env(erlmcp, log_level),
    OrigProfile = application:get_env(erlmcp, active_profile),

    try
        % Apply dev profile
        {ok, applied} = erlmcp_profile_manager:apply_profile(dev, runtime),

        % Verify it was applied
        {ok, debug} = application:get_env(erlmcp, log_level),
        {ok, dev} = application:get_env(erlmcp, active_profile)
    after
        % Restore
        case OrigLogLevel of
            {ok, Level} -> application:set_env(erlmcp, log_level, Level);
            _ -> ok
        end,
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> ok
        end
    end.

apply_profile_runtime_prod_test() ->
    OrigLogLevel = application:get_env(erlmcp, log_level),
    OrigProfile = application:get_env(erlmcp, active_profile),

    try
        {ok, applied} = erlmcp_profile_manager:apply_profile(prod, runtime),

        {ok, info} = application:get_env(erlmcp, log_level),
        {ok, prod} = application:get_env(erlmcp, active_profile)
    after
        case OrigLogLevel of
            {ok, Level} -> application:set_env(erlmcp, log_level, Level);
            _ -> ok
        end,
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> ok
        end
    end.

apply_profile_runtime_gov_test() ->
    OrigLogLevel = application:get_env(erlmcp, log_level),
    OrigProfile = application:get_env(erlmcp, active_profile),

    try
        {ok, applied} = erlmcp_profile_manager:apply_profile(gov, runtime),

        {ok, debug} = application:get_env(erlmcp, log_level),
        {ok, gov} = application:get_env(erlmcp, active_profile)
    after
        case OrigLogLevel of
            {ok, Level} -> application:set_env(erlmcp, log_level, Level);
            _ -> ok
        end,
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> ok
        end
    end.

apply_profile_invalid_test() ->
    Result = erlmcp_profile_manager:apply_profile(nonexistent, runtime),
    ?assert(element(1, Result) =:= error).

%% Get current profile tests
get_current_profile_test() ->
    % Save original
    OrigProfile = application:get_env(erlmcp, active_profile),

    try
        % Set profile
        application:set_env(erlmcp, active_profile, dev),
        ?assertEqual(dev, erlmcp_profile_manager:get_current_profile()),

        % Set another
        application:set_env(erlmcp, active_profile, prod),
        ?assertEqual(prod, erlmcp_profile_manager:get_current_profile())
    after
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> application:unset_env(erlmcp, active_profile)
        end
    end.

get_current_profile_undefined_test() ->
    % Ensure not set
    application:unset_env(erlmcp, active_profile),
    ?assertEqual(undefined, erlmcp_profile_manager:get_current_profile()).

%% Merge profile tests
merge_profile_dev_test() ->
    {ok, Merged} = erlmcp_profile_manager:merge_profile(dev),
    ?assert(is_list(Merged)),

    % Should have erlmcp config
    ErlmcpConfig = proplists:get_value(erlmcp, Merged),
    ?assert(ErlmcpConfig =/= undefined).

merge_profile_prod_test() ->
    {ok, Merged} = erlmcp_profile_manager:merge_profile(prod),
    ?assert(is_list(Merged)),

    ErlmcpConfig = proplists:get_value(erlmcp, Merged),
    ?assert(ErlmcpConfig =/= undefined).

merge_profile_gov_test() ->
    {ok, Merged} = erlmcp_profile_manager:merge_profile(gov),
    ?assert(is_list(Merged)),

    ErlmcpConfig = proplists:get_value(erlmcp, Merged),
    ?assert(ErlmcpConfig =/= undefined).

merge_profile_nonexistent_test() ->
    Result = erlmcp_profile_manager:merge_profile(nonexistent),
    ?assert(element(1, Result) =:= error).

%% Profile characteristics tests
profile_dev_relaxed_limits_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(dev),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),

    % Dev should have relaxed limits
    RateLimiting = proplists:get_value(rate_limiting, ErlmcpConfig),

    % Rate limiting should be disabled
    Enabled = maps:get(enabled, RateLimiting),
    ?assertEqual(false, Enabled),

    % Message limits should be higher
    MessageLimits = proplists:get_value(message_size_limits, ErlmcpConfig),
    DefaultLimit = maps:get(default, MessageLimits),
    ?assertEqual(33554432, DefaultLimit).  % 32 MB

profile_prod_strict_limits_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(prod),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),

    % Prod should have strict limits
    RateLimiting = proplists:get_value(rate_limiting, ErlmcpConfig),

    % Rate limiting should be enabled
    Enabled = maps:get(enabled, RateLimiting),
    ?assertEqual(true, Enabled),

    % Should have standard limits
    MessageLimits = proplists:get_value(message_size_limits, ErlmcpConfig),
    DefaultLimit = maps:get(default, MessageLimits),
    ?assertEqual(16777216, DefaultLimit).  % 16 MB

profile_gov_audit_enabled_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(gov),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),

    % Gov should have audit logging enabled
    AuditLogging = proplists:get_value(audit_logging, ErlmcpConfig),
    Enabled = proplists:get_value(enabled, AuditLogging),
    ?assertEqual(true, Enabled),

    LogAllOps = proplists:get_value(log_all_operations, AuditLogging),
    ?assertEqual(true, LogAllOps).

profile_gov_deterministic_test() ->
    {ok, Config} = erlmcp_profile_manager:show_profile(gov),
    ErlmcpConfig = proplists:get_value(erlmcp, Config),

    % Gov should have deterministic behavior
    Backpressure = proplists:get_value(backpressure, ErlmcpConfig),

    % Compression should be disabled (determinism)
    SessionReplication = proplists:get_value(session_replication, ErlmcpConfig),
    Compression = proplists:get_value(enable_compression, SessionReplication),
    ?assertEqual(false, Compression).

%% Error handling tests
apply_profile_invalid_target_test() ->
    Result = erlmcp_profile_manager:apply_profile(dev, invalid_target),
    ?assertEqual({error, {invalid_target, invalid_target}}, Result).

%% Configuration consistency tests
profile_configs_valid_erlang_test() ->
    % Ensure all profiles can be consulted as valid Erlang terms
    lists:foreach(fun(Profile) ->
        {ok, Path} = erlmcp_profile_manager:get_profile_path(Profile),
        {ok, _Config} = file:consult(Path)
    end, [dev, prod, gov]).

profile_configs_contain_erlmcp_test() ->
    % All profiles should contain erlmcp config
    lists:foreach(fun(Profile) ->
        {ok, Config} = erlmcp_profile_manager:show_profile(Profile),
        ErlmcpConfig = proplists:get_value(erlmcp, Config),
        ?assert(ErlmcpConfig =/= undefined,
                io_lib:format("Profile ~w missing erlmcp config", [Profile]))
    end, [dev, prod, gov]).

profile_configs_contain_kernel_test() ->
    % All profiles should contain kernel config
    lists:foreach(fun(Profile) ->
        {ok, Config} = erlmcp_profile_manager:show_profile(Profile),
        KernelConfig = proplists:get_value(kernel, Config),
        ?assert(KernelConfig =/= undefined,
                io_lib:format("Profile ~w missing kernel config", [Profile]))
    end, [dev, prod, gov]).

%% Profile transitions test
profile_transition_dev_to_prod_test() ->
    OrigProfile = application:get_env(erlmcp, active_profile),
    OrigLogLevel = application:get_env(erlmcp, log_level),

    try
        % Start with dev
        {ok, applied} = erlmcp_profile_manager:apply_profile(dev, runtime),
        {ok, debug} = application:get_env(erlmcp, log_level),

        % Transition to prod
        {ok, applied} = erlmcp_profile_manager:apply_profile(prod, runtime),
        {ok, info} = application:get_env(erlmcp, log_level),
        {ok, prod} = application:get_env(erlmcp, active_profile)
    after
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> ok
        end,
        case OrigLogLevel of
            {ok, Level} -> application:set_env(erlmcp, log_level, Level);
            _ -> ok
        end
    end.

profile_transition_prod_to_gov_test() ->
    OrigProfile = application:get_env(erlmcp, active_profile),

    try
        % Start with prod
        {ok, applied} = erlmcp_profile_manager:apply_profile(prod, runtime),
        {ok, prod} = application:get_env(erlmcp, active_profile),

        % Transition to gov
        {ok, applied} = erlmcp_profile_manager:apply_profile(gov, runtime),
        {ok, gov} = application:get_env(erlmcp, active_profile)
    after
        case OrigProfile of
            {ok, Prof} -> application:set_env(erlmcp, active_profile, Prof);
            _ -> ok
        end
    end.

%% Performance characteristics test
profile_dev_vs_prod_timeout_test() ->
    {ok, DevConfig} = erlmcp_profile_manager:show_profile(dev),
    {ok, ProdConfig} = erlmcp_profile_manager:show_profile(prod),

    DevDefaults = proplists:get_value(client_defaults,
                                      proplists:get_value(erlmcp, DevConfig)),
    ProdDefaults = proplists:get_value(client_defaults,
                                       proplists:get_value(erlmcp, ProdConfig)),

    DevTimeout = maps:get(timeout, DevDefaults),
    ProdTimeout = maps:get(timeout, ProdDefaults),

    % Dev should have longer timeout for debugging
    ?assert(DevTimeout > ProdTimeout).

profile_dev_vs_prod_logging_test() ->
    {ok, DevConfig} = erlmcp_profile_manager:show_profile(dev),
    {ok, ProdConfig} = erlmcp_profile_manager:show_profile(prod),

    DevErlmcp = proplists:get_value(erlmcp, DevConfig),
    ProdErlmcp = proplists:get_value(erlmcp, ProdConfig),

    DevLogLevel = proplists:get_value(log_level, DevErlmcp),
    ProdLogLevel = proplists:get_value(log_level, ProdErlmcp),

    % Dev should log debug, prod should log info
    ?assertEqual(debug, DevLogLevel),
    ?assertEqual(info, ProdLogLevel).

%% ============================================================================
%% Test Execution
%% ============================================================================

profile_manager_test_() ->
    {
        "Profile Manager Tests",
        [
            {"List profiles", fun list_profiles_test/0},
            {"Show profile (dev)", fun show_profile_dev_test/0},
            {"Show profile (prod)", fun show_profile_prod_test/0},
            {"Show profile (gov)", fun show_profile_gov_test/0},
            {"Show profile (nonexistent)", fun show_profile_nonexistent_test/0},
            {"Validate profile (dev)", fun validate_profile_dev_test/0},
            {"Validate profile (prod)", fun validate_profile_prod_test/0},
            {"Validate profile (gov)", fun validate_profile_gov_test/0},
            {"Validate profile (nonexistent)", fun validate_profile_nonexistent_test/0},
            {"Get profile path (dev)", fun get_profile_path_dev_test/0},
            {"Get profile path (prod)", fun get_profile_path_prod_test/0},
            {"Get profile path (gov)", fun get_profile_path_gov_test/0},
            {"Get profile path (nonexistent)", fun get_profile_path_nonexistent_test/0},
            {"Apply profile runtime (dev)", fun apply_profile_runtime_dev_test/0},
            {"Apply profile runtime (prod)", fun apply_profile_runtime_prod_test/0},
            {"Apply profile runtime (gov)", fun apply_profile_runtime_gov_test/0},
            {"Apply profile invalid", fun apply_profile_invalid_test/0},
            {"Get current profile", fun get_current_profile_test/0},
            {"Get current profile undefined", fun get_current_profile_undefined_test/0},
            {"Merge profile (dev)", fun merge_profile_dev_test/0},
            {"Merge profile (prod)", fun merge_profile_prod_test/0},
            {"Merge profile (gov)", fun merge_profile_gov_test/0},
            {"Merge profile (nonexistent)", fun merge_profile_nonexistent_test/0},
            {"Profile dev has relaxed limits", fun profile_dev_relaxed_limits_test/0},
            {"Profile prod has strict limits", fun profile_prod_strict_limits_test/0},
            {"Profile gov has audit enabled", fun profile_gov_audit_enabled_test/0},
            {"Profile gov is deterministic", fun profile_gov_deterministic_test/0},
            {"Apply profile invalid target", fun apply_profile_invalid_target_test/0},
            {"Profile configs are valid Erlang", fun profile_configs_valid_erlang_test/0},
            {"Profile configs contain erlmcp", fun profile_configs_contain_erlmcp_test/0},
            {"Profile configs contain kernel", fun profile_configs_contain_kernel_test/0},
            {"Profile transition dev to prod", fun profile_transition_dev_to_prod_test/0},
            {"Profile transition prod to gov", fun profile_transition_prod_to_gov_test/0},
            {"Profile dev vs prod timeout", fun profile_dev_vs_prod_timeout_test/0},
            {"Profile dev vs prod logging", fun profile_dev_vs_prod_logging_test/0}
        ]
    }.
