%% @doc Profile Manager for erlmcp Configuration Management
%%
%% Provides runtime configuration management through profiles:
%% - dev: Development (relaxed limits, debug logging)
%% - prod: Production (strict limits, HTTPS required, circuit breaker on)
%% - gov: Government/Regulated (audit logging, deterministic, FIPS-140-2)
%%
%% Usage:
%%   erlmcp_profile_manager:list_profiles()
%%   erlmcp_profile_manager:show_profile(dev)
%%   erlmcp_profile_manager:apply_profile(prod)
%%   erlmcp_profile_manager:validate_profile(gov)

-module(erlmcp_profile_manager).

-export([
    list_profiles/0,
    show_profile/1,
    apply_profile/1,
    apply_profile/2,
    validate_profile/1,
    get_profile_path/1,
    merge_profile/1,
    get_current_profile/0
]).

-type profile_name() :: dev | prod | gov | atom().
-type config_term() :: tuple() | atom().
-type config_list() :: [config_term()].

%% Profile definitions
-define(PROFILES_DIR, "profiles").
-define(DEFAULT_PROFILE, dev).

%% ============================================================================
%% Public API
%% ============================================================================

%% @doc List all available profiles with descriptions
-spec list_profiles() -> {ok, [{profile_name(), string()}]} | {error, term()}.
list_profiles() ->
    case list_profile_files() of
        {ok, Files} ->
            Profiles = lists:map(fun(File) ->
                Name = profile_name_from_file(File),
                Desc = profile_description(Name),
                {Name, Desc}
            end, Files),
            {ok, Profiles};
        Error ->
            Error
    end.

%% @doc Show configuration for a specific profile
-spec show_profile(profile_name()) -> {ok, config_list()} | {error, term()}.
show_profile(Profile) ->
    case get_profile_path(Profile) of
        {ok, Path} ->
            case file:consult(Path) of
                {ok, Config} ->
                    {ok, Config};
                {error, Reason} ->
                    {error, {failed_to_read_profile, Profile, Reason}}
            end;
        Error ->
            Error
    end.

%% @doc Apply a profile to runtime configuration
-spec apply_profile(profile_name()) -> {ok, applied} | {error, term()}.
apply_profile(Profile) ->
    apply_profile(Profile, runtime).

%% @doc Apply a profile with target (runtime or file)
%% target: runtime = merge into application environment
%% target: file = write to sys.config
-spec apply_profile(profile_name(), runtime | file) -> {ok, applied} | {error, term()}.
apply_profile(Profile, Target) ->
    case validate_profile(Profile) of
        ok ->
            case show_profile(Profile) of
                {ok, Config} ->
                    case Target of
                        runtime ->
                            apply_runtime(Profile, Config);
                        file ->
                            apply_to_file(Profile, Config);
                        _ ->
                            {error, {invalid_target, Target}}
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% @doc Validate a profile configuration
-spec validate_profile(profile_name()) -> ok | {error, term()}.
validate_profile(Profile) ->
    case show_profile(Profile) of
        {ok, Config} ->
            validate_config(Config, Profile);
        Error ->
            Error
    end.

%% @doc Get full path to profile configuration file
-spec get_profile_path(profile_name()) -> {ok, string()} | {error, term()}.
get_profile_path(Profile) ->
    FileName = atom_to_list(Profile) ++ ".config",
    ProfileDir = get_profiles_dir(),
    Path = filename:join(ProfileDir, FileName),
    case filelib:is_file(Path) of
        true ->
            {ok, Path};
        false ->
            {error, {profile_not_found, Profile}}
    end.

%% @doc Merge profile configuration into current config
-spec merge_profile(profile_name()) -> {ok, config_list()} | {error, term()}.
merge_profile(Profile) ->
    case show_profile(Profile) of
        {ok, NewConfig} ->
            case application:get_all_env() of
                CurrentEnv ->
                    Merged = merge_configs(CurrentEnv, NewConfig),
                    {ok, Merged};
                _ ->
                    {error, failed_to_get_current_config}
            end;
        Error ->
            Error
    end.

%% @doc Get the current active profile (from environment)
-spec get_current_profile() -> profile_name() | undefined.
get_current_profile() ->
    case application:get_env(erlmcp, active_profile) of
        {ok, Profile} ->
            Profile;
        undefined ->
            undefined
    end.

%% ============================================================================
%% Internal Functions
%% ============================================================================

%% @private Get profiles directory
-spec get_profiles_dir() -> string().
get_profiles_dir() ->
    case code:priv_dir(erlmcp) of
        {error, _} ->
            %% Fall back to relative path
            ?PROFILES_DIR;
        PrivDir ->
            %% Try relative to code root
            RootDir = filename:dirname(filename:dirname(PrivDir)),
            filename:join(RootDir, ?PROFILES_DIR)
    end.

%% @private List all profile files in profiles directory
-spec list_profile_files() -> {ok, [string()]} | {error, term()}.
list_profile_files() ->
    ProfileDir = get_profiles_dir(),
    case file:list_dir(ProfileDir) of
        {ok, Files} ->
            ConfigFiles = [F || F <- Files, filename:extension(F) =:= ".config"],
            {ok, ConfigFiles};
        {error, Reason} ->
            {error, {failed_to_list_profiles, Reason}}
    end.

%% @private Extract profile name from filename
-spec profile_name_from_file(string()) -> profile_name().
profile_name_from_file(File) ->
    BaseName = filename:basename(File, ".config"),
    try
        list_to_atom(BaseName)
    catch
        _:_ ->
            unknown
    end.

%% @private Get human-readable description for profile
-spec profile_description(profile_name()) -> string().
profile_description(dev) ->
    "Development - Fast feedback, relaxed limits, debug logging";
profile_description(prod) ->
    "Production - Strict limits, HTTPS required, circuit breaker ON";
profile_description(gov) ->
    "Government - Audit logging, deterministic, FIPS-140-2 compatible";
profile_description(_) ->
    "Unknown profile".

%% @private Validate profile configuration structure
-spec validate_config(config_list(), profile_name()) -> ok | {error, term()}.
validate_config(Config, Profile) ->
    try
        case is_list(Config) of
            true ->
                validate_config_terms(Config, Profile);
            false ->
                {error, {invalid_config_format, not_a_list}}
        end
    catch
        _:Reason ->
            {error, {validation_error, Reason}}
    end.

%% @private Validate individual configuration terms
-spec validate_config_terms(config_list(), profile_name()) -> ok | {error, term()}.
validate_config_terms([], _Profile) ->
    ok;
validate_config_terms([{AppName, AppConfig} | Rest], Profile) ->
    case validate_app_config(AppName, AppConfig) of
        ok ->
            validate_config_terms(Rest, Profile);
        Error ->
            Error
    end;
validate_config_terms([_Invalid | _], _Profile) ->
    {error, {invalid_config_term, not_a_tuple}}.

%% @private Validate application-specific configuration
-spec validate_app_config(atom(), term()) -> ok | {error, term()}.
validate_app_config(kernel, Config) when is_list(Config) ->
    ok;
validate_app_config(erlmcp, Config) when is_list(Config) ->
    validate_erlmcp_config(Config);
validate_app_config(sasl, Config) when is_list(Config) ->
    ok;
validate_app_config(AppName, Config) when is_list(Config) ->
    % Allow other applications with list configs
    ok;
validate_app_config(AppName, _Config) ->
    {error, {invalid_app_config, AppName}}.

%% @private Validate erlmcp-specific configuration
-spec validate_erlmcp_config(config_list()) -> ok | {error, term()}.
validate_erlmcp_config(Config) ->
    case lists:keyfind(log_level, 1, Config) of
        false ->
            {error, {missing_config, log_level}};
        {log_level, Level} when Level =:= debug; Level =:= info; Level =:= warning; Level =:= error ->
            ok;
        {log_level, _} ->
            {error, {invalid_log_level, invalid_value}}
    end.

%% @private Apply profile to runtime environment
-spec apply_runtime(profile_name(), config_list()) -> {ok, applied} | {error, term()}.
apply_runtime(Profile, Config) ->
    try
        lists:foreach(fun({AppName, AppConfig}) ->
            apply_app_config(AppName, AppConfig)
        end, Config),
        application:set_env(erlmcp, active_profile, Profile),
        {ok, applied}
    catch
        _:Reason ->
            {error, {failed_to_apply_profile, Reason}}
    end.

%% @private Apply application configuration at runtime
-spec apply_app_config(atom(), config_list()) -> ok.
apply_app_config(AppName, AppConfig) ->
    lists:foreach(fun({Key, Value}) ->
        application:set_env(AppName, Key, Value)
    end, AppConfig).

%% @private Apply profile to sys.config file
-spec apply_to_file(profile_name(), config_list()) -> {ok, applied} | {error, term()}.
apply_to_file(_Profile, Config) ->
    try
        SysConfigPath = get_sys_config_path(),
        case file:read_file(SysConfigPath) of
            {ok, _Binary} ->
                %% Create backup
                BackupPath = SysConfigPath ++ ".backup-" ++ format_timestamp(),
                file:copy(SysConfigPath, BackupPath),

                %% Write new config
                ConfigString = format_config(Config),
                case file:write_file(SysConfigPath, ConfigString) of
                    ok ->
                        {ok, applied};
                    {error, WriteReason} ->
                        %% Restore backup on failure
                        file:copy(BackupPath, SysConfigPath),
                        {error, {failed_to_write_config, WriteReason}}
                end;
            {error, ReadReason} ->
                {error, {failed_to_read_sys_config, ReadReason}}
        end
    catch
        _:Reason ->
            {error, {failed_to_apply_to_file, Reason}}
    end.

%% @private Get path to sys.config
-spec get_sys_config_path() -> string().
get_sys_config_path() ->
    case code:priv_dir(erlmcp) of
        {error, _} ->
            "config/sys.config";
        PrivDir ->
            RootDir = filename:dirname(filename:dirname(PrivDir)),
            filename:join(RootDir, "config/sys.config")
    end.

%% @private Merge two configurations (new overrides old)
-spec merge_configs(config_list(), config_list()) -> config_list().
merge_configs(OldConfig, NewConfig) ->
    % This is simplified - in practice would need deep merging
    NewAppNames = lists:map(fun({App, _}) -> App end, NewConfig),
    OldFiltered = lists:filter(fun({App, _}) ->
        not lists:member(App, NewAppNames)
    end, OldConfig),
    OldFiltered ++ NewConfig.

%% @private Format config for writing to file
-spec format_config(config_list()) -> string().
format_config(Config) ->
    io_lib:format("~tp.~n", [Config]).

%% @private Format current timestamp for backup naming
-spec format_timestamp() -> string().
format_timestamp() ->
    {Year, Month, Day} = erlang:date(),
    {Hour, Min, Sec} = erlang:time(),
    io_lib:format("~4..0b~2..0b~2..0b-~2..0b~2..0b~2..0b",
                  [Year, Month, Day, Hour, Min, Sec]).
