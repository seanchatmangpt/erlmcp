%%%-------------------------------------------------------------------
%%% @doc
%%% Feature Flags Management Module
%%%
%%% This module provides runtime feature flag management with support for:
%%% - Loading feature flags from ConfigMap
%%% - Hot-reloading feature flags
%%% - Conditional feature rollout (canary, percentage-based)
%%% - Environment-specific flag overrides
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_feature_flags).

%% API
-export([
    load_flags/0,
    load_flags/1,
    reload_flags/0,
    get_flag/1,
    get_flag/2,
    set_flag/2,
    set_flag/3,
    is_enabled/1,
    is_enabled/2,
    get_rollout_percentage/1,
    evaluate_rollout/2,
    list_flags/0,
    list_flags/1,
    export_flags/0,
    import_flags/1,
    watch_config/0
]).

%% Internal exports for RPC
%% reload_config/0 removed - use reload_flags/0 instead

%% Types
-type flag_name() :: binary() | atom() | string().
-type flag_value() :: boolean() | number() | binary() | map().
-type flag_category() :: binary() | atom().
-type rollout_strategy() :: all | none | canary | percentage | user_segment.
-type flag_state() :: enabled | disabled | rollout.
-type feature_flag() :: #{
    name := binary(),
    category => flag_category(),
    state => flag_state(),
    value => flag_value(),
    rollout_percentage => 0..100,
    rollout_strategy => rollout_strategy(),
    conditions => map(),
    description => binary(),
    updated_at => integer()
}.
-type flags() :: #{flag_name() => feature_flag()}.

-export_type([feature_flag/0, flags/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Load feature flags from default location
-spec load_flags() -> {ok, flags()} | {error, term()}.
load_flags() ->
    load_flags(default).

%% @doc Load feature flags from specified source
-spec load_flags(default | file:filename() | binary()) -> {ok, flags()} | {error, term()}.
load_flags(Source) ->
    case read_flags(Source) of
        {ok, Flags} ->
            cache_flags(Flags),
            {ok, Flags};
        {error, Reason} = Error ->
            logger:error("Failed to load feature flags: ~p", [Reason]),
            Error
    end.

%% @doc Reload feature flags from source
-spec reload_flags() -> {ok, flags()} | {error, term()}.
reload_flags() ->
    logger:info("Reloading feature flags"),
    load_flags().

%% @doc Get a feature flag value (returns default if not found)
-spec get_flag(flag_name()) -> flag_value() | undefined.
get_flag(FlagName) ->
    get_flag(FlagName, undefined).

%% @doc Get a feature flag value with default
-spec get_flag(flag_name(), flag_value()) -> flag_value().
get_flag(FlagName, Default) ->
    Key = as_key(FlagName),
    case get_cached_flags() of
        #{Key := Flag} ->
            maps:get(value, Flag, Default);
        _ ->
            Default
    end.

%% @doc Set a feature flag value
-spec set_flag(flag_name(), flag_value()) -> ok | {error, term()}.
set_flag(FlagName, Value) ->
    set_flag(FlagName, Value, #{}).

%% @doc Set a feature flag with options
-spec set_flag(flag_name(), flag_value(), map()) -> ok | {error, term()}.
set_flag(FlagName, Value, Options) ->
    State = maps:get(state, Options, enabled),
    Flag = #{
        name => as_binary(FlagName),
        category => maps:get(category, Options, custom),
        state => State,
        value => Value,
        rollout_percentage => maps:get(rollout_percentage, Options, 100),
        rollout_strategy => maps:get(rollout_strategy, Options, all),
        conditions => maps:get(conditions, Options, #{}),
        description => maps:get(description, Options, <<>>),
        updated_at => system_time()
    },
    Flags = get_cached_flags(),
    UpdatedFlags = Flags#{as_key(FlagName) => Flag},
    cache_flags(UpdatedFlags),
    ok.

%% @doc Check if a feature flag is enabled
-spec is_enabled(flag_name()) -> boolean().
is_enabled(FlagName) ->
    is_enabled(FlagName, false).

%% @doc Check if a feature flag is enabled with default
-spec is_enabled(flag_name(), boolean()) -> boolean().
is_enabled(FlagName, Default) ->
    case get_flag(FlagName) of
        Value when is_boolean(Value) -> Value;
        _ -> Default
    end.

%% @doc Get rollout percentage for a flag
-spec get_rollout_percentage(flag_name()) -> 0..100.
get_rollout_percentage(FlagName) ->
    Key = as_key(FlagName),
    case get_cached_flags() of
        #{Key := #{rollout_percentage := Percentage}} ->
            Percentage;
        _ ->
            0
    end.

%% @doc Evaluate if a rollout applies to the given identifier
-spec evaluate_rollout(flag_name(), binary()) -> boolean().
evaluate_rollout(FlagName, Identifier) ->
    Key = as_key(FlagName),
    case get_cached_flags() of
        #{Key := Flag} ->
            evaluate_flag_rollout(Flag, Identifier);
        _ ->
            false
    end.

%% @doc List all feature flags
-spec list_flags() -> flags().
list_flags() ->
    get_cached_flags().

%% @doc List flags in a specific category
-spec list_flags(flag_category()) -> [feature_flag()].
list_flags(Category) ->
    Flags = get_cached_flags(),
    maps:fold(
        fun(_Key, Flag = #{category := Cat}, Acc) ->
            case Cat of
                Category -> [Flag | Acc];
                _ -> Acc
            end;
        (_Key, _Flag, Acc) ->
            Acc
        end,
        [],
        Flags
    ).

%% @doc Export flags as JSON
-spec export_flags() -> {ok, binary()} | {error, term()}.
export_flags() ->
    Flags = get_cached_flags(),
    Export = #{
        version => "3.0.0",
        environment => application:get_env(erlmcp, environment, production),
        last_updated => system_time(),
        flags => maps:values(Flags)
    },
    try
        {ok, json:encode(Export)}
    catch
        error:_ -> {error, json_encode_failed}
    end.

%% @doc Import flags from JSON
-spec import_flags(binary()) -> ok | {error, term()}.
import_flags(JSON) when is_binary(JSON) ->
    try json:decode(JSON) of
        #{<<"flags">> := FlagsList} ->
            Flags = lists:foldl(
                fun(Flag = #{<<"name">> := Name}, Acc) ->
                    Acc#{as_binary(Name) => normalize_flag(Flag)}
                end,
                #{},
                FlagsList
            ),
            cache_flags(Flags),
            logger:info("Imported ~p feature flags", [maps:size(Flags)]),
            ok;
        _ ->
            {error, invalid_format}
    catch
        error:_ -> {error, json_decode_failed}
    end;
import_flags(_) ->
    {error, invalid_input}.

%% @doc Start watching for config changes
-spec watch_config() -> {ok, pid()} | {error, term()}.
watch_config() ->
    case whereis(erlmcp_feature_flags_watcher) of
        undefined ->
            spawn_link(?MODULE, config_watcher_loop, []),
            {ok, erlmcp_feature_flags_watcher};
        Pid ->
            {ok, Pid}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Read flags from various sources
read_flags(default) ->
    % Try environment variable first
    case os:getenv("FEATURE_FLAGS_PATH") of
        false ->
            % Try ConfigMap mount
            read_from_configmap();
        Path ->
            read_from_file(Path)
    end;
read_flags(Path) when is_list(Path) ->
    read_from_file(Path);
read_flags(JSON) when is_binary(JSON) ->
    import_flags(JSON).

%% @private Read flags from ConfigMap
read_from_configmap() ->
    ConfigMapPath = "/opt/erlmcp/etc/feature-flags.json",
    case file:read_file(ConfigMapPath) of
        {ok, Content} ->
            import_flags(Content);
        {error, Reason} ->
            logger:warning("Failed to read feature flags from ConfigMap: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Read flags from file
read_from_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            import_flags(Content);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @private Cache flags in ETS for fast access
cache_flags(Flags) ->
    try
        ets:insert(erlmcp_feature_flags, {flags, Flags}),
        ok
    catch
        error:badarg ->
            % Table doesn't exist, create it
            ets:new(erlmcp_feature_flags, [
                named_table,
                set,
                public,
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ets:insert(erlmcp_feature_flags, {flags, Flags}),
            ok
    end.

%% @private Get cached flags
get_cached_flags() ->
    try ets:lookup_element(erlmcp_feature_flags, flags, 1) of
        Flags -> Flags
    catch
        error:badarg ->
            % Initialize with empty flags
            Empty = #{},
            cache_flags(Empty),
            Empty
    end.

%% @private Evaluate flag rollout
evaluate_flag_rollout(#{state := State, value := Value}, _Identifier) when is_boolean(Value) ->
    Value;
evaluate_flag_rollout(#{state := enabled}, _Identifier) ->
    true;
evaluate_flag_rollout(#{state := disabled}, _Identifier) ->
    false;
evaluate_flag_rollout(#{state := rollout, rollout_percentage := Percentage, rollout_strategy := Strategy}, Identifier) ->
    case Strategy of
        percentage ->
            Hash = erlang:phash2(Identifier),
            Hash rem 100 < Percentage;
        canary ->
            % Canary based on hash of identifier
            Hash = erlang:phash2(Identifier),
            Hash rem 100 < Percentage;
        all ->
            true;
        none ->
            false;
        user_segment ->
            evaluate_conditions(Identifier, Percentage)
    end;
evaluate_flag_rollout(_Flag, _Identifier) ->
    false.

%% @private Evaluate conditions for user segment rollout
evaluate_conditions(Identifier, Percentage) ->
    % Simple hash-based segment assignment
    Hash = erlang:phash2(Identifier),
    Hash rem 100 < Percentage.

%% @private Normalize flag from import
normalize_flag(Flag) ->
    #{
        name => maps:get(<<"name">>, Flag),
        category => maps:get(<<"category">>, Flag, custom),
        state => normalize_state(maps:get(<<"state">>, Flag, enabled)),
        value => maps:get(<<"value">>, Flag, true),
        rollout_percentage => maps:get(<<"rollout_percentage">>, Flag, 100),
        rollout_strategy => normalize_strategy(maps:get(<<"rollout_strategy">>, Flag, all)),
        conditions => maps:get(<<"conditions">>, Flag, #{}),
        description => maps:get(<<"description">>, Flag, <<>>),
        updated_at => maps:get(<<"updated_at">>, Flag, system_time())
    }.

%% @private Normalize state atom
normalize_state(<<"enabled">>) -> enabled;
normalize_state(<<"disabled">>) -> disabled;
normalize_state(<<"rollout">>) -> rollout;
normalize_state(enabled) -> enabled;
normalize_state(disabled) -> disabled;
normalize_state(rollout) -> rollout;
normalize_state(_) -> enabled.

%% @private Normalize strategy atom
normalize_strategy(<<"all">>) -> all;
normalize_strategy(<<"none">>) -> none;
normalize_strategy(<<"canary">>) -> canary;
normalize_strategy(<<"percentage">>) -> percentage;
normalize_strategy(<<"user_segment">>) -> user_segment;
normalize_strategy(all) -> all;
normalize_strategy(none) -> none;
normalize_strategy(canary) -> canary;
normalize_strategy(percentage) -> percentage;
normalize_strategy(user_segment) -> user_segment;
normalize_strategy(_) -> all.

%% @private Convert to binary key
as_key(Key) when is_binary(Key) -> Key;
as_key(Key) when is_atom(Key) -> atom_to_binary(Key, utf8);
as_key(Key) when is_list(Key) -> list_to_binary(Key).

%% @private Convert to binary
as_binary(B) when is_binary(B) -> B;
as_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
as_binary(L) when is_list(L) -> list_to_binary(L).

%% @private Get system time in milliseconds
system_time() ->
    erlang:system_time(millisecond).

%% @private Config watcher loop
config_watcher_loop() ->
    register(erlmcp_feature_flags_watcher, self()),
    logger:info("Feature flags watcher started"),
    config_watcher_loop(60000). % Check every minute

config_watcher_loop(Interval) ->
    receive
        stop ->
            logger:info("Feature flags watcher stopped"),
            ok
    after Interval ->
        % Check for ConfigMap updates
        case check_config_update() of
            {updated, NewVersion} ->
                logger:info("ConfigMap updated to version ~s, reloading flags", [NewVersion]),
                reload_flags();
            unchanged ->
                ok
        end,
        config_watcher_loop(Interval)
    end.

%% @private Check for ConfigMap updates
check_config_update() ->
    CurrentVersion = get_config_version(),
    case get_last_version() of
        CurrentVersion ->
            unchanged;
        _ ->
            set_last_version(CurrentVersion),
            {updated, CurrentVersion}
    end.

%% @private Get ConfigMap resource version
get_config_version() ->
    ConfigMapPath = "/opt/erlmcp/etc/.configmap_version",
    case file:read_file(ConfigMapPath) of
        {ok, Version} -> binary:trim(Version);
        {error, _} -> <<>>
    end.

%% @private Get last seen version
get_last_version() ->
    case ets:lookup(erlmcp_feature_flags, config_version) of
        [{config_version, Version}] -> Version;
        _ -> <<>>
    end.

%% @private Set last seen version
set_last_version(Version) ->
    try ets:insert(erlmcp_feature_flags, {config_version, Version}), ok
    catch error:badarg -> ok
    end.
