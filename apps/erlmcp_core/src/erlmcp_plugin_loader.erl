%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Loader - Dynamic code loading and validation
%%%
%%% Handles:
%%% - Plugin discovery (filesystem scan)
%%% - Code loading (code:load_file/1)
%%% - Behavior validation
%%% - Dependency resolution
%%%
%%% == Discovery Paths ==
%%% 1. ~/.erlmcp/plugins/*.beam
%%% 2. sys.config: {erlmcp_core, [{plugin_dirs, ["/path/to/plugins"]}]}
%%% 3. Application priv dir: priv/plugins/*.beam
%%%
%%% == Validation ==
%%% - Module exports required behavior callbacks
%%% - metadata/0 returns valid plugin metadata
%%% - Dependencies are available
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin_loader).

%% API
-export([discover_plugins/0, discover_plugins/1, load_plugin/1, unload_plugin/1, validate_plugin/1,
         get_plugin_paths/0]).

-define(DEFAULT_PLUGIN_DIRS,
        ["~/.erlmcp/plugins", "/usr/local/lib/erlmcp/plugins", "/opt/erlmcp/plugins"]).

%%====================================================================
%% API
%%====================================================================

%% @doc Discover all plugins in default paths
-spec discover_plugins() -> {ok, [module()]} | {error, term()}.
discover_plugins() ->
    Paths = get_plugin_paths(),
    discover_plugins(Paths).

%% @doc Discover plugins in specific paths
-spec discover_plugins([string()]) -> {ok, [module()]} | {error, term()}.
discover_plugins(Paths) ->
    try
        ExpandedPaths = [expand_path(Path) || Path <- Paths],
        ExistingPaths = [Path || Path <- ExpandedPaths, filelib:is_dir(Path)],

        Modules =
            lists:flatmap(fun(Path) ->
                             BeamFiles =
                                 filelib:wildcard(
                                     filename:join(Path, "*.beam")),
                             [beam_to_module(Beam) || Beam <- BeamFiles]
                          end,
                          ExistingPaths),

        {ok, lists:usort(Modules)}
    catch
        _:Error:Stack ->
            {error, {discovery_failed, Error, Stack}}
    end.

%% @doc Load a plugin module
-spec load_plugin(module()) -> {ok, module()} | {error, term()}.
load_plugin(Module) when is_atom(Module) ->
    try
        %% Check if module is already loaded
        case code:is_loaded(Module) of
            {file, _} ->
                %% Already loaded - validate it
                case validate_plugin(Module) of
                    ok ->
                        {ok, Module};
                    {error, Reason} ->
                        {error, {validation_failed, Reason}}
                end;
            false ->
                %% Load the module
                case code:load_file(Module) of
                    {module, Module} ->
                        %% Validate after loading
                        case validate_plugin(Module) of
                            ok ->
                                {ok, Module};
                            {error, Reason} ->
                                %% Unload on validation failure
                                code:purge(Module),
                                code:delete(Module),
                                {error, {validation_failed, Reason}}
                        end;
                    {error, Reason} ->
                        {error, {load_failed, Reason}}
                end
        end
    catch
        _:Error:Stack ->
            {error, {load_error, Error, Stack}}
    end.

%% @doc Unload a plugin module
-spec unload_plugin(module()) -> ok | {error, term()}.
unload_plugin(Module) when is_atom(Module) ->
    try
        %% Purge old code
        code:purge(Module),
        %% Delete module
        case code:delete(Module) of
            true ->
                ok;
            false ->
                {error, not_loaded}
        end
    catch
        _:Error:Stack ->
            {error, {unload_error, Error, Stack}}
    end.

%% @doc Validate plugin module
-spec validate_plugin(module()) -> ok | {error, term()}.
validate_plugin(Module) when is_atom(Module) ->
    try
        %% Check module is loaded
        case code:is_loaded(Module) of
            {file, _} ->
                %% Check for metadata/0 export
                case erlang:function_exported(Module, metadata, 0) of
                    true ->
                        %% Validate metadata
                        case validate_metadata(Module) of
                            ok ->
                                %% Validate behavior callbacks
                                validate_behavior_callbacks(Module);
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    false ->
                        {error, missing_metadata_function}
                end;
            false ->
                {error, module_not_loaded}
        end
    catch
        _:Error:Stack ->
            {error, {validation_error, Error, Stack}}
    end.

%% @doc Get plugin search paths
-spec get_plugin_paths() -> [string()].
get_plugin_paths() ->
    %% From application config
    ConfigPaths = application:get_env(erlmcp_core, plugin_dirs, []),

    %% Application priv dir
    PrivPath =
        case code:priv_dir(erlmcp_core) of
            {error, bad_name} ->
                [];
            PrivDir ->
                [filename:join(PrivDir, "plugins")]
        end,

    %% Combine all paths
    ConfigPaths ++ PrivPath ++ ?DEFAULT_PLUGIN_DIRS.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Expand path (handle ~)
expand_path(Path) ->
    case Path of
        "~/" ++ Rest ->
            Home = os:getenv("HOME", "/tmp"),
            filename:join(Home, Rest);
        "~" ->
            os:getenv("HOME", "/tmp");
        _ ->
            Path
    end.

%% @private Convert beam file path to module name
beam_to_module(BeamPath) ->
    Basename = filename:basename(BeamPath, ".beam"),
    list_to_atom(Basename).

%% @private Validate plugin metadata
validate_metadata(Module) ->
    try
        Metadata = Module:metadata(),
        RequiredFields = [name, version, type, description],
        case lists:all(fun(Field) -> maps:is_key(Field, Metadata) end, RequiredFields) of
            true ->
                %% Validate type
                Type = maps:get(type, Metadata),
                ValidTypes = [validator, formatter, exporter, command, middleware],
                case lists:member(Type, ValidTypes) of
                    true ->
                        ok;
                    false ->
                        {error, {invalid_type, Type}}
                end;
            false ->
                MissingFields = [F || F <- RequiredFields, not maps:is_key(F, Metadata)],
                {error, {missing_metadata_fields, MissingFields}}
        end
    catch
        _:Error:Stack ->
            {error, {metadata_error, Error, Stack}}
    end.

%% @private Validate behavior callbacks
validate_behavior_callbacks(Module) ->
    try
        Metadata = Module:metadata(),
        Type = maps:get(type, Metadata),

        %% Base callbacks (all plugins)
        BaseCallbacks = [{init, 1}, {metadata, 0}],

        %% Type-specific callbacks
        TypeCallbacks =
            case Type of
                validator ->
                    [{validate, 2}, {get_schema, 0}];
                formatter ->
                    [{format, 2}, {supports_format, 0}];
                exporter ->
                    [{export, 2}, {get_config_schema, 0}];
                command ->
                    [{execute, 2}, {help, 0}];
                middleware ->
                    [{pre_execute, 2}, {post_execute, 2}];
                _ ->
                    []
            end,

        AllCallbacks = BaseCallbacks ++ TypeCallbacks,

        %% Check each callback is exported
        Exports = Module:module_info(exports),
        MissingCallbacks =
            lists:filter(fun(Callback) -> not lists:member(Callback, Exports) end, AllCallbacks),

        case MissingCallbacks of
            [] ->
                ok;
            _ ->
                {error, {missing_callbacks, MissingCallbacks}}
        end
    catch
        _:Error:Stack ->
            {error, {behavior_validation_error, Error, Stack}}
    end.
