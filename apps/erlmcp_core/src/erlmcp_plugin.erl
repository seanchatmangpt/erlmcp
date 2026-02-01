%%%-------------------------------------------------------------------
%%% @doc
%%% Plugin Behavior - Base interface for erlmcp plugins
%%%
%%% All erlmcp plugins must implement this behavior. Plugins extend
%%% the CLI with custom validators, formatters, exporters, and commands.
%%%
%%% == Plugin Types ==
%%%
%%% - Validators: Custom spec validators (erlmcp_plugin_validator)
%%% - Formatters: Output formatters (erlmcp_plugin_formatter)
%%% - Exporters: Export to external systems (erlmcp_plugin_exporter)
%%% - Commands: New CLI commands (erlmcp_plugin_command)
%%%
%%% == Lifecycle ==
%%%
%%% 1. Plugin discovered via erlmcp_plugin_manager
%%% 2. init/1 called with plugin options
%%% 3. Plugin registered in erlmcp_plugin_registry
%%% 4. Plugin available for execution
%%% 5. terminate/2 called on shutdown/reload
%%%
%%% == Security ==
%%%
%%% - Each plugin runs in isolated process
%%% - Plugin crash ≠ system crash (let-it-crash)
%%% - Process-level isolation (Erlang VM sandbox)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_plugin).

%% Plugin metadata
-export([
    plugin_name/1,
    plugin_version/1,
    plugin_type/1,
    plugin_description/1,
    plugin_author/1
]).

%% Behavior definition
-callback init(Opts :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback terminate(Reason :: term(), State :: term()) -> ok.

-callback metadata() -> #{
    name := binary(),
    version := binary(),
    type := validator | formatter | exporter | command | middleware,
    description := binary(),
    author => binary(),
    dependencies => [atom()]
}.

-optional_callbacks([terminate/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Extract plugin name from metadata
-spec plugin_name(module()) -> {ok, binary()} | {error, missing_field}.
plugin_name(Module) ->
    Metadata = Module:metadata(),
    case maps:find(name, Metadata) of
        {ok, Name} when is_binary(Name) -> {ok, Name};
        {ok, _} -> {error, invalid_name_type};
        error -> {error, missing_name}
    end.

%% @doc Extract plugin version from metadata
-spec plugin_version(module()) -> {ok, binary()} | {error, missing_field}.
plugin_version(Module) ->
    Metadata = Module:metadata(),
    case maps:find(version, Metadata) of
        {ok, Version} when is_binary(Version) -> {ok, Version};
        {ok, _} -> {error, invalid_version_type};
        error -> {error, missing_version}
    end.

%% @doc Extract plugin type from metadata
-spec plugin_type(module()) -> {ok, validator | formatter | exporter | command | middleware} | {error, missing_field}.
plugin_type(Module) ->
    Metadata = Module:metadata(),
    case maps:find(type, Metadata) of
        {ok, Type} when Type =:= validator; Type =:= formatter; Type =:= exporter;
                        Type =:= command; Type =:= middleware ->
            {ok, Type};
        {ok, _} -> {error, invalid_type};
        error -> {error, missing_type}
    end.

%% @doc Extract plugin description from metadata
-spec plugin_description(module()) -> {ok, binary()} | {error, missing_field}.
plugin_description(Module) ->
    Metadata = Module:metadata(),
    case maps:find(description, Metadata) of
        {ok, Desc} when is_binary(Desc) -> {ok, Desc};
        {ok, _} -> {error, invalid_description_type};
        error -> {error, missing_description}
    end.

%% @doc Extract plugin author from metadata
-spec plugin_author(module()) -> binary() | undefined.
plugin_author(Module) ->
    Metadata = Module:metadata(),
    maps:get(author, Metadata, undefined).
