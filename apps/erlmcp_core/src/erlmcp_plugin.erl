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
-export([plugin_name/1, plugin_version/1, plugin_type/1, plugin_description/1, plugin_author/1]).

%% Behavior definition
-callback init(Opts :: map()) -> {ok, State :: term()} | {error, Reason :: term()}.
-callback terminate(Reason :: term(), State :: term()) -> ok.
-callback metadata() ->
                      #{name := binary(),
                        version := binary(),
                        type := validator | formatter | exporter | command | middleware,
                        description := binary(),
                        author => binary(),
                        dependencies => [atom()]}.

-optional_callbacks([terminate/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Extract plugin name from metadata
-spec plugin_name(module()) -> binary().
plugin_name(Module) ->
    Metadata = Module:metadata(),
    maps:get(name, Metadata).

%% @doc Extract plugin version from metadata
-spec plugin_version(module()) -> binary().
plugin_version(Module) ->
    Metadata = Module:metadata(),
    maps:get(version, Metadata).

%% @doc Extract plugin type from metadata
-spec plugin_type(module()) -> validator | formatter | exporter | command | middleware.
plugin_type(Module) ->
    Metadata = Module:metadata(),
    maps:get(type, Metadata).

%% @doc Extract plugin description from metadata
-spec plugin_description(module()) -> binary().
plugin_description(Module) ->
    Metadata = Module:metadata(),
    maps:get(description, Metadata).

%% @doc Extract plugin author from metadata
-spec plugin_author(module()) -> binary() | undefined.
plugin_author(Module) ->
    Metadata = Module:metadata(),
    maps:get(author, Metadata, undefined).
