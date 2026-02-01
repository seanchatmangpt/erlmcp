%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Auto-Completer - Tab completion for commands and arguments
%%%
%%% Provides context-aware tab completion for:
%%% - Command names
%%% - Command arguments
%%% - Resource names
%%% - Tool names
%%% - File paths
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_completer).

%% API exports
-export([complete/1, complete/2, get_completions/1, get_completions/2, register_command/2,
         register_resource/1, register_tool/1, clear_dynamic_completions/0]).

%% ETS table for dynamic completions
-define(COMPLETIONS_TABLE, erlmcp_cli_completions).
%% Built-in commands
-define(COMMANDS,
        ["help",
         "exit",
         "quit",
         "connect",
         "disconnect",
         "connections",
         "list-resources",
         "list-tools",
         "list-prompts",
         "read-resource",
         "call-tool",
         "get-prompt",
         "subscribe",
         "unsubscribe",
         "history",
         "clear",
         "status",
         "version",
         "validate",
         "spec-check",
         "transport-check"]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Complete a partial command (returns first match)
-spec complete(string()) -> {ok, string()} | {error, no_match} | {ambiguous, [string()]}.
complete(Partial) ->
    case get_completions(Partial) of
        [] ->
            {error, no_match};
        [Single] ->
            {ok, Single};
        Multiple ->
            {ambiguous, Multiple}
    end.

%% @doc Complete with context (command, argument, resource, tool)
-spec complete(atom(), string()) -> {ok, string()} | {error, no_match} | {ambiguous, [string()]}.
complete(Context, Partial) ->
    case get_completions(Context, Partial) of
        [] ->
            {error, no_match};
        [Single] ->
            {ok, Single};
        Multiple ->
            {ambiguous, Multiple}
    end.

%% @doc Get all possible completions for a partial input
-spec get_completions(string()) -> [string()].
get_completions(Partial) ->
    % Parse input to determine context
    Tokens = string:lexemes(Partial, " "),

    case Tokens of
        [] ->
            % Empty input - show all commands
            ?COMMANDS;
        [CommandPartial] ->
            % Completing command name
            filter_completions(CommandPartial, ?COMMANDS);
        [Command | Args] ->
            % Completing arguments
            get_argument_completions(Command, Args)
    end.

%% @doc Get completions for a specific context
-spec get_completions(atom(), string()) -> [string()].
get_completions(command, Partial) ->
    filter_completions(Partial, ?COMMANDS);
get_completions(resource, Partial) ->
    Resources = get_dynamic_completions(resources),
    filter_completions(Partial, Resources);
get_completions(tool, Partial) ->
    Tools = get_dynamic_completions(tools),
    filter_completions(Partial, Tools);
get_completions(transport, Partial) ->
    Transports = ["stdio", "tcp", "http", "websocket"],
    filter_completions(Partial, Transports);
get_completions(format, Partial) ->
    Formats = ["text", "json", "markdown", "html"],
    filter_completions(Partial, Formats);
get_completions(_Context, _Partial) ->
    [].

%% @doc Register a command with its argument spec
-spec register_command(string(), list()) -> ok.
register_command(Command, ArgSpec) ->
    ensure_table(),
    ets:insert(?COMPLETIONS_TABLE, {{command, Command}, ArgSpec}),
    ok.

%% @doc Register a resource name for completion
-spec register_resource(string()) -> ok.
register_resource(ResourceName) ->
    ensure_table(),
    Resources = get_dynamic_completions(resources),
    case lists:member(ResourceName, Resources) of
        true ->
            ok;
        false ->
            ets:insert(?COMPLETIONS_TABLE, {resources, [ResourceName | Resources]}),
            ok
    end.

%% @doc Register a tool name for completion
-spec register_tool(string()) -> ok.
register_tool(ToolName) ->
    ensure_table(),
    Tools = get_dynamic_completions(tools),
    case lists:member(ToolName, Tools) of
        true ->
            ok;
        false ->
            ets:insert(?COMPLETIONS_TABLE, {tools, [ToolName | Tools]}),
            ok
    end.

%% @doc Clear dynamic completions (resources, tools)
-spec clear_dynamic_completions() -> ok.
clear_dynamic_completions() ->
    ensure_table(),
    ets:delete(?COMPLETIONS_TABLE, resources),
    ets:delete(?COMPLETIONS_TABLE, tools),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Filter completions by prefix
-spec filter_completions(string(), [string()]) -> [string()].
filter_completions(Prefix, Candidates) ->
    PrefixLower = string:lowercase(Prefix),
    lists:filter(fun(Candidate) ->
                    CandidateLower = string:lowercase(Candidate),
                    string:prefix(CandidateLower, PrefixLower) =/= nomatch
                 end,
                 Candidates).

%% @doc Get argument completions based on command
-spec get_argument_completions(string(), [string()]) -> [string()].
get_argument_completions("connect", Args) ->
    case length(Args) of
        0 ->
            ["stdio://", "tcp://", "http://", "ws://"];
        _ ->
            []
    end;
get_argument_completions("disconnect", Args) ->
    case length(Args) of
        0 ->
            get_active_connections();
        _ ->
            []
    end;
get_argument_completions("read-resource", Args) ->
    case length(Args) of
        0 ->
            get_dynamic_completions(resources);
        _ ->
            []
    end;
get_argument_completions("call-tool", Args) ->
    case length(Args) of
        0 ->
            get_dynamic_completions(tools);
        _ ->
            []
    end;
get_argument_completions("subscribe", Args) ->
    case length(Args) of
        0 ->
            get_dynamic_completions(resources);
        _ ->
            []
    end;
get_argument_completions("unsubscribe", Args) ->
    case length(Args) of
        0 ->
            get_dynamic_completions(resources);
        _ ->
            []
    end;
get_argument_completions("transport-check", Args) ->
    case length(Args) of
        0 ->
            ["stdio", "tcp", "http", "websocket"];
        1 ->
            get_flag_completions();
        _ ->
            []
    end;
get_argument_completions("validate", Args) ->
    case length(Args) of
        0 ->
            ["stdio://", "tcp://", "http://"];
        1 ->
            get_flag_completions();
        _ ->
            []
    end;
get_argument_completions(_Command, _Args) ->
    [].

%% @doc Get flag completions
-spec get_flag_completions() -> [string()].
get_flag_completions() ->
    ["--format", "--output", "--verbose", "--quiet"].

%% @doc Get active connections (stub for now)
-spec get_active_connections() -> [string()].
get_active_connections() ->
    % This would query the interactive server state
    % For now, return empty list
    [].

%% @doc Get dynamic completions from ETS
-spec get_dynamic_completions(atom()) -> [string()].
get_dynamic_completions(Key) ->
    ensure_table(),
    case ets:lookup(?COMPLETIONS_TABLE, Key) of
        [{Key, List}] ->
            List;
        [] ->
            []
    end.

%% @doc Ensure ETS table exists
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:info(?COMPLETIONS_TABLE, name) of
        undefined ->
            ets:new(?COMPLETIONS_TABLE, [named_table, public, set]),
            ok;
        ?COMPLETIONS_TABLE ->
            ok
    end.
