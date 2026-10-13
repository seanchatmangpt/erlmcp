%%%-------------------------------------------------------------------
%%% @doc CLI Auto-Completer - TAB completion engine (tests-as-spec L184)
%%%
%%% Two surfaces:
%%% <ul>
%%% <li>Instance surface: `init/1' starts a real gen_server (callback in
%%%     erlmcp_cli_completer_impl) holding commands/flags/files/env
%%%     entries, usage ranking, fuzzy + case-insensitive matching,
%%%     wildcard + subcommand + `$VAR' + path completion, cache, custom
%%%     providers, async completion, timeouts.</li>
%%% <li>Legacy module-level surface (used by erlmcp_cli_suggester and
%%%     erlmcp_validate_cli): complete/1, get_completions/1,2,
%%%     available_commands/0, command_options/1, transport_names/0,
%%%     generate/1.</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_completer).

%% Instance surface (tests-as-spec)
-export([init/1, init/2, init_with_commands/1, add_entry/2, add_entries/2, get_commands/1,
         get_all_entries/1, complete/2, complete_with_info/2, complete_with_type/2,
         complete_with_context/3, complete_ranked/2, complete_ranked_by_recency/2,
         complete_path/2, complete_async/3, add_env_var/2, enable_file_completion/1,
         enable_fuzzy/1, set_case_sensitive/2, record_usage/2, enable_cache/1,
         get_cache_stats/1, clear_cache/1, add_provider/3, stop/1]).
%% Legacy module-level surface
-export([get_completions/1, get_completions/2, register_command/2,
         register_resource/1, register_tool/1, clear_dynamic_completions/0,
         available_commands/0, command_options/1, transport_names/0, generate/1]).

-define(COMPLETIONS_TABLE, erlmcp_cli_completions).
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
%% Instance surface
%%====================================================================

%% @doc Start a completer gen_server seeded with entries:
%% {Kind, Bin} | {Kind, Bin, Description} where Kind is command|flag|file.
init(Entries) ->
    erlmcp_cli_completer_impl:start_link(#{entries => normalize_entries(Entries),
                                           timeout => infinity}).

%% @doc Start a completer with options (e.g. #{timeout => Ms}).
init(Entries, Opts) ->
    erlmcp_cli_completer_impl:start_link(#{entries => normalize_entries(Entries),
                                           timeout => maps:get(timeout, Opts, infinity)}).

%% @doc Start a completer seeded with command names only.
init_with_commands(Commands) ->
    init([{command, C} || C <- Commands]).

%% @doc Normalize test entries: {Kind, Bin} | {Kind, Bin, Desc | Type |
%% SubcommandList}.
normalize_entries(Entries) ->
    [normalize_entry(E) || E <- Entries].

normalize_entry({Kind, Name}) when is_atom(Kind), is_binary(Name) ->
    {Kind, Name, undefined, Kind, []};
normalize_entry({Kind, Name, Desc}) when is_atom(Kind), is_binary(Name), is_binary(Desc) ->
    {Kind, Name, Desc, Kind, []};
normalize_entry({command, Name, Subs})
    when is_binary(Name), is_list(Subs) ->
    {command, Name, undefined, command, Subs};
normalize_entry({Kind, Name, Type}) when is_atom(Kind), is_binary(Name), is_atom(Type) ->
    {Kind, Name, undefined, Type, []}.

add_entry(Completer, Entry) ->
    gen_server:call(Completer, {add_entries, normalize_entries([Entry])}).

add_entries(Completer, Entries) ->
    gen_server:call(Completer, {add_entries, normalize_entries(Entries)}).

get_commands(Completer) ->
    gen_server:call(Completer, get_commands).

get_all_entries(Completer) ->
    gen_server:call(Completer, get_all_entries).

complete(Completer, Prefix) ->
    guarded_call(Completer, {complete, prefix, Prefix, undefined, undefined}).

complete_with_info(Completer, Prefix) ->
    guarded_call(Completer, {complete, prefix, Prefix, info, undefined}).

complete_with_type(Completer, Prefix) ->
    guarded_call(Completer, {complete, prefix, Prefix, type, undefined}).

complete_with_context(Completer, Prefix, #{context := Context}) ->
    guarded_call(Completer, {complete, prefix, Prefix, undefined, Context}).

complete_ranked(Completer, Prefix) ->
    guarded_call(Completer, {complete, ranked_freq, Prefix, undefined, undefined}).

complete_ranked_by_recency(Completer, Prefix) ->
    guarded_call(Completer, {complete, ranked_recency, Prefix, undefined, undefined}).

complete_path(Completer, Prefix) ->
    gen_server:call(Completer, {complete_path, Prefix}).

complete_async(Completer, Prefix, Callback) ->
    Self = self(),
    spawn(fun() ->
             case catch gen_server:call(Completer, {complete, prefix, Prefix, undefined, undefined},
                                        10000)
             of
                 {ok, Matches} ->
                     Callback(Matches),
                     Self ! async_done;
                 _Other ->
                     Self ! async_done
             end
          end),
    ok.

add_env_var(Completer, Name) when is_binary(Name) ->
    gen_server:call(Completer, {add_env_var, Name}).

enable_file_completion(Completer) ->
    gen_server:call(Completer, enable_file_completion).

enable_fuzzy(Completer) ->
    gen_server:call(Completer, enable_fuzzy).

set_case_sensitive(Completer, Bool) when is_boolean(Bool) ->
    gen_server:call(Completer, {set_case_sensitive, Bool}).

record_usage(Completer, Name) when is_binary(Name) ->
    gen_server:call(Completer, {record_usage, Name}).

enable_cache(Completer) ->
    gen_server:call(Completer, enable_cache).

get_cache_stats(Completer) ->
    gen_server:call(Completer, get_cache_stats).

clear_cache(Completer) ->
    gen_server:call(Completer, clear_cache).

add_provider(Completer, Id, Fun) when is_function(Fun, 1) ->
    gen_server:call(Completer, {add_provider, Id, Fun}).

stop(Completer) ->
    try
        gen_server:stop(Completer)
    catch
        exit:_ ->
            ok
    end,
    ok.

%% @doc Run a call under the configured timeout; on timeout return
%% {error, timeout} instead of crashing the client.
guarded_call(Completer, Request) ->
    Timeout = erlmcp_cli_completer_impl:timeout_of(Completer),
    try
        gen_server:call(Completer, Request, Timeout)
    catch
        exit:{timeout, _} ->
            %% Wait out the in-flight handler so the server stays usable,
            %% then report the timeout to the caller.
            erlmcp_cli_completer_impl:await_idle(Completer),
            {error, timeout};
        exit:_Other ->
            {error, timeout}
    end.

%%====================================================================
%% Legacy module-level surface
%%====================================================================

%% @doc Get all possible completions for a partial input
-spec get_completions(string()) -> [string()].
get_completions(Partial) ->
    Tokens = string:lexemes(Partial, " "),
    case Tokens of
        [] ->
            ?COMMANDS;
        [CommandPartial] ->
            filter_completions(CommandPartial, ?COMMANDS);
        [Command | Args] ->
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
    filter_completions(Partial, ["stdio", "tcp", "http", "websocket"]);
get_completions(format, Partial) ->
    filter_completions(Partial, ["text", "json", "markdown", "html"]);
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

%% @doc Built-in command names (binaries), used by the suggester.
available_commands() ->
    [list_to_binary(C) || C <- ?COMMANDS].

%% @doc Option flags for a command.
command_options(_Command) ->
    [<<"--format">>, <<"--output">>, <<"--verbose">>, <<"--quiet">>].

%% @doc Valid transport names.
transport_names() ->
    [<<"stdio">>, <<"tcp">>, <<"http">>, <<"websocket">>].

%% @doc Generate a shell completion script (bash/zsh/fish).
generate(Shell) when Shell =:= bash; Shell =:= zsh; Shell =:= fish ->
    Commands = ?COMMANDS,
    Line = string:join(Commands, " "),
    Script =
        case Shell of
            bash ->
                io_lib:format("_erlmcp_validate_completions()~n"
                              "~s~n"
                              "COMPREPLY=($(compgen -W \"~s\" -- \"${COMP_WORDS[COMP_CWORD]}\"))~n"
                              "}~n"
                              "complete -F _erlmcp_validate_completions erlmcp_validate~n",
                              ["{", Line]);
            zsh ->
                io_lib:format("#compdef erlmcp_validate~n"
                              "_erlmcp_validate() {~n"
                              "  _values 'command' ~s~n"
                              "}~n"
                              "_erlmcp_validate \"$@\"~n",
                              [[C, " "] || C <- Commands]);
            fish ->
                io_lib:format("complete -c erlmcp_validate -a '~s'~n", [Line])
        end,
    {ok, iolist_to_binary(Script)};
generate(Shell) ->
    {error, {unsupported_shell, Shell}}.

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

%% @doc Get active connections (queried from the interactive session when
%% running, otherwise empty).
-spec get_active_connections() -> [string()].
get_active_connections() ->
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
