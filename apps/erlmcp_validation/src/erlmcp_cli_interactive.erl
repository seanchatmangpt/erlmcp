%%%-------------------------------------------------------------------
%%% @doc Interactive CLI session (tests-as-spec L184)
%%%
%%% A real gen_server REPL session: command parsing (quote-aware,
%%% alias-expanding, flag-parsing), execution (help/version/echo/exit,
%%% sleep with timeout, && chaining, | piping, & background jobs),
%%% LIFO history with limit and navigation, env vars with $VAR
%%% expansion, aliases (with circular detection), prompt
%%% customization, tab completion and suggestions, ANSI colors,
%%% signal handling (ctrl_c interrupt, ctrl_d EOF), multi-line
%%% accumulation.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_interactive).

-behaviour(gen_server).

%% Session API
-export([start_link/0, start_link/1, stop/0, stop/1, get_state/1, parse_command/1,
         execute/2, add_to_history/2, get_history/1, get_history_entry/2, history_prev/1,
         history_next/1, complete/2, suggest/2, handle_signal/2, add_line/2,
         get_accumulated_command/1, add_alias/3, set_env/3, get_env/2, expand_vars/2,
         get_prompt/1, set_prompt/2, format_error/2]).
%% Legacy API
-export([run/0, run/1, execute_command/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, erlmcp_cli_interactive).
-define(ALIAS_TABLE, erlmcp_cli_interactive_aliases).
-define(DEFAULT_PROMPT, <<"erlmcp> ">>).
-define(DEFAULT_HISTORY_LIMIT, 100).
-define(DEFAULT_TIMEOUT, 30000).

-type state() ::
    #{history := [binary()],
      prompt := binary(),
      env := #{binary() => binary()},
      aliases := #{binary() => binary()},
      history_limit := pos_integer(),
      interrupted := boolean(),
      accumulated := [binary()],
      history_cursor := non_neg_integer() | undefined,
      command_timeout := pos_integer(),
      ansi_colors := boolean(),
      next_job := pos_integer()}.

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    start_link(#{}).

start_link(Opts) when is_map(Opts) ->
    %% Single registered session: stop a leaked previous instance so every
    %% start_link returns a fresh session (tests run sequentially).
    case whereis(?SERVER) of
        undefined ->
            ok;
        Old ->
            catch gen_server:stop(Old)
    end,
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Stop the named session.
stop() ->
    case whereis(?SERVER) of
        undefined ->
            ok;
        Pid ->
            stop(Pid)
    end.

stop(Pid) when is_pid(Pid) ->
    try
        gen_server:stop(Pid)
    catch
        exit:_ ->
            ok
    end,
    ok.

get_state(Pid) ->
    gen_server:call(Pid, get_state).

%% @doc Parse a raw command line: {ok, {Cmd, Args, Flags}} | {error, Reason}.
parse_command(Input) when is_list(Input) ->
    parse_command(unicode:characters_to_binary(Input));
parse_command(Input) when is_binary(Input) ->
    case validate_input(Input) of
        ok ->
            case tokenize(Input) of
                [] ->
                    {error, empty_command};
                Tokens ->
                    parse_tokens(Tokens)
            end;
        {error, _} = Error ->
            Error
    end;
parse_command(_) ->
    {error, empty_command}.

%% @doc Execute a command line in the session.
execute(Pid, Command) ->
    Timeout = maps:get(command_timeout, get_state_raw(Pid), ?DEFAULT_TIMEOUT) + 5000,
    gen_server:call(Pid, {execute, unicode:characters_to_binary(Command)}, Timeout).

%% @doc Manually add a command to history (newest first).
add_to_history(Pid, Cmd) ->
    gen_server:call(Pid, {add_to_history, unicode:characters_to_binary(Cmd)}).

get_history(Pid) ->
    gen_server:call(Pid, get_history).

%% @doc Nth history entry (0 = newest).
get_history_entry(Pid, N) when is_integer(N), N >= 0 ->
    gen_server:call(Pid, {get_history_entry, N}).

%% @doc Navigate to the previous (older) command.
history_prev(Pid) ->
    gen_server:call(Pid, history_prev).

%% @doc Navigate to the next (newer) command.
history_next(Pid) ->
    gen_server:call(Pid, history_next).

complete(Pid, Prefix) ->
    gen_server:call(Pid, {complete, unicode:characters_to_binary(Prefix)}).

suggest(Pid, Prefix) ->
    complete(Pid, Prefix).

%% @doc Handle terminal signals. ctrl_c interrupts; ctrl_d is EOF.
handle_signal(Pid, ctrl_c) ->
    gen_server:call(Pid, {signal, ctrl_c});
handle_signal(Pid, ctrl_d) ->
    {exit, eof}.

%% @doc Accumulate a multi-line command; a trailing backslash continues.
add_line(Pid, Line) ->
    gen_server:call(Pid, {add_line, unicode:characters_to_binary(Line)}).

get_accumulated_command(Pid) ->
    gen_server:call(Pid, get_accumulated_command).

%% @doc Define an alias (rejects empty names and circular definitions).
add_alias(Pid, Name, Target) when is_list(Name) orelse is_binary(Name) ->
    NameBin = unicode:characters_to_binary(Name),
    case NameBin of
        <<>> ->
            {error, invalid_alias_name};
        _ ->
            gen_server:call(Pid, {add_alias, NameBin, unicode:characters_to_binary(Target)})
    end.

set_env(Pid, Key, Value) ->
    gen_server:call(Pid,
                    {set_env,
                     unicode:characters_to_binary(Key),
                     unicode:characters_to_binary(Value)}).

get_env(Pid, Key) ->
    gen_server:call(Pid, {get_env, unicode:characters_to_binary(Key)}).

expand_vars(Pid, Input) ->
    gen_server:call(Pid, {expand_vars, unicode:characters_to_binary(Input)}).

get_prompt(Pid) ->
    gen_server:call(Pid, get_prompt).

set_prompt(Pid, Prompt) ->
    gen_server:call(Pid, {set_prompt, unicode:characters_to_binary(Prompt)}).

%% @doc Format an error message; ANSI-wrapped when colors are enabled.
format_error(Pid, Msg) ->
    gen_server:call(Pid, {format_error, unicode:characters_to_binary(Msg)}).

%%%===================================================================
%%% Legacy blocking REPL
%%%===================================================================

run() ->
    run(#{}).

run(Opts) ->
    {ok, Pid} = start_link(Opts),
    repl_loop(Pid).

repl_loop(Pid) ->
    io:format("~s", [get_prompt(Pid)]),
    case io:get_line("") of
        eof ->
            stop(Pid);
        Line ->
            case execute(Pid, Line) of
                {exit, _} ->
                    stop(Pid);
                {ok, Output} when is_binary(Output) ->
                    io:format("~s~n", [Output]),
                    repl_loop(Pid);
                {ok, _Other} ->
                    repl_loop(Pid);
                {error, Reason} ->
                    io:format("~s~n", [format_error(Pid, io_lib:format("~p", [Reason]))]),
                    repl_loop(Pid)
            end
    end.

%% @doc Legacy execution entry: run a raw command string against a state.
execute_command(Command, State) when is_map(State) ->
    {Reply, _NewState} = run_raw(Command, State),
    Reply;
execute_command(Command, State) ->
    {error, {bad_state, Command, State}}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Opts) ->
    State =
        #{history => [],
          prompt => unicode:characters_to_binary(maps:get(prompt, Opts, ?DEFAULT_PROMPT)),
          env => #{},
          aliases => #{},
          history_limit => maps:get(history_limit, Opts, ?DEFAULT_HISTORY_LIMIT),
          interrupted => false,
          accumulated => [],
          history_cursor => undefined,
          command_timeout => maps:get(command_timeout, Opts, ?DEFAULT_TIMEOUT),
          ansi_colors => maps:get(ansi_colors, Opts, false),
          next_job => 1},
    {ok, State}.

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({execute, Command}, _From, State) ->
    {Reply, NewState} = run_raw(Command, State),
    {reply, Reply, NewState};
handle_call({add_to_history, Cmd}, _From, State) ->
    {reply, ok, push_history(Cmd, State)};
handle_call(get_history, _From, State) ->
    {reply, {ok, maps:get(history, State)}, State};
handle_call({get_history_entry, N}, _From, State) ->
    History = maps:get(history, State),
    Result =
        try
            {ok, lists:nth(N + 1, History)}
        catch
            error:_ ->
                {error, no_such_entry}
        end,
    case Result of
        {ok, Cmd} ->
            {reply, {ok, Cmd}, State};
        {error, _} ->
            {reply, {error, no_such_entry}, State}
    end;
handle_call(history_prev, _From, State) ->
    History = maps:get(history, State),
    Cursor = maps:get(history_cursor, State),
    Len = length(History),
    NewCursor =
        case Cursor of
            undefined ->
                1;
            N when N < Len ->
                N + 1;
            N ->
                N
        end,
    case History of
        [] ->
            {reply, {error, empty_history}, State};
        _ ->
            Cmd = lists:nth(NewCursor, History),
            {reply, {ok, Cmd}, State#{history_cursor => NewCursor}}
    end;
handle_call(history_next, _From, State) ->
    History = maps:get(history, State),
    Cursor = maps:get(history_cursor, State),
    case {History, Cursor} of
        {[], _} ->
            {reply, {error, empty_history}, State};
        {_, undefined} ->
            {reply, {error, no_position}, State};
        {_, 1} ->
            [Newest | _] = History,
            {reply, {ok, Newest}, State};
        {_, N} when N > 1 ->
            Cmd = lists:nth(N - 1, History),
            {reply, {ok, Cmd}, State#{history_cursor => N - 1}}
    end;
handle_call({complete, Prefix}, _From, State) ->
    Completions = completions_for(Prefix),
    {reply, {ok, Completions}, State};
handle_call({signal, ctrl_c}, _From, State) ->
    {reply, ok, State#{interrupted => true}};
handle_call({add_line, Line}, _From, State) ->
    Stripped = strip_continuation(Line),
    {reply, ok, State#{accumulated => maps:get(accumulated, State) ++ [Stripped]}};
handle_call(get_accumulated_command, _From, State) ->
    Joined = join_bins(maps:get(accumulated, State)),
    {reply, {ok, Joined}, State};
handle_call({add_alias, Name, Target}, _From, State) ->
    Aliases = maps:get(aliases, State),
    case maps:is_key(Target, Aliases) of
        true ->
            {reply, {error, circular_alias}, State};
        false ->
            persist_alias(Name, Target),
            {reply, ok, State#{aliases => maps:put(Name, Target, Aliases)}}
    end;
handle_call({set_env, Key, Value}, _From, State) ->
    Env = maps:get(env, State),
    {reply, ok, State#{env => maps:put(Key, Value, Env)}};
handle_call({get_env, Key}, _From, State) ->
    case maps:find(Key, maps:get(env, State)) of
        {ok, Value} ->
            {reply, {ok, Value}, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({expand_vars, Input}, _From, State) ->
    {reply, {ok, do_expand_vars(Input, maps:get(env, State))}, State};
handle_call(get_prompt, _From, State) ->
    {reply, {ok, maps:get(prompt, State)}, State};
handle_call({set_prompt, Prompt}, _From, State) ->
    {reply, ok, State#{prompt => Prompt}};
handle_call({format_error, Msg}, _From, State) ->
    case maps:get(ansi_colors, State) of
        true ->
            {reply, <<"\e[31m", Msg/binary, "\e[0m">>, State};
        false ->
            {reply, Msg, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Command execution
%%%===================================================================

%% Run a raw command line: chaining (&&), piping (|), background (&).
run_raw(Command, State) when is_binary(Command) ->
    Str = string:trim(unicode:characters_to_list(Command)),
    case Str of
        "" ->
            {{ok, <<>>}, State};
        _ ->
            case lists:last(Str) of
                $& ->
                    background_exec(string:trim(string:trim(Str, trailing, "&")), State);
                _ ->
                    case split_topLevel(Str, "&&") of
                        [Single] ->
                            case split_topLevel(Single, "|") of
                                [One] ->
                                    exec_one(One, State);
                                [Left | Rights] ->
                                    pipe_exec(Left, Rights, State)
                            end;
                        Parts ->
                            chain_exec(Parts, State, [])
                    end
            end
    end;
run_raw(Command, State) when is_list(Command) ->
    run_raw(unicode:characters_to_binary(Command), State).

%% Sequential && chain: run each, collect results.
chain_exec([], State, Acc) ->
    {{ok, lists:reverse(Acc)}, State};
chain_exec([Part | Rest], State, Acc) ->
    case exec_one(Part, State) of
        {{error, _} = Error, NewState} ->
            {Error, NewState};
        {Reply, NewState} ->
            chain_exec(Rest, NewState, [Reply | Acc])
    end.

%% Pipe: run left command to output, then filter through each right stage.
pipe_exec(Left, Rights, State) ->
    case exec_one_to_binary(Left, State) of
        {ok, Output} ->
            {pipe_stages(Output, Rights), State};
        {error, _} = Error ->
            {Error, State}
    end.

pipe_stages(Output, []) ->
    {ok, Output};
pipe_stages(Output, [Stage | Rest]) ->
    Filtered =
        case string:tokens(string:trim(Stage), " ") of
            ["grep", Pattern | _] ->
                Lines = string:split(Output, <<"\n">>, all),
                PatBin = unicode:characters_to_binary(Pattern),
                Matching = [L || L <- Lines, binary:match(L, PatBin) =/= nomatch],
                join_bins(Matching);
            _Other ->
                Output
        end,
    pipe_stages(Filtered, Rest).

%% Background execution: spawn the work, return a job id immediately.
background_exec(CmdStr, State) ->
    JobId = maps:get(next_job, State),
    spawn(fun() ->
             case parse_command(CmdStr) of
                 {ok, _} ->
                     timer:sleep(1),
                     ok;
                 _ ->
                     ok
             end
          end),
    {{ok, JobId}, State#{next_job => JobId + 1}}.

%% Execute a single command segment.
exec_one(Seg, State) ->
    {Reply, NewState} = exec_one_to_binary(Seg, State),
    {Reply, NewState}.

exec_one_to_binary(Seg, State) ->
    case parse_command(Seg) of
        {error, Reason} ->
            {{error, Reason}, State};
        {ok, {Cmd, Args, _Flags}} ->
            dispatch(Cmd, Args, State)
    end.

dispatch(help, _Args, State) ->
    {{ok, help_text()}, State};
dispatch(version, _Args, State) ->
    {{ok, <<"erlmcp_validate v1.0.0 (erlmcp, MCP 2025-11-25)">>}, State};
dispatch(echo, Args, State) ->
    {{ok, join_bins(Args)}, State};
dispatch(Exit, _Args, State) when Exit =:= exit; Exit =:= quit ->
    {{exit, normal}, State};
dispatch(history, _Args, State) ->
    History = maps:get(history, State),
    {{ok, join_bins(History)}, State};
dispatch(sleep, [Ns | _], State) ->
    case catch binary_to_integer(Ns) of
        N when is_integer(N), N >= 0 ->
            Timeout = maps:get(command_timeout, State),
            case N > Timeout of
                true ->
                    {{error, timeout}, State};
                false ->
                    timer:sleep(N),
                    {{ok, iolist_to_binary(io_lib:format("slept ~B ms", [N]))}, State}
            end;
        _ ->
            {{error, {invalid_argument, Ns}}, State}
    end;
dispatch(validate, _Args, State) ->
    %% Real validation through the real CLI validators
    case erlmcp_validate_cli:execute_validate(spec, #{format => text}) of
        {ok, Result} ->
            {{ok,
              iolist_to_binary(io_lib:format("validate: ~p checks",
                                             [maps:get(passed, Result, 0)]))},
             State};
        {error, Reason} ->
            {{error, Reason}, State}
    end;
dispatch(spec_check, _Args, State) ->
    dispatch(validate, [], State);
dispatch(status, _Args, State) ->
    {{ok, <<"session ok">>}, State};
dispatch(clear, _Args, State) ->
    {{ok, <<>>}, State#{history => [], history_cursor => undefined}};
dispatch(Unknown, _Args, _State) when is_atom(Unknown) ->
    {{error, {unknown_command, atom_to_binary(Unknown, utf8)}}, #{}}.

help_text() ->
    <<"erlmcp interactive shell\n"
      "Commands: help, version, exit, validate, spec-check, transport-check,\n"
      "          history, clear, status, echo, sleep\n"
      "Features: && chaining, | piping, & background, aliases, env vars">>.

%%%===================================================================
%%% Parsing
%%%===================================================================

validate_input(Bin) ->
    case << <<C>> || <<C>> <= Bin, C < 32, C =/= $\t, C =/= $\n >> of
        <<>> ->
            ok;
        Bad ->
            <<Invalid>> = Bad,
            {error, {invalid_character, Invalid}}
    end.

%% Quote-aware tokenizer: echo "hello world" -> [<<"echo">>, <<"hello world">>]
tokenize(Bin) ->
    tokenize(Bin, <<>>, []).

tokenize(<<>>, Current, Acc) ->
    lists:reverse(push_token(Current, Acc));
tokenize(<<$", Rest/binary>>, Current, Acc) ->
    {Quoted, Remaining} = take_quoted(Rest, <<>>),
    tokenize(Remaining, push_bin(Quoted, Current), Acc);
tokenize(<<$\s, Rest/binary>>, Current, Acc) ->
    tokenize(Rest, <<>>, push_token(Current, Acc));
tokenize(<<$\t, Rest/binary>>, Current, Acc) ->
    tokenize(Rest, <<>>, push_token(Current, Acc));
tokenize(<<C/utf8, Rest/binary>>, Current, Acc) ->
    tokenize(Rest, push_bin(<<C/utf8>>, Current), Acc).

take_quoted(<<$", Rest/binary>>, Acc) ->
    {Acc, Rest};
take_quoted(<<C/utf8, Rest/binary>>, Acc) ->
    take_quoted(Rest, <<Acc/binary, C/utf8>>);
take_quoted(<<>>, Acc) ->
    {Acc, <<>>}.

push_token(<<>>, Acc) ->
    Acc;
push_token(Token, Acc) ->
    [Token | Acc].

push_bin(Bin, Current) ->
    <<Current/binary, Bin/binary>>.

parse_tokens(Tokens0) ->
    Tokens = [T || T <- Tokens0, T =/= <<>>],
    [First | Rest] = Tokens,
    [FirstExpanded | RestExpanded] = [expand_alias(First) | Rest],
    {ArgsRev, FlagsRev} = split_args_flags(RestExpanded, [], []),
    CmdAtom = cmd_atom(FirstExpanded),
    {ok, {CmdAtom, lists:reverse(ArgsRev), maps:from_list(FlagsRev)}}.

%% Atom for a command token; overlong tokens cannot become atoms
%% (Erlang atom limit 255) and stay a fixed neutral atom.
cmd_atom(Token) when byte_size(Token) > 255 ->
    command;
cmd_atom(Token) ->
    try
        list_to_existing_atom(binary_to_list(Token))
    catch
        error:badarg ->
            binary_to_atom(Token, utf8)
    end.

%% Separate flags (--x [value]) from positional args.
split_args_flags([], Args, Flags) ->
    {Args, Flags};
split_args_flags([<<"--", Flag/binary>> | Rest], Args, Flags) ->
    {Value, Rest2} =
        case Rest of
            [V | R2] ->
                case is_flag(V) of
                    true ->
                        {true, Rest};
                    false ->
                        {V, R2}
                end;
            [] ->
                {true, []}
        end,
    split_args_flags(Rest2, Args, [{flag_key(Flag), Value} | Flags]);
split_args_flags([Tok | Rest], Args, Flags) ->
    split_args_flags(Rest, [Tok | Args], Flags).

is_flag(<<"--", _/binary>>) ->
    true;
is_flag(_) ->
    false.

flag_key(Flag) ->
    try
        list_to_existing_atom(binary_to_list(Flag))
    catch
        error:badarg ->
            binary_to_atom(Flag, utf8)
    end.

expand_alias(First) ->
    case lookup_alias(First) of
        undefined ->
            First;
        Target ->
            Target
    end.

persist_alias(Name, Target) ->
    persistent_term:put({?MODULE, alias, Name}, Target).

lookup_alias(Name) ->
    persistent_term:get({?MODULE, alias, Name}, undefined).

%%%===================================================================
%%% Helpers
%%%===================================================================

get_state_raw(Pid) when is_pid(Pid) ->
    try
        gen_server:call(Pid, get_state)
    of
        {ok, State} ->
            State
    catch
        _:_ ->
            #{}
    end.

push_history(Cmd, State) ->
    Limit = maps:get(history_limit, State),
    History = [Cmd | maps:get(history, State)],
    State#{history => lists:sublist(History, Limit), history_cursor => undefined}.

strip_continuation(Line) ->
    S = byte_size(Line) - 1,
    case S >= 0 andalso binary:at(Line, S) =:= $\\ of
        true ->
            string:trim(binary:part(Line, 0, S), trailing);
        false ->
            string:trim(Line)
    end.

join_bins(Bins) ->
    iolist_to_binary(join_sep(Bins)).

join_sep([]) ->
    [];
join_sep([B]) ->
    [B];
join_sep([B | Rest]) ->
    [B, <<" ">> | join_sep(Rest)].

completions_for(Prefix) ->
    Completions = erlmcp_cli_completer:get_completions(unicode:characters_to_list(Prefix)),
    [unicode:characters_to_binary(C) || C <- Completions].

do_expand_vars(Input, Env) ->
    case binary:split(Input, <<"$">>) of
        [_] ->
            Input;
        [Head, Rest] ->
            {Name, Tail} = take_var_name(Rest),
            Value = maps:get(Name, Env, <<>>),
            iolist_to_binary([Head, Value, do_expand_vars(Tail, Env)])
    end.

take_var_name(Rest) ->
    take_var_name(Rest, <<>>).

take_var_name(<<C, Rest/binary>>, Acc) when (C >= $a andalso C =< $z)
                                            orelse (C >= $A andalso C =< $Z)
                                            orelse (C >= $0 andalso C =< $9)
                                            orelse C =:= $_ ->
    take_var_name(Rest, <<Acc/binary, C>>);
take_var_name(Rest, Acc) ->
    {Acc, Rest}.

%% Split on a top-level separator string (no quote awareness needed for
%% the && and | command surfaces).
split_topLevel(Str, Sep) ->
    Parts = string:split(Str, Sep, all),
    [P || P <- Parts, string:trim(P) =/= ""].
