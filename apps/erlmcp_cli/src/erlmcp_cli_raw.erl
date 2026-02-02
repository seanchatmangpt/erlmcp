%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_raw - OTP 28 Shell Raw Mode for MCP Interactive CLI
%%%
%%% Implements OTP 28's raw mode for immediate keypress handling without
%%% line editing. Use case: Interactive CLI debugging with real-time
%%% character input.
%%%
%%% == OTP 28 Innovation: Raw Mode ==
%%%
%%% - erl -noshell -s erlmcp_cli raw_session
%%% - No line editing, immediate keypress handling
%%% - Direct terminal control for interactive debugging
%%%
%%% == Features ==
%%%
%%% - Raw character input (no buffering)
%%% - Interactive debugging commands
%%% - Context inspection
%%% - Real-time statistics
%%% - Graceful signal handling (Ctrl+C, Ctrl+D)
%%%
%%% == Usage ==
%%%
%%% ```erlang
%%% % Start raw mode session
%%% erlmcp_cli_raw:start_raw_session().
%%%
%%% % Or run directly
%%% erlmcp_cli_raw:run().
%%%
%%% % Or from shell
%%% erl -noshell -s erlmcp_cli_raw run
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_raw).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, run/0, run/1, stop/0]).
-export([start_raw_session/0, send_char/1, inspect_context/0, cancel_operation/0]).
-export([get_stats/0, get_state/0, execute_command/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Macros
-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 60000).
-define(MAX_HISTORY, 100).
-define(CTRL_C, 3).
-define(CTRL_D, 4).
-define(CTRL_L, 12).
-define(CARRIAGE_RETURN, 13).
-define(ESC, 27).

%% Records
-record(state,
        {buffer = <<>> :: binary(),
         history = [] :: [binary()],
         history_index = 0 :: non_neg_integer(),
         session_context = #{} :: map(),
         operation_pid = undefined :: pid() | undefined,
         stats = #{commands => 0, interrupts => 0} :: map(),
         opts = #{} :: map()}).

-type state() :: #state{}.
-type command() :: binary().
-type result() :: {ok, term()} | {error, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start raw mode server with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start raw mode server with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @doc Run raw mode session (blocking)
-spec run() -> ok.
run() ->
    run(#{}).

%% @doc Run raw mode session with options (blocking)
-spec run(map()) -> ok.
run(Opts) ->
    case start_link(Opts) of
        {ok, Pid} ->
            monitor(process, Pid),
            receive
                {'DOWN', _, process, Pid, _} ->
                    ok
            end;
        {error, Reason} ->
            io:format("Failed to start raw mode: ~p~n", [Reason]),
            error
    end.

%% @doc Stop raw mode session
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Start a raw mode interactive session
-spec start_raw_session() -> {ok, pid()}.
start_raw_session() ->
    spawn(fun() -> raw_loop(#{}) end).

%% @doc Send a character to the raw mode session
-spec send_char(char()) -> ok.
send_char(Char) when is_integer(Char), Char >= 0, Char =< 255 ->
    gen_server:cast(?SERVER, {char, Char}).

%% @doc Inspect current context state
-spec inspect_context() -> {ok, map()}.
inspect_context() ->
    gen_server:call(?SERVER, inspect_context).

%% @doc Cancel current operation
-spec cancel_operation() -> ok | {error, term()}.
cancel_operation() ->
    gen_server:call(?SERVER, cancel_operation).

%% @doc Get session statistics
-spec get_stats() -> {ok, map()}.
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%% @doc Get current state
-spec get_state() -> {ok, map()}.
get_state() ->
    gen_server:call(?SERVER, get_state).

%% @doc Execute a command in raw mode
-spec execute_command(command()) -> result().
execute_command(Command) ->
    gen_server:call(?SERVER, {execute_command, Command}, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize raw mode
init(Opts) ->
    process_flag(trap_exit, true),

    % Enable raw mode on stdin
    case enable_raw_mode() of
        ok ->
            State = #state{opts = Opts},
            io:format("~n~s~n~n", [welcome_message()]),
            self() ! start_raw_loop,
            {ok, State};
        {error, Reason} ->
            {stop, {raw_mode_failed, Reason}}
    end.

%% @doc Handle synchronous calls
handle_call(inspect_context, _From, State) ->
    Context = State#state.session_context,
    Reply = format_context(Context),
    {reply, Reply, State};
handle_call(cancel_operation, _From, #state{operation_pid = Pid, stats = Stats} = State) ->
    Reply =
        case Pid of
            undefined ->
                {error, no_operation};
            _ when is_pid(Pid) ->
                exit(Pid, cancel),
                {ok, cancelled}
        end,
    NewStats =
        case Reply of
            {ok, cancelled} ->
                maps:update_with(interrupts, fun(V) -> V + 1 end, 1, Stats);
            _ ->
                Stats
        end,
    {reply, Reply, State#state{operation_pid = undefined, stats = NewStats}};
handle_call(get_stats, _From, State) ->
    Reply = {ok, State#state.stats},
    {reply, Reply, State};
handle_call(get_state, _From, State) ->
    StateMap =
        #{buffer => State#state.buffer,
          history => State#state.history,
          history_index => State#state.history_index,
          session_context => State#state.session_context,
          has_operation => State#state.operation_pid =/= undefined,
          stats => State#state.stats},
    {reply, {ok, StateMap}, State};
handle_call({execute_command, Command}, _From, State) ->
    Result =
        case do_execute_command(Command, State) of
            {ok, Res, _NewState} ->
                {ok, Res};
            {error, Reason} ->
                {error, Reason}
        end,
    NewStats = maps:update_with(commands, fun(V) -> V + 1 end, 1, State#state.stats),
    {reply, Result, State#state{stats = NewStats}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
handle_cast({char, Char}, State) ->
    NewState = handle_char(Char, State),
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(start_raw_loop, State) ->
    % Start raw input loop in separate process
    spawn_link(fun() -> raw_input_loop(self()) end),
    {noreply, State};
handle_info({raw_input, Char}, State) ->
    NewState = handle_char(Char, State),
    {noreply, NewState};
handle_info({'EXIT', Pid, Reason}, State) when Pid =:= State#state.operation_pid ->
    io:format("~nOperation ended: ~p~n", [Reason]),
    {noreply, State#state{operation_pid = undefined}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate raw mode
terminate(_Reason, _State) ->
    disable_raw_mode(),
    io:format("~nRaw mode session ended.~n"),
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Character Handling
%%====================================================================

%% @doc Handle incoming character
-spec handle_char(char(), state()) -> state().
handle_char(Char, State) ->
    case Char of
        ?CTRL_C ->
            % Interrupt signal
            io:format("~n^C~nInterrupted. Type /quit to exit.~n> "),
            State;
        ?CTRL_D ->
            % EOF signal - exit
            io:format("~n^D~n"),
            self() ! stop,
            State;
        ?CTRL_L ->
            % Clear screen
            clear_screen(),
            io:format("> ~s", [State#state.buffer]),
            State;
        ?CARRIAGE_RETURN ->
            % Enter key - process command
            io:format("~n"),
            process_command_buffer(State);
        ?ESC ->
            % Escape sequence - handle ANSI codes
            handle_escape_sequence(State);
        Char when Char >= $\s, Char =< $\~ ->
            % Printable character
            BinChar = <<Char>>,
            NewBuffer = <<(State#state.buffer)/binary, BinChar/binary>>,
            io:format("~c", [Char]),
            State#state{buffer = NewBuffer};
        Char when Char =:= $\b; Char =:= 127 ->
            % Backspace/Delete
            case State#state.buffer of
                <<>> ->
                    State;
                <<_Rest/binary>> ->
                    NewBuffer =
                        binary_part(State#state.buffer, {0, byte_size(State#state.buffer) - 1}),
                    io:format("\b \b"),
                    State#state{buffer = NewBuffer}
            end;
        _ ->
            % Ignore other characters
            State
    end.

%% @doc Handle escape sequences
-spec handle_escape_sequence(state()) -> state().
handle_escape_sequence(State) ->
    % Read next two chars for ANSI sequences
    case get_raw_char() of
        {ok, $[} ->
            case get_raw_char() of
                {ok, $A} ->
                    % Up arrow - history back
                    history_back(State);
                {ok, $B} ->
                    % Down arrow - history forward
                    history_forward(State);
                {ok, $C} ->
                    % Right arrow
                    State;
                {ok, $D} ->
                    % Left arrow
                    State;
                _ ->
                    State
            end;
        _ ->
            State
    end.

%% @doc Process command buffer
-spec process_command_buffer(state()) -> state().
process_command_buffer(#state{buffer = Buffer} = State) ->
    Command = binary:trim(Buffer),

    % Add to history
    NewHistory = add_to_history(Command, State#state.history),

    case Command of
        <<>> ->
            % Empty command
            State#state{buffer = <<>>, history = NewHistory};
        <<$/, $i, $n, $s, $p, $e, $c, $t>> ->
            % /inspect command
            inspect_context_command(State),
            State#state{buffer = <<>>, history = NewHistory};
        <<$/, $c, $a, $n, $c, $e, $l>> ->
            % /cancel command
            cancel_command(State),
            State#state{buffer = <<>>, history = NewHistory};
        <<$/, $s, $t, $a, $t, $s>> ->
            % /stats command
            stats_command(State),
            State#state{buffer = <<>>, history = NewHistory};
        <<$/, $q, $u, $i, $t>> ->
            % /quit command
            self() ! stop,
            State#state{buffer = <<>>, history = NewHistory};
        <<$/, $h, $e, $l, $p>> ->
            % /help command
            help_command(),
            State#state{buffer = <<>>, history = NewHistory};
        _ ->
            % Regular command - execute it
            case do_execute_command(Command, State) of
                {ok, Result, _NewState} ->
                    io:format("~p~n> ", [Result]);
                {error, Reason} ->
                    io:format("Error: ~p~n> ", [Reason])
            end,
            State#state{buffer = <<>>, history = NewHistory}
    end.

%%====================================================================
%% Internal Functions - Commands
%%====================================================================

%% @doc Execute command implementation
-spec do_execute_command(command(), state()) -> {ok, term(), state()} | {error, term()}.
do_execute_command(Command, State) ->
    % Parse command
    case binary:split(Command, <<" ">>) of
        [<<"ping">>] ->
            {ok, pong, State};
        [<<"echo">>, Rest] ->
            {ok, Rest, State};
        [<<"context">>] ->
            {ok, State#state.session_context, State};
        [<<"set">>, Key, Value] ->
            NewContext = maps:put(Key, Value, State#state.session_context),
            {ok, #{key => Key, value => Value}, State#state{session_context = NewContext}};
        [<<"get">>, Key] ->
            case maps:get(Key, State#state.session_context, undefined) of
                undefined ->
                    {error, {key_not_found, Key}};
                Value ->
                    {ok, #{key => Key, value => Value}, State}
            end;
        _ ->
            {error, {unknown_command, Command}}
    end.

%% @doc Inspect context command
-spec inspect_context_command(state()) -> ok.
inspect_context_command(#state{session_context = Context}) ->
    case format_context(Context) of
        {ok, Formatted} ->
            io:format("~s> ", [Formatted]);
        {error, Reason} ->
            io:format("Error inspecting context: ~p> ", [Reason])
    end.

%% @doc Cancel command
-spec cancel_command(state()) -> ok.
cancel_command(#state{operation_pid = Pid}) ->
    case Pid of
        undefined ->
            io:format("No operation to cancel.> ");
        _ ->
            exit(Pid, cancel),
            io:format("Operation cancelled.> ")
    end.

%% @doc Stats command
-spec stats_command(state()) -> ok.
stats_command(#state{stats = Stats, history = History}) ->
    io:format("Statistics:~n"),
    io:format("  Commands executed: ~p~n", [maps:get(commands, Stats, 0)]),
    io:format("  Interrupts: ~p~n", [maps:get(interrupts, Stats, 0)]),
    io:format("  History size: ~p~n> ", [length(History)]).

%% @doc Help command
-spec help_command() -> ok.
help_command() ->
    Help =
        "Raw Mode Commands:\n"
        "  /inspect   - Show context state\n"
        "  /cancel    - Cancel current operation\n"
        "  /stats     - Show statistics\n"
        "  /quit      - Exit raw mode\n"
        "  /help      - Show this help\n"
        "  Ctrl+C     - Interrupt\n"
        "  Ctrl+D     - Exit\n"
        "  Ctrl+L     - Clear screen\n",
    io:format("~s> ", [Help]).

%%====================================================================
%% Internal Functions - History Management
%%====================================================================

%% @doc Add command to history
-spec add_to_history(command(), [command()]) -> [command()].
add_to_history(Command, History) ->
    NewHistory = [Command | History],
    case length(NewHistory) > ?MAX_HISTORY of
        true ->
            lists:sublist(NewHistory, ?MAX_HISTORY);
        false ->
            NewHistory
    end.

%% @doc Navigate history back
-spec history_back(state()) -> state().
history_back(#state{history = [], buffer = Buffer} = State) ->
    % Empty history, just show buffer
    io:format("~s", [Buffer]),
    State;
history_back(#state{history = History, history_index = Index} = State) ->
    case Index < length(History) of
        true ->
            Command = lists:nth(Index + 1, History),
            % Clear current line
            io:format("\r~c[2K", [27]),
            io:format("> ~s", [Command]),
            State#state{buffer = Command, history_index = Index + 1};
        false ->
            State
    end.

%% @doc Navigate history forward
-spec history_forward(state()) -> state().
history_forward(#state{history_index = 0} = State) ->
    % Already at beginning
    State;
history_forward(#state{history = History, history_index = Index} = State) ->
    NewIndex = max(0, Index - 1),
    case NewIndex of
        0 ->
            io:format("\r~c[2K> ", [27]),
            State#state{buffer = <<>>, history_index = 0};
        _ ->
            Command = lists:nth(NewIndex, History),
            io:format("\r~c[2K> ~s", [27, Command]),
            State#state{buffer = Command, history_index = NewIndex}
    end.

%%====================================================================
%% Internal Functions - Raw Mode Utilities
%%====================================================================

%% @doc Enable raw mode on stdin
-spec enable_raw_mode() -> ok | {error, term()}.
enable_raw_mode() ->
    case os:type() of
        {unix, _} ->
            % Use stty to enable raw mode
            case os:cmd("stty -echo raw 2>/dev/null") of
                [] ->
                    ok;
                _ ->
                    ok
            end;
        {win32, _} ->
            % Windows - use PowerShell
            case os:cmd("powershell -Command $InputMode = [Console]::KeyAvailable") of
                _ ->
                    {error, windows_raw_mode_not_supported}
            end;
        _ ->
            {error, unsupported_platform}
    end.

%% @doc Disable raw mode on stdin
-spec disable_raw_mode() -> ok.
disable_raw_mode() ->
    case os:type() of
        {unix, _} ->
            os:cmd("stty echo cooked 2>/dev/null"),
            ok;
        _ ->
            ok
    end.

%% @doc Get a single raw character from stdin
-spec get_raw_char() -> {ok, char()} | {error, term()}.
get_raw_char() ->
    case io:get_chars("", 1) of
        [Char] ->
            {ok, Char};
        eof ->
            {ok, ?CTRL_D};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Clear screen
-spec clear_screen() -> ok.
clear_screen() ->
    io:format("\e[H\e[2J").

%% @doc Format context for display
-spec format_context(map()) -> {ok, string()} | {error, term()}.
format_context(Context) ->
    case maps:size(Context) of
        0 ->
            {ok, "Context: (empty)"};
        _ ->
            Lines = [io_lib:format("  ~s: ~p", [K, V]) || {K, V} <- maps:to_list(Context)],
            {ok, "Context:\n" ++ string:join(Lines, "\n")}
    end.

%% @doc Welcome message
-spec welcome_message() -> string().
welcome_message() ->
    "erlmcp Raw Mode - OTP 28 Interactive Debugging\n"
    "Type /help for commands, Ctrl+D to exit\n"
    "> ".

%%====================================================================
%% External Raw Loop Process
%%====================================================================

%% @doc Raw input loop (runs in separate process)
-spec raw_input_loop(pid()) -> no_return().
raw_input_loop(ServerPid) ->
    case get_raw_char() of
        {ok, Char} ->
            ServerPid ! {raw_input, Char},
            raw_input_loop(ServerPid);
        {error, Reason} ->
            io:format("~nInput error: ~p~n", [Reason]),
            exit({input_error, Reason})
    end.

%% @doc Raw mode loop (standalone)
-spec raw_loop(map()) -> no_return().
raw_loop(State) ->
    case get_raw_char() of
        {ok, ?CTRL_C} ->
            io:format("~n^C~nInterrupted.~n"),
            raw_loop(State);
        {ok, ?CTRL_D} ->
            io:format("~n^D~nExiting.~n"),
            exit(normal);
        {ok, ?CARRIAGE_RETURN} ->
            io:format("~n"),
            raw_loop(State);
        {ok, Char} when Char >= $\s, Char =< $\~ ->
            io:format("~c", [Char]),
            raw_loop(State);
        {ok, _} ->
            raw_loop(State);
        {error, Reason} ->
            io:format("~nInput error: ~p~n", [Reason]),
            exit({input_error, Reason})
    end.
