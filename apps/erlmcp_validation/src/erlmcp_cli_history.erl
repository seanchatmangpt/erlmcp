%%%-------------------------------------------------------------------
%%% @doc
%%% CLI History Manager - Command history storage and retrieval
%%%
%%% Manages command history with persistence to ~/.erlmcp/history
%%% Provides search, replay, and navigation capabilities.
%%%
%%% == Features ==
%%%
%%% - In-memory history with ETS
%%% - Persistent storage to ~/.erlmcp/history
%%% - History search and filtering
%%% - Command replay (!n syntax)
%%% - Configurable max history size
%%% - Duplicate suppression
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_history).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, add/1, get/1, get_all/0, search/1, clear/0, size/0, save/0,
         load/0, stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Default configuration
-define(DEFAULT_MAX_SIZE, 1000).
-define(DEFAULT_HISTORY_FILE, ".erlmcp/history").
-define(SAVE_INTERVAL, 30000). % Save every 30 seconds

%% State record
-record(state,
        {history = [] :: [string()],
         max_size = ?DEFAULT_MAX_SIZE :: pos_integer(),
         history_file :: file:filename(),
         save_timer :: reference() | undefined,
         suppress_duplicates = true :: boolean()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start history manager with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start history manager with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Add command to history
-spec add(string()) -> ok.
add(Command) when is_list(Command) ->
    gen_server:cast(?MODULE, {add, Command}).

%% @doc Get command by index (1-based, most recent = 1)
-spec get(pos_integer()) -> {ok, string()} | {error, not_found}.
get(Index) when is_integer(Index), Index > 0 ->
    gen_server:call(?MODULE, {get, Index}).

%% @doc Get all history commands (most recent first)
-spec get_all() -> [string()].
get_all() ->
    gen_server:call(?MODULE, get_all).

%% @doc Search history for commands matching pattern
-spec search(string()) -> [string()].
search(Pattern) ->
    gen_server:call(?MODULE, {search, Pattern}).

%% @doc Clear all history
-spec clear() -> ok.
clear() ->
    gen_server:call(?MODULE, clear).

%% @doc Get history size
-spec size() -> non_neg_integer().
size() ->
    gen_server:call(?MODULE, size).

%% @doc Save history to disk
-spec save() -> ok | {error, term()}.
save() ->
    gen_server:call(?MODULE, save).

%% @doc Load history from disk
-spec load() -> ok | {error, term()}.
load() ->
    gen_server:call(?MODULE, load).

%% @doc Stop history manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize history manager
init(Opts) ->
    process_flag(trap_exit, true),

    % Determine history file location
    HomeDir = os:getenv("HOME", "/tmp"),
    DefaultFile = filename:join(HomeDir, ?DEFAULT_HISTORY_FILE),
    HistoryFile = maps:get(history_file, Opts, DefaultFile),

    % Ensure directory exists
    ok = filelib:ensure_dir(HistoryFile),

    State =
        #state{max_size = maps:get(max_size, Opts, ?DEFAULT_MAX_SIZE),
               history_file = HistoryFile,
               suppress_duplicates = maps:get(suppress_duplicates, Opts, true)},

    % Load existing history
    State2 =
        case load_history_from_file(HistoryFile) of
            {ok, History} ->
                State#state{history = History};
            {error, _} ->
                State
        end,

    % Start auto-save timer
    Timer = erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
    State3 = State2#state{save_timer = Timer},

    {ok, State3}.

%% @doc Handle synchronous calls
handle_call({get, Index}, _From, State) ->
    Reply =
        case Index =< length(State#state.history) of
            true ->
                Command = lists:nth(Index, State#state.history),
                {ok, Command};
            false ->
                {error, not_found}
        end,
    {reply, Reply, State};
handle_call(get_all, _From, State) ->
    {reply, State#state.history, State};
handle_call({search, Pattern}, _From, State) ->
    Matches = [Cmd || Cmd <- State#state.history, string:find(Cmd, Pattern) =/= nomatch],
    {reply, Matches, State};
handle_call(clear, _From, State) ->
    NewState = State#state{history = []},
    {reply, ok, NewState};
handle_call(size, _From, State) ->
    {reply, length(State#state.history), State};
handle_call(save, _From, State) ->
    Reply = save_history_to_file(State#state.history, State#state.history_file),
    {reply, Reply, State};
handle_call(load, _From, State) ->
    case load_history_from_file(State#state.history_file) of
        {ok, History} ->
            {reply, ok, State#state{history = History}};
        {error, Reason} = Error ->
            {reply, Error, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous casts
handle_cast({add, Command}, State) ->
    % Trim whitespace
    Trimmed = string:trim(Command),

    % Skip empty commands
    NewState =
        case Trimmed of
            "" ->
                State;
            _ ->
                add_to_history(Trimmed, State)
        end,
    {noreply, NewState};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(auto_save, State) ->
    % Save history to disk
    _ = save_history_to_file(State#state.history, State#state.history_file),

    % Restart timer
    Timer = erlang:send_after(?SAVE_INTERVAL, self(), auto_save),
    {noreply, State#state{save_timer = Timer}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Cleanup on termination
terminate(_Reason, State) ->
    % Cancel timer
    case State#state.save_timer of
        undefined ->
            ok;
        Timer ->
            erlang:cancel_timer(Timer)
    end,

    % Final save
    _ = save_history_to_file(State#state.history, State#state.history_file),
    ok.

%% @doc Handle code changes
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Add command to history with duplicate suppression
-spec add_to_history(string(), #state{}) -> #state{}.
add_to_history(Command, State) ->
    History = State#state.history,
    MaxSize = State#state.max_size,
    SuppressDuplicates = State#state.suppress_duplicates,

    % Remove duplicate if suppression is enabled
    History2 =
        case SuppressDuplicates of
            true ->
                lists:delete(Command, History);
            false ->
                History
        end,

    % Add to front of history
    History3 = [Command | History2],

    % Trim to max size
    History4 =
        case length(History3) > MaxSize of
            true ->
                lists:sublist(History3, MaxSize);
            false ->
                History3
        end,

    State#state{history = History4}.

%% @doc Save history to file
-spec save_history_to_file([string()], file:filename()) -> ok | {error, term()}.
save_history_to_file(History, FilePath) ->
    % Reverse to save oldest first
    Lines = lists:reverse(History),
    Content = string:join(Lines, "\n"),

    case file:write_file(FilePath, Content) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Load history from file
-spec load_history_from_file(file:filename()) -> {ok, [string()]} | {error, term()}.
load_history_from_file(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            Content = binary_to_list(Binary),
            Lines = string:split(Content, "\n", all),
            % Filter empty lines and reverse (most recent first)
            History = lists:reverse([L || L <- Lines, string:trim(L) =/= ""]),
            {ok, History};
        {error, enoent} ->
            % File doesn't exist yet, that's ok
            {ok, []};
        {error, Reason} ->
            {error, Reason}
    end.
