%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Smart Suggestion Engine
%%%
%%% Provides smart command suggestions using Levenshtein distance
%%% and prefix matching. Helps users discover commands and fix typos.
%%%
%%% == Features ==
%%%
%%% - Fuzzy command matching with Levenshtein distance
%%% - "Did you mean?" suggestions
%%% - Prefix-based suggestions
%%% - Context-aware help
%%% - Command history tracking (optional)
%%%
%%% == Usage ==
%%%
%%% ```
%%% % Get suggestions for a typo
%%% erlmcp_cli_suggester:suggest("valdate")
%%% %% => [<<"validate">>, <<"validate-check">>]
%%%
%%% % Check if command exists
%%% erlmcp_cli_suggester:valid_command("validate")
%%% %% => true
%%%
%%% % Get contextual suggestions
%%% erlmcp_cli_suggester:suggest_next("transport-check", [])
%%% %% => [<<"stdio">>, <<"tcp">>, <<"http">>, <<"websocket">>]
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_suggester).

-behaviour(gen_server).

%% Module-level API (used by erlmcp_validate_cli parse_args fallback)
-export([suggest/1, suggest/2, suggest_next/2, valid_command/1, format_suggestion/2,
         levenshtein_distance/2]).
%% Instance API (tests-as-spec L184)
-export([start_link/0, start_link/1, stop/1, record_command/2, set_context/2,
         suggest_by_frequency/1, suggest_by_recency/1, suggest_corrections/2,
         register_command/3, suggest_flags/2, get_learned_patterns/1,
         get_ranked_suggestions/1, suggest_filtered/2, suggest/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(MAX_DISTANCE, 3).
-define(MIN_SIMILARITY, 0.5).
-define(SERVER, erlmcp_cli_suggester).

-record(state,
        {commands = [] :: [binary()],
         history = #{} :: #{binary() => {Count :: non_neg_integer(), Last :: integer()}},
         context = #{} :: map(),
         flags = #{} :: #{binary() => [{flag, binary(), [binary()]}]}}).

%%====================================================================
%% API
%%====================================================================

%% @doc Suggest commands based on input (with default threshold)
-spec suggest(string() | binary()) -> [binary()].
suggest(Input) ->
    suggest(Input, ?MAX_DISTANCE).

%% @doc Suggest commands based on input with custom distance threshold
-spec suggest(pid() | string() | binary(), non_neg_integer() | string() | binary()) ->
                 [binary()].
%% Instance form: suggest(SuggesterPid, Prefix) — history + commands
suggest(Suggester, Input) when is_pid(Suggester) ->
    gen_server:call(Suggester, {suggest, to_bin(Input)});
suggest(Input, MaxDistance) when is_binary(Input) ->
    suggest(binary_to_list(Input), MaxDistance);
suggest(Input, MaxDistance) when is_list(Input) ->
    Commands = erlmcp_cli_completer:available_commands(),

    %% Calculate distances for all commands
    Distances = [{Cmd, levenshtein_distance(Input, binary_to_list(Cmd))} || Cmd <- Commands],

    %% Filter by max distance
    Filtered = [{Cmd, Dist} || {Cmd, Dist} <- Distances, Dist =< MaxDistance],

    %% Sort by distance (closest first)
    Sorted = lists:sort(fun({_, D1}, {_, D2}) -> D1 =< D2 end, Filtered),

    %% Also include prefix matches (even if distance is high)
    PrefixMatches = prefix_matches(Input, Commands),

    %% Combine and deduplicate
    SuggestedCmds = [Cmd || {Cmd, _} <- Sorted],
    Combined = lists:usort(SuggestedCmds ++ PrefixMatches),

    %% Limit to top 5 suggestions
    lists:sublist(Combined, 5).

%% @doc Suggest next argument based on command and current args
-spec suggest_next(string() | binary(), [string() | binary()]) -> [binary()].
suggest_next(Command, Args) when is_binary(Command) ->
    suggest_next(binary_to_list(Command), Args);
suggest_next(Command, Args) when is_list(Command) ->
    case normalize_command(Command) of
        "validate" ->
            case length(Args) of
                0 ->
                    [<<"stdio://">>, <<"tcp://">>, <<"http://">>, <<"https://">>];
                _ ->
                    erlmcp_cli_completer:command_options(validate)
            end;
        "spec-check" ->
            erlmcp_cli_completer:command_options(spec_check);
        "transport-check" ->
            case length(Args) of
                0 ->
                    erlmcp_cli_completer:transport_names();
                _ ->
                    erlmcp_cli_completer:command_options(transport_check)
            end;
        "report" ->
            erlmcp_cli_completer:command_options(report);
        "transport" ->
            case length(Args) of
                0 ->
                    erlmcp_cli_completer:transport_names();
                _ ->
                    erlmcp_cli_completer:command_options(transport)
            end;
        "run" ->
            erlmcp_cli_completer:command_options(run);
        "protocol" ->
            erlmcp_cli_completer:command_options(protocol);
        "--gen-completions" ->
            case length(Args) of
                0 ->
                    [<<"bash">>, <<"zsh">>, <<"fish">>];
                _ ->
                    []
            end;
        _ ->
            []
    end.

%% @doc Check if command is valid
-spec valid_command(string() | binary()) -> boolean().
valid_command(Input) when is_binary(Input) ->
    valid_command(binary_to_list(Input));
valid_command(Input) when is_list(Input) ->
    Commands = erlmcp_cli_completer:available_commands(),
    InputBin = list_to_binary(Input),
    lists:member(InputBin, Commands).

%% @doc Format suggestion message
-spec format_suggestion(string() | binary(), [binary()]) -> binary().
format_suggestion(_Input, []) ->
    <<"No suggestions available.">>;
format_suggestion(Input, Suggestions) when is_binary(Input) ->
    format_suggestion(binary_to_list(Input), Suggestions);
format_suggestion(Input, [Single]) ->
    iolist_to_binary(io_lib:format("Unknown command: '~s'. Did you mean '~s'?", [Input, Single]));
format_suggestion(Input, Suggestions) ->
    SuggestionList = string:join([binary_to_list(S) || S <- Suggestions], "', '"),
    iolist_to_binary(io_lib:format("Unknown command: '~s'. Did you mean one of these: '~s'?",
                                   [Input, SuggestionList])).

%% @doc Calculate Levenshtein distance between two strings
-spec levenshtein_distance(string(), string()) -> non_neg_integer().
levenshtein_distance(S1, S2) ->
    levenshtein_distance(S1, S2, #{}).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Calculate Levenshtein distance with memoization
-spec levenshtein_distance(string(), string(), map()) -> non_neg_integer().
levenshtein_distance([], S2, _Cache) ->
    length(S2);
levenshtein_distance(S1, [], _Cache) ->
    length(S1);
levenshtein_distance(S1, S2, Cache) ->
    levenshtein_matrix(S1, S2, Cache).

%% @doc Build Levenshtein distance matrix
-spec levenshtein_matrix(string(), string(), map()) -> non_neg_integer().
levenshtein_matrix(S1, S2, _Cache) ->
    Len1 = length(S1),
    Len2 = length(S2),

    %% Initialize matrix
    InitRow = lists:seq(0, Len2),
    InitMatrix = [{0, InitRow}],

    %% Calculate matrix row by row
    FinalMatrix =
        lists:foldl(fun(I, Matrix) ->
                       Char1 = lists:nth(I, S1),
                       PrevRow = proplists:get_value(I - 1, Matrix),
                       NewRow = calculate_row(Char1, S2, PrevRow, I),
                       [{I, NewRow} | Matrix]
                    end,
                    InitMatrix,
                    lists:seq(1, Len1)),

    %% Get final distance from bottom-right cell
    LastRow = proplists:get_value(Len1, FinalMatrix),
    lists:last(LastRow).

%% @doc Calculate a single row of the Levenshtein matrix
-spec calculate_row(char(), string(), [non_neg_integer()], non_neg_integer()) ->
                       [non_neg_integer()].
calculate_row(Char1, S2, PrevRow, RowIndex) ->
    InitAcc = {[RowIndex], RowIndex},
    {Row, _} =
        lists:foldl(fun(J, {RowAcc, PrevCell}) ->
                       Char2 = lists:nth(J, S2),
                       Cost =
                           case Char1 =:= Char2 of
                               true ->
                                   0;
                               false ->
                                   1
                           end,

                       Above = lists:nth(J + 1, PrevRow),
                       Left = PrevCell,
                       Diag = lists:nth(J, PrevRow),

                       NewCell =
                           lists:min([Above + 1,      %% Deletion
                                      Left + 1,       %% Insertion
                                      Diag + Cost]),     %% Substitution

                       {RowAcc ++ [NewCell], NewCell}
                    end,
                    InitAcc,
                    lists:seq(1, length(S2))),

    Row.

%% @doc Find commands that start with given prefix
-spec prefix_matches(string(), [binary()]) -> [binary()].
prefix_matches(Prefix, Commands) ->
    PrefixLower = string:to_lower(Prefix),
    [Cmd
     || Cmd <- Commands,
        string:str(
            string:to_lower(binary_to_list(Cmd)), PrefixLower)
        =:= 1].

%% @doc Normalize command name (remove dashes, lowercase)
-spec normalize_command(string()) -> string().
normalize_command(Cmd) ->
    string:to_lower(Cmd).

%% @doc Calculate similarity score (0.0 to 1.0)
-spec similarity_score(string(), string()) -> float().
similarity_score(S1, S2) ->
    Distance = levenshtein_distance(S1, S2),
    MaxLen = max(length(S1), length(S2)),
    case MaxLen of
        0 ->
            1.0;
        _ ->
            1.0 - Distance / MaxLen
    end.

%% @doc Filter suggestions by minimum similarity
-spec filter_by_similarity(string(), [binary()], float()) -> [binary()].
filter_by_similarity(Input, Commands, MinSimilarity) ->
    WithScores = [{Cmd, similarity_score(Input, binary_to_list(Cmd))} || Cmd <- Commands],
    Filtered = [{Cmd, Score} || {Cmd, Score} <- WithScores, Score >= MinSimilarity],
    Sorted = lists:sort(fun({_, S1}, {_, S2}) -> S1 >= S2 end, Filtered),
    [Cmd || {Cmd, _} <- Sorted].

%%====================================================================
%% Instance API (tests-as-spec L184): a real gen_server tracking command
%% history, usage frequency/recency, context, per-command flags, and
%% learned patterns.
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, #{}, []).

start_link(Opts) when is_list(Opts) ->
    Commands = [to_bin(C) || {command, C} <- Opts],
    gen_server:start_link({local, ?SERVER}, ?MODULE, #{commands => Commands}, []);
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

stop(Pid) when is_pid(Pid) ->
    catch gen_server:stop(Pid),
    ok.

record_command(Pid, Cmd) ->
    gen_server:call(Pid, {record_command, to_bin(Cmd)}).

set_context(Pid, Context) when is_map(Context) ->
    gen_server:call(Pid, {set_context, Context}).

suggest_by_frequency(Pid) ->
    gen_server:call(Pid, suggest_by_frequency).

suggest_by_recency(Pid) ->
    gen_server:call(Pid, suggest_by_recency).

suggest_corrections(Pid, Input) ->
    gen_server:call(Pid, {suggest_corrections, to_bin(Input)}).

register_command(Pid, Cmd, Spec) ->
    gen_server:call(Pid, {register_command, to_bin(Cmd), Spec}).

suggest_flags(Pid, Cmd) ->
    gen_server:call(Pid, {suggest_flags, to_bin(Cmd)}).

get_learned_patterns(Pid) ->
    gen_server:call(Pid, get_learned_patterns).

get_ranked_suggestions(Pid) ->
    gen_server:call(Pid, get_ranked_suggestions).

suggest_filtered(Pid, Pred) when is_function(Pred, 1) ->
    gen_server:call(Pid, {suggest_filtered, Pred}).

to_bin(B) when is_binary(B) ->
    B;
to_bin(L) when is_list(L) ->
    iolist_to_binary(L).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    {ok, #state{commands = maps:get(commands, Opts, [])}}.

handle_call({record_command, Cmd}, _From, State) ->
    {Count, _} = maps:get(Cmd, State#state.history, {0, 0}),
    Entry = {Count + 1, erlang:monotonic_time(nanosecond)},
    Commands = case lists:member(Cmd, State#state.commands) of
                   true -> State#state.commands;
                   false -> State#state.commands ++ [Cmd]
               end,
    {reply, ok,
     State#state{history = maps:put(Cmd, Entry, State#state.history),
                 commands = Commands}};
handle_call({set_context, Context}, _From, State) ->
    {reply, ok, State#state{context = Context}};
handle_call({suggest, Prefix}, _From, State) ->
    {reply, {ok, do_suggest(Prefix, State)}, State};
handle_call(suggest_by_frequency, _From, State) ->
    {reply, {ok, rank_freq(State#state.history)}, State};
handle_call(suggest_by_recency, _From, State) ->
    {reply, {ok, rank_recency(State#state.history)}, State};
handle_call({suggest_corrections, Input}, _From, State) ->
    Corrections =
        [C || C <- State#state.commands,
              levenshtein_distance(binary_to_list(Input), binary_to_list(C))
              =< ?MAX_DISTANCE],
    {reply, {ok, Corrections}, State};
handle_call({register_command, Cmd, Spec}, _From, State) ->
    Commands = case lists:member(Cmd, State#state.commands) of
                   true -> State#state.commands;
                   false -> State#state.commands ++ [Cmd]
               end,
    {reply, ok,
     State#state{flags = maps:put(Cmd, Spec, State#state.flags), commands = Commands}};
handle_call({suggest_flags, Cmd}, _From, State) ->
    Spec = maps:get(Cmd, State#state.flags, []),
    {reply, {ok, [Flag || {flag, Flag, _} <- Spec]}, State};
handle_call(get_learned_patterns, _From, State) ->
    Patterns =
        maps:map(fun(_Cmd, {Count, _Last}) ->
                    Count
                 end,
                 State#state.history),
    {reply, {ok, Patterns}, State};
handle_call(get_ranked_suggestions, _From, State) ->
    {reply, {ok, rank_freq(State#state.history)}, State};
handle_call({suggest_filtered, Pred}, _From, State) ->
    Filtered = [Cmd || Cmd <- maps:keys(State#state.history), Pred(Cmd)],
    {reply, {ok, Filtered}, State};
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

%%====================================================================
%% Instance internals
%%====================================================================

%% Prefix match against history + commands, then distance-based corrections.
do_suggest(<<>>, State) ->
    maps:keys(State#state.history) ++ State#state.commands;
do_suggest(Prefix, State) ->
    PrefixLower = string:lowercase(Prefix),
    HistoryMatches =
        [C
         || C <- maps:keys(State#state.history),
            string:prefix(string:lowercase(C), PrefixLower) =/= nomatch],
    CommandMatches =
        [C
         || C <- State#state.commands,
            string:prefix(string:lowercase(C), PrefixLower) =/= nomatch
            orelse levenshtein_distance(binary_to_list(Prefix), binary_to_list(C))
                   =< ?MAX_DISTANCE],
    lists:usort(HistoryMatches ++ CommandMatches).

rank_freq(History) ->
    Ranked =
        lists:sort(fun({_, {C1, _}}, {_, {C2, _}}) ->
                      C1 >= C2
                   end,
                   maps:to_list(History)),
    [Cmd || {Cmd, _} <- Ranked].

rank_recency(History) ->
    Ranked =
        lists:sort(fun({_, {_, T1}}, {_, {_, T2}}) ->
                      T1 >= T2
                   end,
                   maps:to_list(History)),
    [Cmd || {Cmd, _} <- Ranked].
