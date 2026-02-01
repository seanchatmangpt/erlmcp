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

%% API
-export([suggest/1, suggest/2, suggest_next/2, valid_command/1, format_suggestion/2,
         levenshtein_distance/2]).

-define(MAX_DISTANCE, 3).
-define(MIN_SIMILARITY, 0.5).

%%====================================================================
%% API
%%====================================================================

%% @doc Suggest commands based on input (with default threshold)
-spec suggest(string() | binary()) -> [binary()].
suggest(Input) ->
    suggest(Input, ?MAX_DISTANCE).

%% @doc Suggest commands based on input with custom distance threshold
-spec suggest(string() | binary(), non_neg_integer()) -> [binary()].
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
