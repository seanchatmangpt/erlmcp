#!/usr/bin/env escript
%% -*- erlang -*-
%%! +A0

%% Analyze Erlang export declarations
%% Finds: 1) Exported but undefined functions
%%        2) Defined but not exported functions (potential missing exports)
%%        3) Duplicate exports

-mode(compile).

main([Dir]) ->
    analyze_exports(Dir);
main(_) ->
    io:format("Usage: analyze-exports.erl <directory>~n"),
    halt(1).

analyze_exports(Dir) ->
    Files = find_erl_files(Dir),
    io:format("Found ~p Erlang files to analyze...~n~n", [length(Files)]),
    analyze_files(Files, [], []),
    io:format("~nAnalysis complete.~n"),
    halt(0).

find_erl_files(Dir) ->
    OsCmd = io_lib:format("find ~s -name '*.erl' -type f | grep -v '_build' | grep -v '_archived'", [Dir]),
    Output = os:cmd(OsCmd),
    [string:trim(F) || F <- string:split(Output, "\n", all), F =/= ""].

analyze_files([], _AllExports, _AllDefs) ->
    ok;
analyze_files([File | Rest], AllExports, AllDefs) ->
    case parse_file(File) of
        {ok, {Exports, Defs}} ->
            check_exports(File, Exports, Defs),
            analyze_files(Rest, [{File, Exports} | AllExports], [{File, Defs} | AllDefs]);
        {error, Reason} ->
            io:format(standard_error, "Error parsing ~s: ~p~n", [File, Reason]),
            analyze_files(Rest, AllExports, AllDefs)
    end.

parse_file(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Content = binary_to_list(Bin),
            Exports = extract_exports(Content),
            Defs = extract_definitions(Content),
            {ok, {Exports, Defs}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Extract exports from -export([...]) directives
extract_exports(Content) ->
    Exports = re:run(Content, "-export\\s*\\(\\s*\\[([^\\]]+)\\]\\s*\\)\\s*\\.",
                       [global, {capture, all_but_first, list}, ungreedy]),
    case Exports of
        {match, Matches} ->
            lists:flatten([parse_export_list(Match) || Match <- Matches]);
        nomatch ->
            []
    end.

%% Parse export list like "foo/1, bar/2, baz/0"
parse_export_list(String) ->
    Tokens = re:replace(String, "\\s+", "", [global, {return, list}]),
    FuncStrings = string:split(Tokens, ",", all),
    lists:filtermap(fun parse_function_arity/1, FuncStrings).

%% Parse "function_name/arity"
parse_function_arity("") ->
    false;
parse_function_arity(FuncString) ->
    case string:split(FuncString, "/", all) of
        [Name, ArityStr] ->
            case string:to_integer(ArityStr) of
                {Arity, ""} when is_integer(Arity), Arity >= 0 ->
                    {true, {list_to_atom(Name), Arity}};
                _ ->
                    false
            end;
        _ ->
            false
    end.

%% Extract function definitions
extract_definitions(Content) ->
    %% Match function definitions at module level
    %% This regex looks for "function_name(Args) ->" or "function_name(Args) when"
    %% We need to be careful to avoid matching inside comments or strings
    Lines = string:split(Content, "\n", all),
    extract_definitions_from_lines(Lines, [], in_code, 0, []).

extract_definitions_from_lines([], _Acc, _State, _Depth, Defs) ->
    lists:reverse(Defs);
extract_definitions_from_lines([Line | Rest], Acc, State, Depth, Defs) ->
    {NewState, NewDepth} = process_line_state(Line, State, Depth),
    case NewState of
        in_comment ->
            extract_definitions_from_lines(Rest, Acc, NewState, NewDepth, Defs);
        in_code ->
            case extract_function_def(Line) of
                {ok, Name, Arity} ->
                    extract_definitions_from_lines(Rest, Acc, NewState, NewDepth,
                                                   [{Name, Arity} | Defs]);
                none ->
                    extract_definitions_from_lines(Rest, Acc, NewState, NewDepth, Defs)
            end
    end.

%% Process line to determine if we're in a comment or code
process_line_state(Line, State, Depth) ->
    %% Check for attribute/record/macro definitions (no depth tracking)
    IsDirective = re:run(Line, "^-\\s*(ifdef|ifndef|else|endif|define|undef|record|macro|type|spec|opaque|callback|spec)", [caseless, {capture, none}]),
    %% Check for function/attribute start
    IsFunStart = re:run(Line, "^[a-z]\\w*\\s*\\(", [{capture, none}]),
    IsAttrStart = re:run(Line, "^-\\s*[a-z]", [{capture, none}]),
    %% Check for block start/end
    BlockStart = re:run(Line, "\\b(case|if|receive|begin|try|fun|after|catch)\\b", [caseless, {capture, none}]),
    BlockEnd = re:run(Line, "^\\s*(end|\\])\\.?", [{capture, none}]),
    HasEnd = re:run(Line, "\\bend\\b", [{capture, none}]),
    HasOpen = count_occurrences(Line, "["),
    HasClose = count_occurrences(Line, "]"),
    HasCase = re:run(Line, "\\bcase\\b", [caseless, {capture, none}]),
    HasIf = re:run(Line, "\\bif\\b", [caseless, {capture, none}]),
    HasReceive = re:run(Line, "\\breceive\\b", [caseless, {capture, none}]),
    HasTry = re:run(Line, "\\btry\\b", [caseless, {capture, none}]),
    HasBegin = re:run(Line, "\\bbegin\\b", [caseless, {capture, none}]),
    HasFun = re:run(Line, "\\bfun\\s", [caseless, {capture, none}]),

    %% Simple comment detection
    HasCommentStart = re:run(Line, "%", [{capture, none}]),
    HasBlockCommentStart = re:run(Line, "/\\*", [{capture, none}]),
    HasBlockCommentEnd = re:run(Line, "\\*/", [{capture, none}]),

    %% Calculate depth change
    StartInc = case BlockStart of
                   {match, _} -> 1;
                   nomatch -> 0
               end,
    EndInc = case HasEnd of
                 {match, _} -> 1;
                 nomatch -> 0
             end,

    NewDepth = case {HasCase, HasIf, HasReceive, HasTry, HasBegin} of
                   {{match, _}, _, _, _, _} -> Depth + 1;
                   {_, {match, _}, _, _, _} -> Depth + 1;
                   {_, _, {match, _}, _, _} -> Depth + 1;
                   {_, _, _, {match, _}, _} -> Depth + 1;
                   {_, _, _, _, {match, _}} -> Depth + 1;
                   _ -> Depth
               end,

    FinalDepth = case HasEnd of
                     {match, _} when Depth > 0 -> Depth - 1;
                     _ -> NewDepth
                 end,

    NewState = case State of
                   in_block_comment ->
                       case HasBlockCommentEnd of
                           {match, _} -> in_code;
                           nomatch -> in_block_comment
                       end;
                   in_code ->
                       case HasBlockCommentStart of
                           {match, _} -> in_block_comment;
                           nomatch -> in_code
                       end
               end,
    {NewState, FinalDepth}.

count_occurrences(_String, _Pattern) ->
    nomatch.  %% Placeholder for complex counting

%% Extract function name and arity from a line like "foo(A, B) ->"
extract_function_def(Line) ->
    Trimmed = string:trim(Line),
    case re:run(Trimmed, "^([a-z][a-zA-Z0-9_]*)\\s*\\(([^)]*)\\)\\s*(->|when)",
                 [{capture, [1, 2], list}]) of
        {match, [Name, Args]} when Name =/= "" ->
            Arity = count_arguments(Args),
            {ok, list_to_atom(Name), Arity};
        _ ->
            none
    end.

%% Count arguments in argument list
count_arguments("") ->
    0;
count_arguments(Args) ->
    %% Split by comma, but handle nested tuples, lists, etc.
    %% Simple version: count commas + 1
    Trimmed = string:trim(Args),
    case Trimmed of
        "" -> 0;
        _ -> count_commas_outside_brackets(Trimmed, 0, 0, 0, 0)
    end.

count_commas_outside_brackets([], _, _, _, Count) ->
    Count + 1;
count_commas_outside_brackets([$, | Rest], Paren, Bracket, Brace, Count) when
        Paren =:= 0, Bracket =:= 0, Brace =:= 0 ->
    count_commas_outside_brackets(Rest, Paren, Bracket, Brace, Count + 1);
count_commas_outside_brackets([$( | Rest], Paren, Bracket, Brace, Count) ->
    count_commas_outside_brackets(Rest, Paren + 1, Bracket, Brace, Count);
count_commas_outside_brackets([$) | Rest], Paren, Bracket, Brace, Count) when Paren > 0 ->
    count_commas_outside_brackets(Rest, Paren - 1, Bracket, Brace, Count);
count_commas_outside_brackets($[ | Rest], Paren, Bracket, Brace, Count) ->
    count_commas_outside_brackets(Rest, Paren, Bracket + 1, Brace, Count);
count_commas_outside_brackets($] | Rest], Paren, Bracket, Brace, Count) when Bracket > 0 ->
    count_commas_outside_brackets(Rest, Paren, Bracket - 1, Brace, Count);
count_commas_outside_brackets(${ | Rest], Paren, Bracket, Brace, Count) ->
    count_commas_outside_brackets(Rest, Paren, Bracket, Brace + 1, Count);
count_commas_outside_brackets($} | Rest], Paren, Bracket, Brace, Count) when Brace > 0 ->
    count_commas_outside_brackets(Rest, Paren, Bracket, Brace - 1, Count);
count_commas_outside_brackets([_ | Rest], Paren, Bracket, Brace, Count) ->
    count_commas_outside_brackets(Rest, Paren, Bracket, Brace, Count).

%% Check if exported functions have definitions
check_exports(File, Exports, Defs) ->
    DefSet = sets:from_list(Defs),
    ExportSet = sets:from_list(Exports),

    %% Find exported but undefined
    Undefined = sets:to_list(sets:subtract(ExportSet, DefSet)),
    %% Find defined but not exported (public interface candidates)
    NotExported = sets:to_list(sets:subtract(DefSet, ExportSet)),

    %% Check for duplicates in exports
    DuplicateExports = find_duplicates(Exports),

    %% Report issues
    case {Undefined, NotExported, DuplicateExports} of
        {[], [], []} ->
            ok;  %% No issues
        _ ->
            io:format("~s:~n", [File]),
            print_issues("  Exported but undefined", Undefined),
            print_issues("  Defined but not exported", filter_public_interface(NotExported)),
            print_issues("  Duplicate exports", DuplicateExports),
            io:format("~n")
    end.

%% Filter out internal functions that shouldn't be exported
filter_public_interface(Funcs) ->
    [F || F <- Funcs, not is_internal_function(F)].

is_internal_function({Name, _}) ->
    %% Filter out test functions, internal helpers, etc.
    NameStr = atom_to_list(Name),
    case NameStr of
        "test" ++ _ -> false;
        "_" ++ _ -> false;
        "do_" ++ _ -> false;
        "loop" -> false;
        "init" -> false;
        "handle_call" -> false;
        "handle_cast" -> false;
        "handle_info" -> false;
        "terminate" -> false;
        "code_change" -> false;
        _ -> true
    end.

find_duplicates(List) ->
    Counts = lists:foldl(fun(E, Acc) ->
                                 maps:update_with(E, fun(V) -> V + 1 end, 1, Acc)
                         end, #{}, List),
    [E || E <- maps:keys(Counts), maps:get(E, Counts) > 1].

print_issues(_, []) ->
    ok;
print_issues(Header, Issues) ->
    io:format("~s: ~p~n", [Header, Issues]).
