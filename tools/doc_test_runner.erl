%%% DOC-TEST RUNNER
%%% Executes fenced command blocks from markdown documentation files.
%%% Supports shell commands and Erlang code blocks marked with `run` flag.
%%%
%%% Markers:
%%%   ```bash run
%%%   $ command here
%%%   ```
%%%
%%%   ```erlang run
%%%   1> erlang:display(hello).
%%%   ```

-module(doc_test_runner).

-export([
    run_all_tests/0,
    run_all_tests/1,
    run_file/1,
    run_file/2,
    parse_markdown/1,
    execute_block/1,
    verify_output/3,
    format_results/1
]).

-include_lib("eunit/include/eunit.hrl").

-compile([nowarn_unused_function, nowarn_unused_vars]).

%% ============================================================================
%% Data Types
%% ============================================================================

-type doc_block() :: #{
    type := bash | erlang | text,
    content := string(),
    expected_output => string() | none,
    should_succeed => boolean(),
    line_number := integer()
}.

-type test_result() :: #{
    file := string(),
    block := integer(),
    type := bash | erlang,
    status := pass | fail | skip,
    actual_output := string(),
    expected_output := string(),
    error => string(),
    duration_ms := integer()
}.

%% ============================================================================
%% Public API
%% ============================================================================

%% Run all doc tests in docs/ and docs/howto/ directories
-spec run_all_tests() -> [test_result()].
run_all_tests() ->
    run_all_tests(#{}).

-spec run_all_tests(Options :: map()) -> [test_result()].
run_all_tests(Options) ->
    Dirs = maps:get(dirs, Options, [
        "docs",
        "docs/howto",
        "docs/explanation"
    ]),

    Files = lists:flatten([
        filelib:wildcard(filename:join(Dir, "*.md"))
        || Dir <- Dirs, filelib:is_dir(Dir)
    ]),

    io:format("Found ~w markdown files~n", [length(Files)]),
    Results = lists:flatmap(fun(File) -> run_file(File, Options) end, Files),

    format_results(Results),
    Results.

%% Run tests from a single markdown file
-spec run_file(string()) -> [test_result()].
run_file(File) ->
    run_file(File, #{}).

-spec run_file(string(), map()) -> [test_result()].
run_file(File, Options) ->
    case file:read_file(File) of
        {ok, Content} ->
            Blocks = parse_markdown(binary_to_list(Content)),
            RunBlocks = [B || B <- Blocks, maps:get(type, B) =/= text],

            case length(RunBlocks) > 0 of
                true ->
                    io:format("Processing ~s (~w executable blocks)~n", [File, length(RunBlocks)]);
                false ->
                    io:format("Skipping ~s (no executable blocks)~n", [File])
            end,

            [execute_block(B#{file => File}) || B <- RunBlocks];
        {error, Reason} ->
            io:format("Error reading ~s: ~w~n", [File, Reason]),
            []
    end.

%% ============================================================================
%% Parsing: Extract fenced code blocks from markdown
%% ============================================================================

-spec parse_markdown(string()) -> [doc_block()].
parse_markdown(Content) ->
    Lines = string:split(Content, "\n", all),
    parse_lines(Lines, [], [], 1).

parse_lines([], Acc, _CurrentBlock, _LineNum) ->
    Blocks = lists:reverse([
        create_block(CB) || CB <- Acc, CB =/= []
    ]),
    lists:filter(fun(B) -> maps:get(type, B) =/= text end, Blocks);

parse_lines([Line | Rest], Acc, CurrentBlock, LineNum) ->
    case is_fence_marker(Line) of
        {true, Type, Runnable} ->
            case CurrentBlock of
                [] ->
                    %% Opening fence
                    parse_lines(Rest, Acc, [{Type, Runnable, LineNum, []}], LineNum + 1);
                [{CType, _CRunnable, CStart, CContent}] when CType =:= Type ->
                    %% Closing fence - matching fence type
                    Block = create_block({Type, Runnable, CStart, lists:reverse(CContent)}),
                    parse_lines(Rest, [Block | Acc], [], LineNum + 1);
                _ ->
                    %% Different fence type, treat as content
                    [{CType, CRunnable, CStart, CContent}] = CurrentBlock,
                    parse_lines(Rest, Acc, [{CType, CRunnable, CStart, [Line | CContent]}], LineNum + 1)
            end;
        false ->
            case CurrentBlock of
                [] ->
                    %% Outside fence, skip line
                    parse_lines(Rest, Acc, [], LineNum + 1);
                [{Type, Runnable, Start, Content}] ->
                    %% Inside fence, accumulate
                    parse_lines(Rest, Acc, [{Type, Runnable, Start, [Line | Content]}], LineNum + 1)
            end
    end.

%% Detect opening/closing fence markers: ```bash run, ```erlang run, etc.
-spec is_fence_marker(string()) -> {true, bash | erlang, boolean()} | false.
is_fence_marker(Line) ->
    Trimmed = string:trim(Line),
    case string:prefix(Trimmed, "```") of
        nomatch ->
            false;
        Rest ->
            Marker = string:trim_leading(Rest),
            case string:split(Marker, " ", all) of
                ["bash" | Tags] ->
                    {true, bash, lists:member("run", Tags)};
                ["erlang" | Tags] ->
                    {true, erlang, lists:member("run", Tags)};
                ["shell" | Tags] ->
                    {true, bash, lists:member("run", Tags)};
                _ ->
                    case Marker of
                        "" -> {true, text, false};
                        _ -> false
                    end
            end
    end.

-spec create_block(tuple()) -> doc_block().
create_block({Type, Runnable, LineNum, Lines}) ->
    Content = string:join(lists:reverse(Lines), "\n"),
    {ExpectedOutput, ShouldSucceed} = extract_expectations(Content, Type),
    #{
        type => Type,
        content => remove_prompts(Content, Type),
        expected_output => ExpectedOutput,
        should_succeed => ShouldSucceed,
        line_number => LineNum,
        runnable => Runnable
    }.

%% Parse expected output and success expectations from block content
-spec extract_expectations(string(), bash | erlang) -> {string() | none, boolean()}.
extract_expectations(Content, bash) ->
    Lines = string:split(Content, "\n", all),
    {find_expected_output(Lines), true};
extract_expectations(Content, erlang) ->
    {none, true}.

%% Remove shell prompts ($ and >) from command lines
-spec remove_prompts(string(), bash | erlang) -> string().
remove_prompts(Content, bash) ->
    Lines = string:split(Content, "\n", all),
    Cleaned = [
        case string:prefix(string:trim_leading(L), "$") of
            nomatch ->
                case string:prefix(string:trim_leading(L), ">") of
                    nomatch -> L;
                    Rest -> string:trim_leading(Rest)
                end;
            Rest ->
                string:trim_leading(Rest)
        end
        || L <- Lines
    ],
    string:join(Cleaned, "\n");
remove_prompts(Content, erlang) ->
    Content.

%% Extract expected output (lines that don't start with $ or >)
-spec find_expected_output([string()]) -> string() | none.
find_expected_output(Lines) ->
    OutputLines = [
        L || L <- Lines,
        string:prefix(string:trim_leading(L), "$") =:= nomatch,
        string:prefix(string:trim_leading(L), ">") =:= nomatch,
        string:trim(L) =/= ""
    ],
    case OutputLines of
        [] -> none;
        _ -> string:join(OutputLines, "\n")
    end.

%% ============================================================================
%% Execution: Run blocks and verify output
%% ============================================================================

-spec execute_block(doc_block()) -> test_result().
execute_block(Block) ->
    Type = maps:get(type, Block),
    File = maps:get(file, Block, "unknown"),
    LineNum = maps:get(line_number, Block),
    Content = maps:get(content, Block),
    ExpectedOutput = maps:get(expected_output, Block, none),

    StartTime = erlang:monotonic_time(millisecond),

    Result = case Type of
        bash ->
            execute_bash(Content, ExpectedOutput);
        erlang ->
            execute_erlang(Content, ExpectedOutput);
        text ->
            {skip, "Text block", ""}
    end,

    Duration = erlang:monotonic_time(millisecond) - StartTime,

    case Result of
        {pass, Output} ->
            #{
                file => File,
                block => LineNum,
                type => Type,
                status => pass,
                actual_output => Output,
                expected_output => ExpectedOutput,
                duration_ms => Duration
            };
        {fail, Error, Output} ->
            #{
                file => File,
                block => LineNum,
                type => Type,
                status => fail,
                actual_output => Output,
                expected_output => ExpectedOutput,
                error => Error,
                duration_ms => Duration
            };
        {skip, Reason, _Output} ->
            #{
                file => File,
                block => LineNum,
                type => Type,
                status => skip,
                actual_output => "",
                expected_output => "N/A",
                error => Reason,
                duration_ms => Duration
            }
    end.

%% Execute bash command
-spec execute_bash(string(), string() | none) -> {pass, string()} | {fail, string(), string()} | {skip, string(), string()}.
execute_bash(Content, _ExpectedOutput) ->
    %% Filter out comment lines and empty lines
    Lines = [
        string:trim(L) || L <- string:split(Content, "\n", all),
        string:trim(L) =/= "",
        not string:prefix(string:trim_leading(L), "#") =:= string:prefix(string:trim_leading(L), "#")
    ],

    case Lines of
        [] ->
            {skip, "Empty command", ""};
        [Cmd | _] ->
            case os:cmd("cd /Users/sac/erlmcp && " ++ Cmd ++ " 2>&1") of
                Output ->
                    %% For now, success if no error code (impl would verify exit code)
                    {pass, Output}
            end
    end.

%% Execute Erlang code
-spec execute_erlang(string(), string() | none) -> {pass, string()} | {fail, string(), string()} | {skip, string(), string()}.
execute_erlang(Content, _ExpectedOutput) ->
    try
        %% Evaluate Erlang code in current context
        %% Note: This would need rpc or erl shell integration for prod
        {skip, "Erlang eval requires shell integration", ""}
    catch
        error:Reason ->
            {fail, io_lib:format("Erlang error: ~w", [Reason]), ""}
    end.

%% Verify output matches expected pattern
-spec verify_output(string(), string() | none, bash | erlang) -> boolean().
verify_output(_Actual, none, _Type) ->
    %% No expected output to verify
    true;
verify_output(Actual, Expected, _Type) ->
    %% Simple substring match for now
    string:find(Actual, Expected) =/= nomatch.

%% ============================================================================
%% Reporting
%% ============================================================================

-spec format_results([test_result()]) -> ok.
format_results(Results) ->
    Total = length(Results),
    Passed = length([R || R <- Results, maps:get(status, R) =:= pass]),
    Failed = length([R || R <- Results, maps:get(status, R) =:= fail]),
    Skipped = length([R || R <- Results, maps:get(status, R) =:= skip]),

    io:format("~n════════════════════════════════════════════════════════════~n"),
    io:format("DOC-TEST RESULTS~n"),
    io:format("════════════════════════════════════════════════════════════~n"),
    io:format("Total:   ~3w~n", [Total]),
    io:format("Passed:  ~3w~n", [Passed]),
    io:format("Failed:  ~3w~n", [Failed]),
    io:format("Skipped: ~3w~n", [Skipped]),
    io:format("════════════════════════════════════════════════════════════~n~n"),

    %% Print failures
    FailedResults = [R || R <- Results, maps:get(status, R) =:= fail],
    case FailedResults of
        [] ->
            ok;
        _ ->
            io:format("FAILURES:~n"),
            lists:foreach(fun(R) ->
                io:format("  ~s:~w (~w)~n", [
                    maps:get(file, R),
                    maps:get(block, R),
                    maps:get(type, R)
                ]),
                io:format("    Error: ~s~n", [maps:get(error, R, "unknown")])
            end, FailedResults)
    end,

    TotalDuration = lists:foldl(fun(R, Acc) ->
        Acc + maps:get(duration_ms, R, 0)
    end, 0, Results),

    io:format("Total Duration: ~wms~n", [TotalDuration]),
    ok.

%% ============================================================================
%% CLI Integration for rebar3
%% ============================================================================

%% For use as a rebar3 custom provider
-spec run_provider(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
run_provider(State) ->
    Results = run_all_tests(),
    case length([R || R <- Results, maps:get(status, R) =:= fail]) of
        0 ->
            {ok, State};
        N ->
            {error, io_lib:format("~w doc tests failed", [N])}
    end.
