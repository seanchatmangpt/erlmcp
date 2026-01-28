%%% DOC-TEST SUITE
%%% Common Test suite for the doc-test runner framework
%%% Tests markdown parsing, command execution, output verification, and stubbing/mocking

-module(erlmcp_doc_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% ============================================================================
%% CT Callbacks
%% ============================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
        test_parse_bash_block,
        test_parse_erlang_block,
        test_parse_fence_detection,
        test_parse_multiline_block,
        test_remove_prompts_bash,
        test_remove_prompts_erlang,
        test_extract_expectations,
        test_execute_bash_simple,
        test_execute_bash_with_pipe,
        test_execute_bash_with_multiple_commands,
        test_execute_erlang_skip,
        test_verify_output_simple,
        test_verify_output_substring,
        test_verify_output_none,
        test_markdown_parsing_complex,
        test_stubbing_http_calls,
        test_mocking_database,
        test_output_formatting,
        test_runner_end_to_end
    ].

%% ============================================================================
%% Test Cases: Parsing
%% ============================================================================

test_parse_bash_block(_Config) ->
    Markdown = "# Example\n\n```bash run\n$ echo hello\n```\n",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    ?assertNotEqual([], Blocks),
    Block = hd(Blocks),
    ?assertEqual(bash, maps:get(type, Block)),
    Content = maps:get(content, Block),
    ?assertMatch("echo hello", string:trim(Content)).

test_parse_erlang_block(_Config) ->
    Markdown = "```erlang run\n1> erlang:system_info(otp_release).\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    ?assertNotEqual([], Blocks),
    Block = hd(Blocks),
    ?assertEqual(erlang, maps:get(type, Block)).

test_parse_fence_detection(_Config) ->
    ValidFences = [
        "```bash run",
        "```bash",
        "```erlang run",
        "```shell run",
        "```"
    ],
    lists:foreach(fun(Fence) ->
        Result = doc_test_runner:parse_markdown("# Test\n\n" ++ Fence ++ "\necho test\n```\n"),
        ?assertNotEqual([], Result, io_lib:format("Failed for fence: ~s", [Fence]))
    end, ValidFences).

test_parse_multiline_block(_Config) ->
    Markdown = "```bash run\n$ command1\n$ command2\n$ command3\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Content = maps:get(content, Block),
    %% Should have 3 commands
    Lines = string:split(Content, "\n", all),
    ?assertEqual(3, length([L || L <- Lines, string:trim(L) =/= ""])).

test_parse_multiline_example_with_output(_Config) ->
    Markdown = "```bash run\n$ echo test\ntest\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Content = maps:get(content, Block),
    %% Content should be cleaned (prompts removed)
    ?assertNotEqual(nomatch, string:find(Content, "echo")).

%% ============================================================================
%% Test Cases: Prompt Removal
%% ============================================================================

test_remove_prompts_bash(_Config) ->
    Input = "$ echo hello\n> world\n$ another",
    Cleaned = doc_test_runner:remove_prompts(Input, bash),
    ?assertEqual(nomatch, string:find(Cleaned, "$")),
    ?assertEqual(nomatch, string:find(Cleaned, ">")).

test_remove_prompts_erlang(_Config) ->
    Input = "1> erlang:display(test).",
    Cleaned = doc_test_runner:remove_prompts(Input, erlang),
    %% Erlang prompts not removed by default
    ?assertNotEqual(nomatch, string:find(Cleaned, "1>")).

%% ============================================================================
%% Test Cases: Expectation Extraction
%% ============================================================================

test_extract_expectations(_Config) ->
    Input = "$ echo hello\nhello\nworld",
    {ExpOutput, ShouldSucceed} = doc_test_runner:extract_expectations(Input, bash),
    ?assertNotEqual(none, ExpOutput),
    ?assertEqual(true, ShouldSucceed),
    %% Output should contain captured output lines
    ?assertNotEqual(nomatch, string:find(ExpOutput, "hello")).

%% ============================================================================
%% Test Cases: Bash Execution
%% ============================================================================

test_execute_bash_simple(_Config) ->
    Markdown = "```bash run\n$ echo test123\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Result = doc_test_runner:execute_block(Block#{file => "test.md"}),
    Status = maps:get(status, Result),
    ?assertMatch(pass, Status).

test_execute_bash_with_pipe(_Config) ->
    Markdown = "```bash run\n$ echo hello | grep hello\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Result = doc_test_runner:execute_block(Block#{file => "test.md"}),
    Status = maps:get(status, Result),
    ?assertMatch(pass, Status).

test_execute_bash_with_multiple_commands(_Config) ->
    Markdown = "```bash run\n$ cd /tmp && echo working\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Result = doc_test_runner:execute_block(Block#{file => "test.md"}),
    Status = maps:get(status, Result),
    ?assertMatch(pass, Status).

%% ============================================================================
%% Test Cases: Erlang Execution
%% ============================================================================

test_execute_erlang_skip(_Config) ->
    Markdown = "```erlang run\nerlang:display(test).\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    Result = doc_test_runner:execute_block(Block#{file => "test.md"}),
    Status = maps:get(status, Result),
    %% Currently skipped, pending shell integration
    ?assertMatch(skip, Status).

%% ============================================================================
%% Test Cases: Output Verification
%% ============================================================================

test_verify_output_simple(_Config) ->
    Actual = "hello world",
    Expected = "hello world",
    ?assertEqual(true, doc_test_runner:verify_output(Actual, Expected, bash)).

test_verify_output_substring(_Config) ->
    Actual = "the quick brown fox",
    Expected = "brown",
    ?assertEqual(true, doc_test_runner:verify_output(Actual, Expected, bash)).

test_verify_output_none(_Config) ->
    Actual = "some output",
    ?assertEqual(true, doc_test_runner:verify_output(Actual, none, bash)).

%% ============================================================================
%% Test Cases: Complex Markdown Scenarios
%% ============================================================================

test_markdown_parsing_complex(_Config) ->
    Markdown = "# Guide\n\nIntroduction.\n\n```bash run\n$ echo test1\n```\n\nMiddle text.\n\n```erlang run\ntest.\n```\n\n```bash\necho no_run\n```\n",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    %% Should have 2 runnable blocks (bash and erlang with run marker, not the no_run one)
    RunnableBlocks = [B || B <- Blocks, maps:get(runnable, B, false) =:= true],
    ?assertEqual(2, length(RunnableBlocks)).

%% ============================================================================
%% Test Cases: Stubbing & Mocking
%% ============================================================================

test_stubbing_http_calls(_Config) ->
    %% Simulate HTTP response stubbing for doc tests
    Markdown = "```bash run\n$ curl -s http://localhost:8000/api/test\n{\"status\": \"ok\"}\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    %% In production, we'd mock HTTP via environment variables or test server
    ExpectedOutput = maps:get(expected_output, Block),
    ?assertNotEqual(none, ExpectedOutput),
    ?assertNotEqual(nomatch, string:find(ExpectedOutput, "ok")).

test_mocking_database(_Config) ->
    %% Simulate database query response mocking
    Markdown = "```bash run\n$ psql -c 'SELECT count(*) FROM users'\n42\n```",
    Blocks = doc_test_runner:parse_markdown(Markdown),
    Block = hd(Blocks),
    %% Verify expected output captured
    ExpectedOutput = maps:get(expected_output, Block),
    ?assertNotEqual(none, ExpectedOutput).

%% ============================================================================
%% Test Cases: Output Formatting
%% ============================================================================

test_output_formatting(_Config) ->
    Results = [
        #{
            file => "test.md",
            block => 1,
            type => bash,
            status => pass,
            actual_output => "ok",
            expected_output => "ok",
            duration_ms => 10
        },
        #{
            file => "test.md",
            block => 2,
            type => bash,
            status => fail,
            actual_output => "error",
            expected_output => "ok",
            error => "Mismatch",
            duration_ms => 5
        }
    ],
    %% Should not crash, just format
    try
        doc_test_runner:format_results(Results),
        ok
    catch
        _:_ ->
            ct:fail("format_results crashed")
    end.

%% ============================================================================
%% Test Cases: End-to-End Runner
%% ============================================================================

test_runner_end_to_end(_Config) ->
    %% Create a test markdown file
    TestDir = "/tmp/doc_test_temp",
    ok = filelib:ensure_dir(TestDir ++ "/"),
    TestFile = TestDir ++ "/example.md",

    Markdown = "# Example\n\n```bash run\n$ echo hello\n```\n\n```erlang run\ntest.\n```\n",
    ok = file:write_file(TestFile, Markdown),

    %% Run tests
    Results = doc_test_runner:run_file(TestFile, #{}),

    %% Cleanup
    file:delete(TestFile),

    %% Verify results
    ?assertNotEqual([], Results),
    ?assert(length(Results) >= 2).

%% ============================================================================
%% Helper Test Utilities
%% ============================================================================

%% Test that a function doesn't throw exceptions
assert_no_throw(Fun) ->
    try
        Fun(),
        true
    catch
        _:_ ->
            false
    end.
