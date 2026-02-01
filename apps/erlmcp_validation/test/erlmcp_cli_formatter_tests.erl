%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_cli_formatter
%%% Chicago School TDD - Real processes, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_formatter_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

erlmcp_cli_formatter_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Color functions work correctly", fun test_colors/0},
      {"Format status correctly", fun test_format_status/0},
      {"Format table correctly", fun test_format_table/0},
      {"Format tree correctly", fun test_format_tree/0},
      {"Format JSON correctly", fun test_format_json/0},
      {"Format list correctly", fun test_format_list/0},
      {"Format box correctly", fun test_format_box/0},
      {"Strip colors correctly", fun test_strip_colors/0},
      {"Enable and disable colors", fun test_color_toggle/0},
      {"Format result map", fun test_format_result/0}
     ]}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    % Enable colors for testing
    erlmcp_cli_formatter:enable_colors(),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_colors() ->
    % Test basic colors
    Red = erlmcp_cli_formatter:error("test"),
    ?assert(is_list(Red)),
    ?assert(length(Red) > 4),  % Should include ANSI codes

    Green = erlmcp_cli_formatter:success("test"),
    ?assert(is_list(Green)),

    Yellow = erlmcp_cli_formatter:warning("test"),
    ?assert(is_list(Yellow)),

    Blue = erlmcp_cli_formatter:info("test"),
    ?assert(is_list(Blue)),

    % Test bold and underline
    Bold = erlmcp_cli_formatter:bold("test"),
    ?assert(is_list(Bold)),

    Underline = erlmcp_cli_formatter:underline("test"),
    ?assert(is_list(Underline)),

    ok.

test_format_status() ->
    Passed = erlmcp_cli_formatter:format_status(passed),
    ?assert(is_list(Passed)),
    PassedStr = lists:flatten(Passed),
    ?assert(string:find(PassedStr, "PASSED") =/= nomatch),

    Failed = erlmcp_cli_formatter:format_status(failed),
    FailedStr = lists:flatten(Failed),
    ?assert(string:find(FailedStr, "FAILED") =/= nomatch),

    Warning = erlmcp_cli_formatter:format_status(warning),
    WarningStr = lists:flatten(Warning),
    ?assert(string:find(WarningStr, "WARNING") =/= nomatch),

    ok.

test_format_table() ->
    Rows = [
        #{name => "alice", age => 30, city => "NYC"},
        #{name => "bob", age => 25, city => "LA"},
        #{name => "charlie", age => 35, city => "SF"}
    ],
    Columns = [name, age, city],

    Table = erlmcp_cli_formatter:format_table(Rows, Columns),
    TableStr = lists:flatten(Table),

    % Check that all data is present
    ?assert(string:find(TableStr, "alice") =/= nomatch),
    ?assert(string:find(TableStr, "bob") =/= nomatch),
    ?assert(string:find(TableStr, "charlie") =/= nomatch),
    ?assert(string:find(TableStr, "30") =/= nomatch),
    ?assert(string:find(TableStr, "25") =/= nomatch),
    ?assert(string:find(TableStr, "35") =/= nomatch),

    % Test empty table
    Empty = erlmcp_cli_formatter:format_table([], Columns),
    ?assert(is_list(Empty)),

    ok.

test_format_tree() ->
    Tree = #{
        name => "root",
        children => [
            #{name => "child1"},
            #{name => "child2"}
        ]
    },

    TreeStr = lists:flatten(erlmcp_cli_formatter:format_tree(Tree)),

    ?assert(string:find(TreeStr, "name") =/= nomatch),
    ?assert(string:find(TreeStr, "root") =/= nomatch),
    ?assert(string:find(TreeStr, "children") =/= nomatch),

    ok.

test_format_json() ->
    Data = #{
        key => <<"value">>,
        number => 42,
        nested => #{inner => true}
    },

    Json = erlmcp_cli_formatter:format_json(Data),
    JsonStr = lists:flatten(Json),

    % Check JSON structure
    ?assert(string:find(JsonStr, "key") =/= nomatch),
    ?assert(string:find(JsonStr, "value") =/= nomatch),
    ?assert(string:find(JsonStr, "42") =/= nomatch),

    ok.

test_format_list() ->
    Items = ["item1", "item2", "item3"],

    List = erlmcp_cli_formatter:format_list(Items),
    ListStr = lists:flatten(List),

    ?assert(string:find(ListStr, "item1") =/= nomatch),
    ?assert(string:find(ListStr, "item2") =/= nomatch),
    ?assert(string:find(ListStr, "item3") =/= nomatch),

    % Test custom bullet
    CustomList = erlmcp_cli_formatter:format_list(Items, "-"),
    CustomStr = lists:flatten(CustomList),
    ?assert(string:find(CustomStr, "-") =/= nomatch),

    ok.

test_format_box() ->
    Content = "Hello, World!",

    Box = erlmcp_cli_formatter:format_box(Content),
    BoxStr = lists:flatten(Box),

    % Check box drawing characters
    ?assert(string:find(BoxStr, "╔") =/= nomatch),
    ?assert(string:find(BoxStr, "╚") =/= nomatch),
    ?assert(string:find(BoxStr, "║") =/= nomatch),
    ?assert(string:find(BoxStr, "Hello") =/= nomatch),

    % Test with title
    BoxWithTitle = erlmcp_cli_formatter:format_box(Content, #{title => "Test"}),
    TitleStr = lists:flatten(BoxWithTitle),
    ?assert(string:find(TitleStr, "Test") =/= nomatch),

    ok.

test_strip_colors() ->
    ColoredText = erlmcp_cli_formatter:error("Error message"),
    Stripped = erlmcp_cli_formatter:strip_colors(ColoredText),

    % Stripped text should not contain ANSI escape codes
    ?assertEqual("Error message", Stripped),

    ok.

test_color_toggle() ->
    % Enable colors
    ok = erlmcp_cli_formatter:enable_colors(),
    ?assert(erlmcp_cli_formatter:is_color_enabled()),

    % Disable colors
    ok = erlmcp_cli_formatter:disable_colors(),
    ?assertNot(erlmcp_cli_formatter:is_color_enabled()),

    % Re-enable for other tests
    erlmcp_cli_formatter:enable_colors(),

    ok.

test_format_result() ->
    Result = #{
        status => passed,
        score => 100,
        message => "All tests passed"
    },

    Formatted = erlmcp_cli_formatter:format_result(Result),
    FormattedStr = lists:flatten(Formatted),

    ?assert(string:find(FormattedStr, "PASSED") =/= nomatch),
    ?assert(string:find(FormattedStr, "100") =/= nomatch),

    ok.
