-module(erlmcp_tool_router_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_tool_router
%% Chicago School TDD - Test API boundaries, no state inspection
%% OTP 28 PCRE2 Pattern Testing
%%====================================================================

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    erlmcp_tool_router:init().

cleanup(_) ->
    erlmcp_tool_router:stop().

%%====================================================================
%% Calculator Pattern Tests
%%====================================================================

calculator_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(calculate_simple_expression()),
          ?_test(calculate_complex_expression()),
          ?_test(calculate_with_whitespace()),
          ?_test(calculate_negative_numbers())]
     end}.

calculate_simple_expression() ->
    Text = <<"calculate(2 + 2)">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, <<"2 + 2">>, _}, Result).

calculate_complex_expression() ->
    Text = <<"calculate(10 * 5 + 3 / 2)">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, <<"10 * 5 + 3 / 2">>, _}, Result).

calculate_with_whitespace() ->
    Text = <<"calculate(  42 + 8  )">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, <<"42 + 8">>, _}, Result).

calculate_negative_numbers() ->
    Text = <<"calculate(-5 + 10)">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, <<"-5 + 10">>, _}, Result).

%%====================================================================
%% Search Pattern Tests
%%====================================================================

search_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(search_simple_query()),
          ?_test(search_with_punctuation()),
          ?_test(search_multi_word()),
          ?_test(search_unicode())]
     end}.

search_simple_query() ->
    Text = <<"search for Erlang patterns">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, <<"Erlang patterns">>, _}, Result).

search_with_punctuation() ->
    Text = <<"search for OTP supervisor patterns!">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, <<"OTP supervisor patterns">>, _}, Result).

search_multi_word() ->
    Text = <<"search for gen_server callback implementations in Erlang">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, _, _}, Result).

search_unicode() ->
    Text = <<"search for 天気 patterns in 日本"/utf8>>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, _, _}, Result).

%%====================================================================
%% Code Execution Pattern Tests
%%====================================================================

code_exec_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(execute_erlang_code()),
          ?_test(execute_python_code()),
          ?_test(execute_with_multiline())]
     end}.

execute_erlang_code() ->
    Text = <<"execute erlang: io:format(\"Hello\")">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_execute, _, _}, Result).

execute_python_code() ->
    Text = <<"execute python: print('world')">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_execute, _, _}, Result).

execute_with_multiline() ->
    Text = <<"execute erlang: io:format(\"Line1\"),\nio:format(\"Line2\")">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_execute, _, _}, Result).

%%====================================================================
%% No Match Tests
%%====================================================================

no_match_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(random_text()),
          ?_test(invalid_pattern_syntax()),
          ?_test(empty_string())]
     end}.

random_text() ->
    Text = <<"This is just regular text without tool calls">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertEqual(not_found, Result).

invalid_pattern_syntax() ->
    Text = <<"calc(2 + 2">>,  % Missing closing paren
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertEqual(not_found, Result).

empty_string() ->
    Text = <<>>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertEqual(not_found, Result).

%%====================================================================
%% Custom Pattern Tests
%%====================================================================

custom_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(add_valid_pattern()),
          ?_test(add_invalid_pattern()),
          ?_test(remove_existing_pattern()),
          ?_test(remove_nonexistent_pattern()),
          ?_test(detect_custom_pattern())]
     end}.

add_valid_pattern() ->
    Pattern = <<"fetch\\s+(?<url>https?://[^\\s]+)">>,
    Result = erlmcp_tool_router:add_pattern(<<"fetch">>, Pattern, erlmcp_tool_fetch),
    ?assertEqual(ok, Result).

add_invalid_pattern() ->
    Pattern = <<"invalid(?<unclosed">>,  % Invalid PCRE2 syntax
    Result = erlmcp_tool_router:add_pattern(<<"invalid">>, Pattern, erlmcp_tool_invalid),
    ?assertMatch({error, {invalid_pattern, _}}, Result).

remove_existing_pattern() ->
    Pattern = <<"temp\\s+(?<value>\\d+)">>,
    ok = erlmcp_tool_router:add_pattern(<<"temp">>, Pattern, erlmcp_tool_temp),
    Result = erlmcp_tool_router:remove_pattern(<<"temp">>),
    ?assertEqual(ok, Result).

remove_nonexistent_pattern() ->
    Result = erlmcp_tool_router:remove_pattern(<<"nonexistent">>),
    ?assertEqual({error, pattern_not_found}, Result).

detect_custom_pattern() ->
    Pattern = <<"weather\\s+in\\s+(?<city>[A-Za-z\\s]+)">>,
    ok = erlmcp_tool_router:add_pattern(<<"weather">>, Pattern, erlmcp_tool_weather),
    Text = <<"weather in San Francisco">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_weather, _, _}, Result),
    erlmcp_tool_router:remove_pattern(<<"weather">>).

%%====================================================================
%% Pattern Listing Tests
%%====================================================================

list_patterns_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [?_test(list_builtin_patterns()),
          ?_test(list_with_custom_patterns())]
     end}.

list_builtin_patterns() ->
    Patterns = erlmcp_tool_router:list_patterns(),
    ?assert(length(Patterns) >= 3).

list_with_custom_patterns() ->
    Pattern = <<"custom\\s+(?<input>\\w+)">>,
    ok = erlmcp_tool_router:add_pattern(<<"custom">>, Pattern, erlmcp_tool_custom),
    Patterns = erlmcp_tool_router:list_patterns(),
    ?assert(length(Patterns) >= 4),
    erlmcp_tool_router:remove_pattern(<<"custom">>).

%%====================================================================
%% Pattern Validation Tests
%%====================================================================

validate_pattern_test_() ->
    [?_test(valid_simple_pattern()),
     ?_test(valid_named_captures()),
     ?_test(valid_lookahead()),
     ?_test(invalid_regex_syntax())].

valid_simple_pattern() ->
    Pattern = <<"test\\s+(?<value>\\d+)">>,
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

valid_named_captures() ->
    Pattern = <<"(?<tool>\\w+)\\((?<args>[^)]*)\\)">>,
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

valid_lookahead() ->
    Pattern = <<"\\w+\\s+(?=\\d+)">>,  % PCRE2 lookahead
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

invalid_regex_syntax() ->
    Pattern = <<"invalid(?<paren">>,
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertMatch({error, {invalid_pattern, _}}, Result).

%%====================================================================
%% Argument Extraction Tests
%%====================================================================

extract_args_test_() ->
    [?_test(extract_named_captures()),
     ?_test(extract_from_calculator()),
     ?_test(extract_no_match()),
     ?_test(extract_unicode_captures())].

extract_named_captures() ->
    Pattern = <<"calculate\\((?<expr>[^)]+)\\)">>,
    Text = <<"calculate(2 + 2)">>,
    Result = erlmcp_tool_router:extract_args(Text, Pattern),
    ?assertMatch({ok, #{<<"expr">> := <<"2 + 2">>}}, Result).

extract_from_calculator() ->
    Pattern = list_to_binary("calculate\\((?<expr>[^)]+)\\)"),
    Text = <<"calculate(10 * 5)">>,
    Result = erlmcp_tool_router:extract_args(Text, Pattern),
    ?assertMatch({ok, #{<<"expr">> := <<"10 * 5">>}}, Result).

extract_no_match() ->
    Pattern = <<"calculate\\((?<expr>[^)]+)\\)">>,
    Text = <<"no match here">>,
    Result = erlmcp_tool_router:extract_args(Text, Pattern),
    ?assertEqual({error, no_match}, Result).

extract_unicode_captures() ->
    Pattern = <<"search\\s+for\\s+(?<query>[^.!?]+)">>,
    Text = <<"search for 天気"/utf8>>,
    Result = erlmcp_tool_router:extract_args(Text, Pattern),
    ?assertMatch({ok, #{<<"query">> := <<"天気"/utf8>>}}, Result).

%%====================================================================
%% PCRE2 Advanced Features Tests
%%====================================================================

pcre2_advanced_test_() ->
    [?_test(possessive_quantifier()),
     ?_test(lookbehind()),
     ?_test(atomic_group()),
     ?_test(conditional_pattern())].

possessive_quantifier() ->
    Pattern = <<"\\w*+(?<value>\\d+)">>,  % PCRE2 possessive quantifier
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

lookbehind() ->
    Pattern = <<"(?<number>\\d+)\\s*(?<=\\d)$">>,  % Positive lookbehind
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

atomic_group() ->
    Pattern = <<"(?>(?<text>\\w+\\s*)+)+">>,  % Atomic group
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

conditional_pattern() ->
    Pattern = <<"(?(?=\\d)(?<digits>\\d+)|(?<words>\\w+))">>,  % Conditional
    Result = erlmcp_tool_router:validate_pattern(Pattern),
    ?assertEqual(ok, Result).

%%====================================================================
%% Routing Tests
%%====================================================================

route_to_tool_test_() ->
    [?_test(route_calculator()),
     ?_test(route_search()),
     ?_test(route_execute()),
     ?_test(route_unknown())].

route_calculator() ->
    Result = erlmcp_tool_router:route_to_tool(<<"calculator">>, <<"2 + 2">>),
    ?assertEqual({ok, erlmcp_tool_calculator, <<"2 + 2">>}, Result).

route_search() ->
    Result = erlmcp_tool_router:route_to_tool(<<"search">>, <<"Erlang patterns">>),
    ?assertEqual({ok, erlmcp_tool_search, <<"Erlang patterns">>}, Result).

route_execute() ->
    Result = erlmcp_tool_router:route_to_tool(<<"execute">>, <<"code">>),
    ?assertEqual({ok, erlmcp_tool_execute, <<"code">>}, Result).

route_unknown() ->
    Result = erlmcp_tool_router:route_to_tool(<<"unknown">>, <<"args">>),
    ?assertMatch({error, {unknown_tool, <<"unknown">>}}, Result).

%%====================================================================
%% Edge Cases
%%====================================================================

edge_cases_test_() ->
    [?_test(nested_parentheses()),
     ?_test(escaped_characters()),
     ?_test(very_long_expression()),
     ?_test(special_characters())].

nested_parentheses() ->
    Text = <<"calculate((2 + 3) * 4)">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, <<"(2 + 3) * 4">>, _}, Result).

escaped_characters() ->
    Text = <<"search for \\d+ patterns">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, <<"\\\\d+ patterns">>, _}, Result).

very_long_expression() ->
    LongExpr = binary:copy(<<"1 + ">>, 100),
    LongExpr2 = <<LongExpr/binary, "0">>,
    Text = <<"calculate(", LongExpr2/binary, ")">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_calculator, _, _}, Result).

special_characters() ->
    Text = <<"search for @#$%^&*() patterns">>,
    Result = erlmcp_tool_router:detect_tool_call(Text),
    ?assertMatch({ok, erlmcp_tool_search, _, _}, Result).
