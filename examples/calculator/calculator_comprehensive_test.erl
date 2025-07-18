-module(calculator_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").

-export([format_number/1]).

%%====================================================================
%% Main Test Suite
%%====================================================================

calculator_comprehensive_test_() ->
    [
        {"Number formatting comprehensive tests", format_number_comprehensive_tests()},
        {"Arithmetic operations comprehensive tests", arithmetic_comprehensive_tests()},
        {"Advanced mathematical operations", advanced_math_tests()},
        {"Error handling and edge cases", error_handling_comprehensive_tests()},
        {"Expression evaluation tests", expression_evaluation_tests()},
        {"Tool schema validation tests", tool_schema_tests()},
        {"Resource generation tests", resource_generation_tests()},
        {"Prompt generation tests", prompt_generation_tests()}
    ].

%%====================================================================
%% Number Formatting Tests
%%====================================================================

format_number_comprehensive_tests() ->
    [
        {"Integer edge cases", fun test_integer_edge_cases/0},
        {"Float precision", fun test_float_precision/0},
        {"Decimal normalization", fun test_decimal_normalization/0},
        {"Scientific notation", fun test_scientific_notation/0},
        {"Very large numbers", fun test_very_large_numbers/0},
        {"Very small numbers", fun test_very_small_numbers/0}
    ].

test_integer_edge_cases() ->
    ?assertEqual(<<"0">>, format_number(0)),
    ?assertEqual(<<"1">>, format_number(1)),
    ?assertEqual(<<"-1">>, format_number(-1)),
    ?assertEqual(<<"2147483647">>, format_number(2147483647)),
    ?assertEqual(<<"-2147483648">>, format_number(-2147483648)).

test_float_precision() ->
    ?assertEqual(<<"3.14159">>, format_number(3.14159)),
    ?assertEqual(<<"2.71828">>, format_number(2.71828)),
    ?assertEqual(<<"1.4142135624">>, format_number(1.41421356237)),
    ?assertEqual(<<"0.5772156649">>, format_number(0.5772156649)).

test_decimal_normalization() ->
    ?assertEqual(<<"5">>, format_number(5.0)),
    ?assertEqual(<<"5">>, format_number(5.00)),
    ?assertEqual(<<"5">>, format_number(5.000000)),
    ?assertEqual(<<"0">>, format_number(0.0)),
    ?assertEqual(<<"0">>, format_number(-0.0)).

test_scientific_notation() ->
    ?assertEqual(<<"1000">>, format_number(1.0e3)),
    ?assertEqual(<<"0.001">>, format_number(1.0e-3)),
    ?assertEqual(<<"1000000">>, format_number(1.0e6)).

test_very_large_numbers() ->
    ?assertEqual(<<"1000000000000">>, format_number(1.0e12)),
    ?assertEqual(<<"999999999999">>, format_number(999999999999.0)).

test_very_small_numbers() ->
    ?assertEqual(<<"0.000001">>, format_number(0.000001)),
    ?assertEqual(<<"0.0000001">>, format_number(0.0000001)).

%%====================================================================
%% Arithmetic Operations Tests
%%====================================================================

arithmetic_comprehensive_tests() ->
    [
        {"Addition comprehensive", fun test_addition_comprehensive/0},
        {"Subtraction comprehensive", fun test_subtraction_comprehensive/0},
        {"Multiplication comprehensive", fun test_multiplication_comprehensive/0},
        {"Division comprehensive", fun test_division_comprehensive/0},
        {"Mixed operations", fun test_mixed_operations/0}
    ].

test_addition_comprehensive() ->
    %% Positive numbers
    ?assertEqual(<<"100">>, add_tool(#{<<"a">> => 42, <<"b">> => 58})),
    ?assertEqual(<<"3.14159">>, add_tool(#{<<"a">> => 3.14, <<"b">> => 0.00159})),
    
    %% Negative numbers
    ?assertEqual(<<"-10">>, add_tool(#{<<"a">> => -15, <<"b">> => 5})),
    ?assertEqual(<<"-20">>, add_tool(#{<<"a">> => -12, <<"b">> => -8})),
    
    %% Zero cases
    ?assertEqual(<<"42">>, add_tool(#{<<"a">> => 42, <<"b">> => 0})),
    ?assertEqual(<<"0">>, add_tool(#{<<"a">> => 0, <<"b">> => 0})).

test_subtraction_comprehensive() ->
    %% Positive results
    ?assertEqual(<<"15">>, subtract_tool(#{<<"a">> => 23, <<"b">> => 8})),
    ?assertEqual(<<"1.5">>, subtract_tool(#{<<"a">> => 3.5, <<"b">> => 2.0})),
    
    %% Negative results
    ?assertEqual(<<"-5">>, subtract_tool(#{<<"a">> => 10, <<"b">> => 15})),
    ?assertEqual(<<"-2.5">>, subtract_tool(#{<<"a">> => 1.5, <<"b">> => 4.0})),
    
    %% Zero cases
    ?assertEqual(<<"42">>, subtract_tool(#{<<"a">> => 42, <<"b">> => 0})),
    ?assertEqual(<<"0">>, subtract_tool(#{<<"a">> => 15, <<"b">> => 15})).

test_multiplication_comprehensive() ->
    %% Positive numbers
    ?assertEqual(<<"24">>, multiply_tool(#{<<"a">> => 6, <<"b">> => 4})),
    ?assertEqual(<<"6.25">>, multiply_tool(#{<<"a">> => 2.5, <<"b">> => 2.5})),
    
    %% Negative numbers
    ?assertEqual(<<"-24">>, multiply_tool(#{<<"a">> => -6, <<"b">> => 4})),
    ?assertEqual(<<"24">>, multiply_tool(#{<<"a">> => -6, <<"b">> => -4})),
    
    %% Zero cases
    ?assertEqual(<<"0">>, multiply_tool(#{<<"a">> => 0, <<"b">> => 42})),
    ?assertEqual(<<"0">>, multiply_tool(#{<<"a">> => 42, <<"b">> => 0})),
    
    %% One cases
    ?assertEqual(<<"42">>, multiply_tool(#{<<"a">> => 42, <<"b">> => 1})),
    ?assertEqual(<<"42">>, multiply_tool(#{<<"a">> => 1, <<"b">> => 42})).

test_division_comprehensive() ->
    %% Exact division
    ?assertEqual(<<"6">>, divide_tool(#{<<"a">> => 24, <<"b">> => 4})),
    ?assertEqual(<<"2.5">>, divide_tool(#{<<"a">> => 5, <<"b">> => 2})),
    
    %% Division with decimals
    ?assertEqual(<<"3.3333333333">>, divide_tool(#{<<"a">> => 10, <<"b">> => 3})),
    ?assertEqual(<<"0.3333333333">>, divide_tool(#{<<"a">> => 1, <<"b">> => 3})),
    
    %% Negative division
    ?assertEqual(<<"-6">>, divide_tool(#{<<"a">> => -24, <<"b">> => 4})),
    ?assertEqual(<<"6">>, divide_tool(#{<<"a">> => -24, <<"b">> => -4})),
    
    %% Division by one
    ?assertEqual(<<"42">>, divide_tool(#{<<"a">> => 42, <<"b">> => 1})),
    
    %% Division resulting in less than 1
    ?assertEqual(<<"0.5">>, divide_tool(#{<<"a">> => 1, <<"b">> => 2})),
    ?assertEqual(<<"0.25">>, divide_tool(#{<<"a">> => 1, <<"b">> => 4})).

test_mixed_operations() ->
    %% Test combinations that might reveal issues
    A = add_tool(#{<<"a">> => 10, <<"b">> => 5}),
    B = subtract_tool(#{<<"a">> => 10, <<"b">> => 5}),
    ?assertEqual(<<"15">>, A),
    ?assertEqual(<<"5">>, B).

%%====================================================================
%% Advanced Mathematical Operations
%%====================================================================

advanced_math_tests() ->
    [
        {"Power function comprehensive", fun test_power_comprehensive/0},
        {"Square root comprehensive", fun test_sqrt_comprehensive/0},
        {"Factorial comprehensive", fun test_factorial_comprehensive/0},
        {"Mathematical constants", fun test_mathematical_constants/0}
    ].

test_power_comprehensive() ->
    %% Basic powers
    ?assertEqual(<<"1">>, power_tool(#{<<"a">> => 2, <<"b">> => 0})),
    ?assertEqual(<<"2">>, power_tool(#{<<"a">> => 2, <<"b">> => 1})),
    ?assertEqual(<<"4">>, power_tool(#{<<"a">> => 2, <<"b">> => 2})),
    ?assertEqual(<<"8">>, power_tool(#{<<"a">> => 2, <<"b">> => 3})),
    
    %% Larger powers
    ?assertEqual(<<"1024">>, power_tool(#{<<"a">> => 2, <<"b">> => 10})),
    ?assertEqual(<<"59049">>, power_tool(#{<<"a">> => 3, <<"b">> => 10})),
    
    %% Fractional powers (exact cases)
    ?assertEqual(<<"4">>, power_tool(#{<<"a">> => 16, <<"b">> => 0.5})),
    ?assertEqual(<<"1.9999861371">>, power_tool(#{<<"a">> => 8, <<"b">> => 0.33333})),
    
    %% Edge cases
    ?assertEqual(<<"1">>, power_tool(#{<<"a">> => 1, <<"b">> => 100})),
    ?assertEqual(<<"0">>, power_tool(#{<<"a">> => 0, <<"b">> => 5})).

test_sqrt_comprehensive() ->
    %% Perfect squares
    ?assertEqual(<<"0">>, sqrt_tool(#{<<"a">> => 0})),
    ?assertEqual(<<"1">>, sqrt_tool(#{<<"a">> => 1})),
    ?assertEqual(<<"2">>, sqrt_tool(#{<<"a">> => 4})),
    ?assertEqual(<<"3">>, sqrt_tool(#{<<"a">> => 9})),
    ?assertEqual(<<"10">>, sqrt_tool(#{<<"a">> => 100})),
    
    %% Non-perfect squares
    ?assertEqual(<<"1.4142135624">>, sqrt_tool(#{<<"a">> => 2})),
    ?assertEqual(<<"2.2360679775">>, sqrt_tool(#{<<"a">> => 5})),
    
    %% Decimal inputs
    ?assertEqual(<<"1.5811388301">>, sqrt_tool(#{<<"a">> => 2.5})),
    ?assertEqual(<<"0.7071067812">>, sqrt_tool(#{<<"a">> => 0.5})).

test_factorial_comprehensive() ->
    %% Basic factorials
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 0})),
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 1})),
    ?assertEqual(<<"2">>, factorial_tool(#{<<"n">> => 2})),
    ?assertEqual(<<"6">>, factorial_tool(#{<<"n">> => 3})),
    ?assertEqual(<<"24">>, factorial_tool(#{<<"n">> => 4})),
    ?assertEqual(<<"120">>, factorial_tool(#{<<"n">> => 5})),
    
    %% Larger factorials
    ?assertEqual(<<"720">>, factorial_tool(#{<<"n">> => 6})),
    ?assertEqual(<<"5040">>, factorial_tool(#{<<"n">> => 7})),
    ?assertEqual(<<"40320">>, factorial_tool(#{<<"n">> => 8})),
    ?assertEqual(<<"362880">>, factorial_tool(#{<<"n">> => 9})),
    ?assertEqual(<<"3628800">>, factorial_tool(#{<<"n">> => 10})),
    
    %% Even larger (but still safe)
    ?assertEqual(<<"87178291200">>, factorial_tool(#{<<"n">> => 14})).

test_mathematical_constants() ->
    %% Test results that should approximate mathematical constants
    %% These test that our power function can approximate e^1
    PowerE = power_tool(#{<<"a">> => 2.71828, <<"b">> => 1}),
    ?assertEqual(<<"2.71828">>, PowerE),
    
    %% Test that we can calculate square root of pi^2
    PiSquared = power_tool(#{<<"a">> => 3.14159, <<"b">> => 2}),
    ?assertEqual(<<"9.8695877281">>, PiSquared).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_comprehensive_tests() ->
    [
        {"Division errors", fun test_division_errors/0},
        {"Square root errors", fun test_sqrt_errors/0},
        {"Factorial errors", fun test_factorial_errors/0},
        {"Power function errors", fun test_power_errors/0},
        {"Expression evaluation errors", fun test_expression_errors/0}
    ].

test_division_errors() ->
    ?assertEqual(<<"Error: Division by zero">>, divide_tool(#{<<"a">> => 1, <<"b">> => 0})),
    ?assertEqual(<<"Error: Division by zero">>, divide_tool(#{<<"a">> => 0, <<"b">> => 0})),
    ?assertEqual(<<"Error: Division by zero">>, divide_tool(#{<<"a">> => -5, <<"b">> => 0})),
    ?assertEqual(<<"Error: Division by zero">>, divide_tool(#{<<"a">> => 3.14, <<"b">> => 0})).

test_sqrt_errors() ->
    ?assertEqual(<<"Error: Cannot calculate square root of negative number">>, 
                 sqrt_tool(#{<<"a">> => -1})),
    ?assertEqual(<<"Error: Cannot calculate square root of negative number">>, 
                 sqrt_tool(#{<<"a">> => -0.1})),
    ?assertEqual(<<"Error: Cannot calculate square root of negative number">>, 
                 sqrt_tool(#{<<"a">> => -100})).

test_factorial_errors() ->
    ?assertEqual(<<"Error: Cannot calculate factorial of negative number">>, 
                 factorial_tool(#{<<"n">> => -1})),
    ?assertEqual(<<"Error: Cannot calculate factorial of negative number">>, 
                 factorial_tool(#{<<"n">> => -10})),
    ?assertEqual(<<"Error: Factorial requires integer input">>, 
                 factorial_tool(#{<<"n">> => 3.5})),
    ?assertEqual(<<"Error: Factorial requires integer input">>, 
                 factorial_tool(#{<<"n">> => 2.1})),
    ?assertEqual(<<"Error: Number too large for factorial calculation">>, 
                 factorial_tool(#{<<"n">> => 171})),
    ?assertEqual(<<"Error: Number too large for factorial calculation">>, 
                 factorial_tool(#{<<"n">> => 200})).

test_power_errors() ->
    %% Test edge cases that might cause errors
    Result1 = power_tool(#{<<"a">> => 0, <<"b">> => -1}),
    ?assert(binary:match(Result1, <<"Error">>) =/= nomatch).

test_expression_errors() ->
    ?assertMatch(<<"Error: ", _/binary>>, calculate_tool(#{<<"expression">> => <<"">>})),
    ?assertMatch(<<"Error: ", _/binary>>, calculate_tool(#{<<"expression">> => <<"abc">>})),
    ?assertMatch(<<"Error: ", _/binary>>, calculate_tool(#{<<"expression">> => <<"1 +">>})),
    ?assertMatch(<<"Error: ", _/binary>>, calculate_tool(#{<<"expression">> => <<"/ 5">>})).

%%====================================================================
%% Expression Evaluation Tests
%%====================================================================

expression_evaluation_tests() ->
    [
        {"Simple expressions", fun test_simple_expressions/0},
        {"Complex expressions", fun test_complex_expressions/0},
        {"Expression edge cases", fun test_expression_edge_cases/0}
    ].

test_simple_expressions() ->
    ?assertEqual(<<"5">>, calculate_tool(#{<<"expression">> => <<"5">>})),
    ?assertEqual(<<"8">>, calculate_tool(#{<<"expression">> => <<"3 + 5">>})),
    ?assertEqual(<<"15">>, calculate_tool(#{<<"expression">> => <<"3 * 5">>})),
    ?assertEqual(<<"2">>, calculate_tool(#{<<"expression">> => <<"10 / 5">>})),
    ?assertEqual(<<"5">>, calculate_tool(#{<<"expression">> => <<"8 - 3">>})).

test_complex_expressions() ->
    ?assertEqual(<<"14">>, calculate_tool(#{<<"expression">> => <<"2 + 3 * 4">>})),
    ?assertEqual(<<"8">>, calculate_tool(#{<<"expression">> => <<"10 / 2 + 3">>})),
    ?assertEqual(<<"20">>, calculate_tool(#{<<"expression">> => <<"5 * 2 + 10">>})),
    ?assertEqual(<<"1">>, calculate_tool(#{<<"expression">> => <<"6 / 2 - 2">>})),
    ?assertEqual(<<"57">>, calculate_tool(#{<<"expression">> => <<"(15 + 8) * 3 - 12">>})),
    ?assertEqual(<<"20">>, calculate_tool(#{<<"expression">> => <<"(2 + 3) * 4">>})),
    ?assertEqual(<<"14">>, calculate_tool(#{<<"expression">> => <<"2 + (3 * 4)">>})).

test_expression_edge_cases() ->
    ?assertEqual(<<"0">>, calculate_tool(#{<<"expression">> => <<"0">>})),
    ?assertEqual(<<"0">>, calculate_tool(#{<<"expression">> => <<"5 - 5">>})),
    ?assertEqual(<<"10">>, calculate_tool(#{<<"expression">> => <<"5 + 5">>})).

%%====================================================================
%% Tool Schema Tests
%%====================================================================

tool_schema_tests() ->
    [
        {"Schema validation", fun test_schema_validation/0},
        {"Parameter types", fun test_parameter_types/0}
    ].

test_schema_validation() ->
    %% Test that our tool implementations handle the expected parameter formats
    ?assertEqual(<<"10">>, add_tool(#{<<"a">> => 4, <<"b">> => 6})),
    ?assertEqual(<<"10">>, add_tool(#{<<"a">> => 4.0, <<"b">> => 6.0})),
    ?assertEqual(<<"10">>, add_tool(#{<<"a">> => 4, <<"b">> => 6.0})).

test_parameter_types() ->
    %% Test mixed integer/float parameters
    ?assertEqual(<<"7.5">>, add_tool(#{<<"a">> => 3, <<"b">> => 4.5})),
    ?assertEqual(<<"7.5">>, add_tool(#{<<"a">> => 3.0, <<"b">> => 4.5})),
    ?assertEqual(<<"7.5">>, multiply_tool(#{<<"a">> => 1.5, <<"b">> => 5})).

%%====================================================================
%% Resource Generation Tests
%%====================================================================

resource_generation_tests() ->
    [
        {"Help resource", fun test_help_resource/0},
        {"History resource", fun test_history_resource/0}
    ].

test_help_resource() ->
    Help = generate_help_resource(<<"calculator://help">>),
    ?assert(is_binary(Help)),
    ?assert(byte_size(Help) > 0),
    ?assert(binary:match(Help, <<"Calculator">>) =/= nomatch).

test_history_resource() ->
    History = generate_history_resource(<<"calculator://history">>),
    ?assert(is_binary(History)),
    ?assert(byte_size(History) > 0).

%%====================================================================
%% Prompt Generation Tests
%%====================================================================

prompt_generation_tests() ->
    [
        {"Math problem prompts", fun test_math_problem_prompts/0},
        {"Prompt parameter handling", fun test_prompt_parameters/0}
    ].

test_math_problem_prompts() ->
    Easy = generate_math_problem(#{<<"difficulty">> => <<"easy">>}),
    Medium = generate_math_problem(#{<<"difficulty">> => <<"medium">>}),
    Hard = generate_math_problem(#{<<"difficulty">> => <<"hard">>}),
    
    ?assertMatch([#{<<"role">> := <<"user">>}], Easy),
    ?assertMatch([#{<<"role">> := <<"user">>}], Medium),
    ?assertMatch([#{<<"role">> := <<"user">>}], Hard).

test_prompt_parameters() ->
    Default = generate_math_problem(#{}),
    ?assertMatch([#{<<"role">> := <<"user">>}], Default).

%%====================================================================
%% Tool Implementation Functions
%%====================================================================

add_tool(#{<<"a">> := A, <<"b">> := B}) ->
    Result = A + B,
    format_number(Result).

subtract_tool(#{<<"a">> := A, <<"b">> := B}) ->
    Result = A - B,
    format_number(Result).

multiply_tool(#{<<"a">> := A, <<"b">> := B}) ->
    Result = A * B,
    format_number(Result).

divide_tool(#{<<"a">> := A, <<"b">> := B}) ->
    case B of
        0 -> <<"Error: Division by zero">>;
        _ -> 
            Result = A / B,
            format_number(Result)
    end.

power_tool(#{<<"a">> := A, <<"b">> := B}) ->
    try
        Result = math:pow(A, B),
        format_number(Result)
    catch
        error:badarith ->
            <<"Error: Invalid power operation">>;
        error:_ ->
            <<"Error: Power calculation failed">>
    end.

sqrt_tool(#{<<"a">> := A}) ->
    if
        A < 0 -> <<"Error: Cannot calculate square root of negative number">>;
        true ->
            Result = math:sqrt(A),
            format_number(Result)
    end.

factorial_tool(#{<<"n">> := N}) ->
    if
        N < 0 -> <<"Error: Cannot calculate factorial of negative number">>;
        not is_integer(N) -> <<"Error: Factorial requires integer input">>;
        N > 170 -> <<"Error: Number too large for factorial calculation">>;
        true ->
            Result = factorial(N),
            format_number(Result)
    end.

calculate_tool(#{<<"expression">> := Expression}) ->
    try
        Result = evaluate_expression(Expression),
        format_number(Result)
    catch
        error:Reason ->
            iolist_to_binary(io_lib:format("Error: ~p", [Reason]));
        throw:Reason ->
            iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
    end.

generate_help_resource(_Uri) ->
    <<"Calculator MCP Server Help\n\n"
      "Available Tools:\n"
      "- add: Add two numbers\n"
      "- subtract: Subtract two numbers\n"
      "- multiply: Multiply two numbers\n"
      "- divide: Divide two numbers\n"
      "- power: Calculate power (a^b)\n"
      "- sqrt: Calculate square root\n"
      "- factorial: Calculate factorial\n"
      "- calculate: Evaluate mathematical expressions\n\n"
      "Usage Examples:\n"
      "- Use the add tool: {\"a\": 5, \"b\": 3}\n"
      "- Use calculate tool: {\"expression\": \"2 + 3 * 4\"}\n">>.

generate_history_resource(_Uri) ->
    <<"Recent calculations:\n- Last session calculations would be stored here\n- This is a demo resource">>.

generate_math_problem(Args) ->
    Difficulty = maps:get(<<"difficulty">>, Args, <<"medium">>),
    Type = maps:get(<<"type">>, Args, <<"mixed">>),
    
    Problem = case {Difficulty, Type} of
        {<<"easy">>, <<"addition">>} ->
            <<"What is 7 + 15?">>;
        {<<"medium">>, <<"multiplication">>} ->
            <<"What is 23 × 17?">>;
        {<<"hard">>, <<"algebra">>} ->
            <<"Solve for x: 3x + 7 = 22">>;
        {_, _} ->
            <<"Calculate: (15 + 8) × 3 - 12">>
    end,
    
    [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Please solve this math problem: ", Problem/binary>>
        }
    }].

%%====================================================================
%% Helper Functions
%%====================================================================

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% Simple expression evaluator
evaluate_expression(Expression) when is_binary(Expression) ->
    evaluate_expression(binary_to_list(Expression));
evaluate_expression(Expression) when is_list(Expression) ->
    Tokens = tokenize(Expression),
    evaluate_tokens(Tokens).

tokenize(Expression) ->
    CleanExpr = lists:filter(fun(C) -> C =/= $  end, Expression),
    tokenize_chars(CleanExpr, []).

tokenize_chars([], Acc) ->
    lists:reverse(Acc);
tokenize_chars([C | Rest], Acc) when C >= $0, C =< $9 ->
    {Number, Remaining} = read_number([C | Rest]),
    tokenize_chars(Remaining, [Number | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $+ ->
    tokenize_chars(Rest, [add | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $- ->
    tokenize_chars(Rest, [subtract | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $* ->
    tokenize_chars(Rest, [multiply | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $/ ->
    tokenize_chars(Rest, [divide | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $( ->
    tokenize_chars(Rest, [lparen | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $) ->
    tokenize_chars(Rest, [rparen | Acc]);
tokenize_chars([C | Rest], Acc) when C =:= $. ->
    {Number, Remaining} = read_number([C | Rest]),
    tokenize_chars(Remaining, [Number | Acc]);
tokenize_chars([_C | Rest], Acc) ->
    tokenize_chars(Rest, Acc).

read_number(String) ->
    read_number(String, []).

read_number([], Acc) ->
    {parse_number(lists:reverse(Acc)), []};
read_number([C | Rest], Acc) when C >= $0, C =< $9 ->
    read_number(Rest, [C | Acc]);
read_number([C | Rest], Acc) when C =:= $., length(Acc) > 0 ->
    read_number(Rest, [C | Acc]);
read_number(Rest, Acc) ->
    {parse_number(lists:reverse(Acc)), Rest}.

parse_number(NumStr) ->
    case string:to_float(NumStr) of
        {Float, []} -> Float;
        {error, no_float} ->
            case string:to_integer(NumStr) of
                {Int, []} -> Int;
                _ -> throw({invalid_number, NumStr})
            end
    end.

evaluate_tokens(Tokens) ->
    case Tokens of
        [] -> throw(empty_expression);
        [Number] when is_number(Number) -> Number;
        _ -> 
            %% First handle parentheses, then operator precedence
            TokensWithoutParens = evaluate_parentheses(Tokens),
            evaluate_with_precedence(TokensWithoutParens)
    end.

%% Handle parentheses by evaluating innermost expressions first
evaluate_parentheses(Tokens) ->
    case find_innermost_parens(Tokens) of
        {Start, End} ->
            %% Extract the expression inside parentheses
            {Before, ParenExpr, After} = extract_paren_expr(Tokens, Start, End),
            %% Evaluate the expression inside parentheses
            Result = evaluate_with_precedence(ParenExpr),
            %% Replace the parentheses expression with the result
            NewTokens = Before ++ [Result] ++ After,
            %% Recursively handle any remaining parentheses
            evaluate_parentheses(NewTokens);
        not_found ->
            Tokens
    end.

%% Find the innermost parentheses (rightmost opening paren)
find_innermost_parens(Tokens) ->
    find_innermost_parens(Tokens, 0, not_found, not_found).

find_innermost_parens([], _Pos, _LastOpen, not_found) ->
    not_found;
find_innermost_parens([], _Pos, LastOpen, _Close) when LastOpen =/= not_found ->
    throw(unmatched_parentheses);
find_innermost_parens([lparen | Rest], Pos, _LastOpen, Close) ->
    find_innermost_parens(Rest, Pos + 1, Pos, Close);
find_innermost_parens([rparen | _Rest], Pos, LastOpen, _Close) when LastOpen =/= not_found ->
    {LastOpen, Pos};
find_innermost_parens([rparen | _Rest], _Pos, not_found, _Close) ->
    throw(unmatched_parentheses);
find_innermost_parens([_Token | Rest], Pos, LastOpen, Close) ->
    find_innermost_parens(Rest, Pos + 1, LastOpen, Close).

%% Extract expression inside parentheses
extract_paren_expr(Tokens, Start, End) ->
    {Before, Rest1} = lists:split(Start, Tokens),
    {_LParen, Rest2} = lists:split(1, Rest1),  % Remove lparen
    {ParenExpr, Rest3} = lists:split(End - Start - 1, Rest2),
    {_RParen, After} = lists:split(1, Rest3),  % Remove rparen
    {Before, ParenExpr, After}.

%% Evaluate with proper operator precedence
evaluate_with_precedence(Tokens) ->
    %% First pass: handle multiplication and division (left to right)
    Tokens1 = handle_high_precedence(Tokens, []),
    %% Second pass: handle addition and subtraction (left to right)
    handle_low_precedence(Tokens1, []).

handle_high_precedence([], Acc) ->
    lists:reverse(Acc);
handle_high_precedence([A, multiply, B | Rest], Acc) when is_number(A), is_number(B) ->
    Result = A * B,
    handle_high_precedence([Result | Rest], Acc);
handle_high_precedence([A, divide, B | Rest], Acc) when is_number(A), is_number(B) ->
    if
        B =:= 0 -> throw(division_by_zero);
        true -> 
            Result = A / B,
            handle_high_precedence([Result | Rest], Acc)
    end;
handle_high_precedence([Token | Rest], Acc) ->
    handle_high_precedence(Rest, [Token | Acc]).

handle_low_precedence([], Acc) ->
    case lists:reverse(Acc) of
        [Result] when is_number(Result) -> Result;
        _ -> throw(invalid_expression)
    end;
handle_low_precedence([A, add, B | Rest], Acc) when is_number(A), is_number(B) ->
    Result = A + B,
    handle_low_precedence([Result | Rest], Acc);
handle_low_precedence([A, subtract, B | Rest], Acc) when is_number(A), is_number(B) ->
    Result = A - B,
    handle_low_precedence([Result | Rest], Acc);
handle_low_precedence([Token | Rest], Acc) ->
    handle_low_precedence(Rest, [Token | Acc]).

%% Format number function with proper decimal point handling
format_number(N) when is_integer(N) ->
    integer_to_binary(N);
format_number(N) when is_float(N) ->
    %% Handle negative zero by checking if the number is zero
    ActualN = if
        N == 0.0 -> 0.0;  % Convert both 0.0 and -0.0 to 0.0
        true -> N
    end,
    
    Formatted = io_lib:format("~.10f", [ActualN]),
    Trimmed = lists:reverse(string:trim(lists:reverse(Formatted), both, "0")),
    Result = case Trimmed of
        "." -> "0";  % Handle 0.0 case
        "." ++ _ -> "0" ++ Trimmed;  % Handle .5 case
        _ -> 
            % Handle 5. case - remove trailing decimal point
            case lists:reverse(Trimmed) of
                "." ++ Rest -> lists:reverse(Rest);
                _ -> Trimmed
            end
    end,
    iolist_to_binary(Result).

%%====================================================================
%% Test Runner Functions
%%====================================================================

run_all_tests() ->
    eunit:test(?MODULE, [verbose, {timeout, 60}]).

run_format_tests() ->
    eunit:test({generator, fun format_number_comprehensive_tests/0}, [verbose]).

run_arithmetic_tests() ->
    eunit:test({generator, fun arithmetic_comprehensive_tests/0}, [verbose]).

run_advanced_tests() ->
    eunit:test({generator, fun advanced_math_tests/0}, [verbose]).

run_error_tests() ->
    eunit:test({generator, fun error_handling_comprehensive_tests/0}, [verbose]).