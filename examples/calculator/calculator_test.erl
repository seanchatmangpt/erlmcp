-module(calculator_test).
-include_lib("eunit/include/eunit.hrl").

-export([format_number/1]).

%%====================================================================
%% Test Fixtures
%%====================================================================

%% No setup/teardown needed since we're testing logic directly
%% without starting the stdio server

%%====================================================================
%% Main Test Suite
%%====================================================================

calculator_test_() ->
    [
        {"Format number tests", format_number_tests()},
        {"Arithmetic operation tests", arithmetic_tests()},
        {"Advanced operation tests", advanced_tests()},
        {"Error handling tests", error_handling_tests()},
        {"Tool handler tests", tool_handler_tests()}
    ].

%%====================================================================
%% Format Number Tests
%%====================================================================

format_number_tests() ->
    [
        {"Integer formatting", fun test_integer_formatting/0},
        {"Float formatting", fun test_float_formatting/0},
        {"Decimal point handling", fun test_decimal_point_handling/0},
        {"Trailing zeros removal", fun test_trailing_zeros_removal/0},
        {"Edge cases", fun test_edge_cases/0}
    ].

test_integer_formatting() ->
    ?assertEqual(<<"42">>, format_number(42)),
    ?assertEqual(<<"-15">>, format_number(-15)),
    ?assertEqual(<<"0">>, format_number(0)),
    ?assertEqual(<<"1000000">>, format_number(1000000)).

test_float_formatting() ->
    ?assertEqual(<<"3.14">>, format_number(3.14)),
    ?assertEqual(<<"2.5">>, format_number(2.5)),
    ?assertEqual(<<"-1.5">>, format_number(-1.5)),
    ?assertEqual(<<"0.1">>, format_number(0.1)).

test_decimal_point_handling() ->
    ?assertEqual(<<"5">>, format_number(5.0)),
    ?assertEqual(<<"10">>, format_number(10.0)),
    ?assertEqual(<<"-7">>, format_number(-7.0)),
    ?assertEqual(<<"0">>, format_number(0.0)).

test_trailing_zeros_removal() ->
    ?assertEqual(<<"1.1">>, format_number(1.1000)),
    ?assertEqual(<<"2.25">>, format_number(2.2500)),
    ?assertEqual(<<"3.333">>, format_number(3.3330)),
    ?assertEqual(<<"4">>, format_number(4.0000)).

test_edge_cases() ->
    ?assertEqual(<<"0.0001">>, format_number(0.0001)),
    ?assertEqual(<<"1000">>, format_number(1000.0)),
    ?assertEqual(<<"-0.5">>, format_number(-0.5)).

%%====================================================================
%% Arithmetic Operation Tests
%%====================================================================

arithmetic_tests() ->
    [
        {"Addition", fun test_addition/0},
        {"Subtraction", fun test_subtraction/0},
        {"Multiplication", fun test_multiplication/0},
        {"Division", fun test_division/0}
    ].

test_addition() ->
    ?assertEqual(<<"42">>, add_tool(#{<<"a">> => 25, <<"b">> => 17})),
    ?assertEqual(<<"0">>, add_tool(#{<<"a">> => -5, <<"b">> => 5})),
    ?assertEqual(<<"7.5">>, add_tool(#{<<"a">> => 3.2, <<"b">> => 4.3})).

test_subtraction() ->
    ?assertEqual(<<"27">>, subtract_tool(#{<<"a">> => 50, <<"b">> => 23})),
    ?assertEqual(<<"-10">>, subtract_tool(#{<<"a">> => 5, <<"b">> => 15})),
    ?assertEqual(<<"1.1">>, subtract_tool(#{<<"a">> => 4.3, <<"b">> => 3.2})).

test_multiplication() ->
    ?assertEqual(<<"96">>, multiply_tool(#{<<"a">> => 12, <<"b">> => 8})),
    ?assertEqual(<<"0">>, multiply_tool(#{<<"a">> => 0, <<"b">> => 42})),
    ?assertEqual(<<"6.25">>, multiply_tool(#{<<"a">> => 2.5, <<"b">> => 2.5})).

test_division() ->
    ?assertEqual(<<"12">>, divide_tool(#{<<"a">> => 144, <<"b">> => 12})),
    ?assertEqual(<<"2.5">>, divide_tool(#{<<"a">> => 5, <<"b">> => 2})),
    ?assertEqual(<<"0.5">>, divide_tool(#{<<"a">> => 1, <<"b">> => 2})).

%%====================================================================
%% Advanced Operation Tests
%%====================================================================

advanced_tests() ->
    [
        {"Power calculation", fun test_power/0},
        {"Square root", fun test_sqrt/0},
        {"Factorial", fun test_factorial/0}
    ].

test_power() ->
    ?assertEqual(<<"1024">>, power_tool(#{<<"a">> => 2, <<"b">> => 10})),
    ?assertEqual(<<"1">>, power_tool(#{<<"a">> => 5, <<"b">> => 0})),
    ?assertEqual(<<"27">>, power_tool(#{<<"a">> => 3, <<"b">> => 3})).

test_sqrt() ->
    ?assertEqual(<<"8">>, sqrt_tool(#{<<"a">> => 64})),
    ?assertEqual(<<"0">>, sqrt_tool(#{<<"a">> => 0})),
    ?assertEqual(<<"1">>, sqrt_tool(#{<<"a">> => 1})).

test_factorial() ->
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 0})),
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 1})),
    ?assertEqual(<<"120">>, factorial_tool(#{<<"n">> => 5})),
    ?assertEqual(<<"3628800">>, factorial_tool(#{<<"n">> => 10})).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    [
        {"Division by zero", fun test_division_by_zero/0},
        {"Negative square root", fun test_negative_sqrt/0},
        {"Negative factorial", fun test_negative_factorial/0},
        {"Non-integer factorial", fun test_non_integer_factorial/0},
        {"Large factorial", fun test_large_factorial/0}
    ].

test_division_by_zero() ->
    ?assertEqual(<<"Error: Division by zero">>, divide_tool(#{<<"a">> => 10, <<"b">> => 0})).

test_negative_sqrt() ->
    ?assertEqual(<<"Error: Cannot calculate square root of negative number">>, 
                 sqrt_tool(#{<<"a">> => -4})).

test_negative_factorial() ->
    ?assertEqual(<<"Error: Cannot calculate factorial of negative number">>, 
                 factorial_tool(#{<<"n">> => -3})).

test_non_integer_factorial() ->
    ?assertEqual(<<"Error: Factorial requires integer input">>, 
                 factorial_tool(#{<<"n">> => 3.5})).

test_large_factorial() ->
    ?assertEqual(<<"Error: Number too large for factorial calculation">>, 
                 factorial_tool(#{<<"n">> => 200})).

%%====================================================================
%% Tool Handler Tests
%%====================================================================

tool_handler_tests() ->
    [
        {"Calculate tool", fun test_calculate_tool/0},
        {"Expression evaluation", fun test_expression_evaluation/0}
    ].

test_calculate_tool() ->
    ?assertEqual(<<"14">>, calculate_tool(#{<<"expression">> => <<"2 + 3 * 4">>})),
    ?assertEqual(<<"8">>, calculate_tool(#{<<"expression">> => <<"10 / 2 + 3">>})),
    ?assertEqual(<<"5">>, calculate_tool(#{<<"expression">> => <<"5">>})),
    ?assertEqual(<<"57">>, calculate_tool(#{<<"expression">> => <<"(15 + 8) * 3 - 12">>})),
    ?assertEqual(<<"23">>, calculate_tool(#{<<"expression">> => <<"15 + 8">>})),
    ?assertEqual(<<"69">>, calculate_tool(#{<<"expression">> => <<"23 * 3">>})).

test_expression_evaluation() ->
    %% Test simple expressions
    ?assertEqual(5, evaluate_expression("5")),
    ?assertEqual(8, evaluate_expression("3 + 5")),
    ?assertEqual(15, evaluate_expression("3 * 5")),
    ?assertEqual(2.0, evaluate_expression("10 / 5")),
    ?assertEqual(5, evaluate_expression("8 - 3")).

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
%% Legacy Functions for Backward Compatibility
%%====================================================================

%% Legacy run function that uses EUnit
run() ->
    eunit:test(?MODULE, [verbose]).