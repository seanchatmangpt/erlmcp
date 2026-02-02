-module(erlmcp_tool_calculator).

%% Calculator tool for erlmcp_tool_router
%% Evaluates mathematical expressions

-export([handle/2]).

%% @doc Handle calculator tool invocation
-spec handle(binary(), map()) -> {ok, map()} | {error, term()}.
handle(Expression, _Captures) ->
    try
        %% Safe evaluation of mathematical expressions
        %% Parse and evaluate the expression
        Tokens = tokenize(Expression),
        {Result, _} = evaluate(Tokens),
        {ok, #{<<"result">> => list_to_binary(io_lib:format("~p", [Result]))}}
    catch
        _:_:Stacktrace ->
            {error, {evaluation_failed, Stacktrace}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Tokenize expression into numbers and operators
tokenize(Expression) ->
    binary:split(Expression, <<$\s>>, [global, trim_all]).

%% Evaluate expression (simple calculator)
evaluate(Tokens) ->
    evaluate(Tokens, 0, "+").

evaluate([], Acc, _Op) ->
    {Acc, []};
evaluate([<<"(">> | Rest], _Acc, Op) ->
    {SubExpr, Remaining} = evaluate_parens(Rest),
    %% Apply operator with accumulator and sub-expression result
    NewAcc = case Op of
                 "+" -> _Acc + SubExpr;
                 "-" -> _Acc - SubExpr;
                 "*" -> _Acc * SubExpr;
                 "/" -> _Acc div SubExpr
             end,
    evaluate(Remaining, NewAcc, "+");
evaluate([<<"+">> | Rest], Acc, _Op) ->
    evaluate(Rest, Acc, "+");
evaluate([<<"-">> | Rest], Acc, _Op) ->
    evaluate(Rest, Acc, "-");
evaluate([<<"*">> | Rest], Acc, _Op) ->
    evaluate(Rest, Acc, "*");
evaluate([<<"/">> | Rest], Acc, _Op) ->
    evaluate(Rest, Acc, "/");
evaluate([Token | Rest], Acc, Op) ->
    Value = binary_to_integer(Token),
    NewAcc = apply_op(Op, Acc, Value),
    evaluate(Rest, NewAcc, "+").

apply_op("+", A, B) -> A + B;
apply_op("-", A, B) -> A - B;
apply_op("*", A, B) -> A * B;
apply_op("/", A, B) -> A div B.

%% Evaluate expression in parentheses
evaluate_parens(Tokens) ->
    evaluate_parens(Tokens, [], 0).

evaluate_parens([], _Stack, _Depth) ->
    {0, []};
evaluate_parens([<<")">> | Rest], Stack, 0) ->
    %% Evaluate the sub-expression in parentheses
    {SubExpr, _} = evaluate(lists:reverse(Stack), 0, "+"),
    {SubExpr, Rest};
evaluate_parens([<<")">> | Rest], Stack, Depth) ->
    evaluate_parens(Rest, [<<")">> | Stack], Depth - 1);
evaluate_parens([<<"(">> | Rest], Stack, Depth) ->
    evaluate_parens(Rest, [<<"(">> | Stack], Depth + 1);
evaluate_parens([Token | Rest], Stack, Depth) ->
    evaluate_parens(Rest, [Token | Stack], Depth).
