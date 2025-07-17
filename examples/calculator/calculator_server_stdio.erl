-module(calculator_server_stdio).
-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Enable debug logging
    logger:remove_handler(default),
    logger:add_handler(stderr_handler, logger_std_h, #{
        level => info,
        config => #{type => standard_error}
    }),
    logger:set_primary_config(level, info),

    logger:info("Starting Calculator MCP server...~n"),

    %% Start the stdio MCP server
    case erlmcp_stdio:start() of
        ok ->
            logger:info("Successfully started stdio server~n"),
            setup_calculator_server(),
            logger:info("Calculator server setup complete, waiting for shutdown...~n"),
            wait_for_shutdown();
        {error, Reason} ->
            logger:error("Failed to start stdio server: ~p", [Reason]),
            halt(1)
    end.

setup_calculator_server() ->
    %% Add basic arithmetic tools
    ok = erlmcp_stdio:add_tool(<<"add">>, <<"Add two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A + B,
            format_number(Result)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"First number">>},
                <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Second number">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"subtract">>, <<"Subtract two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A - B,
            format_number(Result)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"First number">>},
                <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Second number">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"multiply">>, <<"Multiply two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A * B,
            format_number(Result)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"First number">>},
                <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Second number">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"divide">>, <<"Divide two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            case B of
                0 -> <<"Error: Division by zero">>;
                _ -> 
                    Result = A / B,
                    format_number(Result)
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Dividend">>},
                <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Divisor">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"power">>, <<"Calculate power (a^b)">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            try
                Result = math:pow(A, B),
                format_number(Result)
            catch
                error:badarith ->
                    <<"Error: Invalid power operation">>;
                error:_ ->
                    <<"Error: Power calculation failed">>
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Base">>},
                <<"b">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Exponent">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"sqrt">>, <<"Calculate square root">>,
        fun(#{<<"a">> := A}) ->
            if
                A < 0 -> <<"Error: Cannot calculate square root of negative number">>;
                true ->
                    Result = math:sqrt(A),
                    format_number(Result)
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>, <<"description">> => <<"Number to find square root of">>}
            },
            <<"required">> => [<<"a">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"factorial">>, <<"Calculate factorial">>,
        fun(#{<<"n">> := N}) ->
            if
                N < 0 -> <<"Error: Cannot calculate factorial of negative number">>;
                not is_integer(N) -> <<"Error: Factorial requires integer input">>;
                N > 170 -> <<"Error: Number too large for factorial calculation">>;
                true ->
                    Result = factorial(N),
                    format_number(Result)
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"n">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"Integer to calculate factorial of">>}
            },
            <<"required">> => [<<"n">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"calculate">>, <<"Evaluate a mathematical expression">>,
        fun(#{<<"expression">> := Expression}) ->
            try
                Result = evaluate_expression(Expression),
                format_number(Result)
            catch
                error:Reason ->
                    iolist_to_binary(io_lib:format("Error: ~p", [Reason]));
                throw:Reason ->
                    iolist_to_binary(io_lib:format("Error: ~p", [Reason]))
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"expression">> => #{<<"type">> => <<"string">>, <<"description">> => <<"Mathematical expression to evaluate">>}
            },
            <<"required">> => [<<"expression">>]
        }),

    %% Add a resource for calculation history
    ok = erlmcp_stdio:add_resource(<<"calculator://history">>, <<"Calculation History">>,
        fun(_Uri) ->
            %% In a real implementation, you'd store history in a persistent way
            <<"Recent calculations:\n- Last session calculations would be stored here\n- This is a demo resource">>
        end,
        <<"text/plain">>),

    %% Add a resource for calculator help
    ok = erlmcp_stdio:add_resource(<<"calculator://help">>, <<"Calculator Help">>,
        fun(_Uri) ->
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
              "- Use calculate tool: {\"expression\": \"2 + 3 * 4\"}\n">>
        end,
        <<"text/plain">>),

    %% Add a prompt for generating math problems
    ok = erlmcp_stdio:add_prompt(<<"math_problem">>, <<"Generate a math problem">>,
        fun(Args) ->
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
            }]
        end,
        [
            #{<<"name">> => <<"difficulty">>, <<"description">> => <<"Problem difficulty (easy, medium, hard)">>, <<"required">> => false},
            #{<<"name">> => <<"type">>, <<"description">> => <<"Problem type (addition, multiplication, algebra, mixed)">>, <<"required">> => false}
        ]),

    logger:info("Calculator server configured with tools, resources, and prompts~n").

%% Helper functions
format_number(N) when is_integer(N) ->
    integer_to_binary(N);
format_number(N) when is_float(N) ->
    %% Format float with up to 10 decimal places, removing trailing zeros
    Formatted = io_lib:format("~.10f", [N]),
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

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

%% Simple expression evaluator
evaluate_expression(Expression) when is_binary(Expression) ->
    %% Convert to string and evaluate
    ExprStr = binary_to_list(Expression),
    evaluate_expression(ExprStr);
evaluate_expression(Expression) when is_list(Expression) ->
    %% Simple tokenizer and evaluator
    Tokens = tokenize(Expression),
    evaluate_tokens(Tokens).

tokenize(Expression) ->
    %% Simple tokenizer for basic arithmetic
    %% Remove spaces first
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
    % Handle decimal point as part of number
    {Number, Remaining} = read_number([C | Rest]),
    tokenize_chars(Remaining, [Number | Acc]);
tokenize_chars([_C | Rest], Acc) ->
    % Skip unknown characters
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
    %% Simple expression evaluator (left-to-right, no operator precedence)
    %% In a real implementation, you'd want proper precedence handling
    case Tokens of
        [] -> throw(empty_expression);
        [Number] when is_number(Number) -> Number;
        _ -> evaluate_simple(lists:reverse(Tokens))
    end.

evaluate_simple([Result]) when is_number(Result) ->
    Result;
evaluate_simple([Op, B, A | Rest]) when is_number(A), is_number(B) ->
    Result = case Op of
        add -> A + B;
        subtract -> A - B;
        multiply -> A * B;
        divide when B =/= 0 -> A / B;
        divide -> throw(division_by_zero);
        _ -> throw({unknown_operator, Op})
    end,
    evaluate_simple([Result | Rest]);
evaluate_simple(_) ->
    throw(invalid_expression).

wait_for_shutdown() ->
    %% Monitor the stdio server process to know when it's done
    case whereis(erlmcp_stdio_server) of
        undefined ->
            logger:warn("Stdio server not found, exiting~n");
        Pid ->
            monitor(process, Pid),
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    logger:info("Stdio server terminated, exiting~n")
            end
    end.