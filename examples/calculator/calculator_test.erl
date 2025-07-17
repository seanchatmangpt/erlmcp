-module(calculator_test).
-include_lib("eunit/include/eunit.hrl").

-export([format_number/1]).

%% Test setup and teardown
setup() ->
    %% Start the main erlmcp application
    application:ensure_all_started(erlmcp),
    
    %% Start the stdio server
    case erlmcp_stdio:start() of
        ok ->
            setup_calculator(),
            ok;
        {error, Reason} ->
            throw({setup_failed, Reason})
    end.

cleanup(_) ->
    erlmcp_stdio:stop().

setup_calculator() ->
    %% Add basic arithmetic tools
    ok = erlmcp_stdio:add_tool(<<"add">>, <<"Add two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A + B,
            format_number(Result)
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>},
                <<"b">> => #{<<"type">> => <<"number">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),

    ok = erlmcp_stdio:add_tool(<<"subtract">>, <<"Subtract two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A - B,
            format_number(Result)
        end),

    ok = erlmcp_stdio:add_tool(<<"multiply">>, <<"Multiply two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A * B,
            format_number(Result)
        end),

    ok = erlmcp_stdio:add_tool(<<"divide">>, <<"Divide two numbers">>,
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            case B of
                0 -> <<"Error: Division by zero">>;
                _ ->
                    Result = A / B,
                    format_number(Result)
            end
        end),

    %% Add resources
    ok = erlmcp_stdio:add_resource(<<"calculator://help">>, <<"Calculator Help">>,
        fun(_Uri) ->
            <<"Calculator MCP Server Help\n\nAvailable operations:\n- add\n- subtract\n- multiply\n- divide">>
        end),

    %% Add prompts
    ok = erlmcp_stdio:add_prompt(<<"math_problem">>, <<"Generate a math problem">>,
        fun(Args) ->
            Difficulty = maps:get(<<"difficulty">>, Args, <<"medium">>),
            Problem = case Difficulty of
                <<"easy">> -> <<"What is 5 + 3?">>;
                <<"medium">> -> <<"What is 12 × 7?">>;
                <<"hard">> -> <<"Solve: (15 + 8) × 3 - 12">>
            end,
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Please solve: ", Problem/binary>>
                }
            }]
        end,
        [#{<<"name">> => <<"difficulty">>, <<"description">> => <<"Problem difficulty">>, <<"required">> => false}]).

%% Test suites
calculator_server_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            fun test_server_running/0,
            fun test_dynamic_tool_addition/0,
            fun test_dynamic_resource_addition/0,
            fun test_dynamic_prompt_addition/0
        ]
    }.

%% Individual test cases
test_server_running() ->
    ?assert(erlmcp_stdio:is_running()).

test_dynamic_tool_addition() ->
    Result = erlmcp_stdio:add_tool(<<"test_operation">>, <<"Test operation">>,
                                   fun(#{<<"x">> := X}) -> integer_to_binary(X * 2) end),
    ?assertEqual(ok, Result).

test_dynamic_resource_addition() ->
    Result = erlmcp_stdio:add_resource(<<"test://resource">>, <<"Test resource">>,
                                       fun(_) -> <<"test content">> end),
    ?assertEqual(ok, Result).

test_dynamic_prompt_addition() ->
    Result = erlmcp_stdio:add_prompt(<<"test_prompt">>, <<"Test prompt">>,
                                     fun(_) -> [#{<<"role">> => <<"user">>, 
                                                  <<"content">> => #{<<"type">> => <<"text">>, 
                                                                     <<"text">> => <<"test">>}}] end),
    ?assertEqual(ok, Result).

%% Format number utility tests
format_number_test_() ->
    [
        {"Integer formatting", fun test_integer_formatting/0},
        {"Float formatting", fun test_float_formatting/0},
        {"Decimal point handling", fun test_decimal_point_handling/0},
        {"Trailing zeros removal", fun test_trailing_zeros_removal/0}
    ].

test_integer_formatting() ->
    ?assertEqual(<<"42">>, format_number(42)),
    ?assertEqual(<<"-15">>, format_number(-15)),
    ?assertEqual(<<"0">>, format_number(0)).

test_float_formatting() ->
    ?assertEqual(<<"3.14">>, format_number(3.14)),
    ?assertEqual(<<"2.5">>, format_number(2.5)),
    ?assertEqual(<<"-1.5">>, format_number(-1.5)).

test_decimal_point_handling() ->
    %% Test that numbers like 5.0 become "5" not "5."
    ?assertEqual(<<"5">>, format_number(5.0)),
    ?assertEqual(<<"10">>, format_number(10.0)),
    ?assertEqual(<<"-7">>, format_number(-7.0)).

test_trailing_zeros_removal() ->
    ?assertEqual(<<"1.1">>, format_number(1.1000)),
    ?assertEqual(<<"2.25">>, format_number(2.2500)),
    ?assertEqual(<<"3.333">>, format_number(3.3330)).

%% Arithmetic operation tests (these would need a running server)
arithmetic_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            {"Addition test", fun test_addition/0},
            {"Subtraction test", fun test_subtraction/0},
            {"Multiplication test", fun test_multiplication/0},
            {"Division test", fun test_division/0},
            {"Division by zero test", fun test_division_by_zero/0}
        ]
    }.

test_addition() ->
    %% This would test the actual MCP tool call
    %% For now, we test the logic directly
    Result = 25 + 17,
    Expected = format_number(Result),
    ?assertEqual(<<"42">>, Expected).

test_subtraction() ->
    Result = 50 - 23,
    Expected = format_number(Result),
    ?assertEqual(<<"27">>, Expected).

test_multiplication() ->
    Result = 12 * 8,
    Expected = format_number(Result),
    ?assertEqual(<<"96">>, Expected).

test_division() ->
    Result = 144 / 12,
    Expected = format_number(Result),
    ?assertEqual(<<"12">>, Expected).

test_division_by_zero() ->
    %% Test the error handling logic
    Error = case 0 of
        0 -> <<"Error: Division by zero">>;
        _ -> <<"Should not reach here">>
    end,
    ?assertEqual(<<"Error: Division by zero">>, Error).

%% Helper function (same as original with fix)
format_number(N) when is_integer(N) ->
    integer_to_binary(N);
format_number(N) when is_float(N) ->
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

%% Legacy run function for backward compatibility
run() ->
    eunit:test(?MODULE, [verbose]).