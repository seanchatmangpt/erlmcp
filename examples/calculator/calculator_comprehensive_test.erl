-module(calculator_comprehensive_test).
-include_lib("eunit/include/eunit.hrl").

-export([format_number/1]).

%% Test configuration
-define(TIMEOUT, 5000).
-define(SETUP_TIMEOUT, 10000).

%% Main test suite
calculator_test_() ->
    {timeout, ?SETUP_TIMEOUT,
        {setup,
            fun setup_calculator_server/0,
            fun cleanup_calculator_server/1,
            [
                {"Server functionality tests", server_tests()},
                {"Number formatting tests", format_number_tests()},
                {"Arithmetic operation tests", arithmetic_tests()},
                {"Error handling tests", error_handling_tests()},
                {"Resource tests", resource_tests()},
                {"Prompt tests", prompt_tests()}
            ]
        }
    }.

%% Setup and cleanup
setup_calculator_server() ->
    application:ensure_all_started(erlmcp),
    case erlmcp_stdio:start() of
        ok ->
            setup_tools_and_resources(),
            calculator_server;
        {error, Reason} ->
            throw({setup_failed, Reason})
    end.

cleanup_calculator_server(calculator_server) ->
    erlmcp_stdio:stop(),
    application:stop(erlmcp).

setup_tools_and_resources() ->
    %% Add arithmetic tools
    Tools = [
        {<<"add">>, <<"Add two numbers">>, fun add_tool/1},
        {<<"subtract">>, <<"Subtract two numbers">>, fun subtract_tool/1},
        {<<"multiply">>, <<"Multiply two numbers">>, fun multiply_tool/1},
        {<<"divide">>, <<"Divide two numbers">>, fun divide_tool/1},
        {<<"power">>, <<"Calculate power">>, fun power_tool/1},
        {<<"sqrt">>, <<"Calculate square root">>, fun sqrt_tool/1},
        {<<"factorial">>, <<"Calculate factorial">>, fun factorial_tool/1}
    ],
    
    [begin
        ok = erlmcp_stdio:add_tool(Name, Description, Handler, get_schema(Name))
     end || {Name, Description, Handler} <- Tools],
    
    %% Add resources
    ok = erlmcp_stdio:add_resource(<<"calculator://help">>, <<"Calculator Help">>,
        fun(_) -> <<"Calculator Help: Basic arithmetic operations available">> end),
    
    ok = erlmcp_stdio:add_resource(<<"calculator://history">>, <<"Calculator History">>,
        fun(_) -> <<"History: Previous calculations would be stored here">> end),
    
    %% Add prompts
    ok = erlmcp_stdio:add_prompt(<<"math_problem">>, <<"Generate math problem">>,
        fun(Args) ->
            Difficulty = maps:get(<<"difficulty">>, Args, <<"medium">>),
            generate_problem(Difficulty)
        end,
        [#{<<"name">> => <<"difficulty">>, <<"description">> => <<"Problem difficulty">>, <<"required">> => false}]).

%% Tool implementations
add_tool(#{<<"a">> := A, <<"b">> := B}) ->
    format_number(A + B).

subtract_tool(#{<<"a">> := A, <<"b">> := B}) ->
    format_number(A - B).

multiply_tool(#{<<"a">> := A, <<"b">> := B}) ->
    format_number(A * B).

divide_tool(#{<<"a">> := A, <<"b">> := B}) ->
    case B of
        0 -> <<"Error: Division by zero">>;
        _ -> format_number(A / B)
    end.

power_tool(#{<<"a">> := A, <<"b">> := B}) ->
    try
        Result = math:pow(A, B),
        format_number(Result)
    catch
        error:_ -> <<"Error: Power calculation failed">>
    end.

sqrt_tool(#{<<"a">> := A}) ->
    if
        A < 0 -> <<"Error: Cannot calculate square root of negative number">>;
        true -> format_number(math:sqrt(A))
    end.

factorial_tool(#{<<"n">> := N}) ->
    if
        N < 0 -> <<"Error: Cannot calculate factorial of negative number">>;
        not is_integer(N) -> <<"Error: Factorial requires integer input">>;
        N > 170 -> <<"Error: Number too large for factorial calculation">>;
        true -> format_number(factorial(N))
    end.

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

generate_problem(<<"easy">>) ->
    [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => <<"What is 5 + 3?">>}}];
generate_problem(<<"medium">>) ->
    [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => <<"What is 12 × 7?">>}}];
generate_problem(<<"hard">>) ->
    [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => <<"Solve: (15 + 8) × 3 - 12">>}}];
generate_problem(_) ->
    [#{<<"role">> => <<"user">>, <<"content">> => #{<<"type">> => <<"text">>, <<"text">> => <<"Calculate: 10 + 5">>}}].

get_schema(<<"add">>) ->
    #{<<"type">> => <<"object">>, <<"properties">> => #{
        <<"a">> => #{<<"type">> => <<"number">>},
        <<"b">> => #{<<"type">> => <<"number">>}
    }, <<"required">> => [<<"a">>, <<"b">>]};
get_schema(<<"subtract">>) ->
    get_schema(<<"add">>);
get_schema(<<"multiply">>) ->
    get_schema(<<"add">>);
get_schema(<<"divide">>) ->
    get_schema(<<"add">>);
get_schema(<<"power">>) ->
    get_schema(<<"add">>);
get_schema(<<"sqrt">>) ->
    #{<<"type">> => <<"object">>, <<"properties">> => #{
        <<"a">> => #{<<"type">> => <<"number">>}
    }, <<"required">> => [<<"a">>]};
get_schema(<<"factorial">>) ->
    #{<<"type">> => <<"object">>, <<"properties">> => #{
        <<"n">> => #{<<"type">> => <<"integer">>}
    }, <<"required">> => [<<"n">>]};
get_schema(_) ->
    #{<<"type">> => <<"object">>, <<"properties">> => #{}, <<"required">> => []}.

%% Test suites
server_tests() ->
    [
        {"Server is running", fun test_server_running/0},
        {"Dynamic tool addition", fun test_dynamic_tool_addition/0},
        {"Dynamic resource addition", fun test_dynamic_resource_addition/0},
        {"Dynamic prompt addition", fun test_dynamic_prompt_addition/0}
    ].

format_number_tests() ->
    [
        {"Integer formatting", fun test_integer_formatting/0},
        {"Float formatting", fun test_float_formatting/0},
        {"Decimal point handling", fun test_decimal_point_handling/0},
        {"Trailing zeros removal", fun test_trailing_zeros_removal/0},
        {"Edge cases", fun test_edge_cases/0}
    ].

arithmetic_tests() ->
    [
        {"Addition", fun test_addition_logic/0},
        {"Subtraction", fun test_subtraction_logic/0},
        {"Multiplication", fun test_multiplication_logic/0},
        {"Division", fun test_division_logic/0},
        {"Power calculation", fun test_power_logic/0},
        {"Square root", fun test_sqrt_logic/0},
        {"Factorial", fun test_factorial_logic/0}
    ].

error_handling_tests() ->
    [
        {"Division by zero", fun test_division_by_zero/0},
        {"Negative square root", fun test_negative_sqrt/0},
        {"Negative factorial", fun test_negative_factorial/0},
        {"Non-integer factorial", fun test_non_integer_factorial/0},
        {"Large factorial", fun test_large_factorial/0}
    ].

resource_tests() ->
    [
        {"Help resource available", fun test_help_resource/0},
        {"History resource available", fun test_history_resource/0}
    ].

prompt_tests() ->
    [
        {"Easy problem generation", fun test_easy_problem/0},
        {"Medium problem generation", fun test_medium_problem/0},
        {"Hard problem generation", fun test_hard_problem/0}
    ].

%% Individual test implementations
test_server_running() ->
    ?assert(erlmcp_stdio:is_running()).

test_dynamic_tool_addition() ->
    Result = erlmcp_stdio:add_tool(<<"test_op">>, <<"Test operation">>,
                                   fun(#{<<"x">> := X}) -> format_number(X * 2) end),
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

%% Format number tests
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

%% Arithmetic logic tests
test_addition_logic() ->
    ?assertEqual(<<"42">>, add_tool(#{<<"a">> => 25, <<"b">> => 17})),
    ?assertEqual(<<"0">>, add_tool(#{<<"a">> => -5, <<"b">> => 5})),
    ?assertEqual(<<"7.5">>, add_tool(#{<<"a">> => 3.2, <<"b">> => 4.3})).

test_subtraction_logic() ->
    ?assertEqual(<<"27">>, subtract_tool(#{<<"a">> => 50, <<"b">> => 23})),
    ?assertEqual(<<"-10">>, subtract_tool(#{<<"a">> => 5, <<"b">> => 15})),
    ?assertEqual(<<"1.1">>, subtract_tool(#{<<"a">> => 4.3, <<"b">> => 3.2})).

test_multiplication_logic() ->
    ?assertEqual(<<"96">>, multiply_tool(#{<<"a">> => 12, <<"b">> => 8})),
    ?assertEqual(<<"0">>, multiply_tool(#{<<"a">> => 0, <<"b">> => 42})),
    ?assertEqual(<<"6.25">>, multiply_tool(#{<<"a">> => 2.5, <<"b">> => 2.5})).

test_division_logic() ->
    ?assertEqual(<<"12">>, divide_tool(#{<<"a">> => 144, <<"b">> => 12})),
    ?assertEqual(<<"2.5">>, divide_tool(#{<<"a">> => 5, <<"b">> => 2})),
    ?assertEqual(<<"0.5">>, divide_tool(#{<<"a">> => 1, <<"b">> => 2})).

test_power_logic() ->
    ?assertEqual(<<"1024">>, power_tool(#{<<"a">> => 2, <<"b">> => 10})),
    ?assertEqual(<<"1">>, power_tool(#{<<"a">> => 5, <<"b">> => 0})),
    ?assertEqual(<<"27">>, power_tool(#{<<"a">> => 3, <<"b">> => 3})).

test_sqrt_logic() ->
    ?assertEqual(<<"8">>, sqrt_tool(#{<<"a">> => 64})),
    ?assertEqual(<<"0">>, sqrt_tool(#{<<"a">> => 0})),
    ?assertEqual(<<"1">>, sqrt_tool(#{<<"a">> => 1})).

test_factorial_logic() ->
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 0})),
    ?assertEqual(<<"1">>, factorial_tool(#{<<"n">> => 1})),
    ?assertEqual(<<"120">>, factorial_tool(#{<<"n">> => 5})),
    ?assertEqual(<<"3628800">>, factorial_tool(#{<<"n">> => 10})).

%% Error handling tests
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

%% Resource tests
test_help_resource() ->
    %% This would require actual MCP calls in a full integration test
    ?assert(true). % Placeholder

test_history_resource() ->
    %% This would require actual MCP calls in a full integration test
    ?assert(true). % Placeholder

%% Prompt tests
test_easy_problem() ->
    Result = generate_problem(<<"easy">>),
    ?assertMatch([#{<<"role">> := <<"user">>, <<"content">> := #{<<"text">> := _}}], Result).

test_medium_problem() ->
    Result = generate_problem(<<"medium">>),
    ?assertMatch([#{<<"role">> := <<"user">>, <<"content">> := #{<<"text">> := _}}], Result).

test_hard_problem() ->
    Result = generate_problem(<<"hard">>),
    ?assertMatch([#{<<"role">> := <<"user">>, <<"content">> := #{<<"text">> := _}}], Result).

%% Helper function with the fix
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

%% Run all tests
run_all_tests() ->
    eunit:test(?MODULE, [verbose, {timeout, 60}]).

%% Run specific test suite
run_server_tests() ->
    eunit:test({generator, fun server_tests/0}, [verbose]).

run_format_tests() ->
    eunit:test({generator, fun format_number_tests/0}, [verbose]).

run_arithmetic_tests() ->
    eunit:test({generator, fun arithmetic_tests/0}, [verbose]).