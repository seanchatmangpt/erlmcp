-module(calculator_demo).
-export([run/0]).

run() ->
    %% Ensure all required applications are started
    application:ensure_all_started(erlmcp),
    
    io:format("=== Calculator MCP Demo ===~n~n"),
    
    %% Start the calculator client
    {ok, Client} = calculator_client:start_link(),
    
    %% Connect to the calculator server
    case calculator_client:connect(Client) of
        {ok, ServerInfo} ->
            io:format("✓ Connected to calculator server: ~s~n", 
                     [maps:get(<<"name">>, ServerInfo, <<"unknown">>)]),
            run_demo(Client);
        {error, Reason} ->
            io:format("✗ Failed to connect: ~p~n", [Reason])
    end,
    
    %% Clean up
    calculator_client:stop(Client).

run_demo(Client) ->
    io:format("~n=== Basic Arithmetic ===~n"),
    
    %% Test basic arithmetic
    test_operation(fun() -> calculator_client:add(Client, 25, 17) end, 
                   "25 + 17"),
    test_operation(fun() -> calculator_client:subtract(Client, 50, 23) end, 
                   "50 - 23"),
    test_operation(fun() -> calculator_client:multiply(Client, 12, 8) end, 
                   "12 × 8"),
    test_operation(fun() -> calculator_client:divide(Client, 144, 12) end, 
                   "144 ÷ 12"),
    
    io:format("~n=== Advanced Operations ===~n"),
    
    %% Test advanced operations
    test_operation(fun() -> calculator_client:power(Client, 2, 10) end, 
                   "2^10"),
    test_operation(fun() -> calculator_client:sqrt(Client, 64) end, 
                   "√64"),
    test_operation(fun() -> calculator_client:factorial(Client, 5) end, 
                   "5!"),
    
    io:format("~n=== Expression Evaluation ===~n"),
    
    %% Test expression evaluation
    test_operation(fun() -> calculator_client:calculate(Client, <<"2 + 3 * 4">>) end, 
                   "2 + 3 * 4"),
    test_operation(fun() -> calculator_client:calculate(Client, <<"10 / 2 + 3">>) end, 
                   "10 / 2 + 3"),
    
    io:format("~n=== Error Handling ===~n"),
    
    %% Test error conditions
    test_operation(fun() -> calculator_client:divide(Client, 10, 0) end, 
                   "10 ÷ 0 (division by zero)"),
    test_operation(fun() -> calculator_client:sqrt(Client, -4) end, 
                   "√(-4) (negative square root)"),
    test_operation(fun() -> calculator_client:factorial(Client, -3) end, 
                   "(-3)! (negative factorial)"),
    
    io:format("~n=== Resources ===~n"),
    
    %% Test resources
    test_resource(fun() -> calculator_client:get_help(Client) end, 
                  "calculator://help"),
    test_resource(fun() -> calculator_client:get_history(Client) end, 
                  "calculator://history"),
    
    io:format("~n=== Prompts ===~n"),
    
    %% Test prompts
    test_prompt(fun() -> calculator_client:generate_problem(Client, <<"easy">>, <<"addition">>) end, 
                "easy addition problem"),
    test_prompt(fun() -> calculator_client:generate_problem(Client, <<"hard">>, <<"algebra">>) end, 
                "hard algebra problem"),
    
    io:format("~n=== Demo Complete ===~n").

test_operation(Operation, Description) ->
    case Operation() of
        {ok, Result} ->
            io:format("✓ ~s = ~s~n", [Description, Result]);
        {error, Reason} ->
            io:format("✗ ~s failed: ~p~n", [Description, Reason])
    end.

test_resource(ResourceOp, Description) ->
    case ResourceOp() of
        {ok, Content} ->
            %% Truncate content for display
            DisplayContent = truncate_content(Content, 100),
            io:format("✓ ~s: ~s~n", [Description, DisplayContent]);
        {error, Reason} ->
            io:format("✗ ~s failed: ~p~n", [Description, Reason])
    end.

test_prompt(PromptOp, Description) ->
    case PromptOp() of
        {ok, Messages} when is_list(Messages) ->
            case Messages of
                [#{<<"content">> := #{<<"text">> := Text}} | _] ->
                    io:format("✓ ~s: ~s~n", [Description, Text]);
                _ ->
                    io:format("✓ ~s: Generated ~p messages~n", [Description, length(Messages)])
            end;
        {ok, Result} ->
            io:format("✓ ~s: ~p~n", [Description, Result]);
        {error, Reason} ->
            io:format("✗ ~s failed: ~p~n", [Description, Reason])
    end.

truncate_content(Content, MaxLen) when is_binary(Content) ->
    if
        byte_size(Content) > MaxLen ->
            Size = MaxLen - 3,
            <<Truncated:Size/binary, _/binary>> = Content,
            <<Truncated/binary, "...">>;
        true ->
            Content
    end;
truncate_content(Content, _MaxLen) ->
    Content.