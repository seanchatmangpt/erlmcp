#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

-mode(compile).

main([]) ->
    io:format("~n=== Testing erlmcp_prompt_template ===~n~n"),

    % Test 1: Simple variable rendering
    io:format("Test 1: Simple variable rendering... "),
    try
        Result1 = erlmcp_prompt_template:render(<<"Hello {{name}}!">>, #{<<"name">> => <<"World">>}),
        case Result1 of
            <<"Hello World!">> -> io:format("PASS~n");
            _ -> io:format("FAIL: Got ~p~n", [Result1]), halt(1)
        end
    catch
        Class:Reason ->
            io:format("FAIL: ~p:~p~n", [Class, Reason]),
            halt(1)
    end,

    % Test 2: Multiple variables
    io:format("Test 2: Multiple variables... "),
    try
        Result2 = erlmcp_prompt_template:render(
            <<"{{greeting}} {{name}}!">>,
            #{<<"greeting">> => <<"Hi">>, <<"name">> => <<"Alice">>}
        ),
        case Result2 of
            <<"Hi Alice!">> -> io:format("PASS~n");
            _ -> io:format("FAIL: Got ~p~n", [Result2]), halt(1)
        end
    catch
        Class2:Reason2 ->
            io:format("FAIL: ~p:~p~n", [Class2, Reason2]),
            halt(1)
    end,

    % Test 3: Template validation
    io:format("Test 3: Template validation (valid)... "),
    case erlmcp_prompt_template:validate(<<"Hello {{name}}!">>) of
        ok -> io:format("PASS~n");
        Error -> io:format("FAIL: Got ~p~n", [Error]), halt(1)
    end,

    % Test 4: Template validation (invalid)
    io:format("Test 4: Template validation (invalid)... "),
    case erlmcp_prompt_template:validate(<<"Hello {{name">>) of
        {error, _} -> io:format("PASS~n");
        ok -> io:format("FAIL: Should have failed~n"), halt(1)
    end,

    % Test 5: Template syntax detection
    io:format("Test 5: Template syntax detection (has syntax)... "),
    case erlmcp_prompt_template:has_template_syntax(<<"Hello {{name}}">>) of
        true -> io:format("PASS~n");
        false -> io:format("FAIL~n"), halt(1)
    end,

    % Test 6: Template syntax detection (no syntax)
    io:format("Test 6: Template syntax detection (no syntax)... "),
    case erlmcp_prompt_template:has_template_syntax(<<"Hello World">>) of
        false -> io:format("PASS~n");
        true -> io:format("FAIL~n"), halt(1)
    end,

    % Test 7: Safe rendering (success)
    io:format("Test 7: Safe rendering (success)... "),
    case erlmcp_prompt_template:render_safe(<<"Hello {{name}}">>, #{<<"name">> => <<"Bob">>}) of
        {ok, <<"Hello Bob">>} -> io:format("PASS~n");
        Other -> io:format("FAIL: Got ~p~n", [Other]), halt(1)
    end,

    % Test 8: Essay prompt example from protocol docs
    io:format("Test 8: Essay prompt example... "),
    try
        Result8 = erlmcp_prompt_template:render(
            <<"Write a {{style}} essay about {{topic}}">>,
            #{<<"style">> => <<"formal">>, <<"topic">> => <<"climate change">>}
        ),
        case Result8 of
            <<"Write a formal essay about climate change">> -> io:format("PASS~n");
            _ -> io:format("FAIL: Got ~p~n", [Result8]), halt(1)
        end
    catch
        Class8:Reason8 ->
            io:format("FAIL: ~p:~p~n", [Class8, Reason8]),
            halt(1)
    end,

    % Test 9: Missing variable (should render as empty)
    io:format("Test 9: Missing variable handling... "),
    try
        Result9 = erlmcp_prompt_template:render(
            <<"Hello {{name}}!">>,
            #{}
        ),
        case Result9 of
            <<"Hello !">> -> io:format("PASS~n");
            _ -> io:format("FAIL: Got ~p~n", [Result9]), halt(1)
        end
    catch
        Class9:Reason9 ->
            io:format("FAIL: ~p:~p~n", [Class9, Reason9]),
            halt(1)
    end,

    % Test 10: Numeric values
    io:format("Test 10: Numeric value rendering... "),
    try
        Result10 = erlmcp_prompt_template:render(
            <<"The answer is {{answer}}">>,
            #{<<"answer">> => 42}
        ),
        case Result10 of
            <<"The answer is 42">> -> io:format("PASS~n");
            _ -> io:format("FAIL: Got ~p~n", [Result10]), halt(1)
        end
    catch
        Class10:Reason10 ->
            io:format("FAIL: ~p:~p~n", [Class10, Reason10]),
            halt(1)
    end,

    io:format("~n=== All tests passed! ===~n~n"),
    halt(0).
