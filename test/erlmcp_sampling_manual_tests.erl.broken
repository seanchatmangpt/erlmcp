%%%-------------------------------------------------------------------
%%% @doc Manual test script for sampling capability
%%% Run with: erlc -I ../apps/erlmcp_core/include test/erlmcp_sampling_manual_tests.erl && erl -pa ../apps/erlmcp_core/ebin -eval "erlmcp_sampling_manual_tests:run(), init:stop()."
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_sampling_manual_tests).
-export([run/0, test_validation/0, test_mock_llm/0]).

-include("../apps/erlmcp_core/include/erlmcp.hrl").

%% @doc Run all manual tests
run() ->
    io:format("~n=== Sampling Capability Manual Tests ===~n~n"),
    test_validation(),
    test_mock_llm(),
    io:format("~n=== All Manual Tests Complete ===~n").

%% @doc Test validation functions
test_validation() ->
    io:format("~n--- Testing Validation ---~n"),

    %% Test valid messages
    ValidMessages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Hello">>}
    ],
    case validate_sampling_messages(ValidMessages) of
        ok -> io:format("✓ Valid messages test passed~n");
        {error, Reason} -> io:format("✗ Valid messages test failed: ~p~n", [Reason])
    end,

    %% Test empty messages
    case validate_sampling_messages([]) of
        {error, <<"Messages list cannot be empty">>} ->
            io:format("✓ Empty messages test passed~n");
        {error, Reason} ->
            io:format("✗ Empty messages test failed: ~p~n", [Reason])
    end,

    %% Test invalid format
    InvalidMessages = [#{<<"content">> => <<"No role">>}],
    case validate_sampling_messages(InvalidMessages) of
        {error, <<"Invalid message format">>} ->
            io:format("✓ Invalid format test passed~n");
        {error, Reason} ->
            io:format("✗ Invalid format test failed: ~p~n", [Reason])
    end.

%% @doc Test mock LLM provider
test_mock_llm() ->
    io:format("~n--- Testing Mock LLM Provider ---~n"),

    %% Set response mode to echo
    erlmcp_mock_llm:set_response_mode(echo),

    %% Test echo mode
    Messages = [
        #{<<"role">> => <<"user">>, <<"content">> => <<"Test message">>}
    ],
    Params = #{<<"temperature">> => 0.7},

    case erlmcp_mock_llm:create_message(Messages, Params) of
        {ok, Response} ->
            io:format("✓ Mock LLM echo mode test passed~n"),
            io:format("  Response: ~p~n", [Response]);
        {error, Reason} ->
            io:format("✗ Mock LLM echo mode test failed: ~p~n", [Reason])
    end,

    %% Test template mode
    erlmcp_mock_llm:set_response_mode(template),
    case erlmcp_mock_llm:create_message(Messages, Params) of
        {ok, Response} ->
            io:format("✓ Mock LLM template mode test passed~n"),
            io:format("  Response: ~p~n", [Response]);
        {error, Reason} ->
            io:format("✗ Mock LLM template mode test failed: ~p~n", [Reason])
    end.

%%====================================================================
%% Validation Functions (copied from erlmcp_server for testing)
%%====================================================================

%% @doc Validate sampling messages format
validate_sampling_messages(Messages) when is_list(Messages) ->
    case length(Messages) of
        0 ->
            {error, <<"Messages list cannot be empty">>};
        _ ->
            case lists:all(fun validate_sampling_message/1, Messages) of
                true -> ok;
                false -> {error, <<"Invalid message format">>}
            end
    end;
validate_sampling_messages(_) ->
    {error, <<"Messages must be a list">>}.

%% @doc Validate a single sampling message
validate_sampling_message(Message) when is_map(Message) ->
    Role = maps:get(<<"role">>, Message, undefined),
    Content = maps:get(<<"content">>, Message, undefined),

    case {Role, Content} of
        {undefined, _} -> false;
        {_, undefined} -> false;
        {R, _} when is_binary(R) ->
            case Content of
                C when is_binary(C); is_map(C) -> true;
                _ -> false
            end;
        _ -> false
    end;
validate_sampling_message(_) ->
    false.
