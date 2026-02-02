#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

%%% @doc OTP 28 Strict Comprehensions Demo for MCP Message Validation
%%%
%%% Run: escript strict_comprehensions_demo.erl
%%%
%%% Demonstrates:
%%% 1. Non-strict comprehensions (silent skip)
%%% 2. Strict comprehensions (fail-fast)
%%% 3. MCP message validation scenarios

-mode(compile).

main(_) ->
    io:format("~n=== OTP 28 Strict Comprehensions Demo ===~n~n"),
    check_otp_version(),

    %% Demo 1: Non-strict comprehension
    io:format("~n--- Demo 1: Non-Strict Comprehension ---~n"),
    demo_non_strict(),

    %% Demo 2: Strict comprehension (OTP 28+)
    io:format("~n--- Demo 2: Strict Comprehension ---~n"),
    demo_strict(),

    %% Demo 3: MCP message validation
    io:format("~n--- Demo 3: MCP Message Validation ---~n"),
    demo_mcp_validation(),

    %% Demo 4: Tool call extraction
    io:format("~n--- Demo 4: Tool Call Extraction ---~n"),
    demo_tool_calls(),

    %% Demo 5: Safe validation pattern
    io:format("~n~n--- Demo 5: Safe Validation Pattern ---~n"),
    demo_safe_validation(),

    io:format("~n=== Demo Complete ===~n"),
    ok.

%%%====================================================================
%%% Demo Functions
%%%====================================================================

check_otp_version() ->
    OTPVersion = erlang:system_info(otp_release),
    SupportsStrict = case OTPVersion of
        "28" ++ _ -> true;
        _ -> false
    end,
    io:format("OTP Version: ~s~n", [OTPVersion]),
    Status = case SupportsStrict of
        true -> "Supported";
        false -> "Not Available"
    end,
    io:format("Strict Comprehensions: ~s~n", [Status]).

%%% Demo 1: Non-Strict Comprehension
demo_non_strict() ->
    %% Mixed valid and invalid data
    Messages = [
        #{<<"type">> => <<"request">>, <<"id">> => 1},
        #{<<"invalid">> => <<"data">>},  % Missing 'type' field
        #{<<"type">> => <<"request">>, <<"id">> => 2}
    ],

    io:format("Input messages: ~p~n", [length(Messages)]),

    %% Non-strict: Silently skips invalid messages
    Valid = filter_valid_type(Messages),

    io:format("Valid messages (non-strict): ~p~n", [length(Valid)]),
    io:format("Invalid messages silently skipped!~n"),
    io:format("This is BAD for protocol validation~n").

filter_valid_type(Messages) ->
    [M || M <- Messages, is_map(M), maps:is_key(<<"type">>, M)].

%%% Demo 2: Strict Comprehension
demo_strict() ->
    OTPVersion = erlang:system_info(otp_release),
    case OTPVersion of
        "28" ++ _ ->
            %% Mixed valid and invalid data
            Messages = [
                #{<<"type">> => <<"request">>, <<"id">> => 1},
                #{<<"invalid">> => <<"data">>},  % Missing 'type' field
                #{<<"type">> => <<"request">>, <<"id">> => 2}
            ],

            io:format("Input messages: ~p~n", [length(Messages)]),

            %% Strict: Crashes on invalid messages
            try
                Valid = filter_valid_type_strict(Messages),
                io:format("Valid messages: ~p~n", [length(Valid)])
            catch
                error:{badmatch, BadMessage} ->
                    io:format("CRASH: {badmatch, ~p}~n", [BadMessage]),
                    io:format("This is GOOD for protocol validation!~n"),
                    io:format("System detects malformed data immediately~n")
            end;
        _ ->
            io:format("Skipped: Requires OTP 28+~n")
    end.

filter_valid_type_strict(Messages) ->
    [M || #{<<"type">> := _Type} = M <:- Messages].

%%% Demo 3: MCP Message Validation
demo_mcp_validation() ->
    %% Valid JSON-RPC messages
    ValidMessages = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"tools/list">>},
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 2, <<"method">> => <<"resources/list">>}
    ],

    %% Malformed message (missing jsonrpc)
    MalformedMessages = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"tools/list">>},
        #{<<"id">> => 2}  % Missing jsonrpc field
    ],

    io:format("Valid batch: ~p messages~n", [length(ValidMessages)]),
    validate_batch(ValidMessages),

    io:format("~nMalformed batch: ~p messages~n", [length(MalformedMessages)]),
    validate_batch(MalformedMessages).

%%% Demo 4: Tool Call Extraction
demo_tool_calls() ->
    %% Valid tool calls
    ValidCalls = [
        #{<<"method">> => <<"tools/call">>,
          <<"params">> => #{<<"name">> => <<"weather">>, <<"arguments">> => #{}}},
        #{<<"method">> => <<"tools/call">>,
          <<"params">> => #{<<"name">> => <<"calculator">>, <<"arguments">> => #{}}}
    ],

    %% Malformed tool call (missing name)
    MalformedCalls = [
        #{<<"method">> => <<"tools/call">>,
          <<"params">> => #{<<"arguments">> => #{}}}  % Missing name
    ],

    io:format("Valid tool calls:~n"),
    extract_tool_names(ValidCalls),

    io:format("~nMalformed tool calls:~n"),
    extract_tool_names(MalformedCalls).

%%% Demo 5: Safe Validation Pattern
demo_safe_validation() ->
    Messages = [
        #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1},
        #{<<"invalid">> => <<"data">>}
    ],

    io:format("Using safe validation pattern:~n"),

    case validate_messages_safe(Messages) of
        {ok, Valid} ->
            io:format("Valid messages: ~p~n", [length(Valid)]);
        {error, Reason} ->
            io:format("Validation failed: ~p~n", [Reason]),
            io:format("Handled gracefully!~n")
    end.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

validate_batch(Messages) ->
    OTPVersion = erlang:system_info(otp_release),
    case OTPVersion of
        "28" ++ _ ->
            try
                %% Strict validation
                Valid = [M || #{<<"jsonrpc">> := <<"2.0">>} = M <:- Messages],
                io:format("✓ All messages valid (~p)~n", [length(Valid)])
            catch
                error:{badmatch, BadMsg} ->
                    io:format("✗ Validation failed: ~p~n", [BadMsg]),
                    io:format("  → Supervisor would restart this process~n")
            end;
        _ ->
            io:format("(Skipped: Requires OTP 28+)~n")
    end.

extract_tool_names(Calls) ->
    OTPVersion = erlang:system_info(otp_release),
    case OTPVersion of
        "28" ++ _ ->
            try
                Names = [Name || #{<<"method">> := <<"tools/call">>,
                                   <<"params">> := #{<<"name">> := Name}} <:- Calls],
                io:format("  Tools: ~p~n", [Names])
            catch
                error:{badmatch, _} ->
                    io:format("  ✗ Malformed tool call detected~n")
            end;
        _ ->
            io:format("  (Skipped: Requires OTP 28+)~n")
    end.

validate_messages_safe(Messages) ->
    OTPVersion = erlang:system_info(otp_release),
    case OTPVersion of
        "28" ++ _ ->
            try
                Valid = [M || #{<<"jsonrpc">> := <<"2.0">>} = M <:- Messages],
                {ok, Valid}
            catch
                error:{badmatch, _} ->
                    {error, invalid_message_format}
            end;
        _ ->
            %% Non-strict fallback
            Valid = [M || M <- Messages, maps:is_key(<<"jsonrpc">>, M)],
            {ok, Valid}
    end.
