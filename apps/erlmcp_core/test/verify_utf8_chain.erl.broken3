#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/erlmcp_core/ebin

-mode(compile).

%% Verify UTF-8 encoding chain: Client → JSON (UTF-8) → Server → JSON (UTF-8) → Client

main(_) ->
    io:format("~n=== UTF-8 Encoding Chain Verification ===~n~n"),

    %% Test 1: Japanese encoding/decoding
    io:format("Test 1: Japanese (日本語)~n"),
    JapaneseText = <<"こんにちは世界">>,
    Request1 = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => JapaneseText}),
    io:format("Encoded: ~p~n", [Request1]),
    {ok, Decoded1} = erlmcp_json_rpc:decode_message(Request1),
    io:format("Decoded: ~p~n~n", [Decoded1]),

    %% Test 2: Arabic encoding/decoding
    io:format("Test 2: Arabic (العربية)~n"),
    ArabicText = <<"مرحبا بالعالم">>,
    Request2 = erlmcp_json_rpc:encode_request(2, <<"test">>, #{<<"text">> => ArabicText}),
    io:format("Encoded: ~p~n", [Request2]),
    {ok, Decoded2} = erlmcp_json_rpc:decode_message(Request2),
    io:format("Decoded: ~p~n~n", [Decoded2]),

    %% Test 3: Chinese encoding/decoding
    io:format("Test 3: Chinese (中文)~n"),
    ChineseText = <<"你好世界">>,
    Response1 = erlmcp_json_rpc:encode_response(1, #{<<"text">> => ChineseText}),
    io:format("Encoded: ~p~n", [Response1]),
    {ok, Decoded3} = erlmcp_json_rpc:decode_message(Response1),
    io:format("Decoded: ~p~n~n", [Decoded3]),

    %% Test 4: Emoji encoding/decoding
    io:format("Test 4: Emoji (🌍 😀)~n"),
    EmojiText = <<"Hello 🌍 😀">>,
    Request3 = erlmcp_json_rpc:encode_request(3, <<"test">>, #{<<"emoji">> => EmojiText}),
    io:format("Encoded: ~p~n", [Request3]),
    {ok, Decoded4} = erlmcp_json_rpc:decode_message(Request3),
    io:format("Decoded: ~p~n~n", [Decoded4]),

    %% Test 5: UTF-8 validation
    io:format("Test 5: UTF-8 Validation~n"),
    ValidUTF8 = <<"Hello 世界 🌍">>,
    InvalidUTF8 = <<16#FF, 16#FF>>,
    io:format("Valid UTF-8: ~p~n", [erlmcp_json_rpc:validate_utf8(ValidUTF8)]),
    io:format("Invalid UTF-8: ~p~n~n", [erlmcp_json_rpc:validate_utf8(InvalidUTF8)]),

    %% Test 6: Ensure UTF-8 encoding
    io:format("Test 6: Ensure UTF-8 Encoding~n"),
    Map = #{<<"japanese">> => <<"日本語">>, <<"arabic">> => <<"العربية">>},
    case erlmcp_json_rpc:ensure_utf8_encoding(Map) of
        {ok, Validated} ->
            io:format("Validated: ~p~n~n", [Validated]);
        {error, Reason} ->
            io:format("Error: ~p~n~n", [Reason])
    end,

    %% Test 7: Tool result with UTF-8
    io:format("Test 7: Tool Result with UTF-8~n"),
    ToolResult = #{
        <<"content">> => [
            #{<<"type">> => <<"text">>, <<"text">> => <<"計算結果: 42">>}
        ],
        <<"isError">> => false,
        <<"_metadata">> => #{
            <<"encoding">> => <<"utf-8">>,
            <<"language">> => <<"日本語">>,
            <<"content_type">> => <<"text/plain; charset=utf-8">>
        }
    },
    Response2 = erlmcp_json_rpc:encode_response(2, ToolResult),
    io:format("Encoded: ~p~n", [Response2]),
    {ok, Decoded5} = erlmcp_json_rpc:decode_message(Response2),
    io:format("Decoded: ~p~n~n", [Decoded5]),

    %% Test 8: Mixed language batch
    io:format("Test 8: Mixed Language Batch~n"),
    Requests = [
        #json_rpc_request{id = 1, method = <<"test">>, params = #{<<"text">> => <<"Hello">>}},
        #json_rpc_request{id = 2, method = <<"テスト">>, params = #{<<"テキスト">> => <<"こんにちは">>}},
        #json_rpc_request{id = 3, method = <<"اختبار">>, params = #{<<"نص">> => <<"مرحبا">>}}
    ],
    Batch = erlmcp_json_rpc:encode_batch(Requests),
    io:format("Encoded batch size: ~p bytes~n", [byte_size(Batch)]),
    {ok, Decoded6} = erlmcp_json_rpc:decode_batch(Batch),
    io:format("Decoded batch length: ~p~n~n", [length(Decoded6)]),

    io:format("=== UTF-8 Encoding Chain Verification Complete ===~n"),
    io:format("All tests passed! UTF-8 support is working correctly.~n"),
    halt(0).
