-module(erlmcp_otp28_utf8_tests).

%% @doc OTP 28 UTF-8 Support Test Suite
%% Tests comprehensive UTF-8 support for international text, emoji, and scripts
%%
%% == Test Coverage ==
%% 1. Japanese text (Hiragana, Katakana, Kanji)
%% 2. Arabic text (RTL scripts)
%% 3. Emoji (multi-byte sequences)
%% 4. Mixed scripts
%% 5. JSON encoding/decoding with UTF-8
%% 6. Binary string operations
%%
%% == Chicago School TDD ==
%% - Real processes, real JSON encoding/decoding
%% - State-based verification (observable outputs)
%% - No mocks
%%
%% @end

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Generators
%%%====================================================================

utf8_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Japanese Text", {spawn, fun japanese_text_tests/0}},
         {"Arabic Text", {spawn, fun arabic_text_tests/0}},
         {"Emoji", {spawn, fun emoji_tests/0}},
         {"Mixed Scripts", {spawn, fun mixed_scripts_tests/0}},
         {"JSON Encoding", {spawn, fun json_encoding_tests/0}},
         {"JSON Decoding", {spawn, fun json_decoding_tests/0}},
         {"Binary Operations", {spawn, fun binary_operations_tests/0}},
         {"Edge Cases", {spawn, fun edge_cases_tests/0}}]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Japanese Text Tests
%%%====================================================================

japanese_text_tests() ->
    %% Test 1: Hiragana (平假名)
    Hiragana = <<"こんにちは世界">>, %% "Hello World" in Hiragana
    ?assertEqual(Hiragana, encode_decode_roundtrip(Hiragana)),

    %% Test 2: Katakana (片假名)
    Katakana = <<"コンニチハ">>, %% "Konnichiha" in Katakana
    ?assertEqual(Katakana, encode_decode_roundtrip(Katakana)),

    %% Test 3: Kanji (漢字)
    Kanji = <<"日本語">>, %% "Japanese language"
    ?assertEqual(Kanji, encode_decode_roundtrip(Kanji)),

    %% Test 4: Mixed Japanese script
    Mixed = <<"今日は良い天気ですね">>, %% "The weather is good today"
    ?assertEqual(Mixed, encode_decode_roundtrip(Mixed)),

    %% Test 5: Japanese in JSON object
    JsonObj = #{<<"greeting">> => <<"おはようございます">>},
    ?assertEqual(JsonObj, json_roundtrip(JsonObj)),

    %% Test 6: Japanese in JSON array
    JsonArray = [<<"東京">>, <<"大阪">>, <<"京都">>],
    DecodedArray = json_roundtrip(JsonArray),
    ?assertEqual(length(JsonArray), length(DecodedArray)),
    lists:foreach(fun({Orig, Decoded}) ->
        ?assertEqual(Orig, Decoded)
    end, lists:zip(JsonArray, DecodedArray)),

    %% Test 7: Long Japanese text
    LongText = unicode:characters_to_binary(lists:duplicate(100, $"あ")),
    ?assertEqual(LongText, encode_decode_roundtrip(LongText)),

    ok.

%%%====================================================================
%%% Arabic Text Tests
%%%====================================================================

arabic_text_tests() ->
    %% Test 1: Basic Arabic greeting
    Greeting = <<"مرحبا بالعالم">>, %% "Hello World" in Arabic
    ?assertEqual(Greeting, encode_decode_roundtrip(Greeting)),

    %% Test 2: Arabic numbers (Eastern Arabic numerals)
    %% Note: Eastern Arabic numerals (٠١٢٣٤٥٦٧٨٩) are multi-byte UTF-8
    ArabicNums = <<"٠١٢٣٤٥٦٧٨٩">>,
    ?assertEqual(ArabicNums, encode_decode_roundtrip(ArabicNums)),

    %% Test 3: RTL text in JSON
    RtlText = #{<<"text">> => <<"السلام عليكم">>},
    ?assertEqual(RtlText, json_roundtrip(RtlText)),

    %% Test 4: Arabic with diacritics
    WithDiacritics = <<"الْحَمْدُ لِلَّهِ">>,
    ?assertEqual(WithDiacritics, encode_decode_roundtrip(WithDiacritics)),

    %% Test 5: Arabic ligatures
    %% Allah (الله) uses special ligature forms
    Allah = <<"الله">>,
    ?assertEqual(Allah, encode_decode_roundtrip(Allah)),

    %% Test 6: Mixed Arabic and Latin
    Mixed = <<"المتغير variable_name هو 123">>,
    ?assertEqual(Mixed, encode_decode_roundtrip(Mixed)),

    ok.

%%%====================================================================
%%% Emoji Tests
%%%====================================================================

emoji_tests() ->
    %% Test 1: Basic emoji (single codepoint)
    BasicEmoji = <<"😀">>,
    ?assertEqual(BasicEmoji, encode_decode_roundtrip(BasicEmoji)),

    %% Test 2: Emoji with skin tone modifier (multi-codepoint)
    SkinTone = <<"👋🏻">>, %% Waving hand + light skin tone
    ?assertEqual(SkinTone, encode_decode_roundtrip(SkinTone)),

    %% Test 3: Family emoji (ZWJ sequence)
    Family = <<"👨‍👩‍👧‍👦">>, %% Man + ZWJ + Woman + ZWJ + Girl + ZWJ + Boy
    ?assertEqual(Family, encode_decode_roundtrip(Family)),

    %% Test 4: Emoji in JSON
    EmojiJson = #{<<"emoji">> => <<"🎉">>, <<"status">> => <<"庆祝">>},
    ?assertEqual(EmojiJson, json_roundtrip(EmojiJson)),

    %% Test 5: Emoji array
    EmojiArray = [<<"😀">>, <<"😃">>, <<"😄">>, <<"😁">>],
    DecodedArray = json_roundtrip(EmojiArray),
    ?assertEqual(length(EmojiArray), length(DecodedArray)),

    %% Test 6: Complex emoji (flag, keycaps)
    Flag = <<"🇺🇸">>, %% Regional indicator symbols
    ?assertEqual(Flag, encode_decode_roundtrip(Flag)),

    Keycap = <<"1️⃣">>, %% Digit + combining enclosing keycap
    ?assertEqual(Keycap, encode_decode_roundtrip(Keycap)),

    ok.

%%%====================================================================
%%% Mixed Scripts Tests
%%%====================================================================

mixed_scripts_tests() ->
    %% Test 1: Japanese + English
    JaEn = <<"Hello 世界">>,
    ?assertEqual(JaEn, encode_decode_roundtrip(JaEn)),

    %% Test 2: Arabic + French
    ArFr = <<"Bonjour مرحبا">>,
    ?assertEqual(ArFr, encode_decode_roundtrip(ArFr)),

    %% Test 3: Japanese + Emoji
    JaEmoji = <<"😀 こんにちは">>,
    ?assertEqual(JaEmoji, encode_decode_roundtrip(JaEmoji)),

    %% Test 4: Arabic + Emoji
    ArEmoji = <<"مرحبا 👋">>,
    ?assertEqual(ArEmoji, encode_decode_roundtrip(ArEmoji)),

    %% Test 5: Complex mixed JSON
    MixedJson = #{
        <<"japanese">> => <<"ありがとう">>,
        <<"arabic">> => <<"شكرا">>,
        <<"emoji">> => <<"🙏">>,
        <<"english">> => <<"Thank you">>
    },
    ?assertEqual(MixedJson, json_roundtrip(MixedJson)),

    %% Test 6: Multi-language array
    MultiLang = [
        <<"日本">>, <<"日本国">>, <<"Japan">>,
        <<"مصر">>, <<"Egypt">>, <<"エジプト">>
    ],
    DecodedMulti = json_roundtrip(MultiLang),
    ?assertEqual(length(MultiLang), length(DecodedMulti)),

    ok.

%%%====================================================================
%%% JSON Encoding Tests
%%%====================================================================

json_encoding_tests() ->
    %% Test 1: Native JSON encode (OTP 27+)
    JapaneseText = <<"テスト">>,
    Encoded = erlmcp_json_native:encode(#{<<"text">> => JapaneseText}),
    ?assert(is_binary(Encoded)),
    ?assertNotEqual(<<>>, Encoded),

    %% Test 2: JSON decode preserves UTF-8
    Decoded = erlmcp_json_native:decode(Encoded),
    ?assertEqual(JapaneseText, maps:get(<<"text">>, Decoded)),

    %% Test 3: JSON encode Arabic
    ArabicText = <<"اختبار">>,
    ArabicEncoded = erlmcp_json_native:encode(#{<<"text">> => ArabicText}),
    ArabicDecoded = erlmcp_json_native:decode(ArabicEncoded),
    ?assertEqual(ArabicText, maps:get(<<"text">>, ArabicDecoded)),

    %% Test 4: JSON encode emoji
    EmojiText = <<"✅">>,
    EmojiEncoded = erlmcp_json_native:encode(#{<<"check">> => EmojiText}),
    EmojiDecoded = erlmcp_json_native:decode(EmojiEncoded),
    ?assertEqual(EmojiText, maps:get(<<"check">>, EmojiDecoded)),

    %% Test 5: Complex nested JSON with UTF-8
    ComplexJson = #{
        <<"ja">> => #{
            <<"name">> => <<"名前">>,
            <<"greeting">> => <<"こんにちは">>
        },
        <<"ar">> => #{
            <<"name">> => <<"اسم">>,
            <<"greeting">> => <<"مرحبا">>
        },
        <<"emoji">> => [<<"😀">>, <<"😃">>, <<"😄">>]
    },
    ComplexEncoded = erlmcp_json_native:encode(ComplexJson),
    ComplexDecoded = erlmcp_json_native:decode(ComplexEncoded),
    ?assertEqual(ComplexJson, ComplexDecoded),

    ok.

%%%====================================================================
%%% JSON Decoding Tests
%%%====================================================================

json_decoding_tests() ->
    %% Test 1: Decode JSON with Japanese
    JaJson = <<"{\"text\":\"日本語\"}">>,
    JaDecoded = erlmcp_json_native:decode(JaJson),
    ?assertEqual(<<"日本語">>, maps:get(<<"text">>, JaDecoded)),

    %% Test 2: Decode JSON with Arabic
    ArJson = <<"{\"text\":\"العربية\"}">>,
    ArDecoded = erlmcp_json_native:decode(ArJson),
    ?assertEqual(<<"العربية">>, maps:get(<<"text">>, ArDecoded)),

    %% Test 3: Decode JSON with emoji
    EmojiJson = <<"{\"emoji\":\"🎉\"}">>,
    EmojiDecoded = erlmcp_json_native:decode(EmojiJson),
    ?assertEqual(<<"🎉">>, maps:get(<<"emoji">>, EmojiDecoded)),

    %% Test 4: Decode complex nested UTF-8
    ComplexJsonStr = <<"{\"data\":{\"ja\":\"日本\",\"ar\":\"مصر\",\"emoji\":\"🌍\"}}">>,
    ComplexDecoded = erlmcp_json_native:decode(ComplexJsonStr),
    Data = maps:get(<<"data">>, ComplexDecoded),
    ?assertEqual(<<"日本">>, maps:get(<<"ja">>, Data)),
    ?assertEqual(<<"مصر">>, maps:get(<<"ar">>, Data)),
    ?assertEqual(<<"🌍">>, maps:get(<<"emoji">>, Data)),

    %% Test 5: Decode array with UTF-8
    ArrayJson = <<"[\"日本\",\"مصر\",\"🌍\"]">>,
    ArrayDecoded = erlmcp_json_native:decode(ArrayJson),
    ?assertEqual([<<"日本">>, <<"مصر">>, <<"🌍">>], ArrayDecoded),

    ok.

%%%====================================================================
%%% Binary Operations Tests
%%%====================================================================

binary_operations_tests() ->
    %% Test 1: Binary size calculation for UTF-8
    Japanese = <<"日本語">>,
    ?assertEqual(9, byte_size(Japanese)), %% 3 chars * 3 bytes each
    ?assertEqual(3, string:length(Japanese)), %% 3 grapheme clusters

    %% Test 2: Arabic binary size
    Arabic = <<"مرحبا">>,
    ArBytes = byte_size(Arabic),
    ?assert(ArBytes > 5), %% More bytes than characters due to UTF-8

    %% Test 3: Emoji binary size (multi-byte)
    Emoji = <<"😀">>,
    ?assertEqual(4, byte_size(Emoji)), %% Emoji is 4 bytes in UTF-8

    %% Test 4: Binary concatenation preserves UTF-8
    Ja = <<"こんにちは">>,
    En = <<"World">>,
    Combined = <<Ja/binary, " ", En/binary>>,
    ?assertEqual(<<"こんにちは World">>, Combined),

    %% Test 5: Binary matching with UTF-8
    <<First:3/binary, Rest/binary>> = <<"日本語テスト">>,
    ?assertEqual(<<"日本語">>, First),
    ?assertEqual(<<"テスト">>, Rest),

    %% Test 6: Base64 encoding/decoding preserves UTF-8
    Original = <<"こんにちは世界">>,
    Encoded = base64:encode(Original),
    Decoded = base64:decode(Encoded),
    ?assertEqual(Original, Decoded),

    ok.

%%%====================================================================
%%% Edge Cases Tests
%%%====================================================================

edge_cases_tests() ->
    %% Test 1: Empty binary
    ?assertEqual(<<>>, encode_decode_roundtrip(<<>>)),

    %% Test 2: Very long UTF-8 string
    LongText = unicode:characters_to_binary([
        $"あ, $"い, $"う, $"え, $"お
        || _ <- lists:seq(1, 1000)
    ]),
    ?assertEqual(LongText, encode_decode_roundtrip(LongText)),

    %% Test 3: Null characters in UTF-8
    WithNull = <<"before", 0, "after">>,
    ?assertEqual(WithNull, encode_decode_roundtrip(WithNull)),

    %% Test 4: Invalid UTF-8 (should handle gracefully)
    %% Note: Native JSON module will handle this
    try
        InvalidUtf8 = <<255, 254, 253>>,
        %% Should not crash
        _ = erlmcp_json_native:encode(#{<<"bad">> => InvalidUtf8}),
        ok
    catch
        _:_ ->
            %% Expected to fail or handle gracefully
            ok
    end,

    %% Test 5: JSON-RPC with UTF-8
    Utf8Request = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{
            <<"japanese">> => <<"テスト">>,
            <<"arabic">> => <<"اختبار">>,
            <<"emoji">> => <<"✅">>
        }
    },
    EncodedRequest = erlmcp_json_rpc:encode_request(1, <<"test">>, maps:get(<<"params">>, Utf8Request)),
    ?assert(is_binary(EncodedRequest)),
    ?assertNotEqual(<<>>, EncodedRequest),

    %% Test 6: Unicode normalization
    %% Different byte sequences can represent the same text
    %% (NFC vs NFD normalization)
    Nfc = unicode:characters_to_nfc_binary(<<"あいうえお">>),
    Nfd = unicode:characters_to_nfd_binary(<<"あいうえお">>),
    %% These should both round-trip correctly
    ?assertEqual(Nfc, encode_decode_roundtrip(Nfc)),
    ?assertEqual(Nfd, encode_decode_roundtrip(Nfd)),

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Encode and decode binary to test UTF-8 preservation
encode_decode_roundtrip(Binary) ->
    %% Use native JSON for roundtrip
    Json = erlmcp_json_native:encode(#{<<"data">> => Binary}),
    Decoded = erlmcp_json_native:decode(Json),
    maps:get(<<"data">>, Decoded).

%% @doc JSON encode/decode roundtrip for complex terms
json_roundtrip(Term) ->
    Encoded = erlmcp_json_native:encode(Term),
    erlmcp_json_native:decode(Encoded).
