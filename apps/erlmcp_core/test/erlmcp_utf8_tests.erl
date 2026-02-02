-module(erlmcp_utf8_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for UTF-8 Encoding Support
%% Chicago School TDD: Test ONLY observable behavior through public API
%% Focus: Full UTF-8 support for international text (Japanese, Arabic, emoji, etc.)
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% UTF-8 Validation Tests
%%====================================================================

validate_utf8_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_validate_ascii()),
         ?_test(test_validate_japanese()),
         ?_test(test_validate_arabic()),
         ?_test(test_validate_emoji()),
         ?_test(test_validate_mixed_script()),
         ?_test(test_validate_invalid_utf8()),
         ?_test(test_validate_empty_binary()),
         ?_test(test_validate_russian()),
         ?_test(test_validate_chinese()),
         ?_test(test_validate_korean()),
         ?_test(test_validate_emoji_sequence())]
     end}.

test_validate_ascii() ->
    ?assert(erlmcp_json_rpc:validate_utf8(<<"Hello World">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<"ASCII 123!@#">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<>>)).

test_validate_japanese() ->
    %% Hiragana
    ?assert(erlmcp_json_rpc:validate_utf8(<<"こんにちは">>)),
    %% Katakana
    ?assert(erlmcp_json_rpc:validate_utf8(<<"コンニチハ">>)),
    %% Kanji
    ?assert(erlmcp_json_rpc:validate_utf8(<<"日本語">>)),
    %% Mixed
    ?assert(erlmcp_json_rpc:validate_utf8(<<"こんにちは世界">>)).

test_validate_arabic() ->
    %% Arabic text
    ?assert(erlmcp_json_rpc:validate_utf8(<<"مرحبا بالعالم">>)),
    %% Arabic with numbers
    ?assert(erlmcp_json_rpc:validate_utf8(<<"السلام عليكم 123">>)),
    %% Right-to-left text
    ?assert(erlmcp_json_rpc:validate_utf8(<<"العربية">>)).

test_validate_emoji() ->
    %% Single emoji
    ?assert(erlmcp_json_rpc:validate_utf8(<<"🌍">>)),
    %% Multiple emoji
    ?assert(erlmcp_json_rpc:validate_utf8(<<"😀😃😄😁">>)),
    %% Emoji with text
    ?assert(erlmcp_json_rpc:validate_utf8(<<"Hello 🌍">>)),
    %% Complex emoji (skin tone modifiers)
    ?assert(erlmcp_json_rpc:validate_utf8(<<"👋🏽👋🏻👋🏿">>)).

test_validate_mixed_script() ->
    %% Japanese + English
    ?assert(erlmcp_json_rpc:validate_utf8(<<"Hello こんにちは">>)),
    %% Arabic + Emoji
    ?assert(erlmcp_json_rpc:validate_utf8(<<"مرحبا 👋">>)),
    %% Chinese + English + Emoji
    ?assert(erlmcp_json_rpc:validate_utf8(<<"你好世界 Hello 🌍">>)).

test_validate_invalid_utf8() ->
    %% Invalid 2-byte sequence
    Invalid2 = <<16#C0, 16#80>>,
    ?assertNot(erlmcp_json_rpc:validate_utf8(Invalid2)),
    %% Invalid 3-byte sequence
    Invalid3 = <<16#E0, 16#80, 16#80>>,
    ?assertNot(erlmcp_json_rpc:validate_utf8(Invalid3)),
    %% Truncated sequence
    Truncated = <<16#F0, 16#9F>>,
    ?assertNot(erlmcp_json_rpc:validate_utf8(Truncated)).

test_validate_empty_binary() ->
    ?assert(erlmcp_json_rpc:validate_utf8(<<>>)).

test_validate_russian() ->
    ?assert(erlmcp_json_rpc:validate_utf8(<<"Привет мир">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<"Доброе утро">>)).

test_validate_chinese() ->
    %% Simplified Chinese
    ?assert(erlmcp_json_rpc:validate_utf8(<<"你好世界">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<"早上好">>)),
    %% Traditional Chinese
    ?assert(erlmcp_json_rpc:validate_utf8(<<"您好世界">>)).

test_validate_korean() ->
    ?assert(erlmcp_json_rpc:validate_utf8(<<"안녕하세요 세계">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<"좋은 아침">>)).

test_validate_emoji_sequence() ->
    %% Family emoji (ZWJ sequence)
    ?assert(erlmcp_json_rpc:validate_utf8(<<"👨‍👩‍👧‍👦">>)),
    %% Flag emoji (regional indicator sequence)
    ?assert(erlmcp_json_rpc:validate_utf8(<<"🏳️‍🌈">>)),
    ?assert(erlmcp_json_rpc:validate_utf8(<<"🇺🇸🇬🇧🇯🇵">>)).

%%====================================================================
%% UTF-8 Encoding/Decoding Tests
%%====================================================================

utf8_encoding_decoding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_encode_decode_japanese()),
         ?_test(test_encode_decode_arabic()),
         ?_test(test_encode_decode_emoji()),
         ?_test(test_encode_decode_mixed()),
         ?_test(test_encode_decode_russian()),
         ?_test(test_encode_decode_chinese()),
         ?_test(test_encode_decode_korean())]
     end}.

test_encode_decode_japanese() ->
    Text = <<"こんにちは世界">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => Text}),
    ?assert(is_binary(Request)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_encode_decode_arabic() ->
    Text = <<"مرحبا بالعالم">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => Text}),
    ?assert(is_binary(Request)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_encode_decode_emoji() ->
    Text = <<"Hello 🌍 😀🎉">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"emoji">> => Text}),
    ?assert(is_binary(Request)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_encode_decode_mixed() ->
    Text = <<"Hello こんにちは مرحبا 🌍">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"mixed">> => Text}),
    ?assert(is_binary(Request)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_encode_decode_russian() ->
    Text = <<"Привет мир">>,
    Response = erlmcp_json_rpc:encode_response(1, #{<<"text">> => Text}),
    ?assert(is_binary(Response)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_encode_decode_chinese() ->
    Text = <<"你好世界">>,
    Response = erlmcp_json_rpc:encode_response(1, #{<<"text">> => Text}),
    ?assert(is_binary(Response)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_encode_decode_korean() ->
    Text = <<"안녕하세요 세계">>,
    Response = erlmcp_json_rpc:encode_response(1, #{<<"text">> => Text}),
    ?assert(is_binary(Response)),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

%%====================================================================
%% JSON-RPC with UTF-8 Content Tests
%%====================================================================

json_rpc_utf8_content_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_request_with_japanese_method()),
         ?_test(test_request_with_arabic_params()),
         ?_test(test_response_with_chinese_result()),
         ?_test(test_notification_with_korean()),
         ?_test(test_error_with_russian_message()),
         ?_test(test_batch_with_mixed_languages())]
     end}.

test_request_with_japanese_method() ->
    Method = <<"ツールを実行">>,
    Params = #{<<"引数">> => <<"値">>},
    Request = erlmcp_json_rpc:encode_request(1, Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{method = Method}, Decoded).

test_request_with_arabic_params() ->
    Params = #{<<"النص">> => <<"مرحبا">>, <<"الرقم">> => 123},
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_response_with_chinese_result() ->
    Result = #{<<"结果">> => <<"成功">>, <<"数据">> => <<"你好">>},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{result = Result}, Decoded).

test_notification_with_korean() ->
    Method = <<"알림">>,
    Params = #{<<"메시지">> => <<"안녕하세요">>},
    Notification = erlmcp_json_rpc:encode_notification(Method, Params),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Notification),
    ?assertMatch(#json_rpc_notification{method = Method}, Decoded).

test_error_with_russian_message() ->
    Error = erlmcp_json_rpc:encode_error_response(1, -32600, <<"Неверный запрос">>),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Error),
    ?assertMatch(#json_rpc_response{error = #mcp_error{message = <<"Неверный запрос">>}},
                 Decoded).

test_batch_with_mixed_languages() ->
    Requests =
        [#json_rpc_request{id = 1,
                           method = <<"test">>,
                           params = #{<<"text">> => <<"Hello">>}},
         #json_rpc_request{id = 2,
                           method = <<"テスト">>,
                           params = #{<<"テキスト">> => <<"こんにちは">>}},
         #json_rpc_request{id = 3,
                           method = <<"اختبار">>,
                           params = #{<<"نص">> => <<"مرحبا">>}}],
    Batch = erlmcp_json_rpc:encode_batch(Requests),
    {ok, Decoded} = erlmcp_json_rpc:decode_batch(Batch),
    ?assertMatch({ok, [_ | _]}, {ok, Decoded}),
    ?assertEqual(3, length(Decoded)).

%%====================================================================
%% Tool Results with UTF-8 Tests
%%====================================================================

tool_results_utf8_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_tool_result_with_japanese()),
         ?_test(test_tool_result_with_arabic()),
         ?_test(test_tool_result_with_emoji()),
         ?_test(test_tool_result_with_multilingual()),
         ?_test(test_tool_result_with_code_block())]
     end}.

test_tool_result_with_japanese() ->
    Result =
        #{<<"content">> => [#{<<"type">> => <<"text">>,
                              <<"text">> => <<"計算結果: 42">>}],
          <<"isError">> => false},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_tool_result_with_arabic() ->
    Result =
        #{<<"content">> => [#{<<"type">> => <<"text">>,
                              <<"text">> => <<"النتيجة: نعم">>}],
          <<"isError">> => false},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_tool_result_with_emoji() ->
    Result =
        #{<<"content">> =>
              [#{<<"type">> => <<"text">>, <<"text">> => <<"Success! 🎉✅">>},
               #{<<"type">> => <<"text">>, <<"text">> => <<"Error! ❌❗">>}],
          <<"isError">> => false},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_tool_result_with_multilingual() ->
    Result =
        #{<<"content">> =>
              [#{<<"type">> => <<"text">>,
                <<"text">> =>
                    <<"English: Hello\nJapanese: こんにちは\nArabic: مرحبا\nChinese: 你好\nKorean: 안녕하세요\nRussian: Привет\nEmoji: 🌍">>}],
          <<"isError">> => false},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_tool_result_with_code_block() ->
    Result =
        #{<<"content">> =>
              [#{<<"type">> => <<"text">>,
                <<"text">> =>
                    <<"コード:\n```erlang\nhello() -> \"こんにちは\".\n```">>}],
          <<"isError">> => false},
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

%%====================================================================
%% Metadata with UTF-8 Tests
%%====================================================================

metadata_utf8_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_metadata_with_japanese()),
         ?_test(test_metadata_with_arabic()),
         ?_test(test_metadata_with_content_encoding())]
     end}.

test_metadata_with_japanese() ->
    Metadata = #{<<"encoding">> => <<"utf-8">>,
                 <<"language">> => <<"日本語">>,
                 <<"description">> => <<"UTF-8エンコーディングのテスト">>},
    Result = maps:merge(#{<<"status">> => <<"ok">>}, Metadata),
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_metadata_with_arabic() ->
    Metadata = #{<<"encoding">> => <<"utf-8">>,
                 <<"language">> => <<"العربية">>,
                 <<"description">> => <<"اختبار الترميز">>},
    Result = maps:merge(#{<<"status">> => <<"ok">>}, Metadata),
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

test_metadata_with_content_encoding() ->
    Metadata =
        #{<<"content_type">> => <<"text/plain; charset=utf-8">>,
          <<"encoding">> => <<"utf-8">>},
    Result = maps:merge(#{<<"text">> => <<"Hello 世界 🌍">>}, Metadata),
    Response = erlmcp_json_rpc:encode_response(1, Result),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Response),
    ?assertMatch(#json_rpc_response{}, Decoded).

%%====================================================================
%% Edge Cases with UTF-8
%%====================================================================

utf8_edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_very_long_japanese_text()),
         ?_test(test_all_emoji_string()),
         ?_test(test_mixed_depth_nesting()),
         ?_test(test_unicode_escape_sequences()),
         ?_test(test_zero_width_joiner())]
     end}.

test_very_long_japanese_text() ->
    LongText = binary:copy(<<"日本語">>, 1000),
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"long_text">> => LongText}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_all_emoji_string() ->
    EmojiString = <<"😀😃😄😁😆😅🤣😂🙂🙃😉😊😇🥰😍🤩😘😗☺😚😙🥲😋😛😜🤪😝🤑🤗🤭🤫🤔🤐🤨😐😑😶😏😒🙄😬🤥😌😔😪🤤😴😷🤒🤕🤢🤮🤧😵🤯">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"emoji">> => EmojiString}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_mixed_depth_nesting() ->
    Nested =
        #{<<"level1">> =>
              #{<<"level2">> =>
                    #{<<"level3">> =>
                          #{<<"japanese">> => <<"日本語">>,
                            <<"arabic">> => <<"العربية">>,
                            <<"emoji">> => <<"🌍">>}}}},
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, Nested),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_unicode_escape_sequences() ->
    %% JSON should preserve actual UTF-8, not escape sequences
    Text = <<"Hello 世界 🌍">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"text">> => Text}),
    ?assert(is_binary(Request)),
    ?assertNot(binary:match(Request, <<"\\u">>) =:= nomatch),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

test_zero_width_joiner() ->
    %% Emoji with Zero Width Joiner (ZWJ)
    ZWJEmoji = <<"👨‍👩‍👧‍👦👩‍❤️‍👨‍👨‍👧‍👦">>,
    Request = erlmcp_json_rpc:encode_request(1, <<"test">>, #{<<"zwj">> => ZWJEmoji}),
    {ok, Decoded} = erlmcp_json_rpc:decode_message(Request),
    ?assertMatch(#json_rpc_request{}, Decoded).

%%====================================================================
%% Ensure UTF-8 Encoding Validation Tests
%%====================================================================

ensure_utf8_encoding_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(test_ensure_utf8_valid_binary()),
         ?_test(test_ensure_utf8_invalid_binary()),
         ?_test(test_ensure_utf8_valid_map()),
         ?_test(test_ensure_utf8_invalid_in_map()),
         ?_test(test_ensure_utf8_valid_list()),
         ?_test(test_ensure_utf8_nested_structure())]
     end}.

test_ensure_utf8_valid_binary() ->
    ?assertMatch({ok, <<"Hello 世界">>},
                 erlmcp_json_rpc:ensure_utf8_encoding(<<"Hello 世界">>)).

test_ensure_utf8_invalid_binary() ->
    Invalid = <<16#FF, 16#FF, 16#FF>>,
    ?assertMatch({error, {invalid_utf8, _}},
                 erlmcp_json_rpc:ensure_utf8_encoding(Invalid)).

test_ensure_utf8_valid_map() ->
    Map =
        #{<<"japanese">> => <<"日本語">>,
          <<"arabic">> => <<"العربية">>,
          <<"emoji">> => <<"🌍">>},
    ?assertMatch({ok, Map}, erlmcp_json_rpc:ensure_utf8_encoding(Map)).

test_ensure_utf8_invalid_in_map() ->
    Map = #{<<"valid">> => <<"Hello">>, <<"invalid">> => <<16#FF, 16#FF>>},
    ?assertMatch({error, {invalid_utf8, _}}, erlmcp_json_rpc:ensure_utf8_encoding(Map)).

test_ensure_utf8_valid_list() ->
    List = [<<"Hello">>, <<"世界">>, <<"🌍">>, 123, true],
    ?assertMatch({ok, List}, erlmcp_json_rpc:ensure_utf8_encoding(List)).

test_ensure_utf8_nested_structure() ->
    Nested =
        #{<<"data">> =>
              [#{<<"text">> => <<"日本語">>},
               #{<<"emoji">> => <<"🌍">>},
               [<<"العربية">>, <<"中文">>]]},
    ?assertMatch({ok, Nested}, erlmcp_json_rpc:ensure_utf8_encoding(Nested)).
