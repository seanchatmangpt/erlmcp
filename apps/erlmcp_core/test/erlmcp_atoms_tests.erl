-module(erlmcp_atoms_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%% Test OTP 28 atom size limit changes
%% OTP 28: 255 characters (not bytes)
%% OTP < 28: 255 bytes

%%%===================================================================
%%% Test Data
%%%===================================================================

%% International test strings (multibyte UTF-8)
-define(JAPANESE_SHORT, <<"ツール">>).           % 9 bytes, 3 chars
-define(JAPANESE_LONG, <<"ツール名前確認">>).    % 21 bytes, 7 chars
-define(ARABIC_SHORT, <<"أداة">>).              % 8 bytes, 3 chars
-define(ARABIC_LONG, <<"الأداة_الكبيرة">>).    % 23 bytes, 11 chars
-define(EMOJI_SHORT, <<"🔧">>).                 % 4 bytes, 1 char (2 UTF-16 code units)
-define(EMOJI_LONG, <<"🔧_tool_🚀">>).          % 14 bytes, 8 chars
-define(MIXED, <<"tool_日本語_🔧_الأداة">>).  % 30 bytes, 14 chars

%% Very long strings (for limit testing)
-define(MAX_255_CHARS, list_to_binary(lists:duplicate(255, $a))).
-define(MAX_255_EMOJI, list_to_binary(lists:duplicate(255, "🔧"))). % 1020 bytes!

%%%===================================================================
%%% Length Limit Tests
%%%===================================================================

binary_to_atom_safe_byte_limit_test() ->
    % Test 255 byte limit (OTP < 27 compatibility)
    Valid255 = <<0:255/unit:8>>, % 255 bytes
    {error, _} = erlmcp_atoms:binary_to_atom_safe(Valid255),
    ok.

char_length_check_test() ->
    % Test character length checking (OTP 28)
    ?assertEqual(ok, erlmcp_atoms:char_length_check(<<"test">>)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JAPANESE_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?ARABIC_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?EMOJI_SHORT)),
    ok.

char_length_check_emoji_test() ->
    % Emoji: 1 character but 3-4 bytes
    ?assertEqual(ok, erlmcp_atoms:char_length_check(<<"🔧">>)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(<<"a🔧b">>)), % 3 chars
    ok.

tool_name_to_atom_short_test() ->
    % Test short tool names (all should succeed)
    ?assertEqual('test', erlmcp_atoms:tool_name_to_atom(<<"test">>)),
    ?assertEqual('ツール', erlmcp_atoms:tool_name_to_atom(?JAPANESE_SHORT)),
    ?assertEqual('أداة', erlmcp_atoms:tool_name_to_atom(?ARABIC_SHORT)),
    ?assertEqual('🔧', erlmcp_atoms:tool_name_to_atom(?EMOJI_SHORT)),
    ok.

tool_name_to_atom_mixed_test() ->
    % Test mixed language tool names
    Atom = erlmcp_atoms:tool_name_to_atom(?MIXED),
    ?assert(is_atom(Atom)),
    ?assert(erlmcp_atoms:is_safe_atom(Atom)),
    ok.

tool_name_to_atom_too_long_test() ->
    % Test tool name exceeding character limit
    % Create a 256-character string
    TooLong = list_to_binary(lists:duplicate(256, $a)),
    ?assertError(tool_name_too_long, erlmcp_atoms:tool_name_to_atom(TooLong)),
    ok.

%%%===================================================================
%%% International Character Tests
%%%===================================================================

japanese_atom_test() ->
    % Japanese characters (3 bytes per char)
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JAPANESE_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JAPANESE_LONG)),

    % Should convert successfully
    Atom = erlmcp_atoms:tool_name_to_atom(?JAPANESE_LONG),
    ?assert(is_atom(Atom)),
    ?assert(erlmcp_atoms:is_safe_atom(Atom)),
    ok.

arabic_atom_test() ->
    % Arabic characters (variable length UTF-8)
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?ARABIC_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?ARABIC_LONG)),

    Atom = erlmcp_atoms:tool_name_to_atom(?ARABIC_LONG),
    ?assert(is_atom(Atom)),
    ok.

emoji_atom_test() ->
    % Emoji (multibyte characters)
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?EMOJI_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?EMOJI_LONG)),

    Atom = erlmcp_atoms:tool_name_to_atom(?EMOJI_LONG),
    ?assert(is_atom(Atom)),
    ok.

mixed_language_atom_test() ->
    % Mix of ASCII, Japanese, emoji, Arabic
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?MIXED)),

    Atom = erlmcp_atoms:tool_name_to_atom(?MIXED),
    ?assert(is_atom(Atom)),
    ?assert(erlmcp_atoms:is_safe_atom(Atom)),
    ok.

%%%===================================================================
%%% Safety Tests
%%%===================================================================

empty_binary_test() ->
    % Empty binary should return error
    ?assertEqual({error, empty}, erlmcp_atoms:binary_to_atom_safe(<<>>)),
    ?assertEqual({error, empty}, erlmcp_atoms:char_length_check(<<>>)),
    ?assertEqual({error, empty}, erlmcp_atoms:validate_binary(<<>>)),
    ok.

invalid_utf8_test() ->
    % Invalid UTF-8 sequences
    InvalidUTF8 = <<255, 254, 253>>, % Invalid UTF-8 bytes
    Result = erlmcp_atoms:binary_to_atom_safe(InvalidUTF8),
    ?assertMatch({error, invalid_utf8}, Result),
    ok.

too_long_binary_test() ->
    % Binary exceeding 255 bytes
    TooLong = <<0:256/unit:8>>,
    ?assertEqual({error, too_long}, erlmcp_atoms:binary_to_atom_safe(TooLong)),
    ?assertEqual({error, too_long}, erlmcp_atoms:validate_binary(TooLong)),
    ok.

invalid_binary_test() ->
    % Non-binary input
    ?assertEqual({error, invalid_binary}, erlmcp_atoms:binary_to_atom_safe(123)),
    ?assertEqual({error, invalid_binary}, erlmcp_atoms:binary_to_atom_safe(atom)),
    ?assertEqual({error, invalid_binary}, erlmcp_atoms:binary_to_atom_safe([])),
    ok.

%%%===================================================================
%%% Existing Atom Tests
%%%===================================================================

existing_atom_found_test() ->
    % Test finding existing atom
    Bin = <<"test_existing">>,
    % First create the atom
    _ = erlmcp_atoms:tool_name_to_atom(Bin),

    % Now look it up
    ?assertEqual('test_existing', erlmcp_atoms:existing_atom(Bin)),
    ok.

existing_atom_not_found_test() ->
    % Test atom that doesn't exist
    ?assertEqual({error, not_found}, erlmcp_atoms:existing_atom(<<"nonexistent_atom">>)),
    ok.

atom_reuse_test() ->
    % Test that binary_to_existing_atom prevents atom leak
    Bin = <<"reuse_test">>,

    % First call creates the atom
    Atom1 = erlmcp_atoms:binary_to_atom_safe(Bin),
    ?assert(is_atom(Atom1)),

    % Second call reuses the same atom
    Atom2 = erlmcp_atoms:binary_to_atom_safe(Bin),
    ?assertEqual(Atom1, Atom2),

    % Both should be the same atom
    ?assertEqual(Atom1, Atom2),
    ok.

%%%===================================================================
%%% Namespaced Atom Tests
%%%===================================================================

namespaced_atom_test() ->
    % Test namespaced atom creation
    ?assertEqual('mcp$tool', erlmcp_atoms:namespaced_atom(<<"mcp">>, <<"tool">>)),
    ?assertEqual('server$main', erlmcp_atoms:namespaced_atom(<<"server">>, <<"main">>)),
    ok.

namespaced_atom_international_test() ->
    % Test namespaced atoms with international characters
    ?assertMatch({_, _}, erlmcp_atoms:namespaced_atom(<<"mcp">>, ?JAPANESE_SHORT)),
    ?assertMatch({_, _}, erlmcp_atoms:namespaced_atom(<<"mcp">>, ?ARABIC_SHORT)),
    ?assertMatch({_, _}, erlmcp_atoms:namespaced_atom(<<"mcp">>, ?EMOJI_SHORT)),
    ok.

namespaced_atom_invalid_test() ->
    % Test invalid inputs
    ?assertEqual({error, invalid_binary}, erlmcp_atoms:namespaced_atom(123, <<"tool">>)),
    ?assertEqual({error, invalid_binary}, erlmcp_atoms:namespaced_atom(<<"ns">>, 123)),
    ok.

%%%===================================================================
%%% Safe Atom Check Tests
%%%===================================================================

is_safe_atom_reserved_test() ->
    % Test reserved/unsafe atoms
    ?assertNot(erlmcp_atoms:is_safe_atom(undefined)),
    ?assertNot(erlmcp_atoms:is_safe_atom(true)),
    ?assertNot(erlmcp_atoms:is_safe_atom(false)),
    ?assertNot(erlmcp_atoms:is_safe_atom(ok)),
    ?assertNot(erlmcp_atoms:is_safe_atom(error)),
    ?assertNot(erlmcp_atoms:is_safe_atom(badarg)),
    ?assertNot(erlmcp_atoms:is_safe_atom(badarith)),
    ok.

is_safe_atom_user_test() ->
    % Test user-defined atoms (should be safe)
    ?assert(erlmcp_atoms:is_safe_atom('my_tool')),
    ?assert(erlmcp_atoms:is_safe_atom('server1')),
    ?assert(erlmcp_atoms:is_safe_atom('ツール')), % Japanese
    ?assert(erlmcp_atoms:is_safe_atom('أداة')), % Arabic
    ok.

%%%===================================================================
%%% OTP Version Tests
%%%===================================================================

get_atom_limit_test() ->
    % Test atom limit detection
    Limit = erlmcp_atoms:get_atom_limit(),
    ?assert(is_integer(Limit)),
    ?assert(Limit >= 255),
    ok.

%%%===================================================================
%%% Resource Name Tests
%%%===================================================================

resource_name_to_atom_test() ->
    % Test resource name conversion
    ?assertEqual({ok, 'resource1'}, erlmcp_atoms:resource_name_to_atom(<<"resource1">>)),
    ?assertEqual({ok, 'file_resource'}, erlmcp_atoms:resource_name_to_atom(<<"file_resource">>)),
    ok.

resource_name_uri_test() ->
    % Test resource URI conversion (may contain slashes)
    URI = <<"file:///path/to/resource">>,
    ?assertMatch({ok, _}, erlmcp_atoms:resource_name_to_atom(URI)),
    ok.

%%%===================================================================
%%% Encoding Tests
%%%===================================================================

encoding_utf8_test() ->
    % Test UTF-8 encoding
    ?assertEqual(ok, erlmcp_atoms:validate_binary(<<"test">>)),
    ?assertEqual(ok, erlmcp_atoms:validate_binary(?JAPANESE_SHORT)),
    ?assertEqual(ok, erlmcp_atoms:validate_binary(?EMOJI_SHORT)),
    ok.

encoding_latin1_test() ->
    % Test Latin1 encoding (for compatibility)
    Latin1Bin = <<"test">>,
    ?assertMatch(_, erlmcp_atoms:binary_to_atom_safe(Latin1Bin, latin1)),
    ok.

%%%===================================================================
%%% Backward Compatibility Tests
%%%===================================================================

backward_compatibility_otp27_test() ->
    % Test OTP 27 behavior (byte-based limits)
    ShortBin = <<"short">>,
    ?assertMatch(_, erlmcp_atoms:binary_to_atom_safe(ShortBin)),
    ok.

%%%===================================================================
%%% Edge Cases
%%%===================================================================

edge_case_zero_bytes_test() ->
    % Test zero-byte sequences
    ZeroBin = <<0>>,
    ?assertEqual({error, invalid_utf8}, erlmcp_atoms:validate_binary(ZeroBin)),
    ok.

edge_case_max_boundary_test() ->
    % Test exactly at the boundary
    % 255 bytes, all ASCII
    Max255 = <<0:255/unit:8>>,
    ?assertEqual({error, too_long}, erlmcp_atoms:binary_to_atom_safe(Max255)),

    % 254 bytes, all ASCII (should work)
    Max254 = <<0:254/unit:8>>,
    ?assertMatch({error, _}, erlmcp_atoms:binary_to_atom_safe(Max254)),
    ok.
