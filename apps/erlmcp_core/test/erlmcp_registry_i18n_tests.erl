-module(erlmcp_registry_i18n_tests).
-author('erlmcp').

%% Internationalization tests for erlmcp_registry with OTP 28 UTF-8 support
%% Tests Japanese, Arabic, Korean, Hebrew, emoji, and mixed-language names

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Data: International Tool Names
%%%====================================================================

%% Japanese tool names (Hiragana, Katakana, Kanji)
-define(JAPANESE_TOOL_NAME, <<"ツール名">>).
-define(JAPANESE_TOOL_FULL, <<"日本語ツール">>).
-define(JAPANESE_MIXED, <<"tool_ツール_名">>).

%% Arabic tool names
-define(ARABIC_TOOL_NAME, <<"الأداة">>).
-define(ARABIC_TOOL_FULL, <<"أداة_البرمجيات">>).
-define(ARABIC_MIXED, <<"tool_الأداة">>).

%% Korean tool names
-define(KOREAN_TOOL_NAME, <<"도구">>).
-define(KOREAN_TOOL_FULL, <<"한국어_도구">>).
-define(KOREAN_MIXED, <<"korean_도구_tool">>).

%% Hebrew tool names
-define(HEBREW_TOOL_NAME, <<"כלי">>).
-define(HEBREW_TOOL_FULL, <<"כלי_עברית">>).
-define(HEBREW_MIXED, <<"tool_כלי">>).

%% Emoji tool names
-define(EMOJI_TOOL_SIMPLE, <<"🔧">>).
-define(EMOJI_TOOL_FULL, <<"🔧_tool_🚀">>).
-define(EMOJI_TOOL_COMPLEX, <<"🎨_artist_🎭_studio_🎪">>).

%% Cyrillic tool names (Russian)
-define(CYRILLIC_TOOL_NAME, <<"инструмент">>).
-define(CYRILLIC_TOOL_FULL, <<"русский_инструмент">>).

%% Chinese tool names
-define(CHINESE_TOOL_NAME, <<"工具">>).
-define(CHINESE_TOOL_FULL, <<"中文_工具">>).

%% Mixed language combinations
-define(MIXED_JP_AR, <<"ツール_الأداة">>).
-define(MIXED_EMOJI_ALL, <<"🔧_ツール_الأداة_도구_כלי">>).

%% Invalid names for testing
-define(INVALID_NULL, <<"tool\0name">>).
-define(INVALID_CONTROL, <<"tool\nname">>).
-define(TOO_LONG_256_CHARS, <<"abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxzy">>).

%%%====================================================================
%%% Registry Validation Tests
%%%====================================================================

validate_japanese_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?JAPANESE_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?JAPANESE_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?JAPANESE_MIXED)).

validate_arabic_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?ARABIC_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?ARABIC_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?ARABIC_MIXED)).

validate_korean_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?KOREAN_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?KOREAN_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?KOREAN_MIXED)).

validate_hebrew_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?HEBREW_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?HEBREW_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?HEBREW_MIXED)).

validate_emoji_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?EMOJI_TOOL_SIMPLE)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?EMOJI_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?EMOJI_TOOL_COMPLEX)).

validate_cyrillic_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?CYRILLIC_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?CYRILLIC_TOOL_FULL)).

validate_chinese_tool_name_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?CHINESE_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?CHINESE_TOOL_FULL)).

validate_mixed_language_names_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?MIXED_JP_AR)),
    ?assertEqual(ok, erlmcp_registry:validate_tool_name(?MIXED_EMOJI_ALL)).

validate_invalid_names_test() ->
    ?assertEqual({error, invalid_characters}, erlmcp_registry:validate_tool_name(?INVALID_NULL)),
    ?assertEqual({error, too_long}, erlmcp_registry:validate_tool_name(?TOO_LONG_256_CHARS)).

%%%====================================================================
%%% Name Normalization Tests
%%%====================================================================

normalize_japanese_name_test() ->
    Atom = erlmcp_registry:normalize_name(?JAPANESE_TOOL_NAME),
    ?assert(is_atom(Atom)),
    ?assertEqual(<<"ツール名">>, atom_to_binary(Atom, utf8)).

normalize_arabic_name_test() ->
    Atom = erlmcp_registry:normalize_name(?ARABIC_TOOL_NAME),
    ?assert(is_atom(Atom)),
    ?assertEqual(<<"الأداة">>, atom_to_binary(Atom, utf8)).

normalize_emoji_name_test() ->
    Atom = erlmcp_registry:normalize_name(?EMOJI_TOOL_FULL),
    ?assert(is_atom(Atom)),
    ?assertEqual(<<"🔧_tool_🚀">>, atom_to_binary(Atom, utf8)).

normalize_mixed_language_test() ->
    Atom = erlmcp_registry:normalize_name(?MIXED_JP_AR),
    ?assert(is_atom(Atom)),
    ?assertEqual(<<"ツール_الأداة">>, atom_to_binary(Atom, utf8)).

%%%====================================================================
;;; Character Length Validation Tests (OTP 28)
%%%====================================================================

char_length_japanese_test() ->
    %% Japanese: 3 characters (ツール名) but 9 bytes in UTF-8
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JAPANESE_TOOL_NAME)),
    ?assertEqual(3, string:length(?JAPANESE_TOOL_NAME)),
    ?assertEqual(9, byte_size(?JAPANESE_TOOL_NAME)).

char_length_arabic_test() ->
    %% Arabic: 5 characters (الأداة) but 10 bytes
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?ARABIC_TOOL_NAME)),
    ?assertEqual(5, string:length(?ARABIC_TOOL_NAME)),
    ?assertEqual(10, byte_size(?ARABIC_TOOL_NAME)).

char_length_emoji_test() ->
    %% Emoji: 10 characters (🔧_tool_🚀) but 14 bytes
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?EMOJI_TOOL_FULL)),
    ?assertEqual(10, string:length(?EMOJI_TOOL_FULL)),
    ?assertEqual(14, byte_size(?EMOJI_TOOL_FULL)).

char_length_255_limit_test() ->
    %% Exactly 255 characters should pass
    Name255 = binary:copy(<<"a">>, 255),
    ?assertEqual(ok, erlmcp_atoms:char_length_check(Name255)),
    ?assertEqual(255, string:length(Name255)).

char_length_256_limit_test() ->
    %% 256 characters should fail
    Name256 = binary:copy(<<"a">>, 256),
    ?assertEqual({error, too_long}, erlmcp_atoms:char_length_check(Name256)).

%%%====================================================================
%%% Registry Integration Tests
%%%====================================================================

registry_register_japanese_tool_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),
    ToolName = ?JAPANESE_TOOL_NAME,
    ServerPid = self(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    ?assertEqual(ok, erlmcp_registry:register_server(ToolName, ServerPid, Config)),

    %% Verify we can find the server
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ToolName)),

    gen_server:stop(Pid).

registry_register_arabic_tool_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),
    ToolName = ?ARABIC_TOOL_NAME,
    ServerPid = self(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    ?assertEqual(ok, erlmcp_registry:register_server(ToolName, ServerPid, Config)),
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ToolName)),

    gen_server:stop(Pid).

registry_register_emoji_tool_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),
    ToolName = ?EMOJI_TOOL_FULL,
    ServerPid = self(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    ?assertEqual(ok, erlmcp_registry:register_server(ToolName, ServerPid, Config)),
    ?assertMatch({ok, {ServerPid, _}}, erlmcp_registry:find_server(ToolName)),

    gen_server:stop(Pid).

registry_mixed_language_tools_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),

    Tools = [
        {?JAPANESE_TOOL_NAME, self()},
        {?ARABIC_TOOL_NAME, self()},
        {?KOREAN_TOOL_NAME, self()},
        {?EMOJI_TOOL_FULL, self()}
    ],

    Config = #{capabilities => #mcp_server_capabilities{}},

    lists:foreach(fun({Name, SrvPid}) ->
        ?assertEqual(ok, erlmcp_registry:register_server(Name, SrvPid, Config))
    end, Tools),

    %% Verify all tools are registered
    lists:foreach(fun({Name, _SrvPid}) ->
        ?assertMatch({ok, {_Pid, _}}, erlmcp_registry:find_server(Name))
    end, Tools),

    gen_server:stop(Pid).

%%%====================================================================
%%% Transport Name Tests
%%%====================================================================

validate_transport_japanese_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_transport_name(?JAPANESE_TOOL_NAME)),
    ?assertEqual(ok, erlmcp_registry:validate_transport_name(?JAPANESE_MIXED)).

validate_transport_emoji_test() ->
    ?assertEqual(ok, erlmcp_registry:validate_transport_name(?EMOJI_TOOL_FULL)),
    ?assertEqual(ok, erlmcp_registry:validate_transport_name(?EMOJI_TOOL_COMPLEX)).

validate_transport_invalid_test() ->
    ?assertEqual({error, invalid_binary}, erlmcp_registry:validate_transport_name(invalid)),
    ?assertEqual({error, invalid_characters}, erlmcp_registry:validate_transport_name(?INVALID_NULL)).

%%%====================================================================
%%% Edge Cases and Boundary Tests
%%%====================================================================

empty_name_test() ->
    ?assertEqual({error, empty}, erlmcp_registry:validate_tool_name(<<>>)).

atom_conversion_roundtrip_test() ->
    %% Verify roundtrip conversion preserves UTF-8
    Original = ?MIXED_EMOJI_ALL,
    Atom = erlmcp_registry:normalize_name(Original),
    Recovered = atom_to_binary(Atom, utf8),
    ?assertEqual(Original, Recovered).

concurrent_registration_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    %% Register multiple tools with international names concurrently
    Names = [?JAPANESE_TOOL_NAME, ?ARABIC_TOOL_NAME, ?KOREAN_TOOL_NAME,
             ?HEBREW_TOOL_NAME, ?EMOJI_TOOL_FULL, ?CYRILLIC_TOOL_NAME],

    Pids = lists:duplicate(length(Names), self()),

    lists:foreach(fun({Name, SrvPid}) ->
        erlmcp_registry:register_server(Name, SrvPid, Config)
    end, lists:zip(Names, Pids)),

    %% Verify all registered
    lists:foreach(fun(Name) ->
        ?assertMatch({ok, {_Pid, _}}, erlmcp_registry:find_server(Name))
    end, Names),

    gen_server:stop(Pid).

%%%====================================================================
%%% Performance Tests
%%%====================================================================

international_atom_conversion_performance_test() ->
    %% Measure performance of international name conversions
    Names = lists:duplicate(1000, ?MIXED_EMOJI_ALL),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Name) ->
            erlmcp_registry:normalize_name(Name)
        end, Names)
    end),

    %% Should complete 1000 conversions in under 100ms
    ?assert(Time < 100000).

registry_lookup_performance_test() ->
    {ok, Pid} = erlmcp_registry:start_link(),
    Config = #{capabilities => #mcp_server_capabilities{}},

    %% Register 100 international tools
    Names = [<< <<"tool_", (integer_to_binary(N))/binary, "_", ?JAPANESE_TOOL_NAME/binary>> >>
             || N <- lists:seq(1, 100)],

    lists:foreach(fun(Name) ->
        erlmcp_registry:register_server(Name, self(), Config)
    end, Names),

    %% Measure lookup performance
    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Name) ->
            erlmcp_registry:find_server(Name)
        end, Names)
    end),

    %% Should complete 100 lookups in under 50ms
    ?assert(Time < 50000),

    gen_server:stop(Pid).
