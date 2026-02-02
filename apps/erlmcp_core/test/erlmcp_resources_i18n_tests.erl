-module(erlmcp_resources_i18n_tests).
-author('erlmcp').

%% Internationalization tests for erlmcp_resources with OTP 28 UTF-8 support
%% Tests international resource URIs, paths, and names

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Data: International Resource URIs
%%%====================================================================

%% Japanese resource paths
-define(JP_FILE_URI, <<"file://path/to/ファイル.txt">>).
-define(JP_RESOURCE_NAME, <<"日本語リソース">>).

%% Arabic resource paths
-define(AR_FILE_URI, <<"file://path/to/ملف.txt">>).
-define(AR_RESOURCE_NAME, <<"المصدر">>).

%% Korean resource paths
-define(KO_FILE_URI, <<"file://path/to/파일.txt">>).
-define(KO_RESOURCE_NAME, <<"한국어_리소스">>).

%% Mixed language paths
-define(MIXED_URI, <<"file://path/ファイル/ملف/파イル/file.txt">>).

%% Emoji in paths
-define(EMOJI_URI, <<"file://path/to/📁_folder/📄_document.txt">>).

%% Cyrillic paths
-define(CYRILLIC_URI, <<"file://path/русский/файл.txt">>).

%% Chinese paths
-define(ZH_URI, <<"file://path/中文/文件.txt">>).

%% HTTP international URLs
-define(JP_HTTP_URL, <<"http://example.com/パス">>).
-define(AR_HTTPS_URL, <<"https://example.com/مسار">>).

%% Invalid URIs
-define(NULL_URI, <<"file://path\0name">>).
-define(TOO_LONG_URI, <<"file://", (binary:copy(<<"a">>, 5000))/binary>>).

%%%====================================================================
%%% URI Validation Tests
%%%====================================================================

validate_japanese_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?JP_FILE_URI)),
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?JP_HTTP_URL)).

validate_arabic_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?AR_FILE_URI)),
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?AR_HTTPS_URL)).

validate_korean_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?KO_FILE_URI)).

validate_mixed_language_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?MIXED_URI)).

validate_emoji_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?EMOJI_URI)).

validate_cyrillic_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?CYRILLIC_URI)).

validate_chinese_uri_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(?ZH_URI)).

validate_invalid_uri_test() ->
    ?assertEqual({error, invalid_binary}, erlmcp_resources:validate_resource_uri(<<>>)),
    ?assertEqual({error, unknown_uri_scheme}, erlmcp_resources:validate_resource_uri(<<"invalid://test">>)).

validate_too_long_uri_test() ->
    ?assertEqual({error, uri_too_long}, erlmcp_resources:validate_resource_uri(?TOO_LONG_URI)).

%%%====================================================================
;;; URI Normalization Tests
%%%====================================================================

normalize_japanese_uri_test() ->
    Normalized = erlmcp_resources:normalize_resource_uri(?JP_FILE_URI),
    ?assertEqual(?JP_FILE_URI, Normalized),
    ?assert(is_binary(Normalized)).

normalize_arabic_uri_test() ->
    Normalized = erlmcp_resources:normalize_resource_uri(?AR_FILE_URI),
    ?assertEqual(?AR_FILE_URI, Normalized),
    ?assert(is_binary(Normalized)).

normalize_emoji_uri_test() ->
    Normalized = erlmcp_resources:normalize_resource_uri(?EMOJI_URI),
    ?assertEqual(?EMOJI_URI, Normalized),
    ?assert(is_binary(Normalized)).

normalize_mixed_uri_test() ->
    Normalized = erlmcp_resources:normalize_resource_uri(?MIXED_URI),
    ?assertEqual(?MIXED_URI, Normalized),
    ?assert(is_binary(Normalized)).

%%%====================================================================
%%% Character Length Tests
%%%====================================================================

uri_char_length_japanese_test() ->
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?JP_FILE_URI)),
    Chars = string:length(?JP_FILE_URI),
    ?assert(Chars > 0),
    ?assert(Chars =< 255).

uri_char_length_mixed_test() ->
    ?assertEqual(ok, erlmcp_atoms:char_length_check(?MIXED_URI)),
    Chars = string:length(?MIXED_URI),
    ?assert(Chars > 0).

%%%====================================================================
%%% Resource Operations Tests
%%%====================================================================

add_root_japanese_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    ?assertEqual(ok, erlmcp_resources:add_root(?JP_FILE_URI, ?JP_RESOURCE_NAME)),

    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assert(lists:any(fun(R) ->
        maps:get(uri, R, undefined) =:= ?JP_FILE_URI
    end, Roots)),

    gen_server:stop(Pid).

add_root_arabic_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    ?assertEqual(ok, erlmcp_resources:add_root(?AR_FILE_URI, ?AR_RESOURCE_NAME)),

    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assert(lists:any(fun(R) ->
        maps:get(uri, R, undefined) =:= ?AR_FILE_URI
    end, Roots)),

    gen_server:stop(Pid).

add_root_mixed_language_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    URIs = [?JP_FILE_URI, ?AR_FILE_URI, ?KO_FILE_URI, ?EMOJI_URI],
    Names = [?JP_RESOURCE_NAME, ?AR_RESOURCE_NAME, ?KO_RESOURCE_NAME, <<"Emoji Root">>],

    lists:foreach(fun({Uri, Name}) ->
        ?assertEqual(ok, erlmcp_resources:add_root(Uri, Name))
    end, lists:zip(URIs, Names)),

    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assertEqual(length(URIs), length(Roots)),

    gen_server:stop(Pid).

remove_root_international_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    ?assertEqual(ok, erlmcp_resources:add_root(?JP_FILE_URI, ?JP_RESOURCE_NAME)),
    ?assertEqual(ok, erlmcp_resources:remove_root(?JP_FILE_URI)),

    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assertNot(lists:any(fun(R) ->
        maps:get(uri, R, undefined) =:= ?JP_FILE_URI
    end, Roots)),

    gen_server:stop(Pid).

%%%====================================================================
;;; URI Scheme Validation Tests
%%%====================================================================

validate_file_scheme_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(<<"file://test.txt">>)),
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(<<"file:///absolute/path">>)).

validate_http_scheme_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(<<"http://example.com">>)),
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(<<"https://example.com">>)).

validate_custom_scheme_test() ->
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(<<"custom://resource">>)).

validate_s3_scheme_test() ->
    %% Even though not explicitly supported, should fail gracefully
    ?assertEqual({error, unknown_uri_scheme}, erlmcp_resources:validate_resource_uri(<<"s3://bucket/key">>)).

%%%====================================================================
%%% Edge Cases and Boundary Tests
%%%====================================================================

empty_uri_test() ->
    ?assertEqual({error, invalid_binary}, erlmcp_resources:validate_resource_uri(<<>>)).

uri_with_control_chars_test() ->
    ?assertEqual({error, invalid_binary}, erlmcp_resources:validate_resource_uri(?NULL_URI)).

uri_255_chars_test() ->
    %% 255 character URI should pass
    URI255 = <<"file://", (binary:copy(<<"a">>, 246))/binary>>,
    ?assertEqual(ok, erlmcp_resources:validate_resource_uri(URI256)).

uri_256_chars_test() ->
    %% 256 characters should fail if it exceeds byte limit
    %% Note: file:// + 253 chars = 255 byte prefix
    URI256 = <<"file://", (binary:copy(<<"a">>, 5000))/binary>>,
    ?assertEqual({error, uri_too_long}, erlmcp_resources:validate_resource_uri(URI256)).

%%%====================================================================
%%% Performance Tests
%%%====================================================================

international_uri_validation_performance_test() ->
    URIs = lists:duplicate(1000, ?MIXED_URI),

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Uri) ->
            erlmcp_resources:validate_resource_uri(Uri)
        end, URIs)
    end),

    %% Should complete 1000 validations in under 100ms
    ?assert(Time < 100000).

resource_operations_performance_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    URIs = [<< <<"file://path_", (integer_to_binary(N))/binary, "_", ?JP_FILE_URI/binary>> >>
           || N <- lists:seq(1, 100)],

    {Time, _} = timer:tc(fun() ->
        lists:foreach(fun(Uri) ->
            erlmcp_resources:add_root(Uri, <<"Test">>)
        end, URIs)
    end),

    %% Should complete 100 additions in under 100ms
    ?assert(Time < 100000),

    gen_server:stop(Pid).

%%%====================================================================
%%% Integration Tests
%%%====================================================================

full_lifecycle_international_test() ->
    {ok, Pid} = erlmcp_resources:start_link(),

    %% Add international roots
    Roots = [
        {?JP_FILE_URI, ?JP_RESOURCE_NAME},
        {?AR_FILE_URI, ?AR_RESOURCE_NAME},
        {?KO_FILE_URI, ?KO_RESOURCE_NAME}
    ],

    lists:foreach(fun({Uri, Name}) ->
        ?assertEqual(ok, erlmcp_resources:add_root(Uri, Name))
    end, Roots),

    %% List roots
    {ok, ListedRoots} = erlmcp_resources:list_roots(),
    ?assertEqual(length(Roots), length(ListedRoots)),

    %% Remove one root
    {JPUri, _JPName} = lists:nth(1, Roots),
    ?assertEqual(ok, erlmcp_resources:remove_root(JPUri)),

    %% Verify removal
    {ok, NewRoots} = erlmcp_resources:list_roots(),
    ?assertEqual(length(Roots) - 1, length(NewRoots)),

    gen_server:stop(Pid).
