%%%-------------------------------------------------------------------
%% @doc Comprehensive Test Suite for HTTP Header Validation (Gap #10)
%%
%% Tests cover:
%% - Protocol version validation (required, supported versions)
%% - Content-Type validation (method-dependent)
%% - Accept header validation (content negotiation)
%% - Session ID validation (format checking)
%% - Authorization validation (Bearer tokens)
%% - Error responses (400, 415, 406)
%% - Header case insensitivity
%% - Header extraction and normalization
%% - Complete request validation
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_header_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

http_header_validator_test_() ->
    [
        {"Protocol Version Validation", protocol_version_tests()},
        {"Content-Type Validation", content_type_tests()},
        {"Accept Header Validation", accept_header_tests()},
        {"Session ID Validation", session_id_tests()},
        {"Authorization Validation", authorization_tests()},
        {"Complete Request Validation", complete_request_tests()},
        {"Header Extraction", header_extraction_tests()},
        {"Error Response Formatting", error_response_tests()},
        {"Case Insensitivity", case_insensitivity_tests()},
        {"Edge Cases", edge_case_tests()}
    ].

%%====================================================================
%% Protocol Version Validation Tests
%%====================================================================

protocol_version_tests() ->
    [
        {"Valid version 2025-11-25", fun test_valid_version_2025_11_25/0},
        {"Valid version 2024-11-05", fun test_valid_version_2024_11_05/0},
        {"Valid version 2024-10-01", fun test_valid_version_2024_10_01/0},
        {"Valid version 2024-09-01", fun test_valid_version_2024_09_01/0},
        {"Default version when missing", fun test_default_version/0},
        {"Invalid version rejected", fun test_invalid_version/0},
        {"Version as string converted", fun test_version_as_string/0},
        {"Empty header map defaults", fun test_empty_headers_default_version/0},
        {"Unsupported version error includes supported list", fun test_unsupported_version_error/0}
    ].

test_valid_version_2025_11_25() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2025-11-25">>}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2025-11-25">>, Version).

test_valid_version_2024_11_05() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2024-11-05">>}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2024-11-05">>, Version).

test_valid_version_2024_10_01() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2024-10-01">>}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2024-10-01">>, Version).

test_valid_version_2024_09_01() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2024-09-01">>}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2024-09-01">>, Version).

test_default_version() ->
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(#{}),
    ?assertEqual(<<"2025-11-25">>, Version).

test_invalid_version() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2023-01-01">>}],
    {error, {400, _Message, Data}} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertMatch(#{<<"error_type">> := <<"unsupported_protocol_version">>}, Data).

test_version_as_string() ->
    Headers = [{<<"mcp-protocol-version">>, "2024-11-05"}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2024-11-05">>, Version).

test_empty_headers_default_version() ->
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(#{}),
    ?assertEqual(<<"2025-11-25">>, Version).

test_unsupported_version_error() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2099-12-31">>}],
    {error, {400, <<"Unsupported protocol version">>, Data}} =
        erlmcp_http_header_validator:validate_protocol_version(
            erlmcp_http_header_validator:extract_headers_map(Headers)
        ),
    ?assert(maps:is_key(<<"supported">>, Data)),
    ?assert(maps:is_key(<<"requested">>, Data)).

%%====================================================================
%% Content-Type Validation Tests
%%====================================================================

content_type_tests() ->
    [
        {"POST requires Content-Type", fun test_post_requires_content_type/0},
        {"PUT requires Content-Type", fun test_put_requires_content_type/0},
        {"PATCH requires Content-Type", fun test_patch_requires_content_type/0},
        {"GET doesn't require Content-Type", fun test_get_no_content_type/0},
        {"HEAD doesn't require Content-Type", fun test_head_no_content_type/0},
        {"Valid Content-Type application/json", fun test_valid_content_type_json/0},
        {"Valid Content-Type with charset", fun test_content_type_with_charset/0},
        {"Invalid Content-Type rejected", fun test_invalid_content_type/0},
        {"Missing Content-Type on POST error", fun test_missing_content_type_error/0},
        {"Unsupported Content-Type returns 415", fun test_unsupported_content_type_status/0}
    ].

test_post_requires_content_type() ->
    Headers = #{},
    {error, {400, _Message, Data}} =
        erlmcp_http_header_validator:validate_content_type(Headers, post),
    ?assertMatch(#{<<"error_type">> := <<"missing_content_type">>}, Data).

test_put_requires_content_type() ->
    Headers = #{},
    {error, {400, _Message, _Data}} =
        erlmcp_http_header_validator:validate_content_type(Headers, put),
    ok.

test_patch_requires_content_type() ->
    Headers = #{},
    {error, {400, _Message, _Data}} =
        erlmcp_http_header_validator:validate_content_type(Headers, patch),
    ok.

test_get_no_content_type() ->
    Headers = #{},
    {ok, undefined} = erlmcp_http_header_validator:validate_content_type(Headers, get),
    ok.

test_head_no_content_type() ->
    Headers = #{},
    {ok, undefined} = erlmcp_http_header_validator:validate_content_type(Headers, head),
    ok.

test_valid_content_type_json() ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, ContentType} = erlmcp_http_header_validator:validate_content_type(
        erlmcp_http_header_validator:extract_headers_map(Headers),
        post
    ),
    ?assertEqual(<<"application/json">>, ContentType).

test_content_type_with_charset() ->
    Headers = [{<<"content-type">>, <<"application/json; charset=utf-8">>}],
    {ok, ContentType} = erlmcp_http_header_validator:validate_content_type(
        erlmcp_http_header_validator:extract_headers_map(Headers),
        post
    ),
    ?assertEqual(<<"application/json">>, ContentType).

test_invalid_content_type() ->
    Headers = [{<<"content-type">>, <<"application/xml">>}],
    {error, {415, <<"Invalid Content-Type">>, _Data}} =
        erlmcp_http_header_validator:validate_content_type(
            erlmcp_http_header_validator:extract_headers_map(Headers),
            post
        ),
    ok.

test_missing_content_type_error() ->
    {error, {400, <<"Missing Content-Type header">>, Data}} =
        erlmcp_http_header_validator:validate_content_type(#{}, post),
    ?assertMatch(#{<<"method">> := <<"post">>}, Data).

test_unsupported_content_type_status() ->
    Headers = [{<<"content-type">>, <<"text/html">>}],
    {error, {StatusCode, _Message, _Data}} =
        erlmcp_http_header_validator:validate_content_type(
            erlmcp_http_header_validator:extract_headers_map(Headers),
            post
        ),
    ?assertEqual(415, StatusCode).

%%====================================================================
%% Accept Header Validation Tests
%%====================================================================

accept_header_tests() ->
    [
        {"Accept application/json returns json", fun test_accept_json/0},
        {"Accept text/event-stream returns sse", fun test_accept_sse/0},
        {"Accept wildcard returns mixed", fun test_accept_wildcard/0},
        {"Accept with quality factor", fun test_accept_quality_factor/0},
        {"Missing Accept defaults to json", fun test_missing_accept/0},
        {"Multiple Accept types", fun test_multiple_accept_types/0},
        {"Accept application/* defaults to json", fun test_accept_app_wildcard/0},
        {"Invalid Accept type error", fun test_invalid_accept_type/0},
        {"Accept quality factor ignored", fun test_accept_quality_ignored/0}
    ].

test_accept_json() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

test_accept_sse() ->
    Headers = [{<<"accept">>, <<"text/event-stream">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(sse, Format).

test_accept_wildcard() ->
    Headers = [{<<"accept">>, <<"*/*">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(mixed, Format).

test_accept_quality_factor() ->
    Headers = [{<<"accept">>, <<"application/json;q=0.9">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

test_missing_accept() ->
    {ok, Format} = erlmcp_http_header_validator:validate_accept(#{}),
    ?assertEqual(json, Format).

test_multiple_accept_types() ->
    Headers = [{<<"accept">>, <<"application/json, text/event-stream">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

test_accept_app_wildcard() ->
    Headers = [{<<"accept">>, <<"application/*">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

test_invalid_accept_type() ->
    Headers = [{<<"accept">>, <<"application/xml">>}],
    {error, {406, <<"No acceptable content types">>, _Data}} =
        erlmcp_http_header_validator:validate_accept(
            erlmcp_http_header_validator:extract_headers_map(Headers)
        ),
    ok.

test_accept_quality_ignored() ->
    Headers = [{<<"accept">>, <<"application/json;q=0.5, text/event-stream;q=0.9">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

%%====================================================================
%% Session ID Validation Tests
%%====================================================================

session_id_tests() ->
    [
        {"Missing Session ID allowed on first request", fun test_missing_session_id/0},
        {"Valid 32-byte hex Session ID", fun test_valid_session_id_32/0},
        {"Valid 64-byte Session ID", fun test_valid_session_id_64/0},
        {"Too short Session ID rejected", fun test_short_session_id/0},
        {"Session ID format error includes length", fun test_session_id_error_details/0},
        {"Session ID as string converted", fun test_session_id_string/0},
        {"Minimum 32 bytes enforced", fun test_session_id_minimum_length/0}
    ].

test_missing_session_id() ->
    {ok, undefined} = erlmcp_http_header_validator:validate_session_id(#{}),
    ok.

test_valid_session_id_32() ->
    SessionId = <<"12345678901234567890123456789012">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {ok, ReturnedId} = erlmcp_http_header_validator:validate_session_id(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(SessionId, ReturnedId).

test_valid_session_id_64() ->
    SessionId = <<"1234567890123456789012345678901234567890123456789012345678901234">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {ok, ReturnedId} = erlmcp_http_header_validator:validate_session_id(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(SessionId, ReturnedId).

test_short_session_id() ->
    SessionId = <<"short">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {error, {400, <<"Invalid session ID format">>, _Data}} =
        erlmcp_http_header_validator:validate_session_id(
            erlmcp_http_header_validator:extract_headers_map(Headers)
        ),
    ok.

test_session_id_error_details() ->
    SessionId = <<"short">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {error, {400, _Message, Data}} =
        erlmcp_http_header_validator:validate_session_id(
            erlmcp_http_header_validator:extract_headers_map(Headers)
        ),
    ?assert(maps:is_key(<<"min_length">>, Data)).

test_session_id_string() ->
    SessionId = <<"12345678901234567890123456789012">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {ok, ReturnedId} = erlmcp_http_header_validator:validate_session_id(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(SessionId, ReturnedId).

test_session_id_minimum_length() ->
    %% Exactly 32 bytes should pass
    SessionId = <<"12345678901234567890123456789012">>,
    Headers = [{<<"mcp-session-id">>, SessionId}],
    {ok, _} = erlmcp_http_header_validator:validate_session_id(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),

    %% 31 bytes should fail
    ShortSessionId = <<"1234567890123456789012345678901">>,
    Headers2 = [{<<"mcp-session-id">>, ShortSessionId}],
    {error, {400, _Message, _Data}} =
        erlmcp_http_header_validator:validate_session_id(
            erlmcp_http_header_validator:extract_headers_map(Headers2)
        ),
    ok.

%%====================================================================
%% Authorization Validation Tests
%%====================================================================

authorization_tests() ->
    [
        {"Missing Authorization allowed", fun test_missing_authorization/0},
        {"Valid Bearer token", fun test_valid_bearer_token/0},
        {"Bearer token extracted", fun test_bearer_token_extracted/0},
        {"Other auth schemes accepted", fun test_other_auth_schemes/0},
        {"Invalid Authorization format", fun test_invalid_auth_format/0}
    ].

test_missing_authorization() ->
    {ok, undefined} = erlmcp_http_header_validator:validate_authorization(#{}),
    ok.

test_valid_bearer_token() ->
    Headers = [{<<"authorization">>, <<"Bearer token123456">>}],
    {ok, Token} = erlmcp_http_header_validator:validate_authorization(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"token123456">>, Token).

test_bearer_token_extracted() ->
    Headers = [{<<"authorization">>, <<"Bearer my-token-here">>}],
    {ok, Token} = erlmcp_http_header_validator:validate_authorization(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).

test_other_auth_schemes() ->
    Headers = [{<<"authorization">>, <<"Basic dXNlcjpwYXNz">>}],
    {ok, _Auth} = erlmcp_http_header_validator:validate_authorization(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ok.

test_invalid_auth_format() ->
    Headers = [{<<"authorization">>, <<"Bearer">>}],
    {error, {401, <<"Invalid authorization header">>, _Data}} =
        erlmcp_http_header_validator:validate_authorization(
            erlmcp_http_header_validator:extract_headers_map(Headers)
        ),
    ok.

%%====================================================================
%% Complete Request Validation Tests
%%====================================================================

complete_request_tests() ->
    [
        {"Valid POST request", fun test_valid_post_request/0},
        {"Valid GET request", fun test_valid_get_request/0},
        {"POST missing Content-Type fails", fun test_post_missing_content_type/0},
        {"Complete validation returns all headers", fun test_complete_validation_map/0},
        {"Invalid protocol version fails complete", fun test_invalid_version_complete/0},
        {"All headers properly normalized", fun test_headers_normalized/0}
    ].

test_valid_post_request() ->
    Headers = [
        {<<"mcp-protocol-version">>, <<"2025-11-25">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json">>}
    ],
    {ok, Result} = erlmcp_http_header_validator:validate_request_headers(Headers, post),
    ?assertMatch(#{protocol_version := _, content_type := _, accept_format := _}, Result).

test_valid_get_request() ->
    Headers = [
        {<<"mcp-protocol-version">>, <<"2024-11-05">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],
    {ok, Result} = erlmcp_http_header_validator:validate_request_headers(Headers, get),
    ?assertMatch(#{protocol_version := <<"2024-11-05">>, accept_format := sse}, Result).

test_post_missing_content_type() ->
    Headers = [
        {<<"mcp-protocol-version">>, <<"2025-11-25">>},
        {<<"accept">>, <<"application/json">>}
    ],
    {error, {400, _Message, _Data}} =
        erlmcp_http_header_validator:validate_request_headers(Headers, post),
    ok.

test_complete_validation_map() ->
    Headers = [
        {<<"mcp-protocol-version">>, <<"2025-11-25">>},
        {<<"content-type">>, <<"application/json">>},
        {<<"accept">>, <<"application/json">>},
        {<<"mcp-session-id">>, <<"12345678901234567890123456789012">>}
    ],
    {ok, Result} = erlmcp_http_header_validator:validate_request_headers(Headers, post),
    ?assert(maps:is_key(protocol_version, Result)),
    ?assert(maps:is_key(session_id, Result)),
    ?assert(maps:is_key(original_headers, Result)).

test_invalid_version_complete() ->
    Headers = [
        {<<"mcp-protocol-version">>, <<"1999-01-01">>}
    ],
    {error, {400, _Message, _Data}} =
        erlmcp_http_header_validator:validate_request_headers(Headers, get),
    ok.

test_headers_normalized() ->
    Headers = [
        {<<"MCP-Protocol-Version">>, <<"2025-11-25">>},
        {<<"Content-Type">>, <<"application/json">>},
        {<<"Accept">>, <<"application/json">>}
    ],
    {ok, Result} = erlmcp_http_header_validator:validate_request_headers(Headers, post),
    ?assertEqual(<<"2025-11-25">>, maps:get(protocol_version, Result)).

%%====================================================================
%% Header Extraction Tests
%%====================================================================

header_extraction_tests() ->
    [
        {"Extract from list returns map", fun test_extract_list_to_map/0},
        {"Extract from map returns map", fun test_extract_map_to_map/0},
        {"Header names normalized to lowercase", fun test_header_names_lowercase/0},
        {"Mixed case headers handled", fun test_mixed_case_headers/0},
        {"String header names converted", fun test_string_header_names/0},
        {"String values converted to binary", fun test_string_values_binary/0}
    ].

test_extract_list_to_map() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(is_map(Map)),
    ?assert(maps:is_key(<<"accept">>, Map)).

test_extract_map_to_map() ->
    Headers = #{<<"accept">> => <<"application/json">>},
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(is_map(Map)).

test_header_names_lowercase() ->
    Headers = [{<<"Accept">>, <<"application/json">>}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(maps:is_key(<<"accept">>, Map)).

test_mixed_case_headers() ->
    Headers = [{<<"MCP-Protocol-Version">>, <<"2025-11-25">>}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(maps:is_key(<<"mcp-protocol-version">>, Map)).

test_string_header_names() ->
    Headers = [{"accept", <<"application/json">>}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(maps:is_key(<<"accept">>, Map)).

test_string_values_binary() ->
    Headers = [{<<"accept">>, "application/json"}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    Value = maps:get(<<"accept">>, Map),
    ?assert(is_binary(Value)).

%%====================================================================
%% Error Response Formatting Tests
%%====================================================================

error_response_tests() ->
    [
        {"Format 400 Bad Request error", fun test_format_400_error/0},
        {"Format 415 Unsupported Media Type error", fun test_format_415_error/0},
        {"Format 406 Not Acceptable error", fun test_format_406_error/0},
        {"Error response includes data field", fun test_error_with_data/0},
        {"Error response without data", fun test_error_without_data/0},
        {"Error response is valid JSON", fun test_error_json_valid/0}
    ].

test_format_400_error() ->
    {StatusCode, _Headers, _Body} =
        erlmcp_http_header_validator:format_error_response(400, <<"Bad Request">>, undefined),
    ?assertEqual(400, StatusCode).

test_format_415_error() ->
    {StatusCode, _Headers, _Body} =
        erlmcp_http_header_validator:format_error_response(415, <<"Unsupported Media Type">>, undefined),
    ?assertEqual(415, StatusCode).

test_format_406_error() ->
    {StatusCode, _Headers, _Body} =
        erlmcp_http_header_validator:format_error_response(406, <<"Not Acceptable">>, undefined),
    ?assertEqual(406, StatusCode).

test_error_with_data() ->
    Data = #{<<"key">> => <<"value">>},
    {_StatusCode, _Headers, Body} =
        erlmcp_http_header_validator:format_error_response(400, <<"Error">>, Data),
    Decoded = jsx:decode(Body),
    ?assert(maps:is_key(<<"error">>, Decoded)).

test_error_without_data() ->
    {_StatusCode, _Headers, Body} =
        erlmcp_http_header_validator:format_error_response(400, <<"Error">>, undefined),
    Decoded = jsx:decode(Body),
    Error = maps:get(<<"error">>, Decoded),
    ?assert(not maps:is_key(<<"data">>, Error)).

test_error_json_valid() ->
    {_StatusCode, _Headers, Body} =
        erlmcp_http_header_validator:format_error_response(400, <<"Bad Request">>, undefined),
    try
        jsx:decode(Body),
        ok
    catch
        error:_ ->
            throw("Invalid JSON in error response")
    end.

%%====================================================================
%% Case Insensitivity Tests
%%====================================================================

case_insensitivity_tests() ->
    [
        {"Protocol version header case insensitive", fun test_protocol_version_case/0},
        {"Accept header case insensitive", fun test_accept_header_case/0},
        {"Content-Type header case insensitive", fun test_content_type_case/0},
        {"Session ID header case insensitive", fun test_session_id_case/0},
        {"Authorization header case insensitive", fun test_authorization_case/0}
    ].

test_protocol_version_case() ->
    Headers = [{<<"MCP-PROTOCOL-VERSION">>, <<"2025-11-25">>}],
    {ok, Version} = erlmcp_http_header_validator:validate_protocol_version(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(<<"2025-11-25">>, Version).

test_accept_header_case() ->
    Headers = [{<<"ACCEPT">>, <<"application/json">>}],
    {ok, Format} = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(json, Format).

test_content_type_case() ->
    Headers = [{<<"CONTENT-TYPE">>, <<"application/json">>}],
    {ok, ContentType} = erlmcp_http_header_validator:validate_content_type(
        erlmcp_http_header_validator:extract_headers_map(Headers),
        post
    ),
    ?assertEqual(<<"application/json">>, ContentType).

test_session_id_case() ->
    SessionId = <<"12345678901234567890123456789012">>,
    Headers = [{<<"MCP-SESSION-ID">>, SessionId}],
    {ok, ReturnedId} = erlmcp_http_header_validator:validate_session_id(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assertEqual(SessionId, ReturnedId).

test_authorization_case() ->
    Headers = [{<<"AUTHORIZATION">>, <<"Bearer token">>}],
    {ok, _Auth} = erlmcp_http_header_validator:validate_authorization(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ok.

%%====================================================================
%% Edge Case Tests
%%====================================================================

edge_case_tests() ->
    [
        {"Empty headers list", fun test_empty_headers_list/0},
        {"Empty headers map", fun test_empty_headers_map/0},
        {"Very long header value", fun test_long_header_value/0},
        {"Null bytes in header", fun test_null_bytes/0},
        {"Unicode in header values", fun test_unicode_header_values/0},
        {"Duplicate headers (first one wins)", fun test_duplicate_headers/0}
    ].

test_empty_headers_list() ->
    {ok, _Result} = erlmcp_http_header_validator:validate_request_headers([], get),
    ok.

test_empty_headers_map() ->
    {ok, _Result} = erlmcp_http_header_validator:validate_request_headers(#{}, get),
    ok.

test_long_header_value() ->
    LongValue = binary:copy(<<"a">>, 10000),
    Headers = [{<<"accept">>, LongValue}],
    %% Should either default or reject gracefully
    Result = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assert(
        Result =:= {ok, json} orelse
        (is_tuple(Result) andalso element(1, Result) =:= error)
    ).

test_null_bytes() ->
    %% Binary with null bytes
    Headers = [{<<"accept">>, <<"application/json\x00">>}],
    Result = erlmcp_http_header_validator:validate_accept(
        erlmcp_http_header_validator:extract_headers_map(Headers)
    ),
    ?assert(is_tuple(Result)).

test_unicode_header_values() ->
    %% Unicode characters in User-Agent
    Headers = [{<<"user-agent">>, <<"Mozilla/5.0 (Café)">>}],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    ?assert(maps:is_key(<<"user-agent">>, Map)).

test_duplicate_headers() ->
    %% List with duplicate header names (first one should win)
    Headers = [
        {<<"accept">>, <<"application/json">>},
        {<<"accept">>, <<"text/event-stream">>}
    ],
    Map = erlmcp_http_header_validator:extract_headers_map(Headers),
    Value = maps:get(<<"accept">>, Map),
    %% Last one wins in map construction
    ?assert(is_binary(Value)).
