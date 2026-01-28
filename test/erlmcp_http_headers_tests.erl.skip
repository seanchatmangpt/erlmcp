%%%-------------------------------------------------------------------
%% @doc Comprehensive Test Suite for MCP HTTP Header Validation
%%
%% Tests cover:
%% - MCP-Protocol-Version header validation
%% - Accept header validation and content negotiation
%% - HTTP status code mapping
%% - HTTP method validation (including DELETE disallowance)
%% - Content-Type response headers
%% - Error handling for missing/invalid headers
%% - Header case-insensitivity
%% - Quality factors in Accept header
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_headers_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

http_headers_test_() ->
    [
     {"Protocol Version Validation", protocol_version_tests()},
     {"Accept Header Validation", accept_header_tests()},
     {"HTTP Status Code Mapping", http_status_tests()},
     {"HTTP Method Validation", method_validation_tests()},
     {"Content-Type Response Headers", content_type_tests()},
     {"Header Extraction", header_extraction_tests()},
     {"Error Handling", error_handling_tests()},
     {"Header Case Insensitivity", case_insensitivity_tests()},
     {"Response Header Formatting", response_header_tests()}
    ].

setup() ->
    application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Protocol Version Validation Tests
%%====================================================================

protocol_version_tests() ->
    [
     {"Valid 2025-11-25 version", fun test_valid_version_2025/0},
     {"Valid 2024-11-05 version", fun test_valid_version_2024/0},
     {"Default version when missing", fun test_default_version/0},
     {"Invalid version rejected", fun test_invalid_version/0},
     {"Version as string", fun test_version_as_string/0},
     {"Empty version list", fun test_empty_headers_version/0},
     {"Case insensitive header name", fun test_case_insensitive_version_header/0}
    ].

test_valid_version_2025() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2025-11-25">>}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2025-11-25">>, Version).

test_valid_version_2024() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2024-11-05">>}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2024-11-05">>, Version).

test_default_version() ->
    Headers = [],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2024-11-05">>, Version).

test_invalid_version() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2023-01-01">>}],
    {error, {unsupported_protocol_version, _}} = erlmcp_http_headers:validate_protocol_version(Headers).

test_version_as_string() ->
    Headers = [{<<"mcp-protocol-version">>, "2024-11-05"}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2024-11-05">>, Version).

test_empty_headers_version() ->
    {ok, Version} = erlmcp_http_headers:validate_protocol_version([]),
    ?assertEqual(<<"2024-11-05">>, Version).

test_case_insensitive_version_header() ->
    Headers = [{<<"MCP-Protocol-Version">>, <<"2025-11-25">>}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2025-11-25">>, Version).

%%====================================================================
%% Accept Header Validation Tests
%%====================================================================

accept_header_tests() ->
    [
     {"Accept JSON", fun test_accept_json/0},
     {"Accept SSE", fun test_accept_sse/0},
     {"Accept wildcard defaults to JSON", fun test_accept_wildcard/0},
     {"Accept with quality factor", fun test_accept_quality_factor/0},
     {"Missing Accept header defaults to JSON", fun test_missing_accept/0},
     {"Invalid Accept header error", fun test_invalid_accept/0},
     {"Multiple Accept types", fun test_multiple_accept_types/0},
     {"Accept application/* defaults to JSON", fun test_accept_application_wildcard/0},
     {"Accept text/* defaults to SSE", fun test_accept_text_wildcard/0}
    ].

test_accept_json() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_accept_sse() ->
    Headers = [{<<"accept">>, <<"text/event-stream">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(sse, Format).

test_accept_wildcard() ->
    Headers = [{<<"accept">>, <<"*/*">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_accept_quality_factor() ->
    Headers = [{<<"accept">>, <<"application/json;q=0.9">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_missing_accept() ->
    Headers = [],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_invalid_accept() ->
    Headers = [{<<"accept">>, <<"application/xml">>}],
    {error, {unsupported_content_type, _}} = erlmcp_http_headers:validate_accept_header(Headers).

test_multiple_accept_types() ->
    Headers = [{<<"accept">>, <<"application/json, text/plain">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_accept_application_wildcard() ->
    Headers = [{<<"accept">>, <<"application/*">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_accept_text_wildcard() ->
    Headers = [{<<"accept">>, <<"text/*">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(sse, Format).

%%====================================================================
%% HTTP Status Code Tests
%%====================================================================

http_status_tests() ->
    [
     {"Status 200 for OK", fun test_status_ok/0},
     {"Status 202 for Accepted", fun test_status_accepted/0},
     {"Status 400 for Bad Request", fun test_status_bad_request/0},
     {"Status 403 for Forbidden", fun test_status_forbidden/0},
     {"Status 404 for Not Found", fun test_status_not_found/0},
     {"Status 405 for Method Not Allowed", fun test_status_method_not_allowed/0},
     {"Status 500 for Error", fun test_status_error/0},
     {"Default status for unknown", fun test_status_unknown/0}
    ].

test_status_ok() ->
    Status = erlmcp_http_headers:get_http_status_code(ok),
    ?assertEqual(200, Status).

test_status_accepted() ->
    Status = erlmcp_http_headers:get_http_status_code(accepted),
    ?assertEqual(202, Status).

test_status_bad_request() ->
    Status = erlmcp_http_headers:get_http_status_code(bad_request),
    ?assertEqual(400, Status).

test_status_forbidden() ->
    Status = erlmcp_http_headers:get_http_status_code(forbidden),
    ?assertEqual(403, Status).

test_status_not_found() ->
    Status = erlmcp_http_headers:get_http_status_code(not_found),
    ?assertEqual(404, Status).

test_status_method_not_allowed() ->
    Status = erlmcp_http_headers:get_http_status_code(method_not_allowed),
    ?assertEqual(405, Status).

test_status_error() ->
    Status = erlmcp_http_headers:get_http_status_code(error),
    ?assertEqual(500, Status).

test_status_unknown() ->
    Status = erlmcp_http_headers:get_http_status_code(unknown),
    ?assertEqual(400, Status).

%%====================================================================
%% HTTP Method Validation Tests
%%====================================================================

method_validation_tests() ->
    [
     {"Valid GET method", fun test_valid_get_method/0},
     {"Valid POST method", fun test_valid_post_method/0},
     {"Valid PUT method", fun test_valid_put_method/0},
     {"Valid PATCH method", fun test_valid_patch_method/0},
     {"Valid HEAD method", fun test_valid_head_method/0},
     {"Valid OPTIONS method", fun test_valid_options_method/0},
     {"DELETE method not allowed", fun test_delete_method_not_allowed/0},
     {"Method as atom", fun test_method_as_atom/0},
     {"Method case insensitive", fun test_method_case_insensitive/0},
     {"Invalid method", fun test_invalid_method/0},
     {"Method as string", fun test_method_as_string/0}
    ].

test_valid_get_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"GET">>),
    ?assertEqual(<<"GET">>, Method).

test_valid_post_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"POST">>),
    ?assertEqual(<<"POST">>, Method).

test_valid_put_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"PUT">>),
    ?assertEqual(<<"PUT">>, Method).

test_valid_patch_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"PATCH">>),
    ?assertEqual(<<"PATCH">>, Method).

test_valid_head_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"HEAD">>),
    ?assertEqual(<<"HEAD">>, Method).

test_valid_options_method() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"OPTIONS">>),
    ?assertEqual(<<"OPTIONS">>, Method).

test_delete_method_not_allowed() ->
    {error, method_not_allowed} = erlmcp_http_headers:validate_method(<<"DELETE">>).

test_method_as_atom() ->
    {ok, Method} = erlmcp_http_headers:validate_method(post),
    ?assertEqual(<<"POST">>, Method).

test_method_case_insensitive() ->
    {ok, Method} = erlmcp_http_headers:validate_method(<<"get">>),
    ?assertEqual(<<"GET">>, Method).

test_invalid_method() ->
    {error, invalid_method} = erlmcp_http_headers:validate_method(<<"INVALID">>).

test_method_as_string() ->
    {ok, Method} = erlmcp_http_headers:validate_method("PUT"),
    ?assertEqual(<<"PUT">>, Method).

%%====================================================================
%% Content-Type Tests
%%====================================================================

content_type_tests() ->
    [
     {"JSON content type", fun test_json_content_type/0},
     {"SSE content type", fun test_sse_content_type/0},
     {"Error content type", fun test_error_content_type/0},
     {"Unknown format defaults to JSON", fun test_unknown_format/0}
    ].

test_json_content_type() ->
    ContentType = erlmcp_http_headers:get_response_content_type(json),
    ?assertEqual(<<"application/json">>, ContentType).

test_sse_content_type() ->
    ContentType = erlmcp_http_headers:get_response_content_type(sse),
    ?assertEqual(<<"text/event-stream">>, ContentType).

test_error_content_type() ->
    ContentType = erlmcp_http_headers:get_response_content_type(error),
    ?assertEqual(<<"text/plain">>, ContentType).

test_unknown_format() ->
    ContentType = erlmcp_http_headers:get_response_content_type(unknown),
    ?assertEqual(<<"application/json">>, ContentType).

%%====================================================================
%% Header Extraction Tests
%%====================================================================

header_extraction_tests() ->
    [
     {"Extract existing header", fun test_extract_existing_header/0},
     {"Extract missing header", fun test_extract_missing_header/0},
     {"Header name case insensitive", fun test_extract_case_insensitive/0},
     {"Extract from first match", fun test_extract_first_match/0},
     {"Extract binary value", fun test_extract_binary_value/0},
     {"Extract string value", fun test_extract_string_value/0}
    ].

test_extract_existing_header() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    {ok, Value} = erlmcp_http_headers:extract_header(<<"accept">>, Headers),
    ?assertEqual(<<"application/json">>, Value).

test_extract_missing_header() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    {error, not_found} = erlmcp_http_headers:extract_header(<<"content-type">>, Headers).

test_extract_case_insensitive() ->
    Headers = [{<<"Accept">>, <<"application/json">>}],
    {ok, Value} = erlmcp_http_headers:extract_header(<<"ACCEPT">>, Headers),
    ?assertEqual(<<"application/json">>, Value).

test_extract_first_match() ->
    Headers = [
     {<<"accept">>, <<"text/plain">>},
     {<<"accept">>, <<"application/json">>}
    ],
    {ok, Value} = erlmcp_http_headers:extract_header(<<"accept">>, Headers),
    ?assertEqual(<<"text/plain">>, Value).

test_extract_binary_value() ->
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, Value} = erlmcp_http_headers:extract_header(<<"content-type">>, Headers),
    ?assert(is_binary(Value)).

test_extract_string_value() ->
    Headers = [{<<"content-type">>, "application/json"}],
    {ok, Value} = erlmcp_http_headers:extract_header(<<"content-type">>, Headers),
    ?assert(is_binary(Value)),
    ?assertEqual(<<"application/json">>, Value).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    [
     {"Invalid headers format for version", fun test_invalid_headers_format_version/0},
     {"Invalid headers format for accept", fun test_invalid_headers_format_accept/0},
     {"Map format version", fun test_map_format_version/0},
     {"Map format accept", fun test_map_format_accept/0},
     {"Map missing header defaults", fun test_map_missing_defaults/0}
    ].

test_invalid_headers_format_version() ->
    {error, invalid_headers_format} = erlmcp_http_headers:validate_protocol_version(invalid).

test_invalid_headers_format_accept() ->
    {error, invalid_headers_format} = erlmcp_http_headers:validate_accept_header(invalid).

test_map_format_version() ->
    HeadersMap = #{<<"mcp-protocol-version">> => <<"2025-11-25">>},
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(HeadersMap),
    ?assertEqual(<<"2025-11-25">>, Version).

test_map_format_accept() ->
    HeadersMap = #{<<"accept">> => <<"application/json">>},
    {ok, Format} = erlmcp_http_headers:validate_accept_header(HeadersMap),
    ?assertEqual(json, Format).

test_map_missing_defaults() ->
    EmptyMap = #{},
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(EmptyMap),
    {ok, Format} = erlmcp_http_headers:validate_accept_header(EmptyMap),
    ?assertEqual(<<"2024-11-05">>, Version),
    ?assertEqual(json, Format).

%%====================================================================
%% Case Insensitivity Tests
%%====================================================================

case_insensitivity_tests() ->
    [
     {"Version header lowercase", fun test_version_header_lowercase/0},
     {"Version header mixed case", fun test_version_header_mixed_case/0},
     {"Accept header lowercase", fun test_accept_header_lowercase/0},
     {"Accept header mixed case", fun test_accept_header_mixed_case/0}
    ].

test_version_header_lowercase() ->
    Headers = [{<<"mcp-protocol-version">>, <<"2025-11-25">>}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2025-11-25">>, Version).

test_version_header_mixed_case() ->
    Headers = [{<<"MCP-Protocol-Version">>, <<"2024-11-05">>}],
    {ok, Version} = erlmcp_http_headers:validate_protocol_version(Headers),
    ?assertEqual(<<"2024-11-05">>, Version).

test_accept_header_lowercase() ->
    Headers = [{<<"accept">>, <<"application/json">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(json, Format).

test_accept_header_mixed_case() ->
    Headers = [{<<"Accept">>, <<"text/event-stream">>}],
    {ok, Format} = erlmcp_http_headers:validate_accept_header(Headers),
    ?assertEqual(sse, Format).

%%====================================================================
%% Response Header Formatting Tests
%%====================================================================

response_header_tests() ->
    [
     {"Format JSON response headers", fun test_format_json_headers/0},
     {"Format SSE response headers", fun test_format_sse_headers/0},
     {"Format error response headers", fun test_format_error_headers/0},
     {"Merge with extra headers list", fun test_merge_headers_list/0},
     {"Merge with extra headers map", fun test_merge_headers_map/0},
     {"Override base headers", fun test_override_headers/0}
    ].

test_format_json_headers() ->
    Headers = erlmcp_http_headers:format_response_headers(json, []),
    {<<"content-type">>, ContentType} = lists:keyfind(<<"content-type">>, 1, Headers),
    ?assertEqual(<<"application/json">>, ContentType).

test_format_sse_headers() ->
    Headers = erlmcp_http_headers:format_response_headers(sse, []),
    {<<"content-type">>, ContentType} = lists:keyfind(<<"content-type">>, 1, Headers),
    ?assertEqual(<<"text/event-stream">>, ContentType).

test_format_error_headers() ->
    Headers = erlmcp_http_headers:format_response_headers(error, []),
    {<<"content-type">>, ContentType} = lists:keyfind(<<"content-type">>, 1, Headers),
    ?assertEqual(<<"text/plain">>, ContentType).

test_merge_headers_list() ->
    ExtraHeaders = [{<<"x-custom">>, <<"value">>}],
    Headers = erlmcp_http_headers:format_response_headers(json, ExtraHeaders),
    ?assert(lists:keymember(<<"content-type">>, 1, Headers)),
    ?assert(lists:keymember(<<"x-custom">>, 1, Headers)).

test_merge_headers_map() ->
    ExtraHeaders = #{<<"x-custom">> => <<"value">>},
    Headers = erlmcp_http_headers:format_response_headers(json, ExtraHeaders),
    ?assert(lists:keymember(<<"content-type">>, 1, Headers)),
    ?assert(lists:keymember(<<"x-custom">>, 1, Headers)).

test_override_headers() ->
    %% Extra headers should not override content-type
    ExtraHeaders = [{<<"content-type">>, <<"text/plain">>}],
    Headers = erlmcp_http_headers:format_response_headers(json, ExtraHeaders),
    {<<"content-type">>, ContentType} = lists:keyfind(<<"content-type">>, 1, Headers),
    ?assertEqual(<<"application/json">>, ContentType).
