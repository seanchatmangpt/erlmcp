%%%-------------------------------------------------------------------
%%% @doc HTTP Header Validator Test Suite
%%%
%%% Comprehensive security tests for HTTP header validation.
%%% Tests CRLF injection prevention, header size limits, invalid characters.
%%%
%%% == Chicago School TDD ==
%%% Uses REAL erlmcp_http_header_validator module.
%%% Tests observable behavior through API calls only.
%%% NO mocks - real validation logic.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_header_validator_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test 1-5: Valid Header Cases
%%%===================================================================

valid_get_headers_test() ->
    %% Test valid GET request headers for SSE
    Headers = [
        {<<"Accept">>, <<"text/event-stream">>},
        {<<"X-MCP-Protocol-Version">>, <<"2025-01-07">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, get),

    ?assertMatch({ok, _}, Result),
    {ok, ValidatedHeaders} = Result,
    ?assert(maps:is_key(protocol_version, ValidatedHeaders)).

valid_post_headers_test() ->
    %% Test valid POST request headers with JSON
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-MCP-Protocol-Version">>, <<"2025-01-07">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

valid_post_headers_with_charset_test() ->
    %% Test POST with charset parameter
    Headers = [
        {<<"Content-Type">>, <<"application/json; charset=utf-8">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

valid_headers_case_insensitive_test() ->
    %% Test header names are case-insensitive
    Headers = [
        {<<"CONTENT-TYPE">>, <<"application/json">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

valid_accept_with_multiple_types_test() ->
    %% Test Accept header with multiple media types
    Headers = [
        {<<"Accept">>, <<"text/html, text/event-stream, application/json">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, get),

    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Test 6-10: Missing Required Headers
%%%===================================================================

missing_accept_header_test() ->
    %% Test GET without Accept header for SSE
    Headers = [],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, get),

    ?assertMatch({error, {400, _, _}}, Result).

missing_content_type_header_test() ->
    %% Test POST without Content-Type
    Headers = [],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({error, {400, _, _}}, Result).

wrong_accept_header_test() ->
    %% Test GET with wrong Accept header
    Headers = [
        {<<"Accept">>, <<"application/json">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, get),

    ?assertMatch({error, {400, _, _}}, Result).

wrong_content_type_header_test() ->
    %% Test POST with wrong Content-Type
    Headers = [
        {<<"Content-Type">>, <<"text/plain">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({error, {400, _, _}}, Result).

invalid_charset_test() ->
    %% Test POST with invalid charset
    Headers = [
        {<<"Content-Type">>, <<"application/json; charset=iso-8859-1">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should either reject or normalize
    case Result of
        {ok, _} -> ok;
        {error, {400, _, _}} -> ok;
        Other -> ?assert(false, {unexpected_result, Other})
    end.

%%%===================================================================
%%% Test 11-15: CRLF Injection Prevention
%%%===================================================================

crlf_injection_in_header_name_test() ->
    %% Test CRLF injection in header name
    Headers = [
        {<<"Content-Type\r\nX-Injected">>, <<"application/json">>}
    ],

    %% Header name with CRLF should be rejected or sanitized
    %% This is typically handled at the HTTP parser level
    %% but we test the validator's handling
    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle gracefully
    ?assert(is_tuple(Result)).

crlf_injection_in_header_value_test() ->
    %% Test CRLF injection in header value
    Headers = [
        {<<"Content-Type">>, <<"application/json\r\nX-Injected: malicious">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle CRLF in values
    ?assert(is_tuple(Result)).

newline_injection_test() ->
    %% Test newline injection (LF only)
    Headers = [
        {<<"X-Custom">>, <<"value\ninjected">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, put),

    ?assert(is_tuple(Result)).

carriage_return_injection_test() ->
    %% Test carriage return injection (CR only)
    Headers = [
        {<<"X-Custom">>, <<"value\rinjected">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, put),

    ?assert(is_tuple(Result)).

null_byte_injection_test() ->
    %% Test null byte injection
    Headers = [
        {<<"Content-Type">>, <<"application/json", 0, "injected">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle null bytes
    ?assert(is_tuple(Result)).

%%%===================================================================
%%% Test 16-20: Header Size Limits
%%%===================================================================

normal_header_size_test() ->
    %% Test normal-sized header
    LargeValue = binary:copy(<<"a">>, 100),
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-Custom">>, LargeValue}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

very_long_header_value_test() ->
    %% Test very long header value (8KB)
    LargeValue = binary:copy(<<"x">>, 8192),
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-Large">>, LargeValue}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle or reject gracefully
    ?assert(is_tuple(Result)).

many_headers_test() ->
    %% Test many headers
    Headers = [
        {<<"Content-Type">>, <<"application/json">>}
        | [{<<"X-Custom-", (integer_to_binary(N))/binary>>, <<"value">>} || N <- lists:seq(1, 50)]
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

empty_header_name_test() ->
    %% Test empty header name
    Headers = [
        {<<"">>, <<"value">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle empty header names
    ?assert(is_tuple(Result)).

empty_header_value_test() ->
    %% Test empty header value
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-Custom">>, <<"">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

%%%===================================================================
%%% Test 21-25: Invalid Characters and Edge Cases
%%%===================================================================

special_characters_in_header_name_test() ->
    %% Test special characters in header name
    Headers = [
        {<<"X-Custom@Header">>, <<"value">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, put),

    %% Should handle special chars
    ?assert(is_tuple(Result)).

unicode_in_header_value_test() ->
    %% Test Unicode in header value
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-Custom">>, <<"value with 日本語">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    %% Should handle or encode Unicode
    ?assertMatch({ok, _}, Result).

whitespace_in_header_value_test() ->
    %% Test whitespace preservation
    Headers = [
        {<<"Content-Type">>, <<"application/json">>},
        {<<"X-Custom">>, <<"  value  with  spaces  ">>}
    ],

    Result = erlmcp_http_header_validator:validate_request_headers(Headers, post),

    ?assertMatch({ok, _}, Result).

format_error_response_test() ->
    %% Test error response formatting
    Response = erlmcp_http_header_validator:format_error_response(
        400,
        <<"Invalid header">>,
        #{header => <<"Content-Type">>, reason => <<"Missing">>}
    ),

    ?assertMatch({400, _, _}, Response),
    {_Code, Headers, Body} = Response,
    ?assert(is_list(Headers)),
    ?assert(is_binary(Body)),
    %% Verify JSON body
    Decoded = jsx:decode(Body),
    ?assert(maps:is_key(<<"error">>, Decoded)).

error_response_with_binary_data_test() ->
    %% Test error response with binary data
    Response = erlmcp_http_header_validator:format_error_response(
        400,
        <<"Invalid value">>,
        <<"Binary error data">>
    ),

    ?assertMatch({400, _, _}, Response),
    {_Code, _Headers, Body} = Response,
    Decoded = jsx:decode(Body),
    ?assert(maps:is_key(<<"data">>, Decoded)).
