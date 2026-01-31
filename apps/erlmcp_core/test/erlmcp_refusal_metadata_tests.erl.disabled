%%%-------------------------------------------------------------------
%%% @doc Refusal Code Metadata Tests
%%%
%%% Chicago School TDD tests for erlmcp_refusal metadata retrieval.
%%% Tests observable behavior through public API only.
%%% NO internal state inspection or record duplication.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal_metadata_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_refusal.hrl").

%%%====================================================================
%%% Queue & Backpressure Refusals (1001-1005)
%%%====================================================================

queue_refusal_test_() ->
    [
        ?_test(test_refusal_1001_queue_cap_exceeded()),
        ?_test(test_refusal_1002_queue_byte_cap_exceeded()),
        ?_test(test_refusal_1003_tenant_cap_exceeded()),
        ?_test(test_refusal_1004_buffer_overflow()),
        ?_test(test_refusal_1005_backpressure_active())
    ].

test_refusal_1001_queue_cap_exceeded() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1001, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Queue capacity exceeded">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1002_queue_byte_cap_exceeded() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_BYTE_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1002, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Byte capacity exceeded">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1003_tenant_cap_exceeded() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_TENANT_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1003, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Tenant quota exceeded">>, Message),
    ?assertEqual(critical, Severity).

test_refusal_1004_buffer_overflow() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_BUFFER_OVERFLOW),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1004, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Buffer overflow">>, Message),
    ?assertEqual(critical, Severity).

test_refusal_1005_backpressure_active() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_BACKPRESSURE_ACTIVE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1005, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Backpressure active">>, Message),
    ?assertEqual(warn, Severity).

%%%====================================================================
%%% Authentication & Authorization Refusals (1011-1016)
%%%====================================================================

auth_refusal_test_() ->
    [
        ?_test(test_refusal_1011_auth_failed()),
        ?_test(test_refusal_1012_auth_expired()),
        ?_test(test_refusal_1013_invalid_credentials()),
        ?_test(test_refusal_1014_authz_forbidden()),
        ?_test(test_refusal_1015_auth_missing()),
        ?_test(test_refusal_1016_session_invalid())
    ].

test_refusal_1011_auth_failed() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_FAILED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1011, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Authentication failed">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1012_auth_expired() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_EXPIRED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1012, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Authentication expired">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1013_invalid_credentials() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_INVALID_CREDENTIALS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1013, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Invalid credentials">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1014_authz_forbidden() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTHZ_FORBIDDEN),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1014, Code),
    ?assertEqual(?HTTP_403_FORBIDDEN, Status),
    ?assertEqual(<<"Authorization denied">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1015_auth_missing() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_MISSING),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1015, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Missing authentication">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1016_session_invalid() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SESSION_INVALID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1016, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Invalid session ID">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%%% Parameter & Validation Refusals (1021-1029)
%%%====================================================================

validation_refusal_test_() ->
    [
        ?_test(test_refusal_1021_invalid_params()),
        ?_test(test_refusal_1022_invalid_json_schema()),
        ?_test(test_refusal_1023_invalid_uri()),
        ?_test(test_refusal_1024_invalid_content_type()),
        ?_test(test_refusal_1025_invalid_header()),
        ?_test(test_refusal_1026_invalid_session_id()),
        ?_test(test_refusal_1027_invalid_protocol_version()),
        ?_test(test_refusal_1028_missing_required_field()),
        ?_test(test_refusal_1029_field_type_mismatch())
    ].

test_refusal_1021_invalid_params() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_PARAMS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1021, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid parameters">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1022_invalid_json_schema() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_JSON_SCHEMA),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1022, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"JSON schema validation failed">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1023_invalid_uri() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_URI),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1023, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid URI format">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1024_invalid_content_type() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_CONTENT_TYPE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1024, Code),
    ?assertEqual(?HTTP_415_UNSUPPORTED_MEDIA_TYPE, Status),
    ?assertEqual(<<"Invalid Content-Type">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1025_invalid_header() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_HEADER),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1025, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Required header missing or invalid">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1026_invalid_session_id() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_SESSION_ID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1026, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Session ID invalid">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1027_invalid_protocol_version() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_PROTOCOL_VERSION),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1027, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Protocol version not supported">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1028_missing_required_field() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_MISSING_REQUIRED_FIELD),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1028, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Required field missing">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1029_field_type_mismatch() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_FIELD_TYPE_MISMATCH),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1029, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Field type mismatch">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%%% Path & URI Security Refusals (1036-1040)
%%%====================================================================

security_refusal_test_() ->
    [
        ?_test(test_refusal_1036_path_traversal_detected()),
        ?_test(test_refusal_1037_path_invalid()),
        ?_test(test_refusal_1038_symlink_traversal_detected()),
        ?_test(test_refusal_1039_uri_out_of_bounds()),
        ?_test(test_refusal_1040_canonical_path_violation())
    ].

test_refusal_1036_path_traversal_detected() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PATH_TRAVERSAL_DETECTED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1036, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Path traversal detected">>, Message),
    ?assertEqual(critical, Severity).

test_refusal_1037_path_invalid() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PATH_INVALID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1037, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid path format">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1038_symlink_traversal_detected() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SYMLINK_TRAVERSAL_DETECTED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1038, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Symlink traversal detected">>, Message),
    ?assertEqual(critical, Severity).

test_refusal_1039_uri_out_of_bounds() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_URI_OUT_OF_BOUNDS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1039, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"URI out of bounds">>, Message),
    ?assertEqual(error, Severity).

test_refusal_1040_canonical_path_violation() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_CANONICAL_PATH_VIOLATION),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1040, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Canonical path violation">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%%% Resource & Entity Refusals (1046-1052)
%%%====================================================================

resource_refusal_test_() ->
    [
        ?_test(test_refusal_1046_resource_not_found()),
        ?_test(test_refusal_1047_resource_duplicate()),
        ?_test(test_refusal_1048_tool_not_found()),
        ?_test(test_refusal_1049_tool_duplicate()),
        ?_test(test_refusal_1050_prompt_not_found()),
        ?_test(test_refusal_1051_prompt_duplicate()),
        ?_test(test_refusal_1052_entity_duplicate())
    ].

test_refusal_1046_resource_not_found() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RESOURCE_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1046, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Resource not found">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1047_resource_duplicate() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RESOURCE_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1047, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Resource already exists">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1048_tool_not_found() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TOOL_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1048, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Tool not found">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1049_tool_duplicate() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TOOL_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1049, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Tool already registered">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1050_prompt_not_found() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PROMPT_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1050, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Prompt not found">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1051_prompt_duplicate() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PROMPT_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1051, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Prompt already exists">>, Message),
    ?assertEqual(warn, Severity).

test_refusal_1052_entity_duplicate() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_ENTITY_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1052, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Entity already exists">>, Message),
    ?assertEqual(warn, Severity).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Verify refusal metadata tuple structure
verify_metadata_format({Code, HttpStatus, Message, Hint, Severity}) ->
    ?assert(is_integer(Code)),
    ?assert(Code >= 1001 andalso Code =< 1089),
    ?assert(is_integer(HttpStatus)),
    ?assert(HttpStatus >= 400 andalso HttpStatus =< 599),
    ?assert(is_binary(Message)),
    ?assert(byte_size(Message) > 0),
    ?assert(is_binary(Hint)),
    ?assert(byte_size(Hint) > 0),
    ?assert(lists:member(Severity, [warn, error, critical])),
    ok.
