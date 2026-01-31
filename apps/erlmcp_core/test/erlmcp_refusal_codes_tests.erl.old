%%%-------------------------------------------------------------------
%%% @doc Comprehensive Refusal Code Test Suite
%%%
%%% Tests all refusal codes (1001-1089) defined in erlmcp_refusal.hrl
%%% following Chicago School TDD principles:
%%% - Test observable behavior through public API
%%% - No internal function testing
%%% - Real erlmcp_refusal module (no mocks)
%%%
%%% Coverage: 45 refusal codes across 9 categories
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal_codes_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_refusal.hrl").

%%%====================================================================
%% Test Helpers
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

%%%====================================================================
%% Queue & Backpressure Refusals (1001-1005)
%%%====================================================================

refusal_1001_queue_cap_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1001, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Queue capacity exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1002_queue_byte_cap_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_BYTE_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1002, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Byte capacity exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1003_tenant_cap_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUEUE_TENANT_CAP_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1003, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Tenant quota exceeded">>, Message),
    ?assertEqual(critical, Severity).

refusal_1004_buffer_overflow_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_BUFFER_OVERFLOW),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1004, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Buffer overflow">>, Message),
    ?assertEqual(critical, Severity).

refusal_1005_backpressure_active_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_BACKPRESSURE_ACTIVE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1005, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Backpressure active">>, Message),
    ?assertEqual(warn, Severity).

%%%====================================================================
%% Authentication & Authorization Refusals (1011-1016)
%%%====================================================================

refusal_1011_auth_failed_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_FAILED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1011, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Authentication failed">>, Message),
    ?assertEqual(error, Severity).

refusal_1012_auth_expired_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_EXPIRED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1012, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Authentication expired">>, Message),
    ?assertEqual(error, Severity).

refusal_1013_invalid_credentials_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_INVALID_CREDENTIALS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1013, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Invalid credentials">>, Message),
    ?assertEqual(error, Severity).

refusal_1014_authz_forbidden_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTHZ_FORBIDDEN),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1014, Code),
    ?assertEqual(?HTTP_403_FORBIDDEN, Status),
    ?assertEqual(<<"Authorization denied">>, Message),
    ?assertEqual(error, Severity).

refusal_1015_auth_missing_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_AUTH_MISSING),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1015, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Missing authentication">>, Message),
    ?assertEqual(error, Severity).

refusal_1016_session_invalid_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SESSION_INVALID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1016, Code),
    ?assertEqual(?HTTP_401_UNAUTHORIZED, Status),
    ?assertEqual(<<"Invalid session ID">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%% Parameter & Validation Refusals (1021-1029)
%%%====================================================================

refusal_1021_invalid_params_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_PARAMS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1021, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid parameters">>, Message),
    ?assertEqual(error, Severity).

refusal_1022_invalid_json_schema_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_JSON_SCHEMA),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1022, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"JSON schema validation failed">>, Message),
    ?assertEqual(error, Severity).

refusal_1023_invalid_uri_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_URI),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1023, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid URI format">>, Message),
    ?assertEqual(error, Severity).

refusal_1024_invalid_content_type_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_CONTENT_TYPE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1024, Code),
    ?assertEqual(?HTTP_415_UNSUPPORTED_MEDIA_TYPE, Status),
    ?assertEqual(<<"Invalid Content-Type">>, Message),
    ?assertEqual(error, Severity).

refusal_1025_invalid_header_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_HEADER),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1025, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Required header missing or invalid">>, Message),
    ?assertEqual(error, Severity).

refusal_1026_invalid_session_id_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_SESSION_ID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1026, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Session ID invalid">>, Message),
    ?assertEqual(error, Severity).

refusal_1027_invalid_protocol_version_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INVALID_PROTOCOL_VERSION),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1027, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Protocol version not supported">>, Message),
    ?assertEqual(error, Severity).

refusal_1028_missing_required_field_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_MISSING_REQUIRED_FIELD),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1028, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Required field missing">>, Message),
    ?assertEqual(error, Severity).

refusal_1029_field_type_mismatch_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_FIELD_TYPE_MISMATCH),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1029, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Field type mismatch">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%% Path & URI Security Refusals (1036-1040)
%%%====================================================================

refusal_1036_path_traversal_detected_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PATH_TRAVERSAL_DETECTED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1036, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Path traversal detected">>, Message),
    ?assertEqual(critical, Severity).

refusal_1037_path_invalid_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PATH_INVALID),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1037, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Invalid path format">>, Message),
    ?assertEqual(error, Severity).

refusal_1038_symlink_traversal_detected_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SYMLINK_TRAVERSAL_DETECTED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1038, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Symlink traversal detected">>, Message),
    ?assertEqual(critical, Severity).

refusal_1039_uri_out_of_bounds_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_URI_OUT_OF_BOUNDS),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1039, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"URI out of bounds">>, Message),
    ?assertEqual(error, Severity).

refusal_1040_canonical_path_violation_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_CANONICAL_PATH_VIOLATION),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1040, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Canonical path violation">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%% Resource & Entity Refusals (1046-1052)
%%%====================================================================

refusal_1046_resource_not_found_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RESOURCE_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1046, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Resource not found">>, Message),
    ?assertEqual(warn, Severity).

refusal_1047_resource_duplicate_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RESOURCE_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1047, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Resource already exists">>, Message),
    ?assertEqual(warn, Severity).

refusal_1048_tool_not_found_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TOOL_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1048, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Tool not found">>, Message),
    ?assertEqual(warn, Severity).

refusal_1049_tool_duplicate_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TOOL_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1049, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Tool already registered">>, Message),
    ?assertEqual(warn, Severity).

refusal_1050_prompt_not_found_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PROMPT_NOT_FOUND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1050, Code),
    ?assertEqual(?HTTP_404_NOT_FOUND, Status),
    ?assertEqual(<<"Prompt not found">>, Message),
    ?assertEqual(warn, Severity).

refusal_1051_prompt_duplicate_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PROMPT_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1051, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Prompt already exists">>, Message),
    ?assertEqual(warn, Severity).

refusal_1052_entity_duplicate_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_ENTITY_DUPLICATE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1052, Code),
    ?assertEqual(?HTTP_409_CONFLICT, Status),
    ?assertEqual(<<"Entity already exists">>, Message),
    ?assertEqual(warn, Severity).

%%%====================================================================
%% Rate Limiting & Throttling Refusals (1056-1060)
%%%====================================================================

refusal_1056_rate_limit_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RATE_LIMIT_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1056, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Rate limit exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1057_rate_limit_per_second_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RATE_LIMIT_PER_SECOND),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1057, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Per-second rate limit exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1058_rate_limit_per_minute_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RATE_LIMIT_PER_MINUTE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1058, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Per-minute rate limit exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1059_quota_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_QUOTA_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1059, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Quota exceeded">>, Message),
    ?assertEqual(error, Severity).

refusal_1060_concurrent_limit_exceeded_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_CONCURRENT_LIMIT_EXCEEDED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1060, Code),
    ?assertEqual(?HTTP_429_TOO_MANY_REQUESTS, Status),
    ?assertEqual(<<"Concurrent connection limit exceeded">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%% Protocol & Transport Refusals (1066-1070)
%%%====================================================================

refusal_1066_protocol_error_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_PROTOCOL_ERROR),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1066, Code),
    ?assertEqual(?HTTP_400_BAD_REQUEST, Status),
    ?assertEqual(<<"Protocol error">>, Message),
    ?assertEqual(error, Severity).

refusal_1067_transport_error_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TRANSPORT_ERROR),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1067, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Transport error">>, Message),
    ?assertEqual(error, Severity).

refusal_1068_message_too_large_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_MESSAGE_TOO_LARGE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1068, Code),
    ?assertEqual(?HTTP_413_PAYLOAD_TOO_LARGE, Status),
    ?assertEqual(<<"Message too large">>, Message),
    ?assertEqual(error, Severity).

refusal_1069_timeout_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_TIMEOUT),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1069, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Operation timeout">>, Message),
    ?assertEqual(warn, Severity).

refusal_1070_unsupported_encoding_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_UNSUPPORTED_ENCODING),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1070, Code),
    ?assertEqual(?HTTP_415_UNSUPPORTED_MEDIA_TYPE, Status),
    ?assertEqual(<<"Encoding not supported">>, Message),
    ?assertEqual(error, Severity).

%%%====================================================================
%% Server State Refusals (1076-1080)
%%%====================================================================

refusal_1076_server_uninitialized_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SERVER_UNINITIALIZED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1076, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Server not initialized">>, Message),
    ?assertEqual(error, Severity).

refusal_1077_server_shutting_down_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SERVER_SHUTTING_DOWN),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1077, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Server shutting down">>, Message),
    ?assertEqual(warn, Severity).

refusal_1078_service_unavailable_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_SERVICE_UNAVAILABLE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1078, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Service unavailable">>, Message),
    ?assertEqual(warn, Severity).

refusal_1079_internal_error_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_INTERNAL_ERROR),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1079, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Internal error">>, Message),
    ?assertEqual(critical, Severity).

refusal_1080_dependency_unavailable_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_DEPENDENCY_UNAVAILABLE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1080, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Dependency unavailable">>, Message),
    ?assertEqual(warn, Severity).

%%%====================================================================
%% Circuit Breaker & Health Refusals (1086-1089)
%%%====================================================================

refusal_1086_circuit_breaker_open_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_CIRCUIT_BREAKER_OPEN),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1086, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Circuit breaker open">>, Message),
    ?assertEqual(warn, Severity).

refusal_1087_health_check_failed_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_HEALTH_CHECK_FAILED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1087, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Health check failed">>, Message),
    ?assertEqual(warn, Severity).

refusal_1088_degraded_service_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_DEGRADED_SERVICE),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1088, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Service degraded">>, Message),
    ?assertEqual(warn, Severity).

refusal_1089_resource_exhausted_test() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(?REFUSAL_RESOURCE_EXHAUSTED),
    verify_metadata_format({Code, Status, Message, Hint, Severity}),
    ?assertEqual(1089, Code),
    ?assertEqual(?HTTP_503_SERVICE_UNAVAILABLE, Status),
    ?assertEqual(<<"Resource exhausted">>, Message),
    ?assertEqual(critical, Severity).

%%%====================================================================
%% API Function Tests
%%%====================================================================

get_message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_get_message_success()),
          ?_test(test_get_message_unknown_code())
         ]
     end}.

test_get_message_success() ->
    {ok, Message} = erlmcp_refusal:get_message(1001),
    ?assertEqual(<<"Queue capacity exceeded">>, Message).

test_get_message_unknown_code() ->
    Result = erlmcp_refusal:get_message(9999),
    ?assertEqual({error, unknown_code}, Result).

get_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_get_metadata_success()),
          ?_test(test_get_metadata_unknown_code())
         ]
     end}.

test_get_metadata_success() ->
    {ok, Code, Status, Message, Hint, Severity} = erlmcp_refusal:get_metadata(1001),
    ?assertEqual(1001, Code),
    ?assertEqual(429, Status),
    ?assertEqual(<<"Queue capacity exceeded">>, Message),
    ?assert(is_binary(Hint)),
    ?assert(byte_size(Hint) > 0),
    ?assertEqual(error, Severity).

test_get_metadata_unknown_code() ->
    Result = erlmcp_refusal:get_metadata(9999),
    ?assertEqual({error, unknown_code}, Result).

format_refusal_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_format_refusal_basic()),
          ?_test(test_format_refusal_unknown_code())
         ]
     end}.

test_format_refusal_basic() ->
    Formatted = erlmcp_refusal:format_refusal(1001),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Queue capacity exceeded">> =< Formatted).

test_format_refusal_unknown_code() ->
    Formatted = erlmcp_refusal:format_refusal(9999),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Unknown error code">> =< Formatted).

format_refusal_with_details_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_format_refusal_with_details_empty()),
          ?_test(test_format_refusal_with_details_populated())
         ]
     end}.

test_format_refusal_with_details_empty() ->
    Formatted = erlmcp_refusal:format_refusal(1001, #{}),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    %% Empty details should behave same as no details
    FormattedNoDetails = erlmcp_refusal:format_refusal(1001),
    ?assertEqual(FormattedNoDetails, Formatted).

test_format_refusal_with_details_populated() ->
    Details = #{<<"connection_id">> => <<"conn_123">>, <<"count">> => 1050},
    Formatted = erlmcp_refusal:format_refusal(1001, Details),
    ?assert(is_binary(Formatted)),
    ?assertNotEqual(0, byte_size(Formatted)),
    ?assert(<<"Queue capacity exceeded">> =< Formatted),
    ?assert(<<"Details">> =< Formatted).

%%%====================================================================
%% Category Classification Tests
%%%====================================================================

category_distribution_test() ->
    %% Verify all codes belong to expected categories
    %% Total defined codes: 51 (note: there are gaps in the 1001-1089 range)
    QueueCodes = [1001, 1002, 1003, 1004, 1005],
    AuthCodes = [1011, 1012, 1013, 1014, 1015, 1016],
    ValidationCodes = [1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029],
    SecurityCodes = [1036, 1037, 1038, 1039, 1040],
    ResourceCodes = [1046, 1047, 1048, 1049, 1050, 1051, 1052],
    RateLimitCodes = [1056, 1057, 1058, 1059, 1060],
    ProtocolCodes = [1066, 1067, 1068, 1069, 1070],
    ServerCodes = [1076, 1077, 1078, 1079, 1080],
    CircuitBreakerCodes = [1086, 1087, 1088, 1089],

    AllCodes = QueueCodes ++ AuthCodes ++ ValidationCodes ++ SecurityCodes ++
                ResourceCodes ++ RateLimitCodes ++ ProtocolCodes ++
                ServerCodes ++ CircuitBreakerCodes,

    %% Verify all codes return valid metadata
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(Code)
    end, AllCodes),

    %% Verify total count (51 defined codes, not all numbers in 1001-1089 range)
    ?assertEqual(51, length(AllCodes)).

severity_distribution_test() ->
    %% Verify severity levels are appropriate for categories
    CriticalCodes = [1003, 1004, 1036, 1038, 1079, 1089],
    WarnCodes = [1005, 1046, 1047, 1048, 1049, 1050, 1051, 1052,
                 1069, 1077, 1078, 1080, 1086, 1087, 1088],

    %% Verify critical codes
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, Severity} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(critical, Severity)
    end, CriticalCodes),

    %% Verify warn codes
    lists:foreach(fun(Code) ->
        {ok, _, _, _, _, Severity} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(warn, Severity)
    end, WarnCodes).

http_status_distribution_test() ->
    %% Verify HTTP status codes are appropriate for categories
    Status429Codes = [1001, 1002, 1003, 1056, 1057, 1058, 1059, 1060],
    Status401Codes = [1011, 1012, 1013, 1015, 1016],
    Status403Codes = [1014],
    Status404Codes = [1046, 1048, 1050],
    Status409Codes = [1047, 1049, 1051, 1052],
    Status413Codes = [1068],
    Status415Codes = [1024, 1070],
    Status503Codes = [1004, 1005, 1067, 1069, 1076, 1077, 1078, 1079,
                       1080, 1086, 1087, 1088, 1089],
    Status400Codes = [1021, 1022, 1023, 1025, 1026, 1027, 1028, 1029,
                       1036, 1037, 1038, 1039, 1040, 1066],

    %% Verify 429 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(429, Status)
    end, Status429Codes),

    %% Verify 401 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(401, Status)
    end, Status401Codes),

    %% Verify 403 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(403, Status)
    end, Status403Codes),

    %% Verify 404 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(404, Status)
    end, Status404Codes),

    %% Verify 409 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(409, Status)
    end, Status409Codes),

    %% Verify 413 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(413, Status)
    end, Status413Codes),

    %% Verify 415 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(415, Status)
    end, Status415Codes),

    %% Verify 503 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(503, Status)
    end, Status503Codes),

    %% Verify 400 codes
    lists:foreach(fun(Code) ->
        {ok, _, Status, _, _, _} = erlmcp_refusal:get_metadata(Code),
        ?assertEqual(400, Status)
    end, Status400Codes).

%%%====================================================================
%% Edge Case Tests
%%%====================================================================

edge_case_boundary_codes_test() ->
    %% Test first and last codes in ranges
    {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(1001), % First
    {ok, _, _, _, _, _} = erlmcp_refusal:get_metadata(1089), % Last
    ok.

edge_case_unknown_code_test() ->
    %% Test code outside range
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(1000)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(1090)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(0)),
    ?assertEqual({error, unknown_code}, erlmcp_refusal:get_metadata(-1)),
    ok.

edge_case_message_content_test() ->
    %% Verify all defined codes have non-empty binary messages
    %% Only check the 51 defined codes, not all numbers in 1001-1089 range
    DefinedCodes = [1001, 1002, 1003, 1004, 1005,
                    1011, 1012, 1013, 1014, 1015, 1016,
                    1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                    1036, 1037, 1038, 1039, 1040,
                    1046, 1047, 1048, 1049, 1050, 1051, 1052,
                    1056, 1057, 1058, 1059, 1060,
                    1066, 1067, 1068, 1069, 1070,
                    1076, 1077, 1078, 1079, 1080,
                    1086, 1087, 1088, 1089],
    ValidCodes = [C || C <- DefinedCodes,
        case erlmcp_refusal:get_metadata(C) of
            {ok, _, _, Message, _, _} when is_binary(Message), byte_size(Message) > 0 -> true;
            _ -> false
        end],
    ?assertEqual(51, length(ValidCodes)).

edge_case_hint_content_test() ->
    %% Verify all defined codes have non-empty binary hints
    %% Only check the 51 defined codes, not all numbers in 1001-1089 range
    DefinedCodes = [1001, 1002, 1003, 1004, 1005,
                    1011, 1012, 1013, 1014, 1015, 1016,
                    1021, 1022, 1023, 1024, 1025, 1026, 1027, 1028, 1029,
                    1036, 1037, 1038, 1039, 1040,
                    1046, 1047, 1048, 1049, 1050, 1051, 1052,
                    1056, 1057, 1058, 1059, 1060,
                    1066, 1067, 1068, 1069, 1070,
                    1076, 1077, 1078, 1079, 1080,
                    1086, 1087, 1088, 1089],
    ValidCodes = [C || C <- DefinedCodes,
        case erlmcp_refusal:get_metadata(C) of
            {ok, _, _, _, Hint, _} when is_binary(Hint), byte_size(Hint) > 0 -> true;
            _ -> false
        end],
    ?assertEqual(51, length(ValidCodes)).

%%%====================================================================
%% JSON-RPC Integration Tests
%%%====================================================================

jsonrpc_error_response_format_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(test_jsonrpc_error_response_structure()),
          ?_test(test_jsonrpc_error_with_refusal_code())
         ]
     end}.

test_jsonrpc_error_response_structure() ->
    %% Verify refusal codes can be used in JSON-RPC error responses
    {ok, Code, Status, Message, Hint, _Severity} = erlmcp_refusal:get_metadata(1001),
    ErrorObj = #{
        <<"code">> => Code,
        <<"message">> => Message,
        <<"data">> => #{
            <<"http_status">> => Status,
            <<"remediation_hint">> => Hint
        }
    },
    ?assertEqual(1001, maps:get(<<"code">>, ErrorObj)),
    ?assertEqual(<<"Queue capacity exceeded">>, maps:get(<<"message">>, ErrorObj)),
    ?assertEqual(429, maps:get(<<"http_status">>, maps:get(<<"data">>, ErrorObj))).

test_jsonrpc_error_with_refusal_code() ->
    %% Verify refusal codes integrate with JSON-RPC encoding
    {ok, Code, Status, Message, Hint, _Severity} = erlmcp_refusal:get_metadata(1048),
    JsonError = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message,
            <<"data">> => #{
                <<"http_status">> => Status,
                <<"remediation_hint">> => Hint,
                <<"refusal_code">> => Code
            }
        }
    },
    Encoded = jsx:encode(JsonError),
    Decoded = jsx:decode(Encoded, [return_maps]),
    ?assertEqual(<<"2.0">>, maps:get(<<"jsonrpc">>, Decoded)),
    ?assertEqual(1048, maps:get(<<"code">>, maps:get(<<"error">>, Decoded))),
    ?assertEqual(<<"Tool not found">>, maps:get(<<"message">>, maps:get(<<"error">>, Decoded))).

%%%====================================================================
%% Setup and Cleanup
%%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.
