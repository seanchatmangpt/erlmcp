%%%-------------------------------------------------------------------
%%% @doc erlmcp_request_signer_tests - MCP Request Signing Test Suite
%%%
%%% Tests for request/response signing:
%%% - Request signing and verification
%%% - Response signing and verification
%%% - Nonce replay protection
%%% - Timestamp validation
%%% - Signature tampering detection
%%%
%%% Chicago School TDD: Tests drive behavior, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_signer_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% @doc Setup request signer for tests
setup_signer() ->
    {ok, Pid} = erlmcp_request_signer:start_link(#{
        signature_ttl => 300,
        cleanup_interval => 1000
    }),
    Pid.

%% @doc Cleanup request signer
cleanup_signer(_Pid) ->
    erlmcp_request_signer:stop().

%%====================================================================
%% Request Signing Tests
%%====================================================================

%% @doc Sign and verify request
sign_verify_request_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_signing_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test_tool">>, <<"arguments">> => #{<<"arg1">> => <<"value1">>}},
                 RequestId = <<"req-123">>,

                 %% Sign request
                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 %% Verify request
                 CurrentTime = maps:get(timestamp, SignatureData),
                 Result = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual(ok, Result)
             end)
         ]
     end}.

%% @doc Detect tampered request
tampered_request_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test_tool">>},
                 RequestId = <<"req-456">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 %% Tamper with method
                 TamperedData = SignatureData#{method => <<"different_method">>},

                 CurrentTime = maps:get(timestamp, TamperedData),
                 Result = erlmcp_request_signer:verify_request(Key, TamperedData, RequestId, CurrentTime),
                 ?assertEqual({error, invalid_signature}, Result)
             end)
         ]
     end}.

%% @doc Detect wrong signing key
wrong_key_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key1 = <<"key1">>,
                 Key2 = <<"key2">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},
                 RequestId = <<"req-789">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key1, Method, Params, RequestId),

                 CurrentTime = maps:get(timestamp, SignatureData),
                 Result = erlmcp_request_signer:verify_request(Key2, SignatureData, RequestId, CurrentTime),
                 ?assertEqual({error, invalid_signature}, Result)
             end)
         ]
     end}.

%% @doc Detect expired request
expired_request_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},
                 RequestId = <<"req-expired">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 %% Use old timestamp (more than TTL seconds ago)
                 OldTime = maps:get(timestamp, SignatureData) - 400,
                 Result = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, OldTime, 300),
                 ?assertEqual({error, signature_expired}, Result)
             end)
         ]
     end}.

%%====================================================================
%% Nonce Replay Protection Tests
%%====================================================================

%% @doc Detect nonce reuse (replay attack)
nonce_reuse_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},
                 RequestId = <<"req-nonce">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 CurrentTime = maps:get(timestamp, SignatureData),

                 %% First verification should succeed
                 Result1 = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual(ok, Result1),

                 %% Second verification with same nonce should fail
                 Result2 = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual({error, nonce_reused}, Result2)
             end)
         ]
     end}.

%% @doc Different nonces for different requests
unique_nonces_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},

                 %% Sign two requests
                 {ok, Sig1} = erlmcp_request_signer:sign_request(Key, Method, Params, <<"req1">>),
                 {ok, Sig2} = erlmcp_request_signer:sign_request(Key, Method, Params, <<"req2">>),

                 %% Nonces should be different
                 Nonce1 = maps:get(nonce, Sig1),
                 Nonce2 = maps:get(nonce, Sig2),
                 ?assertNotEqual(Nonce1, Nonce2)
             end)
         ]
     end}.

%%====================================================================
%% Response Signing Tests
%%====================================================================

%% @doc Sign and verify response
sign_verify_response_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Result = <<"{\"status\": \"ok\"}">>,
                 RequestId = <<"req-123">>,

                 %% Sign response
                 {ok, SignatureData} = erlmcp_request_signer:sign_response(Key, Result, RequestId),

                 %% Verify response
                 CurrentTime = maps:get(timestamp, SignatureData),
                 Verified = erlmcp_request_signer:verify_response(Key, SignatureData, CurrentTime),
                 ?assertEqual(ok, Verified)
             end)
         ]
     end}.

%% @doc Detect tampered response
tampered_response_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Result = <<"{\"status\": \"ok\"}">>,
                 RequestId = <<"req-456">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_response(Key, Result, RequestId),

                 %% Tamper with result
                 TamperedData = SignatureData#{result => <<"{\"status\": \"error\"}">>},

                 CurrentTime = maps:get(timestamp, TamperedData),
                 Result = erlmcp_request_signer:verify_response(Key, TamperedData, CurrentTime),
                 ?assertEqual({error, invalid_signature}, Result)
             end)
         ]
     end}.

%%====================================================================
%% Signature Format Tests
%%====================================================================

%% @doc Validate signature data structure
signature_data_structure_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},
                 RequestId = <<"req-struct">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 %% Check required fields
                 ?assert(maps:is_key(method, SignatureData)),
                 ?assert(maps:is_key(params, SignatureData)),
                 ?assert(maps:is_key(timestamp, SignatureData)),
                 ?assert(maps:is_key(nonce, SignatureData)),
                 ?assert(maps:is_key(signature, SignatureData)),

                 %% Check types
                 ?assert(is_binary(maps:get(method, SignatureData))),
                 ?assert(is_map(maps:get(params, SignatureData))),
                 ?assert(is_integer(maps:get(timestamp, SignatureData))),
                 ?assert(is_binary(maps:get(nonce, SignatureData))),
                 ?assert(is_binary(maps:get(signature, SignatureData)))
             end)
         ]
     end}.

%% @doc Signature is base64-encoded
signature_base64_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{<<"name">> => <<"test">>},
                 RequestId = <<"req-base64">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),
                 Signature = maps:get(signature, SignatureData),

                 %% Should be valid base64
                 ?assertMatch({ok, _}, base64:decode(Signature))
             end)
         ]
     end}.

%%====================================================================
%% Configuration Tests
%%====================================================================

%% @doc Set and get signature TTL
signature_ttl_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 %% Default TTL
                 DefaultTTL = erlmcp_request_signer:get_signature_ttl(),
                 ?assertEqual(300, DefaultTTL),

                 %% Set new TTL
                 ok = erlmcp_request_signer:set_signature_ttl(600),
                 NewTTL = erlmcp_request_signer:get_signature_ttl(),
                 ?assertEqual(600, NewTTL)
             end)
         ]
     end}.

%%====================================================================
%% Nonce Management Tests
%%====================================================================

%% @doc Generate and validate nonce
nonce_generate_validate_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 %% Generate nonce
                 Nonce = erlmcp_request_signer:generate_nonce(),
                 ?assertEqual(12, byte_size(Nonce)),

                 %% Validate nonce (first time)
                 ?assert(erlmcp_request_signer:validate_nonce(Nonce))
             end)
         ]
     end}.

%% @doc Cleanup expired nonces
cleanup_nonces_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 %% Mark nonces as used
                 Nonce1 = erlmcp_request_signer:generate_nonce(),
                 Nonce2 = erlmcp_request_signer:generate_nonce(),

                 ok = erlmcp_request_signer:mark_nonce_used(Nonce1),
                 ok = erlmcp_request_signer:mark_nonce_used(Nonce2),

                 %% Cleanup
                 ok = erlmcp_request_signer:cleanup_expired_nonces(),

                 %% Should still be valid (not expired yet)
                 ?assert(erlmcp_request_signer:validate_nonce(Nonce1))
             end)
         ]
     end}.

%%====================================================================
%% Edge Cases
%%====================================================================

%% @doc Handle empty params
empty_params_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{},
                 RequestId = <<"req-empty">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 CurrentTime = maps:get(timestamp, SignatureData),
                 Result = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual(ok, Result)
             end)
         ]
     end}.

%% @doc Handle nested params
nested_params_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{
                     <<"name">> => <<"test">>,
                     <<"arguments">> => #{
                         <<"nested1">> => <<"value1">>,
                         <<"nested2">> => #{
                             <<"deep">> => <<"value2">>
                         }
                     }
                 },
                 RequestId = <<"req-nested">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 CurrentTime = maps:get(timestamp, SignatureData),
                 Result = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual(ok, Result)
             end)
         ]
     end}.

%% @doc Handle array params
array_params_test_() ->
    {setup,
     fun setup_signer/0,
     fun cleanup_signer/1,
     fun(_Pid) ->
         [
             ?_test(begin
                 Key = <<"test_key">>,
                 Method = <<"tools/call">>,
                 Params = #{
                     <<"items">> => [<<"item1">>, <<"item2">>, <<"item3">>]
                 },
                 RequestId = <<"req-array">>,

                 {ok, SignatureData} = erlmcp_request_signer:sign_request(Key, Method, Params, RequestId),

                 CurrentTime = maps:get(timestamp, SignatureData),
                 Result = erlmcp_request_signer:verify_request(Key, SignatureData, RequestId, CurrentTime),
                 ?assertEqual(ok, Result)
             end)
         ]
     end}.
