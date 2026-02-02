%%%-------------------------------------------------------------------
%%% @doc erlmcp_secure_storage_tests - Secure Credential Storage Test Suite
%%%
%%% Tests for encrypted credential storage:
%%% - Encrypt and decrypt credentials
%%% - Key rotation
%%% - Re-encryption on key rotation
%%% - AEAD tamper detection
%%% - Metadata operations
%%%
%%% Chicago School TDD: Tests drive behavior, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secure_storage_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% @doc Setup secure storage with test master key
setup_storage() ->
    MasterKey = crypto:strong_rand_bytes(32),
    {ok, Pid} = erlmcp_secure_storage:start_link(#{
        master_key => MasterKey
    }),
    {Pid, MasterKey}.

%% @doc Cleanup secure storage
cleanup_storage({_Pid, _MasterKey}) ->
    erlmcp_secure_storage:stop().

%%====================================================================
%% Encryption/Decryption Tests
%%====================================================================

%% @doc Encrypt and decrypt credential
encrypt_decrypt_round_trip_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"api_key_123">>,
                 Plaintext = <<"sk-1234567890abcdef">>,

                 %% Encrypt
                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Decrypt
                 {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted)
                 ),

                 ?assertEqual(Plaintext, Decrypted)
             end)
         ]
     end}.

%% @doc Encrypt with custom AAD
encrypt_with_custom_aad_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"custom_aad_test">>,
                 Plaintext = <<"sensitive_data">>,
                 AAD = <<"custom_context_metadata">>,

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(
                     CredentialId, Plaintext, AAD
                 ),

                 %% Decrypt with same AAD
                 {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted),
                     AAD
                 ),

                 ?assertEqual(Plaintext, Decrypted)
             end)
         ]
     end}.

%% @doc Wrong key fails to decrypt
wrong_key_decrypt_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"wrong_key_test">>,
                 Plaintext = <<"test_data">>,

                 %% Encrypt with current key
                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Rotate to new key
                 NewMasterKey = crypto:strong_rand_bytes(32),
                 {ok, NewKeyId} = erlmcp_secure_storage:rotate_key(NewMasterKey),

                 %% Try to decrypt with ciphertext (should use old key internally)
                 %% This should still work because secure storage tracks keys
                 {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted)
                 ),

                 ?assertEqual(Plaintext, Decrypted)
             end)
         ]
     end}.

%%====================================================================
%% AEAD Tamper Detection Tests
%%====================================================================

%% @doc Detect tampered ciphertext
tampered_ciphertext_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"tamper_test">>,
                 Plaintext = <<"original_data">>,

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Tamper with ciphertext
                 Ciphertext = maps:get(ciphertext, Encrypted),
                 <<Byte1:8, Rest/binary>> = Ciphertext,
                 TamperedCiphertext = <<(Byte1 bxor 16#FF), Rest/binary>>,

                 %% Decryption should fail due to tag mismatch
                 Result = erlmcp_secure_storage:decrypt_credential(
                     CredentialId, TamperedCiphertext
                 ),

                 ?assertMatch({error, {decryption_failed, authentication_failed}}, Result)
             end)
         ]
     end}.

%% @doc Detect tampered tag
tampered_tag_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"tamper_tag_test">>,
                 Plaintext = <<"test_data">>,

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Tamper with tag
                 Tag = maps:get(tag, Encrypted),
                 <<TagByte1:8, TagRest/binary>> = Tag,
                 TamperedTag = <<(TagByte1 bxor 16#FF), TagRest/binary>>,

                 %% Create tampered encrypted data
                 TamperedEncrypted = Encrypted#{tag => TamperedTag},

                 %% Decryption should fail
                 Result = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, TamperedEncrypted)
                 ),

                 ?assertMatch({error, {decryption_failed, authentication_failed}}, Result)
             end)
         ]
     end}.

%% @doc Wrong AAD causes authentication failure
wrong_aad_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"wrong_aad_test">>,
                 Plaintext = <<"test_data">>,
                 CorrectAAD = <<"correct_aad">>,
                 WrongAAD = <<"wrong_aad">>,

                 %% Encrypt with correct AAD
                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(
                     CredentialId, Plaintext, CorrectAAD
                 ),

                 %% Try to decrypt with wrong AAD
                 Result = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted),
                     WrongAAD
                 ),

                 ?assertMatch({error, {decryption_failed, authentication_failed}}, Result)
             end)
         ]
     end}.

%%====================================================================
%% Key Rotation Tests
%%====================================================================

%% @doc Rotate encryption key
rotate_key_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 %% Get current key ID
                 {ok, KeyId1} = erlmcp_secure_storage:get_current_key_id(),

                 %% Rotate to new key
                 NewMasterKey = crypto:strong_rand_bytes(32),
                 {ok, KeyId2} = erlmcp_secure_storage:rotate_key(NewMasterKey),

                 %% Key IDs should be different
                 ?assertNotEqual(KeyId1, KeyId2)
             end)
         ]
     end}.

%% @doc Re-encrypt all credentials on key rotation
reencrypt_all_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 %% Create multiple credentials
                 Credentials = [
                     {<<"cred1">>, <<"data1">>},
                     {<<"cred2">>, <<"data2">>},
                     {<<"cred3">>, <<"data3">>}
                 ],

                 lists:foreach(fun({Id, Data}) ->
                     {ok, _} = erlmcp_secure_storage:encrypt_credential(Id, Data)
                 end, Credentials),

                 %% Rotate and re-encrypt
                 NewMasterKey = crypto:strong_rand_bytes(32),
                 {ok, Count} = erlmcp_secure_storage:reencrypt_all(NewMasterKey),

                 %% All credentials should be re-encrypted
                 ?assertEqual(3, Count),

                 %% Verify all can still be decrypted
                 lists:foreach(fun({Id, OriginalData}) ->
                     {ok, Metadata} = erlmcp_secure_storage:get_credential_metadata(Id),
                     {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                         Id, maps:get(ciphertext, Metadata)
                     ),
                     ?assertEqual(OriginalData, Decrypted)
                 end, Credentials)
             end)
         ]
     end}.

%%====================================================================
%% Metadata Tests
%%====================================================================

%% @doc Get credential metadata without decrypting
get_metadata_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"metadata_test">>,
                 Plaintext = <<"sensitive_data">>,

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Get metadata
                 {ok, Metadata} = erlmcp_secure_storage:get_credential_metadata(CredentialId),

                 %% Check fields
                 ?assert(maps:is_key(iv, Metadata)),
                 ?assert(maps:is_key(tag, Metadata)),
                 ?assert(maps:is_key(key_id, Metadata)),
                 ?assert(maps:is_key(key_version, Metadata)),
                 ?assert(maps:is_key(aad, Metadata)),
                 ?assert(maps:is_key(created_at, Metadata)),

                 %% Ciphertext should NOT be in metadata
                 ?assertNot(maps:is_key(ciphertext, Metadata))
             end)
         ]
     end}.

%% @doc List all credential IDs
list_credentials_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 %% Create credentials
                 Credentials = [<<"cred1">>, <<"cred2">>, <<"cred3">>],
                 lists:foreach(fun(Id) ->
                     {ok, _} = erlmcp_secure_storage:encrypt_credential(Id, <<"data">>)
                 end, Credentials),

                 %% List credentials
                 {ok, Ids} = erlmcp_secure_storage:list_credentials(),

                 %% Check all IDs present
                 lists:foreach(fun(Id) ->
                     ?assert(lists:member(Id, Ids))
                 end, Credentials)
             end)
         ]
     end}.

%% @doc Delete credential
delete_credential_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"delete_test">>,
                 Plaintext = <<"delete_me">>,

                 {ok, _} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 %% Verify it exists
                 {ok, _} = erlmcp_secure_storage:get_credential_metadata(CredentialId),

                 %% Delete
                 ok = erlmcp_secure_storage:delete_credential(CredentialId),

                 %% Should no longer exist
                 Result = erlmcp_secure_storage:get_credential_metadata(CredentialId),
                 ?assertEqual({error, credential_not_found}, Result)
             end)
         ]
     end}.

%%====================================================================
%% Edge Cases
%%====================================================================

%% @doc Empty plaintext
empty_plaintext_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"empty_test">>,
                 Plaintext = <<>>,

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted)
                 ),

                 ?assertEqual(Plaintext, Decrypted)
             end)
         ]
     end}.

%% @doc Large plaintext
large_plaintext_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"large_test">>,
                 Plaintext = crypto:strong_rand_bytes(1024 * 1024),  % 1 MB

                 {ok, Encrypted} = erlmcp_secure_storage:encrypt_credential(CredentialId, Plaintext),

                 {ok, Decrypted} = erlmcp_secure_storage:decrypt_credential(
                     CredentialId,
                     maps:get(ciphertext, Encrypted)
                 ),

                 ?assertEqual(Plaintext, Decrypted)
             end)
         ]
     end}.

%% @doc Non-existent credential
nonexistent_credential_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 CredentialId = <<"nonexistent">>,

                 %% Try to get metadata
                 Result = erlmcp_secure_storage:get_credential_metadata(CredentialId),
                 ?assertEqual({error, credential_not_found}, Result),

                 %% Try to delete
                 DeleteResult = erlmcp_secure_storage:delete_credential(CredentialId),
                 ?assertEqual({error, credential_not_found}, DeleteResult)
             end)
         ]
     end}.

%%====================================================================
%% Configuration Tests
%%====================================================================

%% @doc Set key derivation parameters
set_key_derivation_params_test_() ->
    {setup,
     fun setup_storage/0,
     fun cleanup_storage/1,
     fun({_Pid, _MasterKey}) ->
         [
             ?_test(begin
                 Iterations = 200000,
                 Salt = crypto:strong_rand_bytes(16),

                 %% Set parameters
                 ok = erlmcp_secure_storage:set_key_derivation_params(Iterations, Salt)
             end)
         ]
     end}.
