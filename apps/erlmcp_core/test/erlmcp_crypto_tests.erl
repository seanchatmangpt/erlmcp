%%%-------------------------------------------------------------------
%%% @doc erlmcp_crypto_tests - OTP 26-28 Crypto Test Suite
%%%
%%% Tests for modern cryptographic functions:
%%% - SHA-3 hashing (OTP 27)
%%% - HMAC for signing (OTP 26 improvements)
%%% - AEAD encryption (OTP 27)
%%% - Token generation
%%% - Key derivation (PBKDF2)
%%%
%%% Chicago School TDD: Tests drive behavior, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_crypto_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% SHA-3 Hashing Tests (OTP 27)
%%====================================================================

%% @doc SHA-3/256 hash test with known vectors
sha3_256_test() ->
    %% NIST test vectors for SHA3-256
    Input1 = <<"">>,
    Expected1 = hex_to_bin("a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"),
    ?assertEqual(Expected1, erlmcp_crypto:sha3_256(Input1)),

    Input2 = <<"abc">>,
    Expected2 = hex_to_bin("3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"),
    ?assertEqual(Expected2, erlmcp_crypto:sha3_256(Input2)),

    %% Test consistency
    Data = crypto:strong_rand_bytes(32),
    Hash1 = erlmcp_crypto:sha3_256(Data),
    Hash2 = erlmcp_crypto:sha3_256(Data),
    ?assertEqual(Hash1, Hash2).

%% @doc SHA-3/512 hash test with known vectors
sha3_512_test() ->
    %% NIST test vectors for SHA3-512
    Input1 = <<"">>,
    Expected1 = hex_to_bin("a69f73cca23a9ac5c8b567dc185a756e97c982164fe25859e0d1dcc1475c80a615b2123af1f5f94c11e3e9402c3ac558f500199d95b6d3e301758586281dcd26"),
    ?assertEqual(Expected1, erlmcp_crypto:sha3_512(Input1)),

    Input2 = <<"abc">>,
    Expected2 = hex_to_bin("4756da1192850bc58776f4f6a684be2be9c0672c9e9c51c8e5dbdc75c6e25d35574ae8bf153828285b84a4de061cb2b1922f3e64fb66d092b097ce36a7a7a0f4"),
    ?assertEqual(Expected2, erlmcp_crypto:sha3_512(Input2)).

%%====================================================================
%% SHA-2 Hashing Tests (Legacy)
%%====================================================================

%% @doc SHA-256 hash test
sha256_test() ->
    Data = <<"test data for sha256">>,
    Hash = erlmcp_crypto:sha256(Data),
    ?assertEqual(32, byte_size(Hash)),

    %% Test consistency
    Hash2 = erlmcp_crypto:sha256(Data),
    ?assertEqual(Hash, Hash2).

%% @doc SHA-512 hash test
sha512_test() ->
    Data = <<"test data for sha512">>,
    Hash = erlmcp_crypto:sha512(Data),
    ?assertEqual(64, byte_size(Hash)),

    %% Test consistency
    Hash2 = erlmcp_crypto:sha512(Data),
    ?assertEqual(Hash, Hash2).

%%====================================================================
%% HMAC Tests (OTP 26 improvements)
%%====================================================================

%% @doc HMAC-SHA256 test with known vectors
hmac_sha256_test() ->
    %% RFC 4868 test vectors
    Key = hex_to_bin("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b"),
    Data = <<"Hi There">>,
    Expected = hex_to_bin("b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7"),
    ?assertEqual(Expected, erlmcp_crypto:hmac_sha256(Key, Data)),

    %% Test with different data
    Key2 = <<"key">>,
    Data2 = <<"The quick brown fox jumps over the lazy dog">>,
    HMAC = erlmcp_crypto:hmac_sha256(Key2, Data2),
    ?assertEqual(32, byte_size(HMAC)),

    %% Test consistency
    HMAC2 = erlmcp_crypto:hmac_sha256(Key2, Data2),
    ?assertEqual(HMAC, HMAC2).

%% @doc HMAC-SHA512 test
hmac_sha512_test() ->
    Key = <<"test_key">>,
    Data = <<"test data for hmac">>,
    HMAC = erlmcp_crypto:hmac_sha512(Key, Data),
    ?assertEqual(64, byte_size(HMAC)),

    %% Test consistency
    HMAC2 = erlmcp_crypto:hmac_sha512(Key, Data),
    ?assertEqual(HMAC, HMAC2).

%% @doc HMAC-SHA3-256 test (OTP 27+)
hmac_sha3_256_test() ->
    Key = <<"test_key">>,
    Data = <<"test data for hmac-sha3">>,
    HMAC = erlmcp_crypto:hmac_sha3_256(Key, Data),
    ?assertEqual(32, byte_size(HMAC)),

    %% Test consistency
    HMAC2 = erlmcp_crypto:hmac_sha3_256(Key, Data),
    ?assertEqual(HMAC, HMAC2).

%%====================================================================
%% AEAD Encryption Tests (OTP 27)
%%====================================================================

%% @doc AEAD encryption/decryption round-trip
encrypt_aead_round_trip_test() ->
    Plaintext = <<"Sensitive data that needs encryption">>,
    Key = crypto:strong_rand_bytes(32),
    IV = crypto:strong_rand_bytes(12),
    AAD = <<"additional authenticated data">>,

    %% Encrypt
    {ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),
    ?assertNotEqual(Plaintext, Ciphertext),

    %% Decrypt
    {ok, Decrypted} = erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD),
    ?assertEqual(Plaintext, Decrypted).

%% @doc AEAD authentication tag verification
encrypt_aead_tag_verification_test() ->
    Plaintext = <<"Test data">>,
    Key = crypto:strong_rand_bytes(32),
    IV = crypto:strong_rand_bytes(12),
    AAD = <<"metadata">>,

    {ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),

    %% Correct tag should work
    {ok, Decrypted} = erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD),
    ?assertEqual(Plaintext, Decrypted),

    %% Wrong tag should fail
    WrongTag = crypto:strong_rand_bytes(16),
    ?assertEqual({error, authentication_failed},
                 erlmcp_crypto:decrypt_aead(Ciphertext, WrongTag, Key, IV, AAD)).

%% @doc AEAD with tampered ciphertext
encrypt_aead_tamper_test() ->
    Plaintext = <<"Original data">>,
    Key = crypto:strong_rand_bytes(32),
    IV = crypto:strong_rand_bytes(12),
    AAD = <<"metadata">>,

    {ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),

    %% Tamper with ciphertext
    <<FirstByte:8, Rest/binary>> = Ciphertext,
    TamperedCiphertext = <<(FirstByte bxor 16#FF), Rest/binary>>,

    %% Decryption should fail due to tag mismatch
    ?assertEqual({error, authentication_failed},
                 erlmcp_crypto:decrypt_aead(TamperedCiphertext, Tag, Key, IV, AAD)).

%% @doc AEAD with wrong AAD
encrypt_aead_wrong_aad_test() ->
    Plaintext = <<"Test data">>,
    Key = crypto:strong_rand_bytes(32),
    IV = crypto:strong_rand_bytes(12),
    AAD = <<"correct_aad">>,

    {ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),

    %% Wrong AAD should cause authentication failure
    WrongAAD = <<"wrong_aad">>,
    ?assertEqual({error, authentication_failed},
                 erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, WrongAAD)).

%% @doc AEAD empty plaintext
encrypt_aead_empty_test() ->
    Plaintext = <<>>,
    Key = crypto:strong_rand_bytes(32),
    IV = crypto:strong_rand_bytes(12),
    AAD = <<"metadata">>,

    {ok, Ciphertext, Tag} = erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD),
    {ok, Decrypted} = erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD),
    ?assertEqual(Plaintext, Decrypted).

%%====================================================================
%% Token Generation Tests
%%====================================================================

%% @doc Generate default 32-byte token
generate_token_test() ->
    Token = erlmcp_crypto:generate_token(),
    ?assertEqual(32, byte_size(Token)),

    %% Tokens should be unique
    Token2 = erlmcp_crypto:generate_token(),
    ?assertNotEqual(Token, Token2).

%% @doc Generate custom size token
generate_token_custom_size_test() ->
    Token16 = erlmcp_crypto:generate_token(16),
    ?assertEqual(16, byte_size(Token16)),

    Token64 = erlmcp_crypto:generate_token(64),
    ?assertEqual(64, byte_size(Token64)),

    %% Test uniqueness
    Token16_2 = erlmcp_crypto:generate_token(16),
    ?assertNotEqual(Token16, Token16_2).

%% @doc Token generation size constraints
generate_token_invalid_size_test() ->
    %% Size must be positive and <= 1024
    ?assertError(_, erlmcp_crypto:generate_token(0)),
    ?assertError(_, erlmcp_crypto:generate_token(1025)).

%%====================================================================
%% Nonce Tests
%%====================================================================

%% @doc Generate 12-byte nonce (suitable for AES-GCM)
generate_nonce_test() ->
    Nonce = erlmcp_crypto:generate_nonce(),
    ?assertEqual(12, byte_size(Nonce)),

    %% Nonces should be unique
    Nonce2 = erlmcp_crypto:generate_nonce(),
    ?assertNotEqual(Nonce, Nonce2).

%% @doc Validate nonce format
validate_nonce_test() ->
    %% Correct size nonce
    Nonce12 = crypto:strong_rand_bytes(12),
    ?assert(erlmcp_crypto:validate_nonce(Nonce12)),

    %% Wrong size nonce
    Nonce8 = crypto:strong_rand_bytes(8),
    ?assertNot(erlmcp_crypto:validate_nonce(Nonce8)),

    Nonce16 = crypto:strong_rand_bytes(16),
    ?assertNot(erlmcp_crypto:validate_nonce(Nonce16)).

%%====================================================================
%% Key Derivation Tests (PBKDF2)
%%====================================================================

%% @doc PBKDF2 key derivation
derive_key_test() ->
    Password = <<"password123">>,
    Salt = crypto:strong_rand_bytes(16),
    KeyLength = 32,

    Key = erlmcp_crypto:derive_key(Password, Salt, KeyLength),
    ?assertEqual(KeyLength, byte_size(Key)),

    %% Same inputs should produce same key
    Key2 = erlmcp_crypto:derive_key(Password, Salt, KeyLength),
    ?assertEqual(Key, Key2).

%% @doc PBKDF2 with different iterations
derive_key_iterations_test() ->
    Password = <<"password123">>,
    Salt = crypto:strong_rand_bytes(16),
    KeyLength = 32,

    Key1 = erlmcp_crypto:derive_key(Password, Salt, KeyLength, 1000),
    Key2 = erlmcp_crypto:derive_key(Password, Salt, KeyLength, 10000),

    %% Different iterations should produce different keys
    ?assertNotEqual(Key1, Key2).

%% @doc PBKDF2 with different salts
derive_key_salt_test() ->
    Password = <<"password123">>,
    Salt1 = crypto:strong_rand_bytes(16),
    Salt2 = crypto:strong_rand_bytes(16),
    KeyLength = 32,

    Key1 = erlmcp_crypto:derive_key(Password, Salt1, KeyLength),
    Key2 = erlmcp_crypto:derive_key(Password, Salt2, KeyLength),

    %% Different salts should produce different keys
    ?assertNotEqual(Key1, Key2).

%%====================================================================
%% Utility Tests
%%====================================================================

%% @doc Constant-time comparison
constant_time_compare_test() ->
    %% Equal values
    ?assert(erlmcp_crypto:constant_time_compare(<<"test">>, <<"test">>)),

    %% Different values
    ?assertNot(erlmcp_crypto:constant_time_compare(<<"test1">>, <<"test2">>)),

    %% Different lengths
    ?assertNot(erlmcp_crypto:constant_time_compare(<<"test">>, <<"test123">>)),

    %% Empty binaries
    ?assert(erlmcp_crypto:constant_time_compare(<<>>, <<>>)).

%% @doc Secure zero memory
secure_zero_test() ->
    Data = crypto:strong_rand_bytes(32),
    Zeroed = erlmcp_crypto:secure_zero(Data),
    ?assertEqual(<<0:256>>, Zeroed),
    ?assertEqual(byte_size(Data), byte_size(Zeroed)).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Convert hex string to binary
hex_to_bin(Hex) ->
    hex_to_bin(Hex, <<>>).

hex_to_bin(<<>>, Acc) ->
    Acc;
hex_to_bin(<<H1:8, H2:8, Rest/binary>>, Acc) ->
    Val1 = hex_digit_to_val(H1),
    Val2 = hex_digit_to_val(H2),
    hex_to_bin(Rest, <<Acc/binary, (Val1 bsl 4 + Val2):8>>).

hex_digit_to_val(C) when $0 =< C, C =< $9 -> C - $0;
hex_digit_to_val(C) when $A =< C, C =< $F -> C - $A + 10;
hex_digit_to_val(C) when $a =< C, C =< $f -> C - $a + 10.
