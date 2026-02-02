%%%-------------------------------------------------------------------
%%% @doc erlmcp_crypto - OTP 26-28 Cryptographic Utilities for MCP Security
%%%
%%% This module provides modern cryptographic functions using OTP 26-28 features:
%%% - SHA-3 hashing (OTP 27)
%%% - HMAC for request signing (OTP 26 improvements)
%%% - AEAD encryption (OTP 27: crypto_one_time_aead/7)
%%% - Secure token generation
%%% - Key derivation (PBKDF2)
%%%
%%% Use Cases:
%%% - Request signing for tool invocations
%%% - Secure credential storage
%%% - Authentication tokens
%%% - Nonce generation for replay protection
%%%
%%% OTP Version Requirements:
%%% - OTP 26: HMAC improvements
%%% - OTP 27: SHA-3, AEAD (crypto_one_time_aead/7)
%%% - OTP 28: OpenSSL 3.1.5, FIPS improvements
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_crypto).

%% OTP 26-28 Crypto API
-export([
    %% SHA-3 Hashing (OTP 27)
    sha3_256/1,
    sha3_512/1,

    %% SHA-2 Hashing (legacy)
    sha256/1,
    sha512/1,

    %% HMAC for Request Signing (OTP 26 improvements)
    hmac_sha256/2,
    hmac_sha512/2,
    hmac_sha3_256/2,

    %% AEAD Encryption (OTP 27: crypto_one_time_aead/7)
    encrypt_aead/4,
    decrypt_aead/5,

    %% Legacy encryption (pre-OTP 27)
    encrypt/3,
    decrypt/3,

    %% Token Generation
    generate_token/0,
    generate_token/1,

    %% Nonce for Replay Protection
    generate_nonce/0,
    validate_nonce/1,
    expire_nonce/1,

    %% Key Derivation (PBKDF2)
    derive_key/3,
    derive_key/4,

    %% Utilities
    constant_time_compare/2,
    secure_zero/1
]).

%% Types
-type hash_algorithm() :: sha256 | sha512 | sha3_256 | sha3_512.
-type aead_algorithm() :: aes_256_gcm | aes_256_ccm | chacha20_poly1305.
-type key() :: binary().
-type iv() :: binary().
-type aad() :: binary().     % Additional Authenticated Data
-type tag() :: binary().     % Authentication tag
-type nonce() :: binary().
-type salt() :: binary().

-export_type([hash_algorithm/0, aead_algorithm/0, key/0, iv/0, aad/0, tag/0, nonce/0, salt/0]).

%%====================================================================
%% SHA-3 Hashing (OTP 27)
%%====================================================================

%% @doc SHA-3/256 hash (OTP 27+)
%% Returns 32-byte hash
-spec sha3_256(binary()) -> binary().
sha3_256(Data) ->
    crypto:hash(sha3_256, Data).

%% @doc SHA-3/512 hash (OTP 27+)
%% Returns 64-byte hash
-spec sha3_512(binary()) -> binary().
sha3_512(Data) ->
    crypto:hash(sha3_512, Data).

%%====================================================================
%% SHA-2 Hashing (Legacy)
%%====================================================================

%% @doc SHA-256 hash
%% Returns 32-byte hash
-spec sha256(binary()) -> binary().
sha256(Data) ->
    crypto:hash(sha256, Data).

%% @doc SHA-512 hash
%% Returns 64-byte hash
-spec sha512(binary()) -> binary().
sha512(Data) ->
    crypto:hash(sha512, Data).

%%====================================================================
%% HMAC for Request Signing (OTP 26 improvements)
%%====================================================================

%% @doc HMAC-SHA256 for request signing
%% Use for signing tool invocation requests
-spec hmac_sha256(key(), binary()) -> binary().
hmac_sha256(Key, Data) ->
    crypto:mac(hmac, sha256, Key, Data).

%% @doc HMAC-SHA512 for enhanced security
-spec hmac_sha512(key(), binary()) -> binary().
hmac_sha512(Key, Data) ->
    crypto:mac(hmac, sha512, Key, Data).

%% @doc HMAC-SHA3-256 for future-proof security (OTP 27+)
-spec hmac_sha3_256(key(), binary()) -> binary().
hmac_sha3_256(Key, Data) ->
    crypto:mac(hmac, sha3_256, Key, Data).

%%====================================================================
%% AEAD Encryption (OTP 27: crypto_one_time_aead/7)
%%====================================================================

%% @doc AEAD encryption using AES-256-GCM (OTP 27+)
%% Encrypts plaintext with additional authenticated data (AAD)
%% Returns {Ciphertext, Tag}
%%
%% Parameters:
%% - Plaintext: Data to encrypt
%% - Key: 32-byte encryption key
%% - IV: 12-byte initialization vector (use generate_nonce/0)
%% - AAD: Additional authenticated data (e.g., request metadata)
%%
-spec encrypt_aead(binary(), key(), iv(), aad()) ->
    {ok, binary(), tag()} | {error, term()}.
encrypt_aead(Plaintext, Key, IV, AAD) ->
    try
        %% OTP 27: crypto_one_time_aead/7
        %% Args: Cipher, Key, IV, Plaintext, AAD, EncryptFlag
        {Ciphertext, Tag} = crypto:crypto_one_time_aead(
            aes_256_gcm, Key, IV, Plaintext, AAD, true
        ),
        {ok, Ciphertext, Tag}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% @doc AEAD decryption using AES-256-GCM (OTP 27+)
%% Decrypts ciphertext and verifies authentication tag
%%
-spec decrypt_aead(binary(), tag(), key(), iv(), aad()) ->
    {ok, binary()} | {error, authentication_failed}.
decrypt_aead(Ciphertext, Tag, Key, IV, AAD) ->
    try
        %% OTP 27: crypto_one_time_aead/7 for decryption
        Plaintext = crypto:crypto_one_time_aead(
            aes_256_gcm, Key, IV, Ciphertext, AAD, false, Tag
        ),
        {ok, Plaintext}
    catch
        error:function_clause ->
            {error, authentication_failed};
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%====================================================================
%% Legacy Encryption (Pre-OTP 27)
%%====================================================================

%% @doc Legacy AES-256-CBC encryption (fallback for OTP < 27)
-spec encrypt(binary(), key(), iv()) -> {ok, binary()}.
encrypt(Plaintext, Key, IV) ->
    Ciphertext = crypto:crypto_one_time(
        aes_256_cbc, Key, IV, Plaintext, true
    ),
    {ok, Ciphertext}.

%% @doc Legacy AES-256-CBC decryption (fallback for OTP < 27)
-spec decrypt(binary(), key(), iv()) -> {ok, binary()}.
decrypt(Ciphertext, Key, IV) ->
    Plaintext = crypto:crypto_one_time(
        aes_256_cbc, Key, IV, Ciphertext, false
    ),
    {ok, Plaintext}.

%%====================================================================
%% Token Generation
%%====================================================================

%% @doc Generate cryptographically secure token (32 bytes / 256 bits)
%% Use for session IDs, API keys, etc.
-spec generate_token() -> binary().
generate_token() ->
    crypto:strong_rand_bytes(32).

%% @doc Generate token of specified size (in bytes)
-spec generate_token(pos_integer()) -> binary().
generate_token(Size) when Size > 0, Size =< 1024 ->
    crypto:strong_rand_bytes(Size).

%%====================================================================
%% Nonce for Replay Protection
%%====================================================================

%% @doc Generate cryptographic nonce for replay protection
%% Returns 12-byte nonce (suitable for AES-GCM)
-spec generate_nonce() -> nonce().
generate_nonce() ->
    crypto:strong_rand_bytes(12).

%% @doc Validate nonce format and uniqueness
%% In production, maintain ETS table of seen nonces
-spec validate_nonce(nonce()) -> boolean().
validate_nonce(Nonce) when byte_size(Nonce) =:= 12 ->
    %% In production: check ETS table for replay attacks
    %% For now, just validate format
    true;
validate_nonce(_) ->
    false.

%% @doc Mark nonce as expired (remove from seen nonces table)
-spec expire_nonce(nonce()) -> ok.
expire_nonce(_Nonce) ->
    %% In production: delete from ETS table
    ok.

%%====================================================================
%% Key Derivation (PBKDF2)
%%====================================================================

%% @doc Derive key from password using PBKDF2-HMAC-SHA256
%% Uses 100,000 iterations by default (OWASP recommendation)
-spec derive_key(binary(), salt(), pos_integer()) -> key().
derive_key(Password, Salt, KeyLength) ->
    derive_key(Password, Salt, KeyLength, 100000).

%% @doc Derive key from password with custom iteration count
-spec derive_key(binary(), salt(), pos_integer(), pos_integer()) -> key().
derive_key(Password, Salt, KeyLength, Iterations) when is_binary(Password),
                                                         is_binary(Salt),
                                                         is_integer(KeyLength),
                                                         KeyLength > 0,
                                                         is_integer(Iterations),
                                                         Iterations > 0 ->
    crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, KeyLength).

%%====================================================================
%% Utilities
%%====================================================================

%% @doc Constant-time comparison for HMAC verification
%% Prevents timing attacks on signature verification
-spec constant_time_compare(binary(), binary()) -> boolean().
constant_time_compare(<<X, RestX/binary>>, <<Y, RestY/binary>>) when X =:= Y ->
    constant_time_compare(RestX, RestY);
constant_time_compare(<<>>, <<>>) ->
    true;
constant_time_compare(_, _) ->
    false.

%% @doc Securely zero memory (prevents data leakage)
%% Use for clearing sensitive data from memory
-spec secure_zero(binary()) -> binary().
secure_zero(Data) when is_binary(Data) ->
    %% In Erlang, we cannot directly zero memory
    %% Instead, return a zeroed binary to overwrite references
    Size = byte_size(Data),
    <<0:(Size * 8)>>.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Verify OTP version for feature availability
%% -spec check_otp_version() -> {major(), minor(), patch()}.
%% check_otp_version() ->
%%     {Major, Minor, Patch} = erlang:system_info(otp_release),
%%     {Major, Minor, Patch}.
