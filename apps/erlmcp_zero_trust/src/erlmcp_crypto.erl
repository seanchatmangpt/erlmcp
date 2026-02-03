-module(erlmcp_crypto).
-export([encrypt/2, decrypt/2, hash/1, verify_password/2]).
-export([generate_key/1, generate_iv/0, generate_salt/1]).
-export([sign/2, verify_signature/3]).
-export([derive_key/3]).
-export([encrypt_aes_gcm/3, decrypt_aes_gcm/4]).
-export([create_hmac/2, verify_hmac/3]).

%% Encryption functions
encrypt(PlainText, Key) when is_binary(PlainText) ->
    IV = generate_iv(),
    CipherText = crypto:crypto_one_time(aes_256_cbc, Key, IV, PlainText, true),
    <<IV/binary, CipherText/binary>>.

decrypt(CipherText, Key) when is_binary(CipherText) ->
    <<IV:16/binary, CipherTextRest/binary>> = CipherText,
    PlainText = crypto:crypto_one_time(aes_256_cbc, Key, IV, CipherTextRest, false),
    PlainText.

%% Hashing functions
hash(Data) when is_binary(Data) ->
    crypto:hash(sha256, Data).

verify_password(PlainText, Hash) ->
    %% Hash the plain text and compare
    case hash(PlainText) of
        Hash -> true;
        _ -> false
    end.

%% Key generation
generate_key(KeySize) when KeySize == 128; KeySize == 256 ->
    crypto:strong_rand_bytes(KeySize div 8).

generate_iv() ->
    crypto:strong_rand_bytes(16).

generate_salt(SaltSize) ->
    crypto:strong_rand_bytes(SaltSize).

%% Digital signatures
sign(Data, PrivateKey) ->
    crypto:sign(rsa, sha256, Data, PrivateKey).

verify_signature(Data, Signature, PublicKey) ->
    crypto:verify(rsa, sha256, Data, Signature, PublicKey).

%% Key derivation
derive_key(Password, Salt, Iterations) ->
    pbkdf2(Password, Salt, Iterations, {sha256, 32}).

pbkdf2(Password, Salt, Iterations, {Hash, KeyLen}) ->
    crypto:pbkdf2(Hash, Password, Salt, Iterations, KeyLen).

%% AES-GCM encryption (authenticated encryption)
encrypt_aes_gcm(PlainText, Key, AdditionalData) ->
    IV = generate_iv(),
    {CipherText, Tag} = crypto:crypto_one_time(aes_gcm, Key, IV, PlainText, true, AdditionalData),
    <<IV/binary, CipherText/binary, Tag/binary>>.

decrypt_aes_gcm(CipherText, Key, AdditionalData, Tag) ->
    <<IV:12/binary, CipherTextRest/binary>> = CipherText,
    PlainText = crypto:crypto_one_time(aes_gcm, Key, IV, CipherTextRest, false, AdditionalData),
    PlainText.

%% HMAC functions
create_hmac(Data, Key) ->
    crypto:mac(hmac, sha256, Key, Data).

verify_hmac(Data, HMAC, Key) ->
    crypto:mac(hmac, sha256, Key, Data) == HMAC.