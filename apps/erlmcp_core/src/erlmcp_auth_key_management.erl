%%%-------------------------------------------------------------------
%%% @doc FM-04: JWT Key Management Module
%%%
%%% Handles cryptographic key management for JWT validation:
%%% - Load keys from files/fixtures
%%% - Key rotation logic
%%% - JWKS (JSON Web Key Set) parsing
%%% - Key caching and lifecycle management
%%%
%%% Security considerations:
%%% - Keys stored in PEM format
%%% - Support for RSA and ECDSA algorithms
%%% - Automatic key validation before storage
%%% - Secure key rotation (gradual rollover)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_key_management).

%% API exports
-export([
    load_key_from_file/1,
    load_keys_from_directory/1,
    parse_jwks/1,
    validate_key_format/1,
    rotate_keys/3,
    export_public_jwks/1
]).

%% Types
-type key_id() :: binary().
-type public_key_pem() :: binary().
-type private_key_pem() :: binary().
-type jwks() :: #{binary() => term()}.

-export_type([key_id/0, public_key_pem/0, private_key_pem/0, jwks/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Load cryptographic key from PEM file
-spec load_key_from_file(file:filename()) -> {ok, binary()} | {error, term()}.
load_key_from_file(Filename) ->
    case file:read_file(Filename) of
        {ok, KeyData} ->
            case validate_key_format(KeyData) of
                ok ->
                    {ok, KeyData};
                {error, Reason} ->
                    logger:error("Invalid key format in file ~p: ~p", [Filename, Reason]),
                    {error, invalid_key_format}
            end;
        {error, Reason} ->
            logger:error("Failed to read key file ~p: ~p", [Filename, Reason]),
            {error, Reason}
    end.

%% @doc Load all keys from directory (supports .pem files)
-spec load_keys_from_directory(file:filename()) -> {ok, #{key_id() => public_key_pem()}} | {error, term()}.
load_keys_from_directory(DirPath) ->
    case file:list_dir(DirPath) of
        {ok, Files} ->
            PemFiles = [F || F <- Files, filename:extension(F) =:= ".pem"],
            Keys = lists:foldl(fun(File, Acc) ->
                FilePath = filename:join(DirPath, File),
                KeyId = list_to_binary(filename:basename(File, ".pem")),

                case load_key_from_file(FilePath) of
                    {ok, KeyData} ->
                        Acc#{KeyId => KeyData};
                    {error, Reason} ->
                        logger:warning("Skipping invalid key file ~p: ~p", [File, Reason]),
                        Acc
                end
            end, #{}, PemFiles),

            {ok, Keys};
        {error, Reason} ->
            logger:error("Failed to list directory ~p: ~p", [DirPath, Reason]),
            {error, Reason}
    end.

%% @doc Parse JWKS (JSON Web Key Set) from JSON data
-spec parse_jwks(binary()) -> {ok, #{key_id() => public_key_pem()}} | {error, term()}.
parse_jwks(JWKSJson) ->
    try
        JWKS = jsx:decode(JWKSJson, [return_maps]),
        Keys = maps:get(<<"keys">>, JWKS, []),

        ParsedKeys = lists:foldl(fun(JWK, Acc) ->
            case parse_single_jwk(JWK) of
                {ok, KeyId, KeyPem} ->
                    Acc#{KeyId => KeyPem};
                {error, Reason} ->
                    logger:warning("Skipping invalid JWK: ~p", [Reason]),
                    Acc
            end
        end, #{}, Keys),

        {ok, ParsedKeys}
    catch
        error:Reason ->
            logger:error("Failed to parse JWKS: ~p", [Reason]),
            {error, invalid_jwks_format}
    end.

%% @doc Validate key format (PEM or JWK)
-spec validate_key_format(binary()) -> ok | {error, term()}.
validate_key_format(KeyData) ->
    try
        % Try to parse as PEM
        _JWK = jose_jwk:from_pem(KeyData),
        ok
    catch
        error:Reason ->
            logger:debug("Key validation failed: ~p", [Reason]),
            {error, invalid_key_format}
    end.

%% @doc Rotate keys with gradual rollover
%% Returns tuple of {OldKeys, NewKeys, OverlapPeriod}
-spec rotate_keys(#{key_id() => public_key_pem()}, #{key_id() => public_key_pem()}, pos_integer()) ->
    {ok, #{key_id() => public_key_pem()}}.
rotate_keys(OldKeys, NewKeys, OverlapSeconds) ->
    % During overlap period, both old and new keys are valid
    % This allows for gradual migration without breaking existing tokens

    % Merge keys with new keys taking precedence
    MergedKeys = maps:merge(OldKeys, NewKeys),

    % Schedule removal of old keys after overlap period
    erlang:send_after(OverlapSeconds * 1000, self(), {remove_old_keys, OldKeys}),

    logger:info("Key rotation initiated: ~p old keys, ~p new keys, overlap: ~ps",
                [maps:size(OldKeys), maps:size(NewKeys), OverlapSeconds]),

    {ok, MergedKeys}.

%% @doc Export public keys as JWKS format (for publishing)
-spec export_public_jwks(#{key_id() => public_key_pem()}) -> {ok, binary()} | {error, term()}.
export_public_jwks(Keys) ->
    try
        JWKs = maps:fold(fun(KeyId, KeyPem, Acc) ->
            try
                JWK = jose_jwk:from_pem(KeyPem),
                JWKMap = jose_jwk:to_map(JWK),

                % Add kid (key ID) to JWK
                JWKWithKid = JWKMap#{<<"kid">> => KeyId},

                [JWKWithKid | Acc]
            catch
                error:_ ->
                    logger:warning("Failed to export key ~p to JWKS", [KeyId]),
                    Acc
            end
        end, [], Keys),

        JWKS = #{<<"keys">> => JWKs},
        JWKSJson = jsx:encode(JWKS),

        {ok, JWKSJson}
    catch
        error:Reason ->
            logger:error("Failed to export JWKS: ~p", [Reason]),
            {error, jwks_export_failed}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Parse single JWK from JWKS
parse_single_jwk(JWK) ->
    try
        % Extract key ID
        KeyId = maps:get(<<"kid">>, JWK, undefined),

        case KeyId of
            undefined ->
                {error, missing_kid};
            _ ->
                % Convert JWK to PEM
                JWKStruct = jose_jwk:from_map(JWK),
                #{public_key := KeyPem} = jose_jwk:to_pem(JWKStruct),

                {ok, KeyId, KeyPem}
        end
    catch
        error:Reason ->
            {error, {parse_failed, Reason}}
    end.
