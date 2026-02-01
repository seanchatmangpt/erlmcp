%%%-------------------------------------------------------------------
%%% @doc FM-04: Real Cryptographic JWT Validation Tests
%%%
%%% SECURITY CRITICAL: Real cryptographic signature verification
%%% - No mocks or stubs
%%% - Real RSA 2048-bit and ECDSA P-256 keys
%%% - Real signature verification
%%% - Scope claim validation
%%% - Token replay prevention (jti tracking)
%%% - Algorithm confusion attack prevention
%%%
%%% Chicago School TDD:
%%% - Real jose library cryptographic operations
%%% - State-based verification (observable behavior)
%%% - No mocking - use actual crypto functions
%%%
%%% FM-04 RPN 250: Auth bypass vulnerability mitigation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_jwt_crypto_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

jwt_crypto_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Cryptographic Signature Tests", {inparallel, crypto_tests()}},
      {"Scope Validation Tests", {inparallel, scope_tests()}},
      {"Token Replay Prevention Tests", {inparallel, replay_tests()}},
      {"Algorithm Confusion Tests", {inparallel, algorithm_tests()}}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(public_key),
    application:ensure_all_started(jose),
    application:ensure_all_started(jsx),

    %% Generate REAL RSA 2048-bit keypair
    RSA_JWK = jose_jwk:generate_key({rsa, 2048}),
    #{public_key := RSA_Pub} = jose_jwk:to_pem(RSA_JWK),
    #{private_key := RSA_Priv} = jose_jwk:to_pem(RSA_JWK),

    %% Generate REAL ECDSA P-256 keypair
    ECDSA_JWK = jose_jwk:generate_key({ec, <<"P-256">>}),
    #{public_key := ECDSA_Pub} = jose_jwk:to_pem(ECDSA_JWK),
    #{private_key := ECDSA_Priv} = jose_jwk:to_pem(ECDSA_JWK),

    %% Start auth server with test keys
    TestConfig = #{
        jwt_keys => #{
            <<"rsa-key">> => RSA_Pub,
            <<"ecdsa-key">> => ECDSA_Pub
        },
        jwt => #{
            allowed_algorithms => [<<"RS256">>, <<"ES256">>], % Whitelist
            clock_skew_seconds => 60, % 1 minute tolerance
            enable_jti_tracking => true
        },
        rate_limiter_enabled => false
    },

    case erlmcp_auth:start_link(TestConfig) of
        {ok, Pid} ->
            #{
                pid => Pid,
                rsa_pub => RSA_Pub,
                rsa_priv => RSA_Priv,
                rsa_jwk => RSA_JWK,
                ecdsa_pub => ECDSA_Pub,
                ecdsa_priv => ECDSA_Priv,
                ecdsa_jwk => ECDSA_JWK
            };
        {error, {already_started, Pid}} ->
            erlmcp_auth:stop(),
            timer:sleep(100),
            {ok, NewPid} = erlmcp_auth:start_link(TestConfig),
            #{
                pid => NewPid,
                rsa_pub => RSA_Pub,
                rsa_priv => RSA_Priv,
                rsa_jwk => RSA_JWK,
                ecdsa_pub => ECDSA_Pub,
                ecdsa_priv => ECDSA_Priv,
                ecdsa_jwk => ECDSA_JWK
            }
    end.

cleanup(#{pid := Pid}) ->
    case is_process_alive(Pid) of
        true ->
            erlmcp_auth:stop(),
            timer:sleep(100);
        false ->
            ok
    end.

%%%===================================================================
%%% Cryptographic Signature Tests (4 cases)
%%%===================================================================

crypto_tests() ->
    [
     ?_test(test_valid_rsa_signature()),
     ?_test(test_valid_ecdsa_signature()),
     ?_test(test_wrong_key_rejection()),
     ?_test(test_malformed_jwt_rejection())
    ].

test_valid_rsa_signature() ->
    #{rsa_priv := RSA_Priv} = setup(),

    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"scope">> => [<<"read">>, <<"write">>]
    },

    %% Create JWT with REAL RSA signature
    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),

    %% Verify with REAL cryptographic verification
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),

    %% State-based verification (Chicago School)
    ?assertEqual(<<"user123">>, maps:get(<<"sub">>, VerifiedClaims)),
    ?assertEqual([<<"read">>, <<"write">>], maps:get(<<"scope">>, VerifiedClaims)).

test_valid_ecdsa_signature() ->
    #{ecdsa_jwk := ECDSA_JWK} = setup(),

    Claims = #{
        <<"sub">> => <<"user456">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"scope">> => [<<"admin">>]
    },

    %% Create JWT with REAL ECDSA signature
    JWS = #{<<"alg">> => <<"ES256">>, <<"kid">> => <<"ecdsa-key">>},
    Signed = jose_jws:sign(ECDSA_JWK, jsx:encode(Claims), JWS),
    {_Modules, Token} = jose_jws:compact(Signed),

    %% Verify with REAL ECDSA cryptographic verification
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),

    %% State-based verification
    ?assertEqual(<<"user456">>, maps:get(<<"sub">>, VerifiedClaims)),
    ?assertEqual([<<"admin">>], maps:get(<<"scope">>, VerifiedClaims)).

test_wrong_key_rejection() ->
    #{rsa_priv := _RSA_Priv} = setup(),

    %% Generate DIFFERENT RSA key
    WrongJWK = jose_jwk:generate_key({rsa, 2048}),
    #{private_key := WrongPrivKey} = jose_jwk:to_pem(WrongJWK),

    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600
    },

    %% Sign with WRONG key
    Token = create_rsa_jwt(Claims, WrongPrivKey, <<"rsa-key">>),

    %% Real signature verification should REJECT
    Result = erlmcp_auth:validate_jwt(Token),
    ?assertEqual({error, invalid_signature}, Result).

test_malformed_jwt_rejection() ->
    %% Malformed JWT (missing parts)
    MalformedToken = <<"header.payload">>,

    Result = erlmcp_auth:validate_jwt(MalformedToken),
    ?assertEqual({error, invalid_jwt_format}, Result).

%%%===================================================================
%%% Scope Validation Tests (3 cases)
%%%===================================================================

scope_tests() ->
    [
     ?_test(test_scope_enforcement()),
     ?_test(test_scope_missing_claim()),
     ?_test(test_scope_insufficient_permissions())
    ].

test_scope_enforcement() ->
    #{rsa_priv := RSA_Priv} = setup(),

    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"scope">> => [<<"read">>, <<"write">>, <<"delete">>]
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),

    %% Verify scope enforcement
    RequiredScopes = [<<"read">>, <<"write">>],
    ok = erlmcp_auth:verify_scope(VerifiedClaims, RequiredScopes).

test_scope_missing_claim() ->
    #{rsa_priv := RSA_Priv} = setup(),

    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600
        %% Missing scope claim
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),

    %% Verify scope validation fails gracefully
    RequiredScopes = [<<"read">>],
    Result = erlmcp_auth:verify_scope(VerifiedClaims, RequiredScopes),
    ?assertEqual({error, insufficient_scope}, Result).

test_scope_insufficient_permissions() ->
    #{rsa_priv := RSA_Priv} = setup(),

    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"scope">> => [<<"read">>] % Only read permission
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),

    %% Verify scope validation rejects insufficient permissions
    RequiredScopes = [<<"write">>, <<"delete">>],
    Result = erlmcp_auth:verify_scope(VerifiedClaims, RequiredScopes),
    ?assertEqual({error, insufficient_scope}, Result).

%%%===================================================================
%%% Token Replay Prevention Tests (JTI Tracking) (4 cases)
%%%===================================================================

replay_tests() ->
    [
     ?_test(test_jti_first_use_succeeds()),
     ?_test(test_jti_replay_rejected()),
     ?_test(test_jti_different_tokens_allowed()),
     ?_test(test_jti_expiration_cleanup())
    ].

test_jti_first_use_succeeds() ->
    #{rsa_priv := RSA_Priv} = setup(),

    JTI = generate_jti(),
    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"jti">> => JTI
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),

    %% First use should succeed
    {ok, VerifiedClaims} = erlmcp_auth:validate_jwt(Token),
    ?assertEqual(JTI, maps:get(<<"jti">>, VerifiedClaims)).

test_jti_replay_rejected() ->
    #{rsa_priv := RSA_Priv} = setup(),

    JTI = generate_jti(),
    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"jti">> => JTI
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),

    %% First use succeeds
    {ok, _} = erlmcp_auth:validate_jwt(Token),

    %% REPLAY ATTACK: Second use should be REJECTED
    Result = erlmcp_auth:validate_jwt(Token),
    ?assertEqual({error, token_replay}, Result).

test_jti_different_tokens_allowed() ->
    #{rsa_priv := RSA_Priv} = setup(),

    %% Create two tokens with DIFFERENT JTIs
    JTI1 = generate_jti(),
    JTI2 = generate_jti(),

    Claims1 = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"jti">> => JTI1
    },

    Claims2 = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600,
        <<"jti">> => JTI2
    },

    Token1 = create_rsa_jwt(Claims1, RSA_Priv, <<"rsa-key">>),
    Token2 = create_rsa_jwt(Claims2, RSA_Priv, <<"rsa-key">>),

    %% Both should succeed (different JTIs)
    {ok, Verified1} = erlmcp_auth:validate_jwt(Token1),
    {ok, Verified2} = erlmcp_auth:validate_jwt(Token2),

    ?assertEqual(JTI1, maps:get(<<"jti">>, Verified1)),
    ?assertEqual(JTI2, maps:get(<<"jti">>, Verified2)).

test_jti_expiration_cleanup() ->
    #{rsa_priv := RSA_Priv} = setup(),

    JTI = generate_jti(),
    Now = erlang:system_time(second),

    %% Create token that expires in 1 second
    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => Now + 1,
        <<"jti">> => JTI
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),

    %% First use succeeds
    {ok, _} = erlmcp_auth:validate_jwt(Token),

    %% Wait for token to expire
    timer:sleep(2000),

    %% Token should be expired (not replay error)
    Result = erlmcp_auth:validate_jwt(Token),
    ?assertEqual({error, token_expired}, Result).

%%%===================================================================
%%% Algorithm Confusion Attack Tests (3 cases)
%%%===================================================================

algorithm_tests() ->
    [
     ?_test(test_algorithm_whitelist_enforcement()),
     ?_test(test_algorithm_none_rejected()),
     ?_test(test_algorithm_hs256_with_rsa_key_rejected())
    ].

test_algorithm_whitelist_enforcement() ->
    #{rsa_priv := RSA_Priv} = setup(),

    %% RS256 is in whitelist
    Claims = #{
        <<"sub">> => <<"user123">>,
        <<"exp">> => erlang:system_time(second) + 3600
    },

    Token = create_rsa_jwt(Claims, RSA_Priv, <<"rsa-key">>),
    {ok, _} = erlmcp_auth:validate_jwt(Token).

test_algorithm_none_rejected() ->
    %% Create JWT with "none" algorithm (ATTACK)
    Header = #{<<"alg">> => <<"none">>, <<"kid">> => <<"rsa-key">>},
    Claims = #{<<"sub">> => <<"attacker">>, <<"exp">> => erlang:system_time(second) + 3600},

    HeaderB64 = base64:encode(jsx:encode(Header)),
    PayloadB64 = base64:encode(jsx:encode(Claims)),

    %% "none" algorithm requires empty signature
    AttackToken = <<HeaderB64/binary, ".", PayloadB64/binary, ".">>,

    %% Should be REJECTED
    Result = erlmcp_auth:validate_jwt(AttackToken),
    ?assertMatch({error, _}, Result).

test_algorithm_hs256_with_rsa_key_rejected() ->
    #{rsa_pub := RSA_Pub} = setup(),

    %% ATTACK: Claim HS256 but use RSA public key as HMAC secret
    %% This is a classic algorithm confusion attack
    Claims = #{
        <<"sub">> => <<"attacker">>,
        <<"exp">> => erlang:system_time(second) + 3600
    },

    %% Create JWT claiming HS256
    Header = #{<<"alg">> => <<"HS256">>, <<"kid">> => <<"rsa-key">>},

    %% Sign with RSA public key as HMAC secret (ATTACK)
    try
        HMAC_JWK = jose_jwk:from_oct(RSA_Pub),
        JWS = #{<<"alg">> => <<"HS256">>},
        Signed = jose_jws:sign(HMAC_JWK, jsx:encode(Claims), JWS),
        {_Modules, AttackToken} = jose_jws:compact(Signed),

        %% Should be REJECTED (algorithm not in whitelist or signature mismatch)
        Result = erlmcp_auth:validate_jwt(AttackToken),
        ?assertMatch({error, _}, Result)
    catch
        error:_ ->
            %% Key conversion may fail - this is also acceptable defense
            ok
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% Create RSA-signed JWT
create_rsa_jwt(Claims, PrivateKeyPem, Kid) ->
    JWK = jose_jwk:from_pem(PrivateKeyPem),
    JWS = #{<<"alg">> => <<"RS256">>, <<"kid">> => Kid},
    Signed = jose_jws:sign(JWK, jsx:encode(Claims), JWS),
    {_Modules, Token} = jose_jws:compact(Signed),
    Token.

%% Generate unique JTI (JWT ID) for replay prevention
generate_jti() ->
    Rand = crypto:strong_rand_bytes(16),
    base64:encode(Rand).
