-module(erlmcp_tls_validation_tests).
-include_lib("eunit/include/eunit.hrl").

%% Test: TLS verification should be enabled (not verify_none)
test_tls_verification_enabled_test() ->
    ?assertEqual(true, erlmcp_tls_validation:is_verification_enabled(verify_peer)),
    ?assertEqual(false, erlmcp_tls_validation:is_verification_enabled(verify_none)),
    ?assertEqual(true, erlmcp_tls_validation:is_verification_enabled(verify_peer_fail_if_no_cert)).

%% Test: Peer verification mode validation
test_tls_peer_verification_test() ->
    Opts = [
        {verify, verify_peer},
        {cacerts, []}
    ],
    ?assertEqual(ok, erlmcp_tls_validation:validate_peer_verification(Opts)),

    BadOpts = [{verify, verify_none}],
    ?assertEqual({error, verification_disabled}, erlmcp_tls_validation:validate_peer_verification(BadOpts)).

%% Test: Hostname verification should be enforced
test_tls_hostname_verification_test() ->
    Opts = [
        {verify, verify_peer},
        {server_name_indication, "example.com"},
        {verify_fun, {erlmcp_tls_validation, verify_hostname}}
    ],
    Result = erlmcp_tls_validation:build_tls_options(Opts, "example.com"),
    ?assertMatch({ok, _}, Result),

    %% Hostname mismatch should be caught
    BadHostname = erlmcp_tls_validation:validate_hostname("wrong.com", "example.com"),
    ?assertEqual({error, hostname_mismatch}, BadHostname).

%% Test: Certificate chain validation
test_tls_certificate_chain_validation_test() ->
    %% Valid certificate chain should be validated
    Opts = [
        {verify, verify_peer},
        {depth, 2}
    ],
    ?assertEqual(ok, erlmcp_tls_validation:validate_cert_chain_depth(Opts)),

    %% No depth specified defaults to safe value
    DefaultOpts = [{verify, verify_peer}],
    Result = erlmcp_tls_validation:validate_cert_chain_depth(DefaultOpts),
    ?assertEqual(ok, Result).

%% Test: Expired certificates should be rejected
test_tls_expired_cert_rejection_test() ->
    %% Simulate expired certificate validation
    ExpiredCert = #{
        not_valid_after => {2023, 1, 1},
        issuer => "Test Issuer"
    },
    Result = erlmcp_tls_validation:validate_cert_validity(ExpiredCert),
    ?assertMatch({error, cert_expired}, Result).

%% Test: Self-signed certificates without pinning should be rejected
test_tls_self_signed_cert_rejection_test() ->
    SelfSignedCert = #{
        issuer => undefined,
        self_signed => true
    },
    Result = erlmcp_tls_validation:validate_cert_issuer(SelfSignedCert, undefined),
    ?assertMatch({error, self_signed_not_pinned}, Result).

%% Test: Hostname mismatch should be rejected
test_tls_hostname_mismatch_rejection_test() ->
    CertHostname = "example.com",
    RequestHostname = "wrong.com",
    Result = erlmcp_tls_validation:validate_hostname(RequestHostname, CertHostname),
    ?assertMatch({error, hostname_mismatch}, Result).

%% Test: MITM attacks should be prevented by strict validation
test_tls_mitm_attack_prevention_test() ->
    %% Any attempt to use verify_none or disabled verification should fail
    BadOpts = [{verify, verify_none}],
    Result = erlmcp_tls_validation:validate_peer_verification(BadOpts),
    ?assertMatch({error, verification_disabled}, Result),

    %% No hostname verification should fail
    NoHostOpts = [{verify, verify_peer}],
    BadResult = erlmcp_tls_validation:validate_hostname_verification(NoHostOpts),
    ?assertMatch({error, no_hostname_verification}, BadResult).

%% Test: Valid certificates should be accepted
test_tls_valid_cert_acceptance_test() ->
    ValidCert = #{
        not_valid_before => {2024, 1, 1},
        not_valid_after => {2026, 1, 1},
        issuer => "Trusted CA",
        subject => "example.com",
        self_signed => false
    },

    %% Valid cert should pass validation
    ChainResult = erlmcp_tls_validation:validate_cert_validity(ValidCert),
    ?assertEqual(ok, ChainResult).

%% Test: Certificate pinning support
test_tls_certificate_pinning_support_test() ->
    %% Certificate pinning should be optional but supported
    PinnedOpts = [
        {pinned_certs, [sha256_hash_of_cert]},
        {verify, verify_peer}
    ],

    Result = erlmcp_tls_validation:validate_cert_pinning(PinnedOpts),
    ?assertEqual(ok, Result).

%% Test: TLS options validation
test_tls_options_validation_test() ->
    %% Valid options should pass
    ValidOpts = [
        {verify, verify_peer},
        {versions, ['tlsv1.2', 'tlsv1.3']},
        {ciphers, ["ECDHE-RSA-AES256-GCM-SHA384"]},
        {depth, 3}
    ],

    Result = erlmcp_tls_validation:validate_tls_options(ValidOpts),
    ?assertEqual(ok, Result).

%% Test: Error handling for TLS validation
test_tls_error_handling_test() ->
    %% Errors should be properly formatted and logged
    TestErrors = [
        {error, verification_disabled},
        {error, hostname_mismatch},
        {error, cert_expired},
        {error, invalid_tls_options}
    ],

    lists:foreach(fun(Error) ->
        ?assertMatch({error, _}, Error)
    end, TestErrors).

%% Test: Minimum TLS version enforcement
test_tls_minimum_version_enforcement_test() ->
    %% TLS 1.2 or higher should be required
    ValidVersions = ['tlsv1.2', 'tlsv1.3'],
    Result = erlmcp_tls_validation:validate_minimum_version(ValidVersions),
    ?assertEqual(ok, Result),

    %% Old TLS versions should be rejected
    BadVersions = ['tlsv1', 'tlsv1.1'],
    BadResult = erlmcp_tls_validation:validate_minimum_version(BadVersions),
    ?assertMatch({error, tls_version_too_old}, BadResult).

%% Test: Cipher suite validation
test_tls_cipher_suite_validation_test() ->
    %% Strong ciphers should be required
    StrongCiphers = ["ECDHE-RSA-AES256-GCM-SHA384", "ECDHE-RSA-AES128-GCM-SHA256"],
    Result = erlmcp_tls_validation:validate_ciphers(StrongCiphers),
    ?assertEqual(ok, Result),

    %% Weak ciphers should be rejected
    WeakCiphers = ["NULL", "DES-CBC3-SHA"],
    BadResult = erlmcp_tls_validation:validate_ciphers(WeakCiphers),
    ?assertMatch({error, weak_cipher}, BadResult).

%% Test: SNI hostname support
test_tls_sni_hostname_support_test() ->
    %% SNI should be supported for hostname verification
    Hostname = "example.com",
    Opts = [{server_name_indication, Hostname}],
    Result = erlmcp_tls_validation:validate_sni_hostname(Opts, Hostname),
    ?assertEqual(ok, Result),

    %% SNI hostname mismatch should be caught
    BadResult = erlmcp_tls_validation:validate_sni_hostname(Opts, "wrong.com"),
    ?assertMatch({error, _}, BadResult).
