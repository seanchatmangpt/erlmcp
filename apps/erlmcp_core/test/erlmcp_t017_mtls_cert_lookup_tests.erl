%%%-------------------------------------------------------------------
%%% @doc T017 — mTLS CA cert lookup EUnit tests
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_t017_mtls_cert_lookup_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("public_key/include/public_key.hrl").

%%%===================================================================
%%% CA Cert Store Tests (T017)
%%%===================================================================

%% @doc Loaded CA cert is found by its subject
ca_cert_found_by_subject_test() ->
    Subject = {rdnSequence, [[[{'AttributeTypeAndValue',
                                {2,5,4,3},
                                {utf8String, "TestCA-T017-lookup"}}]]]},
    CertDer = <<"fake_ca_cert_der_T017_lookup">>,
    ok = erlmcp_auth_mtls:load_ca_cert(Subject, CertDer),
    Result = erlmcp_auth_mtls:find_issuer_by_subject(Subject),
    ?assertEqual({ok, CertDer}, Result).

%% @doc Unknown subject returns error
ca_cert_unknown_subject_test() ->
    UnknownSubject = {rdnSequence, [[[{'AttributeTypeAndValue',
                                       {2,5,4,3},
                                       {utf8String, "UnknownCA-T017-never-loaded"}}]]]},
    Result = erlmcp_auth_mtls:find_issuer_by_subject(UnknownSubject),
    ?assertEqual(error, Result).
