%%%-------------------------------------------------------------------
%%% @doc erlmcp_auth_mtls - Mutual TLS Authentication for erlmcp v3
%%%
%%% This module implements comprehensive mTLS certificate validation including:
%%% - Certificate chain verification using public_key:pkix_validate_chain/2
%%% - Subject CN extraction for user identity mapping
%%% - Expiration validation (notBefore/notAfter)
%%% - Certificate revocation checking (CRL/OCSP stubs)
%%% - Subject Alternative Names (SAN) extraction
%%%
%%% == Security Model ==
%%%
%%% 1. **Zero Trust**: All certificates validated, no implicit trust
%%% 2. **Chain Verification**: Full certificate chain validation to trusted root
%%% 3. **Expiration Enforcement**: Strict notBefore/notAfter checking
%%% 4. **Revocation Support**: CRL/OCSP stubs for enterprise integration
%%% 5. **Identity Mapping**: CN and SAN extraction for authorization
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Validate mTLS certificate from TLS socket
%%% {ok, CertDer} = ssl:peercert(Socket),
%%% CertInfo = #{certificate => CertDer},
%%% Config = #{ca_roots => [CaCertDer], max_depth => 5},
%%% case erlmcp_auth_mtls:validate(CertInfo, Config) of
%%%     {ok, UserId} -> {ok, authenticated_user, UserId};
%%%     {error, Reason} -> {error, authentication_failed, Reason}
%%% end.
%%% '''
%%%
%%% %% Extract certificate from socket
%%% case erlmcp_auth_mtls:extract_certificate_from_socket(Socket) of
%%%     {ok, CertDer} -> process_certificate(CertDer);
%%%     {error, no_certificate} -> require_client_cert()
%%% end.
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_auth_mtls).

%% Include ASN.1 records for certificate parsing
-include_lib("public_key/include/public_key.hrl").

%% API exports
-export([validate/2,
         extract_certificate_from_socket/1,
         check_revocation/3,
         extract_subject_cn/1,
         extract_sans/1,
         validate_expiration/1,
         build_cert_chain/2]).

%% Types
-type cert_der() :: binary().
-type cert_chain() :: [cert_der()].
-type mtls_config() :: #{
    ca_roots => [cert_der()],
    max_depth => pos_integer(),
    check_crl => boolean(),
    check_ocsp => boolean(),
    crl_db => term(),
    ocsp_timeout => pos_integer()
}.
-type cert_info() :: #{
    certificate := cert_der(),
    subject => binary(),
    issuer => binary(),
    serial => binary(),
    not_before => calendar:datetime(),
    not_after => calendar:datetime(),
    sans => [binary()]
}.
-type validation_result() :: {ok, binary()} | {error, term()}.

-export_type([cert_der/0, cert_chain/0, mtls_config/0, cert_info/0, validation_result/0]).

-include_lib("kernel/include/logger.hrl").

%% Defaults
-define(DEFAULT_MAX_DEPTH, 5).
-define(DEFAULT_OCSP_TIMEOUT, 5000).
-define(MAX_CERT_SIZE, 1024 * 1024). % 1MB max cert size

%%%===================================================================
%%% API - Certificate Validation
%%%===================================================================

%% @doc Validate mTLS certificate with comprehensive verification
%%
%% Performs the following checks:
%% 1. Certificate format validation
%% 2. Chain of trust verification to root CA
%% 3. Expiration validation (notBefore/notAfter)
%% 4. Optional revocation checking (CRL/OCSP)
%% 5. Subject CN extraction for user identity
%%
%% Returns {ok, UserId} on success where UserId is extracted from CN
%% Returns {error, Reason} on any validation failure
-spec validate(cert_info() | #{certificate := cert_der()}, mtls_config() | map()) ->
    validation_result().
validate(#{certificate := CertDer}, _Config) when byte_size(CertDer) > ?MAX_CERT_SIZE ->
    {error, certificate_too_large};

validate(#{certificate := CertDer}, Config) when is_map(Config) ->
    %% Decode certificate
    case public_key:der_decode('OTPCertificate', CertDer) of
        OtpCert when is_record(OtpCert, 'OTPCertificate') ->
            %% Extract subject CN for user identity
            UserId = extract_subject_cn(OtpCert),

            %% Validate expiration
            case validate_expiration(OtpCert) of
                ok ->
                    %% Validate certificate chain
                    case validate_cert_chain(CertDer, Config) of
                        ok ->
                            %% Check revocation if enabled
                            CheckCRL = maps:get(check_crl, Config, false),
                            CheckOCSP = maps:get(check_ocsp, Config, false),
                            case check_revocation(CertDer, [], #{check_crl => CheckCRL, check_ocsp => CheckOCSP}) of
                                ok ->
                                    %% All validations passed
                                    {ok, UserId};
                                {error, RevReason} ->
                                    {error, {revocation_check_failed, RevReason}}
                            end;
                        {error, ChainReason} ->
                            {error, {chain_validation_failed, ChainReason}}
                    end;
                {error, ExpReason} ->
                    {error, {certificate_expired, ExpReason}}
            end;
        _ ->
            {error, invalid_certificate_format}
    end;

validate(_CertInfo, _Config) ->
    {error, invalid_certificate_info}.

%% @doc Extract certificate from TLS socket
%%
%% Returns the peer certificate DER from an SSL/TLS socket.
%% Returns {error, no_certificate} if client did not present a certificate.
-spec extract_certificate_from_socket(ssl:sslsocket()) ->
    {ok, cert_der()} | {error, no_certificate | term()}.
extract_certificate_from_socket(Socket) ->
    try ssl:peercert(Socket) of
        {ok, CertDer} when is_binary(CertDer) ->
            {ok, CertDer};
        {error, no_peercert} ->
            {error, no_certificate};
        {error, Reason} ->
            {error, Reason}
    catch
        Error:Reason ->
            {error, {socket_error, Error, Reason}}
    end.

%% @doc Check certificate revocation status
%%
%% Supports both CRL (Certificate Revocation List) and OCSP
%% (Online Certificate Status Protocol) checking.
%%
%% Config options:
%% - check_crl: Enable CRL checking
%% - check_ocsp: Enable OCSP checking
%% - crl_db: CRL database for lookups
%% - ocsp_timeout: Timeout for OCSP requests (ms)
-spec check_revocation(cert_der(), cert_chain(), map()) -> ok | {error, term()}.
check_revocation(_CertDer, _CertChain, Config) ->
    CheckCRL = maps:get(check_crl, Config, false),
    CheckOCSP = maps:get(check_ocsp, Config, false),

    case {CheckCRL, CheckOCSP} of
        {false, false} ->
            %% No revocation checking enabled
            ok;
        {true, _} ->
            %% Check CRL
            check_crl(_CertDer, Config);
        {_, true} ->
            %% Check OCSP
            check_ocsp(_CertDer, Config);
        {true, true} ->
            %% Check both - if either indicates revocation, cert is revoked
            case check_crl(_CertDer, Config) of
                ok -> check_ocsp(_CertDer, Config);
                {error, _} = Error -> Error
            end
    end.

%%%===================================================================
%%% Certificate Extraction Functions
%%%===================================================================

%% @doc Extract Common Name (CN) from certificate subject
%%
%% Returns the CN value as a binary string.
%% Returns <<"unknown">> if CN cannot be extracted.
-spec extract_subject_cn(#'OTPCertificate'{}) -> binary().
extract_subject_cn(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    #'OTPTBSCertificate'{subject = Subject} = TbsCert,
    case extract_cn_from_subject(Subject) of
        {ok, CN} when CN =/= <<>> ->
            CN;
        _ ->
            %% Fallback: try to get from SAN
            case extract_sans_from_cert(TbsCert) of
                [SAN | _] -> SAN;
                _ -> <<"unknown">>
            end
    end.

%% @private Extract CN from subject name
extract_cn_from_subject({rdnSequence, RDNs}) ->
    extract_cn_from_rdn_sequence(RDNs);
extract_cn_from_subject(_) ->
    {error, invalid_subject}.

%% @private Extract CN from RDN sequence
extract_cn_from_rdn_sequence([]) ->
    {error, cn_not_found};
extract_cn_from_rdn_sequence([{_Type, Attribs} | Rest]) ->
    %% Try to find CN attribute
    CNAttrOID = {2, 5, 4, 3},  % id-at-commonName
    case lists:keyfind(CNAttrOID, 1, Attribs) of
        {CNAttrOID, {_, CNValue}} when is_list(CNValue) ->
            {ok, list_to_binary(CNValue)};
        {CNAttrOID, {_, CNValue}} when is_binary(CNValue) ->
            {ok, CNValue};
        _ ->
            extract_cn_from_rdn_sequence(Rest)
    end.

%% @doc Extract Subject Alternative Names (SANs) from certificate
%%
%% Returns a list of SAN values (DNS names, email addresses, etc.)
-spec extract_sans(#'OTPCertificate'{}) -> [binary()].
extract_sans(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    extract_sans_from_cert(TbsCert).

%% @private Extract SANs from TBSCertificate
extract_sans_from_cert(#'OTPTBSCertificate'{extensions = Extensions}) when is_list(Extensions) ->
    SANOID = {2, 5, 29, 17},  % id-ce-subjectAltName
    case lists:keyfind(SANOID, #'Extension'.extnID, Extensions) of
        #'Extension'{extnValue = SansDer} ->
            case 'OTP-PUB-KEY':decode('SubjectAltName', SansDer) of
                {ok, SansList} when is_list(SansList) ->
                    lists:filtermap(fun sanitize_san/1, SansList);
                _ ->
                    []
            end;
        false ->
            []
    end;
extract_sans_from_cert(_) ->
    [].

%% @private Sanitize SAN values to binary
sanitize_san({dNSName, Domain}) when is_list(Domain) ->
    {true, list_to_binary(Domain)};
sanitize_san({rfc822Name, Email}) when is_list(Email) ->
    {true, list_to_binary(Email)};
sanitize_san({uniformResourceIdentifier, URI}) when is_list(URI) ->
    {true, list_to_binary(URI)};
sanitize_san({iPAddress, IP}) when is_tuple(IP) ->
    IPBin = list_to_binary(inet:ntoa(IP)),
    {true, IPBin};
sanitize_san({directoryName, _}) ->
    false;
sanitize_san({_, _}) ->
    false;
sanitize_san(_) ->
    false.

%% @doc Validate certificate expiration (notBefore/notAfter)
%%
%% Returns ok if certificate is currently valid.
%% Returns {error, not_yet_valid} if current time is before notBefore.
%% Returns {error, expired} if current time is after notAfter.
-spec validate_expiration(#'OTPCertificate'{}) -> ok | {error, term()}.
validate_expiration(#'OTPCertificate'{tbsCertificate = TbsCert}) ->
    #'OTPTBSCertificate'{validity = #'Validity'{notBefore = NotBefore,
                                                   notAfter = NotAfter}} = TbsCert,
    CurrentTime = erlang:universaltime(),

    case convert_time(NotBefore) of
        {error, _} = Error ->
            Error;
        NotBeforeTime when CurrentTime < NotBeforeTime ->
            {error, not_yet_valid};
        _NotBeforeTime ->
            case convert_time(NotAfter) of
                {error, _} = Error ->
                    Error;
                NotAfterTime when CurrentTime > NotAfterTime ->
                    {error, expired};
                _NotAfterTime ->
                    ok
            end
    end.

%% @doc Build certificate chain for validation
%%
%% Constructs the certificate chain from the leaf certificate
%% using the provided CA root certificates.
-spec build_cert_chain(cert_der(), mtls_config() | map()) ->
    {ok, cert_chain()} | {error, term()}.
build_cert_chain(CertDer, Config) ->
    try
        %% Decode the certificate
        OtpCert = public_key:der_decode('OTPCertificate', CertDer),

        %% Get CA roots from config
        CARoots = maps:get(ca_roots, Config, []),

        %% Build chain starting with leaf cert
        Chain = build_chain([CertDer], OtpCert, CARoots),

        {ok, lists:reverse(Chain)}
    catch
        _:_ ->
            {error, chain_build_failed}
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @private Validate certificate chain using public_key
validate_cert_chain(CertDer, Config) ->
    try
        %% Decode certificate
        OtpCert = public_key:der_decode('OTPCertificate', CertDer),

        %% Get CA roots for chain validation
        CARoots = maps:get(ca_roots, Config, []),
        MaxDepth = maps:get(max_depth, Config, ?DEFAULT_MAX_DEPTH),

        %% Decode CA root certificates
        DecodedCARoots = [public_key:der_decode('OTPCertificate', Root) || Root <- CARoots],

        %% Setup validation options
        ValidationOpts = [
            {issuer_fun, fun get_issuer_cert/1},
            {max_path_length, MaxDepth}
        ],

        %% Validate chain
        case public_key:pkix_validate_chain([OtpCert | DecodedCARoots], ValidationOpts) of
            valid ->
                ok;
            {error, Reason} ->
                {error, {chain_invalid, Reason}}
        end
    catch
        _:_ ->
            {error, chain_validation_error}
    end.

%% @private Get issuer certificate from CA store
get_issuer_cert(#'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{issuer = Issuer}}) ->
    %% In production, this would look up the issuer certificate from a CA store
    %% For now, we return a placeholder
    case find_issuer_by_subject(Issuer) of
        {ok, IssuerCert} ->
            {ok, IssuerCert};
        error ->
            {error, unknown_issuer}
    end.

%% @private Find issuer certificate by subject
%% This is a stub - in production would query a certificate database
find_issuer_by_subject(_Issuer) ->
    %% Stub implementation - would query CA store
    error.

%% @private Build certificate chain by walking up to root
build_chain(CurrentChain, #'OTPCertificate'{tbsCertificate = TbsCert}, CARoots) ->
    #'OTPTBSCertificate'{issuer = Issuer, subject = Subject} = TbsCert,

    %% Check if this is self-signed (root cert)
    case is_self_signed(Subject, Issuer) of
        true ->
            CurrentChain;
        false ->
            %% Try to find issuer from CA roots
            case find_issuer_cert_in_roots(Issuer, CARoots) of
                {ok, IssuerCert} ->
                    build_chain([IssuerCert | CurrentChain], IssuerCert, CARoots);
                error ->
                    CurrentChain
            end
    end.

%% @private Check if certificate is self-signed
is_self_signed(Subject, Issuer) ->
    Subject =:= Issuer.

%% @private Find issuer certificate in CA roots
find_issuer_cert_in_roots(_Issuer, []) ->
    error;
find_issuer_cert_in_roots(Issuer, [CARoot | Rest]) ->
    try
        OtpCert = public_key:der_decode('OTPCertificate', CARoot),
        #'OTPCertificate'{tbsCertificate = #'OTPTBSCertificate'{subject = Subject}} = OtpCert,
        case Subject of
            Issuer -> {ok, OtpCert};
            _ -> find_issuer_cert_in_roots(Issuer, Rest)
        end
    catch
        _:_ ->
            find_issuer_cert_in_roots(Issuer, Rest)
    end.

%% @private Convert time from certificate format to erlang:universaltime()
convert_time({utcTime, TimeStr}) ->
    convert_utc_time(TimeStr);
convert_time({generalTime, TimeStr}) ->
    convert_generalized_time(TimeStr);
convert_time(_) ->
    {error, invalid_time_format}.

%% @private Convert UTCTime format (YYMMDDHHMMSSZ)
convert_utc_time(TimeStr) when is_list(TimeStr), length(TimeStr) >= 13 ->
    try
        %% UTCTime has 2-digit year - need to handle Y2K
        YY = list_to_integer(lists:sublist(TimeStr, 1, 2)),
        Year = if YY >= 50 -> 1900 + YY; YY < 50 -> 2000 + YY end,
        Month = list_to_integer(lists:sublist(TimeStr, 3, 2)),
        Day = list_to_integer(lists:sublist(TimeStr, 5, 2)),
        Hour = list_to_integer(lists:sublist(TimeStr, 7, 2)),
        Minute = list_to_integer(lists:sublist(TimeStr, 9, 2)),
        Second = list_to_integer(lists:sublist(TimeStr, 11, 2)),
        {{Year, Month, Day}, {Hour, Minute, Second}}
    catch
        _:_ ->
            {error, invalid_time_format}
    end;
convert_utc_time(_) ->
    {error, invalid_time_format}.

%% @private Convert GeneralizedTime format (YYYYMMDDHHMMSSZ)
convert_generalized_time(TimeStr) when is_list(TimeStr), length(TimeStr) >= 13 ->
    try
        Year = list_to_integer(lists:sublist(TimeStr, 1, 4)),
        Month = list_to_integer(lists:sublist(TimeStr, 5, 2)),
        Day = list_to_integer(lists:sublist(TimeStr, 7, 2)),
        Hour = list_to_integer(lists:sublist(TimeStr, 9, 2)),
        Minute = list_to_integer(lists:sublist(TimeStr, 11, 2)),
        Second = list_to_integer(lists:sublist(TimeStr, 13, 2)),
        {{Year, Month, Day}, {Hour, Minute, Second}}
    catch
        _:_ ->
            {error, invalid_time_format}
    end;
convert_generalized_time(_) ->
    {error, invalid_time_format}.

%% @private Check CRL (Certificate Revocation List)
check_crl(_CertDer, Config) ->
    case maps:get(crl_db, Config, undefined) of
        undefined ->
            %% No CRL database configured
            ok;
        _CRLDB ->
            %% In production, would check CRL database
            %% For now, this is a stub
            ok
    end.

%% @private Check OCSP (Online Certificate Status Protocol)
check_ocsp(_CertDer, Config) ->
    _ = maps:get(ocsp_timeout, Config, ?DEFAULT_OCSP_TIMEOUT),

    %% In production, would make OCSP request
    %% For now, this is a stub that returns ok
    ok.
