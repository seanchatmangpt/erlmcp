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
-compile({no_auto_import,[list_to_existing_atom/1]}).

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

%% CA store API
-export([load_ca_cert/2,
         find_issuer_by_subject/1,
         init_ca_store/0]).

-define(CA_CERT_TABLE, erlmcp_ca_cert_store).

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
        OtpCert when element(1, OtpCert) =:= 'OTPCertificate' ->
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

%% @doc Initialise the CA certificate ETS table if absent.
%% Safe to call multiple times; a second call is a no-op.
-spec init_ca_store() -> ok.
init_ca_store() ->
    case ets:info(?CA_CERT_TABLE) of
        undefined ->
            ets:new(?CA_CERT_TABLE, [named_table, public, set,
                                     {read_concurrency, true}]),
            ok;
        _ ->
            ok
    end.

%% @doc Load a CA certificate DER binary into the cert store.
%% The certificate's subject is used as the lookup key.
-spec load_ca_cert(term(), cert_der()) -> ok | {error, term()}.
load_ca_cert(Subject, CertDer) when is_binary(CertDer) ->
    init_ca_store(),
    ets:insert(?CA_CERT_TABLE, {Subject, CertDer}),
    ok.

%% @private Find issuer certificate by subject — queries CA store ETS table.
-spec find_issuer_by_subject(term()) -> {ok, cert_der()} | error.
find_issuer_by_subject(Issuer) ->
    init_ca_store(),
    case ets:lookup(?CA_CERT_TABLE, Issuer) of
        [{Issuer, CertDer}] ->
            {ok, CertDer};
        [] ->
            error
    end.

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
check_crl(CertDer, Config) ->
    case maps:get(crl_db, Config, undefined) of
        undefined ->
            %% No CRL database configured
            ok;
        CRLDB when is_pid(CRLDB); is_atom(CRLDB) ->
            %% Check CRL via CRL database gen_server
            check_crl_in_db(CertDer, CRLDB, Config);
        _CRLDB ->
            %% In production, would check CRL database
            ok
    end.

%% @private Check CRL in database
check_crl_in_db(CertDer, CRLDB, Config) ->
    try
        %% Decode certificate to extract serial number
        OtpCert = public_key:der_decode('OTPCertificate', CertDer),
        #'OTPCertificate'{tbsCertificate = TbsCert} = OtpCert,
        #'OTPTBSCertificate'{serialNumber = Serial} = TbsCert,

        %% Query CRL database
        case is_pid(CRLDB) of
            true ->
                gen_server:call(CRLDB, {check_revoked, Serial}, maps:get(crl_timeout, Config, 5000));
            false when is_atom(CRLDB) ->
                gen_server:call(CRLDB, {check_revoked, Serial}, maps:get(crl_timeout, Config, 5000))
        end
    catch
        _:_ ->
            %% On error, fail closed for security
            {error, crl_check_failed}
    end.

%% @private Download and parse CRL from URL
-spec download_crl(binary()) -> {ok, map()} | {error, term()}.
download_crl(CRLURL) ->
    %% Parse CRL URL
    case parse_url(CRLURL) of
        {ok, Scheme, Host, Port, Path} when Scheme =:= https; Scheme =:= http ->
            %% Download CRL using gun
            case http_get(Host, Port, Path, Scheme =:= https) of
                {ok, CRLData} ->
                    parse_crl(CRLData);
                {error, Reason} ->
                    {error, {download_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_url, Reason}}
    end.

%% @private Parse CRL data
parse_crl(CRLData) when is_binary(CRLData) ->
    try
        %% Decode CRL (DER format)
        case 'OTP-PUB-KEY':decode('CertificateList', CRLData) of
            {ok, CRL} ->
                %% Extract revoked certificates
                #'CertificateList'{tbsCertList = TBSCertList} = CRL,
                #'TBSCertList'{revokedCertificates = RevokedCerts} = TBSCertList,

                %% Build revocation map
                %% Using tuple-based pattern matching to avoid record dependencies
                RevokedMap = lists:foldl(
                    fun(RevokedCert, Acc) ->
                        try
                            %% Extract fields from tuple (skip record name tag at position 1)
                            Serial = element(2, RevokedCert),
                            RevDate = element(3, RevokedCert),
                            maps:put(Serial, RevDate, Acc)
                        catch
                            _:_ -> Acc
                        end
                    end,
                    #{},
                    RevokedCerts
                ),

                {ok, #{
                    revoked => RevokedMap,
                    this_update => get_crl_field(TBSCertList, thisUpdate),
                    next_update => get_crl_field(TBSCertList, nextUpdate)
                }};
            {error, Reason} ->
                {error, {decode_failed, Reason}}
        end
    catch
        _:_ ->
            {error, parse_error}
    end.

%% @private Get field from TBSCertList
get_crl_field(TBSCertList, Field) ->
    case element(2, TBSCertList) of
        List when is_list(List) ->
            case lists:keyfind(Field, 1, List) of
                {Field, Value} -> Value;
                false -> undefined
            end;
        _ ->
            undefined
    end.

%% @private Check OCSP (Online Certificate Status Protocol)
check_ocsp(CertDer, Config) ->
    Timeout = maps:get(ocsp_timeout, Config, ?DEFAULT_OCSP_TIMEOUT),

    try
        %% Decode certificate to extract OCSP URL
        OtpCert = public_key:der_decode('OTPCertificate', CertDer),
        #'OTPCertificate'{tbsCertificate = TbsCert} = OtpCert,

        %% Extract Authority Information Access extension
        case extract_ocsp_url(TbsCert) of
            {ok, OCSPURL} ->
                %% Build OCSP request
                case build_ocsp_request(CertDer) of
                    {ok, OCSPRequest} ->
                        %% Send OCSP request
                        case send_ocsp_request(OCSPURL, OCSPRequest, Timeout) of
                            {ok, OCSPResponse} ->
                                parse_ocsp_response(OCSPResponse);
                            {error, Reason} ->
                                {error, {ocsp_request_failed, Reason}}
                        end;
                    {error, Reason} ->
                        {error, {ocsp_request_build_failed, Reason}}
                end;
            {error, Reason} ->
                {error, {ocsp_url_extract_failed, Reason}}
        end
    catch
        _:_ ->
            %% On error, fail closed for security
            {error, ocsp_check_failed}
    end.

%% @private Extract OCSP URL from certificate
extract_ocsp_url(#'OTPTBSCertificate'{extensions = Extensions}) when is_list(Extensions) ->
    AIAOID = {1, 3, 6, 1, 5, 5, 7, 1, 1},  % id-pe-authorityInfoAccess

    case lists:keyfind(AIAOID, #'Extension'.extnID, Extensions) of
        #'Extension'{extnValue = AIADer} ->
            case 'OTP-PUB-KEY':decode('AuthorityInfoAccessSyntax', AIADer) of
                {ok, AIAList} when is_list(AIAList) ->
                    %% Find OCSP responder
                    case lists:search(
                        fun({_, AccessDescription}) ->
                            case AccessDescription of
                                {1, 3, 6, 1, 5, 5, 7, 48, 1} -> true;  % id-ad-ocsp
                                _ -> false
                            end
                        end,
                        AIAList
                    ) of
                        {value, {_, {uniformResourceIdentifier, OCSPURL}}} ->
                            {ok, list_to_binary(OCSPURL)};
                        _ ->
                            {error, ocsp_url_not_found}
                    end;
                _ ->
                    {error, aia_decode_failed}
            end;
        false ->
            {error, aia_extension_not_found}
    end;
extract_ocsp_url(_) ->
    {error, no_extensions}.

%% @private Build OCSP request
build_ocsp_request(CertDer) ->
    try
        OtpCert = public_key:der_decode('OTPCertificate', CertDer),
        #'OTPCertificate'{tbsCertificate = TbsCert} = OtpCert,

        #'OTPTBSCertificate'{
            serialNumber = Serial,
            issuer = Issuer
        } = TbsCert,

        %% Build OCSP request (simplified)
        OCSPRequest = #{
            serial => Serial,
            issuer => Issuer,
            hash_algorithm => sha256
        },

        {ok, OCSPRequest}
    catch
        _:_ ->
            {error, cert_decode_failed}
    end.

%% @private Send OCSP request
send_ocsp_request(OCSPURL, OCSPRequest, Timeout) ->
    case parse_url(OCSPURL) of
        {ok, Scheme, Host, Port, Path} when Scheme =:= https; Scheme =:= http ->
            %% Encode OCSP request (DER format)
            OCSPRequestDer = encode_ocsp_request(OCSPRequest),

            %% Make HTTP POST request
            case http_post(Host, Port, Path, OCSPRequestDer, Scheme =:= https, Timeout) of
                {ok, ResponseBody} ->
                    {ok, ResponseBody};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, {invalid_url, Reason}}
    end.

%% @private Encode OCSP request to DER
encode_ocsp_request(#{serial := Serial, issuer := Issuer, hash_algorithm := HashAlg}) ->
    %% Simplified encoding - in production, use proper ASN.1 encoding
    <<>>.

%% @private Parse OCSP response
parse_ocsp_response(OCSPResponseDer) ->
    try
        case 'OTP-PUB-KEY':decode('OCSPResponse', OCSPResponseDer) of
            {ok, OCSPResponse} ->
                %% Check response status
                case OCSPResponse of
                    #{responseStatus := successful} ->
                        %% Parse response bytes
                        #{responseBytes := ResponseBytes} = OCSPResponse,
                        #{response := Response} = ResponseBytes,

                        %% Check certificate status
                        case Response of
                            #{certStatus := good} ->
                                ok;
                            #{certStatus := {revoked, _RevocationReason}} ->
                                {error, certificate_revoked};
                            #{certStatus := unknown} ->
                                {error, certificate_status_unknown}
                        end;
                    #{responseStatus := unauthorized} ->
                        {error, ocsp_unauthorized};
                    #{responseStatus := tryLater} ->
                        {error, ocsp_try_later};
                    #{responseStatus := sigRequired} ->
                        {error, ocsp_sig_required};
                    _ ->
                        {error, ocsp_error}
                end;
            {error, Reason} ->
                {error, {decode_failed, Reason}}
        end
    catch
        _:_ ->
            {error, parse_error}
    end.

%% @private Parse URL
parse_url(URL) when is_binary(URL) ->
    parse_url(binary_to_list(URL));
parse_url(URL) when is_list(URL) ->
    try uri_string:parse(URL) of
        #{scheme := Scheme0, host := Host} = Map ->
            Scheme = list_to_existing_atom(Scheme0),
            Port = case maps:get(port, Map, undefined) of
                undefined -> default_port_for_scheme(Scheme0);
                PortVal -> PortVal
            end,
            Path = case maps:get(path, Map, undefined) of
                undefined -> "/";
                PathVal -> "/" ++ PathVal
            end,
            {ok, Scheme, Host, Port, Path}
    catch
        error:_ ->
            {error, invalid_uri}
    end.

%% @private Get default port for scheme
default_port_for_scheme("https") -> 443;
default_port_for_scheme("http") -> 80;
default_port_for_scheme(_) -> 80.

%% @private HTTP GET request
http_get(Host, Port, Path, SSL) ->
    try
        %% Open connection
        Opts = case SSL of
            true -> #{transport => ssl, protocols => [http]};
            false -> #{protocols => [http]}
        end,
        {ok, ConnPid} = gun:open(Host, Port, Opts),
        {ok, http} = gun:await_up(ConnPid, 5000),

        %% Make GET request
        StreamRef = gun:get(ConnPid, Path),

        %% Await response
        case gun:await(ConnPid, StreamRef, 10000) of
            {response, nofin, 200, _Headers} ->
                %% Read body
                {data, nofin, Body} = gun:await(ConnPid, StreamRef, 10000),
                gun:close(ConnPid),
                {ok, Body};
            {response, _, StatusCode, _Headers} ->
                gun:close(ConnPid),
                {error, {http_error, StatusCode}};
            {error, Reason} ->
                gun:close(ConnPid),
                {error, Reason}
        end
    catch
        _:_ ->
            {error, connection_failed}
    end.

%% @private HTTP POST request
http_post(Host, Port, Path, Body, SSL, Timeout) ->
    try
        %% Open connection
        Opts = case SSL of
            true -> #{transport => ssl, protocols => [http]};
            false -> #{protocols => [http]}
        end,
        {ok, ConnPid} = gun:open(Host, Port, Opts),
        {ok, http} = gun:await_up(ConnPid, 5000),

        %% Make POST request
        Headers = [
            {<<"content-type">>, <<"application/ocsp-request">>},
            {<<"accept">>, <<"application/ocsp-response">>}
        ],
        StreamRef = gun:post(ConnPid, Path, Headers, Body),

        %% Await response
        case gun:await(ConnPid, StreamRef, Timeout) of
            {response, nofin, 200, _Headers} ->
                %% Read body
                {data, nofin, ResponseBody} = gun:await(ConnPid, StreamRef, Timeout),
                gun:close(ConnPid),
                {ok, ResponseBody};
            {response, _, StatusCode, _Headers} ->
                gun:close(ConnPid),
                {error, {http_error, StatusCode}};
            {error, Reason} ->
                gun:close(ConnPid),
                {error, Reason}
        end
    catch
        _:_ ->
            {error, connection_failed}
    end.

%% @private Convert string to existing atom
list_to_existing_atom(List) when is_list(List) ->
    try
        list_to_existing_atom(List)
    catch
        error:badarg ->
            list_to_atom(List)
    end.
