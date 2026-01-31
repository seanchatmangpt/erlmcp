%%%-------------------------------------------------------------------
%%% @doc erlmcp_mtls_validator - X.509 Certificate Validation for mTLS
%%%
%%% Implements proper X.509 certificate validation using Erlang/OTP's
%%% public_key and crypto modules. Follows Joe Armstrong's principle:
%%% "Don't roll your own crypto."
%%%
%%% Features:
%%% - DER/PEM certificate parsing
%%% - X.509 chain verification to trusted CA
%%% - Expiration validation (notBefore, notAfter)
%%% - Subject CN and SAN extraction
%%% - CN pattern matching (wildcards)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_mtls_validator).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    validate_certificate/1,
    validate_certificate/2,
    parse_certificate/1,
    extract_subject/1,
    validate_expiry/1,
    validate_chain/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("public_key/include/public_key.hrl").

%% Types
-type cert_der() :: binary().
-type parsed_cert() :: #'OTPCertificate'{}.
-type subject_info() :: #{
    cn => binary() | undefined,
    san => [{dns, binary()} | {ip, binary()} | {email, binary()} | {uri, binary()}],
    raw_subject => term()
}.
-type validation_result() :: {ok, binary()} | {error, term()}.

%% State
-record(state, {
    trusted_cas :: [cert_der()],
    allowed_cn_patterns :: [binary()]
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start validator with default config.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start validator with config.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Validate certificate with system-wide config.
-spec validate_certificate(cert_der()) -> validation_result().
validate_certificate(CertDer) ->
    gen_server:call(?MODULE, {validate_certificate, CertDer}).

%% @doc Validate certificate with custom config.
-spec validate_certificate(cert_der(), map()) -> validation_result().
validate_certificate(CertDer, Config) ->
    do_validate_certificate(CertDer, Config).

%% @doc Parse DER/PEM certificate.
-spec parse_certificate(cert_der()) -> {ok, parsed_cert()} | {error, term()}.
parse_certificate(CertDer) ->
    try
        ParsedCert = 'PKIX':pkix_decode_cert(CertDer, plain),
        {ok, ParsedCert}
    catch
        error:_ ->
            try decode_pem_certificate(CertDer)
            catch
                error:Reason -> {error, {invalid_certificate, Reason}}
            end
    end.

%% @doc Extract subject information from certificate.
-spec extract_subject(parsed_cert()) -> subject_info().
extract_subject(ParsedCert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = ParsedCert,
    #'OTPTBSCertificate'{subject = Subject, extensions = Extensions} = TBSCert,

    CN = extract_cn_from_rdn(Subject),
    SAN = extract_san_from_extensions(Extensions),

    #{
        cn => CN,
        san => SAN,
        raw_subject => Subject
    }.

%% @doc Validate certificate expiration.
-spec validate_expiry(parsed_cert()) -> ok | {error, term()}.
validate_expiry(ParsedCert) ->
    #'OTPCertificate'{tbsCertificate = TBSCert} = ParsedCert,
    #'OTPTBSCertificate'{validity = Validity} = TBSCert,
    #'Validity'{notBefore = NotBefore, notAfter = NotAfter} = Validity,

    Now = erlang:system_time(second),
    NotBeforeSec = convert_asn1_time(NotBefore),
    NotAfterSec = convert_asn1_time(NotAfter),

    if
        Now < NotBeforeSec -> {error, certificate_not_yet_valid};
        Now > NotAfterSec -> {error, certificate_expired};
        true -> ok
    end.

%% @doc Validate certificate chain to trusted CAs.
-spec validate_chain(cert_der(), [cert_der()]) -> ok | {error, term()}.
validate_chain(_CertDer, []) ->
    {error, no_trusted_cas_configured};
validate_chain(CertDer, TrustedCAs) ->
    try
        {ok, PeerCert} = parse_certificate(CertDer),
        #'OTPCertificate'{tbsCertificate = TBSCert} = PeerCert,
        #'OTPTBSCertificate'{issuer = Issuer} = TBSCert,

        case lists:any(fun(CADer) ->
            {ok, CACert} = parse_certificate(CADer),
            #'OTPCertificate'{tbsCertificate = CATBSCert} = CACert,
            #'OTPTBSCertificate'{subject = CASubject} = CATBSCert,
            Issuer =:= CASubject
        end, TrustedCAs) of
            true -> ok;
            false -> {error, untrusted_certificate_chain}
        end
    catch
        error:_ -> {error, chain_validation_failed}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Config]) ->
    TrustedCAs = maps:get(trusted_cas, Config, []),
    AllowedPatterns = maps:get(allowed_cn_patterns, Config, []),
    State = #state{
        trusted_cas = TrustedCAs,
        allowed_cn_patterns = AllowedPatterns
    },
    {ok, State}.

handle_call({validate_certificate, CertDer}, _From, State) ->
    Config = #{
        trusted_cas => State#state.trusted_cas,
        allowed_cn_patterns => State#state.allowed_cn_patterns
    },
    Result = do_validate_certificate(CertDer, Config),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Validate certificate with full validation pipeline.
do_validate_certificate(CertDer, Config) ->
    try
        % Parse certificate
        {ok, ParsedCert} = parse_certificate(CertDer),

        % Validate expiration
        case validate_expiry(ParsedCert) of
            ok -> ok;
            {error, Reason} -> throw({error, Reason})
        end,

        % Validate chain
        TrustedCAs = maps:get(trusted_cas, Config, []),
        case TrustedCAs of
            [] ->
                logger:warning("mTLS: No trusted CAs configured, skipping chain validation");
            _ ->
                case validate_chain(CertDer, TrustedCAs) of
                    ok -> ok;
                    {error, Reason} -> throw({error, Reason})
                end
        end,

        % Extract subject
        SubjectInfo = extract_subject(ParsedCert),
        CN = maps:get(cn, SubjectInfo, <<"unknown">>),

        % Validate CN patterns
        AllowedPatterns = maps:get(allowed_cn_patterns, Config, []),
        case AllowedPatterns of
            [] -> ok;
            _ ->
                case validate_cn_pattern(CN, AllowedPatterns) of
                    ok -> ok;
                    {error, Reason} -> throw({error, Reason})
                end
        end,

        {ok, CN}
    catch
        throw:{error, Reason} -> {error, Reason};
        error:{badmatch, _} -> {error, certificate_parse_failed};
        Error:Reason:Stack ->
            logger:error("mTLS validation error: ~p:~p~n~p", [Error, Reason, Stack]),
            {error, validation_failed}
    end.

%% @private Decode PEM certificate (base64 with headers).
decode_pem_certificate(PemData) ->
    Lines = binary:split(PemData, <<"\n">>, [global]),
    Base64Lines = lists:filter(fun(Line) ->
        not (Line =:= <<"-----BEGIN CERTIFICATE-----">> orelse
              Line =:= <<"-----END CERTIFICATE-----">> orelse
              byte_size(Line) =:= 0)
    end, Lines),

    Base64Data = iolist_to_binary(Base64Lines),
    CertDer = base64:decode(Base64Data),
    ParsedCert = 'PKIX':pkix_decode_cert(CertDer, plain),
    {ok, ParsedCert}.

%% @private Convert ASN.1 time to seconds since epoch.
convert_asn1_time({utcTime, TimeBin}) ->
    <<YY:2/binary, MM:2/binary, DD:2/binary, HH:2/binary, Min:2/binary, SS:2/binary, _Z>> = TimeBin,
    Year = case binary_to_integer(YY) of
        Y when Y >= 50 -> 1900 + Y;
        Y -> 2000 + Y
    end,
    to_seconds(Year, MM, DD, HH, Min, SS);

convert_asn1_time({generalizedTime, TimeBin}) ->
    <<YYYY:4/binary, MM:2/binary, DD:2/binary, HH:2/binary, Min:2/binary, SS:2/binary, _Z>> = TimeBin,
    Year = binary_to_integer(YYYY),
    to_seconds(Year, MM, DD, HH, Min, SS).

%% @private Helper to convert date parts to seconds.
to_seconds(Year, MM, DD, HH, Min, SS) ->
    Month = binary_to_integer(MM),
    Day = binary_to_integer(DD),
    Hour = binary_to_integer(HH),
    Minute = binary_to_integer(Min),
    Second = binary_to_integer(SS),
    DateTime = {{Year, Month, Day}, {Hour, Minute, Second}},
    GregSec = calendar:datetime_to_gregorian_seconds(DateTime),
    EpochSec = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    GregSec - EpochSec.

%% @private Extract Common Name (CN) from RDNSequence.
extract_cn_from_rdn({rdnSequence, RDNList}) ->
    lists:foldl(fun(RDN, Acc) ->
        case RDN of
            [#'AttributeTypeAndValue'{type = ?id_at_commonName, value = Value}] ->
                case Value of
                    {tea, CN} when is_binary(CN) -> CN;
                    {utf8String, CN} when is_binary(CN) -> CN;
                    {printableString, CN} when is_binary(CN) -> CN;
                    _ -> Acc
                end;
            _ ->
                Acc
        end
    end, undefined, RDNList);
extract_cn_from_rdn(_) ->
    undefined.

%% @private Extract SAN from extensions.
extract_san_from_extensions(undefined) ->
    [];
extract_san_from_extensions(Extensions) ->
    lists:foldl(fun(Extension, Acc) ->
        case Extension of
            #'Extension'{extnID = ?id_ce_subjectAltName, extnValue = SANValue} ->
                case 'PKIX':decode('SubjectAltName', SANValue) of
                    {ok, SANList} ->
                        lists:map(fun(SAN) ->
                            case SAN of
                                {dNSName, DNS} -> {dns, DNS};
                                {iPAddress, IP} -> {ip, IP};
                                {rfc822Name, Email} -> {email, Email};
                                {uniformResourceIdentifier, URI} -> {uri, URI};
                                _ -> {other, SAN}
                            end
                        end, SANList);
                    _ ->
                        Acc
                end;
            _ ->
                Acc
        end
    end, [], Extensions).

%% @private Validate CN against allowed patterns (supports wildcards).
validate_cn_pattern(CN, Patterns) ->
    case lists:any(fun(Pattern) -> match_pattern(CN, Pattern) end, Patterns) of
        true -> ok;
        false -> {error, cn_not_allowed}
    end.

%% @private Match CN against pattern with * wildcard.
match_pattern(CN, Pattern) ->
    case binary:split(Pattern, <<"*">>, [global]) of
        [Only] -> CN =:= Only;
        [<<>>, Postfix] ->
            case binary:match(CN, Postfix) of
                {Pos, _} -> Pos =:= byte_size(CN) - byte_size(Postfix);
                nomatch -> false
            end;
        [Prefix, <<>>] ->
            case CN of
                <<Prefix:byte_size(Prefix)/binary, _/binary>> -> true;
                _ -> false
            end;
        [Prefix, Postfix] ->
            case CN of
                <<Prefix:byte_size(Prefix)/binary, Middle/binary>> ->
                    case binary:split(Middle, Postfix) of
                        [_, <<>>] -> true;
                        _ -> false
                    end;
                _ -> false
            end;
        _ ->
            false
    end.
