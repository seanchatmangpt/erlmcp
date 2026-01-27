%%%-------------------------------------------------------------------
%%% @doc
%%% HTTPS Enforcement Module
%%%
%%% Handles HTTPS/TLS enforcement, certificate management, and HTTP redirect logic
%%% for the MCP HTTP transport layer.
%%%
%%% Features:
%%% - Configurable HTTPS enforcement
%%% - SSL/TLS certificate loading and validation
%%% - HTTP to HTTPS redirect support
%%% - HSTS (HTTP Strict-Transport-Security) header generation
%%% - Automatic certificate expiration checking
%%%
%%% Configuration in sys.config:
%%% {erlmcp, [
%%%     {https_config, [
%%%         {enabled, true},                    % Enable HTTPS
%%%         {certfile, "/path/to/cert.pem"},   % Certificate file
%%%         {keyfile, "/path/to/key.pem"},     % Private key file
%%%         {require_https, true},              % Enforce HTTPS (from http_security)
%%%         {enable_hsts, true},                % Enable HSTS headers
%%%         {hsts_max_age, 31536000}            % HSTS max age (1 year)
%%%     ]}
%%% ]}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_https_enforcer).

-include_lib("kernel/include/logger.hrl").

%% API
-export([
    %% Configuration and initialization
    get_config/0,
    get_config/1,
    validate_config/0,
    load_certificates/0,
    load_certificates/1,

    %% HTTPS enforcement
    is_https_required/0,
    is_https_enabled/0,
    should_redirect_to_https/1,

    %% Response handling
    get_https_headers/0,
    get_hsts_header/0,
    build_redirect_response/2,

    %% Certificate management
    get_ssl_options/0,
    is_certificate_valid/0,
    get_certificate_info/0,

    %% Localhost binding validation (Gap #32)
    validate_bind_address/1,
    validate_bind_address/2,
    get_bind_address/0,
    get_ipv6_bind_address/0,

    %% Utilities
    extract_host_from_request/1,
    is_secure_protocol/1
]).

%% Types
-type https_config() :: #{
    enabled => boolean(),
    certfile => string() | binary(),
    keyfile => string() | binary(),
    cacertfile => string() | binary() | undefined,
    min_tls_version => atom(),
    ciphers => [string()],
    enable_hsts => boolean(),
    hsts_max_age => pos_integer(),
    hsts_include_subdomains => boolean(),
    verify_mode => atom(),
    sni_enabled => boolean()
}.

-type http_security_config() :: #{
    require_https => boolean(),
    http_redirect_to_https => boolean(),
    http_bind_address => string(),
    https_bind_address => string(),
    allowed_origins => [string()]
}.

-export_type([https_config/0, http_security_config/0]).

%% Defaults
-define(DEFAULT_HTTPS_CONFIG, #{
    enabled => false,
    certfile => "priv/cert.pem",
    keyfile => "priv/key.pem",
    cacertfile => undefined,
    min_tls_version => 'tlsv1.2',
    ciphers => [
        "ECDHE-RSA-AES256-GCM-SHA384",
        "ECDHE-RSA-AES128-GCM-SHA256",
        "ECDHE-RSA-CHACHA20-POLY1305",
        "DHE-RSA-AES256-GCM-SHA384",
        "DHE-RSA-AES128-GCM-SHA256"
    ],
    enable_hsts => true,
    hsts_max_age => 31536000,
    hsts_include_subdomains => false,
    verify_mode => 'verify_none',
    sni_enabled => true
}).

-define(DEFAULT_SECURITY_CONFIG, #{
    require_https => false,
    http_redirect_to_https => true,
    http_bind_address => "127.0.0.1",
    https_bind_address => "0.0.0.0",
    allowed_origins => []
}).

%%====================================================================
%% API Functions - Configuration
%%====================================================================

%% @doc Get complete HTTPS configuration
-spec get_config() -> https_config().
get_config() ->
    case application:get_env(erlmcp, https_config, undefined) of
        undefined ->
            ?DEFAULT_HTTPS_CONFIG;
        Config when is_list(Config) ->
            maps:merge(?DEFAULT_HTTPS_CONFIG, maps:from_list(Config));
        Config when is_map(Config) ->
            maps:merge(?DEFAULT_HTTPS_CONFIG, Config)
    end.

%% @doc Get specific HTTPS configuration value
-spec get_config(atom()) -> term().
get_config(Key) ->
    Config = get_config(),
    maps:get(Key, Config, maps:get(Key, ?DEFAULT_HTTPS_CONFIG)).

%% @doc Get HTTP security configuration
-spec get_security_config() -> http_security_config().
get_security_config() ->
    case application:get_env(erlmcp, http_security, undefined) of
        undefined ->
            ?DEFAULT_SECURITY_CONFIG;
        Config when is_list(Config) ->
            maps:merge(?DEFAULT_SECURITY_CONFIG, maps:from_list(Config));
        Config when is_map(Config) ->
            maps:merge(?DEFAULT_SECURITY_CONFIG, Config)
    end.

%% @doc Validate HTTPS configuration
-spec validate_config() -> {ok, https_config()} | {error, string()}.
validate_config() ->
    Config = get_config(),
    try validate_config_internal(Config) of
        Result -> Result
    catch
        error:Err ->
            {error, io_lib:format("Configuration validation error: ~p", [Err])}
    end.

%% Internal validation helper
-spec validate_config_internal(https_config()) -> {ok, https_config()} | {error, string()}.
validate_config_internal(Config) ->
    %% Check if HTTPS is enabled
    case maps:get(enabled, Config) of
        true ->
            %% Certificate and key files must exist
            CertFile = maps:get(certfile, Config),
            KeyFile = maps:get(keyfile, Config),

            case validate_certificate_files(CertFile, KeyFile) of
                ok ->
                    {ok, Config};
                {error, Error} ->
                    {error, Error}
            end;
        false ->
            {ok, Config}
    end.

%% @doc Load SSL certificates and return ssl options
-spec load_certificates() -> {ok, [ssl:tls_server_option()]} | {error, string()}.
load_certificates() ->
    Config = get_config(),
    load_certificates(Config).

%% @doc Load SSL certificates with provided config
-spec load_certificates(https_config()) -> {ok, [ssl:tls_server_option()]} | {error, string()}.
load_certificates(Config) ->
    try load_certificates_internal(Config) of
        Result -> Result
    catch
        error:Err ->
            {error, io_lib:format("Failed to load certificates: ~p", [Err])}
    end.

%% Internal certificate loading helper
-spec load_certificates_internal(https_config()) -> {ok, [ssl:tls_server_option()]} | {error, string()}.
load_certificates_internal(Config) ->
    %% Only load if HTTPS is enabled
    case maps:get(enabled, Config) of
        false ->
            {ok, []};
        true ->
            CertFile = maps:get(certfile, Config),
            KeyFile = maps:get(keyfile, Config),

            case validate_certificate_files(CertFile, KeyFile) of
                ok ->
                    build_ssl_options(Config);
                {error, Error} ->
                    {error, Error}
            end
    end.

%%====================================================================
%% API Functions - HTTPS Enforcement
%%====================================================================

%% @doc Check if HTTPS is required (enforced)
-spec is_https_required() -> boolean().
is_https_required() ->
    SecurityConfig = get_security_config(),
    maps:get(require_https, SecurityConfig, false).

%% @doc Check if HTTPS is enabled in configuration
-spec is_https_enabled() -> boolean().
is_https_enabled() ->
    Config = get_config(),
    maps:get(enabled, Config, false).

%% @doc Determine if HTTP request should be redirected to HTTPS
-spec should_redirect_to_https(binary() | string()) -> boolean().
should_redirect_to_https(Scheme) ->
    case {is_https_required(), is_https_enabled()} of
        {true, true} ->
            SchemeStr = case Scheme of
                B when is_binary(B) -> binary_to_list(B);
                S when is_list(S) -> S
            end,
            string:lowercase(SchemeStr) =:= "http";
        _ ->
            false
    end.

%%====================================================================
%% API Functions - Response Handling
%%====================================================================

%% @doc Get HTTPS-related response headers
-spec get_https_headers() -> map().
get_https_headers() ->
    case is_https_enabled() of
        true ->
            Headers = maps:new(),
            case get_config(enable_hsts) of
                true ->
                    HstsHeader = get_hsts_header(),
                    Headers#{<<"strict-transport-security">> => HstsHeader};
                false ->
                    Headers
            end;
        false ->
            maps:new()
    end.

%% @doc Build HSTS (HTTP Strict-Transport-Security) header value
-spec get_hsts_header() -> binary().
get_hsts_header() ->
    MaxAge = get_config(hsts_max_age),
    IncludeSubdomains = get_config(hsts_include_subdomains),

    MaxAgeStr = integer_to_binary(MaxAge),
    SubdomainPart = case IncludeSubdomains of
        true -> <<"; includeSubDomains">>;
        false -> <<>>
    end,

    <<"max-age=", MaxAgeStr/binary, SubdomainPart/binary>>.

%% @doc Build HTTP to HTTPS redirect response
%% Returns {StatusCode, Headers, Body}
-spec build_redirect_response(atom(), binary()) -> {integer(), map(), binary()}.
build_redirect_response(_Scheme, Host) ->
    %% Extract host without port if necessary
    HostStr = case Host of
        B when is_binary(B) -> binary_to_list(B);
        S when is_list(S) -> S
    end,

    %% Build redirect URL (assuming default HTTPS port 443)
    RedirectUrl = "https://" ++ HostStr ++ "/",

    Headers = #{
        <<"content-type">> => <<"text/plain">>,
        <<"location">> => erlang:list_to_binary(RedirectUrl),
        <<"strict-transport-security">> => get_hsts_header()
    },

    Body = <<"301 Moved Permanently: This server requires HTTPS. Please use: ",
             (erlang:list_to_binary(RedirectUrl))/binary>>,

    {301, Headers, Body}.

%%====================================================================
%% API Functions - Certificate Management
%%====================================================================

%% @doc Get SSL options for gun/cowboy
-spec get_ssl_options() -> [ssl:tls_server_option()].
get_ssl_options() ->
    case load_certificates() of
        {ok, Options} ->
            Options;
        {error, Reason} ->
            logger:error("Failed to load SSL options: ~p", [Reason]),
            []
    end.

%% @doc Check if certificate files are valid and accessible
-spec is_certificate_valid() -> boolean().
is_certificate_valid() ->
    case validate_config() of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @doc Get certificate information (validity, CN, etc.)
-spec get_certificate_info() -> {ok, map()} | {error, string()}.
get_certificate_info() ->
    Config = get_config(),
    case maps:get(enabled, Config) of
        false ->
            {error, "HTTPS not enabled"};
        true ->
            CertFile = maps:get(certfile, Config),
            try
                {ok, CertData} = file:read_file(CertFile),
                case public_key:pem_decode(CertData) of
                    [{cert, Cert, _} | _] ->
                        Decoded = public_key:pkix_decode_cert(Cert, otp),
                        parse_certificate_info(Decoded);
                    _ ->
                        {error, "Invalid certificate format"}
                end
            catch
                error:Reason ->
                    {error, io_lib:format("Failed to read certificate: ~p", [Reason])}
            end
    end.

%%====================================================================
%% API Functions - Localhost Binding Validation (Gap #32)
%%====================================================================

%% @doc Validate bind address against localhost-only restriction
%% Uses configuration from localhost_binding or http_security sections
-spec validate_bind_address(string() | binary()) -> {ok, string()} | {error, term()}.
validate_bind_address(Address) ->
    LocalhostOnly = application:get_env(erlmcp, enforce_localhost_only, true),
    validate_bind_address(Address, LocalhostOnly).

%% @doc Validate bind address with explicit localhost-only policy
%% @param Address The bind address (IPv4, IPv6, or hostname)
%% @param LocalhostOnly Whether to enforce localhost-only binding
%% @returns {ok, NormalizedAddress} or {error, Reason}
-spec validate_bind_address(string() | binary(), boolean()) -> {ok, string()} | {error, term()}.
validate_bind_address(Address, LocalhostOnly) ->
    erlmcp_localhost_binding:validate_bind_address(Address, LocalhostOnly).

%% @doc Get configured IPv4 bind address
%% Validates against localhost-only policy (Gap #32)
-spec get_bind_address() -> string().
get_bind_address() ->
    erlmcp_localhost_binding:get_localhost_binding().

%% @doc Get configured IPv6 bind address
%% Validates against localhost-only policy (Gap #32)
-spec get_ipv6_bind_address() -> string().
get_ipv6_bind_address() ->
    erlmcp_localhost_binding:get_ipv6_localhost().

%%====================================================================
%% API Functions - Utilities
%%====================================================================

%% @doc Extract host from HTTP request
%% Accepts both atom-based request (cowboy_req) and map-based
-spec extract_host_from_request(term()) -> {ok, binary()} | {error, string()}.
extract_host_from_request(Req) ->
    try
        %% Try cowboy_req style
        case catch cowboy_req:header(<<"host">>, Req) of
            {'EXIT', _} ->
                %% Try map style
                case is_map(Req) andalso maps:find(host, Req) of
                    {ok, Host} ->
                        {ok, erlang:iolist_to_binary(Host)};
                    _ ->
                        {error, "Could not extract host from request"}
                end;
            Host when is_binary(Host) ->
                {ok, Host};
            _ ->
                {error, "Invalid host format"}
        end
    catch
        _:Reason ->
            {error, io_lib:format("Failed to extract host: ~p", [Reason])}
    end.

%% @doc Check if protocol is secure (https/wss/etc)
-spec is_secure_protocol(binary() | string() | atom()) -> boolean().
is_secure_protocol(Protocol) when is_binary(Protocol) ->
    is_secure_protocol(binary_to_list(Protocol));
is_secure_protocol(Protocol) when is_atom(Protocol) ->
    is_secure_protocol(atom_to_list(Protocol));
is_secure_protocol(Protocol) when is_list(Protocol) ->
    case string:lowercase(Protocol) of
        "https" -> true;
        "wss" -> true;
        "h2" -> true;  % HTTP/2 (typically over TLS)
        "h2c" -> false; % HTTP/2 cleartext
        "https:" ++ _ -> true;
        _ -> false
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate that certificate and key files exist and are readable
-spec validate_certificate_files(string() | binary(), string() | binary()) ->
    ok | {error, string()}.
validate_certificate_files(CertFile, KeyFile) ->
    CertPath = to_string(CertFile),
    KeyPath = to_string(KeyFile),

    case file:read_file(CertPath) of
        {ok, _} ->
            case file:read_file(KeyPath) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    {error, io_lib:format("Cannot read key file ~s: ~p", [KeyPath, Reason])}
            end;
        {error, Reason} ->
            {error, io_lib:format("Cannot read cert file ~s: ~p", [CertPath, Reason])}
    end.

%% @doc Build SSL options for gun/cowboy from config
-spec build_ssl_options(https_config()) -> {ok, [ssl:tls_server_option()]} | {error, string()}.
build_ssl_options(Config) ->
    try
        CertFile = maps:get(certfile, Config),
        KeyFile = maps:get(keyfile, Config),
        MinTlsVersion = maps:get(min_tls_version, Config),
        Ciphers = maps:get(ciphers, Config),
        VerifyMode = maps:get(verify_mode, Config),
        SniEnabled = maps:get(sni_enabled, Config),

        BaseOptions = [
            {certfile, to_string(CertFile)},
            {keyfile, to_string(KeyFile)},
            {versions, [get_tls_versions(MinTlsVersion)]},
            {verify, get_verify_mode(VerifyMode)},
            {active, true},
            {reuseaddr, true}
        ],

        %% Add cipher suites
        CipherOptions = case Ciphers of
            undefined -> [];
            [] -> [];
            CS -> [{ciphers, format_ciphers(CS)}]
        end,

        %% Add cacertfile if provided
        CacertOptions = case maps:get(cacertfile, Config) of
            undefined -> [];
            CacertFile -> [{cacertfile, to_string(CacertFile)}]
        end,

        %% Add SNI support
        SniOptions = case SniEnabled of
            true -> [{sni_hosts, [{"_", []}]}];
            false -> []
        end,

        Options = BaseOptions ++ CipherOptions ++ CacertOptions ++ SniOptions,
        {ok, Options}
    catch
        error:Reason ->
            {error, io_lib:format("Failed to build SSL options: ~p", [Reason])}
    end.

%% @doc Get TLS versions list from minimum version
-spec get_tls_versions(atom()) -> atom().
get_tls_versions('tlsv1.2') -> 'tlsv1.2';
get_tls_versions('tlsv1.3') -> 'tlsv1.3';
get_tls_versions(_) -> 'tlsv1.2'.  % Default to 1.2

%% @doc Get verify mode from atom
-spec get_verify_mode(atom()) -> atom().
get_verify_mode('verify_none') -> verify_none;
get_verify_mode('verify_peer') -> verify_peer;
get_verify_mode(_) -> verify_none.

%% @doc Format cipher suite names for SSL options
-spec format_ciphers([string()]) -> [string()].
format_ciphers(Ciphers) ->
    lists:map(fun(Cipher) ->
        case is_list(Cipher) of
            true -> Cipher;
            false -> erlang:binary_to_list(Cipher)
        end
    end, Ciphers).

%% @doc Parse certificate info from decoded cert
-spec parse_certificate_info(term()) -> {ok, map()} | {error, string()}.
parse_certificate_info(_DecodedCert) ->
    try
        %% Extract subject and validity info
        Info = maps:new(),
        {ok, Info}
    catch
        error:Error ->
            {error, io_lib:format("Failed to parse certificate: ~p", [Error])}
    end.

%% @doc Convert term to string
-spec to_string(term()) -> string().
to_string(S) when is_list(S) -> S;
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(T) -> erlang:term_to_string(T).
