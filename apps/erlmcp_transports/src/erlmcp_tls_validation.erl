%%%-------------------------------------------------------------------
%%% @doc
%%% TLS Validation and Configuration for erlmcp Transports
%%%
%%% This module provides secure TLS/SSL configuration for transport layers
%%% including TCP, HTTP, and WebSocket transports. It implements secure
%%% defaults with configurable options for both client and server roles.
%%%
%%% == Features ==
%%%
%%% - Secure TLS defaults (TLS 1.2+, strong ciphers, certificate validation)
%%% - Client and server TLS option builders
%%% - Certificate and key file validation
%%% - Configuration validation with helpful error messages
%%% - Support for SNI (Server Name Indication)
%%% - Forward secrecy through ephemeral cipher suites
%%%
%%% == Security Model ==
%%%
%%% 1. **Secure by Default**: verify_peer enabled, TLS 1.2+ only
%%% 2. **Strong Ciphers**: Only TLS 1.3 and Ephemeral Diffie-Hellman suites
%%% 3. **Certificate Validation**: Full chain validation with configurable depth
%%% 4. **No Legacy Protocols**: SSLv3, TLS 1.0, TLS 1.1 explicitly disabled
%%% 5. **Secure Renegotiation**: Always enabled to prevent MITM attacks
%%%
%%% == Usage ==
%%%
%%% ```
%%% %% Build client TLS options
%%% Config => #{
%%%     role => client,
%%%     certfile => "/path/to/client.crt",
%%%     keyfile => "/path/to/client.key",
%%%     cacertfile => "/path/to/ca.crt",
%%%     server_name_indication => "example.com"
%%% },
%%% {ok, ClientOpts} = erlmcp_tls_validation:build_tls_options(client, Config).
%%'
%%% %% Build server TLS options
%%% ServerConfig => #{
%%%     role => server,
%%%     certfile => "/path/to/server.crt",
%%%     keyfile => "/path/to/server.key",
%%%     cacertfile => "/path/to/ca.crt",
%%%     verify => verify_peer,
%%%     fail_if_no_peer_cert => true
%%% },
%%% {ok, ServerOpts} = erlmcp_tls_validation:build_tls_options(server, ServerConfig).
%%% '''
%%% %% Validate TLS configuration
%%% case erlmcp_tls_validation:validate_tls_config(Config) of
%%%     ok -> io:format("Configuration valid~n");
%%%     {error, Reason} -> io:format("Invalid config: ~p~n", [Reason])
%%% end.
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_tls_validation).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    build_tls_options/2,
    validate_tls_config/1,
    get_default_tls_opts/0,
    get_client_tls_opts/0,
    get_server_tls_opts/0,
    reload_config/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% Constants - Secure TLS Defaults
%%====================================================================

%% TLS protocol versions (minimum 1.2, no legacy)
-define(DEFAULT_TLS_VERSIONS, ['tlsv1.2', 'tlsv1.3']).

%% Strong cipher suites for TLS 1.3 and forward secrecy
-define(DEFAULT_CIPHERS, [
    %% TLS 1.3 cipher suites (most secure)
    "TLS_AES_128_GCM_SHA256",
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",

    %% TLS 1.2 cipher suites with forward secrecy (Ephemeral Diffie-Hellman)
    "ECDHE-ECDSA-AES128-GCM-SHA256",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-ECDSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-ECDSA-CHACHA20-POLY1305",
    "ECDHE-RSA-CHACHA20-POLY1305"
]).

%% Default verification mode
-define(DEFAULT_VERIFY, verify_peer).

%% Certificate chain validation depth
-define(DEFAULT_VERIFY_DEPTH, 5).

%%====================================================================
%% Type Definitions
%%====================================================================

-type tls_role() :: client | server.
-type tls_version() :: 'tlsv1.2' | 'tlsv1.3'.
-type verify_mode() :: verify_none | verify_peer.

-type tls_config() :: #{
    role := tls_role(),
    certfile => file:filename_all(),
    keyfile => file:filename_all(),
    cacertfile => file:filename_all(),
    dhfile => file:filename_all(),
    verify => verify_mode(),
    verify_fun => {fun(), term()},
    fail_if_no_peer_cert => boolean(),
    depth => non_neg_integer(),
    password => string(),
    ciphers => [string()],
    versions => [tls_version()],
    server_name_indication => disable | string(),
    customize_hostname_check => map(),
    secure_renegotiate => boolean(),
    honor_cipher_order => boolean(),
    session_tickets => boolean(),
    reuse_session => boolean(),
    client_renegotiation => boolean(),
    max_empty_record => non_neg_integer()
}.

-type tls_options() :: [ssl:tls_client_option() | ssl:tls_server_option()].
-type validation_error() :: {invalid_config, term()} | {missing_file, file:filename_all()}.

-export_type([tls_role/0, tls_config/0, tls_options/0, tls_version/0, verify_mode/0, validation_error/0]).

%%====================================================================
%% State Record
%%====================================================================

-record(state, {
    cached_client_opts :: tls_options() | undefined,
    cached_server_opts :: tls_options() | undefined,
    config_timestamp :: integer() | undefined
}).

-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the TLS validation server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Build TLS options from configuration map
%% Returns a proplist suitable for ssl:connect/3 (client) or ssl:listen/2 (server)
%%
%% Configuration keys:
%% - role: client | server (required)
%% - certfile: Path to certificate file (optional, required for mTLS)
%% - keyfile: Path to private key file (optional, required for mTLS)
%% - cacertfile: Path to CA certificate file (optional, required for verify_peer)
%% - verify: verify_peer | verify_none (default: verify_peer)
%% - server_name_indication: SNI hostname (default: disable)
%% - password: Private key password (optional)
%% - ciphers: List of cipher suites (default: secure defaults)
%% - versions: List of TLS versions (default: ['tlsv1.2', 'tlsv1.3'])
-spec build_tls_options(tls_role(), tls_config()) -> {ok, tls_options()} | {error, validation_error()}.
build_tls_options(Role, Config) when is_map(Config) ->
    case validate_tls_config(Config) of
        ok ->
            try
                Opts = case Role of
                    client -> build_client_tls_opts(Config);
                    server -> build_server_tls_opts(Config)
                end,
                {ok, Opts}
            catch
                Type:Error:Stacktrace ->
                    ?LOG_ERROR("Failed to build TLS options: ~p:~p~n~p",
                               [Type, Error, Stacktrace]),
                    {error, {build_failed, {Type, Error}}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate TLS configuration map
%% Returns ok if valid, {error, Reason} otherwise
-spec validate_tls_config(tls_config()) -> ok | {error, validation_error()}.
validate_tls_config(Config) when is_map(Config) ->
    Required = [role],
    checks([
        fun() -> validate_required_keys(Config, Required) end,
        fun() -> validate_role(Config) end,
        fun() -> validate_versions(Config) end,
        fun() -> validate_ciphers(Config) end,
        fun() -> validate_verify_mode(Config) end,
        fun() -> validate_certificates_exist(Config) end
    ]);
validate_tls_config(_Config) ->
    {error, {invalid_config, "Configuration must be a map"}}.

%% @doc Get default TLS options for any role
-spec get_default_tls_opts() -> tls_options().
get_default_tls_opts() ->
    [
        binary,
        {active, false},
        {verify, ?DEFAULT_VERIFY},
        {versions, ?DEFAULT_TLS_VERSIONS},
        {ciphers, ?DEFAULT_CIPHERS},
        {secure_renegotiate, true},
        {honor_cipher_order, true},
        {depth, ?DEFAULT_VERIFY_DEPTH},
        {client_renegotiation, false}
    ].

%% @doc Get cached client TLS options
-spec get_client_tls_opts() -> {ok, tls_options()} | {error, not_cached}.
get_client_tls_opts() ->
    gen_server:call(?SERVER, get_client_opts).

%% @doc Get cached server TLS options
-spec get_server_tls_opts() -> {ok, tls_options()} | {error, not_cached}.
get_server_tls_opts() ->
    gen_server:call(?SERVER, get_server_opts).

%% @doc Reload TLS configuration and clear cache
-spec reload_config() -> ok.
reload_config() ->
    gen_server:call(?SERVER, reload_config).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(get_client_opts, _From, #state{cached_client_opts = undefined} = State) ->
    {reply, {error, not_cached}, State};
handle_call(get_client_opts, _From, #state{cached_client_opts = Opts} = State) ->
    {reply, {ok, Opts}, State};

handle_call(get_server_opts, _From, #state{cached_server_opts = undefined} = State) ->
    {reply, {error, not_cached}, State};
handle_call(get_server_opts, _From, #state{cached_server_opts = Opts} = State) ->
    {reply, {ok, Opts}, State};

handle_call(reload_config, _From, State) ->
    {reply, ok, State#state{
        cached_client_opts = undefined,
        cached_server_opts = undefined,
        config_timestamp = undefined
    }};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions - Option Builders
%%%===================================================================

%% @private Build client TLS options
build_client_tls_opts(Config) ->
    BaseOpts = get_default_tls_opts(),
    CertOpts = build_cert_options(Config),
    SNI = build_sni_options(Config),
    CustomOpts = build_custom_options(Config),

    %% Combine all options, with custom options taking precedence
    lists:keysort(1, merge_options([BaseOpts, CertOpts, SNI, CustomOpts])).

%% @private Build server TLS options
build_server_tls_opts(Config) ->
    BaseOpts = get_default_tls_opts(),
    CertOpts = build_cert_options(Config),
    VerifyOpts = build_server_verify_options(Config),
    SessionOpts = build_session_options(Config),
    CustomOpts = build_custom_options(Config),

    %% Combine all options, with custom options taking precedence
    lists:keysort(1, merge_options([BaseOpts, CertOpts, VerifyOpts, SessionOpts, CustomOpts])).

%% @private Build certificate and key options
build_cert_options(Config) ->
    Opts = [],
    Opts1 = case maps:get(certfile, Config, undefined) of
        undefined -> Opts;
        CertFile -> [{certfile, CertFile} | Opts]
    end,
    Opts2 = case maps:get(keyfile, Config, undefined) of
        undefined -> Opts1;
        KeyFile -> [{keyfile, KeyFile} | Opts1]
    end,
    Opts3 = case maps:get(cacertfile, Config, undefined) of
        undefined -> Opts2;
        CaCertFile -> [{cacertfile, CaCertFile} | Opts2]
    end,
    Opts4 = case maps:get(dhfile, Config, undefined) of
        undefined -> Opts3;
        DHFile -> [{dhfile, DHFile} | Opts3]
    end,
    Opts5 = case maps:get(password, Config, undefined) of
        undefined -> Opts4;
        Password -> [{password, Password} | Opts4]
    end,
    lists:reverse(Opts5).

%% @private Build SNI (Server Name Indication) options for clients
build_sni_options(Config) ->
    case maps:get(server_name_indication, Config, disable) of
        disable -> [];
        SNI -> [{server_name_indication, SNI}]
    end.

%% @private Build server-specific verification options
build_server_verify_options(Config) ->
    Opts = [],
    Opts1 = case maps:get(fail_if_no_peer_cert, Config, false) of
        false -> Opts;
        true -> [{fail_if_no_peer_cert, true} | Opts]
    end,
    Opts2 = case maps:get(verify_fun, Config, undefined) of
        undefined -> Opts1;
        VerifyFun -> [{verify_fun, VerifyFun} | Opts1]
    end,
    Opts3 = case maps:get(depth, Config, undefined) of
        undefined -> Opts2;
        Depth -> [{depth, Depth} | Opts2]
    end,
    lists:reverse(Opts3).

%% @private Build session-related options
build_session_options(Config) ->
    Opts = [],
    Opts1 = case maps:get(session_tickets, Config, true) of
        true -> Opts;
        false -> [{session_tickets, disabled} | Opts]
    end,
    Opts2 = case maps:get(reuse_session, Config, true) of
        true -> [{reuse_sessions, true} | Opts1];
        false -> [{reuse_sessions, false} | Opts1]
    end,
    lists:reverse(Opts2).

%% @private Build custom override options
build_custom_options(Config) ->
    %% Extract any SSL-specific options that aren't in our standard set
    CustomKeys = [
        customize_hostname_check,
        max_empty_record,
        client_renegotiation,
        honor_cipher_order,
        secure_renegotiate,
        hibernate,
        log_alert,
        keep_secrets,
        signature_algs,
        supported_groups,
        padding_check,
        beast_mitigation,
        send_close_after,
        receive_close_after
    ],

    lists:foldl(fun(Key, Acc) ->
        case maps:get(Key, Config, undefined) of
            undefined -> Acc;
            Value -> [{Key, Value} | Acc]
        end
    end, [], CustomKeys).

%% @private Merge multiple option lists, later values override earlier ones
merge_options(OptionLists) ->
    %% Convert to map, merge, then convert back to proplist
    MergedMap = lists:foldl(fun(Opts, AccMap) ->
        lists:foldl(fun
            ({K, _V} = Pair, Map) when is_atom(K) ->
                maps:put(K, Pair, Map);
            ({K, _V} = Pair, Map) when is_tuple(K) ->
                maps:put(K, Pair, Map);
            (Other, Map) ->
                maps:put(element(1, Other), Other, Map)
        end, AccMap, Opts)
    end, #{}, OptionLists),

    %% Extract values from map
    maps:fold(fun(_K, V, Acc) -> [V | Acc] end, [], MergedMap).

%%%===================================================================
%%% Internal Functions - Validation
%%%===================================================================

%% @private Run all validation checks
checks(CheckFuns) ->
    case lists:foldl(fun(CheckFun, Acc) ->
        case Acc of
            {error, _} -> Acc;  %% Stop on first error
            ok -> CheckFun()
        end
    end, ok, CheckFuns) of
        {error, _} = Error -> Error;
        ok -> ok
    end.

%% @private Validate required configuration keys
validate_required_keys(Config, RequiredKeys) ->
    MissingKeys = lists:filter(fun(Key) ->
        not maps:is_key(Key, Config)
    end, RequiredKeys),

    case MissingKeys of
        [] ->
            ok;
        _ ->
            {error, {invalid_config, {missing_keys, MissingKeys}}}
    end.

%% @private Validate role is either client or server
validate_role(Config) ->
    case maps:get(role, Config) of
        Role when Role =:= client orelse Role =:= server ->
            ok;
        InvalidRole ->
            {error, {invalid_config, {invalid_role, InvalidRole}}}
    end.

%% @private Validate TLS versions
validate_versions(Config) ->
    case maps:get(versions, Config, undefined) of
        undefined ->
            ok;  %% Will use defaults
        Versions when is_list(Versions) ->
            ValidVersions = ?DEFAULT_TLS_VERSIONS,
            InvalidVersions = lists:filter(fun(V) ->
                not lists:member(V, ValidVersions)
            end, Versions),

            case InvalidVersions of
                [] -> ok;
                _ -> {error, {invalid_config, {invalid_versions, InvalidVersions}}}
            end;
        _ ->
            {error, {invalid_config, versions_must_be_list}}
    end.

%% @private Validate cipher suites
validate_ciphers(Config) ->
    case maps:get(ciphers, Config, undefined) of
        undefined ->
            ok;  %% Will use defaults
        Ciphers when is_list(Ciphers) ->
            %% Check that cipher strings are valid (basic format check)
            InvalidCiphers = lists:filter(fun(Cipher) when is_binary(Cipher) ->
                not is_valid_cipher_string(Cipher);
            (Cipher) when is_list(Cipher) ->
                not is_valid_cipher_string(Cipher);
            (_) ->
                true
        end, Ciphers),

            case InvalidCiphers of
                [] -> ok;
                _ -> {error, {invalid_config, {invalid_ciphers, InvalidCiphers}}}
            end;
        _ ->
            {error, {invalid_config, ciphers_must_be_list}}
    end.

%% @private Check if cipher string is valid format
is_valid_cipher_string(Cipher) when is_binary(Cipher) ->
    byte_size(Cipher) > 0 andalso byte_size(Cipher) < 256;
is_valid_cipher_string(Cipher) when is_list(Cipher) ->
    length(Cipher) > 0 andalso length(Cipher) < 256.

%% @private Validate verify mode
validate_verify_mode(Config) ->
    case maps:get(verify, Config, undefined) of
        undefined -> ok;
        verify_none -> ok;
        verify_peer -> ok;
        InvalidMode ->
            {error, {invalid_config, {invalid_verify_mode, InvalidMode}}}
    end.

%% @private Validate certificate files exist and are readable
validate_certificates_exist(Config) ->
    FilesToCheck = [
        maps:get(certfile, Config, undefined),
        maps:get(keyfile, Config, undefined),
        maps:get(cacertfile, Config, undefined),
        maps:get(dhfile, Config, undefined)
    ],

    ExistingFiles = lists:filter(fun(undefined) -> false;
                                    (_) -> true
                                 end, FilesToCheck),

    case check_files_readable(ExistingFiles) of
        ok -> ok;
        {error, File} -> {error, {missing_file, File}}
    end.

%% @private Check that files are readable
check_files_readable([]) ->
    ok;
check_files_readable([File | Rest]) ->
    case file:read_file_info(File) of
        {ok, _FileInfo} ->
            check_files_readable(Rest);
        {error, _Reason} ->
            {error, File}
    end.
