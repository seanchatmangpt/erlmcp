-module(erlmcp_tls_validation).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    build_tls_options/2,
    validate_tls_options/1,
    validate_peer_verification/1,
    validate_hostname_verification/1,
    validate_cert_chain_depth/1,
    validate_cert_validity/1,
    validate_cert_issuer/2,
    validate_hostname/2,
    validate_cert_pinning/1,
    validate_minimum_version/1,
    validate_ciphers/1,
    validate_sni_hostname/2,
    is_verification_enabled/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal types
-type tls_option() :: {atom(), term()}.
-type tls_options() :: [tls_option()].
-type hostname() :: string() | binary().
-type verify_mode() :: verify_peer | verify_peer_fail_if_no_cert | verify_none.

-record(state, {
    pinned_certs = #{} :: map(),
    tls_config = #{} :: map()
}).

-include_lib("kernel/include/logger.hrl").

%% Default TLS configuration
-define(DEFAULT_MIN_TLS_VERSION, 'tlsv1.2').
-define(DEFAULT_VERIFY_MODE, verify_peer).
-define(DEFAULT_DEPTH, 3).
-define(DEFAULT_CIPHERS, [
    "ECDHE-RSA-AES256-GCM-SHA384",
    "ECDHE-RSA-AES128-GCM-SHA256",
    "ECDHE-RSA-CHACHA20-POLY1305",
    "DHE-RSA-AES256-GCM-SHA384",
    "DHE-RSA-AES128-GCM-SHA256"
]).

%% Weak ciphers to reject
-define(WEAK_CIPHERS, [
    "NULL", "EXPORT", "DES", "RC4", "MD5", "PSK", "DES-CBC3-SHA",
    "DES-CBC-SHA", "IDEA-CBC-SHA", "RC2-CBC-MD5", "AES128-SHA", "AES256-SHA"
]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec build_tls_options(tls_options(), hostname()) ->
    {ok, tls_options()} | {error, term()}.
build_tls_options(UserOpts, Hostname) ->
    gen_server:call(?MODULE, {build_tls_options, UserOpts, Hostname}).

-spec validate_tls_options(tls_options()) -> ok | {error, term()}.
validate_tls_options(Opts) ->
    gen_server:call(?MODULE, {validate_tls_options, Opts}).

-spec validate_peer_verification(tls_options()) -> ok | {error, term()}.
validate_peer_verification(Opts) ->
    gen_server:call(?MODULE, {validate_peer_verification, Opts}).

-spec validate_hostname_verification(tls_options()) -> ok | {error, term()}.
validate_hostname_verification(Opts) ->
    gen_server:call(?MODULE, {validate_hostname_verification, Opts}).

-spec validate_cert_chain_depth(tls_options()) -> ok | {error, term()}.
validate_cert_chain_depth(Opts) ->
    gen_server:call(?MODULE, {validate_cert_chain_depth, Opts}).

-spec validate_cert_validity(map()) -> ok | {error, term()}.
validate_cert_validity(Cert) ->
    gen_server:call(?MODULE, {validate_cert_validity, Cert}).

-spec validate_cert_issuer(map(), term()) -> ok | {error, term()}.
validate_cert_issuer(Cert, PinnedCerts) ->
    gen_server:call(?MODULE, {validate_cert_issuer, Cert, PinnedCerts}).

-spec validate_hostname(hostname(), hostname()) -> ok | {error, term()}.
validate_hostname(RequestHostname, CertHostname) ->
    gen_server:call(?MODULE, {validate_hostname, RequestHostname, CertHostname}).

-spec validate_cert_pinning(tls_options()) -> ok | {error, term()}.
validate_cert_pinning(Opts) ->
    gen_server:call(?MODULE, {validate_cert_pinning, Opts}).

-spec validate_minimum_version(list()) -> ok | {error, term()}.
validate_minimum_version(Versions) ->
    gen_server:call(?MODULE, {validate_minimum_version, Versions}).

-spec validate_ciphers(list()) -> ok | {error, term()}.
validate_ciphers(Ciphers) ->
    gen_server:call(?MODULE, {validate_ciphers, Ciphers}).

-spec validate_sni_hostname(tls_options(), hostname()) -> ok | {error, term()}.
validate_sni_hostname(Opts, Hostname) ->
    gen_server:call(?MODULE, {validate_sni_hostname, Opts, Hostname}).

-spec is_verification_enabled(verify_mode()) -> boolean().
is_verification_enabled(verify_peer) ->
    true;
is_verification_enabled(verify_peer_fail_if_no_cert) ->
    true;
is_verification_enabled(verify_none) ->
    false.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.

handle_call({build_tls_options, UserOpts, Hostname}, _From, State) ->
    Result = do_build_tls_options(UserOpts, Hostname),
    {reply, Result, State};

handle_call({validate_tls_options, Opts}, _From, State) ->
    Result = do_validate_tls_options(Opts),
    {reply, Result, State};

handle_call({validate_peer_verification, Opts}, _From, State) ->
    Result = do_validate_peer_verification(Opts),
    {reply, Result, State};

handle_call({validate_hostname_verification, Opts}, _From, State) ->
    Result = do_validate_hostname_verification(Opts),
    {reply, Result, State};

handle_call({validate_cert_chain_depth, Opts}, _From, State) ->
    Result = do_validate_cert_chain_depth(Opts),
    {reply, Result, State};

handle_call({validate_cert_validity, Cert}, _From, State) ->
    Result = do_validate_cert_validity(Cert),
    {reply, Result, State};

handle_call({validate_cert_issuer, Cert, PinnedCerts}, _From, State) ->
    Result = do_validate_cert_issuer(Cert, PinnedCerts),
    {reply, Result, State};

handle_call({validate_hostname, RequestHostname, CertHostname}, _From, State) ->
    Result = do_validate_hostname(RequestHostname, CertHostname),
    {reply, Result, State};

handle_call({validate_cert_pinning, Opts}, _From, State) ->
    Result = do_validate_cert_pinning(Opts),
    {reply, Result, State};

handle_call({validate_minimum_version, Versions}, _From, State) ->
    Result = do_validate_minimum_version(Versions),
    {reply, Result, State};

handle_call({validate_ciphers, Ciphers}, _From, State) ->
    Result = do_validate_ciphers(Ciphers),
    {reply, Result, State};

handle_call({validate_sni_hostname, Opts, Hostname}, _From, State) ->
    Result = do_validate_sni_hostname(Opts, Hostname),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - TLS option building
%%====================================================================

-spec do_build_tls_options(tls_options(), hostname()) ->
    {ok, tls_options()} | {error, term()}.
do_build_tls_options(UserOpts, Hostname) ->
    case do_validate_peer_verification(UserOpts) of
        ok ->
            case do_validate_hostname_verification(UserOpts) of
                ok ->
                    case do_validate_tls_options(UserOpts) of
                        ok ->
                            %% Build final options with sensible defaults
                            FinalOpts = build_final_options(UserOpts, Hostname),
                            {ok, FinalOpts};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec build_final_options(tls_options(), hostname()) -> tls_options().
build_final_options(UserOpts, Hostname) ->
    %% Merge user options with safe defaults
    Defaults = [
        {verify, ?DEFAULT_VERIFY_MODE},
        {server_name_indication, to_list(Hostname)},
        {depth, ?DEFAULT_DEPTH},
        {versions, ['tlsv1.2', 'tlsv1.3']}
    ],

    %% User options override defaults
    merge_options(Defaults, UserOpts).

-spec merge_options(tls_options(), tls_options()) -> tls_options().
merge_options(Defaults, User) ->
    UserKeys = [element(1, Opt) || Opt <- User],
    Filtered = [D || D <- Defaults, not lists:member(element(1, D), UserKeys)],
    Filtered ++ User.

-spec to_list(term()) -> string().
to_list(S) when is_list(S) -> S;
to_list(B) when is_binary(B) -> binary_to_list(B);
to_list(A) when is_atom(A) -> atom_to_list(A).

%%====================================================================
%% Internal functions - Validation
%%====================================================================

-spec do_validate_tls_options(tls_options()) -> ok | {error, term()}.
do_validate_tls_options(Opts) ->
    case do_validate_peer_verification(Opts) of
        ok ->
            case do_validate_minimum_version(proplists:get_value(versions, Opts, [])) of
                ok ->
                    case do_validate_ciphers(proplists:get_value(ciphers, Opts, [])) of
                        ok -> do_validate_cert_chain_depth(Opts);
                        Error -> Error
                    end;
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

-spec do_validate_peer_verification(tls_options()) -> ok | {error, term()}.
do_validate_peer_verification(Opts) ->
    case proplists:get_value(verify, Opts, ?DEFAULT_VERIFY_MODE) of
        verify_none ->
            logger:error("TLS verification disabled - certificate validation disabled"),
            {error, verification_disabled};
        verify_peer ->
            ok;
        verify_peer_fail_if_no_cert ->
            ok;
        _ ->
            {error, invalid_verify_mode}
    end.

-spec do_validate_hostname_verification(tls_options()) -> ok | {error, term()}.
do_validate_hostname_verification(Opts) ->
    HasSNI = proplists:is_defined(server_name_indication, Opts),
    HasVerifyFun = proplists:is_defined(verify_fun, Opts),

    case HasSNI orelse HasVerifyFun of
        true ->
            ok;
        false ->
            logger:error("No hostname verification configured"),
            {error, no_hostname_verification}
    end.

-spec do_validate_cert_chain_depth(tls_options()) -> ok | {error, term()}.
do_validate_cert_chain_depth(Opts) ->
    Depth = proplists:get_value(depth, Opts, ?DEFAULT_DEPTH),
    case Depth > 0 andalso Depth =< 10 of
        true ->
            ok;
        false ->
            logger:error("Invalid certificate chain depth: ~p", [Depth]),
            {error, invalid_chain_depth}
    end.

-spec do_validate_cert_validity(map()) -> ok | {error, term()}.
do_validate_cert_validity(Cert) ->
    case maps:get(not_valid_after, Cert, undefined) of
        {Y, M, D} ->
            case calendar:valid_date(Y, M, D) of
                true ->
                    Now = calendar:local_time(),
                    case calendar:datetime_to_gregorian_seconds(Now) <
                         calendar:datetime_to_gregorian_seconds({calendar:date_to_gregorian_days({Y, M, D}), {0, 0, 0}}) of
                        true ->
                            ok;
                        false ->
                            logger:error("Certificate expired: ~p-~p-~p", [Y, M, D]),
                            {error, cert_expired}
                    end;
                false ->
                    {error, invalid_cert_date}
            end;
        undefined ->
            ok;
        _ ->
            {error, invalid_validity_format}
    end.

-spec do_validate_cert_issuer(map(), term()) -> ok | {error, term()}.
do_validate_cert_issuer(Cert, PinnedCerts) ->
    SelfSigned = maps:get(self_signed, Cert, false),
    case SelfSigned of
        true ->
            case PinnedCerts of
                undefined ->
                    logger:error("Self-signed certificate without pinning"),
                    {error, self_signed_not_pinned};
                _ ->
                    %% TODO: Implement pinning verification
                    ok
            end;
        false ->
            ok
    end.

-spec do_validate_hostname(hostname(), hostname()) -> ok | {error, term()}.
do_validate_hostname(RequestHostname, CertHostname) ->
    ReqHost = string:lowercase(to_list(RequestHostname)),
    CertHost = string:lowercase(to_list(CertHostname)),

    case match_hostname(ReqHost, CertHost) of
        true ->
            ok;
        false ->
            logger:error("Hostname mismatch: requested ~s, certificate ~s", [ReqHost, CertHost]),
            {error, hostname_mismatch}
    end.

-spec match_hostname(string(), string()) -> boolean().
match_hostname(Hostname, Hostname) ->
    true;
match_hostname("*." ++ Rest1, Hostname) ->
    case string:split(Hostname, ".") of
        [_Subdomain | Rest2] ->
            match_hostname("." ++ Rest1, "." ++ string:join(Rest2, "."));
        _ ->
            false
    end;
match_hostname(_, _) ->
    false.

-spec do_validate_cert_pinning(tls_options()) -> ok | {error, term()}.
do_validate_cert_pinning(Opts) ->
    case proplists:get_value(pinned_certs, Opts) of
        undefined ->
            %% Pinning is optional
            ok;
        PinnedCerts when is_list(PinnedCerts) ->
            %% Verify pinned certs are valid format
            case lists:all(fun is_valid_pin_hash/1, PinnedCerts) of
                true -> ok;
                false -> {error, invalid_pin_format}
            end;
        _ ->
            {error, invalid_pinned_certs}
    end.

-spec is_valid_pin_hash(term()) -> boolean().
is_valid_pin_hash(Hash) when is_binary(Hash), size(Hash) =:= 32 ->
    true; % SHA-256 hash
is_valid_pin_hash(Hash) when is_binary(Hash), size(Hash) =:= 20 ->
    true; % SHA-1 hash
is_valid_pin_hash(_) ->
    false.

-spec do_validate_minimum_version(list()) -> ok | {error, term()}.
do_validate_minimum_version([]) ->
    ok;
do_validate_minimum_version(Versions) ->
    case lists:all(fun is_valid_tls_version/1, Versions) of
        true ->
            case has_min_version(Versions) of
                true -> ok;
                false ->
                    logger:error("TLS version too old: ~p", [Versions]),
                    {error, tls_version_too_old}
            end;
        false ->
            {error, invalid_tls_version}
    end.

-spec is_valid_tls_version(term()) -> boolean().
is_valid_tls_version('tlsv1.2') -> true;
is_valid_tls_version('tlsv1.3') -> true;
is_valid_tls_version(_) -> false.

-spec has_min_version(list()) -> boolean().
has_min_version(Versions) ->
    lists:any(fun(V) -> V =:= 'tlsv1.2' orelse V =:= 'tlsv1.3' end, Versions).

-spec do_validate_ciphers(list()) -> ok | {error, term()}.
do_validate_ciphers([]) ->
    ok;
do_validate_ciphers(Ciphers) ->
    case lists:any(fun is_weak_cipher/1, Ciphers) of
        true ->
            logger:error("Weak cipher detected in suite"),
            {error, weak_cipher};
        false ->
            ok
    end.

-spec is_weak_cipher(string()) -> boolean().
is_weak_cipher(Cipher) ->
    lists:any(fun(Weak) ->
        string:find(Cipher, Weak) =/= nomatch
    end, ?WEAK_CIPHERS).

-spec do_validate_sni_hostname(tls_options(), hostname()) -> ok | {error, term()}.
do_validate_sni_hostname(Opts, Hostname) ->
    case proplists:get_value(server_name_indication, Opts) of
        undefined ->
            {error, sni_not_configured};
        SNIHostname ->
            do_validate_hostname(to_list(SNIHostname), Hostname)
    end.
