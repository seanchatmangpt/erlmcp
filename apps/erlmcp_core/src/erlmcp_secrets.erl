%%%-------------------------------------------------------------------
%%% @doc erlmcp_secrets - Secrets Management Module
%%% Integrates with HashiCorp Vault, AWS Secrets Manager
%%% with encrypted local storage fallback (AES-256-GCM).
%%%
%%% Design:
%%% - gen_server for secret lifecycle management
%%% - ETS cache with TTL for performance
%%% - Encrypted local storage for offline mode
%%% - Secret rotation support
%%% - Integration with external secret stores
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secrets).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    get_secret/1,
    set_secret/2,
    delete_secret/1,
    rotate_secret/1,
    list_secrets/0,
    configure_vault/1,
    configure_aws/1,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type secret_key() :: binary().
-type secret_value() :: binary().
-type backend() :: vault | aws_secrets_manager | local_encrypted.

-export_type([secret_key/0, secret_value/0, backend/0]).

%% State record
-record(state, {
    cache :: ets:tid(),              % secret_key -> {value, expires_at}
    backend :: backend(),
    backend_config :: map(),
    encryption_key :: binary(),
    ttl_seconds :: pos_integer(),
    storage_path :: file:filename()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Get secret by key.
-spec get_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.
get_secret(Key) ->
    gen_server:call(?MODULE, {get_secret, Key}, 10000).

%% @doc Set secret (store in backend).
-spec set_secret(secret_key(), secret_value()) -> ok | {error, term()}.
set_secret(Key, Value) ->
    gen_server:call(?MODULE, {set_secret, Key, Value}, 10000).

%% @doc Delete secret from backend.
-spec delete_secret(secret_key()) -> ok | {error, term()}.
delete_secret(Key) ->
    gen_server:call(?MODULE, {delete_secret, Key}, 10000).

%% @doc Rotate secret (generate new value, update backend).
-spec rotate_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.
rotate_secret(Key) ->
    gen_server:call(?MODULE, {rotate_secret, Key}, 10000).

%% @doc List all secret keys.
-spec list_secrets() -> {ok, [secret_key()]} | {error, term()}.
list_secrets() ->
    gen_server:call(?MODULE, list_secrets, 10000).

%% @doc Configure HashiCorp Vault backend.
-spec configure_vault(map()) -> ok.
configure_vault(Config) ->
    gen_server:call(?MODULE, {configure_backend, vault, Config}, 10000).

%% @doc Configure AWS Secrets Manager backend.
-spec configure_aws(map()) -> ok.
configure_aws(Config) ->
    gen_server:call(?MODULE, {configure_backend, aws_secrets_manager, Config}, 10000).

%% @doc Stop secrets manager.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    Backend = maps:get(backend, Config, local_encrypted),
    BackendConfig = maps:get(backend_config, Config, #{}),
    TtlSeconds = maps:get(ttl_seconds, Config, 300),  % 5 minutes default
    StoragePath = maps:get(storage_path, Config, "priv/secrets/secrets.enc"),

    % SECURITY FIX (P1): Move blocking file I/O to async init to prevent supervisor delays.
    % Generate or load encryption key asynchronously after init returns.
    State = #state{
        cache = ets:new(secrets_cache, [set, protected]),
        backend = Backend,
        backend_config = BackendConfig,
        encryption_key = undefined,  % Will be set in async init
        ttl_seconds = TtlSeconds,
        storage_path = StoragePath
    },

    % Start cache cleanup timer
    erlang:send_after(60000, self(), cleanup_cache),

    % Trigger async initialization (file I/O happens after init returns)
    self() ! {init_async, Config},

    logger:info("Secrets manager started with backend: ~p (async init pending)", [Backend]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({get_secret, Key}, _From, State) ->
    Result = do_get_secret(Key, State),
    {reply, Result, State};

handle_call({set_secret, Key, Value}, _From, State) ->
    Result = do_set_secret(Key, Value, State),
    {reply, Result, State};

handle_call({delete_secret, Key}, _From, State) ->
    Result = do_delete_secret(Key, State),
    % Also clear cache
    ets:delete(State#state.cache, Key),
    {reply, Result, State};

handle_call({rotate_secret, Key}, _From, State) ->
    Result = do_rotate_secret(Key, State),
    {reply, Result, State};

handle_call(list_secrets, _From, State) ->
    Result = do_list_secrets(State),
    {reply, Result, State};

handle_call({configure_backend, Backend, Config}, _From, State) ->
    NewState = State#state{
        backend = Backend,
        backend_config = Config
    },
    logger:info("Backend configured: ~p", [Backend]),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({init_async, Config}, State) ->
    % SECURITY FIX (P1): Perform blocking file I/O asynchronously after init/1 returns.
    % This prevents supervisor startup delays while maintaining OTP best practices.
    EncryptionKey = load_or_generate_encryption_key(Config),
    StorageDir = filename:dirname(State#state.storage_path),
    ok = filelib:ensure_dir(StorageDir ++ "/"),
    logger:info("Async initialization complete, encryption key loaded"),
    {noreply, State#state{encryption_key = EncryptionKey}};

handle_info(cleanup_cache, State) ->
    Now = erlang:system_time(second),
    ets:foldl(fun({Key, {_Value, ExpiresAt}}, Acc) ->
        case ExpiresAt < Now of
            true -> ets:delete(State#state.cache, Key);
            false -> ok
        end,
        Acc
    end, ok, State#state.cache),
    erlang:send_after(60000, self(), cleanup_cache),
    {noreply, State};

handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    % Handle gun connection process death
    % These are temporary monitors created during HTTP requests to Vault/AWS
    % The actual request will fail with timeout/error, so we just log this
    logger:warning("Gun connection process ~p died during request: ~p (monitor: ~p)",
                   [Pid, Reason, MonitorRef]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.cache),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Get secret with caching.
do_get_secret(Key, State) ->
    Now = erlang:system_time(second),

    % Check cache first
    case ets:lookup(State#state.cache, Key) of
        [{_, {Value, ExpiresAt}}] when ExpiresAt > Now ->
            {ok, Value};
        _ ->
            % Fetch from backend
            case fetch_from_backend(Key, State) of
                {ok, Value} ->
                    % Cache with TTL
                    ExpiresAt = Now + State#state.ttl_seconds,
                    ets:insert(State#state.cache, {Key, {Value, ExpiresAt}}),
                    {ok, Value};
                Error ->
                    Error
            end
    end.

%% @private Set secret in backend.
do_set_secret(Key, Value, State) ->
    Result = store_in_backend(Key, Value, State),
    % Invalidate cache
    ets:delete(State#state.cache, Key),
    Result.

%% @private Delete secret from backend.
do_delete_secret(Key, State) ->
    delete_from_backend(Key, State).

%% @private Rotate secret (generate new value).
do_rotate_secret(Key, State) ->
    % Generate new random secret (32 bytes)
    NewValue = base64:encode(crypto:strong_rand_bytes(32)),

    case store_in_backend(Key, NewValue, State) of
        ok ->
            % Invalidate cache
            ets:delete(State#state.cache, Key),
            logger:info("Secret rotated: ~p", [Key]),
            {ok, NewValue};
        Error ->
            Error
    end.

%% @private List secrets from backend.
do_list_secrets(State) ->
    list_from_backend(State).

%% @private Fetch secret from configured backend.
fetch_from_backend(Key, #state{backend = vault, backend_config = Config}) ->
    vault_get(Key, Config);
fetch_from_backend(Key, #state{backend = aws_secrets_manager, backend_config = Config}) ->
    aws_secrets_get(Key, Config);
fetch_from_backend(Key, #state{backend = local_encrypted} = State) ->
    local_get(Key, State).

%% @private Store secret in configured backend.
store_in_backend(Key, Value, #state{backend = vault, backend_config = Config}) ->
    vault_set(Key, Value, Config);
store_in_backend(Key, Value, #state{backend = aws_secrets_manager, backend_config = Config}) ->
    aws_secrets_set(Key, Value, Config);
store_in_backend(Key, Value, #state{backend = local_encrypted} = State) ->
    local_set(Key, Value, State).

%% @private Delete secret from backend.
delete_from_backend(Key, #state{backend = vault, backend_config = Config}) ->
    vault_delete(Key, Config);
delete_from_backend(Key, #state{backend = aws_secrets_manager, backend_config = Config}) ->
    aws_secrets_delete(Key, Config);
delete_from_backend(Key, #state{backend = local_encrypted} = State) ->
    local_delete(Key, State).

%% @private List secrets from backend.
list_from_backend(#state{backend = vault, backend_config = Config}) ->
    vault_list(Config);
list_from_backend(#state{backend = aws_secrets_manager, backend_config = Config}) ->
    aws_secrets_list(Config);
list_from_backend(#state{backend = local_encrypted} = State) ->
    local_list(State).

%%====================================================================
%% HashiCorp Vault Integration
%%====================================================================

%% Vault state record for tracking connection and auth
-record(vault_state, {
    url :: binary(),
    token :: binary() | undefined,
    auth_method :: token | approle | kubernetes,
    mount :: binary(),
    namespace :: binary() | undefined,
    timeout :: pos_integer(),
    circuit_breaker :: closed | open | half_open,
    failure_count :: non_neg_integer(),
    last_failure :: erlang:timestamp() | undefined,
    token_expiry :: erlang:timestamp() | undefined,
    role_id :: binary() | undefined,
    secret_id :: binary() | undefined,
    k8s_jwt_path :: binary() | undefined
}).

-type vault_state() :: #vault_state{}.
-type vault_config() :: #{
    url => binary(),
    token => binary(),
    auth_method => token | approle | kubernetes,
    role_id => binary(),
    secret_id => binary(),
    mount => binary(),
    namespace => binary(),
    timeout => pos_integer()
}.

%% @private Get secret from Vault KV v2 engine.
-spec vault_get(secret_key(), vault_config()) -> {ok, secret_value()} | {error, term()}.
vault_get(Key, Config) ->
    case parse_vault_config(Config) of
        {ok, VaultState} ->
            case ensure_authenticated(VaultState) of
                {ok, AuthenticatedState} ->
                    Path = build_vault_path(<<"data">>, Key, AuthenticatedState),
                    case vault_http_request(get, Path, <<>>, AuthenticatedState) of
                        {ok, ResponseBody} ->
                            parse_vault_secret_response(ResponseBody);
                        {error, Reason} ->
                            logger:error("Vault GET failed for ~p: ~p", [Key, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {auth_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @private Set secret in Vault KV v2 engine.
-spec vault_set(secret_key(), secret_value(), vault_config()) -> ok | {error, term()}.
vault_set(Key, Value, Config) ->
    case parse_vault_config(Config) of
        {ok, VaultState} ->
            case ensure_authenticated(VaultState) of
                {ok, AuthenticatedState} ->
                    Path = build_vault_path(<<"data">>, Key, AuthenticatedState),
                    Body = jsx:encode(#{<<"data">> => #{<<"value">> => Value}}),
                    case vault_http_request(post, Path, Body, AuthenticatedState) of
                        {ok, _ResponseBody} ->
                            logger:info("Vault SET succeeded for ~p", [Key]),
                            ok;
                        {error, Reason} ->
                            logger:error("Vault SET failed for ~p: ~p", [Key, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {auth_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @private Delete secret from Vault KV v2 engine (soft delete).
-spec vault_delete(secret_key(), vault_config()) -> ok | {error, term()}.
vault_delete(Key, Config) ->
    case parse_vault_config(Config) of
        {ok, VaultState} ->
            case ensure_authenticated(VaultState) of
                {ok, AuthenticatedState} ->
                    Path = build_vault_path(<<"data">>, Key, AuthenticatedState),
                    case vault_http_request(delete, Path, <<>>, AuthenticatedState) of
                        {ok, _ResponseBody} ->
                            logger:info("Vault DELETE succeeded for ~p", [Key]),
                            ok;
                        {error, Reason} ->
                            logger:error("Vault DELETE failed for ~p: ~p", [Key, Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {auth_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @private List secrets from Vault KV v2 metadata.
-spec vault_list(vault_config()) -> {ok, [secret_key()]} | {error, term()}.
vault_list(Config) ->
    case parse_vault_config(Config) of
        {ok, VaultState} ->
            case ensure_authenticated(VaultState) of
                {ok, AuthenticatedState} ->
                    Path = build_vault_path(<<"metadata">>, <<>>, AuthenticatedState) ++ "?list=true",
                    case vault_http_request(get, Path, <<>>, AuthenticatedState) of
                        {ok, ResponseBody} ->
                            parse_vault_list_response(ResponseBody);
                        {error, Reason} ->
                            logger:error("Vault LIST failed: ~p", [Reason]),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    {error, {auth_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {invalid_config, Reason}}
    end.

%% @private Parse Vault configuration into vault_state record.
-spec parse_vault_config(vault_config()) -> {ok, vault_state()} | {error, term()}.
parse_vault_config(Config) ->
    Url = maps:get(url, Config, <<"http://localhost:8200">>),
    AuthMethod = maps:get(auth_method, Config, token),
    Mount = maps:get(mount, Config, <<"secret">>),
    Namespace = maps:get(namespace, Config, undefined),
    Timeout = maps:get(timeout, Config, 5000),

    case AuthMethod of
        token ->
            Token = maps:get(token, Config, <<>>),
            case Token of
                <<>> -> {error, missing_token};
                _ -> {ok, #vault_state{
                    url = Url,
                    token = Token,
                    auth_method = token,
                    mount = Mount,
                    namespace = Namespace,
                    timeout = Timeout,
                    circuit_breaker = closed,
                    failure_count = 0,
                    last_failure = undefined,
                    token_expiry = undefined
                }}
            end;
        approle ->
            RoleId = maps:get(role_id, Config, <<>>),
            SecretId = maps:get(secret_id, Config, <<>>),
            case {RoleId, SecretId} of
                {<<>>, _} -> {error, missing_role_id};
                {_, <<>>} -> {error, missing_secret_id};
                _ -> {ok, #vault_state{
                    url = Url,
                    token = undefined,
                    auth_method = approle,
                    mount = Mount,
                    namespace = Namespace,
                    timeout = Timeout,
                    circuit_breaker = closed,
                    failure_count = 0,
                    last_failure = undefined,
                    token_expiry = undefined,
                    role_id = RoleId,
                    secret_id = SecretId
                }}
            end;
        kubernetes ->
            JwtPath = maps:get(k8s_jwt_path, Config, <<"/var/run/secrets/kubernetes.io/serviceaccount/token">>),
            {ok, #vault_state{
                url = Url,
                token = undefined,
                auth_method = kubernetes,
                mount = Mount,
                namespace = Namespace,
                timeout = Timeout,
                circuit_breaker = closed,
                failure_count = 0,
                last_failure = undefined,
                token_expiry = undefined,
                k8s_jwt_path = JwtPath
            }}
    end.

%% @private Ensure we have a valid Vault token.
-spec ensure_authenticated(vault_state()) -> {ok, vault_state()} | {error, term()}.
ensure_authenticated(#vault_state{token = Token, token_expiry = Expiry} = State) ->
    Now = erlang:timestamp(),
    case {Token, Expiry} of
        {<<>>, _} ->
            authenticate(State);
        {_, undefined} ->
            {ok, State};
        {_, ExpiryTime} ->
            case Now < ExpiryTime of
                true -> {ok, State};
                false -> authenticate(State)
            end
    end.

%% @private Authenticate with Vault and obtain token.
-spec authenticate(vault_state()) -> {ok, vault_state()} | {error, term()}.
authenticate(#vault_state{auth_method = token} = State) ->
    {ok, State};
authenticate(#vault_state{auth_method = approle, url = Url, role_id = RoleId, secret_id = SecretId, timeout = Timeout} = State) ->
    Path = <<"/v1/auth/approle/login">>,
    Body = jsx:encode(#{
        <<"role_id">> => RoleId,
        <<"secret_id">> => SecretId
    }),
    case vault_http_request_raw(post, Url, Path, Body, #{}, Timeout) of
        {ok, ResponseBody} ->
            case jsx:decode(ResponseBody, [return_maps]) of
                #{<<"auth">> := #{<<"client_token">> := Token, <<"lease_duration">> := LeaseDuration}} ->
                    Expiry = seconds_to_future_timestamp(LeaseDuration - 60), % Refresh 1min early
                    {ok, State#vault_state{token = Token, token_expiry = Expiry}};
                Error ->
                    logger:error("Vault AppRole auth failed: ~p", [Error]),
                    {error, invalid_auth_response}
            end;
        {error, Reason} ->
            {error, {auth_failed, Reason}}
    end;
authenticate(#vault_state{auth_method = kubernetes, url = Url, k8s_jwt_path = JwtPath, timeout = Timeout} = State) ->
    case file:read_file(JwtPath) of
        {ok, Jwt} ->
            Role = maps:get(k8s_role, State, undefined),
            case Role of
                undefined -> {error, missing_k8s_role};
                _ ->
                    Path = <<"/v1/auth/kubernetes/login">>,
                    Body = jsx:encode(#{
                        <<"jwt">> => Jwt,
                        <<"role">> => Role
                    }),
                    case vault_http_request_raw(post, Url, Path, Body, #{}, Timeout) of
                        {ok, ResponseBody} ->
                            case jsx:decode(ResponseBody, [return_maps]) of
                                #{<<"auth">> := #{<<"client_token">> := Token, <<"lease_duration">> := LeaseDuration}} ->
                                    Expiry = seconds_to_future_timestamp(LeaseDuration - 60),
                                    {ok, State#vault_state{token = Token, token_expiry = Expiry}};
                                Error ->
                                    logger:error("Vault K8s auth failed: ~p", [Error]),
                                    {error, invalid_auth_response}
                            end;
                        {error, Reason} ->
                            {error, {auth_failed, Reason}}
                    end
            end;
        {error, Reason} ->
            {error, {jwt_read_failed, Reason}}
    end.

%% @private Build Vault API path for KV v2.
-spec build_vault_path(binary(), secret_key(), vault_state()) -> binary().
build_vault_path(Endpoint, Key, #vault_state{mount = Mount, namespace = undefined}) ->
    iolist_to_binary([<<"/v1/">>, Mount, <<"/">>, Endpoint, <<"/">>, Key]);
build_vault_path(Endpoint, Key, #vault_state{mount = Mount, namespace = Namespace}) ->
    iolist_to_binary([<<"/v1/">>, Namespace, <<"/">>, Mount, <<"/">>, Endpoint, <<"/">>, Key]).

%% @private Execute HTTP request to Vault with auth headers.
-spec vault_http_request(get | post | delete, binary(), binary(), vault_state()) -> {ok, binary()} | {error, term()}.
vault_http_request(Method, Path, Body, #vault_state{url = Url, token = Token, timeout = Timeout} = State) ->
    Headers = #{
        <<"X-Vault-Token">> => Token,
        <<"Content-Type">> => <<"application/json">>
    },
    case vault_http_request_raw(Method, Url, Path, Body, Headers, Timeout) of
        {ok, ResponseBody} ->
            {ok, ResponseBody};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Execute raw HTTP request to Vault using gun.
-spec vault_http_request_raw(get | post | delete, binary(), binary(), binary(), map(), pos_integer()) -> {ok, binary()} | {error, term()}.
vault_http_request_raw(Method, VaultUrl, Path, Body, Headers, Timeout) ->
    % Parse URL
    UriMap = uri_string:parse(VaultUrl),
    Scheme = maps:get(scheme, UriMap, <<"http">>),
    Host = maps:get(host, UriMap),

    % Determine port (use default if not specified)
    Port = case maps:get(port, UriMap, undefined) of
        undefined ->
            case Scheme of
                <<"https">> -> 443;
                <<"http">> -> 8200;  % Vault default port
                _ -> 8200
            end;
        P -> P
    end,

    % Open gun connection
    Transport = case Scheme of
        <<"https">> -> tls;
        <<"http">> -> tcp;
        _ -> tcp
    end,

    % Convert host to list for gun (gun 2.0.1 accepts both binary and list, but list is safer)
    HostStr = binary_to_list(Host),

    case gun:open(HostStr, Port, #{transport => Transport, protocols => [http]}) of
        {ok, ConnPid} ->
            MonRef = monitor(process, ConnPid),

            % Wait for connection up
            case gun:await_up(ConnPid, Timeout) of
                {up, _Protocol} ->
                    % Make request
                    StreamRef = case Method of
                        get -> gun:get(ConnPid, Path, maps:to_list(Headers));
                        post -> gun:post(ConnPid, Path, maps:to_list(Headers), Body);
                        delete -> gun:delete(ConnPid, Path, maps:to_list(Headers))
                    end,

                    % Wait for response
                    case gun:await(ConnPid, StreamRef, Timeout) of
                        {response, fin, Status, _RespHeaders} when Status >= 200, Status < 300 ->
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {ok, <<>>};
                        {response, nofin, Status, _RespHeaders} when Status >= 200, Status < 300 ->
                            % Get body
                            case gun:await_body(ConnPid, StreamRef, Timeout) of
                                {ok, ResponseBody} ->
                                    demonitor(MonRef, [flush]),
                                    gun:close(ConnPid),
                                    {ok, ResponseBody};
                                {error, Reason} ->
                                    demonitor(MonRef, [flush]),
                                    gun:close(ConnPid),
                                    {error, {body_read_failed, Reason}}
                            end;
                        {response, fin, Status, _RespHeaders} ->
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {error, {http_error, Status, <<>>}};
                        {response, nofin, Status, _RespHeaders} ->
                            % Get error body for non-2xx responses
                            ErrorBody = case gun:await_body(ConnPid, StreamRef, Timeout) of
                                {ok, ErrBody} -> ErrBody;
                                {error, _} -> <<>>
                            end,
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {error, {http_error, Status, ErrorBody}};
                        {error, Reason} ->
                            demonitor(MonRef, [flush]),
                            gun:close(ConnPid),
                            {error, {request_failed, Reason}}
                    end;
                {error, Reason} ->
                    demonitor(MonRef, [flush]),
                    gun:close(ConnPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {gun_open_failed, Reason}}
    end.

%% @private Parse Vault secret response (KV v2 format).
-spec parse_vault_secret_response(binary()) -> {ok, secret_value()} | {error, term()}.
parse_vault_secret_response(ResponseBody) ->
    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"data">> := #{<<"data">> := Data}} ->
            case maps:get(<<"value">>, Data, undefined) of
                undefined ->
                    {error, secret_value_not_found};
                Value ->
                    {ok, Value}
            end;
        #{<<"errors">> := Errors} ->
            {error, {vault_errors, Errors}};
        _ ->
            {error, invalid_response_format}
    catch
        _:_:Stacktrace ->
            logger:error("Failed to parse Vault response: ~p", [Stacktrace]),
            {error, json_decode_failed}
    end.

%% @private Parse Vault list response.
-spec parse_vault_list_response(binary()) -> {ok, [secret_key()]} | {error, term()}.
parse_vault_list_response(ResponseBody) ->
    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"data">> := #{<<"keys">> := Keys}} when is_list(Keys) ->
            {ok, Keys};
        #{<<"errors">> := Errors} ->
            {error, {vault_errors, Errors}};
        _ ->
            {error, invalid_response_format}
    catch
        _:_:_ ->
            {error, json_decode_failed}
    end.

%% @private Convert seconds to future timestamp.
-spec seconds_to_future_timestamp(non_neg_integer()) -> erlang:timestamp().
seconds_to_future_timestamp(Seconds) ->
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    TotalSeconds = MegaSecs * 1000000 + Secs + Seconds,
    NewMegaSecs = TotalSeconds div 1000000,
    NewSecs = TotalSeconds rem 1000000,
    {NewMegaSecs, NewSecs, MicroSecs}.

%%====================================================================
%% AWS Secrets Manager Integration
%%====================================================================

%% AWS credential record
-record(aws_credentials, {
    access_key_id :: binary(),
    secret_access_key :: binary(),
    token :: binary() | undefined,
    expiration :: integer() | undefined  % Unix timestamp
}).

-type aws_credentials() :: #aws_credentials{}.

%% @private Get secret from AWS Secrets Manager.
-spec aws_secrets_get(binary(), map()) -> {ok, binary()} | {error, term()}.
aws_secrets_get(SecretId, Config) ->
    Region = maps_get(region, Config, <<"us-east-1">>),
    AuthMethod = maps_get(auth_method, Config, access_key),

    logger:info("AWS Secrets Manager: Getting secret ~s from region ~s", [SecretId, Region]),

    case get_aws_credentials(AuthMethod, Config) of
        {ok, Creds} ->
            % Build request payload
            RequestBody = jsx:encode(#{
                <<"SecretId">> => SecretId,
                <<"VersionStage">> => <<"AWSCURRENT">>  % Get current version
            }),

            case do_aws_request(Region, <<"secretsmanager">>, Creds,
                               <<"secretsmanager.GetSecretValue">>,
                               RequestBody, Config) of
                {ok, ResponseBody} ->
                    parse_get_secret_response(SecretId, ResponseBody);
                {error, Reason} = Error ->
                    logger:error("AWS Secrets Manager GetSecretValue failed for ~s: ~p", [SecretId, Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            logger:error("Failed to get AWS credentials: ~p", [Reason]),
            Error
    end.

%% @private Set secret in AWS Secrets Manager.
-spec aws_secrets_set(binary(), binary(), map()) -> ok | {error, term()}.
aws_secrets_set(SecretId, SecretValue, Config) ->
    Region = maps_get(region, Config, <<"us-east-1">>),
    AuthMethod = maps_get(auth_method, Config, access_key),

    logger:info("AWS Secrets Manager: Setting secret ~s in region ~s", [SecretId, Region]),

    case get_aws_credentials(AuthMethod, Config) of
        {ok, Creds} ->
            % Try to create secret first
            CreateParams = #{
                <<"Name">> => SecretId,
                <<"SecretString">> => SecretValue
            },

            case do_aws_request(Region, <<"secretsmanager">>, Creds,
                               <<"secretsmanager.CreateSecret">>,
                               jsx:encode(CreateParams), Config) of
                {ok, _ResponseBody} ->
                    logger:info("AWS Secrets Manager: Created secret ~s", [SecretId]),
                    ok;
                {error, {aws_error, <<"ResourceExistsException">>, _Msg}} ->
                    % Secret exists, try updating
                    logger:info("AWS Secrets Manager: Secret ~s exists, updating", [SecretId]),
                    update_aws_secret(SecretId, SecretValue, Region, Creds, Config);
                {error, Reason} = Error ->
                    logger:error("AWS Secrets Manager: Failed to create secret ~s: ~p", [SecretId, Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            logger:error("Failed to get AWS credentials: ~p", [Reason]),
            Error
    end.

%% @private Delete secret from AWS Secrets Manager.
-spec aws_secrets_delete(binary(), map()) -> ok | {error, term()}.
aws_secrets_delete(SecretId, Config) ->
    Region = maps_get(region, Config, <<"us-east-1">>),
    AuthMethod = maps_get(auth_method, Config, access_key),
    RecoveryWindow = maps_get(recovery_window, Config, 30),

    logger:info("AWS Secrets Manager: Deleting secret ~s (recovery window: ~p days)", [SecretId, RecoveryWindow]),

    case get_aws_credentials(AuthMethod, Config) of
        {ok, Creds} ->
            DeleteParams = #{
                <<"SecretId">> => SecretId,
                <<"RecoveryWindowInDays">> => RecoveryWindow
            },

            case do_aws_request(Region, <<"secretsmanager">>, Creds,
                               <<"secretsmanager.DeleteSecret">>,
                               jsx:encode(DeleteParams), Config) of
                {ok, _ResponseBody} ->
                    logger:info("AWS Secrets Manager: Deleted secret ~s", [SecretId]),
                    ok;
                {error, Reason} = Error ->
                    logger:error("AWS Secrets Manager: Failed to delete secret ~s: ~p", [SecretId, Reason]),
                    Error
            end;
        {error, Reason} = Error ->
            logger:error("Failed to get AWS credentials: ~p", [Reason]),
            Error
    end.

%% @private List secrets from AWS Secrets Manager.
-spec aws_secrets_list(map()) -> {ok, [binary()]} | {error, term()}.
aws_secrets_list(Config) ->
    Region = maps_get(region, Config, <<"us-east-1">>),
    AuthMethod = maps_get(auth_method, Config, access_key),

    logger:info("AWS Secrets Manager: Listing secrets in region ~s", [Region]),

    case get_aws_credentials(AuthMethod, Config) of
        {ok, Creds} ->
            list_all_secrets(Region, Creds, Config, [], undefined);
        {error, Reason} = Error ->
            logger:error("Failed to get AWS credentials: ~p", [Reason]),
            Error
    end.

%% @private List all secrets with pagination.
list_all_secrets(Region, Creds, Config, Acc, NextToken) ->
    ListParams = case NextToken of
        undefined -> #{};
        _ -> #{<<"NextToken">> => NextToken}
    end,

    case do_aws_request(Region, <<"secretsmanager">>, Creds,
                       <<"secretsmanager.ListSecrets">>,
                       jsx:encode(ListParams), Config) of
        {ok, ResponseBody} ->
            try jsx:decode(ResponseBody, [return_maps]) of
                #{<<"SecretList">> := SecretList} = Response ->
                    Names = [maps:get(<<"Name">>, S) || S <- SecretList],
                    NewAcc = Acc ++ Names,

                    case maps:get(<<"NextToken">>, Response, undefined) of
                        undefined ->
                            logger:info("AWS Secrets Manager: Listed ~p secrets", [length(NewAcc)]),
                            {ok, NewAcc};
                        NewNextToken ->
                            list_all_secrets(Region, Creds, Config, NewAcc, NewNextToken)
                    end;
                Other ->
                    logger:error("Unexpected AWS list response: ~p", [Other]),
                    {error, unexpected_response}
            catch
                _:_:Stacktrace ->
                    logger:error("Failed to decode AWS list response: ~p", [Stacktrace]),
                    {error, invalid_json}
            end;
        {error, Reason} = Error ->
            logger:error("Failed to list secrets: ~p", [Reason]),
            Error
    end.

%% @private Get AWS credentials based on auth method.
-spec get_aws_credentials(iam_role | access_key, map()) -> {ok, aws_credentials()} | {error, term()}.
get_aws_credentials(iam_role, Config) ->
    % Try to retrieve credentials from IAM role metadata service
    case maps_get(role_arn, Config, undefined) of
        undefined ->
            % EC2/ECS IAM role
            get_iam_role_credentials(Config);
        RoleArn ->
            % Assume role with STS
            case get_iam_role_credentials(Config) of
                {ok, BaseCreds} ->
                    assume_role(BaseCreds, RoleArn, Config);
                {error, Reason} ->
                    logger:warning("Failed to get base credentials for assume role: ~p", [Reason]),
                    {error, assume_role_failed}
            end
    end;
get_aws_credentials(access_key, Config) ->
    AccessKey = maps_get(access_key, Config, <<>>),
    SecretKey = maps_get(secret_key, Config, <<>>),

    case {AccessKey, SecretKey} of
        {<<>>, _} ->
            {error, missing_access_key};
        {_, <<>>} ->
            {error, missing_secret_key};
        {_, _} ->
            {ok, #aws_credentials{
                access_key_id = AccessKey,
                secret_access_key = SecretKey,
                token = undefined,
                expiration = undefined
            }}
    end.

%% @private Get credentials from EC2/ECS IAM role metadata service.
-spec get_iam_role_credentials(map()) -> {ok, aws_credentials()} | {error, term()}.
get_iam_role_credentials(Config) ->
    % First, get the role name
    MetadataUrl = maps_get(metadata_url, Config, <<"http://169.254.169.254/latest/meta-data/iam/security-credentials/">>),

    case httpc_request(get, {MetadataUrl, []}, [], Config) of
        {ok, {{_, 200, _}, _, RoleName}} ->
            RoleUrl = <<MetadataUrl/binary, RoleName/binary>>,

            case httpc_request(get, {RoleUrl, []}, [], Config) of
                {ok, {{_, 200, _}, _, CredsBody}} ->
                    try jsx:decode(CredsBody, [return_maps]) of
                        #{<<"AccessKeyId">> := AccessKey,
                          <<"SecretAccessKey">> := SecretKey,
                          <<"Token">> := Token,
                          <<"Expiration">> := ExpirationISO8601} ->
                            % Parse ISO8601 expiration timestamp
                            Expiration = parse_iso8601(ExpirationISO8601),
                            {ok, #aws_credentials{
                                access_key_id = AccessKey,
                                secret_access_key = SecretKey,
                                token = Token,
                                expiration = Expiration
                            }};
                        Other ->
                            logger:error("Unexpected IAM credentials response: ~p", [Other]),
                            {error, invalid_credentials_response}
                    catch
                        _:_:_ ->
                            {error, invalid_credentials_json}
                    end;
                {ok, {{_, Code, _}, _, _}} ->
                    {error, {metadata_service_error, Code}};
                {error, Reason} ->
                    {error, {metadata_service_error, Reason}}
            end;
        {ok, {{_, Code, _}, _, _}} when Code =/= 200 ->
            {error, {metadata_service_error, Code}};
        {error, Reason} ->
            {error, {metadata_service_error, Reason}}
    end.

%% @private Assume role using STS.
-spec assume_role(aws_credentials(), binary(), map()) -> {ok, aws_credentials()} | {error, term()}.
assume_role(BaseCreds, RoleArn, Config) ->
    Region = maps_get(region, Config, <<"us-east-1">>),
    Duration = maps_get(role_duration, Config, 3600),

    AssumeParams = #{
        <<"RoleArn">> => RoleArn,
        <<"RoleSessionName">> => <<"erlmcp-secrets-", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
        <<"DurationSeconds">> => Duration
    },

    case do_aws_request(Region, <<"sts">>, BaseCreds,
                         <<"sts.AssumeRole">>, jsx:encode(AssumeParams), Config) of
        {ok, ResponseBody} ->
            try jsx:decode(ResponseBody, [return_maps]) of
                #{<<"Credentials">> := #{
                    <<"AccessKeyId">> := AccessKey,
                    <<"SecretAccessKey">> := SecretKey,
                    <<"SessionToken">> := Token,
                    <<"Expiration">> := ExpirationISO8601
                }} ->
                    Expiration = parse_iso8601(ExpirationISO8601),
                    {ok, #aws_credentials{
                        access_key_id = AccessKey,
                        secret_access_key = SecretKey,
                        token = Token,
                        expiration = Expiration
                    }};
                Other ->
                    logger:error("Unexpected assume role response: ~p", [Other]),
                    {error, invalid_assume_role_response}
            catch
                _:_:_ ->
                    {error, invalid_assume_role_json}
            end;
        {error, Reason} ->
            {error, {assume_role_failed, Reason}}
    end.

%% @private Execute AWS Secrets Manager request using gun HTTP client.
-spec do_aws_request(binary(), binary(), aws_credentials(), binary(), binary(), map()) ->
    {ok, binary()} | {error, term()}.
do_aws_request(Region, Service, Creds, Target, Body, Config) ->
    Timeout = maps_get(timeout, Config, 5000),

    % Build AWS endpoint
    Host = <<Service/binary, ".", Region/binary, ".amazonaws.com">>,
    Port = 443,
    Path = <<"/">>,

    % Get current timestamp
    Now = erlang:universaltime(),
    AmzDate = format_amz_date(Now),
    DateStamp = format_date_stamp(Now),

    % Build headers for signing
    BaseHeaders = #{
        <<"X-Amz-Target">> => Target,
        <<"Content-Type">> => <<"application/x-amz-json-1.1">>
    },

    % Calculate AWS Signature v4
    SigV4Headers = calculate_sigv4(
        post, Host, Path, BaseHeaders, Body,
        Region, Service, Creds, AmzDate, DateStamp
    ),

    % Combine all headers
    AllHeaders = maps:merge(BaseHeaders, SigV4Headers),

    % Open gun connection
    case gun:open(binary_to_list(Host), Port, #{
        transport => tls,
        protocols => [http]
    }) of
        {ok, ConnPid} ->
            MonRef = monitor(process, ConnPid),

            % Wait for connection
            case gun:await_up(ConnPid, Timeout) of
                {up, _Protocol} ->
                    % Convert headers to list format for gun
                    GunHeaders = maps:to_list(AllHeaders),

                    % Make POST request
                    StreamRef = gun:post(ConnPid, binary_to_list(Path), GunHeaders, Body),

                    % Wait for response
                    Result = case gun:await(ConnPid, StreamRef, Timeout) of
                        {response, fin, Status, _RespHeaders} when Status >= 200, Status < 300 ->
                            {ok, <<>>};
                        {response, nofin, Status, RespHeaders} when Status >= 200, Status < 300 ->
                            case gun:await_body(ConnPid, StreamRef, Timeout) of
                                {ok, ResponseBody} ->
                                    {ok, ResponseBody};
                                {error, Reason} ->
                                    {error, {body_read_failed, Reason}}
                            end;
                        {response, fin, Status, RespHeaders} ->
                            parse_aws_error(Status, <<>>, RespHeaders);
                        {response, nofin, Status, RespHeaders} ->
                            case gun:await_body(ConnPid, StreamRef, Timeout) of
                                {ok, ErrorBody} ->
                                    parse_aws_error(Status, ErrorBody, RespHeaders);
                                {error, Reason} ->
                                    {error, {error_body_read_failed, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {request_failed, Reason}}
                    end,

                    demonitor(MonRef, [flush]),
                    gun:close(ConnPid),
                    Result;
                {error, Reason} ->
                    demonitor(MonRef, [flush]),
                    gun:close(ConnPid),
                    {error, {connection_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {gun_open_failed, Reason}}
    end.

%% @private Calculate AWS SigV4 signature.
-spec calculate_sigv4(get | post, binary(), binary(), map(), binary(),
                      binary(), binary(), aws_credentials(), binary(), binary()) -> map().
calculate_sigv4(Method, Host, Path, ExtraHeaders, Body,
                Region, Service, Creds, AmzDate, DateStamp) ->
    % Canonical query string (empty for our use case)
    CanonicalQueryString = <<>>,

    % Canonical headers
    CanonicalHeadersList = [
        {<<"content-type">>, <<"application/x-amz-json-1.1">>},
        {<<"host">>, Host},
        {<<"x-amz-date">>, AmzDate}
    ] ++ maps:to_list(ExtraHeaders),

    % Sort headers (lowercase keys)
    SortedHeaders = lists:sort(fun({A, _}, {B, _}) ->
            string:lowercase(binary_to_list(A)) =< string:lowercase(binary_to_list(B))
        end, CanonicalHeadersList),

    CanonicalHeaders = <<<< (list_to_binary(string:lowercase(binary_to_list(K))))/binary, ":", V/binary, "\n" >>
        || {K, V} <- SortedHeaders>>,

    % Build signed headers (lowercase, semicolon-separated)
    SignedHeadersList = [string:lowercase(binary_to_list(K)) || {K, _} <- SortedHeaders],
    SignedHeaders = list_to_binary(string:join(SignedHeadersList, ";")),

    % Payload hash
    PayloadHash = crypto:hash(sha256, Body),

    % Canonical request
    CanonicalRequest = iolist_to_binary([
        string:uppercase(atom_to_list(Method)), "\n",
        Path, "\n",
        CanonicalQueryString, "\n",
        CanonicalHeaders, "\n",
        SignedHeaders, "\n",
        hex_encode(PayloadHash)
    ]),

    % String to sign
    Algorithm = <<"AWS4-HMAC-SHA256">>,
    CredentialScope = <<DateStamp/binary, "/", Region/binary, "/",
                        Service/binary, "/aws4_request">>,
    StringToSign = <<Algorithm/binary, "\n",
                     AmzDate/binary, "\n",
                     CredentialScope/binary, "\n",
                     (hex_encode(crypto:hash(sha256, CanonicalRequest)))/binary>>,

    % Calculate signature
    KDate = hmac_sha256(<<"AWS4", (Creds#aws_credentials.secret_access_key)/binary>>, DateStamp),
    KRegion = hmac_sha256(KDate, Region),
    KService = hmac_sha256(KRegion, Service),
    KSigning = hmac_sha256(KService, <<"aws4_request">>),
    Signature = hmac_sha256(KSigning, StringToSign),

    % Authorization header
    Credential = <<(Creds#aws_credentials.access_key_id)/binary, "/", CredentialScope/binary>>,

    Authorization = <<Algorithm/binary, " Credential=", Credential/binary,
                      ", SignedHeaders=", SignedHeaders/binary,
                      ", Signature=", (hex_encode(Signature))/binary>>,

    % Return headers
    Headers = #{
        <<"Authorization">> => Authorization,
        <<"X-Amz-Date">> => AmzDate,
        <<"Content-Type">> => <<"application/x-amz-json-1.1">>
    },

    % Add session token if present
    case Creds#aws_credentials.token of
        undefined -> Headers;
        Token -> maps:put(<<"X-Amz-Security-Token">>, Token, Headers)
    end.

%% @private Parse AWS error response.
-spec parse_aws_error(non_neg_integer(), binary(), list()) -> {error, term()}.
parse_aws_error(Status, Body, _Headers) ->
    case Body of
        <<>> ->
            {error, {http_error, Status, <<"No response body">>}};
        _ ->
            try jsx:decode(Body, [return_maps]) of
                #{<<"__type">> := ErrorType, <<"message">> := ErrorMsg} ->
                    % Extract error type (remove prefix if present)
                    CleanType = case binary:split(ErrorType, <<"#">>) of
                        [_, Type] -> Type;
                        [Type] -> Type
                    end,
                    logger:error("AWS error ~p: ~s - ~s", [Status, CleanType, ErrorMsg]),
                    {error, {aws_error, CleanType, ErrorMsg}};
                #{<<"__type">> := ErrorType} ->
                    CleanType = case binary:split(ErrorType, <<"#">>) of
                        [_, Type] -> Type;
                        [Type] -> Type
                    end,
                    logger:error("AWS error ~p: ~s", [Status, CleanType]),
                    {error, {aws_error, CleanType, <<>>}};
                #{<<"message">> := ErrorMsg} ->
                    logger:error("AWS error ~p: ~s", [Status, ErrorMsg]),
                    {error, {aws_error, <<"Unknown">>, ErrorMsg}};
                Other ->
                    logger:error("AWS error ~p: ~p", [Status, Other]),
                    {error, {http_error, Status, Body}}
            catch
                _:_:_ ->
                    logger:error("AWS error ~p (failed to parse): ~s", [Status, Body]),
                    {error, {http_error, Status, Body}}
            end
    end.

%% @private Parse GetSecretValue response.
-spec parse_get_secret_response(binary(), binary()) -> {ok, binary()} | {error, term()}.
parse_get_secret_response(SecretId, ResponseBody) ->
    try jsx:decode(ResponseBody, [return_maps]) of
        #{<<"SecretString">> := SecretString} ->
            logger:info("AWS Secrets Manager: Retrieved SecretString for ~s", [SecretId]),
            {ok, SecretString};
        #{<<"SecretBinary">> := SecretBinary} ->
            logger:info("AWS Secrets Manager: Retrieved SecretBinary for ~s", [SecretId]),
            % SecretBinary is base64 encoded
            {ok, base64:decode(SecretBinary)};
        #{<<"ARN">> := _} = Response ->
            % Response might not have the secret value
            case {maps:get(<<"SecretString">>, Response, undefined),
                  maps:get(<<"SecretBinary">>, Response, undefined)} of
                {undefined, undefined} ->
                    logger:error("AWS response missing secret value for ~s", [SecretId]),
                    {error, secret_value_missing};
                {SecretString, _} when SecretString =/= undefined ->
                    {ok, SecretString};
                {_, SecretBinary} when SecretBinary =/= undefined ->
                    {ok, base64:decode(SecretBinary)}
            end;
        Other ->
            logger:error("Unexpected AWS GetSecretValue response: ~p", [Other]),
            {error, unexpected_response}
    catch
        _:_:Stacktrace ->
            logger:error("Failed to decode AWS GetSecretValue response: ~p", [Stacktrace]),
            {error, invalid_json}
    end.

%% @private Update existing secret.
-spec update_aws_secret(binary(), binary(), binary(), aws_credentials(), map()) ->
    ok | {error, term()}.
update_aws_secret(SecretId, SecretValue, Region, Creds, Config) ->
    UpdateParams = #{
        <<"SecretId">> => SecretId,
        <<"SecretString">> => SecretValue
    },

    case do_aws_request(Region, <<"secretsmanager">>, Creds,
                       <<"secretsmanager.UpdateSecret">>,
                       jsx:encode(UpdateParams), Config) of
        {ok, _ResponseBody} ->
            logger:info("AWS Secrets Manager: Updated secret ~s", [SecretId]),
            ok;
        {error, Reason} = Error ->
            logger:error("AWS Secrets Manager: Failed to update secret ~s: ~p", [SecretId, Reason]),
            Error
    end.

%% @private HTTP client wrapper.
-spec httpc_request(get | post, tuple() | list(), list(), map()) ->
    {ok, tuple()} | {error, term()}.
httpc_request(Method, Request, Options, _Config) ->
    % Start inets if not already started
    case whereis(inets) of
        undefined -> inets:start();
        _ -> ok
    end,

    % Start ssl if not already started
    case whereis(ssl_sup) of
        undefined -> ssl:start();
        _ -> ok
    end,

    % SECURITY FIX (P0): Enable SSL certificate verification to prevent MITM attacks.
    % Changed from verify_none to verify_peer with proper certificate validation.
    SslOpts = [
        {ssl, [
            {verify, verify_peer},  % Enable peer verification (was verify_none - VULNERABLE)
            {cacerts, public_key:cacerts_get()},  % Use system CA certificates
            {depth, 3},  % Maximum certificate chain depth
            {customize_hostname_check, [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
        ]}
    ],

    case httpc:request(Method, Request, SslOpts ++ Options, []) of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private HMAC-SHA256 helper.
-spec hmac_sha256(binary() | [byte()], binary() | [byte()]) -> binary().
hmac_sha256(Key, Data) ->
    crypto:mac(hmac, sha256, Key, Data).

%% @private Hex encode binary.
-spec hex_encode(binary()) -> binary().
hex_encode(Bin) ->
    list_to_binary([io_lib:format("~2.16.0B", [B]) || <<B>> <= Bin]).

%% @private Format date for AWS (YYYYMMDD).
-spec format_date_stamp(calendar:datetime()) -> binary().
format_date_stamp({{Year, Month, Day}, _}) ->
    list_to_binary(io_lib:format("~4.10.0B~2.10.0B~2.10.0B", [Year, Month, Day])).

%% @private Format datetime for AWS (YYYYMMDDTHHMMSSZ).
-spec format_amz_date(calendar:datetime()) -> binary().
format_amz_date({{Year, Month, Day}, {Hour, Min, Sec}}) ->
    list_to_binary(io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
                                 [Year, Month, Day, Hour, Min, Sec])).

%% @private Parse ISO8601 timestamp to Unix timestamp.
-spec parse_iso8601(binary()) -> integer().
parse_iso8601(Iso8601) ->
    % Parse format: "2025-01-30T12:34:56Z"
    case re:run(Iso8601, <<"^(\\d{4})-(\\d{2})-(\\d{2})T(\\d{2}):(\\d{2}):(\\d{2})Z$">>,
                [{capture, all_but_first, list}]) of
        {match, [YearStr, MonthStr, DayStr, HourStr, MinStr, SecStr]} ->
            Year = list_to_integer(YearStr),
            Month = list_to_integer(MonthStr),
            Day = list_to_integer(DayStr),
            Hour = list_to_integer(HourStr),
            Min = list_to_integer(MinStr),
            Sec = list_to_integer(SecStr),
            % Convert to Unix timestamp (seconds since epoch)
            Secs = calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {Hour, Min, Sec}}) -
                    calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
            Secs;
        _ ->
            logger:warning("Failed to parse ISO8601 timestamp: ~s", [Iso8601]),
            % Default to 1 hour from now
            erlang:system_time(second) + 3600
    end.

%% @private Safe maps:get with default.
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%%====================================================================
%% Local Encrypted Storage (AES-256-GCM)
%%====================================================================

%% @private Get secret from local encrypted storage.
local_get(Key, State) ->
    % SECURITY FIX (P1): Check if async init has completed before accessing encryption key.
    case State#state.encryption_key of
        undefined ->
            {error, initializing};
        _ ->
            case load_encrypted_storage(State) of
                {ok, Secrets} ->
                    case maps:get(Key, Secrets, undefined) of
                        undefined -> {error, not_found};
                        Value -> {ok, Value}
                    end;
                {error, enoent} ->
                    {error, not_found};
                Error ->
                    Error
            end
    end.

%% @private Set secret in local encrypted storage.
local_set(Key, Value, State) ->
    % SECURITY FIX (P1): Check if async init has completed before accessing encryption key.
    case State#state.encryption_key of
        undefined ->
            {error, initializing};
        _ ->
            Secrets = case load_encrypted_storage(State) of
                {ok, S} -> S;
                {error, enoent} -> #{};
                {error, Reason} -> error({failed_to_load_secrets, Reason})
            end,

            NewSecrets = maps:put(Key, Value, Secrets),
            save_encrypted_storage(NewSecrets, State)
    end.

%% @private Delete secret from local storage.
local_delete(Key, State) ->
    % SECURITY FIX (P1): Check if async init has completed before accessing encryption key.
    case State#state.encryption_key of
        undefined ->
            {error, initializing};
        _ ->
            case load_encrypted_storage(State) of
                {ok, Secrets} ->
                    NewSecrets = maps:remove(Key, Secrets),
                    save_encrypted_storage(NewSecrets, State);
                {error, enoent} ->
                    ok;
                Error ->
                    Error
            end
    end.

%% @private List secrets from local storage.
local_list(State) ->
    % SECURITY FIX (P1): Check if async init has completed before accessing encryption key.
    case State#state.encryption_key of
        undefined ->
            {error, initializing};
        _ ->
            case load_encrypted_storage(State) of
                {ok, Secrets} ->
                    {ok, maps:keys(Secrets)};
                {error, enoent} ->
                    {ok, []};
                Error ->
                    Error
            end
    end.

%% @private Load encrypted storage file.
load_encrypted_storage(State) ->
    case file:read_file(State#state.storage_path) of
        {ok, EncryptedData} ->
            PlainData = decrypt_aes_gcm(EncryptedData, State#state.encryption_key),
            Secrets = binary_to_term(PlainData),
            {ok, Secrets};
        Error ->
            Error
    end.

%% @private Save encrypted storage file.
save_encrypted_storage(Secrets, State) ->
    PlainData = term_to_binary(Secrets),
    EncryptedData = encrypt_aes_gcm(PlainData, State#state.encryption_key),
    file:write_file(State#state.storage_path, EncryptedData).

%% @private Encrypt data with AES-256-GCM.
encrypt_aes_gcm(PlainText, Key) ->
    % Generate random IV (12 bytes for GCM)
    IV = crypto:strong_rand_bytes(12),
    % Encrypt with AES-256-GCM
    {CipherText, Tag} = crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, PlainText, <<>>, true),
    % Prepend IV and Tag to ciphertext
    <<IV/binary, Tag/binary, CipherText/binary>>.

%% @private Decrypt data with AES-256-GCM.
decrypt_aes_gcm(<<IV:12/binary, Tag:16/binary, CipherText/binary>>, Key) ->
    crypto:crypto_one_time_aead(aes_256_gcm, Key, IV, CipherText, <<>>, Tag, false).

%% @private Load or generate encryption key.
load_or_generate_encryption_key(Config) ->
    KeyPath = maps:get(encryption_key_path, Config, "priv/secrets/master.key"),

    case file:read_file(KeyPath) of
        {ok, Key} when byte_size(Key) =:= 32 ->
            Key;
        _ ->
            % Generate new 256-bit key
            NewKey = crypto:strong_rand_bytes(32),
            KeyDir = filename:dirname(KeyPath),
            ok = filelib:ensure_dir(KeyDir ++ "/"),
            ok = file:write_file(KeyPath, NewKey),
            % SECURITY FIX (P0): Use file:change_mode/2 instead of os:cmd to prevent command injection.
            % The previous os:cmd("chmod 600 " ++ KeyPath) was vulnerable to shell injection attacks.
            % Mode 8#600 = rw------- (owner read/write only).
            case file:change_mode(KeyPath, 8#600) of
                ok -> ok;
                {error, Reason} ->
                    logger:warning("Failed to set restrictive permissions on ~s: ~p", [KeyPath, Reason])
            end,
            logger:warning("Generated new encryption key: ~s", [KeyPath]),
            NewKey
    end.
