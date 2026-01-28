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
    gen_server:call(?MODULE, {get_secret, Key}).

%% @doc Set secret (store in backend).
-spec set_secret(secret_key(), secret_value()) -> ok | {error, term()}.
set_secret(Key, Value) ->
    gen_server:call(?MODULE, {set_secret, Key, Value}).

%% @doc Delete secret from backend.
-spec delete_secret(secret_key()) -> ok | {error, term()}.
delete_secret(Key) ->
    gen_server:call(?MODULE, {delete_secret, Key}).

%% @doc Rotate secret (generate new value, update backend).
-spec rotate_secret(secret_key()) -> {ok, secret_value()} | {error, term()}.
rotate_secret(Key) ->
    gen_server:call(?MODULE, {rotate_secret, Key}).

%% @doc List all secret keys.
-spec list_secrets() -> {ok, [secret_key()]} | {error, term()}.
list_secrets() ->
    gen_server:call(?MODULE, list_secrets).

%% @doc Configure HashiCorp Vault backend.
-spec configure_vault(map()) -> ok.
configure_vault(Config) ->
    gen_server:call(?MODULE, {configure_backend, vault, Config}).

%% @doc Configure AWS Secrets Manager backend.
-spec configure_aws(map()) -> ok.
configure_aws(Config) ->
    gen_server:call(?MODULE, {configure_backend, aws_secrets_manager, Config}).

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

    % Generate or load encryption key
    EncryptionKey = load_or_generate_encryption_key(Config),

    % Ensure storage directory exists
    StorageDir = filename:dirname(StoragePath),
    ok = filelib:ensure_dir(StorageDir ++ "/"),

    State = #state{
        cache = ets:new(secrets_cache, [set, protected]),
        backend = Backend,
        backend_config = BackendConfig,
        encryption_key = EncryptionKey,
        ttl_seconds = TtlSeconds,
        storage_path = StoragePath
    },

    % Start cache cleanup timer
    erlang:send_after(60000, self(), cleanup_cache),

    logger:info("Secrets manager started with backend: ~p", [Backend]),
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

%% @private Get secret from Vault.
vault_get(Key, Config) ->
    % TODO: Implement Vault API call
    % For now, return placeholder
    case maps:get(enabled, Config, false) of
        true ->
            VaultUrl = maps:get(url, Config, <<"http://localhost:8200">>),
            _VaultToken = maps:get(token, Config, <<>>),
            Path = iolist_to_binary([<<"/v1/secret/data/">>, Key]),

            % Mock HTTP call - in production, use gun or httpc
            logger:debug("Vault GET: ~s~s (token: present=~p)",
                        [VaultUrl, Path, _VaultToken =/= <<>>]),
            {error, not_implemented};
        false ->
            {error, vault_not_configured}
    end.

%% @private Set secret in Vault.
vault_set(Key, Value, Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            VaultUrl = maps:get(url, Config, <<"http://localhost:8200">>),
            Path = iolist_to_binary([<<"/v1/secret/data/">>, Key]),
            logger:debug("Vault POST: ~s~s", [VaultUrl, Path]),
            {error, not_implemented};
        false ->
            {error, vault_not_configured}
    end.

%% @private Delete secret from Vault.
vault_delete(Key, Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            {error, not_implemented};
        false ->
            {error, vault_not_configured}
    end.

%% @private List secrets from Vault.
vault_list(Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            {error, not_implemented};
        false ->
            {error, vault_not_configured}
    end.

%%====================================================================
%% AWS Secrets Manager Integration
%%====================================================================

%% @private Get secret from AWS.
aws_secrets_get(Key, Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            % TODO: Implement AWS Secrets Manager API call
            Region = maps:get(region, Config, <<"us-east-1">>),
            logger:debug("AWS Secrets Manager GET: ~s in ~s", [Key, Region]),
            {error, not_implemented};
        false ->
            {error, aws_not_configured}
    end.

%% @private Set secret in AWS.
aws_secrets_set(Key, Value, Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            {error, not_implemented};
        false ->
            {error, aws_not_configured}
    end.

%% @private Delete secret from AWS.
aws_secrets_delete(Key, Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            {error, not_implemented};
        false ->
            {error, aws_not_configured}
    end.

%% @private List secrets from AWS.
aws_secrets_list(Config) ->
    case maps:get(enabled, Config, false) of
        true ->
            {error, not_implemented};
        false ->
            {error, aws_not_configured}
    end.

%%====================================================================
%% Local Encrypted Storage (AES-256-GCM)
%%====================================================================

%% @private Get secret from local encrypted storage.
local_get(Key, State) ->
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
    end.

%% @private Set secret in local encrypted storage.
local_set(Key, Value, State) ->
    Secrets = case load_encrypted_storage(State) of
        {ok, S} -> S;
        {error, enoent} -> #{};
        {error, Reason} -> error({failed_to_load_secrets, Reason})
    end,

    NewSecrets = maps:put(Key, Value, Secrets),
    save_encrypted_storage(NewSecrets, State).

%% @private Delete secret from local storage.
local_delete(Key, State) ->
    case load_encrypted_storage(State) of
        {ok, Secrets} ->
            NewSecrets = maps:remove(Key, Secrets),
            save_encrypted_storage(NewSecrets, State);
        {error, enoent} ->
            ok;
        Error ->
            Error
    end.

%% @private List secrets from local storage.
local_list(State) ->
    case load_encrypted_storage(State) of
        {ok, Secrets} ->
            {ok, maps:keys(Secrets)};
        {error, enoent} ->
            {ok, []};
        Error ->
            Error
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
            % Set restrictive permissions (Unix only)
            os:cmd("chmod 600 " ++ KeyPath),
            logger:warning("Generated new encryption key: ~s", [KeyPath]),
            NewKey
    end.
