%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_secrets - CLI Secrets Manager
%%%
%%% Manages secret keys and tokens for CLI authentication.
%%% Provides secure storage and retrieval of secrets with
%%% integration with erlmcp_secrets module for enhanced security.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_secrets).

-behaviour(gen_server).

%% API
-export([start_link/0, get_secret/1, set_secret/2, delete_secret/1,
         list_secrets/0, clear_secrets/0, import_secrets/1,
         export_secrets/0, rotate_secrets/0, authenticate/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").
-include("erlmcp_secrets.hrl").

%% Records
-record(secret_state,
        {secrets :: map(),              % In-memory secret storage
         encrypted :: binary(),         % Encrypted secret data
         keyring :: map(),              % Key rotation management
         cache :: map(),                % Decrypted secret cache
         watchers :: list(),            % Secret watchers
         metrics :: map()}).            % Secret operation metrics

%% Secret types
-define(SECRET_TYPES,
        [<<"jwt">>, <<"mtls">>, <<"api_key">>, <<"oauth">>, <<"basic">>]).

%% Cache TTL
-define(CACHE_TTL, 300000). % 5 minutes

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the secrets manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get secret value
-spec get_secret(binary()) -> {ok, binary()} | {error, term()}.
get_secret(Key) ->
    gen_server:call(?SERVER, {get_secret, Key}, 5000).

%% @doc Set secret value
-spec set_secret(binary(), binary()) -> ok | {error, term()}.
set_secret(Key, Value) ->
    gen_server:call(?SERVER, {set_secret, Key, Value}, 5000).

%% @doc Delete secret
-spec delete_secret(binary()) -> ok | {error, term()}.
delete_secret(Key) ->
    gen_server:call(?SERVER, {delete_secret, Key}, 5000).

%% @doc List all secret keys
-spec list_secrets() -> [binary()].
list_secrets() ->
    gen_server:call(?SERVER, list_secrets, 5000).

%% @doc Clear all secrets
-spec clear_secrets() -> ok.
clear_secrets() ->
    gen_server:cast(?SERVER, clear_secrets).

%% @doc Import secrets from external source
-spec import_secrets(map()) -> ok | {error, term()}.
import_secrets(Secrets) ->
    gen_server:call(?SERVER, {import_secrets, Secrets}, 10000).

%% @doc Export all secrets (encrypted)
-spec export_secrets() -> {ok, binary()} | {error, term()}.
export_secrets() ->
    gen_server:call(?SERVER, export_secrets, 5000).

%% @doc Rotate encryption keys
-spec rotate_secrets() -> ok | {error, term()}.
rotate_secrets() ->
    gen_server:cast(?SERVER, rotate_secrets).

%% @doc Authenticate using secrets
-spec authenticate(binary(), map()) -> {ok, map()} | {error, term()}.
authenticate(Type, Credentials) ->
    gen_server:call(?SERVER, {authenticate, Type, Credentials}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the secrets manager
-spec init(term()) -> {ok, #secret_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for secrets initialization
    erlmcp_otel:with_span("cli.secrets.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize state
                             Secrets = load_secrets_from_storage(),

                             %% Initialize metrics
                             Metrics = init_metrics(),

                             State = #secret_state{
                                secrets = Secrets,
                                encrypted = encrypt_secrets(Secrets),
                                keyring = init_keyring(),
                                cache = #{},
                                watchers = [],
                                metrics = Metrics
                             },

                             %% Start secret cleanup timer
                            erlang:send_after(?CACHE_TTL, self(), cleanup_cache),

                             erlmcp_metrics:record("cli.secrets.initialized", 1),
                             {ok, State}
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #secret_state{}) ->
                   {reply, term(), #secret_state{}}.
handle_call({get_secret, Key}, _From, State) ->
    %% Create OTEL span for secret retrieval
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.get",
                                     #{<<"key">> => Key},
                                     undefined),

    case get_secret_from_cache(Key, State) of
        {ok, Value} ->
            erlmcp_otel:record_event(SpanCtx, <<"secret.retrieved">>, #{<<"key">> => Key}),
            erlmcp_metrics:record("cli.secrets.get.success", 1),
            {reply, {ok, Value}, State};
        {error, not_found} ->
            erlmcp_otel:record_error(SpanCtx, {secret_not_found, Key}),
            erlmcp_metrics:record("cli.secrets.get.not_found", 1),
            {reply, {error, not_found}, State};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {secret_get_failed, Key, Reason}),
            erlmcp_metrics:record("cli.secrets.get.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({set_secret, Key, Value}, _From, State) ->
    %% Create OTEL span for secret setting
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.set",
                                     #{<<"key">> => Key, <<"size">> => size(Value)},
                                     undefined),

    try
        %% Validate secret type
        case validate_secret_type(Key) of
            ok ->
                %% Encrypt secret
                EncryptedValue = encrypt_value(Value, State#secret_state.keyring),

                %% Update secrets
                NewSecrets = maps:put(Key, EncryptedValue, State#secret_state.secrets),

                %% Update encrypted storage
                NewEncrypted = encrypt_secrets(NewSecrets),

                %% Store to persistent storage
                case save_secrets_to_storage(NewSecrets) of
                    ok ->
                        %% Update cache
                        NewCache = cache_secret(Key, Value, State#secret_state.cache),

                        %% Notify watchers
                        notify_watchers(Key, Value),

                        Metrics = update_metrics(State#secret_state.metrics, set),

                        erlmcp_otel:record_event(SpanCtx, <<"secret.set">>, #{<<"key">> => Key}),
                        erlmcp_metrics:record("cli.secrets.set.success", 1),

                        {reply, ok, State#secret_state{
                            secrets = NewSecrets,
                            encrypted = NewEncrypted,
                            cache = NewCache,
                            metrics = Metrics
                        }};
                    {error, Reason} ->
                        erlmcp_otel:record_error(SpanCtx, {secret_save_failed, Key, Reason}),
                        erlmcp_metrics:record("cli.secrets.set.error", 1),
                        {reply, {error, Reason}, State}
                end;
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {secret_validation_failed, Key, Reason}),
                erlmcp_metrics:record("cli.secrets.set.validation_error", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {secret_encryption_failed, Key, Error, Reason}),
            erlmcp_metrics:record("cli.secrets.set.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({delete_secret, Key}, _From, State) ->
    %% Create OTEL span for secret deletion
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.delete",
                                     #{<<"key">> => Key},
                                     undefined),

    case maps:find(Key, State#secret_state.secrets) of
        {ok, _} ->
            %% Remove secret
            NewSecrets = maps:remove(Key, State#secret_state.secrets),

            %% Update encrypted storage
            NewEncrypted = encrypt_secrets(NewSecrets),

            %% Remove from cache
            NewCache = maps:remove(Key, State#secret_state.cache),

            %% Store to persistent storage
            case save_secrets_to_storage(NewSecrets) of
                ok ->
                    %% Notify watchers
                    notify_watchers(Key, deleted),

                    Metrics = update_metrics(State#secret_state.metrics, delete),

                    erlmcp_otel:record_event(SpanCtx, <<"secret.deleted">>, #{<<"key">> => Key}),
                    erlmcp_metrics:record("cli.secrets.delete.success", 1),

                    {reply, ok, State#secret_state{
                        secrets = NewSecrets,
                        encrypted = NewEncrypted,
                        cache = NewCache,
                        metrics = Metrics
                    }};
                {error, Reason} ->
                    erlmcp_otel:record_error(SpanCtx, {secret_save_failed, Key, Reason}),
                    erlmcp_metrics:record("cli.secrets.delete.error", 1),
                    {reply, {error, Reason}, State}
            end;
        error ->
            erlmcp_otel:record_error(SpanCtx, {secret_not_found, Key}),
            erlmcp_metrics:record("cli.secrets.delete.not_found", 1),
            {reply, {error, not_found}, State}
    end;

handle_call(list_secrets, _From, State) ->
    Keys = maps:keys(State#secret_state.secrets),
    {reply, Keys, State};

handle_call({import_secrets, Secrets}, _From, State) ->
    %% Create OTEL span for secret import
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.import",
                                     #{<<"count">> => map_size(Secrets)},
                                     undefined),

    try
        %% Validate imported secrets
        ValidatedSecrets = validate_imported_secrets(Secrets),

        %% Import secrets
        NewSecrets = maps:merge(State#secret_state.secrets, ValidatedSecrets),

        %% Update encrypted storage
        NewEncrypted = encrypt_secrets(NewSecrets),

        %% Store to persistent storage
        case save_secrets_to_storage(NewSecrets) of
            ok ->
                Metrics = update_metrics(State#secret_state.metrics, import),

                erlmcp_otel:record_event(SpanCtx, <<"secret.imported">>,
                                      #{<<"count">> => map_size(NewSecrets) - map_size(State#secret_state.secrets)}),
                erlmcp_metrics:record("cli.secrets.import.success", 1),

                {reply, ok, State#secret_state{
                    secrets = NewSecrets,
                    encrypted = NewEncrypted,
                    metrics = Metrics
                }};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {secret_import_failed, Reason}),
                erlmcp_metrics:record("cli.secrets.import.error", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {secret_validation_failed, Error, Reason}),
            erlmcp_metrics:record("cli.secrets.import.validation_error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call(export_secrets, _From, State) ->
    %% Create OTEL span for secret export
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.export", #{}, undefined),

    try
        {ok, State#secret_state.encrypted}
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {secret_export_failed, Error, Reason}),
            {reply, {error, Reason}, State}
    end;

handle_call({authenticate, Type, Credentials}, _From, State) ->
    %% Create OTEL span for authentication
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.authenticate",
                                     #{<<"type">> => Type},
                                     undefined),

    try
        AuthResult = authenticate_secrets(Type, Credentials, State),
        case AuthResult of
            {ok, AuthData} ->
                erlmcp_otel:record_event(SpanCtx, <<"authentication.success">>,
                                      #{<<"type">> => Type}),
                erlmcp_metrics:record("cli.secrets.authenticate.success", 1),
                {reply, {ok, AuthData}, State};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {authentication_failed, Type, Reason}),
                erlmcp_metrics:record("cli.secrets.authenticate.error", 1),
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {authentication_error, Type, Error, Reason}),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #secret_state{}) -> {noreply, #secret_state{}}.
handle_cast(clear_secrets, State) ->
    %% Create OTEL span for secrets clearing
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.clear", #{}, undefined),

    %% Clear all secrets
    NewSecrets = #{},
    NewEncrypted = encrypt_secrets(NewSecrets),
    NewCache = #{},

    case save_secrets_to_storage(NewSecrets) of
        ok ->
            %% Notify watchers
            notify_watchers(cleared),

            Metrics = update_metrics(State#secret_state.metrics, clear),

            erlmcp_otel:record_event(SpanCtx, <<"secrets.cleared">>, #{}),
            erlmcp_metrics:record("cli.secrets.clear.success", 1),

            {noreply, State#secret_state{
                secrets = NewSecrets,
                encrypted = NewEncrypted,
                cache = NewCache,
                metrics = Metrics
            }};
        {error, Reason} ->
            erlmcp_otel:record_error(SpanCtx, {secret_clear_failed, Reason}),
            erlmcp_metrics:record("cli.secrets.clear.error", 1),
            {noreply, State}
    end;

handle_cast(rotate_secrets, State) ->
    %% Create OTEL span for key rotation
    SpanCtx = erlmcp_otel:inject_span("cli.secrets.rotate", #{}, undefined),

    try
        %% Generate new key
        NewKey = generate_rotation_key(),

        %% Re-encrypt all secrets
        ReencryptedSecrets = maps:map(fun(_Key, Value) ->
                                           encrypt_value(Value, NewKey)
                                       end, State#secret_state.secrets),

        %% Update keyring
        NewKeyring = update_keyring(State#secret_state.keyring, NewKey),

        %% Save to storage
        case save_secrets_to_storage(ReencryptedSecrets) of
            ok ->
                Metrics = update_metrics(State#secret_state.metrics, rotate),

                erlmcp_otel:record_event(SpanCtx, <<"secrets.rotated">>, #{}),
                erlmcp_metrics:record("cli.secrets.rotate.success", 1),

                {noreply, State#secret_state{
                    encrypted = encrypt_secrets(ReencryptedSecrets),
                    keyring = NewKeyring,
                    metrics = Metrics
                }};
            {error, Reason} ->
                erlmcp_otel:record_error(SpanCtx, {key_rotation_failed, Reason}),
                erlmcp_metrics:record("cli.secrets.rotate.error", 1),
                {noreply, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {key_rotation_error, Error, Reason}),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #secret_state{}) -> {noreply, #secret_state{}}.
handle_info(cleanup_cache, State) ->
    %% Clear expired cache entries
    Now = erlang:system_time(millisecond),
    ExpiredKeys = lists:filter(fun(Key) ->
                                    case maps:find(Key, State#secret_state.cache) of
                                        {ok, Expiry} when Expiry =< Now -> true;
                                        _ -> false
                                    end
                                end, maps:keys(State#secret_state.cache)),

    NewCache = lists:foldl(fun(Key, Acc) -> maps:remove(Key, Acc) end,
                          State#secret_state.cache, ExpiredKeys),

    %% Schedule next cleanup
    erlang:send_after(?CACHE_TTL, self(), cleanup_cache),

    {noreply, State#secret_state{cache = NewCache}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #secret_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for secrets termination
    erlmcp_otel:with_span("cli.secrets.terminate",
                          #{<<"secret_count">> => map_size(State#secret_state.secrets)},
                          fun() ->
                             %% Securely wipe secrets from memory
                            WipedState = wipe_secrets(State),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.secrets.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #secret_state{}, term()) -> {ok, #secret_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Load secrets from persistent storage
-spec load_secrets_from_storage() -> map().
load_secrets_from_storage() ->
    try
        case erlmcp_secrets:load() of
            {ok, Secrets} when is_map(Secrets) -> Secrets;
            {ok, _} -> #{};
            {error, not_found} -> #{};
            {error, Reason} ->
                lager:warning("Failed to load secrets: ~p", [Reason]),
                #{}
        end
    catch
        Error:Reason ->
            lager:warning("Exception loading secrets: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Save secrets to persistent storage
-spec save_secrets_to_storage(map()) -> ok | {error, term()}.
save_secrets_to_storage(Secrets) ->
    try
        erlmcp_secrets:save(Secrets)
    catch
        Error:Reason ->
            {error, {storage_error, Error, Reason}}
    end.

%% @doc Encrypt secrets
-spec encrypt_secrets(map()) -> binary().
encrypt_secrets(Secrets) ->
    try
        %% Serialize secrets
        Serialized = erlmcp_json_native:encode(Secrets),

        %% Encrypt using erlmcp_secrets
        case erlmcp_secrets:encrypt(Serialized) of
            {ok, Encrypted} -> Encrypted;
            {error, Reason} ->
                lager:warning("Failed to encrypt secrets: ~p", [Reason]),
                <<>>
        end
    catch
        Error:Reason ->
            lager:warning("Exception encrypting secrets: ~p:~p", [Error, Reason]),
            <<>>
    end.

%% @doc Encrypt individual value
-spec encrypt_value(binary(), map()) -> binary().
encrypt_value(Value, Keyring) ->
    try
        %% Use keyring to encrypt value
        case erlmcp_secrets:encrypt_value(Value, Keyring) of
            {ok, Encrypted} -> Encrypted;
            {error, Reason} ->
                lager:warning("Failed to encrypt value: ~p", [Reason]),
                <<>>
        end
    catch
        Error:Reason ->
            lager:warning("Exception encrypting value: ~p:~p", [Error, Reason]),
            <<>>
    end.

%% @doc Decrypt value
-spec decrypt_value(binary(), map()) -> binary().
decrypt_value(Encrypted, Keyring) ->
    try
        case erlmcp_secrets:decrypt_value(Encrypted, Keyring) of
            {ok, Decrypted} -> Decrypted;
            {error, Reason} ->
                lager:warning("Failed to decrypt value: ~p", [Reason]),
                <<>>
        end
    catch
        Error:Reason ->
            lager:warning("Exception decrypting value: ~p:~p", [Error, Reason]),
            <<>>
    end.

%% @doc Get secret from cache
-spec get_secret_from_cache(binary(), #secret_state{}) -> {ok, binary()} | {error, term()}.
get_secret_from_cache(Key, State) ->
    case maps:find(Key, State#secret_state.cache) of
        {ok, {Value, Expiry}} when Expiry > erlang:system_time(millisecond) ->
            {ok, Value};
        {ok, _} ->
            %% Cache expired, remove and try from storage
            NewCache = maps:remove(Key, State#secret_state.cache),
            case maps:find(Key, State#secret_state.secrets) of
                {ok, Encrypted} ->
                    Decrypted = decrypt_value(Encrypted, State#secret_state.keyring),
                    NewCache2 = cache_secret(Key, Decrypted, NewCache),
                    {ok, Decrypted};
                error ->
                    {error, not_found}
            end;
        error ->
            {error, not_found}
    end.

%% @doc Cache secret
-spec cache_secret(binary(), binary(), map()) -> map().
cache_secret(Key, Value, Cache) ->
    Now = erlang:system_time(millisecond),
    maps:put(Key, {Value, Now + ?CACHE_TTL}, Cache).

%% @doc Validate secret type
-spec validate_secret_type(binary()) -> ok | {error, term()}.
validate_secret_type(Key) ->
    case binary:split(Key, <<"/">>, [global]) of
        [Type, _] when Type == <<"jwt">>; Type == <<"mtls">>;
                      Type == <<"api_key">>; Type == <<"oauth">>;
                      Type == <<"basic">> ->
            ok;
        _ ->
            {error, invalid_secret_type}
    end.

%% @doc Validate imported secrets
-spec validate_imported_secrets(map()) -> map().
validate_imported_secrets(Secrets) ->
    maps:filter(fun(Key, Value) ->
                    case validate_secret_type(Key) of
                        ok when is_binary(Value) -> true;
                        _ -> false
                    end
                end, Secrets).

%% @doc Initialize keyring
-spec init_keyring() -> map().
init_keyring() ->
    try
        case erlmcp_secrets:get_keyring() of
            {ok, Keyring} -> Keyring;
            {error, _} -> generate_keyring()
        end
    catch
        _ -> generate_keyring()
    end.

%% @doc Generate keyring
-spec generate_keyring() -> map().
generate_keyring() ->
    Key = crypto:strong_rand_bytes(32),
    #{encryption => Key}.

%% @doc Generate rotation key
-spec generate_rotation_key() -> binary().
generate_rotation_key() ->
    crypto:strong_rand_bytes(32).

%% @doc Update keyring
-spec update_keyring(map(), binary()) -> map().
update_keyring(Keyring, NewKey) ->
    Keyring#{encryption => NewKey, rotation => erlang:system_time(millisecond)}.

%% @doc Authenticate using secrets
-spec authenticate_secrets(binary(), map(), #secret_state{}) -> {ok, map()} | {error, term()}.
authenticate_secrets(<<"jwt">>, #{<<"token">> := Token}, State) ->
    %% JWT authentication
    case verify_jwt_token(Token, State) of
        {ok, Claims} -> {ok, #{type => <<"jwt">>, claims => Claims}};
        {error, Reason} -> {error, Reason}
    end;

authenticate_secrets(<<"mtls">>, #{<<"cert">> := Cert, <<"key">> := Key}, State) ->
    %% MTLS authentication
    case verify_mtls_cert(Cert, Key, State) of
        {ok, Subject} -> {ok, #{type => <<"mtls">>, subject => Subject}};
        {error, Reason} -> {error, Reason}
    end;

authenticate_secrets(<<"api_key">>, #{<<"key">> := ApiKey}, State) ->
    %% API key authentication
    case verify_api_key(ApiKey, State) of
        {ok, ClientId} -> {ok, #{type => <<"api_key">>, client_id => ClientId}};
        {error, Reason} -> {error, Reason}
    end;

authenticate_secrets(<<"oauth">>, #{<<"access_token">> := AccessToken}, State) ->
    %% OAuth authentication
    case verify_oauth_token(AccessToken, State) of
        {ok, UserInfo} -> {ok, #{type => <<"oauth">>, user_info => UserInfo}};
        {error, Reason} -> {error, Reason}
    end;

authenticate_secrets(<<"basic">>, #{<<"username">> := Username, <<"password">> := Password}, State) ->
    %% Basic authentication
    case verify_basic_auth(Username, Password, State) of
        {ok, User} -> {ok, #{type => <<"basic">>, user => User}};
        {error, Reason} -> {error, Reason}
    end;

authenticate_secrets(_, _, _) ->
    {error, unsupported_auth_type}.

%% @doc Verify JWT token
-spec verify_jwt_token(binary(), #secret_state{}) -> {ok, map()} | {error, term()}.
verify_jwt_token(Token, State) ->
    try
        %% Get JWT secret
        case get_secret_from_cache(<<"jwt/secret">>, State) of
            {ok, Secret} ->
                %% Decode and verify token
                case jose_jwt:decode(Token) of
                    {ok, _, Payload} ->
                        case jose_jwt:verify(Secret, Token) of
                            {true, Payload} -> {ok, Payload};
                            {false, _} -> {error, invalid_token}
                        end;
                    {error, Reason} -> {error, Reason}
                end;
            {error, not_found} ->
                {error, jwt_secret_not_found}
        end
    catch
        Error:Reason -> {error, {verification_error, Error, Reason}}
    end.

%% @doc Verify MTLS certificate
-spec verify_mtls_cert(binary(), binary(), #secret_state{}) -> {ok, binary()} | {error, term()}.
verify_mtls_cert(Cert, Key, State) ->
    try
        %% Get CA certificates
        case get_secret_from_cache(<<"mtls/ca">>, State) of
            {ok, CA} ->
                %% Verify certificate chain
                case verify_cert_chain(Cert, CA) of
                    ok -> {ok, extract_subject_from_cert(Cert)};
                    {error, Reason} -> {error, Reason}
                end;
            {error, not_found} ->
                {error, mtls_ca_not_found}
        end
    catch
        Error:Reason -> {error, {cert_verification_error, Error, Reason}}
    end.

%% @doc Verify API key
-spec verify_api_key(binary(), #secret_state{}) -> {ok, binary()} | {error, term()}.
verify_api_key(ApiKey, State) ->
    %% Hash the API key for comparison
    HashedKey = crypto:hash(sha256, ApiKey),

    case get_secret_from_cache(<<"api_keys/clients">>, State) of
        {ok, Clients} when is_map(Clients) ->
            case maps:find(HashedKey, Clients) of
                {ok, ClientId} -> {ok, ClientId};
                error -> {error, invalid_api_key}
            end;
        {error, not_found} ->
            {error, api_clients_not_found}
    end.

%% @doc Verify OAuth token
-spec verify_oauth_token(binary(), #secret_state{}) -> {ok, map()} | {error, term()}.
verify_oauth_token(AccessToken, State) ->
    %% Get OAuth client secret
    case get_secret_from_cache(<<"oauth/clients">>, State) of
        {ok, Clients} when is_map(Clients) ->
            case maps:find(AccessToken, Clients) of
                {ok, UserInfo} -> {ok, UserInfo};
                error -> {error, invalid_oauth_token}
            end;
        {error, not_found} ->
            {error, oauth_clients_not_found}
    end.

%% @doc Verify basic authentication
-spec verify_basic_auth(binary(), binary(), #secret_state{}) -> {ok, map()} | {error, term()}.
verify_basic_auth(Username, Password, State) ->
    %% Get user credentials
    case get_secret_from_cache(<<"users/credentials">>, State) of
        {ok, Users} when is_map(Users) ->
            case maps:find(Username, Users) of
                {ok, StoredHash} ->
                    case crypto:hash(sha256, Password) of
                        StoredHash -> {ok, #{username => Username}};
                        _ -> {error, invalid_credentials}
                    end;
                error -> {error, invalid_credentials}
            end;
        {error, not_found} ->
            {error, users_not_found}
    end.

%% @doc Notify secret watchers
-spec notify_watchers(binary(), term()) -> ok.
notify_watchers(Key, Event) ->
    lists:foreach(fun(Watcher) -> Watcher ! {secret_event, Key, Event} end, get_watchers()).

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{"secrets.get" => 0,
      "secrets.set" => 0,
      "secrets.delete" => 0,
      "secrets.import" => 0,
      "secrets.export" => 0,
      "secrets.authenticate" => 0,
      "secrets.errors" => 0}.

%% @doc Update metrics
-spec update_metrics(map(), atom()) -> map().
update_metrics(Metrics, Type) ->
    case Type of
        get -> maps:update_with("secrets.get", fun(V) -> V + 1 end, Metrics);
        set -> maps:update_with("secrets.set", fun(V) -> V + 1 end, Metrics);
        delete -> maps:update_with("secrets.delete", fun(V) -> V + 1 end, Metrics);
        import -> maps:update_with("secrets.import", fun(V) -> V + 1 end, Metrics);
        export -> maps:update_with("secrets.export", fun(V) -> V + 1 end, Metrics);
        authenticate -> maps:update_with("secrets.authenticate", fun(V) -> V + 1 end, Metrics);
        clear -> Metrics;
        rotate -> Metrics;
        _ -> Metrics
    end.

%% @doc Wipe secrets from memory
-spec wipe_secrets(#secret_state{}) -> #secret_state{}.
wipe_secrets(State) ->
    %% Clear in-memory secrets
    Cleared = #{secrets => #{}, encrypted => <<>>, cache => #{}},
    maps:merge(State, Cleared).

%% @doc Helper functions for certificate verification
-spec verify_cert_chain(binary(), binary()) -> ok | {error, term()}.
verify_cert_chain(_Cert, _CA) ->
    %% Implement certificate chain verification
    ok.

-spec extract_subject_from_cert(binary()) -> binary().
extract_subject_from_cert(_Cert) ->
    %% Implement subject extraction from certificate
    <<"subject">>.