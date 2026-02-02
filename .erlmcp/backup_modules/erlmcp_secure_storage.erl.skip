%%%-------------------------------------------------------------------
%%% @doc erlmcp_secure_storage - Secure Credential Storage with AEAD
%%%
%%% Provides encrypted storage for sensitive data (API keys, tokens):
%%% - AEAD encryption (AES-256-GCM) for confidentiality + integrity
%%% - Key derivation from master password
%%% - Key rotation support
%%% - Re-encryption on key rotation
%%%
%%% Security:
%%% - Uses OTP 27 AEAD (crypto_one_time_aead/7)
%%% - Additional Authenticated Data (AAD) for context binding
%%% - 12-byte IV for AES-GCM (NIST recommendation)
%%% - 32-byte keys (256-bit)
%%%
%%% Use Cases:
%%% - Encrypt API keys at rest
%%% - Store authentication tokens securely
%%% - Protect sensitive configuration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_secure_storage).

-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Credential encryption/decryption
    encrypt_credential/2,
    encrypt_credential/3,
    decrypt_credential/2,
    decrypt_credential/3,

    %% Key management
    rotate_key/1,
    get_current_key_id/0,

    %% Batch operations
    reencrypt_all/1,

    %% Metadata
    list_credentials/0,
    get_credential_metadata/1,
    delete_credential/1,

    %% Configuration
    set_key_derivation_params/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type key_id() :: binary().
-type key_version() :: pos_integer().
-type plaintext() :: binary().
-type ciphertext() :: binary().
-type aad() :: binary().           % Additional Authenticated Data
-type credential_id() :: binary().
-type encryption_context() :: map().

-type encrypted_credential() :: #{
    ciphertext := ciphertext(),
    iv := binary(),
    tag := binary(),
    key_id := key_id(),
    key_version := key_version(),
    aad := aad(),
    created_at => integer()
}.

-type key_entry() :: #{
    key_id := key_id(),
    key := binary(),
    version := key_version(),
    created_at := integer(),
    expires_at => integer() | undefined
}.

-record(state, {
    keys :: ets:tid(),                  % key_id -> key_entry()
    credentials :: ets:tid(),           % credential_id -> encrypted_credential()
    current_key_id :: key_id() | undefined,
    master_key :: binary() | undefined,
    pbkdf2_iterations :: pos_integer(),
    pbkdf2_salt :: binary()
}).

-type state() :: #state{}.

-export_type([encrypted_credential/0, key_entry/0, key_id/0, key_version/0,
              credential_id/0, aad/0, encryption_context/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% Credential Encryption
%%--------------------------------------------------------------------

%% @doc Encrypt credential with default AAD
-spec encrypt_credential(credential_id(), plaintext()) ->
    {ok, encrypted_credential()} | {error, term()}.
encrypt_credential(CredentialId, Plaintext) ->
    AAD = build_default_aad(CredentialId),
    encrypt_credential(CredentialId, Plaintext, AAD).

%% @doc Encrypt credential with custom AAD
-spec encrypt_credential(credential_id(), plaintext(), aad()) ->
    {ok, encrypted_credential()} | {error, term()}.
encrypt_credential(CredentialId, Plaintext, AAD) ->
    gen_server:call(?MODULE, {encrypt_credential, CredentialId, Plaintext, AAD}).

%%--------------------------------------------------------------------
%% Credential Decryption
%%--------------------------------------------------------------------

%% @doc Decrypt credential
-spec decrypt_credential(credential_id(), ciphertext()) ->
    {ok, plaintext()} | {error, term()}.
decrypt_credential(CredentialId, Ciphertext) ->
    decrypt_credential(CredentialId, Ciphertext, <<>>).

%% @doc Decrypt credential with AAD verification
-spec decrypt_credential(credential_id(), ciphertext(), aad()) ->
    {ok, plaintext()} | {error, term()}.
decrypt_credential(CredentialId, Ciphertext, AAD) ->
    gen_server:call(?MODULE, {decrypt_credential, CredentialId, Ciphertext, AAD}).

%%--------------------------------------------------------------------
%% Key Management
%%--------------------------------------------------------------------

%% @doc Rotate to new encryption key
-spec rotate_key(binary()) -> {ok, key_id()} | {error, term()}.
rotate_key(NewMasterKey) ->
    gen_server:call(?MODULE, {rotate_key, NewMasterKey}).

%% @doc Get current key ID
-spec get_current_key_id() -> {ok, key_id()} | {error, term()}.
get_current_key_id() ->
    gen_server:call(?MODULE, get_current_key_id).

%%--------------------------------------------------------------------
%% Batch Operations
%%--------------------------------------------------------------------

%% @doc Re-encrypt all credentials with new key
-spec reencrypt_all(binary()) -> {ok, pos_integer()} | {error, term()}.
reencrypt_all(NewMasterKey) ->
    gen_server:call(?MODULE, {reencrypt_all, NewMasterKey}, 60000).

%%--------------------------------------------------------------------
%% Metadata
%%--------------------------------------------------------------------

%% @doc List all credential IDs
-spec list_credentials() -> {ok, [credential_id()]}.
list_credentials() ->
    gen_server:call(?MODULE, list_credentials).

%% @doc Get credential metadata (without decrypting)
-spec get_credential_metadata(credential_id()) ->
    {ok, map()} | {error, term()}.
get_credential_metadata(CredentialId) ->
    gen_server:call(?MODULE, {get_credential_metadata, CredentialId}).

%% @doc Delete credential
-spec delete_credential(credential_id()) -> ok | {error, term()}.
delete_credential(CredentialId) ->
    gen_server:call(?MODULE, {delete_credential, CredentialId}).

%%--------------------------------------------------------------------
%% Configuration
%%--------------------------------------------------------------------

%% @doc Set key derivation parameters
-spec set_key_derivation_params(pos_integer(), binary()) -> ok.
set_key_derivation_params(Iterations, Salt) ->
    gen_server:call(?MODULE, {set_key_derivation_params, Iterations, Salt}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    %% Create ETS tables
    Keys = ets:new(?MODULE_KEYS, [set, protected]),
    Credentials = ets:new(?MODULE_CREDENTIALS, [set, protected]),

    %% Get or derive master key
    MasterKey = case maps:get(master_key, Config, undefined) of
        undefined ->
            %% Derive from password
            Password = maps:get(password, Config, <<>>),
            Salt = maps:get(salt, Config, generate_salt()),
            Iterations = maps:get(iterations, Config, 100000),
            erlmcp_crypto:derive_key(Password, Salt, 32, Iterations);
        Key ->
            Key
    end,

    %% Initialize with first key version
    KeyId = generate_key_id(),
    KeyEntry = #{
        key_id => KeyId,
        key => MasterKey,
        version => 1,
        created_at => erlang:system_time(second),
        expires_at => undefined
    },
    ets:insert(Keys, {KeyId, KeyEntry}),

    State = #state{
        keys = Keys,
        credentials = Credentials,
        current_key_id = KeyId,
        master_key = MasterKey,
        pbkdf2_iterations = maps:get(iterations, Config, 100000),
        pbkdf2_salt = maps:get(salt, Config, generate_salt())
    },

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({encrypt_credential, CredentialId, Plaintext, AAD}, _From,
            #state{current_key_id = KeyId, keys = Keys, credentials = Credentials} = State) ->
    case ets:lookup(Keys, KeyId) of
        [{_, KeyEntry}] ->
            #{key := Key} = KeyEntry,
            IV = erlmcp_crypto:generate_nonce(),
            case erlmcp_crypto:encrypt_aead(Plaintext, Key, IV, AAD) of
                {ok, Ciphertext, Tag} ->
                    Encrypted = #{
                        ciphertext => Ciphertext,
                        iv => IV,
                        tag => Tag,
                        key_id => KeyId,
                        key_version => maps:get(version, KeyEntry),
                        aad => AAD,
                        created_at => erlang:system_time(second)
                    },
                    ets:insert(Credentials, {CredentialId, Encrypted}),
                    {reply, {ok, Encrypted}, State};
                {error, Reason} ->
                    {reply, {error, {encryption_failed, Reason}}, State}
            end;
        [] ->
            {reply, {error, key_not_found}, State}
    end;

handle_call({decrypt_credential, CredentialId, Ciphertext, AAD}, _From,
            #state{credentials = Credentials, keys = Keys} = State) ->
    case ets:lookup(Credentials, CredentialId) of
        [{_, Encrypted}] ->
            #{iv := IV, tag := Tag, key_id := KeyId} = Encrypted,
            case ets:lookup(Keys, KeyId) of
                [{_, KeyEntry}] ->
                    #{key := Key} = KeyEntry,
                    case erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD) of
                        {ok, Plaintext} ->
                            {reply, {ok, Plaintext}, State};
                        {error, Reason} ->
                            {reply, {error, {decryption_failed, Reason}}, State}
                    end;
                [] ->
                    {reply, {error, encryption_key_not_found}, State}
            end;
        [] ->
            {reply, {error, credential_not_found}, State}
    end;

handle_call({rotate_key, NewMasterKey}, _From,
            #state{keys = Keys, current_key_id = CurrentKeyId} = State) ->
    %% Generate new key ID
    NewKeyId = generate_key_id(),

    %% Get current max version
    MaxVersion = case ets:tab2list(Keys) of
        [] -> 0;
        KeyList ->
            lists:max([Version || {_, #{version := Version}} <- KeyList])
    end,

    %% Insert new key
    KeyEntry = #{
        key_id => NewKeyId,
        key => NewMasterKey,
        version => MaxVersion + 1,
        created_at => erlang:system_time(second),
        expires_at => undefined
    },
    ets:insert(Keys, {NewKeyId, KeyEntry}),

    NewState = State#state{
        current_key_id = NewKeyId,
        master_key = NewMasterKey
    },

    {reply, {ok, NewKeyId}, NewState};

handle_call(get_current_key_id, _From, #state{current_key_id = KeyId} = State) ->
    {reply, {ok, KeyId}, State};

handle_call({reencrypt_all, NewMasterKey}, _From,
            #state{keys = Keys, credentials = Credentials, current_key_id = OldKeyId} = State) ->
    %% Rotate to new key
    NewKeyId = generate_key_id(),

    %% Get current max version
    MaxVersion = case ets:tab2list(Keys) of
        [] -> 0;
        KeyList ->
            lists:max([Version || {_, #{version := Version}} <- KeyList])
    end,

    %% Insert new key
    KeyEntry = #{
        key_id => NewKeyId,
        key => NewMasterKey,
        version => MaxVersion + 1,
        created_at => erlang:system_time(second),
        expires_at => undefined
    },
    ets:insert(Keys, {NewKeyId, KeyEntry}),

    %% Re-encrypt all credentials
    CredentialList = ets:tab2list(Credentials),
    ReencryptCount = lists:foldl(
        fun({CredentialId, Encrypted}, Count) ->
            #{iv := IV, aad := AAD} = Encrypted,

            %% Decrypt with old key
            {ok, Plaintext} = decrypt_credential_internal(Encrypted, Keys),

            %% Encrypt with new key
            case erlmcp_crypto:encrypt_aead(Plaintext, NewMasterKey, IV, AAD) of
                {ok, NewCiphertext, NewTag} ->
                    NewEncrypted = Encrypted#{
                        ciphertext => NewCiphertext,
                        iv => IV,
                        tag => NewTag,
                        key_id => NewKeyId,
                        key_version => MaxVersion + 1,
                        created_at => erlang:system_time(second)
                    },
                    ets:insert(Credentials, {CredentialId, NewEncrypted}),
                    Count + 1;
                {error, _} ->
                    Count
            end
        end,
        0,
        CredentialList
    ),

    NewState = State#state{
        current_key_id = NewKeyId,
        master_key = NewMasterKey
    },

    {reply, {ok, ReencryptCount}, NewState};

handle_call(list_credentials, _From, #state{credentials = Credentials} = State) ->
    CredentialIds = [Id || {Id, _} <- ets:tab2list(Credentials)],
    {reply, {ok, CredentialIds}, State};

handle_call({get_credential_metadata, CredentialId}, _From,
            #state{credentials = Credentials} = State) ->
    case ets:lookup(Credentials, CredentialId) of
        [{_, Encrypted}] ->
            %% Return metadata without ciphertext
            Metadata = maps:without([ciphertext], Encrypted),
            {reply, {ok, Metadata}, State};
        [] ->
            {reply, {error, credential_not_found}, State}
    end;

handle_call({delete_credential, CredentialId}, _From,
            #state{credentials = Credentials} = State) ->
    case ets:lookup(Credentials, CredentialId) of
        [{_, _}] ->
            ets:delete(Credentials, CredentialId),
            {reply, ok, State};
        [] ->
            {reply, {error, credential_not_found}, State}
    end;

handle_call({set_key_derivation_params, Iterations, Salt}, _From, State) ->
    NewState = State#state{
        pbkdf2_iterations = Iterations,
        pbkdf2_salt = Salt
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{keys = Keys, credentials = Credentials}) ->
    ets:delete(Keys),
    ets:delete(Credentials),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Decrypt credential internally (for re-encryption)
decrypt_credential_internal(Encrypted, Keys) ->
    #{ciphertext := Ciphertext, iv := IV, tag := Tag, key_id := KeyId, aad := AAD} = Encrypted,
    case ets:lookup(Keys, KeyId) of
        [{_, KeyEntry}] ->
            #{key := Key} = KeyEntry,
            case erlmcp_crypto:decrypt_aead(Ciphertext, Tag, Key, IV, AAD) of
                {ok, Plaintext} ->
                    {ok, Plaintext};
                {error, Reason} ->
                    {error, {decryption_failed, Reason}}
            end;
        [] ->
            {error, encryption_key_not_found}
    end.

%% @doc Build default AAD for credential
build_default_aad(CredentialId) ->
    Timestamp = erlang:system_time(second),
    Context = [
        {<<"credential_id">>, CredentialId},
        {<<"purpose">>, <<"credential_storage">>},
        {<<"created_at">>, Timestamp}
    ],
    %% Use simple iolist_to_binary for AAD (JSON encoding optional)
    %% AAD is just additional authenticated data, doesn't need to be JSON
    iolist_to_binary([
        CredentialId, $|,
        <<"credential_storage">>, $|,
        integer_to_binary(Timestamp)
    ]).

%% @doc Generate unique key ID
generate_key_id() ->
    <<"key_", (base64:encode(erlmcp_crypto:generate_token(16)))/binary>>.

%% @doc Generate salt for key derivation
generate_salt() ->
    crypto:strong_rand_bytes(16).
