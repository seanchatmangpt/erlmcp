%%%-------------------------------------------------------------------
%%% @doc erlmcp_request_signer - MCP Request Signing Module
%%%
%%% Provides request signing for MCP tool invocations:
%%% - Sign requests with HMAC-SHA256
%%% - Verify signatures on responses
%%% - Nonce management for replay protection
%%% - Timestamp validation
%%%
%%% Security:
%%% - Uses HMAC-SHA256 for signature generation
%%% - Includes timestamp and nonce in signature
%%% - Validates signature expiration (default 5 minutes)
%%% - Constant-time comparison for signature verification
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_signer).

-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Request signing
    sign_request/4,
    sign_request/6,

    %% Request verification
    verify_request/4,
    verify_request/5,

    %% Response signing
    sign_response/3,
    sign_response/4,

    %% Response verification
    verify_response/3,
    verify_response/4,

    %% Nonce management
    generate_nonce/0,
    validate_nonce/1,
    mark_nonce_used/1,
    cleanup_expired_nonces/0,

    %% Configuration
    set_signature_ttl/1,
    get_signature_ttl/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type signature() :: binary().
-type timestamp() :: integer().
-type nonce() :: binary().
-type signing_key() :: binary().
-type request_id() :: binary().
-type method() :: binary().
-type params() :: map().

-type signature_data() :: #{
    method := method(),
    params := params(),
    timestamp := timestamp(),
    nonce := nonce(),
    signature := signature()
}.

-type nonce_state() :: #{
    nonce => nonce(),
    used_at => timestamp(),
    expires_at => timestamp()
}.

-record(state, {
    nonces :: ets:tid(),              % nonce -> nonce_state()
    signature_ttl :: pos_integer(),    % TTL in seconds (default: 300)
    cleanup_interval :: pos_integer()  % Cleanup interval in ms (default: 60000)
}).

-type state() :: #state{}.

-export_type([signature/0, timestamp/0, nonce/0, signing_key/0, signature_data/0]).

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
%% Request Signing
%%--------------------------------------------------------------------

%% @doc Sign request with HMAC-SHA256 (default timestamp and nonce)
-spec sign_request(signing_key(), method(), params(), request_id()) ->
    {ok, signature_data()}.
sign_request(Key, Method, Params, RequestId) ->
    Timestamp = erlang:system_time(second),
    Nonce = generate_nonce(),
    sign_request(Key, Method, Params, RequestId, Timestamp, Nonce).

%% @doc Sign request with explicit timestamp and nonce
-spec sign_request(signing_key(), method(), params(), request_id(), timestamp(), nonce()) ->
    {ok, signature_data()}.
sign_request(Key, Method, Params, RequestId, Timestamp, Nonce) ->
    %% Build signature base string
    SignatureBase = build_signature_base(Method, Params, RequestId, Timestamp, Nonce),

    %% Generate HMAC-SHA256 signature
    Signature = erlmcp_crypto:hmac_sha256(Key, SignatureBase),

    %% Return signature data
    SignatureData = #{
        method => Method,
        params => Params,
        timestamp => Timestamp,
        nonce => Nonce,
        signature => base64:encode(Signature)
    },

    {ok, SignatureData}.

%%--------------------------------------------------------------------
%% Request Verification
%%--------------------------------------------------------------------

%% @doc Verify request signature (default TTL)
-spec verify_request(signing_key(), signature_data(), request_id(), timestamp()) ->
    ok | {error, term()}.
verify_request(Key, SignatureData, RequestId, CurrentTime) ->
    verify_request(Key, SignatureData, RequestId, CurrentTime, 300).

%% @doc Verify request signature with custom TTL
-spec verify_request(signing_key(), signature_data(), request_id(), timestamp(), pos_integer()) ->
    ok | {error, term()}.
verify_request(Key, SignatureData, RequestId, CurrentTime, TTL) ->
    %% Extract signature components
    #{method := Method, params := Params, timestamp := Timestamp,
      nonce := Nonce, signature := Signature} = SignatureData,

    %% Check timestamp expiration
    case (CurrentTime - Timestamp) =< TTL of
        true ->
            ok;
        false ->
            {error, signature_expired}
    end,

    %% Check nonce hasn't been used (replay protection)
    case validate_nonce(Nonce) of
        true ->
            ok;
        false ->
            {error, nonce_reused}
    end,

    %% Rebuild signature base string
    SignatureBase = build_signature_base(Method, Params, RequestId, Timestamp, Nonce),

    %% Decode and verify signature
    try
        DecodedSignature = base64:decode(Signature),
        ExpectedSignature = erlmcp_crypto:hmac_sha256(Key, SignatureBase),

        case erlmcp_crypto:constant_time_compare(DecodedSignature, ExpectedSignature) of
            true ->
                %% Mark nonce as used
                mark_nonce_used(Nonce),
                ok;
            false ->
                {error, invalid_signature}
        end
    catch
        error:_ ->
            {error, invalid_signature_format}
    end.

%%--------------------------------------------------------------------
%% Response Signing
%%--------------------------------------------------------------------

%% @doc Sign response with HMAC-SHA256 (default timestamp)
-spec sign_response(signing_key(), binary(), request_id()) ->
    {ok, signature_data()}.
sign_response(Key, Result, RequestId) ->
    Timestamp = erlang:system_time(second),
    sign_response(Key, Result, RequestId, Timestamp).

%% @doc Sign response with explicit timestamp
-spec sign_response(signing_key(), binary(), request_id(), timestamp()) ->
    {ok, signature_data()}.
sign_response(Key, Result, RequestId, Timestamp) ->
    %% Build signature base string for response
    SignatureBase = build_response_signature_base(Result, RequestId, Timestamp),

    %% Generate HMAC-SHA256 signature
    Signature = erlmcp_crypto:hmac_sha256(Key, SignatureBase),

    %% Return signature data
    SignatureData = #{
        result => Result,
        request_id => RequestId,
        timestamp => Timestamp,
        signature => base64:encode(Signature)
    },

    {ok, SignatureData}.

%%--------------------------------------------------------------------
%% Response Verification
%%--------------------------------------------------------------------

%% @doc Verify response signature (default TTL)
-spec verify_response(signing_key(), signature_data(), timestamp()) ->
    ok | {error, term()}.
verify_response(Key, SignatureData, CurrentTime) ->
    verify_response(Key, SignatureData, CurrentTime, 300).

%% @doc Verify response signature with custom TTL
-spec verify_response(signing_key(), signature_data(), timestamp(), pos_integer()) ->
    ok | {error, term()}.
verify_response(Key, SignatureData, CurrentTime, TTL) ->
    %% Extract signature components
    #{result := Result, request_id := RequestId,
      timestamp := Timestamp, signature := Signature} = SignatureData,

    %% Check timestamp expiration
    case (CurrentTime - Timestamp) =< TTL of
        true ->
            ok;
        false ->
            {error, signature_expired}
    end,

    %% Rebuild signature base string
    SignatureBase = build_response_signature_base(Result, RequestId, Timestamp),

    %% Decode and verify signature
    try
        DecodedSignature = base64:decode(Signature),
        ExpectedSignature = erlmcp_crypto:hmac_sha256(Key, SignatureBase),

        case erlmcp_crypto:constant_time_compare(DecodedSignature, ExpectedSignature) of
            true ->
                ok;
            false ->
                {error, invalid_signature}
        end
    catch
        error:_ ->
            {error, invalid_signature_format}
    end.

%%--------------------------------------------------------------------
%% Nonce Management
%%--------------------------------------------------------------------

%% @doc Generate cryptographic nonce
-spec generate_nonce() -> nonce().
generate_nonce() ->
    erlmcp_crypto:generate_nonce().

%% @doc Validate nonce (check if already used)
-spec validate_nonce(nonce()) -> boolean().
validate_nonce(Nonce) ->
    gen_server:call(?MODULE, {validate_nonce, Nonce}).

%% @doc Mark nonce as used (replay protection)
-spec mark_nonce_used(nonce()) -> ok.
mark_nonce_used(Nonce) ->
    gen_server:cast(?MODULE, {mark_nonce_used, Nonce}).

%% @doc Cleanup expired nonces
-spec cleanup_expired_nonces() -> ok.
cleanup_expired_nonces() ->
    gen_server:cast(?MODULE, cleanup_expired_nonces).

%%--------------------------------------------------------------------
%% Configuration
%%--------------------------------------------------------------------

%% @doc Set signature TTL (time-to-live)
-spec set_signature_ttl(pos_integer()) -> ok.
set_signature_ttl(TTL) when is_integer(TTL), TTL > 0 ->
    gen_server:call(?MODULE, {set_signature_ttl, TTL}).

%% @doc Get signature TTL
-spec get_signature_ttl() -> pos_integer().
get_signature_ttl() ->
    gen_server:call(?MODULE, get_signature_ttl).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    %% Create ETS table for nonce tracking
    Nonces = ets:new(?MODULE, [set, protected, {read_concurrency, true}]),

    %% Get configuration
    SignatureTTL = maps:get(signature_ttl, Config, 300),
    CleanupInterval = maps:get(cleanup_interval, Config, 60000),

    %% Start cleanup timer
    erlang:send_after(CleanupInterval, self(), cleanup_expired_nonces),

    {ok, #state{
        nonces = Nonces,
        signature_ttl = SignatureTTL,
        cleanup_interval = CleanupInterval
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({validate_nonce, Nonce}, _From, #state{nonces = Nonces} = State) ->
    Result = case ets:lookup(Nonces, Nonce) of
        [] ->
            %% Nonce not used yet
            true;
        _ ->
            %% Nonce already used (replay attack)
            false
    end,
    {reply, Result, State};

handle_call({set_signature_ttl, TTL}, _From, State) ->
    {reply, ok, State#state{signature_ttl = TTL}};

handle_call(get_signature_ttl, _From, #state{signature_ttl = TTL} = State) ->
    {reply, TTL, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({mark_nonce_used, Nonce}, #state{nonces = Nonces, signature_ttl = TTL} = State) ->
    UsedAt = erlang:system_time(second),
    ExpiresAt = UsedAt + TTL,
    NonceState = #{
        nonce => Nonce,
        used_at => UsedAt,
        expires_at => ExpiresAt
    },
    ets:insert(Nonces, {Nonce, NonceState}),
    {noreply, State};

handle_cast(cleanup_expired_nonces, #state{nonces = Nonces} = State) ->
    cleanup_expired_nonces_internal(Nonces),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired_nonces, #state{cleanup_interval = Interval} = State) ->
    cleanup_expired_nonces_internal(State#state.nonces),
    erlang:send_after(Interval, self(), cleanup_expired_nonces),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{nonces = Nonces}) ->
    ets:delete(Nonces),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Build signature base string for request
build_signature_base(Method, Params, RequestId, Timestamp, Nonce) ->
    %% Use JSON-RPC request format
    MethodBin = to_binary(Method),
    RequestIdBin = to_binary(RequestId),

    %% Encode params as canonical JSON
    ParamsJson = encode_params_canonical(Params),

    %% Build signature base: Method|RequestId|Params|Timestamp|Nonce
    iolist_to_binary([
        MethodBin, $|,
        RequestIdBin, $|,
        ParamsJson, $|,
        integer_to_binary(Timestamp), $|,
        base64:encode(Nonce)
    ]).

%% @doc Build signature base string for response
build_response_signature_base(Result, RequestId, Timestamp) ->
    RequestIdBin = to_binary(RequestId),

    %% Encode result as canonical JSON
    ResultJson = encode_result_canonical(Result),

    %% Build signature base: RequestId|Result|Timestamp
    iolist_to_binary([
        RequestIdBin, $|,
        ResultJson, $|,
        integer_to_binary(Timestamp)
    ]).

%% @doc Encode params as canonical JSON (sorted keys)
encode_params_canonical(Params) when is_map(Params) ->
    %% Sort keys for canonical encoding
    SortedKeys = lists:sort(maps:keys(Params)),
    EncodedPairs = [encode_param(Key, maps:get(Key, Params)) || Key <- SortedKeys],
    <<"{", (iolist_to_binary(lists:join($,, EncodedPairs)))/binary, "}">>;

encode_params_canonical(Params) when is_list(Params) ->
    %% JSON array
    EncodedItems = [encode_param(Item) || Item <- Params],
    <<"[", (iolist_to_binary(lists:join($,, EncodedItems)))/binary, "]">>;

encode_params_canonical(Params) ->
    to_binary(Params).

%% @doc Encode single param (key-value pair)
encode_param(Key, Value) when is_binary(Key); is_atom(Key) ->
    KeyBin = to_binary(Key),
    ValueBin = encode_param(Value),
    <<"\"", KeyBin/binary, "\":", ValueBin/binary>>.

%% @doc Encode single value
encode_param(Value) when is_binary(Value) ->
    <<"\"", Value/binary, "\">>;
encode_param(Value) when is_integer(Value) ->
    integer_to_binary(Value);
encode_param(Value) when is_float(Value) ->
    %% Use JSON float format
    float_to_binary(Value, [{scientific, 20}]);
encode_param(Value) when is_boolean(Value) ->
    case Value of
        true -> <<"true">>;
        false -> <<"false">>
    end;
encode_param(Value) when is_map(Value) ->
    encode_params_canonical(Value);
encode_param(Value) when is_list(Value) ->
    encode_params_canonical(Value);
encode_param(null) ->
    <<"null">>;
encode_param(undefined) ->
    <<"null">>;
encode_param(Value) ->
    to_binary(Value).

%% @doc Encode result as canonical JSON
encode_result_canonical(Result) when is_binary(Result) ->
    Result;
encode_result_canonical(Result) ->
    to_binary(Result).

%% @doc Cleanup expired nonces from ETS table
cleanup_expired_nonces_internal(Nonces) ->
    CurrentTime = erlang:system_time(second),
    Pattern = {['_', #{expires_at => '$1', used_at => '_'}], [{'=<', '$1', CurrentTime}], ['$_']},
    ExpiredNonces = ets:select(Nonces, Pattern),
    lists:foreach(fun({Nonce, _}) -> ets:delete(Nonces, Nonce) end, ExpiredNonces).

%% @doc Convert to binary
to_binary(Value) when is_binary(Value) -> Value;
to_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8);
to_binary(Value) when is_integer(Value) -> integer_to_binary(Value);
to_binary(Value) when is_list(Value) -> iolist_to_binary(Value);
to_binary(Value) -> term_to_binary(Value).
