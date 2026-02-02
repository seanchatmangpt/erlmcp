-module(erlmcp_transport_http_server).

-behaviour(gen_server).

%% API
-export([start_link/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Re-use types from erlmcp_transport_http
-include_lib("kernel/include/logger.hrl").

-record(state,
        {url :: string(),
         owner :: pid(),
         method :: get | post,
         headers :: [{binary(), binary()}],
         timeout :: timeout(),
         connect_timeout :: timeout(),
         max_retries :: non_neg_integer(),
         retry_delay :: pos_integer(),
         gun_pid :: pid() | undefined,
         gun_monitor :: reference() | undefined,
         gun_stream_ref :: reference() | undefined,
         pending_requests = #{} ::
             #{reference() => {pid(), reference(), binary(), non_neg_integer()}},
         message_queue :: queue:queue({binary(), {pid(), reference()}}),
         pool_size :: non_neg_integer(),
         active_requests :: non_neg_integer(),
         ssl_options :: [ssl:tls_client_option()],
         host :: string(),
         port :: pos_integer(),
         path :: string(),
         scheme :: http | https}).

-type state() :: #state{}.
-type http_opts() ::
    #{url := binary() | string(),
      owner := pid(),
      method => get | post,
      headers => [{string() | binary(), string() | binary()}],
      timeout => timeout(),
      connect_timeout => timeout(),
      max_retries => non_neg_integer(),
      retry_delay => pos_integer(),
      ssl_options => [ssl:tls_client_option()],
      pool_size => non_neg_integer()}.

%% Default values
-define(DEFAULT_TIMEOUT, 30000).
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_MAX_RETRIES, 3).
-define(DEFAULT_RETRY_DELAY, 1000).
-define(DEFAULT_POOL_SIZE, 5).
-define(DEFAULT_METHOD, post).

%%====================================================================
%% API
%%====================================================================

-spec start_link(http_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(http_opts()) -> {ok, state()} | {error, term()}.
init(Opts) when is_map(Opts) ->
    process_flag(trap_exit, true),

    %% Ensure required applications are started
    ensure_apps_started(),

    %% Monitor owner
    Owner = maps:get(owner, Opts),
    monitor(process, Owner),

    %% Build state
    State = build_initial_state(Opts),

    %% Connect to server
    case connect(State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({send, Data}, From, State) ->
    %% Queue the request
    NewState = enqueue_request(Data, From, State),
    %% Try to process queue
    {noreply, process_request_queue(NewState)};
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({extract_session_id, Req}, _From, State) ->
    %% Extract session ID from request headers
    SessionId = cowboy_req:header(<<"mcp-session-id">>, Req, undefined),
    {reply, SessionId, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
%% Gun connection established
handle_info({gun_up, GunPid, _Protocol}, #state{gun_pid = GunPid} = State) ->
    logger:info("Gun connection established to ~s://~s:~p",
                [State#state.scheme, State#state.host, State#state.port]),
    {noreply, process_request_queue(State)};
%% Gun connection down
handle_info({gun_down, GunPid, _Protocol, Reason, _}, #state{gun_pid = GunPid} = State) ->
    logger:warning("Gun connection down: ~p", [Reason]),
    %% Attempt reconnect
    case connect(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            %% Will retry on next request
            {noreply, State#state{gun_pid = undefined, gun_monitor = undefined}}
    end;
%% Gun response headers
handle_info({gun_response, GunPid, StreamRef, fin, StatusCode, Headers},
            #state{gun_pid = GunPid} = State) ->
    %% Response with no body
    {noreply, handle_gun_response(StreamRef, StatusCode, Headers, <<>>, State)};
handle_info({gun_response, GunPid, StreamRef, nofin, StatusCode, Headers},
            #state{gun_pid = GunPid} = State) ->
    %% Response with body coming
    %% Store headers and status, wait for data
    NewPending =
        case maps:get(StreamRef, State#state.pending_requests, undefined) of
            {From, FromRef, Data, Attempts} ->
                maps:put(StreamRef,
                         {From, FromRef, Data, Attempts, StatusCode, Headers},
                         State#state.pending_requests);
            _ ->
                State#state.pending_requests
        end,
    {noreply, State#state{pending_requests = NewPending}};
%% Gun response data
handle_info({gun_data, GunPid, StreamRef, fin, Data}, #state{gun_pid = GunPid} = State) ->
    %% Final data chunk
    case maps:take(StreamRef, State#state.pending_requests) of
        {{From, FromRef, ReqData, Attempts, StatusCode, Headers}, NewPending} ->
            NewState =
                State#state{pending_requests = NewPending,
                            active_requests = max(0, State#state.active_requests - 1)},
            FinalState =
                handle_gun_response(StreamRef,
                                    StatusCode,
                                    Headers,
                                    Data,
                                    NewState#state{pending_requests =
                                                       maps:put(StreamRef,
                                                                {From, FromRef, ReqData, Attempts},
                                                                NewPending)}),
            {noreply, process_request_queue(FinalState)};
        error ->
            logger:warning("Received data for unknown stream: ~p", [StreamRef]),
            {noreply, State}
    end;
handle_info({gun_data, GunPid, _StreamRef, nofin, _Data}, #state{gun_pid = GunPid} = State) ->
    %% More data coming, accumulate if needed
    %% For simplicity, we handle single chunk responses
    {noreply, State};
%% Gun error
handle_info({gun_error, GunPid, StreamRef, Reason}, #state{gun_pid = GunPid} = State) ->
    logger:error("Gun stream error ~p: ~p", [StreamRef, Reason]),
    {noreply, handle_gun_error(StreamRef, Reason, State)};
handle_info({gun_error, GunPid, Reason}, #state{gun_pid = GunPid} = State) ->
    logger:error("Gun connection error: ~p", [Reason]),
    {noreply, State};
%% Retry request
handle_info({retry_request, Data, From, Attempts}, State) ->
    {noreply, retry_request(Data, From, Attempts, State)};
%% Owner process down
handle_info({'DOWN', _MonitorRef, process, Owner, Reason}, #state{owner = Owner} = State) ->
    logger:info("Owner process ~p died: ~p", [Owner, Reason]),
    {stop, {owner_died, Reason}, State};
%% Gun monitor down
handle_info({'DOWN', MonitorRef, process, GunPid, Reason},
            #state{gun_pid = GunPid, gun_monitor = MonitorRef} = State) ->
    logger:warning("Gun process ~p died: ~p", [GunPid, Reason]),
    %% Reconnect
    case connect(State#state{gun_pid = undefined, gun_monitor = undefined}) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            {noreply, State#state{gun_pid = undefined, gun_monitor = undefined}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{gun_pid = undefined}) ->
    ok;
terminate(_Reason, #state{gun_pid = GunPid}) ->
    gun:close(GunPid),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Initialization
%%====================================================================

-spec ensure_apps_started() -> ok.
ensure_apps_started() ->
    %% Start gun and all its dependencies
    case application:ensure_all_started(gun) of
        {ok, _Started1} ->
            ok;
        {error, {already_started, gun}} ->
            ok
    end,

    %% Start SSL if needed
    case application:ensure_all_started(ssl) of
        {ok, _Started2} ->
            ok;
        {error, {already_started, ssl}} ->
            ok
    end,

    %% Start crypto (dependency of SSL)
    case application:ensure_started(crypto) of
        ok ->
            ok;
        {error, {already_started, crypto}} ->
            ok
    end,

    ok.

-spec build_initial_state(http_opts()) -> state().
build_initial_state(Opts) ->
    Url = normalize_url(maps:get(url, Opts)),
    {Scheme, Host, Port, Path} = parse_url(Url),

    #state{url = Url,
           owner = maps:get(owner, Opts),
           method = maps:get(method, Opts, ?DEFAULT_METHOD),
           headers = normalize_headers(maps:get(headers, Opts, [])),
           timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
           connect_timeout = maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
           max_retries = maps:get(max_retries, Opts, ?DEFAULT_MAX_RETRIES),
           retry_delay = maps:get(retry_delay, Opts, ?DEFAULT_RETRY_DELAY),
           pending_requests = #{},
           message_queue = queue:new(),
           pool_size = maps:get(pool_size, Opts, ?DEFAULT_POOL_SIZE),
           active_requests = 0,
           ssl_options = maps:get(ssl_options, Opts, []),
           host = Host,
           port = Port,
           path = Path,
           scheme = Scheme}.

-spec normalize_url(binary() | string()) -> string().
normalize_url(Url) when is_binary(Url) ->
    binary_to_list(Url);
normalize_url(Url) when is_list(Url) ->
    Url.

-spec parse_url(string()) -> {http | https, string(), pos_integer(), string()}.
parse_url("http://" ++ Rest) ->
    parse_url_parts(http, 80, Rest);
parse_url("https://" ++ Rest) ->
    parse_url_parts(https, 443, Rest);
parse_url(Rest) ->
    parse_url_parts(http, 80, Rest).

-spec parse_url_parts(http | https, pos_integer(), string()) ->
                         {http | https, string(), pos_integer(), string()}.
parse_url_parts(Scheme, DefaultPort, Rest) ->
    case string:split(Rest, "/", leading) of
        [HostPort, Path] ->
            {Host, Port} = parse_host_port(HostPort, DefaultPort),
            {Scheme, Host, Port, "/" ++ Path};
        [HostPort] ->
            {Host, Port} = parse_host_port(HostPort, DefaultPort),
            {Scheme, Host, Port, "/"}
    end.

-spec parse_host_port(string(), pos_integer()) -> {string(), pos_integer()}.
parse_host_port(HostPort, DefaultPort) ->
    case string:split(HostPort, ":") of
        [Host, PortStr] ->
            Port = list_to_integer(PortStr),
            {Host, Port};
        [Host] ->
            {Host, DefaultPort}
    end.

-spec normalize_headers([{term(), term()}]) -> [{binary(), binary()}].
normalize_headers(Headers) ->
    DefaultHeaders =
        [{<<"content-type">>, <<"application/json">>},
         {<<"accept">>, <<"application/json">>},
         {<<"user-agent">>, <<"erlmcp/1.0">>}],

    %% Convert and merge headers
    UserHeaders = lists:map(fun({K, V}) -> {to_binary(K), to_binary(V)} end, Headers),

    %% User headers override defaults
    merge_headers(DefaultHeaders, UserHeaders).

-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B) ->
    B;
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(A) when is_atom(A) ->
    atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) ->
    integer_to_binary(I).

-spec merge_headers([{binary(), binary()}], [{binary(), binary()}]) -> [{binary(), binary()}].
merge_headers(Defaults, User) ->
    %% Create a map from user headers for efficient lookup
    UserMap = maps:from_list([{string:lowercase(K), {K, V}} || {K, V} <- User]),

    %% Filter defaults and add all user headers
    Filtered =
        lists:filter(fun({K, _}) ->
                        not
                            maps:is_key(
                                string:lowercase(K), UserMap)
                     end,
                     Defaults),

    Filtered ++ User.

%%====================================================================
%% Internal functions - Connection Management
%%====================================================================

-spec connect(state()) -> {ok, state()} | {error, term()}.
connect(#state{gun_pid = OldPid, gun_monitor = OldMon} = State) ->
    %% Close old connection if exists
    case OldPid of
        undefined ->
            ok;
        _ ->
            case OldMon of
                undefined ->
                    ok;
                _ ->
                    demonitor(OldMon, [flush])
            end,
            gun:close(OldPid)
    end,

    %% Open new connection
    GunOpts = build_gun_opts(State),

    case gun:open(State#state.host, State#state.port, GunOpts) of
        {ok, GunPid} ->
            MonitorRef = monitor(process, GunPid),
            NewState = State#state{gun_pid = GunPid, gun_monitor = MonitorRef},

            %% Wait for connection
            case gun:await_up(GunPid, State#state.connect_timeout) of
                {ok, _Protocol} ->
                    {ok, NewState};
                {error, timeout} ->
                    logger:error("Gun connection timeout"),
                    gun:close(GunPid),
                    {error, connect_timeout};
                {error, Reason} ->
                    logger:error("Gun connection failed: ~p", [Reason]),
                    gun:close(GunPid),
                    {error, Reason}
            end;
        {error, Reason} ->
            logger:error("Failed to open gun connection: ~p", [Reason]),
            {error, Reason}
    end.

-spec build_gun_opts(state()) -> map().
build_gun_opts(#state{scheme = https,
                      ssl_options = SslOpts,
                      connect_timeout = ConnTimeout,
                      timeout = Timeout,
                      host = Host}) ->
    %% OTP 27-28 TLS 1.3 optimization: Build TLS options with performance improvements
    ValidatedOpts =
        case erlmcp_tls_validation:build_tls_options(SslOpts, Host) of
            {ok, Opts} ->
                Opts;
            {error, Reason} ->
                logger:error("TLS validation failed: ~p - using strict defaults", [Reason]),
                %% Use strict defaults on validation failure
                build_strict_tls_options(Host)
        end,

    %% OTP 27-28: Prefer HTTP/2 with TLS 1.3 for better performance
    #{protocols => [http2, http],
      transport => ssl,
      tls_opts => ValidatedOpts,
      transport_opts => #{  %% OTP 27-28 optimized transport options
          verify => verify_peer,
          versions => ['tlsv1.3', 'tlsv1.2'],  %% Prefer TLS 1.3
          ciphers => ssl:cipher_suites(all, 'tlsv1.3')  %% TLS 1.3 ciphers
      },
      connect_timeout => ConnTimeout,
      http_opts => #{keepalive => Timeout},
      http2_opts => #{keepalive => Timeout}};
build_gun_opts(#state{connect_timeout = ConnTimeout, timeout = Timeout}) ->
    #{protocols => [http2, http],
      transport => tcp,
      connect_timeout => ConnTimeout,
      http_opts => #{keepalive => Timeout},
      http2_opts => #{keepalive => Timeout}}.

-spec build_strict_tls_options(string()) -> [ssl:tls_client_option()].
build_strict_tls_options(Hostname) ->
    %% OTP 27-28 optimized: Strict TLS defaults with TLS 1.3 preferred
    OTPVersion = get_otp_version(),

    %% OTP 27-28: Prefer TLS 1.3 for performance (15-25% improvement)
    Versions =
        case OTPVersion of
            V when V >= 27 ->
                ['tlsv1.3', 'tlsv1.2'];
            _ ->
                ['tlsv1.2', 'tlsv1.3']
        end,

    %% OTP 27-28: TLS 1.3 optimized cipher suites
    Ciphers =
        case OTPVersion of
            V when V >= 27 ->
                %% TLS 1.3 ciphers (AES_256_GCM first for hardware acceleration)
                ["TLS_AES_256_GCM_SHA384",
                 "TLS_AES_128_GCM_SHA256",
                 "TLS_CHACHA20_POLY1305_SHA256"];
            _ ->
                %% TLS 1.2 fallback ciphers
                ["ECDHE-RSA-AES256-GCM-SHA384",
                 "ECDHE-RSA-AES128-GCM-SHA256",
                 "ECDHE-RSA-CHACHA20-POLY1305"]
        end,

    [{verify, verify_peer},
     {server_name_indication, Hostname},
     {versions, Versions},
     {depth, 3},
     {ciphers, Ciphers}].

%% @private Get OTP version as integer
get_otp_version() ->
    try
        VersionStr = erlang:system_info(otp_release),
        [MajorStr | _] = string:split(VersionStr, "."),
        list_to_integer(MajorStr)
    catch
        _:_ ->
            0
    end.

%%====================================================================
%% Internal functions - Request Handling
%%====================================================================

-spec enqueue_request(binary(), {pid(), term()}, state()) -> state().
enqueue_request(Data, From, State) ->
    %% Validate message size before enqueueing
    DataSize = byte_size(Data),
    case erlmcp_memory_guard:check_allocation(DataSize) of
        ok ->
            %% Add to queue
            NewQueue = queue:in({Data, From}, State#state.message_queue),
            State#state{message_queue = NewQueue};
        {error, payload_too_large} ->
            logger:error("HTTP message too large: ~p bytes", [DataSize]),
            gen_server:reply(From, {error, {message_too_large, DataSize}}),
            State;
        {error, resource_exhausted} ->
            logger:error("System memory exhausted, rejecting HTTP request"),
            gen_server:reply(From, {error, resource_exhausted}),
            State
    end.

-spec process_request_queue(state()) -> state().
process_request_queue(#state{active_requests = Active, pool_size = PoolSize} = State)
    when Active >= PoolSize ->
    %% Pool is full, wait for completions
    State;
process_request_queue(#state{message_queue = Queue} = State) ->
    case queue:out(Queue) of
        {{value, {Data, From}}, NewQueue} ->
            %% Process next request
            NewState = State#state{message_queue = NewQueue},
            process_request_queue(send_request(Data, From, NewState));
        {empty, _} ->
            %% No more requests
            State
    end.

-spec send_request(binary(), {pid(), term()}, state()) -> state().
send_request(Data, From, #state{gun_pid = undefined} = State) ->
    %% No connection, try to connect
    case connect(State) of
        {ok, NewState} ->
            send_request(Data, From, NewState);
        {error, Reason} ->
            gen_server:reply(From, {error, {connect_failed, Reason}}),
            State
    end;
send_request(Data, From, State) ->
    case perform_request(Data, State) of
        {ok, StreamRef} ->
            %% Track pending request
            {_, FromRef} = From,
            NewPending =
                maps:put(StreamRef, {From, FromRef, Data, 0}, State#state.pending_requests),
            State#state{pending_requests = NewPending,
                        active_requests = State#state.active_requests + 1};
        {error, Reason} = Error ->
            %% Immediate failure
            gen_server:reply(From, Error),
            logger:error("HTTP request failed immediately: ~p", [Reason]),
            State
    end.

-spec perform_request(binary(), state()) -> {ok, reference()} | {error, term()}.
perform_request(Data,
                #state{gun_pid = GunPid,
                       path = Path,
                       headers = Headers,
                       method = Method}) ->
    try
        StreamRef =
            case Method of
                post ->
                    gun:post(GunPid, Path, Headers, Data);
                get ->
                    %% For GET, append data as query parameters
                    QueryPath = Path ++ "?" ++ binary_to_list(Data),
                    gun:get(GunPid, QueryPath, Headers)
            end,
        {ok, StreamRef}
    catch
        error:Reason ->
            logger:error("Failed to send gun request: ~p", [Reason]),
            {error, Reason}
    end.

-spec handle_gun_response(reference(),
                          non_neg_integer(),
                          [{binary(), binary()}],
                          binary(),
                          state()) ->
                             state().
handle_gun_response(StreamRef, StatusCode, Headers, Body, State) ->
    %% Validate response body size before processing
    BodySize = byte_size(Body),
    case erlmcp_memory_guard:check_allocation(BodySize) of
        ok ->
            case maps:take(StreamRef, State#state.pending_requests) of
                {{From, _FromRef, Data, Attempts}, NewPending} ->
                    NewState =
                        State#state{pending_requests = NewPending,
                                    active_requests = max(0, State#state.active_requests - 1)},

                    %% Process the response
                    case process_response(StatusCode, Headers, Body) of
                        {ok, Response} ->
                            %% Success - send to owner and reply
                            State#state.owner ! {transport_message, Response},
                            gen_server:reply(From, ok),
                            NewState;
                        {error, Reason} = Error ->
                            %% Check if we should retry
                            case should_retry(Reason, Attempts, State) of
                                true ->
                                    schedule_retry(Data, From, Attempts + 1, NewState);
                                false ->
                                    gen_server:reply(From, Error),
                                    NewState
                            end
                    end;
                error ->
                    logger:warning("Received response for unknown stream: ~p", [StreamRef]),
                    State
            end;
        {error, payload_too_large} ->
            logger:error("HTTP response body too large: ~p bytes", [BodySize]),
            %% Clean up the pending request
            case maps:take(StreamRef, State#state.pending_requests) of
                {{From, _FromRef, _Data, _Attempts}, NewPending} ->
                    NewState =
                        State#state{pending_requests = NewPending,
                                    active_requests = max(0, State#state.active_requests - 1)},
                    gen_server:reply(From, {error, {response_too_large, BodySize}}),
                    NewState;
                error ->
                    State
            end;
        {error, resource_exhausted} ->
            logger:error("System memory exhausted, cannot process HTTP response"),
            case maps:take(StreamRef, State#state.pending_requests) of
                {{From, _FromRef, _Data, _Attempts}, NewPending} ->
                    NewState =
                        State#state{pending_requests = NewPending,
                                    active_requests = max(0, State#state.active_requests - 1)},
                    gen_server:reply(From, {error, resource_exhausted}),
                    NewState;
                error ->
                    State
            end
    end.

-spec handle_gun_error(reference(), term(), state()) -> state().
handle_gun_error(StreamRef, Reason, State) ->
    case maps:take(StreamRef, State#state.pending_requests) of
        {{From, _FromRef, Data, Attempts}, NewPending} ->
            NewState =
                State#state{pending_requests = NewPending,
                            active_requests = max(0, State#state.active_requests - 1)},

            %% Check if we should retry
            Error = {error, {gun_error, Reason}},
            case should_retry(Error, Attempts, State) of
                true ->
                    schedule_retry(Data, From, Attempts + 1, NewState);
                false ->
                    gen_server:reply(From, Error),
                    NewState
            end;
        error ->
            logger:warning("Received error for unknown stream: ~p", [StreamRef]),
            State
    end.

-spec process_response(non_neg_integer(), [{binary(), binary()}], binary()) ->
                          {ok, binary()} | {error, term()}.
process_response(StatusCode, Headers, Body) when StatusCode >= 200, StatusCode < 300 ->
    %% Success response
    process_body(Body, Headers);
process_response(StatusCode, _Headers, Body) ->
    %% Error response
    logger:error("HTTP error ~p, Body: ~s", [StatusCode, Body]),
    {error, {http_error, StatusCode, Body}}.

-spec process_body(binary(), [{binary(), binary()}]) -> {ok, binary()} | {error, term()}.
process_body(Body, Headers) when is_binary(Body) ->
    %% Check content type
    case lists:keyfind(<<"content-type">>, 1, [{string:lowercase(K), V} || {K, V} <- Headers]) of
        {_, ContentType} ->
            case binary:match(ContentType, <<"application/json">>) of
                nomatch ->
                    logger:warning("Non-JSON response: ~s", [ContentType]);
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {ok, Body};
process_body(Body, _Headers) ->
    {error, {invalid_body_format, Body}}.

-spec should_retry(term(), non_neg_integer(), state()) -> boolean().
should_retry(_Reason, Attempts, #state{max_retries = MaxRetries}) when Attempts >= MaxRetries ->
    false;
should_retry({http_error, StatusCode, _}, _Attempts, _State) ->
    %% Retry on server errors and rate limiting
    StatusCode >= 500 orelse StatusCode =:= 429;
should_retry({gun_error, _}, _Attempts, _State) ->
    true;
should_retry(_Reason, _Attempts, _State) ->
    false.

-spec schedule_retry(binary(), {pid(), term()}, non_neg_integer(), state()) -> state().
schedule_retry(Data, From, Attempts, State) ->
    Delay = calculate_retry_delay(Attempts, State),
    logger:info("Scheduling HTTP retry ~p in ~p ms", [Attempts, Delay]),

    erlang:send_after(Delay, self(), {retry_request, Data, From, Attempts}),
    State.

-spec retry_request(binary(), {pid(), term()}, non_neg_integer(), state()) -> state().
retry_request(Data, From, _Attempts, State) ->
    %% Resend the request
    send_request(Data, From, State).

-spec calculate_retry_delay(non_neg_integer(), state()) -> pos_integer().
calculate_retry_delay(Attempts, #state{retry_delay = BaseDelay}) ->
    %% Exponential backoff with jitter
    Backoff = BaseDelay * (1 bsl (Attempts - 1)),
    MaxDelay = 60000,  % 1 minute max
    Jitter = rand:uniform(1000),
    min(Backoff + Jitter, MaxDelay).
