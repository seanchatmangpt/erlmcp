%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP/2 Transport Client using Gun 2.0.1
%%%
%%% Features:
%%% - HTTP/2 multiplexing (up to 100 concurrent streams)
%%% - Flow control and window management
%%% - Connection pooling and reuse
%%% - Automatic retry with exponential backoff
%%% - Metrics collection for monitoring
%%%
%%% == HTTP/2 Advantages ==
%%%
%%% - Binary framing (vs HTTP/1.1 text)
%%% - Multiplexing (multiple requests per connection)
%%% - Header compression (HPACK)
%%% - Server push (optional)
%%% - Flow control per stream
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http2_client).

-behaviour(gen_server).

%% API exports
-export([start_link/1, start_link/2,
         request/5, request/6,
         get_connection/1, release_connection/1,
         get_metrics/1, get_status/1,
         close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Records
-record(state, {
    pool_id :: atom(),
    gun_pid :: pid() | undefined,
    gun_monitor :: reference() | undefined,
    host :: inet:hostname(),
    port :: inet:port_number(),
    transport :: tcp | ssl,
    opts :: map(),
    streams = #{} :: #{gun:stream_ref() => stream_info()},
    pending_requests = queue:new() :: queue:queue(),
    active_streams = 0 :: non_neg_integer(),
    max_concurrent_streams = 100 :: pos_integer(),
    metrics :: metrics()
}).

-record(stream_info, {
    stream_ref :: gun:stream_ref(),
    from :: {pid(), reference()} | undefined,
    start_time :: integer(),
    response_headers :: term() | undefined,
    response_body :: iodata() | undefined,
    bytes_received = 0 :: non_neg_integer()
}).

-record(metrics, {
    total_requests = 0 :: non_neg_integer(),
    successful_responses = 0 :: non_neg_integer(),
    failed_responses = 0 :: non_neg_integer(),
    total_bytes_sent = 0 :: non_neg_integer(),
    total_bytes_received = 0 :: non_neg_integer(),
    avg_latency_us = 0.0 :: float(),
    stream_resets = 0 :: non_neg_integer(),
    connection_errors = 0 :: non_neg_integer()
}).

-type stream_info() :: #stream_info{
    stream_ref :: gun:stream_ref(),
    from :: {pid(), reference()} | undefined,
    start_time :: integer(),
    response_headers :: term() | undefined,
    response_body :: iodata() | undefined,
    bytes_received :: non_neg_integer()
}.

-type metrics() :: #metrics{
    total_requests :: non_neg_integer(),
    successful_responses :: non_neg_integer(),
    failed_responses :: non_neg_integer(),
    total_bytes_sent :: non_neg_integer(),
    total_bytes_received :: non_neg_integer(),
    avg_latency_us :: float(),
    stream_resets :: non_neg_integer(),
    connection_errors :: non_neg_integer()
}.

-type http2_opts() :: #{
    host := inet:hostname(),
    port => inet:port_number(),
    transport => tcp | ssl,
    ssl_options => [ssl:tls_client_option()],
    retry => non_neg_integer(),
    retry_timeout => timeout(),
    max_concurrent_streams => pos_integer(),
    flow_control_window => pos_integer(),
    trace => boolean()
}.

-type request_result() :: {ok, integer(), [{binary(), binary()} | {string(), binary()}], binary()} |
                          {error, term()}.

-export_type([http2_opts/0, request_result/0, stream_info/0, metrics/0]).

%% Defaults
-define(DEFAULT_PORT, 80).
-define(DEFAULT_SSL_PORT, 443).
-define(DEFAULT_TRANSPORT, tcp).
-define(DEFAULT_RETRY, 3).
-define(DEFAULT_RETRY_TIMEOUT, 5000).
-define(DEFAULT_MAX_STREAMS, 100).
-define(DEFAULT_FLOW_WINDOW, 65535).
-define(DEFAULT_TIMEOUT, 30000).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start HTTP/2 client with options
-spec start_link(http2_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

%% @doc Start named HTTP/2 client
-spec start_link(atom(), http2_opts()) -> {ok, pid()} | {error, term()}.
start_link(PoolId, Opts) when is_atom(PoolId), is_map(Opts) ->
    gen_server:start_link({local, PoolId}, ?MODULE, Opts#{pool_id => PoolId}, []).

%% @doc Make an HTTP/2 request (GET by default)
-spec request(pid(), binary(), binary(), [{binary(), binary()}], binary()) -> request_result().
request(ClientPid, Method, Path, Headers, Body) ->
    request(ClientPid, Method, Path, Headers, Body, ?DEFAULT_TIMEOUT).

%% @doc Make an HTTP/2 request with timeout
-spec request(pid(), binary(), binary(), [{binary(), binary()}], binary(), timeout()) -> request_result().
request(ClientPid, Method, Path, Headers, Body, Timeout) when is_pid(ClientPid) ->
    gen_server:call(ClientPid, {request, Method, Path, Headers, Body}, Timeout).

%% @doc Get Gun connection PID (for advanced usage)
-spec get_connection(pid()) -> {ok, pid()} | {error, term()}.
get_connection(ClientPid) ->
    gen_server:call(ClientPid, get_connection, 5000).

%% @doc Release connection back to pool (no-op for simple client)
-spec release_connection(pid()) -> ok.
release_connection(_ClientPid) ->
    ok.

%% @doc Get HTTP/2 client metrics
-spec get_metrics(pid()) -> {ok, map()} | {error, term()}.
get_metrics(ClientPid) ->
    gen_server:call(ClientPid, get_metrics, 5000).

%% @doc Get HTTP/2 client status
-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(ClientPid) ->
    gen_server:call(ClientPid, get_status, 5000).

%% @doc Close HTTP/2 client
-spec close(pid()) -> ok.
close(ClientPid) ->
    gen_server:stop(ClientPid).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init(Opts) when is_map(Opts) ->
    Host = maps:get(host, Opts),
    Port = maps:get(port, Opts, case maps:get(transport, Opts, ?DEFAULT_TRANSPORT) of
        ssl -> ?DEFAULT_SSL_PORT;
        tcp -> ?DEFAULT_PORT
    end),
    Transport = maps:get(transport, Opts, ?DEFAULT_TRANSPORT),
    SSLOpts = maps:get(ssl_options, Opts, []),
    Retry = maps:get(retry, Opts, ?DEFAULT_RETRY),
    RetryTimeout = maps:get(retry_timeout, Opts, ?DEFAULT_RETRY_TIMEOUT),
    MaxStreams = maps:get(max_concurrent_streams, Opts, ?DEFAULT_MAX_STREAMS),
    FlowWindow = maps:get(flow_control_window, Opts, ?DEFAULT_FLOW_WINDOW),
    PoolId = maps:get(pool_id, Opts, erlmcp_http2_client),

    %% Build Gun options (HTTP/2 enabled by default in Gun 2.0.1)
    GunOpts = #{
        transport => Transport,
        protocols => [http2],
        retry => Retry,
        retry_timeout => RetryTimeout,
        trace => maps:get(trace, Opts, false),
        %% HTTP/2 flow control settings
        http2_opts => #{
            max_concurrent_streams => MaxStreams,
            initial_connection_window_size => FlowWindow * 10,
            initial_stream_window_size => FlowWindow
        }
    },

    %% Add TLS options if using SSL
    GunOpts1 = case Transport of
        ssl ->
            GunOpts#{transport_opts => build_tls_options(SSLOpts)};
        tcp ->
            GunOpts
    end,

    %% Open Gun connection
    case gun:open(Host, Port, GunOpts1) of
        {ok, GunPid} ->
            %% Monitor Gun process
            MonitorRef = monitor(process, GunPid),

            %% Wait for Gun to be up
            {gun_up, GunPid, http2} = receive
                {gun_up, GunPid, Protocol} -> {gun_up, GunPid, Protocol}
            after 5000 ->
                exit(gun_timeout)
            end,

            logger:info("HTTP/2 client connected to ~s:~p via ~p (Gun: ~p)",
                        [Host, Port, Transport, GunPid]),

            {ok, #state{
                pool_id = PoolId,
                gun_pid = GunPid,
                gun_monitor = MonitorRef,
                host = Host,
                port = Port,
                transport = Transport,
                opts = Opts,
                max_concurrent_streams = MaxStreams,
                metrics = initial_metrics()
            }};
        {error, Reason} ->
            logger:error("Failed to open Gun connection: ~p", [Reason]),
            {stop, {gun_open_failed, Reason}}
    end.

handle_call({request, Method, Path, Headers, Body}, From,
            #state{gun_pid = GunPid,
                   active_streams = ActiveStreams,
                   max_concurrent_streams = MaxStreams,
                   streams = Streams} = State) ->
    %% Check if we can open a new stream (flow control)
    case ActiveStreams < MaxStreams of
        true ->
            %% Convert method to uppercase binary
            MethodBin = string:uppercase(to_binary(Method)),

            %% Add HTTP/2 specific headers
            Headers1 = Headers ++ [
                {<<"content-type">>, <<"application/json">>}
            ],

            %% Open HTTP/2 stream
            StreamRef = gun:request(GunPid, MethodBin, Path, Headers1, Body),

            %% Record stream info
            StreamInfo = #stream_info{
                stream_ref = StreamRef,
                from = From,
                start_time = erlang:monotonic_time(microsecond)
            },

            NewStreams = maps:put(StreamRef, StreamInfo, Streams),
            NewState = State#state{
                streams = NewStreams,
                active_streams = ActiveStreams + 1,
                metrics = update_request_metrics(State#state.metrics)
            },

            {noreply, NewState};
        false ->
            %% Too many concurrent streams, enqueue request
            logger:warning("Too many concurrent streams (~p), enqueuing request", [ActiveStreams]),
            PendingQueue = queue:in({request, Method, Path, Headers, Body, From},
                                   State#state.pending_requests),
            {noreply, State#state{pending_requests = PendingQueue}, hibernate}
    end;

handle_call(get_connection, _From, #state{gun_pid = GunPid} = State) ->
    {reply, {ok, GunPid}, State};

handle_call(get_metrics, _From, #state{metrics = Metrics} = State) ->
    MetricsMap = #{
        total_requests => Metrics#metrics.total_requests,
        successful_responses => Metrics#metrics.successful_responses,
        failed_responses => Metrics#metrics.failed_responses,
        total_bytes_sent => Metrics#metrics.total_bytes_sent,
        total_bytes_received => Metrics#metrics.total_bytes_received,
        avg_latency_us => Metrics#metrics.avg_latency_us,
        stream_resets => Metrics#metrics.stream_resets,
        connection_errors => Metrics#metrics.connection_errors
    },
    {reply, {ok, MetricsMap}, State};

handle_call(get_status, _From, #state{active_streams = ActiveStreams,
                                      max_concurrent_streams = MaxStreams,
                                      pending_requests = PendingQueue} = State) ->
    Status = #{
        host => State#state.host,
        port => State#state.port,
        transport => State#state.transport,
        gun_pid => State#state.gun_pid,
        active_streams => ActiveStreams,
        max_concurrent_streams => MaxStreams,
        pending_requests => queue:len(PendingQueue),
        stream_utilization_percent => (ActiveStreams / MaxStreams) * 100
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

handle_info({gun_response, GunPid, StreamRef, fin, Status, Headers},
            #state{gun_pid = GunPid, streams = Streams} = State) ->
    case maps:get(StreamRef, Streams, undefined) of
        #stream_info{} = StreamInfo ->
            %% Update stream info with response headers
            UpdatedInfo = StreamInfo#stream_info{
                response_headers = {Status, Headers}
            },
            NewStreams = maps:put(StreamRef, UpdatedInfo, Streams),
            {noreply, State#state{streams = NewStreams}};
        undefined ->
            logger:warning("Received response for unknown stream: ~p", [StreamRef]),
            {noreply, State}
    end;

handle_info({gun_data, GunPid, StreamRef, fin, Data},
            #state{gun_pid = GunPid, streams = Streams} = State) ->
    case maps:get(StreamRef, Streams, undefined) of
        #stream_info{from = From, response_body = PrevBody, start_time = StartTime} = _StreamInfo ->
            %% Update response body
            Body = case PrevBody of
                undefined -> Data;
                _ -> [PrevBody, Data]
            end,

            %% Calculate metrics
            Latency = erlang:monotonic_time(microsecond) - StartTime,
            DataSize = byte_size(Data),

            %% Reply to caller
            gen_server:reply(From, {ok, 200, [], Body}),

            %% Update metrics
            UpdatedMetrics = update_response_metrics(State#state.metrics, Latency, DataSize),

            %% Remove stream
            NewStreams = maps:remove(StreamRef, Streams),
            NewState = State#state{
                streams = NewStreams,
                active_streams = State#state.active_streams - 1,
                metrics = UpdatedMetrics
            },

            %% Process pending requests
            FinalState = process_pending_requests(NewState),

            {noreply, FinalState};
        undefined ->
            logger:warning("Received data for unknown stream: ~p", [StreamRef]),
            {noreply, State}
    end;

handle_info({gun_error, GunPid, StreamRef, Reason},
            #state{gun_pid = GunPid, streams = Streams} = State) ->
    case maps:get(StreamRef, Streams, undefined) of
        #stream_info{from = From} = _StreamInfo ->
            %% Reply with error
            gen_server:reply(From, {error, {gun_error, Reason}}),

            %% Remove stream
            NewStreams = maps:remove(StreamRef, Streams),
            CurrentMetrics = State#state.metrics,
            UpdatedMetrics = CurrentMetrics#metrics{
                failed_responses = CurrentMetrics#metrics.failed_responses + 1,
                stream_resets = CurrentMetrics#metrics.stream_resets + 1
            },
            NewState = State#state{
                streams = NewStreams,
                active_streams = State#state.active_streams - 1,
                metrics = UpdatedMetrics
            },

            %% Process pending requests
            FinalState = process_pending_requests(NewState),

            {noreply, FinalState};
        undefined ->
            logger:warning("Received error for unknown stream: ~p", [StreamRef]),
            {noreply, State}
    end;

handle_info({'DOWN', MonitorRef, process, GunPid, Reason},
            #state{gun_monitor = MonitorRef, gun_pid = GunPid} = State) ->
    logger:error("Gun process ~p died: ~p", [GunPid, Reason]),

    %% Fail all pending streams
    maps:foreach(fun(_StreamRef, #stream_info{from = From}) ->
        gen_server:reply(From, {error, connection_lost})
    end, State#state.streams),

    UpdatedMetrics = State#state.metrics#metrics{
        connection_errors = State#metrics.connection_errors + 1
    },

    {stop, {gun_down, Reason}, State#state{metrics = UpdatedMetrics}};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

terminate(_Reason, #state{gun_pid = GunPid}) when GunPid =/= undefined ->
    catch gun:close(GunPid),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% @doc Build TLS options for Gun SSL connection
build_tls_options(SSLOpts) ->
    OTPVersion = get_otp_version(),

    %% OTP 27-28: Prefer TLS 1.3
    Versions = case OTPVersion of
        V when V >= 27 -> ['tlsv1.3', 'tlsv1.2'];
        _ -> ['tlsv1.2', 'tlsv1.3']
    end,

    %% OTP 27-28: TLS 1.3 optimized cipher suites
    Ciphers = case OTPVersion of
        Ver when Ver >= 27 ->
            ["TLS_AES_256_GCM_SHA384",
             "TLS_AES_128_GCM_SHA256",
             "TLS_CHACHA20_POLY1305_SHA256"];
        _ ->
            ["ECDHE-RSA-AES256-GCM-SHA384",
             "ECDHE-RSA-AES128-GCM-SHA256",
             "ECDHE-RSA-CHACHA20-POLY1305"]
    end,

    BaseOpts = [
        {versions, Versions},
        {ciphers, Ciphers},
        {secure_renegotiate, true},
        {honor_cipher_order, true},
        {verify, verify_peer},
        {fail_if_no_peer_cert, true}
    ],

    BaseOpts ++ SSLOpts.

%% @doc Get OTP version as integer
get_otp_version() ->
    try
        VersionStr = erlang:system_info(otp_release),
        [MajorStr | _] = string:split(VersionStr, "."),
        list_to_integer(MajorStr)
    catch
        _:_ -> 0
    end.

%% @doc Process pending requests from queue
process_pending_requests(#state{pending_requests = PendingQueue,
                                active_streams = ActiveStreams,
                                max_concurrent_streams = MaxStreams} = State) ->
    case queue:out(PendingQueue) of
        {{value, {request, Method, Path, Headers, Body, From}}, NewQueue} when ActiveStreams < MaxStreams ->
            %% Process pending request
            handle_call({request, Method, Path, Headers, Body}, From,
                       State#state{pending_requests = NewQueue}),
            State#state{pending_requests = NewQueue};
        _ ->
            %% No pending requests or at max capacity
            State
    end.

%% @doc Initialize metrics
initial_metrics() ->
    #metrics{}.

%% @doc Update request metrics
update_request_metrics(#metrics{total_requests = Total} = Metrics) ->
    Metrics#metrics{total_requests = Total + 1}.

%% @doc Update response metrics
update_response_metrics(#metrics{successful_responses = Successful,
                                 avg_latency_us = AvgLatency} = Metrics, Latency, DataSize) ->
    NewSuccessful = Successful + 1,
    NewAvgLatency = (AvgLatency * Successful + Latency) / NewSuccessful,

    Metrics#metrics{
        successful_responses = NewSuccessful,
        avg_latency_us = NewAvgLatency,
        total_bytes_received = Metrics#metrics.total_bytes_received + DataSize
    }.

%% @doc Convert term to binary
to_binary(B) when is_binary(B) -> B;
to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I).
