-module(erlmcp_socket_metrics).

%% Socket-level metrics for OTP 26-28 transport optimizations
%% Tracks buffer usage, packet drops, backpressure events

-behaviour(gen_server).

%% API
-export([start_link/0,
         record_buffer_usage/3,
         record_packet_drop/2,
         record_backpressure_event/2,
         record_socket_stats/2,
         get_socket_metrics/0,
         get_socket_metrics/1,
         reset_socket_metrics/0,
         get_buffer_summary/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-include("erlmcp.hrl").

%% Types
-type metric_name() :: binary().
-type metric_value() :: number().
-type transport_id() :: atom() | binary().

-record(buffer_metrics,
        {transport_id :: transport_id(),
         rcvbuf_used :: non_neg_integer(),
         sndbuf_used :: non_neg_integer(),
         rcvbuf_size :: pos_integer(),
         sndbuf_size :: pos_integer(),
         rcvbuf_utilization :: float(),
         sndbuf_utilization :: float(),
         timestamp :: integer()}).

-record(packet_drops,
        {transport_id :: transport_id(),
         drops :: non_neg_integer(),
         reason :: term(),
         timestamp :: integer()}).

-record(backpressure_event,
        {transport_id :: transport_id(),
         event_type :: activated | deactivated | timeout,
         buffer_size :: non_neg_integer(),
         threshold :: pos_integer(),
         duration_ms :: non_neg_integer() | undefined,
         timestamp :: integer()}).

-record(socket_stats,
        {transport_id :: transport_id(),
         bytes_sent :: non_neg_integer(),
         bytes_received :: non_neg_integer(),
         packets_sent :: non_neg_integer(),
         packets_received :: non_neg_integer(),
         read_count :: non_neg_integer(),
         write_count :: non_neg_integer(),
         timestamp :: integer()}).

-record(state,
        {buffer_metrics = [] :: [#buffer_metrics{}],
         packet_drops = [] :: [#packet_drops{}],
         backpressure_events = [] :: [#backpressure_event{}],
         socket_stats = [] :: [#socket_stats{}],
         active_backpressure = #{} :: map(),  % Track active backpressure state
         start_time :: integer()}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the socket metrics server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Record buffer usage metrics
-spec record_buffer_usage(transport_id(), #{rcvbuf_used => non_neg_integer(),
                                            sndbuf_used => non_neg_integer(),
                                            rcvbuf_size => pos_integer(),
                                            sndbuf_size => pos_integer()}, pid()) -> ok.
record_buffer_usage(TransportId, BufferData, Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, {record_buffer, TransportId, BufferData});
record_buffer_usage(TransportId, BufferData, _Pid) ->
    gen_server:cast(?MODULE, {record_buffer, TransportId, BufferData}).

%% @doc Record a packet drop event
-spec record_packet_drop(transport_id(), term()) -> ok.
record_packet_drop(TransportId, Reason) ->
    gen_server:cast(?MODULE, {record_packet_drop, TransportId, Reason}).

%% @doc Record a backpressure event
-spec record_backpressure_event(transport_id(), #{type => activated | deactivated | timeout,
                                                  buffer_size => non_neg_integer(),
                                                  threshold => pos_integer(),
                                                  duration_ms => non_neg_integer() | undefined}) -> ok.
record_backpressure_event(TransportId, EventData) ->
    gen_server:cast(?MODULE, {record_backpressure, TransportId, EventData}).

%% @doc Record socket statistics
-spec record_socket_stats(transport_id(), #{bytes_sent => non_neg_integer(),
                                             bytes_received => non_neg_integer(),
                                             packets_sent => non_neg_integer(),
                                             packets_received => non_neg_integer(),
                                             read_count => non_neg_integer(),
                                             write_count => non_neg_integer()}) -> ok.
record_socket_stats(TransportId, Stats) ->
    gen_server:cast(?MODULE, {record_stats, TransportId, Stats}).

%% @doc Get all socket metrics
-spec get_socket_metrics() -> #{atom() => list()}.
get_socket_metrics() ->
    gen_server:call(?MODULE, get_all_metrics).

%% @doc Get metrics for a specific transport
-spec get_socket_metrics(transport_id()) -> #{atom() => term()}.
get_socket_metrics(TransportId) ->
    gen_server:call(?MODULE, {get_metrics, TransportId}).

%% @doc Reset all socket metrics
-spec reset_socket_metrics() -> ok.
reset_socket_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%% @doc Get buffer usage summary across all transports
-spec get_buffer_summary() -> #{atom() => term()}.
get_buffer_summary() ->
    gen_server:call(?MODULE, get_buffer_summary).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    logger:info("Starting socket metrics collector (OTP 26-28 optimizations)"),
    {ok, #state{start_time = erlang:system_time(millisecond)}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call(get_all_metrics, _From, State) ->
    Reply = #{
        buffer_metrics => State#state.buffer_metrics,
        packet_drops => State#state.packet_drops,
        backpressure_events => State#state.backpressure_events,
        socket_stats => State#state.socket_stats
    },
    {reply, Reply, State};
handle_call({get_metrics, TransportId}, _From, State) ->
    Reply = #{
        buffer_metrics => filter_by_transport(State#state.buffer_metrics, TransportId),
        packet_drops => filter_by_transport(State#state.packet_drops, TransportId),
        backpressure_events => filter_by_transport(State#state.backpressure_events, TransportId),
        socket_stats => filter_by_transport(State#state.socket_stats, TransportId),
        active_backpressure => maps:get(TransportId, State#state.active_backpressure, undefined)
    },
    {reply, Reply, State};
handle_call(reset_metrics, _From, State) ->
    {reply, ok, State#state{
        buffer_metrics = [],
        packet_drops = [],
        backpressure_events = [],
        socket_stats = [],
        active_backpressure = #{},
        start_time = erlang:system_time(millisecond)
    }};
handle_call(get_buffer_summary, _From, State) ->
    Summary = calculate_buffer_summary(State),
    {reply, Summary, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({record_buffer, TransportId, BufferData}, State) ->
    RcvBufUsed = maps:get(rcvbuf_used, BufferData, 0),
    SndBufUsed = maps:get(sndbuf_used, BufferData, 0),
    RcvBufSize = maps:get(rcvbuf_size, BufferData, 8192),
    SndBufSize = maps:get(sndbuf_size, BufferData, 8192),

    RcvUtil = case RcvBufSize > 0 of
        true -> (RcvBufUsed / RcvBufSize) * 100.0;
        false -> 0.0
    end,
    SndUtil = case SndBufSize > 0 of
        true -> (SndBufUsed / SndBufSize) * 100.0;
        false -> 0.0
    end,

    Metric = #buffer_metrics{
        transport_id = TransportId,
        rcvbuf_used = RcvBufUsed,
        sndbuf_used = SndBufUsed,
        rcvbuf_size = RcvBufSize,
        sndbuf_size = SndBufSize,
        rcvbuf_utilization = RcvUtil,
        sndbuf_utilization = SndUtil,
        timestamp = erlang:system_time(millisecond)
    },

    NewMetrics = [Metric | lists:sublist(State#state.buffer_metrics, 999)],
    {noreply, State#state{buffer_metrics = NewMetrics}};
handle_cast({record_packet_drop, TransportId, Reason}, State) ->
    Drop = #packet_drops{
        transport_id = TransportId,
        drops = 1,
        reason = Reason,
        timestamp = erlang:system_time(millisecond)
    },

    NewDrops = [Drop | lists:sublist(State#state.packet_drops, 999)],
    {noreply, State#state{packet_drops = NewDrops}};
handle_cast({record_backpressure, TransportId, EventData}, State) ->
    EventType = maps:get(type, EventData, activated),
    BufferSize = maps:get(buffer_size, EventData, 0),
    Threshold = maps:get(threshold, EventData, 8192),
    DurationMs = maps:get(duration_ms, EventData, undefined),

    Event = #backpressure_event{
        transport_id = TransportId,
        event_type = EventType,
        buffer_size = BufferSize,
        threshold = Threshold,
        duration_ms = DurationMs,
        timestamp = erlang:system_time(millisecond)
    },

    %% Update active backpressure tracking
    NewActiveBackpressure = case EventType of
        activated ->
            maps:put(TransportId, erlang:monotonic_time(millisecond), State#state.active_backpressure);
        deactivated ->
            maps:remove(TransportId, State#state.active_backpressure);
        timeout ->
            maps:remove(TransportId, State#state.active_backpressure)
    end,

    NewEvents = [Event | lists:sublist(State#state.backpressure_events, 999)],
    {noreply, State#state{backpressure_events = NewEvents, active_backpressure = NewActiveBackpressure}};
handle_cast({record_stats, TransportId, Stats}, State) ->
    SocketStats = #socket_stats{
        transport_id = TransportId,
        bytes_sent = maps:get(bytes_sent, Stats, 0),
        bytes_received = maps:get(bytes_received, Stats, 0),
        packets_sent = maps:get(packets_sent, Stats, 0),
        packets_received = maps:get(packets_received, Stats, 0),
        read_count = maps:get(read_count, Stats, 0),
        write_count = maps:get(write_count, Stats, 0),
        timestamp = erlang:system_time(millisecond)
    },

    NewStats = [SocketStats | lists:sublist(State#state.socket_stats, 999)],
    {noreply, State#state{socket_stats = NewStats}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Socket metrics collector terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Filter metrics by transport ID
-spec filter_by_transport(list(), transport_id()) -> list().
filter_by_transport(Metrics, TransportId) ->
    [M || M <- Metrics,
          case M of
              #buffer_metrics{transport_id = Tid} -> Tid =:= TransportId;
              #packet_drops{transport_id = Tid} -> Tid =:= TransportId;
              #backpressure_event{transport_id = Tid} -> Tid =:= TransportId;
              #socket_stats{transport_id = Tid} -> Tid =:= TransportId;
              _ -> false
          end].

%% @doc Calculate buffer usage summary
-spec calculate_buffer_summary(state()) -> map().
calculate_buffer_summary(#state{buffer_metrics = Metrics}) ->
    case Metrics of
        [] ->
            #{
                total_transports => 0,
                avg_rcvbuf_utilization => 0.0,
                avg_sndbuf_utilization => 0.0,
                max_rcvbuf_utilization => 0.0,
                max_sndbuf_utilization => 0.0,
                transports_over_threshold => []
            };
        _ ->
            RcvUtils = [M#buffer_metrics.rcvbuf_utilization || M <- Metrics],
            SndUtils = [M#buffer_metrics.sndbuf_utilization || M <- Metrics],

            AvgRcv = lists:sum(RcvUtils) / length(RcvUtils),
            AvgSnd = lists:sum(SndUtils) / length(SndUtils),

            %% Find transports over 80% threshold
            OverThreshold = [M#buffer_metrics.transport_id ||
                              M <- Metrics,
                              M#buffer_metrics.rcvbuf_utilization > 80.0 orelse
                              M#buffer_metrics.sndbuf_utilization > 80.0],

            #{
                total_transports => length(Metrics),
                avg_rcvbuf_utilization => AvgRcv,
                avg_sndbuf_utilization => AvgSnd,
                max_rcvbuf_utilization => lists:max(RcvUtils),
                max_sndbuf_utilization => lists:max(SndUtils),
                transports_over_threshold => lists:usort(OverThreshold)
            }
    end.
