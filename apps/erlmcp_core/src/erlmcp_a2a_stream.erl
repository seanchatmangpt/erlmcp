%%%-------------------------------------------------------------------
%%% @doc A2A Streaming Module
%%%
%%% This module handles A2A protocol streaming responses for task updates
%%% and artifact delivery. It provides a gen_server-based stream manager
%%% that supports:
%%%
%%% - Task status update streaming (TaskStatusUpdateEvent)
%%% - Artifact streaming with chunking (TaskArtifactUpdateEvent)
%%% - Per-task subscriber management
%%% - Backpressure handling with configurable limits
%%% - Stream cancellation
%%% - SSE (Server-Sent Events) format support
%%% - WebSocket stream support
%%%
%%% Streaming Protocol:
%%% The A2A protocol uses streaming for real-time task updates. Each
%%% stream is associated with a task_id and can have multiple subscribers.
%%% Events are delivered in order to all subscribers.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_stream).

-behaviour(gen_server).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    %% Lifecycle
    start_link/0,
    start_link/1,
    stop/0,
    stop/1,

    %% Stream management
    start_stream/2,
    close_stream/1,
    close_stream/2,

    %% Subscription
    subscribe/2,
    subscribe/3,
    subscribe/4,
    unsubscribe/2,
    unsubscribe/3,

    %% Event sending
    send_status_update/2,
    send_status_update/3,
    send_artifact_update/3,
    send_artifact_update/4,

    %% Query
    is_streaming/1,
    is_streaming/2,
    get_subscribers/1,
    get_subscribers/2,
    get_stream_info/1,
    get_stream_info/2,

    %% SSE/WebSocket format helpers
    format_sse_event/2,
    format_ws_message/2
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

%%====================================================================
%% Type Definitions
%%====================================================================

-type task_id() :: binary().
-type context_id() :: binary().
-type transport_type() :: sse | websocket | raw.

-record(subscriber, {
    pid :: pid(),
    monitor_ref :: reference(),
    transport :: transport_type(),
    metadata :: map(),
    subscribed_at :: integer(),
    %% Backpressure tracking
    pending_count :: non_neg_integer(),
    max_pending :: non_neg_integer(),
    paused :: boolean()
}).

-record(stream, {
    task_id :: task_id(),
    context_id :: context_id(),
    subscribers :: #{pid() => #subscriber{}},
    %% Artifact accumulator for chunked delivery
    artifacts :: #{binary() => #a2a_artifact{}},
    %% Stream metadata
    created_at :: integer(),
    last_event_at :: integer() | undefined,
    event_count :: non_neg_integer(),
    %% Backpressure settings
    max_subscribers :: pos_integer(),
    max_pending_per_subscriber :: pos_integer()
}).

-record(state, {
    streams :: #{task_id() => #stream{}},
    %% Global settings
    default_max_subscribers :: pos_integer(),
    default_max_pending :: pos_integer(),
    %% Metrics
    total_events_sent :: non_neg_integer(),
    total_streams_created :: non_neg_integer()
}).

-type stream() :: #stream{}.
-type state() :: #state{}.

%% Default configuration
-define(DEFAULT_MAX_SUBSCRIBERS, 100).
-define(DEFAULT_MAX_PENDING, 1000).
-define(SSE_CONTENT_TYPE, <<"text/event-stream">>).
-define(SSE_RETRY_MS, 3000).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the stream manager with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the stream manager with options
-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Stop the default stream manager
-spec stop() -> ok.
stop() ->
    stop(?MODULE).

%% @doc Stop a specific stream manager
-spec stop(pid() | atom()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% @doc Start streaming for a task
%% Creates a new stream for the given task_id and context_id.
%% Returns error if stream already exists for this task.
-spec start_stream(task_id(), context_id()) -> ok | {error, term()}.
start_stream(TaskId, ContextId) ->
    gen_server:call(?MODULE, {start_stream, TaskId, ContextId, #{}}).

%% @doc Subscribe to task updates
%% The subscriber will receive messages in the format:
%% - {a2a_status_update, TaskId, #a2a_task_status_update_event{}}
%% - {a2a_artifact_update, TaskId, #a2a_task_artifact_update_event{}}
%% - {a2a_stream_closed, TaskId, Reason}
-spec subscribe(task_id(), pid()) -> ok | {error, term()}.
subscribe(TaskId, Subscriber) ->
    subscribe(?MODULE, TaskId, Subscriber).

%% @doc Subscribe with server reference
-spec subscribe(pid() | atom(), task_id(), pid()) -> ok | {error, term()}.
subscribe(Server, TaskId, Subscriber) ->
    subscribe(Server, TaskId, Subscriber, #{}).

%% @doc Subscribe with options
-spec subscribe(pid() | atom(), task_id(), pid(), map()) -> ok | {error, term()}.
subscribe(Server, TaskId, Subscriber, Opts) when is_pid(Subscriber), is_map(Opts) ->
    gen_server:call(Server, {subscribe, TaskId, Subscriber, Opts}).

%% @doc Unsubscribe from task updates
-spec unsubscribe(task_id(), pid()) -> ok | {error, term()}.
unsubscribe(TaskId, Subscriber) ->
    unsubscribe(?MODULE, TaskId, Subscriber).

%% @doc Unsubscribe with server reference
-spec unsubscribe(pid() | atom(), task_id(), pid()) -> ok | {error, term()}.
unsubscribe(Server, TaskId, Subscriber) when is_pid(Subscriber) ->
    gen_server:call(Server, {unsubscribe, TaskId, Subscriber}).

%% @doc Send a task status update event to all subscribers
-spec send_status_update(task_id(), #a2a_task_status_update_event{}) -> ok | {error, term()}.
send_status_update(TaskId, Event) ->
    send_status_update(?MODULE, TaskId, Event).

%% @doc Send status update with server reference
-spec send_status_update(pid() | atom(), task_id(), #a2a_task_status_update_event{}) ->
    ok | {error, term()}.
send_status_update(Server, TaskId, #a2a_task_status_update_event{} = Event) ->
    gen_server:cast(Server, {send_status_update, TaskId, Event}).

%% @doc Send an artifact update event with append/last_chunk flags
-spec send_artifact_update(task_id(), #a2a_artifact{}, map()) -> ok | {error, term()}.
send_artifact_update(TaskId, Artifact, Opts) ->
    send_artifact_update(?MODULE, TaskId, Artifact, Opts).

%% @doc Send artifact update with server reference
%% Opts should contain:
%% - append :: boolean() - whether to append to existing artifact
%% - last_chunk :: boolean() - whether this is the final chunk
-spec send_artifact_update(pid() | atom(), task_id(), #a2a_artifact{}, map()) ->
    ok | {error, term()}.
send_artifact_update(Server, TaskId, #a2a_artifact{} = Artifact, Opts) when is_map(Opts) ->
    Append = maps:get(append, Opts, false),
    LastChunk = maps:get(last_chunk, Opts, false),
    gen_server:cast(Server, {send_artifact_update, TaskId, Artifact, Append, LastChunk}).

%% @doc Close a stream gracefully
-spec close_stream(task_id()) -> ok | {error, term()}.
close_stream(TaskId) ->
    close_stream(?MODULE, TaskId).

%% @doc Close stream with server reference
-spec close_stream(pid() | atom(), task_id()) -> ok | {error, term()}.
close_stream(Server, TaskId) ->
    gen_server:call(Server, {close_stream, TaskId}).

%% @doc Check if a task is being streamed
-spec is_streaming(task_id()) -> boolean().
is_streaming(TaskId) ->
    is_streaming(?MODULE, TaskId).

%% @doc Check if streaming with server reference
-spec is_streaming(pid() | atom(), task_id()) -> boolean().
is_streaming(Server, TaskId) ->
    gen_server:call(Server, {is_streaming, TaskId}).

%% @doc Get subscribers for a task
-spec get_subscribers(task_id()) -> {ok, [pid()]} | {error, not_found}.
get_subscribers(TaskId) ->
    get_subscribers(?MODULE, TaskId).

%% @doc Get subscribers with server reference
-spec get_subscribers(pid() | atom(), task_id()) -> {ok, [pid()]} | {error, not_found}.
get_subscribers(Server, TaskId) ->
    gen_server:call(Server, {get_subscribers, TaskId}).

%% @doc Get detailed stream information
-spec get_stream_info(task_id()) -> {ok, map()} | {error, not_found}.
get_stream_info(TaskId) ->
    get_stream_info(?MODULE, TaskId).

%% @doc Get stream info with server reference
-spec get_stream_info(pid() | atom(), task_id()) -> {ok, map()} | {error, not_found}.
get_stream_info(Server, TaskId) ->
    gen_server:call(Server, {get_stream_info, TaskId}).

%%====================================================================
%% SSE/WebSocket Format Helpers
%%====================================================================

%% @doc Format an event for Server-Sent Events (SSE) delivery
%% Returns a binary suitable for SSE streaming:
%% event: <type>
%% id: <event_id>
%% retry: <retry_ms>
%% data: <json_data>
%%
-spec format_sse_event(status_update | artifact_update, term()) -> binary().
format_sse_event(status_update, #a2a_task_status_update_event{} = Event) ->
    JsonMap = erlmcp_a2a_protocol:encode_task_status_update_event(Event),
    JsonBin = erlmcp_json:encode(JsonMap),
    EventId = generate_event_id(),
    format_sse_lines(<<"a2a.taskStatusUpdate">>, EventId, JsonBin);

format_sse_event(artifact_update, #a2a_task_artifact_update_event{} = Event) ->
    JsonMap = erlmcp_a2a_protocol:encode_task_artifact_update_event(Event),
    JsonBin = erlmcp_json:encode(JsonMap),
    EventId = generate_event_id(),
    format_sse_lines(<<"a2a.taskArtifactUpdate">>, EventId, JsonBin).

%% @doc Format an event for WebSocket delivery
%% Returns a map suitable for JSON encoding and WebSocket transmission
-spec format_ws_message(status_update | artifact_update, term()) -> map().
format_ws_message(status_update, #a2a_task_status_update_event{} = Event) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => ?A2A_NOTIFICATION_TASK_STATUS_UPDATE,
        <<"params">> => erlmcp_a2a_protocol:encode_task_status_update_event(Event)
    };

format_ws_message(artifact_update, #a2a_task_artifact_update_event{} = Event) ->
    #{
        <<"jsonrpc">> => ?JSONRPC_VERSION,
        <<"method">> => ?A2A_NOTIFICATION_TASK_ARTIFACT_UPDATE,
        <<"params">> => erlmcp_a2a_protocol:encode_task_artifact_update_event(Event)
    }.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init(Opts) ->
    MaxSubscribers = proplists:get_value(max_subscribers, Opts, ?DEFAULT_MAX_SUBSCRIBERS),
    MaxPending = proplists:get_value(max_pending, Opts, ?DEFAULT_MAX_PENDING),

    State = #state{
        streams = #{},
        default_max_subscribers = MaxSubscribers,
        default_max_pending = MaxPending,
        total_events_sent = 0,
        total_streams_created = 0
    },
    {ok, State}.

handle_call({start_stream, TaskId, ContextId, _Opts}, _From,
            State = #state{streams = Streams}) ->
    case maps:is_key(TaskId, Streams) of
        true ->
            {reply, {error, {stream_already_exists, TaskId}}, State};
        false ->
            Now = erlang:system_time(millisecond),
            Stream = #stream{
                task_id = TaskId,
                context_id = ContextId,
                subscribers = #{},
                artifacts = #{},
                created_at = Now,
                last_event_at = undefined,
                event_count = 0,
                max_subscribers = State#state.default_max_subscribers,
                max_pending_per_subscriber = State#state.default_max_pending
            },
            NewStreams = maps:put(TaskId, Stream, Streams),
            NewState = State#state{
                streams = NewStreams,
                total_streams_created = State#state.total_streams_created + 1
            },
            {reply, ok, NewState}
    end;

handle_call({subscribe, TaskId, Pid, Opts}, _From,
            State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream = #stream{subscribers = Subs}} ->
            case maps:size(Subs) >= Stream#stream.max_subscribers of
                true ->
                    {reply, {error, max_subscribers_reached}, State};
                false ->
                    case maps:is_key(Pid, Subs) of
                        true ->
                            {reply, {error, already_subscribed}, State};
                        false ->
                            MonRef = monitor(process, Pid),
                            Transport = maps:get(transport, Opts, raw),
                            Subscriber = #subscriber{
                                pid = Pid,
                                monitor_ref = MonRef,
                                transport = Transport,
                                metadata = maps:get(metadata, Opts, #{}),
                                subscribed_at = erlang:system_time(millisecond),
                                pending_count = 0,
                                max_pending = Stream#stream.max_pending_per_subscriber,
                                paused = false
                            },
                            NewSubs = maps:put(Pid, Subscriber, Subs),
                            NewStream = Stream#stream{subscribers = NewSubs},
                            NewStreams = maps:put(TaskId, NewStream, Streams),
                            {reply, ok, State#state{streams = NewStreams}}
                    end
            end;
        error ->
            {reply, {error, stream_not_found}, State}
    end;

handle_call({unsubscribe, TaskId, Pid}, _From,
            State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream = #stream{subscribers = Subs}} ->
            case maps:find(Pid, Subs) of
                {ok, #subscriber{monitor_ref = MonRef}} ->
                    demonitor(MonRef, [flush]),
                    NewSubs = maps:remove(Pid, Subs),
                    NewStream = Stream#stream{subscribers = NewSubs},
                    NewStreams = maps:put(TaskId, NewStream, Streams),
                    {reply, ok, State#state{streams = NewStreams}};
                error ->
                    {reply, {error, not_subscribed}, State}
            end;
        error ->
            {reply, {error, stream_not_found}, State}
    end;

handle_call({close_stream, TaskId}, _From,
            State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream} ->
            %% Notify all subscribers of stream closure
            notify_subscribers(Stream, {a2a_stream_closed, TaskId, normal}),
            %% Cleanup monitors
            cleanup_stream_monitors(Stream),
            NewStreams = maps:remove(TaskId, Streams),
            {reply, ok, State#state{streams = NewStreams}};
        error ->
            {reply, {error, stream_not_found}, State}
    end;

handle_call({is_streaming, TaskId}, _From,
            State = #state{streams = Streams}) ->
    {reply, maps:is_key(TaskId, Streams), State};

handle_call({get_subscribers, TaskId}, _From,
            State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, #stream{subscribers = Subs}} ->
            Pids = maps:keys(Subs),
            {reply, {ok, Pids}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_stream_info, TaskId}, _From,
            State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream} ->
            Info = #{
                task_id => Stream#stream.task_id,
                context_id => Stream#stream.context_id,
                subscriber_count => maps:size(Stream#stream.subscribers),
                artifact_count => maps:size(Stream#stream.artifacts),
                created_at => Stream#stream.created_at,
                last_event_at => Stream#stream.last_event_at,
                event_count => Stream#stream.event_count
            },
            {reply, {ok, Info}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_status_update, TaskId, Event}, State) ->
    NewState = do_send_status_update(TaskId, Event, State),
    {noreply, NewState};

handle_cast({send_artifact_update, TaskId, Artifact, Append, LastChunk}, State) ->
    NewState = do_send_artifact_update(TaskId, Artifact, Append, LastChunk, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonRef, process, Pid, _Reason},
            State = #state{streams = Streams}) ->
    %% Remove dead subscriber from all streams
    NewStreams = maps:map(
        fun(_TaskId, Stream = #stream{subscribers = Subs}) ->
            case maps:find(Pid, Subs) of
                {ok, #subscriber{monitor_ref = MonRef}} ->
                    Stream#stream{subscribers = maps:remove(Pid, Subs)};
                _ ->
                    Stream
            end
        end,
        Streams
    ),
    {noreply, State#state{streams = NewStreams}};

handle_info({backpressure_resume, TaskId, Pid}, State = #state{streams = Streams}) ->
    %% Resume a paused subscriber
    case maps:find(TaskId, Streams) of
        {ok, Stream = #stream{subscribers = Subs}} ->
            case maps:find(Pid, Subs) of
                {ok, Sub} ->
                    NewSub = Sub#subscriber{paused = false, pending_count = 0},
                    NewSubs = maps:put(Pid, NewSub, Subs),
                    NewStream = Stream#stream{subscribers = NewSubs},
                    NewStreams = maps:put(TaskId, NewStream, Streams),
                    {noreply, State#state{streams = NewStreams}};
                error ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{streams = Streams}) ->
    %% Notify all subscribers that streams are closing
    maps:foreach(
        fun(TaskId, Stream) ->
            notify_subscribers(Stream, {a2a_stream_closed, TaskId, shutdown}),
            cleanup_stream_monitors(Stream)
        end,
        Streams
    ),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Send status update to all subscribers
-spec do_send_status_update(task_id(), #a2a_task_status_update_event{}, state()) -> state().
do_send_status_update(TaskId, Event, State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream} ->
            Now = erlang:system_time(millisecond),
            Message = {a2a_status_update, TaskId, Event},

            %% Send to all non-paused subscribers with backpressure tracking
            {NewSubs, SentCount} = maps:fold(
                fun(Pid, Sub, {AccSubs, AccCount}) ->
                    case should_send(Sub) of
                        true ->
                            send_to_subscriber(Pid, Sub, Message),
                            NewSub = increment_pending(Sub),
                            {maps:put(Pid, NewSub, AccSubs), AccCount + 1};
                        false ->
                            {maps:put(Pid, Sub, AccSubs), AccCount}
                    end
                end,
                {#{}, 0},
                Stream#stream.subscribers
            ),

            NewStream = Stream#stream{
                subscribers = NewSubs,
                last_event_at = Now,
                event_count = Stream#stream.event_count + 1
            },
            NewStreams = maps:put(TaskId, NewStream, Streams),

            State#state{
                streams = NewStreams,
                total_events_sent = State#state.total_events_sent + SentCount
            };
        error ->
            State
    end.

%% @private Send artifact update to all subscribers
-spec do_send_artifact_update(task_id(), #a2a_artifact{}, boolean(), boolean(), state()) -> state().
do_send_artifact_update(TaskId, Artifact, Append, LastChunk, State = #state{streams = Streams}) ->
    case maps:find(TaskId, Streams) of
        {ok, Stream} ->
            Now = erlang:system_time(millisecond),

            %% Build the event
            Event = #a2a_task_artifact_update_event{
                task_id = TaskId,
                context_id = Stream#stream.context_id,
                artifact = Artifact,
                append = Append,
                last_chunk = LastChunk,
                metadata = undefined
            },

            Message = {a2a_artifact_update, TaskId, Event},

            %% Send to all non-paused subscribers
            {NewSubs, SentCount} = maps:fold(
                fun(Pid, Sub, {AccSubs, AccCount}) ->
                    case should_send(Sub) of
                        true ->
                            send_to_subscriber(Pid, Sub, Message),
                            NewSub = increment_pending(Sub),
                            {maps:put(Pid, NewSub, AccSubs), AccCount + 1};
                        false ->
                            {maps:put(Pid, Sub, AccSubs), AccCount}
                    end
                end,
                {#{}, 0},
                Stream#stream.subscribers
            ),

            %% Track artifact accumulation
            ArtifactId = Artifact#a2a_artifact.artifact_id,
            NewArtifacts = case {Append, LastChunk} of
                {false, _} ->
                    %% New artifact, replace any existing
                    maps:put(ArtifactId, Artifact, Stream#stream.artifacts);
                {true, false} ->
                    %% Append chunk, merge with existing
                    case maps:find(ArtifactId, Stream#stream.artifacts) of
                        {ok, Existing} ->
                            Merged = merge_artifact_parts(Existing, Artifact),
                            maps:put(ArtifactId, Merged, Stream#stream.artifacts);
                        error ->
                            maps:put(ArtifactId, Artifact, Stream#stream.artifacts)
                    end;
                {true, true} ->
                    %% Final chunk, remove from accumulator
                    maps:remove(ArtifactId, Stream#stream.artifacts)
            end,

            NewStream = Stream#stream{
                subscribers = NewSubs,
                artifacts = NewArtifacts,
                last_event_at = Now,
                event_count = Stream#stream.event_count + 1
            },
            NewStreams = maps:put(TaskId, NewStream, Streams),

            State#state{
                streams = NewStreams,
                total_events_sent = State#state.total_events_sent + SentCount
            };
        error ->
            State
    end.

%% @private Check if we should send to this subscriber (backpressure)
-spec should_send(#subscriber{}) -> boolean().
should_send(#subscriber{paused = true}) ->
    false;
should_send(#subscriber{pending_count = Pending, max_pending = Max}) when Pending >= Max ->
    false;
should_send(_) ->
    true.

%% @private Increment pending count and potentially pause
-spec increment_pending(#subscriber{}) -> #subscriber{}.
increment_pending(Sub = #subscriber{pending_count = Pending, max_pending = Max}) ->
    NewPending = Pending + 1,
    Paused = NewPending >= Max,
    Sub#subscriber{pending_count = NewPending, paused = Paused}.

%% @private Send message to a subscriber based on transport type
-spec send_to_subscriber(pid(), #subscriber{}, term()) -> ok.
send_to_subscriber(Pid, #subscriber{transport = sse}, {a2a_status_update, _, Event}) ->
    SSEData = format_sse_event(status_update, Event),
    Pid ! {a2a_sse_data, SSEData},
    ok;
send_to_subscriber(Pid, #subscriber{transport = sse}, {a2a_artifact_update, _, Event}) ->
    SSEData = format_sse_event(artifact_update, Event),
    Pid ! {a2a_sse_data, SSEData},
    ok;
send_to_subscriber(Pid, #subscriber{transport = websocket}, {a2a_status_update, TaskId, Event}) ->
    WSMsg = format_ws_message(status_update, Event),
    Pid ! {a2a_ws_message, TaskId, WSMsg},
    ok;
send_to_subscriber(Pid, #subscriber{transport = websocket}, {a2a_artifact_update, TaskId, Event}) ->
    WSMsg = format_ws_message(artifact_update, Event),
    Pid ! {a2a_ws_message, TaskId, WSMsg},
    ok;
send_to_subscriber(Pid, #subscriber{transport = raw}, Message) ->
    Pid ! Message,
    ok;
send_to_subscriber(Pid, _, Message) ->
    %% Default to raw
    Pid ! Message,
    ok.

%% @private Notify all subscribers of an event
-spec notify_subscribers(#stream{}, term()) -> ok.
notify_subscribers(#stream{subscribers = Subs}, Message) ->
    maps:foreach(
        fun(Pid, _Sub) ->
            Pid ! Message
        end,
        Subs
    ),
    ok.

%% @private Cleanup all monitors for a stream
-spec cleanup_stream_monitors(#stream{}) -> ok.
cleanup_stream_monitors(#stream{subscribers = Subs}) ->
    maps:foreach(
        fun(_Pid, #subscriber{monitor_ref = MonRef}) ->
            demonitor(MonRef, [flush])
        end,
        Subs
    ),
    ok.

%% @private Merge artifact parts for chunked delivery
-spec merge_artifact_parts(#a2a_artifact{}, #a2a_artifact{}) -> #a2a_artifact{}.
merge_artifact_parts(Existing, New) ->
    %% Append new parts to existing parts
    MergedParts = Existing#a2a_artifact.parts ++ New#a2a_artifact.parts,
    Existing#a2a_artifact{parts = MergedParts}.

%% @private Generate a unique event ID for SSE
-spec generate_event_id() -> binary().
generate_event_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("~.16B-~.4.0B", [Timestamp, Random])).

%% @private Format SSE event lines
-spec format_sse_lines(binary(), binary(), binary()) -> binary().
format_sse_lines(EventType, EventId, JsonData) ->
    %% Split data by newlines and prefix each line with "data: "
    DataLines = format_sse_data_lines(JsonData),
    iolist_to_binary([
        <<"event: ">>, EventType, <<"\n">>,
        <<"id: ">>, EventId, <<"\n">>,
        <<"retry: ">>, integer_to_binary(?SSE_RETRY_MS), <<"\n">>,
        DataLines,
        <<"\n">>  % Empty line to signal end of event
    ]).

%% @private Format data lines for SSE (handle multiline JSON)
-spec format_sse_data_lines(binary()) -> iolist().
format_sse_data_lines(JsonData) ->
    %% For simplicity, assume JSON is on a single line
    %% In production, you might want to split by \n
    [<<"data: ">>, JsonData, <<"\n">>].
