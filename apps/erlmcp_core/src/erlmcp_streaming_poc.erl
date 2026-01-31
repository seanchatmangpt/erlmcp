-module(erlmcp_streaming_poc).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    start_stream/3,
    get_chunk/2,
    stream_complete/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    streams = #{} :: #{stream_id() => stream()}
}).

-record(stream, {
    id :: stream_id(),
    data :: binary(),
    chunk_size :: pos_integer(),
    position = 0 :: non_neg_integer(),
    subscriber :: pid()
}).

-type stream_id() :: term().

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec start_stream(pid(), stream_id(), #{data := binary(), chunk_size := pos_integer(), subscriber := pid()}) -> ok.
start_stream(Pid, StreamId, Opts) ->
    gen_server:call(Pid, {start_stream, StreamId, Opts}).

-spec get_chunk(pid(), stream_id()) -> {ok, binary()} | {error, stream_complete} | {error, not_found}.
get_chunk(Pid, StreamId) ->
    gen_server:call(Pid, {get_chunk, StreamId}).

-spec stream_complete(pid(), stream_id()) -> boolean().
stream_complete(Pid, StreamId) ->
    gen_server:call(Pid, {stream_complete, StreamId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Opts) ->
    {ok, #state{}}.

handle_call({start_stream, StreamId, Opts}, _From, State = #state{streams = Streams}) ->
    Data = maps:get(data, Opts),
    ChunkSize = maps:get(chunk_size, Opts),
    Subscriber = maps:get(subscriber, Opts),

    Stream = #stream{
        id = StreamId,
        data = Data,
        chunk_size = ChunkSize,
        subscriber = Subscriber
    },

    monitor(process, Subscriber),
    NewStreams = maps:put(StreamId, Stream, Streams),

    %% Start sending chunks asynchronously
    gen_server:cast(self(), {send_next_chunk, StreamId}),

    {reply, ok, State#state{streams = NewStreams}};

handle_call({get_chunk, StreamId}, _From, State = #state{streams = Streams}) ->
    case maps:find(StreamId, Streams) of
        {ok, Stream = #stream{data = Data, chunk_size = ChunkSize, position = Pos}} ->
            DataSize = byte_size(Data),
            case Pos >= DataSize of
                true ->
                    {reply, {error, stream_complete}, State};
                false ->
                    RemainingSize = DataSize - Pos,
                    ActualChunkSize = min(ChunkSize, RemainingSize),
                    Chunk = binary:part(Data, Pos, ActualChunkSize),
                    NewPos = Pos + ActualChunkSize,
                    NewStream = Stream#stream{position = NewPos},
                    NewStreams = maps:put(StreamId, NewStream, Streams),
                    {reply, {ok, Chunk}, State#state{streams = NewStreams}}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({stream_complete, StreamId}, _From, State = #state{streams = Streams}) ->
    case maps:find(StreamId, Streams) of
        {ok, #stream{data = Data, position = Pos}} ->
            Complete = Pos >= byte_size(Data),
            {reply, Complete, State};
        error ->
            {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_next_chunk, StreamId}, State = #state{streams = Streams}) ->
    case maps:find(StreamId, Streams) of
        {ok, Stream = #stream{data = Data, chunk_size = ChunkSize, position = Pos, subscriber = Sub}} ->
            DataSize = byte_size(Data),
            case Pos >= DataSize of
                true ->
                    %% Stream complete
                    Sub ! {stream_complete, StreamId},
                    NewStreams = maps:remove(StreamId, Streams),
                    {noreply, State#state{streams = NewStreams}};
                false ->
                    %% Send next chunk
                    RemainingSize = DataSize - Pos,
                    ActualChunkSize = min(ChunkSize, RemainingSize),
                    Chunk = binary:part(Data, Pos, ActualChunkSize),
                    Sub ! {stream_chunk, StreamId, Chunk},

                    NewPos = Pos + ActualChunkSize,
                    NewStream = Stream#stream{position = NewPos},
                    NewStreams = maps:put(StreamId, NewStream, Streams),

                    %% Schedule next chunk
                    gen_server:cast(self(), {send_next_chunk, StreamId}),
                    {noreply, State#state{streams = NewStreams}}
            end;
        error ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{streams = Streams}) ->
    %% Remove streams for dead subscriber
    NewStreams = maps:filter(fun(_Id, #stream{subscriber = Sub}) -> Sub =/= Pid end, Streams),
    {noreply, State#state{streams = NewStreams}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
