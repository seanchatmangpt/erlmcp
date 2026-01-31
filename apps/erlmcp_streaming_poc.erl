%%%-------------------------------------------------------------------
%%% @doc Streaming tool execution POC for progressive result delivery
%%%
%%% Proof of concept implementation for streaming tool results with:
%%% - Multiple subscribers per execution
%%% - Progressive chunk delivery
%%% - Backpressure handling (drop strategy)
%%% - Completion signaling
%%% - Automatic cleanup on subscriber death
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_streaming_poc).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    start_link/1,
    stop/1,
    execute_tool/3,
    subscribe/2,
    unsubscribe/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    streams = #{} :: #{execution_id() => stream()},
    execution_counter = 0 :: non_neg_integer()
}).

-record(stream, {
    id :: execution_id(),
    tool_name :: binary(),
    chunks :: pos_integer(),
    delay_ms :: pos_integer(),
    chunk_size :: pos_integer(),
    position = 0 :: non_neg_integer(),
    subscribers = [] :: [pid()],
    monitors = #{} :: #{pid() => reference()},
    backpressure_strategy = drop :: drop | block
}).

-type execution_id() :: reference().
-type stream() :: #stream{}.
-type backpressure_strategy() :: drop | block.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link(?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Execute a tool and create a streaming execution
-spec execute_tool(pid(), binary(), map()) -> {ok, execution_id()}.
execute_tool(Pid, ToolName, Opts) ->
    gen_server:call(Pid, {execute_tool, ToolName, Opts}).

%% @doc Subscribe to a streaming execution
-spec subscribe(pid(), execution_id()) -> ok.
subscribe(Pid, ExecutionId) ->
    gen_server:call(Pid, {subscribe, ExecutionId, self()}).

%% @doc Unsubscribe from a streaming execution
-spec unsubscribe(pid(), execution_id()) -> ok.
unsubscribe(Pid, ExecutionId) ->
    gen_server:call(Pid, {unsubscribe, ExecutionId, self()}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Opts) ->
    {ok, #state{}}.

handle_call({execute_tool, ToolName, Opts}, _From, State = #state{streams = Streams, execution_counter = Counter}) ->
    %% Create unique execution ID
    ExecutionId = make_ref(),

    %% Extract options
    Chunks = maps:get(chunks, Opts, 10),
    DelayMs = maps:get(delay_ms, Opts, 10),
    ChunkSize = maps:get(chunk_size, Opts, 10),

    %% Create stream
    Stream = #stream{
        id = ExecutionId,
        tool_name = ToolName,
        chunks = Chunks,
        delay_ms = DelayMs,
        chunk_size = ChunkSize
    },

    %% Start streaming asynchronously
    self() ! {start_streaming, ExecutionId},

    NewStreams = maps:put(ExecutionId, Stream, Streams),
    {reply, {ok, ExecutionId}, State#state{streams = NewStreams, execution_counter = Counter + 1}};

handle_call({subscribe, ExecutionId, SubscriberPid}, _From, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{subscribers = Subs, monitors = Mons}} ->
            case lists:member(SubscriberPid, Subs) of
                true ->
                    {reply, ok, State};
                false ->
                    %% Monitor the subscriber
                    Ref = monitor(process, SubscriberPid),
                    NewSubs = [SubscriberPid | Subs],
                    NewMons = maps:put(SubscriberPid, Ref, Mons),
                    NewStream = Stream#stream{subscribers = NewSubs, monitors = NewMons},
                    NewStreams = maps:put(ExecutionId, NewStream, Streams),
                    {reply, ok, State#state{streams = NewStreams}}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({unsubscribe, ExecutionId, SubscriberPid}, _From, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{subscribers = Subs, monitors = Mons}} ->
            NewSubs = lists:delete(SubscriberPid, Subs),
            NewMons = case maps:find(SubscriberPid, Mons) of
                {ok, Ref} ->
                    demonitor(Ref, [flush]),
                    maps:remove(SubscriberPid, Mons);
                error ->
                    Mons
            end,
            NewStream = Stream#stream{subscribers = NewSubs, monitors = NewMons},
            NewStreams = maps:put(ExecutionId, NewStream, Streams),
            {reply, ok, State#state{streams = NewStreams}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({start_streaming, ExecutionId}, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{chunks = Chunks, delay_ms = DelayMs}} ->
            %% Send first chunk
            self() ! {send_chunk, ExecutionId, 1},
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_info({send_chunk, ExecutionId, ChunkNum}, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{
            id = Id,
            chunks = TotalChunks,
            delay_ms = DelayMs,
            chunk_size = ChunkSize,
            subscribers = Subs,
            backpressure_strategy = Strategy
        }} ->
            %% Generate chunk data
            ChunkData = #{
                chunk_num => ChunkNum,
                data => generate_chunk_data(ChunkSize, ChunkNum),
                timestamp => erlang:monotonic_time(microsecond)
            },

            %% Send to all subscribers (with backpressure handling)
            SendTime = erlang:monotonic_time(microsecond),
            lists:foreach(
                fun(Sub) ->
                    case Strategy of
                        drop ->
                            %% Non-blocking send - drop if mailbox full
                            try
                                Sub ! {stream_chunk, Id, ChunkData, SendTime}
                            catch
                                _:_ ->
                                    drop
                            end;
                        block ->
                            Sub ! {stream_chunk, Id, ChunkData, SendTime}
                    end
                end,
                Subs
            ),

            %% Schedule next chunk or complete
            case ChunkNum >= TotalChunks of
                true ->
                    %% Send completion signal
                    lists:foreach(
                        fun(Sub) ->
                            Sub ! {stream_complete, Id}
                        end,
                        Subs
                    ),
                    %% Remove stream
                    NewStreams = maps:remove(ExecutionId, Streams),
                    {noreply, State#state{streams = NewStreams}};
                false ->
                    %% Schedule next chunk
                    erlang:send_after(DelayMs, self(), {send_chunk, ExecutionId, ChunkNum + 1}),
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{streams = Streams}) ->
    %% Remove dead subscriber from all streams
    NewStreams = maps:map(
        fun(_ExecId, Stream = #stream{subscribers = Subs, monitors = Mons}) ->
            case lists:member(Pid, Subs) of
                true ->
                    NewSubs = lists:delete(Pid, Subs),
                    NewMons = maps:remove(Pid, Mons),
                    Stream#stream{subscribers = NewSubs, monitors = NewMons};
                false ->
                    Stream
            end
        end,
        Streams
    ),

    %% Remove streams with no subscribers
    CleanedStreams = maps:filter(
        fun(_ExecId, #stream{subscribers = Subs}) ->
            Subs =/= []
        end,
        NewStreams
    ),

    {noreply, State#state{streams = CleanedStreams}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate dummy chunk data for testing
-spec generate_chunk_data(pos_integer(), pos_integer()) -> binary().
generate_chunk_data(ChunkSize, ChunkNum) ->
    Pattern = <<"chunk-", (integer_to_binary(ChunkNum))/binary, "-">>,
    RepeatCount = max(1, ChunkSize div byte_size(Pattern)),
    Remainder = ChunkSize rem byte_size(Pattern),
    Base = binary:copy(Pattern, RepeatCount),
    if
        Remainder > 0 ->
            <<Base/binary, (binary:part(Pattern, 0, Remainder))/binary>>;
        true ->
            Base
    end.
