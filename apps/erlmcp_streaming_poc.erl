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
    subscribers :: sets:set(pid()),
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
    ExecutionId = make_ref(),

    Chunks = maps:get(chunks, Opts, 10),
    DelayMs = maps:get(delay_ms, Opts, 10),
    ChunkSize = maps:get(chunk_size, Opts, 10),

    Stream = #stream{
        id = ExecutionId,
        tool_name = ToolName,
        chunks = Chunks,
        delay_ms = DelayMs,
        chunk_size = ChunkSize,
        subscribers = sets:new([{version, 2}])
    },

    self() ! {start_streaming, ExecutionId},

    NewStreams = maps:put(ExecutionId, Stream, Streams),
    {reply, {ok, ExecutionId}, State#state{streams = NewStreams, execution_counter = Counter + 1}};

handle_call({subscribe, ExecutionId, SubscriberPid}, _From, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{subscribers = Subs, monitors = Mons}} ->
            case sets:is_element(SubscriberPid, Subs) of
                true ->
                    {reply, ok, State};
                false ->
                    Ref = monitor(process, SubscriberPid),
                    NewSubs = sets:add_element(SubscriberPid, Subs),
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
            NewSubs = sets:del_element(SubscriberPid, Subs),
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
            ChunkData = #{
                chunk_num => ChunkNum,
                data => generate_chunk_data(ChunkSize, ChunkNum),
                timestamp => erlang:monotonic_time(microsecond)
            },

            SendTime = erlang:monotonic_time(microsecond),
            sets:fold(
                fun(Sub, _Acc) ->
                    case Strategy of
                        drop ->
                            try
                                Sub ! {stream_chunk, Id, ChunkData, SendTime}
                            catch
                                _:_ ->
                                    drop
                            end;
                        block ->
                            Sub ! {stream_chunk, Id, ChunkData, SendTime}
                    end,
                    ok
                end,
                ok,
                Subs
            ),

            case ChunkNum >= TotalChunks of
                true ->
                    sets:fold(
                        fun(Sub, _Acc) ->
                            Sub ! {stream_complete, Id},
                            ok
                        end,
                        ok,
                        Subs
                    ),
                    NewStreams = maps:remove(ExecutionId, Streams),
                    {noreply, State#state{streams = NewStreams}};
                false ->
                    erlang:send_after(DelayMs, self(), {send_chunk, ExecutionId, ChunkNum + 1}),
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{streams = Streams}) ->
    NewStreams = maps:map(
        fun(_ExecId, Stream = #stream{subscribers = Subs, monitors = Mons}) ->
            case sets:is_element(Pid, Subs) of
                true ->
                    NewSubs = sets:del_element(Pid, Subs),
                    NewMons = maps:remove(Pid, Mons),
                    Stream#stream{subscribers = NewSubs, monitors = NewMons};
                false ->
                    Stream
            end
        end,
        Streams
    ),

    CleanedStreams = maps:filter(
        fun(_ExecId, #stream{subscribers = Subs}) ->
            sets:size(Subs) > 0
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
