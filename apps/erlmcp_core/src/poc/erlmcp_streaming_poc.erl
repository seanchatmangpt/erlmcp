%%%-------------------------------------------------------------------
%%% @doc Proof of Concept for Streaming Tool Results in erlmcp
%%%
%%% Demonstrates progressive/streaming tool responses similar to LLM token
%%% streaming. Critical for AI applications where tools return large results
%%% progressively.
%%%
%%% Features:
%%% - Progressive/streaming tool responses (like LLM token streaming)
%%% - Multiple subscribers receiving partial results
%%% - Backpressure handling
%%% - Completion signaling
%%% - End-to-end latency measurement
%%%
%%% Example:
%%% ```erlang
%%% erlmcp_streaming_poc:run_demo().
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_streaming_poc).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1, stop/1, subscribe/2, unsubscribe/2, execute_tool/3,
         run_demo/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type tool_name() :: binary().
-type tool_params() :: map().
-type chunk_data() :: binary() | map().
-type execution_id() :: reference().
-type subscriber_pid() :: pid().
%% Backpressure strategy
-type backpressure_strategy() :: drop | buffer | block.

%% Subscriber info
-record(subscriber,
        {pid :: subscriber_pid(),
         monitor_ref :: reference(),
         buffer = [] :: [chunk_data()],
         buffer_size = 0 :: non_neg_integer(),
         max_buffer_size = 100 :: pos_integer(),
         backpressure = drop :: backpressure_strategy(),
         chunks_received = 0 :: non_neg_integer(),
         chunks_dropped = 0 :: non_neg_integer()}).
%% Execution info
-record(execution,
        {id :: execution_id(),
         tool_name :: tool_name(),
         params :: tool_params(),
         subscribers = [] :: [#subscriber{}],
         chunks_sent = 0 :: non_neg_integer(),
         start_time :: integer(),
         completed = false :: boolean()}).
%% State record
-record(state,
        {executions = #{} :: #{execution_id() => #execution{}},
         tools = #{} :: #{tool_name() => fun((tool_params()) -> ok)}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start streaming POC server with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start streaming POC server with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, [Options], []).

%% @doc Stop the streaming POC server
-spec stop(pid()) -> ok.
stop(Server) ->
    gen_server:stop(Server).

%% @doc Subscribe to streaming results for an execution
-spec subscribe(pid(), execution_id()) -> ok | {error, term()}.
subscribe(Server, ExecutionId) ->
    gen_server:call(Server, {subscribe, ExecutionId, self()}).

%% @doc Unsubscribe from streaming results
-spec unsubscribe(pid(), execution_id()) -> ok.
unsubscribe(Server, ExecutionId) ->
    gen_server:call(Server, {unsubscribe, ExecutionId, self()}).

%% @doc Execute a tool with streaming results
-spec execute_tool(pid(), tool_name(), tool_params()) -> {ok, execution_id()} | {error, term()}.
execute_tool(Server, ToolName, Params) ->
    gen_server:call(Server, {execute_tool, ToolName, Params}).

%%====================================================================
%% Demo Function
%%====================================================================

%% @doc Run complete end-to-end streaming demo
%%
%% Demonstrates:
%% - Tool that generates 10 chunks over 2 seconds
%% - 3 subscribers all receiving chunks in real-time
%% - End-to-end latency measurement
%% - Backpressure handling
-spec run_demo() -> ok.
run_demo() ->
    io:format("~n=== erlmcp Streaming Tool Results POC ===~n~n", []),

    %% Start the streaming server
    {ok, Server} = start_link(),
    io:format("[Server] Started streaming POC server: ~p~n", [Server]),

    %% Register the demo tool
    register_demo_tool(Server),

    %% Execute the tool
    {ok, ExecutionId} =
        execute_tool(Server,
                     <<"slow_data_processor">>,
                     #{chunks => 10,
                       delay_ms => 200,
                       chunk_size => 100}),
    io:format("[Execution] Started execution: ~p~n", [ExecutionId]),

    %% Start 3 subscriber processes
    Subscribers =
        [spawn_subscriber(Server, ExecutionId, <<"Subscriber-1">>, fast),
         spawn_subscriber(Server, ExecutionId, <<"Subscriber-2">>, medium),
         spawn_subscriber(Server, ExecutionId, <<"Subscriber-3">>, slow)],
    io:format("[Subscribers] Started 3 subscribers: ~p~n", [Subscribers]),

    %% Wait for completion and collect results
    timer:sleep(3000),

    %% Collect stats from subscribers
    Stats = [collect_subscriber_stats(Sub) || Sub <- Subscribers],

    %% Print results
    io:format("~n=== Results ===~n", []),
    lists:foreach(fun({Name, ChunksReceived, Latencies}) ->
                     AvgLatency =
                         case Latencies of
                             [] ->
                                 0;
                             _ ->
                                 lists:sum(Latencies) div length(Latencies)
                         end,
                     P50 = percentile(Latencies, 0.5),
                     P95 = percentile(Latencies, 0.95),
                     P99 = percentile(Latencies, 0.99),
                     io:format("[~s] Chunks: ~p, Avg Latency: ~p us, P50: ~p us, P95: ~p us, P99: ~p us~n",
                               [Name, ChunksReceived, AvgLatency, P50, P95, P99])
                  end,
                  Stats),

    %% Cleanup
    lists:foreach(fun(Sub) -> Sub ! stop end, Subscribers),
    stop(Server),

    io:format("~n=== Demo Complete ===~n~n", []),
    ok.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([_Options]) ->
    {ok, #state{}}.

handle_call({subscribe, ExecutionId, SubscriberPid}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, {error, execution_not_found}, State};
        Execution ->
            %% Check if already subscribed
            case lists:keyfind(SubscriberPid, #subscriber.pid, Execution#execution.subscribers) of
                false ->
                    %% Add new subscriber
                    MonitorRef = monitor(process, SubscriberPid),
                    Subscriber = #subscriber{pid = SubscriberPid, monitor_ref = MonitorRef},
                    NewSubscribers = [Subscriber | Execution#execution.subscribers],
                    NewExecution = Execution#execution{subscribers = NewSubscribers},
                    NewExecutions = maps:put(ExecutionId, NewExecution, State#state.executions),
                    {reply, ok, State#state{executions = NewExecutions}};
                _ ->
                    {reply, {error, already_subscribed}, State}
            end
    end;
handle_call({unsubscribe, ExecutionId, SubscriberPid}, _From, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {reply, ok, State};
        Execution ->
            case lists:keyfind(SubscriberPid, #subscriber.pid, Execution#execution.subscribers) of
                false ->
                    {reply, ok, State};
                Subscriber ->
                    demonitor(Subscriber#subscriber.monitor_ref, [flush]),
                    NewSubscribers =
                        lists:keydelete(SubscriberPid,
                                        #subscriber.pid,
                                        Execution#execution.subscribers),
                    NewExecution = Execution#execution{subscribers = NewSubscribers},
                    NewExecutions = maps:put(ExecutionId, NewExecution, State#state.executions),
                    {reply, ok, State#state{executions = NewExecutions}}
            end
    end;
handle_call({execute_tool, ToolName, Params}, _From, State) ->
    %% Create execution
    ExecutionId = make_ref(),
    Execution =
        #execution{id = ExecutionId,
                   tool_name = ToolName,
                   params = Params,
                   start_time = erlang:monotonic_time(microsecond)},
    NewExecutions = maps:put(ExecutionId, Execution, State#state.executions),
    NewState = State#state{executions = NewExecutions},

    %% Start async tool execution
    Self = self(),
    spawn(fun() -> execute_tool_async(Self, ExecutionId, ToolName, Params) end),

    {reply, {ok, ExecutionId}, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({stream_chunk, ExecutionId, ChunkData}, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {noreply, State};
        Execution ->
            %% Send chunk to all subscribers
            Timestamp = erlang:monotonic_time(microsecond),
            NewSubscribers =
                lists:map(fun(Subscriber) ->
                             send_chunk_to_subscriber(Subscriber, ExecutionId, ChunkData, Timestamp)
                          end,
                          Execution#execution.subscribers),

            %% Update execution
            NewExecution =
                Execution#execution{subscribers = NewSubscribers,
                                    chunks_sent = Execution#execution.chunks_sent + 1},
            NewExecutions = maps:put(ExecutionId, NewExecution, State#state.executions),
            {noreply, State#state{executions = NewExecutions}}
    end;
handle_cast({stream_complete, ExecutionId}, State) ->
    case maps:get(ExecutionId, State#state.executions, undefined) of
        undefined ->
            {noreply, State};
        Execution ->
            %% Notify all subscribers of completion
            lists:foreach(fun(Subscriber) ->
                             Subscriber#subscriber.pid ! {stream_complete, ExecutionId}
                          end,
                          Execution#execution.subscribers),

            %% Mark execution as complete
            NewExecution = Execution#execution{completed = true},
            NewExecutions = maps:put(ExecutionId, NewExecution, State#state.executions),
            {noreply, State#state{executions = NewExecutions}}
    end;
handle_cast({register_tool, ToolName, Handler}, State) ->
    NewTools = maps:put(ToolName, Handler, State#state.tools),
    {noreply, State#state{tools = NewTools}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, SubscriberPid, _Reason}, State) ->
    %% Remove dead subscriber from all executions
    NewExecutions =
        maps:map(fun(_ExecId, Execution) ->
                    case lists:keyfind(MonitorRef,
                                       #subscriber.monitor_ref,
                                       Execution#execution.subscribers)
                    of
                        false ->
                            Execution;
                        _Subscriber ->
                            NewSubscribers =
                                lists:keydelete(SubscriberPid,
                                                #subscriber.pid,
                                                Execution#execution.subscribers),
                            Execution#execution{subscribers = NewSubscribers}
                    end
                 end,
                 State#state.executions),
    {noreply, State#state{executions = NewExecutions}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Send chunk to subscriber with backpressure handling
send_chunk_to_subscriber(Subscriber, ExecutionId, ChunkData, Timestamp) ->
    case Subscriber#subscriber.backpressure of
        drop ->
            %% Drop if subscriber is slow (mailbox check)
            case erlang:process_info(Subscriber#subscriber.pid, message_queue_len) of
                {message_queue_len, Len} when Len < 10 ->
                    Subscriber#subscriber.pid ! {stream_chunk, ExecutionId, ChunkData, Timestamp},
                    Subscriber#subscriber{chunks_received =
                                              Subscriber#subscriber.chunks_received + 1};
                _ ->
                    %% Drop chunk
                    Subscriber#subscriber{chunks_dropped = Subscriber#subscriber.chunks_dropped + 1}
            end;
        buffer ->
            %% Buffer chunks if under limit
            if Subscriber#subscriber.buffer_size < Subscriber#subscriber.max_buffer_size ->
                   %% Add to buffer
                   NewBuffer = Subscriber#subscriber.buffer ++ [{ChunkData, Timestamp}],
                   Subscriber#subscriber{buffer = NewBuffer,
                                         buffer_size = Subscriber#subscriber.buffer_size + 1};
               true ->
                   %% Buffer full, drop
                   Subscriber#subscriber{chunks_dropped = Subscriber#subscriber.chunks_dropped + 1}
            end;
        block ->
            %% Always send (blocking)
            Subscriber#subscriber.pid ! {stream_chunk, ExecutionId, ChunkData, Timestamp},
            Subscriber#subscriber{chunks_received = Subscriber#subscriber.chunks_received + 1}
    end.

%% @doc Execute tool asynchronously and stream results
execute_tool_async(Server, ExecutionId, ToolName, Params) ->
    %% Demo tool: slow data processor
    NumChunks = maps:get(chunks, Params, 10),
    DelayMs = maps:get(delay_ms, Params, 200),
    ChunkSize = maps:get(chunk_size, Params, 100),

    %% Generate and stream chunks
    lists:foreach(fun(ChunkNum) ->
                     %% Simulate processing delay
                     timer:sleep(DelayMs),

                     %% Generate chunk data
                     ChunkData =
                         #{chunk_num => ChunkNum,
                           total_chunks => NumChunks,
                           data => generate_chunk_data(ChunkSize),
                           timestamp => erlang:system_time(microsecond)},

                     %% Stream chunk to server
                     gen_server:cast(Server, {stream_chunk, ExecutionId, ChunkData})
                  end,
                  lists:seq(1, NumChunks)),

    %% Signal completion
    gen_server:cast(Server, {stream_complete, ExecutionId}).

%% @doc Generate random chunk data
generate_chunk_data(Size) ->
    list_to_binary([rand:uniform(26) + 64 || _ <- lists:seq(1, Size)]).

%% @doc Register demo tool
register_demo_tool(Server) ->
    Handler = fun(_Params) -> ok end,
    gen_server:cast(Server, {register_tool, <<"slow_data_processor">>, Handler}).

%% @doc Spawn a subscriber process
spawn_subscriber(Server, ExecutionId, Name, Speed) ->
    Parent = self(),
    spawn(fun() ->
             %% Subscribe to execution
             ok = subscribe(Server, ExecutionId),
             io:format("[~s] Subscribed to execution: ~p~n", [Name, ExecutionId]),

             %% Processing delay based on speed
             DelayMs =
                 case Speed of
                     fast ->
                         0;
                     medium ->
                         10;
                     slow ->
                         50
                 end,

             %% Receive chunks
             subscriber_loop(Name, ExecutionId, DelayMs, [], Parent)
          end).

%% @doc Subscriber loop receiving chunks
subscriber_loop(Name, ExecutionId, DelayMs, Latencies, Parent) ->
    receive
        {stream_chunk, ExecutionId, ChunkData, SendTime} ->
            %% Calculate latency
            ReceiveTime = erlang:monotonic_time(microsecond),
            Latency = ReceiveTime - SendTime,

            %% Simulate processing
            timer:sleep(DelayMs),

            ChunkNum = maps:get(chunk_num, ChunkData),
            TotalChunks = maps:get(total_chunks, ChunkData),
            io:format("[~s] Received chunk ~p/~p (latency: ~p us)~n",
                      [Name, ChunkNum, TotalChunks, Latency]),

            subscriber_loop(Name, ExecutionId, DelayMs, [Latency | Latencies], Parent);
        {stream_complete, ExecutionId} ->
            io:format("[~s] Stream complete. Total chunks: ~p~n", [Name, length(Latencies)]),
            subscriber_complete(Name, Latencies, Parent);
        stop ->
            ok;
        _Other ->
            subscriber_loop(Name, ExecutionId, DelayMs, Latencies, Parent)
    after 5000 ->
        io:format("[~s] Timeout waiting for chunks~n", [Name]),
        subscriber_complete(Name, Latencies, Parent)
    end.

%% @doc Subscriber completion
subscriber_complete(Name, Latencies, Parent) ->
    receive
        {get_stats, From} ->
            From ! {stats, Name, length(Latencies), Latencies},
            subscriber_complete(Name, Latencies, Parent);
        stop ->
            ok
    end.

%% @doc Collect subscriber statistics
collect_subscriber_stats(SubscriberPid) ->
    SubscriberPid ! {get_stats, self()},
    receive
        {stats, Name, ChunksReceived, Latencies} ->
            {Name, ChunksReceived, Latencies}
    after 1000 ->
        {<<"Unknown">>, 0, []}
    end.

%% @doc Calculate percentile
percentile([], _P) ->
    0;
percentile(List, P) ->
    Sorted = lists:sort(List),
    Index = max(1, min(length(Sorted), round(P * length(Sorted)))),
    lists:nth(Index, Sorted).
