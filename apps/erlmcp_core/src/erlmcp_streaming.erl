%% @doc Streaming execution manager for progressive tool results.
%% Manages streaming executions with support for:
%% - Multiple subscribers per execution
%% - Progressive chunk delivery
%% - Completion signaling
%% - Automatic cleanup on subscriber death
-module(erlmcp_streaming).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API
-export([start_link/0, start_link/1, stop/1, start_stream/2, start_stream/3, send_chunk/2,
         send_chunk/3, complete_stream/2, complete_stream/3, error_stream/2, error_stream/3,
         cancel_stream/1, cancel_stream/2, is_streaming/1, is_streaming/2, get_subscribers/1,
         get_subscribers/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {streams = #{} :: #{execution_id() => stream()}}).
-record(stream,
        {id :: execution_id(),
         subscribers = [] :: [pid()],
         monitors = #{} :: #{pid() => reference()},
         metadata = #{} :: map(),
         started_at :: integer()}).

-type execution_id() :: reference() | binary().
-type stream() :: #stream{}.
-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Start a streaming execution (single pid or list of pids)
-spec start_stream(execution_id(), pid() | [pid()]) -> ok | {error, term()}.
start_stream(ExecutionId, Subscriber) when is_pid(Subscriber) ->
    start_stream(ExecutionId, [Subscriber], #{});
start_stream(ExecutionId, Subscribers) when is_list(Subscribers) ->
    gen_server:call(?MODULE, {start_stream, ExecutionId, Subscribers, #{}}).

%% @doc Start a streaming execution with subscribers and metadata
-spec start_stream(execution_id(), [pid()], map()) -> ok | {error, term()}.
start_stream(ExecutionId, Subscribers, Metadata) when is_list(Subscribers), is_map(Metadata) ->
    gen_server:call(?MODULE, {start_stream, ExecutionId, Subscribers, Metadata}).

%% @doc Send a chunk to all subscribers of an execution
-spec send_chunk(execution_id(), term()) -> ok | {error, term()}.
send_chunk(ExecutionId, Chunk) ->
    gen_server:cast(?MODULE, {send_chunk, ExecutionId, Chunk}).

%% @doc Send a chunk to all subscribers with server pid
-spec send_chunk(pid(), execution_id(), term()) -> ok | {error, term()}.
send_chunk(Pid, ExecutionId, Chunk) ->
    gen_server:cast(Pid, {send_chunk, ExecutionId, Chunk}).

%% @doc Signal completion of a streaming execution
-spec complete_stream(execution_id(), term()) -> ok | {error, term()}.
complete_stream(ExecutionId, FinalResult) ->
    gen_server:cast(?MODULE, {complete_stream, ExecutionId, FinalResult}).

%% @doc Signal completion with server pid
-spec complete_stream(pid(), execution_id(), term()) -> ok | {error, term()}.
complete_stream(Pid, ExecutionId, FinalResult) ->
    gen_server:cast(Pid, {complete_stream, ExecutionId, FinalResult}).

%% @doc Signal error in a streaming execution
-spec error_stream(execution_id(), term()) -> ok | {error, term()}.
error_stream(ExecutionId, Error) ->
    gen_server:cast(?MODULE, {error_stream, ExecutionId, Error}).

%% @doc Signal error with server pid
-spec error_stream(pid(), execution_id(), term()) -> ok | {error, term()}.
error_stream(Pid, ExecutionId, Error) ->
    gen_server:cast(Pid, {error_stream, ExecutionId, Error}).

%% @doc Cancel a streaming execution
-spec cancel_stream(execution_id()) -> ok | {error, term()}.
cancel_stream(ExecutionId) ->
    gen_server:call(?MODULE, {cancel_stream, ExecutionId}).

%% @doc Cancel with server pid
-spec cancel_stream(pid(), execution_id()) -> ok | {error, term()}.
cancel_stream(Pid, ExecutionId) ->
    gen_server:call(Pid, {cancel_stream, ExecutionId}).

%% @doc Check if an execution is currently streaming
-spec is_streaming(execution_id()) -> boolean().
is_streaming(ExecutionId) ->
    gen_server:call(?MODULE, {is_streaming, ExecutionId}).

%% @doc Check if streaming with server pid
-spec is_streaming(pid(), execution_id()) -> boolean().
is_streaming(Pid, ExecutionId) ->
    gen_server:call(Pid, {is_streaming, ExecutionId}).

%% @doc Get subscribers for an execution
-spec get_subscribers(execution_id()) -> {ok, [pid()]} | {error, not_found}.
get_subscribers(ExecutionId) ->
    gen_server:call(?MODULE, {get_subscribers, ExecutionId}).

%% @doc Get subscribers with server pid
-spec get_subscribers(pid(), execution_id()) -> {ok, [pid()]} | {error, not_found}.
get_subscribers(Pid, ExecutionId) ->
    gen_server:call(Pid, {get_subscribers, ExecutionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, state()}.
init(_Opts) ->
    {ok, #state{}}.

handle_call({start_stream, ExecutionId, Subscribers, Metadata},
            _From,
            State = #state{streams = Streams}) ->
    case maps:is_key(ExecutionId, Streams) of
        true ->
            {reply, {error, already_streaming}, State};
        false ->
            %% Monitor all subscribers
            Monitors =
                lists:foldl(fun(Sub, Acc) ->
                               Ref = monitor(process, Sub),
                               maps:put(Sub, Ref, Acc)
                            end,
                            #{},
                            Subscribers),

            Stream =
                #stream{id = ExecutionId,
                        subscribers = Subscribers,
                        monitors = Monitors,
                        metadata = Metadata,
                        started_at = erlang:system_time(millisecond)},

            NewStreams = maps:put(ExecutionId, Stream, Streams),
            {reply, ok, State#state{streams = NewStreams}}
    end;
handle_call({cancel_stream, ExecutionId}, _From, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream} ->
            %% Notify all subscribers of cancellation
            lists:foreach(fun(Sub) -> Sub ! {stream_cancelled, ExecutionId} end,
                          Stream#stream.subscribers),
            %% Cleanup and reply
            {noreply, NewState} = cleanup_stream(ExecutionId, Stream, State),
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call({is_streaming, ExecutionId}, _From, State = #state{streams = Streams}) ->
    {reply, maps:is_key(ExecutionId, Streams), State};
handle_call({get_subscribers, ExecutionId}, _From, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, #stream{subscribers = Subs}} ->
            {reply, {ok, Subs}, State};
        error ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({send_chunk, ExecutionId, Chunk}, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, #stream{subscribers = Subs}} ->
            %% Send chunk to all subscribers
            lists:foreach(fun(Sub) -> Sub ! {stream_chunk, ExecutionId, Chunk} end, Subs),
            {noreply, State};
        error ->
            %% Execution not found - silently ignore
            {noreply, State}
    end;
handle_cast({complete_stream, ExecutionId, FinalResult}, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{subscribers = Subs}} ->
            %% Send completion to all subscribers
            lists:foreach(fun(Sub) -> Sub ! {stream_complete, ExecutionId, FinalResult} end, Subs),
            %% Cleanup
            cleanup_stream(ExecutionId, Stream, State);
        error ->
            {noreply, State}
    end;
handle_cast({error_stream, ExecutionId, Error}, State = #state{streams = Streams}) ->
    case maps:find(ExecutionId, Streams) of
        {ok, Stream = #stream{subscribers = Subs}} ->
            %% Send error to all subscribers
            lists:foreach(fun(Sub) -> Sub ! {stream_error, ExecutionId, Error} end, Subs),
            %% Cleanup
            cleanup_stream(ExecutionId, Stream, State);
        error ->
            {noreply, State}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{streams = Streams}) ->
    %% Remove dead subscriber from all streams
    NewStreams =
        maps:map(fun(_ExecId, Stream = #stream{subscribers = Subs, monitors = Mons}) ->
                    case lists:member(Pid, Subs) of
                        true ->
                            NewSubs = lists:delete(Pid, Subs),
                            NewMons = maps:remove(Pid, Mons),
                            Stream#stream{subscribers = NewSubs, monitors = NewMons};
                        false ->
                            Stream
                    end
                 end,
                 Streams),

    %% Remove streams with no subscribers
    CleanedStreams =
        maps:filter(fun(_ExecId, #stream{subscribers = Subs}) -> Subs =/= [] end, NewStreams),

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

%% @doc Cleanup a stream and demonitor subscribers
-spec cleanup_stream(execution_id(), stream(), state()) -> {noreply, state()}.
cleanup_stream(ExecutionId, #stream{monitors = Monitors}, State = #state{streams = Streams}) ->
    %% Demonitor all subscribers
    maps:foreach(fun(_Pid, Ref) -> demonitor(Ref, [flush]) end, Monitors),

    %% Remove stream
    NewStreams = maps:remove(ExecutionId, Streams),
    {noreply, State#state{streams = NewStreams}}.
