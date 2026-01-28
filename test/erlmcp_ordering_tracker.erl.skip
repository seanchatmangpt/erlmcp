%%%-------------------------------------------------------------------
%%% @doc
%%% Message Ordering Tracker
%%% Validates that messages arrive in order under chaos conditions
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ordering_tracker).

-behaviour(gen_server).

%% API
-export([
    start/0,
    track_sent/3,
    track_received/3,
    get_out_of_order_count/0,
    get_total_messages/0,
    reset/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    sent_sequences = #{},       % WorkerId -> [SeqNum]
    received_sequences = #{},   % WorkerId -> [SeqNum]
    out_of_order_count = 0,
    total_messages = 0
}).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

track_sent(Tracker, WorkerId, SeqNum) ->
    gen_server:cast(Tracker, {sent, WorkerId, SeqNum}).

track_received(Tracker, WorkerId, SeqNum) ->
    gen_server:cast(Tracker, {received, WorkerId, SeqNum}).

get_out_of_order_count() ->
    gen_server:call(?MODULE, get_out_of_order).

get_total_messages() ->
    gen_server:call(?MODULE, get_total).

reset() ->
    gen_server:call(?MODULE, reset).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_out_of_order, _From, State) ->
    {reply, State#state.out_of_order_count, State};

handle_call(get_total, _From, State) ->
    {reply, State#state.total_messages, State};

handle_call(reset, _From, _State) ->
    {reply, ok, #state{}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({sent, WorkerId, SeqNum}, State) ->
    SentSeqs = State#state.sent_sequences,
    WorkerSeqs = maps:get(WorkerId, SentSeqs, []),
    NewWorkerSeqs = [SeqNum | WorkerSeqs],
    NewSentSeqs = maps:put(WorkerId, NewWorkerSeqs, SentSeqs),
    NewState = State#state{sent_sequences = NewSentSeqs},
    {noreply, NewState};

handle_cast({received, WorkerId, SeqNum}, State) ->
    RecvSeqs = State#state.received_sequences,
    WorkerRecvSeqs = maps:get(WorkerId, RecvSeqs, []),

    %% Check if this message is out of order
    %% A message is out of order if we've already seen a higher sequence number
    IsOutOfOrder = lists:any(fun(PrevSeq) -> PrevSeq > SeqNum end, WorkerRecvSeqs),

    OutOfOrderIncrement = case IsOutOfOrder of
        true -> 1;
        false -> 0
    end,

    NewWorkerRecvSeqs = [SeqNum | WorkerRecvSeqs],
    NewRecvSeqs = maps:put(WorkerId, NewWorkerRecvSeqs, RecvSeqs),

    NewState = State#state{
        received_sequences = NewRecvSeqs,
        out_of_order_count = State#state.out_of_order_count + OutOfOrderIncrement,
        total_messages = State#state.total_messages + 1
    },
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
