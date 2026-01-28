%%%-------------------------------------------------------------------
%%% @doc
%%% Message Tracker
%%% Tracks message sending, receiving, loss, and duplication
%%% for chaos testing and validation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_message_tracker).

-behaviour(gen_server).

%% API
-export([
    start/0,
    track_sent/4,
    track_received/3,
    track_lost/2,
    get_sent_count/0,
    get_received_count/0,
    get_lost_count/0,
    get_duplicate_count/0,
    reset/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    sent = #{},         % WorkerId -> [MessageIds]
    received = #{},     % WorkerId -> [MessageIds]
    lost = #{},         % WorkerId -> [MessageIds]
    duplicates = #{},   % WorkerId -> count
    total_sent = 0,
    total_received = 0,
    total_lost = 0,
    total_duplicates = 0
}).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    Pid.

track_sent(Tracker, WorkerId, MessageId, SeqNum) ->
    gen_server:cast(Tracker, {track_sent, WorkerId, MessageId, SeqNum}).

track_received(Tracker, WorkerId, MessageId) ->
    gen_server:cast(Tracker, {track_received, WorkerId, MessageId}).

track_lost(Tracker, WorkerId, MessageId) ->
    gen_server:cast(Tracker, {track_lost, WorkerId, MessageId}).

get_sent_count() ->
    gen_server:call(?MODULE, get_sent_count).

get_received_count() ->
    gen_server:call(?MODULE, get_received_count).

get_lost_count() ->
    gen_server:call(?MODULE, get_lost_count).

get_duplicate_count() ->
    gen_server:call(?MODULE, get_duplicate_count).

reset() ->
    gen_server:call(?MODULE, reset).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call(get_sent_count, _From, State) ->
    {reply, State#state.total_sent, State};

handle_call(get_received_count, _From, State) ->
    {reply, State#state.total_received, State};

handle_call(get_lost_count, _From, State) ->
    {reply, State#state.total_lost, State};

handle_call(get_duplicate_count, _From, State) ->
    {reply, State#state.total_duplicates, State};

handle_call(reset, _From, _State) ->
    {reply, ok, #state{}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({track_sent, WorkerId, MessageId, _SeqNum}, State) ->
    SentMap = State#state.sent,
    WorkerSent = maps:get(WorkerId, SentMap, []),
    NewWorkerSent = [MessageId | WorkerSent],
    NewSentMap = maps:put(WorkerId, NewWorkerSent, SentMap),
    NewState = State#state{
        sent = NewSentMap,
        total_sent = State#state.total_sent + 1
    },
    {noreply, NewState};

handle_cast({track_received, WorkerId, MessageId}, State) ->
    RecvMap = State#state.received,
    WorkerRecv = maps:get(WorkerId, RecvMap, []),

    %% Check for duplicates
    IsDuplicate = lists:member(MessageId, WorkerRecv),
    DupCount = case IsDuplicate of
        true -> 1;
        false -> 0
    end,

    NewWorkerRecv = [MessageId | WorkerRecv],
    NewRecvMap = maps:put(WorkerId, NewWorkerRecv, RecvMap),

    %% Update duplicate count
    CurrentDups = maps:get(WorkerId, State#state.duplicates, 0),
    NewDups = maps:put(WorkerId, CurrentDups + DupCount, State#state.duplicates),

    NewState = State#state{
        received = NewRecvMap,
        total_received = State#state.total_received + 1,
        total_duplicates = State#state.total_duplicates + DupCount,
        duplicates = NewDups
    },
    {noreply, NewState};

handle_cast({track_lost, WorkerId, MessageId}, State) ->
    LostMap = State#state.lost,
    WorkerLost = maps:get(WorkerId, LostMap, []),
    NewWorkerLost = [MessageId | WorkerLost],
    NewLostMap = maps:put(WorkerId, NewWorkerLost, LostMap),
    NewState = State#state{
        lost = NewLostMap,
        total_lost = State#state.total_lost + 1
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
