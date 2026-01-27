%% @doc Dummy worker for connection pool testing
%% Simulates a real connection without actual network I/O
%% Used in stress tests to validate pool behavior under load

-module(erlmcp_test_dummy_worker).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    worker_id :: pos_integer(),
    created_at :: pos_integer(),
    operation_count = 0 :: non_neg_integer()
}).

-type state() :: #state{}.

%%====================================================================
%% API
%%====================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(WorkerId) ->
    gen_server:start_link(?MODULE, [WorkerId], []).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([pos_integer()]) -> {ok, state()}.
init([WorkerId]) ->
    State = #state{
        worker_id = WorkerId,
        created_at = erlang:system_time(millisecond),
        operation_count = 0
    },
    {ok, State}.

-spec handle_call(term(), {pid(), reference()}, state()) ->
    {reply, term(), state()}.
handle_call(get_id, _From, State) ->
    {reply, State#state.worker_id, State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        worker_id => State#state.worker_id,
        created_at => State#state.created_at,
        operation_count => State#state.operation_count,
        uptime_ms => erlang:system_time(millisecond) - State#state.created_at
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State#state{operation_count = State#state.operation_count + 1}}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State#state{operation_count = State#state.operation_count + 1}}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

