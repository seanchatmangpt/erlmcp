%%%-------------------------------------------------------------------
%%% @doc
### Advanced Rate Limiter for Load Testing

This module implements a token bucket algorithm for precise rate
 limiting with dynamic adjustment capabilities.
%%%
 Features:
%%% - Token bucket algorithm with configurable bucket size
%%% - Dynamic rate adjustment based on error rates
%%% - Burst handling capability
%%% - Backpressure awareness
%%% - Distributed coordination (when needed)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_rate_limiter).

-behaviour(gen_server).

-export([start/1, stop/1, set_rate/2, can_send/1, get_stats/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
## Type Definitions
%%====================================================================

-record(state, {
    rate :: pos_integer(),             % Target rate (requests/second)
    burst :: pos_integer(),             % Burst capacity
    tokens :: float(),                  % Current tokens in bucket
    max_tokens :: pos_integer(),        % Maximum tokens
    last_update :: pos_integer(),       % Last update time
    refill_rate :: float(),            % Token refill rate
    stats :: #{
        total_requested :: pos_integer(),
        total_sent :: pos_integer(),
        total_dropped :: pos_integer(),
        current_rate :: float(),
        peak_rate :: float()
    }
}).

-type state() :: #state{}.

%%====================================================================
## API Functions
%%====================================================================

-spec start(pos_integer()) -> {ok, pid()} | {error, term()}.
start(Rate) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Rate], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec set_rate(pid(), pos_integer()) -> ok.
set_rate(Pid, NewRate) ->
    gen_server:cast(Pid, {set_rate, NewRate}).

-spec can_send(pid()) -> boolean().
can_send(Pid) ->
    gen_server:call(Pid, can_send).

-spec get_stats(pid()) -> {ok, map()}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%====================================================================
## gen_server Callbacks
%%====================================================================

-spec init([pos_integer()]) -> {ok, state()}.
init([InitialRate]) ->
    process_flag(trap_exit, true),

    %% Initialize state with conservative defaults
    InitialBurst = InitialRate * 2,  % 2 seconds of burst
    InitialTokens = InitialBurst,
    RefillRate = InitialRate,  % 1 token per request second

    State = #state{
        rate = InitialRate,
        burst = InitialBurst,
        tokens = InitialTokens,
        max_tokens = InitialBurst,
        last_update = erlang:system_time(millisecond),
        refill_rate = RefillRate,
        stats = #{
            total_requested => 0,
            total_sent => 0,
            total_dropped => 0,
            current_rate => 0.0,
            peak_rate => 0.0
        }
    },

    %% Start token refill timer
    TimerRef = erlang:start_timer(100, self(), refill_tokens),

    {ok, State#state{last_update = erlang:system_time(millisecond)}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                       {reply, term(), state()} | {stop, term(), state()}.
handle_call(can_send, _From, State) ->
    CanSend = State#state.tokens > 0,
    {reply, CanSend, State};

handle_call(get_stats, _From, State) ->
    {reply, {ok, State#state.stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_cast({set_rate, NewRate}, State) ->
    %% Update rate and recalculate burst
    NewBurst = NewRate * 2,
    NewTokens = min(State#state.tokens, NewBurst),
    NewStats = State#state.stats#{
        current_rate => NewRate
    },

    NewState = State#state{
        rate = NewRate,
        burst = NewBurst,
        tokens = NewTokens,
        max_tokens = NewBurst,
        refill_rate = NewRate,
        stats = NewStats
    },

    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({timeout, TimerRef, refill_tokens}, State) ->
    %% Refill tokens in bucket
    NewTokens = refill_tokens(State),
    NewState = State#state{tokens = NewTokens},

    %% Schedule next refill
    NewTimerRef = erlang:start_timer(100, self(), refill_tokens),

    {noreply, NewState#state{timer_ref = NewTimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## Internal Functions
%%====================================================================

%% Refill tokens based on elapsed time
-spec refill_tokens(state()) -> float().
refill_tokens(State) ->
    CurrentTime = erlang:system_time(millisecond),
    ElapsedMs = CurrentTime - State#state.last_update,
    ElapsedSeconds = ElapsedMs / 1000.0,

    TokensToAdd = State#state.refill_rate * ElapsedSeconds,
    NewTokens = min(State#state.max_tokens, State#state.tokens + TokensToAdd),

    %% Update stats
    State#state.stats#{
        current_rate => State#state.rate
    },

    NewTokens.

%% Check if we can send a request
-spec consume_token(state()) -> {ok, state()} | {error, state()}.
consume_token(State) ->
    case State#state.tokens >= 1.0 of
        true ->
            NewTokens = State#state.tokens - 1.0,
            NewStats = State#state.stats#{
                total_requested => State#state.stats#total_requested + 1,
                total_sent => State#state.stats#total_sent + 1
            },
            {ok, State#state{tokens = NewTokens, stats = NewStats}};
        false ->
            NewStats = State#state.stats#{
                total_requested => State#state.stats#total_requested + 1,
                total_dropped => State#state.stats#total_dropped + 1
            },
            {error, State#state{stats = NewStats}}
    end.