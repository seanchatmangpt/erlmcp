%%%====================================================================
%%% @doc Cache Warming Worker
%%%
%%% Supervised gen_server for async cache warming operations.
%%% Replaces unsupervised spawn/1 calls with proper OTP supervision.
%%%
%%% Supervision: Managed by erlmcp_cache_warmer_sup
%%% Strategy: simple_one_for_one - one worker per warming task
%%%
%%% @end
%%%====================================================================
-module(erlmcp_cache_warmer).
-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    key :: term(),
    value_fun :: fun(() -> term()),
    ttl_seconds :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a cache warming worker
-spec start_link(term(), fun(() -> term()), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Key, ValueFun, TTLSeconds) ->
    gen_server:start_link(?MODULE, [Key, ValueFun, TTLSeconds], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Key, ValueFun, TTLSeconds]) ->
    %% Trigger async warming immediately
    gen_server:cast(self(), warm_cache),
    {ok, #state{key = Key, value_fun = ValueFun, ttl_seconds = TTLSeconds}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(warm_cache, State) ->
    %% Execute warming in handle_cast (async)
    try
        Value = (State#state.value_fun)(),
        erlmcp_cache:put(State#state.key, Value, {ttl, State#state.ttl_seconds}),
        ?LOG_DEBUG("Cache warmed for key ~p", [State#state.key]),
        %% Stop worker after successful warming
        {stop, normal, State}
    catch
        Class:Reason:Stack ->
            ?LOG_WARNING("Cache warm failed for ~p: ~p:~p~n~p",
                        [State#state.key, Class, Reason, Stack]),
            %% Stop worker after failure
            {stop, {warming_failed, Class, Reason}, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
