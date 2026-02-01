%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Connection Pool Manager
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_pool).

-behaviour(gen_server).

%% API
-export([start_link/2, acquire/1, acquire/2, release/2, get_pool_stats/1, close_pool/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_POOL_SIZE, 10).
-define(CHECKOUT_TIMEOUT, 5000).

-type pool_id() :: atom().
-type connection() :: pid().

-record(state, {pools = #{} :: #{pool_id() => map()}, health_timer :: reference() | undefined}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(PoolId, Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, {PoolId, Config}, []).

acquire(PoolId) ->
    acquire(PoolId, ?CHECKOUT_TIMEOUT).

acquire(PoolId, Timeout) ->
    gen_server:call(?MODULE, {acquire, PoolId}, Timeout).

release(PoolId, Connection) ->
    gen_server:cast(?MODULE, {release, PoolId, Connection}).

get_pool_stats(PoolId) ->
    gen_server:call(?MODULE, {get_stats, PoolId}).

close_pool(PoolId) ->
    gen_server:call(?MODULE, {close, PoolId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({PoolId, Config}) ->
    InitialState =
        #{available => queue:new(),
          in_use => #{},
          size => 0,
          config => Config},

    {ok, #state{pools = #{PoolId => InitialState}, health_timer = undefined}}.

handle_call({acquire, PoolId}, From, State) ->
    case maps:get(PoolId, State#state.pools, undefined) of
        undefined ->
            gen_server:reply(From, {error, pool_not_found}),
            {noreply, State};
        PoolState ->
            Available = maps:get(available, PoolState),
            InUse = maps:get(in_use, PoolState),
            case queue:out(Available) of
                {{value, Connection}, NewAvailable} ->
                    MonitorRef = monitor(process, Connection),
                    NewInUse = maps:put(Connection, MonitorRef, InUse),
                    NewPoolState = PoolState#{available => NewAvailable, in_use => NewInUse},
                    NewPools = maps:put(PoolId, NewPoolState, State#state.pools),
                    gen_server:reply(From, {ok, Connection}),
                    {noreply, State#state{pools = NewPools}};
                {empty, _} ->
                    gen_server:reply(From, {error, no_connections}),
                    {noreply, State}
            end
    end;
handle_call({get_stats, PoolId}, _From, State) ->
    case maps:get(PoolId, State#state.pools, undefined) of
        undefined ->
            {reply, {error, pool_not_found}, State};
        PoolState ->
            AvailableCount =
                queue:len(
                    maps:get(available, PoolState)),
            InUseCount =
                maps:size(
                    maps:get(in_use, PoolState)),
            Stats =
                #{pool_id => PoolId,
                  available_connections => AvailableCount,
                  in_use_connections => InUseCount,
                  total_connections => AvailableCount + InUseCount},
            {reply, {ok, Stats}, State}
    end;
handle_call({close, PoolId}, _From, State) ->
    case maps:get(PoolId, State#state.pools, undefined) of
        undefined ->
            {reply, {error, pool_not_found}, State};
        PoolState ->
            close_all_connections(PoolState),
            NewPools = maps:remove(PoolId, State#state.pools),
            {reply, ok, State#state{pools = NewPools}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({release, PoolId, Connection}, State) ->
    case maps:get(PoolId, State#state.pools, undefined) of
        undefined ->
            {noreply, State};
        PoolState ->
            InUse = maps:get(in_use, PoolState),
            case maps:get(Connection, InUse, undefined) of
                undefined ->
                    {noreply, State};
                MonitorRef ->
                    demonitor(MonitorRef, [flush]),
                    NewInUse = maps:remove(Connection, InUse),
                    Available = maps:get(available, PoolState),
                    NewAvailable = queue:in(Connection, Available),
                    NewPoolState = PoolState#{in_use => NewInUse, available => NewAvailable},
                    NewPools = maps:put(PoolId, NewPoolState, State#state.pools),
                    {noreply, State#state{pools = NewPools}}
            end
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    NewPools =
        maps:map(fun(_PoolId, PoolState) ->
                    InUse = maps:get(in_use, PoolState),
                    % Find and remove the connection with this monitor ref
                    case maps:fold(fun (Conn, MonRef, {found, ConnKey}) ->
                                           {found, ConnKey};
                                       (Conn, MonRef, not_found) when MonRef =:= MonitorRef ->
                                           {found, Conn};
                                       (_Conn, _MonRef, Acc) ->
                                           Acc
                                   end,
                                   not_found,
                                   InUse)
                    of
                        {found, Connection} ->
                            NewInUse = maps:remove(Connection, InUse),
                            PoolState#{in_use => NewInUse};
                        not_found ->
                            PoolState
                    end
                 end,
                 State#state.pools),
    {noreply, State#state{pools = NewPools}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{pools = Pools}) ->
    maps:foreach(fun(_PoolId, PoolState) -> close_all_connections(PoolState) end, Pools),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

close_all_connections(PoolState) ->
    Available = maps:get(available, PoolState),
    InUse = maps:get(in_use, PoolState),

    queue:fold(fun(Connection, _Acc) ->
                  catch gen_tcp:close(Connection),
                  ok
               end,
               ok,
               Available),

    maps:foreach(fun(Connection, _MonitorRef) ->
                    catch gen_tcp:close(Connection),
                    ok
                 end,
                 InUse),
    ok.
