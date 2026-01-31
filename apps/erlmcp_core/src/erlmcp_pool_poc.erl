-module(erlmcp_pool_poc).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    stop/1,
    checkout/1,
    checkin/2,
    get_stats/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    pool_size :: pos_integer(),
    available = [] :: [resource()],
    checked_out = #{} :: #{pid() => resource()},
    waiting = queue:new() :: queue:queue({pid(), reference()})
}).

-type resource() :: term().

%%====================================================================
%% API
%%====================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(PoolSize) ->
    gen_server:start_link(?MODULE, [PoolSize], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

-spec checkout(pid()) -> {ok, resource()} | {error, timeout}.
checkout(Pid) ->
    gen_server:call(Pid, checkout, 10000).

-spec checkin(pid(), resource()) -> ok | {error, not_checked_out}.
checkin(Pid, Resource) ->
    gen_server:call(Pid, {checkin, Resource}).

-spec get_stats(pid()) -> {ok, #{atom() => term()}}.
get_stats(Pid) ->
    gen_server:call(Pid, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([PoolSize]) ->
    %% Create pool of resources (simple integers for POC)
    Available = lists:seq(1, PoolSize),
    {ok, #state{pool_size = PoolSize, available = Available}}.

handle_call(checkout, From = {ClientPid, _Ref}, State = #state{available = Available, checked_out = CheckedOut, waiting = Waiting}) ->
    case Available of
        [Resource | Rest] ->
            %% Resource available - check it out
            monitor(process, ClientPid),
            NewCheckedOut = maps:put(ClientPid, Resource, CheckedOut),
            {reply, {ok, Resource}, State#state{available = Rest, checked_out = NewCheckedOut}};
        [] ->
            %% No resources available - queue the request
            NewWaiting = queue:in(From, Waiting),
            {noreply, State#state{waiting = NewWaiting}}
    end;

handle_call({checkin, Resource}, {ClientPid, _Ref}, State = #state{available = Available, checked_out = CheckedOut, waiting = Waiting}) ->
    case maps:take(ClientPid, CheckedOut) of
        {Resource, NewCheckedOut} ->
            %% Valid checkin
            case queue:out(Waiting) of
                {{value, WaitingFrom = {WaitingPid, _}}, NewWaiting} ->
                    %% Give resource to waiting client
                    monitor(process, WaitingPid),
                    gen_server:reply(WaitingFrom, {ok, Resource}),
                    FinalCheckedOut = maps:put(WaitingPid, Resource, NewCheckedOut),
                    {reply, ok, State#state{checked_out = FinalCheckedOut, waiting = NewWaiting}};
                {empty, _} ->
                    %% No one waiting - return to pool
                    {reply, ok, State#state{available = [Resource | Available], checked_out = NewCheckedOut}}
            end;
        error ->
            {reply, {error, not_checked_out}, State}
    end;

handle_call(get_stats, _From, State) ->
    Stats = #{
        pool_size => State#state.pool_size,
        available => length(State#state.available),
        checked_out => maps:size(State#state.checked_out),
        waiting => queue:len(State#state.waiting)
    },
    {reply, {ok, Stats}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{available = Available, checked_out = CheckedOut, waiting = Waiting}) ->
    %% Client died - return resource to pool
    case maps:take(Pid, CheckedOut) of
        {Resource, NewCheckedOut} ->
            case queue:out(Waiting) of
                {{value, WaitingFrom = {WaitingPid, _}}, NewWaiting} ->
                    %% Give resource to waiting client
                    monitor(process, WaitingPid),
                    gen_server:reply(WaitingFrom, {ok, Resource}),
                    FinalCheckedOut = maps:put(WaitingPid, Resource, NewCheckedOut),
                    {noreply, State#state{checked_out = FinalCheckedOut, waiting = NewWaiting}};
                {empty, _} ->
                    %% No one waiting - return to pool
                    {noreply, State#state{available = [Resource | Available], checked_out = NewCheckedOut}}
            end;
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
