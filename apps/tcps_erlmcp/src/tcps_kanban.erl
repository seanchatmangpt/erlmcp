%%%-----------------------------------------------------------------------------
%%% @doc TCPS Kanban WIP Limits and Heijunka Leveling System
%%%
%%% This module implements Toyota-style Kanban work-in-progress (WIP) limit
%%% management and Heijunka (production leveling) for TPS-aligned continuous
%%% improvement workflows.
%%%
%%% == Features ==
%%% - WIP limit enforcement per bucket (reliability, security, cost, compliance)
%%% - Heijunka leveling algorithm prevents batching
%%% - Pull signal processing with WIP checks
%%% - State persistence to ontology (work_orders.ttl)
%%% - Receipt emission for all state changes
%%%
%%% == Kanban Flow ==
%%% 1. Pull signal arrives (demand from downstream)
%%% 2. Check WIP limit for target bucket
%%% 3. If available, create work order and increment WIP
%%% 4. If limit reached, return refusal receipt
%%% 5. Heijunka leveling distributes work evenly
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_kanban).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,
    check_wip_limit/1,
    set_wip_limit/2,
    get_current_wip/1,
    get_wip_status/1,
    level_work_orders/1,
    process_pull_signal/1,
    complete_work_order/2,
    reset_state/0,
    heijunka_schedule/0,
    start_work_order/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Types
-type bucket() :: reliability | security | cost | compliance.
-type work_order_id() :: binary().
-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := non_neg_integer(),
    created_at := erlang:timestamp(),
    status := pending | in_progress | completed,
    payload := map()
}.
-type pull_signal() :: #{
    bucket := bucket(),
    priority => non_neg_integer(),
    payload := map()
}.
-type leveled_schedule() :: [{bucket(), [work_order()]}].
-type wip_status() :: #{
    current := non_neg_integer(),
    limit := non_neg_integer() | infinity,
    available := non_neg_integer() | infinity,
    utilization := float()
}.

-export_type([bucket/0, work_order_id/0, work_order/0, pull_signal/0,
              leveled_schedule/0, wip_status/0]).

%% State record
-record(state, {
    %% WIP limits per bucket (default: 5)
    wip_limits = #{
        reliability => 5,
        security => 5,
        cost => 5,
        compliance => 5
    } :: #{bucket() => pos_integer() | infinity},

    %% Current WIP count per bucket
    current_wip = #{
        reliability => 0,
        security => 0,
        cost => 0,
        compliance => 0
    } :: #{bucket() => non_neg_integer()},

    %% Active work orders by bucket
    work_orders = #{
        reliability => [],
        security => [],
        cost => [],
        compliance => []
    } :: #{bucket() => [work_order()]},

    %% Work order index for fast lookup
    order_index = #{} :: #{work_order_id() => work_order()},

    %% Heijunka state - track last N allocations for leveling
    recent_allocations = [] :: [bucket()],
    max_consecutive = 2 :: pos_integer(),

    %% Receipt counter for tracking
    receipt_counter = 0 :: non_neg_integer()
}).

-type state() :: #state{}.

%%=============================================================================
%% API Functions
%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec check_wip_limit(bucket()) -> {ok, available} | {error, limit_reached}.
check_wip_limit(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {check_wip_limit, Bucket}).

-spec set_wip_limit(bucket(), pos_integer() | infinity) -> ok.
set_wip_limit(Bucket, Limit) when is_atom(Bucket), (is_integer(Limit) orelse Limit =:= infinity) ->
    gen_server:call(?MODULE, {set_wip_limit, Bucket, Limit}).

-spec get_current_wip(bucket()) -> non_neg_integer().
get_current_wip(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {get_current_wip, Bucket}).

-spec get_wip_status(bucket()) -> wip_status().
get_wip_status(Bucket) when is_atom(Bucket) ->
    gen_server:call(?MODULE, {get_wip_status, Bucket}).

-spec level_work_orders([work_order()]) -> leveled_schedule().
level_work_orders(WorkOrders) when is_list(WorkOrders) ->
    gen_server:call(?MODULE, {level_work_orders, WorkOrders}).

-spec process_pull_signal(pull_signal()) ->
    {ok, work_order_id()} | {error, limit_reached} | {error, term()}.
process_pull_signal(Signal) when is_map(Signal) ->
    gen_server:call(?MODULE, {process_pull_signal, Signal}).

-spec complete_work_order(bucket(), work_order_id()) -> ok | {error, not_found}.
complete_work_order(Bucket, OrderId) when is_atom(Bucket), is_binary(OrderId) ->
    gen_server:call(?MODULE, {complete_work_order, Bucket, OrderId}).

-spec reset_state() -> ok.
reset_state() ->
    gen_server:call(?MODULE, reset_state).

-spec heijunka_schedule() -> [work_order()].
heijunka_schedule() ->
    gen_server:call(?MODULE, heijunka_schedule).

-spec start_work_order(work_order()) -> ok | {error, term()}.
start_work_order(WorkOrder) when is_map(WorkOrder) ->
    gen_server:call(?MODULE, {start_work_order, WorkOrder}).

%%=============================================================================
%% gen_server Callbacks
%%=============================================================================

-spec init(list()) -> {ok, state()}.
init([]) ->
    {ok, #state{}};
init([Config]) when is_map(Config) ->
    State = #state{},
    %% Apply custom WIP limits if provided
    State2 = case maps:get(wip_limits, Config, undefined) of
        undefined -> State;
        Limits when is_map(Limits) ->
            State#state{wip_limits = Limits}
    end,
    %% Apply custom max_consecutive if provided
    State3 = case maps:get(max_consecutive, Config, undefined) of
        undefined -> State2;
        MaxConsec when is_integer(MaxConsec), MaxConsec > 0 ->
            State2#state{max_consecutive = MaxConsec}
    end,
    {ok, State3}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.

handle_call({check_wip_limit, Bucket}, _From, State) ->
    #state{wip_limits = Limits, current_wip = CurrentWip} = State,
    Limit = maps:get(Bucket, Limits, infinity),
    Current = maps:get(Bucket, CurrentWip, 0),
    Result = if
        Limit =:= infinity -> {ok, available};
        Current < Limit -> {ok, available};
        true -> {error, limit_reached}
    end,
    {reply, Result, State};

handle_call({set_wip_limit, Bucket, Limit}, _From, State) ->
    #state{wip_limits = Limits} = State,
    NewLimits = maps:put(Bucket, Limit, Limits),
    NewState = State#state{wip_limits = NewLimits},
    emit_receipt(NewState, wip_limit_changed, #{bucket => Bucket, limit => Limit}),
    {reply, ok, NewState};

handle_call({get_current_wip, Bucket}, _From, State) ->
    #state{current_wip = CurrentWip} = State,
    Current = maps:get(Bucket, CurrentWip, 0),
    {reply, Current, State};

handle_call({get_wip_status, Bucket}, _From, State) ->
    #state{wip_limits = Limits, current_wip = CurrentWip} = State,
    Limit = maps:get(Bucket, Limits, infinity),
    Current = maps:get(Bucket, CurrentWip, 0),
    Status = #{
        current => Current,
        limit => Limit,
        available => if
            Limit =:= infinity -> infinity;
            true -> max(0, Limit - Current)
        end,
        utilization => if
            Limit =:= infinity -> 0.0;
            Limit =:= 0 -> 0.0;
            true -> Current / Limit
        end
    },
    {reply, Status, State};

handle_call({level_work_orders, WorkOrders}, _From, State) ->
    #state{max_consecutive = MaxConsec} = State,
    LeveledSchedule = apply_heijunka_leveling(WorkOrders, MaxConsec),
    {reply, LeveledSchedule, State};

handle_call({process_pull_signal, Signal}, _From, State) ->
    case process_pull_signal_impl(Signal, State) of
        {ok, OrderId, NewState} ->
            {reply, {ok, OrderId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({complete_work_order, Bucket, OrderId}, _From, State) ->
    case complete_work_order_impl(Bucket, OrderId, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(reset_state, _From, _State) ->
    {reply, ok, #state{}};

handle_call(heijunka_schedule, _From, State) ->
    Result = do_heijunka_schedule(State),
    {reply, Result, State};

handle_call({start_work_order, WorkOrder}, _From, State) ->
    case do_start_work_order(WorkOrder, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%=============================================================================
%% Internal Functions
%%=============================================================================

%% @doc Apply Heijunka leveling algorithm to prevent batching
%% Rules:
%% 1. Count work orders per bucket
%% 2. Distribute proportionally across time slots
%% 3. Prevent more than MaxConsecutive from same bucket (best effort with balanced distribution)
%% 4. Rotate through buckets (reliability -> security -> cost -> compliance)
%%
%% Note: This is a best-effort algorithm. With severely uneven distribution
%% (e.g., 7 items from one bucket, 1 from another), the max consecutive constraint
%% may be violated when work from other buckets is exhausted.  In production TPS/Kanban,
%% this indicates upstream process imbalance that should be addressed.
-spec apply_heijunka_leveling([work_order()], pos_integer()) -> leveled_schedule().
apply_heijunka_leveling(WorkOrders, MaxConsecutive) ->
    %% Group by bucket
    Grouped = lists:foldl(fun(Order, Acc) ->
        Bucket = maps:get(bucket, Order),
        maps:update_with(Bucket, fun(L) -> [Order | L] end, [Order], Acc)
    end, #{}, WorkOrders),

    %% Sort each bucket by priority (highest first)
    Sorted = maps:map(fun(_Bucket, Orders) ->
        lists:sort(fun(A, B) ->
            maps:get(priority, A, 0) >= maps:get(priority, B, 0)
        end, Orders)
    end, Grouped),

    %% Apply improved leveling algorithm using round-robin interleaving
    AllBuckets = [reliability, security, cost, compliance],
    apply_round_robin_leveling(AllBuckets, Sorted, MaxConsecutive).

%% @doc Improved round-robin leveling - take MaxConsec from each bucket in turn
-spec apply_round_robin_leveling([bucket()], #{bucket() => [work_order()]}, pos_integer()) ->
    leveled_schedule().
apply_round_robin_leveling(_AllBuckets, Remaining, _MaxConsecutive) when map_size(Remaining) =:= 0 ->
    [];
apply_round_robin_leveling(AllBuckets, Remaining, MaxConsecutive) ->
    %% Get available buckets (those with work remaining)
    AvailableBuckets = [B || B <- AllBuckets, maps:is_key(B, Remaining)],

    case AvailableBuckets of
        [] -> [];
        [SingleBucket] ->
            %% Only one bucket left - take one at a time to avoid violations
            case maps:get(SingleBucket, Remaining) of
                [] -> [];
                [Order | RestOrders] ->
                    NewRemaining = case RestOrders of
                        [] -> maps:remove(SingleBucket, Remaining);
                        _ -> maps:put(SingleBucket, RestOrders, Remaining)
                    end,
                    [{SingleBucket, [Order]} | apply_round_robin_leveling(AllBuckets, NewRemaining, 1)]
            end;
        _ ->
            %% Multiple buckets - simple strategy: always take 1 at a time
            %% This guarantees max consecutive constraint is never violated
            TakeCount = 1,

            %% Take TakeCount items from each bucket in round-robin
            {NewRemaining, Schedule} = lists:foldl(fun(Bucket, {RemAcc, SchedAcc}) ->
                case maps:get(Bucket, RemAcc, undefined) of
                    undefined ->
                        {RemAcc, SchedAcc};
                    Orders ->
                        %% Take up to TakeCount orders
                        {ToTake, LeftOver} = take_n(Orders, TakeCount),
                        NewRem = case LeftOver of
                            [] -> maps:remove(Bucket, RemAcc);
                            _ -> maps:put(Bucket, LeftOver, RemAcc)
                        end,
                        %% Add to schedule
                        NewSched = SchedAcc ++ [{Bucket, [O]} || O <- ToTake],
                        {NewRem, NewSched}
                end
            end, {Remaining, []}, AvailableBuckets),

            %% Continue with remaining work
            Schedule ++ apply_round_robin_leveling(AllBuckets, NewRemaining, MaxConsecutive)
    end.

-spec update_recent_buckets([bucket()], bucket()) -> [bucket()].
update_recent_buckets(Recent, Bucket) ->
    %% Keep last 10 for tracking
    case length(Recent) >= 10 of
        true -> [Bucket | lists:sublist(Recent, 9)];
        false -> [Bucket | Recent]
    end.

%% @doc Take up to N elements from list
-spec take_n([T], pos_integer()) -> {[T], [T]}.
take_n(List, N) ->
    take_n(List, N, []).

take_n([], _N, Acc) ->
    {lists:reverse(Acc), []};
take_n(Rest, 0, Acc) ->
    {lists:reverse(Acc), Rest};
take_n([H | T], N, Acc) ->
    take_n(T, N - 1, [H | Acc]).


%% @doc Process pull signal - check WIP limit and create work order
-spec process_pull_signal_impl(pull_signal(), state()) ->
    {ok, work_order_id(), state()} | {error, term()}.
process_pull_signal_impl(Signal, State) ->
    %% Validate signal
    case validate_pull_signal(Signal) of
        {ok, Bucket, Priority, Payload} ->
            %% Check WIP limit
            case check_wip_limit_internal(Bucket, State) of
                {ok, available} ->
                    %% Create work order
                    OrderId = generate_work_order_id(),
                    WorkOrder = #{
                        id => OrderId,
                        bucket => Bucket,
                        priority => Priority,
                        created_at => erlang:timestamp(),
                        status => pending,
                        payload => Payload
                    },

                    %% Add to state
                    NewState = add_work_order(Bucket, WorkOrder, State),

                    %% Emit receipt
                    emit_receipt(NewState, work_order_created, #{
                        order_id => OrderId,
                        bucket => Bucket,
                        priority => Priority
                    }),

                    {ok, OrderId, NewState};
                {error, limit_reached} ->
                    %% Emit refusal receipt
                    emit_receipt(State, work_order_refused, #{
                        bucket => Bucket,
                        reason => wip_limit_reached
                    }),
                    {error, limit_reached}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_pull_signal(pull_signal()) ->
    {ok, bucket(), non_neg_integer(), map()} | {error, term()}.
validate_pull_signal(Signal) ->
    case maps:get(bucket, Signal, undefined) of
        undefined -> {error, missing_bucket};
        Bucket when not is_atom(Bucket) -> {error, invalid_bucket};
        Bucket ->
            case maps:get(payload, Signal, undefined) of
                undefined -> {error, missing_payload};
                Payload when not is_map(Payload) -> {error, invalid_payload};
                Payload ->
                    Priority = maps:get(priority, Signal, 0),
                    {ok, Bucket, Priority, Payload}
            end
    end.

-spec check_wip_limit_internal(bucket(), state()) ->
    {ok, available} | {error, limit_reached}.
check_wip_limit_internal(Bucket, State) ->
    #state{wip_limits = Limits, current_wip = CurrentWip} = State,
    Limit = maps:get(Bucket, Limits, infinity),
    Current = maps:get(Bucket, CurrentWip, 0),
    if
        Limit =:= infinity -> {ok, available};
        Current < Limit -> {ok, available};
        true -> {error, limit_reached}
    end.

-spec generate_work_order_id() -> work_order_id().
generate_work_order_id() ->
    Timestamp = erlang:system_time(microsecond),
    Rand = rand:uniform(1000000),
    iolist_to_binary(io_lib:format("wo_~p_~p", [Timestamp, Rand])).

-spec add_work_order(bucket(), work_order(), state()) -> state().
add_work_order(Bucket, WorkOrder, State) ->
    #state{
        work_orders = Orders,
        order_index = Index,
        current_wip = CurrentWip,
        recent_allocations = RecentAlloc
    } = State,

    OrderId = maps:get(id, WorkOrder),
    BucketOrders = maps:get(Bucket, Orders, []),
    NewBucketOrders = [WorkOrder | BucketOrders],
    NewOrders = maps:put(Bucket, NewBucketOrders, Orders),
    NewIndex = maps:put(OrderId, WorkOrder, Index),

    %% Increment WIP count
    CurrentCount = maps:get(Bucket, CurrentWip, 0),
    NewCurrentWip = maps:put(Bucket, CurrentCount + 1, CurrentWip),

    %% Update recent allocations
    NewRecentAlloc = update_recent_buckets(RecentAlloc, Bucket),

    State#state{
        work_orders = NewOrders,
        order_index = NewIndex,
        current_wip = NewCurrentWip,
        recent_allocations = NewRecentAlloc
    }.

%% @doc Complete work order and decrement WIP count
-spec complete_work_order_impl(bucket(), work_order_id(), state()) ->
    {ok, state()} | {error, term()}.
complete_work_order_impl(Bucket, OrderId, State) ->
    #state{
        work_orders = Orders,
        order_index = Index,
        current_wip = CurrentWip
    } = State,

    %% Check if work order exists
    case maps:get(OrderId, Index, undefined) of
        undefined ->
            {error, not_found};
        WorkOrder ->
            %% Verify bucket matches
            case maps:get(bucket, WorkOrder) of
                Bucket ->
                    %% Remove from bucket orders
                    BucketOrders = maps:get(Bucket, Orders, []),
                    NewBucketOrders = lists:filter(fun(O) ->
                        maps:get(id, O) =/= OrderId
                    end, BucketOrders),
                    NewOrders = maps:put(Bucket, NewBucketOrders, Orders),

                    %% Remove from index
                    NewIndex = maps:remove(OrderId, Index),

                    %% Decrement WIP count
                    CurrentCount = maps:get(Bucket, CurrentWip, 0),
                    NewCurrentWip = maps:put(Bucket, max(0, CurrentCount - 1), CurrentWip),

                    NewState = State#state{
                        work_orders = NewOrders,
                        order_index = NewIndex,
                        current_wip = NewCurrentWip
                    },

                    %% Emit receipt
                    emit_receipt(NewState, work_order_completed, #{
                        order_id => OrderId,
                        bucket => Bucket
                    }),

                    {ok, NewState};
                _OtherBucket ->
                    {error, bucket_mismatch}
            end
    end.

%% @doc Emit receipt for state change (stub - would integrate with ontology)
-spec emit_receipt(state(), atom(), map()) -> ok.
emit_receipt(State, EventType, Data) ->
    #state{receipt_counter = Counter} = State,
    Receipt = #{
        counter => Counter + 1,
        event_type => EventType,
        timestamp => erlang:timestamp(),
        data => Data
    },
    %% TODO: Persist to ontology (work_orders.ttl)
    %% For now, just log
    logger:info("Kanban receipt emitted: ~p", [Receipt]),
    ok.

%% @doc Get heijunka schedule - all work orders leveled
-spec do_heijunka_schedule(state()) -> [work_order()].
do_heijunka_schedule(State) ->
    #state{work_orders = Orders, max_consecutive = MaxConsec} = State,

    %% Get all work orders across all buckets
    AllWorkOrders = lists:flatten(maps:values(Orders)),

    %% Apply Heijunka leveling
    LeveledSchedule = apply_heijunka_leveling(AllWorkOrders, MaxConsec),

    %% Flatten to list of work orders
    lists:flatten([WOs || {_Bucket, WOs} <- LeveledSchedule]).

%% @doc Start a work order - check WIP limit and add to active
-spec do_start_work_order(work_order(), state()) ->
    {ok, state()} | {error, term()}.
do_start_work_order(WorkOrder, State) ->
    Bucket = maps:get(bucket, WorkOrder),

    %% Check WIP limit
    case check_wip_limit_internal(Bucket, State) of
        {ok, available} ->
            %% Add work order to active state
            OrderId = maps:get(id, WorkOrder),
            UpdatedWorkOrder = WorkOrder#{status => in_progress},

            NewState = add_work_order(Bucket, UpdatedWorkOrder, State),

            %% Emit receipt
            emit_receipt(NewState, work_order_started, #{
                order_id => OrderId,
                bucket => Bucket
            }),

            {ok, NewState};
        {error, limit_reached} ->
            {error, wip_limit}
    end.
