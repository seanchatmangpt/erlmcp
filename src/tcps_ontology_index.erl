%%%-------------------------------------------------------------------
%%% @doc TCPS Ontology ETS Indexing for Fast Queries
%%%
%%% Creates and maintains ETS indexes for common SPARQL query patterns.
%%% Provides 100x+ faster lookups than full SPARQL queries for simple
%%% access patterns.
%%%
%%% Indexes:
%%% - sku_receipts_index: SKU -> [Receipt IDs]
%%% - andon_by_type_index: Severity -> [Andon IDs]
%%% - work_order_by_bucket_index: Bucket -> [Work Order IDs]
%%% - receipt_by_stage_index: Stage -> [Receipt IDs]
%%% - sku_by_status_index: Status -> [SKU IDs]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_ontology_index).

-behaviour(gen_server).

-export([
    start_link/0,
    stop/0,

    % Index management
    create_indexes/0,
    rebuild_indexes/0,
    clear_indexes/0,

    % Index updates (incremental)
    index_receipt/1,
    index_work_order/1,
    index_andon/1,
    index_sku/1,

    % Fast queries (no SPARQL needed)
    get_receipts_by_sku/1,
    get_receipts_by_stage/1,
    get_andons_by_severity/1,
    get_work_orders_by_bucket/1,
    get_skus_by_readiness/1,

    % Statistics
    index_stats/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

% ETS table names
-define(SKU_RECEIPTS_INDEX, sku_receipts_index).
-define(ANDON_BY_TYPE_INDEX, andon_by_type_index).
-define(WORK_ORDER_BY_BUCKET_INDEX, work_order_by_bucket_index).
-define(RECEIPT_BY_STAGE_INDEX, receipt_by_stage_index).
-define(SKU_BY_STATUS_INDEX, sku_by_status_index).
-define(RECEIPT_DATA_INDEX, receipt_data_index).
-define(WORK_ORDER_DATA_INDEX, work_order_data_index).
-define(ANDON_DATA_INDEX, andon_data_index).

-record(state, {
    indexes_created = false :: boolean(),
    last_rebuild = undefined :: undefined | integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec create_indexes() -> ok.
create_indexes() ->
    gen_server:call(?SERVER, create_indexes).

-spec rebuild_indexes() -> ok.
rebuild_indexes() ->
    gen_server:call(?SERVER, rebuild_indexes).

-spec clear_indexes() -> ok.
clear_indexes() ->
    gen_server:call(?SERVER, clear_indexes).

%% Incremental index updates

-spec index_receipt(map()) -> ok.
index_receipt(Receipt) ->
    gen_server:cast(?SERVER, {index_receipt, Receipt}).

-spec index_work_order(map()) -> ok.
index_work_order(WorkOrder) ->
    gen_server:cast(?SERVER, {index_work_order, WorkOrder}).

-spec index_andon(map()) -> ok.
index_andon(Andon) ->
    gen_server:cast(?SERVER, {index_andon, Andon}).

-spec index_sku(map()) -> ok.
index_sku(SKU) ->
    gen_server:cast(?SERVER, {index_sku, SKU}).

%% Fast queries

-spec get_receipts_by_sku(binary()) -> [map()].
get_receipts_by_sku(SKUId) ->
    case ets:lookup(?SKU_RECEIPTS_INDEX, SKUId) of
        [] -> [];
        Objects ->
            ReceiptIds = [ReceiptId || {_, ReceiptId} <- Objects],
            lists:filtermap(fun(RId) ->
                case ets:lookup(?RECEIPT_DATA_INDEX, RId) of
                    [{_, Data}] -> {true, Data};
                    [] -> false
                end
            end, ReceiptIds)
    end.

-spec get_receipts_by_stage(binary()) -> [map()].
get_receipts_by_stage(Stage) ->
    case ets:lookup(?RECEIPT_BY_STAGE_INDEX, Stage) of
        [] -> [];
        Objects ->
            ReceiptIds = [ReceiptId || {_, ReceiptId} <- Objects],
            lists:filtermap(fun(RId) ->
                case ets:lookup(?RECEIPT_DATA_INDEX, RId) of
                    [{_, Data}] -> {true, Data};
                    [] -> false
                end
            end, ReceiptIds)
    end.

-spec get_andons_by_severity(binary()) -> [map()].
get_andons_by_severity(Severity) ->
    case ets:lookup(?ANDON_BY_TYPE_INDEX, Severity) of
        [] -> [];
        Objects ->
            AndonIds = [AndonId || {_, AndonId} <- Objects],
            lists:filtermap(fun(AId) ->
                case ets:lookup(?ANDON_DATA_INDEX, AId) of
                    [{_, Data}] -> {true, Data};
                    [] -> false
                end
            end, AndonIds)
    end.

-spec get_work_orders_by_bucket(binary()) -> [map()].
get_work_orders_by_bucket(Bucket) ->
    case ets:lookup(?WORK_ORDER_BY_BUCKET_INDEX, Bucket) of
        [] -> [];
        Objects ->
            WOIds = [WOId || {_, WOId} <- Objects],
            lists:filtermap(fun(WId) ->
                case ets:lookup(?WORK_ORDER_DATA_INDEX, WId) of
                    [{_, Data}] -> {true, Data};
                    [] -> false
                end
            end, WOIds)
    end.

-spec get_skus_by_readiness(boolean()) -> [binary()].
get_skus_by_readiness(IsReady) ->
    Status = if IsReady -> ready; true -> not_ready end,
    case ets:lookup(?SKU_BY_STATUS_INDEX, Status) of
        [] -> [];
        Objects -> [SKUId || {_, SKUId} <- Objects]
    end.

-spec index_stats() -> map().
index_stats() ->
    #{
        sku_receipts => ets:info(?SKU_RECEIPTS_INDEX, size),
        andon_by_type => ets:info(?ANDON_BY_TYPE_INDEX, size),
        work_order_by_bucket => ets:info(?WORK_ORDER_BY_BUCKET_INDEX, size),
        receipt_by_stage => ets:info(?RECEIPT_BY_STAGE_INDEX, size),
        sku_by_status => ets:info(?SKU_BY_STATUS_INDEX, size),
        receipt_data => ets:info(?RECEIPT_DATA_INDEX, size),
        work_order_data => ets:info(?WORK_ORDER_DATA_INDEX, size),
        andon_data => ets:info(?ANDON_DATA_INDEX, size)
    }.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(create_indexes, _From, State) ->
    Result = do_create_indexes(),
    {reply, Result, State#state{indexes_created = true}};

handle_call(rebuild_indexes, _From, State) ->
    do_clear_indexes(),
    do_create_indexes(),
    Result = do_rebuild_from_ontology(),
    {reply, Result, State#state{
        indexes_created = true,
        last_rebuild = erlang:system_time(second)
    }};

handle_call(clear_indexes, _From, State) ->
    do_clear_indexes(),
    {reply, ok, State#state{indexes_created = false}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({index_receipt, Receipt}, State) ->
    do_index_receipt(Receipt),
    {noreply, State};

handle_cast({index_work_order, WorkOrder}, State) ->
    do_index_work_order(WorkOrder),
    {noreply, State};

handle_cast({index_andon, Andon}, State) ->
    do_index_andon(Andon),
    {noreply, State};

handle_cast({index_sku, SKU}, State) ->
    do_index_sku(SKU),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    do_clear_indexes(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

do_create_indexes() ->
    % Bag tables for multi-value indexes
    ets:new(?SKU_RECEIPTS_INDEX, [bag, named_table, public]),
    ets:new(?ANDON_BY_TYPE_INDEX, [bag, named_table, public]),
    ets:new(?WORK_ORDER_BY_BUCKET_INDEX, [bag, named_table, public]),
    ets:new(?RECEIPT_BY_STAGE_INDEX, [bag, named_table, public]),
    ets:new(?SKU_BY_STATUS_INDEX, [bag, named_table, public]),

    % Set tables for data storage
    ets:new(?RECEIPT_DATA_INDEX, [set, named_table, public]),
    ets:new(?WORK_ORDER_DATA_INDEX, [set, named_table, public]),
    ets:new(?ANDON_DATA_INDEX, [set, named_table, public]),

    ?LOG_INFO("Created TCPS ontology ETS indexes"),
    ok.

do_clear_indexes() ->
    Tables = [
        ?SKU_RECEIPTS_INDEX,
        ?ANDON_BY_TYPE_INDEX,
        ?WORK_ORDER_BY_BUCKET_INDEX,
        ?RECEIPT_BY_STAGE_INDEX,
        ?SKU_BY_STATUS_INDEX,
        ?RECEIPT_DATA_INDEX,
        ?WORK_ORDER_DATA_INDEX,
        ?ANDON_DATA_INDEX
    ],
    lists:foreach(fun(Table) ->
        catch ets:delete(Table)
    end, Tables),
    ok.

do_rebuild_from_ontology() ->
    % This would parse RDF files and populate indexes
    % For now, placeholder for ontology parsing
    ?LOG_INFO("Rebuilding indexes from ontology files"),
    ok.

do_index_receipt(Receipt) ->
    ReceiptId = maps:get(receipt_id, Receipt),
    SKUId = maps:get(sku_id, Receipt),
    Stage = maps:get(stage, Receipt),

    % Update indexes
    ets:insert(?SKU_RECEIPTS_INDEX, {SKUId, ReceiptId}),
    ets:insert(?RECEIPT_BY_STAGE_INDEX, {Stage, ReceiptId}),
    ets:insert(?RECEIPT_DATA_INDEX, {ReceiptId, Receipt}),

    ok.

do_index_work_order(WorkOrder) ->
    WOId = maps:get(work_order_id, WorkOrder),
    Bucket = maps:get(bucket, WorkOrder),

    ets:insert(?WORK_ORDER_BY_BUCKET_INDEX, {Bucket, WOId}),
    ets:insert(?WORK_ORDER_DATA_INDEX, {WOId, WorkOrder}),

    ok.

do_index_andon(Andon) ->
    AndonId = maps:get(andon_id, Andon),
    Severity = maps:get(severity, Andon),

    ets:insert(?ANDON_BY_TYPE_INDEX, {Severity, AndonId}),
    ets:insert(?ANDON_DATA_INDEX, {AndonId, Andon}),

    ok.

do_index_sku(SKU) ->
    SKUId = maps:get(sku_id, SKU),
    IsReady = maps:get(is_ready, SKU, false),

    Status = if IsReady -> ready; true -> not_ready end,
    ets:insert(?SKU_BY_STATUS_INDEX, {Status, SKUId}),

    ok.
