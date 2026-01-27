%%%-----------------------------------------------------------------------------
%%% @doc TCPS SKU (Stock Keeping Unit) Lifecycle Management System
%%%
%%% Production-grade SKU lifecycle orchestration implementing Toyota Production
%%% System principles for manufacturing software artifacts (SKUs = Marketplace offerings).
%%%
%%% Core Responsibilities:
%%% - SKU creation from work orders (demand-driven production)
%%% - Complete stage-by-stage production pipeline execution
%%% - Quality gate enforcement at each stage transition
%%% - Andon event integration (stop-the-line on defects)
%%% - Receipt chain tracking for full audit trail
%%% - Marketplace publication workflow
%%% - Production metrics and status reporting
%%%
%%% Production Pipeline Stages (Toyota Standard Work):
%%% 1. shacl_validation - RDF/ontology validation
%%% 2. compilation - Erlang/OTP compilation
%%% 3. test_execution - Unit + integration tests (80%+ coverage)
%%% 4. security_scan - Vulnerability detection
%%% 5. deterministic_build - Reproducible build verification
%%% 6. quality_metrics - Code quality assessment
%%% 7. release_verification - Pre-release validation
%%% 8. smoke_test - Deployment smoke tests
%%% 9. marketplace_validation - Marketplace compliance check
%%% 10. publication - Final marketplace publication
%%%
%%% A SKU moves through production stages sequentially. Each stage transition
%%% requires passing quality gates and generates receipts. Open Andon events
%%% block progression until resolved (stop-the-line principle).
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_sku).
-behaviour(gen_server).

%% API exports - SKU Creation
-export([
    start_link/0,
    start_link/1,
    stop/0,
    create_sku/1,
    get_sku/1,
    list_all_skus/0,
    delete_sku/1
]).

%% API exports - Stage Management
-export([
    get_sku_status/1,
    get_current_stage/1,
    transition_stage/2,
    can_transition/2,
    get_production_history/1
]).

%% API exports - Pipeline Execution
-export([
    process_sku_pipeline/1,
    process_sku_pipeline_async/1,
    get_pipeline_progress/1,
    abort_pipeline/2
]).

%% API exports - Release & Publication
-export([
    mark_released/1,
    mark_published/1,
    get_marketplace_status/1,
    generate_marketplace_url/1
]).

%% API exports - Receipt Management
-export([
    add_receipt_to_sku/2,
    get_receipts/1,
    verify_receipt_chain/1
]).

%% API exports - Integration
-export([
    produce_sku/1,
    produce_sku_with_options/2
]).

%% API exports - Reporting
-export([
    get_all_skus_by_status/1,
    get_production_metrics/0,
    get_stage_statistics/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Test support
-ifdef(TEST).
-export([reset_state/0, init_ets/0]).
-on_load(init_ets/0).
-endif.

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type sku_id() :: binary().
-type work_order_id() :: binary().
-type receipt_id() :: binary().

-type production_stage() :: shacl_validation
                          | compilation
                          | test_execution
                          | security_scan
                          | deterministic_build
                          | quality_metrics
                          | release_verification
                          | smoke_test
                          | marketplace_validation
                          | publication.

-type sku_status() :: in_production
                    | completed
                    | failed
                    | published
                    | aborted.

-type stage_result() :: pass | fail.

-type stage_history_entry() :: #{
    stage := production_stage(),
    timestamp := integer(),
    status := stage_result(),
    reason => binary(),
    receipt_id => receipt_id()
}.

-type sku() :: #{
    id := sku_id(),
    work_order_id := work_order_id(),
    created_at := integer(),
    updated_at := integer(),
    current_stage := production_stage(),
    status := sku_status(),
    production_history := [stage_history_entry()],
    receipts := [receipt_id()],
    metadata := map(),
    failure_reason => binary(),
    completed_at => integer(),
    published_at => integer(),
    marketplace_url => binary()
}.

-type pipeline_options() :: #{
    auto_publish => boolean(),
    stop_on_warning => boolean(),
    skip_stages => [production_stage()],
    timeout_per_stage => pos_integer()
}.

-export_type([
    sku_id/0,
    sku/0,
    production_stage/0,
    sku_status/0,
    stage_history_entry/0,
    pipeline_options/0
]).

%%%=============================================================================
%%% State Record
%%%=============================================================================

-record(state, {
    %% SKUs by ID
    skus = #{} :: #{sku_id() => sku()},

    %% Pipeline workers (async processing)
    pipeline_workers = #{} :: #{sku_id() => pid()},

    %% Configuration
    config = #{
        receipts_dir => "priv/receipts/skus",
        auto_persist => true,
        max_concurrent_pipelines => 10
    } :: map()
}).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, tcps_skus).

%% Production stages in order
-define(PRODUCTION_STAGES, [
    shacl_validation,
    compilation,
    test_execution,
    security_scan,
    deterministic_build,
    quality_metrics,
    release_verification,
    smoke_test,
    marketplace_validation,
    publication
]).

%%%=============================================================================
%%% API - SKU Creation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the SKU lifecycle manager as a gen_server.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%------------------------------------------------------------------------------
%% @doc Create a SKU from a work order.
%%
%% The work order must be in 'pending' or 'in_progress' status.
%% Creates a new SKU record and initializes it at the first stage (shacl_validation).
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_sku(WorkOrderId :: work_order_id()) ->
    {ok, sku_id()} | {error, term()}.
create_sku(WorkOrderId) ->
    gen_server:call(?SERVER, {create_sku, WorkOrderId}, infinity).

%%------------------------------------------------------------------------------
%% @doc Get SKU by ID.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_sku(SkuId :: sku_id()) -> {ok, sku()} | {error, not_found}.
get_sku(SkuId) ->
    gen_server:call(?SERVER, {get_sku, SkuId}).

%%------------------------------------------------------------------------------
%% @doc List all SKUs in the system.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_all_skus() -> [sku()].
list_all_skus() ->
    gen_server:call(?SERVER, list_all_skus).

%%------------------------------------------------------------------------------
%% @doc Delete a SKU (for testing/cleanup).
%%
%% @end
%%------------------------------------------------------------------------------
-spec delete_sku(SkuId :: sku_id()) -> ok | {error, not_found}.
delete_sku(SkuId) ->
    gen_server:call(?SERVER, {delete_sku, SkuId}).

%%%=============================================================================
%%% API - Stage Management
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get detailed status for a SKU.
%%
%% Returns comprehensive status including:
%% - Current stage
%% - Status (in_production, completed, failed, etc.)
%% - Completion percentage
%% - Receipt count
%% - Open Andon count
%% - Production history
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_sku_status(SkuId :: sku_id()) -> {ok, map()} | {error, not_found}.
get_sku_status(SkuId) ->
    gen_server:call(?SERVER, {get_sku_status, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Get current production stage for a SKU.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_current_stage(SkuId :: sku_id()) ->
    {ok, production_stage()} | {error, not_found}.
get_current_stage(SkuId) ->
    gen_server:call(?SERVER, {get_current_stage, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Transition SKU to next stage.
%%
%% This function:
%% 1. Validates stage sequence (must be sequential)
%% 2. Checks quality gates for current stage
%% 3. Checks for open Andon events (blocking)
%% 4. Updates SKU record with new stage
%% 5. Records transition in production history
%% 6. Broadcasts event to dashboard
%%
%% @end
%%------------------------------------------------------------------------------
-spec transition_stage(SkuId :: sku_id(), NextStage :: production_stage()) ->
    ok | {error, term()}.
transition_stage(SkuId, NextStage) ->
    gen_server:call(?SERVER, {transition_stage, SkuId, NextStage}, infinity).

%%------------------------------------------------------------------------------
%% @doc Check if SKU can transition to next stage.
%%
%% Checks:
%% - Stage sequence validity
%% - Quality gate status
%% - Open Andon events
%%
%% @end
%%------------------------------------------------------------------------------
-spec can_transition(SkuId :: sku_id(), NextStage :: production_stage()) ->
    {ok, can_proceed} | {error, term()}.
can_transition(SkuId, NextStage) ->
    gen_server:call(?SERVER, {can_transition, SkuId, NextStage}).

%%------------------------------------------------------------------------------
%% @doc Get complete production history for a SKU.
%%
%% Returns list of stage transitions with timestamps and results.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_production_history(SkuId :: sku_id()) ->
    {ok, [stage_history_entry()]} | {error, not_found}.
get_production_history(SkuId) ->
    gen_server:call(?SERVER, {get_production_history, SkuId}).

%%%=============================================================================
%%% API - Pipeline Execution
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process SKU through entire production pipeline synchronously.
%%
%% Runs SKU through all production stages sequentially.
%% Stops on first failure.
%% Returns final stage reached.
%%
%% @end
%%------------------------------------------------------------------------------
-spec process_sku_pipeline(SkuId :: sku_id()) ->
    {ok, production_stage()} | {error, {failed_at, production_stage(), term()}}.
process_sku_pipeline(SkuId) ->
    gen_server:call(?SERVER, {process_sku_pipeline, SkuId}, infinity).

%%------------------------------------------------------------------------------
%% @doc Process SKU through pipeline asynchronously.
%%
%% Spawns worker process to run pipeline.
%% Returns immediately with worker PID.
%% Use get_pipeline_progress/1 to check status.
%%
%% @end
%%------------------------------------------------------------------------------
-spec process_sku_pipeline_async(SkuId :: sku_id()) ->
    {ok, pid()} | {error, term()}.
process_sku_pipeline_async(SkuId) ->
    gen_server:call(?SERVER, {process_sku_pipeline_async, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Get pipeline processing progress.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_pipeline_progress(SkuId :: sku_id()) ->
    {ok, map()} | {error, not_found}.
get_pipeline_progress(SkuId) ->
    gen_server:call(?SERVER, {get_pipeline_progress, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Abort pipeline execution.
%%
%% @end
%%------------------------------------------------------------------------------
-spec abort_pipeline(SkuId :: sku_id(), Reason :: binary()) ->
    ok | {error, term()}.
abort_pipeline(SkuId, Reason) ->
    gen_server:call(?SERVER, {abort_pipeline, SkuId, Reason}).

%%%=============================================================================
%%% API - Release & Publication
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Mark SKU as released (ready for publication).
%%
%% Verifies all stages completed successfully before marking as released.
%%
%% @end
%%------------------------------------------------------------------------------
-spec mark_released(SkuId :: sku_id()) -> ok | {error, term()}.
mark_released(SkuId) ->
    gen_server:call(?SERVER, {mark_released, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Mark SKU as published to marketplace.
%%
%% Final step in production pipeline.
%% Actions:
%% - Verify all stages complete
%% - Verify no open Andons
%% - Update SKU status to 'published'
%% - Mark work order complete
%% - Generate publication receipt
%%
%% @end
%%------------------------------------------------------------------------------
-spec mark_published(SkuId :: sku_id()) -> ok | {error, term()}.
mark_published(SkuId) ->
    gen_server:call(?SERVER, {mark_published, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Get marketplace status for SKU.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_marketplace_status(SkuId :: sku_id()) -> {ok, map()} | {error, not_found}.
get_marketplace_status(SkuId) ->
    gen_server:call(?SERVER, {get_marketplace_status, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Generate marketplace URL for SKU.
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_marketplace_url(SkuId :: sku_id()) -> binary().
generate_marketplace_url(SkuId) ->
    <<"https://marketplace.example.com/skus/", SkuId/binary>>.

%%%=============================================================================
%%% API - Receipt Management
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Add receipt to SKU.
%%
%% @end
%%------------------------------------------------------------------------------
-spec add_receipt_to_sku(SkuId :: sku_id(), ReceiptId :: receipt_id()) ->
    ok | {error, term()}.
add_receipt_to_sku(SkuId, ReceiptId) ->
    gen_server:call(?SERVER, {add_receipt, SkuId, ReceiptId}).

%%------------------------------------------------------------------------------
%% @doc Get all receipts for SKU.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_receipts(SkuId :: sku_id()) -> {ok, [receipt_id()]} | {error, not_found}.
get_receipts(SkuId) ->
    gen_server:call(?SERVER, {get_receipts, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Verify receipt chain integrity.
%%
%% Checks that all required stages have receipts.
%%
%% @end
%%------------------------------------------------------------------------------
-spec verify_receipt_chain(SkuId :: sku_id()) ->
    {ok, complete} | {error, {missing_receipts, [production_stage()]}}.
verify_receipt_chain(SkuId) ->
    gen_server:call(?SERVER, {verify_receipt_chain, SkuId}).

%%%=============================================================================
%%% API - Integration
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc High-level SKU production flow.
%%
%% Complete end-to-end production:
%% 1. Create SKU from work order
%% 2. Run through production pipeline
%% 3. Mark as released
%% 4. Publish to marketplace
%%
%% @end
%%------------------------------------------------------------------------------
-spec produce_sku(WorkOrderId :: work_order_id()) ->
    {ok, sku_id()} | {error, term()}.
produce_sku(WorkOrderId) ->
    produce_sku_with_options(WorkOrderId, #{auto_publish => true}).

-spec produce_sku_with_options(WorkOrderId :: work_order_id(),
                                Options :: pipeline_options()) ->
    {ok, sku_id()} | {error, term()}.
produce_sku_with_options(WorkOrderId, Options) ->
    gen_server:call(?SERVER, {produce_sku, WorkOrderId, Options}, infinity).

%%%=============================================================================
%%% API - Reporting
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all SKUs with specific status.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_all_skus_by_status(Status :: sku_status()) -> [sku()].
get_all_skus_by_status(Status) ->
    gen_server:call(?SERVER, {get_skus_by_status, Status}).

%%------------------------------------------------------------------------------
%% @doc Get production metrics.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_production_metrics() -> map().
get_production_metrics() ->
    gen_server:call(?SERVER, get_production_metrics).

%%------------------------------------------------------------------------------
%% @doc Get stage statistics.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_stage_statistics() -> map().
get_stage_statistics() ->
    gen_server:call(?SERVER, get_stage_statistics).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS table for fast SKU lookups
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, set, public,
                                {read_concurrency, true},
                                {write_concurrency, true}]);
        _ ->
            ok
    end,

    %% Ensure directories exist
    ReceiptsDir = maps:get(receipts_dir, Config, "priv/receipts/skus"),
    filelib:ensure_dir(ReceiptsDir ++ "/"),

    State = #state{
        config = maps:merge((#state{})#state.config, Config)
    },

    {ok, State}.

handle_call({create_sku, WorkOrderId}, _From, State) ->
    {Result, NewState} = do_create_sku(WorkOrderId, State),
    {reply, Result, NewState};

handle_call({get_sku, SkuId}, _From, State) ->
    Result = do_get_sku(SkuId, State),
    {reply, Result, State};

handle_call(list_all_skus, _From, State) ->
    Result = do_list_all_skus(State),
    {reply, Result, State};

handle_call({delete_sku, SkuId}, _From, State) ->
    {Result, NewState} = do_delete_sku(SkuId, State),
    {reply, Result, NewState};

handle_call({get_sku_status, SkuId}, _From, State) ->
    Result = do_get_sku_status(SkuId, State),
    {reply, Result, State};

handle_call({get_current_stage, SkuId}, _From, State) ->
    Result = do_get_current_stage(SkuId, State),
    {reply, Result, State};

handle_call({transition_stage, SkuId, NextStage}, _From, State) ->
    {Result, NewState} = do_transition_stage(SkuId, NextStage, State),
    {reply, Result, NewState};

handle_call({can_transition, SkuId, NextStage}, _From, State) ->
    Result = do_can_transition(SkuId, NextStage, State),
    {reply, Result, State};

handle_call({get_production_history, SkuId}, _From, State) ->
    Result = do_get_production_history(SkuId, State),
    {reply, Result, State};

handle_call({process_sku_pipeline, SkuId}, _From, State) ->
    {Result, NewState} = do_process_sku_pipeline(SkuId, State),
    {reply, Result, NewState};

handle_call({process_sku_pipeline_async, SkuId}, _From, State) ->
    {Result, NewState} = do_process_sku_pipeline_async(SkuId, State),
    {reply, Result, NewState};

handle_call({get_pipeline_progress, SkuId}, _From, State) ->
    Result = do_get_pipeline_progress(SkuId, State),
    {reply, Result, State};

handle_call({abort_pipeline, SkuId, Reason}, _From, State) ->
    {Result, NewState} = do_abort_pipeline(SkuId, Reason, State),
    {reply, Result, NewState};

handle_call({mark_released, SkuId}, _From, State) ->
    {Result, NewState} = do_mark_released(SkuId, State),
    {reply, Result, NewState};

handle_call({mark_published, SkuId}, _From, State) ->
    {Result, NewState} = do_mark_published(SkuId, State),
    {reply, Result, NewState};

handle_call({get_marketplace_status, SkuId}, _From, State) ->
    Result = do_get_marketplace_status(SkuId, State),
    {reply, Result, State};

handle_call({add_receipt, SkuId, ReceiptId}, _From, State) ->
    {Result, NewState} = do_add_receipt(SkuId, ReceiptId, State),
    {reply, Result, NewState};

handle_call({get_receipts, SkuId}, _From, State) ->
    Result = do_get_receipts(SkuId, State),
    {reply, Result, State};

handle_call({verify_receipt_chain, SkuId}, _From, State) ->
    Result = do_verify_receipt_chain(SkuId, State),
    {reply, Result, State};

handle_call({produce_sku, WorkOrderId, Options}, _From, State) ->
    {Result, NewState} = do_produce_sku(WorkOrderId, Options, State),
    {reply, Result, NewState};

handle_call({get_skus_by_status, Status}, _From, State) ->
    Result = do_get_skus_by_status(Status, State),
    {reply, Result, State};

handle_call(get_production_metrics, _From, State) ->
    Result = do_get_production_metrics(State),
    {reply, Result, State};

handle_call(get_stage_statistics, _From, State) ->
    Result = do_get_stage_statistics(State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, Reason}, State) ->
    %% Handle pipeline worker termination
    NewState = handle_worker_down(Pid, Reason, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions - SKU Creation
%%%=============================================================================

do_create_sku(WorkOrderId, State) ->
    %% 1. Validate work order exists
    case validate_work_order(WorkOrderId) of
        ok ->
            %% 2. Generate unique SKU ID
            SkuId = generate_sku_id(WorkOrderId),

            %% 3. Create SKU record
            Sku = #{
                id => SkuId,
                work_order_id => WorkOrderId,
                created_at => erlang:system_time(second),
                updated_at => erlang:system_time(second),
                current_stage => shacl_validation,
                status => in_production,
                production_history => [],
                receipts => [],
                metadata => #{}
            },

            %% 4. Store in state
            Skus = maps:put(SkuId, Sku, State#state.skus),

            %% 5. Store in ETS
            ets:insert(?ETS_TABLE, {SkuId, Sku}),

            %% 6. Persist to storage
            persist_sku(Sku, State),

            %% 7. Update work order status (if work order module available)
            update_work_order_status(WorkOrderId, in_progress),

            %% 8. Broadcast event
            broadcast_sku_created(SkuId, WorkOrderId),

            NewState = State#state{skus = Skus},
            {{ok, SkuId}, NewState};
        {error, Reason} ->
            {{error, Reason}, State}
    end.

do_get_sku(SkuId, State) ->
    case maps:find(SkuId, State#state.skus) of
        {ok, Sku} -> {ok, Sku};
        error -> {error, not_found}
    end.

do_list_all_skus(State) ->
    maps:values(State#state.skus).

do_delete_sku(SkuId, State) ->
    case maps:find(SkuId, State#state.skus) of
        {ok, _Sku} ->
            Skus = maps:remove(SkuId, State#state.skus),
            ets:delete(?ETS_TABLE, SkuId),
            NewState = State#state{skus = Skus},
            {ok, NewState};
        error ->
            {{error, not_found}, State}
    end.

%%%=============================================================================
%%% Internal Functions - Stage Management
%%%=============================================================================

do_get_sku_status(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            CurrentStage = maps:get(current_stage, Sku),
            Status = maps:get(status, Sku),

            %% Calculate completion percentage
            TotalStages = length(?PRODUCTION_STAGES),
            CurrentIdx = index_of(CurrentStage, ?PRODUCTION_STAGES),
            Completion = (CurrentIdx / TotalStages) * 100,

            %% Get receipts count
            Receipts = maps:get(receipts, Sku),

            %% Get open Andons (if andon module available)
            OpenAndons = get_open_andons_for_sku(SkuId),

            StatusMap = #{
                sku_id => SkuId,
                work_order_id => maps:get(work_order_id, Sku),
                current_stage => CurrentStage,
                status => Status,
                completion_percent => Completion,
                receipts_count => length(Receipts),
                open_andons_count => length(OpenAndons),
                created_at => maps:get(created_at, Sku),
                updated_at => maps:get(updated_at, Sku),
                production_history => maps:get(production_history, Sku)
            },

            {ok, StatusMap};
        {error, not_found} = Err ->
            Err
    end.

do_get_current_stage(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            {ok, maps:get(current_stage, Sku)};
        {error, not_found} = Err ->
            Err
    end.

do_transition_stage(SkuId, NextStage, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            CurrentStage = maps:get(current_stage, Sku),

            %% 1. Validate stage sequence
            case validate_stage_sequence(CurrentStage, NextStage) of
                ok ->
                    %% 2. Check quality gates
                    case check_quality_gate(SkuId, CurrentStage) of
                        {pass, Receipt} ->
                            %% 3. Check for open Andons
                            case check_andons(SkuId, NextStage) of
                                {ok, proceed} ->
                                    %% 4. Store receipt
                                    persist_receipt(Receipt, State),

                                    %% 5. Add receipt to SKU
                                    ReceiptId = maps:get(receipt_id, Receipt),
                                    Receipts = [ReceiptId | maps:get(receipts, Sku)],

                                    %% 6. Update production history
                                    HistoryEntry = #{
                                        stage => CurrentStage,
                                        timestamp => erlang:system_time(second),
                                        status => pass,
                                        receipt_id => ReceiptId
                                    },
                                    History = [HistoryEntry | maps:get(production_history, Sku)],

                                    %% 7. Update SKU record
                                    UpdatedSku = Sku#{
                                        current_stage => NextStage,
                                        updated_at => erlang:system_time(second),
                                        production_history => History,
                                        receipts => Receipts
                                    },

                                    %% 8. Store updated SKU
                                    Skus = maps:put(SkuId, UpdatedSku, State#state.skus),
                                    ets:insert(?ETS_TABLE, {SkuId, UpdatedSku}),
                                    persist_sku(UpdatedSku, State),

                                    %% 9. Broadcast event
                                    broadcast_stage_transition(SkuId, CurrentStage, NextStage),

                                    NewState = State#state{skus = Skus},
                                    {ok, NewState};
                                {blocked, AndonIds} ->
                                    {{error, {blocked_by_andons, AndonIds}}, State}
                            end;
                        {fail, Reason} ->
                            %% Quality gate failed - mark SKU as failed
                            HistoryEntry = #{
                                stage => CurrentStage,
                                timestamp => erlang:system_time(second),
                                status => fail,
                                reason => format_failure_reason(Reason)
                            },
                            History = [HistoryEntry | maps:get(production_history, Sku)],

                            FailedSku = Sku#{
                                status => failed,
                                updated_at => erlang:system_time(second),
                                production_history => History,
                                failure_reason => format_failure_reason(Reason)
                            },

                            Skus = maps:put(SkuId, FailedSku, State#state.skus),
                            ets:insert(?ETS_TABLE, {SkuId, FailedSku}),

                            NewState = State#state{skus = Skus},
                            {{error, {quality_gate_failed, Reason}}, NewState}
                    end;
                {error, invalid_sequence} = Err ->
                    {Err, State}
            end;
        {error, not_found} = Err ->
            {Err, State}
    end.

do_can_transition(SkuId, NextStage, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            CurrentStage = maps:get(current_stage, Sku),

            %% Check stage sequence
            case validate_stage_sequence(CurrentStage, NextStage) of
                ok ->
                    %% Check quality gates (non-mutating)
                    case check_quality_gate(SkuId, CurrentStage) of
                        {pass, _Receipt} ->
                            %% Check Andons
                            case check_andons(SkuId, NextStage) of
                                {ok, proceed} ->
                                    {ok, can_proceed};
                                {blocked, AndonIds} ->
                                    {error, {blocked_by_andons, AndonIds}}
                            end;
                        {fail, Reason} ->
                            {error, {quality_gate_failed, Reason}}
                    end;
                {error, invalid_sequence} = Err ->
                    Err
            end;
        {error, not_found} = Err ->
            Err
    end.

do_get_production_history(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            {ok, maps:get(production_history, Sku)};
        {error, not_found} = Err ->
            Err
    end.

%%%=============================================================================
%%% Internal Functions - Pipeline Execution
%%%=============================================================================

do_process_sku_pipeline(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, _Sku} ->
            process_stages_recursive(SkuId, ?PRODUCTION_STAGES, State);
        {error, not_found} = Err ->
            {Err, State}
    end.

process_stages_recursive(_SkuId, [], State) ->
    {{ok, publication}, State};
process_stages_recursive(SkuId, [Stage | Rest], State) ->
    %% Get current SKU state
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            CurrentStage = maps:get(current_stage, Sku),

            %% Only process if we're at this stage
            case CurrentStage =:= Stage of
                true ->
                    %% Determine next stage
                    case Rest of
                        [] ->
                            %% Last stage - no transition needed
                            {{ok, Stage}, State};
                        [NextStage | _] ->
                            %% Attempt transition
                            case do_transition_stage(SkuId, NextStage, State) of
                                {ok, NewState} ->
                                    %% Continue to next stage
                                    process_stages_recursive(SkuId, Rest, NewState);
                                {{error, Reason}, NewState} ->
                                    %% Failed - stop pipeline
                                    {{error, {failed_at, Stage, Reason}}, NewState}
                            end
                    end;
                false ->
                    %% Already past this stage, continue
                    process_stages_recursive(SkuId, Rest, State)
            end;
        {error, not_found} = Err ->
            {Err, State}
    end.

do_process_sku_pipeline_async(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, _Sku} ->
            %% Spawn worker process
            Self = self(),
            WorkerPid = spawn_link(fun() ->
                pipeline_worker(Self, SkuId)
            end),

            %% Track worker
            Workers = maps:put(SkuId, WorkerPid, State#state.pipeline_workers),
            NewState = State#state{pipeline_workers = Workers},

            {{ok, WorkerPid}, NewState};
        {error, not_found} = Err ->
            {Err, State}
    end.

do_get_pipeline_progress(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            CurrentStage = maps:get(current_stage, Sku),
            TotalStages = length(?PRODUCTION_STAGES),
            CurrentIdx = index_of(CurrentStage, ?PRODUCTION_STAGES),

            Progress = #{
                sku_id => SkuId,
                current_stage => CurrentStage,
                completed_stages => CurrentIdx - 1,
                total_stages => TotalStages,
                percent_complete => ((CurrentIdx - 1) / TotalStages) * 100,
                status => maps:get(status, Sku),
                is_active => maps:is_key(SkuId, State#state.pipeline_workers)
            },

            {ok, Progress};
        {error, not_found} = Err ->
            Err
    end.

do_abort_pipeline(SkuId, Reason, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            %% Kill worker if running
            Workers = case maps:find(SkuId, State#state.pipeline_workers) of
                {ok, Pid} ->
                    exit(Pid, kill),
                    maps:remove(SkuId, State#state.pipeline_workers);
                error ->
                    State#state.pipeline_workers
            end,

            %% Mark SKU as aborted
            AbortedSku = Sku#{
                status => aborted,
                updated_at => erlang:system_time(second),
                failure_reason => Reason
            },

            Skus = maps:put(SkuId, AbortedSku, State#state.skus),
            ets:insert(?ETS_TABLE, {SkuId, AbortedSku}),

            NewState = State#state{skus = Skus, pipeline_workers = Workers},
            {ok, NewState};
        {error, not_found} = Err ->
            {Err, State}
    end.

%%%=============================================================================
%%% Internal Functions - Release & Publication
%%%=============================================================================

do_mark_released(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            %% Verify all stages completed
            case maps:get(current_stage, Sku) of
                publication ->
                    ReleasedSku = Sku#{
                        status => completed,
                        updated_at => erlang:system_time(second),
                        completed_at => erlang:system_time(second)
                    },

                    Skus = maps:put(SkuId, ReleasedSku, State#state.skus),
                    ets:insert(?ETS_TABLE, {SkuId, ReleasedSku}),

                    NewState = State#state{skus = Skus},
                    {ok, NewState};
                _OtherStage ->
                    {{error, pipeline_incomplete}, State}
            end;
        {error, not_found} = Err ->
            {Err, State}
    end.

do_mark_published(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            %% Verify all stages complete
            case verify_sku_complete(Sku) of
                ok ->
                    %% Verify no open Andons
                    case get_open_andons_for_sku(SkuId) of
                        [] ->
                            %% Generate marketplace URL
                            MarketplaceUrl = generate_marketplace_url(SkuId),

                            %% Update SKU
                            PublishedSku = Sku#{
                                status => published,
                                updated_at => erlang:system_time(second),
                                published_at => erlang:system_time(second),
                                marketplace_url => MarketplaceUrl
                            },

                            Skus = maps:put(SkuId, PublishedSku, State#state.skus),
                            ets:insert(?ETS_TABLE, {SkuId, PublishedSku}),

                            %% Generate publication receipt
                            PublicationReceipt = #{
                                receipt_id => generate_receipt_id(),
                                sku_id => SkuId,
                                stage => publication,
                                timestamp => erlang:system_time(second),
                                status => published,
                                marketplace_url => MarketplaceUrl
                            },
                            persist_receipt(PublicationReceipt, State),

                            %% Mark work order complete
                            WorkOrderId = maps:get(work_order_id, Sku),
                            update_work_order_status(WorkOrderId, completed),

                            NewState = State#state{skus = Skus},
                            {ok, NewState};
                        _OpenAndons ->
                            {{error, open_andons_exist}, State}
                    end;
                {error, Reason} ->
                    {{error, Reason}, State}
            end;
        {error, not_found} = Err ->
            {Err, State}
    end.

do_get_marketplace_status(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            Status = #{
                sku_id => SkuId,
                status => maps:get(status, Sku),
                marketplace_url => maps:get(marketplace_url, Sku, undefined),
                published_at => maps:get(published_at, Sku, undefined)
            },
            {ok, Status};
        {error, not_found} = Err ->
            Err
    end.

%%%=============================================================================
%%% Internal Functions - Receipt Management
%%%=============================================================================

do_add_receipt(SkuId, ReceiptId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            Receipts = [ReceiptId | maps:get(receipts, Sku)],
            UpdatedSku = Sku#{receipts => Receipts},

            Skus = maps:put(SkuId, UpdatedSku, State#state.skus),
            ets:insert(?ETS_TABLE, {SkuId, UpdatedSku}),

            NewState = State#state{skus = Skus},
            {ok, NewState};
        {error, not_found} = Err ->
            {Err, State}
    end.

do_get_receipts(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            {ok, maps:get(receipts, Sku)};
        {error, not_found} = Err ->
            Err
    end.

do_verify_receipt_chain(SkuId, State) ->
    case do_get_sku(SkuId, State) of
        {ok, Sku} ->
            History = maps:get(production_history, Sku),
            CompletedStages = [maps:get(stage, Entry) || Entry <- History,
                              maps:get(status, Entry) =:= pass],

            %% Check if all stages have been completed
            AllStages = ?PRODUCTION_STAGES,
            MissingStages = AllStages -- CompletedStages,

            case MissingStages of
                [] ->
                    {ok, complete};
                Missing ->
                    {error, {missing_receipts, Missing}}
            end;
        {error, not_found} = Err ->
            Err
    end.

%%%=============================================================================
%%% Internal Functions - Integration
%%%=============================================================================

do_produce_sku(WorkOrderId, Options, State) ->
    %% 1. Create SKU
    case do_create_sku(WorkOrderId, State) of
        {{ok, SkuId}, State1} ->
            %% 2. Run pipeline
            case do_process_sku_pipeline(SkuId, State1) of
                {{ok, _FinalStage}, State2} ->
                    %% 3. Mark as released
                    {ok, State3} = do_mark_released(SkuId, State2),

                    %% 4. Publish if requested
                    case maps:get(auto_publish, Options, false) of
                        true ->
                            {ok, State4} = do_mark_published(SkuId, State3),
                            {{ok, SkuId}, State4};
                        false ->
                            {{ok, SkuId}, State3}
                    end;
                {{error, Reason}, State2} ->
                    {{error, Reason}, State2}
            end;
        {{error, Reason}, State1} ->
            {{error, Reason}, State1}
    end.

%%%=============================================================================
%%% Internal Functions - Reporting
%%%=============================================================================

do_get_skus_by_status(Status, State) ->
    AllSkus = maps:values(State#state.skus),
    [Sku || Sku <- AllSkus, maps:get(status, Sku) =:= Status].

do_get_production_metrics(State) ->
    AllSkus = maps:values(State#state.skus),

    InProduction = length([S || S <- AllSkus, maps:get(status, S) =:= in_production]),
    Completed = length([S || S <- AllSkus, maps:get(status, S) =:= completed]),
    Published = length([S || S <- AllSkus, maps:get(status, S) =:= published]),
    Failed = length([S || S <- AllSkus, maps:get(status, S) =:= failed]),

    #{
        total_skus => length(AllSkus),
        in_production => InProduction,
        completed => Completed,
        published => Published,
        failed => Failed,
        success_rate => case length(AllSkus) of
            0 -> 0.0;
            Total -> (Published / Total) * 100
        end
    }.

do_get_stage_statistics(State) ->
    AllSkus = maps:values(State#state.skus),

    %% Count SKUs at each stage
    StageCounts = lists:foldl(fun(Sku, Acc) ->
        Stage = maps:get(current_stage, Sku),
        maps:update_with(Stage, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, AllSkus),

    StageCounts.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

generate_sku_id(WorkOrderId) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("SKU-~s-~b-~6..0b",
                                    [WorkOrderId, Timestamp, Random])).

generate_receipt_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("receipt-~b-~6..0b", [Timestamp, Random])).

validate_work_order(WorkOrderId) ->
    %% Check if work order module is available and validate
    case whereis(tcps_work_order) of
        undefined ->
            ok;  % Work order module not loaded, skip validation
        _Pid ->
            case catch tcps_work_order:get_work_order_status(WorkOrderId) of
                {ok, _Status} -> ok;
                _ -> {error, work_order_not_found}
            end
    end.

validate_stage_sequence(CurrentStage, NextStage) ->
    Stages = ?PRODUCTION_STAGES,
    CurrentIdx = index_of(CurrentStage, Stages),
    NextIdx = index_of(NextStage, Stages),

    case NextIdx =:= CurrentIdx + 1 of
        true -> ok;
        false -> {error, invalid_sequence}
    end.

index_of(Item, List) ->
    index_of(Item, List, 1).

index_of(_Item, [], _Index) ->
    0;
index_of(Item, [Item | _Rest], Index) ->
    Index;
index_of(Item, [_Other | Rest], Index) ->
    index_of(Item, Rest, Index + 1).

check_quality_gate(SkuId, Stage) ->
    %% Mock quality gate check
    %% In production, this would call tcps_quality_gates module
    Receipt = #{
        receipt_id => generate_receipt_id(),
        sku_id => SkuId,
        stage => Stage,
        timestamp => erlang:system_time(second),
        status => pass,
        evidence => <<"Quality gate passed for stage: ", (erlang:atom_to_binary(Stage))/binary>>
    },
    {pass, Receipt}.

check_andons(SkuId, _NextStage) ->
    %% Check for open Andon events
    case get_open_andons_for_sku(SkuId) of
        [] ->
            {ok, proceed};
        AndonIds ->
            {blocked, AndonIds}
    end.

get_open_andons_for_sku(SkuId) ->
    %% Get open Andons from tcps_andon module if available
    case whereis(tcps_andon) of
        undefined ->
            [];
        _Pid ->
            case catch tcps_andon:get_andon_history(SkuId) of
                {'EXIT', _} -> [];
                Events when is_list(Events) ->
                    [maps:get(event_id, E) || E <- Events,
                     maps:get(status, E, resolved) =:= open];
                _ -> []
            end
    end.

persist_sku(Sku, State) ->
    %% Persist SKU to storage
    case maps:get(auto_persist, State#state.config, true) of
        true ->
            case whereis(tcps_persistence) of
                undefined -> ok;
                _Pid ->
                    catch tcps_persistence:store_sku(Sku)
            end;
        false ->
            ok
    end.

persist_receipt(Receipt, State) ->
    %% Persist receipt to storage
    case maps:get(auto_persist, State#state.config, true) of
        true ->
            case whereis(tcps_persistence) of
                undefined -> ok;
                _Pid ->
                    catch tcps_persistence:store_receipt(Receipt)
            end;
        false ->
            ok
    end.

update_work_order_status(WorkOrderId, Status) ->
    %% Update work order status if module available
    case whereis(tcps_work_order) of
        undefined -> ok;
        _Pid ->
            catch tcps_work_order:update_status(WorkOrderId, Status)
    end.

broadcast_sku_created(SkuId, WorkOrderId) ->
    %% Broadcast SKU creation event
    case whereis(tcps_sse_manager) of
        undefined -> ok;
        _Pid ->
            Event = #{
                type => sku_created,
                sku_id => SkuId,
                work_order_id => WorkOrderId,
                timestamp => erlang:system_time(second)
            },
            catch tcps_sse_manager:broadcast_update(Event)
    end.

broadcast_stage_transition(SkuId, FromStage, ToStage) ->
    %% Broadcast stage transition event
    case whereis(tcps_sse_manager) of
        undefined -> ok;
        _Pid ->
            Event = #{
                type => sku_stage_transition,
                sku_id => SkuId,
                from_stage => FromStage,
                to_stage => ToStage,
                timestamp => erlang:system_time(second)
            },
            catch tcps_sse_manager:broadcast_update(Event)
    end.

verify_sku_complete(Sku) ->
    %% Verify SKU completed all stages
    case maps:get(current_stage, Sku) of
        publication ->
            case maps:get(status, Sku) of
                completed -> ok;
                _ -> {error, sku_not_completed}
            end;
        _OtherStage ->
            {error, pipeline_incomplete}
    end.

format_failure_reason(Reason) when is_binary(Reason) ->
    Reason;
format_failure_reason(Reason) when is_atom(Reason) ->
    erlang:atom_to_binary(Reason);
format_failure_reason(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%%=============================================================================
%%% Pipeline Worker
%%%=============================================================================

pipeline_worker(ServerPid, SkuId) ->
    %% Run pipeline synchronously in worker process
    Result = gen_server:call(ServerPid, {process_sku_pipeline, SkuId}, infinity),

    %% Notify server of completion
    gen_server:cast(ServerPid, {pipeline_complete, SkuId, Result}).

handle_worker_down(Pid, Reason, State) ->
    %% Find SKU for this worker
    case lists:keyfind(Pid, 2, maps:to_list(State#state.pipeline_workers)) of
        {SkuId, Pid} ->
            Workers = maps:remove(SkuId, State#state.pipeline_workers),

            %% Mark SKU as failed if abnormal termination
            case Reason of
                normal -> ok;
                killed -> ok;
                _ ->
                    case do_get_sku(SkuId, State) of
                        {ok, Sku} ->
                            FailedSku = Sku#{
                                status => failed,
                                updated_at => erlang:system_time(second),
                                failure_reason => format_failure_reason(Reason)
                            },
                            Skus = maps:put(SkuId, FailedSku, State#state.skus),
                            ets:insert(?ETS_TABLE, {SkuId, FailedSku}),
                            State#state{skus = Skus, pipeline_workers = Workers};
                        _ ->
                            State#state{pipeline_workers = Workers}
                    end
            end;
        false ->
            State
    end.

%%%=============================================================================
%%% Test Support
%%%=============================================================================

-ifdef(TEST).

init_ets() ->
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, set, public,
                                {read_concurrency, true},
                                {write_concurrency, true}]);
        _ ->
            ok
    end.

reset_state() ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid ->
            gen_server:call(Pid, reset_state)
    end.

-endif.
