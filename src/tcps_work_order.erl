%%%-----------------------------------------------------------------------------
%%% @doc TCPS Work Order Management System with Pull Signal Processing
%%%
%%% Production-grade work order lifecycle management implementing Toyota
%%% Production System principles for demand-driven software development.
%%%
%%% Core Responsibilities:
%%% - Work order creation from multiple pull signals (GitHub, CVE, marketplace)
%%% - Complete lifecycle management (start, progress, complete, cancel)
%%% - Pull signal routing to appropriate buckets (reliability, security, etc.)
%%% - Queue management with priority-based scheduling
%%% - SLA tracking and breach detection
%%% - Dependency management with blocking
%%% - Comprehensive reporting and analytics
%%% - RDF ontology persistence with receipts
%%%
%%% Pull Signal Sources:
%%% - Marketplace: install/refund events (demand signal)
%%% - GitHub: issues/PRs (feature requests, bugs)
%%% - Security: CVE advisories (vulnerabilities)
%%% - Internal: technical debt, refactoring
%%%
%%% Bucket Assignment:
%%% - security: CVE, security advisories (priority 8-10)
%%% - reliability: bugs, production issues (priority 5-9)
%%% - cost: performance, resource optimization (priority 3-7)
%%% - compliance: legal, regulatory (priority 4-8)
%%% - features: enhancements, new functionality (priority 2-6)
%%%
%%% SLA Targets:
%%% - Security (CVE): 24 hours
%%% - Production bugs: 7 days
%%% - Features: 30 days
%%% - Technical debt: best effort
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_work_order).
-behaviour(gen_server).

%% API exports - Work Order Creation
-export([
    create_work_order/1,
    create_from_github/1,
    create_from_security_advisory/1,
    create_from_marketplace/1
]).

%% API exports - Work Order Lifecycle
-export([
    start_work_order/1,
    progress_work_order/2,
    complete_work_order/2,
    cancel_work_order/2
]).

%% API exports - Pull Signal Routing
-export([
    route_pull_signal/1,
    prioritize_signals/1
]).

%% API exports - Queue Management
-export([
    get_queue/1,
    dequeue_next/1,
    reorder_queue/2,
    list_by_status/1,
    list_by_bucket/1,
    process_next/0,
    list_all/0,
    delete/1
]).

%% API exports - SLA Tracking
-export([
    check_sla/1,
    get_sla_breaches/0,
    get_sla_warnings/1
]).

%% API exports - Dependency Management
-export([
    add_dependency/2,
    get_dependencies/1,
    resolve_dependency/1
]).

%% API exports - Reporting
-export([
    get_work_order_status/1,
    generate_work_order_report/1,
    get_metrics/1
]).

%% API exports - Persistence
-export([
    save_to_ontology/1,
    load_from_ontology/1,
    export_to_json/1
]).

%% gen_server exports
-export([
    start_link/0,
    start_link/1,
    stop/0
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Test support
-ifdef(TEST).
-export([init/0, reset_state/0]).
-on_load(init/0).
-endif.

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type work_order_id() :: binary().
-type sku_id() :: binary().
-type bucket() :: reliability | security | cost | compliance | features | technical_debt.
-type priority() :: 1..10.  % 1=lowest, 10=critical
-type stage() :: requirements | design | implementation | testing |
                 integration | deployment | published.
-type work_order_status() :: pending | queued | in_progress | blocked |
                             completed | cancelled.

-type pull_signal() :: #{
    type := github_issue | github_pr | cve | marketplace_install |
            marketplace_refund | internal_request,
    source := binary(),
    description := binary(),
    labels => [binary()],
    metadata => map()
}.

-type work_order() :: #{
    id := work_order_id(),
    bucket := bucket(),
    priority := priority(),
    status := work_order_status(),
    description := binary(),
    pull_signal := pull_signal(),
    created_at := erlang:timestamp(),
    started_at => erlang:timestamp(),
    completed_at => erlang:timestamp(),
    cancelled_at => erlang:timestamp(),
    sla_deadline := erlang:timestamp(),
    current_stage => stage(),
    stages_completed => [stage()],
    receipts => [binary()],
    sku_id => sku_id(),
    dependencies => [work_order_id()],
    blocked_by => [work_order_id()],
    cancellation_reason => binary(),
    metadata => map()
}.

-type sla_status() :: on_time | warning | breached.
-type sla_check() :: #{
    work_order_id := work_order_id(),
    status := sla_status(),
    deadline := erlang:timestamp(),
    remaining_hours => float(),
    overdue_hours => float()
}.

-type dependency_graph() :: #{
    blocking := [work_order_id()],
    blocked_by := [work_order_id()]
}.

-type work_order_report() :: #{
    time_period := {calendar:date(), calendar:date()},
    total_work_orders := non_neg_integer(),
    by_bucket := #{bucket() => non_neg_integer()},
    by_status := #{work_order_status() => non_neg_integer()},
    completed := non_neg_integer(),
    cancelled := non_neg_integer(),
    average_lead_time := float(),
    sla_compliance_rate := float(),
    top_priorities := [work_order()]
}.

-export_type([
    work_order_id/0,
    work_order/0,
    pull_signal/0,
    bucket/0,
    priority/0,
    stage/0,
    work_order_status/0,
    sla_status/0,
    sla_check/0,
    dependency_graph/0,
    work_order_report/0
]).

%%%=============================================================================
%%% State Record
%%%=============================================================================

-record(state, {
    %% Work orders by ID
    work_orders = #{} :: #{work_order_id() => work_order()},

    %% Queues by bucket (priority-ordered)
    queues = #{
        reliability => [],
        security => [],
        cost => [],
        compliance => [],
        features => [],
        technical_debt => []
    } :: #{bucket() => [work_order_id()]},

    %% Active work orders (in progress)
    active = #{} :: #{bucket() => [work_order_id()]},

    %% Dependency graph
    dependencies = #{} :: #{work_order_id() => [work_order_id()]},

    %% Reverse dependency lookup
    blocked_by = #{} :: #{work_order_id() => [work_order_id()]},

    %% Receipt storage
    receipts_dir = "priv/receipts/work_orders" :: string(),

    %% Ontology file
    ontology_file = "ontology/work_orders.ttl" :: string(),

    %% Configuration
    config = #{
        sla_hours => #{
            security => 24,
            reliability => 168,  % 7 days
            features => 720,     % 30 days
            cost => 720,
            compliance => 168,
            technical_debt => infinity
        },
        wip_limits => #{
            security => 5,
            reliability => 5,
            cost => 5,
            compliance => 5,
            features => 10,
            technical_debt => 5
        }
    } :: map()
}).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, tcps_work_orders).

%%%=============================================================================
%%% API - Work Order Creation
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a work order from a pull signal.
%%
%% Automatically assigns:
%% - Bucket (based on signal type and labels)
%% - Priority (1-10 based on signal urgency)
%% - SLA deadline
%% - Required stages
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_work_order(PullSignal :: pull_signal()) ->
    {ok, work_order_id()} | {error, term()}.
create_work_order(PullSignal) ->
    gen_server:call(?SERVER, {create_work_order, PullSignal}).

%%------------------------------------------------------------------------------
%% @doc Create work order from GitHub issue URL.
%%
%% Parses GitHub issue to extract:
%% - Title → work order description
%% - Labels → bucket assignment
%% - Milestone → deadline
%% - Comments → context metadata
%%
%% Example: create_from_github(<<"https://github.com/org/repo/issues/123">>)
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_from_github(IssueUrl :: binary()) ->
    {ok, work_order_id()} | {error, term()}.
create_from_github(IssueUrl) ->
    gen_server:call(?SERVER, {create_from_github, IssueUrl}).

%%------------------------------------------------------------------------------
%% @doc Create urgent security work order from CVE advisory.
%%
%% Auto-creates with:
%% - Bucket: security
%% - Priority: 10 (critical)
%% - SLA: 24 hours
%% - Auto-notify team via Andon
%%
%% Example: create_from_security_advisory(<<"CVE-2026-1234">>)
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_from_security_advisory(CVE :: binary()) ->
    {ok, work_order_id()}.
create_from_security_advisory(CVE) ->
    gen_server:call(?SERVER, {create_from_security_advisory, CVE}).

%%------------------------------------------------------------------------------
%% @doc Create work order from marketplace event (install/refund).
%%
%% Marketplace events represent demand signals:
%% - Install: potential feature request or integration need
%% - Refund: quality issue requiring investigation
%%
%% @end
%%------------------------------------------------------------------------------
-spec create_from_marketplace(Event :: map()) ->
    {ok, work_order_id()} | {error, term()}.
create_from_marketplace(Event) ->
    gen_server:call(?SERVER, {create_from_marketplace, Event}).

%%%=============================================================================
%%% API - Work Order Lifecycle
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start work order execution.
%%
%% Checks Kanban WIP limit before starting.
%% If limit reached, work order remains queued.
%% If available, marks as in_progress and generates start receipt.
%%
%% @end
%%------------------------------------------------------------------------------
-spec start_work_order(WorkOrderId :: work_order_id()) ->
    ok | {error, wip_limit | not_found | already_started}.
start_work_order(WorkOrderId) ->
    gen_server:call(?SERVER, {start_work_order, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Update work order progress to new stage.
%%
%% Generates stage completion receipt.
%% Checks quality gates for stage.
%% Updates current_stage and stages_completed.
%%
%% @end
%%------------------------------------------------------------------------------
-spec progress_work_order(WorkOrderId :: work_order_id(), Stage :: stage()) ->
    ok | {error, term()}.
progress_work_order(WorkOrderId, Stage) ->
    gen_server:call(?SERVER, {progress_work_order, WorkOrderId, Stage}).

%%------------------------------------------------------------------------------
%% @doc Complete work order and link to published SKU.
%%
%% Actions:
%% - Mark status as completed
%% - Link to published SKU
%% - Generate completion receipt
%% - Update Kanban (free WIP slot)
%% - Update Kaizen metrics
%% - Save to ontology
%%
%% @end
%%------------------------------------------------------------------------------
-spec complete_work_order(WorkOrderId :: work_order_id(), SkuId :: sku_id()) ->
    ok | {error, term()}.
complete_work_order(WorkOrderId, SkuId) ->
    gen_server:call(?SERVER, {complete_work_order, WorkOrderId, SkuId}).

%%------------------------------------------------------------------------------
%% @doc Cancel work order with reason.
%%
%% Actions:
%% - Mark status as cancelled
%% - Record cancellation reason
%% - Generate cancellation receipt
%% - Free Kanban WIP slot
%% - Unblock dependent work orders
%%
%% @end
%%------------------------------------------------------------------------------
-spec cancel_work_order(WorkOrderId :: work_order_id(), Reason :: binary()) ->
    ok | {error, term()}.
cancel_work_order(WorkOrderId, Reason) ->
    gen_server:call(?SERVER, {cancel_work_order, WorkOrderId, Reason}).

%%%=============================================================================
%%% API - Pull Signal Routing
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Route pull signal to appropriate bucket.
%%
%% Routing rules:
%% - "CVE-" prefix → security bucket (priority 10)
%% - "bug" label → reliability bucket (priority 7-9)
%% - "enhancement" → features bucket (priority 3-5)
%% - "compliance" label → compliance bucket (priority 6-8)
%% - "refactor" → technical debt bucket (priority 2-4)
%%
%% Returns {ok, WorkOrderId} or {rejected, Reason}
%%
%% @end
%%------------------------------------------------------------------------------
-spec route_pull_signal(Signal :: pull_signal()) ->
    {ok, work_order_id()} | {rejected, term()}.
route_pull_signal(Signal) ->
    gen_server:call(?SERVER, {route_pull_signal, Signal}).

%%------------------------------------------------------------------------------
%% @doc Prioritize list of signals.
%%
%% Priority order:
%% 1. Security (CVE) - always highest (priority 10)
%% 2. Production bugs - high (priority 8-9)
%% 3. Features - medium (priority 4-6)
%% 4. Technical debt - low (priority 2-3)
%%
%% @end
%%------------------------------------------------------------------------------
-spec prioritize_signals(Signals :: [pull_signal()]) -> [pull_signal()].
prioritize_signals(Signals) ->
    Scored = lists:map(fun(Signal) ->
        Priority = calculate_signal_priority(Signal),
        {Priority, Signal}
    end, Signals),

    Sorted = lists:reverse(lists:keysort(1, Scored)),
    [Signal || {_Priority, Signal} <- Sorted].

%%%=============================================================================
%%% API - Queue Management
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get pending work orders for bucket, sorted by priority.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_queue(Bucket :: bucket()) -> [work_order()].
get_queue(Bucket) ->
    gen_server:call(?SERVER, {get_queue, Bucket}).

%%------------------------------------------------------------------------------
%% @doc Dequeue next highest priority work order.
%%
%% Checks WIP limit before dequeuing.
%% If available, removes from queue and starts work order.
%% If WIP limit reached, returns {error, wip_limit}.
%%
%% @end
%%------------------------------------------------------------------------------
-spec dequeue_next(Bucket :: bucket()) ->
    {ok, work_order()} | {empty} | {error, wip_limit}.
dequeue_next(Bucket) ->
    gen_server:call(?SERVER, {dequeue_next, Bucket}).

%%------------------------------------------------------------------------------
%% @doc Manually reorder queue (emergency reprioritization).
%%
%% @end
%%------------------------------------------------------------------------------
-spec reorder_queue(Bucket :: bucket(), NewOrder :: [work_order_id()]) ->
    ok | {error, term()}.
reorder_queue(Bucket, NewOrder) ->
    gen_server:call(?SERVER, {reorder_queue, Bucket, NewOrder}).

%%------------------------------------------------------------------------------
%% @doc Get all work orders with a specific status.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_by_status(Status :: work_order_status()) -> [work_order()].
list_by_status(Status) ->
    gen_server:call(?SERVER, {list_by_status, Status}).

%%------------------------------------------------------------------------------
%% @doc Get all work orders in a specific bucket.
%%
%% @end
%%------------------------------------------------------------------------------
-spec list_by_bucket(Bucket :: bucket()) -> [work_order()].
list_by_bucket(Bucket) ->
    gen_server:call(?SERVER, {list_by_bucket, Bucket}).

%%------------------------------------------------------------------------------
%% @doc Process next work order using Heijunka leveling.
%%
%% Uses tcps_heijunka to select the next work order that best balances
%% the production schedule across buckets.
%%
%% @end
%%------------------------------------------------------------------------------
-spec process_next() -> {ok, work_order()} | {empty} | {error, term()}.
process_next() ->
    gen_server:call(?SERVER, process_next).

%%%=============================================================================
%%% API - SLA Tracking
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Check SLA status for work order.
%%
%% Returns:
%% - {ok, on_time} - within SLA
%% - {warning, Hours} - approaching SLA (< 25% time remaining)
%% - {breached, Hours} - past SLA deadline
%%
%% @end
%%------------------------------------------------------------------------------
-spec check_sla(WorkOrderId :: work_order_id()) ->
    {ok, on_time} | {warning, float()} | {breached, float()} | {error, not_found}.
check_sla(WorkOrderId) ->
    gen_server:call(?SERVER, {check_sla, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Get all work orders past SLA deadline.
%%
%% Triggers Andon if critical SLA breached.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_sla_breaches() -> [sla_check()].
get_sla_breaches() ->
    gen_server:call(?SERVER, get_sla_breaches).

%%------------------------------------------------------------------------------
%% @doc Get work orders approaching SLA deadline.
%%
%% Warning threshold: < 25% time remaining
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_sla_warnings(WarningHours :: float()) -> [sla_check()].
get_sla_warnings(WarningHours) ->
    gen_server:call(?SERVER, {get_sla_warnings, WarningHours}).

%%%=============================================================================
%%% API - Dependency Management
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Add dependency between work orders.
%%
%% WorkOrderId depends on (is blocked by) DependsOn.
%% Work order cannot start until dependency is completed.
%%
%% @end
%%------------------------------------------------------------------------------
-spec add_dependency(WorkOrderId :: work_order_id(),
                     DependsOn :: work_order_id()) -> ok | {error, term()}.
add_dependency(WorkOrderId, DependsOn) ->
    gen_server:call(?SERVER, {add_dependency, WorkOrderId, DependsOn}).

%%------------------------------------------------------------------------------
%% @doc Get dependency graph for work order.
%%
%% Returns:
%% - blocking: work orders that depend on this one
%% - blocked_by: work orders blocking this one
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_dependencies(WorkOrderId :: work_order_id()) ->
    {ok, dependency_graph()} | {error, not_found}.
get_dependencies(WorkOrderId) ->
    gen_server:call(?SERVER, {get_dependencies, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Resolve dependency (mark dependency as completed).
%%
%% When work order completes, unblocks all dependent work orders.
%%
%% @end
%%------------------------------------------------------------------------------
-spec resolve_dependency(WorkOrderId :: work_order_id()) -> ok.
resolve_dependency(WorkOrderId) ->
    gen_server:call(?SERVER, {resolve_dependency, WorkOrderId}).

%%%=============================================================================
%%% API - Reporting
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get complete status for work order.
%%
%% Returns:
%% - Current stage and status
%% - Elapsed time
%% - SLA status
%% - Receipts generated
%% - Any Andons triggered
%% - Dependencies
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_work_order_status(WorkOrderId :: work_order_id()) ->
    {ok, map()} | {error, not_found}.
get_work_order_status(WorkOrderId) ->
    gen_server:call(?SERVER, {get_work_order_status, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Generate comprehensive work order report.
%%
%% Report includes:
%% - Total work orders created
%% - Breakdown by bucket
%% - Completed vs cancelled
%% - Average lead time
%% - SLA compliance rate
%% - Top priority items
%%
%% @end
%%------------------------------------------------------------------------------
-spec generate_work_order_report(TimePeriod :: {calendar:date(), calendar:date()}) ->
    work_order_report().
generate_work_order_report(TimePeriod) ->
    gen_server:call(?SERVER, {generate_report, TimePeriod}).

%%------------------------------------------------------------------------------
%% @doc Get metrics for time period.
%%
%% @end
%%------------------------------------------------------------------------------
-spec get_metrics(TimePeriod :: {calendar:date(), calendar:date()}) -> map().
get_metrics(TimePeriod) ->
    gen_server:call(?SERVER, {get_metrics, TimePeriod}).

%%%=============================================================================
%%% API - Persistence
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Save work order to RDF ontology.
%%
%% @end
%%------------------------------------------------------------------------------
-spec save_to_ontology(WorkOrderId :: work_order_id()) -> ok | {error, term()}.
save_to_ontology(WorkOrderId) ->
    gen_server:call(?SERVER, {save_to_ontology, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Load work order from RDF ontology.
%%
%% @end
%%------------------------------------------------------------------------------
-spec load_from_ontology(WorkOrderId :: work_order_id()) ->
    {ok, work_order()} | {error, not_found}.
load_from_ontology(WorkOrderId) ->
    gen_server:call(?SERVER, {load_from_ontology, WorkOrderId}).

%%------------------------------------------------------------------------------
%% @doc Export work order to JSON for backup/reporting.
%%
%% @end
%%------------------------------------------------------------------------------
-spec export_to_json(WorkOrderId :: work_order_id()) ->
    {ok, binary()} | {error, term()}.
export_to_json(WorkOrderId) ->
    gen_server:call(?SERVER, {export_to_json, WorkOrderId}).

%%%=============================================================================
%%% gen_server API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

init(Config) ->
    process_flag(trap_exit, true),

    %% Create ETS table for fast lookups
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, set, public, {read_concurrency, true}]);
        _ ->
            ok
    end,

    %% Ensure directories exist
    ReceiptsDir = maps:get(receipts_dir, Config, "priv/receipts/work_orders"),
    OntologyFile = maps:get(ontology_file, Config, "ontology/work_orders.ttl"),

    filelib:ensure_dir(ReceiptsDir ++ "/"),
    filelib:ensure_dir(OntologyFile),

    State = #state{
        receipts_dir = ReceiptsDir,
        ontology_file = OntologyFile,
        config = maps:merge((#state{})#state.config,
                           maps:get(config, Config, #{}))
    },

    {ok, State}.

handle_call({create_work_order, PullSignal}, _From, State) ->
    {Result, NewState} = do_create_work_order(PullSignal, State),
    {reply, Result, NewState};

handle_call({create_from_github, IssueUrl}, _From, State) ->
    PullSignal = parse_github_issue(IssueUrl),
    {Result, NewState} = do_create_work_order(PullSignal, State),
    {reply, Result, NewState};

handle_call({create_from_security_advisory, CVE}, _From, State) ->
    PullSignal = #{
        type => cve,
        source => CVE,
        description => <<"Security advisory: ", CVE/binary>>,
        labels => [<<"security">>, <<"critical">>],
        metadata => #{cve => CVE, urgency => critical}
    },
    {Result, NewState} = do_create_work_order(PullSignal, State),

    %% Trigger Andon for critical security issue
    case Result of
        {ok, WorkOrderId} ->
            trigger_security_andon(WorkOrderId, CVE);
        _ ->
            ok
    end,

    {reply, Result, NewState};

handle_call({create_from_marketplace, Event}, _From, State) ->
    PullSignal = parse_marketplace_event(Event),
    {Result, NewState} = do_create_work_order(PullSignal, State),
    {reply, Result, NewState};

handle_call({start_work_order, WorkOrderId}, _From, State) ->
    {Result, NewState} = do_start_work_order(WorkOrderId, State),
    {reply, Result, NewState};

handle_call({progress_work_order, WorkOrderId, Stage}, _From, State) ->
    {Result, NewState} = do_progress_work_order(WorkOrderId, Stage, State),
    {reply, Result, NewState};

handle_call({complete_work_order, WorkOrderId, SkuId}, _From, State) ->
    {Result, NewState} = do_complete_work_order(WorkOrderId, SkuId, State),
    {reply, Result, NewState};

handle_call({cancel_work_order, WorkOrderId, Reason}, _From, State) ->
    {Result, NewState} = do_cancel_work_order(WorkOrderId, Reason, State),
    {reply, Result, NewState};

handle_call({route_pull_signal, Signal}, _From, State) ->
    {Result, NewState} = do_route_pull_signal(Signal, State),
    {reply, Result, NewState};

handle_call({get_queue, Bucket}, _From, State) ->
    Result = do_get_queue(Bucket, State),
    {reply, Result, State};

handle_call({dequeue_next, Bucket}, _From, State) ->
    {Result, NewState} = do_dequeue_next(Bucket, State),
    {reply, Result, NewState};

handle_call({reorder_queue, Bucket, NewOrder}, _From, State) ->
    {Result, NewState} = do_reorder_queue(Bucket, NewOrder, State),
    {reply, Result, NewState};

handle_call({check_sla, WorkOrderId}, _From, State) ->
    Result = do_check_sla(WorkOrderId, State),
    {reply, Result, State};

handle_call(get_sla_breaches, _From, State) ->
    Result = do_get_sla_breaches(State),
    {reply, Result, State};

handle_call({get_sla_warnings, WarningHours}, _From, State) ->
    Result = do_get_sla_warnings(WarningHours, State),
    {reply, Result, State};

handle_call({add_dependency, WorkOrderId, DependsOn}, _From, State) ->
    {Result, NewState} = do_add_dependency(WorkOrderId, DependsOn, State),
    {reply, Result, NewState};

handle_call({get_dependencies, WorkOrderId}, _From, State) ->
    Result = do_get_dependencies(WorkOrderId, State),
    {reply, Result, State};

handle_call({resolve_dependency, WorkOrderId}, _From, State) ->
    NewState = do_resolve_dependency(WorkOrderId, State),
    {reply, ok, NewState};

handle_call({get_work_order_status, WorkOrderId}, _From, State) ->
    Result = do_get_work_order_status(WorkOrderId, State),
    {reply, Result, State};

handle_call({generate_report, TimePeriod}, _From, State) ->
    Result = do_generate_report(TimePeriod, State),
    {reply, Result, State};

handle_call({get_metrics, TimePeriod}, _From, State) ->
    Result = do_get_metrics(TimePeriod, State),
    {reply, Result, State};

handle_call({save_to_ontology, WorkOrderId}, _From, State) ->
    Result = do_save_to_ontology(WorkOrderId, State),
    {reply, Result, State};

handle_call({load_from_ontology, WorkOrderId}, _From, State) ->
    Result = do_load_from_ontology(WorkOrderId, State),
    {reply, Result, State};

handle_call({export_to_json, WorkOrderId}, _From, State) ->
    Result = do_export_to_json(WorkOrderId, State),
    {reply, Result, State};

handle_call({list_by_status, Status}, _From, State) ->
    Result = do_list_by_status(Status, State),
    {reply, Result, State};

handle_call({list_by_bucket, Bucket}, _From, State) ->
    Result = do_list_by_bucket(Bucket, State),
    {reply, Result, State};

handle_call(process_next, _From, State) ->
    {Result, NewState} = do_process_next(State),
    {reply, Result, NewState};

handle_call(list_all, _From, State) ->
    Result = maps:values(State#state.work_orders),
    {reply, Result, State};

handle_call({delete, WorkOrderId}, _From, State) ->
    case maps:is_key(WorkOrderId, State#state.work_orders) of
        true ->
            ets:delete(?ETS_TABLE, WorkOrderId),
            NewWorkOrders = maps:remove(WorkOrderId, State#state.work_orders),
            NewState = State#state{work_orders = NewWorkOrders},
            {reply, ok, NewState};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions - Work Order Creation
%%%=============================================================================

do_create_work_order(PullSignal, State) ->
    %% Generate unique ID
    WorkOrderId = generate_work_order_id(),

    %% Determine bucket and priority
    Bucket = determine_bucket(PullSignal),
    Priority = calculate_priority(PullSignal, Bucket),

    %% Calculate SLA deadline
    SlaHours = get_sla_hours(Bucket, State),
    SlaDeadline = calculate_deadline(SlaHours),

    %% Create work order
    WorkOrder = #{
        id => WorkOrderId,
        bucket => Bucket,
        priority => Priority,
        status => queued,
        description => maps:get(description, PullSignal),
        pull_signal => PullSignal,
        created_at => erlang:timestamp(),
        sla_deadline => SlaDeadline,
        stages_completed => [],
        receipts => [],
        dependencies => [],
        metadata => maps:get(metadata, PullSignal, #{})
    },

    %% Store work order
    WorkOrders = maps:put(WorkOrderId, WorkOrder, State#state.work_orders),

    %% Add to bucket queue (sorted by priority)
    Queues = State#state.queues,
    BucketQueue = maps:get(Bucket, Queues, []),
    NewQueue = insert_by_priority(WorkOrderId, Priority, BucketQueue, WorkOrders),
    NewQueues = maps:put(Bucket, NewQueue, Queues),

    %% Store in ETS
    ets:insert(?ETS_TABLE, {WorkOrderId, WorkOrder}),

    %% Generate creation receipt
    generate_creation_receipt(WorkOrder, State),

    NewState = State#state{
        work_orders = WorkOrders,
        queues = NewQueues
    },

    {{ok, WorkOrderId}, NewState}.

%%%=============================================================================
%%% Internal Functions - Work Order Lifecycle
%%%=============================================================================

do_start_work_order(WorkOrderId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            Bucket = maps:get(bucket, WorkOrder),

            %% Check WIP limit
            case check_wip_available(Bucket, State) of
                true ->
                    %% Check dependencies
                    case check_dependencies_resolved(WorkOrderId, State) of
                        true ->
                            %% Update work order status
                            UpdatedWorkOrder = WorkOrder#{
                                status => in_progress,
                                started_at => erlang:timestamp()
                            },

                            %% Update state
                            WorkOrders = maps:put(WorkOrderId, UpdatedWorkOrder,
                                                 State#state.work_orders),

                            %% Remove from queue
                            Queues = State#state.queues,
                            BucketQueue = maps:get(Bucket, Queues),
                            NewQueue = lists:delete(WorkOrderId, BucketQueue),
                            NewQueues = maps:put(Bucket, NewQueue, Queues),

                            %% Add to active
                            Active = State#state.active,
                            BucketActive = maps:get(Bucket, Active, []),
                            NewActive = maps:put(Bucket, [WorkOrderId | BucketActive], Active),

                            %% Update ETS
                            ets:insert(?ETS_TABLE, {WorkOrderId, UpdatedWorkOrder}),

                            %% Generate start receipt
                            generate_start_receipt(UpdatedWorkOrder, State),

                            %% Integrate with Kanban
                            notify_kanban_start(Bucket, WorkOrderId),

                            NewState = State#state{
                                work_orders = WorkOrders,
                                queues = NewQueues,
                                active = NewActive
                            },

                            {ok, NewState};
                        false ->
                            {{error, blocked_by_dependencies}, State}
                    end;
                false ->
                    {{error, wip_limit}, State}
            end;
        error ->
            {{error, not_found}, State}
    end.

do_progress_work_order(WorkOrderId, Stage, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            %% Update stages completed
            StagesCompleted = maps:get(stages_completed, WorkOrder, []),
            NewStagesCompleted = [Stage | StagesCompleted],

            UpdatedWorkOrder = WorkOrder#{
                current_stage => Stage,
                stages_completed => NewStagesCompleted
            },

            WorkOrders = maps:put(WorkOrderId, UpdatedWorkOrder, State#state.work_orders),
            ets:insert(?ETS_TABLE, {WorkOrderId, UpdatedWorkOrder}),

            %% Generate stage receipt
            generate_stage_receipt(UpdatedWorkOrder, Stage, State),

            NewState = State#state{work_orders = WorkOrders},
            {ok, NewState};
        error ->
            {{error, not_found}, State}
    end.

do_complete_work_order(WorkOrderId, SkuId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            Bucket = maps:get(bucket, WorkOrder),

            %% Update work order
            UpdatedWorkOrder = WorkOrder#{
                status => completed,
                completed_at => erlang:timestamp(),
                sku_id => SkuId,
                current_stage => published
            },

            WorkOrders = maps:put(WorkOrderId, UpdatedWorkOrder, State#state.work_orders),
            ets:insert(?ETS_TABLE, {WorkOrderId, UpdatedWorkOrder}),

            %% Remove from active
            Active = State#state.active,
            BucketActive = maps:get(Bucket, Active, []),
            NewActive = maps:put(Bucket, lists:delete(WorkOrderId, BucketActive), Active),

            %% Generate completion receipt
            generate_completion_receipt(UpdatedWorkOrder, State),

            %% Free Kanban WIP slot
            notify_kanban_complete(Bucket, WorkOrderId),

            %% Resolve dependencies (unblock dependent work orders)
            do_resolve_dependency(WorkOrderId, State#state{
                work_orders = WorkOrders,
                active = NewActive
            }),

            %% Update Kaizen metrics
            notify_kaizen_completion(UpdatedWorkOrder),

            %% Save to ontology
            do_save_to_ontology(WorkOrderId, State#state{
                work_orders = WorkOrders,
                active = NewActive
            }),

            NewState = State#state{
                work_orders = WorkOrders,
                active = NewActive
            },

            {ok, NewState};
        error ->
            {{error, not_found}, State}
    end.

do_cancel_work_order(WorkOrderId, Reason, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            Bucket = maps:get(bucket, WorkOrder),
            Status = maps:get(status, WorkOrder),

            %% Update work order
            UpdatedWorkOrder = WorkOrder#{
                status => cancelled,
                cancelled_at => erlang:timestamp(),
                cancellation_reason => Reason
            },

            WorkOrders = maps:put(WorkOrderId, UpdatedWorkOrder, State#state.work_orders),
            ets:insert(?ETS_TABLE, {WorkOrderId, UpdatedWorkOrder}),

            %% Remove from queue or active
            {NewQueues, NewActive} = case Status of
                queued ->
                    Queues = State#state.queues,
                    BucketQueue = maps:get(Bucket, Queues),
                    {maps:put(Bucket, lists:delete(WorkOrderId, BucketQueue), Queues),
                     State#state.active};
                in_progress ->
                    Active = State#state.active,
                    BucketActive = maps:get(Bucket, Active, []),
                    {State#state.queues,
                     maps:put(Bucket, lists:delete(WorkOrderId, BucketActive), Active)}
            end,

            %% Generate cancellation receipt
            generate_cancellation_receipt(UpdatedWorkOrder, Reason, State),

            %% Free Kanban WIP slot if was in progress
            case Status of
                in_progress ->
                    notify_kanban_complete(Bucket, WorkOrderId);
                _ ->
                    ok
            end,

            NewState = State#state{
                work_orders = WorkOrders,
                queues = NewQueues,
                active = NewActive
            },

            {ok, NewState};
        error ->
            {{error, not_found}, State}
    end.

%%%=============================================================================
%%% Internal Functions - Pull Signal Routing
%%%=============================================================================

do_route_pull_signal(Signal, State) ->
    %% Check if signal should be accepted
    case should_accept_signal(Signal) of
        true ->
            do_create_work_order(Signal, State);
        {false, Reason} ->
            {{rejected, Reason}, State}
    end.

determine_bucket(#{type := cve}) -> security;
determine_bucket(#{type := github_issue, labels := Labels}) ->
    case lists:member(<<"security">>, Labels) of
        true -> security;
        false ->
            case lists:member(<<"bug">>, Labels) of
                true -> reliability;
                false ->
                    case lists:member(<<"compliance">>, Labels) of
                        true -> compliance;
                        false ->
                            case lists:member(<<"refactor">>, Labels) of
                                true -> technical_debt;
                                false ->
                                    case lists:member(<<"performance">>, Labels) of
                                        true -> cost;
                                        false -> features
                                    end
                            end
                    end
            end
    end;
determine_bucket(#{type := marketplace_refund}) -> reliability;
determine_bucket(#{type := marketplace_install}) -> features;
determine_bucket(#{type := internal_request, metadata := #{bucket := Bucket}}) -> Bucket;
determine_bucket(_) -> features.

calculate_priority(#{type := cve}, security) -> 10;
calculate_priority(#{type := github_issue, labels := Labels}, _Bucket) ->
    case lists:member(<<"critical">>, Labels) of
        true -> 9;
        false ->
            case lists:member(<<"high">>, Labels) of
                true -> 7;
                false ->
                    case lists:member(<<"medium">>, Labels) of
                        true -> 5;
                        false -> 3
                    end
            end
    end;
calculate_priority(#{type := marketplace_refund}, reliability) -> 8;
calculate_priority(#{type := marketplace_install}, features) -> 4;
calculate_priority(_, security) -> 8;
calculate_priority(_, reliability) -> 6;
calculate_priority(_, compliance) -> 6;
calculate_priority(_, cost) -> 5;
calculate_priority(_, features) -> 4;
calculate_priority(_, technical_debt) -> 3.

calculate_signal_priority(Signal) ->
    Bucket = determine_bucket(Signal),
    calculate_priority(Signal, Bucket).

should_accept_signal(_Signal) ->
    %% Could add filters here (e.g., spam detection, duplicate checking)
    true.

%%%=============================================================================
%%% Internal Functions - Queue Management
%%%=============================================================================

do_get_queue(Bucket, State) ->
    Queue = maps:get(Bucket, State#state.queues, []),
    WorkOrders = State#state.work_orders,
    [maps:get(WoId, WorkOrders) || WoId <- Queue].

do_dequeue_next(Bucket, State) ->
    Queue = maps:get(Bucket, State#state.queues, []),
    case Queue of
        [] ->
            {empty, State};
        [WorkOrderId | _Rest] ->
            case check_wip_available(Bucket, State) of
                true ->
                    case do_start_work_order(WorkOrderId, State) of
                        {ok, NewState} ->
                            WorkOrder = maps:get(WorkOrderId, NewState#state.work_orders),
                            {{ok, WorkOrder}, NewState};
                        {Error, NewState} ->
                            {Error, NewState}
                    end;
                false ->
                    {{error, wip_limit}, State}
            end
    end.

do_reorder_queue(Bucket, NewOrder, State) ->
    %% Validate all IDs exist and belong to this bucket
    WorkOrders = State#state.work_orders,
    Valid = lists:all(fun(WoId) ->
        case maps:find(WoId, WorkOrders) of
            {ok, WO} -> maps:get(bucket, WO) =:= Bucket
                       andalso maps:get(status, WO) =:= queued;
            error -> false
        end
    end, NewOrder),

    case Valid of
        true ->
            Queues = maps:put(Bucket, NewOrder, State#state.queues),
            {ok, State#state{queues = Queues}};
        false ->
            {{error, invalid_queue_order}, State}
    end.

insert_by_priority(WorkOrderId, Priority, Queue, WorkOrders) ->
    %% Insert work order maintaining priority order (highest first)
    insert_by_priority(WorkOrderId, Priority, Queue, WorkOrders, []).

insert_by_priority(WorkOrderId, _Priority, [], _WorkOrders, Acc) ->
    lists:reverse([WorkOrderId | Acc]);
insert_by_priority(WorkOrderId, Priority, [H | T], WorkOrders, Acc) ->
    case maps:find(H, WorkOrders) of
        {ok, #{priority := HPriority}} when Priority > HPriority ->
            lists:reverse(Acc) ++ [WorkOrderId, H | T];
        {ok, _} ->
            insert_by_priority(WorkOrderId, Priority, T, WorkOrders, [H | Acc]);
        error ->
            %% Skip invalid entries
            insert_by_priority(WorkOrderId, Priority, T, WorkOrders, Acc)
    end.

%%%=============================================================================
%%% Internal Functions - SLA Tracking
%%%=============================================================================

do_check_sla(WorkOrderId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            Status = maps:get(status, WorkOrder),
            case Status of
                completed ->
                    {ok, on_time};
                cancelled ->
                    {ok, on_time};
                _ ->
                    Deadline = maps:get(sla_deadline, WorkOrder),
                    Now = erlang:timestamp(),

                    case compare_timestamps(Now, Deadline) of
                        less ->
                            %% Calculate time remaining
                            RemainingSeconds = timestamp_diff_seconds(Deadline, Now),
                            RemainingHours = RemainingSeconds / 3600,

                            %% Get total SLA hours
                            Bucket = maps:get(bucket, WorkOrder),
                            TotalHours = get_sla_hours(Bucket, State),

                            %% Check if < 25% time remaining (warning)
                            case TotalHours of
                                infinity ->
                                    {ok, on_time};
                                _ ->
                                    WarningThreshold = TotalHours * 0.25,
                                    if
                                        RemainingHours < WarningThreshold ->
                                            {warning, RemainingHours};
                                        true ->
                                            {ok, on_time}
                                    end
                            end;
                        _ ->
                            %% Breached
                            OverdueSeconds = timestamp_diff_seconds(Now, Deadline),
                            OverdueHours = OverdueSeconds / 3600,
                            {breached, OverdueHours}
                    end
            end;
        error ->
            {error, not_found}
    end.

do_get_sla_breaches(State) ->
    AllWorkOrders = maps:values(State#state.work_orders),
    ActiveWorkOrders = [WO || WO <- AllWorkOrders,
                             maps:get(status, WO) =:= in_progress
                             orelse maps:get(status, WO) =:= queued],

    Breaches = lists:filtermap(fun(WorkOrder) ->
        WorkOrderId = maps:get(id, WorkOrder),
        case do_check_sla(WorkOrderId, State) of
            {breached, Hours} ->
                {true, #{
                    work_order_id => WorkOrderId,
                    status => breached,
                    deadline => maps:get(sla_deadline, WorkOrder),
                    overdue_hours => Hours
                }};
            _ ->
                false
        end
    end, ActiveWorkOrders),

    %% Trigger Andons for critical breaches
    lists:foreach(fun(Breach) ->
        #{work_order_id := WoId} = Breach,
        case maps:find(WoId, State#state.work_orders) of
            {ok, #{bucket := security}} ->
                trigger_sla_andon(WoId, Breach);
            _ ->
                ok
        end
    end, Breaches),

    Breaches.

do_get_sla_warnings(WarningHours, State) ->
    AllWorkOrders = maps:values(State#state.work_orders),
    ActiveWorkOrders = [WO || WO <- AllWorkOrders,
                             maps:get(status, WO) =:= in_progress
                             orelse maps:get(status, WO) =:= queued],

    lists:filtermap(fun(WorkOrder) ->
        WorkOrderId = maps:get(id, WorkOrder),
        case do_check_sla(WorkOrderId, State) of
            {warning, Hours} when Hours =< WarningHours ->
                {true, #{
                    work_order_id => WorkOrderId,
                    status => warning,
                    deadline => maps:get(sla_deadline, WorkOrder),
                    remaining_hours => Hours
                }};
            _ ->
                false
        end
    end, ActiveWorkOrders).

%%%=============================================================================
%%% Internal Functions - Dependency Management
%%%=============================================================================

do_add_dependency(WorkOrderId, DependsOn, State) ->
    %% Check both work orders exist
    case {maps:is_key(WorkOrderId, State#state.work_orders),
          maps:is_key(DependsOn, State#state.work_orders)} of
        {true, true} ->
            %% Check for circular dependencies
            case would_create_cycle(WorkOrderId, DependsOn, State) of
                false ->
                    %% Add to dependencies map
                    Dependencies = State#state.dependencies,
                    CurrentDeps = maps:get(WorkOrderId, Dependencies, []),
                    NewDeps = case lists:member(DependsOn, CurrentDeps) of
                        true -> CurrentDeps;
                        false -> [DependsOn | CurrentDeps]
                    end,
                    NewDependencies = maps:put(WorkOrderId, NewDeps, Dependencies),

                    %% Add to blocked_by map
                    BlockedBy = State#state.blocked_by,
                    CurrentBlocked = maps:get(DependsOn, BlockedBy, []),
                    NewBlocked = case lists:member(WorkOrderId, CurrentBlocked) of
                        true -> CurrentBlocked;
                        false -> [WorkOrderId | CurrentBlocked]
                    end,
                    NewBlockedBy = maps:put(DependsOn, NewBlocked, BlockedBy),

                    %% Update work order status to blocked if in queue
                    WorkOrders = State#state.work_orders,
                    WorkOrder = maps:get(WorkOrderId, WorkOrders),
                    UpdatedWorkOrder = case maps:get(status, WorkOrder) of
                        queued ->
                            WorkOrder#{status => blocked};
                        _ ->
                            WorkOrder
                    end,
                    NewWorkOrders = maps:put(WorkOrderId, UpdatedWorkOrder, WorkOrders),
                    ets:insert(?ETS_TABLE, {WorkOrderId, UpdatedWorkOrder}),

                    NewState = State#state{
                        dependencies = NewDependencies,
                        blocked_by = NewBlockedBy,
                        work_orders = NewWorkOrders
                    },

                    {ok, NewState};
                true ->
                    {{error, circular_dependency}, State}
            end;
        _ ->
            {{error, work_order_not_found}, State}
    end.

do_get_dependencies(WorkOrderId, State) ->
    case maps:is_key(WorkOrderId, State#state.work_orders) of
        true ->
            Blocking = maps:get(WorkOrderId, State#state.blocked_by, []),
            BlockedBy = maps:get(WorkOrderId, State#state.dependencies, []),
            {ok, #{blocking => Blocking, blocked_by => BlockedBy}};
        false ->
            {error, not_found}
    end.

do_resolve_dependency(WorkOrderId, State) ->
    %% Get work orders blocked by this one
    BlockedWorkOrders = maps:get(WorkOrderId, State#state.blocked_by, []),

    %% For each blocked work order, remove this dependency
    NewState = lists:foldl(fun(BlockedId, AccState) ->
        Dependencies = AccState#state.dependencies,
        CurrentDeps = maps:get(BlockedId, Dependencies, []),
        NewDeps = lists:delete(WorkOrderId, CurrentDeps),

        %% If no more dependencies, unblock
        NewWorkOrders = case NewDeps of
            [] ->
                WorkOrders = AccState#state.work_orders,
                WorkOrder = maps:get(BlockedId, WorkOrders),
                UpdatedWorkOrder = case maps:get(status, WorkOrder) of
                    blocked -> WorkOrder#{status => queued};
                    _ -> WorkOrder
                end,
                ets:insert(?ETS_TABLE, {BlockedId, UpdatedWorkOrder}),
                maps:put(BlockedId, UpdatedWorkOrder, WorkOrders);
            _ ->
                AccState#state.work_orders
        end,

        NewDependencies = maps:put(BlockedId, NewDeps, Dependencies),

        AccState#state{
            dependencies = NewDependencies,
            work_orders = NewWorkOrders
        }
    end, State, BlockedWorkOrders),

    %% Remove from blocked_by map
    BlockedBy = maps:remove(WorkOrderId, NewState#state.blocked_by),

    NewState#state{blocked_by = BlockedBy}.

would_create_cycle(WorkOrderId, DependsOn, State) ->
    %% Check if DependsOn eventually depends on WorkOrderId
    check_transitive_dependency(DependsOn, WorkOrderId, State, []).

check_transitive_dependency(Current, Target, State, Visited) ->
    case lists:member(Current, Visited) of
        true -> false;  % Already checked
        false ->
            case Current =:= Target of
                true -> true;  % Found cycle
                false ->
                    Deps = maps:get(Current, State#state.dependencies, []),
                    lists:any(fun(Dep) ->
                        check_transitive_dependency(Dep, Target, State,
                                                   [Current | Visited])
                    end, Deps)
            end
    end.

check_dependencies_resolved(WorkOrderId, State) ->
    Dependencies = maps:get(WorkOrderId, State#state.dependencies, []),
    case Dependencies of
        [] ->
            true;
        _ ->
            %% Check if all dependencies are completed
            WorkOrders = State#state.work_orders,
            lists:all(fun(DepId) ->
                case maps:find(DepId, WorkOrders) of
                    {ok, #{status := completed}} -> true;
                    _ -> false
                end
            end, Dependencies)
    end.

%%%=============================================================================
%%% Internal Functions - Reporting
%%%=============================================================================

do_get_work_order_status(WorkOrderId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            %% Calculate elapsed time
            CreatedAt = maps:get(created_at, WorkOrder),
            Now = erlang:timestamp(),
            ElapsedSeconds = timestamp_diff_seconds(Now, CreatedAt),
            ElapsedHours = ElapsedSeconds / 3600,

            %% Get SLA status
            SlaStatus = case do_check_sla(WorkOrderId, State) of
                {ok, on_time} -> on_time;
                {warning, Hours} -> {warning, Hours};
                {breached, Hours} -> {breached, Hours};
                _ -> unknown
            end,

            %% Get dependencies
            {ok, Dependencies} = do_get_dependencies(WorkOrderId, State),

            Status = #{
                work_order => WorkOrder,
                elapsed_hours => ElapsedHours,
                sla_status => SlaStatus,
                dependencies => Dependencies,
                receipts => maps:get(receipts, WorkOrder, [])
            },

            {ok, Status};
        error ->
            {error, not_found}
    end.

do_generate_report(TimePeriod, State) ->
    {StartDate, EndDate} = TimePeriod,

    %% Filter work orders in time period
    AllWorkOrders = maps:values(State#state.work_orders),
    PeriodWorkOrders = filter_by_date_range(AllWorkOrders, StartDate, EndDate),

    %% Count by bucket
    ByBucket = lists:foldl(fun(WO, Acc) ->
        Bucket = maps:get(bucket, WO),
        maps:update_with(Bucket, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, PeriodWorkOrders),

    %% Count by status
    ByStatus = lists:foldl(fun(WO, Acc) ->
        Status = maps:get(status, WO),
        maps:update_with(Status, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, PeriodWorkOrders),

    %% Calculate average lead time (completed work orders only)
    CompletedWorkOrders = [WO || WO <- PeriodWorkOrders,
                                  maps:get(status, WO) =:= completed],

    AvgLeadTime = case CompletedWorkOrders of
        [] -> 0.0;
        _ ->
            TotalLeadTime = lists:sum([
                timestamp_diff_seconds(
                    maps:get(completed_at, WO),
                    maps:get(created_at, WO)
                ) / 3600
                || WO <- CompletedWorkOrders
            ]),
            TotalLeadTime / length(CompletedWorkOrders)
    end,

    %% Calculate SLA compliance rate
    TotalWithSla = length([WO || WO <- CompletedWorkOrders,
                                 maps:get(bucket, WO) =/= technical_debt]),

    SlaCompliant = length([WO || WO <- CompletedWorkOrders,
                                 maps:get(bucket, WO) =/= technical_debt,
                                 compare_timestamps(
                                     maps:get(completed_at, WO),
                                     maps:get(sla_deadline, WO)
                                 ) =:= less]),

    SlaComplianceRate = case TotalWithSla of
        0 -> 100.0;
        _ -> (SlaCompliant / TotalWithSla) * 100
    end,

    %% Get top priority items (highest priority, not completed)
    ActiveWorkOrders = [WO || WO <- PeriodWorkOrders,
                              maps:get(status, WO) =/= completed,
                              maps:get(status, WO) =/= cancelled],

    TopPriorities = lists:sublist(
        lists:reverse(lists:keysort(2, [
            {WO, maps:get(priority, WO)} || WO <- ActiveWorkOrders
        ])),
        10
    ),

    #{
        time_period => TimePeriod,
        total_work_orders => length(PeriodWorkOrders),
        by_bucket => ByBucket,
        by_status => ByStatus,
        completed => length(CompletedWorkOrders),
        cancelled => maps:get(cancelled, ByStatus, 0),
        average_lead_time => AvgLeadTime,
        sla_compliance_rate => SlaComplianceRate,
        top_priorities => [WO || {WO, _Priority} <- TopPriorities]
    }.

do_get_metrics(TimePeriod, State) ->
    Report = do_generate_report(TimePeriod, State),

    %% Additional metrics
    #{
        lead_time => maps:get(average_lead_time, Report),
        throughput => calculate_throughput(Report, TimePeriod),
        sla_compliance => maps:get(sla_compliance_rate, Report),
        work_in_progress => count_active_work_orders(State),
        queue_depth_by_bucket => get_queue_depths(State)
    }.

%%%=============================================================================
%%% Internal Functions - Persistence
%%%=============================================================================

do_save_to_ontology(WorkOrderId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            RdfTriples = work_order_to_rdf(WorkOrder),
            OntologyFile = State#state.ontology_file,

            %% Append to ontology file
            case file:open(OntologyFile, [append]) of
                {ok, File} ->
                    io:format(File, "~n# Work Order: ~s~n", [WorkOrderId]),
                    io:format(File, "~s~n", [RdfTriples]),
                    file:close(File),
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, not_found}
    end.

do_load_from_ontology(_WorkOrderId, _State) ->
    %% TODO: Implement RDF parsing
    {error, not_implemented}.

do_export_to_json(WorkOrderId, State) ->
    case maps:find(WorkOrderId, State#state.work_orders) of
        {ok, WorkOrder} ->
            %% Convert timestamps to ISO8601
            JsonWorkOrder = prepare_for_json(WorkOrder),
            Json = json_encode(JsonWorkOrder),

            %% Save to file
            Filename = binary_to_list(WorkOrderId) ++ ".json",
            FilePath = filename:join([State#state.receipts_dir, Filename]),

            case file:write_file(FilePath, Json) of
                ok -> {ok, Json};
                {error, Reason} -> {error, Reason}
            end;
        error ->
            {error, not_found}
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

generate_work_order_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("WO-~b-~6..0b", [Timestamp, Random])).

get_sla_hours(Bucket, State) ->
    SlaHours = maps:get(sla_hours, State#state.config),
    maps:get(Bucket, SlaHours, infinity).

calculate_deadline(infinity) ->
    %% Far future (100 years)
    {MegaSecs, Secs, MicroSecs} = erlang:timestamp(),
    {MegaSecs + 3153600, Secs, MicroSecs};
calculate_deadline(Hours) ->
    Now = erlang:timestamp(),
    add_hours_to_timestamp(Now, Hours).

add_hours_to_timestamp({MegaSecs, Secs, MicroSecs}, Hours) ->
    AdditionalSecs = round(Hours * 3600),
    NewSecs = Secs + AdditionalSecs,

    %% Handle overflow
    case NewSecs >= 1000000 of
        true ->
            {MegaSecs + (NewSecs div 1000000), NewSecs rem 1000000, MicroSecs};
        false ->
            {MegaSecs, NewSecs, MicroSecs}
    end.

compare_timestamps({Mega1, Secs1, Micro1}, {Mega2, Secs2, Micro2}) ->
    case Mega1 of
        M when M < Mega2 -> less;
        M when M > Mega2 -> greater;
        _ ->
            case Secs1 of
                S when S < Secs2 -> less;
                S when S > Secs2 -> greater;
                _ ->
                    case Micro1 of
                        Mi when Mi < Micro2 -> less;
                        Mi when Mi > Micro2 -> greater;
                        _ -> equal
                    end
            end
    end.

timestamp_diff_seconds({Mega1, Secs1, Micro1}, {Mega2, Secs2, Micro2}) ->
    TotalMicro1 = Mega1 * 1000000000000 + Secs1 * 1000000 + Micro1,
    TotalMicro2 = Mega2 * 1000000000000 + Secs2 * 1000000 + Micro2,
    (TotalMicro1 - TotalMicro2) / 1000000.

check_wip_available(Bucket, State) ->
    WipLimits = maps:get(wip_limits, State#state.config),
    Limit = maps:get(Bucket, WipLimits, infinity),

    Active = State#state.active,
    CurrentWip = length(maps:get(Bucket, Active, [])),

    case Limit of
        infinity -> true;
        _ -> CurrentWip < Limit
    end.

filter_by_date_range(WorkOrders, StartDate, EndDate) ->
    StartTimestamp = date_to_timestamp(StartDate),
    EndTimestamp = date_to_timestamp(EndDate),

    lists:filter(fun(WO) ->
        CreatedAt = maps:get(created_at, WO),
        compare_timestamps(CreatedAt, StartTimestamp) =/= less
            andalso compare_timestamps(CreatedAt, EndTimestamp) =/= greater
    end, WorkOrders).

date_to_timestamp({Y, M, D}) ->
    Seconds = calendar:datetime_to_gregorian_seconds({{Y, M, D}, {0, 0, 0}})
              - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    {Seconds div 1000000, Seconds rem 1000000, 0}.

calculate_throughput(Report, {StartDate, EndDate}) ->
    Completed = maps:get(completed, Report),
    Days = calendar:date_to_gregorian_days(EndDate)
           - calendar:date_to_gregorian_days(StartDate) + 1,
    case Days of
        0 -> 0.0;
        _ -> Completed / Days
    end.

count_active_work_orders(State) ->
    Active = State#state.active,
    lists:sum([length(List) || {_Bucket, List} <- maps:to_list(Active)]).

get_queue_depths(State) ->
    Queues = State#state.queues,
    maps:map(fun(_Bucket, Queue) -> length(Queue) end, Queues).

%%%=============================================================================
%%% Receipt Generation
%%%=============================================================================

generate_creation_receipt(WorkOrder, State) ->
    ReceiptId = generate_receipt_id("creation"),
    WorkOrderId = maps:get(id, WorkOrder),

    Receipt = #{
        id => ReceiptId,
        type => work_order_created,
        work_order_id => WorkOrderId,
        bucket => maps:get(bucket, WorkOrder),
        priority => maps:get(priority, WorkOrder),
        sla_deadline => maps:get(sla_deadline, WorkOrder),
        timestamp => erlang:timestamp()
    },

    save_receipt(Receipt, State).

generate_start_receipt(WorkOrder, State) ->
    ReceiptId = generate_receipt_id("start"),
    WorkOrderId = maps:get(id, WorkOrder),

    Receipt = #{
        id => ReceiptId,
        type => work_order_started,
        work_order_id => WorkOrderId,
        timestamp => erlang:timestamp()
    },

    save_receipt(Receipt, State).

generate_stage_receipt(WorkOrder, Stage, State) ->
    ReceiptId = generate_receipt_id("stage"),
    WorkOrderId = maps:get(id, WorkOrder),

    Receipt = #{
        id => ReceiptId,
        type => stage_completed,
        work_order_id => WorkOrderId,
        stage => Stage,
        timestamp => erlang:timestamp()
    },

    save_receipt(Receipt, State).

generate_completion_receipt(WorkOrder, State) ->
    ReceiptId = generate_receipt_id("completion"),
    WorkOrderId = maps:get(id, WorkOrder),

    %% Calculate lead time
    CreatedAt = maps:get(created_at, WorkOrder),
    CompletedAt = maps:get(completed_at, WorkOrder),
    LeadTimeHours = timestamp_diff_seconds(CompletedAt, CreatedAt) / 3600,

    Receipt = #{
        id => ReceiptId,
        type => work_order_completed,
        work_order_id => WorkOrderId,
        sku_id => maps:get(sku_id, WorkOrder),
        lead_time_hours => LeadTimeHours,
        stages_completed => maps:get(stages_completed, WorkOrder, []),
        timestamp => erlang:timestamp()
    },

    save_receipt(Receipt, State).

generate_cancellation_receipt(WorkOrder, Reason, State) ->
    ReceiptId = generate_receipt_id("cancellation"),
    WorkOrderId = maps:get(id, WorkOrder),

    Receipt = #{
        id => ReceiptId,
        type => work_order_cancelled,
        work_order_id => WorkOrderId,
        reason => Reason,
        timestamp => erlang:timestamp()
    },

    save_receipt(Receipt, State).

save_receipt(Receipt, State) ->
    ReceiptId = maps:get(id, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    FilePath = filename:join([State#state.receipts_dir, Filename]),

    Json = json_encode(Receipt),
    file:write_file(FilePath, Json).

generate_receipt_id(Type) ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("receipt-~s-~b-~6..0b", [Type, Timestamp, Random])).

%%%=============================================================================
%%% Integration Functions
%%%=============================================================================

notify_kanban_start(Bucket, WorkOrderId) ->
    %% Integrate with tcps_kanban to update WIP
    case whereis(tcps_kanban) of
        undefined -> ok;
        _Pid ->
            PullSignal = #{bucket => Bucket, payload => #{work_order_id => WorkOrderId}},
            tcps_kanban:process_pull_signal(PullSignal)
    end.

notify_kanban_complete(Bucket, WorkOrderId) ->
    %% Integrate with tcps_kanban to free WIP slot
    case whereis(tcps_kanban) of
        undefined -> ok;
        _Pid ->
            tcps_kanban:complete_work_order(WorkOrderId, Bucket)
    end.

notify_kaizen_completion(WorkOrder) ->
    %% Integrate with tcps_kaizen for metrics
    case whereis(tcps_kaizen) of
        undefined -> ok;
        _Pid ->
            Receipt = #{
                type => work_order,
                work_order_id => maps:get(id, WorkOrder),
                timestamp => maps:get(completed_at, WorkOrder),
                duration => calculate_work_order_duration(WorkOrder),
                success => true
            },
            tcps_kaizen:record_receipt(Receipt)
    end.

calculate_work_order_duration(WorkOrder) ->
    CreatedAt = maps:get(created_at, WorkOrder),
    CompletedAt = maps:get(completed_at, WorkOrder),
    timestamp_diff_seconds(CompletedAt, CreatedAt).

trigger_security_andon(WorkOrderId, CVE) ->
    %% Trigger Andon for critical security work order
    case whereis(tcps_andon) of
        undefined -> ok;
        _Pid ->
            Context = #{
                sku_id => <<"security-advisory">>,
                stage => requirements,
                details => #{
                    work_order_id => WorkOrderId,
                    cve => CVE,
                    severity => critical
                }
            },
            tcps_andon:trigger_andon(non_determinism, Context)
    end.

trigger_sla_andon(WorkOrderId, Breach) ->
    %% Trigger Andon for SLA breach
    case whereis(tcps_andon) of
        undefined -> ok;
        _Pid ->
            Context = #{
                sku_id => <<"sla-breach">>,
                stage => execution,
                details => #{
                    work_order_id => WorkOrderId,
                    breach => Breach
                }
            },
            tcps_andon:trigger_andon(missing_receipt, Context)
    end.

%%%=============================================================================
%%% GitHub Integration
%%%=============================================================================

parse_github_issue(IssueUrl) ->
    %% Parse GitHub issue URL
    %% Example: https://github.com/org/repo/issues/123

    %% TODO: Implement actual GitHub API integration
    %% For now, return mock data

    #{
        type => github_issue,
        source => IssueUrl,
        description => <<"GitHub issue from ", IssueUrl/binary>>,
        labels => [<<"bug">>, <<"high">>],
        metadata => #{
            url => IssueUrl,
            issue_number => extract_issue_number(IssueUrl)
        }
    }.

extract_issue_number(IssueUrl) ->
    %% Extract issue number from URL
    Parts = binary:split(IssueUrl, <<"/">>, [global]),
    case lists:reverse(Parts) of
        [Number | _] -> Number;
        _ -> <<"unknown">>
    end.

%%%=============================================================================
%%% Marketplace Integration
%%%=============================================================================

parse_marketplace_event(Event) ->
    EventType = maps:get(type, Event),

    #{
        type => case EventType of
            install -> marketplace_install;
            refund -> marketplace_refund
        end,
        source => <<"marketplace">>,
        description => iolist_to_binary(
            io_lib:format("Marketplace ~s event", [EventType])
        ),
        labels => case EventType of
            refund -> [<<"reliability">>, <<"customer-issue">>];
            install -> [<<"feature-request">>]
        end,
        metadata => Event
    }.

%%%=============================================================================
%%% RDF Generation
%%%=============================================================================

work_order_to_rdf(WorkOrder) ->
    WorkOrderId = maps:get(id, WorkOrder),
    Bucket = maps:get(bucket, WorkOrder),
    Priority = maps:get(priority, WorkOrder),
    Status = maps:get(status, WorkOrder),
    CreatedAt = timestamp_to_iso8601(maps:get(created_at, WorkOrder)),
    Description = escape_rdf_string(maps:get(description, WorkOrder)),

    SkuTriple = case maps:find(sku_id, WorkOrder) of
        {ok, SkuId} ->
            io_lib:format("    tcps:skuId \"~s\" ;~n", [SkuId]);
        error ->
            ""
    end,

    io_lib:format(
        "tcps:~s a tcps:WorkOrder ;~n"
        "    tcps:workOrderId \"~s\" ;~n"
        "    tcps:bucket \"~s\" ;~n"
        "    tcps:priority \"~p\" ;~n"
        "    tcps:status \"~s\" ;~n"
        "~s"
        "    tcps:createdAt \"~s\"^^xsd:dateTime ;~n"
        "    rdfs:label \"~s\" .~n",
        [WorkOrderId, WorkOrderId, Bucket, Priority, Status,
         SkuTriple, CreatedAt, Description]
    ).

timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    TotalSecs = MegaSecs * 1000000 + Secs,
    BaseDateTime = calendar:gregorian_seconds_to_datetime(
        TotalSecs + calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}})
    ),
    {{Y, M, D}, {H, Mi, S}} = BaseDateTime,
    Ms = MicroSecs div 1000,
    io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.~3..0wZ",
                  [Y, M, D, H, Mi, S, Ms]).

escape_rdf_string(Binary) when is_binary(Binary) ->
    %% Escape quotes and backslashes for RDF
    Str = binary_to_list(Binary),
    Escaped = lists:flatmap(fun
        ($") -> "\\\"";
        ($\\) -> "\\\\";
        (C) -> [C]
    end, Str),
    Escaped;
escape_rdf_string(Other) ->
    io_lib:format("~p", [Other]).

%%%=============================================================================
%%% JSON Encoding
%%%=============================================================================

json_encode(Map) when is_map(Map) ->
    %% Simple JSON encoder (replace with proper library in production)
    Fields = maps:fold(fun(K, V, Acc) ->
        Key = erlang:atom_to_binary(K),
        Value = json_encode_value(V),
        [io_lib:format("\"~s\":~s", [Key, Value]) | Acc]
    end, [], Map),

    iolist_to_binary(["{", string:join(Fields, ","), "}"]).

json_encode_value(V) when is_binary(V) ->
    io_lib:format("\"~s\"", [V]);
json_encode_value(V) when is_atom(V) ->
    io_lib:format("\"~s\"", [atom_to_list(V)]);
json_encode_value(V) when is_integer(V) ->
    integer_to_list(V);
json_encode_value(V) when is_float(V) ->
    float_to_list(V, [{decimals, 2}]);
json_encode_value({_, _, _} = Timestamp) ->
    io_lib:format("\"~s\"", [timestamp_to_iso8601(Timestamp)]);
json_encode_value(V) when is_list(V) ->
    case io_lib:printable_list(V) of
        true ->
            io_lib:format("\"~s\"", [V]);
        false ->
            Elements = [json_encode_value(E) || E <- V],
            ["[", string:join(Elements, ","), "]"]
    end;
json_encode_value(V) when is_map(V) ->
    json_encode(V);
json_encode_value(_) ->
    "null".

prepare_for_json(Map) when is_map(Map) ->
    maps:map(fun
        (_K, {_, _, _} = Timestamp) ->
            list_to_binary(timestamp_to_iso8601(Timestamp));
        (_K, V) when is_map(V) ->
            prepare_for_json(V);
        (_K, V) ->
            V
    end, Map).

atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom);
atom_to_binary(Binary) when is_binary(Binary) ->
    Binary.

%%%=============================================================================
%%% Internal Functions - List Operations
%%%=============================================================================

do_list_by_status(Status, State) ->
    AllWorkOrders = maps:values(State#state.work_orders),
    [WO || WO <- AllWorkOrders, maps:get(status, WO) =:= Status].

do_list_by_bucket(Bucket, State) ->
    AllWorkOrders = maps:values(State#state.work_orders),
    [WO || WO <- AllWorkOrders, maps:get(bucket, WO) =:= Bucket].

do_process_next(State) ->
    %% Get all pending work orders across all buckets
    AllWorkOrders = maps:values(State#state.work_orders),
    PendingWorkOrders = [WO || WO <- AllWorkOrders,
                               maps:get(status, WO) =:= queued orelse
                               maps:get(status, WO) =:= pending],

    case PendingWorkOrders of
        [] ->
            {empty, State};
        _ ->
            %% Use Heijunka leveling to select next work order
            case whereis(tcps_heijunka) of
                undefined ->
                    %% Fallback: pick highest priority
                    [NextWO | _] = lists:reverse(lists:keysort(2, [
                        {WO, maps:get(priority, WO)} || WO <- PendingWorkOrders
                    ])),
                    WorkOrderId = maps:get(id, NextWO),
                    case do_start_work_order(WorkOrderId, State) of
                        {ok, NewState} ->
                            {{ok, NextWO}, NewState};
                        {Error, NewState} ->
                            {Error, NewState}
                    end;
                _Pid ->
                    %% Use Heijunka for leveling
                    NextBatch = tcps_heijunka:schedule_next_batch(1),
                    case NextBatch of
                        {ok, []} ->
                            {empty, State};
                        {ok, [WorkOrderId | _]} ->
                            case maps:find(WorkOrderId, State#state.work_orders) of
                                {ok, WorkOrder} ->
                                    case do_start_work_order(WorkOrderId, State) of
                                        {ok, NewState} ->
                                            {{ok, WorkOrder}, NewState};
                                        {Error, NewState} ->
                                            {Error, NewState}
                                    end;
                                error ->
                                    {{error, not_found}, State}
                            end;
                        _ ->
                            {empty, State}
                    end
            end
    end.

%%%=============================================================================
%%% Helper Functions for Testing
%%%=============================================================================

%% @doc List all work orders (for testing and cleanup)
-spec list_all() -> [work_order()].
list_all() ->
    case whereis(?SERVER) of
        undefined -> [];
        _Pid -> gen_server:call(?SERVER, list_all)
    end.

%% @doc Delete a work order (for testing and cleanup)
-spec delete(work_order_id()) -> ok | {error, term()}.
delete(WorkOrderId) ->
    case whereis(?SERVER) of
        undefined -> ok;
        _Pid -> gen_server:call(?SERVER, {delete, WorkOrderId})
    end.

%%%=============================================================================
%%% Test Support
%%%=============================================================================

-ifdef(TEST).

init() ->
    case ets:info(?ETS_TABLE) of
        undefined ->
            ets:new(?ETS_TABLE, [named_table, set, public, {read_concurrency, true}]);
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
