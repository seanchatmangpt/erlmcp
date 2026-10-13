%%%-------------------------------------------------------------------
%%% @doc erlmcp_request_manager - Sophisticated Request Management System
%%%
%%% Provides enterprise-grade request management with:
%%% - Request tracking with pending_requests map
%%% - Request ID correlation and management
%%% - Priority-based request ordering
%%% - Timeout and cancellation handling
%%% - Concurrent request processing
%%% - Request result caching and deduplication
%%% - Comprehensive error handling and recovery
%%% - Monitoring and metrics for request processing
%%%
%%% Chicago TDD implementation with comprehensive type specifications
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_manager).

-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% Request management
    register_request/3,
    register_request/4,
    get_request/1,
    get_request/2,
    update_request_status/2,
    update_request_status/3,
    complete_request/2,
    complete_request/3,
    cancel_request/1,
    cancel_request/2,
    remove_request/1,

    %% Batch operations
    register_requests/2,
    register_requests/3,
    complete_requests/1,
    cancel_requests/1,
    clear_all_requests/0,

    %% Priority and queueing
    get_priority_queue/0,
    get_next_request/0,
    get_next_request/1,

    %% Caching and deduplication
    get_cached_result/1,
    cache_result/2,
    clear_cache/0,
    is_duplicate_request/3,

    %% Monitoring and metrics
    get_metrics/0,
    get_active_requests/0,
    get_pending_requests/0,
    get_completed_requests/0,
    get_failed_requests/0,

    %% Configuration
    set_default_timeout/1,
    get_default_timeout/0,
    set_max_concurrent/1,
    get_max_concurrent/0,
    set_cache_ttl/1,
    get_cache_ttl/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records and types
-include("erlmcp.hrl").

%% Records
-record(request_info, {
    id :: pos_integer(),
    client_pid :: pid(),
    method :: binary(),
    params :: map(),
    priority :: pos_integer(),
    status :: pending | processing | completed | failed | cancelled | timeout,
    timestamp :: integer(),
    timeout_ms :: pos_integer(),
    expiration :: integer(),
    result :: term() | undefined,
    error :: term() | undefined,
    retry_count :: non_neg_integer(),
    metadata :: map()
}).

-record(cache_entry, {
    key :: pos_integer(),
    value :: term(),
    timestamp :: integer(),
    expiration :: integer()
}).

-record(metrics, {
    total_requests :: non_neg_integer(),
    active_requests :: non_neg_integer(),
    pending_requests :: non_neg_integer(),
    completed_requests :: non_neg_integer(),
    failed_requests :: non_neg_integer(),
    cancelled_requests :: non_neg_integer(),
    timeout_requests :: non_neg_integer(),
    cache_hits :: non_neg_integer(),
    cache_misses :: non_neg_integer(),
    average_processing_time :: float()
}).

-record(state, {
    requests :: ets:tid(),             % request_id -> #request_info{}
    cache :: ets:tid(),                % request_id -> #cache_entry{}
    priority_queue :: ets:tid(),        % priority -> queue of request_ids
    metrics :: ets:tid(),              % metrics counters
    monitors :: ets:tid(),             % request_id -> client_pid
    config :: map(),                   % configuration settings
    cleanup_timer :: reference() | undefined
}).

%% Type specifications
-type request_id() :: pos_integer().
-type request_status() :: pending | processing | completed | failed | cancelled | timeout.
-type priority() :: pos_integer().
-type client_pid() :: pid().
-type request_data() :: #{
    method := binary(),
    params := map(),
    priority := priority(),
    metadata := map() | undefined
}.

-type request_info() :: #request_info{}.
-type cache_entry() :: #cache_entry{}.
-type metrics() :: #metrics{}.
-type state() :: #state{}.

-export_type([
    request_id/0,
    request_status/0,
    priority/0,
    client_pid/0,
    request_data/0,
    request_info/0,
    metrics/0
]).


%% Configuration defaults
-define(DEFAULT_TIMEOUT_MS, 30000).
-define(MAX_CONCURRENT_REQUESTS, 100).
-define(DEFAULT_CACHE_TTL_MS, 300000). %% 5 minutes
-define(DEFAULT_CLEANUP_INTERVAL_MS, 60000). %% 1 minute
-define(MAX_REQUEST_ID, 9223372036854775807). %% 2^63 - 1

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the request manager with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the request manager with custom configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Stop the request manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% Request Management Functions
%%====================================================================

%% @doc Register a new request with default timeout
-spec register_request(request_id(), request_data(), client_pid()) ->
    ok | {error, term()}.
register_request(RequestId, RequestData, ClientPid) ->
    register_request(RequestId, RequestData, ClientPid, ?DEFAULT_TIMEOUT_MS).

%% @doc Register a new request with custom timeout
-spec register_request(request_id(), request_data(), client_pid(), pos_integer()) ->
    ok | {error, term()}.
register_request(RequestId, RequestData, ClientPid, TimeoutMs) ->
    gen_server:call(?MODULE, {register_request, RequestId, RequestData, ClientPid, TimeoutMs}).

%% @doc Get request information with timeout
-spec get_request(request_id()) -> {ok, request_info()} | {error, term()}.
get_request(RequestId) ->
    get_request(RequestId, 5000).

%% @doc Get request information with custom timeout
-spec get_request(request_id(), timeout()) -> {ok, request_info()} | {error, term()}.
get_request(RequestId, Timeout) ->
    gen_server:call(?MODULE, {get_request, RequestId}, Timeout).

%% @doc Update request status
-spec update_request_status(request_id(), request_status()) -> ok | {error, term()}.
update_request_status(RequestId, Status) ->
    update_request_status(RequestId, Status, #{}).

%% @doc Update request status with metadata
-spec update_request_status(request_id(), request_status(), map()) -> ok | {error, term()}.
update_request_status(RequestId, Status, Metadata) ->
    gen_server:call(?MODULE, {update_request_status, RequestId, Status, Metadata}).

%% @doc Complete a request with result
-spec complete_request(request_id(), term()) -> ok | {error, term()}.
complete_request(RequestId, Result) ->
    complete_request(RequestId, Result, #{}).

%% @doc Complete a request with result and metadata
-spec complete_request(request_id(), term(), map()) -> ok | {error, term()}.
complete_request(RequestId, Result, Metadata) ->
    gen_server:call(?MODULE, {complete_request, RequestId, Result, Metadata}).

%% @doc Cancel a request
-spec cancel_request(request_id()) -> ok | {error, term()}.
cancel_request(RequestId) ->
    cancel_request(RequestId, #{}).

%% @doc Cancel a request with metadata
-spec cancel_request(request_id(), map()) -> ok | {error, term()}.
cancel_request(RequestId, Metadata) ->
    gen_server:call(?MODULE, {cancel_request, RequestId, Metadata}).

%% @doc Remove a request
-spec remove_request(request_id()) -> ok | {error, term()}.
remove_request(RequestId) ->
    gen_server:call(?MODULE, {remove_request, RequestId}).

%%====================================================================
%% Batch Operations
%%====================================================================

%% @doc Register multiple requests
-spec register_requests([{request_id(), request_data()}], client_pid()) ->
    ok | {error, term()}.
register_requests(Requests, ClientPid) ->
    register_requests(Requests, ClientPid, ?DEFAULT_TIMEOUT_MS).

%% @doc Register multiple requests with custom timeout
-spec register_requests([{request_id(), request_data()}], client_pid(), pos_integer()) ->
    ok | {error, term()}.
register_requests(Requests, ClientPid, TimeoutMs) ->
    gen_server:call(?MODULE, {register_requests, Requests, ClientPid, TimeoutMs}).

%% @doc Complete multiple requests
-spec complete_requests([{request_id(), term()}]) -> ok | {error, term()}.
complete_requests(Results) ->
    gen_server:call(?MODULE, {complete_requests, Results}).

%% @doc Cancel multiple requests
-spec cancel_requests([request_id()]) -> ok | {error, term()}.
cancel_requests(RequestIds) ->
    gen_server:call(?MODULE, {cancel_requests, RequestIds}).

%% @doc Clear all requests
-spec clear_all_requests() -> ok.
clear_all_requests() ->
    gen_server:call(?MODULE, clear_all_requests).

%%====================================================================
%% Priority and Queueing
%%====================================================================

%% @doc Get priority queue (ordered by priority)
-spec get_priority_queue() -> [request_id()].
get_priority_queue() ->
    gen_server:call(?MODULE, get_priority_queue).

%% @doc Get next request from priority queue
-spec get_next_request() -> {ok, request_info()} | {error, term()}.
get_next_request() ->
    get_next_request(?DEFAULT_TIMEOUT_MS).

%% @doc Get next request with timeout
-spec get_next_request(timeout()) -> {ok, request_info()} | {error, term()}.
get_next_request(Timeout) ->
    gen_server:call(?MODULE, get_next_request, Timeout).

%%====================================================================
%% Caching and Deduplication
%%====================================================================

%% @doc Get cached result for request
-spec get_cached_result(request_id()) -> {ok, term()} | {error, not_found}.
get_cached_result(RequestId) ->
    gen_server:call(?MODULE, {get_cached_result, RequestId}).

%% @doc Cache a result for a request
-spec cache_result(request_id(), term()) -> ok | {error, term()}.
cache_result(RequestId, Result) ->
    gen_server:call(?MODULE, {cache_result, RequestId, Result}).

%% @doc Clear all cached results
-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?MODULE, clear_cache).

%% @doc Check if request is duplicate
-spec is_duplicate_request(request_id(), request_data(), client_pid()) ->
    boolean() | {error, term()}.
is_duplicate_request(RequestId, RequestData, ClientPid) ->
    case get_request(RequestId) of
        {ok, RequestInfo} ->
            is_request_duplicate(RequestInfo, RequestData, ClientPid);
        {error, not_found} ->
            false
    end.

%%====================================================================
%% Monitoring and Metrics
%%====================================================================

%% @doc Get current metrics
-spec get_metrics() -> metrics().
get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

%% @doc Get active requests
-spec get_active_requests() -> [request_info()].
get_active_requests() ->
    gen_server:call(?MODULE, get_active_requests).

%% @doc Get pending requests
-spec get_pending_requests() -> [request_info()].
get_pending_requests() ->
    gen_server:call(?MODULE, get_pending_requests).

%% @doc Get completed requests
-spec get_completed_requests() -> [request_info()].
get_completed_requests() ->
    gen_server:call(?MODULE, get_completed_requests).

%% @doc Get failed requests
-spec get_failed_requests() -> [request_info()].
get_failed_requests() ->
    gen_server:call(?MODULE, get_failed_requests).

%%====================================================================
%% Configuration Functions
%%====================================================================

%% @doc Set default timeout
-spec set_default_timeout(pos_integer()) -> ok.
set_default_timeout(TimeoutMs) ->
    gen_server:call(?MODULE, {set_config, default_timeout, TimeoutMs}).

%% @doc Get default timeout
-spec get_default_timeout() -> pos_integer().
get_default_timeout() ->
    gen_server:call(?MODULE, get_config).

%% @doc Set maximum concurrent requests
-spec set_max_concurrent(pos_integer()) -> ok.
set_max_concurrent(MaxConcurrent) ->
    gen_server:call(?MODULE, {set_config, max_concurrent, MaxConcurrent}).

%% @doc Get maximum concurrent requests
-spec get_max_concurrent() -> pos_integer().
get_max_concurrent() ->
    gen_server:call(?MODULE, get_config).

%% @doc Set cache TTL
-spec set_cache_ttl(pos_integer()) -> ok.
set_cache_ttl(TtlMs) ->
    gen_server:call(?MODULE, {set_config, cache_ttl, TtlMs}).

%% @doc Get cache TTL
-spec get_cache_ttl() -> pos_integer().
get_cache_ttl() ->
    gen_server:call(?MODULE, get_config).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    %% Create ETS tables
    Requests = ets:new(?MODULE, [
        set, protected, named_table,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),

    Cache = ets:new(table_name(cache), [
        set, protected, named_table,
        {read_concurrency, true}
    ]),

    PriorityQueue = ets:new(table_name(priority), [
        ordered_set, protected, named_table
    ]),

    Metrics = ets:new(table_name(metrics), [
        set, protected, named_table
    ]),

    Monitors = ets:new(table_name(monitors), [
        set, protected, named_table
    ]),

    %% Initialize metrics
    InitialMetrics = #metrics{
        total_requests = 0,
        active_requests = 0,
        pending_requests = 0,
        completed_requests = 0,
        failed_requests = 0,
        cancelled_requests = 0,
        timeout_requests = 0,
        cache_hits = 0,
        cache_misses = 0,
        average_processing_time = 0.0
    },
    ets:insert(Metrics, {metrics, InitialMetrics}),

    %% Merge configuration with defaults
    DefaultConfig = #{
        default_timeout => ?DEFAULT_TIMEOUT_MS,
        max_concurrent => ?MAX_CONCURRENT_REQUESTS,
        cache_ttl => ?DEFAULT_CACHE_TTL_MS,
        cleanup_interval => ?DEFAULT_CLEANUP_INTERVAL_MS
    },
    MergedConfig = maps:merge(DefaultConfig, Config),

    %% Start cleanup timer
    CleanupInterval = maps:get(cleanup_interval, MergedConfig),
    CleanupTimer = erlang:send_after(CleanupInterval, self(), cleanup_expired_requests),

    {ok, #state{
        requests = Requests,
        cache = Cache,
        priority_queue = PriorityQueue,
        metrics = Metrics,
        monitors = Monitors,
        config = MergedConfig,
        cleanup_timer = CleanupTimer
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({register_request, RequestId, RequestData, ClientPid, TimeoutMs}, _From, State) ->
    case validate_request_id(RequestId) of
        true ->
            case is_duplicate_request_state(State, RequestId, RequestData, ClientPid) of
                true ->
                    {reply, {error, duplicate_request}, State};
                false ->
                    %% Check concurrent request limit
                    case check_concurrent_limit(State) of
                        true ->
                            RequestInfo = create_request_info(RequestId, RequestData, ClientPid, TimeoutMs),
                            case add_request(RequestInfo, State) of
                                {ok, NewState} ->
                                    %% Add to priority queue
                                    add_to_priority_queue(RequestInfo, NewState),
                                    {reply, ok, NewState};
                                {error, Reason} ->
                                    {reply, {error, Reason}, State}
                            end;
                        false ->
                            {reply, {error, max_concurrent_limit}, State}
                    end
            end;
        false ->
            {reply, {error, invalid_request_id}, State}
    end;

handle_call({get_request, RequestId}, _From, State) ->
    case ets:lookup(State#state.requests, RequestId) of
        [{RequestId, RequestInfo}] ->
            {reply, {ok, RequestInfo}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_request_status, RequestId, Status, Metadata}, _From, State) ->
    case update_request_status_state(RequestId, Status, Metadata, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({complete_request, RequestId, Result, Metadata}, _From, State) ->
    case complete_request_state(RequestId, Result, Metadata, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_request, RequestId, Metadata}, _From, State) ->
    case cancel_request_state(RequestId, Metadata, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove_request, RequestId}, _From, State) ->
    case remove_request_state(RequestId, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({register_requests, Requests, ClientPid, TimeoutMs}, _From, State) ->
    case check_concurrent_limit_length(State, length(Requests)) of
        true ->
            Results = lists:map(fun({RequestId, RequestData}) ->
                case register_request_state(RequestId, RequestData, ClientPid, TimeoutMs, State) of
                    {ok, RequestInfo, NewState} ->
                        add_to_priority_queue(RequestInfo, NewState),
                        {RequestId, ok};
                    {error, Reason} ->
                        {RequestId, {error, Reason}}
                end
            end, Requests),
            {reply, Results, State};
        false ->
            {reply, {error, max_concurrent_limit}, State}
    end;

handle_call({complete_requests, Results}, _From, State) ->
    ResultsWithState = lists:map(fun({RequestId, Result}) ->
        case complete_request_state(RequestId, Result, #{}, State) of
            {ok, NewState} ->
                {RequestId, ok, NewState};
            {error, Reason} ->
                {RequestId, {error, Reason}, State}
        end
    end, Results),

    %% Extract final state from all operations
    FinalState = lists:foldl(fun({_, _, S}, _Acc) -> S end, State, ResultsWithState),

    {reply, ResultsWithState, FinalState};

handle_call({cancel_requests, RequestIds}, _From, State) ->
    Results = lists:map(fun(RequestId) ->
        case cancel_request_state(RequestId, #{}, State) of
            {ok, NewState} ->
                {RequestId, ok, NewState};
            {error, Reason} ->
                {RequestId, {error, Reason}, State}
        end
    end, RequestIds),

    FinalState = lists:foldl(fun({_, _, S}, _Acc) -> S end, State, Results),
    {reply, Results, FinalState};

handle_call(clear_all_requests, _From, State) ->
    NewState = clear_all_requests_state(State),
    {reply, ok, NewState};

handle_call(get_priority_queue, _From, State) ->
    PriorityQueue = get_priority_queue_state(State),
    {reply, PriorityQueue, State};

handle_call(get_next_request, _From, State) ->
    case get_next_request_state(State) of
        {ok, RequestInfo, NewState} ->
            {reply, {ok, RequestInfo}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_cached_result, RequestId}, _From, State) ->
    case get_cached_result_state(RequestId, State) of
        {ok, Result, NewState} ->
            update_metrics(metrics, cache_hits, 1, NewState),
            {reply, {ok, Result}, NewState};
        {error, Reason} ->
            update_metrics(metrics, cache_misses, 1, State),
            {reply, {error, Reason}, State}
    end;

handle_call({cache_result, RequestId, Result}, _From, State) ->
    case cache_result_state(RequestId, Result, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(clear_cache, _From, State) ->
    NewState = clear_cache_state(State),
    {reply, ok, NewState};

handle_call(get_metrics, _From, State) ->
    case ets:lookup(State#state.metrics, metrics) of
        [{metrics, Metrics}] ->
            {reply, Metrics, State};
        [] ->
            InitialMetrics = #metrics{},
            {reply, InitialMetrics, State}
    end;

handle_call(get_active_requests, _From, State) ->
    ActiveRequests = get_requests_by_status([pending, processing], State),
    {reply, ActiveRequests, State};

handle_call(get_pending_requests, _From, State) ->
    PendingRequests = get_requests_by_status([pending], State),
    {reply, PendingRequests, State};

handle_call(get_completed_requests, _From, State) ->
    CompletedRequests = get_requests_by_status([completed], State),
    {reply, CompletedRequests, State};

handle_call(get_failed_requests, _From, State) ->
    FailedRequests = get_requests_by_status([failed], State),
    {reply, FailedRequests, State};

handle_call({set_config, Key, Value}, _From, State) ->
    NewConfig = (State#state.config)#{Key => Value},
    {reply, ok, State#state{config = NewConfig}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired_requests, State) ->
    NewState = cleanup_expired_requests_state(State),

    %% Schedule next cleanup
    CleanupInterval = maps:get(cleanup_interval, State#state.config),
    NewCleanupTimer = erlang:send_after(CleanupInterval, self(), cleanup_expired_requests),

    {noreply, NewState#state{cleanup_timer = NewCleanupTimer}};

handle_info({'DOWN', _Ref, process, _Pid, _Reason} = Msg, State) ->
    %% Handle client process down
    NewState = handle_client_down(Msg, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{cleanup_timer = CleanupTimer}) ->
    %% Cancel cleanup timer
    case is_reference(CleanupTimer) of
        true -> erlang:cancel_timer(CleanupTimer);
        false -> ok
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Request validation functions
table_name(Suffix) ->
    binary_to_atom(<<(atom_to_binary(?MODULE, utf8))/binary,
                     "_",
                     (atom_to_binary(Suffix, utf8))/binary>>, utf8).

validate_request_id(RequestId) when is_integer(RequestId), RequestId > 0, RequestId =< ?MAX_REQUEST_ID ->
    true;
validate_request_id(_) ->
    false.

%% Request creation and management
create_request_info(RequestId, RequestData, ClientPid, TimeoutMs) ->
    Method = maps:get(method, RequestData),
    Params = maps:get(params, RequestData, #{}),
    Priority = maps:get(priority, RequestData, 5),
    Metadata = maps:get(metadata, RequestData, #{}),

    CurrentTime = erlang:system_time(millisecond),
    ExpirationTime = CurrentTime + TimeoutMs,

    #request_info{
        id = RequestId,
        client_pid = ClientPid,
        method = Method,
        params = Params,
        priority = Priority,
        status = pending,
        timestamp = CurrentTime,
        timeout_ms = TimeoutMs,
        expiration = ExpirationTime,
        retry_count = 0,
        metadata = Metadata
    }.

%% Request state management
add_request(RequestInfo, State) ->
    try
        ets:insert(State#state.requests, {RequestInfo#request_info.id, RequestInfo}),

        %% Monitor client process
        MonitorRef = erlang:monitor(process, RequestInfo#request_info.client_pid),
        ets:insert(State#state.monitors, {RequestInfo#request_info.id, MonitorRef}),

        %% Update metrics
        update_metrics(metrics, total_requests, 1, State),
        update_metrics(metrics, pending_requests, 1, State),

        {ok, State}
    catch
        error:_ ->
            {error, database_error}
    end.

update_request_status_state(RequestId, Status, Metadata, State) ->
    case ets:lookup(State#state.requests, RequestId) of
        [{RequestId, RequestInfo}] ->
            UpdatedRequestInfo = RequestInfo#request_info{
                status = Status,
                metadata = maps:merge(RequestInfo#request_info.metadata, Metadata)
            },

            %% Update metrics
            update_metrics_by_status(RequestInfo#request_info.status, Status, State),

            %% Update in database
            ets:insert(State#state.requests, {RequestId, UpdatedRequestInfo}),
            {ok, State};
        [] ->
            {error, not_found}
    end.

complete_request_state(RequestId, Result, Metadata, State) ->
    case update_request_status_state(RequestId, completed, Metadata, State) of
        {ok, NewState} ->
            %% Cache the result
            cache_result_state(RequestId, Result, NewState),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

cancel_request_state(RequestId, Metadata, State) ->
    case update_request_status_state(RequestId, cancelled, Metadata, State) of
        {ok, NewState} ->
            %% Remove from priority queue if present
            remove_from_priority_queue(RequestId, NewState),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

remove_request_state(RequestId, State) ->
    case ets:lookup(State#state.requests, RequestId) of
        [{RequestId, RequestInfo}] ->
            %% Remove from requests table
            ets:delete(State#state.requests, RequestId),

            %% Remove monitor
            case ets:lookup(State#state.monitors, RequestId) of
                [{RequestId, MonitorRef}] ->
                    erlang:demonitor(MonitorRef),
                    ets:delete(State#state.monitors, RequestId);
                _ -> ok
            end,

            %% Remove from cache
            ets:delete(State#state.cache, RequestId),

            %% Remove from priority queue
            remove_from_priority_queue(RequestId, State),

            %% Update metrics
            update_metrics_by_status(RequestInfo#request_info.status, removed, State),

            {ok, State};
        [] ->
            {error, not_found}
    end.

%% Priority queue management
add_to_priority_queue(RequestInfo, State) ->
    Priority = RequestInfo#request_info.priority,
    RequestId = RequestInfo#request_info.id,

    %% Get current queue for this priority
    case ets:lookup(State#state.priority_queue, Priority) of
        [{Priority, Queue}] ->
            NewQueue = queue:in(RequestId, Queue),
            ets:insert(State#state.priority_queue, {Priority, NewQueue});
        [] ->
            NewQueue = queue:in(RequestId, queue:new()),
            ets:insert(State#state.priority_queue, {Priority, NewQueue})
    end,
    State.

remove_from_priority_queue(RequestId, State) ->
    %% Find priority for this request ID
    case ets:lookup(State#state.requests, RequestId) of
        [{RequestId, RequestInfo}] ->
            Priority = RequestInfo#request_info.priority,
            case ets:lookup(State#state.priority_queue, Priority) of
                [{Priority, Queue}] ->
                    NewQueue = queue:filter(fun(Id) -> Id =/= RequestId end, Queue),
                    ets:insert(State#state.priority_queue, {Priority, NewQueue});
                _ -> ok
            end;
        _ -> ok
    end.

get_priority_queue_state(State) ->
    %% Get all priorities in order
    Priorities = ets:tab2list(State#state.priority_queue),
    lists:flatmap(fun({_Priority, Queue}) ->
        queue:to_list(Queue)
    end, Priorities).

get_next_request_state(State) ->
    %% Get the highest priority (lowest number)
    case ets:first(State#state.priority_queue) of
        '$end_of_table' ->
            {error, no_requests};
        Priority ->
            case ets:lookup(State#state.priority_queue, Priority) of
                [{Priority, Queue}] ->
                    case queue:out(Queue) of
                        {value, RequestId, NewQueue} ->
                            %% Update priority queue
                            ets:insert(State#state.priority_queue, {Priority, NewQueue}),

                            %% Get request info
                            case ets:lookup(State#state.requests, RequestId) of
                                [{RequestId, RequestInfo}] ->
                                    %% Update status to processing
                                    UpdatedRequestInfo = RequestInfo#request_info{status = processing},
                                    ets:insert(State#state.requests, {RequestId, UpdatedRequestInfo}),

                                    %% Update metrics
                                    update_metrics_by_status(pending, processing, State),

                                    {ok, UpdatedRequestInfo, State};
                                [] ->
                                    {error, request_not_found}
                            end;
                        {empty, _} ->
                            {error, no_requests}
                    end
            end
    end.

%% Caching functions
cache_result_state(RequestId, Result, State) ->
    CurrentTime = erlang:system_time(millisecond),
    CacheTTL = maps:get(cache_ttl, State#state.config),
    ExpirationTime = CurrentTime + CacheTTL,

    CacheEntry = #cache_entry{
        key = RequestId,
        value = Result,
        timestamp = CurrentTime,
        expiration = ExpirationTime
    },

    ets:insert(State#state.cache, {RequestId, CacheEntry}),
    {ok, State}.

get_cached_result_state(RequestId, State) ->
    case ets:lookup(State#state.cache, RequestId) of
        [{RequestId, CacheEntry}] ->
            CurrentTime = erlang:system_time(millisecond),
            case CurrentTime =< CacheEntry#cache_entry.expiration of
                true ->
                    {ok, CacheEntry#cache_entry.value, State};
                false ->
                    %% Cache expired, remove it
                    ets:delete(State#state.cache, RequestId),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

clear_cache_state(State) ->
    ets:delete_all_objects(State#state.cache),
    State.

%% Duplicate detection
is_duplicate_request_state(State, RequestId, RequestData, ClientPid) ->
    case ets:lookup(State#state.requests, RequestId) of
        [{RequestId, RequestInfo}] ->
            is_request_duplicate(RequestInfo, RequestData, ClientPid);
        [] ->
            false
    end.

is_request_duplicate(RequestInfo, RequestData, ClientPid) ->
    %% Check if method, params, and client match
    MethodMatch = RequestInfo#request_info.method =:= maps:get(method, RequestData),
    ParamsMatch = RequestInfo#request_info.params =:= maps:get(params, RequestData),
    ClientMatch = RequestInfo#request_info.client_pid =:= ClientPid,

    MethodMatch andalso ParamsMatch andalso ClientMatch.

%% Metrics functions
update_metrics(Table, Field, Increment, State) ->
    case ets:lookup(State#state.metrics, Table) of
        [{Table, Metrics}] ->
            UpdatedMetrics = bump_field(Field, Metrics, Increment),
            ets:insert(State#state.metrics, {Table, UpdatedMetrics});
        [] ->
            %% Create initial metrics record
            InitialMetrics = bump_field(Field, #metrics{}, Increment),
            ets:insert(State#state.metrics, {Table, InitialMetrics})
    end,
    State.

bump_field(total_requests, M, I) ->
    M#metrics{total_requests = M#metrics.total_requests + I};
bump_field(active_requests, M, I) ->
    M#metrics{active_requests = M#metrics.active_requests + I};
bump_field(pending_requests, M, I) ->
    M#metrics{pending_requests = M#metrics.pending_requests + I};
bump_field(completed_requests, M, I) ->
    M#metrics{completed_requests = M#metrics.completed_requests + I};
bump_field(failed_requests, M, I) ->
    M#metrics{failed_requests = M#metrics.failed_requests + I};
bump_field(cancelled_requests, M, I) ->
    M#metrics{cancelled_requests = M#metrics.cancelled_requests + I};
bump_field(timeout_requests, M, I) ->
    M#metrics{timeout_requests = M#metrics.timeout_requests + I};
bump_field(cache_hits, M, I) ->
    M#metrics{cache_hits = M#metrics.cache_hits + I};
bump_field(cache_misses, M, I) ->
    M#metrics{cache_misses = M#metrics.cache_misses + I};
bump_field(_Field, M, _I) ->
    M.

update_metrics_by_status(OldStatus, NewStatus, State) ->
    %% Decrement old status count
    case OldStatus of
        pending -> update_metrics(metrics, pending_requests, -1, State);
        processing -> update_metrics(metrics, active_requests, -1, State);
        _ -> ok
    end,

    %% Increment new status count
    case NewStatus of
        pending -> update_metrics(metrics, pending_requests, 1, State);
        processing -> update_metrics(metrics, active_requests, 1, State);
        completed -> update_metrics(metrics, completed_requests, 1, State);
        failed -> update_metrics(metrics, failed_requests, 1, State);
        cancelled -> update_metrics(metrics, cancelled_requests, 1, State);
        timeout -> update_metrics(metrics, timeout_requests, 1, State);
        removed -> ok
    end,

    State.

%% Batch operations
register_request_state(RequestId, RequestData, ClientPid, TimeoutMs, State) ->
    RequestInfo = create_request_info(RequestId, RequestData, ClientPid, TimeoutMs),
    add_request(RequestInfo, State).

check_concurrent_limit(State) ->
    Metrics = get_metrics_state(State),
    ActiveCount = Metrics#metrics.active_requests + Metrics#metrics.pending_requests,
    MaxConcurrent = maps:get(max_concurrent, State#state.config),
    ActiveCount < MaxConcurrent.

check_concurrent_limit_length(State, Length) ->
    Metrics = get_metrics_state(State),
    ActiveCount = Metrics#metrics.active_requests + Metrics#metrics.pending_requests,
    MaxConcurrent = maps:get(max_concurrent, State#state.config),
    (ActiveCount + Length) =< MaxConcurrent.

get_metrics_state(State) ->
    case ets:lookup(State#state.metrics, metrics) of
        [{metrics, Metrics}] -> Metrics;
        [] -> #metrics{}
    end.

get_requests_by_status(Statuses, State) ->
    Requests = ets:tab2list(State#state.requests),
    lists:filter(fun({_RequestId, RequestInfo}) ->
        lists:member(RequestInfo#request_info.status, Statuses)
    end, Requests).

%% Cleanup functions
cleanup_expired_requests_state(State) ->
    CurrentTime = erlang:system_time(millisecond),

    %% Find expired requests (record field access; maps unsupported in
    %% match-spec patterns)
    ExpiredRequests = lists:filter(
        fun({_Id, RequestInfo}) ->
            RequestInfo#request_info.expiration =< CurrentTime
        end, ets:tab2list(State#state.requests)),

    %% Handle expired requests
    lists:foreach(fun({RequestId, RequestInfo}) ->
        case RequestInfo#request_info.status of
            pending ->
                %% Mark as timeout
                update_request_status_state(RequestId, timeout, #{reason => expired}, State);
            _ ->
                %% Already completed or failed, just remove
                remove_request_state(RequestId, State)
        end
    end, ExpiredRequests),

    %% Clean up expired cache entries
    ExpiredCache = lists:filter(
        fun({_Id, CacheEntry}) ->
            CacheEntry#cache_entry.expiration =< CurrentTime
        end, ets:tab2list(State#state.cache)),
    lists:foreach(fun({RequestId, _}) ->
        ets:delete(State#state.cache, RequestId)
    end, ExpiredCache),

    State.

clear_all_requests_state(State) ->
    %% Clear all tables
    ets:delete_all_objects(State#state.requests),
    ets:delete_all_objects(State#state.cache),
    ets:delete_all_objects(State#state.priority_queue),

    ResetMetrics = #metrics{
        total_requests = 0,
        active_requests = 0,
        pending_requests = 0,
        completed_requests = 0,
        failed_requests = 0,
        cancelled_requests = 0,
        timeout_requests = 0,
        cache_hits = 0,
        cache_misses = 0,
        average_processing_time = 0.0
    },
    ets:insert(State#state.metrics, {metrics, ResetMetrics}),

    State.

%% Client process monitoring
handle_client_down({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    %% Find requests monitored via this ref (the dead client)
    DeadClients = ets:match(State#state.monitors, {'$1', Ref}),

    lists:foreach(fun([RequestId]) ->
        %% Clean up monitor
        erlang:demonitor(Ref),

        case ets:lookup(State#state.requests, RequestId) of
            [{RequestId, RequestInfo}] ->
                if
                    RequestInfo#request_info.status =:= pending ->
                        %% Cancel pending requests for dead client
                        update_request_status_state(RequestId, cancelled, #{reason => client_died}, State);
                    RequestInfo#request_info.status =:= processing ->
                        %% Mark processing requests as failed
                        update_request_status_state(RequestId, failed, #{reason => client_died}, State);
                    true ->
                        ok
                end;
            [] ->
                ok
        end,

        ets:delete(State#state.monitors, RequestId)
    end, DeadClients),

    State;
handle_client_down(_Other, State) ->
    State.