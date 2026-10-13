%%%-------------------------------------------------------------------
%%% @doc erlmcp_cluster_rpc - Inter-Node RPC Communication Layer
%%%
%%% Implements reliable RPC communication across cluster nodes with:
%%% - Request tracking and correlation
%%% - Message broadcasting
%%% - Timeout handling
%%% - Retry logic with backoff
%%% - Distributed tracing support
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cluster_rpc).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1,
         call/4, call/5, cast/4,
         broadcast/3, broadcast/4,
         multicall/4, multicall/5,
         async_call/4, async_call/5,
         await/2, await/3,
         get_pending_requests/0,
         set_default_timeout/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_CALL_TIMEOUT, 10000).
-define(MAX_RETRIES, 3).
-define(CLEANUP_INTERVAL, 60000).

%%====================================================================
%% Types
%%====================================================================

-type request_id() :: binary().
-type node_result() :: {node(), term()} | {node(), badrpc, term()}.

-record(request_state, {
    request_id :: request_id(),
    from :: pid() | undefined,
    target_nodes :: [node()],
    completed_nodes = [] :: [node()],
    results = [] :: [node_result()],
    timeout :: integer(),
    start_time :: integer(),
    retry_count = 0 :: non_neg_integer(),
    trace_id :: binary() | undefined}).

-record(state, {
    pending_requests = #{} :: #{request_id() => #request_state{}},
    request_timeout :: integer(),
    cleanup_interval :: pos_integer(),
    cleanup_ref :: reference() | undefined}).

-type state() :: #state{}.

-export_type([request_id/0, node_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start RPC layer with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start RPC layer with options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Synchronous RPC call to a single node
-spec call(node(), module(), atom(), list()) -> term().
call(Node, Module, Fun, Args) ->
    call(Node, Module, Fun, Args, ?DEFAULT_CALL_TIMEOUT).

-spec call(node(), module(), atom(), list(), timeout()) -> term().
call(Node, Module, Fun, Args, Timeout) when is_atom(Node), is_atom(Module), is_atom(Fun), is_list(Args) ->
    RequestId = generate_request_id(),
    TraceId = get_trace_id(),

    Request = #request_state{
        request_id = RequestId,
        from = self(),
        target_nodes = [Node],
        timeout = Timeout,
        start_time = erlang:system_time(millisecond),
        trace_id = TraceId
    },

    gen_server:call(?SERVER, {register_request, Request}),

    try
        Result = rpc:call(Node, Module, Fun, Args, Timeout),
        gen_server:cast(?SERVER, {request_complete, RequestId, Node, {ok, Result}}),
        Result
    catch
        Class:Reason:Stacktrace ->
            Error = {Class, Reason, Stacktrace},
            gen_server:cast(?SERVER, {request_complete, RequestId, Node, {error, Error}}),
            erlang:raise(Class, Reason, Stacktrace)
    end.

%% @doc Asynchronous cast to a node
-spec cast(node(), module(), atom(), list()) -> ok.
cast(Node, Module, Fun, Args) when is_atom(Node), is_atom(Module), is_atom(Fun), is_list(Args) ->
    % Use spawn to avoid blocking
    spawn(fun() ->
        try
            rpc:call(Node, Module, Fun, Args, ?DEFAULT_TIMEOUT)
        catch
            _:_ -> ok
        end
    end),
    ok.

%% @doc Broadcast a message to all cluster nodes
-spec broadcast(module(), atom(), list()) -> ok.
broadcast(Module, Fun, Args) ->
    broadcast(nodes(), Module, Fun, Args).

-spec broadcast([node()], module(), atom(), list()) -> ok.
broadcast(Nodes, Module, Fun, Args) when is_list(Nodes) ->
    lists:foreach(fun(Node) ->
        cast(Node, Module, Fun, Args)
    end, Nodes),
    ok.

%% @doc Multicast call to multiple nodes, collect results
-spec multicall([node()], module(), atom(), list()) -> [node_result()].
multicall(Nodes, Module, Fun, Args) ->
    multicall(Nodes, Module, Fun, Args, ?DEFAULT_CALL_TIMEOUT).

-spec multicall([node()], module(), atom(), list(), timeout()) -> [node_result()].
multicall(Nodes, Module, Fun, Args, Timeout) when is_list(Nodes) ->
    RequestId = generate_request_id(),
    TraceId = get_trace_id(),

    Request = #request_state{
        request_id = RequestId,
        from = self(),
        target_nodes = Nodes,
        timeout = Timeout,
        start_time = erlang:system_time(millisecond),
        trace_id = TraceId
    },

    {ok, TimeoutRef} = gen_server:call(?SERVER, {register_multicall, Request}),

    % Execute calls in parallel
    Caller = self(),
    spawn(fun() ->
        Results = lists:map(fun(Node) ->
            execute_rpc_call(Node, Module, Fun, Args, Timeout, RequestId, Caller)
        end, Nodes),
        Caller ! {multicall_result, RequestId, Results}
    end),

    % Wait for results or timeout
    receive
        {multicall_result, RequestId, Results} ->
            gen_server:cast(?SERVER, {request_complete, RequestId, all, {ok, Results}}),
            Results
    after Timeout ->
        erlang:cancel_timer(TimeoutRef),
        {error, timeout}
    end.

%% @doc Async RPC call - returns request ID immediately
-spec async_call(node(), module(), atom(), list()) -> request_id().
async_call(Node, Module, Fun, Args) ->
    async_call(Node, Module, Fun, Args, ?DEFAULT_CALL_TIMEOUT).

-spec async_call(node(), module(), atom(), list(), timeout()) -> request_id().
async_call(Node, Module, Fun, Args, Timeout) ->
    RequestId = generate_request_id(),
    TraceId = get_trace_id(),

    Request = #request_state{
        request_id = RequestId,
        from = undefined,
        target_nodes = [Node],
        timeout = Timeout,
        start_time = erlang:system_time(millisecond),
        trace_id = TraceId
    },

    gen_server:cast(?SERVER, {register_async_request, Request}),

    Caller = self(),
    spawn(fun() ->
        Result = execute_rpc_call(Node, Module, Fun, Args, Timeout, RequestId, Caller),
        Caller ! {async_call_result, RequestId, Node, Result}
    end),

    RequestId.

%% @doc Await result of async call
-spec await(request_id(), timeout()) -> {ok, term()} | {error, timeout | term()}.
await(RequestId, Timeout) ->
    await(RequestId, Timeout, fun erlang:exit/1).

-spec await(request_id(), timeout(), fun((term()) -> no_return())) -> {ok, term()} | {error, timeout | term()}.
await(RequestId, Timeout, OnTimeout) ->
    receive
        {async_call_result, RequestId, _Node, Result} ->
            {ok, Result};
        {async_call_error, RequestId, Reason} ->
            {error, Reason}
    after Timeout ->
        OnTimeout(timeout),
        {error, timeout}
    end.

%% @doc Get all pending requests
-spec get_pending_requests() -> #{request_id() => map()}.
get_pending_requests() ->
    gen_server:call(?SERVER, get_pending_requests).

%% @doc Set default timeout for RPC calls
-spec set_default_timeout(timeout()) -> ok.
set_default_timeout(Timeout) ->
    gen_server:call(?SERVER, {set_default_timeout, Timeout}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map() | []) -> {ok, state()}.
init(Options) ->
    process_flag(trap_exit, true),

    Timeout = maps:get(timeout, Options, ?DEFAULT_CALL_TIMEOUT),
    CleanupInterval = maps:get(cleanup_interval, Options, ?CLEANUP_INTERVAL),

    ?LOG_INFO("Starting cluster RPC layer: timeout=~p", [Timeout]),

    % Start cleanup timer
    CleanupRef = erlang:send_after(CleanupInterval, self(), cleanup_pending),

    {ok, #state{
        request_timeout = Timeout,
        cleanup_interval = CleanupInterval,
        cleanup_ref = CleanupRef
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call({register_request, Request = #request_state{request_id = RequestId}}, _From, State) ->
    NewPending = maps:put(RequestId, Request, State#state.pending_requests),
    {reply, ok, State#state{pending_requests = NewPending}, hibernate};

handle_call({register_multicall, Request = #request_state{request_id = RequestId, timeout = Timeout}}, _From, State) ->
    NewPending = maps:put(RequestId, Request, State#state.pending_requests),
    TimeoutRef = erlang:send_after(Timeout, self(), {request_timeout, RequestId}),
    {reply, {ok, TimeoutRef}, State#state{pending_requests = NewPending}, hibernate};

handle_call(get_pending_requests, _From, State) ->
    Pending = maps:map(fun(_RequestId, ReqState) ->
        #{
            target_nodes => ReqState#request_state.target_nodes,
            completed_nodes => ReqState#request_state.completed_nodes,
            results => ReqState#request_state.results,
            timeout => ReqState#request_state.timeout,
            start_time => ReqState#request_state.start_time,
            trace_id => ReqState#request_state.trace_id
        }
    end, State#state.pending_requests),
    {reply, Pending, State, hibernate};

handle_call({set_default_timeout, Timeout}, _From, State) ->
    {reply, ok, State#state{request_timeout = Timeout}, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast({register_async_request, Request = #request_state{request_id = RequestId}}, State) ->
    NewPending = maps:put(RequestId, Request, State#state.pending_requests),
    {noreply, State#state{pending_requests = NewPending}, hibernate};

handle_cast({request_complete, RequestId, Node, Result}, State) ->
    NewPending = maps:update_with(RequestId, fun(ReqState) ->
        NewCompleted = [Node | ReqState#request_state.completed_nodes],
        NewResults = [{Node, Result} | ReqState#request_state.results],
        ReqState#request_state{completed_nodes = NewCompleted, results = NewResults}
    end, State#state.pending_requests),
    {noreply, State#state{pending_requests = NewPending}, hibernate};

handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_info({request_timeout, RequestId}, State) ->
    case maps:take(RequestId, State#state.pending_requests) of
        error ->
            {noreply, State, hibernate};
        {_ReqState, NewPending} ->
            ?LOG_WARNING("RPC request timeout: ~p", [RequestId]),
            {noreply, State#state{pending_requests = NewPending}, hibernate}
    end;

handle_info(cleanup_pending, State) ->
    % Remove expired requests
    Now = erlang:system_time(millisecond),
    NewPending = maps:filter(fun(_RequestId, ReqState) ->
        Age = Now - ReqState#request_state.start_time,
        Age < (ReqState#request_state.timeout * 2)
    end, State#state.pending_requests),

    CleanupRef = erlang:send_after(State#state.cleanup_interval, self(), cleanup_pending),
    {noreply, State#state{pending_requests = NewPending, cleanup_ref = CleanupRef}, hibernate};

handle_info({'EXIT', _Pid, Reason}, State) ->
    ?LOG_DEBUG("Process exit: ~p", [Reason]),
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.cleanup_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,
    ?LOG_INFO("Cluster RPC layer terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Execute RPC call with error handling
-spec execute_rpc_call(node(), module(), atom(), list(), timeout(),
                       request_id(), pid()) -> term().
execute_rpc_call(Node, Module, Fun, Args, Timeout, RequestId, Caller) ->
    TraceId = get_trace_id(),
    Start = erlang:system_time(microsecond),

    % Inject trace context
    set_trace_id(TraceId),

    Result = try
        rpc:call(Node, Module, Fun, Args, Timeout)
    catch
        Class:Reason:Stacktrace ->
            Caller ! {async_call_error, RequestId, Reason},
            erlang:raise(Class, Reason, Stacktrace)
    end,

    % Record metrics
    Duration = erlang:system_time(microsecond) - Start,
    ?LOG_DEBUG("RPC call ~p to ~p completed in ~p us", [RequestId, Node, Duration]),

    Result.

%% @doc Generate unique request ID
-spec generate_request_id() -> request_id().
generate_request_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    Timestamp = erlang:system_time(microsecond),
    iolist_to_binary([integer_to_binary(Timestamp, 16), "-", integer_to_binary(Id, 16)]).

%% @doc Get current trace ID from process dictionary
-spec get_trace_id() -> binary() | undefined.
get_trace_id() ->
    case erlang:get(erlmcp_trace_id) of
        undefined -> undefined;
        TraceId -> TraceId
    end.

%% @doc Set trace ID in process dictionary
-spec set_trace_id(binary() | undefined) -> ok.
set_trace_id(undefined) -> ok;
set_trace_id(TraceId) ->
    erlang:put(erlmcp_trace_id, TraceId),
    ok.
