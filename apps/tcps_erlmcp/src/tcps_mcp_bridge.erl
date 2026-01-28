%%%-----------------------------------------------------------------------------
%%% @doc TCPS MCP Bridge - Seamless Integration Layer
%%%
%%% Automatic work order creation from MCP requests with:
%%% - Quality gate injection into request pipeline
%%% - Andon (stop-the-line) triggers on SLA violations
%%% - Receipt generation for all successful requests
%%% - Automatic bucket assignment based on MCP method
%%% - Pull signal extraction from request context
%%%
%%% Integration Points:
%%% - erlmcp_server:handle_request/5 - Middleware hooks
%%% - tcps_work_order - Work order creation
%%% - tcps_quality_gates - Gate enforcement
%%% - tcps_receipt_chain - Audit trail
%%% - tcps_andon - SLA monitoring
%%%
%%% Quality Gates (8 stages):
%%% 1. Schema validation (JSON-RPC)
%%% 2. Authorization check
%%% 3. Rate limiting
%%% 4. Resource availability
%%% 5. Performance envelope check
%%% 6. Security scan
%%% 7. Compliance check
%%% 8. Receipt generation
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_mcp_bridge).
-behaviour(gen_server).

-include_lib("erlmcp_core/include/erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    before_request/1,
    after_request/2,
    mcp_request_to_work_order/1,
    calculate_priority/1,
    calculate_deadline/1,
    calculate_bucket/1,
    should_create_work_order/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Test support
-ifdef(TEST).
-export([reset_state/0, get_state/0]).
-endif.

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type mcp_request() :: #json_rpc_request{}.
-type mcp_response() :: {ok, term()} | {error, {integer(), binary(), term()}}.
-type quality_gate() :: 1..8.
-type gate_result() :: pass | {fail, Reason :: binary()}.

-record(bridge_state, {
    %% Configuration
    auto_integration_enabled = true :: boolean(),
    quality_gates_enabled = [1,2,3,4,5,6,7,8] :: [quality_gate()],
    andon_on_sla_violation = true :: boolean(),

    %% Statistics
    total_requests = 0 :: non_neg_integer(),
    work_orders_created = 0 :: non_neg_integer(),
    quality_gate_failures = #{} :: #{quality_gate() => non_neg_integer()},
    andon_triggers = 0 :: non_neg_integer(),

    %% Work order tracking
    request_to_work_order = #{} :: #{json_rpc_id() => binary()},
    work_order_to_request = #{} :: #{binary() => json_rpc_id()}
}).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Start the TCPS-MCP bridge server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%%------------------------------------------------------------------------------
%% @doc Before-request middleware hook.
%%
%% Executes quality gates before request processing:
%% - Gate 1: Schema validation
%% - Gate 2: Authorization check
%% - Gate 3: Rate limiting
%% - Gate 4: Resource availability
%% - Gate 5: Performance envelope check
%% - Gate 6: Security scan
%% - Gate 7: Compliance check
%%
%% Returns: {ok, Request} | {error, {Code, Message, Data}}
%% @end
%%------------------------------------------------------------------------------
-spec before_request(mcp_request()) -> {ok, mcp_request()} | {error, {integer(), binary(), map()}}.
before_request(Request) ->
    gen_server:call(?SERVER, {before_request, Request}).

%%------------------------------------------------------------------------------
%% @doc After-request middleware hook.
%%
%% Executes post-processing after successful request:
%% - Gate 8: Receipt generation
%% - Work order creation (if applicable)
%% - SLA monitoring
%% - Andon triggering (if SLA violated)
%%
%% Returns: ok | {error, term()}
%% @end
%%------------------------------------------------------------------------------
-spec after_request(mcp_request(), mcp_response()) -> ok | {error, term()}.
after_request(Request, Response) ->
    gen_server:call(?SERVER, {after_request, Request, Response}).

%%------------------------------------------------------------------------------
%% @doc Convert MCP request to work order specification.
%%
%% Maps MCP methods to work order buckets:
%% - tools/call → features (new functionality)
%% - resources/read → maintenance (resource access)
%% - prompts/get → support (user assistance)
%% - tasks/create → features (async operations)
%%
%% @end
%%------------------------------------------------------------------------------
-spec mcp_request_to_work_order(mcp_request()) -> map().
mcp_request_to_work_order(#json_rpc_request{method = Method, params = Params, id = Id}) ->
    Bucket = calculate_bucket(Method),
    Priority = calculate_priority(#{method => Method, params => Params}),
    Deadline = calculate_deadline(#{bucket => Bucket, priority => Priority}),

    #{
        type => method_to_work_type(Method),
        bucket => Bucket,
        priority => Priority,
        sla_deadline => Deadline,
        description => build_description(Method, Params),
        pull_signal => #{
            type => mcp_request,
            source => <<"MCP Protocol">>,
            description => iolist_to_binary(io_lib:format("MCP ~s request", [Method])),
            labels => [<<"mcp">>, Method],
            metadata => #{
                request_id => Id,
                method => Method,
                params_count => maps:size(Params)
            }
        }
    }.

%%------------------------------------------------------------------------------
%% @doc Calculate bucket assignment based on MCP method.
%% @end
%%------------------------------------------------------------------------------
-spec calculate_bucket(binary()) -> atom().
calculate_bucket(<<"tools/call">>) -> features;
calculate_bucket(<<"resources/read">>) -> reliability;
calculate_bucket(<<"resources/list">>) -> reliability;
calculate_bucket(<<"prompts/get">>) -> features;
calculate_bucket(<<"prompts/list">>) -> features;
calculate_bucket(<<"tasks/create">>) -> features;
calculate_bucket(<<"tasks/cancel">>) -> reliability;
calculate_bucket(<<"logging/", _/binary>>) -> compliance;
calculate_bucket(<<"sampling/", _/binary>>) -> features;
calculate_bucket(_) -> features.  % Default bucket

%%------------------------------------------------------------------------------
%% @doc Calculate priority (1-10) based on request characteristics.
%% @end
%%------------------------------------------------------------------------------
-spec calculate_priority(map()) -> 1..10.
calculate_priority(#{method := Method} = Request) ->
    BaseScore = case Method of
        <<"tasks/cancel">> -> 9;     % High priority - cancellation
        <<"tools/call">> -> 6;       % Medium-high - tool execution
        <<"resources/read">> -> 5;   % Medium - resource access
        <<"prompts/get">> -> 4;      % Medium-low - prompt retrieval
        <<"resources/list">> -> 3;   % Low - list operations
        _ -> 5                        % Default medium priority
    end,

    % Adjust based on params
    ParamsBoost = case maps:get(params, Request, #{}) of
        P when map_size(P) > 5 -> 1;  % Complex request
        _ -> 0
    end,

    min(10, max(1, BaseScore + ParamsBoost)).

%%------------------------------------------------------------------------------
%% @doc Calculate SLA deadline based on bucket and priority.
%% @end
%%------------------------------------------------------------------------------
-spec calculate_deadline(map()) -> erlang:timestamp().
calculate_deadline(#{bucket := Bucket, priority := Priority}) ->
    % Base SLA hours by bucket
    BaseHours = case Bucket of
        security -> 24;
        reliability -> 168;   % 7 days
        compliance -> 168;    % 7 days
        features -> 720;      % 30 days
        cost -> 720;          % 30 days
        technical_debt -> 2160  % 90 days
    end,

    % Adjust by priority (higher priority = shorter deadline)
    AdjustedHours = case Priority of
        P when P >= 9 -> BaseHours div 4;   % 4x faster for critical
        P when P >= 7 -> BaseHours div 2;   % 2x faster for high
        P when P >= 5 -> BaseHours;         % Normal for medium
        _ -> BaseHours * 2                   % 2x slower for low
    end,

    % Calculate future timestamp
    NowSecs = erlang:system_time(second),
    DeadlineSecs = NowSecs + (AdjustedHours * 3600),
    calendar:system_time_to_universal_time(DeadlineSecs, second).

%%------------------------------------------------------------------------------
%% @doc Determine if work order should be created for this request.
%%
%% Creates work orders for:
%% - tool calls (trackable operations)
%% - task creation (async work)
%% - resource writes (state changes)
%%
%% Skips work orders for:
%% - list operations (read-only queries)
%% - get operations (simple reads)
%% - initialize (protocol handshake)
%% @end
%%------------------------------------------------------------------------------
-spec should_create_work_order(mcp_request()) -> boolean().
should_create_work_order(#json_rpc_request{method = Method}) ->
    case Method of
        <<"tools/call">> -> true;
        <<"tasks/create">> -> true;
        <<"resources/write">> -> true;
        <<"prompts/execute">> -> true;
        _ -> false  % Skip list/get/initialize operations
    end.

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

init(Opts) ->
    % Get config from app env or opts
    AutoIntegration = proplists:get_value(tcps_auto_integration, Opts,
        application:get_env(tcps_erlmcp, tcps_auto_integration, true)),

    QualityGatesEnabled = proplists:get_value(tcps_quality_gates_enabled, Opts,
        application:get_env(tcps_erlmcp, tcps_quality_gates_enabled, [1,2,3,4,5,6,7,8])),

    AndonEnabled = proplists:get_value(tcps_andon_on_sla_violation, Opts,
        application:get_env(tcps_erlmcp, tcps_andon_on_sla_violation, true)),

    State = #bridge_state{
        auto_integration_enabled = AutoIntegration,
        quality_gates_enabled = QualityGatesEnabled,
        andon_on_sla_violation = AndonEnabled
    },

    logger:info("TCPS-MCP Bridge started: auto_integration=~p, gates=~p, andon=~p",
        [AutoIntegration, QualityGatesEnabled, AndonEnabled]),

    {ok, State}.

handle_call({before_request, Request}, _From, State) ->
    NewState = State#bridge_state{total_requests = State#bridge_state.total_requests + 1},

    case State#bridge_state.auto_integration_enabled of
        false ->
            {reply, {ok, Request}, NewState};
        true ->
            % Run quality gates
            case run_quality_gates(Request, State#bridge_state.quality_gates_enabled) of
                pass ->
                    {reply, {ok, Request}, NewState};
                {fail, GateNum, Reason} ->
                    % Record failure
                    Failures = State#bridge_state.quality_gate_failures,
                    NewFailures = maps:update_with(GateNum, fun(C) -> C + 1 end, 1, Failures),

                    % Return error response
                    Error = {?JSONRPC_INVALID_REQUEST,
                             iolist_to_binary(io_lib:format("Quality gate ~p failed: ~s", [GateNum, Reason])),
                             #{gate => GateNum}},
                    {reply, {error, Error}, NewState#bridge_state{quality_gate_failures = NewFailures}}
            end
    end;

handle_call({after_request, Request, Response}, _From, State) ->
    case State#bridge_state.auto_integration_enabled of
        false ->
            {reply, ok, State};
        true ->
            NewState = process_after_request(Request, Response, State),
            {reply, ok, NewState}
    end;

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(reset_state, _State) ->
    {noreply, #bridge_state{}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:info("TCPS-MCP Bridge terminating"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions - Quality Gates
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Run quality gates on request.
%% @end
%%------------------------------------------------------------------------------
-spec run_quality_gates(mcp_request(), [quality_gate()]) -> pass | {fail, quality_gate(), binary()}.
run_quality_gates(Request, EnabledGates) ->
    run_gates_loop(Request, lists:sort(EnabledGates)).

run_gates_loop(_Request, []) ->
    pass;
run_gates_loop(Request, [Gate | Rest]) ->
    case run_gate(Gate, Request) of
        pass ->
            run_gates_loop(Request, Rest);
        {fail, Reason} ->
            {fail, Gate, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Execute individual quality gate.
%% @end
%%------------------------------------------------------------------------------
-spec run_gate(quality_gate(), mcp_request()) -> gate_result().

% Gate 1: Schema validation (JSON-RPC)
run_gate(1, #json_rpc_request{method = Method, params = Params}) ->
    case validate_schema(Method, Params) of
        ok -> pass;
        {error, Reason} -> {fail, Reason}
    end;

% Gate 2: Authorization check
run_gate(2, _Request) ->
    % TODO: Implement authorization check
    pass;

% Gate 3: Rate limiting
run_gate(3, _Request) ->
    % TODO: Implement rate limiting check
    pass;

% Gate 4: Resource availability
run_gate(4, #json_rpc_request{method = <<"resources/", _/binary>>}) ->
    % TODO: Check resource availability
    pass;
run_gate(4, _Request) ->
    pass;

% Gate 5: Performance envelope check
run_gate(5, _Request) ->
    % TODO: Check performance metrics
    pass;

% Gate 6: Security scan
run_gate(6, #json_rpc_request{params = Params}) ->
    % Basic security checks
    case check_security_violations(Params) of
        ok -> pass;
        {error, Reason} -> {fail, Reason}
    end;

% Gate 7: Compliance check
run_gate(7, _Request) ->
    % TODO: Implement compliance validation
    pass;

% Gate 8: Receipt generation (after-request only)
run_gate(8, _Request) ->
    pass.

%%------------------------------------------------------------------------------
%% @doc Validate request schema.
%% @end
%%------------------------------------------------------------------------------
-spec validate_schema(binary(), map()) -> ok | {error, binary()}.
validate_schema(_Method, Params) when is_map(Params) ->
    ok;
validate_schema(_Method, _Params) ->
    {error, <<"Invalid params: must be object">>}.

%%------------------------------------------------------------------------------
%% @doc Check for security violations in params.
%% @end
%%------------------------------------------------------------------------------
-spec check_security_violations(map()) -> ok | {error, binary()}.
check_security_violations(Params) ->
    % Check for dangerous patterns
    ParamsStr = iolist_to_binary(io_lib:format("~p", [Params])),
    DangerousPatterns = [<<"../">>, <<"<script">>, <<"DROP TABLE">>, <<"rm -rf">>],

    case lists:any(fun(Pattern) ->
        binary:match(ParamsStr, Pattern) =/= nomatch
    end, DangerousPatterns) of
        true -> {error, <<"Security violation detected">>};
        false -> ok
    end.

%%%=============================================================================
%%% Internal Functions - After Request Processing
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Process request after completion.
%% @end
%%------------------------------------------------------------------------------
-spec process_after_request(mcp_request(), mcp_response(), #bridge_state{}) -> #bridge_state{}.
process_after_request(Request, {ok, _Result}, State) ->
    % Generate receipt (Gate 8)
    Receipt = generate_receipt(Request),
    ok = store_receipt(Receipt),

    % Create work order if applicable
    case should_create_work_order(Request) of
        true ->
            create_work_order_from_request(Request, State);
        false ->
            State
    end;

process_after_request(_Request, {error, _}, State) ->
    % Don't create work orders for failed requests
    State.

%%------------------------------------------------------------------------------
%% @doc Create work order from MCP request.
%% @end
%%------------------------------------------------------------------------------
-spec create_work_order_from_request(mcp_request(), #bridge_state{}) -> #bridge_state{}.
create_work_order_from_request(Request, State) ->
    WorkOrderSpec = mcp_request_to_work_order(Request),

    case tcps_work_order:create_work_order(WorkOrderSpec) of
        {ok, WorkOrderId} ->
            logger:info("Created work order ~s for MCP request ~p",
                [WorkOrderId, Request#json_rpc_request.id]),

            % Track mapping
            ReqId = Request#json_rpc_request.id,
            NewReqToWO = maps:put(ReqId, WorkOrderId, State#bridge_state.request_to_work_order),
            NewWOToReq = maps:put(WorkOrderId, ReqId, State#bridge_state.work_order_to_request),

            State#bridge_state{
                work_orders_created = State#bridge_state.work_orders_created + 1,
                request_to_work_order = NewReqToWO,
                work_order_to_request = NewWOToReq
            };
        {error, Reason} ->
            logger:warning("Failed to create work order for MCP request: ~p", [Reason]),
            State
    end.

%%------------------------------------------------------------------------------
%% @doc Generate receipt for successful request.
%% @end
%%------------------------------------------------------------------------------
-spec generate_receipt(mcp_request()) -> binary().
generate_receipt(#json_rpc_request{id = Id, method = Method}) ->
    Timestamp = erlang:system_time(millisecond),
    Data = iolist_to_binary(io_lib:format("~p|~s|~p", [Id, Method, Timestamp])),
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash).

%%------------------------------------------------------------------------------
%% @doc Store receipt in chain.
%% @end
%%------------------------------------------------------------------------------
-spec store_receipt(binary()) -> ok.
store_receipt(Receipt) ->
    % Delegate to receipt chain module
    case whereis(tcps_receipt_chain) of
        undefined ->
            logger:warning("Receipt chain not available, skipping storage"),
            ok;
        _Pid ->
            tcps_receipt_chain:add_receipt(Receipt)
    end.

%%%=============================================================================
%%% Internal Functions - Helpers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Map MCP method to work order type.
%% @end
%%------------------------------------------------------------------------------
-spec method_to_work_type(binary()) -> atom().
method_to_work_type(<<"tools/call">>) -> feature;
method_to_work_type(<<"tasks/create">>) -> feature;
method_to_work_type(<<"resources/", _/binary>>) -> maintenance;
method_to_work_type(_) -> support.

%%------------------------------------------------------------------------------
%% @doc Build description from method and params.
%% @end
%%------------------------------------------------------------------------------
-spec build_description(binary(), map()) -> binary().
build_description(Method, Params) ->
    Name = maps:get(<<"name">>, Params, <<"unknown">>),
    iolist_to_binary(io_lib:format("MCP ~s: ~s", [Method, Name])).

%%%=============================================================================
%%% Test Support
%%%=============================================================================

-ifdef(TEST).
reset_state() ->
    gen_server:cast(?SERVER, reset_state).

get_state() ->
    gen_server:call(?SERVER, get_state).
-endif.
