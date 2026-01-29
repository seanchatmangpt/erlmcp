%%%-------------------------------------------------------------------
%% @doc MCP+ Governance Coordinator - Unified Governance Interface
%%
%% Coordinates all governance components:
%% - Contract validation and enforcement
%% - Envelope limits and monitoring
%% - Receipt generation and chain management
%% - Evidence bundle creation
%% - Kill switch integration
%%
%% Provides a single entry point for governed operations:
%%   execute(Contract, Envelope, Operation, Args) -> Result | Refusal
%%
%% All operations produce receipts for text-blind verification.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_governance).

-include("erlmcp_governance.hrl").
-include("erlmcp_refusal.hrl").

%% API - Execution
-export([
    execute/4,
    execute_with_context/5
]).

%% API - Configuration
-export([
    configure/1,
    get_config/0
]).

%% API - Status
-export([
    status/0,
    health_check/0
]).

%% API - RACI Roles
-export([
    assign_role/3,
    get_role/2,
    list_roles/0
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/1]).

%%====================================================================
%% Types
%%====================================================================

-type operation() :: binary().
-type args() :: map().
-type execution_result() :: {ok, term(), #mcp_receipt{}} | {refused, #refusal{}, #mcp_receipt{}}.

-type raci_role() :: responsible | accountable | consulted | informed.
-type role_assignment() :: #{
    entity := binary(),
    role := raci_role(),
    scope := binary()
}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
    config :: map(),
    roles :: #{binary() => role_assignment()},
    metrics :: governance_metrics()
}).

-record(governance_metrics, {
    total_executions :: pos_integer(),
    total_refusals :: pos_integer(),
    last_execution :: timestamp_ms() | undefined,
    uptime_start :: timestamp_ms()
}).

-type governance_metrics() :: #governance_metrics{}.

%%====================================================================
%% API - Execution
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Execute an operation under governance.
%% Validates contract, enforces envelope, generates receipt.
-spec execute(#mcp_contract{}, #mcp_envelope{}, operation(), args()) -> execution_result().
execute(Contract, Envelope, Operation, Args) ->
    Context = #{
        contract_family => Contract#mcp_contract.family,
        epoch => Contract#mcp_contract.epoch
    },
    execute_with_context(Contract, Envelope, Operation, Args, Context).

%% @doc Execute with additional context.
-spec execute_with_context(#mcp_contract{}, #mcp_envelope{}, operation(), args(), map()) ->
    execution_result().
execute_with_context(Contract, Envelope, Operation, Args, Context) ->
    gen_server:call(?MODULE, {execute, Contract, Envelope, Operation, Args, Context}).

%%====================================================================
%% API - Configuration
%%====================================================================

%% @doc Configure governance parameters.
-spec configure(map()) -> ok.
configure(Config) ->
    gen_server:call(?MODULE, {configure, Config}).

%% @doc Get current configuration.
-spec get_config() -> map().
get_config() ->
    gen_server:call(?MODULE, get_config).

%%====================================================================
%% API - Status
%%====================================================================

%% @doc Get governance status.
-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

%% @doc Perform health check.
-spec health_check() -> ok | {degraded, [binary()]} | {down, [binary()]}.
health_check() ->
    gen_server:call(?MODULE, health_check).

%%====================================================================
%% API - RACI Roles
%%====================================================================

%% @doc Assign a RACI role to an entity.
-spec assign_role(binary(), raci_role(), binary()) -> ok.
assign_role(Entity, Role, Scope) ->
    gen_server:call(?MODULE, {assign_role, Entity, Role, Scope}).

%% @doc Get role for an entity.
-spec get_role(binary(), binary()) -> {ok, raci_role()} | {error, not_found}.
get_role(Entity, Scope) ->
    gen_server:call(?MODULE, {get_role, Entity, Scope}).

%% @doc List all role assignments.
-spec list_roles() -> [role_assignment()].
list_roles() ->
    gen_server:call(?MODULE, list_roles).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Config]) ->
    Now = erlang:system_time(millisecond),
    Metrics = #governance_metrics{
        total_executions = 0,
        total_refusals = 0,
        last_execution = undefined,
        uptime_start = Now
    },
    State = #state{
        config = Config,
        roles = #{},
        metrics = Metrics
    },
    logger:info("MCP+ Governance coordinator started"),
    {ok, State}.

handle_call({execute, Contract, Envelope, Operation, Args, Context}, _From, State) ->
    StartTime = erlang:system_time(microsecond),
    RequestId = generate_request_id(),

    %% Step 1: Check kill switches
    Capabilities = Contract#mcp_contract.capabilities,
    KillCheck = erlmcp_kill_switch:check_all(Context, Capabilities),

    Result = case KillCheck of
        {refused, KillSwitch} ->
            %% Kill switch active - immediate refusal
            Refusal = make_kill_switch_refusal(KillSwitch),
            {refused, Refusal};
        ok ->
            %% Step 2: Validate contract
            case erlmcp_contract:validate(Contract) of
                {error, ContractError} ->
                    Refusal = make_contract_refusal(ContractError),
                    {refused, Refusal};
                ok ->
                    %% Step 3: Check preconditions
                    case erlmcp_contract:check_preconditions(Contract, Args) of
                        {error, PreError} ->
                            Refusal = make_precondition_refusal(PreError),
                            {refused, Refusal};
                        ok ->
                            %% Step 4: Enforce envelope (pre-execution)
                            EnvelopeContext = Context#{
                                operation => Operation,
                                payload_bytes => estimate_payload_size(Args),
                                recursion_depth => maps:get(recursion_depth, Context, 0)
                            },
                            case erlmcp_envelope:enforce(Envelope, EnvelopeContext) of
                                {refused, EnvRefusal} ->
                                    {refused, EnvRefusal};
                                ok ->
                                    %% Step 5: Execute operation
                                    execute_operation(Operation, Args)
                            end
                    end
            end
    end,

    %% Calculate metrics
    EndTime = erlang:system_time(microsecond),
    DurationUs = EndTime - StartTime,
    {MemoryBytes, CpuUs, IoBytes} = estimate_resource_usage(),

    %% Step 6: Generate receipt
    {Outcome, RefusalCode, ResponsePayload} = case Result of
        {ok, Response} -> {ok, undefined, Response};
        {refused, Ref} -> {refused, Ref#refusal.code, #{error => Ref#refusal.message}}
    end,

    ReceiptOpts = #{
        request_id => RequestId,
        contract_id => Contract#mcp_contract.id,
        envelope_id => Envelope#mcp_envelope.id,
        method => Operation,
        request_payload => Args,
        response_payload => ResponsePayload
    },
    ReceiptMetrics = #{
        duration_us => DurationUs,
        memory_bytes => MemoryBytes,
        cpu_us => CpuUs,
        io_bytes => IoBytes
    },

    {ok, Receipt} = erlmcp_receipt:create(Outcome, RefusalCode, ReceiptOpts, ReceiptMetrics),

    %% Update metrics
    NewMetrics = update_metrics(State#state.metrics, Outcome),
    NewState = State#state{metrics = NewMetrics},

    %% Return result with receipt
    FinalResult = case Result of
        {ok, Resp} -> {ok, Resp, Receipt};
        {refused, Refusal} -> {refused, Refusal, Receipt}
    end,

    {reply, FinalResult, NewState};

handle_call({configure, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),
    {reply, ok, State#state{config = MergedConfig}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(status, _From, State) ->
    Now = erlang:system_time(millisecond),
    Metrics = State#state.metrics,
    Status = #{
        <<"status">> => <<"running">>,
        <<"uptime_ms">> => Now - Metrics#governance_metrics.uptime_start,
        <<"total_executions">> => Metrics#governance_metrics.total_executions,
        <<"total_refusals">> => Metrics#governance_metrics.total_refusals,
        <<"refusal_rate">> => calculate_refusal_rate(Metrics),
        <<"last_execution">> => Metrics#governance_metrics.last_execution,
        <<"kill_switches_active">> => length(erlmcp_kill_switch:get_active_switches()),
        <<"drill_active">> => erlmcp_kill_switch:is_drill_active()
    },
    {reply, Status, State};

handle_call(health_check, _From, State) ->
    Checks = [
        {<<"contract_registry">>, check_component(erlmcp_contract)},
        {<<"envelope_enforcer">>, check_component(erlmcp_envelope)},
        {<<"receipt_manager">>, check_component(erlmcp_receipt)},
        {<<"evidence_bundler">>, check_component(erlmcp_evidence_bundle)},
        {<<"verifier">>, check_component(erlmcp_verifier)},
        {<<"kill_switch">>, check_component(erlmcp_kill_switch)}
    ],

    Failures = [Name || {Name, Status} <- Checks, Status =/= ok],
    Result = case Failures of
        [] -> ok;
        _ when length(Failures) < 3 -> {degraded, Failures};
        _ -> {down, Failures}
    end,
    {reply, Result, State};

handle_call({assign_role, Entity, Role, Scope}, _From, State) ->
    Key = <<Entity/binary, ":", Scope/binary>>,
    Assignment = #{entity => Entity, role => Role, scope => Scope},
    NewRoles = maps:put(Key, Assignment, State#state.roles),
    {reply, ok, State#state{roles = NewRoles}};

handle_call({get_role, Entity, Scope}, _From, State) ->
    Key = <<Entity/binary, ":", Scope/binary>>,
    case maps:get(Key, State#state.roles, undefined) of
        undefined -> {reply, {error, not_found}, State};
        #{role := Role} -> {reply, {ok, Role}, State}
    end;

handle_call(list_roles, _From, State) ->
    Roles = maps:values(State#state.roles),
    {reply, Roles, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    logger:info("MCP+ Governance coordinator stopping"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_request_id() -> binary().
generate_request_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    base64:encode(Bytes).

-spec make_kill_switch_refusal(#mcp_kill_switch{}) -> #refusal{}.
make_kill_switch_refusal(#mcp_kill_switch{scope = Scope} = Switch) ->
    Code = case Scope of
        global -> ?REFUSAL_KILL_SWITCH_GLOBAL;
        family -> ?REFUSAL_KILL_SWITCH_FAMILY;
        capability -> ?REFUSAL_KILL_SWITCH_CAPABILITY;
        epoch -> ?REFUSAL_KILL_SWITCH_EPOCH
    end,
    #refusal{
        code = Code,
        http_status = 503,
        message = <<"Kill switch active">>,
        hint = <<"Operations suspended - contact administrator">>,
        severity = critical,
        details = #{
            scope => Scope,
            activated_at => Switch#mcp_kill_switch.activated_at,
            reason_code => Switch#mcp_kill_switch.reason_code
        },
        timestamp = erlang:system_time(millisecond)
    }.

-spec make_contract_refusal({pos_integer(), binary(), map()}) -> #refusal{}.
make_contract_refusal({Code, Message, Details}) ->
    #refusal{
        code = Code,
        http_status = 403,
        message = Message,
        hint = <<"Check contract validity and signature">>,
        severity = error,
        details = Details,
        timestamp = erlang:system_time(millisecond)
    }.

-spec make_precondition_refusal({pos_integer(), binary(), map()}) -> #refusal{}.
make_precondition_refusal({Code, Message, Details}) ->
    #refusal{
        code = Code,
        http_status = 400,
        message = Message,
        hint = <<"Verify input meets contract preconditions">>,
        severity = error,
        details = Details,
        timestamp = erlang:system_time(millisecond)
    }.

-spec execute_operation(operation(), args()) -> {ok, term()} | {refused, #refusal{}}.
execute_operation(_Operation, _Args) ->
    %% In production, dispatch to actual operation handler
    %% For now, return success placeholder
    {ok, #{status => <<"completed">>}}.

-spec estimate_payload_size(map()) -> pos_integer().
estimate_payload_size(Args) when is_map(Args) ->
    %% Rough estimate based on JSON encoding
    try
        byte_size(jsx:encode(Args))
    catch
        _:_ -> 0
    end.

-spec estimate_resource_usage() -> {pos_integer(), pos_integer(), pos_integer()}.
estimate_resource_usage() ->
    %% In production, would use actual process metrics
    {Info} = erlang:process_info(self(), [memory]),
    Memory = proplists:get_value(memory, Info, 0),
    %% Estimates for CPU and IO
    {Memory, 100, 0}.

-spec update_metrics(governance_metrics(), ok | refused) -> governance_metrics().
update_metrics(Metrics, Outcome) ->
    Now = erlang:system_time(millisecond),
    NewExecutions = Metrics#governance_metrics.total_executions + 1,
    NewRefusals = case Outcome of
        refused -> Metrics#governance_metrics.total_refusals + 1;
        ok -> Metrics#governance_metrics.total_refusals
    end,
    Metrics#governance_metrics{
        total_executions = NewExecutions,
        total_refusals = NewRefusals,
        last_execution = Now
    }.

-spec calculate_refusal_rate(governance_metrics()) -> float().
calculate_refusal_rate(#governance_metrics{total_executions = 0}) ->
    0.0;
calculate_refusal_rate(#governance_metrics{total_executions = Total, total_refusals = Refusals}) ->
    Refusals / Total.

-spec check_component(atom()) -> ok | error.
check_component(Module) ->
    try
        case whereis(Module) of
            undefined -> error;
            Pid when is_pid(Pid) ->
                case is_process_alive(Pid) of
                    true -> ok;
                    false -> error
                end
        end
    catch
        _:_ -> error
    end.
