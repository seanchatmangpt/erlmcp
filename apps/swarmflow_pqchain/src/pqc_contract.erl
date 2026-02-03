%%%-------------------------------------------------------------------
%%% @doc PQChain Smart Contracts as SwarmFlow Workflow Nets
%%%
%%% Implements smart contracts as deterministic workflow nets instead of
%%% EVM-style re-entrant code. Each contract is a Petri net with:
%%% - Deterministic replay via event logs
%%% - Bounded side effects via workflow semantics
%%% - Explicit compensation paths (SAGA pattern)
%%% - Full audit trail with execution receipts
%%%
%%% Architecture:
%%% - Contract definitions stored in swf_net_registry
%%% - Contract instances run as swf_case gen_statem processes
%%% - Contract state = workflow marking + case variables
%%% - Contract calls = workflow transition firings
%%% - Events captured via swf_event_log
%%%
%%% Address Derivation:
%%% contract_address = SHA3-256(creator_address || nonce)[0..19]
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_contract).

-include("pqchain.hrl").
-include("swarmflow.hrl").

%% API exports
-export([
    deploy/3,
    call/4,
    get_state/1,
    get_code/1,
    validate_transition/3,
    simulate/4,
    get_events/2,
    encode_call/3,
    decode_result/1,
    get_contract/1,
    get_balance/1,
    set_balance/2
]).

%% Internal exports for testing
-export([
    derive_address/2,
    init_contracts_table/0
]).

%%====================================================================
%% Type definitions
%%====================================================================

-type contract_address() :: binary().
-type creator_address() :: binary().
-type nonce() :: non_neg_integer().
-type contract_state() :: #{
    marking := marking(),
    variables := map(),
    balance := non_neg_integer(),
    nonce := non_neg_integer(),
    status := case_status()
}.
-type marking() :: #{binary() => non_neg_integer()}.
-type execution_result() :: #{
    status := success | failure | reverted,
    new_state := contract_state(),
    events := [#contract_event{}],
    gas_used := non_neg_integer(),
    return_value := term() | undefined,
    error := binary() | undefined
}.

-export_type([
    contract_address/0,
    contract_state/0,
    execution_result/0
]).

%% ETS table for contract metadata
-define(CONTRACTS_TABLE, pqc_contracts).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Deploy a new smart contract
%% Creates contract address, registers workflow net with SwarmFlow,
%% initializes contract state, and stores contract metadata.
%%
%% @param Creator The deploying account address
%% @param WorkflowNet The workflow net definition (#swf_net{})
%% @param InitialState Initial contract state (variables, balance)
%% @returns {ok, ContractAddress} | {error, Reason}
-spec deploy(creator_address(), #swf_net{}, map()) ->
    {ok, contract_address()} | {error, term()}.
deploy(Creator, #swf_net{} = WorkflowNet, InitialState) when is_binary(Creator), is_map(InitialState) ->
    try
        %% Ensure contracts table exists
        ensure_contracts_table(),

        %% Get creator's nonce
        Nonce = get_creator_nonce(Creator),

        %% Derive contract address
        ContractAddress = derive_address(Creator, Nonce),

        %% Validate workflow net
        case swf_net_registry:validate_net(WorkflowNet) of
            {ok, _ValidationResult} ->
                %% Register workflow net with SwarmFlow
                NetWithAddress = WorkflowNet#swf_net{
                    id = ContractAddress,
                    metadata = maps:merge(
                        maps:get(metadata, WorkflowNet, #{}),
                        #{
                            contract_address => ContractAddress,
                            creator => Creator,
                            deployed_at => erlang:system_time(millisecond)
                        }
                    )
                },

                case swf_net_registry:register_net(NetWithAddress) of
                    {ok, _RegisteredNet} ->
                        %% Calculate code hash
                        CodeHash = crypto:hash(sha3_256, term_to_binary(WorkflowNet)),

                        %% Create contract record
                        Balance = maps:get(balance, InitialState, 0),
                        Variables = maps:get(variables, InitialState, #{}),

                        Contract = #pqc_contract{
                            address = ContractAddress,
                            creator = Creator,
                            workflow_net_id = ContractAddress,
                            code_hash = CodeHash,
                            state_root = calculate_state_root(NetWithAddress#swf_net.initial_marking, Variables),
                            balance = Balance,
                            nonce = 0,
                            created_at_height = maps:get(block_height, InitialState, 0),
                            metadata = maps:get(metadata, InitialState, #{})
                        },

                        %% Store contract in ETS
                        true = ets:insert(?CONTRACTS_TABLE, {ContractAddress, Contract}),

                        %% Increment creator nonce
                        increment_creator_nonce(Creator),

                        {ok, ContractAddress};

                    {error, Reason} ->
                        {error, {net_registration_failed, Reason}}
                end;

            {error, ValidationErrors} ->
                {error, {invalid_workflow_net, ValidationErrors}}
        end
    catch
        error:Reason:Stacktrace ->
            {error, {deployment_failed, Reason, Stacktrace}}
    end.

%% @doc Call a contract transition
%% Loads contract instance, validates transition, fires it through SwarmFlow,
%% captures state changes and events, returns execution result.
%%
%% @param ContractAddress The contract to call
%% @param Transition The transition (function) to fire
%% @param Args Arguments for the transition
%% @param Caller The calling account address
%% @returns {ok, ExecutionResult} | {error, Reason}
-spec call(contract_address(), atom(), [term()], binary()) ->
    {ok, execution_result()} | {error, term()}.
call(ContractAddress, Transition, Args, Caller) when is_binary(ContractAddress), is_atom(Transition), is_list(Args), is_binary(Caller) ->
    try
        %% Load contract
        case get_contract(ContractAddress) of
            {ok, Contract} ->
                %% Get or create case instance
                case get_or_create_case(Contract, Caller) of
                    {ok, CasePid, CaseId} ->
                        %% Prepare transition arguments in case variables
                        TransitionId = atom_to_binary(Transition, utf8),
                        CallVars = #{
                            caller => Caller,
                            args => Args,
                            timestamp => erlang:system_time(millisecond)
                        },

                        %% Set variables in case
                        _ = [swf_case:set_variable(CasePid, K, V) || {K, V} <- maps:to_list(CallVars)],

                        %% Get state before
                        {ok, StateBefore} = get_case_state(CasePid),
                        GasStart = erlang:monotonic_time(microsecond),

                        %% Fire transition
                        case swf_case:fire_transition(CasePid, TransitionId) of
                            {ok, NewMarking} ->
                                GasUsed = erlang:monotonic_time(microsecond) - GasStart,

                                %% Get state after
                                {ok, StateAfter} = get_case_state(CasePid),

                                %% Get events from this call
                                Events = get_case_events(CaseId, StateBefore, StateAfter),

                                %% Update contract state root
                                NewStateRoot = calculate_state_root(NewMarking, StateAfter),
                                UpdatedContract = Contract#pqc_contract{
                                    state_root = NewStateRoot,
                                    nonce = Contract#pqc_contract.nonce + 1
                                },
                                true = ets:insert(?CONTRACTS_TABLE, {ContractAddress, UpdatedContract}),

                                Result = #{
                                    status => success,
                                    new_state => StateAfter,
                                    events => Events,
                                    gas_used => max(1000, GasUsed div 1000), % Convert to gas units
                                    return_value => maps:get(return_value, StateAfter, undefined),
                                    error => undefined
                                },

                                {ok, Result};

                            {error, not_enabled} ->
                                GasUsed = erlang:monotonic_time(microsecond) - GasStart,
                                {error, {transition_not_enabled, Transition, GasUsed div 1000}};

                            {error, guard_failed} ->
                                GasUsed = erlang:monotonic_time(microsecond) - GasStart,
                                {error, {guard_failed, Transition, GasUsed div 1000}};

                            {error, Reason} ->
                                GasUsed = erlang:monotonic_time(microsecond) - GasStart,
                                ErrorResult = #{
                                    status => failure,
                                    new_state => StateBefore,
                                    events => [],
                                    gas_used => GasUsed div 1000,
                                    return_value => undefined,
                                    error => iolist_to_binary(io_lib:format("~p", [Reason]))
                                },
                                {ok, ErrorResult}
                        end;

                    {error, Reason} ->
                        {error, {case_creation_failed, Reason}}
                end;

            {error, not_found} ->
                {error, contract_not_found}
        end
    catch
        error:Reason:Stacktrace ->
            {error, {call_failed, Reason, Stacktrace}}
    end.

%% @doc Get contract current state (marking + variables)
-spec get_state(contract_address()) ->
    {ok, contract_state()} | {error, term()}.
get_state(ContractAddress) when is_binary(ContractAddress) ->
    try
        case get_contract(ContractAddress) of
            {ok, Contract} ->
                case get_case_pid(ContractAddress) of
                    {ok, CasePid} ->
                        get_case_state(CasePid);
                    {error, not_found} ->
                        %% Return initial state if case not started
                        case swf_net_registry:get_net(Contract#pqc_contract.workflow_net_id) of
                            {ok, Net} ->
                                InitialState = #{
                                    marking => Net#swf_net.initial_marking,
                                    variables => #{},
                                    balance => Contract#pqc_contract.balance,
                                    nonce => Contract#pqc_contract.nonce,
                                    status => created
                                },
                                {ok, InitialState};
                            {error, Reason} ->
                                {error, {net_not_found, Reason}}
                        end
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Reason:Stacktrace ->
            {error, {get_state_failed, Reason, Stacktrace}}
    end.

%% @doc Get contract workflow net definition
-spec get_code(contract_address()) ->
    {ok, #swf_net{}} | {error, term()}.
get_code(ContractAddress) when is_binary(ContractAddress) ->
    case get_contract(ContractAddress) of
        {ok, Contract} ->
            swf_net_registry:get_net(Contract#pqc_contract.workflow_net_id);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Validate if a transition can fire in current state
-spec validate_transition(contract_address(), atom(), [term()]) ->
    {ok, enabled} | {error, not_enabled | term()}.
validate_transition(ContractAddress, Transition, Args) when is_binary(ContractAddress), is_atom(Transition), is_list(Args) ->
    try
        case get_contract(ContractAddress) of
            {ok, _Contract} ->
                case get_case_pid(ContractAddress) of
                    {ok, CasePid} ->
                        TransitionId = atom_to_binary(Transition, utf8),
                        {ok, EnabledTransitions} = swf_case:get_enabled_transitions(CasePid),
                        case lists:member(TransitionId, EnabledTransitions) of
                            true -> {ok, enabled};
                            false -> {error, not_enabled}
                        end;
                    {error, not_found} ->
                        {error, contract_not_initialized}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Reason:Stacktrace ->
            {error, {validation_failed, Reason, Stacktrace}}
    end.

%% @doc Simulate a contract call without committing state changes
%% Creates a temporary case, fires transition, captures result, cleans up
-spec simulate(contract_address(), atom(), [term()], binary()) ->
    {ok, execution_result()} | {error, term()}.
simulate(ContractAddress, Transition, Args, Caller) when is_binary(ContractAddress), is_atom(Transition), is_list(Args), is_binary(Caller) ->
    try
        case get_contract(ContractAddress) of
            {ok, Contract} ->
                %% Get current state
                {ok, CurrentState} = get_state(ContractAddress),

                %% Create temporary case for simulation
                NetId = Contract#pqc_contract.workflow_net_id,
                SimVars = maps:merge(
                    maps:get(variables, CurrentState, #{}),
                    #{
                        caller => Caller,
                        args => Args,
                        simulation => true
                    }
                ),

                case swf_case:start_link(NetId, SimVars, #{simulation => true}) of
                    {ok, SimPid} ->
                        try
                            %% Set marking to current state
                            CurrentMarking = maps:get(marking, CurrentState, #{}),
                            %% Note: swf_case doesn't expose set_marking, so we simulate from initial

                            TransitionId = atom_to_binary(Transition, utf8),
                            GasStart = erlang:monotonic_time(microsecond),

                            case swf_case:fire_transition(SimPid, TransitionId) of
                                {ok, NewMarking} ->
                                    GasUsed = erlang:monotonic_time(microsecond) - GasStart,
                                    {ok, SimState} = get_case_state(SimPid),

                                    Result = #{
                                        status => success,
                                        new_state => SimState#{marking => NewMarking},
                                        events => [], % Events not captured in simulation
                                        gas_used => max(1000, GasUsed div 1000),
                                        return_value => maps:get(return_value, SimState, undefined),
                                        error => undefined
                                    },
                                    {ok, Result};

                                {error, Reason} ->
                                    GasUsed = erlang:monotonic_time(microsecond) - GasStart,
                                    ErrorResult = #{
                                        status => failure,
                                        new_state => CurrentState,
                                        events => [],
                                        gas_used => GasUsed div 1000,
                                        return_value => undefined,
                                        error => iolist_to_binary(io_lib:format("~p", [Reason]))
                                    },
                                    {ok, ErrorResult}
                            end
                        after
                            %% Clean up simulation case
                            catch gen_statem:stop(SimPid)
                        end;

                    {error, Reason} ->
                        {error, {simulation_case_failed, Reason}}
                end;

            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Reason:Stacktrace ->
            {error, {simulation_failed, Reason, Stacktrace}}
    end.

%% @doc Get contract events in block range
%% Queries swf_event_log for events from contract's case
-spec get_events(contract_address(), {non_neg_integer(), non_neg_integer()}) ->
    {ok, [#contract_event{}]} | {error, term()}.
get_events(ContractAddress, {FromBlock, ToBlock}) when is_binary(ContractAddress), is_integer(FromBlock), is_integer(ToBlock) ->
    try
        case get_case_id(ContractAddress) of
            {ok, CaseId} ->
                %% Query SwarmFlow event log
                case swf_event_log:get_case_events(CaseId) of
                    {ok, SwfEvents} ->
                        %% Convert SwarmFlow events to contract events
                        ContractEvents = convert_swf_events_to_contract_events(
                            SwfEvents,
                            ContractAddress,
                            FromBlock,
                            ToBlock
                        ),
                        {ok, ContractEvents};

                    {error, Reason} ->
                        {error, {event_log_query_failed, Reason}}
                end;

            {error, not_found} ->
                {ok, []} % No events if contract not initialized
        end
    catch
        error:Reason:Stacktrace ->
            {error, {get_events_failed, Reason, Stacktrace}}
    end.

%% @doc Encode contract call for transaction payload
-spec encode_call(atom(), [term()], [binary()]) -> #contract_call_payload{}.
encode_call(Transition, Arguments, Artifacts) when is_atom(Transition), is_list(Arguments), is_list(Artifacts) ->
    #contract_call_payload{
        contract_address = undefined, % Set by caller
        transition = Transition,
        arguments = Arguments,
        artifacts = Artifacts
    }.

%% @doc Decode execution result from receipt
-spec decode_result(#execution_receipt{}) ->
    {ok, execution_result()} | {error, term()}.
decode_result(#execution_receipt{} = Receipt) ->
    try
        Result = #{
            status => Receipt#execution_receipt.status,
            new_state => #{}, % Would need to reconstruct from state_changes
            events => Receipt#execution_receipt.events,
            gas_used => Receipt#execution_receipt.gas_used,
            return_value => undefined,
            error => Receipt#execution_receipt.error
        },
        {ok, Result}
    catch
        error:Reason:Stacktrace ->
            {error, {decode_failed, Reason, Stacktrace}}
    end.

%% @doc Get contract record
-spec get_contract(contract_address()) ->
    {ok, #pqc_contract{}} | {error, not_found}.
get_contract(ContractAddress) when is_binary(ContractAddress) ->
    ensure_contracts_table(),
    case ets:lookup(?CONTRACTS_TABLE, ContractAddress) of
        [{ContractAddress, Contract}] -> {ok, Contract};
        [] -> {error, not_found}
    end.

%% @doc Get contract balance
-spec get_balance(contract_address()) ->
    {ok, non_neg_integer()} | {error, term()}.
get_balance(ContractAddress) when is_binary(ContractAddress) ->
    case get_contract(ContractAddress) of
        {ok, Contract} -> {ok, Contract#pqc_contract.balance};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Set contract balance (for transfers)
-spec set_balance(contract_address(), non_neg_integer()) ->
    ok | {error, term()}.
set_balance(ContractAddress, NewBalance) when is_binary(ContractAddress), is_integer(NewBalance), NewBalance >= 0 ->
    case get_contract(ContractAddress) of
        {ok, Contract} ->
            UpdatedContract = Contract#pqc_contract{balance = NewBalance},
            true = ets:insert(?CONTRACTS_TABLE, {ContractAddress, UpdatedContract}),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Derive contract address from creator and nonce
%% Uses SHA3-256(creator_address || nonce), takes first 20 bytes
-spec derive_address(creator_address(), nonce()) -> contract_address().
derive_address(Creator, Nonce) when is_binary(Creator), is_integer(Nonce) ->
    %% Encode nonce as 64-bit big-endian
    NonceBytes = <<Nonce:64/big-unsigned-integer>>,
    %% Hash creator || nonce
    Hash = crypto:hash(sha3_256, <<Creator/binary, NonceBytes/binary>>),
    %% Take first 20 bytes as address
    <<Address:20/binary, _Rest/binary>> = Hash,
    Address.

%% @doc Initialize contracts ETS table
-spec init_contracts_table() -> ok.
init_contracts_table() ->
    case ets:whereis(?CONTRACTS_TABLE) of
        undefined ->
            _Tid = ets:new(?CONTRACTS_TABLE, [
                named_table,
                public,
                set,
                {keypos, 1},
                {read_concurrency, true},
                {write_concurrency, true}
            ]),
            ok;
        _Tid ->
            ok
    end.

%% @doc Ensure contracts table exists
-spec ensure_contracts_table() -> ok.
ensure_contracts_table() ->
    init_contracts_table().

%% @doc Get creator nonce for address derivation
-spec get_creator_nonce(creator_address()) -> nonce().
get_creator_nonce(Creator) when is_binary(Creator) ->
    %% In a real implementation, this would query account state
    %% For now, count existing contracts from this creator
    ensure_contracts_table(),
    MS = ets:fun2ms(fun({_Addr, #pqc_contract{creator = C}}) when C =:= Creator -> true end),
    ets:select_count(?CONTRACTS_TABLE, MS).

%% @doc Increment creator nonce (no-op in current implementation)
-spec increment_creator_nonce(creator_address()) -> ok.
increment_creator_nonce(_Creator) ->
    %% In a real implementation, this would update account state
    ok.

%% @doc Calculate state root from marking and variables
-spec calculate_state_root(marking(), map()) -> binary().
calculate_state_root(Marking, Variables) ->
    StateBinary = term_to_binary({Marking, Variables}),
    crypto:hash(sha3_256, StateBinary).

%% @doc Get or create workflow case for contract
-spec get_or_create_case(#pqc_contract{}, binary()) ->
    {ok, pid(), binary()} | {error, term()}.
get_or_create_case(#pqc_contract{address = ContractAddress, workflow_net_id = NetId}, _Caller) ->
    %% Check if case already exists
    case get_case_pid(ContractAddress) of
        {ok, Pid} ->
            {ok, CaseRecord} = swf_case:get_case_record(Pid),
            {ok, Pid, CaseRecord#swf_case.id};

        {error, not_found} ->
            %% Create new case
            CaseId = generate_case_id(ContractAddress),
            InitialVars = #{contract_address => ContractAddress},

            case swf_case:start_link(NetId, InitialVars, #{case_id => CaseId}) of
                {ok, Pid} ->
                    %% Register case PID
                    register_case_pid(ContractAddress, Pid, CaseId),
                    {ok, Pid, CaseId};

                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Get case PID for contract
-spec get_case_pid(contract_address()) -> {ok, pid()} | {error, not_found}.
get_case_pid(ContractAddress) ->
    %% In a real implementation, this would use gproc or similar registry
    %% For now, use process dictionary or ETS
    case erlang:get({contract_case_pid, ContractAddress}) of
        undefined -> {error, not_found};
        Pid when is_pid(Pid) ->
            case erlang:is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, not_found}
            end
    end.

%% @doc Register case PID for contract
-spec register_case_pid(contract_address(), pid(), binary()) -> ok.
register_case_pid(ContractAddress, Pid, CaseId) ->
    erlang:put({contract_case_pid, ContractAddress}, Pid),
    erlang:put({contract_case_id, ContractAddress}, CaseId),
    ok.

%% @doc Get case ID for contract
-spec get_case_id(contract_address()) -> {ok, binary()} | {error, not_found}.
get_case_id(ContractAddress) ->
    case erlang:get({contract_case_id, ContractAddress}) of
        undefined -> {error, not_found};
        CaseId -> {ok, CaseId}
    end.

%% @doc Generate case ID for contract
-spec generate_case_id(contract_address()) -> binary().
generate_case_id(ContractAddress) ->
    %% Use contract address + timestamp as case ID
    Timestamp = erlang:system_time(millisecond),
    Hash = crypto:hash(sha3_256, <<ContractAddress/binary, Timestamp:64>>),
    base64:encode(Hash).

%% @doc Get case state (marking + variables)
-spec get_case_state(pid()) -> {ok, contract_state()} | {error, term()}.
get_case_state(CasePid) ->
    try
        {ok, Marking} = swf_case:get_marking(CasePid),
        {ok, Variables} = swf_case:get_variables(CasePid),
        {ok, CaseRecord} = swf_case:get_case_record(CasePid),

        State = #{
            marking => Marking,
            variables => Variables,
            balance => maps:get(balance, Variables, 0),
            nonce => maps:get(nonce, Variables, 0),
            status => CaseRecord#swf_case.status
        },
        {ok, State}
    catch
        _:Reason ->
            {error, Reason}
    end.

%% @doc Get events from case execution
-spec get_case_events(binary(), contract_state(), contract_state()) -> [#contract_event{}].
get_case_events(_CaseId, _StateBefore, _StateAfter) ->
    %% In a real implementation, this would query swf_event_log
    %% and convert events to contract events
    %% For now, return empty list
    [].

%% @doc Convert SwarmFlow events to contract events
-spec convert_swf_events_to_contract_events([#swf_event{}], contract_address(), non_neg_integer(), non_neg_integer()) ->
    [#contract_event{}].
convert_swf_events_to_contract_events(SwfEvents, ContractAddress, FromBlock, ToBlock) ->
    lists:filtermap(
        fun(#swf_event{} = Event) ->
            %% Convert event timestamp to block height (simplified)
            BlockHeight = Event#swf_event.timestamp div 1000000,

            if
                BlockHeight >= FromBlock andalso BlockHeight =< ToBlock ->
                    ContractEvent = #contract_event{
                        contract_address = ContractAddress,
                        name = Event#swf_event.event_type,
                        data = Event#swf_event.data,
                        indexed = extract_indexed_fields(Event#swf_event.data),
                        tx_id = maps:get(tx_id, Event#swf_event.data, <<>>),
                        block_height = BlockHeight,
                        log_index = Event#swf_event.sequence
                    },
                    {true, ContractEvent};
                true ->
                    false
            end
        end,
        SwfEvents
    ).

%% @doc Extract indexed fields from event data
-spec extract_indexed_fields(map()) -> [term()].
extract_indexed_fields(Data) when is_map(Data) ->
    %% Extract commonly indexed fields
    [
        maps:get(caller, Data, undefined),
        maps:get(transition_id, Data, undefined),
        maps:get(place_id, Data, undefined)
    ].
