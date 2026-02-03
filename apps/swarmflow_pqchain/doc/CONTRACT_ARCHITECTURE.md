# PQChain Smart Contracts: Workflow Nets Instead of Re-entrant Code

## Overview

PQChain smart contracts are implemented as **SwarmFlow workflow nets** (Petri nets with YAWL semantics), not EVM-style re-entrant code. This design provides:

- **Deterministic replay** via event logs
- **Bounded side effects** via workflow semantics
- **Explicit compensation paths** (SAGA pattern)
- **Full audit trail** with execution receipts

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    PQChain Contract Layer                    │
├─────────────────────────────────────────────────────────────┤
│  Contract Address  │  Creator + Nonce → SHA3-256[0..19]     │
│  Contract Storage  │  ETS table: pqc_contracts              │
│  State Management  │  Workflow marking + case variables     │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                   SwarmFlow Integration                      │
├─────────────────────────────────────────────────────────────┤
│  Net Registry      │  swf_net_registry (workflow defs)      │
│  Case Execution    │  swf_case (gen_statem per instance)    │
│  Event Logging     │  swf_event_log (audit trail)           │
│  Conformance       │  swf_conformance (validation)          │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    Blockchain Layer                          │
├─────────────────────────────────────────────────────────────┤
│  Transactions      │  TX_TYPE_CONTRACT_DEPLOY/_CALL         │
│  Receipts          │  Execution results with events         │
│  State Root        │  SHA3-256(marking, variables)          │
└─────────────────────────────────────────────────────────────┘
```

## Contract Lifecycle

### 1. Deployment

```erlang
%% Create workflow net (token contract example)
TokenNet = #swf_net{
    id = <<"token">>,
    name = <<"ERC20-like Token">>,
    places = #{
        <<"initial">> => #swf_place{tokens = 1},
        <<"balance">> => #swf_place{tokens = 0},
        <<"approved">> => #swf_place{tokens = 0}
    },
    transitions = #{
        <<"transfer">> => #swf_transition{
            kind = manual,
            guard = fun(#{balance := Balance, amount := Amount}) ->
                Balance >= Amount
            end,
            action = fun(Vars) ->
                Balance = maps:get(balance, Vars),
                Amount = maps:get(amount, Vars),
                {ok, Vars#{balance => Balance - Amount}}
            end
        }
    },
    ...
},

%% Deploy contract
Creator = <<0:160>>,  % 20-byte address
InitialState = #{
    balance => 1000000,
    variables => #{total_supply => 1000000}
},
{ok, ContractAddress} = pqc_contract:deploy(Creator, TokenNet, InitialState).
```

**What happens:**
1. Contract address derived: `SHA3-256(Creator || Nonce)[0..19]`
2. Workflow net registered in `swf_net_registry`
3. Contract metadata stored in ETS table `pqc_contracts`
4. Initial state root calculated: `SHA3-256(initial_marking, variables)`
5. Creator nonce incremented

### 2. Contract Calls

```erlang
%% Call contract transition
Caller = <<1:160>>,
Args = [
    Recipient = <<2:160>>,
    Amount = 1000
],
{ok, Result} = pqc_contract:call(ContractAddress, transfer, Args, Caller).
```

**What happens:**
1. Load contract from ETS
2. Get or create SwarmFlow case (workflow instance)
3. Validate transition is enabled (guard passes)
4. Fire transition through `swf_case:fire_transition/2`
5. Capture state changes (new marking, updated variables)
6. Extract events from SwarmFlow event log
7. Update contract state root
8. Return execution receipt

**Result structure:**
```erlang
#{
    status => success | failure | reverted,
    new_state => #{
        marking => #{PlaceId => TokenCount},
        variables => #{Key => Value},
        balance => ContractBalance,
        nonce => ContractNonce
    },
    events => [#contract_event{}],
    gas_used => GasUnits,
    return_value => Term,
    error => Binary | undefined
}
```

### 3. State Queries

```erlang
%% Get current contract state
{ok, State} = pqc_contract:get_state(ContractAddress).
%% State = #{
%%     marking => #{<<"balance">> => 999000},
%%     variables => #{total_supply => 1000000, ...},
%%     balance => 0,
%%     nonce => 1,
%%     status => running
%% }

%% Get contract code (workflow net)
{ok, Net} = pqc_contract:get_code(ContractAddress).

%% Get contract balance
{ok, Balance} = pqc_contract:get_balance(ContractAddress).
```

### 4. Validation & Simulation

```erlang
%% Check if transition can fire
{ok, enabled} = pqc_contract:validate_transition(
    ContractAddress,
    transfer,
    [Recipient, Amount]
).

%% Simulate call without committing (dry-run)
{ok, SimResult} = pqc_contract:simulate(
    ContractAddress,
    transfer,
    [Recipient, Amount],
    Caller
).
%% SimResult has same structure as call result, but state not persisted
```

### 5. Event Queries

```erlang
%% Get contract events in block range
{ok, Events} = pqc_contract:get_events(ContractAddress, {FromBlock, ToBlock}).
%% Events = [
%%     #contract_event{
%%         contract_address = ContractAddress,
%%         name = transition_fired,
%%         data = #{transition_id => <<"transfer">>, ...},
%%         indexed = [Caller, TransitionId, PlaceId],
%%         tx_id = TxId,
%%         block_height = 123,
%%         log_index = 0
%%     }
%% ]
```

## Key Design Principles

### 1. Deterministic Replay

Every contract execution is recorded in the SwarmFlow event log:
- `case_created` - Contract instance initialized
- `transition_enabled` - Function became callable
- `transition_fired` - Function executed
- `token_produced` - State updated
- `variable_set` - Storage written

Given the same event log, the contract state can be reconstructed identically.

### 2. Bounded Side Effects

Unlike EVM re-entrancy:
- Each transition has explicit **input places** (preconditions)
- Each transition has explicit **output places** (postconditions)
- No hidden state mutations
- No call stack depth issues
- No DAO-style re-entrancy attacks

### 3. Explicit Compensation

Contracts can define compensation handlers (SAGA pattern):
```erlang
#swf_compensation{
    transition_id = <<"transfer">>,
    compensation_transition_id = <<"rollback_transfer">>,
    scope = saga,
    order = lifo  % Compensate in reverse order
}
```

### 4. Audit Trail

Every contract action produces:
- **Execution receipt** (`#execution_receipt{}`)
- **State changes** (`#state_change{}`)
- **Events** (`#contract_event{}`)
- **SwarmFlow events** (`#swf_event{}`)

## Transaction Integration

### Deploy Transaction

```erlang
#pqc_transaction{
    type = ?TX_TYPE_CONTRACT_DEPLOY,
    payload = #contract_deploy_payload{
        workflow_net = Net,
        initial_state = InitialState,
        constructor_args = []
    },
    signature = #pqc_signature{...}
}
```

### Call Transaction

```erlang
#pqc_transaction{
    type = ?TX_TYPE_CONTRACT_CALL,
    payload = #contract_call_payload{
        contract_address = ContractAddress,
        transition = transfer,
        arguments = [Recipient, Amount],
        artifacts = []  % A2A artifact references
    },
    signature = #pqc_signature{...}
}
```

## Storage Model

### Contract Metadata (ETS)

```erlang
#pqc_contract{
    address,           % 20 bytes, derived from creator + nonce
    creator,           % Deploying account
    workflow_net_id,   % SwarmFlow net ID
    code_hash,         % SHA3-256(workflow_net)
    state_root,        % SHA3-256(marking, variables)
    balance,           % Native token balance
    nonce,             % Contract call counter
    created_at_height, % Block height at deployment
    metadata           % Custom contract data
}
```

### Workflow Net (SwarmFlow Registry)

```erlang
#swf_net{
    id = ContractAddress,  % Net ID = contract address
    places,                % State locations
    transitions,           % Functions
    arcs,                  % Flow edges
    initial_marking,       % Initial state
    final_places,          % Terminal states
    metadata               % Contract metadata
}
```

### Contract Instance (SwarmFlow Case)

```erlang
#swf_case{
    id = CaseId,           % UUID per contract instance
    net_id = ContractAddress,
    marking,               % Current token distribution
    variables,             % Contract storage
    status,                % created | running | completed | failed
    ...
}
```

## Gas Model

Gas is calculated based on:
- **Transition complexity**: Guard evaluation + action execution
- **State size**: Marking size + variable count
- **Event count**: Number of events emitted
- **Compensation depth**: Number of compensation handlers

```erlang
GasUsed = BaseGas +
          (GuardComplexity * GasPerGuard) +
          (ActionComplexity * GasPerAction) +
          (StateSize * GasPerByte) +
          (EventCount * GasPerEvent)
```

## Comparison: EVM vs. Workflow Nets

| Feature | EVM | Workflow Nets |
|---------|-----|---------------|
| Execution model | Re-entrant stack | Petri net transitions |
| State model | Key-value storage | Marking + variables |
| Re-entrancy | Vulnerable (DAO attack) | Structurally prevented |
| Audit trail | Events only | Full event log + replay |
| Compensation | Manual | Built-in SAGA |
| Determinism | Yes | Yes |
| Formal verification | Difficult | Petri net theory |
| Tooling | Solidity, Vyper | SwarmFlow nets |

## Advanced Features

### Multi-Instance Transitions (YAWL)

```erlang
#swf_yawl_task{
    mi_minimum = 1,
    mi_maximum = 100,
    mi_threshold = 50,  % Complete when 50 instances done
    mi_creation = dynamic
}
```

### Timeouts

```erlang
#swf_transition{
    timeout_ms = 60000,  % 1 minute timeout
    metadata = #{on_timeout => refund}
}
```

### Resource Allocation

```erlang
#swf_resourcing{
    offer_strategy = role_based,
    roles = [<<"validator">>, <<"admin">>],
    allocate_strategy = shortest_queue
}
```

### A2A Integration

Contracts can invoke A2A tasks:
```erlang
#swf_tool_binding{
    transition_id = <<"oracle_query">>,
    tool_name = <<"chainlink_price_feed">>,
    input_mapping = fun(#{symbol := S}) -> #{symbol => S} end,
    output_mapping = fun(#{price := P}) -> #{oracle_price => P} end
}
```

## Testing Contracts

```erlang
%% Unit test: Create net, deploy, call
TokenNet = create_token_net(),
{ok, Addr} = pqc_contract:deploy(Creator, TokenNet, InitialState),
{ok, Result} = pqc_contract:call(Addr, transfer, [Recipient, 100], Caller),
?assertEqual(success, maps:get(status, Result)).

%% Property test: Invariants hold
prop_balance_never_negative() ->
    ?FORALL(Transitions, list(contract_transition()),
        begin
            {ok, State} = execute_transitions(Addr, Transitions),
            Balance = maps:get(balance, State),
            Balance >= 0
        end).

%% Conformance test: Replay matches live execution
{ok, Events} = pqc_contract:get_events(Addr, {0, 1000}),
{ok, ReplayedState} = swf_conformance:replay(Events),
{ok, LiveState} = pqc_contract:get_state(Addr),
?assertEqual(LiveState, ReplayedState).
```

## Security Considerations

1. **No re-entrancy**: Workflow structure prevents it
2. **Explicit guards**: All preconditions visible
3. **Bounded execution**: Petri net soundness checks prevent infinite loops
4. **Deterministic gas**: Gas calculation is predictable
5. **Compensation safety**: SAGA handlers ensure cleanup
6. **Event integrity**: Append-only log with checksums

## Future Enhancements

- **Parallel execution**: Fire independent transitions concurrently
- **Formal verification**: Model check Petri net properties
- **Optimization**: Compile nets to BEAM bytecode
- **Upgradeability**: Workflow patching via swarm proposals
- **Cross-chain**: Bridge contracts via A2A protocol
