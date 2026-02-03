# PQChain Contract API Reference

Quick reference for `pqc_contract` module.

## Deployment

### `deploy/3`

Deploy a new smart contract.

```erlang
-spec deploy(creator_address(), #swf_net{}, map()) ->
    {ok, contract_address()} | {error, term()}.
```

**Parameters:**
- `Creator` - 20-byte deploying account address
- `WorkflowNet` - SwarmFlow workflow net definition (`#swf_net{}`)
- `InitialState` - Map with initial contract state:
  - `balance` - Initial native token balance (optional, default 0)
  - `variables` - Initial contract variables (optional, default `#{}`)
  - `metadata` - Contract metadata (optional)
  - `block_height` - Deployment block height (optional)

**Returns:**
- `{ok, ContractAddress}` - 20-byte contract address
- `{error, Reason}` - Deployment failed

**Example:**
```erlang
Creator = <<1:160>>,
Net = #swf_net{...},
InitialState = #{
    balance => 1000,
    variables => #{owner => Creator}
},
{ok, ContractAddress} = pqc_contract:deploy(Creator, Net, InitialState).
```

**What it does:**
1. Derives contract address: `SHA3-256(Creator || Nonce)[0..19]`
2. Validates workflow net structure
3. Registers net with SwarmFlow registry
4. Stores contract metadata in ETS
5. Calculates initial state root
6. Increments creator nonce

---

## Execution

### `call/4`

Execute a contract transition (function call).

```erlang
-spec call(contract_address(), atom(), [term()], binary()) ->
    {ok, execution_result()} | {error, term()}.
```

**Parameters:**
- `ContractAddress` - 20-byte contract address
- `Transition` - Transition (function) name as atom
- `Args` - List of arguments
- `Caller` - 20-byte calling account address

**Returns:**
- `{ok, ExecutionResult}` - Execution succeeded or reverted
- `{error, Reason}` - Execution failed (e.g., contract not found)

**ExecutionResult structure:**
```erlang
#{
    status := success | failure | reverted,
    new_state := #{
        marking := #{binary() => non_neg_integer()},
        variables := map(),
        balance := non_neg_integer(),
        nonce := non_neg_integer(),
        status := case_status()
    },
    events := [#contract_event{}],
    gas_used := non_neg_integer(),
    return_value := term() | undefined,
    error := binary() | undefined
}
```

**Example:**
```erlang
Caller = <<1:160>>,
Recipient = <<2:160>>,
Amount = 1000,

{ok, Result} = pqc_contract:call(
    ContractAddress,
    transfer,
    [Recipient, Amount],
    Caller
),

case maps:get(status, Result) of
    success ->
        GasUsed = maps:get(gas_used, Result),
        Events = maps:get(events, Result),
        io:format("Success! Gas: ~p, Events: ~p~n", [GasUsed, Events]);
    failure ->
        Error = maps:get(error, Result),
        io:format("Failed: ~s~n", [Error])
end.
```

**What it does:**
1. Loads contract from ETS
2. Gets or creates SwarmFlow case instance
3. Validates transition is enabled
4. Fires transition with arguments
5. Captures state changes and events
6. Updates contract state root
7. Returns execution receipt

---

## State Queries

### `get_state/1`

Get current contract state.

```erlang
-spec get_state(contract_address()) ->
    {ok, contract_state()} | {error, term()}.
```

**Returns:**
```erlang
{ok, #{
    marking := #{binary() => non_neg_integer()},  % Petri net token distribution
    variables := map(),                            % Contract storage
    balance := non_neg_integer(),                  % Native token balance
    nonce := non_neg_integer(),                    % Call counter
    status := created | running | suspended | completed | failed
}}
```

**Example:**
```erlang
{ok, State} = pqc_contract:get_state(ContractAddress),
Variables = maps:get(variables, State),
TotalSupply = maps:get(total_supply, Variables),
Balances = maps:get(balances, Variables).
```

---

### `get_code/1`

Get contract workflow net definition.

```erlang
-spec get_code(contract_address()) ->
    {ok, #swf_net{}} | {error, term()}.
```

**Example:**
```erlang
{ok, Net} = pqc_contract:get_code(ContractAddress),
Places = Net#swf_net.places,
Transitions = Net#swf_net.transitions.
```

---

### `get_contract/1`

Get contract metadata record.

```erlang
-spec get_contract(contract_address()) ->
    {ok, #pqc_contract{}} | {error, not_found}.
```

**Example:**
```erlang
{ok, Contract} = pqc_contract:get_contract(ContractAddress),
Creator = Contract#pqc_contract.creator,
CodeHash = Contract#pqc_contract.code_hash,
CreatedAt = Contract#pqc_contract.created_at_height.
```

---

### `get_balance/1`

Get contract native token balance.

```erlang
-spec get_balance(contract_address()) ->
    {ok, non_neg_integer()} | {error, term()}.
```

**Example:**
```erlang
{ok, Balance} = pqc_contract:get_balance(ContractAddress).
```

---

### `set_balance/2`

Set contract native token balance (for internal use by transfer logic).

```erlang
-spec set_balance(contract_address(), non_neg_integer()) ->
    ok | {error, term()}.
```

---

## Validation

### `validate_transition/3`

Check if a transition can fire without executing it.

```erlang
-spec validate_transition(contract_address(), atom(), [term()]) ->
    {ok, enabled} | {error, not_enabled | term()}.
```

**Example:**
```erlang
case pqc_contract:validate_transition(ContractAddress, transfer, [Recipient, 1000]) of
    {ok, enabled} ->
        io:format("Transfer can proceed~n");
    {error, not_enabled} ->
        io:format("Transfer would fail (insufficient balance or disabled)~n")
end.
```

---

## Simulation

### `simulate/4`

Simulate a contract call without committing state changes (dry-run).

```erlang
-spec simulate(contract_address(), atom(), [term()], binary()) ->
    {ok, execution_result()} | {error, term()}.
```

**Example:**
```erlang
%% Test if transfer would succeed without actually doing it
{ok, SimResult} = pqc_contract:simulate(
    ContractAddress,
    transfer,
    [Recipient, 1000000],
    Caller
),

case maps:get(status, SimResult) of
    success ->
        GasUsed = maps:get(gas_used, SimResult),
        io:format("Transfer would succeed, gas: ~p~n", [GasUsed]);
    failure ->
        Error = maps:get(error, SimResult),
        io:format("Transfer would fail: ~s~n", [Error])
end.
```

**Note:** Simulation creates a temporary case, executes the transition, captures the result, then discards the case. State is not persisted.

---

## Events

### `get_events/2`

Get contract events in a block range.

```erlang
-spec get_events(contract_address(), {non_neg_integer(), non_neg_integer()}) ->
    {ok, [#contract_event{}]} | {error, term()}.
```

**Parameters:**
- `ContractAddress` - Contract to query
- `{FromBlock, ToBlock}` - Inclusive block range

**Example:**
```erlang
{ok, Events} = pqc_contract:get_events(ContractAddress, {0, 1000}),
lists:foreach(
    fun(#contract_event{name = Name, data = Data, block_height = Height}) ->
        io:format("Event ~p at block ~p: ~p~n", [Name, Height, Data])
    end,
    Events
).
```

**Event structure:**
```erlang
#contract_event{
    contract_address :: binary(),
    name :: atom(),                        % Event type
    data :: term(),                        % Event-specific data
    indexed :: [term()],                   % Indexed fields for filtering
    tx_id :: binary(),                     % Transaction ID
    block_height :: non_neg_integer(),     % Block number
    log_index :: non_neg_integer()         % Event index in block
}
```

---

## Transaction Encoding

### `encode_call/3`

Encode a contract call for transaction payload.

```erlang
-spec encode_call(atom(), [term()], [binary()]) ->
    #contract_call_payload{}.
```

**Example:**
```erlang
Payload = pqc_contract:encode_call(
    transfer,
    [Recipient, 1000],
    []  % Artifacts (A2A references)
),

Tx = #pqc_transaction{
    type = ?TX_TYPE_CONTRACT_CALL,
    sender = Caller,
    recipient = ContractAddress,
    payload = Payload#contract_call_payload{contract_address = ContractAddress},
    signature = #pqc_signature{...},
    ...
}.
```

---

### `decode_result/1`

Decode execution result from receipt.

```erlang
-spec decode_result(#execution_receipt{}) ->
    {ok, execution_result()} | {error, term()}.
```

**Example:**
```erlang
#execution_receipt{} = Receipt,  % From block
{ok, Result} = pqc_contract:decode_result(Receipt),
Status = maps:get(status, Result),
GasUsed = maps:get(gas_used, Result).
```

---

## Utility Functions

### `derive_address/2`

Derive contract address from creator and nonce (for testing/verification).

```erlang
-spec derive_address(creator_address(), nonce()) ->
    contract_address().
```

**Example:**
```erlang
Creator = <<1:160>>,
Nonce = 5,
ExpectedAddress = pqc_contract:derive_address(Creator, Nonce).
% Returns: SHA3-256(Creator || Nonce)[0..19]
```

---

### `init_contracts_table/0`

Initialize the ETS contracts table (called automatically, exposed for testing).

```erlang
-spec init_contracts_table() -> ok.
```

---

## Error Handling

Common errors:

| Error | Cause | Solution |
|-------|-------|----------|
| `contract_not_found` | Invalid contract address | Verify address, check deployment |
| `{net_registration_failed, Reason}` | Invalid workflow net | Validate net structure |
| `{transition_not_enabled, Transition, Gas}` | Transition cannot fire | Check guards, verify state |
| `{guard_failed, Transition, Gas}` | Guard evaluated to false | Verify preconditions met |
| `{invalid_workflow_net, Errors}` | Net validation failed | Fix net structure errors |

---

## Type Definitions

```erlang
-type contract_address() :: binary().      % 20 bytes
-type creator_address() :: binary().       % 20 bytes
-type nonce() :: non_neg_integer().

-type contract_state() :: #{
    marking := #{binary() => non_neg_integer()},
    variables := map(),
    balance := non_neg_integer(),
    nonce := non_neg_integer(),
    status := created | running | suspended | completed | failed | cancelled | compensating
}.

-type execution_result() :: #{
    status := success | failure | reverted,
    new_state := contract_state(),
    events := [#contract_event{}],
    gas_used := non_neg_integer(),
    return_value := term() | undefined,
    error := binary() | undefined
}.
```

---

## Best Practices

1. **Always validate before calling:**
   ```erlang
   {ok, enabled} = pqc_contract:validate_transition(Addr, transfer, Args),
   {ok, Result} = pqc_contract:call(Addr, transfer, Args, Caller).
   ```

2. **Use simulation for gas estimation:**
   ```erlang
   {ok, SimResult} = pqc_contract:simulate(Addr, transfer, Args, Caller),
   EstimatedGas = maps:get(gas_used, SimResult),
   GasLimit = EstimatedGas * 1.2.  % Add 20% buffer
   ```

3. **Check execution status:**
   ```erlang
   {ok, Result} = pqc_contract:call(...),
   case maps:get(status, Result) of
       success -> handle_success(Result);
       failure -> handle_failure(Result);
       reverted -> handle_revert(Result)
   end.
   ```

4. **Query events for history:**
   ```erlang
   {ok, Events} = pqc_contract:get_events(Addr, {0, CurrentBlock}),
   TransferEvents = [E || E <- Events, E#contract_event.name =:= transfer].
   ```

5. **Store contract address after deployment:**
   ```erlang
   {ok, Addr} = pqc_contract:deploy(Creator, Net, InitialState),
   persistent_term:put({my_app, token_contract}, Addr).
   ```
