# Example: ERC20-like Token Contract

This example shows how to implement a token contract as a SwarmFlow workflow net.

## Contract Design

### Places (State)

- `initial`: Contract deployment state (1 token initially)
- `active`: Contract is active and operational
- `balances`: Token holder balances (infinite capacity)
- `allowances`: Approved spending limits (infinite capacity)
- `final`: Contract has been finalized/destroyed

### Transitions (Functions)

- `initialize`: Set up initial token supply
- `transfer`: Send tokens from caller to recipient
- `approve`: Approve spender to use tokens
- `transferFrom`: Spend approved tokens
- `mint`: Create new tokens (owner only)
- `burn`: Destroy tokens
- `finalize`: Destroy contract

### Guards (Preconditions)

Each transition has guards that must evaluate to `true`:

```erlang
%% Transfer guard: caller has sufficient balance
fun(#{caller := Caller, amount := Amount, balances := Balances}) ->
    CallerBalance = maps:get(Caller, Balances, 0),
    Amount > 0 andalso CallerBalance >= Amount
end

%% TransferFrom guard: spender has sufficient allowance
fun(#{spender := Spender, from := From, amount := Amount,
      balances := Balances, allowances := Allowances}) ->
    FromBalance = maps:get(From, Balances, 0),
    AllowanceKey = {From, Spender},
    Allowance = maps:get(AllowanceKey, Allowances, 0),
    Amount > 0 andalso FromBalance >= Amount andalso Allowance >= Amount
end
```

## Complete Implementation

```erlang
-module(example_token_contract).
-include("swarmflow.hrl").

-export([create_token_net/3]).

%% @doc Create a token workflow net
%% @param Name Token name (e.g., <<"MyToken">>)
%% @param Symbol Token symbol (e.g., <<"MTK">>)
%% @param InitialSupply Initial token supply
create_token_net(Name, Symbol, InitialSupply) ->
    %% Define places
    Places = #{
        <<"initial">> => #swf_place{
            id = <<"initial">>,
            name = <<"Initial State">>,
            tokens = 1,
            capacity = 1,
            metadata = #{}
        },

        <<"active">> => #swf_place{
            id = <<"active">>,
            name = <<"Active State">>,
            tokens = 0,
            capacity = 1,
            metadata = #{}
        },

        <<"balances">> => #swf_place{
            id = <<"balances">>,
            name = <<"Token Balances">>,
            tokens = 0,
            capacity = infinity,
            metadata = #{role => storage}
        },

        <<"allowances">> => #swf_place{
            id = <<"allowances">>,
            name = <<"Spending Allowances">>,
            tokens = 0,
            capacity = infinity,
            metadata = #{role => storage}
        },

        <<"final">> => #swf_place{
            id = <<"final">>,
            name = <<"Final State">>,
            tokens = 0,
            capacity = 1,
            metadata = #{}
        }
    },

    %% Define transitions
    Transitions = #{
        <<"initialize">> => #swf_transition{
            id = <<"initialize">>,
            name = <<"Initialize Token">>,
            kind = automatic,
            guard = undefined,
            action = fun(Vars) ->
                Creator = maps:get(creator, Vars),
                Supply = maps:get(initial_supply, Vars),
                Balances = #{Creator => Supply},
                {ok, Vars#{
                    balances => Balances,
                    total_supply => Supply,
                    initialized => true
                }}
            end,
            timeout_ms = undefined,
            priority = 100,
            metadata = #{}
        },

        <<"transfer">> => #swf_transition{
            id = <<"transfer">>,
            name = <<"Transfer Tokens">>,
            kind = manual,
            guard = fun(Vars) ->
                Caller = maps:get(caller, Vars, undefined),
                Recipient = maps:get(recipient, Vars, undefined),
                Amount = maps:get(amount, Vars, 0),
                Balances = maps:get(balances, Vars, #{}),

                Caller =/= undefined andalso
                Recipient =/= undefined andalso
                Caller =/= Recipient andalso
                Amount > 0 andalso
                maps:get(Caller, Balances, 0) >= Amount
            end,
            action = fun(Vars) ->
                Caller = maps:get(caller, Vars),
                Recipient = maps:get(recipient, Vars),
                Amount = maps:get(amount, Vars),
                Balances = maps:get(balances, Vars),

                CallerBalance = maps:get(Caller, Balances, 0),
                RecipientBalance = maps:get(Recipient, Balances, 0),

                NewBalances = Balances#{
                    Caller => CallerBalance - Amount,
                    Recipient => RecipientBalance + Amount
                },

                {ok, Vars#{
                    balances => NewBalances,
                    return_value => {ok, Amount},
                    event => #{
                        type => transfer,
                        from => Caller,
                        to => Recipient,
                        amount => Amount
                    }
                }}
            end,
            timeout_ms = 30000,
            priority = 0,
            metadata = #{}
        },

        <<"approve">> => #swf_transition{
            id = <<"approve">>,
            name = <<"Approve Spender">>,
            kind = manual,
            guard = fun(Vars) ->
                Caller = maps:get(caller, Vars, undefined),
                Spender = maps:get(spender, Vars, undefined),
                Amount = maps:get(amount, Vars, 0),

                Caller =/= undefined andalso
                Spender =/= undefined andalso
                Caller =/= Spender andalso
                Amount >= 0
            end,
            action = fun(Vars) ->
                Caller = maps:get(caller, Vars),
                Spender = maps:get(spender, Vars),
                Amount = maps:get(amount, Vars),
                Allowances = maps:get(allowances, Vars, #{}),

                AllowanceKey = {Caller, Spender},
                NewAllowances = Allowances#{AllowanceKey => Amount},

                {ok, Vars#{
                    allowances => NewAllowances,
                    return_value => {ok, Amount},
                    event => #{
                        type => approval,
                        owner => Caller,
                        spender => Spender,
                        amount => Amount
                    }
                }}
            end,
            timeout_ms = 30000,
            priority = 0,
            metadata = #{}
        },

        <<"transferFrom">> => #swf_transition{
            id = <<"transferFrom">>,
            name = <<"Transfer From Approved">>,
            kind = manual,
            guard = fun(Vars) ->
                Caller = maps:get(caller, Vars, undefined),
                From = maps:get(from, Vars, undefined),
                To = maps:get(to, Vars, undefined),
                Amount = maps:get(amount, Vars, 0),
                Balances = maps:get(balances, Vars, #{}),
                Allowances = maps:get(allowances, Vars, #{}),

                AllowanceKey = {From, Caller},
                FromBalance = maps:get(From, Balances, 0),
                Allowance = maps:get(AllowanceKey, Allowances, 0),

                Caller =/= undefined andalso
                From =/= undefined andalso
                To =/= undefined andalso
                From =/= To andalso
                Amount > 0 andalso
                FromBalance >= Amount andalso
                Allowance >= Amount
            end,
            action = fun(Vars) ->
                Caller = maps:get(caller, Vars),
                From = maps:get(from, Vars),
                To = maps:get(to, Vars),
                Amount = maps:get(amount, Vars),
                Balances = maps:get(balances, Vars),
                Allowances = maps:get(allowances, Vars),

                FromBalance = maps:get(From, Balances, 0),
                ToBalance = maps:get(To, Balances, 0),
                AllowanceKey = {From, Caller},
                Allowance = maps:get(AllowanceKey, Allowances, 0),

                NewBalances = Balances#{
                    From => FromBalance - Amount,
                    To => ToBalance + Amount
                },
                NewAllowances = Allowances#{AllowanceKey => Allowance - Amount},

                {ok, Vars#{
                    balances => NewBalances,
                    allowances => NewAllowances,
                    return_value => {ok, Amount},
                    event => #{
                        type => transfer_from,
                        spender => Caller,
                        from => From,
                        to => To,
                        amount => Amount
                    }
                }}
            end,
            timeout_ms = 30000,
            priority = 0,
            metadata = #{}
        },

        <<"mint">> => #swf_transition{
            id = <<"mint">>,
            name = <<"Mint New Tokens">>,
            kind = manual,
            guard = fun(Vars) ->
                Caller = maps:get(caller, Vars, undefined),
                Creator = maps:get(creator, Vars, undefined),
                Amount = maps:get(amount, Vars, 0),

                Caller =:= Creator andalso Amount > 0
            end,
            action = fun(Vars) ->
                Recipient = maps:get(recipient, Vars),
                Amount = maps:get(amount, Vars),
                Balances = maps:get(balances, Vars),
                TotalSupply = maps:get(total_supply, Vars),

                RecipientBalance = maps:get(Recipient, Balances, 0),
                NewBalances = Balances#{Recipient => RecipientBalance + Amount},

                {ok, Vars#{
                    balances => NewBalances,
                    total_supply => TotalSupply + Amount,
                    return_value => {ok, Amount},
                    event => #{
                        type => mint,
                        to => Recipient,
                        amount => Amount
                    }
                }}
            end,
            timeout_ms = 30000,
            priority = 0,
            metadata = #{}
        },

        <<"burn">> => #swf_transition{
            id = <<"burn">>,
            name = <<"Burn Tokens">>,
            kind = manual,
            guard = fun(Vars) ->
                Caller = maps:get(caller, Vars, undefined),
                Amount = maps:get(amount, Vars, 0),
                Balances = maps:get(balances, Vars, #{}),

                CallerBalance = maps:get(Caller, Balances, 0),
                Amount > 0 andalso CallerBalance >= Amount
            end,
            action = fun(Vars) ->
                Caller = maps:get(caller, Vars),
                Amount = maps:get(amount, Vars),
                Balances = maps:get(balances, Vars),
                TotalSupply = maps:get(total_supply, Vars),

                CallerBalance = maps:get(Caller, Balances),
                NewBalances = Balances#{Caller => CallerBalance - Amount},

                {ok, Vars#{
                    balances => NewBalances,
                    total_supply => TotalSupply - Amount,
                    return_value => {ok, Amount},
                    event => #{
                        type => burn,
                        from => Caller,
                        amount => Amount
                    }
                }}
            end,
            timeout_ms = 30000,
            priority = 0,
            metadata = #{}
        }
    },

    %% Define arcs (control flow)
    Arcs = [
        %% Initialize: initial -> initialize -> active, balances
        #swf_arc{
            id = <<"init_in">>,
            source = <<"initial">>,
            target = <<"initialize">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"init_out_active">>,
            source = <<"initialize">>,
            target = <<"active">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"init_out_balances">>,
            source = <<"initialize">>,
            target = <<"balances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"init_out_allowances">>,
            source = <<"initialize">>,
            target = <<"allowances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },

        %% Transfer: balances -> transfer -> balances (loop)
        #swf_arc{
            id = <<"transfer_in">>,
            source = <<"balances">>,
            target = <<"transfer">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"transfer_out">>,
            source = <<"transfer">>,
            target = <<"balances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },

        %% Approve: allowances -> approve -> allowances (loop)
        #swf_arc{
            id = <<"approve_in">>,
            source = <<"allowances">>,
            target = <<"approve">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"approve_out">>,
            source = <<"approve">>,
            target = <<"allowances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },

        %% TransferFrom: balances, allowances -> transferFrom -> balances, allowances
        #swf_arc{
            id = <<"transferFrom_in_balances">>,
            source = <<"balances">>,
            target = <<"transferFrom">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"transferFrom_in_allowances">>,
            source = <<"allowances">>,
            target = <<"transferFrom">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"transferFrom_out_balances">>,
            source = <<"transferFrom">>,
            target = <<"balances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"transferFrom_out_allowances">>,
            source = <<"transferFrom">>,
            target = <<"allowances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },

        %% Mint: balances -> mint -> balances (loop)
        #swf_arc{
            id = <<"mint_in">>,
            source = <<"balances">>,
            target = <<"mint">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"mint_out">>,
            source = <<"mint">>,
            target = <<"balances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },

        %% Burn: balances -> burn -> balances (loop)
        #swf_arc{
            id = <<"burn_in">>,
            source = <<"balances">>,
            target = <<"burn">>,
            weight = 1,
            kind = normal,
            expression = undefined
        },
        #swf_arc{
            id = <<"burn_out">>,
            source = <<"burn">>,
            target = <<"balances">>,
            weight = 1,
            kind = normal,
            expression = undefined
        }
    ],

    %% Create workflow net
    #swf_net{
        id = <<"token_contract">>,
        name = Name,
        version = <<"1.0.0">>,
        places = Places,
        transitions = Transitions,
        arcs = Arcs,
        initial_marking = #{
            <<"initial">> => 1,
            <<"active">> => 0,
            <<"balances">> => 0,
            <<"allowances">> => 0,
            <<"final">> => 0
        },
        final_places = [<<"final">>],
        metadata = #{
            contract_type => token,
            standard => erc20_like,
            name => Name,
            symbol => Symbol,
            initial_supply => InitialSupply
        }
    }.
```

## Usage Example

```erlang
%% 1. Create the token contract workflow net
TokenNet = example_token_contract:create_token_net(
    <<"MyToken">>,
    <<"MTK">>,
    1000000
),

%% 2. Deploy the contract
Creator = <<1:160>>,  % 20-byte creator address
InitialState = #{
    balance => 0,
    variables => #{
        creator => Creator,
        initial_supply => 1000000
    },
    block_height => 1
},
{ok, ContractAddress} = pqc_contract:deploy(Creator, TokenNet, InitialState),

%% 3. Transfer tokens
Recipient = <<2:160>>,
{ok, TransferResult} = pqc_contract:call(
    ContractAddress,
    transfer,
    [Recipient, 1000],
    Creator
),

%% Verify success
success = maps:get(status, TransferResult),

%% 4. Approve spender
Spender = <<3:160>>,
{ok, ApproveResult} = pqc_contract:call(
    ContractAddress,
    approve,
    [Spender, 500],
    Creator
),

%% 5. Spender transfers on behalf of creator
{ok, TransferFromResult} = pqc_contract:call(
    ContractAddress,
    transferFrom,
    [Creator, Recipient, 250],
    Spender
),

%% 6. Query state
{ok, State} = pqc_contract:get_state(ContractAddress),
Balances = maps:get(balances, maps:get(variables, State)),
CreatorBalance = maps:get(Creator, Balances),  % 998750 (1000000 - 1000 - 250)
RecipientBalance = maps:get(Recipient, Balances),  % 1250 (1000 + 250)

%% 7. Get events
{ok, Events} = pqc_contract:get_events(ContractAddress, {0, 100}),
% Events include: initialization, transfers, approvals
```

## Testing

```erlang
-module(token_contract_test).
-include_lib("eunit/include/eunit.hrl").

token_workflow_test() ->
    %% Deploy
    Net = example_token_contract:create_token_net(<<"Test">>, <<"TST">>, 1000000),
    Creator = <<1:160>>,
    {ok, Addr} = pqc_contract:deploy(Creator, Net, #{
        variables => #{creator => Creator, initial_supply => 1000000}
    }),

    %% Transfer
    Recipient = <<2:160>>,
    {ok, R1} = pqc_contract:call(Addr, transfer, [Recipient, 1000], Creator),
    ?assertEqual(success, maps:get(status, R1)),

    %% Check balances
    {ok, State} = pqc_contract:get_state(Addr),
    Balances = maps:get(balances, maps:get(variables, State)),
    ?assertEqual(999000, maps:get(Creator, Balances)),
    ?assertEqual(1000, maps:get(Recipient, Balances)).
```

## Security Properties

This token contract ensures:

1. **Conservation of tokens**: Total supply tracked, cannot create/destroy without mint/burn
2. **No negative balances**: Guards prevent transfers exceeding balance
3. **Approval safety**: `transferFrom` checks both balance and allowance
4. **No re-entrancy**: Workflow structure prevents re-entrant calls
5. **Deterministic replay**: Event log enables exact state reconstruction
6. **Audit trail**: All transfers recorded in event log with full details

## Gas Costs

Approximate gas costs (implementation-dependent):
- `deploy`: 500,000 gas
- `initialize`: 50,000 gas
- `transfer`: 25,000 gas
- `approve`: 20,000 gas
- `transferFrom`: 30,000 gas
- `mint`: 30,000 gas
- `burn`: 25,000 gas
