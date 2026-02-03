%%% @doc PQC Tools Registry
%%%
%%% "Swallowing the world" - Tool adapter layer for Case syscalls.
%%% Bridges SwarmFlow Case execution to external capabilities:
%%% - PQC crypto operations (ML-DSA, ML-KEM, SLH-DSA)
%%% - Blockchain operations (query, submit, identity)
%%% - Smart contract invocation
%%% - SaaS and infrastructure adapters (extensible)
%%% - AI/ML capabilities integration
%%%
%%% Architecture:
%%% - gen_server with ETS-based tool registry
%%% - JSON Schema validation for tool arguments
%%% - Tool handlers are fun/2: (Args :: map(), CaseId :: binary())
%%% - Built-in PQC and blockchain tools registered on startup
%%% - MCP tools/list and tools/call map directly to this registry
%%% - A2A tool invocations route through Case -> pqc_tools
%%%
%%% Usage:
%%%   {ok, Pid} = pqc_tools:start_link(),
%%%   ok = pqc_tools:register(<<"custom_tool">>, Schema, HandlerFun),
%%%   {ok, Result} = pqc_tools:call(<<"pqc_sign">>, #{message => Data}, CaseId).
%%%
%%% @end
-module(pqc_tools).

-behaviour(gen_server).

-include("pqchain.hrl").

%% API exports
-export([
    start_link/0,
    register/3,
    unregister/1,
    list/0,
    call/3,
    get_schema/1,
    validate_args/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% ETS table for tool registry
-define(TOOLS_TABLE, pqc_tools_registry).

%% Default timeout for tool execution
-define(TOOL_CALL_TIMEOUT, 30000).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type tool_name() :: binary().
-type tool_schema() :: #{
    name := binary(),
    description := binary(),
    input_schema := map()
}.
-type tool_handler() :: fun((Args :: map(), CaseId :: binary()) ->
    {ok, Result :: term()} | {error, Reason :: binary()}).

-export_type([tool_name/0, tool_schema/0, tool_handler/0]).

%%% ============================================================================
%%% Internal State
%%% ============================================================================

-record(state, {
    tools_table :: ets:tid(),
    registered_at :: non_neg_integer()
}).

-type state() :: #state{}.

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the tool registry gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register a tool with name, schema, and handler function
-spec register(tool_name(), tool_schema(), tool_handler()) -> ok | {error, term()}.
register(Name, Schema, Handler) when is_binary(Name), is_map(Schema), is_function(Handler, 2) ->
    gen_server:call(?MODULE, {register, Name, Schema, Handler}, 5000).

%% @doc Unregister a tool by name
-spec unregister(tool_name()) -> ok | {error, not_found}.
unregister(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {unregister, Name}, 5000).

%% @doc List all registered tools with their schemas
-spec list() -> {ok, [tool_schema()]}.
list() ->
    gen_server:call(?MODULE, list_tools, 5000).

%% @doc Call a tool with arguments in the context of a Case
-spec call(tool_name(), map(), binary()) -> {ok, term()} | {error, term()}.
call(Name, Args, CaseId) when is_binary(Name), is_map(Args), is_binary(CaseId) ->
    gen_server:call(?MODULE, {call_tool, Name, Args, CaseId}, ?TOOL_CALL_TIMEOUT).

%% @doc Get the schema for a specific tool
-spec get_schema(tool_name()) -> {ok, tool_schema()} | {error, not_found}.
get_schema(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {get_schema, Name}, 5000).

%% @doc Validate arguments against a tool's schema
-spec validate_args(tool_name(), map()) -> ok | {error, [binary()]}.
validate_args(Name, Args) when is_binary(Name), is_map(Args) ->
    gen_server:call(?MODULE, {validate_args, Name, Args}, 5000).

%%% ============================================================================
%%% gen_server callbacks
%%% ============================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS table for tool registry
    Table = ets:new(?TOOLS_TABLE, [
        named_table,
        set,
        protected,
        {read_concurrency, true}
    ]),

    %% Register with gproc if available
    register_with_gproc(),

    State = #state{
        tools_table = Table,
        registered_at = erlang:system_time(millisecond)
    },

    %% Register built-in tools
    ok = register_builtin_tools(State),

    logger:info("PQC Tools Registry started with ~p built-in tools",
                [ets:info(Table, size)]),

    {ok, State}.

-spec handle_call(term(), term(), state()) ->
    {reply, term(), state()} | {noreply, state()} | {stop, term(), term(), state()}.

handle_call({register, Name, Schema, Handler}, _From, State) ->
    case ets:lookup(State#state.tools_table, Name) of
        [] ->
            %% Insert new tool
            true = ets:insert(State#state.tools_table, {Name, Schema, Handler}),
            logger:info("Registered tool: ~s", [Name]),
            {reply, ok, State};
        [_] ->
            %% Tool already exists
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister, Name}, _From, State) ->
    case ets:lookup(State#state.tools_table, Name) of
        [] ->
            {reply, {error, not_found}, State};
        [_] ->
            true = ets:delete(State#state.tools_table, Name),
            logger:info("Unregistered tool: ~s", [Name]),
            {reply, ok, State}
    end;

handle_call(list_tools, _From, State) ->
    Tools = ets:foldl(fun({_Name, Schema, _Handler}, Acc) ->
        [Schema | Acc]
    end, [], State#state.tools_table),
    {reply, {ok, Tools}, State};

handle_call({call_tool, Name, Args, CaseId}, From, State) ->
    case ets:lookup(State#state.tools_table, Name) of
        [] ->
            {reply, {error, tool_not_found}, State};
        [{_Name, Schema, Handler}] ->
            %% Validate arguments
            case validate_args_internal(Schema, Args) of
                ok ->
                    %% Execute tool handler asynchronously to avoid blocking
                    spawn_link(fun() ->
                        Result = execute_tool_handler(Handler, Args, CaseId, Name),
                        gen_server:reply(From, Result)
                    end),
                    {noreply, State};
                {error, ValidationErrors} ->
                    {reply, {error, {validation_failed, ValidationErrors}}, State}
            end
    end;

handle_call({get_schema, Name}, _From, State) ->
    case ets:lookup(State#state.tools_table, Name) of
        [] ->
            {reply, {error, not_found}, State};
        [{_Name, Schema, _Handler}] ->
            {reply, {ok, Schema}, State}
    end;

handle_call({validate_args, Name, Args}, _From, State) ->
    case ets:lookup(State#state.tools_table, Name) of
        [] ->
            {reply, {error, not_found}, State};
        [{_Name, Schema, _Handler}] ->
            Result = validate_args_internal(Schema, Args),
            {reply, Result, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    logger:info("PQC Tools Registry terminating: ~p", [Reason]),

    %% Unregister from gproc
    unregister_from_gproc(),

    %% Clean up ETS table
    ets:delete(State#state.tools_table),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal Functions: Built-in Tools
%%% ============================================================================

%% @doc Register all built-in tools at startup
-spec register_builtin_tools(state()) -> ok.
register_builtin_tools(State) ->
    Table = State#state.tools_table,

    %% 1. pqc_sign - Sign data with ML-DSA
    true = ets:insert(Table, {
        <<"pqc_sign">>,
        #{
            name => <<"pqc_sign">>,
            description => <<"Sign data with ML-DSA (FIPS 204) post-quantum signature">>,
            input_schema => #{
                type => object,
                properties => #{
                    message => #{type => binary, description => <<"Data to sign">>},
                    algorithm => #{
                        type => atom,
                        enum => [ml_dsa_44, ml_dsa_65, ml_dsa_87],
                        default => ml_dsa_65,
                        description => <<"ML-DSA algorithm variant">>
                    }
                },
                required => [message]
            }
        },
        fun builtin_pqc_sign/2
    }),

    %% 2. pqc_verify - Verify PQC signature
    true = ets:insert(Table, {
        <<"pqc_verify">>,
        #{
            name => <<"pqc_verify">>,
            description => <<"Verify a post-quantum signature">>,
            input_schema => #{
                type => object,
                properties => #{
                    message => #{type => binary, description => <<"Original message">>},
                    signature => #{type => binary, description => <<"Signature to verify">>},
                    public_key => #{type => binary, description => <<"Signer's public key">>}
                },
                required => [message, signature, public_key]
            }
        },
        fun builtin_pqc_verify/2
    }),

    %% 3. pqc_hash - Hash with SHA3/BLAKE3
    true = ets:insert(Table, {
        <<"pqc_hash">>,
        #{
            name => <<"pqc_hash">>,
            description => <<"Hash data with SHA3 or BLAKE3">>,
            input_schema => #{
                type => object,
                properties => #{
                    data => #{type => binary, description => <<"Data to hash">>},
                    algorithm => #{
                        type => atom,
                        enum => [sha3_256, sha3_512, blake3],
                        default => sha3_256,
                        description => <<"Hash algorithm">>
                    }
                },
                required => [data]
            }
        },
        fun builtin_pqc_hash/2
    }),

    %% 4. pqc_encrypt - Encrypt with ML-KEM
    true = ets:insert(Table, {
        <<"pqc_encrypt">>,
        #{
            name => <<"pqc_encrypt">>,
            description => <<"Encrypt data with ML-KEM (FIPS 203) key encapsulation">>,
            input_schema => #{
                type => object,
                properties => #{
                    data => #{type => binary, description => <<"Data to encrypt">>},
                    public_key => #{type => binary, description => <<"Recipient's ML-KEM public key">>}
                },
                required => [data, public_key]
            }
        },
        fun builtin_pqc_encrypt/2
    }),

    %% 5. pqc_decrypt - Decrypt with ML-KEM
    true = ets:insert(Table, {
        <<"pqc_decrypt">>,
        #{
            name => <<"pqc_decrypt">>,
            description => <<"Decrypt data with ML-KEM key decapsulation">>,
            input_schema => #{
                type => object,
                properties => #{
                    ciphertext => #{type => binary, description => <<"Encrypted data">>},
                    encapsulated_key => #{type => binary, description => <<"Encapsulated KEM key">>},
                    keypair => #{type => map, description => <<"Recipient's ML-KEM keypair">>}
                },
                required => [ciphertext, encapsulated_key, keypair]
            }
        },
        fun builtin_pqc_decrypt/2
    }),

    %% 6. chain_query - Query blockchain
    true = ets:insert(Table, {
        <<"chain_query">>,
        #{
            name => <<"chain_query">>,
            description => <<"Query the post-quantum blockchain">>,
            input_schema => #{
                type => object,
                properties => #{
                    query_type => #{
                        type => atom,
                        enum => [block, transaction, account, validator],
                        description => <<"Type of query">>
                    },
                    params => #{type => map, description => <<"Query parameters">>}
                },
                required => [query_type, params]
            }
        },
        fun builtin_chain_query/2
    }),

    %% 7. chain_submit - Submit transaction
    true = ets:insert(Table, {
        <<"chain_submit">>,
        #{
            name => <<"chain_submit">>,
            description => <<"Submit a transaction to the blockchain">>,
            input_schema => #{
                type => object,
                properties => #{
                    tx_type => #{
                        type => atom,
                        enum => [transfer, key_registration, contract_deploy, contract_call],
                        description => <<"Transaction type">>
                    },
                    params => #{type => map, description => <<"Transaction parameters">>}
                },
                required => [tx_type, params]
            }
        },
        fun builtin_chain_submit/2
    }),

    %% 8. identity_create - Create PQC identity
    true = ets:insert(Table, {
        <<"identity_create">>,
        #{
            name => <<"identity_create">>,
            description => <<"Create a new post-quantum identity with key pair">>,
            input_schema => #{
                type => object,
                properties => #{
                    options => #{
                        type => map,
                        description => <<"Identity creation options (algorithm, metadata)">>
                    }
                },
                required => []
            }
        },
        fun builtin_identity_create/2
    }),

    %% 9. identity_lookup - Lookup identity
    true = ets:insert(Table, {
        <<"identity_lookup">>,
        #{
            name => <<"identity_lookup">>,
            description => <<"Lookup an identity by blockchain address">>,
            input_schema => #{
                type => object,
                properties => #{
                    address => #{type => binary, description => <<"Blockchain address">>}
                },
                required => [address]
            }
        },
        fun builtin_identity_lookup/2
    }),

    %% 10. contract_call - Call smart contract
    true = ets:insert(Table, {
        <<"contract_call">>,
        #{
            name => <<"contract_call">>,
            description => <<"Invoke a smart contract transition (workflow net)">>,
            input_schema => #{
                type => object,
                properties => #{
                    address => #{type => binary, description => <<"Contract address">>},
                    transition => #{type => atom, description => <<"Workflow transition to fire">>},
                    args => #{type => list, description => <<"Transition arguments">>}
                },
                required => [address, transition, args]
            }
        },
        fun builtin_contract_call/2
    }),

    ok.

%%% ============================================================================
%%% Internal Functions: Built-in Tool Handlers
%%% ============================================================================

%% @doc Built-in: Sign data with ML-DSA
-spec builtin_pqc_sign(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_pqc_sign(Args, CaseId) ->
    Message = maps:get(message, Args),
    Algorithm = maps:get(algorithm, Args, ml_dsa_65),

    logger:debug("pqc_sign called by case ~s: algorithm=~p", [CaseId, Algorithm]),

    %% Delegate to pqc_crypto module
    case pqc_crypto:sign(Message, Algorithm) of
        {ok, Signature} ->
            {ok, #{
                signature => Signature,
                algorithm => Algorithm,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Sign failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Verify PQC signature
-spec builtin_pqc_verify(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_pqc_verify(Args, CaseId) ->
    Message = maps:get(message, Args),
    Signature = maps:get(signature, Args),
    PublicKey = maps:get(public_key, Args),

    logger:debug("pqc_verify called by case ~s", [CaseId]),

    %% Delegate to pqc_crypto module
    case pqc_crypto:verify(Message, Signature, PublicKey) of
        {ok, valid} ->
            {ok, #{valid => true, case_id => CaseId}};
        {ok, invalid} ->
            {ok, #{valid => false, case_id => CaseId}};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Verify failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Hash data
-spec builtin_pqc_hash(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_pqc_hash(Args, CaseId) ->
    Data = maps:get(data, Args),
    Algorithm = maps:get(algorithm, Args, sha3_256),

    logger:debug("pqc_hash called by case ~s: algorithm=~p", [CaseId, Algorithm]),

    %% Delegate to pqc_crypto module
    case pqc_crypto:hash(Data, Algorithm) of
        {ok, Hash} ->
            {ok, #{
                hash => Hash,
                algorithm => Algorithm,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Hash failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Encrypt with ML-KEM
-spec builtin_pqc_encrypt(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_pqc_encrypt(Args, CaseId) ->
    Data = maps:get(data, Args),
    PublicKey = maps:get(public_key, Args),

    logger:debug("pqc_encrypt called by case ~s", [CaseId]),

    %% Delegate to pqc_crypto module
    case pqc_crypto:encapsulate_and_encrypt(Data, PublicKey) of
        {ok, Ciphertext, EncapsulatedKey} ->
            {ok, #{
                ciphertext => Ciphertext,
                encapsulated_key => EncapsulatedKey,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Encrypt failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Decrypt with ML-KEM
-spec builtin_pqc_decrypt(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_pqc_decrypt(Args, CaseId) ->
    Ciphertext = maps:get(ciphertext, Args),
    EncapsulatedKey = maps:get(encapsulated_key, Args),
    Keypair = maps:get(keypair, Args),

    logger:debug("pqc_decrypt called by case ~s", [CaseId]),

    %% Delegate to pqc_crypto module
    case pqc_crypto:decapsulate_and_decrypt(Ciphertext, EncapsulatedKey, Keypair) of
        {ok, Plaintext} ->
            {ok, #{
                plaintext => Plaintext,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Decrypt failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Query blockchain
-spec builtin_chain_query(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_chain_query(Args, CaseId) ->
    QueryType = maps:get(query_type, Args),
    Params = maps:get(params, Args),

    logger:debug("chain_query called by case ~s: type=~p", [CaseId, QueryType]),

    %% Route to appropriate blockchain query handler
    case QueryType of
        block ->
            {ok, #{result => <<"block_query_not_implemented">>, case_id => CaseId}};
        transaction ->
            {ok, #{result => <<"tx_query_not_implemented">>, case_id => CaseId}};
        account ->
            {ok, #{result => <<"account_query_not_implemented">>, case_id => CaseId}};
        validator ->
            {ok, #{result => <<"validator_query_not_implemented">>, case_id => CaseId}};
        _ ->
            {error, <<"Unknown query type">>}
    end.

%% @doc Built-in: Submit transaction
-spec builtin_chain_submit(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_chain_submit(Args, CaseId) ->
    TxType = maps:get(tx_type, Args),
    Params = maps:get(params, Args),

    logger:debug("chain_submit called by case ~s: type=~p", [CaseId, TxType]),

    %% Route to appropriate transaction handler
    case TxType of
        transfer ->
            {ok, #{tx_id => <<"transfer_tx_not_implemented">>, case_id => CaseId}};
        key_registration ->
            {ok, #{tx_id => <<"key_reg_tx_not_implemented">>, case_id => CaseId}};
        contract_deploy ->
            {ok, #{tx_id => <<"deploy_tx_not_implemented">>, case_id => CaseId}};
        contract_call ->
            {ok, #{tx_id => <<"call_tx_not_implemented">>, case_id => CaseId}};
        _ ->
            {error, <<"Unknown transaction type">>}
    end.

%% @doc Built-in: Create PQC identity
-spec builtin_identity_create(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_identity_create(Args, CaseId) ->
    Options = maps:get(options, Args, #{}),
    Algorithm = maps:get(algorithm, Options, ml_dsa_65),

    logger:debug("identity_create called by case ~s: algorithm=~p", [CaseId, Algorithm]),

    %% Delegate to pqc_identity module
    case pqc_identity:create(Algorithm, Options) of
        {ok, Identity} ->
            {ok, #{
                identity => Identity,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Identity creation failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Lookup identity
-spec builtin_identity_lookup(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_identity_lookup(Args, CaseId) ->
    Address = maps:get(address, Args),

    logger:debug("identity_lookup called by case ~s: address=~s", [CaseId, Address]),

    %% Delegate to pqc_identity module
    case pqc_identity:lookup(Address) of
        {ok, Identity} ->
            {ok, #{
                identity => Identity,
                case_id => CaseId
            }};
        {error, not_found} ->
            {error, <<"Identity not found">>};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Identity lookup failed: ~p", [Reason]))}
    end.

%% @doc Built-in: Call smart contract
-spec builtin_contract_call(map(), binary()) -> {ok, map()} | {error, binary()}.
builtin_contract_call(Args, CaseId) ->
    ContractAddress = maps:get(address, Args),
    Transition = maps:get(transition, Args),
    TransitionArgs = maps:get(args, Args),

    logger:debug("contract_call called by case ~s: contract=~s transition=~p",
                [CaseId, ContractAddress, Transition]),

    %% Delegate to pqc_contract module
    case pqc_contract:call(ContractAddress, Transition, TransitionArgs) of
        {ok, Result} ->
            {ok, #{
                result => Result,
                case_id => CaseId
            }};
        {error, Reason} ->
            {error, iolist_to_binary(io_lib:format("Contract call failed: ~p", [Reason]))}
    end.

%%% ============================================================================
%%% Internal Functions: Validation
%%% ============================================================================

%% @doc Validate arguments against JSON schema
-spec validate_args_internal(tool_schema(), map()) -> ok | {error, [binary()]}.
validate_args_internal(Schema, Args) ->
    InputSchema = maps:get(input_schema, Schema),

    %% Extract required fields
    Required = maps:get(required, InputSchema, []),
    Properties = maps:get(properties, InputSchema, #{}),

    %% Check required fields
    MissingFields = [Field || Field <- Required, not maps:is_key(Field, Args)],

    case MissingFields of
        [] ->
            %% Validate field types
            case validate_field_types(Args, Properties) of
                [] -> ok;
                TypeErrors -> {error, TypeErrors}
            end;
        _ ->
            Errors = [iolist_to_binary(io_lib:format("Missing required field: ~s", [F]))
                      || F <- MissingFields],
            {error, Errors}
    end.

%% @doc Validate field types
-spec validate_field_types(map(), map()) -> [binary()].
validate_field_types(Args, Properties) ->
    maps:fold(fun(Key, Value, Acc) ->
        case maps:get(Key, Properties, undefined) of
            undefined ->
                %% Unknown field - allow it (permissive)
                Acc;
            PropSpec ->
                case validate_field_type(Key, Value, PropSpec) of
                    ok -> Acc;
                    {error, Error} -> [Error | Acc]
                end
        end
    end, [], Args).

%% @doc Validate a single field type
-spec validate_field_type(atom() | binary(), term(), map()) -> ok | {error, binary()}.
validate_field_type(Key, Value, PropSpec) ->
    ExpectedType = maps:get(type, PropSpec, any),

    Valid = case ExpectedType of
        binary -> is_binary(Value);
        atom -> is_atom(Value);
        integer -> is_integer(Value);
        float -> is_float(Value);
        number -> is_number(Value);
        boolean -> is_boolean(Value);
        list -> is_list(Value);
        map -> is_map(Value);
        object -> is_map(Value);
        any -> true;
        _ -> true
    end,

    case Valid of
        true ->
            %% Check enum constraint if present
            case maps:get(enum, PropSpec, undefined) of
                undefined -> ok;
                EnumValues ->
                    case lists:member(Value, EnumValues) of
                        true -> ok;
                        false ->
                            {error, iolist_to_binary(
                                io_lib:format("Field ~p value ~p not in enum: ~p",
                                            [Key, Value, EnumValues]))}
                    end
            end;
        false ->
            {error, iolist_to_binary(
                io_lib:format("Field ~p has wrong type: expected ~p, got ~p",
                            [Key, ExpectedType, typeof(Value)]))}
    end.

%% @doc Get type of a term
-spec typeof(term()) -> atom().
typeof(V) when is_binary(V) -> binary;
typeof(V) when is_atom(V) -> atom;
typeof(V) when is_integer(V) -> integer;
typeof(V) when is_float(V) -> float;
typeof(V) when is_boolean(V) -> boolean;
typeof(V) when is_list(V) -> list;
typeof(V) when is_map(V) -> map;
typeof(_) -> unknown.

%%% ============================================================================
%%% Internal Functions: Tool Execution
%%% ============================================================================

%% @doc Execute a tool handler with proper error handling
-spec execute_tool_handler(tool_handler(), map(), binary(), tool_name()) ->
    {ok, term()} | {error, term()}.
execute_tool_handler(Handler, Args, CaseId, ToolName) ->
    StartTime = erlang:monotonic_time(microsecond),

    try Handler(Args, CaseId) of
        {ok, Result} ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            logger:debug("Tool ~s executed successfully in ~pμs", [ToolName, Duration]),
            {ok, Result};
        {error, Reason} ->
            Duration = erlang:monotonic_time(microsecond) - StartTime,
            logger:warning("Tool ~s failed in ~pμs: ~p", [ToolName, Duration, Reason]),
            {error, Reason};
        Other ->
            logger:error("Tool ~s returned invalid result: ~p", [ToolName, Other]),
            {error, <<"Tool handler returned invalid result">>}
    catch
        Class:Reason:Stacktrace ->
            logger:error("Tool ~s crashed: ~p:~p~n~p",
                        [ToolName, Class, Reason, Stacktrace]),
            {error, iolist_to_binary(
                io_lib:format("Tool handler crashed: ~p:~p", [Class, Reason]))}
    end.

%%% ============================================================================
%%% Internal Functions: Registry
%%% ============================================================================

%% @doc Register with gproc if available
-spec register_with_gproc() -> ok.
register_with_gproc() ->
    case code:is_loaded(gproc) of
        false -> ok;
        _ ->
            try gproc:reg({n, l, {pqc_tools, registry}}), ok
            catch _:_ -> ok
            end
    end,
    ok.

%% @doc Unregister from gproc
-spec unregister_from_gproc() -> ok.
unregister_from_gproc() ->
    case code:is_loaded(gproc) of
        false -> ok;
        _ ->
            try gproc:unreg({n, l, {pqc_tools, registry}})
            catch _:_ -> ok
            end
    end,
    ok.
