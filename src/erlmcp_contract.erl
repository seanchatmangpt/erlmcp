%%%-------------------------------------------------------------------
%% @doc MCP+ Contract Management - Sealed Operating Contracts
%%
%% Implements sealed operating contracts with:
%% - Cryptographic signatures (Ed25519)
%% - Idempotent transformation (mu . mu = mu)
%% - Hash consistency (hash(A) = hash(mu(O)))
%% - Specification satisfaction (O |= Sigma)
%%
%% Contracts are immutable once sealed. Changes require new contracts
%% with epoch increments and explicit revocation of old versions.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_contract).

-include("erlmcp_governance.hrl").

%% API
-export([
    %% Contract Creation
    create/4,
    seal/2,
    verify_signature/1,

    %% Contract Lookup
    get/1,
    get_by_family/1,
    list_active/0,

    %% Contract Validation
    validate/1,
    check_preconditions/2,
    check_invariants/2,
    check_postconditions/3,

    %% Contract Lifecycle
    revoke/2,
    is_active/1,
    is_expired/1,

    %% Epoch Management
    current_epoch/0,
    advance_epoch/1,

    %% Tier Classification
    classify_tier/1,
    validate_tier/2,

    %% Hash Operations (mu transformation)
    compute_origin_hash/1,
    compute_artifact_hash/1,
    verify_hash_consistency/1
]).

%% gen_server callbacks (for contract registry)
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type create_opts() :: #{
    tier => automation_tier(),
    capabilities => [binary()],
    boundaries => map(),
    expires_at => timestamp_ms()
}.

-type seal_result() :: {ok, #mcp_contract{}} | {error, term()}.
-type validation_result() :: ok | {error, {pos_integer(), binary(), map()}}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
    contracts :: #{contract_id() => #mcp_contract{}},
    families :: #{contract_family() => [contract_id()]},
    current_epoch :: epoch(),
    private_key :: binary() | undefined,
    public_key :: binary()
}).

%%====================================================================
%% API - Contract Creation
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Create a new unsigned contract from specification.
-spec create(contract_family(), binary(), map(), create_opts()) ->
    {ok, #mcp_contract{}} | {error, term()}.
create(Family, Version, Specification, Opts) when
    is_binary(Family), is_binary(Version), is_map(Specification), is_map(Opts) ->
    Now = erlang:system_time(millisecond),
    Id = generate_contract_id(Family, Version, Now),

    Contract = #mcp_contract{
        id = Id,
        family = Family,
        version = Version,
        specification = Specification,
        invariants = maps:get(invariants, Opts, []),
        preconditions = maps:get(preconditions, Opts, []),
        postconditions = maps:get(postconditions, Opts, []),
        tier = maps:get(tier, Opts, ?TIER_A_AUTOMATE),
        capabilities = maps:get(capabilities, Opts, []),
        boundaries = maps:get(boundaries, Opts, #{}),
        epoch = 0,  % Set during sealing
        public_key = <<>>,
        signature = <<>>,
        created_at = Now,
        expires_at = maps:get(expires_at, Opts, undefined),
        revoked = false,
        origin_hash = <<>>,
        artifact_hash = <<>>
    },

    %% Compute hashes
    %% Origin hash must be computed first and set before artifact hash
    %% (artifact hash includes origin_hash in its computation)
    OriginHash = compute_origin_hash(Specification),
    ContractWithOrigin = Contract#mcp_contract{origin_hash = OriginHash},
    ArtifactHash = compute_artifact_hash(ContractWithOrigin),

    {ok, ContractWithOrigin#mcp_contract{artifact_hash = ArtifactHash}}.

%% @doc Seal a contract with cryptographic signature.
-spec seal(#mcp_contract{}, binary()) -> seal_result().
seal(Contract, PrivateKey) when is_binary(PrivateKey) ->
    gen_server:call(?MODULE, {seal, Contract, PrivateKey}).

%% @doc Verify a contract's signature.
-spec verify_signature(#mcp_contract{}) -> boolean().
verify_signature(#mcp_contract{} = Contract) ->
    SignableData = contract_to_signable(Contract),
    verify_ed25519(
        Contract#mcp_contract.signature,
        SignableData,
        Contract#mcp_contract.public_key
    ).

%%====================================================================
%% API - Contract Lookup
%%====================================================================

%% @doc Get a contract by ID.
-spec get(contract_id()) -> {ok, #mcp_contract{}} | {error, not_found}.
get(ContractId) ->
    gen_server:call(?MODULE, {get, ContractId}).

%% @doc Get all contracts in a family.
-spec get_by_family(contract_family()) -> [#mcp_contract{}].
get_by_family(Family) ->
    gen_server:call(?MODULE, {get_by_family, Family}).

%% @doc List all active (non-revoked, non-expired) contracts.
-spec list_active() -> [#mcp_contract{}].
list_active() ->
    gen_server:call(?MODULE, list_active).

%%====================================================================
%% API - Contract Validation
%%====================================================================

%% @doc Validate a contract for use.
-spec validate(#mcp_contract{}) -> validation_result().
validate(#mcp_contract{} = Contract) ->
    Checks = [
        {signature, fun() -> verify_signature(Contract) end},
        {not_revoked, fun() -> not Contract#mcp_contract.revoked end},
        {not_expired, fun() -> not is_expired(Contract) end},
        {hash_consistency, fun() -> verify_hash_consistency(Contract) end}
    ],
    run_validation_checks(Checks).

%% @doc Check preconditions against input.
-spec check_preconditions(#mcp_contract{}, map()) -> validation_result().
check_preconditions(#mcp_contract{preconditions = []}, _Input) ->
    ok;
check_preconditions(#mcp_contract{specification = Spec}, Input) ->
    case maps:get(<<"inputSchema">>, Spec, undefined) of
        undefined -> ok;
        Schema -> validate_against_schema(Input, Schema)
    end.

%% @doc Check invariants are maintained.
-spec check_invariants(#mcp_contract{}, map()) -> validation_result().
check_invariants(#mcp_contract{invariants = []}, _Context) ->
    ok;
check_invariants(#mcp_contract{invariants = Invariants}, Context) ->
    check_invariant_list(Invariants, Context).

%% @doc Check postconditions against output.
-spec check_postconditions(#mcp_contract{}, map(), map()) -> validation_result().
check_postconditions(#mcp_contract{postconditions = []}, _Input, _Output) ->
    ok;
check_postconditions(#mcp_contract{specification = Spec}, _Input, Output) ->
    case maps:get(<<"outputSchema">>, Spec, undefined) of
        undefined -> ok;
        Schema -> validate_against_schema(Output, Schema)
    end.

%%====================================================================
%% API - Contract Lifecycle
%%====================================================================

%% @doc Revoke a contract.
-spec revoke(contract_id(), binary()) -> ok | {error, term()}.
revoke(ContractId, Reason) ->
    gen_server:call(?MODULE, {revoke, ContractId, Reason}).

%% @doc Check if a contract is active.
-spec is_active(#mcp_contract{}) -> boolean().
is_active(#mcp_contract{revoked = true}) -> false;
is_active(Contract) -> not is_expired(Contract).

%% @doc Check if a contract is expired.
-spec is_expired(#mcp_contract{}) -> boolean().
is_expired(#mcp_contract{expires_at = undefined}) -> false;
is_expired(#mcp_contract{expires_at = ExpiresAt}) ->
    erlang:system_time(millisecond) > ExpiresAt.

%%====================================================================
%% API - Epoch Management
%%====================================================================

%% @doc Get current signing epoch.
-spec current_epoch() -> epoch().
current_epoch() ->
    gen_server:call(?MODULE, current_epoch).

%% @doc Advance to new epoch (revokes old epoch keys).
-spec advance_epoch(binary()) -> {ok, epoch()} | {error, term()}.
advance_epoch(AuthorizationSignature) ->
    gen_server:call(?MODULE, {advance_epoch, AuthorizationSignature}).

%%====================================================================
%% API - Tier Classification
%%====================================================================

%% @doc Classify a workflow into automation tier.
-spec classify_tier(map()) -> automation_tier().
classify_tier(WorkflowSpec) when is_map(WorkflowSpec) ->
    %% Classification logic based on workflow properties
    case maps:get(<<"requires_human_judgment">>, WorkflowSpec, false) of
        true -> ?TIER_C_DO_NOT_AUTOMATE;
        false ->
            case maps:get(<<"requires_supervision">>, WorkflowSpec, false) of
                true -> ?TIER_B_ASSIST;
                false -> ?TIER_A_AUTOMATE
            end
    end.

%% @doc Validate operation against tier restrictions.
-spec validate_tier(automation_tier(), binary()) -> validation_result().
validate_tier(?TIER_C_DO_NOT_AUTOMATE, _Operation) ->
    {error, {?REFUSAL_CONTRACT_TIER_VIOLATION,
             <<"Operation in do-not-automate tier">>,
             #{tier => tier_c_do_not_automate}}};
validate_tier(?TIER_B_ASSIST, Operation) ->
    %% Tier B requires supervision flag
    case is_supervised() of
        true -> ok;
        false ->
            {error, {?REFUSAL_CONTRACT_TIER_VIOLATION,
                     <<"Tier B operation requires supervision">>,
                     #{tier => tier_b_assist, operation => Operation}}}
    end;
validate_tier(?TIER_A_AUTOMATE, _Operation) ->
    ok.

%%====================================================================
%% API - Hash Operations
%%====================================================================

%% @doc Compute hash of origin specification.
-spec compute_origin_hash(map()) -> hash().
compute_origin_hash(Specification) when is_map(Specification) ->
    CanonicalJson = erlmcp_json:canonical_encode(Specification),
    crypto:hash(sha256, CanonicalJson).

%% @doc Compute hash of contract artifact (mu(O)).
-spec compute_artifact_hash(#mcp_contract{}) -> hash().
compute_artifact_hash(#mcp_contract{} = Contract) ->
    %% Idempotent: mu(mu(O)) = mu(O)
    %% Only hash structural fields, not signature
    SignableData = contract_to_signable(Contract),
    crypto:hash(sha256, SignableData).

%% @doc Verify hash(A) = hash(mu(O)).
-spec verify_hash_consistency(#mcp_contract{}) -> boolean().
verify_hash_consistency(#mcp_contract{} = Contract) ->
    ExpectedArtifactHash = compute_artifact_hash(Contract),
    Contract#mcp_contract.artifact_hash =:= ExpectedArtifactHash.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Generate initial keypair or load from config
    {PublicKey, PrivateKey} = generate_keypair(),
    State = #state{
        contracts = #{},
        families = #{},
        current_epoch = 1,
        private_key = PrivateKey,
        public_key = PublicKey
    },
    {ok, State}.

handle_call({seal, Contract, PrivateKey}, _From, State) ->
    Epoch = State#state.current_epoch,
    PublicKey = derive_public_key(PrivateKey),

    ContractWithEpoch = Contract#mcp_contract{
        epoch = Epoch,
        public_key = PublicKey
    },

    SignableData = contract_to_signable(ContractWithEpoch),
    Signature = sign_ed25519(SignableData, PrivateKey),

    SealedContract = ContractWithEpoch#mcp_contract{
        signature = Signature,
        artifact_hash = compute_artifact_hash(ContractWithEpoch)
    },

    %% Store contract
    NewContracts = maps:put(
        SealedContract#mcp_contract.id,
        SealedContract,
        State#state.contracts
    ),

    %% Update family index
    Family = SealedContract#mcp_contract.family,
    FamilyIds = maps:get(Family, State#state.families, []),
    NewFamilies = maps:put(
        Family,
        [SealedContract#mcp_contract.id | FamilyIds],
        State#state.families
    ),

    {reply, {ok, SealedContract}, State#state{
        contracts = NewContracts,
        families = NewFamilies
    }};

handle_call({get, ContractId}, _From, State) ->
    case maps:get(ContractId, State#state.contracts, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Contract -> {reply, {ok, Contract}, State}
    end;

handle_call({get_by_family, Family}, _From, State) ->
    Ids = maps:get(Family, State#state.families, []),
    Contracts = [maps:get(Id, State#state.contracts) || Id <- Ids,
                 maps:is_key(Id, State#state.contracts)],
    {reply, Contracts, State};

handle_call(list_active, _From, State) ->
    Active = maps:fold(fun(_Id, Contract, Acc) ->
        case is_active(Contract) of
            true -> [Contract | Acc];
            false -> Acc
        end
    end, [], State#state.contracts),
    {reply, Active, State};

handle_call({revoke, ContractId, _Reason}, _From, State) ->
    case maps:get(ContractId, State#state.contracts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Contract ->
            RevokedContract = Contract#mcp_contract{revoked = true},
            NewContracts = maps:put(ContractId, RevokedContract, State#state.contracts),
            {reply, ok, State#state{contracts = NewContracts}}
    end;

handle_call(current_epoch, _From, State) ->
    {reply, State#state.current_epoch, State};

handle_call({advance_epoch, _AuthSig}, _From, State) ->
    %% In production, verify authorization signature
    NewEpoch = State#state.current_epoch + 1,
    {NewPub, NewPriv} = generate_keypair(),
    NewState = State#state{
        current_epoch = NewEpoch,
        public_key = NewPub,
        private_key = NewPriv
    },
    {reply, {ok, NewEpoch}, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_contract_id(binary(), binary(), timestamp_ms()) -> contract_id().
generate_contract_id(Family, Version, Timestamp) ->
    Data = <<Family/binary, ":", Version/binary, ":",
             (integer_to_binary(Timestamp))/binary>>,
    Hash = crypto:hash(sha256, Data),
    base64:encode(Hash).

-spec contract_to_signable(#mcp_contract{}) -> binary().
contract_to_signable(#mcp_contract{} = C) ->
    %% Canonical representation for signing (excludes signature field)
    Map = #{
        <<"id">> => C#mcp_contract.id,
        <<"family">> => C#mcp_contract.family,
        <<"version">> => C#mcp_contract.version,
        <<"specification">> => C#mcp_contract.specification,
        <<"invariants">> => C#mcp_contract.invariants,
        <<"preconditions">> => C#mcp_contract.preconditions,
        <<"postconditions">> => C#mcp_contract.postconditions,
        <<"tier">> => atom_to_binary(C#mcp_contract.tier, utf8),
        <<"capabilities">> => C#mcp_contract.capabilities,
        <<"boundaries">> => C#mcp_contract.boundaries,
        <<"epoch">> => C#mcp_contract.epoch,
        <<"created_at">> => C#mcp_contract.created_at,
        <<"expires_at">> => C#mcp_contract.expires_at,
        <<"origin_hash">> => base64:encode(C#mcp_contract.origin_hash)
    },
    erlmcp_json:canonical_encode(Map).

-spec generate_keypair() -> {binary(), binary()}.
generate_keypair() ->
    %% Generate Ed25519 keypair
    #{public := Pub, secret := Priv} = crypto:generate_key(eddsa, ed25519),
    {Pub, Priv}.

-spec derive_public_key(binary()) -> binary().
derive_public_key(PrivateKey) ->
    %% For Ed25519, public key can be derived from private
    case byte_size(PrivateKey) of
        64 -> binary:part(PrivateKey, 32, 32);  % Combined format
        32 ->
            %% Generate from seed
            #{public := Pub} = crypto:generate_key(eddsa, ed25519, PrivateKey),
            Pub
    end.

-spec sign_ed25519(binary(), binary()) -> signature().
sign_ed25519(Data, PrivateKey) ->
    crypto:sign(eddsa, sha512, Data, [PrivateKey, ed25519]).

-spec verify_ed25519(signature(), binary(), binary()) -> boolean().
verify_ed25519(Signature, Data, PublicKey) ->
    try
        crypto:verify(eddsa, sha512, Data, Signature, [PublicKey, ed25519])
    catch
        _:_ -> false
    end.

-spec run_validation_checks([{atom(), fun(() -> boolean())}]) -> validation_result().
run_validation_checks([]) ->
    ok;
run_validation_checks([{Name, CheckFun} | Rest]) ->
    case CheckFun() of
        true -> run_validation_checks(Rest);
        false ->
            {error, {?REFUSAL_CONTRACT_SIGNATURE_INVALID,
                     iolist_to_binary(["Validation failed: ", atom_to_list(Name)]),
                     #{check => Name}}}
    end.

-spec validate_against_schema(map(), map()) -> validation_result().
validate_against_schema(Data, Schema) ->
    case jesse:validate_with_schema(Schema, Data, []) of
        {ok, _} -> ok;
        {error, Errors} ->
            {error, {?REFUSAL_CONTRACT_PRECONDITION_FAILED,
                     <<"Schema validation failed">>,
                     #{errors => format_schema_errors(Errors)}}}
    end.

-spec format_schema_errors(term()) -> binary().
format_schema_errors(Errors) ->
    iolist_to_binary(io_lib:format("~p", [Errors])).

-spec check_invariant_list([binary()], map()) -> validation_result().
check_invariant_list([], _Context) ->
    ok;
check_invariant_list([_Invariant | Rest], Context) ->
    %% In production, evaluate invariant expressions
    %% For now, pass through
    check_invariant_list(Rest, Context).

-spec is_supervised() -> boolean().
is_supervised() ->
    %% Check if current operation has supervision flag
    case erlang:get(governance_supervised) of
        true -> true;
        _ -> false
    end.
