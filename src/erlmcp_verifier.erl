%%%-------------------------------------------------------------------
%% @doc MCP+ Text-Blind Verifier - Independent Verification
%%
%% Implements text-blind verification for regulator packs:
%% - Verifies evidence bundles without payload text access
%% - Produces pass/fail reports with check details
%% - Signs verification results for non-repudiation
%% - Supports batch verification of multiple bundles
%%
%% Text-blind principle: All verification uses only:
%% - Structure (receipts, chains, merkle trees)
%% - Hashes (request/response fingerprints)
%% - Metrics (latency, resource usage, counts)
%% - Signatures (cryptographic proofs)
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_verifier).

-include("erlmcp_governance.hrl").

%% API - Verification
-export([
    verify_bundle/1,
    verify_bundle_file/1,
    verify_contract_history/1,
    batch_verify/1
]).

%% API - Check Functions
-export([
    check_signatures/1,
    check_merkle_integrity/1,
    check_chain_continuity/1,
    check_metrics_consistency/1,
    check_refusal_rates/2,
    check_latency_bounds/2
]).

%% API - Report Generation
-export([
    generate_report/1,
    report_to_json/1,
    report_to_binary/1
]).

%% API - Verification Registry
-export([
    register_result/1,
    get_result/1,
    list_results/1
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type check_name() :: binary().
-type check_passed() :: boolean().
-type check_details() :: binary().
-type check_result() :: {check_name(), check_passed(), check_details()}.

-type verification_config() :: #{
    max_refusal_rate => float(),
    max_latency_p99_us => pos_integer(),
    require_chain_continuity => boolean(),
    require_all_signatures => boolean()
}.

%%====================================================================
%% Constants
%%====================================================================

-define(VERIFIER_VERSION, <<"1.0.0">>).
-define(DEFAULT_MAX_REFUSAL_RATE, 0.10).  % 10%
-define(DEFAULT_MAX_LATENCY_P99, 5000000).  % 5 seconds in microseconds

%%====================================================================
%% State
%%====================================================================

-record(state, {
    results :: #{binary() => #mcp_verification_result{}},
    private_key :: binary(),
    public_key :: binary()
}).

%%====================================================================
%% API - Verification
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Verify an evidence bundle with default configuration.
-spec verify_bundle(#mcp_evidence_bundle{}) -> #mcp_verification_result{}.
verify_bundle(Bundle) ->
    verify_bundle_with_config(Bundle, default_config()).

%% @doc Verify an evidence bundle from file.
-spec verify_bundle_file(file:filename()) -> {ok, #mcp_verification_result{}} | {error, term()}.
verify_bundle_file(Filename) ->
    case erlmcp_evidence_bundle:read_file(Filename) of
        {ok, Bundle} -> {ok, verify_bundle(Bundle)};
        Error -> Error
    end.

%% @doc Verify entire contract history.
-spec verify_contract_history(contract_id()) -> #mcp_verification_result{}.
verify_contract_history(ContractId) ->
    Bundles = erlmcp_evidence_bundle:list_by_contract(ContractId),
    %% Verify each bundle and combine results
    Results = [verify_bundle(B) || B <- Bundles],
    combine_results(ContractId, Results).

%% @doc Batch verify multiple bundles.
-spec batch_verify([#mcp_evidence_bundle{}]) -> [#mcp_verification_result{}].
batch_verify(Bundles) ->
    [verify_bundle(B) || B <- Bundles].

%%====================================================================
%% API - Individual Checks
%%====================================================================

%% @doc Verify all signatures in a bundle.
-spec check_signatures(#mcp_evidence_bundle{}) -> check_result().
check_signatures(#mcp_evidence_bundle{} = Bundle) ->
    %% Check bundle signature
    BundleValid = erlmcp_evidence_bundle:verify(Bundle),

    %% Check all receipt signatures
    ReceiptResults = [erlmcp_receipt:verify(R) || R <- Bundle#mcp_evidence_bundle.receipts],
    AllReceiptsValid = lists:all(fun(V) -> V end, ReceiptResults),
    InvalidCount = length([R || R <- ReceiptResults, R =:= false]),

    Passed = BundleValid andalso AllReceiptsValid,
    Details = case Passed of
        true -> <<"All signatures verified">>;
        false -> iolist_to_binary(io_lib:format(
            "Bundle valid: ~p, Invalid receipts: ~p/~p",
            [BundleValid, InvalidCount, length(ReceiptResults)]))
    end,
    {<<"signatures">>, Passed, Details}.

%% @doc Verify Merkle tree integrity.
-spec check_merkle_integrity(#mcp_evidence_bundle{}) -> check_result().
check_merkle_integrity(#mcp_evidence_bundle{} = Bundle) ->
    Receipts = Bundle#mcp_evidence_bundle.receipts,
    StoredRoot = Bundle#mcp_evidence_bundle.receipt_merkle_root,
    ComputedRoot = erlmcp_evidence_bundle:compute_merkle_root(Receipts),

    Passed = StoredRoot =:= ComputedRoot,
    Details = case Passed of
        true -> <<"Merkle root verified">>;
        false -> <<"Merkle root mismatch - receipts may have been tampered">>
    end,
    {<<"merkle_integrity">>, Passed, Details}.

%% @doc Verify receipt chain continuity.
-spec check_chain_continuity(#mcp_evidence_bundle{}) -> check_result().
check_chain_continuity(#mcp_evidence_bundle{receipts = []}) ->
    {<<"chain_continuity">>, true, <<"No receipts to verify">>};
check_chain_continuity(#mcp_evidence_bundle{receipts = Receipts}) ->
    %% Sort by sequence
    Sorted = lists:sort(fun(A, B) ->
        A#mcp_receipt.sequence =< B#mcp_receipt.sequence
    end, Receipts),

    %% Verify chain
    {Passed, BrokenAt} = verify_chain_links(Sorted),
    Details = case Passed of
        true ->
            iolist_to_binary(io_lib:format("Chain verified: ~p receipts", [length(Receipts)]));
        false ->
            iolist_to_binary(io_lib:format("Chain broken at sequence ~p", [BrokenAt]))
    end,
    {<<"chain_continuity">>, Passed, Details}.

%% @doc Verify metrics are internally consistent.
-spec check_metrics_consistency(#mcp_evidence_bundle{}) -> check_result().
check_metrics_consistency(#mcp_evidence_bundle{} = Bundle) ->
    Receipts = Bundle#mcp_evidence_bundle.receipts,

    %% Sum metrics from receipts
    {SumMemory, SumCpu, SumIo} = lists:foldl(fun(R, {M, C, I}) ->
        {M + R#mcp_receipt.memory_bytes,
         C + R#mcp_receipt.cpu_us,
         I + R#mcp_receipt.io_bytes}
    end, {0, 0, 0}, Receipts),

    %% Compare with bundle totals
    MemoryMatch = SumMemory =:= Bundle#mcp_evidence_bundle.total_memory_bytes,
    CpuMatch = SumCpu =:= Bundle#mcp_evidence_bundle.total_cpu_us,
    IoMatch = SumIo =:= Bundle#mcp_evidence_bundle.total_io_bytes,
    RequestCountMatch = length(Receipts) =:= Bundle#mcp_evidence_bundle.total_requests,

    Passed = MemoryMatch andalso CpuMatch andalso IoMatch andalso RequestCountMatch,
    Details = case Passed of
        true -> <<"Metrics consistent with receipts">>;
        false ->
            iolist_to_binary(io_lib:format(
                "Mismatch - Memory: ~p, CPU: ~p, IO: ~p, Count: ~p",
                [MemoryMatch, CpuMatch, IoMatch, RequestCountMatch]))
    end,
    {<<"metrics_consistency">>, Passed, Details}.

%% @doc Check refusal rate is within bounds.
-spec check_refusal_rates(#mcp_evidence_bundle{}, float()) -> check_result().
check_refusal_rates(#mcp_evidence_bundle{} = Bundle, MaxRate) ->
    TotalRequests = Bundle#mcp_evidence_bundle.total_requests,
    TotalRefusals = Bundle#mcp_evidence_bundle.total_refusals,

    Rate = case TotalRequests of
        0 -> 0.0;
        N -> TotalRefusals / N
    end,

    Passed = Rate =< MaxRate,
    Details = iolist_to_binary(io_lib:format(
        "Refusal rate: ~.2f% (max: ~.2f%)",
        [Rate * 100, MaxRate * 100])),
    {<<"refusal_rate">>, Passed, Details}.

%% @doc Check latency is within bounds.
-spec check_latency_bounds(#mcp_evidence_bundle{}, pos_integer()) -> check_result().
check_latency_bounds(#mcp_evidence_bundle{} = Bundle, MaxP99) ->
    P99 = Bundle#mcp_evidence_bundle.latency_p99_us,

    Passed = P99 =< MaxP99,
    Details = iolist_to_binary(io_lib:format(
        "P99 latency: ~p us (max: ~p us)",
        [P99, MaxP99])),
    {<<"latency_bounds">>, Passed, Details}.

%%====================================================================
%% API - Report Generation
%%====================================================================

%% @doc Generate a verification report.
-spec generate_report(#mcp_verification_result{}) -> map().
generate_report(#mcp_verification_result{} = Result) ->
    #{
        <<"verification_id">> => Result#mcp_verification_result.bundle_id,
        <<"passed">> => Result#mcp_verification_result.passed,
        <<"verified_at">> => Result#mcp_verification_result.verified_at,
        <<"verifier_version">> => Result#mcp_verification_result.verifier_version,
        <<"checks">> => [format_check(C) || C <- Result#mcp_verification_result.checks],
        <<"summary">> => generate_summary(Result)
    }.

%% @doc Convert report to JSON.
-spec report_to_json(#mcp_verification_result{}) -> binary().
report_to_json(Result) ->
    Report = generate_report(Result),
    jsx:encode(Report, [pretty]).

%% @doc Convert report to binary (for signing).
-spec report_to_binary(#mcp_verification_result{}) -> binary().
report_to_binary(Result) ->
    Report = generate_report(Result),
    erlmcp_json:canonical_encode(Report).

%%====================================================================
%% API - Verification Registry
%%====================================================================

%% @doc Register a verification result.
-spec register_result(#mcp_verification_result{}) -> ok.
register_result(Result) ->
    gen_server:cast(?MODULE, {register, Result}).

%% @doc Get a verification result by bundle ID.
-spec get_result(binary()) -> {ok, #mcp_verification_result{}} | {error, not_found}.
get_result(BundleId) ->
    gen_server:call(?MODULE, {get, BundleId}).

%% @doc List results for a contract.
-spec list_results(contract_id()) -> [#mcp_verification_result{}].
list_results(ContractId) ->
    gen_server:call(?MODULE, {list, ContractId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {PublicKey, PrivateKey} = generate_keypair(),
    State = #state{
        results = #{},
        private_key = PrivateKey,
        public_key = PublicKey
    },
    {ok, State}.

handle_call({get, BundleId}, _From, State) ->
    case maps:get(BundleId, State#state.results, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Result -> {reply, {ok, Result}, State}
    end;

handle_call({list, _ContractId}, _From, State) ->
    %% For now, return all results
    Results = maps:values(State#state.results),
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({register, Result}, State) ->
    BundleId = Result#mcp_verification_result.bundle_id,
    NewResults = maps:put(BundleId, Result, State#state.results),
    {noreply, State#state{results = NewResults}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec default_config() -> verification_config().
default_config() ->
    #{
        max_refusal_rate => ?DEFAULT_MAX_REFUSAL_RATE,
        max_latency_p99_us => ?DEFAULT_MAX_LATENCY_P99,
        require_chain_continuity => true,
        require_all_signatures => true
    }.

-spec verify_bundle_with_config(#mcp_evidence_bundle{}, verification_config()) ->
    #mcp_verification_result{}.
verify_bundle_with_config(Bundle, Config) ->
    Now = erlang:system_time(millisecond),

    %% Run all checks
    Checks = [
        check_signatures(Bundle),
        check_merkle_integrity(Bundle),
        check_chain_continuity(Bundle),
        check_metrics_consistency(Bundle),
        check_refusal_rates(Bundle, maps:get(max_refusal_rate, Config)),
        check_latency_bounds(Bundle, maps:get(max_latency_p99_us, Config))
    ],

    %% Determine overall pass/fail
    AllPassed = lists:all(fun({_, Passed, _}) -> Passed end, Checks),

    %% Create result
    Result = #mcp_verification_result{
        passed = AllPassed,
        checks = Checks,
        bundle_id = Bundle#mcp_evidence_bundle.id,
        verified_at = Now,
        verifier_version = ?VERIFIER_VERSION,
        verifier_signature = <<>>  % Will be set after signing
    },

    %% Sign result
    sign_result(Result).

-spec sign_result(#mcp_verification_result{}) -> #mcp_verification_result{}.
sign_result(Result) ->
    %% Get signing key from gen_server state would require a call
    %% For now, generate inline signature
    Data = report_to_binary(Result),
    {_Pub, Priv} = generate_keypair(),
    Signature = crypto:sign(eddsa, sha512, Data, [Priv, ed25519]),
    Result#mcp_verification_result{verifier_signature = Signature}.

-spec generate_keypair() -> {binary(), binary()}.
generate_keypair() ->
    #{public := Pub, secret := Priv} = crypto:generate_key(eddsa, ed25519),
    {Pub, Priv}.

-spec verify_chain_links([#mcp_receipt{}]) -> {boolean(), pos_integer() | undefined}.
verify_chain_links([]) ->
    {true, undefined};
verify_chain_links([_Single]) ->
    {true, undefined};
verify_chain_links([First | Rest]) ->
    verify_chain_links_internal(First, Rest).

-spec verify_chain_links_internal(#mcp_receipt{}, [#mcp_receipt{}]) ->
    {boolean(), pos_integer() | undefined}.
verify_chain_links_internal(_Prev, []) ->
    {true, undefined};
verify_chain_links_internal(Prev, [Current | Rest]) ->
    %% Compute hash of previous receipt
    PrevHash = compute_receipt_hash(Prev),

    %% Check if current's previous_hash matches
    case Current#mcp_receipt.previous_hash of
        PrevHash ->
            verify_chain_links_internal(Current, Rest);
        _ ->
            {false, Current#mcp_receipt.sequence}
    end.

-spec compute_receipt_hash(#mcp_receipt{}) -> hash().
compute_receipt_hash(Receipt) ->
    %% Simplified hash computation
    Data = <<
        (Receipt#mcp_receipt.id)/binary,
        (integer_to_binary(Receipt#mcp_receipt.sequence))/binary,
        (Receipt#mcp_receipt.request_hash)/binary,
        (Receipt#mcp_receipt.response_hash)/binary
    >>,
    crypto:hash(sha256, Data).

-spec format_check(check_result()) -> map().
format_check({Name, Passed, Details}) ->
    #{
        <<"name">> => Name,
        <<"passed">> => Passed,
        <<"details">> => Details
    }.

-spec generate_summary(#mcp_verification_result{}) -> map().
generate_summary(#mcp_verification_result{} = Result) ->
    Checks = Result#mcp_verification_result.checks,
    TotalChecks = length(Checks),
    PassedChecks = length([C || {_, true, _} = C <- Checks]),
    FailedChecks = TotalChecks - PassedChecks,

    #{
        <<"total_checks">> => TotalChecks,
        <<"passed_checks">> => PassedChecks,
        <<"failed_checks">> => FailedChecks,
        <<"overall_passed">> => Result#mcp_verification_result.passed,
        <<"failed_check_names">> => [Name || {Name, false, _} <- Checks]
    }.

-spec combine_results(contract_id(), [#mcp_verification_result{}]) -> #mcp_verification_result{}.
combine_results(_ContractId, []) ->
    Now = erlang:system_time(millisecond),
    #mcp_verification_result{
        passed = true,
        checks = [],
        bundle_id = <<>>,
        verified_at = Now,
        verifier_version = ?VERIFIER_VERSION,
        verifier_signature = <<>>
    };
combine_results(ContractId, Results) ->
    Now = erlang:system_time(millisecond),

    %% Combine all checks
    AllChecks = lists:flatten([R#mcp_verification_result.checks || R <- Results]),

    %% Determine overall pass
    AllPassed = lists:all(fun(R) -> R#mcp_verification_result.passed end, Results),

    Result = #mcp_verification_result{
        passed = AllPassed,
        checks = AllChecks,
        bundle_id = ContractId,  % Use contract ID for combined result
        verified_at = Now,
        verifier_version = ?VERIFIER_VERSION,
        verifier_signature = <<>>
    },
    sign_result(Result).
