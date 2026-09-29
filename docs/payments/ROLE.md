# Payments Role: erlmcp Rail Simulator

erlmcp supplies an independent, deterministic rail/message simulator in
`apps/erlmcp_rail_sim` (kernel and stdlib only). Money movement is not
modeled; the simulator is the rail whose truth a ledger must observe.

## Modules

- `erlmcp_rail_sim`: rail with per-submission faults `drop_ack`,
  `duplicate_status`, `reorder`, `delayed_reject`. Dedupes on correlation
  id only, so a fresh-id retry is a second acceptance.
- `erlmcp_rail_recon`: client state machine
  prepared, submitted, unknown, not_accepted, accepted, rejected, settled.
  Timeout gives unknown; retry from unknown is `REFUSED_BLIND_RETRY`.

## Falsifier coverage

| Falsifier | Test |
|---|---|
| F3 timeout permits blind retry | `recon_refuses_blind_retry_from_unknown_test`, `blind_retry_after_drop_ack_double_settles_on_rail_test` (hazard witness) |
| F5 ledger SETTLED without observed finality | `delayed_reject_is_not_settlement_test`, `reordered_status_converges_test` |
| F4 ids joinable (partial) | correlation ids carry obligation id; `settlements/2` counts per obligation |
| Target invariant, settlements per obligation <= 1 | `single_settlement_over_all_fault_sequences_test` (all fault sequences up to length 3) |

Other falsifiers are out of scope for this repo. Status names are
illustrative, not ISO 20022 conformance.

## Gate

    erlc -Wall +warnings_as_errors -o OUT apps/erlmcp_rail_sim/src/*.erl apps/erlmcp_rail_sim/test/*.erl
    erl -noshell -pa OUT -eval 'eunit:test([erlmcp_rail_sim_tests])'
