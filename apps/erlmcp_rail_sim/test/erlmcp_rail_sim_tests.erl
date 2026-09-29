-module(erlmcp_rail_sim_tests).
-include_lib("eunit/include/eunit.hrl").

msg(Obl, Cid) -> #{obligation_id => Obl, correlation_id => Cid}.

%% Drive obligation o1 to a terminal state; retry only via reconcile.
drive(Rail, o1, Faults) -> drive(Rail, erlmcp_rail_recon:new(o1), Faults, 1, first).

drive(Rail, R, [F | Fs], N, Mode) ->
    Cid = {o1, N},
    {ok, R1} = case Mode of
                   first -> erlmcp_rail_recon:submit(R, Cid);
                   retry -> erlmcp_rail_recon:retry(R, Cid)
               end,
    {Rail1, Reply} = erlmcp_rail_sim:submit(Rail, msg(o1, Cid), F),
    R2 = case Reply of timeout -> erlmcp_rail_recon:on_timeout(R1); {ack, _} -> R1 end,
    settle(Rail1, R2, Fs, N);
drive(Rail, R, [], _, _) -> {Rail, R}.

settle(Rail, R, Fs, N) ->
    {Rail2, Evs} = erlmcp_rail_sim:poll(erlmcp_rail_sim:tick(Rail, 5)),
    R1 = lists:foldl(fun(E, A) -> erlmcp_rail_recon:observe(A, E) end, R, Evs),
    R2 = case erlmcp_rail_recon:state(R1) of
             S when S =:= unknown; S =:= accepted; S =:= submitted ->
                 Cid = lists:last(erlmcp_rail_recon:correlation_ids(R1)),
                 erlmcp_rail_recon:reconcile(R1, erlmcp_rail_sim:query(Rail2, Cid));
             _ -> R1
         end,
    case erlmcp_rail_recon:state(R2) of
        not_accepted -> drive(Rail2, R2, Fs, N + 1, retry);
        _ -> {Rail2, R2}
    end.

drop_ack_reconciles_to_single_settlement_test() ->
    {Rail, R} = drive(erlmcp_rail_sim:new(), o1, [drop_ack, none]),
    ?assertEqual(settled, erlmcp_rail_recon:state(R)),
    ?assertEqual(1, erlmcp_rail_sim:settlements(Rail, o1)),
    ?assertEqual(1, length(erlmcp_rail_recon:correlation_ids(R))).

blind_retry_after_drop_ack_double_settles_on_rail_test() ->
    %% The hazard: retry with a fresh correlation id without reconciling.
    Rail0 = erlmcp_rail_sim:new(),
    {Rail1, timeout} = erlmcp_rail_sim:submit(Rail0, msg(o1, c1), drop_ack),
    {Rail2, {ack, c2}} = erlmcp_rail_sim:submit(Rail1, msg(o1, c2), none),
    ?assertEqual(2, erlmcp_rail_sim:settlements(Rail2, o1)).

recon_refuses_blind_retry_from_unknown_test() ->
    {ok, R1} = erlmcp_rail_recon:submit(erlmcp_rail_recon:new(o1), c1),
    R2 = erlmcp_rail_recon:on_timeout(R1),
    ?assertEqual(unknown, erlmcp_rail_recon:state(R2)),
    ?assertEqual({error, 'REFUSED_BLIND_RETRY'}, erlmcp_rail_recon:retry(R2, c2)),
    ?assertEqual({error, 'REFUSED_BLIND_RETRY'}, erlmcp_rail_recon:submit(R2, c2)).

retry_eligible_only_when_non_acceptance_proved_test() ->
    Rail = erlmcp_rail_sim:new(),
    {ok, R1} = erlmcp_rail_recon:submit(erlmcp_rail_recon:new(o1), c1),
    R2 = erlmcp_rail_recon:on_timeout(R1),   %% request lost before the rail saw it
    R3 = erlmcp_rail_recon:reconcile(R2, erlmcp_rail_sim:query(Rail, c1)),
    ?assertEqual(not_accepted, erlmcp_rail_recon:state(R3)),
    ?assertMatch({ok, _}, erlmcp_rail_recon:retry(R3, c2)).

duplicate_status_is_idempotent_test() ->
    {Rail, R} = drive(erlmcp_rail_sim:new(), o1, [duplicate_status]),
    ?assertEqual(settled, erlmcp_rail_recon:state(R)),
    ?assertEqual(1, erlmcp_rail_sim:settlements(Rail, o1)).

reordered_status_converges_test() ->
    {_, R} = drive(erlmcp_rail_sim:new(), o1, [reorder]),
    ?assertEqual(settled, erlmcp_rail_recon:state(R)).

delayed_reject_is_not_settlement_test() ->
    %% Accepted first, rejected later: ledger must not show settled.
    Rail0 = erlmcp_rail_sim:new(),
    {ok, R0} = erlmcp_rail_recon:submit(erlmcp_rail_recon:new(o1), c1),
    {Rail1, {ack, c1}} = erlmcp_rail_sim:submit(Rail0, msg(o1, c1), delayed_reject),
    {Rail2, Ev0} = erlmcp_rail_sim:poll(Rail1),
    R1 = lists:foldl(fun(E, A) -> erlmcp_rail_recon:observe(A, E) end, R0, Ev0),
    ?assertEqual(accepted, erlmcp_rail_recon:state(R1)),
    Rail3 = erlmcp_rail_sim:tick(Rail2, 3),
    {Rail4, Ev1} = erlmcp_rail_sim:poll(Rail3),
    R2 = lists:foldl(fun(E, A) -> erlmcp_rail_recon:observe(A, E) end, R1, Ev1),
    ?assertEqual(rejected, erlmcp_rail_recon:state(R2)),
    ?assertEqual(0, erlmcp_rail_sim:settlements(Rail4, o1)).

query_hides_final_outcome_until_due_test() ->
    {Rail, timeout} = erlmcp_rail_sim:submit(erlmcp_rail_sim:new(), msg(o1, c1), drop_ack),
    ?assertEqual({ok, accepted}, erlmcp_rail_sim:query(Rail, c1)),
    ?assertEqual({ok, settled}, erlmcp_rail_sim:query(erlmcp_rail_sim:tick(Rail, 1), c1)).

%% Property-style sweep: every fault sequence of length 1..3 through the
%% reconciling client yields <= 1 rail settlement per obligation.
single_settlement_over_all_fault_sequences_test() ->
    Faults = [none, drop_ack, duplicate_status, reorder, delayed_reject],
    Seqs = [[A] || A <- Faults] ++ [[A, B] || A <- Faults, B <- Faults]
        ++ [[A, B, C] || A <- Faults, B <- Faults, C <- Faults],
    lists:foreach(
      fun(Seq) ->
              {Rail, _} = drive(erlmcp_rail_sim:new(), o1, Seq),
              ?assert(erlmcp_rail_sim:settlements(Rail, o1) =< 1)
      end, Seqs).
