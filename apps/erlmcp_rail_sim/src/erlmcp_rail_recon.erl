%%% @doc Client-side settlement/reconciliation state machine.
%%% prepared -> submitted -> {unknown | accepted | rejected} -> {settled | ...}
%%% timeout => unknown => reconcile. Blind retry from unknown is refused; only a
%%% reconciliation that proves non-acceptance (rail has no such correlation id)
%%% makes a new submission eligible. Status events are applied idempotently
%%% and order-insensitively; a ledger `settled` requires an observed settled event.
-module(erlmcp_rail_recon).

-export([new/1, submit/2, on_timeout/1, observe/2, reconcile/2, retry/2,
         state/1, correlation_ids/1]).

-export_type([recon/0, state/0]).

-type state() :: prepared | submitted | unknown | not_accepted | accepted
               | rejected | settled | conflict.
-opaque recon() :: #{obligation := term(), state := state(), cid := term() | undefined,
                     cids := [term()], seen := #{atom() => true}}.

-spec new(term()) -> recon().
new(Obl) ->
    #{obligation => Obl, state => prepared, cid => undefined, cids => [], seen => #{}}.

-spec state(recon()) -> state().
state(#{state := S}) -> S.

-spec correlation_ids(recon()) -> [term()].
correlation_ids(#{cids := C}) -> lists:reverse(C).

%% @doc First submission (from prepared) under Cid.
-spec submit(recon(), term()) -> {ok, recon()} | {error, atom()}.
submit(R = #{state := prepared, cids := Cs}, Cid) ->
    {ok, R#{state := submitted, cid := Cid, cids := [Cid | Cs], seen := #{}}};
submit(#{state := unknown}, _) -> {error, 'REFUSED_BLIND_RETRY'};
submit(_, _) -> {error, 'REFUSED_ILLEGAL_TRANSITION'}.

%% @doc Client timeout: outcome unknown.
-spec on_timeout(recon()) -> recon().
on_timeout(R = #{state := submitted}) -> R#{state := unknown};
on_timeout(R) -> R.

%% @doc Apply a status event for the current correlation id. Idempotent and
%% order-insensitive: state is derived from the set of statuses seen.
-spec observe(recon(), {term(), accepted | rejected | settled}) -> recon().
observe(R = #{cid := Cid, seen := Seen, state := St}, {Cid, Status})
  when St =/= prepared, St =/= not_accepted ->
    R1 = R#{seen := Seen#{Status => true}},
    R1#{state := derive(maps:get(seen, R1))};
observe(R, _Foreign) -> R.

derive(#{settled := true, rejected := true}) -> conflict;
derive(#{settled := true}) -> settled;
derive(#{rejected := true}) -> rejected;
derive(#{accepted := true}) -> accepted.

%% @doc Resolve unknown by a rail query. Only valid from unknown/accepted.
-spec reconcile(recon(), {ok, accepted | rejected | settled} | unknown_correlation) -> recon().
reconcile(R = #{state := unknown}, unknown_correlation) -> R#{state := not_accepted};
reconcile(R = #{state := St, cid := Cid}, {ok, Status})
  when St =:= unknown; St =:= accepted ->
    observe(R#{state := submitted}, {Cid, Status});
reconcile(R, _) -> R.

%% @doc New submission under a fresh correlation id: allowed only after
%% reconciliation proved non-acceptance.
-spec retry(recon(), term()) -> {ok, recon()} | {error, atom()}.
retry(R = #{state := not_accepted, cids := Cs}, Cid) ->
    {ok, R#{state := submitted, cid := Cid, cids := [Cid | Cs], seen := #{}}};
retry(#{state := unknown}, _) -> {error, 'REFUSED_BLIND_RETRY'};
retry(_, _) -> {error, 'REFUSED_ILLEGAL_TRANSITION'}.
