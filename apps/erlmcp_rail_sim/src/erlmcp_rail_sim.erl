%%% @doc Deterministic, dependency-free simulator of an independent payment rail.
%%% The rail is the source of truth for acceptance and settlement; a client only
%%% observes it through replies and status events, which can be faulty.
%%% Rail dedupes on correlation id only, so a retry under a fresh correlation
%%% id is a second acceptance (the hazard erlmcp_rail_recon exists to prevent).
%%% Not an ISO 20022 implementation; status names are illustrative.
-module(erlmcp_rail_sim).

-export([new/0, submit/3, tick/2, poll/1, query/2, settlements/2, now/1]).

-export_type([rail/0, fault/0, event/0, msg/0]).

-type fault() :: none | drop_ack | duplicate_status | reorder | delayed_reject.
-type msg() :: #{obligation_id := term(), correlation_id := term(), term() => term()}.
-type event() :: {term(), accepted | rejected | settled}.
-type rail() :: #{clock := non_neg_integer(),
                  seq := non_neg_integer(),
                  accepted := #{term() => msg()},
                  truth := #{term() => accepted | rejected | settled},
                  queue := [{non_neg_integer(), non_neg_integer(), event()}]}.

-spec new() -> rail().
new() ->
    #{clock => 0, seq => 0, accepted => #{}, truth => #{}, queue => []}.

-spec now(rail()) -> non_neg_integer().
now(#{clock := C}) -> C.

%% @doc Submit a message under a fault. Returns the client-visible reply:
%% {ack, Cid} or timeout (ACK dropped; the rail may still have accepted).
-spec submit(rail(), msg(), fault()) -> {rail(), {ack, term()} | timeout}.
submit(Rail = #{accepted := Acc}, #{correlation_id := Cid}, _Fault)
  when is_map_key(Cid, Acc) ->
    %% Duplicate correlation id: rail dedupes, replays current truth as ack.
    {Rail, {ack, Cid}};
submit(Rail0 = #{clock := T, accepted := Acc, truth := Tr},
       Msg = #{correlation_id := Cid}, Fault) ->
    Rail1 = Rail0#{accepted := Acc#{Cid => Msg}, truth := Tr#{Cid => accepted}},
    Plan = plan(Fault, Cid, T),
    Rail2 = lists:foldl(fun({At, Ev}, R) -> enqueue(R, At, Ev) end, Rail1, Plan),
    Truth = final_truth(Fault),
    Rail3 = Rail2#{truth := (maps:get(truth, Rail2))#{Cid => Truth}},
    Reply = case Fault of drop_ack -> timeout; _ -> {ack, Cid} end,
    {Rail3, Reply}.

plan(none, C, T)             -> [{T, {C, accepted}}, {T + 1, {C, settled}}];
plan(drop_ack, C, T)         -> [{T, {C, accepted}}, {T + 1, {C, settled}}];
plan(duplicate_status, C, T) -> [{T, {C, accepted}}, {T, {C, accepted}},
                                 {T + 1, {C, settled}}, {T + 1, {C, settled}}];
plan(reorder, C, T)          -> [{T, {C, settled}}, {T + 1, {C, accepted}}];
plan(delayed_reject, C, T)   -> [{T, {C, accepted}}, {T + 3, {C, rejected}}].

final_truth(delayed_reject) -> rejected;
final_truth(_) -> settled.

enqueue(R = #{seq := S, queue := Q}, At, Ev) ->
    R#{seq := S + 1, queue := Q ++ [{At, S, Ev}]}.

-spec tick(rail(), non_neg_integer()) -> rail().
tick(R = #{clock := C}, N) -> R#{clock := C + N}.

%% @doc Deliver all events due at or before the clock, in (time, seq) order.
-spec poll(rail()) -> {rail(), [event()]}.
poll(R = #{clock := C, queue := Q}) ->
    {Due, Rest} = lists:partition(fun({At, _, _}) -> At =< C end, Q),
    Sorted = lists:sort(Due),
    {R#{queue := Rest}, [Ev || {_, _, Ev} <- Sorted]}.

%% @doc Reconciliation query against rail truth (as of the clock: final
%% outcomes are visible only once their events are due).
-spec query(rail(), term()) -> {ok, accepted | rejected | settled} | unknown_correlation.
query(#{accepted := Acc, truth := Tr, queue := Q, clock := C}, Cid) ->
    case maps:is_key(Cid, Acc) of
        false -> unknown_correlation;
        true ->
            Pending = [Ev || {At, _, {K, _} = Ev} <- Q, K =:= Cid, At > C],
            case Pending of
                [] -> {ok, maps:get(Cid, Tr)};
                _  -> {ok, accepted}
            end
    end.

%% @doc Number of correlation ids for an obligation that the rail settles
%% (rail truth, ignoring delivery timing). Target invariant: =< 1.
-spec settlements(rail(), term()) -> non_neg_integer().
settlements(#{accepted := Acc, truth := Tr}, Obl) ->
    length([C || {C, #{obligation_id := O}} <- maps:to_list(Acc),
                 O =:= Obl, maps:get(C, Tr) =:= settled]).
