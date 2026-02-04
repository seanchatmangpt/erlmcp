%%%-------------------------------------------------------------------
%%% @doc pqc_pattern_harness - Canonical net fragments for 43 workflow patterns
%%%
%%% Each pattern returns:
%%%   {Id, Name, Net, InitMarking, Script, ExpectFun}
%%%
%%% Script is a list of steps:
%%%   {drive, MaxSteps}
%%%   {inject, Kind, KeyBin, Payload}
%%%   clear_transient
%%%   {assert_fired, [Tid...]}
%%%
%%% ExpectFun(Marking, FiredTids) -> ok | {error, Reason}
%%%
%%% This exercises all operator families from Van der Aalst/ter Hofstede:
%%% - Splits: AND/XOR/OR/THREAD/MI
%%% - Joins: AND/XOR/OR/PARTIAL + discriminator policies
%%% - Repetition: cycles, loops, recursion
%%% - Concurrency: interleaving, critical section, milestone
%%% - Triggers: transient, persistent
%%% - Cancellation: task, region, MI, case
%%% - Termination: explicit, implicit
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_pattern_harness).

-export([all/0]).

-define(PLACES, places).
-define(META, meta).

%%====================================================================
%% Pattern Catalog
%%====================================================================

all() ->
    [
        %% Split Families (3)
        p_and_split_family(),
        p_xor_split_family(),
        p_or_split_family(),

        %% Branching (5)
        p_parallel_split(),
        p_exclusive_choice(),
        p_deferred_choice(),
        p_multi_choice(),
        p_thread_split(),

        %% Synchronization (11)
        p_synchronization(),
        p_structured_partial_join(),
        p_blocking_partial_join(),
        p_canceling_partial_join(),
        p_generalized_and_join(),
        p_simple_merge(),
        p_multi_merge(),
        p_structured_sync_merge(),
        p_local_sync_merge(),
        p_general_sync_merge(),
        p_thread_merge(),

        %% Repetition (3)
        p_arbitrary_cycles(),
        p_structured_loop(),
        p_recursion(),

        %% Multiple Instances (7)
        p_mi_without_sync(),
        p_mi_design_time(),
        p_mi_runtime_known(),
        p_mi_runtime_unknown(),
        p_mi_static_partial_join(),
        p_mi_canceling_partial_join(),
        p_mi_dynamic_partial_join(),

        %% Concurrency (5)
        p_sequence(),
        p_interleaved_routing(),
        p_interleaved_parallel_routing(),
        p_critical_section(),
        p_milestone(),

        %% Triggers (2)
        p_transient_trigger(),
        p_persistent_trigger(),

        %% Cancellation + Completion (5)
        p_cancel_task(),
        p_cancel_mi_task(),
        p_complete_mi_task(),
        p_cancel_region(),
        p_cancel_case(),

        %% Termination (2)
        p_explicit_termination(),
        p_implicit_termination()
    ].

%%====================================================================
%% Helpers
%%====================================================================

net(Places, Transitions, Order) ->
    #{places => Places, transitions => Transitions, order => Order}.

t(Ins, Outs, Extra) ->
    maps:merge(#{inputs => Ins, outputs => Outs, join => 'and', split => 'and'}, Extra).

in(P) -> #{place => P, weight => 1}.
in(P, W) -> #{place => P, weight => W}.
out(P) -> #{place => P, weight => 1}.

m0(Pairs) ->
    PlacesMap = maps:from_list([{P, mk_toks(P, N)} || {P, N} <- Pairs]),
    #{
        ?PLACES => PlacesMap,
        ?META => #{
            events => #{transient => #{}, persistent => #{}, event => #{}},
            locks => #{},
            join_memory => #{},
            canceled_tasks => #{},
            canceled_mi_tasks => #{},
            completed_mi_tasks => #{},
            place_regions => #{}
        }
    }.

mk_toks(_P, 0) -> [];
mk_toks(P, N) ->
    [#{id => iolist_to_binary([place_key(P), $:, integer_to_binary(I)]),
       stack => [<<>>],
       mi => undefined} || I <- lists:seq(1, N)].

place_key(P) when is_atom(P) -> atom_to_binary(P, utf8);
place_key(P) when is_binary(P) -> P;
place_key(P) -> term_to_binary(P).

count(P, M) ->
    length(maps:get(P, maps:get(?PLACES, M, #{}), [])).

has_any(P, M) -> count(P, M) > 0.

expect_counts(Req) ->
    fun(M, _Fired) ->
        case [{P, C, count(P, M)} || {P, C} <- Req, count(P, M) =/= C] of
            [] -> ok;
            Bad -> {error, {bad_counts, Bad}}
        end
    end.

expect_fired(Need) ->
    fun(_M, Fired) ->
        case lists:all(fun(Tid) -> lists:member(Tid, Fired) end, Need) of
            true -> ok;
            false -> {error, {missing_fires, Need, Fired}}
        end
    end.

expect_mi(P, N) ->
    fun(M, _Fired) ->
        Ts = maps:get(P, maps:get(?PLACES, M, #{}), []),
        case length(Ts) of
            N ->
                MIs = [maps:get(mi, Tok) || Tok <- Ts],
                case lists:all(fun(MI) -> MI =/= undefined end, MIs) of
                    true ->
                        Idxs = [maps:get(idx, MI) || MI <- MIs],
                        Totals = [maps:get(total, MI) || MI <- MIs],
                        case lists:usort(Idxs) =:= lists:seq(1, N) andalso
                             lists:usort(Totals) =:= [N] of
                            true -> ok;
                            false -> {error, {bad_mi_tokens, Ts}}
                        end;
                    false -> {error, {missing_mi_info, Ts}}
                end;
            Other -> {error, {bad_mi_count, P, N, Other}}
        end
    end.

%%====================================================================
%% Split Families (3)
%%====================================================================

p_and_split_family() ->
    Id = <<"P_AND_SPLIT">>,
    Name = <<"AND-split family">>,
    Places = [p0, p1, p2],
    Ts = #{
        t_split => t([in(p0)], [out(p1), out(p2)], #{split => 'and'})
    },
    Net = net(Places, Ts, [t_split]),
    Init = m0([{p0, 1}, {p1, 0}, {p2, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{p0, 0}, {p1, 1}, {p2, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_xor_split_family() ->
    Id = <<"P_XOR_SPLIT">>,
    Name = <<"XOR-split family">>,
    Places = [p0, p1, p2],
    Ts = #{
        t_split => t([in(p0)], [out(p1), out(p2)], #{split => 'xor'})
    },
    Net = net(Places, Ts, [t_split]),
    Init = m0([{p0, 1}, {p1, 0}, {p2, 0}]),
    Script = [{drive, 10}],
    %% Deterministic: chooses first place in sorted order
    Expect = expect_counts([{p0, 0}, {p1, 1}, {p2, 0}]),
    {Id, Name, Net, Init, Script, Expect}.

p_or_split_family() ->
    Id = <<"P_OR_SPLIT">>,
    Name = <<"OR-split family">>,
    Places = [p0, p1, p2],
    Ts = #{
        t_split => t([in(p0)], [out(p1), out(p2)], #{split => 'or'})
    },
    Net = net(Places, Ts, [t_split]),
    Init = m0([{p0, 1}, {p1, 0}, {p2, 0}]),
    Script = [{drive, 10}],
    %% Default OR: all outputs
    Expect = expect_counts([{p0, 0}, {p1, 1}, {p2, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Branching (5)
%%====================================================================

p_parallel_split() ->
    Id = <<"P01_PARALLEL_SPLIT">>,
    Name = <<"Parallel split">>,
    Places = [p0, a, b, j, p_end],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_join => t([in(j, 2)], [out(p_end)], #{join => 'and'})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_join]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {p_end, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{p_end, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_exclusive_choice() ->
    Id = <<"P04_EXCLUSIVE_CHOICE">>,
    Name = <<"Exclusive choice">>,
    Places = [p0, x, y, m, p_end],
    Ts = #{
        t_choice => t([in(p0)], [out(x), out(y)], #{split => 'xor'}),
        t_x => t([in(x)], [out(m)], #{}),
        t_y => t([in(y)], [out(m)], #{}),
        t_merge => t([in(m)], [out(p_end)], #{join => 'xor'})
    },
    Net = net(Places, Ts, [t_choice, t_x, t_y, t_merge]),
    Init = m0([{p0, 1}, {x, 0}, {y, 0}, {m, 0}, {p_end, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{p_end, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_deferred_choice() ->
    Id = <<"P16_DEFERRED_CHOICE">>,
    Name = <<"Deferred choice">>,
    Places = [p0, a, b, p_end],
    Ts = #{
        t_enable => t([in(p0)], [out(a), out(b)], #{split => 'and'}),
        t_a => t([in(a)], [out(p_end)], #{trigger => {transient, <<"go_a">>}}),
        t_b => t([in(b)], [out(p_end)], #{trigger => {transient, <<"go_b">>}})
    },
    Net = net(Places, Ts, [t_enable, t_a, t_b]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {p_end, 0}]),
    Script = [
        {drive, 10},
        {inject, transient, <<"go_b">>, ok},
        {drive, 10},
        clear_transient
    ],
    Expect = fun(M, Fired) ->
        case lists:member(t_b, Fired) andalso count(p_end, M) =:= 1 of
            true -> ok;
            false -> {error, {deferred_choice_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_multi_choice() ->
    Id = <<"P06_MULTI_CHOICE">>,
    Name = <<"Multi-choice">>,
    Places = [p0, a, b],
    Ts = #{
        t_multi => t([in(p0)], [out(a), out(b)], #{split => 'or'})
    },
    Net = net(Places, Ts, [t_multi]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{a, 1}, {b, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_thread_split() ->
    Id = <<"P41_THREAD_SPLIT">>,
    Name = <<"Thread split">>,
    Places = [p0, w],
    Ts = #{
        t_thread => t([in(p0)], [out(w)], #{split => {thread, 3}})
    },
    Net = net(Places, Ts, [t_thread]),
    Init = m0([{p0, 1}, {w, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{w, 3}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Synchronization (11)
%%====================================================================

p_synchronization() ->
    Id = <<"P03_SYNCHRONIZATION">>,
    Name = <<"Synchronization (AND-join)">>,
    Places = [p0, a, b, j, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_join => t([in(j, 2)], [out(endp)], #{join => 'and'})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_join]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_structured_partial_join() ->
    Id = <<"P30_STRUCTURED_PARTIAL_JOIN">>,
    Name = <<"Structured partial join">>,
    Places = [p0, a, b, c, j, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b), out(c)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_c => t([in(c)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j)], [out(endp)],
                  #{join => {partial, 2}, join_policy => {partial_policy, structured}})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_c, t_pj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {c, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_pj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {structured_partial_join_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_blocking_partial_join() ->
    Id = <<"P31_BLOCKING_PARTIAL_JOIN">>,
    Name = <<"Blocking partial join">>,
    Places = [p0, a, b, c, j, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b), out(c)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_c => t([in(c)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j)], [out(endp)],
                  #{join => {partial, 2}, join_policy => {partial_policy, blocking}})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_c, t_pj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {c, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_pj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {blocking_partial_join_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_canceling_partial_join() ->
    Id = <<"P32_CANCELING_PARTIAL_JOIN">>,
    Name = <<"Canceling partial join">>,
    Places = [p0, a, b, c, j, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b), out(c)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_c => t([in(c)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j)], [out(endp)],
                  #{join => {partial, 2}, join_policy => {partial_policy, canceling}})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_c, t_pj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {c, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_fired([t_pj]),
    {Id, Name, Net, Init, Script, Expect}.

p_generalized_and_join() ->
    Id = <<"P33_GENERALIZED_AND_JOIN">>,
    Name = <<"Generalized AND-join">>,
    Places = [p0, a, b, j, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b)], #{split => 'and'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_join => t([in(j, 2)], [out(endp)], #{join => 'and'})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b, t_join]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_simple_merge() ->
    Id = <<"P05_SIMPLE_MERGE">>,
    Name = <<"Simple merge (XOR-join)">>,
    Places = [p0, a, b, m, endp],
    Ts = #{
        t_xor => t([in(p0)], [out(a), out(b)], #{split => 'xor'}),
        t_a => t([in(a)], [out(m)], #{}),
        t_b => t([in(b)], [out(m)], #{}),
        t_m => t([in(m)], [out(endp)], #{join => 'xor'})
    },
    Net = net(Places, Ts, [t_xor, t_a, t_b, t_m]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {m, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_multi_merge() ->
    Id = <<"P08_MULTI_MERGE">>,
    Name = <<"Multi-merge">>,
    Places = [p0, a, b, m, endp],
    Ts = #{
        t_or => t([in(p0)], [out(a), out(b)], #{split => 'or'}),
        t_a => t([in(a)], [out(m)], #{}),
        t_b => t([in(b)], [out(m)], #{}),
        t_m => t([in(m)], [out(endp)], #{join => 'xor'})
    },
    Net = net(Places, Ts, [t_or, t_a, t_b, t_m]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {m, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 2}]),
    {Id, Name, Net, Init, Script, Expect}.

p_structured_sync_merge() ->
    Id = <<"P07_STRUCTURED_SYNC_MERGE">>,
    Name = <<"Structured synchronizing merge">>,
    Places = [p0, a, b, j, endp],
    Ts = #{
        t_or => t([in(p0)], [out(a), out(b)], #{split => 'or'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_orj => t([in(j), in(j)], [out(endp)], #{join => 'or', or_join_kind => structured})
    },
    Net = net(Places, Ts, [t_or, t_a, t_b, t_orj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_orj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {structured_sync_merge_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_local_sync_merge() ->
    Id = <<"P37_LOCAL_SYNC_MERGE">>,
    Name = <<"Local synchronizing merge">>,
    Places = [p0, a, b, j, endp],
    Ts = #{
        t_or => t([in(p0)], [out(a), out(b)], #{split => 'or'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_orj => t([in(j), in(j)], [out(endp)], #{join => 'or', or_join_kind => local})
    },
    Net = net(Places, Ts, [t_or, t_a, t_b, t_orj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_orj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {local_sync_merge_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_general_sync_merge() ->
    Id = <<"P38_GENERAL_SYNC_MERGE">>,
    Name = <<"General synchronizing merge">>,
    Places = [p0, a, b, j, endp],
    Ts = #{
        t_or => t([in(p0)], [out(a), out(b)], #{split => 'or'}),
        t_a => t([in(a)], [out(j)], #{}),
        t_b => t([in(b)], [out(j)], #{}),
        t_orj => t([in(j), in(j)], [out(endp)], #{join => 'or', or_join_kind => general})
    },
    Net = net(Places, Ts, [t_or, t_a, t_b, t_orj]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_orj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {general_sync_merge_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_thread_merge() ->
    Id = <<"P42_THREAD_MERGE">>,
    Name = <<"Thread merge">>,
    Places = [p0, w, endp],
    Ts = #{
        t_thread => t([in(p0)], [out(w)], #{split => {thread, 3}}),
        t_merge => t([in(w, 3)], [out(endp)], #{join => 'and'})
    },
    Net = net(Places, Ts, [t_thread, t_merge]),
    Init = m0([{p0, 1}, {w, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Repetition (3)
%%====================================================================

p_arbitrary_cycles() ->
    Id = <<"P10_ARBITRARY_CYCLES">>,
    Name = <<"Arbitrary cycles">>,
    Places = [p0, loop],
    Ts = #{
        t1 => t([in(p0)], [out(loop)], #{}),
        t2 => t([in(loop)], [out(loop)], #{})
    },
    Net = net(Places, Ts, [t1, t2]),
    Init = m0([{p0, 1}, {loop, 0}]),
    Script = [{drive, 5}],
    Expect = fun(M, Fired) ->
        case lists:member(t2, Fired) andalso has_any(loop, M) of
            true -> ok;
            false -> {error, {arbitrary_cycle_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_structured_loop() ->
    Id = <<"P21_STRUCTURED_LOOP">>,
    Name = <<"Structured loop">>,
    Places = [p0, body, test, endp],
    Ts = #{
        t_enter => t([in(p0)], [out(body)], #{}),
        t_body => t([in(body)], [out(test)], #{}),
        t_back => t([in(test)], [out(body)], #{guard => fun(M) -> count(body, M) =:= 0 end}),
        t_exit => t([in(test)], [out(endp)], #{guard => fun(_) -> true end})
    },
    Net = net(Places, Ts, [t_enter, t_body, t_back, t_exit]),
    Init = m0([{p0, 1}, {body, 0}, {test, 0}, {endp, 0}]),
    Script = [{drive, 10}],
    Expect = fun(M, _Fired) ->
        case has_any(endp, M) of
            true -> ok;
            false -> {error, {structured_loop_failed, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_recursion() ->
    Id = <<"P22_RECURSION">>,
    Name = <<"Recursion">>,
    Places = [p0, rec, endp],
    Ts = #{
        t_call => t([in(p0)], [out(rec)], #{}),
        t_rec => t([in(rec)], [out(endp)], #{})
    },
    Net = net(Places, Ts, [t_call, t_rec]),
    Init = m0([{p0, 1}, {rec, 0}, {endp, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Multiple Instances (7)
%%====================================================================

p_mi_without_sync() ->
    Id = <<"P12_MI_NO_SYNC">>,
    Name = <<"MI without synchronization">>,
    Places = [p0, mi],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 3}}})
    },
    Net = net(Places, Ts, [t_mi]),
    Init = m0([{p0, 1}, {mi, 0}]),
    Script = [{drive, 10}],
    Expect = expect_mi(mi, 3),
    {Id, Name, Net, Init, Script, Expect}.

p_mi_design_time() ->
    Id = <<"P13_MI_DESIGN_TIME">>,
    Name = <<"MI with design-time knowledge">>,
    Places = [p0, mi],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 2}}})
    },
    Net = net(Places, Ts, [t_mi]),
    Init = m0([{p0, 1}, {mi, 0}]),
    Script = [{drive, 10}],
    Expect = expect_mi(mi, 2),
    {Id, Name, Net, Init, Script, Expect}.

p_mi_runtime_known() ->
    Id = <<"P14_MI_RUNTIME_KNOWN">>,
    Name = <<"MI with runtime knowledge">>,
    Places = [p0, mi],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {runtime, fun(_M) -> 4 end}}})
    },
    Net = net(Places, Ts, [t_mi]),
    Init = m0([{p0, 1}, {mi, 0}]),
    Script = [{drive, 10}],
    Expect = expect_mi(mi, 4),
    {Id, Name, Net, Init, Script, Expect}.

p_mi_runtime_unknown() ->
    Id = <<"P15_MI_RUNTIME_UNKNOWN">>,
    Name = <<"MI without prior runtime knowledge">>,
    Places = [p0, mi],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {runtime, fun(_M) -> 3 end}}})
    },
    Net = net(Places, Ts, [t_mi]),
    Init = m0([{p0, 1}, {mi, 0}]),
    Script = [{drive, 10}],
    Expect = expect_mi(mi, 3),
    {Id, Name, Net, Init, Script, Expect}.

p_mi_static_partial_join() ->
    Id = <<"P34_MI_STATIC_PARTIAL_JOIN">>,
    Name = <<"Static partial join for MI">>,
    Places = [p0, mi, j, endp],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 3}}}),
        t_done => t([in(mi)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j)], [out(endp)], #{join => {partial, 2}})
    },
    Net = net(Places, Ts, [t_mi, t_done, t_pj]),
    Init = m0([{p0, 1}, {mi, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = fun(M, Fired) ->
        case lists:member(t_pj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {mi_static_partial_join_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_mi_canceling_partial_join() ->
    Id = <<"P35_MI_CANCELING_PARTIAL_JOIN">>,
    Name = <<"Canceling partial join for MI">>,
    Places = [p0, mi, j, endp],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 3}}}),
        t_done => t([in(mi)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j)], [out(endp)],
                  #{join => {partial, 2}, join_policy => {partial_policy, canceling}})
    },
    Net = net(Places, Ts, [t_mi, t_done, t_pj]),
    Init = m0([{p0, 1}, {mi, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_fired([t_pj]),
    {Id, Name, Net, Init, Script, Expect}.

p_mi_dynamic_partial_join() ->
    Id = <<"P36_MI_DYNAMIC_PARTIAL_JOIN">>,
    Name = <<"Dynamic partial join for MI">>,
    Places = [p0, mi, j, endp],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {runtime, fun(_) -> 5 end}}}),
        t_done => t([in(mi)], [out(j)], #{}),
        t_pj => t([in(j), in(j), in(j), in(j), in(j)], [out(endp)], #{join => {partial, 3}})
    },
    Net = net(Places, Ts, [t_mi, t_done, t_pj]),
    Init = m0([{p0, 1}, {mi, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 100}],
    Expect = fun(M, Fired) ->
        case lists:member(t_pj, Fired) andalso has_any(endp, M) of
            true -> ok;
            false -> {error, {mi_dynamic_partial_join_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Concurrency (5)
%%====================================================================

p_sequence() ->
    Id = <<"P02_SEQUENCE">>,
    Name = <<"Sequence">>,
    Places = [p0, p1, p2],
    Ts = #{
        t1 => t([in(p0)], [out(p1)], #{}),
        t2 => t([in(p1)], [out(p2)], #{})
    },
    Net = net(Places, Ts, [t1, t2]),
    Init = m0([{p0, 1}, {p1, 0}, {p2, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{p2, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_interleaved_routing() ->
    Id = <<"P40_INTERLEAVED_ROUTING">>,
    Name = <<"Interleaved routing">>,
    Places = [p0, a, b, lock, done],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b), out(lock)], #{split => 'and'}),
        t_a => t([in(a), in(lock)], [out(done), out(lock)], #{}),
        t_b => t([in(b), in(lock)], [out(done), out(lock)], #{})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {lock, 0}, {done, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{done, 2}]),
    {Id, Name, Net, Init, Script, Expect}.

p_interleaved_parallel_routing() ->
    Id = <<"P17_INTERLEAVED_PARALLEL">>,
    Name = <<"Interleaved parallel routing">>,
    Places = [p0, a, b, r, done],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b), out(r)], #{split => 'and'}),
        t_a => t([in(a), in(r)], [out(done), out(r)], #{}),
        t_b => t([in(b), in(r)], [out(done), out(r)], #{})
    },
    Net = net(Places, Ts, [t_split, t_a, t_b]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {r, 0}, {done, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{done, 2}]),
    {Id, Name, Net, Init, Script, Expect}.

p_critical_section() ->
    Id = <<"P39_CRITICAL_SECTION">>,
    Name = <<"Critical section">>,
    Places = [p0, a, b, lock, endp],
    Ts = #{
        t_split => t([in(p0)], [out(a), out(b)], #{split => 'and'}),
        t_lock => t([], [out(lock)], #{}),
        t_a => t([in(a), in(lock)], [out(endp), out(lock)], #{}),
        t_b => t([in(b), in(lock)], [out(endp), out(lock)], #{})
    },
    Net = net(Places, Ts, [t_lock, t_split, t_a, t_b]),
    Init = m0([{p0, 1}, {a, 0}, {b, 0}, {lock, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 2}]),
    {Id, Name, Net, Init, Script, Expect}.

p_milestone() ->
    Id = <<"P18_MILESTONE">>,
    Name = <<"Milestone">>,
    Places = [p0, work, milestone, endp],
    Ts = #{
        t_work => t([in(p0)], [out(work)], #{}),
        t_ms => t([in(work)], [out(milestone)], #{}),
        t_b => t([in(milestone)], [out(endp)], #{})
    },
    Net = net(Places, Ts, [t_work, t_ms, t_b]),
    Init = m0([{p0, 1}, {work, 0}, {milestone, 0}, {endp, 0}]),
    Script = [{drive, 50}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Triggers (2)
%%====================================================================

p_transient_trigger() ->
    Id = <<"P23_TRANSIENT_TRIGGER">>,
    Name = <<"Transient trigger">>,
    Places = [p0, endp],
    Ts = #{
        t_go => t([in(p0)], [out(endp)], #{trigger => {transient, <<"pulse">>}})
    },
    Net = net(Places, Ts, [t_go]),
    Init = m0([{p0, 1}, {endp, 0}]),
    Script = [
        {inject, transient, <<"pulse">>, ok},
        {drive, 10},
        clear_transient
    ],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_persistent_trigger() ->
    Id = <<"P24_PERSISTENT_TRIGGER">>,
    Name = <<"Persistent trigger">>,
    Places = [p0, endp],
    Ts = #{
        t_go => t([in(p0)], [out(endp)], #{trigger => {persistent, <<"queue">>}})
    },
    Net = net(Places, Ts, [t_go]),
    Init = m0([{p0, 2}, {endp, 0}]),
    Script = [
        {inject, persistent, <<"queue">>, ok},
        {inject, persistent, <<"queue">>, ok},
        {drive, 50}
    ],
    Expect = expect_counts([{endp, 2}]),
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Cancellation + Completion (5)
%%====================================================================

p_cancel_task() ->
    Id = <<"P19_CANCEL_TASK">>,
    Name = <<"Cancel task">>,
    Places = [p0, running, canceled],
    Ts = #{
        t_start => t([in(p0)], [out(running)], #{}),
        t_cancel => t([in(running)], [out(canceled)], #{})
    },
    Net = net(Places, Ts, [t_start, t_cancel]),
    Init = m0([{p0, 1}, {running, 0}, {canceled, 0}]),
    Script = [{drive, 10}],
    Expect = expect_counts([{canceled, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_cancel_mi_task() ->
    Id = <<"P26_CANCEL_MI_TASK">>,
    Name = <<"Cancel MI task">>,
    Places = [p0, mi, canceled],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 3}}}),
        t_c_all => t([in(mi)], [out(canceled)],
                     #{join => {partial, 1}, join_policy => {discriminator, canceling}})
    },
    Net = net(Places, Ts, [t_mi, t_c_all]),
    Init = m0([{p0, 1}, {mi, 0}, {canceled, 0}]),
    Script = [{drive, 50}],
    Expect = expect_fired([t_c_all]),
    {Id, Name, Net, Init, Script, Expect}.

p_complete_mi_task() ->
    Id = <<"P27_COMPLETE_MI_TASK">>,
    Name = <<"Complete MI task">>,
    Places = [p0, mi, j, endp],
    Ts = #{
        t_mi => t([in(p0)], [out(mi)], #{split => {mi, {design, 3}}}),
        t_done => t([in(mi)], [out(j)], #{}),
        t_all => t([in(j), in(j), in(j)], [out(endp)], #{join => 'and'})
    },
    Net = net(Places, Ts, [t_mi, t_done, t_all]),
    Init = m0([{p0, 1}, {mi, 0}, {j, 0}, {endp, 0}]),
    Script = [{drive, 100}],
    Expect = expect_counts([{endp, 1}]),
    {Id, Name, Net, Init, Script, Expect}.

p_cancel_region() ->
    Id = <<"P25_CANCEL_REGION">>,
    Name = <<"Cancel region">>,
    Places = [p0, r1p, r2p],
    Ts = #{
        t_seed => t([in(p0)], [out(r1p), out(r2p)], #{split => 'and'}),
        t_kill => t([], [], #{cancel => {cancel_region, <<"R1">>}})
    },
    Net = net(Places, Ts, [t_seed, t_kill]),
    Init0 = m0([{p0, 1}, {r1p, 0}, {r2p, 0}]),
    Meta0 = maps:get(meta, Init0),
    Init = Init0#{meta => Meta0#{place_regions => #{r1p => <<"R1">>, r2p => <<"R2">>}}},
    Script = [{drive, 10}],
    Expect = fun(M, Fired) ->
        case lists:member(t_kill, Fired) andalso count(r1p, M) =:= 0 andalso count(r2p, M) =:= 1 of
            true -> ok;
            false -> {error, {cancel_region_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_cancel_case() ->
    Id = <<"P20_CANCEL_CASE">>,
    Name = <<"Cancel case">>,
    Places = [p0, a],
    Ts = #{
        t_seed => t([in(p0)], [out(a)], #{}),
        t_kill => t([], [], #{cancel => explicit_terminate})
    },
    Net = net(Places, Ts, [t_seed, t_kill]),
    Init = m0([{p0, 1}, {a, 0}]),
    Script = [{drive, 10}],
    Expect = fun(M, Fired) ->
        case lists:member(t_kill, Fired) andalso count(a, M) =:= 0 andalso count(p0, M) =:= 0 of
            true -> ok;
            false -> {error, {cancel_case_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

%%====================================================================
%% Termination (2)
%%====================================================================

p_explicit_termination() ->
    Id = <<"P43_EXPLICIT_TERMINATION">>,
    Name = <<"Explicit termination">>,
    Places = [p0, a],
    Ts = #{
        t_seed => t([in(p0)], [out(a)], #{}),
        t_term => t([in(a)], [], #{cancel => explicit_terminate})
    },
    Net = net(Places, Ts, [t_seed, t_term]),
    Init = m0([{p0, 1}, {a, 0}]),
    Script = [{drive, 10}],
    Expect = fun(M, Fired) ->
        case lists:member(t_term, Fired) andalso count(a, M) =:= 0 andalso count(p0, M) =:= 0 of
            true -> ok;
            false -> {error, {explicit_termination_failed, Fired, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.

p_implicit_termination() ->
    Id = <<"P11_IMPLICIT_TERMINATION">>,
    Name = <<"Implicit termination">>,
    Places = [p0, endp],
    Ts = #{
        t1 => t([in(p0)], [out(endp)], #{})
    },
    Net = net(Places, Ts, [t1]),
    Init = m0([{p0, 1}, {endp, 0}]),
    Script = [{drive, 10}],
    Expect = fun(M, _Fired) ->
        case count(p0, M) =:= 0 andalso count(endp, M) =:= 1 of
            true -> ok;
            false -> {error, {implicit_termination_failed, M}}
        end
    end,
    {Id, Name, Net, Init, Script, Expect}.
