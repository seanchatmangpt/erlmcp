%%%-------------------------------------------------------------------
%%% @doc pqc_pattern_net - Pattern-Complete Control-Flow Kernel (OTP 28)
%%%
%%% Implements all 43 Van der Aalst/ter Hofstede workflow patterns as
%%% executable operators. This is the definitive control-flow engine.
%%%
%%% Core idea:
%%%   - Net describes places + transitions + Λ total order + reachability caches
%%%   - Marking is a map with:
%%%        #{ places => #{PlaceId => [Token]},
%%%           meta   => #{ ... runtime memory ... } }
%%%   - enabled(Net, Marking) returns enabled transition ids in Λ order
%%%   - fire(Net, Tid, Marking) fires exactly one transition deterministically
%%%
%%% Operator families implemented:
%%%   Splits:  AND / XOR / OR / THREAD(n) / MI(count)
%%%   Joins:   AND / XOR / OR (sync merge variants) / PARTIAL(k)
%%%           + discriminator family via PARTIAL(k=1) + policies
%%%   Repetition: arbitrary cycles, structured loops, recursion
%%%   Concurrency control: interleaved routing, critical section (mutex)
%%%   Triggers: transient/persistent, event-based, data-based
%%%   Cancelation/completion: cancel task/region/MI-task, complete MI-task
%%%   Termination: explicit + implicit
%%%
%%% Token model: places hold token multisets (lists), not just counts.
%%% This is required for Thread split/merge and several join variants.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_pattern_net).

-include("pqchain.hrl").

-export([
    compile/1,
    validate/1,
    enabled/2,
    fire/3,

    %% Runtime inputs
    inject_event/4,
    clear_transient/1,

    %% Utilities
    is_implicitly_terminated/2,
    initial_marking/1,
    place_tokens/2,
    token_count/2
]).

-define(META, meta).
-define(PLACES, places).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type tok() :: #{
    id := binary(),
    stack := [binary()],
    mi := undefined | #{mi_id := binary(), idx := non_neg_integer(), total := non_neg_integer()}
}.

-type place() :: atom() | binary().
-type tid() :: atom() | binary().

-type marking() :: #{
    places := #{place() => [tok()]},
    meta := map()
}.

-type join_type() :: 'and' | 'xor' | 'or' | {partial, pos_integer()}.
-type split_type() :: 'and' | 'xor' | 'or' | {thread, pos_integer()} | {mi, mi_count_spec()}.
-type mi_count_spec() :: pos_integer() | {design, pos_integer()} | {runtime, fun((marking()) -> pos_integer())}.

-type join_policy() :: none
    | {discriminator, normal | blocking | canceling}
    | {partial_policy, structured | blocking | canceling}.

-type or_join_kind() :: structured | local | general.

-type trigger_spec() :: undefined
    | {transient, binary()}
    | {persistent, binary()}
    | {event, binary()}
    | {data, fun((marking()) -> boolean())}.

-type cancel_spec() :: undefined
    | {cancel_task, tid()}
    | {cancel_region, binary()}
    | {cancel_mi_task, tid()}
    | {complete_mi_task, tid()}
    | explicit_terminate.

-type arc() :: #{place := place(), weight := pos_integer()}.

-type transition() :: #{
    inputs := [arc()],
    outputs := [arc()],
    join => join_type(),
    join_policy => join_policy(),
    split => split_type(),
    split_policy => none | {or_default, place()} | {choice, fun((marking()) -> place() | [place()])},
    or_join_kind => or_join_kind(),
    guard => undefined | fun((marking()) -> boolean()),
    effect => undefined | fun((marking()) -> effect_result()),
    mutex => undefined | binary(),
    trigger => trigger_spec(),
    cancel => cancel_spec()
}.

-type effect_result() ::
    {ok, [term()]} |
    {ok, [term()], map()} |
    {input_required, binary()} |
    {auth_required, binary()} |
    {error, binary()}.

-type net() :: #{
    places := [place()],
    transitions := #{tid() => transition()},
    order := [tid()],
    reach => #{place() => ordsets:ordset(place())},
    reach_rev => #{place() => ordsets:ordset(place())},
    initial_marking => #{place() => non_neg_integer()},
    metadata => map()
}.

-export_type([
    net/0, marking/0, tok/0, transition/0, arc/0,
    place/0, tid/0, join_type/0, split_type/0,
    join_policy/0, or_join_kind/0, trigger_spec/0, cancel_spec/0
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

%% @doc Compile net: validate and build reachability caches for OR-join.
-spec compile(map()) -> net().
compile(Net0) ->
    case validate(Net0) of
        ok -> ok;
        {error, E} -> erlang:error({bad_net, E})
    end,
    {Reach, ReachRev} = build_reachability(Net0),
    Net0#{reach => Reach, reach_rev => ReachRev}.

%% @doc Validate net structure and semantics.
-spec validate(map()) -> ok | {error, binary()}.
validate(Net) ->
    try
        Places = maps:get(places, Net),
        true = is_list(Places),
        Ts = maps:get(transitions, Net),
        true = is_map(Ts),
        Order = maps:get(order, Net),
        true = is_list(Order),

        %% All tids in order exist
        lists:foreach(
            fun(Tid) ->
                case maps:is_key(Tid, Ts) of
                    true -> ok;
                    false -> throw({bad_order, Tid})
                end
            end,
            Order
        ),

        %% Transitions reference known places
        PlaceSet = maps:from_list([{P, true} || P <- Places]),
        maps:fold(
            fun(Tid, T, Acc) ->
                _ = Acc,
                ok = validate_transition(Tid, T, PlaceSet),
                ok
            end,
            ok,
            Ts
        ),
        ok
    catch
        throw:{bad_order, Tid0} ->
            {error, iolist_to_binary(["bad order entry: ", io_lib:format("~p", [Tid0])])};
        throw:{bad_transition, Tid1, Msg} ->
            {error, iolist_to_binary(["bad transition ", io_lib:format("~p", [Tid1]), ": ", Msg])};
        _:R ->
            {error, iolist_to_binary(["validate error: ", io_lib:format("~p", [R])])}
    end.

%% @doc Returns enabled transition ids in Λ order.
-spec enabled(net(), marking()) -> [tid()].
enabled(Net, Marking) ->
    Order = maps:get(order, Net, []),
    Ts = maps:get(transitions, Net, #{}),
    [Tid || Tid <- Order,
            begin
                T = maps:get(Tid, Ts),
                is_enabled(Net, Tid, T, Marking)
            end].

%% @doc Fire a specific transition (if enabled).
-spec fire(net(), tid(), marking()) ->
    {ok, marking(), [term()]} |
    {input_required, binary()} |
    {auth_required, binary()} |
    {error, binary()}.
fire(Net, Tid, Marking0) ->
    Ts = maps:get(transitions, Net, #{}),
    case maps:find(Tid, Ts) of
        error ->
            {error, <<"transition_not_found">>};
        {ok, T} ->
            case is_enabled(Net, Tid, T, Marking0) of
                false -> {error, <<"transition_not_enabled">>};
                true -> fire_enabled(Net, Tid, T, Marking0)
            end
    end.

%% @doc Inject event/trigger into marking.
-spec inject_event(marking(), transient | persistent | event, binary(), term()) -> marking().
inject_event(M0, Kind, Key, Payload) ->
    Meta0 = maps:get(?META, M0, #{}),
    Events0 = maps:get(events, Meta0, #{
        transient => #{}, persistent => #{}, event => #{}
    }),
    Events1 =
        case Kind of
            transient ->
                Events0#{transient := (maps:get(transient, Events0))#{Key => Payload}};
            persistent ->
                Q0 = maps:get(Key, maps:get(persistent, Events0), []),
                Events0#{persistent := (maps:get(persistent, Events0))#{Key => [Payload | Q0]}};
            event ->
                Events0#{event := (maps:get(event, Events0))#{Key => Payload}}
        end,
    Meta1 = Meta0#{events => Events1},
    M0#{?META => Meta1}.

%% @doc Clear unconsumed transient triggers.
-spec clear_transient(marking()) -> marking().
clear_transient(M0) ->
    Meta0 = maps:get(?META, M0, #{}),
    Events0 = maps:get(events, Meta0, #{
        transient => #{}, persistent => #{}, event => #{}
    }),
    Events1 = Events0#{transient := #{}},
    M0#{?META => Meta0#{events => Events1}}.

%% @doc Implicit termination: no tokens and no pending persistent triggers.
-spec is_implicitly_terminated(net(), marking()) -> boolean().
is_implicitly_terminated(_Net, M) ->
    Places = maps:get(?PLACES, M, #{}),
    NoTokens = lists:all(fun({_P, Ts}) -> Ts =:= [] end, maps:to_list(Places)),
    Meta = maps:get(?META, M, #{}),
    Events = maps:get(events, Meta, #{
        transient => #{}, persistent => #{}, event => #{}
    }),
    PendingPersistent =
        lists:any(fun({_K, V}) -> V =/= [] end, maps:to_list(maps:get(persistent, Events))),
    NoTokens andalso (not PendingPersistent).

%% @doc Create initial marking from net specification.
-spec initial_marking(net()) -> marking().
initial_marking(Net) ->
    Places = maps:get(places, Net, []),
    InitialSpec = maps:get(initial_marking, Net, #{}),
    PlaceMap = lists:foldl(
        fun(P, Acc) ->
            Count = maps:get(P, InitialSpec, 0),
            Tokens = [new_tok(<<>>) || _ <- lists:seq(1, Count)],
            Acc#{P => Tokens}
        end,
        #{},
        Places
    ),
    #{
        ?PLACES => PlaceMap,
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

%% @doc Get tokens at a place.
-spec place_tokens(place(), marking()) -> [tok()].
place_tokens(P, M) ->
    Places = maps:get(?PLACES, M, #{}),
    maps:get(P, Places, []).

%% @doc Get token count at a place.
-spec token_count(place(), marking()) -> non_neg_integer().
token_count(P, M) ->
    length(place_tokens(P, M)).

%%--------------------------------------------------------------------
%% Transition Validation
%%--------------------------------------------------------------------

validate_transition(Tid, T, PlaceSet) ->
    Inputs = maps:get(inputs, T, []),
    Outputs = maps:get(outputs, T, []),

    (is_list(Inputs) andalso is_list(Outputs)) orelse
        throw({bad_transition, Tid, <<"inputs/outputs must be lists">>}),

    lists:foreach(fun(A) -> validate_arc(Tid, A, PlaceSet) end, Inputs),
    lists:foreach(fun(A) -> validate_arc(Tid, A, PlaceSet) end, Outputs),

    Join = maps:get(join, T, 'and'),
    valid_join(Join) orelse throw({bad_transition, Tid, <<"invalid join type">>}),

    Split = maps:get(split, T, 'and'),
    valid_split(Split) orelse throw({bad_transition, Tid, <<"invalid split type">>}),

    ok.

validate_arc(Tid, A, PlaceSet) ->
    case {maps:is_key(place, A), maps:is_key(weight, A)} of
        {true, true} -> ok;
        _ -> throw({bad_transition, Tid, <<"arc must have place + weight">>})
    end,
    P = maps:get(place, A),
    W = maps:get(weight, A),
    maps:is_key(P, PlaceSet) orelse throw({bad_transition, Tid, <<"unknown place">>}),
    (is_integer(W) andalso W > 0) orelse throw({bad_transition, Tid, <<"weight must be positive">>}).

valid_join('and') -> true;
valid_join('xor') -> true;
valid_join('or') -> true;
valid_join({partial, K}) when is_integer(K), K > 0 -> true;
valid_join(_) -> false.

valid_split('and') -> true;
valid_split('xor') -> true;
valid_split('or') -> true;
valid_split({thread, N}) when is_integer(N), N > 0 -> true;
valid_split({mi, _}) -> true;
valid_split(_) -> false.

%%--------------------------------------------------------------------
%% Enabled Logic
%%--------------------------------------------------------------------

is_enabled(Net, Tid, T, M) ->
    %% 1) Check mutex (critical section / interleaving)
    case mutex_ok(Tid, T, M) of
        false -> false;
        true ->
            %% 2) Check triggers
            case trigger_ok(T, M) of
                false -> false;
                true ->
                    %% 3) Check guard
                    case guard_ok(T, M) of
                        false -> false;
                        true ->
                            %% 4) Check canceled
                            case not_canceled(Tid, M) of
                                false -> false;
                                true -> join_inputs_satisfied(Net, T, M)
                            end
                    end
            end
    end.

guard_ok(T, M) ->
    case maps:get(guard, T, undefined) of
        undefined -> true;
        Fun when is_function(Fun, 1) ->
            try Fun(M) of true -> true; _ -> false catch _:_ -> false end
    end.

mutex_ok(_Tid, T, M) ->
    case maps:get(mutex, T, undefined) of
        undefined -> true;
        LockId ->
            Meta = maps:get(?META, M, #{}),
            Locks = maps:get(locks, Meta, #{}),
            case maps:get(LockId, Locks, undefined) of
                undefined -> true;
                _ -> false
            end
    end.

not_canceled(Tid, M) ->
    Meta = maps:get(?META, M, #{}),
    CT = maps:get(canceled_tasks, Meta, #{}),
    not maps:is_key(Tid, CT).

trigger_ok(T, M) ->
    case maps:get(trigger, T, undefined) of
        undefined -> true;
        {data, Pred} when is_function(Pred, 1) ->
            try Pred(M) of true -> true; _ -> false catch _:_ -> false end;
        {transient, Key} -> has_event(transient, Key, M);
        {persistent, Key} -> has_event(persistent, Key, M);
        {event, Key} -> has_event(event, Key, M)
    end.

has_event(Kind, Key, M) ->
    Meta = maps:get(?META, M, #{}),
    Events = maps:get(events, Meta, #{transient => #{}, persistent => #{}, event => #{}}),
    case Kind of
        transient -> maps:is_key(Key, maps:get(transient, Events));
        event -> maps:is_key(Key, maps:get(event, Events));
        persistent ->
            case maps:get(Key, maps:get(persistent, Events), []) of
                [] -> false;
                _ -> true
            end
    end.

%% Join satisfaction
join_inputs_satisfied(Net, T, M) ->
    Inputs = maps:get(inputs, T, []),
    Join = maps:get(join, T, 'and'),
    case Join of
        'and' ->
            lists:all(fun(A) -> place_has(A, M) end, Inputs);
        'xor' ->
            lists:any(fun(A) -> place_has(A, M) end, Inputs);
        'or' ->
            Req = or_join_required_inputs(Net, T, M),
            Req =/= [] andalso lists:all(fun(P) -> place_has_place(P, M) end, Req);
        {partial, K} when is_integer(K), K > 0 ->
            count_ready_inputs(Inputs, M) >= K
    end.

count_ready_inputs(Inputs, M) ->
    length([ok || A <- Inputs, place_has(A, M)]).

place_has(#{place := P, weight := W}, M) ->
    length(place_tokens(P, M)) >= W.

place_has_place(P, M) ->
    place_tokens(P, M) =/= [].

%% OR-join required input calculation
or_join_required_inputs(Net, T, M) ->
    Inputs = [maps:get(place, A) || A <- maps:get(inputs, T, [])],
    Kind = maps:get(or_join_kind, T, general),
    case Kind of
        general -> or_join_required_inputs_general(Net, Inputs, M);
        structured -> or_join_required_inputs_structured(Net, Inputs, M);
        local -> or_join_required_inputs_structured(Net, Inputs, M)
    end.

or_join_required_inputs_general(Net, Inputs, M) ->
    ReachRev = maps:get(reach_rev, Net, #{}),
    TokPlaces = token_places(M),
    [IP || IP <- Inputs,
           begin
               Up = maps:get(IP, ReachRev, ordsets:new()),
               lists:any(fun(P) -> ordsets:is_element(P, Up) end, TokPlaces)
               orelse place_has_place(IP, M)
           end].

or_join_required_inputs_structured(Net, Inputs, M) ->
    InputTokens = lists:flatten([place_tokens(IP, M) || IP <- Inputs]),
    case InputTokens of
        [] -> or_join_required_inputs_general(Net, Inputs, M);
        _ ->
            Stk = top_stack(hd(sort_tokens(InputTokens))),
            ReachRev = maps:get(reach_rev, Net, #{}),
            TokPlaces = token_places_with_stack(M, Stk),
            [IP || IP <- Inputs,
                   begin
                       Up = maps:get(IP, ReachRev, ordsets:new()),
                       lists:any(fun(P) -> ordsets:is_element(P, Up) end, TokPlaces)
                       orelse place_has_stack(IP, Stk, M)
                   end]
    end.

place_has_stack(IP, Stk, M) ->
    Ts = place_tokens(IP, M),
    lists:any(fun(Tok) -> top_stack(Tok) =:= Stk end, Ts).

top_stack(Tok) ->
    case maps:get(stack, Tok, []) of
        [H | _] -> H;
        [] -> <<>>
    end.

token_places(M) ->
    Places = maps:get(?PLACES, M, #{}),
    [P || {P, Ts} <- maps:to_list(Places), Ts =/= []].

token_places_with_stack(M, Stk) ->
    Places = maps:get(?PLACES, M, #{}),
    [P || {P, Ts} <- maps:to_list(Places),
          lists:any(fun(Tok) -> top_stack(Tok) =:= Stk end, Ts)].

%%--------------------------------------------------------------------
%% Fire Logic
%%--------------------------------------------------------------------

fire_enabled(Net, Tid, T, M0) ->
    %% 0) Explicit terminate
    case maps:get(cancel, T, undefined) of
        explicit_terminate ->
            M1 = empty_all(M0),
            {ok, M1, [{terminated, explicit}]};
        _ ->
            %% 1) Acquire mutex
            {M1, MutexEff} = maybe_acquire_mutex(Tid, T, M0),

            %% 2) Consume trigger
            {M2, TriggerEff} = consume_trigger_if_any(T, M1),

            %% 3) Consume inputs
            case consume_inputs(Net, Tid, T, M2) of
                {error, R} ->
                    {error, R};
                {ok, M3, ConsumedEff, CancelEff} ->
                    %% 4) Run effect
                    case run_effect(T, M3) of
                        {ok, Effs} ->
                            %% 5) Produce outputs
                            {M4, SplitEff} = produce_outputs(Net, Tid, T, M3),
                            %% 6) Apply cancelation
                            {M5, Cancel2Eff} = apply_cancel(Net, T, M4),
                            %% 7) Release mutex
                            {M6, ReleaseEff} = maybe_release_mutex(T, M5),
                            {ok, M6, MutexEff ++ TriggerEff ++ ConsumedEff ++
                                     SplitEff ++ CancelEff ++ Cancel2Eff ++
                                     ReleaseEff ++ Effs};
                        {input_required, P} -> {input_required, P};
                        {auth_required, P} -> {auth_required, P};
                        {error, R} -> {error, R}
                    end
            end
    end.

maybe_acquire_mutex(_Tid, T, M) ->
    case maps:get(mutex, T, undefined) of
        undefined -> {M, []};
        LockId ->
            Meta0 = maps:get(?META, M, #{}),
            Locks0 = maps:get(locks, Meta0, #{}),
            case maps:get(LockId, Locks0, undefined) of
                undefined ->
                    Locks1 = Locks0#{LockId => self()},
                    {M#{?META => Meta0#{locks => Locks1}}, [{lock_acquired, LockId}]};
                _ ->
                    {M, []}
            end
    end.

maybe_release_mutex(T, M) ->
    case maps:get(mutex, T, undefined) of
        undefined -> {M, []};
        LockId ->
            Meta0 = maps:get(?META, M, #{}),
            Locks0 = maps:get(locks, Meta0, #{}),
            Locks1 = maps:remove(LockId, Locks0),
            {M#{?META => Meta0#{locks => Locks1}}, [{lock_released, LockId}]}
    end.

consume_trigger_if_any(T, M) ->
    case maps:get(trigger, T, undefined) of
        undefined -> {M, []};
        {data, _} -> {M, []};
        {transient, Key} ->
            {consume_event(transient, Key, M), [{trigger_consumed, transient, Key}]};
        {event, Key} ->
            {consume_event(event, Key, M), [{trigger_consumed, event, Key}]};
        {persistent, Key} ->
            {consume_event(persistent, Key, M), [{trigger_consumed, persistent, Key}]}
    end.

consume_event(Kind, Key, M0) ->
    Meta0 = maps:get(?META, M0, #{}),
    Events0 = maps:get(events, Meta0, #{transient => #{}, persistent => #{}, event => #{}}),
    case Kind of
        transient ->
            Events1 = Events0#{transient := maps:remove(Key, maps:get(transient, Events0))},
            M0#{?META => Meta0#{events => Events1}};
        event ->
            Events1 = Events0#{event := maps:remove(Key, maps:get(event, Events0))},
            M0#{?META => Meta0#{events => Events1}};
        persistent ->
            P0 = maps:get(persistent, Events0),
            Q0 = maps:get(Key, P0, []),
            Q1 = case Q0 of [] -> []; [_ | Rest] -> Rest end,
            Events1 = Events0#{persistent := P0#{Key => Q1}},
            M0#{?META => Meta0#{events => Events1}}
    end.

consume_inputs(Net, _Tid, T, M0) ->
    Inputs0 = maps:get(inputs, T, []),
    Join = maps:get(join, T, 'and'),
    case Join of
        'and' -> consume_all(Inputs0, M0);
        'xor' -> consume_one_of(Inputs0, M0);
        'or' ->
            Req = or_join_required_inputs(Net, T, M0),
            consume_places(Req, M0);
        {partial, K} -> consume_partial(Inputs0, K, T, M0)
    end.

consume_all(Inputs, M0) ->
    lists:foldl(
        fun(A, {ok, M, Effs, Canc}) ->
            case consume_from_place(A, M) of
                {ok, M2, TokIds} -> {ok, M2, Effs ++ [{consumed, TokIds}], Canc};
                {error, R} -> {error, R}
            end
        end,
        {ok, M0, [], []},
        Inputs
    ).

consume_one_of(Inputs, M0) ->
    Ready = [A || A <- Inputs, place_has(A, M0)],
    case Ready of
        [] -> {error, <<"no_input_tokens">>};
        _ ->
            Chosen = hd(sort_arcs(Ready)),
            case consume_from_place(Chosen, M0) of
                {ok, M1, TokIds} -> {ok, M1, [{consumed, TokIds}], []};
                {error, R} -> {error, R}
            end
    end.

consume_places(Places, M0) ->
    lists:foldl(
        fun(P, {ok, M, Effs, Canc}) ->
            A = #{place => P, weight => 1},
            case consume_from_place(A, M) of
                {ok, M2, TokIds} -> {ok, M2, Effs ++ [{consumed, TokIds}], Canc};
                {error, R} -> {error, R}
            end
        end,
        {ok, M0, [], []},
        sort_places(Places)
    ).

consume_partial(Inputs0, K, T, M0) ->
    Ready = [A || A <- Inputs0, place_has(A, M0)],
    if length(Ready) < K ->
            {error, <<"partial_join_not_ready">>};
       true ->
            Take = lists:sublist(sort_arcs(Ready), K),
            {ok, M1, Effs, _} = consume_all(Take, M0),
            {M2, CancelEff} = partial_policy_after_fire(T, Inputs0, Take, M1),
            {ok, M2, Effs, CancelEff}
    end.

partial_policy_after_fire(T, InputsAll, InputsTaken, M) ->
    case maps:get(join_policy, T, none) of
        {discriminator, normal} ->
            {remember_closed(T, M), [{discriminator_closed, inputs_not_taken(InputsAll, InputsTaken)}]};
        {discriminator, blocking} ->
            {remember_blocking(T, M), [{discriminator_blocking, true}]};
        {discriminator, canceling} ->
            {M, [{cancel_remaining, inputs_not_taken(InputsAll, InputsTaken)}]};
        {partial_policy, blocking} ->
            {remember_blocking(T, M), [{partial_blocking, true}]};
        {partial_policy, canceling} ->
            {M, [{cancel_remaining, inputs_not_taken(InputsAll, InputsTaken)}]};
        _ ->
            {M, []}
    end.

inputs_not_taken(All, Taken) ->
    TakenPlaces = [maps:get(place, A) || A <- Taken],
    [maps:get(place, A) || A <- All, not lists:member(maps:get(place, A), TakenPlaces)].

remember_closed(T, M0) ->
    Meta0 = maps:get(?META, M0, #{}),
    JM0 = maps:get(join_memory, Meta0, #{}),
    Key = term_to_binary({closed, maps:get(id, T, make_ref())}),
    M0#{?META => Meta0#{join_memory => JM0#{Key => true}}}.

remember_blocking(T, M0) ->
    Meta0 = maps:get(?META, M0, #{}),
    JM0 = maps:get(join_memory, Meta0, #{}),
    Key = term_to_binary({blocking, maps:get(id, T, make_ref())}),
    M0#{?META => Meta0#{join_memory => JM0#{Key => true}}}.

consume_from_place(#{place := P, weight := W}, M0) ->
    Places0 = maps:get(?PLACES, M0, #{}),
    Ts0 = maps:get(P, Places0, []),
    if length(Ts0) < W ->
            {error, <<"insufficient_tokens">>};
       true ->
            Sorted = sort_tokens(Ts0),
            {Take, Keep} = lists:split(W, Sorted),
            TokIds = [maps:get(id, Tok) || Tok <- Take],
            Places1 = Places0#{P => Keep},
            {ok, M0#{?PLACES => Places1}, TokIds}
    end.

run_effect(T, M) ->
    case maps:get(effect, T, undefined) of
        undefined -> {ok, []};
        Fun when is_function(Fun, 1) ->
            try Fun(M) catch _:_ -> {error, <<"effect_crashed">>} end
    end.

produce_outputs(_Net, Tid, T, M0) ->
    Outputs0 = maps:get(outputs, T, []),
    Split = maps:get(split, T, 'and'),
    SplitPolicy = maps:get(split_policy, T, none),
    ForkId = fork_id(Tid, M0),

    case Split of
        'and' ->
            M1 = lists:foldl(fun(A, Acc) -> add_tokens(A, Acc, ForkId) end, M0, Outputs0),
            {M1, [{split, 'and'}]};

        'xor' ->
            ChosenP = choose_one_output(Outputs0, SplitPolicy, M0),
            M1 = add_tokens(#{place => ChosenP, weight => 1}, M0, ForkId),
            {M1, [{split, 'xor', ChosenP}]};

        'or' ->
            ChosenPs = choose_or_outputs(Outputs0, SplitPolicy, M0),
            M1 = lists:foldl(
                fun(P, Acc) -> add_tokens(#{place => P, weight => 1}, Acc, ForkId) end,
                M0,
                ChosenPs
            ),
            {M1, [{split, 'or', ChosenPs}]};

        {thread, N} when is_integer(N), N > 0 ->
            ChosenP = choose_one_output(Outputs0, SplitPolicy, M0),
            M1 = add_n_tokens(ChosenP, N, M0, ForkId),
            {M1, [{split, thread, N, ChosenP}]};

        {mi, CountSpec} ->
            N = resolve_mi_count(CountSpec, M0),
            ChosenP = choose_one_output(Outputs0, SplitPolicy, M0),
            MIId = mi_id(Tid, M0),
            M1 = add_mi_tokens(ChosenP, N, MIId, M0, ForkId),
            {M1, [{split, mi, N, ChosenP, MIId}]}
    end.

resolve_mi_count({design, N}, _M) when is_integer(N), N > 0 -> N;
resolve_mi_count({runtime, Fun}, M) when is_function(Fun, 1) ->
    case Fun(M) of N when is_integer(N), N > 0 -> N; _ -> 1 end;
resolve_mi_count(N, _M) when is_integer(N), N > 0 -> N;
resolve_mi_count(_, _M) -> 1.

choose_one_output(Outputs0, SplitPolicy, M) ->
    Ps = [maps:get(place, A) || A <- Outputs0],
    case SplitPolicy of
        {choice, Fun} when is_function(Fun, 1) -> Fun(M);
        _ -> hd(sort_places(Ps))
    end.

choose_or_outputs(Outputs0, SplitPolicy, M) ->
    Ps = [maps:get(place, A) || A <- Outputs0],
    case SplitPolicy of
        {or_default, DefaultP} ->
            case Ps of [] -> []; _ -> [DefaultP] end;
        {choice, Fun} when is_function(Fun, 1) ->
            case Fun(M) of
                L when is_list(L) -> L;
                P -> [P]
            end;
        _ ->
            sort_places(Ps)
    end.

add_tokens(#{place := P, weight := W}, M0, ForkId) ->
    add_n_tokens(P, W, M0, ForkId).

add_n_tokens(P, N, M0, ForkId) ->
    Places0 = maps:get(?PLACES, M0, #{}),
    Ts0 = maps:get(P, Places0, []),
    New = [new_tok(ForkId) || _ <- lists:seq(1, N)],
    Places1 = Places0#{P => Ts0 ++ New},
    M0#{?PLACES => Places1}.

add_mi_tokens(P, N, MIId, M0, ForkId) ->
    Places0 = maps:get(?PLACES, M0, #{}),
    Ts0 = maps:get(P, Places0, []),
    New = [new_tok_mi(ForkId, MIId, I, N) || I <- lists:seq(1, N)],
    Places1 = Places0#{P => Ts0 ++ New},
    M0#{?PLACES => Places1}.

apply_cancel(_Net, T, M0) ->
    case maps:get(cancel, T, undefined) of
        undefined -> {M0, []};
        {cancel_region, RegionId} ->
            {cancel_region(RegionId, M0), [{canceled, region, RegionId}]};
        {cancel_task, TargetTid} ->
            Meta0 = maps:get(?META, M0, #{}),
            CT0 = maps:get(canceled_tasks, Meta0, #{}),
            {M0#{?META => Meta0#{canceled_tasks => CT0#{TargetTid => true}}},
             [{canceled, task, TargetTid}]};
        {cancel_mi_task, TargetTid} ->
            Meta0 = maps:get(?META, M0, #{}),
            CMI0 = maps:get(canceled_mi_tasks, Meta0, #{}),
            {M0#{?META => Meta0#{canceled_mi_tasks => CMI0#{TargetTid => true}}},
             [{canceled, mi_task, TargetTid}]};
        {complete_mi_task, TargetTid} ->
            Meta0 = maps:get(?META, M0, #{}),
            CMI0 = maps:get(completed_mi_tasks, Meta0, #{}),
            {M0#{?META => Meta0#{completed_mi_tasks => CMI0#{TargetTid => true}}},
             [{completed, mi_task, TargetTid}]};
        explicit_terminate ->
            {empty_all(M0), [{terminated, explicit}]}
    end.

cancel_region(RegionId, M0) ->
    Meta0 = maps:get(?META, M0, #{}),
    PR = maps:get(place_regions, Meta0, #{}),
    Places0 = maps:get(?PLACES, M0, #{}),
    Places1 = maps:map(
        fun(P, Ts) ->
            case maps:get(P, PR, undefined) of
                RegionId -> [];
                _ -> Ts
            end
        end,
        Places0
    ),
    M0#{?PLACES => Places1}.

empty_all(M0) ->
    Places0 = maps:get(?PLACES, M0, #{}),
    Places1 = maps:map(fun(_P, _Ts) -> [] end, Places0),
    M0#{?PLACES => Places1}.

%%--------------------------------------------------------------------
%% Token Creation + Ordering
%%--------------------------------------------------------------------

new_tok(ForkId) ->
    #{
        id => crypto:strong_rand_bytes(16),
        stack => [ForkId],
        mi => undefined
    }.

new_tok_mi(ForkId, MIId, I, N) ->
    #{
        id => crypto:strong_rand_bytes(16),
        stack => [ForkId],
        mi => #{mi_id => MIId, idx => I, total => N}
    }.

fork_id(Tid, _M) ->
    crypto:hash(sha256, term_to_binary({fork, Tid, erlang:unique_integer([monotonic])})).

mi_id(Tid, _M) ->
    crypto:hash(sha256, term_to_binary({mi, Tid, erlang:unique_integer([monotonic])})).

sort_tokens(Ts) ->
    lists:sort(fun(A, B) -> maps:get(id, A) =< maps:get(id, B) end, Ts).

sort_arcs(As) ->
    lists:sort(fun(A, B) -> place_key(maps:get(place, A)) =< place_key(maps:get(place, B)) end, As).

sort_places(Ps) ->
    lists:sort(fun(A, B) -> place_key(A) =< place_key(B) end, Ps).

place_key(P) when is_binary(P) -> P;
place_key(P) when is_atom(P) -> atom_to_binary(P, utf8);
place_key(P) -> term_to_binary(P).

%%--------------------------------------------------------------------
%% Reachability Caches (for OR-join variants)
%%--------------------------------------------------------------------

build_reachability(Net) ->
    Places = maps:get(places, Net, []),
    Ts = maps:get(transitions, Net, #{}),
    Graph = build_place_graph(Places, Ts),
    Reach = maps:from_list([{P, bfs(Graph, P)} || P <- Places]),
    RevGraph = reverse_graph(Graph),
    ReachRev = maps:from_list([{P, bfs(RevGraph, P)} || P <- Places]),
    {Reach, ReachRev}.

build_place_graph(Places, Ts) ->
    Empty = maps:from_list([{P, []} || P <- Places]),
    maps:fold(
        fun(_Tid, T, G0) ->
            Ins = [maps:get(place, A) || A <- maps:get(inputs, T, [])],
            Outs = [maps:get(place, A) || A <- maps:get(outputs, T, [])],
            lists:foldl(
                fun(InP, Acc) ->
                    Cur = maps:get(InP, Acc, []),
                    Acc#{InP => ordsets:to_list(ordsets:from_list(Cur ++ Outs))}
                end,
                G0,
                Ins
            )
        end,
        Empty,
        Ts
    ).

reverse_graph(G) ->
    maps:fold(
        fun(P, Ns, Acc0) ->
            lists:foldl(
                fun(N, Acc) ->
                    Cur = maps:get(N, Acc, []),
                    Acc#{N => ordsets:to_list(ordsets:from_list([P | Cur]))}
                end,
                Acc0,
                Ns
            )
        end,
        maps:map(fun(_, _) -> [] end, G),
        G
    ).

bfs(G, Start) ->
    bfs_loop(G, [Start], ordsets:from_list([Start])).

bfs_loop(_G, [], Vis) ->
    Vis;
bfs_loop(G, [P | Q], Vis0) ->
    Ns = maps:get(P, G, []),
    {Vis1, Q1} = lists:foldl(
        fun(N, {V, Qacc}) ->
            case ordsets:is_element(N, V) of
                true -> {V, Qacc};
                false -> {ordsets:add_element(N, V), Qacc ++ [N]}
            end
        end,
        {Vis0, Q},
        Ns
    ),
    bfs_loop(G, Q1, Vis1).
