%%%-------------------------------------------------------------------
%%% @doc pqc_net - Deterministic YAWL/Petri Workflow Net Executor
%%%
%%% Pure functional executor for workflow nets with PQC blockchain integration.
%%% - Net = places + transitions + Λ total order
%%% - Marking = #{place_id() => non_neg_integer()} token counts
%%%
%%% This module is intentionally pure: no process state, no IO.
%%% The Case kernel (pqc_case) decides WHEN to fire (drive-to-quiescence).
%%%
%%% Transition firing can return:
%%%   {ok, NewMarking, Effects}
%%%   {input_required, PromptBin}
%%%   {auth_required, PromptBin}
%%%   {error, ReasonBin}
%%%
%%% Effects is a list of syscalls for the kernel:
%%%   {emit, NameBin, PayloadTerm}
%%%   {call_tool, ToolAtom, ArgsMap}
%%%   {set_variable, Key, Value}
%%%   {send_message, Target, Msg}
%%%   {invoke_mcp, Method, Params}
%%%   {note, PayloadTerm}
%%%
%%% YAWL Patterns Supported:
%%% - AND-split: One transition produces tokens in all output places
%%% - AND-join: Transition requires tokens in all input places
%%% - XOR-split: choose_out selects one output path
%%% - XOR-join: choose_in selects one input path
%%%
%%% OTP 28 compatible.
%%% @end
%%%-------------------------------------------------------------------

-module(pqc_net).

-include("pqchain.hrl").

-export([
    validate/1,
    enabled/2,
    enabled_with/3,
    fire/3,
    is_enabled/2,
    create_net/1,
    get_enabled_count/2,
    is_final/2,
    simple_approval_net/0,
    yawl_example_net/0
]).

%%--------------------------------------------------------------------
%% Types
%%--------------------------------------------------------------------

-type place_id() :: atom() | binary().
-type tid()      :: atom() | binary().
-type weight()   :: pos_integer().

-type marking()  :: #{place_id() => non_neg_integer()}.

-type join_type()  :: 'and' | 'xor'.
-type split_type() :: 'and' | 'xor'.

-type arc_in()  :: #{place := place_id(), weight := weight()}.
-type arc_out() :: #{place := place_id(), weight := weight()}.

-type effects() :: [effect()].

-type effect() ::
    {emit, atom() | binary(), term()} |
    {call_tool, atom() | binary(), map()} |
    {set_variable, term(), term()} |
    {send_message, binary(), term()} |
    {invoke_mcp, binary(), map()} |
    {note, term()} |
    {consumed, [arc_in()]} |
    {extra, map()}.

-type guard_fun()  :: fun((marking()) -> boolean()).
-type choose_fun() :: fun((marking()) -> place_id()).
-type effect_fun() ::
    fun((marking()) ->
        {ok, effects()} |
        {ok, effects(), map()} |
        {input_required, binary()} |
        {auth_required, binary()} |
        {error, binary()}
    ).

%% Transition spec
-type transition() :: #{
    inputs := [arc_in()],
    outputs := [arc_out()],
    join => join_type(),          % default: and
    split => split_type(),        % default: and
    guard => guard_fun() | undefined,
    choose_in => choose_fun() | undefined,   % xor-join selection
    choose_out => choose_fun() | undefined,  % xor-split selection
    effect => effect_fun() | undefined,
    metadata => map()
}.

%% Net spec
-type net() :: #{
    id => binary(),
    name => binary(),
    places := [place_id()],
    transitions := #{tid() => transition()},
    order := [tid()],  % Λ total order
    metadata => map()
}.

-export_type([
    net/0,
    marking/0,
    effect/0,
    effects/0,
    transition/0,
    arc_in/0,
    arc_out/0,
    place_id/0,
    tid/0,
    join_type/0,
    split_type/0
]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

%% @doc Validate net structure and semantics.
-spec validate(net()) -> ok | {error, binary()}.
validate(Net) ->
    try
        Places = maps:get(places, Net),
        true = is_list(Places),
        Ts = maps:get(transitions, Net),
        true = is_map(Ts),
        Order = maps:get(order, Net),
        true = is_list(Order),

        %% Ensure order covers only known tids
        lists:foreach(
          fun(Tid) ->
              case maps:is_key(Tid, Ts) of
                  true -> ok;
                  false -> throw({bad_order, Tid})
              end
          end,
          Order
        ),

        %% Validate each transition references only known places
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
        throw:{bad_transition, Tid1, ReasonBin} ->
            {error, iolist_to_binary(["bad transition ", io_lib:format("~p", [Tid1]), ": ", ReasonBin])};
        error:Reason ->
            {error, iolist_to_binary(["validate error: ", io_lib:format("~p", [Reason])])}
    end.

%% @doc Returns enabled transition ids in Λ order.
-spec enabled(net(), marking()) -> [tid()].
enabled(Net, Marking) ->
    Order = maps:get(order, Net, []),
    Ts = maps:get(transitions, Net, #{}),
    [Tid || Tid <- Order,
            is_enabled_internal(maps:get(Tid, Ts), Marking)].

%% @doc Filter enabled transitions by a predicate on Tid/Transition.
-spec enabled_with(net(), marking(), fun((tid(), transition()) -> boolean())) -> [tid()].
enabled_with(Net, Marking, Pred) ->
    Order = maps:get(order, Net, []),
    Ts = maps:get(transitions, Net, #{}),
    [Tid || Tid <- Order,
            begin
                T = maps:get(Tid, Ts),
                is_enabled_internal(T, Marking) andalso Pred(Tid, T)
            end].

%% @doc Fire a specific transition (if enabled).
%% Deterministic selection for xor join/split.
-spec fire(net(), tid(), marking()) ->
    {ok, marking(), effects()} |
    {input_required, binary()} |
    {auth_required, binary()} |
    {error, binary()}.
fire(Net, Tid, Marking0) ->
    Ts = maps:get(transitions, Net, #{}),
    case maps:find(Tid, Ts) of
        error ->
            {error, <<"transition_not_found">>};
        {ok, T} ->
            case is_enabled_internal(T, Marking0) of
                false -> {error, <<"transition_not_enabled">>};
                true  -> fire_enabled(Tid, T, Marking0)
            end
    end.

%% @doc Check if a specific transition is enabled.
-spec is_enabled(transition(), marking()) -> boolean().
is_enabled(Transition, Marking) ->
    is_enabled_internal(Transition, Marking).

%% @doc Create net from specification with defaults.
-spec create_net(map()) -> {ok, net()} | {error, term()}.
create_net(Spec) ->
    try
        Net = #{
            id => maps:get(id, Spec, generate_net_id()),
            name => maps:get(name, Spec, <<"unnamed">>),
            places => maps:get(places, Spec, []),
            transitions => normalize_transitions(maps:get(transitions, Spec, #{})),
            order => maps:get(order, Spec, []),
            metadata => maps:get(metadata, Spec, #{})
        },
        case validate(Net) of
            ok -> {ok, Net};
            {error, _} = Error -> Error
        end
    catch
        error:Reason:Stack ->
            {error, {create_failed, Reason, Stack}}
    end.

%% @doc Get count of enabled transitions.
-spec get_enabled_count(net(), marking()) -> non_neg_integer().
get_enabled_count(Net, Marking) ->
    length(enabled(Net, Marking)).

%% @doc Check if marking is final (no enabled transitions).
-spec is_final(net(), marking()) -> boolean().
is_final(Net, Marking) ->
    get_enabled_count(Net, Marking) =:= 0.

%% @doc Simple approval workflow (XOR-split pattern).
-spec simple_approval_net() -> net().
simple_approval_net() ->
    #{
        id => <<"simple_approval">>,
        name => <<"Simple Approval Workflow">>,
        places => [start, pending, approved_place, rejected_place, done],
        transitions => #{
            submit => #{
                inputs => [#{place => start, weight => 1}],
                outputs => [#{place => pending, weight => 1}],
                join => 'and',
                split => 'and',
                guard => undefined,
                effect => fun(_M) -> {ok, [{note, <<"submitted">>}]} end
            },
            approve => #{
                inputs => [#{place => pending, weight => 1}],
                outputs => [#{place => approved_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) -> maps:get(decision, M, undefined) =:= approve end,
                effect => fun(_M) ->
                    {ok, [{emit, <<"approved">>, #{ts => erlang:system_time(millisecond)}}]}
                end
            },
            reject => #{
                inputs => [#{place => pending, weight => 1}],
                outputs => [#{place => rejected_place, weight => 1}],
                join => 'and',
                split => 'and',
                guard => fun(M) -> maps:get(decision, M, undefined) =:= reject end,
                effect => fun(_M) ->
                    {ok, [{emit, <<"rejected">>, #{ts => erlang:system_time(millisecond)}}]}
                end
            },
            finalize_approved => #{
                inputs => [#{place => approved_place, weight => 1}],
                outputs => [#{place => done, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, []} end
            },
            finalize_rejected => #{
                inputs => [#{place => rejected_place, weight => 1}],
                outputs => [#{place => done, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, []} end
            }
        },
        order => [submit, approve, reject, finalize_approved, finalize_rejected],
        metadata => #{version => 1, description => <<"Simple approval with XOR-split">>}
    }.

%% @doc YAWL-style net with AND-split/join and XOR-split/join.
-spec yawl_example_net() -> net().
yawl_example_net() ->
    #{
        id => <<"yawl_example">>,
        name => <<"YAWL Example: Parallel Tasks with Conditional Completion">>,
        places => [start, task_a, task_b, join_point, condition_check,
                   success_path, failure_path, done],
        transitions => #{
            %% AND-split: start enables both task_a and task_b
            begin_parallel => #{
                inputs => [#{place => start, weight => 1}],
                outputs => [#{place => task_a, weight => 1},
                           #{place => task_b, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{note, <<"parallel_started">>}]} end
            },
            %% Complete task A
            complete_a => #{
                inputs => [#{place => task_a, weight => 1}],
                outputs => [#{place => join_point, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{emit, <<"task_a_done">>, #{}}]} end
            },
            %% Complete task B
            complete_b => #{
                inputs => [#{place => task_b, weight => 1}],
                outputs => [#{place => join_point, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{emit, <<"task_b_done">>, #{}}]} end
            },
            %% AND-join: both tasks must complete (need 2 tokens)
            synchronize => #{
                inputs => [#{place => join_point, weight => 2}],
                outputs => [#{place => condition_check, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{note, <<"synchronized">>}]} end
            },
            %% XOR-split: condition determines path
            check_success => #{
                inputs => [#{place => condition_check, weight => 1}],
                outputs => [#{place => success_path, weight => 1},
                           #{place => failure_path, weight => 1}],
                join => 'and',
                split => 'xor',
                choose_out => fun(M) ->
                    case maps:get(success, M, true) of
                        true -> success_path;
                        false -> failure_path
                    end
                end,
                effect => fun(_M) -> {ok, []} end
            },
            %% XOR-join: either path leads to done
            finish_success => #{
                inputs => [#{place => success_path, weight => 1}],
                outputs => [#{place => done, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{emit, <<"success">>, #{}}]} end
            },
            finish_failure => #{
                inputs => [#{place => failure_path, weight => 1}],
                outputs => [#{place => done, weight => 1}],
                join => 'and',
                split => 'and',
                effect => fun(_M) -> {ok, [{emit, <<"failure">>, #{}}]} end
            }
        },
        order => [begin_parallel, complete_a, complete_b, synchronize,
                  check_success, finish_success, finish_failure],
        metadata => #{
            version => 1,
            description => <<"YAWL pattern: AND-split -> parallel tasks -> AND-join -> XOR-split">>
        }
    }.

%%--------------------------------------------------------------------
%% Internal validation
%%--------------------------------------------------------------------

-spec validate_transition(tid(), transition(), map()) -> ok.
validate_transition(Tid, T, PlaceSet) ->
    Inputs = maps:get(inputs, T, []),
    Outputs = maps:get(outputs, T, []),

    (is_list(Inputs) andalso is_list(Outputs)) orelse
        throw({bad_transition, Tid, <<"inputs/outputs must be lists">>}),

    lists:foreach(fun(A) -> validate_arc_in(Tid, A, PlaceSet) end, Inputs),
    lists:foreach(fun(A) -> validate_arc_out(Tid, A, PlaceSet) end, Outputs),

    Join = maps:get(join, T, 'and'),
    Split = maps:get(split, T, 'and'),
    (Join =:= 'and' orelse Join =:= 'xor') orelse
        throw({bad_transition, Tid, <<"join must be and|xor">>}),
    (Split =:= 'and' orelse Split =:= 'xor') orelse
        throw({bad_transition, Tid, <<"split must be and|xor">>}),

    Guard = maps:get(guard, T, undefined),
    (Guard =:= undefined orelse is_function(Guard, 1)) orelse
        throw({bad_transition, Tid, <<"guard must be fun/1 or undefined">>}),

    ChooseIn = maps:get(choose_in, T, undefined),
    (ChooseIn =:= undefined orelse is_function(ChooseIn, 1)) orelse
        throw({bad_transition, Tid, <<"choose_in must be fun/1 or undefined">>}),

    ChooseOut = maps:get(choose_out, T, undefined),
    (ChooseOut =:= undefined orelse is_function(ChooseOut, 1)) orelse
        throw({bad_transition, Tid, <<"choose_out must be fun/1 or undefined">>}),

    Eff = maps:get(effect, T, undefined),
    (Eff =:= undefined orelse is_function(Eff, 1)) orelse
        throw({bad_transition, Tid, <<"effect must be fun/1 or undefined">>}),

    ok.

validate_arc_in(Tid, A, PlaceSet) ->
    case {maps:is_key(place, A), maps:is_key(weight, A)} of
        {true, true} -> ok;
        _ -> throw({bad_transition, Tid, <<"input arc must contain place + weight">>})
    end,
    P = maps:get(place, A),
    W = maps:get(weight, A),
    maps:is_key(P, PlaceSet) orelse throw({bad_transition, Tid, <<"input arc references unknown place">>}),
    (is_integer(W) andalso W > 0) orelse throw({bad_transition, Tid, <<"input arc weight must be pos_integer">>}).

validate_arc_out(Tid, A, PlaceSet) ->
    case {maps:is_key(place, A), maps:is_key(weight, A)} of
        {true, true} -> ok;
        _ -> throw({bad_transition, Tid, <<"output arc must contain place + weight">>})
    end,
    P = maps:get(place, A),
    W = maps:get(weight, A),
    maps:is_key(P, PlaceSet) orelse throw({bad_transition, Tid, <<"output arc references unknown place">>}),
    (is_integer(W) andalso W > 0) orelse throw({bad_transition, Tid, <<"output arc weight must be pos_integer">>}).

%%--------------------------------------------------------------------
%% Enabled check
%%--------------------------------------------------------------------

-spec is_enabled_internal(transition(), marking()) -> boolean().
is_enabled_internal(T, Marking) ->
    GuardOk =
        case maps:get(guard, T, undefined) of
            undefined -> true;
            GuardFun -> safe_bool(GuardFun(Marking))
        end,
    GuardOk andalso inputs_satisfied(T, Marking).

-spec inputs_satisfied(transition(), marking()) -> boolean().
inputs_satisfied(T, Marking) ->
    Inputs = maps:get(inputs, T, []),
    Join = maps:get(join, T, 'and'),
    case Join of
        'and' ->
            lists:all(fun(A) -> has_tokens(A, Marking) end, Inputs);
        'xor' ->
            lists:any(fun(A) -> has_tokens(A, Marking) end, Inputs)
    end.

-spec has_tokens(arc_in(), marking()) -> boolean().
has_tokens(#{place := P, weight := W}, Marking) ->
    token_get(P, Marking) >= W.

safe_bool(true) -> true;
safe_bool(false) -> false;
safe_bool(_) -> false.

%%--------------------------------------------------------------------
%% Fire
%%--------------------------------------------------------------------

-spec fire_enabled(tid(), transition(), marking()) ->
    {ok, marking(), effects()} |
    {input_required, binary()} |
    {auth_required, binary()} |
    {error, binary()}.
fire_enabled(_Tid, T, Marking0) ->
    %% 1) Choose which inputs to consume (for xor join)
    {ConsumeIns, Marking1} = consume_inputs(T, Marking0),

    %% 2) Run effect (may request input/auth or error)
    case run_effect(T, Marking1) of
        {ok, Effects} ->
            %% 3) Produce outputs (for xor split choose single output place)
            Marking2 = produce_outputs(T, Marking1),
            {ok, Marking2, [{consumed, ConsumeIns} | Effects]};
        {ok, Effects, Extra} when is_map(Extra) ->
            Marking2 = produce_outputs(T, Marking1),
            {ok, Marking2, [{consumed, ConsumeIns}, {extra, Extra} | Effects]};
        {input_required, Prompt} ->
            %% Rollback: don't consume tokens
            {input_required, Prompt};
        {auth_required, Prompt} ->
            {auth_required, Prompt};
        {error, Reason} ->
            {error, Reason}
    end.

%% Consume tokens according to join type; deterministic selection for xor.
-spec consume_inputs(transition(), marking()) -> {[arc_in()], marking()}.
consume_inputs(T, Marking0) ->
    Inputs0 = maps:get(inputs, T, []),
    Join = maps:get(join, T, 'and'),
    Inputs = sort_arcs_in(Inputs0),

    case Join of
        'and' ->
            Marking1 = lists:foldl(fun consume_one/2, Marking0, Inputs),
            {Inputs, Marking1};
        'xor' ->
            Chosen = choose_input_arc(T, Inputs, Marking0),
            Marking1 = consume_one(Chosen, Marking0),
            {[Chosen], Marking1}
    end.

-spec consume_one(arc_in(), marking()) -> marking().
consume_one(#{place := P, weight := W}, Marking0) ->
    Cur = token_get(P, Marking0),
    New = Cur - W,
    token_put(P, New, Marking0).

-spec choose_input_arc(transition(), [arc_in()], marking()) -> arc_in().
choose_input_arc(T, Inputs, Marking) ->
    case maps:get(choose_in, T, undefined) of
        undefined ->
            %% First enabled in stable order
            case [A || A <- Inputs, has_tokens(A, Marking)] of
                [H | _] -> H;
                [] -> hd(Inputs)  %% should not happen if enabled
            end;
        ChooseFun ->
            P = ChooseFun(Marking),
            %% Find arc for that place; otherwise fallback to first enabled
            case lists:filter(fun(#{place := P0}) -> P0 =:= P end, Inputs) of
                [Arc | _] when has_tokens(Arc, Marking) -> Arc;
                _ ->
                    case [A || A <- Inputs, has_tokens(A, Marking)] of
                        [H | _] -> H;
                        [] -> hd(Inputs)
                    end
            end
    end.

%% Produce tokens according to split type; deterministic selection for xor.
-spec produce_outputs(transition(), marking()) -> marking().
produce_outputs(T, Marking0) ->
    Outputs0 = maps:get(outputs, T, []),
    Split = maps:get(split, T, 'and'),
    Outputs = sort_arcs_out(Outputs0),

    case Split of
        'and' ->
            lists:foldl(fun produce_one/2, Marking0, Outputs);
        'xor' ->
            case Outputs of
                [] -> Marking0;
                _ ->
                    Chosen = choose_output_arc(T, Outputs, Marking0),
                    produce_one(Chosen, Marking0)
            end
    end.

-spec produce_one(arc_out(), marking()) -> marking().
produce_one(#{place := P, weight := W}, Marking0) ->
    Cur = token_get(P, Marking0),
    token_put(P, Cur + W, Marking0).

-spec choose_output_arc(transition(), [arc_out()], marking()) -> arc_out().
choose_output_arc(T, Outputs, Marking) ->
    case maps:get(choose_out, T, undefined) of
        undefined ->
            %% First output in stable order
            hd(Outputs);
        ChooseFun ->
            P = ChooseFun(Marking),
            case lists:filter(fun(#{place := P0}) -> P0 =:= P end, Outputs) of
                [Arc | _] -> Arc;
                _ -> hd(Outputs)
            end
    end.

%% Run transition effect (if any) against the post-consumption marking.
-spec run_effect(transition(), marking()) ->
    {ok, effects()} |
    {ok, effects(), map()} |
    {input_required, binary()} |
    {auth_required, binary()} |
    {error, binary()}.
run_effect(T, Marking) ->
    case maps:get(effect, T, undefined) of
        undefined ->
            {ok, []};
        EffFun ->
            try
                case EffFun(Marking) of
                    {ok, E} when is_list(E) -> {ok, E};
                    {ok, E, Extra} when is_list(E), is_map(Extra) -> {ok, E, Extra};
                    {input_required, Bin} when is_binary(Bin) -> {input_required, Bin};
                    {auth_required, Bin} when is_binary(Bin) -> {auth_required, Bin};
                    {error, Bin} when is_binary(Bin) -> {error, Bin};
                    Other ->
                        {error, iolist_to_binary(["bad effect return: ", io_lib:format("~p", [Other])])}
                end
            catch
                _:Reason ->
                    {error, iolist_to_binary(["effect error: ", io_lib:format("~p", [Reason])])}
            end
    end.

%%--------------------------------------------------------------------
%% Marking helpers
%%--------------------------------------------------------------------

-spec token_get(place_id(), marking()) -> non_neg_integer().
token_get(P, Marking) ->
    maps:get(P, Marking, 0).

-spec token_put(place_id(), non_neg_integer(), marking()) -> marking().
token_put(P, V, Marking) when is_integer(V), V >= 0 ->
    Marking#{P => V}.

%%--------------------------------------------------------------------
%% Normalization helpers
%%--------------------------------------------------------------------

-spec normalize_transitions(map()) -> map().
normalize_transitions(Transitions) when is_map(Transitions) ->
    maps:map(fun(_Tid, T) -> normalize_transition(T) end, Transitions);
normalize_transitions(_) ->
    #{}.

-spec normalize_transition(map()) -> transition().
normalize_transition(T) when is_map(T) ->
    #{
        inputs => maps:get(inputs, T, []),
        outputs => maps:get(outputs, T, []),
        join => maps:get(join, T, 'and'),
        split => maps:get(split, T, 'and'),
        guard => maps:get(guard, T, undefined),
        choose_in => maps:get(choose_in, T, undefined),
        choose_out => maps:get(choose_out, T, undefined),
        effect => maps:get(effect, T, undefined),
        metadata => maps:get(metadata, T, #{})
    };
normalize_transition(_) ->
    #{inputs => [], outputs => [], join => 'and', split => 'and'}.

%%--------------------------------------------------------------------
%% Stable ordering helpers (Λ-friendly determinism)
%%--------------------------------------------------------------------

sort_arcs_in(Arcs) ->
    lists:sort(fun arc_less/2, Arcs).

sort_arcs_out(Arcs) ->
    lists:sort(fun arc_less/2, Arcs).

arc_less(A, B) ->
    Pa = maps:get(place, A),
    Pb = maps:get(place, B),
    Wa = maps:get(weight, A),
    Wb = maps:get(weight, B),
    case compare_key(Pa, Pb) of
        lt -> true;
        gt -> false;
        eq -> Wa =< Wb
    end.

compare_key(A, B) when is_binary(A), is_binary(B) ->
    if A < B -> lt; A > B -> gt; true -> eq end;
compare_key(A, B) when is_atom(A), is_atom(B) ->
    compare_key(atom_to_binary(A, utf8), atom_to_binary(B, utf8));
compare_key(A, B) ->
    if A < B -> lt; A > B -> gt; true -> eq end.

%%--------------------------------------------------------------------
%% ID generation
%%--------------------------------------------------------------------

-spec generate_net_id() -> binary().
generate_net_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(10),
    Base = <<Timestamp:48, Random/binary>>,
    base64:encode(Base).
