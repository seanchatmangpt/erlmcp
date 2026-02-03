%%%-------------------------------------------------------------------
%%% @doc SwarmFlow Net Registry
%%%
%%% Manages workflow net definitions (Petri net / YAWL specifications).
%%% Provides ETS-based storage with version management, structural
%%% validation, and net compilation for runtime optimization.
%%%
%%% Architecture:
%%% - Two ETS tables: nets (versioned definitions), active_versions
%%% - Structural validation: connectivity, deadlock-freedom, soundness
%%% - Compiled nets include precomputed adjacency and reachability data
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_net_registry).

-behaviour(gen_server).

-include("swarmflow.hrl").

%% API
-export([
    start_link/0,
    register_net/1,
    get_net/1,
    get_net/2,
    get_active_version/1,
    set_active_version/2,
    list_nets/0,
    list_versions/1,
    validate_net/1,
    delete_net/1,
    delete_version/2,
    compile_net/1
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

-define(SERVER, ?MODULE).
-define(NETS_TABLE, swf_nets).
-define(ACTIVE_TABLE, swf_active_versions).

%% Compiled net with precomputed data structures for fast runtime execution
-record(compiled_net, {
    net :: #swf_net{},
    %% Adjacency lists for fast traversal
    place_to_out_arcs :: #{binary() => [#swf_arc{}]},
    place_to_in_arcs :: #{binary() => [#swf_arc{}]},
    trans_to_out_arcs :: #{binary() => [#swf_arc{}]},
    trans_to_in_arcs :: #{binary() => [#swf_arc{}]},
    %% Precomputed input/output places for each transition
    trans_inputs :: #{binary() => [{binary(), pos_integer(), atom()}]},  % {PlaceId, Weight, ArcKind}
    trans_outputs :: #{binary() => [{binary(), pos_integer(), atom()}]},
    %% Structural properties
    is_sound :: boolean(),
    is_free_choice :: boolean(),
    reachable_from_initial :: sets:set(binary()),
    can_reach_final :: sets:set(binary()),
    compiled_at :: integer()
}).

-record(state, {
    nets_table :: ets:tid(),
    active_table :: ets:tid()
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the net registry server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Register a new workflow net definition
%% Validates and compiles the net before storage
-spec register_net(#swf_net{}) -> {ok, #swf_net{}} | {error, term()}.
register_net(#swf_net{} = Net) ->
    gen_server:call(?SERVER, {register_net, Net}).

%% @doc Get net by ID (returns active version)
-spec get_net(binary()) -> {ok, #swf_net{}} | {error, not_found}.
get_net(NetId) ->
    gen_server:call(?SERVER, {get_net, NetId}).

%% @doc Get net by ID and specific version
-spec get_net(binary(), binary()) -> {ok, #swf_net{}} | {error, not_found}.
get_net(NetId, Version) ->
    gen_server:call(?SERVER, {get_net, NetId, Version}).

%% @doc Get currently active version of a net
-spec get_active_version(binary()) -> {ok, binary()} | {error, not_found}.
get_active_version(NetId) ->
    gen_server:call(?SERVER, {get_active_version, NetId}).

%% @doc Set active version for a net
-spec set_active_version(binary(), binary()) -> ok | {error, term()}.
set_active_version(NetId, Version) ->
    gen_server:call(?SERVER, {set_active_version, NetId, Version}).

%% @doc List all registered net IDs
-spec list_nets() -> [binary()].
list_nets() ->
    gen_server:call(?SERVER, list_nets).

%% @doc List all versions of a specific net
-spec list_versions(binary()) -> {ok, [binary()]} | {error, not_found}.
list_versions(NetId) ->
    gen_server:call(?SERVER, {list_versions, NetId}).

%% @doc Validate net structure
%% Returns ok or detailed validation errors
-spec validate_net(#swf_net{}) -> ok | {error, [term()]}.
validate_net(#swf_net{} = Net) ->
    do_validate_net(Net).

%% @doc Delete all versions of a net
-spec delete_net(binary()) -> ok | {error, not_found}.
delete_net(NetId) ->
    gen_server:call(?SERVER, {delete_net, NetId}).

%% @doc Delete specific version of a net
-spec delete_version(binary(), binary()) -> ok | {error, term()}.
delete_version(NetId, Version) ->
    gen_server:call(?SERVER, {delete_version, NetId, Version}).

%% @doc Compile a net for optimized runtime execution
-spec compile_net(#swf_net{}) -> {ok, #compiled_net{}} | {error, term()}.
compile_net(#swf_net{} = Net) ->
    do_compile_net(Net).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    NetsTable = ets:new(?NETS_TABLE, [
        set,
        protected,
        named_table,
        {keypos, 1},  % Key is {NetId, Version}
        {read_concurrency, true}
    ]),
    ActiveTable = ets:new(?ACTIVE_TABLE, [
        set,
        protected,
        named_table,
        {keypos, 1},  % Key is NetId
        {read_concurrency, true}
    ]),
    {ok, #state{nets_table = NetsTable, active_table = ActiveTable}}.

handle_call({register_net, Net}, _From, State) ->
    Result = do_register_net(Net, State),
    {reply, Result, State};

handle_call({get_net, NetId}, _From, State) ->
    Result = do_get_net(NetId, State),
    {reply, Result, State};

handle_call({get_net, NetId, Version}, _From, State) ->
    Result = do_get_net(NetId, Version, State),
    {reply, Result, State};

handle_call({get_active_version, NetId}, _From, State) ->
    Result = do_get_active_version(NetId, State),
    {reply, Result, State};

handle_call({set_active_version, NetId, Version}, _From, State) ->
    Result = do_set_active_version(NetId, Version, State),
    {reply, Result, State};

handle_call(list_nets, _From, State) ->
    Result = do_list_nets(State),
    {reply, Result, State};

handle_call({list_versions, NetId}, _From, State) ->
    Result = do_list_versions(NetId, State),
    {reply, Result, State};

handle_call({delete_net, NetId}, _From, State) ->
    Result = do_delete_net(NetId, State),
    {reply, Result, State};

handle_call({delete_version, NetId, Version}, _From, State) ->
    Result = do_delete_version(NetId, Version, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - CRUD Operations
%%%===================================================================

do_register_net(#swf_net{id = NetId, version = Version} = Net, State) ->
    case do_validate_net(Net) of
        ok ->
            case do_compile_net(Net) of
                {ok, CompiledNet} ->
                    Key = {NetId, Version},
                    ets:insert(State#state.nets_table, {Key, CompiledNet}),
                    %% Set as active if first version
                    case ets:lookup(State#state.active_table, NetId) of
                        [] ->
                            ets:insert(State#state.active_table, {NetId, Version});
                        _ ->
                            ok
                    end,
                    {ok, Net};
                {error, _} = CompileError ->
                    CompileError
            end;
        {error, _} = ValidationError ->
            ValidationError
    end.

do_get_net(NetId, State) ->
    case ets:lookup(State#state.active_table, NetId) of
        [{NetId, ActiveVersion}] ->
            do_get_net(NetId, ActiveVersion, State);
        [] ->
            {error, not_found}
    end.

do_get_net(NetId, Version, State) ->
    Key = {NetId, Version},
    case ets:lookup(State#state.nets_table, Key) of
        [{Key, #compiled_net{net = Net}}] ->
            {ok, Net};
        [] ->
            {error, not_found}
    end.

do_get_active_version(NetId, State) ->
    case ets:lookup(State#state.active_table, NetId) of
        [{NetId, Version}] ->
            {ok, Version};
        [] ->
            {error, not_found}
    end.

do_set_active_version(NetId, Version, State) ->
    Key = {NetId, Version},
    case ets:lookup(State#state.nets_table, Key) of
        [{Key, _}] ->
            ets:insert(State#state.active_table, {NetId, Version}),
            ok;
        [] ->
            {error, version_not_found}
    end.

do_list_nets(State) ->
    Nets = ets:foldl(
        fun({NetId, _Version}, Acc) ->
            case lists:member(NetId, Acc) of
                true -> Acc;
                false -> [NetId | Acc]
            end
        end,
        [],
        State#state.active_table
    ),
    lists:sort(Nets).

do_list_versions(NetId, State) ->
    Versions = ets:foldl(
        fun({{Id, Version}, _}, Acc) when Id =:= NetId ->
                [Version | Acc];
           (_, Acc) ->
                Acc
        end,
        [],
        State#state.nets_table
    ),
    case Versions of
        [] -> {error, not_found};
        _ -> {ok, lists:sort(Versions)}
    end.

do_delete_net(NetId, State) ->
    case ets:lookup(State#state.active_table, NetId) of
        [] ->
            {error, not_found};
        _ ->
            %% Delete all versions
            ets:foldl(
                fun({{Id, _Version} = Key, _}, _) when Id =:= NetId ->
                        ets:delete(State#state.nets_table, Key);
                   (_, Acc) ->
                        Acc
                end,
                ok,
                State#state.nets_table
            ),
            ets:delete(State#state.active_table, NetId),
            ok
    end.

do_delete_version(NetId, Version, State) ->
    Key = {NetId, Version},
    case ets:lookup(State#state.nets_table, Key) of
        [] ->
            {error, not_found};
        _ ->
            %% Check if this is the active version
            case ets:lookup(State#state.active_table, NetId) of
                [{NetId, Version}] ->
                    %% Cannot delete active version unless it's the last one
                    case do_list_versions(NetId, State) of
                        {ok, [Version]} ->
                            %% Last version, delete everything
                            ets:delete(State#state.nets_table, Key),
                            ets:delete(State#state.active_table, NetId),
                            ok;
                        {ok, Versions} ->
                            %% Switch to another version first
                            OtherVersions = lists:delete(Version, Versions),
                            NewActive = lists:last(OtherVersions),
                            ets:insert(State#state.active_table, {NetId, NewActive}),
                            ets:delete(State#state.nets_table, Key),
                            ok
                    end;
                _ ->
                    ets:delete(State#state.nets_table, Key),
                    ok
            end
    end.

%%%===================================================================
%%% Internal functions - Validation
%%%===================================================================

do_validate_net(#swf_net{} = Net) ->
    Validators = [
        fun validate_basic_structure/1,
        fun validate_arcs/1,
        fun validate_initial_marking/1,
        fun validate_final_places/1,
        fun validate_connectivity/1,
        fun validate_deadlock_freedom/1
    ],
    run_validators(Validators, Net, []).

run_validators([], _Net, []) ->
    ok;
run_validators([], _Net, Errors) ->
    {error, lists:reverse(Errors)};
run_validators([Validator | Rest], Net, Errors) ->
    case Validator(Net) of
        ok ->
            run_validators(Rest, Net, Errors);
        {error, Error} ->
            run_validators(Rest, Net, [Error | Errors])
    end.

validate_basic_structure(#swf_net{id = Id, name = Name, version = Version,
                                   places = Places, transitions = Transitions}) ->
    Errors = [],
    Errors1 = case is_binary(Id) andalso byte_size(Id) > 0 of
        true -> Errors;
        false -> [{invalid_id, Id} | Errors]
    end,
    Errors2 = case is_binary(Name) andalso byte_size(Name) > 0 of
        true -> Errors1;
        false -> [{invalid_name, Name} | Errors1]
    end,
    Errors3 = case is_binary(Version) andalso byte_size(Version) > 0 of
        true -> Errors2;
        false -> [{invalid_version, Version} | Errors2]
    end,
    Errors4 = case is_map(Places) andalso map_size(Places) > 0 of
        true -> Errors3;
        false -> [{no_places, Places} | Errors3]
    end,
    Errors5 = case is_map(Transitions) andalso map_size(Transitions) > 0 of
        true -> Errors4;
        false -> [{no_transitions, Transitions} | Errors4]
    end,
    case Errors5 of
        [] -> ok;
        _ -> {error, {basic_structure, Errors5}}
    end.

validate_arcs(#swf_net{places = Places, transitions = Transitions, arcs = Arcs}) ->
    PlaceIds = maps:keys(Places),
    TransIds = maps:keys(Transitions),
    AllIds = PlaceIds ++ TransIds,

    Errors = lists:foldl(
        fun(#swf_arc{id = ArcId, source = Source, target = Target, kind = Kind}, Acc) ->
            Acc1 = case lists:member(Source, AllIds) of
                true -> Acc;
                false -> [{arc_invalid_source, ArcId, Source} | Acc]
            end,
            Acc2 = case lists:member(Target, AllIds) of
                true -> Acc1;
                false -> [{arc_invalid_target, ArcId, Target} | Acc1]
            end,
            %% Arcs must connect place<->transition, not place<->place or trans<->trans
            SrcIsPlace = lists:member(Source, PlaceIds),
            TgtIsPlace = lists:member(Target, PlaceIds),
            Acc3 = case SrcIsPlace =/= TgtIsPlace of
                true -> Acc2;
                false -> [{arc_invalid_endpoints, ArcId, Source, Target} | Acc2]
            end,
            %% Validate arc kind
            Acc4 = case lists:member(Kind, [normal, inhibitor, reset, read]) of
                true -> Acc3;
                false -> [{arc_invalid_kind, ArcId, Kind} | Acc3]
            end,
            Acc4
        end,
        [],
        Arcs
    ),

    case Errors of
        [] -> ok;
        _ -> {error, {invalid_arcs, lists:reverse(Errors)}}
    end.

validate_initial_marking(#swf_net{places = Places, initial_marking = InitialMarking}) ->
    PlaceIds = maps:keys(Places),

    %% Check that all places in initial marking exist
    InvalidPlaces = maps:fold(
        fun(PlaceId, Tokens, Acc) ->
            case lists:member(PlaceId, PlaceIds) of
                true ->
                    case is_integer(Tokens) andalso Tokens >= 0 of
                        true -> Acc;
                        false -> [{invalid_token_count, PlaceId, Tokens} | Acc]
                    end;
                false ->
                    [{unknown_place_in_marking, PlaceId} | Acc]
            end
        end,
        [],
        InitialMarking
    ),

    %% Check that at least one place has tokens
    HasTokens = maps:fold(
        fun(_, Tokens, Acc) -> Acc orelse Tokens > 0 end,
        false,
        InitialMarking
    ),

    Errors = case HasTokens of
        true -> InvalidPlaces;
        false -> [{empty_initial_marking} | InvalidPlaces]
    end,

    case Errors of
        [] -> ok;
        _ -> {error, {invalid_initial_marking, lists:reverse(Errors)}}
    end.

validate_final_places(#swf_net{places = Places, final_places = FinalPlaces}) ->
    PlaceIds = maps:keys(Places),

    case FinalPlaces of
        [] ->
            {error, {no_final_places}};
        _ ->
            Invalid = [P || P <- FinalPlaces, not lists:member(P, PlaceIds)],
            case Invalid of
                [] -> ok;
                _ -> {error, {unknown_final_places, Invalid}}
            end
    end.

validate_connectivity(#swf_net{} = Net) ->
    %% Build adjacency map
    {ForwardAdj, _BackwardAdj} = build_adjacency_maps(Net),

    %% Find nodes with initial tokens
    InitialNodes = maps:fold(
        fun(PlaceId, Tokens, Acc) when Tokens > 0 ->
                [PlaceId | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        Net#swf_net.initial_marking
    ),

    %% BFS from initial nodes
    Reachable = bfs_reachable(InitialNodes, ForwardAdj),

    %% Check all transitions are reachable
    AllTransitions = maps:keys(Net#swf_net.transitions),
    UnreachableTransitions = [T || T <- AllTransitions, not sets:is_element(T, Reachable)],

    case UnreachableTransitions of
        [] -> ok;
        _ -> {error, {unreachable_transitions, UnreachableTransitions}}
    end.

validate_deadlock_freedom(#swf_net{} = Net) ->
    %% Basic structural deadlock check using siphon analysis
    %% A siphon is a set of places where no transition can add tokens
    %% if all places in the set are empty
    %%
    %% For simplicity, we check if every non-final place has at least
    %% one outgoing arc, and final places are reachable

    #swf_net{places = Places, arcs = Arcs, final_places = FinalPlaces} = Net,

    %% Find places with no outgoing arcs (other than final places)
    PlacesWithOutArcs = lists:foldl(
        fun(#swf_arc{source = Src}, Acc) ->
            case maps:is_key(Src, Places) of
                true -> sets:add_element(Src, Acc);
                false -> Acc
            end
        end,
        sets:new(),
        Arcs
    ),

    %% Non-final places without outgoing arcs indicate potential deadlock
    AllPlaceIds = maps:keys(Places),
    FinalSet = sets:from_list(FinalPlaces),
    DeadEndPlaces = [P || P <- AllPlaceIds,
                          not sets:is_element(P, PlacesWithOutArcs),
                          not sets:is_element(P, FinalSet)],

    case DeadEndPlaces of
        [] ->
            %% Additionally check that final places are reachable
            {ForwardAdj, _} = build_adjacency_maps(Net),
            InitialNodes = maps:fold(
                fun(PlaceId, Tokens, Acc) when Tokens > 0 -> [PlaceId | Acc];
                   (_, _, Acc) -> Acc
                end,
                [],
                Net#swf_net.initial_marking
            ),
            Reachable = bfs_reachable(InitialNodes, ForwardAdj),
            UnreachableFinal = [F || F <- FinalPlaces, not sets:is_element(F, Reachable)],
            case UnreachableFinal of
                [] -> ok;
                _ -> {error, {unreachable_final_places, UnreachableFinal}}
            end;
        _ ->
            {error, {potential_deadlock_places, DeadEndPlaces}}
    end.

%%%===================================================================
%%% Internal functions - Compilation
%%%===================================================================

do_compile_net(#swf_net{} = Net) ->
    case do_validate_net(Net) of
        ok ->
            {ForwardAdj, BackwardAdj} = build_adjacency_maps(Net),

            %% Build arc lookup maps
            {PlaceToOut, PlaceToIn, TransToOut, TransToIn} =
                build_arc_lookup_maps(Net),

            %% Compute transition input/output specifications
            TransInputs = compute_trans_inputs(Net),
            TransOutputs = compute_trans_outputs(Net),

            %% Compute reachability
            InitialNodes = maps:fold(
                fun(PlaceId, Tokens, Acc) when Tokens > 0 -> [PlaceId | Acc];
                   (_, _, Acc) -> Acc
                end,
                [],
                Net#swf_net.initial_marking
            ),
            ReachableFromInitial = bfs_reachable(InitialNodes, ForwardAdj),

            %% Compute places that can reach final
            CanReachFinal = bfs_reachable(Net#swf_net.final_places, BackwardAdj),

            %% Check soundness: all transitions in between
            AllTrans = maps:keys(Net#swf_net.transitions),
            IsSound = lists:all(
                fun(T) ->
                    sets:is_element(T, ReachableFromInitial) andalso
                    sets:is_element(T, CanReachFinal)
                end,
                AllTrans
            ),

            %% Check free-choice property
            IsFreeChoice = check_free_choice(Net),

            CompiledNet = #compiled_net{
                net = Net,
                place_to_out_arcs = PlaceToOut,
                place_to_in_arcs = PlaceToIn,
                trans_to_out_arcs = TransToOut,
                trans_to_in_arcs = TransToIn,
                trans_inputs = TransInputs,
                trans_outputs = TransOutputs,
                is_sound = IsSound,
                is_free_choice = IsFreeChoice,
                reachable_from_initial = ReachableFromInitial,
                can_reach_final = CanReachFinal,
                compiled_at = erlang:system_time(millisecond)
            },
            {ok, CompiledNet};
        {error, _} = Error ->
            Error
    end.

build_adjacency_maps(#swf_net{arcs = Arcs}) ->
    %% Build forward and backward adjacency maps
    {Forward, Backward} = lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt}, {Fwd, Bwd}) ->
            Fwd1 = maps:update_with(Src, fun(L) -> [Tgt | L] end, [Tgt], Fwd),
            Bwd1 = maps:update_with(Tgt, fun(L) -> [Src | L] end, [Src], Bwd),
            {Fwd1, Bwd1}
        end,
        {#{}, #{}},
        Arcs
    ),
    {Forward, Backward}.

build_arc_lookup_maps(#swf_net{places = Places, arcs = Arcs}) ->
    PlaceIds = maps:keys(Places),

    {PlaceToOut, PlaceToIn, TransToOut, TransToIn} = lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt} = Arc, {POut, PIn, TOut, TIn}) ->
            SrcIsPlace = lists:member(Src, PlaceIds),
            case SrcIsPlace of
                true ->
                    %% Place -> Transition arc
                    POut1 = maps:update_with(Src, fun(L) -> [Arc | L] end, [Arc], POut),
                    TIn1 = maps:update_with(Tgt, fun(L) -> [Arc | L] end, [Arc], TIn),
                    {POut1, PIn, TOut, TIn1};
                false ->
                    %% Transition -> Place arc
                    TOut1 = maps:update_with(Src, fun(L) -> [Arc | L] end, [Arc], TOut),
                    PIn1 = maps:update_with(Tgt, fun(L) -> [Arc | L] end, [Arc], PIn),
                    {POut, PIn1, TOut1, TIn}
            end
        end,
        {#{}, #{}, #{}, #{}},
        Arcs
    ),
    {PlaceToOut, PlaceToIn, TransToOut, TransToIn}.

compute_trans_inputs(#swf_net{places = Places, arcs = Arcs}) ->
    PlaceIds = maps:keys(Places),

    lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt, weight = W, kind = Kind}, Acc) ->
            case lists:member(Src, PlaceIds) of
                true ->
                    %% This is an input arc to transition Tgt
                    maps:update_with(
                        Tgt,
                        fun(L) -> [{Src, W, Kind} | L] end,
                        [{Src, W, Kind}],
                        Acc
                    );
                false ->
                    Acc
            end
        end,
        #{},
        Arcs
    ).

compute_trans_outputs(#swf_net{places = Places, arcs = Arcs}) ->
    PlaceIds = maps:keys(Places),

    lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt, weight = W, kind = Kind}, Acc) ->
            case lists:member(Tgt, PlaceIds) of
                true ->
                    %% This is an output arc from transition Src
                    maps:update_with(
                        Src,
                        fun(L) -> [{Tgt, W, Kind} | L] end,
                        [{Tgt, W, Kind}],
                        Acc
                    );
                false ->
                    Acc
            end
        end,
        #{},
        Arcs
    ).

bfs_reachable(StartNodes, AdjMap) ->
    bfs_reachable(StartNodes, AdjMap, sets:from_list(StartNodes)).

bfs_reachable([], _AdjMap, Visited) ->
    Visited;
bfs_reachable([Node | Rest], AdjMap, Visited) ->
    Neighbors = maps:get(Node, AdjMap, []),
    NewNodes = [N || N <- Neighbors, not sets:is_element(N, Visited)],
    NewVisited = lists:foldl(fun sets:add_element/2, Visited, NewNodes),
    bfs_reachable(Rest ++ NewNodes, AdjMap, NewVisited).

check_free_choice(#swf_net{places = Places, arcs = Arcs}) ->
    %% A net is free-choice if for every two arcs sharing a source place,
    %% the source place is the only input to both target transitions
    PlaceIds = maps:keys(Places),

    %% Group arcs by source place
    PlaceToTargets = lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt}, Acc) ->
            case lists:member(Src, PlaceIds) of
                true ->
                    maps:update_with(Src, fun(L) -> [Tgt | L] end, [Tgt], Acc);
                false ->
                    Acc
            end
        end,
        #{},
        Arcs
    ),

    %% Build transition input sets
    TransInputs = lists:foldl(
        fun(#swf_arc{source = Src, target = Tgt}, Acc) ->
            case lists:member(Src, PlaceIds) of
                true ->
                    maps:update_with(Tgt, fun(S) -> sets:add_element(Src, S) end,
                                    sets:from_list([Src]), Acc);
                false ->
                    Acc
            end
        end,
        #{},
        Arcs
    ),

    %% Check free-choice property
    maps:fold(
        fun(PlaceId, Transitions, Acc) ->
            case length(Transitions) > 1 of
                true ->
                    %% Multiple transitions from this place
                    %% Check if place is the only input to all of them
                    AllSingleInput = lists:all(
                        fun(T) ->
                            case maps:get(T, TransInputs, sets:new()) of
                                Inputs -> sets:size(Inputs) =:= 1
                            end
                        end,
                        Transitions
                    ),
                    Acc andalso AllSingleInput;
                false ->
                    Acc
            end
        end,
        true,
        PlaceToTargets
    ).
