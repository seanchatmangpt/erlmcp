-module(erlmcp_code_reload).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    reload_module/2,
    reload_modules/2,
    get_reload_history/0,
    rollback_module/1,
    validate_module/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Reload options
-type reload_opts() :: #{
    validate_syntax => boolean(),
    validate_dialyzer => boolean(),
    run_tests => boolean(),
    drain_connections => boolean(),
    drain_timeout_ms => pos_integer(),
    rollback_window_s => pos_integer(),
    smoke_tests => [fun(() -> ok | {error, term()})]
}.

%% Reload result
-type reload_result() :: {ok, OldVsn :: term(), NewVsn :: term()} | {error, term()}.

%% State record
-record(state, {
    reload_history = [] :: [reload_entry()],
    rollback_timers = #{} :: #{module() => reference()},
    draining = false :: boolean()
}).

-type reload_entry() :: #{
    module => module(),
    old_vsn => term(),
    new_vsn => term(),
    timestamp => erlang:timestamp(),
    result => ok | {error, term()}
}.

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Reload a single module with safety checks
-spec reload_module(module(), reload_opts()) -> reload_result().
reload_module(Module, Opts) ->
    gen_server:call(?MODULE, {reload_module, Module, Opts}, 60000).

%% @doc Reload multiple modules in dependency order
-spec reload_modules([module()], reload_opts()) -> [{module(), reload_result()}].
reload_modules(Modules, Opts) ->
    gen_server:call(?MODULE, {reload_modules, Modules, Opts}, 60000).

%% @doc Get reload history
-spec get_reload_history() -> [reload_entry()].
get_reload_history() ->
    gen_server:call(?MODULE, get_reload_history).

%% @doc Rollback a module to its previous version
-spec rollback_module(module()) -> ok | {error, term()}.
rollback_module(Module) ->
    gen_server:call(?MODULE, {rollback_module, Module}).

%% @doc Validate module without reloading
-spec validate_module(module()) -> ok | {error, term()}.
validate_module(Module) ->
    gen_server:call(?MODULE, {validate_module, Module}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Code reload manager started"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({reload_module, Module, Opts}, _From, State) ->
    Result = do_reload_module(Module, Opts, State),

    % Record in history
    Entry = #{
        module => Module,
        old_vsn => get_module_version(Module),
        new_vsn => get_module_version(Module),
        timestamp => erlang:timestamp(),
        result => element(1, Result)
    },
    NewHistory = [Entry | State#state.reload_history],

    % Setup rollback timer if configured
    RollbackWindow = maps:get(rollback_window_s, Opts, 60),
    NewState = case Result of
        {ok, _, _} ->
            TimerRef = erlang:send_after(RollbackWindow * 1000, self(), {rollback_window_expired, Module}),
            State#state{
                reload_history = NewHistory,
                rollback_timers = maps:put(Module, TimerRef, State#state.rollback_timers)
            };
        _ ->
            State#state{reload_history = NewHistory}
    end,

    {reply, Result, NewState};

handle_call({reload_modules, Modules, Opts}, _From, State) ->
    % Sort modules by dependency order
    SortedModules = sort_by_dependencies(Modules),

    % Reload each module
    Results = lists:map(fun(Mod) ->
        {Mod, do_reload_module(Mod, Opts, State)}
    end, SortedModules),

    {reply, Results, State};

handle_call(get_reload_history, _From, State) ->
    {reply, State#state.reload_history, State};

handle_call({rollback_module, Module}, _From, State) ->
    Result = do_rollback_module(Module),
    {reply, Result, State};

handle_call({validate_module, Module}, _From, State) ->
    Result = validate_module_internal(Module),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({rollback_window_expired, Module}, State) ->
    % Rollback window expired - keep the new code
    logger:info("Rollback window expired for module ~p, keeping new code", [Module]),
    NewTimers = maps:remove(Module, State#state.rollback_timers),
    {noreply, State#state{rollback_timers = NewTimers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Reload Logic
%%====================================================================

-spec do_reload_module(module(), reload_opts(), state()) -> reload_result().
do_reload_module(Module, Opts, State) ->
    logger:info("Starting reload for module ~p", [Module]),

    % Step 1: Validate new code
    case validate_module_internal(Module) of
        ok ->
            % Step 2: Drain connections if requested
            case maps:get(drain_connections, Opts, true) of
                true ->
                    DrainTimeout = maps:get(drain_timeout_ms, Opts, 5000),
                    case drain_connections(Module, DrainTimeout) of
                        ok ->
                            do_reload_module_validated(Module, Opts, State);
                        {error, DrainErr} ->
                            {error, {drain_failed, DrainErr}}
                    end;
                false ->
                    do_reload_module_validated(Module, Opts, State)
            end;
        {error, ValidationErr} ->
            {error, {validation_failed, ValidationErr}}
    end.

-spec do_reload_module_validated(module(), reload_opts(), state()) -> reload_result().
do_reload_module_validated(Module, Opts, _State) ->
    OldVsn = get_module_version(Module),

    % Step 3: Check in-flight requests
    case wait_for_in_flight_requests(Module, 5000) of
        ok ->
            % Step 4: Save old .beam for rollback
            case backup_module_beam(Module) of
                ok ->
                    % Step 5: Perform code reload
                    case perform_code_reload(Module) of
                        ok ->
                            NewVsn = get_module_version(Module),

                            % Step 6: Run smoke tests
                            SmokeTests = maps:get(smoke_tests, Opts, []),
                            case run_smoke_tests(SmokeTests) of
                                ok ->
                                    logger:info("Module ~p reloaded successfully: ~p -> ~p",
                                               [Module, OldVsn, NewVsn]),
                                    {ok, OldVsn, NewVsn};
                                {error, TestErr} ->
                                    logger:error("Smoke tests failed for ~p, rolling back", [Module]),
                                    _ = do_rollback_module(Module),
                                    {error, {smoke_test_failed, TestErr}}
                            end;
                        {error, ReloadErr} ->
                            {error, {reload_failed, ReloadErr}}
                    end;
                {error, BackupErr} ->
                    {error, {backup_failed, BackupErr}}
            end;
        {error, WaitErr} ->
            {error, {in_flight_timeout, WaitErr}}
    end.

%%====================================================================
%% Internal functions - Validation
%%====================================================================

-spec validate_module_internal(module()) -> ok | {error, term()}.
validate_module_internal(Module) ->
    % Check if module exists
    case code:is_loaded(Module) of
        false ->
            {error, module_not_loaded};
        {file, _} ->
            % Validate syntax by attempting to compile check
            BeamFile = code:which(Module),
            case BeamFile of
                non_existing ->
                    {error, beam_file_not_found};
                BeamPath when is_list(BeamPath) ->
                    % Check beam file is readable
                    case filelib:is_regular(BeamPath) of
                        true ->
                            validate_beam_file(Module, BeamPath);
                        false ->
                            {error, beam_file_not_accessible}
                    end
            end
    end.

-spec validate_beam_file(module(), file:filename()) -> ok | {error, term()}.
validate_beam_file(Module, BeamPath) ->
    % Read beam file to verify it's valid
    case beam_lib:chunks(BeamPath, [exports, attributes]) of
        {ok, {Module, Chunks}} ->
            % Verify exports exist
            case lists:keyfind(exports, 1, Chunks) of
                {exports, Exports} when is_list(Exports) ->
                    % Check state record compatibility if gen_server
                    check_state_compatibility(Module);
                false ->
                    {error, no_exports}
            end;
        {error, beam_lib, Reason} ->
            {error, {beam_invalid, Reason}}
    end.

-spec check_state_compatibility(module()) -> ok | {error, term()}.
check_state_compatibility(_Module) ->
    % TODO: Implement state record version check
    % For now, assume compatible
    ok.

%%====================================================================
%% Internal functions - Connection Draining
%%====================================================================

-spec drain_connections(module(), pos_integer()) -> ok | {error, term()}.
drain_connections(Module, TimeoutMs) ->
    logger:info("Draining connections for module ~p (timeout: ~pms)", [Module, TimeoutMs]),

    % Signal to erlmcp_graceful_drain to pause new requests for this module
    case whereis(erlmcp_graceful_drain) of
        undefined ->
            logger:warning("Graceful drain service not available, skipping drain"),
            ok;
        Pid ->
            try
                gen_server:call(Pid, {drain_module, Module, TimeoutMs}, TimeoutMs + 1000)
            catch
                exit:{timeout, _} ->
                    {error, drain_timeout};
                exit:{noproc, _} ->
                    {error, drain_service_died}
            end
    end.

-spec wait_for_in_flight_requests(module(), pos_integer()) -> ok | {error, term()}.
wait_for_in_flight_requests(Module, TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    wait_for_in_flight_loop(Module, StartTime, TimeoutMs).

-spec wait_for_in_flight_loop(module(), integer(), pos_integer()) -> ok | {error, term()}.
wait_for_in_flight_loop(Module, StartTime, TimeoutMs) ->
    case get_in_flight_count(Module) of
        0 ->
            ok;
        Count ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            case Elapsed >= TimeoutMs of
                true ->
                    {error, {in_flight_timeout, Count}};
                false ->
                    timer:sleep(100),
                    wait_for_in_flight_loop(Module, StartTime, TimeoutMs)
            end
    end.

-spec get_in_flight_count(module()) -> non_neg_integer().
get_in_flight_count(_Module) ->
    % TODO: Query actual in-flight request count from registry
    % For now, assume 0
    0.

%%====================================================================
%% Internal functions - Code Reload
%%====================================================================

-spec backup_module_beam(module()) -> ok | {error, term()}.
backup_module_beam(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, module_not_found};
        BeamPath when is_list(BeamPath) ->
            BackupPath = BeamPath ++ ".backup",
            case file:copy(BeamPath, BackupPath) of
                {ok, _} ->
                    ok;
                {error, Reason} ->
                    {error, {backup_failed, Reason}}
            end
    end.

-spec perform_code_reload(module()) -> ok | {error, term()}.
perform_code_reload(Module) ->
    logger:info("Performing code reload for ~p", [Module]),

    % Step 1: Purge old code
    case code:soft_purge(Module) of
        true ->
            % Step 2: Load new code
            case code:load_file(Module) of
                {module, Module} ->
                    logger:info("Successfully reloaded module ~p", [Module]),
                    ok;
                {error, Reason} ->
                    logger:error("Failed to load module ~p: ~p", [Module, Reason]),
                    {error, {load_failed, Reason}}
            end;
        false ->
            % Processes still using old code, force purge
            logger:warning("Forcing purge of module ~p", [Module]),
            true = code:purge(Module),
            case code:load_file(Module) of
                {module, Module} ->
                    ok;
                {error, Reason} ->
                    {error, {load_failed, Reason}}
            end
    end.

-spec do_rollback_module(module()) -> ok | {error, term()}.
do_rollback_module(Module) ->
    logger:warning("Rolling back module ~p", [Module]),

    case code:which(Module) of
        non_existing ->
            {error, module_not_found};
        BeamPath when is_list(BeamPath) ->
            BackupPath = BeamPath ++ ".backup",
            case filelib:is_regular(BackupPath) of
                true ->
                    % Restore backup
                    case file:copy(BackupPath, BeamPath) of
                        {ok, _} ->
                            % Reload from backup
                            code:purge(Module),
                            case code:load_file(Module) of
                                {module, Module} ->
                                    logger:info("Successfully rolled back module ~p", [Module]),
                                    ok;
                                {error, Reason} ->
                                    {error, {rollback_load_failed, Reason}}
                            end;
                        {error, Reason} ->
                            {error, {restore_failed, Reason}}
                    end;
                false ->
                    {error, no_backup_available}
            end
    end.

%%====================================================================
%% Internal functions - Smoke Tests
%%====================================================================

-spec run_smoke_tests([fun(() -> ok | {error, term()})]) -> ok | {error, term()}.
run_smoke_tests([]) ->
    ok;
run_smoke_tests([Test | Rest]) ->
    case catch Test() of
        ok ->
            run_smoke_tests(Rest);
        {error, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, {test_crashed, Reason}}
    end.

%%====================================================================
%% Internal functions - Dependency Analysis
%%====================================================================

-spec sort_by_dependencies([module()]) -> [module()].
sort_by_dependencies(Modules) ->
    % Build dependency graph
    Graph = build_dependency_graph(Modules),

    % Topological sort
    case topological_sort(Graph) of
        {ok, Sorted} ->
            Sorted;
        {error, _Cycle} ->
            % Fallback to original order if cycle detected
            logger:warning("Circular dependency detected, using original order"),
            Modules
    end.

-spec build_dependency_graph([module()]) -> #{module() => [module()]}.
build_dependency_graph(Modules) ->
    lists:foldl(fun(Mod, Acc) ->
        Deps = get_module_dependencies(Mod),
        maps:put(Mod, Deps, Acc)
    end, #{}, Modules).

-spec get_module_dependencies(module()) -> [module()].
get_module_dependencies(Module) ->
    case code:which(Module) of
        non_existing ->
            [];
        BeamPath when is_list(BeamPath) ->
            case beam_lib:chunks(BeamPath, [imports]) of
                {ok, {Module, [{imports, Imports}]}} ->
                    % Extract unique modules
                    lists:usort([Mod || {Mod, _Fun, _Arity} <- Imports]);
                _ ->
                    []
            end
    end.

-spec topological_sort(#{module() => [module()]}) -> {ok, [module()]} | {error, cycle}.
topological_sort(Graph) ->
    % Simple topological sort using Kahn's algorithm
    Nodes = maps:keys(Graph),
    InDegree = calculate_in_degree(Graph, Nodes),
    Queue = [N || N <- Nodes, maps:get(N, InDegree, 0) =:= 0],
    topological_sort_loop(Queue, InDegree, Graph, []).

-spec topological_sort_loop([module()], #{module() => non_neg_integer()},
                            #{module() => [module()]}, [module()]) ->
    {ok, [module()]} | {error, cycle}.
topological_sort_loop([], _InDegree, Graph, Result) ->
    case length(Result) =:= maps:size(Graph) of
        true ->
            {ok, lists:reverse(Result)};
        false ->
            {error, cycle}
    end;
topological_sort_loop([Node | Queue], InDegree, Graph, Result) ->
    Neighbors = maps:get(Node, Graph, []),
    {NewQueue, NewInDegree} = lists:foldl(fun(Neighbor, {Q, ID}) ->
        NewDegree = maps:get(Neighbor, ID, 1) - 1,
        NewID = maps:put(Neighbor, NewDegree, ID),
        case NewDegree of
            0 ->
                {[Neighbor | Q], NewID};
            _ ->
                {Q, NewID}
        end
    end, {Queue, InDegree}, Neighbors),
    topological_sort_loop(NewQueue, NewInDegree, Graph, [Node | Result]).

-spec calculate_in_degree(#{module() => [module()]}, [module()]) ->
    #{module() => non_neg_integer()}.
calculate_in_degree(Graph, Nodes) ->
    Initial = maps:from_list([{N, 0} || N <- Nodes]),
    maps:fold(fun(_From, Tos, Acc) ->
        lists:foldl(fun(To, A) ->
            maps:update_with(To, fun(V) -> V + 1 end, 1, A)
        end, Acc, Tos)
    end, Initial, Graph).

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

-spec get_module_version(module()) -> term().
get_module_version(Module) ->
    case code:which(Module) of
        non_existing ->
            undefined;
        BeamPath when is_list(BeamPath) ->
            case beam_lib:version(BeamPath) of
                {ok, {Module, Version}} ->
                    Version;
                _ ->
                    undefined
            end
    end.
