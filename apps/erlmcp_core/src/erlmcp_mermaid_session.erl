-module(erlmcp_mermaid_session).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_link/2,
    create_session/1,
    create_session/2,
    get_session/1,
    update_diagram/3,
    add_version/3,
    undo/2,
    redo/2,
    get_history/2,
    add_collaborator/3,
    remove_collaborator/2,
    list_collaborators/1,
    set_lock/3,
    release_lock/2,
    get_locks/1,
    save_session/1,
    delete_session/1,
    list_sessions/0,
    get_session_stats/1,
    export_session/2,
    import_session/2
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

-include("erlmcp.hrl").

%%====================================================================
%% Types
%%====================================================================

-type diagram_id() :: binary().
-type version_id() :: binary().
-type collaborator_id() :: binary().
-type lock_type() :: read | write.
-export_type([diagram_id/0, version_id/0, collaborator_id/0, lock_type/0]).

-type diagram() :: #{
    id := diagram_id(),
    title => binary(),
    description => binary(),
    content := binary(),
    type => flowchart | sequence | class | state | er | gantt | pie | mindmap,
    metadata => map()
}.

-type version() :: #{
    id := version_id(),
    diagram := diagram(),
    timestamp := integer(),
    author := collaborator_id(),
    comment => binary()
}.

-type collaborator() :: #{
    id := collaborator_id(),
    name => binary(),
    role => owner | editor | viewer,
    joined_at := integer(),
    last_active := integer()
}.

-type lock() :: #{
    resource := binary(),
    type := lock_type(),
    owner := collaborator_id(),
    acquired_at := integer(),
    expires_at => integer()
}.

-type history_entry() :: #{
    version_id := version_id(),
    timestamp := integer(),
    author := collaborator_id(),
    action => created | updated | deleted | reverted,
    comment => binary()
}.

-type session_state() :: #{
    session_id := binary(),
    diagrams := #{diagram_id() => diagram()},
    active_diagram => diagram_id(),
    history := [history_entry()],
    undo_stack := [version_id()],
    redo_stack := [version_id()],
    collaborators := #{collaborator_id() => collaborator()},
    locks := #{binary() => lock()},
    created_at := integer(),
    updated_at := integer(),
    metadata := map()
}.

-record(state, {
    session_id :: binary(),
    session_state :: session_state(),
    backend :: module(),
    save_timer :: reference() | undefined,
    auto_save_interval :: pos_integer(),
    max_history :: pos_integer(),
    collaborators_monitors :: #{collaborator_id() => reference()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(SessionId) ->
    start_link(SessionId, #{}).

-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(SessionId, Opts) ->
    gen_server:start_link(?MODULE, [SessionId, Opts], []).

-spec create_session(map()) -> {ok, binary(), pid()} | {error, term()}.
create_session(Metadata) ->
    create_session(Metadata, #{}).

-spec create_session(map(), map()) -> {ok, binary(), pid()} | {error, term()}.
create_session(Metadata, Opts) ->
    SessionId = generate_session_id(),
    case start_link(SessionId, Opts) of
        {ok, Pid} ->
            InitialState = #{
                session_id => SessionId,
                diagrams => #{},
                history => [],
                undo_stack => [],
                redo_stack => [],
                collaborators => #{},
                locks => #{},
                created_at => erlang:system_time(millisecond),
                updated_at => erlang:system_time(millisecond),
                metadata => Metadata
            },
            case gen_server:call(Pid, {initialize, InitialState}, 5000) of
                ok -> {ok, SessionId, Pid};
                Error -> Error
            end;
        Error ->
            Error
    end.

-spec get_session(binary()) -> {ok, session_state()} | {error, not_found | term()}.
get_session(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, get_state, 5000);
        undefined ->
            {error, not_found}
    end.

-spec update_diagram(binary(), diagram_id(), diagram()) -> ok | {error, term()}.
update_diagram(SessionId, DiagramId, Diagram) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {update_diagram, DiagramId, Diagram}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec add_version(binary(), diagram_id(), binary()) -> {ok, version_id()} | {error, term()}.
add_version(SessionId, DiagramId, Comment) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {add_version, DiagramId, Comment}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec undo(binary(), diagram_id()) -> {ok, diagram()} | {error, term()}.
undo(SessionId, DiagramId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {undo, DiagramId}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec redo(binary(), diagram_id()) -> {ok, diagram()} | {error, term()}.
redo(SessionId, DiagramId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {redo, DiagramId}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec get_history(binary(), diagram_id()) -> {ok, [history_entry()]} | {error, term()}.
get_history(SessionId, DiagramId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {get_history, DiagramId}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec add_collaborator(binary(), collaborator_id(), map()) -> ok | {error, term()}.
add_collaborator(SessionId, CollaboratorId, Info) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {add_collaborator, CollaboratorId, Info}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec remove_collaborator(binary(), collaborator_id()) -> ok | {error, term()}.
remove_collaborator(SessionId, CollaboratorId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {remove_collaborator, CollaboratorId}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec list_collaborators(binary()) -> {ok, [collaborator()]} | {error, term()}.
list_collaborators(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, list_collaborators, 5000);
        undefined ->
            {error, not_found}
    end.

-spec set_lock(binary(), binary(), lock_type()) -> ok | {error, term()}.
set_lock(SessionId, Resource, LockType) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {set_lock, Resource, LockType}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec release_lock(binary(), binary()) -> ok | {error, term()}.
release_lock(SessionId, Resource) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {release_lock, Resource}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec get_locks(binary()) -> {ok, [lock()]} | {error, term()}.
get_locks(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, get_locks, 5000);
        undefined ->
            {error, not_found}
    end.

-spec save_session(binary()) -> ok | {error, term()}.
save_session(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, save_session, 5000);
        undefined ->
            {error, not_found}
    end.

-spec delete_session(binary()) -> ok | {error, term()}.
delete_session(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:stop(Pid, normal, 5000);
        undefined ->
            {error, not_found}
    end.

-spec list_sessions() -> {ok, [binary()]}.
list_sessions() ->
    Sessions = gproc:lookup_local_names(?MODULE),
    {ok, [SessionId || {{_, SessionId} = _Key, _Pid, _Count} <- Sessions]}.

-spec get_session_stats(binary()) -> {ok, map()} | {error, term()}.
get_session_stats(SessionId) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, get_stats, 5000);
        undefined ->
            {error, not_found}
    end.

-spec export_session(binary(), binary()) -> {ok, binary()} | {error, term()}.
export_session(SessionId, Format) when Format =:= json; Format =:= msgpack ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {export, Format}, 5000);
        undefined ->
            {error, not_found}
    end.

-spec import_session(binary(), binary()) -> {ok, session_state()} | {error, term()}.
import_session(SessionId, Data) ->
    case gproc:lookup_local_name({?MODULE, SessionId}) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {import, Data}, 5000);
        undefined ->
            {error, not_found}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init({binary(), map()}) -> {ok, state()}.
init([SessionId, Opts]) ->
    Backend = maps:get(backend, Opts, erlmcp_session_ets),
    AutoSaveInterval = maps:get(auto_save_interval, Opts, 30000),
    MaxHistory = maps:get(max_history, Opts, 100),

    true = gproc:register_local_name({?MODULE, SessionId}, self()),

    {ok, #state{
        session_id = SessionId,
        session_state = undefined,
        backend = Backend,
        save_timer = undefined,
        auto_save_interval = AutoSaveInterval,
        max_history = MaxHistory,
        collaborators_monitors = #{}
    }}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({initialize, SessionState}, _From, State) ->
    NewState = State#state{session_state = SessionState},
    SaveTimer = schedule_auto_save(State#state.auto_save_interval),
    {reply, ok, NewState#state{save_timer = SaveTimer}};

handle_call(get_state, _From, State) ->
    {reply, {ok, State#state.session_state}, State};

handle_call({update_diagram, DiagramId, Diagram}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{collaborators := Collaborators} = SessionState0,

    case verify_write_permission(DiagramId, Pid, Collaborators) of
        ok ->
            case acquire_lock(DiagramId, Pid, SessionState0) of
                {ok, NewSessionState} ->
                    VersionId = generate_version_id(),
                    Version = #{
                        id => VersionId,
                        diagram => Diagram,
                        timestamp => erlang:system_time(millisecond),
                        author => get_collaborator_id(Pid, Collaborators),
                        comment => maps:get(comment, Diagram, <<>>)
                    },

                    UpdatedDiagrams = maps:put(DiagramId, Diagram, NewSessionState#{diagrams := Diagrams}),
                    UpdatedHistory = add_history_entry(updated, VersionId, Version, NewSessionState),
                    UpdatedUndoStack = [VersionId | NewSessionState#{undo_stack := UndoStack}],
                    UpdatedRedoStack = [],

                    FinalSessionState = NewSessionState#{
                        diagrams => UpdatedDiagrams,
                        history => UpdatedHistory,
                        undo_stack => UpdatedUndoStack,
                        redo_stack => UpdatedRedoStack,
                        updated_at => erlang:system_time(millisecond)
                    },

                    {reply, ok, State#state{session_state = FinalSessionState}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({add_version, DiagramId, Comment}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{diagrams := Diagrams} = SessionState0,

    case maps:get(DiagramId, Diagrams, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Diagram ->
            VersionId = generate_version_id(),
            Version = #{
                id => VersionId,
                diagram => Diagram,
                timestamp => erlang:system_time(millisecond),
                author => get_collaborator_id(Pid, SessionState0#{collaborators := Collaborators}),
                comment => Comment
            },

            UpdatedHistory = add_history_entry(updated, VersionId, Version, SessionState0),
            UpdatedUndoStack = [VersionId | SessionState0#{undo_stack := UndoStack}],
            UpdatedRedoStack = [],

            FinalSessionState = SessionState0#{
                history => UpdatedHistory,
                undo_stack => UpdatedUndoStack,
                redo_stack => UpdatedRedoStack,
                updated_at => erlang:system_time(millisecond)
            },

            {reply, {ok, VersionId}, State#state{session_state = FinalSessionState}}
    end;

handle_call({undo, DiagramId}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{undo_stack := UndoStack, diagrams := Diagrams} = SessionState0,

    case UndoStack of
        [] ->
            {reply, {error, nothing_to_undo}, State};
        [CurrentVersionId | RestUndoStack] ->
            case find_version_by_id(CurrentVersionId, SessionState0) of
                {ok, #{} = Version} ->
                    #{diagram := Diagram} = Version,
                    UpdatedDiagrams = maps:put(DiagramId, Diagram, Diagrams),

                    UpdatedRedoStack = [CurrentVersionId | SessionState0#{redo_stack := RedoStack}],
                    UpdatedHistory = add_history_entry(reverted, CurrentVersionId, Version, SessionState0),

                    FinalSessionState = SessionState0#{
                        diagrams => UpdatedDiagrams,
                        undo_stack => RestUndoStack,
                        redo_stack => UpdatedRedoStack,
                        history => UpdatedHistory,
                        updated_at => erlang:system_time(millisecond)
                    },

                    {reply, {ok, Diagram}, State#state{session_state = FinalSessionState}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({redo, DiagramId}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{redo_stack := RedoStack, diagrams := Diagrams} = SessionState0,

    case RedoStack of
        [] ->
            {reply, {error, nothing_to_redo}, State};
        [VersionId | RestRedoStack] ->
            case find_version_by_id(VersionId, SessionState0) of
                {ok, #{} = Version} ->
                    #{diagram := Diagram} = Version,
                    UpdatedDiagrams = maps:put(DiagramId, Diagram, Diagrams),

                    UpdatedUndoStack = [VersionId | SessionState0#{undo_stack := UndoStack}],
                    UpdatedHistory = add_history_entry(redone, VersionId, Version, SessionState0),

                    FinalSessionState = SessionState0#{
                        diagrams => UpdatedDiagrams,
                        undo_stack => UpdatedUndoStack,
                        redo_stack => RestRedoStack,
                        history => UpdatedHistory,
                        updated_at => erlang:system_time(millisecond)
                    },

                    {reply, {ok, Diagram}, State#state{session_state = FinalSessionState}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_history, DiagramId}, _From, State) ->
    SessionState = State#state.session_state,
    #{history := History} = SessionState,

    FilteredHistory = lists:filter(
        fun(Entry) ->
            maps:get(diagram_id, Entry, undefined) =:= DiagramId
        end,
        History
    ),

    {reply, {ok, FilteredHistory}, State};

handle_call({add_collaborator, CollaboratorId, Info}, _From, State) ->
    SessionState0 = State#state.session_state,
    #{collaborators := Collaborators} = SessionState0,

    case maps:get(CollaboratorId, Collaborators, undefined) of
        undefined ->
            Collaborator = #{
                id => CollaboratorId,
                name => maps:get(name, Info, <<>>),
                role => maps:get(role, Info, viewer),
                joined_at => erlang:system_time(millisecond),
                last_active => erlang:system_time(millisecond)
            },
            UpdatedCollaborators = maps:put(CollaboratorId, Collaborator, Collaborators),

            MonitorRef = monitor(process, maps:get(pid, Info, self())),
            UpdatedMonitors = maps:put(CollaboratorId, MonitorRef, State#state.collaborators_monitors),

            FinalSessionState = SessionState0#{
                collaborators => UpdatedCollaborators,
                updated_at => erlang:system_time(millisecond)
            },

            {reply, ok, State#state{
                session_state = FinalSessionState,
                collaborators_monitors = UpdatedMonitors
            }};
        _Existing ->
            {reply, {error, already_exists}, State}
    end;

handle_call({remove_collaborator, CollaboratorId}, _From, State) ->
    SessionState0 = State#state.session_state,
    #{collaborators := Collaborators} = SessionState0,

    case maps:get(CollaboratorId, Collaborators, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _Collaborator ->
            UpdatedCollaborators = maps:remove(CollaboratorId, Collaborators),

            UpdatedMonitors = case maps:get(CollaboratorId, State#state.collaborators_monitors, undefined) of
                undefined -> State#state.collaborators_monitors;
                MonitorRef ->
                    demonitor(MonitorRef, [flush]),
                    maps:remove(CollaboratorId, State#state.collaborators_monitors)
            end,

            FinalSessionState = SessionState0#{
                collaborators => UpdatedCollaborators,
                updated_at => erlang:system_time(millisecond)
            },

            {reply, ok, State#state{
                session_state = FinalSessionState,
                collaborators_monitors = UpdatedMonitors
            }}
    end;

handle_call(list_collaborators, _From, State) ->
    SessionState = State#state.session_state,
    #{collaborators := Collaborators} = SessionState,
    {reply, {ok, maps:values(Collaborators)}, State};

handle_call({set_lock, Resource, LockType}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{locks := Locks, collaborators := Collaborators} = SessionState0,

    CollaboratorId = get_collaborator_id(Pid, Collaborators),

    case maps:get(Resource, Locks, undefined) of
        undefined ->
            Lock = #{
                resource => Resource,
                type => LockType,
                owner => CollaboratorId,
                acquired_at => erlang:system_time(millisecond)
            },
            UpdatedLocks = maps:put(Resource, Lock, Locks),
            FinalSessionState = SessionState0#{locks => UpdatedLocks},
            {reply, ok, State#state{session_state = FinalSessionState}};
        ExistingLock when ExistingLock#{owner := CollaboratorId} ->
            {reply, {error, already_locked}, State};
        ExistingLock when ExistingLock#{type := read} andalso LockType =:= read ->
            Lock = #{
                resource => Resource,
                type => read,
                owner => CollaboratorId,
                acquired_at => erlang:system_time(millisecond)
            },
            UpdatedLocks = maps:put(Resource, Lock, Locks),
            FinalSessionState = SessionState0#{locks => UpdatedLocks},
            {reply, ok, State#state{session_state = FinalSessionState}};
        _ExistingLock ->
            {reply, {error, resource_locked}, State}
    end;

handle_call({release_lock, Resource}, {Pid, _Tag}, State) ->
    SessionState0 = State#state.session_state,
    #{locks := Locks, collaborators := Collaborators} = SessionState0,

    CollaboratorId = get_collaborator_id(Pid, Collaborators),

    case maps:get(Resource, Locks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Lock when Lock#{owner := CollaboratorId} ->
            UpdatedLocks = maps:remove(Resource, Locks),
            FinalSessionState = SessionState0#{locks => UpdatedLocks},
            {reply, ok, State#state{session_state = FinalSessionState}};
        _Lock ->
            {reply, {error, not_owner}, State}
    end;

handle_call(get_locks, _From, State) ->
    SessionState = State#state.session_state,
    #{locks := Locks} = SessionState,
    {reply, {ok, maps:values(Locks)}, State};

handle_call(save_session, _From, State) ->
    SessionState = State#state.session_state,
    #{session_id := SessionId} = SessionState,

    case erlmcp_session_backend:store(SessionId, SessionState) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_stats, _From, State) ->
    SessionState = State#state.session_state,
    #{
        diagrams := Diagrams,
        history := History,
        collaborators := Collaborators,
        locks := Locks,
        created_at := CreatedAt,
        updated_at := UpdatedAt
    } = SessionState,

    Stats = #{
        num_diagrams => maps:size(Diagrams),
        num_history_entries => length(History),
        num_collaborators => maps:size(Collaborators),
        num_locks => maps:size(Locks),
        age_ms => UpdatedAt - CreatedAt,
        session_id => State#state.session_id
    },

    {reply, {ok, Stats}, State};

handle_call({export, Format}, _From, State) ->
    SessionState = State#state.session_state,

    try
        Data = case Format of
            json -> jsx:encode(SessionState);
            msgpack -> msgpack:pack(SessionState)
        end,
        {reply, {ok, Data}, State}
    catch
        Type:Error:Stack ->
            logger:error("Failed to export session: ~p:~p~n~p", [Type, Error, Stack]),
            {reply, {error, {export_failed, Error}}, State}
    end;

handle_call({import, Data}, _From, State) ->
    try
        SessionState = case is_binary(Data) andalso byte_size(Data) > 0 of
            true -> jsx:decode(Data, [return_maps]);
            false when is_map(Data) -> Data
        end,

        {reply, {ok, SessionState}, State}
    catch
        Type:Error:Stack ->
            logger:error("Failed to import session: ~p:~p~n~p", [Type, Error, Stack]),
            {reply, {error, {import_failed, Error}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(auto_save, State) ->
    SessionState = State#state.session_state,
    #{session_id := SessionId} = SessionState,

    case erlmcp_session_backend:store(SessionId, SessionState) of
        ok ->
            logger:debug("Auto-saved session ~p", [SessionId]);
        {error, Reason} ->
            logger:error("Failed to auto-save session ~p: ~p", [SessionId, Reason])
    end,

    NewTimer = schedule_auto_save(State#state.auto_save_interval),
    {noreply, State#state{save_timer = NewTimer}};

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    #{collaborators := Collaborators} = State#state.session_state,

    case find_collaborator_by_monitor(MonitorRef, Collaborators) of
        {ok, CollaboratorId} ->
            UpdatedCollaborators = maps:remove(CollaboratorId, Collaborators),
            UpdatedMonitors = maps:remove(CollaboratorId, State#state.collaborators_monitors),

            UpdatedSessionState = (State#state.session_state)#{
                collaborators => UpdatedCollaborators,
                updated_at => erlang:system_time(millisecond)
            },

            logger:info("Collaborator ~p disconnected from session ~p",
                [CollaboratorId, State#state.session_id]),

            {noreply, State#state{
                session_state => UpdatedSessionState,
                collaborators_monitors => UpdatedMonitors
            }};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    case State#state.save_timer of
        undefined -> ok;
        Timer -> erlang:cancel_timer(Timer)
    end,

    SessionState = State#state.session_state,
    case SessionState of
        undefined -> ok;
        _ ->
            #{session_id := SessionId} = SessionState,
            erlmcp_session_backend:store(SessionId, SessionState)
    end,

    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_session_id() -> binary().
generate_session_id() ->
    Rand = crypto:strong_rand_bytes(16),
    <<"mermaid_", (binary:encode_hex(Rand))/binary>>.

-spec generate_version_id() -> binary().
generate_version_id() ->
    Rand = crypto:strong_rand_bytes(12),
    <<"v_", (binary:encode_hex(Rand))/binary>>.

-spec verify_write_permission(binary(), pid(), map()) -> ok | {error, term()}.
verify_write_permission(_DiagramId, _Pid, Collaborators) when map_size(Collaborators) =:= 0 ->
    ok;
verify_write_permission(_DiagramId, Pid, Collaborators) ->
    CollaboratorId = get_collaborator_id(Pid, Collaborators),
    case maps:get(CollaboratorId, Collaborators, undefined) of
        #{role := Role} when Role =:= owner; Role =:= editor ->
            ok;
        _ ->
            {error, permission_denied}
    end.

-spec acquire_lock(binary(), pid(), session_state()) -> {ok, session_state()} | {error, term()}.
acquire_lock(Resource, Pid, SessionState) ->
    #{locks := Locks, collaborators := Collaborators} = SessionState,
    CollaboratorId = get_collaborator_id(Pid, Collaborators),

    case maps:get(Resource, Locks, undefined) of
        undefined ->
            Lock = #{
                resource => Resource,
                type => write,
                owner => CollaboratorId,
                acquired_at => erlang:system_time(millisecond)
            },
            UpdatedLocks = maps:put(Resource, Lock, Locks),
            {ok, SessionState#{locks => UpdatedLocks}};
        ExistingLock when ExistingLock#{owner := CollaboratorId} ->
            {ok, SessionState};
        _ ->
            {error, resource_locked}
    end.

-spec get_collaborator_id(pid(), map()) -> binary().
get_collaborator_id(Pid, Collaborators) ->
    case maps:to_list(Collaborators) of
        [] ->
            <<"_anonymous">>;
        List ->
            case lists:keyfind(Pid, 2, List) of
                {CollaboratorId, #{}} -> CollaboratorId;
                false -> <<"_anonymous">>
            end
    end.

-spec add_history_entry(atom(), version_id(), version(), session_state()) -> [history_entry()].
add_history_entry(Action, VersionId, Version, SessionState) ->
    #{history := History} = SessionState,

    Entry = #{
        version_id => VersionId,
        timestamp => Version#{timestamp},
        author => Version#{author},
        action => Action,
        comment => maps:get(comment, Version, <<>>)
    },

    [Entry | History].

-spec find_version_by_id(version_id(), session_state()) -> {ok, version()} | {error, not_found}.
find_version_by_id(_VersionId, #{history := History}) ->
    case lists:search(fun(Entry) -> maps:get(version_id, Entry) =:= _VersionId end, History) of
        {value, Entry} -> {ok, Entry};
        false -> {error, not_found}
    end.

-spec find_collaborator_by_monitor(reference(), map()) -> {ok, collaborator_id()} | error.
find_collaborator_by_monitor(MonitorRef, Collaborators) ->
    maps:fold(
        fun(CollaboratorId, _Collaborator, Acc) ->
            case Acc of
                {ok, _} -> Acc;
                error ->
                    case maps:get(CollaboratorId, Collaborators, undefined) of
                        #{monitor_ref := MonitorRef} -> {ok, CollaboratorId};
                        _ -> error
                    end
            end
        end,
        error,
        Collaborators
    ).

-spec schedule_auto_save(pos_integer()) -> reference().
schedule_auto_save(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), auto_save).
