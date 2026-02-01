-module(erlmcp_mermaid_session_tests).
-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_mermaid_session:start_link(<<"test_session">>),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

setup_with_state() ->
    {ok, SessionId, Pid} = erlmcp_mermaid_session:create_session(#{
        test => true,
        description => <<"Test session">>
    }),
    {SessionId, Pid}.

cleanup_with_state({SessionId, Pid}) ->
    erlmcp_mermaid_session:delete_session(SessionId),
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end.

%%====================================================================
%% Session Lifecycle Tests
%%====================================================================

session_lifecycle_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        fun(_Pid) ->
            [
                ?_test(begin
                    %% Verify session is registered
                    ?assertEqual(1, length(gproc:lookup_local_names(erlmcp_mermaid_session)))
                end)
            ]
        end}.

create_session_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    ?assert(is_binary(SessionId)),
                    ?assertEqual(<<"mermaid_", _/binary>>, SessionId)
                end),
                ?_test(begin
                    {ok, SessionState} = erlmcp_mermaid_session:get_session(SessionId),
                    ?assertMatch(#{session_id := SessionId}, SessionState),
                    ?assertEqual(#{}, maps:get(diagrams, SessionState)),
                    ?assertEqual([], maps:get(history, SessionState))
                end)
            ]
        end}.

delete_session_test_() ->
    {foreach,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    ok = erlmcp_mermaid_session:delete_session(SessionId),
                    timer:sleep(100),
                    ?assertEqual(undefined, gproc:lookup_local_name({erlmcp_mermaid_session, SessionId}))
                end)
            ]
        end}.

%%====================================================================
%% Diagram Management Tests
%%====================================================================

update_diagram_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram1">>,
                    Diagram = #{
                        id => DiagramId,
                        title => <<"Test Flowchart">>,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    %% Add as owner (should succeed)
                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),

                    {ok, SessionState} = erlmcp_mermaid_session:get_session(SessionId),
                    ?assertEqual(1, maps:size(maps:get(diagrams, SessionState))),
                    ?assertMatch(#{title := <<"Test Flowchart">>}, maps:get(DiagramId, maps:get(diagrams, SessionState))),

                    %% Verify history entry created
                    History = maps:get(history, SessionState),
                    ?assertEqual(1, length(History)),

                    %% Verify undo stack
                    UndoStack = maps:get(undo_stack, SessionState),
                    ?assertEqual(1, length(UndoStack))
                end)
            ]
        end}.

update_diagram_without_permission_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram2">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    %% Add as viewer (should fail)
                    {ok, ViewerId} = add_collaborator(SessionId, Pid, #{role => viewer}),
                    Result = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),
                    ?assertEqual({error, permission_denied}, Result)
                end)
            ]
        end}.

%%====================================================================
%% Version Control Tests (Undo/Redo)
%%====================================================================

undo_redo_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_undo">>,
                    Diagram1 = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },
                    Diagram2 = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B-->C;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),

                    %% First update
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram1),

                    %% Second update
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram2),

                    %% Undo should revert to Diagram1
                    {ok, RevertedDiagram} = erlmcp_mermaid_session:undo(SessionId, DiagramId),
                    ?assertEqual(Diagram1#{content := <<"graph TD; A-->B;">>}, RevertedDiagram),

                    %% Redo should restore Diagram2
                    {ok, RedoneDiagram} = erlmcp_mermaid_session:redo(SessionId, DiagramId),
                    ?assertEqual(Diagram2#{content := <<"graph TD; A-->B-->C;">>}, RedoneDiagram)
                end)
            ]
        end}.

undo_empty_stack_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_empty">>,
                    Result = erlmcp_mermaid_session:undo(SessionId, DiagramId),
                    ?assertEqual({error, nothing_to_undo}, Result)
                end)
            ]
        end}.

redo_empty_stack_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_empty">>,
                    Result = erlmcp_mermaid_session:redo(SessionId, DiagramId),
                    ?assertEqual({error, nothing_to_redo}, Result)
                end)
            ]
        end}.

%%====================================================================
%% History Management Tests
%%====================================================================

get_history_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_history">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),
                    {ok, _VersionId} = erlmcp_mermaid_session:add_version(SessionId, DiagramId, <<"Checkpoint">>),

                    {ok, History} = erlmcp_mermaid_session:get_history(SessionId, DiagramId),
                    ?assertEqual(2, length(History)),
                    ?assertMatch([#{action := updated}, #{action := updated}], History)
                end)
            ]
        end}.

add_version_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_version">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),

                    {ok, VersionId} = erlmcp_mermaid_session:add_version(SessionId, DiagramId, <<"Checkpoint 1">>),
                    ?assertEqual(<<"v_", _/binary>>, VersionId)
                end)
            ]
        end}.

%%====================================================================
%% Collaboration Tests
%%====================================================================

add_collaborator_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    CollaboratorId = <<"collab1">>,
                    Info = #{
                        pid => Pid,
                        name => <<"Alice">>,
                        role => editor
                    },

                    ok = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),

                    {ok, Collaborators} = erlmcp_mermaid_session:list_collaborators(SessionId),
                    ?assertEqual(1, length(Collaborators)),
                    ?assertMatch(#{id := CollaboratorId, name := <<"Alice">>, role := editor}, hd(Collaborators))
                end)
            ]
        end}.

remove_collaborator_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    CollaboratorId = <<"collab2">>,
                    Info = #{pid => Pid, name => <<"Bob">>, role => viewer},

                    ok = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),
                    {ok, Collaborators1} = erlmcp_mermaid_session:list_collaborators(SessionId),
                    ?assertEqual(1, length(Collaborators1)),

                    ok = erlmcp_mermaid_session:remove_collaborator(SessionId, CollaboratorId),
                    {ok, Collaborators2} = erlmcp_mermaid_session:list_collaborators(SessionId),
                    ?assertEqual(0, length(Collaborators2))
                end)
            ]
        end}.

add_duplicate_collaborator_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    CollaboratorId = <<"collab_dup">>,
                    Info = #{pid => Pid, name => <<"Charlie">>},

                    ok = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),
                    Result = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),
                    ?assertEqual({error, already_exists}, Result)
                end)
            ]
        end}.

collaborator_monitor_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    %% Spawn a temporary collaborator process
                    CollaboratorPid = spawn_link(fun() -> receive stop -> ok end end),
                    CollaboratorId = <<"collab_monitor">>,
                    Info = #{pid => CollaboratorPid, name => <<"Dave">>},

                    ok = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),
                    {ok, Collaborators1} = erlmcp_mermaid_session:list_collaborators(SessionId),
                    ?assertEqual(1, length(Collaborators1)),

                    %% Kill the collaborator process
                    exit(CollaboratorPid, kill),
                    timer:sleep(200),

                    %% Verify collaborator was removed
                    {ok, Collaborators2} = erlmcp_mermaid_session:list_collaborators(SessionId),
                    ?assertEqual(0, length(Collaborators2))
                end)
            ]
        end}.

%%====================================================================
%% Locking Tests
%%====================================================================

set_lock_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    Resource = <<"diagram_lock">>,
                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),

                    ok = erlmcp_mermaid_session:set_lock(SessionId, Resource, write),
                    {ok, Locks} = erlmcp_mermaid_session:get_locks(SessionId),
                    ?assertEqual(1, length(Locks)),
                    ?assertMatch(#{resource := Resource, type := write}, hd(Locks))
                end)
            ]
        end}.

release_lock_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    Resource = <<"diagram_unlock">>,
                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),

                    ok = erlmcp_mermaid_session:set_lock(SessionId, Resource, write),
                    ok = erlmcp_mermaid_session:release_lock(SessionId, Resource),
                    {ok, Locks} = erlmcp_mermaid_session:get_locks(SessionId),
                    ?assertEqual(0, length(Locks))
                end)
            ]
        end}.

lock_conflict_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    Resource = <<"diagram_conflict">>,
                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),

                    %% First lock succeeds
                    ok = erlmcp_mermaid_session:set_lock(SessionId, Resource, write),

                    %% Second lock from same owner should fail (already locked)
                    Result = erlmcp_mermaid_session:set_lock(SessionId, Resource, write),
                    ?assertEqual({error, already_locked}, Result)
                end)
            ]
        end}.

%%====================================================================
%% Session Statistics Tests
%%====================================================================

get_session_stats_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_stats">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),
                    {ok, _VersionId} = erlmcp_mermaid_session:add_version(SessionId, DiagramId, <<"Stats test">>),

                    {ok, Stats} = erlmcp_mermaid_session:get_session_stats(SessionId),
                    ?assertEqual(1, maps:get(num_diagrams, Stats)),
                    ?assertEqual(2, maps:get(num_history_entries, Stats)),
                    ?assertEqual(1, maps:get(num_collaborators, Stats)),
                    ?assert(is_integer(maps:get(age_ms, Stats)))
                end)
            ]
        end}.

%%====================================================================
%% Export/Import Tests
%%====================================================================

export_session_json_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_export">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),

                    {ok, JsonData} = erlmcp_mermaid_session:export_session(SessionId, json),
                    ?assert(is_binary(JsonData)),
                    ?assert(byte_size(JsonData) > 0)
                end)
            ]
        end}.

import_session_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    SessionData = jsx:encode(#{
                        session_id => <<"imported_session">>,
                        diagrams => #{},
                        history => [],
                        collaborators => #{},
                        locks => #{}
                    }),

                    {ok, ImportedState} = erlmcp_mermaid_session:import_session(SessionId, SessionData),
                    ?assertMatch(#{session_id := <<"imported_session">>}, ImportedState)
                end)
            ]
        end}.

%%====================================================================
%% Persistence Tests
%%====================================================================

save_session_test_() ->
    {setup,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, Pid}) ->
            [
                ?_test(begin
                    DiagramId = <<"diagram_save">>,
                    Diagram = #{
                        id => DiagramId,
                        type => flowchart,
                        content => <<"graph TD; A-->B;">>
                    },

                    {ok, OwnerId} = add_collaborator(SessionId, Pid, #{role => owner}),
                    ok = erlmcp_mermaid_session:update_diagram(SessionId, DiagramId, Diagram),
                    ok = erlmcp_mermaid_session:save_session(SessionId),

                    %% Verify persistence
                    {ok, SavedSession} = erlmcp_session_backend:fetch(SessionId),
                    ?assertMatch(#{diagrams := #{DiagramId := _}}, SavedSession)
                end)
            ]
        end}.

list_sessions_test_() ->
    {foreach,
        fun setup_with_state/0,
        fun cleanup_with_state/1,
        fun({SessionId, _Pid}) ->
            [
                ?_test(begin
                    {ok, SessionIds} = erlmcp_mermaid_session:list_sessions(),
                    ?assert(lists:member(SessionId, SessionIds))
                end)
            ]
        end}.

%%====================================================================
%% Helper Functions
%%====================================================================

add_collaborator(SessionId, Pid, RoleMap) ->
    CollaboratorId = list_to_binary("collaborator_" ++ integer_to_list(erlang:unique_integer([positive]))),
    Info = RoleMap#{pid => Pid},
    ok = erlmcp_mermaid_session:add_collaborator(SessionId, CollaboratorId, Info),
    {ok, CollaboratorId}.
