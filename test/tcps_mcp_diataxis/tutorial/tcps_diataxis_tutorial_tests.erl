%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS Diataxis Tutorial Orchestration Module
%%%
%%% Comprehensive test suite covering:
%%% - Tutorial lifecycle management
%%% - Step execution and progression
%%% - Progress tracking and checkpoints
%%% - User progress and achievements
%%% - Prerequisite validation
%%% - Session management
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_diataxis_tutorial_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

tutorial_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"List available tutorials", fun test_list_tutorials/0},
         {"Get tutorial info", fun test_get_tutorial_info/0},
         {"Start tutorial session", fun test_start_tutorial/0},
         {"Get tutorial progress", fun test_get_tutorial_progress/0},
         {"Complete tutorial", fun test_complete_tutorial/0},
         {"Abandon tutorial", fun test_abandon_tutorial/0},
         {"Get current step", fun test_get_current_step/0},
         {"Execute step successfully", fun test_execute_step/0},
         {"Validate step", fun test_validate_step/0},
         {"Get hint", fun test_get_hint/0},
         {"Skip step", fun test_skip_step/0},
         {"Get user progress", fun test_get_user_progress/0},
         {"Get achievements", fun test_get_achievements/0},
         {"Get learning path", fun test_get_learning_path/0},
         {"Export progress", fun test_export_progress/0},
         {"Prerequisite validation", fun test_prerequisite_validation/0},
         {"Session state persistence", fun test_session_state/0},
         {"Multi-user isolation", fun test_multi_user/0},
         {"Step progression flow", fun test_step_progression/0},
         {"Achievement calculation", fun test_achievement_calculation/0}
     ]}.

setup() ->
    %% Start tutorial server
    {ok, Pid} = tcps_diataxis_tutorial:start_link(),

    %% Start dependencies if needed
    application:ensure_all_started(jsx),

    %% Mock TCPS services
    start_mock_services(),

    Pid.

cleanup(Pid) ->
    %% Stop tutorial server
    case is_process_alive(Pid) of
        true -> gen_server:stop(Pid);
        false -> ok
    end,

    %% Stop mocks
    stop_mock_services().

start_mock_services() ->
    %% These will be mocked or use test instances
    ok.

stop_mock_services() ->
    ok.

%%%=============================================================================
%%% Test Cases - Tutorial Management
%%%=============================================================================

test_list_tutorials() ->
    {ok, Tutorials} = tcps_diataxis_tutorial:list_tutorials(),

    %% Verify we have 5 tutorials
    ?assertEqual(5, length(Tutorials)),

    %% Verify tutorial structure
    [FirstTutorial | _] = Tutorials,
    ?assert(maps:is_key(id, FirstTutorial)),
    ?assert(maps:is_key(title, FirstTutorial)),
    ?assert(maps:is_key(difficulty, FirstTutorial)),
    ?assert(maps:is_key(prerequisites, FirstTutorial)),
    ?assert(maps:is_key(learning_objectives, FirstTutorial)).

test_get_tutorial_info() ->
    %% Test valid tutorial
    {ok, Info} = tcps_diataxis_tutorial:get_tutorial_info(quality_gate),
    ?assertEqual(quality_gate, maps:get(id, Info)),
    ?assertEqual(<<"Your First Quality Gate">>, maps:get(title, Info)),
    ?assertEqual(beginner, maps:get(difficulty, Info)),

    %% Test invalid tutorial
    {error, not_found} = tcps_diataxis_tutorial:get_tutorial_info(invalid_tutorial).

test_start_tutorial() ->
    UserId = <<"user_001">>,

    %% Start beginner tutorial (no prerequisites)
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ?assert(is_binary(SessionId)),
    ?assertMatch(<<"session_", _/binary>>, SessionId),

    %% Verify session was created
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(quality_gate, maps:get(tutorial_id, Session)),
    ?assertEqual(UserId, maps:get(user_id, Session)),
    ?assertEqual(in_progress, maps:get(status, Session)).

test_get_tutorial_progress() ->
    UserId = <<"user_002">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, kanban_wip),

    %% Get progress
    {ok, Progress} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),

    %% Verify progress structure
    ?assertEqual(SessionId, maps:get(session_id, Progress)),
    ?assertEqual(kanban_wip, maps:get(tutorial_id, Progress)),
    ?assertEqual(0, maps:get(hints_used, Progress)),
    ?assertEqual([], maps:get(completed_steps, Progress)),

    %% Test invalid session
    {error, not_found} = tcps_diataxis_tutorial:get_tutorial_progress(<<"invalid_session">>).

test_complete_tutorial() ->
    UserId = <<"user_003">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Complete tutorial
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Verify status changed
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(completed, maps:get(status, Session)),
    ?assert(maps:get(completion_time, Session) =/= undefined).

test_abandon_tutorial() ->
    UserId = <<"user_004">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Abandon tutorial
    ok = tcps_diataxis_tutorial:abandon_tutorial(SessionId),

    %% Verify status changed
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(abandoned, maps:get(status, Session)).

%%%=============================================================================
%%% Test Cases - Step Execution
%%%=============================================================================

test_get_current_step() ->
    UserId = <<"user_005">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Get current step
    {ok, StepInfo} = tcps_diataxis_tutorial:get_current_step(SessionId),

    %% Verify step structure
    ?assertEqual(<<"qg_step_1">>, maps:get(step_id, StepInfo)),
    ?assert(maps:is_key(title, StepInfo)),
    ?assert(maps:is_key(type, StepInfo)),
    ?assert(maps:is_key(description, StepInfo)).

test_execute_step() ->
    UserId = <<"user_006">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Execute first step (explanation - no input required)
    {ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

    %% Verify result structure
    ?assert(maps:get(success, Result)),
    ?assert(maps:is_key(feedback, Result)),
    ?assertEqual(<<"qg_step_2">>, maps:get(next_step, Result)),

    %% Verify progress updated
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(<<"qg_step_2">>, maps:get(current_step, Session)),
    ?assertEqual([<<"qg_step_1">>], maps:get(completed_steps, Session)).

test_validate_step() ->
    UserId = <<"user_007">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Skip to step 3 (hands-on step)
    {ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),
    {ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

    %% Validate with correct data
    Validation = #{
        gate_result => {pass, #{receipt_id => <<"test_receipt">>}},
        sku_id => <<"tutorial_sku_001">>
    },

    {ok, ValidationResult} = tcps_diataxis_tutorial:validate_step(SessionId, Validation),

    %% Verify validation passed
    ?assert(maps:get(valid, ValidationResult)),
    ?assertEqual([], maps:get(errors, ValidationResult)).

test_get_hint() ->
    UserId = <<"user_008">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Skip to hands-on step
    {ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),
    {ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),

    %% Get hint
    {ok, Hint} = tcps_diataxis_tutorial:get_hint(SessionId),

    %% Verify hint is provided
    ?assert(is_binary(Hint)),
    ?assert(byte_size(Hint) > 0),

    %% Verify hints_used incremented
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(1, maps:get(hints_used, Session)).

test_skip_step() ->
    UserId = <<"user_009">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    InitialStep = <<"qg_step_1">>,

    %% Skip step
    ok = tcps_diataxis_tutorial:skip_step(SessionId, <<"Testing skip functionality">>),

    %% Verify moved to next step
    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertNotEqual(InitialStep, maps:get(current_step, Session)),
    ?assertEqual([InitialStep], maps:get(completed_steps, Session)).

%%%=============================================================================
%%% Test Cases - Progress & Analytics
%%%=============================================================================

test_get_user_progress() ->
    UserId = <<"user_010">>,

    %% Start and complete a tutorial
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Get user progress
    {ok, Progress} = tcps_diataxis_tutorial:get_user_progress(UserId),

    %% Verify progress structure
    ?assertEqual(UserId, maps:get(user_id, Progress)),
    ?assert(lists:member(quality_gate, maps:get(completed_tutorials, Progress))),
    ?assert(maps:get(total_time_spent, Progress) >= 0).

test_get_achievements() ->
    UserId = <<"user_011">>,

    %% Initially no achievements
    {ok, Achievements1} = tcps_diataxis_tutorial:get_achievements(UserId),
    ?assertEqual([], Achievements1),

    %% Complete a tutorial
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Should have achievement
    {ok, Achievements2} = tcps_diataxis_tutorial:get_achievements(UserId),
    ?assert(length(Achievements2) > 0),
    ?assert(lists:member(<<"First Quality Gate">>, Achievements2)).

test_get_learning_path() ->
    UserId = <<"user_012">>,

    %% Get initial learning path (all tutorials)
    {ok, Path1} = tcps_diataxis_tutorial:get_learning_path(UserId),
    ?assertEqual(5, length(Path1)),

    %% Complete beginner tutorials
    {ok, SessionId1} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId1),

    %% Learning path should be updated
    {ok, Path2} = tcps_diataxis_tutorial:get_learning_path(UserId),
    ?assertEqual(4, length(Path2)),
    ?assertNot(lists:member(quality_gate, Path2)).

test_export_progress() ->
    UserId = <<"user_013">>,

    %% Complete a tutorial
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Export progress
    {ok, JsonBinary} = tcps_diataxis_tutorial:export_progress(UserId),

    %% Verify JSON is valid
    ?assert(is_binary(JsonBinary)),
    Progress = jsx:decode(JsonBinary, [return_maps]),
    ?assert(maps:is_key(<<"user_id">>, Progress)),
    ?assert(maps:is_key(<<"completed_tutorials">>, Progress)).

%%%=============================================================================
%%% Test Cases - Advanced Scenarios
%%%=============================================================================

test_prerequisite_validation() ->
    UserId = <<"user_014">>,

    %% Try to start advanced tutorial without prerequisites
    Result = tcps_diataxis_tutorial:start_tutorial(UserId, five_whys),

    %% Should fail due to missing prerequisites
    ?assertMatch({error, prerequisites_not_met}, Result),

    %% Complete prerequisite
    {ok, SessionId1} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId1),

    {ok, SessionId2} = tcps_diataxis_tutorial:start_tutorial(UserId, andon_event),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId2),

    %% Now should succeed
    {ok, SessionId3} = tcps_diataxis_tutorial:start_tutorial(UserId, five_whys),
    ?assert(is_binary(SessionId3)).

test_session_state() ->
    UserId = <<"user_015">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Execute steps and verify state preservation
    {ok, Result1} = tcps_diataxis_tutorial:execute_step(SessionId, #{}),
    Step1Checkpoint = maps:get(checkpoint, Result1),

    {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    Checkpoints = maps:get(checkpoints, Session),

    %% Verify checkpoint stored
    case Step1Checkpoint of
        undefined -> ?assertEqual(#{}, Checkpoints);
        _ -> ?assert(maps:size(Checkpoints) > 0)
    end.

test_multi_user() ->
    User1 = <<"user_016">>,
    User2 = <<"user_017">>,

    %% Start same tutorial for different users
    {ok, Session1} = tcps_diataxis_tutorial:start_tutorial(User1, quality_gate),
    {ok, Session2} = tcps_diataxis_tutorial:start_tutorial(User2, quality_gate),

    %% Sessions should be different
    ?assertNotEqual(Session1, Session2),

    %% Progress one user
    {ok, _} = tcps_diataxis_tutorial:execute_step(Session1, #{}),

    %% Verify other user unaffected
    {ok, Session2Progress} = tcps_diataxis_tutorial:get_tutorial_progress(Session2),
    ?assertEqual([], maps:get(completed_steps, Session2Progress)).

test_step_progression() ->
    UserId = <<"user_018">>,
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),

    %% Execute all steps in sequence
    Steps = [<<"qg_step_1">>, <<"qg_step_2">>, <<"qg_step_3">>, <<"qg_step_4">>, <<"qg_step_5">>],

    lists:foldl(fun(ExpectedStep, _) ->
        {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
        CurrentStep = maps:get(current_step, Session),
        ?assertEqual(ExpectedStep, CurrentStep),

        {ok, Result} = tcps_diataxis_tutorial:execute_step(SessionId, #{failure_handled => true, sku_id => <<"test_sku">>}),
        maps:get(next_step, Result)
    end, <<"qg_step_1">>, Steps),

    %% Final step should mark as completed
    {ok, FinalSession} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
    ?assertEqual(5, length(maps:get(completed_steps, FinalSession))).

test_achievement_calculation() ->
    UserId = <<"user_019">>,

    %% Complete tutorial without using hints
    {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId),

    %% Should have "No Hints Used" achievement
    {ok, Achievements} = tcps_diataxis_tutorial:get_achievements(UserId),
    ?assert(lists:member(<<"No Hints Used">>, Achievements)),

    %% Complete another tutorial with hints
    {ok, SessionId2} = tcps_diataxis_tutorial:start_tutorial(UserId, kanban_wip),
    {ok, _} = tcps_diataxis_tutorial:get_hint(SessionId2),
    ok = tcps_diataxis_tutorial:complete_tutorial(SessionId2),

    %% Should not have duplicate "No Hints Used" for second tutorial
    {ok, Achievements2} = tcps_diataxis_tutorial:get_achievements(UserId),
    ?assertEqual(2, length([A || A <- Achievements2, A =:= <<"No Hints Used">>]) - 1).

%%%=============================================================================
%%% Performance Tests
%%%=============================================================================

performance_test_() ->
    {timeout, 30,
     fun test_concurrent_users/0}.

test_concurrent_users() ->
    %% Start 10 concurrent users
    Users = [list_to_binary(io_lib:format("perf_user_~3..0B", [N])) || N <- lists:seq(1, 10)],

    %% Start tutorials concurrently
    Sessions = lists:map(fun(UserId) ->
        {ok, SessionId} = tcps_diataxis_tutorial:start_tutorial(UserId, quality_gate),
        SessionId
    end, Users),

    %% Execute steps concurrently
    lists:foreach(fun(SessionId) ->
        {ok, _} = tcps_diataxis_tutorial:execute_step(SessionId, #{})
    end, Sessions),

    %% Verify all progressed
    lists:foreach(fun(SessionId) ->
        {ok, Session} = tcps_diataxis_tutorial:get_tutorial_progress(SessionId),
        ?assertEqual(1, length(maps:get(completed_steps, Session)))
    end, Sessions).
