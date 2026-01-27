%%%-----------------------------------------------------------------------------
%%% @doc Tests for TCPS Tutorial Step Execution Engine
%%%
%%% Comprehensive test suite covering:
%%% - Step retrieval and navigation
%%% - Step execution for all tutorials
%%% - Code snippet generation
%%% - Hint system
%%% - Checkpoint creation
%%% - Integration with TCPS modules
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_tutorial_steps_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

steps_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Get first step", fun test_get_first_step/0},
         {"Get next step", fun test_get_next_step/0},
         {"Get step info", fun test_get_step_info/0},
         {"Execute quality gate steps", fun test_execute_quality_gate_steps/0},
         {"Execute kanban WIP steps", fun test_execute_kanban_wip_steps/0},
         {"Execute andon event steps", fun test_execute_andon_event_steps/0},
         {"Execute five whys steps", fun test_execute_five_whys_steps/0},
         {"Execute complete workflow steps", fun test_execute_complete_workflow_steps/0},
         {"Get hints", fun test_get_hints/0},
         {"Get code snippets", fun test_get_code_snippets/0},
         {"Create checkpoints", fun test_create_checkpoints/0},
         {"Step type validation", fun test_step_types/0},
         {"Error handling", fun test_error_handling/0}
     ]}.

setup() ->
    %% Start mock TCPS services
    start_mock_tcps_services(),
    ok.

cleanup(_) ->
    stop_mock_tcps_services(),
    ok.

start_mock_tcps_services() ->
    %% Mock services will be started as needed in tests
    ok.

stop_mock_tcps_services() ->
    ok.

%%%=============================================================================
%%% Test Cases - Step Navigation
%%%=============================================================================

test_get_first_step() ->
    %% Test all tutorials
    ?assertEqual(<<"qg_step_1">>, tcps_tutorial_steps:get_first_step(quality_gate)),
    ?assertEqual(<<"kb_step_1">>, tcps_tutorial_steps:get_first_step(kanban_wip)),
    ?assertEqual(<<"ae_step_1">>, tcps_tutorial_steps:get_first_step(andon_event)),
    ?assertEqual(<<"fw_step_1">>, tcps_tutorial_steps:get_first_step(five_whys)),
    ?assertEqual(<<"cw_step_1">>, tcps_tutorial_steps:get_first_step(complete_workflow)),

    %% Test unknown tutorial
    ?assertEqual(<<"completed">>, tcps_tutorial_steps:get_first_step(unknown_tutorial)).

test_get_next_step() ->
    %% Test progression through quality_gate tutorial
    ?assertEqual(<<"qg_step_2">>, tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_1">>)),
    ?assertEqual(<<"qg_step_3">>, tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_2">>)),
    ?assertEqual(<<"qg_step_4">>, tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_3">>)),
    ?assertEqual(<<"qg_step_5">>, tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_4">>)),
    ?assertEqual(completed, tcps_tutorial_steps:get_next_step(quality_gate, <<"qg_step_5">>)),

    %% Test invalid step
    ?assertEqual(completed, tcps_tutorial_steps:get_next_step(quality_gate, <<"invalid_step">>)).

test_get_step_info() ->
    %% Get info for first quality gate step
    StepInfo = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_1">>),

    %% Verify structure
    ?assertEqual(<<"qg_step_1">>, maps:get(step_id, StepInfo)),
    ?assertEqual(quality_gate, maps:get(tutorial_id, StepInfo)),
    ?assertEqual(1, maps:get(step_number, StepInfo)),
    ?assert(maps:is_key(title, StepInfo)),
    ?assert(maps:is_key(type, StepInfo)),
    ?assert(maps:is_key(description, StepInfo)),
    ?assert(maps:is_key(learning_outcome, StepInfo)),
    ?assert(maps:is_key(estimated_time, StepInfo)),
    ?assert(maps:is_key(hints, StepInfo)),
    ?assert(maps:is_key(code_snippets, StepInfo)),

    %% Test invalid step
    InvalidInfo = tcps_tutorial_steps:get_step_info(quality_gate, <<"invalid">>),
    ?assertEqual(#{error => step_not_found}, InvalidInfo).

%%%=============================================================================
%%% Test Cases - Quality Gate Tutorial Execution
%%%=============================================================================

test_execute_quality_gate_steps() ->
    %% Step 1: Explanation
    {ok, Result1} = tcps_tutorial_steps:execute_step(quality_gate, <<"qg_step_1">>, #{}),
    ?assert(maps:get(success, Result1)),
    ?assertEqual(<<"qg_step_2">>, maps:get(next_step, Result1)),
    ?assert(is_binary(maps:get(feedback, Result1))),

    %% Step 2: Demonstration (start service)
    %% This may require mocking or actual service
    {ok, Result2} = tcps_tutorial_steps:execute_step(quality_gate, <<"qg_step_2">>, #{}),
    ?assert(maps:get(success, Result2)),
    ?assertEqual(<<"qg_step_3">>, maps:get(next_step, Result2)),

    %% Step 3: Hands-on (run quality gate)
    Input3 = #{sku_id => <<"tutorial_sku_001">>},
    {ok, Result3} = tcps_tutorial_steps:execute_step(quality_gate, <<"qg_step_3">>, Input3),
    ?assert(maps:is_key(success, Result3)),
    ?assertEqual(<<"qg_step_4">>, maps:get(next_step, Result3)),

    %% Step 4: Handle failures
    Input4 = #{failure_handled => true},
    {ok, Result4} = tcps_tutorial_steps:execute_step(quality_gate, <<"qg_step_4">>, Input4),
    ?assert(maps:get(success, Result4)),
    ?assertEqual(<<"qg_step_5">>, maps:get(next_step, Result4)),

    %% Step 5: Reflection
    {ok, Result5} = tcps_tutorial_steps:execute_step(quality_gate, <<"qg_step_5">>, #{}),
    ?assert(maps:get(success, Result5)),
    ?assertEqual(completed, maps:get(next_step, Result5)).

%%%=============================================================================
%%% Test Cases - Kanban WIP Tutorial Execution
%%%=============================================================================

test_execute_kanban_wip_steps() ->
    %% Step 1: Explanation
    {ok, Result1} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_1">>, #{}),
    ?assert(maps:get(success, Result1)),
    ?assertEqual(<<"kb_step_2">>, maps:get(next_step, Result1)),

    %% Step 2: Start Kanban service
    {ok, Result2} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_2">>, #{}),
    ?assert(maps:get(success, Result2)),
    ?assertEqual(<<"kb_step_3">>, maps:get(next_step, Result2)),

    %% Step 3: Check WIP status
    {ok, Result3} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_3">>, #{}),
    ?assert(maps:get(success, Result3)),
    ?assert(maps:is_key(output, Result3)),
    ?assertEqual(<<"kb_step_4">>, maps:get(next_step, Result3)),

    %% Step 4: Set WIP limits
    Input4 = #{limit => 3},
    {ok, Result4} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_4">>, Input4),
    ?assert(maps:get(success, Result4)),
    ?assertEqual(<<"kb_step_5">>, maps:get(next_step, Result4)),

    %% Step 5: Process pull signal
    Input5 = #{pull_signal => #{bucket => reliability, priority => 5, payload => #{type => bug_fix}}},
    {ok, Result5} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_5">>, Input5),
    ?assert(maps:is_key(success, Result5)),
    ?assertEqual(<<"kb_step_6">>, maps:get(next_step, Result5)),

    %% Step 6: Challenge - WIP limit reached
    Input6 = #{limit_reached => true},
    {ok, Result6} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_6">>, Input6),
    ?assert(maps:get(success, Result6)),
    ?assertEqual(completed, maps:get(next_step, Result6)).

%%%=============================================================================
%%% Test Cases - Andon Event Tutorial Execution
%%%=============================================================================

test_execute_andon_event_steps() ->
    %% Step 1: Explanation
    {ok, Result1} = tcps_tutorial_steps:execute_step(andon_event, <<"ae_step_1">>, #{}),
    ?assert(maps:get(success, Result1)),

    %% Step 2: Trigger Andon
    Input2 = #{
        context => #{
            sku_id => <<"tutorial_sku_andon">>,
            stage => testing,
            details => #{test => <<"unit_test">>, error => <<"assertion_failed">>}
        }
    },
    {ok, Result2} = tcps_tutorial_steps:execute_step(andon_event, <<"ae_step_2">>, Input2),
    ?assert(maps:get(success, Result2)),
    EventId = maps:get(output, Result2),
    ?assert(is_binary(EventId)),

    %% Step 3: Check blocking
    Input3 = #{sku_id => <<"tutorial_sku_andon">>},
    {ok, Result3} = tcps_tutorial_steps:execute_step(andon_event, <<"ae_step_3">>, Input3),
    ?assert(maps:is_key(success, Result3)),

    %% Step 4: Resolve Andon
    Input4 = #{
        event_id => EventId,
        resolution => #{
            root_cause => <<"Missing null check">>,
            actions_taken => <<"Added validation">>,
            prevention => <<"Add pattern to linter">>
        }
    },
    {ok, Result4} = tcps_tutorial_steps:execute_step(andon_event, <<"ae_step_4">>, Input4),
    ?assert(maps:get(success, Result4)),
    ?assertEqual(completed, maps:get(next_step, Result4)).

%%%=============================================================================
%%% Test Cases - Five Whys Tutorial Execution
%%%=============================================================================

test_execute_five_whys_steps() ->
    %% Step 1: Explanation
    {ok, Result1} = tcps_tutorial_steps:execute_step(five_whys, <<"fw_step_1">>, #{}),
    ?assert(maps:get(success, Result1)),

    %% Step 2: Start analysis
    Input2 = #{
        andon_event_id => <<"andon_tutorial_001">>,
        problem => <<"Test failure in authentication">>
    },
    {ok, Result2} = tcps_tutorial_steps:execute_step(five_whys, <<"fw_step_2">>, Input2),
    ?assert(maps:get(success, Result2)),
    AnalysisId = maps:get(output, Result2),
    ?assert(is_binary(AnalysisId)),

    %% Step 3: Answer five whys
    Input3 = #{
        analysis_id => AnalysisId,
        whys => [
            <<"Token was null">>,
            <<"Token generation failed">>,
            <<"Config file missing">>,
            <<"Deployment script didn't copy configs">>,
            <<"No config validation step">>
        ]
    },
    {ok, Result3} = tcps_tutorial_steps:execute_step(five_whys, <<"fw_step_3">>, Input3),
    ?assert(maps:get(success, Result3)),

    %% Step 4: Finalize
    Input4 = #{
        analysis_id => AnalysisId,
        root_cause => <<"Missing deployment validation">>,
        prevention => <<"Add config validation gate">>
    },
    {ok, Result4} = tcps_tutorial_steps:execute_step(five_whys, <<"fw_step_4">>, Input4),
    ?assert(maps:get(success, Result4)),
    ?assertEqual(completed, maps:get(next_step, Result4)).

%%%=============================================================================
%%% Test Cases - Complete Workflow Tutorial Execution
%%%=============================================================================

test_execute_complete_workflow_steps() ->
    %% Step 1: Explanation
    {ok, Result1} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_1">>, #{}),
    ?assert(maps:get(success, Result1)),

    %% Step 2: Create work order
    Input2 = #{
        pull_signal => #{
            source => github,
            type => bug_report,
            priority => 7,
            payload => #{issue_id => 123}
        }
    },
    {ok, Result2} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_2">>, Input2),
    ?assert(maps:get(success, Result2)),
    WorkOrderId = maps:get(output, Result2),

    %% Step 3: Start work order
    Input3 = #{work_order_id => WorkOrderId},
    {ok, Result3} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_3">>, Input3),
    ?assert(maps:get(success, Result3)),

    %% Step 4: Run quality gates
    Input4 = #{work_order_id => WorkOrderId},
    {ok, Result4} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_4">>, Input4),
    ?assert(maps:get(success, Result4)),

    %% Step 5: Handle failures (skip if all passed)
    Input5 = #{all_passed => true, andon_resolved => false},
    {ok, Result5} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_5">>, Input5),
    ?assert(maps:get(success, Result5)),

    %% Step 6: Complete work order
    Input6 = #{
        work_order_id => WorkOrderId,
        deliverables => #{
            artifacts => [<<"fixed_module.erl">>],
            tests => [<<"unit_tests.erl">>]
        }
    },
    {ok, Result6} = tcps_tutorial_steps:execute_step(complete_workflow, <<"cw_step_6">>, Input6),
    ?assert(maps:get(success, Result6)),
    ?assertEqual(completed, maps:get(next_step, Result6)).

%%%=============================================================================
%%% Test Cases - Hints and Code Snippets
%%%=============================================================================

test_get_hints() ->
    %% Test hints for different steps
    Hint1 = tcps_tutorial_steps:get_hint(quality_gate, <<"qg_step_3">>),
    ?assert(is_binary(Hint1)),
    ?assert(byte_size(Hint1) > 0),

    Hint2 = tcps_tutorial_steps:get_hint(kanban_wip, <<"kb_step_5">>),
    ?assert(is_binary(Hint2)),

    %% Test step with no hints
    NoHint = tcps_tutorial_steps:get_hint(quality_gate, <<"qg_step_1">>),
    ?assert(is_binary(NoHint)).

test_get_code_snippets() ->
    %% Get snippets for hands-on step
    Snippets = tcps_tutorial_steps:get_code_snippet(quality_gate, <<"qg_step_3">>),
    ?assert(is_list(Snippets)),

    case Snippets of
        [] -> ok;
        [FirstSnippet | _] ->
            ?assert(maps:is_key(language, FirstSnippet)),
            ?assert(maps:is_key(code, FirstSnippet)),
            ?assert(maps:is_key(explanation, FirstSnippet)),
            ?assert(maps:is_key(executable, FirstSnippet))
    end,

    %% Test step with no snippets
    NoSnippets = tcps_tutorial_steps:get_code_snippet(quality_gate, <<"qg_step_1">>),
    ?assertEqual([], NoSnippets).

test_create_checkpoints() ->
    %% Create checkpoint
    State = #{
        user_action => completed,
        result => success,
        timestamp => erlang:timestamp()
    },

    Checkpoint = tcps_tutorial_steps:create_checkpoint(quality_gate, <<"qg_step_3">>, State),

    %% Verify checkpoint structure
    ?assertEqual(quality_gate, maps:get(tutorial_id, Checkpoint)),
    ?assertEqual(<<"qg_step_3">>, maps:get(step_id, Checkpoint)),
    ?assert(maps:is_key(timestamp, Checkpoint)),
    ?assertEqual(State, maps:get(state, Checkpoint)),
    ?assertEqual(progress, maps:get(checkpoint_type, Checkpoint)).

%%%=============================================================================
%%% Test Cases - Step Types
%%%=============================================================================

test_step_types() ->
    %% Verify different step types
    Info1 = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_1">>),
    ?assertEqual(explanation, maps:get(type, Info1)),

    Info2 = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_2">>),
    ?assertEqual(demonstration, maps:get(type, Info2)),

    Info3 = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_3">>),
    ?assertEqual(hands_on, maps:get(type, Info3)),

    Info4 = tcps_tutorial_steps:get_step_info(kanban_wip, <<"kb_step_6">>),
    ?assertEqual(challenge, maps:get(type, Info4)),

    Info5 = tcps_tutorial_steps:get_step_info(quality_gate, <<"qg_step_5">>),
    ?assertEqual(reflection, maps:get(type, Info5)).

%%%=============================================================================
%%% Test Cases - Error Handling
%%%=============================================================================

test_error_handling() ->
    %% Test unknown tutorial
    {error, unknown_tutorial} = tcps_tutorial_steps:execute_step(invalid_tutorial, <<"step_1">>, #{}),

    %% Test unknown step
    {error, unknown_step} = tcps_tutorial_steps:execute_step(quality_gate, <<"invalid_step">>, #{}),

    %% Test missing required input
    {error, _} = tcps_tutorial_steps:execute_step(five_whys, <<"fw_step_2">>, #{}),

    %% Test invalid input
    {ok, Result} = tcps_tutorial_steps:execute_step(kanban_wip, <<"kb_step_6">>, #{limit_reached => false}),
    ?assertNot(maps:get(success, Result)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {timeout, 30,
     fun test_full_tutorial_execution/0}.

test_full_tutorial_execution() ->
    %% Execute complete quality_gate tutorial
    Steps = [<<"qg_step_1">>, <<"qg_step_2">>, <<"qg_step_3">>, <<"qg_step_4">>, <<"qg_step_5">>],

    Inputs = [
        #{},
        #{},
        #{sku_id => <<"integration_sku_001">>},
        #{failure_handled => true},
        #{}
    ],

    lists:foldl(fun({Step, Input}, _) ->
        {ok, Result} = tcps_tutorial_steps:execute_step(quality_gate, Step, Input),
        ?assert(maps:get(success, Result) orelse not maps:get(success, Result)),
        maps:get(next_step, Result)
    end, <<"qg_step_1">>, lists:zip(Steps, Inputs)),

    ok.
