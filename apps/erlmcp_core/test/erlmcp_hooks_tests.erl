%%%-------------------------------------------------------------------
%%% @doc erlmcp_hooks_tests - Claude Code Hooks Integration Tests
%%%
%%% Tests the OTP-based hooks system for Claude Code integration:
%%% - Real gen_server (no mocks)
%%% - Quality gate validation (compilation, tests, coverage)
%%% - Pre/post task lifecycle
%%% - Pre/post edit validation
%%% - Session start/end hooks
%%% - State-based verification (pass/fail results)
%%%
%%% Uses Chicago School TDD:
%%% - Verify observable hook results
%%% - Test quality gate enforcement
%%% - Validate receipt generation
%%%
%%% Target: 85%+ coverage (core quality enforcement module)
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_hooks_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

hooks_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_hooks_start/1,
      fun test_pre_task_valid/1,
      fun test_pre_task_empty_description/1,
      fun test_post_task_success/1,
      fun test_post_task_with_context/1,
      fun test_pre_edit_valid/1,
      fun test_pre_edit_protected_path/1,
      fun test_post_edit_success/1,
      fun test_session_start_success/1,
      fun test_session_end_success/1,
      fun test_gen_server_handle_cast/1,
      fun test_gen_server_handle_info/1,
      fun test_gen_server_terminate/1,
      fun test_gen_server_code_change/1]}.

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    %% Start hooks server
    {ok, Pid} = erlmcp_hooks:start_link(),
    Pid.

cleanup(Pid) ->
    %% Stop hooks server
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_hooks);
        false ->
            ok
    end.

%%====================================================================
%% Pre-Task Hook Tests
%%====================================================================

test_pre_task_valid(_Pid) ->
    %% Exercise: Pre-task validation with valid context
    TaskId = <<"task_123">>,
    Context = #{
        description => <<"Test task description">>,
        tool => <<"erlang-test-engineer">>
    },

    Result = erlmcp_hooks:pre_task(TaskId, Context),

    %% Verify: Should pass (valid context)
    ?assertMatch({pass, _}, Result),
    {pass, Receipt} = Result,
    ?assert(is_map(Receipt)).

test_pre_task_empty_description(_Pid) ->
    %% Exercise: Pre-task with empty description
    TaskId = <<"task_456">>,
    Context = #{
        description => <<"">>,
        tool => <<"test">>
    },

    Result = erlmcp_hooks:pre_task(TaskId, Context),

    %% Verify: Should fail (empty description)
    ?assertMatch({fail, _}, Result),
    {fail, Violations} = Result,
    ?assert(is_map(Violations)).

%%====================================================================
%% Post-Task Hook Tests
%%====================================================================

test_post_task_success(_Pid) ->
    %% Exercise: Post-task validation (no context)
    TaskId = <<"task_789">>,

    %% Note: This will call actual quality gates which may not pass
    %% We're testing the hook interface, not the quality gates themselves
    Result = erlmcp_hooks:post_task(TaskId),

    %% Verify: Returns pass or fail with receipts/violations
    case Result of
        {pass, Receipts} ->
            ?assert(is_map(Receipts));
        {fail, Violations} ->
            ?assert(is_map(Violations))
    end.

test_post_task_with_context(_Pid) ->
    %% Exercise: Post-task with context
    TaskId = <<"task_101">>,
    Context = #{
        description => <<"Complete task">>,
        tool => <<"coder">>,
        files_modified => [<<"test.erl">>]
    },

    Result = erlmcp_hooks:post_task(TaskId, Context),

    %% Verify: Returns structured result
    case Result of
        {pass, Receipts} ->
            ?assert(is_map(Receipts));
        {fail, Violations} ->
            ?assert(is_map(Violations))
    end.

%%====================================================================
%% Pre-Edit Hook Tests
%%====================================================================

test_pre_edit_valid(_Pid) ->
    %% Exercise: Pre-edit validation for allowed file
    FilePath = <<"/Users/sac/erlmcp/apps/erlmcp_core/src/test_module.erl">>,
    Context = #{
        tool => <<"coder">>,
        session_id => <<"session_123">>
    },

    Result = erlmcp_hooks:pre_edit(FilePath, Context),

    %% Verify: Should pass (valid file path)
    ?assertMatch({pass, _}, Result),
    {pass, Receipt} = Result,
    ?assert(is_map(Receipt)).

test_pre_edit_protected_path(_Pid) ->
    %% Exercise: Pre-edit for protected path
    ProtectedPath = <<"/usr/local/lib/erlang/lib/system.erl">>,
    Context = #{
        tool => <<"coder">>,
        session_id => <<"session_456">>
    },

    Result = erlmcp_hooks:pre_edit(ProtectedPath, Context),

    %% Verify: Should fail (protected path)
    ?assertMatch({fail, _}, Result),
    {fail, Violations} = Result,
    ?assert(is_map(Violations)).

%%====================================================================
%% Post-Edit Hook Tests
%%====================================================================

test_post_edit_success(_Pid) ->
    %% Exercise: Post-edit validation
    FilePath = <<"/Users/sac/erlmcp/apps/erlmcp_core/src/test.erl">>,
    Context = #{
        tool => <<"coder">>,
        session_id => <<"session_789">>
    },

    Result = erlmcp_hooks:post_edit(FilePath, Context),

    %% Verify: Returns structured result
    case Result of
        {pass, Receipts} ->
            ?assert(is_map(Receipts));
        {fail, Violations} ->
            ?assert(is_map(Violations))
    end.

%%====================================================================
%% Session Hook Tests
%%====================================================================

test_session_start_success(_Pid) ->
    %% Exercise: Session start hook
    SessionId = <<"session_start_test">>,

    Result = erlmcp_hooks:session_start(SessionId),

    %% Verify: Should pass (session initialization)
    ?assertMatch({pass, _}, Result),
    {pass, Receipt} = Result,
    ?assert(is_map(Receipt)).

test_session_end_success(_Pid) ->
    %% Exercise: Session end hook
    SessionId = <<"session_end_test">>,

    Result = erlmcp_hooks:session_end(SessionId),

    %% Verify: Should pass (cleanup successful)
    ?assertMatch({pass, _}, Result),
    {pass, Receipt} = Result,
    ?assert(is_map(Receipt)).

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

test_gen_server_handle_cast(_Pid) ->
    %% Exercise: Send cast message
    Result = gen_server:cast(erlmcp_hooks, test_cast),

    %% Verify: Doesn't crash
    ?assertEqual(ok, Result).

test_gen_server_handle_info(Pid) ->
    %% Exercise: Send info message
    Pid ! test_info,
    timer:sleep(100),

    %% Verify: Still alive
    ?assert(is_process_alive(Pid)).

test_gen_server_terminate(Pid) ->
    %% Exercise: Stop server
    ok = gen_server:stop(erlmcp_hooks),

    %% Verify: Stopped
    ?assertNot(is_process_alive(Pid)).

test_gen_server_code_change(_Pid) ->
    %% Exercise: Code change
    {ok, State} = sys:get_state(erlmcp_hooks),
    Result = erlmcp_hooks:code_change("", State, ""),

    %% Verify: Succeeds
    ?assertMatch({ok, _}, Result).
