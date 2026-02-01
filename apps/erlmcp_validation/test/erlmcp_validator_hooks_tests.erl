%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Tests for erlmcp_validator_hooks
%%%
%%% Chicago School TDD: Test real integration, no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_validator_hooks_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

validator_hooks_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(emit_protocol_failure_when_enabled_test()),
          ?_test(emit_transport_failure_when_enabled_test()),
          ?_test(emit_sse_failure_when_enabled_test()),
          ?_test(no_emit_when_disabled_test()),
          ?_test(no_emit_when_reproducer_not_started_test())
         ]
     end}.

setup() ->
    %% Start reproducer system
    {ok, _Pid} = erlmcp_reproducer:start_link(),
    #{}.

cleanup(_State) ->
    %% Stop reproducer system
    case whereis(erlmcp_reproducer) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,
    ok.

%%%===================================================================
%%% Test Cases (Chicago School: Real integration)
%%%===================================================================

emit_protocol_failure_when_enabled_test() ->
    %% Setup: Enable capture
    erlmcp_validator_hooks:enable_reproducer_capture(),

    %% Exercise: Emit protocol failure
    ok = erlmcp_validator_hooks:emit_protocol_failure(
        <<"TEST_PROTOCOL">>,
        {ok, valid},
        {error, invalid}
    ),

    %% Verify: Reproducer was created (state-based)
    {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),
    ?assert(length(Unfixed) >= 1),

    %% Cleanup
    erlmcp_validator_hooks:disable_reproducer_capture().

emit_transport_failure_when_enabled_test() ->
    %% Setup: Enable capture
    erlmcp_validator_hooks:enable_reproducer_capture(),

    %% Exercise: Emit transport failure
    ok = erlmcp_validator_hooks:emit_transport_failure(
        <<"TEST_TRANSPORT">>,
        {ok, valid_frame},
        {error, invalid_frame}
    ),

    %% Verify: Reproducer was created
    {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),
    ?assert(length(Unfixed) >= 1),

    %% Cleanup
    erlmcp_validator_hooks:disable_reproducer_capture().

emit_sse_failure_when_enabled_test() ->
    %% Setup: Enable capture
    erlmcp_validator_hooks:enable_reproducer_capture(),

    %% Exercise: Emit SSE failure
    ok = erlmcp_validator_hooks:emit_sse_failure(
        <<"TEST_SSE">>,
        {ok, valid_resume},
        {error, invalid_resume}
    ),

    %% Verify: Reproducer was created
    {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),
    ?assert(length(Unfixed) >= 1),

    %% Cleanup
    erlmcp_validator_hooks:disable_reproducer_capture().

no_emit_when_disabled_test() ->
    %% Setup: Disable capture
    erlmcp_validator_hooks:disable_reproducer_capture(),

    %% Get baseline count
    {ok, Before} = erlmcp_reproducer:list_unfixed(),
    BeforeCount = length(Before),

    %% Exercise: Try to emit (should be no-op)
    ok = erlmcp_validator_hooks:emit_protocol_failure(
        <<"TEST_DISABLED">>,
        {ok, valid},
        {error, invalid}
    ),

    %% Verify: No new reproducers created
    {ok, After} = erlmcp_reproducer:list_unfixed(),
    AfterCount = length(After),
    ?assertEqual(BeforeCount, AfterCount).

no_emit_when_reproducer_not_started_test() ->
    %% Setup: Stop reproducer system
    case whereis(erlmcp_reproducer) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end,

    %% Exercise: Try to emit (should gracefully handle)
    ok = erlmcp_validator_hooks:emit_protocol_failure(
        <<"TEST_NO_REPRODUCER">>,
        {ok, valid},
        {error, invalid}
    ),

    %% Verify: No crash (observable behavior)
    ?assert(true).
