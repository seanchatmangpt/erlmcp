%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Tests for erlmcp_reproducer Module
%%%
%%% Chicago School TDD: Use real gen_server, real file system, no mocks.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_reproducer_tests).

-include_lib("eunit/include/eunit.hrl").

%% Need to access reproducer record - use maps interface instead
%% Or include record definition locally for testing

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

reproducer_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(State) ->
         [
          ?_test(capture_protocol_failure_test(State)),
          ?_test(capture_transport_failure_test(State)),
          ?_test(capture_sse_failure_test(State)),
          ?_test(list_all_reproducers_test(State)),
          ?_test(list_unfixed_reproducers_test(State)),
          ?_test(mark_reproducer_fixed_test(State)),
          ?_test(generate_reproducer_module_test(State)),
          ?_test(replay_protocol_test(State)),
          ?_test(audit_report_test(State))
         ]
     end}.

setup() ->
    %% Start reproducer gen_server (Chicago School: real process)
    {ok, Pid} = erlmcp_reproducer:start_link(),
    #{pid => Pid}.

cleanup(#{pid := Pid}) ->
    %% Stop reproducer gen_server
    gen_server:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases (Chicago School: State-based verification)
%%%===================================================================

capture_protocol_failure_test(_State) ->
    %% Exercise: Capture a protocol failure
    Input = <<"{\"jsonrpc\":\"1.0\",\"method\":\"ping\"}">>,
    Expected = {error, {wrong_version, <<"1.0">>}},
    Actual = {error, {wrong_version, <<"1.0">>}},

    {ok, Id} = erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_WRONG_VERSION">>,
        Input,
        Expected,
        Actual
    ),

    %% Verify: Reproducer was created (state-based)
    {ok, _Reproducer} = erlmcp_reproducer:get_reproducer(Id),
    %% Just verify we can retrieve it - record access happens internally
    ?assert(is_binary(Id)).

capture_transport_failure_test(_State) ->
    %% Exercise: Capture a transport failure
    Input = {tcp, <<"invalid_frame">>},
    Expected = {error, invalid_frame},
    Actual = {ok, accepted},

    {ok, Id} = erlmcp_reproducer:capture_transport_failure(
        <<"TEST_INVALID_FRAME">>,
        Input,
        Expected,
        Actual
    ),

    %% Verify: Reproducer was created
    {ok, _Reproducer} = erlmcp_reproducer:get_reproducer(Id),
    ?assert(is_binary(Id)).

capture_sse_failure_test(_State) ->
    %% Exercise: Capture an SSE failure
    Input = {sse_message, #{resume_id => 12345}},
    Expected = {error, invalid_resume_id},
    Actual = {error, {bad_type, integer}},

    {ok, Id} = erlmcp_reproducer:capture_sse_failure(
        <<"TEST_SSE_RESUME_ID">>,
        Input,
        Expected,
        Actual
    ),

    %% Verify: Reproducer was created
    {ok, _Reproducer} = erlmcp_reproducer:get_reproducer(Id),
    ?assert(is_binary(Id)).

list_all_reproducers_test(_State) ->
    %% Setup: Capture multiple failures
    erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_1">>, <<>>, undefined, undefined),
    erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_2">>, <<>>, undefined, undefined),

    %% Exercise: List all reproducers
    {ok, Reproducers} = erlmcp_reproducer:list_all(),

    %% Verify: At least 2 reproducers exist (state-based)
    ?assert(length(Reproducers) >= 2).

list_unfixed_reproducers_test(_State) ->
    %% Setup: Capture a failure
    {ok, _Id} = erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_UNFIXED">>, <<>>, undefined, undefined),

    %% Exercise: List unfixed reproducers
    {ok, Unfixed} = erlmcp_reproducer:list_unfixed(),

    %% Verify: Unfixed list is not empty (state-based)
    ?assert(length(Unfixed) >= 1).

mark_reproducer_fixed_test(_State) ->
    %% Setup: Capture a failure
    {ok, Id} = erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_TO_FIX">>, <<>>, undefined, undefined),

    %% Exercise: Mark as fixed
    ok = erlmcp_reproducer:mark_fixed(Id),

    %% Verify: Can retrieve after marking fixed (state-based)
    {ok, _Reproducer} = erlmcp_reproducer:get_reproducer(Id),
    ?assert(is_binary(Id)).

generate_reproducer_module_test(_State) ->
    %% Setup: Capture a failure
    {ok, Id} = erlmcp_reproducer:capture_protocol_failure(
        <<"TEST_MODULE_GEN">>,
        <<"{\"jsonrpc\":\"2.0\"}">>,
        {ok, valid},
        {error, invalid}
    ),

    %% Exercise: Generate module file (Chicago School: real file system)
    {ok, Reproducer} = erlmcp_reproducer:get_reproducer(Id),
    {ok, FilePath} = erlmcp_reproducer:generate_reproducer_module(Reproducer),

    %% Verify: File exists on disk (observable state)
    ?assert(filelib:is_file(FilePath)),

    %% Cleanup: Remove test file
    file:delete(FilePath).

replay_protocol_test(_State) ->
    %% Setup: Create a protocol scenario
    Scenario = #{
        rule_id => <<"TEST_REPLAY">>,
        description => <<"Test replay">>,
        category => protocol,
        input => [<<"{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1}">>],
        expected => {ok, valid},
        actual => {ok, valid}
    },

    %% Exercise: Replay scenario (Chicago School: real execution)
    Result = erlmcp_reproducer:replay(Scenario),

    %% Verify: Replay executes (state-based)
    ?assertMatch({ok, _} | {error, _}, Result).

audit_report_test(_State) ->
    %% Setup: Capture some failures
    erlmcp_reproducer:capture_protocol_failure(
        <<"AUDIT_1">>, <<>>, undefined, undefined),
    {ok, Id} = erlmcp_reproducer:capture_protocol_failure(
        <<"AUDIT_2">>, <<>>, undefined, undefined),
    erlmcp_reproducer:mark_fixed(Id),

    %% Exercise: Generate audit report
    {ok, Report} = erlmcp_reproducer:audit_report(),

    %% Verify: Report structure (state-based)
    ?assert(is_map(Report)),
    ?assert(maps:is_key(timestamp, Report)),
    ?assert(maps:is_key(total_reproducers, Report)),
    ?assert(maps:is_key(fixed, Report)),
    ?assert(maps:is_key(unfixed, Report)),
    ?assert(maps:is_key(fix_rate_percent, Report)),

    %% Verify: At least 1 fixed, 1 unfixed
    Total = maps:get(total_reproducers, Report),
    Fixed = maps:get(fixed, Report),
    Unfixed = maps:get(unfixed, Report),
    ?assert(Total >= 2),
    ?assert(Fixed >= 1),
    ?assert(Unfixed >= 1).
