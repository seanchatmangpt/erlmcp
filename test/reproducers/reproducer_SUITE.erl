%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test Suite for Armstrong-Style Reproducers
%%%
%%% Runs all reproducer modules and tracks which failures are fixed.
%%% @end
%%%-------------------------------------------------------------------
-module(reproducer_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    test_sse_invalid_resume_id/1,
    test_jsonrpc_wrong_version/1,
    test_initialize_missing_client_info/1,
    test_invalid_error_code/1,
    test_message_too_large/1,
    test_all_reproducers_discoverable/1,
    test_reproducer_audit_report/1
]).

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
        {group, individual_reproducers},
        {group, reproducer_system}
    ].

groups() ->
    [
        {individual_reproducers, [parallel], [
            test_sse_invalid_resume_id,
            test_jsonrpc_wrong_version,
            test_initialize_missing_client_info,
            test_invalid_error_code,
            test_message_too_large
        ]},
        {reproducer_system, [sequence], [
            test_all_reproducers_discoverable,
            test_reproducer_audit_report
        ]}
    ].

init_per_suite(Config) ->
    %% Start erlmcp application
    application:ensure_all_started(erlmcp_core),

    %% Start reproducer system
    {ok, _Pid} = erlmcp_reproducer:start_link(),

    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp_core),
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases - Individual Reproducers
%%%===================================================================

test_sse_invalid_resume_id(_Config) ->
    %% Run reproducer
    Result = reproducer_20260201_120000_001:run(),

    %% This reproducer demonstrates a known bug
    %% It should fail until bug is fixed
    case Result of
        {ok, fixed} ->
            ct:pal("FIXED: SSE invalid resume-id bug resolved"),
            ok;
        {error, still_failing} ->
            ct:pal("KNOWN BUG: SSE invalid resume-id still failing"),
            ok; % Don't fail test - this is a known issue
        Other ->
            ct:fail("Unexpected reproducer result: ~p", [Other])
    end.

test_jsonrpc_wrong_version(_Config) ->
    %% Run reproducer
    Result = reproducer_20260201_120000_002:run(),

    %% This reproducer should pass - validator correctly rejects wrong version
    {ok, fixed} = Result,
    ok.

test_initialize_missing_client_info(_Config) ->
    %% Run reproducer
    Result = reproducer_20260201_120000_003:run(),

    %% This reproducer demonstrates incomplete validation
    case Result of
        {ok, fixed} ->
            ct:pal("FIXED: Initialize missing clientInfo now properly validated"),
            ok;
        {error, still_failing} ->
            ct:pal("KNOWN BUG: Initialize validation needs improvement"),
            ok; % Don't fail test - this is a known issue
        Other ->
            ct:fail("Unexpected reproducer result: ~p", [Other])
    end.

test_invalid_error_code(_Config) ->
    %% Run reproducer
    Result = reproducer_20260201_120000_004:run(),

    %% This reproducer demonstrates missing error code validation
    case Result of
        {ok, fixed} ->
            ct:pal("FIXED: Invalid error codes now rejected"),
            ok;
        {error, still_failing} ->
            ct:pal("KNOWN BUG: Error code validation too permissive"),
            ok; % Don't fail test - this is a known issue
        Other ->
            ct:fail("Unexpected reproducer result: ~p", [Other])
    end.

test_message_too_large(_Config) ->
    %% Run reproducer
    Result = reproducer_20260201_120000_005:run(),

    %% This reproducer demonstrates missing size validation
    case Result of
        {ok, fixed} ->
            ct:pal("FIXED: Message size limits now enforced"),
            ok;
        {error, still_failing} ->
            ct:pal("KNOWN BUG: Message size validation needed"),
            ok; % Don't fail test - this is a known issue
        Other ->
            ct:fail("Unexpected reproducer result: ~p", [Other])
    end.

%%%===================================================================
%%% Test Cases - Reproducer System
%%%===================================================================

test_all_reproducers_discoverable(_Config) ->
    %% List all reproducers
    {ok, Reproducers} = erlmcp_reproducer:list_all(),

    %% Should have at least the 5 example reproducers
    true = length(Reproducers) >= 5,

    ct:pal("Found ~p reproducers", [length(Reproducers)]),
    ok.

test_reproducer_audit_report(_Config) ->
    %% Generate audit report
    {ok, Report} = erlmcp_reproducer:audit_report(),

    %% Verify report structure
    true = is_map(Report),
    true = maps:is_key(timestamp, Report),
    true = maps:is_key(total_reproducers, Report),
    true = maps:is_key(fixed, Report),
    true = maps:is_key(unfixed, Report),
    true = maps:is_key(fix_rate_percent, Report),

    %% Log report
    ct:pal("Reproducer Audit Report: ~p", [Report]),

    %% Verify we have some reproducers
    Total = maps:get(total_reproducers, Report),
    true = Total >= 5,

    ok.
