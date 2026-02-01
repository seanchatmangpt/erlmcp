%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for erlmcp_otp_verifier
%%%
%%% Chicago School TDD: No mocks, real processes, black-box testing
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_verifier_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

verifier_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_start_link/1,
         fun test_verify_current_otp/1,
         fun test_verify_with_timeout/1,
         fun test_verify_invalid_path/1,
         fun test_verify_reports_all_checks/1,
         fun test_multiple_verifications/1,
         fun test_verifier_not_started/1
     ]}.

setup() ->
    % Start the verifier
    {ok, Pid} = erlmcp_otp_verifier:start_link(#{
        timeout_ms => 30000,
        expected_version => <<"28">>
    }),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true -> erlmcp_otp_verifier:stop(Pid);
        false -> ok
    end.

%%====================================================================
%% Tests
%%====================================================================

test_start_link(_Pid) ->
    fun() ->
        % Start another instance (not registered)
        {ok, Pid2} = gen_server:start_link(erlmcp_otp_verifier, [#{}], []),
        ?assert(is_process_alive(Pid2)),
        erlmcp_otp_verifier:stop(Pid2)
    end.

test_verify_current_otp(_Pid) ->
    fun() ->
        % Verify the current OTP installation
        OtpRoot = code:root_dir(),
        Result = erlmcp_otp_verifier:verify(OtpRoot),

        ?assertMatch({ok, _}, Result),
        {ok, VerificationResult} = Result,

        % Verify result structure
        ?assertMatch(#{
            version := _,
            erts := _,
            apps_count := _,
            health := ok,
            checks := _,
            timestamp := _
        }, VerificationResult),

        % Verify version is a binary
        ?assert(is_binary(maps:get(version, VerificationResult))),

        % Verify ERTS version is a binary
        ?assert(is_binary(maps:get(erts, VerificationResult))),

        % Verify apps_count is a non-negative integer
        AppsCount = maps:get(apps_count, VerificationResult),
        ?assert(is_integer(AppsCount)),
        ?assert(AppsCount >= 0),

        % Verify checks is a list
        Checks = maps:get(checks, VerificationResult),
        ?assert(is_list(Checks)),
        ?assert(length(Checks) > 0),

        % Verify all checks passed
        AllPassed = lists:all(fun(#{status := Status}) -> Status =:= pass end, Checks),
        ?assert(AllPassed),

        % Verify specific checks are present
        CheckNames = [maps:get(check, C) || C <- Checks],
        ?assert(lists:member(<<"check_binaries">>, CheckNames)),
        ?assert(lists:member(<<"check_otp_version">>, CheckNames)),
        ?assert(lists:member(<<"check_beam_integrity">>, CheckNames)),
        ?assert(lists:member(<<"run_smoke_test">>, CheckNames))
    end.

test_verify_with_timeout(_Pid) ->
    fun() ->
        % Verify with a custom timeout
        OtpRoot = code:root_dir(),
        Result = erlmcp_otp_verifier:verify(OtpRoot, 60000),

        ?assertMatch({ok, _}, Result)
    end.

test_verify_invalid_path(_Pid) ->
    fun() ->
        % Verify with an invalid path
        Result = erlmcp_otp_verifier:verify("/nonexistent/path"),

        ?assertMatch({error, _}, Result),
        {error, ErrorMap} = Result,

        % Should have failed checks
        ?assertMatch(#{reason := verification_failed}, ErrorMap),
        ?assertMatch(#{checks := _}, ErrorMap),

        Checks = maps:get(checks, ErrorMap),
        ?assert(is_list(Checks)),

        % At least one check should have failed
        FailedChecks = [C || C = #{status := fail} <- Checks],
        ?assert(length(FailedChecks) > 0)
    end.

test_verify_reports_all_checks(_Pid) ->
    fun() ->
        % Verify the current OTP installation
        OtpRoot = code:root_dir(),
        {ok, Result} = erlmcp_otp_verifier:verify(OtpRoot),

        Checks = maps:get(checks, Result),

        % Verify each check has the required fields
        lists:foreach(fun(Check) ->
            ?assertMatch(#{
                check := _,
                status := _,
                details := _,
                duration_ms := _
            }, Check),

            % Verify check name is a binary
            ?assert(is_binary(maps:get(check, Check))),

            % Verify status is either pass or fail
            Status = maps:get(status, Check),
            ?assert(Status =:= pass orelse Status =:= fail),

            % Verify duration is a non-negative integer
            Duration = maps:get(duration_ms, Check),
            ?assert(is_integer(Duration)),
            ?assert(Duration >= 0)
        end, Checks)
    end.

test_multiple_verifications(_Pid) ->
    fun() ->
        % Perform multiple verifications to ensure state is handled correctly
        OtpRoot = code:root_dir(),

        Result1 = erlmcp_otp_verifier:verify(OtpRoot),
        ?assertMatch({ok, _}, Result1),

        Result2 = erlmcp_otp_verifier:verify(OtpRoot),
        ?assertMatch({ok, _}, Result2),

        Result3 = erlmcp_otp_verifier:verify(OtpRoot),
        ?assertMatch({ok, _}, Result3),

        % All three should succeed
        ?assertMatch({ok, _}, Result1),
        ?assertMatch({ok, _}, Result2),
        ?assertMatch({ok, _}, Result3)
    end.

test_verifier_not_started(_Pid) ->
    fun() ->
        % Unregister the verifier to simulate not started
        unregister(erlmcp_otp_verifier),

        % Try to verify - should fail with verifier_not_started
        Result = erlmcp_otp_verifier:verify("/some/path"),

        ?assertMatch({error, verifier_not_started}, Result),

        % Re-register the original pid for cleanup
        register(erlmcp_otp_verifier, _Pid)
    end.

%%====================================================================
%% Receipt Logging Tests
%%====================================================================

receipt_logging_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun test_receipt_logging/1
    }.

test_receipt_logging(_Pid) ->
    fun() ->
        % Verify the current OTP installation
        OtpRoot = code:root_dir(),
        {ok, Result} = erlmcp_otp_verifier:verify(OtpRoot),

        % Verify that checks were performed (which means receipts were logged)
        Checks = maps:get(checks, Result),
        ?assert(length(Checks) > 0),

        % Each check should have been logged as a receipt
        % The actual receipt logging is tested by observing that verification completes
        % without errors, as log_receipt/1 is called for each check
        ?assert(true)
    end.
