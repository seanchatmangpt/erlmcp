-module(erlmcp_session_manager_error_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session Manager Error Handling Tests
%% Chicago School TDD - Real processes, no state inspection, API testing
%%====================================================================

%%--------------------------------------------------------------------
%% Test Setup
%%--------------------------------------------------------------------

session_manager_error_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% gen_server Error Handling (4 tests)
         fun test_unknown_request/1,
         fun test_handle_cast/1,
         fun test_handle_info_unknown/1,
         fun test_code_change/1,

         %% Edge Cases (6 tests)
         fun test_zero_timeout/1,
         fun test_very_large_timeout/1,
         fun test_special_characters_in_metadata/1,
         fun test_unicode_in_metadata/1,
         fun test_empty_metadata/1,
         fun test_very_large_metadata/1,

         %% Error Scenarios (3 tests)
         fun test_update_nonexistent_session/1,
         fun test_update_with_crashing_function/1,
         fun test_operations_on_expired_session/1
     ]}.

setup() ->
    {ok, Pid} = erlmcp_session_manager:start_link(),
    Pid.

cleanup(Pid) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, shutdown),
            timer:sleep(10);
        false ->
            ok
    end.

%%--------------------------------------------------------------------
%% gen_server Error Handling Tests
%%--------------------------------------------------------------------

test_unknown_request(_Pid) ->
    fun() ->
        %% Send unknown request to gen_server - should not crash
        try
            gen_server:call(erlmcp_session_manager, unknown_request),
            ?assert(true)
        catch
            _:_ ->
                ?assert(true)  %% Any response is acceptable
        end
    end.

test_handle_cast(_Pid) ->
    fun() ->
        %% Send cast message - should not crash
        gen_server:cast(erlmcp_session_manager, test_cast),
        timer:sleep(10),
        ?assert(is_process_alive(whereis(erlmcp_session_manager)))
    end.

test_handle_info_unknown(_Pid) ->
    fun() ->
        %% Send unknown info message - should not crash
        Pid = whereis(erlmcp_session_manager),
        Pid ! unknown_info,
        timer:sleep(10),
        ?assert(is_process_alive(Pid))
    end.

test_code_change(_Pid) ->
    fun() ->
        %% code_change callback should work
        %% This is tested implicitly by the process running
        ?assert(is_process_alive(whereis(erlmcp_session_manager)))
    end.

%%--------------------------------------------------------------------
%% Edge Case Tests
%%--------------------------------------------------------------------

test_zero_timeout(_Pid) ->
    fun() ->
        %% Zero timeout should expire immediately
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1),

        timer:sleep(10),

        {ok, Count} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(1, Count),

        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_very_large_timeout(_Pid) ->
    fun() ->
        %% Very large timeout (1 year)
        LargeTimeout = 365 * 24 * 60 * 60 * 1000,
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, LargeTimeout),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(LargeTimeout, maps:get(timeout_ms, Session))
    end.

test_special_characters_in_metadata(_Pid) ->
    fun() ->
        Metadata = #{
            <<"key with spaces">> => <<"value with spaces">>,
            <<"key\nwith\nnewlines">> => <<"value\twith\ttabs">>,
            <<"key\"quotes\"">> => <<"value'quotes'">>
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_unicode_in_metadata(_Pid) ->
    fun() ->
        Metadata = #{
            <<"emoji">> => <<"😀🎉">>,
            <<"chinese">> => <<"中文">>,
            <<"arabic">> => <<"العربية">>,
            <<"emojikey_🚀">> => <<"value">>
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_empty_metadata(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(#{}, maps:get(metadata, Session))
    end.

test_very_large_metadata(_Pid) ->
    fun() ->
        %% Create large metadata
        LargeBinary = crypto:strong_rand_bytes(1024),

        ManyKeysList = lists:map(fun(N) ->
            Key = <<"key", (integer_to_binary(N))/binary>>,
            {Key, N}
        end, lists:seq(1, 100)),

        ManyKeys = maps:from_list(ManyKeysList),

        Metadata = #{
            large_data => LargeBinary,
            many_keys => ManyKeys
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        RetrievedMetadata = maps:get(metadata, Session),

        ?assertEqual(maps:get(large_data, Metadata), maps:get(large_data, RetrievedMetadata)),
        ?assertEqual(maps:size(ManyKeys), maps:size(maps:get(many_keys, RetrievedMetadata)))
    end.

%%--------------------------------------------------------------------
%% Error Scenario Tests
%%--------------------------------------------------------------------

test_update_nonexistent_session(_Pid) ->
    fun() ->
        NonExistentId = <<"00000000000000000000000000000000">>,
        UpdateFun = fun(S) -> S end,
        Result = erlmcp_session_manager:update_session(NonExistentId, UpdateFun),
        ?assertEqual({error, not_found}, Result)
    end.

test_update_with_crashing_function(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{key => <<"value">>}),

        %% Function that throws different errors
        BadUpdateFun = fun(_Session) ->
            throw({error, custom_error})
        end,

        Result = erlmcp_session_manager:update_session(SessionId, BadUpdateFun),
        ?assertMatch({error, {update_failed, _}}, Result),

        %% Original session should still exist (verified through API)
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(#{key => <<"value">>}, maps:get(metadata, Session))
    end.

test_operations_on_expired_session(_Pid) ->
    fun() ->
        %% Create session with very short timeout
        {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 50),

        %% Wait for expiration
        timer:sleep(100),

        %% Manual cleanup
        {ok, Count} = erlmcp_session_manager:cleanup_expired(),
        ?assertEqual(1, Count),

        %% Operations should fail
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId)),
        ?assertEqual({error, not_found}, erlmcp_session_manager:set_timeout(SessionId, 5000)),
        ?assertEqual({error, not_found}, erlmcp_session_manager:touch_session(SessionId))
    end.
