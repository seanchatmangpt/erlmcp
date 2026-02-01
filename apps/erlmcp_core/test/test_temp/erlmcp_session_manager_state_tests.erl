-module(erlmcp_session_manager_state_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session State Transition Tests
%% Chicago School TDD - Real processes, no state inspection, API testing
%%====================================================================

%%--------------------------------------------------------------------
%% Test Setup
%%--------------------------------------------------------------------

session_manager_state_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [%% Update Operations (5 tests)
      fun test_update_session/1,
      fun test_update_with_function/1,
      fun test_update_with_invalid_function/1,
      fun test_update_creates_new_map/1,
      fun test_metadata_preserved_through_update/1,
      %% Timeout Operations (4 tests)
      fun test_set_timeout/1,
      fun test_set_timeout_to_infinity/1,
      fun test_set_timeout_from_infinity/1,
      fun test_set_timeout_nonexistent_session/1,
      %% Touch Operations (3 tests)
      fun test_touch_session/1,
      fun test_touch_preserves_metadata/1,
      fun test_touch_nonexistent_session/1]}.

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
%% Update Operation Tests
%%--------------------------------------------------------------------

test_update_session(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0}),

       %% Update session with function
       UpdateFun =
           fun(Session) ->
              Metadata = maps:get(metadata, Session),
              Counter = maps:get(counter, Metadata),
              NewMetadata = Metadata#{counter => Counter + 1},
              Session#{metadata => NewMetadata}
           end,

       ?assertEqual(ok, erlmcp_session_manager:update_session(SessionId, UpdateFun)),

       %% Verify update through API
       {ok, UpdatedSession} = erlmcp_session_manager:get_session(SessionId),
       UpdatedMetadata = maps:get(metadata, UpdatedSession),
       ?assertEqual(1, maps:get(counter, UpdatedMetadata))
    end.

test_update_with_function(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0, values => []}),

       %% Multiple updates
       lists:foreach(fun(N) ->
                        UpdateFun =
                            fun(Session) ->
                               Meta = maps:get(metadata, Session),
                               Counter = maps:get(counter, Meta),
                               Values = maps:get(values, Meta),
                               NewMeta = Meta#{counter => Counter + 1, values => [N | Values]},
                               Session#{metadata => NewMeta}
                            end,
                        erlmcp_session_manager:update_session(SessionId, UpdateFun)
                     end,
                     lists:seq(1, 10)),

       %% Verify final state through API
       {ok, FinalSession} = erlmcp_session_manager:get_session(SessionId),
       FinalMeta = maps:get(metadata, FinalSession),
       ?assertEqual(10, maps:get(counter, FinalMeta)),
       ?assertEqual(10, length(maps:get(values, FinalMeta)))
    end.

test_update_with_invalid_function(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{key => <<"value">>}),

       %% Function that throws an error
       BadUpdateFun = fun(_Session) -> error(deliberate_error) end,

       Result = erlmcp_session_manager:update_session(SessionId, BadUpdateFun),
       ?assertMatch({error, {update_failed, _}}, Result),

       %% Original session should still exist (verified through API)
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{key => <<"value">>}, maps:get(metadata, Session))
    end.

test_update_creates_new_map(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{old => <<"data">>}),

       %% Update function that changes entire map
       UpdateFun = fun(Session) -> Session#{metadata => #{new => <<"data">>}} end,

       erlmcp_session_manager:update_session(SessionId, UpdateFun),

       {ok, Updated} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(#{new => <<"data">>}, maps:get(metadata, Updated))
    end.

test_metadata_preserved_through_update(_Pid) ->
    fun() ->
       OriginalMetadata = #{key => <<"value">>, counter => 0},
       {ok, SessionId} = erlmcp_session_manager:create_session(OriginalMetadata),

       %% Update that modifies metadata
       UpdateFun =
           fun(Session) ->
              Meta = maps:get(metadata, Session),
              Counter = maps:get(counter, Meta),
              Session#{metadata => Meta#{counter => Counter + 1}}
           end,

       erlmcp_session_manager:update_session(SessionId, UpdateFun),

       {ok, Updated} = erlmcp_session_manager:get_session(SessionId),
       UpdatedMeta = maps:get(metadata, Updated),
       ?assertEqual(<<"value">>, maps:get(key, UpdatedMeta)),
       ?assertEqual(1, maps:get(counter, UpdatedMeta))
    end.

%%--------------------------------------------------------------------
%% Timeout Operation Tests
%%--------------------------------------------------------------------

test_set_timeout(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1000),

       %% Update timeout
       NewTimeout = 5000,
       ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, NewTimeout)),

       %% Verify timeout updated through API
       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(NewTimeout, maps:get(timeout_ms, Session))
    end.

test_set_timeout_to_infinity(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, 1000),

       ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, infinity)),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(infinity, maps:get(timeout_ms, Session))
    end.

test_set_timeout_from_infinity(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}, infinity),

       ?assertEqual(ok, erlmcp_session_manager:set_timeout(SessionId, 5000)),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(5000, maps:get(timeout_ms, Session))
    end.

test_set_timeout_nonexistent_session(_Pid) ->
    fun() ->
       NonExistentId = <<"00000000000000000000000000000000">>,
       Result = erlmcp_session_manager:set_timeout(NonExistentId, 5000),
       ?assertEqual({error, not_found}, Result)
    end.

%%--------------------------------------------------------------------
%% Touch Operation Tests
%%--------------------------------------------------------------------

test_touch_session(_Pid) ->
    fun() ->
       {ok, SessionId} = erlmcp_session_manager:create_session(#{}),
       {ok, OriginalSession} = erlmcp_session_manager:get_session(SessionId),
       OriginalAccessed = maps:get(last_accessed, OriginalSession),

       %% Wait a bit
       timer:sleep(10),

       %% Touch session
       ?assertEqual(ok, erlmcp_session_manager:touch_session(SessionId)),

       %% Verify last_accessed updated through API
       {ok, TouchedSession} = erlmcp_session_manager:get_session(SessionId),
       TouchedAccessed = maps:get(last_accessed, TouchedSession),
       ?assert(TouchedAccessed > OriginalAccessed)
    end.

test_touch_preserves_metadata(_Pid) ->
    fun() ->
       Metadata = #{key => <<"value">>, counter => 42},
       {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

       erlmcp_session_manager:touch_session(SessionId),

       {ok, Session} = erlmcp_session_manager:get_session(SessionId),
       ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_touch_nonexistent_session(_Pid) ->
    fun() ->
       NonExistentId = <<"00000000000000000000000000000000">>,
       Result = erlmcp_session_manager:touch_session(NonExistentId),
       ?assertEqual({error, not_found}, Result)
    end.
