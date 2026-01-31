-module(erlmcp_session_manager_client_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Session Manager Client & Integration Tests
%% Chicago School TDD - Real processes, no state inspection, API testing
%%====================================================================

%%--------------------------------------------------------------------
%% Test Setup
%%--------------------------------------------------------------------

session_manager_client_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Client Integration (3 tests)
         fun test_session_with_client_association/1,
         fun test_multiple_clients_isolated/1,
         fun test_client_lifecycle_integration/1,

         %% Metadata Handling (3 tests)
         fun test_complex_nested_metadata/1,
         fun test_metadata_with_binary_keys/1,
         fun test_various_metadata_types/1,

         %% List & Filter (4 tests)
         fun test_list_sessions_with_filter/1,
         fun test_list_sessions_with_complex_filter/1,
         fun test_list_sessions_filter_all_match/1,
         fun test_list_sessions_filter_none_match/1,

         %% Concurrency (5 tests)
         fun test_concurrent_session_creation/1,
         fun test_concurrent_get_operations/1,
         fun test_concurrent_update_operations/1,
         fun test_concurrent_delete_operations/1,
         fun test_concurrent_mixed_operations/1,

         %% Session Lifecycle Integration (3 tests)
         fun test_multiple_sessions/1,
         fun test_session_lifecycle/1,
         fun test_bulk_operations/1
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
%% Client Integration Tests
%%--------------------------------------------------------------------

test_session_with_client_association(_Pid) ->
    fun() ->
        %% Create session with client metadata
        ClientMetadata = #{
            client_id => <<"client_123">>,
            transport => stdio,
            connected_at => erlang:system_time(millisecond)
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(ClientMetadata),

        %% Verify session with client info
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(<<"client_123">>, maps:get(client_id, maps:get(metadata, Session))),

        %% Update session with client activity
        UpdateFun = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{last_activity => erlang:system_time(millisecond)}}
        end,
        ok = erlmcp_session_manager:update_session(SessionId, UpdateFun),

        {ok, Updated} = erlmcp_session_manager:get_session(SessionId),
        ?assert(maps:is_key(last_activity, maps:get(metadata, Updated)))
    end.

test_multiple_clients_isolated(_Pid) ->
    fun() ->
        %% Create sessions for different clients
        {ok, Id1} = erlmcp_session_manager:create_session(#{
            client_id => <<"client_1">>,
            user => alice
        }),
        {ok, Id2} = erlmcp_session_manager:create_session(#{
            client_id => <<"client_2">>,
            user => bob
        }),

        %% Update client 1 session
        UpdateFun = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{updated => true}}
        end,
        erlmcp_session_manager:update_session(Id1, UpdateFun),

        %% Client 2 session should be unchanged
        {ok, Session2} = erlmcp_session_manager:get_session(Id2),
        Meta2 = maps:get(metadata, Session2),
        ?assertEqual(<<"client_2">>, maps:get(client_id, Meta2)),
        ?assertEqual(bob, maps:get(user, Meta2)),
        ?assertNot(maps:is_key(updated, Meta2))
    end.

test_client_lifecycle_integration(_Pid) ->
    fun() ->
        %% Simulate client lifecycle
        {ok, SessionId} = erlmcp_session_manager:create_session(#{
            client_id => <<"client_lifecycle">>,
            state => connecting
        }),

        %% Client connects
        UpdateFun = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{state => connected}}
        end,
        erlmcp_session_manager:update_session(SessionId, UpdateFun),

        %% Client active
        {ok, Session1} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(connected, maps:get(state, maps:get(metadata, Session1))),

        %% Client disconnects
        UpdateFun2 = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{state => disconnected}}
        end,
        erlmcp_session_manager:update_session(SessionId, UpdateFun2),

        %% Verify final state
        {ok, Session2} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(disconnected, maps:get(state, maps:get(metadata, Session2))),

        %% Cleanup
        ok = erlmcp_session_manager:delete_session(SessionId),
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

%%--------------------------------------------------------------------
%% Metadata Handling Tests
%%--------------------------------------------------------------------

test_complex_nested_metadata(_Pid) ->
    fun() ->
        Metadata = #{
            level1 => #{
                level2 => #{
                    level3 => #{
                        value => <<"deep">>
                    }
                }
            }
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_metadata_with_binary_keys(_Pid) ->
    fun() ->
        Metadata = #{
            <<"binary_key">> => <<"value">>,
            <<"user_id">> => 12345,
            <<"active">> => true
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(Metadata, maps:get(metadata, Session))
    end.

test_various_metadata_types(_Pid) ->
    fun() ->
        %% Test various metadata types
        Metadata = #{
            binary_key => <<"binary_value">>,
            atom_key => atom_value,
            integer_key => 42,
            list_key => [1, 2, 3],
            map_key => #{nested => true},
            bool_key => true,
            float_key => 3.14
        },

        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),

        RetrievedMetadata = maps:get(metadata, Session),
        ?assertEqual(Metadata, RetrievedMetadata)
    end.

%%--------------------------------------------------------------------
%% List & Filter Tests
%%--------------------------------------------------------------------

test_list_sessions_with_filter(_Pid) ->
    fun() ->
        %% Create sessions with different metadata
        {ok, _Id1} = erlmcp_session_manager:create_session(#{type => admin}),
        {ok, _Id2} = erlmcp_session_manager:create_session(#{type => user}),
        {ok, _Id3} = erlmcp_session_manager:create_session(#{type => admin}),

        %% Filter admin sessions
        FilterFun = fun(Session) ->
            Metadata = maps:get(metadata, Session),
            maps:get(type, Metadata, undefined) =:= admin
        end,

        AdminSessions = erlmcp_session_manager:list_sessions(FilterFun),
        ?assertEqual(2, length(AdminSessions))
    end.

test_list_sessions_with_complex_filter(_Pid) ->
    fun() ->
        %% Create sessions with various attributes
        {ok, _} = erlmcp_session_manager:create_session(#{role => admin, active => true}),
        {ok, _} = erlmcp_session_manager:create_session(#{role => user, active => true}),
        {ok, _} = erlmcp_session_manager:create_session(#{role => admin, active => false}),
        {ok, _} = erlmcp_session_manager:create_session(#{role => user, active => false}),

        %% Filter: active admins only
        FilterFun = fun(Session) ->
            Metadata = maps:get(metadata, Session),
            maps:get(role, Metadata, undefined) =:= admin andalso
            maps:get(active, Metadata, false) =:= true
        end,

        Result = erlmcp_session_manager:list_sessions(FilterFun),
        ?assertEqual(1, length(Result))
    end.

test_list_sessions_filter_all_match(_Pid) ->
    fun() ->
        {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
        {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
        {ok, _} = erlmcp_session_manager:create_session(#{type => user}),

        %% Filter that matches all
        FilterFun = fun(_Session) -> true end,

        Result = erlmcp_session_manager:list_sessions(FilterFun),
        ?assertEqual(3, length(Result))
    end.

test_list_sessions_filter_none_match(_Pid) ->
    fun() ->
        {ok, _} = erlmcp_session_manager:create_session(#{type => user}),
        {ok, _} = erlmcp_session_manager:create_session(#{type => admin}),

        %% Filter that matches none
        FilterFun = fun(_Session) -> false end,

        Result = erlmcp_session_manager:list_sessions(FilterFun),
        ?assertEqual(0, length(Result))
    end.

%%--------------------------------------------------------------------
%% Concurrency Tests
%%--------------------------------------------------------------------

test_concurrent_session_creation(_Pid) ->
    fun() ->
        Parent = self(),
        NumProcesses = 10,

        %% Spawn concurrent session creators
        Pids = [spawn(fun() ->
            {ok, SessionId} = erlmcp_session_manager:create_session(#{index => N}),
            Parent ! {session_created, SessionId}
        end) || N <- lists:seq(1, NumProcesses)],

        %% Collect results
        SessionIds = [receive
            {session_created, Id} -> Id
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        %% All IDs should be unique
        ?assertEqual(NumProcesses, length(lists:usort(SessionIds)))
    end.

test_concurrent_get_operations(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{test => <<"concurrent">>}),
        Parent = self(),
        NumProcesses = 20,

        %% Spawn concurrent readers
        Pids = [spawn(fun() ->
            Result = erlmcp_session_manager:get_session(SessionId),
            Parent ! {get_result, Result}
        end) || _ <- lists:seq(1, NumProcesses)],

        %% All should succeed
        Results = [receive
            {get_result, R} -> R
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        SuccessCount = length([1 || {ok, _} <- Results]),
        ?assertEqual(NumProcesses, SuccessCount)
    end.

test_concurrent_update_operations(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0}),
        Parent = self(),
        NumProcesses = 10,

        %% Spawn concurrent updaters
        Pids = [spawn(fun() ->
            UpdateFun = fun(S) ->
                Meta = maps:get(metadata, S),
                Counter = maps:get(counter, Meta),
                S#{metadata => Meta#{counter => Counter + 1}}
            end,
            Result = erlmcp_session_manager:update_session(SessionId, UpdateFun),
            Parent ! {update_result, Result}
        end) || _ <- lists:seq(1, NumProcesses)],

        %% All should succeed
        Results = [receive
            {update_result, R} -> R
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        SuccessCount = length([R || ok = R <- Results]),
        ?assertEqual(NumProcesses, SuccessCount),

        %% Final counter should be NumProcesses
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        Meta = maps:get(metadata, Session),
        ?assertEqual(NumProcesses, maps:get(counter, Meta))
    end.

test_concurrent_delete_operations(_Pid) ->
    fun() ->
        %% Create multiple sessions
        SessionIds = [begin
            {ok, Id} = erlmcp_session_manager:create_session(#{index => N}),
            Id
        end || N <- lists:seq(1, 10)],

        Parent = self(),

        %% Spawn concurrent deleters
        Pids = [spawn(fun() ->
            Result = erlmcp_session_manager:delete_session(Id),
            Parent ! {delete_result, Result}
        end) || Id <- SessionIds],

        %% All should succeed
        Results = [receive
            {delete_result, R} -> R
        after 5000 ->
            error(timeout)
        end || _ <- Pids],

        SuccessCount = length([R || ok = R <- Results]),
        ?assertEqual(10, SuccessCount)
    end.

test_concurrent_mixed_operations(_Pid) ->
    fun() ->
        {ok, SessionId} = erlmcp_session_manager:create_session(#{counter => 0, value => <<"test">>}),
        Parent = self(),

        %% Spawn mixed operations
        Pids = [
            spawn(fun() ->
                [erlmcp_session_manager:get_session(SessionId) || _ <- lists:seq(1, 5)],
                Parent ! {ops_complete, get}
            end),
            spawn(fun() ->
                UpdateFun = fun(S) -> S end,
                [erlmcp_session_manager:update_session(SessionId, UpdateFun) || _ <- lists:seq(1, 5)],
                Parent ! {ops_complete, update}
            end),
            spawn(fun() ->
                [erlmcp_session_manager:touch_session(SessionId) || _ <- lists:seq(1, 5)],
                Parent ! {ops_complete, touch}
            end)
        ],

        %% All should complete
        lists:foreach(fun(_) ->
            receive
                {ops_complete, _} -> ok
            after 5000 ->
                error(timeout)
            end
        end, Pids),

        %% Session should still be valid
        ?assertMatch({ok, _}, erlmcp_session_manager:get_session(SessionId))
    end.

%%--------------------------------------------------------------------
%% Session Lifecycle Integration Tests
%%--------------------------------------------------------------------

test_multiple_sessions(_Pid) ->
    fun() ->
        %% Create sessions with different configurations
        {ok, Id1} = erlmcp_session_manager:create_session(#{user => <<"alice">>}, 5000),
        {ok, Id2} = erlmcp_session_manager:create_session(#{user => <<"bob">>}, 10000),
        {ok, Id3} = erlmcp_session_manager:create_session(#{user => <<"charlie">>}, infinity),

        %% Update session 1
        UpdateFun = fun(S) ->
            Meta = maps:get(metadata, S),
            S#{metadata => Meta#{updated => true}}
        end,
        erlmcp_session_manager:update_session(Id1, UpdateFun),

        %% Touch session 2
        erlmcp_session_manager:touch_session(Id2),

        %% Delete session 3
        erlmcp_session_manager:delete_session(Id3),

        %% List remaining sessions
        Sessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(2, length(Sessions)),

        %% Verify session 1 updated
        {ok, S1} = erlmcp_session_manager:get_session(Id1),
        Meta1 = maps:get(metadata, S1),
        ?assertEqual(true, maps:get(updated, Meta1)),

        %% Verify session 3 deleted
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(Id3))
    end.

test_session_lifecycle(_Pid) ->
    fun() ->
        %% Complete lifecycle: create -> update -> read -> delete
        Metadata = #{stage => initial},
        {ok, SessionId} = erlmcp_session_manager:create_session(Metadata),

        %% Update
        UpdateFun = fun(S) ->
            S#{metadata => #{stage => updated}}
        end,
        ?assertEqual(ok, erlmcp_session_manager:update_session(SessionId, UpdateFun)),

        %% Read
        {ok, Session} = erlmcp_session_manager:get_session(SessionId),
        ?assertEqual(#{stage => updated}, maps:get(metadata, Session)),

        %% Delete
        ?assertEqual(ok, erlmcp_session_manager:delete_session(SessionId)),

        %% Verify gone
        ?assertEqual({error, not_found}, erlmcp_session_manager:get_session(SessionId))
    end.

test_bulk_operations(_Pid) ->
    fun() ->
        %% Create many sessions
        NumSessions = 100,
        lists:foreach(fun(N) ->
            {ok, _} = erlmcp_session_manager:create_session(#{index => N})
        end, lists:seq(1, NumSessions)),

        %% List all
        AllSessions = erlmcp_session_manager:list_sessions(),
        ?assertEqual(NumSessions, length(AllSessions)),

        %% Filter
        EvenFilter = fun(S) ->
            Meta = maps:get(metadata, S),
            case maps:get(index, Meta, undefined) of
                N when N rem 2 =:= 0 -> true;
                _ -> false
            end
        end,

        EvenSessions = erlmcp_session_manager:list_sessions(EvenFilter),
        ?assertEqual(NumSessions div 2, length(EvenSessions))
    end.
