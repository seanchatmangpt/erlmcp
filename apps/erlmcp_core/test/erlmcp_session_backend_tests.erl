-module(erlmcp_session_backend_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite for Session Backend Behavior
%% Chicago School TDD - Real backends, no mocks
%%====================================================================

%%====================================================================
%% ETS Backend Tests
%%====================================================================

ets_backend_test_() ->
    {setup,
     fun setup_ets/0,
     fun cleanup_ets/1,
     fun(_) -> [
         ?_test(test_ets_store_and_fetch()),
         ?_test(test_ets_update_last_accessed()),
         ?_test(test_ets_delete()),
         ?_test(test_ets_list()),
         ?_test(test_ets_cleanup_expired()),
         ?_test(test_ets_concurrent_access())
     ] end}.

setup_ets() ->
    {ok, State} = erlmcp_session_ets:init(#{}),
    State.

cleanup_ets(_State) ->
    ets:delete(erlmcp_sessions_ets),
    ok.

test_ets_store_and_fetch() ->
    State = setup_ets(),
    SessionId = <<"session_ets_1">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => 3600000,
        metadata => #{backend => ets}
    },
    ?assertEqual({ok, State}, erlmcp_session_ets:store(SessionId, Session, State)),
    ?assertMatch({ok, StoredSession, _}, erlmcp_session_ets:fetch(SessionId, State)),
    ?assertEqual(SessionId, maps:get(id, element(2, element(2, erlmcp_session_ets:fetch(SessionId, State))))),
    cleanup_ets(State).

test_ets_update_last_accessed() ->
    State = setup_ets(),
    SessionId = <<"session_ets_2">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond) - 10000,
        last_accessed => erlang:system_time(millisecond) - 10000,
        timeout_ms => infinity,
        metadata => #{}
    },
    {ok, State1} = erlmcp_session_ets:store(SessionId, Session, State),
    timer:sleep(10),  % Small delay
    {ok, FetchedSession, _} = erlmcp_session_ets:fetch(SessionId, State1),
    ?assert(maps:get(last_accessed, FetchedSession) > maps:get(last_accessed, Session)),
    cleanup_ets(State1).

test_ets_delete() ->
    State = setup_ets(),
    SessionId = <<"session_ets_3">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => infinity,
        metadata => #{}
    },
    {ok, State1} = erlmcp_session_ets:store(SessionId, Session, State),
    ?assertEqual({ok, State1}, erlmcp_session_ets:delete(SessionId, State1)),
    ?assertEqual({error, not_found, State1}, erlmcp_session_ets:fetch(SessionId, State1)),
    cleanup_ets(State1).

test_ets_list() ->
    State = setup_ets(),
    Sessions = [
        {<<"session_ets_list_1">>, #{id => <<"session_ets_list_1">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}},
        {<<"session_ets_list_2">>, #{id => <<"session_ets_list_2">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}},
        {<<"session_ets_list_3">>, #{id => <<"session_ets_list_3">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_ets:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, SessionIds, _} = erlmcp_session_ets:list(State1),
    ?assertEqual(3, length(SessionIds)),
    ?assert(lists:member(<<"session_ets_list_1">>, SessionIds)),
    ?assert(lists:member(<<"session_ets_list_2">>, SessionIds)),
    ?assert(lists:member(<<"session_ets_list_3">>, SessionIds)),
    cleanup_ets(State1).

test_ets_cleanup_expired() ->
    State = setup_ets(),
    Now = erlang:system_time(millisecond),
    Sessions = [
        {<<"expired_1">>, #{id => <<"expired_1">>, created_at => Now - 10000, last_accessed => Now - 5000, timeout_ms => 1000, metadata => #{}}},
        {<<"valid_1">>, #{id => <<"valid_1">>, created_at => Now - 10000, last_accessed => Now - 500, timeout_ms => 10000, metadata => #{}}},
        {<<"expired_2">>, #{id => <<"expired_2">>, created_at => Now - 10000, last_accessed => Now - 3000, timeout_ms => 2000, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_ets:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, Count, _} = erlmcp_session_ets:cleanup_expired(State1),
    ?assertEqual(2, Count),
    ?assertEqual({error, not_found, State1}, erlmcp_session_ets:fetch(<<"expired_1">>, State1)),
    ?assertMatch({ok, _, _}, erlmcp_session_ets:fetch(<<"valid_1">>, State1)),
    ?assertEqual({error, not_found, State1}, erlmcp_session_ets:fetch(<<"expired_2">>, State1)),
    cleanup_ets(State1).

test_ets_concurrent_access() ->
    State = setup_ets(),
    SessionId = <<"concurrent_ets">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => infinity,
        metadata => #{counter => 0}
    },
    {ok, State1} = erlmcp_session_ets:store(SessionId, Session, State),

    %% Spawn multiple processes to read/update concurrently
    Parent = self(),
    Pids = [spawn(fun() ->
        {ok, _S, _} = erlmcp_session_ets:fetch(SessionId, State1),
        Parent ! done
    end) || _ <- lists:seq(1, 10)],

    %% Wait for all to complete
    timer:sleep(100),
    ?assertEqual(10, length([receive done -> done end || _ <- Pids])),
    cleanup_ets(State1).

%%====================================================================
%% DETS Backend Tests
%%====================================================================

dets_backend_test_() ->
    {setup,
     fun setup_dets/0,
     fun cleanup_dets/1,
     fun(_) -> [
         ?_test(test_dets_store_and_fetch()),
         ?_test(test_dets_persistence()),
         ?_test(test_dets_delete()),
         ?_test(test_dets_list()),
         ?_test(test_dets_cleanup_expired())
     ] end}.

setup_dets() ->
    {ok, State} = erlmcp_session_dets:init(#{
        table_name => erlmcp_sessions_dets_test,
        file_path => "test_sessions.dets"
    }),
    State.

cleanup_dets(_State) ->
    dets:close(erlmcp_sessions_dets_test),
    file:delete("test_sessions.dets"),
    ok.

test_dets_store_and_fetch() ->
    State = setup_dets(),
    SessionId = <<"session_dets_1">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => 3600000,
        metadata => #{backend => dets}
    },
    ?assertEqual({ok, State}, erlmcp_session_dets:store(SessionId, Session, State)),
    ?assertMatch({ok, _, _}, erlmcp_session_dets:fetch(SessionId, State)),
    cleanup_dets(State).

test_dets_persistence() ->
    State = setup_dets(),
    SessionId = <<"session_dets_persist">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => infinity,
        metadata => #{persistent => true}
    },
    {ok, State1} = erlmcp_session_dets:store(SessionId, Session, State),

    %% Close and reopen to test persistence
    cleanup_dets(State1),
    timer:sleep(100),
    NewState = setup_dets(),

    ?assertMatch({ok, _, _}, erlmcp_session_dets:fetch(SessionId, NewState)),
    cleanup_dets(NewState).

test_dets_delete() ->
    State = setup_dets(),
    SessionId = <<"session_dets_del">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => infinity,
        metadata => #{}
    },
    {ok, State1} = erlmcp_session_dets:store(SessionId, Session, State),
    ?assertEqual({ok, State1}, erlmcp_session_dets:delete(SessionId, State1)),
    ?assertEqual({error, not_found, State1}, erlmcp_session_dets:fetch(SessionId, State1)),
    cleanup_dets(State1).

test_dets_list() ->
    State = setup_dets(),
    Sessions = [
        {<<"dets_list_1">>, #{id => <<"dets_list_1">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}},
        {<<"dets_list_2">>, #{id => <<"dets_list_2">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_dets:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, SessionIds, _} = erlmcp_session_dets:list(State1),
    ?assertEqual(2, length(SessionIds)),
    cleanup_dets(State1).

test_dets_cleanup_expired() ->
    State = setup_dets(),
    Now = erlang:system_time(millisecond),
    Sessions = [
        {<<"expired_dets_1">>, #{id => <<"expired_dets_1">>, created_at => Now - 10000, last_accessed => Now - 5000, timeout_ms => 1000, metadata => #{}}},
        {<<"valid_dets_1">>, #{id => <<"valid_dets_1">>, created_at => Now - 10000, last_accessed => Now - 500, timeout_ms => 10000, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_dets:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, Count, _} = erlmcp_session_dets:cleanup_expired(State1),
    ?assertEqual(1, Count),
    ?assertEqual({error, not_found, State1}, erlmcp_session_dets:fetch(<<"expired_dets_1">>, State1)),
    ?assertMatch({ok, _, _}, erlmcp_session_dets:fetch(<<"valid_dets_1">>, State1)),
    cleanup_dets(State1).

%%====================================================================
%% Mnesia Backend Tests
%%====================================================================

mnesia_backend_test_() ->
    {setup,
     fun setup_mnesia/0,
     fun cleanup_mnesia/1,
     fun(_) -> [
         ?_test(test_mnesia_store_and_fetch()),
         ?_test(test_mnesia_delete()),
         ?_test(test_mnesia_list()),
         ?_test(test_mnesia_cleanup_expired())
     ] end}.

setup_mnesia() ->
    %% Stop Mnesia if running and delete any existing schema
    application:stop(mnesia),
    case node() of
        'nonode@nohost' ->
            %% Single-node mode: delete Mnesia directory if it exists
            mnesia:delete_schema([node()]);
        _ ->
            %% Named node: delete schema properly
            mnesia:delete_schema([node()])
    end,

    %% Create schema for testing (only if not nonode@nohost)
    %% disc_copies requires a named node, but we use ram_copies for testing
    case node() of
        'nonode@nohost' ->
            %% Single-node mode: start without schema
            ok;
        _ ->
            mnesia:create_schema([node()])
    end,
    application:start(mnesia),

    {ok, State} = erlmcp_session_mnesia:init(#{
        table_name => erlmcp_session_mnesia_test,
        disc_copies => false
    }),
    State.

cleanup_mnesia(_State) ->
    mnesia:delete_table(erlmcp_session_mnesia_test),
    application:stop(mnesia),
    %% Only delete schema if we created it (i.e., not on nonode@nohost)
    case node() of
        'nonode@nohost' -> ok;
        _ -> mnesia:delete_schema([node()])
    end,
    ok.

test_mnesia_store_and_fetch() ->
    State = setup_mnesia(),
    SessionId = <<"session_mnesia_1">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => 3600000,
        metadata => #{backend => mnesia}
    },
    ?assertEqual({ok, State}, erlmcp_session_mnesia:store(SessionId, Session, State)),
    ?assertMatch({ok, _, _}, erlmcp_session_mnesia:fetch(SessionId, State)),
    cleanup_mnesia(State).

test_mnesia_delete() ->
    State = setup_mnesia(),
    SessionId = <<"session_mnesia_del">>,
    Session = #{
        id => SessionId,
        created_at => erlang:system_time(millisecond),
        last_accessed => erlang:system_time(millisecond),
        timeout_ms => infinity,
        metadata => #{}
    },
    {ok, State1} = erlmcp_session_mnesia:store(SessionId, Session, State),
    ?assertEqual({ok, State1}, erlmcp_session_mnesia:delete(SessionId, State1)),
    ?assertEqual({error, not_found, State1}, erlmcp_session_mnesia:fetch(SessionId, State1)),
    cleanup_mnesia(State1).

test_mnesia_list() ->
    State = setup_mnesia(),
    Sessions = [
        {<<"mnesia_list_1">>, #{id => <<"mnesia_list_1">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}},
        {<<"mnesia_list_2">>, #{id => <<"mnesia_list_2">>, created_at => 1, last_accessed => 1, timeout_ms => infinity, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_mnesia:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, SessionIds, _} = erlmcp_session_mnesia:list(State1),
    ?assertEqual(2, length(SessionIds)),
    cleanup_mnesia(State1).

test_mnesia_cleanup_expired() ->
    State = setup_mnesia(),
    Now = erlang:system_time(millisecond),
    Sessions = [
        {<<"expired_mnesia_1">>, #{id => <<"expired_mnesia_1">>, created_at => Now - 10000, last_accessed => Now - 5000, timeout_ms => 1000, metadata => #{}}},
        {<<"valid_mnesia_1">>, #{id => <<"valid_mnesia_1">>, created_at => Now - 10000, last_accessed => Now - 500, timeout_ms => 10000, metadata => #{}}}
    ],
    State1 = lists:foldl(fun({Id, Sess}, Acc) ->
        {ok, NewAcc} = erlmcp_session_mnesia:store(Id, Sess, Acc),
        NewAcc
    end, State, Sessions),
    {ok, Count, _} = erlmcp_session_mnesia:cleanup_expired(State1),
    ?assertEqual(1, Count),
    cleanup_mnesia(State1).

%%====================================================================
%% Integration Tests - Backend Comparison
%%====================================================================

backend_comparison_test_() ->
    {setup,
     fun setup_all_backends/0,
     fun cleanup_all_backends/1,
     fun(_) -> [
         ?_test(test_all_backends_basic_ops()),
         ?_test(test_all_backends_ttl_expiration())
     ] end}.

setup_all_backends() ->
    #{
        ets => setup_ets(),
        dets => setup_dets(),
        mnesia => setup_mnesia()
    }.

cleanup_all_backends(Backends) ->
    cleanup_ets(maps:get(ets, Backends)),
    cleanup_dets(maps:get(dets, Backends)),
    cleanup_mnesia(maps:get(mnesia, Backends)),
    ok.

test_all_backends_basic_ops() ->
    Backends = setup_all_backends(),

    %% Test basic store/fetch for all backends
    lists:foreach(fun({BackendName, State}) ->
        SessionId = <<"session_", (atom_to_binary(BackendName))/binary>>,
        Session = #{
            id => SessionId,
            created_at => erlang:system_time(millisecond),
            last_accessed => erlang:system_time(millisecond),
            timeout_ms => infinity,
            metadata => #{backend => BackendName}
        },

        StoreFun = case BackendName of
            ets -> fun erlmcp_session_ets:store/3;
            dets -> fun erlmcp_session_dets:store/3;
            mnesia -> fun erlmcp_session_mnesia:store/3
        end,

        FetchFun = case BackendName of
            ets -> fun erlmcp_session_ets:fetch/2;
            dets -> fun erlmcp_session_dets:fetch/2;
            mnesia -> fun erlmcp_session_mnesia:fetch/2
        end,

        ?assertEqual({ok, State}, StoreFun(SessionId, Session, State)),
        ?assertMatch({ok, _, _}, FetchFun(SessionId, State))
    end, maps:to_list(Backends)),

    cleanup_all_backends(Backends).

test_all_backends_ttl_expiration() ->
    Backends = setup_all_backends(),
    Now = erlang:system_time(millisecond),

    %% Test TTL expiration for all backends
    lists:foreach(fun({BackendName, State}) ->
        ExpiredId = <<"expired_", (atom_to_binary(BackendName))/binary>>,
        ValidId = <<"valid_", (atom_to_binary(BackendName))/binary>>,

        ExpiredSession = #{
            id => ExpiredId,
            created_at => Now - 10000,
            last_accessed => Now - 5000,
            timeout_ms => 1000,
            metadata => #{}
        },

        ValidSession = #{
            id => ValidId,
            created_at => Now - 10000,
            last_accessed => Now - 500,
            timeout_ms => 10000,
            metadata => #{}
        },

        StoreFun = case BackendName of
            ets -> fun erlmcp_session_ets:store/3;
            dets -> fun erlmcp_session_dets:store/3;
            mnesia -> fun erlmcp_session_mnesia:store/3
        end,

        CleanupFun = case BackendName of
            ets -> fun erlmcp_session_ets:cleanup_expired/1;
            dets -> fun erlmcp_session_dets:cleanup_expired/1;
            mnesia -> fun erlmcp_session_mnesia:cleanup_expired/1
        end,

        FetchFun = case BackendName of
            ets -> fun erlmcp_session_ets:fetch/2;
            dets -> fun erlmcp_session_dets:fetch/2;
            mnesia -> fun erlmcp_session_mnesia:fetch/2
        end,

        {ok, State1} = StoreFun(ExpiredId, ExpiredSession, State),
        {ok, State2} = StoreFun(ValidId, ValidSession, State1),
        {ok, Count, _} = CleanupFun(State2),

        ?assert(Count >= 1),  % At least expired session should be cleaned
        ?assertEqual({error, not_found, State2}, FetchFun(ExpiredId, State2))
    end, maps:to_list(Backends)),

    cleanup_all_backends(Backends).
