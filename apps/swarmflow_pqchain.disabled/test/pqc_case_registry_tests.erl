%%% @doc EUnit tests for pqc_case_registry
%%%
%%% Tests:
%%% - Registry start/stop
%%% - Case creation and lookup
%%% - ensure_case/3 idempotency
%%% - Process monitoring and cleanup on Case termination
%%% - PubSub subscription/unsubscription
%%% - Event publishing and delivery
%%% - list_cases/0 and get_case_count/0
%%%
%%% Chicago School TDD: Real processes, no mocks
%%%
%%% @end
-module(pqc_case_registry_tests).

-include_lib("eunit/include/eunit.hrl").
-include("pqchain.hrl").

%%% ============================================================================
%%% Test Fixtures
%%% ============================================================================

%% Setup function for each test
registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_link/1,
      fun test_ensure_case_creates_new/1,
      fun test_ensure_case_returns_existing/1,
      fun test_lookup_existing_case/1,
      fun test_lookup_nonexistent_case/1,
      fun test_list_cases/1,
      fun test_get_case_count/1,
      fun test_subscribe_and_publish/1,
      fun test_unsubscribe/1,
      fun test_case_termination_cleanup/1,
      fun test_multiple_subscribers/1,
      fun test_concurrent_ensure_case/1
     ]}.

setup() ->
    %% Start pg if not already started
    case pg:start_link(pqc_case_pg) of
        {ok, PgPid} ->
            {ok, PgPid};
        {error, {already_started, PgPid}} ->
            {ok, PgPid}
    end,

    %% Start registry
    {ok, RegPid} = pqc_case_registry:start_link(),

    %% Return pids for cleanup
    {RegPid, PgPid}.

cleanup({RegPid, _PgPid}) ->
    %% Stop registry
    case is_process_alive(RegPid) of
        true ->
            exit(RegPid, shutdown),
            timer:sleep(100);
        false ->
            ok
    end,

    %% Clean up any remaining Case processes
    case ets:info(pqc_case_registry_tab) of
        undefined ->
            ok;
        _ ->
            ets:foldl(
                fun({_CaseId, Pid}, Acc) ->
                    case is_process_alive(Pid) of
                        true -> exit(Pid, kill);
                        false -> ok
                    end,
                    Acc
                end,
                ok,
                pqc_case_registry_tab
            )
    end,
    ok.

%%% ============================================================================
%%% Tests
%%% ============================================================================

test_start_link({RegPid, _}) ->
    fun() ->
        ?assert(is_process_alive(RegPid)),
        ?assertEqual(RegPid, whereis(pqc_case_registry))
    end.

test_ensure_case_creates_new({_, _}) ->
    fun() ->
        CaseId = <<"case-001">>,
        Net = test_net,
        SigningKey = test_key,

        {ok, Pid} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),

        ?assert(is_pid(Pid)),
        ?assert(is_process_alive(Pid))
    end.

test_ensure_case_returns_existing({_, _}) ->
    fun() ->
        CaseId = <<"case-002">>,
        Net = test_net,
        SigningKey = test_key,

        %% Create Case first time
        {ok, Pid1} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),

        %% Ensure same Case second time
        {ok, Pid2} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),

        %% Should return the same Pid
        ?assertEqual(Pid1, Pid2)
    end.

test_lookup_existing_case({_, _}) ->
    fun() ->
        CaseId = <<"case-003">>,
        Net = test_net,
        SigningKey = test_key,

        %% Create Case
        {ok, ExpectedPid} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),

        %% Lookup should find it
        {ok, ActualPid} = pqc_case_registry:lookup(CaseId),
        ?assertEqual(ExpectedPid, ActualPid)
    end.

test_lookup_nonexistent_case({_, _}) ->
    fun() ->
        CaseId = <<"nonexistent-case">>,

        %% Lookup should fail
        ?assertEqual({error, not_found}, pqc_case_registry:lookup(CaseId))
    end.

test_list_cases({_, _}) ->
    fun() ->
        %% Create multiple Cases
        CaseId1 = <<"case-list-001">>,
        CaseId2 = <<"case-list-002">>,
        CaseId3 = <<"case-list-003">>,

        {ok, _} = pqc_case_registry:ensure_case(CaseId1, net1, key1),
        {ok, _} = pqc_case_registry:ensure_case(CaseId2, net2, key2),
        {ok, _} = pqc_case_registry:ensure_case(CaseId3, net3, key3),

        %% List should contain all Cases
        Cases = pqc_case_registry:list_cases(),

        ?assert(lists:member(CaseId1, Cases)),
        ?assert(lists:member(CaseId2, Cases)),
        ?assert(lists:member(CaseId3, Cases)),
        ?assert(length(Cases) >= 3)
    end.

test_get_case_count({_, _}) ->
    fun() ->
        %% Get initial count
        InitialCount = pqc_case_registry:get_case_count(),

        %% Create Cases
        {ok, _} = pqc_case_registry:ensure_case(<<"case-count-001">>, net1, key1),
        {ok, _} = pqc_case_registry:ensure_case(<<"case-count-002">>, net2, key2),

        %% Count should increase
        NewCount = pqc_case_registry:get_case_count(),
        ?assertEqual(InitialCount + 2, NewCount)
    end.

test_subscribe_and_publish({_, _}) ->
    fun() ->
        CaseId = <<"case-pubsub-001">>,
        {ok, _Pid} = pqc_case_registry:ensure_case(CaseId, test_net, test_key),

        %% Subscribe to Case events
        ok = pqc_case_registry:subscribe(CaseId),

        %% Publish an event
        Event = {status, #{state => <<"running">>}},
        ok = pqc_case_registry:publish(CaseId, Event),

        %% Should receive the event
        receive
            {pqc_case_event, CaseId, Event} ->
                ok
        after 1000 ->
            ?assert(false, "Did not receive published event")
        end
    end.

test_unsubscribe({_, _}) ->
    fun() ->
        CaseId = <<"case-pubsub-002">>,
        {ok, _Pid} = pqc_case_registry:ensure_case(CaseId, test_net, test_key),

        %% Subscribe
        ok = pqc_case_registry:subscribe(CaseId),

        %% Unsubscribe
        ok = pqc_case_registry:unsubscribe(CaseId),

        %% Publish an event
        Event = {status, #{state => <<"running">>}},
        ok = pqc_case_registry:publish(CaseId, Event),

        %% Should NOT receive the event
        receive
            {pqc_case_event, CaseId, Event} ->
                ?assert(false, "Received event after unsubscribe")
        after 500 ->
            ok
        end
    end.

test_case_termination_cleanup({_, _}) ->
    fun() ->
        CaseId = <<"case-terminate-001">>,
        {ok, CasePid} = pqc_case_registry:ensure_case(CaseId, test_net, test_key),

        %% Subscribe to termination events
        ok = pqc_case_registry:subscribe(CaseId),

        %% Verify Case exists
        ?assertEqual({ok, CasePid}, pqc_case_registry:lookup(CaseId)),

        %% Terminate the Case process
        exit(CasePid, test_termination),

        %% Wait for cleanup
        timer:sleep(200),

        %% Case should be removed from registry
        ?assertEqual({error, not_found}, pqc_case_registry:lookup(CaseId)),

        %% Should receive termination event
        receive
            {pqc_case_event, CaseId, {case_terminated, test_termination}} ->
                ok
        after 1000 ->
            ?assert(false, "Did not receive case_terminated event")
        end
    end.

test_multiple_subscribers({_, _}) ->
    fun() ->
        CaseId = <<"case-multi-sub-001">>,
        {ok, _Pid} = pqc_case_registry:ensure_case(CaseId, test_net, test_key),

        %% Create multiple subscriber processes
        Parent = self(),
        Subscriber1 = spawn(fun() ->
            ok = pqc_case_registry:subscribe(CaseId),
            Parent ! {subscribed, 1},
            receive
                {pqc_case_event, CaseId, Event} ->
                    Parent ! {received, 1, Event}
            after 2000 ->
                Parent ! {timeout, 1}
            end
        end),

        Subscriber2 = spawn(fun() ->
            ok = pqc_case_registry:subscribe(CaseId),
            Parent ! {subscribed, 2},
            receive
                {pqc_case_event, CaseId, Event} ->
                    Parent ! {received, 2, Event}
            after 2000 ->
                Parent ! {timeout, 2}
            end
        end),

        %% Wait for subscriptions
        receive {subscribed, 1} -> ok after 1000 -> error(timeout) end,
        receive {subscribed, 2} -> ok after 1000 -> error(timeout) end,

        %% Publish event
        Event = {artifact, #{name => <<"test-artifact">>}},
        ok = pqc_case_registry:publish(CaseId, Event),

        %% Both subscribers should receive
        receive {received, 1, Event} -> ok after 1000 -> error({timeout, subscriber1}) end,
        receive {received, 2, Event} -> ok after 1000 -> error({timeout, subscriber2}) end,

        %% Cleanup
        exit(Subscriber1, kill),
        exit(Subscriber2, kill),
        ok
    end.

test_concurrent_ensure_case({_, _}) ->
    fun() ->
        CaseId = <<"case-concurrent-001">>,
        Net = test_net,
        SigningKey = test_key,

        Parent = self(),

        %% Spawn multiple processes trying to ensure the same Case
        Pids = [spawn(fun() ->
            {ok, Pid} = pqc_case_registry:ensure_case(CaseId, Net, SigningKey),
            Parent ! {ensured, self(), Pid}
        end) || _ <- lists:seq(1, 10)],

        %% Collect all results
        Results = [receive
            {ensured, P, Pid} when P == Proc ->
                Pid
        after 2000 ->
            error({timeout, Proc})
        end || Proc <- Pids],

        %% All should have received the same Pid
        [FirstPid | Rest] = Results,
        ?assert(lists:all(fun(P) -> P == FirstPid end, Rest)),

        %% Verify only one Case was created
        ?assertEqual({ok, FirstPid}, pqc_case_registry:lookup(CaseId))
    end.

%%% ============================================================================
%%% Property-Based Tests (if PropEr is available)
%%% ============================================================================

%% TODO: Add PropEr tests when available:
%% - prop_ensure_case_idempotent()
%% - prop_lookup_after_ensure()
%% - prop_subscriber_receives_all_events()
%% - prop_termination_cleanup()
