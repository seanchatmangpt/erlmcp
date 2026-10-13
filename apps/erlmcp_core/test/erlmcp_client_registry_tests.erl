%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for erlmcp_client_registry module
%%% Chicago School TDD - tests drive implementation
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test basic client registration and lookup
basic_register_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Act: Register a client
    ClientPid = self(),
    ClientName = <<"test-client">>,
    Result = erlmcp_client_registry:register(ClientName, ClientPid),

    %% Assert: Registration succeeded
    ?assertMatch({ok, _}, Result),

    %% Act: Lookup the client
    LookupResult = erlmcp_client_registry:lookup(ClientName),

    %% Assert: Client found with correct data
    ?assertMatch({ok, ClientPid, #{}, _Timestamp}, LookupResult),

    %% Cleanup
    erlmcp_client_registry:unregister(ClientName).

%% @doc Test registration of existing client fails
duplicate_register_test() ->
    %% Arrange: Start registry and register client
    {ok, _Pid} = erlmcp_client_registry:start_link(),
    ClientPid = self(),
    ClientName = <<"duplicate-client">>,
    {ok, _Ref1} = erlmcp_client_registry:register(ClientName, ClientPid),

    %% Act: Try to register same client again
    Result2 = erlmcp_client_registry:register(ClientName, ClientPid),

    %% Assert: Second registration fails
    ?assertEqual({error, already_exists}, Result2),

    %% Cleanup
    erlmcp_client_registry:unregister(ClientName).

%% @doc Test lookup of non-existent client
lookup_nonexistent_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Act: Lookup non-existent client
    Result = erlmcp_client_registry:lookup(<<"non-existent">>),

    %% Assert: Returns error
    ?assertEqual({error, not_found}, Result),

    %% Cleanup
    ok.

%% @doc Test client unregistration
unregister_test() ->
    %% Arrange: Start registry and register client
    {ok, _Pid} = erlmcp_client_registry:start_link(),
    ClientPid = self(),
    ClientName = <<"unregister-test">>,
    {ok, _Ref} = erlmcp_client_registry:register(ClientName, ClientPid),

    %% Verify client exists
    ?assertEqual(true, erlmcp_client_registry:exists(ClientName)),

    %% Act: Unregister client
    Result = erlmcp_client_registry:unregister(ClientName),

    %% Assert: Unregistration succeeded
    ?assertEqual(ok, Result),

    %% Verify client is gone
    ?assertEqual(false, erlmcp_client_registry:exists(ClientName)),

    %% Lookup should fail now
    ?assertEqual({error, not_found}, erlmcp_client_registry:lookup(ClientName)).

%% @doc Test unregistering non-existent client
unregister_nonexistent_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Act: Try to unregister non-existent client
    Result = erlmcp_client_registry:unregister(<<"non-existent">>),

    %% Assert: Returns not_found error
    ?assertEqual({error, not_found}, Result).

%% @doc Test listing all clients
list_clients_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Register multiple clients
    Client1Pid = self(),
    Client1Name = <<"client1">>,
    Client2Pid = spawn_link(fun() -> timer:sleep(1000) end),
    Client2Name = <<"client2">>,

    {ok, _Ref1} = erlmcp_client_registry:register(Client1Name, Client1Pid),
    {ok, _Ref2} = erlmcp_client_registry:register(Client2Name, Client2Pid),

    %% Act: List all clients
    Result = erlmcp_client_registry:list(),

    %% Assert: Both clients are listed
    ?assertMatch({ok, [Client1Name, Client2Name]}, Result),

    %% Cleanup
    erlmcp_client_registry:unregister(Client1Name),
    erlmcp_client_registry:unregister(Client2Name).

%% @doc Test client count functionality
count_clients_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Initially no clients
    ?assertEqual(0, erlmcp_client_registry:count()),

    %% Register first client
    Client1Pid = self(),
    Client1Name = <<"count-client1">>,
    {ok, _Ref1} = erlmcp_client_registry:register(Client1Name, Client1Pid),
    ?assertEqual(1, erlmcp_client_registry:count()),

    %% Register second client
    Client2Pid = spawn_link(fun() -> timer:sleep(1000) end),
    Client2Name = <<"count-client2">>,
    {ok, _Ref2} = erlmcp_client_registry:register(Client2Name, Client2Pid),
    ?assertEqual(2, erlmcp_client_registry:count()),

    %% Unregister first client
    erlmcp_client_registry:unregister(Client1Name),
    ?assertEqual(1, erlmcp_client_registry:count()),

    %% Unregister second client
    erlmcp_client_registry:unregister(Client2Name),
    ?assertEqual(0, erlmcp_client_registry:count()).

%% @doc Test get_pids functionality
get_pids_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Register clients
    Client1Pid = self(),
    Client1Name = <<"pid-client1">>,
    Client2Pid = spawn_link(fun() -> timer:sleep(1000) end),
    Client2Name = <<"pid-client2">>,

    {ok, _Ref1} = erlmcp_client_registry:register(Client1Name, Client1Pid),
    {ok, _Ref2} = erlmcp_client_registry:register(Client2Name, Client2Pid),

    %% Act: Get all PIDs
    Pids = erlmcp_client_registry:get_pids(),

    %% Assert: Both PIDs are returned
    ?assert(lists:member(Client1Pid, Pids)),
    ?assert(lists:member(Client2Pid, Pids)),

    %% Cleanup
    erlmcp_client_registry:unregister(Client1Name),
    erlmcp_client_registry:unregister(Client2Name).

%% @doc Test metadata registration
metadata_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Register client with metadata
    ClientPid = self(),
    ClientName = <<"metadata-client">>,
    Metadata = #{
        version => <<"1.0.0">>,
        type => <<"tool">>,
        capabilities => [<<"list">>, <<"create">>]
    },

    Result = erlmcp_client_registry:register(ClientName, ClientPid, Metadata),

    %% Assert: Registration succeeded
    ?assertMatch({ok, _Ref}, Result),

    %% Act: Lookup client with metadata
    LookupResult = erlmcp_client_registry:lookup(ClientName),

    %% Assert: Metadata is preserved
    ?assertMatch({ok, ClientPid, Metadata, _Timestamp}, LookupResult),

    %% Cleanup
    erlmcp_client_registry:unregister(ClientName).

%% @doc Test invalid name validation
invalid_name_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Test empty name
    ?assertEqual({error, invalid_name}, erlmcp_client_registry:register(<<>>, self())),

    %% Test too long name
    LongName = binary:copy(<<"x">>, 256),
    ?assertEqual({error, invalid_name}, erlmcp_client_registry:register(LongName, self())).

%% @doc Test process death cleanup
process_death_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Spawn a temporary client
    ClientPid = spawn_link(fun() ->
        %% Process will exit after 100ms
        timer:sleep(100)
    end),
    ClientName = <<"temp-client">>,

    %% Register the client
    {ok, _Ref} = erlmcp_client_registry:register(ClientName, ClientPid),

    %% Wait for process to die
    timer:sleep(200),

    %% Act: Try to lookup the client
    Result = erlmcp_client_registry:lookup(ClientName),

    %% Assert: Client should be cleaned up
    ?assertEqual({error, not_found}, Result).

%% @doc Test manual cleanup functionality
cleanup_dead_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Register a live client
    LivePid = self(),
    LiveName = <<"live-client">>,
    {ok, _Ref1} = erlmcp_client_registry:register(LiveName, LivePid),

    %% Register a dead client (this won't be detected automatically)
    DeadPid = spawn_link(fun() ->
        timer:sleep(50),
        exit(normal)
    end),
    DeadName = <<"dead-client">>,
    {ok, _Ref2} = erlmcp_client_registry:register(DeadName, DeadPid),

    %% Wait for dead process to actually die
    timer:sleep(100),

    %% Act: Perform manual cleanup
    Result = erlmcp_client_registry:cleanup_dead(),

    %% Assert: Cleanup completed
    ?assertEqual(ok, Result),

    %% Verify live client still exists
    ?assertEqual(true, erlmcp_client_registry:exists(LiveName)),

    %% Verify dead client is cleaned up
    ?assertEqual(false, erlmcp_client_registry:exists(DeadName)),

    %% Cleanup
    erlmcp_client_registry:unregister(LiveName).

%% @doc Test concurrent registration
concurrent_register_test() ->
    %% Arrange: Start registry
    {ok, _Pid} = erlmcp_client_registry:start_link(),

    %% Number of concurrent clients
    NumClients = 10,

    %% Spawn concurrent registration processes
    Processes = lists:map(fun(I) ->
        spawn_link(fun() ->
            ClientName = list_to_binary("concurrent-" ++ integer_to_list(I)),
            ClientPid = self(),
            Result = erlmcp_client_registry:register(ClientName, ClientPid),
            %% Send result back to test process
            whereis(test_server) ! {concurrent_result, self(), Result}
        end)
    end, lists:seq(1, NumClients)),

    %% Wait for all processes to complete
    Results = collect_concurrent_results(Processes, []),

    %% Assert: All registrations succeeded (no duplicates)
    Successful = lists:filter(fun({ok, _}) -> true; (_) -> false end, Results),
    ?assertEqual(NumClients, length(Successful)),

    %% Clean up all registered clients
    lists:foreach(fun(Process) ->
        receive
            {concurrent_result, Process, {ok, Ref}} ->
                %% Get the client name from the process
                ClientName = list_to_binary("concurrent-" ++ integer_to_list(?MODULE)),
                erlmcp_client_registry:unregister(ClientName)
        after 1000 ->
            ok
        end
    end, Processes).

%% @doc Test multiple registry instances
multiple_instances_test() ->
    %% Arrange: Start two registry instances
    {ok, Pid1} = erlmcp_client_registry:start_link(#{name => client_registry_1}),
    {ok, Pid2} = erlmcp_client_registry:start_link(#{name => client_registry_2}),

    %% Register different clients in each registry
    Client1Pid = self(),
    Client1Name = <<"registry1-client">>,
    Client2Pid = spawn_link(fun() -> timer:sleep(1000) end),
    Client2Name = <<"registry2-client">>,

    {ok, _Ref1} = erlmcp_client_registry:register(Client1Name, Client1Pid),
    {ok, _Ref2} = erlmcp_client_registry:register(Client2Name, Client2Pid),

    %% Act: Check each registry separately
    Result1 = erlmcp_client_registry:lookup(Client1Name),
    Result2 = erlmcp_client_registry:lookup(Client2Name),

    %% Assert: Each registry has its own clients
    ?assertMatch({ok, Client1Pid, #{}, _}, Result1),
    ?assertMatch({ok, Client2Pid, #{}, _}, Result2),

    %% Cleanup
    erlmcp_client_registry:unregister(Client1Name),
    erlmcp_client_registry:unregister(Client2Name).

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Collect results from concurrent processes
-spec collect_concurrent_results([pid()], [term()]) -> [term()].
collect_concurrent_results([], Acc) ->
    Acc;
collect_concurrent_results(Processes, Acc) ->
    receive
        {concurrent_result, Pid, Result} ->
            NewProcesses = lists:delete(Pid, Processes),
            collect_concurrent_results(NewProcesses, [Result | Acc])
    after 5000 ->
        %% Timeout - return what we have
        Acc
    end.