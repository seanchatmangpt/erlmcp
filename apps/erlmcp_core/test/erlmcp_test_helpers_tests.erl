-module(erlmcp_test_helpers_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%====================================================================
%%% Test Suite for erlmcp_test_helpers
%%% Chicago School TDD: Test the test helpers themselves
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

unique_id_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Generate unique server IDs", fun test_generate_server_ids/0},
          {"Generate unique client IDs", fun test_generate_client_ids/0},
          {"IDs are unique across calls", fun test_ids_are_unique/0}
         ]
     end}.

port_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Get free port without tracking", fun test_get_free_port/0},
          {"Allocate and release port", fun test_allocate_release_port/0},
          {"Allocate multiple ports", fun test_allocate_multiple_ports/0}
         ]
     end}.

process_management_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Start and stop server", fun test_start_stop_server/0},
          {"Server with custom capabilities", fun test_server_with_caps/0},
          {"Stop all test processes", fun test_stop_all_processes/0}
         ]
     end}.

test_data_generation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Generate random binary", fun test_random_binary/0},
          {"Generate random URI", fun test_random_uri/0},
          {"Generate test resource", fun test_generate_resource/0},
          {"Generate test tool", fun test_generate_tool/0}
         ]
     end}.

utility_functions_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Assert process alive", fun test_assert_alive/0},
          {"Wait for process death", fun test_wait_for_death/0},
          {"With server wrapper", fun test_with_server/0},
          {"Unique ref generation", fun test_unique_ref/0}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    %% Start only the core app, skip observability which has issues
    application:ensure_all_started(gproc),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Unique ID Generation Tests
%%%====================================================================

test_generate_server_ids() ->
    Id1 = erlmcp_test_helpers:generate_server_id(),
    Id2 = erlmcp_test_helpers:generate_server_id(),
    ?assert(is_binary(Id1)),
    ?assert(is_binary(Id2)),
    ?assertNotEqual(Id1, Id2).

test_generate_client_ids() ->
    Id1 = erlmcp_test_helpers:generate_client_id(),
    Id2 = erlmcp_test_helpers:generate_client_id(<<"custom">>),
    ?assert(is_binary(Id1)),
    ?assert(is_binary(Id2)),
    ?assertNotEqual(Id1, Id2).

test_ids_are_unique() ->
    %% Generate 50 IDs and verify all are unique
    Ids = [erlmcp_test_helpers:generate_server_id() || _ <- lists:seq(1, 50)],
    UniqueIds = lists:usort(Ids),
    ?assertEqual(50, length(UniqueIds)).

%%%====================================================================
%%% Port Management Tests
%%%====================================================================

test_get_free_port() ->
    Port1 = erlmcp_test_helpers:get_free_port(),
    Port2 = erlmcp_test_helpers:get_free_port(),
    ?assert(is_integer(Port1)),
    ?assert(is_integer(Port2)),
    ?assert(Port1 > 0),
    ?assert(Port2 > 0).

test_allocate_release_port() ->
    {ok, Port} = erlmcp_test_helpers:allocate_port(),
    ?assert(is_integer(Port)),
    ?assert(Port >= 10000),
    ?assert(Port =< 65000),
    ok = erlmcp_test_helpers:release_port(Port).

test_allocate_multiple_ports() ->
    Ports = [begin
        {ok, P} = erlmcp_test_helpers:allocate_port(),
        P
    end || _ <- lists:seq(1, 5)],
    ?assertEqual(5, length(Ports)),
    %% All ports should be unique
    ?assertEqual(5, length(lists:usort(Ports))),
    %% Cleanup
    lists:foreach(fun(P) -> erlmcp_test_helpers:release_port(P) end, Ports),
    ok.

%%%====================================================================
%%% Process Management Tests
%%%====================================================================

test_start_stop_server() ->
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    ?assert(is_pid(Server)),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_test_helpers:stop_test_process(Server),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Server)).

test_server_with_caps() ->
    ServerId = erlmcp_test_helpers:generate_server_id(),
    Caps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true}
    },
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId, Caps),
    ?assert(is_pid(Server)),
    ok = erlmcp_test_helpers:stop_test_process(Server).

test_stop_all_processes() ->
    %% Start multiple servers
    Servers = [begin
        Id = erlmcp_test_helpers:generate_server_id(),
        {ok, S} = erlmcp_test_helpers:start_test_server(Id),
        S
    end || _ <- lists:seq(1, 3)],

    %% All should be alive
    ?assertEqual(3, length([S || S <- Servers, erlang:is_process_alive(S)])),

    %% Stop all
    ok = erlmcp_test_helpers:stop_all_test_processes(),
    timer:sleep(100),

    %% All should be dead
    ?assertEqual(0, length([S || S <- Servers, erlang:is_process_alive(S)])).

%%%====================================================================
%%% Test Data Generation Tests
%%%====================================================================

test_random_binary() ->
    Bin1 = erlmcp_test_helpers:random_binary(16),
    Bin2 = erlmcp_test_helpers:random_binary(16),
    ?assertEqual(16, byte_size(Bin1)),
    ?assertEqual(16, byte_size(Bin2)),
    %% Should be different (very high probability)
    ?assertNotEqual(Bin1, Bin2).

test_random_uri() ->
    Uri1 = erlmcp_test_helpers:random_uri(),
    Uri2 = erlmcp_test_helpers:random_uri(<<"custom">>),
    ?assert(is_binary(Uri1)),
    ?assert(is_binary(Uri2)),
    ?assert(string:prefix(binary_to_list(Uri1), "test://") =:= 0),
    ?assert(string:prefix(binary_to_list(Uri2), "custom://") =:= 0).

test_generate_resource() ->
    {Uri, Handler} = erlmcp_test_helpers:generate_test_resource(),
    ?assert(is_binary(Uri)),
    ?assert(is_function(Handler, 1)).

test_generate_tool() ->
    {Name, Handler} = erlmcp_test_helpers:generate_test_tool(),
    ?assert(is_binary(Name)),
    ?assert(is_function(Handler, 2)).

%%%====================================================================
%%% Utility Functions Tests
%%%====================================================================

test_assert_alive() ->
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    ?assertEqual(true, erlmcp_test_helpers:assert_process_alive(Server)),
    ok = erlmcp_test_helpers:stop_test_process(Server).

test_wait_for_death() ->
    ServerId = erlmcp_test_helpers:generate_server_id(),
    {ok, Server} = erlmcp_test_helpers:start_test_server(ServerId),
    spawn(fun() ->
        timer:sleep(100),
        erlmcp_test_helpers:stop_test_process(Server)
    end),
    ?assertEqual(ok, erlmcp_test_helpers:wait_for_process_death(Server, 5000)).

test_with_server() ->
    %% This tests the with_test_server helper which ensures cleanup
    ServerId = erlmcp_test_helpers:generate_server_id(),
    Result = erlmcp_test_helpers:with_test_server(ServerId, fun(Server) ->
        ?assert(erlang:is_process_alive(Server)),
        {ok, server_was_alive}
    end),
    ?assertEqual({ok, server_was_alive}, Result).

test_unique_ref() ->
    Ref1 = erlmcp_test_helpers:unique_ref(),
    Ref2 = erlmcp_test_helpers:unique_ref(),
    ?assert(is_binary(Ref1)),
    ?assert(is_binary(Ref2)),
    ?assertNotEqual(Ref1, Ref2).
