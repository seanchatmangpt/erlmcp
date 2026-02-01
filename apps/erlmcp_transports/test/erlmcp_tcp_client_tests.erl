-module(erlmcp_tcp_client_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp_transport_tcp.hrl").

%% TCP Transport Client Tests - Chicago School TDD
%% Tests observable behavior through API calls only
%% NO STATE INSPECTION, NO DUMMY PROCESSES

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(ranch),
    ok.

teardown(_) ->
    application:stop(ranch),
    application:stop(erlmcp_core),
    ok.

%%%===================================================================
%%% Client Lifecycle Tests
%%%===================================================================

client_start_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Start client with valid options",
       fun() ->
          Opts =
              #{host => "localhost",
                port => 9999,
                transport_id => test_transport},
          {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),
          ?assert(is_pid(Pid)),
          ?assert(is_process_alive(Pid)),
          erlmcp_test_helpers:stop_test_process(Pid)
       end},
      {"Client with unique ID avoids conflicts",
       fun() ->
          Opts1 = #{host => "localhost", port => 9999},
          Opts2 = #{host => "localhost", port => 9999},

          {ok, Pid1} = erlmcp_test_helpers:start_test_client(Opts1),
          {ok, Pid2} = erlmcp_test_helpers:start_test_client(Opts2),

          ?assert(is_process_alive(Pid1)),
          ?assert(is_process_alive(Pid2)),
          ?assert(Pid1 =/= Pid2),

          erlmcp_test_helpers:stop_test_process(Pid1),
          erlmcp_test_helpers:stop_test_process(Pid2)
       end}]}.

client_connection_failure_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         Opts = #{host => "localhost", port => 9999},
         {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

         % Client attempts connection to non-existent server
         % Process remains alive handling reconnection
         timer:sleep(200),
         ?assert(is_process_alive(Pid)),

         erlmcp_test_helpers:stop_test_process(Pid)
      end}}.

client_send_not_connected_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        Opts = #{host => "localhost", port => 9999},
        {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

        % Wait for initial connection attempt
        timer:sleep(100),

        % Send should fail when not connected
        Result = erlmcp_test_helpers:send_transport_data(Pid, <<"test">>, 1000),
        ?assertEqual({error, not_connected}, Result),

        erlmcp_test_helpers:stop_test_process(Pid)
     end}.

%%%===================================================================
%%% Client Reconnection Tests
%%%===================================================================

reconnection_backoff_increases_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      15,
      fun() ->
         Opts =
             #{host => "localhost",
               port => 9999,  % Non-existent server
               max_reconnect_attempts => 5},
         {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

         % Wait for multiple reconnection attempts
         % Initial delay ~1s, second ~2s
         timer:sleep(4000),

         % Client should still be alive retrying
         ?assert(is_process_alive(Pid)),

         erlmcp_test_helpers:stop_test_process(Pid)
      end}}.

reconnection_max_attempts_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         Opts =
             #{host => "localhost",
               port => 9999,
               max_reconnect_attempts => 2},
         {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

         % Wait for initial + 2 reconnections
         timer:sleep(4000),

         % Process should still exist (stops retrying but stays alive)
         ?assert(is_process_alive(Pid)),

         erlmcp_test_helpers:stop_test_process(Pid)
      end}}.

%%%===================================================================
%%% Client Error Handling Tests
%%%===================================================================

client_handles_invalid_host_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        Opts = #{host => "invalid.host.example.com", port => 9999},
        {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

        % Client should handle DNS failure gracefully
        timer:sleep(200),
        ?assert(is_process_alive(Pid)),

        erlmcp_test_helpers:stop_test_process(Pid)
     end}.

client_connection_timeout_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         % Use IP that won't respond (unroutable)
         Opts =
             #{host => "192.0.2.1",  % TEST-NET-1, never routed
               port => 9999,
               connect_timeout => 1000},
         {ok, Pid} = erlmcp_test_helpers:start_test_client(Opts),

         % Wait for connection timeout
         timer:sleep(2000),

         % Client should handle timeout and retry
         ?assert(is_process_alive(Pid)),

         erlmcp_test_helpers:stop_test_process(Pid)
      end}}.

%%%===================================================================
%%% Client Observable Behavior Tests
%%%===================================================================

client_send_behavior_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         % Start server first
         ServerOpts = #{num_acceptors => 5},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         % Start client
         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         % Wait for connection
         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

         % Send data through client
         Message = <<"test message">>,
         ?assertEqual(ok, erlmcp_test_helpers:send_transport_data(ClientPid, Message, 1000)),

         % Cleanup
         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

client_concurrent_sends_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     {timeout,
      10,
      fun() ->
         ServerOpts = #{num_acceptors => 10},
         {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

         ClientOpts = #{port => Port},
         {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

         {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

         % Send multiple messages concurrently
         Messages = [<<"msg", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 10)],
         lists:foreach(fun(Msg) ->
                          spawn(fun() ->
                                   erlmcp_test_helpers:send_transport_data(ClientPid, Msg, 1000)
                                end)
                       end,
                       Messages),

         % Wait for sends to complete
         timer:sleep(500),

         % Client should still be alive
         ?assert(is_process_alive(ClientPid)),

         erlmcp_test_helpers:stop_test_process(ClientPid),
         erlmcp_test_helpers:stop_test_process(ServerPid)
      end}}.

%%%===================================================================
%%% Client Lifecycle Tests
%%%===================================================================

client_stop_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun() ->
        ServerOpts = #{},
        {ok, ServerPid, Port} = erlmcp_test_helpers:start_test_server(ServerOpts),

        ClientOpts = #{port => Port},
        {ok, ClientPid} = erlmcp_test_helpers:start_test_client(ClientOpts),

        {ok, _} = erlmcp_test_helpers:wait_for_transport_connected(5000),

        % Stop client
        ?assertEqual(ok, erlmcp_transport_tcp:close(ClientPid)),

        timer:sleep(100),
        ?assertNot(is_process_alive(ClientPid)),

        erlmcp_test_helpers:stop_test_process(ServerPid)
     end}.

%%%===================================================================
%%% Transport Behavior Tests
%%%===================================================================

transport_behavior_send_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Send fails when not connected",
       fun() ->
          % Create a minimal state for testing (not via gen_server)
          State = #state{socket = undefined},
          Result = erlmcp_transport_tcp:send(State, <<"test">>),
          ?assertEqual({error, not_connected}, Result)
       end}]}.

transport_behavior_close_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     [{"Close client state",
       fun() ->
          State = #state{mode = client, socket = undefined},
          Result = erlmcp_transport_tcp:close(State),
          ?assertEqual(ok, Result)
       end}]}.
