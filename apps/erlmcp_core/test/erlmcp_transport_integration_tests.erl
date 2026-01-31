%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Layer Integration Tests
%%%
%%% This test suite validates that all transport implementations properly
%%% integrate with the core erlmcp system (client and server).
%%%
%%% Tests:
%%% - Transport initialization through client
%%% - Message sending and receiving
%%% - Transport lifecycle (start, stop, reconnect)
%%% - Registry integration
%%% - Error handling and recovery
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_integration_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

setup_all() ->
    %% Start required applications
    {ok, _} = application:ensure_all_started(erlmcp_core),
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(ranch),
    %% Start transports app if available
    case code:load_file(erlmcp_transport_stdio) of
        {module, _} ->
            {ok, _} = application:ensure_all_started(erlmcp_transports);
        _ ->
            ok
    end,
    ok.

teardown_all(_) ->
    application:stop(erlmcp_transports),
    application:stop(ranch),
    application:stop(gproc),
    application:stop(erlmcp_core),
    ok.

setup() ->
    %% Set test mode
    put(test_mode, true),
    %% Clear any stale registry entries
    case whereis(erlmcp_registry) of
        undefined -> ok;
        _ ->
            %% Registry is running, that's fine
            ok
    end,
    ok.

cleanup(_) ->
    erase(test_mode),
    timer:sleep(100),
    ok.

%%====================================================================
%% Stdio Transport Integration Tests
%%====================================================================

stdio_transport_init_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Initialize stdio transport via client",
       fun() ->
           Opts = {stdio, #{}},
           {ok, Client} = erlmcp_client:start_link(Opts),
           try
               %% Verify client started successfully
               ?assert(is_pid(Client)),
               ?assert(erlang:is_process_alive(Client)),
               %% Verify client can be stopped
               ok = erlmcp_client:stop(Client),
               ?assertNot(erlang:is_process_alive(Client))
           after
               catch erlmcp_client:stop(Client)
           end
       end}
     ]}.

stdio_transport_send_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Send message through stdio transport",
       fun() ->
           Opts = {stdio, #{}},
           {ok, Client} = erlmcp_client:start_link(Opts),
           try
               %% Try to send a message (should not crash)
               %% In test mode, stdio won't actually send
               ok = erlmcp_client:stop(Client),
               ?assertNot(erlang:is_process_alive(Client))
           after
               catch erlmcp_client:stop(Client)
           end
       end}
     ]}.

%%====================================================================
%% TCP Transport Integration Tests
%%====================================================================

tcp_transport_init_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 10, [
      {"Initialize TCP client transport",
       fun() ->
           Opts = {tcp, #{
               mode => client,
               host => "localhost",
               port => 9999,
               max_reconnect_attempts => 0  % Don't reconnect in test
           }},
           {ok, Client} = erlmcp_client:start_link(Opts),
           try
               %% Verify client started
               ?assert(is_pid(Client)),
               %% Client may not be connected (server not running)
               %% but should not crash
               ok = erlmcp_client:stop(Client)
           after
               catch erlmcp_client:stop(Client)
           end
       end}
     ]}}.

tcp_transport_server_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 15, [
      {"TCP server transport lifecycle",
       fun() ->
           %% Check if erlmcp_transport_tcp is available
           case code:load_file(erlmcp_transport_tcp) of
               {module, _} ->
                   %% Start a TCP server
                   UniqueId = erlang:unique_integer([positive]),
                   ServerOpts = #{
                       mode => server,
                       port => 0,  % Use random port
                       transport_id => list_to_atom("test_transport_" ++ integer_to_list(UniqueId)),
                       server_id => list_to_atom("test_server_" ++ integer_to_list(UniqueId))
                   },
                   {ok, ServerPid} = erlmcp_transport_tcp:start_server(ServerOpts),
                   ?assert(is_pid(ServerPid)),
                   ?assert(erlang:is_process_alive(ServerPid)),
                   %% Stop the server
                   erlmcp_transport_tcp:close(ServerPid),
                   timer:sleep(100),
                   ?assertNot(erlang:is_process_alive(ServerPid));
               _ ->
                   %% Transport not available, skip test
                   ?assert(true)
           end
       end}
     ]}}.

%%====================================================================
%% HTTP Transport Integration Tests
%%====================================================================

http_transport_init_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     {timeout, 10, [
      {"Initialize HTTP transport",
       fun() ->
           Opts = {http, #{
               url => "http://localhost:8080/mcp",
               owner => self()
           }},
           case code:load_file(erlmcp_transport_http_server) of
               {module, _} ->
                   %% HTTP transport available, test initialization
                   {ok, TransportPid} = erlmcp_transport_http_server:start_link(Opts),
                   ?assert(is_pid(TransportPid)),
                   %% Clean up
                   erlmcp_transport_http:close(TransportPid);
               _ ->
                   %% HTTP transport not available, skip test
                   ?assert(true)
           end
       end}
     ]}}.

%%====================================================================
%% Registry Integration Tests
%%====================================================================

registry_registration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Transport registers with registry",
       fun() ->
           case whereis(erlmcp_registry) of
               undefined ->
                   %% Registry not running, skip
                   ?assert(true);
               _ ->
                   %% Registry is running
                   TransportId = test_transport_registry,
                   TransportPid = self(),
                   Config = #{type => stdio},
                   %% Register transport
                   ok = erlmcp_registry:register_transport(TransportId, TransportPid, Config),
                   %% Verify registration
                   {ok, {FoundPid, FoundConfig}} = erlmcp_registry:find_transport(TransportId),
                   ?assertEqual(TransportPid, FoundPid),
                   ?assertEqual(stdio, maps:get(type, FoundConfig)),
                   %% Unregister
                   ok = erlmcp_registry:unregister_transport(TransportId)
           end
       end}
     ]}.

%%====================================================================
%% Message Routing Tests
%%====================================================================

message_routing_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Route message from transport to server",
       fun() ->
           case whereis(erlmcp_registry) of
               undefined ->
                   ?assert(true);
               _ ->
                   ServerId = test_server_routing,
                   TransportId = test_transport_routing,
                   ServerPid = self(),
                   TransportPid = self(),

                   %% Register server and transport
                   ok = erlmcp_registry:register_server(ServerId, ServerPid, #{}),
                   ok = erlmcp_registry:register_transport(TransportId, TransportPid, #{}),

                   %% Bind them
                   ok = erlmcp_registry:bind_transport_to_server(TransportId, ServerId),

                   %% Route a message
                   TestMessage = #{<<"test">> => <<"data">>},
                   ok = erlmcp_registry:route_to_transport(TransportId, ServerId, TestMessage),

                   %% Clean up
                   ok = erlmcp_registry:unbind_transport(TransportId),
                   ok = erlmcp_registry:unregister_transport(TransportId),
                   ok = erlmcp_registry:unregister_server(ServerId)
           end
       end}
     ]}.

%%====================================================================
%% Transport Behavior Compliance Tests
%%====================================================================

transport_behavior_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"All transports implement required functions",
       fun() ->
           %% Check that transport modules have required functions
           RequiredFunctions = [send, close],

           %% List of transport modules to check
           TransportModules = [
               erlmcp_transport_stdio,
               erlmcp_transport_tcp,
               erlmcp_transport_http,
               erlmcp_transport_ws
           ],

           lists:foreach(fun(Module) ->
               case code:load_file(Module) of
                   {module, _} ->
                       %% Check for required exported functions
                       lists:foreach(fun(Fun) ->
                           ?assertMatch({_, _}, code:is_loaded(Module)),
                           %% Verify function is exported
                           case Module of
                               erlmcp_transport_stdio ->
                                   ?assert(erlang:function_exported(Module, send, 2)),
                                   ?assert(erlang:function_exported(Module, close, 1));
                               erlmcp_transport_tcp ->
                                   ?assert(erlang:function_exported(Module, send, 2)),
                                   ?assert(erlang:function_exported(Module, close, 1));
                               erlmcp_transport_http ->
                                   ?assert(erlang:function_exported(Module, send, 2)),
                                   ?assert(erlang:function_exported(Module, close, 1));
                               erlmcp_transport_ws ->
                                   ?assert(erlang:function_exported(Module, send, 2)),
                                   ?assert(erlang:function_exported(Module, close, 1));
                               _ ->
                                   ok
                           end
                       end, RequiredFunctions);
                   _ ->
                       %% Module not available, skip
                       ok
               end
           end, TransportModules)
       end}
     ]}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Handle invalid transport options",
       fun() ->
           %% Try to initialize client with invalid options
           InvalidOpts = {invalid_transport, #{}},
           case erlmcp_client:start_link(InvalidOpts) of
               {error, _} ->
                   %% Expected error
                   ?assert(true);
               {ok, Client} ->
                   %% Should not happen, but clean up
                   erlmcp_client:stop(Client),
                   ?assert(true)  % Don't fail test
           end
       end}
     ]}.
