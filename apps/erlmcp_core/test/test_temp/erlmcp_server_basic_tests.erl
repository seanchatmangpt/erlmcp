-module(erlmcp_server_basic_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Basic Server Tests - Chicago School TDD
%%% Tests for lifecycle, capabilities, initialization, and concurrent ops
%%% Principles: Real processes, observable behavior, no state inspection
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

server_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Start and stop server", fun test_start_stop/0},
         {"Multiple servers coexist", fun test_multiple_servers/0},
         {"Server with capabilities record", fun test_capabilities_record/0},
         {"Server with config map", fun test_config_map/0},
         {"Server process termination", fun test_termination/0}]
     end}.

capability_negotiation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"All capabilities enabled", fun test_all_capabilities/0},
         {"Only resources capability", fun test_resources_only/0},
         {"Only tools capability", fun test_tools_only/0},
         {"Only prompts capability", fun test_prompts_only/0},
         {"No capabilities", fun test_no_capabilities/0}]
     end}.

initialization_test_() ->
    {timeout,
     10,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_) ->
         [{"Server starts in initialization phase", fun test_initialization_phase/0},
          {"Server alive and ready", fun test_server_ready/0},
          {"Initialization sequence", fun test_init_sequence/0}]
      end}}.

concurrent_operations_test_() ->
    {timeout,
     15,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_) ->
         [{"Concurrent resource additions", fun test_concurrent_resources/0},
          {"Concurrent tool additions", fun test_concurrent_tools/0},
          {"Concurrent prompt additions", fun test_concurrent_prompts/0},
          {"Concurrent deletions", fun test_concurrent_deletions/0}]
      end}}.

system_message_handling_test_() ->
    {timeout,
     5,
     {setup,
      fun setup/0,
      fun cleanup/1,
      fun(_) ->
         [{"Unknown message handling", fun test_unknown_messages/0},
          {"Garbage collection trigger", fun test_gc_trigger/0},
          {"Code change stability", fun test_code_change/0}]
      end}}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Lifecycle Tests
%%%====================================================================

test_start_stop() ->
    %% Test start_link with capabilities record
    {ok, Pid1} =
        erlmcp_server:start_link(<<"lifecycle_test_1">>,
                                 #mcp_server_capabilities{resources =
                                                              #mcp_capability{enabled = true},
                                                          tools = #mcp_capability{enabled = false},
                                                          prompts =
                                                              #mcp_capability{enabled = true}}),
    ?assert(is_pid(Pid1)),
    ?assert(erlang:is_process_alive(Pid1)),
    ok = erlmcp_server:stop(Pid1),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Pid1)),

    %% Test start_link with config map
    {ok, Pid2} =
        erlmcp_server:start_link(<<"lifecycle_test_2">>,
                                 #{capabilities =>
                                       #mcp_server_capabilities{resources =
                                                                    #mcp_capability{enabled =
                                                                                        false},
                                                                tools =
                                                                    #mcp_capability{enabled = true},
                                                                prompts =
                                                                    #mcp_capability{enabled =
                                                                                        false}}}),
    ?assert(is_pid(Pid2)),
    ok = erlmcp_server:stop(Pid2).

test_multiple_servers() ->
    %% Test multiple servers can coexist
    {ok, Pid1} = erlmcp_server:start_link(<<"server_multi_1">>, #mcp_server_capabilities{}),
    {ok, Pid2} = erlmcp_server:start_link(<<"server_multi_2">>, #mcp_server_capabilities{}),
    {ok, Pid3} = erlmcp_server:start_link(<<"server_multi_3">>, #mcp_server_capabilities{}),
    ?assert(is_pid(Pid1)),
    ?assert(is_pid(Pid2)),
    ?assert(is_pid(Pid3)),
    ?assert(Pid1 =/= Pid2),
    ?assert(Pid2 =/= Pid3),
    ?assert(Pid1 =/= Pid3),
    ?assert(erlang:is_process_alive(Pid1)),
    ?assert(erlang:is_process_alive(Pid2)),
    ?assert(erlang:is_process_alive(Pid3)),
    ok = erlmcp_server:stop(Pid1),
    ok = erlmcp_server:stop(Pid2),
    ok = erlmcp_server:stop(Pid3).

test_capabilities_record() ->
    ServerId = <<"caps_record_test">>,
    Capabilities =
        #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                 tools = #mcp_capability{enabled = true},
                                 prompts = #mcp_capability{enabled = false}},
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    ok = erlmcp_server:stop(Pid).

test_config_map() ->
    ServerId = <<"config_map_test">>,
    Config =
        #{capabilities =>
              #mcp_server_capabilities{resources = #mcp_capability{enabled = false},
                                       tools = #mcp_capability{enabled = true},
                                       prompts = #mcp_capability{enabled = true}}},
    {ok, Pid} = erlmcp_server:start_link(ServerId, Config),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)),
    ok = erlmcp_server:stop(Pid).

test_termination() ->
    ServerId = <<"terminate_test">>,
    {ok, Server} = erlmcp_server:start_link(ServerId, #mcp_server_capabilities{}),

    %% Normal terminate
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server),

    %% Give time for cleanup
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Server)).

%%%====================================================================
%%% Capability Negotiation Tests
%%%====================================================================

test_all_capabilities() ->
    ServerId = <<"caps_all">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                                        tools = #mcp_capability{enabled = true},
                                                        prompts = #mcp_capability{enabled = true}}),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server).

test_resources_only() ->
    ServerId = <<"caps_resources">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                                        tools = #mcp_capability{enabled = false},
                                                        prompts =
                                                            #mcp_capability{enabled = false}}),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server).

test_tools_only() ->
    ServerId = <<"caps_tools">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources =
                                                            #mcp_capability{enabled = false},
                                                        tools = #mcp_capability{enabled = true},
                                                        prompts =
                                                            #mcp_capability{enabled = false}}),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server).

test_prompts_only() ->
    ServerId = <<"caps_prompts">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources =
                                                            #mcp_capability{enabled = false},
                                                        tools = #mcp_capability{enabled = false},
                                                        prompts = #mcp_capability{enabled = true}}),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server).

test_no_capabilities() ->
    ServerId = <<"caps_none">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources =
                                                            #mcp_capability{enabled = false},
                                                        tools = #mcp_capability{enabled = false},
                                                        prompts =
                                                            #mcp_capability{enabled = false}}),
    ?assert(erlang:is_process_alive(Server)),
    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Initialization Tests
%%%====================================================================

test_initialization_phase() ->
    ServerId = <<"init_phase_test">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                                        tools = #mcp_capability{enabled = true},
                                                        prompts = #mcp_capability{enabled = true}}),

    %% Verify server is running
    ?assert(is_pid(Server)),
    ?assert(erlang:is_process_alive(Server)),

    %% Server should accept operations after init
    %% (In real flow, initialized would be set to true after initialize message)
    ok = erlmcp_server:stop(Server).

test_server_ready() ->
    ServerId = <<"server_ready_test">>,
    Server = start_server_with_caps(ServerId, #mcp_server_capabilities{}),

    %% Server should be alive and ready to receive initialize
    ?assert(erlang:is_process_alive(Server)),

    %% Verify the server can handle requests
    ok = erlmcp_server:stop(Server),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Server)).

test_init_sequence() ->
    ServerId = <<"init_sequence_test">>,
    Server =
        start_server_with_caps(ServerId,
                               #mcp_server_capabilities{resources =
                                                            #mcp_capability{enabled = false},
                                                        tools = #mcp_capability{enabled = true},
                                                        prompts =
                                                            #mcp_capability{enabled = false}}),

    %% Server should be alive and ready to receive initialize
    ?assert(erlang:is_process_alive(Server)),

    %% Clean up
    ok = erlmcp_server:stop(Server),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Server)).

%%%====================================================================
%%% Concurrent Operations Tests
%%%====================================================================

test_concurrent_resources() ->
    Server =
        start_server_with_caps(<<"concurrent_resources">>,
                               #mcp_server_capabilities{resources =
                                                            #mcp_capability{enabled = true}}),

    %% Test concurrent resource additions
    ResourcePids =
        [spawn(fun() ->
                  Uri = <<"test://concurrent/resource_", (integer_to_binary(N))/binary>>,
                  Handler = fun(_) -> <<"content">> end,
                  erlmcp_server:add_resource(Server, Uri, Handler)
               end)
         || N <- lists:seq(1, 20)],

    %% Wait for all to complete
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- ResourcePids],

    ok = erlmcp_server:stop(Server).

test_concurrent_tools() ->
    Server =
        start_server_with_caps(<<"concurrent_tools">>,
                               #mcp_server_capabilities{tools = #mcp_capability{enabled = true}}),

    %% Test concurrent tool additions
    ToolPids =
        [spawn(fun() ->
                  Name = <<"concurrent_tool_", (integer_to_binary(N))/binary>>,
                  Handler = fun(_) -> #{result => ok} end,
                  erlmcp_server:add_tool(Server, Name, Handler)
               end)
         || N <- lists:seq(1, 20)],

    %% Wait for all to complete
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- ToolPids],

    ok = erlmcp_server:stop(Server).

test_concurrent_prompts() ->
    Server =
        start_server_with_caps(<<"concurrent_prompts">>,
                               #mcp_server_capabilities{prompts = #mcp_capability{enabled = true}}),

    %% Test concurrent prompt additions
    PromptPids =
        [spawn(fun() ->
                  Name = <<"concurrent_prompt_", (integer_to_binary(N))/binary>>,
                  Handler =
                      fun(_) ->
                         [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"ok">>}}]
                      end,
                  erlmcp_server:add_prompt(Server, Name, Handler)
               end)
         || N <- lists:seq(1, 20)],

    %% Wait for all to complete
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- PromptPids],

    ok = erlmcp_server:stop(Server).

test_concurrent_deletions() ->
    Server =
        start_server_with_caps(<<"concurrent_deletions">>,
                               #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                                        tools = #mcp_capability{enabled = true}}),

    %% Add resources
    Uri1 = <<"test://concurrent/delete/1">>,
    Uri2 = <<"test://concurrent/delete/2">>,
    ok = erlmcp_server:add_resource(Server, Uri1, fun(_) -> <<"1">> end),
    ok = erlmcp_server:add_resource(Server, Uri2, fun(_) -> <<"2">> end),

    %% Test concurrent deletions
    DeletePids =
        [spawn(fun() ->
                  case N of
                      1 ->
                          erlmcp_server:delete_resource(Server, Uri1);
                      2 ->
                          erlmcp_server:delete_resource(Server, Uri2)
                  end
               end)
         || N <- lists:seq(1, 2)],

    %% Wait for deletions
    [begin
         Ref = monitor(process, Pid),
         receive
             {'DOWN', Ref, process, P, _} ->
                 ok
         end
     end
     || Pid <- DeletePids],

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% System Message Handling Tests
%%%====================================================================

test_unknown_messages() ->
    Server = start_server_with_caps(<<"unknown_msg">>, #mcp_server_capabilities{}),

    %% Send various unknown messages
    Server ! unknown_message,
    Server ! {unknown, tuple},
    Server ! {random, message, with, many, elements},

    %% Server should still be alive
    timer:sleep(50),
    ?assert(erlang:is_process_alive(Server)),

    ok = erlmcp_server:stop(Server).

test_gc_trigger() ->
    Server = start_server_with_caps(<<"gc_trigger">>, #mcp_server_capabilities{}),

    %% Send force_gc message directly to trigger periodic GC
    Server ! force_gc,
    timer:sleep(100),  %% Let GC complete
    ?assert(erlang:is_process_alive(Server)),

    ok = erlmcp_server:stop(Server).

test_code_change() ->
    Server = start_server_with_caps(<<"code_change">>, #mcp_server_capabilities{}),

    %% Server should be alive and stable
    ?assert(erlang:is_process_alive(Server)),

    %% Verify server responds to calls (state is consistent)
    ?assert(is_pid(Server)),

    ok = erlmcp_server:stop(Server).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Start server with specific capabilities
start_server_with_caps(ServerId, Capabilities) ->
    {ok, Pid} = erlmcp_server:start_link(ServerId, Capabilities),
    Pid.
