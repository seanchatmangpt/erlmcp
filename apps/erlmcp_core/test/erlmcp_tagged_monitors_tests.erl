-module(erlmcp_tagged_monitors_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%% Test OTP 27/28 tagged monitors for granular process monitoring

%%====================================================================
%% Test Setup
%%====================================================================

tagged_monitor_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Tag monitor creates embedded tag in DOWN message", fun test_tag_embedded_in_down/0},
      {"Multiple monitors with different tags are distinguishable", fun test_multiple_tagged_monitors/0},
      {"No need for Ref -> Tag mapping with tagged monitors", fun test_no_ref_mapping_needed/0},
      {"Session backend spawns tools with tagged monitors", fun test_session_backend_tagged_tools/0},
      {"Monitored registry uses tags for automatic cleanup", fun test_monitored_registry_tags/0},
      {"Tagged monitor handles process crash gracefully", fun test_tagged_monitor_crash/0}
     ]}.

setup() ->
    %% Start application for testing
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_State) ->
    %% Clean up any spawned processes
    cleanup_processes(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test that tag is embedded in DOWN message
test_tag_embedded_in_down() ->
    %% Spawn a test process
    Tester = self(),
    Pid = spawn(
        fun() ->
            receive
                die -> ok
            end
        end
    ),

    %% Create tagged monitor
    Tag = {test_process, unique_tag_123},
    MonRef = erlang:monitor(process, Pid, [{tag, Tag}]),

    %% Verify monitor reference contains tag
    ?assertEqual({Tag, Pid}, MonRef),

    %% Kill process
    Pid ! die,

    %% Receive DOWN message
    receive
        {'DOWN', MonRef, process, Pid, Reason} ->
            %% The tag should be in the reference
            ?assertEqual({Tag, Pid}, MonRef),
            ?assertEqual(normal, Reason)
    after
        1000 ->
            ?assert(false, "Timeout waiting for DOWN message")
    end,

    %% Clean up
    erlang:demonitor(MonRef, [flush]),
    ok.

%% @doc Test multiple monitors with different tags
test_multiple_tagged_monitors() ->
    %% Spawn multiple processes
    Pid1 = spawn(fun() -> receive die -> ok end end),
    Pid2 = spawn(fun() -> receive die -> ok end end),
    Pid3 = spawn(fun() -> receive die -> ok end end),

    %% Create tagged monitors with different tags
    Tag1 = {tool, calculator},
    Tag2 = {tool, converter},
    Tag3 = {tool, validator},

    MonRef1 = erlang:monitor(process, Pid1, [{tag, Tag1}]),
    MonRef2 = erlang:monitor(process, Pid2, [{tag, Tag2}]),
    MonRef3 = erlang:monitor(process, Pid3, [{tag, Tag3}]),

    %% Verify each monitor reference has unique tag
    ?assertEqual({Tag1, Pid1}, MonRef1),
    ?assertEqual({Tag2, Pid2}, MonRef2),
    ?assertEqual({Tag3, Pid3}, MonRef3),

    %% Kill processes in different order
    Pid2 ! die,
    receive
        {'DOWN', MonRef2, process, Pid2, _} ->
            ?assertEqual({Tag2, Pid2}, MonRef2)
    after
        1000 ->
            ?assert(false, "Timeout waiting for Pid2 DOWN")
    end,

    Pid1 ! die,
    receive
        {'DOWN', MonRef1, process, Pid1, _} ->
            ?assertEqual({Tag1, Pid1}, MonRef1)
    after
        1000 ->
            ?assert(false, "Timeout waiting for Pid1 DOWN")
    end,

    Pid3 ! die,
    receive
        {'DOWN', MonRef3, process, Pid3, _} ->
            ?assertEqual({Tag3, Pid3}, MonRef3)
    after
        1000 ->
            ?assert(false, "Timeout waiting for Pid3 DOWN")
    end,

    %% Clean up
    erlang:demonitor(MonRef1, [flush]),
    erlang:demonitor(MonRef2, [flush]),
    erlang:demonitor(MonRef3, [flush]),
    ok.

%% @doc Test that no Ref -> Tag mapping is needed
test_no_ref_mapping_needed() ->
    %% This is the key benefit: we don't need to maintain a map
    %% The tag is embedded in the reference itself

    %% Old way (without tagged monitors):
    %% RefToTagMap = #{},
    %% Ref = erlang:monitor(process, Pid),
    %% RefToTagMap2 = maps:put(Ref, Tag, RefToTagMap),
    %% %% Later in DOWN handler:
    %% Tag = maps:get(Ref, RefToTagMap)

    %% New way (with tagged monitors):
    Tag = {session, session_123},
    Pid = spawn(fun() -> receive die -> ok end end),
    Ref = erlang:monitor(process, Pid, [{tag, Tag}]),

    %% Tag is embedded in Ref - no separate map needed!
    ?assertEqual({Tag, Pid}, Ref),

    %% In DOWN handler, tag is directly available:
    Pid ! die,
    receive
        {'DOWN', Ref, process, Pid, _} ->
            %% Extract tag directly from Ref
            ExtractedTag = element(1, Ref),
            ?assertEqual(Tag, ExtractedTag)
    after
        1000 ->
            ?assert(false, "Timeout waiting for DOWN")
    end,

    ok.

%% @doc Test session backend spawning tools with tagged monitors
test_session_backend_tagged_tools() ->
    %% Start session backend
    {ok, Pid} = erlmcp_session_backend:start_link(
        #{backend => erlmcp_session_ets, cleanup_interval => 60000}
    ),

    %% Spawn a tool with tagged monitor
    ToolName = <<"test_tool">>,
    Params = #{param1 => value1},

    Result = erlmcp_session_backend:spawn_tool(ToolName, Params),

    ?assertMatch({ok, _ToolPid, _MonRef}, Result),

    {ok, ToolPid, MonRef} = Result,

    %% Verify monitor reference contains tag
    %% Tag format: {tool, ToolName}
    ?assertMatch({{tool, ToolName}, ToolPid}, MonRef),

    %% Wait for tool to complete
    timer:sleep(200),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test monitored registry with tagged monitors
test_monitored_registry_tags() ->
    %% Start monitored registry
    {ok, RegPid} = erlmcp_monitored_registry:start_link(),

    %% Register a process with tag
    Key = key1,
    Value = value1,
    Tag = {service, cache},

    %% Spawn a process to register
    ProcPid = spawn(fun() -> receive die -> ok end end),

    {ok, MonRef} = erlmcp_monitored_registry:register(Key, Value, Tag),

    %% Verify monitor reference contains tag
    ?assertMatch({Tag, ProcPid}, MonRef),

    %% Lookup should work
    {ok, Value, ProcPid} = erlmcp_monitored_registry:lookup(Key),

    %% Kill process
    ProcPid ! die,

    %% Wait for DOWN to be processed
    timer:sleep(100),

    %% Lookup should now fail (automatic cleanup)
    ?assertEqual({error, not_found}, erlmcp_monitored_registry:lookup(Key)),

    %% Clean up
    gen_server:stop(RegPid),
    ok.

%% @doc Test tagged monitor handles crashes gracefully
test_tagged_monitor_crash() ->
    %% Spawn a process that will crash
    Pid = spawn(
        fun() ->
            timer:sleep(50),
            exit({crash, intentional_crash})
        end
    ),

    %% Monitor with tag
    Tag = {worker, id_456},
    MonRef = erlang:monitor(process, Pid, [{tag, Tag}]),

    %% Wait for crash
    receive
        {'DOWN', MonRef, process, Pid, Reason} ->
            ?assertEqual({crash, intentional_crash}, Reason),
            %% Tag should still be in reference
            ?assertEqual({Tag, Pid}, MonRef)
    after
        1000 ->
            ?assert(false, "Timeout waiting for crash DOWN")
    end,

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Clean up any remaining processes
cleanup_processes() ->
    %% Kill any spawned test processes
    processes:filter(
        fun(P) ->
            case erlang:process_info(P, dictionary) of
                {dictionary, Dict} ->
                    case lists:keyfind(test_process, 1, Dict) of
                        false -> false;
                        _ -> exit(P, kill), true
                    end;
                _ ->
                    false
            end
        end
    ),
    ok.
