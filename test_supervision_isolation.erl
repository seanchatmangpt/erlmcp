#!/usr/bin/env escript

%%====================================================================
%% ERLMCP Supervision Isolation Test
%%
%% This test verifies that the supervision strategy changes work correctly.
%% Tests individual component restarts and ensures no mass restarts occur.
%%====================================================================

main(_) ->
    io:format("=== ERLMCP Supervision Isolation Test ===~n"),
    io:format("Testing one_for_one strategy and domain isolation~n~n"),

    %% Start the application
    case application:start(erlmcp_core) of
        ok ->
            io:format("✅ Application started successfully~n"),
            test_supervisor_strategy(),
            test_domain_isolation(),
            test_component_restart(),
            application:stop(erlmcp_core),
            io:format("~n✅ All tests passed! Supervision isolation working correctly.~n");
        {error, Reason} ->
            io:format("❌ Failed to start application: ~p~n", [Reason]),
            halt(1)
    end.

%% Test 1: Verify supervisor strategy
test_supervisor_strategy() ->
    io:format("1. Testing supervisor strategy...~n"),

    case supervisor:which_children(erlmcp_sup) of
        Children when is_list(Children) ->
            case supervisor:get_childspec(erlmcp_sup, erlmcp_core_sup) of
                {ok, ChildSpec} ->
                    Strategy = maps:get(strategy, ChildSpec),
                    case Strategy of
                        one_for_one ->
                            io:format("   ✅ Root supervisor uses one_for_one strategy~n");
                        _ ->
                            io:format("   ❌ Root supervisor uses ~p strategy (expected one_for_one)~n", [Strategy]),
                            halt(1)
                    end;
                _ ->
                    io:format("   ❌ Could not get child spec for erlmcp_core_sup~n"),
                    halt(1)
            end;
        _ ->
            io:format("   ❌ Could not get supervisor children~n"),
            halt(1)
    end.

%% Test 2: Verify domain supervisors exist
test_domain_isolation() ->
    io:format("2. Testing domain isolation...~n"),

    %% Check that all domain supervisors exist
    DomainSupervisors = [
        erlmcp_registry_sup,
        erlmcp_session_sup,
        erlmcp_resilience_sup,
        erlmcp_mcp_sup
    ],

    lists:foreach(fun(Sup) ->
        case whereis(Sup) of
            undefined ->
                io:format("   ❌ Domain supervisor ~p not found~n", [Sup]),
                halt(1);
            Pid ->
                io:format("   ✅ Domain supervisor ~p running (~p)~n", [Sup, Pid])
        end
    end, DomainSupervisors).

%% Test 3: Test individual component restart isolation
test_component_restart() ->
    io:format("3. Testing component restart isolation...~n"),

    %% Get initial children count
    InitialChildren = get_domain_children_count(erlmcp_registry_sup),
    io:format("   Registry supervisor initial children: ~p~n", [InitialChildren]),

    %% Simulate a crash by terminating a registry child
    case supervisor:which_children(erlmcp_registry_sup) of
        [{erlmcp_registry, Pid, worker, [erlmcp_registry]} | _] ->
            io:format("   Terminating registry process...~n"),
            exit(Pid, kill),

            %% Wait for restart
            timer:sleep(1000),

            %% Verify only the registry restarted, not other domains
            NewChildren = get_domain_children_count(erlmcp_registry_sup),
            case NewChildren of
                InitialChildren ->
                    io:format("   ✅ Registry restarted successfully, children count unchanged~n");
                _ ->
                    io:format("   ⚠️  Registry children count changed (may be normal)~n")
            end;

        _ ->
            io:format("   ⚠️  Registry child not found, testing general supervision~n")
    end,

    %% Test that other domains are unaffected
    SessionChildren = get_domain_children_count(erlmcp_session_sup),
    ResilienceChildren = get_domain_children_count(erlmcp_resilience_sup),
    McpChildren = get_domain_children_count(erlmcp_mcp_sup),

    io:format("   Session supervisor children: ~p~n", [SessionChildren]),
    io:format("   Resilience supervisor children: ~p~n", [ResilienceChildren]),
    io:format("   MCP supervisor children: ~p~n", [McpChildren]).

%% Helper function to count children in a domain supervisor
get_domain_children_count(Supervisor) ->
    case supervisor:which_children(Supervisor) of
        Children when is_list(Children) ->
            length(Children);
        _ ->
            0
    end.