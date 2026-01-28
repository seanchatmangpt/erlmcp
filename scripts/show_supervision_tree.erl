#!/usr/bin/env escript
%%% -*- erlang -*-
%%! -pa _build/default/lib/*/ebin

%% Script to display the supervision tree structure
main([]) ->
    io:format("~n=== erlmcp Supervision Tree (v1.4.0) ===~n~n"),

    case erlmcp_sup:init([]) of
        {ok, {SupFlags, ChildSpecs}} ->
            io:format("Top Supervisor (erlmcp_sup):~n"),
            io:format("  Strategy: ~p~n", [maps:get(strategy, SupFlags)]),
            io:format("  Intensity: ~p~n", [maps:get(intensity, SupFlags)]),
            io:format("  Period: ~p~n~n", [maps:get(period, SupFlags)]),

            lists:foreach(fun(Child) ->
                Id = maps:get(id, Child),
                Type = maps:get(type, Child),
                Restart = maps:get(restart, Child),

                io:format("  [TIER] ~p (~p)~n", [Id, Type]),
                io:format("    Restart: ~p~n", [Restart]),

                % Try to get child supervisor's specs
                case Type of
                    supervisor ->
                        case Id of
                            erlmcp_core_sup ->
                                show_core_sup();
                            erlmcp_server_sup ->
                                show_server_sup();
                            erlmcp_observability_sup ->
                                show_observability_sup();
                            _ ->
                                ok
                        end;
                    _ ->
                        ok
                end,
                io:format("~n")
            end, ChildSpecs),

            io:format("~n=== Tree Summary ===~n"),
            io:format("3-tier architecture:~n"),
            io:format("  TIER 1: Core (registry + infrastructure)~n"),
            io:format("  TIER 2: Protocol (MCP servers)~n"),
            io:format("  TIER 3: Observability (isolated monitoring)~n~n"),
            halt(0);
        Error ->
            io:format("Error getting supervision tree: ~p~n", [Error]),
            halt(1)
    end.

show_core_sup() ->
    case erlmcp_core_sup:init([]) of
        {ok, {SupFlags, Children}} ->
            Strategy = maps:get(strategy, SupFlags),
            io:format("    └─ Strategy: ~p~n", [Strategy]),
            io:format("    └─ Workers: ~p~n", [length(Children)]),
            lists:foreach(fun(Child) ->
                io:format("       ├─ ~p~n", [maps:get(id, Child)])
            end, Children);
        _ ->
            ok
    end.

show_server_sup() ->
    case erlmcp_server_sup:init([]) of
        {ok, {SupFlags, _Children}} ->
            Strategy = maps:get(strategy, SupFlags),
            io:format("    └─ Strategy: ~p~n", [Strategy]),
            io:format("    └─ Dynamic children: erlmcp_server_new~n");
        _ ->
            ok
    end.

show_observability_sup() ->
    case erlmcp_observability_sup:init([]) of
        {ok, {SupFlags, Children}} ->
            Strategy = maps:get(strategy, SupFlags),
            io:format("    └─ Strategy: ~p~n", [Strategy]),
            io:format("    └─ Workers: ~p~n", [length(Children)]),
            lists:foreach(fun(Child) ->
                io:format("       ├─ ~p~n", [maps:get(id, Child)])
            end, Children);
        _ ->
            ok
    end.
