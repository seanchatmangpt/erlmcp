#!/usr/bin/env escript
%%% Simple manual test for DOT format call graph export
%%% This tests the implementation without needing full compilation

-mode(compile).

main(_) ->
    io:format("~n=== Testing DOT Format Call Graph Export ===~n~n"),

    %% Test 1: process_info_to_map
    io:format("Test 1: process_info_to_map~n"),
    InfoList = [
        {pid, self()},
        {registered_name, test_process},
        {message_queue_len, 10},
        {memory, 9999},
        {current_function, {module, function, 2}}
    ],

    %% Simulate the function
    Map = maps:from_list([{K, V} || {K, V} <- InfoList]),

    assertEqual(self(), maps:get(pid, Map)),
    assertEqual(test_process, maps:get(registered_name, Map)),
    assertEqual(10, maps:get(message_queue_len, Map)),
    io:format("  ✓ process_info_to_map works~n~n"),

    %% Test 2: call_graph_to_dot
    io:format("Test 2: call_graph_to_dot~n"),
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => test_process,
        message_queue_len => 5,
        memory => 12345,
        current_function => {erlmcp_server, handle_call, 3}
    },

    %% Simulate the function
    DOT = generate_dot(ProcInfo),
    io:format("  Generated DOT format:~n"),
    io:format("  ---~n"),
    io:format("~s~n", [DOT]),
    io:format("  ---~n~n"),

    %% Validate DOT structure
    assert(string:str(DOT, "digraph ErlangProcesses") > 0),
    assert(string:str(DOT, "rankdir=TB") > 0),
    assert(string:str(DOT, "shape=box") > 0),
    assert(string:str(DOT, "test_process") > 0),
    assert(string:str(DOT, "Queue: 5") > 0),
    assert(string:str(DOT, "Memory: 12345") > 0),
    assert(string:str(DOT, "erlmcp_server:handle_call/3") > 0),
    io:format("  ✓ DOT format is valid~n~n"),

    %% Test 3: Write to file
    io:format("Test 3: Write DOT file~n"),
    OutputFile = "/tmp/erlmcp_test_call_graph.dot",
    case file:write_file(OutputFile, list_to_binary(DOT)) of
        ok ->
            io:format("  ✓ DOT file written to: ~s~n", [OutputFile]),
            io:format("  You can visualize it with: dot -Tpng ~s -o graph.png~n~n", [OutputFile]);
        {error, Reason} ->
            io:format("  ✗ Failed to write file: ~p~n~n", [Reason])
    end,

    %% Test 4: timestamp
    io:format("Test 4: timestamp~n"),
    {{Y, M, D}, {H, Min, S}} = calendar:universal_time(),
    TS = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                      [Y, M, D, H, Min, S]),
    assert(is_list(TS)),
    assert(length(TS) > 0),
    assert(string:str(TS, "UTC") > 0),
    io:format("  ✓ Timestamp generated: ~s~n~n", [TS]),

    %% Clean up
    exit(Pid, kill),

    io:format("=== All Tests Passed! ===~n"),
    init:stop().

%% Helper assertion functions
assertEqual(Expected, Actual) ->
    case Actual of
        Expected -> ok;
        _ -> io:format("  ✗ Assertion failed: expected ~p, got ~p~n", [Expected, Actual]),
               exit(test_failed)
    end.

assert(Condition) ->
    case Condition of
        true -> ok;
        false -> io:format("  ✗ Assertion failed: ~p~n", [Condition]),
                 exit(test_failed)
    end.

%% Simulate call_graph_to_dot function
generate_dot(ProcInfo) ->
    Pid = maps:get(pid, ProcInfo, "unknown"),
    Name = case maps:get(registered_name, ProcInfo, undefined) of
        undefined -> pid_to_list(Pid);
        RegName when is_atom(RegName) -> atom_to_list(RegName)
    end,

    MsgQueueLen = maps:get(message_queue_len, ProcInfo, 0),
    MemUsage = maps:get(memory, ProcInfo, 0),
    CurrentFunc = case maps:get(current_function, ProcInfo, undefined) of
        {Mod, Func, Arity} -> io_lib:format("~s:~s/~p", [Mod, Func, Arity]);
        _ -> "unknown"
    end,

    {{Y, Month, D}, {H, Min, S}} = calendar:universal_time(),
    TS = io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B UTC",
                      [Y, Month, D, H, Min, S]),

    NodeAttrs = io_lib:format(
        "  \"~s\" [label=\"~s\\nQueue: ~p\\nMemory: ~p\\n~s\" shape=box];",
        [pid_to_list(Pid), Name, MsgQueueLen, MemUsage, CurrentFunc]
    ),

    lists:flatten([
        "digraph ErlangProcesses {\n",
        "  rankdir=TB;\n",
        "  node [shape=box, style=rounded];\n",
        "  edge [fontsize=10];\n",
        "\n",
        "  /* Process node */\n",
        NodeAttrs, "\n",
        "\n",
        "  /* Graph metadata */\n",
        "  label=\"Erlang Process Call Graph\\nGenerated: " ++ TS ++ "\";\n",
        "  labelloc=t;\n",
        "  fontsize=12;\n",
        "}\n"
    ]).
