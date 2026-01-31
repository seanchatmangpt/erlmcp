#!/usr/bin/env escript
%%% Demonstration of DOT format call graph export

-mode(compile).

main(_) ->
    io:format("~n"),
    io:format("╔════════════════════════════════════════════════════════════════╗╗"),
    io:format("║  DOT Format Call Graph Export - Live Demonstration              ║"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

    %% Create a sample process
    io:format("Step 1: Creating sample Erlang process...~n"),
    Pid = spawn(fun() ->
        io:format("  → Process ~p is running~n", [self()]),
        receive after infinity -> ok end
    end),
    io:format("  ✓ Process created: ~p~n~n", [Pid]),

    %% Simulate process info
    io:format("Step 2: Simulating process information...~n"),
    ProcInfo = #{
        pid => Pid,
        registered_name => demo_process,
        message_queue_len => 3,
        memory => 8192,
        current_function => {erlmcp_server, handle_call, 3}
    },
    io:format("  ✓ Process info gathered:~n"),
    io:format("    - Registered name: demo_process~n"),
    io:format("    - Message queue length: 3~n"),
    io:format("    - Memory usage: 8192 bytes~n"),
    io:format("    - Current function: erlmcp_server:handle_call/3~n~n"),

    %% Generate DOT format
    io:format("Step 3: Generating DOT format...~n"),
    DOT = generate_dot(ProcInfo),
    io:format("  ✓ DOT format generated~n~n"),

    %% Write to file
    io:format("Step 4: Writing DOT file...~n"),
    OutputFile = "/tmp/erlmcp_demo_call_graph.dot",
    case file:write_file(OutputFile, list_to_binary(DOT)) of
        ok ->
            io:format("  ✓ Written to: ~s~n~n", [OutputFile]);
        {error, Reason} ->
            io:format("  ✗ Failed: ~p~n~n", [Reason])
    end,

    %% Display DOT content
    io:format("Step 5: DOT File Content:~n"),
    io:format("─────────────────────────────────────────────────────────────────~n"),
    io:format("~s", [DOT]),
    io:format("─────────────────────────────────────────────────────────────────~n~n"),

    %% Try to render with GraphViz
    io:format("Step 6: Rendering with GraphViz...~n"),
    PNGFile = "/tmp/erlmcp_demo_call_graph.png",
    case os:find_executable("dot") of
        false ->
            io:format("  ⚠ GraphViz 'dot' command not found~n"),
            io:format("  Install with: brew install graphviz (macOS)~n"),
            io:format("              or: apt-get install graphviz (Linux)~n~n");
        DotPath ->
            io:format("  ✓ GraphViz found at: ~s~n", [DotPath]),
            RenderCmd = io_lib:format("dot -Tpng ~s -o ~s", [OutputFile, PNGFile]),
            case os:cmd(RenderCmd) of
                [] ->
                    io:format("  ✓ PNG rendered successfully~n"),
                    io:format("  ✓ Output: ~s~n", [PNGFile]),
                    io:format("  ✓ Size: ~s~n~n", [file_size(PNGFile)]);
                Error ->
                    io:format("  ✗ Render failed: ~s~n~n", [Error])
            end
    end,

    %% Display usage instructions
    io:format("Step 7: Usage Instructions:~n"),
    io:format("─────────────────────────────────────────────────────────────────~n"),
    io:format("To use this feature in your code:~n~n"),
    io:format("  %% Start call graph collection~n"),
    io:format("  {ok, Ref} = erlmcp_debugger:call_graph(MyPid, 5000),~n~n"),
    io:format("  %% Wait for collection~n"),
    io:format("  timer:sleep(5100),~n~n"),
    io:format("  %% Export to DOT file~n"),
    io:format("  ok = erlmcp_debugger:visualize_call_graph(Ref, \"/tmp/graph.dot\"),~n~n"),
    io:format("  %% Render to PNG (requires GraphViz)~n"),
    io:format("  %% $ dot -Tpng /tmp/graph.dot -o graph.png~n"),
    io:format("─────────────────────────────────────────────────────────────────~n~n"),

    %% Clean up
    exit(Pid, kill),

    io:format("╔════════════════════════════════════════════════════════════════╗╗"),
    io:format("║  Demonstration Complete!                                        ║"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

    init:stop().

%% Generate DOT format (mirrors erlmcp_debugger:call_graph_to_dot/1)
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

file_size(File) ->
    case file:read_file_info(File) of
        {ok, Info} ->
            Size = filelib:file_size(File),
            io_lib:format("~p bytes", [Size]);
        {error, _} ->
            "unknown"
    end.
