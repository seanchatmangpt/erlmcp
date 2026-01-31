#!/usr/bin/env escript
%%% Manual test script for DOT format call graph export

main(_) ->
    io:format("Testing DOT format call graph export...~n"),

    %% Start the observability application
    application:ensure_all_started(erlmcp_observability),

    %% Create a test process
    Pid = spawn(fun() -> receive after infinity -> ok end end),

    %% Create process info map
    ProcInfo = #{
        pid => Pid,
        registered_name => test_process,
        message_queue_len => 5,
        memory => 12345,
        current_function => {erlmcp_server, handle_call, 3}
    },

    io:format("Process info: ~p~n", [ProcInfo]),

    %% Test process_info_to_map
    InfoList = [
        {pid, Pid},
        {registered_name, test_process},
        {message_queue_len, 10}
    ],
    Map = erlmcp_debugger:process_info_to_map(InfoList),
    io:format("process_info_to_map result: ~p~n", [Map]),

    %% Test call_graph_to_dot
    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),
    io:format("~nGenerated DOT format:~n---~n~s~n---~n", [DOT]),

    %% Write to file
    OutputFile = "/tmp/erlmcp_test_call_graph.dot",
    case file:write_file(OutputFile, DOT) of
        ok ->
            io:format("~nDOT file written to: ~s~n", [OutputFile]),
            io:format("You can visualize it with: dot -Tpng ~s -o graph.png~n", [OutputFile]);
        {error, Reason} ->
            io:format("~nFailed to write file: ~p~n", [Reason])
    end,

    %% Test with missing reference
    FakeRef = make_ref(),
    Result = erlmcp_debugger:visualize_call_graph(FakeRef, "/tmp/fake.dot"),
    io:format("~nTest with missing ref: ~p~n", [Result]),

    %% Clean up
    exit(Pid, kill),

    io:format("~nAll tests passed!~n"),
    init:stop().
