%%%-----------------------------------------------------------------------------
%%% @doc Tests for DOT format call graph export in erlmcp_debugger
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_debugger_dot_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TEST FIXTURES
%%%=============================================================================

dot_format_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Generate DOT from process info", fun test_call_graph_to_dot/0},
      {"Generate DOT with registered name", fun test_call_graph_to_dot_registered/0},
      {"Process info to map conversion", fun test_process_info_to_map/0},
      {"Timestamp generation", fun test_timestamp/0},
      {"Visualize call graph writes file", fun test_visualize_call_graph_file/0},
      {"Visualize call graph with missing ref", fun test_visualize_call_graph_missing_ref/0},
      {"DOT format validity", fun test_dot_format_validity/0},
      {"DOT format with current function", fun test_dot_format_with_current_function/0},
      {"DOT format with unknown current function", fun test_dot_format_unknown_function/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp_observability),
    %% Start test gen_server
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    register(dot_test_server, Pid),
    Pid.

cleanup(Pid) ->
    catch gen_server:stop(Pid),
    %% Cleanup any attached processes
    [erlmcp_debugger:detach(P) || #{pid := P} <- erlmcp_debugger:list_attached()],
    ok.

%%%=============================================================================
%%% DOT GENERATION TESTS
%%%=============================================================================

test_call_graph_to_dot() ->
    %% Create minimal process info map
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => undefined,
        message_queue_len => 5,
        memory => 12345,
        current_function => {erlmcp_server, handle_call, 3}
    },

    %% Generate DOT
    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),

    %% Verify DOT structure
    ?assert(is_list(DOT)),
    ?assertMatch("digraph ErlangProcesses {" ++ _, lists:flatten(DOT)),

    %% Check for required DOT elements
    FlatDOT = lists:flatten(DOT),
    ?assert(string:find(FlatDOT, "digraph ErlangProcesses") =/= nomatch),
    ?assert(string:find(FlatDOT, "rankdir=TB") =/= nomatch),
    ?assert(string:find(FlatDOT, "shape=box") =/= nomatch),
    ?assert(string:find(FlatDOT, pid_to_list(Pid)) =/= nomatch),
    ?assert(string:find(FlatDOT, "Queue: 5") =/= nomatch),
    ?assert(string:find(FlatDOT, "Memory: 12345") =/= nomatch),
    ?assert(string:find(FlatDOT, "erlmcp_server:handle_call/3") =/= nomatch),

    %% Clean up
    exit(Pid, kill).

test_call_graph_to_dot_registered() ->
    %% Test with registered process name
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => my_registered_process,
        message_queue_len => 0,
        memory => 6789,
        current_function => {gen_server, loop, 7}
    },

    %% Generate DOT
    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),
    FlatDOT = lists:flatten(DOT),

    %% Verify registered name appears in label
    ?assert(string:find(FlatDOT, "my_registered_process") =/= nomatch),
    ?assert(string:find(FlatDOT, "Queue: 0") =/= nomatch),
    ?assert(string:find(FlatDOT, "Memory: 6789") =/= nomatch),
    ?assert(string:find(FlatDOT, "gen_server:loop/7") =/= nomatch),

    %% Clean up
    exit(Pid, kill).

test_process_info_to_map() ->
    %% Test process_info_to_map conversion
    InfoList = [
        {pid, self()},
        {registered_name, test_process},
        {message_queue_len, 10},
        {memory, 9999},
        {current_function, {module, function, 2}}
    ],

    Map = erlmcp_debugger:process_info_to_map(InfoList),

    ?assertEqual(self(), maps:get(pid, Map)),
    ?assertEqual(test_process, maps:get(registered_name, Map)),
    ?assertEqual(10, maps:get(message_queue_len, Map)),
    ?assertEqual(9999, maps:get(memory, Map)),
    ?assertEqual({module, function, 2}, maps:get(current_function, Map)).

test_timestamp() ->
    %% Test timestamp generation
    TS = erlmcp_debugger:timestamp(),
    ?assert(is_list(TS)),
    ?assert(length(TS) > 0),

    %% Verify format (should match YYYY-MM-DD HH:MM:SS UTC)
    ?assertMatch("20" ++ _, TS), %% Year starts with 20
    ?assert(string:find(TS, "UTC") =/= nomatch).

test_visualize_call_graph_file() ->
    %% Test that visualize_call_graph writes a file
    Pid = whereis(dot_test_server),

    %% Start call graph collection
    {ok, Ref} = erlmcp_debugger:call_graph(Pid, 100),

    %% Give it time to collect some data
    timer:sleep(150),

    %% Create temp file
    TempFile = "/tmp/erlmcp_test_call_graph.dot",

    %% Generate DOT file
    Result = erlmcp_debugger:visualize_call_graph(Ref, TempFile),

    ?assertEqual(ok, Result),

    %% Verify file exists and is valid
    ?assert(filelib:is_file(TempFile)),

    {ok, Content} = file:read_file(TempFile),
    ?assert(byte_size(Content) > 0),

    %% Verify DOT format
    ContentStr = binary_to_list(Content),
    ?assert(string:find(ContentStr, "digraph ErlangProcesses") =/= nomatch),

    %% Clean up
    file:delete(TempFile).

test_visualize_call_graph_missing_ref() ->
    %% Test with non-existent reference
    FakeRef = make_ref(),

    Result = erlmcp_debugger:visualize_call_graph(FakeRef, "/tmp/fake.dot"),

    ?assertEqual({error, call_graph_not_found}, Result).

test_dot_format_validity() ->
    %% Test that DOT format is syntactically valid
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => undefined,
        message_queue_len => 1,
        memory => 5000,
        current_function => {erlmcp_core, init, 1}
    },

    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),
    FlatDOT = lists:flatten(DOT),

    %% Basic DOT syntax checks
    ?assert(string:find(FlatDOT, "digraph") =/= nomatch),
    ?assert(string:find(FlatDOT, "{") =/= nomatch),
    ?assert(string:find(FlatDOT, "}") =/= nomatch),
    ?assert(string:find(FlatDOT, "[") =/= nomatch),
    ?assert(string:find(FlatDOT, "]") =/= nomatch),
    ?assert(string:find(FlatDOT, ";") =/= nomatch),

    %% Clean up
    exit(Pid, kill).

test_dot_format_with_current_function() ->
    %% Test DOT format includes current function info
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => my_process,
        message_queue_len => 3,
        memory => 25000,
        current_function => {erlmcp_json_rpc, decode, 1}
    },

    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),
    FlatDOT = lists:flatten(DOT),

    %% Verify all expected elements present
    ?assert(string:find(FlatDOT, pid_to_list(Pid)) =/= nomatch),
    ?assert(string:find(FlatDOT, "my_process") =/= nomatch),
    ?assert(string:find(FlatDOT, "Queue: 3") =/= nomatch),
    ?assert(string:find(FlatDOT, "Memory: 25000") =/= nomatch),
    ?assert(string:find(FlatDOT, "erlmcp_json_rpc:decode/1") =/= nomatch),

    %% Clean up
    exit(Pid, kill).

test_dot_format_unknown_function() ->
    %% Test DOT format handles unknown current function
    Pid = spawn(fun() -> receive after infinity -> ok end end),
    ProcInfo = #{
        pid => Pid,
        registered_name => undefined,
        message_queue_len => 0,
        memory => 1000,
        current_function => undefined
    },

    DOT = erlmcp_debugger:call_graph_to_dot(ProcInfo),
    FlatDOT = lists:flatten(DOT),

    %% Should show "unknown" for current function
    ?assert(string:find(FlatDOT, "unknown") =/= nomatch),
    ?assert(string:find(FlatDOT, pid_to_list(Pid)) =/= nomatch),

    %% Clean up
    exit(Pid, kill).

%%%=============================================================================
%%% GEN_SERVER CALLBACKS (for test server)
%%%=============================================================================
%% NOTE: Currently unused, kept for future test infrastructure

% init([]) ->
%     {ok, #{counter => 0}}.
%
% handle_call(_Request, _From, State) ->
%     {reply, ok, State}.
%
% handle_cast(_Msg, State = #{counter := N}) ->
%     {noreply, State#{counter => N + 1}}.
%
% handle_info(_Info, State) ->
%     {noreply, State}.
%
% terminate(_Reason, _State) ->
%     ok.
%
% code_change(_OldVsn, State, _Extra) ->
%     {ok, State}.
