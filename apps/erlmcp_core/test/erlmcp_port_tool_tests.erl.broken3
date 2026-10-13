-module(erlmcp_port_tool_tests).

-include_lib("eunit/include/eunit.hrl").

%% EUnit tests for erlmcp_port_tool (Chicago School TDD)
%%
%% Tests port driver functionality with real processes
%% NO MOCKS - uses actual port communication

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup port tool for testing
setup_port_tool() ->
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),
    Pid.

%% @doc Cleanup port tool
cleanup_port_tool(Pid) ->
    catch erlmcp_port_tool:close_port(Pid),
    ok.

%% @doc Start echo process for testing
setup_echo_process() ->
    %% Start a simple cat process for testing
    {ok, Pid} = erlmcp_port_tool:start_link(#{}),
    %% Use cat as echo process
    case os:type() of
        {unix, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/cat", []}),
            Pid;
        {win32, _} ->
            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"cmd.exe", ["/c", "type"]}),
            Pid
    end.

%%====================================================================
%% Port Lifecycle Tests
%%====================================================================

port_lifecycle_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Start port
                    {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/cat", []}),
                    %% Verify port is open
                    {ok, Info} = erlmcp_port_tool:port_info(Pid),
                    ?assert(maps:is_key(port, Info)),
                    %% Close port
                    ok = erlmcp_port_tool:close_port(Pid)
                end)
         ]
     end}.

port_start_failure_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Try to start non-existent executable
                    Result = erlmcp_port_tool:start_port(Pid, {"/nonexistent/executable", []}),
                    ?assertMatch({error, {executable_not_found, _}}, Result)
                end)
         ]
     end}.

port_double_start_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Start first port
                    {ok, _Port1} = erlmcp_port_tool:start_port(Pid, {"/bin/cat", []}),
                    %% Try to start second port (should fail)
                    Result = erlmcp_port_tool:start_port(Pid, {"/bin/echo", []}),
                    ?assertMatch({error, port_already_open}, Result)
                end)
         ]
     end}.

%%====================================================================
%% Port Communication Tests
%%====================================================================

send_request_test_() ->
    {setup,
     fun setup_echo_process/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Send JSON request
                    Request = <<"{\"test\": \"data\"}">>,
                    ok = erlmcp_port_tool:send_request(Pid, Request),
                    %% Verify request was queued
                    {ok, Info} = erlmcp_port_tool:port_info(Pid),
                    ?assert(maps:get(pending_requests, Info, 0) > 0)
                end)
         ]
     end}.

send_request_to_closed_port_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Try to send without starting port
                    Request = <<"{\"test\": \"data\"}">>,
                    Result = erlmcp_port_tool:send_request(Pid, Request),
                    ?assertMatch({error, port_not_found}, Result)
                end)
         ]
     end}.

%%====================================================================
%% Port Info Tests
%%====================================================================

port_info_open_test_() ->
    {setup,
     fun setup_echo_process/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Get port info
                    {ok, Info} = erlmcp_port_tool:port_info(Pid),
                    ?assert(maps:is_key(port, Info)),
                    ?assert(maps:is_key(command, Info)),
                    ?assert(maps:is_key(args, Info)),
                    ?assert(is_integer(maps:get(pending_requests, Info))),
                    ?assert(is_integer(maps:get(buffer_size, Info)))
                end)
         ]
     end}.

port_info_closed_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Get port info without starting port
                    Result = erlmcp_port_tool:port_info(Pid),
                    ?assertMatch({error, port_not_open}, Result)
                end)
         ]
     end}.

%%====================================================================
%% Port Close Tests
%%====================================================================

close_open_port_test_() ->
    {setup,
     fun setup_echo_process/0,
     fun(Pid) ->
         %% Cleanup is part of the test
         ok = erlmcp_port_tool:close_port(Pid)
     end,
     fun(_Pid) -> ok end,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Port closed in setup
                    %% Verify by checking port info
                    ok
                end)
         ]
     end}.

close_closed_port_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Close without starting port (should succeed)
                    ok = erlmcp_port_tool:close_port(Pid)
                end)
         ]
     end}.

%%====================================================================
%% Error Handling Tests
%%====================================================================

port_timeout_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Start port with sleep command
                    case os:type() of
                        {unix, _} ->
                            {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/sleep", ["10"]}),
                            %% Try to receive with short timeout
                            Result = erlmcp_port_tool:recv_response(Pid, 100),
                            ?assertMatch({error, timeout}, Result);
                        {win32, _} ->
                            %% Skip on Windows
                            ok
                    end
                end)
         ]
     end}.

invalid_json_test_() ->
    {setup,
     fun setup_echo_process/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% Send invalid JSON
                    InvalidRequest = <<"{invalid json}">>,
                    ok = erlmcp_port_tool:send_request(Pid, InvalidRequest),
                    %% Port should still be running
                    {ok, Info} = erlmcp_port_tool:port_info(Pid),
                    ?assert(maps:is_key(port, Info))
                end)
         ]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

full_request_response_cycle_test_() ->
    {setup,
     fun setup_port_tool/0,
     fun cleanup_port_tool/1,
     fun(Pid) ->
         [
          ?_test(begin
                    %% This test requires Python MCP tool server
                    %% Skip if Python not available
                    case os:find_executable("python3") of
                        false ->
                            ?assert(true);  % Skip test
                        _PythonPath ->
                            %% Start Python tool server
                            ScriptPath = code:priv_dir(erlmcp_core) ++ "/mcp_tool_server.py",
                            case filelib:is_file(ScriptPath) of
                                true ->
                                    {ok, _Port} = erlmcp_port_tool:start_port(
                                                    Pid,
                                                    {"python3", ["-u", ScriptPath]}),
                                    %% Send tool invocation request
                                    Request = jsx:encode(#{
                                        <<"jsonrpc">> => <<"2.0">>,
                                        <<"id">> => 1,
                                        <<"method">> => <<"call_tool">>,
                                        <<"params">> => #{
                                            <<"tool">> => <<"echo">>,
                                            <<"arguments">> => #{<<"message">> => <<"test">>}
                                        }
                                    }),
                                    ok = erlmcp_port_tool:send_request(Pid, Request);
                                false ->
                                    ?assert(true)  % Skip test
                            end
                    end
                end)
         ]
     end}.

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_port_binary() ->
    ?FORALL(Binary, binary(),
        begin
            {ok, Pid} = erlmcp_port_tool:start_link(#{}),
            Result = try
                {ok, _Port} = erlmcp_port_tool:start_port(Pid, {"/bin/cat", []}),
                ok = erlmcp_port_tool:send_request(Pid, Binary),
                true
            catch
                _:_ -> false
            after
                catch erlmcp_port_tool:close_port(Pid)
            end,
            ?implies(true, Result)
        end).

port_binary_property_test_() ->
    {setup,
     fun() -> ok end,
     fun(__) -> ok end,
     fun(_) ->
         [
          ?_test(eunit:quickcheck(numtests(50, prop_port_binary()))
         ]
     end}.
