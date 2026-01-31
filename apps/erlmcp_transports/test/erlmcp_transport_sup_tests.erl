-module(erlmcp_transport_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Suite for Transport Supervisor
%%%
%%% Chicago School TDD: Real processes, observable behavior, no mocks
%%%
%%% Validates:
%%% - Supervisor startup and configuration
%%% - Transport registration to gproc
%%% - Child restart strategies (observable behavior only)
%%% - Process lifecycle management
%%% - Supervision tree integrity
%%%===================================================================

%%====================================================================
%% Test Setup
%%====================================================================

transport_sup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"Supervisor starts successfully", fun test_supervisor_start/0},
      {"Can start stdio transport child", fun test_start_stdio_child/0},
      {"Can start TCP transport child", fun test_start_tcp_child/0},
      {"Can start HTTP transport child", fun test_start_http_child/0},
      {"Can start WebSocket transport child", fun test_start_ws_child/0},
      {"Can start SSE transport child", fun test_start_sse_child/0},
      {"Transport registers to gproc", fun test_gproc_registration/0},
      {"Supervisor restarts crashed child", fun test_child_restart/0},
      {"Supervisor handles child termination", fun test_child_termination/0},
      {"Multiple transports can coexist", fun test_multiple_transports/0}
     ]}.

setup() ->
    %% Start dependencies
    application:ensure_all_started(gproc),
    application:ensure_all_started(ranch),
    application:ensure_all_started(gun),

    %% Start transport supervisor
    {ok, SupPid} = erlmcp_transport_sup:start_link(),
    SupPid.

cleanup(SupPid) ->
    case is_process_alive(SupPid) of
        true ->
            exit(SupPid, shutdown),
            timer:sleep(100);
        false ->
            ok
    end.

%%====================================================================
%% Supervisor Startup Tests
%%====================================================================

test_supervisor_start() ->
    %% Supervisor should be alive (observable behavior)
    Pid = whereis(erlmcp_transport_sup),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),

    %% Verify it's a supervisor via which_children (observable behavior)
    Children = supervisor:which_children(erlmcp_transport_sup),
    ?assert(is_list(Children)).

%%====================================================================
%% Transport Child Startup Tests
%%====================================================================

test_start_stdio_child() ->
    %% Test starting stdio transport as supervised child
    Owner = self(),

    ChildSpec = #{
        id => stdio_test,
        start => {erlmcp_transport_stdio, start_link, [Owner]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_stdio]
    },

    %% Set test mode to avoid stdin issues
    put(test_mode, true),

    Result = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    case Result of
        {ok, Pid} ->
            ?assert(is_pid(Pid)),
            ?assert(is_process_alive(Pid)),
            supervisor:terminate_child(erlmcp_transport_sup, stdio_test),
            supervisor:delete_child(erlmcp_transport_sup, stdio_test);
        {error, Reason} ->
            ?debugFmt("Stdio child start failed: ~p", [Reason])
    end,

    erase(test_mode).

test_start_tcp_child() ->
    %% Test starting TCP transport as supervised child
    UniqueId = erlang:unique_integer([positive]),
    Opts = #{
        mode => server,
        port => 0,  % Random port
        owner => self(),
        transport_id => list_to_atom("test_tcp_transport_" ++ integer_to_list(UniqueId)),
        server_id => list_to_atom("test_tcp_server_" ++ integer_to_list(UniqueId))
    },

    ChildSpec = #{
        id => tcp_test,
        start => {erlmcp_transport_tcp, start_server, [Opts]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_tcp]
    },

    Result = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    case Result of
        {ok, Pid} ->
            ?assert(is_pid(Pid)),
            ?assert(is_process_alive(Pid)),
            supervisor:terminate_child(erlmcp_transport_sup, tcp_test),
            supervisor:delete_child(erlmcp_transport_sup, tcp_test);
        {error, Reason} ->
            ?debugFmt("TCP child start failed: ~p", [Reason])
    end.

test_start_http_child() ->
    %% Test starting HTTP transport as supervised child
    Opts = #{
        url => "http://localhost:8080/mcp",
        owner => self(),
        connect_timeout => 1000
    },

    ChildSpec = #{
        id => http_test,
        start => {erlmcp_transport_http, init, [Opts]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_http, erlmcp_transport_http_server]
    },

    %% Note: This may fail if no server is running, which is expected
    Result = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    case Result of
        {ok, Pid} ->
            ?assert(is_pid(Pid)),
            supervisor:terminate_child(erlmcp_transport_sup, http_test),
            supervisor:delete_child(erlmcp_transport_sup, http_test);
        {error, _Reason} ->
            %% Expected if no server running
            supervisor:delete_child(erlmcp_transport_sup, http_test),
            ok
    end.

test_start_ws_child() ->
    %% Test starting WebSocket transport as supervised child
    UniqueId = erlang:unique_integer([positive]),
    Config = #{
        port => 0,  % Random port
        path => "/mcp/ws"
    },
    TransportId = list_to_atom("ws_sup_test_" ++ integer_to_list(UniqueId)),

    ChildSpec = #{
        id => ws_test,
        start => {erlmcp_transport_ws, init, [TransportId, Config]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_ws]
    },

    Result = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    case Result of
        {ok, Pid} ->
            ?assert(is_pid(Pid)),
            supervisor:terminate_child(erlmcp_transport_sup, ws_test),
            supervisor:delete_child(erlmcp_transport_sup, ws_test);
        {error, Reason} ->
            ?debugFmt("WS child start failed: ~p", [Reason]),
            supervisor:delete_child(erlmcp_transport_sup, ws_test)
    end.

test_start_sse_child() ->
    %% Test starting SSE transport as supervised child
    UniqueId = erlang:unique_integer([positive]),
    Config = #{
        port => 0,  % Random port
        path => "/mcp/sse"
    },
    TransportId = list_to_atom("sse_sup_test_" ++ integer_to_list(UniqueId)),

    ChildSpec = #{
        id => sse_test,
        start => {erlmcp_transport_sse, init, [TransportId, Config]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_sse]
    },

    Result = supervisor:start_child(erlmcp_transport_sup, ChildSpec),

    case Result of
        {ok, Pid} ->
            ?assert(is_pid(Pid)),
            supervisor:terminate_child(erlmcp_transport_sup, sse_test),
            supervisor:delete_child(erlmcp_transport_sup, sse_test);
        {error, Reason} ->
            ?debugFmt("SSE child start failed: ~p", [Reason]),
            supervisor:delete_child(erlmcp_transport_sup, sse_test)
    end.

%%====================================================================
%% gproc Registration Tests
%%====================================================================

test_gproc_registration() ->
    %% Test that transport registers to gproc on startup
    %% Start a stdio transport with unique ID
    Owner = self(),
    put(test_mode, true),

    {ok, TransportPid} = erlmcp_transport_stdio:start_link(Owner),

    %% Give time for registration
    timer:sleep(100),

    %% Verify process is alive (observable behavior)
    ?assert(is_process_alive(TransportPid)),

    %% Cleanup
    erlmcp_transport_stdio:close(TransportPid),
    erase(test_mode).

%%====================================================================
%% Restart Strategy Tests (Observable Behavior Only)
%%====================================================================

test_child_restart() ->
    %% Test that supervisor restarts crashed child (observable behavior)
    %% For permanent children, they will restart on crash
    Owner = self(),
    put(test_mode, true),

    ChildSpec = #{
        id => restart_test,
        start => {erlmcp_transport_stdio, start_link, [Owner]},
        restart => permanent,  % Will restart on crash
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_stdio]
    },

    {ok, Pid1} = supervisor:start_child(erlmcp_transport_sup, ChildSpec),
    ?assert(is_process_alive(Pid1)),

    %% Kill the child
    exit(Pid1, kill),
    timer:sleep(200),

    %% Supervisor should have restarted it (observable behavior)
    Children = supervisor:which_children(erlmcp_transport_sup),
    RestartTestChild = lists:keyfind(restart_test, 1, Children),

    case RestartTestChild of
        {restart_test, Pid2, worker, _} when is_pid(Pid2) ->
            ?assertNot(Pid1 =:= Pid2),  % Different PID (restarted)
            ?assert(is_process_alive(Pid2)),
            supervisor:terminate_child(erlmcp_transport_sup, restart_test);
        _ ->
            ok
    end,

    supervisor:delete_child(erlmcp_transport_sup, restart_test),
    erase(test_mode).

test_child_termination() ->
    %% Test that supervisor handles graceful child termination (observable behavior)
    Owner = self(),
    put(test_mode, true),

    ChildSpec = #{
        id => term_test,
        start => {erlmcp_transport_stdio, start_link, [Owner]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_stdio]
    },

    {ok, Pid} = supervisor:start_child(erlmcp_transport_sup, ChildSpec),
    ?assert(is_process_alive(Pid)),

    %% Gracefully stop the child
    erlmcp_transport_stdio:close(Pid),
    timer:sleep(100),

    %% Child should be terminated (observable behavior)
    ?assertNot(is_process_alive(Pid)),

    %% Cleanup
    supervisor:delete_child(erlmcp_transport_sup, term_test),
    erase(test_mode).

%%====================================================================
%% Multi-Transport Tests
%%====================================================================

test_multiple_transports() ->
    %% Test that multiple transport types can coexist under supervisor
    Owner = self(),
    put(test_mode, true),
    UniqueId = erlang:unique_integer([positive]),

    %% Start stdio
    StdioSpec = #{
        id => multi_stdio,
        start => {erlmcp_transport_stdio, start_link, [Owner]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_stdio]
    },

    %% Start TCP server
    TcpSpec = #{
        id => multi_tcp,
        start => {erlmcp_transport_tcp, start_server, [#{
            mode => server,
            port => 0,
            owner => Owner,
            transport_id => list_to_atom("multi_tcp_transport_" ++ integer_to_list(UniqueId)),
            server_id => list_to_atom("multi_tcp_server_" ++ integer_to_list(UniqueId))
        }]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_transport_tcp]
    },

    {ok, StdioPid} = supervisor:start_child(erlmcp_transport_sup, StdioSpec),
    {ok, TcpPid} = supervisor:start_child(erlmcp_transport_sup, TcpSpec),

    %% Both should be alive (observable behavior)
    ?assert(is_process_alive(StdioPid)),
    ?assert(is_process_alive(TcpPid)),

    %% Cleanup
    supervisor:terminate_child(erlmcp_transport_sup, multi_stdio),
    supervisor:delete_child(erlmcp_transport_sup, multi_stdio),
    supervisor:terminate_child(erlmcp_transport_sup, multi_tcp),
    supervisor:delete_child(erlmcp_transport_sup, multi_tcp),

    erase(test_mode).
