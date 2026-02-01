%%%-------------------------------------------------------------------
%%% @doc
%%% CLI Test Fixtures
%%%
%%% Shared test fixtures for CLI test suites
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_test_fixtures).

%% Exports
-export([
    create_mock_server/0,
    create_mock_server/1,
    create_test_plugin/1,
    create_test_plugin/2,
    create_test_spec/0,
    cleanup_test_files/1,
    wait_for_process/2,
    mock_stdio_transport/0,
    mock_http_transport/0,
    mock_ws_transport/0
]).

%%%====================================================================
%%% Mock Server Creation
%%%====================================================================

create_mock_server() ->
    create_mock_server(#{}).

create_mock_server(Opts) ->
    spawn(fun() -> mock_server_loop(Opts) end).

mock_server_loop(State) ->
    receive
        {get_state, From} ->
            From ! {state, State},
            mock_server_loop(State);
        {set_state, NewState} ->
            mock_server_loop(NewState);
        stop ->
            ok;
        {request, From, _Request} ->
            From ! {response, #{status => success}},
            mock_server_loop(State);
        _ ->
            mock_server_loop(State)
    end.

%%%====================================================================
%%% Plugin Creation
%%%====================================================================

create_test_plugin(Path) ->
    create_test_plugin(Path, "test_plugin").

create_test_plugin(Path, ModuleName) ->
    Code = io_lib:format("-module(~s).
-export([init/1, execute/2]).

init(_Config) -> 
    {ok, #{initialized => true}}.

execute(Command, Args) -> 
    {ok, #{
        command => Command,
        args => Args,
        status => success
    }}.
", [ModuleName]),
    file:write_file(Path, list_to_binary(Code)).

%%%====================================================================
%%% Test Spec Creation
%%%====================================================================

create_test_spec() ->
    #{
        version => <<"2025-11-25">>,
        methods => [
            <<"initialize">>,
            <<"ping">>,
            <<"resources/list">>,
            <<"tools/call">>
        ],
        error_codes => [
            -32700,
            -32600,
            -32601,
            -32602,
            -32603
        ],
        transports => [
            stdio,
            tcp,
            http,
            websocket
        ]
    }.

%%%====================================================================
%%% Cleanup Utilities
%%%====================================================================

cleanup_test_files(Paths) when is_list(Paths) ->
    [cleanup_test_file(Path) || Path <- Paths],
    ok.

cleanup_test_file(Path) ->
    case filelib:is_file(Path) of
        true -> file:delete(Path);
        false -> ok
    end.

%%%====================================================================
%%% Process Utilities
%%%====================================================================

wait_for_process(Pid, Timeout) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            {ok, Reason}
    after Timeout ->
        demonitor(Ref, [flush]),
        {error, timeout}
    end.

%%%====================================================================
%%% Mock Transport Creation
%%%====================================================================

mock_stdio_transport() ->
    #{
        type => stdio,
        send => fun(Data) -> {ok, Data} end,
        receive_fn => fun() -> {ok, <<"{\"jsonrpc\":\"2.0\"}">>} end,
        close => fun() -> ok end
    }.

mock_http_transport() ->
    #{
        type => http,
        port => 8080,
        send => fun(Data) -> {ok, Data} end,
        close => fun() -> ok end
    }.

mock_ws_transport() ->
    #{
        type => websocket,
        url => <<"ws://localhost:8080">>,
        send => fun(Data) -> {ok, Data} end,
        close => fun() -> ok end
    }.
