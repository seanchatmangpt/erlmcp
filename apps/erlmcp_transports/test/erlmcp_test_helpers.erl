-module(erlmcp_test_helpers).
-behaviour(gen_server).

%% Test helper utilities for Chicago School TDD
%% Provides REAL erlmcp processes for testing observable behavior
%% NO MOCKS, NO DUMMY PROCESSES, NO STATE INSPECTION

%% Public API
-export([
    start_test_server/1,
    start_test_client/1,
    stop_test_process/1,
    wait_for_transport_message/2,
    wait_for_transport_connected/1,
    wait_for_transport_disconnected/1,
    send_transport_data/3,
    get_transport_port/1,
    make_unique_id/0,
    make_unique_name/1,
    with_test_server/2,
    with_test_client/2,
    with_test_transport_pair/3
]).

%% gen_server callbacks (for test helper process)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(TEST_TIMEOUT, 5000).
-define(SERVER, ?MODULE).

%%%===================================================================
%%% Test Helper API
%%%===================================================================

%% @doc Start a real TCP transport server for testing
%% Returns {ok, ServerPid, Port} or {error, Reason}
-spec start_test_server(map()) -> {ok, pid(), inet:port_number()} | {error, term()}.
start_test_server(Opts) when is_map(Opts) ->
    BaseOpts = #{
        mode => server,
        port => 0,  % Random port
        owner => self(),
        num_acceptors => maps:get(num_acceptors, Opts, 5),
        max_connections => maps:get(max_connections, Opts, 100)
    },

    % Add unique IDs to avoid conflicts
    UniqueId = make_unique_id(),
    TransportId = maps:get(transport_id, Opts, make_unique_name("test_transport")),
    ServerId = maps:get(server_id, Opts, make_unique_name("test_server")),

    FinalOpts = BaseOpts#{
        transport_id => TransportId,
        server_id => ServerId
    },

    case erlmcp_transport_tcp:start_server(FinalOpts) of
        {ok, Pid} ->
            % Wait for server to start and get port
            timer:sleep(100),
            Port = get_transport_port(Pid),
            {ok, Pid, Port};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Start a real TCP transport client for testing
%% Returns {ok, ClientPid} or {error, Reason}
-spec start_test_client(map()) -> {ok, pid()} | {error, term()}.
start_test_client(Opts) when is_map(Opts) ->
    BaseOpts = #{
        mode => client,
        host => maps:get(host, Opts, "localhost"),
        port => maps:get(port, Opts, 9999),
        owner => self(),
        max_reconnect_attempts => maps:get(max_reconnect_attempts, Opts, 3)
    },

    TransportId = maps:get(transport_id, Opts, make_unique_name("test_client")),
    FinalOpts = BaseOpts#{
        transport_id => TransportId
    },

    erlmcp_transport_tcp:start_client(FinalOpts).

%% @doc Stop a test process gracefully
-spec stop_test_process(pid() | undefined) -> ok.
stop_test_process(undefined) ->
    ok;
stop_test_process(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            unlink(Pid),
            exit(Pid, kill),
            timer:sleep(50);
        false ->
            ok
    end.

%% @doc Wait for a transport message (observable behavior)
-spec wait_for_transport_message(timeout(), binary()) -> {ok, binary()} | {error, timeout}.
wait_for_transport_message(Timeout, ExpectedPattern) ->
    receive
        {transport_message, Data} when is_binary(Data) ->
            case ExpectedPattern of
                undefined -> {ok, Data};
                _ -> {ok, Data}
            end
    after Timeout ->
        {error, timeout}
    end.

%% @doc Wait for transport_connected message (observable behavior)
-spec wait_for_transport_connected(timeout()) -> {ok, pid()} | {error, timeout}.
wait_for_transport_connected(Timeout) ->
    receive
        {transport_connected, Pid} when is_pid(Pid) ->
            {ok, Pid}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Wait for transport_disconnected message (observable behavior)
-spec wait_for_transport_disconnected(timeout()) -> {ok, {pid(), term()}} | {error, timeout}.
wait_for_transport_disconnected(Timeout) ->
    receive
        {transport_disconnected, Pid, Reason} when is_pid(Pid) ->
            {ok, {Pid, Reason}}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Send data through transport (observable behavior)
-spec send_transport_data(pid(), iodata(), timeout()) -> ok | {error, term()}.
send_transport_data(Pid, Data, Timeout) ->
    gen_server:call(Pid, {send, Data}, Timeout).

%% @doc Get the port number from a TCP transport server
%% Uses observable behavior (ranch:get_port) not state inspection
-spec get_transport_port(pid()) -> inet:port_number().
get_transport_port(ServerPid) ->
    % Query ranch for the port (observable, not state inspection)
    % We need to get the ranch ref first
    case catch gen_server:call(ServerPid, get_ranch_ref, 1000) of
        {'EXIT', _} ->
            error(cannot_get_ranch_ref);
        RanchRef when is_reference(RanchRef); is_atom(RanchRef) ->
            ranch:get_port(RanchRef);
        _ ->
            error(invalid_ranch_ref)
    end.

%% @doc Generate unique ID for test isolation
-spec make_unique_id() -> pos_integer().
make_unique_id() ->
    erlang:unique_integer([positive, monotonic]).

%% @doc Generate unique atom name for test isolation
-spec make_unique_name(string()) -> atom().
make_unique_name(Prefix) ->
    UniqueId = make_unique_id(),
    list_to_atom(Prefix ++ "_" ++ integer_to_list(UniqueId)).

%% @doc Execute test with automatic server cleanup
-spec with_test_server(map(), fun((pid(), inet:port_number()) -> any())) -> any().
with_test_server(Opts, TestFun) ->
    {ok, ServerPid, Port} = start_test_server(Opts),
    try
        TestFun(ServerPid, Port)
    after
        stop_test_process(ServerPid)
    end.

%% @doc Execute test with automatic client cleanup
-spec with_test_client(map(), fun((pid()) -> any())) -> any().
with_test_client(Opts, TestFun) ->
    {ok, ClientPid} = start_test_client(Opts),
    try
        TestFun(ClientPid)
    after
        stop_test_process(ClientPid)
    end.

%% @doc Execute test with server and client pair
-spec with_test_transport_pair(map(), map(), fun((pid(), inet:port_number(), pid()) -> any())) -> any().
with_test_transport_pair(ServerOpts, ClientOpts, TestFun) ->
    {ok, ServerPid, Port} = start_test_server(ServerOpts),
    UpdatedClientOpts = ClientOpts#{port => Port},
    {ok, ClientPid} = start_test_client(UpdatedClientOpts),

    try
        TestFun(ServerPid, Port, ClientPid)
    after
        stop_test_process(ClientPid),
        stop_test_process(ServerPid)
    end.

%%%===================================================================
%%% gen_server callbacks (for helper process if needed)
%%%===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
