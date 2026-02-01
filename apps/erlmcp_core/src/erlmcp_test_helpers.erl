-module(erlmcp_test_helpers).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% @doc
%% Comprehensive test infrastructure helpers for erlmcp testing.
%%
%% This module provides utilities for:
%% - Starting test servers with unique IDs to avoid conflicts
%% - Dynamic port allocation to prevent port conflicts
%% - Process cleanup and monitoring
%% - Mock transport setup
%% - Test data generation
%%
%% All operations are designed to support parallel test execution
%% and avoid conflicts between concurrent tests.
%%
%% @end

%% API
-export([start_link/0, generate_server_id/0, generate_server_id/1, generate_client_id/0,
         generate_client_id/1, generate_transport_id/0, generate_transport_id/1, allocate_port/0,
         allocate_port/1, release_port/1, get_free_port/0, start_test_server/1, start_test_server/2,
         start_test_server/3, start_test_client/1, start_test_client/2, stop_test_process/1,
         stop_all_test_processes/0, random_binary/1, random_uri/0, random_uri/1,
         generate_test_resource/0, generate_test_tool/0, generate_test_prompt/0,
         create_mock_transport/0, create_mock_transport/1, setup_test/0, cleanup_test/0,
         with_test_server/2, with_test_client/2, assert_process_alive/1, assert_process_dead/1,
         wait_for_process_death/1, wait_for_process_death/2, sleep/1, timestamp/0, unique_ref/0]).

    %% Unique ID generation

    %% Port management

    %% Process management

    %% Test data generation

    %% Mock transport helpers

    %% Test setup/teardown

    %% Assertions

    %% Utilities

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT_RANGE, {10000, 65000}).

%% State record
-record(state,
        {port_pool :: sets:set(integer()),
         port_allocations = #{} :: map(),  %% Port -> Allocator Pid
         test_processes = sets:new() :: sets:set(pid()),
         id_counters = #{} :: map(),  %% Prefix -> Counter
         allocated_ports = sets:new() :: sets:set(integer())}).

-type state() :: #state{}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @private
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%--------------------------------------------------------------------
%%% Unique ID Generation
%%%--------------------------------------------------------------------

%% @doc Generate a unique server ID for testing.
%% Format: <<"test_server_<timestamp>_<random>">>
-spec generate_server_id() -> binary().
generate_server_id() ->
    gen_server:call(?SERVER, generate_server_id).

%% @doc Generate a unique server ID with a custom prefix.
-spec generate_server_id(binary()) -> binary().
generate_server_id(Prefix) when is_binary(Prefix) ->
    gen_server:call(?SERVER, {generate_server_id, Prefix}).

%% @doc Generate a unique client ID for testing.
-spec generate_client_id() -> binary().
generate_client_id() ->
    generate_client_id(<<"test_client">>).

%% @doc Generate a unique client ID with a custom prefix.
-spec generate_client_id(binary()) -> binary().
generate_client_id(Prefix) when is_binary(Prefix) ->
    gen_server:call(?SERVER, {generate_client_id, Prefix}).

%% @doc Generate a unique transport ID for testing.
-spec generate_transport_id() -> binary().
generate_transport_id() ->
    generate_transport_id(<<"test_transport">>).

%% @doc Generate a unique transport ID with a custom prefix.
-spec generate_transport_id(binary()) -> binary().
generate_transport_id(Prefix) when is_binary(Prefix) ->
    gen_server:call(?SERVER, {generate_transport_id, Prefix}).

%%%--------------------------------------------------------------------
%%% Port Management
%%%--------------------------------------------------------------------

%% @doc Allocate a free port for testing.
%% The port is tracked and can be released later.
-spec allocate_port() -> {ok, integer()} | {error, term()}.
allocate_port() ->
    allocate_port(?DEFAULT_PORT_RANGE).

%% @doc Allocate a port within a specific range.
-spec allocate_port({integer(), integer()}) -> {ok, integer()} | {error, term()}.
allocate_port(Range) ->
    gen_server:call(?SERVER, {allocate_port, Range}).

%% @doc Release a previously allocated port.
-spec release_port(integer()) -> ok.
release_port(Port) when is_integer(Port) ->
    gen_server:cast(?SERVER, {release_port, Port}).

%% @doc Get a free port without tracking (fire and forget).
%% This is a convenience function that doesn't require the helper server.
-spec get_free_port() -> integer().
get_free_port() ->
    {ok, Socket} = gen_tcp:listen(0, [binary, {active, false}]),
    {ok, Port} = inet:port(Socket),
    ok = gen_tcp:close(Socket),
    Port.

%%%--------------------------------------------------------------------
%%% Process Management
%%%--------------------------------------------------------------------

%% @doc Start a test server with unique ID and default capabilities.
-spec start_test_server(binary()) -> {ok, pid()} | {error, term()}.
start_test_server(ServerId) ->
    start_test_server(ServerId, #mcp_server_capabilities{}, #{}).

%% @doc Start a test server with custom capabilities.
-spec start_test_server(binary(), #mcp_server_capabilities{} | map()) ->
                           {ok, pid()} | {error, term()}.
start_test_server(ServerId, Capabilities) ->
    start_test_server(ServerId, Capabilities, #{}).

%% @doc Start a test server with full options.
-spec start_test_server(binary(), #mcp_server_capabilities{} | map(), map()) ->
                           {ok, pid()} | {error, term()}.
start_test_server(ServerId, Capabilities, Options) ->
    case erlmcp_server:start_link(ServerId, Capabilities) of
        {ok, Pid} = Result ->
            gen_server:cast(?SERVER, {register_process, Pid, server, ServerId}),
            Result;
        Error ->
            Error
    end.

%% @doc Start a test client with unique ID and transport.
-spec start_test_client({atom(), map()}) -> {ok, pid()} | {error, term()}.
start_test_client(TransportOpts) ->
    start_test_client(TransportOpts, #{}).

%% @doc Start a test client with custom options.
-spec start_test_client({atom(), map()}, map()) -> {ok, pid()} | {error, term()}.
start_test_client(TransportOpts, ClientOpts) when is_tuple(TransportOpts), is_map(ClientOpts) ->
    case erlmcp_client:start_link(TransportOpts, ClientOpts) of
        {ok, Pid} = Result ->
            gen_server:cast(?SERVER, {register_process, Pid, client, TransportOpts}),
            Result;
        Error ->
            Error
    end.

%% @doc Stop a test process and clean up tracking.
-spec stop_test_process(pid()) -> ok.
stop_test_process(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            case get_process_type(Pid) of
                server ->
                    erlmcp_server:stop(Pid);
                client ->
                    erlmcp_client:stop(Pid);
                _ ->
                    exit(Pid, kill)
            end,
            gen_server:cast(?SERVER, {unregister_process, Pid});
        false ->
            gen_server:cast(?SERVER, {unregister_process, Pid})
    end,
    ok.

%% @doc Stop all tracked test processes.
-spec stop_all_test_processes() -> ok.
stop_all_test_processes() ->
    gen_server:call(?SERVER, stop_all_test_processes).

%%%--------------------------------------------------------------------
%%% Test Data Generation
%%%--------------------------------------------------------------------

%% @doc Generate a random binary of specified length.
-spec random_binary(pos_integer()) -> binary().
random_binary(Length) when is_integer(Length), Length > 0 ->
    << <<(rand:uniform(255))>> || _ <- lists:seq(1, Length) >>.

%% @doc Generate a random URI for testing.
-spec random_uri() -> binary().
random_uri() ->
    random_uri(<<"test">>).

%% @doc Generate a random URI with a custom scheme.
-spec random_uri(binary()) -> binary().
random_uri(Scheme) when is_binary(Scheme) ->
    Unique = unique_ref(),
    <<Scheme/binary,
      "://",
      Unique/binary,
      "/",
      (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

%% @doc Generate a test resource with random data.
-spec generate_test_resource() -> {binary(), fun()}.
generate_test_resource() ->
    Uri = random_uri(),
    Handler = fun(_) -> {ok, #{<<"data">> => random_binary(16)}} end,
    {Uri, Handler}.

%% @doc Generate a test tool with random data.
-spec generate_test_tool() -> {binary(), fun()}.
generate_test_tool() ->
    Name = <<"tool_", (random_binary(8))/binary>>,
    Handler = fun(_, _) -> #{result => <<"ok">>} end,
    {Name, Handler}.

%% @doc Generate a test prompt with random data.
-spec generate_test_prompt() -> {binary(), fun()}.
generate_test_prompt() ->
    Name = <<"prompt_", (random_binary(8))/binary>>,
    Handler =
        fun(_) -> [#{role => <<"user">>, content => #{type => <<"text">>, text => <<"test">>}}] end,
    {Name, Handler}.

%%%--------------------------------------------------------------------
%%% Mock Transport Helpers
%%%--------------------------------------------------------------------

%% @doc Create a mock transport for testing.
-spec create_mock_transport() -> {module(), map()}.
create_mock_transport() ->
    create_mock_transport(#{}).

%% @doc Create a mock transport with custom configuration.
-spec create_mock_transport(map()) -> {module(), map()}.
create_mock_transport(Options) when is_map(Options) ->
    MockModule =
        case maps:get(type, Options, stdio) of
            stdio ->
                erlmcp_transport_stdio;
            tcp ->
                erlmcp_transport_tcp;
            http ->
                erlmcp_transport_http;
            _ ->
                erlmcp_transport_stdio
        end,
    Config = maps:merge(#{test_mode => true, mock_responses => []}, Options),
    {MockModule, Config}.

%%%--------------------------------------------------------------------
%%% Test Setup/Teardown
%%%--------------------------------------------------------------------

%% @doc Setup test environment.
%% Ensures applications are started and helper server is available.
-spec setup_test() -> ok.
setup_test() ->
    %% Ensure erlmcp_core application is started
    application:ensure_all_started(erlmcp_core),

    %% Ensure helper server is running
    case whereis(?SERVER) of
        undefined ->
            case start_link() of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _}} ->
                    ok
            end;
        _Pid ->
            ok
    end,
    ok.

%% @doc Cleanup test environment.
%% Stops all tracked test processes but keeps helper server alive.
-spec cleanup_test() -> ok.
cleanup_test() ->
    stop_all_test_processes(),
    ok.

%% @doc Execute a function with a test server, ensuring cleanup.
-spec with_test_server(binary(), fun((pid()) -> any())) -> any().
with_test_server(ServerId, Fun) when is_binary(ServerId), is_function(Fun, 1) ->
    UniqueId = generate_server_id(ServerId),
    {ok, Server} = start_test_server(UniqueId),
    try
        Fun(Server)
    after
        stop_test_process(Server)
    end.

%% @doc Execute a function with a test client, ensuring cleanup.
-spec with_test_client({atom(), map()}, fun((pid()) -> any())) -> any().
with_test_client(TransportOpts, Fun) when is_tuple(TransportOpts), is_function(Fun, 1) ->
    {ok, Client} = start_test_client(TransportOpts),
    try
        Fun(Client)
    after
        stop_test_process(Client)
    end.

%%%--------------------------------------------------------------------
%%% Assertions
%%%--------------------------------------------------------------------

%% @doc Assert that a process is alive.
-spec assert_process_alive(pid()) -> true.
assert_process_alive(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        true ->
            true;
        false ->
            error({process_dead, Pid})
    end.

%% @doc Assert that a process is dead.
-spec assert_process_dead(pid()) -> true.
assert_process_dead(Pid) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        false ->
            true;
        true ->
            error({process_alive, Pid})
    end.

%% @doc Wait for a process to die (default 5 seconds).
-spec wait_for_process_death(pid()) -> ok.
wait_for_process_death(Pid) ->
    wait_for_process_death(Pid, 5000).

%% @doc Wait for a process to die with custom timeout.
-spec wait_for_process_death(pid(), timeout()) -> ok.
wait_for_process_death(Pid, Timeout) when is_pid(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    after Timeout ->
        demonitor(Ref, [flush]),
        error({timeout_waiting_for_death, Pid})
    end.

%%%--------------------------------------------------------------------
%%% Utilities
%%%--------------------------------------------------------------------

%% @doc Sleep for specified milliseconds.
-spec sleep(pos_integer()) -> ok.
sleep(Ms) when is_integer(Ms), Ms > 0 ->
    timer:sleep(Ms).

%% @doc Get current timestamp as integer (microseconds).
-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(microsecond).

%% @doc Generate a unique reference binary.
-spec unique_ref() -> binary().
unique_ref() ->
    {Mega, Secs, Micro} = erlang:timestamp(),
    Unique = erlang:unique_integer([positive]),
    iolist_to_binary([integer_to_binary(Mega),
                      <<"_">>,
                      integer_to_binary(Secs),
                      <<"_">>,
                      integer_to_binary(Micro),
                      <<"_">>,
                      integer_to_binary(Unique)]).

%%%--------------------------------------------------------------------
%%% Internal functions
%%%--------------------------------------------------------------------

%% @private Get the type of a tracked process.
-spec get_process_type(pid()) -> atom() | undefined.
get_process_type(Pid) ->
    gen_server:call(?SERVER, {get_process_type, Pid}).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

%% @private
init([]) ->
    process_flag(trap_exit, true),
    %% Initialize ETS table for process tracking
    State =
        #state{port_pool = initialize_port_pool(),
               test_processes = sets:new(),
               id_counters = #{},
               allocated_ports = sets:new()},
    {ok, State}.

%% @private
handle_call(generate_server_id, _From, State) ->
    ServerId = generate_unique_id(<<"server">>, State),
    {reply, ServerId, State};
handle_call({generate_server_id, Prefix}, _From, State) ->
    ServerId = generate_unique_id(Prefix, State),
    {reply, ServerId, State};
handle_call({generate_client_id, Prefix}, _From, State) ->
    ClientId = generate_unique_id(Prefix, State),
    {reply, ClientId, State};
handle_call({generate_transport_id, Prefix}, _From, State) ->
    TransportId = generate_unique_id(Prefix, State),
    {reply, TransportId, State};
handle_call({allocate_port, {Min, Max}}, _From, #state{allocated_ports = Allocated} = State) ->
    case find_free_port(Min, Max, Allocated) of
        {ok, Port} ->
            NewAllocated = sets:add_element(Port, Allocated),
            NewState = State#state{allocated_ports = NewAllocated},
            {reply, {ok, Port}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(stop_all_test_processes, _From, #state{test_processes = Processes} = State) ->
    %% Stop all tracked processes
    PidList = sets:to_list(Processes),
    lists:foreach(fun(Pid) ->
                     case erlang:is_process_alive(Pid) of
                         true ->
                             try
                                 stop_test_process(Pid)
                             catch
                                 _:_ ->
                                     ok
                             end;
                         false ->
                             ok
                     end
                  end,
                  PidList),
    NewState = State#state{test_processes = sets:new()},
    {reply, ok, NewState};
handle_call({get_process_type, Pid}, _From, State) ->
    %% Look up process type from ETS table or internal state
    Type =
        case get_process_entry(Pid, State) of
            {_, ProcessType, _} ->
                ProcessType;
            undefined ->
                undefined
        end,
    {reply, Type, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({register_process, Pid, Type, Id}, #state{test_processes = Processes} = State) ->
    %% Register process for tracking
    MonitorRef = monitor(process, Pid),
    %% Store process info (in real implementation, use ETS)
    NewProcesses = sets:add_element(Pid, Processes),
    {noreply, State#state{test_processes = NewProcesses}};
handle_cast({unregister_process, Pid}, #state{test_processes = Processes} = State) ->
    NewProcesses = sets:del_element(Pid, Processes),
    {noreply, State#state{test_processes = NewProcesses}};
handle_cast({release_port, Port}, #state{allocated_ports = Allocated} = State) ->
    NewAllocated = sets:del_element(Port, Allocated),
    {noreply, State#state{allocated_ports = NewAllocated}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', MonitorRef, process, Pid, _Reason},
            #state{test_processes = Processes} = State) ->
    %% Clean up process that died
    NewProcesses = sets:del_element(Pid, Processes),
    {noreply, State#state{test_processes = NewProcesses}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Helper Functions
%%%====================================================================

%% @private Initialize the port pool with available ports.
-spec initialize_port_pool() -> sets:set(integer()).
initialize_port_pool() ->
    %% Create a pool of ports in the valid range
    {Min, Max} = ?DEFAULT_PORT_RANGE,
    sets:from_list(
        lists:seq(Min, Max)).

%% @private Find a free port in the specified range.
-spec find_free_port(integer(), integer(), sets:set(integer())) ->
                        {ok, integer()} | {error, term()}.
find_free_port(Min, Max, Allocated) ->
    %% Try to bind to ports in the range
    find_free_port_loop(Min, Max, Allocated, 100).

%% @private Loop to find a free port with max attempts.
-spec find_free_port_loop(integer(), integer(), sets:set(integer()), integer()) ->
                             {ok, integer()} | {error, term()}.
find_free_port_loop(_Min, _Max, _Allocated, 0) ->
    {error, no_free_port};
find_free_port_loop(Min, Max, Allocated, Attempts) ->
    Port = Min + rand:uniform(Max - Min),
    case sets:is_element(Port, Allocated) of
        true ->
            %% Port already allocated, try again
            find_free_port_loop(Min, Max, Allocated, Attempts - 1);
        false ->
            %% Try to bind to verify port is actually free
            case gen_tcp:listen(Port, [binary, {active, false}]) of
                {ok, Socket} ->
                    ok = gen_tcp:close(Socket),
                    {ok, Port};
                {error, _} ->
                    %% Port in use by system, try again
                    find_free_port_loop(Min, Max, Allocated, Attempts - 1)
            end
    end.

%% @private Generate a unique ID with prefix and timestamp.
-spec generate_unique_id(binary(), #state{}) -> binary().
generate_unique_id(Prefix, #state{id_counters = Counters}) ->
    Counter = maps:get(Prefix, Counters, 0) + 1,
    Timestamp = integer_to_binary(timestamp()),
    Random = random_binary(4),
    <<Prefix/binary,
      "_",
      Timestamp/binary,
      "_",
      Random/binary,
      "_",
      (integer_to_binary(Counter))/binary>>.

%% @private Get process entry from tracking.
-spec get_process_entry(pid(), #state{}) -> {pid(), atom(), term()} | undefined.
get_process_entry(_Pid, _State) ->
    %% In a full implementation, this would query an ETS table
    %% For now, return undefined
    undefined.
