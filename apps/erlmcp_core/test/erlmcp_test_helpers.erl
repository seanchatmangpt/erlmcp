-module(erlmcp_test_helpers).
-behaviour(gen_server).

%%====================================================================
%% Chicago School TDD Test Helpers for erlmcp
%%====================================================================
%%
%% Module providing test infrastructure for erlmcp following Chicago
%% School TDD principles:
%%
%%   - Test observable behavior through ALL interfaces
%%   - Use REAL erlmcp processes (NO mocked/stubbed versions)
%%   - NO internal state inspection (test API boundaries only)
%%   - NO record duplication (respect encapsulation)
%%
%% Functions provide setup/cleanup wrappers and helpers for starting
%% real erlmcp_server and erlmcp_client processes in tests.
%%
%% Usage Example:
%%   % Setup/cleanup wrapper
%%   Result = erlmcp_test_helpers:with_test_server(fun(ServerPid) ->
%%       {ok, Tools} = erlmcp_server:list_tools(ServerPid),
%%       length(Tools)
%%   end).
%%
%%   % Manual setup
%%   {ok, ServerPid} = erlmcp_test_helpers:start_test_server(),
%%   {ok, ClientPid} = erlmcp_test_helpers:start_test_client(),
%%   ... perform tests ...
%%   ok = erlmcp_test_helpers:stop_test_server(ServerPid),
%%   ok = erlmcp_test_helpers:stop_test_client(ClientPid).
%%
%%====================================================================

%% API exports
-export([
    start_test_server/0,
    start_test_server/1,
    start_test_server/2,
    start_test_client/0,
    start_test_client/1,
    start_test_client/2,
    stop_test_server/1,
    stop_test_server/2,
    stop_test_client/1,
    stop_test_client/2,
    with_test_server/1,
    with_test_server/2,
    with_test_client/1,
    with_test_client/2
]).

%% gen_server callbacks (for test coordinator process)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%====================================================================
%% Types
%%====================================================================

-type server_id() :: binary().
-type server_config() :: #{
    capabilities => #mcp_server_capabilities{},
    server_id => server_id(),
    timeout => timeout()
}.
-type client_config() :: #{
    transport => {stdio, list()} | {tcp, map()} | {http, map()},
    timeout => timeout()
}.
-type test_result() :: term().
-type timeout_ms() :: pos_integer().

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_server with default capabilities.
%%
%% Creates a real erlmcp_server process with all capabilities enabled:
%%   - resources: enabled
%%   - tools: enabled
%%   - prompts: enabled
%%
%% Returns {ok, ServerPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_server process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_server() -> {ok, pid()} | {error, term()}.
start_test_server() ->
    start_test_server(<<"test_server_default">>).

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_server with a specific server ID.
%%
%% Server ID must be a binary. Uses default capabilities (all enabled).
%%
%% Returns {ok, ServerPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_server process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_server(server_id()) -> {ok, pid()} | {error, term()}.
start_test_server(ServerId) when is_binary(ServerId) ->
    DefaultCaps = #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    },
    start_test_server(ServerId, DefaultCaps).

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_server with specific capabilities.
%%
%% Parameters:
%%   - ServerId: Binary identifier for the server
%%   - Capabilities: #mcp_server_capabilities{} record
%%
%% Returns {ok, ServerPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_server process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_server(server_id(), #mcp_server_capabilities{}) ->
    {ok, pid()} | {error, term()}.
start_test_server(ServerId, Capabilities) when is_binary(ServerId) ->
    case erlmcp_server:start_link(ServerId, Capabilities) of
        {ok, ServerPid} when is_pid(ServerPid) ->
            {ok, ServerPid};
        {error, Reason} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_client with default configuration.
%%
%% Creates a real erlmcp_client process with stdio transport.
%%
%% Returns {ok, ClientPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_client process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_client() -> {ok, pid()} | {error, term()}.
start_test_client() ->
    start_test_client(#{}).

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_client with specific configuration.
%%
%% Configuration map options:
%%   - transport: {stdio, Opts}, {tcp, Opts}, or {http, Opts}
%%   - timeout: Request timeout in milliseconds (default: 5000)
%%
%% Returns {ok, ClientPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_client process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_client(client_config()) -> {ok, pid()} | {error, term()}.
start_test_client(Config) when is_map(Config) ->
    Transport = maps:get(transport, Config, {stdio, []}),
    Timeout = maps:get(timeout, Config, 5000),
    ClientOpts = #{timeout => Timeout},
    case erlmcp_client:start_link(Transport, ClientOpts) of
        {ok, ClientPid} when is_pid(ClientPid) ->
            {ok, ClientPid};
        {error, Reason} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Start a test erlmcp_client with transport and options.
%%
%% Parameters:
%%   - Transport: {stdio, Opts}, {tcp, Opts}, or {http, Opts}
%%   - Options: Client options map (e.g., #{timeout => 5000})
%%
%% Returns {ok, ClientPid} on success, {error, Reason} on failure.
%%
%% Chicago School: Uses REAL erlmcp_client process (no mocks).
%% @end
%%--------------------------------------------------------------------
-spec start_test_client(term(), map()) -> {ok, pid()} | {error, term()}.
start_test_client(Transport, Options) when is_map(Options) ->
    erlmcp_client:start_link(Transport, Options).

%%--------------------------------------------------------------------
%% @doc Stop a test erlmcp_server with default timeout.
%%
%% Gracefully terminates the server process. Uses 5000ms default timeout.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% Chicago School: Clean shutdown processes (no lingering processes).
%% @end
%%--------------------------------------------------------------------
-spec stop_test_server(pid()) -> ok | {error, term()}.
stop_test_server(ServerPid) when is_pid(ServerPid) ->
    stop_test_server(ServerPid, 5000).

%%--------------------------------------------------------------------
%% @doc Stop a test erlmcp_server with custom timeout.
%%
%% Gracefully terminates the server process with specified timeout.
%%
%% Parameters:
%%   - ServerPid: PID of the server process
%%   - Timeout: Timeout in milliseconds
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% Chicago School: Clean shutdown processes (no lingering processes).
%% @end
%%--------------------------------------------------------------------
-spec stop_test_server(pid(), timeout_ms()) -> ok | {error, term()}.
stop_test_server(ServerPid, Timeout) when is_pid(ServerPid), is_integer(Timeout) ->
    try
        case erlmcp_server:stop(ServerPid) of
            ok ->
                %% Wait for process to terminate
                wait_for_death(ServerPid, Timeout),
                ok;
            {error, Reason} = Error ->
                Error
        end
    catch
        _:_ ->
            %% Process already dead or not an erlmcp_server
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Stop a test erlmcp_client with default timeout.
%%
%% Gracefully terminates the client process. Uses 5000ms default timeout.
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% Chicago School: Clean shutdown processes (no lingering processes).
%% @end
%%--------------------------------------------------------------------
-spec stop_test_client(pid()) -> ok | {error, term()}.
stop_test_client(ClientPid) when is_pid(ClientPid) ->
    stop_test_client(ClientPid, 5000).

%%--------------------------------------------------------------------
%% @doc Stop a test erlmcp_client with custom timeout.
%%
%% Gracefully terminates the client process with specified timeout.
%%
%% Parameters:
%%   - ClientPid: PID of the client process
%%   - Timeout: Timeout in milliseconds
%%
%% Returns ok on success, {error, Reason} on failure.
%%
%% Chicago School: Clean shutdown processes (no lingering processes).
%% @end
%%--------------------------------------------------------------------
-spec stop_test_client(pid(), timeout_ms()) -> ok | {error, term()}.
stop_test_client(ClientPid, Timeout) when is_pid(ClientPid), is_integer(Timeout) ->
    try
        case erlmcp_client:stop(ClientPid) of
            ok ->
                %% Wait for process to terminate
                wait_for_death(ClientPid, Timeout),
                ok;
            {error, Reason} = Error ->
                Error
        end
    catch
        _:_ ->
            %% Process already dead or not an erlmcp_client
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Setup/cleanup wrapper for test server with defaults.
%%
%% Automatically starts a test server, executes the test function,
%% and ensures cleanup even if the test fails.
%%
%% Test function receives ServerPid as argument.
%%
%% Example:
%%   Result = erlmcp_test_helpers:with_test_server(fun(ServerPid) ->
%%       {ok, Tools} = erlmcp_server:list_tools(ServerPid),
%%       length(Tools)
%%   end).
%%
%% Returns the result of the test function.
%%
%% Chicago School: Automatic cleanup prevents resource leaks.
%% @end
%%--------------------------------------------------------------------
-spec with_test_server(fun((pid()) -> test_result())) -> test_result().
with_test_server(TestFun) when is_function(TestFun, 1) ->
    {ok, ServerPid} = start_test_server(),
    try
        TestFun(ServerPid)
    after
        stop_test_server(ServerPid)
    end.

%%--------------------------------------------------------------------
%% @doc Setup/cleanup wrapper for test server with capabilities.
%%
%% Automatically starts a test server with specific capabilities,
%% executes the test function, and ensures cleanup even if the test fails.
%%
%% Parameters:
%%   - ServerId: Binary server ID
%%   - Capabilities: #mcp_server_capabilities{} record
%%   - TestFun: Function receiving ServerPid
%%
%% Example:
%%   Caps = #mcp_server_capabilities{
%%       tools = #mcp_capability{enabled = true}
%%   },
%%   Result = erlmcp_test_helpers:with_test_server(
%%       <<"my_test">>, Caps,
%%       fun(ServerPid) ->
%%           {ok, Tools} = erlmcp_server:list_tools(ServerPid),
%%           length(Tools)
%%       end).
%%
%% Returns the result of the test function.
%%
%% Chicago School: Automatic cleanup prevents resource leaks.
%% @end
%%--------------------------------------------------------------------
-spec with_test_server(server_id(), #mcp_server_capabilities{},
                       fun((pid()) -> test_result())) -> test_result().
with_test_server(ServerId, Capabilities, TestFun)
  when is_binary(ServerId), is_function(TestFun, 1) ->
    {ok, ServerPid} = start_test_server(ServerId, Capabilities),
    try
        TestFun(ServerPid)
    after
        stop_test_server(ServerPid)
    end.

%%--------------------------------------------------------------------
%% @doc Setup/cleanup wrapper for test client with defaults.
%%
%% Automatically starts a test client, executes the test function,
%% and ensures cleanup even if the test fails.
%%
%% Test function receives ClientPid as argument.
%%
%% Example:
%%   Result = erlmcp_test_helpers:with_test_client(fun(ClientPid) ->
%%       {ok, Prompts} = erlmcp_client:list_prompts(ClientPid),
%%       length(Prompts)
%%   end).
%%
%% Returns the result of the test function.
%%
%% Chicago School: Automatic cleanup prevents resource leaks.
%% @end
%%--------------------------------------------------------------------
-spec with_test_client(fun((pid()) -> test_result())) -> test_result().
with_test_client(TestFun) when is_function(TestFun, 1) ->
    {ok, ClientPid} = start_test_client(),
    try
        TestFun(ClientPid)
    after
        stop_test_client(ClientPid)
    end.

%%--------------------------------------------------------------------
%% @doc Setup/cleanup wrapper for test client with configuration.
%%
%% Automatically starts a test client with specific configuration,
%% executes the test function, and ensures cleanup even if the test fails.
%%
%% Parameters:
%%   - Config: Client configuration map
%%   - TestFun: Function receiving ClientPid
%%
%% Example:
%%   Config = #{transport => {stdio, []}, timeout => 10000},
%%   Result = erlmcp_test_helpers:with_test_client(Config, fun(ClientPid) ->
%%       {ok, Tools} = erlmcp_client:list_tools(ClientPid),
%%       length(Tools)
%%   end).
%%
%% Returns the result of the test function.
%%
%% Chicago School: Automatic cleanup prevents resource leaks.
%% @end
%%--------------------------------------------------------------------
-spec with_test_client(client_config(), fun((pid()) -> test_result())) ->
    test_result().
with_test_client(Config, TestFun) when is_map(Config), is_function(TestFun, 1) ->
    {ok, ClientPid} = start_test_client(Config),
    try
        TestFun(ClientPid)
    after
        stop_test_client(ClientPid)
    end.

%%====================================================================
%% gen_server callbacks (for test coordinator)
%%====================================================================

%% @private
-spec init([]) -> {ok, #{}}.
init([]) ->
    {ok, #{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, map()) ->
    {reply, ok, map()}.
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private
-spec handle_cast(term(), map()) -> {noreply, map()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), map()) -> {noreply, map()}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), map()) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), map(), term()) -> {ok, map()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Wait for a process to terminate (with timeout).
-spec wait_for_death(pid(), timeout_ms()) -> true | timeout.
wait_for_death(Pid, Timeout) when is_pid(Pid), is_integer(Timeout) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Reason} ->
            true
    after Timeout ->
        demonitor(Ref, [flush]),
        timeout
    end.
