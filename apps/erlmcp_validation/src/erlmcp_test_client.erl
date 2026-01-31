%%%-------------------------------------------------------------------
%%% @doc Minimal Test Client for MCP Validation
%%%
%%% This is a minimal implementation providing only the functions required
%%% by erlmcp_performance_validator to avoid xref warnings.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_test_client).
-behaviour(gen_server).

%% API exports - Only functions required by performance_validator
-export([
    start_test_client/2,
    stop_test_server/1,
    send_request/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    transport_type,
    config,
    connected
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start test client with specified transport type and configuration
-spec start_test_client(atom(), map()) -> {ok, pid()} | {error, term()}.
start_test_client(TransportType, Config) when is_atom(TransportType), is_map(Config) ->
    gen_server:start_link(?MODULE, [TransportType, Config], []).

%% @doc Stop test server
-spec stop_test_server(pid()) -> ok.
stop_test_server(ServerRef) when is_pid(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Send a single request through the test client
-spec send_request(pid(), map()) -> {ok, map()} | {error, term()}.
send_request(ServerRef, Request) when is_pid(ServerRef), is_map(Request) ->
    gen_server:call(ServerRef, {send_request, Request}, infinity).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, #state{}}.
init([TransportType, Config]) ->
    {ok, #state{
        transport_type = TransportType,
        config = Config,
        connected = false
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({send_request, _Request}, _From, State) ->
    %% Minimal response - return empty map
    {reply, {ok, #{}}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
