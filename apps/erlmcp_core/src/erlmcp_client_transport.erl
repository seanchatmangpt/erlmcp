%%%-------------------------------------------------------------------
%%% @doc
%%% Simple client transport for testing erlmcp_client.
%%%
%%% This is a minimal transport implementation used by erlmcp_client
%%% when running in isolation (without the full erlmcp_transports app).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_transport).
-behaviour(gen_server).

%% API
-export([start_link/0, send/2, close/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    parent :: pid(),
    messages :: [binary()]
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec send(pid() | term(), binary()) -> ok.
send(_TransportState, _Message) ->
    %% For testing, we just silently accept messages
    %% In a real transport, this would send to the actual transport layer
    ok.

-spec close(term()) -> ok.
close(_TransportState) ->
    ok.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{messages = []}}.

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
