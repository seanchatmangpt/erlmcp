%%%-------------------------------------------------------------------
%%% @doc Test Server for Common Test Compatibility Suite
%%%
%%% Simple gen_server for testing crash recovery and supervision.
%%% Used by erlmcp_ct_compat_SUITE.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(test_server).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the test server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Stop the test server
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(crash, _From, State) ->
    error(intentional_crash),
    {reply, ok, State};

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
