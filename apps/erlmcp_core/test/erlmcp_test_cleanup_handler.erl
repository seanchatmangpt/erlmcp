%%%-------------------------------------------------------------------
%%% @doc Test cleanup handler for cancellation tests
%%%
%%% This module provides cleanup operation handlers for testing cancellation
%%% functionality. Implements the erlmcp_cancellation cleanup handler contract.
%%%-------------------------------------------------------------------
-module(erlmcp_test_cleanup_handler).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([set_tracker/1]).
-export([cleanup_operation/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    tracker_pid :: pid() | undefined
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the cleanup handler server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Set the tracker process to receive cleanup notifications
set_tracker(TrackerPid) when is_pid(TrackerPid) ->
    gen_server:cast(?MODULE, {set_tracker, TrackerPid}).

%% @doc Cleanup operation callback (called by erlmcp_cancellation)
cleanup_operation(Token, Reason) ->
    gen_server:cast(?MODULE, {cleanup, Token, Reason}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{tracker_pid = undefined}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({set_tracker, TrackerPid}, State) ->
    {noreply, State#state{tracker_pid = TrackerPid}};

handle_cast({cleanup, Token, Reason}, #state{tracker_pid = TrackerPid} = State) ->
    case TrackerPid of
        undefined ->
            ok;
        _ when is_pid(TrackerPid) ->
            try
                TrackerPid ! {cleanup, Token, Reason}
            catch
                _:_ -> ok
            end
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
