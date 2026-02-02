%%%-------------------------------------------------------------------
%%% @doc Byzantine Failure Detector
%%% Detects malicious or faulty behavior in agent swarms
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_flow_byzantine).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    suspicions = #{} :: map()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
