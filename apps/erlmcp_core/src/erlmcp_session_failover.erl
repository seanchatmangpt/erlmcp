%%%-------------------------------------------------------------------
%%% @doc Session Failover Manager (Stub Implementation)
%%%
%%% Temporary stub for session failover across nodes.
%%% TODO: Implement failover with node monitoring.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_session_failover).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    node :: node(),
    failover_targets = [] :: [node()]
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(node()) -> {ok, pid()} | {error, term()}.
start_link(Node) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Node], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Node]) ->
    {ok, #state{node = Node}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
