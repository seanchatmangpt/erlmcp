-module(erlmcp_change_notifier).

-behaviour(gen_server).

%% API
-export([start_link/0, notify_list_changed/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {subscribers = sets:new() :: sets:set(pid())}).

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

notify_list_changed(Feature) when is_atom(Feature) ->
    gen_server:cast(?MODULE, {notify_list_changed, Feature}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify_list_changed, _Feature}, State) ->
    % In a real implementation, this would notify subscribers
    % For now, just log and continue
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
