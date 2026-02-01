-module(erlmcp_prompt_list_change_notifier).

-behaviour(gen_server).

%% API
-export([start_link/0, notify_prompt_added/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% State record
-record(state, {subscribers = sets:new() :: sets:set(pid())}).

-type state() :: #state{}.

%%%===================================================================
%%% API Functions
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Notify that a prompt has been added with metadata
%% Gap #27: Send prompt added notification with metadata
-spec notify_prompt_added(server_id(), binary(), #mcp_prompt{}, pid() | undefined) -> ok.
notify_prompt_added(ServerId, Name, Prompt, NotifierPid) ->
    gen_server:cast(?MODULE, {notify_prompt_added, ServerId, Name, Prompt, NotifierPid}),
    ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({notify_prompt_added, ServerId, Name, Prompt, _NotifierPid}, State) ->
    % Log the prompt addition for observability
    logger:info("Prompt added to server ~p: ~s", [ServerId, Name]),

    % Notify generic change notifier
    erlmcp_change_notifier:notify_list_changed(prompts),

    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
