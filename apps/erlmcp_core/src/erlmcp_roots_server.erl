-module(erlmcp_roots_server).

-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([start_link/0, add_root/2, list_roots/0, get_root/1, remove_root/1,
         subscribe_root_changes/1, unsubscribe_root_changes/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type root_id() :: binary().
-type root_uri() :: binary().
-type metadata() :: map().
-type subscription_id() :: reference().

%% State record
-record(state,
        {roots_ets :: ets:tid(),
         root_list = [] :: [{root_id(), root_uri(), metadata()}],
         subscribers = #{} :: #{pid() => {reference(), subscription_id()}}}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_root(root_uri(), metadata()) -> ok | {error, already_exists}.
add_root(RootUri, Metadata) when is_binary(RootUri), is_map(Metadata) ->
    gen_server:call(?MODULE, {add_root, RootUri, Metadata}, 5000).

-spec list_roots() -> {ok, [map()]}.
list_roots() ->
    gen_server:call(?MODULE, list_roots, 5000).

-spec get_root(root_id()) -> {ok, map()} | {error, not_found}.
get_root(RootId) when is_binary(RootId) ->
    gen_server:call(?MODULE, {get_root, RootId}, 5000).

-spec remove_root(root_id()) -> ok | {error, not_found}.
remove_root(RootId) when is_binary(RootId) ->
    gen_server:call(?MODULE, {remove_root, RootId}, 5000).

-spec subscribe_root_changes(pid()) -> {ok, subscription_id()}.
subscribe_root_changes(CallbackPid) when is_pid(CallbackPid) ->
    gen_server:call(?MODULE, {subscribe_root_changes, CallbackPid}, 5000).

-spec unsubscribe_root_changes(pid()) -> ok.
unsubscribe_root_changes(CallbackPid) when is_pid(CallbackPid) ->
    gen_server:call(?MODULE, {unsubscribe_root_changes, CallbackPid}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Create ETS table with heir pattern - supervisor inherits on crash
    Tid = ets:new(erlmcp_roots,
                  [set,
                   public,
                   named_table,
                   {read_concurrency, true},
                   {write_concurrency, false},
                   {heir, whereis(erlmcp_core_sup), []}]),

    logger:info("Starting roots server with ETS table ~p", [Tid]),
    {ok, #state{roots_ets = Tid}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({add_root, RootUri, Metadata}, _From, State) ->
    RootId = generate_root_id(RootUri),

    %% Check if root already exists
    case ets:lookup(State#state.roots_ets, RootId) of
        [] ->
            %% Add new root
            Root =
                #{id => RootId,
                  uri => RootUri,
                  metadata => Metadata,
                  created_at => erlang:system_time(millisecond)},
            ets:insert(State#state.roots_ets, {RootId, Root}),

            %% Update root list
            NewRootList = [{RootId, RootUri, Metadata} | State#state.root_list],
            NewState = State#state{root_list = NewRootList},

            %% Notify subscribers
            notify_subscribers(root_added, Root, NewState),

            {reply, ok, NewState};
        [_] ->
            {reply, {error, already_exists}, State}
    end;
handle_call(list_roots, _From, State) ->
    Roots = ets:foldl(fun({_Id, Root}, Acc) -> [Root | Acc] end, [], State#state.roots_ets),
    {reply, {ok, Roots}, State};
handle_call({get_root, RootId}, _From, State) ->
    case ets:lookup(State#state.roots_ets, RootId) of
        [{_, Root}] ->
            {reply, {ok, Root}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({remove_root, RootId}, _From, State) ->
    case ets:lookup(State#state.roots_ets, RootId) of
        [{_, Root}] ->
            ets:delete(State#state.roots_ets, RootId),

            %% Update root list
            NewRootList =
                lists:filter(fun({Id, _Uri, _Meta}) -> Id =/= RootId end, State#state.root_list),
            NewState = State#state{root_list = NewRootList},

            %% Notify subscribers
            notify_subscribers(root_removed, Root, NewState),

            {reply, ok, NewState};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({subscribe_root_changes, CallbackPid}, _From, State) ->
    %% Monitor subscriber for cleanup
    MonitorRef = monitor(process, CallbackPid),
    SubId = make_ref(),

    NewSubscribers = maps:put(CallbackPid, {MonitorRef, SubId}, State#state.subscribers),
    {reply, {ok, SubId}, State#state{subscribers = NewSubscribers}};
handle_call({unsubscribe_root_changes, CallbackPid}, _From, State) ->
    case maps:get(CallbackPid, State#state.subscribers, undefined) of
        undefined ->
            {reply, ok, State};
        {MonitorRef, _SubId} ->
            demonitor(MonitorRef, [flush]),
            NewSubscribers = maps:remove(CallbackPid, State#state.subscribers),
            {reply, ok, State#state{subscribers = NewSubscribers}}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    %% Clean up dead subscriber
    NewSubscribers =
        maps:filter(fun(SubPid, {MRef, _SubId}) -> not (SubPid =:= Pid andalso MRef =:= MonitorRef)
                    end,
                    State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    logger:info("Roots server terminating, cleaning up ETS table"),
    catch ets:delete(State#state.roots_ets),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec generate_root_id(root_uri()) -> root_id().
generate_root_id(RootUri) ->
    %% Generate deterministic ID from URI
    Hash = crypto:hash(sha256, RootUri),
    base64:encode(Hash).

-spec notify_subscribers(atom(), map(), state()) -> ok.
notify_subscribers(EventType, Root, State) ->
    maps:foreach(fun(Pid, {_MonitorRef, SubId}) -> Pid ! {root_change, SubId, EventType, Root} end,
                 State#state.subscribers),
    ok.
