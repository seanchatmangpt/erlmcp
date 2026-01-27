%%%-------------------------------------------------------------------
%%% @doc
%%% Resource Subscriptions Manager
%%%
%%% Manages resource subscriptions with automatic cleanup on client disconnection.
%%% Uses ETS for efficient lookup of subscriptions by resource URI.
%%%
%%% Subscriptions are stored as:
%%%   {resource_uri => set(client_pids)}
%%%
%%% Process monitoring ensures automatic cleanup when clients disconnect.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_resource_subscriptions).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    subscribe/2,
    unsubscribe/2,
    get_subscribers/1,
    notify_updated/2,
    notify_deleted/1,
    list_subscriptions/0,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type resource_uri() :: binary().
-type client_pid() :: pid().
-type subscription_data() :: #{
    clients := sets:set(client_pid()),
    monitored := sets:set(pid())
}.

%% State record
-record(subscription_state, {
    subscriptions = #{} :: #{resource_uri() => subscription_data()},
    monitors = #{} :: #{pid() => [resource_uri()]},
    ets_table :: atom()
}).

-type state() :: #subscription_state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the resource subscriptions manager as a singleton gen_server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Subscribe a client to resource updates
%% Automatically monitors the client process for cleanup
-spec subscribe(resource_uri(), client_pid()) -> ok | {error, term()}.
subscribe(ResourceUri, ClientPid) when is_binary(ResourceUri), is_pid(ClientPid) ->
    gen_server:call(?MODULE, {subscribe, ResourceUri, ClientPid}).

%% @doc Unsubscribe a client from resource updates
-spec unsubscribe(resource_uri(), client_pid()) -> ok | {error, term()}.
unsubscribe(ResourceUri, ClientPid) when is_binary(ResourceUri), is_pid(ClientPid) ->
    gen_server:call(?MODULE, {unsubscribe, ResourceUri, ClientPid}).

%% @doc Get all subscribers for a specific resource
-spec get_subscribers(resource_uri()) -> {ok, [client_pid()]} | {error, term()}.
get_subscribers(ResourceUri) when is_binary(ResourceUri) ->
    gen_server:call(?MODULE, {get_subscribers, ResourceUri}).

%% @doc Notify all subscribers of resource update
%% Sends notifications: {resource_updated, Uri, Metadata}
-spec notify_updated(resource_uri(), map()) -> ok.
notify_updated(ResourceUri, Metadata) when is_binary(ResourceUri), is_map(Metadata) ->
    gen_server:cast(?MODULE, {notify_updated, ResourceUri, Metadata}).

%% @doc Notify all subscribers that resource has been deleted
%% Sends notifications: {resource_deleted, Uri}
-spec notify_deleted(resource_uri()) -> ok.
notify_deleted(ResourceUri) when is_binary(ResourceUri) ->
    gen_server:cast(?MODULE, {notify_deleted, ResourceUri}).

%% @doc List all current subscriptions
-spec list_subscriptions() -> {ok, [{resource_uri(), [client_pid()]}]}.
list_subscriptions() ->
    gen_server:call(?MODULE, list_subscriptions).

%% @doc Stop the subscriptions manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    logger:info("Starting resource subscriptions manager"),
    State = #subscription_state{},
    {ok, State}.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.

handle_call({subscribe, ResourceUri, ClientPid}, _From, State) ->
    logger:debug("Subscribe ~p to resource ~p", [ClientPid, ResourceUri]),
    case do_subscribe(ResourceUri, ClientPid, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} = Error ->
            logger:error("Failed to subscribe ~p to ~p: ~p", [ClientPid, ResourceUri, Reason]),
            {reply, Error, State}
    end;

handle_call({unsubscribe, ResourceUri, ClientPid}, _From, State) ->
    logger:debug("Unsubscribe ~p from resource ~p", [ClientPid, ResourceUri]),
    NewState = do_unsubscribe(ResourceUri, ClientPid, State),
    {reply, ok, NewState};

handle_call({get_subscribers, ResourceUri}, _From, State) ->
    Subs = maps:get(ResourceUri, State#subscription_state.subscriptions, undefined),
    case Subs of
        undefined ->
            {reply, {ok, []}, State};
        #{clients := Clients} ->
            {reply, {ok, sets:to_list(Clients)}, State}
    end;

handle_call(list_subscriptions, _From, State) ->
    Subs = maps:fold(fun(Uri, #{clients := Clients}, Acc) ->
        [{Uri, sets:to_list(Clients)} | Acc]
    end, [], State#subscription_state.subscriptions),
    {reply, {ok, Subs}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({notify_updated, ResourceUri, Metadata}, State) ->
    case maps:get(ResourceUri, State#subscription_state.subscriptions, undefined) of
        undefined ->
            logger:debug("No subscribers for updated resource ~p", [ResourceUri]);
        #{clients := Clients} ->
            logger:debug("Notifying ~p clients of resource update ~p", [sets:size(Clients), ResourceUri]),
            sets:fold(fun(ClientPid, _) ->
                try
                    ClientPid ! {resource_updated, ResourceUri, Metadata}
                catch
                    error:_ -> ok
                end,
                ok
            end, ok, Clients)
    end,
    {noreply, State};

handle_cast({notify_deleted, ResourceUri}, State) ->
    case maps:get(ResourceUri, State#subscription_state.subscriptions, undefined) of
        undefined ->
            logger:debug("No subscribers for deleted resource ~p", [ResourceUri]);
        #{clients := Clients} ->
            logger:debug("Notifying ~p clients of resource deletion ~p", [sets:size(Clients), ResourceUri]),
            sets:fold(fun(ClientPid, _) ->
                try
                    ClientPid ! {resource_deleted, ResourceUri}
                catch
                    error:_ -> ok
                end,
                ok
            end, ok, Clients),
            %% Clean up subscriptions for this resource
            NewSubscriptions = maps:remove(ResourceUri, State#subscription_state.subscriptions),
            {noreply, State#subscription_state{subscriptions = NewSubscriptions}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info({'DOWN', MonitorRef, process, Pid, _Reason}, State) ->
    logger:debug("Client process down: ~p (monitor: ~p)", [Pid, MonitorRef]),
    NewState = cleanup_client_subscriptions(Pid, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(Reason, _State) ->
    logger:info("Resource subscriptions manager terminating: ~p", [Reason]),
    ok.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Subscribe a client to a resource
-spec do_subscribe(resource_uri(), client_pid(), state()) ->
    {ok, state()} | {error, term()}.
do_subscribe(ResourceUri, ClientPid, State) ->
    try
        Subscriptions = State#subscription_state.subscriptions,
        Monitors = State#subscription_state.monitors,

        %% Get or create subscription data for this resource
        SubData = maps:get(ResourceUri, Subscriptions, #{
            clients => sets:new(),
            monitored => sets:new()
        }),
        #{clients := Clients, monitored := Monitored} = SubData,

        %% Check if already subscribed
        case sets:is_element(ClientPid, Clients) of
            true ->
                %% Already subscribed
                {ok, State};
            false ->
                %% Add client to subscribers
                NewClients = sets:add_element(ClientPid, Clients),

                %% Monitor the client if not already monitored
                NewMonitored = case sets:is_element(ClientPid, Monitored) of
                    true -> Monitored;
                    false ->
                        _MonitorRef = erlang:monitor(process, ClientPid),
                        sets:add_element(ClientPid, Monitored)
                end,

                NewSubData = SubData#{
                    clients => NewClients,
                    monitored => NewMonitored
                },
                NewSubscriptions = maps:put(ResourceUri, NewSubData, Subscriptions),

                %% Track which resources this client is subscribed to
                ClientSubs = maps:get(ClientPid, Monitors, []),
                NewClientSubs = case lists:member(ResourceUri, ClientSubs) of
                    true -> ClientSubs;
                    false -> [ResourceUri | ClientSubs]
                end,
                NewMonitors = maps:put(ClientPid, NewClientSubs, Monitors),

                logger:debug("Added subscription for ~p to ~p", [ClientPid, ResourceUri]),
                {ok, State#subscription_state{
                    subscriptions = NewSubscriptions,
                    monitors = NewMonitors
                }}
        end
    catch
        Class:Reason:Stack ->
            logger:error("Error subscribing ~p to ~p: ~p:~p~n~p", [ClientPid, ResourceUri, Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% @private Unsubscribe a client from a resource
-spec do_unsubscribe(resource_uri(), client_pid(), state()) -> state().
do_unsubscribe(ResourceUri, ClientPid, State) ->
    Subscriptions = State#subscription_state.subscriptions,
    Monitors = State#subscription_state.monitors,

    case maps:get(ResourceUri, Subscriptions, undefined) of
        undefined ->
            State;
        #{clients := Clients} = SubData ->
            NewClients = sets:del_element(ClientPid, Clients),
            case sets:size(NewClients) of
                0 ->
                    %% No more subscribers, remove the resource subscription
                    NewSubscriptions = maps:remove(ResourceUri, Subscriptions),
                    NewMonitors = update_client_subscriptions(ClientPid, ResourceUri, Monitors),
                    logger:debug("Removed subscription for ~p from ~p (last subscriber)", [ClientPid, ResourceUri]),
                    State#subscription_state{
                        subscriptions = NewSubscriptions,
                        monitors = NewMonitors
                    };
                _ ->
                    %% Other subscribers remain
                    NewSubData = SubData#{clients => NewClients},
                    NewSubscriptions = maps:put(ResourceUri, NewSubData, Subscriptions),
                    NewMonitors = update_client_subscriptions(ClientPid, ResourceUri, Monitors),
                    logger:debug("Removed subscription for ~p from ~p", [ClientPid, ResourceUri]),
                    State#subscription_state{
                        subscriptions = NewSubscriptions,
                        monitors = NewMonitors
                    }
            end
    end.

%% @private Cleanup all subscriptions for a specific client when it dies
-spec cleanup_client_subscriptions(pid(), state()) -> state().
cleanup_client_subscriptions(ClientPid, State) ->
    Monitors = State#subscription_state.monitors,
    case maps:get(ClientPid, Monitors, undefined) of
        undefined ->
            State;
        ResourceUris ->
            %% Remove client from all resource subscriptions
            NewState = lists:foldl(fun(Uri, Acc) ->
                do_unsubscribe(Uri, ClientPid, Acc)
            end, State, ResourceUris),
            logger:debug("Cleaned up ~p resource subscriptions for dead client ~p", [length(ResourceUris), ClientPid]),
            NewMonitors = maps:remove(ClientPid, NewState#subscription_state.monitors),
            NewState#subscription_state{monitors = NewMonitors}
    end.

%% @private Update the monitor map when client subscription changes
-spec update_client_subscriptions(pid(), resource_uri(), map()) -> map().
update_client_subscriptions(ClientPid, ResourceUri, Monitors) ->
    case maps:get(ClientPid, Monitors, undefined) of
        undefined ->
            Monitors;
        ResourceUris ->
            NewResourceUris = lists:delete(ResourceUri, ResourceUris),
            case NewResourceUris of
                [] ->
                    maps:remove(ClientPid, Monitors);
                _ ->
                    maps:put(ClientPid, NewResourceUris, Monitors)
            end
    end.
