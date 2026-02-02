-module(erlmcp_tool_registry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register_tool/3,
         unregister_tool/1,
         get_tool/1,
         list_tools/0,
         get_tool_version/1,
         update_tool_version/2,
         check_tool_updated/1,
         subscribe_to_tool_updates/1,
         unsubscribe_from_tool_updates/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Tool Registry with Version Tracking (OTP 27-28)
%% Tracks MCP tools, their versions, and state

-type tool_name() :: binary().
-type tool_version() :: binary().

-record(tool,
        {name :: tool_name(),
         module :: module(),
         version :: tool_version(),
         load_time :: erlang:timestamp(),
         metadata :: map(),
         state :: term()}).

-record(state,
        {tool_table :: ets:tid(),           % Tool registry
         version_table :: ets:tid(),        % Version tracking
         subscribers = #{} :: map()}).      % Update subscribers

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register tool with version tracking
-spec register_tool(tool_name(), module(), map()) -> ok | {error, term()}.
register_tool(Name, Module, Metadata) ->
    gen_server:call(?MODULE, {register_tool, Name, Module, Metadata}, 5000).

%% @doc Unregister tool
-spec unregister_tool(tool_name()) -> ok | {error, not_found}.
unregister_tool(Name) ->
    gen_server:call(?MODULE, {unregister_tool, Name}, 5000).

%% @doc Get tool info
-spec get_tool(tool_name()) -> {ok, #tool{}} | {error, not_found}.
get_tool(Name) ->
    gen_server:call(?MODULE, {get_tool, Name}).

%% @doc List all registered tools
-spec list_tools() -> [#tool{}].
list_tools() ->
    gen_server:call(?MODULE, list_tools).

%% @doc Get tool version
-spec get_tool_version(tool_name()) -> {ok, tool_version()} | {error, not_found}.
get_tool_version(Name) ->
    gen_server:call(?MODULE, {get_tool_version, Name}).

%% @doc Update tool version (called after hot reload)
-spec update_tool_version(tool_name(), tool_version()) -> ok | {error, not_found}.
update_tool_version(Name, Version) ->
    gen_server:call(?MODULE, {update_tool_version, Name, Version}).

%% @doc Check if tool has been updated since last check
-spec check_tool_updated(tool_name()) -> boolean().
check_tool_updated(Name) ->
    gen_server:call(?MODULE, {check_tool_updated, Name}).

%% @doc Subscribe to tool update notifications
-spec subscribe_to_tool_updates(tool_name()) -> ok | {error, term()}.
subscribe_to_tool_updates(Name) ->
    gen_server:call(?MODULE, {subscribe_to_updates, Name}).

%% @doc Unsubscribe from tool update notifications
-spec unsubscribe_from_tool_updates(tool_name()) -> ok.
unsubscribe_from_tool_updates(Name) ->
    gen_server:cast(?MODULE, {unsubscribe_from_updates, Name}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Tool table: name -> tool record
    ToolTable = ets:new(erlmcp_tool_table,
                        [named_table,
                         public,
                         set,
                         {read_concurrency, true}]),

    %% Version table: name -> {version, last_checked}
    VersionTable = ets:new(erlmcp_tool_versions,
                           [named_table,
                            public,
                            set]),

    logger:info("Tool registry started with OTP ~p", [erlang:system_info(otp_release)]),

    {ok, #state{tool_table = ToolTable,
                version_table = VersionTable}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                          {reply, term(), state()}.
handle_call({register_tool, Name, Module, Metadata}, _From, State) ->
    Result = do_register_tool(Name, Module, Metadata, State),
    {reply, Result, State};

handle_call({unregister_tool, Name}, _From, State) ->
    Result = do_unregister_tool(Name, State),
    {reply, Result, State};

handle_call({get_tool, Name}, _From, State) ->
    Result = do_get_tool(Name, State),
    {reply, Result, State};

handle_call(list_tools, _From, State) ->
    Result = do_list_tools(State),
    {reply, Result, State};

handle_call({get_tool_version, Name}, _From, State) ->
    Result = do_get_tool_version(Name, State),
    {reply, Result, State};

handle_call({update_tool_version, Name, Version}, _From, State) ->
    Result = do_update_tool_version(Name, Version, State),
    {reply, Result, State};

handle_call({check_tool_updated, Name}, _From, State) ->
    Result = do_check_tool_updated(Name, State),
    {reply, Result, State};

handle_call({subscribe_to_updates, Name}, {Pid, _Ref}, State) ->
    Result = do_subscribe_to_updates(Name, Pid, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({unsubscribe_from_updates, Name}, State) ->
    NewState = do_unsubscribe_from_updates(Name, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', _MonitorRef, process, Pid, _Reason}, State) ->
    %% Subscriber died, remove from subscriptions
    NewState = remove_subscriber(Pid, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{tool_table = ToolTable, version_table = VersionTable}) ->
    ets:delete(ToolTable),
    ets:delete(VersionTable),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Register tool
-spec do_register_tool(tool_name(), module(), map(), state()) -> ok | {error, term()}.
do_register_tool(Name, Module, Metadata, _State) ->
    logger:info("Registering tool: ~p (module: ~p)", [Name, Module]),

    %% Get module version (OTP 28)
    Version = case erlmcp_code_loader:get_module_md5(Module) of
                  {ok, MD5} -> MD5;
                  {error, _} -> <<>>
              end,

    Tool = #tool{name = Name,
                 module = Module,
                 version = Version,
                 load_time = erlang:timestamp(),
                 metadata = Metadata,
                 state = undefined},

    ets:insert(erlmcp_tool_table, {Name, Tool}),
    ets:insert(erlmcp_tool_versions, {Name, {Version, erlang:timestamp()}}),

    logger:info("Tool registered: ~p (version: ~p)", [Name, Version]),
    ok.

%% @doc Unregister tool
-spec do_unregister_tool(tool_name(), state()) -> ok | {error, not_found}.
do_unregister_tool(Name, _State) ->
    case ets:lookup(erlmcp_tool_table, Name) of
        [{Name, _Tool}] ->
            ets:delete(erlmcp_tool_table, Name),
            ets:delete(erlmcp_tool_versions, Name),
            logger:info("Tool unregistered: ~p", [Name]),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc Get tool info
-spec do_get_tool(tool_name(), state()) -> {ok, #tool{}} | {error, not_found}.
do_get_tool(Name, _State) ->
    case ets:lookup(erlmcp_tool_table, Name) of
        [{Name, Tool}] ->
            {ok, Tool};
        [] ->
            {error, not_found}
    end.

%% @doc List all tools
-spec do_list_tools(state()) -> [#tool{}].
do_list_tools(_State) ->
    ets:foldl(fun({_Name, Tool}, Acc) -> [Tool | Acc] end, [], erlmcp_tool_table).

%% @doc Get tool version
-spec do_get_tool_version(tool_name(), state()) -> {ok, tool_version()} | {error, not_found}.
do_get_tool_version(Name, _State) ->
    case ets:lookup(erlmcp_tool_versions, Name) of
        [{Name, {Version, _Timestamp}}] ->
            {ok, Version};
        [] ->
            {error, not_found}
    end.

%% @doc Update tool version
-spec do_update_tool_version(tool_name(), tool_version(), state()) -> ok | {error, not_found}.
do_update_tool_version(Name, NewVersion, State) ->
    case ets:lookup(erlmcp_tool_table, Name) of
        [{Name, #tool{} = Tool}] ->
            %% Update tool record
            UpdatedTool = Tool#tool{version = NewVersion,
                                    load_time = erlang:timestamp()},
            ets:insert(erlmcp_tool_table, {Name, UpdatedTool}),

            %% Update version table
            ets:insert(erlmcp_tool_versions, {Name, {NewVersion, erlang:timestamp()}}),

            logger:info("Tool version updated: ~p -> ~p", [Name, NewVersion]),

            %% Notify subscribers
            notify_tool_updated(Name, NewVersion, State),
            ok;
        [] ->
            {error, not_found}
    end.

%% @doc Check if tool has been updated
-spec do_check_tool_updated(tool_name(), state()) -> boolean().
do_check_tool_updated(Name, _State) ->
    case ets:lookup(erlmcp_tool_versions, Name) of
        [{Name, {Version, LastChecked}}] ->
            %% Get current module version
            case ets:lookup(erlmcp_tool_table, Name) of
                [{Name, #tool{version = CurrentVersion}}] ->
                    %% Update last checked time
                    ets:insert(erlmcp_tool_versions, {Name, {Version, erlang:timestamp()}}),

                    %% Compare versions
                    CurrentVersion =/= Version;
                [] ->
                    false
            end;
        [] ->
            false
    end.

%% @doc Subscribe to tool updates
-spec do_subscribe_to_updates(tool_name(), pid(), state()) -> ok | {error, term()}.
do_subscribe_to_updates(Name, SubscriberPid, State) ->
    case ets:lookup(erlmcp_tool_table, Name) of
        [{Name, _Tool}] ->
            %% Monitor subscriber
            MonitorRef = monitor(process, SubscriberPid),

            %% Add to subscribers map
            Subscribers = maps:get(Name, State#state.subscribers, #{}),
            NewSubscribers = maps:put(SubscriberPid, MonitorRef, Subscribers),

            NewState = State#state{subscribers = maps:put(Name, NewSubscribers, State#state.subscribers)},

            logger:info("Subscriber ~p added to tool ~p", [SubscriberPid, Name]),
            {ok, NewState};
        [] ->
            {error, not_found}
    end.

%% @doc Unsubscribe from tool updates
-spec do_unsubscribe_from_updates(tool_name(), state()) -> state().
do_unsubscribe_from_updates(Name, State) ->
    case maps:get(Name, State#state.subscribers, undefined) of
        undefined ->
            State;
        Subscribers ->
            %% Demonitor all subscribers
            maps:foreach(fun(_Pid, MonRef) -> demonitor(MonRef, [flush]) end, Subscribers),

            %% Remove from state
            NewSubscribers = maps:remove(Name, State#state.subscribers),
            State#state{subscribers = NewSubscribers}
    end.

%% @doc Remove subscriber (when process dies)
-spec remove_subscriber(pid(), state()) -> state().
remove_subscriber(SubscriberPid, #state{subscribers = Subscribers} = State) ->
    %% Find and remove subscriber from all tools
    NewSubscribers = maps:map(fun(_Name, SubsMap) ->
                                      maps:remove(SubscriberPid, SubsMap)
                              end, Subscribers),
    State#state{subscribers = NewSubscribers}.

%% @doc Notify subscribers of tool update
-spec notify_tool_updated(tool_name(), tool_version(), state()) -> ok.
notify_tool_updated(Name, Version, #state{subscribers = Subscribers}) ->
    case maps:get(Name, Subscribers, undefined) of
        undefined ->
            ok;
        SubsMap ->
            maps:foreach(fun(SubscriberPid, _MonRef) ->
                                try
                                    SubscriberPid ! {tool_updated, Name, Version}
                                catch
                                    _:_ ->
                                        ok
                                end
                        end, SubsMap),
            ok
    end.
