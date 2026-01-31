%%%-------------------------------------------------------------------
%%% @doc
%%% Distributed Registry Backend - Uses global + pg for multi-node deployments
%%%
%%% This backend uses Erlang's built-in distributed primitives:
%%% - global: For unique process registration across cluster
%%% - pg: For process group membership
%%%
%%% Advantages:
%%% - No external dependencies (gproc not required)
%%% - Native clustering support
%%% - Automatic conflict resolution
%%% - Built-in failover
%%%
%%% Trade-offs:
%%% - Slower than local gproc (~2-3x)
%%% - Global locks may impact scalability
%%% - No property/counter support
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_distributed).
-behaviour(gen_server).
-behaviour(erlmcp_registry_behavior).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    register/4,
    unregister/2,
    whereis/2,
    list/1,
    update/3,
    join_group/2,
    leave_group/2,
    get_group_members/1,
    is_distributed/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Process groups
-define(PG_SCOPE, erlmcp_registry).
-define(GROUP_ALL_SERVERS, mcp_all_servers).
-define(GROUP_ALL_TRANSPORTS, mcp_all_transports).
-define(GROUP_TOOL_SERVERS, mcp_tool_servers).
-define(GROUP_RESOURCE_SERVERS, mcp_resource_servers).
-define(GROUP_PROMPT_SERVERS, mcp_prompt_servers).

%% State record
-record(state, {
    monitors = #{} :: #{reference() => {Type :: atom(), Id :: term(), Groups :: [atom()]}}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Register an entity globally with a unique name
%% @end
%%--------------------------------------------------------------------
-spec register(entity_type(), entity_id(), pid(), entity_config()) -> ok | {error, term()}.
register(Type, Id, Pid, Config) when is_atom(Type), is_pid(Pid), is_map(Config) ->
    GlobalName = make_global_name(Type, Id),
    case global:register_name(GlobalName, Pid) of
        yes ->
            %% Join appropriate groups
            Groups = determine_groups(Type, Config),
            lists:foreach(fun(Group) -> ok = join_group(Group, Pid) end, Groups),

            %% Setup monitoring for automatic cleanup
            gen_server:call(?MODULE, {monitor_process, Type, Id, Pid, Groups}),
            logger:info("Registered global ~p ~p with pid ~p", [Type, Id, Pid]),
            ok;
        no ->
            ExistingPid = global:whereis_name(GlobalName),
            logger:warning("Global ~p ~p already registered with pid ~p", [Type, Id, ExistingPid]),
            {error, {already_registered, ExistingPid}}
    end.

%%--------------------------------------------------------------------
%% @doc Unregister an entity from global registry
%% @end
%%--------------------------------------------------------------------
-spec unregister(entity_type(), entity_id()) -> ok.
unregister(Type, Id) when is_atom(Type) ->
    GlobalName = make_global_name(Type, Id),
    case global:whereis_name(GlobalName) of
        undefined ->
            ok;
        Pid ->
            global:unregister_name(GlobalName),
            %% Remove from all groups
            Groups = determine_groups(Type, #{}),
            lists:foreach(fun(Group) -> ok = leave_group(Group, Pid) end, Groups),
            gen_server:cast(?MODULE, {stop_monitoring, Type, Id}),
            logger:info("Unregistered global ~p ~p", [Type, Id]),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Find an entity by its global name
%% @end
%%--------------------------------------------------------------------
-spec whereis(entity_type(), entity_id()) ->
    {ok, {node(), pid(), entity_config()}} | {error, not_found}.
whereis(Type, Id) when is_atom(Type) ->
    GlobalName = make_global_name(Type, Id),
    case global:whereis_name(GlobalName) of
        undefined ->
            {error, not_found};
        Pid ->
            %% Get config from process dictionary (stored during registration)
            Config = get_entity_config(Type, Id),
            {ok, {node(Pid), Pid, Config}}
    end.

%%--------------------------------------------------------------------
%% @doc List all entities of a given type
%% @end
%%--------------------------------------------------------------------
-spec list(entity_type()) -> [{entity_id(), {node(), pid(), entity_config()}}].
list(Type) when is_atom(Type) ->
    %% Get all global names matching our pattern
    Pattern = make_global_pattern(Type),
    AllNames = global:registered_names(),
    MatchingNames = lists:filter(fun(Name) -> matches_pattern(Name, Pattern) end, AllNames),

    %% Build result list
    lists:filtermap(fun(GlobalName) ->
        case global:whereis_name(GlobalName) of
            undefined -> false;
            Pid ->
                Id = extract_id_from_global_name(GlobalName, Type),
                Config = get_entity_config(Type, Id),
                {true, {Id, {node(Pid), Pid, Config}}}
        end
    end, MatchingNames).

%%--------------------------------------------------------------------
%% @doc Update entity configuration
%% @end
%%--------------------------------------------------------------------
-spec update(entity_type(), entity_id(), entity_config()) -> ok | {error, term()}.
update(Type, Id, Config) when is_atom(Type), is_map(Config) ->
    case whereis(Type, Id) of
        {error, not_found} ->
            {error, not_found};
        {ok, {_Node, Pid, _OldConfig}} ->
            %% Store updated config
            put_entity_config(Type, Id, Config),
            logger:info("Updated config for ~p ~p", [Type, Id]),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Join a process group
%% @end
%%--------------------------------------------------------------------
-spec join_group(atom(), pid()) -> ok.
join_group(Group, Pid) when is_atom(Group), is_pid(Pid) ->
    ok = pg:join(?PG_SCOPE, Group, Pid),
    gen_server:cast(?MODULE, {add_group, Pid, Group}),
    ok.

%%--------------------------------------------------------------------
%% @doc Leave a process group
%% @end
%%--------------------------------------------------------------------
-spec leave_group(atom(), pid()) -> ok.
leave_group(Group, Pid) when is_atom(Group), is_pid(Pid) ->
    ok = pg:leave(?PG_SCOPE, Group, Pid),
    gen_server:cast(?MODULE, {remove_group, Pid, Group}),
    ok.

%%--------------------------------------------------------------------
%% @doc Get all members of a process group
%% @end
%%--------------------------------------------------------------------
-spec get_group_members(atom()) -> [pid()].
get_group_members(Group) when is_atom(Group) ->
    pg:get_members(?PG_SCOPE, Group).

%%--------------------------------------------------------------------
%% @doc Check if this backend is distributed
%% @end
%%--------------------------------------------------------------------
-spec is_distributed() -> boolean().
is_distributed() ->
    true.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),
    ensure_pg_scope(),
    logger:info("Starting distributed registry backend (global + pg)"),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.

handle_call({monitor_process, Type, Id, Pid, Groups}, _From, State) ->
    Ref = monitor(process, Pid),
    NewMonitors = maps:put(Ref, {Type, Id, Groups}, State#state.monitors),
    {reply, ok, State#state{monitors = NewMonitors}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({stop_monitoring, Type, Id}, State) ->
    %% Find and remove monitor for this entity
    NewMonitors = maps:filter(fun(_, {T, I, _}) -> not (T =:= Type andalso I =:= Id) end,
                              State#state.monitors),
    {noreply, State#state{monitors = NewMonitors}};

handle_cast({add_group, Pid, Group}, State) ->
    %% Update groups for monitored process
    NewMonitors = maps:map(fun(Ref, {Type, Id, Groups}) ->
        case monitor_info(Ref, pid) of
            {pid, Pid} -> {Type, Id, lists:usort([Group | Groups])};
            _ -> {Type, Id, Groups}
        end
    end, State#state.monitors),
    {noreply, State#state{monitors = NewMonitors}};

handle_cast({remove_group, Pid, Group}, State) ->
    %% Remove group from monitored process
    NewMonitors = maps:map(fun(Ref, {Type, Id, Groups}) ->
        case monitor_info(Ref, pid) of
            {pid, Pid} -> {Type, Id, lists:delete(Group, Groups)};
            _ -> {Type, Id, Groups}
        end
    end, State#state.monitors),
    {noreply, State#state{monitors = NewMonitors}};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    %% Process died - clean up global registration and pg groups
    case maps:get(Ref, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        {Type, Id, Groups} ->
            logger:warning("Process ~p died (reason: ~p), cleaning up ~p ~p",
                          [Pid, Reason, Type, Id]),

            %% Unregister from global
            GlobalName = make_global_name(Type, Id),
            global:unregister_name(GlobalName),

            %% Leave all groups
            lists:foreach(fun(Group) -> pg:leave(?PG_SCOPE, Group, Pid) end, Groups),

            %% Remove config
            erase_entity_config(Type, Id),

            %% Remove monitor
            NewMonitors = maps:remove(Ref, State#state.monitors),
            {noreply, State#state{monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Distributed registry backend terminating"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Ensure pg scope exists
-spec ensure_pg_scope() -> ok.
ensure_pg_scope() ->
    case whereis(pg) of
        undefined ->
            {ok, _} = pg:start_link(?PG_SCOPE),
            ok;
        _ ->
            ok
    end.

%% @doc Make a global name for an entity
-spec make_global_name(entity_type(), entity_id()) -> atom().
make_global_name(Type, Id) when is_atom(Type), is_atom(Id) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ atom_to_list(Id));
make_global_name(Type, Id) when is_atom(Type), is_binary(Id) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ binary_to_list(Id));
make_global_name(Type, Id) when is_atom(Type) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ lists:flatten(io_lib:format("~p", [Id]))).

%% @doc Make a pattern for matching global names
-spec make_global_pattern(entity_type()) -> string().
make_global_pattern(Type) when is_atom(Type) ->
    "erlmcp_" ++ atom_to_list(Type) ++ "_".

%% @doc Check if a global name matches a pattern
-spec matches_pattern(atom(), string()) -> boolean().
matches_pattern(Name, Pattern) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    string:prefix(NameStr, Pattern) =/= nomatch.

%% @doc Extract entity ID from global name
-spec extract_id_from_global_name(atom(), entity_type()) -> entity_id().
extract_id_from_global_name(GlobalName, Type) when is_atom(GlobalName), is_atom(Type) ->
    Pattern = make_global_pattern(Type),
    NameStr = atom_to_list(GlobalName),
    IdStr = string:prefix(NameStr, Pattern),
    %% Try to convert back to atom or binary
    try
        list_to_existing_atom(IdStr)
    catch
        error:badarg -> list_to_binary(IdStr)
    end.

%% @doc Determine which groups an entity should join
-spec determine_groups(entity_type(), entity_config()) -> [atom()].
determine_groups(server, Config) ->
    BaseGroups = [?GROUP_ALL_SERVERS],
    %% Add type-specific groups based on capabilities
    case maps:get(capabilities, Config, undefined) of
        #mcp_server_capabilities{tools = Tools, resources = Resources, prompts = Prompts} ->
            TypeGroups =
                case {Tools, Resources, Prompts} of
                    {#{}, _, _} -> [?GROUP_TOOL_SERVERS];
                    {_, #{}, _} -> [?GROUP_RESOURCE_SERVERS];
                    {_, _, #{}} -> [?GROUP_PROMPT_SERVERS];
                    _ -> []
                end,
            BaseGroups ++ TypeGroups;
        _ ->
            BaseGroups
    end;
determine_groups(transport, _Config) ->
    [?GROUP_ALL_TRANSPORTS].

%% @doc Get entity config from process dictionary
-spec get_entity_config(entity_type(), entity_id()) -> entity_config().
get_entity_config(Type, Id) ->
    Key = {erlmcp_config, Type, Id},
    case get(Key) of
        undefined -> #{};
        Config -> Config
    end.

%% @doc Put entity config into process dictionary
-spec put_entity_config(entity_type(), entity_id(), entity_config()) -> ok.
put_entity_config(Type, Id, Config) ->
    Key = {erlmcp_config, Type, Id},
    put(Key, Config),
    ok.

%% @doc Erase entity config from process dictionary
-spec erase_entity_config(entity_type(), entity_id()) -> ok.
erase_entity_config(Type, Id) ->
    Key = {erlmcp_config, Type, Id},
    erase(Key),
    ok.

%% @doc Get monitor info (OTP 24+ has erlang:monitor_info/2)
-spec monitor_info(reference(), atom()) -> {atom(), term()} | undefined.
monitor_info(Ref, Item) ->
    try
        erlang:monitor_info(Ref, Item)
    catch
        error:undef ->
            %% Fallback for older OTP versions
            case erlang:process_info(self(), monitors) of
                {monitors, Monitors} ->
                    case lists:keyfind(Ref, 1, Monitors) of
                        {Ref, Pid} when Item =:= pid -> {pid, Pid};
                        _ -> undefined
                    end;
                _ ->
                    undefined
            end
    end.
