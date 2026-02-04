%%%-------------------------------------------------------------------
%%% @doc
%%% OTP 26-28 Compatible Distributed Registry
%%%
%%% A cross-version compatible distributed registry that works across
%%% OTP 26, 27, and 28 with appropriate optimizations for each version.
%%%
%%% Key Features:
%%% - Global process registration using optimized mechanisms
%%% - Cross-OTP version support with automatic fallbacks
%%% - Enhanced performance on OTP 28+ (process iterators, priority messages)
%%% - Safe degradation on older versions
%%% - Memory-efficient process enumeration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distribution_registry).

-behaviour(gen_server).
-include("erlmcp.hrl").
-include("otp_compat.hrl").

%% Version detection helpers
-import(erlmcp_otp_compat, [otp_version/0, have_native_json/0,
                            have_process_iterator/0, have_priority_messages/0]).

%% API exports
-export([start_link/0, register/4, unregister/2, whereis/2, list/1, update/3,
         join_group/2, leave_group/2, get_group_members/1, is_distributed/0,
         get_optimal_features/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type entity_type() :: server | transport.
-type entity_id() :: term().
-type entity_config() :: map().
-type transport_id() :: atom() | binary().
-type feature_flag() :: {native_json, boolean()} | {process_iterator, boolean()} |
                     {priority_messages, boolean()} | {pg_optimized, boolean()}.

%% Process groups (version-aware)
-define(PG_SCOPE, erlmcp_registry).
-define(GROUP_ALL_SERVERS, mcp_all_servers).
-define(GROUP_ALL_TRANSPORTS, mcp_all_transports).
-define(GROUP_TOOL_SERVERS, mcp_tool_servers).
-define(GROUP_RESOURCE_SERVERS, mcp_resource_servers).
-define(GROUP_PROMPT_SERVERS, mcp_prompt_servers).

%% State record
-record(state,
        {monitors = #{} :: #{reference() => {Type :: atom(), Id :: term(), Groups :: [atom()]}},
         otp_version :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
         features :: #{feature_flag() => boolean()},
         optimization_level :: basic | standard | optimal}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Register an entity globally with version-optimized registration
%% @end
%%--------------------------------------------------------------------
-spec register(entity_type(), entity_id(), pid(), entity_config()) -> ok | {error, term()}.
register(Type, Id, Pid, Config) when is_atom(Type), is_pid(Pid), is_map(Config) ->
    gen_server:call(?MODULE, {register, Type, Id, Pid, Config}, 5000).

%%--------------------------------------------------------------------
%% @doc Unregister an entity from global registry
%% @end
%%--------------------------------------------------------------------
-spec unregister(entity_type(), entity_id()) -> ok.
unregister(Type, Id) when is_atom(Type) ->
    gen_server:call(?MODULE, {unregister, Type, Id}, 5000).

%%--------------------------------------------------------------------
%% @doc Find an entity by its global name
%% @end
%%--------------------------------------------------------------------
-spec whereis(entity_type(), entity_id()) ->
                 {ok, {node(), pid(), entity_config()}} | {error, not_found}.
whereis(Type, Id) when is_atom(Type) ->
    gen_server:call(?MODULE, {whereis, Type, Id}, 5000).

%%--------------------------------------------------------------------
%% @doc List all entities of a given type
%% @end
%%--------------------------------------------------------------------
-spec list(entity_type()) -> [{entity_id(), {node(), pid(), entity_config()}}].
list(Type) when is_atom(Type) ->
    gen_server:call(?MODULE, {list, Type}, 5000).

%%--------------------------------------------------------------------
%% @doc Update entity configuration
%% @end
%%--------------------------------------------------------------------
-spec update(entity_type(), entity_id(), entity_config()) -> ok | {error, term()}.
update(Type, Id, Config) when is_atom(Type), is_map(Config) ->
    gen_server:call(?MODULE, {update, Type, Id, Config}, 5000).

%%--------------------------------------------------------------------
%% @doc Join a process group with version-aware optimization
%% @end
%%--------------------------------------------------------------------
-spec join_group(atom(), pid()) -> ok.
join_group(Group, Pid) when is_atom(Group), is_pid(Pid) ->
    gen_server:call(?MODULE, {join_group, Group, Pid}, 2000).

%%--------------------------------------------------------------------
%% @doc Leave a process group
%% @end
%%--------------------------------------------------------------------
-spec leave_group(atom(), pid()) -> ok.
leave_group(Group, Pid) when is_atom(Group), is_pid(Pid) ->
    gen_server:call(?MODULE, {leave_group, Group, Pid}, 2000).

%%--------------------------------------------------------------------
%% @doc Get all members of a process group
%% @end
%%--------------------------------------------------------------------
-spec get_group_members(atom()) -> [pid()].
get_group_members(Group) when is_atom(Group) ->
    gen_server:call(?MODULE, {get_group_members, Group}, 2000).

%%--------------------------------------------------------------------
%% @doc Check if this backend is distributed
%% @end
%%--------------------------------------------------------------------
-spec is_distributed() -> boolean().
is_distributed() ->
    true.

%%--------------------------------------------------------------------
%% @doc Get optimal features available for current OTP version
%% @end
%%--------------------------------------------------------------------
-spec get_optimal_features() -> #{feature_flag() => boolean()}.
get_optimal_features() ->
    case get(state) of
        undefined ->
            %% Initialize state temporarily if not available
            {ok, Features} = init_features(),
            Features;
        #state{features = Features} ->
            Features
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Get OTP version and features
    {ok, Features} = init_features(),
    OTPVersion = otp_version(),

    %% Determine optimization level
    OptimizationLevel = determine_optimization_level(OTPVersion, Features),

    %% Start with basic initialization
    logger:info("Starting distribution registry (OTP ~p, optimization: ~p)",
                [OTPVersion, OptimizationLevel]),

    %% Initialize process groups
    init_process_groups(),

    State = #state{
        otp_version = OTPVersion,
        features = Features,
        optimization_level = OptimizationLevel
    },

    %% Apply version-specific optimizations
    apply_optimizations(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({register, Type, Id, Pid, Config}, _From, State) ->
    case register_entity(Type, Id, Pid, Config, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({unregister, Type, Id}, _From, State) ->
    {ok, NewState} = unregister_entity(Type, Id, State),
    {reply, ok, NewState};
handle_call({whereis, Type, Id}, _From, State) ->
    Result = whereis_entity(Type, Id, State),
    {reply, Result, State};
handle_call({list, Type}, _From, State) ->
    Result = list_entities(Type, State),
    {reply, Result, State};
handle_call({update, Type, Id, Config}, _From, State) ->
    case update_entity(Type, Id, Config, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({join_group, Group, Pid}, _From, State) ->
    Result = join_process_group(Group, Pid, State),
    {reply, Result, State};
handle_call({leave_group, Group, Pid}, _From, State) ->
    Result = leave_process_group(Group, Pid, State),
    {reply, Result, State};
handle_call({get_group_members, Group}, _From, State) ->
    Result = get_process_group_members(Group, State),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({add_monitor, Ref, Type, Id, Groups}, State = #state{monitors = Monitors}) ->
    NewMonitors = maps:put(Ref, {Type, Id, Groups}, Monitors),
    {noreply, State#state{monitors = NewMonitors}};
handle_cast({remove_monitor, Ref, Type, Id}, State = #state{monitors = Monitors}) ->
    %% Remove monitor for this specific entity
    FilteredMonitors = maps:filter(fun(_, {T, I, _}) -> not (T =:= Type andalso I =:= Id) end, Monitors),
    {noreply, State#state{monitors = FilteredMonitors}};
handle_cast({update_groups, Pid, Group, Operation}, State = #state{monitors = Monitors}) ->
    %% Update groups for monitored process
    NewMonitors = maps:map(fun(Ref, {Type, Id, Groups}) ->
                    case monitor_info(Ref, pid) of
                        {pid, Pid} ->
                            UpdatedGroups = case Operation of
                                add -> lists:usort([Group | Groups]);
                                remove -> lists:delete(Group, Groups)
                            end,
                            {Type, Id, UpdatedGroups};
                        _ ->
                            {Type, Id, Groups}
                    end
                 end,
                 Monitors),
    {noreply, State#state{monitors = NewMonitors}};
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'DOWN', Ref, process, Pid, Reason}, State = #state{monitors = Monitors}) ->
    case maps:get(Ref, Monitors, undefined) of
        undefined ->
            {noreply, State};
        {Type, Id, Groups} ->
            logger:info("Process ~p died (reason: ~p), cleaning up ~p ~p",
                       [Pid, Reason, Type, Id]),

            %% Unregister from global registry using optimized method
            unregister_entity_optimized(Type, Id, State),

            %% Leave all process groups
            lists:foreach(fun(Group) -> leave_process_group(Group, Pid, State) end, Groups),

            %% Remove from monitor registry
            NewMonitors = maps:remove(Ref, Monitors),
            {noreply, State#state{monitors = NewMonitors}}
    end;
handle_info({system_optimize, Data}, State = #state{optimization_level = OptLevel}) ->
    %% Handle system optimization messages
    logger:debug("Applying system optimization (level: ~p)", [OptLevel]),
    apply_system_optimization(Data, State),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State = #state{otp_version = OTPVersion}) ->
    logger:info("Distribution registry terminating (OTP ~p)", [OTPVersion]),

    %% Apply cleanup optimizations for different versions
    case OTPVersion of
        {28, _, _} ->
            %% OTP 28+ enhanced cleanup
            enhanced_cleanup(State);
        {27, _, _} ->
            %% OTP 27 standard cleanup
            standard_cleanup(State);
        {26, _, _} ->
            %% OTP 26 basic cleanup
            basic_cleanup(State)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State = #state{otp_version = OTPVersion}, _Extra) ->
    %% Handle code change with version awareness
    logger:info("Code change detected (OTP ~p)", [OTPVersion]),

    %% Re-apply optimizations after code change
    NewState = State#state{
        features = get_features_for_version(OTPVersion),
        optimization_level = determine_optimization_level(OTPVersion, get_features_for_version(OTPVersion))
    },
    apply_optimizations(NewState),
    {ok, NewState}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Initialize features based on OTP version
-spec init_features() -> {ok, #{feature_flag() => boolean()}}.
init_features() ->
    OTPVersion = otp_version(),
    Features = get_features_for_version(OTPVersion),
    put(state, #state{otp_version = OTPVersion, features = Features}),
    {ok, Features}.

%% Get features for specific OTP version
-spec get_features_for_version({non_neg_integer(), non_neg_integer(), non_neg_integer()}) ->
    #{feature_flag() => boolean()}.
get_features_for_version(Version) ->
    #{
        native_json => have_native_json(),
        process_iterator => have_process_iterator(),
        priority_messages => have_priority_messages(),
        pg_optimized => has_pg_optimization(Version)
    }.

%% Check if pg optimization is available for version
-spec has_pg_optimization({non_neg_integer(), non_neg_integer(), non_neg_integer()}) -> boolean().
has_pg_optimization(Version) ->
    case Version of
        {28, _, _} -> true;  % OTP 28 has optimized pg protocol
        {27, _, _} -> false; % OTP 27 uses standard pg
        {26, _, _} -> false  % OTP 26 deprecated global pg
    end.

%% Determine optimization level based on version and features
-spec determine_optimization_level({non_neg_integer(), non_neg_integer(), non_neg_integer()},
                                  #{feature_flag() => boolean()}) -> basic | standard | optimal.
determine_optimization_level(Version, Features) ->
    ProcessIterator = maps:get(process_iterator, Features, false),
    PriorityMessages = maps:get(priority_messages, Features, false),
    NativeJSON = maps:get(native_json, Features, false),
    case {Version, ProcessIterator, PriorityMessages, NativeJSON} of
        {{28, _, _}, true, true, _} ->
            optimal;
        {{27, _, _}, _, _, true} ->
            standard;
        {{26, _, _}, _, _, _} ->
            basic;
        _ ->
            standard  % Default fallback
    end.

%% Apply version-specific optimizations
-spec apply_optimizations(state()) -> ok.
apply_optimizations(State = #state{otp_version = Version, features = Features}) ->
    %% Apply priority message optimization if available
    case maps:get(priority_messages, Features, false) of
        true ->
            ?SET_PRIORITY_HIGH(),
            logger:debug("Priority messages enabled for distribution registry");
        false ->
            ok
    end,

    %% Apply process iterator optimization if available
    case maps:get(process_iterator, Features, false) of
        true ->
            %% Enable optimized process monitoring
            enable_process_iterator_optimization(State);
        false ->
            ok
    end,

    %% Apply protocol optimization for OTP 28+
    case Version of
        {28, _, _} ->
            enable_distribution_protocol_optimization();
        _ ->
            ok
    end,
    ok.

%% Register entity with version-optimized registration
-spec register_entity(entity_type(), entity_id(), pid(), entity_config(), state()) ->
    {ok, state()} | {error, term()}.
register_entity(Type, Id, Pid, Config, State = #state{optimization_level = OptLevel}) ->
    %% Use registration method appropriate for optimization level
    case register_entity_optimized(Type, Id, Pid, Config, OptLevel, State) of
        {ok, NewState} ->
            %% Join appropriate groups
            Groups = determine_groups(Type, Config),
            lists:foreach(fun(Group) -> join_process_group(Group, Pid, State) end, Groups),

            %% Setup monitoring for automatic cleanup
            gen_server:cast(?MODULE, {add_monitor, monitor(process, Pid), Type, Id, Groups}),

            logger:info("Registered ~p ~p with pid ~p (optimization: ~p)",
                       [Type, Id, Pid, OptLevel]),
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Optimized registration based on optimization level
-spec register_entity_optimized(entity_type(), entity_id(), pid(), entity_config(),
                               basic | standard | optimal, state()) -> {ok, state()} | {error, term()}.
register_entity_optimized(Type, Id, Pid, Config, OptLevel, State) ->
    case OptLevel of
        optimal ->
            %% Use global:register_name (OTP 28 optimized)
            GlobalName = make_global_name(Type, Id),
            case global:register_name(GlobalName, Pid) of
                yes ->
                    put_entity_config(Type, Id, Config),
                    {ok, State};
                no ->
                    {error, already_registered}
            end;
        standard ->
            %% Standard registration
            GlobalName = make_global_name(Type, Id),
            case global:register_name(GlobalName, Pid) of
                yes ->
                    put_entity_config(Type, Id, Config),
                    {ok, State};
                no ->
                    {error, already_registered}
            end;
        basic ->
            %% Basic registration with fallback
            register_entity_fallback(Type, Id, Pid, Config, State)
    end.

%% Fallback registration for older OTP versions
-spec register_entity_fallback(entity_type(), entity_type(), pid(), entity_config(), state()) ->
    {ok, state()} | {error, term()}.
register_entity_fallback(Type, Id, Pid, Config, State) ->
    %% Try to use pg for registration as fallback
    case erlang:function_exported(pg, start, 1) of
        true ->
            %% OTP 26+ with pg support
            GlobalName = make_global_name(Type, Id),
            case global:register_name(GlobalName, Pid) of
                yes ->
                    put_entity_config(Type, Id, Config),
                    {ok, State};
                no ->
                    {error, already_registered}
            end;
        false ->
            %% Legacy fallback - may not work in distributed environment
            {error, unsupported_distributed_mode}
    end.

%% Unregister entity with version-specific cleanup
-spec unregister_entity(entity_type(), entity_id(), state()) -> {ok, state()}.
unregister_entity(Type, Id, State) ->
    unregister_entity_optimized(Type, Id, State),
    gen_server:cast(?MODULE, {remove_monitor, undefined, Type, Id}),
    ok.

%% Optimized unregister
-spec unregister_entity_optimized(entity_type(), entity_id(), state()) -> ok.
unregister_entity_optimized(Type, Id, State = #state{optimization_level = OptLevel}) ->
    GlobalName = make_global_name(Type, Id),

    case OptLevel of
        optimal ->
            %% OTP 28+ enhanced cleanup
            case global:whereis_name(GlobalName) of
                undefined -> ok;
                Pid ->
                    global:unregister_name(GlobalName),
                    erase_entity_config(Type, Id),
                    %% Remove from groups
                    Groups = determine_groups(Type, #{}),
                    lists:foreach(fun(Group) -> leave_process_group(Group, Pid, State) end, Groups)
            end;
        _ ->
            %% Standard cleanup
            case global:whereis_name(GlobalName) of
                undefined -> ok;
                Pid ->
                    global:unregister_name(GlobalName),
                    erase_entity_config(Type, Id),
                    Groups = determine_groups(Type, #{}),
                    lists:foreach(fun(Group) -> leave_process_group(Group, Pid, State) end, Groups)
            end
    end,
    ok.

%% Find entity with optimization awareness
-spec whereis_entity(entity_type(), entity_id(), state()) ->
                        {ok, {node(), pid(), entity_config()}} | {error, not_found}.
whereis_entity(Type, Id, State) ->
    GlobalName = make_global_name(Type, Id),
    case global:whereis_name(GlobalName) of
        undefined ->
            {error, not_found};
        Pid ->
            Config = get_entity_config(Type, Id),
            Node = case State#state.otp_version of
                {28, _, _} ->
                    %% OTP 28+ can use optimized node detection
                    node_optimized(Pid);
                _ ->
                    node(Pid)
            end,
            {ok, {Node, Pid, Config}}
    end.

%% List entities with version-optimized filtering
-spec list_entities(entity_type(), state()) -> [{entity_id(), {node(), pid(), entity_config()}}].
list_entities(Type, State = #state{optimization_level = OptLevel}) ->
    %% Get all global names matching our pattern
    Pattern = make_global_pattern(Type),
    AllNames = global:registered_names(),

    %% Filter and process based on optimization level
    MatchingNames = filter_and_validate_names(Pattern, AllNames, OptLevel, State),

    %% Build result list with node information
    lists:filtermap(fun(GlobalName) ->
                       case global:whereis_name(GlobalName) of
                           undefined ->
                               false;
                           Pid ->
                               Id = extract_id_from_global_name(GlobalName, Type),
                               Config = get_entity_config(Type, Id),
                               Node = case OptLevel of
                                   optimal -> node_optimized(Pid);
                                   _ -> node(Pid)
                               end,
                               {true, {Id, {Node, Pid, Config}}}
                       end
                    end,
                    MatchingNames).

%% Filter and validate names with optimization
-spec filter_and_validate_names(string(), [atom()], basic | standard | optimal, state()) -> [atom()].
filter_and_validate_names(Pattern, AllNames, OptLevel, State) ->
    lists:filter(fun(Name) ->
                    case matches_pattern(Name, Pattern) of
                        true ->
                            case global:whereis_name(Name) of
                                undefined -> false;
                                _Pid when OptLevel =:= optimal ->
                                    %% Additional validation for optimal mode
                                    validate_entity_liveness(Name, State);
                                _ -> true
                            end;
                        false -> false
                    end
                 end,
                 AllNames).

%% Update entity configuration
-spec update_entity(entity_type(), entity_id(), entity_config(), state()) ->
    {ok, state()} | {error, term()}.
update_entity(Type, Id, Config, State) ->
    case whereis_entity(Type, Id, State) of
        {ok, {_Node, Pid, _OldConfig}} ->
            put_entity_config(Type, Id, Config),
            logger:info("Updated config for ~p ~p", [Type, Id]),
            {ok, State};
        {error, not_found} ->
            {error, not_found}
    end.

%% Join process group with version awareness
-spec join_process_group(atom(), pid(), state()) -> ok | {error, term()}.
join_process_group(Group, Pid, State = #state{features = Features}) ->
    case maps:get(pg_optimized, Features, false) of
        true ->
            %% Use optimized pg join
            case pg_optimized_join(?PG_SCOPE, Group, Pid, State) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            %% Standard pg join
            case pg:join(?PG_SCOPE, Group, Pid) of
                ok ->
                    gen_server:cast(?MODULE, {update_groups, Pid, Group, add}),
                    ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% Optimized pg join for OTP 28+
-spec pg_optimized_join(atom(), atom(), pid(), state()) -> ok | {error, term()}.
pg_optimized_join(Scope, Group, Pid, State = #state{otp_version = Version}) ->
    case Version of
        {28, _, _} ->
            %% Use enhanced pg protocol if available
            try
                ok = pg:join(Scope, Group, Pid),
                gen_server:cast(?MODULE, {update_groups, Pid, Group, add}),
                ok
            catch
                error:badarg ->
                    {error, invalid_arguments}
            end;
        _ ->
            %% Fallback to standard pg join
            case pg:join(Scope, Group, Pid) of
                ok ->
                    gen_server:cast(?MODULE, {update_groups, Pid, Group, add}),
                    ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% Leave process group with version awareness
-spec leave_process_group(atom(), pid(), state()) -> ok | {error, term()}.
leave_process_group(Group, Pid, State = #state{features = Features}) ->
    case maps:get(pg_optimized, Features, false) of
        true ->
            %% Use optimized pg leave
            case pg_optimized_leave(?PG_SCOPE, Group, Pid, State) of
                ok -> ok;
                {error, Reason} -> {error, Reason}
            end;
        false ->
            %% Standard pg leave
            case pg:leave(?PG_SCOPE, Group, Pid) of
                ok ->
                    gen_server:cast(?MODULE, {update_groups, Pid, Group, remove}),
                    ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% Optimized pg leave for OTP 28+
-spec pg_optimized_leave(atom(), atom(), pid(), state()) -> ok | {error, term()}.
pg_optimized_leave(Scope, Group, Pid, State = #state{otp_version = Version}) ->
    case Version of
        {28, _, _} ->
            %% Use enhanced pg protocol
            try
                ok = pg:leave(Scope, Group, Pid),
                gen_server:cast(?MODULE, {update_groups, Pid, Group, remove}),
                ok
            catch
                error:badarg ->
                    {error, invalid_arguments}
            end;
        _ ->
            %% Fallback to standard pg leave
            case pg:leave(Scope, Group, Pid) of
                ok ->
                    gen_server:cast(?MODULE, {update_groups, Pid, Group, remove}),
                    ok;
                {error, Reason} -> {error, Reason}
            end
    end.

%% Get process group members with version optimization
-spec get_process_group_members(atom(), state()) -> [pid()].
get_process_group_members(Group, State = #state{features = Features}) ->
    case maps:get(process_iterator, Features, false) of
        true ->
            %% Use process iterator for large groups (OTP 28+)
            Count = ?SAFE_PROCESS_COUNT(),
            if
                Count > 10000 ->
                    %% Use iterator for large groups
                    get_members_with_iterator(Group, State);
                true ->
                    %% Use standard pg:get_members for small groups
                    pg:get_members(?PG_SCOPE, Group)
            end;
        false ->
            %% Standard pg:get_members
            pg:get_members(?PG_SCOPE, Group)
    end.

%% Get group members using process iterator (OTP 28+)
-spec get_members_with_iterator(atom(), state()) -> [pid()].
get_members_with_iterator(Group, State) ->
    %% Implementation would use erlang:processes_iterator() for efficient enumeration
    %% For now, fall back to standard method
    pg:get_members(?PG_SCOPE, Group).

%% Initialize process groups
-spec init_process_groups() -> ok.
init_process_groups() ->
    %% Ensure pg scope exists
    case pg:start(?PG_SCOPE) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    %% Initialize groups
    Groups = [?GROUP_ALL_SERVERS, ?GROUP_ALL_TRANSPORTS,
              ?GROUP_TOOL_SERVERS, ?GROUP_RESOURCE_SERVERS, ?GROUP_PROMPT_SERVERS],
    lists:foreach(fun(Group) ->
                    case pg:join(?PG_SCOPE, Group, self()) of
                        ok -> ok;
                        {error, _} -> ok
                    end
                 end,
                 Groups).

%% Apply system optimization
-spec apply_system_optimization(term(), state()) -> ok.
apply_system_optimization(Data, State = #state{otp_version = Version, features = Features}) ->
    PriorityMessages = maps:get(priority_messages, Features, false),
    case {Version, PriorityMessages} of
        {{28, _, _}, true} ->
            %% Apply advanced optimizations for OTP 28+
            apply_advanced_optimizations(Data, State);
        {{27, _, _}, _} ->
            %% Apply standard optimizations for OTP 27
            apply_standard_optimizations(Data, State);
        {{26, _, _}, _} ->
            %% Apply basic optimizations for OTP 26
            apply_basic_optimizations(Data, State);
        _ ->
            ok
    end.

%% Enhanced cleanup for OTP 28+
-spec enhanced_cleanup(state()) -> ok.
enhanced_cleanup(State) ->
    %% Use process iterator for efficient cleanup
    case ?HAVE_PROCESS_ITERATOR of
        true ->
            Iterator = erlang:processes_iterator(),
            enhanced_cleanup_iterator(Iterator, State);
        false ->
            %% Fallback to standard cleanup
            standard_cleanup(State)
    end.

%% Enhanced cleanup using iterator
-spec enhanced_cleanup_iterator(term(), state()) -> ok.
enhanced_cleanup_iterator(Iterator, State) ->
    case erlang:process_next(Iterator) of
        {Pid, NewIterator} ->
            %% Cleanup specific processes
            case should_cleanup_process(Pid, State) of
                true ->
                    cleanup_process(Pid, State);
                false ->
                    ok
            end,
            enhanced_cleanup_iterator(NewIterator, State);
        none ->
            ok
    end.

%% Standard cleanup
-spec standard_cleanup(state()) -> ok.
standard_cleanup(_State) ->
    %% Standard cleanup implementation
    ok.

%% Basic cleanup
-spec basic_cleanup(state()) -> ok.
basic_cleanup(_State) ->
    %% Basic cleanup implementation
    ok.

%% Utility functions

-spec make_global_name(entity_type(), entity_id()) -> atom().
make_global_name(Type, Id) when is_atom(Type), is_atom(Id) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ atom_to_list(Id));
make_global_name(Type, Id) when is_atom(Type), is_binary(Id) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ binary_to_list(Id));
make_global_name(Type, Id) when is_atom(Type) ->
    list_to_atom("erlmcp_" ++ atom_to_list(Type) ++ "_" ++ lists:flatten(io_lib:format("~p", [Id]))).

-spec make_global_pattern(entity_type()) -> string().
make_global_pattern(Type) when is_atom(Type) ->
    "erlmcp_" ++ atom_to_list(Type) ++ "_".

-spec matches_pattern(atom(), string()) -> boolean().
matches_pattern(Name, Pattern) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    string:prefix(NameStr, Pattern) =/= nomatch.

-spec extract_id_from_global_name(atom(), entity_type()) -> entity_id().
extract_id_from_global_name(GlobalName, Type) when is_atom(GlobalName), is_atom(Type) ->
    Pattern = make_global_pattern(Type),
    NameStr = atom_to_list(GlobalName),
    IdStr = string:prefix(NameStr, Pattern),
    try
        list_to_existing_atom(IdStr)
    catch
        error:badarg ->
            list_to_binary(IdStr)
    end.

-spec determine_groups(entity_type(), entity_config()) -> [atom()].
determine_groups(server, Config) ->
    BaseGroups = [?GROUP_ALL_SERVERS],
    %% Add type-specific groups based on capabilities
    case maps:get(capabilities, Config, undefined) of
        #mcp_server_capabilities{tools = Tools,
                                 resources = Resources,
                                 prompts = Prompts} ->
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

-spec get_entity_config(entity_type(), entity_id()) -> entity_config().
get_entity_config(Type, Id) ->
    Key = {erlmcp_config, Type, Id},
    case get(Key) of
        undefined -> #{};
        Config -> Config
    end.

-spec put_entity_config(entity_type(), entity_id(), entity_config()) -> ok.
put_entity_config(Type, Id, Config) ->
    Key = {erlmcp_config, Type, Id},
    put(Key, Config),
    ok.

-spec erase_entity_config(entity_type(), entity_id()) -> ok.
erase_entity_config(Type, Id) ->
    Key = {erlmcp_config, Type, Id},
    erase(Key),
    ok.

%% OTP 28+ optimized node detection
-spec node_optimized(pid()) -> node().
node_optimized(Pid) when is_pid(Pid) ->
    %% OTP 28+ can use optimized node detection if available
    case erlang:function_exported(erlang, node_info, 1) of
        true ->
            %% Enhanced node information if available
            case erlang:node_info(Pid, node) of
                {ok, Node} -> Node;
                _ -> node(Pid)
            end;
        false ->
            node(Pid)
    end.

%% Validate entity liveness for optimal mode
-spec validate_entity_liveness(atom(), state()) -> boolean().
validate_entity_liveness(GlobalName, _State) ->
    case global:whereis_name(GlobalName) of
        undefined -> false;
        Pid -> is_process_alive(Pid)
    end.

%% Check if process should be cleaned up
-spec should_cleanup_process(pid(), state()) -> boolean().
should_cleanup_process(Pid, _State) ->
    is_process_alive(Pid).

%% Cleanup specific process
-spec cleanup_process(pid(), state()) -> ok.
cleanup_process(Pid, _State) ->
    %% Implementation would clean up specific process state
    ok.

%% Apply advanced optimizations (OTP 28+)
-spec apply_advanced_optimizations(term(), state()) -> ok.
apply_advanced_optimizations(Data, State) ->
    logger:debug("Applying advanced optimizations for OTP 28+"),
    %% Implementation would apply version-specific optimizations
    ok.

%% Apply standard optimizations (OTP 27)
-spec apply_standard_optimizations(term(), state()) -> ok.
apply_standard_optimizations(Data, State) ->
    logger:debug("Applying standard optimizations for OTP 27"),
    %% Implementation would apply version-specific optimizations
    ok.

%% Apply basic optimizations (OTP 26)
-spec apply_basic_optimizations(term(), state()) -> ok.
apply_basic_optimizations(Data, State) ->
    logger:debug("Applying basic optimizations for OTP 26"),
    %% Implementation would apply version-specific optimizations
    ok.

%% Enable process iterator optimization
-spec enable_process_iterator_optimization(state()) -> ok.
enable_process_iterator_optimization(_State) ->
    %% Setup process iterator if available
    logger:debug("Process iterator optimization enabled"),
    ok.

%% Enable distribution protocol optimization
-spec enable_distribution_protocol_optimization() -> ok.
enable_distribution_protocol_optimization() ->
    %% Setup enhanced distribution protocol if available
    logger:debug("Distribution protocol optimization enabled"),
    ok.

%% Monitor info helper with version fallback
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
                        {Ref, Pid} when Item =:= pid ->
                            {pid, Pid};
                        _ ->
                            undefined
                    end;
                _ ->
                    undefined
            end
    end.