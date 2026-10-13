%%%-------------------------------------------------------------------
%%% @doc
%%% Client Registry for erlmcp
%%% Implements a simple named client registry for MCP API Phase 1 compatibility.
%%% Provides client connection management with process monitoring and cleanup.
%%% Uses gproc for distributed registration with local supervisor integration.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_client_registry).

-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1]).
-export([register/2, unregister/1, lookup/1, list/0]).
-export([exists/1, count/0, get_pids/0, cleanup_dead/0]).
-export([register_distributed/3, lookup_distributed/1, list_distributed/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Include files
-include("erlmcp.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%% Client record
-record(client,
        {name :: binary(),
         pid :: pid(),
         registered_at :: integer(),
         metadata :: map()}).

%% Registry state
-record(state,
        {clients = #{} :: #{binary() => #client{}},
         name_index = #{} :: #{pid() => binary()},
         monitor_refs = #{} :: #{reference() => binary()}}).

%% Type exports
-type client_name() :: binary().
-type client_pid() :: pid().
-type client_metadata() :: map().
-type client_entry() :: #client{}.
-type registry_key() :: client_name().
-type registry_value() :: {client_pid(), client_metadata(), integer()}.

-export_type([client_name/0, client_pid/0, client_metadata/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the client registry with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the client registry with custom options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Register a client with the registry.
%% @param Name The unique name for the client (binary)
%% @param Pid The process ID of the client
%% @return {ok, reference()} if successful, {error, already_exists | invalid_name} if failed
-spec register(client_name(), client_pid()) -> {ok, reference()} | {error, term()}.
register(Name, Pid) when is_binary(Name), is_pid(Pid) ->
    gen_server:call(?MODULE, {register, Name, Pid, #{}}).

%% @doc Register a client with metadata.
%% @param Name The unique name for the client (binary)
%% @param Pid The process ID of the client
%% @param Metadata Additional metadata for the client
%% @return {ok, reference()} if successful, {error, already_exists | invalid_name} if failed
-spec register(client_name(), client_pid(), client_metadata()) -> {ok, reference()} | {error, term()}.
register(Name, Pid, Metadata) when is_binary(Name), is_pid(Pid), is_map(Metadata) ->
    gen_server:call(?MODULE, {register, Name, Pid, Metadata}).

%% @doc Unregister a client from the registry.
%% @param Name The name of the client to unregister
%% @return ok if successful, {error, not_found} if client doesn't exist
-spec unregister(client_name()) -> ok | {error, not_found}.
unregister(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%% @doc Lookup a client by name.
%% @param Name The name of the client to lookup
%% @return {ok, pid(), metadata(), timestamp()} if found, {error, not_found} if not found
-spec lookup(client_name()) -> {ok, client_pid(), client_metadata(), integer()} | {error, not_found}.
lookup(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {lookup, Name}).

%% @doc List all registered client names.
%% @return {ok, [client_name()]} with all registered client names
-spec list() -> {ok, [client_name()]}.
list() ->
    gen_server:call(?MODULE, list).

%% @doc Check if a client name exists in the registry.
%% @param Name The name to check
%% @return true if exists, false otherwise
-spec exists(client_name()) -> boolean().
exists(Name) when is_binary(Name) ->
    case lookup(Name) of
        {ok, _, _, _} -> true;
        {error, not_found} -> false
    end.

%% @doc Get the total count of registered clients.
%% @return The number of registered clients
-spec count() -> non_neg_integer().
count() ->
    gen_server:call(?MODULE, count).

%% @doc Get all registered client PIDs.
%% @return [client_pid()] with all registered client PIDs
-spec get_pids() -> [client_pid()].
get_pids() ->
    case list() of
        {ok, Names} ->
            lists:foldl(fun(Name, Acc) ->
                case lookup(Name) of
                    {ok, Pid, _, _} -> [Pid | Acc];
                    {error, not_found} -> Acc
                end
            end, [], Names);
        {error, no_clients} -> []
    end.

%% @doc Cleanup dead clients from the registry.
%% @return ok after cleanup
-spec cleanup_dead() -> ok.
cleanup_dead() ->
    gen_server:call(?MODULE, cleanup_dead).

%% @doc Register a client across the cluster.
%% @param Name The unique name for the client (binary)
%% @param Pid The process ID of the client
%% @param Metadata Additional metadata for the client
%% @return {ok, reference()} if successful, {error, already_exists | invalid_name} if failed
-spec register_distributed(client_name(), client_pid(), client_metadata()) -> {ok, reference()} | {error, term()}.
register_distributed(Name, Pid, Metadata) when is_binary(Name), is_pid(Pid), is_map(Metadata) ->
    %% Register locally first
    case register(Name, Pid, Metadata) of
        {ok, Ref} ->
            %% Register on all cluster nodes
            case erlmcp_cluster:cluster_nodes() of
                [] -> {ok, Ref};
                Nodes -> register_on_nodes(Nodes, Name, Pid, Metadata)
            end;
        Error -> Error
    end.

%% @doc Lookup a client across the entire cluster.
%% @param Name The name of the client to lookup
%% @return {ok, pid(), metadata(), timestamp(), node()} if found, {error, not_found} if not found
-spec lookup_distributed(client_name()) -> {ok, client_pid(), client_metadata(), integer(), node()} | {error, not_found}.
lookup_distributed(Name) when is_binary(Name) ->
    %% Try local lookup first
    case lookup(Name) of
        {ok, Pid, Metadata, Timestamp} -> {ok, Pid, Metadata, Timestamp, node()};
        {error, not_found} -> lookup_on_nodes(Name)
    end.

%% @doc List all registered client names across the cluster.
%% @return {ok, [{client_name(), node()}]} with all registered client names and their nodes
-spec list_distributed() -> {ok, [{client_name(), node()}]}.
list_distributed() ->
    LocalClients = case list() of
        {ok, Names} -> [{Name, node()} || Name <- Names];
        {error, no_clients} -> []
    end,

    %% Get clients from other nodes
    OtherClients = list_clients_on_nodes(),

    {ok, LocalClients ++ OtherClients}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(_Options) ->
    %% Set up periodic cleanup timer (every 5 minutes)
    erlang:send_after(300000, self(), cleanup_timer),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {reply, term(), #state{}, hibernate}.
handle_call({register, Name, Pid, Metadata}, {FromPid, _Ref}, State) ->
    case validate_name(Name) of
        false ->
            {reply, {error, invalid_name}, State};
        true ->
            case maps:is_key(Name, State#state.clients) of
                true ->
                    {reply, {error, already_exists}, State};
                false ->
                    %% Create client entry
                    Client = #client{
                        name = Name,
                        pid = Pid,
                        registered_at = erlang:system_time(millisecond),
                        metadata = Metadata
                    },

                    %% Monitor the client process
                    MonRef = erlang:monitor(process, Pid),

                    %% Update state
                    NewClients = maps:put(Name, Client, State#state.clients),
                    NewNameIndex = maps:put(Pid, Name, State#state.name_index),
                    NewMonitorRefs = maps:put(MonRef, Name, State#state.monitor_refs),

                    %% Log registration
                    logger:info("Client registered: ~p (~p)", [Name, Pid]),

                    {reply, {ok, MonRef}, State#state{
                        clients = NewClients,
                        name_index = NewNameIndex,
                        monitor_refs = NewMonitorRefs
                    }, hibernate}
            end
    end;

handle_call({unregister, Name}, {FromPid, _Ref}, State) ->
    case maps:find(Name, State#state.clients) of
        {ok, Client} ->
            Pid = Client#client.pid,
            MonRef = maps:get(Pid, State#state.name_index),

            %% Remove monitor
            erlang:demonitor(MonRef, [flush]),

            %% Update state
            NewClients = maps:remove(Name, State#state.clients),
            NewNameIndex = maps:remove(Pid, State#state.name_index),
            NewMonitorRefs = maps:remove(MonRef, State#state.monitor_refs),

            %% Log unregistration
            logger:info("Client unregistered: ~p (~p)", [Name, Pid]),

            {reply, ok, State#state{
                clients = NewClients,
                name_index = NewNameIndex,
                monitor_refs = NewMonitorRefs
            }, hibernate};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({lookup, Name}, {FromPid, _Ref}, State) ->
    case maps:find(Name, State#state.clients) of
        {ok, Client} ->
            {reply, {ok, Client#client.pid, Client#client.metadata, Client#client.registered_at}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(list, {FromPid, _Ref}, State) ->
    ClientNames = maps:keys(State#state.clients),
    {reply, {ok, ClientNames}, State};

handle_call(count, {FromPid, _Ref}, State) ->
    Count = maps:size(State#state.clients),
    {reply, Count, State};

handle_call(cleanup_dead, {FromPid, _Ref}, State) ->
    %% Find dead processes and remove them
    DeadClients = find_dead_clients(State),
    NewState = cleanup_dead_clients(State, DeadClients),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {noreply, #state{}, hibernate}.
handle_info({'DOWN', MonRef, process, _Pid, _Reason}, State) ->
    %% Process went down - clean it up
    case maps:find(MonRef, State#state.monitor_refs) of
        {ok, Name} ->
            logger:info("Client process died: ~p", [Name]),
            NewState = remove_client(State, Name),
            {noreply, NewState};
        error ->
            %% Unknown monitor reference - ignore
            {noreply, State}
    end;

handle_info(cleanup_timer, State) ->
    %% Periodic cleanup timer
    DeadClients = find_dead_clients(State),
    NewState = cleanup_dead_clients(State, DeadClients),
    erlang:send_after(300000, self(), cleanup_timer),  %% Schedule next cleanup
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> term().
terminate(_Reason, State) ->
    %% Log termination
    logger:info("Client registry terminating with ~p clients", [maps:size(State#state.clients)]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate client name
-spec validate_name(binary()) -> boolean().
validate_name(Name) when is_binary(Name) ->
    %% Name must not be empty and must be reasonable length
    case byte_size(Name) of
        0 -> false;
        N when N > 255 -> false;
        _ -> true
    end.

%% @doc Find dead clients in the registry
-spec find_dead_clients(#state{}) -> [client_name()].
find_dead_clients(State) ->
    maps:fold(fun(Name, Client, Acc) ->
        case is_process_alive(Client#client.pid) of
            true -> Acc;
            false -> [Name | Acc]
        end
    end, [], State#state.clients).

%% @doc Cleanup dead clients from state
-spec cleanup_dead_clients(#state{}, [client_name()]) -> #state{}.
cleanup_dead_clients(State, []) ->
    State;
cleanup_dead_clients(State, [Name | Rest]) ->
    NewState = remove_client(State, Name),
    cleanup_dead_clients(NewState, Rest).

%% @doc Remove a client from the state
-spec remove_client(#state{}, client_name()) -> #state{}.
remove_client(State, Name) ->
    case maps:find(Name, State#state.clients) of
        {ok, Client} ->
            Pid = Client#client.pid,
            MonRef = maps:get(Pid, State#state.name_index, undefined),

            %% Remove monitor
            if MonRef =/= undefined ->
                   erlang:demonitor(MonRef, [flush]);
               true -> ok
            end,

            %% Update state
            NewClients = maps:remove(Name, State#state.clients),
            NewNameIndex = maps:remove(Pid, State#state.name_index),
            NewMonitorRefs = case MonRef of
                              undefined -> State#state.monitor_refs;
                              _ -> maps:remove(MonRef, State#state.monitor_refs)
                           end,

            State#state{
                clients = NewClients,
                name_index = NewNameIndex,
                monitor_refs = NewMonitorRefs
            };
        error ->
            State
    end.

%%====================================================================
%% Distributed Functions
%%====================================================================

%% @doc Register client on multiple nodes
-spec register_on_nodes([node()], client_name(), client_pid(), client_metadata()) -> ok | {error, term()}.
register_on_nodes([], _Name, _Pid, _Metadata) ->
    ok;
register_on_nodes([Node | Rest], Name, Pid, Metadata) ->
    case Node =:= node() of
        true ->
            %% Skip local node - already registered
            register_on_nodes(Rest, Name, Pid, Metadata);
        false ->
            %% Register on remote node
            case erlmcp_cluster:distribute_call(Node, ?MODULE, register_distributed_local, [Name, Pid, Metadata]) of
                {ok, _} ->
                    register_on_nodes(Rest, Name, Pid, Metadata);
                {error, _Reason} ->
                    %% Continue with other nodes even if one fails
                    register_on_nodes(Rest, Name, Pid, Metadata)
            end
    end.

%% @doc Local helper for distributed registration (called via RPC)
-spec register_distributed_local(client_name(), client_pid(), client_metadata()) -> {ok, reference()} | {error, term()}.
register_distributed_local(Name, Pid, Metadata) ->
    register(Name, Pid, Metadata).

%% @doc Lookup client on multiple nodes
-spec lookup_on_nodes(client_name()) -> {ok, client_pid(), client_metadata(), integer(), node()} | {error, not_found}.
lookup_on_nodes(Name) ->
    Nodes = erlmcp_cluster:cluster_nodes(),
    lookup_on_nodes(Nodes, Name).

%% @doc Helper function for distributed lookup
-spec lookup_on_nodes([node()], client_name()) -> {ok, client_pid(), client_metadata(), integer(), node()} | {error, not_found}.
lookup_on_nodes([], _Name) ->
    {error, not_found};
lookup_on_nodes([Node | Rest], Name) ->
    case Node =:= node() of
        true ->
            %% Skip local node - already tried
            lookup_on_nodes(Rest, Name);
        false ->
            %% Try remote node
            case erlmcp_cluster:distribute_call(Node, ?MODULE, lookup_distributed_local, [Name]) of
                {ok, {Pid, Metadata, Timestamp}} ->
                    {ok, Pid, Metadata, Timestamp, Node};
                {error, not_found} ->
                    lookup_on_nodes(Rest, Name)
            end
    end.

%% @doc Local helper for distributed lookup (called via RPC)
-spec lookup_distributed_local(client_name()) -> {ok, {client_pid(), client_metadata(), integer()}} | {error, not_found}.
lookup_distributed_local(Name) ->
    case lookup(Name) of
        {ok, Pid, Metadata, Timestamp} -> {ok, {Pid, Metadata, Timestamp}};
        {error, not_found} -> {error, not_found}
    end.

%% @doc List clients on multiple nodes
-spec list_clients_on_nodes() -> [{client_name(), node()}].
list_clients_on_nodes() ->
    Nodes = erlmcp_cluster:cluster_nodes(),
    list_clients_on_nodes(Nodes, []).

%% @doc Helper function for listing clients on nodes
-spec list_clients_on_nodes([node()], [{client_name(), node()}]) -> [{client_name(), node()}].
list_clients_on_nodes([], Acc) ->
    Acc;
list_clients_on_nodes([Node | Rest], Acc) ->
    case Node =:= node() of
        true ->
            %% Skip local node - already handled
            list_clients_on_nodes(Rest, Acc);
        false ->
            %% Get clients from remote node
            case erlmcp_cluster:distribute_call(Node, ?MODULE, list_clients_on_node_local, []) of
                {ok, Clients} ->
                    list_clients_on_nodes(Rest, Clients ++ Acc);
                {error, _} ->
                    list_clients_on_nodes(Rest, Acc)
            end
    end.

%% @doc Local helper for listing clients on a node (called via RPC)
-spec list_clients_on_node_local() -> {ok, [{client_name(), node()}]}.
list_clients_on_node_local() ->
    case list() of
        {ok, Names} -> {ok, [{Name, node()} || Name <- Names]};
        {error, no_clients} -> {ok, []}
    end.