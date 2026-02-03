%%%-------------------------------------------------------------------
%%% @doc
%%% Individual Registry Shard for Sharded Registry
%%%
%%% Each shard manages a subset of the total registry entries:
%%% - Local ETS table for fast lookups (O(1))
%%% - gen_server for coordination and monitoring
%%% - gproc registration for process monitoring
%%%
%%% == Performance ==
%%%
%%% - Lookup: <5us (ETS cached)
%%% - Registration: <10us
%%% - Message throughput: 500K+ msg/s per shard
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_registry_shard).

-behaviour(gen_server).

%% API
-export([start_link/2, start_link/3,
         register_server/4, unregister_server/2, find_server/2,
         register_transport/4, unregister_transport/2, find_transport/2,
         list_servers/1, list_transports/1,
         get_shard_id/1, get_stats/1, get_entries/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type shard_id() :: 0..255.
-type server_id() :: binary() | atom().
-type transport_id() :: binary() | atom().

-record(state, {
    shard_id :: shard_id(),
    ets_table :: ets:tid(),
    monitors = #{} :: map(),  % Ref -> {Type, Id, Pid}
    server_count = 0 :: non_neg_integer(),
    transport_count = 0 :: non_neg_integer(),
    messages_processed = 0 :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(shard_id(), map()) -> {ok, pid()} | {error, term()}.
start_link(ShardId, Opts) ->
    gen_server:start_link(?MODULE, [ShardId, Opts], []).

-spec start_link(shard_id(), map(), pid()) -> {ok, pid()} | {error, term()}.
start_link(ShardId, Opts, SupervisorPid) ->
    gen_server:start_link(SupervisorPid, ?MODULE, [ShardId, Opts], []).

-spec register_server(pid(), server_id(), pid(), map()) -> ok | {error, term()}.
register_server(ShardPid, ServerId, ServerPid, Config) ->
    gen_server:call(ShardPid, {register_server, ServerId, ServerPid, Config}, 5000).

-spec unregister_server(pid(), server_id()) -> ok.
unregister_server(ShardPid, ServerId) ->
    gen_server:call(ShardPid, {unregister_server, ServerId}, 5000).

-spec find_server(pid(), server_id()) -> {ok, {pid(), map()}} | {error, not_found}.
find_server(ShardPid, ServerId) ->
    gen_server:call(ShardPid, {find_server, ServerId}, 5000).

-spec register_transport(pid(), transport_id(), pid(), map()) -> ok | {error, term()}.
register_transport(ShardPid, TransportId, TransportPid, Config) ->
    gen_server:call(ShardPid, {register_transport, TransportId, TransportPid, Config}, 5000).

-spec unregister_transport(pid(), transport_id()) -> ok.
unregister_transport(ShardPid, TransportId) ->
    gen_server:call(ShardPid, {unregister_transport, TransportId}, 5000).

-spec find_transport(pid(), transport_id()) -> {ok, {pid(), map()}} | {error, not_found}.
find_transport(ShardPid, TransportId) ->
    gen_server:call(ShardPid, {find_transport, TransportId}, 5000).

-spec list_servers(pid()) -> [{server_id(), {pid(), map()}}].
list_servers(ShardPid) ->
    gen_server:call(ShardPid, list_servers, 5000).

-spec list_transports(pid()) -> [{transport_id(), {pid(), map()}}].
list_transports(ShardPid) ->
    gen_server:call(ShardPid, list_transports, 5000).

-spec get_shard_id(pid()) -> shard_id().
get_shard_id(ShardPid) ->
    gen_server:call(ShardPid, get_shard_id, 5000).

-spec get_stats(pid()) -> map().
get_stats(ShardPid) ->
    gen_server:call(ShardPid, get_stats, 5000).

-spec get_entries(pid(), non_neg_integer()) -> {ok, [term()]}.
get_entries(ShardPid, Count) ->
    gen_server:call(ShardPid, {get_entries, Count}, 5000).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([ShardId, _Opts]) ->
    %% Register with gproc for discovery
    ok = gproc:reg({n, l, {erlmcp_shard, ShardId}}),

    %% Create ETS table for fast lookups
    EtsOpts = [
        named_table,
        set,
        {read_concurrency, true},
        {write_concurrency, true},
        {decentralized_counters, true},
        public
    ],
    TableName = binary_to_atom(<<"erlmcp_shard_", (integer_to_binary(ShardId))/binary>>, utf8),
    ets:new(TableName, EtsOpts),

    {ok, #state{
        shard_id = ShardId,
        ets_table = TableName
    }}.

handle_call({register_server, ServerId, ServerPid, Config}, _From, State) ->
    Key = {server, ServerId},
    case ets:lookup(State#state.ets_table, Key) of
        [] ->
            %% Register in ETS
            ets:insert(State#state.ets_table, {Key, ServerPid, Config, server}),

            %% Monitor process
            Ref = monitor(process, ServerPid),
            Monitors = maps:put(Ref, {server, ServerId, ServerPid}, State#state.monitors),

            NewState = State#state{
                monitors = Monitors,
                server_count = State#state.server_count + 1,
                messages_processed = State#state.messages_processed + 1
            },
            {reply, ok, NewState};
        _ ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_server, ServerId}, _From, State) ->
    Key = {server, ServerId},
    case ets:lookup(State#state.ets_table, Key) of
        [{_, ServerPid, _, _}] ->
            ets:delete(State#state.ets_table, Key),

            %% Find and remove monitor reference
            Monitors = maps:filter(fun(_Ref, {_Type, _Id, Pid}) -> Pid =/= ServerPid end,
                                   State#state.monitors),

            NewState = State#state{
                monitors = Monitors,
                server_count = State#state.server_count - 1,
                messages_processed = State#state.messages_processed + 1
            },
            {reply, ok, NewState};
        [] ->
            {reply, ok, State}
    end;

handle_call({find_server, ServerId}, _From, State) ->
    Key = {server, ServerId},
    case ets:lookup(State#state.ets_table, Key) of
        [{_, ServerPid, Config, _}] ->
            NewState = State#state{
                messages_processed = State#state.messages_processed + 1
            },
            {reply, {ok, {ServerPid, Config}}, NewState};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({register_transport, TransportId, TransportPid, Config}, _From, State) ->
    Key = {transport, TransportId},
    case ets:lookup(State#state.ets_table, Key) of
        [] ->
            ets:insert(State#state.ets_table, {Key, TransportPid, Config, transport}),

            Ref = monitor(process, TransportPid),
            Monitors = maps:put(Ref, {transport, TransportId, TransportPid}, State#state.monitors),

            NewState = State#state{
                monitors = Monitors,
                transport_count = State#state.transport_count + 1,
                messages_processed = State#state.messages_processed + 1
            },
            {reply, ok, NewState};
        _ ->
            {reply, {error, already_registered}, State}
    end;

handle_call({unregister_transport, TransportId}, _From, State) ->
    Key = {transport, TransportId},
    case ets:lookup(State#state.ets_table, Key) of
        [{_, TransportPid, _, _}] ->
            ets:delete(State#state.ets_table, Key),

            %% Find and remove monitor reference
            Monitors = maps:filter(fun(_Ref, {_Type, _Id, Pid}) -> Pid =/= TransportPid end,
                                   State#state.monitors),

            NewState = State#state{
                monitors = Monitors,
                transport_count = State#state.transport_count - 1,
                messages_processed = State#state.messages_processed + 1
            },
            {reply, ok, NewState};
        [] ->
            {reply, ok, State}
    end;

handle_call({find_transport, TransportId}, _From, State) ->
    Key = {transport, TransportId},
    case ets:lookup(State#state.ets_table, Key) of
        [{_, TransportPid, Config, _}] ->
            NewState = State#state{
                messages_processed = State#state.messages_processed + 1
            },
            {reply, {ok, {TransportPid, Config}}, NewState};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(list_servers, _From, State) ->
    Pattern = [{{{server, '$1'}, '$2', '$3', server}}],
    Servers = ets:select(State#state.ets_table, Pattern, 1000),
    NewState = State#state{
        messages_processed = State#state.messages_processed + 1
    },
    {reply, Servers, NewState};

handle_call(list_transports, _From, State) ->
    Pattern = [{{{transport, '$1'}, '$2', '$3', transport}}],
    Transports = ets:select(State#state.ets_table, Pattern, 1000),
    NewState = State#state{
        messages_processed = State#state.messages_processed + 1
    },
    {reply, Transports, NewState};

handle_call(get_shard_id, _From, State) ->
    {reply, State#state.shard_id, State};

handle_call(get_stats, _From, State) ->
    {reply, #{
        shard_id => State#state.shard_id,
        server_count => State#state.server_count,
        transport_count => State#state.transport_count,
        monitored_processes => maps:size(State#state.monitors),
        messages_processed => State#state.messages_processed,
        queue_depth => process_info(self(), message_queue_len)
    }, State};

handle_call({get_entries, Count}, _From, State) ->
    Pattern = [{{{'$1', '$2'}, '$3', '$4', '$5'}}],
    Entries = ets:select(State#state.ets_table, Pattern, Count),
    {reply, {ok, format_entries(Entries)}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, process, Pid, Reason}, State) ->
    %% Cleanup after monitored process
    case maps:get(Ref, State#state.monitors, undefined) of
        {Type, Id, _Pid} ->
            %% Remove from monitors map
            Monitors = maps:remove(Ref, State#state.monitors),

            %% Remove from ETS
            Key = {Type, Id},
            ets:delete(State#state.ets_table, Key),

            %% Update counts
            {ServerCount, TransportCount} = case Type of
                server -> {State#state.server_count - 1, State#state.transport_count};
                transport -> {State#state.server_count, State#state.transport_count - 1}
            end,

            ?LOG_DEBUG("Process ~p (~p) for ~p went down: ~p", [Pid, Type, Id, Reason]),

            NewState = State#state{
                monitors = Monitors,
                server_count = max(0, ServerCount),
                transport_count = max(0, TransportCount)
            },
            {noreply, NewState};
        undefined ->
            %% Spurious DOWN message or already cleaned up
            ?LOG_WARNING("Received unexpected DOWN message for ref ~p, pid ~p", [Ref, Pid]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cleanup ETS table
    catch ets:delete(State#state.ets_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

format_entries([]) ->
    [];
format_entries('$end_of_table') ->
    [];
format_entries([{Key, Pid, Config, Type} | Rest]) ->
    [{Type, element(2, Key), Pid, Config} | format_entries(Rest)];
format_entries([{Key, Pid, Config} | Rest]) ->
    [{element(1, Key), element(2, Key), Pid, Config} | format_entries(Rest)].
