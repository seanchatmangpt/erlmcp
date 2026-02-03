%% @doc Database Sharding Manager for erlmcp
%% Implements sharding and replication for high availability
-module(erlmcp_shard_manager).
-behaviour(gen_server).

%% API
-export([start_link/0, get_shard/1, get_shard_nodes/1, write_data/3,
         read_data/2, set_active_region/1, get_shard_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

-record(shard_info, {
    shard_id :: integer(),
    primary_node :: node(),
    secondary_nodes :: [node()],
    status :: 'active' | 'readonly' | 'failed' | 'recovering',
    last_updated :: integer(),
    write_count :: integer(),
    read_count :: integer()
}).

-record(state, {
    shard_count :: integer(),
    replication_factor :: integer(),
    current_region :: 'primary' | 'secondary' | 'tertiary',
    shard_table :: ets:tid(),
    health_check_interval :: pos_integer(),
    write_quorum :: integer()
}).

-define(TAB, erlmcp_shards).
-define(SHARD_COUNT, 12).
-define(REPLICATION_FACTOR, 3).
-define(WRITE_QUORUM, 2).
-define(HEALTH_CHECK_INTERVAL, 10000). % 10 seconds
-define(READ_CONSISTENCY, strong).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_shard(Key) ->
    gen_server:call(?MODULE, {get_shard, Key}).

get_shard_nodes(ShardId) ->
    gen_server:call(?MODULE, {get_shard_nodes, ShardId}).

write_data(Key, Value, Consistency) ->
    gen_server:call(?MODULE, {write_data, Key, Value, Consistency}).

read_data(Key, Consistency) ->
    gen_server:call(?MODULE, {read_data, Key, Consistency}).

set_active_region(Region) ->
    gen_server:cast(?MODULE, {set_active_region, Region}).

get_shard_status(ShardId) ->
    gen_server:call(?MODULE, {get_shard_status, ShardId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    %% Create ETS table for shard metadata
    ShardTable = ets:new(?TAB, [
        set,
        public,
        {keypos, #shard_info.shard_id},
        named_table
    ]),

    %% Initialize shards
    initialize_shards(ShardTable),

    State = #state{
        shard_count = ?SHARD_COUNT,
        replication_factor = ?REPLICATION_FACTOR,
        current_region = primary,
        shard_table = ShardTable,
        health_check_interval = ?HEALTH_CHECK_INTERVAL,
        write_quorum = ?WRITE_QUORUM
    },

    %% Start health monitoring
    spawn_health_monitor(State),

    {ok, State}.

handle_call({get_shard, Key}, _From, State) ->
    ShardId = calculate_shard_id(Key),
    {reply, {ok, ShardId}, State};

handle_call({get_shard_nodes, ShardId}, _From, State) ->
    Reply = get_shard_nodes_internal(ShardId, State),
    {reply, Reply, State};

handle_call({write_data, Key, Value, Consistency}, _From, State) ->
    ShardId = calculate_shard_id(Key),
    Reply = write_data_to_shard(ShardId, Key, Value, Consistency, State),
    {reply, Reply, State};

handle_call({read_data, Key, Consistency}, _From, State) ->
    ShardId = calculate_shard_id(Key),
    Reply = read_data_from_shard(ShardId, Key, Consistency, State),
    {reply, Reply, State};

handle_call({get_shard_status, ShardId}, _From, State) ->
    case ets:lookup(State#state.shard_table, ShardId) of
        [ShardInfo] ->
            {reply, {ok, ShardInfo#shard_info.status}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({set_active_region, Region}, State) ->
    logger:info("Setting active region to ~p", [Region]),
    NewState = State#state{current_region = Region},
    %% Notify all shards of region change
    update_shard_regions(NewState),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(health_check_interval, State) ->
    perform_health_checks(State),
    spawn_health_monitor(State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Cleanup ETS table
    ets:delete(?TAB),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

initialize_shards(ShardTable) ->
    %% Initialize shard information for all shards
    Shards = lists:map(fun(ShardId) ->
        #shard_info{
            shard_id = ShardId,
            primary_node = get_primary_node_for_shard(ShardId),
            secondary_nodes = get_secondary_nodes_for_shard(ShardId),
            status = active,
            last_updated = erlang:system_time(millisecond),
            write_count = 0,
            read_count = 0
        }
    end, lists:seq(1, ?SHARD_COUNT)),

    %% Insert all shards into ETS
    lists:foreach(fun(Shard) ->
        true = ets:insert(ShardTable, Shard)
    end, Shards).

calculate_shard_id(Key) ->
    %% Consistent hashing using murmur3
    Hash = murmur3:hash128(term_to_binary(Key)),
    (Hash rem ?SHARD_COUNT) + 1.

murmur3_hash128(Data) ->
    %% Simple implementation of murmur3 hash
    %% In production, use a proper murmur3 implementation
    erlang:phash2(Data).

get_primary_node_for_shard(ShardId) ->
    %% Distribute shards across regions
    case ShardId rem 3 of
        1 -> 'us-east-1-node1@10.0.1.10';
        2 -> 'us-east-1-node2@10.0.1.11';
        0 -> 'us-east-1-node3@10.0.1.12'
    end.

get_secondary_nodes_for_shard(ShardId) ->
    %% Get secondary nodes for replication
    case ShardId rem 3 of
        1 -> ['eu-west-1-node1@10.0.2.10', 'eu-west-1-node2@10.0.2.11'];
        2 -> ['eu-west-1-node2@10.0.2.11', 'eu-west-1-node3@10.0.2.12'];
        0 -> ['eu-west-1-node3@10.0.2.12', 'us-east-1-node1@10.0.1.10']
    end.

get_shard_nodes_internal(ShardId, State) ->
    case ets:lookup(State#state.shard_table, ShardId) of
        [ShardInfo] ->
            Nodes = [ShardInfo#shard_info.primary_node | ShardInfo#shard_info.secondary_nodes],
            {ok, Nodes};
        [] ->
            {error, not_found}
    end.

write_data_to_shard(ShardId, Key, Value, Consistency, State) ->
    case ets:lookup(State#state.shard_table, ShardId) of
        [ShardInfo] ->
            case ShardInfo#shard_info.status of
                active ->
                    %% Perform write operation
                    case Consistency of
                        strong ->
                            write_with_consistency(ShardInfo, Key, Value, strong);
                        eventual ->
                            write_with_consistency(ShardInfo, Key, Value, eventual)
                    end;
                _ ->
                    {error, shard_not_active}
            end;
        [] ->
            {error, not_found}
    end.

write_with_consistency(ShardInfo, Key, Value, Consistency) ->
    PrimaryNode = ShardInfo#shard_info.primary_node,
    SecondaryNodes = ShardInfo#shard_info.secondary_nodes,

    %% Write to primary
    case rpc:call(PrimaryNode, erlmcp_shard_storage, write, [Key, Value]) of
        ok ->
            %% Replicate to secondaries
            SuccessfulWrites = [begin
                case rpc:call(Node, erlmcp_shard_storage, write, [Key, Value]) of
                    ok -> ok;
                    _ -> failed
                end
            end || Node <- SecondaryNodes],

            %% Check quorum
            case Consistency of
                strong ->
                    case SuccessfulWrites of
                        [ok | _] -> ok; % Primary + at least one secondary
                        _ -> {error, quorum_not_met}
                    end;
                eventual ->
                    ok % Success if primary wrote
            end;
        Error ->
            Error
    end.

read_data_from_shard(ShardId, Key, Consistency, State) ->
    case ets:lookup(State#state.shard_table, ShardId) of
        [ShardInfo] ->
            case Consistency of
                strong ->
                    %% Read from primary for strong consistency
                    read_from_node(ShardInfo#shard_info.primary_node, Key);
                eventual ->
                    %% Read from any available node
                    read_from_any_node(ShardInfo, Key)
            end;
        [] ->
            {error, not_found}
    end.

read_from_node(Node, Key) ->
    case is_node_healthy(Node) of
        true ->
            rpc:call(Node, erlmcp_shard_storage, read, [Key]);
        false ->
            {error, node_unavailable}
    end.

read_from_any_node(ShardInfo, Key) ->
    %% Try to read from any available node
    Nodes = [ShardInfo#shard_info.primary_node | ShardInfo#shard_info.secondary_nodes],

    case [N || N <- Nodes, is_node_healthy(N)] of
        [ReadNode | _] ->
            read_from_node(ReadNode, Key);
        [] ->
            {error, no_nodes_available}
    end.

is_node_healthy(Node) ->
    case net_adm:ping(Node) of
        pong ->
            case rpc:call(Node, erlmcp_health_check, status, [], 2000) of
                ok -> true;
                _ -> false
            end;
        _ ->
            false
    end.

spawn_health_monitor(State) ->
    spawn(fun() ->
        timer:sleep(State#state.health_check_interval),
        gen_server:cast(?MODULE, health_check)
    end).

perform_health_checks(State) ->
    %% Check health of all shards
    Shards = ets:tab2list(State#state.shard_table),

    lists:foreach(fun(Shard) ->
        %% Check primary node health
        PrimaryHealthy = is_node_healthy(Shard#shard_info.primary_node),

        %% Check secondary nodes health
        SecondaryHealthy = lists:any(fun is_node_healthy/1, Shard#shard_info.secondary_nodes),

        %% Update shard status based on health
        NewStatus = case PrimaryHealthy of
            true when SecondaryHealthy -> active;
            true -> readonly;
            false when SecondaryHealthy -> readonly;
            false -> failed
        end,

        %% Update shard record
        UpdatedShard = Shard#shard_info{
            status = NewStatus,
            last_updated = erlang:system_time(millisecond)
        },

        true = ets:insert(State#state.shard_table, UpdatedShard)
    end, Shards).

update_shard_regions(State) ->
    %% Update all shards with new region information
    Shards = ets:tab2list(State#state.shard_table),

    lists:foreach(fun(Shard) ->
        UpdatedShard = Shard#shard_info{
            primary_node = get_primary_node_for_region(State#state.current_region, Shard#shard_info.shard_id),
            secondary_nodes = get_secondary_nodes_for_region(State#state.current_region, Shard#shard_info.shard_id),
            last_updated = erlang:system_time(millisecond)
        },

        true = ets:insert(State#state.shard_table, UpdatedShard)
    end, Shards).

get_primary_node_for_region(primary, ShardId) ->
    get_primary_node_for_shard(ShardId);
get_primary_node_for_region(secondary, ShardId) ->
    case ShardId rem 3 of
        1 -> 'eu-west-1-node1@10.0.2.10';
        2 -> 'eu-west-1-node2@10.0.2.11';
        0 -> 'eu-west-1-node3@10.0.2.12'
    end;
get_primary_node_for_region(tertiary, ShardId) ->
    case ShardId rem 2 of
        1 -> 'ap-southeast-1-node1@10.0.3.10';
        0 -> 'ap-southeast-1-node2@10.0.3.11'
    end.

get_secondary_nodes_for_region(primary, ShardId) ->
    get_secondary_nodes_for_shard(ShardId);
get_secondary_nodes_for_region(secondary, ShardId) ->
    case ShardId rem 3 of
        1 -> ['us-east-1-node1@10.0.1.10', 'us-east-1-node2@10.0.1.11'];
        2 -> ['us-east-1-node2@10.0.1.11', 'us-east-1-node3@10.0.1.12'];
        0 -> ['us-east-1-node3@10.0.1.12', 'eu-west-1-node1@10.0.2.10']
    end;
get_secondary_nodes_for_region(tertiary, ShardId) ->
    case ShardId rem 2 of
        1 -> ['us-east-1-node1@10.0.1.10', 'eu-west-1-node1@10.0.2.10'];
        0 -> ['eu-west-1-node1@10.0.2.10', 'ap-southeast-1-node1@10.0.3.10']
    end.