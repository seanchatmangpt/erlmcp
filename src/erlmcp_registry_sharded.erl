-module(erlmcp_registry_sharded).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    register_server/3,
    register_transport/3,
    unregister_server/1,
    unregister_transport/1,
    route_to_server/3,
    route_to_transport/3,
    find_server/1,
    find_transport/1,
    list_servers/0,
    list_transports/0,
    bind_transport_to_server/2,
    unbind_transport/1,
    get_server_for_transport/1,
    get_partition_stats/0,
    get_partition_stats/1,
    reset_stats/0,
    get_contention_status/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Default configuration
-define(DEFAULT_PARTITION_COUNT, 16).

%% Type definitions
-type partition_id() :: 0..15.
-type server_id() :: binary().
-type transport_id() :: atom() | binary().

-record(state, {
    partition_count :: pos_integer(),
    partition_tables :: [atom()],
    server_transport_map = #{} :: #{transport_id() => server_id()},
    latency_history :: #{partition_id() => [non_neg_integer()]},
    write_count :: #{partition_id() => non_neg_integer()},
    contention_alarms = #{} :: #{partition_id() => boolean()},
    admission_control_enabled = false :: boolean(),
    stats_timer_ref :: reference() | undefined
}).

-type state() :: #state{}.

%% API Functions
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_PARTITION_COUNT).

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(PartitionCount) when PartitionCount > 0, PartitionCount =< 64 ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PartitionCount], []).

%% Stub API to match erlmcp_registry interface
register_server(ServerId, ServerPid, Config) when is_pid(ServerPid) ->
    gen_server:call(?MODULE, {register_server, ServerId, ServerPid, Config}).

register_transport(TransportId, TransportPid, Config) when is_pid(TransportPid) ->
    gen_server:call(?MODULE, {register_transport, TransportId, TransportPid, Config}).

unregister_server(ServerId) ->
    gen_server:call(?MODULE, {unregister_server, ServerId}).

unregister_transport(TransportId) ->
    gen_server:call(?MODULE, {unregister_transport, TransportId}).

route_to_server(ServerId, TransportId, Message) ->
    gen_server:cast(?MODULE, {route_to_server, ServerId, TransportId, Message}).

route_to_transport(TransportId, ServerId, Message) ->
    gen_server:cast(?MODULE, {route_to_transport, TransportId, ServerId, Message}).

find_server(ServerId) ->
    gen_server:call(?MODULE, {find_server, ServerId}).

find_transport(TransportId) ->
    gen_server:call(?MODULE, {find_transport, TransportId}).

list_servers() ->
    gen_server:call(?MODULE, list_servers).

list_transports() ->
    gen_server:call(?MODULE, list_transports).

bind_transport_to_server(TransportId, ServerId) ->
    gen_server:call(?MODULE, {bind_transport_to_server, TransportId, ServerId}).

unbind_transport(TransportId) ->
    gen_server:call(?MODULE, {unbind_transport, TransportId}).

get_server_for_transport(TransportId) ->
    gen_server:call(?MODULE, {get_server_for_transport, TransportId}).

get_partition_stats() ->
    gen_server:call(?MODULE, get_partition_stats).

get_partition_stats(PartitionId) ->
    gen_server:call(?MODULE, {get_partition_stats, PartitionId}).

reset_stats() ->
    gen_server:call(?MODULE, reset_stats).

get_contention_status() ->
    gen_server:call(?MODULE, get_contention_status).

%% gen_server callbacks
-spec init([pos_integer()]) -> {ok, state()}.
init([PartitionCount]) ->
    process_flag(trap_exit, true),
    logger:info("Starting sharded MCP registry with ~p partitions", [PartitionCount]),

    Tables = create_partition_tables(PartitionCount),

    LatencyHistory = maps:from_list([{I, []} || I <- lists:seq(0, PartitionCount - 1)]),
    WriteCount = maps:from_list([{I, 0} || I <- lists:seq(0, PartitionCount - 1)]),

    {ok, StatsTimer} = timer:send_interval(1000, {stats_tick}),

    logger:info("Sharded registry initialized with ~p partitions: ~p", [PartitionCount, Tables]),

    {ok, #state{
        partition_count = PartitionCount,
        partition_tables = Tables,
        latency_history = LatencyHistory,
        write_count = WriteCount,
        stats_timer_ref = StatsTimer
    }}.

handle_call({register_server, ServerId, ServerPid, Config}, _From, State) ->
    PartitionId = partition_for_server(ServerId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    case ets:lookup(Table, {server, ServerId}) of
        [{_, _, _}] ->
            {reply, {error, already_registered}, State};
        [] ->
            Start = erlang:system_time(microsecond),
            try
                gproc:reg_other({n, l, {mcp, server, ServerId}}, ServerPid, Config),
                gproc:monitor({n, l, {mcp, server, ServerId}}),
                true = ets:insert(Table, {{server, ServerId}, ServerPid, Config}),
                ElapsedUs = erlang:system_time(microsecond) - Start,
                ElapsedMs = max(1, ElapsedUs div 1000),
                NewState = record_write_latency(State, PartitionId, ElapsedMs),
                logger:info("Registered server ~p with pid ~p (~p ms)", [ServerId, ServerPid, ElapsedMs]),
                {reply, ok, NewState}
            catch
                error:badarg ->
                    logger:warning("Server ~p already registered by another process", [ServerId]),
                    {reply, {error, already_registered}, State}
            end
    end;

handle_call({register_transport, TransportId, TransportPid, Config}, _From, State) ->
    PartitionId = partition_for_transport(TransportId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    case ets:lookup(Table, {transport, TransportId}) of
        [{_, _, _}] ->
            {reply, {error, already_registered}, State};
        [] ->
            Start = erlang:system_time(microsecond),
            try
                gproc:reg_other({n, l, {mcp, transport, TransportId}}, TransportPid, Config),
                gproc:monitor({n, l, {mcp, transport, TransportId}}),
                true = ets:insert(Table, {{transport, TransportId}, TransportPid, Config}),
                ElapsedUs = erlang:system_time(microsecond) - Start,
                ElapsedMs = max(1, ElapsedUs div 1000),
                NewState = record_write_latency(State, PartitionId, ElapsedMs),

                NewState2 = case maps:get(server_id, Config, undefined) of
                    undefined -> NewState;
                    ServerId ->
                        NewMap = maps:put(TransportId, ServerId, State#state.server_transport_map),
                        NewState#state{server_transport_map = NewMap}
                end,

                logger:info("Registered transport ~p with pid ~p (~p ms)", [TransportId, TransportPid, ElapsedMs]),
                {reply, ok, NewState2}
            catch
                error:badarg ->
                    logger:warning("Transport ~p already registered", [TransportId]),
                    {reply, {error, already_registered}, State}
            end
    end;

handle_call({unregister_server, ServerId}, _From, State) ->
    PartitionId = partition_for_server(ServerId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    Start = erlang:system_time(microsecond),
    ets:delete(Table, {server, ServerId}),
    ElapsedUs = erlang:system_time(microsecond) - Start,
    ElapsedMs = max(1, ElapsedUs div 1000),
    NewState = record_write_latency(State, PartitionId, ElapsedMs),

    try
        gproc:unreg_other({n, l, {mcp, server, ServerId}}, undefined)
    catch
        error:badarg -> ok
    end,

    NewTransportMap = maps:filter(fun(_, SId) -> SId =/= ServerId end,
                                 State#state.server_transport_map),

    NewState2 = NewState#state{server_transport_map = NewTransportMap},
    logger:info("Unregistered server ~p", [ServerId]),
    {reply, ok, NewState2};

handle_call({unregister_transport, TransportId}, _From, State) ->
    PartitionId = partition_for_transport(TransportId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    Start = erlang:system_time(microsecond),
    ets:delete(Table, {transport, TransportId}),
    ElapsedUs = erlang:system_time(microsecond) - Start,
    ElapsedMs = max(1, ElapsedUs div 1000),
    NewState = record_write_latency(State, PartitionId, ElapsedMs),

    try
        gproc:unreg_other({n, l, {mcp, transport, TransportId}}, undefined)
    catch
        error:badarg -> ok
    end,

    NewTransportMap = maps:remove(TransportId, State#state.server_transport_map),
    NewState2 = NewState#state{server_transport_map = NewTransportMap},
    logger:info("Unregistered transport ~p", [TransportId]),
    {reply, ok, NewState2};

handle_call({find_server, ServerId}, _From, State) ->
    PartitionId = partition_for_server(ServerId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    Result = case ets:lookup(Table, {server, ServerId}) of
        [{_, ServerPid, Config}] ->
            {ok, {ServerPid, Config}};
        [] ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call({find_transport, TransportId}, _From, State) ->
    PartitionId = partition_for_transport(TransportId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    Result = case ets:lookup(Table, {transport, TransportId}) of
        [{_, TransportPid, Config}] ->
            {ok, {TransportPid, Config}};
        [] ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call(list_servers, _From, State) ->
    Servers = scan_all_partitions(State#state.partition_tables, server),
    {reply, Servers, State};

handle_call(list_transports, _From, State) ->
    Transports = scan_all_partitions(State#state.partition_tables, transport),
    {reply, Transports, State};

handle_call({bind_transport_to_server, TransportId, ServerId}, _From, State) ->
    ServerPartitionId = partition_for_server(ServerId, State#state.partition_count),
    ServerTable = lists:nth(ServerPartitionId + 1, State#state.partition_tables),
    ServerExists = ets:member(ServerTable, {server, ServerId}),

    TransportPartitionId = partition_for_transport(TransportId, State#state.partition_count),
    TransportTable = lists:nth(TransportPartitionId + 1, State#state.partition_tables),
    TransportExists = ets:member(TransportTable, {transport, TransportId}),

    case {ServerExists, TransportExists} of
        {true, true} ->
            NewMap = maps:put(TransportId, ServerId, State#state.server_transport_map),
            NewState = State#state{server_transport_map = NewMap},
            logger:info("Bound transport ~p to server ~p", [TransportId, ServerId]),
            {reply, ok, NewState};
        {false, _} ->
            {reply, {error, server_not_found}, State};
        {_, false} ->
            {reply, {error, transport_not_found}, State}
    end;

handle_call({unbind_transport, TransportId}, _From, State) ->
    NewMap = maps:remove(TransportId, State#state.server_transport_map),
    NewState = State#state{server_transport_map = NewMap},
    {reply, ok, NewState};

handle_call({get_server_for_transport, TransportId}, _From, State) ->
    case maps:get(TransportId, State#state.server_transport_map, undefined) of
        undefined -> {reply, {error, not_found}, State};
        ServerId -> {reply, {ok, ServerId}, State}
    end;

handle_call(get_partition_stats, _From, State) ->
    Stats = maps:map(fun(PartId, _) ->
        build_partition_stats(State, PartId)
    end, State#state.latency_history),
    {reply, Stats, State};

handle_call({get_partition_stats, PartitionId}, _From, State) ->
    case maps:get(PartitionId, State#state.latency_history, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _ ->
            Stats = build_partition_stats(State, PartitionId),
            {reply, {ok, Stats}, State}
    end;

handle_call(reset_stats, _From, State) ->
    PartitionCount = State#state.partition_count,
    NewLatencyHistory = maps:from_list([{I, []} || I <- lists:seq(0, PartitionCount - 1)]),
    NewWriteCount = maps:from_list([{I, 0} || I <- lists:seq(0, PartitionCount - 1)]),
    NewState = State#state{
        latency_history = NewLatencyHistory,
        write_count = NewWriteCount,
        contention_alarms = #{}
    },
    logger:info("Reset all partition statistics"),
    {reply, ok, NewState};

handle_call(get_contention_status, _From, State) ->
    Status = #{
        admission_control => State#state.admission_control_enabled,
        active_alarms => maps:keys(maps:filter(fun(_, V) -> V end, State#state.contention_alarms))
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({route_to_server, ServerId, TransportId, Message}, State) ->
    PartitionId = partition_for_server(ServerId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    case ets:lookup(Table, {server, ServerId}) of
        [{_, ServerPid, _}] ->
            ServerPid ! {mcp_message, TransportId, Message},
            {noreply, State};
        [] ->
            logger:warning("Cannot route to server ~p: not found", [ServerId]),
            {noreply, State}
    end;

handle_cast({route_to_transport, broadcast, ServerId, Message}, State) ->
    case transports_for_server(ServerId, State) of
        [] ->
            logger:warning("Cannot broadcast to transports for server ~p: none bound", [ServerId]),
            {noreply, State};
        TransportIds ->
            lists:foreach(fun(TransportId) ->
                send_to_transport(TransportId, ServerId, Message, State)
            end, TransportIds),
            {noreply, State}
    end;

handle_cast({route_to_transport, TransportId, ServerId, Message}, State) ->
    send_to_transport(TransportId, ServerId, Message, State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({stats_tick}, State) ->
    NewState = check_contention(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(Reason, #state{stats_timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        Ref -> catch timer:cancel(Ref)
    end,
    logger:info("Sharded registry terminating: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal Functions

create_partition_tables(PartitionCount) ->
    [create_partition_table(I) || I <- lists:seq(0, PartitionCount - 1)].

create_partition_table(PartitionId) ->
    TableName = list_to_atom("registry_" ++ integer_to_list(PartitionId)),
    ets:new(TableName, [set, public, {write_concurrency, true}, {read_concurrency, true}]),
    TableName.

partition_for_server(ServerId, PartitionCount) ->
    erlang:phash2(ServerId) rem PartitionCount.

partition_for_transport(TransportId, PartitionCount) ->
    erlang:phash2(TransportId) rem PartitionCount.

record_write_latency(State, PartitionId, LatencyMs) ->
    LatencyHistory = State#state.latency_history,
    History = maps:get(PartitionId, LatencyHistory, []),
    NewHistory = lists:sublist([LatencyMs | History], 10),
    WriteCount = State#state.write_count,
    NewWriteCount = maps:update_with(PartitionId, fun(C) -> C + 1 end, 1, WriteCount),
    State#state{
        latency_history = maps:put(PartitionId, NewHistory, LatencyHistory),
        write_count = NewWriteCount
    }.

check_contention(State) ->
    PartitionCount = State#state.partition_count,
    Alarms = maps:from_list([
        {PartitionId, check_partition_contention(State, PartitionId)}
        || PartitionId <- lists:seq(0, PartitionCount - 1)
    ]),

    ActiveAlarmCount = maps:size(maps:filter(fun(_, V) -> V end, Alarms)),
    AdmissionControlThreshold = PartitionCount div 2,
    AdmissionControlActive = ActiveAlarmCount > AdmissionControlThreshold,

    State#state{
        contention_alarms = Alarms,
        admission_control_enabled = AdmissionControlActive
    }.

check_partition_contention(State, PartitionId) ->
    LatencyHistory = maps:get(PartitionId, State#state.latency_history, []),
    WriteCount = maps:get(PartitionId, State#state.write_count, 0),

    case {length(LatencyHistory), WriteCount} of
        {0, _} -> false;
        {_, WC} when WC < 10 -> false;
        _ ->
            AvgLatency = lists:sum(LatencyHistory) div length(LatencyHistory),
            AvgLatency > 100
    end.

build_partition_stats(State, PartitionId) ->
    LatencyHistory = maps:get(PartitionId, State#state.latency_history, []),
    WriteCount = maps:get(PartitionId, State#state.write_count, 0),
    IsAlarmed = maps:get(PartitionId, State#state.contention_alarms, false),

    case LatencyHistory of
        [] ->
            #{
                partition_id => PartitionId,
                write_count => WriteCount,
                avg_latency_ms => 0,
                max_latency_ms => 0,
                min_latency_ms => 0,
                contention_alarm => IsAlarmed
            };
        _ ->
            AvgLatency = lists:sum(LatencyHistory) div length(LatencyHistory),
            MaxLatency = lists:max(LatencyHistory),
            MinLatency = lists:min(LatencyHistory),
            #{
                partition_id => PartitionId,
                write_count => WriteCount,
                sample_count => length(LatencyHistory),
                avg_latency_ms => AvgLatency,
                max_latency_ms => MaxLatency,
                min_latency_ms => MinLatency,
                contention_alarm => IsAlarmed
            }
    end.

scan_all_partitions(Tables, EntryType) ->
    lists:concat([
        ets:match_object(Table, {{EntryType, '$1'}, '$2', '$3'})
        || Table <- Tables
    ]).

transports_for_server(ServerId, #state{server_transport_map = Map}) ->
    [TransportId || {TransportId, SId} <- maps:to_list(Map), SId =:= ServerId].

send_to_transport(TransportId, ServerId, Message, State) ->
    PartitionId = partition_for_transport(TransportId, State#state.partition_count),
    Table = lists:nth(PartitionId + 1, State#state.partition_tables),

    case ets:lookup(Table, {transport, TransportId}) of
        [{_, TransportPid, _}] ->
            TransportPid ! {mcp_response, ServerId, Message},
            ok;
        [] ->
            logger:warning("Cannot route to transport ~p: not found", [TransportId]),
            ok
    end.
