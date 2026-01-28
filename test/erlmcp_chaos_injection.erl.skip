%%%-------------------------------------------------------------------
%%% @doc
%%% Chaos Injection Utilities
%%% Provides functions to inject various failure modes into the system
%%% for testing resilience and recovery capabilities
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_injection).

%% API
-export([
    kill_random_process/0,
    simulate_network_partition/0,
    heal_network_partition/0,
    enable_message_drops/1,
    disable_message_drops/0,
    enable_connection_drops/1,
    disable_connection_drops/0,
    set_latency/1,
    clear_latency/0,
    crash_random_process/0,
    enable_message_reordering/1,
    disable_message_reordering/0,
    enable_all_chaos/0,
    disable_all_chaos/0,
    get_chaos_status/0,
    should_drop_message/0,
    should_drop_connection/0,
    should_reorder_message/0,
    is_network_partitioned/0,
    get_latency_ms/0,
    apply_latency/0,
    should_apply_chaos/0
]).

%% State tracking
-define(CHAOS_STATE_KEY, erlmcp_chaos_state).

%%====================================================================
%% API Functions
%%====================================================================

%% Kill a random process in the erlmcp system
-spec kill_random_process() -> ok | {error, term()}.
kill_random_process() ->
    case get_random_erlmcp_process() of
        {ok, Pid} ->
            try
                exit(Pid, chaos_killed),
                ok
            catch
                _:_ -> {error, process_already_dead}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Simulate network partition
-spec simulate_network_partition() -> ok.
simulate_network_partition() ->
    put(?CHAOS_STATE_KEY, #{network_partition => true, start_time => erlang:monotonic_time(millisecond)}),
    ok.

%% Heal network partition
-spec heal_network_partition() -> ok.
heal_network_partition() ->
    put(?CHAOS_STATE_KEY, #{network_partition => false, end_time => erlang:monotonic_time(millisecond)}),
    ok.

%% Enable message drops (% of messages dropped)
-spec enable_message_drops(float()) -> ok.
enable_message_drops(DropRate) when is_float(DropRate), DropRate >= 0.0, DropRate =< 1.0 ->
    State = get_chaos_state(),
    NewState = State#{message_drop_rate => DropRate, message_drops_enabled => true},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Disable message drops
-spec disable_message_drops() -> ok.
disable_message_drops() ->
    State = get_chaos_state(),
    NewState = State#{message_drops_enabled => false},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Enable connection drops (% of connections forced closed)
-spec enable_connection_drops(float()) -> ok.
enable_connection_drops(DropRate) when is_float(DropRate), DropRate >= 0.0, DropRate =< 1.0 ->
    State = get_chaos_state(),
    NewState = State#{connection_drop_rate => DropRate, connection_drops_enabled => true},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Disable connection drops
-spec disable_connection_drops() -> ok.
disable_connection_drops() ->
    State = get_chaos_state(),
    NewState = State#{connection_drops_enabled => false},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Set network latency (milliseconds)
-spec set_latency(non_neg_integer()) -> ok.
set_latency(LatencyMs) when is_integer(LatencyMs), LatencyMs >= 0 ->
    State = get_chaos_state(),
    NewState = State#{latency_ms => LatencyMs},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Clear network latency
-spec clear_latency() -> ok.
clear_latency() ->
    State = get_chaos_state(),
    NewState = State#{latency_ms => 0},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Crash a random process (ungraceful exit)
-spec crash_random_process() -> ok | {error, term()}.
crash_random_process() ->
    case get_random_erlmcp_process() of
        {ok, Pid} ->
            try
                exit(Pid, kill),
                ok
            catch
                _:_ -> {error, process_already_dead}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Enable message reordering
-spec enable_message_reordering(float()) -> ok.
enable_message_reordering(ReorderRate) when is_float(ReorderRate), ReorderRate >= 0.0, ReorderRate =< 1.0 ->
    State = get_chaos_state(),
    NewState = State#{message_reorder_rate => ReorderRate, message_reordering_enabled => true},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Disable message reordering
-spec disable_message_reordering() -> ok.
disable_message_reordering() ->
    State = get_chaos_state(),
    NewState = State#{message_reordering_enabled => false},
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Enable all chaos modes simultaneously
-spec enable_all_chaos() -> ok.
enable_all_chaos() ->
    State = get_chaos_state(),
    NewState = State#{
        message_drops_enabled => true,
        message_drop_rate => 0.01,  % 1%
        connection_drops_enabled => true,
        connection_drop_rate => 0.05,  % 5%
        latency_ms => 50,
        message_reordering_enabled => true,
        message_reorder_rate => 0.02,  % 2%
        all_chaos_enabled => true
    },
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Disable all chaos modes
-spec disable_all_chaos() -> ok.
disable_all_chaos() ->
    State = get_chaos_state(),
    NewState = State#{
        message_drops_enabled => false,
        connection_drops_enabled => false,
        latency_ms => 0,
        message_reordering_enabled => false,
        all_chaos_enabled => false,
        network_partition => false
    },
    put(?CHAOS_STATE_KEY, NewState),
    ok.

%% Get current chaos state
-spec get_chaos_status() -> map().
get_chaos_status() ->
    get_chaos_state().

%%====================================================================
%% Internal Functions
%%====================================================================

%% Get or initialize chaos state
-spec get_chaos_state() -> map().
get_chaos_state() ->
    case get(?CHAOS_STATE_KEY) of
        undefined ->
            Default = #{
                message_drops_enabled => false,
                connection_drops_enabled => false,
                latency_ms => 0,
                message_reordering_enabled => false,
                network_partition => false,
                all_chaos_enabled => false
            },
            put(?CHAOS_STATE_KEY, Default),
            Default;
        State -> State
    end.

%% Get a random erlmcp process (client, server, registry, etc.)
-spec get_random_erlmcp_process() -> {ok, pid()} | {error, no_processes}.
get_random_erlmcp_process() ->
    %% Find all processes related to erlmcp
    AllProcesses = processes(),
    ErlmcpProcesses = lists:filter(fun(Pid) ->
        case catch process_info(Pid, [initial_call]) of
            [{initial_call, {Module, _, _}}] ->
                is_erlmcp_module(Module);
            _ -> false
        end
    end, AllProcesses),

    case ErlmcpProcesses of
        [] ->
            {error, no_processes};
        Processes ->
            RandomIndex = rand:uniform(length(Processes)),
            {ok, lists:nth(RandomIndex, Processes)}
    end.

%% Check if a module is part of erlmcp
-spec is_erlmcp_module(atom()) -> boolean().
is_erlmcp_module(Module) ->
    ModuleStr = atom_to_list(Module),
    string:prefix(ModuleStr, "erlmcp") =:= ModuleStr orelse
    string:prefix(ModuleStr, "tcps") =:= ModuleStr.

%% Check if message drop should occur
-spec should_drop_message() -> boolean().
should_drop_message() ->
    State = get_chaos_state(),
    case maps:get(message_drops_enabled, State, false) of
        false -> false;
        true ->
            DropRate = maps:get(message_drop_rate, State, 0.0),
            rand:uniform() < DropRate
    end.

%% Check if connection drop should occur
-spec should_drop_connection() -> boolean().
should_drop_connection() ->
    State = get_chaos_state(),
    case maps:get(connection_drops_enabled, State, false) of
        false -> false;
        true ->
            DropRate = maps:get(connection_drop_rate, State, 0.0),
            rand:uniform() < DropRate
    end.

%% Check if message should be reordered
-spec should_reorder_message() -> boolean().
should_reorder_message() ->
    State = get_chaos_state(),
    case maps:get(message_reordering_enabled, State, false) of
        false -> false;
        true ->
            ReorderRate = maps:get(message_reorder_rate, State, 0.0),
            rand:uniform() < ReorderRate
    end.

%% Check if network is partitioned
-spec is_network_partitioned() -> boolean().
is_network_partitioned() ->
    State = get_chaos_state(),
    maps:get(network_partition, State, false).

%% Get current latency
-spec get_latency_ms() -> non_neg_integer().
get_latency_ms() ->
    State = get_chaos_state(),
    maps:get(latency_ms, State, 0).

%% Apply latency to a message
-spec apply_latency() -> ok.
apply_latency() ->
    LatencyMs = get_latency_ms(),
    case LatencyMs > 0 of
        true ->
            %% Add some jitter (±20%)
            Jitter = (LatencyMs * 20) div 100,
            ActualLatency = LatencyMs + (rand:uniform(Jitter * 2) - Jitter),
            timer:sleep(max(0, ActualLatency));
        false ->
            ok
    end.

%% Check if should apply chaos to this operation
-spec should_apply_chaos() -> boolean().
should_apply_chaos() ->
    State = get_chaos_state(),
    maps:get(all_chaos_enabled, State, false).
