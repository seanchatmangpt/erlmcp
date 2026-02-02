%%%-------------------------------------------------------------------
%% @doc ETS Statistics and Health Monitoring
%%
%%% This module provides real-time monitoring of ETS table health,
%%% including:
%%%
%%% - Table size tracking (entries count)
%%% - Memory usage monitoring (bytes)
%%% - Growth rate detection (entries/second)
%%% - Alert thresholds for unusual growth
%%% - OTEL integration for metrics export
%%%
%%% OTP 28 SPECIFIC:
%%% - Monitors tables with >1M entries
%%% - Tracks decentralized_counters effectiveness
%%% - Alerts on memory pressure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ets_stats).

-behaviour(gen_server).

%% API
-export([start_link/0,
         register_table/2,
         unregister_table/1,
         get_table_stats/1,
         get_all_stats/0,
         set_growth_alert_threshold/2,
         set_memory_alert_threshold/2,
         enable_otl_export/0,
         disable_otl_export/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").
-include_lib("opentelemetry_api/include/otel_meter.hrl").

%%====================================================================
%% Types
%%====================================================================

-type table_name() :: atom().
-type table_type() :: registry | session | cache | rate_limiter | auth | unknown.
-type alert_threshold() :: #{max_entries => pos_integer(), max_memory_bytes => pos_integer()}.
-type table_snapshot() ::
    #{name := table_name(),
      type := table_type(),
      size := non_neg_integer(),
      memory := non_neg_integer(),
      growth_rate := float(),  % entries/second
      last_snapshot := integer(),  % timestamp
      previous_size := non_neg_integer(),
      time_since_last := integer()}.  % seconds

-record(table_state, {
    name :: table_name(),
    type :: table_type(),
    previous_size = 0 :: non_neg_integer(),
    last_snapshot_time = 0 :: integer(),
    growth_alert_threshold :: alert_threshold() | undefined,
    memory_alert_threshold :: pos_integer() | undefined
}).

-type table_states() :: #{table_name() => #table_state{}}.

-record(state, {
    tables = #{} :: table_states(),
    otl_enabled = false :: boolean(),
    snapshot_interval_ms = 5000 :: pos_integer(),  % 5 seconds
    snapshot_timer :: reference() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Register an ETS table for monitoring.
%%
%% Type must be one of: registry, session, cache, rate_limiter, auth
%%%
%% @end
%%--------------------------------------------------------------------
-spec register_table(table_name(), table_type()) -> ok | {error, term()}.
register_table(Name, Type) ->
    gen_server:call(?MODULE, {register_table, Name, Type}).

%%--------------------------------------------------------------------
%% @doc Unregister an ETS table from monitoring.
%% @end
%%--------------------------------------------------------------------
-spec unregister_table(table_name()) -> ok | {error, not_found}.
unregister_table(Name) ->
    gen_server:call(?MODULE, {unregister_table, Name}).

%%--------------------------------------------------------------------
%% @doc Get current statistics for a monitored table.
%%
%% Returns a snapshot including:
%%% - size: Current number of entries
%%% - memory: Memory usage in bytes
%%% - growth_rate: Entries added/removed per second
%%% - last_snapshot: Timestamp of last measurement
%%%
%% @end
%%--------------------------------------------------------------------
-spec get_table_stats(table_name()) -> {ok, table_snapshot()} | {error, not_found}.
get_table_stats(Name) ->
    gen_server:call(?MODULE, {get_stats, Name}).

%%--------------------------------------------------------------------
%% @doc Get statistics for all monitored tables.
%% @end
%%--------------------------------------------------------------------
-spec get_all_stats() -> {ok, [table_snapshot()]}.
get_all_stats() ->
    gen_server:call(?MODULE, get_all_stats).

%%--------------------------------------------------------------------
%% @doc Set growth alert threshold for a table.
%%
%% Options:
%%% - max_entries: Alert if table exceeds this size
%%% - growth_rate: Alert if growth rate exceeds entries/sec
%%%
%% @end
%%--------------------------------------------------------------------
-spec set_growth_alert_threshold(table_name(), alert_threshold()) -> ok | {error, not_found}.
set_growth_alert_threshold(Name, Threshold) ->
    gen_server:call(?MODULE, {set_growth_threshold, Name, Threshold}).

%%--------------------------------------------------------------------
%% @doc Set memory alert threshold for a table.
%%
%% Alerts if table memory usage exceeds this value (bytes).
%%%
%% @end
%%--------------------------------------------------------------------
-spec set_memory_alert_threshold(table_name(), pos_integer()) -> ok | {error, not_found}.
set_memory_alert_threshold(Name, ThresholdBytes) ->
    gen_server:call(?MODULE, {set_memory_threshold, Name, ThresholdBytes}).

%%--------------------------------------------------------------------
%% @doc Enable OTEL metrics export for ETS statistics.
%% @end
%%--------------------------------------------------------------------
-spec enable_otl_export() -> ok.
enable_otl_export() ->
    gen_server:call(?MODULE, enable_otl_export).

%%--------------------------------------------------------------------
%% @doc Disable OTEL metrics export.
%% @end
%%--------------------------------------------------------------------
-spec disable_otl_export() -> ok.
disable_otl_export() ->
    gen_server:call(?MODULE, disable_otl_export).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state(), {continue, initialize_monitoring}}.
init([]) ->
    ?LOG_INFO("Starting ETS statistics monitor"),
    State = #state{},
    {ok, State, {continue, initialize_monitoring}}.

-spec handle_continue(initialize_monitoring, state()) -> {noreply, state()}.
handle_continue(initialize_monitoring, State) ->
    % Start periodic snapshot timer
    Timer = erlang:send_after(State#state.snapshot_interval_ms, self(), take_snapshots),
    {noreply, State#state{snapshot_timer = Timer}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call({register_table, Name, Type}, _From, State) ->
    case ets:whereis(Name) of
        undefined ->
            {reply, {error, table_not_found}, State};
        _Tid ->
            TableState = #table_state{
                name = Name,
                type = Type,
                previous_size = 0,
                last_snapshot_time = 0,
                growth_alert_threshold = undefined,
                memory_alert_threshold = undefined
            },
            NewTables = maps:put(Name, TableState, State#state.tables),
            ?LOG_INFO("Registered ETS table for monitoring: ~p (type: ~p)", [Name, Type]),
            {reply, ok, State#state{tables = NewTables}}
    end;

handle_call({unregister_table, Name}, _From, State) ->
    case maps:is_key(Name, State#state.tables) of
        false ->
            {reply, {error, not_found}, State};
        true ->
            NewTables = maps:remove(Name, State#state.tables),
            ?LOG_INFO("Unregistered ETS table from monitoring: ~p", [Name]),
            {reply, ok, State#state{tables = NewTables}}
    end;

handle_call({get_stats, Name}, _From, State) ->
    case maps:get(Name, State#state.tables, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TableState ->
            Snapshot = take_snapshot(Name, TableState),
            {reply, {ok, Snapshot}, State}
    end;

handle_call(get_all_stats, _From, State) ->
    Snapshots = maps:fold(fun(Name, TableState, Acc) ->
        Snapshot = take_snapshot(Name, TableState),
        [Snapshot | Acc]
    end, [], State#state.tables),
    {reply, {ok, lists:reverse(Snapshots)}, State};

handle_call({set_growth_threshold, Name, Threshold}, _From, State) ->
    case maps:get(Name, State#state.tables, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TableState ->
            UpdatedTable = TableState#table_state{growth_alert_threshold = Threshold},
            NewTables = maps:put(Name, UpdatedTable, State#state.tables),
            {reply, ok, State#state{tables = NewTables}}
    end;

handle_call({set_memory_threshold, Name, Threshold}, _From, State) ->
    case maps:get(Name, State#state.tables, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        TableState ->
            UpdatedTable = TableState#table_state{memory_alert_threshold = Threshold},
            NewTables = maps:put(Name, UpdatedTable, State#state.tables),
            {reply, ok, State#state{tables = NewTables}}
    end;

handle_call(enable_otl_export, _From, State) ->
    ?LOG_INFO("Enabled OTEL export for ETS statistics"),
    {reply, ok, State#state{otl_enabled = true}};

handle_call(disable_otl_export, _From, State) ->
    ?LOG_INFO("Disabled OTEL export for ETS statistics"),
    {reply, ok, State#state{otl_enabled = false}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(take_snapshots, State) ->
    % Take snapshots for all monitored tables
    NewTables = maps:map(fun(Name, TableState) ->
        Snapshot = take_snapshot(Name, TableState),
        check_alerts(Name, Snapshot, TableState),
        export_otl_metrics(Name, Snapshot, State#state.otl_enabled),
        TableState#table_state{
            previous_size = maps:get(size, Snapshot),
            last_snapshot_time = erlang:system_time(second)
        }
    end, State#state.tables),

    % Schedule next snapshot
    Timer = erlang:send_after(State#state.snapshot_interval_ms, self(), take_snapshots),
    {noreply, State#state{tables = NewTables, snapshot_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Terminating ETS statistics monitor"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Take a snapshot of table statistics
-spec take_snapshot(table_name(), #table_state{}) -> table_snapshot().
take_snapshot(Name, TableState) ->
    Size = ets:info(Name, size),
    Memory = ets:info(Name, memory),
    Now = erlang:system_time(second),

    PreviousSize = TableState#table_state.previous_size,
    LastTime = TableState#table_state.last_snapshot_time,

    % Calculate growth rate (entries/second)
    GrowthRate = case LastTime > 0 of
        true ->
            TimeDelta = Now - LastTime,
            SizeDelta = Size - PreviousSize,
            case TimeDelta > 0 of
                true -> SizeDelta / TimeDelta;
                false -> 0.0
            end;
        false ->
            0.0
    end,

    #{
        name => Name,
        type => TableState#table_state.type,
        size => Size,
        memory => Memory,
        growth_rate => GrowthRate,
        last_snapshot => Now,
        previous_size => PreviousSize,
        time_since_last => Now - LastTime
    }.

%% @private Check if alerts should be triggered
-spec check_alerts(table_name(), table_snapshot(), #table_state{}) -> ok.
check_alerts(Name, Snapshot, TableState) ->
    Size = maps:get(size, Snapshot),
    Memory = maps:get(memory, Snapshot),
    GrowthRate = maps:get(growth_rate, Snapshot),

    % Check growth alert threshold
    case TableState#table_state.growth_alert_threshold of
        undefined ->
            ok;
        Threshold ->
            MaxEntries = maps:get(max_entries, Threshold, infinity),
            MaxGrowthRate = maps:get(max_growth_rate, Threshold, infinity),

            case Size > MaxEntries of
                true ->
                    ?LOG_WARNING("ETS table ~p exceeded size threshold: ~p > ~p",
                        [Name, Size, MaxEntries]);
                false ->
                    ok
            end,

            case GrowthRate > MaxGrowthRate of
                true ->
                    ?LOG_WARNING("ETS table ~p growth rate spike: ~p entries/sec (max: ~p)",
                        [Name, GrowthRate, MaxGrowthRate]);
                false ->
                    ok
            end
    end,

    % Check memory alert threshold
    case TableState#table_state.memory_alert_threshold of
        undefined ->
            ok;
        MaxMemory ->
            case Memory > MaxMemory of
                true ->
                    ?LOG_WARNING("ETS table ~p exceeded memory threshold: ~p bytes > ~p bytes",
                        [Name, Memory, MaxMemory]);
                false ->
                    ok
            end
    end,

    ok.

%% @private Export metrics to OTEL if enabled
-spec export_otl_metrics(table_name(), table_snapshot(), boolean()) -> ok.
export_otl_metrics(_Name, _Snapshot, false) ->
    ok;
export_otl_metrics(Name, Snapshot, true) ->
    try
        Size = maps:get(size, Snapshot),
        Memory = maps:get(memory, Snapshot),
        GrowthRate = maps:get(growth_rate, Snapshot),
        Type = maps:get(type, Snapshot),

        % Create OTEL instrument attributes
        Attributes = #{
            table_name => atom_to_binary(Name, utf8),
            table_type => atom_to_binary(Type, utf8)
        },

        % Record metrics
        otel_meter:record(erlmcp_ets_table_size, Size, Attributes),
        otel_meter:record(erlmcp_ets_table_memory, Memory, Attributes),
        otel_meter:record(erlmcp_ets_table_growth_rate, GrowthRate, Attributes),

        ok
    catch
        Error:Reason:Stack ->
            ?LOG_ERROR("Failed to export OTEL metrics for table ~p: ~p:~p~n~p",
                [Name, Error, Reason, Stack]),
            ok
    end.
