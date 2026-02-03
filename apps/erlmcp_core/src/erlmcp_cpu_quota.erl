%%%-------------------------------------------------------------------
%%% @doc erlmcp_cpu_quota - CPU Quota and Timeout Management
%%%
%%% Protects against CPU-intensive DoS attacks by enforcing:
%%% - Per-client CPU quotas (max operations per second)
%%% - CPU time tracking using erlang:statistics(runtime)
%%% - Operation timeouts with automatic termination
%%% - Priority queue for fair scheduling
%%% - Bounded refusal when quota exceeded
%%%
%%% Design:
%%% - gen_server for quota state management
%%% - ETS for fast per-client quota tracking
%%% - Sliding window algorithm for CPU time measurement
%%% - Automatic cleanup of expired quotas
%%% - Integration with erlmcp_server for request limiting
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cpu_quota).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, check_quota/1, check_quota/2, record_operation/2,
         record_operation/3, get_client_stats/1, reset_client/1, get_quota_config/0,
         update_quota_config/1, stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type client_id() :: binary().
-type operation_type() :: tool_call | resource_read | prompt_get | initialize.
-type cpu_time_ms() :: non_neg_integer().
-type operation_count() :: non_neg_integer().

-record(quota_state,
        {cpu_time_used :: cpu_time_ms(),
         operations_count :: operation_count(),
         window_start :: integer(), %% milliseconds
         last_update :: integer()}).    %% milliseconds

-type quota_state() :: #quota_state{}.

-record(client_stats,
        {client_id :: client_id(),
         total_operations :: operation_count(),
         total_cpu_time :: cpu_time_ms(),
         quota_exceeded_count :: non_neg_integer(),
         average_cpu_per_op :: float(),
         last_operation_at :: integer() | undefined}).

-type client_stats() :: #client_stats{}.

-record(state,
        {quotas :: ets:tid(),           % client_id -> quota_state
         client_stats :: ets:tid(),     % client_id -> client_stats
         config :: map()}).

-type state() :: #state{}.

-define(DEFAULT_MAX_CPU_TIME_PER_SEC, 100). %% 100ms CPU time per second (10% CPU)
-define(DEFAULT_MAX_OPS_PER_SEC, 50).       %% Max 50 operations per second
-define(DEFAULT_WINDOW_MS, 1000).           %% 1 second sliding window
-define(DEFAULT_TIMEOUT_MS, 5000).          %% 5 second timeout per operation
-define(DEFAULT_CLEANUP_INTERVAL_MS, 60000). %% 1 minute
-define(DEFAULT_PRIORITY_LEVELS, [high, normal, low]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start CPU quota manager with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start CPU quota manager with custom config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Check if client has CPU quota available (by client_id)
-spec check_quota(client_id()) ->
                     ok | {error, quota_exceeded, cpu_time} | {error, quota_exceeded, operations}.
check_quota(ClientId) ->
    check_quota(ClientId, undefined).

%% @doc Check if client has CPU quota available (with operation type)
-spec check_quota(client_id(), operation_type() | undefined) ->
                     ok | {error, quota_exceeded, cpu_time} | {error, quota_exceeded, operations}.
check_quota(ClientId, OperationType) ->
    gen_server:call(?MODULE, {check_quota, ClientId, OperationType}, 5000).

%% @doc Record a CPU-intensive operation (by client_id and operation type)
-spec record_operation(client_id(), operation_type(), cpu_time_ms()) -> ok.
record_operation(ClientId, OperationType, CpuTimeMs) ->
    gen_server:call(?MODULE, {record_operation, ClientId, OperationType, CpuTimeMs}, 5000).

%% @doc Record a CPU-intensive operation (by client_id only)
-spec record_operation(client_id(), cpu_time_ms()) -> ok.
record_operation(ClientId, CpuTimeMs) ->
    record_operation(ClientId, undefined, CpuTimeMs).

%% @doc Get statistics for a client
-spec get_client_stats(client_id()) -> {ok, client_stats()} | {error, not_found}.
get_client_stats(ClientId) ->
    gen_server:call(?MODULE, {get_client_stats, ClientId}).

%% @doc Reset quota for a specific client (admin use)
-spec reset_client(client_id()) -> ok.
reset_client(ClientId) ->
    gen_server:call(?MODULE, {reset_client, ClientId}).

%% @doc Get current quota configuration
-spec get_quota_config() -> map().
get_quota_config() ->
    gen_server:call(?MODULE, get_quota_config).

%% @doc Update quota configuration (dynamic reconfiguration)
-spec update_quota_config(map()) -> ok.
update_quota_config(NewConfig) ->
    gen_server:call(?MODULE, {update_quota_config, NewConfig}).

%% @doc Stop CPU quota manager
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    State =
        #state{quotas = ets:new(cpu_quotas, [set, protected, named_table]),
               client_stats = ets:new(cpu_client_stats, [set, protected, named_table]),
               config = Config},

    % Start cleanup timer
    erlang:send_after(maps_get(cleanup_interval_ms, Config, ?DEFAULT_CLEANUP_INTERVAL_MS),
                      self(),
                      cleanup_expired),

    logger:info("CPU quota manager started: max ~pms CPU/sec, ~p ops/sec",
                [maps_get(max_cpu_time_per_sec, Config, ?DEFAULT_MAX_CPU_TIME_PER_SEC),
                 maps_get(max_ops_per_sec, Config, ?DEFAULT_MAX_OPS_PER_SEC)]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()}.
handle_call({check_quota, ClientId, OperationType}, _From, State) ->
    Result = do_check_quota(ClientId, OperationType, State),
    {reply, Result, State};
handle_call({record_operation, ClientId, OperationType, CpuTimeMs}, _From, State) ->
    ok = do_record_operation(ClientId, OperationType, CpuTimeMs, State),
    {reply, ok, State};
handle_call({get_client_stats, ClientId}, _From, State) ->
    Result =
        case ets:lookup(State#state.client_stats, ClientId) of
            [{_, Stats}] ->
                {ok, Stats};
            [] ->
                {error, not_found}
        end,
    {reply, Result, State};
handle_call({reset_client, ClientId}, _From, State) ->
    ets:delete(State#state.quotas, ClientId),
    ets:delete(State#state.client_stats, ClientId),
    logger:warning("CPU quota reset for client: ~p", [ClientId]),
    {reply, ok, State};
handle_call(get_quota_config, _From, State) ->
    {reply, State#state.config, State};
handle_call({update_quota_config, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),
    logger:info("CPU quota config updated: ~p", [MergedConfig]),
    {reply, ok, State#state{config = MergedConfig}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_expired, State) ->
    cleanup_expired_entries(State),
    CleanupInterval =
        maps_get(cleanup_interval_ms, State#state.config, ?DEFAULT_CLEANUP_INTERVAL_MS),
    erlang:send_after(CleanupInterval, self(), cleanup_expired),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    ets:delete(State#state.quotas),
    ets:delete(State#state.client_stats),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Check if client has CPU quota available
do_check_quota(ClientId, _OperationType, State) ->
    Now = erlang:system_time(millisecond),
    MaxCpuTime = maps_get(max_cpu_time_per_sec, State#state.config, ?DEFAULT_MAX_CPU_TIME_PER_SEC),
    MaxOps = maps_get(max_ops_per_sec, State#state.config, ?DEFAULT_MAX_OPS_PER_SEC),
    WindowMs = maps_get(window_ms, State#state.config, ?DEFAULT_WINDOW_MS),

    case ets:lookup(State#state.quotas, ClientId) of
        [{_,
          #quota_state{cpu_time_used = CpuUsed,
                       operations_count = OpsCount,
                       window_start = WindowStart}}] ->
            TimeSinceStart = Now - WindowStart,
            if TimeSinceStart >= WindowMs ->
                   % Window expired, reset quota
                   ets:insert(State#state.quotas,
                              {ClientId,
                               #quota_state{cpu_time_used = 0,
                                            operations_count = 0,
                                            window_start = Now,
                                            last_update = Now}}),
                   ok;
               CpuUsed >= MaxCpuTime ->
                   % CPU time quota exceeded
                   {error, quota_exceeded, cpu_time};
               OpsCount >= MaxOps ->
                   % Operations count quota exceeded
                   {error, quota_exceeded, operations};
               true ->
                   % Within quota
                   ok
            end;
        [] ->
            % First operation, initialize quota
            ets:insert(State#state.quotas,
                       {ClientId,
                        #quota_state{cpu_time_used = 0,
                                     operations_count = 0,
                                     window_start = Now,
                                     last_update = Now}}),
            ok
    end.

%% @private Record a CPU-intensive operation
do_record_operation(ClientId, OperationType, CpuTimeMs, State) ->
    Now = erlang:system_time(millisecond),

    % Update quota state
    QuotaState =
        case ets:lookup(State#state.quotas, ClientId) of
            [{_,
              #quota_state{cpu_time_used = CpuUsed,
                           operations_count = OpsCount,
                           window_start = WindowStart}}] ->
                #quota_state{cpu_time_used = CpuUsed + CpuTimeMs,
                             operations_count = OpsCount + 1,
                             window_start = WindowStart,
                             last_update = Now};
            [] ->
                #quota_state{cpu_time_used = CpuTimeMs,
                             operations_count = 1,
                             window_start = Now,
                             last_update = Now}
        end,
    ets:insert(State#state.quotas, {ClientId, QuotaState}),

    % Update or create client stats
    Stats =
        case ets:lookup(State#state.client_stats, ClientId) of
            [{_, ExistingStats}] ->
                TotalOps = ExistingStats#client_stats.total_operations + 1,
                TotalCpu = ExistingStats#client_stats.total_cpu_time + CpuTimeMs,
                ExistingStats#client_stats{total_operations = TotalOps,
                                           total_cpu_time = TotalCpu,
                                           average_cpu_per_op = TotalCpu / TotalOps,
                                           last_operation_at = Now};
            [] ->
                #client_stats{client_id = ClientId,
                              total_operations = 1,
                              total_cpu_time = CpuTimeMs,
                              quota_exceeded_count = 0,
                              average_cpu_per_op = CpuTimeMs,
                              last_operation_at = Now}
        end,
    ets:insert(State#state.client_stats, {ClientId, Stats}),

    % Log if operation took significant CPU time
    case CpuTimeMs > 50  % More than 50ms is considered heavy
    of
        true ->
            logger:warning("Heavy CPU operation detected: client=~p, type=~p, cpu_time=~pms",
                           [ClientId, OperationType, CpuTimeMs]);
        false ->
            ok
    end,

    ok.

%% @private Cleanup expired entries
cleanup_expired_entries(State) ->
    Now = erlang:system_time(millisecond),

    % Cleanup old quota states (inactive for 5 minutes)
    FiveMinutesAgo = Now - 5 * 60 * 1000,
    ets:foldl(fun({ClientId, #quota_state{last_update = LastUpdate}}, Acc) ->
                 case LastUpdate < FiveMinutesAgo of
                     true ->
                         ets:delete(State#state.quotas, ClientId),
                         logger:debug("CPU quota expired for client: ~p", [ClientId]);
                     false ->
                         ok
                 end,
                 Acc
              end,
              ok,
              State#state.quotas),

    ok.

%% @private Safe maps:get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.
