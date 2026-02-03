%% @private
%% Replication Worker - Handles data replication between sites
-module(erlmcp_replication_worker).
-behaviour(gen_server).

%% API
-export([
    start_link/1,
    start_site/2,
    get_status/1,
    add_target/2,
    remove_target/2,
    replicate_data/3
]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(replication_queue, {
    data :: queue(),
    size :: non_neg_integer(),
    max_size :: pos_integer()
}).

-record(replication_state, {
    worker_id :: integer(),
    site_id :: binary(),
    config :: #replication_config{},
    targets :: [binary()],
    queue :: #replication_queue{},
    metrics :: #replication_metrics{},
    timer :: reference() | undefined,
    active_replications :: #{binary() => reference()}
}).

-define(MAX_QUEUE_SIZE, 10000).
-define(REPLICATION_TIMEOUT, 30000).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(integer()) -> {ok, pid()} | {error, term()}.
start_link(WorkerId) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [WorkerId], []).

-spec start_site(binary(), map()) -> ok.
start_site(SiteId, Config) ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            gen_server:call(Pid, {start_site, SiteId, Config});
        undefined ->
            {error, worker_not_running}
    end.

-spec get_status(pid()) -> {ok, map()} | {error, term()}.
get_status(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_status).

-spec add_target(pid(), binary()) -> ok.
add_target(Pid, TargetSiteId) when is_pid(Pid) ->
    gen_server:cast(Pid, {add_target, TargetSiteId}).

-spec remove_target(pid(), binary()) -> ok.
remove_target(Pid, TargetSiteId) when is_pid(Pid) ->
    gen_server:cast(Pid, {remove_target, TargetSiteId}).

-spec replicate_data(pid(), binary(), any()) -> ok.
replicate_data(Pid, Key, Value) when is_pid(Pid) ->
    gen_server:cast(Pid, {replicate_data, Key, Value}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([integer()]) -> {ok, #replication_state{}}.
init([WorkerId]) ->
    %% Initialize replication state
    State = #replication_state{
        worker_id = WorkerId,
        queue = #replication_queue{
            data = queue:new(),
            size = 0,
            max_size = ?MAX_QUEUE_SIZE
        },
        metrics = #replication_metrics{
            total_replicated = 0,
            failed_replications = 0,
            average_lag = 0.0,
            last_replication = undefined,
            queue_size = 0
        },
        active_replications = #{}
    },

    %% Start replication timer
    Timer = erlang:start_timer(1000, self(), process_queue),

    {ok, State#replication_state{timer = Timer}}.

-spec handle_call(term(), {pid(), any()}, #replication_state{}) -> {reply, term(), #replication_state{}}.
handle_call({start_site, SiteId, Config}, _From, State) ->
    %% Parse and validate configuration
    ReplicationConfig = parse_replication_config(Config),

    %% Initialize replication for this site
    case validate_replication_config(ReplicationConfig) of
        true ->
            NewState = State#replication_state{
                site_id = SiteId,
                config = ReplicationConfig,
                targets = ReplicationConfig#replication_config.targets
            },
            {reply, ok, NewState};
        false ->
            {reply, {error, invalid_config}, State}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        worker_id => State#replication_state.worker_id,
        site_id => State#replication_state.site_id,
        targets => State#replication_state.targets,
        queue_size => State#replication_state.queue#replication_queue.size,
        metrics => State#replication_state.metrics,
        active_replications => maps:size(State#replication_state.active_replications)
    },
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #replication_state{}) -> {noreply, #replication_state{}}.
handle_cast({add_target, TargetSiteId}, State) ->
    %% Add new replication target
    NewTargets = case lists:member(TargetSiteId, State#replication_state.targets) of
        true ->
            State#replication_state.targets;
        false ->
            [TargetSiteId | State#replication_state.targets]
    end,

    NewState = State#replication_state{targets = NewTargets},

    %% Log addition
    logger:info("Replication worker ~p added target ~p for site ~p",
                [State#replication_state.worker_id, TargetSiteId, State#replication_state.site_id]),

    {noreply, NewState};

handle_cast({remove_target, TargetSiteId}, State) ->
    %% Remove replication target
    NewTargets = lists:delete(TargetSiteId, State#replication_state.targets),
    NewState = State#replication_state{targets = NewTargets},

    %% Log removal
    logger:info("Replication worker ~p removed target ~p for site ~p",
                [State#replication_state.worker_id, TargetSiteId, State#replication_state.site_id]),

    {noreply, NewState};

handle_cast({replicate_data, Key, Value}, State) ->
    %% Add data to replication queue
    Queue = State#replication_state.queue,
    NewSize = Queue#replication_queue.size + 1,

    if
        NewSize =< Queue#replication_queue.max_size ->
            %% Add to queue
            NewData = queue:in({Key, Value, erlang:system_time(millisecond)}, Queue#replication_queue.data),
            NewQueue = Queue#replication_queue{data = NewData, size = NewSize},
            NewState = State#replication_state{queue = NewQueue},

            %% Check if we need to process immediately
            case process_queue_immediate(NewState) of
                true ->
                    %% Process queue immediately
                    {noreply, process_replication_queue(NewState)};
                false ->
                    {noreply, NewState}
            end;
        true ->
            %% Queue full - log warning
            logger:warning("Replication queue full for worker ~p", [State#replication_state.worker_id]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #replication_state{}) -> {noreply, #replication_state{}}.
handle_info({timeout, Timer, process_queue}, State) ->
    %% Process replication queue
    case Timer =:= State#replication_state.timer of
        true ->
            {noreply, process_replication_queue(State)};
        false ->
            %% Timer expired - ignore
            {noreply, State}
    end;

handle_info({replication_result, TargetSiteId, Key, Result}, State) ->
    %% Handle replication result
    ActiveReps = State#replication_state.active_replications,

    case maps:find(TargetSiteId, ActiveReps) of
        {ok, Ref} ->
            %% Remove from active replications
            NewActiveReps = maps:remove(TargetSiteId, ActiveReps),

            %% Update metrics
            Metrics = State#replication_state.metrics,
            case Result of
                ok ->
                    %% Successful replication
                    NewMetrics = Metrics#replication_metrics{
                        total_replicated = Metrics#replication_metrics.total_replicated + 1,
                        last_replication = erlang:system_time(millisecond),
                        failed_replications = Metrics#replication_metrics.failed_replications
                    };
                {error, _} ->
                    FailedReps = Metrics#replication_metrics.failed_replications + 1,
                    NewMetrics = Metrics#replication_metrics{
                        total_replicated = Metrics#replication_metrics.total_replicated,
                        failed_replications = FailedReps,
                        last_replication = erlang:system_time(millisecond)
                    }
            end,

            NewState = State#replication_state{
                active_replications = NewActiveReps,
                metrics = NewMetrics
            },

            %% Check if all replications are complete
            case maps:size(NewActiveReps) =:= 0 of
                true ->
                    %% Schedule next batch
                    NewTimer = erlang:start_timer(1000, self(), process_queue),
                    {noreply, NewState#replication_state{timer = NewTimer}};
                false ->
                    {noreply, NewState}
            end;
        error ->
            %% No active replication found
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #replication_state{}) -> ok.
terminate(Reason, State) ->
    logger:warning("Replication worker ~p terminating: ~p",
                   [State#replication_state.worker_id, Reason]),

    %% Cancel timer
    case State#replication_state.timer of
        Timer when is_reference(Timer) ->
            erlang:cancel_timer(Timer);
        undefined ->
            ok
    end,

    %% Flush remaining queue
    Queue = State#replication_state.queue,
    QueueData = Queue#replication_queue.data,
    case queue:len(QueueData) > 0 of
        true ->
            logger:warning("Replication worker ~p has ~p items in queue on termination",
                          [State#replication_state.worker_id, queue:len(QueueData)]);
        false ->
            ok
    end,

    ok.

-spec code_change(term(), #replication_state{}, term()) -> {ok, #replication_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

parse_replication_config(Config) ->
    %% Parse replication configuration from map
    ReplicationConfig = #replication_config{
        mode = maps:get(mode, Config, async),
        compression = maps:get(compression, Config, true),
        encryption = maps:get(encryption, Config, true),
        batch_size = maps:get(batch_size, Config, 100),
        interval = maps:get(interval, Config, 1000),
        targets = maps:get(targets, Config, []),
        bandwidth_limit = maps:get(bandwidth_limit, Config, infinity)
    }.

validate_replication_config(#replication_config{} = Config) ->
    %% Validate replication configuration
    case Config#replication_config.mode of
        sync ->
            %% Sync mode requires smaller batch size
            Config#replication_config.batch_size =< 100;
        async ->
            %% Async mode can handle larger batches
            Config#replication_config.batch_size =< 1000;
        hybrid ->
            %% Hybrid mode - validate both sync and async limits
            Config#replication_config.batch_size =< 500
    end.

process_queue_immediate(State) ->
    %% Check if we should process queue immediately
    Queue = State#replication_state.queue,
    Config = State#replication_state.config,
    QueueSize = Queue#replication_queue.size,

    case Config#replication_config.mode of
        sync ->
            %% Always process sync mode immediately
            true;
        async ->
            %% Process if batch size reached
            QueueSize >= Config#replication_config.batch_size;
        hybrid ->
            %% Process if batch size reached or if queue is large
            QueueSize >= Config#replication_config.batch_size orelse QueueSize >= 500
    end.

process_replication_queue(State) ->
    %% Process replication queue
    Queue = State#replication_state.queue,
    Config = State#replication_state.config,
    QueueData = Queue#replication_queue.data,

    case queue:len(QueueData) of
        0 ->
            %% Queue is empty - schedule next processing
            Timer = erlang:start_timer(1000, self(), process_queue),
            State#replication_state{timer = Timer};
        _ ->
            %% Process batch
            BatchSize = min(Config#replication_config.batch_size, queue:len(QueueData)),
            {Batch, RemainingQueue} = queue:split(BatchSize, QueueData),

            %% Process batch to targets
            NewState = process_batch_to_targets(State, Batch, RemainingQueue),

            %% Schedule next processing
            Timer = erlang:start_timer(1000, self(), process_queue),
            NewState#replication_state{timer = Timer}
    end.

process_batch_to_targets(State, Batch, RemainingQueue) ->
    %% Process batch data to all targets
    Targets = State#replication_state.targets,
    Config = State#replication_state.config,

    %% Update queue state
    NewQueue = State#replication_state.queue#replication_queue{
        data = RemainingQueue,
        size = queue:len(RemainingQueue)
    },

    %% For each item in batch, send to all targets
    BatchList = queue:to_list(Batch),
    lists:foreach(fun({Key, Value, Timestamp}) ->
        %% Prepare data based on configuration
        ProcessedValue = process_data_value(Value, Config),
        ReplicationData = #{
            key => Key,
            value => ProcessedValue,
            timestamp => Timestamp,
            site_id => State#replication_state.site_id,
            worker_id => State#replication_state.worker_id
        },

        %% Send to all targets
        lists:foreach(fun(TargetSiteId) ->
            send_to_target(State, TargetSiteId, ReplicationData)
        end, Targets)
    end, BatchList),

    State#replication_state{queue = NewQueue}.

process_data_value(Value, Config) ->
    %% Process data value based on configuration
    case Config#replication_config.compression of
        true ->
            %% Compress data
            erlmcp_data_compression:compress(Value);
        false ->
            Value
    end,

    case Config#replication_config.encryption of
        true ->
            %% Encrypt data
            erlmcp_data_encryption:encrypt(Value);
        false ->
            Value
    end.

send_to_target(State, TargetSiteId, Data) ->
    %% Send data to target site
    try
        %% Get target site manager
        TargetPid = case erlmcp_global_directory:find_site_manager(TargetSiteId) of
            {ok, Pid} -> Pid;
            {error, not_found} -> error(not_found)
        end,

        %% Send replication data
        erlmcp_site_manager:replicate_data(TargetSiteId, maps:get(key, Data), maps:get(value, Data)),

        %% Track active replication
        Ref = make_ref(),
        ActiveReps = maps:put(TargetSiteId, Ref, State#replication_state.active_replications),
        NewState = State#replication_state{active_replications = ActiveReps},

        %% Set timeout for replication
        erlang:send_after(?REPLICATION_TIMEOUT, self(),
                         {replication_timeout, TargetSiteId, Ref}),

        {ok, NewState}
    catch
        Error:Reason ->
            %% Log error and update metrics
            logger:error("Failed to send data to target ~p: ~p:~p", [TargetSiteId, Error, Reason]),
            State
    end.