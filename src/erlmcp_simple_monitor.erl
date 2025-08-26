%% @doc Simple system monitor gen_server
%% Monitors process count, memory usage, and message queue lengths
-module(erlmcp_simple_monitor).
-behaviour(gen_server).

%% API
-export([start_link/0, get_status/0, set_threshold/2, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(CHECK_INTERVAL, 5000). % 5 seconds

-record(state, {
    process_threshold = 1000,
    memory_threshold = 80,  % percent
    queue_threshold = 100,
    timer_ref
}).

-record(status, {
    process_count,
    memory_percent,
    max_queue_length,
    warnings = [],
    timestamp
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the monitor server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get current system status
-spec get_status() -> {ok, #status{}} | {error, term()}.
get_status() ->
    gen_server:call(?SERVER, get_status).

%% @doc Set warning threshold
-spec set_threshold(process_count | memory_percent | queue_length, non_neg_integer()) -> ok.
set_threshold(Type, Value) ->
    gen_server:cast(?SERVER, {set_threshold, Type, Value}).

%% @doc Stop the monitor server
-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
init([]) ->
    error_logger:info_msg("System monitor starting~n"),
    TimerRef = erlang:send_after(?CHECK_INTERVAL, self(), check_system),
    {ok, #state{timer_ref = TimerRef}}.

%% @private
handle_call(get_status, _From, State) ->
    Status = collect_system_status(State),
    {reply, {ok, Status}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @private
handle_cast({set_threshold, process_count, Value}, State) ->
    error_logger:info_msg("Setting process threshold to ~p~n", [Value]),
    {noreply, State#state{process_threshold = Value}};

handle_cast({set_threshold, memory_percent, Value}, State) ->
    error_logger:info_msg("Setting memory threshold to ~p%~n", [Value]),
    {noreply, State#state{memory_threshold = Value}};

handle_cast({set_threshold, queue_length, Value}, State) ->
    error_logger:info_msg("Setting queue length threshold to ~p~n", [Value]),
    {noreply, State#state{queue_threshold = Value}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(check_system, State) ->
    Status = collect_system_status(State),
    check_thresholds(Status, State),
    
    % Schedule next check
    TimerRef = erlang:send_after(?CHECK_INTERVAL, self(), check_system),
    {noreply, State#state{timer_ref = TimerRef}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, #state{timer_ref = TimerRef}) ->
    erlang:cancel_timer(TimerRef),
    error_logger:info_msg("System monitor stopping~n"),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
collect_system_status(State) ->
    ProcessCount = erlang:system_info(process_count),
    MemoryTotal = erlang:memory(total),
    MemorySystem = erlang:memory(system),
    MemoryPercent = round((MemorySystem / MemoryTotal) * 100),
    
    % Get maximum message queue length
    MaxQueueLength = get_max_queue_length(),
    
    % Check for warnings
    Warnings = check_warnings(ProcessCount, MemoryPercent, MaxQueueLength, State),
    
    #status{
        process_count = ProcessCount,
        memory_percent = MemoryPercent,
        max_queue_length = MaxQueueLength,
        warnings = Warnings,
        timestamp = erlang:timestamp()
    }.

%% @private
get_max_queue_length() ->
    Processes = erlang:processes(),
    lists:foldl(fun(Pid, MaxLen) ->
        case erlang:process_info(Pid, message_queue_len) of
            {message_queue_len, Len} -> max(Len, MaxLen);
            undefined -> MaxLen
        end
    end, 0, Processes).

%% @private
check_warnings(ProcessCount, MemoryPercent, MaxQueueLength, State) ->
    Warnings = [],
    
    % Check process count
    Warnings1 = case ProcessCount > State#state.process_threshold of
        true -> 
            [process_count_high | Warnings];
        false -> 
            Warnings
    end,
    
    % Check memory usage
    Warnings2 = case MemoryPercent > State#state.memory_threshold of
        true -> 
            [memory_usage_high | Warnings1];
        false -> 
            Warnings1
    end,
    
    % Check queue lengths
    case MaxQueueLength > State#state.queue_threshold of
        true -> 
            [message_queue_long | Warnings2];
        false -> 
            Warnings2
    end.

%% @private
check_thresholds(Status, State) ->
    case Status#status.warnings of
        [] -> 
            ok; % No warnings
        Warnings ->
            error_logger:warning_msg(
                "System monitor warnings: ~p~n"
                "Process count: ~p (threshold: ~p)~n"
                "Memory usage: ~p% (threshold: ~p%)~n"
                "Max queue length: ~p (threshold: ~p)~n",
                [Warnings,
                 Status#status.process_count, State#state.process_threshold,
                 Status#status.memory_percent, State#state.memory_threshold,
                 Status#status.max_queue_length, State#state.queue_threshold])
    end.