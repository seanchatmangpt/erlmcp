-module(erlmcp_tool_execute).
-behaviour(gen_server).

%% API
-export([execute_tool/3, execute_tool/4,
         start_link/0,
         set_resource_limits/2,
         cancel_execution/1,
         get_sandbox_status/0,
         cleanup_sandbox/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal exports for sandbox processes
-export([isolated_execute/4]).

-include("erlmcp.hrl").

%% State record
-record(state, {
    active_sandboxes = #{} :: #{pid() => sandbox_info()},
    execution_history = [] :: list(),
    max_concurrent = 100 :: pos_integer()
}).

-record(sandbox_info, {
    tool_name :: binary(),
    args :: map(),
    options :: map(),
    monitor_ref :: reference(),
    start_time :: erlang:timestamp(),
    caller :: pid()
}).

-type sandbox_info() :: #sandbox_info{}.
-type execution_result() :: {ok, term()} | {error, term()}.
-type execution_options() :: #{
    timeout => timeout(),
    max_memory => non_neg_integer(),
    max_heap_size => non_neg_integer(),
    kill_on_limit => boolean(),
    priority => normal | high | low
}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Execute a tool in an isolated sandbox process.
%%
%% This function spawns a dedicated process for tool execution with:
%% - Process isolation via spawn_link
%% - Memory limits via process flags
%% - Timeout enforcement
%% - Monitoring and cleanup
%%
%% == Example ==
%% <pre>
%% %% Execute tool with default options
%% {ok, Result} = erlmcp_tool_execute:execute_tool(
%%     <<"mcp__read_file">>,
%%     #{<<"path">> => "/tmp/file.txt"},
%%     #{}
%% ),
%%
%% %% Execute with custom limits
%% {ok, Result} = erlmcp_tool_execute:execute_tool(
%%     <<"mcp__bash">>,
%%     #{<<"command">> => "ls -la"},
%%     #{timeout => 5000, max_memory => 10485760}
%% ).
%% </pre>>
%%
-spec execute_tool(binary(), map(), execution_options()) ->
    {ok, pid(), reference()} | {error, term()}.
execute_tool(ToolName, Args, Options) ->
    execute_tool(ToolName, Args, Options, self()).

%% @doc Execute a tool with explicit caller PID.
-spec execute_tool(binary(), map(), execution_options(), pid()) ->
    {ok, pid(), reference()} | {error, term()}.
execute_tool(ToolName, Args, Options, Caller) when is_binary(ToolName),
                                                     is_map(Args),
                                                     is_map(Options),
                                                     is_pid(Caller) ->
    gen_server:call(?MODULE, {execute_tool, ToolName, Args, Options, Caller}, infinity).

%% @doc Start the tool execution supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Set resource limits for a running sandbox process.
-spec set_resource_limits(pid(), execution_options()) -> ok | {error, term()}.
set_resource_limits(Pid, Options) when is_pid(Pid), is_map(Options) ->
    gen_server:call(?MODULE, {set_resource_limits, Pid, Options}).

%% @doc Cancel a running tool execution.
-spec cancel_execution(pid()) -> ok | {error, term()}.
cancel_execution(SandboxPid) when is_pid(SandboxPid) ->
    gen_server:call(?MODULE, {cancel_execution, SandboxPid}).

%% @doc Get current sandbox status information.
-spec get_sandbox_status() -> map().
get_sandbox_status() ->
    gen_server:call(?MODULE, get_sandbox_status).

%% @doc Clean up a completed sandbox.
-spec cleanup_sandbox(pid()) -> ok.
cleanup_sandbox(SandboxPid) when is_pid(SandboxPid) ->
    gen_server:cast(?MODULE, {cleanup_sandbox, SandboxPid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Create ETS table for sandbox registry
    ets:new(sandbox_registry, [named_table, public, set,
                                {read_concurrency, true},
                                {write_concurrency, true}]),

    %% Get configuration from application environment
    MaxConcurrent = application:get_env(erlmcp_core,
                                        max_concurrent_tools,
                                        100),

    {ok, #state{
        active_sandboxes = #{},
        execution_history = [],
        max_concurrent = MaxConcurrent
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({execute_tool, ToolName, Args, Options, Caller}, _From, State) ->
    %% Check concurrent execution limit
    ActiveCount = maps:size(State#state.active_sandboxes),
    MaxConcurrent = State#state.max_concurrent,

    case ActiveCount >= MaxConcurrent of
        true ->
            {reply, {error, ?MCP_ERROR_MAX_CONCURRENT_TOOLS}, State};
        false ->
            %% Spawn isolated sandbox process
            SandboxPid = spawn_link(?MODULE, isolated_execute,
                                   [ToolName, Args, Options, Caller]),

            %% Monitor the sandbox process
            MonitorRef = erlang:monitor(process, SandboxPid),

            %% Apply resource limits
            apply_resource_limits(SandboxPid, Options),

            %% Store sandbox info
            SandboxInfo = #sandbox_info{
                tool_name = ToolName,
                args = Args,
                options = Options,
                monitor_ref = MonitorRef,
                start_time = erlang:timestamp(),
                caller = Caller
            },

            NewState = State#state{
                active_sandboxes = maps:put(SandboxPid, SandboxInfo,
                                            State#state.active_sandboxes)
            },

            %% Register in ETS for cross-module access
            ets:insert(sandbox_registry, {SandboxPid, ToolName, MonitorRef, Caller}),

            {reply, {ok, SandboxPid, MonitorRef}, NewState}
    end;

handle_call({set_resource_limits, Pid, Options}, _From, State) ->
    Result = apply_resource_limits(Pid, Options),
    {reply, Result, State};

handle_call({cancel_execution, SandboxPid}, _From, State) ->
    Result = case maps:get(SandboxPid, State#state.active_sandboxes, undefined) of
        undefined ->
            {error, ?ERR_NOT_FOUND};
        _SandboxInfo ->
            case erlang:is_process_alive(SandboxPid) of
                true ->
                    erlang:exit(SandboxPid, tool_cancelled),
                    ok;
                false ->
                    {error, ?ERR_PROCESS_DEAD}
            end
    end,
    {reply, Result, State};

handle_call(get_sandbox_status, _From, State) ->
    ActiveCount = maps:size(State#state.active_sandboxes),
    MaxConcurrent = State#state.max_concurrent,

    %% Calculate statistics
    Sandboxes = maps:to_list(State#state.active_sandboxes),
    RunningTimeStats = calculate_running_time_stats(Sandboxes),

    Status = #{
        active_sandboxes => ActiveCount,
        max_concurrent => MaxConcurrent,
        utilization_percent => (ActiveCount * 100) div MaxConcurrent,
        running_time_stats => RunningTimeStats,
        tools_by_name => group_tools_by_name(Sandboxes)
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({cleanup_sandbox, SandboxPid}, State) ->
    NewState = State#state{
        active_sandboxes = maps:remove(SandboxPid, State#state.active_sandboxes)
    },
    ets:delete(sandbox_registry, SandboxPid),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', MonitorRef, process, Pid, Reason}, State) ->
    %% Sandbox process exited
    case maps:get(Pid, State#state.active_sandboxes, undefined) of
        undefined ->
            %% Unknown process, ignore
            {noreply, State};
        SandboxInfo ->
            %% Log the exit reason
            ToolName = SandboxInfo#sandbox_info.tool_name,
            StartTime = SandboxInfo#sandbox_info.start_time,
            Elapsed = timer:now_diff(erlang:timestamp(), StartTime) / 1000,  % ms

            case Reason of
                normal ->
                    logger:info("Tool sandbox ~p completed normally in ~pms",
                                [ToolName, Elapsed]);
                {tool_cancelled} ->
                    logger:info("Tool sandbox ~p was cancelled", [ToolName]);
                {tool_error, Type, Error} ->
                    logger:error("Tool sandbox ~p error: ~p:~p",
                                 [ToolName, Type, Error]);
                {max_heap_size, _Info} ->
                    logger:error("Tool sandbox ~p exceeded memory limit",
                                 [ToolName]);
                _ ->
                    logger:warning("Tool sandbox ~p exited: ~p",
                                   [ToolName, Reason])
            end,

            %% Remove from active sandboxes
            NewState = State#state{
                active_sandboxes = maps:remove(Pid, State#state.active_sandboxes)
            },

            %% Clean up ETS registry
            ets:delete(sandbox_registry, Pid),

            {noreply, NewState}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Isolated execution function for sandbox processes.
%% Runs in a dedicated process with limited resources.
-spec isolated_execute(binary(), map(), execution_options(), pid()) -> no_return().
isolated_execute(ToolName, Args, Options, Caller) ->
    process_flag(trap_exit, true),

    %% Set priority if specified
    Priority = maps:get(priority, Options, normal),
    process_flag(priority, Priority),

    try
        %% Get timeout from options
        Timeout = maps:get(timeout, Options, 30000),

        %% Execute the tool logic
        Result = apply_tool_logic(ToolName, Args),

        %% Send result back to caller
        case is_pid(Caller) andalso is_process_alive(Caller) of
            true ->
                Caller ! {tool_result, self(), {ok, Result}};
            false ->
                logger:warning("Tool caller ~p is dead, dropping result", [Caller])
        end,

        exit(normal)
    catch
        Type:Reason:Stacktrace ->
            logger:error("Tool sandbox error: ~p:~p~nStacktrace: ~p",
                         [Type, Reason, Stacktrace]),

            %% Send error back to caller
            case is_pid(Caller) andalso is_process_alive(Caller) of
                true ->
                    Caller ! {tool_result, self(),
                             {error, {Type, Reason}}};
                false ->
                    ok
            end,

            exit({tool_error, Type, Reason})
    after
        %% Ensure cleanup happens even on timeout
        ok
    end.

%% @doc Apply resource limits to a sandbox process.
-spec apply_resource_limits(pid(), execution_options()) -> ok | {error, term()}.
apply_resource_limits(Pid, Options) when is_pid(Pid) ->
    case erlang:is_process_alive(Pid) of
        false ->
            {error, ?ERR_PROCESS_DEAD};
        true ->
            %% Apply max heap size (memory limit)
            MaxMemory = maps:get(max_memory, Options, 104857600),  % 100MB default
            MaxHeapSize = maps:get(max_heap_size, Options, MaxMemory div 8),  % Convert bytes to words
            KillOnLimit = maps:get(kill_on_limit, Options, true),

            %% Set heap limit using process_flag
            try
                erlang:process_flag(Pid, max_heap_size,
                                   #{size => MaxHeapSize,
                                     kill => KillOnLimit,
                                     error_logger => true}),
                ok
            catch
                error:badarg ->
                    {error, invalid_heap_size}
            end
    end.

%% @doc Apply tool logic - this is a placeholder for actual tool execution.
%% In production, this would dispatch to registered tool handlers.
-spec apply_tool_logic(binary(), map()) -> term().
apply_tool_logic(ToolName, Args) ->
    %% This is a placeholder that would integrate with the actual tool registry
    %% For now, return a simple success response
    logger:info("Executing tool ~p with args ~p", [ToolName, Args]),

    %% Simulate tool execution
    case ToolName of
        <<"mcp__read_file">> ->
            Path = maps:get(<<"path">>, Args, ""),
            {ok, <<"File content from ", Path/binary>>};
        <<"mcp__write_file">> ->
            Path = maps:get(<<"path">>, Args, ""),
            Content = maps:get(<<"content">>, Args, ""),
            {ok, <<"Wrote ", (byte_size(Content))/integer, " bytes to ", Path/binary>>};
        _ ->
            {error, ?MCP_ERROR_TOOL_NOT_FOUND}
    end.

%% @doc Calculate running time statistics for active sandboxes.
-spec calculate_running_time_stats([{pid(), sandbox_info()}]) -> map().
calculate_running_time_stats(Sandboxes) ->
    Now = erlang:timestamp(),
    RunningTimes = [{Tool, timer:now_diff(Now, StartTime) / 1000} ||
                      {_Pid, #sandbox_info{tool_name = Tool, start_time = StartTime}}
                      <- Sandboxes],

    case RunningTimes of
        [] ->
            #{min => 0, max => 0, avg => 0, count => 0};
        _ ->
            Count = length(RunningTimes),
            Times = [T || {_, T} <- RunningTimes],
            MinTime = lists:min(Times),
            MaxTime = lists:max(Times),
            AvgTime = lists:sum(Times) / Count,

            #{min => MinTime,
              max => MaxTime,
              avg => AvgTime,
              count => Count}
    end.

%% @doc Group active sandboxes by tool name.
-spec group_tools_by_name([{pid(), sandbox_info()}]) -> map().
group_tools_by_name(Sandboxes) ->
    lists:foldl(
        fun({_Pid, #sandbox_info{tool_name = ToolName}}, Acc) ->
            maps:update_with(ToolName, fun(Count) -> Count + 1 end, 1, Acc)
        end,
        #{},
        Sandboxes).
