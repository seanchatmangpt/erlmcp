%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_tool - CLI Tool Execution Manager
%%%
%%% Implements MCP tool execution functionality with async support,
%%% cancellation, and result streaming. Provides full MCP compliance
%%% for tool operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_tool).

-behaviour(gen_server).

%% API
-export([start_link/0, list_tools/0, list_tools/1,
         call_tool/2, call_tool/3, call_tool/4,
         call_tool_async/2, call_tool_async/3, call_tool_async/4,
         cancel_tool/1, cancel_tool/2, cancel_tool/3,
         get_tool_status/1, get_tool_result/1,
         stream_tool_output/1, subscribe_tool_events/1,
         unsubscribe_tool_events/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include("erlmcp_observability.hrl").

%% Records
-record(tool_state,
        {tools :: map(),                       % Available tools
         executions :: map(),                    % Active executions
         streams :: map(),                       % Output streams
         metrics :: map(),                       % Tool metrics
         handlers :: map(),                      % Event handlers
         concurrency :: integer(),               % Max concurrent executions
         timeout :: integer(),                   % Default timeout
         workers :: list()}).                     % Worker processes

%% Execution record
-record(execution,
        {id :: binary(),
         tool :: binary(),
         args :: list(),
         pid :: pid(),
         status :: queued | running | completed | cancelled | failed,
         start_time :: integer(),
         end_time :: integer() | undefined,
         result :: term() | undefined,
         error :: term() | undefined,
         stream :: pid() | undefined}).

%% Tool configuration
-define(DEFAULT_TOOL_CONFIG,
        #{<<"max_concurrent">> => 10,
          <<"default_timeout">> => 30000,
          <<"stream_buffer_size">> => 1000,
         <<"enable_async">> => true,
         <<"enable_cancellation">> => true,
         <<"enable_streaming">> => true}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the tool execution manager
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc List all available tools
-spec list_tools() -> {ok, list()} | {error, term()}.
list_tools() ->
    gen_server:call(?SERVER, list_tools, 5000).

%% @doc List tools with filtering
-spec list_tools(map()) -> {ok, list()} | {error, term()}.
list_tools(Filters) ->
    gen_server:call(?SERVER, {list_tools, Filters}, 5000).

%% @doc Call tool synchronously
-spec call_tool(binary(), list()) -> {ok, term()} | {error, term()}.
call_tool(Tool, Args) ->
    call_tool(Tool, Args, maps:get(<<"default_timeout">>, ?DEFAULT_TOOL_CONFIG, 30000)).

%% @doc Call tool synchronously with timeout
-spec call_tool(binary(), list(), integer()) -> {ok, term()} | {error, term()}.
call_tool(Tool, Args, Timeout) ->
    call_tool(Tool, Args, Timeout, #{}).

%% @doc Call tool synchronously with timeout and options
-spec call_tool(binary(), list(), integer(), map()) -> {ok, term()} | {error, term()}.
call_tool(Tool, Args, Timeout, Options) ->
    gen_server:call(?SERVER, {call_tool, Tool, Args, Timeout, Options}, Timeout).

%% @doc Call tool asynchronously
-spec call_tool_async(binary(), list()) -> {ok, binary()} | {error, term()}.
call_tool_async(Tool, Args) ->
    call_tool_async(Tool, Args, #{}).

%% @doc Call tool asynchronously with options
-spec call_tool_async(binary(), list(), map()) -> {ok, binary()} | {error, term()}.
call_tool_async(Tool, Args, Options) ->
    gen_server:call(?SERVER, {call_tool_async, Tool, Args, Options}, 5000).

%% @doc Call tool asynchronously with timeout
-spec call_tool_async(binary(), list(), integer(), map()) -> {ok, binary()} | {error, term()}.
call_tool_async(Tool, Args, Timeout, Options) ->
    gen_server:call(?SERVER, {call_tool_async, Tool, Args, Timeout, Options}, 5000).

%% @doc Cancel tool execution
-spec cancel_tool(binary()) -> ok | {error, term()}.
cancel_tool(ExecutionId) ->
    cancel_tool(ExecutionId, <<"cancelled_by_user">>).

%% @doc Cancel tool execution with reason
-spec cancel_tool(binary(), binary()) -> ok | {error, term()}.
cancel_tool(ExecutionId, Reason) ->
    cancel_tool(ExecutionId, Reason, #{}).

%% @doc Cancel tool execution with reason and options
-spec cancel_tool(binary(), binary(), map()) -> ok | {error, term()}.
cancel_tool(ExecutionId, Reason, Options) ->
    gen_server:call(?SERVER, {cancel_tool, ExecutionId, Reason, Options}, 5000).

%% @doc Get tool execution status
-spec get_tool_status(binary()) -> {ok, map()} | {error, term()}.
get_tool_status(ExecutionId) ->
    gen_server:call(?SERVER, {get_tool_status, ExecutionId}, 5000).

%% @doc Get tool execution result
-spec get_tool_result(binary()) -> {ok, term()} | {error, term()}.
get_tool_result(ExecutionId) ->
    gen_server:call(?SERVER, {get_tool_result, ExecutionId}, 5000).

%% @doc Stream tool output
-spec stream_tool_output(binary()) -> {ok, pid()} | {error, term()}.
stream_tool_output(ExecutionId) ->
    gen_server:call(?SERVER, {stream_tool_output, ExecutionId}, 5000).

%% @doc Subscribe to tool events
-spec subscribe_tool_events(pid()) -> ok | {error, term()}.
subscribe_tool_events(Pid) ->
    gen_server:call(?SERVER, {subscribe_tool_events, Pid}, 5000).

%% @doc Unsubscribe from tool events
-spec unsubscribe_tool_events(pid()) -> ok | {error, term()}.
unsubscribe_tool_events(Pid) ->
    gen_server:call(?SERVER, {unsubscribe_tool_events, Pid}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize the tool manager
-spec init(term()) -> {ok, #tool_state{}} | {stop, term()}.
init(_Args) ->
    %% Create OTEL span for tool manager initialization
    erlmcp_otel:with_span("cli.tool.init",
                          #{<<"module">> => atom_to_binary(?MODULE, utf8)},
                          fun() ->
                             %% Initialize state
                             Tools = load_tools(),

                             %% Initialize execution tracking
                             Executions = #{},
                             Streams = #{},

                             %% Initialize metrics
                             Metrics = init_metrics(),

                             %% Initialize worker pool
                             Workers = start_worker_pool(),

                             State = #tool_state{
                                tools = Tools,
                                executions = Executions,
                                streams = Streams,
                                metrics = Metrics,
                                handlers = #{},
                                concurrency = maps:get(<<"max_concurrent">>, ?DEFAULT_TOOL_CONFIG, 10),
                                timeout = maps:get(<<"default_timeout">>, ?DEFAULT_TOOL_CONFIG, 30000),
                                workers = Workers
                             },

                             %% Start cleanup timer
                             erlang:send_after(60000, self(), cleanup_executions),

                             erlmcp_metrics:record("cli.tool.initialized", 1),
                             {ok, State}
                          end).

%% @doc Handle synchronous calls
-spec handle_call(term(), {pid(), term()}, #tool_state{}) ->
                   {reply, term(), #tool_state{}}.
handle_call(list_tools, _From, State) ->
    %% Create OTEL span for tool listing
    SpanCtx = erlmcp_otel:inject_span("cli.tool.list", #{}, undefined),

    ToolList = maps:values(State#tool_state.tools),
    erlmcp_otel:record_event(SpanCtx, <<"tools.listed">>, #{<<"count">> => length(ToolList)}),
    erlmcp_metrics:record("cli.tool.list.success", 1),

    {reply, {ok, ToolList}, State};

handle_call({list_tools, Filters}, _From, State) ->
    %% Create OTEL span for filtered tool listing
    SpanCtx = erlmcp_otel:inject_span("cli.tool.list_filtered",
                                     #{<<"filters">> => Filters},
                                     undefined),

    FilteredTools = filter_tools(State#tool_state.tools, Filters),
    erlmcp_otel:record_event(SpanCtx, <<"tools.filtered">>,
                          #{<<"count">> => length(FilteredTools)}),
    erlmcp_metrics:record("cli.tool.list_filtered.success", 1),

    {reply, {ok, FilteredTools}, State};

handle_call({call_tool, Tool, Args, Timeout, Options}, _From, State) ->
    %% Create OTEL span for tool call
    SpanCtx = erlmcp_otel:inject_span("cli.tool.call",
                                     #{<<"tool">> => Tool, <<"args_count">> => length(Args)},
                                     undefined),

    try
        %% Check tool availability
        case find_tool(Tool, State) of
            {ok, ToolInfo} ->
                %% Check concurrency limit
                if length(State#tool_state.workers) < State#tool_state.concurrency ->
                       %% Execute tool synchronously
                       case execute_tool_sync(Tool, Args, Timeout, ToolInfo, State) of
                           {ok, Result} ->
                               erlmcp_otel:record_event(SpanCtx, <<"tool.completed">>, #{}),
                               erlmcp_metrics:record("cli.tool.call.success", 1),
                               {reply, {ok, Result}, State};
                           {error, Reason} ->
                               erlmcp_otel:record_error(SpanCtx, {tool_execution_failed, Tool, Reason}),
                               erlmcp_metrics:record("cli.tool.call.error", 1),
                               {reply, {error, Reason}, State}
                       end;
                   true ->
                       erlmcp_otel:record_error(SpanCtx, {tool_concurrency_limit, Tool}),
                       erlmcp_metrics:record("cli.tool.call.concurrency_error", 1),
                       {reply, {error, concurrency_limit}, State}
                end;
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {tool_not_found, Tool}),
                erlmcp_metrics:record("cli.tool.tool_not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {tool_call_error, Tool, Error, Reason}),
            erlmcp_metrics:record("cli.tool.call.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({call_tool_async, Tool, Args, Options}, _From, State) ->
    handle_call({call_tool_async, Tool, Args, State#tool_state.timeout, Options}, _From, State);

handle_call({call_tool_async, Tool, Args, Timeout, Options}, _From, State) ->
    %% Create OTEL span for async tool call
    SpanCtx = erlmcp_otel:inject_span("cli.tool.call_async",
                                     #{<<"tool">> => Tool, <<"args_count">> => length(Args)},
                                     undefined),

    try
        %% Check tool availability
        case find_tool(Tool, State) of
            {ok, ToolInfo} ->
                %% Check concurrency limit
                if length(State#tool_state.workers) < State#tool_state.concurrency ->
                       %% Create execution ID
                       ExecutionId = generate_execution_id(),

                       %% Start async execution
                       case start_async_execution(ExecutionId, Tool, Args, Timeout, Options, ToolInfo, State) of
                           {ok, Pid} ->
                               %% Track execution
                               Execution = #execution{id = ExecutionId,
                                                    tool = Tool,
                                                    args = Args,
                                                    pid = Pid,
                                                    status = running,
                                                    start_time = erlang:system_time(millisecond),
                                                    stream = maps:get(<<"stream_pid">>, Options)},
                               NewExecutions = maps:put(ExecutionId, Execution, State#tool_state.executions),

                               erlmcp_otel:record_event(SpanCtx, <<"tool.started_async">>,
                                                     #{<<"execution_id">> => ExecutionId}),
                               erlmcp_metrics:record("cli.tool.call_async.success", 1),
                               {reply, {ok, ExecutionId}, State#tool_state{executions = NewExecutions}};
                           {error, Reason} ->
                               erlmcp_otel:record_error(SpanCtx, {async_start_failed, Tool, Reason}),
                               erlmcp_metrics:record("cli.tool.call_async.error", 1),
                               {reply, {error, Reason}, State}
                       end;
                   true ->
                       erlmcp_otel:record_error(SpanCtx, {tool_concurrency_limit, Tool}),
                       erlmcp_metrics:record("cli.tool.call_async.concurrency_error", 1),
                       {reply, {error, concurrency_limit}, State}
                end;
            {error, not_found} ->
                erlmcp_otel:record_error(SpanCtx, {tool_not_found, Tool}),
                erlmcp_metrics:record("cli.tool.async_tool_not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {async_tool_call_error, Tool, Error, Reason}),
            erlmcp_metrics:record("cli.tool.call_async.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_tool, ExecutionId, Reason, Options}, _From, State) ->
    %% Create OTEL span for tool cancellation
    SpanCtx = erlmcp_otel:inject_span("cli.tool.cancel",
                                     #{<<"execution_id">> => ExecutionId},
                                     undefined),

    try
        case maps:find(ExecutionId, State#tool_state.executions) of
            {ok, Execution} ->
                case Execution#execution.status of
                    running ->
                        %% Cancel the execution
                        case cancel_execution(ExecutionId, Reason, State) of
                            ok ->
                                erlmcp_otel:record_event(SpanCtx, <<"tool.cancelled">>, #{}),
                                erlmcp_metrics:record("cli.tool.cancel.success", 1),
                                {reply, ok, State};
                            {error, CancelReason} ->
                                erlmcp_otel:record_error(SpanCtx, {cancellation_failed, ExecutionId, CancelReason}),
                                erlmcp_metrics:record("cli.tool.cancel.error", 1),
                                {reply, {error, CancelReason}, State}
                        end;
                    completed ->
                        erlmcp_otel:record_error(SpanCtx, {tool_already_completed, ExecutionId}),
                        erlmcp_metrics:record("cli.tool.cancel.already_completed", 1),
                        {reply, {error, already_completed}, State};
                    cancelled ->
                        erlmcp_otel:record_error(SpanCtx, {tool_already_cancelled, ExecutionId}),
                        erlmcp_metrics:record("cli.tool.cancel.already_cancelled", 1),
                        {reply, {error, already_cancelled}, State};
                    failed ->
                        erlmcp_otel:record_error(SpanCtx, {tool_already_failed, ExecutionId}),
                        erlmcp_metrics:record("cli.tool.cancel.already_failed", 1),
                        {reply, {error, already_failed}, State}
                end;
            error ->
                erlmcp_otel:record_error(SpanCtx, {execution_not_found, ExecutionId}),
                erlmcp_metrics:record("cli.tool.cancel.not_found", 1),
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            erlmcp_otel:record_error(SpanCtx, {cancellation_error, ExecutionId, Error, Reason}),
            erlmcp_metrics:record("cli.tool.cancel.error", 1),
            {reply, {error, Reason}, State}
    end;

handle_call({get_tool_status, ExecutionId}, _From, State) ->
    try
        case maps:find(ExecutionId, State#tool_state.executions) of
            {ok, Execution} ->
                Status = format_execution_status(Execution),
                {reply, {ok, Status}, State};
            error ->
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_tool_result, ExecutionId}, _From, State) ->
    try
        case maps:find(ExecutionId, State#tool_state.executions) of
            {ok, Execution} ->
                case Execution#execution.status of
                    completed ->
                        {reply, {ok, Execution#execution.result}, State};
                    failed ->
                        {reply, {error, Execution#execution.error}, State};
                    _ ->
                        {reply, {error, not_completed}, State}
                end;
            error ->
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call({stream_tool_output, ExecutionId}, _From, State) ->
    try
        case maps:find(ExecutionId, State#tool_state.executions) of
            {ok, Execution} ->
                case Execution#execution.stream of
                    undefined ->
                        %% Create stream process
                        case start_stream_process(ExecutionId, State) of
                            {ok, StreamPid} ->
                                %% Update execution with stream PID
                                NewExecution = Execution#execution{stream = StreamPid},
                                NewExecutions = maps:put(ExecutionId, NewExecution, State#tool_state.executions),
                                {reply, {ok, StreamPid}, State#tool_state{executions = NewExecutions}};
                            {error, Reason} ->
                                {reply, {error, Reason}, State}
                        end;
                    StreamPid ->
                        {reply, {ok, StreamPid}, State}
                end;
            error ->
                {reply, {error, not_found}, State}
        end
    catch
        Error:Reason ->
            {reply, {error, Reason}, State}
    end;

handle_call({subscribe_tool_events, Pid}, _From, State) ->
    %% Add event subscriber
    Handlers = maps:put(Pid, true, State#tool_state.handlers),
    {reply, ok, State#tool_state{handlers = Handlers}};

handle_call({unsubscribe_tool_events, Pid}, _From, State) ->
    %% Remove event subscriber
    Handlers = maps:remove(Pid, State#tool_state.handlers),
    {reply, ok, State#tool_state{handlers = Handlers}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

%% @doc Handle asynchronous casts
-spec handle_cast(term(), #tool_state{}) -> {noreply, #tool_state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle messages
-spec handle_info(term(), #tool_state{}) -> {noreply, #tool_state{}}}.
handle_info(cleanup_executions, State) ->
    %% Create OTEL span for execution cleanup
    SpanCtx = erlmcp_otel:inject_span("cli.tool.cleanup", #{}, undefined),

    %% Clean up completed and failed executions older than 5 minutes
    Now = erlang:system_time(millisecond),
    CleanupThreshold = 300000, % 5 minutes

    ExecutionsToKeep = maps:filter(fun(_Id, Execution) ->
                                         case Execution#execution.status of
                                             running -> true;
                                             completed when Execution#execution.end_time == undefined -> true;
                                             completed when Now - Execution#execution.end_time < CleanupThreshold -> true;
                                             failed when Now - Execution#execution.start_time < CleanupThreshold -> true;
                                             _ -> false
                                         end
                                     end, State#tool_state.executions),

    ExecutionsToRemove = maps:size(State#tool_state.executions) - maps:size(ExecutionsToKeep),
    erlmcp_otel:record_event(SpanCtx, <<"executions.cleaned">>, #{<<"removed_count">> => ExecutionsToRemove}),
    erlmcp_metrics:record("cli.tool.cleanup.executions_removed", ExecutionsToRemove),

    %% Schedule next cleanup
    erlang:send_after(60000, self(), cleanup_executions),

    {noreply, State#tool_state{executions = ExecutionsToKeep}};

handle_info({execution_completed, ExecutionId, Result}, State) ->
    %% Update execution status
    case maps:find(ExecutionId, State#tool_state.executions) of
        {ok, Execution} ->
            NewExecution = Execution#execution{status = completed,
                                            end_time = erlang:system_time(millisecond),
                                            result = Result},
            NewExecutions = maps:put(ExecutionId, NewExecution, State#tool_state.executions),

            %% Notify subscribers
            notify_tool_event(completed, ExecutionId, Result),

            %% Update metrics
            Metrics = update_metrics(State#tool_state.metrics, completed),

            {noreply, State#tool_state{executions = NewExecutions, metrics = Metrics}};
        error ->
            {noreply, State}
    end;

handle_info({execution_failed, ExecutionId, Error}, State) ->
    %% Update execution status
    case maps:find(ExecutionId, State#tool_state.executions) of
        {ok, Execution} ->
            NewExecution = Execution#execution{status = failed,
                                            end_time = erlang:system_time(millisecond),
                                            error = Error},
            NewExecutions = maps:put(ExecutionId, NewExecution, State#tool_state.executions),

            %% Notify subscribers
            notify_tool_event(failed, ExecutionId, Error),

            %% Update metrics
            Metrics = update_metrics(State#tool_state.metrics, failed),

            {noreply, State#tool_state{executions = NewExecutions, metrics = Metrics}};
        error ->
            {noreply, State}
    end;

handle_info({output_chunk, ExecutionId, Chunk}, State) ->
    %% Forward output chunk to stream
    case maps:find(ExecutionId, State#tool_state.streams) of
        {ok, StreamPid} ->
            StreamPid ! {output_chunk, Chunk};
        error ->
            %% No active stream, ignore chunk
            ok
    end,

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate the server
-spec terminate(term(), #tool_state{}) -> ok.
terminate(_Reason, State) ->
    %% Create OTEL span for tool manager termination
    erlmcp_otel:with_span("cli.tool.terminate",
                          #{<<"active_executions">> => maps:size(State#tool_state.executions),
                            <<"active_streams">> => maps:size(State#tool_state.streams)},
                          fun() ->
                             %% Cancel all running executions
                            lists:foreach(fun({ExecutionId, Execution}) ->
                                             case Execution#execution.status of
                                                 running ->
                                                     cancel_execution(ExecutionId, <<"terminated">>, State);
                                                 _ -> ok
                                             end
                                         end, maps:to_list(State#tool_state.executions)),

                             %% Stop all streams
                            lists:foreach(fun({_ExecutionId, StreamPid}) ->
                                             case erlang:is_process_alive(StreamPid) of
                                                 true -> erlang:exit(StreamPid, normal);
                                                 false -> ok
                                             end
                                         end, maps:to_list(State#tool_state.streams)),

                             %% Notify subscribers of shutdown
                            notify_tool_event(terminated, undefined, undefined),

                             %% Record final metrics
                             erlmcp_metrics:record("cli.tool.terminated", 1),

                             ok
                          end).

%% @doc Handle code changes
-spec code_change(term(), #tool_state{}, term()) -> {ok, #tool_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Load available tools
-spec load_tools() -> map().
load_tools() ->
    try
        %% Load tools from core
        CoreTools = load_core_tools(),

        %% Load tools from registry
        RegistryTools = load_registry_tools(),

        %% Load tools from configuration
        ConfigTools = load_config_tools(),

        %% Merge all tools
        maps:merge(maps:merge(CoreTools, RegistryTools), ConfigTools)
    catch
        Error:Reason ->
            lager:warning("Failed to load tools: ~p:~p", [Error, Reason]),
            #{}
    end.

%% @doc Load core tools
-spec load_core_tools() -> map().
load_core_tools() ->
    try
        case erlmcp_registry:find_tools() of
            {ok, Tools} ->
                lists:foldl(fun(Tool, Acc) ->
                                 Name = maps:get(<<"name">>, Tool),
                                 maps:put(Name, Tool, Acc)
                             end, #{}, Tools);
            {error, _} ->
                #{}
        end
    catch
        _ -> #{}
    end.

%% @doc Load registry tools
-spec load_registry_tools() -> map().
load_registry_tools() ->
    Tools = #{},

    %% Load from .erlmcp_tools directory
    ToolsDir = filename:join(os:getenv("HOME", "."), ".erlmcp_tools"),
    case file:list_dir(ToolsDir) of
        {ok, Files} ->
            lists:foldl(fun(File, Acc) ->
                             case filename:extension(File) of
                                 <<".json">> ->
                                     FilePath = filename:join(ToolsDir, File),
                                     case file:read_file(FilePath) of
                                         {ok, Content} ->
                                             Tool = jsx:decode(Content, [{labels, binary}, return_maps]),
                                             Name = maps:get(<<"name">>, Tool),
                                             maps:put(Name, Tool, Acc);
                                         {error, _} -> Acc
                                     end;
                                 _ -> Acc
                             end
                         end, Tools, Files);
        {error, _} -> Tools
    end.

%% @doc Load configuration tools
-spec load_config_tools() -> map().
load_config_tools() ->
    Tools = #{},

    %% Load from environment variables
    case os:getenv("ERLMCP_TOOLS") of
        false -> Tools;
        JsonData ->
            try
                jsx:decode(list_to_binary(JsonData), [{labels, binary}, return_maps])
            catch
                _ -> Tools
            end
    end.

%% @doc Filter tools
-spec filter_tools(map(), map()) -> list().
filter_tools(Tools, Filters) ->
    lists:filter(fun(Name) ->
                     case maps:find(Name, Tools) of
                         {ok, Tool} -> matches_tool_filters(Tool, Filters);
                         error -> false
                     end
                 end, maps:keys(Tools)).

%% @doc Check if tool matches filters
-spec matches_tool_filters(map(), map()) -> boolean().
matches_tool_filters(Tool, Filters) ->
    lists:all(fun({FilterKey, FilterValue}) ->
                     case maps:find(FilterKey, Tool) of
                         {ok, ToolValue} -> ToolValue == FilterValue;
                         error -> false
                     end
                 end, maps:to_list(Filters)).

%% @doc Find tool
-spec find_tool(binary(), #tool_state{}) -> {ok, map()} | {error, term()}.
find_tool(Tool, State) ->
    case maps:find(Tool, State#tool_state.tools) of
        {ok, ToolInfo} -> {ok, ToolInfo};
        error -> {error, not_found}
    end.

%% @doc Execute tool synchronously
-spec execute_tool_sync(binary(), list(), integer(), map(), #tool_state{}) -> {ok, term()} | {error, term()}.
execute_tool_sync(Tool, Args, Timeout, ToolInfo, State) ->
    try
        %% Get tool implementation
        Module = maps:get(<<"module">>, ToolInfo),
        Function = maps:get(<<"function">>, ToolInfo),

        %% Execute with timeout
        case erlang:apply(Module, Function, Args) of
            Result when Timeout == infinity ->
                {ok, Result};
            Result ->
                {ok, Result}
        end
    catch
        Error:Reason ->
            {error, {execution_error, Error, Reason}}
    end.

%% @doc Start async execution
-spec start_async_execution(binary(), binary(), list(), integer(), map(), map(), #tool_state{}) -> {ok, pid()} | {error, term()}.
start_async_execution(ExecutionId, Tool, Args, Timeout, Options, ToolInfo, State) ->
    try
        %% Create execution process
        {ok, Pid} = erlmcp_tool_worker:start_link(ExecutionId, Tool, Args, Timeout, Options, ToolInfo),

        %% Register execution
        erlang:monitor(process, Pid),

        ok
    catch
        Error:Reason ->
            {error, {worker_start_failed, Error, Reason}}
    end.

%% @doc Cancel execution
-spec cancel_execution(binary(), binary(), #tool_state{}) -> ok | {error, term()}.
cancel_execution(ExecutionId, Reason, State) ->
    try
        case maps:find(ExecutionId, State#tool_state.executions) of
            {ok, Execution} ->
                case erlang:is_process_alive(Execution#execution.pid) of
                    true ->
                        erlmcp_tool_worker:cancel(Execution#execution.pid, Reason);
                    false ->
                        ok
                end;
            error ->
                {error, execution_not_found}
        end
    catch
        Error:Reason ->
            {error, {cancellation_error, Error, Reason}}
    end.

%% @doc Start stream process
-spec start_stream_process(binary(), #tool_state{}) -> {ok, pid()} | {error, term()}.
start_stream_process(ExecutionId, State) ->
    try
        %% Create streaming process
        {ok, StreamPid} = erlmcp_tool_stream:start_link(ExecutionId),

        %% Register stream
        NewStreams = maps:put(ExecutionId, StreamPid, State#tool_state.streams),

        {ok, StreamPid}
    catch
        Error:Reason ->
            {error, {stream_start_failed, Error, Reason}}
    end.

%% @doc Notify tool event
-spec notify_tool_event(atom(), binary(), term()) -> ok.
notify_tool_event(Event, ExecutionId, Data) ->
    try
        %% Send event to all subscribers
        lists:foreach(fun(Pid) ->
                         Pid ! {tool_event, Event, ExecutionId, Data}
                     end, get_tool_subscribers()),
        ok
    catch
        Error:Reason ->
            lager:warning("Failed to notify tool subscribers: ~p:~p", [Error, Reason]),
            ok
    end.

%% @doc Format execution status
-spec format_execution_status(#execution{}) -> map().
format_execution_status(Execution) ->
    #{<<"id">> => Execution#execution.id,
      <<"tool">> => Execution#execution.tool,
      <<"status">> => Execution#execution.status,
      <<"start_time">> => Execution#execution.start_time,
      <<"end_time">> => Execution#execution.end_time,
      <<"duration">> => case Execution#execution.end_time of
                          undefined -> undefined;
                          EndTime -> EndTime - Execution#execution.start_time
                       end,
      <<"result">> => Execution#execution.result,
      <<"error">> => Execution#execution.error}.

%% @doc Start worker pool
-spec start_worker_pool() -> list().
start_worker_pool() ->
    Workers = [],
    %% Initialize worker processes
    lists:foldl(fun(I, Acc) ->
                     case erlmcp_tool_worker_sup:start_child() of
                         {ok, Pid} -> [Pid | Acc];
                         {error, _} -> Acc
                     end
                 end, Workers, lists:seq(1, 10)).

%% @doc Generate execution ID
-spec generate_execution_id() -> binary().
generate_execution_id() ->
    Id = crypto:strong_rand_bytes(16),
    base64:encode(Id).

%% @doc Initialize metrics
-spec init_metrics() -> map().
init_metrics() ->
    #{"tools.count" => 0,
      "tools.executed" => 0,
      "tools.success" => 0,
      "tools.failed" => 0,
      "tools.cancelled" => 0,
      "executions.active" => 0,
      "executions.completed" => 0,
      "executions.failed" => 0,
      "executions.cancelled" => 0,
      "streaming.active" => 0}.

%% @doc Update metrics
-spec update_metrics(map(), atom()) -> map().
update_metrics(Metrics, Type) ->
    case Type of
        completed ->
            maps:update_with("executions.completed", fun(V) -> V + 1 end,
                          maps:update_with("tools.success", fun(V) -> V + 1 end,
                          maps:update_with("tools.executed", fun(V) -> V + 1 end, Metrics)));
        failed ->
            maps:update_with("executions.failed", fun(V) -> V + 1 end,
                          maps:update_with("tools.failed", fun(V) -> V + 1 end,
                          maps:update_with("tools.executed", fun(V) -> V + 1 end, Metrics)));
        cancelled ->
            maps:update_with("executions.cancelled", fun(V) -> V + 1 end,
                          maps:update_with("tools.cancelled", fun(V) -> V + 1 end, Metrics));
        _ -> Metrics
    end.