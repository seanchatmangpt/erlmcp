-module(erlmcp_python_bridge).

-behaviour(gen_server).

%% Python Bridge for MCP Tool Execution
%%
%% This module provides a bridge between Erlang and Python for executing
%% MCP tools as Python subprocesses. Uses JSON-RPC 2.0 protocol for
%% communication with Python processes.
%%
%% Features:
%% - Python subprocess management
%% - JSON-RPC 2.0 protocol
%% - Tool invocation and result retrieval
%% - Error handling and recovery
%% - Timeout management

%% API exports
-export([start_link/0, start_link/1,
         invoke_tool/3,
         invoke_tool/4,
         list_tools/1,
         load_tool/2,
         close_bridge/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type exports
-export_type([python_tool/0, tool_result/0, bridge_error/0]).

-include("erlmcp.hrl").

%%====================================================================
%% Types
%%====================================================================

-type tool_name() :: binary().
-type tool_params() :: map().
-type python_path() :: binary() | string().

-record(python_tool,
        {name :: tool_name(),
         description :: binary(),
         module :: binary(),
         params :: #{binary() => term()}}).

-type python_tool() :: #python_tool{}.

-type tool_result() :: {ok, term()} | {error, bridge_error()}.
-type bridge_error() :: {python_not_found | tool_not_found |
                        timeout | json_decode_error | python_error,
                        term()}.

-record(bridge_state,
        {port_pid :: pid() | undefined,
         python_path :: python_path(),
         tools = #{} :: #{tool_name() => python_tool()},
         request_id = 1 :: pos_integer(),
         pending = #{} :: #{pos_integer() => {pid(), reference()}}}).

-type bridge_state() :: #bridge_state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start Python bridge with default python3
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start Python bridge with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Invoke Python tool with parameters (default 10s timeout)
-spec invoke_tool(pid(), tool_name(), tool_params()) -> tool_result().
invoke_tool(BridgePid, ToolName, Params) ->
    invoke_tool(BridgePid, ToolName, Params, 10000).

%% @doc Invoke Python tool with parameters and custom timeout
-spec invoke_tool(pid(), tool_name(), tool_params(), timeout()) -> tool_result().
invoke_tool(BridgePid, ToolName, Params, Timeout) when is_pid(BridgePid), is_binary(ToolName) ->
    gen_server:call(BridgePid, {invoke_tool, ToolName, Params, Timeout}, Timeout + 1000).

%% @doc List available Python tools
-spec list_tools(pid()) -> {ok, [tool_name()]} | {error, term()}.
list_tools(BridgePid) when is_pid(BridgePid) ->
    gen_server:call(BridgePid, list_tools, 5000).

%% @doc Load Python tool module
-spec load_tool(pid(), binary()) -> ok | {error, term()}.
load_tool(BridgePid, ToolModule) when is_pid(BridgePid), is_binary(ToolModule) ->
    gen_server:call(BridgePid, {load_tool, ToolModule}, 5000).

%% @doc Close Python bridge and cleanup
-spec close_bridge(pid()) -> ok.
close_bridge(BridgePid) when is_pid(BridgePid) ->
    gen_server:call(BridgePid, close_bridge, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize Python bridge
init(Options) ->
    process_flag(trap_exit, true),

    %% Get Python path from options or default to python3
    PythonPath = maps:get(python_path, Options, "python3"),

    %% Start port tool
    PortToolOptions = maps:get(port_options, Options, #{}),
    case erlmcp_port_tool:start_link(PortToolOptions) of
        {ok, PortPid} ->
            %% Start Python subprocess with JSON-RPC handler
            ScriptPath = find_python_script(),
            Args = ["-u", ScriptPath],  % -u for unbuffered output
            case erlmcp_port_tool:start_port(PortPid, {PythonPath, Args}) of
                {ok, _Port} ->
                    logger:info("Python bridge started with: ~p ~p",
                               [PythonPath, ScriptPath]),
                    {ok, #bridge_state{
                        port_pid = PortPid,
                        python_path = PythonPath
                    }};
                {error, Reason} ->
                    logger:error("Failed to start Python port: ~p", [Reason]),
                    {stop, {port_failed, Reason}}
            end;
        {error, Reason} ->
            logger:error("Failed to start port tool: ~p", [Reason]),
            {stop, {port_tool_failed, Reason}}
    end.

%% @doc Handle tool invocation
handle_call({invoke_tool, ToolName, Params, Timeout}, From, State) ->
    %% Create JSON-RPC request
    RequestId = State#bridge_state.request_id,
    Request = format_tool_request(RequestId, ToolName, Params),

    %% Send request to Python
    case erlmcp_port_tool:send_request(State#bridge_state.port_pid, Request) of
        ok ->
            %% Track pending request
            NewPending = maps:put(RequestId, {From, make_ref(), erlang:monotonic_time(millisecond)},
                                  State#bridge_state.pending),
            NewState = State#bridge_state{
                request_id = RequestId + 1,
                pending = NewPending
            },
            %% Start timeout timer
            _TimerRef = erlang:send_after(Timeout, self(), {request_timeout, RequestId, From}),
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, {send_failed, Reason}}, State}
    end;

%% @doc Handle list tools request
handle_call(list_tools, _From, #bridge_state{tools = Tools} = State) ->
    ToolNames = maps:keys(Tools),
    {reply, {ok, ToolNames}, State};

%% @doc Handle load tool request
handle_call({load_tool, ToolModule}, _From, State) ->
    %% Send import request to Python
    Request = erlmcp_json_native:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => State#bridge_state.request_id,
        <<"method">> => <<"load_tool">>,
        <<"params">> => #{<<"module">> => ToolModule}
    }),

    case erlmcp_port_tool:send_request(State#bridge_state.port_pid, Request) of
        ok ->
            %% Wait for response (simplified - should use async in production)
            case receive_response(5000) of
                {ok, _Response} ->
                    NewState = State,
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, {send_failed, Reason}}, State}
    end;

%% @doc Handle close bridge request
handle_call(close_bridge, _From, #bridge_state{port_pid = PortPid} = State) ->
    erlmcp_port_tool:close_port(PortPid),
    {reply, ok, State};

%% @doc Handle unknown calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle timeout messages
handle_info({request_timeout, RequestId, From}, State) ->
    %% Request timed out
    case maps:take(RequestId, State#bridge_state.pending) of
        {{From, _Ref, _StartTime}, NewPending} ->
            gen_server:reply(From, {error, timeout}),
            {noreply, State#bridge_state{pending = NewPending}};
        error ->
            {noreply, State}
    end;

%% @doc Handle port responses
handle_info({port_response, Data}, State) ->
    %% Parse JSON-RPC response
    try erlmcp_json_native:decode(Data) of
        #{<<"id">> := RequestId, <<"result">> := Result} ->
            %% Successful response
            case maps:take(RequestId, State#bridge_state.pending) of
                {{From, _Ref, StartTime}, NewPending} ->
                    %% Calculate latency
                    Latency = erlang:monotonic_time(millisecond) - StartTime,
                    logger:debug("Tool request ~p completed in ~pms", [RequestId, Latency]),
                    gen_server:reply(From, {ok, Result}),
                    {noreply, State#bridge_state{pending = NewPending}};
                error ->
                    logger:warning("Unexpected response ID: ~p", [RequestId]),
                    {noreply, State}
            end;
        #{<<"id">> := RequestId, <<"error">> := Error} ->
            %% Error response
            case maps:take(RequestId, State#bridge_state.pending) of
                {{From, _Ref, _StartTime}, NewPending} ->
                    gen_server:reply(From, {error, {python_error, Error}}),
                    {noreply, State#bridge_state{pending = NewPending}};
                error ->
                    {noreply, State}
            end;
        _ ->
            logger:warning("Malformed Python response: ~p", [Data]),
            {noreply, State}
    catch
        _:Reason ->
            logger:error("Failed to decode Python response: ~p, reason: ~p",
                        [Data, Reason]),
            {noreply, State}
    end;

%% @doc Handle EXIT from port process
handle_info({'EXIT', PortPid, Reason}, #bridge_state{port_pid = PortPid} = State) ->
    logger:error("Port process exited: ~p", [Reason]),
    %% Notify all pending requests
    notify_pending_errors({error, port_exited}, State),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate bridge and cleanup
terminate(_Reason, #bridge_state{port_pid = PortPid}) when PortPid =/= undefined ->
    erlmcp_port_tool:close_port(PortPid),
    ok;
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Find Python JSON-RPC script
-spec find_python_script() -> string().
find_python_script() ->
    %% Look for script in priv directory
    case code:priv_dir(erlmcp_core) of
        {error, bad_name} ->
            %% Fallback to development path
            filename:join([filename:dirname(code:which(?MODULE)),
                          "..", "priv", "mcp_tool_server.py"]);
        Dir ->
            filename:join(Dir, "mcp_tool_server.py")
    end.

%% @doc Format tool invocation as JSON-RPC request
-spec format_tool_request(pos_integer(), tool_name(), tool_params()) -> binary().
format_tool_request(RequestId, ToolName, Params) ->
    erlmcp_json_native:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"call_tool">>,
        <<"params">> => #{
            <<"tool">> => ToolName,
            <<"arguments">> => Params
        }
    }).

%% @doc Receive response from port (simplified)
-spec receive_response(timeout()) -> {ok, binary()} | {error, term()}.
receive_response(Timeout) ->
    receive
        {port_response, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, Reason}
    after Timeout ->
        {error, timeout}
    end.

%% @doc Notify all pending requests of error
-spec notify_pending_errors(term(), bridge_state()) -> ok.
notify_pending_errors(Error, #bridge_state{pending = Pending}) ->
    maps:foreach(fun(_RequestId, {From, _Ref, _StartTime}) ->
                    gen_server:reply(From, Error)
                 end, Pending),
    ok.
