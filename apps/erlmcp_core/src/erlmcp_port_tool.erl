-module(erlmcp_port_tool).

-behaviour(gen_server).

%% OTP 26-28 Port Driver for MCP External Tool Integration
%%
%% This module implements a gen_server wrapper around Erlang ports for
%% executing external tools (Python scripts, shell commands, etc.) as
%% part of the MCP tool system.
%%
%% OTP 26-28 Improvements:
%% - OTP 26: Improved error handling with {exit_status, Status} messages
%% - OTP 27: Better port driver memory management
%% - OTP 28: Enhanced port interaction with binary optimization

%% API exports
-export([start_link/0, start_link/1,
         start_port/2,
         send_request/2,
         recv_response/1, recv_response/2,
         close_port/1,
         port_info/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type exports
-export_type([port_state/0, request_id/0, port_error/0]).

-include("erlmcp.hrl").

%%====================================================================
%% Types
%%====================================================================

-type command() :: binary() | string().
-type args() :: [binary() | string()].
-type request_id() :: pos_integer().

-record(port_state,
        {port :: port() | undefined,
         command :: command() | undefined,
         args :: args() | undefined,
         request_id = 1 :: request_id(),
         pending = #{} :: #{request_id() => {pid(), reference()}},
         buffer = <<>> :: binary(),
         monitor :: reference() | undefined}).

-type port_state() :: #port_state{}.

-type port_error() :: {port_not_found | port_closed | timeout |
                       {exit_status, non_neg_integer()} |
                       {port_error, term()}, term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start port tool gen_server with default options
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start port tool gen_server with custom options
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% @doc Start external process as port
%% Uses OTP 28 port options: binary, exit_status, use_stdio, stream, line
-spec start_port(pid(), {command(), args()}) -> {ok, port()} | {error, term()}.
start_port(Pid, {Command, Args}) when is_pid(Pid) ->
    gen_server:call(Pid, {start_port, Command, Args}, 5000).

%% @doc Send request to port process
%% Request is sent as JSON binary for structured communication
-spec send_request(pid(), binary()) -> ok | {error, term()}.
send_request(Pid, Request) when is_pid(Pid), is_binary(Request) ->
    gen_server:call(Pid, {send_request, Request}, 5000).

%% @doc Receive response from port (uses default 5s timeout)
-spec recv_response(pid()) -> {ok, binary()} | {error, port_error()}.
recv_response(Pid) ->
    recv_response(Pid, 5000).

%% @doc Receive response from port with custom timeout
-spec recv_response(pid(), timeout()) -> {ok, binary()} | {error, port_error()}.
recv_response(Pid, Timeout) when is_pid(Pid) ->
    gen_server:call(Pid, {recv_response, Timeout}, Timeout + 1000).

%% @doc Close port and cleanup resources
-spec close_port(pid()) -> ok.
close_port(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, close_port, 5000).

%% @doc Get information about active port
-spec port_info(pid()) -> {ok, map()} | {error, term()}.
port_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, port_info, 1000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize port tool server
init(_Options) ->
    %% Trap exits for proper port cleanup
    process_flag(trap_exit, true),
    {ok, #port_state{}}.

%% @doc Handle synchronous call to start port
%% OTP 28: Uses spawn_executable with explicit binary mode
handle_call({start_port, Command, Args}, _From, State) ->
    case State#port_state.port of
        undefined ->
            %% Find executable
            Executable = find_executable(Command),
            case Executable of
                {ok, ExecPath} ->
                    %% OTP 28 port options for robust communication
                    PortOpts = [
                        {args, Args},
                        binary,            % Receive data as binaries
                        exit_status,       % Get exit status (OTP 26+)
                        use_stdio,         % Use stdin/stdout
                        stream,            % Stream data
                        {line, 8192},      % Line-oriented mode (8K max)
                        {cd, code:priv_dir(?MODULE)} % Working directory
                    ],
                    case catch open_port({spawn_executable, ExecPath}, PortOpts) of
                        Port when is_port(Port) ->
                            %% Monitor port for crashes
                            MonitorRef = erlang:monitor(port, Port),
                            NewState = State#port_state{
                                port = Port,
                                command = Command,
                                args = Args,
                                monitor = MonitorRef
                            },
                            {reply, {ok, Port}, NewState};
                        {'EXIT', Reason} ->
                            {reply, {error, {port_failed, Reason}}, State};
                        Error ->
                            {reply, {error, {port_failed, Error}}, State}
                    end;
                {error, Reason} ->
                    {reply, {error, {executable_not_found, Reason}}, State}
            end;
        _Port ->
            {reply, {error, port_already_open}, State}
    end;

%% @doc Handle synchronous request sending
handle_call({send_request, Request}, From, State) ->
    case State#port_state.port of
        undefined ->
            {reply, {error, port_not_found}, State};
        Port ->
            %% Create request correlation ID
            RequestId = State#port_state.request_id,
            %% Format request as JSON-RPC message
            JsonRequest = format_json_request(RequestId, Request),
            %% Send to port
            Port ! {self(), {command, JsonRequest}},
            %% Track pending request
            NewPending = maps:put(RequestId,
                                   {From, make_ref()},
                                   State#port_state.pending),
            %% Don't reply yet - wait for port response in handle_info
            NewState = State#port_state{
                request_id = RequestId + 1,
                pending = NewPending
            },
            {noreply, NewState}
    end;

%% @doc Handle synchronous response receiving
handle_call({recv_response, _Timeout}, From, State) ->
    %% Check if we have data in buffer
    case State#port_state.buffer of
        <<>> ->
            %% No data yet, wait for port response
            %% Store the From for later reply
            NewState = State#port_state{
                buffer = <<>>,
                pending = maps:put(wait, {From, make_ref()}, State#port_state.pending)
            },
            {noreply, NewState};
        Data ->
            %% Return buffered data
            NewState = State#port_state{buffer = <<>>},
            {reply, {ok, Data}, NewState}
    end;

%% @doc Handle port close request
handle_call(close_port, _From, #port_state{port = undefined} = State) ->
    {reply, ok, State};
handle_call(close_port, _From, #port_state{port = Port, monitor = Monitor} = State) ->
    %% Close port
    catch port_close(Port),
    %% Demonitor
    erlang:demonitor(Monitor, [flush]),
    NewState = State#port_state{
        port = undefined,
        command = undefined,
        args = undefined,
        monitor = undefined,
        pending = #{},
        buffer = <<>>
    },
    {reply, ok, NewState};

%% @doc Handle port info request
handle_call(port_info, _From, #port_state{port = undefined} = State) ->
    {reply, {error, port_not_open}, State};
handle_call(port_info, _From, #port_state{port = Port, command = Cmd, args = Args} = State) ->
    Info = #{
        port => Port,
        command => Cmd,
        args => Args,
        pending_requests => maps:size(State#port_state.pending),
        buffer_size => byte_size(State#port_state.buffer)
    },
    {reply, {ok, Info}, State};

%% @doc Handle unknown calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle port data messages (OTP 26+ improved handling)
handle_info({Port, {data, {eol, Data}}}, #port_state{port = Port} = State) ->
    %% End of line - complete message received
    NewState = State#port_state{
        buffer = <<>>
    },
    %% Process the response
    process_port_response(Data, NewState);

handle_info({Port, {data, {noeol, Data}}}, #port_state{port = Port} = State) ->
    %% Partial line - buffer it
    NewBuffer = <<(State#port_state.buffer)/binary, Data/binary>>,
    NewState = State#port_state{
        buffer = NewBuffer
    },
    {noreply, NewState};

handle_info({Port, {exit_status, 0}}, #port_state{port = Port} = State) ->
    %% Normal exit (status 0)
    logger:debug("Port exited normally: ~p", [State#port_state.command]),
    {noreply, State};

handle_info({Port, {exit_status, Status}}, #port_state{port = Port} = State) ->
    %% Abnormal exit
    logger:warning("Port exited with status ~p: ~p",
                   [Status, State#port_state.command]),
    %% Notify pending requests of failure
    notify_pending_errors({error, {exit_status, Status}}, State),
    {noreply, State};

handle_info({'DOWN', MonitorRef, port, Port, Reason}, #port_state{port = Port, monitor = MonitorRef} = State) ->
    %% Port crashed
    logger:error("Port crashed: ~p, reason: ~p",
                 [State#port_state.command, Reason]),
    %% Notify pending requests
    notify_pending_errors({error, port_crashed}, State),
    {noreply, State};

handle_info({'EXIT', Port, Reason}, #port_state{port = Port} = State) ->
    %% Port exited
    logger:info("Port exited: ~p, reason: ~p",
                [State#port_state.command, Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate port and cleanup
terminate(_Reason, #port_state{port = Port, monitor = Monitor}) when Port =/= undefined ->
    catch port_close(Port),
    erlang:demonitor(Monitor, [flush]),
    ok;
terminate(_Reason, _State) ->
    ok.

%% @doc Handle code change (hot code reloading)
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Find executable in PATH
-spec find_executable(command()) -> {ok, string()} | {error, not_found}.
find_executable(Command) when is_binary(Command) ->
    find_executable(binary_to_list(Command));
find_executable(Command) when is_list(Command) ->
    case os:find_executable(Command) of
        false -> {error, not_found};
        Path -> {ok, Path}
    end.

%% @doc Format request as JSON-RPC message
-spec format_json_request(request_id(), binary()) -> binary().
format_json_request(RequestId, Request) ->
    Json = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => RequestId,
        <<"method">> => <<"execute">>,
        <<"params">> => #{<<"request">> => Request}
    }),
    %% Add newline for line-oriented mode
    <<Json/binary, "\n">>.

%% @doc Process port response and reply to pending request
-spec process_port_response(binary(), port_state()) ->
    {noreply, port_state()}.
process_port_response(Data, #port_state{pending = Pending} = State) ->
    %% Try to parse JSON response
    try jsx:decode(Data, [return_maps]) of
        #{<<"id">> := RequestId, <<"result">> := Result} ->
            %% Successful response
            case maps:take(RequestId, Pending) of
                {{From, _Ref}, NewPending} ->
                    gen_server:reply(From, {ok, Result}),
                    {noreply, State#port_state{pending = NewPending}};
                error ->
                    %% No pending request for this ID
                    logger:warning("Unexpected response ID: ~p", [RequestId]),
                    {noreply, State}
            end;
        #{<<"id">> := RequestId, <<"error">> := Error} ->
            %% Error response
            case maps:take(RequestId, Pending) of
                {{From, _Ref}, NewPending} ->
                    gen_server:reply(From, {error, Error}),
                    {noreply, State#port_state{pending = NewPending}};
                error ->
                    {noreply, State}
            end;
        Json ->
            %% Malformed JSON or unexpected format
            logger:warning("Malformed port response: ~p", [Json]),
            {noreply, State}
    catch
        _:Reason ->
            %% JSON decode failed
            logger:error("Failed to decode port response: ~p, reason: ~p",
                        [Data, Reason]),
            {noreply, State}
    end.

%% @doc Notify all pending requests of error
-spec notify_pending_errors(term(), port_state()) -> ok.
notify_pending_errors(Error, #port_state{pending = Pending}) ->
    maps:foreach(fun(_RequestId, {From, _Ref}) ->
                    gen_server:reply(From, Error)
                 end, Pending),
    ok.
