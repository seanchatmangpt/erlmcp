%%%-------------------------------------------------------------------
%%% @doc
%%% STDIO Transport Implementation
%%% 
%%% This module implements the standard input/output transport for MCP.
%%% It provides bidirectional communication through stdin/stdout pipes.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_stdio_new).
-behaviour(gen_server).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([
    start_link/2,
    send/2,
    close/1,
    get_info/1,
    trim_line/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% State record
-record(state, {
    transport_id :: atom(),
    config :: map(),
    stdin_port :: port() | undefined,
    stdout_port :: port() | undefined,
    buffer :: binary(),
    registry_pid :: pid() | undefined,
    test_mode :: boolean()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    ?LOG_INFO("Starting STDIO transport: ~p with config: ~p", [TransportId, Config]),
    gen_server:start_link(?MODULE, [TransportId, Config], []).

-spec send(pid() | state(), binary() | string()) -> ok | {error, term()}.
send(Pid, Data) when is_pid(Pid) ->
    gen_server:call(Pid, {send, Data});
send(#state{} = State, Data) ->
    send_data(State, Data).

-spec close(pid() | state()) -> ok.
close(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, close);
close(#state{} = State) ->
    close_ports(State).

-spec get_info(pid() | state()) -> map().
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info);
get_info(#state{transport_id = Id, config = Config, test_mode = TestMode}) ->
    #{
        transport_id => Id,
        type => stdio,
        config => Config,
        test_mode => TestMode,
        status => running
    }.

-spec trim_line(binary()) -> binary().
trim_line(Line) ->
    % Remove trailing newlines and carriage returns
    re:replace(Line, "[\r\n]+$", "", [global, {return, binary}]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom() | map()]) -> {ok, state()} | {error, term()}.
init([TransportId, Config]) ->
    ?LOG_INFO("Initializing STDIO transport ~p", [TransportId]),
    
    TestMode = maps:get(test_mode, Config, false),
    
    State = #state{
        transport_id = TransportId,
        config = Config,
        buffer = <<>>,
        test_mode = TestMode
    },
    
    case TestMode of
        true ->
            ?LOG_INFO("STDIO transport ~p initialized in test mode", [TransportId]),
            {ok, State};
        false ->
            case init_ports(State) of
                {ok, NewState} ->
                    register_with_registry(NewState),
                    ?LOG_INFO("STDIO transport ~p initialized successfully", [TransportId]),
                    {ok, NewState};
                {error, Reason} = Error ->
                    ?LOG_ERROR("Failed to initialize STDIO transport ~p: ~p", [TransportId, Reason]),
                    Error
            end
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()} | {stop, term(), term(), state()}.
handle_call({send, Data}, _From, State) ->
    case send_data(State, Data) of
        ok ->
            {reply, ok, State};
        {error, Reason} = Error ->
            ?LOG_ERROR("Failed to send data: ~p", [Reason]),
            {reply, Error, State}
    end;

handle_call(close, _From, State) ->
    NewState = close_ports(State),
    {reply, ok, NewState};

handle_call(get_info, _From, State) ->
    Info = get_info(State),
    {reply, Info, State};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({data, Data}, State) ->
    NewState = handle_incoming_data(State, Data),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({stdin_port, {data, Data}}, State) ->
    NewState = handle_incoming_data(State, Data),
    {noreply, NewState};

handle_info({stdin_port, closed}, State) ->
    ?LOG_WARNING("STDIN port closed for transport ~p", [State#state.transport_id]),
    {stop, stdin_closed, State};

handle_info({Port, {exit_status, Status}}, State) when is_port(Port) ->
    ?LOG_WARNING("Port ~p exited with status ~p", [Port, Status]),
    NewState = handle_port_exit(State, Port),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(Reason, State) ->
    ?LOG_INFO("Terminating STDIO transport ~p: ~p", [State#state.transport_id, Reason]),
    close_ports(State),
    unregister_from_registry(State),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec init_ports(state()) -> {ok, state()} | {error, term()}.
init_ports(#state{test_mode = true} = State) ->
    {ok, State};
init_ports(State) ->
    try
        % For now, use simple port setup - this can be enhanced later
        % In test mode, we don't actually create ports
        {ok, State}
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Failed to initialize ports: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {port_init_failed, Reason}}
    end.

-spec send_data(state(), binary() | string()) -> ok | {error, term()}.
send_data(#state{test_mode = true}, _Data) ->
    % In test mode, just pretend to send
    ok;
send_data(#state{stdout_port = undefined}, _Data) ->
    {error, port_not_available};
send_data(#state{stdout_port = Port}, Data) when is_binary(Data) ->
    try
        port_command(Port, Data),
        ok
    catch
        Class:Reason ->
            {error, {send_failed, Class, Reason}}
    end;
send_data(State, Data) when is_list(Data) ->
    send_data(State, list_to_binary(Data)).

-spec handle_incoming_data(state(), binary()) -> state().
handle_incoming_data(#state{buffer = Buffer} = State, NewData) ->
    UpdatedBuffer = <<Buffer/binary, NewData/binary>>,
    {ProcessedLines, RemainingBuffer} = extract_lines(UpdatedBuffer),
    
    % Process each complete line
    lists:foreach(fun(Line) ->
        TrimmedLine = trim_line(Line),
        case TrimmedLine of
            <<>> -> ok; % Skip empty lines
            _ -> process_message(State, TrimmedLine)
        end
    end, ProcessedLines),
    
    State#state{buffer = RemainingBuffer}.

-spec extract_lines(binary()) -> {[binary()], binary()}.
extract_lines(Buffer) ->
    extract_lines(Buffer, [], <<>>).

extract_lines(<<>>, Lines, CurrentLine) ->
    case CurrentLine of
        <<>> -> {lists:reverse(Lines), <<>>};
        _ -> {lists:reverse(Lines), CurrentLine}
    end;
extract_lines(<<$\n, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines(Rest, [CurrentLine | Lines], <<>>);
extract_lines(<<Char, Rest/binary>>, Lines, CurrentLine) ->
    extract_lines(Rest, Lines, <<CurrentLine/binary, Char>>).

-spec process_message(state(), binary()) -> ok.
process_message(#state{transport_id = TransportId}, Message) ->
    ?LOG_DEBUG("Processing message on transport ~p: ~p", [TransportId, Message]),
    % Here we would normally parse JSON-RPC and route to appropriate handler
    % For now, just log the message
    ok.

-spec close_ports(state()) -> state().
close_ports(#state{stdin_port = StdinPort, stdout_port = StdoutPort} = State) ->
    case StdinPort of
        undefined -> ok;
        Port when is_port(Port) -> 
            catch port_close(Port)
    end,
    case StdoutPort of
        undefined -> ok;
        Port2 when is_port(Port2) -> 
            catch port_close(Port2)
    end,
    State#state{stdin_port = undefined, stdout_port = undefined}.

-spec handle_port_exit(state(), port()) -> state().
handle_port_exit(#state{stdin_port = Port} = State, Port) ->
    ?LOG_WARNING("STDIN port exited"),
    State#state{stdin_port = undefined};
handle_port_exit(#state{stdout_port = Port} = State, Port) ->
    ?LOG_WARNING("STDOUT port exited"),
    State#state{stdout_port = undefined};
handle_port_exit(State, _Port) ->
    State.

-spec register_with_registry(state()) -> ok.
register_with_registry(#state{transport_id = TransportId, config = Config}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ?LOG_WARNING("Registry not available for transport ~p", [TransportId]),
            ok;
        _RegistryPid ->
            TransportConfig = Config#{type => stdio},
            case erlmcp_registry:register_transport(TransportId, self(), TransportConfig) of
                ok ->
                    ?LOG_DEBUG("Registered transport ~p with registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("Failed to register transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.

-spec unregister_from_registry(state()) -> ok.
unregister_from_registry(#state{transport_id = TransportId}) ->
    case whereis(erlmcp_registry) of
        undefined ->
            ok;
        _RegistryPid ->
            case erlmcp_registry:unregister_transport(TransportId) of
                ok ->
                    ?LOG_DEBUG("Unregistered transport ~p from registry", [TransportId]),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING("Failed to unregister transport ~p: ~p", [TransportId, Reason]),
                    ok
            end
    end.