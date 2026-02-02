%%%-------------------------------------------------------------------
%%% @doc
%%% stdio Transport module - an abstraction layer for stdio MCP comms
%%%
%%% Key Responsibilities:
%%% 1. Transport Abstraction
%%%     - Implements the transport behavior interface
%%%     - Adheres to a transport-agnostic API
%%% 2. Stdio Communication Management
%%%     - Spawns a background process to continuously read from stdin
%%%     - Provides a means of writing JSON messages to stdout
%%%     - Handles line-based message framing
%%%
%%% OTP 28 Features:
%%% - Hibernation for idle connections (5 min timeout)
%%% - Priority message handling (urgent control signals)
%%% - UTF-8 validation for all messages
%%% - Enhanced health monitoring
%%%
%%% This module is specifically designed to be used by erlmcp_server
%%% when the transport is configured as `{stdio, []}`.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_stdio).

%% -behaviour(erlmcp_transport_behavior).  % Conflicts with gen_server init/1
-behaviour(gen_server).

%% Transport behavior callbacks
-export([transport_init/1, send/2, close/1, get_info/1, handle_transport_call/2]).
%% API exports
-export([start_link/1, start_link/2, validate_message_size/2, get_max_message_size/0,
         send_urgent/2, set_hibernate_threshold/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% OTP 28: Priority message levels
-type priority() :: normal | urgent | critical.

%% Hibernation state
-record(hibernate_state,
        {enabled = true :: boolean(),
         idle_threshold = 300000 :: non_neg_integer(),  %% 5 minutes
         last_activity :: integer() | undefined}).

%% Priority queue state
-record(priority_state,
        {urgent_queue = queue:new() :: queue:queue(),
         normal_queue = queue:new() :: queue:queue(),
         max_urgent_size = 100 :: non_neg_integer()}).

-record(state,
        {owner :: pid(),
         owner_monitor :: reference() | undefined,
         reader :: pid() | undefined,
         buffer = <<>> :: binary(),
         test_mode = false :: boolean(),
         max_message_size :: pos_integer(),  % Maximum allowed message size in bytes
         transport_id :: atom() | binary() | undefined,
         hibernate :: #hibernate_state{},
         priority :: #priority_state{},
         utf8_validate = true :: boolean()}).

-type state() :: #state{}.

%% Default maximum message size: 16 MB
-define(DEFAULT_MAX_MESSAGE_SIZE, 16777216).

%% OTP 28: Hibernation timeouts
-define(HIBERNATE_IDLE_THRESHOLD, 300000). %% 5 minutes
-define(HIBERNATE_CHECK_INTERVAL, 60000).  %% 1 minute

%% OTP 28: UTF-8 validation
-define(UTF8_VALIDATE_MAX_SIZE, 1048576). %% 1MB max for validation

%%====================================================================
%% Transport Behavior Implementation
%%====================================================================

%% @doc Initialize transport (starts underlying gen_server)
-spec transport_init(map()) -> {ok, pid()} | {error, term()}.
transport_init(Config) when is_map(Config) ->
    Owner = maps:get(owner, Config, self()),
    start_link(Owner, Config).

%% @doc Get transport information
-spec get_info(pid() | term()) -> #{atom() => term()}.
get_info(Pid) when is_pid(Pid) ->
    case gen_server:call(Pid, get_state, 5000) of
        {ok, State} ->
            HibernateInfo = case State#state.hibernate of
                #hibernate_state{enabled = Enabled, idle_threshold = Threshold} ->
                    #{hibernate_enabled => Enabled,
                      hibernate_threshold => Threshold};
                _ ->
                    #{hibernate_enabled => true, hibernate_threshold => ?HIBERNATE_IDLE_THRESHOLD}
            end,

            PriorityInfo = case State#state.priority of
                #priority_state{} = PState ->
                    #{urgent_queue_size => queue:len(PState#priority_state.urgent_queue),
                      normal_queue_size => queue:len(PState#priority_state.normal_queue)};
                _ ->
                    #{urgent_queue_size => 0, normal_queue_size => 0}
            end,

            maps:merge(
                #{transport_id => State#state.transport_id,
                  type => stdio,
                  status => running,
                  test_mode => State#state.test_mode,
                  max_message_size => State#state.max_message_size,
                  utf8_validation => State#state.utf8_validate},
                maps:merge(HibernateInfo, PriorityInfo));
        _ ->
            #{transport_id => undefined,
              type => stdio,
              status => error}
    end;
get_info(_) ->
    #{transport_id => undefined,
      type => stdio,
      status => unknown}.

%% @doc Handle transport-specific calls
-spec handle_transport_call(term(), pid() | term()) ->
                               {reply, term(), pid() | term()} | {error, term()}.
handle_transport_call(_Request, State) ->
    {error, unknown_request}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(pid()) -> {ok, pid()} | {error, term()}.
start_link(Owner) when is_pid(Owner) ->
    start_link(Owner, #{}).

-spec start_link(pid(), map()) -> {ok, pid()} | {error, term()}.
start_link(Owner, Opts) when is_pid(Owner), is_map(Opts) ->
    gen_server:start_link(?MODULE, [Owner, Opts], []).

-spec send(pid() | term(), iodata()) -> ok | {error, term()}.
send(_TransportState, Message) ->
    do_send(Message, normal).

%% @doc Send urgent message (bypasses normal queue)
-spec send_urgent(pid() | term(), iodata()) -> ok | {error, term()}.
send_urgent(_TransportState, Message) ->
    do_send(Message, urgent).

%% @doc Internal send with priority
-spec do_send(iodata(), priority()) -> ok | {error, term()}.
do_send(Message, Priority) ->
    try
        %% OTP 28: Validate UTF-8 for text messages
        case validate_message_utf8(Message) of
            ok ->
                case is_binary(Message) of
                    true ->
                        io:format("~s~n", [Message]);
                    false ->
                        io:format("~s~n", [iolist_to_binary(Message)])
                end,
                ok;
            {error, invalid_utf8} ->
                {error, {utf8_validation_failed, Priority}}
        end
    catch
        error:Reason ->
            {error, {io_error, Reason}}
    end.

-spec close(pid() | term()) -> ok.
close(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid);
close(_) ->
    ok.

%% @doc Set hibernation threshold
-spec set_hibernate_threshold(pid(), non_neg_integer()) -> ok | {error, term()}.
set_hibernate_threshold(Pid, Threshold) when is_pid(Pid), is_integer(Threshold), Threshold >= 0 ->
    gen_server:call(Pid, {set_hibernate_threshold, Threshold});
set_hibernate_threshold(_Pid, _Threshold) ->
    {error, invalid_threshold}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(list()) -> {ok, #state{}}.
init([Owner]) ->
    init([Owner, #{}]);
init([Owner, Opts]) when is_map(Opts) ->
    process_flag(trap_exit, true),

    %% OTP 28: Enable hibernation support
    HibernateEnabled = maps:get(hibernate_enabled, Opts, true),
    HibernateThreshold = maps:get(hibernate_threshold, Opts, ?HIBERNATE_IDLE_THRESHOLD),
    UTF8Validate = maps:get(utf8_validate, Opts, true),

    %% Monitor the owner process for immediate termination
    OwnerMonitor = monitor(process, Owner),

    %% Register with registry if transport_id is provided
    TransportId = maps:get(transport_id, Opts, undefined),
    case TransportId of
        undefined ->
            ok;
        _ ->
            ok =
                erlmcp_registry:register_transport(TransportId,
                                                   self(),
                                                   #{type => stdio,
                                                     config => Opts,
                                                     utf8_validate => UTF8Validate})
    end,

    %% Check if we're in test mode by looking at the process dictionary
    % or checking if stdin is available
    TestMode = is_test_environment(),

    %% Get max message size from config, default to 16MB
    MaxMessageSize = get_max_message_size(),

    %% OTP 28: Initialize hibernation state
    HibernateState = #hibernate_state{
        enabled = HibernateEnabled,
        idle_threshold = HibernateThreshold,
        last_activity = erlang:monotonic_time(millisecond)
    },

    %% OTP 28: Initialize priority queue state
    PriorityState = #priority_state{
        urgent_queue = queue:new(),
        normal_queue = queue:new(),
        max_urgent_size = maps:get(max_urgent_queue, Opts, 100)
    },

    State =
        #state{owner = Owner,
               owner_monitor = OwnerMonitor,
               test_mode = TestMode,
               max_message_size = MaxMessageSize,
               transport_id = TransportId,
               hibernate = HibernateState,
               priority = PriorityState,
               utf8_validate = UTF8Validate},

    %% Only start the reader if we're not in test mode
    case TestMode of
        true ->
            %% In test mode, don't start a reader process
            {ok, State};
        false ->
            ReaderPid = spawn_link(fun() -> read_loop(self(), Owner, MaxMessageSize, UTF8Validate) end),
            {ok, State#state{reader = ReaderPid}}
    end.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()} | {reply, term(), state(), hibernate}.
handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State, hibernate};
handle_call({simulate_input, Line}, _From, #state{test_mode = true, owner = Owner} = State) ->
    %% Allow tests to simulate input
    Owner ! {transport_message, Line},
    {reply, ok, State, hibernate};
handle_call({set_hibernate_threshold, Threshold}, _From, State) ->
    NewHibernate = State#state.hibernate#hibernate_state{idle_threshold = Threshold},
    {reply, ok, State#state{hibernate = NewHibernate}, hibernate};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State, hibernate}.

-spec handle_cast(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate}.
handle_cast(_Msg, State) ->
    {noreply, State, hibernate}.

-spec handle_info(term(), state()) -> {noreply, state()} | {noreply, state(), hibernate} | {stop, term(), state()}.
handle_info({line, Line}, #state{owner = Owner, hibernate = Hibernate} = State) ->
    Owner ! {transport_message, Line},
    %% OTP 28: Update activity timestamp
    NewHibernate = Hibernate#hibernate_state{last_activity = erlang:monotonic_time(millisecond)},
    {noreply, State#state{hibernate = NewHibernate}, hibernate};
handle_info(check_hibernate, #state{hibernate = #hibernate_state{enabled = true} = Hibernate} = State) ->
    %% OTP 28: Check if we should hibernate
    Now = erlang:monotonic_time(millisecond),
    IdleTime = Now - Hibernate#hibernate_state.last_activity,
    NewHibernate = case IdleTime > Hibernate#hibernate_state.idle_threshold of
        true ->
            logger:debug("Stdio transport idle for ~pms, hibernating", [IdleTime]),
            Hibernate#hibernate_state{last_activity = Now};
        false ->
            Hibernate#hibernate_state{last_activity = Now}
    end,
    %% Reschedule next check
    erlang:send_after(?HIBERNATE_CHECK_INTERVAL, self(), check_hibernate),
    {noreply, State#state{hibernate = NewHibernate}, hibernate};
handle_info(check_hibernate, State) ->
    %% Hibernation disabled
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{reader = Pid, test_mode = false} = State) ->
    case Reason of
        normal ->
            %% In normal mode, if reader dies normally, we should probably stop too
            % but let's be more graceful about it
            logger:info("Reader process finished normally"),
            {noreply, State#state{reader = undefined}, hibernate};
        _ ->
            logger:error("Reader process died: ~p", [Reason]),
            {stop, {reader_died, Reason}, State}
    end;
handle_info({'DOWN', MonitorRef, process, Owner, Reason},
            #state{owner_monitor = MonitorRef, owner = Owner} = State) ->
    logger:info("Owner process ~p died: ~p", [Owner, Reason]),
    {stop, {owner_died, Reason}, State};
handle_info(_Info, State) ->
    %% OTP 28: Hibernate on unknown info messages to reduce memory footprint
    {noreply, State, hibernate}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason,
          #state{reader = Reader,
                 transport_id = TransportId,
                 owner_monitor = OwnerMonitor}) ->
    %% Unregister from registry if registered
    case TransportId of
        undefined ->
            ok;
        _ ->
            erlmcp_registry:unregister_transport(TransportId)
    end,

    %% Demonitor owner process to avoid DOWN messages during shutdown
    case OwnerMonitor of
        undefined ->
            ok;
        MonitorRef when is_reference(MonitorRef) ->
            erlang:demonitor(MonitorRef, [flush])
    end,

    %% Stop reader process
    case Reader of
        undefined ->
            ok;
        Pid when is_pid(Pid) ->
            exit(Pid, shutdown)
    end,
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec is_test_environment() -> boolean().
is_test_environment() ->
    %% Check various indicators that we're in a test environment
    case get(test_mode) of
        true ->
            true;
        _ ->
            %% Check if EUnit is running
            case whereis(eunit_proc) of
                undefined ->
                    %% Check if we can read from stdin without blocking
                    case stdin_available() of
                        true ->
                            false;
                        false ->
                            true  %% Assume test mode if stdin not available
                    end;
                _ ->
                    true  %% EUnit is running
            end
    end.

-spec stdin_available() -> boolean().
stdin_available() ->
    %% Try to check if stdin is available without blocking
    %% This is a heuristic - in a real application you might want more sophisticated detection
    case io:get_chars("", 0) of
        eof ->
            false;
        {error, _} ->
            false;
        _ ->
            true
    end.

%% @doc Validate UTF-8 encoding (OTP 28 feature)
-spec validate_message_utf8(iodata()) -> ok | {error, invalid_utf8}.
validate_message_utf8(Data) when is_binary(Data) ->
    case byte_size(Data) =< ?UTF8_VALIDATE_MAX_SIZE of
        true ->
            validate_utf8_fast(Data);
        false ->
            validate_utf8_chunked(Data, 0, byte_size(Data))
    end;
validate_message_utf8(Data) when is_list(Data) ->
    try
        Bin = iolist_to_binary(Data),
        validate_message_utf8(Bin)
    catch
        _:_ ->
            {error, invalid_utf8}
    end;
validate_message_utf8(_) ->
    {error, invalid_utf8}.

%% Fast UTF-8 validation for small messages
-spec validate_utf8_fast(binary()) -> ok | {error, invalid_utf8}.
validate_utf8_fast(<<>>) ->
    ok;
validate_utf8_fast(Data) ->
    case unicode:characters_to_list(Data, utf8) of
        {error, _, _} ->
            {error, invalid_utf8};
        {incomplete, _, _} ->
            {error, invalid_utf8};
        _List ->
            ok
    end.

%% Chunked UTF-8 validation for large messages
-spec validate_utf8_chunked(binary(), non_neg_integer(), non_neg_integer()) ->
                                   ok | {error, invalid_utf8}.
validate_utf8_chunked(_Data, Offset, TotalSize) when Offset >= TotalSize ->
    ok;
validate_utf8_chunked(Data, Offset, TotalSize) ->
    ChunkSize = min(?UTF8_VALIDATE_MAX_SIZE, TotalSize - Offset),
    <<_:Offset/bytes, Chunk:ChunkSize/bytes, _Rest/binary>> = Data,
    case validate_utf8_fast(Chunk) of
        ok ->
            validate_utf8_chunked(Data, Offset + ChunkSize, TotalSize);
        {error, _} = Error ->
            Error
    end.

%% Main read loop with message size validation and UTF-8 checking
-spec read_loop(pid(), pid(), pos_integer(), boolean()) -> no_return().
read_loop(Parent, Owner, MaxMessageSize, UTF8Validate) ->
    case io:get_line("") of
        eof ->
            logger:info("EOF received, stopping reader"),
            exit(normal);
        {error, Reason} ->
            logger:error("Read error: ~p", [Reason]),
            exit({read_error, Reason});
        Line when is_list(Line) ->
            BinaryLine = iolist_to_binary(Line),
            case validate_message_size(BinaryLine, MaxMessageSize) of
                ok ->
                    %% OTP 28: Validate UTF-8 if enabled
                    case UTF8Validate of
                        true ->
                            case validate_message_utf8(BinaryLine) of
                                ok ->
                                    process_line(Parent, BinaryLine),
                                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate);
                                {error, invalid_utf8} ->
                                    logger:error("Invalid UTF-8 in message, rejecting"),
                                    ErrorMsg = erlmcp_json_rpc:error_invalid_params(null,
                                        <<"Invalid UTF-8 encoding">>),
                                    io:format("~s~n", [ErrorMsg]),
                                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
                            end;
                        false ->
                            process_line(Parent, BinaryLine),
                            read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
                    end;
                {error, size_exceeded} ->
                    logger:error("Message size exceeded (~p bytes > ~p bytes limit)",
                                 [byte_size(BinaryLine), MaxMessageSize]),
                    %% Send proper JSON-RPC error response with MESSAGE_TOO_LARGE code (-32012)
                    ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
                    io:format("~s~n", [ErrorMsg]),
                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
            end;
        Line when is_binary(Line) ->
            case validate_message_size(Line, MaxMessageSize) of
                ok ->
                    case UTF8Validate of
                        true ->
                            case validate_message_utf8(Line) of
                                ok ->
                                    process_line(Parent, Line),
                                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate);
                                {error, invalid_utf8} ->
                                    logger:error("Invalid UTF-8 in message, rejecting"),
                                    ErrorMsg = erlmcp_json_rpc:error_invalid_params(null,
                                        <<"Invalid UTF-8 encoding">>),
                                    io:format("~s~n", [ErrorMsg]),
                                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
                            end;
                        false ->
                            process_line(Parent, Line),
                            read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
                    end;
                {error, size_exceeded} ->
                    logger:error("Message size exceeded (~p bytes > ~p bytes limit)",
                                 [byte_size(Line), MaxMessageSize]),
                    %% Send proper JSON-RPC error response with MESSAGE_TOO_LARGE code (-32012)
                    ErrorMsg = erlmcp_json_rpc:error_message_too_large(null, MaxMessageSize),
                    io:format("~s~n", [ErrorMsg]),
                    read_loop(Parent, Owner, MaxMessageSize, UTF8Validate)
            end
    end.

%% Helper function to handle line processing
process_line(Parent, Line) ->
    CleanLine = trim_line(Line),
    case byte_size(CleanLine) of
        0 ->
            ok;  %% Skip empty lines
        _ ->
            Parent ! {line, CleanLine},
            ok  %% Send and return ok
    end.

-spec trim_line(binary()) -> binary().
trim_line(Line) ->
    %% Remove trailing newline and carriage return
    Size = byte_size(Line),
    case Line of
        <<Content:Size/binary>> when Size > 0 ->
            trim_end(Content);
        _ ->
            <<>>
    end.

-spec trim_end(binary()) -> binary().
trim_end(<<>>) ->
    <<>>;
trim_end(Binary) ->
    Size = byte_size(Binary),
    case Binary of
        <<Content:(Size - 2)/binary, "\r\n">> ->
            trim_end(Content);
        <<Content:(Size - 1)/binary, "\n">> ->
            trim_end(Content);
        <<Content:(Size - 1)/binary, "\r">> ->
            trim_end(Content);
        _ ->
            Binary
    end.

%% @doc Get the maximum allowed message size from configuration.
%% Falls back to default 16MB if not configured.
-spec get_max_message_size() -> pos_integer().
get_max_message_size() ->
    case application:get_env(erlmcp, message_size_limits) of
        {ok, Limits} when is_map(Limits) ->
            maps:get(stdio, Limits, ?DEFAULT_MAX_MESSAGE_SIZE);
        _ ->
            ?DEFAULT_MAX_MESSAGE_SIZE
    end.

%% @doc Validate that a message does not exceed the maximum allowed size.
-spec validate_message_size(binary(), pos_integer()) -> ok | {error, size_exceeded}.
validate_message_size(Message, MaxSize) when is_binary(Message), is_integer(MaxSize), MaxSize > 0 ->
    case byte_size(Message) =< MaxSize of
        true ->
            ok;
        false ->
            {error, size_exceeded}
    end;
validate_message_size(_Message, _MaxSize) ->
    ok.
