%%%-------------------------------------------------------------------
%%% @doc Request cancellation support for long-running operations
%%%
%%% This module provides cancellation capability for in-flight MCP operations
%%% (tool calls, resource reads, etc.) per the MCP specification.
%%%
%%% == Cancellation Overview ==
%%% Cancellation allows clients to abort long-running operations before completion.
%%%
%%% == Flow ==
%%% 1. Client initiates operation (tool/call, resources/read, etc.)
%%% 2. Server returns request ID (cancellation token)
%%% 3. Client sends cancel_operation request with token
%%% 4. Server aborts operation and sends cancellation notification
%%% 5. Resources are cleaned up properly
%%%
%%% == Cancellation Tokens ==
%%% Cancellation tokens are references that uniquely identify in-flight operations.
%%% They are created when operations start and removed when operations complete
%%% or are cancelled.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cancellation).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([register/2, register/3, cancel/1, cancel/2]).
-export([check/1, is_cancelled/1]).
-export([set_cleanup_handler/2]).
-export([get_operation_info/1, list_operations/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records
-record(operation_info, {
    token :: reference(),
    pid :: pid(),
    monitor :: reference(),
    start_time :: integer(),
    client_pid :: pid(),
    operation_type :: binary(),
    reason :: term() | undefined,
    metadata :: map()
}).

%% State record
-record(state, {
    operations :: #{reference() => #operation_info{}},
    cleanup_handlers :: map()
}).

%% Type definitions
-type cancellation_token() :: reference().
-type operation_type() :: binary().
-type cancellation_reason() :: client_requested | timeout | server_shutdown | {error, term()}.
-type operation_info() :: #operation_info{}.

-export_type([cancellation_token/0, operation_type/0, cancellation_reason/0, operation_info/0]).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the cancellation manager server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Register an operation for cancellation with default operation type.
-spec register(pid(), pid()) -> cancellation_token().
register(ClientPid, OperationPid) ->
    register(ClientPid, OperationPid, <<"unknown">>).

%% @doc Register an operation for cancellation with specified operation type.
%% Returns a cancellation token that can be used to cancel the operation.
-spec register(pid(), pid(), operation_type()) -> cancellation_token().
register(ClientPid, OperationPid, OperationType)
  when is_pid(ClientPid), is_pid(OperationPid), is_binary(OperationType) ->
    Token = make_ref(),
    Monitor = erlang:monitor(process, OperationPid),
    gen_server:cast(?MODULE, {register, Token, ClientPid, OperationPid, Monitor, OperationType}),
    Token.

%% @doc Cancel an operation with default reason (client_requested).
-spec cancel(cancellation_token()) -> ok | {error, not_found}.
cancel(Token) when is_reference(Token) ->
    cancel(Token, client_requested).

%% @doc Cancel an operation with specific reason.
%% Reasons:
%%   - client_requested: Client explicitly cancelled
%%   - timeout: Operation exceeded timeout
%%   - server_shutdown: Server is shutting down
%%   - {error, Reason}: Operation failed
-spec cancel(cancellation_token(), cancellation_reason()) -> ok | {error, not_found}.
cancel(Token, Reason) when is_reference(Token) ->
    gen_server:cast(?MODULE, {cancel, Token, Reason}),
    ok.

%% @doc Check if an operation exists (not cancelled or completed).
%% Returns ok if operation is still active, {error, cancelled} if cancelled,
%% {error, not_found} if never existed or completed.
-spec check(cancellation_token()) -> ok | {error, cancelled | not_found}.
check(Token) when is_reference(Token) ->
    gen_server:call(?MODULE, {check, Token}).

%% @doc Check if an operation was cancelled.
-spec is_cancelled(cancellation_token()) -> boolean().
is_cancelled(Token) when is_reference(Token) ->
    case check(Token) of
        {error, cancelled} -> true;
        _ -> false
    end.

%% @doc Set cleanup handler for operation type.
%% Handler module must implement cleanup_operation(Token, Reason) -> ok.
-spec set_cleanup_handler(operation_type(), module()) -> ok.
set_cleanup_handler(OperationType, HandlerModule)
  when is_binary(OperationType), is_atom(HandlerModule) ->
    gen_server:call(?MODULE, {set_cleanup_handler, OperationType, HandlerModule}).

%% @doc Get detailed information about an operation.
-spec get_operation_info(cancellation_token()) ->
    {ok, map()} | {error, not_found}.
get_operation_info(Token) when is_reference(Token) ->
    gen_server:call(?MODULE, {get_operation_info, Token}).

%% @doc List all active operations.
-spec list_operations() -> [map()].
list_operations() ->
    gen_server:call(?MODULE, list_operations).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    logger:info("Starting cancellation manager server"),
    {ok, #state{
        operations = #{},
        cleanup_handlers = #{}
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({check, Token}, _From, State) ->
    Reply = case maps:get(Token, State#state.operations, undefined) of
        undefined -> {error, not_found};
        OpInfo ->
            case OpInfo#operation_info.reason of
                undefined -> ok;
                _Reason -> {error, cancelled}
            end
    end,
    {reply, Reply, State};

handle_call({get_operation_info, Token}, _From, State) ->
    Reply = case maps:get(Token, State#state.operations, undefined) of
        undefined -> {error, not_found};
        OpInfo ->
            {ok, format_operation_info(OpInfo)}
    end,
    {reply, Reply, State};

handle_call(list_operations, _From, State) ->
    Operations = maps:fold(
        fun(_Token, OpInfo, Acc) ->
            [format_operation_info(OpInfo) | Acc]
        end,
        [],
        State#state.operations
    ),
    {reply, lists:reverse(Operations), State};

handle_call({set_cleanup_handler, OpType, HandlerModule}, _From, State) ->
    Handlers = State#state.cleanup_handlers,
    {reply, ok, State#state{cleanup_handlers = Handlers#{OpType => HandlerModule}}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({register, Token, ClientPid, OperationPid, Monitor, OpType}, State) ->
    OpInfo = #operation_info{
        token = Token,
        pid = OperationPid,
        monitor = Monitor,
        start_time = erlang:system_time(millisecond),
        client_pid = ClientPid,
        operation_type = OpType,
        reason = undefined,
        metadata = #{}
    },
    logger:debug("Registered operation ~p for ~p", [Token, OpType]),
    {noreply, State#state{operations = (State#state.operations)#{Token => OpInfo}}};

handle_cast({cancel, Token, Reason}, State) ->
    case maps:get(Token, State#state.operations, undefined) of
        undefined ->
            logger:warning("Attempted to cancel unknown operation: ~p", [Token]),
            {noreply, State};
        OpInfo ->
            logger:info("Cancelling operation ~p: ~p", [Token, Reason]),

            %% Mark as cancelled (don't remove yet, allow cleanup)
            UpdatedOpInfo = OpInfo#operation_info{reason = Reason},
            NewOps = (State#state.operations)#{Token => UpdatedOpInfo},

            %% Kill operation process
            case is_process_alive(OpInfo#operation_info.pid) of
                true ->
                    exit(OpInfo#operation_info.pid, {cancelled, Reason});
                false ->
                    ok
            end,

            %% Send cancellation notification to client
            send_cancellation_notification(OpInfo#operation_info.client_pid, Token, Reason),

            %% Call cleanup handler if registered
            call_cleanup_handler(OpInfo#operation_info.operation_type, Token, Reason, State#state.cleanup_handlers),

            {noreply, State#state{operations = NewOps}}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'DOWN', Monitor, process, _Pid, Reason}, State) ->
    %% Clean up completed operations (normal or abnormal termination)
    Ops = maps:filter(fun(_Token, OpInfo) ->
        case OpInfo#operation_info.monitor of
            Monitor ->
                logger:debug("Operation ~p terminated: ~p", [OpInfo#operation_info.token, Reason]),
                false;  % Remove from operations map
            _Monitor ->
                true
        end
    end, State#state.operations),
    {noreply, State#state{operations = Ops}};

handle_info(Info, State) ->
    logger:warning("Unexpected handle_info message: ~p", [Info]),
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{operations = Ops}) ->
    logger:info("Cancellation manager terminating with ~p operations", [maps:size(Ops)]),

    %% Cancel all active operations on shutdown
    maps:foreach(fun(Token, OpInfo) ->
        case is_process_alive(OpInfo#operation_info.pid) of
            true ->
                exit(OpInfo#operation_info.pid, server_shutdown),
                send_cancellation_notification(OpInfo#operation_info.client_pid, Token, server_shutdown);
            false ->
                ok
        end
    end, Ops),

    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Send cancellation notification to client
-spec send_cancellation_notification(pid(), cancellation_token(), cancellation_reason()) -> ok.
send_cancellation_notification(ClientPid, Token, Reason) when is_pid(ClientPid) ->
    Notification = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"notifications/cancelled">>,
        <<"params">> => #{
            <<"requestId">> => format_token(Token),
            <<"reason">> => format_cancellation_reason(Reason),
            <<"timestamp">> => erlang:system_time(millisecond)
        }
    },

    %% Send notification to client process
    try
        ClientPid ! {mcp_notification, Notification},
        ok
    catch
        Class:Error:Stack ->
            logger:error("Failed to send cancellation notification: ~p:~p~n~p",
                        [Class, Error, Stack]),
            ok
    end.

%% @doc Format cancellation token for JSON encoding
-spec format_token(cancellation_token()) -> binary().
format_token(Token) when is_reference(Token) ->
    Term = erlang:term_to_binary(Token),
    binary:encode_hex(Term).

%% @doc Format cancellation reason for JSON encoding
-spec format_cancellation_reason(cancellation_reason()) -> binary().
format_cancellation_reason(client_requested) -> <<"client_requested">>;
format_cancellation_reason(timeout) -> <<"timeout">>;
format_cancellation_reason(server_shutdown) -> <<"server_shutdown">>;
format_cancellation_reason({error, Reason}) ->
    ReasonBin = case Reason of
        Binary when is_binary(Binary) -> Binary;
        Atom when is_atom(Atom) -> atom_to_binary(Atom, utf8);
        Term ->
            list_to_binary(io_lib:format("~p", [Term]))
    end,
    <<"error:", ReasonBin/binary>>.

%% @doc Call cleanup handler for operation type
-spec call_cleanup_handler(operation_type(), cancellation_token(), cancellation_reason(), map()) -> ok.
call_cleanup_handler(OperationType, Token, Reason, Handlers) ->
    case maps:get(OperationType, Handlers, undefined) of
        undefined ->
            ok;
        HandlerModule when is_atom(HandlerModule) ->
            try
                case erlang:function_exported(HandlerModule, cleanup_operation, 2) of
                    true ->
                        HandlerModule:cleanup_operation(Token, Reason);
                    false ->
                        logger:warning("Cleanup handler ~p does not export cleanup_operation/2",
                                     [HandlerModule]),
                        ok
                end
            catch
                Class:Error:Stack ->
                    logger:error("Cleanup handler failed: ~p:~p~n~p",
                                [Class, Error, Stack]),
                    ok
            end
    end.

%% @doc Format operation info for API responses
-spec format_operation_info(#operation_info{}) -> map().
format_operation_info(OpInfo) ->
    #{
        <<"token">> => format_token(OpInfo#operation_info.token),
        <<"operationType">> => OpInfo#operation_info.operation_type,
        <<"startTime">> => OpInfo#operation_info.start_time,
        <<"duration">> => erlang:system_time(millisecond) - OpInfo#operation_info.start_time,
        <<"status">> => case OpInfo#operation_info.reason of
            undefined -> <<"active">>;
            _ -> <<"cancelled">>
        end,
        <<"reason">> => case OpInfo#operation_info.reason of
            undefined -> null;
            Reason -> format_cancellation_reason(Reason)
        end
    }.
