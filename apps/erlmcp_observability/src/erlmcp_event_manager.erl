%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_event_manager - Decoupled Event Handling with gen_event
%%%
%%% Provides a gen_event manager for MCP notifications and events.
%%% Follows Joe Armstrong's principle of decoupling through process isolation.
%%%
%%% Benefits:
%%% - Multiple handlers for same event
%%% - Decoupled event producers and consumers
%%% - Dynamic handler add/remove
%%% - Crash isolation (one handler crash doesn't affect others)
%%%
%%% Event Types:
%%% - {tool_executed, ToolName, Duration, Result}
%%% - {resource_updated, Uri, Metadata}
%%% - {connection_state, State, Info}
%%% - {error, Category, Reason}
%%% - {request_received, Method, RequestId}
%%% - {response_sent, Method, RequestId, Duration}
%%% - {notification_sent, Method, Params}
%%% - {session_created, SessionId, Metadata}
%%% - {session_terminated, SessionId, Reason}
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_event_manager).

%% API exports
-export([start_link/0, notify/1, notify_async/1, add_handler/2, add_handler/3, delete_handler/2,
         swap_handler/3, which_handlers/0, stop/0]).

%% Type exports
-export_type([event/0, handler/0, handler_state/0]).

%%====================================================================
%% Types
%%====================================================================

-type handler() :: module().
-type handler_state() :: term().
-type event() ::
    {tool_executed, ToolName :: binary(), Duration :: non_neg_integer(), Result :: term()} |
    {resource_updated, Uri :: binary(), Metadata :: map()} |
    {connection_state, State :: connected | disconnected, Info :: map()} |
    {error, Category :: atom(), Reason :: term()} |
    {request_received, Method :: binary(), RequestId :: term()} |
    {response_sent, Method :: binary(), RequestId :: term(), Duration :: non_neg_integer()} |
    {notification_sent, Method :: binary(), Params :: map()} |
    {session_created, SessionId :: binary(), Metadata :: map()} |
    {session_terminated, SessionId :: binary(), Reason :: term()}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the event manager.
%% Registers the manager locally so it can be accessed by name.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% @doc Notify all handlers of an event synchronously.
%% Blocks until all handlers have processed the event.
%% Returns ok if all handlers succeed, {error, Reason} if any fail.
-spec notify(event()) -> ok | {error, term()}.
notify(Event) ->
    try
        gen_event:sync_notify(?MODULE, Event),
        ok
    catch
        exit:noproc ->
            %% Event manager not started - log but don't crash
            logger:warning("Event manager not started, dropping event: ~p", [Event]),
            {error, noproc};
        exit:{noproc, _} ->
            %% Event manager not started - log but don't crash (alternative format)
            logger:warning("Event manager not started, dropping event: ~p", [Event]),
            {error, noproc};
        Class:Reason:Stack ->
            logger:error("Event notification failed: ~p:~p~nStack: ~p", [Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% @doc Notify all handlers of an event asynchronously.
%% Returns immediately without waiting for handlers to process.
-spec notify_async(event()) -> ok.
notify_async(Event) ->
    try
        gen_event:notify(?MODULE, Event),
        ok
    catch
        exit:{noproc, _} ->
            %% Event manager not started - log but don't crash
            logger:warning("Event manager not started, dropping event: ~p", [Event]),
            ok;
        Class:Reason:Stack ->
            logger:error("Async event notification failed: ~p:~p~nStack: ~p",
                         [Class, Reason, Stack]),
            ok
    end.

%% @doc Add an event handler with no initial arguments.
-spec add_handler(handler(), handler_state()) -> ok | {error, term()}.
add_handler(Handler, InitialState) ->
    add_handler(Handler, InitialState, []).

%% @doc Add an event handler with initial arguments.
%% The handler's init/1 callback will be called with InitialState.
-spec add_handler(handler(), handler_state(), list()) -> ok | {error, term()}.
add_handler(Handler, InitialState, _Options) ->
    case gen_event:add_handler(?MODULE, Handler, InitialState) of
        ok ->
            logger:info("Event handler added: ~p", [Handler]),
            ok;
        {error, Reason} = Error ->
            logger:error("Failed to add event handler ~p: ~p", [Handler, Reason]),
            Error
    end.

%% @doc Delete an event handler.
%% Calls the handler's terminate/2 callback before removal.
-spec delete_handler(handler(), term()) -> ok | {error, term()}.
delete_handler(Handler, Args) ->
    case gen_event:delete_handler(?MODULE, Handler, Args) of
        ok ->
            logger:info("Event handler deleted: ~p", [Handler]),
            ok;
        {error, Reason} = Error ->
            logger:error("Failed to delete event handler ~p: ~p", [Handler, Reason]),
            Error
    end.

%% @doc Swap one handler for another.
%% Useful for hot code upgrades or changing handler implementation.
-spec swap_handler(handler(), handler(), handler_state()) -> ok | {error, term()}.
swap_handler(OldHandler, NewHandler, NewState) ->
    case gen_event:swap_handler(?MODULE, {OldHandler, []}, {NewHandler, NewState}) of
        ok ->
            logger:info("Event handler swapped: ~p -> ~p", [OldHandler, NewHandler]),
            ok;
        {error, Reason} = Error ->
            logger:error("Failed to swap handler ~p -> ~p: ~p", [OldHandler, NewHandler, Reason]),
            Error
    end.

%% @doc Get list of all registered handlers.
-spec which_handlers() -> [handler()].
which_handlers() ->
    gen_event:which_handlers(?MODULE).

%% @doc Stop the event manager.
%% Calls terminate/2 on all handlers before stopping.
-spec stop() -> ok.
stop() ->
    gen_event:stop(?MODULE).

%%====================================================================
%% Internal Functions
%%====================================================================
