-module(erlmcp_notification_handler).
-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% State record
-record(state, {
    method :: binary(),
    handler :: term(),
    params :: map(),
    client_pid :: pid()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(binary(), term(), map()) -> {ok, pid()} | {error, term()}.
start_link(Method, Handler, Params) ->
    gen_server:start_link(?MODULE, [Method, Handler, Params], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([binary(), any(), map()]) -> {ok, state()}.
init([Method, Handler, Params]) ->
    %% Trap exit for graceful shutdown
    process_flag(trap_exit, true),

    %% Get client PID from params or caller
    ClientPid = case maps:get(<<"client_pid">>, Params, undefined) of
        undefined ->
            %% If not in params, assume the calling process is the client
            self();
        Pid when is_pid(Pid) ->
            Pid
    end,

    %% Initialize state and execute handler immediately
    State = #state{
        method = Method,
        handler = Handler,
        params = Params,
        client_pid = ClientPid
    },

    %% Execute the handler function
    execute_handler(State),

    %% Exit normally after execution (transient workers don't restart)
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec execute_handler(state()) -> ok.
execute_handler(#state{handler = Handler, method = Method, params = Params}) when is_function(Handler, 2) ->
    try
        Handler(Method, Params)
    catch
        Class:Reason:Stacktrace ->
            logger:error("Notification handler crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            error(handler_crashed)
    end;
execute_handler(#state{handler = {Module, Function}, method = Method, params = Params}) ->
    try
        Module:Function(Method, Params)
    catch
        Class:Reason:Stacktrace ->
            logger:error("Notification handler crashed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            error(handler_crashed)
    end;
execute_handler(#state{handler = Pid, method = Method, params = Params}) when is_pid(Pid) ->
    try
        Pid ! {sampling_request, Method, Params},
        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Notification handler send failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            error(handler_send_failed)
    end.
