%%%-------------------------------------------------------------------
%%% @doc SSE Transport Manager - gen_server for managing SSE listeners
%%%
%%% This module manages Cowboy HTTP listeners for SSE transport.
%%% It provides proper gen_server interface for transport lifecycle.
%%%
%%% Architecture:
%%% - gen_server manages Cowboy listener lifecycle
%%% - Tracks active SSE connections
%%% - Routes events to active connections via gproc registry
%%% - Handles cleanup and shutdown
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_sse_manager).
-behaviour(gen_server).

%% API
-export([start_link/2]).
-export([send_event/2, close_stream/1]).
-export([get_connections/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").
-include_lib("kernel/include/logger.hrl").

%% State record
-record(state, {
    transport_id :: binary(),
    port :: integer(),
    path :: binary(),
    listener_ref :: term() | undefined,
    connections :: #{binary() => term()}  % Map of SessionId -> RequestInfo
}).

%% Connection info
-record(conn_info, {
    session_id :: binary(),
    req :: term(),
    ping_timer :: reference()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start SSE transport manager
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportId, Config) ->
    gen_server:start_link(?MODULE, [TransportId, Config], []).

%% @doc Send event to SSE stream
-spec send_event(pid(), binary()) -> ok | {error, term()}.
send_event(ManagerPid, Data) when is_pid(ManagerPid), is_binary(Data) ->
    gen_server:call(ManagerPid, {send_event, Data}, 5000).

%% @doc Close SSE stream
-spec close_stream(pid()) -> ok.
close_stream(ManagerPid) when is_pid(ManagerPid) ->
    gen_server:call(ManagerPid, close_stream, 5000).

%% @doc Get active connections
-spec get_connections(pid()) -> {ok, map()}.
get_connections(ManagerPid) when is_pid(ManagerPid) ->
    gen_server:call(ManagerPid, get_connections, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([binary() | map()]) -> {ok, #state{}}.
init([TransportId, Config]) ->
    process_flag(trap_exit, true),
    Port = maps:get(port, Config, 8081),
    Path = maps:get(path, Config, "/mcp/sse"),

    ?LOG_INFO("Starting SSE transport manager: ~p on port ~p path ~p",
              [TransportId, Port, Path]),

    % Start Cowboy listener
    Dispatch = cowboy_router:compile([
        {'_', [
            {Path, erlmcp_transport_sse, [TransportId, Config]},
            {<<Path/binary, "/subscribe">>, erlmcp_transport_sse, [TransportId, Config]}
        ]}
    ]),

    case cowboy:start_clear(erlmcp_sse_listener,
                           [{port, Port}],
                           #{env => #{dispatch => Dispatch}}) of
        {ok, ListenerRef} ->
            ?LOG_INFO("SSE transport ~p started successfully", [TransportId]),
            {ok, #state{
                transport_id = TransportId,
                port = Port,
                path = Path,
                listener_ref = ListenerRef,
                connections = #{}
            }};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start SSE transport ~p: ~p", [TransportId, Reason]),
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({send_event, Data}, _From, State) ->
    % Broadcast event to all active connections
    % Connections manage their own event loops in Cowboy handlers
    % We use gproc registry to notify them
    lists:foreach(
        fun({SessionId, _ConnInfo}) ->
            % Send to Cowboy handler process via registry
            case gproc:lookup_local_name({sse_connection, SessionId}) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! {send_event, <<"message">>, Data}
            end
        end,
        maps:to_list(State#state.connections)
    ),
    {reply, ok, State};

handle_call(close_stream, _From, State) ->
    % Close all connections and stop listener
    lists:foreach(
        fun({SessionId, _ConnInfo}) ->
            case gproc:lookup_local_name({sse_connection, SessionId}) of
                undefined ->
                    ok;
                Pid when is_pid(Pid) ->
                    Pid ! close
            end
        end,
        maps:to_list(State#state.connections)
    ),

    % Stop Cowboy listener
    case State#state.listener_ref of
        undefined ->
            ok;
        ListenerRef ->
            cowboy:stop_listener(ListenerRef)
    end,

    {reply, ok, State#state{connections = #{}, listener_ref = undefined}};

handle_call(get_connections, _From, State) ->
    {reply, {ok, State#state.connections}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({register_connection, SessionId, Req}, State) ->
    ?LOG_DEBUG("Registering SSE connection: ~p", [SessionId]),
    NewConnections = maps:put(SessionId,
                              #conn_info{
                                  session_id = SessionId,
                                  req = Req,
                                  ping_timer = undefined
                              },
                              State#state.connections),
    {noreply, State#state{connections = NewConnections}};

handle_cast({unregister_connection, SessionId}, State) ->
    ?LOG_DEBUG("Unregistering SSE connection: ~p", [SessionId]),
    NewConnections = maps:remove(SessionId, State#state.connections),
    {noreply, State#state{connections = NewConnections}};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'EXIT', _Pid, Reason}, State) ->
    ?LOG_WARNING("SSE connection exited: ~p", [Reason]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    % Stop Cowboy listener
    case State#state.listener_ref of
        undefined ->
            ok;
        ListenerRef ->
            cowboy:stop_listener(ListenerRef)
    end,
    ?LOG_INFO("SSE transport manager ~p stopped", [State#state.transport_id]),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
