%%%-------------------------------------------------------------------
%%% @doc
%%% Health Check HTTP Server for Docker/Kubernetes Health Probes
%%%
%%% Dedicated HTTP server for orchestration health checks on port 8080.
%%% Provides 3-level health check hierarchy:
%%%   Level 1: GET /health  - Application health (comprehensive)
%%%   Level 2: GET /ready   - Readiness probe (ready to serve traffic)
%%%   Level 3: GET /live    - Liveness probe (process is alive)
%%%
%%% Used by Docker HEALTHCHECK, Kubernetes probes, and load balancers.
%%% This is a lightweight server separate from the dashboard (port 9090).
%%%
%%% Port: 8080 (default, configurable via ERLMCP_HEALTH_PORT env)
%%% Listener Name: erlmcp_health_http_listener
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health_http_server).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, stop/0, get_port/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_PORT, 8080).
-define(SERVER, ?MODULE).

-record(state,
        {port :: inet:port_number(),
         listener_name :: atom(),
         listener_pid :: pid() | undefined}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the health HTTP server with default port (8080)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    Port = get_health_port(),
    start_link(Port).

%% @doc Start the health HTTP server with specified port
-spec start_link(inet:port_number()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Stop the health HTTP server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Get the current port the health server is listening on
-spec get_port() -> {ok, inet:port_number()} | {error, not_started}.
get_port() ->
    gen_server:call(?SERVER, get_port).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([inet:port_number()]) -> {ok, #state{}, {continue, start_http_listener}}.
init([Port]) ->
    ?LOG_INFO("Starting health HTTP server on port ~p (async initialization)", [Port]),
    State = #state{port = Port, listener_name = health_listener_name(Port)},
    {ok, State, {continue, start_http_listener}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(get_port, _From, State) ->
    {reply, {ok, State#state.port}, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_continue(term(), #state{}) -> {noreply, #state{}}.
%% Async HTTP listener startup - doesn't block supervisor
handle_continue(start_http_listener, State) ->
    %% Configure Cowboy routes for health endpoints
    %% Using erlmcp_health_http handler for comprehensive checks
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", erlmcp_health_http, []},
            {"/ready", erlmcp_health_http, []},
            {"/live", erlmcp_health_http, []},
            {"/healthz", erlmcp_health_http, []},  % Kubernetes-style
            {"/readiness", erlmcp_health_http, []},
            {"/liveness", erlmcp_health_http, []},
            {"/metrics", erlmcp_health_http, []}
        ]}
    ]),

    %% Start Cowboy HTTP listener with unique name
    ListenerPid =
        case cowboy:start_clear(State#state.listener_name,
                                [{port, State#state.port}],
                                #{env => #{dispatch => Dispatch}})
        of
            {ok, Pid} ->
                Pid;
            {error, {already_started, Pid}} ->
                Pid
        end,

    NewState = State#state{listener_pid = ListenerPid},
    ?LOG_INFO("Health HTTP server started successfully on http://localhost:~p", [State#state.port]),
    {noreply, NewState};
handle_continue(_Continue, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.listener_name of
        undefined ->
            ok;
        ListenerName ->
            cowboy:stop_listener(ListenerName)
    end,
    ?LOG_INFO("Health HTTP server stopped"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Get health port from environment or use default
-spec get_health_port() -> inet:port_number().
get_health_port() ->
    case os:getenv("ERLMCP_HEALTH_PORT") of
        false ->
            case application:get_env(erlmcp_observability, health_port, ?DEFAULT_PORT) of
                Port when is_integer(Port) -> Port;
                _ -> ?DEFAULT_PORT
            end;
        PortStr ->
            try list_to_integer(PortStr)
            catch
                _:_ -> ?DEFAULT_PORT
            end
    end.

%% @doc Generate unique listener name for the given port
-spec health_listener_name(inet:port_number()) -> atom().
health_listener_name(Port) ->
    list_to_atom("erlmcp_health_http_listener_" ++ integer_to_list(Port)).
