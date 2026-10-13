-module(erlmcp_health_http).
-behaviour(gen_server).

%% HTTP Health Check API - 3-Level Health Checks for erlmcp v3.0.0
%%
%% Provides Kubernetes-ready health endpoints:
%%   /health         - Basic health (all checks OK)
%%   /health/ready   - Readiness probe (can accept traffic)
%%   /health/live    - Liveness probe (process is running)
%%
%% Usage:
%%   erlmcp_health_http:start_link(Port)
%%
%% Endpoints:
%%   GET /health         - Returns 200 if all checks pass, 503 otherwise
%%   GET /health/ready   - Returns 200 if ready for traffic
%%   GET /health/live    - Returns 200 if process is alive
%%   GET /health/detailed - Returns detailed health report

%% API
-export([start_link/0, start_link/1]).
-export([get_health_status/0, get_readiness_status/0, get_liveness_status/0]).
-export([set_ready/1, set_alive/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 9090).
-define(HEALTH_CHECK_TIMEOUT, 5000).

%% Health status records
-record(state, {
    port :: integer(),
    listener :: pid() | undefined,
    ready :: boolean(),
    alive :: boolean(),
    last_check :: erlang:timestamp(),
    cache :: map(),
    cache_ttl :: integer()
}).

-record(health_response, {
    status :: ok | error | degraded,
    timestamp :: binary(),
    uptime :: non_neg_integer(),
    checks :: map()
}).

%%%===================================================================
%%% Types
%%%===================================================================

-type state() :: #state{}.
-type health_response() :: #health_response{}.

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_PORT).

-spec start_link(Port :: integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) when is_integer(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

-spec get_health_status() -> map().
get_health_status() ->
    gen_server:call(?SERVER, get_health_status, ?HEALTH_CHECK_TIMEOUT).

-spec get_readiness_status() -> map().
get_readiness_status() ->
    gen_server:call(?SERVER, get_readiness_status, ?HEALTH_CHECK_TIMEOUT).

-spec get_liveness_status() -> map().
get_liveness_status() ->
    gen_server:call(?SERVER, get_liveness_status, ?HEALTH_CHECK_TIMEOUT).

-spec set_ready(Ready :: boolean()) -> ok.
set_ready(Ready) when is_boolean(Ready) ->
    gen_server:call(?SERVER, {set_ready, Ready}).

-spec set_alive(Alive :: boolean()) -> ok.
set_alive(Alive) when is_boolean(Alive) ->
    gen_server:call(?SERVER, {set_alive, Alive}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([integer()]) -> {ok, state()} | {stop, term()}.
init([Port]) ->
    process_flag(trap_exit, true),

    % Start HTTP listener using inets (simpler than Cowboy for health checks)
    case start_http_server(Port) of
        {ok, Pid} ->
            {ok, #state{
                port = Port,
                listener = Pid,
                ready = true,
                alive = true,
                last_check = erlang:timestamp(),
                cache = #{},
                cache_ttl = 5000
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(get_health_status, _From, State) ->
    Response = generate_health_response(State),
    {reply, Response, State};

handle_call(get_readiness_status, _From, State) ->
    Response = generate_readiness_response(State),
    {reply, Response, State};

handle_call(get_liveness_status, _From, State) ->
    Response = generate_liveness_response(State),
    {reply, Response, State};

handle_call({set_ready, Ready}, _From, State) ->
    {reply, ok, State#state{ready = Ready}};

handle_call({set_alive, Alive}, _From, State) ->
    {reply, ok, State#state{alive = Alive}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{listener = Pid}) when is_pid(Pid) ->
    catch inets:stop(httpd, Pid),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec start_http_server(Port :: integer()) -> {ok, pid()} | {error, term()}.
start_http_server(Port) ->
    % Use inets httpd for simple health check endpoints
    % This is lightweight and doesn't require additional dependencies
    inets:start(),

    ServerRoot = code:priv_dir(erlmcp_new_features),
    DataDir = filename:join([ServerRoot, "data"]),
    LogDir = filename:join([ServerRoot, "logs"]),

    % Ensure directories exist
    filelib:ensure_path(LogDir),

    % Configure HTTP server with health check handlers
    Config = [
        {port, Port},
        {server_name, "erlmcp-health"},
        {server_root, ServerRoot},
        {document_root, DataDir},
        {directory_index, ["index.html"]},
        {error_log, {file, filename:join([LogDir, "error.log"])}},
        {security_log, {file, filename:join([LogDir, "security.log"])}},
        {transfer_log, {file, filename:join([LogDir, "access.log"])}},
        {modules, [
            mod_alias,
            mod_auth,
            mod_esi,
            mod_actions,
            mod_dir,
            mod_get,
            mod_head,
            mod_log,
            mod_disk_log
        ]},
        {erl_script_alias, {"/health", [erlmcp_health_http_handler]}}
    ],

    case inets:start(httpd, Config) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid};
        Error ->
            Error
    end.

-spec generate_health_response(state()) -> map().
generate_health_response(State) ->
    % Run all health checks
    BaseHealth = erlmcp_health_check:health_check(),
    OverallHealth = case BaseHealth of
        #{status := ok} = Map -> Map#{overall_status => ok};
        #{status := warning} = Map -> Map#{overall_status => degraded};
        #{status := error} = Map -> Map#{overall_status => error};
        Map -> Map#{overall_status => unknown}
    end,

    OverallHealth#{
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        port => State#state.port,
        uptime => uptime()
    }.

-spec generate_readiness_response(state()) -> map().
generate_readiness_response(State) ->
    % Readiness: can we accept traffic?
    Ready = State#state.ready,
    {Running, _} = rpc:statistics(?MODULE),

    #{
        ready => Ready andalso Running > 0,
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        port => State#state.port,
        checks => #{
            process_running => Running > 0,
            flag_ready => Ready
        }
    }.

-spec generate_liveness_response(state()) -> map().
generate_liveness_response(State) ->
    % Liveness: is the process alive?
    Alive = State#state.alive,

    #{
        alive => Alive,
        timestamp => timestamp_to_iso8601(erlang:timestamp()),
        uptime => uptime(),
        process_info => #{
            memory_total => erlang:memory(total),
            process_count => erlang:system_info(process_count),
            node => node()
        }
    }.

-spec timestamp_to_iso8601(erlang:timestamp()) -> binary().
timestamp_to_iso8601({MegaSecs, Secs, MicroSecs}) ->
    {{Year, Month, Day}, {Hour, Min, SecVal}} = calendar:now_to_universal_time({MegaSecs, Secs, MicroSecs}),
    MicroSecPart = integer_to_binary(MicroSecs div 1000),
    iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~sZ",
                                   [Year, Month, Day, Hour, Min, SecVal, MicroSecPart])).

-spec uptime() -> non_neg_integer().
uptime() ->
    erlang:monotonic_time(millisecond).

%%%===================================================================
%%% HTTP Handler Module (mod_esi callback)
%%%===================================================================

% This module handles HTTP requests for health checks
% It's designed to work with inets httpd's mod_si
