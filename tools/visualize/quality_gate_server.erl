%%%-----------------------------------------------------------------------------
%%% @doc Quality Gates Visualization Server
%%% Simple Cowboy-based HTTP + WebSocket server for real-time quality gate monitoring.
%%% Serves HTML dashboard on port 9091 and provides WebSocket updates.
%%%
%%% Usage:
%%%   1. Start: quality_gate_server:start().
%%%   2. Open: http://localhost:9091/
%%%   3. Updates broadcast automatically when gate status changes
%%%
%%% TCPS Integration:
%%%   - Monitors all 8 quality gates (Jidoka stages)
%%%   - Real-time status updates via WebSocket
%%%   - Historical pass rate tracking
%%%   - Mobile-responsive dashboard
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(quality_gate_server).
-behaviour(gen_server).

%% API
-export([start/0, start_link/0, stop/0]).
-export([update_gate_status/1, get_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Cowboy handlers
-export([init_http/2, websocket_init/1, websocket_handle/2,
         websocket_info/2, terminate_websocket/3]).

-record(state, {
    gates = [] :: list(),
    history = [] :: list(),
    ws_pids = [] :: list(pid()),
    start_time :: erlang:timestamp()
}).

-define(SERVER, ?MODULE).
-define(PORT, 9091).
-define(MAX_HISTORY, 10).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start_link().

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

-spec update_gate_status(map()) -> ok.
update_gate_status(GateStatus) ->
    gen_server:cast(?SERVER, {update_gate, GateStatus}).

-spec get_status() -> map().
get_status() ->
    gen_server:call(?SERVER, get_status).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init([]) ->
    %% Start Cowboy HTTP server
    case start_cowboy() of
        ok ->
            %% Initialize with default gates
            Gates = initialize_gates(),
            io:format("[Quality Gate Server] Started on http://localhost:~p/~n", [?PORT]),
            {ok, #state{
                gates = Gates,
                history = [],
                ws_pids = [],
                start_time = erlang:timestamp()
            }};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(get_status, _From, State) ->
    Status = build_status(State),
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_gate, GateStatus}, State) ->
    %% Update specific gate
    GateId = maps:get(id, GateStatus, undefined),
    UpdatedGates = update_gate_in_list(GateId, GateStatus, State#state.gates),

    %% Calculate overall status
    Overall = calculate_overall_status(UpdatedGates),

    %% Update history if overall status changed
    PassRate = calculate_pass_rate(UpdatedGates),
    NewHistory = update_history(PassRate, State#state.history),

    NewState = State#state{
        gates = UpdatedGates,
        history = NewHistory
    },

    %% Broadcast to all connected WebSocket clients
    broadcast_update(NewState),

    {noreply, NewState};

handle_cast({register_ws, Pid}, State) ->
    monitor(process, Pid),
    {noreply, State#state{ws_pids = [Pid | State#state.ws_pids]}};

handle_cast({unregister_ws, Pid}, State) ->
    {noreply, State#state{ws_pids = lists:delete(Pid, State#state.ws_pids)}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% WebSocket client disconnected
    {noreply, State#state{ws_pids = lists:delete(Pid, State#state.ws_pids)}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(quality_gate_http),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Cowboy HTTP Handlers
%%%=============================================================================

init_http(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    handle_http_request(Method, Path, Req0, State).

handle_http_request(<<"GET">>, <<"/">>, Req, State) ->
    %% Serve HTML dashboard
    HtmlPath = filename:join([code:priv_dir(erlmcp), "..", "tools", "visualize", "quality-gates.html"]),
    case file:read_file(HtmlPath) of
        {ok, Html} ->
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/html">>},
                Html,
                Req),
            {ok, Req2, State};
        {error, _} ->
            %% Fallback: serve inline HTML
            Html = generate_inline_html(),
            Req2 = cowboy_req:reply(200,
                #{<<"content-type">> => <<"text/html">>},
                Html,
                Req),
            {ok, Req2, State}
    end;

handle_http_request(<<"GET">>, <<"/api/quality-gates/status">>, Req, _State) ->
    %% REST API endpoint
    Status = get_status(),
    Json = jsx:encode(Status),
    Req2 = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        Json,
        Req),
    {ok, Req2, _State};

handle_http_request(_, _, Req, State) ->
    Req2 = cowboy_req:reply(404, #{}, <<"Not Found">>, Req),
    {ok, Req2, State}.

%%%=============================================================================
%%% WebSocket Handler
%%%=============================================================================

websocket_init(State) ->
    gen_server:cast(?SERVER, {register_ws, self()}),
    %% Send initial status
    Status = get_status(),
    {[{text, jsx:encode(Status)}], State}.

websocket_handle({text, Msg}, State) ->
    try jsx:decode(Msg, [return_maps]) of
        #{<<"type">> := <<"get_status">>} ->
            Status = get_status(),
            {[{text, jsx:encode(Status)}], State};
        _ ->
            {[], State}
    catch
        _:_ ->
            {[], State}
    end;

websocket_handle(_Frame, State) ->
    {[], State}.

websocket_info({update, Status}, State) ->
    {[{text, jsx:encode(Status)}], State};

websocket_info(_Info, State) ->
    {[], State}.

terminate_websocket(_Reason, _Req, _State) ->
    gen_server:cast(?SERVER, {unregister_ws, self()}),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

start_cowboy() ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", ?MODULE, http},
            {"/api/quality-gates/status", ?MODULE, http},
            {"/ws/quality-gates", ?MODULE, websocket}
        ]}
    ]),

    case cowboy:start_clear(quality_gate_http,
        [{port, ?PORT}],
        #{env => #{dispatch => Dispatch}}) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> {error, Reason}
    end.

initialize_gates() ->
    [
        #{id => 1, name => <<"Compilation">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 2, name => <<"Unit Tests">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 3, name => <<"Integration Tests">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 4, name => <<"Coverage (≥80%)">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 5, name => <<"Dialyzer">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 6, name => <<"Xref">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 7, name => <<"Format Check">>, status => <<"pending">>,
          timestamp => null, details => null},
        #{id => 8, name => <<"Benchmarks">>, status => <<"pending">>,
          timestamp => null, details => null}
    ].

update_gate_in_list(GateId, NewGate, Gates) ->
    lists:map(fun(Gate) ->
        case maps:get(id, Gate) of
            GateId ->
                maps:merge(Gate, NewGate#{timestamp => iso8601_timestamp()});
            _ ->
                Gate
        end
    end, Gates).

calculate_overall_status(Gates) ->
    Statuses = [maps:get(status, G) || G <- Gates],
    case lists:member(<<"fail">>, Statuses) of
        true -> <<"fail">>;
        false ->
            case lists:member(<<"running">>, Statuses) of
                true -> <<"running">>;
                false ->
                    case lists:all(fun(S) -> S =:= <<"pass">> end, Statuses) of
                        true -> <<"pass">>;
                        false -> <<"blocked">>
                    end
            end
    end.

calculate_pass_rate(Gates) ->
    Passed = length([G || G <- Gates, maps:get(status, G) =:= <<"pass">>]),
    Total = length(Gates),
    case Total of
        0 -> 0;
        _ -> round((Passed / Total) * 100)
    end.

update_history(PassRate, History) ->
    NewEntry = #{
        pass_rate => PassRate,
        timestamp => iso8601_timestamp()
    },
    NewHistory = [NewEntry | History],
    case length(NewHistory) > ?MAX_HISTORY of
        true -> lists:sublist(NewHistory, ?MAX_HISTORY);
        false -> NewHistory
    end.

build_status(State) ->
    #{
        gates => State#state.gates,
        overall => calculate_overall_status(State#state.gates),
        history => State#state.history,
        timestamp => iso8601_timestamp()
    }.

broadcast_update(State) ->
    Status = build_status(State),
    Msg = {update, Status},
    [Pid ! Msg || Pid <- State#state.ws_pids].

iso8601_timestamp() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Y, M, D, H, Mi, S]).

generate_inline_html() ->
    <<"<!DOCTYPE html>
<html><head><title>Quality Gates</title></head>
<body>
<h1>Quality Gates Dashboard</h1>
<p>WebSocket connection at ws://localhost:9091/ws/quality-gates</p>
<div id='status'>Loading...</div>
<script>
var ws = new WebSocket('ws://localhost:9091/ws/quality-gates');
ws.onmessage = function(e) {
    document.getElementById('status').innerHTML = '<pre>' + e.data + '</pre>';
};
</script>
</body></html>">>.
