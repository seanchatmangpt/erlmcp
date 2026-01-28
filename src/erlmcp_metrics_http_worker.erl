-module(erlmcp_metrics_http_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-record(state, {
    port :: pos_integer(),
    server_pid :: pid() | undefined
}).

-spec init([pos_integer()]) -> {ok, #state{}}.
init([Port]) ->
    ?LOG_INFO("Metrics HTTP Worker starting on port ~B~n", [Port]),

    % Start simple HTTP server using inets httpd
    case start_http_server(Port) of
        {ok, Pid} ->
            {ok, #state{port = Port, server_pid = Pid}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start HTTP server: ~p~n", [Reason]),
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.server_pid of
        undefined -> ok;
        Pid -> inets:stop(httpd, Pid), ok
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec start_http_server(pos_integer()) -> {ok, pid()} | {error, term()}.
start_http_server(Port) ->
    % Ensure inets is started
    application:ensure_all_started(inets),

    ConfigFile = erlang:atom_to_list(erlang:node()) ++ ".conf",
    Config = [
        {port, Port},
        {server_name, "erlmcp_metrics"},
        {server_root, "/tmp"},
        {document_root, "/tmp"},
        {modules, [erlmcp_metrics_http_handler]},
        {socket_type, ip_comm}
    ],

    case inets:start(httpd, Config) of
        {ok, Pid} ->
            ?LOG_INFO("HTTP server started with PID: ~p on port ~B~n", [Pid, Port]),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.
