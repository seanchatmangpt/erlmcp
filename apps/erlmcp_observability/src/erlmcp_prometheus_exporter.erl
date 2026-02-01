%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_prometheus_exporter - Prometheus Metrics Exporter
%%%
%%% Exports erlmcp metrics in Prometheus text format.
%%% Integrates with erlmcp_counters for lock-free metrics.
%%%
%%% HTTP Endpoint:
%%% GET /metrics - Returns Prometheus text format metrics
%%%
%%% Usage:
%%% ```
%%% % Start exporter on port 9090
%%% erlmcp_prometheus_exporter:start_link(9090).
%%%
%%% % Export metrics
%%% Metrics = erlmcp_prometheus_exporter:export().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_prometheus_exporter).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, export/0, export_with_system_metrics/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 9090).

-record(state, {port :: non_neg_integer(), start_time :: integer()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the Prometheus exporter with default port (9090)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(?DEFAULT_PORT).

%% @doc Start the Prometheus exporter with specified port
-spec start_link(non_neg_integer()) -> {ok, pid()} | {error, term()}.
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%% @doc Export metrics in Prometheus text format
-spec export() -> iolist().
export() ->
    erlmcp_counters:get_prometheus().

%% @doc Export metrics with system metrics
-spec export_with_system_metrics() -> iolist().
export_with_system_metrics() ->
    CounterMetrics = erlmcp_counters:get_prometheus(),
    SystemMetrics = export_system_metrics(),
    FlagMetrics = export_flag_metrics(),
    [CounterMetrics, "\n", SystemMetrics, "\n", FlagMetrics].

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([non_neg_integer()]) -> {ok, #state{}}.
init([Port]) ->
    ?LOG_INFO("Prometheus exporter starting on port ~p~n", [Port]),
    State = #state{port = Port, start_time = erlang:system_time(millisecond)},
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(export, _From, State) ->
    Metrics = export_with_system_metrics(),
    {reply, Metrics, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Prometheus exporter terminating~n", []),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Export system metrics in Prometheus format
-spec export_system_metrics() -> iolist().
export_system_metrics() ->
    Memory = erlang:memory(),
    ProcessCount = erlang:system_info(process_count),
    PortCount = erlang:system_info(port_count),
    Schedulers = erlang:system_info(schedulers),
    SchedulersOnline = erlang:system_info(schedulers_online),

    ["# HELP erlmcp_memory_total_bytes Total memory allocated\n",
     "# TYPE erlmcp_memory_total_bytes gauge\n",
     io_lib:format("erlmcp_memory_total_bytes ~p~n", [proplists:get_value(total, Memory)]), "\n",
     "# HELP erlmcp_memory_processes_bytes Memory used by processes\n",
     "# TYPE erlmcp_memory_processes_bytes gauge\n",
     io_lib:format("erlmcp_memory_processes_bytes ~p~n", [proplists:get_value(processes, Memory)]),
     "\n", "# HELP erlmcp_memory_system_bytes Memory used by system\n",
     "# TYPE erlmcp_memory_system_bytes gauge\n",
     io_lib:format("erlmcp_memory_system_bytes ~p~n", [proplists:get_value(system, Memory)]), "\n",
     "# HELP erlmcp_process_count Current number of processes\n",
     "# TYPE erlmcp_process_count gauge\n",
     io_lib:format("erlmcp_process_count ~p~n", [ProcessCount]), "\n",
     "# HELP erlmcp_port_count Current number of ports\n", "# TYPE erlmcp_port_count gauge\n",
     io_lib:format("erlmcp_port_count ~p~n", [PortCount]), "\n",
     "# HELP erlmcp_schedulers Total number of schedulers\n", "# TYPE erlmcp_schedulers gauge\n",
     io_lib:format("erlmcp_schedulers ~p~n", [Schedulers]), "\n",
     "# HELP erlmcp_schedulers_online Number of schedulers online\n",
     "# TYPE erlmcp_schedulers_online gauge\n",
     io_lib:format("erlmcp_schedulers_online ~p~n", [SchedulersOnline])].

%% @doc Export flag metrics in Prometheus format
-spec export_flag_metrics() -> iolist().
export_flag_metrics() ->
    Flags = erlmcp_flags:get_all(),
    ["# HELP erlmcp_accepting_connections Whether server is accepting connections (1=yes, 0=no)\n",
     "# TYPE erlmcp_accepting_connections gauge\n",
     io_lib:format("erlmcp_accepting_connections ~p~n",
                   [bool_to_int(maps:get(accepting_connections, Flags))]),
     "\n",
     "# HELP erlmcp_maintenance_mode Whether system is in maintenance mode (1=yes, 0=no)\n",
     "# TYPE erlmcp_maintenance_mode gauge\n",
     io_lib:format("erlmcp_maintenance_mode ~p~n",
                   [bool_to_int(maps:get(maintenance_mode, Flags))]),
     "\n",
     "# HELP erlmcp_shutting_down Whether system is shutting down (1=yes, 0=no)\n",
     "# TYPE erlmcp_shutting_down gauge\n",
     io_lib:format("erlmcp_shutting_down ~p~n", [bool_to_int(maps:get(shutting_down, Flags))]),
     "\n",
     "# HELP erlmcp_healthy Whether system is healthy (1=yes, 0=no)\n",
     "# TYPE erlmcp_healthy gauge\n",
     io_lib:format("erlmcp_healthy ~p~n", [bool_to_int(maps:get(healthy, Flags))])].

%% @doc Convert boolean to integer for Prometheus
-spec bool_to_int(boolean()) -> 0 | 1.
bool_to_int(true) ->
    1;
bool_to_int(false) ->
    0.

%%====================================================================
%% Utility Functions
%%====================================================================
