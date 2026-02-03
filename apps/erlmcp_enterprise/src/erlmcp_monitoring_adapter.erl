%% @doc Enterprise Monitoring Adapter
%% Integrates with Splunk, Datadog, New Relic for metrics and logs
-module(erlmcp_monitoring_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    system :: splunk | datadog | new_relic,
    config :: map(),
    connection :: pid() | undefined,
    metrics :: map(),
    buffer :: queue:queue()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    System = maps:get(system, Config),
    gen_server:start_link({local, monitoring_name(System)}, ?MODULE, [System, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([System, Config]) ->
    process_flag(trap_exit, true),
    State = #state{system = System, config = Config, metrics = #{}, buffer = queue:new()},

    %% Initialize connection
    case init_monitoring_connection(System, Config) of
        {ok, Connection} ->
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(send_metric, {From, Metric}, State) ->
    case send_metric_data(State, Metric) of
        ok ->
            Metrics = update_metric(State#state.metrics, metrics_sent, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(send_log, {From, Log}, State) ->
    case send_log_data(State, Log) of
        ok ->
            Metrics = update_metric(State#state.metrics, logs_sent, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_alert, {From, Alert}, State) ->
    case create_alert(State, Alert) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_dashboard, {From, DashboardId}, State) ->
    case get_dashboard(State, DashboardId) of
        {ok, Dashboard} ->
            {reply, {ok, Dashboard}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_metrics, _From, State) ->
    {reply, State#state.metrics, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({metric, Metric}, State) ->
    Buffer = queue:in(Metric, State#state.buffer),
    case flush_buffer(State#state{buffer = Buffer}) of
        {ok, NewState} ->
            Metrics = update_metric(NewState#state.metrics, metrics_queued, 1),
            {noreply, NewState#state{metrics = Metrics}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to flush metrics: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({log, Log}, State) ->
    Buffer = queue:in(Log, State#state.buffer),
    case flush_buffer(State#state{buffer = Buffer}) of
        {ok, NewState} ->
            Metrics = update_metric(NewState#state.metrics, logs_queued, 1),
            {noreply, NewState#state{metrics = Metrics}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to flush logs: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_monitoring(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({flush_timer, _}, State) ->
    case flush_buffer(State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to flush buffer: ~p", [Reason]),
            {noreply, State}
    end;

handle_info({connection_lost, System}, State) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [System]),
    case reconnect_monitoring(State, State#state.config) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Reconnect failed: ~p", [Reason]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_monitoring_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec monitoring_name(atom()) -> atom().
monitoring_name(System) ->
    list_to_atom("monitoring_" ++ atom_to_list(System) ++ "_adapter").

-spec init_monitoring_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_monitoring_connection(splunk, Config) ->
    erlmcp_splunk_client:start(Config);
init_monitoring_connection(datadog, Config) ->
    erlmcp_datadog_client:start(Config);
init_monitoring_connection(new_relic, Config) ->
    erlmcp_newrelic_client:start(Config).

-spec send_metric_data(#state{}, map()) -> ok | {error, term()}.
send_metric_data(State, Metric) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.system of
                splunk -> erlmcp_splunk_client:send_metric(Connection, Metric);
                datadog -> erlmcp_datadog_client:send_metric(Connection, Metric);
                new_relic -> erlmcp_newrelic_client:send_metric(Connection, Metric)
            end
    end.

-spec send_log_data(#state{}, map()) -> ok | {error, term()}.
send_log_data(State, Log) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.system of
                splunk -> erlmcp_splunk_client:send_log(Connection, Log);
                datadog -> erlmcp_datadog_client:send_log(Connection, Log);
                new_relic -> erlmcp_newrelic_client:send_log(Connection, Log)
            end
    end.

-spec create_alert(#state{}, map()) -> ok | {error, term()}.
create_alert(State, Alert) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.system of
                splunk -> erlmcp_splunk_client:create_alert(Connection, Alert);
                datadog -> erlmcp_datadog_client:create_alert(Connection, Alert);
                new_relic -> erlmcp_newrelic_client:create_alert(Connection, Alert)
            end
    end.

-spec get_dashboard(#state(), binary()) -> {ok, map()} | {error, term()}.
get_dashboard(State, DashboardId) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.system of
                splunk -> erlmcp_splunk_client:get_dashboard(Connection, DashboardId);
                datadog -> erlmcp_datadog_client:get_dashboard(Connection, DashboardId);
                new_relic -> erlmcp_newrelic_client:get_dashboard(Connection, DashboardId)
            end
    end.

-spec flush_buffer(#state{}) -> {ok, #state{}} | {error, term()}.
flush_buffer(State) ->
    case queue:out(State#state.buffer) of
        {{value, Data}, NewBuffer} ->
            case send_batch_data(State#state.system, Data) of
                ok ->
                    {ok, State#state{buffer = NewBuffer}};
                {error, Reason} ->
                    {error, Reason}
            end;
        {empty, _} ->
            ok
    end.

-spec send_batch_data(atom(), term()) -> ok | {error, term()}.
send_batch_data(splunk, Data) ->
    erlmcp_splunk_client:send_batch(Data);
send_batch_data(datadog, Data) ->
    erlmcp_datadog_client:send_batch(Data);
send_batch_data(new_relic, Data) ->
    erlmcp_newrelic_client:send_batch(Data).

-spec reconnect_monitoring(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_monitoring(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_monitoring_connection(State#state.system, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_monitoring_connection:close(OldConnection),
            case init_monitoring_connection(State#state.system, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.