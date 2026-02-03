%% @doc Enterprise Logging Adapter
%% Integrates with ELK, Graylog, Sumo Logic for centralized logging
-module(erlmcp_logging_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    platform :: elasticsearch | graylog | sumo_logic,
    config :: map(),
    connection :: pid() | undefined,
    buffer :: queue:queue(),
    seq_id :: integer()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Platform = maps:get(platform, Config),
    gen_server:start_link({local, logging_name(Platform)}, ?MODULE, [Platform, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Platform, Config]) ->
    process_flag(trap_exit, true),
    State = #state{platform = Platform, config = Config, buffer = queue:new(), seq_id = 1},

    %% Initialize connection
    case init_logging_connection(Platform, Config) of
        {ok, Connection} ->
            %% Start flush timer
            erlang:send_after(1000, self(), flush_timer),
            {ok, State#state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(log_event, {From, Event}, State) ->
    case send_log_event(State, Event) of
        ok ->
            SeqId = State#state.seq_id + 1,
            Metrics = update_metric(State#state.metrics, events_sent, 1),
            {reply, ok, State#state{seq_id = SeqId, metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_index, {From, IndexConfig}, State) ->
    case create_log_index(State, IndexConfig) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(search_logs, {From, Query}, State) ->
    case search_logs(State, Query) of
        {ok, Results} ->
            {reply, {ok, Results}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_log_stats, _From, State) ->
    Stats = #{
        total_logs => maps:get(events_sent, State#state.metrics, 0),
        buffer_size => queue:len(State#state.buffer),
        platform => State#state.platform
    },
    {reply, {ok, Stats}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({log, Event}, State) ->
    Buffer = queue:in(Event, State#state.buffer),
    Metrics = update_metric(State#state.metrics, events_queued, 1),
    {noreply, State#state{buffer = Buffer, metrics = Metrics}};

handle_cast({batch_log, Events}, State) ->
    Buffer = lists:foldl(fun(Event, Acc) -> queue:in(Event, Acc) end, State#state.buffer, Events),
    Metrics = update_metric(State#state.metrics, events_queued, length(Events)),
    {noreply, State#state{buffer = Buffer, metrics = Metrics}};

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_logging(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(flush_timer, State) ->
    case flush_buffer(State) of
        {ok, NewState} ->
            %% Schedule next flush
            erlang:send_after(1000, self(), flush_timer),
            {noreply, NewState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to flush buffer: ~p", [Reason]),
            %% Schedule next flush despite error
            erlang:send_after(5000, self(), flush_timer),
            {noreply, State}
    end;

handle_info({connection_lost, Platform}, State) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [Platform]),
    case reconnect_logging(State, State#state.config) of
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
    %% Flush remaining buffer
    _ = flush_buffer(State),
    case State#state.connection of
        undefined -> ok;
        Connection -> erlmcp_logging_connection:close(Connection)
    end.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec logging_name(atom()) -> atom().
logging_name(Platform) ->
    list_to_atom("logging_" ++ atom_to_list(Platform) ++ "_adapter").

-spec init_logging_connection(atom(), map()) -> {ok, pid()} | {error, term()}.
init_logging_connection(elasticsearch, Config) ->
    erlmcp_elasticsearch_client:start(Config);
init_logging_connection(graylog, Config) ->
    erlmcp_graylog_client:start(Config);
init_logging_connection(sumo_logic, Config) ->
    erlmcp_sumologic_client:start(Config).

-spec send_log_event(#state{}, map()) -> ok | {error, term()}.
send_log_event(State, Event) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                elasticsearch -> erlmcp_elasticsearch_client:log(Connection, Event);
                graylog -> erlmcp_graylog_client:log(Connection, Event);
                sumo_logic -> erlmcp_sumologic_client:log(Connection, Event)
            end
    end.

-spec create_log_index(#state{}, map()) -> ok | {error, term()}.
create_log_index(State, IndexConfig) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                elasticsearch -> erlmcp_elasticsearch_client:create_index(Connection, IndexConfig);
                graylog -> erlmcp_graylog_client:create_index(Connection, IndexConfig);
                sumo_logic -> erlmcp_sumologic_client:create_index(Connection, IndexConfig)
            end
    end.

-spec search_logs(#state{}, map()) -> {ok, [map()]} | {error, term()}.
search_logs(State, Query) ->
    case State#state.connection of
        undefined ->
            {error, not_connected};
        Connection ->
            case State#state.platform of
                elasticsearch -> erlmcp_elasticsearch_client:search(Connection, Query);
                graylog -> erlmcp_graylog_client:search(Connection, Query);
                sumo_logic -> erlmcp_sumologic_client:search(Connection, Query)
            end
    end.

-spec flush_buffer(#state{}) -> {ok, #state{}} | {error, term()}.
flush_buffer(State) ->
    case queue:out(State#state.buffer) of
        {{value, Event}, NewBuffer} ->
            case send_log_event(State, Event) of
                ok ->
                    flush_buffer(State#state{buffer = NewBuffer});
                {error, Reason} ->
                    %% Put event back and stop
                    {error, Reason}
            end;
        {empty, _} ->
            ok
    end.

-spec reconnect_logging(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_logging(State, NewConfig) ->
    case State#state.connection of
        undefined ->
            case init_logging_connection(State#state.platform, NewConfig) of
                {ok, Connection} ->
                    {ok, State#state{connection = Connection}};
                {error, Reason} ->
                    {error, Reason}
            end;
        OldConnection ->
            erlmcp_logging_connection:close(OldConnection),
            case init_logging_connection(State#state.platform, NewConfig) of
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