%% @doc Data Warehouse Adapter
%% Integrates with Snowflake, BigQuery for data warehousing
-module(erlmcp_data_adapter).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    warehouse :: snowflake | bigquery,
    config :: map(),
    connection :: pid() | undefined,
    queries :: queue:queue(),
    connections :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    Warehouse = maps:get(warehouse, Config),
    gen_server:start_link({local, data_name(Warehouse)}, ?MODULE, [Warehouse, Config], []).

-spec stop(Ref :: pid() | atom()) -> ok.
stop(Ref) ->
    gen_server:stop(Ref).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom(), map()]) -> {ok, #state{}} | {stop, term()}.
init([Warehouse, Config]) ->
    process_flag(trap_exit, true),
    State = #state{warehouse = Warehouse, config = Config, queries = queue:new(), connections = #{}},

    %% Initialize connection pool
    case init_data_connections(Warehouse, Config) of
        {ok, Connections} ->
            {ok, State#state{connections = Connections}};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(execute_query, {From, Query}, State) ->
    QueryId = generate_query_id(),
    case execute_query(State, QueryId, Query) of
        {ok, Results} ->
            Metrics = update_metric(State#state.metrics, queries_executed, 1),
            {reply, {ok, QueryId, Results}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(execute_batch, {From, Queries}, State) ->
    BatchId = generate_batch_id(),
    case execute_batch(State, BatchId, Queries) of
        {ok, Results} ->
            Metrics = update_metric(State#state.metrics, batches_executed, 1),
            {reply, {ok, BatchId, Results}, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_query_status, {From, QueryId}, State) ->
    case get_query_status(State, QueryId) of
        {ok, Status} ->
            {reply, {ok, Status}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_query_results, {From, QueryId}, State) ->
    case get_query_results(State, QueryId) of
        {ok, Results} ->
            {reply, {ok, Results}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(create_table, {From, TableConfig}, State) ->
    case create_table(State, TableConfig) of
        ok ->
            Metrics = update_metric(State#state.metrics, tables_created, 1),
            {reply, ok, State#state{metrics = Metrics}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(load_data, {From, LoadConfig}, State) ->
    case load_data(State, LoadConfig) of
        {ok, JobId} ->
            Metrics = update_metric(State#state.metrics, data_loaded, 1),
            {reply, {ok, JobId}, State#state{metrics = Metrics};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_tables, _From, State) ->
    case list_tables(State) of
        {ok, Tables} ->
            {reply, {ok, Tables}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(get_database_info, _From, State) ->
    case get_database_info(State) of
        {ok, Info} ->
            {reply, {ok, Info}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({queue_query, Query}, State) ->
    Queries = queue:in(Query, State#state.queries),
    {noreply, State#state{queries = Queries}};

handle_cast({cancel_query, QueryId}, State) ->
    case cancel_query(State, QueryId) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            ?LOG_ERROR("Failed to cancel query ~p: ~p", [QueryId, Reason]),
            {noreply, State}
    end;

handle_cast({update_config, NewConfig}, State) ->
    case reconnect_data(State, NewConfig) of
        {ok, NewState} ->
            {noreply, NewState#state{config = NewConfig}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to reconnect: ~p", [Reason]),
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(query_completed, State) ->
    %% Process completed query
    process_completed_queries(State),
    {noreply, State};

handle_info(connection_lost, Warehouse) ->
    ?LOG_WARNING("Lost connection to ~p, attempting reconnect", [Warehouse]),
    case reconnect_data(State, State#state.config) of
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
    %% Close all connections
    maps:fold(fun(_Key, Conn, Acc) ->
        erlmcp_data_connection:close(Conn),
        Acc
    end, ok, State#state.connections).

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec data_name(atom()) -> atom().
data_name(Warehouse) ->
    list_to_atom("data_" ++ atom_to_list(Warehouse) ++ "_adapter").

-spec init_data_connections(atom(), map()) -> {ok, map()} | {error, term()}.
init_data_connections(snowflake, Config) ->
    case erlmcp_snowflake_connection:start_pool(Config) of
        {ok, Connections} ->
            {ok, Connections};
        {error, Reason} ->
            {error, Reason}
    end;

init_data_connections(bigquery, Config) ->
    case erlmcp_bigquery_connection:start_pool(Config) of
        {ok, Connections} ->
            {ok, Connections};
        {error, Reason} ->
            {error, Reason}
    end.

-spec execute_query(#state(), binary(), map()) -> {ok, map()} | {error, term()}.
execute_query(State, QueryId, Query) ->
    %% Get connection from pool
    case get_connection(State) of
        {ok, Connection} ->
            case State#state.warehouse of
                snowflake -> erlmcp_snowflake_connection:execute(Connection, QueryId, Query);
                bigquery -> erlmcp_bigquery_connection:execute(Connection, QueryId, Query)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec execute_batch(#state(), binary(), [map()]) -> {ok, [map()]} | {error, term()}.
execute_batch(State, BatchId, Queries) ->
    %% Execute queries in parallel
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:execute_batch(State#state.connections, BatchId, Queries);
        bigquery -> erlmcp_bigquery_connection:execute_batch(State#state.connections, BatchId, Queries)
    end.

-spec get_query_status(#state(), binary()) -> {ok, map()} | {error, term()}.
get_query_status(State, QueryId) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:get_query_status(State#state.connections, QueryId);
        bigquery -> erlmcp_bigquery_connection:get_query_status(State#state.connections, QueryId)
    end.

-spec get_query_results(#state{}, binary()) -> {ok, map()} | {error, term()}.
get_query_results(State, QueryId) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:get_query_results(State#state.connections, QueryId);
        bigquery -> erlmcp_bigquery_connection:get_query_results(State#state.connections, QueryId)
    end.

-spec create_table(#state{}, map()) -> ok | {error, term()}.
create_table(State, TableConfig) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:create_table(State#state.connections, TableConfig);
        bigquery -> erlmcp_bigquery_connection:create_table(State#state.connections, TableConfig)
    end.

-spec load_data(#state{}, map()) -> {ok, binary()} | {error, term()}.
load_data(State, LoadConfig) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:load_data(State#state.connections, LoadConfig);
        bigquery -> erlmcp_bigquery_connection:load_data(State#state.connections, LoadConfig)
    end.

-spec list_tables(#state{}) -> {ok, [map()]} | {error, term()}.
list_tables(State) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:list_tables(State#state.connections);
        bigquery -> erlmcp_bigquery_connection:list_tables(State#state.connections)
    end.

-spec get_database_info(#state{}) -> {ok, map()} | {error, term()}.
get_database_info(State) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:get_info(State#state.connections);
        bigquery -> erlmcp_bigquery_connection:get_info(State#state.connections)
    end.

-spec cancel_query(#state{}, binary()) -> ok | {error, term()}.
cancel_query(State, QueryId) ->
    case State#state.warehouse of
        snowflake -> erlmcp_snowflake_connection:cancel_query(State#state.connections, QueryId);
        bigquery -> erlmcp_bigquery_connection:cancel_query(State#state.connections, QueryId)
    end.

-spec get_connection(#state{}) -> {ok, pid()} | {error, term()}.
get_connection(State) ->
    %% Round-robin connection selection
    Connections = maps:to_list(State#state.connections),
    case Connections of
        [] ->
            {error, no_connections};
        _ ->
            %% Simple round-robin
            Index = erlang:phash2(self()) rem length(Connections),
            {_, Connection} = lists:nth(Index + 1, Connections),
            {ok, Connection}
    end.

-spec process_completed_queries(#state{}) -> ok.
process_completed_queries(State) ->
    %% Process any completed queries in the queue
    case queue:out(State#state.queries) of
        {{value, Query}, NewQueue} ->
            case execute_query(State, generate_query_id(), Query) of
                {ok, _} ->
                    process_completed_queries(State#state{queries = NewQueue});
                {error, _} ->
                    %% Put failed query back
                    process_completed_queries(State#state{queries = queue:in(Query, NewQueue)})
            end;
        {empty, _} ->
            ok
    end.

-spec reconnect_data(#state{}, map()) -> {ok, #state{}} | {error, term()}.
reconnect_data(State, NewConfig) ->
    %% Close existing connections
    maps:fold(fun(_Key, Conn, Acc) ->
        erlmcp_data_connection:close(Conn),
        Acc
    end, ok, State#state.connections),

    %% Reinitialize connections
    case init_data_connections(State#state.warehouse, NewConfig) of
        {ok, Connections} ->
            {ok, State#state{connections = Connections}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec generate_query_id() -> binary().
generate_query_id() ->
    list_to_binary("query_" + integer_to_list(erlang:system_time(second))).

-spec generate_batch_id() -> binary().
generate_batch_id() ->
    list_to_binary("batch_" + integer_to_list(erlang:system_time(second))).

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.