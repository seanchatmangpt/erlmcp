%%%-------------------------------------------------------------------
%%% @doc
%%% Database Connection Worker for Poolboy Integration
%%%
%%% Implements poolboy_worker behaviour for database connections.
%%% Supports multiple database backends through a modular architecture.
%%%
%%% Supported backends:
%%% - PostgreSQL (via epgsql)
%%% - MySQL (via mysql)
%%% - Mock (for testing)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_db_connection).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([query/3]).
-export([begin_transaction/1]).
-export([commit/1]).
-export([rollback/1]).
-export([health_check/1]).
-export([disconnect/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Constants
-define(DEFAULT_CONNECT_TIMEOUT, 5000).
-define(DEFAULT_QUERY_TIMEOUT, 30000).
-define(HEALTH_CHECK_QUERY, "SELECT 1").

%% Records
-record(state,
        {backend :: postgres | mysql | mock,
         connection :: term(),
         config :: map(),
         in_transaction = false :: boolean()}).

-type connection() :: pid().
-type query_result() :: {ok, list(map())} | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start a new database connection worker (for poolboy)
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%% @doc Execute a query on the connection
-spec query(connection(), iodata(), list()) -> query_result().
query(Connection, Query, Params) ->
    gen_server:call(Connection, {query, Query, Params}, ?DEFAULT_QUERY_TIMEOUT).

%% @doc Begin a transaction
-spec begin_transaction(connection()) -> {ok, term()} | {error, term()}.
begin_transaction(Connection) ->
    gen_server:call(Connection, begin_transaction, ?DEFAULT_QUERY_TIMEOUT).

%% @doc Commit the current transaction
-spec commit(connection()) -> {ok, term()} | {error, term()}.
commit(Connection) ->
    gen_server:call(Connection, commit, ?DEFAULT_QUERY_TIMEOUT).

%% @doc Rollback the current transaction
-spec rollback(connection()) -> ok.
rollback(Connection) ->
    gen_server:cast(Connection, rollback).

%% @doc Perform a health check on the connection
-spec health_check(connection()) -> {ok, map()} | {error, term()}.
health_check(Connection) ->
    gen_server:call(Connection, health_check, 5000).

%% @doc Disconnect the connection
-spec disconnect(connection()) -> ok.
disconnect(Connection) ->
    gen_server:stop(Connection).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init(proplists:proplist()) -> {ok, #state{}} | {stop, term()}.
init(Args) ->
    process_flag(trap_exit, true),

    Backend = proplists:get_value(backend, Args, mock),
    Config = maps:from_list(Args),

    case connect(Backend, Config) of
        {ok, Connection} ->
            {ok, #state{
                backend = Backend,
                connection = Connection,
                config = Config
            }};
        {error, Reason} ->
            logger:error("Failed to connect to database: ~p", [Reason]),
            {stop, Reason}
    end.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({query, Query, Params}, _From, State) ->
    Result = do_query(State, Query, Params),
    {reply, Result, State};

handle_call(begin_transaction, _From, State = #state{in_transaction = true}) ->
    {reply, {error, already_in_transaction}, State};

handle_call(begin_transaction, _From, State) ->
    Result = do_begin_transaction(State),
    case Result of
        {ok, _} ->
            {reply, Result, State#state{in_transaction = true}};
        Error ->
            {reply, Error, State}
    end;

handle_call(commit, _From, State = #state{in_transaction = false}) ->
    {reply, {error, no_transaction}, State};

handle_call(commit, _From, State) ->
    Result = do_commit(State),
    {reply, Result, State#state{in_transaction = false}};

handle_call(health_check, _From, State) ->
    Result = do_health_check(State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(rollback, State = #state{in_transaction = false}) ->
    {noreply, State};

handle_cast(rollback, State) ->
    ok = do_rollback(State),
    {noreply, State#state{in_transaction = false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:warning("Connection process received EXIT: ~p", [Reason]),
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    do_disconnect(State),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Backend-specific implementations
%%%===================================================================

%% @private
-spec connect(atom(), map()) -> {ok, term()} | {error, term()}.
connect(mock, _Config) ->
    % Mock connection for testing
    {ok, #{mock => true, connected_at => erlang:timestamp()}};

connect(postgres, Config) ->
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 5432),
    Database = maps:get(database, Config, "erlmcp"),
    User = maps:get(user, Config, "postgres"),
    Password = maps:get(password, Config, ""),

    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try
                case epgsql:connect(Host, User, Password,
                                   #{database => Database,
                                     port => Port,
                                     timeout => ?DEFAULT_CONNECT_TIMEOUT}) of
                    {ok, Conn} -> {ok, Conn};
                    ConnectError -> ConnectError
                end
            catch
                _:CatchError -> {error, CatchError}
            end;
        _ ->
            logger:warning("epgsql not available, using mock connection"),
            {ok, #{mock => true, backend => postgres_mock}}
    end;

connect(mysql, Config) ->
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 3306),
    Database = maps:get(database, Config, "erlmcp"),
    User = maps:get(user, Config, "root"),
    Password = maps:get(password, Config, ""),

    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try
                case mysql:connect([Host, Port, User, Password, Database,
                                   ?DEFAULT_CONNECT_TIMEOUT]) of
                    {ok, Conn} -> {ok, Conn};
                    ConnectError -> ConnectError
                end
            catch
                _:CatchError -> {error, CatchError}
            end;
        _ ->
            logger:warning("mysql not available, using mock connection"),
            {ok, #{mock => true, backend => mysql_mock}}
    end;

connect(Backend, _Config) ->
    {error, {unsupported_backend, Backend}}.

%% @private
-spec do_query(#state{}, iodata(), list()) -> query_result().
do_query(#state{backend = mock}, Query, _Params) ->
    % Mock query execution for testing
    logger:debug("Mock query: ~s", [Query]),
    {ok, [#{
        result => ok,
        query => iolist_to_binary(Query),
        timestamp => erlang:timestamp()
    }]};

do_query(#state{backend = postgres, connection = Conn}, Query, Params) ->
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try
                case epgsql:equery(Conn, Query, Params) of
                    {ok, _Columns, Rows} ->
                        {ok, rows_to_maps(Rows)};
                    {ok, Count} when is_integer(Count) ->
                        {ok, [{count, Count}]};
                    {error, QueryError} ->
                        {error, QueryError}
                end
            catch
                _:CatchError -> {error, CatchError}
            end;
        _ ->
            {error, driver_not_available}
    end;

do_query(#state{backend = mysql, connection = Conn}, Query, Params) ->
    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try
                case mysql:execute(Conn, Query, Params) of
                    {ok, Rows} when is_list(Rows) ->
                        {ok, rows_to_maps(Rows)};
                    {ok, Count} when is_integer(Count) ->
                        {ok, [{count, Count}]};
                    {error, QueryError} ->
                        {error, QueryError}
                end
            catch
                _:CatchError -> {error, CatchError}
            end;
        _ ->
            {error, driver_not_available}
    end;

do_query(_State, _Query, _Params) ->
    {error, unsupported_backend}.

%% @private
-spec do_begin_transaction(#state{}) -> {ok, term()} | {error, term()}.
do_begin_transaction(#state{backend = mock}) ->
    {ok, transaction_started};

do_begin_transaction(#state{backend = postgres, connection = Conn}) ->
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try epgsql:begin_transaction(Conn) of
                {ok, _} -> {ok, transaction_started};
                Error -> Error
            catch
                _:Error -> {error, Error}
            end;
        _ ->
            {ok, transaction_started}
    end;

do_begin_transaction(#state{backend = mysql, connection = Conn}) ->
    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try mysql:begin_transaction(Conn) of
                {ok, _} -> {ok, transaction_started};
                Error -> Error
            catch
                _:Error -> {error, Error}
            end;
        _ ->
            {ok, transaction_started}
    end;

do_begin_transaction(_State) ->
    {error, unsupported_backend}.

%% @private
-spec do_commit(#state{}) -> {ok, term()} | {error, term()}.
do_commit(#state{backend = mock}) ->
    {ok, committed};

do_commit(#state{backend = postgres, connection = Conn}) ->
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try epgsql:commit(Conn) of
                {ok, _} -> {ok, committed};
                Error -> Error
            catch
                _:Error -> {error, Error}
            end;
        _ ->
            {ok, committed}
    end;

do_commit(#state{backend = mysql, connection = Conn}) ->
    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try mysql:commit(Conn) of
                {ok, _} -> {ok, committed};
                Error -> Error
            catch
                _:Error -> {error, Error}
            end;
        _ ->
            {ok, committed}
    end;

do_commit(_State) ->
    {error, unsupported_backend}.

%% @private
-spec do_rollback(#state{}) -> ok.
do_rollback(#state{backend = mock}) ->
    ok;

do_rollback(#state{backend = postgres, connection = Conn}) ->
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try epgsql:rollback(Conn) of
                {ok, _} -> ok;
                _ -> ok
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end;

do_rollback(#state{backend = mysql, connection = Conn}) ->
    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try mysql:rollback(Conn) of
                {ok, _} -> ok;
                _ -> ok
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end;

do_rollback(_State) ->
    ok.

%% @private
-spec do_health_check(#state{}) -> {ok, map()} | {error, term()}.
do_health_check(State) ->
    case do_query(State, ?HEALTH_CHECK_QUERY, []) of
        {ok, [_ | _] = Results} ->
            {ok, #{
                status => healthy,
                backend => State#state.backend,
                result => Results
            }};
        {ok, Result} ->
            {ok, #{
                status => healthy,
                backend => State#state.backend,
                result => Result
            }};
        {error, Reason} ->
            {error, #{
                status => unhealthy,
                backend => State#state.backend,
                reason => Reason
            }}
    end.

%% @private
-spec do_disconnect(#state{}) -> ok.
do_disconnect(#state{backend = mock}) ->
    ok;

do_disconnect(#state{backend = postgres, connection = Conn}) ->
    case code:ensure_loaded(epgsql) of
        {module, epgsql} ->
            try epgsql:close(Conn) of
                ok -> ok;
                _ -> ok
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end;

do_disconnect(#state{backend = mysql, connection = Conn}) ->
    case code:ensure_loaded(mysql) of
        {module, mysql} ->
            try mysql:close(Conn) of
                ok -> ok;
                _ -> ok
            catch
                _:_ -> ok
            end;
        _ ->
            ok
    end;

do_disconnect(_State) ->
    ok.

%% @private
-spec rows_to_maps(list()) -> list(map()).
rows_to_maps(Rows) when is_list(Rows) ->
    lists:map(fun row_to_map/1, Rows).

%% @private
-spec row_to_map(tuple() | map()) -> map().
row_to_map(Row) when is_tuple(Row) ->
    % Convert tuple to map with column names as keys
    % In production, we would get column names from the query result
    Size = tuple_size(Row),
    maps:from_list([{N, element(N, Row)} || N <- lists:seq(1, Size)]);
row_to_map(Row) when is_map(Row) ->
    Row;
row_to_map(Row) ->
    #{row => Row}.
