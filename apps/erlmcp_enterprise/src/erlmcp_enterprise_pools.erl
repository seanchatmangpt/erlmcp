%% @doc Enterprise Connection Pool
%% Manages connection pools for enterprise services
-module(erlmcp_enterprise_pools).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([get_connection/1, release_connection/2, pool_status/1, create_pool/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(connection, {
    pid :: pid(),
    type :: atom(),
    created :: integer(),
    last_used :: integer(),
    in_use :: boolean()
}).

-record(state, {
    pools :: map(),  % Type -> {Size, Connections}
    active_connections :: map(),  % Pid -> Connection
    config :: map()  % Pool configuration
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec get_connection(atom()) -> {ok, pid()} | {error, term()}.
get_connection(Type) ->
    gen_server:call(?MODULE, {get_connection, Type}).

-spec release_connection(atom(), pid()) -> ok.
release_connection(Type, Pid) ->
    gen_server:call(?MODULE, {release_connection, Type, Pid}).

-spec pool_status(atom()) -> {ok, map()} | {error, not_found}.
pool_status(Type) ->
    gen_server:call(?MODULE, {pool_status, Type}).

-spec create_pool(atom(), map()) -> ok.
create_pool(Type, Config) ->
    gen_server:call(?MODULE, {create_pool, Type, Config}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize pools map
    Pools = #{},

    %% Initialize active connections map
    ActiveConnections = #{},

    %% Default configuration
    DefaultConfig = #{
        pool_size => 10,
        max_retries => 3,
        retry_interval => 1000,
        idle_timeout => 300000,  % 5 minutes
        health_check_interval => 60000  % 1 minute
    },

    State = #state{
        pools = Pools,
        active_connections = ActiveConnections,
        config = DefaultConfig
    },

    %% Start connection maintenance timer
    erlang:send_after(60000, self(), maintain_connections),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({get_connection, Type}, _From, State) ->
    case get_connection_from_pool(Type, State) of
        {ok, Connection} ->
            {reply, {ok, Connection#pid}, State};
        {error, Reason} ->
            case create_new_connection(Type, State) of
                {ok, Connection} ->
                    {reply, {ok, Connection#pid}, State};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({release_connection, Type, Pid}, _From, State) ->
    case release_connection_to_pool(Type, Pid, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({pool_status, Type}, _From, State) ->
    case maps:find(Type, State#state.pools) of
        {ok, {Size, Connections}} ->
            Status = #{
                type => Type,
                size => Size,
                available => length([C || C <- Connections, not C#connection.in_use]),
                total => length(Connections),
                active => maps:size(State#state.active_connections)
            },
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({create_pool, Type, Config}, _From, State) ->
    case create_connection_pool(Type, Config, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info(maintain_connections, State) ->
    %% Perform connection maintenance
    NewState = perform_maintenance(State),

    %% Schedule next maintenance
    erlang:send_after(State#state.config#{health_check_interval}, self(), maintain_connections),

    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, State) ->
    %% Handle connection crashes
    case maps:find(Pid, State#state.active_connections) of
        {ok, Connection} ->
            ct:warning("Connection ~p crashed: ~p", [Pid, Reason]),
            %% Remove from active connections
            NewActiveConnections = maps:remove(Pid, State#state.active_connections),

            %% Remove from pool
            NewPools = case maps:find(Connection#connection.type, State#state.pools) of
                {ok, {Size, Connections}} ->
                    UpdatedConnections = lists:filter(fun(C) -> C#connection.pid =/= Pid end, Connections),
                    State#state.pools#{Connection#connection.type => {Size, UpdatedConnections}};
                error ->
                    State#state.pools
            end,

            {noreply, State#state{
                active_connections = NewActiveConnections,
                pools = NewPools
            }};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Close all connections
    maps:foreach(fun(_Type, {Size, Connections}) ->
        lists:foreach(fun(Connection) ->
            case Connection#connection.pid of
                undefined -> ok;
                Pid -> exit(Pid, shutdown)
            end
        end, Connections)
    end, State#state.pools),

    %% Close active connections
    maps:foreach(fun(_Pid, Connection) ->
        case Connection#connection.pid of
            undefined -> ok;
            Pid -> exit(Pid, shutdown)
        end
    end, State#state.active_connections).

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_connection_from_pool(atom(), #state{}) -> {ok, #connection{}} | {error, term()}.
get_connection_from_pool(Type, State) ->
    case maps:find(Type, State#state.pools) of
        {ok, {Size, Connections}} ->
            %% Find available connection
            case lists:filter(fun(C) -> not C#connection.in_use end, Connections) of
                [Connection | _] ->
                    %% Mark as in use
                    UpdatedConnection = Connection#connection{
                        in_use = true,
                        last_used = erlang:system_time(millisecond)
                    },
                    UpdatedConnections = lists:map(fun(C) ->
                        case C#connection.pid of
                            Connection#connection.pid -> UpdatedConnection;
                            _ -> C
                        end
                    end, Connections),

                    %% Update active connections
                    ActiveConnections = State#state.active_connections#{
                        Connection#connection.pid => UpdatedConnection
                    },

                    {ok, UpdatedConnection};
                [] ->
                    {error, no_available_connections}
            end;
        error ->
            {error, pool_not_found}
    end.

-spec release_connection_to_pool(atom(), pid(), #state{}) -> {ok, #state{}} | {error, term()}.
release_connection_to_pool(Type, Pid, State) ->
    case maps:find(Pid, State#state.active_connections) of
        {ok, Connection} ->
            %% Mark as not in use
            UpdatedConnection = Connection#connection{in_use = false},
            UpdatedConnections = lists:map(fun(C) ->
                case C#connection.pid of
                    Pid -> UpdatedConnection;
                    _ -> C
                end
            end, case maps:find(Type, State#state.pools) of
                {ok, {Size, Cs}} -> Cs;
                error -> []
            end),

            %% Remove from active connections
            NewActiveConnections = maps:remove(Pid, State#state.active_connections),

            %% Update pool
            NewPools = case maps:find(Type, State#state.pools) of
                {ok, {Size, _}} ->
                    State#state.pools#{Type => {Size, UpdatedConnections}};
                error ->
                    State#state.pools
            end,

            {ok, State#state{
                pools = NewPools,
                active_connections = NewActiveConnections
            }};
        error ->
            {error, connection_not_found}
    end.

-spec create_new_connection(atom(), #state{}) -> {ok, #connection{}} | {error, term()}.
create_new_connection(Type, State) ->
    case maps:find(Type, State#state.pools) of
        {ok, {Size, Connections}} when length(Connections) < Size ->
            %% Create new connection
            case start_connection_process(Type) of
                {ok, Pid} ->
                    Connection = #connection{
                        pid = Pid,
                        type = Type,
                        created = erlang:system_time(millisecond),
                        last_used = erlang:system_time(millisecond),
                        in_use = true
                    },

                    %% Add to active connections
                    ActiveConnections = State#state.active_connections#{Pid => Connection},

                    %% Add to pool
                    NewConnections = [Connection | Connections],
                    NewPools = State#state.pools#{Type => {Size, NewConnections}},

                    {ok, Connection};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, pool_full}
    end.

-spec create_connection_pool(atom(), map(), #state{}) -> {ok, #state{}} | {error, term()}.
create_connection_pool(Type, Config, State) ->
    Size = maps:get(pool_size, Config, 10),

    %% Create initial connections
    Connections = lists:foldl(fun(_I, Acc) ->
        case start_connection_process(Type) of
            {ok, Pid} ->
                Connection = #connection{
                    pid = Pid,
                    type = Type,
                    created = erlang:system_time(millisecond),
                    last_used = erlang:system_time(millisecond),
                    in_use = false
                },
                [Connection | Acc];
            {error, _} ->
                Acc
        end
    end, [], lists:seq(1, Size)),

    %% Update pools
    NewPools = State#state.pools#{Type => {Size, Connections}},

    {ok, State#state{pools = NewPools}}.

-spec start_connection_process(atom()) -> {ok, pid()} | {error, term()}.
start_connection_process(identity) ->
    %% Start identity connection
    case erlmcp_identity_connection:start() of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end;
start_connection_process(monitoring) ->
    %% Start monitoring connection
    case erlmcp_monitoring_connection:start() of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end;
start_connection_process(logging) ->
    %% Start logging connection
    case erlmcp_logging_connection:start() of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end;
start_connection_process(service_bus) ->
    %% Start service bus connection
    case erlmcp_servicebus_connection:start() of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end;
start_connection_process(data_warehouse) ->
    %% Start data warehouse connection
    case erlmcp_data_connection:start() of
        {ok, Pid} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end;
start_connection_process(_) ->
    {error, unknown_connection_type}.

-spec perform_maintenance(#state{}) -> #state{}.
perform_maintenance(State) ->
    CurrentTime = erlang:system_time(millisecond),
    IdleTimeout = State#state.config#{idle_timeout},

    %% Clean up idle connections
    NewPools = maps:fold(fun(Type, {Size, Connections}, Acc) ->
        FilteredConnections = lists:filter(fun(Connection) ->
            case Connection#connection.in_use of
                true -> true;
                false ->
                    %% Check if connection has been idle too long
                    (CurrentTime - Connection#connection.last_used) < IdleTimeout
            end
        end, Connections),

        %% Replenish pool if needed
        case length(FilteredConnections) of
            CurrentSize when CurrentSize < Size ->
                Replenished = lists:foldl(fun(_I, Acc1) ->
                    case create_new_connection(Type, State) of
                        {ok, Conn} -> [Conn | Acc1];
                        {error, _} -> Acc1
                    end
                end, FilteredConnections, lists:seq(1, Size - CurrentSize)),
                Acc#{Type => {Size, Replenished}};
            _ ->
                Acc#{Type => {Size, FilteredConnections}}
        end
    end, #{}, State#state.pools),

    State#state{pools = NewPools}.