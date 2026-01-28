%%%====================================================================
%%% erlmcp_pool_strategy.erl - Connection Pool Load Balancing Strategies
%%%====================================================================
%%%
%%% Behavior for custom pool selection strategies.
%%% Implements 3 default strategies:
%%% - round_robin: Circular selection
%%% - least_loaded: Select connection with fewest active requests
%%% - random: Random selection (good for uniform distribution)
%%%
%%% Usage:
%%%   Strategy = erlmcp_pool_strategy:get_strategy_module(round_robin),
%%%   {ok, ConnPid} = erlmcp_pool_strategy:select_connection(Strategy, Connections, IdleList).
%%%
%%%====================================================================

-module(erlmcp_pool_strategy).

-include("erlmcp_pool.hrl").

%% Behavior definition
-callback init() -> State :: term().
-callback select(State :: term(), Connections :: [#connection{}], IdleList :: [pid()]) ->
    {ok, pid(), NewState :: term()} | {error, term()}.
-callback name() -> atom().

%% Public API
-export([
    get_strategy_module/1,
    select_connection/3,
    strategy_name/1
]).

%% Strategy modules
-export([
    round_robin_init/0,
    round_robin_select/3,
    round_robin_name/0,

    least_loaded_init/0,
    least_loaded_select/3,
    least_loaded_name/0,

    random_init/0,
    random_select/3,
    random_name/0
]).

%% Types
-type strategy() :: {module(), State :: term()}.

-export_type([strategy/0]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Get strategy module and initial state
-spec get_strategy_module(atom()) -> strategy().
get_strategy_module(round_robin) ->
    {?MODULE, #{type => round_robin, index => 0}};
get_strategy_module(least_loaded) ->
    {?MODULE, #{type => least_loaded}};
get_strategy_module(random) ->
    {?MODULE, #{type => random}};
get_strategy_module(Strategy) when is_atom(Strategy) ->
    error({unknown_strategy, Strategy}).

%% @doc Select a connection using the strategy
-spec select_connection(strategy(), [#connection{}], [pid()]) ->
    {ok, pid()} | {error, term()}.
select_connection({?MODULE, #{type := round_robin} = State}, Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections};
        _ ->
            Index = maps:get(index, State, 0),
            SelectedPid = lists:nth((Index rem length(IdleList)) + 1, IdleList),
            {ok, SelectedPid}
    end;

select_connection({?MODULE, #{type := least_loaded}}, Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections};
        _ ->
            IdleConns = [C || C <- Connections, lists:member(C#connection.pid, IdleList)],
            case IdleConns of
                [] -> {error, no_idle_connections};
                _ ->
                    %% Select connection with lowest request count
                    LeastLoaded = lists:foldl(fun(Conn, Acc) ->
                        case Acc of
                            undefined -> Conn;
                            _ ->
                                if Conn#connection.request_count < Acc#connection.request_count ->
                                    Conn;
                                   true ->
                                    Acc
                                end
                        end
                    end, undefined, IdleConns),
                    {ok, LeastLoaded#connection.pid}
            end
    end;

select_connection({?MODULE, #{type := random}}, _Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections};
        _ ->
            RandomIndex = rand:uniform(length(IdleList)),
            SelectedPid = lists:nth(RandomIndex, IdleList),
            {ok, SelectedPid}
    end;

select_connection(Strategy, _Connections, _IdleList) ->
    {error, {invalid_strategy, Strategy}}.

%% @doc Get strategy name
-spec strategy_name(strategy()) -> atom().
strategy_name({?MODULE, #{type := Type}}) ->
    Type;
strategy_name(_) ->
    unknown.

%%====================================================================
%% Round Robin Strategy
%%====================================================================

round_robin_init() ->
    #{type => round_robin, index => 0}.

round_robin_select(State, _Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections, State};
        _ ->
            Index = maps:get(index, State, 0),
            SelectedPid = lists:nth((Index rem length(IdleList)) + 1, IdleList),
            NewState = State#{index => Index + 1},
            {ok, SelectedPid, NewState}
    end.

round_robin_name() ->
    round_robin.

%%====================================================================
%% Least Loaded Strategy
%%====================================================================

least_loaded_init() ->
    #{type => least_loaded}.

least_loaded_select(State, Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections, State};
        _ ->
            IdleConns = [C || C <- Connections, lists:member(C#connection.pid, IdleList)],
            case IdleConns of
                [] -> {error, no_idle_connections, State};
                _ ->
                    LeastLoaded = lists:foldl(fun(Conn, Acc) ->
                        case Acc of
                            undefined -> Conn;
                            _ ->
                                if Conn#connection.request_count < Acc#connection.request_count ->
                                    Conn;
                                   true ->
                                    Acc
                                end
                        end
                    end, undefined, IdleConns),
                    {ok, LeastLoaded#connection.pid, State}
            end
    end.

least_loaded_name() ->
    least_loaded.

%%====================================================================
%% Random Strategy
%%====================================================================

random_init() ->
    #{type => random}.

random_select(State, _Connections, IdleList) ->
    case IdleList of
        [] -> {error, no_idle_connections, State};
        _ ->
            RandomIndex = rand:uniform(length(IdleList)),
            SelectedPid = lists:nth(RandomIndex, IdleList),
            {ok, SelectedPid, State}
    end.

random_name() ->
    random.
