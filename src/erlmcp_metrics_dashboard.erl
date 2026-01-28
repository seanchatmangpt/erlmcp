%%%-------------------------------------------------------------------
%%% @doc erlmcp_metrics_dashboard - Real-time quality metrics dashboard
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_metrics_dashboard).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([get_current_metrics/0, get_trend/1, get_history/1]).
-export([update_snapshot/1, alert_on_regression/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    snapshots_dir :: string(),
    current_metrics :: map(),
    last_update :: erlang:timestamp(),
    alert_thresholds :: map(),
    subscribers :: [pid()]
}).

-define(DEFAULT_SNAPSHOTS_DIR, "metrics/snapshots").
-define(UPDATE_INTERVAL, 300000). % 5 minutes

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec get_current_metrics() -> {ok, map()} | {error, term()}.
get_current_metrics() ->
    gen_server:call(?MODULE, get_current_metrics).

-spec get_trend(Days :: pos_integer()) -> {ok, list()} | {error, term()}.
get_trend(Days) ->
    gen_server:call(?MODULE, {get_trend, Days}).

-spec get_history(Days :: pos_integer()) -> {ok, list()} | {error, term()}.
get_history(Days) ->
    gen_server:call(?MODULE, {get_history, Days}).

-spec update_snapshot(file:filename()) -> ok.
update_snapshot(SnapshotFile) ->
    gen_server:cast(?MODULE, {update_snapshot, SnapshotFile}).

-spec alert_on_regression() -> ok | {alert, list()}.
alert_on_regression() ->
    gen_server:call(?MODULE, alert_on_regression).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    SnapshotsDir = proplists:get_value(snapshots_dir, Opts, ?DEFAULT_SNAPSHOTS_DIR),
    
    % Default alert thresholds
    Thresholds = #{
        coverage_drop => 5.0,           % Alert if coverage drops >5%
        test_pass_rate_drop => 5.0,     % Alert if pass rate drops >5%
        warnings_increase => 5,         % Alert if warnings increase by >5
        quality_score_drop => 10.0      % Alert if quality score drops >10 points
    },
    
    State = #state{
        snapshots_dir = SnapshotsDir,
        current_metrics = #{},
        last_update = erlang:timestamp(),
        alert_thresholds = Thresholds,
        subscribers = []
    },
    
    % Load latest snapshot
    {ok, load_latest_snapshot(State)}.

handle_call(get_current_metrics, _From, State) ->
    {reply, {ok, State#state.current_metrics}, State};

handle_call({get_trend, Days}, _From, State) ->
    Trend = calculate_trend(Days, State),
    {reply, {ok, Trend}, State};

handle_call({get_history, Days}, _From, State) ->
    History = load_history(Days, State),
    {reply, {ok, History}, State};

handle_call(alert_on_regression, _From, State) ->
    Result = check_regressions(State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_snapshot, SnapshotFile}, State) ->
    NewState = load_snapshot(SnapshotFile, State),
    notify_subscribers(NewState),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

load_latest_snapshot(State) ->
    Dir = State#state.snapshots_dir,
    case filelib:wildcard(filename:join(Dir, "*.json")) of
        [] ->
            State;
        Files ->
            Latest = lists:last(lists:sort(Files)),
            load_snapshot(Latest, State)
    end.

load_snapshot(SnapshotFile, State) ->
    case file:read_file(SnapshotFile) of
        {ok, Binary} ->
            case jsx:decode(Binary, [return_maps]) of
                Metrics when is_map(Metrics) ->
                    State#state{
                        current_metrics = Metrics,
                        last_update = erlang:timestamp()
                    };
                _ ->
                    State
            end;
        {error, _} ->
            State
    end.

load_history(Days, State) ->
    Dir = State#state.snapshots_dir,
    Files = filelib:wildcard(filename:join(Dir, "*.json")),
    SortedFiles = lists:reverse(lists:sort(Files)),
    
    % Take last N days (assuming daily snapshots)
    RelevantFiles = lists:sublist(SortedFiles, Days),
    
    lists:filtermap(fun(File) ->
        case file:read_file(File) of
            {ok, Binary} ->
                case jsx:decode(Binary, [return_maps]) of
                    Metrics when is_map(Metrics) ->
                        {true, Metrics};
                    _ ->
                        false
                end;
            {error, _} ->
                false
        end
    end, RelevantFiles).

calculate_trend(Days, State) ->
    History = load_history(Days, State),
    
    case {History, State#state.current_metrics} of
        {[First | _], Current} when map_size(Current) > 0 ->
            #{
                test_pass_rate => calculate_metric_trend(
                    maps:get(<<"tests">>, First, #{}),
                    maps:get(<<"tests">>, Current, #{}),
                    <<"pass_rate">>
                ),
                coverage => calculate_metric_trend(
                    maps:get(<<"coverage">>, First, #{}),
                    maps:get(<<"coverage">>, Current, #{}),
                    <<"percentage">>
                ),
                quality_score => calculate_metric_trend(
                    maps:get(<<"quality_score">>, First, #{}),
                    maps:get(<<"quality_score">>, Current, #{}),
                    <<"overall">>
                ),
                compile_warnings => calculate_metric_trend(
                    maps:get(<<"compilation">>, First, #{}),
                    maps:get(<<"compilation">>, Current, #{}),
                    <<"warnings">>
                )
            };
        _ ->
            #{}
    end.

calculate_metric_trend(FirstMap, CurrentMap, Key) ->
    First = maps:get(Key, FirstMap, 0),
    Current = maps:get(Key, CurrentMap, 0),
    Delta = Current - First,
    
    PercentChange = case First of
        0 -> 0;
        _ -> (Delta / First) * 100
    end,
    
    #{
        first => First,
        current => Current,
        delta => Delta,
        percent_change => PercentChange,
        direction => case Delta > 0 of
            true -> improving;
            false when Delta < 0 -> degrading;
            _ -> stable
        end
    }.

check_regressions(State) ->
    case load_history(7, State) of
        [Previous | _] when map_size(State#state.current_metrics) > 0 ->
            Current = State#state.current_metrics,
            Thresholds = State#state.alert_thresholds,
            Alerts = check_all_regressions(Previous, Current, Thresholds),
            
            case Alerts of
                [] -> ok;
                _ -> {alert, Alerts}
            end;
        _ ->
            ok
    end.

check_all_regressions(Previous, Current, Thresholds) ->
    lists:flatten([
        check_coverage_regression(Previous, Current, Thresholds),
        check_test_pass_regression(Previous, Current, Thresholds),
        check_warnings_regression(Previous, Current, Thresholds),
        check_quality_regression(Previous, Current, Thresholds)
    ]).

check_coverage_regression(Previous, Current, Thresholds) ->
    PrevCov = maps:get(<<"percentage">>, maps:get(<<"coverage">>, Previous, #{}), 0),
    CurrCov = maps:get(<<"percentage">>, maps:get(<<"coverage">>, Current, #{}), 0),
    Threshold = maps:get(coverage_drop, Thresholds, 5.0),
    
    case PrevCov - CurrCov of
        Drop when Drop > Threshold ->
            [{coverage_drop, #{
                previous => PrevCov,
                current => CurrCov,
                drop => Drop,
                threshold => Threshold
            }}];
        _ ->
            []
    end.

check_test_pass_regression(Previous, Current, Thresholds) ->
    PrevPass = maps:get(<<"pass_rate">>, maps:get(<<"tests">>, Previous, #{}), 0),
    CurrPass = maps:get(<<"pass_rate">>, maps:get(<<"tests">>, Current, #{}), 0),
    Threshold = maps:get(test_pass_rate_drop, Thresholds, 5.0),
    
    case PrevPass - CurrPass of
        Drop when Drop > Threshold ->
            [{test_pass_rate_drop, #{
                previous => PrevPass,
                current => CurrPass,
                drop => Drop,
                threshold => Threshold
            }}];
        _ ->
            []
    end.

check_warnings_regression(Previous, Current, Thresholds) ->
    PrevWarn = maps:get(<<"warnings">>, maps:get(<<"compilation">>, Previous, #{}), 0),
    CurrWarn = maps:get(<<"warnings">>, maps:get(<<"compilation">>, Current, #{}), 0),
    Threshold = maps:get(warnings_increase, Thresholds, 5),
    
    case CurrWarn - PrevWarn of
        Increase when Increase > Threshold ->
            [{warnings_increase, #{
                previous => PrevWarn,
                current => CurrWarn,
                increase => Increase,
                threshold => Threshold
            }}];
        _ ->
            []
    end.

check_quality_regression(Previous, Current, Thresholds) ->
    PrevQual = maps:get(<<"overall">>, maps:get(<<"quality_score">>, Previous, #{}), 0),
    CurrQual = maps:get(<<"overall">>, maps:get(<<"quality_score">>, Current, #{}), 0),
    Threshold = maps:get(quality_score_drop, Thresholds, 10.0),
    
    case PrevQual - CurrQual of
        Drop when Drop > Threshold ->
            [{quality_score_drop, #{
                previous => PrevQual,
                current => CurrQual,
                drop => Drop,
                threshold => Threshold
            }}];
        _ ->
            []
    end.

notify_subscribers(State) ->
    Msg = {metrics_updated, State#state.current_metrics},
    lists:foreach(fun(Pid) ->
        Pid ! Msg
    end, State#state.subscribers).

%% Additional API
-export([stop/0]).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).
