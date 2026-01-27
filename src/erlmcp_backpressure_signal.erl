%% @doc Backpressure Signaling for MCP High-Performance Queue System
-module(erlmcp_backpressure_signal).

-export([
    new/2,
    check_and_signal/2,
    set_warning_threshold/2, set_critical_threshold/2,
    get_status/1,
    reset_metrics/1
]).

-type conn_id() :: term().
-type backpressure_state() :: {conn_id, non_neg_integer(), non_neg_integer(), integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.

-export_type([backpressure_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec new(conn_id(), non_neg_integer()) -> backpressure_state().
new(ConnId, QueueCapacity) when is_integer(QueueCapacity), QueueCapacity > 0 ->
    WarningThreshold = round(QueueCapacity * 0.80),
    CriticalThreshold = round(QueueCapacity * 0.90),
    {
        ConnId,
        WarningThreshold,
        CriticalThreshold,
        erlang:timestamp(),
        0,
        0,
        0
    }.

-spec check_and_signal(non_neg_integer(), backpressure_state()) ->
    {ok, backpressure_state()} |
    {backpressure, warning | critical | severe, backpressure_state()}.
check_and_signal(CurrentDepth, State) ->
    {ConnId, WarningThreshold, CriticalThreshold, _, SignalCount, PeakDepth, _LastDepth} = State,

    NewPeakDepth = max(CurrentDepth, PeakDepth),
    NewState = {ConnId, WarningThreshold, CriticalThreshold, erlang:timestamp(), SignalCount, NewPeakDepth, CurrentDepth},

    case CurrentDepth of
        D when D >= CriticalThreshold ->
            case D >= (CriticalThreshold + 50) of
                true ->
                    NewState2 = increment_signal_count(NewState),
                    {backpressure, severe, NewState2};
                false ->
                    NewState2 = increment_signal_count(NewState),
                    {backpressure, critical, NewState2}
            end;
        D when D >= WarningThreshold ->
            NewState2 = increment_signal_count(NewState),
            {backpressure, warning, NewState2};
        _ ->
            {ok, NewState}
    end.

-spec set_warning_threshold(non_neg_integer(), backpressure_state()) -> backpressure_state().
set_warning_threshold(PercentageThreshold, State) when
    is_integer(PercentageThreshold), PercentageThreshold >= 0, PercentageThreshold =< 100
->
    {ConnId, _OldThreshold, CriticalThreshold, LastSignalTime, SignalCount, PeakDepth, LastDepth} = State,
    {ConnId, PercentageThreshold, CriticalThreshold, LastSignalTime, SignalCount, PeakDepth, LastDepth}.

-spec set_critical_threshold(non_neg_integer(), backpressure_state()) -> backpressure_state().
set_critical_threshold(PercentageThreshold, State) when
    is_integer(PercentageThreshold), PercentageThreshold >= 0, PercentageThreshold =< 100
->
    {ConnId, WarningThreshold, _OldCriticalThreshold, LastSignalTime, SignalCount, PeakDepth, LastDepth} = State,
    {ConnId, WarningThreshold, PercentageThreshold, LastSignalTime, SignalCount, PeakDepth, LastDepth}.

-spec get_status(backpressure_state()) -> map().
get_status(State) ->
    {ConnId, WarningThreshold, CriticalThreshold, _LastSignalTime, SignalCount, PeakDepth, LastDepth} = State,

    PressureLevel = case LastDepth of
        D when D >= CriticalThreshold + 50 -> severe;
        D when D >= CriticalThreshold -> critical;
        D when D >= WarningThreshold -> warning;
        _ -> normal
    end,

    #{
        conn_id => ConnId,
        warning_threshold => WarningThreshold,
        critical_threshold => CriticalThreshold,
        signal_count => SignalCount,
        peak_depth => PeakDepth,
        last_depth => LastDepth,
        pressure_level => PressureLevel
    }.

-spec reset_metrics(backpressure_state()) -> backpressure_state().
reset_metrics(State) ->
    {ConnId, WarningThreshold, CriticalThreshold, _LastSignalTime, _SignalCount, _PeakDepth, _LastDepth} = State,
    {
        ConnId,
        WarningThreshold,
        CriticalThreshold,
        erlang:timestamp(),
        0,
        0,
        0
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec increment_signal_count(backpressure_state()) -> backpressure_state().
increment_signal_count(State) ->
    {ConnId, WarningThreshold, CriticalThreshold, LastSignalTime, SignalCount, PeakDepth, LastDepth} = State,
    {ConnId, WarningThreshold, CriticalThreshold, LastSignalTime, SignalCount + 1, PeakDepth, LastDepth}.
