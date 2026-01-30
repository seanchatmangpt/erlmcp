%%%====================================================================
%%% @doc Test Synchronization Helpers
%%%
%%% Provides poll-based synchronization and process monitoring helpers
%%% to replace timer:sleep calls in tests. This eliminates timing
%%% dependencies and makes tests more reliable and faster.
%%%
%%% Usage in tests:
%%% ```
%%% -include_lib("erlmcp_test_sync.hrl").
%%% {ok, _} = poll_until(fun() -> is_ready() end, 100, 2000).
%%% ```
%%% @end
%%%====================================================================
-module(erlmcp_test_sync).
-include("../include/erlmcp.hrl").

%% Exported functions
-export([
    poll_until/4,
    poll_until/5,
    wait_for_process_death/2,
    wait_for_process_death/3,
    wait_for_message/2,
    wait_for_message/3,
    wait_for_state/3,
    wait_for_state/4,
    wait_for_condition/3,
    wait_for_condition/4,
    wait_for_ets_insert/2,
    wait_for_ets_insert/3,
    wait_for_ets_delete/2,
    wait_for_ets_delete/3,
    wait_for_ets_match/3,
    assert_process_dead/1,
    assert_process_alive/1,
    synchronize_with_monitor/1,
    flush_mailbox/0,
    flush_messages/1
]).

%% Types
-type condition_fun() :: fun(() -> boolean() | {true, any()}).
-type poll_options() :: #{
    timeout => non_neg_integer(),
    interval => non_neg_integer(),
    sleep => boolean()
}.

%%%===================================================================
%%% Poll-Based Synchronization
%%%===================================================================

%% @doc Poll until a condition function returns true or timeout
%% Default: 1000ms timeout, 10ms interval
-spec poll_until(condition_fun(), any(), non_neg_integer(), non_neg_integer()) ->
    {ok, any()} | {error, timeout}.
poll_until(ConditionFun, TimeoutMsg, Timeout, Interval) ->
    poll_until(ConditionFun, TimeoutMsg, Timeout, Interval, #{}).

%% @doc Poll until a condition function returns true or timeout
%% Options: #{timeout => ms, interval => ms, sleep => boolean()}
-spec poll_until(condition_fun(), any(), poll_options() | non_neg_integer(), non_neg_integer(), map()) ->
    {ok, any()} | {error, timeout}.
poll_until(ConditionFun, TimeoutMsg, OptionsOrTimeout, DefaultInterval, DefaultOptions) ->
    Options = case is_map(OptionsOrTimeout) of
        true -> OptionsOrTimeout;
        false -> DefaultOptions#{timeout => OptionsOrTimeout}
    end,

    Timeout = maps:get(timeout, Options, 1000),
    Interval = maps:get(interval, Options, DefaultInterval),
    ShouldSleep = maps:get(sleep, Options, true),

    StartTime = erlang:monotonic_time(millisecond),
    poll_loop(ConditionFun, TimeoutMsg, StartTime, Timeout, Interval, ShouldSleep).

%% @doc Poll loop implementation
poll_loop(ConditionFun, TimeoutMsg, StartTime, Timeout, Interval, ShouldSleep) ->
    case ConditionFun() of
        true ->
            {ok, true};
        {true, Result} ->
            {ok, Result};
        false when ShouldSleep =:= true ->
            CurrentTime = erlang:monotonic_time(millisecond),
            Elapsed = CurrentTime - StartTime,
            if
                Elapsed >= Timeout ->
                    {error, timeout};
                true ->
                    NextInterval = min(Interval, Timeout - Elapsed),
                    timer:sleep(NextInterval),
                    poll_loop(ConditionFun, TimeoutMsg, StartTime, Timeout, Interval, ShouldSleep)
            end;
        false when ShouldSleep =:= false ->
            CurrentTime = erlang:monotonic_time(millisecond),
            Elapsed = CurrentTime - StartTime,
            if
                Elapsed >= Timeout ->
                    {error, timeout};
                true ->
                    poll_loop(ConditionFun, TimeoutMsg, StartTime, Timeout, Interval, ShouldSleep)
            end
    end.

%%%===================================================================
%%% Process Monitoring Helpers
%%%===================================================================

%% @doc Wait for a process to die (monitor-based)
-spec wait_for_process_death(pid(), non_neg_integer()) -> ok | {error, timeout}.
wait_for_process_death(Pid, Timeout) ->
    wait_for_process_death(Pid, Timeout, #{}).

%% @doc Wait for a process to die with options
-spec wait_for_process_death(pid(), non_neg_integer(), map()) -> ok | {error, timeout}.
wait_for_process_death(Pid, Timeout, Options) ->
    MonitorRef = monitor(process, Pid),
    ConditionFun = fun() ->
        not is_process_alive(Pid)
    end,
    Result = poll_until(ConditionFun, process_death, Options, Timeout, 5),
    demonitor(MonitorRef, [flush]),
    case Result of
        {ok, _} -> ok;
        {error, timeout} -> {error, timeout}
    end.

%% @doc Assert a process is dead
-spec assert_process_dead(pid()) -> ok.
assert_process_dead(Pid) ->
    case is_process_alive(Pid) of
        false -> ok;
        true -> error({process_should_be_dead, Pid})
    end.

%% @doc Assert a process is alive
-spec assert_process_alive(pid()) -> ok.
assert_process_alive(Pid) ->
    case is_process_alive(Pid) of
        true -> ok;
        false -> error({process_should_be_alive, Pid})
    end.

%% @doc Synchronize with a process using monitor
%% Returns ok when DOWN message received
-spec synchronize_with_monitor(pid()) -> ok.
synchronize_with_monitor(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, _Info} -> ok
    end.

%%%===================================================================
%%% Message Waiting Helpers
%%%===================================================================

%% @doc Wait for a specific message pattern
-spec wait_for_message(fun((any()) -> boolean()), non_neg_integer()) ->
    {ok, any()} | {error, timeout}.
wait_for_message(PatternFun, Timeout) ->
    wait_for_message(PatternFun, Timeout, #{}).

%% @doc Wait for a specific message pattern with options
-spec wait_for_message(fun((any()) -> boolean()), non_neg_integer(), map()) ->
    {ok, any()} | {error, timeout}.
wait_for_message(PatternFun, Timeout, Options) ->
    ConditionFun = fun() ->
        receive
            Msg ->
                case PatternFun(Msg) of
                    true -> {true, Msg};
                    false -> false
                end
        after 0 ->
            false
        end
    end,
    poll_until(ConditionFun, message_match, Options, Timeout, 5).

%%%===================================================================
%%% State Waiting Helpers
%%%===================================================================

%% @doc Wait for a gen_server to reach a specific state
-spec wait_for_state(pid(), fun((any()) -> boolean()), non_neg_integer()) ->
    {ok, any()} | {error, timeout}.
wait_for_state(Pid, StateFun, Timeout) ->
    wait_for_state(Pid, StateFun, Timeout, #{}).

%% @doc Wait for a gen_server to reach a specific state with options
-spec wait_for_state(pid(), fun((any()) -> boolean()), non_neg_integer(), map()) ->
    {ok, any()} | {error, timeout}.
wait_for_state(Pid, StateFun, Timeout, Options) ->
    ConditionFun = fun() ->
        try sys:get_state(Pid, 5000) of
            State ->
                case StateFun(State) of
                    true -> {true, State};
                    false -> false
                end
        catch
            _:_ -> false
        end
    end,
    poll_until(ConditionFun, state_match, Options, Timeout, 10).

%%%===================================================================
%%% Condition Waiting Helpers
%%%===================================================================

%% @doc Wait for a custom condition to be true
-spec wait_for_condition(condition_fun(), non_neg_integer(), non_neg_integer()) ->
    {ok, any()} | {error, timeout}.
wait_for_condition(ConditionFun, Timeout, Interval) ->
    wait_for_condition(ConditionFun, Timeout, Interval, #{}).

%% @doc Wait for a custom condition with options
-spec wait_for_condition(condition_fun(), non_neg_integer(), non_neg_integer(), map()) ->
    {ok, any()} | {error, timeout}.
wait_for_condition(ConditionFun, Timeout, Interval, Options) ->
    poll_until(ConditionFun, condition, Options, Timeout, Interval).

%%%===================================================================
%%% ETS Table Helpers
%%%===================================================================

%% @doc Wait for an ETS insert to occur
-spec wait_for_ets_insert(ets:tid(), any()) ->
    {ok, any()} | {error, timeout}.
wait_for_ets_insert(Table, Key) ->
    wait_for_ets_insert(Table, Key, 1000).

%% @doc Wait for an ETS insert with timeout
-spec wait_for_ets_insert(ets:tid(), any(), non_neg_integer()) ->
    {ok, any()} | {error, timeout}.
wait_for_ets_insert(Table, Key, Timeout) ->
    ConditionFun = fun() ->
        case ets:lookup(Table, Key) of
            [Obj | _] -> {true, Obj};
            [] -> false
        end
    end,
    poll_until(ConditionFun, ets_insert, Timeout, 5).

%% @doc Wait for an ETS delete to occur
-spec wait_for_ets_delete(ets:tid(), any()) -> ok | {error, timeout}.
wait_for_ets_delete(Table, Key) ->
    wait_for_ets_delete(Table, Key, 1000).

%% @doc Wait for an ETS delete with timeout
-spec wait_for_ets_delete(ets:tid(), any(), non_neg_integer()) -> ok | {error, timeout}.
wait_for_ets_delete(Table, Key, Timeout) ->
    ConditionFun = fun() ->
        case ets:lookup(Table, Key) of
            [] -> true;
            _ -> false
        end
    end,
    case poll_until(ConditionFun, ets_delete, Timeout, 5) of
        {ok, _} -> ok;
        {error, _} -> {error, timeout}
    end.

%% @doc Wait for ETS match to return results
-spec wait_for_ets_match(ets:tid(), ets:match_pattern(), non_neg_integer()) ->
    {ok, [any()]} | {error, timeout}.
wait_for_ets_match(Table, Pattern, Timeout) ->
    ConditionFun = fun() ->
        case ets:match_object(Table, Pattern) of
            [] -> false;
            Results -> {true, Results}
        end
    end,
    poll_until(ConditionFun, ets_match, Timeout, 5).

%%%===================================================================
%%% Mailbox Management
%%%===================================================================

%% @doc Flush all messages from mailbox
-spec flush_mailbox() -> ok.
flush_mailbox() ->
    receive
        _Msg -> flush_mailbox()
    after 0 ->
        ok
    end.

%% @doc Flush specific messages from mailbox
-spec flush_messages(fun((any()) -> boolean())) -> non_neg_integer().
flush_messages(PatternFun) ->
    flush_messages(PatternFun, 0, []).

flush_messages(PatternFun, Count, Acc) ->
    receive
        Msg ->
            case PatternFun(Msg) of
                true -> flush_messages(PatternFun, Count + 1, Acc);
                false -> flush_messages(PatternFun, Count, [Msg | Acc])
            end
    after 0 ->
        %% Put non-matching messages back in reverse order
        lists:foreach(fun(M) -> self() ! M end, lists:reverse(Acc)),
        Count
    end.
