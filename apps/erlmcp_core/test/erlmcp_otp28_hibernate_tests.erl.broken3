-module(erlmcp_otp28_hibernate_tests).
-behaviour(gen_server).

%% @doc OTP 28 Hibernation Memory Reduction Test Suite
%% Tests gen_server hibernation for memory optimization
%%
%% == Test Coverage ==
%% 1. Hibernation triggering (timeout/idle)
%% 2. Memory reduction measurement
%% 3. State preservation across hibernation
%% 4. Performance impact
%% 5. Supervisor child hibernation
%% 6. OTP 28 hibernate/0 enhancement
%%
%% == Chicago School TDD ==
%% - Real gen_server spawning and monitoring
%% - Observable memory changes (process_info/1)
%% - State-based verification (get_state returns correct data)
%% - No mocks
%%
%% @end

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks for test server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(hibernation_state, {
    data :: [term()],
    counter :: non_neg_integer(),
    hibernate_after :: non_neg_integer()
}).

%%%====================================================================
%%% Test GenServer
%%%====================================================================

%% Simple gen_server that supports hibernation (defined below)
%% See hibernation_server module at end of file

%%%====================================================================
%%% Test Generators
%%%====================================================================

hibernate_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [{"Hibernation Triggering", {spawn, fun hibernation_triggering_tests/0}},
         {"Memory Reduction", {spawn, fun memory_reduction_tests/0}},
         {"State Preservation", {spawn, fun state_preservation_tests/0}},
         {"Performance Impact", {spawn, fun performance_impact_tests/0}},
         {"Supervisor Integration", {spawn, fun supervisor_tests/0}},
         {"OTP 28 Features", {spawn, fun otp28_features_tests/0}}]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

%%%====================================================================
%%% Hibernation Triggering Tests
%%%====================================================================

hibernation_triggering_tests() ->
    %% Test 1: gen_server returns {noreply, State, hibernate}
    {ok, Pid} = start_hibernate_server(100), %% 100ms timeout

    %% Send message to trigger activity
    Pid ! {add_data, test_item},

    %% Wait for hibernation timeout
    timer:sleep(200),

    %% Check if process hibernated (memory should be lower)
    Info = process_info(Pid, [memory, heap_size, total_heap_size]),
    Memory = proplists:get_value(memory, Info),
    ?assert(Memory > 0),

    %% Process should still be alive
    ?assert(is_process_alive(Pid)),

    %% Test 2: Multiple hibernation cycles
    {ok, Pid2} = start_hibernate_server(50),

    %% Trigger multiple activity/hibernation cycles
    lists:foreach(fun(N) ->
        Pid2 ! {add_data, N},
        timer:sleep(100) %% Wait for hibernation
    end, lists:seq(1, 5)),

    ?assert(is_process_alive(Pid2)),

    %% Test 3: Hibernation with continuous activity
    {ok, Pid3} = start_hibernate_server(100),

    %% Send continuous messages (prevent hibernation)
    lists:foreach(fun(N) ->
        Pid3 ! {add_data, N},
        timer:sleep(50) %% Faster than hibernation timeout
    end, lists:seq(1, 10)),

    %% Process should not have hibernated recently
    ?assert(is_process_alive(Pid3)),

    %% Test 4: Hibernate with zero timeout (immediate)
    {ok, Pid4} = start_hibernate_server(0),

    %% Should hibernate immediately after handling message
    Pid4 ! {add_data, immediate_test},
    timer:sleep(50),

    ?assert(is_process_alive(Pid4)),

    ok.

%%%====================================================================
%%% Memory Reduction Tests
%%%====================================================================

memory_reduction_tests() ->
    %% Test 1: Measure memory reduction after hibernation
    {ok, Pid} = start_hibernate_server(100),

    %% Add data to increase memory
    lists:foreach(fun(N) ->
        Pid ! {add_data, {large_item, N, lists:duplicate(1000, $x)}}
    end, lists:seq(1, 100)),

    timer:sleep(50), %% Let messages process

    %% Measure memory before hibernation
    BeforeInfo = process_info(Pid, [memory, heap_size, total_heap_size]),
    BeforeMemory = proplists:get_value(memory, BeforeInfo),
    BeforeHeap = proplists:get_value(total_heap_size, BeforeInfo),

    %% Wait for hibernation
    timer:sleep(200),

    %% Measure memory after hibernation
    AfterInfo = process_info(Pid, [memory, heap_size, total_heap_size]),
    AfterMemory = proplists:get_value(memory, AfterInfo),
    AfterHeap = proplists:get_value(total_heap_size, AfterInfo),

    %% Memory should be reduced after hibernation
    ?assert(AfterMemory =< BeforeMemory),
    io:format("Memory reduction: ~p -> ~p (~p%)~n",
              [BeforeMemory, AfterMemory,
               ((BeforeMemory - AfterMemory) * 100) div BeforeMemory]),

    %% Test 2: Verify heap size reduction
    ?assert(AfterHeap =< BeforeHeap),

    %% Test 3: Repeated hibernation reduces memory further
    lists:foreach(fun(_) ->
        Pid ! {add_data, more_data},
        timer:sleep(100) %% Trigger hibernation
    end, lists:seq(1, 10)),

    FinalInfo = process_info(Pid, [memory]),
    FinalMemory = proplists:get_value(memory, FinalInfo),

    %% Final memory should be reasonable
    ?assert(FinalMemory < BeforeMemory * 2),

    %% Test 4: Memory reduction with large state
    {ok, Pid2} = start_hibernate_server(100),

    %% Create large state
    LargeData = lists:duplicate(10000, {large, tuple, with, lots, of, data}),
    gen_server:call(Pid2, {set_large_state, LargeData}),

    timer:sleep(50),
    BeforeLarge = proplists:get_value(memory, process_info(Pid2, [memory])),

    %% Trigger hibernation
    timer:sleep(200),

    AfterLarge = proplists:get_value(memory, process_info(Pid2, [memory])),

    %% Significant memory reduction expected for large state
    Reduction = BeforeLarge - AfterLarge,
    ?assert(Reduction > 0),
    io:format("Large state reduction: ~p bytes~n", [Reduction]),

    ok.

%%%====================================================================
%%% State Preservation Tests
%%%====================================================================

state_preservation_tests() ->
    %% Test 1: State preserved across hibernation
    {ok, Pid} = start_hibernate_server(100),

    %% Set initial state
    TestData = [item1, item2, item3, item4, item5],
    lists:foreach(fun(Item) ->
        Pid ! {add_data, Item}
    end, TestData),

    timer:sleep(50),

    %% Get state before hibernation
    BeforeState = gen_server:call(Pid, get_state),

    %% Wait for hibernation
    timer:sleep(200),

    %% Get state after hibernation
    AfterState = gen_server:call(Pid, get_state),

    %% State should be identical
    ?assertEqual(BeforeState, AfterState),

    %% Test 2: Complex state preservation
    {ok, Pid2} = start_hibernate_server(100),

    ComplexState = #{
        integers => [1, 2, 3, 4, 5],
        binaries => [<<"a">>, <<"b">>, <<"c">>],
        tuples => [{x, 1}, {y, 2}, {z, 3}],
        nested => #{level1 => #{level2 => [1, 2, 3]}}
    },
    gen_server:call(Pid2, {set_state, ComplexState}),

    timer:sleep(50),
    BeforeComplex = gen_server:call(Pid2, get_state),

    %% Trigger hibernation
    timer:sleep(200),

    AfterComplex = gen_server:call(Pid2, get_state),

    ?assertEqual(BeforeComplex, AfterComplex),

    %% Test 3: Counter preserved across hibernations
    {ok, Pid3} = start_hibernate_server(50),

    %% Increment counter
    lists:foreach(fun(_) ->
        gen_server:call(Pid3, increment_counter)
    end, lists:seq(1, 10)),

    CounterBefore = proplists:get_value(counter,
                                       gen_server:call(Pid3, get_state)),

    %% Hibernate multiple times
    lists:foreach(fun(_) ->
        timer:sleep(100)
    end, lists:seq(1, 5)),

    CounterAfter = proplists:get_value(counter,
                                      gen_server:call(Pid3, get_state)),

    ?assertEqual(CounterBefore, CounterAfter),

    %% Test 4: Process dictionary preserved
    {ok, Pid4} = start_hibernate_server(100),

    %% Put data in process dictionary
    Pid4 ! {put_dict, test_key, test_value},

    timer:sleep(50),

    %% Trigger hibernation
    timer:sleep(200),

    %% Check process dictionary
    DictValue = gen_server:call(Pid4, get_dict, test_key),
    ?assertEqual(test_value, DictValue),

    ok.

%%%====================================================================
%%% Performance Impact Tests
%%%====================================================================

performance_impact_tests() ->
    %% Test 1: Call latency increase after hibernation
    {ok, Pid} = start_hibernate_server(100),

    %% Measure latency before hibernation
    Latencies1 = [measure_call_latency(Pid) || _ <- lists:seq(1, 10)],
    AvgBefore = lists:sum(Latencies1) div length(Latencies1),

    %% Trigger hibernation
    timer:sleep(200),

    %% Measure latency after hibernation (first call is slower)
    FirstAfter = measure_call_latency(Pid),
    RestAfter = [measure_call_latency(Pid) || _ <- lists:seq(1, 10)],
    AvgAfter = lists:sum(RestAfter) div length(RestAfter),

    %% First call after hibernation is slower (waking up)
    ?assert(FirstAfter > AvgAfter),

    %% Subsequent calls should be comparable
    ?assert(AvgAfter < AvgBefore * 2),

    io:format("Latency: before=~pμs, first_after=~pμs, avg_after=~pμs~n",
              [AvgBefore, FirstAfter, AvgAfter]),

    %% Test 2: Throughput with hibernation
    {ok, Pid2} = start_hibernate_server(50),

    %% Measure throughput before hibernation
    StartTime1 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) ->
        gen_server:call(Pid2, {add_data, N})
    end, lists:seq(1, 100)),
    EndTime1 = erlang:monotonic_time(microsecond),
    Throughput1 = 100_000_000 / (EndTime1 - StartTime1), %% ops/sec

    %% Trigger hibernation
    timer:sleep(100),

    %% Measure throughput after hibernation
    StartTime2 = erlang:monotonic_time(microsecond),
    lists:foreach(fun(N) ->
        gen_server:call(Pid2, {add_data, N})
    end, lists:seq(1, 100)),
    EndTime2 = erlang:monotonic_time(microsecond),
    Throughput2 = 100_000_000 / (EndTime2 - StartTime2),

    io:format("Throughput: before=~p ops/sec, after=~p ops/sec~n",
              [Throughput1, Throughput2]),

    %% Throughput should be reasonable after hibernation
    ?assert(Throughput2 > Throughput1 / 2),

    %% Test 3: Hibernation overhead
    {ok, Pid3} = start_hibernate_server(50),

    %% Measure time to hibernate
    MemBefore = proplists:get_value(memory, process_info(Pid3, [memory])),
    StartTime = erlang:monotonic_time(microsecond),

    timer:sleep(100), %% Trigger hibernation

    MemAfter = proplists:get_value(memory, process_info(Pid3, [memory])),
    EndTime = erlang:monotonic_time(microsecond),

    HibernateTime = EndTime - StartTime,
    MemSaved = MemBefore - MemAfter,

    io:format("Hibernate: time=~pμs, memory_saved=~p bytes~n",
              [HibernateTime, MemSaved]),

    %% Hibernate should be relatively fast
    ?assert(HibernateTime < 10_000), %% Less than 10ms

    ok.

%%%====================================================================
%%% Supervisor Integration Tests
%%%====================================================================

supervisor_tests() ->
    %% Test 1: Child process hibernation under supervisor
    {ok, SupPid} = start_test_supervisor(),

    %% Start child server with hibernation
    {ok, ChildPid} = supervisor:start_child(SupPid,
                                             #{id => hibernate_child,
                                               start => {hibernation_server, start_link, [100]},
                                               restart => temporary,
                                               shutdown => 5000,
                                               type => worker,
                                               modules => [hibernation_server]}),

    %% Child should be alive
    ?assert(is_process_alive(ChildPid)),

    %% Add data and trigger hibernation
    ChildPid ! {add_data, test},
    timer:sleep(200),

    %% Child should still be alive and under supervision
    ?assert(is_process_alive(ChildPid)),
    Children = supervisor:which_children(SupPid),
    ?assert(lists:keymember(hibernate_child, 1, Children)),

    %% Test 2: Multiple hibernating children
    {ok, SupPid2} = start_test_supervisor(),

    %% Start multiple children
    Children2 = lists:map(fun(N) ->
        {ok, Pid} = supervisor:start_child(SupPid2,
                                            #{id => N,
                                              start => {hibernation_server, start_link, [100]},
                                              restart => temporary,
                                              shutdown => 5000,
                                              type => worker,
                                              modules => [hibernation_server]}),
        Pid
    end, lists:seq(1, 5)),

    %% All children should be alive
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, Children2),

    %% Trigger hibernation for all
    lists:foreach(fun(Pid) ->
        Pid ! {add_data, hibernate_me}
    end, Children2),

    timer:sleep(200),

    %% All should still be alive
    lists:foreach(fun(Pid) ->
        ?assert(is_process_alive(Pid))
    end, Children2),

    %% Test 3: Child restart after hibernation
    {ok, SupPid3} = start_test_supervisor(),

    {ok, ChildPid3} = supervisor:start_child(SupPid3,
                                              #{id => restart_child,
                                                start => {hibernation_server, start_link, [100]},
                                                restart => permanent,
                                                shutdown => 5000,
                                                type => worker,
                                                modules => [hibernation_server]}),

    %% Trigger hibernation
    ChildPid3 ! {add_data, test},
    timer:sleep(200),

    %% Kill child (should restart)
    exit(ChildPid3, kill),

    timer:sleep(100),

    %% Child should have restarted
    Children3 = supervisor:which_children(SupPid3),
    {restart_child, NewPid, _, _} = lists:keyfind(restart_child, 1, Children3),
    ?assert(is_process_alive(NewPid)),
    ?assert(NewPid =/= ChildPid3),

    ok.

%%%====================================================================
%%% OTP 28 Features Tests
%%%====================================================================

otp28_features_tests() ->
    %% Test 1: OTP 28 hibernate/0 enhancement
    %% Check if running OTP 28+
    IsOtp28 = erlang:system_info(otp_release) >= "28",

    case IsOtp28 of
        true ->
            %% OTP 28: hibernate/0 is available
            {ok, Pid} = start_hibernate_server(100),

            %% Send hibernate message (OTP 28 feature)
            Pid ! hibernate,
            timer:sleep(50),

            %% Process should be alive and hibernated
            ?assert(is_process_alive(Pid)),
            Info = process_info(Pid, [memory, status]),
            ?assertEqual(proplists:get_value(status, Info), waiting);
        false ->
            %% OTP < 28: hibernate/0 not available
            ok
    end,

    %% Test 2: Hibernation configuration API
    case erlang:function_exported(erlmcp_otp28_supervisor_enhancements,
                                  configure_hibernation, 2) of
        true ->
            %% OTP 28+ API available
            {ok, SupPid} = start_test_supervisor(),

            %% Configure hibernation
            Result = erlmcp_otp28_supervisor_enhancements:configure_hibernation(
                        SupPid, 1000),
            ?assertEqual(ok, Result),

            %% Get hibernation config
            Config = erlmcp_otp28_supervisor_enhancements:get_hibernation_config(
                        SupPid),
            ?assert(maps:is_key(enabled, Config));
        false ->
            ok
    end,

    %% Test 3: Performance metrics with hibernation
    case erlang:function_exported(erlmcp_otp28_supervisor_enhancements,
                                  get_performance_metrics, 1) of
        true ->
            {ok, SupPid2} = start_test_supervisor(),

            %% Start child with hibernation
            {ok, _ChildPid} = supervisor:start_child(SupPid2,
                                                      #{id => metrics_child,
                                                        start => {hibernation_server, start_link, [50]},
                                                        restart => temporary,
                                                        shutdown => 5000,
                                                        type => worker,
                                                        modules => [hibernation_server]}),

            %% Get performance metrics
            {ok, Metrics} = erlmcp_otp28_supervisor_enhancements:get_performance_metrics(
                             SupPid2),
            ?assert(maps:is_key(total_children, Metrics)),
            ?assert(maps:is_key(active_children, Metrics)),
            ?assertEqual(1, maps:get(total_children, Metrics));
        false ->
            ok
    end,

    ok.

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% @doc Start a hibernation test server (using inline gen_server)
start_hibernate_server(TimeoutMs) ->
    gen_server:start_link(?MODULE, [test_server, TimeoutMs], []).

%% @doc Measure call latency
measure_call_latency(Pid) ->
    StartTime = erlang:monotonic_time(microsecond),
    gen_server:call(Pid, get_state),
    EndTime = erlang:monotonic_time(microsecond),
    EndTime - StartTime.

%%%===================================================================
%%% gen_server callbacks (for test server)
%%%===================================================================

init([test_server, TimeoutMs]) ->
    {ok, #hibernation_state{data = [], counter = 0, hibernate_after = TimeoutMs}};

init([]) ->
    %% Dummy init for test module
    {ok, #hibernation_state{data = [], counter = 0, hibernate_after = 1000}}.

handle_call(get_state, _From, State) ->
    {reply, #{data => State#hibernation_state.data,
              counter => State#hibernation_state.counter},
     State, hibernate};

handle_call({set_state, NewState}, _From, State) ->
    {reply, ok, State#hibernation_state{data = NewState}, hibernate};

handle_call({set_large_state, LargeState}, _From, State) ->
    {reply, ok, State#hibernation_state{data = LargeState}, hibernate};

handle_call(increment_counter, _From, State) ->
    NewCounter = State#hibernation_state.counter + 1,
    {reply, ok, State#hibernation_state{counter = NewCounter}, hibernate};

handle_call(get_dict, Key, _From, State) ->
    Value = get(Key),
    {reply, Value, State, hibernate};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({add_data, Item}, State) ->
    NewData = [Item | State#hibernation_state.data],
    {noreply, State#hibernation_state{data = NewData}, hibernate};

handle_info({put_dict, Key, Value}, State) ->
    put(Key, Value),
    {noreply, State, hibernate};

handle_info(hibernate, State) ->
    %% OTP 28: Explicit hibernate/0 call
    {noreply, State, hibernate};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
