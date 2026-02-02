%%%====================================================================
%%% @doc Supervisor Hibernation Benchmark (OTP 28)
%%%
%%% Measures memory savings from supervisor auto-hibernation.
%%%
%%% @end
%%%====================================================================
-module(erlmcp_bench_hibernation).

%% Benchmark API
-export([run/0, run/1, run_static/1, run_dynamic/1]).
-export([measure_supervisor_memory/1]).

-define(NUM_CHILDREN, 1000).
-define(IDLE_TIME_MS, 2000).  % Wait for hibernation to trigger

%%%====================================================================
%%% Benchmark API
%%%====================================================================

%% @doc Run full benchmark comparing static vs dynamic supervisors
-spec run() -> [{atom(), map()}].
run() ->
    run(?NUM_CHILDREN).

-spec run(pos_integer()) -> [{atom(), map()}].
run(NumChildren) ->
    io:format("~n=== OTP 28 Supervisor Hibernation Benchmark ===~n"),
    io:format("Testing with ~p children~n~n", [NumChildren]),

    Results = [
        {static_supervisor, run_static(NumChildren)},
        {dynamic_supervisor, run_dynamic(NumChildren)}
    ],

    print_results(Results),
    Results.

%% @doc Benchmark static supervisor with auto-hibernation
-spec run_static(pos_integer()) -> map().
run_static(NumChildren) ->
    io:format("~n--- Static Supervisor (Auto-Hibernate) ---~n"),

    %% Start static supervisor
    {ok, SupPid} = start_static_supervisor(),

    %% Measure memory before children
    MemoryBefore = measure_supervisor_memory(SupPid),
    io:format("Memory before children: ~p bytes~n", [MemoryBefore]),

    %% Start children
    Children = start_children(SupPid, NumChildren),
    io:format("Started ~p children~n", [length(Children)]),

    %% Measure memory with active children
    MemoryWithChildren = measure_supervisor_memory(SupPid),
    io:format("Memory with ~p children: ~p bytes~n", [NumChildren, MemoryWithChildren]),

    %% Wait for hibernation (1s idle + 1s safety margin)
    io:format("Waiting ~pms for hibernation...~n", [?IDLE_TIME_MS]),
    timer:sleep(?IDLE_TIME_MS),

    %% Measure memory after hibernation
    MemoryAfterHibernate = measure_supervisor_memory(SupPid),
    io:format("Memory after hibernation: ~p bytes~n", [MemoryAfterHibernate]),

    %% Stop children and supervisor
    stop_children(Children),
    stop_supervisor(SupPid),

    %% Calculate savings
    ActivePerChild = (MemoryWithChildren - MemoryBefore) div NumChildren,
    Savings = MemoryWithChildren - MemoryAfterHibernate,
    SavingsPercent = (Savings * 100) div MemoryWithChildren,

    io:format("Memory per child (active): ~p bytes~n", [ActivePerChild]),
    io:format("Memory savings: ~p bytes (~p%)~n", [Savings, SavingsPercent]),

    #{
        type => static,
        num_children => NumChildren,
        memory_before => MemoryBefore,
        memory_with_children => MemoryWithChildren,
        memory_after_hibernate => MemoryAfterHibernate,
        memory_per_child => ActivePerChild,
        total_savings => Savings,
        savings_percent => SavingsPercent
    }.

%% @doc Benchmark dynamic supervisor without auto-hibernation
-spec run_dynamic(pos_integer()) -> map().
run_dynamic(NumChildren) ->
    io:format("~n--- Dynamic Supervisor (No Auto-Hibernate) ---~n"),

    %% Start dynamic supervisor
    {ok, SupPid} = start_dynamic_supervisor(),

    %% Measure memory before children
    MemoryBefore = measure_supervisor_memory(SupPid),
    io:format("Memory before children: ~p bytes~n", [MemoryBefore]),

    %% Start children
    Children = start_children(SupPid, NumChildren),
    io:format("Started ~p children~n", [length(Children)]),

    %% Measure memory with active children
    MemoryWithChildren = measure_supervisor_memory(SupPid),
    io:format("Memory with ~p children: ~p bytes~n", [NumChildren, MemoryWithChildren]),

    %% Wait same time (no hibernation expected)
    io:format("Waiting ~pms...~n", [?IDLE_TIME_MS]),
    timer:sleep(?IDLE_TIME_MS),

    %% Measure memory (should be same)
    MemoryAfter = measure_supervisor_memory(SupPid),
    io:format("Memory after wait: ~p bytes~n", [MemoryAfter]),

    %% Stop children and supervisor
    stop_children(Children),
    stop_supervisor(SupPid),

    %% Calculate per-child memory
    PerChild = (MemoryWithChildren - MemoryBefore) div NumChildren,
    Unchanged = MemoryWithChildren - MemoryAfter,

    io:format("Memory per child: ~p bytes~n", [PerChild]),
    io:format("Memory unchanged: ~p bytes~n", [Unchanged]),

    #{
        type => dynamic,
        num_children => NumChildren,
        memory_before => MemoryBefore,
        memory_with_children => MemoryWithChildren,
        memory_after_wait => MemoryAfter,
        memory_per_child => PerChild,
        unchanged => Unchanged
    }.

%% @doc Measure memory usage of a supervisor process
-spec measure_supervisor_memory(pid()) -> pos_integer().
measure_supervisor_memory(SupPid) ->
    %% Get process info
    {memory, Memory} = erlang:process_info(SupPid, memory),
    %% Get message queue len (included in memory)
    {message_queue_len, _QLen} = erlang:process_info(SupPid, message_queue_len),
    Memory.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Start a static supervisor with auto-hibernation
start_static_supervisor() ->
    SupSpec = #{
        id => hibernation_bench_sup,
        start => {hibernation_bench_sup, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => supervisor,
        modules => [hibernation_bench_sup]
    },
    {ok, Pid} = supervisor:start_link(SupSpec),
    {ok, Pid}.

%% @doc Start a dynamic supervisor without auto-hibernation
start_dynamic_supervisor() ->
    SupSpec = #{
        id => hibernation_bench_dyn_sup,
        start => {hibernation_bench_dyn_sup, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => supervisor,
        modules => [hibernation_bench_dyn_sup]
    },
    {ok, Pid} = supervisor:start_link(SupSpec),
    {ok, Pid}.

%% @doc Start children under supervisor
start_children(SupPid, NumChildren) ->
    lists:map(fun(I) ->
        ChildSpec = #{
            id => I,
            start => {bench_worker, start_link, []},
            restart => temporary,
            shutdown => 5000
        },
        {ok, Pid} = supervisor:start_child(SupPid, ChildSpec),
        Pid
    end, lists:seq(1, NumChildren)).

%% @doc Stop children
stop_children(Children) ->
    lists:foreach(fun(Pid) ->
        exit(Pid, kill)
    end, Children).

%% @doc Stop supervisor
stop_supervisor(Pid) ->
    exit(Pid, shutdown).

%% @doc Print benchmark results
print_results(Results) ->
    io:format("~n=== Benchmark Results ===~n~n"),

    lists:foreach(fun({Type, Data}) ->
        io:format("~p Supervisor:~n", [Type]),
        io:format("  Children: ~p~n", [maps:get(num_children, Data)]),

        case Type of
            static_supervisor ->
                Savings = maps:get(total_savings, Data),
                Percent = maps:get(savings_percent, Data),
                io:format("  Memory Savings: ~p bytes (~p%)~n", [Savings, Percent]),
                io:format("  Per-Child Memory: ~p bytes~n", [maps:get(memory_per_child, Data)]);
            _ ->
                io:format("  Memory Unchanged: ~p bytes~n", [maps:get(unchanged, Data)]),
                io:format("  Per-Child Memory: ~p bytes~n", [maps:get(memory_per_child, Data)])
        end,
        io:format("~n")
    end, Results).

%%%====================================================================
%%% Test Supervisor Modules (for benchmark)
%%%====================================================================

%% Static supervisor with auto-hibernation
-module(hibernation_bench_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, hibernate_after/0]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60,
        auto_hibernation => ?MODULE  % Enable auto-hibernation
    },
    {ok, {SupFlags, []}}.

%% OTP 28 callback: hibernate after 1 second idle
hibernate_after() -> 1000.

%% Dynamic supervisor without auto-hibernation
-module(hibernation_bench_dyn_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 60,
        auto_hibernation => false  % Disable auto-hibernation
    },
    ChildSpec = [#{
        id => bench_worker,
        start => {bench_worker, start_link, []},
        restart => temporary,
        shutdown => 5000
    }],
    {ok, {SupFlags, ChildSpec}}.

%% Benchmark worker process
-module(bench_worker).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
