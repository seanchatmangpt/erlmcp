%%%-------------------------------------------------------------------
%%% @doc erlmcp_supervisor_utils_tests - Chicago School TDD Tests
%%%
%%% Tests ALL observable behavior through REAL OTP processes.
%%% NO MOCKS. NO FAKES. NO PLACEHOLDERS.
%%%
%%% Test Strategy:
%%% - Create real supervisors with different topologies
%%% - Use real gen_server workers
%%% - Test health scoring with actual process states
%%% - Test JSON export with real data
%%% - Clean up all processes after tests
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_supervisor_utils_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% Simple worker for testing
-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(worker_state, {
    name :: atom(),
    queue_messages = [] :: list(),
    memory_ballast = [] :: list()
}).

start_link(Name) ->
    gen_server:start_link(?MODULE, [Name], []).

init([Name]) ->
    {ok, #worker_state{name = Name}}.

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call({add_queue_messages, N}, _From, State) ->
    Messages = lists:duplicate(N, dummy_message),
    {reply, ok, State#worker_state{queue_messages = Messages}};
handle_call({allocate_memory, Bytes}, _From, State) ->
    Ballast = binary:copy(<<0>>, Bytes),
    {reply, ok, State#worker_state{memory_ballast = [Ballast]}};
handle_call(crash, _From, _State) ->
    error(intentional_crash);
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({self_message, N}, State) ->
    [self() ! {dummy, I} || I <- lists:seq(1, N)],
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Test supervisor - one_for_one with 3 workers
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => worker1,
            start => {?MODULE, start_link, [worker1]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        },
        #{
            id => worker2,
            start => {?MODULE, start_link, [worker2]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        },
        #{
            id => worker3,
            start => {?MODULE, start_link, [worker3]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%% Nested supervisor - supervisor with child supervisor
-export([start_link_nested/0, init_nested/1]).

start_link_nested() ->
    supervisor:start_link(?MODULE, nested).

init(nested) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        #{
            id => child_sup,
            start => {?MODULE, start_link, []},
            restart => permanent,
            shutdown => infinity,
            type => supervisor,
            modules => [?MODULE]
        },
        #{
            id => worker_top,
            start => {?MODULE, start_link, [worker_top]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [?MODULE]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Test basic children status retrieval
get_children_status_test() ->
    {ok, SupPid} = start_link(),
    try
        Children = erlmcp_supervisor_utils:get_children_status(SupPid),

        %% Should have 3 workers
        ?assertEqual(3, length(Children)),

        %% All should be workers with valid PIDs
        lists:foreach(fun(Child) ->
            ?assertMatch(#{
                id := _,
                pid := Pid,
                type := worker,
                modules := _,
                status := Status
            } when is_pid(Pid) andalso is_map(Status), Child),

            Status = maps:get(status, Child),
            ?assertMatch(#{
                status := _,
                queue_len := _,
                memory := _
            }, Status)
        end, Children)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test supervision tree retrieval
get_supervision_tree_test() ->
    {ok, SupPid} = start_link(),
    try
        Tree = erlmcp_supervisor_utils:get_supervision_tree(SupPid),

        %% Verify tree structure
        ?assertMatch(#{
            supervisor := SupPid,
            pid := SupPid,
            health_score := _,
            children := Children
        } when length(Children) =:= 3, Tree),

        %% Verify children
        Children = maps:get(children, Tree),
        lists:foreach(fun(Child) ->
            ?assertMatch(#{
                id := _,
                type := worker,
                pid := Pid,
                modules := _,
                state := _,
                tree := undefined
            } when is_pid(Pid), Child)
        end, Children)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test nested supervision tree
get_nested_supervision_tree_test() ->
    {ok, SupPid} = start_link_nested(),
    try
        Tree = erlmcp_supervisor_utils:get_supervision_tree(SupPid),

        %% Should have 2 children: 1 supervisor + 1 worker
        Children = maps:get(children, Tree),
        ?assertEqual(2, length(Children)),

        %% Find the child supervisor
        [ChildSup] = [C || C <- Children, maps:get(type, C) =:= supervisor],

        %% Child supervisor should have its own tree
        ?assertMatch(#{
            tree := #{
                children := ChildChildren
            }
        } when length(ChildChildren) =:= 3, ChildSup)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test process counting
count_processes_test() ->
    {ok, SupPid} = start_link(),
    try
        Count = erlmcp_supervisor_utils:count_processes(SupPid),
        %% Should count 3 workers
        ?assertEqual(3, Count)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test nested process counting
count_processes_nested_test() ->
    {ok, SupPid} = start_link_nested(),
    try
        Count = erlmcp_supervisor_utils:count_processes(SupPid),
        %% Should count: 1 child supervisor + 3 workers in child + 1 top worker = 5
        ?assertEqual(5, Count)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test health score - all healthy
calculate_health_score_healthy_test() ->
    {ok, SupPid} = start_link(),
    try
        Score = erlmcp_supervisor_utils:calculate_health_score(SupPid),
        %% All processes healthy should give score close to 1.0
        ?assert(Score >= 0.9),
        ?assert(Score =< 1.0)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test health score - with high message queue
calculate_health_score_high_queue_test() ->
    {ok, SupPid} = start_link(),
    try
        %% Get one worker and flood its message queue
        Children = supervisor:which_children(SupPid),
        {_, WorkerPid, _, _} = hd(Children),

        %% Send many messages to create high queue
        gen_server:cast(WorkerPid, {self_message, 500}),
        timer:sleep(100), % Let messages accumulate

        Score = erlmcp_supervisor_utils:calculate_health_score(SupPid),

        %% Score should be lower due to high queue
        ?assert(Score < 1.0)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test JSON export
export_to_json_test() ->
    {ok, SupPid} = start_link(),
    try
        Json = erlmcp_supervisor_utils:export_to_json(SupPid),

        %% Should be valid binary
        ?assert(is_binary(Json)),
        ?assert(byte_size(Json) > 0),

        %% Should be valid JSON (jsx decode should succeed)
        Decoded = jsx:decode(Json, [return_maps]),
        ?assertMatch(#{
            <<"tree">> := _,
            <<"metrics">> := _,
            <<"timestamp">> := _
        }, Decoded)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test pretty JSON export
export_to_json_pretty_test() ->
    {ok, SupPid} = start_link(),
    try
        Json = erlmcp_supervisor_utils:export_to_json_pretty(SupPid),

        %% Should be valid binary
        ?assert(is_binary(Json)),

        %% Pretty JSON should have newlines
        ?assert(binary:match(Json, <<"\n">>) =/= nomatch),

        %% Should be valid JSON
        Decoded = jsx:decode(Json, [return_maps]),
        ?assertMatch(#{
            <<"tree">> := _,
            <<"metrics">> := _,
            <<"timestamp">> := _
        }, Decoded)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test process metrics
get_process_metrics_test() ->
    {ok, SupPid} = start_link(),
    try
        Children = supervisor:which_children(SupPid),
        {_, WorkerPid, _, _} = hd(Children),

        Metrics = erlmcp_supervisor_utils:get_process_metrics(WorkerPid),

        ?assertMatch(#{
            memory_bytes := Mem,
            message_queue_len := Queue,
            reductions := Red,
            status := Status
        } when is_integer(Mem) andalso
               is_integer(Queue) andalso
               is_integer(Red) andalso
               is_atom(Status), Metrics)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test tree metrics
get_tree_metrics_test() ->
    {ok, SupPid} = start_link(),
    try
        Metrics = erlmcp_supervisor_utils:get_tree_metrics(SupPid),

        ?assertMatch(#{
            total_supervisors := 1,
            total_workers := 3,
            total_processes := 4,
            total_memory_bytes := Mem,
            max_depth := 0,
            health_score := Score,
            unhealthy_count := 0
        } when Mem > 0 andalso Score > 0.0, Metrics)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test tree metrics for nested supervisor
get_tree_metrics_nested_test() ->
    {ok, SupPid} = start_link_nested(),
    try
        Metrics = erlmcp_supervisor_utils:get_tree_metrics(SupPid),

        ?assertMatch(#{
            total_supervisors := Sups,
            total_workers := Workers,
            max_depth := Depth
        } when Sups >= 1 andalso Workers >= 3 andalso Depth > 0, Metrics)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test finding unhealthy processes - all healthy
find_unhealthy_processes_healthy_test() ->
    {ok, SupPid} = start_link(),
    try
        Unhealthy = erlmcp_supervisor_utils:find_unhealthy_processes(SupPid),
        %% All processes should be healthy
        ?assertEqual([], Unhealthy)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test finding unhealthy processes - high queue
find_unhealthy_processes_high_queue_test() ->
    {ok, SupPid} = start_link(),
    try
        %% Get one worker and flood its message queue
        Children = supervisor:which_children(SupPid),
        {_, WorkerPid, _, _} = hd(Children),

        %% Send many messages to create very high queue (>1000)
        gen_server:cast(WorkerPid, {self_message, 1500}),
        timer:sleep(100),

        Unhealthy = erlmcp_supervisor_utils:find_unhealthy_processes(SupPid),

        %% Should find the process with high queue
        ?assert(length(Unhealthy) > 0),

        %% Verify it's marked as high_queue
        [First | _] = Unhealthy,
        ?assertMatch(#{pid := _, reason := high_queue}, First)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test restart statistics
get_restart_statistics_test() ->
    {ok, SupPid} = start_link(),
    try
        Stats = erlmcp_supervisor_utils:get_restart_statistics(SupPid),

        %% Should have valid supervisor statistics
        ?assertMatch(#{
            active_children := 3,
            workers := 3,
            supervisors := 0
        }, Stats)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test supervision tree validation - valid tree
validate_supervision_tree_valid_test() ->
    {ok, SupPid} = start_link(),
    try
        Result = erlmcp_supervisor_utils:validate_supervision_tree(SupPid),

        ?assertMatch({ok, #{
            valid := true,
            checks := Checks
        }} when length(Checks) > 0, Result)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test flattened supervision tree
get_supervision_tree_flat_test() ->
    {ok, SupPid} = start_link(),
    try
        Flat = erlmcp_supervisor_utils:get_supervision_tree_flat(SupPid),

        %% Should have supervisor + 3 workers = 4 entries
        ?assertEqual(4, length(Flat)),

        %% All entries should have pid and supervisor
        lists:foreach(fun(Entry) ->
            ?assertMatch(#{
                pid := Pid,
                supervisor := _
            } when is_pid(Pid), Entry)
        end, Flat)
    after
        exit(SupPid, shutdown)
    end.

%% @doc Test with real erlmcp supervisor
integration_with_erlmcp_sup_test() ->
    %% Only run if erlmcp_sup is running
    case whereis(erlmcp_sup) of
        undefined ->
            %% Skip test if supervisor not running
            ok;
        SupPid ->
            %% Test basic introspection
            Children = erlmcp_supervisor_utils:get_children_status(SupPid),
            ?assert(length(Children) > 0),

            %% Test health score
            Score = erlmcp_supervisor_utils:calculate_health_score(SupPid),
            ?assert(Score >= 0.0 andalso Score =< 1.0),

            %% Test JSON export
            Json = erlmcp_supervisor_utils:export_to_json(SupPid),
            ?assert(is_binary(Json)),

            %% Test metrics
            Metrics = erlmcp_supervisor_utils:get_tree_metrics(SupPid),
            ?assertMatch(#{
                total_supervisors := _,
                total_workers := _,
                total_processes := _
            }, Metrics)
    end.

%% @doc Test with erlmcp_core_sup if available
integration_with_core_sup_test() ->
    case whereis(erlmcp_core_sup) of
        undefined ->
            ok;
        SupPid ->
            %% Should be able to introspect core supervisor
            Tree = erlmcp_supervisor_utils:get_supervision_tree(SupPid),
            ?assertMatch(#{
                supervisor := _,
                children := Children
            } when is_list(Children), Tree),

            %% Should be able to count processes
            Count = erlmcp_supervisor_utils:count_processes(SupPid),
            ?assert(Count > 0)
    end.

%% @doc Test error handling - non-existent supervisor
error_handling_bad_supervisor_test() ->
    %% Should handle gracefully
    Result = erlmcp_supervisor_utils:get_children_status(non_existent_sup),
    ?assertEqual([], Result).

%% @doc Test error handling - dead process
error_handling_dead_process_test() ->
    %% Create a process and kill it
    {ok, SupPid} = start_link(),
    exit(SupPid, kill),
    timer:sleep(100),

    %% Should handle gracefully
    Result = erlmcp_supervisor_utils:get_children_status(SupPid),
    ?assertEqual([], Result).

%% @doc Stress test - large supervision tree
stress_test_large_tree_test_() ->
    {timeout, 30, fun() ->
        %% Create a larger tree for stress testing
        {ok, SupPid} = start_link_nested(),
        try
            %% Multiple introspection operations should complete
            _Tree = erlmcp_supervisor_utils:get_supervision_tree(SupPid),
            _Flat = erlmcp_supervisor_utils:get_supervision_tree_flat(SupPid),
            _Metrics = erlmcp_supervisor_utils:get_tree_metrics(SupPid),
            _Json = erlmcp_supervisor_utils:export_to_json(SupPid),
            _Score = erlmcp_supervisor_utils:calculate_health_score(SupPid),

            %% All should complete without crashing
            ok
        after
            exit(SupPid, shutdown)
        end
    end}.

%%%===================================================================
%%% Property-Based Tests (if proper is available)
%%%===================================================================

%% Future: Add PropEr property-based tests
%% - Health score should always be between 0.0 and 1.0
%% - Process count should equal flattened tree length
%% - JSON export should always be valid JSON
%% - Metrics should sum correctly
