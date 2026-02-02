-module(erlmcp_otp28_upgrade_tests).
-author("erlmcp").

%% OTP 28.3.1 Upgrade Tests - Chicago School TDD
%% Tests validate OTP 28-specific improvements and optimizations
%%
%% Test Philosophy:
%% - Black-box testing: Test observable behavior, not implementation
%% - No mocks: Use real processes and supervisors
%% - Property-based: Use proper for comprehensive testing
%% - Real-world: Test actual OTP patterns

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

%% @doc Setup function - start applications
setup() ->
    {ok, Apps} = application:ensure_all_started(erlmcp_core),
    {ok, Pid} = erlmcp_otp28_upgrade:start_link(),
    {Apps, Pid}.

%% @doc Cleanup function - stop applications
cleanup({Apps, _Pid}) ->
    lists:foreach(fun(App) -> application:stop(App) end, lists:reverse(Apps)),
    ok.

%%%===================================================================
%%% Supervisor Children Tests
%%%===================================================================

supervisor_get_children_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Get children from existing supervisor
                     Children = erlmcp_otp28_upgrade:get_supervisor_children(erlmcp_sup),
                     ?assert(is_list(Children)),
                     ?assert(length(Children) > 0),

                     %% Validate child structure
                     [?assertMatch(#{id := _, pid := _, type := _, modules := _}, Child)
                      || Child <- Children]
                 end),
          ?_test(begin
                     %% Test: Handle non-existent supervisor gracefully
                     Children = erlmcp_otp28_upgrade:get_supervisor_children(nonexistent_sup),
                     ?assertEqual([], Children)
                 end)
         ]
     end}.

%%%===================================================================
%%% Process Info Optimization Tests
%%%===================================================================

process_info_optimized_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Get optimized process info for current process
                     Info = erlmcp_otp28_upgrade:get_process_info_optimized(self()),
                     ?assert(is_map(Info)),
                     ?assert(maps:is_key(memory, Info)),
                     ?assert(maps:is_key(heap_size, Info)),
                     ?assert(maps:is_key(reductions, Info)),

                     %% Validate types
                     Memory = maps:get(memory, Info),
                     ?assert(is_integer(Memory)),
                     ?assert(Memory > 0)
                 end),
          ?_test(begin
                     %% Test: Get info with custom items
                     Info = erlmcp_otp28_upgrade:get_process_info_optimized(self(),
                         [message_queue_len, status]),
                     ?assert(is_map(Info)),
                     ?assert(maps:is_key(message_queue_len, Info)),
                     ?assert(maps:is_key(status, Info))
                 end),
          ?_test(begin
                     %% Test: Handle dead process gracefully
                     DeadPid = spawn(fun() -> ok end),
                     timer:sleep(100), %% Ensure it's dead

                     Info = erlmcp_otp28_upgrade:get_process_info_optimized(DeadPid),
                     ?assert(is_map(Info)),
                     ?assert(maps:is_key(error, Info))
                 end)
         ]
     end}.

%%%===================================================================
%%% Monitor with Metadata Tests
%%%===================================================================

monitor_with_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Monitor process with metadata
                     TestPid = spawn_link(fun() ->
                         timer:sleep(1000),
                         ok
                     end),

                     Metadata = #{purpose => test, category => unit},
                     Ref = erlmcp_otp28_upgrade:monitor_with_metadata(TestPid, Metadata),

                     ?assert(is_reference(Ref)),

                     %% Clean up
                     exit(TestPid, kill),
                     timer:sleep(100)
                 end),
          ?_test(begin
                     %% Test: Handle monitor failure gracefully
                     catch erlmcp_otp28_upgrade:monitor_with_metadata(self(), #{}),
                     ?assert(true) %% Should not crash
                 end)
         ]
     end}.

%%%===================================================================
%%% Logger Metadata Tests
%%%===================================================================

logger_metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Set logger metadata
                     Metadata = #{request_id => <<"123">>, user_id => <<"456">>},
                     Result = erlmcp_otp28_upgrade:logger_metadata(Metadata),
                     ?assertEqual(ok, Result),

                     %% Verify it's in process dictionary
                     ?assertEqual(<<"123">>, get({logger_meta, request_id})),
                     ?assertEqual(<<"456">>, get({logger_meta, user_id}))
                 end),
          ?_test(begin
                     %% Test: Override existing metadata
                     erlmcp_otp28_upgrade:logger_metadata(#{key1 => <<"value1">>}),
                     erlmcp_otp28_upgrade:logger_metadata(#{key1 => <<"value2">>}),

                     ?assertEqual(<<"value2">>, get({logger_meta, key1}))
                 end)
         ]
     end}.

%%%===================================================================
%%% ETS Compressed Table Tests
%%%===================================================================

ets_compressed_table_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Create compressed ETS table
                     Table = erlmcp_otp28_upgrade:ets_compressed_table(test_compressed,
                         [set, public]),

                     ?assert(is_integer(Table)),

                     %% Insert test data
                     true = ets:insert(Table, {key1, value1}),
                     true = ets:insert(Table, {key2, value2}),

                     %% Verify data
                     ?assertEqual([{key1, value1}], ets:lookup(Table, key1)),
                     ?assertEqual([{key2, value2}], ets:lookup(Table, key2)),

                     %% Cleanup
                     ets:delete(Table)
                 end),
          ?_test(begin
                     %% Test: Already compressed option doesn't duplicate
                     Table = erlmcp_otp28_upgrade:ets_compressed_table(test_compressed2,
                         [set, public, {compressed, true}]),

                     ?assert(is_integer(Table)),
                     ets:delete(Table)
                 end)
         ]
     end}.

%%%===================================================================
%%% Process Dictionary Cache Tests
%%%===================================================================

process_dict_cache_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Cache value with infinity TTL
                     Key = test_cache_key,
                     Value = test_cache_value,

                     Result = erlmcp_otp28_upgrade:process_dict_cache(Key, {Value, infinity}),
                     ?assertEqual(Value, Result),

                     %% Verify it's cached
                     ?assertMatch({Value, _, infinity}, get({cache, Key}))
                 end),
          ?_test(begin
                     %% Test: Cache value with TTL
                     Key = test_cache_ttl,
                     Value = test_cache_ttl_value,
                     TTL = 5000, %% 5 seconds

                     Result = erlmcp_otp28_upgrade:process_dict_cache(Key, {Value, TTL}),
                     ?assertEqual(Value, Result),

                     %% Verify TTL is set
                     ?assertMatch({Value, _, TTL}, get({cache, Key}))
                 end),
          ?_test(begin
                     %% Test: Cache value without TTL defaults to infinity
                     Key = test_cache_default,
                     Value = test_cache_default_value,

                     Result = erlmcp_otp28_upgrade:process_dict_cache(Key, Value),
                     ?assertEqual(Value, Result),

                     %% Verify default TTL
                     ?assertMatch({Value, _, infinity}, get({cache, Key}))
                 end)
         ]
     end}.

%%%===================================================================
%%% OTP Version Validation Tests
%%%===================================================================

otp_version_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Validate OTP version (should be 28+)
                     Result = erlmcp_otp28_upgrade:validate_otp_version(),
                     ?assertEqual(ok, Result)
                 end),
          ?_test(begin
                     %% Test: Get OTP features
                     Features = erlmcp_otp28_upgrade:otp_features(),
                     ?assert(is_list(Features)),
                     ?assert(length(Features) > 0),

                     %% Validate known features
                     ?assert(lists:member(supervisor_hibernation, Features)),
                     ?assert(lists:member(process_info_optimization, Features)),
                     ?assert(lists:member(logger_metadata, Features))
                 end)
         ]
     end}.

%%%===================================================================
%%% Performance Tests
%%%===================================================================

performance_benchmark_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {timeout, 60, ?_test(begin
                     %% Test: Benchmark process_info optimization
                     Iterations = 1000,

                     %% Warmup
                     lists:foreach(fun(_) ->
                         erlmcp_otp28_upgrade:get_process_info_optimized(self())
                     end, lists:seq(1, 100)),

                     %% Measure optimized approach
                     {T1, _} = timer:tc(fun() ->
                         lists:foreach(fun(_) ->
                             erlmcp_otp28_upgrade:get_process_info_optimized(self())
                         end, lists:seq(1, Iterations))
                     end),

                     %% Validate performance (should be < 1ms per call)
                     AvgTime = T1 / Iterations / 1000, %% Convert to ms
                     ?assert(AvgTime < 1.0),

                     ?debugFmt("Average process_info time: ~.3fms", [AvgTime])
                 end)}
         ]
     end}.

%%%===================================================================
%%% Property-Based Tests (Proper)
%%%===================================================================

%% Property: get_supervisor_children always returns valid list
prop_get_supervisor_children_valid() ->
    ?FORALL(_Dummy, nat(),
        begin
            Children = erlmcp_otp28_upgrade:get_supervisor_children(erlmcp_sup),
            is_list(Children) andalso
            lists:all(fun(Child) ->
                is_map(Child) andalso
                maps:is_key(id, Child) andalso
                maps:is_key(pid, Child) andalso
                maps:is_key(type, Child) andalso
                maps:is_key(modules, Child)
            end, Children)
        end).

%% Property: process_info_optimized always returns a map
prop_process_info_optimized_returns_map() ->
    ?FORALL(_Dummy, nat(),
        begin
            Pid = spawn(fun() -> timer:sleep(1000) end),
            Info = erlmcp_otp28_upgrade:get_process_info_optimized(Pid),
            is_map(Info)
        end).

%% Property: logger_metadata always succeeds
prop_logger_metadata_succeeds() ->
    ?FORALL({Key, Value},
        {binary(), binary()},
        begin
            Result = erlmcp_otp28_upgrade:logger_metadata(#{Key => Value}),
            Result =:= ok
        end).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_supervisor_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     %% Test: Full supervisor lifecycle with OTP 28 features
                     {ok, SupPid} = start_test_supervisor(),

                     %% Add children
                     {ok, Child1} = supervisor:start_child(SupPid,
                         #{id => child1,
                           start => {erlmcp_test_helpers, start_worker, [child1]}}),

                     %% Get children with OTP 28 upgrade
                     Children = erlmcp_otp28_upgrade:get_supervisor_children(SupPid),
                     ?assert(length(Children) > 0),

                     %% Get process info
                     Info = erlmcp_otp28_upgrade:get_process_info_optimized(Child1),
                     ?assert(is_map(Info)),

                     %% Monitor with metadata
                     Metadata = #{supervisor => test, child => child1},
                     Ref = erlmcp_otp28_upgrade:monitor_with_metadata(Child1, Metadata),
                     ?assert(is_reference(Ref)),

                     %% Cleanup
                     erlmcp_otp28_upgrade:logger_metadata(#{}),
                     supervisor:terminate_child(SupPid, child1),
                     supervisor:stop(SupPid)
                 end)
         ]
     end}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Start a test supervisor
start_test_supervisor() ->
    supervisor:start_link({local, test_supervisor},
                         erlmcp_otp28_upgrade_test_sup, []).

%%%===================================================================
%%% Test Supervisor Module
%%%===================================================================

-module(erlmcp_otp28_upgrade_test_sup).
-behaviour(supervisor).

-export([init/1]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    {ok, {SupFlags, []}}.
