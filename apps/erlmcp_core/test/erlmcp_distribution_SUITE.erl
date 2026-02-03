%%%-------------------------------------------------------------------
%%% @doc
%%% Test suite for OTP 26-28 distribution modules
%%%
%%% This suite tests the distribution compatibility across OTP versions,
%%% focusing on protocol negotiation, node management, and feature availability.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distribution_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([all/0, groups/0, init_per_suite/1, end_per_suite/1,
         init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([
    %% Version detection tests
    test_version_detection/1,
    test_feature_availability/1,
    test_support_level_assessment/1,

    %% Distribution registry tests
    test_registry_start_stop/1,
    test_entity_registration/1,
    test_cross_node_registration/1,
    test_entity_update/1,
    test_entity_listing/1,
    test_process_group_operations/1,

    %% Distribution manager tests
    test_manager_start_stop/1,
    test_node_connection/1,
    test_node_disconnection/1,
    test_node_monitoring/1,
    test_protocol_negotiation/1,

    %% Compatibility module tests
    test_compatibility_info/1,
    test_patch_application/1,
    test_node_compatibility_check/1,
    test_version_mismatch_handling/1,

    %% Integration tests
    test_end_to_end_distribution/1,
    test_cross_version_compatibility/1,
    test_performance_benchmarks/1,
    test_error_handling_scenarios/1
]).

%% Test groups
%%====================================================================
%% Test specification
%%====================================================================

all() -> [
    {group, version_tests},
    {group, registry_tests},
    {group, manager_tests},
    {group, compatibility_tests},
    {group, integration_tests}
].

groups() -> [
    {version_tests, [], [
        test_version_detection,
        test_feature_availability,
        test_support_level_assessment
    ]},
    {registry_tests, [], [
        test_registry_start_stop,
        test_entity_registration,
        test_cross_node_registration,
        test_entity_update,
        test_entity_listing,
        test_process_group_operations
    ]},
    {manager_tests, [], [
        test_manager_start_stop,
        test_node_connection,
        test_node_disconnection,
        test_node_monitoring,
        test_protocol_negotiation
    ]},
    {compatibility_tests, [], [
        test_compatibility_info,
        test_patch_application,
        test_node_compatibility_check,
        test_version_mismatch_handling
    ]},
    {integration_tests, [], [
        test_end_to_end_distribution,
        test_cross_version_compatibility,
        test_performance_benchmarks,
        test_error_handling_scenarios
    ]}
].

%%====================================================================
%% Test setup
%%====================================================================

init_per_suite(Config) ->
    %% Start applications
    ok = application:start(gproc),
    ok = application:start(pg),
    ok = application:start(logger),

    %% Log test start
    ct:log("Starting erlmcp distribution test suite"),

    %% Get OTP version for reference
    OTPVersion = erlang:system_info(otp_release),
    ct:log("Running on OTP ~s", [OTPVersion]),

    %% Store config
    [{otp_version, OTPVersion} | Config].

end_per_suite(Config) ->
    %% Stop applications
    application:stop(logger),
    application:stop(pg),
    application:stop(gproc),

    ct:log("Finished erlmcp distribution test suite"),
    Config.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),

    %% Start distribution modules
    start_distribution_modules(Config),

    %% Set up test data
    TestData = setup_test_data(TestCase, Config),

    [{test_case, TestCase}, {test_data, TestData} | Config].

end_per_testcase(TestCase, Config) ->
    ct:log("Ending test case: ~p", [TestCase]),

    %% Clean up test data
    cleanup_test_data(TestCase, Config),

    %% Stop distribution modules
    stop_distribution_modules(Config),

    Config.

%%====================================================================
%% Version Detection Tests
%%====================================================================

test_version_detection(_Config) ->
    %% Test version detection functionality
    Version = erlmcp_version_detector:otp_version(),
    ct:log("Detected OTP version: ~p", [Version]),

    %% Version should be a tuple
    ?assert(is_tuple(Version)),
    ?assert(tuple_size(Version) =:= 3),

    %% All elements should be non-negative integers
    [Major, Minor, Patch] = tuple_to_list(Version),
    ?assert(is_integer(Major) andalso Major >= 0),
    ?assert(is_integer(Minor) andalso Minor >= 0),
    ?assert(is_integer(Patch) andalso Patch >= 0),

    %% Version should be supported
    ?assert(erlmcp_version_detector:is_otp_supported()),

    ok.

test_feature_availability(_Config) ->
    %% Test feature detection
    Features = erlmcp_version_detector:get_optimal_features(),
    ct:log("Available features: ~p", [Features]),

    %% Features should be a map
    ?assert(is_map(Features)),

    %% Check specific features
    NativeJson = maps:get(native_json, Features, false),
    ProcessIterator = maps:get(process_iterator, Features, false),
    PriorityMessages = maps:get(priority_messages, Features, false),

    ct:log("Native JSON: ~p, Process Iterator: ~p, Priority Messages: ~p",
           [NativeJson, ProcessIterator, PriorityMessages]),

    %% Features should be consistent with OTP version
    Version = erlmcp_version_detector:otp_version(),

    %% OTP 27+ should have native JSON
    if
        Version >= {27, 0, 0} -> ?assert(NativeJson);
        true -> ok
    end,

    %% OTP 28+ should have process iterator and priority messages
    if
        Version >= {28, 0, 0} ->
            ?assert(ProcessIterator),
            ?assert(PriorityMessages);
        true -> ok
    end,

    ok.

test_support_level_assessment(_Config) ->
    %% Test support level assessment
    Version = erlmcp_version_detector:otp_version(),
    SupportLevel = erlmcp_version_detector:get_support_level(),
    ct:log("Version: ~p, Support Level: ~p", [Version, SupportLevel]),

    %% Support level should be correct
    ExpectedSupportLevel = case Version of
        {M, _, _} when M < 26 -> unsupported;
        {26, _, _} -> legacy;
        {27, _, _} -> stable;
        {M, _, _} when M >= 28 -> recommended
    end,

    ?assert(SupportLevel =:= ExpectedSupportLevel),

    %% Minimum version check
    ?assert(erlmcp_version_detector:is_version_at_least(Version, {26, 0, 0})),

    %% Version comparison
    ?assert(erlmcp_version_detector:compare_versions(Version, {26, 0, 0}) =/= lt),

    ok.

%%====================================================================
%% Distribution Registry Tests
%%====================================================================

test_registry_start_stop(_Config) ->
    %% Test registry start/stop
    {ok, Pid} = erlmcp_distribution_registry:start_link(),
    ?assert(is_pid(Pid)),

    %% Check if registry is running
    ?assert(erlmcp_distribution_registry:is_distributed()),

    %% Stop the registry
    ok = gen_server:stop(Pid),

    %% Verify it's stopped
    ?assert(erlmcp_distribution_registry:is_distributed() =:= false),

    ok.

test_entity_registration(_Config) ->
    %% Test entity registration
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Register a server entity
    Config = #{pid => self(), type => server},
    ok = erlmcp_distribution_registry:register(server, test_server, self(), Config),

    %% Verify registration
    {ok, {Node, RegisteredPid, RegisteredConfig}} =
        erlmcp_distribution_registry:whereis(server, test_server),

    ?assert(Node =:= node()),
    ?assert(RegisteredPid =:= self()),
    ?assert(RegisteredConfig =:= Config),

    %% Try to register the same entity again (should fail)
    {error, already_registered} =
        erlmcp_distribution_registry:register(server, test_server, self(), Config),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, test_server),
    ok = gen_server:stop(Pid),

    ok.

test_cross_node_registration(_Config) ->
    %% Test cross-node registration (simulated)
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Register with different node information
    Config = #{pid => self(), node => node()},
    ok = erlmcp_distribution_registry:register(server, cross_node_server, self(), Config),

    %% Verify registration
    {ok, {Node, RegisteredPid, RegisteredConfig}} =
        erlmcp_distribution_registry:whereis(server, cross_node_server),

    ?assert(Node =:= node()),
    ?assert(RegisteredPid =:= self()),
    ?assert(maps:get(node, RegisteredConfig, undefined) =:= node()),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, cross_node_server),
    ok = gen_server:stop(Pid),

    ok.

test_entity_update(_Config) ->
    %% Test entity configuration update
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Register initial entity
    InitialConfig = #{pid => self(), version => 1},
    ok = erlmcp_distribution_registry:register(server, updatable_server, self(), InitialConfig),

    %% Update configuration
    UpdatedConfig = InitialConfig#{version => 2, metadata => #{updated => true}},
    ok = erlmcp_distribution_registry:update(server, updatable_server, UpdatedConfig),

    %% Verify update
    {ok, {_Node, _, RetrievedConfig}} =
        erlmcp_distribution_registry:whereis(server, updatable_server),

    ?assert(maps:get(version, RetrievedConfig) =:= 2),
    ?assert(maps:get(metadata, RetrievedConfig) =:= #{updated => true}),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, updatable_server),
    ok = gen_server:stop(Pid),

    ok.

test_entity_listing(_Config) ->
    %% Test entity listing functionality
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Register multiple servers
    ok = erlmcp_distribution_registry:register(server, server1, self(), #{pid => self()}),
    ok = erlmcp_distribution_registry:register(server, server2, self(), #{pid => self()}),
    ok = erlmcp_distribution_registry:register(transport, transport1, self(), #{pid => self()}),

    %% List servers
    Servers = erlmcp_distribution_registry:list(server),
    ?assert(length(Servers) =:= 2),

    %% List transports
    Transports = erlmcp_distribution_registry:list(transport),
    ?assert(length(Transports) =:= 1),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, server1),
    ok = erlmcp_distribution_registry:unregister(server, server2),
    ok = erlmcp_distribution_registry:unregister(transport, transport1),
    ok = gen_server:stop(Pid),

    ok.

test_process_group_operations(_Config) ->
    %% Test process group operations
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Test process
    TestPid = self(),

    %% Join multiple groups
    ok = erlmcp_distribution_registry:join_group(test_group1, TestPid),
    ok = erlmcp_distribution_registry:join_group(test_group2, TestPid),

    %% Check group members
    Members1 = erlmcp_distribution_registry:get_group_members(test_group1),
    Members2 = erlmcp_distribution_registry:get_group_members(test_group2),

    ?assert(lists:member(TestPid, Members1)),
    ?assert(lists:member(TestPid, Members2)),

    %% Leave group
    ok = erlmcp_distribution_registry:leave_group(test_group1, TestPid),

    %% Verify left
    Members1After = erlmcp_distribution_registry:get_group_members(test_group1),
    ?assert(not lists:member(TestPid, Members1After)),
    ?assert(lists:member(TestPid, Members2)),

    %% Clean up
    ok = erlmcp_distribution_registry:leave_group(test_group2, TestPid),
    ok = gen_server:stop(Pid),

    ok.

%%====================================================================
%% Distribution Manager Tests
%%====================================================================

test_manager_start_stop(_Config) ->
    %% Test manager start/stop
    {ok, Pid} = erlmcp_distribution_manager:start_link(),
    ?assert(is_pid(Pid)),

    %% Check protocol
    Protocol = erlmcp_distribution_manager:get_distribution_protocol(),
    ?assert(is_tuple(Protocol)),

    %% Stop the manager
    ok = gen_server:stop(Pid),

    ok.

test_node_connection(_Config) ->
    %% Test node connection (simulated)
    {ok, Pid} = erlmcp_distribution_manager:start_link(),

    %% Set up test node (use local node for testing)
    TestNode = node(),

    %% Test connection to self (should work)
    Result = erlmcp_distribution_manager:connect_node(TestNode),
    ct:log("Connection result: ~p", [Result]),

    %% Check node info
    case erlmcp_distribution_manager:get_node_info(TestNode) of
        {ok, Info} ->
            ?assert(Info#node_info.node =:= TestNode);
        {error, not_found} ->
            % This is expected for self-node in some configurations
            ok
    end,

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_node_disconnection(_Config) ->
    %% Test node disconnection
    {ok, Pid} = erlmcp_distribution_manager:start_link(),

    %% Set up test node
    TestNode = node(),

    %% Connect and then disconnect
    ok = erlmcp_distribution_manager:connect_node(TestNode),
    ok = erlmcp_distribution_manager:disconnect_node(TestNode),

    %% Verify disconnection
    case erlmcp_distribution_manager:get_node_info(TestNode) of
        {error, not_found} ->
            ok;
        {ok, Info} ->
            % Should be disconnected
            ?assert(Info#node_info.status =/= connected)
    end,

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_node_monitoring(_Config) ->
    %% Test node monitoring
    {ok, Pid} = erlmcp_distribution_manager:start_link(),

    %% Test node availability check
    Available = erlmcp_distribution_manager:is_node_available(node()),
    ct:log("Local node available: ~p", [Available]),

    %% Test with non-existent node
    NonExistentNode = 'nonexistent@node',
    Available2 = erlmcp_distribution_manager:is_node_available(NonExistentNode),
    ?assert(Available2 =:= false),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_protocol_negotiation(_Config) ->
    %% Test protocol negotiation
    {ok, Pid} = erlmcp_distribution_manager:start_link(),

    %% Get current protocol
    Protocol = erlmcp_distribution_manager:get_distribution_protocol(),
    ct:log("Current protocol: ~p", [Protocol]),

    %% Change distribution mode
    erlmcp_distribution_manager:set_distribution_mode(optimal),

    %% Protocol should remain the same (based on OTP version)
    Protocol2 = erlmcp_distribution_manager:get_distribution_protocol(),
    ?assert(Protocol =:= Protocol2),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

%%====================================================================
%% Compatibility Module Tests
%%====================================================================

test_compatibility_info(_Config) ->
    %% Test compatibility information
    {ok, Pid} = erlmcp_distribution_compat:start_link(),

    %% Get compatibility info
    Info = erlmcp_distribution_compat:get_compatibility_info(),
    ct:log("Compatibility info: ~p", [Info]),

    %% Verify structure
    ?assert(is_map(Info)),
    ?assert(maps:is_key(current_version, Info)),
    ?assert(maps:is_key(optimization_level, Info)),
    ?assert(maps:is_key(applied_patches, Info)),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_patch_application(_Config) ->
    %% Test patch application
    {ok, Pid} = erlmcp_distribution_compat:start_link(),

    %% Apply patches
    Result = erlmcp_distribution_compat:apply_compatibility_patches(),
    ct:log("Patch application result: ~p", [Result]),

    %% Verify patches were applied
    Info = erlmcp_distribution_compat:get_compatibility_info(),
    ?assert(maps:get(applied_patches, Info) =/= []),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_node_compatibility_check(_Config) ->
    %% Test node compatibility checking
    {ok, Pid} = erlmcp_distribution_compat:start_link(),

    %% Check compatibility with local node
    Level = erlmcp_distribution_compat:check_node_compatibility(node()),
    ct:log("Local node compatibility: ~p", [Level]),

    %% Should be compatible
    ?assert(Level =/= incompatible),

    %% Check with non-existent node
    Level2 = erlmcp_distribution_compat:check_node_compatibility('nonexistent@node'),
    ct:log("Non-existent node compatibility: ~p", [Level2]),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

test_version_mismatch_handling(_Config) ->
    %% Test version mismatch handling
    {ok, Pid} = erlmcp_distribution_compat:start_link(),

    %% Simulate version mismatch
    erlmcp_distribution_compat:handle_version_mismatch('test@node', version_mismatch),

    %% Get updated compatibility info
    Info = erlmcp_distribution_compat:get_compatibility_info(),
    ct:log("Updated compatibility info: ~p", [Info]),

    %% Clean up
    ok = gen_server:stop(Pid),

    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

test_end_to_end_distribution(_Config) ->
    %% Test end-to-end distribution scenario
    ct:log("Testing end-to-end distribution scenario"),

    %% Start all modules
    {ok, RegistryPid} = erlmcp_distribution_registry:start_link(),
    {ok, ManagerPid} = erlmcp_distribution_manager:start_link(),
    {ok, CompatPid} = erlmcp_distribution_compat:start_link(),

    %% Apply compatibility patches
    ok = erlmcp_distribution_compat:apply_compatibility_patches(),

    %% Register multiple entities
    ok = erlmcp_distribution_registry:register(server, entity1, self(), #{pid => self()}),
    ok = erlmcp_distribution_registry:register(server, entity2, self(), #{pid => self()}),

    %% Connect to local node
    ok = erlmcp_distribution_manager:connect_node(node()),

    %% Verify everything is working
    {ok, _} = erlmcp_distribution_registry:whereis(server, entity1),
    {ok, _} = erlmcp_distribution_registry:whereis(server, entity2),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, entity1),
    ok = erlmcp_distribution_registry:unregister(server, entity2),
    ok = gen_server:stop(RegistryPid),
    ok = gen_server:stop(ManagerPid),
    ok = gen_server:stop(CompatPid),

    ok.

test_cross_version_compatibility(_Config) ->
    %% Test cross-version compatibility
    ct:log("Testing cross-version compatibility scenario"),

    %% Start modules
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Test with different entity types
    Entities = [
        {server, "server1", #{pid => self()}},
        {transport, "transport1", #{pid => self()}},
        {server, "server2", #{pid => self(), capabilities => #{}}}
    ],

    %% Register all entities
    lists:foreach(fun({Type, Id, Config}) ->
                    ok = erlmcp_distribution_registry:register(Type, Id, self(), Config)
                 end,
                 Entities),

    %% Verify all registrations
    lists:foreach(fun({Type, Id, _}) ->
                    {ok, {_Node, Pid, Config}} = erlmcp_distribution_registry:whereis(Type, Id),
                    ?assert(Pid =:= self())
                 end,
                 Entities),

    %% Clean up
    lists:foreach(fun({Type, Id, _}) ->
                    ok = erlmcp_distribution_registry:unregister(Type, Id)
                 end,
                 Entities),
    ok = gen_server:stop(Pid),

    ok.

test_performance_benchmarks(_Config) ->
    %% Test performance benchmarks
    ct:log("Running performance benchmarks"),

    %% Start registry
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Benchmark registration
    StartReg = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
                    ok = erlmcp_distribution_registry:register(server, "bench_" ++ integer_to_list(I), self(), #{pid => self()})
                 end,
                 lists:seq(1, 100)),
    EndReg = erlang:monotonic_time(millisecond),

    ct:log("Registration benchmark: ~p ms for 100 entities", [EndReg - StartReg]),

    %% Benchmark lookup
    StartLookup = erlang:monotonic_time(millisecond),
    lists:foreach(fun(I) ->
                    {ok, _} = erlmcp_distribution_registry:whereis(server, "bench_" ++ integer_to_list(I))
                 end,
                 lists:seq(1, 100)),
    EndLookup = erlang:monotonic_time(millisecond),

    ct:log("Lookup benchmark: ~p ms for 100 lookups", [EndLookup - StartLookup]),

    %% Clean up
    lists:foreach(fun(I) ->
                    ok = erlmcp_distribution_registry:unregister(server, "bench_" ++ integer_to_list(I))
                 end,
                 lists:seq(1, 100)),
    ok = gen_server:stop(Pid),

    ok.

test_error_handling_scenarios(_Config) ->
    %% Test error handling scenarios
    ct:log("Testing error handling scenarios"),

    %% Start registry
    {ok, Pid} = erlmcp_distribution_registry:start_link(),

    %% Test duplicate registration
    ok = erlmcp_distribution_registry:register(server, error_test, self(), #{pid => self()}),
    {error, already_registered} =
        erlmcp_distribution_registry:register(server, error_test, self(), #{pid => self()}),

    %% Test lookup of non-existent entity
    {error, not_found} = erlmcp_distribution_registry:whereis(server, non_existent),

    %% Test update of non-existent entity
    {error, not_found} =
        erlmcp_distribution_registry:update(server, non_existent, #{pid => self()}),

    %% Test unregister of non-existent entity
    ok = erlmcp_distribution_registry:unregister(server, non_existent),

    %% Clean up
    ok = erlmcp_distribution_registry:unregister(server, error_test),
    ok = gen_server:stop(Pid),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_distribution_modules(Config) ->
    %% Start required applications
    ok = application:start(gproc),
    ok = application:start(pg),
    ok = application:start(logger),

    %% Log start
    ct:log("Starting distribution modules"),

    %% Start distribution modules
    {ok, _} = erlmcp_distribution_registry:start_link(),
    {ok, _} = erlmcp_distribution_manager:start_link(),
    {ok, _} = erlmcp_distribution_compat:start_link(),

    ok.

stop_distribution_modules(_Config) ->
    %% Stop distribution modules
    application:stop(erlmcp_distribution_compat),
    application:stop(erlmcp_distribution_manager),
    application:stop(erlmcp_distribution_registry),

    %% Stop applications
    application:stop(logger),
    application:stop(pg),
    application:stop(gproc),

    ok.

setup_test_data(TestCase, Config) ->
    %% Set up test data specific to test case
    ct:log("Setting up test data for: ~p", [TestCase]),

    %% Create test data structure
    TestData = #{
        test_case => TestCase,
        created_at => erlang:monotonic_time(millisecond),
        entities => [],
        nodes => []
    },

    %% Initialize test entities
    TestData2 = lists:foldl(fun(I, Acc) ->
                    EntityId = "test_entity_" ++ integer_to_list(I),
                    Acc#{EntityId => #{type => server, id => EntityId, pid => self()}}
                 end,
                 TestData#{entities => []},
                 lists:seq(1, 5)),

    TestData2.

cleanup_test_data(TestCase, Config) ->
    %% Clean up test data specific to test case
    ct:log("Cleaning up test data for: ~p", [TestCase]),

    %% Clean up any remaining entities
    TestData = proplists:get_value(test_data, Config, #{}),
    lists:foldl(fun(EntityId, _Acc) ->
                    %% Clean up entity if it still exists
                    catch erlmcp_distribution_registry:unregister(server, EntityId)
                 end,
                 ok,
                 maps:keys(TestData)),

    ok.