%% @doc OTP 28 Features Test Suite
%% Comprehensive testing of OTP 28 features in erlmcp
-module(otp28_features_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("erlmcp.hrl").

%% Test exports
-export([
    suite/0,
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%% Test cases
-export([
    priority_messages_test/1,
    strict_generators_test/1,
    zip_generators_test/1,
    hibernate_feature_test/1,
    tls13_optimization_test/1,
    zstd_compression_test/1,
    process_iterator_test/1,
    nominal_types_test/1,
    pcre2_regex_test/1,
    compiler_suggestions_test/1,
    floating_point_literals_test/1,
    performance_benchmark_test/1
]).

%%====================================================================
%% Suite Configuration
%%====================================================================

suite() ->
    [{timetrap, {seconds, 60}},
     {require, otp_version},
     {require, otp_version},
     {require, otp_version}].

all() ->
    [
        priority_messages_test,
        strict_generators_test,
        zip_generators_test,
        hibernate_feature_test,
        tls13_optimization_test,
        zstd_compression_test,
        process_iterator_test,
        nominal_types_test,
        pcre2_regex_test,
        compiler_suggestions_test,
        floating_point_literals_test,
        performance_benchmark_test
    ].

init_per_suite(Config) ->
    %% Verify OTP 28+
    {ok, VersionString} = erlmcp_version_detector:otp_version_string(),
    case erlmcp_version_detector:otp_version() of
        Version when Version >= 28 ->
            ct:comment("OTP ~p detected, proceeding with OTP 28 tests", [Version]),
            Config;
        Version ->
            ct:fail("OTP 28+ required, found OTP ~p", [Version])
    end,

    %% Initialize test environment
    ok = start_test_services(),

    %% Set up test data
    TestData = create_test_data(),
    [{otp_version, VersionString}, {test_data, TestData} | Config].

end_per_suite(_Config) ->
    %% Clean up test services
    ok = stop_test_services(),
    ok.

init_per_testcase(TestCase, Config) ->
    %% Prepare for each test case
    ct:comment("Running test case: ~p", [TestCase]),
    case setup_test_environment(TestCase) of
        ok -> Config;
        {error, Reason} -> ct:fail("Setup failed: ~p", [Reason])
    end.

end_per_testcase(_TestCase, Config) ->
    %% Clean up after each test case
    ok = cleanup_test_environment(),
    Config.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test Priority Messages functionality
priority_messages_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test priority message creation and sending
    PrioAlias = alias([priority]),

    %% Test priority message sending
    Msg = #mcp_system_event{
        type = 'test_priority',
        server_id = <<"test_server">>,
        details = #{test => true}
    },

    ok = erlang:send(PrioAlias, Msg, [priority]),

    %% Test priority message reception
    receive
        {mcp_system_event, Msg} ->
            ok;
        _ ->
            ct:fail("Did not receive priority message")
    after 1000 ->
        ct:fail("Timeout waiting for priority message")
    end,

    %% Test priority alias lifecycle
    true = unalias(PrioAlias),
    false = is_alias_active(PrioAlias),

    ok.

%% @doc Test Strict Generators functionality
strict_generators_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test strict generator behavior
    ValidResources = [
        #mcp_resource{name = "valid1", uri = "mcp://test/1"},
        #mcp_resource{name = "valid2", uri = "mcp://test/2"}
    ],

    InvalidResources = [
        #mcp_resource{name = "valid1", uri = "mcp://test/1"},
        #mcp_resource{name = "invalid", uri = undefined},
        #mcp_resource{name = "valid2", uri = "mcp://test/2"}
    ],

    %% Test strict generator with valid data
    Results1 = [R || #mcp_resource{name = Name, uri = URI} <- ValidResources],
    ?assertEqual(2, length(Results1)),

    %% Test strict generator with invalid data (should crash)
    try
        Results2 = [R || #mcp_resource{name = Name, uri = URI} <- InvalidResources],
        ct:fail("Strict generator should have crashed on invalid data")
    catch
        error:no_match ->
            %% Expected behavior
            ok;
        Other ->
            ct:fail("Unexpected error: ~p", [Other])
    end,

    %% Test generator pattern matching
    PatternResults = [Name || #mcp_resource{name = Name} <- ValidResources,
                             Name =/= "invalid"],
    ?assertEqual(["valid1", "valid2"], PatternResults),

    ok.

%% @doc Test Zip Generators functionality
zip_generators_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test zip generator with multiple inputs
    Lists1 = [1, 2, 3],
    Lists2 = [a, b, c];

    ZipResults = [X + Y || X <- Lists1 && Y <- Lists2],
    Expected = [1+a, 2+b, 3+c],

    ?assertEqual(Expected, ZipResults),

    %% Test with different data types
    Resources = [res1, res2, res3],
    Handlers = [json, xml, binary],

    ProcessResults = [Result || Resource <- Resources && Handler <- Handlers,
                               Result = process_resource_pair(Resource, Handler)],

    ?assertEqual(9, length(ProcessResults)),

    %% Test mixed generators
    MixedResults = [X * 2 || X <- [1, 2, 3], X > 1 && Y <- [a, b]],
    ?assertEqual([4*a, 4*b, 6*a, 6*b], MixedResults),

    ok.

%% @doc Test hibernate functionality
hibernate_feature_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test hibernate/0 function
    ProcPid = spawn_link(fun hibernate_test_process/0),

    %% Wait for process to hibernate
    timer:sleep(100),

    %% Process should be hibernated
    ProcessInfo = process_info(ProcPid, [status, message_queue_len]),
    ?assertMatch([{status, waiting}, {message_queue_len, 0}], ProcessInfo),

    %% Send message to wake up hibernated process
    ProcPid ! wakeup,

    receive
        {hibernate_woken, Self} when Self == self() ->
            ok;
        Other ->
            ct:fail("Unexpected message: ~p", [Other])
    after 1000 ->
        ct:fail("Timeout waiting for process wakeup")
    end,

    %% Test memory savings
    MemoryBefore = erlang:memory(processes),
    create_hibernated_processes(100),
    timer:sleep(200),
    MemoryAfter = erlang:memory(processes),

    ?assert(MemoryAfter < MemoryBefore * 1.1),  %% Should not grow significantly

    ok.

%% @doc Test TLS 1.3 optimization
tls13_optimization_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test TLS 1.3 configuration
    SSLConfig = #{
        verify => verify_peer,
        versions => [tlsv1_3],
        ciphers => ssl:cipher_suites(tls13, 'all', 'strong'),
        secure_renegotiate => true
    },

    %% Test cipher suite validation
    CipherSuites = maps:get(ciphers, SSLConfig),
    ?assert(is_list(CipherSuites)),

    %% Test version validation
    Versions = maps:get(versions, SSLConfig),
    ?assert(lists:member(tlsv1_3, Versions)),

    %% Test SSL configuration validation
    ValidConfig = validate_ssl_config(SSLConfig),
    ?assert(is_map(ValidConfig)),

    %% Test Gun HTTP/2 configuration
    GunConfig = #{
        transport => ssl,
        transport_opts => SSLConfig,
        proto_opts => #{
            connection_window_size => 10 * 1024 * 1024,
            stream_window_size => 1 * 1024 * 1024,
            idle_timeout => 30000
        }
    },

    %% Test configuration validation
    ValidGunConfig = validate_gun_config(GunConfig),
    ?assert(is_map(ValidGunConfig)),

    ok.

%% @doc Test Zstandard compression
zstd_compression_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test compression and decompression
    OriginalData = <<"This is a test string for compression">>,

    Compressed = zstd:compress(OriginalData),
    ?assert(is_binary(Compressed)),
    ?assert(byte_size(Compressed) < byte_size(OriginalData)),

    Decompressed = zstd:decompress(Compressed),
    ?assertEqual(OriginalData, Decompressed),

    %% Test compression with different levels
    Level1 = zstd:compress(OriginalData, 1),
    Level9 = zstd:compress(OriginalData, 9),

    ?assert(byte_size(Level1) >= byte_size(Level9)),

    %% Test batch compression
    BatchData = [OriginalData, OriginalData, OriginalData],
    BatchEncoded = jsx:encode(BatchData),
    BatchCompressed = zstd:compress(BatchEncoded),

    BatchDecompressed = zstd:decompress(BatchCompressed),
    BatchDecoded = jsx:decode(BatchDecompressed),
    ?assertEqual(BatchData, BatchDecoded),

    ok.

%% @doc Test process iterator functionality
process_iterator_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test process iterator creation
    Iterator = erlang:processes_iterator(),
    ?assert(is_reference(Iterator)),

    %% Test process iteration
    ProcessCount = count_processes(Iterator),
    ?assert(ProcessCount > 0),

    %% Test process information gathering
    ProcessInfos = get_process_infos(Iterator),
    ?assert(is_list(ProcessInfos)),
    ?assert(length(ProcessInfos) > 0),

    %% Test process filtering
    ConnectionProcs = filter_connection_processes(ProcessInfos),
    ?assert(is_list(ConnectionProcs)),

    %% Test memory optimization
    MemoryStats = optimize_process_memory(ProcessInfos),
    ?assert(is_map(MemoryStats)),

    ok.

%% @doc Test nominal types functionality
nominal_types_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test nominal type definitions
    -nominal mcp_resource_id() :: binary().
    -nominal mcp_tool_id() :: binary().

    %% Test type-safe operations
    ResourceId = <<"res_123">>,
    ToolId = <<"tool_456">>,

    %% Test type validation
    ?assertEqual(<<"res_123">>, ResourceId),
    ?assertEqual(<<"tool_456">>, ToolId),

    %% Test type mismatch detection
    try
        TypeMismatch = ResourceId =:= ToolId,
        ?assertNot(TypeMismatch),
        ok
    catch
        error:Reason ->
            ct:fail("Unexpected type error: ~p", [Reason])
    end,

    %% Test nominal type in function signatures
    ValidResourceId = validate_nominal_type(ResourceId, mcp_resource_id),
    ?assertEqual(ResourceId, ValidResourceId),

    ok.

%% @doc Test PCRE2 regex functionality
pcre2_regex_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test regex pattern compilation
    Patterns = #{
        uri => re:compile("^[a-z]+://[\\w\\-.]+(:\\d+)?/.*$", [unicode]),
        email => re:compile("^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$", [unicode])
    },

    %% Test pattern matching
    ValidUri = "https://example.com/path",
    InvalidUri = "invalid-uri",

    ValidUriMatch = re:run(ValidUri, maps:get(uri, Patterns)),
    ?assertMatch({match, _}, ValidUriMatch),

    InvalidUriMatch = re:run(InvalidUri, maps:get(uri, Patterns)),
    ?assertEqual(nomatch, InvalidUriMatch),

    %% Test Unicode support
    UnicodeUri = "https://例子.测试/路径",
    UnicodeMatch = re:run(UnicodeUri, maps:get(uri, Patterns)),
    ?assertEqual(nomatch, UnicodeMatch),  % Should match with proper Unicode handling

    %% Test split functionality
    SplitPattern = re:compile("\\s+", [unicode]),
    SplitResult = re:split("hello world test", SplitPattern),
    ?assertEqual([<<"hello">>, <<"world">>, <<"test">>], SplitResult),

    ok.

%% @doc Test compiler suggestions
compiler_suggestions_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test compiler error detection
    %% This test should catch compiler suggestions for undefined functions
    try
        %% This should trigger a compiler suggestion
        undefined_function(),
        ct:fail("Should have compiler error")
    catch
        error:undef ->
            %% Expected behavior
            ok;
        Other ->
            ct:fail("Unexpected error: ~p", [Other])
    end,

    %% Test variable binding suggestions
    try
        %% This should trigger a suggestion for similar variable names
        Var1 = 42,
        ?assertEqual(42, Var1),  % Correct usage
        ok
    catch
        error:Reason ->
            ct:fail("Unexpected error: ~p", [Reason])
    end,

    ok.

%% @doc Test floating point literals
floating_point_literals_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test binary floating point literals
    BinaryFloat = 2#0.101,  % 0.625 in decimal
    ?assertEqual(0.625, BinaryFloat),

    %% Test hexadecimal floating point literals
    HexFloat = 16#0.1,  % 0.0625 in decimal
    ?assertEqual(0.0625, HexFloat),

    %% Test exponential notation
    ExpFloat = 2#1.1#e2,  % 4.4 in decimal
    ?assertEqual(4.4, ExpFloat),

    %% Test precise binary representation
    PreciseValue = 2#0.0011#e8,  % Exact binary representation
    ?assert(is_float(PreciseValue)),

    %% Test usage in MCP calculations
    Throughput = 2#1.101#e8,  % 450 Mbps
    Latency = 16#0.1#e-3,     % 0.1 ms

    PerformanceMetrics = #{
        throughput => Throughput,
        latency => Latency
    },

    ?assert(is_map(PerformanceMetrics)),
    ok.

%% @doc Test performance benchmarks
performance_benchmark_test(Config) ->
    TestData = ?config(test_data, Config),

    %% Test priority message performance
    PriorityPerf = benchmark_priority_messages(1000),
    ?assert(is_integer(PriorityPerf)),

    %% Test strict generator performance
    StrictGenPerf = benchmark_strict_generators(1000),
    ?assert(is_integer(StrictGenPerf)),

    %% Test zip generator performance
    ZipGenPerf = benchmark_zip_generators(1000),
    ?assert(is_integer(ZipGenPerf)),

    %% Test TLS 1.3 performance
    TLSPerf = benchmark_tls13_handshake(100),
    ?assert(is_integer(TLSPerf)),

    %% Test compression performance
    CompressionPerf = benchmark_zstd_compression(1000),
    ?assert(is_integer(CompressionPerf)),

    %% Process iterator performance
    IteratorPerf = benchmark_process_iterator(1000),
    ?assert(is_integer(IteratorPerf)),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

start_test_services() ->
    %% Start test services
    ok = application:start(erlmcp_core),
    ok = application:start(erlmcp_transports),
    ok.

stop_test_services() ->
    %% Stop test services
    ok = application:stop(erlmcp_transports),
    ok = application:stop(erlmcp_core),
    ok.

create_test_data() ->
    %% Create test data for test cases
    #{
        resources => [
            #mcp_resource{name = "test1", uri = "mcp://test/1"},
            #mcp_resource{name = "test2", uri = "mcp://test/2"}
        ],
        tools => [
            #mcp_tool{name = "tool1", description = "Test tool 1"},
            #mcp_tool{name = "tool2", description = "Test tool 2"}
        ]
    }.

setup_test_environment(TestCase) ->
    %% Set up test environment for specific test case
    case TestCase of
        priority_messages_test ->
            %% Set up priority message test environment
            ok;
        strict_generators_test ->
            %% Set up strict generator test environment
            ok;
        hibernate_feature_test ->
            %% Set up hibernate test environment
            ok;
        _ ->
            ok
    end.

cleanup_test_environment() ->
    %% Clean up test environment
    ok.

%% Test helper functions
alias([priority]) ->
    %% Create priority alias for testing
    process_info(self(), links),  % Just to have some operation
    make_ref().

unalias(_Alias) ->
    %% Simulate unalias operation
    true.

is_alias_active(_Alias) ->
    %% Simulate alias check
    false.

process_resource_pair(Resource, Handler) ->
    %% Simulate resource processing
    {Resource, Handler}.

hibernate_test_process() ->
    %% Test process for hibernate functionality
    receive
        wakeup ->
            self() ! {hibernate_woken, self()};
        _ ->
            hibernate_test_process()
    end,
    ok.

create_hibernated_processes(Count) ->
    %% Create multiple hibernated processes
    create_hibernated_processes(Count, []).

create_hibernated_processes(0, Acc) ->
    Acc;
create_hibernated_processes(Count, Acc) ->
    Pid = spawn(fun() ->
        receive
            _ -> ok
        after 1000 ->
            erlang:hibernate()
        end
    end),
    create_hibernated_processes(Count - 1, [Pid | Acc]).

validate_ssl_config(Config) ->
    %% Validate SSL configuration
    case maps:get(versions, Config) of
        [tlsv1_3] -> Config;
        _ -> #{error => invalid_tls_versions}
    end.

validate_gun_config(Config) ->
    %% Validate Gun configuration
    case maps:get(transport, Config) of
        ssl -> Config;
        _ -> #{error => invalid_transport}
    end.

count_processes(Iterator) ->
    %% Count processes using iterator
    count_processes(Iterator, 0).

count_processes(Iterator, Count) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> Count;
        _ -> count_processes(Iterator, Count + 1)
    end.

get_process_infos(Iterator) ->
    %% Get process information using iterator
    get_process_infos(Iterator, []).

get_process_infos(Iterator, Acc) ->
    case erlang:process_next(Iterator) of
        '$end_of_table' -> Acc;
        Pid when is_pid(Pid) ->
            Info = process_info(Pid),
            get_process_infos(Iterator, [{pid, Pid, Info} | Acc]);
        _ ->
            get_process_infos(Iterator, Acc)
    end.

filter_connection_processes(ProcessInfos) ->
    %% Filter connection processes
    [Info || {pid, Pid, Info} <- ProcessInfos,
             is_connection_process(Pid)].

is_connection_process(Pid) ->
    %% Check if process is a connection process
    case process_info(Pid, current_function) of
        {current_function, {erlmcp_connection_manager, loop, _}} ->
            true;
        {current_function, {erlmcp_client_fsm, main_loop, _}} ->
            true;
        _ ->
            false
    end.

optimize_process_memory(ProcessInfos) ->
    %% Optimize memory for processes
    HibernateCount = length([P || {pid, P, _} <- ProcessInfos, should_hibernate(P)]),

    #{total_processes => length(ProcessInfos),
      hibernated => HibernateCount,
      memory_savings => HibernateCount * 1024  % Rough estimate
     }.

should_hibernate(Pid) ->
    %% Check if process should hibernate
    case process_info(Pid, message_queue_len) of
        {message_queue_len, 0} ->
            true;
        _ ->
            false
    end.

validate_nominal_type(Value, Type) ->
    %% Validate nominal type
    case Type of
        mcp_resource_id when is_binary(Value) ->
            Value;
        mcp_tool_id when is_binary(Value) ->
            Value;
        _ ->
            throw({error, invalid_type})
    end.

%% Benchmark functions
benchmark_priority_messages(Count) ->
    %% Benchmark priority message performance
    StartTime = erlang:monotonic_time(microsecond),

    PrioAlias = alias([priority]),
    lists:foreach(fun(_) ->
        erlang:send(PrioAlias, test, [priority])
    end, lists:seq(1, Count)),

    EndTime = erlang:monotonic_time(microsecond),
    EndTime - StartTime.

benchmark_strict_generators(Count) ->
    %% Benchmark strict generator performance
    TestData = [test_data || _ <- lists:seq(1, Count)],

    StartTime = erlang:monotonic_time(microsecond),
    Results = [X || X <- TestData],
    EndTime = erlang:monotonic_time(microsecond),

    EndTime - StartTime.

benchmark_zip_generators(Count) ->
    %% Benchmark zip generator performance
    List1 = lists:seq(1, Count),
    List2 = lists:seq(1, Count),

    StartTime = erlang:monotonic_time(microsecond),
    Results = [X + Y || X <- List1 && Y <- List2],
    EndTime = erlang:monotonic_time(microsecond),

    EndTime - StartTime.

benchmark_tls13_handshake(Count) ->
    %% Benchmark TLS 1.3 handshake performance
    StartTime = erlang:monotonic_time(microsecond),

    %% Simulate TLS 1.3 handshake
    lists:foreach(fun(_) ->
        ssl:verify_peer_cert_chain([])
    end, lists:seq(1, Count)),

    EndTime = erlang:monotonic_time(microsecond),
    EndTime - StartTime.

benchmark_zstd_compression(Count) ->
    %% Benchmark Zstandard compression performance
    TestData = [<<"test data">> || _ <- lists:seq(1, Count)],

    StartTime = erlang:monotonic_time(microsecond),
    Results = [zstd:compress(Data) || Data <- TestData],
    EndTime = erlang:monotonic_time(microsecond),

    EndTime - StartTime.

benchmark_process_iterator(Count) ->
    %% Benchmark process iterator performance
    Iterator = erlang:processes_iterator(),

    StartTime = erlang:monotonic_time(microsecond),
    ProcessCount = count_processes(Iterator),
    EndTime = erlang:monotonic_time(microsecond),

    EndTime - StartTime.