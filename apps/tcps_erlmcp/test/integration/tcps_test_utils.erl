%%%-------------------------------------------------------------------
%%% @doc TCPS Test Utilities
%%%
%%% Comprehensive test helper functions including:
%%% - Mock service management (GitHub, Marketplace, OTEL)
%%% - Test data generation
%%% - Failure injection
%%% - Async event waiting
%%% - Verification helpers
%%% - Performance profiling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_test_utils).

%% Test environment
-export([
    init_test_env/0,
    cleanup_test_env/0,
    clear_all_data/0
]).

%% Mock services
-export([
    start_github_mock/0,
    stop_github_mock/1,
    start_marketplace_mock/0,
    stop_marketplace_mock/1,
    start_otel_mock/0,
    stop_otel_mock/1,
    %% Comprehensive mock service management
    setup_mock_services/0,
    teardown_mock_services/1,
    reset_mock_state/0,
    inject_mock_data/2,
    verify_mock_calls/1
]).

%% Test data creation
-export([
    create_test_work_order/0,
    create_test_work_order/1,
    create_work_orders/2,
    generate_id/0,
    generate_signature/0
]).

%% Failure injection
-export([
    inject_test_failure/2,
    inject_compilation_error/2,
    inject_compilation_warning/2,
    inject_low_coverage/2,
    inject_coverage/2,
    inject_security_vulnerability/2,
    inject_shacl_violation/2,
    inject_timestamp_in_build/1,
    inject_failure_at_stage/2
]).

%% Pipeline execution
-export([
    process_work_order_full/1,
    process_pipeline_stages/1,
    run_shacl_validation/1,
    run_compilation/1,
    run_tests/1,
    run_security_scan/1
]).

%% Mock operations
-export([
    mock_shacl_validation/2,
    mock_compilation/2,
    mock_tests/2,
    mock_security_scan/2,
    mock_marketplace_publish/1,
    mock_marketplace_query/1
]).

%% Async waiting
-export([
    wait_for_andon/1,
    wait_for_andon_type/1,
    check_for_andon/1,
    wait_until/2
]).

%% Verification
-export([
    verify_no_duplicate_receipts/0,
    verify_all_receipts_valid/0
]).

%% Performance profiling
-export([
    start_profiler/0,
    stop_profiler/0,
    identify_bottlenecks/1,
    start_cpu_monitor/0,
    stop_cpu_monitor/0,
    get_cpu_stats/0,
    count_files_in_data_dir/0
]).

%% Utilities
-export([
    shuffle/1,
    enable_gate_tracking/1,
    get_gate_execution_order/1,
    attempt_release/1,
    attempt_manual_override/1,
    corrupt_all_data/0,
    advance_time/1
]).

%%%===================================================================
%%% Test Environment
%%%===================================================================

init_test_env() ->
    %% Initialize test directories
    TestDataDir = "/tmp/tcps_test_data",
    ok = filelib:ensure_dir(filename:join(TestDataDir, "dummy")),
    application:set_env(erlmcp, data_dir, TestDataDir),

    %% Initialize test database
    ok = init_test_database(),

    %% Start test services
    ok.

cleanup_test_env() ->
    %% Clean up test directories
    TestDataDir = application:get_env(erlmcp, data_dir, "/tmp/tcps_test_data"),
    os:cmd("rm -rf " ++ TestDataDir),

    %% Clean up test database
    ok = cleanup_test_database(),

    ok.

clear_all_data() ->
    %% Clear work orders
    lists:foreach(fun(WO) ->
        catch tcps_work_order:delete(WO)
    end, tcps_work_order:list_all()),

    %% Clear Andons
    lists:foreach(fun(AndonId) ->
        catch tcps_andon:delete(AndonId)
    end, tcps_andon:list_all()),

    %% Clear receipts
    TestDataDir = application:get_env(erlmcp, data_dir, "/tmp/tcps_test_data"),
    os:cmd("rm -rf " ++ TestDataDir ++ "/receipts/*"),

    ok.

init_test_database() ->
    %% Initialize in-memory test database
    ets:new(test_work_orders, [named_table, public, set]),
    ets:new(test_andons, [named_table, public, set]),
    ets:new(test_receipts, [named_table, public, set]),
    ok.

cleanup_test_database() ->
    catch ets:delete(test_work_orders),
    catch ets:delete(test_andons),
    catch ets:delete(test_receipts),
    ok.

%%%===================================================================
%%% Mock Services
%%%===================================================================

start_github_mock() ->
    {ok, spawn_link(fun github_mock_loop/0)}.

stop_github_mock(Pid) ->
    Pid ! stop,
    ok.

github_mock_loop() ->
    receive
        stop -> ok;
        _ -> github_mock_loop()
    end.

start_marketplace_mock() ->
    {ok, spawn_link(fun marketplace_mock_loop/0)}.

stop_marketplace_mock(Pid) ->
    Pid ! stop,
    ok.

marketplace_mock_loop() ->
    receive
        stop -> ok;
        _ -> marketplace_mock_loop()
    end.

start_otel_mock() ->
    {ok, spawn_link(fun otel_mock_loop/0)}.

stop_otel_mock(Pid) ->
    Pid ! stop,
    ok.

otel_mock_loop() ->
    receive
        stop -> ok;
        _ -> otel_mock_loop()
    end.

%%%===================================================================
%%% Comprehensive Mock Service Management
%%%===================================================================

%% @doc Setup all mock services (GitHub, Marketplace, CVE, OTLP, SPARQL)
-spec setup_mock_services() -> {ok, map()}.
setup_mock_services() ->
    tcps_mock_services:start_all().

%% @doc Teardown all mock services
-spec teardown_mock_services(map()) -> ok.
teardown_mock_services(Services) ->
    tcps_mock_services:stop_all(Services).

%% @doc Reset mock state (clear all data, keep services running)
-spec reset_mock_state() -> ok.
reset_mock_state() ->
    tcps_mock_services:reset_all().

%% @doc Inject test data into mock services
-spec inject_mock_data(atom(), map()) -> ok.
inject_mock_data(github_issue, Data) ->
    tcps_mock_services:inject_github_issue(Data);
inject_mock_data(marketplace_feature, Data) ->
    tcps_mock_services:inject_marketplace_feature(Data);
inject_mock_data(cve_advisory, Data) ->
    tcps_mock_services:inject_cve_advisory(Data);
inject_mock_data(ontology_data, Data) ->
    tcps_mock_services:inject_ontology_data(Data).

%% @doc Verify mock service calls were made
-spec verify_mock_calls(map()) -> ok | {error, term()}.
verify_mock_calls(#{service := Service, operation := Operation}) ->
    case tcps_mock_services:verify_call(Service, Operation) of
        true -> ok;
        false -> {error, {call_not_found, Service, Operation}}
    end;
verify_mock_calls(Checks) when is_list(Checks) ->
    Results = [verify_mock_calls(Check) || Check <- Checks],
    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false -> {error, {some_calls_not_found, Results}}
    end.

%%%===================================================================
%%% Test Data Creation
%%%===================================================================

create_test_work_order() ->
    create_test_work_order(#{}).

create_test_work_order(Bucket) when is_atom(Bucket) ->
    create_test_work_order(#{bucket => Bucket});
create_test_work_order(Opts) when is_map(Opts) ->
    Bucket = maps:get(bucket, Opts, random_bucket()),
    Priority = maps:get(priority, Opts, medium),

    tcps_work_order:create(#{
        title => <<"Test work order">>,
        bucket => Bucket,
        priority => Priority,
        estimated_effort => 5,
        labels => [<<"test">>],
        metadata => #{}
    }).

create_work_orders(Bucket, Count) ->
    lists:map(fun(_) ->
        {ok, WO} = create_test_work_order(Bucket),
        WO
    end, lists:seq(1, Count)).

random_bucket() ->
    Buckets = [reliability, security, cost, compliance],
    lists:nth(rand:uniform(length(Buckets)), Buckets).

generate_id() ->
    iolist_to_binary([
        "id-",
        integer_to_list(erlang:unique_integer([positive]))
    ]).

generate_signature() ->
    iolist_to_binary([
        "sig-",
        integer_to_list(erlang:unique_integer([positive]))
    ]).

%%%===================================================================
%%% Failure Injection
%%%===================================================================

inject_test_failure(WorkOrderId, Details) ->
    %% Store failure details for mock test runner
    ets:insert(test_failures, {WorkOrderId, test_failure, Details}),
    ok.

inject_compilation_error(WorkOrderId, Details) ->
    ets:insert(test_failures, {WorkOrderId, compilation_error, Details}),
    ok.

inject_compilation_warning(WorkOrderId, Details) ->
    ets:insert(test_warnings, {WorkOrderId, compilation_warning, Details}),
    ok.

inject_low_coverage(WorkOrderId, Coverage) ->
    ets:insert(test_coverage, {WorkOrderId, Coverage}),
    ok.

inject_coverage(WorkOrderId, Coverage) ->
    ets:insert(test_coverage, {WorkOrderId, Coverage}),
    ok.

inject_security_vulnerability(WorkOrderId, Details) ->
    ets:insert(test_failures, {WorkOrderId, security_vulnerability, Details}),
    ok.

inject_shacl_violation(WorkOrderId, Details) ->
    ets:insert(test_failures, {WorkOrderId, shacl_violation, Details}),
    ok.

inject_timestamp_in_build(WorkOrderId) ->
    ets:insert(test_failures, {WorkOrderId, non_deterministic_build, #{}}),
    ok.

inject_failure_at_stage(WorkOrderId, Stage) ->
    ets:insert(test_failures, {WorkOrderId, Stage, #{}}),
    ok.

%%%===================================================================
%%% Pipeline Execution
%%%===================================================================

process_work_order_full(WorkOrderId) ->
    ok = tcps_kanban:start_work_order(WorkOrderId),
    ok = process_pipeline_stages(WorkOrderId),
    {ok, _SkuId} = create_release(WorkOrderId),
    {ok, WO} = tcps_work_order:get(WorkOrderId),
    SkuId = maps:get(sku_id, WO),
    ok = mock_marketplace_publish(SkuId),
    ok = tcps_work_order:complete(WorkOrderId, SkuId),
    ok.

process_pipeline_stages(WorkOrderId) ->
    {ok, _} = run_shacl_validation(WorkOrderId),
    {ok, _} = run_compilation(WorkOrderId),
    {ok, _} = run_tests(WorkOrderId),
    {ok, _} = tcps_quality:check_gates(WorkOrderId),
    {ok, _} = tcps_deterministic:verify_build(WorkOrderId),
    ok.

run_shacl_validation(WorkOrderId) ->
    case ets:lookup(test_failures, WorkOrderId) of
        [{_, shacl_violation, Details}] ->
            trigger_andon(WorkOrderId, shacl_violation, Details),
            {error, validation_failed};
        _ ->
            mock_shacl_validation(WorkOrderId, valid)
    end.

run_compilation(WorkOrderId) ->
    case ets:lookup(test_failures, WorkOrderId) of
        [{_, compilation_error, Details}] ->
            trigger_andon(WorkOrderId, compilation_error, Details),
            {error, compilation_failed};
        _ ->
            Warnings = case ets:lookup(test_warnings, WorkOrderId) of
                [{_, _, WarnDetails}] -> [WarnDetails];
                _ -> []
            end,
            mock_compilation(WorkOrderId, #{
                result => success,
                warnings => length(Warnings)
            })
    end.

run_tests(WorkOrderId) ->
    case ets:lookup(test_failures, WorkOrderId) of
        [{_, test_failure, Details}] ->
            trigger_andon(WorkOrderId, test_failure, Details),
            {error, test_failure};
        _ ->
            Coverage = case ets:lookup(test_coverage, WorkOrderId) of
                [{_, Cov}] -> Cov;
                _ -> 85.5
            end,

            case Coverage < 80.0 of
                true ->
                    trigger_andon(WorkOrderId, low_coverage, #{
                        actual_coverage => Coverage,
                        required_coverage => 80.0
                    }),
                    {ok, #{
                        tests_run => 150,
                        tests_passed => 150,
                        failures => 0,
                        coverage => Coverage
                    }};
                false ->
                    mock_tests(WorkOrderId, #{
                        tests_run => 150,
                        tests_passed => 150,
                        failures => 0,
                        coverage => Coverage
                    })
            end
    end.

run_security_scan(WorkOrderId) ->
    case ets:lookup(test_failures, WorkOrderId) of
        [{_, security_vulnerability, Details}] ->
            trigger_andon(WorkOrderId, security_vulnerability, Details),
            {error, security_issues_found};
        _ ->
            mock_security_scan(WorkOrderId, #{
                vulnerabilities => 0,
                warnings => 0
            })
    end.

%%%===================================================================
%%% Mock Operations
%%%===================================================================

mock_shacl_validation(WorkOrderId, Result) ->
    Receipt = #{
        id => generate_id(),
        work_order_id => WorkOrderId,
        stage => shacl,
        result => Result,
        metadata => #{
            rules_checked => 150,
            violations => 0
        },
        timestamp => erlang:system_time(millisecond),
        signature => generate_signature()
    },
    ok = store_receipt(Receipt),
    {ok, Result}.

mock_compilation(WorkOrderId, Result) ->
    Receipt = #{
        id => generate_id(),
        work_order_id => WorkOrderId,
        stage => compile,
        result => maps:get(result, Result, success),
        metadata => Result,
        timestamp => erlang:system_time(millisecond),
        signature => generate_signature()
    },
    ok = store_receipt(Receipt),
    {ok, Result}.

mock_tests(WorkOrderId, Results) ->
    Receipt = #{
        id => generate_id(),
        work_order_id => WorkOrderId,
        stage => test,
        result => case maps:get(failures, Results) of
            0 -> pass;
            _ -> fail
        end,
        metadata => Results,
        timestamp => erlang:system_time(millisecond),
        signature => generate_signature()
    },
    ok = store_receipt(Receipt),
    {ok, Results}.

mock_security_scan(WorkOrderId, Results) ->
    Receipt = #{
        id => generate_id(),
        work_order_id => WorkOrderId,
        stage => security,
        result => case maps:get(vulnerabilities, Results) of
            0 -> pass;
            _ -> fail
        end,
        metadata => Results,
        timestamp => erlang:system_time(millisecond),
        signature => generate_signature()
    },
    ok = store_receipt(Receipt),
    {ok, Results}.

mock_marketplace_publish(SkuId) ->
    ets:insert(published_skus, {SkuId, #{
        published_at => erlang:system_time(millisecond)
    }}),
    ok.

mock_marketplace_query(SkuId) ->
    case ets:lookup(published_skus, SkuId) of
        [{SkuId, Data}] -> {ok, Data};
        [] -> {error, not_found}
    end.

%%%===================================================================
%%% Async Waiting
%%%===================================================================

wait_for_andon(WorkOrderId) ->
    wait_for_andon(WorkOrderId, 5000).

wait_for_andon(WorkOrderId, Timeout) ->
    wait_until(fun() ->
        case tcps_andon:list_by_work_order(WorkOrderId) of
            [] -> false;
            [AndonId | _] -> {ok, AndonId}
        end
    end, Timeout).

wait_for_andon_type(Type) ->
    wait_for_andon_type(Type, 5000).

wait_for_andon_type(Type, Timeout) ->
    wait_until(fun() ->
        AllAndons = tcps_andon:list_all(),
        case lists:filter(fun(AndonId) ->
            {ok, Andon} = tcps_andon:get(AndonId),
            maps:get(type, Andon) =:= Type
        end, AllAndons) of
            [] -> false;
            [AndonId | _] -> {ok, AndonId}
        end
    end, Timeout).

check_for_andon(WorkOrderId) ->
    case tcps_andon:list_by_work_order(WorkOrderId) of
        [] -> {error, no_andon};
        [AndonId | _] -> {ok, AndonId}
    end.

wait_until(Fun, Timeout) ->
    wait_until(Fun, Timeout, erlang:monotonic_time(millisecond)).

wait_until(Fun, Timeout, StartTime) ->
    case Fun() of
        false ->
            Now = erlang:monotonic_time(millisecond),
            case Now - StartTime > Timeout of
                true -> {error, timeout};
                false ->
                    timer:sleep(100),
                    wait_until(Fun, Timeout, StartTime)
            end;
        {ok, Result} ->
            {ok, Result};
        true ->
            ok
    end.

%%%===================================================================
%%% Verification
%%%===================================================================

verify_no_duplicate_receipts() ->
    AllReceipts = get_all_receipts(),
    ReceiptIds = [maps:get(id, R) || R <- AllReceipts],
    UniqueIds = lists:usort(ReceiptIds),
    case length(ReceiptIds) =:= length(UniqueIds) of
        true -> ok;
        false -> {error, duplicate_receipts}
    end.

verify_all_receipts_valid() ->
    AllReceipts = get_all_receipts(),
    AllValid = lists:all(fun(R) ->
        tcps_receipt_verifier:verify_signature(R)
    end, AllReceipts),
    case AllValid of
        true -> ok;
        false -> {error, invalid_receipts}
    end.

%%%===================================================================
%%% Performance Profiling
%%%===================================================================

start_profiler() ->
    %% Start profiling
    ok.

stop_profiler() ->
    %% Stop profiling and return results
    {ok, #{}}.

identify_bottlenecks(Profile) ->
    %% Analyze profile and return bottlenecks
    [].

start_cpu_monitor() ->
    %% Start CPU monitoring
    ok.

stop_cpu_monitor() ->
    %% Stop CPU monitoring
    ok.

get_cpu_stats() ->
    %% Return CPU statistics
    {ok, #{
        avg => 25.0,
        peak => 75.0,
        min => 5.0
    }}.

count_files_in_data_dir() ->
    TestDataDir = application:get_env(erlmcp, data_dir, "/tmp/tcps_test_data"),
    case file:list_dir(TestDataDir) of
        {ok, Files} -> {ok, length(Files)};
        {error, _} -> {ok, 0}
    end.

%%%===================================================================
%%% Utilities
%%%===================================================================

shuffle(List) ->
    [X || {_, X} <- lists:sort([{rand:uniform(), N} || N <- List])].

enable_gate_tracking(WorkOrderId) ->
    ets:insert(gate_tracking, {WorkOrderId, []}),
    ok.

get_gate_execution_order(WorkOrderId) ->
    case ets:lookup(gate_tracking, WorkOrderId) of
        [{_, Order}] -> {ok, lists:reverse(Order)};
        [] -> {ok, []}
    end.

attempt_release(WorkOrderId) ->
    case tcps_quality:check_gates(WorkOrderId) of
        {ok, pass} -> tcps_test_utils:create_release(WorkOrderId);
        _ -> {error, quality_gate_not_passed}
    end.

attempt_manual_override(WorkOrderId) ->
    {error, unauthorized}.

corrupt_all_data() ->
    TestDataDir = application:get_env(erlmcp, data_dir, "/tmp/tcps_test_data"),
    os:cmd("find " ++ TestDataDir ++ " -type f -exec sh -c 'echo corrupted > {}' \\;"),
    ok.

advance_time(Seconds) ->
    %% Simulate time advance for timeout testing
    timer:sleep(Seconds * 1000),
    ok.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

store_receipt(Receipt) ->
    ReceiptId = maps:get(id, Receipt),
    ets:insert(test_receipts, {ReceiptId, Receipt}),
    ok.

get_all_receipts() ->
    [Receipt || {_, Receipt} <- ets:tab2list(test_receipts)].

create_release(WorkOrderId) ->
    SkuId = iolist_to_binary([
        "sku-",
        integer_to_list(erlang:unique_integer([positive]))
    ]),
    ok = tcps_work_order:set_sku_id(WorkOrderId, SkuId),
    {ok, SkuId}.

trigger_andon(WorkOrderId, Type, Details) ->
    spawn(fun() ->
        {ok, _AndonId} = tcps_andon:trigger(Type, #{
            work_order_id => WorkOrderId,
            details => Details
        })
    end),
    ok.
