%%%-----------------------------------------------------------------------------
%%% @doc TCPS Andon System Usage Example
%%%
%%% Demonstrates real-world usage of the Andon stop-the-line system in a
%%% CI/CD pipeline context.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(andon_example).

-export([
    example_full_pipeline/0,
    example_test_failure/0,
    example_resolution/0,
    example_concurrent_builds/0
]).

%%%=============================================================================
%%% Full Pipeline Example
%%%=============================================================================

example_full_pipeline() ->
    io:format("=== Full CI/CD Pipeline with Andon ===~n~n"),

    SkuId = <<"BUILD-2024-001">>,

    % 1. Compilation Stage
    io:format("Stage 1: Compilation~n"),
    case simulate_compilation() of
        ok ->
            io:format("  ✓ Compilation successful~n"),
            ok;
        {error, CompileErrors} ->
            io:format("  ✗ Compilation failed~n"),
            lists:foreach(fun(Error) ->
                {ok, AndonId} = tcps_andon:hook_compilation_failure(
                    Error#{sku_id => SkuId}
                ),
                io:format("  → Andon triggered: ~s~n", [AndonId])
            end, CompileErrors),
            return_blocked
    end,

    % 2. Check if we can proceed to testing
    io:format("~nStage 2: Testing~n"),
    case tcps_andon:can_proceed_to_stage(SkuId, testing) of
        {blocked, AndonIds} ->
            io:format("  ⚠ BLOCKED by Andons: ~p~n", [AndonIds]),
            io:format("  Cannot proceed to testing until resolved.~n"),
            {error, blocked};
        {ok, proceed} ->
            io:format("  ✓ Clear to proceed to testing~n"),

            % 3. Run tests
            case simulate_tests() of
                ok ->
                    io:format("  ✓ All tests passed~n"),
                    ok;
                {error, TestFailures} ->
                    io:format("  ✗ Tests failed~n"),
                    lists:foreach(fun(Failure) ->
                        {ok, AndonId} = tcps_andon:hook_test_failure(
                            Failure#{sku_id => SkuId}
                        ),
                        io:format("  → Andon triggered: ~s~n", [AndonId])
                    end, TestFailures),
                    return_blocked
            end
    end,

    % 4. SHACL Validation
    io:format("~nStage 3: SHACL Validation~n"),
    case tcps_andon:can_proceed_to_stage(SkuId, validation) of
        {blocked, AndonIds} ->
            io:format("  ⚠ BLOCKED by Andons: ~p~n", [AndonIds]),
            {error, blocked};
        {ok, proceed} ->
            io:format("  ✓ Clear to proceed to validation~n"),

            case simulate_shacl_validation() of
                ok ->
                    io:format("  ✓ SHACL validation passed~n"),
                    {ok, ready_for_deployment};
                {error, Violations} ->
                    io:format("  ✗ SHACL violations detected~n"),
                    {ok, AndonId} = tcps_andon:hook_shacl_failure(
                        #{validation_report => Violations, sku_id => SkuId}
                    ),
                    io:format("  → Andon triggered: ~s~n", [AndonId]),
                    {error, blocked}
            end
    end.

%%%=============================================================================
%%% Test Failure Example
%%%=============================================================================

example_test_failure() ->
    io:format("=== Test Failure Andon Example ===~n~n"),

    SkuId = <<"BUILD-2024-002">>,

    % Simulate a failing test
    io:format("Running test: test_calculate_total/0~n"),

    Expected = 100,
    Actual = 99,

    io:format("  Expected: ~p~n", [Expected]),
    io:format("  Actual: ~p~n", [Actual]),
    io:format("  Result: FAILED~n~n"),

    % Trigger Andon
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, #{
        sku_id => SkuId,
        stage => testing,
        details => #{
            test_module => example_tests,
            test_case => test_calculate_total,
            failure_reason => <<"Assertion failed: expected 100, got 99">>,
            expected => Expected,
            actual => Actual
        },
        metadata => #{
            ci_build_id => <<"jenkins-build-456">>,
            commit_sha => <<"abc123def456">>,
            branch => <<"feature/new-calculation">>
        }
    }),

    io:format("Andon Event Created: ~s~n", [AndonId]),
    io:format("Status: SKU ~s is now BLOCKED~n~n", [SkuId]),

    % Show blocking effect
    case tcps_andon:is_blocked(SkuId) of
        true ->
            io:format("✗ SKU is blocked from proceeding~n"),
            io:format("  Cannot deploy until Andon is resolved~n~n");
        false ->
            io:format("✓ SKU is clear to proceed~n")
    end,

    {ok, AndonId}.

%%%=============================================================================
%%% Resolution Example
%%%=============================================================================

example_resolution() ->
    io:format("=== Andon Resolution Example ===~n~n"),

    % First, create an Andon
    SkuId = <<"BUILD-2024-003">>,
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, #{
        sku_id => SkuId,
        stage => testing,
        details => #{
            test_module => example_tests,
            test_case => test_concurrent_access,
            failure_reason => <<"Race condition detected">>
        }
    }),

    io:format("Andon Created: ~s~n", [AndonId]),
    io:format("Status: BLOCKED~n~n"),

    % Simulate investigation and fix
    io:format("Engineer investigating...~n"),
    timer:sleep(1000),
    io:format("Root cause identified: Missing synchronization in cache~n"),
    io:format("Fix applied: Added gen_server serialization~n"),
    io:format("Prevention: Added property-based tests for concurrency~n~n"),

    % Resolve the Andon
    Resolution = #{
        root_cause =>
            <<"Missing synchronization when updating shared cache. "
              "Multiple processes could read-modify-write simultaneously, "
              "causing lost updates.">>,
        fix_applied =>
            <<"Wrapped cache operations in gen_server to serialize access. "
              "All cache updates now go through handle_call to ensure atomicity.">>,
        prevention_added =>
            <<"Added PropEr property-based tests with 1000+ concurrent "
              "processes. Added ETS write_concurrency analysis. "
              "Updated code review checklist to flag shared state.">>,
        resolver => <<"alice@example.com">>,
        resolution_time_minutes => 45
    },

    case tcps_andon:resolve_andon(AndonId, Resolution) of
        ok ->
            io:format("✓ Andon Resolved Successfully~n"),
            io:format("Status: SKU ~s is now UNBLOCKED~n~n", [SkuId]),

            % Verify unblocked
            case tcps_andon:is_blocked(SkuId) of
                false ->
                    io:format("✓ Confirmed: SKU can now proceed~n"),
                    ok;
                true ->
                    io:format("✗ Error: SKU still blocked~n"),
                    error
            end;
        {error, Reason} ->
            io:format("✗ Resolution failed: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%=============================================================================
%%% Concurrent Builds Example
%%%=============================================================================

example_concurrent_builds() ->
    io:format("=== Concurrent Builds with Independent Andons ===~n~n"),

    % Simulate 5 concurrent builds
    BuildIds = [
        <<"BUILD-A">>,
        <<"BUILD-B">>,
        <<"BUILD-C">>,
        <<"BUILD-D">>,
        <<"BUILD-E">>
    ],

    io:format("Starting 5 concurrent builds...~n~n"),

    % Each build runs independently
    Results = lists:map(fun(BuildId) ->
        io:format("Build ~s:~n", [BuildId]),

        % Random failure simulation
        case rand:uniform(3) of
            1 ->
                io:format("  ✓ All stages passed~n"),
                {BuildId, passed};
            2 ->
                {ok, AndonId} = tcps_andon:trigger_andon(test_failure, #{
                    sku_id => BuildId,
                    stage => testing,
                    details => #{failure => <<"random test failure">>}
                }),
                io:format("  ✗ Test failed → Andon ~s~n", [AndonId]),
                {BuildId, blocked, AndonId};
            3 ->
                {ok, AndonId} = tcps_andon:trigger_andon(shacl_violation, #{
                    sku_id => BuildId,
                    stage => validation,
                    details => #{violation => <<"missing property">>}
                }),
                io:format("  ✗ SHACL failed → Andon ~s~n", [AndonId]),
                {BuildId, blocked, AndonId}
        end
    end, BuildIds),

    io:format("~n=== Build Summary ===~n"),
    Passed = length([R || {_, passed} <- Results]),
    Blocked = length([R || {_, blocked, _} <- Results]),
    io:format("Passed: ~p~n", [Passed]),
    io:format("Blocked: ~p~n~n", [Blocked]),

    % Show that each build is independent
    lists:foreach(fun({BuildId, Status}) ->
        case Status of
            passed ->
                case tcps_andon:is_blocked(BuildId) of
                    false ->
                        io:format("✓ ~s: Ready to deploy~n", [BuildId]);
                    true ->
                        io:format("✗ ~s: ERROR - should be unblocked~n", [BuildId])
                end;
            _ ->
                case tcps_andon:is_blocked(BuildId) of
                    true ->
                        io:format("⚠ ~s: Blocked (correct)~n", [BuildId]);
                    false ->
                        io:format("✗ ~s: ERROR - should be blocked~n", [BuildId])
                end
        end
    end, Results).

%%%=============================================================================
%%% Simulation Helpers
%%%=============================================================================

simulate_compilation() ->
    case rand:uniform(10) of
        N when N > 8 ->
            {error, [
                #{
                    error_type => syntax_error,
                    file => <<"src/example.erl">>,
                    line => 42,
                    message => <<"syntax error before: '->'">>,
                    sku_id => <<"current">>
                }
            ]};
        _ ->
            ok
    end.

simulate_tests() ->
    case rand:uniform(10) of
        N when N > 7 ->
            {error, [
                #{
                    test_module => example_tests,
                    test_function => test_feature,
                    failure_type => assertion_failed,
                    expected => 42,
                    actual => 43
                }
            ]};
        _ ->
            ok
    end.

simulate_shacl_validation() ->
    case rand:uniform(10) of
        N when N > 8 ->
            {error, #{
                conforms => false,
                results => [
                    #{
                        result_severity => <<"sh:Violation">>,
                        focus_node => <<"ex:Component1">>,
                        result_path => <<"ex:hasManufacturer">>,
                        result_message => <<"Required property missing">>
                    }
                ]
            }};
        _ ->
            ok
    end.
