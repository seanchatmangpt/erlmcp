%%%-----------------------------------------------------------------------------
%%% @doc TCPS Deterministic Build Demo
%%%
%%% Demonstrates usage of the TCPS deterministic build verification system
%%% with real-world scenarios including:
%%% - Verifying build determinism
%%% - Detecting non-determinism sources
%%% - Generating and executing build recipes
%%% - Creating reproducible Docker images
%%% - Managing build caches
%%% - Generating SBOMs for compliance
%%% - Running quality gates
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_deterministic_demo).

-export([
    demo_verify_build/0,
    demo_detect_non_determinism/0,
    demo_build_recipe/0,
    demo_docker_build/0,
    demo_build_cache/0,
    demo_sbom_generation/0,
    demo_quality_gates/0,
    demo_full_workflow/0,
    run_all_demos/0
]).

%%%=============================================================================
%%% Demo 1: Verify Build Determinism
%%%=============================================================================

demo_verify_build() ->
    io:format("~n=== DEMO 1: Verify Build Determinism ===~n~n"),

    SkuId = <<"SKU-DEMO-001">>,

    io:format("Verifying deterministic build for SKU: ~s~n", [SkuId]),
    io:format("This will build the project twice and compare artifacts...~n~n"),

    case tcps_deterministic:verify_deterministic_build(SkuId) of
        {ok, deterministic, Hash} ->
            io:format("~nRESULT: Build is deterministic!~n"),
            io:format("  Artifact Hash: ~s~n", [hash_to_hex(Hash)]),
            io:format("  Status: PASS~n"),
            {ok, deterministic};

        {error, {non_deterministic, Diff}} ->
            io:format("~nRESULT: Build is NON-deterministic!~n"),
            io:format("  First Hash:  ~s~n", [hash_to_hex(maps:get(first_hash, Diff))]),
            io:format("  Second Hash: ~s~n", [hash_to_hex(maps:get(second_hash, Diff))]),
            io:format("  Status: FAIL~n"),
            io:format("  Details: ~s~n", [maps:get(details, Diff)]),
            io:format("~nAn Andon event has been triggered.~n"),
            {error, non_deterministic}
    end.

%%%=============================================================================
%%% Demo 2: Detect Non-Determinism Sources
%%%=============================================================================

demo_detect_non_determinism() ->
    io:format("~n=== DEMO 2: Detect Non-Determinism Sources ===~n~n"),

    io:format("Scanning codebase for potential non-determinism...~n"),

    Sources = tcps_deterministic:detect_non_determinism_sources("src"),

    io:format("~nFound ~p potential non-determinism sources:~n~n", [length(Sources)]),

    lists:foreach(fun(Source) ->
        Type = maps:get(type, Source),
        File = maps:get(file, Source),
        Line = maps:get(line, Source),
        Severity = maps:get(severity, Source),
        Fix = maps:get(suggested_fix, Source),

        io:format("  [~p] ~s:~p~n", [Severity, File, Line]),
        io:format("    Type: ~p~n", [Type]),
        io:format("    Fix:  ~s~n~n", [Fix])
    end, Sources),

    case Sources of
        [] ->
            io:format("No non-determinism sources detected!~n"),
            {ok, no_sources};
        [_|_] ->
            io:format("Generating automated fixes...~n"),
            Fixes = tcps_deterministic:fix_non_determinism(Sources),

            io:format("~nGenerated ~p fixes:~n~n", [length(Fixes)]),
            lists:foreach(fun(Fix) ->
                FixType = maps:get(fix_type, Fix),
                SuggestedCode = maps:get(suggested_code, Fix),

                io:format("  Fix Type: ~p~n", [FixType]),
                io:format("  Suggested Code:~n~s~n~n", [SuggestedCode])
            end, Fixes),

            {ok, Sources}
    end.

%%%=============================================================================
%%% Demo 3: Build Recipe Generation and Execution
%%%=============================================================================

demo_build_recipe() ->
    io:format("~n=== DEMO 3: Build Recipe Generation ===~n~n"),

    SkuId = <<"SKU-DEMO-003">>,

    io:format("Generating reproducible build recipe for SKU: ~s~n", [SkuId]),

    Recipe = tcps_deterministic:generate_build_recipe(SkuId),

    io:format("~nBuild Recipe:~n"),
    io:format("  SKU ID: ~s~n", [maps:get(sku_id, Recipe)]),
    io:format("  Version: ~s~n", [maps:get(version, Recipe)]),
    io:format("  Generated At: ~p~n", [maps:get(generated_at, Recipe)]),

    BuildEnv = maps:get(build_env, Recipe),
    io:format("~nBuild Environment:~n"),
    io:format("  OTP Version: ~s~n", [maps:get(otp_version, BuildEnv)]),
    io:format("  Rebar3 Version: ~s~n", [maps:get(rebar3_version, BuildEnv)]),
    io:format("  Architecture: ~s~n", [maps:get(architecture, BuildEnv)]),

    Steps = maps:get(steps, Recipe),
    io:format("~nBuild Steps (~p total):~n", [length(Steps)]),
    lists:foreach(fun(Step) ->
        Name = maps:get(name, Step),
        Command = maps:get(command, Step),
        io:format("  - ~s: ~s~n", [Name, Command])
    end, Steps),

    % Save recipe to file
    RecipePath = "/tmp/build_recipe.json",
    RecipeJson = jsx:encode(Recipe),
    ok = file:write_file(RecipePath, RecipeJson),

    io:format("~nRecipe saved to: ~s~n", [RecipePath]),
    io:format("~nTo execute this recipe on another machine:~n"),
    io:format("  1. Copy the recipe file~n"),
    io:format("  2. Ensure matching build environment~n"),
    io:format("  3. Run: tcps_deterministic:execute_build_recipe(\"~s\")~n", [RecipePath]),

    {ok, Recipe}.

%%%=============================================================================
%%% Demo 4: Docker Build Integration
%%%=============================================================================

demo_docker_build() ->
    io:format("~n=== DEMO 4: Docker Build Integration ===~n~n"),

    SkuId = <<"SKU-DOCKER-DEMO">>,

    io:format("Generating Dockerfile for SKU: ~s~n", [SkuId]),

    Dockerfile = tcps_deterministic:generate_dockerfile(SkuId),

    io:format("~nGenerated Dockerfile:~n"),
    io:format("~s~n", [Dockerfile]),

    DockerfilePath = "/tmp/Dockerfile.tcps",
    ok = file:write_file(DockerfilePath, Dockerfile),

    io:format("Dockerfile saved to: ~s~n", [DockerfilePath]),
    io:format("~nTo build Docker image:~n"),
    io:format("  docker build -f ~s -t tcps-~s .~n", [DockerfilePath, SkuId]),

    io:format("~nTo verify Docker build determinism:~n"),
    io:format("  tcps_deterministic:verify_docker_determinism(<<\"~s\">>)~n", [SkuId]),

    {ok, Dockerfile}.

%%%=============================================================================
%%% Demo 5: Build Cache Management
%%%=============================================================================

demo_build_cache() ->
    io:format("~n=== DEMO 5: Build Cache Management ===~n~n"),

    SkuId = <<"SKU-CACHE-DEMO">>,

    io:format("Caching build artifacts for SKU: ~s~n", [SkuId]),

    ok = tcps_deterministic:cache_build_artifacts(SkuId),

    io:format("  Artifacts cached successfully~n"),

    % Get cache stats
    Stats = tcps_deterministic:get_cache_stats(),

    io:format("~nCache Statistics:~n"),
    io:format("  Total Entries: ~p~n", [maps:get(entries, Stats)]),
    io:format("  Total Size: ~.2f MB~n", [maps:get(total_size_mb, Stats)]),

    % Calculate hash for lookup
    {ok, Hash} = tcps_deterministic:calculate_artifact_hash("rebar.config"),

    io:format("~nAttempting cache restore...~n"),
    case tcps_deterministic:restore_from_cache(Hash) of
        {ok, restored} ->
            io:format("  Cache hit! Artifacts restored.~n"),
            {ok, cache_hit};
        miss ->
            io:format("  Cache miss. Need to rebuild.~n"),
            {ok, cache_miss}
    end.

%%%=============================================================================
%%% Demo 6: SBOM Generation
%%%=============================================================================

demo_sbom_generation() ->
    io:format("~n=== DEMO 6: SBOM Generation ===~n~n"),

    SkuId = <<"SKU-SBOM-DEMO">>,

    io:format("Generating Software Bill of Materials (SBOM) for SKU: ~s~n", [SkuId]),

    SBOM = tcps_deterministic:generate_sbom(SkuId),

    io:format("~nSBOM Details:~n"),
    io:format("  Format: ~p~n", [maps:get(format, SBOM)]),
    io:format("  Version: ~s~n", [maps:get(version, SBOM)]),

    Components = maps:get(components, SBOM),
    io:format("  Components: ~p~n", [length(Components)]),

    io:format("~nComponent List:~n"),
    lists:foreach(fun(Component) ->
        Name = maps:get(name, Component),
        Version = maps:get(version, Component),
        Licenses = maps:get(licenses, Component),
        Type = maps:get(type, Component),

        io:format("  - ~s (~s)~n", [Name, Version]),
        io:format("    Type: ~p~n", [Type]),
        io:format("    Licenses: ~p~n", [Licenses])
    end, Components),

    % Verify licenses
    io:format("~nVerifying license compliance...~n"),
    case tcps_deterministic:verify_licenses() of
        {ok, compliant} ->
            io:format("  All licenses are compliant!~n");
        {error, {violations, Violations}} ->
            io:format("  License violations found:~n"),
            lists:foreach(fun(Violation) ->
                io:format("    - ~s~n", [maps:get(name, Violation)])
            end, Violations)
    end,

    % Check vulnerabilities
    io:format("~nChecking for vulnerabilities...~n"),
    case tcps_deterministic:check_vulnerabilities(SkuId) of
        {ok, no_vulnerabilities} ->
            io:format("  No vulnerabilities found!~n");
        {error, {vulnerabilities_found, Vulns}} ->
            io:format("  Vulnerabilities found: ~p~n", [length(Vulns)])
    end,

    {ok, SBOM}.

%%%=============================================================================
%%% Demo 7: Quality Gates
%%%=============================================================================

demo_quality_gates() ->
    io:format("~n=== DEMO 7: Quality Gates ===~n~n"),

    SkuId = <<"SKU-GATES-DEMO">>,

    io:format("Running quality gates for SKU: ~s~n", [SkuId]),
    io:format("~nQuality Gates:~n"),
    io:format("  1. Build Determinism~n"),
    io:format("  2. Dependencies Pinned~n"),
    io:format("  3. No Non-Determinism Sources~n"),
    io:format("  4. SBOM Generated~n"),
    io:format("  5. Licenses Compliant~n"),

    io:format("~nExecuting gates...~n"),

    case tcps_deterministic:check_determinism_gate(SkuId) of
        {ok, pass} ->
            io:format("~nRESULT: All quality gates PASSED!~n"),
            io:format("  Status: READY FOR RELEASE~n"),
            {ok, pass};
        {error, {fail, Reason}} ->
            io:format("~nRESULT: Quality gates FAILED!~n"),
            io:format("  Failed Gate: ~p~n", [Reason]),
            io:format("  Status: BLOCKED~n"),
            io:format("~nAn Andon event has been triggered.~n"),
            {error, {gate_failed, Reason}}
    end.

%%%=============================================================================
%%% Demo 8: Full Workflow
%%%=============================================================================

demo_full_workflow() ->
    io:format("~n=== DEMO 8: Full Deterministic Build Workflow ===~n~n"),

    SkuId = <<"SKU-WORKFLOW-DEMO">>,

    io:format("Running complete deterministic build workflow for SKU: ~s~n~n", [SkuId]),

    % Step 1: Detect non-determinism
    io:format("[Step 1/6] Detecting non-determinism sources...~n"),
    Sources = tcps_deterministic:detect_non_determinism_sources("src"),
    case Sources of
        [] ->
            io:format("  No issues found~n");
        [_|_] ->
            io:format("  WARNING: Found ~p sources, generating fixes~n", [length(Sources)]),
            _Fixes = tcps_deterministic:fix_non_determinism(Sources)
    end,

    % Step 2: Pin dependencies
    io:format("~n[Step 2/6] Verifying dependency pinning...~n"),
    case tcps_deterministic:verify_pinned_deps() of
        {ok, pinned} ->
            io:format("  All dependencies are pinned~n");
        {error, {unpinned, UnpinnedDeps}} ->
            io:format("  WARNING: Unpinned dependencies: ~p~n", [UnpinnedDeps])
    end,

    % Step 3: Verify build determinism
    io:format("~n[Step 3/6] Verifying build determinism...~n"),
    _BuildResult = tcps_deterministic:verify_deterministic_build(SkuId),

    % Step 4: Generate build recipe
    io:format("~n[Step 4/6] Generating build recipe...~n"),
    Recipe = tcps_deterministic:generate_build_recipe(SkuId),
    io:format("  Recipe generated with ~p steps~n", [length(maps:get(steps, Recipe))]),

    % Step 5: Generate SBOM
    io:format("~n[Step 5/6] Generating SBOM...~n"),
    SBOM = tcps_deterministic:generate_sbom(SkuId),
    io:format("  SBOM generated with ~p components~n",
              [length(maps:get(components, SBOM))]),

    % Step 6: Run quality gates
    io:format("~n[Step 6/6] Running quality gates...~n"),
    GateResult = tcps_deterministic:run_quality_gates(SkuId),

    io:format("~n=== WORKFLOW COMPLETE ===~n"),
    case GateResult of
        {ok, all_passed} ->
            io:format("Status: SUCCESS - Ready for deployment~n");
        {error, {failed_gates, Failed}} ->
            io:format("Status: BLOCKED - Failed gates: ~p~n", [Failed])
    end,

    {ok, GateResult}.

%%%=============================================================================
%%% Run All Demos
%%%=============================================================================

run_all_demos() ->
    io:format("~n~n"),
    io:format("╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║  TCPS Deterministic Build Verification System - Demo Suite    ║~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n"),

    % Initialize system
    tcps_deterministic:start(),

    Demos = [
        {1, "Verify Build Determinism", fun demo_verify_build/0},
        {2, "Detect Non-Determinism", fun demo_detect_non_determinism/0},
        {3, "Build Recipe Generation", fun demo_build_recipe/0},
        {4, "Docker Build Integration", fun demo_docker_build/0},
        {5, "Build Cache Management", fun demo_build_cache/0},
        {6, "SBOM Generation", fun demo_sbom_generation/0},
        {7, "Quality Gates", fun demo_quality_gates/0},
        {8, "Full Workflow", fun demo_full_workflow/0}
    ],

    Results = lists:map(fun({Num, Name, DemoFun}) ->
        io:format("~n~nRunning Demo ~p: ~s~n", [Num, Name]),
        io:format("═══════════════════════════════════════════════════════~n"),

        try DemoFun() of
            Result ->
                io:format("~nDemo ~p completed successfully~n", [Num]),
                {Num, ok, Result}
        catch
            Type:Error:Stacktrace ->
                io:format("~nDemo ~p failed: ~p:~p~n", [Num, Type, Error]),
                io:format("Stacktrace: ~p~n", [Stacktrace]),
                {Num, error, {Type, Error}}
        end
    end, Demos),

    % Print summary
    io:format("~n~n"),
    io:format("╔════════════════════════════════════════════════════════════════╗~n"),
    io:format("║  Demo Summary                                                  ║~n"),
    io:format("╚════════════════════════════════════════════════════════════════╝~n~n"),

    Passed = length([Num || {Num, ok, _} <- Results]),
    Failed = length([Num || {Num, error, _} <- Results]),

    io:format("Demos Passed: ~p/~p~n", [Passed, length(Demos)]),
    io:format("Demos Failed: ~p/~p~n", [Failed, length(Demos)]),

    case Failed of
        0 ->
            io:format("~nAll demos completed successfully!~n"),
            {ok, all_passed};
        _ ->
            io:format("~nSome demos failed. Check output above for details.~n"),
            {error, {failed_demos, Failed}}
    end.

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

hash_to_hex(Hash) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= Hash]).
