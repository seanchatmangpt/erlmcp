-module(erlmcp_otp_builder_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

builder_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Ctx) ->
         [
             {"Start and stop builder", fun() -> test_start_stop(Ctx) end},
             {"Get status of idle builder", fun() -> test_get_status_idle(Ctx) end},
             {"Build with missing source directory", fun() -> test_build_missing_source(Ctx) end},
             {"Build in progress prevents second build", fun() -> test_build_in_progress(Ctx) end},
             {"Cancel build during configure", fun() -> test_cancel_during_build(Ctx) end},
             {"Successful mock build", fun() -> test_successful_mock_build(Ctx) end},
             {"Build with invalid configure", fun() -> test_build_configure_fail(Ctx) end},
             {"Receipt contains all outputs", fun() -> test_receipt_structure(Ctx) end}
         ]
     end}.

%%====================================================================
%% Setup/Cleanup
%%====================================================================

setup() ->
    %% Start builder
    {ok, Builder} = erlmcp_otp_builder:start_link(),

    %% Create temporary test directories
    TmpDir = create_temp_dir(),
    SourceDir = filename:join(TmpDir, "otp_source"),
    PrefixDir = filename:join(TmpDir, "otp_install"),
    filelib:ensure_dir(filename:join(SourceDir, "dummy")),
    filelib:ensure_dir(filename:join(PrefixDir, "dummy")),

    #{
        builder => Builder,
        tmp_dir => TmpDir,
        source_dir => SourceDir,
        prefix_dir => PrefixDir
    }.

cleanup(#{builder := Builder, tmp_dir := TmpDir}) ->
    %% Stop builder
    catch erlang:exit(Builder, kill),

    %% Clean up temp directories
    os:cmd("rm -rf " ++ TmpDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_start_stop(#{builder := Builder}) ->
    %% Builder should be alive
    ?assert(erlang:is_process_alive(Builder)),

    %% Should be able to stop it
    erlang:exit(Builder, normal),
    timer:sleep(100),
    ?assertNot(erlang:is_process_alive(Builder)).

test_get_status_idle(#{builder := Builder}) ->
    %% Get status of idle builder
    {ok, Status} = erlmcp_otp_builder:get_status(Builder),

    ?assertEqual(idle, maps:get(phase, Status)),
    ?assertEqual(undefined, maps:get(source_dir, Status)),
    ?assertEqual(undefined, maps:get(prefix, Status)),
    ?assertEqual(0, maps:get(elapsed_ms, Status)),
    ?assertEqual(false, maps:get(cancelled, Status)).

test_build_missing_source(#{builder := Builder, prefix_dir := PrefixDir}) ->
    %% Try to build with non-existent source directory
    MissingDir = "/tmp/nonexistent_otp_source_12345",
    Result = erlmcp_otp_builder:build(Builder, #{
        source_dir => MissingDir,
        prefix => PrefixDir
    }),

    ?assertMatch({error, {source_dir_not_found, MissingDir}}, Result).

test_build_in_progress(#{builder := Builder, source_dir := SourceDir, prefix_dir := PrefixDir}) ->
    %% Create a mock configure script that sleeps
    create_mock_configure_script(SourceDir, "sleep 2"),

    %% Start first build (don't wait)
    BuilderPid = Builder,
    spawn(fun() ->
        erlmcp_otp_builder:build(BuilderPid, #{
            source_dir => SourceDir,
            prefix => PrefixDir
        })
    end),

    %% Wait a bit for first build to start
    timer:sleep(100),

    %% Try to start second build - should fail
    Result = erlmcp_otp_builder:build(Builder, #{
        source_dir => SourceDir,
        prefix => PrefixDir
    }),

    ?assertEqual({error, build_in_progress}, Result),

    %% Cancel the first build
    erlmcp_otp_builder:cancel(Builder),
    timer:sleep(100).

test_cancel_during_build(#{builder := Builder, source_dir := SourceDir, prefix_dir := PrefixDir}) ->
    %% Create a mock configure script that sleeps
    create_mock_configure_script(SourceDir, "sleep 5"),

    %% Start build in background
    BuilderPid = Builder,
    Ref = make_ref(),
    Caller = self(),
    spawn(fun() ->
        Result = erlmcp_otp_builder:build(BuilderPid, #{
            source_dir => SourceDir,
            prefix => PrefixDir
        }),
        Caller ! {Ref, Result}
    end),

    %% Wait a bit for build to start
    timer:sleep(200),

    %% Cancel the build
    ok = erlmcp_otp_builder:cancel(Builder),

    %% Should receive error result
    receive
        {Ref, Result} ->
            ?assertMatch({error, cancelled}, Result)
    after 3000 ->
        ?assert(false, "Build did not complete after cancel")
    end.

test_successful_mock_build(#{builder := Builder, source_dir := SourceDir, prefix_dir := PrefixDir}) ->
    %% Create mock build scripts
    create_mock_configure_script(SourceDir, "echo 'checking for gcc...' && echo 'configure: creating Makefile'"),
    create_mock_makefile(SourceDir),

    %% Run build
    Result = erlmcp_otp_builder:build(Builder, #{
        source_dir => SourceDir,
        prefix => PrefixDir
    }),

    %% Should succeed
    case Result of
        {ok, Prefix, Receipt} ->
            ?assertEqual(PrefixDir, Prefix),
            ?assert(is_map(Receipt)),
            ?assert(maps:is_key(timestamp, Receipt)),
            ?assert(maps:is_key(duration, Receipt)),
            ?assert(maps:is_key(configure_output, Receipt)),
            ?assert(maps:is_key(build_output, Receipt)),
            ?assert(maps:is_key(install_output, Receipt)),
            ?assert(maps:is_key(build_log, Receipt)),

            %% Duration should be reasonable
            Duration = maps:get(duration, Receipt),
            ?assert(Duration > 0),
            ?assert(Duration < 10000), % Less than 10 seconds

            %% Configure output should contain our mock messages
            ConfigureOutput = maps:get(configure_output, Receipt),
            ?assert(is_binary(ConfigureOutput)),
            ?assertNotEqual(<<>>, ConfigureOutput);

        {error, Reason} ->
            ?debugFmt("Build failed: ~p", [Reason]),
            ?assert(false, "Expected successful build")
    end.

test_build_configure_fail(#{builder := Builder, source_dir := SourceDir, prefix_dir := PrefixDir}) ->
    %% Create a configure script that fails
    create_mock_configure_script(SourceDir, "echo 'Error: gcc not found' && exit 1"),

    %% Run build
    Result = erlmcp_otp_builder:build(Builder, #{
        source_dir => SourceDir,
        prefix => PrefixDir
    }),

    %% Should fail during configure
    ?assertMatch({error, {configure_failed, 1, _Output}}, Result),

    case Result of
        {error, {configure_failed, _Code, Output}} ->
            ?assert(is_binary(Output)),
            ?assertNotEqual(<<>>, Output);
        _ ->
            ok
    end.

test_receipt_structure(#{builder := Builder, source_dir := SourceDir, prefix_dir := PrefixDir}) ->
    %% Create successful mock build
    create_mock_configure_script(SourceDir, "echo 'configure complete'"),
    create_mock_makefile(SourceDir),

    %% Run build
    {ok, _Prefix, Receipt} = erlmcp_otp_builder:build(Builder, #{
        source_dir => SourceDir,
        prefix => PrefixDir
    }),

    %% Verify receipt structure
    ?assert(is_integer(maps:get(timestamp, Receipt))),
    ?assert(is_integer(maps:get(duration, Receipt))),
    ?assert(is_binary(maps:get(configure_output, Receipt))),
    ?assert(is_binary(maps:get(build_output, Receipt))),
    ?assert(is_binary(maps:get(install_output, Receipt))),
    ?assert(is_binary(maps:get(build_log, Receipt))),

    %% Build log should contain all phase outputs
    BuildLog = maps:get(build_log, Receipt),
    ?assert(binary:match(BuildLog, <<"[CONFIGURE]">>) =/= nomatch),
    ?assert(binary:match(BuildLog, <<"[MAKE]">>) =/= nomatch),
    ?assert(binary:match(BuildLog, <<"[INSTALL]">>) =/= nomatch).

%%====================================================================
%% Test Helpers
%%====================================================================

create_temp_dir() ->
    Rand = integer_to_list(erlang:unique_integer([positive])),
    TmpDir = filename:join("/tmp", "erlmcp_otp_builder_test_" ++ Rand),
    filelib:ensure_dir(filename:join(TmpDir, "dummy")),
    TmpDir.

create_mock_configure_script(SourceDir, Commands) ->
    Script = filename:join(SourceDir, "configure"),
    Content = "#!/bin/sh\n" ++ Commands ++ "\n",
    ok = file:write_file(Script, Content),
    ok = file:change_mode(Script, 8#755).

create_mock_makefile(SourceDir) ->
    Makefile = filename:join(SourceDir, "Makefile"),
    Content = ".PHONY: all install\n\n"
              "all:\n"
              "\t@echo 'CC beam/erl_init.c'\n"
              "\t@echo 'LD beam/beam.smp'\n"
              "\t@echo 'Build complete'\n\n"
              "install:\n"
              "\t@echo 'Installing to $(PREFIX)'\n"
              "\t@mkdir -p $(PREFIX)/bin\n"
              "\t@echo 'Installation complete'\n",
    ok = file:write_file(Makefile, Content).

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_() ->
    {timeout, 30,
     fun() ->
         {ok, Builder} = erlmcp_otp_builder:start_link(),

         %% Create mock build environment
         TmpDir = create_temp_dir(),
         SourceDir = filename:join(TmpDir, "otp_source"),
         PrefixDir = filename:join(TmpDir, "otp_install"),
         filelib:ensure_dir(filename:join(SourceDir, "dummy")),

         create_mock_configure_script(SourceDir, "echo 'fast configure'"),
         create_mock_makefile(SourceDir),

         %% Time the build
         StartTime = erlang:monotonic_time(millisecond),
         {ok, _Prefix, Receipt} = erlmcp_otp_builder:build(Builder, #{
             source_dir => SourceDir,
             prefix => PrefixDir
         }),
         EndTime = erlang:monotonic_time(millisecond),

         Elapsed = EndTime - StartTime,
         ReceiptDuration = maps:get(duration, Receipt),

         %% Performance assertions
         ?assert(Elapsed < 5000, "Build took too long"),
         ?assert(ReceiptDuration > 0, "Duration must be positive"),
         ?assert(abs(Elapsed - ReceiptDuration) < 1000, "Receipt duration should match elapsed time"),

         %% Cleanup
         erlang:exit(Builder, kill),
         os:cmd("rm -rf " ++ TmpDir),
         ok
     end}.

%%====================================================================
%% Concurrent Builds Test
%%====================================================================

concurrent_builds_test_() ->
    {timeout, 30,
     fun() ->
         %% Start multiple builders
         {ok, Builder1} = erlmcp_otp_builder:start_link(),
         {ok, Builder2} = erlmcp_otp_builder:start_link(),
         {ok, Builder3} = erlmcp_otp_builder:start_link(),

         %% Create separate build environments
         Builders = [
             {Builder1, setup_build_env("build1")},
             {Builder2, setup_build_env("build2")},
             {Builder3, setup_build_env("build3")}
         ],

         %% Run builds concurrently
         Ref = make_ref(),
         Caller = self(),

         lists:foreach(fun({Builder, {SourceDir, PrefixDir}}) ->
             spawn(fun() ->
                 Result = erlmcp_otp_builder:build(Builder, #{
                     source_dir => SourceDir,
                     prefix => PrefixDir
                 }),
                 Caller ! {Ref, Builder, Result}
             end)
         end, Builders),

         %% Wait for all builds to complete
         Results = collect_results(Ref, 3, []),

         %% All should succeed
         ?assertEqual(3, length(Results)),
         lists:foreach(fun({_Builder, Result}) ->
             ?assertMatch({ok, _Prefix, _Receipt}, Result)
         end, Results),

         %% Cleanup
         lists:foreach(fun({Builder, {SourceDir, _}}) ->
             erlang:exit(Builder, kill),
             TmpDir = filename:dirname(SourceDir),
             os:cmd("rm -rf " ++ TmpDir)
         end, Builders),
         ok
     end}.

setup_build_env(Name) ->
    TmpDir = create_temp_dir(),
    SourceDir = filename:join(TmpDir, Name ++ "_source"),
    PrefixDir = filename:join(TmpDir, Name ++ "_install"),
    filelib:ensure_dir(filename:join(SourceDir, "dummy")),

    create_mock_configure_script(SourceDir, "echo 'configure " ++ Name ++ "'"),
    create_mock_makefile(SourceDir),

    {SourceDir, PrefixDir}.

collect_results(_Ref, 0, Acc) ->
    Acc;
collect_results(Ref, Count, Acc) ->
    receive
        {Ref, Builder, Result} ->
            collect_results(Ref, Count - 1, [{Builder, Result} | Acc])
    after 10000 ->
        ?assert(false, "Timeout waiting for build results"),
        Acc
    end.
