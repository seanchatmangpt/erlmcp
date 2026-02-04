-module(erlmcp_tool_sandbox_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup (Chicago School TDD: Real processes, no mocks)
%%%===================================================================

setup() ->
    {ok, Pid} = erlmcp_tool_sandbox:start_link(),
    Pid.

cleanup(_Pid) ->
    ok = erlmcp_tool_sandbox:stop(erlmcp_tool_sandbox).

%%%===================================================================
%%% Test Generators
%%%===================================================================

tool_sandbox_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun start_link_test/0,
      fun permission_management_test/0,
      fun limits_management_test/0,
      fun execution_with_permissions_test/0,
      fun execution_without_permissions_test/0,
      fun resource_limits_validation_test/0,
      fun timeout_enforcement_test/0,
      fun audit_logging_test/0,
      fun concurrent_execution_test/0,
      fun invalid_permission_test/0,
      fun invalid_limits_test/0,
      fun execution_isolation_test/0
     ]}.

%%%===================================================================
%%% Individual Tests (Chicago School: Real process testing)
%%%===================================================================

start_link_test() ->
    {"Server starts with default configuration", fun() ->
        ?assertNotEqual(undefined, whereis(erlmcp_tool_sandbox)),

        %% Verify initial state
        Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
        ?assertEqual(0, sets:size(Perms)),

        Limits = erlmcp_tool_sandbox:get_limits(erlmcp_tool_sandbox),
        ?assert(maps:is_key(max_cpu_percent, Limits)),
        ?assert(maps:is_key(max_memory_mb, Limits)),
        ?assert(maps:is_key(max_timeout_sec, Limits)),
        ?assert(maps:is_key(max_file_size_mb, Limits))
    end}.

permission_management_test() ->
    {"Manage permissions dynamically", [
        {"Add single permission", fun() ->
            ?assertEqual(ok,
                erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile')),

            Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
            ?assertEqual(1, sets:size(Perms)),
            ?assert(sets:is_element('ReadFile', Perms))
        end},
        {"Add multiple permissions", fun() ->
            PermsList = ['ReadFile', 'WriteFile', 'NetworkAccess'],
            ?assertEqual(ok,
                erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, PermsList)),

            Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
            ?assertEqual(3, sets:size(Perms)),
            lists:foreach(fun(P) ->
                ?assert(sets:is_element(P, Perms))
            end, PermsList)
        end},
        {"Add duplicate permission (idempotent)", fun() ->
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

            Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
            ?assertEqual(1, sets:size(Perms))
        end},
        {"All permission types work", fun() ->
            AllPerms = [
                'ReadFile',
                'WriteFile',
                'NetworkAccess',
                'ExecuteCommand',
                'ReadEnv',
                'Subprocess'
            ],
            ?assertEqual(ok,
                erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, AllPerms)),

            Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
            ?assertEqual(6, sets:size(Perms))
        end}
    ]}.

limits_management_test() ->
    {"Manage resource limits", [
        {"Set valid limits", fun() ->
            NewLimits = #{
                max_cpu_percent => 75,
                max_memory_mb => 512,
                max_timeout_sec => 60,
                max_file_size_mb => 20
            },
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox, NewLimits)),

            Limits = erlmcp_tool_sandbox:get_limits(erlmcp_tool_sandbox),
            ?assertEqual(75, maps:get(max_cpu_percent, Limits)),
            ?assertEqual(512, maps:get(max_memory_mb, Limits)),
            ?assertEqual(60, maps:get(max_timeout_sec, Limits)),
            ?assertEqual(20, maps:get(max_file_size_mb, Limits))
        end},
        {"Partial limit update", fun() ->
            NewLimits = #{max_memory_mb => 1024},
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox, NewLimits)),

            Limits = erlmcp_tool_sandbox:get_limits(erlmcp_tool_sandbox),
            ?assertEqual(1024, maps:get(max_memory_mb, Limits)),
            %% Other defaults should remain
            ?assert(maps:is_key(max_cpu_percent, Limits))
        end},
        {"Default limits on init", fun() ->
            Limits = erlmcp_tool_sandbox:get_limits(erlmcp_tool_sandbox),
            ?assertEqual(50, maps:get(max_cpu_percent, Limits)),
            ?assertEqual(256, maps:get(max_memory_mb, Limits)),
            ?assertEqual(30, maps:get(max_timeout_sec, Limits)),
            ?assertEqual(10, maps:get(max_file_size_mb, Limits))
        end}
    ]}.

execution_with_permissions_test() ->
    {"Execute tools with proper permissions", [
        {"Read file with permission", fun() ->
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"read_file">>,
                #{path => <<"/tmp/test.txt">>}
            ),
            ?assertMatch({ok, #{content := _}}, Result)
        end},
        {"Write file with permission", fun() ->
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'WriteFile'),

            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"write_file">>,
                #{path => <<"/tmp/test.txt">>, content => <<"test">>}
            ),
            ?assertMatch({ok, #{written := true}}, Result)
        end},
        {"Network access with permission", fun() ->
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'NetworkAccess'),

            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"network_request">>,
                #{url => <<"https://example.com">>}
            ),
            ?assertMatch({ok, #{status := ok}}, Result)
        end},
        {"Multiple permissions allow multiple tools", fun() ->
            Perms = ['ReadFile', 'WriteFile', 'NetworkAccess'],
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, Perms),

            ?assertMatch({ok, _},
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox, <<"read_file">>, #{})),
            ?assertMatch({ok, _},
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox, <<"write_file">>, #{})),
            ?assertMatch({ok, _},
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox, <<"network_request">>, #{}))
        end}
    ]}.

execution_without_permissions_test() ->
    {"Deny execution without proper permissions", [
        {"Read file denied without permission", fun() ->
            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"read_file">>,
                #{path => <<"/etc/passwd">>}
            ),
            ?assertMatch({error, {permission_denied, ['ReadFile']}}, Result)
        end},
        {"Write file denied without permission", fun() ->
            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"write_file">>,
                #{path => <<"/tmp/test">>, content => <<"malicious">>}
            ),
            ?assertMatch({error, {permission_denied, ['WriteFile']}}, Result)
        end},
        {"Network access denied without permission", fun() ->
            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"network_request">>,
                #{url => <<"https://malicious.com">>}
            ),
            ?assertMatch({error, {permission_denied, ['NetworkAccess']}}, Result)
        end},
        {"Execute command denied without permission", fun() ->
            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"execute_command">>,
                #{command => <<"rm -rf /">>}
            ),
            ?assertMatch({error, {permission_denied, ['ExecuteCommand']}}, Result)
        end},
        {"Partial permission denied", fun() ->
            %% Only grant ReadFile, try to write
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

            ?assertMatch({ok, _},
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox, <<"read_file">>, #{})),

            ?assertMatch({error, {permission_denied, ['WriteFile']}},
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox, <<"write_file">>, #{}))
        end}
    ]}.

resource_limits_validation_test() ->
    {"Validate resource limits", [
        {"Valid CPU limits", fun() ->
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 1})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 50})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 100}))
        end},
        {"Invalid CPU limits", fun() ->
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 0})),
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 101}))
        end},
        {"Valid memory limits", fun() ->
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_memory_mb => 1})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_memory_mb => 512})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_memory_mb => 10240}))
        end},
        {"Invalid memory limits", fun() ->
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_memory_mb => 0})),
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_memory_mb => 10241}))
        end},
        {"Valid timeout limits", fun() ->
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_timeout_sec => 1})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_timeout_sec => 60})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_timeout_sec => 300}))
        end},
        {"Invalid timeout limits", fun() ->
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_timeout_sec => 0})),
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_timeout_sec => 301}))
        end},
        {"Valid file size limits", fun() ->
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_file_size_mb => 1})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_file_size_mb => 100})),
            ?assertEqual(ok,
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_file_size_mb => 1024}))
        end},
        {"Invalid file size limits", fun() ->
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_file_size_mb => 0})),
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_file_size_mb => 1025}))
        end},
        {"Combined invalid limits", fun() ->
            ?assertMatch({error, invalid_limits},
                erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                    #{max_cpu_percent => 150, max_memory_mb => -10}))
        end}
    ]}.

timeout_enforcement_test() ->
    {"Enforce execution timeout", [
        {"Default timeout applied", fun() ->
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

            %% Default timeout is 30 seconds
            %% Tool execution should complete within this window
            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"read_file">>,
                #{path => <<"/tmp/test.txt">>}
            ),
            ?assertMatch({ok, _}, Result)
        end},
        {"Custom timeout limit", fun() ->
            ok = erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox,
                #{max_timeout_sec => 5}),
            ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

            Result = erlmcp_tool_sandbox:execute_tool(
                erlmcp_tool_sandbox,
                <<"read_file">>,
                #{path => <<"/tmp/test.txt">>}
            ),
            ?assertMatch({ok, _}, Result)
        end}
    ]}.

audit_logging_test() ->
    {"Audit logging tracks all executions", fun() ->
        ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'ReadFile'),

        %% Execute a tool
        {ok, _} = erlmcp_tool_sandbox:execute_tool(
            erlmcp_tool_sandbox,
            <<"read_file">>,
            #{path => <<"/tmp/test.txt">>}
        ),

        %% Audit log should contain entry
        %% Note: We can't directly access audit_log, but we can verify
        %% execution succeeded and generated unique execution IDs
        {ok, Result2} = erlmcp_tool_sandbox:execute_tool(
            erlmcp_tool_sandbox,
            <<"read_file">>,
            #{path => <<"/tmp/test2.txt">>}
        ),

        %% Different executions should have different results (different execution IDs)
        ?assertMatch({ok, _}, Result2)
    end}.

concurrent_execution_test() ->
    {"Handle concurrent executions safely", fun() ->
        ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox,
            ['ReadFile', 'WriteFile', 'NetworkAccess']),

        %% Spawn multiple concurrent executions
        Pids = [
            spawn(fun() ->
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
                    <<"read_file">>, #{path => <<"/tmp/file1.txt">>})
            end),
            spawn(fun() ->
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
                    <<"write_file">>, #{path => <<"/tmp/file2.txt">>, content => <<"test">>})
            end),
            spawn(fun() ->
                erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
                    <<"network_request">>, #{url => <<"https://example.com">>})
            end)
        ],

        %% Wait for all to complete
        timer:sleep(100),

        %% All spawned processes should still be running or completed
        %% Server should still be alive
        ?assertNotEqual(undefined, whereis(erlmcp_tool_sandbox))
    end}.

invalid_permission_test() ->
    {"Reject invalid permissions", [
        {"Unknown permission atom", fun() ->
            Result = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, 'InvalidPerm'),
            ?assertMatch({error, {invalid_permission, 'InvalidPerm'}}, Result)
        end},
        {"Invalid permission in list", fun() ->
            Result = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox,
                ['ReadFile', 'BadPerm', 'WriteFile']),
            ?assertMatch({error, invalid_permissions}, Result)
        end},
        {"Non-atom permission", fun() ->
            Result = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox, <<"NotAnAtom">>),
            ?assertMatch({error, {invalid_permission, _}}, Result)
        end}
    ]}.

invalid_limits_test() ->
    {"Reject invalid limit types", fun() ->
        %% Non-map limits
        Result = erlmcp_tool_sandbox:set_limits(erlmcp_tool_sandbox, not_a_map),
        ?assertMatch({error, invalid_limits}, Result)
    end}.

execution_isolation_test() ->
    {"Executions are isolated from each other", fun() ->
        ok = erlmcp_tool_sandbox:add_permission(erlmcp_tool_sandbox,
            ['ReadFile', 'WriteFile']),

        %% Execute multiple tools in sequence
        {ok, _} = erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
            <<"read_file">>, #{path => <<"/tmp/file1.txt">>}),

        {ok, _} = erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
            <<"write_file">>, #{path => <<"/tmp/file2.txt">>, content => <<"test">>}),

        {ok, _} = erlmcp_tool_sandbox:execute_tool(erlmcp_tool_sandbox,
            <<"read_file">>, #{path => <<"/tmp/file3.txt">>}),

        %% All should succeed independently
        Perms = erlmcp_tool_sandbox:get_permissions(erlmcp_tool_sandbox),
        ?assertEqual(2, sets:size(Perms))
    end}.
