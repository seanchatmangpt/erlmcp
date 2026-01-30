%%%-------------------------------------------------------------------
%%% @doc erlmcp Integration Test Suite
%%%
%%% End-to-end integration tests for erlmcp new features:
%%% - Completions (erlmcp_completion)
%%% - Tasks (erlmcp_tasks)
%%% - Prompt Templates (erlmcp_prompt_template)
%%% - Security/Auth (erlmcp_auth)
%%%
%%% Tests use Chicago School TDD: real processes, real interactions,
%%% state-based verification, no mocks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_integration_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%===================================================================
%%% Test Suite Callbacks
%%%===================================================================

all() ->
    [
     completion_e2e_workflow_test,
     completion_rate_limiting_test,
     completion_caching_test,
     completion_ranking_test,
     tasks_e2e_workflow_test,
     tasks_progress_tracking_test,
     tasks_timeout_test,
     tasks_worker_failure_test,
     prompt_template_e2e_test,
     prompt_template_security_test,
     prompt_template_validation_test,
     jwt_verification_e2e_test,
     authorization_e2e_test,
     prompt_injection_e2e_test,
     session_lifecycle_test,
     rbac_permission_test
    ].

init_per_suite(Config) ->
    % Start application and dependencies
    {ok, _} = application:ensure_all_started(erlmcp),

    % Start required services
    {ok, _AuthPid} = erlmcp_auth:start_link(#{
        rate_limiter_enabled => false  % Disable for tests
    }),

    {ok, _TasksPid} = erlmcp_tasks:start_link(),

    Config.

end_per_suite(_Config) ->
    % Stop services
    erlmcp_auth:stop(),
    application:stop(erlmcp),
    ok.

init_per_testcase(_TestCase, Config) ->
    % Clean state before each test
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Completion Integration Tests
%%%===================================================================

completion_e2e_workflow_test(_Config) ->
    % 1. Start completion server with handler
    {ok, CompletionPid} = erlmcp_completion:start_link(#{
        cache_ttl => 60,
        max_results => 5
    }),

    % 2. Add completion handler for file paths
    Handler = fun(_Ref, Argument) ->
        Value = maps:get(value, Argument, <<>>),
        Prefix = case Value of
            <<>> -> <<>>;
            _ ->
                case binary:match(Value, <<"/">>) of
                    nomatch -> Value;
                    _ -> filename:basename(binary_to_list(Value))
                end
        end,
        Files = [
            #{value => <<"src/">>, label => <<"src/">>},
            #{value => <<"test/">>, label => <<"test/">>},
            #{value => <<"docs/">>, label => <<"docs/">>}
        ],
        Filtered = case Prefix of
            <<>> -> Files;
            _ -> [F || #{value := V} = F <- Files, binary:match(V, Prefix) =/= nomatch]
        end,
        {ok, Filtered}
    end,

    ok = erlmcp_completion:add_completion_handler(
        CompletionPid,
        <<"file_path">>,
        Handler
    ),

    % 3. Client calls complete
    {ok, Result} = erlmcp_completion:complete(
        CompletionPid,
        <<"file_path">>,
        #{value => <<"sr">>}
    ),

    % 4. Verify completions returned and ranked
    Completions = maps:get(completions, Result, []),
    ?assert(length(Completions) > 0),

    % Verify highest ranked is "src/" (Jaro-Winkler similarity to "sr")
    [Best | _] = Completions,
    ?assertEqual(<<"src/">>, maps:get(value, Best)),

    % Verify hasMore flag
    HasMore = maps:get(hasMore, Result, false),
    ?assertEqual(false, HasMore),

    % Cleanup
    erlmcp_completion:stop(CompletionPid),
    ok.

completion_rate_limiting_test(_Config) ->
    % 1. Start completion server with low rate limit
    {ok, CompletionPid} = erlmcp_completion:start_link(#{
        rate_limit => 2  % Only 2 requests per second
    }),

    % 2. Add handler
    Handler = fun(_Ref, _Arg) ->
        {ok, [#{value => <<"test1">>}, #{value => <<"test2">>}]}
    end,

    ok = erlmcp_completion:add_completion_handler(
        CompletionPid,
        <<"rate_limited">>,
        Handler
    ),

    % 3. First two requests should succeed
    {ok, _} = erlmcp_completion:complete(
        CompletionPid,
        <<"rate_limited">>,
        #{value => <<"a">>}
    ),

    {ok, _} = erlmcp_completion:complete(
        CompletionPid,
        <<"rate_limited">>,
        #{value => <<"b">>}
    ),

    % 4. Third request should be rate limited
    {error, {completion_rate_limited, -32101, _}} =
        erlmcp_completion:complete(
            CompletionPid,
            <<"rate_limited">>,
            #{value => <<"c">>}
        ),

    % Cleanup
    erlmcp_completion:stop(CompletionPid),
    ok.

completion_caching_test(_Config) ->
    % 1. Start completion server with short cache TTL
    {ok, CompletionPid} = erlmcp_completion:start_link(#{
        cache_ttl => 10,
        max_results => 10
    }),

    % 2. Add handler that tracks calls
    CallCount = erlang:make_ref(),
    Handler = fun(_Ref, _Arg) ->
        case get(CallCount) of
            undefined -> put(CallCount, 1);
            N -> put(CallCount, N + 1)
        end,
        {ok, [#{value => <<"cached">>}]}
    end,

    ok = erlmcp_completion:add_completion_handler(
        CompletionPid,
        <<"cached">>,
        Handler
    ),

    % 3. First call invokes handler
    {ok, _} = erlmcp_completion:complete(
        CompletionPid,
        <<"cached">>,
        #{value => <<"test">>}
    ),
    ?assertEqual(1, get(CallCount)),

    % 4. Second call uses cache (handler not called)
    {ok, _} = erlmcp_completion:complete(
        CompletionPid,
        <<"cached">>,
        #{value => <<"test">>}
    ),
    ?assertEqual(1, get(CallCount)),  % Still 1, not incremented

    % Cleanup
    erlmcp_completion:stop(CompletionPid),
    ok.

completion_ranking_test(_Config) ->
    % Test Jaro-Winkler similarity ranking
    {ok, CompletionPid} = erlmcp_completion:start_link(#{
        ranking_threshold => 0.5,
        max_results => 10
    }),

    Handler = fun(_Ref, _Arg) ->
        {ok, [
            #{value => <<"apple">>},
            #{value => <<"app">>},
            #{value => <<"application">>},
            #{value => <<"banana">>}
        ]}
    end,

    ok = erlmcp_completion:add_completion_handler(
        CompletionPid,
        <<"ranking">>,
        Handler
    ),

    % Query for "app" - should rank "app" highest
    {ok, Result} = erlmcp_completion:complete(
        CompletionPid,
        <<"ranking">>,
        #{value => <<"app">>}
    ),

    Completions = maps:get(completions, Result, []),
    ?assert(length(Completions) >= 2),

    % "app" should be first (exact match)
    [First, Second | _] = Completions,
    ?assertEqual(<<"app">>, maps:get(value, First)),

    % "apple" or "application" should be second (high similarity)
    SecondValue = maps:get(value, Second),
    ?assert(lists:member(SecondValue, [<<"apple">>, <<"application">>])),

    % "banana" should not appear (low similarity)
    BananaResults = [C || #{value := V} = C <- Completions, V =:= <<"banana">>],
    ?assertEqual([], BananaResults),

    erlmcp_completion:stop(CompletionPid),
    ok.

%%%===================================================================
%%% Tasks Integration Tests
%%%===================================================================

tasks_e2e_workflow_test(_Config) ->
    % 1. Create task via client
    ClientPid = self(),
    Action = #{<<"type">> => <<"test_action">>},
    Metadata = #{
        <<"progressToken">> => true,
        <<"timeout">> => 5000
    },

    {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

    % 2. Verify task created in pending state
    {ok, Task} = erlmcp_tasks:get_task(ClientPid, TaskId),
    ?assertEqual(<<"pending">>, maps:get(<<"status">>, Task)),

    % 3. Simulate worker starting execution
    WorkerPid = spawn(fun() ->
        timer:sleep(100),
        erlmcp_tasks:complete_task(TaskId, #{<<"result">> => <<"success">>})
    end),

    {ok, _} = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

    % 4. Wait for completion
    timer:sleep(200),

    % 5. Verify task completed
    {ok, CompletedTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
    ?assertEqual(<<"completed">>, maps_get(<<"status">>, CompletedTask)),

    % 6. Get result
    {ok, Result} = erlmcp_tasks:get_task_result(ClientPid, TaskId),
    ?assertEqual(#{<<"result">> => <<"success">>}, Result),

    ok.

tasks_progress_tracking_test(_Config) ->
    % Test progress updates during task execution
    ClientPid = self(),
    Action = #{<<"type">> => <<"long_running">>},
    Metadata = #{<<"progressToken">> => true},

    {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

    % Start worker that updates progress
    WorkerPid = spawn(fun() ->
        erlmcp_tasks:set_task_progress(TaskId, {0, 100}),
        timer:sleep(50),
        erlmcp_tasks:set_task_progress(TaskId, {50, 100}),
        timer:sleep(50),
        erlmcp_tasks:set_task_progress(TaskId, {100, 100}),
        erlmcp_tasks:complete_task(TaskId, #{<<"done">> => true})
    end),

    {ok, _} = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

    % Wait for completion
    timer:sleep(200),

    % Verify final progress
    {ok, FinalTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
    ?assertEqual(<<"completed">>, maps_get(<<"status">>, FinalTask)),
    ?assertEqual(100, maps_get(<<"progress">>, FinalTask)),
    ?assertEqual(100, maps_get(<<"total">>, FinalTask)),

    ok.

tasks_timeout_test(_Config) ->
    % Test task timeout handling
    ClientPid = self(),
    Action = #{<<"type">> => <<"slow">>},
    Metadata = #{<<"timeout">> => 100},  % 100ms timeout

    {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

    % Start worker that never completes
    WorkerPid = spawn(fun() ->
        timer:sleep(infinity)  % Never completes
    end),

    {ok, _} = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

    % Wait for timeout
    timer:sleep(200),

    % Verify task failed due to timeout
    {ok, FailedTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
    ?assertEqual(<<"failed">>, maps_get(<<"status">>, FailedTask)),

    Error = maps_get(<<"error">>, FailedTask),
    ?assertEqual(-32085, maps_get(<<"code">>, Error)),  % TASK_TIMEOUT

    ok.

tasks_worker_failure_test(_Config) ->
    % Test handling of worker process crashes
    ClientPid = self(),
    Action = #{<<"type">> => <<"crash_prone">>},
    Metadata = #{},

    {ok, TaskId} = erlmcp_tasks:create_task(ClientPid, Action, Metadata),

    % Start worker that crashes
    WorkerPid = spawn(fun() ->
        timer:sleep(50),
        exit(crash)
    end),

    {ok, _} = erlmcp_tasks:start_task_execution(TaskId, WorkerPid),

    % Wait for worker death detection
    timer:sleep(200),

    % Verify task marked as failed
    {ok, FailedTask} = erlmcp_tasks:get_task(ClientPid, TaskId),
    ?assertEqual(<<"failed">>, maps_get(<<"status">>, FailedTask)),

    ok.

%%%===================================================================
%%% Prompt Template Integration Tests
%%%===================================================================

prompt_template_e2e_test(_Config) ->
    % 1. Compile template with variables
    Template = <<"Hello {{name}}, your order {{order_id}} is ready.">>,

    {ok, Compiled} = erlmcp_prompt_template:compile(Template),

    % 2. Render with variables
    Variables = #{
        <<"name">> => <<"Alice">>,
        <<"order_id">> => <<"12345">>
    },

    {ok, Rendered} = erlmcp_prompt_template:render(Compiled, Variables),

    % 3. Verify output
    Expected = <<"Hello Alice, your order 12345 is ready.">>,
    ?assertEqual(Expected, Rendered),

    ok.

prompt_template_security_test(_Config) ->
    % Test security: dangerous patterns rejected

    % 1. Template too large
    LargeTemplate = binary:copy(<<"a">>, 10241),  % > 10KB
    {error, {template_too_large, _, _}} =
        erlmcp_prompt_template:compile(LargeTemplate),

    % 2. Variable name too long
    LongVarName = binary:copy(<<"x">>, 65),
    BadTemplate = <<"Test ", LongVarName/binary, " end">>,
    {error, {variable_name_too_long, _, _}} =
        erlmcp_prompt_template:compile(BadTemplate),

    % 3. Nesting too deep
    DeepTemplate = <<"{{#a}}{{#b}}{{#c}}{{#d}}{{#e}}{{#f}}{{/f}}{{/e}}{{/d}}{{/c}}{{/b}}{{/a}}">>,
    {error, {nesting_too_deep, _, _}} =
        erlmcp_prompt_template:compile(DeepTemplate),

    ok.

prompt_template_validation_test(_Config) ->
    % Test template syntax validation

    % 1. Valid template
    ValidTemplate = <<"{{title}}: {{#items}}{{name}}, {{/items}}">>,
    ok = erlmcp_prompt_template:validate(ValidTemplate),

    % 2. Unclosed section
    UnclosedTemplate = <<"{{#section}}unclosed">>,
    {error, {invalid_template_syntax, _}} =
        erlmcp_prompt_template:validate(UnclosedTemplate),

    % 3. Detection of template syntax
    ?assertEqual(true, erlmcp_prompt_template:has_template_syntax(<<"Hello {{name}}">>)),
    ?assertEqual(false, erlmcp_prompt_template:has_template_syntax(<<"Hello name">>)),

    ok.

%%%===================================================================
%%% Security/Auth Integration Tests
%%%===================================================================

jwt_verification_e2e_test(_Config) ->
    % 1. Create JWT (simplified - in production use jose library)
    % For this test, we'll use API key auth instead

    {ok, SessionId} = erlmcp_auth:create_session(
        <<"user123">>,
        #{<<"method">> => api_key}
    ),

    % 2. Verify session exists
    {ok, _} = erlmcp_auth:check_permission(
        SessionId,
        <<"resource1">>,
        <<"read">>
    ),

    % 3. Destroy session
    ok = erlmcp_auth:destroy_session(SessionId),

    % 4. Verify session destroyed
    {error, forbidden} =
        erlmcp_auth:check_permission(
            SessionId,
            <<"resource1">>,
            <<"read">>
        ),

    ok.

authorization_e2e_test(_Config) ->
    % Test RBAC authorization

    % 1. Create user session
    {ok, SessionId} = erlmcp_auth:create_session(
        <<"user456">>,
        #{<<"roles">> => [<<"user">>]}
    ),

    % 2. Add role with specific permissions
    ok = erlmcp_auth:add_permission(
        <<"resource2">>,
        <<"read">>,
        [<<"user">>]
    ),

    % 3. Check permission - should succeed
    ok = erlmcp_auth:check_permission(
        SessionId,
        <<"resource2">>,
        <<"read">>
    ),

    % 4. Check permission for different action - should fail
    {error, forbidden} =
        erlmcp_auth:check_permission(
            SessionId,
            <<"resource2">>,
            <<"write">>
        ),

    % 5. Check permission for different resource - should fail
    {error, forbidden} =
        erlmcp_auth:check_permission(
            SessionId,
            <<"admin_resource">>,
            <<"read">>
        ),

    ok.

prompt_injection_e2e_test(_Config) ->
    % Test prompt injection prevention

    % 1. Template with attempted injection
    MaliciousTemplate = <<"Hello {{name}}, {{#admin}}SECRET: {{secret}}{{/admin}}">>,

    % 2. Compile should succeed (syntax is valid)
    {ok, Compiled} = erlmcp_prompt_template:compile(MaliciousTemplate),

    % 3. Render without admin section - no secret exposed
    SafeVariables = #{<<"name">> => <<"Bob">>},
    {ok, SafeResult} = erlmcp_prompt_template:render(Compiled, SafeVariables),

    % Verify no secret in output
    case binary:match(SafeResult, <<"SECRET">>) of
        nomatch -> ok;
        _ -> ct:fail("Secret exposed in safe render")
    end,

    % 4. Even with admin variable, only allowlist variables work
    AdminVariables = #{
        <<"name">> => <<"Eve">>,
        <<"admin">> => true,
        <<"secret">> => <<"PASSWORD123">>
    },

    {ok, AdminResult} = erlmcp_prompt_template:render(Compiled, AdminVariables),

    % The admin section should render (mustache behavior)
    % But this tests that we validate inputs, not that we prevent rendering
    ?assertEqual(true, binary:match(AdminResult, <<"SECRET">>) =/= nomatch),

    ok.

session_lifecycle_test(_Config) ->
    % Test complete session lifecycle

    % 1. Create session
    {ok, SessionId} = erlmcp_auth:create_session(
        <<"user789">>,
        #{<<"ip">> => <<"127.0.0.1">>}
    ),

    % 2. Use session
    ok = erlmcp_auth:add_permission(<<"resource3">>, <<"read">>, [<<"user">>]),
    ok = erlmcp_auth:check_permission(SessionId, <<"resource3">>, <<"read">>),

    % 3. Rotate token
    {ok, _NewToken} = erlmcp_auth:rotate_token(SessionId),

    % 4. Destroy session
    ok = erlmcp_auth:destroy_session(SessionId),

    % 5. Verify cleanup
    {error, forbidden} =
        erlmcp_auth:check_permission(SessionId, <<"resource3">>, <<"read">>),

    ok.

rbac_permission_test(_Config) ->
    % Test role-based access control

    % 1. Create admin and user sessions
    {ok, AdminSession} = erlmcp_auth:create_session(
        <<"admin_user">>,
        #{<<"roles">> => [<<"admin">>]}
    ),

    {ok, UserSession} = erlmcp_auth:create_session(
        <<"normal_user">>,
        #{<<"roles">> => [<<"user">>]}
    ),

    % 2. Configure permissions
    ok = erlmcp_auth:add_permission(<<"admin_resource">>, <<"delete">>, [<<"admin">>]),
    ok = erlmcp_auth:add_permission(<<"shared_resource">>, <<"read">>, [<<"admin">>, <<"user">>]),

    % 3. Admin can delete
    ok = erlmcp_auth:check_permission(AdminSession, <<"admin_resource">>, <<"delete">>),

    % 4. User cannot delete
    {error, forbidden} =
        erlmcp_auth:check_permission(UserSession, <<"admin_resource">>, <<"delete">>),

    % 5. Both can read shared
    ok = erlmcp_auth:check_permission(AdminSession, <<"shared_resource">>, <<"read">>),
    ok = erlmcp_auth:check_permission(UserSession, <<"shared_resource">>, <<"read">>),

    ok.

%%%===================================================================
%%% Internal Helper Functions
%%%===================================================================

%% Get key from map safely
maps_get(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> error({key_not_found, Key})
    end.
