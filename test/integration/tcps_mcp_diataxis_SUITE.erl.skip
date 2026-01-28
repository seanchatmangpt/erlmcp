%%%-------------------------------------------------------------------
%%% @doc CommonTest suite for TCPS MCP Diataxis Integration
%%% Tests end-to-end MCP server with Diataxis documentation system.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_mcp_diataxis_SUITE).
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% CT Callbacks
%%%===================================================================

all() ->
    [
     test_mcp_server_lifecycle,
     test_all_tools_registered,
     test_work_order_workflow,
     test_quality_gates_workflow,
     test_andon_workflow,
     test_root_cause_workflow,
     test_diataxis_tutorial_access,
     test_diataxis_howto_access,
     test_diataxis_search,
     test_cross_module_integration,
     test_error_handling,
     test_concurrent_tool_calls
    ].

init_per_suite(Config) ->
    application:ensure_all_started(opentelemetry),
    {ok, _} = tcps_mcp_server:start_link([{port, 3200}, {transport, stdio}]),
    Config.

end_per_suite(_Config) ->
    tcps_mcp_server:stop(),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_mcp_server_lifecycle(_Config) ->
    ct:comment("Testing MCP server lifecycle"),

    % Server should be running
    Info = tcps_mcp_server:get_server_info(),
    ?assertEqual(<<"0.1.0">>, maps:get(version, Info)),
    ?assertEqual(<<"2024-11-05">>, maps:get(protocol_version, Info)),
    ?assertEqual(8, maps:get(tool_count, Info)),

    ct:pal("Server info: ~p", [Info]),
    ok.

test_all_tools_registered(_Config) ->
    ct:comment("Testing all 8 TCPS tools are registered"),

    Tools = tcps_mcp_server:list_tools(),
    ?assertEqual(8, length(Tools)),

    ToolNames = [maps:get(name, T) || T <- Tools],

    % Verify all expected tools
    ExpectedTools = [
        <<"tcps_work_order_create">>,
        <<"tcps_work_order_verify">>,
        <<"tcps_quality_gates_check">>,
        <<"tcps_andon_trigger">>,
        <<"tcps_root_cause_analyze">>,
        <<"tcps_diataxis_get_tutorial">>,
        <<"tcps_diataxis_get_howto">>,
        <<"tcps_diataxis_search">>
    ],

    lists:foreach(
        fun(Expected) ->
            ?assert(lists:member(Expected, ToolNames))
        end,
        ExpectedTools
    ),

    ct:pal("All tools registered: ~p", [ToolNames]),
    ok.

test_work_order_workflow(_Config) ->
    ct:comment("Testing complete work order workflow via MCP"),

    % Create work order
    {ok, WO} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
        <<"type">> => <<"feature">>,
        <<"priority">> => <<"high">>,
        <<"description">> => <<"Integration test work order">>
    }),

    ?assert(maps:is_key(id, WO)),
    ?assertEqual(feature, maps:get(type, WO)),
    ?assertEqual(high, maps:get(priority, WO)),
    ?assertEqual(created, maps:get(status, WO)),

    WOId = maps:get(id, WO),

    % Verify work order
    {ok, Receipt} = tcps_mcp_server:call_tool(<<"tcps_work_order_verify">>, #{
        <<"work_order_id">> => WOId
    }),

    ?assertEqual(WOId, maps:get(work_order_id, Receipt)),
    ?assert(maps:is_key(verification_hash, Receipt)),
    ?assertEqual(verified, maps:get(status, Receipt)),

    ct:pal("Work order workflow: Created ~p, Verified ~p", [WOId, Receipt]),
    ok.

test_quality_gates_workflow(_Config) ->
    ct:comment("Testing quality gates via MCP"),

    {ok, Result} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{
        <<"project_path">> => <<"/test/integration/project">>
    }),

    ?assertEqual(pass, maps:get(status, Result)),
    ?assert(maps:is_key(metrics, Result)),

    Metrics = maps:get(metrics, Result),
    ?assert(maps:is_key(coverage, Metrics)),
    ?assert(maps:is_key(complexity, Metrics)),
    ?assert(maps:is_key(duplication, Metrics)),

    ?assertEqual([], maps:get(violations, Result)),

    ct:pal("Quality gates result: ~p", [Result]),
    ok.

test_andon_workflow(_Config) ->
    ct:comment("Testing andon alert triggering via MCP"),

    {ok, Alert} = tcps_mcp_server:call_tool(<<"tcps_andon_trigger">>, #{
        <<"severity">> => <<"critical">>,
        <<"message">> => <<"Integration test alert">>
    }),

    ?assert(maps:is_key(id, Alert)),
    ?assertEqual(critical, maps:get(severity, Alert)),
    ?assertEqual(<<"Integration test alert">>, maps:get(message, Alert)),
    ?assertEqual(active, maps:get(status, Alert)),

    ct:pal("Andon alert triggered: ~p", [Alert]),
    ok.

test_root_cause_workflow(_Config) ->
    ct:comment("Testing root cause analysis via MCP"),

    {ok, Analysis} = tcps_mcp_server:call_tool(<<"tcps_root_cause_analyze">>, #{
        <<"incident_id">> => <<"inc_integration_test">>,
        <<"description">> => <<"Test incident for integration">>
    }),

    ?assertEqual(<<"inc_integration_test">>, maps:get(incident_id, Analysis)),
    ?assert(maps:is_key(root_cause, Analysis)),
    ?assert(maps:is_key(why_chain, Analysis)),
    ?assert(maps:is_key(corrective_actions, Analysis)),

    WhyChain = maps:get(why_chain, Analysis),
    ?assert(is_list(WhyChain)),
    ?assert(length(WhyChain) =:= 5), % 5 Whys

    ct:pal("Root cause analysis: ~p", [Analysis]),
    ok.

test_diataxis_tutorial_access(_Config) ->
    ct:comment("Testing Diataxis tutorial access via MCP"),

    {ok, Tutorial} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
        <<"tutorial_id">> => <<"getting_started_tcps">>
    }),

    ?assertEqual(<<"Getting Started with TCPS">>, maps:get(title, Tutorial)),
    ?assertEqual(beginner, maps:get(level, Tutorial)),
    ?assert(maps:is_key(steps, Tutorial)),
    ?assert(maps:is_key(learning_outcomes, Tutorial)),

    Steps = maps:get(steps, Tutorial),
    ?assert(length(Steps) > 0),

    ct:pal("Tutorial accessed: ~p", [maps:get(title, Tutorial)]),
    ok.

test_diataxis_howto_access(_Config) ->
    ct:comment("Testing Diataxis how-to guide access via MCP"),

    {ok, Guide} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_howto">>, #{
        <<"guide_id">> => <<"configure_quality_gates">>
    }),

    ?assertEqual(<<"How to Configure Quality Gates">>, maps:get(title, Guide)),
    ?assertEqual(configuration, maps:get(category, Guide)),
    ?assertEqual(easy, maps:get(difficulty, Guide)),
    ?assert(maps:is_key(steps, Guide)),

    ct:pal("How-to guide accessed: ~p", [maps:get(title, Guide)]),
    ok.

test_diataxis_search(_Config) ->
    ct:comment("Testing Diataxis search across all documentation types"),

    % Search all types
    {ok, AllResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
        <<"query">> => <<"quality">>,
        <<"type">> => <<"all">>
    }),

    ?assert(maps:is_key(tutorials, AllResults)),
    ?assert(maps:is_key(howtos, AllResults)),
    ?assert(maps:is_key(explanations, AllResults)),
    ?assert(maps:is_key(references, AllResults)),

    % Search specific type
    {ok, HowtoResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
        <<"query">> => <<"quality">>,
        <<"type">> => <<"howto">>
    }),

    ?assert(is_list(HowtoResults)),
    ?assert(length(HowtoResults) > 0),

    ct:pal("Search results: ~p howtos found", [length(HowtoResults)]),
    ok.

test_cross_module_integration(_Config) ->
    ct:comment("Testing integration across TCPS and Diataxis modules"),

    % Create work order
    {ok, WO} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
        <<"type">> => <<"feature">>
    }),

    % Search for how-to guide
    {ok, Guides} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
        <<"query">> => <<"work order">>,
        <<"type">> => <<"howto">>
    }),

    % Run quality check
    {ok, QGResult} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{}),

    % Get tutorial
    {ok, Tutorial} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
        <<"tutorial_id">> => <<"getting_started_tcps">>
    }),

    % All operations should succeed
    ?assert(maps:is_key(id, WO)),
    ?assert(is_list(Guides)),
    ?assertEqual(pass, maps:get(status, QGResult)),
    ?assert(maps:is_key(title, Tutorial)),

    ct:pal("Cross-module integration successful"),
    ok.

test_error_handling(_Config) ->
    ct:comment("Testing error handling in MCP tools"),

    % Call non-existent tool
    ?assertEqual({error, tool_not_found},
                 tcps_mcp_server:call_tool(<<"nonexistent_tool">>, #{})),

    % Call tutorial tool with invalid ID
    ?assertEqual({error, not_found},
                 tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
                     <<"tutorial_id">> => <<"invalid_id">>
                 })),

    % Call how-to tool with invalid ID
    ?assertEqual({error, not_found},
                 tcps_mcp_server:call_tool(<<"tcps_diataxis_get_howto">>, #{
                     <<"guide_id">> => <<"invalid_id">>
                 })),

    ct:pal("Error handling verified"),
    ok.

test_concurrent_tool_calls(_Config) ->
    ct:comment("Testing concurrent MCP tool calls"),

    % Spawn multiple concurrent tool calls
    Parent = self(),

    spawn_link(fun() ->
        Result = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
            <<"type">> => <<"feature">>
        }),
        Parent ! {result, 1, Result}
    end),

    spawn_link(fun() ->
        Result = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{}),
        Parent ! {result, 2, Result}
    end),

    spawn_link(fun() ->
        Result = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
            <<"query">> => <<"quality">>,
            <<"type">> => <<"all">>
        }),
        Parent ! {result, 3, Result}
    end),

    % Collect results
    Results = [
        receive {result, 1, R1} -> R1 after 5000 -> timeout end,
        receive {result, 2, R2} -> R2 after 5000 -> timeout end,
        receive {result, 3, R3} -> R3 after 5000 -> timeout end
    ],

    % All should succeed
    lists:foreach(
        fun(Result) ->
            ?assertMatch({ok, _}, Result)
        end,
        Results
    ),

    ct:pal("Concurrent tool calls successful"),
    ok.
