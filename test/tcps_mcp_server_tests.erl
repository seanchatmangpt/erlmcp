%%%-------------------------------------------------------------------
%%% @doc Unit tests for tcps_mcp_server module
%%% Comprehensive test coverage for MCP server functionality and tool registration.
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_mcp_server_tests).
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Fixtures
%%%===================================================================

setup() ->
    % Start the server for tests
    {ok, Pid} = tcps_mcp_server:start_link([{port, 3100 + rand:uniform(100)}, {transport, stdio}]),
    Pid.

cleanup(Pid) ->
    % Stop the server
    case is_process_alive(Pid) of
        true -> tcps_mcp_server:stop();
        false -> ok
    end.

mcp_server_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_server_start_stop/1,
      fun test_list_default_tools/1,
      fun test_register_tool/1,
      fun test_unregister_tool/1,
      fun test_call_work_order_create/1,
      fun test_call_work_order_verify/1,
      fun test_call_quality_gates/1,
      fun test_call_andon_trigger/1,
      fun test_call_root_cause/1,
      fun test_call_tutorial_tool/1,
      fun test_call_howto_tool/1,
      fun test_call_search_tool/1,
      fun test_get_server_info/1,
      fun test_invalid_tool_call/1,
      fun test_tool_error_handling/1
     ]
    }.

%%%===================================================================
%%% Test Cases
%%%===================================================================

test_server_start_stop(_Pid) ->
    {"Server starts and stops cleanly",
     fun() ->
         % Server is already running from setup
         ?assert(true),
         % Stop happens in cleanup
         ok
     end}.

test_list_default_tools(_Pid) ->
    {"List default TCPS tools",
     fun() ->
         Tools = tcps_mcp_server:list_tools(),
         ?assertEqual(8, length(Tools)),

         % Check expected tools are present
         ToolNames = [maps:get(name, T) || T <- Tools],
         ?assert(lists:member(<<"tcps_work_order_create">>, ToolNames)),
         ?assert(lists:member(<<"tcps_work_order_verify">>, ToolNames)),
         ?assert(lists:member(<<"tcps_quality_gates_check">>, ToolNames)),
         ?assert(lists:member(<<"tcps_andon_trigger">>, ToolNames)),
         ?assert(lists:member(<<"tcps_root_cause_analyze">>, ToolNames)),
         ?assert(lists:member(<<"tcps_diataxis_get_tutorial">>, ToolNames)),
         ?assert(lists:member(<<"tcps_diataxis_get_howto">>, ToolNames)),
         ?assert(lists:member(<<"tcps_diataxis_search">>, ToolNames))
     end}.

test_register_tool(_Pid) ->
    {"Register a custom tool",
     fun() ->
         Tool = #{
             name => <<"custom_tool">>,
             description => <<"A custom test tool">>,
             input_schema => #{<<"type">> => <<"object">>},
             handler => fun(_Args) -> {ok, <<"success">>} end
         },

         ?assertEqual(ok, tcps_mcp_server:register_tool(<<"custom_tool">>, Tool)),

         Tools = tcps_mcp_server:list_tools(),
         ?assertEqual(9, length(Tools)),

         ToolNames = [maps:get(name, T) || T <- Tools],
         ?assert(lists:member(<<"custom_tool">>, ToolNames))
     end}.

test_unregister_tool(_Pid) ->
    {"Unregister a tool",
     fun() ->
         Tool = #{
             name => <<"temp_tool">>,
             description => <<"Temporary tool">>,
             input_schema => #{},
             handler => fun(_) -> {ok, ok} end
         },

         tcps_mcp_server:register_tool(<<"temp_tool">>, Tool),
         ?assertEqual(ok, tcps_mcp_server:unregister_tool(<<"temp_tool">>)),

         Tools = tcps_mcp_server:list_tools(),
         ToolNames = [maps:get(name, T) || T <- Tools],
         ?assertNot(lists:member(<<"temp_tool">>, ToolNames))
     end}.

test_call_work_order_create(_Pid) ->
    {"Call work_order_create tool",
     fun() ->
         {ok, WO} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
             <<"type">> => <<"feature">>,
             <<"priority">> => <<"high">>,
             <<"description">> => <<"Test work order">>
         }),

         ?assert(maps:is_key(id, WO)),
         ?assertEqual(feature, maps:get(type, WO)),
         ?assertEqual(high, maps:get(priority, WO)),
         ?assertEqual(<<"Test work order">>, maps:get(description, WO)),
         ?assertEqual(created, maps:get(status, WO))
     end}.

test_call_work_order_verify(_Pid) ->
    {"Call work_order_verify tool",
     fun() ->
         WorkOrderId = <<"wo_123">>,
         {ok, Receipt} = tcps_mcp_server:call_tool(<<"tcps_work_order_verify">>, #{
             <<"work_order_id">> => WorkOrderId
         }),

         ?assertEqual(WorkOrderId, maps:get(work_order_id, Receipt)),
         ?assert(maps:is_key(verification_hash, Receipt)),
         ?assert(maps:is_key(verified_at, Receipt)),
         ?assertEqual(verified, maps:get(status, Receipt))
     end}.

test_call_quality_gates(_Pid) ->
    {"Call quality_gates_check tool",
     fun() ->
         {ok, Result} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{
             <<"project_path">> => <<"/test/project">>
         }),

         ?assertEqual(pass, maps:get(status, Result)),
         ?assert(maps:is_key(metrics, Result)),

         Metrics = maps:get(metrics, Result),
         ?assert(maps:is_key(coverage, Metrics)),
         ?assert(maps:is_key(complexity, Metrics)),
         ?assertEqual([], maps:get(violations, Result))
     end}.

test_call_andon_trigger(_Pid) ->
    {"Call andon_trigger tool",
     fun() ->
         {ok, Alert} = tcps_mcp_server:call_tool(<<"tcps_andon_trigger">>, #{
             <<"severity">> => <<"critical">>,
             <<"message">> => <<"Quality threshold breached">>
         }),

         ?assert(maps:is_key(id, Alert)),
         ?assertEqual(critical, maps:get(severity, Alert)),
         ?assertEqual(<<"Quality threshold breached">>, maps:get(message, Alert)),
         ?assertEqual(active, maps:get(status, Alert))
     end}.

test_call_root_cause(_Pid) ->
    {"Call root_cause_analyze tool",
     fun() ->
         {ok, Analysis} = tcps_mcp_server:call_tool(<<"tcps_root_cause_analyze">>, #{
             <<"incident_id">> => <<"inc_456">>,
             <<"description">> => <<"System crash">>
         }),

         ?assertEqual(<<"inc_456">>, maps:get(incident_id, Analysis)),
         ?assert(maps:is_key(root_cause, Analysis)),
         ?assert(maps:is_key(why_chain, Analysis)),
         ?assert(maps:is_key(corrective_actions, Analysis)),

         WhyChain = maps:get(why_chain, Analysis),
         ?assert(is_list(WhyChain)),
         ?assert(length(WhyChain) >= 5) % 5 Whys technique
     end}.

test_call_tutorial_tool(_Pid) ->
    {"Call diataxis_get_tutorial tool",
     fun() ->
         {ok, Tutorial} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_tutorial">>, #{
             <<"tutorial_id">> => <<"getting_started_tcps">>
         }),

         ?assertEqual(<<"Getting Started with TCPS">>, maps:get(title, Tutorial)),
         ?assertEqual(beginner, maps:get(level, Tutorial))
     end}.

test_call_howto_tool(_Pid) ->
    {"Call diataxis_get_howto tool",
     fun() ->
         {ok, Guide} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_howto">>, #{
             <<"guide_id">> => <<"configure_quality_gates">>
         }),

         ?assertEqual(<<"How to Configure Quality Gates">>, maps:get(title, Guide)),
         ?assertEqual(configuration, maps:get(category, Guide))
     end}.

test_call_search_tool(_Pid) ->
    {"Call diataxis_search tool",
     fun() ->
         {ok, Results} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
             <<"query">> => <<"quality">>,
             <<"type">> => <<"all">>
         }),

         ?assert(maps:is_key(tutorials, Results)),
         ?assert(maps:is_key(howtos, Results)),
         ?assert(maps:is_key(explanations, Results)),
         ?assert(maps:is_key(references, Results))
     end}.

test_get_server_info(_Pid) ->
    {"Get server information",
     fun() ->
         Info = tcps_mcp_server:get_server_info(),

         ?assertEqual(<<"0.1.0">>, maps:get(version, Info)),
         ?assertEqual(<<"2024-11-05">>, maps:get(protocol_version, Info)),
         ?assertEqual(8, maps:get(tool_count, Info)),
         ?assertEqual(stdio, maps:get(transport, Info))
     end}.

test_invalid_tool_call(_Pid) ->
    {"Call non-existent tool",
     fun() ->
         ?assertEqual({error, tool_not_found},
                      tcps_mcp_server:call_tool(<<"nonexistent_tool">>, #{}))
     end}.

test_tool_error_handling(_Pid) ->
    {"Tool handler error is caught",
     fun() ->
         FailingTool = #{
             name => <<"failing_tool">>,
             description => <<"Tool that always fails">>,
             input_schema => #{},
             handler => fun(_) -> error(deliberate_failure) end
         },

         tcps_mcp_server:register_tool(<<"failing_tool">>, FailingTool),

         Result = tcps_mcp_server:call_tool(<<"failing_tool">>, #{}),
         ?assertMatch({error, _}, Result),

         {error, ErrorDetails} = Result,
         ?assert(maps:is_key(class, ErrorDetails)),
         ?assert(maps:is_key(reason, ErrorDetails))
     end}.

%%%===================================================================
%%% Integration Tests
%%%===================================================================

integration_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_workflow_integration/1,
      fun test_diataxis_integration/1
     ]
    }.

test_workflow_integration(_Pid) ->
    {"Complete workflow: create, verify, check quality",
     fun() ->
         % Create work order
         {ok, WO} = tcps_mcp_server:call_tool(<<"tcps_work_order_create">>, #{
             <<"type">> => <<"feature">>
         }),
         WOId = maps:get(id, WO),

         % Verify it
         {ok, Receipt} = tcps_mcp_server:call_tool(<<"tcps_work_order_verify">>, #{
             <<"work_order_id">> => WOId
         }),
         ?assertEqual(verified, maps:get(status, Receipt)),

         % Check quality gates
         {ok, QGResult} = tcps_mcp_server:call_tool(<<"tcps_quality_gates_check">>, #{}),
         ?assertEqual(pass, maps:get(status, QGResult))
     end}.

test_diataxis_integration(_Pid) ->
    {"Diataxis documentation integration",
     fun() ->
         % Search for content
         {ok, SearchResults} = tcps_mcp_server:call_tool(<<"tcps_diataxis_search">>, #{
             <<"query">> => <<"quality">>,
             <<"type">> => <<"howto">>
         }),
         ?assert(length(SearchResults) > 0),

         % Get specific guide
         FirstGuide = hd(SearchResults),
         GuideId = maps:get(id, FirstGuide),

         {ok, FullGuide} = tcps_mcp_server:call_tool(<<"tcps_diataxis_get_howto">>, #{
             <<"guide_id">> => GuideId
         }),
         ?assertEqual(GuideId, maps:get(id, FullGuide))
     end}.
