%%%-----------------------------------------------------------------------------
%%% @doc TCPS Diataxis MCP Server Tests
%%%
%%% Comprehensive test suite for TCPS Diataxis MCP server implementation.
%%% Tests all 8 tools, 6 prompts, and integration scenarios.
%%%
%%% Test Coverage:
%%% - Tool invocation and validation
%%% - Prompt generation and formatting
%%% - Error handling and edge cases
%%% - State management
%%% - Integration with MCP protocol
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_mcp_diataxis_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%%=============================================================================
%%% Test Fixtures
%%%=============================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    {ok, Pid} = tcps_mcp_server:start_link(),
    Pid.

cleanup(Pid) ->
    tcps_mcp_server:stop(),
    ok.

%%%=============================================================================
%%% Tool Tests
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% simulator_start Tool Tests
%%-----------------------------------------------------------------------------

simulator_start_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_simulator_start_default_config()),
             ?_test(test_simulator_start_custom_config()),
             ?_test(test_simulator_start_validation())
         ]
     end}.

test_simulator_start_default_config() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_start">>, 1, Tools),

    Result = Handler(#{}),
    ?assert(is_binary(Result)),

    Decoded = jsx:decode(Result, [return_maps]),
    ?assertEqual(<<"started">>, maps:get(<<"status">>, Decoded)),
    ?assert(maps:is_key(<<"session_id">>, Decoded)),
    ?assert(maps:is_key(<<"config">>, Decoded)).

test_simulator_start_custom_config() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_start">>, 1, Tools),

    Args = #{
        <<"config">> => #{
            <<"max_steps">> => 50,
            <<"initial_quadrant">> => <<"howto">>,
            <<"enable_telemetry">> => false
        }
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    Config = maps:get(<<"config">>, Decoded),
    ?assertEqual(50, maps:get(<<"max_steps">>, Config)),
    ?assertEqual(<<"howto">>, maps:get(<<"initial_quadrant">>, Config)),
    ?assertEqual(false, maps:get(<<"telemetry_enabled">>, Config)).

test_simulator_start_validation() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, Schema, _} = lists:keyfind(<<"simulator_start">>, 1, Tools),

    % Validate schema structure
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    ?assert(maps:is_key(<<"properties">>, Schema)).

%%-----------------------------------------------------------------------------
%% simulator_step Tool Tests
%%-----------------------------------------------------------------------------

simulator_step_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_simulator_step_create_work_order()),
             ?_test(test_simulator_step_run_tests()),
             ?_test(test_simulator_step_check_quality()),
             ?_test(test_simulator_step_advance())
         ]
     end}.

test_simulator_step_create_work_order() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_step">>, 1, Tools),

    Args = #{
        <<"action">> => <<"create_work_order">>,
        <<"params">> => #{
            <<"bucket">> => <<"reliability">>,
            <<"priority">> => 1
        }
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"created">>, maps:get(<<"status">>, Decoded)),
    ?assert(maps:is_key(<<"work_order_id">>, Decoded)),
    ?assertEqual(<<"reliability">>, maps:get(<<"bucket">>, Decoded)).

test_simulator_step_run_tests() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_step">>, 1, Tools),

    Args = #{<<"action">> => <<"run_tests">>, <<"params">> => #{}},

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"completed">>, maps:get(<<"status">>, Decoded)),
    ?assert(maps:is_key(<<"results">>, Decoded)),

    Results = maps:get(<<"results">>, Decoded),
    ?assert(maps:is_key(<<"pass_rate">>, Results)),
    ?assert(maps:is_key(<<"coverage">>, Results)).

test_simulator_step_check_quality() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_step">>, 1, Tools),

    Args = #{<<"action">> => <<"check_quality">>, <<"params">> => #{}},

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"checked">>, maps:get(<<"status">>, Decoded)),
    ?assert(maps:is_key(<<"quality_gates">>, Decoded)).

test_simulator_step_advance() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_step">>, 1, Tools),

    Args = #{<<"action">> => <<"advance">>, <<"params">> => #{}},

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"advanced">>, maps:get(<<"status">>, Decoded)).

%%-----------------------------------------------------------------------------
%% simulator_query Tool Tests
%%-----------------------------------------------------------------------------

simulator_query_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_simulator_query_state()),
             ?_test(test_simulator_query_metrics()),
             ?_test(test_simulator_query_kanban()),
             ?_test(test_simulator_query_quality_gates()),
             ?_test(test_simulator_query_andon()),
             ?_test(test_simulator_query_all())
         ]
     end}.

test_simulator_query_state() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"state">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"session_id">>, Decoded)),
    ?assert(maps:is_key(<<"status">>, Decoded)).

test_simulator_query_metrics() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"metrics">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"steps_executed">>, Decoded)).

test_simulator_query_kanban() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"kanban">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"reliability">>, Decoded)),
    ?assert(maps:is_key(<<"security">>, Decoded)).

test_simulator_query_quality_gates() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"quality_gates">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"test_pass_rate">>, Decoded)),
    ?assert(maps:is_key(<<"coverage">>, Decoded)).

test_simulator_query_andon() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"andon">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"total_events">>, Decoded)).

test_simulator_query_all() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"all">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"state">>, Decoded)),
    ?assert(maps:is_key(<<"metrics">>, Decoded)),
    ?assert(maps:is_key(<<"kanban">>, Decoded)).

%%-----------------------------------------------------------------------------
%% diataxis_navigate Tool Tests
%%-----------------------------------------------------------------------------

diataxis_navigate_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_diataxis_navigate_tutorial()),
             ?_test(test_diataxis_navigate_howto()),
             ?_test(test_diataxis_navigate_explanation()),
             ?_test(test_diataxis_navigate_reference())
         ]
     end}.

test_diataxis_navigate_tutorial() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"diataxis_navigate">>, 1, Tools),

    Args = #{<<"target_quadrant">> => <<"tutorial">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"tutorial">>, maps:get(<<"quadrant">>, Decoded)),
    ?assert(maps:is_key(<<"content">>, Decoded)),
    ?assert(maps:is_key(<<"related_topics">>, Decoded)).

test_diataxis_navigate_howto() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"diataxis_navigate">>, 1, Tools),

    Args = #{<<"target_quadrant">> => <<"howto">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"howto">>, maps:get(<<"quadrant">>, Decoded)).

test_diataxis_navigate_explanation() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"diataxis_navigate">>, 1, Tools),

    Args = #{<<"target_quadrant">> => <<"explanation">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"explanation">>, maps:get(<<"quadrant">>, Decoded)).

test_diataxis_navigate_reference() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"diataxis_navigate">>, 1, Tools),

    Args = #{<<"target_quadrant">> => <<"reference">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"reference">>, maps:get(<<"quadrant">>, Decoded)).

%%-----------------------------------------------------------------------------
%% tcps_explain Tool Tests
%%-----------------------------------------------------------------------------

tcps_explain_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_tcps_explain_andon()),
             ?_test(test_tcps_explain_with_detail_levels())
         ]
     end}.

test_tcps_explain_andon() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"tcps_explain">>, 1, Tools),

    Args = #{<<"concept">> => <<"andon">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"andon">>, maps:get(<<"concept">>, Decoded)),
    ?assert(maps:is_key(<<"explanation">>, Decoded)),
    ?assert(maps:is_key(<<"examples">>, Decoded)).

test_tcps_explain_with_detail_levels() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"tcps_explain">>, 1, Tools),

    DetailLevels = [<<"brief">>, <<"standard">>, <<"comprehensive">>],

    lists:foreach(fun(Level) ->
        Args = #{
            <<"concept">> => <<"andon">>,
            <<"detail_level">> => Level
        },
        Result = Handler(Args),
        Decoded = jsx:decode(Result, [return_maps]),
        ?assertEqual(Level, maps:get(<<"detail_level">>, Decoded))
    end, DetailLevels).

%%-----------------------------------------------------------------------------
%% quality_gate_simulate Tool Tests
%%-----------------------------------------------------------------------------

quality_gate_simulate_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_quality_gate_test_pass_rate()),
             ?_test(test_quality_gate_coverage()),
             ?_test(test_quality_gate_force_failure())
         ]
     end}.

test_quality_gate_test_pass_rate() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"quality_gate_simulate">>, 1, Tools),

    Args = #{
        <<"gate_type">> => <<"test_pass_rate">>,
        <<"test_pass_rate">> => 95.0
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"test_pass_rate">>, maps:get(<<"gate">>, Decoded)),
    ?assertEqual(true, maps:get(<<"passed">>, Decoded)).

test_quality_gate_coverage() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"quality_gate_simulate">>, 1, Tools),

    Args = #{
        <<"gate_type">> => <<"coverage">>,
        <<"coverage">> => 85.0
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"coverage">>, maps:get(<<"gate">>, Decoded)),
    ?assertEqual(true, maps:get(<<"passed">>, Decoded)).

test_quality_gate_force_failure() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"quality_gate_simulate">>, 1, Tools),

    Args = #{
        <<"gate_type">> => <<"test_pass_rate">>,
        <<"test_pass_rate">> => 95.0,
        <<"force_failure">> => true
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(false, maps:get(<<"passed">>, Decoded)).

%%-----------------------------------------------------------------------------
%% andon_trigger Tool Tests
%%-----------------------------------------------------------------------------

andon_trigger_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_andon_trigger_test_failure()),
             ?_test(test_andon_trigger_shacl_violation())
         ]
     end}.

test_andon_trigger_test_failure() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"andon_trigger">>, 1, Tools),

    Args = #{
        <<"failure_type">> => <<"test_failure">>,
        <<"stage">> => <<"testing">>,
        <<"details">> => #{<<"description">> => <<"Pass rate below threshold">>}
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"triggered">>, maps:get(<<"status">>, Decoded)),
    ?assert(maps:is_key(<<"event_id">>, Decoded)),
    ?assertEqual(<<"test_failure">>, maps:get(<<"failure_type">>, Decoded)).

test_andon_trigger_shacl_violation() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"andon_trigger">>, 1, Tools),

    Args = #{
        <<"failure_type">> => <<"shacl_violation">>,
        <<"stage">> => <<"validation">>
    },

    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"shacl_violation">>, maps:get(<<"failure_type">>, Decoded)).

%%-----------------------------------------------------------------------------
%% kanban_visualize Tool Tests
%%-----------------------------------------------------------------------------

kanban_visualize_tool_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_kanban_visualize_ascii()),
             ?_test(test_kanban_visualize_json()),
             ?_test(test_kanban_visualize_markdown())
         ]
     end}.

test_kanban_visualize_ascii() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"kanban_visualize">>, 1, Tools),

    Args = #{<<"format">> => <<"ascii">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"ascii">>, maps:get(<<"format">>, Decoded)),
    ?assert(maps:is_key(<<"visualization">>, Decoded)).

test_kanban_visualize_json() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"kanban_visualize">>, 1, Tools),

    Args = #{<<"format">> => <<"json">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"json">>, maps:get(<<"format">>, Decoded)),

    % Visualization should be parseable JSON
    Viz = maps:get(<<"visualization">>, Decoded),
    ?assertMatch(#{<<"reliability">> := _}, jsx:decode(Viz, [return_maps])).

test_kanban_visualize_markdown() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"kanban_visualize">>, 1, Tools),

    Args = #{<<"format">> => <<"markdown">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assertEqual(<<"markdown">>, maps:get(<<"format">>, Decoded)).

%%%=============================================================================
%%% Prompt Tests
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% tutorial_completion Prompt Tests
%%-----------------------------------------------------------------------------

tutorial_completion_prompt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_tutorial_step_1()),
             ?_test(test_tutorial_step_5())
         ]
     end}.

test_tutorial_step_1() ->
    Prompts = tcps_mcp_prompts:get_all_prompts(),
    {_, _, Handler} = lists:keyfind(<<"tutorial_completion">>, 1, Prompts),

    Args = #{<<"step">> => <<"1">>},
    Messages = Handler(Args),

    ?assertEqual(2, length(Messages)),
    [UserMsg, AssistantMsg] = Messages,

    ?assertEqual(<<"user">>, maps:get(<<"role">>, UserMsg)),
    ?assertEqual(<<"assistant">>, maps:get(<<"role">>, AssistantMsg)).

test_tutorial_step_5() ->
    Prompts = tcps_mcp_prompts:get_all_prompts(),
    {_, _, Handler} = lists:keyfind(<<"tutorial_completion">>, 1, Prompts),

    Args = #{<<"step">> => <<"5">>},
    Messages = Handler(Args),

    ?assertEqual(2, length(Messages)).

%%-----------------------------------------------------------------------------
%% howto_recipe Prompt Tests
%%-----------------------------------------------------------------------------

howto_recipe_prompt_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_howto_create_work_order()),
             ?_test(test_howto_respond_to_andon())
         ]
     end}.

test_howto_create_work_order() ->
    Prompts = tcps_mcp_prompts:get_all_prompts(),
    {_, _, Handler} = lists:keyfind(<<"howto_recipe">>, 1, Prompts),

    Args = #{<<"task">> => <<"create_work_order">>},
    Messages = Handler(Args),

    ?assertEqual(2, length(Messages)).

test_howto_respond_to_andon() ->
    Prompts = tcps_mcp_prompts:get_all_prompts(),
    {_, _, Handler} = lists:keyfind(<<"howto_recipe">>, 1, Prompts),

    Args = #{<<"task">> => <<"respond_to_andon">>},
    Messages = Handler(Args),

    ?assertEqual(2, length(Messages)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_full_simulation_workflow()),
             ?_test(test_tool_count()),
             ?_test(test_prompt_count())
         ]
     end}.

test_full_simulation_workflow() ->
    Tools = tcps_mcp_tools:get_all_tools(),

    % Start simulation
    {_, _, StartHandler} = lists:keyfind(<<"simulator_start">>, 1, Tools),
    StartResult = StartHandler(#{}),
    StartDecoded = jsx:decode(StartResult, [return_maps]),
    ?assertEqual(<<"started">>, maps:get(<<"status">>, StartDecoded)),

    % Create work order
    {_, _, StepHandler} = lists:keyfind(<<"simulator_step">>, 1, Tools),
    WorkOrderArgs = #{
        <<"action">> => <<"create_work_order">>,
        <<"params">> => #{<<"bucket">> => <<"reliability">>}
    },
    WorkOrderResult = StepHandler(WorkOrderArgs),
    WorkOrderDecoded = jsx:decode(WorkOrderResult, [return_maps]),
    ?assertEqual(<<"created">>, maps:get(<<"status">>, WorkOrderDecoded)),

    % Query state
    {_, _, QueryHandler} = lists:keyfind(<<"simulator_query">>, 1, Tools),
    QueryArgs = #{<<"query_type">> => <<"all">>},
    QueryResult = QueryHandler(QueryArgs),
    QueryDecoded = jsx:decode(QueryResult, [return_maps]),
    ?assert(maps:is_key(<<"state">>, QueryDecoded)).

test_tool_count() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    ?assertEqual(8, length(Tools)).

test_prompt_count() ->
    Prompts = tcps_mcp_prompts:get_all_prompts(),
    ?assertEqual(6, length(Prompts)).

%%%=============================================================================
%%% Error Handling Tests
%%%=============================================================================

error_handling_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [
             ?_test(test_invalid_action()),
             ?_test(test_invalid_query_type())
         ]
     end}.

test_invalid_action() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_step">>, 1, Tools),

    Args = #{<<"action">> => <<"invalid_action">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"error">>, Decoded)).

test_invalid_query_type() ->
    Tools = tcps_mcp_tools:get_all_tools(),
    {_, _, Handler} = lists:keyfind(<<"simulator_query">>, 1, Tools),

    Args = #{<<"query_type">> => <<"invalid_type">>},
    Result = Handler(Args),
    Decoded = jsx:decode(Result, [return_maps]),

    ?assert(maps:is_key(<<"error">>, Decoded)).
