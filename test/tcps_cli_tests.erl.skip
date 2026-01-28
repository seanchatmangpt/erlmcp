%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Comprehensive Tests
%%%
%%% Tests for all TCPS CLI commands and functionality.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Setup/Cleanup
%%%=============================================================================

setup() ->
    % Start required servers
    tcps_andon:start(),
    tcps_kanban:start_link(),
    tcps_root_cause:start_link(),
    ok.

cleanup(_) ->
    % Stop servers
    catch tcps_kanban:stop(),
    catch tcps_root_cause:stop(),
    catch tcps_andon:stop(),
    ok.

%%%=============================================================================
%%% Configuration Tests
%%%=============================================================================

config_test_() ->
    {"Configuration management tests",
     [
      ?_test(test_default_config()),
      ?_test(test_load_config()),
      ?_test(test_set_get_config())
     ]}.

test_default_config() ->
    Config = tcps_cli_config:default_config(),
    ?assertMatch(#{ontology_path := _}, Config),
    ?assertMatch(#{output_format := table}, Config).

test_load_config() ->
    Config = tcps_cli_config:load(),
    ?assert(is_map(Config)),
    ?assert(maps:size(Config) > 0).

test_set_get_config() ->
    % Note: This would normally write to ~/.tcps/config
    % For testing, we just verify the API works
    Value = tcps_cli_config:get(output_format, table),
    ?assertEqual(table, Value).

%%%=============================================================================
%%% Formatting Tests
%%%=============================================================================

format_test_() ->
    {"Output formatting tests",
     [
      ?_test(test_format_timestamp()),
      ?_test(test_format_duration()),
      ?_test(test_format_bytes()),
      ?_test(test_format_percentage())
     ]}.

test_format_timestamp() ->
    Timestamp = 1706284800000,  % 2024-01-26 12:00:00
    Result = tcps_cli_format:format_timestamp(Timestamp),
    ?assert(is_binary(Result)),
    ?assert(byte_size(Result) > 0).

test_format_duration() ->
    ?assertEqual(<<"30.0s">>, tcps_cli_format:format_duration(30)),
    ?assertEqual(<<"2.0m">>, tcps_cli_format:format_duration(120)),
    ?assertEqual(<<"1.0h">>, tcps_cli_format:format_duration(3600)).

test_format_bytes() ->
    ?assertEqual(<<"512B">>, tcps_cli_format:format_bytes(512)),
    ?assertEqual(<<"1.0KB">>, tcps_cli_format:format_bytes(1024)),
    ?assertEqual(<<"1.0MB">>, tcps_cli_format:format_bytes(1024*1024)).

test_format_percentage() ->
    Result = tcps_cli_format:format_percentage(75.5),
    ?assertEqual(<<"75.5%">>, Result).

%%%=============================================================================
%%% Work Order CLI Tests
%%%=============================================================================

work_order_cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_work_order_creation()),
      ?_test(test_work_order_completion())
     ]}.

test_work_order_creation() ->
    % Test creating a work order via API
    Signal = #{
        bucket => security,
        priority => 10,
        payload => #{
            title => <<"Test work order">>,
            description => <<"Test description">>
        }
    },

    case tcps_kanban:process_pull_signal(Signal) of
        {ok, OrderId} ->
            ?assert(is_binary(OrderId)),
            ?assert(byte_size(OrderId) > 0);
        {error, Reason} ->
            ?debugFmt("Work order creation failed: ~p", [Reason]),
            ?assert(false)
    end.

test_work_order_completion() ->
    % Create work order first
    Signal = #{
        bucket => reliability,
        priority => 50,
        payload => #{title => <<"Test">>}
    },

    {ok, OrderId} = tcps_kanban:process_pull_signal(Signal),

    % Complete it
    Result = tcps_kanban:complete_work_order(reliability, OrderId),
    ?assertEqual(ok, Result).

%%%=============================================================================
%%% Andon CLI Tests
%%%=============================================================================

andon_cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_andon_trigger()),
      ?_test(test_andon_status()),
      ?_test(test_andon_resolve())
     ]}.

test_andon_trigger() ->
    Context = #{
        sku_id => <<"test_sku_001">>,
        stage => testing,
        details => #{message => <<"Test failure">>}
    },

    case tcps_andon:trigger_andon(test_failure, Context) of
        {ok, AndonId} ->
            ?assert(is_binary(AndonId)),
            ?assert(byte_size(AndonId) > 0);
        {error, Reason} ->
            ?debugFmt("Andon trigger failed: ~p", [Reason]),
            ?assert(false)
    end.

test_andon_status() ->
    SkuId = <<"test_sku_002">>,

    % Initially should not be blocked
    ?assertEqual(false, tcps_andon:is_blocked(SkuId)),

    % Trigger andon
    Context = #{
        sku_id => SkuId,
        stage => testing,
        details => #{}
    },
    {ok, _AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % Now should be blocked
    ?assertEqual(true, tcps_andon:is_blocked(SkuId)).

test_andon_resolve() ->
    % Trigger andon
    Context = #{
        sku_id => <<"test_sku_003">>,
        stage => testing,
        details => #{}
    },
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % Resolve it
    Resolution = #{
        root_cause => <<"Test root cause">>,
        fix_applied => <<"Test fix">>,
        prevention_added => <<"Test prevention">>
    },

    Result = tcps_andon:resolve_andon(AndonId, Resolution),
    ?assertEqual(ok, Result).

%%%=============================================================================
%%% Root Cause CLI Tests
%%%=============================================================================

root_cause_cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_root_cause_analysis())
     ]}.

test_root_cause_analysis() ->
    AndonId = <<"test_andon_004">>,
    Problem = <<"Test problem">>,

    % Start analysis
    {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, Problem),
    ?assert(is_binary(AnalysisId)),

    % Add 5 whys
    lists:foreach(fun(N) ->
        Answer = iolist_to_binary(io_lib:format("Why #~p answer", [N])),
        Result = tcps_root_cause:add_why(AnalysisId, N, Answer),
        ?assertEqual(ok, Result)
    end, lists:seq(1, 5)),

    % Finalize
    RootCause = <<"Root cause">>,
    Prevention = <<"Prevention action">>,

    case tcps_root_cause:finalize_analysis(AnalysisId, RootCause, Prevention) of
        {ok, Result} ->
            ?assertMatch(#{receipt := _, prevention_delta := _}, Result);
        {error, Reason} ->
            ?debugFmt("Finalization failed: ~p", [Reason]),
            ?assert(false)
    end.

%%%=============================================================================
%%% Kanban CLI Tests
%%%=============================================================================

kanban_cli_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_kanban_wip_limits()),
      ?_test(test_kanban_pull_signal())
     ]}.

test_kanban_wip_limits() ->
    % Get initial status
    Status1 = tcps_kanban:get_wip_status(security),
    ?assertMatch(#{current := 0, limit := 5}, Status1),

    % Set new limit
    ok = tcps_kanban:set_wip_limit(security, 10),

    % Verify change
    Status2 = tcps_kanban:get_wip_status(security),
    ?assertMatch(#{limit := 10}, Status2).

test_kanban_pull_signal() ->
    % Reset state first
    tcps_kanban:reset_state(),

    Signal = #{
        bucket => cost,
        priority => 30,
        payload => #{test => <<"data">>}
    },

    % Process pull signal
    case tcps_kanban:process_pull_signal(Signal) of
        {ok, OrderId} ->
            ?assert(is_binary(OrderId)),

            % Verify WIP increased
            Status = tcps_kanban:get_wip_status(cost),
            ?assertMatch(#{current := 1}, Status);
        {error, Reason} ->
            ?debugFmt("Pull signal failed: ~p", [Reason]),
            ?assert(false)
    end.

%%%=============================================================================
%%% Kaizen CLI Tests
%%%=============================================================================

kaizen_cli_test_() ->
    {"Kaizen CLI tests",
     [
      ?_test(test_metrics_collection()),
      ?_test(test_waste_identification())
     ]}.

test_metrics_collection() ->
    Today = date(),
    Yesterday = subtract_days(Today, 1),

    Metrics = tcps_kaizen:collect_metrics({Yesterday, Today}),

    ?assertMatch(#{
        lead_time := _,
        defect_rate := _,
        rework_pct := _,
        cycle_time := _,
        first_pass_yield := _,
        throughput := _
    }, Metrics).

test_waste_identification() ->
    WastePoints = tcps_kaizen:identify_waste_points(),
    ?assert(is_list(WastePoints)).

%%%=============================================================================
%%% Integration Tests
%%%=============================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(test_complete_workflow())
     ]}.

test_complete_workflow() ->
    % 1. Create work order
    Signal = #{
        bucket => reliability,
        priority => 50,
        payload => #{title => <<"Integration test">>}
    },
    {ok, WorkOrderId} = tcps_kanban:process_pull_signal(Signal),

    % 2. Simulate SKU generation
    SkuId = <<"integration_sku_001">>,

    % 3. Trigger andon (simulating test failure)
    Context = #{
        sku_id => SkuId,
        stage => testing,
        details => #{message => <<"Integration test failure">>}
    },
    {ok, AndonId} = tcps_andon:trigger_andon(test_failure, Context),

    % 4. Verify SKU is blocked
    ?assertEqual(true, tcps_andon:is_blocked(SkuId)),

    % 5. Start root cause analysis
    {ok, AnalysisId} = tcps_root_cause:start_analysis(AndonId, <<"Test failure">>),

    % 6. Complete 5 whys
    lists:foreach(fun(N) ->
        ok = tcps_root_cause:add_why(AnalysisId, N, <<"Answer">>)
    end, lists:seq(1, 5)),

    % 7. Finalize analysis
    {ok, _} = tcps_root_cause:finalize_analysis(AnalysisId,
                                                <<"Root cause">>,
                                                <<"Prevention">>),

    % 8. Resolve andon
    Resolution = #{
        root_cause => <<"Root cause">>,
        fix_applied => <<"Fix">>,
        prevention_added => <<"Prevention">>
    },
    ok = tcps_andon:resolve_andon(AndonId, Resolution),

    % 9. Verify SKU is unblocked
    ?assertEqual(false, tcps_andon:is_blocked(SkuId)),

    % 10. Complete work order
    ok = tcps_kanban:complete_work_order(reliability, WorkOrderId),

    ?assert(true).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

subtract_days(Date, Days) ->
    GregorianDays = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(GregorianDays - Days).
