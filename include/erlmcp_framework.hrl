%%%-------------------------------------------------------------------
%%% @doc
%%% ErlMCP Framework Header - Record Definitions
%%% @end
%%%-------------------------------------------------------------------

%% Test result record
-record(test_result,
        {scenario :: atom(),
         status :: passed | failed | skipped,
         start_time :: integer(),
         end_time :: integer(),
         duration_ms :: integer(),
         assertions_total :: integer(),
         assertions_passed :: integer(),
         assertions_failed :: integer(),
         metrics :: map(),
         errors :: [binary()],
         sub_results :: [#test_result{}]}).
%% Report summary record
-record(report_summary,
        {total_tests :: integer(),
         passed_tests :: integer(),
         failed_tests :: integer(),
         skipped_tests :: integer(),
         total_assertions :: integer(),
         assertions_passed :: integer(),
         assertions_failed :: integer(),
         total_duration_ms :: integer(),
         pass_rate :: float(),
         assertion_pass_rate :: float(),
         test_tree_depth :: integer(),
         anomalies_detected :: integer(),
         regressions_detected :: integer()}).
%% Baseline metrics record
-record(baseline_metrics,
        {metric_name :: atom(),
         baseline_id :: binary(),
         created_at :: integer(),
         test_count :: integer(),
         mean :: float(),
         std_dev :: float(),
         min :: float(),
         max :: float(),
         p50 :: float(),
         p95 :: float(),
         p99 :: float(),
         samples :: [float()]}).
%% Regression result record
-record(regression_result,
        {regression_id :: binary(),
         metric_name :: atom(),
         baseline_value :: float(),
         current_value :: float(),
         difference_percent :: float(),
         z_score :: float(),
         is_regression :: boolean(),
         confidence_level :: float(),
         anomaly_detected :: boolean(),
         severity :: none | low | medium | high | critical,
         detected_at :: integer()}).
%% Framework metrics record
-record(framework_metrics,
        {framework_startup_ms :: integer(),
         framework_shutdown_ms :: integer(),
         test_scheduling_overhead_us :: integer(),
         result_aggregation_overhead_us :: integer(),
         memory_peak_mb :: integer(),
         memory_avg_mb :: integer(),
         cpu_avg_percent :: float(),
         total_tests_executed :: integer(),
         total_assertions :: integer(),
         framework_reliability_percent :: float()}).
