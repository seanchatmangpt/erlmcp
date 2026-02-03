-ifndef(ERLMCP_OBSERVABILITY_HRL).
-define(ERLMCP_OBSERVABILITY_HRL, 1).

%%% Version constant for observability modules
-define(ERLMCP_VERSION, <<"3.0.0">>).

%%% Metric types
-define(COUNTER, counter).
-define(GAUGE, gauge).
-define(HISTOGRAM, histogram).
-define(SUMMARY, summary).

%%% Default histogram buckets (in seconds)
-define(HISTOGRAM_BUCKETS, [0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10]).

%%% Metric definition record
-record(metric_def, {
    name :: binary(),
    type :: counter | gauge | histogram | summary,
    description :: binary(),
    tags :: [binary()] | map(),
    registered_at :: integer() | undefined
}).

%%% Monitoring state record
-record(monitoring_state, {
    scrape_interval :: pos_integer(),
    metrics_table :: ets:tid(),
    histogram_buckets :: [number()],
    last_scrape :: integer(),
    error_count :: non_neg_integer()
}).

%%% Export type
-export_type([
    metric_def/0
]).

-endif.
