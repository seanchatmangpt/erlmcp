%%%====================================================================
%%% Common Test Specification for erlmcp Integration Tests
%%%====================================================================
%%% Configuration for running integration tests with Common Test
%%%
%%% Usage:
%%%   ct_run -pa _build/test/lib/*/ebin -spec test/integration/erlmcp_integration.spec
%%%====================================================================

{logdir, "log/ct"}.
{incremental, true}.

%% Cover specification
{cover, "test/integration/cover.spec"}.
{cover_opts, [verbose]}.

%% Test directories
{dir, ["test/integration"]}.

%% Include paths
{include, ["apps/erlmcp_core/include", "include"]}.

%% Test compilation options
{enable_builtin_hooks, true}.

%% Define for test environment
{def, 'INTEGRATION_TEST'}.

%% Parallel execution
{parallel, 1}.  % Run sequentially due to shared resource subscriptions

%% Maximum time per test case (30 seconds)
{timetrap, 30.seconds}.

%% Maximum time for suite initialization (60 seconds)
{init_timetrap, 60.seconds}.

%% Event log options
{event_log, []}.
{event_log_items, [error_report, progress]}.

%% Silent connections (reduce log noise)
{silent_connections, true}.

%% Scale CPU and memory for tests
{scale_cpu, 1}.
{scale_memory, 1}.

%% Auto-compile settings
{auto_compile, false}.

%% Create private directory per test case
{create_priv_dir, auto_per_tc}.
