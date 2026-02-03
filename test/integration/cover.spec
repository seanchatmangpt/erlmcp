%%%====================================================================
%%% Cover Specification for erlmcp Integration Tests
%%%====================================================================
%%% Defines which modules to include/exclude in coverage reports
%%%
%%% Usage:
%%%   ct_run -cover test/integration/cover.spec ...
%%%====================================================================

%% Include all application modules
{incl_app, erlmcp_core}.
{incl_app, erlmcp_transports}.
{incl_app, erlmcp_observability}.
{incl_app, erlmcp_validation}.
{incl_app, erlmcp_zero_trust}.

%% Exclude test modules from coverage
{ excl_mods, [
    %% Common test modules
    '.*_SUITE$',
    '.*_tests$',

    %% Mock modules
    '.*_mock$',
    'fake_.*',

    %% Utility modules
    erlmcp_test_helpers,
    erlmcp_test_sync,

    %% Supervisors (usually minimal logic)
    '.*_sup$',

    %% Application modules
    '.*_app$'
]}.

%% Coverage level requirements ( informational only, not enforced)
{level, details}.  % Show function-level coverage
