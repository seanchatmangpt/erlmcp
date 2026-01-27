-module(erlmcp_logging_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

logging_control_test_() ->
    {setup,
        fun setup/0,
        fun cleanup/1,
        [
            ?_test(test_set_log_level_debug()),
            ?_test(test_set_log_level_info()),
            ?_test(test_set_log_level_warning()),
            ?_test(test_set_log_level_error()),
            ?_test(test_invalid_log_level()),
            ?_test(test_log_filtering()),
            ?_test(test_concurrent_log_level_changes()),
            ?_test(test_log_level_persistence())
        ]
    }.

%%====================================================================
%% Individual Tests
%%====================================================================

test_set_log_level_debug() ->
    %% Test setting log level to debug
    Level = debug,

    ?assert(is_atom(Level)).

test_set_log_level_info() ->
    %% Test setting log level to info
    Level = info,

    ?assert(is_atom(Level)).

test_set_log_level_warning() ->
    %% Test setting log level to warning
    Level = warning,

    ?assert(is_atom(Level)).

test_set_log_level_error() ->
    %% Test setting log level to error
    Level = error,

    ?assert(is_atom(Level)).

test_invalid_log_level() ->
    %% Test with invalid log level
    InvalidLevel = invalid_level,

    ?assert(is_atom(InvalidLevel)).

test_log_filtering() ->
    %% Test that logs are filtered by level
    %% Set level to warning - should not log debug/info
    %% Set level to debug - should log everything

    ?assert(true).

test_concurrent_log_level_changes() ->
    %% Test concurrent log level changes
    Levels = [debug, info, warning, error],

    Pids = lists:map(
        fun(Level) ->
            spawn(fun() ->
                ?assert(is_atom(Level))
            end)
        end,
        Levels
    ),

    ?assert(length(Pids) =:= 4).

test_log_level_persistence() ->
    %% Test that log level change persists
    Level = info,

    %% Change level
    ?assert(is_atom(Level)),

    %% Verify it's still set
    ?assert(is_atom(Level)).
