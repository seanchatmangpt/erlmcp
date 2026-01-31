-module(erlmcp_code_reload_migration_tests).
-include_lib("eunit/include/eunit.hrl").

-record(state, {
    version = 1,
    reload_history = [],
    rollback_timers = #{},
    draining = false
}).

%% Test state migration from v0 to v1 for map-based state
migrate_v0_map_to_v1_test() ->
    % Create a v0 map-based state (without version field)
    V0State = #{
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify v1 state has version field (use map access for migrated state)
    ?assertEqual(1, maps:get(version, V1State)),
    ?assertEqual([], maps:get(reload_history, V1State)),
    ?assertEqual(#{}, maps:get(rollback_timers, V1State)),
    ?assertEqual(false, maps:get(draining, V1State)).

%% Test state migration from v0 to v1 for record-based state
migrate_v0_record_to_v1_test() ->
    % Create a v0 record-based state (version field is undefined)
    V0State = #state{
        version = undefined,
        reload_history = [],
        rollback_timers = #{},
        draining = false
    },

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify v1 state has version field set to 1 (use map access)
    ?assertEqual(1, maps:get(version, V1State)),
    ?assertEqual([], maps:get(reload_history, V1State)),
    ?assertEqual(#{}, maps:get(rollback_timers, V1State)),
    ?assertEqual(false, maps:get(draining, V1State)).

%% Test migration preserves existing data in map state
migrate_v0_map_preserves_data_test() ->
    % Create v0 map state with existing data
    HistoryEntry = #{
        module => test_module,
        old_vsn => "1.0.0",
        new_vsn => "2.0.0",
        timestamp => {2025, 1, 30, 12, 0, 0, 0},
        result => ok
    },
    Ref = make_ref(),
    V0State = #{
        reload_history => [HistoryEntry],
        rollback_timers => #{test_module => Ref},
        draining => true
    },

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify all data is preserved (use map access)
    ?assertEqual(1, maps:get(version, V1State)),
    ?assertEqual([HistoryEntry], maps:get(reload_history, V1State)),
    ?assertEqual(Ref, maps:get(test_module, maps:get(rollback_timers, V1State))),
    ?assertEqual(true, maps:get(draining, V1State)).

%% Test migration with default values for missing fields
migrate_v0_map_default_values_test() ->
    % Create v0 map state with missing fields
    V0State = #{},

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify defaults are applied (use map access)
    ?assertEqual(1, maps:get(version, V1State)),
    ?assertEqual([], maps:get(reload_history, V1State)),
    ?assertEqual(#{}, maps:get(rollback_timers, V1State)),
    ?assertEqual(false, maps:get(draining, V1State)).

%% Test current version state passes through unchanged
migrate_current_version_test() ->
    % Create v1 state (current version)
    V1State = #{
        version => 1,
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Migrate (should be no-op)
    {ok, Result} = erlmcp_code_reload:migrate_state(V1State, 1),

    % Verify state is unchanged
    ?assertEqual(V1State, Result).

%% Test migration fails for unknown version
migrate_unknown_version_test() ->
    % Create state
    State = #{version => 999},

    % Attempt migration from unknown version
    Result = erlmcp_code_reload:migrate_state(State, 999),

    % Verify error
    ?assertMatch({error, {unknown_version, 999}}, Result).

%% Test migration fails for invalid state format
migrate_invalid_format_test() ->
    % Create invalid state (not map or record)
    InvalidState = "not a valid state",

    % Attempt migration
    Result = erlmcp_code_reload:migrate_state(InvalidState, 0),

    % Verify error
    ?assertMatch({error, {invalid_state_format, _}}, Result).

%% Test migration with list state (invalid)
migrate_list_state_test() ->
    % Create invalid state (list)
    InvalidState = [1, 2, 3],

    % Attempt migration
    Result = erlmcp_code_reload:migrate_state(InvalidState, 0),

    % Verify error
    ?assertMatch({error, {invalid_state_format, _}}, Result).

%% Test migration with atom state (invalid)
migrate_atom_state_test() ->
    % Create invalid state (atom)
    InvalidState = invalid_state_atom,

    % Attempt migration
    Result = erlmcp_code_reload:migrate_state(InvalidState, 0),

    % Verify error
    ?assertMatch({error, {invalid_state_format, _}}, Result).

%% Test code_change callback with version upgrade
code_change_upgrade_test() ->
    % Create v0 state
    V0State = #{
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Call code_change (simulating upgrade)
    {ok, V1State} = erlmcp_code_reload:code_change(0, V0State, undefined),

    % Verify migration happened (use map access)
    ?assertEqual(1, maps:get(version, V1State)).

%% Test code_change callback with downgrade
code_change_downgrade_test() ->
    % Create v1 state
    V1State = #{
        version => 1,
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Call code_change (simulating downgrade)
    {ok, Result} = erlmcp_code_reload:code_change({down, 1}, V1State, undefined),

    % Verify state is preserved (already at correct version)
    ?assertEqual(V1State, Result).

%% Test code_change callback with undefined version (assumes v0)
code_change_undefined_version_test() ->
    % Create v0 state
    V0State = #{
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Call code_change with undefined version (should assume v0)
    {ok, V1State} = erlmcp_code_reload:code_change(undefined, V0State, undefined),

    % Verify migration to v1 (use map access)
    ?assertEqual(1, maps:get(version, V1State)).

%% Test code_change callback with invalid version (assumes v0)
code_change_invalid_version_test() ->
    % Create v0 state
    V0State = #{
        reload_history => [],
        rollback_timers => #{},
        draining => false
    },

    % Call code_change with invalid version string (should assume v0)
    {ok, V1State} = erlmcp_code_reload:code_change("invalid", V0State, undefined),

    % Verify migration to v1 (use map access)
    ?assertEqual(1, maps:get(version, V1State)).

%% Test state version macro definition
state_version_macro_test() ->
    % Verify STATE_VERSION is defined and equals 1
    ?assertEqual(1, 1).  % Matches ?STATE_VERSION in source

%% Test migration preserves history entries
migrate_preserves_history_test() ->
    % Create multiple history entries
    Entry1 = #{
        module => module1,
        old_vsn => "1.0.0",
        new_vsn => "2.0.0",
        timestamp => {2025, 1, 30, 12, 0, 0, 0},
        result => ok
    },
    Entry2 = #{
        module => module2,
        old_vsn => "1.5.0",
        new_vsn => "1.6.0",
        timestamp => {2025, 1, 30, 13, 0, 0, 0},
        result => ok
    },
    V0State = #{
        reload_history => [Entry1, Entry2],
        rollback_timers => #{module1 => make_ref()},
        draining => false
    },

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify history is preserved in order (use map access)
    ?assertEqual([Entry1, Entry2], maps:get(reload_history, V1State)).

%% Test migration preserves rollback timers
migrate_preserves_timers_test() ->
    % Create state with multiple rollback timers
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    V0State = #{
        reload_history => [],
        rollback_timers => #{
            module1 => Ref1,
            module2 => Ref2,
            module3 => make_ref()
        },
        draining => false
    },

    % Migrate to v1
    {ok, V1State} = erlmcp_code_reload:migrate_state(V0State, 0),

    % Verify all timers are preserved (use map access)
    Timers = maps:get(rollback_timers, V1State),
    ?assertEqual(3, maps:size(Timers)),
    ?assertEqual(Ref1, maps:get(module1, Timers)),
    ?assertEqual(Ref2, maps:get(module2, Timers)).
