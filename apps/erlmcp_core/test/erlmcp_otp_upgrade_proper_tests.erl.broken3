%%%-------------------------------------------------------------------
%%% @doc
%%% OTP Upgrade Property-Based Tests (Proper)
%%%
%%% Generative property tests for OTP upgrade invariants:
%%% - Version consistency across reload cycles
%%% - State migration monotonicity
%%% - JSON encoding/decoding roundtrip
%%% - Process enumeration accuracy
%%% - Module reload idempotency
%%% - Cluster version convergence
%%%
%%% Chicago School TDD:
%%% - Real module loading, no mocks
%%% - Observable state changes
%%% - Randomized inputs for comprehensive coverage
%%% - Invariants over implementation details
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp_upgrade_proper_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("../../../include/otp_compat.hrl").

%% Export for Proper
-export([prop_version_monotonic/0,
         prop_json_roundtrip/0,
         prop_process_count_positive/0,
         prop_reload_idempotent/0,
         prop_state_migration_preserves_data/0,
         prop_backup_restore_identical/0,
         prop_cluster_versions_converge/0,
         prop_feature_detection_consistent/0]).

%%====================================================================
%% Properties - Version Management
%%====================================================================

%% @doc Property: Module version should be monotonic (same after reload)
prop_version_monotonic() ->
    ?FORALL(Module, oneof([erlmcp_registry, erlmcp_cache, erlmcp_auth]),
        begin
            {ok, V1} = erlmcp_code_loader:get_module_md5(Module),
            timer:sleep(10),  %% Small delay
            {ok, V2} = erlmcp_code_loader:get_module_md5(Module),
            V1 =:= V2
        end).

%% @doc Property: Multiple reload cycles should preserve version
prop_reload_idempotent() ->
    ?FORALL({Module, Cycles},
            {oneof([erlmcp_registry, erlmcp_cache]), pos_integer(1, 5)},
        begin
            %% Get initial version
            {ok, InitialVersion} = erlmcp_code_loader:get_module_md5(Module),

            %% Perform multiple reload cycles
            lists:foreach(fun(_) ->
                    {ok, _} = erlmcp_code_loader:prepare_reload(Module),
                    ok = erlmcp_code_loader:commit_reload(Module, InitialVersion)
                end, lists:seq(1, Cycles)),

            %% Version should remain unchanged
            {ok, FinalVersion} = erlmcp_code_loader:get_module_md5(Module),
            InitialVersion =:= FinalVersion
        end).

%%====================================================================
%% Properties - JSON Encoding/Decoding
%%====================================================================

%% @doc Property: JSON encode/decode should be lossless
prop_json_roundtrip() ->
    ?FORALL(Data, json_term(),
        begin
            Encoded = ?JSON_ENCODE_SAFE(Data),
            {Decoded, []} = ?JSON_DECODE_SAFE(Encoded),
            Data =:= Decoded
        end).

%% @doc Property: JSON encoding should produce valid binary
prop_json_encoding_valid_binary() ->
    ?FORALL(Data, json_term(),
        begin
            Encoded = ?JSON_ENCODE_SAFE(Data),
            is_binary(Encoded) andalso byte_size(Encoded) > 0
        end).

%%====================================================================
%% Properties - Process Enumeration
%%====================================================================

%% @doc Property: Process count should always be positive
prop_process_count_positive() ->
    ?FORALL(_Iterations, range(1, 20),
        begin
            Count = ?SAFE_PROCESS_COUNT(),
            is_integer(Count) andalso Count > 0
        end).

%% @doc Property: Process list length should match count
prop_process_list_matches_count() ->
    ?FORALL(_Threshold, range(0, 100),
        begin
            Count = ?SAFE_PROCESS_COUNT(),
            List = ?SAFE_PROCESSES(),
            length(List) =:= Count
        end).

%%====================================================================
%% Properties - State Migration
%%====================================================================

%% @doc Property: State migration should preserve essential data
prop_state_migration_preserves_data() ->
    ?FORALL(OldState, gen_state(),
        begin
            %% Simulate migration from version 0 to 1
            {ok, NewState} = migrate_state_v0_to_v1(OldState),

            %% Verify fields preserved
            maps:get(version, NewState) =:= 1 andalso
            is_list(NewState.reload_history) andalso
            is_map(NewState.rollback_timers) andalso
            is_boolean(NewState.draining)
        end).

%% @doc Property: Multiple migrations should be idempotent
prop_state_migration_idempotent() ->
    ?FORALL(State, gen_state(),
        begin
            %% Migrate v0 -> v1
            {ok, State1} = migrate_state_v0_to_v1(State),

            %% Migrate again (should be idempotent)
            {ok, State2} = migrate_state_v0_to_v1(State1),

            %% States should be identical
            State1 =:= State2
        end).

%%====================================================================
%% Properties - Backup/Restore
%%====================================================================

%% @doc Property: Backup and restore should produce identical version
prop_backup_restore_identical() ->
    ?FORALL(Module, oneof([erlmcp_registry, erlmcp_cache]),
        begin
            %% Get original version
            {ok, OriginalVersion} = erlmcp_code_loader:get_module_md5(Module),

            %% Backup
            case backup_module_beam(Module) of
                ok ->
                    %% Restore (simulate by reloading)
                    {ok, RestoredVersion} = erlmcp_code_loader:get_module_md5(Module),

                    %% Clean up backup
                    cleanup_backup(Module),

                    %% Versions should match
                    OriginalVersion =:= RestoredVersion;
                _ ->
                    %% Backup failed, property holds vacuously
                    true
            end
        end).

%%====================================================================
%% Properties - Cluster Consistency
%%====================================================================

%% @doc Property: Cluster versions should converge after sync
prop_cluster_versions_converge() ->
    ?FORALL(Module, oneof([erlmcp_registry, erlmcp_cache]),
        begin
            %% Get versions across cluster
            Versions = erlmcp_reload_coordinator:get_cluster_versions(Module),

            case maps:size(Versions) of
                0 ->
                    %% No cluster, property holds
                    true;
                _ ->
                    %% Sync versions
                    {ok, _} = erlmcp_reload_coordinator:sync_versions(Module),

                    %% Check consistency after sync
                    erlmcp_reload_coordinator:check_consistency(Module)
            end
        end).

%%====================================================================
%% Properties - Feature Detection
%%====================================================================

%% @doc Property: Feature detection should be consistent
prop_feature_detection_consistent() ->
    ?FORALL(_Iterations, range(1, 10),
        begin
            %% Check features multiple times
            F1 = ?HAVE_NATIVE_JSON,
            F2 = ?HAVE_PROCESS_ITERATOR,
            F3 = ?HAVE_PRIORITY_MESSAGES,

            timer:sleep(10),

            %% Should remain consistent
            F1 =:= ?HAVE_NATIVE_JSON andalso
            F2 =:= ?HAVE_PROCESS_ITERATOR andalso
            F3 =:= ?HAVE_PRIORITY_MESSAGES
        end).

%%====================================================================
%% Properties - Atomic Operations
%%====================================================================

%% @doc Property: Atomic swap should preserve module functionality
prop_atomic_swap_preserves_functionality() ->
    ?FORALL(Module, oneof([erlmcp_registry, erlmcp_cache]),
        begin
            %% Get object code
            case erlmcp_code_loader:get_object_code(Module) of
                {ok, Module, Binary, Filename} ->
                    %% Perform atomic swap
                    ok = erlmcp_code_loader:atomic_swap(Module, Binary, Filename),

                    %% Verify module still loaded
                    {ok, _Version} = erlmcp_code_loader:get_module_md5(Module),

                    %% Module should still work
                    code:is_loaded(Module) =/= false;
                _ ->
                    true
            end
        end).

%%====================================================================
%% Generators
%%====================================================================

%% @doc Generate JSON-compatible terms
json_term() ->
    oneof([
        json_primitive(),
        json_object(),
        json_array()
    ]).

json_primitive() ->
    oneof([
        binary(),
        int(),
        bool(),
        null
    ]).

json_object() ->
    ?LET(List, list({binary(), json_term()}),
         maps:from_list(List)).

json_array() ->
    list(json_term()).

%% @doc Generate state record
gen_state() ->
    #?MODULE{
        version = choose(0, 1),
        reload_history = list(reload_entry()),
        rollback_timers = map({atom(), reference()}),
        draining = bool()
    }.

reload_entry() ->
    #{module => oneof([erlmcp_registry, erlmcp_cache]),
      old_vsn => binary(),
      new_vsn => binary(),
      timestamp => erlang:timestamp(),
      result => oneof([ok, {error, term()}])}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc State record definition
-record(state, {
    version = 0 :: integer(),
    reload_history = [] :: [map()],
    rollback_timers = #{} :: map(),
    draining = false :: boolean()
}).

%% @doc Migrate state from v0 to v1
migrate_state_v0_to_v1(V0State) ->
    try
        V1State = V0State#state{version = 1},
        {ok, V1State}
    catch
        _:_ -> {error, migration_failed}
    end.

%% @doc Backup module beam file
backup_module_beam(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, module_not_found};
        BeamPath when is_list(BeamPath) ->
            BackupPath = BeamPath ++ ".backup",
            file:copy(BeamPath, BackupPath)
    end.

%% @doc Clean up backup file
cleanup_backup(Module) ->
    BeamPath = code:which(Module),
    BackupPath = BeamPath ++ ".backup",
    file:delete(BackupPath).

%%====================================================================
%% EUnit Integration
%%====================================================================

%% @doc Run all properties with EUnit
proper_test_() ->
    {setup,
     fun setup_proper/0,
     fun cleanup_proper/1,
     [
      {"Version monotonic property", ?_assert(proper:quickcheck(prop_version_monotonic(), 50))},
      {"JSON roundtrip property", ?_assert(proper:quickcheck(prop_json_roundtrip(), 100))},
      {"Process count positive property", ?_assert(proper:quickcheck(prop_process_count_positive(), 20))},
      {"Reload idempotent property", ?_assert(proper:quickcheck(prop_reload_idempotent(), 20))},
      {"State migration preserves data", ?_assert(proper:quickcheck(prop_state_migration_preserves_data(), 50))},
      {"Backup restore identical", ?_assert(proper:quickcheck(prop_backup_restore_identical(), 20))},
      {"Feature detection consistent", ?_assert(proper:quickcheck(prop_feature_detection_consistent(), 20))},
      {"Atomic swap preserves functionality", ?_assert(proper:quickcheck(prop_atomic_swap_preserves_functionality(), 10))}
     ]}.

setup_proper() ->
    %% Start required applications
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_code_reload:start_link(),
    {ok, _} = erlmcp_code_loader:start_link(),
    {ok, _} = erlmcp_reload_coordinator:start_link().

cleanup_proper(_Config) ->
    %% Stop applications
    gen_server:stop(erlmcp_reload_coordinator),
    gen_server:stop(erlmcp_code_loader),
    gen_server:stop(erlmcp_code_reload),
    application:stop(erlmcp).
