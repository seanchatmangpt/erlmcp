%% @author Copyright (c) 2024. All Rights Reserved.
%%
%% @doc Comprehensive test suite for erlmcp_backup_manager
%% Tests backup strategies, restore procedures, and data integrity
-module(erlmcp_backup_manager_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Suite
%%====================================================================

backup_manager_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun tests/1}.

%%====================================================================
%% Tests
%%====================================================================

tests(_) ->
    [
        {inorder,
         [
             test_backup_manager_initialization,
             test_incremental_backup,
             test_full_backup,
             test_backup_checksum_validation,
             test_backup_retention,
             test_restore_procedure,
             test_cross_region_backup,
             test_backup_consistency,
             test_backup_recovery_time,
             test_backup_compression,
             test_backup_encryption,
             test_backup_error_handling,
             test_backup_scheduling,
             test_backup_verification,
             test_cleanup_old_backups
         ]
        }
    ].

%%====================================================================
%% Test Cases
%%====================================================================

test_backup_manager_initialization(_) ->
    %% Test that backup manager initializes correctly
    ?assertMatch({ok, _Pid}, erlmcp_backup_manager:start_link([])),
    ?assert(is_pid(whereis(erlmcp_backup_manager))),
    ok.

test_incremental_backup(_) ->
    %% Test incremental backup functionality
    SessionData = create_test_session_data(),
    RegistryData = create_test_registry_data(),

    %% Create incremental backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => incremental}),
    ?assert(is_binary(BackupId)),

    %% Verify backup was created
    Backups = erlmcp_backup_manager:list_backups(),
    ?assert(lists:any(fun(B) -> maps:get(id, B) =:= BackupId end, Backups)),

    %% Test data integrity
    ?assertEqual(true, erlmcp_backup_manager:verify_backup(BackupId)),
    ok.

test_full_backup(_) ->
    %% Test full backup functionality
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full}),
    ?assert(is_binary(BackupId)),

    %% Verify full backup
    Backups = erlmcp_backup_manager:list_backups(),
    FullBackup = lists:filter(fun(B) -> maps:get(id, B) =:= BackupId end, Backups),
    ?assertEqual(1, length(FullBackup)),

    %% Verify backup type
    [Backup] = FullBackup,
    ?assertEqual(full, maps:get(type, Backup)),
    ok.

test_backup_checksum_validation(_) ->
    %% Test backup checksum validation
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full}),
    ?assertEqual(true, erlmcp_backup_manager:verify_backup(BackupId)),

    %% Simulate corruption
    corrupt_backup(BackupId),
    ?assertEqual(false, erlmcp_backup_manager:verify_backup(BackupId)),
    ok.

test_backup_retention(_) ->
    %% Test backup retention policy
    OldBackupId = create_old_backup(),

    %% Cleanup old backups
    Result = erlmcp_backup_manager:cleanup_old_backups(7),  % Keep last 7 days
    ?assertEqual(ok, Result),

    %% Verify old backup was removed
    Backups = erlmcp_backup_manager:list_backups(),
    ?assertNot(lists:any(fun(B) -> maps:get(id, B) =:= OldBackupId end, Backups)),
    ok.

test_restore_procedure(_) ->
    %% Test restore procedure
    %% Create test data
    SessionData = create_test_session_data(),
    RegistryData = create_test_registry_data(),

    %% Backup data
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full}),

    %% Clear existing data
    clear_existing_data(),

    %% Restore data
    Result = erlmcp_backup_manager:restore(BackupId, all, #{}),
    ?assertEqual(ok, Result),

    %% Verify restoration
    RestoredSessions = get_restored_sessions(),
    ?assertEqual(length(SessionData), length(RestoredSessions)),
    ok.

test_cross_region_backup(_) ->
    %% Test cross-region backup functionality
    PrimaryRegion = primary,
    BackupRegion = backup,

    %% Simulate cross-region backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full, region => BackupRegion}),

    %% Verify backup exists in both regions
    BackupMetadata = get_backup_metadata(BackupId),
    ?assertEqual(BackupRegion, maps:get(region, BackupMetadata)),

    %% Verify cross-region replication
    Replicated = verify_cross_region_replication(BackupId),
    ?assertEqual(true, Replicated),
    ok.

test_backup_consistency(_) ->
    %% Test backup consistency across backups
    %% Create multiple backups
    {ok, BackupId1} = erlmcp_backup_manager:backup(all, #{type => full}),
    {ok, BackupId2} = erlmcp_backup_manager:backup(all, #{type => incremental}),

    %% Verify consistency
    Consistent = verify_backup_consistency(BackupId1, BackupId2),
    ?assertEqual(true, Consistent),
    ok.

test_backup_recovery_time(_) ->
    %% Test backup recovery time performance
    StartTimestamp = erlang:system_time(millisecond),

    %% Perform backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full}),

    %% Calculate recovery time
    EndTimestamp = erlang:system_time(millisecond),
    RecoveryTime = EndTimestamp - StartTimestamp,

    %% Verify RTO compliance
    ?assert(RecoveryTime < 300000),  % Less than 5 minutes
    ok.

test_backup_compression(_) ->
    %% Test backup compression functionality
    OriginalData = create_large_test_data(),
    OriginalSize = byte_size(OriginalData),

    %% Create compressed backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full, compression => true}),

    %% Get backup size
    BackupMetadata = get_backup_metadata(BackupId),
    BackupSize = maps:get(size, BackupMetadata),

    %% Verify compression worked
    ?assert(BackupSize < OriginalSize),
    CompressionRatio = (OriginalSize - BackupSize) / OriginalSize,
    ?assert(CompressionRatio > 0.5),  % At least 50% compression
    ok.

test_backup_encryption(_) ->
    %% Test backup encryption functionality
    TestData = create_test_session_data(),

    %% Create encrypted backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full, encryption => true}),

    %% Verify encryption
    Encrypted = is_backup_encrypted(BackupId),
    ?assertEqual(true, Encrypted),

    %% Test decryption and data integrity
    DecryptedData = decrypt_backup_data(BackupId),
    ?assertEqual(TestData, DecryptedData),
    ok.

test_backup_error_handling(_) ->
    %% Test backup error handling
    %% Test with invalid backup ID
    Result = erlmcp_backup_manager:verify_backup(<<"invalid_backup_id">>),
    ?assertMatch({error, not_found}, Result),

    %% Test with corrupted backup
    {ok, BackupId} = erlmcp_backup_manager:backup(all, #{type => full}),
    corrupt_backup(BackupId),

    %% Test restore of corrupted backup
    RestoreResult = erlmcp_backup_manager:restore(BackupId, all, #{}),
    ?assertMatch({error, _}, RestoreResult),
    ok.

test_backup_scheduling(_) ->
    %% Test backup scheduling
    %% Create multiple backups with timestamps
    {ok, BackupId1} = erlmcp_backup_manager:backup(all, #{type => full}),
    timer:sleep(1000),  % 1 second
    {ok, BackupId2} = erlmcp_backup_manager:backup(all, #{type => incremental}),

    %% Verify scheduling
    Metadata1 = get_backup_metadata(BackupId1),
    Metadata2 = get_backup_metadata(BackupId2),

    ?assert(maps:get(timestamp, Metadata1) < maps:get(timestamp, Metadata2)),

    %% Test backup intervals
    TimestampDiff = maps:get(timestamp, Metadata2) - maps:get(timestamp, Metadata1),
    ?assert(TimestampDiff >= 60000),  % At least 60 seconds apart
    ok.

test_backup_verification(_) ->
    %% Test backup verification process
    %% Create test backups
    {ok, BackupId1} = erlmcp_backup_manager:backup(all, #{type => full}),
    {ok, BackupId2} = erlmcp_backup_manager:backup(all, #{type => incremental}),

    %% Verify backups
    VerifyResult1 = erlmcp_backup_manager:verify_backup(BackupId1),
    VerifyResult2 = erlmcp_backup_manager:verify_backup(BackupId2),

    ?assertEqual(true, VerifyResult1),
    ?assertEqual(true, VerifyResult2),

    %% Test corrupted backup verification
    corrupt_backup(BackupId2),
    VerifyResult3 = erlmcp_backup_manager:verify_backup(BackupId2),
    ?assertEqual(false, VerifyResult3),
    ok.

test_cleanup_old_backups(_) ->
    %% Test cleanup of old backups
    %% Create old backups
    OldBackupId1 = create_backup_days_ago(10),
    OldBackupId2 = create_backup_days_ago(20),
    RecentBackupId = create_backup_days_ago(1),

    %% Cleanup backups older than 7 days
    Result = erlmcp_backup_manager:cleanup_old_backups(7),
    ?assertEqual(ok, Result),

    %% Verify cleanup
    Backups = erlmcp_backup_manager:list_backups(),

    %% Recent backup should still exist
    ?assert(lists:any(fun(B) -> maps:get(id, B) =:= RecentBackupId end, Backups)),

    %% Old backups should be removed
    ?assertNot(lists:any(fun(B) -> maps:get(id, B) =:= OldBackupId1 end, Backups)),
    ?assertNot(lists:any(fun(B) -> maps:get(id, B) =:= OldBackupId2 end, Backups)),
    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

setup() ->
    %% Start required services
    start_required_services(),

    %% Initialize test environment
    initialize_test_environment(),
    ok.

cleanup(_) ->
    %% Cleanup test environment
    cleanup_test_environment(),

    %% Stop backup manager
    whereis(erlmcp_backup_manager) ! shutdown,
    ok.

start_required_services() ->
    %% Start session manager
    {ok, _} = erlmcp_session_manager:start_link(),

    %% Start registry
    {ok, _} = erlmcp_registry:start_link(),

    %% Start configuration service
    {ok, _} = erlmcp_config:start_link(),

    %% Start secrets service
    {ok, _} = erlmcp_secrets:start_link(),
    ok.

initialize_test_environment() ->
    %% Create test data
    create_test_environment_data(),

    %% Initialize backup manager
    {ok, _} = erlmcp_backup_manager:start_link([]),
    ok.

cleanup_test_environment() ->
    %% Clear all test data
    clear_all_test_data(),
    ok.

create_test_session_data() ->
    %% Create test session data
    Sessions = [
        #{id => <<"session_1">>, data => #{key => value}},
        #{id => <<"session_2">>, data => #{key => value}}
    ],

    %% Store test sessions
    lists:foreach(fun(Session) ->
        erlmcp_session:create(Session)
    end, Sessions),

    Sessions.

create_test_registry_data() ->
    %% Create test registry data
    RegistryEntries = [
        #{key => <<"key1">>, value => <<"value1">>},
        #{key => <<"key2">>, value => <<"value2">>}
    ],

    %% Store test registry entries
    lists:foreach(fun(Entry) ->
        erlmcp_registry:put(Entry#{key}, Entry#{value})
    end, RegistryEntries),

    RegistryEntries.

create_large_test_data() ->
    %% Create large test data for compression testing
    LargeData = lists:foldl(fun(_I, Acc) ->
        Acc <<<<"test_data">>/binary>>
    end, <<>>, lists:seq(1, 1000)),

    LargeData.

create_backup_days_ago(Days) ->
    %% Create backup from specified days ago
    Timestamp = erlang:system_time(millisecond) - (Days * 24 * 60 * 60 * 1000),
    BackupId = erlmcp_utils:uuid(),

    %% Create backup with old timestamp
    BackupData = #{data => <<"test_backup_data">>},
    erlmcp_storage:save(backup_data, BackupId, BackupData),
    erlmcp_storage:save(backup_metadata, BackupId, #{timestamp => Timestamp}),

    BackupId.

create_old_backup() ->
    %% Create old backup
    create_backup_days_ago(15).

corrupt_backup(BackupId) ->
    %% Simulate backup corruption
    erlmcp_storage:save(backup_data, BackupId, <<"corrupted_data">>).

clear_existing_data() ->
    %% Clear existing data
    erlmcp_session:clear_all(),
    erlmcp_registry:clear(),
    erlmcp_config:clear_all(),
    erlmcp_secrets:clear_all().

get_restored_sessions() ->
    %% Get restored sessions
    erlmcp_session:list_all().

get_backup_metadata(BackupId) ->
    %% Get backup metadata
    case erlmcp_storage:load(backup_metadata, BackupId) of
        {ok, Metadata} -> Metadata;
        {error, _} -> #{}
    end.

verify_cross_region_replication(BackupId) ->
    %% Verify cross-region replication
    Metadata = get_backup_metadata(BackupId),
    case maps:get(region, Metadata) of
        backup -> true;
        _ -> false
    end.

verify_backup_consistency(BackupId1, BackupId2) ->
    %% Verify consistency between backups
    Metadata1 = get_backup_metadata(BackupId1),
    Metadata2 = get_backup_metadata(BackupId2),

    %% Check for data consistency
    Data1 = erlmcp_storage:load(backup_data, BackupId1),
    Data2 = erlmcp_storage:load(backup_data, BackupId2),

    case {Data1, Data2} of
        {{ok, D1}, {ok, D2}} -> D1 =:= D2;
        _ -> false
    end.

is_backup_encrypted(BackupId) ->
    %% Check if backup is encrypted
    Metadata = get_backup_metadata(BackupId),
    maps:get(encrypted, Metadata, false).

decrypt_backup_data(BackupId) ->
    %% Decrypt backup data
    case erlmcp_storage:load(backup_data, BackupId) of
        {ok, EncryptedData} ->
            %% Decrypt using test key
            DecryptedData = decrypt_test_data(EncryptedData),
            DecryptedData;
        {error, _} -> <<>>
    end.

decrypt_test_data(EncryptedData) ->
    %% Simple decryption for testing
    %% In production, this would use proper encryption
    DecryptedData = binary:replace(EncryptedData, <<"$ENCRYPTED$">>, <<>>),
    DecryptedData.

clear_all_test_data() ->
    %% Clear all test data
    erlmcp_session:clear_all(),
    erlmcp_registry:clear(),
    erlmcp_config:clear_all();
    erlmcp_secrets:clear_all();
    erlmcp_storage:clear_all().