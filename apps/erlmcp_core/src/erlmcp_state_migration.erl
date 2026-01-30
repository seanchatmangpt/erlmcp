%%%-------------------------------------------------------------------
%%% @doc State Migration Utilities for Hot Code Loading
%%%
%%% This module provides utilities for implementing proper hot code
%%% loading with state versioning in gen_server modules. It handles:
%%%
%%% - State record version tracking
%%% - ETS table schema migration
%%% - Data transformation between versions
%%% - Backup/restore procedures
%%% - Migration validation
%%%
%%% Usage Pattern:
%%% ```erlang
%%% -record(state, {
%%%     version = v1 :: v1 | v2 | v3,
%%%     % ... other fields
%%% }).
%%%
%%% code_change(OldVsn, State, Extra) ->
%%%     erlmcp_state_migration:code_change(
%%%         ?MODULE, State, OldVsn, Extra, migration_path()).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_state_migration).
-behaviour(gen_server).

%% API
-export([
    code_change/5,
    migrate_state/3,
    migrate_ets_table/5,
    backup_ets_table/2,
    restore_ets_table/3,
    validate_migration/2,
    version/1
]).

%% Types
-type version() :: v1 | v2 | v3 | v4 | v5.
-type state_version() :: version() | {down, version()}.
-type migration_fun() :: fun((term()) -> {ok, term()} | {error, term()}).
-type migration_path() :: #{version() => migration_fun()}.

-export_type([version/0, state_version/0, migration_fun/0, migration_path/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Generic code_change implementation for gen_servers
-spec code_change(module(), term(), term(), term(), migration_path()) ->
    {ok, term()} | {error, term()}.
code_change(Module, OldState, OldVsn, Extra, MigrationPath) ->
    try
        %% Determine current version from state
        CurrentVersion = version(OldState),

        %% Log migration start
        logger:info("~p: Starting code change from ~p to ~p",
                   [Module, OldVsn, CurrentVersion]),

        %% Apply migrations sequentially
        NewState = migrate_state(OldState, OldVsn, MigrationPath),

        %% Validate final state
        case validate_migration(Module, NewState) of
            ok ->
                logger:info("~p: Code change completed successfully", [Module]),
                {ok, NewState};
            {error, Reason} ->
                logger:error("~p: Migration validation failed: ~p", [Module, Reason]),
                {error, Reason}
        end
    catch
        Class:Reason:Stack ->
            logger:error("~p: Code change failed: ~p:~p~n~p",
                        [Module, Class, Reason, Stack]),
            {error, {Class, Reason}}
    end.

%% @doc Migrate state through version path
-spec migrate_state(term(), term(), migration_path()) -> term().
migrate_state(State, {down, FromVersion}, _MigrationPath) ->
    %% Downgrade migration - reverse logic would be implemented here
    logger:warning("Downgrade migration from ~p not fully supported", [FromVersion]),
    State;
migrate_state(#{version := CurrentVersion} = State, TargetVersion, MigrationPath)
  when CurrentVersion =:= TargetVersion ->
    %% Already at target version
    State;
migrate_state(#{version := FromVersion} = State, ToVersion, MigrationPath) ->
    %% Apply migration function
    case maps:find(FromVersion, MigrationPath) of
        {ok, MigrateFun} when is_function(MigrateFun, 1) ->
            case MigrateFun(State) of
                {ok, NewState} ->
                    %% Continue migrating if needed
                    migrate_state(NewState, ToVersion, MigrationPath);
                {error, Reason} ->
                    error({migration_failed, FromVersion, Reason})
            end;
        error ->
            logger:warning("No migration function found for version ~p", [FromVersion]),
            State
    end;
migrate_state(State, _OldVsn, _MigrationPath) ->
    %% No version field - assume current version
    State.

%% @doc Migrate ETS table schema
-spec migrate_ets_table(ets:tid(), term(), version(), version(),
                        fun((term()) -> term())) -> ok.
migrate_ets_table(Table, Module, FromVersion, ToVersion, TransformFun) ->
    logger:info("~p: Migrating ETS table from ~p to ~p", [Module, FromVersion, ToVersion]),

    %% Backup original table
    BackupTable = backup_ets_table(Table, Module),

    try
        %% Transform all entries
        ets:foldl(
            fun({Key, Value}, _Acc) ->
                try TransformFun({Key, Value}) of
                    {NewKey, NewValue} ->
                        ets:insert(Table, {NewKey, NewValue});
                    NewValue when element(1, NewValue) =:= error ->
                        %% Transform returned error, skip this entry
                        logger:warning("~p: Failed to transform entry ~p: ~p",
                                      [Module, Key, NewValue]),
                        ok
                catch
                    Class:Reason:Stack ->
                        logger:error("~p: Transform failed for key ~p: ~p:~p~n~p",
                                    [Module, Key, Class, Reason, Stack]),
                        error({transform_failed, Key, Class, Reason})
                end
            end,
            ok,
            Table
        ),

        logger:info("~p: ETS table migration completed", [Module]),
        ok
    catch
        error:{transform_failed, Key, Class, Reason} ->
            %% Restore from backup
            restore_ets_table(Table, BackupTable, Module),
            error({ets_migration_failed, Key, Class, Reason})
    after
        %% Cleanup backup table
        ets:delete(BackupTable)
    end.

%% @doc Backup ETS table to temporary table
-spec backup_ets_table(ets:tid(), module()) -> ets:tid().
backup_ets_table(SourceTable, Module) ->
    BackupName = list_to_atom(atom_to_list(Module) ++ "_backup_" ++
                               integer_to_list(erlang:unique_integer([positive]))),

    %% Get source table info
    TypeInfo = ets:info(SourceTable, type),
    KeyPos = ets:info(SourceTable, keypos),
    Protection = case ets:info(SourceTable, protection) of
        public -> public;
        protected -> protected;
        private -> private
    end,

    %% Create backup table with same properties
    BackupTable = ets:new(BackupName, [
        TypeInfo,
        {keypos, KeyPos},
        Protection
    ]),

    %% Copy all entries
    ets:foldl(
        fun(Entry, _Acc) ->
            ets:insert(BackupTable, Entry)
        end,
        ok,
        SourceTable
    ),

    logger:debug("~p: Created backup table ~p with ~p entries",
                 [Module, BackupName, ets:info(BackupTable, size)]),
    BackupTable.

%% @doc Restore ETS table from backup
-spec restore_ets_table(ets:tid(), ets:tid(), module()) -> ok.
restore_ets_table(TargetTable, BackupTable, Module) ->
    logger:warning("~p: Restoring from backup table", [Module]),

    %% Clear target table
    ets:delete_all_objects(TargetTable),

    %% Restore from backup
    ets:foldl(
        fun(Entry, _Acc) ->
            ets:insert(TargetTable, Entry)
        end,
        ok,
        BackupTable
    ),

    logger:info("~p: Restore completed, ~p entries restored",
                [Module, ets:info(TargetTable, size)]),
    ok.

%% @doc Validate migrated state structure
-spec validate_migration(module(), term()) -> ok | {error, term()}.
validate_migration(Module, State) when is_map(State); is_record(State, state) ->
    try
        %% Check for required fields
        case erlang:is_map(State) of
            true ->
                %% Map-based state - check version key
                case maps:is_key(version, State) of
                    true -> ok;
                    false -> {error, {missing_version, State}}
                end;
            false ->
                %% Record-based state - assume valid if it's a record
                ok
        end
    catch
        _:Reason ->
            {error, {validation_failed, Reason}}
    end;
validate_migration(_Module, State) ->
    %% Unknown state format
    {error, {invalid_state_format, State}}.

%% @doc Extract version from state
-spec version(term()) -> version().
version(#{version := Ver}) when is_atom(Ver) ->
    Ver;
version(#state{version = Ver}) when is_atom(Ver) ->
    Ver;
version(_) ->
    %% No version field - assume v1 (legacy)
    v1.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Check if term is a record
-is_record(Term, Rec) when is_tuple(Term), size(Term) > 0, element(1, Term) =:= Rec -> true;
-is_record(_, _) -> false.
