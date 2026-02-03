-module(erlmcp_state_migration).

%% API
-export([migrate_ets_tables/2,
         rollback_ets_tables/2,
         migrate_mnesia_schemas/2,
         rollback_mnesia_schemas/2,
         migrate_transport_state/2,
         rollback_transport_state/2,
         backup_table/1,
         restore_table/2]).

-include("erlmcp.hrl").

-type version() :: binary().
-type migration_result() :: {ok, map()} | {error, term()}.

%%%===================================================================
%%% API
%%%===================================================================

-spec migrate_ets_tables(version(), version()) -> migration_result().
migrate_ets_tables(_FromVersion, ToVersion) ->
    logger:info("Migrating ETS tables to version ~s", [ToVersion]),

    %% Critical ETS tables for erlmcp
    CriticalTables = [
        erlmcp_registry,
        erlmcp_cache,
        erlmcp_session
    ],

    Results = [migrate_ets_table(Table, FromVersion, ToVersion) || Table <- CriticalTables],

    case lists:all(fun({ok, _}) -> true; (_) -> false end, Results) of
        true ->
            {ok, #{tables_migrated => length(Results), target_version => ToVersion}};
        false ->
            Failed = [{T, R} || {T, {error, R}} <- lists:zip(CriticalTables, Results)],
            {error, {ets_migration_failed, Failed}}
    end.

-spec rollback_ets_tables(version(), version()) -> migration_result().
rollback_ets_tables(FromVersion, ToVersion) ->
    logger:warning("Rolling back ETS tables from ~s to ~s", [FromVersion, ToVersion]),
    migrate_ets_tables(FromVersion, ToVersion).

-spec migrate_mnesia_schemas(version(), version()) -> migration_result().
migrate_mnesia_schemas(_FromVersion, ToVersion) ->
    logger:info("Migrating Mnesia schemas to version ~s", [ToVersion]),

    case mnesia:system_info(is_running) of
        no ->
            {ok, #{mnesia_not_used => true}};
        yes ->
            Tables = mnesia:system_info(tables),
            ErlmcpTables = [T || T <- Tables, lists:prefix("erlmcp", atom_to_list(T))],

            case ErlmcpTables of
                [] ->
                    {ok, #{no_mnesia_tables => true}};
                _ ->
                    %% Transform Mnesia table data
                    Results = [migrate_mnesia_table(Table, ToVersion) || Table <- ErlmcpTables],
                    {ok, #{tables_migrated => length(Results), target_version => ToVersion}}
            end
    end.

-spec rollback_mnesia_schemas(version(), version()) -> migration_result().
rollback_mnesia_schemas(FromVersion, ToVersion) ->
    logger:warning("Rolling back Mnesia schemas from ~s to ~s", [FromVersion, ToVersion]),
    migrate_mnesia_schemas(FromVersion, ToVersion).

-spec migrate_transport_state(version(), version()) -> migration_result().
migrate_transport_state(_FromVersion, ToVersion) ->
    logger:info("Migrating transport state to version ~s", [ToVersion]),

    %% Migrate transport process states if needed
    %% For now, transport processes are stateless wrt versions
    {ok, #{transport_state_migrated => true, target_version => ToVersion}}.

-spec rollback_transport_state(version(), version()) -> migration_result().
rollback_transport_state(FromVersion, ToVersion) ->
    logger:warning("Rolling back transport state from ~s to ~s", [FromVersion, ToVersion]),
    migrate_transport_state(FromVersion, ToVersion).

-spec backup_table(ets:tid()) -> {ok, map()} | {error, term()}.
backup_table(TableId) ->
    try
        Info = ets:info(TableId),
        Size = proplists:get_value(size, Info),
        Type = proplists:get_value(type, Info),

        %% Snapshot table data
        Data = case Size of
            0 -> [];
            N when N < 10000 ->
                ets:tab2list(TableId);
            _ when N >= 10000 ->
                %% Large table - use selective backup
                backup_table_sample(TableId, 1000)
        end,

        Backup = #{
            table => TableId,
            type => Type,
            size => Size,
            data => Data,
            timestamp => erlang:timestamp()
        },
        {ok, Backup}
    catch
        _:Error -> {error, {backup_failed, Error}}
    end.

-spec restore_table(ets:tid(), map()) -> ok | {error, term()}.
restore_table(TableId, Backup) ->
    try
        %% Clear existing table
        true = ets:delete_all_objects(TableId),

        %% Restore data
        Data = maps:get(data, Backup, []),
        lists:foreach(fun(Object) -> ets:insert(TableId, Object) end, Data),

        logger:info("Restored table ~p with ~p objects", [TableId, length(Data)]),
        ok
    catch
        _:Error -> {error, {restore_failed, Error}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

migrate_ets_table(Table, _FromVersion, ToVersion) ->
    case ets:info(Table) of
        undefined ->
            {error, {table_not_found, Table}};
        _Info ->
            try
                %% For v2.1.0 -> v3.0.0, add version tracking
                case ets:lookup(Table, schema_version) of
                    [] ->
                        %% Add version marker
                        ets:insert(Table, {schema_version, ToVersion}),
                        {ok, Table};
                    [{schema_version, Current}] when Current =:= ToVersion ->
                        {ok, Table};
                    [{schema_version, _Old}] ->
                        %% Transform table data
                        transform_ets_table(Table, ToVersion),
                        {ok, Table}
                end
            catch
                _:Error -> {error, {migration_error, Table, Error}}
            end
    end.

transform_ets_table(Table, ToVersion) ->
    %% Update schema version
    ets:insert(Table, {schema_version, ToVersion}),

    %% For backward compatibility, preserve old data format
    %% New fields will be added on-demand during access
    ok.

migrate_mnesia_table(Table, ToVersion) ->
    try
        %% Migrate Mnesia table schema if needed
        case mnesia:table_info(Table, attributes) of
            Attributes when is_list(Attributes) ->
                %% Add version tracking if not present
                case lists:member(schema_version, Attributes) of
                    false ->
                        %% For v3.0.0, we don't alter existing schemas
                        %% to ensure backward compatibility
                        ok;
                    true ->
                        ok
                end,
                {ok, Table};
            _ ->
                {error, {invalid_attributes, Table}}
        end
    catch
        _:Error -> {error, {mnesia_migration_error, Table, Error}}
    end.

backup_table_sample(TableId, SampleSize) ->
    %% Backup a sample of large tables for rollback
    Iterator = ets:first(TableId),
    backup_table_sample(TableId, Iterator, SampleSize, []).

backup_table_sample(_TableId, '$end_of_table', _N, Acc) ->
    lists:reverse(Acc);
backup_table_sample(_TableId, _Key, 0, Acc) ->
    lists:reverse(Acc);
backup_table_sample(TableId, Key, N, Acc) ->
    case ets:lookup(TableId, Key) of
        [Object] ->
            NextKey = ets:next(TableId, Key),
            backup_table_sample(TableId, NextKey, N - 1, [Object | Acc]);
        [] ->
            NextKey = ets:next(TableId, Key),
            backup_table_sample(TableId, NextKey, N, Acc)
    end.
