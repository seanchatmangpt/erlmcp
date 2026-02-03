%% @doc Data consistency manager for erlmcp v3
%% Implements data consistency and integrity checks for distributed systems
%% Uses CRDTs and conflict resolution strategies
-module(erlmcp_consistency_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, status/0, check_consistency/0, check_consistency/1,
         resolve_conflicts/1, verify_data_integrity/0, get_data_hashes/0, sync_regions/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records
-record(data_hash, {
    type :: session | registry | configuration | secrets,
    key :: binary(),
    hash :: binary(),
    timestamp :: integer(),
    version :: integer(),
    region :: atom()
}).

-record(conflict, {
    id :: binary(),
    type :: data_hash,
    left_data :: #data_hash{},
    right_data :: #data_hash{},
    resolution :: merge | discard_left | discard_right | manual,
    resolved :: boolean(),
    resolved_by :: binary(),
    resolved_timestamp :: integer()
}).

-record(state, {
    config :: map(),
    data_hashes :: #{binary() => #data_hash{}},
    conflicts :: #{binary() => #conflict{}},
    sync_schedule :: timer:tref(),
    check_schedule :: timer:tref(),
    regional_data :: #{atom() => map()},
    replication_log :: [binary()]
}).

%% Constants
-define(CHECK_INTERVAL, 10000).  % 10 seconds
-define(SYNC_INTERVAL, 60000).    % 1 minute
-define(HASH_ALGORITHM, sha256).
-define(MAX_CONFLICTS, 100).
-define(RETENTION_DAYS, 30).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    start_link([]).

-spec start_link(Options :: list()) -> {ok, pid()} | {error, any()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec status() -> map().
status() ->
    status([]).

-spec status(Options :: list()) -> map().
status(Options) ->
    gen_server:call(?MODULE, {status, Options}).

-spec check_consistency() -> map().
check_consistency() ->
    check_consistency([]).

-spec check_consistency(Options :: list()) -> map().
check_consistency(Options) ->
    gen_server:call(?MODULE, {check_consistency, Options}).

-spec resolve_conflicts(ConflictIds :: list(binary())) -> ok | {error, any()}.
resolve_conflicts(ConflictIds) ->
    gen_server:call(?MODULE, {resolve_conflicts, ConflictIds}).

-spec verify_data_integrity() -> map().
verify_data_integrity() ->
    gen_server:call(?MODULE, verify_data_integrity).

-spec get_data_hashes() -> [map()].
get_data_hashes() ->
    gen_server:call(?MODULE, get_data_hashes).

-spec sync_regions(Regions :: list(atom())) -> ok | {error, any()}.
sync_regions(Regions) ->
    gen_server:call(?MODULE, {sync_regions, Regions}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    %% Initialize consistency manager
    State = #state{
        config = parse_config(Options),
        data_hashes = #{},
        conflicts = #{},
        regional_data => load_regional_data(),
        replication_log = []
    },

    %% Start consistency checking
    CheckSchedule = start_consistency_checks(),

    %% Start synchronization
    SyncSchedule = start_synchronization(),

    %% Load existing data hashes
    DataHashes = load_data_hashes(),
    UpdatedState = State#state{data_hashes = DataHashes},

    %% Register for cluster events
    erlmcp_event_manager:subscribe(?MODULE, [data_changed, cluster_state_changed]),

    {ok, UpdatedState}.

handle_call({status, Options}, _From, State) ->
    Status = build_status_report(State, Options),
    {reply, Status, State};

handle_call({check_consistency, Options}, _From, State) ->
    Result = perform_consistency_check(Options),
    {reply, Result, State};

handle_call({resolve_conflicts, ConflictIds}, _From, State) ->
    %% Resolve specified conflicts
    Results = lists:map(fun(ConflictId) ->
        resolve_conflict(ConflictId, State)
    end, ConflictIds),

    %% Update conflicts
    UpdatedConflicts = lists:foldl(fun({ConflictId, Result}, Acc) ->
        case Result of
            {ok, ResolvedConflict} ->
                maps:put(ConflictId, ResolvedConflict, Acc);
            {error, _} ->
                Acc
        end
    end, State#state.conflicts, Results),

    UpdatedState = State#state{conflicts = UpdatedConflicts},

    {reply, {ok, Results}, UpdatedState};

handle_call(verify_data_integrity, _From, State) ->
    Result = verify_all_data_integrity(State),
    {reply, Result, State};

handle_call(get_data_hashes, _From, State) ->
    Hashes = [format_data_hash(Hash) || Hash <- maps:values(State#state.data_hashes)],
    {reply, Hashes, State};

handle_call({sync_regions, Regions}, _From, State) ->
    %% Sync data between regions
    Result = perform_region_sync(Regions, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({data_changed, DataType, Key, Value, Region}, State) ->
    %% Handle data change events
    UpdatedState = handle_data_change(DataType, Key, Value, Region, State),
    {noreply, UpdatedState};

handle_info({conflict_detected, Conflict}, State) ->
    %% Handle conflict detection
    UpdatedState = handle_conflict_detection(Conflict, State),
    {noreply, UpdatedState};

handle_info({sync_completed, Region, Result}, State) ->
    %% Handle sync completion
    UpdatedState = handle_sync_completion(Region, Result, State),
    {noreply, UpdatedState};

handle_info({timeout, ScheduleId, check_consistency}, State) ->
    %% Handle consistency check timeout
    UpdatedState = perform_periodic_consistency_check(State),
    {noreply, UpdatedState};

handle_info({timeout, ScheduleId, sync_regions}, State) ->
    %% Handle sync timeout
    UpdatedState = perform_periodic_sync(State),
    {noreply, UpdatedState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel schedules
    erlang:cancel_timer(State#state.sync_schedule),
    erlang:cancel_timer(State#state.check_schedule),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
 Internal Functions
%%====================================================================

parse_config(Options) ->
    Defaults = #{
        check_interval => ?CHECK_INTERVAL,
        sync_interval => ?SYNC_INTERVAL,
        max_conflicts => ?MAX_CONFLICTS,
        retention_days => ?RETENTION_DAYS,
        conflict_resolution => merge,  % merge, discard_left, discard_right, manual
        enable_crdt => true
    },
    maps:merge(Defaults, maps:from_list(Options)).

start_consistency_checks() ->
    %% Start periodic consistency checks
    erlang:send_after(?CHECK_INTERVAL, self(), {timeout, make_ref(), check_consistency}).

start_synchronization() ->
    %% Start periodic synchronization
    erlang:send_after(?SYNC_INTERVAL, self(), {timeout, make_ref(), sync_regions}).

load_regional_data() ->
    %% Load regional data from storage
    case erlmcp_config:get(regional_data, #{}) of
        #{primary := Data, backup := Backup, dr := DR} ->
            #{primary => Data, backup => Backup, dr => DR};
        _ ->
            #{primary => #{}, backup => #{}, dr => #{}}
    end.

load_data_hashes() ->
    %% Load existing data hashes from storage
    case erlmcp_storage:load(data_hashes) of
        {ok, Hashes} -> Hashes;
        {error, _} -> #{}
    end.

perform_consistency_check(Options) ->
    %% Perform comprehensive consistency check
    Region = proplists:get_value(region, Options, all),
    DataType = proplists:get_value(data_type, Options, all),

    Results = check_data_consistency(Region, DataType),

    %% Log results
    erlmcp_audit:log(consistency_check, #{
        timestamp => erlang:system_time(millisecond),
        results => Results,
        options => maps:from_list(Options)
    }),

    Results.

check_data_consistency(Region, DataType) ->
    %% Check data consistency across regions
    Primary = erlmcp_replication:get_region_data(primary),
    Backup = erlmcp_replication:get_region_data(backup),
    DR = erlmcp_replication:get_region_data(dr),

    ConsistencyResults = #{
        primary_vs_backup => compare_data_regions(Primary, Backup),
        primary_vs_dr => compare_data_regions(Primary, DR),
        backup_vs_dr => compare_data_regions(Backup, DR)
    },

    %% Detect conflicts
    Conflicts = detect_conflicts(Region, DataType, ConsistencyResults),

    #{consistency => ConsistencyResults, conflicts => Conflicts}.

compare_data_regions(Region1, Region2) ->
    %% Compare data between two regions
    Keys1 = maps:keys(Region1),
    Keys2 = maps:keys(Region2),

    %% Find missing keys
    MissingIn2 = Keys1 -- Keys2,
    MissingIn1 = Keys2 -- Keys1,

    %% Find differing values
    CommonKeys = lists:intersection(Keys1, Keys2),
    Differing = lists:filter(fun(Key) ->
        Region1#{Key} =/= Region2#{Key}
    end, CommonKeys),

    #{
        missing_in_2 => MissingIn2,
        missing_in_1 => MissingIn1,
        differing => Differing,
        total_keys => length(Keys1),
        consistent => length(MissingIn1) =:= 0 andalso length(MissingIn2) =:= 0 andalso length(Differing) =:= 0
    }.

detect_conflicts(Region, DataType, ConsistencyResults) ->
    %% Detect conflicts in data
    Conflicts = maps:fold(fun(Comparison, Result, Acc) ->
        case Result#{consistent} of
            true -> Acc;
            false ->
                %% Extract conflicts from comparison
                NewConflicts = extract_conflicts_from_comparison(Comparison, Result, Region, DataType),
                Acc ++ NewConflicts
        end
    end, [], ConsistencyResults),

    %% Limit conflicts to max count
    lists:sublist(Conflicts, 1, ?MAX_CONFLICTS).

extract_conflicts_from_comparison(Comparison, Result, Region, DataType) ->
    %% Extract conflicts from comparison result
    case Comparison of
        primary_vs_backup ->
            extract_conflicts(Result#{primary}, Result#{backup}, Region, DataType);
        primary_vs_dr ->
            extract_conflicts(Result#{primary}, Result#{dr}, Region, DataType);
        backup_vs_dr ->
            extract_conflicts(Result#{backup}, Result#{dr}, Region, DataType)
    end.

extract_conflicts(Region1Data, Region2Data, FilterRegion, FilterType) ->
    %% Extract conflicts between regions
    Conflicts = lists:foldl(fun(Key, Acc) ->
        case {FilterRegion, FilterType} of
            {all, all} ->
                case {maps:is_key(Key, Region1Data), maps:is_key(Key, Region2Data)} of
                    {true, true} ->
                        case Region1Data#{Key} =/= Region2Data#{Key} of
                            true -> [create_conflict(Key, Region1Data#{Key}, Region2Data#{Key}) | Acc];
                            false -> Acc
                        end;
                    _ -> Acc
                end;
            {all, Type} ->
                %% Filter by type
                case extract_key_type(Key) of
                    Type ->
                        case {maps:is_key(Key, Region1Data), maps:is_key(Key, Region2Data)} of
                            {true, true} ->
                                case Region1Data#{Key} =/= Region2Data#{Key} of
                                    true -> [create_conflict(Key, Region1Data#{Key}, Region2Data#{Key}) | Acc];
                                    false -> Acc
                                end;
                            _ -> Acc
                        end;
                    _ -> Acc
                end;
            {Region, all} ->
                %% Filter by region
                case extract_key_region(Key) of
                    Region ->
                        case {maps:is_key(Key, Region1Data), maps:is_key(Key, Region2Data)} of
                            {true, true} ->
                                case Region1Data#{Key} =/= Region2Data#{Key} of
                                    true -> [create_conflict(Key, Region1Data#{Key}, Region2Data#{Key}) | Acc];
                                    false -> Acc
                                end;
                            _ -> Acc
                        end;
                    _ -> Acc
                end
        end
    end, [], lists:union(maps:keys(Region1Data), maps:keys(Region2Data))).

extract_key_type(Key) ->
    %% Extract data type from key
    case binary:split(Key, <<"_">>) of
        [<<"session">>, _] -> session;
        [<<"registry">>, _] -> registry;
        [<<"configuration">>, _] -> configuration;
        [<<"secrets">>, _] -> secrets;
        _ -> unknown
    end.

extract_key_region(Key) ->
    %% Extract region from key
    case binary:split(Key, <<"_">>) of
        [_, _, RegionBin] ->
            case RegionBin of
                <<"primary">> -> primary;
                <<"backup">> -> backup;
                <<"dr">> -> dr;
                _ -> unknown
            end;
        _ -> unknown
    end.

create_conflict(Key, Value1, Value2) ->
    %% Create conflict record
    ConflictId = erlmcp_utils:uuid(),
    #conflict{
        id = ConflictId,
        type = data_hash,
        left_data = #data_hash{
            key = Key,
            hash = calculate_hash(Value1),
            timestamp erlang:system_time(millisecond),
            version = 1,
            region = extract_key_region(Key)
        },
        right_data = #data_hash{
            key = Key,
            hash = calculate_hash(Value2),
            timestamp = erlang:system_time(millisecond),
            version = 1,
            region = extract_key_region(Key)
        },
        resolution = merge,
        resolved = false,
        resolved_by = <<>>,
        resolved_timestamp = 0
    }.

calculate_hash(Value) ->
    %% Calculate hash of value
    Data = jsx:encode(Value),
    crypto:hash(?HASH_ALGORITHM, Data).

resolve_conflict(ConflictId, State) ->
    %% Resolve a specific conflict
    case State#state.conflicts#{ConflictId} of
        undefined ->
            {error, conflict_not_found};
        Conflict ->
            %% Apply conflict resolution strategy
            ResolvedConflict = apply_conflict_resolution(Conflict, State),
            {ok, ResolvedConflict}
    end.

apply_conflict_resolution(Conflict, State) ->
    %% Apply conflict resolution strategy
    case Conflict#conflict.resolution of
        merge ->
            %% Merge conflicting data using CRDT
            MergedData = merge_data(Conflict#conflict.left_data, Conflict#conflict.right_data),
            update_data_hash(MergedData, State);
        discard_left ->
            %% Keep right data
            update_data_hash(Conflict#conflict.right_data, State);
        discard_right ->
            %% Keep left data
            update_data_hash(Conflict#conflict.left_data, State);
        manual ->
            %% Manual resolution - keep as is
            Conflict
    end.

merge_data(Data1, Data2) ->
    %% Merge data using CRDT strategy
    %% Implementation would use appropriate CRDT based on data type
    %% For now, use last-write-wins
    case Data1#data_hash.timestamp > Data2#data_hash.timestamp of
        true -> Data1;
        false -> Data2
    end.

update_data_hash(DataHash, State) ->
    %% Update data hash in storage
    UpdatedHashes = maps:put(DataHash#data_hash.key, DataHash, State#state.data_hashes),
    erlmcp_storage:save(data_hashes, UpdatedHashes),

    %% Mark conflict as resolved
    UpdatedConflicts = maps:map(fun(ConflictId, Conflict) ->
        case ConflictId of
            DataHash#data_hash.key ->
                Conflict#conflict{
                    resolved = true,
                    resolved_by = erlmcp_auth:current_user(),
                    resolved_timestamp = erlang:system_time(millisecond)
                };
            _ -> Conflict
        end
    end, State#state.conflicts),

    %% Return updated conflict
    case UpdatedConflicts#{DataHash#data_hash.key} of
        undefined -> Conflict;
        ResolvedConflict -> ResolvedConflict
    end.

handle_data_change(DataType, Key, Value, Region, State) ->
    %% Handle data change events
    DataHash = #data_hash{
        type = DataType,
        key = Key,
        hash = calculate_hash(Value),
        timestamp = erlang:system_time(millisecond),
        version = get_next_version(Key, State),
        region = Region
    },

    %% Update data hash
    UpdatedHashes = maps:put(Key, DataHash, State#state.data_hashes),

    %% Log change
    erlmcp_audit:log(data_changed, #{
        timestamp => erlang:system_time(millisecond),
        data_type => DataType,
        key => Key,
        region => Region,
        version => DataHash#data_hash.version
    }),

    %% Check for conflicts
    case detect_conflict(DataHash, State) of
        {ok, Conflict} ->
            %% Notify of conflict
            erlmcp_event_manager:notify(?MODULE, conflict_detected, Conflict);
        false ->
            ok
    end,

    %% Sync to other regions
    sync_to_other_regions(DataType, Key, Value, Region),

    State#state{data_hashes = UpdatedHashes}.

detect_conflict(DataHash, State) ->
    %% Detect if new data conflicts with existing
    case State#state.data_hashes#{DataHash#data_hash.key} of
        undefined ->
            false;
        ExistingHash ->
            case ExistingHash#data_hash.hash =:= DataHash#data_hash.hash of
                true -> false;
                false ->
                    %% Create conflict
                    Conflict = #conflict{
                        id = erlmcp_utils:uuid(),
                        type = data_hash,
                        left_data = ExistingHash,
                        right_data = DataHash,
                        resolution = State#state.config#{conflict_resolution},
                        resolved = false,
                        resolved_by = <<>>,
                        resolved_timestamp = 0
                    },
                    {ok, Conflict}
            end
    end.

get_next_version(Key, State) ->
    %% Get next version number for key
    case State#state.data_hashes#{Key} of
        undefined -> 1;
        Hash -> Hash#data_hash.version + 1
    end.

sync_to_other_regions(DataType, Key, Value, SourceRegion) ->
    %% Sync data to other regions
    Regions = [primary, backup, dr],
    TargetRegions = lists:delete(SourceRegion, Regions),

    lists:foreach(fun(TargetRegion) ->
        %% Sync data to target region
        erlmcp_replication:sync(DataType, Key, Value, TargetRegion)
    end, TargetRegions).

handle_conflict_detection(Conflict, State) ->
    %% Handle conflict detection
    %% Add conflict to conflicts map
    UpdatedConflicts = maps:put(Conflict#conflict.id, Conflict, State#state.conflicts),

    %% Limit conflicts to max count
    case maps:size(UpdatedConflicts) > ?MAX_CONFLICTS of
        true ->
            %% Remove oldest conflict
            OldestConflict = find_oldest_conflict(UpdatedConflicts),
            maps:remove(OldestConflict#conflict.id, UpdatedConflicts);
        false ->
            UpdatedConflicts
    end.

find_oldest_conflict(Conflicts) ->
    %% Find the oldest conflict
    lists:min(maps:values(Conflicts), fun(C1, C2) ->
        C1#conflict.timestamp < C2#conflict.timestamp
    end).

perform_periodic_consistency_check(State) ->
    %% Perform periodic consistency check
    Result = check_data_consistency(all, all),

    %% Check if there are any inconsistencies
    case has_inconsistencies(Result) of
        true ->
            %% Trigger alert
            erlmcp_alert:notify(consistency_error, Result);
        false ->
            ok
    end,

    %% Schedule next check
    erlang:send_after(State#state.config#{check_interval}, self(), {timeout, make_ref(), check_consistency}),

    State.

has_inconsistencies(Results) ->
    %% Check if there are any inconsistencies
    lists:any(fun({_Region, Result}) ->
        not Result#{consistent}
    end, maps:to_list(Results#{consistency})).

perform_periodic_sync(State) ->
    %% Perform periodic synchronization
    Regions = [primary, backup, dr],

    lists:foreach(fun(Region) ->
        %% Sync to other regions
        OtherRegions = lists:delete(Region, Regions),
        perform_region_sync(OtherRegions, State)
    end, Regions),

    %% Schedule next sync
    erlang:send_after(State#state.config#{sync_interval}, self(), {timeout, make_ref(), sync_regions}),

    State.

perform_region_sync(Regions, State) ->
    %% Sync data between regions
    Results = lists:map(fun(TargetRegion) ->
        sync_to_region(TargetRegion, State)
    end, Regions),

    %% Log results
    erlmcp_audit:log(region_sync, #{
        timestamp => erlang:system_time(millisecond),
        regions => Regions,
        results => Results
    }),

    {ok, Results}.

sync_to_region(TargetRegion, State) ->
    %% Sync all data to target region
    Results = maps:fold(fun(Key, DataHash, Acc) ->
        case sync_data_to_region(Key, DataHash, TargetRegion) of
            {ok, _} -> Acc ++ [Key];
            {error, _} -> Acc
        end
    end, [], State#state.data_hashes),

    #{region => TargetRegion, synced_keys => length(Results), results => Results}.

sync_data_to_region(Key, DataHash, TargetRegion) ->
    %% Sync specific data to target region
    Value = erlmcp_storage:get(Key),
    erlmcp_replication:sync(DataHash#data_hash.type, Key, Value, TargetRegion).

handle_sync_completion(Region, Result, State) ->
    %% Handle sync completion
    %% Log completion
    erlmcp_audit:log(sync_completed, #{
        region => Region,
        timestamp => erlang:system_time(millisecond),
        result => Result
    }),

    %% Update replication log
    LogEntry = #{timestamp => erlang:system_time(millisecond), region => Region, result => Result},
    UpdatedLog = [LogEntry | State#state.replication_log],

    %% Limit log size
    LimitedLog = lists:sublist(UpdatedLog, 1, 1000),

    State#state{replication_log = LimitedLog}.

verify_all_data_integrity(State) ->
    %% Verify data integrity across all regions
    Regions = [primary, backup, dr],
    Results = lists:foldl(fun(Region, Acc) ->
        case verify_region_integrity(Region, State) of
            {ok, Integrity} -> Acc#{Region => Integrity};
            {error, Reason} -> Acc#{Region => {error, Reason}}
        end
    end, #{}, Regions),

    %% Detect any integrity issues
    IntegrityIssues = detect_integrity_issues(Results),

    #{results => Results, issues => IntegrityIssues}.

verify_region_integrity(Region, State) ->
    %% Verify data integrity for specific region
    RegionData = State#state.regional_data#{Region},

    %% Check data hashes
    Hashes = maps:values(RegionData),
    Verified = lists:all(fun(Data) ->
        ExpectedHash = calculate_hash(erlmcp_storage:get(Data#data_hash.key)),
        ExpectedHash =:= Data#data_hash.hash
    end, Hashes),

    #{region => Region, verified => Verified, hash_count => length(Hashes)}.

detect_integrity_issues(Results) ->
    %% Detect integrity issues
    Issues = maps:fold(fun(Region, Result, Acc) ->
        case Result of
            #{verified := false} ->
                Acc ++ [#{region => Region, issue => hash_mismatch}];
            {error, Reason} ->
                Acc ++ [#{region => Region, issue => Reason}];
            _ ->
                Acc
        end
    end, [], Results),

    Issues.

build_status_report(State, Options) ->
    %% Build comprehensive status report
    Status = #{
        data_hashes_count => maps:size(State#state.data_hashes),
        conflicts_count => maps:size(State#state.conflicts),
        next_consistency_check => erlang:system_time(millisecond) + State#state.config#{check_interval},
        next_sync => erlang:system_time(millisecond) + State#state.config#{sync_interval},
        regional_data => State#state.regional_data,
        replication_log_length => length(State#state.replication_log),
        conflicts => [format_conflict(Conflict) || Conflict <- maps:values(State#state.conflicts)],
        data_hashes => [format_data_hash(Hash) || Hash <- maps:values(State#state.data_hashes)]
    },
    Status.

format_data_hash(DataHash) ->
    %% Format data hash for display
    #{
        key => DataHash#data_hash.key,
        type => DataHash#data_hash.type,
        hash => DataHash#data_hash.hash,
        timestamp => DataHash#data_hash.timestamp,
        version => DataHash#data_hash.version,
        region => DataHash#data_hash.region
    }.

format_conflict(Conflict) ->
    %% Format conflict for display
    #{
        id => Conflict#conflict.id,
        type => Conflict#conflict.type,
        left_data => format_data_hash(Conflict#conflict.left_data),
        right_data => format_data_hash(Conflict#conflict.right_data),
        resolution => Conflict#conflict.resolution,
        resolved => Conflict#conflict.resolved,
        resolved_by => Conflict#conflict.resolved_by,
        resolved_timestamp => Conflict#conflict.resolved_timestamp
    }.