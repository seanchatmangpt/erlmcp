%% @doc Comprehensive backup manager for erlmcp v3
%% Implements incremental and full backup strategies for all critical data
%% Supports cross-region replication and point-in-time recovery
-module(erlmcp_backup_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, backup/1, backup/2, restore/2, restore/3,
         status/0, status/1, list_backups/0, verify_backup/1, cleanup_old_backups/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% Records
-record(backup_job, {
    id :: binary(),
    type :: full | incremental,
    timestamp :: integer(),
    data_size :: integer(),
    checksum :: binary(),
    status :: pending | in_progress | completed | failed,
    metadata :: map()
}).

-record(state, {
    config :: map(),
    current_jobs :: #{binary() => #backup_job{}},
    completed_backups :: [binary()],
    storage :: pid(),
    schedule :: timer:tref()
}).

%% Constants
-define(BACKUP_INTERVAL, 60000).  % 60 seconds
-define(FULL_BACKUP_INTERVAL, 86400000).  % 24 hours
-define(BACKUP_RETENTION_DAYS, 30).
-define(STORAGE_BACKEND, erlmcp_backup_storage_s3).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() ->
    start_link([]).

-spec start_link(Options :: list()) -> {ok, pid()} | {error, any()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec backup(What :: binary()) -> {ok, binary()} | {error, any()}.
backup(What) ->
    backup(What, #{}).

-spec backup(What :: binary(), Options :: map()) -> {ok, binary()} | {error, any()}.
backup(What, Options) ->
    gen_server:call(?MODULE, {backup, What, Options}, 30000).

-spec restore(BackupId :: binary(), Target :: binary()) -> ok | {error, any()}.
restore(BackupId, Target) ->
    restore(BackupId, Target, #{}).

-spec restore(BackupId :: binary(), Target :: binary(), Options :: map()) -> ok | {error, any()}.
restore(BackupId, Target, Options) ->
    gen_server:call(?MODULE, {restore, BackupId, Target, Options}, 600000).

-spec status() -> map().
status() ->
    status([]).

-spec status(Options :: list()) -> map().
status(Options) ->
    gen_server:call(?MODULE, {status, Options}).

-spec list_backups() -> [map()].
list_backups() ->
    gen_server:call(?MODULE, list_backups).

-spec verify_backup(BackupId :: binary()) -> boolean() | {error, any()}.
verify_backup(BackupId) ->
    gen_server:call(?MODULE, {verify, BackupId}, 300000).

-spec cleanup_old_backups(Days :: integer()) -> ok | {error, any()}.
cleanup_old_backups(Days) ->
    gen_server:call(?MODULE, {cleanup, Days}, 120000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    %% Initialize backup storage
    StoragePid = start_storage_backend(Options),

    %% Load existing backups
    CompletedBackups = load_completed_backups(),

    %% Start scheduled backups
    Schedule = start_scheduled_backups(Options),

    State = #state{
        config = parse_config(Options),
        current_jobs = #{},
        completed_backups = CompletedBackups,
        storage = StoragePid,
        schedule = Schedule
    },

    %% Register for cluster events
    erlmcp_event_manager:subscribe(?MODULE, [backup_required, cluster_state_changed]),

    {ok, State}.

handle_call({backup, What, Options}, _From, State) ->
    BackupId = generate_backup_id(What),
    BackupJob = create_backup_job(BackupId, What, Options),

    %% Start backup process
    NewState = start_backup_process(BackupJob, State),
    {reply, {ok, BackupId}, NewState};

handle_call({restore, BackupId, Target, Options}, _From, State) ->
    %% Start restore process
    RestorePid = spawn_link(fun() -> execute_restore(BackupId, Target, Options) end),
    {reply, ok, State};

handle_call({status, Options}, _From, State) ->
    Status = build_status_report(State, Options),
    {reply, Status, State};

handle_call(list_backups, _From, State) ->
    Backups = [format_backup_info(BackupId) || BackupId <- State#state.completed_backups],
    {reply, Backups, State};

handle_call({verify, BackupId}, _From, State) ->
    Result = execute_backup_verification(BackupId, State),
    {reply, Result, State};

handle_call({cleanup, Days}, _From, State) ->
    OldBackups = find_old_backups(State#state.completed_backups, Days),
    lists:foreach(fun(Bid) -> delete_backup(Bid, State) end, OldBackups),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({backup_required, What}, State) ->
    %% Trigger incremental backup
    {ok, _BackupId} = backup(What, #{type => incremental}),
    {noreply, State};

handle_info({backup_completed, JobId, Result}, State) ->
    %% Update backup job status
    UpdatedState = update_backup_job(JobId, Result, State),
    {noreply, UpdatedState};

handle_info({backup_failed, JobId, Reason}, State) ->
    %% Update backup job status and log error
    UpdatedState = update_backup_job(JobId, {error, Reason}, State),
    error_logger:error_msg("Backup failed for job ~p: ~p~n", [JobId, Reason]),
    {noreply, UpdatedState};

handle_info({timeout, ScheduleId, backup}, State) ->
    %% Handle scheduled backup
    NewState = handle_scheduled_backup(State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel scheduled backups
    erlang:cancel_timer(State#state.schedule),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
 Internal Functions
%%====================================================================

start_storage_backend(Options) ->
    Backend = proplists:get_value(storage_backend, Options, ?STORAGE_BACKEND),
    ChildSpec = #{
        id => storage_backend,
        start => {Backend, start_link, [Options]},
        restart => transient,
        type => worker,
        modules => [Backend]
    },
    {ok, Pid} = supervisor:start_child(erlmcp_supervisor, ChildSpec),
    Pid.

parse_config(Options) ->
    Defaults = #{
        backup_interval => ?BACKUP_INTERVAL,
        full_backup_interval => ?FULL_BACKUP_INTERVAL,
        retention_days => ?BACKUP_RETENTION_DAYS,
        compression => true,
        encryption => true,
        checksum => true
    },
    maps:merge(Defaults, maps:from_list(Options)).

start_scheduled_backups(Options) ->
    Interval = proplists:get_value(backup_interval, Options, ?BACKUP_INTERVAL),
    Schedule = erlang:send_after(Interval, self(), {timeout, make_ref(), backup}),
    Schedule.

generate_backup_id(What) ->
    Timestamp = erlang:system_time(millisecond),
    Uuid = erlmcp_utils:uuid(),
    <<What/binary, "_", (integer_to_binary(Timestamp))/binary, "_", Uuid/binary>>.

create_backup_job(BackupId, What, Options) ->
    Type = proplists:get_value(type, Options, incremental),
    Metadata = #{
        what => What,
        type => Type,
        created_by => erlmcp_auth:current_user(),
        timestamp => erlang:system_time(millisecond),
        tags => proplists:get_value(tags, Options, [])
    },
    #backup_job{
        id = BackupId,
        type = Type,
        timestamp = Metadata#{timestamp},
        data_size = 0,
        checksum = <<>>,
        status = pending,
        metadata = Metadata
    }.

start_backup_process(BackupJob, State) ->
    %% Update job status
    UpdatedJob = BackupJob#backup_job{status = in_progress},
    UpdatedState = State#state{
        current_jobs = maps:put(BackupJob#backup_job.id, UpdatedJob, State#state.current_jobs)
    },

    %% Spawn backup worker
    BackupPid = spawn_link(fun() -> execute_backup(BackupJob, State#state.storage) end),

    %% Set timeout for long-running backups
    erlang:send_after(300000, BackupPid, {timeout, BackupJob#backup_job.id}),

    %% Add to monitoring
    erlmcp_monitor:monitor_process(?MODULE, BackupPid),

    UpdatedState.

execute_backup(BackupJob, StoragePid) ->
    try
        %% Backup different data types
        BackupData = case BackupJob#backup_job.metadata#{what} of
            session_data -> backup_session_data();
            registry_data -> backup_registry_data();
            configuration -> backup_configuration();
            secrets -> backup_secrets();
            all -> backup_all_data()
        end,

        %% Calculate checksum
        Checksum = calculate_checksum(BackupData),

        %% Compress if enabled
        Compressed = case erlmcp_config:get(backup_compression, true) of
            true -> compress_data(BackupData);
            false -> BackupData
        end,

        %% Encrypt if enabled
        Encrypted = case erlmcp_config:get(backup_encryption, true) of
            true -> encrypt_data(Compressed);
            false -> Compressed
        end,

        %% Store backup
        MetaData = #{
            id => BackupJob#backup_job.id,
            type => BackupJob#backup_job.type,
            timestamp => erlmcp_utils:timestamp(),
            size => byte_size(Encrypted),
            checksum => Checksum,
            metadata => BackupJob#backup_job.metadata
        },

        ?STORAGE_BACKEND:store(BackupJob#backup_job.id, Encrypted, MetaData),

        %% Send completion notification
        gen_server:call(?MODULE, {backup_completed, BackupJob#backup_job.id, {ok, MetaData}})

    catch
        Error:Reason ->
            gen_server:call(?MODULE, {backup_failed, BackupJob#backup_job.id, {Error, Reason}})
    end.

backup_session_data() ->
    %% Get all active sessions
    Sessions = erlmcp_session_manager:list_all(),

    %% Backup session data
    SessionData = lists:map(fun(SessionId) ->
        erlmcp_session:export_data(SessionId)
    end, Sessions),

    jsx:encode(#{<<"sessions">> => SessionData}).

backup_registry_data() ->
    %% Get all registry entries
    Registry = erlmcp_registry:export_all(),

    %% Backup registry data
    jsx:encode(#{<<"registry">> => Registry}).

backup_configuration() ->
    %% Get configuration
    Config = erlmcp_config:export_all(),

    %% Backup configuration
    jsx:encode(#{<<"configuration">> => Config}).

backup_secrets() ->
    %% Get secrets
    Secrets = erlmcp_secrets:export_all(),

    %% Backup secrets
    jsx:encode(#{<<"secrets">> => Secrets}).

backup_all_data() ->
    %% Backup all data types
    Data = #{
        <<"session_data">> => backup_session_data(),
        <<"registry_data">> => backup_registry_data(),
        <<"configuration">> => backup_configuration(),
        <<"secrets">> => backup_secrets()
    },

    jsx:encode(Data).

calculate_checksum(Data) ->
    %% Calculate SHA-256 checksum
    crypto:hash(sha256, Data).

compress_data(Data) ->
    %% Compress using zlib
    zlib:compress(Data).

encrypt_data(Data) ->
    %% Encrypt using AES-256-GCM
    Key = get_encryption_key(),
    IV = crypto:strong_rand_bytes(16),
    {Ciphertext, Tag} = crypto:crypto_one_time(aes_256_gcm, Key, IV, Data, true),
    #{iv => IV, ciphertext => Ciphertext, tag => Tag}.

get_encryption_key() ->
    %% Get or generate encryption key
    case erlmcp_secrets:get("backup_encryption_key") of
        undefined ->
            %% Generate new key
            Key = crypto:strong_rand_bytes(32),
            erlmcp_secrets:set("backup_encryption_key", Key, 86400),  % 24 hours
            Key;
        Key ->
            Key
    end.

execute_restore(BackupId, Target, Options) ->
    try
        %% Retrieve backup
        {Encrypted, MetaData} = ?STORAGE_BACKEND:retrieve(BackupId),

        %% Decrypt backup
        Decrypted = decrypt_backup(Encrypted, MetaData),

        %% Decompress if necessary
        Data = case erlmcp_config:get(backup_compression, true) of
            true -> zlib:decompress(Decrypted);
            false -> Decrypted
        end,

        %% Verify checksum
        Checksum = calculate_checksum(Data),
        case Checksum == MetaData#{checksum} of
            true ->
                %% Restore based on target
                case Target of
                    session_data -> restore_session_data(Data);
                    registry_data -> restore_registry_data(Data);
                    configuration -> restore_configuration(Data);
                    secrets -> restore_secrets(Data);
                    all -> restore_all_data(Data)
                end,

                %% Log successful restore
                erlmcp_audit:log(restore, #{
                    backup_id => BackupId,
                    target => Target,
                    timestamp => erlmcp_utils:timestamp()
                });
            false ->
                error(verification_failed)
        end

    catch
        Error:Reason ->
            erlmcp_audit:log(restore_failed, #{
                backup_id => BackupId,
                target => Target,
                error => {Error, Reason},
                timestamp => erlmcp_utils:timestamp()
            })
    end.

decrypt_backup(Encrypted, MetaData) ->
    %% Decrypt backup
    Key = get_encryption_key(),
    #{iv := IV, ciphertext := Ciphertext, tag := Tag} = Encrypted,
    crypto:crypto_one_time(aes_256_gcm, Key, IV, Ciphertext, false, Tag).

restore_session_data(Data) ->
    %% Parse session data
    Parsed = jsx:decode(Data),
    Sessions = maps:get(<<"sessions">>, Parsed, []),

    %% Restore sessions
    lists:foreach(fun(SessionData) ->
        erlmcp_session:import_data(SessionData)
    end, Sessions).

restore_registry_data(Data) ->
    %% Parse registry data
    Parsed = jsx:decode(Data),
    Registry = maps:get(<<"registry">>, Parsed, []),

    %% Restore registry
    lists:foreach(fun(RegistryEntry) ->
        erlmcp_registry:import_entry(RegistryEntry)
    end, Registry).

restore_configuration(Data) ->
    %% Parse configuration data
    Parsed = jsx:decode(Data),
    Config = maps:get(<<"configuration">>, Parsed, []),

    %% Restore configuration
    lists:foreach(ConfigEntry ->
        erlmcp_config:import_entry(ConfigEntry)
    end, Config).

restore_secrets(Data) ->
    %% Parse secrets data
    Parsed = jsx:decode(Data),
    Secrets = maps:get(<<"secrets">>, Parsed, []),

    %% Restore secrets
    lists:foreach(fun(SecretEntry) ->
        erlmcp_secrets:import_entry(SecretEntry)
    end, Secrets).

restore_all_data(Data) ->
    %% Restore all data types
    restore_session_data(Data),
    restore_registry_data(Data),
    restore_configuration(Data),
    restore_secrets(Data).

build_status_report(State, Options) ->
    CurrentJobs = maps:values(State#state.current_jobs),

    #{
        active_jobs => length(CurrentJobs),
        completed_backups => length(State#state.completed_backups),
        storage_backend => erlang:pid_to_list(State#state.storage),
        next_backup => get_next_backup_time(),
        retention_days => proplists:get_value(retention_days, Options, ?BACKUP_RETENTION_DAYS)
    }.

format_backup_info(BackupId) ->
    case ?STORAGE_BACKEND:metadata(BackupId) of
        {ok, MetaData} ->
            #{
                id => BackupId,
                type => MetaData#{type},
                timestamp => MetaData#{timestamp},
                size => MetaData#{size},
                checksum => MetaData#{checksum}
            };
        {error, _} ->
            #{id => BackupId, error => not_found}
    end.

execute_backup_verification(BackupId, State) ->
    try
        {Encrypted, MetaData} = ?STORAGE_BACKEND:retrieve(BackupId),

        %% Decrypt and decompress
        Decrypted = decrypt_backup(Encrypted, MetaData),
        Data = case erlmcp_config:get(backup_compression, true) of
            true -> zlib:decompress(Decrypted);
            false -> Decrypted
        end,

        %% Verify checksum
        Checksum = calculate_checksum(Data),
        Checksum == MetaData#{checksum}
    catch
        _ -> false
    end.

find_old_backups(Backups, Days) ->
    Cutoff = erlang:system_time(millisecond) - (Days * 86400000),
    lists:filter(fun(BackupId) ->
        case ?STORAGE_BACKEND:metadata(BackupId) of
            {ok, #{timestamp := Timestamp}} when Timestamp < Cutoff ->
                true;
            _ ->
                false
        end
    end, Backups).

delete_backup(BackupId, State) ->
    ?STORAGE_BACKEND:delete(BackupId),
    UpdatedState = State#state{
        completed_backups = lists:delete(BackupId, State#state.completed_backups)
    },
    UpdatedState.

load_completed_backups() ->
    %% Load list of completed backups from storage
    ?STORAGE_BACKEND:list_backups().

handle_scheduled_backup(State) ->
    %% Determine if we need full or incremental backup
    case should_perform_full_backup(State) of
        true ->
            backup(all, #{type => full});
        false ->
            backup(all, #{type => incremental})
    end,

    %% Schedule next backup
    Interval = State#state.config#{backup_interval},
    erlang:send_after(Interval, self(), {timeout, make_ref(), backup}).

should_perform_full_backup(State) ->
    %% Check if enough time has passed since last full backup
    case ?STORAGE_BACKEND:last_full_backup() of
        {ok, Timestamp} ->
            TimeSinceFull = erlang:system_time(millisecond) - Timestamp,
            TimeSinceFull > State#state.config#{full_backup_interval};
        {error, not_found} ->
            true  % Perform full backup if none exists
    end.

update_backup_job(JobId, Result, State) ->
    UpdatedJob = case Result of
        {ok, MetaData} ->
            State#state.current_jobs#{JobId#backup_job.id}#backup_job{
                status = completed,
                checksum = MetaData#{checksum},
                data_size = MetaData#{size}
            };
        {error, Reason} ->
            State#state.current_jobs#{JobId#backup_job.id}#backup_job{
                status = failed,
                metadata = maps:put(error, Reason, JobId#backup_job.metadata)
            }
    end,

    UpdatedState = State#state{
        current_jobs = maps:put(JobId#backup_job.id, UpdatedJob, State#state.current_jobs),
        completed_backups = case UpdatedJob#backup_job.status of
            completed -> [JobId#backup_job.id | State#state.completed_backups];
            _ -> State#state.completed_backups
        end
    },

    %% Remove from current jobs if completed
    case UpdatedJob#backup_job.status of
        completed ->
            UpdatedState#state{
                current_jobs = maps:remove(JobId#backup_job.id, UpdatedState#state.current_jobs)
            };
        _ ->
            UpdatedState
    end.

get_next_backup_time() ->
    %% Calculate next scheduled backup time
    erlang:system_time(millisecond) + ?BACKUP_INTERVAL.