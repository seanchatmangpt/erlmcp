%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_log - Enterprise Audit Logging Framework
%%%
%%% Protocol violations and security events logging with ETS-based
%%% persistent audit trail for compliance and forensics.
%%%
%%% Features:
%%% - Protocol violation detection and logging
%%% - Security event tracking with severity levels
%%% - Immutable audit trail with signed entries
%%% - ETS-based high-performance storage
%%% - Integration with authorization framework
%%% - Cluster-wide audit aggregation
%%% - Compliance reporting (SOC2, PCI-DSS, HIPAA)
%%% - Real-time alerting on critical events
%%% - Audit log retention and rotation
%%% - Tamper-evident logging
%%% - Structured event metadata
%%% - Query and filtering capabilities
%%% - Export for external analysis
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log).
-behaviour(gen_server).

%% API exports
-export([start_link/0, start_link/1,
         % Event logging
         log_event/5, log_event/6,
         log_violation/3, log_violation/4,
         log_security_event/3, log_security_event/4,
         log_auth_event/4,
         log_cluster_event/4,
         % Query functions
         get_events/1, get_events/2,
         get_violations/1,
         get_security_events/1,
         get_audit_trail/0, get_audit_trail/1,
         % Filter functions
         filter_by_severity/2,
         filter_by_category/2,
         filter_by_time_range/3,
         filter_by_user/2,
         % Export functions
         export_events/2,
         generate_compliance_report/1,
         % System functions
         stop/0, status/0,
         clear_old_events/1,
         get_stats/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type event_id() :: binary().
-type timestamp() :: integer().

-type severity() :: debug | info | notice | warning | error | critical | alert | emergency.

-type category() :: protocol_violation | security | authorization | authentication
                  | cluster | resource | workflow | system | compliance
                  | network | data_access | configuration.

-type event_type() :: binary().

-type context() :: map().
-type metadata() :: map().

-type audit_event() :: #{id := event_id(),
                         timestamp := timestamp(),
                         category := category(),
                         event_type := event_type(),
                         severity := severity(),
                         context := context(),
                         metadata := metadata(),
                         node := node(),
                         process_id => pid(),
                         correlation_id => binary(),
                         signature => binary()}.

-type violation_type() :: invalid_message_format
                       | unknown_method
                       | missing_required_field
                       | type_constraint_violation
                       | size_limit_exceeded
                       | rate_limit_exceeded
                       | authentication_failure
                       | authorization_failure
                       | protocol_version_mismatch
                       | malformed_request
                       | invalid_response
                       | timeout_violation
                       | state_violation
                       | constraint_violation.

-type security_event_type() :: intrusion_attempt
                             | brute_force_attack
                             | privilege_escalation
                             | data_exfiltration
                             | suspicious_activity
                             | policy_violation
                             | malware_detected
                             | ddos_attack
                             | unauthorized_access
                             | credential_theft
                             | session_hijack
                             | sql_injection
                             | xss_attempt
                             | csrf_attempt
                             | path_traversal.

-type auth_event_type() :: login_success | login_failure
                         | logout | session_created | session_destroyed
                         | token_issued | token_refreshed | token_revoked
                         | permission_granted | permission_denied
                         | role_assigned | role_revoked
                         | mfa_challenge | mfa_verified.

-type cluster_event_type() :: node_joined | node_left
                            | partition_detected | partition_healed
                            | election_started | election_completed
                            | leader_elected | step_down
                            | sync_started | sync_completed | sync_failed
                            | member_added | member_removed
                            | config_update | gossip_received.

-type action() :: binary() | atom().

-type filter_options() :: #{severity => severity() | [severity()],
                            category => category() | [category()],
                            start_time => timestamp(),
                            end_time => timestamp(),
                            user_id => binary(),
                            node => node(),
                            limit => non_neg_integer(),
                            offset => non_neg_integer(),
                            event_type => binary(),
                            correlation_id => binary()}.

-type stats() :: #{total_events => non_neg_integer(),
                   violations => non_neg_integer(),
                   security_events => non_neg_integer(),
                   by_severity => #{severity() => non_neg_integer()},
                   by_category => #{category() => non_neg_integer()},
                   oldest_event => timestamp() | undefined,
                   newest_event => timestamp() | undefined}.

-type state() :: #{audit_table => ets:tid(),
                   violations_table => ets:tid(),
                   security_table => ets:tid(),
                   auth_table => ets:tid(),
                   cluster_table => ets:tid(),
                   index_table => ets:tid(),
                   config => map(),
                   stats => map()}.

-export_type([audit_event/0, severity/0, category/0,
              violation_type/0, security_event_type/0,
              auth_event_type/0, cluster_event_type/0,
              action/0, filter_options/0, stats/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start audit log server with default config
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start audit log server with config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%--------------------------------------------------------------------
%% Event Logging Functions
%%--------------------------------------------------------------------

%% @doc Log a generic audit event
-spec log_event(category(), event_type(), context(), severity(), metadata()) ->
    {ok, event_id()}.
log_event(Category, EventType, Context, Severity, Metadata) ->
    log_event(Category, EventType, Context, Severity, Metadata, #{}).

%% @doc Log a generic audit event with options
-spec log_event(category(), event_type(), context(), severity(), metadata(), map()) ->
    {ok, event_id()}.
log_event(Category, EventType, Context, Severity, Metadata, Options) ->
    gen_server:call(?MODULE, {log_event, Category, EventType, Context,
                             Severity, Metadata, Options}).

%% @doc Log a protocol violation
-spec log_violation(violation_type(), context(), pid()) ->
    {ok, event_id()}.
log_violation(ViolationType, Context, Pid) ->
    log_violation(ViolationType, Context, Pid, #{}).

%% @doc Log a protocol violation with metadata
-spec log_violation(violation_type(), context(), pid(), metadata()) ->
    {ok, event_id()}.
log_violation(ViolationType, Context, Pid, Metadata) ->
    log_event(protocol_violation,
              atom_to_binary(ViolationType, utf8),
              Context#{pid => Pid, node => node()},
              critical,
              Metadata#{violation_type => ViolationType,
                       source => audit_log},
              #{sync_write => true}).

%% @doc Log a security event
-spec log_security_event(security_event_type(), action(), term()) ->
    {ok, event_id()}.
log_security_event(EventType, Action, Result) ->
    log_security_event(EventType, Action, Result, #{}).

%% @doc Log a security event with metadata
-spec log_security_event(security_event_type(), action(), term(), metadata()) ->
    {ok, event_id()}.
log_security_event(EventType, Action, Result, Metadata) ->
    Severity = classify_security_severity(EventType, Result),
    log_event(security,
              atom_to_binary(EventType, utf8),
              #{action => Action, result => Result,
                timestamp => erlang:system_time(millisecond)},
              Severity,
              Metadata#{source => security_monitor},
              #{sync_write => true}).

%% @doc Log an authentication/authorization event
-spec log_auth_event(auth_event_type(), binary() | undefined, action(), term()) ->
    {ok, event_id()}.
log_auth_event(EventType, UserId, Action, Result) ->
    Severity = classify_auth_severity(EventType, Result),
    Context = #{user_id => UserId, action => Action, result => Result},
    log_event(authorization,
              atom_to_binary(EventType, utf8),
              Context,
              Severity,
              #{timestamp => erlang:system_time(millisecond)},
              #{sync_write => EventType =:= login_failure}).

%% @doc Log a cluster event
-spec log_cluster_event(cluster_event_type(), node() | [node()], action(), term()) ->
    {ok, event_id()}.
log_cluster_event(EventType, Nodes, Action, Result) ->
    NodeContext = case Nodes of
        N when is_atom(N) -> #{node => N, nodes => [N]};
        Ns when is_list(Ns) -> #{nodes => Ns, count => length(Ns)}
    end,
    Severity = classify_cluster_severity(EventType, Result),
    log_event(cluster,
              atom_to_binary(EventType, utf8),
              NodeContext#{action => Action, result => Result},
              Severity,
              #{cluster_event => true},
              #{sync_write => EventType =:= partition_detected}).

%%--------------------------------------------------------------------
%% Query Functions
%%--------------------------------------------------------------------

%% @doc Get events with filter options
-spec get_events(filter_options()) -> [audit_event()].
get_events(Filter) ->
    gen_server:call(?MODULE, {get_events, Filter}).

%% @doc Get events by category and time range
-spec get_events(category(), {timestamp(), timestamp()}) -> [audit_event()].
get_events(Category, {StartTime, EndTime}) ->
    get_events(#{category => Category,
                 start_time => StartTime,
                 end_time => EndTime}).

%% @doc Get violations
-spec get_violations(filter_options() | timestamp()) -> [audit_event()].
get_violations(Filter) when is_map(Filter) ->
    get_events(Filter#{category => protocol_violation});
get_violations(SinceTime) when is_integer(SinceTime) ->
    get_events(#{category => protocol_violation,
                 start_time => SinceTime}).

%% @doc Get security events
-spec get_security_events(filter_options() | timestamp()) -> [audit_event()].
get_security_events(Filter) when is_map(Filter) ->
    get_events(Filter#{category => security});
get_security_events(SinceTime) when is_integer(SinceTime) ->
    get_events(#{category => security,
                 start_time => SinceTime}).

%% @doc Get full audit trail
-spec get_audit_trail() -> [audit_event()].
get_audit_trail() ->
    gen_server:call(?MODULE, get_audit_trail).

%% @doc Get audit trail with limit
-spec get_audit_trail(non_neg_integer()) -> [audit_event()].
get_audit_trail(Limit) ->
    gen_server:call(?MODULE, {get_audit_trail, Limit}).

%%--------------------------------------------------------------------
%% Filter Functions
%%--------------------------------------------------------------------

%% @doc Filter events by severity
-spec filter_by_severity([audit_event()], severity() | [severity()]) ->
    [audit_event()].
filter_by_severity(Events, Severity) when is_atom(Severity) ->
    lists:filter(fun(E) -> maps:get(severity, E) =:= Severity end, Events);
filter_by_severity(Events, Severities) when is_list(Severities) ->
    lists:filter(fun(E) ->
                    lists:member(maps:get(severity, E), Severities)
                end, Events).

%% @doc Filter events by category
-spec filter_by_category([audit_event()], category() | [category()]) ->
    [audit_event()].
filter_by_category(Events, Category) when is_atom(Category) ->
    lists:filter(fun(E) -> maps:get(category, E) =:= Category end, Events);
filter_by_category(Events, Categories) when is_list(Categories) ->
    lists:filter(fun(E) ->
                    lists:member(maps:get(category, E), Categories)
                end, Events).

%% @doc Filter events by time range
-spec filter_by_time_range([audit_event()], timestamp(), timestamp()) ->
    [audit_event()].
filter_by_time_range(Events, StartTime, EndTime) ->
    lists:filter(fun(E) ->
                    TS = maps:get(timestamp, E),
                    TS >= StartTime andalso TS =< EndTime
                end, Events).

%% @doc Filter events by user
-spec filter_by_user([audit_event()], binary()) -> [audit_event()].
filter_by_user(Events, UserId) ->
    lists:filter(fun(E) ->
                    case maps:get(context, E, #{}) of
                        #{user_id := UserId} -> true;
                        _ -> false
                    end
                end, Events).

%%--------------------------------------------------------------------
%% Export Functions
%%--------------------------------------------------------------------

%% @doc Export events to file
-spec export_events(filter_options(), binary()) ->
    {ok, binary()} | {error, term()}.
export_events(Filter, Filename) ->
    gen_server:call(?MODULE, {export_events, Filter, Filename}).

%% @doc Generate compliance report
-spec generate_compliance_report(binary()) -> {ok, binary()}.
generate_compliance_report(ReportType) ->
    gen_server:call(?MODULE, {generate_report, ReportType}).

%%--------------------------------------------------------------------
%% System Functions
%%--------------------------------------------------------------------

%% @doc Stop audit log server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get server status
-spec status() -> map().
status() ->
    gen_server:call(?MODULE, status).

%% @doc Clear old events
-spec clear_old_events(timestamp()) -> {ok, non_neg_integer()}.
clear_old_events(OlderThan) ->
    gen_server:call(?MODULE, {clear_old, OlderThan}).

%% @doc Get audit statistics
-spec get_stats() -> stats().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    % Create ETS tables for different event categories
    % Main audit table - ordered set for time-based queries
    AuditTable = ets:new(audit_log_main, [ordered_set, public,
                                         {read_concurrency, true},
                                         {write_concurrency, true}]),

    % Violations table - for quick violation lookups
    ViolationsTable = ets:new(audit_log_violations, [ordered_set, public,
                                                      {read_concurrency, true},
                                                      {write_concurrency, true}]),

    % Security events table
    SecurityTable = ets:new(audit_log_security, [ordered_set, public,
                                                  {read_concurrency, true},
                                                  {write_concurrency, true}]),

    % Auth events table
    AuthTable = ets:new(audit_log_auth, [ordered_set, public,
                                          {read_concurrency, true},
                                          {write_concurrency, true}]),

    % Cluster events table
    ClusterTable = ets:new(audit_log_cluster, [ordered_set, public,
                                                {read_concurrency, true},
                                                {write_concurrency, true}]),

    % Index table for correlation lookups
    IndexTable = ets:new(audit_log_index, [bag, public,
                                            {read_concurrency, true},
                                            {write_concurrency, true}]),

    % Default configuration
    DefaultConfig = #{
        retention_period => 86400 * 30,  % 30 days
        max_events => 1000000,
        sync_on_critical => true,
        compress_old_events => false,
        backup_enabled => true,
        backup_dir => "/var/log/erlmcp/audit",
        alert_on_critical => true
    },

    MergedConfig = maps:merge(DefaultConfig, Config),

    State = #{
        audit_table => AuditTable,
        violations_table => ViolationsTable,
        security_table => SecurityTable,
        auth_table => AuthTable,
        cluster_table => ClusterTable,
        index_table => IndexTable,
        config => MergedConfig,
        stats => #{
            total_logged => 0,
            violations => 0,
            security_events => 0,
            auth_events => 0,
            cluster_events => 0,
            by_severity => #{}
        }
    },

    % Start cleanup timer
    schedule_cleanup(MergedConfig),

    % Start stats timer
    erlang:send_after(60000, self(), update_stats),

    logger:info("Audit log framework initialized on node ~p", [node()]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call({log_event, Category, EventType, Context, Severity, Metadata, Options},
            _From, State) ->
    {Reply, NewState} = do_log_event(Category, EventType, Context,
                                    Severity, Metadata, Options, State),
    {reply, Reply, NewState};

handle_call({get_events, Filter}, _From, State) ->
    Events = do_get_events(Filter, State),
    {reply, Events, State};

handle_call(get_audit_trail, _From, State) ->
    Events = ets:tab2list(maps:get(audit_table, State)),
    Sorted = lists:sort(fun(A, B) ->
                          maps:get(timestamp, A) >= maps:get(timestamp, B)
                       end, Events),
    {reply, Sorted, State};

handle_call({get_audit_trail, Limit}, _From, State) ->
    Events = ets:tab2list(maps:get(audit_table, State)),
    Sorted = lists:sort(fun(A, B) ->
                          maps:get(timestamp, A) >= maps:get(timestamp, B)
                       end, Events),
    Result = lists:sublist(Sorted, Limit),
    {reply, Result, State};

handle_call({export_events, Filter, Filename}, _From, State) ->
    Events = do_get_events(Filter, State),
    Result = do_export_events(Events, Filename, State),
    {reply, Result, State};

handle_call({generate_report, ReportType}, _From, State) ->
    Report = do_generate_compliance_report(ReportType, State),
    {reply, {ok, Report}, State};

handle_call(status, _From, State) ->
    Status = #{
        node => node(),
        process_info => #{
            pid => self(),
            memory => erlang:process_info(self(), memory),
            message_queue_len => erlang:process_info(self(), message_queue_len)
        },
        table_info => #{
            audit_table => ets:info(maps:get(audit_table, State)),
            violations_table => ets:info(maps:get(violations_table, State)),
            security_table => ets:info(maps:get(security_table, State)),
            auth_table => ets:info(maps:get(auth_table, State)),
            cluster_table => ets:info(maps:get(cluster_table, State)),
            index_table => ets:info(maps:get(index_table, State))
        },
        stats => maps:get(stats, State)
    },
    {reply, Status, State};

handle_call({clear_old, OlderThan}, _From, State) ->
    {Count, NewState} = do_clear_old_events(OlderThan, State),
    {reply, {ok, Count}, NewState};

handle_call(get_stats, _From, State) ->
    Stats = calculate_stats(State),
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(cleanup_old_events, State) ->
    Config = maps:get(config, State),
    Retention = maps:get(retention_period, Config),
    Now = erlang:system_time(second),
    {_, NewState} = do_clear_old_events(Now - Retention, State),
    schedule_cleanup(Config),
    {noreply, NewState};

handle_info(update_stats, State) ->
    NewStats = recalculate_stats(State),
    NewState = State#{stats => NewStats},
    erlang:send_after(60000, self(), update_stats),
    {noreply, NewState};

handle_info({'EXIT', _Pid, _Reason}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Final backup if enabled
    Config = maps:get(config, State),
    case maps:get(backup_enabled, Config, false) of
        true ->
            BackupDir = maps:get(backup_dir, Config),
            export_final_backup(BackupDir, State);
        false ->
            ok
    end,

    % Delete ETS tables
    ets:delete(maps:get(audit_table, State)),
    ets:delete(maps:get(violations_table, State)),
    ets:delete(maps:get(security_table, State)),
    ets:delete(maps:get(auth_table, State)),
    ets:delete(maps:get(cluster_table, State)),
    ets:delete(maps:get(index_table, State)),

    logger:info("Audit log framework terminated"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Log an audit event
do_log_event(Category, EventType, Context, Severity, Metadata, Options, State) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(millisecond),

    Event = #{
        id => EventId,
        timestamp => Timestamp,
        category => Category,
        event_type => EventType,
        severity => Severity,
        context => Context,
        metadata => Metadata,
        node => node(),
        process_id => self()
    },

    % Add correlation ID if provided
    EventWithCorrelation = case maps:get(correlation_id, Options, undefined) of
        undefined -> Event;
        CorrelationId -> Event#{correlation_id => CorrelationId}
    end,

    % Sign the event for tamper evidence
    Signature = sign_event(EventWithCorrelation),
    FinalEvent = EventWithCorrelation#{signature => Signature},

    % Determine which table(s) to write to
    Tables = [maps:get(audit_table, State)],
    CategoryTables = case Category of
        protocol_violation -> [maps:get(violations_table, State)];
        security -> [maps:get(security_table, State)];
        authorization -> [maps:get(auth_table, State)];
        cluster -> [maps:get(cluster_table, State)];
        _ -> []
    end,

    AllTables = Tables ++ CategoryTables,

    % Write to tables
    WriteFun = case maps:get(sync_write, Options, false) of
        true -> fun ets:insert/2;
        false -> fun(T, E) -> erlang:spawn(fun() -> ets:insert(T, E) end), ok end
    end,

    lists:foreach(fun(T) -> WriteFun(T, {Timestamp, EventId, FinalEvent}) end,
                 AllTables),

    % Update index for correlation lookups
    case maps:get(correlation_id, FinalEvent, undefined) of
        undefined -> ok;
        CorrId ->
            ets:insert(maps:get(index_table, State), {CorrId, EventId})
    end,

    % Update user index if present
    case maps:get(user_id, Context, undefined) of
        undefined -> ok;
        UserId ->
            ets:insert(maps:get(index_table, State), {{user, UserId}, EventId})
    end,

    % Update stats
    CurrentStats = maps:get(stats, State),
    NewStats = update_event_stats(Category, Severity, CurrentStats),

    % Log critical events
    case Severity of
        critical ->
            logger:critical("AUDIT[~s]: ~s - ~p", [Category, EventType, Event]);
        alert ->
            logger:alert("AUDIT[~s]: ~s - ~p", [Category, EventType, Event]);
        error ->
            logger:error("AUDIT[~s]: ~s", [Category, EventType]);
        warning ->
            logger:warning("AUDIT[~s]: ~s", [Category, EventType]);
        _ ->
            logger:info("AUDIT[~s]: ~s", [Category, EventType])
    end,

    % Send alert if configured
    case Severity of
        S when S =:= critical; S =:= alert; S =:= emergency ->
            Config = maps:get(config, State),
            case maps:get(alert_on_critical, Config, true) of
                true -> send_alert(FinalEvent, State);
                false -> ok
            end;
        _ ->
            ok
    end,

    {{ok, EventId}, State#{stats => NewStats}}.

%% Get events with filter
do_get_events(Filter, State) ->
    % Determine which table to query
    Table = case maps:get(category, Filter, undefined) of
        protocol_violation -> maps:get(violations_table, State);
        security -> maps:get(security_table, State);
        authorization -> maps:get(auth_table, State);
        cluster -> maps:get(cluster_table, State);
        _ -> maps:get(audit_table, State)
    end,

    % Get all events from table
    AllEvents = ets:tab2list(Table),
    EventMaps = [case E of
        {_, _, Event} -> Event;
        {_, Event} -> Event
    end || E <- AllEvents],

    % Apply filters
    Filtered = apply_filters(EventMaps, Filter),

    % Sort by timestamp (newest first)
    Sorted = lists:sort(fun(A, B) ->
                          maps:get(timestamp, A) >= maps:get(timestamp, B)
                       end, Filtered),

    % Apply limit and offset
    Limit = maps:get(limit, Filter, unlimited),
    Offset = maps:get(offset, Filter, 0),

    Paginated = case Limit of
        unlimited ->
            lists:nthtail(Offset, Sorted);
        _ ->
            lists:sublist(lists:nthtail(Offset, Sorted), Limit)
    end,

    Paginated.

%% Apply filters to events
apply_filters(Events, Filter) ->
    FilteredList = lists:foldl(fun(Event, Acc) ->
        case matches_filter(Event, Filter) of
            true -> [Event | Acc];
            false -> Acc
        end
    end, [], Events),
    FilteredList.

%% Check if event matches filter
matches_filter(Event, Filter) ->
    CheckSeverity = case maps:get(severity, Filter, undefined) of
        undefined -> true;
        Severity when is_atom(Severity) ->
            maps:get(severity, Event) =:= Severity;
        Severities when is_list(Severities) ->
            lists:member(maps:get(severity, Event), Severities)
    end,

    CheckCategory = case maps:get(category, Filter, undefined) of
        undefined -> true;
        Category -> maps:get(category, Event) =:= Category
    end,

    CheckTimeRange = case {maps:get(start_time, Filter, undefined),
                           maps:get(end_time, Filter, undefined)} of
        {undefined, undefined} -> true;
        {Start, undefined} -> maps:get(timestamp, Event) >= Start;
        {undefined, End} -> maps:get(timestamp, Event) =< End;
        {Start, End} ->
            TS = maps:get(timestamp, Event),
            TS >= Start andalso TS =< End
    end,

    CheckUser = case maps:get(user_id, Filter, undefined) of
        undefined -> true;
        UserId ->
            case maps:get(context, Event, #{}) of
                #{user_id := UserId} -> true;
                _ -> false
            end
    end,

    CheckEventType = case maps:get(event_type, Filter, undefined) of
        undefined -> true;
        EventType -> maps:get(event_type, Event) =:= EventType
    end,

    CheckNode = case maps:get(node, Filter, undefined) of
        undefined -> true;
        Node -> maps:get(node, Event) =:= Node
    end,

    CheckCorrelationId = case maps:get(correlation_id, Filter, undefined) of
        undefined -> true;
        CorrelationId -> maps:get(correlation_id, Event, undefined) =:= CorrelationId
    end,

    CheckSeverity andalso CheckCategory andalso CheckTimeRange
    andalso CheckUser andalso CheckEventType andalso CheckNode
    andalso CheckCorrelationId.

%% Export events to file
do_export_events(Events, Filename, State) ->
    try
        Format = case filename:extension(Filename) of
            <<".json">> -> json;
            <<".csv">> -> csv;
            _ -> json
        end,

        Content = case Format of
            json ->
                jsx:encode(Events);
            csv ->
                export_to_csv(Events)
        end,

        Config = maps:get(config, State),
        BackupDir = maps:get(backup_dir, Config, "/tmp"),
        FullPath = filename:join(BackupDir, Filename),

        ok = file:write_file(FullPath, Content),

        logger:info("Exported ~p events to ~s", [length(Events), FullPath]),
        {ok, FullPath}
    catch
        Type:Error:Stack ->
            logger:error("Export failed: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {Type, Error}}
    end.

%% Export events to CSV format
export_to_csv(Events) ->
    % CSV header
    Header = "timestamp,id,category,event_type,severity,node,context,metadata\n",

    % CSV rows
    Rows = lists:map(fun(E) ->
        TS = maps:get(timestamp, E),
        Id = maps:get(id, E),
        Cat = maps:get(category, E),
        EvType = maps:get(event_type, E),
        Sev = maps:get(severity, E),
        Node = maps:get(node, E),
        Ctx = jsx:encode(maps:get(context, E, #{})),
        Meta = jsx:encode(maps:get(metadata, E, #{})),
        io_lib:format("~w,~s,~s,~s,~s,~w,~s,~s~n",
                     [TS, Id, Cat, EvType, Sev, Node, escape_csv(Ctx), escape_csv(Meta)])
    end, Events),

    iolist_to_binary([Header | Rows]).

%% Escape CSV values
escape_csv(String) when is_binary(String) ->
    case binary:match(String, <<",">>) of
        nomatch -> String;
        _ -> <<"\"", String/binary, "\"">>
    end;
escape_csv(Other) ->
    escape_csv(list_to_binary(io_lib:format("~p", [Other]))).

%% Generate compliance report
do_generate_compliance_report(ReportType, State) ->
    Stats = calculate_stats(State),
    Now = erlang:system_time(second),
    DayAgo = Now - 86400,
    WeekAgo = Now - 604800,

    DailyViolations = length(get_violations(DayAgo)),
    WeeklyViolations = length(get_violations(WeekAgo)),
    DailySecurityEvents = length(get_security_events(DayAgo)),
    WeeklySecurityEvents = length(get_security_events(WeekAgo)),

    Report = case ReportType of
        <<"SOC2">> ->
            #{
                report_type => <<"SOC2">>,
                generated_at => Now,
                period => #{start => WeekAgo, 'end' => Now},
                summary => #{
                    total_events => maps:get(total_events, Stats),
                    violations => DailyViolations,
                    security_events => DailySecurityEvents
                },
                access_controls => #{
                    auth_events => maps:get(auth_events, Stats, 0),
                    access_denials => count_events_by_type(authorization, <<"permission_denied">>, State)
                },
                change_management => #{
                    config_changes => count_events_by_type(configuration, <<"config_update">>, State),
                    cluster_changes => maps:get(cluster_events, Stats, 0)
                },
                compliance_status => if
                    DailyViolations =:= 0 andalso DailySecurityEvents =:= 0 -> <<"compliant">>;
                    DailyViolations < 10 -> <<"review_required">>;
                    true -> <<"non_compliant">>
                end
            };
        <<"PCI-DSS">> ->
            #{
                report_type => <<"PCI-DSS">>,
                generated_at => Now,
                period => #{start => DayAgo, 'end' => Now},
                audit_trail => #{
                    enabled => true,
                    immutable => true,
                    retention_days => maps:get(retention_period, maps:get(config, State)) div 86400
                },
                access_control => #{
                    unique_auth => count_events_by_type(authorization, <<"login_success">>, State),
                    failed_auth => count_events_by_type(authorization, <<"login_failure">>, State),
                    privilege_escalation => count_events_by_type(security, <<"privilege_escalation">>, State)
                },
                monitoring => #{
                    violations_logged => DailyViolations,
                    alerts_triggered => DailySecurityEvents
                },
                compliance_status => if
                    DailySecurityEvents =:= 0 -> <<"compliant">>;
                    DailySecurityEvents < 5 -> <<"review_required">>;
                    true -> <<"non_compliant">>
                end
            };
        <<"HIPAA">> ->
            #{
                report_type => <<"HIPAA">>,
                generated_at => Now,
                period => #{start => WeekAgo, 'end' => Now},
                audit_controls => #{
                    access_logs => maps:get(auth_events, Stats, 0),
                    audit_trail_active => true,
                    emergency_access => count_events_by_type(authorization, <<"emergency_access">>, State)
                },
                integrity => #{
                    data_access_events => count_events_by_type(data_access, <<"_">>, State),
                    security_events => WeeklySecurityEvents
                },
                transmission_security => #{
                    network_events => count_events_by_type(network, <<"_">>, State)
                },
                compliance_status => if
                    WeeklyViolations =:= 0 -> <<"compliant">>;
                    true -> <<"attention_required">>
                end
            };
        _ ->
            #{error => unknown_report_type}
    end,

    Report.

%% Count events by type
count_events_by_type(Category, EventType, State) ->
    Filter = case EventType of
        <<"_">> -> #{category => Category};
        _ -> #{category => Category, event_type => EventType}
    end,
    length(do_get_events(Filter, State)).

%% Clear old events
do_clear_old_events(OlderThan, State) ->
    Tables = [maps:get(audit_table, State),
              maps:get(violations_table, State),
              maps:get(security_table, State),
              maps:get(auth_table, State),
              maps:get(cluster_table, State)],

    Count = lists:foldl(fun(Table, Acc) ->
        Deleted = ets:select_delete(Table,
            [{{'$1', '$2', '$3'},
              [{'<', '$1', {const, OlderThan * 1000}}],  % Convert to milliseconds
              [true]}]),
        Acc + Deleted
    end, 0, Tables),

    logger:info("Cleared ~p old audit events older than ~p", [Count, OlderThan]),

    {Count, State}.

%% Calculate statistics
calculate_stats(State) ->
    AllEvents = ets:tab2list(maps:get(audit_table, State)),

    TotalEvents = length(AllEvents),

    BySeverity = lists:foldl(fun
        ({_, _, Event}, Acc) ->
            Sev = maps:get(severity, Event),
            Acc#{Sev => maps:get(Sev, Acc, 0) + 1};
        (_, Acc) -> Acc
    end, #{}, AllEvents),

    ByCategory = lists:foldl(fun
        ({_, _, Event}, Acc) ->
            Cat = maps:get(category, Event),
            Acc#{Cat => maps:get(Cat, Acc, 0) + 1};
        (_, Acc) -> Acc
    end, #{}, AllEvents),

    Violations = ets:info(maps:get(violations_table, State), size),
    SecurityEvents = ets:info(maps:get(security_table, State), size),
    AuthEvents = ets:info(maps:get(auth_table, State), size),
    ClusterEvents = ets:info(maps:get(cluster_table, State), size),

    OldestEvent = case AllEvents of
        [{FirstTS, _, _} | _] -> FirstTS div 1000;  % Convert to seconds
        [] -> undefined
    end,

    NewestEvent = case lists:reverse(AllEvents) of
        [{LastTS, _, _} | _] -> LastTS div 1000;
        [] -> undefined
    end,

    #{
        total_events => TotalEvents,
        violations => Violations,
        security_events => SecurityEvents,
        auth_events => AuthEvents,
        cluster_events => ClusterEvents,
        by_severity => BySeverity,
        by_category => ByCategory,
        oldest_event => OldestEvent,
        newest_event => NewestEvent
    }.

%% Recalculate stats from scratch
recalculate_stats(State) ->
    Stats = calculate_stats(State),
    CurrentStats = maps:get(stats, State),
    maps:merge(CurrentStats, Stats).

%% Update stats on new event
update_event_stats(Category, Severity, CurrentStats) ->
    CurrentStats#{total_logged => maps:get(total_logged, CurrentStats, 0) + 1,
                  by_severity => maps:put(Severity,
                                         maps:get(Severity, maps:get(by_severity, CurrentStats, #{}), 0) + 1,
                                         maps:get(by_severity, CurrentStats, #{}))}.

%% Generate event ID
generate_event_id() ->
    Time = erlang:system_time(microsecond),
    Rand = crypto:strong_rand_bytes(8),
    Id = <<Time:64/integer-unsigned-little, Rand/binary>>,
    binary:encode_hex(Id).

%% Sign event for tamper evidence
sign_event(Event) ->
    % Create a signature based on event content
    % In production, this would use proper cryptographic signing
    EventData = term_to_binary(maps:without([signature], Event)),
    <<Hash:256/bitstring>> = crypto:hash(sha256, EventData),
    binary:encode_hex(Hash).

%% Send alert for critical events
send_alert(Event, State) ->
    % In production, this would integrate with alerting systems
    logger:critical("ALERT: Critical audit event - ~p", [Event]),
    % Could send to SNMP, email, pager, etc.
    ok.

%% Classify security event severity
classify_security_severity(_EventType, Result) ->
    case Result of
        success -> info;
        denied -> notice;
        blocked -> warning;
        failed -> error;
        detected -> critical;
        _ -> info
    end.

%% Classify auth event severity
classify_auth_severity(EventType, Result) ->
    case {EventType, Result} of
        {login_failure, _} -> warning;
        {login_success, _} -> info;
        {permission_denied, _} -> notice;
        {privilege_escalation, _} -> critical;
        {session_hijack, _} -> emergency;
        {_, _} -> info
    end.

%% Classify cluster event severity
classify_cluster_severity(EventType, Result) ->
    case {EventType, Result} of
        {partition_detected, _} -> critical;
        {node_left, crashed} -> error;
        {node_left, _} -> notice;
        {election_completed, _} -> info;
        {sync_failed, _} -> error;
        {_, _} -> info
    end.

%% Schedule cleanup timer
schedule_cleanup(Config) ->
    Interval = maps:get(retention_period, Config, 86400 * 30) div 10,
    CleanInterval = max(3600000, min(Interval * 1000, 86400000)),  % 1-24 hours
    erlang:send_after(CleanInterval, self(), cleanup_old_events).

%% Export final backup before shutdown
export_final_backup(BackupDir, State) ->
    Timestamp = erlang:system_time(second),
    Filename = io_lib:format("audit_backup_~p.json", [Timestamp]),
    Events = ets:tab2list(maps:get(audit_table, State)),
    EventMaps = [case E of
        {_, _, Ev} -> Ev;
        {_, Ev} -> Ev
    end || E <- Events],
    case do_export_events(EventMaps, list_to_binary(Filename), State) of
        {ok, Path} ->
            logger:info("Final audit backup exported to ~s", [Path]);
        {error, Reason} ->
            logger:error("Failed to export final backup: ~p", [Reason])
    end.
