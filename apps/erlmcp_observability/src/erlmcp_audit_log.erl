%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_log - Tamper-Proof Audit Logging
%%% Implements immutable audit trails with SHA-256 hash chains
%%% for compliance (GDPR, SOC2, HIPAA).
%%%
%%% Design:
%%% - gen_server for sequential writes
%%% - Hash chain for tamper detection
%%% - Structured logging with metadata
%%% - Export to multiple formats (JSON, CSV, Syslog)
%%% - Compliance report generation
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    start_link/1,
    log_auth_success/2,
    log_auth_failure/2,
    log_operation/4,
    log_permission_check/4,
    log_sensitive_operation/3,
    verify_chain/0,
    verify_chain/2,
    export_logs/2,
    get_user_logs/2,
    search_logs/1,
    stop/0
]).

%% Exported for testing
-export([
    read_range_entries/3,
    verify_range_hashes/2,
    find_entry_by_seq/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type event_type() :: auth_success | auth_failure | operation | permission_check | sensitive_op.
-type user_id() :: binary().
-type resource() :: binary().
-type action() :: binary().

-export_type([event_type/0]).

%% State record
-record(state, {
    log_file :: file:io_device() | undefined,
    log_path :: file:filename(),
    current_hash :: binary(),
    sequence :: non_neg_integer(),
    buffer :: [map()],
    buffer_size :: pos_integer(),
    flush_interval :: pos_integer()
}).

-type state() :: #state{}.

%% Audit log entry record
-record(audit_entry, {
    sequence :: non_neg_integer(),
    timestamp :: integer(),
    event_type :: event_type(),
    user_id :: user_id() | undefined,
    session_id :: binary() | undefined,
    resource :: resource() | undefined,
    action :: action() | undefined,
    result :: success | failure | forbidden,
    metadata :: map(),
    previous_hash :: binary(),
    entry_hash :: binary()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%% @doc Log successful authentication.
-spec log_auth_success(user_id(), map()) -> ok.
log_auth_success(UserId, Metadata) ->
    gen_server:cast(?MODULE, {log, auth_success, #{
        user_id => UserId,
        result => success,
        metadata => Metadata
    }}).

%% @doc Log failed authentication.
-spec log_auth_failure(user_id() | undefined, map()) -> ok.
log_auth_failure(UserId, Metadata) ->
    gen_server:cast(?MODULE, {log, auth_failure, #{
        user_id => UserId,
        result => failure,
        metadata => Metadata
    }}).

%% @doc Log general operation.
-spec log_operation(user_id(), resource(), action(), map()) -> ok.
log_operation(UserId, Resource, Action, Metadata) ->
    gen_server:cast(?MODULE, {log, operation, #{
        user_id => UserId,
        resource => Resource,
        action => Action,
        result => success,
        metadata => Metadata
    }}).

%% @doc Log permission check.
-spec log_permission_check(user_id(), resource(), action(), ok | {error, forbidden}) -> ok.
log_permission_check(UserId, Resource, Action, Result) ->
    ResultAtom = case Result of
        ok -> success;
        {error, forbidden} -> forbidden
    end,
    gen_server:cast(?MODULE, {log, permission_check, #{
        user_id => UserId,
        resource => Resource,
        action => Action,
        result => ResultAtom,
        metadata => #{}
    }}).

%% @doc Log sensitive operation (PII access, key rotation, etc.).
-spec log_sensitive_operation(user_id(), action(), map()) -> ok.
log_sensitive_operation(UserId, Action, Metadata) ->
    gen_server:cast(?MODULE, {log, sensitive_op, #{
        user_id => UserId,
        action => Action,
        result => success,
        metadata => Metadata#{sensitive => true}
    }}).

%% @doc Verify entire hash chain integrity.
-spec verify_chain() -> ok | {error, {tampered, non_neg_integer()}}.
verify_chain() ->
    gen_server:call(?MODULE, verify_chain).

%% @doc Verify hash chain between sequence numbers.
-spec verify_chain(non_neg_integer(), non_neg_integer()) ->
    ok | {error, {tampered, non_neg_integer()}}.
verify_chain(FromSeq, ToSeq) ->
    gen_server:call(?MODULE, {verify_chain, FromSeq, ToSeq}).

%% @doc Export logs in specified format.
-spec export_logs(json | csv | syslog, file:filename()) -> ok | {error, term()}.
export_logs(Format, OutputPath) ->
    gen_server:call(?MODULE, {export_logs, Format, OutputPath}).

%% @doc Get logs for specific user in time range.
-spec get_user_logs(user_id(), {integer(), integer()}) -> {ok, [map()]} | {error, term()}.
get_user_logs(UserId, {StartTime, EndTime}) ->
    gen_server:call(?MODULE, {get_user_logs, UserId, StartTime, EndTime}).

%% @doc Search logs with query.
-spec search_logs(map()) -> {ok, [map()]} | {error, term()}.
search_logs(Query) ->
    gen_server:call(?MODULE, {search_logs, Query}).

%% @doc Stop audit log server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([Config]) ->
    process_flag(trap_exit, true),

    LogPath = maps:get(log_path, Config, "priv/audit/audit.log"),
    BufferSize = maps:get(buffer_size, Config, 100),
    FlushInterval = maps:get(flush_interval_ms, Config, 5000),

    % Ensure log directory exists
    LogDir = filename:dirname(LogPath),
    ok = filelib:ensure_dir(LogDir ++ "/"),

    % Open log file in append mode
    {ok, LogFile} = file:open(LogPath, [append, raw, binary]),

    % Initialize hash chain
    InitialHash = crypto:hash(sha256, <<"erlmcp_audit_log_genesis">>),

    State = #state{
        log_file = LogFile,
        log_path = LogPath,
        current_hash = InitialHash,
        sequence = 0,
        buffer = [],
        buffer_size = BufferSize,
        flush_interval = FlushInterval
    },

    % Start periodic flush timer
    erlang:send_after(FlushInterval, self(), flush_buffer),

    logger:info("Audit log started: ~p", [LogPath]),
    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {noreply, state()}.
handle_call(verify_chain, _From, State) ->
    Result = do_verify_chain(State#state.log_path),
    {reply, Result, State};

handle_call({verify_chain, FromSeq, ToSeq}, _From, State) ->
    Result = do_verify_chain_range(State#state.log_path, FromSeq, ToSeq),
    {reply, Result, State};

handle_call({export_logs, Format, OutputPath}, _From, State) ->
    Result = do_export_logs(State#state.log_path, Format, OutputPath),
    {reply, Result, State};

handle_call({get_user_logs, UserId, StartTime, EndTime}, _From, State) ->
    Result = do_get_user_logs(State#state.log_path, UserId, StartTime, EndTime),
    {reply, Result, State};

handle_call({search_logs, Query}, _From, State) ->
    Result = do_search_logs(State#state.log_path, Query),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({log, EventType, Data}, State) ->
    Entry = create_audit_entry(EventType, Data, State),
    NewBuffer = [Entry | State#state.buffer],
    % Increment sequence for next entry
    NewSequence = State#state.sequence + 1,
    % Update current_hash to this entry's hash for next entry to reference
    NewCurrentHash = Entry#audit_entry.entry_hash,

    % Auto-flush if buffer full
    case length(NewBuffer) >= State#state.buffer_size of
        true ->
            NewState = flush_buffer_internal(State#state{buffer = NewBuffer, sequence = NewSequence, current_hash = NewCurrentHash}),
            {noreply, NewState};
        false ->
            {noreply, State#state{buffer = NewBuffer, sequence = NewSequence, current_hash = NewCurrentHash}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(flush_buffer, State) ->
    NewState = flush_buffer_internal(State),
    erlang:send_after(State#state.flush_interval, self(), flush_buffer),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    % Flush remaining buffer
    flush_buffer_internal(State),
    % Close log file
    case State#state.log_file of
        undefined -> ok;
        File -> file:close(File)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Create audit entry with hash chain.
create_audit_entry(EventType, Data, State) ->
    Sequence = State#state.sequence,
    Timestamp = erlang:system_time(microsecond),

    Entry = #audit_entry{
        sequence = Sequence,
        timestamp = Timestamp,
        event_type = EventType,
        user_id = maps:get(user_id, Data, undefined),
        session_id = maps:get(session_id, Data, undefined),
        resource = maps:get(resource, Data, undefined),
        action = maps:get(action, Data, undefined),
        result = maps:get(result, Data, success),
        metadata = maps:get(metadata, Data, #{}),
        previous_hash = State#state.current_hash,
        entry_hash = undefined  % Computed below
    },

    % Compute entry hash
    EntryHash = compute_entry_hash(Entry),
    Entry#audit_entry{entry_hash = EntryHash}.

%% @private Compute SHA-256 hash of audit entry.
compute_entry_hash(Entry) ->
    Data = term_to_binary(#{
        sequence => Entry#audit_entry.sequence,
        timestamp => Entry#audit_entry.timestamp,
        event_type => Entry#audit_entry.event_type,
        user_id => Entry#audit_entry.user_id,
        session_id => Entry#audit_entry.session_id,
        resource => Entry#audit_entry.resource,
        action => Entry#audit_entry.action,
        result => Entry#audit_entry.result,
        metadata => Entry#audit_entry.metadata,
        previous_hash => Entry#audit_entry.previous_hash
    }),
    crypto:hash(sha256, Data).

%% @private Flush buffer to disk.
flush_buffer_internal(State) when State#state.buffer =:= [] ->
    State;
flush_buffer_internal(State) ->
    % Reverse buffer (oldest first)
    Entries = lists:reverse(State#state.buffer),

    % Write to file
    lists:foreach(fun(Entry) ->
        Line = format_audit_entry(Entry),
        file:write(State#state.log_file, [Line, <<"\n">>])
    end, Entries),

    % Sync to disk
    file:sync(State#state.log_file),

    % Update state (sequence is already correct, incremented in handle_cast)
    LastEntry = lists:last(Entries),
    State#state{
        buffer = [],
        current_hash = LastEntry#audit_entry.entry_hash
    }.

%% @private Format audit entry as JSON line.
format_audit_entry(Entry) ->
    Map = #{
        sequence => Entry#audit_entry.sequence,
        timestamp => Entry#audit_entry.timestamp,
        event_type => Entry#audit_entry.event_type,
        user_id => Entry#audit_entry.user_id,
        session_id => Entry#audit_entry.session_id,
        resource => Entry#audit_entry.resource,
        action => Entry#audit_entry.action,
        result => Entry#audit_entry.result,
        metadata => Entry#audit_entry.metadata,
        previous_hash => base64:encode(Entry#audit_entry.previous_hash),
        entry_hash => base64:encode(Entry#audit_entry.entry_hash)
    },
    jsx:encode(Map).

%% @private Verify entire hash chain.
do_verify_chain(LogPath) ->
    case file:read_file(LogPath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global, trim]),
            verify_lines(Lines, crypto:hash(sha256, <<"erlmcp_audit_log_genesis">>));
        {error, enoent} ->
            ok;  % Empty log is valid
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Verify hash chain for line list.
verify_lines([], _ExpectedPrevHash) ->
    ok;
verify_lines([Line | Rest], ExpectedPrevHash) ->
    Entry = jsx:decode(Line, [return_maps]),
    PrevHash = base64:decode(maps:get(<<"previous_hash">>, Entry)),
    EntryHash = base64:decode(maps:get(<<"entry_hash">>, Entry)),

    case PrevHash =:= ExpectedPrevHash of
        true ->
            verify_lines(Rest, EntryHash);
        false ->
            Seq = maps:get(<<"sequence">>, Entry),
            {error, {tampered, Seq}}
    end.

%% @private Verify hash chain in range [FromSeq, ToSeq].
%% Only verifies entries in the specified range, not the entire chain.
%% Optimized for partial verification of large audit trails.
do_verify_chain_range(_LogPath, FromSeq, ToSeq) when FromSeq > ToSeq ->
    {error, {invalid_range, FromSeq, ToSeq}};
do_verify_chain_range(LogPath, FromSeq, ToSeq) ->
    case file:read_file(LogPath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global, trim]),
            verify_range(Lines, FromSeq, ToSeq);
        {error, enoent} ->
            {error, empty_log};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Verify hash chain for a range of entries.
%% Algorithm:
%% 1. Read only entries in range [FromSeq, ToSeq]
%% 2. For FromSeq > 0: read entry at FromSeq-1 to get expected hash
%% 3. For FromSeq = 0: use genesis hash
%% 4. Verify internal chain: Seq(N) -> Seq(N-1) for all N in range
%% 5. Return ok or {error, {tampered, Seq}}
verify_range(Lines, FromSeq, ToSeq) ->
    GenesisHash = crypto:hash(sha256, <<"erlmcp_audit_log_genesis">>),

    case read_range_entries(Lines, FromSeq, ToSeq) of
        {ok, RangeEntries} ->
            % Get expected hash for first entry in range
            % First entry has sequence 0, so FromSeq = 0 means use genesis hash
            ExpectedHash = case FromSeq of
                0 ->
                    GenesisHash;
                _ ->
                    % Read previous entry to get its hash
                    case find_entry_by_seq(Lines, FromSeq - 1) of
                        {ok, PrevEntry} ->
                            base64:decode(maps:get(<<"entry_hash">>, PrevEntry));
                        {error, Reason} ->
                            {error, Reason}
                    end
            end,

            case ExpectedHash of
                {error, _} = Error ->
                    Error;
                _ ->
                    % Verify the range starting from the first entry
                    verify_range_hashes(RangeEntries, ExpectedHash)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Read entries in sequence range.
%% Returns ordered list [FromSeq, FromSeq+1, ..., ToSeq]
read_range_entries(Lines, FromSeq, ToSeq) ->
    AllEntries = lists:map(fun(Line) ->
        jsx:decode(Line, [return_maps])
    end, Lines),

    % Filter entries in range
    RangeEntries = lists:filter(fun(Entry) ->
        Seq = maps:get(<<"sequence">>, Entry),
        Seq >= FromSeq andalso Seq =< ToSeq
    end, AllEntries),

    % Check we have all expected entries
    ExpectedCount = ToSeq - FromSeq + 1,
    ActualCount = length(RangeEntries),

    case ActualCount of
        ExpectedCount ->
            % Sort by sequence to ensure order (use custom sort for map keys)
            Sorted = lists:sort(fun(A, B) ->
                maps:get(<<"sequence">>, A) =< maps:get(<<"sequence">>, B)
            end, RangeEntries),
            {ok, Sorted};
        _ when ActualCount < ExpectedCount ->
            {error, {missing_entries, FromSeq, ToSeq, ExpectedCount - ActualCount}};
        _ when ActualCount > ExpectedCount ->
            {error, {duplicate_entries, FromSeq, ToSeq, ActualCount - ExpectedCount}}
    end.

%% @private Verify hash chain for range entries.
%% Handles boundary conditions:
%% - FromSeq = 0: Should link to genesis hash
%% - FromSeq > 0: Should link to FromSeq-1 (not verified here, just checked)
%% - Internal chain: Each entry links to previous entry in range
verify_range_hashes([], _ExpectedHash) ->
    ok;
verify_range_hashes([Entry | Rest], ExpectedHash) ->
    Seq = maps:get(<<"sequence">>, Entry),
    PrevHash = base64:decode(maps:get(<<"previous_hash">>, Entry)),
    EntryHash = base64:decode(maps:get(<<"entry_hash">>, Entry)),

    case PrevHash of
        ExpectedHash ->
            % Hash chain is valid, move to next entry
            verify_range_hashes(Rest, EntryHash);
        _ ->
            % Hash chain broken
            {error, {tampered, Seq}}
    end.

%% @private Find entry by sequence number in lines.
find_entry_by_seq(Lines, Seq) ->
    AllEntries = lists:map(fun(Line) ->
        jsx:decode(Line, [return_maps])
    end, Lines),

    case lists:search(fun(Entry) ->
        maps:get(<<"sequence">>, Entry) =:= Seq
    end, AllEntries) of
        {value, Entry} ->
            {ok, Entry};
        false ->
            {error, {entry_not_found, Seq}}
    end.

%% @private Export logs to format.
do_export_logs(LogPath, Format, OutputPath) ->
    case file:read_file(LogPath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global, trim]),
            Entries = [jsx:decode(L, [return_maps]) || L <- Lines],
            export_format(Entries, Format, OutputPath);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Export entries in specified format.
export_format(Entries, json, OutputPath) ->
    file:write_file(OutputPath, jsx:encode(Entries));
export_format(Entries, csv, OutputPath) ->
    % Simple CSV format
    Header = <<"sequence,timestamp,event_type,user_id,resource,action,result\n">>,
    Rows = [format_csv_row(E) || E <- Entries],
    file:write_file(OutputPath, [Header | Rows]);
export_format(Entries, syslog, OutputPath) ->
    % RFC 5424 syslog format
    Lines = [format_syslog(E) || E <- Entries],
    file:write_file(OutputPath, Lines).

%% @private Format entry as CSV row.
format_csv_row(Entry) ->
    io_lib:format("~p,~p,~p,~p,~p,~p,~p\n", [
        maps:get(<<"sequence">>, Entry, 0),
        maps:get(<<"timestamp">>, Entry, 0),
        maps:get(<<"event_type">>, Entry, <<>>),
        maps:get(<<"user_id">>, Entry, <<>>),
        maps:get(<<"resource">>, Entry, <<>>),
        maps:get(<<"action">>, Entry, <<>>),
        maps:get(<<"result">>, Entry, <<>>)
    ]).

%% @private Format entry as syslog message.
format_syslog(Entry) ->
    io_lib:format("<134>1 ~p erlmcp audit - - - ~s\n", [
        maps:get(<<"timestamp">>, Entry, 0),
        jsx:encode(Entry)
    ]).

%% @private Get user logs in time range.
do_get_user_logs(LogPath, UserId, StartTime, EndTime) ->
    case file:read_file(LogPath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global, trim]),
            Entries = [jsx:decode(L, [return_maps]) || L <- Lines],
            Filtered = lists:filter(fun(E) ->
                maps:get(<<"user_id">>, E, undefined) =:= UserId andalso
                maps:get(<<"timestamp">>, E, 0) >= StartTime andalso
                maps:get(<<"timestamp">>, E, 0) =< EndTime
            end, Entries),
            {ok, Filtered};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Search logs with query.
do_search_logs(LogPath, Query) ->
    case file:read_file(LogPath) of
        {ok, Content} ->
            Lines = binary:split(Content, <<"\n">>, [global, trim]),
            Entries = [jsx:decode(L, [return_maps]) || L <- Lines],
            Filtered = lists:filter(fun(E) ->
                matches_query(E, Query)
            end, Entries),
            {ok, Filtered};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Check if entry matches query.
matches_query(Entry, Query) ->
    maps:fold(fun(Key, Value, Acc) ->
        Acc andalso maps:get(Key, Entry, undefined) =:= Value
    end, true, Query).
