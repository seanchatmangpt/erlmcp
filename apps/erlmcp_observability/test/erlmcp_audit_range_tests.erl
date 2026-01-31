%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_range_tests - Range-Based Audit Log Verification Tests
%%% Tests partial hash chain verification for efficient audit trail validation.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_range_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

range_verify_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Verify valid range 1-3", fun test_verify_valid_range/0},
         {"Verify valid range 2-5", fun test_verify_partial_range/0},
         {"Verify entire log via range", fun test_verify_entire_log/0},
         {"Verify invalid range (reversed)", fun test_verify_invalid_range_reversed/0},
         {"Verify range with missing entries", fun test_verify_range_missing_entries/0},
         {"Verify single entry range", fun test_verify_single_entry/0},
         {"Verify range from beginning (seq 1)", fun test_verify_from_beginning/0},
         {"Verify range boundary conditions", fun test_verify_boundary_conditions/0}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Stop any existing audit log server
    case whereis(erlmcp_audit_log) of
        undefined -> ok;
        _ExistingPid -> erlmcp_audit_log:stop(), timer:sleep(100)
    end,

    % Create temp log directory
    LogDir = "/tmp/erlmcp_audit_range_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    LogPath = LogDir ++ "/audit.log",

    Config = #{
        log_path => LogPath,
        buffer_size => 1,  % Flush immediately
        flush_interval_ms => 100
    },

    {ok, _ServerPid} = erlmcp_audit_log:start_link(Config),

    % Log multiple events
    erlmcp_audit_log:log_auth_success(<<"user_1">>, #{index => 1}),
    erlmcp_audit_log:log_auth_success(<<"user_2">>, #{index => 2}),
    erlmcp_audit_log:log_auth_success(<<"user_3">>, #{index => 3}),
    erlmcp_audit_log:log_auth_success(<<"user_4">>, #{index => 4}),
    erlmcp_audit_log:log_auth_success(<<"user_5">>, #{index => 5}),
    erlmcp_audit_log:log_auth_success(<<"user_6">>, #{index => 6}),

    % Wait for flush
    timer:sleep(200),

    {LogPath, LogDir}.

cleanup({_LogPath, LogDir}) ->
    erlmcp_audit_log:stop(),
    timer:sleep(50),
    % Clean up test directory
    os:cmd("rm -rf " ++ LogDir),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% @doc Verify valid range of entries (1-3)
test_verify_valid_range() ->
    % Verify range 1-3
    ok = erlmcp_audit_log:verify_chain(1, 3),

    % Verify range 4-6
    ok = erlmcp_audit_log:verify_chain(4, 6),

    ok.

%% @doc Verify partial range in middle of log
test_verify_partial_range() ->
    % Verify range 2-5 (middle entries)
    ok = erlmcp_audit_log:verify_chain(2, 5),

    % Verify range 3-4 (small middle range)
    ok = erlmcp_audit_log:verify_chain(3, 4),

    ok.

%% @doc Verify entire log using range verification
test_verify_entire_log() ->
    % Verify entire log using range 1-6
    ok = erlmcp_audit_log:verify_chain(1, 6),

    % Compare with full chain verification
    ok = erlmcp_audit_log:verify_chain(),

    ok.

%% @doc Verify invalid range (FromSeq > ToSeq)
test_verify_invalid_range_reversed() ->
    % Invalid range: FromSeq > ToSeq
    Result = erlmcp_audit_log:verify_chain(5, 2),
    ?assertEqual({error, {invalid_range, 5, 2}}, Result),

    ok.

%% @doc Verify range with entries that don't exist
test_verify_range_missing_entries() ->
    % Try to verify range beyond available entries
    Result = erlmcp_audit_log:verify_chain(1, 100),
    ?assertMatch({error, {missing_entries, 1, 100, _}}, Result),

    ok.

%% @doc Verify single entry range
test_verify_single_entry() ->
    % Verify single entry at sequence 1
    ok = erlmcp_audit_log:verify_chain(1, 1),

    % Verify single entry at sequence 4
    ok = erlmcp_audit_log:verify_chain(4, 4),

    ok.

%% @doc Verify range starting from beginning (sequence 1)
test_verify_from_beginning() ->
    % Range starting at 1 should link to genesis hash
    ok = erlmcp_audit_log:verify_chain(1, 3),

    ok.

%% @doc Verify boundary conditions
test_verify_boundary_conditions() ->
    % Test various boundary conditions
    ok = erlmcp_audit_log:verify_chain(1, 6),  % Full range
    ok = erlmcp_audit_log:verify_chain(6, 6),  % Last entry only
    ok = erlmcp_audit_log:verify_chain(1, 1),  % First entry only
    ok = erlmcp_audit_log:verify_chain(2, 5),  % Middle range

    ok.

%%====================================================================
%% Unit Tests for Internal Functions
%%====================================================================

%% @doc Test read_range_entries with valid range
read_range_entries_test() ->
    % Create test data
    Lines = [
        create_test_line(1, <<"hash1">>),
        create_test_line(2, <<"hash2">>),
        create_test_line(3, <<"hash3">>),
        create_test_line(4, <<"hash4">>)
    ],

    % Test reading range 2-3
    {ok, RangeEntries} = erlmcp_audit_log:read_range_entries(Lines, 2, 3),

    ?assertEqual(2, length(RangeEntries)),
    ?assertEqual(2, maps:get(<<"sequence">>, lists:nth(1, RangeEntries))),
    ?assertEqual(3, maps:get(<<"sequence">>, lists:nth(2, RangeEntries))),

    ok.

%% @doc Test read_range_entries with missing entries
read_range_entries_missing_test() ->
    Lines = [
        create_test_line(1, <<"hash1">>),
        create_test_line(3, <<"hash3">>)  % Missing sequence 2
    ],

    % Try to read range 1-3 (sequence 2 is missing)
    Result = erlmcp_audit_log:read_range_entries(Lines, 1, 3),
    ?assertMatch({error, {missing_entries, 1, 3, _}}, Result),

    ok.

%% @doc Test verify_range_hashes with valid chain
verify_range_hashes_valid_test() ->
    GenesisHash = crypto:hash(sha256, <<"erlmcp_audit_log_genesis">>),
    FixedTimestamp = 1234567890000000,

    % Create entries with proper hash chain
    Entry1Data = #{
        sequence => 1,
        timestamp => FixedTimestamp,
        event_type => auth_success,
        user_id => <<"user_1">>,
        session_id => <<>>,
        resource => <<>>,
        action => <<>>,
        result => success,
        metadata => #{},
        previous_hash => GenesisHash
    },
    Entry1Hash = crypto:hash(sha256, term_to_binary(Entry1Data)),

    Entry2Data = #{
        sequence => 2,
        timestamp => FixedTimestamp,
        event_type => auth_success,
        user_id => <<"user_2">>,
        session_id => <<>>,
        resource => <<>>,
        action => <<>>,
        result => success,
        metadata => #{},
        previous_hash => Entry1Hash
    },
    Entry2Hash = crypto:hash(sha256, term_to_binary(Entry2Data)),

    Entry3Data = #{
        sequence => 3,
        timestamp => FixedTimestamp,
        event_type => auth_success,
        user_id => <<"user_3">>,
        session_id => <<>>,
        resource => <<>>,
        action => <<>>,
        result => success,
        metadata => #{},
        previous_hash => Entry2Hash
    },
    Entry3Hash = crypto:hash(sha256, term_to_binary(Entry3Data)),

    Entries = [
        #{
            <<"sequence">> => 1,
            <<"timestamp">> => FixedTimestamp,
            <<"event_type">> => auth_success,
            <<"user_id">> => <<"user_1">>,
            <<"session_id">> => <<>>,
            <<"resource">> => <<>>,
            <<"action">> => <<>>,
            <<"result">> => success,
            <<"metadata">> => #{},
            <<"previous_hash">> => base64:encode(GenesisHash),
            <<"entry_hash">> => base64:encode(Entry1Hash)
        },
        #{
            <<"sequence">> => 2,
            <<"timestamp">> => FixedTimestamp,
            <<"event_type">> => auth_success,
            <<"user_id">> => <<"user_2">>,
            <<"session_id">> => <<>>,
            <<"resource">> => <<>>,
            <<"action">> => <<>>,
            <<"result">> => success,
            <<"metadata">> => #{},
            <<"previous_hash">> => base64:encode(Entry1Hash),
            <<"entry_hash">> => base64:encode(Entry2Hash)
        },
        #{
            <<"sequence">> => 3,
            <<"timestamp">> => FixedTimestamp,
            <<"event_type">> => auth_success,
            <<"user_id">> => <<"user_3">>,
            <<"session_id">> => <<>>,
            <<"resource">> => <<>>,
            <<"action">> => <<>>,
            <<"result">> => success,
            <<"metadata">> => #{},
            <<"previous_hash">> => base64:encode(Entry2Hash),
            <<"entry_hash">> => base64:encode(Entry3Hash)
        }
    ],

    Result = erlmcp_audit_log:verify_range_hashes(Entries, 1, 3, GenesisHash),
    ?assertEqual(ok, Result),

    ok.

%% @doc Test verify_range_hashes with broken chain
verify_range_hashes_broken_test() ->
    GenesisHash = crypto:hash(sha256, <<"erlmcp_audit_log_genesis">>),

    Entries = [
        create_test_entry(1, GenesisHash),
        create_test_entry(2, <<"wrong_hash">>),  % Broken link
        create_test_entry(3, <<"another_hash">>)
    ],

    Result = erlmcp_audit_log:verify_range_hashes(Entries, 1, 3, GenesisHash),
    ?assertMatch({error, {tampered, 2}}, Result),

    ok.

%% @doc Test find_entry_by_seq
find_entry_by_seq_test() ->
    Lines = [
        create_test_line(1, <<"hash1">>),
        create_test_line(2, <<"hash2">>),
        create_test_line(3, <<"hash3">>)
    ],

    % Find existing entry
    {ok, Entry} = erlmcp_audit_log:find_entry_by_seq(Lines, 2),
    ?assertEqual(2, maps:get(<<"sequence">>, Entry)),

    % Find non-existing entry
    Result = erlmcp_audit_log:find_entry_by_seq(Lines, 99),
    ?assertEqual({error, {entry_not_found, 99}}, Result),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Create a test log line
create_test_line(Seq, PrevHash) ->
    Entry = #{
        <<"sequence">> => Seq,
        <<"timestamp">> => erlang:system_time(microsecond),
        <<"event_type">> => auth_success,
        <<"user_id">> => <<"user_", (integer_to_binary(Seq))/binary>>,
        <<"session_id">> => <<>>,
        <<"resource">> => <<>>,
        <<"action">> => <<>>,
        <<"result">> => success,
        <<"metadata">> => #{},
        <<"previous_hash">> => base64:encode(PrevHash),
        <<"entry_hash">> => base64:encode(crypto:hash(sha256, term_to_binary(Seq)))
    },
    jsx:encode(Entry).

%% @private Create a test entry map
create_test_entry(Seq, PrevHash) ->
    % Create entry data matching the actual audit log format
    EntryData = #{
        sequence => Seq,
        timestamp => erlang:system_time(microsecond),
        event_type => auth_success,
        user_id => <<"user_", (integer_to_binary(Seq))/binary>>,
        session_id => <<>>,
        resource => <<>>,
        action => <<>>,
        result => success,
        metadata => #{},
        previous_hash => PrevHash
    },
    EntryHash = crypto:hash(sha256, term_to_binary(EntryData)),
    #{
        <<"sequence">> => Seq,
        <<"timestamp">> => erlang:system_time(microsecond),
        <<"event_type">> => auth_success,
        <<"user_id">> => <<"user_", (integer_to_binary(Seq))/binary>>,
        <<"session_id">> => <<>>,
        <<"resource">> => <<>>,
        <<"action">> => <<>>,
        <<"result">> => success,
        <<"metadata">> => #{},
        <<"previous_hash">> => base64:encode(PrevHash),
        <<"entry_hash">> => base64:encode(EntryHash)
    }.

%% @private Compute entry hash for test (matches audit log format)
compute_entry_hash(Seq, PrevHash) ->
    EntryData = #{
        sequence => Seq,
        timestamp => erlang:system_time(microsecond),
        event_type => auth_success,
        user_id => <<"user_", (integer_to_binary(Seq))/binary>>,
        session_id => <<>>,
        resource => <<>>,
        action => <<>>,
        result => success,
        metadata => #{},
        previous_hash => PrevHash
    },
    crypto:hash(sha256, term_to_binary(EntryData)).
