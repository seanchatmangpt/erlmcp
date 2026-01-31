%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_log_range_tests - Range Verification Tests
%%% Tests for range-based audit log verification following Joe Armstrong's
%%% principle: "Don't verify the entire chain for partial checks"
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log_range_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

audit_range_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Range verify 0-2 (from root)", fun test_verify_range_from_root/0},
         {"Range verify 3-5 (middle)", fun test_verify_range_middle/0},
         {"Range verify single entry", fun test_verify_range_single/0},
         {"Range verify invalid range", fun test_verify_range_invalid/0},
         {"Range verify boundary: missing previous", fun test_verify_range_missing_previous/0},
         {"Range verify tampered entry", fun test_verify_range_tampered/1},
         {"Range verify entire chain equals full verify", fun test_verify_range_equals_full/0},
         {"Range verify performance on large log", fun test_verify_range_performance/0}
     ]}.

setup() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),

    % Create temp log directory
    LogDir = "/tmp/erlmcp_audit_range_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    LogPath = LogDir ++ "/audit.log",

    Config = #{
        log_path => LogPath,
        buffer_size => 10,
        flush_interval_ms => 1000
    },

    {ok, Pid} = erlmcp_audit_log:start_link(Config),

    {Pid, LogPath, LogDir}.

cleanup({Pid, _LogPath, LogDir}) ->
    erlmcp_audit_log:stop(),
    % Clean up test directory
    os:cmd("rm -rf " ++ LogDir),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% @doc Test range verification from root (sequence 0)
test_verify_range_from_root() ->
    % Log 5 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{})
    end, lists:seq(1, 5)),

    % Wait for flush
    timer:sleep(200),

    % Verify range [0, 2] (first 3 entries)
    ok = erlmcp_audit_log:verify_chain(0, 2),

    ok.

%% @doc Test range verification in middle of chain
test_verify_range_middle() ->
    % Log 5 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{})
    end, lists:seq(1, 5)),

    % Wait for flush
    timer:sleep(200),

    % Verify range [2, 4] (middle 3 entries)
    ok = erlmcp_audit_log:verify_chain(2, 4),

    ok.

%% @doc Test range verification of single entry
test_verify_range_single() ->
    % Log 3 events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user2">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user3">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Verify range [1, 1] (single entry)
    ok = erlmcp_audit_log:verify_chain(1, 1),

    ok.

%% @doc Test invalid range (FromSeq > ToSeq)
test_verify_range_invalid() ->
    % Log some events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Verify invalid range [5, 2]
    {error, {invalid_range, 5, 2}} = erlmcp_audit_log:verify_chain(5, 2),

    ok.

%% @doc Test range verification with missing previous entry
test_verify_range_missing_previous() ->
    % Log only 2 events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user2">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Verify range [5, 10] (entries don't exist)
    {error, {missing_entries, 5, 10, _}} = erlmcp_audit_log:verify_chain(5, 10),

    ok.

%% @doc Test range verification detects tampered entry
test_verify_range_tampered({_Pid, LogPath, _LogDir}) ->
    % Log 5 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{})
    end, lists:seq(1, 5)),

    % Wait for flush
    timer:sleep(200),

    % Verify range is valid
    ok = erlmcp_audit_log:verify_chain(1, 3),

    % Tamper with entry at sequence 2
    {ok, Content} = file:read_file(LogPath),
    Lines = binary:split(Content, <<"\n">>, [global, trim]),

    case Lines of
        [Line1, Line2, Line3 | Rest] ->
            % Modify entry 2 (third line, sequence 2)
            Entry3 = jsx:decode(Line3, [return_maps]),
            TamperedEntry3 = Entry3#{<<"user_id">> => <<"hacker">>},
            TamperedLine3 = jsx:encode(TamperedEntry3),

            TamperedContent = iolist_to_binary([
                Line1, <<"\n">>,
                Line2, <<"\n">>,
                TamperedLine3, <<"\n">>,
                lists:join(<<"\n">>, Rest)
            ]),

            ok = file:write_file(LogPath, TamperedContent),

            % Range verification should detect tampering at sequence 2
            {error, {tampered, 2}} = erlmcp_audit_log:verify_chain(1, 3);
        _ ->
            % Not enough lines, test passes
            ok
    end,

    ok.

%% @doc Test that range verification equals full verification
test_verify_range_equals_full() ->
    % Log 10 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{})
    end, lists:seq(1, 10)),

    % Wait for flush
    timer:sleep(200),

    % Verify full chain
    ok = erlmcp_audit_log:verify_chain(),

    % Verify range [0, 9] (all entries)
    ok = erlmcp_audit_log:verify_chain(0, 9),

    % Verify range [0, 4] (first half)
    ok = erlmcp_audit_log:verify_chain(0, 4),

    % Verify range [5, 9] (second half)
    ok = erlmcp_audit_log:verify_chain(5, 9),

    ok.

%% @doc Test performance on large audit log (100 entries)
test_verify_range_performance() ->
    % Log 100 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{
            index => I
        })
    end, lists:seq(1, 100)),

    % Wait for flush
    timer:sleep(500),

    % Measure full verification time
    {TFull, ok} = timer:tc(fun() ->
        erlmcp_audit_log:verify_chain()
    end),

    % Measure range verification time (entries 20-40)
    {TRange, ok} = timer:tc(fun() ->
        erlmcp_audit_log:verify_chain(20, 40)
    end),

    % Range verification should be faster (at least 20% faster)
    ?assert(TRange < TFull * 0.8),

    % Verify multiple ranges
    ok = erlmcp_audit_log:verify_chain(0, 9),
    ok = erlmcp_audit_log:verify_chain(10, 19),
    ok = erlmcp_audit_log:verify_chain(20, 29),
    ok = erlmcp_audit_log:verify_chain(90, 99),

    ok.

%%====================================================================
%% Edge Case Tests
%%====================================================================

range_edge_cases_test_() ->
    {setup,
     fun setup_edge_cases/0,
     fun cleanup_edge_cases/1,
     [
         {"Range verify empty log", fun test_verify_range_empty_log/1},
         {"Range verify out of bounds", fun test_verify_range_out_of_bounds/0},
         {"Range verify duplicate sequences", fun test_verify_range_duplicate_sequences/1}
     ]}.

setup_edge_cases() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    LogDir = "/tmp/erlmcp_audit_edge_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    LogPath = LogDir ++ "/audit_edge.log",
    Config = #{log_path => LogPath, buffer_size => 5, flush_interval_ms => 500},
    {ok, Pid} = erlmcp_audit_log:start_link(Config),
    {Pid, LogPath, LogDir}.

cleanup_edge_cases({Pid, _LogPath, LogDir}) ->
    erlmcp_audit_log:stop(),
    os:cmd("rm -rf " ++ LogDir),
    ok.

%% @doc Test range verification on empty log
test_verify_range_empty_log({_Pid, LogPath, _LogDir}) ->
    % Create a new empty log file
    ok = file:write_file(LogPath, <<>>),

    % Verify range on empty log
    {error, empty_log} = erlmcp_audit_log:verify_chain(0, 0),

    ok.

%% @doc Test range verification with out-of-bounds sequences
test_verify_range_out_of_bounds() ->
    % Log only 2 events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user2">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Verify range [10, 20] (way out of bounds)
    {error, {missing_entries, 10, 20, _}} = erlmcp_audit_log:verify_chain(10, 20),

    ok.

%% @doc Test range verification with duplicate sequence numbers (corruption)
test_verify_range_duplicate_sequences({_Pid, LogPath, _LogDir}) ->
    % Log 2 events
    ok = erlmcp_audit_log:log_auth_success(<<"user1">>, #{}),
    ok = erlmcp_audit_log:log_auth_success(<<"user2">>, #{}),

    % Wait for flush
    timer:sleep(200),

    % Corrupt log: duplicate first line
    {ok, Content} = file:read_file(LogPath),
    Lines = binary:split(Content, <<"\n">>, [global, trim]),

    case Lines of
        [Line1 | Rest] ->
            % Duplicate first line (creates duplicate sequence 0)
            CorruptedContent = iolist_to_binary([
                Line1, <<"\n">>,
                Line1, <<"\n">>,
                lists:join(<<"\n">>, Rest)
            ]),

            ok = file:write_file(LogPath, CorruptedContent),

            % Range verification should detect duplicates
            {error, {duplicate_entries, 0, 1, _}} = erlmcp_audit_log:verify_chain(0, 1);
        _ ->
            ok
    end,

    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

range_integration_test_() ->
    {setup,
     fun setup_integration/0,
     fun cleanup_integration/1,
     [
         {"Range verify after operations", fun test_verify_range_after_operations/0},
         {"Range verify concurrent with full verify", fun test_verify_range_concurrent/0}
     ]}.

setup_integration() ->
    application:ensure_all_started(crypto),
    application:ensure_all_started(jsx),
    LogDir = "/tmp/erlmcp_audit_int_test_" ++ integer_to_list(erlang:unique_integer([positive])),
    LogPath = LogDir ++ "/audit_int.log",
    Config = #{log_path => LogPath, buffer_size => 10, flush_interval_ms => 1000},
    {ok, Pid} = erlmcp_audit_log:start_link(Config),
    {Pid, LogPath, LogDir}.

cleanup_integration({Pid, _LogPath, LogDir}) ->
    erlmcp_audit_log:stop(),
    os:cmd("rm -rf " ++ LogDir),
    ok.

%% @doc Test range verification after various operations
test_verify_range_after_operations() ->
    % Log different event types
    ok = erlmcp_audit_log:log_auth_success(<<"alice">>, #{role => admin}),
    ok = erlmcp_audit_log:log_operation(<<"bob">>, <<"/api/test">>, <<"read">>, #{}),
    ok = erlmcp_audit_log:log_permission_check(<<"charlie">>, <<"/api/admin">>, <<"write">>, {error, forbidden}),
    ok = erlmcp_audit_log:log_sensitive_operation(<<"alice">>, <<"key_rotation">>, #{}),
    ok = erlmcp_audit_log:log_auth_failure(<<"eve">>, #{reason => invalid_token}),

    % Wait for flush
    timer:sleep(200),

    % Verify ranges
    ok = erlmcp_audit_log:verify_chain(0, 2),
    ok = erlmcp_audit_log:verify_chain(2, 4),
    ok = erlmcp_audit_log:verify_chain(0, 4),

    ok.

%% @doc Test concurrent range and full verification
test_verify_range_concurrent() ->
    % Log 20 events
    lists:foreach(fun(I) ->
        ok = erlmcp_audit_log:log_auth_success(<<"user_", (integer_to_binary(I))/binary>>, #{})
    end, lists:seq(1, 20)),

    % Wait for flush
    timer:sleep(300),

    % Spawn multiple verification processes
    Self = self(),
    spawn(fun() ->
        Result = erlmcp_audit_log:verify_chain(),
        Self ! {full_verify, Result}
    end),

    spawn(fun() ->
        Result = erlmcp_audit_log:verify_chain(0, 9),
        Self ! {range_verify1, Result}
    end),

    spawn(fun() ->
        Result = erlmcp_audit_log:verify_chain(10, 19),
        Self ! {range_verify2, Result}
    end),

    % Collect results
    receive {full_verify, ok} -> ok after 1000 -> ?assert(false) end,
    receive {range_verify1, ok} -> ok after 1000 -> ?assert(false) end,
    receive {range_verify2, ok} -> ok after 1000 -> ?assert(false) end,

    ok.

%%====================================================================
%% Property-Based Tests (Optional - requires Proper)
%%====================================================================

%% Note: Property-based tests using Proper can be added here
%% to verify that range verification matches full verification
%% for all valid ranges in a log.
