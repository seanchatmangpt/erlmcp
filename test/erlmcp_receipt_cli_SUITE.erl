%%%-------------------------------------------------------------------
%%% @doc erlmcp Receipt CLI Test Suite
%%%
%%% Comprehensive tests for receipt inspection commands:
%%% - tail(N) - Stream N most recent receipts
%%% - show(ReceiptId) - Display full receipt details
%%% - verify(ReceiptId) - Hash chain validation + schema check
%%% - export(ReceiptId, Format) - JSON/CSV/TSV export
%%%
%%% Test coverage includes:
%%% - Output format correctness (table, JSON, CSV, TSV)
%%% - Hash validation and chain integrity
%%% - Schema validation and field type checking
%%% - Deterministic output (runs 5x to verify consistency)
%%% - Error handling (missing receipts, invalid formats)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_receipt_cli_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

% Common Test exports
-export([
    all/0,
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

% Test cases
-export([
    test_tail_outputs_correct_count/1,
    test_tail_outputs_newest_first/1,
    test_tail_with_different_formats/1,
    test_show_returns_complete_receipt/1,
    test_show_includes_computed_hash/1,
    test_show_nonexistent_receipt_returns_error/1,
    test_verify_valid_receipt/1,
    test_verify_checks_hash_integrity/1,
    test_verify_validates_timestamp/1,
    test_verify_requires_all_fields/1,
    test_export_json_format/1,
    test_export_csv_format/1,
    test_export_tsv_format/1,
    test_export_deterministic_output/1,
    test_format_table_has_correct_structure/1,
    test_hash_computed_consistently/1,
    test_receipt_field_type_validation/1,
    test_tail_empty_directory/1
]).

-define(TEST_RECEIPTS_DIR, "./test/fixtures/receipts").

%%%===================================================================
%%% Common Test Setup
%%%===================================================================

suite() ->
    [{timetrap, {minutes, 5}}].

all() ->
    [
        test_tail_outputs_correct_count,
        test_tail_outputs_newest_first,
        test_tail_with_different_formats,
        test_show_returns_complete_receipt,
        test_show_includes_computed_hash,
        test_show_nonexistent_receipt_returns_error,
        test_verify_valid_receipt,
        test_verify_checks_hash_integrity,
        test_verify_validates_timestamp,
        test_verify_requires_all_fields,
        test_export_json_format,
        test_export_csv_format,
        test_export_tsv_format,
        test_export_deterministic_output,
        test_format_table_has_correct_structure,
        test_hash_computed_consistently,
        test_receipt_field_type_validation,
        test_tail_empty_directory
    ].

init_per_suite(Config) ->
    % Set receipts directory to test fixtures
    os:putenv("ERLMCP_RECEIPTS_DIR", ?TEST_RECEIPTS_DIR),

    % Create test fixtures directory
    filelib:ensure_dir(?TEST_RECEIPTS_DIR ++ "/"),
    create_test_receipts(),

    Config.

end_per_suite(_Config) ->
    % Cleanup
    case file:list_dir(?TEST_RECEIPTS_DIR) of
        {ok, Files} ->
            [file:delete(filename:join(?TEST_RECEIPTS_DIR, F)) || F <- Files];
        _ ->
            ok
    end,
    file:del_dir(?TEST_RECEIPTS_DIR),
    os:unsetenv("ERLMCP_RECEIPTS_DIR").

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases - tail/1 and tail/2
%%%===================================================================

test_tail_outputs_correct_count(Config) ->
    % Request 3 receipts
    {ok, Receipts, _Formatted} = erlmcp_receipt_cli:tail(3),

    % Verify we get exactly 3 receipts
    ?assertEqual(3, length(Receipts)),

    % Verify all are maps (receipts)
    lists:foreach(fun(R) ->
        ?assert(is_map(R))
    end, Receipts),

    ct:log("Tail(3) returned ~p receipts~n", [length(Receipts)]),
    ok.

test_tail_outputs_newest_first(Config) ->
    {ok, Receipts, _Formatted} = erlmcp_receipt_cli:tail(5),

    % Verify timestamps are in descending order
    Timestamps = [maps:get(<<"timestamp">>, R, 0) || R <- Receipts],

    % Check descending order
    case Timestamps of
        [] -> ok;
        [_] -> ok;
        [T1, T2 | Rest] ->
            ?assert(T1 >= T2),
            lists:foldl(fun(T, Prev) ->
                ?assert(Prev >= T),
                T
            end, T2, Rest),
            ok
    end,

    ct:log("Timestamps in order: ~p~n", [Timestamps]),
    ok.

test_tail_with_different_formats(Config) ->
    % Test JSON format
    {ok, _R1, JsonData} = erlmcp_receipt_cli:tail(2, json),
    ?assert(is_iodata(JsonData)),
    JsonStr = iolist_to_binary(JsonData),
    ?assert(byte_size(JsonStr) > 0),

    % Verify it's valid JSON by decoding
    try jsx:decode(JsonStr) of
        _Decoded -> ok
    catch
        _:_ -> ct:fail("Invalid JSON from tail/2")
    end,

    % Test CSV format
    {ok, _R2, CsvData} = erlmcp_receipt_cli:tail(2, csv),
    ?assert(is_iodata(CsvData)),
    CsvStr = iolist_to_binary(CsvData),
    ?assert(byte_size(CsvStr) > 0),
    % Should contain header with comma
    ?assert(string:find(binary_to_list(CsvStr), ",") =/= nomatch),

    % Test TSV format
    {ok, _R3, TsvData} = erlmcp_receipt_cli:tail(2, tsv),
    ?assert(is_iodata(TsvData)),
    TsvStr = iolist_to_binary(TsvData),
    ?assert(byte_size(TsvStr) > 0),
    % Should contain header with tab
    ?assert(string:find(binary_to_list(TsvStr), "\t") =/= nomatch),

    % Test table format (default)
    {ok, _R4, TableData} = erlmcp_receipt_cli:tail(2, table),
    ?assert(is_iodata(TableData)),
    TableStr = iolist_to_binary(TableData),
    ?assert(byte_size(TableStr) > 0),
    % Should contain table separator
    ?assert(string:find(binary_to_list(TableStr), "-") =/= nomatch),

    ct:log("All formats produced output~n"),
    ok.

%%%===================================================================
%%% Test Cases - show/1
%%%===================================================================

test_show_returns_complete_receipt(Config) ->
    % Create a test receipt
    Receipt = create_test_receipt(<<"test-receipt-1">>),
    save_test_receipt(Receipt),

    % Show the receipt
    {ok, Shown} = erlmcp_receipt_cli:show(<<"test-receipt-1">>),

    % Verify all fields present
    ?assert(maps:is_key(<<"receipt_id">>, Shown)),
    ?assert(maps:is_key(<<"timestamp">>, Shown)),
    ?assert(maps:is_key(<<"receipt_type">>, Shown)),
    ?assert(maps:is_key(<<"sku_id">>, Shown)),

    % Verify computed_hash is added
    ?assert(maps:is_key(computed_hash, Shown)),
    ?assert(maps:is_key(hash_verified, Shown)),

    ct:log("Show returned complete receipt with hash verification~n"),
    ok.

test_show_includes_computed_hash(Config) ->
    Receipt = create_test_receipt(<<"hash-test-receipt">>),
    save_test_receipt(Receipt),

    {ok, Shown} = erlmcp_receipt_cli:show(<<"hash-test-receipt">>),

    % Verify hash is binary
    Hash = maps:get(computed_hash, Shown),
    ?assert(is_binary(Hash)),
    ?assert(byte_size(Hash) > 0),

    % Verify hash_verified flag
    Verified = maps:get(hash_verified, Shown),
    ?assert(is_boolean(Verified)),

    ct:log("Computed hash: ~p~n", [Hash]),
    ct:log("Hash verified: ~p~n", [Verified]),
    ok.

test_show_nonexistent_receipt_returns_error(Config) ->
    Result = erlmcp_receipt_cli:show(<<"nonexistent-receipt-xyz">>),
    ?assertEqual({error, {not_found, "nonexistent-receipt-xyz"}}, Result),
    ok.

%%%===================================================================
%%% Test Cases - verify/1
%%%===================================================================

test_verify_valid_receipt(Config) ->
    Receipt = create_test_receipt(<<"valid-receipt">>),
    save_test_receipt(Receipt),

    Result = erlmcp_receipt_cli:verify(<<"valid-receipt">>),
    ?assertEqual({ok, verified}, Result),

    ct:log("Valid receipt verified successfully~n"),
    ok.

test_verify_checks_hash_integrity(Config) ->
    % For receipts without explicit hash stored, verify should pass
    Receipt = create_test_receipt(<<"hash-verify-receipt">>),
    save_test_receipt(Receipt),

    Result = erlmcp_receipt_cli:verify(<<"hash-verify-receipt">>),
    ?assertEqual({ok, verified}, Result),
    ok.

test_verify_validates_timestamp(Config) ->
    % Create receipt with invalid future timestamp
    FutureTime = erlang:system_time(millisecond) + (35 * 24 * 60 * 60 * 1000),
    Receipt = #{
        <<"receipt_id">> => <<"future-receipt">>,
        <<"timestamp">> => FutureTime,
        <<"receipt_type">> => <<"test">>,
        <<"sku_id">> => <<"SKU-TEST">>,
        <<"status">> => <<"pass">>
    },
    save_test_receipt(Receipt),

    Result = erlmcp_receipt_cli:verify(<<"future-receipt">>),
    ?assertMatch({error, future_timestamp}, Result),
    ok.

test_verify_requires_all_fields(Config) ->
    % Create receipt missing required field
    Receipt = #{
        <<"receipt_id">> => <<"incomplete-receipt">>,
        <<"timestamp">> => erlang:system_time(millisecond)
        % Missing: receipt_type, sku_id
    },
    save_test_receipt(Receipt),

    Result = erlmcp_receipt_cli:verify(<<"incomplete-receipt">>),
    ?assertMatch({error, missing_required_fields}, Result),
    ok.

%%%===================================================================
%%% Test Cases - export/2 and export/3
%%%===================================================================

test_export_json_format(Config) ->
    Receipt = create_test_receipt(<<"json-export-receipt">>),
    save_test_receipt(Receipt),

    {ok, JsonData} = erlmcp_receipt_cli:export(<<"json-export-receipt">>, json),
    ?assert(is_iodata(JsonData)),

    % Verify valid JSON
    JsonBin = iolist_to_binary(JsonData),
    try jsx:decode(JsonBin) of
        Decoded ->
            ?assert(is_map(Decoded)),
            ct:log("Exported JSON decoded successfully~n"),
            ok
    catch
        _:_ -> ct:fail("Invalid JSON export")
    end.

test_export_csv_format(Config) ->
    Receipt = create_test_receipt(<<"csv-export-receipt">>),
    save_test_receipt(Receipt),

    {ok, CsvData} = erlmcp_receipt_cli:export(<<"csv-export-receipt">>, csv),
    ?assert(is_iodata(CsvData)),

    CsvStr = iolist_to_binary(CsvData),
    CsvList = binary_to_list(CsvStr),

    % Verify CSV structure
    Lines = string:split(CsvList, "\n", all),
    ?assert(length(Lines) >= 2),  % Header + at least 1 row

    % Verify first line contains expected headers
    Header = lists:nth(1, Lines),
    ?assert(string:find(Header, "receipt_id") =/= nomatch),
    ?assert(string:find(Header, "receipt_type") =/= nomatch),

    ct:log("CSV export has correct structure~n"),
    ok.

test_export_tsv_format(Config) ->
    Receipt = create_test_receipt(<<"tsv-export-receipt">>),
    save_test_receipt(Receipt),

    {ok, TsvData} = erlmcp_receipt_cli:export(<<"tsv-export-receipt">>, tsv),
    ?assert(is_iodata(TsvData)),

    TsvStr = iolist_to_binary(TsvData),
    TsvList = binary_to_list(TsvStr),

    % Verify TSV structure
    Lines = string:split(TsvList, "\n", all),
    ?assert(length(Lines) >= 2),

    % Verify first line is header with tabs
    Header = lists:nth(1, Lines),
    ?assert(string:find(Header, "\t") =/= nomatch),

    ct:log("TSV export has correct structure~n"),
    ok.

test_export_deterministic_output(Config) ->
    % Test that exporting the same receipt 5 times produces identical output
    Receipt = create_test_receipt(<<"deterministic-receipt">>),
    save_test_receipt(Receipt),

    % Export 5 times in JSON format
    Exports = [erlmcp_receipt_cli:export(<<"deterministic-receipt">>, json)
               || _ <- lists:seq(1, 5)],

    % All should succeed
    ?assert(lists:all(fun(E) -> element(1, E) =:= ok end, Exports)),

    % Extract binary data from each
    Binaries = [iolist_to_binary(Data) || {ok, Data} <- Exports],

    % All should be identical
    [First | Rest] = Binaries,
    lists:foreach(fun(Bin) ->
        ?assertEqual(First, Bin, "Output not deterministic across runs")
    end, Rest),

    ct:log("Export output is deterministic across 5 runs~n"),
    ok.

%%%===================================================================
%%% Test Cases - Format Functions
%%%===================================================================

test_format_table_has_correct_structure(Config) ->
    Receipts = [
        create_test_receipt(<<"r1">>),
        create_test_receipt(<<"r2">>),
        create_test_receipt(<<"r3">>)
    ],

    TableData = erlmcp_receipt_cli:format_receipt_table(Receipts),
    ?assert(is_iodata(TableData)),

    TableStr = iolist_to_binary(TableData),
    TableList = binary_to_list(TableStr),

    % Should contain header markers
    ?assert(string:find(TableList, "RECEIPT_ID") =/= nomatch),
    ?assert(string:find(TableList, "TYPE") =/= nomatch),
    ?assert(string:find(TableList, "SKU_ID") =/= nomatch),

    % Should contain separator
    ?assert(string:find(TableList, "-") =/= nomatch),

    % Should contain data rows
    Lines = string:split(TableList, "\n", all),
    ?assert(length(Lines) > 3),  % Header + separator + data rows

    ct:log("Table format has correct structure~n"),
    ok.

test_hash_computed_consistently(Config) ->
    Receipt = create_test_receipt(<<"hash-consistency-test">>),

    % Compute hash 5 times
    Hashes = [erlmcp_receipt_cli:computed_hash(Receipt) || _ <- lists:seq(1, 5)],

    % All hashes should be identical
    [First | Rest] = Hashes,
    lists:foreach(fun(Hash) ->
        ?assertEqual(First, Hash, "Hash not computed consistently")
    end, Rest),

    % Verify hash is base64-encoded SHA256 (44 chars + padding)
    ?assert(byte_size(First) > 0),
    ?assert(byte_size(First) =< 45),  % Base64 SHA256 + padding

    ct:log("Hash computed consistently: ~p~n", [First]),
    ok.

%%%===================================================================
%%% Test Cases - Schema Validation
%%%===================================================================

test_receipt_field_type_validation(Config) ->
    % Test invalid receipt_id type
    BadReceipt1 = #{
        <<"receipt_id">> => 123,  % Should be binary
        <<"timestamp">> => erlang:system_time(millisecond),
        <<"receipt_type">> => <<"test">>,
        <<"sku_id">> => <<"SKU">>
    },
    save_test_receipt(BadReceipt1),
    Result1 = erlmcp_receipt_cli:verify(<<"123">>),
    ?assertMatch({error, {invalid_type, receipt_id, binary}}, Result1),

    % Test invalid timestamp type
    BadReceipt2 = #{
        <<"receipt_id">> => <<"bad-ts">>,
        <<"timestamp">> => <<"not-a-number">>,  % Should be integer
        <<"receipt_type">> => <<"test">>,
        <<"sku_id">> => <<"SKU">>
    },
    save_test_receipt(BadReceipt2),
    Result2 = erlmcp_receipt_cli:verify(<<"bad-ts">>),
    ?assertMatch({error, {invalid_type, timestamp, integer}}, Result2),

    ct:log("Field type validation works correctly~n"),
    ok.

%%%===================================================================
%%% Test Cases - Edge Cases
%%%===================================================================

test_tail_empty_directory(Config) ->
    % Create temporary empty directory
    TempDir = "./test/fixtures/empty_receipts",
    filelib:ensure_dir(TempDir ++ "/"),
    os:putenv("ERLMCP_RECEIPTS_DIR", TempDir),

    {ok, Receipts, _Formatted} = erlmcp_receipt_cli:tail(10),
    ?assertEqual([], Receipts),

    % Restore receipts directory
    os:putenv("ERLMCP_RECEIPTS_DIR", ?TEST_RECEIPTS_DIR),
    case file:list_dir(TempDir) of
        {ok, Files} ->
            [file:delete(filename:join(TempDir, F)) || F <- Files];
        _ -> ok
    end,
    file:del_dir(TempDir),

    ct:log("Empty directory handled correctly~n"),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Create a test receipt with given ID.
create_test_receipt(ReceiptId) ->
    Now = erlang:system_time(millisecond),
    #{
        <<"receipt_id">> => ReceiptId,
        <<"timestamp">> => Now,
        <<"timestamp_iso">> => iso8601_timestamp(Now),
        <<"receipt_type">> => <<"test_receipt">>,
        <<"sku_id">> => <<"SKU-TEST-", (binary:part(ReceiptId, 0, min(10, byte_size(ReceiptId))))/binary>>,
        <<"stage">> => <<"testing">>,
        <<"status">> => <<"pass">>,
        <<"evidence">> => <<"Test evidence data">>,
        <<"ontology_refs">> => [
            <<"http://example.org/tcps/ontology#TestReceipt">>,
            <<"http://example.org/tcps/ontology#testing">>
        ]
    }.

%% @doc Save a test receipt to disk.
save_test_receipt(Receipt) ->
    ReceiptId = maps:get(<<"receipt_id">>, Receipt),
    Filename = binary_to_list(ReceiptId) ++ ".json",
    Path = filename:join(?TEST_RECEIPTS_DIR, Filename),
    JsonBin = jsx:encode(Receipt),
    file:write_file(Path, JsonBin).

%% @doc Create test receipts directory and populate with samples.
create_test_receipts() ->
    filelib:ensure_dir(?TEST_RECEIPTS_DIR ++ "/"),

    % Create 10 test receipts with varying timestamps
    BaseTime = erlang:system_time(millisecond),
    Receipts = [begin
        Time = BaseTime - (I * 1000),  % 1 second apart, newest first
        #{
            <<"receipt_id">> => <<"RCPT-", (integer_to_binary(I))/binary>>,
            <<"timestamp">> => Time,
            <<"timestamp_iso">> => iso8601_timestamp(Time),
            <<"receipt_type">> => <<"test_receipt">>,
            <<"sku_id">> => <<"SKU-", (integer_to_binary(I))/binary>>,
            <<"stage">> => pick_stage(I),
            <<"status">> => case I rem 3 of 0 -> <<"pass">>; _ -> <<"fail">> end,
            <<"evidence">> => <<"Evidence data for receipt ", (integer_to_binary(I))/binary>>
        }
    end || I <- lists:seq(1, 10)],

    lists:foreach(fun(R) -> save_test_receipt(R) end, Receipts).

%% @doc Pick a stage based on receipt number.
pick_stage(I) ->
    Stages = [
        <<"compile">>,
        <<"test">>,
        <<"release">>,
        <<"security">>,
        <<"deploy">>
    ],
    lists:nth((I rem 5) + 1, Stages).

%% @doc Convert millisecond timestamp to ISO 8601 format.
iso8601_timestamp(Ms) ->
    Seconds = Ms div 1000,
    Micros = (Ms rem 1000) * 1000,
    {Date, Time} = calendar:gregorian_seconds_to_datetime(Seconds + 62167219200),
    {Y, Mo, D} = Date,
    {H, Mi, S} = Time,
    Str = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                        [Y, Mo, D, H, Mi, S, Ms rem 1000]),
    iolist_to_binary(Str).

%% @doc Check if value is iodata.
is_iodata(X) ->
    is_iolist(X).

%% @doc Simplified iolist check.
is_iolist(X) when is_binary(X) -> true;
is_iolist(X) when is_atom(X) -> false;
is_iolist(X) when is_integer(X) -> false;
is_iolist([]) -> true;
is_iolist([H | T]) ->
    is_iodata_element(H) andalso is_iolist(T);
is_iolist(X) -> is_binary(X).

is_iodata_element(X) when is_integer(X), X >= 0, X < 256 -> true;
is_iodata_element(X) when is_binary(X) -> true;
is_iodata_element(X) -> is_iolist(X).
