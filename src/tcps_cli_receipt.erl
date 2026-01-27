%%%-----------------------------------------------------------------------------
%%% @doc TCPS CLI Receipt Management
%%%
%%% Handles all receipt-related CLI commands.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_cli_receipt).

-export([run/1]).

%%%=============================================================================
%%% API
%%%=============================================================================

run(["verify", Stage, SkuId | _]) ->
    verify_receipt(Stage, list_to_binary(SkuId));

run(["chain", SkuId | Args]) ->
    show_chain(list_to_binary(SkuId), parse_args(Args));

run(["list" | Args]) ->
    list_receipts(parse_args(Args));

run(["show", ReceiptId | _]) ->
    show_receipt(list_to_binary(ReceiptId));

run(["validate", ReceiptId | _]) ->
    validate_receipt(list_to_binary(ReceiptId));

run([]) ->
    tcps_cli_format:error("Missing subcommand. Use: verify, chain, list, show, validate"),
    halt(1);

run([Unknown | _]) ->
    tcps_cli_format:error("Unknown subcommand: ~s", [Unknown]),
    halt(1).

%%%=============================================================================
%%% Command Implementations
%%%=============================================================================

verify_receipt(StageStr, SkuId) ->
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),

    % Look for receipt matching stage and SKU
    case find_receipt(ReceiptsDir, StageStr, SkuId) of
        {ok, Receipt} ->
            tcps_cli_format:success("Receipt verified for ~s/~s", [StageStr, SkuId]),
            tcps_cli_format:info("Receipt ID: ~s", [maps:get(<<"receipt_id">>, Receipt, <<"N/A">>)]),
            tcps_cli_format:info("Timestamp:  ~s", [maps:get(<<"timestamp_iso">>, Receipt, <<"N/A">>)]),
            halt(0);
        {error, not_found} ->
            tcps_cli_format:error("No receipt found for ~s/~s", [StageStr, SkuId]),
            halt(1);
        {error, Reason} ->
            tcps_cli_format:error("Error verifying receipt: ~p", [Reason]),
            halt(1)
    end.

show_chain(SkuId, Args) ->
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),

    case find_all_receipts(ReceiptsDir, SkuId) of
        {ok, Receipts} when length(Receipts) > 0 ->
            Format = maps:get(format, Args, tcps_cli_config:get(output_format, table)),

            case Format of
                graph -> print_receipt_graph(Receipts, SkuId);
                _ ->
                    Headers = [receipt_id, receipt_type, stage, timestamp],
                    FormattedReceipts = [format_receipt_summary(R) || R <- Receipts],
                    tcps_cli_format:output(FormattedReceipts, Format, #{headers => Headers})
            end,
            halt(0);
        {ok, []} ->
            tcps_cli_format:error("No receipts found for SKU: ~s", [SkuId]),
            halt(1);
        {error, Reason} ->
            tcps_cli_format:error("Error retrieving receipt chain: ~p", [Reason]),
            halt(1)
    end.

list_receipts(Args) ->
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    Recent = maps:get(recent, Args, 10),

    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
            Receipts = lists:filtermap(fun(File) ->
                FullPath = filename:join(ReceiptsDir, File),
                case file:read_file(FullPath) of
                    {ok, JsonBin} ->
                        {true, jsx:decode(JsonBin, [return_maps])};
                    _ ->
                        false
                end
            end, JsonFiles),

            % Sort by timestamp (newest first)
            Sorted = lists:reverse(lists:keysort(2, [{R, timestamp_to_int(R)} || R <- Receipts])),
            TopReceipts = lists:sublist([R || {R, _} <- Sorted], Recent),

            case TopReceipts of
                [] ->
                    io:format("No receipts found.~n"),
                    halt(0);
                _ ->
                    Format = tcps_cli_config:get(output_format, table),
                    Headers = [receipt_id, receipt_type, sku_id, timestamp],
                    FormattedData = [format_receipt_summary(R) || R <- TopReceipts],
                    tcps_cli_format:output(FormattedData, Format, #{headers => Headers}),
                    halt(0)
            end;
        {error, Reason} ->
            tcps_cli_format:error("Error listing receipts: ~p", [Reason]),
            halt(1)
    end.

show_receipt(ReceiptId) ->
    ReceiptsDir = tcps_cli_config:get(receipts_path, "./priv/receipts"),
    ReceiptFile = binary_to_list(ReceiptId) ++ ".json",
    FullPath = filename:join(ReceiptsDir, ReceiptFile),

    case file:read_file(FullPath) of
        {ok, JsonBin} ->
            Receipt = jsx:decode(JsonBin, [return_maps]),
            Format = tcps_cli_config:get(output_format, table),
            case Format of
                json ->
                    tcps_cli_format:json(Receipt);
                _ ->
                    print_receipt_details(Receipt)
            end,
            halt(0);
        {error, enoent} ->
            tcps_cli_format:error("Receipt not found: ~s", [ReceiptId]),
            halt(1);
        {error, Reason} ->
            tcps_cli_format:error("Error reading receipt: ~p", [Reason]),
            halt(1)
    end.

validate_receipt(_ReceiptId) ->
    tcps_cli_format:error("SHACL validation not yet implemented"),
    halt(1).

%%%=============================================================================
%%% Helper Functions
%%%=============================================================================

parse_args(Args) ->
    parse_args(Args, #{}).

parse_args([], Acc) ->
    Acc;
parse_args(["--format", Format | Rest], Acc) ->
    parse_args(Rest, Acc#{format => list_to_atom(Format)});
parse_args(["--recent", Num | Rest], Acc) ->
    parse_args(Rest, Acc#{recent => list_to_integer(Num)});
parse_args([Unknown | Rest], Acc) ->
    tcps_cli_format:warning("Unknown argument: ~s", [Unknown]),
    parse_args(Rest, Acc).

find_receipt(ReceiptsDir, StageStr, SkuId) ->
    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
            find_matching_receipt(ReceiptsDir, JsonFiles, StageStr, SkuId);
        {error, Reason} ->
            {error, Reason}
    end.

find_matching_receipt(_Dir, [], _Stage, _SkuId) ->
    {error, not_found};
find_matching_receipt(Dir, [File | Rest], Stage, SkuId) ->
    FullPath = filename:join(Dir, File),
    case file:read_file(FullPath) of
        {ok, JsonBin} ->
            Receipt = jsx:decode(JsonBin, [return_maps]),
            ReceiptStage = erlang:atom_to_binary(maps:get(<<"stage">>, Receipt, <<>>)),
            ReceiptSku = maps:get(<<"sku_id">>, Receipt, <<>>),

            case {ReceiptStage, ReceiptSku} of
                {Stage, SkuId} -> {ok, Receipt};
                _ -> find_matching_receipt(Dir, Rest, Stage, SkuId)
            end;
        _ ->
            find_matching_receipt(Dir, Rest, Stage, SkuId)
    end.

find_all_receipts(ReceiptsDir, SkuId) ->
    case file:list_dir(ReceiptsDir) of
        {ok, Files} ->
            JsonFiles = [F || F <- Files, filename:extension(F) =:= ".json"],
            Receipts = lists:filtermap(fun(File) ->
                FullPath = filename:join(ReceiptsDir, File),
                case file:read_file(FullPath) of
                    {ok, JsonBin} ->
                        Receipt = jsx:decode(JsonBin, [return_maps]),
                        case maps:get(<<"sku_id">>, Receipt, undefined) of
                            SkuId -> {true, Receipt};
                            _ -> false
                        end;
                    _ ->
                        false
                end
            end, JsonFiles),

            % Sort by timestamp
            Sorted = lists:keysort(2, [{R, timestamp_to_int(R)} || R <- Receipts]),
            {ok, [R || {R, _} <- Sorted]};
        {error, Reason} ->
            {error, Reason}
    end.

timestamp_to_int(Receipt) ->
    case maps:get(<<"timestamp">>, Receipt, 0) of
        T when is_integer(T) -> T;
        _ -> 0
    end.

format_receipt_summary(Receipt) ->
    #{
        receipt_id => maps:get(<<"receipt_id">>, Receipt, <<"N/A">>),
        receipt_type => maps:get(<<"receipt_type">>, Receipt, <<"unknown">>),
        sku_id => maps:get(<<"sku_id">>, Receipt, <<"N/A">>),
        stage => maps:get(<<"stage">>, Receipt, <<"N/A">>),
        timestamp => maps:get(<<"timestamp_iso">>, Receipt, <<"N/A">>)
    }.

print_receipt_details(Receipt) ->
    io:format("~n"),
    io:format("Receipt Details:~n"),
    io:format("================~n"),
    maps:foreach(fun(Key, Value) ->
        io:format("~s: ~p~n", [Key, Value])
    end, Receipt),
    io:format("~n").

print_receipt_graph(Receipts, SkuId) ->
    io:format("~n"),
    io:format("Receipt Chain for SKU: ~s~n", [SkuId]),
    io:format("========================~s~n", [lists:duplicate(byte_size(SkuId), $=)]),
    io:format("~n"),

    lists:foreach(fun(Receipt) ->
        ReceiptType = maps:get(<<"receipt_type">>, Receipt, <<"unknown">>),
        Stage = maps:get(<<"stage">>, Receipt, <<"N/A">>),
        Timestamp = maps:get(<<"timestamp_iso">>, Receipt, <<"N/A">>),

        io:format("  [~s]~n", [Stage]),
        io:format("    └─ ~s~n", [ReceiptType]),
        io:format("       @ ~s~n", [Timestamp]),
        io:format("    |~n")
    end, Receipts),

    io:format("  END~n~n").

atom_to_binary(Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);
atom_to_binary(Bin) when is_binary(Bin) ->
    Bin.
