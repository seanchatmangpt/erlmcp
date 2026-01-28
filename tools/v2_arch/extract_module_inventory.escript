#!/usr/bin/env escript
%% -*- erlang -*-
%%! -smp enable -scl false

%% Module Inventory Extractor for erlmcp v2
%% Enumerates all modules, detects duplicates/broken files, classifies, extracts exports
%% Output: inventory.json

-mode(compile).

main([]) ->
    SrcDir = "src",
    case filelib:is_dir(SrcDir) of
        false ->
            io:format("Error: ~s directory not found~n", [SrcDir]),
            halt(1);
        true ->
            Inventory = extract_inventory(SrcDir),
            Output = "inventory.json",
            case write_json(Output, Inventory) of
                ok ->
                    io:format("✓ Inventory written to ~s~n", [Output]),
                    io:format("  Total modules: ~w~n", [maps:get(total_modules, Inventory)]),
                    io:format("  Families: ~w~n", [maps:get(num_families, Inventory)]),
                    io:format("  Duplicates: ~w~n", [maps:get(num_duplicates, Inventory)]),
                    io:format("  Broken: ~w~n", [maps:get(num_broken, Inventory)]),
                    halt(0);
                {error, Reason} ->
                    io:format("Error writing ~s: ~p~n", [Output, Reason]),
                    halt(1)
            end
    end.

extract_inventory(SrcDir) ->
    AllFiles = filelib:wildcard(SrcDir ++ "/**/*.erl"),
    {Active, Broken, Duplicates} = categorize_files(AllFiles),
    Modules = lists:map(fun analyze_module/1, Active),
    Families = group_by_family(Modules),

    #{
        timestamp => erlang:system_time(second),
        src_directory => SrcDir,
        total_modules => length(Active),
        num_families => maps:size(Families),
        num_broken => length(Broken),
        num_duplicates => length(Duplicates),
        families => Families,
        broken => Broken,
        duplicates => Duplicates,
        all_files => #{
            active => length(Active),
            broken => length(Broken),
            duplicates => length(Duplicates)
        }
    }.

categorize_files(Files) ->
    categorize_files(Files, {[], [], []}).

categorize_files([], Acc) ->
    Acc;
categorize_files([File | Rest], {Active, Broken, Dups}) ->
    case classify_file(File) of
        active ->
            categorize_files(Rest, {[File | Active], Broken, Dups});
        broken ->
            categorize_files(Rest, {Active, [File | Broken], Dups});
        duplicate ->
            categorize_files(Rest, {Active, Broken, [File | Dups]})
    end.

classify_file(File) ->
    BaseName = filename:basename(File),
    case string:find(BaseName, ".broken") of
        nomatch ->
            case string:find(BaseName, ".backup") of
                nomatch ->
                    case string:find(BaseName, ".bak") of
                        nomatch ->
                            case is_duplicate_variant(BaseName) of
                                true -> duplicate;
                                false -> active
                            end;
                        _ -> duplicate
                    end;
                _ -> duplicate
            end;
        _ -> broken
    end.

is_duplicate_variant(FileName) ->
    Variants = ["_new", "_refactored", "_copy", "_old"],
    lists:any(fun(Suffix) ->
        string:find(FileName, Suffix) =/= nomatch
    end, Variants).

analyze_module(File) ->
    Module = filename:basename(File, ".erl"),
    Exports = extract_exports(File),
    Size = filelib:file_size(File),
    Family = extract_family(Module),

    #{
        file => File,
        module => Module,
        family => Family,
        exports => Exports,
        num_exports => length(Exports),
        size_bytes => Size
    }.

extract_family(Module) ->
    case extract_family_internal(Module) of
        Family -> Family
    end.

extract_family_internal("erlmcp") ->
    "core";
extract_family_internal("rdf_utils") ->
    "utilities";
extract_family_internal(M) ->
    case string:find(M, "tcps") of
        M -> "tcps";
        _ ->
            case string:find(M, "erlmcp_transport_") of
                nomatch ->
                    case string:find(M, "erlmcp_cli_") of
                        nomatch ->
                            case string:find(M, "erlmcp_metrics") of
                                nomatch -> classify_rest(M);
                                _ -> "metrics"
                            end;
                        _ -> "cli"
                    end;
                _ -> "transport"
            end
    end.

classify_rest(M) ->
    case string:find(M, "erlmcp_monitor") of
        nomatch ->
            case string:find(M, "erlmcp_pricing") of
                nomatch ->
                    case string:find(M, "erlmcp_config") of
                        nomatch -> classify_rest2(M);
                        _ -> "configuration"
                    end;
                _ -> "pricing"
            end;
        _ -> "monitoring"
    end.

classify_rest2(M) ->
    case string:find(M, "erlmcp_server") of
        nomatch ->
            case string:find(M, "erlmcp_client") of
                nomatch -> classify_rest3(M);
                _ -> "client"
            end;
        _ -> "server"
    end.

classify_rest3(M) ->
    case string:find(M, "erlmcp_circuit") of
        nomatch ->
            case string:find(M, "erlmcp_backpressure") of
                nomatch ->
                    case string:find(M, "erlmcp_graceful") of
                        nomatch ->
                            case string:find(M, "erlmcp_rate_limiter") of
                                nomatch -> classify_rest4(M);
                                _ -> "resilience"
                            end;
                        _ -> "resilience"
                    end;
                _ -> "resilience"
            end;
        _ -> "resilience"
    end.

classify_rest4(M) ->
    case string:find(M, "erlmcp_http") of
        nomatch ->
            case string:find(M, "erlmcp_health") of
                nomatch ->
                    case string:find(M, "erlmcp_pool") of
                        nomatch ->
                            case string:find(M, "erlmcp_buffer") of
                                nomatch -> classify_rest5(M);
                                _ -> "pooling"
                            end;
                        _ -> "pooling"
                    end;
                _ -> "health"
            end;
        _ -> "http"
    end.

classify_rest5(M) ->
    case string:find(M, "erlmcp_memory") of
        nomatch ->
            case string:find(M, "erlmcp_profile") of
                nomatch ->
                    case string:find(M, "erlmcp_bench") of
                        nomatch ->
                            case string:find(M, "erlmcp_queue") of
                                nomatch -> classify_rest6(M);
                                _ -> "queuing"
                            end;
                        _ -> "benchmarking"
                    end;
                _ -> "profiling"
            end;
        _ -> "memory"
    end.

classify_rest6(M) ->
    case string:find(M, "erlmcp_receipt") of
        nomatch ->
            case string:find(M, "erlmcp_registry") of
                nomatch ->
                    case string:find(M, "erlmcp_session") of
                        nomatch ->
                            case string:find(M, "erlmcp_routing") of
                                nomatch ->
                                    case string:find(M, "erlmcp_json_rpc") of
                                        nomatch -> classify_rest7(M);
                                        _ -> "json_rpc"
                                    end;
                                _ -> "routing"
                            end;
                        _ -> "sessions"
                    end;
                _ -> "registry"
            end;
        _ -> "receipts"
    end.

classify_rest7(M) ->
    case string:find(M, "erlmcp_logging") of
        nomatch ->
            case string:find(M, "erlmcp_trace") of
                nomatch ->
                    case string:find(M, "erlmcp_report") of
                        nomatch ->
                            case string:find(M, "erlmcp_plan") of
                                nomatch ->
                                    case string:find(M, "erlmcp_sup") of
                                        nomatch -> "utilities";
                                        _ -> "supervision"
                                    end;
                                _ -> "planning"
                            end;
                        _ -> "reporting"
                    end;
                _ -> "tracing"
            end;
        _ -> "logging"
    end.

extract_exports(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            extract_exports_from_content(binary_to_list(Content));
        {error, _} ->
            []
    end.

extract_exports_from_content(Content) ->
    Lines = string:split(Content, "\n", all),
    ExportLines = [L || L <- Lines, string:find(L, "-export") =/= nomatch],
    ExtractFuns = fun(Line) ->
        case re:run(Line, "-export\\(\\[(.+)\\]\\)", [{capture, [1], list}]) of
            {match, [Funs]} ->
                parse_function_names(Funs);
            _ ->
                []
        end
    end,
    lists:append(lists:map(ExtractFuns, ExportLines)).

parse_function_names(FunString) ->
    case re:split(FunString, ",", [{return, list}]) of
        Funs ->
            [string:trim(F) || F <- Funs, string:trim(F) =/= ""];
        _ ->
            []
    end.

group_by_family(Modules) ->
    lists:foldl(fun(Module, Acc) ->
        Family = maps:get(family, Module),
        CurrentFamily = maps:get(Family, Acc, []),
        maps:put(Family, [Module | CurrentFamily], Acc)
    end, #{}, Modules).

write_json(File, Inventory) ->
    try
        JSON = inventory_to_json(Inventory),
        file:write_file(File, JSON)
    catch
        _:Reason ->
            {error, Reason}
    end.

inventory_to_json(Inventory) ->
    JSON = maps:to_list(Inventory),
    encode_json(JSON, 0).

encode_json(List, Indent) when is_list(List) ->
    case lists:all(fun is_tuple/1, List) of
        true ->
            encode_object(List, Indent);
        false ->
            encode_array(List, Indent)
    end;
encode_json(Map, Indent) when is_map(Map) ->
    encode_object(maps:to_list(Map), Indent);
encode_json(String, _) when is_list(String) ->
    encode_string(String);
encode_json(Atom, _) when is_atom(Atom) ->
    encode_string(atom_to_list(Atom));
encode_json(Int, _) when is_integer(Int) ->
    integer_to_list(Int);
encode_json(Float, _) when is_float(Float) ->
    float_to_list(Float);
encode_json(true, _) -> "true";
encode_json(false, _) -> "false";
encode_json(null, _) -> "null".

encode_object([], _Indent) ->
    "{}";
encode_object(List, Indent) ->
    Pairs = [encode_pair(K, V, Indent + 2) || {K, V} <- List],
    "{\n" ++ string:join(Pairs, ",\n") ++ "\n" ++ indent(Indent) ++ "}".

encode_pair(Key, Value, Indent) ->
    KeyStr = encode_json(Key, 0),
    ValueStr = encode_json(Value, Indent),
    indent(Indent) ++ KeyStr ++ ": " ++ ValueStr.

encode_array([], _Indent) ->
    "[]";
encode_array(List, Indent) ->
    Items = [encode_json(Item, Indent + 2) || Item <- List],
    "[\n" ++ indent(Indent + 2) ++ string:join(Items, ",\n" ++ indent(Indent + 2)) ++ "\n" ++ indent(Indent) ++ "]".

encode_string(String) ->
    "\"" ++ escape_json_string(String) ++ "\"".

escape_json_string(String) ->
    Escaped = string:replace(String, "\"", "\\\"", all),
    string:replace(Escaped, "\n", "\\n", all).

indent(N) ->
    string:chars(32, N).
