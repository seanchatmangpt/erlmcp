#!/usr/bin/env escript
%% Extract OTP application entrypoints from .app.src files
%% Output: entrypoints.json with applications, mods, dependencies, entry supervisors

main(_Args) ->
    ProjectRoot = find_project_root(),
    io:format("Analyzing OTP applications in ~s~n", [ProjectRoot]),

    %% Find all .app.src files
    AppFiles = find_app_files(ProjectRoot),
    io:format("Found ~w application files~n", [length(AppFiles)]),

    %% Extract metadata from each application
    Applications = lists:map(
        fun(AppFile) -> extract_app_metadata(ProjectRoot, AppFile) end,
        AppFiles
    ),

    %% Filter to main erlmcp app (exclude build artifacts)
    MainApps = [App || App <- Applications, should_include(App)],

    %% Format and write output
    Output = format_json(MainApps),
    OutputFile = filename:join([ProjectRoot, "tools", "v2_arch", "entrypoints.json"]),
    ok = file:write_file(OutputFile, Output),
    io:format("Wrote: ~s~n", [OutputFile]),

    %% Also write human-readable summary
    write_summary(OutputFile, MainApps),

    halt(0).

%% Find project root (erlmcp directory containing rebar.config)
find_project_root() ->
    case file:get_cwd() of
        {ok, Cwd} ->
            case filelib:is_file(filename:join(Cwd, "rebar.config")) of
                true -> Cwd;
                false ->
                    Parent = filename:dirname(Cwd),
                    case filelib:is_file(filename:join(Parent, "rebar.config")) of
                        true -> Parent;
                        false -> Cwd
                    end
            end;
        {error, _} -> "."
    end.

%% Find all .app.src files (but only in src/, not _build/)
find_app_files(ProjectRoot) ->
    SrcDir = filename:join(ProjectRoot, "src"),
    case filelib:wildcard(filename:join(SrcDir, "*.app.src")) of
        [] -> [];
        Files -> Files
    end.

%% Should we include this app in output? (not test/build artifacts)
should_include(App) ->
    case lists:keyfind(<<"name">>, 1, App) of
        {_, Name} ->
            NameStr = binary_to_list(Name),
            % Only include erlmcp and its immediate dependencies, not test plugins
            not lists:member(NameStr, [
                "proper", "meck", "coveralls", "rebar3_hex", "rebar3_proper",
                "hex_core", "verl", "covertool"
            ]);
        false -> true
    end.

%% Extract metadata from a single .app.src file
extract_app_metadata(ProjectRoot, AppFile) ->
    case file:consult(AppFile) of
        {ok, [{application, Name, Props}]} ->
            Mod = proplists:get_value(mod, Props, undefined),
            Apps = proplists:get_value(applications, Props, []),
            IncludedApps = proplists:get_value(included_applications, Props, []),

            {ModuleInfo, EntrySupervisor} = case Mod of
                undefined ->
                    {null, null};
                {ModName, ModArgs} ->
                    ModInfo = [
                        {<<"module">>, atom_to_binary(ModName)},
                        {<<"args">>, format_args(ModArgs)},
                        {<<"file">>, list_to_binary(find_module_file(ProjectRoot, ModName))}
                    ],
                    % The entry point starts erlmcp_sup - look for it
                    SupName = erlmcp_sup,
                    SupFile = find_module_file(ProjectRoot, SupName),
                    EntrySupv = case SupFile of
                        "unknown" -> null;
                        _ -> atom_to_binary(SupName)
                    end,
                    {ModInfo, EntrySupv};
                _ ->
                    {null, null}
            end,

            DependencyApps = lists:usort(Apps ++ IncludedApps),

            [
                {<<"name">>, atom_to_binary(Name)},
                {<<"mod">>, ModuleInfo},
                {<<"dependencies">>, [atom_to_binary(App) || App <- DependencyApps]},
                {<<"included_applications">>, [atom_to_binary(App) || App <- IncludedApps]},
                {<<"entry_supervisor">>, EntrySupervisor},
                {<<"app_file">>, list_to_binary(AppFile)}
            ];
        {error, Reason} ->
            io:format("ERROR reading ~s: ~w~n", [AppFile, Reason]),
            []
    end.

%% Find the module file by searching src/
find_module_file(ProjectRoot, ModName) ->
    SrcDir = filename:join(ProjectRoot, "src"),
    ModFile = filename:join(SrcDir, atom_to_list(ModName) ++ ".erl"),
    case filelib:is_file(ModFile) of
        true -> ModFile;
        false -> "unknown"
    end.

%% Format module arguments
format_args(Args) when is_list(Args) ->
    case length(Args) of
        0 -> <<"[]">>;
        _ -> list_to_binary(io_lib:format("~w", [Args]))
    end;
format_args(_) ->
    <<"unknown">>.

%% Simple JSON formatter (no external deps)
format_json(Apps) ->
    Timestamp = iso8601_now(),
    AppList = [
        io_lib:format("    ~s~n", [format_app_json(App)])
        || App <- Apps
    ],
    AppStr = string:join(AppList, ",\n"),

    io_lib:format(
        "{\n"
        "  \"timestamp\": \"~s\",\n"
        "  \"applications\": [\n~s\n  ]\n"
        "}\n",
        [Timestamp, AppStr]
    ).

format_app_json(App) ->
    Name = proplists:get_value(<<"name">>, App, <<"unknown">>),
    Mod = proplists:get_value(<<"mod">>, App, null),
    Deps = proplists:get_value(<<"dependencies">>, App, []),
    EntrySup = proplists:get_value(<<"entry_supervisor">>, App, null),

    ModStr = case Mod of
        null -> "null";
        _ -> format_value_json(Mod)
    end,

    SupStr = case EntrySup of
        null -> "null";
        _ -> format_value_json(EntrySup)
    end,

    DepStr = format_list_json(Deps),

    io_lib:format(
        "{\n"
        "      \"name\": \"~s\",\n"
        "      \"mod\": ~s,\n"
        "      \"entry_supervisor\": ~s,\n"
        "      \"dependencies\": ~s\n"
        "    }",
        [binary_to_list(Name), ModStr, SupStr, DepStr]
    ).

format_value_json(Value) when is_binary(Value) ->
    io_lib:format("\"~s\"", [binary_to_list(Value)]);
format_value_json(Value) when is_list(Value) ->
    case is_proplist(Value) of
        true ->
            Pairs = [
                io_lib:format("\"~s\": ~s", [
                    binary_to_list(K),
                    format_value_json(V)
                ])
                || {K, V} <- Value
            ],
            io_lib:format("{ ~s }", [string:join(Pairs, ", ")]);
        false ->
            format_list_json(Value)
    end;
format_value_json(null) ->
    "null";
format_value_json(V) ->
    io_lib:format("\"~w\"", [V]).

is_proplist(List) when is_list(List) ->
    case List of
        [] -> true;
        [{K, _}|_] when is_binary(K) -> true;
        _ -> false
    end;
is_proplist(_) -> false.

format_list_json(List) ->
    Items = [format_value_json(Item) || Item <- List],
    io_lib:format("[~s]", [string:join(Items, ", ")]).

%% ISO8601 timestamp
iso8601_now() ->
    Now = erlang:timestamp(),
    {{Y, M, D}, {H, Mi, S}} = calendar:now_to_universal_time(Now),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Y, M, D, H, Mi, S]).

%% Write human-readable summary
write_summary(JsonFile, Apps) ->
    SummaryFile = filename:rootname(JsonFile) ++ ".txt",
    Summary = format_summary(Apps),
    ok = file:write_file(SummaryFile, Summary),
    io:format("Wrote summary: ~s~n", [SummaryFile]).

format_summary(Apps) ->
    Header = "=== OTP APPLICATIONS - ENTRYPOINTS ===\n\n",
    Body = lists:map(fun format_app_summary/1, Apps),
    Header ++ string:join(Body, "").

format_app_summary(App) ->
    Name = case lists:keyfind(<<"name">>, 1, App) of
        {_, N} -> binary_to_list(N);
        false -> "unknown"
    end,

    Mod = case lists:keyfind(<<"mod">>, 1, App) of
        {_, null} -> "none";
        {_, M} when is_list(M) ->
            case lists:keyfind(<<"module">>, 1, M) of
                {_, ModName} -> binary_to_list(ModName) ++ "/2";
                false -> "none"
            end;
        false -> "none"
    end,

    SupStr = case lists:keyfind(<<"entry_supervisor">>, 1, App) of
        {_, null} -> "(not found)";
        {_, S} -> binary_to_list(S);
        false -> "(not found)"
    end,

    Deps = case lists:keyfind(<<"dependencies">>, 1, App) of
        {_, []} -> "(none)";
        {_, D} -> string:join([binary_to_list(X) || X <- D], ", ");
        false -> "(none)"
    end,

    io_lib:format(
        "Application: ~s~n"
        "  Entry Module:      ~s~n"
        "  Entry Supervisor:  ~s~n"
        "  Dependencies:      ~s~n~n",
        [Name, Mod, SupStr, Deps]
    ).
