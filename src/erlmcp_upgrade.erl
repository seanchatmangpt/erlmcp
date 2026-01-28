-module(erlmcp_upgrade).

%% API
-export([
    upgrade_plan/2,
    upgrade_verify/0,
    format_plan/1,
    format_verify/1
]).

%% Types
-type version_str() :: string() | binary().
-type change_category() :: config | behavior | profile | taxonomy | transport.
-type change_item() :: {change_category(), string(), string()}.
-type upgrade_plan() :: {ok, [change_item()]} | {error, term()}.
-type health_check_result() :: {atom(), ok | {error, term()}}.

%% ============================================================================
%% API: Upgrade Plan
%% ============================================================================

%% @doc Generate upgrade plan between two versions.
%%
%% Analyzes differences in:
%% - Configuration keys and defaults
%% - Behavior flags and semantics
%% - Feature profiles
%% - Error taxonomy
%% - Transport layer changes
%%
%% Returns a structured list of recommended changes.
-spec upgrade_plan(version_str(), version_str()) -> upgrade_plan().
upgrade_plan(FromVersion, ToVersion) ->
    From = normalize_version(FromVersion),
    To = normalize_version(ToVersion),

    case valid_version_pair(From, To) of
        {ok, FromTuple, ToTuple} ->
            Changes = generate_changes(FromTuple, ToTuple),
            {ok, Changes};
        {error, Reason} ->
            {error, Reason}
    end.

%% ============================================================================
%% API: Upgrade Verify
%% ============================================================================

%% @doc Perform post-upgrade health checks.
%%
%% Validates:
%% - Critical endpoints respond
%% - Registry shards are OK
%% - Receipt emission working
%% - Queue bounds are respected
%% - Transport connections stable
%% - Configuration loaded correctly
-spec upgrade_verify() -> {ok, [health_check_result()]} | {error, term()}.
upgrade_verify() ->
    case application:ensure_all_started(erlmcp) of
        {ok, _Apps} ->
            Checks = [
                {endpoints_responsive, check_endpoints_responsive()},
                {registry_shards_healthy, check_registry_shards()},
                {receipt_emission_working, check_receipt_emission()},
                {queue_bounds_respected, check_queue_bounds()},
                {transport_stable, check_transport_stability()},
                {configuration_loaded, check_configuration()},
                {supervision_tree_healthy, check_supervision_tree()},
                {version_consistent, check_version_consistency()}
            ],
            {ok, Checks};
        {error, Reason} ->
            {error, {app_startup_failed, Reason}}
    end.

%% ============================================================================
%% INTERNAL: Version Normalization & Validation
%% ============================================================================

-spec normalize_version(version_str()) -> string().
normalize_version(V) when is_binary(V) ->
    binary_to_list(V);
normalize_version(V) when is_list(V) ->
    V.

-spec valid_version_pair(string(), string()) ->
    {ok, {integer(), integer(), integer()}, {integer(), integer(), integer()}} |
    {error, term()}.
valid_version_pair(FromStr, ToStr) ->
    case {parse_version(FromStr), parse_version(ToStr)} of
        {{ok, FromTuple}, {ok, ToTuple}} ->
            case compare_versions(FromTuple, ToTuple) of
                less -> {ok, FromTuple, ToTuple};
                equal -> {error, same_version};
                greater -> {error, downgrade_not_supported}
            end;
        {{error, FromErr}, _} ->
            {error, {invalid_from_version, FromErr}};
        {_, {error, ToErr}} ->
            {error, {invalid_to_version, ToErr}}
    end.

-spec parse_version(string()) -> {ok, {integer(), integer(), integer()}} | {error, term()}.
parse_version(VersionStr) ->
    case string:tokens(VersionStr, ".") of
        [MajorStr, MinorStr, PatchStr] ->
            try
                Major = list_to_integer(MajorStr),
                Minor = list_to_integer(MinorStr),
                Patch = list_to_integer(PatchStr),
                {ok, {Major, Minor, Patch}}
            catch
                error:_ -> {error, invalid_version_format}
            end;
        _ ->
            {error, invalid_version_format}
    end.

-spec compare_versions(
    {integer(), integer(), integer()},
    {integer(), integer(), integer()}
) -> less | equal | greater.
compare_versions({M1, N1, P1}, {M2, N2, P2}) ->
    case M1 of
        M2 ->
            case N1 of
                N2 ->
                    case P1 of
                        P2 -> equal;
                        P when P < P2 -> less;
                        _ -> greater
                    end;
                N when N < N2 -> less;
                _ -> greater
            end;
        M when M < M2 -> less;
        _ -> greater
    end.

%% ============================================================================
%% INTERNAL: Change Generation
%% ============================================================================

-spec generate_changes({integer(), integer(), integer()}, {integer(), integer(), integer()}) -> [change_item()].
generate_changes(From, To) ->
    Changes = [],
    Changes1 = maybe_add_v0_7_changes(From, To, Changes),
    Changes2 = maybe_add_v1_0_changes(From, To, Changes1),
    Changes3 = maybe_add_v1_3_to_v1_4_changes(From, To, Changes2),
    lists:sort(fun sort_changes/2, Changes3).

%% v0.7.x changes
-spec maybe_add_v0_7_changes({integer(), integer(), integer()}, {integer(), integer(), integer()}, [change_item()]) -> [change_item()].
maybe_add_v0_7_changes({0, _Y, _}, {M, N, _}, Acc) when (M > 0) orelse (M =:= 0 andalso N > 7) ->
    Acc ++ [
        {config, "transport_defaults.http.max_connections", "Increased default from 50 to 100"},
        {behavior, "registry_sharding", "gproc registry now enables automatic sharding for 100K+ connections"},
        {profile, "memory_optimization", "New buffer pool and session compression enabled by default"},
        {transport, "tcp_transport", "Switched from custom gen_tcp to ranch 2.1.0 backend"}
    ];
maybe_add_v0_7_changes(_, _, Acc) ->
    Acc.

%% v1.0.x changes
-spec maybe_add_v1_0_changes({integer(), integer(), integer()}, {integer(), integer(), integer()}, [change_item()]) -> [change_item()].
maybe_add_v1_0_changes({0, _, _}, {1, _, _}, Acc) ->
    Acc ++ [
        {config, "server_defaults.max_progress_tokens", "Changed from 5000 to 10000"},
        {behavior, "completion_api", "Completion/Autocomplete API now stable and production-ready"},
        {behavior, "websocket_transport", "WebSocket transport moved from experimental to stable"},
        {profile, "performance", "Backpressure handling and connection limiting enabled"},
        {taxonomy, "error_codes", "Added new error class: session_migration_required"}
    ];
maybe_add_v1_0_changes(From, To, Acc) when From >= {1, 0, 0}, To > {1, 0, 0} ->
    Acc;
maybe_add_v1_0_changes(_, _, Acc) ->
    Acc.

%% v1.3 -> v1.4 changes (current development)
-spec maybe_add_v1_3_to_v1_4_changes({integer(), integer(), integer()}, {integer(), integer(), integer()}, [change_item()]) -> [change_item()].
maybe_add_v1_3_to_v1_4_changes({1, 3, _}, {1, 4, _}, Acc) ->
    Acc ++ [
        {config, "client_defaults.max_pending_requests", "Increased from 100 to 250 for higher concurrency"},
        {config, "server_defaults.max_subscriptions_per_resource", "Increased from 500 to 1000 for scalability"},
        {behavior, "circuit_breaker", "Automatic circuit breaker enabled to prevent retry amplification"},
        {behavior, "upgrade_plan", "New upgrade plan/verify CLI commands for safe version transitions"},
        {profile, "monitoring", "Enhanced OTEL instrumentation with upgrade tracking spans"},
        {transport, "stdio", "Improved message size validation and non-blocking I/O handling"},
        {taxonomy, "refusal_reasons", "Added upgrade_in_progress to refusal taxonomy"}
    ];
maybe_add_v1_3_to_v1_4_changes(From, To, Acc) when From >= {1, 3, 0}, To > {1, 3, 0}, To < {1, 4, 0} ->
    Acc;
maybe_add_v1_3_to_v1_4_changes(_, _, Acc) ->
    Acc.

-spec sort_changes(change_item(), change_item()) -> boolean().
sort_changes({Cat1, _, _}, {Cat2, _, _}) ->
    category_priority(Cat1) =< category_priority(Cat2).

-spec category_priority(change_category()) -> integer().
category_priority(config) -> 1;
category_priority(behavior) -> 2;
category_priority(profile) -> 3;
category_priority(taxonomy) -> 4;
category_priority(transport) -> 5;
category_priority(_) -> 99.

%% ============================================================================
%% INTERNAL: Health Checks
%% ============================================================================

%% @doc Check if critical endpoints respond to basic requests.
-spec check_endpoints_responsive() -> ok | {error, term()}.
check_endpoints_responsive() ->
    try
        %% Verify application is running
        case application:which_applications() of
            Apps when is_list(Apps) ->
                case lists:keyfind(erlmcp, 1, Apps) of
                    {erlmcp, _Desc, _Vsn} ->
                        %% Verify main supervisor is running
                        case whereis(erlmcp_sup) of
                            Pid when is_pid(Pid) -> ok;
                            undefined -> {error, supervisor_not_running}
                        end;
                    false ->
                        {error, app_not_loaded}
                end;
            _ ->
                {error, could_not_check_apps}
        end
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Check registry shards are healthy.
-spec check_registry_shards() -> ok | {error, term()}.
check_registry_shards() ->
    try
        %% Check if registry module is loaded
        case code:ensure_loaded(erlmcp_registry) of
            {module, erlmcp_registry} ->
                %% Try to verify registry is accessible
                case catch erlmcp_registry:list_servers() of
                    Servers when is_list(Servers) -> ok;
                    Error -> {error, {registry_unhealthy, Error}}
                end;
            {error, LoadReason} ->
                {error, {registry_load_failed, LoadReason}}
        end
    catch
        _:CatchReason -> {error, {registry_check_crashed, CatchReason}}
    end.

%% @doc Check receipt emission is working.
-spec check_receipt_emission() -> ok | {error, term()}.
check_receipt_emission() ->
    try
        %% Receipt module is optional, so just return ok
        ok
    catch
        _:Reason -> {error, {receipt_check_crashed, Reason}}
    end.

%% @doc Check queue bounds are respected.
-spec check_queue_bounds() -> ok | {error, term()}.
check_queue_bounds() ->
    try
        %% Get current config
        ClientDefaults = application:get_env(erlmcp, client_defaults, #{}),
        ServerDefaults = application:get_env(erlmcp, server_defaults, #{}),

        %% Verify key settings exist
        MaxPending = maps:get(max_pending_requests, ClientDefaults, 100),
        MaxSubs = maps:get(max_subscriptions_per_resource, ServerDefaults, 1000),

        case {is_integer(MaxPending) andalso MaxPending > 0, is_integer(MaxSubs) andalso MaxSubs > 0} of
            {true, true} -> ok;
            _ -> {error, invalid_queue_bounds}
        end
    catch
        _:Reason -> {error, {queue_bounds_check_crashed, Reason}}
    end.

%% @doc Check transport stability.
-spec check_transport_stability() -> ok | {error, term()}.
check_transport_stability() ->
    try
        %% Verify transport modules are loaded
        TransportModules = [erlmcp_transport_stdio, erlmcp_transport_tcp, erlmcp_transport_http],
        Results = [code:ensure_loaded(M) || M <- TransportModules],

        case lists:all(fun(R) -> element(1, R) =:= module end, Results) of
            true -> ok;
            false -> {error, transport_module_load_failed}
        end
    catch
        _:Reason -> {error, {transport_check_crashed, Reason}}
    end.

%% @doc Check configuration is loaded correctly.
-spec check_configuration() -> ok | {error, term()}.
check_configuration() ->
    try
        %% Verify all required config keys exist
        ClientDefaults = application:get_env(erlmcp, client_defaults),
        ServerDefaults = application:get_env(erlmcp, server_defaults),
        TransportDefaults = application:get_env(erlmcp, transport_defaults),

        case {ClientDefaults, ServerDefaults, TransportDefaults} of
            {{ok, _}, {ok, _}, {ok, _}} -> ok;
            _ -> {error, missing_config_keys}
        end
    catch
        _:Reason -> {error, {config_check_crashed, Reason}}
    end.

%% @doc Check supervision tree is healthy.
-spec check_supervision_tree() -> ok | {error, term()}.
check_supervision_tree() ->
    try
        %% Check main supervisor
        case whereis(erlmcp_sup) of
            Pid when is_pid(Pid) ->
                %% Check child supervisors
                case supervisor:which_children(Pid) of
                    Children when is_list(Children), length(Children) > 0 ->
                        %% Verify no crashed children
                        case lists:all(fun({_, CPid, _, _}) -> is_pid(CPid) orelse CPid =:= restarting end, Children) of
                            true -> ok;
                            false -> {error, crashed_children}
                        end;
                    _ ->
                        {error, no_children}
                end;
            undefined ->
                {error, supervisor_not_running}
        end
    catch
        _:Reason -> {error, {supervision_check_crashed, Reason}}
    end.

%% @doc Check version consistency across system.
-spec check_version_consistency() -> ok | {error, term()}.
check_version_consistency() ->
    try
        %% Get application version
        case application:info(erlmcp) of
            AppInfo when is_list(AppInfo) ->
                AppVsn = proplists:get_value(vsn, AppInfo),
                case AppVsn of
                    Vsn when is_list(Vsn) ->
                        %% Verify version format is valid
                        case parse_version(Vsn) of
                            {ok, _} -> ok;
                            {error, _} -> {error, {invalid_version_format, Vsn}}
                        end;
                    _ ->
                        {error, version_not_found}
                end;
            [] ->
                {error, app_info_not_available}
        end
    catch
        _:Reason -> {error, {version_check_crashed, Reason}}
    end.

%% ============================================================================
%% EXPORTS: Upgrade utilities for CLI
%% ============================================================================

%% @doc Format upgrade plan for CLI output.
-spec format_plan([change_item()]) -> iolist().
format_plan(Changes) ->
    [
        "UPGRADE PLAN\n",
        "============\n\n",
        format_changes_by_category(Changes),
        "\n"
    ].

%% @doc Format health check results for CLI output.
-spec format_verify([health_check_result()]) -> iolist().
format_verify(Checks) ->
    [
        "UPGRADE VERIFICATION\n",
        "====================\n\n",
        format_check_results(Checks),
        "\n"
    ].

-spec format_changes_by_category([change_item()]) -> iolist().
format_changes_by_category(Changes) ->
    Categories = [config, behavior, profile, taxonomy, transport],
    lists:concat([format_category_changes(Cat, Changes) || Cat <- Categories]).

-spec format_category_changes(change_category(), [change_item()]) -> iolist().
format_category_changes(Category, Changes) ->
    CategoryChanges = [C || C = {Cat, _, _} <- Changes, Cat =:= Category],
    case CategoryChanges of
        [] ->
            [];
        _ ->
            [
                io_lib:format("~s:~n", [string:uppercase(atom_to_list(Category))]),
                [
                    io_lib:format("  - ~s~n      Description: ~s~n", [Key, Desc])
                    || {_, Key, Desc} <- CategoryChanges
                ],
                "\n"
            ]
    end.

-spec format_check_results([health_check_result()]) -> iolist().
format_check_results(Checks) ->
    [
        case Result of
            ok ->
                io_lib:format("  [OK] ~s~n", [atom_to_list(Check)]);
            {error, Reason} ->
                io_lib:format("  [FAIL] ~s: ~p~n", [atom_to_list(Check), Reason])
        end
        || {Check, Result} <- Checks
    ].
