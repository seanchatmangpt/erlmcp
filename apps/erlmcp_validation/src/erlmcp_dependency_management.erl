%%%-------------------------------------------------------------------
%%% @doc
%%% Dependency Management Support (FM-12)
%%%
%%% Provides dependency freshness checking, update recommendations,
%%% and vulnerable version tracking to support vulnerability scanning.
%%%
%%% == Features ==
%%%
%%% - Dependency freshness checking (query Hex.pm for latest versions)
%%% - Outdated dependency detection
%%% - Safe version recommendations (versions without CVEs)
%%% - Vulnerable version tracking
%%% - Transitive dependency analysis
%%% - Update prioritization by severity
%%%
%%% == API ==
%%%
%%% - check_freshness/2 : Check if dependency is latest version
%%% - detect_outdated/2 : Find outdated dependencies
%%% - recommend_safe_version/2 : Recommend safe version for vulnerable dep
%%% - is_vulnerable_version/3 : Check if version is known vulnerable
%%% - analyze_transitive/1 : Analyze transitive dependency tree
%%% - prioritize_updates/1 : Prioritize updates by severity
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_dependency_management).

%% API exports
-export([
    check_freshness/2,
    detect_outdated/2,
    recommend_safe_version/2,
    is_vulnerable_version/3,
    analyze_transitive/1,
    prioritize_updates/1
]).

-include_lib("kernel/include/logger.hrl").

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Check dependency freshness (is it the latest version?)
-spec check_freshness(Dependency :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
check_freshness(Dependency, #{mock := true} = Opts) ->
    %% Mock mode for testing
    LatestVersion = maps:get(latest_version, Opts),
    ReleaseDate = maps:get(release_date, Opts, <<"2024-01-01">>),
    CurrentVersion = maps:get(version, Dependency),

    IsLatest = (CurrentVersion =:= LatestVersion),

    {ok, #{
        is_latest => IsLatest,
        current_version => CurrentVersion,
        latest_version => LatestVersion,
        release_date => ReleaseDate
    }};
check_freshness(Dependency, _Opts) ->
    %% Query Hex.pm API for latest version
    Name = binary_to_list(maps:get(name, Dependency)),
    CurrentVersion = maps:get(version, Dependency),

    case query_hex_pm(Name) of
        {ok, #{<<"latest_version">> := LatestVersion, <<"release_date">> := ReleaseDate}} ->
            IsLatest = (CurrentVersion =:= LatestVersion),
            {ok, #{
                is_latest => IsLatest,
                current_version => CurrentVersion,
                latest_version => LatestVersion,
                release_date => ReleaseDate
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Detect outdated dependencies
-spec detect_outdated(Dependencies :: list(map()), Opts :: map()) -> list(map()).
detect_outdated(Dependencies, #{mock := true} = Opts) ->
    %% Mock mode - use provided latest versions
    LatestVersions = maps:get(latest_versions, Opts),

    lists:filtermap(fun(Dep) ->
        Name = maps:get(name, Dep),
        CurrentVersion = maps:get(version, Dep),
        case maps:get(Name, LatestVersions, CurrentVersion) of
            LatestVersion when LatestVersion =/= CurrentVersion ->
                {true, Dep#{
                    is_outdated => true,
                    latest_version => LatestVersion
                }};
            _ ->
                false
        end
    end, Dependencies);
detect_outdated(Dependencies, _Opts) ->
    %% Real mode - query Hex.pm for each
    lists:filtermap(fun(Dep) ->
        case check_freshness(Dep, #{}) of
            {ok, #{is_latest := false, latest_version := LatestVersion}} ->
                {true, Dep#{
                    is_outdated => true,
                    latest_version => LatestVersion
                }};
            _ ->
                false
        end
    end, Dependencies).

%% @doc Recommend safe version for vulnerable dependency
-spec recommend_safe_version(Dependency :: map(), Opts :: map()) ->
    {ok, map()} | {error, term()}.
recommend_safe_version(Dependency, #{mock := true} = Opts) ->
    %% Mock mode - use provided safe versions
    SafeVersions = maps:get(safe_versions, Opts),
    [RecommendedVersion | _] = SafeVersions,

    CVEs = maps:get(cves, Dependency),
    CVEIDs = [maps:get(cve_id, CVE) || CVE <- CVEs],

    {ok, #{
        recommended_version => RecommendedVersion,
        fixes_cves => CVEIDs,
        available_safe_versions => SafeVersions
    }};
recommend_safe_version(Dependency, _Opts) ->
    %% Real mode - query Hex.pm for versions and cross-reference with CVEs
    Name = binary_to_list(maps:get(name, Dependency)),
    CVEs = maps:get(cves, Dependency),

    case query_hex_pm_versions(Name) of
        {ok, Versions} ->
            %% Filter out vulnerable versions
            SafeVersions = filter_safe_versions(Versions, CVEs),

            case SafeVersions of
                [] ->
                    {error, no_safe_version_found};
                [RecommendedVersion | _] ->
                    CVEIDs = [maps:get(cve_id, CVE) || CVE <- CVEs],
                    {ok, #{
                        recommended_version => RecommendedVersion,
                        fixes_cves => CVEIDs,
                        available_safe_versions => SafeVersions
                    }}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Check if version is known vulnerable
-spec is_vulnerable_version(PackageName :: binary(), Version :: binary(),
                           VulnerableVersions :: map()) -> boolean().
is_vulnerable_version(PackageName, Version, VulnerableVersions) ->
    case maps:get(PackageName, VulnerableVersions, []) of
        [] ->
            false;
        Vulnerabilities ->
            lists:any(fun(Vuln) ->
                maps:get(version, Vuln) =:= Version
            end, Vulnerabilities)
    end.

%% @doc Analyze transitive dependency tree
-spec analyze_transitive(DependencyTree :: map()) -> map().
analyze_transitive(DependencyTree) ->
    %% Flatten tree to count total dependencies
    AllDeps = flatten_dependency_tree(DependencyTree),

    %% Find max depth
    MaxDepth = lists:max([maps:get(depth, Dep) || Dep <- AllDeps]),

    %% Count by depth
    DepthCounts = lists:foldl(fun(Dep, Acc) ->
        Depth = maps:get(depth, Dep),
        maps:update_with(Depth, fun(Count) -> Count + 1 end, 1, Acc)
    end, #{}, AllDeps),

    #{
        total_dependencies => length(AllDeps),
        max_depth => MaxDepth,
        depth_distribution => DepthCounts,
        direct_dependencies => maps:get(1, DepthCounts, 0),
        transitive_dependencies => length(AllDeps) - maps:get(1, DepthCounts, 0)
    }.

%% @doc Prioritize updates by severity
-spec prioritize_updates(Vulnerabilities :: list(map())) -> list(map()).
prioritize_updates(Vulnerabilities) ->
    %% Sort by severity (critical > high > medium > low), then by CVSS score
    lists:sort(fun(A, B) ->
        SevA = severity_rank(maps:get(severity, A)),
        SevB = severity_rank(maps:get(severity, B)),
        case SevA =:= SevB of
            true ->
                %% Same severity, sort by CVSS score (descending)
                maps:get(cvss_score, A) >= maps:get(cvss_score, B);
            false ->
                %% Different severity, higher severity first
                SevA > SevB
        end
    end, Vulnerabilities).

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @private Query Hex.pm API for package info
query_hex_pm(PackageName) ->
    URL = "https://hex.pm/api/packages/" ++ PackageName,

    case httpc:request(get, {URL, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, JSONData} = jsx:decode(list_to_binary(Body), [return_maps]),

            %% Extract latest version
            Releases = maps:get(<<"releases">>, JSONData, []),
            case Releases of
                [Latest | _] ->
                    LatestVersion = maps:get(<<"version">>, Latest),
                    ReleaseDate = maps:get(<<"inserted_at">>, Latest),
                    {ok, #{
                        <<"latest_version">> => LatestVersion,
                        <<"release_date">> => ReleaseDate
                    }};
                [] ->
                    {error, no_releases_found}
            end;

        {ok, {{_, 404, _}, _, _}} ->
            {error, package_not_found};

        {ok, {{_, StatusCode, _}, _, Body}} ->
            ?LOG_WARNING("Hex.pm API returned ~p: ~s", [StatusCode, Body]),
            {error, {http_error, StatusCode}};

        {error, Reason} ->
            ?LOG_ERROR("Hex.pm API request failed: ~p", [Reason]),
            {error, Reason}
    end.

%% @private Query Hex.pm for all versions
query_hex_pm_versions(PackageName) ->
    URL = "https://hex.pm/api/packages/" ++ PackageName,

    case httpc:request(get, {URL, []}, [{timeout, 5000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, JSONData} = jsx:decode(list_to_binary(Body), [return_maps]),

            %% Extract all version numbers
            Releases = maps:get(<<"releases">>, JSONData, []),
            Versions = [maps:get(<<"version">>, R) || R <- Releases],

            {ok, Versions};

        {error, Reason} ->
            {error, Reason}
    end.

%% @private Filter versions that don't have CVEs
filter_safe_versions(Versions, CVEs) ->
    %% Extract affected versions from CVEs
    AffectedVersions = lists:flatten([
        maps:get(affected_versions, CVE, []) || CVE <- CVEs
    ]),

    %% Filter out affected versions
    lists:filter(fun(Version) ->
        not lists:member(Version, AffectedVersions)
    end, Versions).

%% @private Flatten dependency tree to list
flatten_dependency_tree(DependencyTree) ->
    lists:flatten([
        Deps || {_Parent, Deps} <- maps:to_list(DependencyTree)
    ]).

%% @private Assign numeric rank to severity for sorting
severity_rank(critical) -> 4;
severity_rank(high) -> 3;
severity_rank(medium) -> 2;
severity_rank(low) -> 1;
severity_rank(_) -> 0.
