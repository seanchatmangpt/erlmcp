%% Auto-generated version module - Updated by release tools
%% This module provides runtime access to version information
-module(erlmcp_version).

%% Public API
-export([
    version/0,
    version_string/0,
    major/0,
    minor/0,
    patch/0,
    pre_release/0,
    is_prerelease/0,
    compare/2,
    info/0
]).

%% =============================================================================
%% Version Constants
%% =============================================================================

-define(VERSION_MAJOR, 1).
-define(VERSION_MINOR, 0).
-define(VERSION_PATCH, 0).
-define(VERSION_PRERELEASE, "").  %% "" for stable, "beta.1", "rc.1", etc.

%% =============================================================================
%% Public API
%% =============================================================================

%% @doc Get version as a tuple {Major, Minor, Patch, Prerelease}
%%
%% Examples:
%% ```
%% > erlmcp_version:version().
%% {1, 0, 0, ""}
%%
%% > erlmcp_version:version().
%% {1, 2, 0, "beta.1"}
%% '''
-spec version() -> {Major::non_neg_integer(),
                     Minor::non_neg_integer(),
                     Patch::non_neg_integer(),
                     PreRelease::string()}.
version() ->
    {?VERSION_MAJOR, ?VERSION_MINOR, ?VERSION_PATCH, ?VERSION_PRERELEASE}.

%% @doc Get version as a semantic version string
%%
%% Examples:
%% ```
%% > erlmcp_version:version_string().
%% "1.0.0"
%%
%% > erlmcp_version:version_string().
%% "1.2.0-beta.1"
%% '''
-spec version_string() -> string().
version_string() ->
    Base = io_lib:format("~w.~w.~w", [?VERSION_MAJOR, ?VERSION_MINOR, ?VERSION_PATCH]),
    case ?VERSION_PRERELEASE of
        "" -> lists:concat(Base);
        Pre -> lists:concat([Base, "-", Pre])
    end.

%% @doc Get major version number (breaking changes)
-spec major() -> non_neg_integer().
major() ->
    ?VERSION_MAJOR.

%% @doc Get minor version number (new features)
-spec minor() -> non_neg_integer().
minor() ->
    ?VERSION_MINOR.

%% @doc Get patch version number (bug fixes)
-spec patch() -> non_neg_integer().
patch() ->
    ?VERSION_PATCH.

%% @doc Get pre-release identifier (empty string for stable releases)
-spec pre_release() -> string().
pre_release() ->
    ?VERSION_PRERELEASE.

%% @doc Check if this is a pre-release version
%%
%% Returns true if version contains pre-release identifier (alpha, beta, rc, etc.)
-spec is_prerelease() -> boolean().
is_prerelease() ->
    ?VERSION_PRERELEASE /= "".

%% @doc Compare two version strings according to semantic versioning rules
%%
%% Returns:
%% - lt: first version is less than second
%% - eq: versions are equal
%% - gt: first version is greater than second
%%
%% Examples:
%% ```
%% > erlmcp_version:compare("1.0.0", "1.1.0").
%% lt
%%
%% > erlmcp_version:compare("1.2.0-alpha", "1.2.0-beta").
%% lt
%%
%% > erlmcp_version:compare("1.0.0", "1.0.0").
%% eq
%% '''
-spec compare(VersionA::string(), VersionB::string()) -> lt | eq | gt.
compare(VersionA, VersionB) ->
    ParsedA = parse_version(VersionA),
    ParsedB = parse_version(VersionB),
    compare_parsed(ParsedA, ParsedB).

%% @doc Get complete version information as a map
%%
%% Returns a map with all version details useful for logging and diagnostics
-spec info() -> map().
info() ->
    {Major, Minor, Patch, PreRelease} = version(),
    #{
        version => version_string(),
        major => Major,
        minor => Minor,
        patch => Patch,
        prerelease => PreRelease,
        is_prerelease => is_prerelease(),
        otp_version => erlang:system_info(otp_release),
        erlang_version => erlang:system_info(version),
        nif_version => erlang:system_info(nif_version)
    }.

%% =============================================================================
%% Internal Functions
%% =============================================================================

%% Parse semantic version string into tuple
-spec parse_version(string()) -> {non_neg_integer(),
                                   non_neg_integer(),
                                   non_neg_integer(),
                                   string()}.
parse_version(VersionStr) ->
    % Remove 'v' prefix if present
    Clean = case VersionStr of
        "v" ++ Rest -> Rest;
        _ -> VersionStr
    end,

    % Split on '-' to separate prerelease
    case string:split(Clean, "-") of
        [VersionPart, PreRelease] ->
            [MajorStr, MinorStr, PatchStr] = string:split(VersionPart, ".", all),
            Major = erlang:binary_to_integer(binary:list_to_bin(MajorStr)),
            Minor = erlang:binary_to_integer(binary:list_to_bin(MinorStr)),
            Patch = erlang:binary_to_integer(binary:list_to_bin(PatchStr)),
            {Major, Minor, Patch, PreRelease};
        [VersionPart] ->
            [MajorStr, MinorStr, PatchStr] = string:split(VersionPart, ".", all),
            Major = erlang:binary_to_integer(binary:list_to_bin(MajorStr)),
            Minor = erlang:binary_to_integer(binary:list_to_bin(MinorStr)),
            Patch = erlang:binary_to_integer(binary:list_to_bin(PatchStr)),
            {Major, Minor, Patch, ""}
    end.

%% Compare two parsed versions
-spec compare_parsed(tuple(), tuple()) -> lt | eq | gt.
compare_parsed({MajA, MinA, PatchA, PreA}, {MajB, MinB, PatchB, PreB}) ->
    % Compare major version
    case compare_numbers(MajA, MajB) of
        eq ->
            % Compare minor version
            case compare_numbers(MinA, MinB) of
                eq ->
                    % Compare patch version
                    case compare_numbers(PatchA, PatchB) of
                        eq ->
                            % Compare prerelease
                            compare_prerelease(PreA, PreB);
                        Result -> Result
                    end;
                Result -> Result
            end;
        Result -> Result
    end.

%% Compare two numbers
-spec compare_numbers(integer(), integer()) -> lt | eq | gt.
compare_numbers(A, B) when A < B -> lt;
compare_numbers(A, B) when A > B -> gt;
compare_numbers(_, _) -> eq.

%% Compare pre-release versions
%% Rules:
%% - Stable version (empty string) > any pre-release
%% - alpha < beta < rc
%% - Same type compared by number (alpha.1 < alpha.2)
-spec compare_prerelease(string(), string()) -> lt | eq | gt.
compare_prerelease("", "") -> eq;
compare_prerelease("", _) -> gt;  % Stable > prerelease
compare_prerelease(_, "") -> lt;  % Prerelease < stable
compare_prerelease(PreA, PreB) ->
    % Extract prerelease type and number
    TypeA = extract_prerelease_type(PreA),
    TypeB = extract_prerelease_type(PreB),

    case compare_prerelease_types(TypeA, TypeB) of
        eq ->
            % Same type, compare by number
            NumA = extract_prerelease_number(PreA),
            NumB = extract_prerelease_number(PreB),
            compare_numbers(NumA, NumB);
        Result -> Result
    end.

%% Extract prerelease type (alpha, beta, rc, etc.)
-spec extract_prerelease_type(string()) -> string().
extract_prerelease_type(Pre) ->
    case string:split(Pre, ".") of
        [Type | _] -> Type;
        _ -> Pre
    end.

%% Extract prerelease number (e.g., "1" from "alpha.1")
-spec extract_prerelease_number(string()) -> non_neg_integer().
extract_prerelease_number(Pre) ->
    case string:split(Pre, ".") of
        [_, NumStr | _] ->
            try
                erlang:binary_to_integer(binary:list_to_bin(NumStr))
            catch
                _:_ -> 0
            end;
        _ -> 0
    end.

%% Compare prerelease types according to semantic versioning
%% Standard order: alpha < beta < rc
-spec compare_prerelease_types(string(), string()) -> lt | eq | gt.
compare_prerelease_types(Same, Same) -> eq;
compare_prerelease_types("alpha", "beta") -> lt;
compare_prerelease_types("alpha", "rc") -> lt;
compare_prerelease_types("beta", "alpha") -> gt;
compare_prerelease_types("beta", "rc") -> lt;
compare_prerelease_types("rc", "alpha") -> gt;
compare_prerelease_types("rc", "beta") -> gt;
compare_prerelease_types(A, B) ->
    % Fallback to string comparison for unknown types
    case A < B of
        true -> lt;
        false -> case A > B of
                   true -> gt;
                   false -> eq
                 end
    end.

%% Convert binary to integer safely
-spec binary_to_integer(binary()) -> non_neg_integer().
binary_to_integer(Bin) ->
    try
        erlang:binary_to_integer(Bin)
    catch
        _:_ -> 0
    end.

%% =============================================================================
%% Tests (run with: erlmcp_version:test())
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

version_test() ->
    {Major, Minor, Patch, _Pre} = version(),
    ?assert(is_integer(Major)),
    ?assert(is_integer(Minor)),
    ?assert(is_integer(Patch)).

version_string_test() ->
    VersionStr = version_string(),
    ?assert(is_list(VersionStr)),
    ?assert(string:find(VersionStr, ".") =/= nomatch).

compare_test() ->
    ?assertEqual(lt, compare("1.0.0", "1.1.0")),
    ?assertEqual(lt, compare("1.0.0", "2.0.0")),
    ?assertEqual(gt, compare("1.1.0", "1.0.0")),
    ?assertEqual(eq, compare("1.0.0", "1.0.0")),
    ?assertEqual(lt, compare("1.0.0-alpha", "1.0.0-beta")),
    ?assertEqual(lt, compare("1.0.0-alpha", "1.0.0")).

is_prerelease_test() ->
    case ?VERSION_PRERELEASE of
        "" ->
            ?assertNot(is_prerelease());
        _ ->
            ?assert(is_prerelease())
    end.

-endif.
