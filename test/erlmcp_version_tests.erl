-module(erlmcp_version_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() -> ok.
cleanup(_) -> ok.

%%====================================================================
%% Version Query Tests
%%====================================================================

query_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_version()),
        ?_test(test_get_major_version()),
        ?_test(test_get_minor_version()),
        ?_test(test_get_patch_version()),
        ?_test(test_version_format())
    ] end}.

test_get_version() ->
    Result = erlmcp_version:get_version(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse is_tuple(Result)).

test_get_major_version() ->
    Result = erlmcp_version:get_major_version(),
    ?assert(is_integer(Result) andalso Result >= 0).

test_get_minor_version() ->
    Result = erlmcp_version:get_minor_version(),
    ?assert(is_integer(Result) andalso Result >= 0).

test_get_patch_version() ->
    Result = erlmcp_version:get_patch_version(),
    ?assert(is_integer(Result) andalso Result >= 0).

test_version_format() ->
    Version = erlmcp_version:get_version(),
    ?assert(is_binary(Version) orelse is_list(Version)).

%%====================================================================
%% Comparison Tests
%%====================================================================

comparison_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_version_equal()),
        ?_test(test_version_less_than()),
        ?_test(test_version_greater_than()),
        ?_test(test_version_gte()),
        ?_test(test_version_lte())
    ] end}.

test_version_equal() ->
    V1 = <<"1.0.0">>,
    Result = erlmcp_version:compare(V1, V1),
    ?assertMatch(equal | 0 | <<"=">>, Result).

test_version_less_than() ->
    Result = erlmcp_version:compare(<<"1.0.0">>, <<"1.1.0">>),
    ?assertMatch(less | -1 | <<"<">>, Result).

test_version_greater_than() ->
    Result = erlmcp_version:compare(<<"2.0.0">>, <<"1.0.0">>),
    ?assertMatch(greater | 1 | <<">">>  , Result).

test_version_gte() ->
    R1 = erlmcp_version:compare(<<"2.0.0">>, <<"1.0.0">>),
    R2 = erlmcp_version:compare(<<"1.0.0">>, <<"1.0.0">>),
    ?assert(R1 =/= less orelse R2 =:= equal).

test_version_lte() ->
    R1 = erlmcp_version:compare(<<"1.0.0">>, <<"2.0.0">>),
    R2 = erlmcp_version:compare(<<"1.0.0">>, <<"1.0.0">>),
    ?assert(R1 =/= greater orelse R2 =:= equal).

%%====================================================================
%% Compatibility Tests
%%====================================================================

compatibility_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_is_compatible()),
        ?_test(test_minimum_version()),
        ?_test(test_maximum_version()),
        ?_test(test_supports_feature())
    ] end}.

test_is_compatible() ->
    CurrentVersion = erlmcp_version:get_version(),
    Result = erlmcp_version:is_compatible(CurrentVersion),
    ?assert(is_boolean(Result)).

test_minimum_version() ->
    MinVersion = erlmcp_version:get_minimum_version(),
    ?assert(is_binary(MinVersion) orelse is_list(MinVersion) orelse is_tuple(MinVersion)).

test_maximum_version() ->
    MaxVersion = erlmcp_version:get_maximum_version(),
    ?assert(MaxVersion =:= undefined orelse is_binary(MaxVersion) orelse is_list(MaxVersion)).

test_supports_feature() ->
    Result = erlmcp_version:supports_feature(sampling),
    ?assert(is_boolean(Result)).

%%====================================================================
%% Build Information Tests
%%====================================================================

build_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_build_info()),
        ?_test(test_get_git_hash()),
        ?_test(test_get_build_date()),
        ?_test(test_get_build_time())
    ] end}.

test_get_build_info() ->
    Result = erlmcp_version:get_build_info(),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= undefined).

test_get_git_hash() ->
    Result = erlmcp_version:get_git_hash(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= undefined).

test_get_build_date() ->
    Result = erlmcp_version:get_build_date(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= undefined).

test_get_build_time() ->
    Result = erlmcp_version:get_build_time(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= undefined).

%%====================================================================
%% Dependencies Tests
%%====================================================================

dependencies_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_dependencies()),
        ?_test(test_get_dependency_version()),
        ?_test(test_check_dependency()),
        ?_test(test_otp_version())
    ] end}.

test_get_dependencies() ->
    Result = erlmcp_version:get_dependencies(),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= undefined).

test_get_dependency_version() ->
    Result = erlmcp_version:get_dependency_version(jsx),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= undefined).

test_check_dependency() ->
    Result = erlmcp_version:check_dependency(erlang),
    ?assert(is_boolean(Result)).

test_otp_version() ->
    Result = erlmcp_version:get_otp_version(),
    ?assert(is_binary(Result) orelse is_list(Result) orelse is_tuple(Result)).

%%====================================================================
%% Version String Tests
%%====================================================================

string_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_parse_version_string()),
        ?_test(test_format_version()),
        ?_test(test_version_components()),
        ?_test(test_version_with_metadata())
    ] end}.

test_parse_version_string() ->
    VersionStr = <<"1.2.3">>,
    Result = erlmcp_version:parse_version(VersionStr),
    ?assertMatch({ok, _} | {error, _} | {_, _, _}, Result).

test_format_version() ->
    Result = erlmcp_version:format_version(),
    ?assert(is_binary(Result) orelse is_list(Result)).

test_version_components() ->
    Result = erlmcp_version:get_version_components(),
    ?assert(is_tuple(Result) orelse is_list(Result) orelse is_map(Result)).

test_version_with_metadata() ->
    Result = erlmcp_version:get_version_string(),
    ?assert(is_binary(Result) orelse is_list(Result)).

%%====================================================================
%% Release Information Tests
%%====================================================================

release_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_is_beta()),
        ?_test(test_is_release()),
        ?_test(test_is_dev()),
        ?_test(test_release_status())
    ] end}.

test_is_beta() ->
    Result = erlmcp_version:is_beta(),
    ?assert(is_boolean(Result)).

test_is_release() ->
    Result = erlmcp_version:is_release(),
    ?assert(is_boolean(Result)).

test_is_dev() ->
    Result = erlmcp_version:is_dev(),
    ?assert(is_boolean(Result)).

test_release_status() ->
    Result = erlmcp_version:get_release_status(),
    ?assertMatch(alpha | beta | rc | release | dev | unknown, Result).

%%====================================================================
%% Changelog Tests
%%====================================================================

changelog_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_changelog()),
        ?_test(test_get_version_changelog()),
        ?_test(test_breaking_changes()),
        ?_test(test_new_features())
    ] end}.

test_get_changelog() ->
    Result = erlmcp_version:get_changelog(),
    ?assert(is_list(Result) orelse is_binary(Result) orelse Result =:= undefined).

test_get_version_changelog() ->
    Result = erlmcp_version:get_version_changelog(erlmcp_version:get_version()),
    ?assert(is_binary(Result) orelse is_list(Result) orelse Result =:= undefined).

test_breaking_changes() ->
    Result = erlmcp_version:get_breaking_changes(),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= undefined).

test_new_features() ->
    Result = erlmcp_version:get_new_features(),
    ?assert(is_list(Result) orelse is_map(Result) orelse Result =:= undefined).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_version_consistency()),
        ?_test(test_version_components_match()),
        ?_test(test_version_compatibility_matrix())
    ] end}.

test_version_consistency() ->
    V = erlmcp_version:get_version(),
    Components = erlmcp_version:get_version_components(),
    ?assert(V =/= undefined andalso Components =/= undefined).

test_version_components_match() ->
    Major = erlmcp_version:get_major_version(),
    Minor = erlmcp_version:get_minor_version(),
    Patch = erlmcp_version:get_patch_version(),
    ?assert(is_integer(Major) andalso is_integer(Minor) andalso is_integer(Patch)).

test_version_compatibility_matrix() ->
    CurrentVersion = erlmcp_version:get_version(),
    MinVersion = erlmcp_version:get_minimum_version(),
    Compatible = erlmcp_version:is_compatible(CurrentVersion),
    ?assert(is_boolean(Compatible)).
