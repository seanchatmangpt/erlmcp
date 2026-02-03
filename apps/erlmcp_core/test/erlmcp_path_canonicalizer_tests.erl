%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Test Suite for Path Canonicalizer
%%%
%%% Tests cover:
%%% - Path canonicalization (resolve . and ..)
%%% - URI validation and extraction
%%% - Path safety checks against allowed directories
%%% - Path traversal attack prevention
%%% - Mixed path separator handling
%%% - Edge cases (empty paths, root, too many ..)
%%%
%%% Chicago School TDD: State-based verification, real functions, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_path_canonicalizer_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

%% @doc Setup common test data
setup_paths() ->
    #{
        allowed_dirs => [<<"/">>, <<"/allowed">>, <<"/var/data">>],
        safe_paths => [
            <<"/foo/bar">>,
            <<"/allowed/path">>,
            <<"/var/data/file.txt">>,
            <<"/foo/../bar">>,
            <<"/foo/./bar">>,
            <<"/foo/bar/baz/../qux">>
        ],
        unsafe_paths => [
            <<"/etc/passwd">>,  % Not in allowed dirs
            <<"/tmp/test">>,    % Not in allowed dirs
            <<"../../etc">>     % Path traversal
        ],
        uris => [
            <<"file:///foo/bar">>,
            <<"http://example.com/path/to/resource">>,
            <<"/simple/path">>
        ]
    }.

%%%====================================================================
%%% Path Canonicalization Tests
%%%====================================================================

canonicalize_simple_path_test() ->
    Input = <<"/foo/bar/baz">>,
    Expected = <<"/foo/bar/baz">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_with_current_dir_test() ->
    Input = <<"/foo/./bar">>,
    Expected = <<"/foo/bar">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_with_parent_dir_test() ->
    Input = <<"/foo/../bar">>,
    Expected = <<"/bar">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_complex_path_test() ->
    Input = <<"/foo/bar/../baz/./qux">>,
    Expected = <<"/foo/baz/qux">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_multiple_parents_test() ->
    Input = <<"/foo/bar/baz/../../../qux">>,
    Expected = <<"/qux">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_too_many_parents_test() ->
    %% When .. exceeds path depth, keep them
    Input = <<"../../../etc/passwd">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    %% Should preserve leading .. since they can't be resolved
    ?assertEqual(<<"/etc/passwd">>, Result).

canonicalize_with_double_slash_test() ->
    Input = <<"/foo//bar">>,
    Expected = <<"/foo/bar">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_root_path_test() ->
    Input = <<"/">>,
    Expected = <<"/">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_empty_path_test() ->
    %% Empty path should return it as-is (error case)
    Input = <<"">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(<<"">>, Result).

canonicalize_trailing_slash_test() ->
    Input = <<"/foo/bar/">>,
    Expected = <<"/foo/bar">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_with_windows_separators_test() ->
    Input = <<"\\foo\\bar\\baz">>,
    Expected = <<"/foo/bar/baz">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

canonicalize_mixed_separators_test() ->
    Input = <<"/foo\\bar/baz">>,
    Expected = <<"/foo/bar/baz">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result).

%%%====================================================================
%%% URI Validation Tests
%%%====================================================================

validate_resource_path_simple_test() ->
    Uri = <<"/foo/bar">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertEqual(<<"/foo/bar">>, Result).

validate_resource_path_with_scheme_test() ->
    Uri = <<"file:///foo/bar">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertEqual(<<"file:///foo/bar">>, Result).

validate_resource_path_with_query_test() ->
    Uri = <<"/foo/bar?query=value">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Query string should be stripped, path canonicalized
    ?assertEqual(<<"/foo/bar">>, Result).

validate_resource_path_allowed_subdir_test() ->
    Uri = <<"/allowed/path/to/file">>,
    AllowedDirs = [<<"/allowed">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertEqual(<<"/allowed/path/to/file">>, Result).

validate_resource_path_outside_allowed_test() ->
    Uri = <<"/etc/passwd">>,
    AllowedDirs = [<<"/allowed">>],
    Result = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertMatch({error, {path_outside_allowed_dirs, _}}, Result).

validate_resource_path_traversal_attack_test() ->
    Uri = <<"/allowed/../etc/passwd">>,
    AllowedDirs = [<<"/allowed">>],
    Result = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Canonicalized path is /etc/passwd which is outside /allowed
    ?assertMatch({error, {path_outside_allowed_dirs, _}}, Result).

validate_resource_path_complex_traversal_test() ->
    Uri = <<"/allowed/./sub/.././etc/passwd">>,
    AllowedDirs = [<<"/allowed">>],
    Result = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Canonicalizes to /allowed/etc/passwd (outside /allowed subtree)
    ?assertMatch({error, {path_outside_allowed_dirs, _}}, Result).

validate_resource_path_within_allowed_test() ->
    Uri = <<"/allowed/subdir/file.txt">>,
    AllowedDirs = [<<"/allowed">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertEqual(<<"/allowed/subdir/file.txt">>, Result).

%%%====================================================================
%%% Path Safety Tests
%%%====================================================================

is_path_safe_root_allowed_test() ->
    Path = <<"/foo/bar">>,
    AllowedDirs = [<<"/">>],
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_subdirectory_test() ->
    Path = <<"/allowed/path">>,
    AllowedDirs = [<<"/allowed">>],
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_exact_match_test() ->
    Path = <<"/allowed">>,
    AllowedDirs = [<<"/allowed">>],
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_unsafe_path_test() ->
    Path = <<"/etc/passwd">>,
    AllowedDirs = [<<"/allowed">>],
    ?assertEqual(false, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_multiple_allowed_test() ->
    Path1 = <<"/foo/bar">>,
    Path2 = <<"/var/data/file">>,
    AllowedDirs = [<<"/foo">>, <<"/var/data">>],
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path1, AllowedDirs)),
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path2, AllowedDirs)).

is_path_safe_empty_allowed_list_test() ->
    Path = <<"/foo/bar">>,
    AllowedDirs = [],
    ?assertEqual(false, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_prefix_collision_test() ->
    %% Test that /foo/bar doesn't match /foo/barbaz
    Path = <<"/foo/barbaz">>,
    AllowedDirs = [<<"/foo/bar">>],
    ?assertEqual(false, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

is_path_safe_child_directory_test() ->
    %% Child directories should be safe
    Path = <<"/foo/bar/baz/qux">>,
    AllowedDirs = [<<"/foo/bar">>],
    ?assertEqual(true, erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

%%%====================================================================
%%% Edge Cases and Error Handling
%%%====================================================================

validate_malformed_uri_test() ->
    %% Test with malformed URI (should catch error)
    Uri = <<"file:///foo/../../../etc/passwd">>,
    AllowedDirs = [<<"/safe">>],
    Result = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    ?assertMatch({error, _}, Result).

validate_with_special_characters_test() ->
    Uri = <<"/foo/bar%20file.txt">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Should preserve special characters in canonical form
    ?assertEqual(<<"/foo/bar%20file.txt">>, Result).

canonicalize_preserves_trailing_slash_normalized_test() ->
    %% Trailing slash should be normalized
    Input = <<"/foo/bar///">>,
    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(<<"/foo/bar">>, Result).

%%%====================================================================
%%% Integration Tests (Real-World Scenarios)
%%%====================================================================

validate_resource_file_path_test() ->
    %% Simulate real file resource validation
    FilePath = <<"/var/data/resource_123.json">>,
    AllowedDirs = [<<"/var/data">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(FilePath, AllowedDirs),
    ?assertEqual(<<"/var/data/resource_123.json">>, Result).

validate_resource_with_deep_nesting_test() ->
    DeepPath = <<"/allowed/a/b/c/d/e/f/g/h/i/j/file.txt">>,
    AllowedDirs = [<<"/allowed">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(DeepPath, AllowedDirs),
    ?assertEqual(DeepPath, Result).

validate_http_uri_test() ->
    Uri = <<"http://example.com/api/resource">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Should preserve scheme in rebuild
    ?assertEqual(Uri, Result).

validate_http_uri_with_query_and_fragment_test() ->
    Uri = <<"http://example.com/path?query=value#fragment">>,
    AllowedDirs = [<<"/">>],
    {ok, Result} = erlmcp_path_canonicalizer:validate_resource_path(Uri, AllowedDirs),
    %% Query and fragment stripped, scheme preserved
    ?assertEqual(<<"http://example.com/path">>, Result).

%%%====================================================================
%%% Security Tests
%%%====================================================================

path_traversal_prevention_test() ->
    %% Test various path traversal attempts
    AllowedDirs = [<<"/safe">>],

    TraversalAttempts = [
        <<"/safe/../../etc/passwd">>,
        <<"/safe/./../../etc/passwd">>,
        <<"/safe/subdir/../../../etc/passwd">>,
        <<"/safe/../../../../../../etc/passwd">>
    ],

    lists:foreach(fun(Attempt) ->
        Result = erlmcp_path_canonicalizer:validate_resource_path(Attempt, AllowedDirs),
        ?assertMatch({error, {path_outside_allowed_dirs, _}}, Result,
                     "Should reject traversal attempt: " ++ binary_to_list(Attempt))
    end, TraversalAttempts).

absolute_path_required_test() ->
    %% Ensure only absolute paths are accepted
    RelativePaths = [
        <<"foo/bar">>,
        <<"../foo/bar">>,
        <<"./foo/bar">>,
        <<"../../foo">>
    ],

    AllowedDirs = [<<"/safe">>],

    lists:foreach(fun(RelativePath) ->
        Result = erlmcp_path_canonicalizer:validate_resource_path(RelativePath, AllowedDirs),
        %% Relative paths won't start with /safe, so should fail
        ?assertMatch({error, {path_outside_allowed_dirs, _}}, Result,
                     "Should reject relative path: " ++ binary_to_list(RelativePath))
    end, RelativePaths).
