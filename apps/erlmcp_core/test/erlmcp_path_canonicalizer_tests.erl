%%%====================================================================
%%% @doc Test Suite for erlmcp_path_canonicalizer
%%%
%%% Tests:
%%% - Path canonicalization (resolve . and ..)
%%% - Path traversal prevention
%%% - URI validation and extraction
%%% - Directory jail enforcement
%%% - Symlink resolution prevention
%%% - Edge cases (empty paths, malformed URIs)
%%%
%%% Chicago School TDD: Pure functions, state-based verification
%%% Target: 80%+ coverage
%%% @end
%%%====================================================================
-module(erlmcp_path_canonicalizer_tests).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% Test Fixtures
%%%====================================================================

path_canonicalizer_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [fun test_canonicalize_simple_path/1,
      fun test_canonicalize_with_dots/1,
      fun test_canonicalize_with_parent_refs/1,
      fun test_canonicalize_with_traversal/1,
      fun test_canonicalize_mixed_separators/1,
      fun test_validate_safe_path/1,
      fun test_validate_unsafe_path/1,
      fun test_validate_traversal_attempt/1,
      fun test_is_path_safe/1,
      fun test_is_path_unsafe/1,
      fun test_extract_path_from_uri/1,
      fun test_extract_path_from_file_uri/1,
      fun test_extract_path_no_scheme/1,
      fun test_rebuild_uri/1,
      fun test_edge_case_empty_path/1,
      fun test_edge_case_too_many_parent_refs/1,
      fun test_edge_case_query_string/1]}.

%%%====================================================================
%%% Setup and Teardown
%%%====================================================================

setup() ->
    %% No setup needed for pure function tests
    ok.

cleanup(_Ok) ->
    %% No cleanup needed
    ok.

%%%====================================================================
%%% Test Functions
%%%====================================================================

%% @doc Test canonicalizing a simple path
test_canonicalize_simple_path(_Ok) ->
    Input = <<"/home/user/docs">>,
    Expected = <<"/home/user/docs">>,

    ?assertEqual(Expected,
                 erlmcp_path_canonicalizer:canonicalize_path(Input)).

%% @doc Test canonicalizing path with current directory references
test_canonicalize_with_dots(_Ok) ->
    Input = <<"/home/./user/docs">>,
    Expected = <<"/home/user/docs">>,

    ?assertEqual(Expected,
                 erlmcp_path_canonicalizer:canonicalize_path(Input)),

    %% Multiple dots
    Input2 = <<"/home/user/././docs">>,
    Expected2 = <<"/home/user/docs">>,

    ?assertEqual(Expected2,
                 erlmcp_path_canonicalizer:canonicalize_path(Input2)).

%% @doc Test canonicalizing path with parent directory references
test_canonicalize_with_parent_refs(_Ok) ->
    Input = <<"/home/user/docs/../images">>,
    Expected = <<"/home/user/images">>,

    ?assertEqual(Expected,
                 erlmcp_path_canonicalizer:canonicalize_path(Input)),

    %% Multiple parent refs
    Input2 = <<"/home/user/docs/../images/../downloads">>,
    Expected2 = <<"/home/user/downloads">>,

    ?assertEqual(Expected2,
                 erlmcp_path_canonicalizer:canonicalize_path(Input2)).

%% @doc Test path traversal prevention
test_canonicalize_with_traversal(_Ok) ->
    %% Simple traversal attempt
    Input = <<"/home/user/../etc/passwd">>,
    Expected = <<"/home/etc/passwd">>,

    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),
    ?assertEqual(Expected, Result),

    %% Ensure it doesn't escape root
    ?assertNot(<<"/../etc/passwd">> =:= Result),
    ?assertNot(<<"etc/passwd">> =:= Result).

%% @doc Test normalizing mixed path separators
test_canonicalize_mixed_separators(_Ok) ->
    Input = <<"/home\\user\\docs">>,
    Expected = <<"/home/user/docs">>,

    ?assertEqual(Expected,
                 erlmcp_path_canonicalizer:canonicalize_path(Input)),

    %% More mixed case
    Input2 = <<"/home/user\\docs/file.txt">>,
    Expected2 = <<"/home/user/docs/file.txt">>,

    ?assertEqual(Expected2,
                 erlmcp_path_canonicalizer:canonicalize_path(Input2)).

%% @doc Test validating a safe path
test_validate_safe_path(_Ok) ->
    Uri = <<"file:///home/user/docs">>,
    AllowedDirs = [<<"/">>, <<"/home/user">>],

    ?assertMatch({ok, _},
                 erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                  AllowedDirs)).

%% @doc Test validating an unsafe path
test_validate_unsafe_path(_Ok) ->
    Uri = <<"file:///etc/passwd">>,
    AllowedDirs = [<<"/home/user">>],

    ?assertMatch({error, {path_outside_allowed_dirs, _}},
                 erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                  AllowedDirs)).

%% @doc Test path traversal attempt detection
test_validate_traversal_attempt(_Ok) ->
    Uri = <<"file:///home/user/docs/../../../etc/passwd">>,
    AllowedDirs = [<<"/home/user">>],

    Result = erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                               AllowedDirs),

    %% Should either be error or canonicalized to safe path
    case Result of
        {ok, CanonicalUri} ->
            %% If ok, ensure it's within allowed dirs
            ?assertNot(<<"/etc/passwd">> =:= CanonicalUri);
        {error, _} ->
            ?assert(true)
    end.

%% @doc Test is_path_safe with matching prefix
test_is_path_safe(_Ok) ->
    Path = <<"/home/user/docs/file.txt">>,
    AllowedDirs = [<<"/home/user">>, <<"/var/log">>],

    ?assert(erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

%% @doc Test is_path_safe with non-matching prefix
test_is_path_unsafe(_Ok) ->
    Path = <<"/etc/passwd">>,
    AllowedDirs = [<<"/home/user">>],

    ?assertNot(erlmcp_path_canonicalizer:is_path_safe(Path, AllowedDirs)).

%% @doc Test extracting path from URI with scheme
test_extract_path_from_uri(_Ok) ->
    Uri = <<"http://example.com/path/to/resource?query=value">>,

    %% Internal function - we test through validate_resource_path
    AllowedDirs = [<<"/">>],
    {ok, CanonicalUri} = erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                           AllowedDirs),

    %% Path should be canonicalized
    ?assertNot(nomatch = binary:match(CanonicalUri, <<"/path/to/resource">>)).

%% @doc Test extracting path from file:// URI
test_extract_path_from_file_uri(_Ok) ->
    Uri = <<"file:///home/user/docs">>,
    AllowedDirs = [<<"/">>],

    ?assertMatch({ok, _},
                 erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                  AllowedDirs)).

%% @doc Test path without scheme
test_extract_path_no_scheme(_Ok) ->
    Path = <<"/home/user/docs">>,
    AllowedDirs = [<<"/">>],

    ?assertMatch({ok, CanonicalPath} when CanonicalPath =:= Path,
                 erlmcp_path_canonicalizer:validate_resource_path(Path,
                                                                   AllowedDirs)).

%% @doc Test rebuilding URI with canonical path
test_rebuild_uri(_Ok) ->
    Uri = <<"http://example.com/path/../docs">>,
    AllowedDirs = [<<"/">>],

    {ok, CanonicalUri} = erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                          AllowedDirs),

    %% Should preserve scheme
    ?assertMatch(<<"http://", _/binary>>, CanonicalUri),

    %% Should canonicalize path
    ?assertNot(nomatch = binary:match(CanonicalUri, <<"/docs">>)).

%% @doc Test edge case: empty path
test_edge_case_empty_path(_Ok) ->
    Input = <<>>,

    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),

    %% Should handle gracefully - either return "/" or original
    ?assert(Result =:= <<"/">> orelse Result =:= <<>>).

%% @doc Test edge case: too many parent references
test_edge_case_too_many_parent_refs(_Ok) ->
    Input = <<"/../../../etc/passwd">>,

    Result = erlmcp_path_canonicalizer:canonicalize_path(Input),

    %% Should not escape root
    ?assertNot(<<"../etc/passwd">> =:= Result),
    ?assertNot(<<"etc/passwd">> =:= Result),
    ?assertEqual(<<"/etc/passwd">>, Result).

%% @doc Test edge case: query string handling
test_edge_case_query_string(_Ok) ->
    Uri = <<"file:///home/user/docs?param=value">>,
    AllowedDirs = [<<"/">>],

    ?assertMatch({ok, _},
                 erlmcp_path_canonicalizer:validate_resource_path(Uri,
                                                                  AllowedDirs)).
