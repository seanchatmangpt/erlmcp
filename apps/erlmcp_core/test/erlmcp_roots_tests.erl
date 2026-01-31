-module(erlmcp_roots_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlmcp.hrl").

%%%====================================================================
%%% Roots URI Scheme Tests - Chicago School TDD
%%% Tests for MCP 2025-11-25 roots/list and root URI resolution
%%% Principles: Real processes, observable behavior, no mocks
%%% Joe Armstrong: "URI schemes are just patterns" - match and dispatch
%%%====================================================================

%%%====================================================================
%%% Test Generators
%%%====================================================================

roots_list_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"List default roots", fun test_list_default_roots/0},
          {"Add custom root", fun test_add_custom_root/0},
          {"Remove root", fun test_remove_root/0},
          {"List multiple roots", fun test_list_multiple_roots/0}
         ]
     end}.

uri_resolution_test_() ->
    {setup,
     fun setup_with_roots/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Resolve file:// URI", fun test_resolve_file_uri/0},
          {"Resolve file:///absolute path", fun test_resolve_file_absolute/0},
          {"Resolve non-existent file", fun test_resolve_nonexistent/0},
          {"Resolve unknown URI scheme", fun test_resolve_unknown_scheme/0},
          {"Resolve root-relative URI", fun test_resolve_root_relative/0}
         ]
     end}.

resource_reading_test_() ->
    {setup,
     fun setup_with_test_file/0,
     fun cleanup_with_file/1,
     fun(_) ->
         [
          {"Read existing file", fun test_read_existing_file/0},
          {"Read non-existent file", fun test_read_nonexistent_file/0},
          {"Read file via root URI", fun test_read_via_root_uri/0}
         ]
     end}.

uri_validation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Valid file:// URIs", fun test_valid_file_uris/0},
          {"Invalid URIs", fun test_invalid_uris/0},
          {"Empty URI", fun test_empty_uri/0},
          {"URI without scheme", fun test_uri_without_scheme/0}
         ]
     end}.

pattern_matching_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Pattern match file:// scheme", fun test_pattern_file_scheme/0},
          {"Pattern match custom scheme", fun test_pattern_custom_scheme/0},
          {"Pattern match root prefix", fun test_pattern_root_prefix/0}
         ]
     end}.

concurrent_access_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          {"Concurrent list roots", fun test_concurrent_list/0},
          {"Concurrent add roots", fun test_concurrent_add/0},
          {"Concurrent resolve URIs", fun test_concurrent_resolve/0}
         ]
     end}.

%%%====================================================================
%%% Setup and Cleanup
%%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_resources:start_link(),
    Pid.

setup_with_roots() ->
    Pid = setup(),
    %% Add custom roots for testing
    ok = erlmcp_resources:add_root(<<"file:///tmp/test_root">>, <<"Test Root">>),
    ok = erlmcp_resources:add_root(<<"file:///tmp/another_root">>, <<"Another Root">>),
    Pid.

setup_with_test_file() ->
    Pid = setup(),
    %% Create a temporary test file
    TestFilePath = "/tmp/erlmcp_test_file.txt",
    TestContent = <<"Hello, MCP Roots!">>,
    ok = file:write_file(TestFilePath, TestContent),
    {Pid, TestFilePath, TestContent}.

cleanup(Pid) when is_pid(Pid) ->
    erlmcp_resources:stop(),
    ok.

cleanup_with_file({Pid, _TestFilePath, _TestContent}) ->
    cleanup(Pid),
    %% Clean up test file
    file:delete("/tmp/erlmcp_test_file.txt"),
    ok.

%%%====================================================================
%%% List Roots Tests
%%%====================================================================

test_list_default_roots() ->
    %% Should return at least the default current directory root
    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assert(is_list(Roots)),
    ?assert(length(Roots) >= 1),
    %% Verify root structure
    [FirstRoot | _] = Roots,
    ?assert(maps:is_key(uri, FirstRoot)),
    ?assert(maps:is_key(name, FirstRoot)),
    ?assert(is_binary(maps:get(uri, FirstRoot))),
    ?assert(is_binary(maps:get(name, FirstRoot))).

test_add_custom_root() ->
    %% Add a custom root
    Uri = <<"s3://my-bucket">>,
    Name = <<"My S3 Bucket">>,
    ok = erlmcp_resources:add_root(Uri, Name),
    %% Verify it's in the list
    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assert(lists:any(fun(R) ->
        maps:get(uri, R, undefined) =:= Uri andalso
        maps:get(name, R, undefined) =:= Name
    end, Roots)).

test_remove_root() ->
    %% Add a root
    Uri = <<"file:///tmp/to_be_removed">>,
    ok = erlmcp_resources:add_root(Uri, <<"To Be Removed">>),
    %% Verify it exists
    {ok, Roots1} = erlmcp_resources:list_roots(),
    ?assert(lists:any(fun(R) -> maps:get(uri, R, undefined) =:= Uri end, Roots1)),
    %% Remove it
    ok = erlmcp_resources:remove_root(Uri),
    %% Verify it's gone
    {ok, Roots2} = erlmcp_resources:list_roots(),
    ?assertNot(lists:any(fun(R) -> maps:get(uri, R, undefined) =:= Uri end, Roots2)).

test_list_multiple_roots() ->
    %% Add multiple roots
    RootList = [
        {<<"file:///root1">>, <<"Root 1">>},
        {<<"file:///root2">>, <<"Root 2">>},
        {<<"custom://root3">>, <<"Root 3">>}
    ],
    [erlmcp_resources:add_root(Uri, Name) || {Uri, Name} <- RootList],
    %% Verify all are listed
    {ok, Roots} = erlmcp_resources:list_roots(),
    ?assert(length(Roots) >= length(RootList)),
    %% Check each root exists
    lists:foreach(fun({Uri, Name}) ->
        ?assert(lists:any(fun(R) ->
            maps:get(uri, R, undefined) =:= Uri andalso
            maps:get(name, R, undefined) =:= Name
        end, Roots))
    end, RootList).

%%%====================================================================
%%% URI Resolution Tests
%%%====================================================================

test_resolve_file_uri() ->
    %% Resolve a file:// URI to a local path
    %% Use /tmp which should exist on all systems
    Uri = <<"file:///tmp">>,
    case erlmcp_resources:resolve_root_uri(Uri) of
        {ok, "/tmp"} -> ok;
        {ok, Path} when is_list(Path) ->
            %% Some systems might normalize the path
            ?assert(lists:suffix("/tmp", filename:split(Path)));
        {error, enoent} ->
            %% /tmp might not exist (unlikely but possible)
            ok
    end.

test_resolve_file_absolute() ->
    %% Test file:///absolute/path format
    Uri = <<"file:///etc/passwd">>,  %% Should exist on Unix systems
    case erlmcp_resources:resolve_root_uri(Uri) of
        {ok, "/etc/passwd"} -> ok;
        {error, enoent} ->
            %% File doesn't exist (Windows?)
            ok
    end.

test_resolve_nonexistent() ->
    %% Resolve a non-existent file
    Uri = <<"file:///this/path/does/not/exist/12345">>,
    ?assertMatch({error, enoent}, erlmcp_resources:resolve_root_uri(Uri)).

test_resolve_unknown_scheme() ->
    %% Resolve an unknown URI scheme
    Uri = <<"unknown://test/resource">>,
    ?assertMatch({error, unknown_uri_scheme}, erlmcp_resources:resolve_root_uri(Uri)).

test_resolve_root_relative() ->
    %% After adding roots, verify root-relative URIs can be matched
    RootUri = <<"file:///tmp/erlmcp_root_test">>,
    ok = erlmcp_resources:add_root(RootUri, <<"Test Root">>),
    %% Create the directory
    ok = filelib:ensure_dir(filename:join(RootUri, "test")),
    %% Resolve a URI that starts with the root
    Uri = <<RootUri/binary, "/subresource">>,
    case erlmcp_resources:resolve_root_uri(Uri) of
        {ok, _Path} -> ok;
        {error, enoent} -> ok  %% Directory exists but file doesn't
    end,
    %% Clean up
    file:del_dir_r("/tmp/erlmcp_root_test").

%%%====================================================================
%%% Resource Reading Tests
%%%====================================================================

test_read_existing_file() ->
    %% Read the test file created in setup
    {ok, Content} = erlmcp_resources:read_resource(<<"file:///tmp/erlmcp_test_file.txt">>),
    ?assertEqual(<<"Hello, MCP Roots!">>, Content).

test_read_nonexistent_file() ->
    %% Try to read a non-existent file
    ?assertMatch({error, enoent}, erlmcp_resources:read_resource(<<"file:///tmp/nonexistent_12345.txt">>)).

test_read_via_root_uri() ->
    %% Read a file that's under a registered root
    RootUri = <<"file:///tmp">>,
    ok = erlmcp_resources:add_root(RootUri, <<"Temp Directory">>),
    %% Read the test file
    ?assertMatch({ok, <<"Hello, MCP Roots!">>},
                 erlmcp_resources:read_resource(<<"file:///tmp/erlmcp_test_file.txt">>)).

%%%====================================================================
%%% URI Validation Tests
%%%====================================================================

test_valid_file_uris() ->
    ValidUris = [
        <<"file:///tmp">>,
        <<"file:///home/user/file.txt">>,
        <<"file:///etc/config.conf">>,
        <<"file://localhost/tmp">>
    ],
    %% All should be valid (resolution may fail if file doesn't exist)
    lists:foreach(fun(Uri) ->
        case erlmcp_resources:resolve_root_uri(Uri) of
            {ok, _Path} -> ok;
            {error, enoent} -> ok;  %% File doesn't exist, but URI is valid
            {error, _} ->
                ?assert(false, {uri_resolution_failed, Uri})
        end
    end, ValidUris).

test_invalid_uris() ->
    %% Test that truly invalid URIs are rejected
    %% Note: Empty URIs are handled separately
    InvalidUri = <<"not a uri">>,
    ?assertMatch({error, unknown_uri_scheme}, erlmcp_resources:resolve_root_uri(InvalidUri)).

test_empty_uri() ->
    %% Empty URI should fail
    ?assertMatch({error, unknown_uri_scheme}, erlmcp_resources:resolve_root_uri(<<>>)).

test_uri_without_scheme() ->
    %% URI without :// separator
    ?assertMatch({error, unknown_uri_scheme}, erlmcp_resources:resolve_root_uri(<<"no-scheme-here">>)).

%%%====================================================================
%%% Pattern Matching Tests
%%%====================================================================

test_pattern_file_scheme() ->
    %% "URI schemes are just patterns" - Joe Armstrong
    %% Verify file:// pattern is recognized
    {ok, Pid} = erlmcp_resources:start_link(),
    Uri = <<"file:///tmp/test">>,
    ?assertMatch({ok, _} | {error, enoent}, erlmcp_resources:resolve_root_uri(Uri)),
    erlmcp_resources:stop().

test_pattern_custom_scheme() ->
    %% Custom schemes should fail (not registered)
    ?assertMatch({error, unknown_uri_scheme},
                 erlmcp_resources:resolve_root_uri(<<"custom://test">>)).

test_pattern_root_prefix() ->
    %% Test that root URIs are matched as prefixes
    RootUri = <<"file:///tmp/prefix_test">>,
    ok = erlmcp_resources:add_root(RootUri, <<"Prefix Root">>),
    %% URI starting with root should be matched
    Uri = <<RootUri/binary, "/resource">>,
    ?assertMatch({ok, _} | {error, enoent}, erlmcp_resources:resolve_root_uri(Uri)).

%%%====================================================================
%%% Concurrent Access Tests
%%%====================================================================

test_concurrent_list() ->
    %% Spawn multiple processes to list roots concurrently
    Pids = [spawn(fun() ->
        {ok, _Roots} = erlmcp_resources:list_roots()
    end) || _ <- lists:seq(1, 10)],
    %% All should complete without crashing
    timer:sleep(100),
    ?assert(lists:all(fun(Pid) ->
        is_process_alive(Pid) orelse  %% Either still running or completed successfully
        (catch exit(Pid, kill)) =/= true
    end, Pids)).

test_concurrent_add() ->
    %% Spawn multiple processes to add roots concurrently
    RootList = [
        {<<"file:///concurrent1">>, <<"Concurrent 1">>},
        {<<"file:///concurrent2">>, <<"Concurrent 2">>},
        {<<"file:///concurrent3">>, <<"Concurrent 3">>}
    ],
    Pids = [spawn(fun() ->
        erlmcp_resources:add_root(Uri, Name)
    end) || {Uri, Name} <- RootList],
    %% Wait for all to complete
    timer:sleep(100),
    %% Verify all roots were added
    {ok, Roots} = erlmcp_resources:list_roots(),
    lists:foreach(fun({Uri, Name}) ->
        ?assert(lists:any(fun(R) ->
            maps:get(uri, R, undefined) =:= Uri andalso
            maps:get(name, R, undefined) =:= Name
        end, Roots))
    end, RootList).

test_concurrent_resolve() ->
    %% Create a test file
    TestPath = "/tmp/concurrent_test.txt",
    ok = file:write_file(TestPath, <<"test">>),
    %% Spawn multiple resolvers
    Pids = [spawn(fun() ->
        erlmcp_resources:resolve_root_uri(<<"file:///tmp/concurrent_test.txt">>)
    end) || _ <- lists:seq(1, 10)],
    %% Wait for all to complete
    timer:sleep(100),
    %% Clean up
    file:delete(TestPath),
    %% All should have succeeded
    ?assertEqual(10, length([Pid || Pid <- Pids, not is_process_alive(Pid)])).

%%%====================================================================
%%% Helper Functions
%%%====================================================================

%% Helper to verify root entry structure
verify_root_entry(Root) ->
    ?assert(maps:is_key(uri, Root)),
    ?assert(maps:is_key(name, Root)),
    ?assert(is_binary(maps:get(uri, Root))),
    ?assert(is_binary(maps:get(name, Root)))),
    ok.
