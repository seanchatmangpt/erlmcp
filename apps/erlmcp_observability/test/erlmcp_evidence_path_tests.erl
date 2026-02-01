%%%-------------------------------------------------------------------
%%% @doc Unit tests for erlmcp_evidence_path following Chicago School TDD
%%%
%%% Chicago School TDD Principles:
%%% - Test observable behavior through API calls
%%% - Use real file system operations (no mocks)
%%% - Verify through file system state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_evidence_path_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

evidence_path_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         fun test_create_evidence_path/0,
         fun test_get_evidence_path/0,
         fun test_list_evidence_artifacts/0,
         fun test_verify_artifact_completeness/0,
         fun test_mark_certified/0,
         fun test_is_certified/0
     ]}.

setup() ->
    % Create temp test directory
    TestDir = "/tmp/erlmcp_evidence_test_" ++ integer_to_list(erlang:system_time()),
    TestDir.

cleanup(TestDir) ->
    % Clean up test directory
    os:cmd("rm -rf " ++ TestDir).

%%====================================================================
%% Test Cases
%%====================================================================

test_create_evidence_path() ->
    {ok, Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", team),

    % Verify path was created
    ?assert(filelib:is_dir(Path)),

    % Verify path structure
    ?assert(string:str(Path, "2.1.0") > 0),
    ?assert(string:str(Path, "team") > 0).

test_get_evidence_path() ->
    % Create path first
    {ok, CreatedPath} = erlmcp_evidence_path:create_evidence_path("2.1.0", enterprise),

    % Get path
    {ok, Path} = erlmcp_evidence_path:get_evidence_path("2.1.0", enterprise),

    ?assertEqual(CreatedPath, Path).

test_list_evidence_artifacts() ->
    % Create path
    {ok, Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", gov),

    % Create some test artifacts
    file:write_file(filename:join(Path, "bench_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "chaos_report.json"), <<"{}">>),

    % List artifacts
    {ok, Artifacts} = erlmcp_evidence_path:list_evidence_artifacts("2.1.0", gov),

    % Should find 2 artifacts
    ?assertEqual(2, length(Artifacts)),
    ?assert(lists:member("bench_report.json", Artifacts)),
    ?assert(lists:member("chaos_report.json", Artifacts)).

test_verify_artifact_completeness() ->
    % Create path
    {ok, Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", team),

    % Initially incomplete
    ?assertMatch({error, {incomplete, _}},
                 erlmcp_evidence_path:verify_artifact_completeness("2.1.0", team)),

    % Add all required artifacts
    file:write_file(filename:join(Path, "bench_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "chaos_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "conformance_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "refusal_audit.json"), <<"{}">>),

    % Now complete
    ?assertEqual({ok, complete},
                 erlmcp_evidence_path:verify_artifact_completeness("2.1.0", team)).

test_mark_certified() ->
    % Create path and add required artifacts
    {ok, Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", enterprise),
    file:write_file(filename:join(Path, "bench_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "chaos_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "conformance_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "refusal_audit.json"), <<"{}">>),

    % Mark as certified
    {ok, CertFile} = erlmcp_evidence_path:mark_certified("2.1.0", enterprise),

    % Verify cert file exists
    ?assert(filelib:is_file(CertFile)).

test_is_certified() ->
    % Create and certify
    {ok, Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", gov),
    file:write_file(filename:join(Path, "bench_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "chaos_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "conformance_report.json"), <<"{}">>),
    file:write_file(filename:join(Path, "refusal_audit.json"), <<"{}">>),

    % Initially not certified
    ?assertEqual(false, erlmcp_evidence_path:is_certified("2.1.0", gov)),

    % Mark as certified
    {ok, _} = erlmcp_evidence_path:mark_certified("2.1.0", gov),

    % Now certified
    ?assertEqual(true, erlmcp_evidence_path:is_certified("2.1.0", gov)).

%%====================================================================
%% Edge Cases
%%====================================================================

nonexistent_path_test() ->
    ?assertEqual({error, not_found},
                 erlmcp_evidence_path:get_evidence_path("99.99.99", team)).

mark_certified_incomplete_test() ->
    % Create path without required artifacts
    {ok, _Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", team),

    % Should fail to certify
    ?assertMatch({error, {incomplete, _}},
                 erlmcp_evidence_path:mark_certified("2.1.0", team)).

create_path_twice_test() ->
    {ok, Path1} = erlmcp_evidence_path:create_evidence_path("2.1.0", enterprise),
    {ok, Path2} = erlmcp_evidence_path:create_evidence_path("2.1.0", enterprise),

    % Should return same path
    ?assertEqual(Path1, Path2).

list_empty_artifacts_test() ->
    {ok, _Path} = erlmcp_evidence_path:create_evidence_path("2.1.0", team),

    {ok, Artifacts} = erlmcp_evidence_path:list_evidence_artifacts("2.1.0", team),

    % Should be empty initially
    ?assertEqual([], Artifacts).
