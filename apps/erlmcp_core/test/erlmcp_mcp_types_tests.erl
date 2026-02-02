-module(erlmcp_mcp_types_tests).

-include_lib("eunit/include/eunit.hrl").

%% OTP 28 Nominal Types Test Suite
%%
%% This test suite demonstrates how Dialyzer enforces nominal type safety
%% to prevent semantic type confusion at compile-time.
%%
%% The key insight: Even though types have the same structure (binary()),
%% Dialyzer treats them as distinct types when declared with -nominal.

%%====================================================================
%% Type Constructor Tests
%%====================================================================

new_request_id_test() ->
    %% Valid request ID
    Id = <<"req-12345">>,
    ?assertEqual(Id, erlmcp_mcp_types:new_request_id(Id)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_request_id(<<"">>)),

    %% Not a binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_request_id(123)),
    ?assertError(badarg, erlmcp_mcp_types:new_request_id(atom)).

new_tool_name_test() ->
    %% Valid tool name
    Name = <<"my_tool">>,
    ?assertEqual(Name, erlmcp_mcp_types:new_tool_name(Name)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_tool_name(<<"">>)).

new_resource_uri_test() ->
    %% Valid resource URI
    Uri = <<"file:///path/to/resource">>,
    ?assertEqual(Uri, erlmcp_mcp_types:new_resource_uri(Uri)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_resource_uri(<<"">>)).

new_session_id_test() ->
    %% Valid session ID
    Id = <<"session-abc-123">>,
    ?assertEqual(Id, erlmcp_mcp_types:new_session_id(Id)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_session_id(<<"">>)).

new_progress_token_test() ->
    %% Valid binary token
    BinToken = <<"token-xyz">>,
    ?assertEqual(BinToken, erlmcp_mcp_types:new_progress_token(BinToken)),

    %% Valid integer token
    IntToken = 12345,
    ?assertEqual(IntToken, erlmcp_mcp_types:new_progress_token(IntToken)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_progress_token(<<"">>)),

    %% Zero or negative integer should fail
    ?assertError(badarg, erlmcp_mcp_types:new_progress_token(0)),
    ?assertError(badarg, erlmcp_mcp_types:new_progress_token(-1)).

new_cursor_token_test() ->
    %% Valid cursor token
    Token = base64:encode(crypto:strong_rand_bytes(24)),
    ?assertEqual(Token, erlmcp_mcp_types:new_cursor_token(Token)),

    %% Empty binary should fail
    ?assertError(badarg, erlmcp_mcp_types:new_cursor_token(<<"">>)).

%%====================================================================
%% Validation Tests
%%====================================================================

validate_request_id_test() ->
    %% Valid request ID
    ?assertMatch({ok, <<"req-123">>}, erlmcp_mcp_types:validate_request_id(<<"req-123">>)),

    %% Invalid: empty binary
    ?assertMatch({error, invalid_request_id}, erlmcp_mcp_types:validate_request_id(<<"">>)),

    %% Invalid: not a binary
    ?assertMatch({error, invalid_request_id}, erlmcp_mcp_types:validate_request_id(123)).

validate_tool_name_test() ->
    %% Valid tool name
    ?assertMatch({ok, <<"my_tool">>}, erlmcp_mcp_types:validate_tool_name(<<"my_tool">>)),

    %% Invalid: empty binary
    ?assertMatch({error, invalid_tool_name}, erlmcp_mcp_types:validate_tool_name(<<"">>)).

validate_resource_uri_test() ->
    %% Valid resource URI
    ?assertMatch({ok, <<"file:///path">>}, erlmcp_mcp_types:validate_resource_uri(<<"file:///path">>)),

    %% Invalid: empty binary
    ?assertMatch({error, invalid_resource_uri}, erlmcp_mcp_types:validate_resource_uri(<<"">>)).

validate_session_id_test() ->
    %% Valid session ID
    ?assertMatch({ok, <<"session-123">>}, erlmcp_mcp_types:validate_session_id(<<"session-123">>)),

    %% Invalid: empty binary
    ?assertMatch({error, invalid_session_id}, erlmcp_mcp_types:validate_session_id(<<"">>)).

%%====================================================================
%% Generation Tests
%%====================================================================

generate_request_id_test() ->
    Id = erlmcp_mcp_types:generate_request_id(),
    ?assert(is_binary(Id)),
    ?assert(byte_size(Id) > 0),
    %% Check UUID format (8-4-4-4-12 pattern)
    ?assertMatch({_, _, _, _, _}, re:run(Id, <<"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$">>, [caseless])).

generate_session_id_test() ->
    Id = erlmcp_mcp_types:generate_session_id(),
    ?assert(is_binary(Id)),
    ?assert(byte_size(Id) > 0),
    %% Check UUID format
    ?assertMatch({_, _, _, _, _}, re:run(Id, <<"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$">>, [caseless])).

generate_task_id_test() ->
    Id = erlmcp_mcp_types:generate_task_id(),
    ?assert(is_binary(Id)),
    ?assert(byte_size(Id) > 0),
    %% Check UUID format
    ?assertMatch({_, _, _, _, _}, re:run(Id, <<"^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$">>, [caseless])).

generate_progress_token_test() ->
    Token = erlmcp_mcp_types:generate_progress_token(),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) =:= 16).  %% 128 bits

generate_cursor_token_test() ->
    Token = erlmcp_mcp_types:generate_cursor_token(),
    ?assert(is_binary(Token)),
    ?assert(byte_size(Token) > 0).

%%====================================================================
%% Nominal Type Safety Tests
%%====================================================================

%% This test demonstrates that nominal types prevent type confusion
%% The commented-out code below would FAIL Dialyzer type checking:

%% nominal_type_safety_test_() ->
%%     {setup,
%%      fun() ->
%%          RequestId = erlmcp_mcp_types:new_request_id(<<"req-123">>),
%%          ToolName = erlmcp_mcp_types:new_tool_name(<<"my_tool">>),
%%          ResourceUri = erlmcp_mcp_types:new_resource_uri(<<"file:///path">>),
%%          SessionId = erlmcp_mcp_types:new_session_id(<<"session-abc">>),
%%          {RequestId, ToolName, ResourceUri, SessionId}
%%      end,
%%      fun({RequestId, ToolName, ResourceUri, SessionId}) ->
%%          [
%%           %% This test demonstrates the TYPE ERROR that Dialyzer catches
%%           %% Uncomment the following to see Dialyzer fail:
%%           fun() ->
%%               %% WRONG: Passing ToolName where RequestId is expected
%%               %% Dialyzer ERROR: The call patterns for erlmcp_json_rpc:encode_request/3
%%               %% do not match the type specification
%%               ?assertError(badarg, erlmcp_json_rpc:encode_request(ToolName, <<"method">>, #{}))
%%           end,
%%
%%           fun() ->
%%               %% WRONG: Passing RequestId where ToolName is expected
%%               %% Dialyzer ERROR: Type mismatch
%%               ?assertError(badarg, erlmcp_tool:validate_tool_name(RequestId))
%%           end,
%%
%%           fun() ->
%%               %% WRONG: Passing ResourceUri where SessionId is expected
%%               %% Dialyzer ERROR: Type mismatch
%%               ?assertError(badarg, erlmcp_session_backend:store(ResourceUri, #{}))
%%           end
%%          ]
%%      end
%%     }.

%%====================================================================
%% Integration Tests
%%====================================================================

%% Test that nominal types work correctly in real usage scenarios
integration_test_() ->
    {setup,
     fun() ->
         ok
     end,
     fun(_) ->
         [
          fun() ->
              %% Create proper nominal types
              RequestId = erlmcp_mcp_types:new_request_id(<<"req-001">>),
              ToolName = erlmcp_mcp_types:new_tool_name(<<"test_tool">>),
              ResourceUri = erlmcp_mcp_types:new_resource_uri(<<"file:///test.txt">>),
              SessionId = erlmcp_mcp_types:new_session_id(<<"sess-001">>),

              %% All validations should pass
              ?assertEqual({ok, RequestId}, erlmcp_mcp_types:validate_request_id(RequestId)),
              ?assertEqual({ok, ToolName}, erlmcp_mcp_types:validate_tool_name(ToolName)),
              ?assertEqual({ok, ResourceUri}, erlmcp_mcp_types:validate_resource_uri(ResourceUri)),
              ?assertEqual({ok, SessionId}, erlmcp_mcp_types:validate_session_id(SessionId))
          end
         ]
     end
    }.

%%====================================================================
%% Dialyzer Compliance Test
%%====================================================================

%% This test file should pass Dialyzer with no warnings
%% Run: rebar3 dialyzer
%%
%% Expected result: 0 warnings
%%
%% The nominal type declarations ensure that:
%% 1. mcp_request_id() cannot be confused with mcp_tool_name()
%% 2. mcp_resource_uri() cannot be confused with mcp_session_id()
%% 3. All nominal types are distinct even though they're all binary()

dialyzer_compliance_info_test() ->
    %% This test documents the expected Dialyzer behavior
    ?assert(true),
    {comment, "Run 'rebar3 dialyzer' to verify nominal type enforcement"}.
