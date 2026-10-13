-module(erlmcp_tool_registry_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    {ok, _Pid} = erlmcp_tool_registry:start_link(),
    ok.

cleanup(_) ->
    case whereis(erlmcp_tool_registry) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid, normal, 5000)
    end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

tool_registry_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [{"Register tool", fun test_register_tool/0},
      {"Unregister tool", fun test_unregister_tool/0},
      {"Get tool", fun test_get_tool/0},
      {"List tools", fun test_list_tools/0},
      {"Get tool version", fun test_get_tool_version/0},
      {"Update tool version", fun test_update_tool_version/0},
      {"Check tool updated", fun test_check_tool_updated/0},
      {"Subscribe to updates", fun test_subscribe_to_updates/0},
      {"Unsubscribe from updates", fun test_unsubscribe_from_updates/0}]}.

%%====================================================================
%% Individual Tests
%%====================================================================

test_register_tool() ->
    Name = <<"test_tool">>,
    Module = erlmcp_json_rpc,
    Metadata = #{description => <<"Test tool">>},

    Result = erlmcp_tool_registry:register_tool(Name, Module, Metadata),

    ?assertEqual(ok, Result).

test_unregister_tool() ->
    %% First register a tool
    Name = <<"temp_tool">>,
    Module = erlmcp_json_rpc,
    Metadata = #{},

    ok = erlmcp_tool_registry:register_tool(Name, Module, Metadata),

    %% Then unregister it
    Result = erlmcp_tool_registry:unregister_tool(Name),

    ?assertEqual(ok, Result),

    %% Verify it's gone
    ?assertEqual({error, not_found}, erlmcp_tool_registry:get_tool(Name)).

test_get_tool() ->
    %% Register a tool
    Name = <<"get_test_tool">>,
    Module = erlmcp_registry,
    Metadata = #{type => test},

    ok = erlmcp_tool_registry:register_tool(Name, Module, Metadata),

    %% Get it back
    Result = erlmcp_tool_registry:get_tool(Name),

    ?assertMatch({ok, #tool{name = Name, module = Module}}, Result).

test_list_tools() ->
    %% Register multiple tools
    Tools = [
        {<<"tool1">>, erlmcp_json_rpc, #{}},
        {<<"tool2">>, erlmcp_registry, #{}},
        {<<"tool3">>, erlmcp_code_reload, #{}}
    ],

    lists:foreach(fun({Name, Module, Meta}) ->
                    ok = erlmcp_tool_registry:register_tool(Name, Module, Meta)
                 end, Tools),

    %% List all tools
    Result = erlmcp_tool_registry:list_tools(),

    ?assertEqual(3, length(Result)),

    %% Verify tool names
    ToolNames = [T#tool.name || T <- Result],
    ?assert(lists:member(<<"tool1">>, ToolNames)),
    ?assert(lists:member(<<"tool2">>, ToolNames)),
    ?assert(lists:member(<<"tool3">>, ToolNames)).

test_get_tool_version() ->
    Name = <<"version_tool">>,
    Module = erlmcp_json_rpc,

    ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),

    Result = erlmcp_tool_registry:get_tool_version(Name),

    ?assertMatch({ok, _Version}, Result),

    %% Version should be binary
    {ok, Version} = Result,
    ?assert(is_binary(Version)).

test_update_tool_version() ->
    Name = <<"update_tool">>,
    Module = erlmcp_json_rpc,

    ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),

    %% Get original version
    {ok, OriginalVersion} = erlmcp_tool_registry:get_tool_version(Name),

    %% Update to new version
    NewVersion = <<"new_version_hash">>,
    Result = erlmcp_tool_registry:update_tool_version(Name, NewVersion),

    ?assertEqual(ok, Result),

    %% Verify version changed
    {ok, UpdatedVersion} = erlmcp_tool_registry:get_tool_version(Name),
    ?assertEqual(NewVersion, UpdatedVersion),
    ?assertNotEqual(OriginalVersion, UpdatedVersion).

test_check_tool_updated() ->
    Name = <<"check_tool">>,
    Module = erlmcp_json_rpc,

    ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),

    %% Initially not updated
    ?assertEqual(false, erlmcp_tool_registry:check_tool_updated(Name)),

    %% Update version
    {ok, OriginalVersion} = erlmcp_tool_registry:get_tool_version(Name),
    NewVersion = <<"updated_version">>,
    ok = erlmcp_tool_registry:update_tool_version(Name, NewVersion),

    %% Now should be updated
    ?assertEqual(true, erlmcp_tool_registry:check_tool_updated(Name)).

test_subscribe_to_updates() ->
    Name = <<"subscribe_tool">>,
    Module = erlmcp_json_rpc,

    ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),

    %% Subscribe
    Result = erlmcp_tool_registry:subscribe_to_tool_updates(Name),

    ?assertEqual(ok, Result).

test_unsubscribe_from_updates() ->
    Name = <<"unsubscribe_tool">>,
    Module = erlmcp_registry,

    ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),

    %% Subscribe
    ok = erlmcp_tool_registry:subscribe_to_tool_updates(Name),

    %% Unsubscribe
    Result = erlmcp_tool_registry:unsubscribe_from_tool_updates(Name),

    ?assertEqual(ok).

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_tool_version_persisted() ->
    ?FORALL({Name, Module},
            {binary(10), elements([erlmcp_json_rpc, erlmcp_registry])},
            begin
                ok = erlmcp_tool_registry:register_tool(Name, Module, #{}),
                {ok, Version} = erlmcp_tool_registry:get_tool_version(Name),
                is_binary(Version) andalso byte_size(Version) > 0
            end).

prop_list_tools_complete() ->
    ?FORALL(ToolCount,
            range(1, 10),
            begin
                %% Register tools
                lists:foreach(fun(N) ->
                                    Name = <<"tool", (integer_to_binary(N))/binary>>,
                                    ok = erlmcp_tool_registry:register_tool(Name, erlmcp_json_rpc, #{})
                              end, lists:seq(1, ToolCount)),

                %% List tools
                Tools = erlmcp_tool_registry:list_tools(),

                length(Tools) >= ToolCount
            end).
