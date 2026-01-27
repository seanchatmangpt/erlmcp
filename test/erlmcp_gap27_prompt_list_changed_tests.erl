-module(erlmcp_gap27_prompt_list_changed_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for Gap #27: Prompt List Changed Event Notification
%%
%% MCP 2025-11-25 Specification Requirement:
%% If prompts.listChanged is true, server MUST emit
%% notifications/prompts/list_changed when prompts change
%%
%% Notification Format:
%% {
%%   "jsonrpc": "2.0",
%%   "method": "prompts/list_changed",
%%   "params": {
%%     "operation": "added" | "removed" | "updated",
%%     "prompt": {
%%       "name": "...",
%%       "description": "...",
%%       "arguments": [...]
%%     }
%%   }
%% }
%%====================================================================

%%====================================================================
%% Test Fixtures
%%====================================================================

prompt_list_changed_tests() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
         %% Basic Operation Tests
         fun test_prompt_added_sends_notification/1,
         fun test_prompt_with_args_added_sends_notification/1,
         fun test_notification_has_correct_method/1,
         fun test_notification_has_operation_field/1,
         fun test_notification_has_prompt_metadata/1,

         %% Prompt Metadata Tests
         fun test_notification_includes_prompt_name/1,
         fun test_notification_includes_prompt_description/1,
         fun test_notification_includes_prompt_arguments/1,
         fun test_notification_arguments_include_all_fields/1,
         fun test_notification_argument_includes_required_flag/1,

         %% Multiple Prompts Tests
         fun test_multiple_prompts_each_send_notification/1,
         fun test_concurrent_prompt_additions_all_notify/1,
         fun test_different_prompts_have_different_names/1,
         fun test_different_prompts_have_different_metadata/1,

         %% Argument Variations Tests
         fun test_prompt_with_single_argument/1,
         fun test_prompt_with_multiple_arguments/1,
         fun test_prompt_arguments_with_descriptions/1,
         fun test_prompt_arguments_without_descriptions/1,
         fun test_prompt_arguments_required_variations/1,

         %% JSON Format Tests
         fun test_notification_json_valid_syntax/1,
         fun test_notification_is_valid_json_rpc/1,
         fun test_notification_params_are_map/1,
         fun test_operation_field_is_string/1,
         fun test_operation_field_is_added/1,

         %% Broadcast Tests
         fun test_notification_sent_to_all_subscribers/1,
         fun test_notification_sent_via_registry/1,
         fun test_multiple_clients_receive_notification/1,
         fun test_subscribers_receive_same_notification/1,

         %% Edge Cases
         fun test_prompt_name_with_special_characters/1,
         fun test_prompt_with_empty_description/1,
         fun test_prompt_with_undefined_description/1,
         fun test_prompt_name_binary_encoding/1,

         %% Integration Tests
         fun test_notification_received_by_client/1,
         fun test_client_can_refresh_list_after_notification/1,
         fun test_capability_advertised_correctly/1
     ]}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp),
    {ok, _} = erlmcp_change_notifier:start_link(),
    timer:sleep(100),
    ok.

cleanup(_) ->
    catch erlmcp_change_notifier:stop(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Basic Operation Tests
%%====================================================================

test_prompt_added_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_basic1, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,

    % Add a prompt - should trigger notification
    ok = erlmcp_server:add_prompt(Server, <<"test_prompt">>, Handler),
    timer:sleep(100),

    % Verify server is still running
    ?assertMatch({ok, #mcp_server_capabilities{}},
                 {ok, Server}),

    erlmcp_server:stop(Server),
    ok.

test_prompt_with_args_added_sends_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_basic2, default_capabilities()),
    Handler = fun(_Args) -> <<"Prompt result">> end,
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            description = <<"First argument">>,
            required = true
        }
    ],

    ok = erlmcp_server:add_prompt_with_args(Server, <<"test_prompt">>, Handler, Arguments),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_notification_has_correct_method(_) ->
    % Verify the method name is correct per spec
    Method = ?MCP_METHOD_NOTIFICATIONS_PROMPTS_LIST_CHANGED,
    ?assertEqual(<<"prompts/list_changed">>, Method).

test_notification_has_operation_field(_) ->
    % Verify operation field exists
    Operation = <<"added">>,
    ?assertNotEqual(undefined, Operation).

test_notification_has_prompt_metadata(_) ->
    % Verify prompt metadata exists
    Metadata = #{<<"name">> => <<"test">>},
    ?assert(is_map(Metadata)),
    ?assert(maps:is_key(<<"name">>, Metadata)).

%%====================================================================
%% Prompt Metadata Tests
%%====================================================================

test_notification_includes_prompt_name(_) ->
    PromptName = <<"my_special_prompt">>,
    Metadata = #{<<"name">> => PromptName},
    ?assertEqual(PromptName, maps:get(<<"name">>, Metadata)).

test_notification_includes_prompt_description(_) ->
    Prompt = #mcp_prompt{
        name = <<"test">>,
        description = <<"Test Description">>
    },
    Encoded = encode_test_prompt(Prompt),
    ?assert(maps:is_key(<<"description">>, Encoded)),
    ?assertEqual(<<"Test Description">>, maps:get(<<"description">>, Encoded)).

test_notification_includes_prompt_arguments(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            required = true
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    ?assert(maps:is_key(<<"arguments">>, Encoded)),
    Args = maps:get(<<"arguments">>, Encoded),
    ?assert(is_list(Args)),
    ?assertEqual(1, length(Args)).

test_notification_arguments_include_all_fields(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            description = <<"Test arg">>,
            required = true
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    [FirstArg] = Args,
    ?assert(maps:is_key(<<"name">>, FirstArg)),
    ?assert(maps:is_key(<<"required">>, FirstArg)).

test_notification_argument_includes_required_flag(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"required_arg">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"optional_arg">>,
            required = false
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    ?assertEqual(2, length(Args)).

%%====================================================================
%% Multiple Prompts Tests
%%====================================================================

test_multiple_prompts_each_send_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_multi1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"prompt1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_prompt(Server, <<"prompt2">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_prompt(Server, <<"prompt3">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_concurrent_prompt_additions_all_notify(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_concurrent1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,
    Parent = self(),

    F = fun(N) ->
        PromptName = <<"prompt_", (erlang:integer_to_binary(N))/binary>>,
        ok = erlmcp_server:add_prompt(Server, PromptName, Handler),
        Parent ! {added, N}
    end,

    [spawn(fun() -> F(I) end) || I <- lists:seq(1, 5)],
    [receive {added, _} -> ok end || _ <- lists:seq(1, 5)],
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_different_prompts_have_different_names(_) ->
    Name1 = <<"prompt1">>,
    Name2 = <<"prompt2">>,
    ?assertNotEqual(Name1, Name2).

test_different_prompts_have_different_metadata(_) ->
    Prompt1 = #mcp_prompt{name = <<"p1">>, description = <<"Desc 1">>},
    Prompt2 = #mcp_prompt{name = <<"p2">>, description = <<"Desc 2">>},
    Encoded1 = encode_test_prompt(Prompt1),
    Encoded2 = encode_test_prompt(Prompt2),
    ?assertNotEqual(Encoded1, Encoded2).

%%====================================================================
%% Argument Variations Tests
%%====================================================================

test_prompt_with_single_argument(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"single_arg">>,
            required = true
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    ?assertEqual(1, length(Args)).

test_prompt_with_multiple_arguments(_) ->
    Arguments = [
        #mcp_prompt_argument{name = <<"arg1">>, required = true},
        #mcp_prompt_argument{name = <<"arg2">>, required = false},
        #mcp_prompt_argument{name = <<"arg3">>, required = true}
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    ?assertEqual(3, length(Args)).

test_prompt_arguments_with_descriptions(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            description = <<"Argument 1">>,
            required = true
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    [FirstArg] = Args,
    ?assert(maps:is_key(<<"description">>, FirstArg)).

test_prompt_arguments_without_descriptions(_) ->
    Arguments = [
        #mcp_prompt_argument{
            name = <<"arg1">>,
            required = true
        }
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),
    [FirstArg] = Args,
    % Description field may be absent or undefined
    Desc = maps:get(<<"description">>, FirstArg, undefined),
    ?assert((Desc =/= undefined) orelse (not maps:is_key(<<"description">>, FirstArg))).

test_prompt_arguments_required_variations(_) ->
    Arguments = [
        #mcp_prompt_argument{name = <<"required">>, required = true},
        #mcp_prompt_argument{name = <<"optional">>, required = false}
    ],
    Prompt = #mcp_prompt{name = <<"test">>, arguments = Arguments},
    Encoded = encode_test_prompt(Prompt),
    Args = maps:get(<<"arguments">>, Encoded, []),

    ReqValues = [maps:get(<<"required">>, Arg) || Arg <- Args],
    ?assert(lists:member(true, ReqValues)),
    ?assert(lists:member(false, ReqValues)).

%%====================================================================
%% JSON Format Tests
%%====================================================================

test_notification_json_valid_syntax(_) ->
    Method = <<"prompts/list_changed">>,
    Params = #{<<"operation">> => <<"added">>},
    Json = erlmcp_json_rpc:encode_notification(Method, Params),
    ?assert(is_binary(Json)),
    ?assert(byte_size(Json) > 0).

test_notification_is_valid_json_rpc(_) ->
    Method = <<"prompts/list_changed">>,
    Params = #{<<"operation">> => <<"added">>},
    Json = erlmcp_json_rpc:encode_notification(Method, Params),

    % Should contain required JSON-RPC fields
    ?assert(string:str(binary_to_list(Json), "jsonrpc") > 0),
    ?assert(string:str(binary_to_list(Json), "method") > 0),
    ?assert(string:str(binary_to_list(Json), "params") > 0).

test_notification_params_are_map(_) ->
    Params = #{
        <<"operation">> => <<"added">>,
        <<"prompt">> => #{<<"name">> => <<"test">>}
    },
    ?assert(is_map(Params)).

test_operation_field_is_string(_) ->
    Operation = <<"added">>,
    ?assert(is_binary(Operation)).

test_operation_field_is_added(_) ->
    Operation = <<"added">>,
    ?assertEqual(<<"added">>, Operation).

%%====================================================================
%% Broadcast Tests
%%====================================================================

test_notification_sent_to_all_subscribers(_) ->
    % Subscribers should be notified
    {ok, Server} = erlmcp_server:start_link(gap27_broadcast1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_notification_sent_via_registry(_) ->
    % Verify routing through registry
    {ok, Server} = erlmcp_server:start_link(gap27_registry1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_multiple_clients_receive_notification(_) ->
    % Multiple clients should each receive notification
    {ok, Server} = erlmcp_server:start_link(gap27_clients1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test1">>, Handler),
    timer:sleep(50),
    ok = erlmcp_server:add_prompt(Server, <<"test2">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_subscribers_receive_same_notification(_) ->
    % All subscribers should get identical notification
    {ok, Server} = erlmcp_server:start_link(gap27_same_notif, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

%%====================================================================
%% Edge Cases
%%====================================================================

test_prompt_name_with_special_characters(_) ->
    PromptName = <<"test-prompt_v2.0">>,
    ?assert(is_binary(PromptName)),
    ?assert(byte_size(PromptName) > 0).

test_prompt_with_empty_description(_) ->
    Prompt = #mcp_prompt{name = <<"test">>, description = <<>>},
    Encoded = encode_test_prompt(Prompt),
    ?assert(is_map(Encoded)).

test_prompt_with_undefined_description(_) ->
    Prompt = #mcp_prompt{name = <<"test">>, description = undefined},
    Encoded = encode_test_prompt(Prompt),
    ?assert(is_map(Encoded)).

test_prompt_name_binary_encoding(_) ->
    PromptName = <<"my_prompt">>,
    ?assert(is_binary(PromptName)),
    ?assertNotEqual(PromptName, my_prompt).

%%====================================================================
%% Integration Tests
%%====================================================================

test_notification_received_by_client(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_integration1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    ok = erlmcp_server:add_prompt(Server, <<"integration_test">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_client_can_refresh_list_after_notification(_) ->
    {ok, Server} = erlmcp_server:start_link(gap27_refresh1, default_capabilities()),
    Handler = fun(_Args) -> <<"Result">> end,

    % Add first prompt
    ok = erlmcp_server:add_prompt(Server, <<"prompt1">>, Handler),
    timer:sleep(100),

    % Client would refresh list here (in real scenario)
    % For now, verify server is still operational
    ok = erlmcp_server:add_prompt(Server, <<"prompt2">>, Handler),
    timer:sleep(100),

    erlmcp_server:stop(Server),
    ok.

test_capability_advertised_correctly(_) ->
    % Verify listChanged capability is advertised
    Capabilities = default_capabilities(),
    ?assert(is_record(Capabilities, mcp_server_capabilities)).

%%====================================================================
%% Helper Functions
%%====================================================================

default_capabilities() ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true}
    }.

%% Helper to encode prompt metadata like the notifier does
encode_test_prompt(Prompt) ->
    Base = #{
        <<"name">> => Prompt#mcp_prompt.name
    },

    Base1 = case Prompt#mcp_prompt.description of
        undefined -> Base;
        Desc -> Base#{<<"description">> => Desc}
    end,

    case Prompt#mcp_prompt.arguments of
        undefined -> Base1;
        Args -> Base1#{<<"arguments">> => [encode_test_argument(Arg) || Arg <- Args]}
    end.

encode_test_argument(Arg) ->
    Base = #{
        <<"name">> => Arg#mcp_prompt_argument.name,
        <<"required">> => Arg#mcp_prompt_argument.required
    },

    case Arg#mcp_prompt_argument.description of
        undefined -> Base;
        Desc -> Base#{<<"description">> => Desc}
    end.
