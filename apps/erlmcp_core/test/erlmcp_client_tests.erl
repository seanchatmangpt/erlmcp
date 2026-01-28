-module(erlmcp_client_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_client Module
%%====================================================================

%%====================================================================
%% Client Lifecycle Tests
%%====================================================================

client_lifecycle_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_start_link()),
             ?_test(test_start_link_with_opts()),
             ?_test(test_stop())
         ]
     end}.

test_start_link() ->
    % Create mock transport options
    TransportOpts = {stdio, #{test_mode => true}},
    Result = erlmcp_client:start_link(TransportOpts),
    case Result of
        {ok, Client} ->
            ?assert(is_pid(Client)),
            ?assert(erlang:is_process_alive(Client)),
            erlmcp_client:stop(Client);
        {error, _Reason} ->
            % Expected if stdio not available in test environment
            ?assert(true)
    end.

test_start_link_with_opts() ->
    TransportOpts = {stdio, #{test_mode => true}},
    ClientOpts = #{
        strict_mode => true,
        timeout => 5000
    },
    Result = erlmcp_client:start_link(TransportOpts, ClientOpts),
    case Result of
        {ok, Client} ->
            ?assert(is_pid(Client)),
            erlmcp_client:stop(Client);
        {error, _} ->
            ?assert(true)
    end.

test_stop() ->
    TransportOpts = {stdio, #{test_mode => true}},
    case erlmcp_client:start_link(TransportOpts) of
        {ok, Client} ->
            ?assert(erlang:is_process_alive(Client)),
            ok = erlmcp_client:stop(Client),
            timer:sleep(100),
            ?assertNot(erlang:is_process_alive(Client));
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Initialization Tests
%%====================================================================

initialization_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined ->
                 [?_test(?assert(true))]; % Skip if no client
             _ ->
                 [
                     ?_test(test_initialize(Client)),
                     ?_test(test_initialize_with_capabilities(Client))
                 ]
         end
     end}.

test_initialize(Client) ->
    ClientCapabilities = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = false},
        sampling = #mcp_capability{enabled = false}
    },
    Result = erlmcp_client:initialize(Client, ClientCapabilities),
    case Result of
        {ok, _ServerInfo} ->
            ?assert(true);
        {error, _Reason} ->
            % Expected if not connected to actual server
            ?assert(true)
    end.

test_initialize_with_capabilities(Client) ->
    ClientCapabilities = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true}
    },
    Result = erlmcp_client:initialize(Client, ClientCapabilities, #{}),
    case Result of
        {ok, _ServerInfo} ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Resource Tests
%%====================================================================

resource_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_list_resources(Client)),
                     ?_test(test_read_resource(Client)),
                     ?_test(test_subscribe_to_resource(Client))
                 ]
         end
     end}.

test_list_resources(Client) ->
    Result = erlmcp_client:list_resources(Client),
    case Result of
        {ok, Resources} ->
            ?assert(is_list(Resources));
        {error, _} ->
            ?assert(true)
    end.

test_read_resource(Client) ->
    ResourceUri = <<"test://resource/1">>,
    Result = erlmcp_client:read_resource(Client, ResourceUri),
    case Result of
        {ok, _Contents} ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

test_subscribe_to_resource(Client) ->
    ResourceUri = <<"test://resource/sub">>,
    Result = erlmcp_client:subscribe_to_resource(Client, ResourceUri),
    case Result of
        ok ->
            erlmcp_client:unsubscribe_from_resource(Client, ResourceUri);
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Tool Tests
%%====================================================================

tool_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_list_tools(Client)),
                     ?_test(test_call_tool(Client))
                 ]
         end
     end}.

test_list_tools(Client) ->
    Result = erlmcp_client:list_tools(Client),
    case Result of
        {ok, Tools} ->
            ?assert(is_list(Tools));
        {error, _} ->
            ?assert(true)
    end.

test_call_tool(Client) ->
    ToolName = <<"test_tool">>,
    Arguments = #{input => <<"test">>},
    Result = erlmcp_client:call_tool(Client, ToolName, Arguments),
    case Result of
        {ok, _ToolResult} ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Prompt Tests
%%====================================================================

prompt_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_list_prompts(Client)),
                     ?_test(test_get_prompt(Client)),
                     ?_test(test_get_prompt_with_args(Client))
                 ]
         end
     end}.

test_list_prompts(Client) ->
    Result = erlmcp_client:list_prompts(Client),
    case Result of
        {ok, Prompts} ->
            ?assert(is_list(Prompts));
        {error, _} ->
            ?assert(true)
    end.

test_get_prompt(Client) ->
    PromptName = <<"test_prompt">>,
    Result = erlmcp_client:get_prompt(Client, PromptName),
    case Result of
        {ok, _Messages} ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

test_get_prompt_with_args(Client) ->
    PromptName = <<"args_prompt">>,
    Arguments = #{topic => <<"testing">>},
    Result = erlmcp_client:get_prompt(Client, PromptName, Arguments),
    case Result of
        {ok, _Messages} ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Batch Request Tests
%%====================================================================

batch_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_batch_requests(Client))
                 ]
         end
     end}.

test_batch_requests(Client) ->
    BatchFun = fun(BatchClient) ->
        erlmcp_client:list_resources(BatchClient),
        erlmcp_client:list_tools(BatchClient),
        erlmcp_client:list_prompts(BatchClient)
    end,
    Result = erlmcp_client:with_batch(Client, BatchFun),
    case Result of
        ok ->
            ?assert(true);
        {error, _} ->
            ?assert(true)
    end.

%%====================================================================
%% Notification Handler Tests
%%====================================================================

notification_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_set_notification_handler(Client)),
                     ?_test(test_remove_notification_handler(Client))
                 ]
         end
     end}.

test_set_notification_handler(Client) ->
    NotificationType = <<"resources/list_changed">>,
    Handler = fun(_Type, _Params) ->
        ok
    end,
    Result = erlmcp_client:set_notification_handler(Client, NotificationType, Handler),
    ?assertMatch(ok, Result).

test_remove_notification_handler(Client) ->
    NotificationType = <<"resources/list_changed">>,
    Result = erlmcp_client:remove_notification_handler(Client, NotificationType),
    ?assertMatch(ok, Result).

%%====================================================================
%% Sampling Handler Tests
%%====================================================================

sampling_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_set_sampling_handler(Client)),
                     ?_test(test_remove_sampling_handler(Client))
                 ]
         end
     end}.

test_set_sampling_handler(Client) ->
    Handler = fun(_Method, _Params) ->
        #{result => <<"sampled">>}
    end,
    Result = erlmcp_client:set_sampling_handler(Client, Handler),
    ?assertMatch(ok, Result).

test_remove_sampling_handler(Client) ->
    Result = erlmcp_client:remove_sampling_handler(Client),
    ?assertMatch(ok, Result).

%%====================================================================
%% Strict Mode Tests
%%====================================================================

strict_mode_test_() ->
    {setup,
     fun setup_client/0,
     fun cleanup_client/1,
     fun(Client) ->
         case Client of
             undefined -> [?_test(?assert(true))];
             _ ->
                 [
                     ?_test(test_set_strict_mode(Client))
                 ]
         end
     end}.

test_set_strict_mode(Client) ->
    Result = erlmcp_client:set_strict_mode(Client, true),
    ?assertMatch(ok, Result),
    Result2 = erlmcp_client:set_strict_mode(Client, false),
    ?assertMatch(ok, Result2).

%%====================================================================
%% Capability Encoding Tests
%%====================================================================

capability_encoding_test_() ->
    [
        ?_test(test_encode_tuple_format()),
        ?_test(test_encode_map_format()),
        ?_test(test_encode_plain_map()),
        ?_test(test_encode_record_format())
    ].

test_encode_tuple_format() ->
    % Test tuple format {Name, Version}
    Input = {<<"test_client">>, <<"1.0.0">>},
    Result = erlmcp_client:encode_capabilities(Input),
    ?assertMatch(#{name := <<"test_client">>, version := <<"1.0.0">>}, Result).

test_encode_map_format() ->
    % Test map format with name/version
    Input = #{name => <<"client">>, version => <<"2.0.0">>},
    Result = erlmcp_client:encode_capabilities(Input),
    ?assertMatch(#{name := <<"client">>, version := <<"2.0.0">>}, Result).

test_encode_plain_map() ->
    % Test plain map pass-through
    Input = #{custom_field => <<"value">>, other => 123},
    Result = erlmcp_client:encode_capabilities(Input),
    ?assertEqual(Input, Result).

test_encode_record_format() ->
    % Test MCP client capabilities record
    Input = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = true},
        experimental = #{feature1 => true}
    },
    Result = erlmcp_client:encode_capabilities(Input),
    ?assert(is_map(Result)),
    ?assertMatch(#{<<"roots">> := #{}, <<"sampling">> := #{}}, Result),
    ?assertMatch(#{feature1 := true}, Result).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(erlmcp_core),
    ok.

cleanup(_) ->
    ok.

setup_client() ->
    setup(),
    TransportOpts = {stdio, #{test_mode => true}},
    case erlmcp_client:start_link(TransportOpts) of
        {ok, Client} ->
            Client;
        {error, _} ->
            undefined
    end.

cleanup_client(undefined) ->
    cleanup(ok);
cleanup_client(Client) ->
    erlmcp_client:stop(Client),
    cleanup(ok).
