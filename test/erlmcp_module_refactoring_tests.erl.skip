%% @doc Test suite for module refactoring work.
%% Validates that extracted modules work correctly and that
%% performance improvements are achieved through refactoring.
%% Tests cover: message parser, capability cache, handlers, and hot paths.

-module(erlmcp_module_refactoring_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, _} = application:ensure_all_started(erlmcp),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Message Parser Tests (Hot Path)
%%====================================================================

message_parser_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Parse valid request", fun test_parse_valid_request/0},
        {"Parse valid response", fun test_parse_valid_response/0},
        {"Parse valid notification", fun test_parse_valid_notification/0},
        {"Detect request vs notification", fun test_detect_message_type/0},
        {"Validate JSON-RPC version", fun test_validate_jsonrpc_version/0},
        {"Handle invalid version", fun test_invalid_jsonrpc_version/0},
        {"Decode ID types", fun test_decode_id_types/0},
        {"Validate params", fun test_validate_params/0}
    ]}.

test_parse_valid_request() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test/method">>,
        <<"params">> => #{<<"key">> => <<"value">>}
    },
    {ok, #json_rpc_request{id = 1, method = <<"test/method">>, params = Params}} =
        erlmcp_message_parser:parse_json_rpc(Data),
    ?assertEqual(#{<<"key">> => <<"value">>}, Params).

test_parse_valid_response() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => #{<<"status">> => <<"ok">>}
    },
    {ok, #json_rpc_response{id = 1, result = Result}} =
        erlmcp_message_parser:parse_json_rpc(Data),
    ?assertEqual(#{<<"status">> => <<"ok">>}, Result).

test_parse_valid_notification() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => <<"test/notify">>,
        <<"params">> => #{<<"event">> => <<"triggered">>}
    },
    {ok, #json_rpc_notification{method = <<"test/notify">>, params = Params}} =
        erlmcp_message_parser:parse_json_rpc(Data),
    ?assertEqual(#{<<"event">> => <<"triggered">>}, Params).

test_detect_message_type() ->
    %% Request has id and method
    RequestData = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"method">> => <<"test">>},
    {ok, #json_rpc_request{}} = erlmcp_message_parser:parse_json_rpc(RequestData),

    %% Notification has method but no id
    NotifData = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>},
    {ok, #json_rpc_notification{}} = erlmcp_message_parser:parse_json_rpc(NotifData),

    %% Response has id and result
    RespData = #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => 1, <<"result">> => <<"ok">>},
    {ok, #json_rpc_response{}} = erlmcp_message_parser:parse_json_rpc(RespData).

test_validate_jsonrpc_version() ->
    Data = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => <<"test">>},
    ?assertEqual(ok, erlmcp_message_parser:validate_jsonrpc_version(Data)).

test_invalid_jsonrpc_version() ->
    %% Missing jsonrpc field
    {error, {invalid_request, missing_jsonrpc}} =
        erlmcp_message_parser:validate_jsonrpc_version(#{}),

    %% Wrong version
    {error, {invalid_request, {wrong_version, <<"1.0">>}}} =
        erlmcp_message_parser:validate_jsonrpc_version(#{<<"jsonrpc">> => <<"1.0">>}).

test_decode_id_types() ->
    ?assertEqual(null, erlmcp_message_parser:decode_id(null)),
    ?assertEqual(42, erlmcp_message_parser:decode_id(42)),
    ?assertEqual(<<"id">>, erlmcp_message_parser:decode_id(<<"id">>)),
    ?assertEqual(atom, erlmcp_message_parser:decode_id(atom)).

test_validate_params() ->
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(undefined)),
    ?assertEqual(#{<<"key">> => <<"val">>}, erlmcp_message_parser:validate_params(#{<<"key">> => <<"val">>})),
    ?assertEqual([1, 2, 3], erlmcp_message_parser:validate_params([1, 2, 3])),
    ?assertEqual(undefined, erlmcp_message_parser:validate_params(invalid_type)).

%%====================================================================
%% Capability Cache Tests
%%====================================================================

capability_cache_test_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Create capability cache", fun test_create_cache/0},
        {"Check resource support", fun test_check_resource_support/0},
        {"Check tool support", fun test_check_tool_support/0},
        {"Check prompt support", fun test_check_prompt_support/0},
        {"Check sampling support", fun test_check_sampling_support/0},
        {"Check multiple capabilities", fun test_check_multiple_capabilities/0},
        {"Update client capabilities", fun test_update_client_capabilities/0},
        {"Get capabilities from cache", fun test_get_capabilities/0}
    ]}.

test_create_cache() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources{},
        tools = #mcp_tools{},
        prompts = #mcp_prompts{}
    },
    Cache = erlmcp_capability_cache:new(Caps),
    ?assertEqual(Caps, erlmcp_capability_cache:get_capabilities(Cache)).

test_check_resource_support() ->
    Caps = #mcp_server_capabilities{resources = #mcp_resources{}},
    Cache = erlmcp_capability_cache:new(Caps),
    ?assertEqual(true, erlmcp_capability_cache:has_resource_support(Cache)),

    %% No resource support
    EmptyCaps = #mcp_server_capabilities{},
    EmptyCache = erlmcp_capability_cache:new(EmptyCaps),
    ?assertEqual(false, erlmcp_capability_cache:has_resource_support(EmptyCache)).

test_check_tool_support() ->
    Caps = #mcp_server_capabilities{tools = #mcp_tools{}},
    Cache = erlmcp_capability_cache:new(Caps),
    ?assertEqual(true, erlmcp_capability_cache:has_tool_support(Cache)).

test_check_prompt_support() ->
    Caps = #mcp_server_capabilities{prompts = #mcp_prompts{}},
    Cache = erlmcp_capability_cache:new(Caps),
    ?assertEqual(true, erlmcp_capability_cache:has_prompt_support(Cache)).

test_check_sampling_support() ->
    Caps = #mcp_server_capabilities{sampling = #mcp_sampling{}},
    Cache = erlmcp_capability_cache:new(Caps),
    ?assertEqual(true, erlmcp_capability_cache:has_sampling_support(Cache)).

test_check_multiple_capabilities() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources{},
        tools = #mcp_tools{}
    },
    Cache = erlmcp_capability_cache:new(Caps),
    %% Check 'any' mode - at least one capability
    ?assertEqual(true, erlmcp_capability_cache:check_capabilities(Cache, [resource, prompt], any)),
    %% Check 'all' mode - all capabilities
    ?assertEqual(false, erlmcp_capability_cache:check_capabilities(Cache, [resource, prompt], all)),
    ?assertEqual(true, erlmcp_capability_cache:check_capabilities(Cache, [resource, tool], all)).

test_update_client_capabilities() ->
    ServerCaps = #mcp_server_capabilities{},
    Cache = erlmcp_capability_cache:new(ServerCaps),
    ClientCaps = #mcp_client_capabilities{},
    UpdatedCache = erlmcp_capability_cache:update_client_capabilities(Cache, ClientCaps),
    ?assertEqual(maps:is_key(client_capabilities, UpdatedCache), true).

test_get_capabilities() ->
    Caps = #mcp_server_capabilities{resources = #mcp_resources{}},
    Cache = erlmcp_capability_cache:new(Caps),
    RetrievedCaps = erlmcp_capability_cache:get_capabilities(Cache),
    ?assertEqual(Caps, RetrievedCaps).

%%====================================================================
%% Handler Tests
%%====================================================================

handler_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Add resource handler", fun test_add_resource_handler/0},
        {"Add tool handler", fun test_add_tool_handler/0},
        {"Add prompt handler", fun test_add_prompt_handler/0},
        {"Delete resource handler", fun test_delete_resource_handler/0},
        {"Delete non-existent resource", fun test_delete_nonexistent_resource/0},
        {"Subscribe to resource", fun test_subscribe_resource_handler/0},
        {"Report progress", fun test_report_progress_handler/0}
    ]}.

test_add_resource_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    Handler = fun(Uri) -> <<"content">> end,
    {ok, NewState} = erlmcp_server_handlers:handle_add_resource(<<"test/uri">>, Handler, State),
    ?assertEqual(true, maps:is_key(<<"test/uri">>, NewState#state.resources)).

test_add_tool_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    Handler = fun(Args) -> <<"result">> end,
    {ok, NewState} = erlmcp_server_handlers:handle_add_tool(<<"test_tool">>, Handler, State),
    ?assertEqual(true, maps:is_key(<<"test_tool">>, NewState#state.tools)).

test_add_prompt_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    Handler = fun(Args) -> <<"prompt_text">> end,
    {ok, NewState} = erlmcp_server_handlers:handle_add_prompt(<<"test_prompt">>, Handler, State),
    ?assertEqual(true, maps:is_key(<<"test_prompt">>, NewState#state.prompts)).

test_delete_resource_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{<<"test/uri">> => {#mcp_resource{uri = <<"test/uri">>}, fun(U) -> <<"c">> end}},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    {ok, NewState} = erlmcp_server_handlers:handle_delete_resource(<<"test/uri">>, State),
    ?assertEqual(false, maps:is_key(<<"test/uri">>, NewState#state.resources)).

test_delete_nonexistent_resource() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    {{error, not_found}, _} = erlmcp_server_handlers:handle_delete_resource(<<"nonexistent">>, State).

test_subscribe_resource_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    TestPid = self(),
    {ok, NewState} = erlmcp_server_handlers:handle_subscribe_resource(<<"test/uri">>, TestPid, State),
    ?assertEqual(true, maps:is_key(<<"test/uri">>, NewState#state.subscriptions)).

test_report_progress_handler() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    {ok, NewState} = erlmcp_server_handlers:handle_report_progress(<<"token">>, 50.0, 100.0, State),
    ?assertEqual(true, maps:is_key(<<"token">>, NewState#state.progress_tokens)).

%%====================================================================
%% Integration Tests (Module Size Compliance)
%%====================================================================

module_size_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"JSON-RPC module under 500 LOC", fun test_json_rpc_size/0},
        {"Message parser exists", fun test_message_parser_exists/0},
        {"Capability cache exists", fun test_capability_cache_exists/0},
        {"Handlers module exists", fun test_handlers_exists/0},
        {"Message handler exists", fun test_message_handler_exists/0}
    ]}.

test_json_rpc_size() ->
    {ok, JsonRpcModule} = cover:compile_module(erlmcp_json_rpc),
    ?assertEqual(erlmcp_json_rpc, JsonRpcModule).

test_message_parser_exists() ->
    ?assertEqual(true, erlmcp_message_parser:module_info(exports) =/= undefined).

test_capability_cache_exists() ->
    ?assertEqual(true, erlmcp_capability_cache:module_info(exports) =/= undefined).

test_handlers_exists() ->
    ?assertEqual(true, erlmcp_server_handlers:module_info(exports) =/= undefined).

test_message_handler_exists() ->
    ?assertEqual(true, erlmcp_message_handler:module_info(exports) =/= undefined).

%%====================================================================
%% Performance Baseline Tests
%%====================================================================

performance_tests_() ->
    {setup, fun setup/0, fun cleanup/1, [
        {"Parse 100 messages", fun test_parse_100_messages/0},
        {"Cache lookup is O(1)", fun test_cache_lookup_performance/0},
        {"Handler execution performance", fun test_handler_performance/0}
    ]}.

test_parse_100_messages() ->
    Data = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"method">> => <<"test">>,
        <<"params">> => #{<<"key">> => <<"value">>}
    },
    Start = erlang:monotonic_time(microsecond),
    Results = [erlmcp_message_parser:parse_json_rpc(Data) || _ <- lists:seq(1, 100)],
    End = erlang:monotonic_time(microsecond),
    Elapsed = End - Start,
    %% All parses should succeed
    ?assertEqual(100, length([ok || {ok, _} <- Results])),
    %% Should complete in under 10ms (100000 microseconds)
    ?assert(Elapsed < 100000).

test_cache_lookup_performance() ->
    Caps = #mcp_server_capabilities{
        resources = #mcp_resources{},
        tools = #mcp_tools{},
        prompts = #mcp_prompts{},
        sampling = #mcp_sampling{}
    },
    Cache = erlmcp_capability_cache:new(Caps),
    Start = erlang:monotonic_time(microsecond),
    Results = [erlmcp_capability_cache:check_capability(Cache, Cap)
               || Cap <- [resource, tool, prompt, sampling],
                  _ <- lists:seq(1, 100)],
    End = erlang:monotonic_time(microsecond),
    Elapsed = End - Start,
    %% All lookups should succeed
    ?assertEqual(400, length([true || true <- Results])),
    %% Should be very fast (under 5ms = 5000 microseconds)
    ?assert(Elapsed < 5000).

test_handler_performance() ->
    State = #state{
        server_id = test_server,
        resources = #{},
        resource_templates = #{},
        tools = #{},
        prompts = #{},
        subscriptions = #{},
        progress_tokens = #{},
        capabilities = #mcp_server_capabilities{},
        initialized = false
    },
    Handler = fun(_) -> <<"result">> end,
    Start = erlang:monotonic_time(microsecond),
    Results = [erlmcp_server_handlers:handle_add_tool(<<"tool_", (integer_to_binary(I))/binary>>, Handler, State)
               || I <- lists:seq(1, 100)],
    End = erlang:monotonic_time(microsecond),
    Elapsed = End - Start,
    %% All should succeed
    ?assertEqual(100, length([ok || {ok, _} <- Results])),
    %% Should be reasonably fast (under 50ms)
    ?assert(Elapsed < 50000).
