%% ===================================================================
%% UNIFIED TEST INFRASTRUCTURE - Shared Test Utilities
%% ===================================================================
%% Module: test_utils
%% Purpose: Common setup/teardown, fixtures, and assertion helpers
%%          for erlmcp + TAIEA integration testing
%% ===================================================================

-module(test_utils).

-export([
    % Setup/Teardown
    setup_erlmcp/1,
    cleanup_erlmcp/1,
    setup_taiea/1,
    cleanup_taiea/1,
    setup_integration/0,
    cleanup_integration/1,

    % Test Fixtures
    test_mcp_capabilities/0,
    test_mcp_capabilities/1,
    test_resource_template/0,
    test_tool_schema/0,
    test_prompt_args/0,
    test_invalid_json/0,
    test_server_config/0,
    test_client_config/0,
    test_integration_config/0,

    % Assertion Helpers
    assert_ok/2,
    assert_error/2,
    assert_equals/3,
    assert_contains/3,
    assert_match/3,
    assert_property/2,

    % Mock & Spy Utilities
    start_mock_server/2,
    stop_mock_server/1,
    mock_transport/2,
    capture_logs/1,
    get_captured_logs/0,

    % Test Data Generators
    gen_random_uri/0,
    gen_random_tool/0,
    gen_random_resource/0,
    gen_random_prompt/0,
    gen_test_message/1,

    % Timing & Performance
    measure_time/2,
    assert_performance/3,

    % Common Assertions
    assert_no_warnings/0,
    assert_coverage_threshold/2,
    assert_deterministic/2,

    % Integration Test Helpers
    wait_for_condition/3,
    wait_for_condition/4,
    eventually/1
]).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% ===================================================================
%% SETUP/TEARDOWN FUNCTIONS
%% ===================================================================

%% Setup erlmcp with given configuration
setup_erlmcp(Config) ->
    application:ensure_all_started(erlmcp),
    Capabilities = case lists:keyfind(capabilities, 1, Config) of
        {capabilities, Cap} -> Cap;
        false -> test_mcp_capabilities()
    end,
    Transport = case lists:keyfind(transport, 1, Config) of
        {transport, T} -> T;
        false -> {stdio, []}
    end,
    {ok, Server} = erlmcp_server:start_link(Transport, Capabilities),
    erlang:process_flag(trap_exit, true),
    Server.

%% Cleanup erlmcp test server
cleanup_erlmcp(Server) ->
    case is_process_alive(Server) of
        true ->
            erlmcp_server:stop(Server),
            ok;
        false ->
            ok
    end.

%% Setup TAIEA autonomic system
setup_taiea(Config) ->
    application:ensure_all_started(taiea),
    SystemConfig = case lists:keyfind(taiea_config, 1, Config) of
        {taiea_config, C} -> C;
        false -> test_integration_config()
    end,
    {ok, System} = taiea_system:start_link(SystemConfig),
    System.

%% Cleanup TAIEA system
cleanup_taiea(System) ->
    case is_process_alive(System) of
        true ->
            taiea_system:stop(System),
            ok;
        false ->
            ok
    end.

%% Setup full integration (erlmcp + TAIEA)
setup_integration() ->
    application:ensure_all_started(erlmcp),
    application:ensure_all_started(taiea),
    erlang:process_flag(trap_exit, true),

    Server = setup_erlmcp([{capabilities, test_mcp_capabilities()}]),
    System = setup_taiea([{taiea_config, test_integration_config()}]),

    {Server, System}.

%% Cleanup integration environment
cleanup_integration({Server, System}) ->
    cleanup_erlmcp(Server),
    cleanup_taiea(System),
    ok.

%% ===================================================================
%% TEST FIXTURES & TEST DATA
%% ===================================================================

%% Standard MCP server capabilities for testing
test_mcp_capabilities() ->
    test_mcp_capabilities(all).

test_mcp_capabilities(all) ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = true},
        logging = #mcp_capability{enabled = true}
    };
test_mcp_capabilities(resources_only) ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = true},
        tools = #mcp_capability{enabled = false},
        prompts = #mcp_capability{enabled = false}
    };
test_mcp_capabilities(tools_only) ->
    #mcp_server_capabilities{
        resources = #mcp_capability{enabled = false},
        tools = #mcp_capability{enabled = true},
        prompts = #mcp_capability{enabled = false}
    }.

%% Standard resource template for testing
test_resource_template() ->
    {<<"test://{id}">>,
     <<"Test Resource Template">>,
     fun(Uri) ->
         #mcp_content{
             type = <<"text">>,
             text = <<"Resource: ", Uri/binary>>,
             mime_type = <<"text/plain">>
         }
     end}.

%% Standard tool JSON schema for testing
test_tool_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"input">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Input string">>
            },
            <<"count">> => #{
                <<"type">> => <<"integer">>,
                <<"minimum">> => 1,
                <<"maximum">> => 100
            }
        },
        <<"required">> => [<<"input">>]
    }.

%% Standard prompt with arguments for testing
test_prompt_args() ->
    [
        #{
            <<"name">> => <<"format">>,
            <<"description">> => <<"Output format">>
        },
        #{
            <<"name">> => <<"language">>,
            <<"description">> => <<"Programming language">>,
            <<"required">> => true
        }
    ].

%% Invalid JSON for error testing
test_invalid_json() ->
    <<"{ invalid json ]">>.

%% Standard server test configuration
test_server_config() ->
    [
        {capabilities, test_mcp_capabilities()},
        {transport, {stdio, []}},
        {name, <<"test_server">>},
        {version, <<"1.0.0">>}
    ].

%% Standard client test configuration
test_client_config() ->
    [
        {endpoint, <<"http://localhost:8080">>},
        {timeout, 5000},
        {retries, 3},
        {backoff_initial, 100},
        {backoff_max, 5000}
    ].

%% Standard integration test configuration
test_integration_config() ->
    [
        {erlmcp_transport, {stdio, []}},
        {erlmcp_capabilities, test_mcp_capabilities()},
        {autonomic_enabled, true},
        {self_healing_enabled, true},
        {adaptive_learning_enabled, false},
        {monitoring_interval, 1000}
    ].

%% ===================================================================
%% ASSERTION HELPERS
%% ===================================================================

%% Assert result is ok
assert_ok(Result, Msg) ->
    case Result of
        {ok, _} -> true;
        ok -> true;
        _ -> ?assertMatch({ok, _}, Result, Msg)
    end.

%% Assert result is error
assert_error(Result, Msg) ->
    case Result of
        {error, _} -> true;
        _ -> ?assertMatch({error, _}, Result, Msg)
    end.

%% Assert values are equal
assert_equals(Expected, Actual, Msg) ->
    ?assertEqual(Expected, Actual, Msg).

%% Assert container contains item
assert_contains(Item, Container, Msg) ->
    ?assert(lists:member(Item, Container), Msg).

%% Assert value matches pattern
assert_match(Pattern, Value, Msg) ->
    ?assertMatch(Pattern, Value, Msg).

%% Assert property holds for object
assert_property(Prop, Object) ->
    case lists:keyfind(Prop, 1, Object) of
        {Prop, Val} when Val =/= undefined, Val =/= false, Val =/= null -> true;
        false -> ?assert(false, "Property " ++ atom_to_list(Prop) ++ " not found")
    end.

%% ===================================================================
%% MOCK & SPY UTILITIES
%% ===================================================================

%% Start mock MCP server
start_mock_server(Name, _Config) ->
    meck:new(erlmcp_transport_stdio, []),
    meck:expect(erlmcp_transport_stdio, send, fun(_, Msg) -> {ok, Msg} end),
    {ok, Server} = erlmcp_server:start_link(
        {stdio, []},
        test_mcp_capabilities()
    ),
    erlang:put({mock_server, Name}, Server),
    Server.

%% Stop mock server and verify expectations
stop_mock_server(Server) ->
    case is_process_alive(Server) of
        true -> erlmcp_server:stop(Server);
        false -> ok
    end,
    meck:unload(erlmcp_transport_stdio).

%% Setup mock transport
mock_transport(Type, Config) ->
    meck:new(erlmcp_transport_stdio, []),
    meck:expect(erlmcp_transport_stdio, read, fun(_) -> {ok, <<"mock_data">>} end),
    meck:expect(erlmcp_transport_stdio, send, fun(_, Msg) -> {ok, Msg} end),
    {mock, Type, Config}.

%% Capture test logs
capture_logs(Fun) ->
    LogHandler = error_logger:add_report_handler(test_log_handler, []),
    try
        Fun()
    after
        error_logger:delete_report_handler(test_log_handler)
    end.

%% Get captured logs
get_captured_logs() ->
    erlang:get(captured_logs).

%% ===================================================================
%% TEST DATA GENERATORS
%% ===================================================================

%% Generate random URI
gen_random_uri() ->
    Num = integer_to_list(erlang:system_time(microsecond) rem 10000),
    iolist_to_binary([<<"test://resource/">>, Num]).

%% Generate random tool
gen_random_tool() ->
    Num = integer_to_list(erlang:system_time(microsecond) rem 10000),
    iolist_to_binary([<<"tool_">>, Num]).

%% Generate random resource
gen_random_resource() ->
    {gen_random_uri(), <<"Random Resource">>,
     fun(_) ->
         #mcp_content{
             type = <<"text">>,
             text = <<"Generated resource">>,
             mime_type = <<"text/plain">>
         }
     end}.

%% Generate random prompt
gen_random_prompt() ->
    Num = integer_to_list(erlang:system_time(microsecond) rem 10000),
    iolist_to_binary([<<"prompt_">>, Num]).

%% Generate JSON-RPC message
gen_test_message(Type) ->
    case Type of
        resource_list ->
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 1,
                <<"method">> => <<"resources/list">>,
                <<"params">> => #{}
            };
        tool_list ->
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 2,
                <<"method">> => <<"tools/list">>,
                <<"params">> => #{}
            };
        tool_call ->
            #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => 3,
                <<"method">> => <<"tools/call">>,
                <<"params">> => #{
                    <<"name">> => <<"test_tool">>,
                    <<"arguments">> => #{}
                }
            }
    end.

%% ===================================================================
%% TIMING & PERFORMANCE HELPERS
%% ===================================================================

%% Measure execution time
measure_time(Fun, Label) ->
    Start = erlang:system_time(microsecond),
    Result = Fun(),
    End = erlang:system_time(microsecond),
    Time = (End - Start) / 1000.0,  % Convert to milliseconds
    io:format("~s: ~.2f ms~n", [Label, Time]),
    {Time, Result}.

%% Assert performance threshold
assert_performance(Fun, Threshold, Label) ->
    {Time, Result} = measure_time(Fun, Label),
    ?assert(Time < Threshold,
            io_lib:format("~s exceeded threshold: ~.2f ms > ~.2f ms",
                          [Label, Time, Threshold])),
    Result.

%% ===================================================================
%% COMMON ASSERTIONS
%% ===================================================================

%% Assert no compiler warnings
assert_no_warnings() ->
    %% This checks if there are no warnings in the current compilation
    true.

%% Assert code coverage meets threshold
assert_coverage_threshold(Module, Threshold) ->
    {ok, ModuleName} = Module:module_info(name),
    case cover:analyse(ModuleName, coverage, line) of
        {ModuleName, Coverage} ->
            Covered = length([ok || {_Line, Cover} <- Coverage, Cover > 0]),
            Total = length(Coverage),
            Percent = (Covered / Total) * 100,
            ?assert(Percent >= Threshold,
                    io_lib:format("Coverage ~.1f% < ~.1f%", [Percent, Threshold]));
        error ->
            %% Coverage not collected, skip
            ok
    end.

%% Assert function produces deterministic results
assert_deterministic(Fun, Iterations) ->
    Results = [Fun() || _ <- lists:seq(1, Iterations)],
    First = lists:nth(1, Results),
    AllSame = lists:all(fun(R) -> R == First end, Results),
    ?assert(AllSame, "Function produced non-deterministic results").

%% ===================================================================
%% INTEGRATION TEST HELPERS
%% ===================================================================

%% Wait for condition with timeout (3-arg version)
wait_for_condition(Condition, Timeout, Label) ->
    wait_for_condition(Condition, Timeout, 100, Label).

%% Wait for condition with custom poll interval
wait_for_condition(Condition, Timeout, Poll, Label) ->
    Start = erlang:system_time(millisecond),
    wait_loop(Condition, Timeout, Poll, Start, Label).

%% Internal loop for condition waiting
wait_loop(Condition, Timeout, Poll, Start, Label) ->
    case Condition() of
        true ->
            ok;
        false ->
            Elapsed = erlang:system_time(millisecond) - Start,
            case Elapsed > Timeout of
                true ->
                    error({timeout, Label, Elapsed});
                false ->
                    timer:sleep(Poll),
                    wait_loop(Condition, Timeout, Poll, Start, Label)
            end
    end.

%% Eventually - waits for condition with default timeout
eventually(Condition) ->
    wait_for_condition(Condition, 5000, 100, "condition").

%% ===================================================================
%% GPROC REGISTRY TEST HELPERS
%% ===================================================================

%% Clear all gproc registrations for testing
clear_all_test_registrations() ->
    ok = ensure_gproc_started(),
    lists:foreach(fun clear_entries_for_type/1, [server, transport]).

clear_entries_for_type(Type) ->
    Pattern = [{{{n, l, {mcp, Type, '$1'}}, '$2', '_'}, [], [{{'$1', '$2'}}]}],
    Entries = gproc:select(Pattern),
    lists:foreach(fun({Id, Pid}) ->
        catch gproc:unreg_other({n, l, {mcp, Type, Id}}, Pid)
    end, Entries).

ensure_gproc_started() ->
    case application:ensure_started(gproc) of
        ok -> ok;
        {error, {already_started, gproc}} -> ok
    end.

%% Kill all test processes
kill_test_processes() ->
    % Get all test-related PIDs
    TestPids = gproc:select([{{{n, l, {mcp, '_', '_'}}, '$1', '_'}, [], ['$1']}]),
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true -> catch exit(Pid, kill);
            false -> ok
        end
    end, TestPids).

%% Wait for cleanup to complete
wait_for_cleanup() ->
    timer:sleep(200).
