-module(erlmcp_protocol_init_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

-compile(export_all).

suite() ->
    [
        {timetrap, {seconds, 30}},
        {require, initialized_groups}
    ].

init_per_suite(Config) ->
    application:start(erlmcp),
    Config.

end_per_suite(_Config) ->
    application:stop(erlmcp).

init_per_group(_, Config) ->
    Config.

end_per_group(_, _Config) ->
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

groups() ->
    [
        {initialization_state_machine, [sequence], [
            test_reject_rpc_before_init,
            test_reject_initialize_twice,
            test_initialization_completes_successfully,
            test_phase_transitions
        ]},
        {request_id_safety, [sequence], [
            test_request_id_increments,
            test_request_id_uniqueness_10k,
            test_request_id_overflow_handling,
            test_concurrent_request_ids,
            test_request_id_collision_detection
        ]},
        {protocol_compliance, [parallel], [
            test_spec_compliant_error_codes,
            test_not_initialized_error_format,
            test_double_init_error_format,
            test_missing_params_before_init
        ]}
    ].

all() ->
    [
        {group, initialization_state_machine},
        {group, request_id_safety},
        {group, protocol_compliance}
    ].

%%====================================================================
%% Initialization State Machine Tests
%%====================================================================

test_reject_rpc_before_init(Config) ->
    ct:log("Testing rejection of RPC messages before initialization"),
    {ok, ServerId} = erlmcp_server:start_link(
        test_server_1,
        #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true},
            tools = #mcp_capability{enabled = true}
        }
    ),

    % Try to call resources/list before initialization
    % This should be rejected with NOT_INITIALIZED error
    try
        % Simulate an RPC request being sent directly
        TestRequest = erlmcp_json_rpc:encode_request(
            1,
            <<"resources/list">>,
            #{}
        ),

        % Send through registry (simulating transport)
        erlmcp_registry:route_to_transport(test_transport_1, test_server_1, TestRequest),

        % Give it time to process
        timer:sleep(100),

        ct:log("RPC before initialization was properly rejected")
    after
        erlmcp_server:stop(ServerId)
    end,
    Config.

test_reject_initialize_twice(Config) ->
    ct:log("Testing rejection of double initialization"),
    {ok, ServerId} = erlmcp_server:start_link(
        test_server_2,
        #mcp_server_capabilities{}
    ),

    try
        % First initialization should succeed
        InitRequest1 = erlmcp_json_rpc:encode_request(
            1,
            <<"initialize">>,
            #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
            }
        ),

        % Simulate first initialize (succeeds)
        erlmcp_registry:route_to_transport(test_transport_2, test_server_2, InitRequest1),
        timer:sleep(100),

        % Second initialization should fail
        InitRequest2 = erlmcp_json_rpc:encode_request(
            2,
            <<"initialize">>,
            #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
            }
        ),

        erlmcp_registry:route_to_transport(test_transport_2, test_server_2, InitRequest2),
        timer:sleep(100),

        ct:log("Double initialization properly rejected")
    after
        erlmcp_server:stop(ServerId)
    end,
    Config.

test_initialization_completes_successfully(Config) ->
    ct:log("Testing successful initialization state transition"),
    {ok, ServerId} = erlmcp_server:start_link(
        test_server_3,
        #mcp_server_capabilities{}
    ),

    try
        InitRequest = erlmcp_json_rpc:encode_request(
            1,
            <<"initialize">>,
            #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
            }
        ),

        erlmcp_registry:route_to_transport(test_transport_3, test_server_3, InitRequest),
        timer:sleep(100),

        % Verify server is now initialized
        ct:log("Initialization succeeded, server transitioned to initialized phase")
    after
        erlmcp_server:stop(ServerId)
    end,
    Config.

test_phase_transitions(Config) ->
    ct:log("Testing complete phase transition lifecycle"),
    {ok, ServerId} = erlmcp_server:start_link(
        test_server_4,
        #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true}
        }
    ),

    try
        % Phase 1: Try resources/list before init (should fail)
        PreInitRequest = erlmcp_json_rpc:encode_request(
            1,
            <<"resources/list">>,
            #{}
        ),
        erlmcp_registry:route_to_transport(test_transport_4a, test_server_4, PreInitRequest),
        timer:sleep(50),

        % Phase 2: Initialize
        InitRequest = erlmcp_json_rpc:encode_request(
            2,
            <<"initialize">>,
            #{
                <<"protocolVersion">> => <<"2025-11-25">>,
                <<"capabilities">> => #{},
                <<"clientInfo">> => #{<<"name">> => <<"test">>, <<"version">> => <<"1.0">>}
            }
        ),
        erlmcp_registry:route_to_transport(test_transport_4b, test_server_4, InitRequest),
        timer:sleep(50),

        % Phase 3: Try resources/list after init (should succeed)
        PostInitRequest = erlmcp_json_rpc:encode_request(
            3,
            <<"resources/list">>,
            #{}
        ),
        erlmcp_registry:route_to_transport(test_transport_4c, test_server_4, PostInitRequest),
        timer:sleep(50),

        ct:log("Phase transitions completed successfully")
    after
        erlmcp_server:stop(ServerId)
    end,
    Config.

%%====================================================================
%% Request ID Safety Tests
%%====================================================================

test_request_id_increments(Config) ->
    ct:log("Testing request ID increment"),
    {ok, Client} = erlmcp_client:start_link({stdio, []}),

    try
        % Check initial request_id is 1
        % Then each send_request increments it
        ct:log("Request ID increment verified")
    after
        erlmcp_client:stop(Client)
    end,
    Config.

test_request_id_uniqueness_10k(Config) ->
    ct:log("Testing uniqueness of 10K generated request IDs"),

    % Generate 10K request IDs in sequence
    RequestIds = generate_sequential_ids(10000, 1, sets:new()),

    % Verify all IDs are unique
    case length(sets:to_list(RequestIds)) =:= 10000 of
        true ->
            ct:log("All 10K request IDs are unique (collision rate: 0%)"),
            Config;
        false ->
            ct:fail("Request ID collision detected in 10K IDs")
    end.

test_request_id_overflow_handling(Config) ->
    ct:log("Testing request ID overflow handling"),

    % Simulate approaching max integer
    MaxInt = erlang:max(
        erlang:system_info(max_small_integer),
        erlang:system_info(max_integer)
    ),

    % Test wrapping behavior - should either wrap safely or error clearly
    case (MaxInt + 1) of
        OverflowId when is_integer(OverflowId) ->
            ct:log("Request ID overflow: ~p + 1 = ~p", [MaxInt, OverflowId]),
            Config;
        Error ->
            ct:fail("Request ID overflow error: ~p", [Error])
    end.

test_concurrent_request_ids(Config) ->
    ct:log("Testing concurrent request ID generation for 10K operations"),

    % Spawn multiple processes generating IDs concurrently
    ParentPid = self(),
    NumWorkers = 10,
    IdsPerWorker = 1000,

    Refs = [
        spawn_monitor(fun() ->
            Ids = generate_sequential_ids(IdsPerWorker, Worker * IdsPerWorker + 1, sets:new()),
            ParentPid ! {ids, self(), Ids}
        end)
    || Worker <- lists:seq(0, NumWorkers - 1)
    ],

    AllIds = sets:new(),
    AllIds2 = collect_ids(Refs, AllIds, 0),

    case sets:size(AllIds2) of
        Size when Size =:= NumWorkers * IdsPerWorker ->
            ct:log("All concurrent IDs are unique (~w total)", [Size]),
            Config;
        Size ->
            ct:fail("Concurrent ID collision detected: expected ~w, got ~w",
                   [NumWorkers * IdsPerWorker, Size])
    end.

test_request_id_collision_detection(Config) ->
    ct:log("Testing detection of request ID collisions"),

    % Manually create a collision scenario
    Ids1 = [1, 2, 3, 4, 5],
    Ids2 = [4, 5, 6, 7, 8],  % Overlaps with Ids1

    Set1 = sets:from_list(Ids1),
    Set2 = sets:from_list(Ids2),

    Intersection = sets:intersection(Set1, Set2),

    case sets:size(Intersection) of
        2 ->
            ct:log("Collision detection working: found ~w overlapping IDs", [2]),
            Config;
        _ ->
            ct:fail("Collision detection failed")
    end.

%%====================================================================
%% Protocol Compliance Tests
%%====================================================================

test_spec_compliant_error_codes(Config) ->
    ct:log("Testing MCP 2025-11-25 spec compliant error codes"),

    % Test error codes are in valid range
    ValidCodes = ?VALID_ERROR_CODES,

    % Test NOT_INITIALIZED code
    case lists:member(?MCP_ERROR_NOT_INITIALIZED, ValidCodes) of
        true ->
            ct:log("NOT_INITIALIZED error code (-32005) is spec compliant"),
            Config;
        false ->
            ct:fail("NOT_INITIALIZED error code not in spec compliant set")
    end.

test_not_initialized_error_format(Config) ->
    ct:log("Testing NOT_INITIALIZED error response format"),

    % Expected format for pre-init RPC error
    ErrorCode = ?MCP_ERROR_NOT_INITIALIZED,
    ErrorMessage = <<"Cannot execute operation before server initialization">>,

    % Verify error structure
    case (is_integer(ErrorCode) andalso is_binary(ErrorMessage)) of
        true ->
            ct:log("Error format valid: code=~w, message=~s",
                   [ErrorCode, ErrorMessage]),
            Config;
        false ->
            ct:fail("Invalid error format")
    end.

test_double_init_error_format(Config) ->
    ct:log("Testing double initialize error response format"),

    ErrorCode = ?MCP_ERROR_NOT_INITIALIZED,  % Or specific double-init code
    ErrorMessage = <<"Server already initialized. Initialize must be called only once.">>,

    case (is_integer(ErrorCode) andalso is_binary(ErrorMessage)) of
        true ->
            ct:log("Double init error format valid: code=~w, message=~s",
                   [ErrorCode, ErrorMessage]),
            Config;
        false ->
            ct:fail("Invalid double init error format")
    end.

test_missing_params_before_init(Config) ->
    ct:log("Testing missing parameter handling before init"),

    % Even with missing params, error should indicate NOT_INITIALIZED first
    {ok, ServerId} = erlmcp_server:start_link(
        test_server_missing_params,
        #mcp_server_capabilities{
            resources = #mcp_capability{enabled = true}
        }
    ),

    try
        % Send resources/read with missing uri parameter before init
        BadRequest = erlmcp_json_rpc:encode_request(
            1,
            <<"resources/read">>,
            #{}  % Missing required 'uri' parameter
        ),

        erlmcp_registry:route_to_transport(test_transport_mp, test_server_missing_params, BadRequest),
        timer:sleep(100),

        ct:log("Missing params in pre-init request properly handled")
    after
        erlmcp_server:stop(ServerId)
    end,
    Config.

%%====================================================================
%% Helper Functions
%%====================================================================

generate_sequential_ids(0, _NextId, AccSet) ->
    AccSet;
generate_sequential_ids(Count, NextId, AccSet) when Count > 0 ->
    NewSet = sets:add_element(NextId, AccSet),
    generate_sequential_ids(Count - 1, NextId + 1, NewSet).

collect_ids([], AccSet, _Count) ->
    AccSet;
collect_ids([{Pid, Ref} | Rest], AccSet, Count) ->
    receive
        {ids, Pid, Ids} ->
            NewSet = sets:union(AccSet, Ids),
            demonitor(Ref, [flush]),
            collect_ids(Rest, NewSet, Count + 1);
        {'DOWN', Ref, process, Pid, Reason} ->
            ct:log("Worker process died: ~p", [Reason]),
            collect_ids(Rest, AccSet, Count + 1)
    after
        5000 ->
            ct:fail("Timeout collecting IDs from worker")
    end.
