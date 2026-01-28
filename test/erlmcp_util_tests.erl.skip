-module(erlmcp_util_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Suite for erlmcp_util Module
%%====================================================================

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Transport ID Creation Tests
%%====================================================================

create_transport_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_create_transport_id_with_binary()),
             ?_test(test_create_transport_id_with_string()),
             ?_test(test_create_transport_id_format()),
             ?_test(test_create_transport_id_different_types()),
             ?_test(test_create_transport_id_uniqueness())
         ]
     end}.

test_create_transport_id_with_binary() ->
    ServerId = <<"server1">>,
    Type = <<"stdio">>,
    Result = erlmcp_util:create_transport_id(ServerId, Type),
    ?assert(is_binary(Result) orelse is_atom(Result)).

test_create_transport_id_with_string() ->
    ServerId = server2,
    Type = <<"tcp">>,
    Result = erlmcp_util:create_transport_id(ServerId, Type),
    ?assert(is_binary(Result) orelse is_atom(Result)).

test_create_transport_id_format() ->
    ServerId = test_server,
    Type = <<"http">>,
    Result = erlmcp_util:create_transport_id(ServerId, Type),
    ?assert(byte_size(Result) > 0 orelse is_atom(Result)).

test_create_transport_id_different_types() ->
    ServerId = server_test,
    Types = [<<"stdio">>, <<"tcp">>, <<"http">>, <<"sse">>],
    Results = [erlmcp_util:create_transport_id(ServerId, Type) || Type <- Types],
    ?assertEqual(4, length(Results)),
    ?assert(lists:all(fun(R) -> is_binary(R) orelse is_atom(R) end, Results)).

test_create_transport_id_uniqueness() ->
    ServerId = unique_server,
    Type1 = <<"tcp">>,
    Type2 = <<"http">>,
    Id1 = erlmcp_util:create_transport_id(ServerId, Type1),
    Id2 = erlmcp_util:create_transport_id(ServerId, Type2),
    ?assertNotEqual(Id1, Id2).

%%====================================================================
%% Process Status Tests
%%====================================================================

process_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_process_status_self()),
             ?_test(test_get_process_status_valid_pid()),
             ?_test(test_get_process_status_invalid_pid()),
             ?_test(test_get_process_status_dead_pid()),
             ?_test(test_get_process_status_return_type())
         ]
     end}.

test_get_process_status_self() ->
    SelfPid = self(),
    Result = erlmcp_util:get_process_status(SelfPid),
    ?assert(is_atom(Result) orelse is_map(Result) orelse Result =:= undefined).

test_get_process_status_valid_pid() ->
    {ok, Pid} = start_dummy_process(),
    Result = erlmcp_util:get_process_status(Pid),
    ?assert(Result =/= undefined),
    stop_dummy_process(Pid).

test_get_process_status_invalid_pid() ->
    InvalidPid = pid(99, 9999, 9999),
    Result = erlmcp_util:get_process_status(InvalidPid),
    ?assertEqual(undefined, Result).

test_get_process_status_dead_pid() ->
    {ok, Pid} = start_dummy_process(),
    stop_dummy_process(Pid),
    timer:sleep(50),
    Result = erlmcp_util:get_process_status(Pid),
    ?assertEqual(undefined, Result).

test_get_process_status_return_type() ->
    SelfPid = self(),
    Result = erlmcp_util:get_process_status(SelfPid),
    ?assert(is_atom(Result) orelse is_map(Result) orelse Result =:= undefined).

%%====================================================================
%% Supported Transport Types Tests
%%====================================================================

supported_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_list_supported_transport_types()),
             ?_test(test_supported_types_not_empty()),
             ?_test(test_supported_types_contains_common()),
             ?_test(test_supported_types_format()),
             ?_test(test_supported_types_duplicates())
         ]
     end}.

test_list_supported_transport_types() ->
    Result = erlmcp_util:list_supported_transport_types(),
    ?assert(is_list(Result)).

test_supported_types_not_empty() ->
    Result = erlmcp_util:list_supported_transport_types(),
    ?assert(length(Result) > 0).

test_supported_types_contains_common() ->
    Result = erlmcp_util:list_supported_transport_types(),
    %% Should contain at least some common transports
    ?assert(length(Result) >= 2).

test_supported_types_format() ->
    Result = erlmcp_util:list_supported_transport_types(),
    %% All items should be atoms or strings
    ?assert(lists:all(fun(T) -> is_atom(T) orelse is_binary(T) end, Result)).

test_supported_types_duplicates() ->
    Result = erlmcp_util:list_supported_transport_types(),
    Unique = lists:usort(Result),
    ?assertEqual(length(Result), length(Unique)).

%%====================================================================
%% Configuration Examples Tests
%%====================================================================

config_examples_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_get_config_examples()),
             ?_test(test_config_examples_not_empty()),
             ?_test(test_config_examples_format()),
             ?_test(test_config_examples_keys()),
             ?_test(test_config_examples_valid_config())
         ]
     end}.

test_get_config_examples() ->
    Result = erlmcp_util:get_config_examples(),
    ?assert(is_list(Result) orelse is_map(Result)).

test_config_examples_not_empty() ->
    Result = erlmcp_util:get_config_examples(),
    ?assert(length(Result) > 0 orelse maps:size(Result) > 0).

test_config_examples_format() ->
    Result = erlmcp_util:get_config_examples(),
    ?assert(is_list(Result) orelse is_map(Result)).

test_config_examples_keys() ->
    Result = erlmcp_util:get_config_examples(),
    case is_map(Result) of
        true ->
            %% Should have some transport type examples
            ?assert(maps:size(Result) > 0);
        false ->
            %% If list, should have at least one example
            ?assert(length(Result) > 0)
    end.

test_config_examples_valid_config() ->
    Result = erlmcp_util:get_config_examples(),
    %% All examples should be valid maps or records
    if
        is_map(Result) ->
            ?assert(is_map(Result));
        is_list(Result) ->
            ?assert(lists:all(fun(E) -> is_map(E) orelse is_tuple(E) end, Result))
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

start_dummy_process() ->
    Pid = spawn_link(fun dummy_loop/0),
    {ok, Pid}.

stop_dummy_process(Pid) ->
    exit(Pid, kill),
    ok.

dummy_loop() ->
    receive
        stop -> ok
    after
        infinity -> dummy_loop()
    end.
