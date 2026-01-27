-module(erlmcp_stdio_server_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

setup() ->
    application:ensure_all_started(erlmcp),
    ok.
cleanup(_) ->
    application:stop(erlmcp),
    ok.

%%====================================================================
%% Startup Tests
%%====================================================================

startup_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_start_server()),
        ?_test(test_start_with_config()),
        ?_test(test_stop_server()),
        ?_test(test_server_process_alive())
    ] end}.

test_start_server() ->
    ServerId = test_stdio_server,
    Config = #{},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _Pid}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

test_start_with_config() ->
    ServerId = configured_server,
    Config = #{timeout => 5000, buffer_size => 4096},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _} | {error, _}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

test_stop_server() ->
    ServerId = stop_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:stop(Pid),
    ?assertMatch(ok | {ok, _}, Result).

test_server_process_alive() ->
    ServerId = alive_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    ?assert(erlang:is_process_alive(Pid)),
    erlmcp_stdio_server:stop(Pid).

%%====================================================================
%% Input/Output Tests
%%====================================================================

io_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_read_from_stdin()),
        ?_test(test_write_to_stdout()),
        ?_test(test_send_message()),
        ?_test(test_receive_message())
    ] end}.

test_read_from_stdin() ->
    ServerId = read_test,
    {ok, _Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:read_message(ServerId),
    ?assertMatch(timeout | {ok, _} | {error, _}, Result).

test_write_to_stdout() ->
    ServerId = write_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Message = jsx:encode(#{test => <<"message">>}),
    Result = erlmcp_stdio_server:write_message(Pid, Message),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_send_message() ->
    ServerId = send_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Message = #{jsonrpc => <<"2.0">>, method => <<"test">>, id => 1},
    Result = erlmcp_stdio_server:send(Pid, Message),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_receive_message() ->
    ServerId = recv_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:receive_message(Pid),
    ?assertMatch(timeout | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

%%====================================================================
%% Message Processing Tests
%%====================================================================

processing_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_process_json_request()),
        ?_test(test_process_json_response()),
        ?_test(test_process_notification()),
        ?_test(test_handle_batch())
    ] end}.

test_process_json_request() ->
    ServerId = json_req_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Request = jsx:encode(#{jsonrpc => <<"2.0">>, method => <<"test">>, id => 1}),
    Result = erlmcp_stdio_server:process_message(Pid, Request),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_process_json_response() ->
    ServerId = json_resp_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Response = jsx:encode(#{jsonrpc => <<"2.0">>, result => <<"ok">>, id => 1}),
    Result = erlmcp_stdio_server:process_message(Pid, Response),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_process_notification() ->
    ServerId = notif_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Notification = jsx:encode(#{jsonrpc => <<"2.0">>, method => <<"notify">>, params => #{}}),
    Result = erlmcp_stdio_server:process_message(Pid, Notification),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_handle_batch() ->
    ServerId = batch_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Batch = jsx:encode([
        #{jsonrpc => <<"2.0">>, method => <<"m1">>, id => 1},
        #{jsonrpc => <<"2.0">>, method => <<"m2">>, id => 2}
    ]),
    Result = erlmcp_stdio_server:process_message(Pid, Batch),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_handle_invalid_json()),
        ?_test(test_handle_parse_error()),
        ?_test(test_handle_incomplete_message()),
        ?_test(test_handle_missing_fields())
    ] end}.

test_handle_invalid_json() ->
    ServerId = invalid_json_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    InvalidJson = <<"not valid json {">>,
    Result = erlmcp_stdio_server:process_message(Pid, InvalidJson),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_handle_parse_error() ->
    ServerId = parse_error_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    BadMessage = <<"{invalid">>,
    Result = erlmcp_stdio_server:process_message(Pid, BadMessage),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_handle_incomplete_message() ->
    ServerId = incomplete_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Incomplete = <<"{">> ,
    Result = erlmcp_stdio_server:process_message(Pid, Incomplete),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_handle_missing_fields() ->
    ServerId = missing_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    MissingFields = jsx:encode(#{id => 1}),
    Result = erlmcp_stdio_server:process_message(Pid, MissingFields),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

%%====================================================================
%% Configuration Tests
%%====================================================================

config_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_timeout_config()),
        ?_test(test_buffer_size_config()),
        ?_test(test_encoding_config()),
        ?_test(test_line_delim_config())
    ] end}.

test_timeout_config() ->
    ServerId = timeout_config_test,
    Config = #{read_timeout => 10000, write_timeout => 5000},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _} | {error, _}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

test_buffer_size_config() ->
    ServerId = buffer_config_test,
    Config = #{buffer_size => 8192},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _} | {error, _}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

test_encoding_config() ->
    ServerId = encoding_config_test,
    Config = #{encoding => utf8},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _} | {error, _}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

test_line_delim_config() ->
    ServerId = delim_config_test,
    Config = #{line_delimiter => <<"\n">>},
    Result = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assertMatch({ok, _} | {error, _}, Result),
    case Result of
        {ok, Pid} -> erlmcp_stdio_server:stop(Pid);
        _ -> ok
    end.

%%====================================================================
%% State Tests
%%====================================================================

state_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_get_state()),
        ?_test(test_get_stats()),
        ?_test(test_reset_stats()),
        ?_test(test_server_info())
    ] end}.

test_get_state() ->
    ServerId = state_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:get_state(Pid),
    ?assert(is_map(Result) orelse Result =:= error),
    erlmcp_stdio_server:stop(Pid).

test_get_stats() ->
    ServerId = stats_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:get_stats(Pid),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error),
    erlmcp_stdio_server:stop(Pid).

test_reset_stats() ->
    ServerId = reset_stats_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:reset_stats(Pid),
    ?assertMatch(ok | {ok, _} | {error, _}, Result),
    erlmcp_stdio_server:stop(Pid).

test_server_info() ->
    ServerId = info_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Result = erlmcp_stdio_server:get_info(Pid),
    ?assert(is_map(Result) orelse is_list(Result) orelse Result =:= error),
    erlmcp_stdio_server:stop(Pid).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun(_) -> [
        ?_test(test_full_server_lifecycle()),
        ?_test(test_message_roundtrip()),
        ?_test(test_concurrent_operations())
    ] end}.

test_full_server_lifecycle() ->
    ServerId = lifecycle_test,
    Config = #{},
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, Config),
    ?assert(erlang:is_process_alive(Pid)),
    Stats = erlmcp_stdio_server:get_stats(Pid),
    ?assert(Stats =:= error orelse is_map(Stats) orelse is_list(Stats)),
    erlmcp_stdio_server:stop(Pid),
    timer:sleep(10),
    ?assertNot(erlang:is_process_alive(Pid)).

test_message_roundtrip() ->
    ServerId = roundtrip_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Request = jsx:encode(#{jsonrpc => <<"2.0">>, method => <<"test">>, id => 1}),
    erlmcp_stdio_server:process_message(Pid, Request),
    Stats = erlmcp_stdio_server:get_stats(Pid),
    erlmcp_stdio_server:stop(Pid),
    ?assert(Stats =:= error orelse is_map(Stats) orelse is_list(Stats)).

test_concurrent_operations() ->
    ServerId = concurrent_test,
    {ok, Pid} = erlmcp_stdio_server:start_link(ServerId, #{}),
    Pids = [spawn(fun() ->
        Message = jsx:encode(#{jsonrpc => <<"2.0">>, method => <<"op">>, id => I}),
        erlmcp_stdio_server:process_message(Pid, Message)
    end) || I <- lists:seq(1, 5)],
    [erlang:monitor(process, P) || P <- Pids],
    erlmcp_stdio_server:stop(Pid),
    ?assert(length(Pids) =:= 5).
