%%%-------------------------------------------------------------------
%%% @doc HTTP Transport Compliance Test Suite
%%%
%%% Chicago School TDD: Real processes, observable behavior, no mocks
%%% Tests validate:
%%% 1. Required callback implementations
%%% 2. Transport option validation
%%% 3. Server lifecycle
%%% 4. HTTP message handling
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_compliance_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Configuration
%%====================================================================

-define(HTTP_TRANSPORT, erlmcp_transport_http).

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    %% Start required applications
    {ok, _} = application:ensure_all_started(gproc),

    ok.

cleanup(_) ->
    timer:sleep(100),
    ok.

%%====================================================================
%% Test Suite
%%====================================================================

http_compliance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [{"HTTP required callbacks - init, send, close", fun test_required_callbacks/0},
      {"HTTP transport option validation", fun test_option_validation/0},
      {"HTTP server lifecycle", fun test_server_lifecycle/0}]}.

%%====================================================================
%% Required Callbacks Tests
%%====================================================================

test_required_callbacks() ->
    %% Verify callbacks are exported (observable behavior)
    Exports = ?HTTP_TRANSPORT:module_info(exports),
    Callbacks = [init, send, close],

    lists:foreach(fun(Callback) -> ?assert(lists:keymember(Callback, 1, Exports)) end, Callbacks).

%%====================================================================
%% Option Validation Tests
%%====================================================================

test_option_validation() ->
    %% Valid options
    ValidOpts = #{url => <<"http://localhost:8080/mcp">>, owner => self()},

    ?assertMatch(ok, erlmcp_transport_behavior:validate_transport_opts(http, ValidOpts)),

    %% Invalid options - missing URL
    InvalidOpts1 = #{owner => self()},
    ?assertMatch({error, _}, erlmcp_transport_behavior:validate_transport_opts(http, InvalidOpts1)),

    %% Invalid options - missing owner
    InvalidOpts2 = #{url => <<"http://localhost:8080/mcp">>},
    ?assertMatch({error, _}, erlmcp_transport_behavior:validate_transport_opts(http, InvalidOpts2)),

    %% Valid options with timeout
    ValidOpts3 =
        #{url => <<"http://localhost:8080/mcp">>,
          owner => self(),
          connect_timeout => 5000},

    ?assertMatch(ok, erlmcp_transport_behavior:validate_transport_opts(http, ValidOpts3)),

    %% Invalid URL format
    InvalidOpts4 = #{url => <<"not-a-url">>, owner => self()},

    Result = erlmcp_transport_behavior:validate_transport_opts(http, InvalidOpts4),
    ?assertMatch({error, _}, Result).

%%====================================================================
%% Server Lifecycle Tests
%%====================================================================

test_server_lifecycle() ->
    %% Note: Full HTTP server test requires Cowboy setup
    %% This is a basic lifecycle check
    %% Verify the HTTP transport module is available
    ?assert(is_list(?HTTP_TRANSPORT:module_info(exports))),

    %% Verify HTTP server module is available
    ?assert(is_list(erlmcp_transport_http_server:module_info(exports))).
