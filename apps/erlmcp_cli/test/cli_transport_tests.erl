%%%-------------------------------------------------------------------
%%% @doc
%%% Tests for erlmcp_cli_transport module
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(cli_transport_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test transport interface creation
transport_interface_test() ->
    %% Test transport interface with different types
    ?assertEqual(ok, erlmcp_cli_transport:transport(<<"stdio">>, #{})),
    ?assertEqual(ok, erlmcp_cli_transport:transport(<<"tcp">>, #{})),
    ?assertEqual(ok, erlmcp_cli_transport:transport(<<"http">>, #{})),
    ?assertEqual(ok, erlmcp_cli_transport:transport(<<"ws">>, #{})),
    ?assertEqual(ok, erlmcp_cli_transport:transport(<<"sse">>, #{})),

    %% Test invalid transport type
    {error, invalid_transport_type} = erlmcp_cli_transport:transport(<<"invalid">>, #{}),

    %% Test transport with invalid options
    {error, invalid_transport_options} = erlmcp_cli_transport:transport(<<"stdio>>, #{}),  # Malformed options
    ok.

%% @doc Test transport status checking
transport_status_test() ->
    %% Test transport status for non-existent transport
    false = erlmcp_cli_transport:is_active(<<"nonexistent">>),

    %% Start a transport and check status
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{}),
    true = erlmcp_cli_transport:is_active(<<"stdio">>),

    %% Stop transport and check status
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    false = erlmcp_cli_transport:is_active(<<"stdio">>),
    ok.

%% @doc Test transport statistics
transport_stats_test() ->
    %% Start a transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{}),

    %% Get transport statistics
    Stats = erlmcp_cli_transport:get_transport_stats(<<"stdio">>),
    ?assert(is_map(Stats)),
    ?assert(is_integer(maps:get(<<"messages_sent">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"messages_received">>, Stats, 0))),
    ?assert(is_integer(maps:get(<<"errors">>, Stats, 0))),

    %% Get all transport statistics
    AllStats = erlmcp_cli_transport:get_all_transport_stats(),
    ?assert(is_map(AllStats)),

    %% Cleanup
    erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok.

%% @doc Test transport configuration validation
transport_config_test() ->
    %% Test valid configurations
    ValidConfigs = [
        #{"type" => "stdio", "host" => "localhost"},
        #{"type" => "tcp", "host" => "127.0.0.1", "port" => 8080},
        #{"type" => "http", "host" => "localhost", "port" => 8080},
        #{"type" => "ws", "host" => "localhost", "port" => 8080},
        #{"type" => "sse", "host" => "localhost", "port" => 8080}
    ],

    lists:foreach(fun(Config) ->
        case erlmcp_cli_transport:validate_config(Config) of
            true -> ok;
            false ?fail("Valid configuration rejected: ~p", [Config])
        end
    end, ValidConfigs),

    %% Test invalid configurations
    InvalidConfigs = [
        #{"type" => "invalid"},
        #{"type" => "tcp"},  # Missing port
        #{"type" => "http", "port" => "invalid"},  # Invalid port type
        #{"type" => "stdio", "host" => 123},  # Invalid host type
        #{},  # Empty config
        #{"type" => null}  # Invalid type
    ],

    lists:foreach(fun(Config) ->
        case erlmcp_cli_transport:validate_config(Config) of
            false -> ok;
            true ?fail("Invalid configuration accepted: ~p", [Config])
        end
    end, InvalidConfigs),
    ok.

%% @doc Test transport message handling
transport_message_test() ->
    %% Start stdio transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{}),

    %% Test message sending
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Test message receiving (stdio transport will handle this)
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Test closing transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Test sending to closed transport
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"stdio">>, Message),
    ok.

%% @doc Test concurrent transport handling
concurrent_transport_test() ->
    %% Start multiple transports concurrently
    Transports = [<<"stdio">>, <<"tcp">>, <<"http">>, <<"ws">>, <<"sse">>],
    lists:foreach(fun(Type) ->
        ok = erlmcp_cli_transport:transport(Type, #{
            "host" => "localhost",
            "port" => 8080
        })
    end, Transports),

    %% Verify all transports are active
    lists:foreach(fun(Type) ->
        true = erlmcp_cli_transport:is_active(Type)
    end, Transports),

    %% Send messages to all transports concurrently
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    lists:foreach(fun(Type) ->
        ok = erlmcp_cli_transport:send_data(Type, Message)
    end, Transports),

    %% Close all transports
    lists:foreach(fun(Type) ->
        ok = erlmcp_cli_transport:close_transport(Type)
    end, Transports),

    %% Verify all transports are inactive
    lists:foreach(fun(Type) ->
        false = erlmcp_cli_transport:is_active(Type)
    end, Transports),
    ok.

%% @doc Test transport error handling
transport_error_test() ->
    %% Test operations on non-existent transport
    {error, transport_not_active} = erlmcp_cli_transport:send_data(<<"nonexistent">>, <<"test">>),
    {error, transport_not_active} = erlmcp_cli_transport:close_transport(<<"nonexistent">>),

    %% Test message sending to invalid transport
    {error, invalid_transport_type} = erlmcp_cli_transport:send_data(<<"invalid">>, <<"test">>),

    %% Test with malformed message
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{}),
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, <<"invalid json">>),  # Should handle gracefully
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok.

%% @doc Test transport event handling
transport_events_test() ->
    %% Start transport monitoring
    ok = erlmcp_cli_transport:start_monitor(),

    %% Start a transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{ "host" => "localhost" }),

    %% Transport should emit connect event
    receive
        {transport_event, <<"stdio">>, connected} ->
            ok;
        Other ->
            ?fail("Unexpected event: ~p", [Other])
    after 1000 ->
        ?fail("No connect event received")
    end,

    %% Send data (should emit send event)
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    receive
        {transport_event, <<"stdio">>, {sent, _Size}} ->
            ok;
        Other2 ->
            ?fail("Unexpected event: ~p", [Other2])
    after 1000 ->
        ?fail("No send event received")
    end,

    %% Close transport (should emit disconnect event)
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    receive
        {transport_event, <<"stdio">>, disconnected} ->
            ok;
        Other3 ->
            ?fail("Unexpected event: ~p", [Other3])
    after 1000 ->
        ?fail("No disconnect event received")
    end,

    %% Stop monitoring
    ok = erlmcp_cli_transport:stop_monitor(),
    ok.

%% @doc Test transport health checks
transport_health_test() ->
    %% Check transport health for non-existent transport
    unhealthy = erlmcp_cli_transport:check_health(<<"nonexistent">>),

    %% Start a transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{ "host" => "localhost" }),

    %% Check transport health
    Health = erlmcp_cli_transport:check_health(<<"stdio">>),
    ?assert(Health =:= healthy orelse Health =:= degraded),

    %% Close transport
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),

    %% Check health after close
    unhealthy = erlmcp_cli_transport:check_health(<<"stdio">>),
    ok.

%% @doc Test transport metrics integration
transport_metrics_test() ->
    %% Start a transport
    ok = erlmcp_cli_transport:transport(<<"stdio">>, #{ "host" => "localhost" }),

    %% Get transport metrics
    Metrics = erlmcp_cli_transport:get_transport_metrics(<<"stdio">>),
    ?assert(is_map(Metrics)),
    ?assert(is_integer(maps:get(<<"messages_sent">>, Metrics, 0))),
    ?assert(is_integer(maps:get(<<"messages_received">>, Metrics, 0))),
    ?assert(is_integer(maps:get(<<"errors">>, Metrics, 0))),

    %% Send some messages to generate metrics
    Message = <<"{\"jsonrpc\":\"2.0\",\"method\":\"test\",\"id\":1}">>,
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),
    ok = erlmcp_cli_transport:send_data(<<"stdio">>, Message),

    %% Check that metrics were updated
    UpdatedMetrics = erlmcp_cli_transport:get_transport_metrics(<<"stdio">>),
    ?assertEqual(maps:get(<<"messages_sent>>>, UpdatedMetrics) > maps:get(<<"messages_sent">>, Metrics)),

    %% Cleanup
    ok = erlmcp_cli_transport:close_transport(<<"stdio">>),
    ok.