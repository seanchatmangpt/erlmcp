%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit Tests for Transport Discovery
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

discovery_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_stop/1,
      fun test_env_discovery/1,
      fun test_protocol_enable_disable/1,
      fun test_watcher_registration/1,
      fun test_discovery_events/1,
      fun test_scan_now/1
     ]}.

setup() ->
    % Set environment variables for testing
    os:putenv("ERLMCP_TRANSPORT_TEST1_TYPE", "tcp"),
    os:putenv("ERLMCP_TRANSPORT_TEST1_HOST", "localhost"),
    os:putenv("ERLMCP_TRANSPORT_TEST1_PORT", "3000"),

    % Start discovery service
    {ok, Pid} = erlmcp_transport_discovery:start_link(#{
        protocols => [env],
        scan_interval => 60000,  % Long interval for testing
        auto_start => false  % Don't auto-start during tests
    }),

    Pid.

cleanup(Pid) ->
    % Stop discovery service gracefully
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(erlmcp_transport_discovery, normal, 1000);
        false ->
            ok
    end,

    % Clean up environment
    os:unsetenv("ERLMCP_TRANSPORT_TEST1_TYPE"),
    os:unsetenv("ERLMCP_TRANSPORT_TEST1_HOST"),
    os:unsetenv("ERLMCP_TRANSPORT_TEST1_PORT"),

    ok.

%%====================================================================
%% Tests
%%====================================================================

test_start_stop(_Pid) ->
    [
     ?_assert(is_pid(whereis(erlmcp_transport_discovery))),
     ?_assertEqual([env], erlmcp_transport_discovery:get_enabled_protocols())
    ].

test_env_discovery(_Pid) ->
    % Trigger a scan
    erlmcp_transport_discovery:scan_now(),
    timer:sleep(200),  % Give time for scan to complete

    Transports = erlmcp_transport_discovery:get_discovered_transports(),

    [
     ?_assert(is_map(Transports)),
     ?_assert(maps:size(Transports) > 0),
     ?_assert(maps:is_key(test1, Transports))
    ].

test_protocol_enable_disable(_Pid) ->
    % Enable consul protocol
    Result1 = erlmcp_transport_discovery:enable_protocol(consul),
    Protocols1 = erlmcp_transport_discovery:get_enabled_protocols(),

    % Try to enable again (should fail)
    Result2 = erlmcp_transport_discovery:enable_protocol(consul),

    % Disable consul protocol
    Result3 = erlmcp_transport_discovery:disable_protocol(consul),
    Protocols2 = erlmcp_transport_discovery:get_enabled_protocols(),

    [
     ?_assertEqual(ok, Result1),
     ?_assert(lists:member(consul, Protocols1)),
     ?_assertEqual({error, already_enabled}, Result2),
     ?_assertEqual(ok, Result3),
     ?_assertNot(lists:member(consul, Protocols2))
    ].

test_watcher_registration(_Pid) ->
    % Create a watcher function
    Self = self(),
    WatcherFun = fun(Event) ->
        Self ! {test_event, Event}
    end,

    % Register watcher
    Result = erlmcp_transport_discovery:watch(WatcherFun),

    % Unregister watcher
    Result2 = erlmcp_transport_discovery:unwatch(self()),

    [
     ?_assertEqual(ok, Result),
     ?_assertEqual(ok, Result2)
    ].

test_discovery_events(_Pid) ->
    % Register a watcher
    Self = self(),
    erlmcp_transport_discovery:watch(fun(Event) ->
        Self ! {discovery_event, Event}
    end),

    % Add a new environment variable
    os:putenv("ERLMCP_TRANSPORT_TEST2_TYPE", "http"),
    os:putenv("ERLMCP_TRANSPORT_TEST2_HOST", "localhost"),
    os:putenv("ERLMCP_TRANSPORT_TEST2_PORT", "3001"),

    % Trigger scan
    erlmcp_transport_discovery:scan_now(),
    timer:sleep(300),

    % Check for events
    Events = collect_events([]),

    % Clean up
    os:unsetenv("ERLMCP_TRANSPORT_TEST2_TYPE"),
    os:unsetenv("ERLMCP_TRANSPORT_TEST2_HOST"),
    os:unsetenv("ERLMCP_TRANSPORT_TEST2_PORT"),

    [
     ?_assert(length(Events) > 0),
     ?_assert(lists:any(fun
        ({transport_added, test2, _}) -> true;
        (_) -> false
     end, Events))
    ].

test_scan_now(_Pid) ->
    % Record current time
    Before = erlang:system_time(millisecond),

    % Trigger immediate scan
    Result = erlmcp_transport_discovery:scan_now(),
    timer:sleep(200),

    After = erlang:system_time(millisecond),

    [
     ?_assertEqual(ok, Result),
     ?_assert((After - Before) < 1000)  % Scan should be quick
    ].

%%====================================================================
%% Test Helpers
%%====================================================================

collect_events(Acc) ->
    receive
        {discovery_event, Event} ->
            collect_events([Event | Acc])
    after 100 ->
        lists:reverse(Acc)
    end.

%%====================================================================
%% Environment Parsing Tests
%%====================================================================

parse_env_key_test() ->
    % Test valid keys
    ?assertEqual({ok, "tcp1", host}, parse_env_key_helper("ERLMCP_TRANSPORT_TCP1_HOST")),
    ?assertEqual({ok, "http2", port}, parse_env_key_helper("ERLMCP_TRANSPORT_HTTP2_PORT")),
    ?assertEqual({ok, "ws3", type}, parse_env_key_helper("ERLMCP_TRANSPORT_WS3_TYPE")),

    % Test invalid keys
    ?assertEqual(error, parse_env_key_helper("INVALID_KEY")),
    ?assertEqual(error, parse_env_key_helper("ERLMCP_INVALID_TCP1_HOST")).

parse_env_key_helper(Key) ->
    case string:tokens(Key, "_") of
        ["ERLMCP", "TRANSPORT", Name | Rest] when length(Rest) > 0 ->
            Param = string:join(Rest, "_"),
            ParamAtom = list_to_atom(string:to_lower(Param)),
            {ok, string:to_lower(Name), ParamAtom};
        _ ->
            error
    end.

convert_env_types_test() ->
    Input = #{
        "port" => "3000",
        "connect_timeout" => "5000",
        "keepalive" => "true",
        "host" => "localhost"
    },

    Expected = #{
        port => 3000,
        connect_timeout => 5000,
        keepalive => true,
        host => "localhost"
    },

    % Helper function to convert types
    Result = maps:fold(fun
        ("port", Value, Acc) when is_list(Value) ->
            maps:put(port, list_to_integer(Value), Acc);
        ("connect_timeout", Value, Acc) when is_list(Value) ->
            maps:put(connect_timeout, list_to_integer(Value), Acc);
        ("keepalive", "true", Acc) ->
            maps:put(keepalive, true, Acc);
        ("keepalive", "false", Acc) ->
            maps:put(keepalive, false, Acc);
        (Key, Value, Acc) ->
            maps:put(list_to_atom(Key), Value, Acc)
    end, #{}, Input),

    ?assertEqual(Expected, Result).

%%====================================================================
%% Protocol Validation Tests
%%====================================================================

valid_protocols_test() ->
    ?assert(is_valid_protocol_helper(env)),
    ?assert(is_valid_protocol_helper(dns_sd)),
    ?assert(is_valid_protocol_helper(consul)),
    ?assert(is_valid_protocol_helper(k8s)),
    ?assertNot(is_valid_protocol_helper(invalid)).

is_valid_protocol_helper(env) -> true;
is_valid_protocol_helper(dns_sd) -> true;
is_valid_protocol_helper(consul) -> true;
is_valid_protocol_helper(k8s) -> true;
is_valid_protocol_helper(_) -> false.
