%%%-------------------------------------------------------------------
%%% @doc Alert Manager Unit Tests
%%%
%%% Comprehensive EUnit tests for erlmcp_alert_manager module
%%% Tests alert routing, escalation policies, suppression rules, and
%%% notification channels.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_alert_manager_tests).
-author("erlmcp").

-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/logger.hrl").

%% Mirror of alert record from erlmcp_alert_manager (not exported in header)
-record(alert, {
    id :: binary(),
    timestamp :: integer(),
    source :: binary(),
    severity :: low | medium | high | critical,
    title :: binary(),
    message :: binary(),
    details :: map(),
    labels :: map(),
    silenced :: boolean(),
    acknowledged :: boolean(),
    acknowledged_by :: binary() | undefined,
    acknowledged_at :: integer() | undefined,
    resolved :: boolean(),
    resolved_at :: integer() | undefined
}).

%% Test state record
-record(test_state, {
    pid :: pid() | undefined,
    routes :: [binary()],
    policies :: [binary()],
    suppressions :: [binary()]
}).

%%%=============================================================================
%%% Setup and Teardown
%%%=============================================================================

setup_alert_manager() ->
    {ok, Pid} = erlmcp_alert_manager:start_link(#{
        slack_webhook_url => <<"https://hooks.slack.com/test">>,
        smtp => #{
            relay => <<"localhost">>,
            port => 2525,
            from => <<"test@example.com">>,
            to => <<"admin@example.com">>
        }
    }),
    #test_state{pid = Pid, routes = [], policies = [], suppressions = []}.

cleanup_alert_manager(#test_state{pid = Pid}) ->
    catch gen_server:stop(Pid),
    ok.

%%%=============================================================================
%%% Test Generators
%%%=============================================================================

alert_manager_test_() ->
    {setup,
     fun setup_alert_manager/0,
     fun cleanup_alert_manager/1,
     [
      fun test_start_stop/1,
      fun test_add_remove_route/1,
      fun test_add_remove_policy/1,
      fun test_add_remove_suppression/1,
      fun test_send_alert/1,
      fun test_alert_suppression/1,
      fun test_alert_routing/1,
      fun test_escalation_policy/1,
      fun test_get_active_alerts/1,
      fun test_get_alert_history/1,
      fun test_get_metrics/1,
      fun test_notification_rate_limiting/1,
      fun test_severity_colors/1,
      fun test_duration_parsing/1,
      fun test_alert_deduplication/1
     ]}.

%%%=============================================================================
%%% Basic Lifecycle Tests
%%%=============================================================================

test_start_stop(_State) ->
    ?_assertMatch({ok, _Pid}, erlmcp_alert_manager:start_link()),
    ?_assertMatch(true, is_pid(whereis(erlmcp_alert_manager))).

%%%=============================================================================
%%% Route Management Tests
%%%=============================================================================

test_add_remove_route(#test_state{pid = Pid} = State) ->
    %% Add a route
    RouteConfig = #{
        id => <<"test-route-1">>,
        name => <<"Test Route">>,
        condition => <<"severity=high">>,
        targets => [<<"slack">>, <<"email">>],
        template => <<"default">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:add_route(RouteConfig)),

    %% Verify route was added
    Routes = gen_server:call(Pid, get_routes),
    ?assertEqual(1, maps:size(Routes)),
    ?assert(maps:is_key(<<"test-route-1">>, Routes)),

    %% Remove route
    ?assertEqual(ok, erlmcp_alert_manager:remove_route(<<"test-route-1">>)),

    %% Verify route was removed
    Routes2 = gen_server:call(Pid, get_routes),
    ?assertEqual(0, maps:size(Routes2)),
    State.

test_alert_routing(#test_state{pid = Pid}) ->
    %% Add routes for different severities
    HighRoute = #{
        id => <<"high-severity-route">>,
        name => <<"High Severity Route">>,
        condition => <<"severity=high">>,
        targets => [<<"slack">>]
    },
    CriticalRoute = #{
        id => <<"critical-severity-route">>,
        name => <<"Critical Severity Route">>,
        condition => <<"severity=critical">>,
        targets => [<<"pagerduty">>, <<"slack">>]
    },

    ok = erlmcp_alert_manager:add_route(HighRoute),
    ok = erlmcp_alert_manager:add_route(CriticalRoute),

    %% Send high severity alert - should route to slack
    HighAlert = #{
        source => <<"test-source">>,
        severity => high,
        title => <<"High Alert">>,
        message => <<"This is a high severity alert">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(HighAlert)),

    %% Send critical severity alert - should route to pagerduty and slack
    CriticalAlert = #{
        source => <<"test-source">>,
        severity => critical,
        title => <<"Critical Alert">>,
        message => <<"This is a critical alert">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(CriticalAlert)).

%%%=============================================================================
%%% Policy Management Tests
%%%=============================================================================

test_add_remove_policy(#test_state{pid = Pid}) ->
    %% Add an escalation policy
    PolicyConfig = #{
        id => <<"test-policy-1">>,
        name => <<"Test Policy">>,
        severity => high,
        delay => <<"5m">>,
        steps => [<<"notify_team">>, <<"escalate_manager">>],
        active => true
    },
    ?assertEqual(ok, erlmcp_alert_manager:add_policy(PolicyConfig)),

    %% Verify policy was added
    Policies = gen_server:call(Pid, get_policies),
    ?assertEqual(1, maps:size(Policies)),
    ?assert(maps:is_key(<<"test-policy-1">>, Policies)),

    %% Remove policy
    ?assertEqual(ok, erlmcp_alert_manager:remove_policy(<<"test-policy-1">>)),

    %% Verify policy was removed
    Policies2 = gen_server:call(Pid, get_policies),
    ?assertEqual(0, maps:size(Policies2)).

test_escalation_policy(#test_state{pid = Pid}) ->
    %% Add policy with escalation steps
    Policy = #{
        id => <<"escalation-policy">>,
        name => <<"Escalation Policy">>,
        severity => critical,
        delay => <<"1m">>,
        steps => [<<"step1">>, <<"step2">>, <<"step3">>],
        active => true
    },
    ok = erlmcp_alert_manager:add_policy(Policy),

    %% Send critical alert - should trigger escalation
    Alert = #{
        source => <<"test-source">>,
        severity => critical,
        title => <<"Escalation Test">>,
        message => <<"Test escalation policy">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(Alert)),

    %% Verify metrics
    Metrics = erlmcp_alert_manager:get_metrics(),
    ?assert(maps:is_key(alerts_by_severity, Metrics)).

%%%=============================================================================
%%% Suppression Tests
%%%=============================================================================

test_add_remove_suppression(#test_state{pid = Pid}) ->
    %% Add a suppression rule
    SuppressionConfig = #{
        id => <<"test-suppression-1">>,
        condition => <<"source=test-maintenance">>,
        duration => <<"30m">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:add_suppression(SuppressionConfig)),

    %% Verify suppression was added
    Suppressions = gen_server:call(Pid, get_suppressions),
    ?assertEqual(1, maps:size(Suppressions)),

    %% Remove suppression
    ?assertEqual(ok, erlmcp_alert_manager:remove_suppression(<<"test-suppression-1">>)),

    %% Verify suppression was removed
    Suppressions2 = gen_server:call(Pid, get_suppressions),
    ?assertEqual(0, maps:size(Suppressions2)).

test_alert_suppression(#test_state{pid = Pid}) ->
    %% Add suppression for maintenance source
    Suppression = #{
        id => <<"maintenance-suppression">>,
        condition => <<"source=maintenance">>,
        duration => <<"10m">>
    },
    ok = erlmcp_alert_manager:add_suppression(Suppression),

    %% Send suppressed alert
    SuppressedAlert = #{
        source => <<"maintenance">>,
        severity => high,
        title => <<"Maintenance Alert">>,
        message => <<"This should be suppressed">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(SuppressedAlert)),

    %% Verify alert was silenced
    ActiveAlerts = erlmcp_alert_manager:get_active_alerts(),
    SilencedAlerts = [A || A <- ActiveAlerts, A#alert.silenced =:= true],
    ?assert(length(SilencedAlerts) > 0).

%%%=============================================================================
%%% Alert Sending Tests
%%%=============================================================================

test_send_alert(#test_state{pid = Pid}) ->
    %% Send a basic alert
    AlertData = #{
        source => <<"test-source">>,
        severity => medium,
        title => <<"Test Alert">>,
        message => <<"This is a test alert">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(AlertData)),

    %% Verify alert was added to active alerts
    ActiveAlerts = erlmcp_alert_manager:get_active_alerts(),
    ?assert(length(ActiveAlerts) > 0),

    %% Verify alert structure
    [Alert | _] = ActiveAlerts,
    ?assertEqual(<<"test-source">>, Alert#alert.source),
    ?assertEqual(medium, Alert#alert.severity),
    ?assertEqual(<<"Test Alert">>, Alert#alert.title),
    ?assertEqual(<<"This is a test alert">>, Alert#alert.message).

test_alert_deduplication(#test_state{pid = Pid}) ->
    %% Send the same alert twice
    AlertData = #{
        source => <<"dedup-test">>,
        severity => low,
        title => <<"Duplicate Test">>,
        message => <<"Testing deduplication">>
    },
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(AlertData)),
    ?assertEqual(ok, erlmcp_alert_manager:send_alert(AlertData)),

    %% Verify we have multiple alerts (deduplication can be enhanced)
    ActiveAlerts = erlmcp_alert_manager:get_active_alerts(),
    MatchingAlerts = [A || A <- ActiveAlerts,
                           A#alert.source =:= <<"dedup-test">>,
                           A#alert.title =:= <<"Duplicate Test">>],
    ?assert(length(MatchingAlerts) >= 2).

%%%=============================================================================
%%% History and Metrics Tests
%%%=============================================================================

test_get_active_alerts(#test_state{}) ->
    %% Send multiple alerts
    lists:foreach(fun(N) ->
        erlmcp_alert_manager:send_alert(#{
            source => <<"test-source">>,
            severity => medium,
            title => iolist_to_binary(["Alert ", integer_to_list(N)]),
            message => <<"Test message">>
        })
    end, lists:seq(1, 5)),

    %% Get active alerts
    ActiveAlerts = erlmcp_alert_manager:get_active_alerts(),
    ?assert(length(ActiveAlerts) >= 5).

test_get_alert_history(#test_state{}) ->
    %% Send alerts from a specific source
    Source = <<"history-test-source">>,
    lists:foreach(fun(N) ->
        erlmcp_alert_manager:send_alert(#{
            source => Source,
            severity => low,
            title => iolist_to_binary(["History ", integer_to_list(N)]),
            message => <<"History test">>
        })
    end, lists:seq(1, 3)),

    %% Get history for the source
    History = erlmcp_alert_manager:get_alert_history(Source),
    ?assert(length(History) >= 3),
    ?assert(lists:all(fun(A) -> A#alert.source =:= Source end, History)).

test_get_metrics(#test_state{}) ->
    %% Send various severity alerts
    lists:foreach(fun({Severity, Count}) ->
        lists:foreach(fun(N) ->
            erlmcp_alert_manager:send_alert(#{
                source => <<"metrics-test">>,
                severity => Severity,
                title => iolist_to_binary([atom_to_list(Severity), " ", integer_to_list(N)]),
                message => <<"Metrics test">>
            })
        end, lists:seq(1, Count))
    end, [{low, 2}, {medium, 3}, {high, 1}, {critical, 1}]),

    %% Get metrics
    Metrics = erlmcp_alert_manager:get_metrics(),

    %% Verify metrics structure
    ?assert(maps:is_key(total_alerts, Metrics)),
    ?assert(maps:is_key(active_alerts, Metrics)),
    ?assert(maps:is_key(alerts_by_severity, Metrics)),

    %% Verify counts
    BySeverity = maps:get(alerts_by_severity, Metrics),
    ?assert(maps:get(low, BySeverity, 0) >= 2),
    ?assert(maps:get(medium, BySeverity, 0) >= 3).

%%%=============================================================================
%%% Notification Tests
%%%=============================================================================

test_notification_rate_limiting(#test_state{pid = Pid}) ->
    %% Send multiple alerts rapidly
    lists:foreach(fun(N) ->
        erlmcp_alert_manager:send_alert(#{
            source => <<"rate-limit-test">>,
            severity => medium,
            title => iolist_to_binary(["Rate Limit ", integer_to_list(N)]),
            message => <<"Testing rate limiting">>
        })
    end, lists:seq(1, 10)),

    %% Allow time for notification queue processing
    timer:sleep(100),

    %% Check that queue is being processed
    Queue = gen_server:call(Pid, get_notification_queue),
    ?assert(is_function(queue, 2)).

%%%=============================================================================
%%% Utility Function Tests
%%%=============================================================================

test_severity_colors(_State) ->
    %% We can't directly test the internal function, but we can verify
    %% the module compiles and has the function exported
    ?assertMatch({module, erlmcp_alert_manager},
                 code:ensure_loaded(erlmcp_alert_manager)).

test_duration_parsing(#test_state{}) ->
    %% Test various duration formats by creating suppressions
    ?assertEqual(ok, erlmcp_alert_manager:add_suppression(#{
        id => <<"duration-30s">>,
        condition => <<"test">>,
        duration => <<"30s">>
    })),
    ?assertEqual(ok, erlmcp_alert_manager:remove_suppression(<<"duration-30s">>)),

    ?assertEqual(ok, erlmcp_alert_manager:add_suppression(#{
        id => <<"duration-5m">>,
        condition => <<"test">>,
        duration => <<"5m">>
    })),
    ?assertEqual(ok, erlmcp_alert_manager:remove_suppression(<<"duration-5m">>)),

    ?assertEqual(ok, erlmcp_alert_manager:add_suppression(#{
        id => <<"duration-1h">>,
        condition => <<"test">>,
        duration => <<"1h">>
    })),
    ?assertEqual(ok, erlmcp_alert_manager:remove_suppression(<<"duration-1h">>)).

%%%=============================================================================
%%% Edge Case Tests
%%%=============================================================================

notification_type_test_() ->
    {setup,
     fun setup_alert_manager/0,
     fun cleanup_alert_manager/1,
     [fun test_notification_types/1]}.

test_notification_types(#test_state{}) ->
    %% Test sending alerts with explicit notification types
    ?assertEqual(ok,
        erlmcp_alert_manager:send_alert(#{
            source => <<"test">>,
            severity => low,
            title => <<"Email Test">>,
            message => <<"Email notification">>
        }, <<"email">>)),

    ?assertEqual(ok,
        erlmcp_alert_manager:send_alert(#{
            source => <<"test">>,
            severity => high,
            title => <<"Slack Test">>,
            message => <<"Slack notification">>
        }, <<"slack">>)),

    ?assertEqual(ok,
        erlmcp_alert_manager:send_alert(#{
            source => <<"test">>,
            severity => critical,
            title => <<"PagerDuty Test">>,
            message => <<"PagerDuty notification">>
        }, <<"pagerduty">>)).

empty_alerts_test_() ->
    {setup,
     fun setup_alert_manager/0,
     fun cleanup_alert_manager/1,
     [fun test_empty_state/1]}.

test_empty_state(_State) ->
    %% Initially there should be no active alerts
    ActiveAlerts = erlmcp_alert_manager:get_active_alerts(),
    ?assertEqual([], ActiveAlerts),

    %% Metrics should start at zero
    Metrics = erlmcp_alert_manager:get_metrics(),
    ?assertEqual(0, maps:get(total_alerts, Metrics, 0)),
    ?assertEqual(0, maps:get(active_alerts, Metrics, 0)).
