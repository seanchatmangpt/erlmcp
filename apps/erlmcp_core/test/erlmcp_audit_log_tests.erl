%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_log tests
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(ASSERT_EVENT_ID(Event),
    begin
        ?assert(is_binary(maps:get(id, Event))),
        ?assert(byte_size(maps:get(id, Event)) >= 16)
    end).

-define(ASSERT_TIMESTAMP(Event),
    ?assert(maps:get(timestamp, Event) > 0)).

-define(ASSERT_SIGNATURE(Event),
    ?assert(is_binary(maps:get(signature, Event)))).

%%====================================================================
%% Setup and Teardown
%%====================================================================

setup() ->
    {ok, Pid} = erlmcp_audit_log:start_link(#{
        retention_period => 3600,
        max_events => 1000,
        backup_enabled => false
    }),
    Pid.

cleanup(Pid) ->
    erlmcp_audit_log:stop(),
    % Wait for process to terminate
    timer:sleep(100).

%%====================================================================
%% Basic Logging Tests
%%====================================================================

log_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_event(
                         security,
                         <<"test_event">>,
                         #{test => true},
                         info,
                         #{}
                     ),
                     ?assert(is_binary(EventId)),
                     ?assert(byte_size(EventId) > 0)
                 end),

          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_event(
                         protocol_violation,
                         <<"invalid_message">>,
                         #{error => bad_format},
                         critical,
                         #{}
                     ),
                     Events = erlmcp_audit_log:get_events(#{}),
                     ?assert(length(Events) > 0),
                     Event = hd(Events),
                     ?assertEqual(protocol_violation, maps:get(category, Event)),
                     ?assertEqual(critical, maps:get(severity, Event))
                 end)]
     end}.

%%====================================================================
%% Violation Logging Tests
%%====================================================================

log_violation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_violation(
                         invalid_message_format,
                         #{details => <<"Missing required field">>},
                         self()
                     ),
                     ?assert(is_binary(EventId)),

                     % Verify violation was logged
                     Violations = erlmcp_audit_log:get_violations(#{}),
                     ?assert(length(Violations) > 0),

                     Violation = hd(Violations),
                     ?assertEqual(protocol_violation, maps:get(category, Violation)),
                     ?ASSERT_EVENT_ID(Violation),
                     ?ASSERT_TIMESTAMP(Violation)
                 end),

          ?_test(begin
                     % Test different violation types
                     ViolationTypes = [
                         unknown_method,
                         missing_required_field,
                         type_constraint_violation,
                         size_limit_exceeded,
                         rate_limit_exceeded
                     ],
                     lists:foreach(fun(Type) ->
                         erlmcp_audit_log:log_violation(
                             Type,
                             #{violation => Type},
                             self()
                         )
                     end, ViolationTypes),

                     % Count violations
                     AllViolations = erlmcp_audit_log:get_violations(#{}),
                     ?assert(length(AllViolations) >= length(ViolationTypes))
                 end)]
     end}.

%%====================================================================
%% Security Event Tests
%%====================================================================

log_security_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_security_event(
                         intrusion_attempt,
                         block_ip,
                         blocked
                     ),
                     ?assert(is_binary(EventId)),

                     SecurityEvents = erlmcp_audit_log:get_security_events(#{}),
                     ?assert(length(SecurityEvents) > 0),

                     Event = hd(SecurityEvents),
                     ?assertEqual(security, maps:get(category, Event))
                 end),

          ?_test(begin
                     % Test different security event types
                     SecurityTypes = [
                         brute_force_attack,
                         privilege_escalation,
                         data_exfiltration,
                         unauthorized_access,
                         ddos_attack
                     ],
                     lists:foreach(fun(Type) ->
                         erlmcp_audit_log:log_security_event(
                             Type,
                             detect,
                             detected
                         )
                     end, SecurityTypes),

                     Events = erlmcp_audit_log:get_security_events(#{}),
                     ?assert(length(Events) >= length(SecurityTypes))
                 end)]
     end}.

%%====================================================================
%% Auth Event Tests
%%====================================================================

log_auth_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_auth_event(
                         login_success,
                         <<"user123">>,
                         authenticate,
                         success
                     ),
                     ?assert(is_binary(EventId)),

                     % Get auth events
                     Events = erlmcp_audit_log:get_events(#{category => authorization}),
                     ?assert(length(Events) > 0),

                     Event = hd(Events),
                     ?assertEqual(authorization, maps:get(category, Event))
                 end),

          ?_test(begin
                     % Test login failure
                     {ok, _} = erlmcp_audit_log:log_auth_event(
                         login_failure,
                         <<"user456">>,
                         authenticate,
                         failed
                     ),

                     Events = erlmcp_audit_log:get_events(#{
                         category => authorization,
                         event_type => <<"login_failure">>
                     }),
                     ?assert(length(Events) > 0)
                 end)]
     end}.

%%====================================================================
%% Cluster Event Tests
%%====================================================================

log_cluster_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     {ok, EventId} = erlmcp_audit_log:log_cluster_event(
                         node_joined,
                         node@host1,
                         join_cluster,
                         success
                     ),
                     ?assert(is_binary(EventId)),

                     Events = erlmcp_audit_log:get_events(#{category => cluster}),
                     ?assert(length(Events) > 0)
                 end),

          ?_test(begin
                     % Test partition detection (critical)
                     {ok, _} = erlmcp_audit_log:log_cluster_event(
                         partition_detected,
                         [node1, node2],
                         detect_partition,
                         detected
                     ),

                     Events = erlmcp_audit_log:get_events(#{category => cluster}),
                     ?assert(length(Events) >= 1)
                 end)]
     end}.

%%====================================================================
%% Filter Tests
%%====================================================================

filter_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Log events with different severities
                     erlmcp_audit_log:log_event(test, <<"e1">>, #{}, debug, #{}),
                     erlmcp_audit_log:log_event(test, <<"e2">>, #{}, info, #{}),
                     erlmcp_audit_log:log_event(test, <<"e3">>, #{}, warning, #{}),
                     erlmcp_audit_log:log_event(test, <<"e4">>, #{}, error, #{}),
                     erlmcp_audit_log:log_event(test, <<"e5">>, #{}, critical, #{}),

                     AllEvents = erlmcp_audit_log:get_events(#{}),
                     ?assert(length(AllEvents) >= 5),

                     % Filter by severity
                     CriticalEvents = erlmcp_audit_log:filter_by_severity(AllEvents, critical),
                     ?assert(length(CriticalEvents) > 0),

                     WarningEvents = erlmcp_audit_log:filter_by_severity(
                         AllEvents, [warning, error, critical]),
                     ?assert(length(WarningEvents) >= 3)
                 end),

          ?_test(begin
                     % Log events with different categories
                     erlmcp_audit_log:log_event(security, <<"s1">>, #{}, info, #{}),
                     erlmcp_audit_log:log_event(authorization, <<"a1">>, #{}, info, #{}),
                     erlmcp_audit_log:log_event(cluster, <<"c1">>, #{}, info, #{}),

                     SecurityEvents = erlmcp_audit_log:filter_by_category(
                         erlmcp_audit_log:get_events(#{}), security),
                     ?assert(length(SecurityEvents) > 0),

                     MultiCatEvents = erlmcp_audit_log:filter_by_category(
                         erlmcp_audit_log:get_events(#{}), [security, authorization]),
                     ?assert(length(MultiCatEvents) >= 2)
                 end),

          ?_test(begin
                     % Test time range filtering
                     Now = erlang:system_time(millisecond),
                     Past = Now - 10000,
                     Future = Now + 10000,

                     erlmcp_audit_log:log_event(test, <<"t1">>, #{}, info, #{}),

                     AllEvents = erlmcp_audit_log:get_events(#{}),
                     TimeFiltered = erlmcp_audit_log:filter_by_time_range(
                         AllEvents, Past, Future),
                     ?assert(length(TimeFiltered) > 0)
                 end),

          ?_test(begin
                     % Test user filtering
                     UserId1 = <<"user1">>,
                     UserId2 = <<"user2">>,

                     erlmcp_audit_log:log_event(auth, <<"e1">>,
                         #{user_id => UserId1}, info, #{}),
                     erlmcp_audit_log:log_event(auth, <<"e2">>,
                         #{user_id => UserId2}, info, #{}),

                     AllEvents = erlmcp_audit_log:get_events(#{}),
                     User1Events = erlmcp_audit_log:filter_by_user(AllEvents, UserId1),
                     ?assert(length(User1Events) > 0)
                 end)]
     end}.

%%====================================================================
%% Query Tests
%%====================================================================

query_tests_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Test complex filter
                     UserId = <<"test_user">>,
                     erlmcp_audit_log:log_auth_event(
                         login_success, UserId, auth, success),

                     Filter = #{
                         category => authorization,
                         user_id => UserId,
                         limit => 10
                     },

                     Events = erlmcp_audit_log:get_events(Filter),
                     ?assert(length(Events) > 0),

                     Event = hd(Events),
                     ?assertEqual(authorization, maps:get(category, Event))
                 end),

          ?_test(begin
                     % Test time range query
                     Now = erlang:system_time(millisecond),
                     HourAgo = Now - 3600000,

                     Events = erlmcp_audit_log:get_events(authorization, {HourAgo, Now}),
                     ?assert(is_list(Events))
                 end),

          ?_test(begin
                     % Test limit and offset
                     erlmcp_audit_log:log_event(test, <<"e1">>, #{}, info, #{}),
                     erlmcp_audit_log:log_event(test, <<"e2">>, #{}, info, #{}),
                     erlmcp_audit_log:log_event(test, <<"e3">>, #{}, info, #{}),

                     Page1 = erlmcp_audit_log:get_events(#{
                         limit => 2,
                         offset => 0
                     }),
                     ?assert(length(Page1) =< 2)
                 end)]
     end}.

%%====================================================================
%% Stats Tests
%%====================================================================

stats_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     Stats = erlmcp_audit_log:get_stats(),
                     ?assert(is_map(Stats)),
                     ?assert(is_integer(maps:get(total_events, Stats))),
                     ?assert(is_integer(maps:get(violations, Stats))),
                     ?assert(is_integer(maps:get(security_events, Stats)))
                 end),

          ?_test(begin
                     Status = erlmcp_audit_log:status(),
                     ?assert(is_map(Status)),
                     ?assertEqual(node(), maps:get(node, Status)),

                     ProcessInfo = maps:get(process_info, Status),
                     ?assert(is_map(ProcessInfo))
                 end)]
     end}.

%%====================================================================
%% Export Tests
%%====================================================================

export_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Log some events
                     lists:foreach(fun(I) ->
                         erlmcp_audit_log:log_event(
                             test,
                             list_to_binary(io_lib:format("event_~p", [I])),
                             #{index => I},
                             info,
                             #{}
                         )
                     end, lists:seq(1, 5)),

                     % Export to JSON
                     {ok, Path} = erlmcp_audit_log:export_events(
                         #{category => test},
                         <<"test_export.json">>
                     ),
                     ?assert(is_binary(Path))
                 end),

          ?_test(begin
                     % Generate compliance report
                     {ok, SOC2Report} = erlmcp_audit_log:generate_compliance_report(<<"SOC2">>),
                     ?assert(is_map(SOC2Report)),
                     ?assertEqual(<<"SOC2">>, maps:get(report_type, SOC2Report)),

                     {ok, PCIReport} = erlmcp_audit_log:generate_compliance_report(<<"PCI-DSS">>),
                     ?assert(is_map(PCIReport)),

                     {ok, HIPAAReport} = erlmcp_audit_log:generate_compliance_report(<<"HIPAA">>),
                     ?assert(is_map(HIPAAReport))
                 end)]
     end}.

%%====================================================================
%% Cleanup Tests
%%====================================================================

cleanup_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Log some events
                     lists:foreach(fun(_) ->
                         erlmcp_audit_log:log_event(
                             old_event,
                             <<"old">>,
                             #{},
                             info,
                             #{}
                         )
                     end, lists:seq(1, 10)),

                     % Clear very old events (future time - should clear none)
                     FutureTime = erlang:system_time(second) + 3600,
                     {ok, Count} = erlmcp_audit_log:clear_old_events(FutureTime),
                     ?assert(is_integer(Count))
                 end)]
     end}.

%%====================================================================
%% Event Structure Tests
%%====================================================================

event_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Log a violation and check structure
                     {ok, _} = erlmcp_audit_log:log_violation(
                         invalid_message_format,
                         #{test => data},
                         self()
                     ),

                     Events = erlmcp_audit_log:get_violations(#{}),
                     ?assert(length(Events) > 0),

                     Event = hd(Events),

                     % Check required fields
                     ?ASSERT_EVENT_ID(Event),
                     ?ASSERT_TIMESTAMP(Event),
                     ?ASSERT_SIGNATURE(Event),

                     ?assertEqual(protocol_violation, maps:get(category, Event)),
                     ?assert(is_binary(maps:get(event_type, Event))),
                     ?assert(is_atom(maps:get(severity, Event))),
                     ?assertEqual(node(), maps:get(node, Event)),

                     Context = maps:get(context, Event),
                     ?assert(is_map(Context)),

                     Metadata = maps:get(metadata, Event),
                     ?assert(is_map(Metadata))
                 end)]
     end}.

%%====================================================================
%% Correlation Tests
%%====================================================================

correlation_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     CorrelationId = <<"corr-12345">>,

                     % Log multiple events with same correlation ID
                     {ok, _} = erlmcp_audit_log:log_event(
                         workflow,
                         <<"step1">>,
                         #{step => 1},
                         info,
                         #{},
                         #{correlation_id => CorrelationId}
                     ),

                     {ok, _} = erlmcp_audit_log:log_event(
                         workflow,
                         <<"step2">>,
                         #{step => 2},
                         info,
                         #{},
                         #{correlation_id => CorrelationId}
                     ),

                     % Query by correlation ID
                     Events = erlmcp_audit_log:get_events(#{
                         correlation_id => CorrelationId
                     }),

                     ?assert(length(Events) >= 2)
                 end)]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Bulk logging test
                     StartTime = erlang:monotonic_time(millisecond),

                     lists:foreach(fun(I) ->
                         erlmcp_audit_log:log_event(
                             performance,
                             list_to_binary(io_lib:format("event_~p", [I])),
                             #{index => I},
                             info,
                             #{}
                         )
                     end, lists:seq(1, 100)),

                     EndTime = erlang:monotonic_time(millisecond),
                     Duration = EndTime - StartTime,

                     % Should complete reasonably fast
                     ?assert(Duration < 5000),  % 5 seconds for 100 events

                     % Verify events were logged
                     Events = erlmcp_audit_log:get_events(#{category => performance}),
                     ?assert(length(Events) >= 100)
                 end)]
     end}.

%%====================================================================
%% Integration Tests
%%====================================================================

integration_with_authorization_test_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
          ?_test(begin
                     % Simulate authorization decision logging
                     UserId = <<"user_123">>,
                     Resource = <<"resource_abc">>,

                     % Log permission check
                     {ok, _} = erlmcp_audit_log:log_auth_event(
                         permission_denied,
                         UserId,
                         {access_resource, Resource},
                         denied
                     ),

                     % Query for user's denied permissions
                     Events = erlmcp_audit_log:get_events(#{
                         category => authorization,
                         user_id => UserId,
                         event_type => <<"permission_denied">>
                     }),

                     ?assert(length(Events) > 0)
                 end)]
     end}.
