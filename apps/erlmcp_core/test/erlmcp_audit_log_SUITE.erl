%%%-------------------------------------------------------------------
%%% @doc erlmcp_audit_log Common Test Suite
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_audit_log_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp_audit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
     group(basic_logging),
     group(violation_logging),
     group(security_events),
     group(auth_events),
     group(cluster_events),
     group(filtering),
     group(compliance_reports),
     group(performance),
     group(integration)
    ].

groups() ->
    [
     {basic_logging, [shuffle], [
         log_event_test,
         event_structure_test,
         event_signature_test
     ]},
     {violation_logging, [shuffle], [
         log_violation_test,
         multiple_violations_test,
         violation_context_test
     ]},
     {security_events, [shuffle], [
         log_security_event_test,
         security_severity_test,
         intrusion_detection_test
     ]},
     {auth_events, [shuffle], [
         log_auth_event_test,
         login_failure_test,
         permission_denied_test
     ]},
     {cluster_events, [shuffle], [
         log_cluster_event_test,
         partition_detection_test,
         node_join_leave_test
     ]},
     {filtering, [shuffle], [
         filter_by_severity_test,
         filter_by_category_test,
         filter_by_time_test,
         filter_by_user_test,
         complex_filter_test
     ]},
     {compliance_reports, [shuffle], [
         soc2_report_test,
         pci_dss_report_test,
         hipaa_report_test
     ]},
     {performance, [shuffle], [
         bulk_logging_test,
         concurrent_logging_test,
         query_performance_test
     ]},
     {integration, [shuffle], [
         authorization_integration_test,
         cluster_integration_test,
         correlation_id_test
     ]}
    ].

init_per_suite(Config) ->
    {ok, Pid} = erlmcp_audit_log:start_link(#{
        retention_period => 7200,
        max_events => 10000,
        backup_enabled => false
    }),
    [{audit_pid, Pid} | Config].

end_per_suite(Config) ->
    erlmcp_audit_log:stop(),
    ok.

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    % Clear tables before each test
    {ok, Pid} = erlmcp_audit_log:start_link(#{
        retention_period => 7200,
        max_events => 10000,
        backup_enabled => false
    }),
    [{audit_pid, Pid} | Config].

end_per_testcase(_TestCase, _Config) ->
    erlmcp_audit_log:stop(),
    ok.

%%====================================================================
%% Basic Logging Test Cases
%%====================================================================

log_event_test(_Config) ->
    {ok, EventId} = erlmcp_audit_log:log_event(
        test_category,
        <<"test_event">>,
        #{test_key => test_value},
        info,
        #{meta_key => meta_value}
    ),
    ?assert(is_binary(EventId)),
    ?assert(byte_size(EventId) > 0),

    Events = erlmcp_audit_log:get_events(#{}),
    ?assert(length(Events) > 0),

    Event = hd(Events),
    ?assertEqual(test_category, maps:get(category, Event)),
    ?assertEqual(<<"test_event">>, maps:get(event_type, Event)),
    ?assertEqual(info, maps:get(severity, Event)).

event_structure_test(_Config) ->
    {ok, _EventId} = erlmcp_audit_log:log_event(
        test,
        <<"struct_test">>,
        #{key => value},
        warning,
        #{}
    ),

    Events = erlmcp_audit_log:get_events(#{}),
    Event = hd(Events),

    % Verify all required fields
    ?assert(is_binary(maps:get(id, Event))),
    ?assert(is_integer(maps:get(timestamp, Event))),
    ?assert(is_atom(maps:get(category, Event))),
    ?assert(is_binary(maps:get(event_type, Event))),
    ?assert(is_atom(maps:get(severity, Event))),
    ?assert(is_map(maps:get(context, Event))),
    ?assert(is_map(maps:get(metadata, Event))),
    ?assert(is_atom(maps:get(node, Event))),
    ?assert(is_binary(maps:get(signature, Event))).

event_signature_test(_Config) ->
    {ok, _EventId} = erlmcp_audit_log:log_event(
        test,
        <<"sig_test">>,
        #{data => <<"test_data">>},
        info,
        #{}
    ),

    Events = erlmcp_audit_log:get_events(#{}),
    Event = hd(Events),

    Signature = maps:get(signature, Event),
    ?assert(is_binary(Signature)),
    ?assert(byte_size(Signature) > 0).

%%====================================================================
%% Violation Logging Test Cases
%%====================================================================

log_violation_test(_Config) ->
    Context = #{details => <<"Invalid message format">>},
    {ok, EventId} = erlmcp_audit_log:log_violation(
        invalid_message_format,
        Context,
        self()
    ),
    ?assert(is_binary(EventId)),

    Violations = erlmcp_audit_log:get_violations(#{}),
    ?assert(length(Violations) > 0),

    Violation = hd(Violations),
    ?assertEqual(protocol_violation, maps:get(category, Violation)),
    ?assertEqual(critical, maps:get(severity, Violation)).

multiple_violations_test(_Config) ->
    ViolationTypes = [
        unknown_method,
        missing_required_field,
        type_constraint_violation,
        size_limit_exceeded,
        rate_limit_exceeded,
        authentication_failure,
        authorization_failure
    ],

    lists:foreach(fun(Type) ->
        erlmcp_audit_log:log_violation(Type, #{type => Type}, self())
    end, ViolationTypes),

    AllViolations = erlmcp_audit_log:get_violations(#{}),
    ?assert(length(AllViolations) >= length(ViolationTypes)).

violation_context_test(_Config) ->
    Pid = self(),
    Context = #{
        violation_type => test_violation,
        process_info => process_info(Pid)
    },
    Metadata = #{
        source => test_suite,
        test_case => violation_context_test
    },

    {ok, _EventId} = erlmcp_audit_log:log_violation(
        state_violation,
        Context,
        Pid,
        Metadata
    ),

    Events = erlmcp_audit_log:get_events(#{}),
    ?assert(length(Events) > 0).

%%====================================================================
%% Security Events Test Cases
%%====================================================================

log_security_event_test(_Config) ->
    {ok, EventId} = erlmcp_audit_log:log_security_event(
        intrusion_attempt,
        block_ip,
        blocked
    ),
    ?assert(is_binary(EventId)),

    SecurityEvents = erlmcp_audit_log:get_security_events(#{}),
    ?assert(length(SecurityEvents) > 0).

security_severity_test(_Config) ->
    % Test different security event severities
    erlmcp_audit_log:log_security_event(intrusion_attempt, action1, success),
    erlmcp_audit_log:log_security_event(unauthorized_access, action2, denied),
    erlmcp_audit_log:log_security_event(suspicious_activity, action3, blocked),
    erlmcp_audit_log:log_security_event(malware_detected, action4, detected),

    Events = erlmcp_audit_log:get_security_events(#{}),
    ?assert(length(Events) >= 4).

intrusion_detection_test(_Config) ->
    {ok, _EventId} = erlmcp_audit_log:log_security_event(
        brute_force_attack,
        detect_brute_force,
        detected
    ),

    Events = erlmcp_audit_log:get_security_events(#{}),
    ?assert(lists:any(fun(E) ->
        maps:get(event_type, E) =:= <<"brute_force_attack">>
    end, Events)).

%%====================================================================
%% Auth Events Test Cases
%%====================================================================

log_auth_event_test(_Config) ->
    UserId = <<"user_123">>,
    {ok, EventId} = erlmcp_audit_log:log_auth_event(
        login_success,
        UserId,
        authenticate,
        success
    ),
    ?assert(is_binary(EventId)),

    Events = erlmcp_audit_log:get_events(#{category => authorization}),
    ?assert(length(Events) > 0).

login_failure_test(_Config) ->
    UserId = <<"bad_user">>,
    {ok, _EventId} = erlmcp_audit_log:log_auth_event(
        login_failure,
        UserId,
        authenticate,
        failed
    ),

    Events = erlmcp_audit_log:get_events(#{
        category => authorization,
        event_type => <<"login_failure">>
    }),
    ?assert(length(Events) > 0).

permission_denied_test(_Config) ->
    UserId = <<"user_456">>,
    {ok, _EventId} = erlmcp_audit_log:log_auth_event(
        permission_denied,
        UserId,
        access_resource,
        denied
    ),

    Events = erlmcp_audit_log:get_events(#{user_id => UserId}),
    ?assert(length(Events) > 0).

%%====================================================================
%% Cluster Events Test Cases
%%====================================================================

log_cluster_event_test(_Config) ->
    {ok, EventId} = erlmcp_audit_log:log_cluster_event(
        node_joined,
        node@test,
        join_cluster,
        success
    ),
    ?assert(is_binary(EventId)).

partition_detection_test(_Config) ->
    {ok, _EventId} = erlmcp_audit_log:log_cluster_event(
        partition_detected,
        [node1, node2],
        detect_partition,
        detected
    ),

    Events = erlmcp_audit_log:get_events(#{category => cluster}),
    ?assert(lists:any(fun(E) ->
        maps:get(event_type, E) =:= <<"partition_detected">>
    end, Events)).

node_join_leave_test(_Config) ->
    erlmcp_audit_log:log_cluster_event(node_joined, node1, join, success),
    erlmcp_audit_log:log_cluster_event(node_left, node2, leave, normal),
    erlmcp_audit_log:log_cluster_event(election_completed, [node1, node2], elect, success),

    Events = erlmcp_audit_log:get_events(#{category => cluster}),
    ?assert(length(Events) >= 3).

%%====================================================================
%% Filtering Test Cases
%%====================================================================

filter_by_severity_test(_Config) ->
    % Log events with different severities
    lists:foreach(fun({Cat, EvType, Sev}) ->
        erlmcp_audit_log:log_event(Cat, EvType, #{}, Sev, #{})
    end, [
        {test, <<"e1">>, debug},
        {test, <<"e2">>, info},
        {test, <<"e3">>, warning},
        {test, <<"e4">>, error},
        {test, <<"e5">>, critical}
    ]),

    AllEvents = erlmcp_audit_log:get_events(#{}),
    CriticalEvents = erlmcp_audit_log:filter_by_severity(AllEvents, critical),
    ?assert(length(CriticalEvents) > 0),

    MultiSevEvents = erlmcp_audit_log:filter_by_severity(
        AllEvents, [warning, error, critical]),
    ?assert(length(MultiSevEvents) >= 3).

filter_by_category_test(_Config) ->
    erlmcp_audit_log:log_event(security, <<"s1">>, #{}, info, #{}),
    erlmcp_audit_log:log_event(authorization, <<"a1">>, #{}, info, #{}),
    erlmcp_audit_log:log_event(cluster, <<"c1">>, #{}, info, #{}),

    AllEvents = erlmcp_audit_log:get_events(#{}),
    SecurityEvents = erlmcp_audit_log:filter_by_category(AllEvents, security),
    ?assert(length(SecurityEvents) > 0).

filter_by_time_test(_Config) ->
    Now = erlang:system_time(millisecond),
    Past = Now - 10000,

    erlmcp_audit_log:log_event(test, <<"t1">>, #{}, info, #{}),

    AllEvents = erlmcp_audit_log:get_events(#{}),
    TimeFiltered = erlmcp_audit_log:filter_by_time_range(AllEvents, Past, Now + 1000),
    ?assert(length(TimeFiltered) > 0).

filter_by_user_test(_Config) ->
    UserId1 = <<"user1">>,
    UserId2 = <<"user2">>,

    erlmcp_audit_log:log_event(auth, <<"e1">>, #{user_id => UserId1}, info, #{}),
    erlmcp_audit_log:log_event(auth, <<"e2">>, #{user_id => UserId2}, info, #{}),

    AllEvents = erlmcp_audit_log:get_events(#{}),
    User1Events = erlmcp_audit_log:filter_by_user(AllEvents, UserId1),
    ?assert(length(User1Events) > 0).

complex_filter_test(_Config) ->
    UserId = <<"test_user">>,

    % Log events
    lists:foreach(fun(I) ->
        erlmcp_audit_log:log_event(
            security,
            list_to_binary(io_lib:format("event_~p", [I])),
            #{user_id => UserId, index => I},
            info,
            #{}
        )
    end, lists:seq(1, 10)),

    % Query with complex filter
    Events = erlmcp_audit_log:get_events(#{
        category => security,
        user_id => UserId,
        limit => 5
    }),

    ?assert(length(Events) =< 5).

%%====================================================================
%% Compliance Reports Test Cases
%%====================================================================

soc2_report_test(_Config) ->
    % Log some events for SOC2 compliance
    erlmcp_audit_log:log_auth_event(login_success, <<"user1">>, auth, success),
    erlmcp_audit_log:log_auth_event(permission_denied, <<"user2">>, access, denied),

    {ok, Report} = erlmcp_audit_log:generate_compliance_report(<<"SOC2">>),

    ?assertEqual(<<"SOC2">>, maps:get(report_type, Report)),
    ?assert(is_map(maps:get(summary, Report))),
    ?assert(is_map(maps:get(access_controls, Report))),
    ?assert(is_map(maps:get(change_management, Report))),
    ?assert(is_atom(maps:get(compliance_status, Report))).

pci_dss_report_test(_Config) ->
    erlmcp_audit_log:log_security_event(intrusion_attempt, block, blocked),
    erlmcp_audit_log:log_auth_event(login_success, <<"user1">>, auth, success),

    {ok, Report} = erlmcp_audit_log:generate_compliance_report(<<"PCI-DSS">>),

    ?assertEqual(<<"PCI_DSS">>, maps:get(report_type, Report)),
    ?assert(is_map(maps:get(audit_trail, Report))),
    ?assert(is_map(maps:get(access_control, Report))),
    ?assert(is_map(maps:get(monitoring, Report))).

hipaa_report_test(_Config) ->
    erlmcp_audit_log:log_event(data_access, <<"phi_access">>,
                               #{user_id => <<"doctor1">>, patient => <<"patient1">>},
                               info, #{}),

    {ok, Report} = erlmcp_audit_log:generate_compliance_report(<<"HIPAA">>),

    ?assertEqual(<<"HIPAA">>, maps:get(report_type, Report)),
    ?assert(is_map(maps:get(audit_controls, Report))),
    ?assert(is_map(maps:get(integrity, Report))).

%%====================================================================
%% Performance Test Cases
%%====================================================================

bulk_logging_test(_Config) ->
    StartTime = erlang:monotonic_time(millisecond),

    NumEvents = 100,
    lists:foreach(fun(I) ->
        erlmcp_audit_log:log_event(
            performance,
            list_to_binary(io_lib:format("event_~p", [I])),
            #{index => I},
            info,
            #{}
        )
    end, lists:seq(1, NumEvents)),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    % Should complete in reasonable time
    ?assert(Duration < 10000),

    Events = erlmcp_audit_log:get_events(#{category => performance}),
    ?assert(length(Events) >= NumEvents).

concurrent_logging_test(_Config) ->
    % Test concurrent logging from multiple processes
    NumProcesses = 10,
    EventsPerProcess = 10,

    Pids = lists:map(fun(I) ->
        spawn_link(fun() ->
            lists:foreach(fun(J) ->
                erlmcp_audit_log:log_event(
                    concurrent,
                    list_to_binary(io_lib:format("proc_~p_ev_~p", [I, J])),
                    #{proc => I, ev => J},
                    info,
                    #{}
                )
            end, lists:seq(1, EventsPerProcess))
        end)
    end, lists:seq(1, NumProcesses)),

    % Wait for all processes to complete
    timer:sleep(1000),

    Events = erlmcp_audit_log:get_events(#{category => concurrent}),
    ?assert(length(Events) >= NumProcesses * EventsPerProcess).

query_performance_test(_Config) ->
    % Log many events
    lists:foreach(fun(I) ->
        erlmcp_audit_log:log_event(query_test, <<>>, #{}, info, #{})
    end, lists:seq(1, 200)),

    StartTime = erlang:monotonic_time(microsecond),
    Events = erlmcp_audit_log:get_events(#{}),
    EndTime = erlang:monotonic_time(microsecond),

    QueryTime = EndTime - StartTime,

    % Query should be fast (less than 100ms for 200 events)
    ?assert(QueryTime < 100000),
    ?assert(length(Events) >= 200).

%%====================================================================
%% Integration Test Cases
%%====================================================================

authorization_integration_test(_Config) ->
    % Simulate authorization decisions
    UserId = <<"test_user">>,
    Resource = <<"protected_resource">>,

    % Log permission check
    erlmcp_audit_log:log_auth_event(
        permission_granted,
        UserId,
        {access_resource, Resource},
        success
    ),

    % Query for user's permissions
    Events = erlmcp_audit_log:get_events(#{
        category => authorization,
        user_id => UserId
    }),

    ?assert(length(Events) > 0),
    ?assert(lists:any(fun(E) ->
        maps:get(event_type, E) =:= <<"permission_granted">>
    end, Events)).

cluster_integration_test(_Config) ->
    % Simulate cluster events
    Nodes = [node1, node2, node3],

    lists:foreach(fun(Node) ->
        erlmcp_audit_log:log_cluster_event(node_joined, Node, join, success)
    end, Nodes),

    Events = erlmcp_audit_log:get_events(#{category => cluster}),
    ?assert(length(Events) >= length(Nodes)).

correlation_id_test(_Config) ->
    CorrelationId = <<"test-correlation-123">>,

    % Log multiple events with same correlation ID
    lists:foreach(fun(I) ->
        erlmcp_audit_log:log_event(
            workflow,
            list_to_binary(io_lib:format("step_~p", [I])),
            #{step => I},
            info,
            #{},
            #{correlation_id => CorrelationId}
        )
    end, lists:seq(1, 5)),

    % Query by correlation ID
    Events = erlmcp_audit_log:get_events(#{
        correlation_id => CorrelationId
    }),

    ?assert(length(Events) >= 5).

    % Verify all events have same correlation ID
    lists:foreach(fun(E) ->
        ?assertEqual(CorrelationId, maps:get(correlation_id, E))
    end, Events).
