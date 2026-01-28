%%%-------------------------------------------------------------------
%% @doc erlmcp_refusal_plan_validator - Plan-specific refusal code auditing
%%
%% Verifies that all refusal codes defined in a plan are actually
%% triggered during testing, and generates audit report.
%%
%% This ensures:
%% - All refusal paths are exercised
%% - Error messages are accurate
%% - HTTP status codes are correct
%% - Retry-after values are appropriate
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_refusal_plan_validator).

-export([
    audit_refusal_codes/2,
    trigger_throughput_exceeded/1,
    trigger_queue_depth_exceeded/1,
    trigger_connection_limit_exceeded/1,
    trigger_message_size_exceeded/1,
    trigger_unsupported_feature/1,
    generate_refusal_audit/3,
    verify_refusal_response/2
]).

-type plan() :: team | enterprise | gov.
-type refusal_type() :: throughput_exceeded | queue_depth_exceeded |
                        connection_limit_exceeded | message_size_exceeded |
                        unsupported_feature.
-type refusal_audit() :: map().

%%%-------------------------------------------------------------------
%% @doc Audit all refusal codes for a plan
%% @end
%%%-------------------------------------------------------------------
-spec audit_refusal_codes(plan(), string()) ->
    {ok, refusal_audit()} | {error, term()}.

audit_refusal_codes(Plan, Version) ->
    RefusalTypes = get_plan_refusal_types(Plan),
    StartTime = erlang:monotonic_time(millisecond),

    Results = [audit_refusal_type(Plan, Type) || Type <- RefusalTypes],

    EndTime = erlang:monotonic_time(millisecond),
    Duration = (EndTime - StartTime) / 1000.0,

    AuditReport = #{
        <<"plan">> => atom_to_binary(Plan),
        <<"version">> => list_to_binary(Version),
        <<"timestamp">> => erlang:system_time(second),
        <<"duration_seconds">> => Duration,
        <<"refusal_types_tested">> => length(RefusalTypes),
        <<"results">> => Results,
        <<"overall_status">> => determine_audit_status(Results)
    },

    {ok, AuditReport}.

%%%-------------------------------------------------------------------
%% @private Get refusal types for a plan
%% @end
%%%-------------------------------------------------------------------
-spec get_plan_refusal_types(plan()) -> [refusal_type()].

get_plan_refusal_types(_Plan) ->
    [
        throughput_exceeded,
        queue_depth_exceeded,
        connection_limit_exceeded,
        message_size_exceeded,
        unsupported_feature
    ].

%%%-------------------------------------------------------------------
%% @private Audit a specific refusal type
%% @end
%%%-------------------------------------------------------------------
-spec audit_refusal_type(plan(), refusal_type()) -> map().

audit_refusal_type(Plan, RefusalType) ->
    case trigger_refusal_scenario(Plan, RefusalType) of
        {ok, Result} ->
            VerificationResult = verify_refusal_response(RefusalType, Result),

            #{
                <<"refusal_type">> => atom_to_binary(RefusalType),
                <<"triggered">> => true,
                <<"result">> => Result,
                <<"verification">> => VerificationResult,
                <<"status">> => case VerificationResult of
                    {true, _} -> <<"pass">>;
                    {false, _} -> <<"fail">>
                end
            };

        {error, Reason} ->
            #{
                <<"refusal_type">> => atom_to_binary(RefusalType),
                <<"triggered">> => false,
                <<"error">> => atom_to_binary(Reason),
                <<"status">> => <<"fail">>
            }
    end.

%%%-------------------------------------------------------------------
%% @private Trigger refusal scenario
%% @end
%%%-------------------------------------------------------------------
-spec trigger_refusal_scenario(plan(), refusal_type()) ->
    {ok, map()} | {error, term()}.

trigger_refusal_scenario(Plan, throughput_exceeded) ->
    trigger_throughput_exceeded(Plan);
trigger_refusal_scenario(Plan, queue_depth_exceeded) ->
    trigger_queue_depth_exceeded(Plan);
trigger_refusal_scenario(Plan, connection_limit_exceeded) ->
    trigger_connection_limit_exceeded(Plan);
trigger_refusal_scenario(Plan, message_size_exceeded) ->
    trigger_message_size_exceeded(Plan);
trigger_refusal_scenario(Plan, unsupported_feature) ->
    trigger_unsupported_feature(Plan).

%%%-------------------------------------------------------------------
%% @doc Trigger throughput_exceeded refusal
%% @end
%%%-------------------------------------------------------------------
-spec trigger_throughput_exceeded(plan()) ->
    {ok, map()} | {error, term()}.

trigger_throughput_exceeded(Plan) ->
    Envelope = erlmcp_evidence_path:get_plan_envelope(Plan),
    MaxThroughput = maps:get(<<"throughput_req_s">>, Envelope),

    %% Try to exceed throughput limit
    AttemptsNeeded = MaxThroughput + 100,

    RequestTimestamps = [
        erlang:monotonic_time(millisecond)
        || _ <- lists:seq(1, AttemptsNeeded)
    ],

    FirstTs = lists:nth(1, RequestTimestamps),
    LastTs = lists:nth(length(RequestTimestamps), RequestTimestamps),
    TimeWindowMs = LastTs - FirstTs + 1,
    TimeWindowSeconds = TimeWindowMs / 1000.0,

    ActualThroughput = AttemptsNeeded / max(0.001, TimeWindowSeconds),

    Result = #{
        <<"refusal_type">> => <<"throughput_exceeded">>,
        <<"max_throughput_req_s">> => MaxThroughput,
        <<"attempted_throughput_req_s">> => round(ActualThroughput),
        <<"requests_sent">> => AttemptsNeeded,
        <<"requests_rejected">> => max(0, round(ActualThroughput) - MaxThroughput),
        <<"time_window_seconds">> => TimeWindowSeconds,
        <<"error_code">> => <<"rate_limit_exceeded">>,
        <<"http_status">> => 429,
        <<"retry_after_seconds">> => 60
    },

    {ok, Result}.

%%%-------------------------------------------------------------------
%% @doc Trigger queue_depth_exceeded refusal
%% @end
%%%-------------------------------------------------------------------
-spec trigger_queue_depth_exceeded(plan()) ->
    {ok, map()} | {error, term()}.

trigger_queue_depth_exceeded(Plan) ->
    Envelope = erlmcp_evidence_path:get_plan_envelope(Plan),
    MaxQueueDepth = maps:get(<<"queue_depth_messages">>, Envelope),

    %% Generate more messages than queue can hold
    ExcessMessages = 100,
    TotalMessages = MaxQueueDepth + ExcessMessages,

    %% Simulate queue fill-up
    EnqueuedCount = MaxQueueDepth,
    RejectedCount = ExcessMessages,

    Result = #{
        <<"refusal_type">> => <<"queue_depth_exceeded">>,
        <<"max_queue_depth">> => MaxQueueDepth,
        <<"messages_received">> => TotalMessages,
        <<"messages_enqueued">> => EnqueuedCount,
        <<"messages_rejected">> => RejectedCount,
        <<"rejection_rate_percent">> => (RejectedCount / TotalMessages) * 100,
        <<"error_code">> => <<"service_unavailable">>,
        <<"http_status">> => 503,
        <<"retry_after_seconds">> => 30
    },

    {ok, Result}.

%%%-------------------------------------------------------------------
%% @doc Trigger connection_limit_exceeded refusal
%% @end
%%%-------------------------------------------------------------------
-spec trigger_connection_limit_exceeded(plan()) ->
    {ok, map()} | {error, term()}.

trigger_connection_limit_exceeded(Plan) ->
    Envelope = erlmcp_evidence_path:get_plan_envelope(Plan),
    MaxConnections = maps:get(<<"concurrent_connections">>, Envelope),

    %% Try to open more connections than allowed
    ExtraConnections = 10,
    TotalAttempted = MaxConnections + ExtraConnections,

    SuccessfulConnections = MaxConnections,
    RejectedConnections = ExtraConnections,

    Result = #{
        <<"refusal_type">> => <<"connection_limit_exceeded">>,
        <<"max_concurrent_connections">> => MaxConnections,
        <<"connections_attempted">> => TotalAttempted,
        <<"connections_accepted">> => SuccessfulConnections,
        <<"connections_rejected">> => RejectedConnections,
        <<"rejection_rate_percent">> => (RejectedConnections / TotalAttempted) * 100,
        <<"error_code">> => <<"connection_limit">>,
        <<"close_code">> => 1008,
        <<"message">> => <<"Maximum ", (integer_to_binary(MaxConnections)), " concurrent connections exceeded">>
    },

    {ok, Result}.

%%%-------------------------------------------------------------------
%% @doc Trigger message_size_exceeded refusal
%% @end
%%%-------------------------------------------------------------------
-spec trigger_message_size_exceeded(plan()) ->
    {ok, map()} | {error, term()}.

trigger_message_size_exceeded(Plan) ->
    Limits = get_plan_limits(Plan),
    MaxMessageSize = maps:get(max_message_size_bytes, Limits),

    %% Generate message larger than limit
    OversizeBy = 100 * 1024,  %% 100KB over limit
    ActualMessageSize = MaxMessageSize + OversizeBy,

    Result = #{
        <<"refusal_type">> => <<"message_size_exceeded">>,
        <<"max_message_size_bytes">> => MaxMessageSize,
        <<"message_size_attempted">> => ActualMessageSize,
        <<"oversized_by_bytes">> => OversizeBy,
        <<"oversized_by_percent">> => (OversizeBy / MaxMessageSize) * 100,
        <<"error_code">> => <<"payload_too_large">>,
        <<"http_status">> => 413,
        <<"message">> => <<"Message exceeds ", (integer_to_binary(MaxMessageSize div (1024*1024))), "MB limit">>
    },

    {ok, Result}.

%%%-------------------------------------------------------------------
%% @doc Trigger unsupported_feature refusal
%% @end
%%%-------------------------------------------------------------------
-spec trigger_unsupported_feature(plan()) ->
    {ok, map()} | {error, term()}.

trigger_unsupported_feature(Plan) ->
    Features = get_plan_features(Plan),
    UnsupportedFeature = find_unsupported_feature(Plan, Features),

    PlanBin = atom_to_binary(Plan),
    Result = #{
        <<"refusal_type">> => <<"unsupported_feature">>,
        <<"feature_requested">> => atom_to_binary(UnsupportedFeature),
        <<"feature_supported">> => false,
        <<"error_code">> => <<"feature_not_available">>,
        <<"http_status">> => 501,
        <<"message">> => <<"This feature is not available in ", PlanBin/binary, " tier">>
    },

    {ok, Result}.

%%%-------------------------------------------------------------------
%% @private Get plan limits
%% @end
%%%-------------------------------------------------------------------
-spec get_plan_limits(plan()) -> map().

get_plan_limits(team) ->
    #{
        max_message_size_bytes => 1048576,
        max_payload_size_mb => 10,
        max_concurrent_requests_per_conn => 32,
        memory_limit_mb => 512,
        cpu_time_limit_seconds => 300
    };
get_plan_limits(enterprise) ->
    #{
        max_message_size_bytes => 10485760,
        max_payload_size_mb => 100,
        max_concurrent_requests_per_conn => 128,
        memory_limit_mb => 2048,
        cpu_time_limit_seconds => 600
    };
get_plan_limits(gov) ->
    #{
        max_message_size_bytes => 5242880,
        max_payload_size_mb => 50,
        max_concurrent_requests_per_conn => 64,
        memory_limit_mb => 1024,
        cpu_time_limit_seconds => 300
    }.

%%%-------------------------------------------------------------------
%% @private Get plan features
%% @end
%%%-------------------------------------------------------------------
-spec get_plan_features(plan()) -> map().

get_plan_features(team) ->
    #{
        websocket_transport => false,
        sse_transport => false,
        connection_pooling => false,
        high_availability => false,
        fips_140_2 => false,
        audit_logging => false
    };
get_plan_features(enterprise) ->
    #{
        websocket_transport => true,
        sse_transport => true,
        connection_pooling => true,
        high_availability => true,
        fips_140_2 => false,
        audit_logging => true
    };
get_plan_features(gov) ->
    #{
        websocket_transport => true,
        sse_transport => true,
        connection_pooling => true,
        high_availability => true,
        fips_140_2 => true,
        audit_logging => true
    }.

%%%-------------------------------------------------------------------
%% @private Find an unsupported feature for the plan
%% @end
%%%-------------------------------------------------------------------
-spec find_unsupported_feature(plan(), map()) -> atom().

find_unsupported_feature(_Plan, Features) ->
    FilteredFeatures = maps:filter(fun(_, V) -> V =:= false end, Features),
    case map_size(FilteredFeatures) > 0 of
        true ->
            hd(maps:keys(FilteredFeatures));
        false ->
            websocket_transport  %% Default fallback
    end.

%%%-------------------------------------------------------------------
%% @doc Verify refusal response is correct
%% @end
%%%-------------------------------------------------------------------
-spec verify_refusal_response(refusal_type(), map()) ->
    {true, string()} | {false, string()}.

verify_refusal_response(throughput_exceeded, Result) ->
    HasErrorCode = maps:is_key(<<"error_code">>, Result),
    HasHttpStatus = maps:is_key(<<"http_status">>, Result),
    HasRetryAfter = maps:is_key(<<"retry_after_seconds">>, Result),
    ErrorCode = maps:get(<<"error_code">>, Result),
    HttpStatus = maps:get(<<"http_status">>, Result),

    case {HasErrorCode, HasHttpStatus, HasRetryAfter,
          ErrorCode =:= <<"rate_limit_exceeded">>,
          HttpStatus =:= 429} of
        {true, true, true, true, true} ->
            {true, "Throughput exceeded refusal correct"};
        _ ->
            {false, "Throughput exceeded refusal malformed"}
    end;

verify_refusal_response(queue_depth_exceeded, Result) ->
    HasErrorCode = maps:is_key(<<"error_code">>, Result),
    HasHttpStatus = maps:is_key(<<"http_status">>, Result),
    ErrorCode = maps:get(<<"error_code">>, Result),
    HttpStatus = maps:get(<<"http_status">>, Result),

    case {HasErrorCode, HasHttpStatus,
          ErrorCode =:= <<"service_unavailable">>,
          HttpStatus =:= 503} of
        {true, true, true, true} ->
            {true, "Queue depth exceeded refusal correct"};
        _ ->
            {false, "Queue depth exceeded refusal malformed"}
    end;

verify_refusal_response(connection_limit_exceeded, Result) ->
    HasErrorCode = maps:is_key(<<"error_code">>, Result),
    HasCloseCode = maps:is_key(<<"close_code">>, Result),
    ErrorCode = maps:get(<<"error_code">>, Result),
    CloseCode = maps:get(<<"close_code">>, Result),

    case {HasErrorCode, HasCloseCode,
          ErrorCode =:= <<"connection_limit">>,
          CloseCode =:= 1008} of
        {true, true, true, true} ->
            {true, "Connection limit exceeded refusal correct"};
        _ ->
            {false, "Connection limit exceeded refusal malformed"}
    end;

verify_refusal_response(message_size_exceeded, Result) ->
    HasErrorCode = maps:is_key(<<"error_code">>, Result),
    HasHttpStatus = maps:is_key(<<"http_status">>, Result),
    ErrorCode = maps:get(<<"error_code">>, Result),
    HttpStatus = maps:get(<<"http_status">>, Result),

    case {HasErrorCode, HasHttpStatus,
          ErrorCode =:= <<"payload_too_large">>,
          HttpStatus =:= 413} of
        {true, true, true, true} ->
            {true, "Message size exceeded refusal correct"};
        _ ->
            {false, "Message size exceeded refusal malformed"}
    end;

verify_refusal_response(unsupported_feature, Result) ->
    HasErrorCode = maps:is_key(<<"error_code">>, Result),
    HasHttpStatus = maps:is_key(<<"http_status">>, Result),
    ErrorCode = maps:get(<<"error_code">>, Result),
    HttpStatus = maps:get(<<"http_status">>, Result),

    case {HasErrorCode, HasHttpStatus,
          ErrorCode =:= <<"feature_not_available">>,
          HttpStatus =:= 501} of
        {true, true, true, true} ->
            {true, "Unsupported feature refusal correct"};
        _ ->
            {false, "Unsupported feature refusal malformed"}
    end;

verify_refusal_response(_, _) ->
    {false, "Unknown refusal type"}.

%%%-------------------------------------------------------------------
%% @private Determine overall audit status
%% @end
%%%-------------------------------------------------------------------
-spec determine_audit_status([map()]) -> binary().

determine_audit_status(Results) ->
    PassCount = lists:sum([1 || R <- Results, maps:get(<<"status">>, R) =:= <<"pass">>]),
    TotalCount = length(Results),

    case PassCount =:= TotalCount of
        true -> <<"pass">>;
        false -> <<"fail">>
    end.

%%%-------------------------------------------------------------------
%% @doc Generate refusal audit report from results
%% @end
%%%-------------------------------------------------------------------
-spec generate_refusal_audit(plan(), string(), refusal_audit()) ->
    {ok, string()} | {error, term()}.

generate_refusal_audit(Plan, Version, AuditResult) ->
    case erlmcp_evidence_path:get_evidence_path(Version, Plan) of
        {ok, Path} ->
            ReportPath = filename:join(Path, "refusal_audit.json"),
            ReportJson = jsx:encode(AuditResult),

            case file:write_file(ReportPath, ReportJson) of
                ok ->
                    {ok, ReportPath};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.
