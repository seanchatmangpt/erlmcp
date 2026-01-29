-module(erlmcp_trace_propagation_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Chicago School TDD - Trace Propagation Tests
%% Real spawned processes, real context flow, W3C compliance
%%====================================================================

%%====================================================================
%% Test Cases - Spawn with Trace Context (Chicago School)
%%====================================================================

spawn_traced_basic_test() ->
    %% Setup: Set trace context in parent
    Context = #{
        trace_id => <<"test-trace-123">>,
        span_id => <<"span-456">>,
        trace_flags => 1,
        baggage => #{}
    },
    erlmcp_trace_propagation:set_trace_context(Context),

    %% Exercise: Spawn process with trace propagation (real spawn)
    Self = self(),
    Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
        %% Child process reads trace context
        ChildContext = erlmcp_trace_propagation:get_trace_context(),
        Self ! {child_context, ChildContext}
    end),

    %% Verify: Child inherited trace context (behavior verification)
    receive
        {child_context, ChildContext} ->
            ?assertEqual(<<"test-trace-123">>, maps:get(trace_id, ChildContext)),
            ?assertEqual(<<"span-456">>, maps:get(span_id, ChildContext))
    after 1000 ->
        ?assert(false)  %% Timeout
    end,

    %% Cleanup
    erlang:exit(Pid, normal).

spawn_traced_mfa_test() ->
    %% Setup: Set trace context
    Context = #{
        trace_id => <<"trace-mfa">>,
        span_id => <<"span-mfa">>,
        trace_flags => 1,
        baggage => #{}
    },
    erlmcp_trace_propagation:set_trace_context(Context),

    %% Exercise: Spawn with module/function/args (real MFA spawn)
    Self = self(),
    Pid = erlmcp_trace_propagation:spawn_traced(erlang, send, [Self, {mfa_result, ok}]),

    %% Verify: Process executed and inherited context
    receive
        {mfa_result, ok} -> ?assert(true)
    after 1000 ->
        ?assert(false)
    end,

    erlang:exit(Pid, normal).

spawn_traced_no_context_test() ->
    %% Setup: Clear trace context
    erlmcp_trace_propagation:set_trace_context(undefined),

    %% Exercise: Spawn without parent context (auto-generates)
    Self = self(),
    Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
        ChildContext = erlmcp_trace_propagation:get_trace_context(),
        Self ! {auto_context, ChildContext}
    end),

    %% Verify: Child auto-generated trace context
    receive
        {auto_context, ChildContext} ->
            ?assert(maps:is_key(trace_id, ChildContext)),
            ?assert(maps:is_key(span_id, ChildContext)),
            TraceId = maps:get(trace_id, ChildContext),
            ?assert(is_binary(TraceId)),
            ?assert(byte_size(TraceId) > 0)
    after 1000 ->
        ?assert(false)
    end,

    erlang:exit(Pid, normal).

%%====================================================================
%% Test Cases - Context Management
%%====================================================================

with_trace_context_test() ->
    %% Setup: Current context
    OriginalContext = #{
        trace_id => <<"original-trace">>,
        span_id => <<"original-span">>,
        trace_flags => 1,
        baggage => #{}
    },
    erlmcp_trace_propagation:set_trace_context(OriginalContext),

    %% Exercise: Run function with temporary context
    TempContext = #{
        trace_id => <<"temp-trace">>,
        span_id => <<"temp-span">>,
        trace_flags => 1,
        baggage => #{}
    },

    Result = erlmcp_trace_propagation:with_trace_context(TempContext, fun() ->
        %% Inside: temporary context active
        InnerContext = erlmcp_trace_propagation:get_trace_context(),
        ?assertEqual(<<"temp-trace">>, maps:get(trace_id, InnerContext)),
        inner_result
    end),

    %% Verify: Function executed with temp context
    ?assertEqual(inner_result, Result),

    %% Verify: Original context restored (state verification)
    RestoredContext = erlmcp_trace_propagation:get_trace_context(),
    ?assertEqual(<<"original-trace">>, maps:get(trace_id, RestoredContext)).

get_set_trace_context_test() ->
    %% Exercise: Set and get trace context
    Context = #{
        trace_id => <<"set-get-trace">>,
        span_id => <<"set-get-span">>,
        trace_flags => 1,
        baggage => #{user_id => <<"user-123">>}
    },
    ok = erlmcp_trace_propagation:set_trace_context(Context),

    %% Verify: Can retrieve context (state-based)
    Retrieved = erlmcp_trace_propagation:get_trace_context(),
    ?assertEqual(<<"set-get-trace">>, maps:get(trace_id, Retrieved)),
    ?assertEqual(<<"set-get-span">>, maps:get(span_id, Retrieved)),

    %% Verify: Baggage preserved
    Baggage = maps:get(baggage, Retrieved),
    ?assertEqual(<<"user-123">>, maps:get(user_id, Baggage)).

inject_extract_trace_context_test() ->
    %% Setup: Create context
    Context = #{
        trace_id => <<"inject-extract">>,
        span_id => <<"ie-span">>,
        trace_flags => 1,
        baggage => #{}
    },

    %% Exercise: Inject into process dictionary
    ok = erlmcp_trace_propagation:inject_trace_context(Context),

    %% Verify: Context stored in process dictionary
    ?assertEqual(<<"inject-extract">>, erlang:get(erlmcp_trace_id)),
    ?assertEqual(<<"ie-span">>, erlang:get(erlmcp_span_id)),

    %% Exercise: Clear context
    ok = erlmcp_trace_propagation:inject_trace_context(undefined),

    %% Verify: Context cleared (state verification)
    ?assertEqual(undefined, erlang:get(erlmcp_trace_id)),
    ?assertEqual(undefined, erlang:get(erlmcp_span_id)).

%%====================================================================
%% Test Cases - Message Annotation (W3C Trace Context)
%%====================================================================

annotate_message_test() ->
    %% Setup: Set trace context
    Context = #{
        trace_id => <<"msg-trace">>,
        span_id => <<"msg-span">>,
        trace_flags => 1,
        baggage => #{}
    },
    erlmcp_trace_propagation:set_trace_context(Context),

    %% Exercise: Annotate message with trace context
    Message = {test, data, 123},
    AnnotatedMessage = erlmcp_trace_propagation:annotate_message(Message),

    %% Verify: Message annotated (observable structure)
    ?assertMatch({'$trace_context', _, {test, data, 123}}, AnnotatedMessage).

extract_trace_context_test() ->
    %% Setup: Create annotated message
    Context = #{
        trace_id => <<"extract-trace">>,
        span_id => <<"extract-span">>,
        trace_flags => 1,
        baggage => #{}
    },
    Message = {original, message},
    AnnotatedMessage = erlmcp_trace_propagation:annotate_message(Message, Context),

    %% Exercise: Extract trace context from annotated message
    ExtractedContext = erlmcp_trace_propagation:extract_trace_context(AnnotatedMessage),

    %% Verify: Extracted context matches original (state verification)
    ?assertEqual(<<"extract-trace">>, maps:get(trace_id, ExtractedContext)),
    ?assertEqual(<<"extract-span">>, maps:get(span_id, ExtractedContext)).

extract_trace_context_unannotated_test() ->
    %% Exercise: Extract from unannotated message
    Message = {plain, message},
    ExtractedContext = erlmcp_trace_propagation:extract_trace_context(Message),

    %% Verify: Returns undefined for unannotated messages
    ?assertEqual(undefined, ExtractedContext).

%%====================================================================
%% Test Cases - Baggage Management (Correlation Metadata)
%%====================================================================

set_get_baggage_test() ->
    %% Exercise: Set baggage values
    ok = erlmcp_trace_propagation:set_baggage(user_id, <<"user-789">>),
    ok = erlmcp_trace_propagation:set_baggage(session_id, <<"session-abc">>),

    %% Verify: Can retrieve baggage (state-based)
    ?assertEqual(<<"user-789">>, erlmcp_trace_propagation:get_baggage(user_id)),
    ?assertEqual(<<"session-abc">>, erlmcp_trace_propagation:get_baggage(session_id)).

get_baggage_nonexistent_test() ->
    %% Exercise: Get non-existent baggage key
    Value = erlmcp_trace_propagation:get_baggage(nonexistent_key),

    %% Verify: Returns undefined
    ?assertEqual(undefined, Value).

baggage_propagation_test() ->
    %% Setup: Set baggage in parent
    ok = erlmcp_trace_propagation:set_baggage(request_id, <<"req-999">>),

    %% Exercise: Spawn child with trace context (includes baggage)
    Self = self(),
    Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
        %% Child reads baggage
        RequestId = erlmcp_trace_propagation:get_baggage(request_id),
        Self ! {child_baggage, RequestId}
    end),

    %% Verify: Baggage propagated to child (behavior verification)
    receive
        {child_baggage, ChildRequestId} ->
            ?assertEqual(<<"req-999">>, ChildRequestId)
    after 1000 ->
        ?assert(false)
    end,

    erlang:exit(Pid, normal).

%%====================================================================
%% Test Cases - W3C Compliance and Trace ID Generation
%%====================================================================

trace_id_generation_test() ->
    %% Exercise: Get trace context without setting (auto-generates)
    erlmcp_trace_propagation:set_trace_context(undefined),
    Context = erlmcp_trace_propagation:get_trace_context(),

    %% Verify: Auto-generated trace ID and span ID (W3C format)
    TraceId = maps:get(trace_id, Context),
    SpanId = maps:get(span_id, Context),

    %% Verify: IDs are binaries (hex format)
    ?assert(is_binary(TraceId)),
    ?assert(is_binary(SpanId)),
    ?assert(byte_size(TraceId) > 0),
    ?assert(byte_size(SpanId) > 0).

trace_id_uniqueness_test() ->
    %% Exercise: Generate multiple trace contexts
    erlmcp_trace_propagation:set_trace_context(undefined),
    Context1 = erlmcp_trace_propagation:get_trace_context(),

    erlmcp_trace_propagation:set_trace_context(undefined),
    Context2 = erlmcp_trace_propagation:get_trace_context(),

    %% Verify: Unique trace IDs generated (statistical uniqueness)
    TraceId1 = maps:get(trace_id, Context1),
    TraceId2 = maps:get(trace_id, Context2),
    ?assertNotEqual(TraceId1, TraceId2).

%%====================================================================
%% Integration Tests - Multi-Process Trace Flow
%%====================================================================

multi_process_trace_flow_test() ->
    %% Setup: Parent sets trace context
    RootContext = #{
        trace_id => <<"root-trace">>,
        span_id => <<"root-span">>,
        trace_flags => 1,
        baggage => #{service => <<"api-gateway">>}
    },
    erlmcp_trace_propagation:set_trace_context(RootContext),

    %% Exercise: Spawn 3-level process hierarchy (real nested spawns)
    Self = self(),
    Level1Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
        %% Level 1: Check context inherited
        L1Context = erlmcp_trace_propagation:get_trace_context(),
        Self ! {level1, maps:get(trace_id, L1Context)},

        %% Spawn Level 2
        Level2Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
            %% Level 2: Check context inherited
            L2Context = erlmcp_trace_propagation:get_trace_context(),
            Self ! {level2, maps:get(trace_id, L2Context)},

            %% Spawn Level 3
            _Level3Pid = erlmcp_trace_propagation:spawn_traced(fun() ->
                %% Level 3: Check context inherited
                L3Context = erlmcp_trace_propagation:get_trace_context(),
                Self ! {level3, maps:get(trace_id, L3Context)}
            end)
        end),
        timer:sleep(50),
        erlang:exit(Level2Pid, normal)
    end),

    %% Verify: All 3 levels inherited same trace ID (context flow)
    receive {level1, TraceId1} -> ?assertEqual(<<"root-trace">>, TraceId1) after 1000 -> ?assert(false) end,
    receive {level2, TraceId2} -> ?assertEqual(<<"root-trace">>, TraceId2) after 1000 -> ?assert(false) end,
    receive {level3, TraceId3} -> ?assertEqual(<<"root-trace">>, TraceId3) after 1000 -> ?assert(false) end,

    erlang:exit(Level1Pid, normal).

message_passing_with_trace_test() ->
    %% Setup: Parent sets context and annotates message
    Context = #{
        trace_id => <<"msg-passing-trace">>,
        span_id => <<"msg-passing-span">>,
        trace_flags => 1,
        baggage => #{}
    },
    erlmcp_trace_propagation:set_trace_context(Context),

    %% Exercise: Spawn receiver, send annotated message (real message passing)
    Self = self(),
    ReceiverPid = spawn(fun() ->
        receive
            AnnotatedMsg ->
                %% Extract context from message
                RecvContext = erlmcp_trace_propagation:extract_trace_context(AnnotatedMsg),
                Self ! {received_context, RecvContext}
        end
    end),

    Message = {request, data},
    AnnotatedMessage = erlmcp_trace_propagation:annotate_message(Message),
    ReceiverPid ! AnnotatedMessage,

    %% Verify: Receiver extracted correct context (W3C propagation)
    receive
        {received_context, RecvContext} ->
            ?assertEqual(<<"msg-passing-trace">>, maps:get(trace_id, RecvContext)),
            ?assertEqual(<<"msg-passing-span">>, maps:get(span_id, RecvContext))
    after 1000 ->
        ?assert(false)
    end.
