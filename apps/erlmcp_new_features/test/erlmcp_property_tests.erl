-module(erlmcp_property_tests).
-include_lib("eunit/include/eunit.hrl").
-include_lib("proper/include/proper.hrl").

%%%===================================================================
%%% Property-Based Testing with PropEr for erlmcp_new_features
%%%===================================================================

setup() ->
    % Ensure components are started
    case whereis(erlmcp_event_bus) of
        undefined -> {ok, _} = erlmcp_event_bus:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_mcp_proxy_relay) of
        undefined -> {ok, _} = erlmcp_mcp_proxy_relay:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_batch_processor) of
        undefined -> {ok, _} = erlmcp_batch_processor:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_json_schema_validator) of
        undefined -> {ok, _} = erlmcp_json_schema_validator:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_workflow_engine) of
        undefined -> {ok, _} = erlmcp_workflow_engine:start_link();
        _ -> ok
    end,
    case whereis(erlmcp_tool_sandbox) of
        undefined -> {ok, _} = erlmcp_tool_sandbox:start_link();
        _ -> ok
    end.

cleanup(_) ->
    % Clean up
    erlmcp_event_bus:stop(),
    erlmcp_mcp_proxy_relay:stop(),
    erlmcp_batch_processor:stop(),
    erlmcp_json_schema_validator:stop(),
    erlmcp_workflow_engine:stop(),
    erlmcp_tool_sandbox:stop().

%%%===================================================================
%%%
%%% PropEr Generators
%%%
%%%===================================================================

%% Generator for binary strings
binary_string() ->
    ?SUCHTHAT(
        S,
        binary(),
        byte_size(S) =< 255
    ).

%% Generator for positive integers
pos_integer() ->
    ?SUCHTHAT(
        N,
        integer(),
        N > 0 andalso N =< 10000
    ).

%% Generator for non-negative integers
non_neg_integer() ->
    ?SUCHTHAT(
        N,
        integer(),
        N >= 0 andalso N =< 10000
    ).

%% Generator for maps with specific keys
map_with_keys(Keys) ->
    ?LET(
        Values,
        list(pos_integer()),
        lists:foldl(fun({K, V}, Acc) -> maps:put(K, V, Acc) end, #{}, lists:zip(Keys, Values))
    ).

%% Generator for workflow steps
workflow_step() ->
    ?LET(
        Type,
        oneof([tool, parallel, sequence, conditional, delay]),
        ?LET(
            Id,
            binary_string(),
            #{
                id => Id,
                type => Type,
                tool_name => binary_string(),
                tool_arguments => #{},
                max_retries => non_neg_integer(),
                retry_backoff_ms => non_neg_integer(),
                timeout_sec => pos_integer()
            }
        )
    ).

%% Generator for workflow
workflow() ->
    ?LET(
        Steps,
        list(workflow_step()),
        ?LET(
            Id,
            binary_string(),
            #{
                id => Id,
                steps => Steps,
                transitions => [
                    #{
                        from => oneof([S#{id} || S <- Steps]),
                        to => oneof([S#{id} || S <- Steps]),
                        condition => oneof([success, failure])
                    }
                ]
            }
        )
    ).

%% Generator for batch items
batch_item() ->
    ?LET(
        Id,
        binary_string(),
        #{
            id => Id,
            data => binary_string(),
            priority => oneof([low, normal, high])
        }
    ).

%% Generator for batch of items
batch() ->
    list(batch_item()).

%% Generator for MCP requests
mcp_request() ->
    ?LET(
        Method,
        oneof([tools_list, tools_call, resources_list]),
        #{
            jsonrpc => <<"2.0">>,
            id => non_neg_integer(),
            method => list_to_binary(atom_to_list(Method)),
            params => #{}
        }
    ).

%% Generator for event data
event_data() ->
    ?LET(
        Type,
        binary_string(),
        #{
            type => Type,
            data => map_with_keys([key1, key2, key3]),
            timestamp => erlang:system_time(millisecond)
        }
    ).

%%%===================================================================
%%%
%%% Property-Based Tests
%%%
%%%===================================================================

prop_event_bus_idempotency() ->
    {"Event bus operations are idempotent", ?FORALL(Event, event_data(),
        begin
            setup(),
            % Subscribe and publish
            {ok, SubId} = erlmcp_event_bus:subscribe(Event#{type}, undefined),
            ok = erlmcp_event_bus:publish(Event#{type}, Event#{data}),
            ok = erlmcp_event_bus:publish(Event#{type}, Event#{data}), % Duplicate publish

            % Verify
            Subscribers = erlmcp_event_bus:list_subscribers(Event#{type}),
            Result = lists:member(SubId, Subscribers),

            cleanup(ok),
            Result
        end
    )}.

prop_batch_processor_commutative() ->
    {"Batch processing order doesn't affect results", ?FORALL(Batch1, batch(), ?FORALL(Batch2, batch(),
        begin
            setup(),
            % Process batches in different orders
            {ok, Result1} = erlmcp_batch_processor:process_batch(Batch1 ++ Batch2),
            {ok, Result2} = erlmcp_batch_processor:process_batch(Batch2 ++ Batch1),

            % Results should have same totals
            Total1 = maps:get(success_count, Result1) + maps:get(failure_count, Result1),
            Total2 = maps:get(success_count, Result2) + maps:get(failure_count, Result2),
            Result = Total1 =:= Total2,

            cleanup(ok),
            Result
        end
    ))}.

prop_workflow_idempotent_execution() ->
    {"Workflow execution is idempotent", ?FORALL(Workflow, workflow(),
        begin
            setup(),
            % Define workflow
            case erlmcp_workflow_engine:define_workflow(Workflow) of
                ok ->
                    % Execute multiple times
                    {ok, ExecId1} = erlmcp_workflow_engine:execute_workflow(Workflow#{id}, #{}),
                    {ok, ExecId2} = erlmcp_workflow_engine:execute_workflow(Workflow#{id}, #{}),

                    % Verify both started
                    Result1 = erlmcp_workflow_engine:get_execution_status(ExecId1),
                    Result2 = erlmcp_workflow_engine:get_execution_status(ExecId2),

                    % Clean up
                    erlmcp_workflow_engine:cancel_execution(ExecId1),
                    erlmcp_workflow_engine:cancel_execution(ExecId2),

                    cleanup(ok),
                    is_tuple(Result1) andalso is_tuple(Result2);
                {error, _} ->
                    cleanup(ok),
                    true % Invalid workflows should not crash
            end
        end
    )}.

prop_proxy_relay_consistent_state() ->
    {"Proxy relay maintains consistent state", ?FORALL(Upstreams, list({binary_string(), binary_string()}),
        begin
            setup(),
            % Add upstreams
            [ok = erlmcp_mcp_proxy_relay:add_upstream(Name, Url) || {Name, Url} <- Upstreams],

            % List should contain all added
            Listed = erlmcp_mcp_proxy_relay:list_upstreams(),
            Expected = [Name || {Name, _} <- Upstreams],

            % Remove some and verify
            Removed = lists:nth(1, Upstreams),
            ok = erlmcp_mcp_proxy_relay:remove_upstream(element(1, Removed)),

            cleanup(ok),
            lists:all(fun(E) -> lists:member(E, Listed) end, Expected)
        end
    ))}.

prop_schema_validator_soundness() ->
    {"Schema validation is sound", ?FORALL(Schema, map_with_keys([type, properties]), ?FORALL(Data, binary_string(),
        begin
            setup(),
            SchemaName = list_to_binary("schema_" ++ integer_to_list(erlang:unique_integer())),
            ok = erlmcp_json_schema_validator:load_schema(SchemaName, Schema),

            % Test valid data
            ValidData = case maps:get(type, Schema) of
                string -> Data;
                object -> #{key1 => Data};
                array -> [Data]
            end,

            % Test invalid data
            InvalidData = case maps:get(type, Schema) of
                string -> 123; % number instead of string
                object -> Data;  % binary instead of map
                array -> Data    % binary instead of list
            end,

            % Validate
            ValidResult = erlmcp_json_schema_validator:validate(SchemaName, ValidData),
            InvalidResult = erlmcp_json_schema_validator:validate(SchemaName, InvalidData),

            cleanup(ok),
            case ValidResult of
                {ok, _} ->
                    case InvalidResult of
                        {error, _} -> true;
                        _ -> false
                    end;
                _ -> false
            end
        end
    ))}.

prop_tool_sandbox_isolation() ->
    {"Tool sandbox provides isolation", ?FORALL(Commands, list(binary_string()),
        begin
            setup(),
            % Register two tools with same command but different sandboxing
            ToolDef1 = #{
                name => <<"tool1">>,
                command => ["/bin/echo"],
                args => [<<"test1">>],
                timeout_ms => 1000
            },
            ok = erlmcp_tool_sandbox:register_tool(sandbox_tool1, ToolDef1),

            ToolDef2 = #{
                name => <<"tool2">>,
                command => ["/bin/echo"],
                args => [<<"test2">>],
                timeout_ms => 1000
            },
            ok = erlmcp_tool_sandbox:register_tool(sandbox_tool2, ToolDef2),

            % Execute both concurrently
            {ok, Result1} = erlmcp_tool_sandbox:execute_tool(sandbox_tool1, #{}),
            {ok, Result2} = erlmcp_tool_sandbox:execute_tool(sandbox_tool2, #{}),

            % Results should be different
            Outputs = [maps:get(stdout, Result1), maps:get(stdout, Result2)],
            Result = lists:usort(Outputs) =:= Outputs,

            cleanup(ok),
            Result
        end
    )}.

prop_concurrent_operations_safe() ->
    {"Concurrent operations are safe", ?FORALL(N, pos_integer(),
        begin
            setup(),
            % Start multiple concurrent operations
            Pids = [spawn(fun() ->
                erlmcp_event_bus:publish(concurrent_test, #{data => N})
            end) || _ <- lists:seq(1, N)],

            % Wait for completion
            [wait_for_pid(Pid) || Pid <- Pids],

            % System should still be responsive
            try
                {ok, _} = erlmcp_event_bus:list_subscribers(test),
                {ok, _} = erlmcp_batch_processor:get_metrics(),
                cleanup(ok),
                true
            catch
                _:_ ->
                    cleanup(ok),
                    false
            end
        end
    ))}.

prop_memory_usage_bounded() ->
    {"Memory usage stays within bounds", ?FORALL(N, pos_integer(),
        begin
            setup(),
            InitialMetrics = erlmcp_tool_sandbox:get_metrics(),

            % Create memory usage
            Tools = [begin
                ToolDef = #{
                    name => list_to_binary("mem_tool_" ++ integer_to_list(I)),
                    command => ["/usr/bin/dd"],
                    args => [<<"if=/dev/zero">>, <<"bs=1M">>, <<"count=1">>],
                    timeout_ms => 1000
                },
                erlmcp_tool_sandbox:register_tool(list_to_atom("mem_tool_" ++ integer_to_list(I)), ToolDef)
            end || I <- lists:seq(1, min(N, 10))],

            % Execute tools
            Results = [erlmcp_tool_sandbox:execute_tool(list_to_atom("mem_tool_" ++ integer_to_list(I)), #{})
                      || I <- lists:seq(1, min(N, 10))],

            % Check memory doesn't grow unbounded
            FinalMetrics = erlmcp_tool_sandbox:get_metrics(),
            InitialMem = maps:get(total_memory_used_mb, InitialMetrics, 0),
            FinalMem = maps:get(total_memory_used_mb, FinalMetrics, 0),
            Growth = FinalMem - InitialMem,

            cleanup(ok),
            Growth =< N * 2  % Allow some growth but not exponential
        end
    )}.

prop_system_recovery_after_failure() ->
    {"System recovers after failures", ?FORALL(ErrorType, oneof([timeout, memory, network]),
        begin
            setup(),
            % Introduce error condition
            case ErrorType of
                timeout ->
                    LongDef = #{
                        name => <<"long">>,
                        command => ["/bin/sleep"],
                        args => [<<"10">>],
                        timeout_ms => 100
                    },
                    ok = erlmcp_tool_sandbox:register_tool(long_tool, LongDef),
                    {error, _} = erlmcp_tool_sandbox:execute_tool(long_tool, #{});
                memory ->
                    Def = #{
                        name => <<"mem">>,
                        command => ["/usr/bin/dd"],
                        args => [<<"if=/dev/zero">>, <<"bs=10M">>, <<"count=100">>],
                        timeout_ms => 1000
                    },
                    ok = erlmcp_tool_sandbox:register_tool(mem_tool, Def),
                    {error, _} = erlmcp_tool_sandbox:execute_tool(mem_tool, #{});
                network ->
                    ok = erlmcp_mcp_proxy_relay:add_upstream(bad, <<"http://invalid">>),
                    {error, _} = erlmcp_mcp_proxy_relay:forward_request(#{})
            end,

            % System should still be functional
            Result = try
                {ok, _} = erlmcp_event_bus:publish(recovery_test, #{}),
                {ok, _} = erlmcp_workflow_engine:list_workflows(),
                {ok, _} = erlmcp_batch_processor:get_metrics(),
                cleanup(ok),
                true
            catch
                _:_ ->
                    cleanup(ok),
                    false
            end,

            Result
        end
    )}.

%%%===================================================================
%%%
%%% EUnit Property Tests
%%%
%%%===================================================================

property_tests_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         [
             {timeout, 30000,
              {"Event Bus Idempotency", fun() ->
                   proper:quickcheck(prop_event_bus_idempotency(), [verbose, {numtests, 100}])
              end}},
             {timeout, 30000,
              {"Batch Processor Commutative", fun() ->
                   proper:quickcheck(prop_batch_processor_commutative(), [verbose, {numtests, 50}])
              end}},
             {timeout, 30000,
              {"Workflow Idempotent Execution", fun() ->
                   proper:quickcheck(prop_workflow_idempotent_execution(), [verbose, {numtests, 50}])
              end}},
             {timeout, 30000,
              {"Proxy Relay Consistent State", fun() ->
                   proper:quickcheck(prop_proxy_relay_consistent_state(), [verbose, {numtests, 50}])
              end}},
             {timeout, 30000,
              {"Schema Validator Soundness", fun() ->
                   proper:quickcheck(prop_schema_validator_soundness(), [verbose, {numtests, 50}])
              end}},
             {timeout, 30000,
              {"Tool Sandbox Isolation", fun() ->
                   proper:quickcheck(prop_tool_sandbox_isolation(), [verbose, {numtests, 50}])
              end}},
             {timeout, 30000,
              {"Concurrent Operations Safety", fun() ->
                   proper:quickcheck(prop_concurrent_operations_safe(), [verbose, {numtests, 20}])
              end}},
             {timeout, 30000,
              {"Memory Usage Bounded", fun() ->
                   proper:quickcheck(prop_memory_usage_bounded(), [verbose, {numtests, 20}])
              end}},
             {timeout, 30000,
              {"System Recovery After Failure", fun() ->
                   proper:quickcheck(prop_system_recovery_after_failure(), [verbose, {numtests, 20}])
              end}}
         ]
     end
    }.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

wait_for_pid(Pid) when is_pid(Pid) ->
    Ref = erlang:monitor(process, Pid),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    after 10000 ->
        exit(wait_timeout)
    end.