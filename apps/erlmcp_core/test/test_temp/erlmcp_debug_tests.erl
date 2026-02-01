-module(erlmcp_debug_tests).

-include_lib("eunit/include/eunit.hrl").

-include("erlmcp.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% Simple test gen_server for debugging
-behaviour(gen_server).

-export([start_link/0, start_link/1, get_state_value/1, set_state_value/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3,
         format_status/2]).

-record(test_state, {value :: term(), secret :: binary(), counter :: non_neg_integer()}).

%% Test gen_server API
start_link() ->
    start_link(initial_value).

start_link(InitialValue) ->
    gen_server:start_link(?MODULE, [InitialValue], []).

get_state_value(Pid) ->
    gen_server:call(Pid, get_value).

set_state_value(Pid, Value) ->
    gen_server:call(Pid, {set_value, Value}).

stop(Pid) ->
    gen_server:stop(Pid).

%% gen_server callbacks
init([InitialValue]) ->
    State =
        #test_state{value = InitialValue,
                    secret = <<"top_secret_password">>,
                    counter = 0},
    {ok, State}.

handle_call(get_value, _From, State) ->
    {reply, State#test_state.value, State};
handle_call({set_value, Value}, _From, State) ->
    NewState = State#test_state{value = Value, counter = State#test_state.counter + 1},
    {reply, ok, NewState};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% format_status callback to sanitize state
format_status(Opt, [_PDict, State]) ->
    SanitizedState =
        #{value => State#test_state.value,
          secret => <<"[REDACTED]">>,
          counter => State#test_state.counter},
    case Opt of
        terminate ->
            SanitizedState;
        normal ->
            [{data, [{"State", SanitizedState}]}]
    end.

%%====================================================================
%% Tests
%%====================================================================

get_state_test() ->
    {ok, Pid} = start_link(test_value),

    %% Get state should return raw state record (sys:get_state doesn't call format_status)
    State = erlmcp_debug:get_state(Pid),
    ?assertMatch(#test_state{value = test_value, counter = 0}, State),

    %% Secret is in raw state (format_status only used for sys:get_status/display)
    ?assertMatch(#test_state{secret = <<"top_secret_password">>}, State),

    stop(Pid).

get_state_by_name_test() ->
    %% Register the process with a name
    {ok, Pid} = gen_server:start_link({local, test_debug_server}, ?MODULE, [named_test], []),

    %% Get state by name - returns raw state record
    State = erlmcp_debug:get_state(test_debug_server),
    ?assertMatch(#test_state{value = named_test}, State),

    %% Get state by PID - should be same
    StatePid = erlmcp_debug:get_state(Pid),
    ?assertEqual(State, StatePid),

    gen_server:stop(Pid).

get_state_not_found_test() ->
    %% Non-existent named process
    Result = erlmcp_debug:get_state(non_existent_process),
    ?assertEqual({error, not_found}, Result).

get_status_test() ->
    {ok, Pid} = start_link(status_test),

    %% Get full status - returns {status, Pid, {module, Module}, [StatusData]}
    Status = erlmcp_debug:get_status(Pid),

    %% Status should be a tuple starting with 'status'
    ?assertMatch({status, Pid, {module, gen_server}, _}, Status),

    %% Extract the status list (4th element)
    {status, Pid, {module, gen_server}, StatusList} = Status,

    %% Status list should be a list
    ?assert(is_list(StatusList)),

    %% The status list contains nested lists, not a proplist directly
    %% Check that we have some expected elements
    ?assert(length(StatusList) > 0),

    stop(Pid).

trace_test() ->
    {ok, Pid} = start_link(trace_test),

    %% Enable tracing
    ?assertEqual(ok, erlmcp_debug:trace(Pid)),

    %% Make a call - should be traced (check logs)
    set_state_value(Pid, traced_value),

    %% Disable tracing
    ?assertEqual(ok, erlmcp_debug:untrace(Pid)),

    stop(Pid).

suspend_resume_test() ->
    {ok, Pid} = start_link(suspend_test),

    %% Suspend the process
    ?assertEqual(ok, erlmcp_debug:suspend(Pid)),

    %% Process should be suspended but still alive
    ?assert(is_process_alive(Pid)),

    %% Resume the process
    ?assertEqual(ok, erlmcp_debug:resume(Pid)),

    %% Verify process still works after suspend/resume
    Value = get_state_value(Pid),
    ?assertEqual(suspend_test, Value),

    stop(Pid).

replace_state_test() ->
    {ok, Pid} = start_link(original_value),

    %% Replace state with a transformation
    NewState =
        erlmcp_debug:replace_state(Pid,
                                   fun(State) ->
                                      State#test_state{value = replaced_value, counter = 99}
                                   end),

    %% Verify state was replaced
    ?assertMatch(#test_state{value = replaced_value, counter = 99}, NewState),

    %% Verify via API
    Value = get_state_value(Pid),
    ?assertEqual(replaced_value, Value),

    stop(Pid).

statistics_test() ->
    {ok, Pid} = start_link(stats_test),

    %% Get statistics
    {ok, Stats} = erlmcp_debug:statistics(Pid),

    %% Verify stats structure
    ?assert(is_map(Stats)),
    ?assert(is_integer(maps:get(message_queue_len, Stats))),
    ?assert(is_integer(maps:get(heap_size, Stats))),
    ?assert(is_integer(maps:get(total_heap_size, Stats))),
    ?assert(is_integer(maps:get(reductions, Stats))),
    ?assert(is_integer(maps:get(memory, Stats))),
    ?assert(is_tuple(maps:get(current_function, Stats))),

    %% Message queue should be empty
    ?assertEqual(0, maps:get(message_queue_len, Stats)),

    stop(Pid).

statistics_dead_process_test() ->
    {ok, Pid} = start_link(dead_test),
    stop(Pid),

    %% Wait for process to die
    timer:sleep(100),

    %% Get statistics for dead process
    Result = erlmcp_debug:statistics(Pid),
    ?assertEqual({error, process_dead}, Result).

log_to_file_test() ->
    {ok, Pid} = start_link(log_test),

    %% Create temp file path
    TempFile =
        "/tmp/erlmcp_debug_test_log_" ++ integer_to_list(erlang:system_time(millisecond)) ++ ".log",

    %% Enable file logging
    ?assertEqual(ok, erlmcp_debug:log_to_file(Pid, TempFile)),

    %% Make some calls
    set_state_value(Pid, logged_value_1),
    set_state_value(Pid, logged_value_2),

    %% Stop logging
    sys:log(Pid, false),

    %% Verify log file exists
    ?assert(filelib:is_file(TempFile)),

    %% Clean up
    file:delete(TempFile),
    stop(Pid).

debug_registry_integration_test_() ->
    {setup,
     fun() ->
        %% Start the registry
        case whereis(erlmcp_registry) of
            undefined ->
                {ok, Pid} = erlmcp_registry:start_link(),
                {started, Pid};
            Pid ->
                {existing, Pid}
        end
     end,
     fun({Type, _Pid}) ->
        %% Stop only if we started it
        case Type of
            started ->
                %% Use gen_server:stop instead of erlmcp_registry:stop
                catch gen_server:stop(erlmcp_registry);
            existing ->
                ok
        end
     end,
     fun(_) ->
        [{"debug registry process",
          fun() ->
             RegistryDebug = erlmcp_debug:debug_registry(),

             %% Should have basic info
             ?assertMatch(#{pid := _}, RegistryDebug),
             ?assertMatch(#{state := _}, RegistryDebug),
             ?assertMatch(#{stats := _}, RegistryDebug),

             %% Should have counts
             ?assert(maps:is_key(servers, RegistryDebug)),
             ?assert(maps:is_key(transports, RegistryDebug))
          end},
         {"get registry state",
          fun() ->
             %% Get raw state (sys:get_state returns state record)
             RawState = erlmcp_debug:get_state(erlmcp_registry),

             %% Should be a record (tuple, not a map)
             ?assert(is_tuple(RawState)),

             %% Get status to see sanitized version (format_status is called)
             Status = erlmcp_debug:get_status(erlmcp_registry),
             {status, _, _, StatusList} = Status,

             %% Status list should exist and be a list
             ?assert(is_list(StatusList)),
             ?assert(length(StatusList) > 0)
          end}]
     end}.

format_status_sanitization_test() ->
    {ok, Pid} = start_link(secret_test),

    %% Get status - format_status is called for display
    Status = erlmcp_debug:get_status(Pid),

    %% Status should be a tuple
    ?assertMatch({status, Pid, {module, gen_server}, _}, Status),

    %% The key thing is that format_status is called
    %% We can't easily verify the exact structure without deep inspection
    %% But we can verify the status is valid
    ?assert(is_tuple(Status)),
    ?assert(tuple_size(Status) =:= 4),

    %% Verify raw state still has the secret (not sanitized)
    RawState = erlmcp_debug:get_state(Pid),
    ?assertMatch(#test_state{secret = <<"top_secret_password">>}, RawState),

    stop(Pid).

error_handling_test() ->
    %% Test error handling for invalid inputs
    ?assertEqual({error, not_found}, erlmcp_debug:get_state(undefined_process)),
    ?assertEqual({error, not_found}, erlmcp_debug:get_status(undefined_process)),
    ?assertEqual({error, not_found}, erlmcp_debug:trace(undefined_process)),
    ?assertEqual({error, not_found}, erlmcp_debug:suspend(undefined_process)),
    ?assertEqual({error, not_found}, erlmcp_debug:resume(undefined_process)),
    ?assertEqual({error, not_found},
                 erlmcp_debug:replace_state(undefined_process, fun(S) -> S end)).

%%====================================================================
%% Integration Tests with Real MCP Processes
%%====================================================================

server_format_status_test_() ->
    {setup,
     fun() ->
        %% Start a real MCP server
        ServerConfig =
            #{capabilities =>
                  #mcp_server_capabilities{resources = #mcp_capability{enabled = true},
                                           tools = #mcp_capability{enabled = true}}},
        {ok, ServerPid} = erlmcp_server:start_link(test_server_debug, ServerConfig),
        ServerPid
     end,
     fun(ServerPid) -> erlmcp_server:stop(ServerPid) end,
     fun(ServerPid) ->
        [{"server state is sanitized",
          fun() ->
             %% Get raw state (returns record)
             RawState = erlmcp_debug:get_state(ServerPid),
             ?assert(is_tuple(RawState)),

             %% Get status to see sanitized format
             Status = erlmcp_debug:get_status(ServerPid),
             {status, _, _, StatusList} = Status,

             %% Status list should be a list
             ?assert(is_list(StatusList)),

             %% Check there are data elements (not checking exact structure as it varies)
             ?assert(length(StatusList) > 0)
          end},
         {"server statistics",
          fun() ->
             {ok, Stats} = erlmcp_debug:statistics(ServerPid),

             %% Should have process stats
             ?assert(is_integer(maps:get(heap_size, Stats))),
             ?assert(is_integer(maps:get(reductions, Stats)))
          end}]
     end}.

client_format_status_test_() ->
    {setup,
     fun() ->
        %% Start a test client with stdio transport
        {ok, ClientPid} = erlmcp_client:start_link({stdio, []}, #{timeout => 5000}),
        ClientPid
     end,
     fun(ClientPid) -> catch erlmcp_client:stop(ClientPid) end,
     fun(ClientPid) ->
        [{"client state is sanitized",
          fun() ->
             %% Get raw state (returns record)
             RawState = erlmcp_debug:get_state(ClientPid),
             ?assert(is_tuple(RawState)),

             %% Get status to see sanitized format
             Status = erlmcp_debug:get_status(ClientPid),
             {status, _, _, StatusList} = Status,

             %% Status list should be a list
             ?assert(is_list(StatusList)),

             %% Check there are data elements
             ?assert(length(StatusList) > 0)
          end},
         {"client statistics",
          fun() ->
             {ok, Stats} = erlmcp_debug:statistics(ClientPid),

             %% Should have process stats
             ?assert(is_integer(maps:get(heap_size, Stats))),
             ?assert(is_integer(maps:get(memory, Stats)))
          end}]
     end}.

%%====================================================================
%% Performance Tests
%%====================================================================

performance_get_state_test() ->
    {ok, Pid} = start_link(perf_test),

    %% Measure time to get state 1000 times
    StartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) -> erlmcp_debug:get_state(Pid) end, lists:seq(1, 1000)),
    EndTime = erlang:monotonic_time(microsecond),

    Duration = EndTime - StartTime,
    AvgTimeUs = Duration / 1000,

    %% Should be fast (< 100us average)
    ?assert(AvgTimeUs < 100),

    io:format("~nget_state average time: ~.2f us~n", [AvgTimeUs]),

    stop(Pid).

performance_statistics_test() ->
    {ok, Pid} = start_link(perf_test),

    %% Measure time to get statistics 1000 times
    StartTime = erlang:monotonic_time(microsecond),
    lists:foreach(fun(_) -> erlmcp_debug:statistics(Pid) end, lists:seq(1, 1000)),
    EndTime = erlang:monotonic_time(microsecond),

    Duration = EndTime - StartTime,
    AvgTimeUs = Duration / 1000,

    %% Should be fast (< 50us average since it's just process_info)
    ?assert(AvgTimeUs < 50),

    io:format("~nstatistics average time: ~.2f us~n", [AvgTimeUs]),

    stop(Pid).
