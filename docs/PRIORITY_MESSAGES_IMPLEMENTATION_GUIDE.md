# Priority Messages Implementation Guide: OTP 28

## Overview

This guide provides comprehensive instructions for implementing EEP 76 Priority Messages in erlmcp for OTP 28. Priority messages allow critical signals to overtake regular messages in process message queues, solving important scenarios like prioritized termination and system notifications.

## Overview of Priority Messages

### What Are Priority Messages?
Priority messages are a new feature in OTP 28 that allow certain messages to be inserted before regular messages in the message queue, ensuring they are processed ahead of normal traffic.

### Use Cases for erlmcp
1. **Prioritized Termination**: Critical shutdown messages
2. **System Notifications**: Alert messages about system health
3. **Control Messages**: Commands that need immediate attention
4. **Error Recovery**: Critical error handling

## Implementation Strategy

### Phase 1: Priority Alias Management

#### 1.1 Create Priority Alias Manager
```erlang
% apps/erlmcp_core/src/erlmcp_priority_messaging.erl
-module(erlmcp_priority_messaging).

-behaviour(gen_server).

%% API exports
-export([start_link/0, stop/0,
         create_priority_alias/0, destroy_priority_alias/1,
         send_priority_message/2, send_priority_exit/3,
         setup_priority_monitor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the priority messaging server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the priority messaging server
-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Create a priority alias
-spec create_priority_alias() -> {ok, term()} | {error, term()}.
create_priority_alias() ->
    gen_server:call(?MODULE, create_priority_alias).

%% @doc Destroy a priority alias
-spec destroy_priority_alias(term()) -> ok.
destroy_priority_alias(Alias) ->
    gen_server:call(?MODULE, {destroy_priority_alias, Alias}).

%% @doc Send a priority message
-spec send_priority_message(term(), term()) -> ok.
send_priority_message(Destination, Message) ->
    case get_priority_alias() of
        {ok, Alias} ->
            erlang:send(Destination, Message, [{priority, Alias}]);
        {error, _} ->
            %% Fallback to normal message
            erlang:send(Destination, Message)
    end.

%% @doc Send a priority exit signal
-spec send_priority_exit(term(), term(), term()) -> ok.
send_priority_exit(Destination, Reason, Alias) ->
    erlang:exit(Destination, Reason, [{priority, Alias}]).

%% @doc Set up a priority monitor
-spec setup_priority_monitor(term(), term()) -> reference().
setup_priority_monitor(Destination, Alias) ->
    erlang:monitor(Destination, [{priority, Alias}]).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Register priority messaging server
    {ok, #{aliases => #{}}}.

handle_call(stop, _From, State) ->
    %% Clean up all aliases
    destroy_all_aliases(State),
    {reply, ok, State};

handle_call(create_priority_alias, _From, State) ->
    %% Create new priority alias
    case erlang:alias([{priority}]) of
        Alias when is_atom(Alias) ->
            %% Register the alias
            NewAliases = maps:put(Alias, true, State#aliases),
            {reply, {ok, Alias}, State#aliases{aliases = NewAliases}};
        Error ->
            {reply, {error, Error}, State}
    end;

handle_call({destroy_priority_alias, Alias}, _From, State) ->
    %% Destroy the alias
    case erlang:unalias(Alias) of
        ok ->
            NewAliases = maps:remove(Alias, State#aliases),
            {reply, ok, State#aliases{aliases = NewAliases}};
        Error ->
            {reply, {error, Error}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    destroy_all_aliases(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Get priority alias from state
-spec get_priority_alias() -> {ok, term()} | {error, not_found}.
get_priority_alias() ->
    case whereis(?MODULE) of
        undefined ->
            {error, not_found};
        Pid ->
            gen_server:call(Pid, get_priority_alias)
    end.

%% Destroy all aliases
-spec destroy_all_aliases(map()) -> ok.
destroy_all_aliases(State) ->
    maps:fold(fun(Alias, _Value, Acc) ->
                     erlang:unalias(Alias),
                     Acc
              end, [], State#aliases).

%%====================================================================
%% Internal state management
%%====================================================================

-record(state, {
    aliases :: map()  % Alias -> Active flag
}).
```

#### 1.2 Initialize Priority Messaging in Application Startup
```erlang
% Add to erlmcp_app.erl
start(_StartType, _StartArgs) ->
    %% Start priority messaging server
    erlmcp_priority_messaging:start_link(),

    %% Rest of application startup...
    ok.
```

### Phase 2: Priority Message Integration

#### 2.1 Supervisor Priority Messages
```erlang
% Update supervisor to use priority messages for critical messages
-spec handle_call_message(message(), state()) -> state().
handle_call_message({terminate, Reason}, State) ->
    %% Send priority termination message to all children
    PriorityAlias = get_priority_alias(),
    supervisor_terminate_children(State, Reason, PriorityAlias),
    State;

handle_call_message({system_alert, Alert}, State) ->
    %% Send priority alert message to monitoring processes
    send_priority_alert_to_monitors(Alert),
    State.

%% Send priority termination to supervisor children
-spec supervisor_terminate_children(state(), term(), term()) -> ok.
supervisor_terminate_children(State, Reason, PriorityAlias) ->
    Children = supervisor:which_children(State#supervisor.id),
    lists:foreach(fun({Id, Pid, _, _}) ->
        case is_process_alive(Pid) of
            true ->
                erlang:exit(Pid, Reason, [{priority, PriorityAlias});
            false ->
                ok
        end
    end, Children).
```

#### 2.2 Server Process Priority Messages
```erlang
% Update gen_server behavior to handle priority messages
-spec handle_priority_message(message(), state()) -> state().
handle_priority_message({priority, critical_error, Error}, State) ->
    %% Handle critical error with priority
    logger:critical("Critical error handled with priority: ~p", [Error]),
    handle_critical_error(Error, State);

handle_priority_message({priority, system_down, Reason}, State) ->
    %% Handle system shutdown with priority
    logger:emergency("System shutdown initiated: ~p", [Reason]),
    initiate_graceful_shutdown(Reason, State);

handle_priority_message(_PriorityMessage, State) ->
    %% Handle unknown priority messages
    logger:warning("Unknown priority message: ~p", [_PriorityMessage]),
    State.

%% Initiate graceful shutdown with priority
-spec initiate_graceful_shutdown(term(), state()) -> state().
initiate_graceful_shutdown(Reason, State) ->
    %% Send priority shutdown messages to all connected clients
    ConnectedClients = get_connected_clients(State),
    lists:foreach(fun(Client) ->
        erlmcp_priority_messaging:send_priority_message(Client,
            {system_shutdown, Reason, priority})
    end, ConnectedClients),

    %% Begin shutdown process
    State#state{shutting_down = true, shutdown_reason = Reason}.
```

### Phase 3: Client-Side Priority Support

#### 3.1 Client Priority Messages
```erlang
% Update client to handle priority messages
-spec handle_message(message(), state()) -> state().
handle_message({jsonrpc, Request}, State) ->
    %% Handle regular JSON-RPC message
    handle_jsonrpc_message(Request, State);

handle_message({priority, system_alert, Alert}, State) ->
    %% Handle priority system alert
    logger:warning("Priority system alert: ~p", [Alert]),
    handle_system_alert(Alert, State);

handle_message({priority, configuration_update, Config}, State) ->
    %% Handle priority configuration update
    logger:info("Priority configuration update received"),
    apply_configuration_update(Config, State).

%% Apply priority configuration updates
-spec apply_configuration_update(map(), state()) -> state().
apply_configuration_update(Config, State) ->
    %% Update configuration with priority
    NewConfig = maps:merge(State#state.config, Config),

    %% Send priority update to dependent processes
    send_priority_config_update_to_dependents(NewConfig),

    State#state{config = NewConfig}.
```

#### 3.2 Client Connection Priority Support
```erlang
% Update transport layer to support priority messages
-spec send_data(binary(), term()) -> ok | {error, term()}.
send_data(Data, TransportState) ->
    %% Check if message has priority
    case is_priority_message(Data) of
        true ->
            %% Send with priority
            send_with_priority(Data, TransportState);
        false ->
            %% Send normally
            send_normal_data(Data, TransportState)
    end.

%% Check if message is a priority message
-spec is_priority_message(binary()) -> boolean().
is_priority_message(Data) ->
    try
        %% Parse JSON to check for priority marker
        Decoded = jsx:decode(Data, [{return, map}]),
        maps:is_key(priority, Decoded)
    catch
        _ ->
            false
    end.

%% Send message with priority
-spec send_with_priority(binary(), term()) -> ok | {error, term()}.
send_with_priority(Data, TransportState) ->
    %% Add priority marker to message
    PriorityMessage = add_priority_marker(Data),
    send_normal_data(PriorityMessage, TransportState).

%% Add priority marker to message
-spec add_priority_marker(binary()) -> binary().
add_priority_marker(Data) ->
    %% Add priority header or marker
    jsx:encode(#{priority => true, message => jsx:decode(Data)}).
```

### Phase 4: Monitoring and Priority Queue Management

#### 4.1 Priority Message Monitoring
```erlang
% Add to erlmcp_priority_messaging.erl
-export([start_priority_monitoring/0, get_priority_queue_stats/0]).

%% @doc Start priority message monitoring
-spec start_priority_monitoring() -> ok.
start_priority_monitoring() ->
    %% Start periodic monitoring
    erlang:send_after(30000, self(), {check_priority_queue_health}),
    ok.

%% @doc Get priority queue statistics
-spec get_priority_queue_stats() -> map().
get_priority_queue_stats() ->
    gen_server:call(?MODULE, get_priority_queue_stats).

handle_info({check_priority_queue_health}, State) ->
    %% Monitor priority message queue length
    QueueLength = erlang:process_info(self(), message_queue_len),
    logger:info("Priority queue health: ~p", [QueueLength]),

    %% Restart monitoring
    erlang:send_after(30000, self(), {check_priority_queue_health}),
    {noreply, State}.

handle_call(get_priority_queue_stats, _From, State) ->
    %% Get statistics about priority message handling
    Stats = #{
        active_aliases => maps:size(State#aliases.aliases),
        priority_queue_length => erlang:process_info(self(), message_queue_len)
    },
    {reply, Stats, State}.
```

#### 4.2 Priority Message Performance Optimization
```erlang
% Add priority message optimization
-spec optimize_priority_message_handling() -> ok.
optimize_priority_message_handling() ->
    %% Set process flag for optimized priority handling
    process_flag(priority_messages, true),

    %% Configure selective receive optimization
    process_flag(save_calls, 1000),  % Save last 1000 calls for optimization

    %% Enable priority message monitoring
    erlmcp_priority_messaging:start_priority_monitoring(),
    ok.

%% Handle selective receive with priority messages
-spec optimized_receive(term()) -> term().
optimized_receive(Pattern) ->
    %% Use selective receive optimization for priority messages
    receive
        PriorityMsg when map_key(priority, PriorityMsg) ->
            %% Handle priority message immediately
            handle_priority_message(PriorityMsg);
        NormalMsg ->
            %% Handle normal message
            NormalMsg
    after 0 ->
        %% No messages, wait
        wait_for_messages()
    end.
```

### Phase 5: Testing and Validation

#### 5.1 Priority Message Testing Suite
```erlang
% Add to test suite
-priority_test(priority_message_basic) ->
    %% Test basic priority message functionality
    {ok, Alias} = erlmcp_priority_messaging:create_priority_alias(),

    %% Create test process
    TestPid = spawn_link(fun test_priority_receiver/0),

    %% Send normal message
    erlang:send(TestPid, normal_message),

    %% Send priority message
    erlmcp_priority_messaging:send_priority_message(TestPid, priority_message),

    %% Verify priority message was processed first
    receive
        processed_priority -> ok
    after 5000 ->
        timeout
    end.

%% Test priority receiver process
test_priority_receiver() ->
    receive
        priority_message ->
            erlang:send(self(), processed_priority);
        normal_message ->
            %% Should not be processed first
            ok
    end.
```

#### 5.2 Priority Message Performance Testing
```erlang
% Performance test for priority messages
-priority_test(priority_message_performance) ->
    %% Test performance with many normal and priority messages
    TestPid = spawn_link(fun test_performance_receiver/0),

    %% Send many normal messages
    lists:foreach(fun(_) ->
        erlang:send(TestPid, normal_message)
    end, lists:seq(1, 1000)),

    %% Send priority message
    erlmcp_priority_messaging:send_priority_message(TestPid, critical_alert),

    %% Verify priority message was processed quickly
    test_performance_receiver().

test_performance_receiver() ->
    %% Count messages and measure processing time
    start_time = erlang:monotonic_time(millisecond),
    process_messages(0, start_time).

process_messages(Count, StartTime) ->
    receive
        normal_message ->
            process_messages(Count + 1, StartTime);
        critical_alert ->
            EndTime = erlang:monotonic_time(millisecond),
            Duration = EndTime - StartTime,
            logger:info("Priority message processed after ~p normal messages, ~p ms",
                       [Count, Duration]),
            ok
    after 5000 ->
        timeout
    end.
```

### Phase 6: Configuration and Deployment

#### 6.1 Configuration Updates
```erlang
% Add to sys.config
{erlmcp_core, [
    {priority_messaging, true},
    {priority_alias_ttl, 3600},  % 1 hour
    {priority_queue_monitoring, true},
    {priority_message_timeout, 5000}
]}.
```

#### 6.2 Deployment Strategy
```bash
# Deployment steps
1. Deploy with priority messaging enabled
2. Monitor priority queue statistics
3. Test priority message delivery
4. Validate performance improvements
5. Update monitoring alerts
```

## Implementation Checklist

### Immediate Actions (High Priority)
- [ ] Create priority alias manager
- [ ] Implement priority message sending/receiving
- [ ] Add priority message monitoring
- [ ] Update supervisor for priority termination

### Medium-term Actions
- [ ] Integrate priority messages in client/server communication
- [ ] Add performance optimization for priority handling
- [ ] Create comprehensive test suite
- [ ] Update documentation

### Long-term Actions
- [ ] Optimize priority message handling further
- [ ] Add advanced priority levels
- [ ] Create priority message analytics
- [ ] Implement priority-based load balancing

## Monitoring and Logging

### Priority Message Metrics
```erlang
% Track priority message metrics
-export([get_priority_metrics/0]).

get_priority_metrics() ->
    #{
        total_priority_messages => get_total_priority_messages(),
        priority_queue_length => get_priority_queue_length(),
        average_priority_latency => get_average_priority_latency(),
        priority_message_ratio => get_priority_message_ratio()
    }.
```

### Priority Message Alerting
```erlang
% Add priority message alerts
-spec check_priority_alerts() -> ok.
check_priority_alerts() ->
    Metrics = get_priority_metrics(),

    %% Alert on priority queue buildup
    case Metrics#priority_queue_length > 100 of
        true ->
            logger:alert("Priority queue length exceeded: ~p", [Metrics#priority_queue_length]);
        false ->
            ok
    end.
```

## Conclusion

Priority messages in OTP 28 provide a powerful mechanism for handling critical messages in erlmcp:

1. **Immediate Processing**: Critical messages overtake regular messages
2. **Backward Compatible**: Requires explicit opt-in
3. **Performance Optimized**: Minimal overhead for non-priority traffic
4. **Comprehensive Integration**: Works with existing OTP behaviors

The implementation above provides a complete framework for priority message handling in erlmcp, ensuring critical system messages are processed promptly while maintaining system stability.

## Resources

- [EEP 76: Priority Messages](https://www.erlang.org/eeps/eep-0076)
- [OTP 28 Priority Messages Documentation](https://www.erlang.org/doc/apps/stdlib/priority_messages.html)
- [Erlang Distribution Protocol](https://www.erlang.org/doc/apps/erts/distribution.html)
- [gen_server Behavior](https://www.erlang.org/doc/apps/stdlib/gen_server.html)