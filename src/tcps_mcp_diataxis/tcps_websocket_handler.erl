%%%-------------------------------------------------------------------
%%% @doc TCPS WebSocket Handler for Diataxis Simulator
%%%
%%% Handles bidirectional WebSocket communication for real-time
%%% simulator updates, interactive commands, and event streaming.
%%%
%%% Supported Commands:
%%% - subscribe: Subscribe to event channels
%%% - trigger_andon: Manually trigger Andon alert
%%% - create_work_item: Add new work item to Kanban
%%% - move_work_item: Move work item between buckets
%%% - execute_mcp_tool: Execute MCP tool from playground
%%% - get_metrics: Request current metrics snapshot
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_websocket_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    client_id :: binary(),
    subscriptions = [] :: [atom()],
    session_data = #{} :: map(),
    connected_at :: erlang:timestamp()
}).

%%%===================================================================
%%% Cowboy WebSocket Callbacks
%%%===================================================================

init(Req, _Opts) ->
    %% Upgrade to WebSocket protocol
    {cowboy_websocket, Req, #state{
        client_id = generate_client_id(),
        connected_at = erlang:timestamp()
    }, #{
        idle_timeout => 300000,  %% 5 minutes
        compress => true
    }}.

websocket_init(State) ->
    ClientId = State#state.client_id,
    ?LOG_INFO("WebSocket client connected: ~s", [ClientId]),

    %% Register connection with web server
    register_connection(self(), #{
        client_id => ClientId,
        connected_at => State#state.connected_at
    }),

    %% Store in ETS for broadcast
    ets:insert(tcps_ws_connections, {self(), #{client_id => ClientId}}),

    %% Send welcome message with initial state
    WelcomeMsg = #{
        type => <<"connected">>,
        client_id => ClientId,
        server_version => <<"1.0.0">>,
        timestamp => erlang:system_time(millisecond),
        simulator_status => get_simulator_status()
    },

    {[{text, jsx:encode(WelcomeMsg)}], State}.

websocket_handle({text, Msg}, State) ->
    %% Parse JSON command from client
    try
        Command = jsx:decode(Msg, [return_maps]),
        ?LOG_DEBUG("WebSocket command from ~s: ~p", [State#state.client_id, Command]),

        case handle_command(Command, State) of
            {reply, Response, NewState} ->
                {[{text, jsx:encode(Response)}], NewState};
            {noreply, NewState} ->
                {ok, NewState};
            {error, Reason} ->
                ErrorResp = create_error_response(Reason),
                {[{text, jsx:encode(ErrorResp)}], State}
        end
    catch
        _:Error:Stacktrace ->
            ?LOG_ERROR("WebSocket message error: ~p~nStacktrace: ~p", [Error, Stacktrace]),
            ErrorResp2 = create_error_response(<<"Invalid JSON message">>),
            {[{text, jsx:encode(ErrorResp2)}], State}
    end;

websocket_handle({binary, _Data}, State) ->
    ErrorResp = create_error_response(<<"Binary messages not supported">>),
    {[{text, jsx:encode(ErrorResp)}], State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({ws_send, Message}, State) ->
    %% Send message from server to client
    {[{text, Message}], State};

websocket_info({broadcast, Event}, State) ->
    %% Forward broadcast event to client
    {[{text, jsx:encode(Event)}], State};

websocket_info({simulator_update, Update}, State) ->
    %% Send simulator state update
    Msg = #{
        type => <<"simulator_update">>,
        data => Update,
        timestamp => erlang:system_time(millisecond)
    },
    {[{text, jsx:encode(Msg)}], State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, State) ->
    ClientId = State#state.client_id,
    ?LOG_INFO("WebSocket client ~s disconnected: ~p", [ClientId, Reason]),

    %% Clean up
    ets:delete(tcps_ws_connections, self()),
    unregister_connection(self()),

    ok.

%%%===================================================================
%%% Command Handlers
%%%===================================================================

%% @private Handle WebSocket command
handle_command(#{<<"type">> := <<"ping">>}, State) ->
    Response = #{
        type => <<"pong">>,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State};

handle_command(#{<<"type">> := <<"subscribe">>, <<"channels">> := Channels}, State) ->
    NewSubs = State#state.subscriptions ++ [erlang:binary_to_atom(C, utf8) || C <- Channels],
    ?LOG_DEBUG("Client ~s subscribed to: ~p", [State#state.client_id, Channels]),
    Response = #{
        type => <<"subscribed">>,
        channels => Channels,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State#state{subscriptions = NewSubs}};

handle_command(#{<<"type">> := <<"unsubscribe">>, <<"channels">> := Channels}, State) ->
    RemoveChannels = [erlang:binary_to_atom(C, utf8) || C <- Channels],
    NewSubs = State#state.subscriptions -- RemoveChannels,
    Response = #{
        type => <<"unsubscribed">>,
        channels => Channels,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State#state{subscriptions = NewSubs}};

handle_command(#{<<"type">> := <<"get_metrics">>}, State) ->
    Metrics = get_current_metrics(),
    Response = #{
        type => <<"metrics">>,
        data => Metrics,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State};

handle_command(#{<<"type">> := <<"get_simulator_status">>}, State) ->
    Status = get_simulator_status(),
    Response = #{
        type => <<"simulator_status">>,
        data => Status,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State};

handle_command(#{<<"type">> := <<"trigger_andon">>, <<"data">> := Data}, State) ->
    Result = handle_trigger_andon(Data),
    Response = #{
        type => <<"andon_triggered">>,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    %% Broadcast event to all clients
    broadcast_event(andon_triggered, Result),
    {reply, Response, State};

handle_command(#{<<"type">> := <<"create_work_item">>, <<"data">> := Data}, State) ->
    Result = handle_create_work_item(Data),
    Response = #{
        type => <<"work_item_created">>,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    broadcast_event(work_item_created, Result),
    {reply, Response, State};

handle_command(#{<<"type">> := <<"move_work_item">>, <<"data">> := Data}, State) ->
    Result = handle_move_work_item(Data),
    Response = #{
        type => <<"work_item_moved">>,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    broadcast_event(work_item_moved, Result),
    {reply, Response, State};

handle_command(#{<<"type">> := <<"execute_mcp_tool">>, <<"data">> := Data}, State) ->
    Result = handle_execute_mcp_tool(Data),
    Response = #{
        type => <<"mcp_tool_executed">>,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State};

handle_command(#{<<"type">> := <<"start_tutorial">>, <<"data">> := Data}, State) ->
    TutorialId = maps:get(<<"tutorial_id">>, Data, <<"basic">>),
    Tutorial = start_tutorial(TutorialId),
    Response = #{
        type => <<"tutorial_started">>,
        tutorial => Tutorial,
        timestamp => erlang:system_time(millisecond)
    },
    NewSessionData = maps:put(current_tutorial, Tutorial, State#state.session_data),
    {reply, Response, State#state{session_data = NewSessionData}};

handle_command(#{<<"type">> := <<"complete_tutorial_step">>, <<"data">> := Data}, State) ->
    StepId = maps:get(<<"step_id">>, Data),
    Result = complete_tutorial_step(StepId, State#state.session_data),
    Response = #{
        type => <<"tutorial_step_completed">>,
        result => Result,
        timestamp => erlang:system_time(millisecond)
    },
    {reply, Response, State};

handle_command(#{<<"type">> := Type}, _State) ->
    {error, iolist_to_binary(io_lib:format("Unknown command type: ~s", [Type]))};

handle_command(_Command, _State) ->
    {error, <<"Invalid command format">>}.

%%%===================================================================
%%% Command Implementation
%%%===================================================================

%% @private Trigger Andon alert
handle_trigger_andon(Data) ->
    Severity = maps:get(<<"severity">>, Data, <<"warning">>),
    Title = maps:get(<<"title">>, Data, <<"Manual Andon">>),
    Description = maps:get(<<"description">>, Data, <<"">>),
    AffectedItems = maps:get(<<"affected_items">>, Data, []),

    AndonId = generate_andon_id(),

    AndonEvent = #{
        id => AndonId,
        severity => Severity,
        title => Title,
        description => Description,
        affected_items => AffectedItems,
        triggered_at => erlang:system_time(millisecond),
        status => <<"active">>
    },

    %% Store in mock Andon system (would integrate with real tcps_andon)
    store_andon_event(AndonEvent),

    #{
        success => true,
        andon_id => AndonId,
        event => AndonEvent
    }.

%% @private Create work item
handle_create_work_item(Data) ->
    Title = maps:get(<<"title">>, Data, <<"New Work Item">>),
    Description = maps:get(<<"description">>, Data, <<"">>),
    Priority = maps:get(<<"priority">>, Data, <<"medium">>),
    Bucket = maps:get(<<"bucket">>, Data, <<"backlog">>),

    WorkItemId = generate_work_item_id(),

    WorkItem = #{
        id => WorkItemId,
        title => Title,
        description => Description,
        priority => Priority,
        bucket => Bucket,
        created_at => erlang:system_time(millisecond),
        updated_at => erlang:system_time(millisecond)
    },

    %% Store work item (would integrate with real tcps_kanban)
    store_work_item(WorkItem),

    #{
        success => true,
        work_item_id => WorkItemId,
        work_item => WorkItem
    }.

%% @private Move work item between buckets
handle_move_work_item(Data) ->
    ItemId = maps:get(<<"item_id">>, Data),
    ToBucket = maps:get(<<"to_bucket">>, Data),

    %% Get existing work item
    case get_work_item(ItemId) of
        {ok, WorkItem} ->
            UpdatedItem = WorkItem#{
                bucket => ToBucket,
                updated_at => erlang:system_time(millisecond)
            },
            store_work_item(UpdatedItem),
            #{
                success => true,
                work_item => UpdatedItem
            };
        {error, not_found} ->
            #{
                success => false,
                error => <<"Work item not found">>
            }
    end.

%% @private Execute MCP tool
handle_execute_mcp_tool(Data) ->
    ToolName = maps:get(<<"tool">>, Data),
    Params = maps:get(<<"params">>, Data, #{}),

    %% Mock MCP tool execution (would integrate with real MCP system)
    Result = #{
        tool => ToolName,
        params => Params,
        output => <<"Tool executed successfully (mock)">>,
        execution_time_ms => 42,
        status => <<"success">>
    },

    #{
        success => true,
        result => Result
    }.

%% @private Start tutorial
start_tutorial(TutorialId) ->
    Tutorials = #{
        <<"basic">> => #{
            id => <<"basic">>,
            title => <<"TCPS Basics Tutorial">>,
            description => <<"Learn the fundamentals of TCPS workflow">>,
            steps => [
                #{id => <<"step1">>, title => <<"Create a Work Item">>, completed => false},
                #{id => <<"step2">>, title => <<"Move Item Through Kanban">>, completed => false},
                #{id => <<"step3">>, title => <<"Trigger Quality Gate">>, completed => false},
                #{id => <<"step4">>, title => <<"Handle Andon Alert">>, completed => false}
            ]
        },
        <<"advanced">> => #{
            id => <<"advanced">>,
            title => <<"Advanced TCPS Features">>,
            description => <<"Explore advanced TCPS capabilities">>,
            steps => [
                #{id => <<"adv1">>, title => <<"Configure Quality Gates">>, completed => false},
                #{id => <<"adv2">>, title => <<"MCP Tool Integration">>, completed => false},
                #{id => <<"adv3">>, title => <<"Custom Metrics">>, completed => false}
            ]
        }
    },
    maps:get(TutorialId, Tutorials, maps:get(<<"basic">>, Tutorials)).

%% @private Complete tutorial step
complete_tutorial_step(StepId, SessionData) ->
    case maps:get(current_tutorial, SessionData, undefined) of
        undefined ->
            #{success => false, error => <<"No active tutorial">>};
        Tutorial ->
            Steps = maps:get(steps, Tutorial),
            UpdatedSteps = lists:map(fun(Step) ->
                case maps:get(id, Step) of
                    StepId -> Step#{completed => true};
                    _ -> Step
                end
            end, Steps),
            #{
                success => true,
                updated_steps => UpdatedSteps
            }
    end.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @private Get simulator status
get_simulator_status() ->
    #{
        status => <<"running">>,
        uptime_seconds => get_uptime(),
        ws_connections => ets:info(tcps_ws_connections, size),
        work_items_count => count_work_items(),
        active_andons => count_active_andons()
    }.

%% @private Get current metrics
get_current_metrics() ->
    case whereis(tcps_metrics_aggregator) of
        undefined ->
            get_mock_metrics();
        _Pid ->
            try
                tcps_metrics_aggregator:get_all_metrics()
            catch
                _:_ -> get_mock_metrics()
            end
    end.

%% @private Get mock metrics
get_mock_metrics() ->
    #{
        kanban => #{
            backlog => #{count => 5, limit => 10},
            ready => #{count => 3, limit => 5},
            in_progress => #{count => 2, limit => 3},
            review => #{count => 1, limit => 2},
            done => #{count => 15, limit => 999}
        },
        quality_gates => #{
            pass_rate => 0.92,
            coverage => 0.85,
            defect_rate => 0.02
        },
        andons => []
    }.

%% @private Generate client ID
generate_client_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("client-~b-~6..0b", [Timestamp, Random])).

%% @private Generate Andon ID
generate_andon_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("andon-~b", [Timestamp])).

%% @private Generate work item ID
generate_work_item_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("work-~b", [Timestamp])).

%% @private Create error response
create_error_response(Reason) ->
    #{
        type => <<"error">>,
        error => Reason,
        timestamp => erlang:system_time(millisecond)
    }.

%% @private Register WebSocket connection
register_connection(Pid, Metadata) ->
    case whereis(tcps_web_server) of
        undefined -> ok;
        ServerPid ->
            gen_server:cast(ServerPid, {register_ws_connection, Pid, Metadata})
    end.

%% @private Unregister WebSocket connection
unregister_connection(Pid) ->
    case whereis(tcps_web_server) of
        undefined -> ok;
        ServerPid ->
            gen_server:cast(ServerPid, {unregister_ws_connection, Pid})
    end.

%% @private Broadcast event to all clients
broadcast_event(Type, Data) ->
    Event = #{
        type => Type,
        data => Data,
        timestamp => erlang:system_time(millisecond)
    },
    case ets:tab2list(tcps_ws_connections) of
        [] -> ok;
        Connections ->
            lists:foreach(fun({Pid, _Meta}) ->
                Pid ! {broadcast, Event}
            end, Connections)
    end.

%% @private Store Andon event (mock)
store_andon_event(Event) ->
    %% Would integrate with real tcps_andon system
    case ets:info(tcps_simulator_andons) of
        undefined ->
            ets:new(tcps_simulator_andons, [set, public, named_table]);
        _ ->
            ok
    end,
    ets:insert(tcps_simulator_andons, {maps:get(id, Event), Event}).

%% @private Store work item (mock)
store_work_item(WorkItem) ->
    %% Would integrate with real tcps_kanban system
    case ets:info(tcps_simulator_work_items) of
        undefined ->
            ets:new(tcps_simulator_work_items, [set, public, named_table]);
        _ ->
            ok
    end,
    ets:insert(tcps_simulator_work_items, {maps:get(id, WorkItem), WorkItem}).

%% @private Get work item
get_work_item(ItemId) ->
    case ets:info(tcps_simulator_work_items) of
        undefined -> {error, not_found};
        _ ->
            case ets:lookup(tcps_simulator_work_items, ItemId) of
                [{_, WorkItem}] -> {ok, WorkItem};
                [] -> {error, not_found}
            end
    end.

%% @private Get uptime
get_uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.

%% @private Count work items
count_work_items() ->
    case ets:info(tcps_simulator_work_items) of
        undefined -> 0;
        _ -> ets:info(tcps_simulator_work_items, size)
    end.

%% @private Count active Andons
count_active_andons() ->
    case ets:info(tcps_simulator_andons) of
        undefined -> 0;
        _ ->
            All = ets:tab2list(tcps_simulator_andons),
            length([1 || {_, Event} <- All, maps:get(status, Event, undefined) =:= <<"active">>])
    end.
