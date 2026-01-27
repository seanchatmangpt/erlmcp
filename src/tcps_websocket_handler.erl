%%%-------------------------------------------------------------------
%%% @doc TCPS WebSocket Handler - Bidirectional real-time communication
%%%
%%% Provides WebSocket support for real-time dashboard updates and
%%% interactive commands (trigger Andon, create work orders, etc.).
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_websocket_handler).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    client_id :: binary(),
    subscriptions = [] :: [atom()],
    authenticated = false :: boolean()
}).

%%%===================================================================
%%% Cowboy WebSocket Callbacks
%%%===================================================================

init(Req, _Opts) ->
    %% Upgrade to WebSocket
    {cowboy_websocket, Req, #state{
        client_id = generate_client_id(),
        authenticated = true  %% TODO: Add proper authentication
    }}.

websocket_init(State) ->
    ClientId = State#state.client_id,
    ?LOG_INFO("WebSocket client connected: ~s", [ClientId]),

    %% Register with SSE manager for metrics updates
    tcps_sse_manager:register_client(self()),

    %% Send welcome message
    WelcomeMsg = #{
        type => <<"connected">>,
        client_id => ClientId,
        server_version => <<"1.0.0">>,
        timestamp => erlang:system_time(millisecond)
    },

    {[{text, jsx:encode(WelcomeMsg)}], State}.

websocket_handle({text, Msg}, State) ->
    %% Parse JSON message from client
    try
        Command = jsx:decode(Msg, [return_maps]),
        ?LOG_DEBUG("Received WebSocket command: ~p", [Command]),

        Response = handle_command(Command, State),
        {[{text, jsx:encode(Response)}], State}
    catch
        _:Error ->
            ErrorMsg = #{
                type => <<"error">>,
                message => iolist_to_binary(io_lib:format("Invalid message: ~p", [Error])),
                timestamp => erlang:system_time(millisecond)
            },
            {[{text, jsx:encode(ErrorMsg)}], State}
    end;

websocket_handle({binary, _Data}, State) ->
    %% Binary messages not supported
    ErrorMsg = #{
        type => <<"error">>,
        message => <<"Binary messages not supported">>,
        timestamp => erlang:system_time(millisecond)
    },
    {[{text, jsx:encode(ErrorMsg)}], State};

websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({sse_update, Update}, State) ->
    %% Forward SSE update to WebSocket client
    Msg = jsx:encode(Update),
    {[{text, Msg}], State};

websocket_info({dashboard_event, Event}, State) ->
    %% Forward dashboard event
    Msg = jsx:encode(Event),
    {[{text, Msg}], State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, State) ->
    ClientId = State#state.client_id,
    ?LOG_INFO("WebSocket client ~s disconnected: ~p", [ClientId, Reason]),

    %% Unregister from SSE manager
    tcps_sse_manager:unregister_client(self()),

    ok.

%%%===================================================================
%%% Command Handlers
%%%===================================================================

%% @private Handle WebSocket command
handle_command(#{<<"type">> := <<"ping">>}, _State) ->
    #{
        type => <<"pong">>,
        timestamp => erlang:system_time(millisecond)
    };

handle_command(#{<<"type">> := <<"subscribe">>, <<"channels">> := Channels}, _State) ->
    %% Subscribe to specific event channels
    ?LOG_INFO("Client subscribed to channels: ~p", [Channels]),
    #{
        type => <<"subscribed">>,
        channels => Channels,
        timestamp => erlang:system_time(millisecond)
    };

handle_command(#{<<"type">> := <<"get_metrics">>}, _State) ->
    %% Get current metrics
    Metrics = tcps_metrics_aggregator:get_all_metrics(),
    #{
        type => <<"metrics">>,
        data => Metrics,
        timestamp => erlang:system_time(millisecond)
    };

handle_command(#{<<"type">> := <<"trigger_andon">>, <<"data">> := Data}, State) ->
    %% Trigger Andon event
    case State#state.authenticated of
        true ->
            Result = handle_trigger_andon(Data),
            #{
                type => <<"andon_triggered">>,
                result => Result,
                timestamp => erlang:system_time(millisecond)
            };
        false ->
            #{
                type => <<"error">>,
                message => <<"Authentication required">>,
                timestamp => erlang:system_time(millisecond)
            }
    end;

handle_command(#{<<"type">> := <<"create_work_order">>, <<"data">> := Data}, State) ->
    %% Create work order
    case State#state.authenticated of
        true ->
            Result = handle_create_work_order(Data),
            #{
                type => <<"work_order_created">>,
                result => Result,
                timestamp => erlang:system_time(millisecond)
            };
        false ->
            #{
                type => <<"error">>,
                message => <<"Authentication required">>,
                timestamp => erlang:system_time(millisecond)
            }
    end;

handle_command(#{<<"type">> := <<"resolve_andon">>, <<"andon_id">> := AndonId, <<"data">> := Data}, State) ->
    %% Resolve Andon
    case State#state.authenticated of
        true ->
            Result = handle_resolve_andon(AndonId, Data),
            #{
                type => <<"andon_resolved">>,
                result => Result,
                timestamp => erlang:system_time(millisecond)
            };
        false ->
            #{
                type => <<"error">>,
                message => <<"Authentication required">>,
                timestamp => erlang:system_time(millisecond)
            }
    end;

handle_command(#{<<"type">> := <<"get_work_orders">>}, _State) ->
    %% Get work orders
    WorkOrders = get_work_orders_list(),
    #{
        type => <<"work_orders">>,
        data => WorkOrders,
        timestamp => erlang:system_time(millisecond)
    };

handle_command(#{<<"type">> := <<"get_andons">>}, _State) ->
    %% Get Andon alerts
    Andons = get_andons_list(),
    #{
        type => <<"andons">>,
        data => Andons,
        timestamp => erlang:system_time(millisecond)
    };

handle_command(#{<<"type">> := Type}, _State) ->
    #{
        type => <<"error">>,
        message => iolist_to_binary(io_lib:format("Unknown command type: ~s", [Type])),
        timestamp => erlang:system_time(millisecond)
    };

handle_command(_Command, _State) ->
    #{
        type => <<"error">>,
        message => <<"Invalid command format">>,
        timestamp => erlang:system_time(millisecond)
    }.

%%%===================================================================
%%% Command Implementation
%%%===================================================================

%% @private Trigger Andon event
handle_trigger_andon(Data) ->
    FailureType = maps:get(<<"failure_type">>, Data, <<"unknown">>),
    SkuId = maps:get(<<"sku_id">>, Data, <<"manual-trigger">>),
    Stage = maps:get(<<"stage">>, Data, <<"execution">>),
    Details = maps:get(<<"details">>, Data, #{}),

    Context = #{
        sku_id => SkuId,
        stage => erlang:binary_to_atom(Stage, utf8),
        details => Details,
        metadata => #{triggered_via => websocket}
    },

    case whereis(tcps_andon) of
        undefined ->
            #{success => false, error => <<"Andon system not available">>};
        _Pid ->
            FailureTypeAtom = erlang:binary_to_atom(FailureType, utf8),
            case tcps_andon:trigger_andon(FailureTypeAtom, Context) of
                {ok, AndonId} ->
                    #{success => true, andon_id => AndonId};
                {error, Reason} ->
                    #{success => false, error => format_error(Reason)}
            end
    end.

%% @private Create work order
handle_create_work_order(Data) ->
    Description = maps:get(<<"description">>, Data, <<"Manual work order">>),
    Priority = maps:get(<<"priority">>, Data, <<"medium">>),
    Bucket = maps:get(<<"bucket">>, Data, <<"features">>),

    PullSignal = #{
        type => internal_request,
        source => <<"websocket">>,
        description => Description,
        labels => [Priority],
        metadata => #{
            bucket => erlang:binary_to_atom(Bucket, utf8),
            created_via => websocket
        }
    },

    case whereis(tcps_work_order) of
        undefined ->
            #{success => false, error => <<"Work order system not available">>};
        _Pid ->
            case tcps_work_order:create_work_order(PullSignal) of
                {ok, WorkOrderId} ->
                    #{success => true, work_order_id => WorkOrderId};
                {error, Reason} ->
                    #{success => false, error => format_error(Reason)}
            end
    end.

%% @private Resolve Andon event
handle_resolve_andon(AndonId, Data) ->
    RootCause = maps:get(<<"root_cause">>, Data, <<"Unknown">>),
    FixApplied = maps:get(<<"fix_applied">>, Data, <<"Unknown">>),
    PreventionAdded = maps:get(<<"prevention_added">>, Data, <<"Unknown">>),
    Resolver = maps:get(<<"resolver">>, Data, <<"websocket-user">>),

    Resolution = #{
        root_cause => RootCause,
        fix_applied => FixApplied,
        prevention_added => PreventionAdded,
        resolver => Resolver,
        resolution_time_minutes => 0
    },

    case whereis(tcps_andon) of
        undefined ->
            #{success => false, error => <<"Andon system not available">>};
        _Pid ->
            case tcps_andon:resolve_andon(AndonId, Resolution) of
                ok ->
                    #{success => true, andon_id => AndonId};
                {error, Reason} ->
                    #{success => false, error => format_error(Reason)}
            end
    end.

%% @private Get work orders list
get_work_orders_list() ->
    case ets:info(tcps_work_orders) of
        undefined -> [];
        _ ->
            AllWOs = ets:tab2list(tcps_work_orders),
            lists:map(fun({Id, WO}) ->
                format_work_order(Id, WO)
            end, lists:sublist(AllWOs, 20))  %% Limit to 20 most recent
    end.

%% @private Get Andons list
get_andons_list() ->
    case ets:info(tcps_andon_events) of
        undefined -> [];
        _ ->
            AllEvents = ets:tab2list(tcps_andon_events),
            OpenEvents = lists:filter(fun({_Id, Event}) ->
                is_map(Event) andalso maps:get(status, Event, undefined) =:= open
            end, AllEvents),

            lists:map(fun({Id, Event}) ->
                format_andon_event(Id, Event)
            end, OpenEvents)
    end.

%%%===================================================================
%%% Formatting Helpers
%%%===================================================================

%% @private Format work order for JSON
format_work_order(Id, WorkOrder) when is_map(WorkOrder) ->
    #{
        id => Id,
        bucket => maps:get(bucket, WorkOrder, unknown),
        priority => maps:get(priority, WorkOrder, 0),
        status => maps:get(status, WorkOrder, unknown),
        description => maps:get(description, WorkOrder, <<"">>),
        created_at => format_timestamp(maps:get(created_at, WorkOrder, undefined))
    };
format_work_order(Id, _) ->
    #{id => Id, error => <<"Invalid work order">>}.

%% @private Format Andon event for JSON
format_andon_event(Id, Event) when is_map(Event) ->
    #{
        id => Id,
        failure_type => maps:get(failure_type, Event, unknown),
        sku_id => maps:get(sku_id, Event, <<"unknown">>),
        stage => maps:get(stage, Event, unknown),
        status => maps:get(status, Event, unknown),
        timestamp => maps:get(timestamp, Event, 0),
        details => maps:get(details, Event, #{})
    };
format_andon_event(Id, _) ->
    #{id => Id, error => <<"Invalid andon event">>}.

%% @private Format timestamp
format_timestamp(undefined) -> null;
format_timestamp({MegaSecs, Secs, MicroSecs}) ->
    TotalSecs = MegaSecs * 1000000 + Secs,
    Milliseconds = MicroSecs div 1000,
    TotalSecs * 1000 + Milliseconds;
format_timestamp(_) -> null.

%% @private Format error for JSON
format_error(Error) when is_atom(Error) ->
    atom_to_binary(Error, utf8);
format_error(Error) when is_binary(Error) ->
    Error;
format_error(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% @private Generate unique client ID
generate_client_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("ws-client-~b-~6..0b", [Timestamp, Random])).

%% @private Safe binary to atom conversion
binary_to_atom(Binary, Encoding) ->
    try
        erlang:binary_to_existing_atom(Binary, Encoding)
    catch
        _:_ ->
            %% Atom doesn't exist, create it (be careful with this)
            erlang:binary_to_atom(Binary, Encoding)
    end.
