%%%-------------------------------------------------------------------
%%% @doc TCPS Dashboard HTTP Request Handler
%%%
%%% Cowboy HTTP handler for REST API endpoints.
%%% Handles all TCPS dashboard API requests.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_dashboard_handler).

-export([init/2, terminate/3]).
-export([allowed_methods/2, content_types_provided/2, content_types_accepted/2]).
-export([to_json/2, to_html/2, from_json/2]).

-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Cowboy REST Handler Callbacks
%%%===================================================================

-spec init(cowboy_req:req(), term()) -> {cowboy_rest, cowboy_req:req(), map()}.
init(Req, Opts) ->
    Endpoint = maps:get(endpoint, Opts, unknown),
    {cowboy_rest, Req, #{endpoint => Endpoint}}.

-spec allowed_methods(cowboy_req:req(), map()) -> {[binary()], cowboy_req:req(), map()}.
allowed_methods(Req, State = #{endpoint := work_orders}) ->
    {[<<"GET">>, <<"POST">>], Req, State};
allowed_methods(Req, State = #{endpoint := andon_resolve}) ->
    {[<<"POST">>], Req, State};
allowed_methods(Req, State) ->
    {[<<"GET">>], Req, State}.

-spec content_types_provided(cowboy_req:req(), map()) -> {list(), cowboy_req:req(), map()}.
content_types_provided(Req, State) ->
    {[
        {<<"application/json">>, to_json},
        {<<"text/html">>, to_html}
    ], Req, State}.

-spec content_types_accepted(cowboy_req:req(), map()) -> {list(), cowboy_req:req(), map()}.
content_types_accepted(Req, State) ->
    {[
        {<<"application/json">>, from_json}
    ], Req, State}.

%%%===================================================================
%%% Response Handlers
%%%===================================================================

to_json(Req, State = #{endpoint := summary}) ->
    Summary = tcps_dashboard:get_metrics_summary(),
    Body = jsx:encode(Summary),
    {Body, Req, State};

to_json(Req, State = #{endpoint := overview}) ->
    %% Use metrics aggregator for real-time overview
    Overview = tcps_metrics_aggregator:get_overview_metrics(),
    Body = jsx:encode(Overview),
    {Body, Req, State};

to_json(Req, State = #{endpoint := kanban}) ->
    %% Use metrics aggregator for real Kanban data
    Kanban = tcps_metrics_aggregator:get_kanban_metrics(),
    Body = jsx:encode(Kanban),
    {Body, Req, State};

to_json(Req, State = #{endpoint := quality}) ->
    %% Use metrics aggregator for real quality data
    Quality = tcps_metrics_aggregator:get_quality_metrics(),
    Body = jsx:encode(Quality),
    {Body, Req, State};

to_json(Req, State = #{endpoint := andon}) ->
    %% Use metrics aggregator for real Andon data
    Andons = tcps_metrics_aggregator:get_andon_metrics(),
    Body = jsx:encode(Andons),
    {Body, Req, State};

to_json(Req, State = #{endpoint := kaizen}) ->
    %% Use metrics aggregator for real Kaizen data
    Kaizen = tcps_metrics_aggregator:get_kaizen_metrics(),
    Body = jsx:encode(Kaizen),
    {Body, Req, State};

to_json(Req, State = #{endpoint := flow}) ->
    %% Use metrics aggregator for flow metrics
    Flow = tcps_metrics_aggregator:get_flow_metrics(),
    Body = jsx:encode(Flow),
    {Body, Req, State};

to_json(Req, State = #{endpoint := all_metrics}) ->
    %% Get all metrics at once (efficient)
    AllMetrics = tcps_metrics_aggregator:get_all_metrics(),
    Body = jsx:encode(AllMetrics),
    {Body, Req, State};

to_json(Req, State = #{endpoint := work_orders}) ->
    %% Get all work orders
    WorkOrders = get_all_work_orders(),
    Body = jsx:encode(#{work_orders => WorkOrders}),
    {Body, Req, State};

to_json(Req, State = #{endpoint := work_order_detail}) ->
    Id = cowboy_req:binding(id, Req),
    WorkOrder = get_work_order_by_id(Id),
    Body = jsx:encode(#{work_order => WorkOrder}),
    {Body, Req, State};

to_json(Req, State = #{endpoint := andon_detail}) ->
    Id = cowboy_req:binding(id, Req),
    Andon = get_andon_by_id(Id),
    Body = jsx:encode(#{andon => Andon}),
    {Body, Req, State};

to_json(Req, State = #{endpoint := sku_receipts}) ->
    SkuId = cowboy_req:binding(id, Req),
    Receipts = get_sku_receipts(SkuId),
    Body = jsx:encode(#{receipts => Receipts}),
    {Body, Req, State};

to_json(Req, State = #{endpoint := health}) ->
    Health = #{
        status => ok,
        timestamp => erlang:timestamp(),
        uptime_seconds => get_uptime(),
        metrics_count => count_metrics(),
        services => #{
            aggregator => service_status(tcps_metrics_aggregator),
            sse_manager => service_status(tcps_sse_manager),
            kanban => service_status(tcps_kanban),
            andon => service_status(tcps_andon),
            work_order => service_status(tcps_work_order)
        },
        sse_clients => get_sse_client_count()
    },
    Body = jsx:encode(Health),
    {Body, Req, State};

to_json(Req, State = #{endpoint := export}) ->
    Format = cowboy_req:binding(format, Req),
    FormatAtom = binary_to_atom(Format, utf8),
    case tcps_dashboard:export_dashboard_data(FormatAtom) of
        {error, Reason} ->
            Body = jsx:encode(#{error => Reason}),
            {Body, Req, State};
        Data ->
            {Data, Req, State}
    end;

to_json(Req, State) ->
    Body = jsx:encode(#{error => <<"Unknown endpoint">>}),
    {Body, Req, State}.

to_html(Req, State) ->
    %% Return HTML for browser requests
    Body = <<"<html><body><h1>TCPS Dashboard</h1><p>Use /dashboard for UI</p></body></html>">>,
    {Body, Req, State}.

from_json(Req, State = #{endpoint := work_orders}) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),

    %% Create work order
    WorkOrder = create_work_order(Data),
    ResponseBody = jsx:encode(#{work_order => WorkOrder, created => true}),

    Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
    {true, Req3, State};

from_json(Req, State = #{endpoint := andon_resolve}) ->
    {ok, Body, Req2} = cowboy_req:read_body(Req),
    Data = jsx:decode(Body, [return_maps]),
    AndonId = cowboy_req:binding(id, Req2),

    %% Resolve andon
    Result = resolve_andon(AndonId, Data),
    ResponseBody = jsx:encode(#{result => Result, resolved => true}),

    Req3 = cowboy_req:set_resp_body(ResponseBody, Req2),
    {true, Req3, State};

from_json(Req, State) ->
    Body = jsx:encode(#{error => <<"Method not allowed">>}),
    Req2 = cowboy_req:set_resp_body(Body, Req),
    {false, Req2, State}.

-spec terminate(term(), cowboy_req:req(), map()) -> ok.
terminate(_Reason, _Req, _State) ->
    ok.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_all_work_orders() ->
    %% Mock implementation - would query tcps_kanban
    [
        #{
            id => <<"WO-001">>,
            sku_id => <<"SKU-2024-006">>,
            bucket => in_progress,
            priority => high,
            created_at => {{2024, 1, 26}, {10, 0, 0}}
        },
        #{
            id => <<"WO-002">>,
            sku_id => <<"SKU-2024-007">>,
            bucket => review,
            priority => medium,
            created_at => {{2024, 1, 26}, {11, 30, 0}}
        }
    ].

get_work_order_by_id(Id) ->
    %% Mock implementation
    #{
        id => Id,
        sku_id => <<"SKU-2024-006">>,
        bucket => in_progress,
        priority => high,
        created_at => {{2024, 1, 26}, {10, 0, 0}},
        history => [
            #{bucket => backlog, timestamp => {{2024, 1, 25}, {14, 0, 0}}},
            #{bucket => ready, timestamp => {{2024, 1, 26}, {9, 0, 0}}},
            #{bucket => in_progress, timestamp => {{2024, 1, 26}, {10, 0, 0}}}
        ]
    }.

get_andon_by_id(Id) ->
    %% Mock implementation
    #{
        id => Id,
        severity => critical,
        title => <<"Test failure in authentication module">>,
        description => <<"Unit tests failing in auth_handler.erl">>,
        affected_skus => [<<"SKU-2024-001">>],
        triggered_at => {{2024, 1, 26}, {10, 30, 0}},
        triggered_by => <<"automated-test-suite">>,
        resolution_steps => []
    }.

get_sku_receipts(SkuId) ->
    %% Mock implementation - would integrate with TCPS receipt system
    [
        #{
            id => <<"receipt-001">>,
            sku_id => SkuId,
            type => design_receipt,
            approved => true,
            approved_by => <<"architect-001">>,
            approved_at => {{2024, 1, 25}, {15, 0, 0}}
        },
        #{
            id => <<"receipt-002">>,
            sku_id => SkuId,
            type => code_receipt,
            approved => true,
            approved_by => <<"reviewer-001">>,
            approved_at => {{2024, 1, 26}, {9, 30, 0}}
        }
    ].

create_work_order(Data) ->
    %% Mock implementation - would call tcps_kanban:add_work_item
    SkuId = maps:get(<<"sku_id">>, Data, <<"SKU-NEW">>),
    Priority = maps:get(<<"priority">>, Data, medium),

    #{
        id => generate_work_order_id(),
        sku_id => SkuId,
        bucket => backlog,
        priority => binary_to_atom(Priority, utf8),
        created_at => erlang:timestamp()
    }.

resolve_andon(AndonId, Data) ->
    %% Mock implementation - would update andon status
    Resolution = maps:get(<<"resolution">>, Data, <<"Fixed">>),
    ResolvedBy = maps:get(<<"resolved_by">>, Data, <<"operator">>),

    ?LOG_INFO("Resolving Andon ~s: ~s by ~s", [AndonId, Resolution, ResolvedBy]),

    #{
        andon_id => AndonId,
        resolved_at => erlang:timestamp(),
        resolution => Resolution,
        resolved_by => ResolvedBy
    }.

get_uptime() ->
    %% Return uptime in seconds
    {UpTime, _} = erlang:statistics(wall_clock),
    UpTime div 1000.

count_metrics() ->
    Summary = tcps_dashboard:get_metrics_summary(),
    maps:size(Summary).

generate_work_order_id() ->
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("WO-~p", [Timestamp])).

%% @private Check service status
service_status(ServiceName) ->
    case whereis(ServiceName) of
        undefined -> down;
        Pid when is_pid(Pid) -> running
    end.

%% @private Get SSE client count
get_sse_client_count() ->
    case whereis(tcps_sse_manager) of
        undefined -> 0;
        _Pid ->
            try
                tcps_sse_manager:get_client_count()
            catch
                _:_ -> 0
            end
    end.
