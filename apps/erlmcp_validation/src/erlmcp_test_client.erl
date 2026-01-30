-module(erlmcp_test_client).
-behaviour(gen_server).

-export([
    start_test_server/2,
    stop_test_server/1,
    send_request/2,
    send_notification/2,
    wait_for_response/2,
    close_test_server/1,
    generate_request_id/0,
    validate_response/2,
    format_request/1,
    handle_transport_errors/1,
    get_server_info/1,
    set_response_timeout/2,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

start_test_server(TransportType, Config) ->
    gen_server:start_link(?MODULE, [TransportType, Config], []).

stop_test_server(ServerRef) when is_pid(ServerRef) ->
    gen_server:stop(ServerRef).

send_request(ServerRef, Request) when is_pid(ServerRef), is_map(Request) ->
    gen_server:call(ServerRef, {send_request, Request}, infinity).

send_notification(ServerRef, Notification) when is_pid(ServerRef), is_map(Notification) ->
    gen_server:call(ServerRef, {send_notification, Notification}, infinity).

wait_for_response(ServerRef, RequestId) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, {wait_for_response, RequestId}, infinity).

close_test_server(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, close_test_server).

generate_request_id() ->
    erlang:unique_integer([positive, monotonic]) band 16#7FFFFFFF.

validate_response(Response, Rules) when is_map(Response), is_map(Rules) ->
    try
        case maps:get(<<"jsonrpc">>, Response, undefined) of
            <<"2.0">> -> ok;
            Invalid -> throw({non_compliant, {invalid_jsonrpc_version, Invalid}})
        end,
        RequiredFields = maps:get(required_fields, Rules, []),
        MissingFields = [F || F <- RequiredFields, not maps:is_key(F, Response)],
        case MissingFields of
            [] -> ok;
            _ -> throw({non_compliant, {missing_required_fields, MissingFields}})
        end,
        {compliant, Response}
    catch
        throw:{non_compliant, Reason} -> {non_compliant, Reason}
    end.

format_request(Request) when is_map(Request) ->
    Method = maps:get(method, Request, <<>>),
    Params = maps:get(params, Request, #{}),
    Id = maps:get(id, Request, generate_request_id()),
    JsonRpc = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"id">> => Id},
    JsonRpcWithParams = case maps:size(Params) of
        0 -> JsonRpc;
        _ -> JsonRpc#{<<"params">> => Params}
    end,
    jsx:encode(JsonRpcWithParams).

handle_transport_errors({error, Reason}) ->
    {error, transport_error, Reason}.

get_server_info(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, get_server_info).

set_response_timeout(ServerRef, Timeout) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, {set_timeout, Timeout}).

init([TransportType, Config]) ->
    Pid = spawn(fun() -> test_server_loop(TransportType, Config) end),
    {ok, #{server_pid => Pid, transport_type => TransportType,
            pending_requests => #{}, counter => 1, timeout => 5000}}.

handle_call({send_request, RequestMap}, From, State) ->
    Method = maps:get(method, RequestMap, <<>>),
    Params = maps:get(params, RequestMap, #{}),
    RequestId = maps:get(id, RequestMap, generate_request_id()),
    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method, <<"id">> => RequestId},
    RequestWithParams = case maps:size(Params) of
        0 -> Request;
        _ -> Request#{<<"params">> => Params}
    end,
    maps:get(server_pid, State) ! {request, RequestWithParams, From},
    {noreply, State#{pending_requests := maps:put(RequestId, From, maps:get(pending_requests, State)),
                      counter := maps:get(counter, State) + 1}};

handle_call({send_notification, Notification}, _From, State) ->
    maps:get(server_pid, State) ! {notification, Notification},
    {reply, ok, State};

handle_call(close_test_server, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_server_info, _From, State) ->
    {reply, {ok, State}, State};

handle_call({set_timeout, Timeout}, _From, State) ->
    {reply, ok, State#{timeout := Timeout}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({response, RequestId, Response}, State) ->
    case maps:take(RequestId, maps:get(pending_requests, State)) of
        {From, NewPending} when is_tuple(From) ->
            gen_server:reply(From, {ok, Response}),
            {noreply, State#{pending_requests => NewPending}};
        error ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case maps:get(server_pid, State, undefined) of
        undefined -> ok;
        Pid when is_pid(Pid) -> exit(Pid, shutdown)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

test_server_loop(_TransportType, _Config) ->
    receive
        {request, Request, From} ->
            Response = handle_test_request(Request),
            gen_server:reply(From, {ok, Response}),
            test_server_loop(_TransportType, _Config);
        {notification, _Notification} ->
            test_server_loop(_TransportType, _Config);
        shutdown ->
            ok
    end.

handle_test_request(#{<<"method">> := <<"initialize">>} = Request) ->
    Id = maps:get(<<"id">>, Request, 1),
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id,
      <<"result">> => #{<<"protocolVersion">> => <<"2025-11-25">>,
                       <<"capabilities">> => #{}}};
handle_test_request(Request) ->
    Id = maps:get(<<"id">>, Request, 1),
    #{<<"jsonrpc">> => <<"2.0">>, <<"id">> => Id, <<"result">> => #{}}.
