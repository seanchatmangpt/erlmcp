-module(erlmcp_json_rpc).

-include("erlmcp.hrl").

-export([
    encode_request/3,
    encode_response/2,
    encode_error_response/3,
    encode_notification/2,
    decode_message/1,
    create_error/3
]).

encode_request(Id, Method, Params) ->
    Request = #json_rpc_request{
        id = Id,
        method = Method,
        params = Params
    },
    encode_json_rpc(Request).

encode_response(Id, Result) ->
    Response = #json_rpc_response{
        id = Id,
        result = Result
    },
    encode_json_rpc(Response).

encode_error_response(Id, Code, Message) ->
    Error = #{
        <<"code">> => Code,
        <<"message">> => Message
    },
    Response = #json_rpc_response{
        id = Id,
        error = Error
    },
    encode_json_rpc(Response).

encode_notification(Method, Params) ->
    Notification = #json_rpc_notification{
        method = Method,
        params = Params
    },
    encode_json_rpc(Notification).

decode_message(Json) when is_binary(Json) ->
    try
        Data = jsx:decode(Json, [return_maps]),
        parse_json_rpc(Data)
    catch
        error:Reason ->
            {error, {parse_error, Reason}}
    end.

create_error(Code, Message, Data) ->
    #mcp_error{
        code = Code,
        message = Message,
        data = Data
    }.

encode_json_rpc(#json_rpc_request{id = Id, method = Method, params = Params}) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"method">> => Method
    },
    MapWithParams = case Params of
        undefined -> Map;
        _ -> Map#{<<"params">> => Params}
    end,
    jsx:encode(MapWithParams);

encode_json_rpc(#json_rpc_response{id = Id, result = Result, error = Error}) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id
    },
    MapWithResult = case {Result, Error} of
        {_, Error} when Error =/= undefined ->
            Map#{<<"error">> => Error};
        {Result, _} ->
            Map#{<<"result">> => Result}
    end,
    jsx:encode(MapWithResult);

encode_json_rpc(#json_rpc_notification{method = Method, params = Params}) ->
    Map = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"method">> => Method
    },
    MapWithParams = case Params of
        undefined -> Map;
        _ -> Map#{<<"params">> => Params}
    end,
    jsx:encode(MapWithParams).

parse_json_rpc(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := Id, <<"method">> := Method} = Data) ->
    Params = maps:get(<<"params">>, Data, undefined),
    {ok, #json_rpc_request{id = Id, method = Method, params = Params}};

parse_json_rpc(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := Id, <<"result">> := Result}) ->
    {ok, #json_rpc_response{id = Id, result = Result}};

parse_json_rpc(#{<<"jsonrpc">> := <<"2.0">>, <<"id">> := Id, <<"error">> := Error}) ->
    {ok, #json_rpc_response{id = Id, error = Error}};

parse_json_rpc(#{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Data) ->
    Params = maps:get(<<"params">>, Data, undefined),
    {ok, #json_rpc_notification{method = Method, params = Params}};

parse_json_rpc(_) ->
    {error, invalid_json_rpc}.
