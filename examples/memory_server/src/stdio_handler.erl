-module(stdio_handler).

-export([start/0, loop/0]).

-include("../include/memory_server.hrl").

start() ->
    spawn(?MODULE, loop, []).

loop() ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            handle_line(string:trim(Line)),
            loop()
    end.

handle_line([]) ->
    ok;
handle_line(Line) ->
    try jsx:decode(list_to_binary(Line), [return_maps]) of
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"id">> := Id} = Request ->
            Params = maps:get(<<"params">>, Request, #{}),
            handle_request(Method, Params, Id);
        #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method} = Request ->
            Params = maps:get(<<"params">>, Request, #{}),
            handle_notification(Method, Params);
        _ ->
            ok
    catch
        _:_ ->
            ok
    end.

handle_request(<<"initialize">>, Params, Id) ->
    Result = mcp_protocol:handle_initialize(Params),
    mcp_protocol:send_response(Id, Result);

handle_request(<<"tools/list">>, _Params, Id) ->
    Result = mcp_protocol:handle_list_tools(),
    mcp_protocol:send_response(Id, Result);

handle_request(<<"tools/call">>, #{<<"name">> := ToolName, <<"arguments">> := Arguments}, Id) ->
    case mcp_protocol:handle_call_tool(ToolName, Arguments) of
        {error, Message} ->
            mcp_protocol:send_error(Id, -32000, Message);
        Content ->
            Result = #{<<"content">> => Content},
            mcp_protocol:send_response(Id, Result)
    end;

handle_request(<<"resources/list">>, _Params, Id) ->
    Result = mcp_protocol:handle_list_resources(),
    mcp_protocol:send_response(Id, Result);

handle_request(<<"resources/read">>, #{<<"uri">> := Uri}, Id) ->
    case mcp_protocol:handle_read_resource(Uri) of
        {error, Message} ->
            mcp_protocol:send_error(Id, -32000, Message);
        Content ->
            Result = #{<<"contents">> => Content},
            mcp_protocol:send_response(Id, Result)
    end;

handle_request(Method, _Params, Id) ->
    mcp_protocol:send_error(Id, -32601, <<"Method not found: ", Method/binary>>).

handle_notification(_Method, _Params) ->
    ok.
