%%%-------------------------------------------------------------------
%%% @doc
%%% Test HTTP MCP handler for testing
%%%
%%% This module implements a cowboy handler that simulates various
%%% HTTP server behaviors for testing the HTTP transport layer.
%%%-------------------------------------------------------------------
-module(test_http_mcp_handler).

-export([init/2]).

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    %% Log request for debugging
    ct:pal("Test HTTP server: ~s ~s", [Method, Path]),

    {ok, handle_request(Req0, State), State}.

handle_request(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),

    case {Method, Path} of
        {<<"POST">>, <<"/mcp">>} ->
            handle_mcp_request(Req0);
        {<<"GET">>, <<"/mcp">>} ->
            handle_mcp_get(Req0);
        {<<"POST">>, <<"/mcp/error">>} ->
            handle_error_response(Req0);
        {<<"POST">>, <<"/mcp/retry">>} ->
            handle_retry_request(Req0);
        {<<"POST">>, <<"/mcp/fail">>} ->
            handle_fail_request(Req0);
        {<<"POST">>, <<"/mcp/invalid">>} ->
            handle_invalid_response(Req0);
        {<<"POST">>, <<"/mcp/slow">>} ->
            handle_slow_request(Req0);
        {<<"POST">>, <<"/mcp/empty">>} ->
            handle_empty_response(Req0);
        _ ->
            handle_not_found(Req0)
    end.

%%====================================================================
%% Request Handlers
%%====================================================================

%% Handle standard MCP POST request
handle_mcp_request(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    %% Echo the request as response
    Response =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"result">> => #{<<"status">> => <<"ok">>, <<"echo">> => Body},
                     <<"id">> => 1}),

    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1).

%% Handle MCP GET request
handle_mcp_get(Req0) ->
    Response =
        jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                     <<"result">> => #{<<"status">> => <<"ok">>},
                     <<"id">> => 1}),

    cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req0).

%% Handle error response (500)
handle_error_response(Req0) ->
    cowboy_req:reply(500,
                     #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(#{<<"error">> => <<"Internal Server Error">>}),
                     Req0).

%% Handle retry scenario (succeed after first attempt)
handle_retry_request(Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),

    case jsx:is_json(Body) of
        true ->
            Response =
                jsx:encode(#{<<"jsonrpc">> => <<"2.0">>,
                             <<"result">> => #{<<"status">> => <<"ok">>},
                             <<"id">> => 1}),
            cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Response, Req1);
        false ->
            cowboy_req:reply(500, #{}, <<"Server Error">>, Req1)
    end.

%% Handle fail request (always fail for retry testing)
handle_fail_request(Req0) ->
    cowboy_req:reply(500,
                     #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(#{<<"error">> => <<"Server Error">>}),
                     Req0).

%% Handle invalid JSON response
handle_invalid_response(Req0) ->
    cowboy_req:reply(200,
                     #{<<"content-type">> => <<"application/json">>},
                     <<"{invalid json}">>,
                     Req0).

%% Handle slow request (for timeout testing)
handle_slow_request(Req0) ->
    timer:sleep(5000),
    cowboy_req:reply(200, #{}, <<"{\"status\":\"ok\"}">>, Req0).

%% Handle empty response
handle_empty_response(Req0) ->
    cowboy_req:reply(204, #{}, <<>>, Req0).

%% Handle not found
handle_not_found(Req0) ->
    cowboy_req:reply(404,
                     #{<<"content-type">> => <<"application/json">>},
                     jsx:encode(#{<<"error">> => <<"Not Found">>}),
                     Req0).
