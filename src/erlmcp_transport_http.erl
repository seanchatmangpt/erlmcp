-module(erlmcp_transport_http).
-export([init/1, send/2, close/1]).

init(Opts) ->
    httpc:start(),
    case maps:get(protocol, Opts, http) of
        https ->
            ssl:start();
        _ ->
            ok
    end,
    Url = maps:get(url, Opts, "http://localhost:8080"),
    {ok, {http, Url}}.

send({http, Url}, Data) ->
    Headers = [{"Content-Type", "application/json"}],
    case httpc:request(post, {Url, Headers, "application/json", Data}, [], []) of
        {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} ->
            {ok, Body};
        {ok, {{_Version, StatusCode, _ReasonPhrase}, _Headers, _Body}} ->
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            {error, Reason}
    end.

close({http, _Url}) ->
    ok.
