-module(erlmcp_api_gateway_health).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    Health = erlmcp_api_gateway_monitor:get_health(),
    Response = jsx:encode(#{<<"health">> => Health}),
    cowboy_req:reply(200, #{}, Response, Req, State).