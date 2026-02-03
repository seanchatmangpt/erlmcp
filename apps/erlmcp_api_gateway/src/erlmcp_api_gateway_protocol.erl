-module(erlmcp_api_gateway_protocol).
-behaviour(cowboy_protocol).

-export([start_link/4]).

start_link(Ref, _, _, _) ->
    cowboy_protocol:start_link(Ref).