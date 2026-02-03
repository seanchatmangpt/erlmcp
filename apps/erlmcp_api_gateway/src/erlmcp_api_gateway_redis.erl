-module(erlmcp_api_gateway_redis).
-export([execute/1]).

execute([Cmd | Args]) ->
    case redis_client:execute(Cmd, Args) of
        {ok, Result} -> {ok, Result};
        {error, Reason} -> {error, Reason}
    end.