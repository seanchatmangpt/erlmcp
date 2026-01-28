%%%===================================================================
%%% http_bench_handler.erl - Cowboy Handler for HTTP Benchmark
%%%===================================================================

-module(http_bench_handler).

-export([init/2]).

%%====================================================================
%% Cowboy Handler Callbacks
%%====================================================================

init(Req0, State) ->
    %% Simple echo handler for benchmark
    {ok, _Body, Req1} = cowboy_req:read_body(Req0),

    %% Parse JSON-RPC (minimal validation for performance)
    %% Just echo back a simple success response
    Response = jsx:encode(#{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => 1,
        <<"result">> => <<"ok">>
    }),

    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Response, Req1),

    {ok, Req, State}.
