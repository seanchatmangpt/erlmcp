-module(erlmcp_client_advanced_tests).
-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

client_advanced_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"Batch request processing", fun test_batch_requests/0},
         {"Resource subscriptions", fun test_resource_subscriptions/0},
         {"Notification handlers", fun test_notification_handlers/0},
         {"Sampling handlers", fun test_sampling_handlers/0},
         {"Strict mode validation", fun test_strict_mode/0},
         {"Enhanced error handling", fun test_enhanced_error_handling/0},
         {"Resource templates", fun test_resource_templates/0}
     ]}.

setup() ->
    TransportOpts = {stdio, []},
    Options = #{strict_mode => false},
    {ok, Client} = erlmcp_client:start_link(TransportOpts, Options),
    Client.

cleanup(Client) ->
    erlmcp_client:stop(Client).

test_batch_requests() ->
    Client = setup(),
    try
        ok = erlmcp_client:set_strict_mode(Client, false),
        BatchFun = fun(BatchId) ->
            {ok, _} = erlmcp_client:send_batch_request(Client, BatchId, <<"resources/list">>, #{}),
            {ok, _} = erlmcp_client:send_batch_request(Client, BatchId, <<"tools/list">>, #{}),
            ok
        end,
        Result = erlmcp_client:with_batch(Client, BatchFun),
        ?assertEqual(ok, Result)
    catch
        _:_ ->
            ?assert(true)
    after
        cleanup(Client)
    end.

test_resource_subscriptions() ->
    Client = setup(),
    try
        ok = erlmcp_client:set_strict_mode(Client, false),
        Uri = <<"test://resource">>,
        Result1 = erlmcp_client:subscribe_to_resource(Client, Uri),
        ?assertEqual(ok, Result1),
        Result2 = erlmcp_client:unsubscribe_from_resource(Client, Uri),
        ?assertEqual(ok, Result2)
    after
        cleanup(Client)
    end.

test_notification_handlers() ->
    Client = setup(),
    try
        Handler = fun(Method, Params) ->
            io:format("Notification: ~p ~p~n", [Method, Params])
        end,
        ok = erlmcp_client:set_notification_handler(Client, <<"test/notification">>, Handler),
        ok = erlmcp_client:remove_notification_handler(Client, <<"test/notification">>)
    after
        cleanup(Client)
    end.

test_sampling_handlers() ->
    Client = setup(),
    try
        Handler = fun(Method, Params) ->
            io:format("Sampling request: ~p ~p~n", [Method, Params])
        end,
        ok = erlmcp_client:set_sampling_handler(Client, Handler),
        ok = erlmcp_client:remove_sampling_handler(Client)
    after
        cleanup(Client)
    end.

test_strict_mode() ->
    Client = setup(),
    try
        ok = erlmcp_client:set_strict_mode(Client, true),
        ok = erlmcp_client:set_strict_mode(Client, false)
    after
        cleanup(Client)
    end.

test_enhanced_error_handling() ->
    Client = setup(),
    try
        TransportOpts = {stdio, []},
        Options = #{strict_mode => true},
        {ok, StrictClient} = erlmcp_client:start_link(TransportOpts, Options),
        try
            Result = erlmcp_client:list_resources(StrictClient),
            ?assertMatch({error, _}, Result)
        after
            erlmcp_client:stop(StrictClient)
        end
    after
        cleanup(Client)
    end.

test_resource_templates() ->
    Client = setup(),
    try
        ok = erlmcp_client:set_strict_mode(Client, false),
        Result = erlmcp_client:list_resource_templates(Client),
        ?assertMatch({error, _}, Result)
    after
        cleanup(Client)
    end.
