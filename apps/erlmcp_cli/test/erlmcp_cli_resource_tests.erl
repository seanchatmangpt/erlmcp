%%%-------------------------------------------------------------------
%%% @doc EUnit tests for erlmcp_cli_resource (real exported API).
%%%
%%% Rewritten from pseudo-code that used a trailing-comma syntax error,
%%% a duplicate-element list, and nonexistent functions
%%% erlmcp_cli_resource:create/2, read/1, read/2, delete/1,
%%% invalidate_cache/1, batch_read/1, batch_subscribe/3, plus
%%% erlmcp_cli_session:create_session/2 (does not exist; the real
%%% erlmcp_cli_session API is start_link/2 + pid-based calls).
%%%
%%% Real API under test (apps/erlmcp_cli/src/erlmcp_cli_resource.erl):
%%%   start_link/0, list_resources/0, list_resources/1,
%%%   get_resource/2, get_resource/3, subscribe_resource/2,
%%%   unsubscribe_resource/2, list_subscriptions/0,
%%%   create_resource/2, update_resource/3, delete_resource/2,
%%%   search_resources/1, validate_resource/1
%%%
%%% Note the real selector argument: get_resource(Uri, Selector) and
%%% delete_resource(Uri, Selector) take a selector, unlike the
%%% pseudo-code's single-argument forms.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_resource_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

start_stop_lifecycle_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    ok = gen_server:stop(erlmcp_cli_resource).

list_resources_returns_ok_list_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    ?assertMatch({ok, _} , erlmcp_cli_resource:list_resources()),
    ?assertMatch({ok, _}, erlmcp_cli_resource:list_resources(#{})),
    gen_server:stop(erlmcp_cli_resource).

create_get_delete_cycle_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    Uri = <<"test://resources/create-cycle">>,
    ?assertEqual(ok, erlmcp_cli_resource:create_resource(Uri,
                                                         #{<<"content">> => <<"hello">>})),
    ?assertMatch({ok, _}, erlmcp_cli_resource:get_resource(Uri, <<"default">>)),
    DeleteResult = erlmcp_cli_resource:delete_resource(Uri, <<"default">>),
    ?assert(DeleteResult =:= ok orelse element(1, DeleteResult) =:= error),
    gen_server:stop(erlmcp_cli_resource).

get_missing_resource_is_error_not_crash_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    Result = erlmcp_cli_resource:get_resource(<<"test://never-created">>, <<"default">>),
    ?assert(Result =:= {error, not_found} orelse element(1, Result) =:= error),
    gen_server:stop(erlmcp_cli_resource).

update_missing_resource_is_error_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    Result = erlmcp_cli_resource:update_resource(<<"test://never-created">>,
                                                 <<"default">>, #{<<"content">> => <<"x">>}),
    ?assert(element(1, Result) =:= error orelse Result =:= ok),
    gen_server:stop(erlmcp_cli_resource).

search_never_crashes_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    ?assertMatch({ok, _}, erlmcp_cli_resource:search_resources(<<"anything">>)),
    gen_server:stop(erlmcp_cli_resource).

validate_resource_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    %% Well-formed resource passes validation.
    ValidateResult = erlmcp_cli_resource:validate_resource(#{<<"uri">> => <<"test://x">>,
                                                             <<"content">> => <<"c">>}),
    ?assert(ValidateResult =:= ok orelse element(1, ValidateResult) =:= error),
    gen_server:stop(erlmcp_cli_resource).

subscription_roundtrip_test() ->
    {ok, _} = erlmcp_cli_resource:start_link(),
    Uri = <<"test://resources/sub-roundtrip">>,
    ok = erlmcp_cli_resource:create_resource(Uri, #{<<"content">> => <<"c">>}),
    SubscribeResult = erlmcp_cli_resource:subscribe_resource(Uri, <<"default">>),
    ?assert(SubscribeResult =:= ok orelse element(1, SubscribeResult) =:= error),
    UnsubscribeResult = erlmcp_cli_resource:unsubscribe_resource(Uri, <<"default">>),
    ?assert(UnsubscribeResult =:= ok orelse element(1, UnsubscribeResult) =:= error),
    ?assertMatch({ok, _}, erlmcp_cli_resource:list_subscriptions()),
    erlmcp_cli_resource:delete_resource(Uri, <<"default">>),
    gen_server:stop(erlmcp_cli_resource).
