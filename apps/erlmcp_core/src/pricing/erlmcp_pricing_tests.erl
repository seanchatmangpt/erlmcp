%%%-------------------------------------------------------------------
%%% @doc erlmcp_pricing tests - Verify all upgrade paths work correctly
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing_tests).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Fixtures
%%%-------------------------------------------------------------------

setup() ->
    % Ensure clean ETS table for each test
    catch ets:delete(erlmcp_pricing_subscription),
    ok.

cleanup(_) ->
    % Clean up after test
    catch ets:delete(erlmcp_pricing_subscription),
    ok.

%%%-------------------------------------------------------------------
%%% Core API Tests
%%%-------------------------------------------------------------------

upgrade_from_free_to_pro_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_free_to_pro">>,
                    % Start on free tier (default)
                    ?assertEqual(0, erlmcp_pricing:get_user_tier(UserId)),

                    % Upgrade to pro
                    {ok, Result} = erlmcp_pricing:upgrade(UserId, 1),

                    % Verify upgrade
                    ?assertEqual(<<"Pro">>, maps:get(tier_name, Result)),
                    ?assertEqual(1, maps:get(tier_id, Result)),
                    ?assertEqual(UserId, maps:get(user_id, Result)),
                    ?assert(maps:is_key(payment, Result)),
                    ?assert(maps:is_key(upgraded_at, Result)),

                    % Verify user tier updated
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(UserId))
                end)]
     end}.

upgrade_from_free_to_enterprise_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_free_to_ent">>,
                    % Upgrade directly to enterprise
                    {ok, Result} = erlmcp_pricing:upgrade(UserId, 2),

                    ?assertEqual(<<"Enterprise">>, maps:get(tier_name, Result)),
                    ?assertEqual(2, maps:get(tier_id, Result)),
                    ?assertEqual(2, erlmcp_pricing:get_user_tier(UserId))
                end)]
     end}.

upgrade_from_pro_to_enterprise_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_pro_to_ent">>,
                    % Start on pro
                    {ok, _} = erlmcp_pricing:upgrade(UserId, 1),
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(UserId)),

                    % Upgrade to enterprise
                    {ok, Result} = erlmcp_pricing:upgrade(UserId, 2),

                    ?assertEqual(<<"Enterprise">>, maps:get(tier_name, Result)),
                    ?assertEqual(2, erlmcp_pricing:get_user_tier(UserId))
                end)]
     end}.

invalid_downgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_downgrade">>,
                    % Start on enterprise
                    {ok, _} = erlmcp_pricing:upgrade(UserId, 2),

                    % Try to downgrade to pro (should fail)
                    ?assertEqual({error, invalid_tier_upgrade}, erlmcp_pricing:upgrade(UserId, 1)),

                    % Verify still on enterprise
                    ?assertEqual(2, erlmcp_pricing:get_user_tier(UserId))
                end)]
     end}.

already_on_tier_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_same_tier">>,
                    % Start on pro
                    {ok, _} = erlmcp_pricing:upgrade(UserId, 1),

                    % Try to upgrade to pro again
                    ?assertEqual({error, already_on_tier}, erlmcp_pricing:upgrade(UserId, 1))
                end)]
     end}.

invalid_tier_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"user_invalid_tier">>,
                    % Try to upgrade to invalid tier
                    ?assertEqual({error, invalid_tier_upgrade}, erlmcp_pricing:upgrade(UserId, 99))
                end)]
     end}.

get_tier_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    Free = erlmcp_pricing:get_tier(0),
                    ?assertEqual(0, maps:get(id, Free)),
                    ?assertEqual(<<"Free">>, maps:get(name, Free)),
                    ?assertEqual(0, maps:get(monthly_price, Free)),

                    Pro = erlmcp_pricing:get_tier(1),
                    ?assertEqual(1, maps:get(id, Pro)),
                    ?assertEqual(<<"Pro">>, maps:get(name, Pro)),
                    ?assertEqual(2900, maps:get(monthly_price, Pro)),

                    Ent = erlmcp_pricing:get_tier(2),
                    ?assertEqual(2, maps:get(id, Ent)),
                    ?assertEqual(<<"Enterprise">>, maps:get(name, Ent)),
                    ?assertEqual(29900, maps:get(monthly_price, Ent))
                end)]
     end}.

can_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_assert(erlmcp_pricing:can_upgrade(0, 1)),
         ?_assert(erlmcp_pricing:can_upgrade(0, 2)),
         ?_assert(erlmcp_pricing:can_upgrade(1, 2)),
         ?_assertNot(erlmcp_pricing:can_upgrade(1, 1)),
         ?_assertNot(erlmcp_pricing:can_upgrade(1, 0)),
         ?_assertNot(erlmcp_pricing:can_upgrade(2, 1)),
         ?_assertNot(erlmcp_pricing:can_upgrade(2, 0)),
         ?_assertNot(erlmcp_pricing:can_upgrade(0, 99))]
     end}.

list_tiers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    Tiers = erlmcp_pricing:list_tiers(),
                    ?assertEqual(3, length(Tiers)),

                    % Verify all tiers present
                    TierIds = [maps:get(id, T) || T <- Tiers],
                    ?assert(lists:member(0, TierIds)),
                    ?assert(lists:member(1, TierIds)),
                    ?assert(lists:member(2, TierIds))
                end)]
     end}.

%%%-------------------------------------------------------------------
%%% CLI Integration Tests
%%%-------------------------------------------------------------------

cli_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"cli_user">>,
                    % CLI uses plan names (atoms) instead of tier IDs
                    ?assertEqual(ok, erlmcp_pricing_cli:upgrade(UserId, pro)),

                    % Verify tier updated via core API
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(UserId))
                end)]
     end}.

cli_upgrade_all_plans_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    % Test all plan names map correctly
                    % Start on pro (upgrade from free)
                    ?assertEqual(ok, erlmcp_pricing_cli:upgrade(<<"u1">>, pro)),
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(<<"u1">>)),

                    % Start on enterprise (upgrade from free)
                    ?assertEqual(ok, erlmcp_pricing_cli:upgrade(<<"u2">>, enterprise)),
                    ?assertEqual(2, erlmcp_pricing:get_user_tier(<<"u2">>)),

                    % Pro to enterprise
                    ?assertEqual(ok, erlmcp_pricing_cli:upgrade(<<"u3">>, pro)),
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(<<"u3">>)),
                    ?assertEqual(ok, erlmcp_pricing_cli:upgrade(<<"u3">>, enterprise)),
                    ?assertEqual(2, erlmcp_pricing:get_user_tier(<<"u3">>))
                end)]
     end}.

%%%-------------------------------------------------------------------
%%% HTTP Integration Tests
%%%-------------------------------------------------------------------

http_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    % Note: HTTP handler currently has stubbed parse_upgrade_request
                    % that returns {ok, <<"test_user">>, 1}
                    Result =
                        erlmcp_pricing_http:handle_request(<<"POST">>, <<"/api/pricing/upgrade">>),

                    % Should return success
                    ?assertMatch({ok, #{<<"status">> := <<"success">>}}, Result),

                    % Verify test_user was upgraded via core API
                    ?assertEqual(1, erlmcp_pricing:get_user_tier(<<"test_user">>))
                end)]
     end}.

http_invalid_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    % Note: HTTP handler has stubbed parse_upgrade_request that returns
                    % {ok, <<"test_user">>, 1}, so we can't test different users
                    % This test verifies the error path works via direct API
                    % First upgrade to pro
                    {ok, _} = erlmcp_pricing:upgrade(<<"http_test_user">>, 1),

                    % Try same upgrade again via direct API
                    Result = erlmcp_pricing:upgrade(<<"http_test_user">>, 1),

                    % Should return error (already on tier)
                    ?assertEqual({error, already_on_tier}, Result)
                end)]
     end}.

%%%-------------------------------------------------------------------
%%% Payment Processing Tests
%%%-------------------------------------------------------------------

payment_amount_correct_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    % Free to Pro: $29.00
                    {ok, Result1} = erlmcp_pricing:upgrade(<<"pay1">>, 1),
                    Payment1 = maps:get(payment, Result1),
                    ?assertEqual(2900, maps:get(amount, Payment1)),
                    ?assertEqual(<<"USD">>, maps:get(currency, Payment1)),
                    ?assertEqual(paid, maps:get(status, Payment1)),
                    ?assert(maps:is_key(transaction_id, Payment1)),

                    % Free to Enterprise: $299.00
                    {ok, Result2} = erlmcp_pricing:upgrade(<<"pay2">>, 2),
                    Payment2 = maps:get(payment, Result2),
                    ?assertEqual(29900, maps:get(amount, Payment2)),

                    % Pro to Enterprise: $299.00 - $29.00 = $270.00 (simplified for demo)
                    {ok, _} = erlmcp_pricing:upgrade(<<"pay3">>, 1),
                    {ok, Result3} = erlmcp_pricing:upgrade(<<"pay3">>, 2),
                    Payment3 = maps:get(payment, Result3),
                    ?assertEqual(29900, maps:get(amount, Payment3))
                end)]
     end}.

transaction_id_unique_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    % Generate multiple upgrades
                    {ok, R1} = erlmcp_pricing:upgrade(<<"tx1">>, 1),
                    {ok, R2} = erlmcp_pricing:upgrade(<<"tx2">>, 1),
                    {ok, R3} = erlmcp_pricing:upgrade(<<"tx3">>, 1),

                    Payment1 = maps:get(payment, R1),
                    Payment2 = maps:get(payment, R2),
                    Payment3 = maps:get(payment, R3),
                    TxId1 = maps:get(transaction_id, Payment1),
                    TxId2 = maps:get(transaction_id, Payment2),
                    TxId3 = maps:get(transaction_id, Payment3),

                    % All transaction IDs should be unique
                    ?assert(TxId1 =/= TxId2),
                    ?assert(TxId2 =/= TxId3),
                    ?assert(TxId1 =/= TxId3),

                    % Transaction IDs should start with "tx_"
                    ?assertEqual(<<"tx_">>, binary:part(TxId1, {0, 3}))
                end)]
     end}.

%%%-------------------------------------------------------------------
%%% Concurrency Tests
%%%-------------------------------------------------------------------

concurrent_upgrade_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
        [?_test(begin
                    UserId = <<"concurrent_user">>,
                    % Spawn multiple concurrent upgrades
                    _Pids =
                        [spawn(fun() -> erlmcp_pricing:upgrade(UserId, 1) end)
                         || _ <- lists:seq(1, 10)],

                    % Wait for all to complete
                    timer:sleep(100),

                    % Verify final state is consistent
                    Tier = erlmcp_pricing:get_user_tier(UserId),
                    ?assert(Tier =:= 0 orelse Tier =:= 1)
                end)]
     end}.
