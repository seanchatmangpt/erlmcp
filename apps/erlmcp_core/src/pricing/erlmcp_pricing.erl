%%%-------------------------------------------------------------------
%%% @doc erlmcp_pricing - Simple pricing tier upgrade logic
%%%
%%% Joe Armstrong principles:
%%% - "Pricing is business logic" - keep it simple
%%% - "State transitions" - track subscription changes
%%% - "Let it crash" - invalid upgrades fail fast
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pricing).

%% Public API
-export([
    upgrade/2,
    get_tier/1,
    can_upgrade/2,
    get_user_tier/1,
    list_tiers/0,
    validate_upgrade/2
]).

%% Type definitions
-type tier_id() :: 0 | 1 | 2.
-type user_id() :: binary().
-type pricing_tier() :: #{
    id := tier_id(),
    name := binary(),
    monthly_price := integer(),
    features := [binary()],
    api_quota := integer(),
    storage_quota := integer()
}.
-type upgrade_result() :: {ok, map()} | {error, term()}.

-export_type([tier_id/0, pricing_tier/0, upgrade_result/0]).

-define(TIER_FREE, 0).
-define(TIER_PRO, 1).
-define(TIER_ENTERPRISE, 2).
-define(SUBSCRIPTION_TABLE, erlmcp_pricing_subscription).

%%%-------------------------------------------------------------------
%%% Public API
%%%-------------------------------------------------------------------

get_tier(?TIER_FREE) ->
    #{
        id => ?TIER_FREE,
        name => <<"Free">>,
        monthly_price => 0,
        features => [<<"1K API requests/month">>, <<"100 MB storage">>],
        api_quota => 1000,
        storage_quota => 100
    };
get_tier(?TIER_PRO) ->
    #{
        id => ?TIER_PRO,
        name => <<"Pro">>,
        monthly_price => 2900,
        features => [<<"100K API requests/month">>, <<"10 GB storage">>],
        api_quota => 100000,
        storage_quota => 10000
    };
get_tier(?TIER_ENTERPRISE) ->
    #{
        id => ?TIER_ENTERPRISE,
        name => <<"Enterprise">>,
        monthly_price => 29900,
        features => [<<"Unlimited API requests">>, <<"Unlimited storage">>],
        api_quota => -1,
        storage_quota => -1
    }.

list_tiers() ->
    [get_tier(?TIER_FREE), get_tier(?TIER_PRO), get_tier(?TIER_ENTERPRISE)].

can_upgrade(FromTier, ToTier) when FromTier < ToTier, ToTier =< ?TIER_ENTERPRISE ->
    true;
can_upgrade(_, _) ->
    false.

get_user_tier(UserId) ->
    ensure_table(),
    Key = {user_tier, UserId},
    case ets:lookup(?SUBSCRIPTION_TABLE, Key) of
        [{_, TierId}] when is_integer(TierId) -> TierId;
        [] -> ?TIER_FREE
    end.

validate_upgrade(UserId, NewTier) ->
    CurrentTier = get_user_tier(UserId),
    case CurrentTier of
        NewTier -> {error, already_on_tier};
        _Other ->
            case can_upgrade(CurrentTier, NewTier) of
                false -> {error, invalid_tier_upgrade};
                true -> ok
            end
    end.

upgrade(UserId, NewTier) when is_binary(UserId), is_integer(NewTier) ->
    case validate_upgrade(UserId, NewTier) of
        {error, Reason} ->
            {error, Reason};
        ok ->
            case process_payment(UserId, NewTier) of
                {ok, Payment} ->
                    update_subscription(UserId, NewTier),
                    Tier = get_tier(NewTier),
                    {ok, #{
                        user_id => UserId,
                        tier_id => NewTier,
                        tier_name => maps:get(name, Tier),
                        payment => Payment,
                        upgraded_at => erlang:system_time(millisecond)
                    }};
                {error, Reason} ->
                    {error, {payment_failed, Reason}}
            end
    end.

%%%-------------------------------------------------------------------
%%% Private Functions
%%%-------------------------------------------------------------------

process_payment(UserId, TierId) ->
    try
        Tier = get_tier(TierId),
        Price = maps:get(monthly_price, Tier),
        Payment = #{
            amount => Price,
            currency => <<"USD">>,
            user_id => UserId,
            tier_id => TierId,
            timestamp => erlang:system_time(millisecond),
            transaction_id => generate_transaction_id(),
            status => paid
        },
        {ok, Payment}
    catch
        _Error:_Reason -> {error, payment_processing_failed}
    end.

update_subscription(UserId, TierId) ->
    ensure_table(),
    Key = {user_tier, UserId},
    ets:insert(?SUBSCRIPTION_TABLE, {Key, TierId}),
    ok.

generate_transaction_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(1000000),
    list_to_binary(io_lib:format("tx_~b_~b", [Timestamp, Random])).

ensure_table() ->
    case ets:info(?SUBSCRIPTION_TABLE) of
        undefined ->
            ets:new(?SUBSCRIPTION_TABLE, [
                named_table,
                public,
                {write_concurrency, true},
                {read_concurrency, true}
            ]);
        _ ->
            ok
    end,
    ok.
