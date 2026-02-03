-module(erlmcp_api_gateway_monetization).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    create_tier/1, get_tier/1, update_tier/2, delete_tier/1,
    subscribe_api/2, calculate_usage/2, generate_invoice/1,
    get_billing_info/1, set_pricing/1, create_promotion/1
]).

-record.tier, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    monthly_fee :: integer(),
    request_limit :: integer(),
    rate_limit :: integer(),
    features :: list(),
    created_at :: integer(),
    active :: boolean()
}.

.subscription, {
    id :: binary(),
    consumer_id :: binary(),
    api_id :: binary(),
    tier_id :: binary(),
    start_date :: integer(),
    end_date :: integer() | undefined,
    status :: active | cancelled | expired,
    pricing_plan :: map()
}.

.usage_record, {
    id :: binary(),
    subscription_id :: binary(),
    timestamp :: integer(),
    request_count :: integer(),
    request_size :: integer(),
    response_size :: integer(),
    cost :: float()
}.

.invoice, {
    id :: binary(),
    consumer_id :: binary(),
    period :: {integer(), integer()},
    usage :: list(),
    subtotal :: float(),
    tax :: float(),
    total :: float(),
    status :: draft | sent | paid | overdue
}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    erlang:send_after(3600000, self(), process_billing),
    {ok, #{tiers => #{}, subscriptions => #{}, usage => #{}, invoices => #{}}}.

create_tier(TierSpec) ->
    gen_server:call(?MODULE, {create_tier, TierSpec}).

get_tier(TierId) ->
    gen_server:call(?MODULE, {get_tier, TierId}).

update_tier(TierId, Updates) ->
    gen_server:call(?MODULE, {update_tier, TierId, Updates}).

delete_tier(TierId) ->
    gen_server:call(?MODULE, {delete_tier, TierId}).

subscribe_api(ApiId, ConsumerId) ->
    gen_server:call(?MODULE, {subscribe_api, ApiId, ConsumerId}).

calculate_usage(SubscriptionId, Period) ->
    gen_server:call(?MODULE, {calculate_usage, SubscriptionId, Period}).

generate_invoice(ConsumerId) ->
    gen_server:call(?MODULE, {generate_invoice, ConsumerId}).

get_billing_info(ConsumerId) ->
    gen_server:call(?MODULE, {get_billing_info, ConsumerId}).

set_pricing(PricingSpec) ->
    gen_server:call(?MODULE, {set_pricing, PricingSpec}).

create_promotion(PromotionSpec) ->
    gen_server:call(?MODULE, {create_promotion, PromotionSpec}).

handle_call({create_tier, TierSpec}, _From, State) ->
    TierId = generate_tier_id(),
    Tier = #tier{
        id = TierId,
        name = maps:get(name, TierSpec),
        description = maps:get(description, TierSpec),
        monthly_fee = maps:get(monthly_fee, TierSpec),
        request_limit = maps:get(request_limit, TierSpec),
        rate_limit = maps:get(rate_limit, TierSpec),
        features = maps:get(features, TierSpec),
        created_at = erlang:system_time(millisecond),
        active = maps:get(active, TierSpec, true)
    },

    Tiers = State#{tiers},
    NewTiers = maps:put(TierId, Tier, Tiers),

    {reply, {ok, Tier}, State#{tiers => NewTiers}};

handle_call({get_tier, TierId}, _From, State) ->
    case maps:find(TierId, State#{tiers}) of
        {ok, Tier} ->
            {reply, {ok, Tier}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update_tier, TierId, Updates}, _From, State) ->
    case maps:find(TierId, State#{tiers}) of
        {ok, Tier} ->
            UpdatedTier = Tier#tier{
                name = maps:get(name, Updates, Tier#tier.name),
                description = maps:get(description, Updates, Tier#tier.description),
                monthly_fee = maps:get(monthly_fee, Updates, Tier#tier.monthly_fee),
                request_limit = maps:get(request_limit, Updates, Tier#tier.request_limit),
                rate_limit = maps:get(rate_limit, Updates, Tier#tier.rate_limit),
                features = maps:get(features, Updates, Tier#tier.features),
                active = maps:get(active, Updates, Tier#tier.active)
            },

            Tiers = State#{tiers},
            NewTiers = maps:put(TierId, UpdatedTier, Tiers),

            {reply, {ok, UpdatedTier}, State#{tiers => NewTiers}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({delete_tier, TierId}, _From, State) ->
    case maps:find(TierId, State#{tiers}) of
        {ok, Tier} when Tier#tier.active =:= false ->
            Tiers = State#{tiers},
            NewTiers = maps:remove(TierId, Tiers),
            {reply, ok, State#{tiers => NewTiers}};
        {ok, _} ->
            {reply, {error, active_tier}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({subscribe_api, ApiId, ConsumerId}, _From, State) ->
    case find_active_tier(State) of
        {ok, Tier} ->
            SubscriptionId = generate_subscription_id(),
            Subscription = #subscription{
                id = SubscriptionId,
                consumer_id = ConsumerId,
                api_id = ApiId,
                tier_id = Tier#tier.id,
                start_date = erlang:system_time(millisecond),
                status = active,
                pricing_plan = generate_pricing_plan(Tier)
            },

            Subscriptions = State#{subscriptions},
            NewSubscriptions = maps:put(SubscriptionId, Subscription, Subscriptions),

            {reply, {ok, Subscription}, State#{subscriptions => NewSubscriptions}};
        {error, no_tiers} ->
            {reply, {error, no_available_tiers}, State}
    end;

handle_call({calculate_usage, SubscriptionId, Period}, _From, State) ->
    case maps:find(SubscriptionId, State#{subscriptions}) of
        {ok, Subscription} ->
            UsageRecords = filter_usage_by_period(Subscription#subscription.id, Period, State#{usage}),
            UsageSummary = summarize_usage(UsageRecords),

            Cost = calculate_usage_cost(UsageSummary, Subscription#subscription.pricing_plan),

            UsageReport = #{
                subscription_id => SubscriptionId,
                period => Period,
                usage => UsageSummary,
                cost => Cost,
                calculated_at => erlang:system_time(millisecond)
            },

            {reply, {ok, UsageReport}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({generate_invoice, ConsumerId}, _From, State) ->
    Subscriptions = filter_active_subscriptions(ConsumerId, State#{subscriptions}),
    UsageRecords = State#{usage},

    InvoiceItems = lists:map(fun(Subscription) ->
        Period = calculate_billing_period(Subscription),
        UsageSummary = summarize_usage(filter_usage_by_period(Subscription#subscription.id, Period, UsageRecords)),
        Cost = calculate_usage_cost(UsageSummary, Subscription#subscription.pricing_plan),

        #{
            subscription_id => Subscription#subscription.id,
            api_id => Subscription#subscription.api_id,
            tier_id => Subscription#subscription.tier_id,
            usage => UsageSummary,
            cost => Cost
        }
    end, Subscriptions),

    Subtotal = lists:sum([Cost || #{cost := Cost} <- InvoiceItems]),
    Tax = calculate_tax(Subtotal),
    Total = Subtotal + Tax,

    InvoiceId = generate_invoice_id(),
    Invoice = #invoice{
        id = InvoiceId,
        consumer_id = ConsumerId,
        period = calculate_billing_period(),
        usage = InvoiceItems,
        subtotal = Subtotal,
        tax = Tax,
        total = Total,
        status = draft
    },

    Invoices = State#{invoices},
    NewInvoices = maps:put(InvoiceId, Invoice, Invoices),

    {reply, {ok, Invoice}, State#{invoices => NewInvoices}};

handle_call({get_billing_info, ConsumerId}, _From, State) ->
    Subscriptions = filter_active_subscriptions(ConsumerId, State#{subscriptions}),
    Invoices = filter_consumer_invoices(ConsumerId, State#{invoices}),

    SubscriptionsInfo = lists:map(fun(Subscription) ->
        Tier = get_tier(Subscription#subscription.tier_id, State#{tiers}),
        #{
            subscription_id => Subscription#subscription.id,
            api_id => Subscription#subscription.api_id,
            tier_id => Subscription#subscription.tier_id,
            tier_name => Tier#tier.name,
            start_date => Subscription#subscription.start_date,
            status => Subscription#subscription.status
        }
    end, Subscriptions),

    InvoicesSummary = lists:map(fun(Invoice) ->
        #{
            invoice_id => Invoice#invoice.id,
            period => Invoice#invoice.period,
            total => Invoice#invoice.total,
            status => Invoice#invoice.status
        }
    end, Invoices),

    BillingInfo = #{
        consumer_id => ConsumerId,
        subscriptions => SubscriptionsInfo,
        invoices => InvoicesSummary,
        total_owed => calculate_total_owed(Invoices)
    },

    {reply, {ok, BillingInfo}, State};

handle_call({set_pricing, PricingSpec}, _From, State) ->
    PricingConfig = maps:get(pricing, PricingSpec),
    PricingId = generate_pricing_id(),

    PricingPlan = #{
        id => PricingId,
        config => PricingConfig,
        created_at => erlang:system_time(millisecond)
    },

    {reply, {ok, PricingPlan}, State};

handle_call({create_promotion, PromotionSpec}, _From, State) ->
    PromotionId = generate_promotion_id(),
    Promotion = #{
        id => PromotionId,
        name => maps:get(name, PromotionSpec),
        description => maps:get(description, PromotionSpec),
        discount_percentage => maps:get(discount_percentage, PromotionSpec),
        duration => maps:get(duration, PromotionSpec),
        start_date => maps:get(start_date, PromotionSpec),
        end_date => maps:get(end_date, PromotionSpec),
        active => true,
        created_at => erlang:system_time(millisecond)
    },

    {reply, {ok, Promotion}, State}.

handle_cast({record_usage, UsageData}, State) ->
    UsageRecord = #usage_record{
        id = generate_usage_id(),
        subscription_id = maps:get(subscription_id, UsageData),
        timestamp = maps:get(timestamp, UsageData),
        request_count = maps:get(request_count, UsageData),
        request_size = maps:get(request_size, UsageData),
        response_size = maps:get(response_size, UsageData),
        cost = calculate_usage_cost(UsageData)
    },

    Usage = State#{usage},
    NewUsage = maps:put(UsageRecord#usage_record.id, UsageRecord, Usage),

    {noreply, State#{usage => NewUsage}}.

handle_info(process_billing, State) ->
    process_monthly_billing(State),
    erlang:send_after(3600000, self(), process_billing),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

find_active_tier(State) ->
    Tiers = State#{tiers},
    ActiveTiers = lists:filter(fun(Tier) ->
        Tier#tier.active =:= true
    end, maps:values(Tiers)),

    case ActiveTiers of
        [Tier | _] -> {ok, Tier};
        [] -> {error, no_tiers}
    end.

generate_pricing_plan(Tier) ->
    #{
        monthly_fee => Tier#tier.monthly_fee,
        per_request_cost => 0.001,
        included_requests => Tier#tier.request_limit,
        included_bandwidth => Tier#tier.rate_limit * 1000000,
        overage_request_cost => 0.005,
        overage_bandwidth_cost => 0.01
    }.

filter_usage_by_period(SubscriptionId, Period, Usage) ->
    lists:filter(fun(Record) ->
        Record#usage_record.subscription_id =:= SubscriptionId andalso
        is_in_period(Record#usage_record.timestamp, Period)
    end, maps:values(Usage)).

summarize_usage(UsageRecords) ->
    TotalRequests = lists:sum([R#usage_record.request_count || R <- UsageRecords]),
    TotalBytesIn = lists:sum([R#usage_record.request_size || R <- UsageRecords]),
    TotalBytesOut = lists:sum([R#usage_record.response_size || R <- UsageRecords]),
    TotalCost = lists:sum([R#usage_record.cost || R <- UsageRecords]),

    #{
        total_requests => TotalRequests,
        total_bytes_in => TotalBytesIn,
        total_bytes_out => TotalBytesOut,
        total_cost => TotalCost,
        record_count => length(UsageRecords)
    }.

calculate_usage_cost(UsageSummary, PricingPlan) ->
    IncludedRequests = maps:get(included_requests, PricingPlan),
    PerRequestCost = maps:get(per_request_cost, PricingPlan),

    TotalRequests = maps:get(total_requests, UsageSummary),
    OverageRequests = max(0, TotalRequests - IncludedRequests),

    BaseCost = maps:get(monthly_fee, PricingPlan),
    OverageCost = OverageRequests * PerRequestCost,

    BaseCost + OverageCost.

calculate_tax(Subtotal) ->
    TaxRate = 0.08,
    Subtotal * TaxRate.

calculate_billing_period(Subscription) ->
    Now = erlang:system_time(millisecond),
    MonthStart = calendar:system_time_to_universal_time(Now, millisecond),
    MonthEnd = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(MonthStart) + 2678400
    ),
    {MonthStart, MonthEnd}.

filter_active_subscriptions(ConsumerId, Subscriptions) ->
    lists:filter(fun(Subscription) ->
        Subscription#subscription.consumer_id =:= ConsumerId andalso
        Subscription#subscription.status =:= active
    end, maps:values(Subscriptions)).

filter_consumer_invoices(ConsumerId, Invoices) ->
    lists:filter(fun(Invoice) ->
        Invoice#invoice.consumer_id =:= ConsumerId
    end, maps:values(Invoices)).

calculate_total_owed(Invoices) ->
    lists:sum([Invoice#invoice.total || Invoice <- Invoices, Invoice#invoice.status =/= paid]).

process_monthly_billing(State) ->
    Consumers = lists:usort([S#subscription.consumer_id || S <- maps:values(State#{subscriptions})]),
    lists:foreach(fun(ConsumerId) ->
        case generate_invoice(ConsumerId) of
            {ok, Invoice} ->
                send_invoice_notification(Invoice);
            {error, _} ->
                ok
        end
    end, Consumers).

send_invoice_notification(Invoice) ->
    io:format("Invoice generated for consumer ~p: ~p~n", [
        Invoice#invoice.consumer_id, Invoice#invoice.total
    ]).

is_in_period(Timestamp, {Start, End}) ->
    Timestamp >= Start andalso Timestamp =< End.

generate_tier_id() ->
    uuid:uuid4().

generate_subscription_id() ->
    uuid:uuid4().

generate_usage_id() ->
    uuid:uuid4().

generate_invoice_id() ->
    uuid:uuid4().

generate_pricing_id() ->
    uuid:uuid4().

generate_promotion_id() ->
    uuid:uuid4().

get_tier(TierId, Tiers) ->
    case maps:find(TierId, Tiers) of
        {ok, Tier} -> Tier;
        error -> undefined
    end.