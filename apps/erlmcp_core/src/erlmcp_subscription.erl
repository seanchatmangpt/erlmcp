-module(erlmcp_subscription).

-include("erlmcp.hrl").

%% API exports
-export([
    subscribe/2,
    unsubscribe/2,
    list_subscribers/1,
    notify/2,
    notify/3
]).

%% Types
-type subscription_id() :: binary().
-type subscriber() :: pid().

-export_type([subscription_id/0, subscriber/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec subscribe(subscription_id(), subscriber()) -> ok | {error, term()}.
subscribe(_SubscriptionId, _Subscriber) ->
    %% TODO: Implement subscription management
    ok.

-spec unsubscribe(subscription_id(), subscriber()) -> ok | {error, term()}.
unsubscribe(_SubscriptionId, _Subscriber) ->
    %% TODO: Implement unsubscription
    ok.

-spec list_subscribers(subscription_id()) -> [subscriber()].
list_subscribers(_SubscriptionId) ->
    %% TODO: Implement subscriber listing
    [].

-spec notify(subscription_id(), term()) -> ok.
notify(SubscriptionId, Message) ->
    notify(SubscriptionId, Message, #{}).

-spec notify(subscription_id(), term(), map()) -> ok.
notify(_SubscriptionId, _Message, _Options) ->
    %% TODO: Implement notification dispatch
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================
