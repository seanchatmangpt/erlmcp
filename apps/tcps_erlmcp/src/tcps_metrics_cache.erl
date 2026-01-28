%%%-------------------------------------------------------------------
%%% @doc TCPS Metrics Cache - ETS-based caching layer
%%%
%%% Provides high-performance caching for frequently accessed metrics
%%% with TTL-based expiration.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_metrics_cache).

%% API
-export([init_cache/0, destroy_cache/0]).
-export([get_cached/1, update_cache/2, invalidate/1, invalidate_all/0]).
-export([get_stats/0]).

-include_lib("kernel/include/logger.hrl").

-define(CACHE_TABLE, tcps_metrics_cache).
-define(DEFAULT_TTL, 60). %% 60 seconds

-type cache_key() :: atom() | binary().
-type cache_value() :: term().
-type cache_entry() :: {cache_key(), cache_value(), integer()}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Initialize cache ETS table
-spec init_cache() -> ok.
init_cache() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            ets:new(?CACHE_TABLE, [
                named_table,
                public,
                set,
                {write_concurrency, true},
                {read_concurrency, true}
            ]),
            ?LOG_INFO("TCPS Metrics Cache initialized"),
            ok;
        _ ->
            ?LOG_DEBUG("TCPS Metrics Cache already exists"),
            ok
    end.

%% @doc Destroy cache ETS table
-spec destroy_cache() -> ok.
destroy_cache() ->
    case ets:info(?CACHE_TABLE) of
        undefined -> ok;
        _ ->
            ets:delete(?CACHE_TABLE),
            ok
    end.

%% @doc Get cached value with TTL check
-spec get_cached(cache_key()) -> {ok, cache_value()} | {error, not_found | expired}.
get_cached(Key) ->
    case ets:lookup(?CACHE_TABLE, Key) of
        [{Key, Value, Timestamp}] ->
            Now = erlang:system_time(second),
            Age = Now - Timestamp,

            if
                Age < ?DEFAULT_TTL ->
                    {ok, Value};
                true ->
                    %% Expired, remove from cache
                    ets:delete(?CACHE_TABLE, Key),
                    {error, expired}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Update cache with new value
-spec update_cache(cache_key(), cache_value()) -> ok.
update_cache(Key, Value) ->
    Timestamp = erlang:system_time(second),
    ets:insert(?CACHE_TABLE, {Key, Value, Timestamp}),
    ok.

%% @doc Invalidate (remove) specific cache entry
-spec invalidate(cache_key()) -> ok.
invalidate(Key) ->
    ets:delete(?CACHE_TABLE, Key),
    ok.

%% @doc Invalidate all cache entries
-spec invalidate_all() -> ok.
invalidate_all() ->
    ets:delete_all_objects(?CACHE_TABLE),
    ok.

%% @doc Get cache statistics
-spec get_stats() -> map().
get_stats() ->
    case ets:info(?CACHE_TABLE) of
        undefined ->
            #{error => cache_not_initialized};
        Info ->
            Now = erlang:system_time(second),

            %% Get all entries
            AllEntries = ets:tab2list(?CACHE_TABLE),

            %% Count valid vs expired
            {ValidCount, ExpiredCount} = lists:foldl(fun({_Key, _Value, Timestamp}, {Valid, Expired}) ->
                Age = Now - Timestamp,
                if
                    Age < ?DEFAULT_TTL -> {Valid + 1, Expired};
                    true -> {Valid, Expired + 1}
                end
            end, {0, 0}, AllEntries),

            %% Clean up expired entries
            lists:foreach(fun({Key, _Value, Timestamp}) ->
                Age = Now - Timestamp,
                if
                    Age >= ?DEFAULT_TTL ->
                        ets:delete(?CACHE_TABLE, Key);
                    true ->
                        ok
                end
            end, AllEntries),

            #{
                total_entries => proplists:get_value(size, Info, 0),
                valid_entries => ValidCount,
                expired_entries => ExpiredCount,
                memory_words => proplists:get_value(memory, Info, 0),
                ttl_seconds => ?DEFAULT_TTL
            }
    end.
