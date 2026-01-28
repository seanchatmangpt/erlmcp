%%%===================================================================
%%% erlmcp_buffer_pool.erl - Zero-copy buffer pool for TCP transport
%%%===================================================================
%%%
%%% High-performance buffer pool supporting 4KB/8KB/16KB tiers.
%%% Minimizes allocation overhead and GC pressure on hot path.
%%%
%%% Key optimizations:
%%% - Pre-allocated pools by size tier (4KB, 8KB, 16KB)
%%% - Process-local buffer caching (no lock contention)
%%% - Automatic tier sizing based on payload
%%% - Configurable pool sizes and growth
%%%
-module(erlmcp_buffer_pool).

%% Public API
-export([
    start_link/1,
    get_buffer/2,
    get_buffer/1,
    return_buffer/2,
    stats/1,
    info/1
]).

%% Gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal API for process-local optimization
-export([init_process_cache/0, cache_get/1, cache_return/2, cache_stats/0]).

-behaviour(gen_server).

%%====================================================================
%% Records and Types
%%====================================================================

-record(pool_tier, {
    size :: pos_integer(),
    buffers :: queue:queue(binary()),
    count :: non_neg_integer(),
    allocated :: non_neg_integer(),
    max_allocated :: pos_integer(),
    hits :: non_neg_integer(),
    misses :: non_neg_integer()
}).

-record(state, {
    tiers :: #{pos_integer() => #pool_tier{}},
    config :: map(),
    total_allocations :: non_neg_integer(),
    total_deallocations :: non_neg_integer()
}).

%% Pool cache key
-define(CACHE_KEY, erlmcp_buffer_cache).

%%====================================================================
%% Configuration Constants
%%====================================================================

%% Standard buffer sizes (tiers)
-define(TIER_4KB, 4096).
-define(TIER_8KB, 8192).
-define(TIER_16KB, 16384).

%% Default pool sizes
-define(DEFAULT_4KB_SIZE, 256).
-define(DEFAULT_8KB_SIZE, 128).
-define(DEFAULT_16KB_SIZE, 64).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Start buffer pool with config
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%% @doc Get buffer from pool, choosing tier automatically
-spec get_buffer(pos_integer()) -> binary().
get_buffer(Size) ->
    case cache_get(Size) of
        {ok, Buffer} ->
            Buffer;
        error ->
            get_buffer_from_pool(Size)
    end.

%% @doc Get buffer from specific tier
-spec get_buffer(atom(), pos_integer()) -> binary().
get_buffer(Tier, _Size) ->
    case cache_get(Tier) of
        {ok, Buffer} ->
            Buffer;
        error ->
            get_buffer_from_pool(Tier)
    end.

%% @doc Return buffer to pool
-spec return_buffer(pos_integer(), binary()) -> ok.
return_buffer(Size, Buffer) when is_binary(Buffer), is_integer(Size) ->
    Tier = select_tier(Size),
    case cache_return(Tier, Buffer) of
        ok -> ok;
        error -> return_buffer_to_pool(Tier, Buffer)
    end.

%% @doc Get pool statistics
-spec stats(pid()) -> map().
stats(Pid) ->
    gen_server:call(Pid, stats).

%% @doc Get pool info
-spec info(pid()) -> map().
info(Pid) ->
    gen_server:call(Pid, info).

%%====================================================================
%% Process-local Cache (Fast Path)
%%====================================================================

%% @doc Initialize process-local buffer cache
-spec init_process_cache() -> ok.
init_process_cache() ->
    erlang:put(?CACHE_KEY, #{
        '4kb' => [],
        '8kb' => [],
        '16kb' => []
    }),
    ok.

%% @doc Try to get buffer from process-local cache (no lock!)
-spec cache_get(pos_integer() | atom()) -> {ok, binary()} | error.
cache_get(Size) when is_integer(Size) ->
    Tier = select_tier(Size),
    cache_get(Tier);
cache_get(Tier) when is_atom(Tier) ->
    case erlang:get(?CACHE_KEY) of
        undefined ->
            error;
        Cache ->
            case maps:get(Tier, Cache, []) of
                [] ->
                    error;
                [Buffer | Rest] ->
                    erlang:put(?CACHE_KEY, Cache#{Tier := Rest}),
                    {ok, Buffer}
            end
    end.

%% @doc Try to return buffer to process-local cache
-spec cache_return(atom(), binary()) -> ok | error.
cache_return(Tier, Buffer) when is_atom(Tier), is_binary(Buffer) ->
    case erlang:get(?CACHE_KEY) of
        undefined ->
            error;
        Cache ->
            Current = maps:get(Tier, Cache, []),
            %% Limit cache size to prevent unbounded growth
            case length(Current) < 16 of
                true ->
                    erlang:put(?CACHE_KEY, Cache#{Tier := [Buffer | Current]}),
                    ok;
                false ->
                    error
            end
    end.

%% @doc Get cache statistics
-spec cache_stats() -> map().
cache_stats() ->
    case erlang:get(?CACHE_KEY) of
        undefined ->
            #{};
        Cache ->
            maps:map(fun(_K, V) -> length(V) end, Cache)
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(Config) ->
    logger:info("Buffer pool starting with config: ~p", [Config]),

    Tiers = #{
        ?TIER_4KB => #pool_tier{
            size = ?TIER_4KB,
            buffers = queue:new(),
            count = 0,
            allocated = 0,
            max_allocated = maps:get(max_4kb, Config, ?DEFAULT_4KB_SIZE),
            hits = 0,
            misses = 0
        },
        ?TIER_8KB => #pool_tier{
            size = ?TIER_8KB,
            buffers = queue:new(),
            count = 0,
            allocated = 0,
            max_allocated = maps:get(max_8kb, Config, ?DEFAULT_8KB_SIZE),
            hits = 0,
            misses = 0
        },
        ?TIER_16KB => #pool_tier{
            size = ?TIER_16KB,
            buffers = queue:new(),
            count = 0,
            allocated = 0,
            max_allocated = maps:get(max_16kb, Config, ?DEFAULT_16KB_SIZE),
            hits = 0,
            misses = 0
        }
    },

    %% Pre-allocate buffers if requested
    State = case maps:get(preallocate, Config, false) of
        true ->
            preallocate_pools(#state{
                tiers = Tiers,
                config = Config,
                total_allocations = 0,
                total_deallocations = 0
            });
        false ->
            #state{
                tiers = Tiers,
                config = Config,
                total_allocations = 0,
                total_deallocations = 0
            }
    end,

    {ok, State}.

handle_call(stats, _From, State) ->
    Stats = maps:fold(fun(Size, Tier, Acc) ->
        Acc#{Size => #{
            count => Tier#pool_tier.count,
            allocated => Tier#pool_tier.allocated,
            max_allocated => Tier#pool_tier.max_allocated,
            hits => Tier#pool_tier.hits,
            misses => Tier#pool_tier.misses
        }}
    end, #{}, State#state.tiers),
    {reply, Stats, State};

handle_call(info, _From, State) ->
    Info = #{
        total_allocations => State#state.total_allocations,
        total_deallocations => State#state.total_deallocations,
        tiers => maps:size(State#state.tiers),
        config => State#state.config
    },
    {reply, Info, State};

handle_call({get_buffer, Tier}, _From, State) ->
    {Buffer, NewState} = get_buffer_internal(Tier, State),
    {reply, Buffer, NewState};

handle_call({return_buffer, Tier, Buffer}, _From, State) ->
    NewState = return_buffer_internal(Tier, Buffer, State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Select appropriate tier for given size
-spec select_tier(pos_integer()) -> atom().
select_tier(Size) when Size =< ?TIER_4KB ->
    '4kb';
select_tier(Size) when Size =< ?TIER_8KB ->
    '8kb';
select_tier(_Size) ->
    '16kb'.

%% @doc Get buffer from pool (called from fast path)
-spec get_buffer_from_pool(pos_integer() | atom()) -> binary().
get_buffer_from_pool(Size) when is_integer(Size) ->
    TierKey = select_tier(Size),
    get_buffer_from_pool(TierKey);
get_buffer_from_pool(Tier) when is_atom(Tier) ->
    case gen_server:call(?MODULE, {get_buffer, Tier}) of
        {ok, Buffer} ->
            Buffer;
        error ->
            %% Allocate new buffer if pool exhausted
            Size = tier_to_size(Tier),
            binary:copy(<<0:Size/unit:8>>)
    end.

%% @doc Return buffer to pool
-spec return_buffer_to_pool(atom(), binary()) -> ok.
return_buffer_to_pool(Tier, Buffer) ->
    gen_server:cast(?MODULE, {return_buffer, Tier, Buffer}),
    ok.

%% @doc Get buffer from internal pool
-spec get_buffer_internal(atom(), #state{}) -> {binary() | error, #state{}}.
get_buffer_internal(Tier, State) ->
    case maps:get(Tier, State#state.tiers, undefined) of
        undefined ->
            {error, State};
        PoolTier ->
            case queue:out(PoolTier#pool_tier.buffers) of
                {{value, Buffer}, NewQ} ->
                    UpdatedTier = PoolTier#pool_tier{
                        buffers = NewQ,
                        count = PoolTier#pool_tier.count - 1,
                        hits = PoolTier#pool_tier.hits + 1
                    },
                    NewState = State#state{
                        tiers = maps:put(Tier, UpdatedTier, State#state.tiers),
                        total_allocations = State#state.total_allocations + 1
                    },
                    {Buffer, NewState};
                {empty, _} ->
                    %% Allocate new buffer
                    Size = tier_to_size(Tier),
                    Buffer = binary:copy(<<0:Size/unit:8>>),
                    UpdatedTier = PoolTier#pool_tier{
                        allocated = PoolTier#pool_tier.allocated + 1,
                        misses = PoolTier#pool_tier.misses + 1
                    },
                    NewState = State#state{
                        tiers = maps:put(Tier, UpdatedTier, State#state.tiers),
                        total_allocations = State#state.total_allocations + 1
                    },
                    {Buffer, NewState}
            end
    end.

%% @doc Return buffer to internal pool
-spec return_buffer_internal(atom(), binary(), #state{}) -> #state{}.
return_buffer_internal(Tier, Buffer, State) ->
    case maps:get(Tier, State#state.tiers, undefined) of
        undefined ->
            State;
        PoolTier ->
            case PoolTier#pool_tier.count < PoolTier#pool_tier.max_allocated of
                true ->
                    UpdatedTier = PoolTier#pool_tier{
                        buffers = queue:in(Buffer, PoolTier#pool_tier.buffers),
                        count = PoolTier#pool_tier.count + 1
                    },
                    State#state{
                        tiers = maps:put(Tier, UpdatedTier, State#state.tiers),
                        total_deallocations = State#state.total_deallocations + 1
                    };
                false ->
                    %% Pool full, discard buffer
                    State#state{total_deallocations = State#state.total_deallocations + 1}
            end
    end.

%% @doc Convert tier name to size in bytes
-spec tier_to_size(atom()) -> pos_integer().
tier_to_size('4kb') -> ?TIER_4KB;
tier_to_size('8kb') -> ?TIER_8KB;
tier_to_size('16kb') -> ?TIER_16KB;
tier_to_size(_) -> ?TIER_4KB.

%% @doc Pre-allocate buffers for all tiers
-spec preallocate_pools(#state{}) -> #state{}.
preallocate_pools(State) ->
    maps:fold(fun(Size, PoolTier, Acc) ->
        Count = PoolTier#pool_tier.max_allocated,
        NewQ = lists:foldl(fun(_I, Q) ->
            Buffer = binary:copy(<<0:Size/unit:8>>),
            queue:in(Buffer, Q)
        end, PoolTier#pool_tier.buffers, lists:seq(1, Count)),

        UpdatedTier = PoolTier#pool_tier{
            buffers = NewQ,
            count = Count,
            allocated = Count
        },

        Acc#state{tiers = maps:put(Size, UpdatedTier, Acc#state.tiers)}
    end, State, State#state.tiers).
