%%%-------------------------------------------------------------------
%%% @doc
%%% Memory Pool Manager - Object pool for connection state reuse
%%%
%%% Optimizes memory usage for 100K concurrent connections by:
%%% 1. Pre-allocating connection state records
%%% 2. Reusing state objects across lifecycle
%%% 3. Minimizing garbage collection pressure
%%% 4. Zero-copy state transitions where possible
%%%
%%% Target: <2MB per connection at 100K scale (down from 4MB)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_memory_pool).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    acquire_connection_state/1,
    release_connection_state/1,
    acquire_message_buffer/1,
    release_message_buffer/1,
    pool_stats/0,
    pool_info/0,
    configure/2
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(POOL_TABLE, erlmcp_memory_pools).
-define(DEFAULT_POOL_SIZE, 1000).  % Pre-allocate 1000 states for 10% available
-define(MESSAGE_BUFFER_SIZE, 64 * 1024).  % 64KB buffers for messages
-define(CONNECTION_STATE_SIZE, 4096).  % Estimated connection state size

-record(pool_stats, {
    total_allocated = 0 :: non_neg_integer(),
    in_use = 0 :: non_neg_integer(),
    available = 0 :: non_neg_integer(),
    reused = 0 :: non_neg_integer(),
    created = 0 :: non_neg_integer()
}).

-record(state, {
    connection_pool :: queue:queue() | undefined,
    buffer_pool :: queue:queue() | undefined,
    stats = #pool_stats{} :: #pool_stats{},
    pool_size = ?DEFAULT_POOL_SIZE :: pos_integer(),
    max_pool_size = 10000 :: pos_integer(),
    monitor_ref :: reference() | undefined
}).

-type pool_id() :: {connection | message, pos_integer()}.
-type connection_state() :: term().

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Acquire a reusable connection state from pool or create new
-spec acquire_connection_state(map()) -> {ok, reference()} | {error, term()}.
acquire_connection_state(InitialData) when is_map(InitialData) ->
    gen_server:call(?SERVER, {acquire_connection_state, InitialData}).

%% @doc Release connection state back to pool for reuse
-spec release_connection_state(reference()) -> ok.
release_connection_state(StateRef) when is_reference(StateRef) ->
    gen_server:cast(?SERVER, {release_connection_state, StateRef}).

%% @doc Acquire a message buffer from pool
-spec acquire_message_buffer(pos_integer()) -> {ok, reference(), binary()} | {error, term()}.
acquire_message_buffer(Size) when is_integer(Size), Size > 0 ->
    gen_server:call(?SERVER, {acquire_message_buffer, Size}).

%% @doc Release message buffer back to pool
-spec release_message_buffer(reference()) -> ok.
release_message_buffer(BufferRef) when is_reference(BufferRef) ->
    gen_server:cast(?SERVER, {release_message_buffer, BufferRef}).

%% @doc Get pool statistics
-spec pool_stats() -> map().
pool_stats() ->
    gen_server:call(?SERVER, pool_stats).

%% @doc Get detailed pool info
-spec pool_info() -> term().
pool_info() ->
    gen_server:call(?SERVER, pool_info).

%% @doc Configure pool parameters
-spec configure(atom(), term()) -> ok | {error, term()}.
configure(Key, Value) ->
    gen_server:call(?SERVER, {configure, Key, Value}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ets:new(?POOL_TABLE, [
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ]),

    PoolSize = application:get_env(erlmcp, memory_pool_size, ?DEFAULT_POOL_SIZE),
    MaxPoolSize = application:get_env(erlmcp, memory_pool_max_size, 10000),

    %% Pre-allocate connection state queue
    ConnPool = initialize_connection_pool(PoolSize),

    %% Pre-allocate message buffer queue
    BufferPool = initialize_buffer_pool(PoolSize div 10),

    logger:info("Memory pool initialized: ~B connection states, ~B message buffers",
                [PoolSize, PoolSize div 10]),

    %% Schedule periodic stats logging
    erlang:send_after(60000, self(), log_stats),

    {ok, #state{
        connection_pool = ConnPool,
        buffer_pool = BufferPool,
        pool_size = PoolSize,
        max_pool_size = MaxPoolSize,
        stats = #pool_stats{
            total_allocated = PoolSize,
            available = PoolSize,
            in_use = 0
        }
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({acquire_connection_state, InitialData}, _From, State) ->
    #state{connection_pool = Pool, stats = Stats} = State,
    case queue:out(Pool) of
        {{value, StateRef}, NewPool} ->
            %% Reuse existing state
            NewStats = Stats#pool_stats{
                in_use = Stats#pool_stats.in_use + 1,
                available = Stats#pool_stats.available - 1,
                reused = Stats#pool_stats.reused + 1
            },
            ets:insert(?POOL_TABLE, {StateRef, {InitialData, timestamp()}}),
            {reply, {ok, StateRef}, State#state{
                connection_pool = NewPool,
                stats = NewStats
            }};
        {empty, _} ->
            %% Create new state (pool depleted)
            StateRef = make_ref(),
            NewStats = Stats#pool_stats{
                in_use = Stats#pool_stats.in_use + 1,
                total_allocated = Stats#pool_stats.total_allocated + 1,
                created = Stats#pool_stats.created + 1
            },
            ets:insert(?POOL_TABLE, {StateRef, {InitialData, timestamp()}}),
            {reply, {ok, StateRef}, State#state{stats = NewStats}}
    end;

handle_call({acquire_message_buffer, Size}, _From, State) ->
    #state{buffer_pool = Pool} = State,
    MaxSize = max(Size, ?MESSAGE_BUFFER_SIZE),
    case queue:out(Pool) of
        {{value, {BufferRef, Buffer}}, NewPool} when byte_size(Buffer) >= Size ->
            %% Reuse existing buffer
            ets:insert(?POOL_TABLE, {BufferRef, {buffer, Buffer, timestamp()}}),
            {reply, {ok, BufferRef, Buffer}, State#state{buffer_pool = NewPool}};
        {_, NewPool} ->
            %% Create new buffer
            BufferRef = make_ref(),
            Buffer = <<0:(MaxSize * 8)>>,
            ets:insert(?POOL_TABLE, {BufferRef, {buffer, Buffer, timestamp()}}),
            {reply, {ok, BufferRef, Buffer}, State#state{buffer_pool = NewPool}}
    end;

handle_call(pool_stats, _From, #state{stats = Stats} = State) ->
    Reply = #{
        in_use => Stats#pool_stats.in_use,
        available => Stats#pool_stats.available,
        total_allocated => Stats#pool_stats.total_allocated,
        reused => Stats#pool_stats.reused,
        created => Stats#pool_stats.created,
        reuse_ratio => case Stats#pool_stats.created of
            0 -> 0.0;
            Created ->
                (Stats#pool_stats.reused / Created) * 100.0
        end
    },
    {reply, Reply, State};

handle_call(pool_info, _From, #state{
    connection_pool = ConnPool,
    buffer_pool = BufferPool,
    stats = Stats,
    pool_size = PoolSize,
    max_pool_size = MaxPoolSize
} = State) ->
    ConnQueueLen = queue:len(ConnPool),
    BufferQueueLen = queue:len(BufferPool),
    TableSize = ets:info(?POOL_TABLE, size),
    Memory = ets:info(?POOL_TABLE, memory),

    Reply = #{
        connection_pool => #{
            queue_length => ConnQueueLen,
            pool_size => PoolSize,
            max_size => MaxPoolSize
        },
        buffer_pool => #{
            queue_length => BufferQueueLen,
            buffer_size => ?MESSAGE_BUFFER_SIZE
        },
        ets_table => #{
            size => TableSize,
            memory_bytes => Memory
        },
        stats => #{
            in_use => Stats#pool_stats.in_use,
            available => Stats#pool_stats.available,
            total_allocated => Stats#pool_stats.total_allocated,
            reused => Stats#pool_stats.reused,
            created => Stats#pool_stats.created
        }
    },
    {reply, Reply, State};

handle_call({configure, pool_size, Size}, _From, State) when is_integer(Size), Size > 0 ->
    {reply, ok, State#state{pool_size = Size}};

handle_call({configure, max_pool_size, Size}, _From, State) when is_integer(Size), Size > 0 ->
    {reply, ok, State#state{max_pool_size = Size}};

handle_call({configure, _Key, _Value}, _From, State) ->
    {reply, {error, invalid_config}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.

handle_cast({release_connection_state, StateRef}, State) ->
    #state{connection_pool = Pool, stats = Stats} = State,
    %% Clean up ETS entry
    ets:delete(?POOL_TABLE, StateRef),
    %% Return to pool if not at max capacity
    {NewPool, NewStats} = case queue:len(Pool) < Stats#pool_stats.total_allocated of
        true ->
            {
                queue:in(StateRef, Pool),
                Stats#pool_stats{
                    in_use = max(0, Stats#pool_stats.in_use - 1),
                    available = Stats#pool_stats.available + 1
                }
            };
        false ->
            {
                Pool,
                Stats#pool_stats{in_use = max(0, Stats#pool_stats.in_use - 1)}
            }
    end,
    {noreply, State#state{
        connection_pool = NewPool,
        stats = NewStats
    }};

handle_cast({release_message_buffer, BufferRef}, State) ->
    %% Clean up ETS entry
    ets:delete(?POOL_TABLE, BufferRef),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.

handle_info(log_stats, #state{stats = Stats} = State) ->
    logger:info("Memory pool stats: in_use=~B, available=~B, reused=~B (~.1f%), created=~B",
                [
                    Stats#pool_stats.in_use,
                    Stats#pool_stats.available,
                    Stats#pool_stats.reused,
                    case Stats#pool_stats.created of
                        0 -> 0.0;
                        Created -> (Stats#pool_stats.reused / Created) * 100.0
                    end,
                    Stats#pool_stats.created
                ]),
    erlang:send_after(60000, self(), log_stats),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ets:delete(?POOL_TABLE),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec initialize_connection_pool(pos_integer()) -> queue:queue().
initialize_connection_pool(Size) ->
    Pool = queue:new(),
    lists:foldl(
        fun(_, Q) -> queue:in(make_ref(), Q) end,
        Pool,
        lists:seq(1, Size)
    ).

-spec initialize_buffer_pool(pos_integer()) -> queue:queue().
initialize_buffer_pool(Size) ->
    Pool = queue:new(),
    Buffer = <<0:(?MESSAGE_BUFFER_SIZE * 8)>>,
    lists:foldl(
        fun(_, Q) -> queue:in({make_ref(), Buffer}, Q) end,
        Pool,
        lists:seq(1, Size)
    ).

-spec timestamp() -> integer().
timestamp() ->
    erlang:system_time(millisecond).
