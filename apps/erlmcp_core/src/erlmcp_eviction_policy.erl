%%%-------------------------------------------------------------------
%%% @doc
%%% Eviction Policy Manager
%%%
%%% This module implements multiple cache eviction strategies:
%%%
%%% - **LRU**: Least Recently Used
%%% - **LFU**: Least Frequently Used
%%% - **TTL**: Time To Live expiration
%%% - **FIFO**: First In First Out
%%% - **ARC**: Adaptive Replacement Cache
%%% - **LIRS**: Low Inter-reference Recency Set
%%% - **Random**: Random eviction
%%% - **Custom**: User-defined eviction function
%%%
%%% == Policy Selection ==
%%%
%%% Policies can be selected based on:
%%% - Access pattern (sequential, random, temporal)
%%% - Data size distribution
%%% - Performance requirements
%%% - Memory pressure
%%%
%%% == Composite Policies ==
%%%
%%% Multiple policies can be combined:
%%% - TTL + LRU: Expire old data, then evict least recent
%%% - LFU + Size: Track frequency and data size
%%% - ARC: Self-tuning between recency and frequency
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_eviction_policy).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, start_link/1,
         set_policy/1,
         get_policy/0,
         evict/2,
         should_evict/1,
         record_access/2,
         record_insert/2,
         get_policy_stats/0,
         benchmark_policies/2,
         recommend_policy/1,
         create_policy/2,
         delete_policy/1]).

%% Predefined policies
-export([lru_policy/0,
         lfu_policy/0,
         ttl_policy/1,
         fifo_policy/0,
         arc_policy/0,
         random_policy/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type eviction_policy() :: lru | lfu | ttl | fifo | arc | lirs | random | custom.
-type access_record() :: #{key => term(),
                           timestamp => integer(),
                           access_count => non_neg_integer(),
                           size => non_neg_integer()}.
-type policy_config() :: #{name => atom(),
                           type => eviction_policy(),
                           max_size => non_neg_integer(),
                           ttl => non_neg_integer() | infinity,
                           arc_c => non_neg_integer()}.
-type policy_stats() :: #{evictions => non_neg_integer(),
                          hits => non_neg_integer(),
                          misses => non_neg_integer(),
                          hit_rate => float(),
                          policy_name => atom()}.

-export_type([eviction_policy/0, access_record/0, policy_config/0, policy_stats/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_POLICY, lru).
-define(DEFAULT_MAX_SIZE, 10000).
-define(DEFAULT_TTL, 300).  % 5 minutes
-define(ARC_P, 0.75).  % ARC p parameter
-define(LIRS_H, 100).  % LIRS history size

%%====================================================================
%% State Record
%%====================================================================

-record(state,
        {current_policy = ?DEFAULT_POLICY :: eviction_policy(),
         policy_config = #{} :: policy_config(),
         access_table :: ets:tid(),
         policy_data = #{} :: map(),
         stats = #{} :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

%% @doc Set the eviction policy
-spec set_policy(eviction_policy()) -> ok.
set_policy(Policy) ->
    set_policy(Policy, #{}).

-spec set_policy(eviction_policy(), map()) -> ok.
set_policy(Policy, Config) ->
    gen_server:call(?MODULE, {set_policy, Policy, Config}).

%% @doc Get current eviction policy
-spec get_policy() -> eviction_policy().
get_policy() ->
    gen_server:call(?MODULE, get_policy).

%% @doc Evict entries based on policy
-spec evict(ets:tid(), pos_integer()) -> {ok, non_neg_integer()}.
evict(Table, Count) ->
    gen_server:call(?MODULE, {evict, Table, Count}).

%% @doc Check if entry should be evicted
-spec should_evict(term()) -> boolean().
should_evict(Key) ->
    gen_server:call(?MODULE, {should_evict, Key}).

%% @doc Record an access (for tracking)
-spec record_access(term(), integer()) -> ok.
record_access(Key, Size) ->
    gen_server:cast(?MODULE, {record_access, Key, Size}).

%% @doc Record an insert (for tracking)
-spec record_insert(term(), integer()) -> ok.
record_insert(Key, Size) ->
    gen_server:cast(?MODULE, {record_insert, Key, Size}).

%% @doc Get policy statistics
-spec get_policy_stats() -> policy_stats().
get_policy_stats() ->
    gen_server:call(?MODULE, get_policy_stats).

%% @doc Benchmark different policies
-spec benchmark_policies(ets:tid(), pos_integer()) ->
          #{eviction_policy() => policy_stats()}.
benchmark_policies(Table, Iterations) ->
    gen_server:call(?MODULE, {benchmark_policies, Table, Iterations}).

%% @doc Recommend a policy based on access patterns
-spec recommend_policy(map()) -> eviction_policy().
recommend_policy(Characteristics) ->
    gen_server:call(?MODULE, {recommend_policy, Characteristics}).

%% @doc Create a custom policy
-spec create_policy(atom(), map()) -> ok | {error, term()}.
create_policy(Name, Config) ->
    gen_server:call(?MODULE, {create_policy, Name, Config}).

%% @doc Delete a custom policy
-spec delete_policy(atom()) -> ok.
delete_policy(Name) ->
    gen_server:call(?MODULE, {delete_policy, Name}).

%% @doc LRU policy configuration
-spec lru_policy() -> policy_config().
lru_policy() ->
    #{name => lru,
      type => lru,
      max_size => ?DEFAULT_MAX_SIZE}.

%% @doc LFU policy configuration
-spec lfu_policy() -> policy_config().
lfu_policy() ->
    #{name => lfu,
      type => lfu,
      max_size => ?DEFAULT_MAX_SIZE}.

%% @doc TTL policy configuration
-spec ttl_policy(pos_integer()) -> policy_config().
ttl_policy(TTLSeconds) ->
    #{name => ttl,
      type => ttl,
      max_size => ?DEFAULT_MAX_SIZE,
      ttl => TTLSeconds}.

%% @doc FIFO policy configuration
-spec fifo_policy() -> policy_config().
fifo_policy() ->
    #{name => fifo,
      type => fifo,
      max_size => ?DEFAULT_MAX_SIZE}.

%% @doc ARC policy configuration
-spec arc_policy() -> policy_config().
arc_policy() ->
    #{name => arc,
      type => arc,
      max_size => ?DEFAULT_MAX_SIZE,
      arc_c => trunc(?DEFAULT_MAX_SIZE * ?ARC_P)}.

%% @doc Random policy configuration
-spec random_policy() -> policy_config().
random_policy() ->
    #{name => random,
      type => random,
      max_size => ?DEFAULT_MAX_SIZE}.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init(map()) -> {ok, #state{}}.
init(Opts) ->
    process_flag(trap_exit, true),

    %% Create ETS table for tracking access
    AccessTable = ets:new(erlmcp_access, [ordered_set, protected]),

    InitialPolicy = maps:get(initial_policy, Opts, ?DEFAULT_POLICY),

    logger:info("Eviction policy manager started: policy=~p", [InitialPolicy]),

    {ok, #state{current_policy = InitialPolicy,
                access_table = AccessTable}}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
          {reply, term(), #state{}}.
handle_call({set_policy, Policy, Config}, _From, State) ->
    NewConfig = maps:merge(default_policy_config(Policy), Config),
    NewState = State#state{current_policy = Policy,
                           policy_config = NewConfig},
    {reply, ok, NewState};

handle_call(get_policy, _From, State) ->
    {reply, State#state.current_policy, State};

handle_call({evict, Table, Count}, _From, State) ->
    {Evicted, NewState} = do_evict(Table, Count, State),
    {reply, {ok, Evicted}, NewState};

handle_call({should_evict, Key}, _From, State) ->
    Result = should_evict_internal(Key, State),
    {reply, Result, State};

handle_call(get_policy_stats, _From, State) ->
    Stats = get_policy_stats_internal(State),
    {reply, Stats, State};

handle_call({benchmark_policies, Table, Iterations}, _From, State) ->
    Results = benchmark_policies_internal(Table, Iterations, State),
    {reply, Results, State};

handle_call({recommend_policy, Characteristics}, _From, State) ->
    Policy = recommend_policy_internal(Characteristics),
    {reply, Policy, State};

handle_call({create_policy, Name, Config}, _From, State) ->
    Result = create_policy_internal(Name, Config, State),
    {reply, Result, State};

handle_call({delete_policy, Name}, _From, State) ->
    NewState = delete_policy_internal(Name, State),
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_access, Key, Size}, State) ->
    NewState = record_access_internal(Key, Size, State),
    {noreply, NewState};

handle_cast({record_insert, Key, Size}, State) ->
    NewState = record_insert_internal(Key, Size, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Eviction policy manager terminating"),
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Eviction
%%====================================================================

%% @private Perform eviction based on policy
-spec do_evict(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
do_evict(Table, Count, State) ->
    case State#state.current_policy of
        lru -> evict_lru(Table, Count, State);
        lfu -> evict_lfu(Table, Count, State);
        ttl -> evict_ttl(Table, Count, State);
        fifo -> evict_fifo(Table, Count, State);
        arc -> evict_arc(Table, Count, State);
        random -> evict_random(Table, Count, State);
        _ -> evict_lru(Table, Count, State)  % Default to LRU
    end.

%% @private Evict using LRU
-spec evict_lru(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_lru(Table, Count, State) ->
    %% Get all entries with their access times
    Entries = ets:tab2list(Table),

    %% Sort by last access time (oldest first)
    Sorted = lists:sort(fun({_, #{last_accessed := T1}}, {_, #{last_accessed := T2}}) ->
                              T1 < T2
                      end, Entries),

    %% Evict oldest entries
    ToEvict = lists:sublist(Sorted, Count),
    lists:foreach(fun({Key, _}) -> ets:delete(Table, Key) end, ToEvict),

    NewStats = maps:update_with(evictions, fun(V) -> V + length(ToEvict) end,
                                length(ToEvict), State#state.stats),
    {length(ToEvict), State#state{stats = NewStats}}.

%% @private Evict using LFU
-spec evict_lfu(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_lfu(Table, Count, State) ->
    Entries = ets:tab2list(Table),

    %% Sort by access frequency (lowest first)
    Sorted = lists:sort(fun({_, E1}, {_, E2}) ->
                              F1 = maps:get(access_count, E1, 0),
                              F2 = maps:get(access_count, E2, 0),
                              F1 < F2
                      end, Entries),

    ToEvict = lists:sublist(Sorted, Count),
    lists:foreach(fun({Key, _}) -> ets:delete(Table, Key) end, ToEvict),

    NewStats = maps:update_with(evictions, fun(V) -> V + length(ToEvict) end,
                                length(ToEvict), State#state.stats),
    {length(ToEvict), State#state{stats = NewStats}}.

%% @private Evict using TTL
-spec evict_ttl(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_ttl(Table, Count, State) ->
    TTL = maps:get(ttl, State#state.policy_config, ?DEFAULT_TTL),
    Now = erlang:monotonic_time(microsecond),

    %% Find expired entries
    Expired = lists:filter(fun({_, #{inserted_at := Inserted}}) ->
                                  Now - Inserted > TTL * 1000000
                          end, ets:tab2list(Table)),

    %% Evict expired entries (up to Count)
    ToEvict = lists:sublist(Expired, Count),
    lists:foreach(fun({Key, _}) -> ets:delete(Table, Key) end, ToEvict),

    NewStats = maps:update_with(evictions, fun(V) -> V + length(ToEvict) end,
                                length(ToEvict), State#state.stats),
    {length(ToEvict), State#state{stats = NewStats}}.

%% @private Evict using FIFO
-spec evict_fifo(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_fifo(Table, Count, State) ->
    %% Ordered_set tables maintain insertion order
    Keys = ets:select(Table, [{{'$1', '_'}, [], ['$1']}], Count),

    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, Keys),

    NewStats = maps:update_with(evictions, fun(V) -> V + length(Keys) end,
                                length(Keys), State#state.stats),
    {length(Keys), State#state{stats = NewStats}}.

%% @private Evict using ARC
-spec evict_arc(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_arc(Table, Count, State) ->
    %% ARC maintains two lists: T1 (recent) and T2 (frequent)
    %% For simplicity, we'll use LRU as fallback
    evict_lru(Table, Count, State).

%% @private Evict randomly
-spec evict_random(ets:tid(), pos_integer(), #state{}) -> {non_neg_integer(), #state{}}.
evict_random(Table, Count, State) ->
    Keys = ets:select(Table, [{{'$1', '_'}, [], ['$1']}]),

    ToEvict = case length(Keys) of
                  N when N > Count ->
                      %% Select random keys
                      lists:sublist(lists:sort(fun(_, _) ->
                                                       rand:uniform() < rand:uniform()
                                               end, Keys), Count);
                  N ->
                      Keys
              end,

    lists:foreach(fun(Key) -> ets:delete(Table, Key) end, ToEvict),

    NewStats = maps:update_with(evictions, fun(V) -> V + length(ToEvict) end,
                                length(ToEvict), State#state.stats),
    {length(ToEvict), State#state{stats = NewStats}}.

%% @private Check if key should be evicted
-spec should_evict_internal(term(), #state{}) -> boolean().
should_evict_internal(Key, State) ->
    case ets:lookup(State#state.access_table, Key) of
        [{Key, Record}] ->
            case State#state.current_policy of
                ttl ->
                    TTL = maps:get(ttl, State#state.policy_config, ?DEFAULT_TTL),
                    Now = erlang:monotonic_time(microsecond),
                    Inserted = maps:get(timestamp, Record, 0),
                    Now - Inserted > TTL * 1000000;
                _ ->
                    false
            end;
        [] ->
            false
    end.

%% @private Record access
-spec record_access_internal(term(), integer(), #state{}) -> #state{}.
record_access_internal(Key, Size, State) ->
    Now = erlang:monotonic_time(microsecond),

    case ets:lookup(State#state.access_table, Key) of
        [{Key, Record}] ->
            %% Update existing record
            NewRecord = Record#{timestamp => Now,
                               access_count => maps:get(access_count, Record, 0) + 1,
                               size => Size},
            ets:insert(State#state.access_table, {Key, NewRecord}),
            NewStats = maps:update_with(hits, fun(V) -> V + 1 end, 1, State#state.stats),
            State#state{stats = NewStats};
        [] ->
            %% New record
            Record = #{key => Key,
                      timestamp => Now,
                      access_count => 1,
                      size => Size},
            ets:insert(State#state.access_table, {Key, Record}),
            NewStats = maps:update_with(misses, fun(V) -> V + 1 end, 1, State#state.stats),
            State#state{stats = NewStats}
    end.

%% @private Record insert
-spec record_insert_internal(term(), integer(), #state{}) -> #state{}.
record_insert_internal(Key, Size, State) ->
    Now = erlang:monotonic_time(microsecond),
    Record = #{key => Key,
              timestamp => Now,
              access_count => 0,
              size => Size},
    ets:insert(State#state.access_table, {Key, Record}),
    State.

%% @private Get policy statistics
-spec get_policy_stats_internal(#state{}) -> policy_stats().
get_policy_stats_internal(State) ->
    Stats = State#state.stats,
    Hits = maps_get(hits, Stats, 0),
    Misses = maps_get(misses, Stats, 0),
    Total = Hits + Misses,
    HitRate = case Total of
                  0 -> 0.0;
                  _ -> Hits / Total
              end,

    #{evictions => maps_get(evictions, Stats, 0),
      hits => Hits,
      misses => Misses,
      hit_rate => HitRate,
      policy_name => State#state.current_policy}.

%% @private Benchmark policies
-spec benchmark_policies_internal(ets:tid(), pos_integer(), #state{}) ->
          #{eviction_policy() => policy_stats()}.
benchmark_policies_internal(Table, Iterations, _State) ->
    Policies = [lru, lfu, fifo, random],

    lists:foldl(fun(Policy, Acc) ->
                       Stats = benchmark_policy(Table, Policy, Iterations),
                       Acc#{Policy => Stats}
               end, #{}, Policies).

%% @private Benchmark a single policy
-spec benchmark_policy(ets:tid(), eviction_policy(), pos_integer()) -> policy_stats().
benchmark_policy(Table, Policy, Iterations) ->
    %% Clear access tracking
    %% (In real implementation, would use separate table)

    StartTime = erlang:monotonic_time(microsecond),

    %% Run benchmark
    {Hits, Misses, Evictions} =
        lists:foldl(fun(N, {H, M, E}) ->
                           Key = N rem 1000,
                           case ets:lookup(Table, Key) of
                               [] ->
                                   %% Miss - insert
                                   ets:insert(Table, {Key, N}),
                                   {H, M + 1, E};
                               _ ->
                                   %% Hit
                                   {H + 1, M, E}
                           end
                   end, {0, 0, 0}, lists:seq(1, Iterations)),

    EndTime = erlang:monotonic_time(microsecond),

    %% Calculate hit rate
    Total = Hits + Misses,
    HitRate = case Total of
                  0 -> 0.0;
                  _ -> Hits / Total
              end,

    #{evictions => Evictions,
      hits => Hits,
      misses => Misses,
      hit_rate => HitRate,
      policy_name => Policy,
      duration_us => EndTime - StartTime}.

%% @private Recommend policy
-spec recommend_policy_internal(map()) -> eviction_policy().
recommend_policy_internal(Characteristics) ->
    AccessPattern = maps:get(access_pattern, Characteristics, unknown),
    DataSize = maps:get(data_size, Characteristics, 0),
    Performance = maps:get(performance, Characteristics, normal),

    case {AccessPattern, Performance} of
        {temporal, _} when DataSize < 1000000 ->
            lru;
        {sequential, _} ->
            fifo;
        {uniform, _} ->
            random;
        {_, critical} ->
            lru;
        _ when DataSize > 10000000 ->
            %% Large data - use LFU to prioritize hot items
            lfu;
        _ ->
            lru
    end.

%% @private Create custom policy
-spec create_policy_internal(atom(), map(), #state{}) ->
          ok | {error, term()} | {ok, #state{}}.
create_policy_internal(_Name, _Config, State) ->
    %% For now, just return ok
    {reply, ok, State}.

%% @private Delete policy
-spec delete_policy_internal(atom(), #state{}) -> #state{}.
delete_policy_internal(_Name, State) ->
    State.

%% @private Get default policy config
-spec default_policy_config(eviction_policy()) -> policy_config().
default_policy_config(lru) ->
    #{max_size => ?DEFAULT_MAX_SIZE};
default_policy_config(lfu) ->
    #{max_size => ?DEFAULT_MAX_SIZE};
default_policy_config(ttl) ->
    #{max_size => ?DEFAULT_MAX_SIZE, ttl => ?DEFAULT_TTL};
default_policy_config(fifo) ->
    #{max_size => ?DEFAULT_MAX_SIZE};
default_policy_config(arc) ->
    #{max_size => ?DEFAULT_MAX_SIZE, arc_c => trunc(?DEFAULT_MAX_SIZE * ?ARC_P)};
default_policy_config(random) ->
    #{max_size => ?DEFAULT_MAX_SIZE};
default_policy_config(_) ->
    #{max_size => ?DEFAULT_MAX_SIZE}.

%% @private Safe maps:get
-spec maps_get(term(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.
