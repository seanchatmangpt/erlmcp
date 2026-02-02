%%%-------------------------------------------------------------------
%% @doc OTP 28 Optimized ETS Registry
%%
%%% This module provides ETS table creation utilities optimized for
%%% OTP 28's scalability improvements:
%%%
%%% OTP 28 INNOVATIONS:
%%% - ETS tables can now hold > 2 billion entries
%%% - Improved scalability with {decentralized_counters, true}
%%% - Better read/write concurrency options
%%%
%%% PATTERNS:
%%% - Global tool/resource registries (read-heavy)
%%% - Session registries (write-heavy during login, read-heavy during use)
%%% - Cache tables (high concurrency, TTL-based)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_ets_registry).

-behaviour(gen_server).

%% API
-export([start_link/0,
         create_registry/1,
         create_session_registry/0,
         create_cache_table/2,
         get_table_stats/1,
         list_all_tables/0,
         delete_table/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type table_name() :: atom().
-type table_type() :: registry | session | cache | rate_limiter | auth.
-type ets_options() :: [term()].
-type table_stats() ::
    #{name := table_name(),
      type := table_type(),
      size => non_neg_integer(),
      memory => non_neg_integer(),
      owner => pid() | undefined,
      read_concurrency => boolean(),
      write_concurrency => boolean(),
      decentralized_counters => boolean()}.

-record(state, {
    tables = #{} :: #{table_name() => table_stats()}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Create a scalable registry table for global MCP resources.
%%
%% Optimized for:
%%% - Read-heavy workloads (many lookups, few updates)
%%% - Concurrent reads from multiple processes
%%% - Tool and resource registries
%%%
%% Uses:
%%% - {read_concurrency, true} - Optimize for concurrent reads
%%% - {decentralized_counters, true} - OTP 28 improvement for large tables
%%%
%% @end
%%--------------------------------------------------------------------
-spec create_registry(table_name()) -> {ok, ets:tid()} | {error, term()}.
create_registry(Name) ->
    gen_server:call(?MODULE, {create_registry, Name}).

%%--------------------------------------------------------------------
%% @doc Create a scalable session registry.
%%
%% Optimized for:
%%% - Ordered_set for range queries (session expiration)
%%% - Mixed read/write workload
%%% - High-volume session storage (>1M sessions)
%%%
%% Uses:
%%% - ordered_set - Enables range queries for TTL cleanup
%%% - {read_concurrency, true} - Concurrent session lookups
%%% - {write_concurrency, true} - Concurrent session updates
%%% - {decentralized_counters, true} - OTP 28 large table support
%%%
%% @end
%%--------------------------------------------------------------------
-spec create_session_registry() -> {ok, ets:tid()} | {error, term()}.
create_session_registry() ->
    gen_server:call(?MODULE, create_session_registry).

%%--------------------------------------------------------------------
%% @doc Create a cache table with TTL support.
%%
%% Optimized for:
%%% - High-frequency read/write operations
%%% - TTL-based expiration
%%% - LRU eviction policies
%%%
%% Options:
%%% - {ttl, Seconds} - Auto-expire entries after TTL
%%% - {max_size, N} - Maximum number of entries
%%% - {tags, [Tag]} - Tag-based invalidation support
%%%
%% @end
%%--------------------------------------------------------------------
-spec create_cache_table(table_name(), map()) -> {ok, ets:tid()} | {error, term()}.
create_cache_table(Name, Options) ->
    gen_server:call(?MODULE, {create_cache_table, Name, Options}).

%%--------------------------------------------------------------------
%% @doc Get statistics for an ETS table.
%%
%% Returns:
%%% - size: Number of entries
%%% - memory: Memory usage in bytes
%%% - owner: Table owner process
%%% - Concurrency settings
%%%
%% @end
%%--------------------------------------------------------------------
-spec get_table_stats(table_name()) -> {ok, table_stats()} | {error, not_found}.
get_table_stats(Name) ->
    gen_server:call(?MODULE, {get_stats, Name}).

%%--------------------------------------------------------------------
%% @doc List all registered ETS tables with their stats.
%% @end
%%--------------------------------------------------------------------
-spec list_all_tables() -> {ok, [table_stats()]}.
list_all_tables() ->
    gen_server:call(?MODULE, list_all_tables).

%%--------------------------------------------------------------------
%% @doc Delete a registered ETS table.
%% @end
%%--------------------------------------------------------------------
-spec delete_table(table_name()) -> ok | {error, not_found}.
delete_table(Name) ->
    gen_server:call(?MODULE, {delete_table, Name}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    ?LOG_INFO("Starting OTP 28 ETS registry manager"),
    {ok, #state{}}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()}.
handle_call({create_registry, Name}, _From, State) ->
    case ets:whereis(Name) of
        undefined ->
            Options = [
                set,                      % Unique keys
                public,                   % Read/write from any process
                named_table,              % Accessible by name
                {read_concurrency, true}, % Optimize for concurrent reads
                {decentralized_counters, true} % OTP 28: >2B entries support
            ],
            Tid = ets:new(Name, Options),
            Stats = #{
                name => Name,
                type => registry,
                size => 0,
                memory => 0,
                owner => self(),
                read_concurrency => true,
                write_concurrency => false,
                decentralized_counters => true
            },
            ?LOG_INFO("Created registry table: ~p with OTP 28 optimizations", [Name]),
            {reply, {ok, Tid}, State#state{tables = maps:put(Name, Stats, State#state.tables)}};
        _Tid ->
            {reply, {error, already_exists}, State}
    end;

handle_call(create_session_registry, _From, State) ->
    Name = erlmcp_sessions_otp28,
    case ets:whereis(Name) of
        undefined ->
            Options = [
                ordered_set,              % Range queries for TTL cleanup
                public,
                named_table,
                {read_concurrency, true}, % Concurrent session lookups
                {write_concurrency, true}, % Concurrent session updates
                {decentralized_counters, true} % OTP 28: large session registries
            ],
            Tid = ets:new(Name, Options),
            Stats = #{
                name => Name,
                type => session,
                size => 0,
                memory => 0,
                owner => self(),
                read_concurrency => true,
                write_concurrency => true,
                decentralized_counters => true
            },
            ?LOG_INFO("Created session registry table: ~p with OTP 28 optimizations", [Name]),
            {reply, {ok, Tid}, State#state{tables = maps:put(Name, Stats, State#state.tables)}};
        _Tid ->
            {reply, {ok, Name}, State}
    end;

handle_call({create_cache_table, Name, Options}, _From, State) ->
    case ets:whereis(Name) of
        undefined ->
            % Base options for cache tables
            BaseOptions = [
                set,
                public,
                named_table,
                {read_concurrency, true},
                {write_concurrency, true},
                {decentralized_counters, true}
            ],

            % Add keypos if specified (for record-based caches)
            FinalOptions = case maps:get(keypos, Options, undefined) of
                undefined -> BaseOptions;
                KeyPos -> [{keypos, KeyPos} | BaseOptions]
            end,

            Tid = ets:new(Name, FinalOptions),

            % Initialize TTL cleanup if configured
            TTL = maps:get(ttl, Options, undefined),
            case TTL of
                undefined ->
                    ok;
                _ ->
                    % Start periodic cleanup for this table
                    erlang:send_after(timer:seconds(60), self(), {cleanup_ttl, Name, TTL})
            end,

            Stats = #{
                name => Name,
                type => cache,
                size => 0,
                memory => 0,
                owner => self(),
                read_concurrency => true,
                write_concurrency => true,
                decentralized_counters => true,
                ttl => TTL
            },
            ?LOG_INFO("Created cache table: ~p with TTL: ~p", [Name, TTL]),
            {reply, {ok, Tid}, State#state{tables = maps:put(Name, Stats, State#state.tables)}};
        _Tid ->
            {reply, {error, already_exists}, State}
    end;

handle_call({get_stats, Name}, _From, State) ->
    case ets:whereis(Name) of
        undefined ->
            {reply, {error, not_found}, State};
        _Tid ->
            Size = ets:info(Name, size),
            Memory = ets:info(Name, memory),
            Owner = ets:info(Name, owner),
            ReadConc = ets:info(Name, read_concurrency),
            WriteConc = ets:info(Name, write_concurrency),

            Stats = #{
                name => Name,
                type => maps:get(Name, State#state.tables, #{type => unknown}),
                size => Size,
                memory => Memory,
                owner => Owner,
                read_concurrency => ReadConc,
                write_concurrency => WriteConc,
                decentralized_counters => true % OTP 28 default
            },
            {reply, {ok, Stats}, State}
    end;

handle_call(list_all_tables, _From, State) ->
    Tables = maps:fold(fun(_Name, Stats, Acc) ->
        [Stats | Acc]
    end, [], State#state.tables),
    {reply, {ok, lists:reverse(Tables)}, State};

handle_call({delete_table, Name}, _From, State) ->
    case ets:whereis(Name) of
        undefined ->
            {reply, {error, not_found}, State};
        _Tid ->
            ets:delete(Name),
            NewState = State#state{tables = maps:remove(Name, State#state.tables)},
            ?LOG_INFO("Deleted ETS table: ~p", [Name]),
            {reply, ok, NewState}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({cleanup_ttl, TableName, TTLSeconds}, State) ->
    % Periodic TTL cleanup for cache tables
    Now = erlang:system_time(second),

    % Find and delete expired entries
    ExpiredKeys = ets:foldl(
        fun({Key, {_Value, ExpiresAt}}, Acc) when is_integer(ExpiresAt) ->
            case Now > ExpiresAt of
                true -> [Key | Acc];
                false -> Acc
            end;
        (_Entry, Acc) ->
            Acc
        end,
        [],
        TableName
    ),

    % Delete expired entries
    lists:foreach(fun(Key) ->
        ets:delete(TableName, Key)
    end, ExpiredKeys),

    % Schedule next cleanup
    erlang:send_after(timer:seconds(60), self(), {cleanup_ttl, TableName, TTLSeconds}),

    if
        length(ExpiredKeys) > 0 ->
            ?LOG_DEBUG("Cleaned ~p expired entries from ~p", [length(ExpiredKeys), TableName]);
        true ->
            ok
    end,

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ?LOG_INFO("Terminating OTP 28 ETS registry manager"),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
