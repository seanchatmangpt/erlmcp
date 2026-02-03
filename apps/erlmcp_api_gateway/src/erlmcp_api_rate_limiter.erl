-module(erlmcp_api_rate_limiter).

-behaviour(gen_server).

%% API exports
-export([start_link/0, check_limit/3, increment_counter/2, get_counters/1, clear_counters/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp_api_gateway.hrl").

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

check_limit(ConsumerId, Path, LimitConfig) ->
    gen_server:call(?MODULE, {check_limit, ConsumerId, Path, LimitConfig}).

increment_counter(ConsumerId, Path) ->
    gen_server:call(?MODULE, {increment_counter, ConsumerId, Path}).

get_counters(ConsumerId) ->
    gen_server:call(?MODULE, {get_counters, ConsumerId}).

clear_counters(ConsumerId) ->
    gen_server:call(?MODULE, {clear_counters, ConsumerId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize countersETS table
    ets:new(rate_limit_counters, [
        named_table,
        public,
        set,
        {keypos, 2},
        {write_concurrency, true}
    ]),

    %% Load rate limiting configuration
    Config = load_rate_config(),

    %% Start cleanup process
    erlang:send_after(60000, self(), cleanup_counters),

    State = #{
        config => Config,
        counters => ets:tab2list(rate_limit_counters)
    },

    {ok, State}.

handle_call({check_limit, ConsumerId, Path, LimitConfig}, _From, State) ->
    Now = erlang:system_time(millisecond),
    CounterKey = {ConsumerId, Path},

    %% Get current counter value
    case ets:lookup(rate_limit_counters, CounterKey) of
        [{_, Count, Timestamp}] ->
            %% Check if window has expired
            Window = get_window_from_config(LimitConfig),
            if
                Now - Timestamp > Window ->
                    %% Reset counter
                    true = ets:insert(rate_limit_counters, {CounterKey, 1, Now}),
                    {reply, {allowed, get_remaining_limit(1, LimitConfig)}, State};
                true ->
                    %% Check if limit exceeded
                    NewCount = Count + 1,
                    if
                        NewCount > get_limit_from_config(LimitConfig) ->
                            {reply, {denied, get_window_end(Timestamp, Window)}, State};
                        true ->
                            %% Update counter
                            true = ets:insert(rate_limit_counters, {CounterKey, NewCount, Timestamp}),
                            {reply, {allowed, get_remaining_limit(NewCount, LimitConfig)}, State}
                    end
            end;
        [] ->
            %% No counter found, initialize
            true = ets:insert(rate_limit_counters, {CounterKey, 1, Now}),
            {reply, {allowed, get_remaining_limit(1, LimitConfig)}, State}
    end;

handle_call({increment_counter, ConsumerId, Path}, _From, State) ->
    Now = erlang:system_time(millisecond),
    CounterKey = {ConsumerId, Path},

    %% Get current counter value
    case ets:lookup(rate_limit_counters, CounterKey) of
        [{_, Count, Timestamp}] ->
            %% Update counter
            NewCount = Count + 1,
            true = ets:insert(rate_limit_counters, {CounterKey, NewCount, Timestamp});
        [] ->
            %% Initialize counter
            true = ets:insert(rate_limit_counters, {CounterKey, 1, Now})
    end,

    {reply, ok, State};

handle_call({get_counters, ConsumerId}, _From, State) ->
    %% Get all counters for this consumer
    Counters = ets:match_object(rate_limit_counters, {ConsumerId, '_', '_'}) ++
                ets:match_object(rate_limit_counters, {'_', ConsumerId, '_'}),

    {reply, {ok, Counters}, State};

handle_call({clear_counters, ConsumerId}, _From, State) ->
    %% Clear all counters for this consumer
    ObjectsToDelete = ets:match_object(rate_limit_counters, {ConsumerId, '_', '_'}),
    lists:foreach(fun(Object) ->
        ets:delete_object(rate_limit_counters, Object)
    end, ObjectsToDelete),

    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_counters, State) ->
    %% Clean up old counters
    Now = erlang:system_time(millisecond),
    Window = 3600000, %% 1 hour

    %% Find counters older than window
    OldCounters = ets:foldl(fun({Key, _, Timestamp}, Acc) ->
        if
            Now - Timestamp > Window ->
                [Key | Acc];
            true ->
                Acc
        end
    end, [], rate_limit_counters),

    %% Delete old counters
    lists:foreach(fun(Key) ->
        ets:delete(rate_limit_counters, Key)
    end, OldCounters),

    %% Schedule next cleanup
    erlang:send_after(60000, self(), cleanup_counters),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_rate_config() ->
    %% Load rate limiting configuration
    DefaultLimits = #{
        default => #{
            window => 60000, %% 1 minute
            limit => 100
        },
        per_consumer => #{
            window => 60000, %% 1 minute
            limit => 1000
        },
        per_api => #{
            window => 60000, %% 1 minute
            limit => 500
        }
    },

    DefaultLimits.

get_window_from_config(Config) ->
    case maps:get(window, Config, undefined) of
        undefined ->
            60000; %% 1 minute default
        Window ->
            Window
    end.

get_limit_from_config(Config) ->
    case maps:get(limit, Config, undefined) of
        undefined ->
            100; %% Default limit
        Limit ->
            Limit
    end.

get_remaining_limit(Current, LimitConfig) ->
    Max = get_limit_from_config(LimitConfig),
    Max - Current.

get_window_end(Timestamp, Window) ->
    Timestamp + Window.