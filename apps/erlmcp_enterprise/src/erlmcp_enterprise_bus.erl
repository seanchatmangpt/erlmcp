%% @doc Enterprise Event Bus
%% Central event bus for enterprise integration events
-module(erlmcp_enterprise_bus).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([publish/2, subscribe/2, unsubscribe/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    subscribers :: map(),  % Event -> [Pids]
    event_log :: ets:tid(),  % Event history
    metrics :: map()
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

-spec publish(Event :: atom(), Data :: term()) -> ok.
publish(Event, Data) ->
    gen_server:call(?MODULE, {publish, Event, Data}).

-spec subscribe(Event :: atom(), Pid :: pid()) -> ok.
subscribe(Event, Pid) ->
    gen_server:call(?MODULE, {subscribe, Event, Pid}).

-spec unsubscribe(Event :: atom(), Pid :: pid()) -> ok.
unsubscribe(Event, Pid) ->
    gen_server:call(?MODULE, {unsubscribe, Event, Pid}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize event log
    EventLog = ets:new(event_log, [
        set,
        public,
        {write_concurrency, true},
        {keypos, 2},  % Event timestamp
        {named_table, true}
    ]),

    State = #state{
        subscribers = #{},
        event_log = EventLog,
        metrics = #{}
    },

    %% Start event retention cleanup timer
    erlang:send_after(86400000, self(), cleanup_events),  % 24 hours

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({publish, Event, Data}, _From, State) ->
    %% Publish event to subscribers
    case maps:get(Event, State#state.subscribers, []) of
        [] ->
            ?LOG_DEBUG("No subscribers for event: ~p", [Event]);
        Subscribers ->
            lists:foreach(fun(Pid) ->
                Pid ! {enterprise_event, Event, Data}
            end, Subscribers)
    end,

    %% Log event
    EventRecord = #{
        timestamp => erlang:system_time(millisecond),
        event => Event,
        data => Data,
        source => node()
    },
    ets:insert(State#state.event_log, {erlang:unique_integer(), EventRecord}),

    %% Update metrics
    Metrics = update_metric(State#state.metrics, events_published, 1),

    {reply, ok, State#state{metrics = Metrics}};

handle_call({subscribe, Event, Pid}, _From, State) ->
    %% Add subscriber
    Subscribers = maps:get(Event, State#state.subscribers, []),
    UpdatedSubscribers = case lists:member(Pid, Subscribers) of
        true -> Subscribers;
        false -> [Pid | Subscribers]
    end,

    NewSubscribers = State#state.subscribers#{Event => UpdatedSubscribers},

    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call({unsubscribe, Event, Pid}, _From, State) ->
    %% Remove subscriber
    Subscribers = maps:get(Event, State#state.subscribers, []),
    UpdatedSubscribers = lists:delete(Pid, Subscribers),

    NewSubscribers = case UpdatedSubscribers of
        [] -> maps:remove(Event, State#state.subscribers);
        _ -> State#state.subscribers#{Event => UpdatedSubscribers}
    end,

    {reply, ok, State#state{subscribers = NewSubscribers}};

handle_call(get_subscriptions, _From, State) ->
    {reply, {ok, State#state.subscribers}, State};

handle_call(get_metrics, _From, State) ->
    {reply, {ok, State#state.metrics}, State};

handle_call(_Msg, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}} | {stop, term(), #state{}}.
handle_info({enterprise_event, Event, Data}, State) ->
    %% Handle incoming events from other nodes
    case maps:get(Event, State#state.subscribers, []) of
        [] ->
            ?LOG_DEBUG("No subscribers for event: ~p", [Event]);
        Subscribers ->
            lists:foreach(fun(Pid) ->
                Pid ! {enterprise_event, Event, Data}
            end, Subscribers)
    end,

    {noreply, State};

handle_info(cleanup_events, State) ->
    %% Clean up old events
    CurrentTime = erlang:system_time(millisecond),
    Cutoff = CurrentTime - 86400000,  % 24 hours

    %% Delete old events
    ets:delete_all_objects(State#state.event_log),

    %% Schedule next cleanup
    erlang:send_after(86400000, self(), cleanup_events),

    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, State) ->
    %% Cleanup event log
    ets:delete(State#state.event_log).

-spec code_change(term(), #state{}, term()) -> {ok, #state{}} | {error, term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec update_metric(map(), atom(), integer()) -> map().
update_metric(Metrics, Key, Inc) ->
    Current = maps:get(Key, Metrics, 0),
    Metrics#{Key => Current + Inc}.