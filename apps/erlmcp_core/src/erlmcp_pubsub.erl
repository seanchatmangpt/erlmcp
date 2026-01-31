%%%-------------------------------------------------------------------
%%% @doc erlmcp_pubsub - Production pg-based pub/sub for MCP resources
%%%
%%% Lightweight gen_server wrapper around Erlang's built-in pg (process groups)
%%% module for topic-based resource subscriptions. Enables real-time resource
%%% updates with fan-out to multiple subscribers.
%%%
%%% Features:
%%% - Topic-based subscriptions (e.g., "resource:weather:sf")
%%% - Multiple subscribers per topic
%%% - Low-latency broadcast (<200μs for 5 subscribers)
%%% - Automatic distributed pub/sub across nodes
%%% - No additional dependencies (pg is built into OTP 23+)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_pubsub).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    stop/0,
    subscribe/2,
    unsubscribe/2,
    broadcast/2,
    list_subscribers/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(PG_SCOPE, erlmcp_pubsub_scope).

%% State record
-record(state, {}).

-type topic() :: term().
-type message() :: term().

%%%=============================================================================
%%% API Functions
%%%=============================================================================

%% @doc Start the pubsub server with supervision link.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the pubsub server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Subscribe a process to a topic.
%% The process will receive {erlmcp_pubsub, Topic, Message} messages.
-spec subscribe(topic(), pid()) -> ok | {error, term()}.
subscribe(Topic, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {subscribe, Topic, Pid}).

%% @doc Unsubscribe a process from a topic.
-spec unsubscribe(topic(), pid()) -> ok | {error, term()}.
unsubscribe(Topic, Pid) when is_pid(Pid) ->
    gen_server:call(?SERVER, {unsubscribe, Topic, Pid}).

%% @doc Broadcast a message to all subscribers of a topic.
%% Non-blocking async operation.
-spec broadcast(topic(), message()) -> ok.
broadcast(Topic, Message) ->
    gen_server:cast(?SERVER, {broadcast, Topic, Message}).

%% @doc List all subscribers to a topic.
-spec list_subscribers(topic()) -> [pid()].
list_subscribers(Topic) ->
    gen_server:call(?SERVER, {list_subscribers, Topic}).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    %% pg is automatically started by kernel application in OTP 23+
    %% We just need to create our scope
    case pg:start(?PG_SCOPE) of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,
    logger:info("Started erlmcp_pubsub with pg scope ~p", [?PG_SCOPE]),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call({subscribe, Topic, Pid}, _From, State) ->
    ok = pg:join(?PG_SCOPE, Topic, Pid),
    logger:debug("Subscribed ~p to topic ~p", [Pid, Topic]),
    {reply, ok, State};

handle_call({unsubscribe, Topic, Pid}, _From, State) ->
    ok = pg:leave(?PG_SCOPE, Topic, Pid),
    logger:debug("Unsubscribed ~p from topic ~p", [Pid, Topic]),
    {reply, ok, State};

handle_call({list_subscribers, Topic}, _From, State) ->
    Subscribers = pg:get_members(?PG_SCOPE, Topic),
    {reply, Subscribers, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({broadcast, Topic, Message}, State) ->
    %% Get all subscribers for this topic (local + remote nodes if distributed)
    Subscribers = pg:get_members(?PG_SCOPE, Topic),

    %% Broadcast to all subscribers
    lists:foreach(fun(Pid) ->
        Pid ! {erlmcp_pubsub, Topic, Message}
    end, Subscribers),

    logger:debug("Broadcast to ~p subscribers on topic ~p", [length(Subscribers), Topic]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
