-module(erlmcp_siem_generic).

-behaviour(gen_server).

-export([start_link/0, send_event/1, send_batch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(DEFAULT_BATCH_SIZE, 100).
-define(DEFAULT_BATCH_TIMEOUT, 5000). % 5 seconds

-record(state, {
    endpoints :: list(),
    connection_pool :: ets:tid(),
    batch_queue :: queue:queue(),
    batch_size :: integer(),
    batch_timeout :: integer(),
    retry_count :: integer(),
    max_retries :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec send_event(map()) -> ok | {error, term()}.
send_event(Event) when is_map(Event) ->
    gen_server:call(?SERVER, {send_event, Event}).

-spec send_batch(list()) -> ok | {error, term()}.
send_batch(Events) when is_list(Events) ->
    gen_server:call(?SERVER, {send_batch, Events}).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

-spec init(term()) -> {ok, #state{}} | {stop, term()}.
init(_Args) ->
    process_flag(trap_exit, true),

    %% Initialize connection pool
    Pool = ets:new(siem_pool, [set, private, {keypos, 1}]),

    %% Load SIEM configuration
    Endpoints = load_siem_endpoints(),
    BatchSize = application:get_env(erlmcp_security_monitoring, batch_size, ?DEFAULT_BATCH_SIZE),
    BatchTimeout = application:get_env(erlmcp_security_monitoring, batch_timeout, ?DEFAULT_BATCH_TIMEOUT),
    MaxRetries = application:get_env(erlmcp_security_monitoring, max_retries, 3),

    State = #state{
        endpoints = Endpoints,
        connection_pool = Pool,
        batch_queue = queue:new(),
        batch_size = BatchSize,
        batch_timeout = BatchTimeout,
        retry_count = 0,
        max_retries = MaxRetries
    },

    %% Start batch processing timer
    erlang:send_after(BatchTimeout, self(), process_batch),

    %% Initialize connections
    init_connections(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}} | {noreply, #state{}}.
handle_call({send_event, Event}, _From, State) ->
    %% Add event to batch queue
    NewQueue = queue:in(Event, State#state.batch_queue),

    %% Check if we should process immediately
    QueueSize = queue:len(NewQueue),
    ShouldProcess = QueueSize >= State#state.batch_size,

    case ShouldProcess of
        true ->
            {_, NewQueue2} = queue:out(NewQueue),
            case process_events(NewQueue2, State) of
                ok ->
                    {reply, ok, State#state{batch_queue = NewQueue2}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        false ->
            {reply, ok, State#state{batch_queue = NewQueue}}
    end;

handle_call({send_batch, Events}, _From, State) ->
    %% Add all events to batch queue
    NewQueue = lists:foldl(fun(Event, Q) -> queue:in(Event, Q) end,
                          State#state.batch_queue, Events),

    %% Process the batch
    case process_events(NewQueue, State) of
        ok ->
            {reply, ok, State#state{batch_queue = queue:new()}};
        {error, Reason} ->
            {reply, {error, Reason}, State#state{batch_queue = NewQueue}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(process_batch, State) ->
    %% Process batch when timer fires
    case process_events(State#state.batch_queue, State) of
        ok ->
            %% Reset timer and continue
            erlang:send_after(State#state.batch_timeout, self(), process_batch),
            {noreply, State#state{batch_queue = queue:new(), retry_count = 0}};
        {error, Reason} ->
            %% Retry logic
            case State#state.retry_count < State#state.max_retries of
                true ->
                    NewRetryCount = State#state.retry_count + 1,
                    %% Exponential backoff
                    Backoff = trunc(math:pow(2, NewRetryCount) * 1000),
                    erlang:send_after(Backoff, self(), process_batch),
                    {noreply, State#state{retry_count = NewRetryCount}};
                false ->
                    %% Max retries reached, log error and reset
                    error_logger:error_msg("SIEM batch processing failed after ~p retries: ~p",
                                         [State#state.max_retries, Reason]),
                    erlang:send_after(State#state.batch_timeout, self(), process_batch),
                    {noreply, State#state{batch_queue = queue:new(), retry_count = 0}}
            end
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

load_siem_endpoints() ->
    %% Load from configuration or environment
    Config = application:get_env(erlmcp_security_monitoring, siem_endpoints, []),
    case Config of
        [] ->
            %% Default endpoints for development
            [
                #{url => "http://localhost:8088/services/collector",
                  type => "splunk",
                  auth => "Basic #{username}:#{password}"},
                #{url => "https://qradar.example.com",
                  type => "qradar",
                  api_key => "API_KEY_HERE"}
            ];
        Endpoints ->
            Endpoints
    end.

init_connections(State) ->
    %% Initialize HTTP connections to SIEM endpoints
    lists:foreach(fun(Endpoint) ->
        ConnectionId = crypto:hash(sha256, erlang:term_to_binary(Endpoint)),
        ets:insert(State#state.connection_pool, {ConnectionId, Endpoint, init})
    end, State#state.endpoints).

process_events(Queue, State) ->
    case queue:len(Queue) of
        0 -> ok;
        _ ->
            %% Convert queue to list
            Events = queue:to_list(Queue),
            %% Format events for SIEM
            FormattedEvents = format_events(Events),
            %% Send to all endpoints
            send_to_endpoints(FormattedEvents, State)
    end.

format_events(Events) ->
    %% Format events according to CEF (Common Event Format) or Syslog
    lists:map(fun(Event) ->
        #{
            timestamp => maps:get(timestamp, Event, erlang:timestamp()),
            event_id => maps:get(event_id, Event, generate_event_id()),
            severity => maps:get(severity, Event, "medium"),
            message => maps:get(message, Event, ""),
            source => maps:get(source, Event, "erlmcp"),
            category => maps:get(category, Event, "security"),
            details => maps:get(details, Event, #{}),
            user => maps:get(user, Event, undefined),
            ip_address => maps:get(ip_address, Event, undefined),
            device => maps:get(device, Event, undefined)
        }
    end, Events).

generate_event_id() ->
    %% Generate unique event ID
    integer_to_binary(erlang:system_time(second)).

send_to_endpoints(Events, State) ->
    %% Send events to all configured SIEM endpoints
    lists:foreach(fun(Endpoint) ->
        send_endpoint_events(Endpoint, Events, State)
    end, State#state.endpoints).

send_endpoint_events(Endpoint, Events, State) ->
    %% HTTP client implementation
    Url = maps:get(url, Endpoint),
    Auth = maps:get(auth, Endpoint, undefined),

    %% Prepare HTTP request
    Headers = case Auth of
        undefined -> [];
        _ -> [{"Authorization", Auth}]
    end,

    %% Convert events to JSON
    JsonPayload = jsx:encode(Events),

    %% Send request (using hackney or gun)
    case httpc:request(post, {Url, Headers, "application/json", JsonPayload},
                     [{timeout, 30000}], []) of
        {ok, {{_, 200, _}, _, _}} ->
            ok;
        {ok, {{_, 202, _}, _, _}} ->
            %% Accepted, async processing
            ok;
        {ok, {{_, 400, _}, _, Response}} ->
            error_logger:error_msg("SIEM bad request: ~p", [Response]),
            {error, bad_request};
        {ok, {{_, 401, _}, _, _}} ->
            {error, unauthorized};
        {ok, {{_, 500, _}, _, _}} ->
            {error, server_error};
        {error, timeout} ->
            {error, timeout};
        {error, Reason} ->
            {error, Reason}
    end.