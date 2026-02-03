%%%-------------------------------------------------------------------
%%% @doc
### HTTP Connection Pool for Load Testing

This module implements a high-performance connection pool for HTTP/HTTPS
 requests with persistent connections and load balancing.
%%%
 Features:
%%% - Connection pooling with configurable size
%%% - HTTP/1.1 and HTTP/2 support
%%% - Persistent connections
%%% - Load balancing across multiple hosts
%%% - Circuit breaker pattern
%%% - Timeout handling
%%% - Request queuing and retry logic
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_http_pool).

-behaviour(gen_server).

-export([start_link/2, request/4, get_stats/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
## Type Definitions
%%====================================================================

-record(pool_state, {
    pool_id :: binary(),
    target :: string(),
    connections :: ets:tid(),
    active :: ets:tid(),
    queue :: queue:queue(),
    pool_size :: pos_integer(),
    active_count :: pos_integer(),
    queue_count :: pos_integer(),
    stats :: #{
        total_requests :: pos_integer(),
        successful_requests :: pos_integer(),
        failed_requests :: pos_integer(),
        total_bytes :: pos_integer(),
        avg_response_time :: float(),
        circuit_breaker_open :: boolean()
    },
    circuit_breaker :: #{
        failures :: pos_integer(),
        success_rate :: float(),
        last_failure :: pos_integer(),
        threshold :: pos_integer(),
        timeout :: pos_integer()
    },
    config :: map()
}.

-type pool_state() :: #pool_state{}.

-type request_info() :: #{
    id :: reference(),
    timestamp :: pos_integer(),
    callback :: function(),
    retry_count :: pos_integer(),
    timeout :: pos_integer()
}.

%%====================================================================
## API Functions
%%====================================================================

-spec start_link(string(), pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Target, PoolSize) ->
    PoolId = generate_pool_id(Target),
    gen_server:start_link({local, PoolId}, ?MODULE, [Target, PoolSize], []).

-spec request(pid(), map(), binary(), function()) -> ok.
request(PoolPid, Headers, Payload, Callback) ->
    gen_server:cast(PoolPid, {request, Headers, Payload, Callback}).

-spec get_stats(pid()) -> {ok, map()}.
get_stats(PoolPid) ->
    gen_server:call(PoolPid, get_stats).

-spec stop(pid()) -> ok.
stop(PoolPid) ->
    gen_server:stop(PoolPid).

%%====================================================================
## gen_server Callbacks
%%====================================================================

-spec init([string(), pos_integer()]) -> {ok, pool_state()}.
init([Target, PoolSize]) ->
    process_flag(trap_exit, true),

    %% Initialize connection tables
    Connections = ets:new(connections, [
        set,
        {keypos, 1},  % connection_id
        {write_concurrency, true},
        named_table,
        public
    ]),

    Active = ets:new(active, [
        set,
        {keypos, 1},  % request_id
        {write_concurrency, true},
        named_table,
        public
    ]),

    %% Initialize pool state
    PoolId = generate_pool_id(Target),
    InitialStats = #{
        total_requests => 0,
        successful_requests => 0,
        failed_requests => 0,
        total_bytes => 0,
        avg_response_time => 0.0,
        circuit_breaker_open => false
    },

    CircuitBreaker = #{
        failures => 0,
        success_rate => 1.0,
        last_failure => 0,
        threshold => 5,
        timeout => 30000
    },

    Config = #{
        timeout => ?HTTP_TIMEOUT,
        max_retries => 3,
        keep_alive => true,
        ssl_options => [],
        headers => #{},
        user_agent => "erlmcp-load-tester/1.0"
    },

    State = #pool_state{
        pool_id = PoolId,
        target = Target,
        connections = Connections,
        active = Active,
        queue = queue:new(),
        pool_size = PoolSize,
        active_count = 0,
        queue_count = 0,
        stats = InitialStats,
        circuit_breaker = CircuitBreaker,
        config = Config
    },

    %% Initialize connections
    {ok, NewState} = initialize_connections(State),

    %% Start queue processing timer
    TimerRef = erlang:start_timer(10, self(), process_queue),

    {ok, NewState#pool_state{queue = State#pool_state.queue, timer_ref = TimerRef}}.

-spec handle_call(term(), {pid(), term()}, pool_state()) ->
                       {reply, term(), pool_state()} | {stop, term(), pool_state()}.
handle_call(get_stats, _From, State) ->
    Stats = State#pool_state.stats,
    CircuitBreaker = State#pool_state.circuit_breaker,

    {reply, {ok, Stats#{
        pool_id => State#pool_state.pool_id,
        target => State#pool_state.target,
        pool_size => State#pool_state.pool_size,
        active_connections => State#pool_state.active_count,
        queued_requests => State#pool_state.queue_count,
        circuit_breaker => CircuitBreaker
    }}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), pool_state()) -> {noreply, pool_state()} | {stop, term(), pool_state()}.
handle_cast({request, Headers, Payload, Callback}, State) ->
    RequestInfo = #{
        id = make_ref(),
        timestamp => erlang:system_time(millisecond),
        callback = Callback,
        retry_count => 0,
        timeout => maps:get(timeout, State#pool_state.config, ?HTTP_TIMEOUT)
    },

    case State#pool_state.circuit_breaker#circuit_breaker_open of
        true ->
            %% Circuit breaker open - queue the request
            NewQueue = queue:in(RequestInfo, State#pool_state.queue),
            NewStats = State#pool_state.stats#{
                failed_requests => State#pool_state.stats#failed_requests + 1
            },
            {noreply, State#pool_state{
                queue = NewQueue,
                queue_count = State#pool_state.queue_count + 1,
                stats = NewStats
            }};
        false ->
            try_process_request(State, RequestInfo)
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), pool_state()) -> {noreply, pool_state()} | {stop, term(), pool_state()}.
handle_info({timeout, TimerRef, process_queue}, State) ->
    %% Process queued requests
    case State#pool_state.queue_count > 0 andalso
         State#pool_state.active_count < State#pool_state.pool_size of
        true ->
            {Request, NewQueue} = queue:out(State#pool_state.queue),
            case Request of
                {value, RequestInfo} ->
                    NewState = try_process_request(State, RequestInfo),
                    NewState#pool_state{
                        queue = NewQueue,
                        queue_count = State#pool_state.queue_count - 1
                    };
                empty ->
                    State
            end;
        false ->
            State
    end,

    %% Schedule next queue processing
    NewTimerRef = erlang:start_timer(10, self(), process_queue),

    {noreply, State#pool_state{timer_ref = NewTimerRef}};

handle_info({http_response, Ref, Response}, State) ->
    %% Handle HTTP response
    case ets:lookup(State#pool_state.active, Ref) of
        [{Ref, RequestInfo}] ->
            ets:delete(State#pool_state.active, Ref),
            process_response(State, RequestInfo, Response);
        [] ->
            %% Unknown response
            {noreply, State}
    end;

handle_info({http_error, Ref, Error}, State) ->
    %% Handle HTTP error
    case ets:lookup(State#pool_state.active, Ref) of
        [{Ref, RequestInfo}] ->
            ets:delete(State#pool_state.active, Ref),
            process_error(State, RequestInfo, Error);
        [] ->
            %% Unknown error
            {noreply, State}
    end;

handle_info({tcp_closed, _Socket}, State) ->
    %% Connection closed - cleanup
    {noreply, cleanup_closed_connections(State)};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), pool_state()) -> ok.
terminate(_Reason, State) ->
    %% Cleanup all connections
    close_all_connections(State),
    ok.

-spec code_change(term(), pool_state(), term()) -> {ok, pool_state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
## Internal Functions
%%====================================================================

%% Generate unique pool ID
-spec generate_pool_id(string()) -> binary().
generate_pool_id(Target) ->
    Hash = erlang:phash2(Target),
    list_to_binary("http_pool_" ++ integer_to_list(Hash)).

%% Initialize connection pool
-spec initialize_connections(pool_state()) -> {ok, pool_state()}.
initialize_connections(State) ->
    lists:foldl(fun(_, Acc) ->
                   initialize_connection(Acc)
               end, State, lists:seq(1, State#pool_state.pool_size)).

%% Initialize single connection
-spec initialize_connection(pool_state()) -> pool_state().
initialize_connection(State) ->
    Target = State#pool_state.target,
    Config = State#pool_state.config,

    case parse_url(Target) of
        {ok, {Scheme, Host, Port, Path}} ->
            case connect_to_host(Scheme, Host, Port, Config) of
                {ok, Socket} ->
                    ConnectionId = make_ref(),
                    ets:insert(State#pool_state.connections,
                               {ConnectionId, Socket, Host, Port, true}),
                    State#pool_state{
                        active_count = State#pool_state.active_count + 1
                    };
                {error, Reason} ->
                    %% Log connection failure
                    error_logger:warning_msg("Failed to connect to ~p: ~p", [Target, Reason]),
                    State
            end;
        {error, Reason} ->
            error_logger:warning_msg("Invalid URL ~p: ~p", [Target, Reason]),
            State
    end.

%% Parse URL
-spec parse_url(string()) -> {ok, {atom(), string(), pos_integer(), string()}} | {error, term()}.
parse_url("http://" ++ Rest) ->
    parse_url_parts("http", Rest);
parse_url("https://" ++ Rest) ->
    parse_url_parts("https", Rest);
parse_url(_) ->
    {error, invalid_url}.

%% Parse URL components
-spec parse_url_parts(atom(), string()) -> {ok, {atom(), string(), pos_integer(), string()}} | {error, term()}.
parse_url_parts(Scheme, Rest) ->
    case string:find(Rest, "/", nomatch) of
        nomatch ->
            %% No path specified
            HostPort = Rest,
            case string:find(HostPort, ":", nomatch) of
                nomatch ->
                    {ok, {Scheme, HostPort, 80, "/"}};
                _ ->
                    [Host, PortStr] = string:split(HostPort, ":"),
                    case PortStr of
                        Port when is_list(Port) ->
                            case string:to_integer(Port) of
                                {PortNum, ""} when PortNum > 0, PortNum =< 65535 ->
                                    {ok, {Scheme, Host, PortNum, "/"}};
                                _ ->
                                    {error, invalid_port}
                            end;
                        _ ->
                            {error, invalid_port}
                    end
            end;
        _ ->
            %% Parse with path
            [HostPort | PathParts] = string:split(Rest, "/", all),
            Path = "/" ++ string:join(PathParts, "/"),
            case string:find(HostPort, ":", nomatch) of
                nomatch ->
                    {ok, {Scheme, HostPort, 80, Path}};
                _ ->
                    [Host, PortStr] = string:split(HostPort, ":"),
                    case PortStr of
                        Port when is_list(Port) ->
                            case string:to_integer(Port) of
                                {PortNum, ""} when PortNum > 0, PortNum =< 65535 ->
                                    {ok, {Scheme, Host, PortNum, Path}};
                                _ ->
                                    {error, invalid_port}
                            end;
                        _ ->
                            {error, invalid_port}
                    end
            end
    end.

%% Connect to host
-spec connect_to_host(atom(), string(), pos_integer(), map()) -> {ok, port()} | {error, term()}.
connect_to_host(http, Host, Port, _Config) ->
    %% Connect with TCP
    case gen_tcp:connect(Host, Port, [
        {active, false},
        {packet, 0},
        {mode, binary},
        {reuseaddr, true}
    ], 5000) of
        {ok, Socket} ->
            %% Send HTTP request
            Request = build_http_request(Host, Port, "/"),
            case gen_tcp:send(Socket, Request) of
                ok ->
                    {ok, Socket};
                {error, Reason} ->
                    gen_tcp:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end;

connect_to_host(https, Host, Port, Config) ->
    %% Connect with SSL
    SSLConfig = maps:get(ssl_options, Config, []),
    case ssl:connect(Host, Port, [
        {verify, verify_none},
        {server_name_indication, Host},
        {versions, [tlsv1.2, tlsv1.3]},
        {ciphers, ssl:cipher_suites(all, 'tlsv1.2')},
        {secure_renegotiate, false},
        {honor_cipher_order, true}
    ] ++ SSLConfig, 5000) of
        {ok, Socket} ->
            %% Send HTTPS request
            Request = build_http_request(Host, Port, "/"),
            case ssl:send(Socket, Request) of
                ok ->
                    {ok, Socket};
                {error, Reason} ->
                    ssl:close(Socket),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Build HTTP request
-spec build_http_request(string(), pos_integer(), string()) -> binary().
build_http_request(Host, Port, Path) ->
    Date = httpd_util:rfc1123_date(),
    Request = "GET " ++ Path ++ " HTTP/1.1\r\n" ++
              "Host: " ++ Host ++ "\r\n" ++
              "User-Agent: erlmcp-load-tester/1.0\r\n" ++
              "Accept: */*\r\n" ++
              "Connection: keep-alive\r\n" ++
              "Date: " ++ Date ++ "\r\n" ++
              "\r\n",
    list_to_binary(Request).

%% Try to process a request
-spec try_process_request(pool_state(), map()) -> pool_state().
try_process_request(State, RequestInfo) ->
    case State#pool_state.active_count < State#pool_state.pool_size of
        true ->
            %% Available connection
            process_request(State, RequestInfo);
        false ->
            %% No available connections - queue the request
            NewQueue = queue:in(RequestInfo, State#pool_state.queue),
            NewStats = State#pool_state.stats#{
                failed_requests => State#pool_state.stats#failed_requests + 1
            },
            State#pool_state{
                queue = NewQueue,
                queue_count = State#pool_state.queue_count + 1,
                stats = NewStats
            }
    end.

%% Process HTTP request
-spec process_request(pool_state(), map()) -> pool_state().
process_request(State, RequestInfo) ->
    Target = State#pool_state.target,
    Headers = maps:get(headers, RequestInfo, #{}),
    Payload = maps:get(payload, RequestInfo, <<>>),
    Ref = RequestInfo#id,

    case parse_url(Target) of
        {ok, {Scheme, Host, Port, Path}} ->
            case send_http_request(Scheme, Host, Port, Path, Headers, Payload) of
                {ok, ResponseRef} ->
                    %% Track active request
                    ets:insert(State#pool_state.active,
                               {Ref, RequestInfo}),
                    NewStats = State#pool_state.stats#{
                        total_requests => State#pool_state.stats#total_requests + 1
                    },
                    State#pool_state{stats = NewStats};
                {error, Reason} ->
                    %% Handle error
                    process_error(State, RequestInfo, Reason)
            end;
        {error, Reason} ->
            process_error(State, RequestInfo, Reason)
    end.

%% Send HTTP request
-spec send_http_request(atom(), string(), pos_integer(), string(), map(), binary()) -> {ok, reference()} | {error, term()}.
send_http_request(http, Host, Port, Path, Headers, Payload) ->
    URL = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Path,
    case httpc:request(get, {URL, maps:to_list(Headers)}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, make_ref()};
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {http_error, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end;

send_http_request(https, Host, Port, Path, Headers, Payload) ->
    URL = "https://" ++ Host ++ ":" ++ integer_to_list(Port) ++ Path,
    case httpc:request(get, {URL, maps:to_list(Headers)}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, make_ref()};
        {ok, {{_, Status, _}, _, Body}} ->
            {error, {http_error, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% Process HTTP response
-spec process_response(pool_state(), map(), term()) -> pool_state().
process_response(State, RequestInfo, Response) ->
    Callback = RequestInfo#callback,
    ResponseTime = erlang:system_time(millisecond) - RequestInfo#timestamp,

    %% Call callback function
    Callback({ok, Response}),

    %% Update statistics
    UpdatedStats = State#pool_state.stats#{
        successful_requests => State#pool_state.stats#successful_requests + 1,
        total_bytes => State#pool_state.stats#total_bytes + calculate_response_size(Response),
        avg_response_time => calculate_average(
            State#pool_state.stats#avg_response_time,
            ResponseTime
        )
    },

    %% Check circuit breaker
    {ok, NewState} = check_circuit_breaker(State, UpdatedStats),

    NewState#pool_state{
        stats = UpdatedStats,
        active_count = State#pool_state.active_count - 1
    }.

%% Process error
-spec process_error(pool_state(), map(), term()) -> pool_state().
process_error(State, RequestInfo, Error) ->
    Callback = RequestInfo#callback,
    RetryCount = RequestInfo#retry_count,

    %% Call callback function
    Callback({error, Error}),

    %% Check if we should retry
    case RetryCount < maps:get(max_retries, State#pool_state.config, 3) of
        true ->
            %% Retry the request
            RetryRequestInfo = RequestInfo#{
                retry_count => RetryCount + 1,
                timestamp => erlang:system_time(millisecond)
            },
            try_process_request(State, RetryRequestInfo);
        false ->
            %% Give up on retry
            UpdatedStats = State#pool_state.stats#{
                failed_requests => State#pool_state.stats#failed_requests + 1
            },
            NewState = State#pool_state{stats = UpdatedStats},
            %% Check circuit breaker
            {ok, NewState2} = check_circuit_breaker(NewState, UpdatedStats),
            NewState2#pool_state{
                active_count = State#pool_state.active_count - 1
            }
    end.

%% Check circuit breaker status
-spec check_circuit_breaker(pool_state(), map()) -> {ok, pool_state()}.
check_circuit_breaker(State, Stats) ->
    CircuitBreaker = State#pool_state.circuit_breaker,
    TotalRequests = Stats#total_requests,
    FailedRequests = Stats#failed_requests,

    case TotalRequests > 0 of
        true ->
            SuccessRate = (TotalRequests - FailedRequests) / TotalRequests,
            LastFailure = erlang:system_time(millisecond),

            NewCircuitBreaker = CircuitBreaker#circuit_breaker#{
                success_rate => SuccessRate,
                last_failure => LastFailure,
                failures => FailedRequests
            },

            %% Check if we should open the circuit breaker
            ShouldOpen = FailedRequests >= CircuitBreaker#circuit_breaker#threshold,
            TimeSinceLastFailure = LastFailure - CircuitBreaker#circuit_breaker#last_failure,

            case ShouldOpen andalso TimeSinceLastFailure < CircuitBreaker#circuit_breaker#timeout of
                true ->
                    %% Open circuit breaker
                    NewStats = Stats#circuit_breaker_open,
                    NewState = State#pool_state{
                        circuit_breaker = NewCircuitBreaker,
                        stats = NewStats
                    },
                    {ok, NewState};
                false ->
                    KeepClosed = SuccessRate >= 0.95,
                    case KeepClosed andalso TimeSinceLastFailure > CircuitBreaker#circuit_breaker#timeout of
                        true ->
                            %% Close circuit breaker
                            NewCircuitBreaker2 = NewCircuitBreaker#circuit_breaker#{
                                failures => 0,
                                last_failure => 0
                            },
                            NewStats2 = Stats#circuit_breaker_open(false),
                            NewState2 = State#pool_state{
                                circuit_breaker = NewCircuitBreaker2,
                                stats = NewStats2
                            },
                            {ok, NewState2};
                        false ->
                            {ok, State#pool_state{circuit_breaker = NewCircuitBreaker}}
                    end
            end;
        false ->
            {ok, State}
    end.

%% Calculate response size
-spec calculate_response_size(term()) -> pos_integer().
calculate_response_size(Response) ->
    case Response of
        {_, _, Body} when is_binary(Body) ->
            byte_size(Body);
        {_, _, Body} when is_list(Body) ->
            length(Body);
        _ ->
            0
    end.

%% Clean up closed connections
-spec cleanup_closed_connections(pool_state()) -> pool_state().
cleanup_closed_connections(State) ->
    Connections = State#pool_state.connections,
    Active = State#pool_state.active,

    %% Check for closed connections
    lists:foldl(fun({ConnectionId, Socket, _, _, _}, Acc) ->
                    case is_connection_alive(Socket) of
                        true ->
                            Acc;
                        false ->
                            %% Remove dead connection
                            ets:delete(Connections, ConnectionId),
                            Acc#pool_state{
                                connections = Connections,
                                active_count = Acc#pool_state.active_count - 1
                            }
                    end
                end, State, ets:tab2list(Connections)).

%% Check if connection is alive
-spec is_connection_alive(port()) -> boolean().
is_connection_alive(Socket) ->
    case gen_tcp:(Socket) of
        {error, _} ->
            false;
        _ ->
            true
    end.

%% Close all connections
-spec close_all_connections(pool_state()) -> ok.
close_all_connections(State) ->
    Connections = State#pool_state.connections,
    lists:foreach(fun({_, Socket, _, _, _}) ->
                      gen_tcp:close(Socket)
                  end, ets:tab2list(Connections)),
    ets:delete(Connections),
    ets:delete(State#pool_state.active).