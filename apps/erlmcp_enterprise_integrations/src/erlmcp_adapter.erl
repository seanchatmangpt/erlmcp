%% @doc Base Integration Adapter
%% Common functionality for all enterprise integration adapters
-module(erlmcp_adapter).

-behaviour(gen_server).

-export([start_link/1, send/2, get_status/1, configure/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type adapter_type() :: identity | monitoring | logging | bi | esb | dwh |
                      devops | api | cloud | security | config | container.
-type message() :: map().
-type config() :: map().
-type status() :: idle | active | error | maintenance.
-type error_reason() :: term().

-record(state, {
    type :: adapter_type(),
    config :: config(),
    status :: status(),
    connection :: pid() | undefined,
    retry_count :: non_neg_integer(),
    max_retries :: non_neg_integer(),
    metrics :: map()
}).

-callback init(config()) -> {ok, state()} | {error, error_reason()}.
-callback send(message(), state()) -> {ok, message()} | {error, error_reason()}.
-callback get_status(state()) -> status().
-callback configure(config(), state()) -> {ok, state()} | {error, error_reason()}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(adapter_type()) -> {ok, pid()} | {error, term()}.
start_link(AdapterType) ->
    gen_server:start_link({local, adapter_name(AdapterType)}, ?MODULE, [AdapterType], []).

%% Send a message through the adapter
-spec send(pid(), message()) -> {ok, message()} | {error, term()}.
send(AdapterPid, Message) ->
    gen_server:call(AdapterPid, {send, Message}).

%% Get the current status of the adapter
-spec get_status(pid()) -> status().
get_status(AdapterPid) ->
    gen_server:call(AdapterPid, get_status).

%% Configure the adapter
-spec configure(pid(), config()) -> ok | {error, term()}.
configure(AdapterPid, Config) ->
    gen_server:call(AdapterPid, {configure, Config}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([AdapterType]) ->
    %% Initialize adapter with default configuration
    Config = default_config(AdapterType),
    State = #state{
        type = AdapterType,
        config = Config,
        status = idle,
        connection = undefined,
        retry_count = 0,
        max_retries = 3,
        metrics = #{}
    },

    %% Initialize the specific adapter
    case AdapterType:init(Config) of
        {ok, NewState} ->
            %% Start connection if needed
            FinalState = maybe_start_connection(NewState),
            {ok, FinalState};
        {error, Reason} ->
            ?LOG_ERROR("Failed to initialize ~p adapter: ~p", [AdapterType, Reason]),
            {stop, Reason}
    end.

handle_call({send, Message}, _From, State) ->
    try
        %% Validate message if validation is enabled
        ValidatedMessage = maybe_validate_message(Message, State),

        %% Send the message
        case AdapterType:send(ValidatedMessage, State) of
            {ok, Response} ->
                %% Update metrics
                NewMetrics = update_metrics(State#state.metrics, send, 1),
                NewState = State#state{metrics = NewMetrics, retry_count = 0},

                {reply, {ok, Response}, NewState};
            {error, Reason} ->
                %% Handle error with retry logic
                handle_retry(Message, Reason, State)
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Error sending message through ~p adapter: ~p:~p",
                       [State#state.type, Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(get_status, _From, State) ->
    %% Get current status from specific adapter
    Status = AdapterType:get_status(State),
    NewState = State#state{status = Status},
    {reply, Status, NewState};

handle_call({configure, NewConfig}, _From, State) ->
    try
        %% Configure the specific adapter
        case AdapterType:configure(NewConfig, State) of
            {ok, NewState} ->
                %% Update configuration
                UpdatedState = NewState#state{config = NewConfig},
                {reply, ok, UpdatedState};
            {error, Reason} ->
                {reply, {error, Reason}, State}
        end
    catch
        Error:Reason ->
            ?LOG_ERROR("Error configuring ~p adapter: ~p:~p",
                       [State#state.type, Error, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info({retry_message, Message, RetryCount}, State) ->
    %% Handle retry of failed message
    handle_retry(Message, retry, State#state{retry_count = RetryCount});

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Clean up resources
    maybe_stop_connection(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%================================================================%%

adapter_name(AdapterType) ->
    list_to_atom("erlmcp_adapter_" ++ atom_to_list(AdapterType)).

default_config(AdapterType) ->
    %% Default configuration for each adapter type
    case AdapterType of
        identity -> #{
            provider => undefined,
            endpoint => undefined,
            auth => undefined,
            timeout => 30000,
            retries => 3
        };
        monitoring -> #{
            system => undefined,
            endpoint => undefined,
            auth => undefined,
            metrics => #{},
            alert_rules => []
        };
        logging -> #{
            platform => undefined,
            endpoint => undefined,
            auth => undefined,
            format => json,
            level => info
        };
        bi -> #{
            tool => undefined,
            endpoint => undefined,
            auth => undefined,
            dataset => undefined,
            refresh_interval => 300000
        };
        esb -> #{
            bus_type => undefined,
            endpoint => undefined,
            auth => undefined,
            message_format => json,
            queue => undefined
        };
        dwh -> #{
            warehouse => undefined,
            endpoint => undefined,
            auth => undefined,
            schema => undefined,
            batch_size => 1000
        };
        devops -> #{
            tool => undefined,
            endpoint => undefined,
            auth => undefined,
            pipeline => undefined
        };
        api -> #{
            gateway => undefined,
            endpoint => undefined,
            auth => undefined,
            rate_limit => 100,
            timeout => 10000
        };
        cloud -> #{
            provider => undefined,
            region => undefined,
            auth => undefined,
            resources => []
        };
        security -> #{
            system => undefined,
            endpoint => undefined,
            auth => undefined,
            alert_rules => []
        };
        config -> #{
            tool => undefined,
            endpoint => undefined,
            auth => undefined,
            nodes => []
        };
        container -> #{
            orchestration => undefined,
            endpoint => undefined,
            auth => undefined,
            cluster => undefined
        }
    end.

maybe_start_connection(State) ->
    %% Start connection if adapter requires it
    case needs_connection(State) of
        true ->
            case start_connection(State) of
                {ok, ConnectionPid} ->
                    State#state{connection = ConnectionPid, status = active};
                {error, Reason} ->
                    ?LOG_WARNING("Failed to start connection for ~p adapter: ~p",
                               [State#state.type, Reason]),
                    State#state{status = error}
            end;
        false ->
            State#state{status = active}
    end.

needs_connection(State) ->
    %% Check if adapter requires an active connection
    case State#state.type of
        identity -> true;
        monitoring -> true;
        logging -> true;
        bi -> true;
        esb -> true;
        dwh -> true;
        devops -> true;
        api -> true;
        cloud -> true;
        security -> true;
        config -> true;
        container -> true
    end.

start_connection(State) ->
    %% Start a connection based on adapter type
    case State#state.type of
        identity ->
            erlmcp_identity_connection:start(State#state.config);
        monitoring ->
            erlmcp_monitoring_connection:start(State#state.config);
        logging ->
            erlmcp_logging_connection:start(State#state.config);
        bi ->
            erlmcp_bi_connection:start(State#state.config);
        esb ->
            erlmcp_esb_connection:start(State#state.config);
        dwh ->
            erlmcp_dwh_connection:start(State#state.config);
        devops ->
            erlmcp_devops_connection:start(State#state.config);
        api ->
            erlmcp_api_connection:start(State#state.config);
        cloud ->
            erlmcp_cloud_connection:start(State#state.config);
        security ->
            erlmcp_security_connection:start(State#state.config);
        config ->
            erlmcp_config_connection:start(State#state.config);
        container ->
            erlmcp_container_connection:start(State#state.config)
    end.

maybe_stop_connection(State) ->
    %% Stop connection if it exists
    case State#state.connection of
        undefined -> ok;
        ConnectionPid ->
            erlmcp_connection_manager:stop(ConnectionPid)
    end.

maybe_validate_message(Message, State) ->
    %% Validate message if validation is enabled
    case maps:get(validate, State#state.config, true) of
        true ->
            validate_message(Message, State);
        false ->
            Message
    end.

validate_message(Message, State) ->
    %% Validate message against schema if available
    case maps:get(schema, State#state.config, undefined) of
        undefined ->
            Message;
        Schema ->
            erlmcp_validator:validate(Message, Schema)
    end.

handle_retry(Message, Reason, State) ->
    %% Handle retry logic
    case State#state.retry_count < State#state.max_retries of
        true ->
            %% Schedule retry
            NewRetryCount = State#state.retry_count + 1,
            Delay = retry_delay(NewRetryCount),
            erlang:send_after(Delay, self(), {retry_message, Message, NewRetryCount}),

            %% Update metrics
            NewMetrics = update_metrics(State#state.metrics, retry, 1),
            NewState = State#state{metrics = NewMetrics, retry_count = NewRetryCount},

            {reply, {error, Reason}, NewState};
        false ->
            %% Max retries exceeded
            ?LOG_ERROR("Max retries exceeded for ~p adapter: ~p",
                      [State#state.type, Reason]),
            {reply, {error, Reason}, State}
    end.

retry_delay(RetryCount) ->
    %% Calculate delay with exponential backoff
    BaseDelay = 1000, % 1 second
    Backoff = math:pow(2, RetryCount - 1),
    trunc(BaseDelay * Backoff).

update_metrics(Metrics, Type, Inc) ->
    %% Update performance metrics
    Key = atom_to_binary(Type, utf8),
    maps:update_with(Key, fun(V) -> V + Inc end, Inc, Metrics).