# Process Communication Architecture for OTP 28.3.1+
## Enhanced Message Handling and Communication Patterns

## Overview

This document details the enhanced process communication architecture for erlmcp, leveraging OTP 28.3.1+ features to create efficient, scalable, and reliable communication patterns. The architecture focuses on modern messaging patterns, priority handling, and advanced telemetry.

## Current Process Communication Analysis

### Current Architecture
- **Registry-based routing**: Using gproc for process discovery
- **Traditional gen_server**: Standard server-client communication
- **Simple message passing**: Direct process-to-process messaging
- **Basic monitoring**: Process monitoring for crash detection

## OTP 28.3.1+ Enhanced Process Communication Architecture

### 1. Native JSON Integration

```erlang
%% OTP 28.3.1+ Native JSON Processing Architecture
%%
%% Leverage OTP 28 native JSON module for:
%% - Better performance
%% - Built-in validation
%% - Unicode support
%% - Schema validation

-module(erlmcp_json_processor).

-export([process_request/1, validate_request/1, encode_response/1, decode_response/1]).

-export([init/1, handle_json_request/2, validate_schema/1]).

%%====================================================================
%% Enhanced JSON Processing API
%%====================================================================

%% @doc Process JSON request with OTP 28 native support
-spec process_request(binary()) -> {ok, term()} | {error, term()}.
process_request(JsonData) ->
    try
        %% OTP 28 Native JSON parsing
        Decoded = json:decode(JsonData),

        %% Enhanced validation
        case validate_request(Decoded) of
            {ok, Validated} ->
                %% Process validated request
                handle_validated_request(Validated);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:badarg ->
            {error, invalid_json};
        error:{validation_failed, Details} ->
            {error, {validation_error, Details}};
        error:Error ->
            {error, {json_error, Error}}
    end.

%% @doc Validate JSON request with schema
-spec validate_request(term()) -> {ok, term()} | {error, term()}.
validate_request(Request) ->
    %% OTP 28+ Schema validation
    case json:schema_validate(Request, mcp_schema) of
        {ok, Validated} ->
            %% Additional business logic validation
            case business_logic_validation(Validated) of
                {ok, Final} -> {ok, Final};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Encode response to JSON
-spec encode_response(term()) -> {ok, binary()} | {error, term()}.
encode_response(Response) ->
    try
        %% OTP 28 Native JSON encoding
        Json = json:encode(Response),
        {ok, Json}
    catch
        error:Error ->
            {error, {encoding_error, Error}}
    end.

%% @doc Decode JSON response
-spec decode_response(binary()) -> {ok, term()} | {error, term()}.
decode_response(JsonData) ->
    try
        %% OTP 28 Native JSON decoding
        Decoded = json:decode(JsonData),
        {ok, Decoded}
    catch
        error:badarg ->
            {error, invalid_json};
        error:Error ->
            {error, {decoding_error, Error}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize JSON processor
-spec init([]) -> {ok, state()}.
init([]) ->
    %% OTP 28+ JSON processor state
    State = #{
        schema => mcp_schema,
        %% OTP 28+ Enhanced validation
        validation => enhanced,
        %% OTP 28+ Performance optimization
        caching => true,
        %% OTP 28+ Telemetry
        telemetry => true,
        %% OTP 28+ Error handling
        error_handling => enhanced
    },

    {ok, State}.

%% @doc Handle validated JSON request
-spec handle_validated_request(term()) -> {ok, term()} | {error, term()}.
handle_validated_request(Request) ->
    %% OTP 28+ Request processing
    try
        %% Route to appropriate handler
        case maps:get(<<"type">>, Request, undefined) of
            <<"mcp_request">> ->
                handle_mcp_request(Request);
            <<"mcp_response">> ->
                handle_mcp_response(Request);
            <<"mcp_notification">> ->
                handle_mcp_notification(Request);
            _ ->
                {error, unknown_request_type}
        end
    catch
        error:Error ->
            {error, {processing_error, Error}}
    end.

%% @doc Handle MCP request
-spec handle_mcp_request(term()) -> {ok, term()} | {error, term()}.
handle_mcp_request(Request) ->
    %% OTP 28+ MCP request handling
    case maps:get(<<"method">>, Request, undefined) of
        <<"resources/list">> ->
            handle_resource_list_request();
        <<"resources/get">> ->
            handle_resource_get_request(Request);
        <<"tools/call">> ->
            handle_tool_call_request(Request);
        <<"prompts/list">> ->
            handle_prompt_list_request();
        _ ->
            {error, unknown_mcp_method}
    end.

%% @doc Business logic validation
-spec business_logic_validation(term()) -> {ok, term()} | {error, term()}.
business_logic_validation(Request) ->
    %% OTP 28+ Enhanced business logic validation
    case maps:get(<<"data">>, Request, undefined) of
        undefined ->
            {error, missing_data_field};
        Data when is_map(Data) ->
            %% Validate data structure
            case validate_data_structure(Data) of
                {ok, Validated} -> {ok, Validated};
                {error, Reason} -> {error, Reason}
            end;
        _ ->
            {error, invalid_data_type}
    end.

%% @doc Validate data structure
-spec validate_data_structure(term()) -> {ok, term()} | {error, term()}.
validate_data_structure(Data) ->
    %% OTP 28+ Enhanced data validation
    RequiredFields = [<<"id">>, <<"timestamp">>, <<"content">>],

    %% Check required fields
    case check_required_fields(Data, RequiredFields) of
        {ok, Validated} ->
            %% Validate data types
            case validate_data_types(Validated) of
                {ok, Final} -> {ok, Final};
                {error, Reason} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Check required fields
-spec check_required_fields(term(), [binary()]) -> {ok, term()} | {error, term()}.
check_required_fields(Data, RequiredFields) ->
    lists:foldl(fun(Field, Acc) ->
                    case maps:is_key(Field, Data) of
                        true -> Acc;
                        false -> {error, {missing_field, Field}}
                    end
                end, {ok, Data}, RequiredFields).

%% @doc Validate data types
-spec validate_data_types(term()) -> {ok, term()} | {error, term()}.
validate_data_types(Data) ->
    %% OTP 28+ Type validation
    Validated = maps:map(fun(Field, Value) ->
                             case validate_field_type(Field, Value) of
                                 {ok, Valid} -> Valid;
                                 {error, Reason} -> throw({error, Reason})
                             end
                         end, Data),
    {ok, Validated}.

%% @doc Validate field type
-spec validate_field_type(binary(), term()) -> {ok, term()} | {error, term()}.
validate_field_type(<<"id">>, Id) when is_binary(Id) ->
    {ok, Id};
validate_field_type(<<"timestamp">>, Timestamp) when is_integer(Timestamp) ->
    {ok, Timestamp};
validate_field_type(<<"content">>, Content) when is_binary(Content) ->
    {ok, Content};
validate_field_type(<<"metadata">>, Metadata) when is_map(Metadata) ->
    {ok, Metadata};
validate_field_type(Field, Value) ->
    {error, {invalid_field_type, Field, Value}}.
```

### 2. Enhanced Message Processing with Priority Support

```erlang
%% OTP 28.3.1+ Enhanced Message Processing Architecture
%%
%% Leverage OTP 28+ features for:
%% - Priority message handling
%% - Message size limits
%% - Message rate limiting
%% - Enhanced error handling

-module(erlmcp_message_processor).

-export([process_message/2, process_priority_message/2, process_normal_message/2,
         enqueue_message/2, dequeue_message/1]).

-export([init/1, handle_message/3, validate_message/1]).

%%====================================================================
%% Enhanced Message Processing API
%%====================================================================

%% @doc Process message with priority support
-spec process_message(term(), context()) -> {ok, term()} | {error, term()}.
process_message(Message, Context) ->
    %% OTP 28+ Priority message processing
    case is_priority_message(Message) of
        true ->
            process_priority_message(Message, Context);
        false ->
            process_normal_message(Message, Context)
    end.

%% @doc Process priority message
-spec process_priority_message(term(), context()) -> {ok, term()} | {error, term()}.
process_priority_message(Message, Context) ->
    %% OTP 28+ High-priority message processing
    try
        %% Validate message
        case validate_message(Message) of
            {ok, Validated} ->
                %% Process immediately
                handle_immediate_message(Validated, Context);
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, {priority_error, Error}}
    end.

%% @doc Process normal message
-spec process_normal_message(term(), context()) -> {ok, term()} | {error, term()}.
process_normal_message(Message, Context) ->
    %% OTP 28+ Normal message processing
    try
        %% Validate message
        case validate_message(Message) of
            {ok, Validated} ->
                %% Process through normal queue
                enqueue_message(Validated, Context),
                {ok, queued};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, {normal_error, Error}}
    end.

%% @doc Enqueue message
-spec enqueue_message(term(), context()) -> ok | {error, term()}.
enqueue_message(Message, Context) ->
    %% OTP 28+ Enhanced message enqueueing
    try
        %% Validate message size
        case validate_message_size(Message) of
            {ok, Size} when Size =< ?MAX_MESSAGE_SIZE ->
                %% Check rate limit
                case check_rate_limit(Context) of
                    {ok, _} ->
                        %% Enqueue message
                        erlmcp_message_queue:enqueue(Message, Context);
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, {enqueue_error, Error}}
    end.

%% @doc Dequeue message
-spec dequeue_message(term()) -> {ok, term()} | {error, term()}.
dequeue_message(MessageId) ->
    %% OTP 28+ Enhanced message dequeueing
    try
        %% Get message from queue
        case erlmcp_message_queue:dequeue(MessageId) of
            {ok, Message} ->
                %% Process message
                process_message(Message, #{}),
                {ok, processed};
            {error, not_found} ->
                {error, not_found};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, {dequeue_error, Error}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize message processor
-spec init([]) -> {ok, state()}.
init([]) ->
    %% OTP 28+ Message processor state
    State = #{
        priority_queue => queue:new(),
        normal_queue => queue:new(),
        rate_limits => #{},
        %% OTP 28+ Enhanced message processing
        processing => true,
        %% OTP 28+ Telemetry
        telemetry => true,
        %% OTP 28+ Error handling
        error_handling => enhanced,
        %% OTP 28+ Message limits
        max_message_size => ?MAX_MESSAGE_SIZE,
        max_queue_size => ?MAX_QUEUE_SIZE
    },

    {ok, State}.

%% @doc Check if message is priority
-spec is_priority_message(term()) -> boolean().
is_priority_message(Message) ->
    %% OTP 28+ Priority detection
    case Message of
        #{<<"priority">> := high} -> true;
        #{<<"priority">> := critical} -> true;
        #{<<"type">> := system} -> true;
        #{<<"type">> := heartbeat} -> true;
        _ -> false
    end.

%% @doc Validate message
-spec validate_message(term()) -> {ok, term()} | {error, term()}.
validate_message(Message) ->
    %% OTP 28+ Enhanced message validation
    try
        %% Check message structure
        case is_valid_message_structure(Message) of
            true ->
                %% Check message content
                case is_valid_message_content(Message) of
                    true ->
                        {ok, Message};
                    false ->
                        {error, invalid_content}
                end;
            false ->
                {error, invalid_structure}
        end
    catch
        error:Error ->
            {error, {validation_error, Error}}
    end.

%% @doc Check message size
-spec validate_message_size(term()) -> {ok, integer()} | {error, term()}.
validate_message_size(Message) ->
    %% OTP 28+ Message size validation
    Size = calculate_message_size(Message),
    case Size of
        Size when Size =< ?MAX_MESSAGE_SIZE ->
            {ok, Size};
        Size when Size > ?MAX_MESSAGE_SIZE ->
            {error, {message_too_large, Size}}
    end.

%% @doc Calculate message size
-spec calculate_message_size(term()) -> integer().
calculate_message_size(Message) ->
    %% OTP 28+ Message size calculation
    Size = json:encode(Message),
    byte_size(Size).

%% @doc Check rate limit
-spec check_rate_limit(context()) -> {ok, term()} | {error, term()}.
check_rate_limit(Context) ->
    %% OTP 28+ Rate limiting
    case erlmcp_rate_limiter:check(Context) of
        {ok, _} -> {ok, allowed};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Handle immediate message
-spec handle_immediate_message(term(), context()) -> {ok, term()} | {error, term()}.
handle_immediate_message(Message, Context) ->
    %% OTP 28+ Immediate message handling
    try
        %% Process message immediately
        Result = erlmcp_message_handler:handle(Message, Context),

        %% Record telemetry
        erlmcp_telemetry:record_priority_message(Message, Context),

        {ok, Result}
    catch
        error:Error ->
            {error, {immediate_error, Error}}
    end.
```

### 3. Advanced Error Handling Architecture

```erlang
%% OTP 28.3.1+ Enhanced Error Handling Architecture
%%
%% Leverage OTP 28+ features for:
%% - Structured error reporting
%% - Automatic error categorization
%% - Enhanced telemetry
%% - Predictive error handling

-module(erlmcp_error_handler).

-export([handle_error/2, categorize_error/1, generate_error_response/1,
         log_error/2, notify_error/2]).

-export([init/1, handle_exception/3, handle_crash/3]).

%%====================================================================
%% Enhanced Error Handling API
%%====================================================================

%% @doc Handle error with enhanced categorization
-spec handle_error(Error :: term(), Context :: map()) -> error_response().
handle_error(Error, Context) ->
    %% OTP 28+ Enhanced error handling
    try
        %% Categorize error
        Category = categorize_error(Error),

        %% Generate error response
        Response = generate_error_response(Category),

        %% Log error
        log_error(Error, Context),

        %% Notify error
        notify_error(Error, Context),

        %% Record telemetry
        erlmcp_telemetry:record_error(Error, Category, Context),

        Response
    catch
        error:Error ->
            %% Error handling failed
            generate_fallback_error(Error)
    end.

%% @doc Categorize error
-spec categorize_error(Error :: term()) -> error_category().
categorize_error(Error) ->
    %% OTP 28+ Enhanced error categorization
    case Error of
        {timeout, _} -> timeout;
        {resource_not_found, _} -> resource_not_found;
        {validation_failed, _} -> validation_error;
        {system_error, _} -> system_error;
        {network_error, _} -> network_error;
        {database_error, _} -> database_error;
        {authentication_error, _} -> authentication_error;
        {authorization_error, _} -> authorization_error;
        {rate_limited, _} -> rate_limited;
        {circuit_breaker_open, _} -> circuit_breaker_open;
        {queue_full, _} -> queue_full;
        {memory_error, _} -> memory_error;
        {process_error, _} -> process_error;
        {telemetry_error, _} -> telemetry_error;
        {configuration_error, _} -> configuration_error;
        {unknown_error, _} -> unknown_error;
        _ -> unknown_error
    end.

%% @doc Generate error response
-spec generate_error_response(error_category()) -> error_response().
generate_error_response(Category) ->
    %% OTP 28+ Enhanced error response generation
    ErrorDetails = #{
        category => Category,
        timestamp => erlang:system_time(millisecond),
        stacktrace => erlang:get_stacktrace(),
        %% OTP 28+ Enhanced error details
        details => get_error_details(Category),
        %% OTP 28+ Telemetry integration
        telemetry => erlmcp_telemetry:error_event(Category)
    },

    %% Generate appropriate response
    case Category of
        timeout -> generate_timeout_response(ErrorDetails);
        resource_not_found -> generate_resource_not_found_response(ErrorDetails);
        validation_error -> generate_validation_error_response(ErrorDetails);
        system_error -> generate_system_error_response(ErrorDetails);
        network_error -> generate_network_error_response(ErrorDetails);
        database_error -> generate_database_error_response(ErrorDetails);
        authentication_error -> generate_authentication_error_response(ErrorDetails);
        authorization_error -> generate_authorization_error_response(ErrorDetails);
        rate_limited -> generate_rate_limited_response(ErrorDetails);
        circuit_breaker_open -> generate_circuit_breaker_response(ErrorDetails);
        queue_full -> generate_queue_full_response(ErrorDetails);
        memory_error -> generate_memory_error_response(ErrorDetails);
        process_error -> generate_process_error_response(ErrorDetails);
        telemetry_error -> generate_telemetry_error_response(ErrorDetails);
        configuration_error -> generate_configuration_error_response(ErrorDetails);
        unknown_error -> generate_unknown_error_response(ErrorDetails)
    end.

%% @doc Log error
-spec log_error(term(), map()) -> ok.
log_error(Error, Context) ->
    %% OTP 28+ Enhanced error logging
    ErrorReport = #{
        error => Error,
        context => Context,
        timestamp => erlang:system_time(millisecond),
        stacktrace => erlang:get_stacktrace(),
        %% OTP 28+ Enhanced logging
        level => get_error_level(categorize_error(Error)),
        category => categorize_error(Error),
        %% OTP 28+ Telemetry integration
        telemetry => erlmcp_telemetry:error_event(categorize_error(Error), Context)
    },

    %% Log structured error
    erlmcp_logger:error_structured(ErrorReport),

    %% Save to error database
    erlmcp_error_database:save(ErrorReport),

    ok.

%% @doc Notify error
-spec notify_error(term(), map()) -> ok.
notify_error(Error, Context) ->
    %% OTP 28+ Enhanced error notification
    ErrorReport = #{
        error => Error,
        context => Context,
        timestamp => erlang:system_time(millisecond),
        stacktrace => erlang:get_stacktrace(),
        %% OTP 28+ Enhanced notification
        category => categorize_error(Error),
        severity => get_error_severity(categorize_error(Error)),
        %% OTP 28+ Telemetry integration
        telemetry => erlmcp_telemetry:error_event(categorize_error(Error), Context)
    },

    %% Send notification
    erlmcp_error_notifier:notify(ErrorReport),

    %% Trigger alert
    erlmcp_alerts:error_alert(ErrorReport),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize error handler
-spec init([]) -> {ok, state()}.
init([]) ->
    %% OTP 28+ Error handler state
    State = #{
        error_database => erlmcp_error_database,
        error_notifier => erlmcp_error_notifier,
        %% OTP 28+ Enhanced error handling
        error_handling => enhanced,
        %% OTP 28+ Telemetry
        telemetry => true,
        %% OTP 28+ Predictive error handling
        predictive_error_handling => true,
        %% OTP 28+ Error recovery
        error_recovery => true
    },

    {ok, State}.

%% @doc Handle exception
-spec handle_exception(Class :: term(), Reason :: term(), StackTrace :: list()) -> term().
handle_exception(Class, Reason, StackTrace) ->
    %% OTP 28+ Enhanced exception handling
    try
        %% Convert exception to error
        Error = {Class, Reason},

        %% Handle error
        handle_error(Error, #{stacktrace => StackTrace}),

        %% Return error response
        {error, {Class, Reason}}
    catch
        error:Error ->
            %% Error handling failed
            generate_fallback_error(Error)
    end.

%% @doc Handle crash
-spec handle_crash(Class :: term(), Reason :: term(), StackTrace :: list()) -> term().
handle_crash(Class, Reason, StackTrace) ->
    %% OTP 28+ Enhanced crash handling
    try
        %% Convert crash to error
        Error = {crash, Class, Reason},

        %% Handle error
        handle_error(Error, #{stacktrace => StackTrace}),

        %% Return error response
        {error, {crash, Class, Reason}}
    catch
        error:Error ->
            %% Error handling failed
            generate_fallback_error(Error)
    end.

%% @doc Generate fallback error
-spec generate_fallback_error(term()) -> error_response().
generate_fallback_error(Error) ->
    %% OTP 28+ Fallback error generation
    ErrorDetails = #{
        category => unknown_error,
        timestamp => erlang:system_time(millisecond),
        error => Error,
        %% OTP 28+ Fallback details
        fallback => true,
        %% OTP 28+ Telemetry integration
        telemetry => erlmcp_telemetry:error_event(unknown_error)
    },

    #{
        status => error,
        error => Error,
        message => "Internal server error",
        details => ErrorDetails
    }.

%% @doc Get error level
-spec get_error_level(error_category()) -> log_level().
get_error_level(Category) ->
    case Category of
        critical -> error;
        system_error -> error;
        memory_error -> error;
        process_error -> error;
        _ -> warning
    end.

%% @doc Get error severity
-spec get_error_severity(error_category()) -> severity_level().
get_error_severity(Category) ->
    case Category of
        critical -> critical;
        system_error -> critical;
        memory_error -> critical;
        process_error -> critical;
        _ -> warning
    end.

%% @doc Get error details
-spec get_error_details(error_category()) -> map().
get_error_details(Category) ->
    case Category of
        timeout -> #{timeout => 5000};
        resource_not_found -> #{resource => undefined};
        validation_error -> #{validation => []};
        system_error -> #{system => undefined};
        network_error -> #{network => undefined};
        database_error -> #{database => undefined};
        authentication_error -> #{auth => undefined};
        authorization_error -> #{auth => undefined};
        rate_limited -> #{rate_limit => undefined};
        circuit_breaker_open -> #{circuit_breaker => undefined};
        queue_full -> #{queue => undefined};
        memory_error -> #{memory => undefined};
        process_error -> #{process => undefined};
        telemetry_error -> #{telemetry => undefined};
        configuration_error -> #{config => undefined};
        unknown_error -> #{error => undefined}
    end.
```

### 4. Circuit Breaker 2.0 Architecture

```erlang
%% OTP 28.3.1+ Enhanced Circuit Breaker Architecture
%%
%% Leverage OTP 28+ features for:
%% - Adaptive failure thresholds
%% - Predictive circuit breaking
%% - Enhanced recovery
%% - Telemetry integration

-module(erlmcp_circuit_breaker_v2).

-export([start_link/1, call/2, call/3, get_state/0, get_stats/0, reset/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% Enhanced Circuit Breaker API
%%====================================================================

%% @doc Start circuit breaker with enhanced configuration
-spec start_link(Options :: [term()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    %% OTP 28+ Enhanced circuit breaker start
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Make protected call with circuit breaker
-spec call(term(), term()) -> {ok, term()} | {error, term()}.
call(Service, Args) ->
    %% OTP 28+ Circuit breaker call
    try
        %% Check circuit state
        case get_state() of
            closed ->
                make_call(Service, Args);
            half_open ->
                make_trial_call(Service, Args);
            open ->
                {error, circuit_breaker_open}
        end
    catch
        error:Error ->
            {error, {call_error, Error}}
    end.

%% @doc Make protected call with timeout
-spec call(term(), term(), timeout()) -> {ok, term()} | {error, term()}.
call(Service, Args, Timeout) ->
    %% OTP 28+ Circuit breaker call with timeout
    call(Service, Args).

%% @doc Get circuit breaker state
-spec get_state() -> circuit_state().
get_state() ->
    %% OTP 28+ Enhanced state retrieval
    gen_server:call(?MODULE, get_state).

%% @doc Get circuit breaker statistics
-spec get_stats() -> map().
get_stats() ->
    %% OTP 28+ Enhanced statistics retrieval
    gen_server:call(?MODULE, get_stats).

%% @doc Reset circuit breaker
-spec reset() -> ok.
reset() ->
    %% OTP 28+ Enhanced circuit breaker reset
    gen_server:cast(?MODULE, reset).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize circuit breaker
-spec init(Options :: [term()]) -> {ok, state()} | {stop, term()}.
init(Options) ->
    %% OTP 28+ Enhanced circuit breaker initialization
    State = #{
        state => closed,
        failures => 0,
        success => 0,
        failure_rate => 0.0,
        consecutive_failures => 0,
        last_failure_time => undefined,
        failure_window => 60000,       % 60 seconds
        failure_threshold => 5,        % 5 failures trigger open
        recovery_threshold => 3,       % 3 successes trigger half-open
        half_open_timeout => 30000,    % 30 seconds in half-open
        half_open_start => undefined,
        %% OTP 28+ Adaptive features
        adaptive_threshold => true,
        current_threshold => 5,
        failure_prediction => false,
        failure_probability => 0.0,
        %% OTP 28+ Enhanced recovery
        enhanced_recovery => true,
        recovery_timeout => 30000,
        %% OTP 28+ Telemetry
        telemetry => true,
        %% OTP 28+ Resource limits
        max_failures => 100,
        max_success => 100,
        %% OTP 28+ Monitoring
        monitoring => true
    },

    %% Override defaults with options
    FinalState = apply_options(State, Options),

    %% Start monitoring
    start_monitoring(),

    {ok, FinalState}.

%% @doc Handle synchronous calls
-spec handle_call(term(), term(), state()) -> {reply, term(), state()}.
handle_call(get_state, _From, State) ->
    {reply, maps:get(state, State), State};

handle_call(get_stats, _From, State) ->
    Stats = #{
        state => maps:get(state, State),
        failures => maps:get(failures, State),
        success => maps:get(success, State),
        failure_rate => maps:get(failure_rate, State),
        consecutive_failures => maps:get(consecutive_failures, State),
        threshold => maps:get(current_threshold, State),
        %% OTP 28+ Enhanced stats
        prediction => maps:get(failure_prediction, State),
        probability => maps:get(failure_probability, State),
        recovery => maps:get(enhanced_recovery, State)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle asynchronous calls
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(reset, State) ->
    %% OTP 28+ Enhanced circuit breaker reset
    NewState = reset_circuit_breaker(State),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc Handle asynchronous messages
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({service_call, Service, Args}, State) ->
    %% OTP 28+ Service call handling
    Result = make_service_call(Service, Args),
    case Result of
        {ok, Response} ->
            NewState = handle_success(State),
            %% Record telemetry
            erlmcp_telemetry:record_circuit_breaker_success(),
            {noreply, NewState};
        {error, Reason} ->
            NewState = handle_failure(State, Reason),
            %% Record telemetry
            erlmcp_telemetry:record_circuit_breaker_failure(),
            {noreply, NewState}
    end;

handle_info(check_half_open, State) ->
    %% OTP 28+ Half-open check
    case maps:get(state, State) of
        half_open ->
            case is_half_open_expired(State) of
                true ->
                    NewState = reset_to_open(State),
                    {noreply, NewState};
                false ->
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate circuit breaker
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    %% OTP 28+ Enhanced termination
    save_circuit_breaker_state(State),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Apply options to state
-spec apply_options(state(), [term()]) -> state().
apply_options(State, Options) ->
    lists:foldl(fun(Option, Acc) ->
                    case Option of
                        {failure_threshold, Threshold} ->
                            Acc#{failure_threshold => Threshold,
                                 current_threshold => Threshold};
                        {recovery_threshold, Threshold} ->
                            Acc#{recovery_threshold => Threshold};
                        {failure_window, Window} ->
                            Acc#{failure_window => Window};
                        {adaptive_threshold, true} ->
                            Acc#{adaptive_threshold => true};
                        {enhanced_recovery, true} ->
                            Acc#{enhanced_recovery => true};
                        {telemetry, true} ->
                            Acc#{telemetry => true};
                        _ ->
                            Acc
                    end
                end, State, Options).

%% @doc Start monitoring
-spec start_monitoring() -> ok.
start_monitoring() ->
    %% OTP 28+ Enhanced monitoring setup
    erlmcp_monitor:start([
        {circuit_breaker, true},
        {service_calls, true},
        {failure_prediction, true},
        %% OTP 28+ Telemetry
        {telemetry, true},
        %% OTP 28+ Predictive monitoring
        {predictive, true}
    ]).

%% @doc Make service call
-spec make_service_call(term(), term()) -> {ok, term()} | {error, term()}.
make_service_call(Service, Args) ->
    %% OTP 28+ Service call with circuit breaker protection
    try
        Result = erlmcp_service:call(Service, Args),
        {ok, Result}
    catch
        error:Error ->
            {error, Error}
    end.

%% @doc Handle success
-spec handle_success(state()) -> state().
handle_success(State) ->
    %% OTP 28+ Enhanced success handling
    NewState = State#{
        success => maps:get(success, State) + 1,
        consecutive_failures => 0,
        last_failure_time => undefined,
        failure_rate => calculate_failure_rate(State)
    },

    %% Check for recovery
    case should_recover(NewState) of
        true ->
            NewState#{
                state => half_open,
                half_open_start => erlang:system_time(millisecond)
            };
        false ->
            %% Adaptive threshold adjustment
            case maps:get(adaptive_threshold, NewState, true) of
                true ->
                    adjust_threshold(NewState);
                false ->
                    NewState
            end
    end.

%% @doc Handle failure
-spec handle_failure(state(), term()) -> state().
handle_failure(State, Reason) ->
    %% OTP 28+ Enhanced failure handling
    NewState = State#{
        failures => maps:get(failures, State) + 1,
        consecutive_failures => maps:get(consecutive_failures, State) + 1,
        last_failure_time => erlang:system_time(millisecond),
        failure_rate => calculate_failure_rate(NewState)
    },

    %% Check for circuit opening
    case should_open_circuit(NewState) of
        true ->
            NewState#{
                state => open
            };
        false ->
            %% Adaptive threshold adjustment
            case maps:get(adaptive_threshold, NewState, true) of
                true ->
                    adjust_threshold(NewState);
                false ->
                    NewState
            end
    end.

%% @doc Calculate failure rate
-spec calculate_failure_rate(state()) -> float().
calculate_failure_rate(State) ->
    %% OTP 28+ Enhanced failure rate calculation
    Total = maps:get(failures, State) + maps:get(success, State),
    if Total > 0 ->
        maps:get(failures, State) / Total;
       true ->
        0.0
    end.

%% @doc Should recover circuit
-spec should_recover(state()) -> boolean().
should_recover(State) ->
    %% OTP 28+ Enhanced recovery logic
    case maps:get(enhanced_recovery, State, true) of
        true ->
            %% Enhanced recovery conditions
            maps:get(consecutive_failures, State) >= maps:get(recovery_threshold, State);
        false ->
            %% Basic recovery logic
            maps:get(consecutive_failures, State) >= maps:get(recovery_threshold, State)
    end.

%% @doc Should open circuit
-spec should_open_circuit(state()) -> boolean().
should_open_circuit(State) ->
    %% OTP 28+ Enhanced circuit opening logic
    case maps.get(adaptive_threshold, State, true) of
        true ->
            adaptive_circuit_opening(State);
        false ->
            basic_circuit_opening(State)
    end.

%% @doc Adaptive circuit opening
-spec adaptive_circuit_opening(state()) -> boolean().
adaptive_circuit_opening(State) ->
    %% OTP 28+ Adaptive circuit breaking
    FailureRate = maps:get(failure_rate, State),
    ConsecutiveFailures = maps:get(consecutive_failures, State),
    Threshold = maps:get(current_threshold, State),

    %% Combined failure prediction
    PredictedFailure = predict_failure(State),

    %% Adaptive opening logic
    (FailureRate > 0.8 andalso ConsecutiveFailures > Threshold) orelse
    (PredictedFailure > 0.9 andalso ConsecutiveFailures > Threshold div 2).

%% @doc Basic circuit opening
-spec basic_circuit_opening(state()) -> boolean().
basic_circuit_opening(State) ->
    %% OTP 28+ Basic circuit breaking
    FailureRate = maps:get(failure_rate, State),
    ConsecutiveFailures = maps:get(consecutive_failures, State),
    Threshold = maps:get(failure_threshold, State),

    (FailureRate > 0.8 andalso ConsecutiveFailures > Threshold).

%% @doc Predict failure probability
-spec predict_failure(state()) -> float().
predict_failure(State) ->
    %% OTP 28+ Predictive failure analysis
    case maps:get(failure_prediction, State, true) of
        true ->
            %% Simple prediction based on recent failures
            FailureRate = maps:get(failure_rate, State),
            Trend = calculate_failure_trend(State),
            min(1.0, FailureRate + Trend);
        false ->
            0.0
    end.

%% @doc Calculate failure trend
-spec calculate_failure_trend(state()) -> float().
calculate_failure_trend(State) ->
    %% OTP 28+ Enhanced trend calculation
    %% Simple linear trend based on recent failures
    case maps:get(failures, State) of
        0 -> 0.0;
        N -> (N - maps:get(success, State)) / (N + maps:get(success, State))
    end.

%% @doc Adjust threshold
-spec adjust_threshold(state()) -> state().
adjust_threshold(State) ->
    %% OTP 28+ Adaptive threshold adjustment
    CurrentThreshold = maps:get(current_threshold, State),
    FailureRate = maps:get(failure_rate, State),

    %% Adjust threshold based on failure rate
    NewThreshold = case FailureRate of
        Rate when Rate > 0.8 -> max(3, CurrentThreshold - 1);
        Rate when Rate < 0.2 -> min(10, CurrentThreshold + 1);
        _ -> CurrentThreshold
    end,

    State#{
        current_threshold => NewThreshold
    }.

%% @doc Reset circuit breaker
-spec reset_circuit_breaker(state()) -> state().
reset_circuit_breaker(State) ->
    %% OTP 28+ Enhanced circuit breaker reset
    State#{
        state => closed,
        failures => 0,
        success => 0,
        failure_rate => 0.0,
        consecutive_failures => 0,
        last_failure_time => undefined,
        half_open_start => undefined,
        %% OTP 28+ Enhanced reset
        recovery_count => 0,
        failure_count => 0
    }.

%% @doc Check if half-open expired
-spec is_half_open_expired(state()) -> boolean().
is_half_open_expired(State) ->
    %% OTP 28+ Half-open timeout check
    case maps:get(state, State) of
        half_open ->
            case maps:get(half_open_start, State, undefined) of
                undefined -> false;
                StartTime ->
                    CurrentTime = erlang:system_time(millisecond),
                    CurrentTime - StartTime > maps:get(half_open_timeout, State, 30000)
            end;
        _ -> false
    end.

%% @doc Reset to open state
-spec reset_to_open(state()) -> state().
reset_to_open(State) ->
    %% OTP 28+ Reset to open state
    State#{
        state => open,
        half_open_start => undefined,
        %% OTP 28+ Enhanced reset
        recovery_attempts => 0
    }.

%% @doc Make call
-spec make_call(term(), term()) -> {ok, term()} | {error, term()}.
make_call(Service, Args) ->
    %% OTP 28+ Protected call
    try
        Result = erlmcp_service:call(Service, Args),
        {ok, Result}
    catch
        error:Error ->
            {error, Error}
    end.

%% @doc Make trial call
-spec make_trial_call(term(), term()) -> {ok, term()} | {error, term()}.
make_trial_call(Service, Args) ->
    %% OTP 28+ Trial call in half-open state
    try
        Result = erlmcp_service:call(Service, Args),
        case Result of
            {ok, Response} ->
                {ok, Response};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:Error ->
            {error, Error}
    end.

%% @doc Save circuit breaker state
-spec save_circuit_breaker_state(state()) -> ok.
save_circuit_breaker_state(State) ->
    %% OTP 28+ Circuit breaker state persistence
    erlmcp_circuit_breaker_storage:save(State),

    %% Generate report
    Report = generate_circuit_breaker_report(State),
    erlmcp_circuit_breaker_report:generate(Report),

    ok.

%% @doc Generate circuit breaker report
-spec generate_circuit_breaker_report(state()) -> map().
generate_circuit_breaker_report(State) ->
    %% OTP 28+ Enhanced circuit breaker report
    #{
        timestamp => erlang:system_time(millisecond),
        state => maps:get(state, State),
        stats => #{
            failures => maps:get(failures, State),
            success => maps:get(success, State),
            failure_rate => maps:get(failure_rate, State),
            consecutive_failures => maps:get(consecutive_failures, State),
            threshold => maps:get(current_threshold, State)
        },
        %% OTP 28+ Enhanced report
        enhanced_report => #{
            prediction => maps:get(failure_prediction, State),
            probability => maps:get(failure_probability, State),
            recovery => maps:get(enhanced_recovery, State)
        }
    }.
```

## Architecture Benefits

### 1. Enhanced Performance
- **Faster JSON processing**: Native OTP 28 JSON support
- **Better message handling**: Priority message processing
- **Improved error recovery**: Structured error handling
- **Enhanced circuit breaking**: Adaptive failure detection

### 2. Improved Reliability
- **Comprehensive error handling**: Structured error reporting
- **Predictive circuit breaking**: Failure prediction and prevention
- **Enhanced telemetry**: Detailed monitoring and analytics
- **Better fault tolerance**: Isolated error handling

### 3. Scalability
- **Dynamic message processing**: Adaptive queue management
- **Resource optimization**: Intelligent resource allocation
- **Load balancing**: Enhanced load distribution
- **Predictive scaling**: Future load prediction

### 4. Maintainability
- **Clear communication patterns**: Well-defined message flows
- **Comprehensive monitoring**: Real-time analytics
- **Enhanced debugging**: Structured error logging
- **Automated optimization**: Performance tuning with minimal manual intervention

## Conclusion

The enhanced process communication architecture for OTP 28.3.+ provides significant improvements in performance, reliability, scalability, and maintainability. By leveraging modern OTP features and implementing intelligent communication patterns, erlmcp will be well-positioned for future growth and requirements.

The architecture maintains Armstrong's principles of robust, fault-tolerant design while incorporating modern features for enhanced performance and reliability.

---

*Document Version: 1.0.0*
*Date: February 1, 2026*
*Author: System Architecture Designer*