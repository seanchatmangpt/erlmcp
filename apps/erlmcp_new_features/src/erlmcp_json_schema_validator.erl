-module(erlmcp_json_schema_validator).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([validate/2]).
-export([load_schema/2]).
-export([unload_schema/1]).
-export([list_schemas/0]).
-export([compile_schema/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-type schema_name() :: atom().
-type schema() :: map().
-type validation_result() :: {ok, map()} | {error, [binary()]}.
-type validation_error() :: #{path => binary(), error => binary()}.

-record(state, {
    schemas = #{},
    compiled = #{}
}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec validate(schema_name(), map()) -> validation_result().
validate(SchemaName, Data) when is_atom(SchemaName), is_map(Data) ->
    gen_server:call(?SERVER, {validate, SchemaName, Data}).

-spec load_schema(schema_name(), schema()) -> ok | {error, term()}.
load_schema(Name, Schema) when is_atom(Name), is_map(Schema) ->
    gen_server:call(?SERVER, {load_schema, Name, Schema}).

-spec unload_schema(schema_name()) -> ok | {error, term()}.
unload_schema(Name) when is_atom(Name) ->
    gen_server:call(?SERVER, {unload_schema, Name}).

-spec list_schemas() -> [schema_name()].
list_schemas() ->
    gen_server:call(?SERVER, list_schemas).

-spec compile_schema(schema()) -> {ok, schema()} | {error, term()}.
compile_schema(Schema) when is_map(Schema) ->
    try
        % Validate schema structure
        validate_schema_structure(Schema),
        % In real implementation, compile to validation functions
        {ok, Schema}
    catch
        _:Error -> {error, {compilation_failed, Error}}
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{}}.

handle_call({validate, SchemaName, Data}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    TraceContext = erlmcp_observability:trace_span(<<"json_schema_validator_validate">>, #{
        schema_name => atom_to_binary(SchemaName),
        data_size => size(jsone:encode(Data))
    }),

    % Record validation request
    erlmcp_observability:counter(<<"json_schema_validator_requests_total">>, #{
        service => ?MODULE
    }),

    #state{schemas = Schemas, compiled = Compiled} = State,
    case maps:find(SchemaName, Schemas) of
        {ok, Schema} ->
            case validate_data(Data, Schema) of
                {ok, _} = Ok ->
                    Duration = erlang:monotonic_time(millisecond) - StartTime,

                    % Record success metrics
                    erlmcp_observability:counter(<<"json_schema_validator_requests_total">>, #{
                        service => ?MODULE,
                        status => "success"
                    }),
                    erlmcp_observability:histogram_observe(<<"json_schema_validator_duration_ms">>, Duration),
                    erlmcp_observability:gauge_set(<<"json_schema_validator_active_connections">>, #{
                        service => ?MODULE
                    }, 1),

                    erlmcp_observability:log(<<"json_validation_success">>, #{
                        schema_name => atom_to_binary(SchemaName),
                        duration => Duration,
                        data_size => size(jsone:encode(Data))
                    }, TraceContext),
                    erlmcp_observability:trace_span(<<"validation_result">>, #{
                        result => success,
                        duration => Duration
                    }, TraceContext),

                    {reply, Ok, State};
                {error, _} = Error ->
                    Duration = erlang:monotonic_time(millisecond) - StartTime,

                    % Record error metrics
                    erlmcp_observability:counter(<<"json_schema_validator_requests_total">>, #{
                        service => ?MODULE,
                        status => "error"
                    }),
                    erlmcp_observability:histogram_observe(<<"json_schema_validator_duration_ms">>, Duration),
                    erlmcp_observability:counter(<<"json_schema_validator_errors_total">>, #{
                        type => "validation",
                        schema_name => atom_to_binary(SchemaName)
                    }),

                    erlmcp_observability:log(<<"json_validation_failed">>, #{
                        schema_name => atom_to_binary(SchemaName),
                        duration => Duration,
                        data_size => size(jsone:encode(Data))
                    }, TraceContext),
                    erlmcp_observability:trace_span(<<"validation_result">>, #{
                        result => error,
                        duration => Duration
                    }, TraceContext),

                    {reply, Error, State}
            end;
        error ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,

            % Record not found error
            erlmcp_observability:counter(<<"json_schema_validator_requests_total">>, #{
                service => ?MODULE,
                status => "error"
            }),
            erlmcp_observability:histogram_observe(<<"json_schema_validator_duration_ms">>, Duration),
            erlmcp_observability:counter(<<"json_schema_validator_errors_total">>, #{
                type => "schema_not_found",
                schema_name => atom_to_binary(SchemaName)
            }),

            erlmcp_observability:log(<<"json_schema_not_found">>, #{
                schema_name => atom_to_binary(SchemaName),
                duration => Duration
            }, TraceContext),
            erlmcp_observability:trace_span(<<"validation_result">>, #{
                result => error,
                reason => schema_not_found,
                duration => Duration
            }, TraceContext),

            {reply, {error, [<<"Schema not found: ", (atom_to_binary(SchemaName))/binary>>]}, State}
    end;

handle_call({load_schema, Name, Schema}, _From, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    TraceContext = erlmcp_observability:trace_span(<<"json_schema_validator_load">>, #{
        schema_name => atom_to_binary(Name),
        schema_size => size(jsone:encode(Schema))
    }),

    % Record load request
    erlmcp_observability:counter(<<"json_schema_validator_load_requests_total">>, #{
        service => ?MODULE
    }),

    #state{schemas = Schemas} = State,
    try
        validate_schema_structure(Schema),
        NewSchemas = maps:put(Name, Schema, Schemas),

        Duration = erlang:monotonic_time(millisecond) - StartTime,

        % Record success metrics
        erlmcp_observability:counter(<<"json_schema_validator_load_requests_total">>, #{
            service => ?MODULE,
            status => "success"
        }),
        erlmcp_observability:histogram_observe(<<"json_schema_validator_load_duration_ms">>, Duration),
        erlmcp_observability:gauge_set(<<"json_schema_validator_schema_count">>, #{
            service => ?MODULE
        }, map_size(NewSchemas)),

        erlmcp_observability:log(<<"json_schema_loaded">>, #{
            schema_name => atom_to_binary(Name),
            duration => Duration,
            schema_size => size(jsone:encode(Schema)),
            total_schemas => map_size(NewSchemas)
        }, TraceContext),

        {reply, ok, State#state{schemas = NewSchemas}}
    catch
        _:Error ->
            ErrorDuration = erlang:monotonic_time(millisecond) - StartTime,

            % Record error metrics
            erlmcp_observability:counter(<<"json_schema_validator_load_requests_total">>, #{
                service => ?MODULE,
                status => "error"
            }),
            erlmcp_observability:histogram_observe(<<"json_schema_validator_load_duration_ms">>, ErrorDuration),
            erlmcp_observability:counter(<<"json_schema_validator_errors_total">>, #{
                type => "load",
                schema_name => atom_to_binary(Name)
            }),

            erlmcp_observability:log(<<"json_schema_load_failed">>, #{
                schema_name => atom_to_binary(Name),
                duration => ErrorDuration,
                error => binary_to_list(io_lib:format("~p", [Error]))
            }, TraceContext),

            {reply, {error, {invalid_schema, Error}}, State}
    end;

handle_call({unload_schema, Name}, _From, State) ->
    #state{schemas = Schemas, compiled = Compiled} = State,
    NewSchemas = maps:remove(Name, Schemas),
    NewCompiled = maps:remove(Name, Compiled),
    {reply, ok, State#state{schemas = NewSchemas, compiled = NewCompiled}};

handle_call(list_schemas, _From, State) ->
    #state{schemas = Schemas} = State,
    Names = maps:keys(Schemas),
    {reply, Names, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

validate_schema_structure(Schema) ->
    % Ensure required fields exist
    case {maps:get(<<"type">>, Schema, undefined),
          maps:get(<<"$schema">>, Schema, undefined)} of
        {_, undefined} ->
            error({missing_field, <<"$schema">>});
        {undefined, _} ->
            error({missing_field, <<"type">>});
        {Type, SchemaVersion} when is_binary(Type), is_binary(SchemaVersion) ->
            ok;
        _ ->
            error({invalid_field_types})
    end.

validate_data(Data, Schema) ->
    % Basic JSON Schema validation
    Type = maps:get(<<"type">>, Schema, <<"object">>),
    case validate_type(Data, Type) of
        ok ->
            % Validate required properties if present
            Required = maps:get(<<"required">>, Schema, []),
            validate_required(Data, Required);
        {error, _} = Error ->
            Error
    end.

validate_type(Data, <<"object">>) when is_map(Data) -> ok;
validate_type(Data, <<"array">>) when is_list(Data) -> ok;
validate_type(Data, <<"string">>) when is_binary(Data) -> ok;
validate_type(Data, <<"number">>) when is_number(Data) -> ok;
validate_type(Data, <<"integer">>) when is_integer(Data) -> ok;
validate_type(Data, <<"boolean">>) when is_boolean(Data) -> ok;
validate_type(_, <<"null">>) -> ok;
validate_type(_, Type) ->
    {error, [<<"Type mismatch: expected ", Type/binary>>]}.

validate_required(_Data, []) ->
    {ok, #{}};
validate_required(Data, Required) when is_list(Required) ->
    Errors = lists:filtermap(
        fun(Field) ->
            case maps:get(Field, Data, undefined) of
                undefined -> {true, {Field, <<"Required field missing">>}};
                _ -> false
            end
        end,
        Required
    ),
    case Errors of
        [] -> {ok, #{}};
        _ -> {error, [format_error(E) || E <- Errors]}
    end.

format_error({Field, Message}) when is_atom(Field) ->
    <<(atom_to_binary(Field))/binary, ": ", Message/binary>>;
format_error({Field, Message}) when is_binary(Field) ->
    <<Field/binary, ": ", Message/binary>>;
format_error({Message}) when is_binary(Message) ->
    Message.
