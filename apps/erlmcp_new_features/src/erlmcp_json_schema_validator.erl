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
    #state{schemas = Schemas, compiled = Compiled} = State,
    case maps:find(SchemaName, Schemas) of
        {ok, Schema} ->
            case validate_data(Data, Schema) of
                {ok, _} = Ok ->
                    {reply, Ok, State};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        error ->
            {reply, {error, [<<"Schema not found: ", (atom_to_binary(SchemaName))/binary>>]}, State}
    end;

handle_call({load_schema, Name, Schema}, _From, State) ->
    #state{schemas = Schemas} = State,
    try
        validate_schema_structure(Schema),
        NewSchemas = maps:put(Name, Schema, Schemas),
        {reply, ok, State#state{schemas = NewSchemas}}
    catch
        _:Error ->
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
