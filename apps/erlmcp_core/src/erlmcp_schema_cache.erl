-module(erlmcp_schema_cache).
-behaviour(gen_server).

%% API exports - Fast zero-copy schema access via persistent_term
-export([
    start_link/0,
    cache_schema/2,
    cache_schema/3,
    get_schema/1,
    has_schema/1,
    validate/2,
    validate_with_options/3,
    invalidate/1,
    clear_all/0,
    list_schemas/0,
    get_stats/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Internal key macros
-define(SCHEMA_KEY(Name), {erlmcp_schema_cache, Name}).
-define(STATS_KEY, {erlmcp_schema_cache, stats}).

%% State record
-record(state, {
    cache_hits :: non_neg_integer(),
    cache_misses :: non_neg_integer(),
    validations :: non_neg_integer(),
    started_at :: erlang:timestamp()
}).

-type state() :: #state{}.
-type schema_name() :: atom() | binary().
-type schema_definition() :: map().
-type validation_options() :: #{
    allowed_errors => non_neg_integer(),
    schema_loader_fun => fun((binary()) -> schema_definition())
}.

-export_type([schema_name/0, schema_definition/0, validation_options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the schema cache server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Cache a compiled schema (without jesse dependency for POC)
%% In production, this would use jesse:compile/1
%% Access time: ~10ns vs ~1μs for ETS lookups
-spec cache_schema(schema_name(), schema_definition()) -> ok.
cache_schema(Name, Schema) ->
    cache_schema(Name, Schema, #{}).

%% @doc Cache a schema with options
-spec cache_schema(schema_name(), schema_definition(), map()) -> ok.
cache_schema(Name, Schema, _Options) when is_map(Schema) ->
    gen_server:call(?MODULE, {cache_schema, Name, Schema}).

%% @doc Get cached schema (fast zero-copy read)
-spec get_schema(schema_name()) -> {ok, schema_definition()} | {error, not_found}.
get_schema(Name) ->
    try
        Schema = persistent_term:get(?SCHEMA_KEY(Name)),
        gen_server:cast(?MODULE, {record_hit, Name}),
        {ok, Schema}
    catch
        error:badarg ->
            gen_server:cast(?MODULE, {record_miss, Name}),
            {error, not_found}
    end.

%% @doc Check if schema exists in cache
-spec has_schema(schema_name()) -> boolean().
has_schema(Name) ->
    try
        _ = persistent_term:get(?SCHEMA_KEY(Name)),
        true
    catch
        error:badarg -> false
    end.

%% @doc Validate data against cached schema
%% This is a POC - in production would use jesse:validate_with_schema/2
-spec validate(schema_name(), term()) -> ok | {error, term()}.
validate(Name, Data) ->
    validate_with_options(Name, Data, #{}).

%% @doc Validate with options
-spec validate_with_options(schema_name(), term(), validation_options()) ->
    ok | {error, term()}.
validate_with_options(Name, Data, Options) ->
    case get_schema(Name) of
        {ok, Schema} ->
            gen_server:cast(?MODULE, increment_validations),
            % POC validation - in production use jesse:validate_with_schema/3
            validate_data(Schema, Data, Options);
        {error, not_found} = Error ->
            Error
    end.

%% @doc Invalidate (remove) a cached schema
-spec invalidate(schema_name()) -> ok.
invalidate(Name) ->
    gen_server:call(?MODULE, {invalidate, Name}).

%% @doc Clear all cached schemas
-spec clear_all() -> ok.
clear_all() ->
    gen_server:call(?MODULE, clear_all).

%% @doc List all cached schema names
-spec list_schemas() -> [schema_name()].
list_schemas() ->
    AllTerms = persistent_term:get(),
    [
        Name
     || {{erlmcp_schema_cache, Name}, _} <- AllTerms, Name =/= stats
    ].

%% @doc Get cache statistics
-spec get_stats() ->
    #{
        cache_hits => non_neg_integer(),
        cache_misses => non_neg_integer(),
        validations => non_neg_integer(),
        hit_rate => float(),
        schema_count => non_neg_integer(),
        uptime_seconds => non_neg_integer()
    }.
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    % Initialize stats in persistent_term
    Stats = #{cache_hits => 0, cache_misses => 0, validations => 0},
    persistent_term:put(?STATS_KEY, Stats),

    logger:info("erlmcp_schema_cache initialized using persistent_term", []),

    {ok, #state{
        cache_hits = 0,
        cache_misses = 0,
        validations = 0,
        started_at = erlang:timestamp()
    }}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({cache_schema, Name, Schema}, _From, State) ->
    % Validate schema structure
    case validate_schema_structure(Schema) of
        ok ->
            persistent_term:put(?SCHEMA_KEY(Name), Schema),
            logger:debug("Cached schema: ~p", [Name]),
            {reply, ok, State};
        {error, Reason} = Error ->
            logger:warning("Invalid schema structure for ~p: ~p", [Name, Reason]),
            {reply, Error, State}
    end;
handle_call({invalidate, Name}, _From, State) ->
    try
        persistent_term:erase(?SCHEMA_KEY(Name)),
        logger:debug("Invalidated schema: ~p", [Name]),
        {reply, ok, State}
    catch
        error:badarg ->
            {reply, {error, not_found}, State}
    end;
handle_call(clear_all, _From, State) ->
    % Remove all erlmcp_schema_cache keys except stats
    AllTerms = persistent_term:get(),
    lists:foreach(
        fun
            ({{erlmcp_schema_cache, Name}, _}) when Name =/= stats ->
                persistent_term:erase(?SCHEMA_KEY(Name));
            (_) ->
                ok
        end,
        AllTerms
    ),
    logger:info("Cleared all cached schemas", []),
    {reply, ok, State#state{cache_hits = 0, cache_misses = 0, validations = 0}};
handle_call(get_stats, _From, State) ->
    Total = State#state.cache_hits + State#state.cache_misses,
    HitRate =
        case Total of
            0 -> 0.0;
            _ -> State#state.cache_hits / Total
        end,

    Now = erlang:timestamp(),
    UptimeSeconds = timer:now_diff(Now, State#state.started_at) div 1000000,

    Stats = #{
        cache_hits => State#state.cache_hits,
        cache_misses => State#state.cache_misses,
        validations => State#state.validations,
        hit_rate => HitRate,
        schema_count => length(list_schemas()),
        uptime_seconds => UptimeSeconds
    },
    {reply, Stats, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({record_hit, _Name}, State) ->
    {noreply, State#state{cache_hits = State#state.cache_hits + 1}};
handle_cast({record_miss, _Name}, State) ->
    {noreply, State#state{cache_misses = State#state.cache_misses + 1}};
handle_cast(increment_validations, State) ->
    {noreply, State#state{validations = State#state.validations + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) ->
    logger:info("erlmcp_schema_cache terminating after ~w validations", [State#state.validations]),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Validate schema structure (POC - basic checks)
%% In production, this would use jesse:compile/1 for full validation
-spec validate_schema_structure(schema_definition()) -> ok | {error, term()}.
validate_schema_structure(Schema) when is_map(Schema) ->
    % Basic JSON Schema validation
    RequiredFields = [<<"type">>],
    case lists:all(fun(Field) -> maps:is_key(Field, Schema) end, RequiredFields) of
        true ->
            ok;
        false ->
            {error, missing_required_fields}
    end;
validate_schema_structure(_) ->
    {error, schema_must_be_map}.

%% @doc Validate data against schema (POC - basic type checking)
%% In production, use jesse:validate_with_schema/2
-spec validate_data(schema_definition(), term(), validation_options()) -> ok | {error, term()}.
validate_data(Schema, Data, _Options) ->
    Type = maps:get(<<"type">>, Schema, <<"object">>),
    validate_type(Type, Data, Schema).

%% @doc Basic type validation (POC)
-spec validate_type(binary(), term(), schema_definition()) -> ok | {error, term()}.
validate_type(<<"object">>, Data, _Schema) when is_map(Data) ->
    ok;
validate_type(<<"object">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_object}};
validate_type(<<"array">>, Data, _Schema) when is_list(Data) ->
    ok;
validate_type(<<"array">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_array}};
validate_type(<<"string">>, Data, _Schema) when is_binary(Data) ->
    ok;
validate_type(<<"string">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_string}};
validate_type(<<"number">>, Data, _Schema) when is_number(Data) ->
    ok;
validate_type(<<"number">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_number}};
validate_type(<<"integer">>, Data, _Schema) when is_integer(Data) ->
    ok;
validate_type(<<"integer">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_integer}};
validate_type(<<"boolean">>, Data, _Schema) when is_boolean(Data) ->
    ok;
validate_type(<<"boolean">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_boolean}};
validate_type(<<"null">>, null, _Schema) ->
    ok;
validate_type(<<"null">>, _Data, _Schema) ->
    {error, {type_mismatch, expected_null}};
validate_type(Type, _Data, _Schema) ->
    {error, {unknown_type, Type}}.
