-module(erlmcp_schema_registry).

-behaviour(gen_server).

%% API exports
-export([start_link/0, register/3, get/2, get_latest/1, validate/3, list_versions/1,
         check_compatibility/3, delete/2, stop/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type schema_name() :: binary().
-type schema_type() :: tool | resource | prompt.
-type version() ::
    {Major :: non_neg_integer(), Minor :: non_neg_integer(), Patch :: non_neg_integer()}.
-type schema_definition() :: map().

-export_type([schema_name/0, schema_type/0, version/0, schema_definition/0]).

%% Schema record
-record(schema,
        {name :: schema_name(),
         version :: version(),
         type :: schema_type(),
         definition :: schema_definition(),
         created_at :: erlang:timestamp()}).
%% State record - ETS for fast lookups
-record(state,
        {schemas_table :: ets:tid(),      % {Name, Version} -> #schema{}
         versions_table :: ets:tid(),     % Name -> [Version]
         validator_pool :: pid()}).          % poolboy pool for async validation

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec register(schema_name(), version(), schema_definition()) -> ok | {error, term()}.
register(Name, Version, Definition) ->
    register(Name, Version, tool, Definition).

-spec register(schema_name(), version(), schema_type(), schema_definition()) ->
                  ok | {error, term()}.
register(Name, Version, Type, Definition)
    when is_binary(Name), is_tuple(Version), is_map(Definition) ->
    gen_server:call(?MODULE, {register, Name, Version, Type, Definition}).

-spec get(schema_name(), version()) -> {ok, #schema{}} | {error, not_found}.
get(Name, Version) when is_binary(Name), is_tuple(Version) ->
    gen_server:call(?MODULE, {get, Name, Version}).

-spec get_latest(schema_name()) -> {ok, #schema{}} | {error, not_found}.
get_latest(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {get_latest, Name}).

-spec validate(schema_name(), version(), term()) -> ok | {error, term()}.
validate(Name, Version, Data) when is_binary(Name), is_tuple(Version) ->
    gen_server:call(?MODULE, {validate, Name, Version, Data}, 10000).

-spec list_versions(schema_name()) -> {ok, [version()]} | {error, not_found}.
list_versions(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {list_versions, Name}).

-spec check_compatibility(schema_name(), version(), version()) ->
                             {ok, compatible} | {ok, breaking_change} | {error, term()}.
check_compatibility(Name, FromVersion, ToVersion) ->
    gen_server:call(?MODULE, {check_compatibility, Name, FromVersion, ToVersion}).

-spec delete(schema_name(), version()) -> ok | {error, not_found}.
delete(Name, Version) ->
    gen_server:call(?MODULE, {delete, Name, Version}).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    % Create ETS tables with read_concurrency for fast lookups
    SchemasTable =
        ets:new(erlmcp_schemas, [named_table, public, {read_concurrency, true}, {keypos, 1}]),

    VersionsTable =
        ets:new(erlmcp_schema_versions, [named_table, public, {read_concurrency, true}]),

    % Start poolboy pool for async validation
    PoolArgs =
        [{name, {local, schema_validator_pool}},
         {worker_module, erlmcp_schema_validator},
         {size, 5},
         {max_overflow, 10}],
    {ok, ValidatorPool} = poolboy:start_link(PoolArgs, []),

    logger:info("Schema registry started with ETS tables", []),

    {ok,
     #state{schemas_table = SchemasTable,
            versions_table = VersionsTable,
            validator_pool = ValidatorPool}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({register, Name, Version, Type, Definition}, _From, State) ->
    % Validate schema definition first
    case validate_schema_definition(Type, Definition) of
        ok ->
            Schema =
                #schema{name = Name,
                        version = Version,
                        type = Type,
                        definition = Definition,
                        created_at = erlang:timestamp()},

            % Store schema
            Key = {Name, Version},
            ets:insert(State#state.schemas_table, {Key, Schema}),

            % Update versions list
            Versions =
                case ets:lookup(State#state.versions_table, Name) of
                    [{Name, Vs}] ->
                        lists:usort([Version | Vs]);
                    [] ->
                        [Version]
                end,
            ets:insert(State#state.versions_table, {Name, Versions}),

            logger:info("Registered schema ~s v~s (~p)", [Name, format_version(Version), Type]),
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({get, Name, Version}, _From, State) ->
    Key = {Name, Version},
    case ets:lookup(State#state.schemas_table, Key) of
        [{Key, Schema}] ->
            {reply, {ok, Schema}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({get_latest, Name}, _From, State) ->
    case ets:lookup(State#state.versions_table, Name) of
        [{Name, Versions}] ->
            LatestVersion =
                lists:last(
                    lists:sort(Versions)),
            Key = {Name, LatestVersion},
            [{Key, Schema}] = ets:lookup(State#state.schemas_table, Key),
            {reply, {ok, Schema}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({validate, Name, Version, Data}, _From, State) ->
    Key = {Name, Version},
    case ets:lookup(State#state.schemas_table, Key) of
        [{Key, Schema}] ->
            % Use poolboy for validation
            Result =
                poolboy:transaction(State#state.validator_pool,
                                    fun(Worker) ->
                                       erlmcp_schema_validator:validate(Worker,
                                                                        Schema#schema.definition,
                                                                        Data)
                                    end,
                                    5000),
            {reply, Result, State};
        [] ->
            {reply, {error, schema_not_found}, State}
    end;
handle_call({list_versions, Name}, _From, State) ->
    case ets:lookup(State#state.versions_table, Name) of
        [{Name, Versions}] ->
            {reply, {ok, lists:sort(Versions)}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call({check_compatibility, Name, FromVersion, ToVersion}, _From, State) ->
    KeyFrom = {Name, FromVersion},
    KeyTo = {Name, ToVersion},

    case {ets:lookup(State#state.schemas_table, KeyFrom),
          ets:lookup(State#state.schemas_table, KeyTo)}
    of
        {[{KeyFrom, SchemaFrom}], [{KeyTo, SchemaTo}]} ->
            Result =
                check_backward_compatibility(SchemaFrom#schema.definition,
                                             SchemaTo#schema.definition),
            {reply, Result, State};
        _ ->
            {reply, {error, schema_not_found}, State}
    end;
handle_call({delete, Name, Version}, _From, State) ->
    Key = {Name, Version},
    case ets:lookup(State#state.schemas_table, Key) of
        [{Key, _}] ->
            ets:delete(State#state.schemas_table, Key),

            % Update versions list
            case ets:lookup(State#state.versions_table, Name) of
                [{Name, Versions}] ->
                    NewVersions = lists:delete(Version, Versions),
                    case NewVersions of
                        [] ->
                            ets:delete(State#state.versions_table, Name);
                        _ ->
                            ets:insert(State#state.versions_table, {Name, NewVersions})
                    end;
                [] ->
                    ok
            end,

            logger:info("Deleted schema ~s v~s", [Name, format_version(Version)]),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    logger:info("Schema registry terminating", []),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec validate_schema_definition(schema_type(), schema_definition()) -> ok | {error, term()}.
validate_schema_definition(_Type, Definition) when is_map(Definition) ->
    % Basic JSON Schema validation
    case maps:is_key(<<"type">>, Definition) of
        true ->
            ok;
        false ->
            {error, missing_type_field}
    end;
validate_schema_definition(_, _) ->
    {error, invalid_schema_definition}.

-spec check_backward_compatibility(schema_definition(), schema_definition()) ->
                                      {ok, compatible} | {ok, breaking_change}.
check_backward_compatibility(OldSchema, NewSchema) ->
    % Simple compatibility check - compare required fields
    OldRequired = maps:get(<<"required">>, OldSchema, []),
    NewRequired = maps:get(<<"required">>, NewSchema, []),

    % Breaking change if new required fields added
    case lists:all(fun(Field) -> lists:member(Field, OldRequired) end, NewRequired) of
        true ->
            {ok, compatible};
        false ->
            {ok, breaking_change}
    end.

-spec format_version(version()) -> binary().
format_version({Major, Minor, Patch}) ->
    iolist_to_binary(io_lib:format("~w.~w.~w", [Major, Minor, Patch])).
