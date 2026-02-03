%% @doc Cluster Configuration Management for erlmcp v3
%%
%% This module manages cluster-wide configuration:
%%   - Configuration versioning
%%   - Distributed configuration propagation
%%   - Hot-reload capability
%%   - Configuration validation
%%   - Rollback support
%%   - Configuration persistence
-module(erlmcp_cluster_config).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([get_config/0, get_config/1]).
-export([set_config/2, set_config/3]).
-export([validate_config/1]).
-export([subscribe_changes/1, unsubscribe_changes/1]).
-export([get_version/0, rollback/1]).
-export([export_config/0, import_config/1]).
-export([get_history/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Internal state
-record(config_state,
        {
         current_version :: non_neg_integer(),
         config :: map(),
         config_history :: [config_version()],
         subscribers :: sets:set(pid()),
         validators :: [module()],
         persistence_module :: module() | undefined,
         auto_persist :: boolean(),
         validation_mode :: validation_mode()
        }).

-type config_version() :: #{
        version => non_neg_integer(),
        config => map(),
        applied_at => integer(),
        applied_by => node() | binary(),
        rollback_to => non_neg_integer() | undefined
       }.

-type validation_mode() ::
    strict    | %% All validations must pass
    lenient   | %% Warnings allowed
    disabled  | %% No validation
    custom.    %% Custom validation logic

%%%====================================================================
%%% API Functions
%%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Get all configuration
-spec get_config() -> {ok, map()}.
get_config() ->
    gen_server:call(?MODULE, get_config).

%% @doc Get specific configuration value
-spec get_config(binary()) -> {ok, term()} | {error, not_found}.
get_config(Key) ->
    gen_server:call(?MODULE, {get_config, Key}).

%% @doc Set configuration value
-spec set_config(binary(), term()) -> ok | {error, term()}.
set_config(Key, Value) ->
    set_config(Key, Value, #{}).

%% @doc Set configuration with options
-spec set_config(binary(), term(), map()) -> ok | {error, term()}.
set_config(Key, Value, Options) ->
    gen_server:call(?MODULE, {set_config, Key, Value, Options}).

%% @doc Validate configuration
-spec validate_config(map()) -> {ok, [binary()]} | {error, [binary()]}.
validate_config(Config) ->
    gen_server:call(?MODULE, {validate_config, Config}).

%% @doc Subscribe to configuration changes
-spec subscribe_changes(pid()) -> ok.
subscribe_changes(Subscriber) ->
    gen_server:cast(?MODULE, {subscribe, Subscriber}).

%% @doc Unsubscribe from configuration changes
-spec unsubscribe_changes(pid()) -> ok.
unsubscribe_changes(Subscriber) ->
    gen_server:cast(?MODULE, {unsubscribe, Subscriber}).

%% @doc Get current configuration version
-spec get_version() -> {ok, non_neg_integer()}.
get_version() ->
    gen_server:call(?MODULE, get_version).

%% @doc Rollback to previous version
-spec rollback(non_neg_integer()) -> ok | {error, term()}.
rollback(Version) ->
    gen_server:call(?MODULE, {rollback, Version}).

%% @doc Export current configuration
-spec export_config() -> {ok, binary()}.
export_config() ->
    gen_server:call(?MODULE, export_config).

%% @doc Import configuration
-spec import_config(binary()) -> ok | {error, term()}.
import_config(JSON) ->
    gen_server:call(?MODULE, {import_config, JSON}).

%% @doc Get configuration history
-spec get_history() -> {ok, [config_version()]}.
get_history() ->
    gen_server:call(?MODULE, get_history).

%%%====================================================================
%%% gen_server Callbacks
%%%====================================================================

-spec init(map()) -> {ok, #config_state{}}.
init(Options) ->
    process_flag(trap_exit, true),

    Validators = maps:get(validators, Options, []),
    PersistenceModule = maps:get(persistence_module, Options, undefined),
    AutoPersist = maps:get(auto_persist, Options, true),
    ValidationMode = maps:get(validation_mode, Options, strict),

    %% Load initial config
    InitialConfig = load_initial_config(Options),

    State = #config_state{
        current_version = 0,
        config = InitialConfig,
        config_history => [],
        subscribers = sets:new(),
        validators = Validators,
        persistence_module = PersistenceModule,
        auto_persist = AutoPersist,
        validation_mode = ValidationMode
    },

    logger:info("Cluster config manager started (version=~p)",
                [State#config_state.current_version]),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, #config_state{}) ->
    {reply, term(), #config_state{}}.
handle_call(get_config, _From, State) ->
    {reply, {ok, State#config_state.config}, State};

handle_call({get_config, Key}, _From, #config_state{config = Config} = State) ->
    case maps:get(Key, Config, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Value ->
            {reply, {ok, Value}, State}
    end;

handle_call({set_config, Key, Value, Options}, _From, State) ->
    case do_set_config(Key, Value, Options, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({validate_config, Config}, _From, #config_state{validators = Validators,
                                                           validation_mode = Mode} = State) ->

    case run_validators(Validators, Config, Mode) of
        {ok, Warnings} when length(Warnings) > 0 ->
            {reply, {ok, Warnings}, State};
        {ok, []} ->
            {reply, {ok, []}, State};
        {error, Errors} ->
            {reply, {error, Errors}, State}
    end;

handle_call(get_version, _From, State) ->
    {reply, {ok, State#config_state.current_version}, State};

handle_call({rollback, Version}, _From, State) ->
    case do_rollback(Version, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(export_config, _From, State) ->
    JSON = jsx:encode(State#config_state.config),
    {reply, {ok, JSON}, State};

handle_call({import_config, JSON}, _From, State) ->
    try jsx:decode(JSON, [return_maps]) of
        Config ->
            case do_import_config(Config, State) of
                {ok, NewState} ->
                    {reply, ok, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    catch
        _:_ ->
            {reply, {error, invalid_json}, State}
    end;

handle_call(get_history, _From, State) ->
    {reply, {ok, lists:reverse(State#config_state.config_history)}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #config_state{}) -> {noreply, #config_state{}}.
handle_cast({subscribe, Subscriber}, State) ->
    monitor(process, Subscriber),
    NewSubscribers = sets:add_element(Subscriber, State#config_state.subscribers),
    {noreply, State#config_state{subscribers = NewSubscribers}};

handle_cast({unsubscribe, Subscriber}, State) ->
    NewSubscribers = sets:del_element(Subscriber, State#config_state.subscribers),
    {noreply, State#config_state{subscribers = NewSubscribers}};

handle_cast({remote_config_update, Version, Config, FromNode}, State) ->
    case Version > State#config_state.current_version of
        true ->
            NewState = apply_remote_config(Version, Config, FromNode, State),
            {noreply, NewState};
        false ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #config_state{}) -> {noreply, #config_state{}}.
handle_info({'EXIT', Pid, _Reason}, State) ->
    NewSubscribers = sets:filter(fun(S) -> S =/= Pid end, State#config_state.subscribers),
    {noreply, State#config_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #config_state{}) -> ok.
terminate(_Reason, #config_state{auto_persist = true, persistence_module = Module}) ->
    case Module of
        undefined -> ok;
        _ -> persist_config(Module, 0, #{})
    end,
    logger:info("Cluster config manager terminating"),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #config_state{}, term()) -> {ok, #config_state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Load initial configuration
-spec load_initial_config(map()) -> map().
load_initial_config(Options) ->
    %% Load from application environment
    AppEnv = application:get_all_env(erlmcp_core),

    %% Merge with provided defaults
    Defaults = maps:get(defaults, Options, #{}),

    maps:merge(Defaults, maps:from_list(AppEnv)).

%% @doc Set configuration value
-spec do_set_config(binary(), term(), map(), #config_state{}) ->
    {ok, #config_state{}} | {error, term()}.
do_set_config(Key, Value, Options, #config_state{
        config = Config,
        current_version = CurrentVersion,
        validators = Validators,
        validation_mode = Mode
    } = State) ->

    %% Create new config with update
    NewConfig = maps:put(Key, Value, Config),

    %% Validate
    case run_validators(Validators, NewConfig, Mode) of
        {ok, _Warnings} ->
            %% Create new version
            NewVersion = CurrentVersion + 1,

            VersionRecord = #{
                version => NewVersion,
                config => NewConfig,
                applied_at => erlang:system_time(millisecond),
                applied_by => node(),
                rollback_to => undefined
            },

            NewHistory = [VersionRecord | State#config_state.config_history],

            NewState = State#config_state{
                current_version = NewVersion,
                config = NewConfig,
                config_history = NewHistory
            },

            %% Persist if enabled
            FinalState = case State#config_state.auto_persist of
                true ->
                    do_persist(NewState);
                false ->
                    NewState
            end,

            %% Propagate to cluster
            propagate_config_change(NewVersion, NewConfig, FinalState),

            %% Notify subscribers
            notify_config_change(FinalState, NewVersion, Key, Value),

            {ok, FinalState};
        {error, Errors} ->
            {error, {validation_failed, Errors}}
    end.

%% @doc Run validators
-spec run_validators([module()], map(), validation_mode()) ->
    {ok, [binary()]} | {error, [binary()]}.
run_validators([], _Config, _Mode) ->
    {ok, []};
run_validators(Validators, Config, Mode) ->
    lists:foldl(fun(Validator, {ok, Warnings}) ->
        case Validator:validate_config(Config) of
            ok ->
                {ok, Warnings};
            {ok, ValidatorWarnings} ->
                {ok, Warnings ++ ValidatorWarnings};
            {error, Errors} when Mode =:= lenient ->
                {ok, Warnings ++ Errors};
            {error, Errors} ->
                {error, Errors}
        end
    end, {ok, []}, Validators).

%% @doc Rollback to version
-spec do_rollback(non_neg_integer(), #config_state{}) ->
    {ok, #config_state{}} | {error, term()}.
do_rollback(Version, #config_state{config_history = History} = State) ->
    case lists:keyfind(Version, 1, lists:reverse(History)) of
        false ->
            {error, version_not_found};
        #{config := Config} ->
            %% Create new version with old config
            NewVersion = State#config_state.current_version + 1,

            VersionRecord = #{
                version => NewVersion,
                config => Config,
                applied_at => erlang:system_time(millisecond),
                applied_by => node(),
                rollback_to => Version
            },

            NewHistory = [VersionRecord | State#config_state.config_history],

            NewState = State#config_state{
                current_version = NewVersion,
                config = Config,
                config_history = NewHistory
            },

            {ok, NewState}
    end.

%% @doc Apply remote configuration
-spec apply_remote_config(non_neg_integer(), map(), node(), #config_state{}) ->
    #config_state{}.
apply_remote_config(Version, Config, FromNode, State) ->

    VersionRecord = #{
        version => Version,
        config => Config,
        applied_at => erlang:system_time(millisecond),
        applied_by => FromNode,
        rollback_to => undefined
    },

    NewHistory = [VersionRecord | State#config_state.config_history],

    NewState = State#config_state{
        current_version = Version,
        config = Config,
        config_history = NewHistory
    },

    notify_config_change(NewState, Version, remote, Config),

    NewState.

%% @doc Propagate config change to cluster
-spec propagate_config_change(non_neg_integer(), map(), #config_state{}) -> ok.
propagate_config_change(Version, Config, _State) ->
    {ok, Members} = erlmcp_cluster_membership:get_members(),

    lists:foreach(fun(Member) ->
        case Member of
            node() -> skip;
            _ ->
                gen_server:cast({?MODULE, Member},
                    {remote_config_update, Version, Config, node()})
        end
    end, Members),

    ok.

%% @doc Notify subscribers of config change
-spec notify_config_change(#config_state{}, non_neg_integer(), term(), term()) -> ok.
notify_config_change(#config_state{subscribers = Subscribers}, Version, Key, Value) ->
    Event = #{
        version => Version,
        key => Key,
        value => Value,
        timestamp => erlang:system_time(millisecond)
    },

    sets:foreach(fun(Subscriber) ->
        case is_process_alive(Subscriber) of
            true ->
                Subscriber ! {config_change, Event};
            false ->
                ok
        end
    end, Subscribers),

    ok.

%% @doc Persist configuration
-spec do_persist(#config_state{}) -> #config_state{}.
do_persist(#config_state{persistence_module = Module} = State) ->
    case Module of
        undefined ->
            State;
        _ ->
            persist_config(Module, State#config_state.current_version, State#config_state.config),
            State
    end.

%% @doc Persist configuration to backend
-spec persist_config(module(), non_neg_integer(), map()) -> ok.
persist_config(Module, Version, Config) ->
    try
        Module:save_config(Version, Config)
    catch
        _:_ ->
            logger:error("Failed to persist config version ~p to ~p", [Version, Module])
    end.

%% @doc Import configuration
-spec do_import_config(map(), #config_state{}) ->
    {ok, #config_state{}} | {error, term()}.
do_import_config(Config, #config_state{validators = Validators,
                                      validation_mode = Mode} = State) ->
    case run_validators(Validators, Config, Mode) of
        {ok, _Warnings} ->
            NewVersion = State#config_state.current_version + 1,

            VersionRecord = #{
                version => NewVersion,
                config => Config,
                applied_at => erlang:system_time(millisecond),
                applied_by => import,
                rollback_to => undefined
            },

            NewHistory = [VersionRecord | State#config_state.config_history],

            NewState = State#config_state{
                current_version = NewVersion,
                config = Config,
                config_history = NewHistory
            },

            {ok, NewState};
        {error, Errors} ->
            {error, {validation_failed, Errors}}
    end.
