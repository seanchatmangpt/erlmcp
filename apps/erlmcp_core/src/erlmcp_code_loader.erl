-module(erlmcp_code_loader).

-behaviour(gen_server).

%% API
-export([start_link/0,
         safe_load/1,
         hot_reload/2,
         prepare_reload/1,
         commit_reload/2,
         get_module_version/1,
         validate_module/1,
         get_module_md5/1,
         get_object_code/1,
         atomic_swap/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("erlmcp.hrl").

%% OTP 27-28 Code Loading Features
%% - Improved code:load_file/1 performance (OTP 27)
%% - Deterministic BEAM chunks (OTP 28)
%% - code:module_md5/1 for version tracking
%% - code:get_object_code/1 for version-aware reloading

-type module_name() :: module().
-type module_version() :: binary().
-type reload_state() :: prepared | committed | rolled_back.

-record(state,
        {module_table :: ets:tid(),           % Track loaded modules
         pending_reloads = #{} :: map(),      % In-flight reloads
         rollback_stack = #{} :: map(),       % Version history
         reload_strategies :: map()}).        % Strategy per module

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Load or reload module with validation (OTP 27 optimized)
%% Uses improved code:load_file/1 performance from OTP 27
-spec safe_load(module_name()) -> ok | {error, term()}.
safe_load(Module) ->
    gen_server:call(?MODULE, {safe_load, Module}, 5000).

%% @doc Hot reload with state preservation
%% Migrates state via code_change/2 if available
-spec hot_reload(module_name(), term()) -> ok | {error, term()}.
hot_reload(Module, State) ->
    gen_server:call(?MODULE, {hot_reload, Module, State}, 10000).

%% @doc Prepare module for reload (OTP 28 deterministic BEAM)
%% Returns version hash for later commit
-spec prepare_reload(module_name()) -> {ok, module_version()} | {error, term()}.
prepare_reload(Module) ->
    gen_server:call(?MODULE, {prepare_reload, Module}, 5000).

%% @doc Commit prepared reload (atomic operation)
-spec commit_reload(module_name(), module_version()) -> ok | {error, term()}.
commit_reload(Module, Version) ->
    gen_server:call(?MODULE, {commit_reload, Module, Version}, 5000).

%% @doc Get module version (OTP 28 MD5 from BEAM)
-spec get_module_version(module_name()) -> {ok, module_version()} | {error, not_found}.
get_module_version(Module) ->
    gen_server:call(?MODULE, {get_module_version, Module}).

%% @doc Validate loaded module
-spec validate_module(module_name()) -> ok | {error, term()}.
validate_module(Module) ->
    gen_server:call(?MODULE, {validate_module, Module}).

%% @doc Get module MD5 hash (OTP 28)
-spec get_module_md5(module_name()) -> {ok, binary()} | {error, term()}.
get_module_md5(Module) ->
    gen_server:call(?MODULE, {get_module_md5, Module}).

%% @doc Get object code for module
-spec get_object_code(module_name()) ->
                           {ok, module_name(), binary(), file:filename()} | {error, term()}.
get_object_code(Module) ->
    gen_server:call(?MODULE, {get_object_code, Module}).

%% @doc Atomic swap of module code (with rollback on error)
-spec atomic_swap(module_name(), binary(), file:filename()) -> ok | {error, term()}.
atomic_swap(Module, NewBinary, Filename) ->
    gen_server:call(?MODULE, {atomic_swap, Module, NewBinary, Filename}, 5000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% ETS table for module tracking (OTP 27 optimized)
    %% Read concurrency enabled for parallel lookups
    ModuleTable = ets:new(erlmcp_module_table,
                          [named_table,
                           public,
                           set,
                           {read_concurrency, true},
                           {write_concurrency, true}]),

    logger:info("Code loader started with OTP ~p optimizations", [erlang:system_info(otp_release)]),

    {ok, #state{module_table = ModuleTable,
                reload_strategies = #{}}}.

-spec handle_call(term(), {pid(), term()}, state()) ->
                          {reply, term(), state()}.
handle_call({safe_load, Module}, _From, State) ->
    Result = do_safe_load(Module, State),
    {reply, Result, State};

handle_call({hot_reload, Module, ModuleState}, _From, State) ->
    Result = do_hot_reload(Module, ModuleState, State),
    {reply, Result, State};

handle_call({prepare_reload, Module}, _From, State) ->
    Result = do_prepare_reload(Module, State),
    {reply, Result, State};

handle_call({commit_reload, Module, Version}, _From, State) ->
    Result = do_commit_reload(Module, Version, State),
    {reply, Result, State};

handle_call({get_module_version, Module}, _From, State) ->
    Result = get_module_version_internal(Module),
    {reply, Result, State};

handle_call({validate_module, Module}, _From, State) ->
    Result = do_validate_module(Module),
    {reply, Result, State};

handle_call({get_module_md5, Module}, _From, State) ->
    Result = do_get_module_md5(Module),
    {reply, Result, State};

handle_call({get_object_code, Module}, _From, State) ->
    Result = do_get_object_code(Module),
    {reply, Result, State};

handle_call({atomic_swap, Module, NewBinary, Filename}, _From, State) ->
    Result = do_atomic_swap(Module, NewBinary, Filename, State),
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{module_table = Table}) ->
    %% Clean up ETS table
    ets:delete(Table),
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions - Module Loading
%%====================================================================

%% @doc Safe load with validation
-spec do_safe_load(module_name(), state()) -> ok | {error, term()}.
do_safe_load(Module, _State) ->
    logger:info("Loading module: ~p", [Module]),

    %% OTP 27: Improved code:load_file/1 performance
    case code:load_file(Module) of
        {module, Module} ->
            logger:info("Module loaded successfully: ~p", [Module]),
            validate_module_internal(Module);
        {error, Reason} ->
            logger:error("Failed to load module ~p: ~p", [Module, Reason]),
            {error, Reason}
    end.

%% @doc Hot reload with state preservation
-spec do_hot_reload(module_name(), term(), state()) -> ok | {error, term()}.
do_hot_reload(Module, ModuleState, State) ->
    logger:info("Hot reloading module: ~p", [Module]),

    %% Get current object code for rollback
    case do_get_object_code(Module) of
        {ok, Module, Binary, Filename} ->
            %% Load new version
            case do_safe_load(Module, State) of
                ok ->
                    %% Migrate state if code_change/2 exists
                    case erlang:function_exported(Module, code_change, 2) of
                        true ->
                            try
                                Module:code_change(up, ModuleState),
                                logger:info("State migrated for module: ~p", [Module]),
                                ok
                            catch
                                _:Error ->
                                    logger:error("State migration failed for ~p: ~p, rolling back",
                                                 [Module, Error]),
                                    %% Rollback to old code
                                    rollback_to_old_code(Module, Binary, Filename),
                                    {error, {state_migration_failed, Error}}
                            end;
                        false ->
                            logger:info("No code_change/2, keeping state as-is for: ~p", [Module]),
                            ok
                    end;
                {error, Reason} ->
                    {error, {load_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {get_object_code_failed, Reason}}
    end.

%% @doc Prepare module for reload
-spec do_prepare_reload(module_name(), state()) -> {ok, module_version()} | {error, term()}.
do_prepare_reload(Module, _State) ->
    logger:info("Preparing reload for module: ~p", [Module]),

    case do_get_module_md5(Module) of
        {ok, MD5} ->
            %% Store in ETS for later commit
            ets:insert(erlmcp_module_table, {{Module, prepared}, MD5}),
            logger:info("Module ~p prepared with version: ~p", [Module, MD5]),
            {ok, MD5};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Commit prepared reload
-spec do_commit_reload(module_name(), module_version(), state()) -> ok | {error, term()}.
do_commit_reload(Module, Version, _State) ->
    logger:info("Committing reload for module: ~p version: ~p", [Module, Version]),

    case ets:lookup(erlmcp_module_table, {Module, prepared}) of
        [{{Module, prepared}, Version}] ->
            %% Versions match, safe to commit
            ets:delete(erlmcp_module_table, {Module, prepared}),
            ets:insert(erlmcp_module_table, {Module, Version}),
            logger:info("Module ~p committed with version: ~p", [Module, Version]),
            ok;
        [{{Module, prepared}, OtherVersion}] ->
            logger:error("Version mismatch for ~p: expected ~p, got ~p",
                         [Module, Version, OtherVersion]),
            {error, version_mismatch};
        [] ->
            {error, not_prepared}
    end.

%% @doc Get module version (OTP 28)
-spec get_module_version_internal(module_name()) -> {ok, module_version()} | {error, not_found}.
get_module_version_internal(Module) ->
    case ets:lookup(erlmcp_module_table, Module) of
        [{Module, Version}] ->
            {ok, Version};
        [] ->
            %% Try to get from BEAM file
            case do_get_module_md5(Module) of
                {ok, MD5} ->
                    %% Cache it
                    ets:insert(erlmcp_module_table, {Module, MD5}),
                    {ok, MD5};
                {error, _} ->
                    {error, not_found}
            end
    end.

%% @doc Validate loaded module
-spec do_validate_module(module_name()) -> ok | {error, term()}.
do_validate_module(Module) ->
    validate_module_internal(Module).

%% @doc Internal validation
-spec validate_module_internal(module_name()) -> ok | {error, term()}.
validate_module_internal(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            %% Check if module_info/0 exists
            case erlang:function_exported(Module, module_info, 0) of
                true ->
                    %% Validate BEAM file integrity (OTP 28)
                    case validate_beam_integrity(Module) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            {error, {beam_integrity_failed, Reason}}
                    end;
                false ->
                    {error, invalid_module}
            end;
        false ->
            {error, module_not_loaded}
    end.

%% @doc Validate BEAM file integrity (OTP 28 deterministic chunks)
-spec validate_beam_integrity(module_name()) -> ok | {error, term()}.
validate_beam_integrity(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, beam_file_not_found};
        BeamPath when is_list(BeamPath) ->
            case beam_lib:chunks(BeamPath, []) of
                {ok, {Module, _Chunks}} ->
                    %% OTP 28: Deterministic BEAM chunks validated
                    ok;
                {error, beam_lib, Reason} ->
                    {error, {beam_invalid, Reason}}
            end
    end.

%% @doc Get module MD5 (OTP 28)
-spec do_get_module_md5(module_name()) -> {ok, binary()} | {error, term()}.
do_get_module_md5(Module) ->
    case code:which(Module) of
        non_existing ->
            {error, beam_file_not_found};
        BeamPath when is_list(BeamPath) ->
            case beam_lib:md5(BeamPath) of
                {ok, {Module, MD5}} ->
                    {ok, MD5};
                {error, beam_lib, Reason} ->
                    {error, {md5_failed, Reason}}
            end
    end.

%% @doc Get object code for module
-spec do_get_object_code(module_name()) ->
                                 {ok, module_name(), binary(), file:filename()} | {error, term()}.
do_get_object_code(Module) ->
    case code:get_object_code(Module) of
        {Module, Binary, Filename} when is_binary(Binary), is_list(Filename) ->
            {ok, Module, Binary, Filename};
        error ->
            {error, object_code_not_available}
    end.

%% @doc Atomic swap with rollback on error
-spec do_atomic_swap(module_name(), binary(), file:filename(), state()) ->
                             ok | {error, term()}.
do_atomic_swap(Module, NewBinary, Filename, State) ->
    logger:info("Atomic swap for module: ~p", [Module]),

    %% Save old version for rollback
    case do_get_object_code(Module) of
        {ok, Module, OldBinary, Filename} ->
            %% Purge old code
            case code:soft_purge(Module) of
                true ->
                    %% Load new version
                    case code:load_binary(Module, Filename, NewBinary) of
                        {module, Module} ->
                            %% Success - update tracking
                            case do_get_module_md5(Module) of
                                {ok, NewMD5} ->
                                    ets:insert(erlmcp_module_table, {Module, NewMD5}),
                                    logger:info("Atomic swap successful for: ~p", [Module]),
                                    ok;
                                {error, _} ->
                                    %% New code loaded but can't get MD5 - non-fatal
                                    logger:warning("Atomic swap successful but MD5 unavailable for: ~p",
                                                  [Module]),
                                    ok
                            end;
                        {error, Reason} ->
                            %% Rollback to old code
                            logger:error("Load failed during atomic swap, rolling back: ~p",
                                         [Module]),
                            case code:load_binary(Module, Filename, OldBinary) of
                                {module, Module} ->
                                    logger:info("Rollback successful for: ~p", [Module]),
                                    {error, {load_failed, Reason}};
                                {error, RollbackReason} ->
                                    logger:error("Rollback failed for ~p: ~p", [Module, RollbackReason]),
                                    {error, {atomic_swap_failed, Reason, {rollback_failed, RollbackReason}}}
                            end
                    end;
                false ->
                    %% Processes still using old code, force purge
                    logger:warning("Forcing purge during atomic swap: ~p", [Module]),
                    true = code:purge(Module),
                    do_atomic_swap(Module, NewBinary, Filename, State)
            end;
        {error, Reason} ->
            {error, {get_object_code_failed, Reason}}
    end.

%% @doc Rollback to old code
-spec rollback_to_old_code(module_name(), binary(), file:filename()) -> ok.
rollback_to_old_code(Module, Binary, Filename) ->
    logger:warning("Rolling back module: ~p", [Module]),

    %% Purge new code
    code:soft_purge(Module),

    %% Load old code
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} ->
            logger:info("Rollback successful for: ~p", [Module]),
            ok;
        {error, Reason} ->
            logger:error("Rollback failed for ~p: ~p", [Module, Reason]),
            exit({rollback_failed, Module, Reason})
    end.
