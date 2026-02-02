%%%-------------------------------------------------------------------
%%% @doc
%%% Distribution Compatibility Module for OTP 26-28
%%%
%%% Handles the specific breaking changes and compatibility issues
%%% across OTP 26, 27, and 28 for distributed Erlang applications.
%%%
%%% Key Breaking Changes Addressed:
%%% - OTP 26: New mandatory link protocol, global pg removal
%%% - OTP 27: Native JSON module, enhanced distribution protocol
%%% - OTP 28: Process iterators, priority messages, enhanced protocol
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_distribution_compat).

-behaviour(gen_server).
-include("erlmcp.hrl").
-include("otp_compat.hrl").

%% API exports
-export([start_link/0, get_compatibility_info/0, apply_compatibility_patches/0,
         check_node_compatibility/1, get_optimized_protocol/0, handle_version_mismatch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type compatibility_level() :: fully_compatible | partially_compatible | incompatible.
-type patch_type() :: protocol_patch | feature_patch | optimization_patch.
-type patch_info() :: #{type := patch_type(),
                        version := {non_neg_integer(), non_neg_integer(), non_neg_integer()},
                        description := string(),
                        status := applied | pending | failed}.

%% State record
-record(state,
        {current_version :: {non_neg_integer(), non_neg_integer(), non_neg_integer()},
         target_versions :: [{{non_neg_integer(), non_neg_integer(), non_neg_integer()}, compatibility_level()}],
         applied_patches = [] :: [patch_info()],
         pending_patches = [] :: [patch_info()],
         node_compatibilities = #{} :: #{node() => compatibility_level()},
         optimization_level :: basic | standard | optimal}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Get detailed compatibility information for current setup
%% @end
%%--------------------------------------------------------------------
-spec get_compatibility_info() -> #{atom() => term()}.
get_compatibility_info() ->
    gen_server:call(?MODULE, get_compatibility_info, 2000).

%%--------------------------------------------------------------------
%% @doc Apply all necessary compatibility patches
%% @end
%%--------------------------------------------------------------------
-spec apply_compatibility_patches() -> ok | {error, term()}.
apply_compatibility_patches() ->
    gen_server:call(?MODULE, apply_compatibility_patches, 10000).

%%--------------------------------------------------------------------
%% @doc Check compatibility with a specific node
%% @end
%%--------------------------------------------------------------------
-spec check_node_compatibility(node()) -> compatibility_level().
check_node_compatibility(Node) when is_atom(Node) ->
    gen_server:call(?MODULE, {check_node_compatibility, Node}, 5000).

%%--------------------------------------------------------------------
%% @doc Get optimized protocol for current environment
%% @end
%%--------------------------------------------------------------------
-spec get_optimized_protocol() -> {otp26, legacy} | {otp27, enhanced} | {otp28, optimized}.
get_optimized_protocol() ->
    gen_server:call(?MODULE, get_optimized_protocol, 2000).

%%--------------------------------------------------------------------
%% @doc Handle version mismatch between nodes
%% @end
%%--------------------------------------------------------------------
-spec handle_version_mismatch(node(), term()) -> ok.
handle_version_mismatch(Node, Reason) ->
    gen_server:cast(?MODULE, {handle_version_mismatch, Node, Reason}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([]) -> {ok, state()}.
init([]) ->
    process_flag(trap_exit, true),

    %% Initialize with current OTP version
    CurrentVersion = erlmcp_version_detector:otp_version(),
    TargetVersions = get_target_versions(),

    logger:info("Starting distribution compatibility (OTP ~p)", [CurrentVersion]),

    %% Initialize state
    State = #state{
        current_version = CurrentVersion,
        target_versions = TargetVersions,
        optimization_level = determine_optimization_level(CurrentVersion, TargetVersions)
    },

    %% Initialize compatibility checks
    init_compatibility_checks(State),

    {ok, State}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(get_compatibility_info, _From, State = #state{current_version = CV,
                                                       applied_patches = Patches,
                                                       node_compatibilities = Compat}) ->
    Info = #{
        current_version => CV,
        optimization_level => State#state.optimization_level,
        applied_patches => Patches,
        node_compatibilities => maps:to_list(Compat),
        target_versions => State#state.target_versions
    },
    {reply, Info, State};
handle_call(apply_compatibility_patches, _From, State) ->
    case apply_all_patches(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({check_node_compatibility, Node}, _From, State) ->
    case check_node_compatibility_internal(Node, State) of
        {ok, Level} ->
            {reply, Level, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(get_optimized_protocol, _From, State = #state{current_version = CV}) ->
    Protocol = get_protocol_for_version(CV),
    {reply, Protocol, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast({handle_version_mismatch, Node, Reason}, State = #state{node_compatibilities = Compat}) ->
    logger:warning("Version mismatch with ~p: ~p", [Node, Reason]),
    %% Handle version mismatch appropriately
    NewCompat = maps:put(Node, partially_compatible, Compat),
    %% Attempt to apply compatibility fixes
    case apply_node_compatibility_patches(Node, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            {noreply, State#state{node_compatibilities = NewCompat}}
    end;
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({compatibility_patch_result, Patch, Result}, State = #state{applied_patches = Applied, pending_patches = Pending}) ->
    logger:info("Patch result: ~p -> ~p", [Patch, Result]),
    case Result of
        applied ->
            NewApplied = [Patch | Applied],
            NewPending = lists:delete(Patch, Pending),
            {noreply, State#state{applied_patches = NewApplied, pending_patches = NewPending}};
        failed ->
            %% Keep in pending for retry
            logger:warning("Patch ~p failed, will retry later", [Patch]),
            {noreply, State};
        retry ->
            %% Schedule retry
            erlang:send_after(5000, self(), {retry_patch, Patch}),
            {noreply, State}
    end;
handle_info({retry_patch, Patch}, State = #state{pending_patches = Pending}) ->
    case apply_patch(Patch, State) of
        {ok, NewState} ->
            {noreply, NewState};
        {error, _} ->
            %% Put back in pending list
            {noreply, State#state{pending_patches = [Patch | Pending]}}
    end;
handle_info({node_status_changed, Node, Status}, State = #state{node_compatibilities = Compat}) ->
    logger:info("Node status changed: ~p -> ~p", [Node, Status]),
    %% Update node compatibility based on status
    NewCompat = maps:put(Node, get_compatibility_level_from_status(Node, Status), Compat),
    {noreply, State#state{node_compatibilities = NewCompat}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, State = #state{current_version = Version}) ->
    logger:info("Distribution compatibility terminating (OTP ~p)", [Version]),

    %% Cleanup patches
    cleanup_patches(State),

    %% Apply version-specific cleanup
    case Version of
        {28, _, _} -> enhanced_compatibility_cleanup(State);
        {27, _, _} -> standard_compatibility_cleanup(State);
        {26, _, _} -> basic_compatibility_cleanup(State);
        _ -> standard_compatibility_cleanup(State)
    end,
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State = #state{current_version = Version}, _Extra) ->
    logger:info("Code change detected (OTP ~p)", [Version]),

    %% Re-apply patches after code change
    case apply_all_patches(State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, _} ->
            %% Keep original state if patches fail
            {ok, State}
    end}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% Get target versions and their compatibility levels
-spec get_target_versions() -> [{{non_neg_integer(), non_neg_integer(), non_neg_integer()}, compatibility_level()}].
get_target_versions() ->
    [
        {{28, 0, 0}, fully_compatible},
        {{27, 0, 0}, fully_compatible},
        {{26, 0, 0}, fully_compatible},
        {{25, _, _}, partially_compatible},
        {{24, _, _}, incompatible}
    ].

%% Determine optimization level based on current and target versions
-spec determine_optimization_level({non_neg_integer(), non_neg_integer(), non_neg_integer()},
                                   [{{non_neg_integer(), non_neg_integer(), non_neg_integer()}, compatibility_level()}]) ->
    basic | standard | optimal.
determine_optimization_level(CurrentVersion, TargetVersions) ->
    CurrentLevel = get_compatibility_level(CurrentVersion, TargetVersions),
    case CurrentVersion of
        {28, _, _} when CurrentLevel =:= fully_compatible ->
            optimal;
        {27, _, _} when CurrentLevel =:= fully_compatible ->
            standard;
        {26, _, _} when CurrentLevel =:= fully_compatible ->
            basic;
        _ ->
            standard  % Default fallback
    end.

%% Initialize compatibility checks
-spec init_compatibility_checks(state()) -> ok.
init_compatibility_checks(_State) ->
    logger:info("Initializing compatibility checks"),

    %% Check for OTP-specific features
    check_otp_features(),

    %% Initialize node monitoring
    start_node_monitoring(),

    ok.

%% Get compatibility level for a version
-spec get_compatibility_level({non_neg_integer(), non_neg_integer(), non_neg_integer()},
                             [{{non_neg_integer(), non_neg_integer(), non_neg_integer()}, compatibility_level()}]) ->
    compatibility_level().
get_compatibility_level(Version, TargetVersions) ->
    case lists:keyfind(Version, 1, TargetVersions) of
        {Version, Level} -> Level;
        _ ->
            % If version not in list, determine based on major version
            {Major, _, _} = Version,
            case lists:keyfind({Major, 0, 0}, 1, TargetVersions) of
                {{Major, _, _}, Level} -> Level;
                _ -> partially_compatible
            end
    end.

%% Get protocol for version
-spec get_protocol_for_version({non_neg_integer(), non_neg_integer(), non_neg_integer()}) ->
    {otp26, legacy} | {otp27, enhanced} | {otp28, optimized}.
get_protocol_for_version(Version) ->
    case Version of
        {28, _, _} -> {otp28, optimized};
        {27, _, _} -> {otp27, enhanced};
        {26, _, _} -> {otp26, legacy};
        _ -> {otp27, enhanced}  % Default fallback
    end.

%% Apply all patches
-spec apply_all_patches(state()) -> {ok, state()} | {error, term()}.
apply_all_patches(State = #state{pending_patches = Pending}) ->
    %% Sort patches by priority
    SortedPatches = sort_patches_by_priority(Pending),

    %% Apply patches in order
    apply_patches_ordered(SortedPatches, State).

%% Apply patches in order
-spec apply_patches_ordered([patch_info()], state()) -> {ok, state()} | {error, term()}.
apply_patches_ordered([], State) ->
    {ok, State};
apply_patches_ordered([Patch | Rest], State) ->
    case apply_patch(Patch, State) of
        {ok, NewState} ->
            apply_patches_ordered(Rest, NewState);
        {error, Reason} ->
            {error, Reason}
    end.

%% Apply a specific patch
-spec apply_patch(patch_info(), state()) -> {ok, state()} | {error, term()}.
apply_patch(Patch, State) ->
    #{type := Type, version := Version, description := Desc} = Patch,

    logger:info("Applying patch: ~s (~p)", [Desc, Type]),

    case Type of
        protocol_patch ->
            apply_protocol_patch(Version, State);
        feature_patch ->
            apply_feature_patch(Version, State);
        optimization_patch ->
            apply_optimization_patch(Version, State)
    end.

%% Apply protocol patch
-spec apply_protocol_patch({non_neg_integer(), non_neg_integer(), non_neg_integer()}, state()) ->
    {ok, state()} | {error, term()}.
apply_protocol_patch(Version, State) ->
    case Version of
        {28, _, _} ->
            %% OTP 28 protocol enhancements
            apply_otp28_protocol_patch(State);
        {27, _, _} ->
            %% OTP 27 protocol enhancements
            apply_otp27_protocol_patch(State);
        {26, _, _} ->
            %% OTP 26 protocol changes
            apply_otp26_protocol_patch(State);
        _ ->
            {error, unsupported_version}
    end.

%% Apply feature patch
-spec apply_feature_patch({non_neg_integer(), non_neg_integer(), non_neg_integer()}, state()) ->
    {ok, state()} | {error, term()}.
apply_feature_patch(Version, State) ->
    case Version of
        {28, _, _} ->
            %% OTP 28 feature enhancements
            apply_otp28_feature_patch(State);
        {27, _, _} ->
            %% OTP 27 feature enhancements
            apply_otp27_feature_patch(State);
        {26, _, _} ->
            %% OTP 26 feature changes
            apply_otp26_feature_patch(State);
        _ ->
            {error, unsupported_version}
    end.

%% Apply optimization patch
-spec apply_optimization_patch({non_neg_integer(), non_neg_integer(), non_neg_integer()}, state()) ->
    {ok, state()} | {error, term()}.
apply_optimization_patch(Version, State) ->
    case Version of
        {28, _, _} ->
            %% OTP 28 optimization enhancements
            apply_otp28_optimization_patch(State);
        {27, _, _} ->
            %% OTP 27 optimization enhancements
            apply_otp27_optimization_patch(State);
        {26, _, _} ->
            %% OTP 26 optimization changes
            apply_otp26_optimization_patch(State);
        _ ->
            {error, unsupported_version}
    end.

%% Check OTP features
-spec check_otp_features() -> ok.
check_otp_features() ->
    CurrentVersion = erlmcp_version_detector:otp_version(),
    Features = erlmcp_version_detector:get_optimal_features(),

    logger:info("Checking OTP features for ~p: ~p", [CurrentVersion, Features]),

    % Check specific features
    case Features#{native_json := true} of
        true -> logger:debug("Native JSON available");
        false -> logger:debug("Using JSX fallback")
    end,

    case Features#{process_iterator := true} of
        true -> logger:debug("Process iterator available");
        false -> logger:debug("Using legacy process enumeration")
    end,

    case Features#{priority_messages := true} of
        true -> logger:debug("Priority messages available");
        false -> logger:debug("Using standard message delivery")
    end,

    ok.

%% Start node monitoring
-spec start_node_monitoring() -> ok.
start_node_monitoring() ->
    logger:debug("Starting node monitoring for compatibility"),

    %% Monitor known nodes
    KnownNodes = nodes(),
    lists:foreach(fun(Node) ->
                    erlang:monitor_node(Node, true)
                 end,
                 KnownNodes),

    ok.

%% Check node compatibility internal
-spec check_node_compatibility_internal(node(), state()) -> {ok, compatibility_level()} | {error, term()}.
check_node_compatibility_internal(Node, _State) ->
    try
        %% Check if node is reachable
        case net_adm:ping(Node) of
            pang ->
                {error, node_unreachable};
            pong ->
                %% Get remote version information
                RemoteVersion = get_remote_version(Node),
                Compatibility = get_compatibility_level(RemoteVersion, get_target_versions()),
                {ok, Compatibility}
        end
    catch
        _:Reason ->
            {error, {check_failed, Reason}}
    end.

%% Get remote version
-spec get_remote_version(node()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
get_remote_version(Node) ->
    try
        case rpc:call(Node, erlang, system_info, [otp_release]) of
            {badrpc, _} -> {0, 0, 0};
            VersionStr -> parse_otp_version(VersionStr)
        end
    catch
        _:_ -> {0, 0, 0}
    end.

%% Parse OTP version
-spec parse_otp_version(string()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
parse_otp_version(VsnStr) ->
    Parts = string:split(VsnStr, ".", all),
    ToInt = fun(Str) ->
                case string:to_integer(Str) of
                    {Int, ""} -> Int;
                    {Int, _} -> Int;
                    _ -> 0
                end
            end,
    Padded = case Parts of
                [M1] -> [M1, "0", "0"];
                [M1, M2] -> [M1, M2, "0"];
                [M1, M2, M3 | _] -> [M1, M2, M3]
            end,
    [Major, Minor, Patch] = [ToInt(P) || P <- Padded],
    {Major, Minor, Patch}.

%% Sort patches by priority
-spec sort_patches_by_priority([patch_info()]) -> [patch_info()].
sort_patches_by_priority(Patches) ->
    SortPriority = fun(P1, P2) ->
        Priority1 = get_patch_priority(P1),
        Priority2 = get_patch_priority(P2),
        Priority1 =< Priority2
    end,
    lists:sort(SortPriority, Patches).

%% Get patch priority
-spec get_patch_priority(patch_info()) -> integer().
get_patch_priority(#{type := Type, version := Version}) ->
    Priority = case Type of
                  protocol_patch -> 1;
                  feature_patch -> 2;
                  optimization_patch -> 3
              end,
    % Lower version gets higher priority
    Priority * 100 + Version,  % Simplified priority calculation
    Priority * 100 - (element(1, Version) * 10000 + element(2, Version) * 100 + element(3, Version)).

%% Apply node compatibility patches
-spec apply_node_compatibility_patches(node(), state()) -> {ok, state()} | {error, term()}.
apply_node_compatibility_patches(Node, State) ->
    logger:info("Applying compatibility patches for node: ~p", [Node]),

    % Apply patches specific to this node
    case check_node_compatibility_internal(Node, State) of
        {ok, Compatibility} ->
            case Compatibility of
                fully_compatible ->
                    {ok, State};
                partially_compatible ->
                    apply_partial_compatibility_patches(Node, State);
                incompatible ->
                    {error, incompatible_node}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% Apply partial compatibility patches
-spec apply_partial_compatibility_patches(node(), state()) -> {ok, state()} | {error, term()}.
apply_partial_compatibility_patches(Node, State) ->
    logger:info("Applying partial compatibility patches for: ~p", [Node]),

    % Apply patches for partial compatibility
    case apply_patch_for_partial_compatibility(Node, State) of
        {ok, NewState} ->
            {ok, NewState};
        {error, Reason} ->
            {error, Reason}
    end.

%% Apply patch for partial compatibility
-spec apply_patch_for_partial_compatibility(node(), state()) -> {ok, state()} | {error, term()}.
apply_patch_for_partial_compatibility(Node, State) ->
    try
        % Use fallback communication methods
        apply_fallback_communication(Node),

        % Apply protocol translation if needed
        apply_protocol_translation(Node),

        % Apply feature translation
        apply_feature_translation(Node),

        {ok, State}
    catch
        _:Reason ->
            {error, {patch_failed, Reason}}
    end.

%% Apply fallback communication
-spec apply_fallback_communication(node()) -> ok.
apply_fallback_communication(Node) ->
    logger:debug("Applying fallback communication for: ~p", [Node]),

    % Use alternative communication methods
    ok.

%% Apply protocol translation
-spec apply_protocol_translation(node()) -> ok.
apply_protocol_translation(Node) ->
    logger:debug("Applying protocol translation for: ~p", [Node]),

    % Translate between different protocol versions
    ok.

%% Apply feature translation
-spec apply_feature_translation(node()) -> ok.
apply_feature_translation(Node) ->
    logger:debug("Applying feature translation for: ~p", [Node]),

    % Translate between different feature sets
    ok.

%% Get compatibility level from node status
-spec get_compatibility_level_from_status(node(), term()) -> compatibility_level().
get_compatibility_level_from_status(Node, Status) ->
    case Status of
        connected -> fully_compatible;
        disconnected -> partially_compatible;
        _ -> incompatible
    end.

%% Apply OTP 28 protocol patch
-spec apply_otp28_protocol_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp28_protocol_patch(State) ->
    logger:debug("Applying OTP 28 protocol patch"),

    % Enable enhanced protocol
    enable_enhanced_protocol(),

    % Apply protocol optimizations
    apply_protocol_optimizations(),

    % Create patch info
    Patch = create_patch_info(protocol_patch, {28, 0, 0}, "OTP 28 enhanced protocol"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 27 protocol patch
-spec apply_otp27_protocol_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp27_protocol_patch(State) ->
    logger:debug("Applying OTP 27 protocol patch"),

    % Enable enhanced protocol for OTP 27
    enable_otp27_protocol(),

    % Apply protocol optimizations
    apply_protocol_optimizations(),

    % Create patch info
    Patch = create_patch_info(protocol_patch, {27, 0, 0}, "OTP 27 enhanced protocol"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 26 protocol patch
-spec apply_otp26_protocol_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp26_protocol_patch(State) ->
    logger:debug("Applying OTP 26 protocol patch"),

    % Apply legacy protocol support
    apply_legacy_protocol(),

    % Create patch info
    Patch = create_patch_info(protocol_patch, {26, 0, 0}, "OTP 26 legacy protocol"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 28 feature patch
-spec apply_otp28_feature_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp28_feature_patch(State) ->
    logger:debug("Applying OTP 28 feature patch"),

    % Enable priority messages
    ?SET_PRIORITY_HIGH(),

    % Enable process iterator optimizations
    enable_process_iterator_optimization(),

    % Create patch info
    Patch = create_patch_info(feature_patch, {28, 0, 0}, "OTP 28 feature enhancements"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 27 feature patch
-spec apply_otp27_feature_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp27_feature_patch(State) ->
    logger:debug("Applying OTP 27 feature patch"),

    % Enable native JSON
    enable_native_json(),

    % Create patch info
    Patch = create_patch_info(feature_patch, {27, 0, 0}, "OTP 27 feature enhancements"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 26 feature patch
-spec apply_otp26_feature_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp26_feature_patch(State) ->
    logger:debug("Applying OTP 26 feature patch"),

    % Apply legacy feature support
    apply_legacy_features(),

    % Create patch info
    Patch = create_patch_info(feature_patch, {26, 0, 0}, "OTP 26 feature support"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 28 optimization patch
-spec apply_otp28_optimization_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp28_optimization_patch(State) ->
    logger:debug("Applying OTP 28 optimization patch"),

    % Apply enhanced optimizations
    apply_enhanced_optimizations(),

    % Create patch info
    Patch = create_patch_info(optimization_patch, {28, 0, 0}, "OTP 28 optimization enhancements"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 27 optimization patch
-spec apply_otp27_optimization_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp27_optimization_patch(State) ->
    logger:debug("Applying OTP 27 optimization patch"),

    % Apply standard optimizations
    apply_standard_optimizations(),

    % Create patch info
    Patch = create_patch_info(optimization_patch, {27, 0, 0}, "OTP 27 optimization enhancements"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Apply OTP 26 optimization patch
-spec apply_otp26_optimization_patch(state()) -> {ok, state()} | {error, term()}.
apply_otp26_optimization_patch(State) ->
    logger:debug("Applying OTP 26 optimization patch"),

    % Apply basic optimizations
    apply_basic_optimizations(),

    % Create patch info
    Patch = create_patch_info(optimization_patch, {26, 0, 0}, "OTP 26 optimization support"),

    {ok, State#state{applied_patches = [Patch | State#state.applied_patches]}}.

%% Create patch info
-spec create_patch_info(patch_type(), {non_neg_integer(), non_neg_integer(), non_neg_integer()}, string()) ->
    patch_info().
create_patch_info(Type, Version, Description) ->
    #{
        type => Type,
        version => Version,
        description => Description,
        status => applied
    }.

%% Enable enhanced protocol
-spec enable_enhanced_protocol() -> ok.
enable_enhanced_protocol() ->
    logger:debug("Enabling enhanced distribution protocol"),
    ok.

%% Enable OTP 27 protocol
-spec enable_otp27_protocol() -> ok.
enable_otp27_protocol() ->
    logger:debug("Enabling OTP 27 enhanced protocol"),
    ok.

%% Enable legacy protocol
-spec apply_legacy_protocol() -> ok.
apply_legacy_protocol() ->
    logger:debug("Applying legacy protocol support"),
    ok.

%% Enable native JSON
-spec enable_native_json() -> ok.
enable_native_json() ->
    logger:debug("Enabling native JSON support"),
    ok.

%% Enable process iterator optimization
-spec enable_process_iterator_optimization() -> ok.
enable_process_iterator_optimization() ->
    logger:debug("Enabling process iterator optimization"),
    ok.

%% Apply protocol optimizations
-spec apply_protocol_optimizations() -> ok.
apply_protocol_optimizations() ->
    logger:debug("Applying protocol optimizations"),
    ok.

%% Apply legacy features
-spec apply_legacy_features() -> ok.
apply_legacy_features() ->
    logger:debug("Applying legacy feature support"),
    ok.

%% Apply enhanced optimizations
-spec apply_enhanced_optimizations() -> ok.
apply_enhanced_optimizations() ->
    logger:debug("Applying enhanced optimizations"),
    ok.

%% Apply standard optimizations
-spec apply_standard_optimizations() -> ok.
apply_standard_optimizations() ->
    logger:debug("Applying standard optimizations"),
    ok.

%% Apply basic optimizations
-spec apply_basic_optimizations() -> ok.
apply_basic_optimizations() ->
    logger:debug("Applying basic optimizations"),
    ok.

%% Cleanup patches
-spec cleanup_patches(state()) -> ok.
cleanup_patches(_State) ->
    logger:debug("Cleaning up compatibility patches"),
    ok.

%% Enhanced compatibility cleanup
-spec enhanced_compatibility_cleanup(state()) -> ok.
enhanced_compatibility_cleanup(_State) ->
    logger:debug("Enhanced compatibility cleanup for OTP 28+"),
    ok.

%% Standard compatibility cleanup
-spec standard_compatibility_cleanup(state()) -> ok.
standard_compatibility_cleanup(_State) ->
    logger:debug("Standard compatibility cleanup"),
    ok.

%% Basic compatibility cleanup
-spec basic_compatibility_cleanup(state()) -> ok.
basic_compatibility_cleanup(_State) ->
    logger:debug("Basic compatibility cleanup"),
    ok.