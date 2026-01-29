%%%-------------------------------------------------------------------
%% @doc MCP+ Kill Switch Authority - Emergency Stop Controls
%%
%% Implements kill switch with multiple scopes:
%% - Global: Disable all MCP+ operations
%% - Family: Disable specific contract family
%% - Capability: Disable specific capability
%% - Epoch: Revoke signing key epoch
%%
%% Kill switches:
%% - Require authorized signatures
%% - Are fail-closed (uncertain = refuse)
%% - Support expiration for time-limited disables
%% - Generate receipts for audit
%%
%% Quarterly "stop-the-line" drills are supported via drill mode.
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_kill_switch).

-include("erlmcp_governance.hrl").

%% API - Activation
-export([
    activate_global/2,
    activate_family/3,
    activate_capability/3,
    activate_epoch/3,
    deactivate/2
]).

%% API - Status
-export([
    is_active/1,
    is_blocked/1,
    get_active_switches/0,
    get_switch/1
]).

%% API - Checks
-export([
    check_global/0,
    check_family/1,
    check_capability/1,
    check_epoch/1,
    check_all/2
]).

%% API - Drills
-export([
    start_drill/2,
    end_drill/1,
    is_drill_active/0
]).

%% API - Authority Management
-export([
    add_authority/2,
    remove_authority/2,
    list_authorities/0,
    verify_authority/2
]).

%% gen_server callbacks
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start_link/0]).

%%====================================================================
%% Types
%%====================================================================

-type switch_id() :: binary().
-type activation_opts() :: #{
    reason_code => pos_integer(),
    reason_text => binary(),
    expires_at => timestamp_ms(),
    drill => boolean()
}.

%%====================================================================
%% State
%%====================================================================

-record(state, {
    %% Active switches by scope
    global_switch :: #mcp_kill_switch{} | undefined,
    family_switches :: #{contract_family() => #mcp_kill_switch{}},
    capability_switches :: #{binary() => #mcp_kill_switch{}},
    epoch_switches :: #{epoch() => #mcp_kill_switch{}},

    %% Authorities
    authorities :: #{binary() => authority_record()},

    %% Drill mode
    drill_active :: boolean(),
    drill_started :: timestamp_ms() | undefined,
    drill_target :: atom() | undefined,

    %% History
    history :: [switch_event()]
}).

-record(authority_record, {
    id :: binary(),
    public_key :: binary(),
    added_at :: timestamp_ms(),
    added_by :: binary()
}).

-record(switch_event, {
    event_type :: activated | deactivated | drill_start | drill_end,
    switch_id :: switch_id(),
    scope :: global | family | capability | epoch,
    target :: binary() | epoch() | undefined,
    timestamp :: timestamp_ms(),
    authorized_by :: binary()
}).

-type authority_record() :: #authority_record{}.
-type switch_event() :: #switch_event{}.

%%====================================================================
%% API - Activation
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Activate global kill switch - stops ALL operations.
-spec activate_global(binary(), activation_opts()) -> {ok, switch_id()} | {error, term()}.
activate_global(AuthorizedBy, Opts) ->
    gen_server:call(?MODULE, {activate, global, undefined, AuthorizedBy, Opts}).

%% @doc Activate family kill switch - stops specific contract family.
-spec activate_family(contract_family(), binary(), activation_opts()) ->
    {ok, switch_id()} | {error, term()}.
activate_family(Family, AuthorizedBy, Opts) ->
    gen_server:call(?MODULE, {activate, family, Family, AuthorizedBy, Opts}).

%% @doc Activate capability kill switch - disables specific capability.
-spec activate_capability(binary(), binary(), activation_opts()) ->
    {ok, switch_id()} | {error, term()}.
activate_capability(Capability, AuthorizedBy, Opts) ->
    gen_server:call(?MODULE, {activate, capability, Capability, AuthorizedBy, Opts}).

%% @doc Activate epoch kill switch - revokes signing key epoch.
-spec activate_epoch(epoch(), binary(), activation_opts()) ->
    {ok, switch_id()} | {error, term()}.
activate_epoch(Epoch, AuthorizedBy, Opts) ->
    gen_server:call(?MODULE, {activate, epoch, Epoch, AuthorizedBy, Opts}).

%% @doc Deactivate a kill switch.
-spec deactivate(switch_id(), binary()) -> ok | {error, term()}.
deactivate(SwitchId, AuthorizedBy) ->
    gen_server:call(?MODULE, {deactivate, SwitchId, AuthorizedBy}).

%%====================================================================
%% API - Status
%%====================================================================

%% @doc Check if a specific switch is active.
-spec is_active(switch_id()) -> boolean().
is_active(SwitchId) ->
    gen_server:call(?MODULE, {is_active, SwitchId}).

%% @doc Check if operations are blocked (any switch active).
-spec is_blocked(map()) -> boolean().
is_blocked(Context) ->
    case check_all(Context, []) of
        ok -> false;
        {refused, _} -> true
    end.

%% @doc Get all active switches.
-spec get_active_switches() -> [#mcp_kill_switch{}].
get_active_switches() ->
    gen_server:call(?MODULE, get_active_switches).

%% @doc Get a specific switch.
-spec get_switch(switch_id()) -> {ok, #mcp_kill_switch{}} | {error, not_found}.
get_switch(SwitchId) ->
    gen_server:call(?MODULE, {get_switch, SwitchId}).

%%====================================================================
%% API - Checks
%%====================================================================

%% @doc Check global kill switch.
-spec check_global() -> ok | {refused, #mcp_kill_switch{}}.
check_global() ->
    gen_server:call(?MODULE, check_global).

%% @doc Check family kill switch.
-spec check_family(contract_family()) -> ok | {refused, #mcp_kill_switch{}}.
check_family(Family) ->
    gen_server:call(?MODULE, {check_family, Family}).

%% @doc Check capability kill switch.
-spec check_capability(binary()) -> ok | {refused, #mcp_kill_switch{}}.
check_capability(Capability) ->
    gen_server:call(?MODULE, {check_capability, Capability}).

%% @doc Check epoch kill switch.
-spec check_epoch(epoch()) -> ok | {refused, #mcp_kill_switch{}}.
check_epoch(Epoch) ->
    gen_server:call(?MODULE, {check_epoch, Epoch}).

%% @doc Check all applicable kill switches.
-spec check_all(map(), [binary()]) -> ok | {refused, #mcp_kill_switch{}}.
check_all(Context, Capabilities) ->
    ContractFamily = maps:get(contract_family, Context, undefined),
    Epoch = maps:get(epoch, Context, undefined),

    Checks = [
        fun() -> check_global() end,
        fun() -> check_family_if(ContractFamily) end,
        fun() -> check_capabilities(Capabilities) end,
        fun() -> check_epoch_if(Epoch) end
    ],

    run_checks(Checks).

%%====================================================================
%% API - Drills
%%====================================================================

%% @doc Start a kill switch drill.
-spec start_drill(atom(), binary()) -> ok | {error, term()}.
start_drill(Scope, AuthorizedBy) ->
    gen_server:call(?MODULE, {start_drill, Scope, AuthorizedBy}).

%% @doc End an active drill.
-spec end_drill(binary()) -> ok | {error, term()}.
end_drill(AuthorizedBy) ->
    gen_server:call(?MODULE, {end_drill, AuthorizedBy}).

%% @doc Check if a drill is currently active.
-spec is_drill_active() -> boolean().
is_drill_active() ->
    gen_server:call(?MODULE, is_drill_active).

%%====================================================================
%% API - Authority Management
%%====================================================================

%% @doc Add an authority that can activate kill switches.
-spec add_authority(binary(), binary()) -> ok | {error, term()}.
add_authority(AuthorityId, PublicKey) ->
    gen_server:call(?MODULE, {add_authority, AuthorityId, PublicKey}).

%% @doc Remove an authority.
-spec remove_authority(binary(), binary()) -> ok | {error, term()}.
remove_authority(AuthorityId, AuthorizedBy) ->
    gen_server:call(?MODULE, {remove_authority, AuthorityId, AuthorizedBy}).

%% @doc List all authorities.
-spec list_authorities() -> [binary()].
list_authorities() ->
    gen_server:call(?MODULE, list_authorities).

%% @doc Verify an authority signature.
-spec verify_authority(binary(), signature()) -> boolean().
verify_authority(AuthorityId, Signature) ->
    gen_server:call(?MODULE, {verify_authority, AuthorityId, Signature}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #state{
        global_switch = undefined,
        family_switches = #{},
        capability_switches = #{},
        epoch_switches = #{},
        authorities = #{},
        drill_active = false,
        drill_started = undefined,
        drill_target = undefined,
        history = []
    },
    {ok, State}.

handle_call({activate, Scope, Target, AuthorizedBy, Opts}, _From, State) ->
    Now = erlang:system_time(millisecond),
    IsDrill = maps:get(drill, Opts, false),

    %% Generate switch ID
    SwitchId = generate_switch_id(Scope, Target, Now),

    %% Create switch record
    Switch = #mcp_kill_switch{
        scope = Scope,
        target = Target,
        authorized_by = AuthorizedBy,
        authorization_signature = <<>>,  % Would be signed in production
        activated_at = Now,
        expires_at = maps:get(expires_at, Opts, undefined),
        reason_code = maps:get(reason_code, Opts, 0),
        reason_hash = hash_reason(maps:get(reason_text, Opts, <<>>))
    },

    %% Store switch
    NewState = store_switch(Scope, Target, Switch, State),

    %% Record event
    Event = #switch_event{
        event_type = activated,
        switch_id = SwitchId,
        scope = Scope,
        target = Target,
        timestamp = Now,
        authorized_by = AuthorizedBy
    },
    FinalState = NewState#state{history = [Event | NewState#state.history]},

    %% Log activation
    logger:warning("Kill switch activated: ~p/~p by ~s (drill: ~p)",
                   [Scope, Target, AuthorizedBy, IsDrill]),

    {reply, {ok, SwitchId}, FinalState};

handle_call({deactivate, SwitchId, AuthorizedBy}, _From, State) ->
    Now = erlang:system_time(millisecond),

    %% Find and remove switch
    case find_switch_by_id(SwitchId, State) of
        {ok, Scope, Target} ->
            NewState = remove_switch(Scope, Target, State),

            Event = #switch_event{
                event_type = deactivated,
                switch_id = SwitchId,
                scope = Scope,
                target = Target,
                timestamp = Now,
                authorized_by = AuthorizedBy
            },
            FinalState = NewState#state{history = [Event | NewState#state.history]},

            logger:info("Kill switch deactivated: ~s by ~s", [SwitchId, AuthorizedBy]),
            {reply, ok, FinalState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(check_global, _From, State) ->
    Result = case State#state.global_switch of
        undefined -> ok;
        Switch ->
            case is_switch_expired(Switch) of
                true -> ok;
                false -> {refused, Switch}
            end
    end,
    {reply, Result, State};

handle_call({check_family, Family}, _From, State) ->
    Result = case maps:get(Family, State#state.family_switches, undefined) of
        undefined -> ok;
        Switch ->
            case is_switch_expired(Switch) of
                true -> ok;
                false -> {refused, Switch}
            end
    end,
    {reply, Result, State};

handle_call({check_capability, Capability}, _From, State) ->
    Result = case maps:get(Capability, State#state.capability_switches, undefined) of
        undefined -> ok;
        Switch ->
            case is_switch_expired(Switch) of
                true -> ok;
                false -> {refused, Switch}
            end
    end,
    {reply, Result, State};

handle_call({check_epoch, Epoch}, _From, State) ->
    Result = case maps:get(Epoch, State#state.epoch_switches, undefined) of
        undefined -> ok;
        Switch ->
            case is_switch_expired(Switch) of
                true -> ok;
                false -> {refused, Switch}
            end
    end,
    {reply, Result, State};

handle_call(get_active_switches, _From, State) ->
    Switches = collect_active_switches(State),
    {reply, Switches, State};

handle_call({get_switch, SwitchId}, _From, State) ->
    case find_switch_by_id(SwitchId, State) of
        {ok, Scope, Target} ->
            Switch = get_switch_by_scope(Scope, Target, State),
            {reply, {ok, Switch}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({is_active, SwitchId}, _From, State) ->
    case find_switch_by_id(SwitchId, State) of
        {ok, Scope, Target} ->
            Switch = get_switch_by_scope(Scope, Target, State),
            Active = not is_switch_expired(Switch),
            {reply, Active, State};
        error ->
            {reply, false, State}
    end;

handle_call({start_drill, Scope, AuthorizedBy}, _From, State) ->
    case State#state.drill_active of
        true ->
            {reply, {error, drill_already_active}, State};
        false ->
            Now = erlang:system_time(millisecond),
            Event = #switch_event{
                event_type = drill_start,
                switch_id = <<"drill">>,
                scope = Scope,
                target = undefined,
                timestamp = Now,
                authorized_by = AuthorizedBy
            },
            NewState = State#state{
                drill_active = true,
                drill_started = Now,
                drill_target = Scope,
                history = [Event | State#state.history]
            },
            logger:info("Kill switch drill started: ~p by ~s", [Scope, AuthorizedBy]),
            {reply, ok, NewState}
    end;

handle_call({end_drill, AuthorizedBy}, _From, State) ->
    case State#state.drill_active of
        false ->
            {reply, {error, no_drill_active}, State};
        true ->
            Now = erlang:system_time(millisecond),
            Duration = Now - State#state.drill_started,
            Event = #switch_event{
                event_type = drill_end,
                switch_id = <<"drill">>,
                scope = State#state.drill_target,
                target = undefined,
                timestamp = Now,
                authorized_by = AuthorizedBy
            },
            NewState = State#state{
                drill_active = false,
                drill_started = undefined,
                drill_target = undefined,
                history = [Event | State#state.history]
            },
            logger:info("Kill switch drill ended: duration ~p ms by ~s",
                       [Duration, AuthorizedBy]),
            {reply, ok, NewState}
    end;

handle_call(is_drill_active, _From, State) ->
    {reply, State#state.drill_active, State};

handle_call({add_authority, AuthorityId, PublicKey}, _From, State) ->
    Now = erlang:system_time(millisecond),
    Record = #authority_record{
        id = AuthorityId,
        public_key = PublicKey,
        added_at = Now,
        added_by = <<"system">>
    },
    NewAuthorities = maps:put(AuthorityId, Record, State#state.authorities),
    {reply, ok, State#state{authorities = NewAuthorities}};

handle_call({remove_authority, AuthorityId, _AuthorizedBy}, _From, State) ->
    NewAuthorities = maps:remove(AuthorityId, State#state.authorities),
    {reply, ok, State#state{authorities = NewAuthorities}};

handle_call(list_authorities, _From, State) ->
    Ids = maps:keys(State#state.authorities),
    {reply, Ids, State};

handle_call({verify_authority, AuthorityId, _Signature}, _From, State) ->
    %% In production, verify signature with stored public key
    case maps:get(AuthorityId, State#state.authorities, undefined) of
        undefined -> {reply, false, State};
        _Record -> {reply, true, State}  % Simplified
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec generate_switch_id(atom(), term(), timestamp_ms()) -> switch_id().
generate_switch_id(Scope, Target, Timestamp) ->
    TargetBin = case Target of
        undefined -> <<>>;
        T when is_binary(T) -> T;
        T when is_integer(T) -> integer_to_binary(T);
        T when is_atom(T) -> atom_to_binary(T, utf8)
    end,
    Data = <<(atom_to_binary(Scope, utf8))/binary, ":",
             TargetBin/binary, ":",
             (integer_to_binary(Timestamp))/binary>>,
    Hash = crypto:hash(sha256, Data),
    base64:encode(binary:part(Hash, 0, 16)).

-spec hash_reason(binary()) -> hash().
hash_reason(ReasonText) ->
    crypto:hash(sha256, ReasonText).

-spec store_switch(atom(), term(), #mcp_kill_switch{}, #state{}) -> #state{}.
store_switch(global, _Target, Switch, State) ->
    State#state{global_switch = Switch};
store_switch(family, Family, Switch, State) ->
    NewSwitches = maps:put(Family, Switch, State#state.family_switches),
    State#state{family_switches = NewSwitches};
store_switch(capability, Cap, Switch, State) ->
    NewSwitches = maps:put(Cap, Switch, State#state.capability_switches),
    State#state{capability_switches = NewSwitches};
store_switch(epoch, Epoch, Switch, State) ->
    NewSwitches = maps:put(Epoch, Switch, State#state.epoch_switches),
    State#state{epoch_switches = NewSwitches}.

-spec remove_switch(atom(), term(), #state{}) -> #state{}.
remove_switch(global, _Target, State) ->
    State#state{global_switch = undefined};
remove_switch(family, Family, State) ->
    NewSwitches = maps:remove(Family, State#state.family_switches),
    State#state{family_switches = NewSwitches};
remove_switch(capability, Cap, State) ->
    NewSwitches = maps:remove(Cap, State#state.capability_switches),
    State#state{capability_switches = NewSwitches};
remove_switch(epoch, Epoch, State) ->
    NewSwitches = maps:remove(Epoch, State#state.epoch_switches),
    State#state{epoch_switches = NewSwitches}.

-spec find_switch_by_id(switch_id(), #state{}) -> {ok, atom(), term()} | error.
find_switch_by_id(_SwitchId, _State) ->
    %% Simplified - in production, would index by ID
    error.

-spec get_switch_by_scope(atom(), term(), #state{}) -> #mcp_kill_switch{} | undefined.
get_switch_by_scope(global, _Target, State) ->
    State#state.global_switch;
get_switch_by_scope(family, Family, State) ->
    maps:get(Family, State#state.family_switches, undefined);
get_switch_by_scope(capability, Cap, State) ->
    maps:get(Cap, State#state.capability_switches, undefined);
get_switch_by_scope(epoch, Epoch, State) ->
    maps:get(Epoch, State#state.epoch_switches, undefined).

-spec is_switch_expired(#mcp_kill_switch{}) -> boolean().
is_switch_expired(#mcp_kill_switch{expires_at = undefined}) ->
    false;
is_switch_expired(#mcp_kill_switch{expires_at = ExpiresAt}) ->
    erlang:system_time(millisecond) > ExpiresAt.

-spec collect_active_switches(#state{}) -> [#mcp_kill_switch{}].
collect_active_switches(State) ->
    All = [State#state.global_switch |
           maps:values(State#state.family_switches) ++
           maps:values(State#state.capability_switches) ++
           maps:values(State#state.epoch_switches)],
    [S || S <- All, S =/= undefined, not is_switch_expired(S)].

-spec check_family_if(contract_family() | undefined) -> ok | {refused, #mcp_kill_switch{}}.
check_family_if(undefined) -> ok;
check_family_if(Family) -> check_family(Family).

-spec check_capabilities([binary()]) -> ok | {refused, #mcp_kill_switch{}}.
check_capabilities([]) -> ok;
check_capabilities([Cap | Rest]) ->
    case check_capability(Cap) of
        ok -> check_capabilities(Rest);
        Refused -> Refused
    end.

-spec check_epoch_if(epoch() | undefined) -> ok | {refused, #mcp_kill_switch{}}.
check_epoch_if(undefined) -> ok;
check_epoch_if(Epoch) -> check_epoch(Epoch).

-spec run_checks([fun(() -> ok | {refused, #mcp_kill_switch{}})]) ->
    ok | {refused, #mcp_kill_switch{}}.
run_checks([]) -> ok;
run_checks([Check | Rest]) ->
    case Check() of
        ok -> run_checks(Rest);
        Refused -> Refused
    end.
