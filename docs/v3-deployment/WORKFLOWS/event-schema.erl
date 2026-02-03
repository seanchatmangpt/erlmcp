%%%-------------------------------------------------------------------
%%% @doc
%%% Event Schema Definitions for erlmcp v3 Deployment Workflows
%%%
%%% This module defines the event types and schemas used throughout
%%% the deployment pipeline. All events follow a common structure
%%% for consistency and observability.
%%%
%%% ## Event Categories
%%%
%%% 1. **Trigger Events** - Initiate deployment workflows
%%% 2. **State Events** - Track deployment state changes
%%% 3. **Approval Events** - Record approval decisions
%%% 4. **Compliance Events** - Compliance check results
%%% 5. **Notification Events** - Sent notifications
%%% 6. **Audit Events** - Immutable audit records
%%%
%%% ## Event Flow
%%%
%%% ```
%%% Trigger -> Validation -> Approval -> Deployment -> Verification -> Audit
%%%    |          |            |          |            |           |
%%%    v          v            v          v            v           v
%%%  Record    Record       Record     Record       Record     Record
%%%    |          |            |          |            |           |
%%%    └──────────┴────────────┴──────────┴────────────┴───────────┘
%%%                           |
%%%                           v
%%%                    Notification Event
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_deployment_events).

%%%-------------------------------------------------------------------
%%% Public API - Event Creation
%%%-------------------------------------------------------------------
-export([
    new_trigger_event/5,
    new_state_event/4,
    new_approval_event/6,
    new_compliance_event/6,
    new_notification_event/5,
    new_audit_event/7
]).

%%%-------------------------------------------------------------------
%%% Public API - Event Validation
%%%-------------------------------------------------------------------
-export([
    validate_event/1,
    validate_trigger_event/1,
    validate_state_event/1,
    is_valid_event_type/1
]).

%%%-------------------------------------------------------------------
%%% Public API - Event Serialization
%%%-------------------------------------------------------------------
-export([
    serialize/1,
    deserialize/1,
    to_json/1,
    from_json/1
]).

%%%-------------------------------------------------------------------
%%% Public API - Event Queries
%%%-------------------------------------------------------------------
-export([
    get_event_id/1,
    get_event_type/1,
    get_event_timestamp/1,
    get_event_actor/1,
    filter_by_type/2,
    filter_by_actor/2,
    filter_by_timerange/3
]).

%%%-------------------------------------------------------------------
%%% Type Definitions
%%%-------------------------------------------------------------------

-type event_id() :: binary().
-type event_type() :: trigger | state | approval | compliance | notification | audit.
-type event_category() ::
    git_push | git_pr | git_tag | manual_dispatch | schedule |
    pending | validation | building | testing | approval | deploying |
    verifying | monitoring | completed | failed | rolling_back | rejected |
    approved | rejected | expired |
    compile | test | security | performance | documentation |
    slack | teams | pagerduty | email |
    deployment | rollback | approval_req | configuration.
-type trigger_source() :: git | api | webhook | cli.
-type approval_state() :: pending | approved | rejected | expired.
-type compliance_status() :: pass | fail | warn | skip.
-type notification_severity() :: critical | high | medium | low | info.
-type environment() :: dev | qa | staging | production | dr.

-record(event_metadata, {
    id              :: event_id(),
    type            :: event_type(),
    category        :: event_category() | undefined,
    timestamp       :: erlang:timestamp(),
    actor           :: binary() | undefined,
    source_ip       :: binary() | undefined,
    correlation_id  :: binary() | undefined,
    parent_id       :: event_id() | undefined
}).

-record(trigger_event, {
    metadata        :: #event_metadata{},
    source          :: trigger_source(),
    git_ref         :: binary() | undefined,
    commit_sha      :: binary() | undefined,
    version         :: binary() | undefined,
    target_env      :: environment() | undefined,
    workflow_id     :: binary() | undefined,
    manual_inputs   :: map() | undefined
}).

-record(state_event, {
    metadata        :: #event_metadata{},
    deployment_id   :: binary(),
    previous_state  :: atom() | undefined,
    current_state   :: atom(),
    reason          :: binary() | undefined,
    details         :: map()
}).

-record(approval_event, {
    metadata        :: #event_metadata{},
    deployment_id   :: binary(),
    gate_name       :: binary(),
    state           :: approval_state(),
    approver        :: binary(),
    decision_reason :: binary() | undefined,
    valid_until     :: erlang:timestamp() | undefined
}).

-record(compliance_event, {
    metadata        :: #event_metadata{},
    deployment_id   :: binary(),
    gate_name       :: binary(),
    status          :: compliance_status(),
    check_result    :: map(),
    threshold       :: map(),
    actual          :: map()
}).

-record(notification_event, {
    metadata        :: #event_metadata{},
    deployment_id   :: binary(),
    channel         :: atom(),
    severity        :: notification_severity(),
    template        :: binary(),
    payload         :: map(),
    delivered       :: boolean(),
    delivery_time   :: erlang:timestamp() | undefined
}).

-record(audit_event, {
    metadata        :: #event_metadata{},
    deployment_id   :: binary(),
    environment     :: environment(),
    version         :: binary(),
    action          :: binary(),
    status          :: atom(),
    details         :: map(),
    tamper_evident  :: boolean()
}).

-type deployment_event() ::
    #trigger_event{} |
    #state_event{} |
    #approval_event{} |
    #compliance_event{} |
    #notification_event{} |
    #audit_event{}.

-export_type([
    event_id/0,
    event_type/0,
    event_category/0,
    trigger_source/0,
    approval_state/0,
    compliance_status/0,
    notification_severity/0,
    environment/0,
    deployment_event/0
]).

%%%-------------------------------------------------------------------
%%% Public API - Event Creation
%%%-------------------------------------------------------------------

%% @doc Create a new trigger event
%% @param Type The trigger type (git_push, git_pr, git_tag, etc.)
%% @param Source The source of the trigger (git, api, webhook, cli)
%% @param Actor The user/service initiating the trigger
%% @param GitRef The git reference (branch, tag, commit)
%% @param TargetEnv The target environment (optional)
-spec new_trigger_event(atom(), trigger_source(), binary(), binary(), environment() | undefined) ->
    #trigger_event{}.
new_trigger_event(Type, Source, Actor, GitRef, TargetEnv) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),
    CorrelationId = generate_correlation_id(),

    Metadata = #event_metadata{
        id = EventId,
        type = trigger,
        category = Type,
        timestamp = Timestamp,
        actor = Actor,
        correlation_id = CorrelationId
    },

    #trigger_event{
        metadata = Metadata,
        source = Source,
        git_ref = GitRef,
        target_env = TargetEnv,
        details = #{}
    }.

%% @doc Create a new state event
%% @param DeploymentId The deployment ID
%% @param PreviousState The previous state
%% @param CurrentState The new state
%% @param Actor The actor causing the state change
-spec new_state_event(binary(), atom(), atom(), binary()) -> #state_event{}.
new_state_event(DeploymentId, PreviousState, CurrentState, Actor) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),

    Metadata = #event_metadata{
        id = EventId,
        type = state,
        category = CurrentState,
        timestamp = Timestamp,
        actor = Actor
    },

    #state_event{
        metadata = Metadata,
        deployment_id = DeploymentId,
        previous_state = PreviousState,
        current_state = CurrentState,
        details = #{}
    }.

%% @doc Create a new approval event
%% @param DeploymentId The deployment ID
%% @param GateName The gate name requiring approval
%% @param State The approval state
%% @param Approver The approver identity
%% @param Reason The reason for decision
%% @param ValidUntil When the approval expires
-spec new_approval_event(binary(), binary(), approval_state(), binary(), binary() | undefined, erlang:timestamp() | undefined) ->
    #approval_event{}.
new_approval_event(DeploymentId, GateName, State, Approver, Reason, ValidUntil) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),

    Metadata = #event_metadata{
        id = EventId,
        type = approval,
        category = State,
        timestamp = Timestamp,
        actor = Approver
    },

    #approval_event{
        metadata = Metadata,
        deployment_id = DeploymentId,
        gate_name = GateName,
        state = State,
        approver = Approver,
        decision_reason = Reason,
        valid_until = ValidUntil
    }.

%% @doc Create a new compliance event
%% @param DeploymentId The deployment ID
%% @param GateName The compliance gate name
%% @param Status The compliance status
%% @param CheckResult The detailed check results
%% @param Threshold The required thresholds
%% @param Actual The actual measured values
-spec new_compliance_event(binary(), binary(), compliance_status(), map(), map(), map()) ->
    #compliance_event{}.
new_compliance_event(DeploymentId, GateName, Status, CheckResult, Threshold, Actual) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),

    Metadata = #event_metadata{
        id = EventId,
        type = compliance,
        category = GateName,
        timestamp = Timestamp,
        actor = <<"system">>
    },

    #compliance_event{
        metadata = Metadata,
        deployment_id = DeploymentId,
        gate_name = GateName,
        status = Status,
        check_result = CheckResult,
        threshold = Threshold,
        actual = Actual
    }.

%% @doc Create a new notification event
%% @param DeploymentId The deployment ID
%% @param Channel The notification channel
%% @param Severity The notification severity
%% @param Template The notification template
%% @param Payload The notification payload
-spec new_notification_event(binary(), atom(), notification_severity(), binary(), map()) ->
    #notification_event{}.
new_notification_event(DeploymentId, Channel, Severity, Template, Payload) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),

    Metadata = #event_metadata{
        id = EventId,
        type = notification,
        category = Channel,
        timestamp = Timestamp,
        actor = <<"system">>
    },

    #notification_event{
        metadata = Metadata,
        deployment_id = DeploymentId,
        channel = Channel,
        severity = Severity,
        template = Template,
        payload = Payload,
        delivered = false
    }.

%% @doc Create a new audit event
%% @param DeploymentId The deployment ID
%% @param Environment The target environment
%% @param Version The version being deployed
%% @param Action The action performed
%% @param Status The action status
%% @param Actor The actor performing the action
%% @param Details Additional details
-spec new_audit_event(binary(), environment(), binary(), binary(), atom(), binary(), map()) ->
    #audit_event{}.
new_audit_event(DeploymentId, Environment, Version, Action, Status, Actor, Details) ->
    EventId = generate_event_id(),
    Timestamp = erlang:system_time(microsecond),

    Metadata = #event_metadata{
        id = EventId,
        type = audit,
        category = Action,
        timestamp = Timestamp,
        actor = Actor
    },

    #audit_event{
        metadata = Metadata,
        deployment_id = DeploymentId,
        environment = Environment,
        version = Version,
        action = Action,
        status = Status,
        details = Details,
        tamper_evident = true
    }.

%%%-------------------------------------------------------------------
%%% Public API - Event Validation
%%%-------------------------------------------------------------------

%% @doc Validate any event type
-spec validate_event(deployment_event()) -> ok | {error, term()}.
validate_event(#trigger_event{}) -> validate_trigger_event(_);
validate_event(#state_event{}) -> validate_state_event(_);
validate_event(#approval_event{}) ->
    {ok, validated} = do_validate_approval(_),
    case validated of
        true -> ok;
        false -> {error, invalid_approval_event}
    end;
validate_event(#compliance_event{}) -> ok;
validate_event(#notification_event{}) -> ok;
validate_event(#audit_event{}) -> ok;
validate_event(_) -> {error, unknown_event_type}.

%% @doc Validate trigger event
-spec validate_trigger_event(#trigger_event{}) -> ok | {error, term()}.
validate_trigger_event(#trigger_event{metadata = Metadata}) ->
    case is_valid_metadata(Metadata) of
        true -> ok;
        false -> {error, invalid_metadata}
    end.

%% @doc Validate state event
-spec validate_state_event(#state_event{}) -> ok | {error, term()}.
validate_state_event(#state_event{deployment_id = Id}) when byte_size(Id) > 0 -> ok;
validate_state_event(_) -> {error, invalid_deployment_id}.

%% @doc Check if event type is valid
-spec is_valid_event_type(atom()) -> boolean().
is_valid_event_type(trigger) -> true;
is_valid_event_type(state) -> true;
is_valid_event_type(approval) -> true;
is_valid_event_type(compliance) -> true;
is_valid_event_type(notification) -> true;
is_valid_event_type(audit) -> true;
is_valid_event_type(_) -> false.

%%%-------------------------------------------------------------------
%%% Public API - Event Serialization
%%%-------------------------------------------------------------------

%% @doc Serialize event to map
-spec serialize(deployment_event()) -> map().
serialize(#trigger_event{metadata = M, source = S, git_ref = Ref, commit_sha = Sha, version = V, target_env = Env}) ->
    #{
        type => trigger,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        actor => M#event_metadata.actor,
        source => S,
        git_ref => Ref,
        commit_sha => Sha,
        version => V,
        target_env => Env
    };
serialize(#state_event{metadata = M, deployment_id = Id, previous_state = Prev, current_state = Cur, reason = R}) ->
    #{
        type => state,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        actor => M#event_metadata.actor,
        deployment_id => Id,
        previous_state => Prev,
        current_state => Cur,
        reason => R
    };
serialize(#approval_event{metadata = M, deployment_id = Id, gate_name = Gate, state = S, approver = A}) ->
    #{
        type => approval,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        deployment_id => Id,
        gate_name => Gate,
        state => S,
        approver => A
    };
serialize(#compliance_event{metadata = M, deployment_id = Id, gate_name = Gate, status = S}) ->
    #{
        type => compliance,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        deployment_id => Id,
        gate_name => Gate,
        status => S
    };
serialize(#notification_event{metadata = M, deployment_id = Id, channel = C, severity = S}) ->
    #{
        type => notification,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        deployment_id => Id,
        channel => C,
        severity => S
    };
serialize(#audit_event{metadata = M, deployment_id = Id, environment = Env, version = V, action = A}) ->
    #{
        type => audit,
        id => M#event_metadata.id,
        category => M#event_metadata.category,
        timestamp => M#event_metadata.timestamp,
        actor => M#event_metadata.actor,
        deployment_id => Id,
        environment => Env,
        version => V,
        action => A
    }.

%% @doc Deserialize map to event
-spec deserialize(map()) -> {ok, deployment_event()} | {error, term()}.
deserialize(#{type := trigger} = Map) ->
    {ok, #trigger_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = trigger,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map),
            actor = maps:get(actor, Map)
        },
        source = maps:get(source, Map),
        git_ref = maps:get(git_ref, Map),
        commit_sha = maps:get(commit_sha, Map),
        version = maps:get(version, Map),
        target_env = maps:get(target_env, Map)
    }};
deserialize(#{type := state} = Map) ->
    {ok, #state_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = state,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map),
            actor = maps:get(actor, Map)
        },
        deployment_id = maps:get(deployment_id, Map),
        previous_state = maps:get(previous_state, Map),
        current_state = maps:get(current_state, Map)
    }};
deserialize(#{type := approval} = Map) ->
    {ok, #approval_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = approval,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map)
        },
        deployment_id = maps:get(deployment_id, Map),
        gate_name = maps:get(gate_name, Map),
        state = maps:get(state, Map),
        approver = maps:get(approver, Map)
    }};
deserialize(#{type := compliance} = Map) ->
    {ok, #compliance_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = compliance,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map)
        },
        deployment_id = maps:get(deployment_id, Map),
        gate_name = maps:get(gate_name, Map),
        status = maps:get(status, Map)
    }};
deserialize(#{type := notification} = Map) ->
    {ok, #notification_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = notification,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map)
        },
        deployment_id = maps:get(deployment_id, Map),
        channel = maps:get(channel, Map),
        severity = maps:get(severity, Map)
    }};
deserialize(#{type := audit} = Map) ->
    {ok, #audit_event{
        metadata = #event_metadata{
            id = maps:get(id, Map),
            type = audit,
            category = maps:get(category, Map),
            timestamp = maps:get(timestamp, Map),
            actor = maps:get(actor, Map)
        },
        deployment_id = maps:get(deployment_id, Map),
        environment = maps:get(environment, Map),
        version = maps:get(version, Map),
        action = maps:get(action, Map)
    }};
deserialize(_) -> {error, unknown_event_type}.

%% @doc Convert event to JSON
-spec to_json(deployment_event()) -> {ok, binary()} | {error, term()}.
to_json(Event) ->
    try
        Map = serialize(Event),
        Json = jsx:encode(Map),
        {ok, Json}
    catch
        _:_ -> {error, json_encode_failed}
    end.

%% @doc Parse JSON to event
-spec from_json(binary()) -> {ok, deployment_event()} | {error, term()}.
from_json(Json) ->
    try
        Map = jsx:decode(Json, [return_maps]),
        deserialize(Map)
    catch
        _:_ -> {error, json_decode_failed}
    end.

%%%-------------------------------------------------------------------
%%% Public API - Event Queries
%%%-------------------------------------------------------------------

%% @doc Get event ID
-spec get_event_id(deployment_event()) -> event_id().
get_event_id(#trigger_event{metadata = M}) -> M#event_metadata.id;
get_event_id(#state_event{metadata = M}) -> M#event_metadata.id;
get_event_id(#approval_event{metadata = M}) -> M#event_metadata.id;
get_event_id(#compliance_event{metadata = M}) -> M#event_metadata.id;
get_event_id(#notification_event{metadata = M}) -> M#event_metadata.id;
get_event_id(#audit_event{metadata = M}) -> M#event_metadata.id.

%% @doc Get event type
-spec get_event_type(deployment_event()) -> event_type().
get_event_type(#trigger_event{}) -> trigger;
get_event_type(#state_event{}) -> state;
get_event_type(#approval_event{}) -> approval;
get_event_type(#compliance_event{}) -> compliance;
get_event_type(#notification_event{}) -> notification;
get_event_type(#audit_event{}) -> audit.

%% @doc Get event timestamp
-spec get_event_timestamp(deployment_event()) -> erlang:timestamp().
get_event_timestamp(#trigger_event{metadata = M}) -> M#event_metadata.timestamp;
get_event_timestamp(#state_event{metadata = M}) -> M#event_metadata.timestamp;
get_event_timestamp(#approval_event{metadata = M}) -> M#event_metadata.timestamp;
get_event_timestamp(#compliance_event{metadata = M}) -> M#event_metadata.timestamp;
get_event_timestamp(#notification_event{metadata = M}) -> M#event_metadata.timestamp;
get_event_timestamp(#audit_event{metadata = M}) -> M#event_metadata.timestamp.

%% @doc Get event actor
-spec get_event_actor(deployment_event()) -> binary() | undefined.
get_event_actor(#trigger_event{metadata = M}) -> M#event_metadata.actor;
get_event_actor(#state_event{metadata = M}) -> M#event_metadata.actor;
get_event_actor(#approval_event{metadata = M}) -> M#event_metadata.actor;
get_event_actor(#compliance_event{metadata = M}) -> M#event_metadata.actor;
get_event_actor(#notification_event{metadata = M}) -> M#event_metadata.actor;
get_event_actor(#audit_event{metadata = M}) -> M#event_metadata.actor.

%% @doc Filter events by type
-spec filter_by_type(event_type(), [deployment_event()]) -> [deployment_event()].
filter_by_type(Type, Events) ->
    lists:filter(fun(E) -> get_event_type(E) =:= Type end, Events).

%% @doc Filter events by actor
-spec filter_by_actor(binary(), [deployment_event()]) -> [deployment_event()].
filter_by_actor(Actor, Events) ->
    lists:filter(fun(E) -> get_event_actor(E) =:= Actor end, Events).

%% @doc Filter events by time range
-spec filter_by_timerange(erlang:timestamp(), erlang:timestamp(), [deployment_event()]) -> [deployment_event()].
filter_by_timerange(StartTime, EndTime, Events) ->
    lists:filter(fun(E) ->
        TS = get_event_timestamp(E),
        TS >= StartTime andalso TS =< EndTime
    end, Events).

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

%% @private Generate unique event ID
generate_event_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary([
        uuid_uuid_to_string(Id)
    ]).

%% @private Format UUID to string
uuid_uuid_to_string(<<TL:32, TM:16, TH:16, CSR:8, CSL:8, N:48>>) ->
    lists:flatten(io_lib:format(
        "~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b",
        [TL, TM, TH, CSR, CSL, N]
    )).

%% @private Generate correlation ID
generate_correlation_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    iolist_to_binary([
        uuid_uuid_to_string(Id)
    ]).

%% @private Validate metadata structure
is_valid_metadata(#event_metadata{id = Id, timestamp = TS})
    when byte_size(Id) > 0, is_integer(TS), TS > 0 ->
    true;
is_valid_metadata(_) ->
    false.

%% @private Validate approval event
do_validate_approval(#approval_event{deployment_id = Id, approver = A, state = S})
    when byte_size(Id) > 0, byte_size(A) > 0, S =/= undefined ->
    {ok, true};
do_validate_approval(_) ->
    {ok, false}.
