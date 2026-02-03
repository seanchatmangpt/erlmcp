-module(erlmcp_continuous_verification).
-behaviour(gen_server).

%% API
-export([start_link/0, verify/3, verify_identity/1, verify_device/1]).
-export([start_verification_session/2, end_verification_session/1]).
-export([add_verification_factor/3, verify_factor/3]).
-export([get_verification_status/1, get_verification_history/2]).
-export([configure_verification_policy/2, trigger_verification/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Records
-record(verification_session, {
    id :: binary(),
    identity_id :: binary(),
    device_id :: binary(),
    created_at :: integer(),
    expires_at :: integer(),
    factors :: list(),
    status :: active | completed | failed | expired,
    context :: map(),
    risk_score :: float(),
    last_verified :: integer()
}).

-record(verification_factor, {
    type :: password | mfa | biometric | location | behavior | device,
    value :: binary(),
    verified :: boolean(),
    verified_at :: integer(),
    confidence :: float(),
    metadata :: map()
}).

-record(verification_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    factors_required :: list(),
    risk_threshold :: float(),
    verification_interval :: integer(),
    enabled :: boolean(),
    conditions :: list()
}).

-record(state, {
    sessions :: map(),
    policies :: map(),
    history :: list(),
    config :: map()
}).

-define(TIMEOUT, 30000).
-define(DEFAULT_RISK_THRESHOLD, 0.7).
-define(DEFAULT_INTERVAL, 3600000). %% 1 hour

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

verify(IdentityId, Resource, Action) ->
    gen_server:call(?MODULE, {verify, IdentityId, Resource, Action}, ?TIMEOUT).

verify_identity(IdentityId) ->
    gen_server:call(?MODULE, {verify_identity, IdentityId}, ?TIMEOUT).

verify_device(DeviceId) ->
    gen_server:call(?MODULE, {verify_device, DeviceId}, ?TIMEOUT).

start_verification_session(IdentityId, DeviceId) ->
    gen_server:call(?MODULE, {start_verification_session, IdentityId, DeviceId}, ?TIMEOUT).

end_verification_session(SessionId) ->
    gen_server:call(?MODULE, {end_verification_session, SessionId}, ?TIMEOUT).

add_verification_factor(SessionId, FactorType, FactorData) ->
    gen_server:call(?MODULE, {add_verification_factor, SessionId, FactorType, FactorData}, ?TIMEOUT).

verify_factor(SessionId, FactorType, FactorData) ->
    gen_server:call(?MODULE, {verify_factor, SessionId, FactorType, FactorData}, ?TIMEOUT).

get_verification_status(SessionId) ->
    gen_server:call(?MODULE, {get_verification_status, SessionId}, ?TIMEOUT).

get_verification_history(IdentityId, Limit) ->
    gen_server:call(?MODULE, {get_verification_history, IdentityId, Limit}, ?TIMEOUT).

configure_verification_policy(PolicyId, PolicyData) ->
    gen_server:call(?MODULE, {configure_verification_policy, PolicyId, PolicyData}, ?TIMEOUT).

trigger_verification(IdentityId, Trigger) ->
    gen_server:call(?MODULE, {trigger_verification, IdentityId, Trigger}, ?TIMEOUT).

init([]) ->
    State = #state{
        sessions = #{},
        policies = load_default_policies(),
        history = [],
        config = load_config()
    },
    erlmcp_continuous_verification:initialize(),
    {ok, State}.

handle_call({verify, IdentityId, Resource, Action}, _From, State) ->
    case verify_identity_continuous(IdentityId, Resource, Action, State) of
        {verified, SessionId, RiskScore} ->
            {reply, {ok, verified, SessionId, RiskScore}, State};
        {not_verified, Reason} ->
            {reply, {error, not_verified, Reason}, State}
    end;

handle_call({verify_identity, IdentityId}, _From, State) ->
    case verify_identity_standalone(IdentityId, State) of
        {verified, RiskScore} ->
            {reply, {ok, verified, RiskScore}, State};
        {not_verified, Reason} ->
            {reply, {error, not_verified, Reason}, State}
    end;

handle_call({verify_device, DeviceId}, _From, State) ->
    case verify_device_standalone(DeviceId, State) of
        {verified, TrustScore} ->
            {reply, {ok, verified, TrustScore}, State};
        {not_verified, Reason} ->
            {reply, {error, not_verified, Reason}, State}
    end;

handle_call({start_verification_session, IdentityId, DeviceId}, _From, State) ->
    case create_verification_session(IdentityId, DeviceId, State) of
        {ok, SessionId} ->
            {reply, {ok, SessionId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({end_verification_session, SessionId}, _From, State) ->
    case end_session(SessionId, State) of
        {ok, SessionData} ->
            {reply, {ok, SessionData}, State};
        {error, not_found} ->
            {reply, {error, not_found}, State}
    end;

handle_call({add_verification_factor, SessionId, FactorType, FactorData}, _From, State) ->
    case add_factor_to_session(SessionId, FactorType, FactorData, State) of
        {ok, SessionId} ->
            {reply, {ok, SessionId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({verify_factor, SessionId, FactorType, FactorData}, _From, State) ->
    case verify_single_factor(SessionId, FactorType, FactorData, State) of
        {verified, Confidence} ->
            {reply, {ok, verified, Confidence}, State};
        {not_verified, Reason} ->
            {reply, {error, not_verified, Reason}, State}
    end;

handle_call({get_verification_status, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            Status = #{
                session_id => Session#verification_session.id,
                identity_id => Session#verification_session.identity_id,
                device_id => Session#verification_session.device_id,
                status => Session#verification_session.status,
                risk_score => Session#verification_session.risk_score,
                factors_verified => lists:filter(fun(F) -> F#verification_factor.verified end, Session#verification_session.factors),
                total_factors => length(Session#verification_session.factors)
            },
            {reply, {ok, Status}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_verification_history, IdentityId, Limit}, _From, State) ->
    FilteredHistory = lists:filter(fun(H) ->
        H#verification_session.identity_id == IdentityId
    end, State#state.history),
    LimitedHistory = lists:sublist(FilteredHistory, Limit),
    {reply, {ok, LimitedHistory}, State};

handle_call({configure_verification_policy, PolicyId, PolicyData}, _From, State) ->
    case validate_policy_data(PolicyData) of
        {ok, ValidatedData} ->
            Policy = #verification_policy{
                id = PolicyId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                factors_required = maps:get(factors_required, ValidatedData),
                risk_threshold = maps:get(risk_threshold, ValidatedData, ?DEFAULT_RISK_THRESHOLD),
                verification_interval = maps:get(verification_interval, ValidatedData, ?DEFAULT_INTERVAL),
                enabled = maps:get(enabled, ValidatedData, true),
                conditions = maps:get(conditions, ValidatedData, [])
            },
            NewState = State#state{
                policies = maps:put(PolicyId, Policy, State#state.policies)
            },
            {reply, {ok, PolicyId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({trigger_verification, IdentityId, Trigger}, _From, State) ->
    case create_triggered_verification(IdentityId, Trigger, State) of
        {ok, SessionId} ->
            {reply, {ok, SessionId}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions
initialize() ->
    %% Initialize verification factors
    %% Configure risk assessment engine
    %% Set up monitoring for verification timeouts
    ok.

load_config() ->
    #{
        session_timeout => 300000, %% 5 minutes
        max_sessions_per_identity => 5,
        risk_evaluation_interval => 60000, %% 1 minute
        continuous_verification_enabled => true,
        risk_threshold => ?DEFAULT_RISK_THRESHOLD,
        factors_required => [password, device]
    }.

load_default_policies() ->
    %% Load default verification policies for Fortune 500
    #{
        standard => #verification_policy{
            id => <<"standard">>,
            name => <<"Standard Verification">>,
            description => <<"Standard identity verification">>,
            factors_required => [password, device],
            risk_threshold => ?DEFAULT_RISK_THRESHOLD,
            verification_interval => ?DEFAULT_INTERVAL,
            enabled => true,
            conditions => []
        },
        high_risk => #verification_policy{
            id => <<"high_risk">>,
            name => <<"High Risk Verification">>,
            description => <<"Enhanced verification for high-risk operations">>,
            factors_required => [password, mfa, device, location],
            risk_threshold => 0.9,
            verification_interval => 300000, %% 5 minutes
            enabled => true,
            conditions => [high_risk_operation]
        },
        admin => #verification_policy{
            id => <<"admin">>,
            name => <<"Administrator Verification">>,
            description => <<"Strict verification for admin operations">>,
            factors_required => [password, mfa, biometric, location, behavior],
            risk_threshold => 0.95,
            verification_interval => 60000, %% 1 minute
            enabled => true,
            conditions => [admin_operation]
        }
    }.

verify_identity_continuous(IdentityId, Resource, Action, State) ->
    %% Check if identity needs continuous verification
    RiskScore = calculate_risk_score(IdentityId, State),

    case RiskScore > State#state.config.risk_threshold of
        true ->
            %% Risk exceeds threshold - require verification
            case get_active_session(IdentityId, State) of
                {ok, SessionId} ->
                    %% Check if session is valid
                    case verify_session_validity(SessionId, State) of
                        true ->
                            {verified, SessionId, RiskScore};
                        false ->
                            %% Start new verification session
                            case create_verification_session(IdentityId, undefined, State) of
                                {ok, NewSessionId} ->
                                    {verified, NewSessionId, RiskScore};
                                {error, Reason} ->
                                    {not_verified, Reason}
                            end
                    end;
                {error, no_active_session} ->
                    %% Start new verification session
                    case create_verification_session(IdentityId, undefined, State) of
                        {ok, SessionId} ->
                            {verified, SessionId, RiskScore};
                        {error, Reason} ->
                            {not_verified, Reason}
                    end
            end;
        false ->
            %% Risk is acceptable - allow access
            {verified, <<"system_bypass">>, RiskScore}
    end.

verify_identity_standalone(IdentityId, State) ->
    case verify_identity_factors(IdentityId, State) of
        {verified, RiskScore} ->
            {verified, RiskScore};
        {not_verified, Reason} ->
            {not_verified, Reason}
    end.

verify_device_standalone(DeviceId, State) ->
    case erlmcp_identity_provider:validate_device(DeviceId) of
        {ok, DeviceData} ->
            TrustScore = maps:get(trust_score, DeviceData, 0.5),
            case TrustScore > 0.7 of
                true ->
                    {verified, TrustScore};
                false ->
                    {not_verified, "device_trust_low"}
            end;
        {error, not_found} ->
            {not_verified, "device_not_found"}
    end.

create_verification_session(IdentityId, DeviceId, State) ->
    SessionId = generate_session_id(),
    PolicyId = get_appropriate_policy(IdentityId, State),
    Policy = maps:get(PolicyId, State#state.policies),

    Session = #verification_session{
        id = SessionId,
        identity_id = IdentityId,
        device_id = DeviceId,
        created_at = timestamp(),
        expires_at = timestamp() + Policy#verification_policy.verification_interval,
        factors = create_required_factors(Policy#verification_policy.factors_required),
        status = active,
        context = extract_verification_context(),
        risk_score = calculate_initial_risk(IdentityId, State),
        last_verified = timestamp()
    },

    NewState = State#state{
        sessions = maps:put(SessionId, Session, State#state.sessions)
    },

    %% Schedule periodic risk assessment
    schedule_risk_assessment(SessionId),
    {ok, SessionId}.

end_session(SessionId, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            CompletedSession = Session#verification_session{
                status = completed,
                last_verified = timestamp()
            },
            %% Add to history
            NewHistory = [CompletedSession|State#state.history],
            NewState = State#state{
                sessions = maps:remove(SessionId, State#state.sessions),
                history = NewHistory
            },
            {ok, CompletedSession};
        error ->
            {error, not_found}
    end.

add_factor_to_session(SessionId, FactorType, FactorData, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            Factor = #verification_factor{
                type = FactorType,
                value = FactorData,
                verified = false,
                verified_at = 0,
                confidence = 0.0,
                metadata = #{}
            },
            NewFactors = [Factor|Session#verification_session.factors],
            UpdatedSession = Session#verification_session{
                factors = NewFactors
            },
            NewState = State#state{
                sessions = maps:put(SessionId, UpdatedSession, State#state.sessions)
            },
            {ok, SessionId};
        error ->
            {error, not_found}
    end.

verify_single_factor(SessionId, FactorType, FactorData, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            case verify_factor_logic(FactorType, FactorData, Session) of
                {verified, Confidence} ->
                    UpdatedFactors = lists:map(fun(F) ->
                        case F#verification_factor.type == FactorType of
                            true ->
                                F#verification_factor{
                                    verified = true,
                                    verified_at = timestamp(),
                                    confidence = Confidence
                                };
                            false ->
                                F
                        end
                    end, Session#verification_session.factors),

                    UpdatedSession = Session#verification_session{
                        factors = UpdatedFactors,
                        last_verified = timestamp()
                    },

                    NewState = State#state{
                        sessions = maps:put(SessionId, UpdatedSession, State#state.sessions)
                    },

                    %% Check if all factors are verified
                    case all_factors_verified(UpdatedFactors) of
                        true ->
                            %% Session fully verified
                            FullyVerifiedSession = UpdatedSession#verification_session{
                                status = completed
                            },
                            NewState2 = NewState#state{
                                sessions = maps:put(SessionId, FullyVerifiedSession, NewState#state.sessions)
                            },
                            {verified, Confidence};
                        false ->
                            {verified, Confidence}
                    end;
                {not_verified, Reason} ->
                    {not_verified, Reason}
            end;
        error ->
            {error, not_found}
    end.

verify_factor_logic(password, Password, _Session) ->
    %% Verify password identity
    case erlmcp_identity_provider:authenticate(Password, #{password => Password}) of
        {ok, _} ->
            {verified, 0.9};
        {error, _} ->
            {not_verified, "invalid_password"}
    end;

verify_factor_logic(mfa, Code, _Session) ->
    %% Verify MFA code
    case erlmcp_mfa_provider:verify_code(Code) of
        {ok, verified} ->
            {verified, 0.95};
        {error, _} ->
            {not_verified, "invalid_mfa"}
    end;

verify_factor_logic(device, DeviceId, Session) ->
    %% Verify device
    case erlmcp_identity_provider:validate_device(DeviceId) of
        {ok, _} ->
            {verified, 0.8};
        {error, _} ->
            {not_verified, "untrusted_device"}
    end;

verify_factor_logic(location, Location, _Session) ->
    %% Verify location
    ExpectedLocation = erlmcp_identity_provider:get_identity_location(Session#verification_session.identity_id),
    case verify_location(Location, ExpectedLocation) of
        true ->
            {verified, 0.7};
        false ->
            {not_verified, "suspicious_location"}
    end;

verify_factor_logic(behavior, BehaviorData, Session) ->
    %% Verify behavioral pattern
    case verify_behavioral_pattern(Session#verification_session.identity_id, BehaviorData) of
        {ok, Confidence} ->
            {verified, Confidence};
        {error, Reason} ->
            {not_verified, Reason}
    end;

verify_factor_logic(biometric, BioData, _Session) ->
    %% Verify biometric data
    case erlmcp_biometric_provider:verify(BioData) of
        {ok, verified} ->
            {verified, 0.98};
        {error, _} ->
            {not_verified, "biometric_mismatch"}
    end.

create_required_factors(FactorTypes) ->
    lists:map(fun(Type) ->
        #verification_factor{
            type = Type,
            verified = false,
            confidence = 0.0
        }
    end, FactorTypes).

get_appropriate_policy(IdentityId, State) ->
    %% Get policy based on identity risk profile
    RiskProfile = erlmcp_risk_assessment:get_risk_profile(IdentityId),

    case RiskProfile of
        high ->
            <<"high_risk">>;
        admin ->
            <<"admin">>;
        _ ->
            <<"standard">>
    end.

calculate_risk_score(IdentityId, State) ->
    %% Calculate comprehensive risk score
    Factors = [
        {authentication_risk, calculate_authentication_risk(IdentityId)},
        {device_risk, calculate_device_risk(IdentityId)},
        {location_risk, calculate_location_risk(IdentityId)},
        {behavior_risk, calculate_behavior_risk(IdentityId)},
        {time_risk, calculate_time_risk(IdentityId)}
    ],

    calculate_weighted_risk(Factors).

calculate_authentication_risk(IdentityId) ->
    %% Risk based on authentication history
    case erlmcp_security_monitor:get_authentication_events(IdentityId) of
        {ok, Events} ->
            FailedAuths = lists:filter(fun(E) -> E#event.type == failed_login end, Events),
            case length(FailedAuths) of
                0 -> 0.1;
                1 -> 0.3;
                2 -> 0.6;
                _ -> 0.9
            end;
        {error, _} ->
            0.5
    end.

calculate_device_risk(IdentityId) ->
    %% Risk based on device trust
    case erlmcp_identity_provider:get_device_info(IdentityId) of
        {ok, Device} ->
            case Device#device.trust_score of
                Score when Score > 0.8 -> 0.1;
                Score when Score > 0.6 -> 0.3;
                Score when Score > 0.4 -> 0.6;
                _ -> 0.9
            end;
        {error, _} ->
            0.7
    end.

calculate_location_risk(IdentityId) ->
    %% Risk based on location anomalies
    case erlmcp_geolocation:get_current_location(IdentityId) of
        {ok, CurrentLocation} ->
            ExpectedLocations = erlmcp_identity_provider:get_allowed_locations(IdentityId),
            case lists:any(fun(L) -> locations_match(CurrentLocation, L) end, ExpectedLocations) of
                true -> 0.1;
                false -> 0.8
            end;
        {error, _} ->
            0.5
    end.

calculate_behavior_risk(IdentityId) ->
    %% Risk based on behavioral anomalies
    case erlmcp_behavioral_analysis.analyze(IdentityId) of
        {ok, Analysis} ->
            case Analysis#behavior_analysis.anomaly_score of
                Score when Score < 0.2 -> 0.1;
                Score when Score < 0.5 -> 0.3;
                Score when Score < 0.8 -> 0.6;
                _ -> 0.9
            end;
        {error, _} ->
            0.4
    end.

calculate_time_risk(IdentityId) ->
    %% Risk based on time-based patterns
    CurrentTime = erlang:system_time(second),
    LastActive = erlmcp_identity_provider:get_last_active(IdentityId),

    case (CurrentTime - LastActive) > 86400 of
        true -> 0.3;
        false -> 0.1
    end.

calculate_weighted_risk(Factors) ->
    Weights = [
        {authentication_risk, 0.3},
        {device_risk, 0.2},
        {location_risk, 0.2},
        {behavior_risk, 0.2},
        {time_risk, 0.1}
    ],

    lists:foldl(fun({Factor, Value}, Acc) ->
        Weight = proplists:get_value(Factor, Weights),
        Acc + Value * Weight
    end, 0.0, Factors).

calculate_initial_risk(IdentityId, State) ->
    %% Calculate initial risk score for session
    calculate_risk_score(IdentityId, State).

verify_session_validity(SessionId, State) ->
    case maps:find(SessionId, State#state.sessions) of
        {ok, Session} ->
            Session#verification_session.expires_at > timestamp();
        error ->
            false
    end.

get_active_session(IdentityId, State) ->
    Sessions = lists:filter(fun(S) ->
        S#verification_session.identity_id == IdentityId andalso
        S#verification_session.status == active
    end, maps:values(State#state.sessions)),

    case Sessions of
        [Session|_] ->
            {ok, Session#verification_session.id};
        _ ->
            {error, no_active_session}
    end.

all_factors_verified(Factors) ->
    lists:all(fun(F) -> F#verification_factor.verified end, Factors).

verify_location(Location, ExpectedLocation) ->
    %% Simple location verification - can be enhanced with geofencing
    case calculate_distance(Location, ExpectedLocation) of
        Distance when Distance < 1000 -> true; %% Within 1km
        _ -> false
    end.

calculate_distance(Location1, Location2) ->
    %% Calculate distance between two locations
    %% Simplified implementation
    0.0.

verify_behavioral_pattern(IdentityId, BehaviorData) ->
    %% Verify behavioral pattern against known patterns
    case erlmcp_behavioral_analytics.verify_pattern(IdentityId, BehaviorData) of
        {ok, Confidence} ->
            {ok, Confidence};
        {error, Reason} ->
            {error, Reason}
    end.

create_triggered_verification(IdentityId, Trigger, State) ->
    case Trigger of
        suspicious_login ->
            create_verification_session(IdentityId, undefined, State);
        high_risk_operation ->
            create_verification_session(IdentityId, undefined, State);
        admin_operation ->
            create_verification_session(IdentityId, undefined, State);
        _ ->
            {error, "invalid_trigger"}
    end.

schedule_risk_assessment(SessionId) ->
    %% Schedule periodic risk assessment for session
    erlang:send_after(60000, self(), {assess_risk, SessionId}).

generate_session_id() ->
    crypto:strong_rand_bytes(32).

timestamp() ->
    erlang:system_time(millisecond).

extract_verification_context() ->
    #{ip => get_client_ip(),
      user_agent => get_user_agent(),
      timestamp => timestamp()}.