%% -*- erlang -*-
%%====================================================================
%% Zero-Trust Identity Management System
%%====================================================================
-module(erlmcp_identity_manager).
-behaviour(gen_server).
-include("erlmcp_zero_trust_app.hrl").

%% API
-export([
    start_link/0,
    authenticate/2,
    verify_identity/1,
    create_session/2,
    validate_token/1,
    get_identity_attributes/1,
    update_trust_score/3,
    add_risk_factor/2,
    remove_risk_factor/2
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

authenticate(Credentials, Context) ->
    gen_server:call(?MODULE, {authenticate, Credentials, Context}).

verify_identity(IdentityId) ->
    gen_server:call(?MODULE, {verify_identity, IdentityId}).

create_session(IdentityId, Request) ->
    gen_server:call(?MODULE, {create_session, IdentityId, Request}).

validate_token(Token) ->
    gen_server:call(?MODULE, {validate_token, Token}).

get_identity_attributes(IdentityId) ->
    gen_server:call(?MODULE, {get_identity_attributes, IdentityId}).

update_trust_score(IdentityId, TrustScore, Reason) ->
    gen_server:call(?MODULE, {update_trust_score, IdentityId, TrustScore, Reason}).

add_risk_factor(IdentityId, RiskFactor) ->
    gen_server:call(?MODULE, {add_risk_factor, IdentityId, RiskFactor}).

remove_risk_factor(IdentityId, RiskFactor) ->
    gen_server:call(?MODULE, {remove_risk_factor, IdentityId, RiskFactor}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize identity store
    IdentityStore = ets:new(identity_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),
    SessionStore = ets:new(session_store, [set, protected, named_table, {keypos, element(2, record_info(state, 2))}]),

    %% Load identities from secure storage
    {ok, #{
        identity_store => IdentityStore,
        session_store => SessionStore,
        config => load_zero_trust_config()
    }}.

handle_call({authenticate, Credentials, Context}, _From, State) ->
    Result = do_authenticate(Credentials, Context, State),
    {reply, Result, State};

handle_call({verify_identity, IdentityId}, _From, State) ->
    Result = do_verify_identity(IdentityId, State),
    {reply, Result, State};

handle_call({create_session, IdentityId, Request}, _From, State) ->
    Result = do_create_session(IdentityId, Request, State),
    {reply, Result, State};

handle_call({validate_token, Token}, _From, State) ->
    Result = do_validate_token(Token, State),
    {reply, Result, State};

handle_call({get_identity_attributes, IdentityId}, _From, State) ->
    Result = do_get_identity_attributes(IdentityId, State),
    {reply, Result, State};

handle_call({update_trust_score, IdentityId, TrustScore, Reason}, _From, State) ->
    Result = do_update_trust_score(IdentityId, TrustScore, Reason, State),
    {reply, Result, State};

handle_call({add_risk_factor, IdentityId, RiskFactor}, _From, State) ->
    Result = do_add_risk_factor(IdentityId, RiskFactor, State),
    {reply, Result, State};

handle_call({remove_risk_factor, IdentityId, RiskFactor}, _From, State) ->
    Result = do_remove_risk_factor(IdentityId, RiskFactor, State),
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

do_authenticate(Credentials, Context, State) ->
    #{identity_store := IdentityStore, config := Config} = State,

    %% Extract credentials
    #{<<"username">> := Username, <<"password">> := Password} = Credentials,
    #{<<"device_fingerprint">> := DeviceFingerprint} = Context,

    %% Find identity
    case ets:lookup(IdentityStore, Username) of
        [#identity{id = Id, attributes = Attrs} = Identity] ->
            %% Verify password
            case verify_password(Password, Attrs) of
                true ->
                    %% Verify MFA if enabled
                    MFAEnabled = maps:get(mfa_enabled, Config, false),
                    case MFAEnabled of
                        true ->
                            case verify_mfa(Id, Context) of
                                true ->
                                    %% Verify device trust
                                    verify_device_trust(DeviceFingerprint, Identity, Config);
                                false ->
                                    {error, invalid_mfa}
                            end;
                        false ->
                            verify_device_trust(DeviceFingerprint, Identity, Config)
                    end;
                false ->
                    {error, invalid_credentials}
            end;
        [] ->
            {error, identity_not_found}
    end.

do_verify_identity(IdentityId, State) ->
    #{identity_store := IdentityStore} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            %% Check if identity is still valid
            CurrentTime = erlang:system_time(second),
            LastAuth = Identity#identity.last_authenticated,

            %% Check if expired
            if
                CurrentTime - LastAuth > 86400 * 30 -> %% 30 days
                    {error, identity_expired};
                true ->
                    %% Check risk factors
                    RiskFactors = Identity#identity.risk_factors,
                    TrustScore = calculate_trust_score(RiskFactors),

                    #verification_result{
                        success = true,
                        message = identity_verified,
                        risk_score = TrustScore,
                        checks = [
                            #{type => identity, status => verified},
                            #{type => risk_score, value => TrustScore, threshold => 0.7}
                        ],
                        timestamp = CurrentTime,
                        expires = CurrentTime + 3600 %% 1 hour
                    }
            end;
        [] ->
            {error, identity_not_found}
    end.

do_create_session(IdentityId, Request, State) ->
    #{identity_store := IdentityStore, config := Config} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            %% Validate request context
            Context = maps:get(context, Request, #{}),
            #access_request{
                resource = Resource,
                action = Action,
                justification = Justification
            } = Request,

            %% Apply security policies
            Policies = load_security_policies(),
            case evaluate_policies(Policies, Identity, Request, Context) of
                allow ->
                    %% Create session
                    SessionId = generate_session_id(),
                    Session = #{
                        id => SessionId,
                        identity_id => IdentityId,
                        resource => Resource,
                        action => Action,
                        created => erlang:system_time(second),
                        expires => erlang:system_time(second) + maps:get(max_session_duration, Config, 3600),
                        context => Context,
                        justification => Justification,
        risk_assessment => calculate_session_risk(Identity, Context)
                    },

                    %% Store session
                    ets:insert(SessionStore, {SessionId, Session}),

                    %% Generate JWT
                    Token = generate_jwt(Session, Config),

                    {ok, #{
                        session_id => SessionId,
                        token => Token,
                        expires => Session#session.expires
                    }};
                deny ->
                    {error, access_denied}
            end;
        [] ->
            {error, identity_not_found}
    end.

do_validate_token(Token, State) ->
    try
        %% Decode JWT
        Decoded = verify_jwt(Token),
        SessionId = maps:get(session_id, Decoded),

        #{session_store := SessionStore} = State,

        case ets:lookup(SessionStore, SessionId) of
            [Session] ->
                CurrentTime = erlang:system_time(second),
                if
                    CurrentTime < Session#session.expires ->
                        %% Check if session needs renewal
                        if
                            CurrentTime > Session#session.expires - 300 %% 5 minutes before expiry
                            -> renew_session(Session);
                        true ->
                            {ok, Session}
                        end;
                    true ->
                        {error, session_expired}
                end;
            [] ->
                {error, invalid_session}
        end
    catch
        _:_ ->
            {error, invalid_token}
    end.

do_get_identity_attributes(IdentityId, State) ->
    #{identity_store := IdentityStore} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            {ok, Identity#identity.attributes};
        [] ->
            {error, identity_not_found}
    end.

do_update_trust_score(IdentityId, TrustScore, Reason, State) ->
    #{identity_store := IdentityStore} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            UpdatedIdentity = Identity#identity{
                trust_score = TrustScore,
                last_authenticated = erlang:system_time(second)
            },
            ets:insert(IdentityStore, UpdatedIdentity),

            %% Log trust score update
            ?LOG_INFO("Trust score updated for ~p: ~p (reason: ~p)",
                     [IdentityId, TrustScore, Reason]),

            {ok, updated};
        [] ->
            {error, identity_not_found}
    end.

do_add_risk_factor(IdentityId, RiskFactor, State) ->
    #{identity_store := IdentityStore} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            UpdatedRiskFactors = [RiskFactor | Identity#identity.risk_factors],
            UpdatedIdentity = Identity#identity{
                risk_factors = UpdatedRiskFactors,
                trust_score = calculate_trust_score(UpdatedRiskFactors),
                last_authenticated = erlang:system_time(second)
            },
            ets:insert(IdentityStore, UpdatedIdentity),

            %% Log risk factor addition
            ?LOG_WARNING("Risk factor added for ~p: ~p", [IdentityId, RiskFactor]),

            {ok, updated};
        [] ->
            {error, identity_not_found}
    end.

do_remove_risk_factor(IdentityId, RiskFactor, State) ->
    #{identity_store := IdentityStore} = State,

    case ets:lookup(IdentityStore, IdentityId) of
        [Identity] ->
            UpdatedRiskFactors = lists:delete(RiskFactor, Identity#identity.risk_factors),
            UpdatedIdentity = Identity#identity{
                risk_factors = UpdatedRiskFactors,
                trust_score = calculate_trust_score(UpdatedRiskFactors),
                last_authenticated = erlang:system_time(second)
            },
            ets:insert(IdentityStore, UpdatedIdentity),

            %% Log risk factor removal
            ?LOG_INFO("Risk factor removed for ~p: ~p", [IdentityId, RiskFactor]),

            {ok, updated};
        [] ->
            {error, identity_not_found}
    end.

%% Helper functions
verify_password(Password, Attributes) ->
    %% Implement password verification
    %% Should be hashed and salted in production
    case maps:get(<<"password_hash">>, Attributes) of
        HashedPassword ->
            erlmcp_utils:verify_password(Password, HashedPassword);
        undefined ->
            false
    end.

verify_mfa(IdentityId, Context) ->
    %% Implement MFA verification
    #{<<"mfa_code">> := MFACode} = Context,

    %% Verify MFA code against TOTP or other method
    erlmcp_mfa:verify_code(IdentityId, MFACode).

verify_device_trust(DeviceFingerprint, Identity, Config) ->
    Threshold = maps:get(device_trust_score_threshold, Config, 0.7),
    TrustScore = Identity#identity.trust_score,

    if
        TrustScore >= Threshold ->
            #verification_result{
                success = true,
                message = device_trusted,
                risk_score = TrustScore,
                checks = [
                    #{type => device_fingerprint, status => verified},
                    #{type => trust_score, value => TrustScore, threshold => Threshold}
                ],
                timestamp = erlang:system_time(second),
                expires = erlang:system_time(second) + 300 %% 5 minutes
            };
        true ->
            #verification_result{
                success = false,
                message = device_untrusted,
                risk_score = TrustScore,
                checks = [
                    #{type => device_fingerprint, status => untrusted},
                    #{type => trust_score, value => TrustScore, threshold => Threshold}
                ],
                timestamp = erlang:system_time(second),
                expires = erlang:system_time(second) + 300
            }
    end.

calculate_trust_score(RiskFactors) ->
    %% Calculate trust score based on risk factors
    BaseScore = 1.0,
    Reduction = length(RiskFactors) * 0.1,
    max(0.0, BaseScore - Reduction).

calculate_session_risk(Identity, Context) ->
    %% Calculate risk score for session
    TrustScore = Identity#identity.trust_score,
    DeviceRisk = maps:get(device_risk, Context, 0.0),
    LocationRisk = maps:get(location_risk, Context, 0.0),

    RiskScore = (TrustScore + 1.0 - DeviceRisk + 1.0 - LocationRisk) / 3.0,
    min(1.0, max(0.0, RiskScore)).

evaluate_policies(Policies, Identity, Request, Context) ->
    %% Evaluate all policies against the request
    evaluate_policies(Policies, Identity, Request, Context, allow).

evaluate_policies([], _, _, _, Result) ->
    Result;

evaluate_policies([Policy | Rest], Identity, Request, Context, Result) ->
    if
        Result == allow ->
            case evaluate_policy(Policy, Identity, Request, Context) of
                allow ->
                    evaluate_policies(Rest, Identity, Request, Context, allow);
                deny ->
                    deny;
                log ->
                    evaluate_policies(Rest, Identity, Request, Context, Result)
            end;
        true ->
            evaluate_policies(Rest, Identity, Request, Context, Result)
    end.

evaluate_policy(Policy, Identity, Request, Context) ->
    #security_policy{
        enabled = Enabled,
        rules = Rules,
        enforcement = Enforcement
    } = Policy,

    if
        not Enabled ->
            allow;
        true ->
            %% Check all rules
            case check_rules(Rules, Identity, Request, Context) of
                true ->
                    case Enforcement of
                        allow -> allow;
                        deny -> deny;
                        log -> log
                    end;
                false ->
                    case Enforcement of
                        allow -> allow;
                        deny -> deny;
                        log -> allow
                    end
            end
    end.

check_rules([], _, _, _) ->
    true;

check_rules([Rule | Rest], Identity, Request, Context) ->
    case check_rule(Rule, Identity, Request, Context) of
        true ->
            check_rules(Rest, Identity, Request, Context);
        false ->
            false
    end.

check_rule(Rule, Identity, Request, Context) ->
    %% Implement rule checking based on rule type
    case maps:get(type, Rule) of
        identity ->
            check_identity_rule(Rule, Identity);
        network ->
            check_network_rule(Rule, Context);
        time ->
            check_time_rule(Rule, Request);
        device ->
            check_device_rule(Rule, Identity, Context);
        data ->
            check_data_rule(Rule, Request, Context);
        _ ->
            true
    end.

check_identity_rule(Rule, Identity) ->
    %% Check identity attributes against rule
    Attrs = Identity#identity.attributes,
    case maps:get(condition, Rule) of
        required ->
            lists:all(fun(Attr) -> maps:is_key(Attr, Attrs) end,
                      maps:get(attributes, Rule));
        _ ->
            true
    end.

check_network_rule(Rule, Context) ->
    %% Check network conditions
    case maps:get(source_network, Rule) of
        allowed ->
            SourceIP = maps:get(source_ip, Context),
            is_network_allowed(SourceIP, maps:get(allowed_networks, Rule));
        _ ->
            true
    end.

check_time_rule(Rule, Request) ->
    %% Check time-based conditions
    CurrentTime = erlang:system_time(second),
    case maps:get(time_window, Rule) of
        {start, End} when CurrentTime >= End ->
            false;
        {Start, end} when CurrentTime < Start ->
            false;
        _ ->
            true
    end.

check_device_rule(Rule, Identity, Context) ->
    %% Check device conditions
    case maps:get(device_fingerprint, Rule) of
        match ->
            DF = Identity#identity.device_fingerprint,
            maps:get(device_fingerprint, Context) == DF;
        _ ->
            true
    end.

check_data_rule(Rule, Request, Context) ->
    %% Check data classification rules
    case maps:get(data_classification, Rule) of
        restricted ->
            maps:get(data_type, Context) /= restricted;
        _ ->
            true
    end.

generate_session_id() ->
    %% Generate unique session ID
    crypto:strong_rand_bytes(16).

generate_jwt(Session, Config) ->
    %% Generate JWT for session
    Claims = #{
        sub => Session#session.identity_id,
        session_id => Session#session.id,
        resource => Session#session.resource,
        action => Session#session.action,
        iat => Session#session.created,
        exp => Session#session.expires,
        jti => generate_session_id(),
        risk => Session#session.risk_assessment
    },

    Secret = maps:get(jwt_secret, Config),
    jose_jwt:sign(Claims, Secret).

verify_jwt(Token) ->
    %% Verify JWT token
    jose_jwt:verify(Token).

renew_session(Session) ->
    %% Extend session expiry
    RenewedSession = Session#session{
        expires = erlang:system_time(second) + 3600 %% 1 hour
    },
    ets:insert(session_store, {RenewedSession#session.id, RenewedSession}),
    {ok, RenewedSession}.

is_network_allowed(IP, AllowedNetworks) ->
    %% Check if IP is in allowed networks
    lists:any(fun(Network) -> is_in_network(IP, Network) end, AllowedNetworks).

is_in_network(IP, Network) ->
    %% Implement network checking
    true.

load_zero_trust_config() ->
    %% Load zero-trust configuration
    case file:read_file("config/zero_trust.json") of
        {ok, Data} ->
            jsone:decode(Data);
        _ ->
            %% Default configuration
            #{
                identity_provider => "ldap",
                mfa_enabled => true,
                jwt_issuer => "erlmcp-zero-trust",
                jwt_audience => "erlmcp",
                jwt_expiration => 3600,
                max_session_duration => 3600,
                just_in_time_ttl => 1800,
                privilege_escalation_timeout => 300,
                device_trust_score_threshold => 0.7,
                micro_segments => #{},
                network_isolation => true,
                allowed_networks => [],
                blocked_ports => [],
                encryption_algorithm => "AES-256-GCM",
                key_rotation_interval => 86400,
                data_classification_levels => [public, internal, confidential, secret],
                threat_detection => true,
                anomaly_threshold => 0.8,
                compliance_checks => [soc2, hipaa, gdpr],
                audit_log_retention => 2592000
            }
    end.

load_security_policies() ->
    %% Load security policies from storage
    case file:read_file("config/security_policies.json") of
        {ok, Data} ->
            jsone:decode(Data);
        _ ->
            %% Default policies
            []
    end.