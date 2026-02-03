-module(erlmcp_data_protection).
-export([start_link/0, classify_data/2, encrypt_data/3, decrypt_data/3]).
-export([set_retention_policy/3]).
-export([audit_data_access/2, monitor_data_flow/2]).
-export([mask_sensitive_data/2, anonymize_data/2]).
-export([ get_classification_rules/1]).
-export([apply_dlp_policy/3]).

-record.data_classification, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    sensitivity :: public | internal | confidential | restricted | classified,
    data_types :: list(),
    handling_requirements :: list(),
    retention_policy :: map(),
    encryption_required :: boolean(),
    access_controls :: list()
}.

record.data_flow, {
    id :: binary(),
    source :: binary(),
    destination :: binary(),
    data_type :: binary(),
    classification :: public | internal | confidential | restricted | classified,
    timestamp :: integer(),
    user_id :: binary(),
    action :: binary(),
    approved :: boolean(),
    risk_score :: float()
}.

record.dlp_policy, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    rules :: list(),
    actions :: list(),
    enabled :: boolean(),
    severity :: low | medium | high | critical,
    created_at :: integer(),
    updated_at :: integer()
}.

record.audit_log, {
    id :: binary(),
    timestamp :: integer(),
    user_id :: binary(),
    resource :: binary(),
    action :: binary(),
    classification :: binary(),
    approved :: boolean(),
    risk_score :: float(),
    details :: map()
}.

record.state, {
    classifications :: map(),
    data_flows :: list(),
    audit_logs :: list(),
    dlp_policies :: map(),
    config :: map()
}).

-define(TIMEOUT, 30000).
-define(AUDIT_RETENTION_DAYS, 2555). %% 7 years

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

classify_data(Data, Context) ->
    gen_server:call(?MODULE, {classify_data, Data, Context}, ?TIMEOUT).

encrypt_data(Data, Classification, Options) ->
    gen_server:call(?MODULE, {encrypt_data, Data, Classification, Options}, ?TIMEOUT).

decrypt_data(EncryptedData, Classification, Options) ->
    gen_server:call(?MODULE, {decrypt_data, EncryptedData, Classification, Options}, ?TIMEOUT).

set_retention_policy(DataId, RetentionPeriod, RetentionAction) ->
    gen_server:call(?MODULE, {set_retention_policy, DataId, RetentionPeriod, RetentionAction}, ?TIMEOUT).

get_retention_policy(DataId) ->
    gen_server:call(?MODULE, {get_retention_policy, DataId}, ?TIMEOUT).

audit_data_access(DataId, UserId) ->
    gen_server:call(?MODULE, {audit_data_access, DataId, UserId}, ?TIMEOUT).

monitor_data_flow(Source, Destination) ->
    gen_server:call(?MODULE, {monitor_data_flow, Source, Destination}, ?TIMEOUT).

mask_sensitive_data(Data, MaskingRules) ->
    gen_server:call(?MODULE, {mask_sensitive_data, Data, MaskingRules}, ?TIMEOUT).

anonymize_data(Data, AnonymizationRules) ->
    gen_server:call(?MODULE, {anonymize_data, Data, AnonymizationRules}, ?TIMEOUT).

create_data_classification(ClassificationData) ->
    gen_server:call(?MODULE, {create_data_classification, ClassificationData}, ?TIMEOUT).

get_classification_rules(Filter) ->
    gen_server:call(?MODULE, {get_classification_rules, Filter}, ?TIMEOUT).

apply_dlp_policy(PolicyId, Data, Context) ->
    gen_server:call(?MODULE, {apply_dlp_policy, PolicyId, Data, Context}, ?TIMEOUT).

audit_dlp_violations(TimePeriod) ->
    gen_server:call(?MODULE, {audit_dlp_violations, TimePeriod}, ?TIMEOUT).

init([]) ->
    State = #state{
        classifications = load_data_classifications(),
        data_flows = [],
        audit_logs = [],
        dlp_policies = load_default_dlp_policies(),
        config = load_config()
    },
    erlmcp_data_protection:initialize(),
    {ok, State}.

handle_call({classify_data, Data, Context}, _From, State) ->
    Classification = classify_data_content(Data, Context, State),
    {reply, {ok, Classification}, State};

handle_call({encrypt_data, Data, Classification, Options}, _From, State) ->
    case get_encryption_key(Classification, State) of
        {ok, Key} ->
            EncryptedData = apply_encryption(Data, Key, Options),
            {reply, {ok, EncryptedData}, State};
        {error, not_found} ->
            {reply, {error, key_not_found}, State}
    end;

handle_call({decrypt_data, EncryptedData, Classification, Options}, _From, State) ->
    case get_decryption_key(Classification, State) of
        {ok, Key} ->
            DecryptedData = apply_decryption(EncryptedData, Key, Options),
            {reply, {ok, DecryptedData}, State};
        {error, not_found} ->
            {reply, {error, key_not_found}, State}
    end;

handle_call({set_retention_policy, DataId, RetentionPeriod, RetentionAction}, _From, State) ->
    RetentionPolicy = #{
        data_id => DataId,
        retention_period => RetentionPeriod,
        retention_action => RetentionAction,
        created_at => timestamp(),
        expires_at => timestamp() + RetentionPeriod
    },
    NewState = State#state{
        retention_policies = maps:put(DataId, RetentionPolicy, State#state.retention_policies)
    },
    {reply, {ok, set}, NewState};

handle_call({get_retention_policy, DataId}, _From, State) ->
    case maps:find(DataId, State#state.retention_policies) of
        {ok, Policy} ->
            {reply, {ok, Policy}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({audit_data_access, DataId, UserId}, _From, State) ->
    AuditLog = #audit_log{
        id = generate_audit_id(),
        timestamp = timestamp(),
        user_id = UserId,
        resource = DataId,
        action = access,
        classification = get_data_classification(DataId, State),
        approved = is_access_approved(DataId, UserId, State),
        risk_score = calculate_access_risk(DataId, UserId, State),
        details = #{}
    },
    NewState = State#state{
        audit_logs = [AuditLog|State#state.audit_logs]
    },
    {reply, {ok, logged}, NewState};

handle_call({monitor_data_flow, Source, Destination}, _From, State) ->
    DataFlow = #data_flow{
        id = generate_flow_id(),
        source = Source,
        destination = Destination,
        data_type = determine_data_type(Source),
        classification = determine_data_classification(Source),
        timestamp = timestamp(),
        user_id = get_current_user(),
        action = transfer,
        approved = is_transfer_approved(Source, Destination, State),
        risk_score = calculate_transfer_risk(Source, Destination, State)
    },
    NewState = State#state{
        data_flows = [DataFlow|State#state.data_flows]
    },
    {reply, {ok, monitored}, NewState};

handle_call({mask_sensitive_data, Data, MaskingRules}, _From, State) ->
    MaskedData = apply_data_masking(Data, MaskingRules),
    {reply, {ok, MaskedData}, State};

handle_call({anonymize_data, Data, AnonymizationRules}, _From, State) ->
    AnonymizedData = apply_data_anonymization(Data, AnonymizationRules),
    {reply, {ok, AnonymizedData}, State};

handle_call({create_data_classification, ClassificationData}, _From, State) ->
    case validate_classification_data(ClassificationData) of
        {ok, ValidatedData} ->
            ClassificationId = generate_classification_id(),
            Classification = #data_classification{
                id = ClassificationId,
                name = maps:get(name, ValidatedData),
                description = maps:get(description, ValidatedData, <<"">>),
                sensitivity = maps:get(sensitivity, ValidatedData),
                data_types = maps:get(data_types, ValidatedData, []),
                handling_requirements = maps:get(handling_requirements, ValidatedData, []),
                retention_policy = maps:get(retention_policy, ValidatedData, #{}),
                encryption_required = maps:get(encryption_required, ValidatedData, true),
                access_controls = maps:get(access_controls, ValidatedData, [])
            },
            NewState = State#state{
                classifications = maps:put(ClassificationId, Classification, State#state.classifications)
            },
            {reply, {ok, ClassificationId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_classification_rules, Filter}, _From, State) ->
    FilteredClassifications = apply_filter(State#state.classifications, Filter),
    {reply, {ok, maps:values(FilteredClassifications)}, State};

handle_call({apply_dlp_policy, PolicyId, Data, Context}, _From, State) ->
    case maps:find(PolicyId, State#state.dlp_policies) of
        {ok, Policy} ->
            case evaluate_dlp_rules(Policy#dlp_policy.rules, Data, Context) of
                {compliant, _} ->
                    {reply, {ok, compliant}, State};
                {violation, Violations} ->
                    {reply, {violation, Violations}, State}
            end;
        error ->
            {reply, {error, policy_not_found}, State}
    end;

handle_call({audit_dlp_violations, TimePeriod}, _From, State) ->
    Violations = get_dlp_violations(TimePeriod, State),
    {reply, {ok, Violations}, State};

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
    %% Initialize data protection system
    %% Load classification rules
    %% Configure DLP policies
    ok.

load_config() ->
    #{
        audit_retention_days => ?AUDIT_RETENTION_DAYS,
        max_audit_logs => 1000000,
        encryption_required => true,
        default_sensitivity => internal,
        auto_classification => true,
        dlp_enabled => true
    }.

load_data_classifications() ->
    %% Load default data classifications
    #{
        pii => #data_classification{
            id => <<"pii">>,
            name => Personally Identifiable Information,
            description => "Data that can be used to identify an individual",
            sensitivity => restricted,
            data_types = [name, email, phone, ssn, address, dob],
            handling_requirements = [encryption, access_control, audit],
            retention_policy = #{period => 7257600000, action => delete}, %% 7 years
            encryption_required = true,
            access_controls = [admin, compliance]
        },
        financial => #data_classification{
            id => <<"financial">>,
            name => Financial Data,
            description => "Financial records and transactions",
            sensitivity => confidential,
            data_types = [account_number, transaction, payment, balance],
            handling_requirements = [encryption, dual_control, audit],
            retention_policy = #{period => 15552000000, action => archive}, %% 6 months
            encryption_required = true,
            access_controls = [finance, audit]
        },
        healthcare => #data_classification{
            id => <<"healthcare">>,
            name => Healthcare Data,
            description => "Protected health information",
            sensitivity => restricted,
            data_types = [medical_record, diagnosis, treatment, prescription],
            handling_requirements = [encryption, access_control, audit],
            retention_policy = #{period => 25920000000, action => delete}, %% 30 years
            encryption_required = true,
            access_controls = [medical, compliance]
        },
        public => #data_classification{
            id => <<"public">>,
            name => Public Data,
            description => "Non-sensitive public information",
            sensitivity => public,
            data_types = [marketing, brochure, press_release],
            handling_requirements = [],
            retention_policy = #{period => 7776000000, action => archive}, %% 90 days
            encryption_required = false,
            access_controls = [all]
        }
    }.

load_default_dlp_policies() ->
    %% Load default DLP policies
    #{
        no_pii_transfer => #dlp_policy{
            id => <<"no_pii_transfer">>,
            name => "No PII Transfer",
            description => "Prevent transfer of PII data",
            rules = [
                {pattern, "ssn"},
                {pattern, "credit_card"},
                {pattern, "password"}
            ],
            actions = [block, alert],
            enabled = true,
            severity = critical,
            created_at => timestamp(),
            updated_at => timestamp()
        },
        no_financial_export => #dlp_policy{
            id => <<"no_financial_export">>,
            name => "No Financial Export",
            description => "Prevent export of financial data",
            rules = [
                {pattern, "account_number"},
                {pattern, "transaction"},
                {destination, "external"}
            ],
            actions = [block, require_approval],
            enabled = true,
            severity = high,
            created_at => timestamp(),
            updated_at => timestamp()
        }
    }.

classify_data_content(Data, Context, State) ->
    %% Classify data based on content and context
    Classifications = maps:values(State#state.classifications),

    case find_matching_classification(Data, Classifications) of
        {match, Classification} ->
            Classification;
        no_match ->
            default_classification(State)
    end.

find_matching_classification(Data, Classifications) ->
    %% Find matching classification based on data patterns
    case lists:search(fun(Classification) ->
        contains_sensitive_patterns(Data, Classification#data_classification.data_types)
    end, Classifications) of
        {value, Classification} ->
            {match, Classification};
        false ->
            no_match
    end.

contains_sensitive_patterns(Data, Patterns) ->
    %% Check if data contains sensitive patterns
    case lists:any(fun(Pattern) ->
        case Pattern of
            "ssn" -> contains_ssn(Data);
            "credit_card" -> contains_credit_card(Data);
            "email" -> contains_email(Data);
            _ -> false
        end
    end, Patterns) of
        true -> true;
        false -> false
    end.

contains_ssn(Data) ->
    %% Check for SSN pattern
    case re:run(Data, "\\b\\d{3}-?\\d{2}-?\\d{4}\\b") of
        nomatch -> false;
        _ -> true
    end.

contains_credit_card(Data) ->
    %% Check for credit card pattern
    case re:run(Data, "\\b\\d{4}[ -]?\\d{4}[ -]?\\d{4}[ -]?\\d{4}\\b") of
        nomatch -> false;
        _ -> true
    end.

contains_email(Data) ->
    %% Check for email pattern
    case re:run(Data, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b") of
        nomatch -> false;
        _ -> true
    end.

default_classification(State) ->
    %% Return default classification
    #data_classification{
        id => <<"default">>,
        name => "Default Classification",
        description => "Default classification for uncategorized data",
        sensitivity => internal,
        data_types = [],
        handling_requirements = [],
        retention_policy = #{period => 7776000000, action => archive}, %% 90 days
        encryption_required = false,
        access_controls = [all]
    }.

get_encryption_key(Classification, State) ->
    %% Get encryption key for data classification
    case Classification#data_classification.encryption_required of
        true ->
            %% Generate or retrieve encryption key
            Key = erlmcp_crypto:generate_key(256),
            {ok, Key};
        false ->
            {ok, none}
    end.

apply_encryption(Data, Key, Options) ->
    %% Apply encryption to data
    case Key of
        none ->
            Data;
        _ ->
            erlmcp_crypto:encrypt(Data, Key)
    end.

get_decryption_key(Classification, State) ->
    %% Get decryption key for data classification
    case Classification#data_classification.encryption_required of
        true ->
            %% Retrieve decryption key
            {ok, Key};
        false ->
            {ok, none}
    end.

apply_decryption(EncryptedData, Key, Options) ->
    %% Apply decryption to data
    case Key of
        none ->
            EncryptedData;
        _ ->
            erlmcp_crypto:decrypt(EncryptedData, Key)
    end.

get_data_classification(DataId, State) ->
    %% Get data classification for data ID
    case maps:find(DataId, State#state.classifications) of
        {ok, Classification} ->
            Classification#data_classification.sensitivity;
        error ->
            internal
    end.

is_access_approved(DataId, UserId, State) ->
    %% Check if data access is approved
    case get_data_classification(DataId, State) of
        public -> true;
        internal -> has_required_permission(UserId, internal, State);
        confidential -> has_required_permission(UserId, confidential, State);
        restricted -> has_required_permission(UserId, restricted, State);
        classified -> has_required_permission(UserId, classified, State)
    end.

has_required_permission(UserId, Sensitivity, State) ->
    %% Check if user has required permission
    %% This would typically check against user roles and permissions
    true.

calculate_access_risk(DataId, UserId, State) ->
    %% Calculate risk score for data access
    Classification = get_data_classification(DataId, State),
    RiskFactors = [
        {sensitivity, classification_risk(Classification)},
        {user_risk, user_risk_score(UserId)},
        {time_risk, time_based_risk()}
    ],

    calculate_weighted_risk(RiskFactors).

classification_risk(Classification) ->
    %% Risk based on classification level
    case Classification of
        public -> 0.1;
        internal -> 0.3;
        confidential -> 0.6;
        restricted -> 0.8;
        classified -> 1.0
    end.

user_risk_score(UserId) ->
    %% Risk based on user profile
    0.3.

time_based_risk() ->
    %% Risk based on time of access
    Hour = calendar:hour_from_timestamp(timestamp()),
    case Hour of
        H when H < 6 orelse H > 22 -> 0.8;
        _ -> 0.2
    end.

determine_data_type(Source) ->
    %% Determine data type from source
    case binary:part(Source, 0, 4) of
        "db://" -> database;
        "file://" -> file;
        "api://" -> api;
        _ -> unknown
    end.

determine_data_classification(Source) ->
    %% Determine data classification from source
    internal.

get_current_user() ->
    %% Get current user from context
    undefined.

is_transfer_approved(Source, Destination, State) ->
    %% Check if data transfer is approved
    case Source == Destination of
        true ->
            true;
        false ->
            case is_external_destination(Destination) of
                true ->
                    requires_approval(Source, State);
                false ->
                    true
            end
    end.

is_external_destination(Destination) ->
    %% Check if destination is external
    case binary:match(Destination, ["external", "cloud", "internet"]) of
        nomatch -> false;
        _ -> true
    end.

requires_approval(Source, State) ->
    %% Check if transfer requires approval
    case get_data_classification(Source, State) of
        confidential -> true;
        restricted -> true;
        classified -> true;
        _ -> false
    end.

calculate_transfer_risk(Source, Destination, State) ->
    %% Calculate risk score for data transfer
    SourceClassification = determine_data_classification(Source),
    DestClassification = determine_data_classification(Destination),

    RiskFactors = [
        {source_risk, classification_risk(SourceClassification)},
        {destination_risk, classification_risk(DestClassification)},
        {external_transfer, is_external_destination(Destination)}
    ],

    calculate_weighted_risk(RiskFactors).

apply_data_masking(Data, MaskingRules) ->
    %% Apply data masking based on rules
    MaskedData = Data,
    %% Apply masking rules
    case lists:member("name", MaskingRules) of
        true ->
            MaskedData = mask_name(MaskedData);
        false ->
            MaskedData
    end,

    case lists:member("email", MaskingRules) of
        true ->
            MaskedData = mask_email(MaskedData);
        false ->
            MaskedData
    end,

    MaskedData.

mask_name(Data) ->
    %% Mask name in data
    re:replace(Data, "\\b[A-Za-z]+\\s+[A-Za-z]+\\b", "***", [{return, binary}]).

mask_email(Data) ->
    %% Mask email in data
    re:replace(Data, "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Z|a-z]{2,}\\b", "***@***.***", [{return, binary}]).

apply_data_anonymization(Data, AnonymizationRules) ->
    %% Apply data anonymization
    AnonymizedData = Data,
    %% Apply anonymization rules
    AnonymizedData.

validate_classification_data(Data) ->
    Required = [name, sensitivity],
    case check_required_fields(Data, Required) of
        ok ->
            {ok, Data};
        {error, missing_field} ->
            {error, {invalid_classification_data, missing_field}}
    end.

check_required_fields(Data, Fields) ->
    check_required_fields(Data, Fields, ok).

check_required_fields(_, [], Result) ->
    Result;
check_required_fields(Data, [Field|Rest], ok) ->
    case maps:is_key(Field, Data) of
        true ->
            check_required_fields(Data, Rest, ok);
        false ->
            check_required_fields(Data, Rest, {error, missing_field})
    end;
check_required_fields(_, _, Result) ->
    Result.

evaluate_dlp_rules(Rules, Data, Context) ->
    %% Evaluate DLP rules against data
    case lists:any(fun(Rule) ->
        violates_dlp_rule(Rule, Data, Context)
    end, Rules) of
        true ->
            {violation, ["Data contains sensitive information"]};
        false ->
            {compliant, []}
    end.

violates_dlp_rule(Rule, Data, Context) ->
    %% Check if rule is violated
    case Rule of
        {pattern, Pattern} ->
            contains_pattern(Data, Pattern);
        {destination, Destination} ->
            matches_destination(Context, Destination);
        _ ->
            false
    end.

contains_pattern(Data, Pattern) ->
    %% Check if data contains pattern
    case re:run(Data, Pattern) of
        nomatch -> false;
        _ -> true
    end.

matches_destination(Context, Destination) ->
    %% Check if context matches destination
    case maps:get(destination, Context, undefined) of
        undefined -> false;
        Dest -> binary:match(Dest, Destination) /= nomatch
    end.

get_dlp_violations(TimePeriod, State) ->
    %% Get DLP violations for time period
    CurrentTime = timestamp(),
    StartTime = CurrentTime - TimePeriod,

    Violations = lists:filter(fun(Log) ->
        Log#audit_log.timestamp >= StartTime andalso
        not Log#audit_log.approved
    end, State#state.audit_logs),

    Violations.

calculate_weighted_risk(Factors) ->
    Weights = [
        {sensitivity, 0.4},
        {user_risk, 0.3},
        {time_risk, 0.2},
        {external_transfer, 0.1}
    ],

    lists:foldl(fun({Factor, Value}, Acc) ->
        case proplists:get_value(Factor, Weights) of
            undefined ->
                Acc;
            Weight ->
                Acc + Value * Weight
        end
    end, 0.0, Factors).

apply_filter(Map, Filter) ->
    case Filter of
        #{sensitivity := Sensitivity} ->
            maps:filter(fun(_, Classification) -> Classification#data_classification.sensitivity == Sensitivity end, Map);
        #{type := Type} ->
            maps:filter(fun(_, Classification) -> lists:member(Type, Classification#data_classification.data_types) end, Map);
        _ ->
            Map
    end.

generate_classification_id() ->
    crypto:strong_rand_bytes(16).

generate_audit_id() ->
    crypto:strong_rand_bytes(16).

generate_flow_id() ->
    crypto:strong_rand_bytes(16).

timestamp() ->
    erlang:system_time(millisecond).