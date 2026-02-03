%% @doc erlmcp Compliance Policy Manager
%% Manages compliance policies across all frameworks (SOC2, HIPAA, GDPR, ISO27001)
%% Provides policy storage, versioning, enforcement, and updates
-module(erlmcp_compliance_policy_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, get_policy/2, get_policies_by_framework/1,
         create_policy/4, update_policy/3, delete_policy/2,
         check_compliance/1, evaluate_controls/2, get_policy_version/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(policy, {
    id :: binary(),
    name :: binary(),
    framework :: soc2 | hipaa | gdpr | iso27001 | industry,
    category :: binary(),
    controls :: list(),
    requirements :: list(),
    implemented :: boolean(),
    effectiveness :: float(),
    last_updated :: erlang:timestamp(),
    version :: integer(),
    owner :: binary(),
    reviewers :: list()
}).

-record(state, {
    policies :: #{binary() => #policy{}},
    frameworks :: #{atom() => list()},
    active_reviews :: list(),
    policy_history :: list(),
    compliance_thresholds :: #{atom() => float()}
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

get_policy(Framework, PolicyId) ->
    gen_server:call(?SERVER, {get_policy, Framework, PolicyId}).

get_policies_by_framework(Framework) ->
    gen_server:call(?SERVER, {get_policies_by_framework, Framework}).

create_policy(Framework, Name, Category, Controls) when is_binary(Name), is_binary(Category) ->
    gen_server:call(?SERVER, {create_policy, Framework, Name, Category, Controls}).

update_policy(Framework, PolicyId, Updates) ->
    gen_server:call(?SERVER, {update_policy, Framework, PolicyId, Updates}).

delete_policy(Framework, PolicyId) ->
    gen_server:call(?SERVER, {delete_policy, Framework, PolicyId}).

check_compliance(Framework) ->
    gen_server:call(?SERVER, {check_compliance, Framework}).

evaluate_controls(Framework, Controls) ->
    gen_server:call(?SERVER, {evaluate_controls, Framework, Controls}).

get_policy_version(Framework, PolicyId, Version) ->
    gen_server:call(?SERVER, {get_policy_version, Framework, PolicyId, Version}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Policies0 = init_default_policies(),
    Frameworks0 = init_frameworks(),
    State0 = #state{
        policies = Policies0,
        frameworks = Frameworks0,
        active_reviews = [],
        policy_history = [],
        compliance_thresholds = init_compliance_thresholds()
    },
    erlmcp_telemetry:counter("compliance.policy_manager.initialized", 1,
                           #{component => "compliance"}),
    {ok, State0}.

handle_call({get_policy, Framework, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.policies, undefined) of
        undefined ->
            {reply, {error, policy_not_found}, State};
        Policy ->
            case Policy#policy.framework of
                Framework ->
                    {reply, {ok, Policy}, State};
                _ ->
                    {reply, {error, framework_mismatch}, State}
            end
    end;

handle_call({get_policies_by_framework, Framework}, _From, State) ->
    Policies = lists:filtermap(
        fun({_Id, Policy}) ->
            case Policy#policy.framework of
                Framework -> {true, Policy};
                _ -> false
            end
        end, maps:to_list(State#state.policies)),
    {reply, {ok, Policies}, State};

handle_call({create_policy, Framework, Name, Category, Controls}, _From, State) ->
    PolicyId = generate_policy_id(),
    Now = erlang:timestamp(),
    Policy = #policy{
        id = PolicyId,
        name = Name,
        framework = Framework,
        category = Category,
        controls = Controls,
        requirements = extract_requirements(Framework, Controls),
        implemented = false,
        effectiveness = 0.0,
        last_updated = Now,
        version = 1,
        owner = <<"system">>,
        reviewers = []
    },

    Policies1 = maps:put(PolicyId, Policy, State#state.policies),
    PolicyHistory1 = [{created, Now, PolicyId, Policy} | State#state.policy_history],

    erlmcp_telemetry:counter("compliance.policy.created", 1,
                           #{framework => atom_to_list(Framework)}),

    {reply, {ok, PolicyId}, State#state{policies = Policies1, policy_history = PolicyHistory1}};

handle_call({update_policy, Framework, PolicyId, Updates}, _From, State) ->
    case maps:get(PolicyId, State#state.policies, undefined) of
        undefined ->
            {reply, {error, policy_not_found}, State};
        Policy when Policy#policy.framework =:= Framework ->
            Policy1 = update_policy_fields(Policy, Updates),
            Policies1 = maps:put(PolicyId, Policy1, State#state.policies),
            PolicyHistory1 = [{updated, erlang:timestamp(), PolicyId, Policy1} | State#state.policy_history],

            erlmcp_telemetry:counter("compliance.policy.updated", 1,
                                   #{framework => atom_to_list(Framework)}),

            {reply, {ok, Policy1}, State#state{policies = Policies1, policy_history = PolicyHistory1}};
        _ ->
            {reply, {error, framework_mismatch}, State}
    end;

handle_call({delete_policy, Framework, PolicyId}, _From, State) ->
    case maps:get(PolicyId, State#state.policies, undefined) of
        undefined ->
            {reply, {error, policy_not_found}, State};
        Policy when Policy#policy.framework =:= Framework ->
            Policies1 = maps:remove(PolicyId, State#state.policies),
            PolicyHistory1 = [{deleted, erlang:timestamp(), PolicyId, Policy} | State#state.policy_history],

            erlmcp_telemetry:counter("compliance.policy.deleted", 1,
                                   #{framework => atom_to_list(Framework)}),

            {reply, {ok, deleted}, State#state{policies = Policies1, policy_history = PolicyHistory1}};
        _ ->
            {reply, {error, framework_mismatch}, State}
    end;

handle_call({check_compliance, Framework}, _From, State) ->
    Policies = get_policies_by_framework(Framework, State),
    Implemented = [P || P <- Policies, P#policy.implemented],
    Total = length(Policies),

    if Total > 0 ->
        ImplementationRate = length(Implemented) / Total,
        ComplianceScore = calculate_compliance_score(Framework, Implemented, State),
        Result = #{
            framework => Framework,
            total_policies => Total,
            implemented_policies => length(Implemented),
            implementation_rate => ImplementationRate,
            compliance_score => ComplianceScore,
            threshold => maps:get(Framework, State#state.compliance_thresholds, 0.8),
            compliant => ComplianceScore >= maps:get(Framework, State#state.compliance_thresholds, 0.8)
        };
       true ->
        Result = #{
            framework => Framework,
            total_policies => 0,
            implemented_policies => 0,
            implementation_rate => 0.0,
            compliance_score => 0.0,
            threshold => maps:get(Framework, State#state.compliance_thresholds, 0.8),
            compliant => false
        }
    end,

    {reply, {ok, Result}, State};

handle_call({evaluate_controls, Framework, Controls}, _From, State) ->
    Evaluation = evaluate_controls_against_framework(Framework, Controls),
    {reply, {ok, Evaluation}, State};

handle_call({get_policy_version, Framework, PolicyId, Version}, _From, State) ->
    case find_policy_version(PolicyId, Version, State) of
        {ok, PolicyVersion} ->
            {reply, {ok, PolicyVersion}, State};
        not_found ->
            {reply, {error, version_not_found}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_default_policies() ->
    #{
        %% SOC2 Policies
        <<"soc2-cc6-1">> => #policy{
            id = <<"soc2-cc6-1">>,
            name = "Logical and Physical Access Controls",
            framework = soc2,
            category = "Access Control",
            controls = ["MFA", "RBAC", "Session Timeout", "Access Revocation"],
            requirements = ["Access control policies", "Identity management", "Access review process"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"compliance_officer">>,
            reviewers = [<<"auditor">>]
        },
        <<"soc2-cc6-6">> => #policy{
            id = <<"soc2-cc6-6">>,
            name = "Security Event Logging",
            framework = soc2,
            category = "Logging",
            controls = ["Log Collection", "Log Retention", "Log Monitoring", "Alerting"],
            requirements = ["Event logging policies", "Log retention periods", "Review procedures"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"security_engineer">>,
            reviewers = [<<"auditor">>]
        },

        %% HIPAA Policies
        <<"hipaa-164-306">> => #policy{
            id = <<"hipaa-164-306">>,
            name = "Physical Safeguards",
            framework = hipaa,
            category = "Physical Security",
            controls = ["Access Control", "Workstation Security", "Device Disposal", "Media Reuse"],
            requirements = ["Physical access policies", "Workstation security", "Device management"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"hipaa_officer">>,
            reviewers = [<<"auditor">>]
        },
        <<"hipaa-164-312">> => #policy{
            id = <<"hipaa-164-312">>,
            name = "Technical Safeguards",
            framework = hipaa,
            category = "Technical Security",
            controls = ["Access Control", "Audit Controls", "Integrity Controls", "Transmission Security"],
            requirements = ["Technical security measures", "Audit trails", "Data integrity"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"security_engineer">>,
            reviewers = [<<"auditor">>]
        },

        %% GDPR Policies
        <<"gdpr-art-5">> => #policy{
            id = <<"gdpr-art-5">>,
            name = "Lawful, Fair and Transparent Processing",
            framework = gdpr,
            category = "Data Processing",
            controls = ["Lawful Basis", "Purpose Limitation", "Data Minimization", "Transparency"],
            requirements = ["Processing documentation", "Consent management", "Privacy notices"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"dpo">>,
            reviewers = [<<"auditor">>]
        },
        <<"gdpr-art-16">> => #policy{
            id = <<"gdpr-art-16">>,
            name = "Right to Erasure",
            framework = gdpr,
            category = "Data Subject Rights",
            controls = ["Data Deletion", "Backup Cleanup", "Third-Party Coordination", "Verification"],
            requirements = ["Data deletion procedures", "Backup retention", "Erasure verification"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"dpo">>,
            reviewers = [<<"auditor">>]
        },

        %% ISO27001 Policies
        <<"iso-27001-A.9">> => #policy{
            id = <<"iso-27001-A.9">>,
            name = "Access Control",
            framework = iso27001,
            category = "Information Access Control",
            controls = ["Access Policy", "User Access Management", "Privilege Management", "Access Control"],
            requirements = ["Access control procedures", "Identity lifecycle", "Privilege review"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"iso_manager">>,
            reviewers = [<<"auditor">>]
        },
        <<"iso-27001-A.12">> => #policy{
            id = <<"iso-27001-A.12">>,
            name = "Information Systems Acquisition, Development and Maintenance",
            framework = iso27001,
            category = "System Development",
            controls = ["Secure Development", "Change Management", "Segregation of Duties"],
            requirements = ["SDLC security", "Change control", "Separation of duties"],
            implemented = false,
            effectiveness = 0.0,
            last_updated = erlang:timestamp(),
            version = 1,
            owner = <<"dev_security">>,
            reviewers = [<<"auditor">>]
        }
    }.

init_frameworks() ->
    #{
        soc2 => [
            {"Security Controls", ["CC6.1", "CC6.2", "CC6.3", "CC6.4", "CC6.5", "CC6.6"]},
            {"Availability Controls", ["CC1.1", "CC1.2", "CC1.3", "CC1.4", "CC1.5", "CC1.6"]},
            {"Processing Integrity", ["CC7.1", "CC7.2", "CC7.3", "CC7.4", "CC7.5"]},
            {"Confidentiality Controls", ["CC2.1", "CC2.2", "CC2.3", "CC2.4", "CC2.5", "CC2.6"]},
            {"Privacy Controls", ["CC14.1", "CC14.2", "CC14.3", "CC14.4", "CC14.5", "CC14.6"]}
        ],
        hipaa => [
            {"Administrative Safeguards", ["164.306", "164.308"]},
            {"Physical Safeguards", ["164.310"]},
            {"Technical Safeguards", ["164.312", "164.314", "164.316", "164.318"]},
            {"Organizational Requirements", ["164.502", "164.504"]},
            {"Policies and Procedures", ["164.316", "164.520"]}
        ],
        gdpr => [
            {"Data Processing", ["Art. 5", "Art. 6", "Art. 7", "Art. 8", "Art. 9"]},
            {"Data Subject Rights", ["Art. 12", "Art. 15", "Art. 16", "Art. 17", "Art. 18", "Art. 20"]},
            {"Data Protection by Design", ["Art. 25", "Art. 32"]},
            {"Data Breach", ["Art. 33", "Art. 34", "Art. 33"]},
            {"International Transfers", ["Art. 44", "Art. 45", "Art. 46", "Art. 47", "Art. 48", "Art. 49"]}
        ],
        iso27001 => [
            {"Information Security Management System", ["4", "5", "6", "7", "8", "9"]},
            {"Information Security Risk Assessment", ["6.1", "6.1.3", "6.1.4", "6.2", "6.3", "A.5"]},
            {"Information Security Risk Treatment", {"6.1.2", "6.2", "A.6", "A.18"}},
            {"Human Resource Security", {"7", "A.7"}},
            {"Asset Management", {"8", "A.8"}},
            {"Access Control", {"9", "A.9"}},
            {"Cryptography", {"10", "A.10"}},
            {"Physical and Environmental Security", {"11", "A.11"}},
            {"Operations Security", {"12", "A.12"}},
            {"Supplier Relationships", {"13", "A.13"}},
            {"Incident Management", {"16", "A.16"}},
            {"Business Continuity", {"17", "A.17"}},
            {"Compliance", {"18", "A.18"}}
        ]
    }.

init_compliance_thresholds() ->
    #{
        soc2 => 0.75,   % 75% policy implementation
        hipaa => 0.60,  % 60% safeguard implementation
        gdpr => 0.80,   % 80% compliance requirements
        iso27001 => 0.70  % 70% ISO controls
    }.

generate_policy_id() ->
    iolist_to_binary(io_lib:format("pol-~s-~s-~w", [
        integer_to_list(erlang:system_time(second)),
        crypto:strong_rand_bytes(8) |> binary:encode_hex(),
        erlang:unique_integer()
    ])).

extract_requirements(Framework, Controls) ->
    case Framework of
        soc2 ->
            ["SOC2 " ++ Control || Control <- Controls];
        hipaa ->
            ["HIPAA " ++ Control || Control <- Controls];
        gdpr ->
            ["GDPR " ++ Control || Control <- Controls];
        iso27001 ->
            ["ISO27001 " ++ Control || Control <- Controls];
        _ ->
            Controls
    end.

update_policy_fields(#policy{name = Name, framework = Framework, category = Category,
                             controls = Controls, requirements = Requirements} = Policy, Updates) ->
    UpdatedPolicy = Policy#policy{
        name = maps:get(name, Updates, Name),
        framework = maps:get(framework, Updates, Framework),
        category = maps:get(category, Updates, Category),
        controls = maps:get(controls, Updates, Controls),
        requirements = maps:get(requirements, Updates, Requirements),
        implemented = maps:get(implemented, Updates, Policy#policy.implemented),
        effectiveness = maps:get(effectiveness, Updates, Policy#policy.effectiveness),
        last_updated = erlang:timestamp(),
        version = Policy#policy.version + 1,
        owner = maps:get(owner, Updates, Policy#policy.owner),
        reviewers = maps:get(reviewers, Updates, Policy#policy.reviewers)
    },
    UpdatedPolicy.

get_policies_by_framework(Framework, State) ->
    maps:fold(
        fun(_Id, Policy, Acc) ->
            case Policy#policy.framework of
                Framework -> [Policy | Acc];
                _ -> Acc
            end
        end, [], State#state.policies).

calculate_compliance_score(Framework, Policies, State) ->
    case Framework of
        soc2 ->
            % Calculate based on control effectiveness
            Effectiveness = lists:sum([P#policy.effectiveness || P <- Policies]),
            case length(Policies) of
                0 -> 0.0;
                N -> min(Effectiveness / N, 1.0)
            end;
        hipaa ->
            % Calculate based on safeguards implemented
            Safeguards = lists:sum([P#policy.effectiveness || P <- Policies]),
            case length(Policies) of
                0 -> 0.0;
                N -> min(Safeguards / N, 1.0)
            end;
        gdpr ->
            % Calculate based on consent and data subject rights
            ConsentRate = lists:sum([P#policy.effectiveness || P <- Policies]) / length(Policies),
            min(ConsentRate, 1.0);
        iso27001 ->
            % Calculate based on ISO controls coverage
            Controls = lists:sum([P#policy.effectiveness || P <- Policies]),
            case length(Policies) of
                0 -> 0.0;
                N -> min(Controls / N, 1.0)
            end;
        _ ->
            0.0
    end.

evaluate_controls_against_framework(Framework, Controls) ->
    % This would contain the actual evaluation logic
    % For now, return a basic structure
    #{
        framework => Framework,
        controls => Controls,
        evaluated_at => erlang:timestamp(),
        assessment => manual_review_required,
        recommendations => ["Review controls against " ++ atom_to_list(Framework) ++ " requirements"]
    }.

find_policy_version(PolicyId, Version, State) ->
    % This would track policy versions in history
    case lists:keyfind(PolicyId, 3, State#state.policy_history) of
        {created, _, _, Policy} when Policy#policy.version == Version ->
            {ok, Policy};
        {updated, _, _, Policy} when Policy#policy.version == Version ->
            {ok, Policy};
        _ ->
            not_found
    end.