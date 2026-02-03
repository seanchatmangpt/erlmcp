%% @doc erlmcp Compliance Metrics Collection
%% Real-time compliance metrics collection and reporting
%% Supports SOC2, HIPAA, GDPR, ISO27001 metrics tracking
-module(erlmcp_compliance_metrics).

-behaviour(gen_server).

%% API
-export([start_link/0, increment/1, increment/2, get_metrics/1, get_all_metrics/0, reset/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    metrics :: #{atom() => non_neg_integer()},
    compliance_status :: #{atom() => boolean()},
    last_reset :: erlang:timestamp()
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

increment(Metric) ->
    increment(Metric, 1).

increment(Metric, Delta) when is_integer(Delta), Delta > 0 ->
    gen_server:cast(?SERVER, {increment, Metric, Delta}).

get_metrics(Metric) ->
    gen_server:call(?SERVER, {get_metrics, Metric}).

get_all_metrics() ->
    gen_server:call(?SERVER, get_all_metrics).

reset(Metric) ->
    gen_server:cast(?SERVER, {reset, Metric}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Metrics0 = init_metrics(),
    ComplianceStatus0 = init_compliance_status(),
    State = #state{
        metrics = Metrics0,
        compliance_status = ComplianceStatus0,
        last_reset = erlang:timestamp()
    },
    erlmcp_telemetry:counter(
        "compliance.metrics.initialized",
        1,
        #{component => "compliance"}
    ),
    {ok, State}.

handle_call({get_metrics, Metric}, _From, State) ->
    Value = maps:get(Metric, State#state.metrics, 0),
    {reply, {ok, Value}, State};

handle_call(get_all_metrics, _From, State) ->
    Metrics = #{
        metrics => State#state.metrics,
        compliance_status => State#state.compliance_status,
        last_reset => State#state.last_reset
    },
    {reply, {ok, Metrics}, State}.

handle_cast({increment, Metric, Delta}, State) ->
    Metrics1 = maps:update_with(Metric, fun(V) -> V + Delta end, Delta, State#state.metrics),
    ComplianceStatus1 = update_compliance_status(Metric, Metrics1, State#state.compliance_status),

    %% Track compliance metrics
    erlmcp_telemetry:counter(
        "compliance.metrics." ++ atom_to_list(Metric),
        Delta,
        #{component => "compliance"}
    ),

    {noreply, State#state{metrics = Metrics1, compliance_status = ComplianceStatus1}};

handle_cast({reset, Metric}, State) ->
    Metrics1 = maps:put(Metric, 0, State#state.metrics),
    {noreply, State#state{metrics = Metrics1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

init_metrics() ->
    #{
        %% SOC2 Metrics
        soc2_security_controls => 0,
        soc2_availability_controls => 0,
        soc2_processing_integrity => 0,
        soc2_confidentiality_controls => 0,
        soc2_privacy_controls => 0,
        soc2_audits_completed => 0,

        %% HIPAA Metrics
        hipaa_safeguards => 0,
        hipaa_privacy_rules => 0,
        hipaa_breach_notifications => 0,
        hipaa_audit_log_events => 0,
        hipaa_encrypted_records => 0,

        %% GDPR Metrics
        gdpr_data_subject_requests => 0,
        gdpr_data_access_requests => 0,
        gdpr_data_erasure_requests => 0,
        gdpr_data_portability_requests => 0,
        gdpr_consent_management => 0,
        gdpr_dpo_complaints => 0,

        %% ISO27001 Metrics
        iso27001_controls => 0,
        iso27001_risk_assessments => 0,
        iso27001_incident_responses => 0,
        iso27001_management_reviews => 0,

        %% General Compliance Metrics
        audit_events => 0,
        policy_violations => 0,
        remediation_tasks => 0,
        compliance_checks => 0,
        control_failures => 0
    }.

init_compliance_status() ->
    #{
        soc2_compliant => false,
        hipaa_compliant => false,
        gdpr_compliant => false,
        iso27001_compliant => false,
        overall_compliance => false,
        last_audit_date => null
    }.

update_compliance_status(Metric, Metrics, Status) ->
    %% Update SOC2 status
    Soc2Controls = get_soc2_control_count(Metrics),
    Soc2Compliant = Soc2Controls >= 75,  % 75% control implementation

    %% Update HIPAA status
    HipaaSafeguards = maps:get(hipaa_safeguards, Metrics, 0),
    HipaaCompliant = HipaaSafeguards >= 60,  % 60% safeguard implementation

    %% Update GDPR status
    GdprCompliance =
        case maps:get(gdpr_consent_management, Metrics, 0) of
            Count when Count >= 50 -> true;  % 50% consent management
            _ -> false
        end,

    %% Update ISO27001 status
    IsoControls = maps:get(iso27001_controls, Metrics, 0),
    IsoCompliant = IsoControls >= 114,  % 114 ISO controls

    Status#{
        soc2_compliant => Soc2Compliant,
        hipaa_compliant => HipaaCompliant,
        gdpr_compliant => GdprCompliance,
        iso27001_compliant => IsoCompliant,
        overall_compliance => Soc2Compliant and HipaaCompliant and GdprCompliant and IsoCompliant
    }.

get_soc2_control_count(Metrics) ->
    Controls = [
        soc2_security_controls,
        soc2_availability_controls,
        soc2_processing_integrity,
        soc2_confidentiality_controls,
        soc2_privacy_controls
    ],
    lists:sum([maps:get(Control, Metrics, 0) || Control <- Controls]).