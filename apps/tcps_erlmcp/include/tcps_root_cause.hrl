%%%-------------------------------------------------------------------
%%% @doc TCPS Root Cause Analysis Header
%%%
%%% Record definitions for root cause analysis framework.
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(TCPS_ROOT_CAUSE_HRL).
-define(TCPS_ROOT_CAUSE_HRL, true).

%% Root cause analysis record
-record(five_whys, {
    analysis_id :: binary(),
    andon_event_id :: binary(),
    problem :: binary(),
    why_1 :: binary() | undefined,
    why_2 :: binary() | undefined,
    why_3 :: binary() | undefined,
    why_4 :: binary() | undefined,
    why_5 :: binary() | undefined,
    root_cause :: binary() | undefined,
    prevention_action :: binary() | undefined,
    created_at :: integer(),
    updated_at :: integer(),
    finalized_at :: integer() | undefined,
    status :: pending | in_progress | finalized
}).

%% Prevention delta record
-record(prevention_delta, {
    shacl_additions :: [binary()],
    test_additions :: [binary()],
    template_improvements :: [binary()],
    dependency_pins :: #{binary() => binary()}
}).

-endif.
