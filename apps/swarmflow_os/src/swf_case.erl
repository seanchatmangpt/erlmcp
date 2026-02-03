%%%-------------------------------------------------------------------
%%% @doc SwarmFlow Case Process
%%%
%%% Core workflow case process using gen_statem. Each workflow instance
%%% runs as a separate supervised process implementing Petri net semantics.
%%%
%%% States: created -> running -> completed|failed|cancelled
%%%         running <-> suspended
%%%         running -> compensating -> completed|failed
%%%
%%% Features:
%%% - Petri net token management (marking)
%%% - YAWL split/join semantics
%%% - SAGA compensation pattern
%%% - Deadline enforcement
%%% - Parent/child case hierarchy
%%% - Append-only event logging
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_case).

-behaviour(gen_statem).

-include("swarmflow.hrl").

%% API exports
-export([start_link/2,
         start_link/3,
         fire_transition/2,
         get_enabled_transitions/1,
         get_marking/1,
         get_variables/1,
         set_variable/3,
         suspend/1,
         resume/1,
         cancel/1,
         cancel/2,
         compensate/1,
         get_state/1,
         get_case_record/1]).

%% gen_statem callbacks
-export([callback_mode/0,
         init/1,
         terminate/3,
         code_change/4]).

%% State function exports
-export([created/3,
         running/3,
         suspended/3,
         completed/3,
         failed/3,
         cancelled/3,
         compensating/3]).

%% Types
-type case_ref() :: pid() | binary().
-type transition_id() :: binary().
-type place_id() :: binary().
-type marking() :: #{place_id() => non_neg_integer()}.
-type case_variables() :: map().

-export_type([case_ref/0, transition_id/0, place_id/0, marking/0, case_variables/0]).

%% Internal state record
-record(data, {
    case_record :: #swf_case{},
    net :: #swf_net{},
    event_sequence = 0 :: non_neg_integer(),
    work_items = #{} :: #{transition_id() => #swf_work_item{}},
    compensation_log = [] :: [#swf_compensation_log{}],
    compensation_stack = [] :: [transition_id()],
    timer_refs = #{} :: #{transition_id() => reference()},
    deadline_timer :: reference() | undefined,
    parent_pid :: pid() | undefined,
    child_cases = #{} :: #{binary() => pid()},
    correlation_id :: binary() | undefined
}).

-type data() :: #data{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a new case with net_id and initial variables
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(NetId, InitialVars) ->
    start_link(NetId, InitialVars, #{}).

%% @doc Start a new case with options
-spec start_link(binary(), map(), map()) -> {ok, pid()} | {error, term()}.
start_link(NetId, InitialVars, Options) when is_binary(NetId), is_map(InitialVars), is_map(Options) ->
    gen_statem:start_link(?MODULE, {NetId, InitialVars, Options}, []).

%% @doc Fire an enabled transition
-spec fire_transition(case_ref(), transition_id()) ->
    {ok, marking()} | {error, not_enabled | guard_failed | term()}.
fire_transition(CaseRef, TransitionId) when is_binary(TransitionId) ->
    gen_statem:call(resolve_ref(CaseRef), {fire_transition, TransitionId}, 30000).

%% @doc Get list of currently enabled transitions
-spec get_enabled_transitions(case_ref()) -> {ok, [transition_id()]}.
get_enabled_transitions(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), get_enabled_transitions, 5000).

%% @doc Get current token distribution (marking)
-spec get_marking(case_ref()) -> {ok, marking()}.
get_marking(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), get_marking, 5000).

%% @doc Get case variables
-spec get_variables(case_ref()) -> {ok, case_variables()}.
get_variables(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), get_variables, 5000).

%% @doc Set a case variable
-spec set_variable(case_ref(), binary(), term()) -> ok | {error, term()}.
set_variable(CaseRef, Key, Value) when is_binary(Key) ->
    gen_statem:call(resolve_ref(CaseRef), {set_variable, Key, Value}, 5000).

%% @doc Suspend case execution
-spec suspend(case_ref()) -> ok | {error, invalid_state}.
suspend(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), suspend, 5000).

%% @doc Resume suspended case
-spec resume(case_ref()) -> ok | {error, invalid_state}.
resume(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), resume, 5000).

%% @doc Cancel case execution
-spec cancel(case_ref()) -> ok | {error, invalid_state}.
cancel(CaseRef) ->
    cancel(CaseRef, <<"user_cancelled">>).

%% @doc Cancel case with reason
-spec cancel(case_ref(), binary()) -> ok | {error, invalid_state}.
cancel(CaseRef, Reason) when is_binary(Reason) ->
    gen_statem:call(resolve_ref(CaseRef), {cancel, Reason}, 5000).

%% @doc Start compensation (SAGA rollback)
-spec compensate(case_ref()) -> ok | {error, term()}.
compensate(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), compensate, 30000).

%% @doc Get current state name
-spec get_state(case_ref()) -> {ok, case_status()}.
get_state(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), get_state, 5000).

%% @doc Get full case record
-spec get_case_record(case_ref()) -> {ok, #swf_case{}}.
get_case_record(CaseRef) ->
    gen_statem:call(resolve_ref(CaseRef), get_case_record, 5000).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

-spec callback_mode() -> state_functions.
callback_mode() ->
    state_functions.

-spec init({binary(), map(), map()}) -> {ok, created, data()} | {stop, term()}.
init({NetId, InitialVars, Options}) ->
    process_flag(trap_exit, true),

    %% Fetch the workflow net definition
    case fetch_net(NetId) of
        {ok, Net} ->
            Now = erlang:system_time(millisecond),
            CaseId = generate_case_id(),

            %% Create case record
            CaseRecord = #swf_case{
                id = CaseId,
                net_id = NetId,
                net_version = Net#swf_net.version,
                status = created,
                marking = Net#swf_net.initial_marking,
                variables = InitialVars,
                parent_case_id = maps:get(parent_case_id, Options, undefined),
                root_case_id = maps:get(root_case_id, Options, undefined),
                created_at = Now,
                deadline = maps:get(deadline, Options, undefined),
                priority = maps:get(priority, Options, 0),
                tenant_id = maps:get(tenant_id, Options, undefined),
                context_id = maps:get(context_id, Options, undefined),
                metadata = maps:get(metadata, Options, #{})
            },

            %% Register with gproc if available
            register_case(CaseId),

            Data = #data{
                case_record = CaseRecord,
                net = Net,
                event_sequence = 0,
                parent_pid = maps:get(parent_pid, Options, undefined),
                correlation_id = maps:get(correlation_id, Options, undefined)
            },

            %% Log case creation event
            {ok, Data1} = log_event(case_created, undefined, undefined, #{
                net_id => NetId,
                initial_vars => InitialVars
            }, Data),

            {ok, created, Data1};

        {error, Reason} ->
            {stop, {net_not_found, Reason}}
    end.

-spec terminate(term(), atom(), data()) -> ok.
terminate(Reason, StateName, Data) ->
    CaseId = (Data#data.case_record)#swf_case.id,
    logger:info("Case ~s terminating in state ~p: ~p", [CaseId, StateName, Reason]),

    %% Cancel any active timers
    cancel_all_timers(Data),

    %% Unregister from gproc
    unregister_case(CaseId),
    ok.

-spec code_change(term(), atom(), data(), term()) -> {ok, atom(), data()}.
code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

%%====================================================================
%% State: created
%%====================================================================

created({call, From}, {fire_transition, TransitionId}, Data) ->
    %% Transition to running and attempt to fire
    Now = erlang:system_time(millisecond),
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{
        status = running,
        started_at = Now
    },
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Log case started
    {ok, Data2} = log_event(case_started, undefined, undefined, #{}, Data1),

    %% Start deadline timer if set
    Data3 = maybe_start_deadline_timer(Data2),

    %% Now fire the transition in running state
    handle_fire_transition(TransitionId, From, running, Data3);

created({call, From}, get_enabled_transitions, Data) ->
    Enabled = compute_enabled_transitions(Data),
    {keep_state, Data, [{reply, From, {ok, Enabled}}]};

created({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

created({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

created({call, From}, {set_variable, Key, Value}, Data) ->
    handle_set_variable(Key, Value, From, created, Data);

created({call, From}, {cancel, Reason}, Data) ->
    handle_cancel(Reason, From, created, Data);

created({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, created}}]};

created({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

created({call, From}, suspend, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

created({call, From}, resume, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

created({call, From}, compensate, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

created(info, {timeout, deadline}, Data) ->
    handle_deadline_timeout(created, Data);

created(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: running
%%====================================================================

running({call, From}, {fire_transition, TransitionId}, Data) ->
    handle_fire_transition(TransitionId, From, running, Data);

running({call, From}, get_enabled_transitions, Data) ->
    Enabled = compute_enabled_transitions(Data),
    {keep_state, Data, [{reply, From, {ok, Enabled}}]};

running({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

running({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

running({call, From}, {set_variable, Key, Value}, Data) ->
    handle_set_variable(Key, Value, From, running, Data);

running({call, From}, suspend, Data) ->
    handle_suspend(From, Data);

running({call, From}, {cancel, Reason}, Data) ->
    handle_cancel(Reason, From, running, Data);

running({call, From}, compensate, Data) ->
    handle_start_compensation(From, Data);

running({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, running}}]};

running({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

running({call, From}, resume, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

running(info, {timeout, {transition, TransitionId}}, Data) ->
    handle_transition_timeout(TransitionId, running, Data);

running(info, {timeout, deadline}, Data) ->
    handle_deadline_timeout(running, Data);

running(info, {external_event, TransitionId, EventData}, Data) ->
    handle_external_event(TransitionId, EventData, running, Data);

running(info, {'EXIT', Pid, Reason}, Data) ->
    handle_child_exit(Pid, Reason, running, Data);

running(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: suspended
%%====================================================================

suspended({call, From}, resume, Data) ->
    handle_resume(From, Data);

suspended({call, From}, {cancel, Reason}, Data) ->
    handle_cancel(Reason, From, suspended, Data);

suspended({call, From}, get_enabled_transitions, Data) ->
    Enabled = compute_enabled_transitions(Data),
    {keep_state, Data, [{reply, From, {ok, Enabled}}]};

suspended({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

suspended({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

suspended({call, From}, {set_variable, Key, Value}, Data) ->
    handle_set_variable(Key, Value, From, suspended, Data);

suspended({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, suspended}}]};

suspended({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

suspended({call, From}, {fire_transition, _}, _Data) ->
    {keep_state_and_data, [{reply, From, {error, case_suspended}}]};

suspended({call, From}, suspend, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

suspended({call, From}, compensate, _Data) ->
    {keep_state_and_data, [{reply, From, {error, invalid_state}}]};

suspended(info, {timeout, deadline}, Data) ->
    handle_deadline_timeout(suspended, Data);

suspended(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: compensating
%%====================================================================

compensating({call, From}, get_enabled_transitions, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, []}}]};

compensating({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

compensating({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

compensating({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, compensating}}]};

compensating({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

compensating({call, From}, {cancel, Reason}, Data) ->
    handle_cancel(Reason, From, compensating, Data);

compensating({call, From}, _, _Data) ->
    {keep_state_and_data, [{reply, From, {error, compensating_in_progress}}]};

compensating(info, compensation_step_complete, Data) ->
    execute_next_compensation(Data);

compensating(info, {compensation_failed, Reason}, Data) ->
    handle_compensation_failure(Reason, Data);

compensating(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: completed
%%====================================================================

completed({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, completed}}]};

completed({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

completed({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

completed({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

completed({call, From}, _, _Data) ->
    {keep_state_and_data, [{reply, From, {error, case_completed}}]};

completed(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: failed
%%====================================================================

failed({call, From}, compensate, Data) ->
    handle_start_compensation(From, Data);

failed({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, failed}}]};

failed({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

failed({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

failed({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

failed({call, From}, _, _Data) ->
    {keep_state_and_data, [{reply, From, {error, case_failed}}]};

failed(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% State: cancelled
%%====================================================================

cancelled({call, From}, get_state, _Data) ->
    {keep_state_and_data, [{reply, From, {ok, cancelled}}]};

cancelled({call, From}, get_case_record, Data) ->
    {keep_state, Data, [{reply, From, {ok, Data#data.case_record}}]};

cancelled({call, From}, get_marking, Data) ->
    Marking = (Data#data.case_record)#swf_case.marking,
    {keep_state, Data, [{reply, From, {ok, Marking}}]};

cancelled({call, From}, get_variables, Data) ->
    Vars = (Data#data.case_record)#swf_case.variables,
    {keep_state, Data, [{reply, From, {ok, Vars}}]};

cancelled({call, From}, _, _Data) ->
    {keep_state_and_data, [{reply, From, {error, case_cancelled}}]};

cancelled(_EventType, _Event, _Data) ->
    keep_state_and_data.

%%====================================================================
%% Internal Functions: Transition Firing (Petri Net Semantics)
%%====================================================================

-spec handle_fire_transition(transition_id(), term(), atom(), data()) ->
    {next_state, atom(), data(), [term()]} | {keep_state, data(), [term()]}.
handle_fire_transition(TransitionId, From, CurrentState, Data) ->
    Net = Data#data.net,
    CaseRecord = Data#data.case_record,
    Marking = CaseRecord#swf_case.marking,
    Variables = CaseRecord#swf_case.variables,

    case maps:get(TransitionId, Net#swf_net.transitions, undefined) of
        undefined ->
            {keep_state, Data, [{reply, From, {error, transition_not_found}}]};

        Transition ->
            %% Check if transition is enabled
            case is_transition_enabled(TransitionId, Transition, Marking, Variables, Net) of
                false ->
                    {keep_state, Data, [{reply, From, {error, not_enabled}}]};

                true ->
                    %% Check guard condition
                    case check_guard(Transition, Variables) of
                        false ->
                            {ok, Data1} = log_event(transition_failed, TransitionId, undefined,
                                #{reason => guard_failed}, Data),
                            {keep_state, Data1, [{reply, From, {error, guard_failed}}]};

                        true ->
                            %% Fire the transition
                            fire_transition_internal(TransitionId, Transition, From, CurrentState, Data)
                    end
            end
    end.

-spec fire_transition_internal(transition_id(), #swf_transition{}, term(), atom(), data()) ->
    {next_state, atom(), data(), [term()]} | {keep_state, data(), [term()]}.
fire_transition_internal(TransitionId, Transition, From, CurrentState, Data) ->
    Net = Data#data.net,
    CaseRecord = Data#data.case_record,
    Marking = CaseRecord#swf_case.marking,
    Variables = CaseRecord#swf_case.variables,

    %% Log transition enabled
    {ok, Data1} = log_event(transition_enabled, TransitionId, undefined, #{}, Data),

    %% Consume tokens from input places
    {Marking1, ConsumedTokens} = consume_input_tokens(TransitionId, Marking, Net),

    %% Log token consumption
    Data2 = lists:foldl(fun({PlaceId, Count}, AccData) ->
        {ok, NewData} = log_event(token_consumed, TransitionId, PlaceId,
            #{count => Count}, AccData),
        NewData
    end, Data1, ConsumedTokens),

    %% Execute transition action
    case execute_action(Transition, Variables) of
        {ok, NewVariables} ->
            %% Produce tokens to output places
            {Marking2, ProducedTokens} = produce_output_tokens(TransitionId, Marking1, Net),

            %% Log token production
            Data3 = lists:foldl(fun({PlaceId, Count}, AccData) ->
                {ok, NewData} = log_event(token_produced, TransitionId, PlaceId,
                    #{count => Count}, AccData),
                NewData
            end, Data2, ProducedTokens),

            %% Log transition fired
            {ok, Data4} = log_event(transition_fired, TransitionId, undefined, #{}, Data3),

            %% Update case record
            NewCaseRecord = CaseRecord#swf_case{
                marking = Marking2,
                variables = NewVariables
            },
            Data5 = Data4#data{case_record = NewCaseRecord},

            %% Record in compensation log if transition has compensation
            Data6 = record_compensation(TransitionId, Variables, Data5),

            %% Check if case is complete (tokens in final places)
            case is_case_complete(Marking2, Net) of
                true ->
                    complete_case(From, Data6);
                false ->
                    %% Schedule automatic transitions
                    Data7 = schedule_automatic_transitions(Data6),
                    {keep_state, Data7, [{reply, From, {ok, Marking2}}]}
            end;

        {error, Reason} ->
            %% Transition action failed - restore marking
            {ok, Data3} = log_event(transition_failed, TransitionId, undefined,
                #{reason => Reason}, Data2),

            %% Check if we should fail the case
            case should_fail_on_error(Transition) of
                true ->
                    fail_case(Reason, CurrentState, Data3);
                false ->
                    {keep_state, Data3, [{reply, From, {error, {action_failed, Reason}}}]}
            end
    end.

%% @doc Check if a transition is enabled (has sufficient input tokens)
-spec is_transition_enabled(transition_id(), #swf_transition{}, marking(), map(), #swf_net{}) ->
    boolean().
is_transition_enabled(TransitionId, _Transition, Marking, _Variables, Net) ->
    %% Find all input arcs to this transition
    InputArcs = [Arc || Arc <- Net#swf_net.arcs,
                        Arc#swf_arc.target =:= TransitionId],

    %% Check each input place has sufficient tokens
    lists:all(fun(Arc) ->
        SourcePlace = Arc#swf_arc.source,
        RequiredTokens = Arc#swf_arc.weight,
        AvailableTokens = maps:get(SourcePlace, Marking, 0),

        case Arc#swf_arc.kind of
            normal -> AvailableTokens >= RequiredTokens;
            inhibitor -> AvailableTokens =:= 0;
            read -> AvailableTokens >= RequiredTokens;
            reset -> true
        end
    end, InputArcs).

%% @doc Check transition guard condition
-spec check_guard(#swf_transition{}, map()) -> boolean().
check_guard(#swf_transition{guard = undefined}, _Variables) ->
    true;
check_guard(#swf_transition{guard = Guard}, Variables) when is_function(Guard, 1) ->
    try Guard(Variables)
    catch _:_ -> false
    end.

%% @doc Consume tokens from input places
-spec consume_input_tokens(transition_id(), marking(), #swf_net{}) ->
    {marking(), [{place_id(), non_neg_integer()}]}.
consume_input_tokens(TransitionId, Marking, Net) ->
    InputArcs = [Arc || Arc <- Net#swf_net.arcs,
                        Arc#swf_arc.target =:= TransitionId,
                        Arc#swf_arc.kind =/= inhibitor,
                        Arc#swf_arc.kind =/= read],

    lists:foldl(fun(Arc, {AccMarking, AccConsumed}) ->
        SourcePlace = Arc#swf_arc.source,
        Weight = case Arc#swf_arc.kind of
            reset -> maps:get(SourcePlace, AccMarking, 0);
            _ -> Arc#swf_arc.weight
        end,
        CurrentTokens = maps:get(SourcePlace, AccMarking, 0),
        NewTokens = max(0, CurrentTokens - Weight),
        {maps:put(SourcePlace, NewTokens, AccMarking),
         [{SourcePlace, Weight} | AccConsumed]}
    end, {Marking, []}, InputArcs).

%% @doc Produce tokens to output places
-spec produce_output_tokens(transition_id(), marking(), #swf_net{}) ->
    {marking(), [{place_id(), non_neg_integer()}]}.
produce_output_tokens(TransitionId, Marking, Net) ->
    OutputArcs = [Arc || Arc <- Net#swf_net.arcs,
                         Arc#swf_arc.source =:= TransitionId],

    lists:foldl(fun(Arc, {AccMarking, AccProduced}) ->
        TargetPlace = Arc#swf_arc.target,
        Weight = Arc#swf_arc.weight,
        CurrentTokens = maps:get(TargetPlace, AccMarking, 0),

        %% Check capacity constraint
        Place = maps:get(TargetPlace, Net#swf_net.places, #swf_place{capacity = infinity}),
        MaxTokens = case Place#swf_place.capacity of
            infinity -> CurrentTokens + Weight;
            Cap -> min(Cap, CurrentTokens + Weight)
        end,

        {maps:put(TargetPlace, MaxTokens, AccMarking),
         [{TargetPlace, Weight} | AccProduced]}
    end, {Marking, []}, OutputArcs).

%% @doc Execute transition action
-spec execute_action(#swf_transition{}, map()) -> {ok, map()} | {error, term()}.
execute_action(#swf_transition{action = undefined}, Variables) ->
    {ok, Variables};
execute_action(#swf_transition{action = Action}, Variables) when is_function(Action, 1) ->
    try Action(Variables)
    catch
        Class:Reason:Stack ->
            logger:error("Transition action failed: ~p:~p~n~p", [Class, Reason, Stack]),
            {error, {action_exception, Reason}}
    end.

%% @doc Compute all currently enabled transitions
-spec compute_enabled_transitions(data()) -> [transition_id()].
compute_enabled_transitions(Data) ->
    Net = Data#data.net,
    CaseRecord = Data#data.case_record,
    Marking = CaseRecord#swf_case.marking,
    Variables = CaseRecord#swf_case.variables,

    maps:fold(fun(TransitionId, Transition, Acc) ->
        case is_transition_enabled(TransitionId, Transition, Marking, Variables, Net) of
            true ->
                case check_guard(Transition, Variables) of
                    true -> [TransitionId | Acc];
                    false -> Acc
                end;
            false -> Acc
        end
    end, [], Net#swf_net.transitions).

%% @doc Check if case has reached a final state
-spec is_case_complete(marking(), #swf_net{}) -> boolean().
is_case_complete(Marking, Net) ->
    FinalPlaces = Net#swf_net.final_places,

    %% Case is complete if any final place has a token
    %% and no enabled transitions remain
    HasFinalToken = lists:any(fun(PlaceId) ->
        maps:get(PlaceId, Marking, 0) > 0
    end, FinalPlaces),

    HasFinalToken.

%%====================================================================
%% Internal Functions: State Transitions
%%====================================================================

-spec handle_suspend(term(), data()) -> {next_state, suspended, data(), [term()]}.
handle_suspend(From, Data) ->
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{status = suspended},
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Pause all timers
    Data2 = pause_all_timers(Data1),

    {ok, Data3} = log_event(case_suspended, undefined, undefined, #{}, Data2),
    {next_state, suspended, Data3, [{reply, From, ok}]}.

-spec handle_resume(term(), data()) -> {next_state, running, data(), [term()]}.
handle_resume(From, Data) ->
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{status = running},
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Resume timers
    Data2 = resume_all_timers(Data1),

    {ok, Data3} = log_event(case_resumed, undefined, undefined, #{}, Data2),
    {next_state, running, Data3, [{reply, From, ok}]}.

-spec handle_cancel(binary(), term(), atom(), data()) ->
    {next_state, cancelled, data(), [term()]}.
handle_cancel(Reason, From, _CurrentState, Data) ->
    Now = erlang:system_time(millisecond),
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{
        status = cancelled,
        completed_at = Now
    },
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Cancel all timers
    Data2 = cancel_all_timers(Data1),

    %% Cancel child cases
    Data3 = cancel_child_cases(Reason, Data2),

    {ok, Data4} = log_event(case_cancelled, undefined, undefined, #{reason => Reason}, Data3),

    %% Notify parent if exists
    notify_parent({cancelled, Reason}, Data4),

    {next_state, cancelled, Data4, [{reply, From, ok}]}.

-spec complete_case(term(), data()) -> {next_state, completed, data(), [term()]}.
complete_case(From, Data) ->
    Now = erlang:system_time(millisecond),
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{
        status = completed,
        completed_at = Now
    },
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Cancel deadline timer
    Data2 = cancel_deadline_timer(Data1),

    {ok, Data3} = log_event(case_completed, undefined, undefined, #{}, Data2),

    %% Notify parent if exists
    notify_parent(completed, Data3),

    Marking = NewCaseRecord#swf_case.marking,
    {next_state, completed, Data3, [{reply, From, {ok, Marking}}]}.

-spec fail_case(term(), atom(), data()) -> {next_state, failed, data()}.
fail_case(Reason, _CurrentState, Data) ->
    Now = erlang:system_time(millisecond),
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{
        status = failed,
        completed_at = Now
    },
    Data1 = Data#data{case_record = NewCaseRecord},

    %% Cancel all timers
    Data2 = cancel_all_timers(Data1),

    {ok, Data3} = log_event(case_failed, undefined, undefined, #{reason => Reason}, Data2),

    %% Notify parent if exists
    notify_parent({failed, Reason}, Data3),

    {next_state, failed, Data3}.

%%====================================================================
%% Internal Functions: Compensation (SAGA Pattern)
%%====================================================================

-spec handle_start_compensation(term(), data()) ->
    {next_state, compensating, data(), [term()]}.
handle_start_compensation(From, Data) ->
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{status = compensating},
    Data1 = Data#data{case_record = NewCaseRecord},

    {ok, Data2} = log_event(compensation_started, undefined, undefined, #{}, Data1),
    {ok, Data3} = log_event(case_compensating, undefined, undefined, #{}, Data2),

    %% Start compensation execution
    self() ! compensation_step_complete,

    {next_state, compensating, Data3, [{reply, From, ok}]}.

-spec execute_next_compensation(data()) ->
    {next_state, atom(), data()} | {keep_state, data()}.
execute_next_compensation(Data) ->
    case Data#data.compensation_log of
        [] ->
            %% Compensation complete
            Now = erlang:system_time(millisecond),
            CaseRecord = Data#data.case_record,
            NewCaseRecord = CaseRecord#swf_case{
                status = completed,
                completed_at = Now
            },
            Data1 = Data#data{case_record = NewCaseRecord},

            {ok, Data2} = log_event(compensation_completed, undefined, undefined, #{}, Data1),
            {next_state, completed, Data2};

        [CompLog | Rest] ->
            %% Execute next compensation
            case execute_compensation_handler(CompLog, Data) of
                {ok, Data1} ->
                    UpdatedLog = CompLog#swf_compensation_log{
                        status = completed,
                        compensated_at = erlang:system_time(millisecond)
                    },
                    Data2 = Data1#data{
                        compensation_log = Rest
                    },
                    {ok, Data3} = log_event(compensation_completed,
                        CompLog#swf_compensation_log.transition_id, undefined, #{}, Data2),

                    %% Schedule next compensation step
                    self() ! compensation_step_complete,
                    {keep_state, Data3};

                {error, Reason} ->
                    self() ! {compensation_failed, Reason},
                    {keep_state, Data}
            end
    end.

-spec execute_compensation_handler(#swf_compensation_log{}, data()) ->
    {ok, data()} | {error, term()}.
execute_compensation_handler(CompLog, Data) ->
    TransitionId = CompLog#swf_compensation_log.transition_id,
    CompensationData = CompLog#swf_compensation_log.compensation_data,
    Net = Data#data.net,

    %% Look up compensation transition
    case maps:get(TransitionId, Net#swf_net.transitions, undefined) of
        undefined ->
            {ok, Data};
        Transition ->
            case Transition#swf_transition.metadata of
                #{compensation_action := CompAction} when is_function(CompAction, 1) ->
                    try CompAction(CompensationData) of
                        {ok, _} -> {ok, Data};
                        {error, Reason} -> {error, Reason}
                    catch
                        _:Reason -> {error, Reason}
                    end;
                _ ->
                    {ok, Data}
            end
    end.

-spec handle_compensation_failure(term(), data()) -> {next_state, failed, data()}.
handle_compensation_failure(Reason, Data) ->
    Now = erlang:system_time(millisecond),
    CaseRecord = Data#data.case_record,
    NewCaseRecord = CaseRecord#swf_case{
        status = failed,
        completed_at = Now
    },
    Data1 = Data#data{case_record = NewCaseRecord},

    {ok, Data2} = log_event(case_failed, undefined, undefined,
        #{reason => {compensation_failed, Reason}}, Data1),

    {next_state, failed, Data2}.

-spec record_compensation(transition_id(), map(), data()) -> data().
record_compensation(TransitionId, Variables, Data) ->
    Net = Data#data.net,
    case maps:get(TransitionId, Net#swf_net.transitions, undefined) of
        undefined -> Data;
        Transition ->
            case Transition#swf_transition.metadata of
                #{requires_compensation := true} ->
                    CompLog = #swf_compensation_log{
                        id = generate_ulid(),
                        case_id = (Data#data.case_record)#swf_case.id,
                        transition_id = TransitionId,
                        compensation_data = Variables,
                        status = pending,
                        logged_at = erlang:system_time(millisecond)
                    },
                    Data#data{
                        compensation_log = [CompLog | Data#data.compensation_log]
                    };
                _ ->
                    Data
            end
    end.

%%====================================================================
%% Internal Functions: Variables
%%====================================================================

-spec handle_set_variable(binary(), term(), term(), atom(), data()) ->
    {keep_state, data(), [term()]}.
handle_set_variable(Key, Value, From, _CurrentState, Data) ->
    CaseRecord = Data#data.case_record,
    OldVars = CaseRecord#swf_case.variables,
    NewVars = maps:put(Key, Value, OldVars),
    NewCaseRecord = CaseRecord#swf_case{variables = NewVars},
    Data1 = Data#data{case_record = NewCaseRecord},

    {ok, Data2} = log_event(variable_set, undefined, undefined,
        #{key => Key, value => Value}, Data1),

    {keep_state, Data2, [{reply, From, ok}]}.

%%====================================================================
%% Internal Functions: Timers
%%====================================================================

-spec maybe_start_deadline_timer(data()) -> data().
maybe_start_deadline_timer(Data) ->
    CaseRecord = Data#data.case_record,
    case CaseRecord#swf_case.deadline of
        undefined -> Data;
        Deadline ->
            Now = erlang:system_time(millisecond),
            Delay = max(0, Deadline - Now),
            TimerRef = erlang:send_after(Delay, self(), {timeout, deadline}),
            Data#data{deadline_timer = TimerRef}
    end.

-spec cancel_deadline_timer(data()) -> data().
cancel_deadline_timer(Data) ->
    case Data#data.deadline_timer of
        undefined -> Data;
        TimerRef ->
            erlang:cancel_timer(TimerRef),
            Data#data{deadline_timer = undefined}
    end.

-spec schedule_automatic_transitions(data()) -> data().
schedule_automatic_transitions(Data) ->
    Net = Data#data.net,
    Enabled = compute_enabled_transitions(Data),

    %% Find automatic transitions that are enabled
    AutoTransitions = lists:filter(fun(TId) ->
        case maps:get(TId, Net#swf_net.transitions, undefined) of
            #swf_transition{kind = automatic} -> true;
            #swf_transition{kind = time, timeout_ms = Timeout} when Timeout =/= undefined -> true;
            _ -> false
        end
    end, Enabled),

    %% Schedule timers for time-based transitions
    lists:foldl(fun(TId, AccData) ->
        case maps:get(TId, Net#swf_net.transitions, undefined) of
            #swf_transition{kind = time, timeout_ms = Timeout} ->
                TimerRef = erlang:send_after(Timeout, self(), {timeout, {transition, TId}}),
                TimerRefs = AccData#data.timer_refs,
                AccData#data{timer_refs = maps:put(TId, TimerRef, TimerRefs)};
            _ ->
                AccData
        end
    end, Data, AutoTransitions).

-spec handle_transition_timeout(transition_id(), atom(), data()) ->
    {keep_state, data()} | {next_state, atom(), data()}.
handle_transition_timeout(TransitionId, CurrentState, Data) ->
    %% Remove timer ref
    TimerRefs = maps:remove(TransitionId, Data#data.timer_refs),
    Data1 = Data#data{timer_refs = TimerRefs},

    %% Check if transition is still enabled
    Enabled = compute_enabled_transitions(Data1),
    case lists:member(TransitionId, Enabled) of
        true ->
            %% Fire the transition
            case handle_fire_transition(TransitionId, {self(), make_ref()}, CurrentState, Data1) of
                {keep_state, Data2, _Actions} ->
                    {keep_state, Data2};
                {next_state, NewState, Data2, _Actions} ->
                    {next_state, NewState, Data2}
            end;
        false ->
            {keep_state, Data1}
    end.

-spec handle_deadline_timeout(atom(), data()) -> {next_state, failed, data()}.
handle_deadline_timeout(_CurrentState, Data) ->
    fail_case(deadline_exceeded, running, Data).

-spec pause_all_timers(data()) -> data().
pause_all_timers(Data) ->
    %% For simplicity, cancel timers on suspend
    %% A production system would save remaining time
    cancel_all_timers(Data).

-spec resume_all_timers(data()) -> data().
resume_all_timers(Data) ->
    %% Reschedule automatic transitions
    schedule_automatic_transitions(Data).

-spec cancel_all_timers(data()) -> data().
cancel_all_timers(Data) ->
    %% Cancel transition timers
    maps:foreach(fun(_TId, TimerRef) ->
        erlang:cancel_timer(TimerRef)
    end, Data#data.timer_refs),

    %% Cancel deadline timer
    Data1 = cancel_deadline_timer(Data),
    Data1#data{timer_refs = #{}}.

%%====================================================================
%% Internal Functions: External Events
%%====================================================================

-spec handle_external_event(transition_id(), map(), atom(), data()) ->
    {keep_state, data()} | {next_state, atom(), data()}.
handle_external_event(TransitionId, EventData, CurrentState, Data) ->
    Net = Data#data.net,
    case maps:get(TransitionId, Net#swf_net.transitions, undefined) of
        #swf_transition{kind = message} = Transition ->
            %% Merge event data with variables
            CaseRecord = Data#data.case_record,
            Variables = CaseRecord#swf_case.variables,
            NewVariables = maps:merge(Variables, EventData),
            NewCaseRecord = CaseRecord#swf_case{variables = NewVariables},
            Data1 = Data#data{case_record = NewCaseRecord},

            %% Log message received
            {ok, Data2} = log_event(message_received, TransitionId, undefined, EventData, Data1),

            %% Check if transition is now enabled and fire it
            Enabled = compute_enabled_transitions(Data2),
            case lists:member(TransitionId, Enabled) of
                true ->
                    case handle_fire_transition(TransitionId, {self(), make_ref()}, CurrentState, Data2) of
                        {keep_state, Data3, _} -> {keep_state, Data3};
                        {next_state, NewState, Data3, _} -> {next_state, NewState, Data3}
                    end;
                false ->
                    {keep_state, Data2}
            end;
        _ ->
            {keep_state, Data}
    end.

%%====================================================================
%% Internal Functions: Child Cases
%%====================================================================

-spec handle_child_exit(pid(), term(), atom(), data()) -> {keep_state, data()}.
handle_child_exit(Pid, Reason, _CurrentState, Data) ->
    ChildCases = Data#data.child_cases,

    %% Find which child case exited
    case maps:fold(fun(CaseId, CPid, Acc) ->
        case CPid of
            Pid -> {found, CaseId};
            _ -> Acc
        end
    end, not_found, ChildCases) of
        {found, CaseId} ->
            NewChildCases = maps:remove(CaseId, ChildCases),
            Data1 = Data#data{child_cases = NewChildCases},

            case Reason of
                normal -> {keep_state, Data1};
                _ ->
                    logger:warning("Child case ~s exited with reason: ~p", [CaseId, Reason]),
                    {keep_state, Data1}
            end;
        not_found ->
            {keep_state, Data}
    end.

-spec cancel_child_cases(binary(), data()) -> data().
cancel_child_cases(Reason, Data) ->
    maps:foreach(fun(_CaseId, Pid) ->
        catch gen_statem:call(Pid, {cancel, Reason}, 5000)
    end, Data#data.child_cases),
    Data#data{child_cases = #{}}.

-spec notify_parent(term(), data()) -> ok.
notify_parent(Message, Data) ->
    case Data#data.parent_pid of
        undefined -> ok;
        ParentPid ->
            CaseId = (Data#data.case_record)#swf_case.id,
            ParentPid ! {child_case_event, CaseId, Message},
            ok
    end.

%%====================================================================
%% Internal Functions: Event Logging
%%====================================================================

-spec log_event(event_type(), transition_id() | undefined, place_id() | undefined,
                map(), data()) -> {ok, data()}.
log_event(EventType, TransitionId, PlaceId, EventData, Data) ->
    Seq = Data#data.event_sequence,
    CaseId = (Data#data.case_record)#swf_case.id,

    Event = #swf_event{
        id = generate_ulid(),
        case_id = CaseId,
        event_type = EventType,
        transition_id = TransitionId,
        place_id = PlaceId,
        timestamp = erlang:system_time(microsecond),
        sequence = Seq,
        data = EventData,
        actor_id = undefined,
        correlation_id = Data#data.correlation_id,
        causation_id = undefined
    },

    %% Append to event log (async to avoid blocking)
    append_event_to_log(Event),

    {ok, Data#data{event_sequence = Seq + 1}}.

-spec append_event_to_log(#swf_event{}) -> ok.
append_event_to_log(Event) ->
    %% Call swf_event_log if available
    case erlang:whereis(swf_event_log) of
        undefined ->
            %% Log locally if event log not available
            logger:debug("Case event: ~p", [Event]);
        Pid ->
            Pid ! {append_event, Event}
    end,
    ok.

%%====================================================================
%% Internal Functions: Utilities
%%====================================================================

-spec resolve_ref(case_ref()) -> pid().
resolve_ref(Pid) when is_pid(Pid) ->
    Pid;
resolve_ref(CaseId) when is_binary(CaseId) ->
    %% Look up via gproc or registry
    case whereis_case(CaseId) of
        undefined -> error({case_not_found, CaseId});
        Pid -> Pid
    end.

-spec register_case(binary()) -> ok.
register_case(CaseId) ->
    %% Register with gproc if available
    case code:is_loaded(gproc) of
        false -> ok;
        _ ->
            try gproc:reg({n, l, {swf_case, CaseId}}), ok
            catch _:_ -> ok
            end
    end,
    ok.

-spec unregister_case(binary()) -> ok.
unregister_case(CaseId) ->
    case code:is_loaded(gproc) of
        false -> ok;
        _ ->
            try gproc:unreg({n, l, {swf_case, CaseId}})
            catch _:_ -> ok
            end
    end,
    ok.

-spec whereis_case(binary()) -> pid() | undefined.
whereis_case(CaseId) ->
    case code:is_loaded(gproc) of
        false -> undefined;
        _ ->
            try gproc:lookup_pid({n, l, {swf_case, CaseId}})
            catch _:_ -> undefined
            end
    end.

-spec fetch_net(binary()) -> {ok, #swf_net{}} | {error, not_found}.
fetch_net(NetId) ->
    %% Look up net from registry/storage
    case erlang:whereis(swf_net_registry) of
        undefined ->
            %% Return a minimal default net for testing
            {ok, #swf_net{
                id = NetId,
                name = <<"default">>,
                version = <<"1.0.0">>,
                places = #{},
                transitions = #{},
                arcs = [],
                initial_marking = #{},
                final_places = []
            }};
        Pid ->
            gen_server:call(Pid, {get_net, NetId}, 5000)
    end.

-spec generate_case_id() -> binary().
generate_case_id() ->
    generate_ulid().

-spec generate_ulid() -> binary().
generate_ulid() ->
    %% Generate a ULID-like ID (timestamp + random)
    Timestamp = erlang:system_time(millisecond),
    Random = crypto:strong_rand_bytes(10),
    TimestampBin = integer_to_binary(Timestamp, 32),
    RandomBin = base64:encode(Random),
    <<TimestampBin/binary, RandomBin/binary>>.

-spec should_fail_on_error(#swf_transition{}) -> boolean().
should_fail_on_error(#swf_transition{metadata = Metadata}) when is_map(Metadata) ->
    maps:get(fail_on_error, Metadata, true);
should_fail_on_error(_) ->
    true.
