%%%-----------------------------------------------------------------------------
%%% @doc TCPS Diataxis Tutorial Orchestration Module
%%%
%%% Main orchestrator for interactive learning-oriented tutorials implementing
%%% the Diataxis framework's tutorial quadrant. Provides comprehensive hands-on
%%% learning experiences for TCPS (Toyota Cyber Production System) concepts.
%%%
%%% Features:
%%% - Tutorial lifecycle management (start, progress, complete, abandon)
%%% - Progress tracking with checkpoints and state persistence
%%% - Multi-tutorial curriculum with prerequisite enforcement
%%% - Interactive step execution with real TCPS integration
%%% - Hint system and guided assistance
%%% - Achievement tracking and learning path recommendations
%%%
%%% Available Tutorials:
%%% 1. quality_gate - Your First Quality Gate (beginner)
%%% 2. kanban_wip - Kanban WIP Limits in Action (beginner)
%%% 3. andon_event - Triggering an Andon Event (intermediate)
%%% 4. five_whys - 5 Whys Root Cause Analysis (intermediate)
%%% 5. complete_workflow - Complete TCPS Workflow (advanced)
%%%
%%% Learning Objectives:
%%% - Understand TCPS core concepts (Jidoka, Kanban, Andon, Kaizen)
%%% - Gain hands-on experience with quality gates and WIP limits
%%% - Practice stop-the-line problem resolution
%%% - Master root cause analysis techniques
%%% - Execute end-to-end production workflows
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(tcps_diataxis_tutorial).
-behaviour(gen_server).

%% API exports - Tutorial Management
-export([
    start_link/0,
    list_tutorials/0,
    get_tutorial_info/1,
    start_tutorial/2,
    get_tutorial_progress/1,
    complete_tutorial/1,
    abandon_tutorial/1
]).

%% API exports - Step Execution
-export([
    get_current_step/1,
    execute_step/2,
    validate_step/2,
    get_hint/1,
    skip_step/2
]).

%% API exports - Progress & Analytics
-export([
    get_user_progress/1,
    get_achievements/1,
    get_learning_path/1,
    export_progress/1
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

%%%=============================================================================
%%% Type Definitions
%%%=============================================================================

-type tutorial_id() :: quality_gate | kanban_wip | andon_event | five_whys | complete_workflow.
-type user_id() :: binary().
-type session_id() :: binary().
-type step_id() :: binary().
-type difficulty() :: beginner | intermediate | advanced.

-type tutorial_metadata() :: #{
    id := tutorial_id(),
    title := binary(),
    description := binary(),
    difficulty := difficulty(),
    estimated_time := pos_integer(),  % minutes
    prerequisites := [tutorial_id()],
    learning_objectives := [binary()],
    tags := [binary()]
}.

-type tutorial_session() :: #{
    session_id := session_id(),
    tutorial_id := tutorial_id(),
    user_id := user_id(),
    started_at := erlang:timestamp(),
    current_step := step_id(),
    completed_steps := [step_id()],
    status := in_progress | completed | abandoned,
    hints_used := non_neg_integer(),
    checkpoints := #{step_id() => map()},
    completion_time := erlang:timestamp() | undefined
}.

-type progress_report() :: #{
    user_id := user_id(),
    completed_tutorials := [tutorial_id()],
    in_progress_tutorials := [tutorial_id()],
    total_time_spent := non_neg_integer(),  % minutes
    achievements := [binary()],
    skill_level := beginner | intermediate | advanced | expert
}.

-export_type([
    tutorial_id/0,
    user_id/0,
    session_id/0,
    tutorial_metadata/0,
    tutorial_session/0,
    progress_report/0
]).

%%%=============================================================================
%%% State Record
%%%=============================================================================

-record(state, {
    tutorials = #{} :: #{tutorial_id() => tutorial_metadata()},
    active_sessions = #{} :: #{session_id() => tutorial_session()},
    user_progress = #{} :: #{user_id() => progress_report()},
    session_by_user = #{} :: #{user_id() => session_id()}
}).

-type state() :: #state{}.

%%%=============================================================================
%%% API Functions - Tutorial Management
%%%=============================================================================

%% @doc Start the tutorial orchestration server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc List all available tutorials with metadata
-spec list_tutorials() -> {ok, [tutorial_metadata()]} | {error, term()}.
list_tutorials() ->
    gen_server:call(?MODULE, list_tutorials).

%% @doc Get detailed information about a specific tutorial
-spec get_tutorial_info(tutorial_id()) -> {ok, tutorial_metadata()} | {error, not_found}.
get_tutorial_info(TutorialId) ->
    gen_server:call(?MODULE, {get_tutorial_info, TutorialId}).

%% @doc Start a new tutorial session for a user
-spec start_tutorial(user_id(), tutorial_id()) ->
    {ok, session_id()} | {error, term()}.
start_tutorial(UserId, TutorialId) ->
    gen_server:call(?MODULE, {start_tutorial, UserId, TutorialId}).

%% @doc Get current progress for a tutorial session
-spec get_tutorial_progress(session_id()) ->
    {ok, tutorial_session()} | {error, not_found}.
get_tutorial_progress(SessionId) ->
    gen_server:call(?MODULE, {get_tutorial_progress, SessionId}).

%% @doc Mark tutorial as completed
-spec complete_tutorial(session_id()) -> ok | {error, term()}.
complete_tutorial(SessionId) ->
    gen_server:call(?MODULE, {complete_tutorial, SessionId}).

%% @doc Abandon a tutorial session
-spec abandon_tutorial(session_id()) -> ok | {error, term()}.
abandon_tutorial(SessionId) ->
    gen_server:call(?MODULE, {abandon_tutorial, SessionId}).

%%%=============================================================================
%%% API Functions - Step Execution
%%%=============================================================================

%% @doc Get current step information for a session
-spec get_current_step(session_id()) ->
    {ok, map()} | {error, term()}.
get_current_step(SessionId) ->
    gen_server:call(?MODULE, {get_current_step, SessionId}).

%% @doc Execute a tutorial step with user input
-spec execute_step(session_id(), map()) ->
    {ok, step_result()} | {error, term()}.
execute_step(SessionId, StepInput) ->
    gen_server:call(?MODULE, {execute_step, SessionId, StepInput}, 30000).

-type step_result() :: #{
    success := boolean(),
    output := term(),
    feedback := binary(),
    next_step := step_id() | completed,
    checkpoint := map() | undefined
}.

%% @doc Validate step completion
-spec validate_step(session_id(), map()) ->
    {ok, validation_result()} | {error, term()}.
validate_step(SessionId, Validation) ->
    gen_server:call(?MODULE, {validate_step, SessionId, Validation}).

-type validation_result() :: #{
    valid := boolean(),
    errors := [binary()],
    suggestions := [binary()]
}.

%% @doc Get hint for current step
-spec get_hint(session_id()) -> {ok, binary()} | {error, term()}.
get_hint(SessionId) ->
    gen_server:call(?MODULE, {get_hint, SessionId}).

%% @doc Skip current step (with justification)
-spec skip_step(session_id(), binary()) -> ok | {error, term()}.
skip_step(SessionId, Reason) ->
    gen_server:call(?MODULE, {skip_step, SessionId, Reason}).

%%%=============================================================================
%%% API Functions - Progress & Analytics
%%%=============================================================================

%% @doc Get overall progress for a user across all tutorials
-spec get_user_progress(user_id()) ->
    {ok, progress_report()} | {error, not_found}.
get_user_progress(UserId) ->
    gen_server:call(?MODULE, {get_user_progress, UserId}).

%% @doc Get achievements earned by user
-spec get_achievements(user_id()) -> {ok, [binary()]} | {error, term()}.
get_achievements(UserId) ->
    gen_server:call(?MODULE, {get_achievements, UserId}).

%% @doc Get recommended learning path for user
-spec get_learning_path(user_id()) -> {ok, [tutorial_id()]} | {error, term()}.
get_learning_path(UserId) ->
    gen_server:call(?MODULE, {get_learning_path, UserId}).

%% @doc Export user progress to JSON
-spec export_progress(user_id()) -> {ok, binary()} | {error, term()}.
export_progress(UserId) ->
    gen_server:call(?MODULE, {export_progress, UserId}).

%%%=============================================================================
%%% gen_server Callbacks
%%%=============================================================================

%% @private
init([]) ->
    Tutorials = initialize_tutorials(),
    {ok, #state{tutorials = Tutorials}}.

%% @private
handle_call(list_tutorials, _From, State) ->
    Tutorials = maps:values(State#state.tutorials),
    {reply, {ok, Tutorials}, State};

handle_call({get_tutorial_info, TutorialId}, _From, State) ->
    case maps:find(TutorialId, State#state.tutorials) of
        {ok, Metadata} -> {reply, {ok, Metadata}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({start_tutorial, UserId, TutorialId}, _From, State) ->
    case validate_prerequisites(UserId, TutorialId, State) of
        ok ->
            SessionId = generate_session_id(),
            Session = create_session(SessionId, UserId, TutorialId),
            NewState = State#state{
                active_sessions = maps:put(SessionId, Session, State#state.active_sessions),
                session_by_user = maps:put(UserId, SessionId, State#state.session_by_user)
            },
            {reply, {ok, SessionId}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_tutorial_progress, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} -> {reply, {ok, Session}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({complete_tutorial, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            CompletedSession = Session#{
                status => completed,
                completion_time => erlang:timestamp()
            },
            NewState = update_user_progress(CompletedSession, State),
            NewState2 = NewState#state{
                active_sessions = maps:put(SessionId, CompletedSession, NewState#state.active_sessions)
            },
            {reply, ok, NewState2};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({abandon_tutorial, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            AbandonedSession = Session#{status => abandoned},
            NewState = State#state{
                active_sessions = maps:put(SessionId, AbandonedSession, State#state.active_sessions)
            },
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_current_step, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            TutorialId = maps:get(tutorial_id, Session),
            StepId = maps:get(current_step, Session),
            StepInfo = tcps_tutorial_steps:get_step_info(TutorialId, StepId),
            {reply, {ok, StepInfo}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({execute_step, SessionId, StepInput}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            TutorialId = maps:get(tutorial_id, Session),
            StepId = maps:get(current_step, Session),

            Result = tcps_tutorial_steps:execute_step(TutorialId, StepId, StepInput),

            case Result of
                {ok, StepResult} ->
                    UpdatedSession = update_session_progress(Session, StepResult),
                    NewState = State#state{
                        active_sessions = maps:put(SessionId, UpdatedSession, State#state.active_sessions)
                    },
                    {reply, {ok, StepResult}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({validate_step, SessionId, Validation}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            TutorialId = maps:get(tutorial_id, Session),
            StepId = maps:get(current_step, Session),
            Result = tcps_tutorial_validation:validate_step(TutorialId, StepId, Validation),
            {reply, {ok, Result}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_hint, SessionId}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            TutorialId = maps:get(tutorial_id, Session),
            StepId = maps:get(current_step, Session),
            Hint = tcps_tutorial_steps:get_hint(TutorialId, StepId),

            UpdatedSession = Session#{
                hints_used => maps:get(hints_used, Session) + 1
            },
            NewState = State#state{
                active_sessions = maps:put(SessionId, UpdatedSession, State#state.active_sessions)
            },
            {reply, {ok, Hint}, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({skip_step, SessionId, Reason}, _From, State) ->
    case maps:find(SessionId, State#state.active_sessions) of
        {ok, Session} ->
            NextStep = get_next_step(Session),
            UpdatedSession = Session#{
                current_step => NextStep,
                completed_steps => [maps:get(current_step, Session) | maps:get(completed_steps, Session)]
            },
            NewState = State#state{
                active_sessions = maps:put(SessionId, UpdatedSession, State#state.active_sessions)
            },
            io:format("Step skipped. Reason: ~s~n", [Reason]),
            {reply, ok, NewState};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({get_user_progress, UserId}, _From, State) ->
    case maps:find(UserId, State#state.user_progress) of
        {ok, Progress} -> {reply, {ok, Progress}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({get_achievements, UserId}, _From, State) ->
    case maps:find(UserId, State#state.user_progress) of
        {ok, Progress} ->
            Achievements = maps:get(achievements, Progress, []),
            {reply, {ok, Achievements}, State};
        error ->
            {reply, {ok, []}, State}
    end;

handle_call({get_learning_path, UserId}, _From, State) ->
    Path = calculate_learning_path(UserId, State),
    {reply, {ok, Path}, State};

handle_call({export_progress, UserId}, _From, State) ->
    case maps:find(UserId, State#state.user_progress) of
        {ok, Progress} ->
            Json = jsx:encode(Progress),
            {reply, {ok, Json}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

%% @private Initialize tutorial metadata
initialize_tutorials() ->
    #{
        quality_gate => #{
            id => quality_gate,
            title => <<"Your First Quality Gate">>,
            description => <<"Learn to create and run quality gates for zero-defect delivery">>,
            difficulty => beginner,
            estimated_time => 15,
            prerequisites => [],
            learning_objectives => [
                <<"Understand quality gate concepts">>,
                <<"Create a simple quality gate">>,
                <<"Run validation and interpret results">>,
                <<"Handle gate failures appropriately">>
            ],
            tags => [<<"quality">>, <<"jidoka">>, <<"beginner">>]
        },
        kanban_wip => #{
            id => kanban_wip,
            title => <<"Kanban WIP Limits in Action">>,
            description => <<"Set up Kanban boards with WIP limits and pull signals">>,
            difficulty => beginner,
            estimated_time => 20,
            prerequisites => [],
            learning_objectives => [
                <<"Understand WIP limit concepts">>,
                <<"Configure Kanban buckets">>,
                <<"Process pull signals">>,
                <<"Handle WIP limit violations">>
            ],
            tags => [<<"kanban">>, <<"wip">>, <<"beginner">>]
        },
        andon_event => #{
            id => andon_event,
            title => <<"Triggering an Andon Event">>,
            description => <<"Simulate stop-the-line quality issues and resolution">>,
            difficulty => intermediate,
            estimated_time => 25,
            prerequisites => [quality_gate],
            learning_objectives => [
                <<"Understand Andon stop-the-line principles">>,
                <<"Trigger Andon events for failures">>,
                <<"Navigate resolution workflows">>,
                <<"Document problem resolution">>
            ],
            tags => [<<"andon">>, <<"jidoka">>, <<"intermediate">>]
        },
        five_whys => #{
            id => five_whys,
            title => <<"5 Whys Root Cause Analysis">>,
            description => <<"Complete a structured 5 Whys investigation">>,
            difficulty => intermediate,
            estimated_time => 30,
            prerequisites => [andon_event],
            learning_objectives => [
                <<"Master 5 Whys methodology">>,
                <<"Identify true root causes">>,
                <<"Generate prevention actions">>,
                <<"Document analysis findings">>
            ],
            tags => [<<"kaizen">>, <<"root-cause">>, <<"intermediate">>]
        },
        complete_workflow => #{
            id => complete_workflow,
            title => <<"Complete TCPS Workflow">>,
            description => <<"Execute end-to-end work order through production">>,
            difficulty => advanced,
            estimated_time => 45,
            prerequisites => [kanban_wip, five_whys],
            learning_objectives => [
                <<"Execute complete TCPS workflow">>,
                <<"Integrate all TCPS concepts">>,
                <<"Handle end-to-end scenarios">>,
                <<"Achieve production readiness">>
            ],
            tags => [<<"workflow">>, <<"integration">>, <<"advanced">>]
        }
    }.

%% @private Generate unique session ID
generate_session_id() ->
    Timestamp = erlang:system_time(microsecond),
    Random = rand:uniform(999999),
    list_to_binary(io_lib:format("session_~p_~p", [Timestamp, Random])).

%% @private Create new tutorial session
create_session(SessionId, UserId, TutorialId) ->
    #{
        session_id => SessionId,
        tutorial_id => TutorialId,
        user_id => UserId,
        started_at => erlang:timestamp(),
        current_step => get_first_step(TutorialId),
        completed_steps => [],
        status => in_progress,
        hints_used => 0,
        checkpoints => #{},
        completion_time => undefined
    }.

%% @private Get first step for tutorial
get_first_step(TutorialId) ->
    tcps_tutorial_steps:get_first_step(TutorialId).

%% @private Get next step in tutorial
get_next_step(Session) ->
    TutorialId = maps:get(tutorial_id, Session),
    CurrentStep = maps:get(current_step, Session),
    tcps_tutorial_steps:get_next_step(TutorialId, CurrentStep).

%% @private Validate prerequisites for tutorial
validate_prerequisites(UserId, TutorialId, State) ->
    case maps:find(TutorialId, State#state.tutorials) of
        {ok, Metadata} ->
            Prerequisites = maps:get(prerequisites, Metadata),
            check_prerequisites_completed(UserId, Prerequisites, State);
        error ->
            {error, tutorial_not_found}
    end.

%% @private Check if prerequisites are completed
check_prerequisites_completed(_UserId, [], _State) ->
    ok;
check_prerequisites_completed(UserId, Prerequisites, State) ->
    case maps:find(UserId, State#state.user_progress) of
        {ok, Progress} ->
            Completed = maps:get(completed_tutorials, Progress, []),
            case lists:all(fun(P) -> lists:member(P, Completed) end, Prerequisites) of
                true -> ok;
                false -> {error, prerequisites_not_met}
            end;
        error ->
            {error, prerequisites_not_met}
    end.

%% @private Update session progress after step execution
update_session_progress(Session, StepResult) ->
    CurrentStep = maps:get(current_step, Session),
    NextStep = maps:get(next_step, StepResult),
    Checkpoint = maps:get(checkpoint, StepResult, undefined),

    Checkpoints = case Checkpoint of
        undefined -> maps:get(checkpoints, Session);
        _ -> maps:put(CurrentStep, Checkpoint, maps:get(checkpoints, Session))
    end,

    Session#{
        current_step => NextStep,
        completed_steps => [CurrentStep | maps:get(completed_steps, Session)],
        checkpoints => Checkpoints
    }.

%% @private Update user progress after tutorial completion
update_user_progress(Session, State) ->
    UserId = maps:get(user_id, Session),
    TutorialId = maps:get(tutorial_id, Session),

    CurrentProgress = maps:get(UserId, State#state.user_progress, #{
        user_id => UserId,
        completed_tutorials => [],
        in_progress_tutorials => [],
        total_time_spent => 0,
        achievements => [],
        skill_level => beginner
    }),

    TimeSpent = calculate_time_spent(Session),
    NewCompleted = [TutorialId | maps:get(completed_tutorials, CurrentProgress)],
    NewInProgress = lists:delete(TutorialId, maps:get(in_progress_tutorials, CurrentProgress)),
    NewTotalTime = maps:get(total_time_spent, CurrentProgress) + TimeSpent,

    NewAchievements = calculate_achievements(NewCompleted, Session),
    NewSkillLevel = calculate_skill_level(NewCompleted),

    UpdatedProgress = CurrentProgress#{
        completed_tutorials => NewCompleted,
        in_progress_tutorials => NewInProgress,
        total_time_spent => NewTotalTime,
        achievements => NewAchievements,
        skill_level => NewSkillLevel
    },

    State#state{
        user_progress = maps:put(UserId, UpdatedProgress, State#state.user_progress)
    }.

%% @private Calculate time spent in tutorial (minutes)
calculate_time_spent(Session) ->
    StartTime = maps:get(started_at, Session),
    EndTime = maps:get(completion_time, Session, erlang:timestamp()),
    timer:now_diff(EndTime, StartTime) div 60000000.

%% @private Calculate achievements based on completed tutorials
calculate_achievements(CompletedTutorials, Session) ->
    BaseAchievements = [
        {quality_gate, <<"First Quality Gate">>},
        {kanban_wip, <<"Kanban Master">>},
        {andon_event, <<"Stop the Line Champion">>},
        {five_whys, <<"Root Cause Detective">>},
        {complete_workflow, <<"TCPS Expert">>}
    ],

    Earned = [Achievement || {TutorialId, Achievement} <- BaseAchievements,
                              lists:member(TutorialId, CompletedTutorials)],

    %% Add bonus achievements
    Bonus = case maps:get(hints_used, Session) of
        0 -> [<<"No Hints Used">>];
        _ -> []
    end,

    Earned ++ Bonus.

%% @private Calculate skill level based on completed tutorials
calculate_skill_level(CompletedTutorials) ->
    Count = length(CompletedTutorials),
    if
        Count >= 5 -> expert;
        Count >= 3 -> advanced;
        Count >= 1 -> intermediate;
        true -> beginner
    end.

%% @private Calculate recommended learning path for user
calculate_learning_path(UserId, State) ->
    case maps:find(UserId, State#state.user_progress) of
        {ok, Progress} ->
            Completed = maps:get(completed_tutorials, Progress, []),
            AllTutorials = [quality_gate, kanban_wip, andon_event, five_whys, complete_workflow],
            Remaining = AllTutorials -- Completed,

            %% Sort by prerequisites and difficulty
            lists:sort(fun(A, B) ->
                compare_tutorials(A, B, State#state.tutorials)
            end, Remaining);
        error ->
            [quality_gate, kanban_wip, andon_event, five_whys, complete_workflow]
    end.

%% @private Compare tutorials for sorting
compare_tutorials(A, B, Tutorials) ->
    #{prerequisites := PrereqsA} = maps:get(A, Tutorials),
    #{prerequisites := PrereqsB} = maps:get(B, Tutorials),
    length(PrereqsA) =< length(PrereqsB).
