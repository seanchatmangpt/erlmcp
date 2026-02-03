%%% @doc PQC Case Registry and PubSub
%%%
%%% Manages active Case processes with:
%%% - ETS-based registry for CaseId -> Pid lookups
%%% - pg-based pubsub for Case event streaming
%%% - Automatic cleanup on Case process termination
%%%
%%% Architecture:
%%% - ETS table 'pqc_case_registry_tab' for O(1) lookups
%%% - pg scope 'pqc_case_pg' for distributed pubsub
%%% - Process monitoring for automatic cleanup
%%% - Chicago School TDD: real processes, no mocks
%%%
%%% PubSub enables:
%%% - A2A streaming: subscribe to Case, forward events as SSE
%%% - MCP notifications: subscribe to Case, send JSON-RPC notifications
%%%
%%% @end
-module(pqc_case_registry).
-behaviour(gen_server).

-include("pqchain.hrl").

%% API
-export([
    start_link/0,
    ensure_case/3,
    lookup/1,
    subscribe/1,
    unsubscribe/1,
    publish/2,
    list_cases/0,
    get_case_count/0
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

%%% ============================================================================
%%% Types
%%% ============================================================================

-record(state, {
    table :: ets:table(),
    monitors :: #{reference() => binary()}  % MonRef -> CaseId
}).

-type case_id() :: binary().
-type case_event() ::
    {status, map()} |
    {artifact, map()} |
    {snapshot, map()} |
    {case_terminated, term()}.

-export_type([case_id/0, case_event/0]).

%%% ============================================================================
%%% Constants
%%% ============================================================================

-define(TABLE, pqc_case_registry_tab).
-define(PG_SCOPE, pqc_case_pg).
-define(TIMEOUT, 5000).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Start the Case registry
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Ensure a Case exists, creating it if necessary
%%
%% If the Case already exists, returns its Pid.
%% If not, creates a new Case process with the provided Net and SigningKey.
%%
%% @end
-spec ensure_case(case_id(), term(), term()) ->
    {ok, pid()} | {error, term()}.
ensure_case(CaseId, Net, SigningKey) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {ensure_case, CaseId, Net, SigningKey}, ?TIMEOUT).

%% @doc Lookup a Case process by ID
-spec lookup(case_id()) -> {ok, pid()} | {error, not_found}.
lookup(CaseId) when is_binary(CaseId) ->
    case ets:lookup(?TABLE, CaseId) of
        [{CaseId, Pid}] ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false -> {error, not_found}
            end;
        [] ->
            {error, not_found}
    end.

%% @doc Subscribe to Case events
%%
%% Joins the pg group for this Case. Events will be sent as:
%% {pqc_case_event, CaseId, Event}
%%
%% @end
-spec subscribe(case_id()) -> ok | {error, term()}.
subscribe(CaseId) when is_binary(CaseId) ->
    try
        ok = pg:join(?PG_SCOPE, {case, CaseId}, self()),
        ok
    catch
        error:badarg ->
            {error, pg_not_started}
    end.

%% @doc Unsubscribe from Case events
-spec unsubscribe(case_id()) -> ok | {error, term()}.
unsubscribe(CaseId) when is_binary(CaseId) ->
    try
        ok = pg:leave(?PG_SCOPE, {case, CaseId}, self()),
        ok
    catch
        error:badarg ->
            {error, pg_not_started}
    end.

%% @doc Publish an event to all Case subscribers
%%
%% Events are wrapped as {pqc_case_event, CaseId, Event} before delivery.
%%
%% @end
-spec publish(case_id(), case_event()) -> ok.
publish(CaseId, Event) when is_binary(CaseId) ->
    gen_server:cast(?MODULE, {publish, CaseId, Event}).

%% @doc List all active Case IDs
-spec list_cases() -> [case_id()].
list_cases() ->
    gen_server:call(?MODULE, list_cases, ?TIMEOUT).

%% @doc Get the count of active Cases
-spec get_case_count() -> non_neg_integer().
get_case_count() ->
    gen_server:call(?MODULE, get_case_count, ?TIMEOUT).

%%% ============================================================================
%%% gen_server Callbacks
%%% ============================================================================

%% @private
init([]) ->
    %% Create ETS table for Case registry
    Table = ets:new(?TABLE, [
        named_table,
        public,
        set,
        {read_concurrency, true},
        {keypos, 1}
    ]),

    %% Start pg scope for pubsub
    case pg:start_link(?PG_SCOPE) of
        {ok, _Pid} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            logger:error("Failed to start pg scope: ~p", [Reason])
    end,

    State = #state{
        table = Table,
        monitors = #{}
    },

    {ok, State}.

%% @private
handle_call({ensure_case, CaseId, Net, SigningKey}, _From, State) ->
    case lookup(CaseId) of
        {ok, Pid} ->
            %% Case already exists
            {reply, {ok, Pid}, State};
        {error, not_found} ->
            %% Create new Case process
            case start_case_process(CaseId, Net, SigningKey) of
                {ok, Pid} ->
                    %% Register in ETS
                    true = ets:insert(?TABLE, {CaseId, Pid}),

                    %% Monitor the Case process
                    MonRef = monitor(process, Pid),
                    NewMonitors = maps:put(MonRef, CaseId, State#state.monitors),

                    {reply, {ok, Pid}, State#state{monitors = NewMonitors}};
                {error, Reason} = Error ->
                    {reply, Error, State}
            end
    end;

handle_call(list_cases, _From, State) ->
    CaseIds = ets:foldl(
        fun({CaseId, Pid}, Acc) ->
            case is_process_alive(Pid) of
                true -> [CaseId | Acc];
                false -> Acc
            end
        end,
        [],
        State#state.table
    ),
    {reply, CaseIds, State};

handle_call(get_case_count, _From, State) ->
    Count = ets:info(State#state.table, size),
    {reply, Count, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({publish, CaseId, Event}, State) ->
    %% Get all subscribers for this Case
    Members = pg:get_members(?PG_SCOPE, {case, CaseId}),

    %% Send event to all subscribers
    Message = {pqc_case_event, CaseId, Event},
    lists:foreach(
        fun(Pid) ->
            Pid ! Message
        end,
        Members
    ),

    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({'DOWN', MonRef, process, Pid, Reason}, State) ->
    %% Case process terminated
    case maps:take(MonRef, State#state.monitors) of
        {CaseId, NewMonitors} ->
            %% Remove from ETS
            true = ets:delete(?TABLE, CaseId),

            %% Publish termination event to subscribers
            publish(CaseId, {case_terminated, Reason}),

            logger:info("Case ~s terminated: ~p", [CaseId, Reason]),

            {noreply, State#state{monitors = NewMonitors}};
        error ->
            %% Unknown monitor (shouldn't happen)
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% ============================================================================
%%% Internal Functions
%%% ============================================================================

%% @private
%% @doc Start a Case process
%%
%% In a real implementation, this would start a pqc_case_fsm or similar.
%% For now, this is a placeholder that would integrate with the actual
%% Case supervisor.
%%
%% @end
-spec start_case_process(case_id(), term(), term()) ->
    {ok, pid()} | {error, term()}.
start_case_process(CaseId, Net, SigningKey) ->
    %% TODO: This should delegate to pqc_case_sup:start_case/3
    %% For now, return a placeholder error
    case whereis(pqc_case_sup) of
        undefined ->
            %% Supervisor not available
            {error, case_supervisor_not_started};
        SupPid when is_pid(SupPid) ->
            %% In production, this would be:
            %% pqc_case_sup:start_case(CaseId, Net, SigningKey)
            %%
            %% For now, we'll create a minimal process placeholder
            %% This should be replaced with actual Case FSM integration
            Pid = spawn(fun() ->
                case_placeholder_loop(CaseId, Net, SigningKey)
            end),
            {ok, Pid}
    end.

%% @private
%% @doc Placeholder loop for Case process
%%
%% This should be replaced with actual pqc_case_fsm integration.
%%
%% @end
-spec case_placeholder_loop(case_id(), term(), term()) -> no_return().
case_placeholder_loop(CaseId, _Net, _SigningKey) ->
    receive
        stop ->
            exit(normal);
        _Other ->
            case_placeholder_loop(CaseId, _Net, _SigningKey)
    end.
