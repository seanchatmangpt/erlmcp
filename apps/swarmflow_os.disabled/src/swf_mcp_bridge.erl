%%%-------------------------------------------------------------------
%%% @doc SwarmFlow MCP Bridge
%%%
%%% Bridges SwarmFlow workflow transitions with MCP tools/resources/prompts.
%%% Provides binding between workflow elements and MCP protocol operations.
%%%
%%% Architecture:
%%% - Tool bindings: Transitions invoke MCP tools via erlmcp_cli_tool
%%% - Resource bindings: Places represent MCP resource states
%%% - Prompt bindings: Prompts provide transition parameters
%%% - Subscription support for resource updates triggering workflow events
%%% - Retry handling with exponential backoff
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(swf_mcp_bridge).

-behaviour(gen_server).

-include("swarmflow.hrl").
-include_lib("erlmcp_core/include/erlmcp.hrl").

%% API
-export([
    start_link/0,
    bind_tool/2,
    bind_resource/2,
    bind_prompt/2,
    unbind_tool/1,
    unbind_resource/1,
    unbind_prompt/1,
    invoke_tool/3,
    read_resource/2,
    write_resource/3,
    subscribe_resource/2,
    unsubscribe_resource/1,
    list_bindings/0,
    list_bindings/1
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

-define(SERVER, ?MODULE).
-define(TOOL_BINDINGS_TABLE, swf_tool_bindings).
-define(RESOURCE_BINDINGS_TABLE, swf_resource_bindings).
-define(PROMPT_BINDINGS_TABLE, swf_prompt_bindings).

%% Default retry policy
-define(DEFAULT_RETRY_POLICY, #swf_retry_policy{
    max_attempts = 3,
    initial_delay_ms = 100,
    max_delay_ms = 10000,
    backoff_multiplier = 2.0,
    retryable_errors = all
}).

%% Prompt binding record (not in swarmflow.hrl)
-record(swf_prompt_binding, {
    transition_id :: binary(),
    prompt_name :: binary(),
    prompt_server :: binary() | undefined,
    argument_mapping :: fun((map()) -> map()),
    timeout_ms :: pos_integer()
}).

%% Internal state
-record(state, {
    tool_bindings_table :: ets:tid(),
    resource_bindings_table :: ets:tid(),
    prompt_bindings_table :: ets:tid(),
    subscriptions :: #{binary() => reference()},
    pending_invocations :: #{reference() => pending_invocation()}
}).

-record(pending_invocation, {
    case_id :: binary(),
    transition_id :: binary(),
    tool_name :: binary(),
    attempt :: pos_integer(),
    max_attempts :: pos_integer(),
    started_at :: integer(),
    timer_ref :: reference() | undefined
}).

-type pending_invocation() :: #pending_invocation{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the MCP bridge server
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Bind MCP tool to workflow transition
%% The binding specifies how case variables map to tool arguments
%% and how tool results map back to case variables.
-spec bind_tool(binary(), #swf_tool_binding{}) -> ok | {error, term()}.
bind_tool(TransitionId, #swf_tool_binding{} = Binding) ->
    gen_server:call(?SERVER, {bind_tool, TransitionId, Binding}).

%% @doc Bind MCP resource to workflow place
%% The binding specifies how resource state is read/written
%% and whether to subscribe for updates.
-spec bind_resource(binary(), #swf_resource_binding{}) -> ok | {error, term()}.
bind_resource(PlaceId, #swf_resource_binding{} = Binding) ->
    gen_server:call(?SERVER, {bind_resource, PlaceId, Binding}).

%% @doc Bind MCP prompt to workflow transition for parameter generation
-spec bind_prompt(binary(), #swf_prompt_binding{}) -> ok | {error, term()}.
bind_prompt(TransitionId, #swf_prompt_binding{} = Binding) ->
    gen_server:call(?SERVER, {bind_prompt, TransitionId, Binding}).

%% @doc Unbind MCP tool from workflow transition
-spec unbind_tool(binary()) -> ok | {error, term()}.
unbind_tool(TransitionId) ->
    gen_server:call(?SERVER, {unbind_tool, TransitionId}).

%% @doc Unbind MCP resource from workflow place
-spec unbind_resource(binary()) -> ok | {error, term()}.
unbind_resource(PlaceId) ->
    gen_server:call(?SERVER, {unbind_resource, PlaceId}).

%% @doc Unbind MCP prompt from workflow transition
-spec unbind_prompt(binary()) -> ok | {error, term()}.
unbind_prompt(TransitionId) ->
    gen_server:call(?SERVER, {unbind_prompt, TransitionId}).

%% @doc Invoke bound MCP tool for a workflow transition
%% Maps case variables to tool arguments, invokes tool, and maps result back.
-spec invoke_tool(binary(), binary(), map()) ->
    {ok, map()} | {error, term()}.
invoke_tool(CaseId, TransitionId, CaseVariables) ->
    gen_server:call(?SERVER, {invoke_tool, CaseId, TransitionId, CaseVariables}, infinity).

%% @doc Read bound MCP resource for a workflow place
%% Maps resource data to case variable format.
-spec read_resource(binary(), map()) -> {ok, map()} | {error, term()}.
read_resource(PlaceId, CaseVariables) ->
    gen_server:call(?SERVER, {read_resource, PlaceId, CaseVariables}).

%% @doc Write to bound MCP resource
%% Maps case variables to resource data format and writes.
-spec write_resource(binary(), map(), map()) -> ok | {error, term()}.
write_resource(PlaceId, CaseVariables, Data) ->
    gen_server:call(?SERVER, {write_resource, PlaceId, CaseVariables, Data}).

%% @doc Subscribe to resource updates for a bound place
%% Updates will trigger workflow events.
-spec subscribe_resource(binary(), pid()) -> ok | {error, term()}.
subscribe_resource(PlaceId, NotifyPid) ->
    gen_server:call(?SERVER, {subscribe_resource, PlaceId, NotifyPid}).

%% @doc Unsubscribe from resource updates
-spec unsubscribe_resource(binary()) -> ok | {error, term()}.
unsubscribe_resource(PlaceId) ->
    gen_server:call(?SERVER, {unsubscribe_resource, PlaceId}).

%% @doc List all bindings
-spec list_bindings() -> {ok, #{tools => [#swf_tool_binding{}],
                                resources => [#swf_resource_binding{}],
                                prompts => [#swf_prompt_binding{}]}}.
list_bindings() ->
    gen_server:call(?SERVER, list_bindings).

%% @doc List bindings by type
-spec list_bindings(tools | resources | prompts) -> {ok, [term()]}.
list_bindings(Type) ->
    gen_server:call(?SERVER, {list_bindings, Type}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),

    ToolTable = ets:new(?TOOL_BINDINGS_TABLE, [
        set,
        protected,
        named_table,
        {keypos, #swf_tool_binding.transition_id},
        {read_concurrency, true}
    ]),

    ResourceTable = ets:new(?RESOURCE_BINDINGS_TABLE, [
        set,
        protected,
        named_table,
        {keypos, #swf_resource_binding.place_id},
        {read_concurrency, true}
    ]),

    PromptTable = ets:new(?PROMPT_BINDINGS_TABLE, [
        set,
        protected,
        named_table,
        {keypos, #swf_prompt_binding.transition_id},
        {read_concurrency, true}
    ]),

    {ok, #state{
        tool_bindings_table = ToolTable,
        resource_bindings_table = ResourceTable,
        prompt_bindings_table = PromptTable,
        subscriptions = #{},
        pending_invocations = #{}
    }}.

handle_call({bind_tool, TransitionId, Binding}, _From, State) ->
    %% Ensure binding has the transition_id set
    FullBinding = Binding#swf_tool_binding{transition_id = TransitionId},
    Result = do_bind_tool(FullBinding, State),
    {reply, Result, State};

handle_call({bind_resource, PlaceId, Binding}, _From, State) ->
    FullBinding = Binding#swf_resource_binding{place_id = PlaceId},
    case do_bind_resource(FullBinding, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({bind_prompt, TransitionId, Binding}, _From, State) ->
    FullBinding = Binding#swf_prompt_binding{transition_id = TransitionId},
    Result = do_bind_prompt(FullBinding, State),
    {reply, Result, State};

handle_call({unbind_tool, TransitionId}, _From, State) ->
    Result = do_unbind_tool(TransitionId, State),
    {reply, Result, State};

handle_call({unbind_resource, PlaceId}, _From, State) ->
    case do_unbind_resource(PlaceId, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unbind_prompt, TransitionId}, _From, State) ->
    Result = do_unbind_prompt(TransitionId, State),
    {reply, Result, State};

handle_call({invoke_tool, CaseId, TransitionId, CaseVariables}, From, State) ->
    case do_invoke_tool(CaseId, TransitionId, CaseVariables, From, State) of
        {async, NewState} ->
            {noreply, NewState};
        {sync, Result, NewState} ->
            {reply, Result, NewState}
    end;

handle_call({read_resource, PlaceId, CaseVariables}, _From, State) ->
    Result = do_read_resource(PlaceId, CaseVariables, State),
    {reply, Result, State};

handle_call({write_resource, PlaceId, CaseVariables, Data}, _From, State) ->
    Result = do_write_resource(PlaceId, CaseVariables, Data, State),
    {reply, Result, State};

handle_call({subscribe_resource, PlaceId, NotifyPid}, _From, State) ->
    case do_subscribe_resource(PlaceId, NotifyPid, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({unsubscribe_resource, PlaceId}, _From, State) ->
    case do_unsubscribe_resource(PlaceId, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call(list_bindings, _From, State) ->
    Result = do_list_bindings(State),
    {reply, {ok, Result}, State};

handle_call({list_bindings, Type}, _From, State) ->
    Result = do_list_bindings(Type, State),
    {reply, {ok, Result}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tool_result, Ref, Result}, State) ->
    NewState = handle_tool_result(Ref, Result, State),
    {noreply, NewState};

handle_info({tool_error, Ref, Error}, State) ->
    NewState = handle_tool_error(Ref, Error, State),
    {noreply, NewState};

handle_info({retry_tool, Ref}, State) ->
    NewState = handle_retry_tool(Ref, State),
    {noreply, NewState};

handle_info({resource_updated, PlaceId, NewResource}, State) ->
    handle_resource_update(PlaceId, NewResource, State),
    {noreply, State};

handle_info({'DOWN', MonitorRef, process, _Pid, _Reason}, State) ->
    %% Handle monitored process death (subscription cleanup)
    NewSubscriptions = maps:filter(
        fun(_PlaceId, Ref) -> Ref =/= MonitorRef end,
        State#state.subscriptions
    ),
    {noreply, State#state{subscriptions = NewSubscriptions}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions - Binding Operations
%%%===================================================================

do_bind_tool(#swf_tool_binding{} = Binding, State) ->
    %% Validate binding has required fields
    case validate_tool_binding(Binding) of
        ok ->
            ets:insert(State#state.tool_bindings_table, Binding),
            ok;
        {error, _} = Error ->
            Error
    end.

do_bind_resource(#swf_resource_binding{} = Binding, State) ->
    case validate_resource_binding(Binding) of
        ok ->
            ets:insert(State#state.resource_bindings_table, Binding),
            %% Auto-subscribe if binding specifies subscription
            case Binding#swf_resource_binding.subscribe of
                true ->
                    %% Subscribe using self() as notification target initially
                    do_subscribe_resource(
                        Binding#swf_resource_binding.place_id,
                        self(),
                        State
                    );
                false ->
                    {ok, State}
            end;
        {error, _} = Error ->
            Error
    end.

do_bind_prompt(#swf_prompt_binding{} = Binding, State) ->
    case validate_prompt_binding(Binding) of
        ok ->
            ets:insert(State#state.prompt_bindings_table, Binding),
            ok;
        {error, _} = Error ->
            Error
    end.

do_unbind_tool(TransitionId, State) ->
    case ets:lookup(State#state.tool_bindings_table, TransitionId) of
        [_] ->
            ets:delete(State#state.tool_bindings_table, TransitionId),
            ok;
        [] ->
            {error, not_found}
    end.

do_unbind_resource(PlaceId, State) ->
    case ets:lookup(State#state.resource_bindings_table, PlaceId) of
        [_Binding] ->
            %% Unsubscribe if subscribed
            NewState = case maps:find(PlaceId, State#state.subscriptions) of
                {ok, _Ref} ->
                    {ok, S} = do_unsubscribe_resource(PlaceId, State),
                    S;
                error ->
                    State
            end,
            ets:delete(NewState#state.resource_bindings_table, PlaceId),
            {ok, NewState};
        [] ->
            {error, not_found}
    end.

do_unbind_prompt(TransitionId, State) ->
    case ets:lookup(State#state.prompt_bindings_table, TransitionId) of
        [_] ->
            ets:delete(State#state.prompt_bindings_table, TransitionId),
            ok;
        [] ->
            {error, not_found}
    end.

%%%===================================================================
%%% Internal functions - Tool Invocation
%%%===================================================================

do_invoke_tool(CaseId, TransitionId, CaseVariables, From, State) ->
    case ets:lookup(State#state.tool_bindings_table, TransitionId) of
        [#swf_tool_binding{
            tool_name = ToolName,
            input_mapping = InputMapping,
            output_mapping = OutputMapping,
            timeout_ms = Timeout,
            retry_policy = RetryPolicy
        }] ->
            %% Map case variables to tool arguments
            ToolArgs = apply_input_mapping(InputMapping, CaseVariables),

            %% Get retry policy (use default if not specified)
            Policy = case RetryPolicy of
                undefined -> ?DEFAULT_RETRY_POLICY;
                P -> P
            end,

            %% Execute with retry support
            execute_tool_with_retry(
                CaseId,
                TransitionId,
                ToolName,
                ToolArgs,
                OutputMapping,
                Timeout,
                Policy,
                From,
                State
            );
        [] ->
            {sync, {error, {binding_not_found, TransitionId}}, State}
    end.

execute_tool_with_retry(CaseId, TransitionId, ToolName, Args, OutputMapping,
                        Timeout, Policy, From, State) ->
    Ref = make_ref(),

    %% Create pending invocation record
    Pending = #pending_invocation{
        case_id = CaseId,
        transition_id = TransitionId,
        tool_name = ToolName,
        attempt = 1,
        max_attempts = Policy#swf_retry_policy.max_attempts,
        started_at = erlang:system_time(millisecond)
    },

    %% Store pending invocation with From for async reply
    NewPending = maps:put(Ref, {Pending, Args, OutputMapping, Timeout, Policy, From},
                          State#state.pending_invocations),
    NewState = State#state{pending_invocations = NewPending},

    %% Spawn tool execution
    Self = self(),
    spawn_link(fun() ->
        Result = erlmcp_cli_tool:call_tool(ToolName, Args, Timeout),
        case Result of
            {ok, ToolResult} ->
                Self ! {tool_result, Ref, ToolResult};
            {error, Reason} ->
                Self ! {tool_error, Ref, Reason}
        end
    end),

    {async, NewState}.

handle_tool_result(Ref, ToolResult, State) ->
    case maps:find(Ref, State#state.pending_invocations) of
        {ok, {_Pending, _Args, OutputMapping, _Timeout, _Policy, From}} ->
            %% Map tool result to case variables
            MappedResult = apply_output_mapping(OutputMapping, ToolResult),
            gen_server:reply(From, {ok, MappedResult}),
            NewPending = maps:remove(Ref, State#state.pending_invocations),
            State#state{pending_invocations = NewPending};
        error ->
            %% Unknown reference, ignore
            State
    end.

handle_tool_error(Ref, Error, State) ->
    case maps:find(Ref, State#state.pending_invocations) of
        {ok, {Pending, Args, OutputMapping, Timeout, Policy, From}} ->
            %% Check if we should retry
            case should_retry(Error, Pending, Policy) of
                {true, Delay} ->
                    %% Schedule retry
                    TimerRef = erlang:send_after(Delay, self(), {retry_tool, Ref}),
                    NewPending = Pending#pending_invocation{
                        attempt = Pending#pending_invocation.attempt + 1,
                        timer_ref = TimerRef
                    },
                    UpdatedPendings = maps:put(
                        Ref,
                        {NewPending, Args, OutputMapping, Timeout, Policy, From},
                        State#state.pending_invocations
                    ),
                    State#state{pending_invocations = UpdatedPendings};
                false ->
                    %% No more retries, return error
                    gen_server:reply(From, {error, Error}),
                    NewPending = maps:remove(Ref, State#state.pending_invocations),
                    State#state{pending_invocations = NewPending}
            end;
        error ->
            State
    end.

handle_retry_tool(Ref, State) ->
    case maps:find(Ref, State#state.pending_invocations) of
        {ok, {Pending, Args, OutputMapping, Timeout, Policy, From}} ->
            %% Retry the tool invocation
            ToolName = Pending#pending_invocation.tool_name,
            Self = self(),
            spawn_link(fun() ->
                Result = erlmcp_cli_tool:call_tool(ToolName, Args, Timeout),
                case Result of
                    {ok, ToolResult} ->
                        Self ! {tool_result, Ref, ToolResult};
                    {error, Reason} ->
                        Self ! {tool_error, Ref, Reason}
                end
            end),
            State;
        error ->
            State
    end.

should_retry(Error, Pending, Policy) ->
    #pending_invocation{attempt = Attempt} = Pending,
    #swf_retry_policy{
        max_attempts = MaxAttempts,
        initial_delay_ms = InitialDelay,
        max_delay_ms = MaxDelay,
        backoff_multiplier = Multiplier,
        retryable_errors = RetryableErrors
    } = Policy,

    %% Check if error is retryable
    IsRetryable = case RetryableErrors of
        all -> true;
        Errors when is_list(Errors) ->
            ErrorType = case Error of
                {Type, _} -> Type;
                Type when is_atom(Type) -> Type;
                _ -> unknown
            end,
            lists:member(ErrorType, Errors)
    end,

    case IsRetryable andalso Attempt < MaxAttempts of
        true ->
            %% Calculate delay with exponential backoff
            Delay = min(
                round(InitialDelay * math:pow(Multiplier, Attempt - 1)),
                MaxDelay
            ),
            {true, Delay};
        false ->
            false
    end.

%%%===================================================================
%%% Internal functions - Resource Operations
%%%===================================================================

do_read_resource(PlaceId, CaseVariables, State) ->
    case ets:lookup(State#state.resource_bindings_table, PlaceId) of
        [#swf_resource_binding{
            resource_uri = ResourceUri,
            read_mapping = ReadMapping
        }] ->
            %% Read resource via MCP
            case erlmcp_cli_resource:get_resource(ResourceUri, undefined) of
                {ok, ResourceData} ->
                    %% Map resource data to case variable format
                    MappedData = apply_read_mapping(ReadMapping, ResourceData),
                    {ok, maps:merge(CaseVariables, MappedData)};
                {error, Reason} ->
                    {error, {resource_read_failed, Reason}}
            end;
        [] ->
            {error, {binding_not_found, PlaceId}}
    end.

do_write_resource(PlaceId, CaseVariables, Data, State) ->
    case ets:lookup(State#state.resource_bindings_table, PlaceId) of
        [#swf_resource_binding{
            resource_uri = ResourceUri,
            write_mapping = WriteMapping
        }] ->
            %% Map case variables to resource format
            ResourceData = apply_write_mapping(WriteMapping, maps:merge(CaseVariables, Data)),

            %% Write resource via MCP
            case erlmcp_cli_resource:update_resource(ResourceUri, undefined, ResourceData) of
                ok ->
                    ok;
                {error, Reason} ->
                    {error, {resource_write_failed, Reason}}
            end;
        [] ->
            {error, {binding_not_found, PlaceId}}
    end.

do_subscribe_resource(PlaceId, NotifyPid, State) ->
    case ets:lookup(State#state.resource_bindings_table, PlaceId) of
        [#swf_resource_binding{resource_uri = ResourceUri}] ->
            %% Subscribe via MCP
            case erlmcp_cli_resource:subscribe_resource(ResourceUri, undefined) of
                ok ->
                    %% Monitor the notification process
                    MonitorRef = erlang:monitor(process, NotifyPid),
                    NewSubscriptions = maps:put(PlaceId, MonitorRef, State#state.subscriptions),
                    {ok, State#state{subscriptions = NewSubscriptions}};
                {error, Reason} ->
                    {error, {subscription_failed, Reason}}
            end;
        [] ->
            {error, {binding_not_found, PlaceId}}
    end.

do_unsubscribe_resource(PlaceId, State) ->
    case maps:find(PlaceId, State#state.subscriptions) of
        {ok, MonitorRef} ->
            erlang:demonitor(MonitorRef, [flush]),
            case ets:lookup(State#state.resource_bindings_table, PlaceId) of
                [#swf_resource_binding{resource_uri = ResourceUri}] ->
                    erlmcp_cli_resource:unsubscribe_resource(ResourceUri, undefined);
                [] ->
                    ok
            end,
            NewSubscriptions = maps:remove(PlaceId, State#state.subscriptions),
            {ok, State#state{subscriptions = NewSubscriptions}};
        error ->
            {error, not_subscribed}
    end.

handle_resource_update(PlaceId, NewResource, State) ->
    case ets:lookup(State#state.resource_bindings_table, PlaceId) of
        [#swf_resource_binding{read_mapping = ReadMapping}] ->
            %% Map resource update to event data
            EventData = apply_read_mapping(ReadMapping, NewResource),
            %% Notify workflow engine about resource update
            notify_resource_update(PlaceId, EventData);
        [] ->
            ok
    end.

notify_resource_update(PlaceId, EventData) ->
    %% Publish event for workflow engine to consume
    %% This would integrate with swf_event_log or similar
    Event = #{
        type => resource_updated,
        place_id => PlaceId,
        data => EventData,
        timestamp => erlang:system_time(millisecond)
    },
    %% Use gproc for event publication if available
    catch gproc:send({p, l, {swf_resource_event, PlaceId}}, Event),
    ok.

%%%===================================================================
%%% Internal functions - Listing
%%%===================================================================

do_list_bindings(State) ->
    #{
        tools => ets:tab2list(State#state.tool_bindings_table),
        resources => ets:tab2list(State#state.resource_bindings_table),
        prompts => ets:tab2list(State#state.prompt_bindings_table)
    }.

do_list_bindings(tools, State) ->
    ets:tab2list(State#state.tool_bindings_table);
do_list_bindings(resources, State) ->
    ets:tab2list(State#state.resource_bindings_table);
do_list_bindings(prompts, State) ->
    ets:tab2list(State#state.prompt_bindings_table).

%%%===================================================================
%%% Internal functions - Validation
%%%===================================================================

validate_tool_binding(#swf_tool_binding{
    transition_id = TransitionId,
    tool_name = ToolName,
    input_mapping = InputMapping,
    output_mapping = OutputMapping,
    timeout_ms = Timeout
}) ->
    Errors = [],
    Errors1 = case is_binary(TransitionId) andalso byte_size(TransitionId) > 0 of
        true -> Errors;
        false -> [{invalid_transition_id, TransitionId} | Errors]
    end,
    Errors2 = case is_binary(ToolName) andalso byte_size(ToolName) > 0 of
        true -> Errors1;
        false -> [{invalid_tool_name, ToolName} | Errors1]
    end,
    Errors3 = case is_function(InputMapping, 1) of
        true -> Errors2;
        false -> [{invalid_input_mapping, not_a_function} | Errors2]
    end,
    Errors4 = case is_function(OutputMapping, 1) of
        true -> Errors3;
        false -> [{invalid_output_mapping, not_a_function} | Errors3]
    end,
    Errors5 = case is_integer(Timeout) andalso Timeout > 0 of
        true -> Errors4;
        false -> [{invalid_timeout, Timeout} | Errors4]
    end,
    case Errors5 of
        [] -> ok;
        _ -> {error, {validation_failed, Errors5}}
    end.

validate_resource_binding(#swf_resource_binding{
    place_id = PlaceId,
    resource_uri = ResourceUri,
    read_mapping = ReadMapping,
    write_mapping = WriteMapping
}) ->
    Errors = [],
    Errors1 = case is_binary(PlaceId) andalso byte_size(PlaceId) > 0 of
        true -> Errors;
        false -> [{invalid_place_id, PlaceId} | Errors]
    end,
    Errors2 = case is_binary(ResourceUri) andalso byte_size(ResourceUri) > 0 of
        true -> Errors1;
        false -> [{invalid_resource_uri, ResourceUri} | Errors1]
    end,
    Errors3 = case is_function(ReadMapping, 1) of
        true -> Errors2;
        false -> [{invalid_read_mapping, not_a_function} | Errors2]
    end,
    Errors4 = case is_function(WriteMapping, 1) of
        true -> Errors3;
        false -> [{invalid_write_mapping, not_a_function} | Errors3]
    end,
    case Errors4 of
        [] -> ok;
        _ -> {error, {validation_failed, Errors4}}
    end.

validate_prompt_binding(#swf_prompt_binding{
    transition_id = TransitionId,
    prompt_name = PromptName,
    argument_mapping = ArgumentMapping,
    timeout_ms = Timeout
}) ->
    Errors = [],
    Errors1 = case is_binary(TransitionId) andalso byte_size(TransitionId) > 0 of
        true -> Errors;
        false -> [{invalid_transition_id, TransitionId} | Errors]
    end,
    Errors2 = case is_binary(PromptName) andalso byte_size(PromptName) > 0 of
        true -> Errors1;
        false -> [{invalid_prompt_name, PromptName} | Errors1]
    end,
    Errors3 = case is_function(ArgumentMapping, 1) of
        true -> Errors2;
        false -> [{invalid_argument_mapping, not_a_function} | Errors2]
    end,
    Errors4 = case is_integer(Timeout) andalso Timeout > 0 of
        true -> Errors3;
        false -> [{invalid_timeout, Timeout} | Errors3]
    end,
    case Errors4 of
        [] -> ok;
        _ -> {error, {validation_failed, Errors4}}
    end.

%%%===================================================================
%%% Internal functions - Mapping Helpers
%%%===================================================================

%% @doc Apply input mapping function to transform case variables to tool arguments
apply_input_mapping(MappingFun, CaseVariables) when is_function(MappingFun, 1) ->
    try
        MappingFun(CaseVariables)
    catch
        _:_ -> CaseVariables
    end;
apply_input_mapping(_, CaseVariables) ->
    CaseVariables.

%% @doc Apply output mapping function to transform tool result to case variables
apply_output_mapping(MappingFun, ToolResult) when is_function(MappingFun, 1) ->
    try
        MappingFun(ToolResult)
    catch
        _:_ -> #{}
    end;
apply_output_mapping(_, ToolResult) when is_map(ToolResult) ->
    ToolResult;
apply_output_mapping(_, _) ->
    #{}.

%% @doc Apply read mapping function to transform resource data
apply_read_mapping(MappingFun, ResourceData) when is_function(MappingFun, 1) ->
    try
        MappingFun(ResourceData)
    catch
        _:_ -> #{}
    end;
apply_read_mapping(_, ResourceData) when is_map(ResourceData) ->
    ResourceData;
apply_read_mapping(_, _) ->
    #{}.

%% @doc Apply write mapping function to transform case variables to resource format
apply_write_mapping(MappingFun, Data) when is_function(MappingFun, 1) ->
    try
        MappingFun(Data)
    catch
        _:_ -> Data
    end;
apply_write_mapping(_, Data) when is_map(Data) ->
    Data;
apply_write_mapping(_, Data) ->
    #{result => Data}.
