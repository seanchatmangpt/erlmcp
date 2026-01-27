%% @doc Handler functions for erlmcp_server.erl
%% Extracted from erlmcp_server.erl to keep module size <500 LOC.
%% Contains all handle_call, handle_cast, and handle_info implementations.
-module(erlmcp_server_handlers).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([
    handle_add_resource/3,
    handle_add_resource_template/4,
    handle_add_tool/3,
    handle_add_tool_with_schema/4,
    handle_add_prompt/3,
    handle_add_prompt_with_args/4,
    handle_add_prompt_with_args_and_schema/5,
    handle_delete_resource/2,
    handle_delete_tool/2,
    handle_delete_prompt/2,
    handle_subscribe_resource/3,
    handle_unsubscribe_resource/2,
    handle_report_progress/4,
    handle_notify_resource_updated/3,
    handle_notify_resources_changed/1,
    handle_mcp_message/3,
    handle_task_execute/2,
    handle_task_status_update/2,
    handle_task_cancel/2
]).

%% Types
-type state() :: #mcp_server_state{}.

%%====================================================================
%% Resource Management Handlers
%%====================================================================

%% @doc Handle add_resource call
-spec handle_add_resource(binary(), term(), state()) -> {ok, state()}.
handle_add_resource(Uri, Handler, State) ->
    Resource = #mcp_resource{uri = Uri, name = Uri},
    NewResources = maps:put(Uri, {Resource, Handler}, State#mcp_server_state.resources),
    {ok, State#mcp_server_state{resources = NewResources}}.

%% @doc Handle add_resource_template call
-spec handle_add_resource_template(binary(), binary(), term(), state()) -> {ok, state()}.
handle_add_resource_template(UriTemplate, Name, Handler, State) ->
    Template = #mcp_resource_template{uri_template = UriTemplate, name = Name},
    NewTemplates = maps:put(UriTemplate, {Template, Handler}, State#mcp_server_state.resource_templates),
    {ok, State#mcp_server_state{resource_templates = NewTemplates}}.

%% @doc Handle delete_resource call
-spec handle_delete_resource(binary(), state()) -> {ok | {error, not_found}, state()}.
handle_delete_resource(Uri, State) ->
    case maps:is_key(Uri, State#mcp_server_state.resources) of
        true ->
            NewResources = maps:remove(Uri, State#mcp_server_state.resources),
            {ok, State#mcp_server_state{resources = NewResources}};
        false ->
            {{error, not_found}, State}
    end.

%%====================================================================
%% Tool Management Handlers
%%====================================================================

%% @doc Handle add_tool call
-spec handle_add_tool(binary(), term(), state()) -> {ok, state()}.
handle_add_tool(Name, Handler, State) ->
    Tool = #mcp_tool{name = Name},
    NewTools = maps:put(Name, {Tool, Handler, undefined}, State#mcp_server_state.tools),
    {ok, State#mcp_server_state{tools = NewTools}}.

%% @doc Handle add_tool_with_schema call
-spec handle_add_tool_with_schema(binary(), term(), map(), state()) -> {ok, state()}.
handle_add_tool_with_schema(Name, Handler, Schema, State) ->
    Tool = #mcp_tool{name = Name},
    NewTools = maps:put(Name, {Tool, Handler, Schema}, State#mcp_server_state.tools),
    {ok, State#mcp_server_state{tools = NewTools}}.

%% @doc Handle delete_tool call
-spec handle_delete_tool(binary(), state()) -> {ok | {error, not_found}, state()}.
handle_delete_tool(Name, State) ->
    case maps:is_key(Name, State#mcp_server_state.tools) of
        true ->
            NewTools = maps:remove(Name, State#mcp_server_state.tools),
            {ok, State#mcp_server_state{tools = NewTools}};
        false ->
            {{error, not_found}, State}
    end.

%%====================================================================
%% Prompt Management Handlers
%%====================================================================

%% @doc Handle add_prompt call
-spec handle_add_prompt(binary(), term(), state()) -> {ok, state()}.
handle_add_prompt(Name, Handler, State) ->
    Prompt = #mcp_prompt{name = Name},
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#mcp_server_state.prompts),
    {ok, State#mcp_server_state{prompts = NewPrompts}}.

%% @doc Handle add_prompt_with_args call
-spec handle_add_prompt_with_args(binary(), term(), [#mcp_prompt_argument{}], state()) -> {ok, state()}.
handle_add_prompt_with_args(Name, Handler, Arguments, State) ->
    Prompt = #mcp_prompt{name = Name, arguments = Arguments},
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#mcp_server_state.prompts),
    {ok, State#mcp_server_state{prompts = NewPrompts}}.

%% @doc Handle add_prompt_with_args_and_schema call
-spec handle_add_prompt_with_args_and_schema(
    binary(), term(), [#mcp_prompt_argument{}], map() | undefined, state()
) -> {ok, state()}.
handle_add_prompt_with_args_and_schema(Name, Handler, Arguments, InputSchema, State) ->
    Prompt = #mcp_prompt{
        name = Name,
        arguments = Arguments,
        input_schema = InputSchema
    },
    NewPrompts = maps:put(Name, {Prompt, Handler}, State#mcp_server_state.prompts),
    {ok, State#mcp_server_state{prompts = NewPrompts}}.

%% @doc Handle delete_prompt call
-spec handle_delete_prompt(binary(), state()) -> {ok | {error, not_found}, state()}.
handle_delete_prompt(Name, State) ->
    case maps:is_key(Name, State#mcp_server_state.prompts) of
        true ->
            NewPrompts = maps:remove(Name, State#mcp_server_state.prompts),
            {ok, State#mcp_server_state{prompts = NewPrompts}};
        false ->
            {{error, not_found}, State}
    end.

%%====================================================================
%% Subscription Handlers
%%====================================================================

%% @doc Handle subscribe_resource call
-spec handle_subscribe_resource(binary(), pid(), state()) -> {ok, state()}.
handle_subscribe_resource(Uri, Subscriber, State) ->
    Subs = State#mcp_server_state.subscriptions,
    CurrentSet = maps:get(Uri, Subs, sets:new()),
    NewSet = sets:add_element(Subscriber, CurrentSet),
    NewSubs = maps:put(Uri, NewSet, Subs),
    {ok, State#mcp_server_state{subscriptions = NewSubs}}.

%% @doc Handle unsubscribe_resource call
-spec handle_unsubscribe_resource(binary(), state()) -> {ok, state()}.
handle_unsubscribe_resource(Uri, State) ->
    Subs = State#mcp_server_state.subscriptions,
    NewSubs = maps:remove(Uri, Subs),
    {ok, State#mcp_server_state{subscriptions = NewSubs}}.

%%====================================================================
%% Progress and Notification Handlers
%%====================================================================

%% @doc Handle report_progress cast
-spec handle_report_progress(binary() | integer(), float(), float(), state()) -> {ok, state()}.
handle_report_progress(Token, Progress, Total, State) ->
    ProgressToken = #mcp_progress_notification{
        progress_token = #mcp_progress_token{token = Token},
        progress = Progress,
        total = Total
    },
    NewTokens = maps:put(Token, ProgressToken, State#mcp_server_state.progress_tokens),
    {ok, State#mcp_server_state{progress_tokens = NewTokens}}.

%% @doc Handle notify_resource_updated cast
-spec handle_notify_resource_updated(binary(), map(), state()) -> {ok, state()}.
handle_notify_resource_updated(_Uri, _Metadata, State) ->
    %% Uri and metadata can be used for tracking update metadata
    {ok, State}.

%% @doc Handle notify_resources_changed cast
-spec handle_notify_resources_changed(state()) -> {ok, state()}.
handle_notify_resources_changed(State) ->
    %% Trigger list changed event notification
    {ok, State}.

%%====================================================================
%% Message Handling (Hot Path)
%%====================================================================

%% @doc Handle incoming MCP messages (hot path)
-spec handle_mcp_message(binary(), binary(), state()) -> {ok | {error, term()}, state()}.
handle_mcp_message(TransportId, Data, State) ->
    %% Delegate to erlmcp_message_handler for message processing
    case erlmcp_message_handler:process_message(TransportId, Data, State) of
        {ok, NewState} -> {ok, NewState};
        {error, Reason} -> {{error, Reason}, State};
        {reply, Reply, NewState} -> {reply, Reply, NewState}
    end.

%%====================================================================
%% Task Handlers
%%====================================================================

%% @doc Handle task_execute info message
-spec handle_task_execute(term(), state()) -> {ok, state()}.
handle_task_execute(_Task, State) ->
    %% Task execution logic
    {ok, State}.

%% @doc Handle task_status_update info message
-spec handle_task_status_update(term(), state()) -> {ok, state()}.
handle_task_status_update(_Task, State) ->
    %% Task status update logic
    {ok, State}.

%% @doc Handle task_cancel info message
-spec handle_task_cancel(term(), state()) -> {ok, state()}.
handle_task_cancel(_Task, State) ->
    %% Task cancellation logic
    {ok, State}.
