%% @doc Telemetry integration for erlmcp_server operations.
%% Provides standardized telemetry events for MCP protocol operations.
%%
%% Events are emitted using the telemetry library (ecosystem standard).
%% All events follow the naming convention: [erlmcp, Category, Operation]
%%
%% Available Events:
%% - [erlmcp, tool, call] - Tool execution with duration and status
%% - [erlmcp, resource, read] - Resource read with duration and status
%% - [erlmcp, prompt, render] - Prompt rendering with duration and status
%% - [erlmcp, subscription, add] - Subscription creation with metadata
%%
%% Measurements:
%% - duration_us: Operation duration in microseconds
%% - count: Event count (always 1 for individual operations)
%%
%% Metadata:
%% - tool_name: Name of the tool (tools/call)
%% - resource_uri: URI of the resource (resources/read)
%% - prompt_name: Name of the prompt (prompts/get)
%% - subscription_uri: URI being subscribed to (subscriptions/add)
%% - status: ok | error
%% - error_reason: Error details (when status = error)
-module(erlmcp_telemetry).

%% API
-export([
    emit_tool_call/3,
    emit_resource_read/3,
    emit_prompt_render/3,
    emit_subscription_add/2,
    attach_default_handler/0,
    detach_default_handler/0
]).

%% Default handler callbacks
-export([
    handle_telemetry_event/4
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Emit telemetry event for tool call.
-spec emit_tool_call(binary(), non_neg_integer(), ok | {error, term()}) -> ok.
emit_tool_call(ToolName, DurationUs, Status) ->
    {StatusAtom, Metadata} = case Status of
        ok ->
            {ok, #{tool_name => ToolName, status => ok}};
        {error, Reason} ->
            {error, #{tool_name => ToolName, status => error, error_reason => Reason}}
    end,
    telemetry:execute(
        [erlmcp, tool, call],
        #{duration_us => DurationUs, count => 1},
        Metadata#{status_atom => StatusAtom}
    ).

%% @doc Emit telemetry event for resource read.
-spec emit_resource_read(binary(), non_neg_integer(), ok | {error, term()}) -> ok.
emit_resource_read(ResourceUri, DurationUs, Status) ->
    {StatusAtom, Metadata} = case Status of
        ok ->
            {ok, #{resource_uri => ResourceUri, status => ok}};
        {error, Reason} ->
            {error, #{resource_uri => ResourceUri, status => error, error_reason => Reason}}
    end,
    telemetry:execute(
        [erlmcp, resource, read],
        #{duration_us => DurationUs, count => 1},
        Metadata#{status_atom => StatusAtom}
    ).

%% @doc Emit telemetry event for prompt rendering.
-spec emit_prompt_render(binary(), non_neg_integer(), ok | {error, term()}) -> ok.
emit_prompt_render(PromptName, DurationUs, Status) ->
    {StatusAtom, Metadata} = case Status of
        ok ->
            {ok, #{prompt_name => PromptName, status => ok}};
        {error, Reason} ->
            {error, #{prompt_name => PromptName, status => error, error_reason => Reason}}
    end,
    telemetry:execute(
        [erlmcp, prompt, render],
        #{duration_us => DurationUs, count => 1},
        Metadata#{status_atom => StatusAtom}
    ).

%% @doc Emit telemetry event for subscription add.
-spec emit_subscription_add(binary(), pid()) -> ok.
emit_subscription_add(Uri, SubscriberPid) ->
    telemetry:execute(
        [erlmcp, subscription, add],
        #{count => 1},
        #{subscription_uri => Uri, subscriber_pid => SubscriberPid}
    ).

%% @doc Attach default telemetry handler that logs events.
%% Useful for debugging and monitoring.
-spec attach_default_handler() -> ok | {error, already_exists}.
attach_default_handler() ->
    Events = [
        [erlmcp, tool, call],
        [erlmcp, resource, read],
        [erlmcp, prompt, render],
        [erlmcp, subscription, add]
    ],
    telemetry:attach_many(
        <<"erlmcp-default-handler">>,
        Events,
        fun ?MODULE:handle_telemetry_event/4,
        #{}
    ).

%% @doc Detach default telemetry handler.
-spec detach_default_handler() -> ok | {error, not_found}.
detach_default_handler() ->
    telemetry:detach(<<"erlmcp-default-handler">>).

%%====================================================================
%% Default Handler Implementation
%%====================================================================

%% @doc Default telemetry event handler that logs events.
-spec handle_telemetry_event(
    [atom()],
    #{atom() => number()},
    #{atom() => term()},
    term()
) -> ok.
handle_telemetry_event([erlmcp, tool, call], Measurements, Metadata, _Config) ->
    ToolName = maps:get(tool_name, Metadata, <<"unknown">>),
    Duration = maps:get(duration_us, Measurements, 0),
    Status = maps:get(status, Metadata, unknown),
    logger:debug("Tool call: ~s (~p) completed in ~p us", [ToolName, Status, Duration]),
    ok;
handle_telemetry_event([erlmcp, resource, read], Measurements, Metadata, _Config) ->
    ResourceUri = maps:get(resource_uri, Metadata, <<"unknown">>),
    Duration = maps:get(duration_us, Measurements, 0),
    Status = maps:get(status, Metadata, unknown),
    logger:debug("Resource read: ~s (~p) completed in ~p us", [ResourceUri, Status, Duration]),
    ok;
handle_telemetry_event([erlmcp, prompt, render], Measurements, Metadata, _Config) ->
    PromptName = maps:get(prompt_name, Metadata, <<"unknown">>),
    Duration = maps:get(duration_us, Measurements, 0),
    Status = maps:get(status, Metadata, unknown),
    logger:debug("Prompt render: ~s (~p) completed in ~p us", [PromptName, Status, Duration]),
    ok;
handle_telemetry_event([erlmcp, subscription, add], _Measurements, Metadata, _Config) ->
    Uri = maps:get(subscription_uri, Metadata, <<"unknown">>),
    logger:debug("Subscription added: ~s", [Uri]),
    ok;
handle_telemetry_event(_Event, _Measurements, _Metadata, _Config) ->
    ok.
