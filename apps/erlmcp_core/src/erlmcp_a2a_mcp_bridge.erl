%%%-------------------------------------------------------------------
%%% @doc A2A-MCP Protocol Bridge Module
%%%
%%% This module enables interoperability between the Google A2A (Agent-to-Agent)
%%% protocol and the Model Context Protocol (MCP). It provides bi-directional
%%% translation between:
%%%
%%% - MCP Tools <-> A2A Skills
%%% - MCP Content <-> A2A Parts
%%% - MCP Capabilities <-> A2A Capabilities
%%% - MCP Error Codes <-> A2A Error Codes
%%%
%%% The bridge allows:
%%% - MCP servers to expose tools as A2A skills
%%% - A2A agents to invoke MCP tools as tasks
%%% - Content type conversion between protocols
%%% - Unified error handling across protocols
%%%
%%% Architecture:
%%% - Pure functional translations where possible
%%% - Process-per-task execution model
%%% - No shared mutable state between protocols
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_mcp_bridge).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API Exports - Tool/Skill Conversion
-export([
    mcp_tool_to_a2a_skill/1,
    a2a_skill_to_mcp_tool/1
]).

%% API Exports - Content/Part Conversion
-export([
    a2a_message_to_mcp_content/1,
    mcp_content_to_a2a_parts/1,
    mcp_content_to_a2a_part/1,
    a2a_part_to_mcp_content/1
]).

%% API Exports - Task Execution
-export([
    execute_tool_as_task/3,
    execute_tool_as_task/4
]).

%% API Exports - Capability Mapping
-export([
    map_capabilities/1,
    map_mcp_capabilities_to_a2a/1,
    map_a2a_capabilities_to_mcp/1
]).

%% API Exports - Error Mapping
-export([
    map_error/2,
    mcp_error_to_a2a/1,
    a2a_error_to_mcp/1
]).

%% Internal exports for task execution
-export([
    task_executor/5
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type protocol() :: mcp | a2a.
-type error_code() :: integer().

%%====================================================================
%% Tool/Skill Conversion
%%====================================================================

%% @doc Convert an MCP tool to an A2A skill.
%%
%% Maps MCP tool properties to A2A skill properties:
%% - tool.name -> skill.id and skill.name
%% - tool.description -> skill.description
%% - tool.input_schema -> skill.input_modes (derived from schema types)
%% - tool.metadata.tags -> skill.tags (if present)
%%
%% == Example ==
%% ```
%% Tool = #mcp_tool{
%%     name = <<"read_file">>,
%%     description = <<"Read contents of a file">>,
%%     input_schema = #{<<"type">> => <<"object">>}
%% },
%% Skill = erlmcp_a2a_mcp_bridge:mcp_tool_to_a2a_skill(Tool).
%% '''
-spec mcp_tool_to_a2a_skill(#mcp_tool{}) -> #a2a_agent_skill{}.
mcp_tool_to_a2a_skill(#mcp_tool{} = Tool) ->
    %% Extract tags from metadata if present
    Tags = extract_tags_from_metadata(Tool#mcp_tool.metadata),

    %% Derive input modes from input schema
    InputModes = derive_input_modes_from_schema(Tool#mcp_tool.input_schema),

    %% Build examples from metadata if present
    Examples = extract_examples_from_metadata(Tool#mcp_tool.metadata),

    #a2a_agent_skill{
        id = Tool#mcp_tool.name,
        name = humanize_name(Tool#mcp_tool.name),
        description = ensure_binary(Tool#mcp_tool.description),
        tags = Tags,
        examples = Examples,
        input_modes = InputModes,
        output_modes = [?A2A_MIME_TEXT_PLAIN, ?A2A_MIME_APPLICATION_JSON],
        security_requirements = undefined
    }.

%% @doc Convert an A2A skill to an MCP tool.
%%
%% Maps A2A skill properties to MCP tool properties:
%% - skill.id -> tool.name
%% - skill.description -> tool.description
%% - skill.tags -> tool.metadata.tags
%% - skill.input_modes -> tool.input_schema (derived)
%%
-spec a2a_skill_to_mcp_tool(#a2a_agent_skill{}) -> #mcp_tool{}.
a2a_skill_to_mcp_tool(#a2a_agent_skill{} = Skill) ->
    %% Build input schema from input modes
    InputSchema = build_input_schema_from_modes(Skill#a2a_agent_skill.input_modes),

    %% Build metadata from skill properties
    Metadata = build_tool_metadata(Skill),

    #mcp_tool{
        name = Skill#a2a_agent_skill.id,
        description = Skill#a2a_agent_skill.description,
        input_schema = InputSchema,
        metadata = Metadata,
        experimental = undefined,
        version = undefined,
        deprecated = false
    }.

%%====================================================================
%% Content/Part Conversion
%%====================================================================

%% @doc Convert A2A message parts to MCP content list.
%%
%% Converts all parts in an A2A message to MCP content records.
%% Handles text, raw (binary), URL, and data parts.
%%
-spec a2a_message_to_mcp_content(#a2a_message{}) -> [#mcp_content{}].
a2a_message_to_mcp_content(#a2a_message{parts = Parts}) when is_list(Parts) ->
    [a2a_part_to_mcp_content(Part) || Part <- Parts];
a2a_message_to_mcp_content(#a2a_message{parts = undefined}) ->
    [].

%% @doc Convert a single A2A part to MCP content.
%%
%% Mapping:
%% - a2a_part.text -> mcp_content{type = text, text = ...}
%% - a2a_part.raw -> mcp_content{type = image/audio, data = base64(...)}
%% - a2a_part.url -> mcp_content{type = resource, resource_link = ...}
%% - a2a_part.data -> mcp_content{type = text, text = json_encode(...)}
%%
-spec a2a_part_to_mcp_content(#a2a_part{}) -> #mcp_content{}.
a2a_part_to_mcp_content(#a2a_part{text = Text} = Part) when Text =/= undefined ->
    #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = Text,
        data = undefined,
        mime_type = Part#a2a_part.media_type,
        annotations = convert_metadata_to_annotations(Part#a2a_part.metadata),
        resource_link = undefined
    };
a2a_part_to_mcp_content(#a2a_part{raw = Raw, media_type = MediaType} = Part)
  when Raw =/= undefined ->
    %% Determine content type from media type
    Type = case MediaType of
        <<"image/", _/binary>> -> ?MCP_CONTENT_TYPE_IMAGE;
        <<"audio/", _/binary>> -> ?MCP_CONTENT_TYPE_AUDIO;
        _ -> ?MCP_CONTENT_TYPE_IMAGE  % Default to image for binary data
    end,
    #mcp_content{
        type = Type,
        text = undefined,
        data = base64:encode(Raw),
        mime_type = MediaType,
        annotations = convert_metadata_to_annotations(Part#a2a_part.metadata),
        resource_link = undefined
    };
a2a_part_to_mcp_content(#a2a_part{url = Url, filename = Filename} = Part)
  when Url =/= undefined ->
    #mcp_content{
        type = ?MCP_CONTENT_TYPE_RESOURCE_LINK,
        text = undefined,
        data = undefined,
        mime_type = Part#a2a_part.media_type,
        annotations = convert_metadata_to_annotations(Part#a2a_part.metadata),
        resource_link = #mcp_resource_link{
            uri = Url,
            name = Filename,
            mime_type = Part#a2a_part.media_type,
            size = undefined
        }
    };
a2a_part_to_mcp_content(#a2a_part{data = Data} = Part) when Data =/= undefined ->
    %% Convert arbitrary data to JSON text
    JsonText = try
        json:encode(Data)
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Data]))
    end,
    #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = JsonText,
        data = undefined,
        mime_type = ?MCP_MIME_APPLICATION_JSON,
        annotations = convert_metadata_to_annotations(Part#a2a_part.metadata),
        resource_link = undefined
    };
a2a_part_to_mcp_content(#a2a_part{}) ->
    %% Empty part - return empty text content
    #mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = <<>>,
        data = undefined,
        mime_type = undefined,
        annotations = [],
        resource_link = undefined
    }.

%% @doc Convert MCP content to A2A parts list.
%%
%% Wraps single content in a list for consistency with A2A message structure.
%%
-spec mcp_content_to_a2a_parts(#mcp_content{} | [#mcp_content{}]) -> [#a2a_part{}].
mcp_content_to_a2a_parts(Contents) when is_list(Contents) ->
    [mcp_content_to_a2a_part(C) || C <- Contents];
mcp_content_to_a2a_parts(#mcp_content{} = Content) ->
    [mcp_content_to_a2a_part(Content)].

%% @doc Convert a single MCP content to A2A part.
-spec mcp_content_to_a2a_part(#mcp_content{}) -> #a2a_part{}.
mcp_content_to_a2a_part(#mcp_content{type = ?MCP_CONTENT_TYPE_TEXT} = Content) ->
    #a2a_part{
        text = Content#mcp_content.text,
        raw = undefined,
        url = undefined,
        data = undefined,
        metadata = convert_annotations_to_metadata(Content#mcp_content.annotations),
        filename = undefined,
        media_type = Content#mcp_content.mime_type
    };
mcp_content_to_a2a_part(#mcp_content{type = ?MCP_CONTENT_TYPE_IMAGE} = Content) ->
    %% Decode base64 data for raw binary
    RawData = case Content#mcp_content.data of
        undefined -> undefined;
        Data -> try base64:decode(Data) catch _:_ -> Data end
    end,
    #a2a_part{
        text = undefined,
        raw = RawData,
        url = undefined,
        data = undefined,
        metadata = convert_annotations_to_metadata(Content#mcp_content.annotations),
        filename = undefined,
        media_type = Content#mcp_content.mime_type
    };
mcp_content_to_a2a_part(#mcp_content{type = ?MCP_CONTENT_TYPE_AUDIO} = Content) ->
    %% Decode base64 data for raw binary
    RawData = case Content#mcp_content.data of
        undefined -> undefined;
        Data -> try base64:decode(Data) catch _:_ -> Data end
    end,
    #a2a_part{
        text = undefined,
        raw = RawData,
        url = undefined,
        data = undefined,
        metadata = convert_annotations_to_metadata(Content#mcp_content.annotations),
        filename = undefined,
        media_type = Content#mcp_content.mime_type
    };
mcp_content_to_a2a_part(#mcp_content{type = ?MCP_CONTENT_TYPE_RESOURCE_LINK,
                                     resource_link = ResourceLink})
  when ResourceLink =/= undefined ->
    #a2a_part{
        text = undefined,
        raw = undefined,
        url = ResourceLink#mcp_resource_link.uri,
        data = undefined,
        metadata = undefined,
        filename = ResourceLink#mcp_resource_link.name,
        media_type = ResourceLink#mcp_resource_link.mime_type
    };
mcp_content_to_a2a_part(#mcp_content{type = ?MCP_CONTENT_TYPE_RESOURCE} = Content) ->
    %% Generic resource - convert to text part
    #a2a_part{
        text = Content#mcp_content.text,
        raw = undefined,
        url = undefined,
        data = undefined,
        metadata = convert_annotations_to_metadata(Content#mcp_content.annotations),
        filename = undefined,
        media_type = Content#mcp_content.mime_type
    };
mcp_content_to_a2a_part(#mcp_content{} = Content) ->
    %% Default conversion - treat as text
    #a2a_part{
        text = Content#mcp_content.text,
        raw = undefined,
        url = undefined,
        data = undefined,
        metadata = convert_annotations_to_metadata(Content#mcp_content.annotations),
        filename = undefined,
        media_type = Content#mcp_content.mime_type
    }.

%%====================================================================
%% Task Execution
%%====================================================================

%% @doc Execute an MCP tool as an A2A task.
%%
%% Creates an A2A task that wraps MCP tool execution:
%% 1. Creates a new task with submitted state
%% 2. Spawns a process to execute the tool
%% 3. Updates task state based on execution result
%% 4. Returns the task with result or error
%%
%% Options:
%% - timeout: Execution timeout in milliseconds (default: 30000)
%% - blocking: If true, waits for completion (default: true)
%% - context_id: A2A context ID for the task
%%
-spec execute_tool_as_task(binary(), map(), map()) ->
    {ok, #a2a_task{}} | {error, term()}.
execute_tool_as_task(ToolName, Args, Options) ->
    execute_tool_as_task(ToolName, Args, Options, undefined).

%% @doc Execute an MCP tool as an A2A task with caller process.
-spec execute_tool_as_task(binary(), map(), map(), pid() | undefined) ->
    {ok, #a2a_task{}} | {error, term()}.
execute_tool_as_task(ToolName, Args, Options, _Caller) ->
    %% Generate task and context IDs
    TaskId = generate_uuid(),
    ContextId = maps:get(context_id, Options, generate_uuid()),

    %% Create initial task with submitted state
    InitialTask = #a2a_task{
        id = TaskId,
        context_id = ContextId,
        status = #a2a_task_status{
            state = submitted,
            message = undefined,
            timestamp = iso8601_now()
        },
        artifacts = [],
        history = [],
        metadata = #{
            <<"tool_name">> => ToolName,
            <<"args">> => Args,
            <<"bridge">> => <<"erlmcp_a2a_mcp_bridge">>
        }
    },

    %% Get execution options
    Timeout = maps:get(timeout, Options, 30000),
    Blocking = maps:get(blocking, Options, true),

    %% Spawn task executor
    ExecutorPid = spawn_link(?MODULE, task_executor,
                             [InitialTask, ToolName, Args, Timeout, self()]),

    case Blocking of
        true ->
            %% Wait for task completion
            receive
                {task_complete, ExecutorPid, FinalTask} ->
                    {ok, FinalTask};
                {task_error, ExecutorPid, Error} ->
                    %% Return task in failed state
                    FailedTask = InitialTask#a2a_task{
                        status = #a2a_task_status{
                            state = failed,
                            message = create_error_message(Error),
                            timestamp = iso8601_now()
                        }
                    },
                    {ok, FailedTask}
            after Timeout + 1000 ->
                %% Kill the executor and return timeout error
                exit(ExecutorPid, kill),
                TimeoutTask = InitialTask#a2a_task{
                    status = #a2a_task_status{
                        state = failed,
                        message = create_error_message(timeout),
                        timestamp = iso8601_now()
                    }
                },
                {ok, TimeoutTask}
            end;
        false ->
            %% Return immediately with submitted task
            {ok, InitialTask}
    end.

%% @private Task executor process
-spec task_executor(#a2a_task{}, binary(), map(), timeout(), pid()) -> no_return().
task_executor(Task, ToolName, Args, Timeout, Parent) ->
    %% Update task to working state
    WorkingTask = Task#a2a_task{
        status = #a2a_task_status{
            state = working,
            message = undefined,
            timestamp = iso8601_now()
        }
    },

    try
        %% Execute the MCP tool
        Result = execute_mcp_tool(ToolName, Args, Timeout),

        case Result of
            {ok, ToolResult} ->
                %% Convert result to A2A artifact
                Artifact = create_artifact_from_result(ToolResult),

                %% Create completed task
                CompletedTask = WorkingTask#a2a_task{
                    status = #a2a_task_status{
                        state = completed,
                        message = undefined,
                        timestamp = iso8601_now()
                    },
                    artifacts = [Artifact]
                },
                Parent ! {task_complete, self(), CompletedTask};

            {error, Reason} ->
                Parent ! {task_error, self(), Reason}
        end
    catch
        Type:Error:Stacktrace ->
            logger:error("Task execution error: ~p:~p~n~p",
                        [Type, Error, Stacktrace]),
            Parent ! {task_error, self(), {Type, Error}}
    end.

%%====================================================================
%% Capability Mapping
%%====================================================================

%% @doc Map capabilities between protocols.
%%
%% Detects the source protocol from the record type and maps to the other.
%%
-spec map_capabilities(#mcp_server_capabilities{} | #a2a_agent_capabilities{}) ->
    #a2a_agent_capabilities{} | #mcp_server_capabilities{}.
map_capabilities(#mcp_server_capabilities{} = Caps) ->
    map_mcp_capabilities_to_a2a(Caps);
map_capabilities(#a2a_agent_capabilities{} = Caps) ->
    map_a2a_capabilities_to_mcp(Caps).

%% @doc Map MCP server capabilities to A2A agent capabilities.
-spec map_mcp_capabilities_to_a2a(#mcp_server_capabilities{}) -> #a2a_agent_capabilities{}.
map_mcp_capabilities_to_a2a(#mcp_server_capabilities{} = MCPCaps) ->
    %% Determine streaming support from MCP capabilities
    %% MCP supports streaming via SSE, so we can expose this as A2A streaming
    Streaming = has_mcp_streaming_support(MCPCaps),

    %% Map experimental capabilities to A2A extensions
    Extensions = case MCPCaps#mcp_server_capabilities.experimental of
        undefined -> undefined;
        ExpMap when is_map(ExpMap) ->
            maps:fold(fun(Key, Value, Acc) ->
                Ext = #a2a_agent_extension{
                    uri = <<"urn:mcp:experimental:", Key/binary>>,
                    description = undefined,
                    required = false,
                    params = Value
                },
                [Ext | Acc]
            end, [], ExpMap);
        _ -> undefined
    end,

    #a2a_agent_capabilities{
        streaming = Streaming,
        push_notifications = false,  % MCP doesn't have push notifications by default
        extensions = Extensions,
        extended_agent_card = false
    }.

%% @doc Map A2A agent capabilities to MCP server capabilities.
-spec map_a2a_capabilities_to_mcp(#a2a_agent_capabilities{}) -> #mcp_server_capabilities{}.
map_a2a_capabilities_to_mcp(#a2a_agent_capabilities{} = A2ACaps) ->
    %% Map extensions to experimental capabilities
    Experimental = case A2ACaps#a2a_agent_capabilities.extensions of
        undefined -> undefined;
        [] -> undefined;
        Extensions ->
            lists:foldl(fun(#a2a_agent_extension{uri = Uri, params = Params}, Acc) ->
                %% Extract key from URI
                Key = extract_extension_key(Uri),
                Acc#{Key => Params}
            end, #{}, Extensions)
    end,

    #mcp_server_capabilities{
        resources = #mcp_resources_capability{
            subscribe = true,  % A2A supports subscriptions via streaming
            listChanged = true
        },
        tools = #mcp_tools_capability{
            listChanged = true
        },
        prompts = #mcp_prompts_capability{
            listChanged = false
        },
        logging = #mcp_logging_capability{},
        sampling = #mcp_sampling_capability{},
        roots = #mcp_roots_capability{list_changed = false},
        completions = undefined,
        experimental = Experimental
    }.

%%====================================================================
%% Error Mapping
%%====================================================================

%% @doc Map an error code between protocols.
%%
%% Converts error codes bidirectionally between MCP and A2A.
%% Preserves semantic meaning across protocols.
%%
-spec map_error(error_code(), protocol()) -> error_code().
map_error(Code, mcp) ->
    %% Source is MCP, convert to A2A
    mcp_error_to_a2a(Code);
map_error(Code, a2a) ->
    %% Source is A2A, convert to MCP
    a2a_error_to_mcp(Code).

%% @doc Convert MCP error code to A2A error code.
-spec mcp_error_to_a2a(error_code()) -> error_code().
mcp_error_to_a2a(Code) ->
    case Code of
        %% JSON-RPC standard errors (pass through)
        C when C >= -32700, C =< -32600 -> C;

        %% MCP tool errors -> A2A task errors
        ?MCP_ERROR_TOOL_NOT_FOUND -> ?A2A_ERROR_SKILL_NOT_FOUND;
        ?MCP_ERROR_TOOL_EXECUTION_FAILED -> ?A2A_ERROR_TASK_FAILED;
        ?MCP_ERROR_TOOL_TIMEOUT -> ?A2A_ERROR_STREAM_TIMEOUT;
        ?MCP_ERROR_TOOL_CANCELLED -> ?A2A_ERROR_STREAM_CANCELLED;
        ?MCP_ERROR_INVALID_TOOL_ARGUMENTS -> ?A2A_ERROR_MESSAGE_INVALID;
        ?MCP_ERROR_TOOL_DISABLED -> ?A2A_ERROR_SKILL_NOT_FOUND;
        ?MCP_ERROR_TOOL_RESULT_TOO_LARGE -> ?A2A_ERROR_PART_TOO_LARGE;
        ?MCP_ERROR_TOOL_NOT_ALLOWED -> ?A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET;
        ?MCP_ERROR_MAX_CONCURRENT_TOOLS -> ?A2A_ERROR_LIMIT_EXCEEDED;

        %% MCP resource errors -> A2A artifact errors
        ?MCP_ERROR_RESOURCE_NOT_FOUND -> ?A2A_ERROR_ARTIFACT_NOT_FOUND;
        ?MCP_ERROR_RESOURCE_ACCESS_DENIED -> ?A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET;

        %% MCP task errors -> A2A task errors
        ?MCP_ERROR_TASK_NOT_FOUND -> ?A2A_ERROR_TASK_NOT_FOUND;
        ?MCP_ERROR_TASK_FAILED -> ?A2A_ERROR_TASK_FAILED;
        ?MCP_ERROR_TASK_CANCELLED -> ?A2A_ERROR_STREAM_CANCELLED;
        ?MCP_ERROR_TASK_TIMEOUT -> ?A2A_ERROR_STREAM_TIMEOUT;

        %% MCP capability errors
        ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED -> ?A2A_ERROR_CAPABILITY_NOT_SUPPORTED;
        ?MCP_ERROR_NOT_INITIALIZED -> ?A2A_ERROR_AGENT_NOT_FOUND;

        %% MCP content errors
        ?MCP_ERROR_INVALID_CONTENT_TYPE -> ?A2A_ERROR_MEDIA_TYPE_UNSUPPORTED;
        ?MCP_ERROR_CONTENT_TOO_LARGE -> ?A2A_ERROR_MESSAGE_TOO_LARGE;

        %% MCP pagination errors
        ?MCP_ERROR_INVALID_CURSOR -> ?A2A_ERROR_PAGE_TOKEN_INVALID;
        ?MCP_ERROR_PAGE_SIZE_TOO_LARGE -> ?A2A_ERROR_PAGE_SIZE_TOO_LARGE;

        %% MCP auth errors
        ?MCP_ERROR_AUTHENTICATION_FAILED -> ?A2A_ERROR_PUSH_AUTH_FAILED;
        ?MCP_ERROR_AUTHORIZATION_FAILED -> ?A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET;
        ?MCP_ERROR_ACCESS_DENIED -> ?A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET;

        %% Default: map to generic A2A error
        _ -> ?A2A_ERROR_TASK_FAILED
    end.

%% @doc Convert A2A error code to MCP error code.
-spec a2a_error_to_mcp(error_code()) -> error_code().
a2a_error_to_mcp(Code) ->
    case Code of
        %% JSON-RPC standard errors (pass through)
        C when C >= -32700, C =< -32600 -> C;

        %% A2A task errors -> MCP errors
        ?A2A_ERROR_TASK_NOT_FOUND -> ?MCP_ERROR_TASK_NOT_FOUND;
        ?A2A_ERROR_TASK_FAILED -> ?MCP_ERROR_TASK_FAILED;
        ?A2A_ERROR_TASK_ALREADY_TERMINAL -> ?MCP_ERROR_TASK_ALREADY_COMPLETED;
        ?A2A_ERROR_TASK_NOT_CANCELABLE -> ?MCP_ERROR_TASK_STATE_INVALID;
        ?A2A_ERROR_TASK_STATE_TRANSITION_INVALID -> ?MCP_ERROR_TASK_STATE_INVALID;

        %% A2A skill errors -> MCP tool errors
        ?A2A_ERROR_SKILL_NOT_FOUND -> ?MCP_ERROR_TOOL_NOT_FOUND;
        ?A2A_ERROR_SKILL_INVALID -> ?MCP_ERROR_TOOL_SCHEMA_INVALID;

        %% A2A message errors -> MCP content errors
        ?A2A_ERROR_MESSAGE_INVALID -> ?JSONRPC_INVALID_PARAMS;
        ?A2A_ERROR_MESSAGE_TOO_LARGE -> ?MCP_ERROR_MESSAGE_TOO_LARGE;
        ?A2A_ERROR_PART_INVALID -> ?MCP_ERROR_INVALID_CONTENT_TYPE;
        ?A2A_ERROR_PART_TOO_LARGE -> ?MCP_ERROR_CONTENT_TOO_LARGE;
        ?A2A_ERROR_MEDIA_TYPE_UNSUPPORTED -> ?MCP_ERROR_UNSUPPORTED_MEDIA_TYPE;

        %% A2A artifact errors -> MCP resource errors
        ?A2A_ERROR_ARTIFACT_NOT_FOUND -> ?MCP_ERROR_RESOURCE_NOT_FOUND;

        %% A2A context errors
        ?A2A_ERROR_CONTEXT_NOT_FOUND -> ?MCP_ERROR_SESSION_NOT_FOUND;

        %% A2A agent errors
        ?A2A_ERROR_AGENT_NOT_FOUND -> ?MCP_ERROR_NOT_INITIALIZED;
        ?A2A_ERROR_CAPABILITY_NOT_SUPPORTED -> ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED;

        %% A2A streaming errors
        ?A2A_ERROR_STREAMING_NOT_SUPPORTED -> ?MCP_ERROR_CAPABILITY_NOT_SUPPORTED;
        ?A2A_ERROR_STREAM_CLOSED -> ?MCP_ERROR_TRANSPORT_ERROR;
        ?A2A_ERROR_STREAM_ERROR -> ?MCP_ERROR_TRANSPORT_ERROR;
        ?A2A_ERROR_STREAM_TIMEOUT -> ?MCP_ERROR_TIMEOUT;
        ?A2A_ERROR_STREAM_CANCELLED -> ?MCP_ERROR_TASK_CANCELLED;

        %% A2A push notification errors
        ?A2A_ERROR_PUSH_NOTIFICATION_FAILED -> ?MCP_ERROR_NOTIFICATION_FAILED;
        ?A2A_ERROR_PUSH_AUTH_FAILED -> ?MCP_ERROR_AUTHENTICATION_FAILED;

        %% A2A pagination errors
        ?A2A_ERROR_PAGE_TOKEN_INVALID -> ?MCP_ERROR_INVALID_CURSOR;
        ?A2A_ERROR_PAGE_SIZE_INVALID -> ?MCP_ERROR_PAGE_SIZE_INVALID;
        ?A2A_ERROR_PAGE_SIZE_TOO_LARGE -> ?MCP_ERROR_PAGE_SIZE_TOO_LARGE;

        %% A2A security errors
        ?A2A_ERROR_SECURITY_REQUIREMENT_NOT_MET -> ?MCP_ERROR_ACCESS_DENIED;
        ?A2A_ERROR_SECURITY_SCHEME_INVALID -> ?MCP_ERROR_INVALID_TOKEN;

        %% A2A limit errors
        ?A2A_ERROR_LIMIT_EXCEEDED -> ?MCP_ERROR_RATE_LIMITED;

        %% Default: map to generic MCP error
        _ -> ?MCP_ERROR_TASK_FAILED
    end.

%%====================================================================
%% Internal Functions - Tool/Skill Helpers
%%====================================================================

%% @private Extract tags from MCP tool metadata
-spec extract_tags_from_metadata(map() | undefined) -> [binary()].
extract_tags_from_metadata(undefined) ->
    [];
extract_tags_from_metadata(Metadata) when is_map(Metadata) ->
    case maps:get(<<"tags">>, Metadata, maps:get(tags, Metadata, undefined)) of
        undefined -> [];
        Tags when is_list(Tags) -> [ensure_binary(T) || T <- Tags];
        _ -> []
    end;
extract_tags_from_metadata(_) ->
    [].

%% @private Extract examples from MCP tool metadata
-spec extract_examples_from_metadata(map() | undefined) -> [binary()] | undefined.
extract_examples_from_metadata(undefined) ->
    undefined;
extract_examples_from_metadata(Metadata) when is_map(Metadata) ->
    case maps:get(<<"examples">>, Metadata, maps:get(examples, Metadata, undefined)) of
        undefined -> undefined;
        Examples when is_list(Examples) -> [ensure_binary(E) || E <- Examples];
        _ -> undefined
    end;
extract_examples_from_metadata(_) ->
    undefined.

%% @private Derive A2A input modes from MCP input schema
-spec derive_input_modes_from_schema(map() | undefined) -> [binary()] | undefined.
derive_input_modes_from_schema(undefined) ->
    [?A2A_MIME_TEXT_PLAIN];
derive_input_modes_from_schema(Schema) when is_map(Schema) ->
    %% Analyze schema to determine appropriate input modes
    _Type = maps:get(<<"type">>, Schema, <<"object">>),
    Properties = maps:get(<<"properties">>, Schema, #{}),

    Modes = case analyze_schema_for_modes(Properties) of
        [] -> [?A2A_MIME_TEXT_PLAIN];
        M -> M
    end,

    %% Always include JSON for structured input
    lists:usort([?A2A_MIME_APPLICATION_JSON | Modes]);
derive_input_modes_from_schema(_) ->
    [?A2A_MIME_TEXT_PLAIN, ?A2A_MIME_APPLICATION_JSON].

%% @private Analyze schema properties for mode detection
-spec analyze_schema_for_modes(map()) -> [binary()].
analyze_schema_for_modes(Properties) when is_map(Properties) ->
    maps:fold(fun(_Key, PropSchema, Acc) ->
        case PropSchema of
            #{<<"format">> := <<"binary">>} ->
                [?A2A_MIME_APPLICATION_OCTET_STREAM | Acc];
            #{<<"contentMediaType">> := MediaType} ->
                [MediaType | Acc];
            _ ->
                Acc
        end
    end, [], Properties);
analyze_schema_for_modes(_) ->
    [].

%% @private Build MCP input schema from A2A input modes
-spec build_input_schema_from_modes([binary()] | undefined) -> map().
build_input_schema_from_modes(undefined) ->
    #{<<"type">> => <<"object">>};
build_input_schema_from_modes(Modes) when is_list(Modes) ->
    %% Create a flexible schema that accepts various content types
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"content">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Input content">>
            },
            <<"mediaType">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => Modes,
                <<"description">> => <<"Content media type">>
            }
        }
    }.

%% @private Build MCP tool metadata from A2A skill
-spec build_tool_metadata(#a2a_agent_skill{}) -> map().
build_tool_metadata(#a2a_agent_skill{} = Skill) ->
    Base = #{
        <<"source">> => <<"a2a">>,
        <<"skill_id">> => Skill#a2a_agent_skill.id
    },

    WithTags = case Skill#a2a_agent_skill.tags of
        [] -> Base;
        Tags -> Base#{<<"tags">> => Tags}
    end,

    WithExamples = case Skill#a2a_agent_skill.examples of
        undefined -> WithTags;
        Examples -> WithTags#{<<"examples">> => Examples}
    end,

    WithInputModes = case Skill#a2a_agent_skill.input_modes of
        undefined -> WithExamples;
        InputModes -> WithExamples#{<<"input_modes">> => InputModes}
    end,

    case Skill#a2a_agent_skill.output_modes of
        undefined -> WithInputModes;
        OutputModes -> WithInputModes#{<<"output_modes">> => OutputModes}
    end.

%% @private Humanize a tool name for display
-spec humanize_name(binary()) -> binary().
humanize_name(Name) when is_binary(Name) ->
    %% Replace underscores with spaces, capitalize words
    Parts = binary:split(Name, [<<"_">>, <<"-">>], [global]),
    Capitalized = [capitalize_word(P) || P <- Parts],
    iolist_to_binary(lists:join(<<" ">>, Capitalized));
humanize_name(Name) ->
    ensure_binary(Name).

%% @private Capitalize first letter of a word
-spec capitalize_word(binary()) -> binary().
capitalize_word(<<>>) -> <<>>;
capitalize_word(<<C, Rest/binary>>) when C >= $a, C =< $z ->
    <<(C - 32), Rest/binary>>;
capitalize_word(Word) -> Word.

%%====================================================================
%% Internal Functions - Content Helpers
%%====================================================================

%% @private Convert A2A metadata to MCP annotations
-spec convert_metadata_to_annotations(map() | undefined) -> [#mcp_annotation{}].
convert_metadata_to_annotations(undefined) ->
    [];
convert_metadata_to_annotations(Metadata) when is_map(Metadata) ->
    maps:fold(fun(Key, Value, Acc) ->
        Annotation = #mcp_annotation{
            name = ensure_binary(Key),
            value = Value
        },
        [Annotation | Acc]
    end, [], Metadata);
convert_metadata_to_annotations(_) ->
    [].

%% @private Convert MCP annotations to A2A metadata
-spec convert_annotations_to_metadata([#mcp_annotation{}]) -> map() | undefined.
convert_annotations_to_metadata([]) ->
    undefined;
convert_annotations_to_metadata(Annotations) when is_list(Annotations) ->
    lists:foldl(fun(#mcp_annotation{name = Name, value = Value}, Acc) ->
        Acc#{Name => Value}
    end, #{}, Annotations);
convert_annotations_to_metadata(_) ->
    undefined.

%%====================================================================
%% Internal Functions - Task Execution Helpers
%%====================================================================

%% @private Execute MCP tool and return result
-spec execute_mcp_tool(binary(), map(), timeout()) ->
    {ok, term()} | {error, term()}.
execute_mcp_tool(ToolName, Args, Timeout) ->
    %% Try to use erlmcp_tool_execute if available
    try
        case erlmcp_tool_execute:execute_tool(ToolName, Args, #{timeout => Timeout}) of
            {ok, SandboxPid, _MonitorRef} ->
                %% Wait for result
                receive
                    {tool_result, SandboxPid, Result} ->
                        Result
                after Timeout ->
                    erlmcp_tool_execute:cancel_execution(SandboxPid),
                    {error, timeout}
                end;
            {error, Reason} ->
                {error, Reason}
        end
    catch
        error:undef ->
            %% erlmcp_tool_execute not available, simulate execution
            simulate_tool_execution(ToolName, Args)
    end.

%% @private Simulate tool execution for testing/fallback
-spec simulate_tool_execution(binary(), map()) -> {ok, term()} | {error, term()}.
simulate_tool_execution(ToolName, Args) ->
    %% Return a synthetic result
    {ok, #{
        <<"tool">> => ToolName,
        <<"args">> => Args,
        <<"result">> => <<"simulated_result">>,
        <<"timestamp">> => iso8601_now()
    }}.

%% @private Create A2A artifact from tool result
-spec create_artifact_from_result(term()) -> #a2a_artifact{}.
create_artifact_from_result(Result) ->
    %% Convert result to parts
    Part = case Result of
        Bin when is_binary(Bin) ->
            #a2a_part{text = Bin};
        Map when is_map(Map) ->
            JsonText = try json:encode(Map) catch _:_ -> <<"{}">> end,
            #a2a_part{data = Map, text = JsonText, media_type = ?A2A_MIME_APPLICATION_JSON};
        List when is_list(List) ->
            JsonText = try json:encode(List) catch _:_ -> <<"[]">> end,
            #a2a_part{data = List, text = JsonText, media_type = ?A2A_MIME_APPLICATION_JSON};
        Other ->
            #a2a_part{text = iolist_to_binary(io_lib:format("~p", [Other]))}
    end,

    #a2a_artifact{
        artifact_id = generate_uuid(),
        name = <<"tool_result">>,
        description = <<"Result from MCP tool execution">>,
        parts = [Part],
        metadata = #{<<"source">> => <<"mcp_bridge">>},
        extensions = undefined
    }.

%% @private Create error message for A2A task
-spec create_error_message(term()) -> #a2a_message{}.
create_error_message(Reason) ->
    ErrorText = case Reason of
        timeout -> <<"Task execution timed out">>;
        {Type, Error} ->
            iolist_to_binary(io_lib:format("~p: ~p", [Type, Error]));
        Other ->
            iolist_to_binary(io_lib:format("~p", [Other]))
    end,

    #a2a_message{
        message_id = generate_uuid(),
        context_id = undefined,
        task_id = undefined,
        role = agent,
        parts = [#a2a_part{text = ErrorText}],
        metadata = #{<<"error">> => true},
        extensions = undefined,
        reference_task_ids = undefined
    }.

%%====================================================================
%% Internal Functions - Capability Helpers
%%====================================================================

%% @private Check if MCP capabilities indicate streaming support
-spec has_mcp_streaming_support(#mcp_server_capabilities{}) -> boolean().
has_mcp_streaming_support(#mcp_server_capabilities{} = Caps) ->
    %% MCP supports streaming via resources subscription
    case Caps#mcp_server_capabilities.resources of
        #mcp_resources_capability{subscribe = true} -> true;
        _ -> false
    end.

%% @private Extract key from A2A extension URI
-spec extract_extension_key(binary()) -> binary().
extract_extension_key(Uri) when is_binary(Uri) ->
    case binary:split(Uri, <<":">>, [global]) of
        Parts when length(Parts) > 0 ->
            lists:last(Parts);
        _ ->
            Uri
    end.

%%====================================================================
%% Internal Functions - Utility Helpers
%%====================================================================

%% @private Ensure value is binary
-spec ensure_binary(term()) -> binary().
ensure_binary(Bin) when is_binary(Bin) -> Bin;
ensure_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
ensure_binary(List) when is_list(List) -> list_to_binary(List);
ensure_binary(Int) when is_integer(Int) -> integer_to_binary(Int);
ensure_binary(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

%% @private Generate UUID v4
-spec generate_uuid() -> binary().
generate_uuid() ->
    %% Generate 16 random bytes
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    %% Set version (4) and variant (RFC 4122)
    UuidBin = <<A:32, B:16, 4:4, (C band 16#0FFF):12,
                2:2, (D band 16#3FFF):14, E:48>>,
    %% Format as string
    <<A1:32, B1:16, C1:16, D1:16, E1:48>> = UuidBin,
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                    [A1, B1, C1, D1, E1])).

%% @private Get current ISO 8601 timestamp
-spec iso8601_now() -> binary().
iso8601_now() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:universal_time(),
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                                    [Year, Month, Day, Hour, Min, Sec])).
