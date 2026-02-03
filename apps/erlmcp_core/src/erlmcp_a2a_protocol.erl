%%%-------------------------------------------------------------------
%%% @doc A2A Protocol Encoding/Decoding Module
%%%
%%% This module handles the serialization and deserialization of A2A
%%% protocol messages between Erlang records and JSON (maps).
%%%
%%% The A2A protocol uses JSON-RPC 2.0 as its transport layer, similar
%%% to MCP. This module provides the bridge between the protocol-agnostic
%%% records defined in erlmcp_a2a.hrl and the wire format.
%%%
%%% Key functions:
%%% - encode_*/1 - Convert records to JSON-serializable maps
%%% - decode_*/1 - Convert JSON maps to records
%%% - validate_*/1 - Validate message structure
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_a2a_protocol).

-include("erlmcp.hrl").
-include("erlmcp_a2a.hrl").

%% API exports
-export([
    %% Part encoding/decoding
    encode_part/1,
    decode_part/1,

    %% Message encoding/decoding
    encode_message/1,
    decode_message/1,

    %% Artifact encoding/decoding
    encode_artifact/1,
    decode_artifact/1,

    %% Task encoding/decoding
    encode_task/1,
    decode_task/1,
    encode_task_status/1,
    decode_task_status/1,

    %% Event encoding/decoding
    encode_task_status_update_event/1,
    decode_task_status_update_event/1,
    encode_task_artifact_update_event/1,
    decode_task_artifact_update_event/1,

    %% Request encoding/decoding
    encode_send_message_request/1,
    decode_send_message_request/1,
    encode_get_task_request/1,
    decode_get_task_request/1,
    encode_list_tasks_request/1,
    decode_list_tasks_request/1,
    encode_cancel_task_request/1,
    decode_cancel_task_request/1,

    %% Response encoding/decoding
    encode_send_message_response/1,
    decode_send_message_response/1,
    encode_stream_response/1,
    decode_stream_response/1,
    encode_list_tasks_response/1,
    decode_list_tasks_response/1,

    %% Agent Card encoding/decoding
    encode_agent_card/1,
    decode_agent_card/1,
    encode_agent_skill/1,
    decode_agent_skill/1,
    encode_agent_interface/1,
    decode_agent_interface/1,

    %% Push notification encoding/decoding
    encode_push_notification_config/1,
    decode_push_notification_config/1,

    %% State conversion utilities
    task_state_to_binary/1,
    binary_to_task_state/1,
    role_to_binary/1,
    binary_to_role/1,

    %% Validation
    validate_message/1,
    validate_task/1,
    validate_part/1,
    is_terminal_state/1,
    is_interrupted_state/1
]).

%%====================================================================
%% Part Encoding/Decoding
%%====================================================================

%% @doc Encode a part record to a JSON-serializable map
-spec encode_part(#a2a_part{}) -> map().
encode_part(#a2a_part{} = Part) ->
    Base = #{},

    %% Add content (only one should be set)
    WithContent = case Part of
        #a2a_part{text = Text} when Text =/= undefined ->
            Base#{<<"text">> => Text};
        #a2a_part{raw = Raw} when Raw =/= undefined ->
            Base#{<<"raw">> => base64:encode(Raw)};
        #a2a_part{url = Url} when Url =/= undefined ->
            Base#{<<"url">> => Url};
        #a2a_part{data = Data} when Data =/= undefined ->
            Base#{<<"data">> => Data};
        _ ->
            Base
    end,

    %% Add optional fields
    maybe_add(<<"metadata">>, Part#a2a_part.metadata,
    maybe_add(<<"filename">>, Part#a2a_part.filename,
    maybe_add(<<"mediaType">>, Part#a2a_part.media_type,
    WithContent))).

%% @doc Decode a JSON map to a part record
-spec decode_part(map()) -> {ok, #a2a_part{}} | {error, term()}.
decode_part(Map) when is_map(Map) ->
    try
        Part = #a2a_part{
            text = maps:get(<<"text">>, Map, undefined),
            raw = decode_raw(maps:get(<<"raw">>, Map, undefined)),
            url = maps:get(<<"url">>, Map, undefined),
            data = maps:get(<<"data">>, Map, undefined),
            metadata = maps:get(<<"metadata">>, Map, undefined),
            filename = maps:get(<<"filename">>, Map, undefined),
            media_type = maps:get(<<"mediaType">>, Map, undefined)
        },
        {ok, Part}
    catch
        _:Reason -> {error, {invalid_part, Reason}}
    end;
decode_part(_) ->
    {error, invalid_part_format}.

%%====================================================================
%% Message Encoding/Decoding
%%====================================================================

%% @doc Encode a message record to a JSON-serializable map
-spec encode_message(#a2a_message{}) -> map().
encode_message(#a2a_message{} = Msg) ->
    Base = #{
        <<"messageId">> => Msg#a2a_message.message_id,
        <<"role">> => role_to_binary(Msg#a2a_message.role),
        <<"parts">> => [encode_part(P) || P <- Msg#a2a_message.parts]
    },

    maybe_add(<<"contextId">>, Msg#a2a_message.context_id,
    maybe_add(<<"taskId">>, Msg#a2a_message.task_id,
    maybe_add(<<"metadata">>, Msg#a2a_message.metadata,
    maybe_add(<<"extensions">>, Msg#a2a_message.extensions,
    maybe_add(<<"referenceTaskIds">>, Msg#a2a_message.reference_task_ids,
    Base))))).

%% @doc Decode a JSON map to a message record
-spec decode_message(map()) -> {ok, #a2a_message{}} | {error, term()}.
decode_message(Map) when is_map(Map) ->
    try
        PartsData = maps:get(<<"parts">>, Map, []),
        Parts = [begin
            {ok, P} = decode_part(PartMap),
            P
        end || PartMap <- PartsData],

        Msg = #a2a_message{
            message_id = maps:get(<<"messageId">>, Map),
            context_id = maps:get(<<"contextId">>, Map, undefined),
            task_id = maps:get(<<"taskId">>, Map, undefined),
            role = binary_to_role(maps:get(<<"role">>, Map)),
            parts = Parts,
            metadata = maps:get(<<"metadata">>, Map, undefined),
            extensions = maps:get(<<"extensions">>, Map, undefined),
            reference_task_ids = maps:get(<<"referenceTaskIds">>, Map, undefined)
        },
        {ok, Msg}
    catch
        _:Reason -> {error, {invalid_message, Reason}}
    end;
decode_message(_) ->
    {error, invalid_message_format}.

%%====================================================================
%% Artifact Encoding/Decoding
%%====================================================================

%% @doc Encode an artifact record to a JSON-serializable map
-spec encode_artifact(#a2a_artifact{}) -> map().
encode_artifact(#a2a_artifact{} = Art) ->
    Base = #{
        <<"artifactId">> => Art#a2a_artifact.artifact_id,
        <<"parts">> => [encode_part(P) || P <- Art#a2a_artifact.parts]
    },

    maybe_add(<<"name">>, Art#a2a_artifact.name,
    maybe_add(<<"description">>, Art#a2a_artifact.description,
    maybe_add(<<"metadata">>, Art#a2a_artifact.metadata,
    maybe_add(<<"extensions">>, Art#a2a_artifact.extensions,
    Base)))).

%% @doc Decode a JSON map to an artifact record
-spec decode_artifact(map()) -> {ok, #a2a_artifact{}} | {error, term()}.
decode_artifact(Map) when is_map(Map) ->
    try
        PartsData = maps:get(<<"parts">>, Map, []),
        Parts = [begin
            {ok, P} = decode_part(PartMap),
            P
        end || PartMap <- PartsData],

        Art = #a2a_artifact{
            artifact_id = maps:get(<<"artifactId">>, Map),
            name = maps:get(<<"name">>, Map, undefined),
            description = maps:get(<<"description">>, Map, undefined),
            parts = Parts,
            metadata = maps:get(<<"metadata">>, Map, undefined),
            extensions = maps:get(<<"extensions">>, Map, undefined)
        },
        {ok, Art}
    catch
        _:Reason -> {error, {invalid_artifact, Reason}}
    end;
decode_artifact(_) ->
    {error, invalid_artifact_format}.

%%====================================================================
%% Task Status Encoding/Decoding
%%====================================================================

%% @doc Encode a task status record to a JSON-serializable map
-spec encode_task_status(#a2a_task_status{}) -> map().
encode_task_status(#a2a_task_status{} = Status) ->
    Base = #{
        <<"state">> => task_state_to_binary(Status#a2a_task_status.state)
    },

    WithMessage = case Status#a2a_task_status.message of
        undefined -> Base;
        Msg -> Base#{<<"message">> => encode_message(Msg)}
    end,

    maybe_add(<<"timestamp">>, Status#a2a_task_status.timestamp, WithMessage).

%% @doc Decode a JSON map to a task status record
-spec decode_task_status(map()) -> {ok, #a2a_task_status{}} | {error, term()}.
decode_task_status(Map) when is_map(Map) ->
    try
        Message = case maps:get(<<"message">>, Map, undefined) of
            undefined -> undefined;
            MsgMap ->
                {ok, Msg} = decode_message(MsgMap),
                Msg
        end,

        Status = #a2a_task_status{
            state = binary_to_task_state(maps:get(<<"state">>, Map)),
            message = Message,
            timestamp = maps:get(<<"timestamp">>, Map, undefined)
        },
        {ok, Status}
    catch
        _:Reason -> {error, {invalid_task_status, Reason}}
    end;
decode_task_status(_) ->
    {error, invalid_task_status_format}.

%%====================================================================
%% Task Encoding/Decoding
%%====================================================================

%% @doc Encode a task record to a JSON-serializable map
-spec encode_task(#a2a_task{}) -> map().
encode_task(#a2a_task{} = Task) ->
    Base = #{
        <<"id">> => Task#a2a_task.id,
        <<"contextId">> => Task#a2a_task.context_id,
        <<"status">> => encode_task_status(Task#a2a_task.status)
    },

    WithArtifacts = case Task#a2a_task.artifacts of
        undefined -> Base;
        [] -> Base;
        Arts -> Base#{<<"artifacts">> => [encode_artifact(A) || A <- Arts]}
    end,

    WithHistory = case Task#a2a_task.history of
        undefined -> WithArtifacts;
        [] -> WithArtifacts;
        Hist -> WithArtifacts#{<<"history">> => [encode_message(M) || M <- Hist]}
    end,

    maybe_add(<<"metadata">>, Task#a2a_task.metadata, WithHistory).

%% @doc Decode a JSON map to a task record
-spec decode_task(map()) -> {ok, #a2a_task{}} | {error, term()}.
decode_task(Map) when is_map(Map) ->
    try
        {ok, Status} = decode_task_status(maps:get(<<"status">>, Map)),

        Artifacts = case maps:get(<<"artifacts">>, Map, undefined) of
            undefined -> undefined;
            ArtList ->
                [begin {ok, A} = decode_artifact(AM), A end || AM <- ArtList]
        end,

        History = case maps:get(<<"history">>, Map, undefined) of
            undefined -> undefined;
            HistList ->
                [begin {ok, M} = decode_message(MM), M end || MM <- HistList]
        end,

        Task = #a2a_task{
            id = maps:get(<<"id">>, Map),
            context_id = maps:get(<<"contextId">>, Map),
            status = Status,
            artifacts = Artifacts,
            history = History,
            metadata = maps:get(<<"metadata">>, Map, undefined)
        },
        {ok, Task}
    catch
        _:Reason -> {error, {invalid_task, Reason}}
    end;
decode_task(_) ->
    {error, invalid_task_format}.

%%====================================================================
%% Event Encoding/Decoding
%%====================================================================

%% @doc Encode a task status update event
-spec encode_task_status_update_event(#a2a_task_status_update_event{}) -> map().
encode_task_status_update_event(#a2a_task_status_update_event{} = Event) ->
    Base = #{
        <<"taskId">> => Event#a2a_task_status_update_event.task_id,
        <<"contextId">> => Event#a2a_task_status_update_event.context_id,
        <<"status">> => encode_task_status(Event#a2a_task_status_update_event.status)
    },
    maybe_add(<<"metadata">>, Event#a2a_task_status_update_event.metadata, Base).

%% @doc Decode a task status update event
-spec decode_task_status_update_event(map()) ->
    {ok, #a2a_task_status_update_event{}} | {error, term()}.
decode_task_status_update_event(Map) when is_map(Map) ->
    try
        {ok, Status} = decode_task_status(maps:get(<<"status">>, Map)),
        Event = #a2a_task_status_update_event{
            task_id = maps:get(<<"taskId">>, Map),
            context_id = maps:get(<<"contextId">>, Map),
            status = Status,
            metadata = maps:get(<<"metadata">>, Map, undefined)
        },
        {ok, Event}
    catch
        _:Reason -> {error, {invalid_event, Reason}}
    end;
decode_task_status_update_event(_) ->
    {error, invalid_event_format}.

%% @doc Encode a task artifact update event
-spec encode_task_artifact_update_event(#a2a_task_artifact_update_event{}) -> map().
encode_task_artifact_update_event(#a2a_task_artifact_update_event{} = Event) ->
    Base = #{
        <<"taskId">> => Event#a2a_task_artifact_update_event.task_id,
        <<"contextId">> => Event#a2a_task_artifact_update_event.context_id,
        <<"artifact">> => encode_artifact(Event#a2a_task_artifact_update_event.artifact),
        <<"append">> => Event#a2a_task_artifact_update_event.append,
        <<"lastChunk">> => Event#a2a_task_artifact_update_event.last_chunk
    },
    maybe_add(<<"metadata">>, Event#a2a_task_artifact_update_event.metadata, Base).

%% @doc Decode a task artifact update event
-spec decode_task_artifact_update_event(map()) ->
    {ok, #a2a_task_artifact_update_event{}} | {error, term()}.
decode_task_artifact_update_event(Map) when is_map(Map) ->
    try
        {ok, Artifact} = decode_artifact(maps:get(<<"artifact">>, Map)),
        Event = #a2a_task_artifact_update_event{
            task_id = maps:get(<<"taskId">>, Map),
            context_id = maps:get(<<"contextId">>, Map),
            artifact = Artifact,
            append = maps:get(<<"append">>, Map, false),
            last_chunk = maps:get(<<"lastChunk">>, Map, false),
            metadata = maps:get(<<"metadata">>, Map, undefined)
        },
        {ok, Event}
    catch
        _:Reason -> {error, {invalid_event, Reason}}
    end;
decode_task_artifact_update_event(_) ->
    {error, invalid_event_format}.

%%====================================================================
%% Request Encoding/Decoding
%%====================================================================

%% @doc Encode a send message request
-spec encode_send_message_request(#a2a_send_message_request{}) -> map().
encode_send_message_request(#a2a_send_message_request{} = Req) ->
    Base = #{
        <<"message">> => encode_message(Req#a2a_send_message_request.message)
    },

    WithConfig = case Req#a2a_send_message_request.configuration of
        undefined -> Base;
        Config -> Base#{<<"configuration">> => encode_send_message_configuration(Config)}
    end,

    maybe_add(<<"tenant">>, Req#a2a_send_message_request.tenant,
    maybe_add(<<"metadata">>, Req#a2a_send_message_request.metadata,
    WithConfig)).

%% @doc Decode a send message request
-spec decode_send_message_request(map()) ->
    {ok, #a2a_send_message_request{}} | {error, term()}.
decode_send_message_request(Map) when is_map(Map) ->
    try
        {ok, Message} = decode_message(maps:get(<<"message">>, Map)),

        Configuration = case maps:get(<<"configuration">>, Map, undefined) of
            undefined -> undefined;
            ConfigMap -> decode_send_message_configuration(ConfigMap)
        end,

        Req = #a2a_send_message_request{
            tenant = maps:get(<<"tenant">>, Map, undefined),
            message = Message,
            configuration = Configuration,
            metadata = maps:get(<<"metadata">>, Map, undefined)
        },
        {ok, Req}
    catch
        _:Reason -> {error, {invalid_request, Reason}}
    end;
decode_send_message_request(_) ->
    {error, invalid_request_format}.

%% @doc Encode a get task request
-spec encode_get_task_request(#a2a_get_task_request{}) -> map().
encode_get_task_request(#a2a_get_task_request{} = Req) ->
    Base = #{<<"id">> => Req#a2a_get_task_request.id},
    maybe_add(<<"tenant">>, Req#a2a_get_task_request.tenant,
    maybe_add(<<"historyLength">>, Req#a2a_get_task_request.history_length,
    Base)).

%% @doc Decode a get task request
-spec decode_get_task_request(map()) ->
    {ok, #a2a_get_task_request{}} | {error, term()}.
decode_get_task_request(Map) when is_map(Map) ->
    try
        Req = #a2a_get_task_request{
            tenant = maps:get(<<"tenant">>, Map, undefined),
            id = maps:get(<<"id">>, Map),
            history_length = maps:get(<<"historyLength">>, Map, undefined)
        },
        {ok, Req}
    catch
        _:Reason -> {error, {invalid_request, Reason}}
    end;
decode_get_task_request(_) ->
    {error, invalid_request_format}.

%% @doc Encode a list tasks request
-spec encode_list_tasks_request(#a2a_list_tasks_request{}) -> map().
encode_list_tasks_request(#a2a_list_tasks_request{} = Req) ->
    Base = #{},
    maybe_add(<<"tenant">>, Req#a2a_list_tasks_request.tenant,
    maybe_add(<<"contextId">>, Req#a2a_list_tasks_request.context_id,
    maybe_add(<<"status">>, maybe_state_to_binary(Req#a2a_list_tasks_request.status),
    maybe_add(<<"pageSize">>, Req#a2a_list_tasks_request.page_size,
    maybe_add(<<"pageToken">>, Req#a2a_list_tasks_request.page_token,
    maybe_add(<<"historyLength">>, Req#a2a_list_tasks_request.history_length,
    maybe_add(<<"statusTimestampAfter">>, Req#a2a_list_tasks_request.status_timestamp_after,
    maybe_add(<<"includeArtifacts">>, Req#a2a_list_tasks_request.include_artifacts,
    Base)))))))).

%% @doc Decode a list tasks request
-spec decode_list_tasks_request(map()) ->
    {ok, #a2a_list_tasks_request{}} | {error, term()}.
decode_list_tasks_request(Map) when is_map(Map) ->
    try
        Req = #a2a_list_tasks_request{
            tenant = maps:get(<<"tenant">>, Map, undefined),
            context_id = maps:get(<<"contextId">>, Map, undefined),
            status = maybe_binary_to_state(maps:get(<<"status">>, Map, undefined)),
            page_size = maps:get(<<"pageSize">>, Map, undefined),
            page_token = maps:get(<<"pageToken">>, Map, undefined),
            history_length = maps:get(<<"historyLength">>, Map, undefined),
            status_timestamp_after = maps:get(<<"statusTimestampAfter">>, Map, undefined),
            include_artifacts = maps:get(<<"includeArtifacts">>, Map, undefined)
        },
        {ok, Req}
    catch
        _:Reason -> {error, {invalid_request, Reason}}
    end;
decode_list_tasks_request(_) ->
    {error, invalid_request_format}.

%% @doc Encode a cancel task request
-spec encode_cancel_task_request(#a2a_cancel_task_request{}) -> map().
encode_cancel_task_request(#a2a_cancel_task_request{} = Req) ->
    Base = #{<<"id">> => Req#a2a_cancel_task_request.id},
    maybe_add(<<"tenant">>, Req#a2a_cancel_task_request.tenant, Base).

%% @doc Decode a cancel task request
-spec decode_cancel_task_request(map()) ->
    {ok, #a2a_cancel_task_request{}} | {error, term()}.
decode_cancel_task_request(Map) when is_map(Map) ->
    try
        Req = #a2a_cancel_task_request{
            tenant = maps:get(<<"tenant">>, Map, undefined),
            id = maps:get(<<"id">>, Map)
        },
        {ok, Req}
    catch
        _:Reason -> {error, {invalid_request, Reason}}
    end;
decode_cancel_task_request(_) ->
    {error, invalid_request_format}.

%%====================================================================
%% Response Encoding/Decoding
%%====================================================================

%% @doc Encode a send message response
-spec encode_send_message_response(#a2a_send_message_response{}) -> map().
encode_send_message_response(#a2a_send_message_response{task = Task})
  when Task =/= undefined ->
    #{<<"task">> => encode_task(Task)};
encode_send_message_response(#a2a_send_message_response{message = Msg})
  when Msg =/= undefined ->
    #{<<"message">> => encode_message(Msg)}.

%% @doc Decode a send message response
-spec decode_send_message_response(map()) ->
    {ok, #a2a_send_message_response{}} | {error, term()}.
decode_send_message_response(Map) when is_map(Map) ->
    try
        Response = case Map of
            #{<<"task">> := TaskMap} ->
                {ok, Task} = decode_task(TaskMap),
                #a2a_send_message_response{task = Task};
            #{<<"message">> := MsgMap} ->
                {ok, Msg} = decode_message(MsgMap),
                #a2a_send_message_response{message = Msg};
            _ ->
                {error, missing_payload}
        end,
        case Response of
            {error, _} = Err -> Err;
            _ -> {ok, Response}
        end
    catch
        _:Reason -> {error, {invalid_response, Reason}}
    end;
decode_send_message_response(_) ->
    {error, invalid_response_format}.

%% @doc Encode a stream response
-spec encode_stream_response(#a2a_stream_response{}) -> map().
encode_stream_response(#a2a_stream_response{task = Task})
  when Task =/= undefined ->
    #{<<"task">> => encode_task(Task)};
encode_stream_response(#a2a_stream_response{message = Msg})
  when Msg =/= undefined ->
    #{<<"message">> => encode_message(Msg)};
encode_stream_response(#a2a_stream_response{status_update = Update})
  when Update =/= undefined ->
    #{<<"statusUpdate">> => encode_task_status_update_event(Update)};
encode_stream_response(#a2a_stream_response{artifact_update = Update})
  when Update =/= undefined ->
    #{<<"artifactUpdate">> => encode_task_artifact_update_event(Update)}.

%% @doc Decode a stream response
-spec decode_stream_response(map()) ->
    {ok, #a2a_stream_response{}} | {error, term()}.
decode_stream_response(Map) when is_map(Map) ->
    try
        Response = case Map of
            #{<<"task">> := TaskMap} ->
                {ok, Task} = decode_task(TaskMap),
                #a2a_stream_response{task = Task};
            #{<<"message">> := MsgMap} ->
                {ok, Msg} = decode_message(MsgMap),
                #a2a_stream_response{message = Msg};
            #{<<"statusUpdate">> := UpdateMap} ->
                {ok, Update} = decode_task_status_update_event(UpdateMap),
                #a2a_stream_response{status_update = Update};
            #{<<"artifactUpdate">> := UpdateMap} ->
                {ok, Update} = decode_task_artifact_update_event(UpdateMap),
                #a2a_stream_response{artifact_update = Update};
            _ ->
                {error, missing_payload}
        end,
        case Response of
            {error, _} = Err -> Err;
            _ -> {ok, Response}
        end
    catch
        _:Reason -> {error, {invalid_response, Reason}}
    end;
decode_stream_response(_) ->
    {error, invalid_response_format}.

%% @doc Encode a list tasks response
-spec encode_list_tasks_response(#a2a_list_tasks_response{}) -> map().
encode_list_tasks_response(#a2a_list_tasks_response{} = Resp) ->
    #{
        <<"tasks">> => [encode_task(T) || T <- Resp#a2a_list_tasks_response.tasks],
        <<"nextPageToken">> => Resp#a2a_list_tasks_response.next_page_token,
        <<"pageSize">> => Resp#a2a_list_tasks_response.page_size,
        <<"totalSize">> => Resp#a2a_list_tasks_response.total_size
    }.

%% @doc Decode a list tasks response
-spec decode_list_tasks_response(map()) ->
    {ok, #a2a_list_tasks_response{}} | {error, term()}.
decode_list_tasks_response(Map) when is_map(Map) ->
    try
        TaskMaps = maps:get(<<"tasks">>, Map, []),
        Tasks = [begin {ok, T} = decode_task(TM), T end || TM <- TaskMaps],

        Resp = #a2a_list_tasks_response{
            tasks = Tasks,
            next_page_token = maps:get(<<"nextPageToken">>, Map, <<>>),
            page_size = maps:get(<<"pageSize">>, Map, 0),
            total_size = maps:get(<<"totalSize">>, Map, 0)
        },
        {ok, Resp}
    catch
        _:Reason -> {error, {invalid_response, Reason}}
    end;
decode_list_tasks_response(_) ->
    {error, invalid_response_format}.

%%====================================================================
%% Agent Card Encoding/Decoding
%%====================================================================

%% @doc Encode an agent card
-spec encode_agent_card(#a2a_agent_card{}) -> map().
encode_agent_card(#a2a_agent_card{} = Card) ->
    Base = #{
        <<"name">> => Card#a2a_agent_card.name,
        <<"description">> => Card#a2a_agent_card.description,
        <<"supportedInterfaces">> => [encode_agent_interface(I) ||
                                       I <- Card#a2a_agent_card.supported_interfaces],
        <<"version">> => Card#a2a_agent_card.version,
        <<"capabilities">> => encode_agent_capabilities(Card#a2a_agent_card.capabilities),
        <<"defaultInputModes">> => Card#a2a_agent_card.default_input_modes,
        <<"defaultOutputModes">> => Card#a2a_agent_card.default_output_modes,
        <<"skills">> => [encode_agent_skill(S) || S <- Card#a2a_agent_card.skills]
    },

    WithProvider = case Card#a2a_agent_card.provider of
        undefined -> Base;
        Prov -> Base#{<<"provider">> => encode_agent_provider(Prov)}
    end,

    maybe_add(<<"documentationUrl">>, Card#a2a_agent_card.documentation_url,
    maybe_add(<<"securitySchemes">>, encode_security_schemes(Card#a2a_agent_card.security_schemes),
    maybe_add(<<"securityRequirements">>, Card#a2a_agent_card.security_requirements,
    maybe_add(<<"signatures">>, encode_signatures(Card#a2a_agent_card.signatures),
    maybe_add(<<"iconUrl">>, Card#a2a_agent_card.icon_url,
    WithProvider))))).

%% @doc Decode an agent card
-spec decode_agent_card(map()) -> {ok, #a2a_agent_card{}} | {error, term()}.
decode_agent_card(Map) when is_map(Map) ->
    try
        Interfaces = [decode_agent_interface_direct(I) ||
                      I <- maps:get(<<"supportedInterfaces">>, Map, [])],
        Skills = [decode_agent_skill_direct(S) ||
                  S <- maps:get(<<"skills">>, Map, [])],

        Card = #a2a_agent_card{
            name = maps:get(<<"name">>, Map),
            description = maps:get(<<"description">>, Map),
            supported_interfaces = Interfaces,
            provider = decode_agent_provider_opt(maps:get(<<"provider">>, Map, undefined)),
            version = maps:get(<<"version">>, Map),
            documentation_url = maps:get(<<"documentationUrl">>, Map, undefined),
            capabilities = decode_agent_capabilities(maps:get(<<"capabilities">>, Map, #{})),
            security_schemes = decode_security_schemes(maps:get(<<"securitySchemes">>, Map, undefined)),
            security_requirements = maps:get(<<"securityRequirements">>, Map, undefined),
            default_input_modes = maps:get(<<"defaultInputModes">>, Map, []),
            default_output_modes = maps:get(<<"defaultOutputModes">>, Map, []),
            skills = Skills,
            signatures = decode_signatures(maps:get(<<"signatures">>, Map, undefined)),
            icon_url = maps:get(<<"iconUrl">>, Map, undefined)
        },
        {ok, Card}
    catch
        _:Reason -> {error, {invalid_agent_card, Reason}}
    end;
decode_agent_card(_) ->
    {error, invalid_agent_card_format}.

%% @doc Encode an agent skill
-spec encode_agent_skill(#a2a_agent_skill{}) -> map().
encode_agent_skill(#a2a_agent_skill{} = Skill) ->
    Base = #{
        <<"id">> => Skill#a2a_agent_skill.id,
        <<"name">> => Skill#a2a_agent_skill.name,
        <<"description">> => Skill#a2a_agent_skill.description,
        <<"tags">> => Skill#a2a_agent_skill.tags
    },

    maybe_add(<<"examples">>, Skill#a2a_agent_skill.examples,
    maybe_add(<<"inputModes">>, Skill#a2a_agent_skill.input_modes,
    maybe_add(<<"outputModes">>, Skill#a2a_agent_skill.output_modes,
    maybe_add(<<"securityRequirements">>, Skill#a2a_agent_skill.security_requirements,
    Base)))).

%% @doc Decode an agent skill
-spec decode_agent_skill(map()) -> {ok, #a2a_agent_skill{}} | {error, term()}.
decode_agent_skill(Map) when is_map(Map) ->
    try
        {ok, decode_agent_skill_direct(Map)}
    catch
        _:Reason -> {error, {invalid_skill, Reason}}
    end;
decode_agent_skill(_) ->
    {error, invalid_skill_format}.

%% @doc Encode an agent interface
-spec encode_agent_interface(#a2a_agent_interface{}) -> map().
encode_agent_interface(#a2a_agent_interface{} = Interface) ->
    Base = #{
        <<"url">> => Interface#a2a_agent_interface.url,
        <<"protocolBinding">> => Interface#a2a_agent_interface.protocol_binding,
        <<"protocolVersion">> => Interface#a2a_agent_interface.protocol_version
    },
    maybe_add(<<"tenant">>, Interface#a2a_agent_interface.tenant, Base).

%% @doc Decode an agent interface
-spec decode_agent_interface(map()) -> {ok, #a2a_agent_interface{}} | {error, term()}.
decode_agent_interface(Map) when is_map(Map) ->
    try
        {ok, decode_agent_interface_direct(Map)}
    catch
        _:Reason -> {error, {invalid_interface, Reason}}
    end;
decode_agent_interface(_) ->
    {error, invalid_interface_format}.

%%====================================================================
%% Push Notification Encoding/Decoding
%%====================================================================

%% @doc Encode a push notification config
-spec encode_push_notification_config(#a2a_push_notification_config{}) -> map().
encode_push_notification_config(#a2a_push_notification_config{} = Config) ->
    Base = #{<<"url">> => Config#a2a_push_notification_config.url},

    WithAuth = case Config#a2a_push_notification_config.authentication of
        undefined -> Base;
        Auth -> Base#{<<"authentication">> => encode_authentication_info(Auth)}
    end,

    maybe_add(<<"id">>, Config#a2a_push_notification_config.id,
    maybe_add(<<"token">>, Config#a2a_push_notification_config.token,
    WithAuth)).

%% @doc Decode a push notification config
-spec decode_push_notification_config(map()) ->
    {ok, #a2a_push_notification_config{}} | {error, term()}.
decode_push_notification_config(Map) when is_map(Map) ->
    try
        Auth = case maps:get(<<"authentication">>, Map, undefined) of
            undefined -> undefined;
            AuthMap -> decode_authentication_info(AuthMap)
        end,

        Config = #a2a_push_notification_config{
            id = maps:get(<<"id">>, Map, undefined),
            url = maps:get(<<"url">>, Map),
            token = maps:get(<<"token">>, Map, undefined),
            authentication = Auth
        },
        {ok, Config}
    catch
        _:Reason -> {error, {invalid_config, Reason}}
    end;
decode_push_notification_config(_) ->
    {error, invalid_config_format}.

%%====================================================================
%% State Conversion Utilities
%%====================================================================

%% @doc Convert task state atom to binary
-spec task_state_to_binary(a2a_task_state()) -> binary().
task_state_to_binary(unspecified) -> <<"TASK_STATE_UNSPECIFIED">>;
task_state_to_binary(submitted) -> <<"TASK_STATE_SUBMITTED">>;
task_state_to_binary(working) -> <<"TASK_STATE_WORKING">>;
task_state_to_binary(completed) -> <<"TASK_STATE_COMPLETED">>;
task_state_to_binary(failed) -> <<"TASK_STATE_FAILED">>;
task_state_to_binary(canceled) -> <<"TASK_STATE_CANCELED">>;
task_state_to_binary(input_required) -> <<"TASK_STATE_INPUT_REQUIRED">>;
task_state_to_binary(rejected) -> <<"TASK_STATE_REJECTED">>;
task_state_to_binary(auth_required) -> <<"TASK_STATE_AUTH_REQUIRED">>.

%% @doc Convert binary to task state atom
-spec binary_to_task_state(binary()) -> a2a_task_state().
binary_to_task_state(<<"TASK_STATE_UNSPECIFIED">>) -> unspecified;
binary_to_task_state(<<"TASK_STATE_SUBMITTED">>) -> submitted;
binary_to_task_state(<<"TASK_STATE_WORKING">>) -> working;
binary_to_task_state(<<"TASK_STATE_COMPLETED">>) -> completed;
binary_to_task_state(<<"TASK_STATE_FAILED">>) -> failed;
binary_to_task_state(<<"TASK_STATE_CANCELED">>) -> canceled;
binary_to_task_state(<<"TASK_STATE_INPUT_REQUIRED">>) -> input_required;
binary_to_task_state(<<"TASK_STATE_REJECTED">>) -> rejected;
binary_to_task_state(<<"TASK_STATE_AUTH_REQUIRED">>) -> auth_required;
%% Also support lowercase/camelCase variants for flexibility
binary_to_task_state(<<"submitted">>) -> submitted;
binary_to_task_state(<<"working">>) -> working;
binary_to_task_state(<<"completed">>) -> completed;
binary_to_task_state(<<"failed">>) -> failed;
binary_to_task_state(<<"canceled">>) -> canceled;
binary_to_task_state(<<"input_required">>) -> input_required;
binary_to_task_state(<<"inputRequired">>) -> input_required;
binary_to_task_state(<<"rejected">>) -> rejected;
binary_to_task_state(<<"auth_required">>) -> auth_required;
binary_to_task_state(<<"authRequired">>) -> auth_required;
binary_to_task_state(_) -> unspecified.

%% @doc Convert role atom to binary
-spec role_to_binary(a2a_role()) -> binary().
role_to_binary(unspecified) -> <<"ROLE_UNSPECIFIED">>;
role_to_binary(user) -> <<"ROLE_USER">>;
role_to_binary(agent) -> <<"ROLE_AGENT">>.

%% @doc Convert binary to role atom
-spec binary_to_role(binary()) -> a2a_role().
binary_to_role(<<"ROLE_UNSPECIFIED">>) -> unspecified;
binary_to_role(<<"ROLE_USER">>) -> user;
binary_to_role(<<"ROLE_AGENT">>) -> agent;
binary_to_role(<<"user">>) -> user;
binary_to_role(<<"agent">>) -> agent;
binary_to_role(_) -> unspecified.

%%====================================================================
%% Validation Functions
%%====================================================================

%% @doc Validate a message structure
-spec validate_message(#a2a_message{}) -> ok | {error, term()}.
validate_message(#a2a_message{message_id = undefined}) ->
    {error, missing_message_id};
validate_message(#a2a_message{role = unspecified}) ->
    {error, invalid_role};
validate_message(#a2a_message{parts = []}) ->
    {error, empty_parts};
validate_message(#a2a_message{parts = Parts}) when is_list(Parts) ->
    case lists:all(fun(P) -> validate_part(P) =:= ok end, Parts) of
        true -> ok;
        false -> {error, invalid_parts}
    end;
validate_message(_) ->
    {error, invalid_message}.

%% @doc Validate a task structure
-spec validate_task(#a2a_task{}) -> ok | {error, term()}.
validate_task(#a2a_task{id = undefined}) ->
    {error, missing_task_id};
validate_task(#a2a_task{context_id = undefined}) ->
    {error, missing_context_id};
validate_task(#a2a_task{status = undefined}) ->
    {error, missing_status};
validate_task(#a2a_task{}) ->
    ok;
validate_task(_) ->
    {error, invalid_task}.

%% @doc Validate a part structure
-spec validate_part(#a2a_part{}) -> ok | {error, term()}.
validate_part(#a2a_part{text = T, raw = R, url = U, data = D})
  when T =:= undefined, R =:= undefined, U =:= undefined, D =:= undefined ->
    {error, empty_part_content};
validate_part(#a2a_part{}) ->
    ok;
validate_part(_) ->
    {error, invalid_part}.

%% @doc Check if a task state is terminal
-spec is_terminal_state(a2a_task_state()) -> boolean().
is_terminal_state(State) ->
    lists:member(State, ?A2A_TERMINAL_STATES).

%% @doc Check if a task state is interrupted
-spec is_interrupted_state(a2a_task_state()) -> boolean().
is_interrupted_state(State) ->
    lists:member(State, ?A2A_INTERRUPTED_STATES).

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @private Add field to map if value is not undefined
maybe_add(_Key, undefined, Map) -> Map;
maybe_add(Key, Value, Map) -> Map#{Key => Value}.

%% @private Decode base64 raw data
decode_raw(undefined) -> undefined;
decode_raw(Data) when is_binary(Data) ->
    try base64:decode(Data)
    catch _:_ -> Data  % If not base64, return as-is
    end.

%% @private Encode send message configuration
encode_send_message_configuration(#a2a_send_message_configuration{} = Config) ->
    Base = #{<<"blocking">> => Config#a2a_send_message_configuration.blocking},

    WithPush = case Config#a2a_send_message_configuration.push_notification_config of
        undefined -> Base;
        PushConfig -> Base#{<<"pushNotificationConfig">> => encode_push_notification_config(PushConfig)}
    end,

    maybe_add(<<"acceptedOutputModes">>, Config#a2a_send_message_configuration.accepted_output_modes,
    maybe_add(<<"historyLength">>, Config#a2a_send_message_configuration.history_length,
    WithPush)).

%% @private Decode send message configuration
decode_send_message_configuration(Map) when is_map(Map) ->
    PushConfig = case maps:get(<<"pushNotificationConfig">>, Map, undefined) of
        undefined -> undefined;
        PushMap ->
            {ok, Config} = decode_push_notification_config(PushMap),
            Config
    end,

    #a2a_send_message_configuration{
        accepted_output_modes = maps:get(<<"acceptedOutputModes">>, Map, undefined),
        push_notification_config = PushConfig,
        history_length = maps:get(<<"historyLength">>, Map, undefined),
        blocking = maps:get(<<"blocking">>, Map, false)
    }.

%% @private Maybe convert state to binary
maybe_state_to_binary(undefined) -> undefined;
maybe_state_to_binary(State) -> task_state_to_binary(State).

%% @private Maybe convert binary to state
maybe_binary_to_state(undefined) -> undefined;
maybe_binary_to_state(Bin) -> binary_to_task_state(Bin).

%% @private Encode agent capabilities
encode_agent_capabilities(#a2a_agent_capabilities{} = Caps) ->
    Base = #{},
    maybe_add(<<"streaming">>, Caps#a2a_agent_capabilities.streaming,
    maybe_add(<<"pushNotifications">>, Caps#a2a_agent_capabilities.push_notifications,
    maybe_add(<<"extensions">>, encode_extensions(Caps#a2a_agent_capabilities.extensions),
    maybe_add(<<"extendedAgentCard">>, Caps#a2a_agent_capabilities.extended_agent_card,
    Base)))).

%% @private Decode agent capabilities
decode_agent_capabilities(Map) when is_map(Map) ->
    #a2a_agent_capabilities{
        streaming = maps:get(<<"streaming">>, Map, undefined),
        push_notifications = maps:get(<<"pushNotifications">>, Map, undefined),
        extensions = decode_extensions(maps:get(<<"extensions">>, Map, undefined)),
        extended_agent_card = maps:get(<<"extendedAgentCard">>, Map, undefined)
    };
decode_agent_capabilities(_) ->
    #a2a_agent_capabilities{}.

%% @private Encode extensions list
encode_extensions(undefined) -> undefined;
encode_extensions([]) -> undefined;
encode_extensions(Extensions) ->
    [encode_agent_extension(E) || E <- Extensions].

%% @private Encode agent extension
encode_agent_extension(#a2a_agent_extension{} = Ext) ->
    Base = #{<<"uri">> => Ext#a2a_agent_extension.uri},
    maybe_add(<<"description">>, Ext#a2a_agent_extension.description,
    maybe_add(<<"required">>, Ext#a2a_agent_extension.required,
    maybe_add(<<"params">>, Ext#a2a_agent_extension.params,
    Base))).

%% @private Decode extensions list
decode_extensions(undefined) -> undefined;
decode_extensions([]) -> undefined;
decode_extensions(Extensions) ->
    [decode_agent_extension(E) || E <- Extensions].

%% @private Decode agent extension
decode_agent_extension(Map) when is_map(Map) ->
    #a2a_agent_extension{
        uri = maps:get(<<"uri">>, Map),
        description = maps:get(<<"description">>, Map, undefined),
        required = maps:get(<<"required">>, Map, false),
        params = maps:get(<<"params">>, Map, undefined)
    }.

%% @private Encode agent provider
encode_agent_provider(#a2a_agent_provider{} = Prov) ->
    #{
        <<"url">> => Prov#a2a_agent_provider.url,
        <<"organization">> => Prov#a2a_agent_provider.organization
    }.

%% @private Decode agent provider (optional)
decode_agent_provider_opt(undefined) -> undefined;
decode_agent_provider_opt(Map) when is_map(Map) ->
    #a2a_agent_provider{
        url = maps:get(<<"url">>, Map),
        organization = maps:get(<<"organization">>, Map)
    }.

%% @private Direct decode of agent interface (no error tuple)
decode_agent_interface_direct(Map) when is_map(Map) ->
    #a2a_agent_interface{
        url = maps:get(<<"url">>, Map),
        protocol_binding = maps:get(<<"protocolBinding">>, Map),
        tenant = maps:get(<<"tenant">>, Map, undefined),
        protocol_version = maps:get(<<"protocolVersion">>, Map)
    }.

%% @private Direct decode of agent skill (no error tuple)
decode_agent_skill_direct(Map) when is_map(Map) ->
    #a2a_agent_skill{
        id = maps:get(<<"id">>, Map),
        name = maps:get(<<"name">>, Map),
        description = maps:get(<<"description">>, Map),
        tags = maps:get(<<"tags">>, Map, []),
        examples = maps:get(<<"examples">>, Map, undefined),
        input_modes = maps:get(<<"inputModes">>, Map, undefined),
        output_modes = maps:get(<<"outputModes">>, Map, undefined),
        security_requirements = maps:get(<<"securityRequirements">>, Map, undefined)
    }.

%% @private Encode authentication info
encode_authentication_info(#a2a_authentication_info{} = Auth) ->
    Base = #{<<"scheme">> => Auth#a2a_authentication_info.scheme},
    maybe_add(<<"credentials">>, Auth#a2a_authentication_info.credentials, Base).

%% @private Decode authentication info
decode_authentication_info(Map) when is_map(Map) ->
    #a2a_authentication_info{
        scheme = maps:get(<<"scheme">>, Map),
        credentials = maps:get(<<"credentials">>, Map, undefined)
    }.

%% @private Encode security schemes map
encode_security_schemes(undefined) -> undefined;
encode_security_schemes(Schemes) when is_map(Schemes) ->
    maps:map(fun(_K, V) -> encode_security_scheme(V) end, Schemes).

%% @private Encode a single security scheme
encode_security_scheme(#a2a_security_scheme{api_key = ApiKey}) when ApiKey =/= undefined ->
    #{<<"apiKeySecurityScheme">> => encode_api_key_scheme(ApiKey)};
encode_security_scheme(#a2a_security_scheme{http_auth = HttpAuth}) when HttpAuth =/= undefined ->
    #{<<"httpAuthSecurityScheme">> => encode_http_auth_scheme(HttpAuth)};
encode_security_scheme(#a2a_security_scheme{oauth2 = OAuth2}) when OAuth2 =/= undefined ->
    #{<<"oauth2SecurityScheme">> => encode_oauth2_scheme(OAuth2)};
encode_security_scheme(#a2a_security_scheme{openid_connect = OpenId}) when OpenId =/= undefined ->
    #{<<"openIdConnectSecurityScheme">> => encode_openid_scheme(OpenId)};
encode_security_scheme(#a2a_security_scheme{mtls = Mtls}) when Mtls =/= undefined ->
    #{<<"mtlsSecurityScheme">> => encode_mtls_scheme(Mtls)};
encode_security_scheme(_) ->
    #{}.

%% @private Encode API key scheme
encode_api_key_scheme(#a2a_api_key_security_scheme{} = Scheme) ->
    Base = #{
        <<"location">> => Scheme#a2a_api_key_security_scheme.location,
        <<"name">> => Scheme#a2a_api_key_security_scheme.name
    },
    maybe_add(<<"description">>, Scheme#a2a_api_key_security_scheme.description, Base).

%% @private Encode HTTP auth scheme
encode_http_auth_scheme(#a2a_http_auth_security_scheme{} = Scheme) ->
    Base = #{<<"scheme">> => Scheme#a2a_http_auth_security_scheme.scheme},
    maybe_add(<<"description">>, Scheme#a2a_http_auth_security_scheme.description,
    maybe_add(<<"bearerFormat">>, Scheme#a2a_http_auth_security_scheme.bearer_format,
    Base)).

%% @private Encode OAuth2 scheme
encode_oauth2_scheme(#a2a_oauth2_security_scheme{} = Scheme) ->
    Base = #{<<"flows">> => encode_oauth_flows(Scheme#a2a_oauth2_security_scheme.flows)},
    maybe_add(<<"description">>, Scheme#a2a_oauth2_security_scheme.description,
    maybe_add(<<"oauth2MetadataUrl">>, Scheme#a2a_oauth2_security_scheme.oauth2_metadata_url,
    Base)).

%% @private Encode OAuth flows
encode_oauth_flows(#a2a_oauth_flows{authorization_code = AuthCode}) when AuthCode =/= undefined ->
    #{<<"authorizationCode">> => encode_auth_code_flow(AuthCode)};
encode_oauth_flows(#a2a_oauth_flows{client_credentials = ClientCreds}) when ClientCreds =/= undefined ->
    #{<<"clientCredentials">> => encode_client_creds_flow(ClientCreds)};
encode_oauth_flows(#a2a_oauth_flows{device_code = DeviceCode}) when DeviceCode =/= undefined ->
    #{<<"deviceCode">> => encode_device_code_flow(DeviceCode)};
encode_oauth_flows(_) ->
    #{}.

%% @private Encode authorization code flow
encode_auth_code_flow(#a2a_authorization_code_oauth_flow{} = Flow) ->
    Base = #{
        <<"authorizationUrl">> => Flow#a2a_authorization_code_oauth_flow.authorization_url,
        <<"tokenUrl">> => Flow#a2a_authorization_code_oauth_flow.token_url,
        <<"scopes">> => Flow#a2a_authorization_code_oauth_flow.scopes
    },
    maybe_add(<<"refreshUrl">>, Flow#a2a_authorization_code_oauth_flow.refresh_url,
    maybe_add(<<"pkceRequired">>, Flow#a2a_authorization_code_oauth_flow.pkce_required,
    Base)).

%% @private Encode client credentials flow
encode_client_creds_flow(#a2a_client_credentials_oauth_flow{} = Flow) ->
    Base = #{
        <<"tokenUrl">> => Flow#a2a_client_credentials_oauth_flow.token_url,
        <<"scopes">> => Flow#a2a_client_credentials_oauth_flow.scopes
    },
    maybe_add(<<"refreshUrl">>, Flow#a2a_client_credentials_oauth_flow.refresh_url, Base).

%% @private Encode device code flow
encode_device_code_flow(#a2a_device_code_oauth_flow{} = Flow) ->
    Base = #{
        <<"deviceAuthorizationUrl">> => Flow#a2a_device_code_oauth_flow.device_authorization_url,
        <<"tokenUrl">> => Flow#a2a_device_code_oauth_flow.token_url,
        <<"scopes">> => Flow#a2a_device_code_oauth_flow.scopes
    },
    maybe_add(<<"refreshUrl">>, Flow#a2a_device_code_oauth_flow.refresh_url, Base).

%% @private Encode OpenID Connect scheme
encode_openid_scheme(#a2a_openid_connect_security_scheme{} = Scheme) ->
    Base = #{<<"openIdConnectUrl">> => Scheme#a2a_openid_connect_security_scheme.open_id_connect_url},
    maybe_add(<<"description">>, Scheme#a2a_openid_connect_security_scheme.description, Base).

%% @private Encode mTLS scheme
encode_mtls_scheme(#a2a_mtls_security_scheme{} = Scheme) ->
    maybe_add(<<"description">>, Scheme#a2a_mtls_security_scheme.description, #{}).

%% @private Decode security schemes
decode_security_schemes(undefined) -> undefined;
decode_security_schemes(Schemes) when is_map(Schemes) ->
    maps:map(fun(_K, V) -> decode_security_scheme(V) end, Schemes).

%% @private Decode a single security scheme
decode_security_scheme(#{<<"apiKeySecurityScheme">> := ApiKey}) ->
    #a2a_security_scheme{api_key = decode_api_key_scheme(ApiKey)};
decode_security_scheme(#{<<"httpAuthSecurityScheme">> := HttpAuth}) ->
    #a2a_security_scheme{http_auth = decode_http_auth_scheme(HttpAuth)};
decode_security_scheme(#{<<"oauth2SecurityScheme">> := OAuth2}) ->
    #a2a_security_scheme{oauth2 = decode_oauth2_scheme(OAuth2)};
decode_security_scheme(#{<<"openIdConnectSecurityScheme">> := OpenId}) ->
    #a2a_security_scheme{openid_connect = decode_openid_scheme(OpenId)};
decode_security_scheme(#{<<"mtlsSecurityScheme">> := Mtls}) ->
    #a2a_security_scheme{mtls = decode_mtls_scheme(Mtls)};
decode_security_scheme(_) ->
    #a2a_security_scheme{}.

%% @private Decode API key scheme
decode_api_key_scheme(Map) ->
    #a2a_api_key_security_scheme{
        description = maps:get(<<"description">>, Map, undefined),
        location = maps:get(<<"location">>, Map),
        name = maps:get(<<"name">>, Map)
    }.

%% @private Decode HTTP auth scheme
decode_http_auth_scheme(Map) ->
    #a2a_http_auth_security_scheme{
        description = maps:get(<<"description">>, Map, undefined),
        scheme = maps:get(<<"scheme">>, Map),
        bearer_format = maps:get(<<"bearerFormat">>, Map, undefined)
    }.

%% @private Decode OAuth2 scheme
decode_oauth2_scheme(Map) ->
    #a2a_oauth2_security_scheme{
        description = maps:get(<<"description">>, Map, undefined),
        flows = decode_oauth_flows(maps:get(<<"flows">>, Map, #{})),
        oauth2_metadata_url = maps:get(<<"oauth2MetadataUrl">>, Map, undefined)
    }.

%% @private Decode OAuth flows
decode_oauth_flows(#{<<"authorizationCode">> := AuthCode}) ->
    #a2a_oauth_flows{authorization_code = decode_auth_code_flow(AuthCode)};
decode_oauth_flows(#{<<"clientCredentials">> := ClientCreds}) ->
    #a2a_oauth_flows{client_credentials = decode_client_creds_flow(ClientCreds)};
decode_oauth_flows(#{<<"deviceCode">> := DeviceCode}) ->
    #a2a_oauth_flows{device_code = decode_device_code_flow(DeviceCode)};
decode_oauth_flows(_) ->
    #a2a_oauth_flows{}.

%% @private Decode authorization code flow
decode_auth_code_flow(Map) ->
    #a2a_authorization_code_oauth_flow{
        authorization_url = maps:get(<<"authorizationUrl">>, Map),
        token_url = maps:get(<<"tokenUrl">>, Map),
        refresh_url = maps:get(<<"refreshUrl">>, Map, undefined),
        scopes = maps:get(<<"scopes">>, Map, #{}),
        pkce_required = maps:get(<<"pkceRequired">>, Map, undefined)
    }.

%% @private Decode client credentials flow
decode_client_creds_flow(Map) ->
    #a2a_client_credentials_oauth_flow{
        token_url = maps:get(<<"tokenUrl">>, Map),
        refresh_url = maps:get(<<"refreshUrl">>, Map, undefined),
        scopes = maps:get(<<"scopes">>, Map, #{})
    }.

%% @private Decode device code flow
decode_device_code_flow(Map) ->
    #a2a_device_code_oauth_flow{
        device_authorization_url = maps:get(<<"deviceAuthorizationUrl">>, Map),
        token_url = maps:get(<<"tokenUrl">>, Map),
        refresh_url = maps:get(<<"refreshUrl">>, Map, undefined),
        scopes = maps:get(<<"scopes">>, Map, #{})
    }.

%% @private Decode OpenID Connect scheme
decode_openid_scheme(Map) ->
    #a2a_openid_connect_security_scheme{
        description = maps:get(<<"description">>, Map, undefined),
        open_id_connect_url = maps:get(<<"openIdConnectUrl">>, Map)
    }.

%% @private Decode mTLS scheme
decode_mtls_scheme(Map) ->
    #a2a_mtls_security_scheme{
        description = maps:get(<<"description">>, Map, undefined)
    }.

%% @private Encode signatures
encode_signatures(undefined) -> undefined;
encode_signatures([]) -> undefined;
encode_signatures(Sigs) ->
    [encode_signature(S) || S <- Sigs].

%% @private Encode a single signature
encode_signature(#a2a_agent_card_signature{} = Sig) ->
    Base = #{
        <<"protected">> => Sig#a2a_agent_card_signature.protected,
        <<"signature">> => Sig#a2a_agent_card_signature.signature
    },
    maybe_add(<<"header">>, Sig#a2a_agent_card_signature.header, Base).

%% @private Decode signatures
decode_signatures(undefined) -> undefined;
decode_signatures([]) -> undefined;
decode_signatures(Sigs) ->
    [decode_signature(S) || S <- Sigs].

%% @private Decode a single signature
decode_signature(Map) when is_map(Map) ->
    #a2a_agent_card_signature{
        protected = maps:get(<<"protected">>, Map),
        signature = maps:get(<<"signature">>, Map),
        header = maps:get(<<"header">>, Map, undefined)
    }.
