%%%-------------------------------------------------------------------
%% @doc HTTP DELETE Handler for MCP 2025-11-25 Compliance
%%
%% Implements Gap #28: HTTP DELETE method support for resource removal
%% Supports:
%% - DELETE /mcp -> terminate session
%% - DELETE /mcp/resources/{uri} -> remove resource
%% - DELETE /mcp/tools/{name} -> remove tool
%% - DELETE /mcp/prompts/{name} -> remove prompt
%%
%% Returns:
%% - 204 No Content on successful deletion
%% - 404 Not Found if resource doesn't exist
%% - 400 Bad Request if validation fails
%% - 500 Internal Error if server error occurs
%%
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_http_delete_handler).

-include("erlmcp.hrl").
-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API
-export([
    handle_delete/3,
    handle_delete_by_path/5,
    handle_delete_session_termination/4,
    handle_delete_resource/5,
    handle_delete_tool/5,
    handle_delete_prompt/5
]).

%% Internal exports
-export([
    error_response/5,
    get_server_for_session/1,
    delete_resource_from_server/2,
    delete_tool_from_server/2,
    delete_prompt_from_server/2
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Main DELETE request handler
%% @param Req - Cowboy request object
%% @param TransportId - Transport ID (unused in delete handler)
%% @param State - Handler state
%% @returns {ok, Req, State} tuple
-spec handle_delete(term(), binary(), term()) -> {ok, term(), term()}.
handle_delete(Req, _TransportId, State) ->
    SpanCtx = erlmcp_tracing:start_span(<<"http_delete_handler.handle_delete">>),

    try
        %% Extract session ID from header
        SessionId = cowboy_req:header(<<"mcp-session-id">>, Req, undefined),

        case SessionId of
            undefined ->
                %% No session ID provided
                erlmcp_tracing:record_error_details(SpanCtx, missing_session_id, <<"Missing MCP-Session-Id header">>),
                error_response(Req, 400, <<"Bad Request">>, <<"Missing MCP-Session-Id header">>, State);
            _ ->
                %% Validate session before proceeding
                case erlmcp_session_manager:validate_session(SessionId) of
                    {ok, _SessionInfo} ->
                        %% Parse path to determine deletion type
                        Path = erlang:binary_to_list(cowboy_req:path(Req)),
                        handle_delete_by_path(Path, Req, SessionId, SpanCtx, State);
                    {error, Reason} ->
                        %% Session not found or expired
                        erlmcp_tracing:record_error_details(SpanCtx, invalid_session, Reason),
                        error_response(Req, 404, <<"Not Found">>, <<"Session not found or expired">>, State)
                end
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            error_response(Req, 500, <<"Internal Error">>, <<"Failed to process DELETE request">>, State)
    after
        erlmcp_tracing:end_span(SpanCtx)
    end.

%% @doc Route DELETE request based on path
%% @private
-spec handle_delete_by_path(string(), term(), binary(), term(), term()) -> {ok, term(), term()}.
handle_delete_by_path(Path, Req, SessionId, SpanCtx, State) ->
    case Path of
        "/mcp" ->
            %% Delete session
            handle_delete_session_termination(Req, SessionId, SpanCtx, State);
        "/mcp/" ->
            %% Delete session (with trailing slash)
            handle_delete_session_termination(Req, SessionId, SpanCtx, State);
        PathStr when is_list(PathStr) ->
            %% Check for resource/tool/prompt deletion
            case string:split(PathStr, "/") of
                [_, "mcp", "resources" | ResourcePath] ->
                    %% DELETE /mcp/resources/{uri}
                    handle_delete_resource(Req, ResourcePath, SessionId, SpanCtx, State);
                [_, "mcp", "tools", Name] ->
                    %% DELETE /mcp/tools/{name}
                    handle_delete_tool(Req, Name, SessionId, SpanCtx, State);
                [_, "mcp", "prompts", Name] ->
                    %% DELETE /mcp/prompts/{name}
                    handle_delete_prompt(Req, Name, SessionId, SpanCtx, State);
                _ ->
                    %% Unknown path
                    erlmcp_tracing:record_error_details(SpanCtx, invalid_path, PathStr),
                    error_response(Req, 404, <<"Not Found">>, <<"Invalid resource path">>, State)
            end
    end.

%% @doc Handle session termination (DELETE /mcp)
%% @private
-spec handle_delete_session_termination(term(), binary(), term(), term()) -> {ok, term(), term()}.
handle_delete_session_termination(Req, SessionId, SpanCtx, State) ->
    erlmcp_tracing:set_attributes(SpanCtx, #{
        <<"operation">> => <<"delete_session">>,
        <<"session_id">> => SessionId
    }),

    %% Delete session
    erlmcp_session_manager:delete_session(SessionId),

    %% Close any active streams for this session
    RegistryPid = erlmcp_registry:get_pid(),
    RegistryPid ! {session_terminated, SessionId},

    erlmcp_tracing:set_status(SpanCtx, ok),

    %% Return 204 No Content
    ReqReply = cowboy_req:reply(204, #{}, Req),
    {ok, ReqReply, State}.

%% @doc Handle resource deletion (DELETE /mcp/resources/{uri})
%% @private
-spec handle_delete_resource(term(), list(), binary(), term(), term()) -> {ok, term(), term()}.
handle_delete_resource(Req, ResourcePath, SessionId, SpanCtx, State) ->
    try
        %% Reconstruct URI from path segments
        Uri = erlang:list_to_binary(string:join(ResourcePath, "/")),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"operation">> => <<"delete_resource">>,
            <<"uri">> => Uri,
            <<"session_id">> => SessionId
        }),

        %% Get server from registry to delete resource
        case get_server_for_session(SessionId) of
            {ok, ServerPid} ->
                case delete_resource_from_server(ServerPid, Uri) of
                    ok ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        ReqReply = cowboy_req:reply(204, #{}, Req),
                        {ok, ReqReply, State};
                    {error, not_found} ->
                        erlmcp_tracing:record_error_details(SpanCtx, resource_not_found, Uri),
                        error_response(Req, 404, <<"Not Found">>, <<"Resource not found">>, State);
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, deletion_error, Reason),
                        error_response(Req, 400, <<"Bad Request">>, <<"Failed to delete resource">>, State)
                end;
            {error, not_found} ->
                erlmcp_tracing:record_error_details(SpanCtx, server_not_found, SessionId),
                error_response(Req, 500, <<"Internal Error">>, <<"Server not found">>, State)
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            error_response(Req, 400, <<"Bad Request">>, <<"Invalid resource URI">>, State)
    end.

%% @doc Handle tool deletion (DELETE /mcp/tools/{name})
%% @private
-spec handle_delete_tool(term(), string(), binary(), term(), term()) -> {ok, term(), term()}.
handle_delete_tool(Req, ToolName, SessionId, SpanCtx, State) ->
    try
        ToolNameBin = erlang:list_to_binary(ToolName),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"operation">> => <<"delete_tool">>,
            <<"tool_name">> => ToolNameBin,
            <<"session_id">> => SessionId
        }),

        %% Get server from registry to delete tool
        case get_server_for_session(SessionId) of
            {ok, ServerPid} ->
                case delete_tool_from_server(ServerPid, ToolNameBin) of
                    ok ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        ReqReply = cowboy_req:reply(204, #{}, Req),
                        {ok, ReqReply, State};
                    {error, not_found} ->
                        erlmcp_tracing:record_error_details(SpanCtx, tool_not_found, ToolNameBin),
                        error_response(Req, 404, <<"Not Found">>, <<"Tool not found">>, State);
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, deletion_error, Reason),
                        error_response(Req, 400, <<"Bad Request">>, <<"Failed to delete tool">>, State)
                end;
            {error, not_found} ->
                erlmcp_tracing:record_error_details(SpanCtx, server_not_found, SessionId),
                error_response(Req, 500, <<"Internal Error">>, <<"Server not found">>, State)
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            error_response(Req, 400, <<"Bad Request">>, <<"Invalid tool name">>, State)
    end.

%% @doc Handle prompt deletion (DELETE /mcp/prompts/{name})
%% @private
-spec handle_delete_prompt(term(), string(), binary(), term(), term()) -> {ok, term(), term()}.
handle_delete_prompt(Req, PromptName, SessionId, SpanCtx, State) ->
    try
        PromptNameBin = erlang:list_to_binary(PromptName),

        erlmcp_tracing:set_attributes(SpanCtx, #{
            <<"operation">> => <<"delete_prompt">>,
            <<"prompt_name">> => PromptNameBin,
            <<"session_id">> => SessionId
        }),

        %% Get server from registry to delete prompt
        case get_server_for_session(SessionId) of
            {ok, ServerPid} ->
                case delete_prompt_from_server(ServerPid, PromptNameBin) of
                    ok ->
                        erlmcp_tracing:set_status(SpanCtx, ok),
                        ReqReply = cowboy_req:reply(204, #{}, Req),
                        {ok, ReqReply, State};
                    {error, not_found} ->
                        erlmcp_tracing:record_error_details(SpanCtx, prompt_not_found, PromptNameBin),
                        error_response(Req, 404, <<"Not Found">>, <<"Prompt not found">>, State);
                    {error, Reason} ->
                        erlmcp_tracing:record_error_details(SpanCtx, deletion_error, Reason),
                        error_response(Req, 400, <<"Bad Request">>, <<"Failed to delete prompt">>, State)
                end;
            {error, not_found} ->
                erlmcp_tracing:record_error_details(SpanCtx, server_not_found, SessionId),
                error_response(Req, 500, <<"Internal Error">>, <<"Server not found">>, State)
        end
    catch
        Class:CaughtReason:Stacktrace ->
            erlmcp_tracing:record_exception(SpanCtx, Class, CaughtReason, Stacktrace),
            error_response(Req, 400, <<"Bad Request">>, <<"Invalid prompt name">>, State)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Format error response
%% @private
-spec error_response(term(), non_neg_integer(), binary(), binary(), term()) -> {ok, term(), term()}.
error_response(Req, StatusCode, ErrorType, Message, State) ->
    ReqReply = cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{
        <<"error">> => ErrorType,
        <<"message">> => Message
    }), Req),
    {ok, ReqReply, State}.

%% @doc Get server PID for a session
%% @private
-spec get_server_for_session(binary()) -> {ok, pid()} | {error, not_found}.
get_server_for_session(_SessionId) ->
    %% For now, get the default server
    %% In a multi-server setup, we'd look up the server by session ID
    case erlmcp_registry:get_servers() of
        [ServerPid | _] -> {ok, ServerPid};
        [] -> {error, not_found}
    end.

%% @doc Delete resource from server
%% @private
-spec delete_resource_from_server(pid(), binary()) -> ok | {error, term()}.
delete_resource_from_server(ServerPid, Uri) ->
    try
        gen_server:call(ServerPid, {delete_resource, Uri}, 5000)
    catch
        _:_ -> {error, server_error}
    end.

%% @doc Delete tool from server
%% @private
-spec delete_tool_from_server(pid(), binary()) -> ok | {error, term()}.
delete_tool_from_server(ServerPid, Name) ->
    try
        gen_server:call(ServerPid, {delete_tool, Name}, 5000)
    catch
        _:_ -> {error, server_error}
    end.

%% @doc Delete prompt from server
%% @private
-spec delete_prompt_from_server(pid(), binary()) -> ok | {error, term()}.
delete_prompt_from_server(ServerPid, Name) ->
    try
        gen_server:call(ServerPid, {delete_prompt, Name}, 5000)
    catch
        _:_ -> {error, server_error}
    end.
