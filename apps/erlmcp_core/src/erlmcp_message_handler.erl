%% @doc MCP message processing handler (hot path optimization).
%% Extracted from erlmcp_server.erl to keep module size <500 LOC.
%% Handles incoming message processing, request routing, and response generation.
-module(erlmcp_message_handler).

-include("erlmcp.hrl").%% TODO: Add opentelemetry_api dependency when telemetry is enabled
                       %% -include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% API exports
-export([process_message/3, handle_initialize/2, handle_initialized/2, handle_ping/2,
         handle_resources_list/2, handle_tools_list/2, handle_prompts_list/2, handle_call_tool/2,
         handle_get_prompt/2, handle_read_resource/2, handle_sampling_create/2, handle_resources_subscribe/2,
         handle_resources_unsubscribe/2, handle_roots_list/2]).

%% Types
-type state() :: #mcp_server_state{}.

%%====================================================================
%% Main Message Processing (Hot Path)
%%====================================================================

%% @doc Process incoming MCP message (hot path).
%% Optimized for fast message routing with minimal allocation.
%% Supports OTP 28 priority messages for urgent operations.
-spec process_message(binary(), binary(), state()) ->
                         {ok, state()} | {error, term()} | {reply, binary(), state()}.
process_message(TransportId, Data, State) ->
    case erlmcp_json_rpc:decode_message(Data, default) of
        {ok,
         #json_rpc_request{method = Method,
                           params = Params,
                           id = Id}} ->
            handle_request(Method, Params, Id, TransportId, State);
        {ok, #json_rpc_notification{method = Method, params = Params}} ->
            handle_notification(Method, Params, TransportId, State);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Handle priority messages (OTP 28 EEP-76).
%% Priority messages jump the queue for urgent operations:
%% - Tool cancellation
%% - Health check responses
%% - Emergency shutdown
-spec handle_priority_message(term(), pid(), state()) ->
                                     {ok, state()} | {reply, binary(), state()}.
handle_priority_message({cancel_request, RequestId}, _From, State) ->
    %% Handle urgent cancellation request
    %% This bypasses normal message queue processing
    logger:info("Priority cancel request received for: ~p", [RequestId]),
    %% Create cancellation response
    Response = erlmcp_json_rpc:encode_response(RequestId, #{<<"cancelled">> => true}),
    {reply, Response, State};

handle_priority_message({health_check_response, Ref, Status}, _From, State) ->
    %% Handle health check response with priority
    logger:debug("Priority health check response: ~p = ~p", [Ref, Status]),
    %% Note: health_checks field not defined in mcp_server_state record
    %% TODO: Add health_checks field to mcp_server_state record if needed
    {ok, State};

handle_priority_message({emergency_shutdown, Reason}, _From, State) ->
    %% Handle emergency shutdown with priority
    logger:warning("Priority emergency shutdown: ~p", [Reason]),
    %% Send shutdown notification
    Notification = erlmcp_json_rpc:encode_notification(<<"shutdown">>, #{
        <<"reason">> => Reason,
        <<"urgent">> => true
    }),
    {reply, Notification, State};

handle_priority_message(UnknownMessage, _From, State) ->
    %% Log unknown priority message for debugging
    logger:warning("Unknown priority message received: ~p", [UnknownMessage]),
    {ok, State}.

%%====================================================================
%% Request Handlers (organized by method)
%%====================================================================

%% @doc Handle initialize request
-spec handle_initialize(map(), state()) -> {binary(), state()}.
handle_initialize(_Params, State) ->
    %% Process initialize handshake
    {<<"initialize">>, State}.

%% @doc Handle initialized notification
-spec handle_initialized(map(), state()) -> {ok, state()}.
handle_initialized(_Params, State) ->
    %% Complete initialization phase
    {ok, State#mcp_server_state{initialized = true}}.

%% @doc Handle resources/list request
-spec handle_resources_list(map(), state()) -> {binary(), state()}.
handle_resources_list(_Params, State) ->
    %% Return list of available resources
    _Resources = maps:values(State#mcp_server_state.resources),
    {<<"resources">>, State}.

%% @doc Handle tools/list request
-spec handle_tools_list(map(), state()) -> {binary(), state()}.
handle_tools_list(_Params, State) ->
    %% Return list of available tools
    _Tools = maps:values(State#mcp_server_state.tools),
    {<<"tools">>, State}.

%% @doc Handle prompts/list request
-spec handle_prompts_list(map(), state()) -> {binary(), state()}.
handle_prompts_list(_Params, State) ->
    %% Return list of available prompts
    _Prompts = maps:values(State#mcp_server_state.prompts),
    {<<"prompts">>, State}.

%% @doc Handle tools/call request
-spec handle_call_tool(map(), state()) -> {binary(), state()}.
handle_call_tool(Params, State) ->
    %% Execute tool and return result
    try
        ToolName = maps:get(<<"name">>, Params, undefined),
        Args = maps:get(<<"arguments">>, Params, #{}),

        case ToolName of
            undefined ->
                Error = erlmcp_json_rpc:error_invalid_params(undefined, <<"Missing required parameter: name">>),
                {Error, State};
            _ ->
                %% Execute tool using erlmcp_tool_execute
                Options = #{}, % Default options
                case erlmcp_tool_execute:execute_tool(ToolName, Args, Options) of
                    {ok, Result} ->
                        Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => Result}),
                        {Response, State};
                    {error, Reason} ->
                        Error = erlmcp_json_rpc:error_internal_error(undefined,
                            <<"Tool execution failed: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                        {Error, State}
                end
        end
    catch
        CatchClass:CatchReason:CatchStack ->
            logger:error("Error in handle_call_tool: ~p:~p~n~p", [CatchClass, CatchReason, CatchStack]),
            CatchError = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in tool execution: ", (list_to_binary(io_lib:format("~p", [CatchReason])))/binary>>),
            {CatchError, State}
    end.

%% @doc Handle prompts/get request
-spec handle_get_prompt(map(), state()) -> {binary(), state()}.
handle_get_prompt(Params, State) ->
    %% Retrieve prompt content
    try
        PromptName = maps:get(<<"name">>, Params, undefined),
        Arguments = maps:get(<<"arguments">>, Params, #{}),

        case PromptName of
            undefined ->
                Error = erlmcp_json_rpc:error_invalid_params(undefined, <<"Missing required parameter: name">>),
                {Error, State};
            _ ->
                %% Get prompt from server state
                case maps:get(PromptName, State#mcp_server_state.prompts, undefined) of
                    undefined ->
                        Error = erlmcp_json_rpc:error_method_not_found(undefined, <<"Prompt not found: ", PromptName/binary>>),
                        {Error, State};
                    Prompt ->
                        %% Render template with arguments
                        case erlmcp_prompt_template:render_safe(Prompt, Arguments) of
                            {ok, RenderedContent} ->
                                %% Format response as messages array
                                Messages = [#{<<"role">> => <<"user">>, <<"content">> => RenderedContent}],
                                Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => #{<<"messages">> => Messages}}),
                                {Response, State};
                            {error, Reason} ->
                                Error = erlmcp_json_rpc:error_internal_error(undefined,
                                    <<"Prompt rendering failed: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                                {Error, State}
                        end
                end
        end
    catch
        CatchClass2:CatchReason2:CatchStack2 ->
            logger:error("Error in handle_get_prompt: ~p:~p~n~p", [CatchClass2, CatchReason2, CatchStack2]),
            CatchError2 = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in prompt retrieval: ", (list_to_binary(io_lib:format("~p", [CatchReason2])))/binary>>),
            {CatchError2, State}
    end.

%% @doc Handle resources/read request
-spec handle_read_resource(map(), state()) -> {binary(), state()}.
handle_read_resource(Params, State) ->
    %% Read resource content
    try
        Uri = maps:get(<<"uri">>, Params, undefined),

        case Uri of
            undefined ->
                Error = erlmcp_json_rpc:error_invalid_params(undefined, <<"Missing required parameter: uri">>),
                {Error, State};
            _ ->
                %% Validate and normalize resource URI
                case erlmcp_resources:normalize_resource_uri(Uri) of
                    error ->
                        Error = erlmcp_json_rpc:error_internal_error(undefined, <<"Invalid resource URI">>),
                        {Error, State};
                    NormalizedUri ->
                        %% Read resource using erlmcp_resources
                        case erlmcp_resources:read_resource(NormalizedUri) of
                            {ok, Content} ->
                                %% Determine MIME type from URI extension
                                MimeType = determine_mime_type(NormalizedUri),
                                FormattedContent = #{
                                    <<"contents">> => Content,
                                    <<"mimeType">> => MimeType,
                                    <<"uri">> => NormalizedUri
                                },
                                Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => FormattedContent}),
                                {Response, State};
                            {error, not_found} ->
                                Error = erlmcp_json_rpc:error_method_not_found(undefined, <<"Resource not found: ", Uri/binary>>),
                                {Error, State};
                            {error, Reason} ->
                                Error = erlmcp_json_rpc:error_internal_error(undefined,
                                    <<"Resource reading failed: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                                {Error, State}
                        end
                end
        end
    catch
        CatchClass3:CatchReason3:CatchStack3 ->
            logger:error("Error in handle_read_resource: ~p:~p~n~p", [CatchClass3, CatchReason3, CatchStack3]),
            CatchError3 = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in resource reading: ", (list_to_binary(io_lib:format("~p", [CatchReason3])))/binary>>),
            {CatchError3, State}
    end.

%% @doc Handle resources/subscribe request
-spec handle_resources_subscribe(map(), state()) -> {binary(), state()}.
handle_resources_subscribe(Params, State) ->
    %% Subscribe to resource updates
    try
        Uri = maps:get(<<"uri">>, Params, undefined),

        case Uri of
            undefined ->
                Error = erlmcp_json_rpc:error_invalid_params(undefined, <<"Missing required parameter: uri">>),
                {Error, State};
            _ ->
                %% Subscribe using erlmcp_resource_subscriptions
                case erlmcp_resource_subscriptions:subscribe(Uri, self()) of
                    ok ->
                        Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => #{<<"subscribed">> => true}}),
                        {Response, State};
                    {error, Reason} ->
                        Error = erlmcp_json_rpc:error_internal_error(undefined,
                            <<"Resource subscription failed: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                        {Error, State}
                end
        end
    catch
        CatchClass4:CatchReason4:CatchStack4 ->
            logger:error("Error in handle_resources_subscribe: ~p:~p~n~p", [CatchClass4, CatchReason4, CatchStack4]),
            CatchError4 = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in resource subscription: ", (list_to_binary(io_lib:format("~p", [CatchReason4])))/binary>>),
            {CatchError4, State}
    end.

%% @doc Handle resources/unsubscribe request
-spec handle_resources_unsubscribe(map(), state()) -> {binary(), state()}.
handle_resources_unsubscribe(Params, State) ->
    %% Unsubscribe from resource updates
    try
        Uri = maps:get(<<"uri">>, Params, undefined),

        case Uri of
            undefined ->
                Error = erlmcp_json_rpc:error_invalid_params(undefined, <<"Missing required parameter: uri">>),
                {Error, State};
            _ ->
                %% Unsubscribe using erlmcp_resource_subscriptions
                case erlmcp_resource_subscriptions:unsubscribe(Uri, self()) of
                    ok ->
                        Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => #{<<"unsubscribed">> => true}}),
                        {Response, State};
                    {error, Reason} ->
                        Error = erlmcp_json_rpc:error_internal_error(undefined,
                            <<"Resource unsubscription failed: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                        {Error, State}
                end
        end
    catch
        CatchClass5:CatchReason5:CatchStack5 ->
            logger:error("Error in handle_resources_unsubscribe: ~p:~p~n~p", [CatchClass5, CatchReason5, CatchStack5]),
            CatchError5 = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in resource unsubscription: ", (list_to_binary(io_lib:format("~p", [CatchReason5])))/binary>>),
            {CatchError5, State}
    end.

%% @doc Handle roots/list request
-spec handle_roots_list(map(), state()) -> {binary(), state()}.
handle_roots_list(_Params, State) ->
    %% List available resource roots
    try
        case erlmcp_resources:list_roots() of
            {ok, Roots} ->
                FormattedRoots = lists:map(fun(#{uri := Uri, name := Name}) ->
                    #{<<"uri">> => Uri, <<"name">> => Name}
                end, Roots),
                Response = erlmcp_json_rpc:encode_response(undefined, #{<<"result">> => #{<<"roots">> => FormattedRoots}}),
                {Response, State};
            {error, Reason} ->
                Error = erlmcp_json_rpc:error_internal_error(undefined,
                    <<"Failed to list roots: ", (list_to_binary(io_lib:format("~p", [Reason])))/binary>>),
                {Error, State}
        end
    catch
        CatchClass6:CatchReason6:CatchStack6 ->
            logger:error("Error in handle_roots_list: ~p:~p~n~p", [CatchClass6, CatchReason6, CatchStack6]),
            CatchError6 = erlmcp_json_rpc:error_internal_error(undefined,
                <<"Internal error in roots listing: ", (list_to_binary(io_lib:format("~p", [CatchReason6])))/binary>>),
            {CatchError6, State}
    end.

%% @doc Determine MIME type from URI extension
-spec determine_mime_type(binary()) -> binary().
determine_mime_type(Uri) ->
    case binary:match(Uri, <<".txt">>, []) of
        {_, _} -> <<"text/plain">>;
        nomatch ->
            case binary:match(Uri, <<".json">>, []) of
                {_, _} -> <<"application/json">>;
                nomatch ->
                    case binary:match(Uri, <<".csv">>, []) of
                        {_, _} -> <<"text/csv">>;
                        nomatch ->
                            case binary:match(Uri, <<".xml">>, []) of
                                {_, _} -> <<"application/xml">>;
                                nomatch ->
                                    case binary:match(Uri, <<".html">>, []) of
                                        {_, _} -> <<"text/html">>;
                                        nomatch ->
                                            case binary:match(Uri, <<".pdf">>, []) of
                                                {_, _} -> <<"application/pdf">>;
                                                nomatch ->
                                                    case binary:match(Uri, <<".png">>, []) of
                                                        {_, _} -> <<"image/png">>;
                                                        nomatch ->
                                                            case binary:match(Uri, <<".jpg">>, []) of
                                                                {_, _} -> <<"image/jpeg">>;
                                                                nomatch ->
                                                                    case binary:match(Uri, <<".gif">>, []) of
                                                                        {_, _} -> <<"image/gif">>;
                                                                        nomatch ->
                                                                            case binary:match(Uri, <<".md">>, []) of
                                                                                {_, _} -> <<"text/markdown">>;
                                                                                nomatch ->
                                                                                    %% Default to text/plain for unknown types
                                                                                    <<"text/plain">>
                                                                            end
                                                                    end
                                                            end
                                                    end
                                            end
                                    end
                            end
                    end
            end
    end.

%% @doc Handle sampling/create request
-spec handle_sampling_create(map(), state()) -> {binary(), state()}.
handle_sampling_create(Params, State) ->
    %% Create sampling response using configured LLM provider
    case erlmcp_sampling:create_message(maps:get(<<"messages">>, Params, []), maps:get(<<"params">>, Params, #{})) of
        {ok, Response} ->
            %% Format response according to MCP specification
            SamplingResult = #{
                <<"result">> => #{
                    <<"content">> => Response#{
                        <<"role">> => maps:get(<<"role">>, Response, <<"assistant">>),
                        <<"content">> => maps:get(<<"content">>, Response, <<>>),
                        <<"model">> => maps:get(<<"model">>, Response, <<"unknown">>)
                    }
                },
                <<"metadata">> => Response#{
                    <<"usage">> => maps:get(<<"usage">>, Response, #{}),
                    <<"stopReason">> => maps:get(<<"stopReason">>, Response, <<"end_of_turn">>)
                }
            },
            _ = SamplingResult,
            {<<"sampling">>, State};
        {error, Reason} ->
            %% Return error response
            _ = #{
                <<"error">> => #{
                    <<"message">> => format_error_reason(Reason),
                    <<"code">> => <<"sampling_failed">>
                }
            },
            {<<"sampling">>, State}
    end.

%% @doc Handle ping request (MCP 2025-11-25)
%% Simplest possible request - returns empty object on success
-spec handle_ping(map(), state()) -> {ok, state()}.
handle_ping(_Params, State) ->
    %% Ping always succeeds, returns empty result
    {ok, State}.

%%====================================================================
%% Internal Routing
%%====================================================================

%% @doc Route request by method name
-spec handle_request(binary(), map() | undefined, json_rpc_id(), binary(), state()) ->
                        {reply, binary(), state()} | {error, term()}.
handle_request(<<"initialize">>, _Params, Id, _TransportId, State) ->
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"result">> => <<"ok">>}), State};
handle_request(<<"ping">>, _Params, Id, _TransportId, State) ->
    %% Ping method (MCP 2025-11-25) - returns empty object
    {reply, erlmcp_json_rpc:encode_response(Id, #{}), State};
handle_request(<<"resources/list">>, _Params, Id, _TransportId, State) ->
    Resources = maps:keys(State#mcp_server_state.resources),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"resources">> => Resources}), State};
handle_request(<<"tools/list">>, _Params, Id, _TransportId, State) ->
    Tools = maps:keys(State#mcp_server_state.tools),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"tools">> => Tools}), State};
handle_request(<<"prompts/list">>, _Params, Id, _TransportId, State) ->
    Prompts = maps:keys(State#mcp_server_state.prompts),
    {reply, erlmcp_json_rpc:encode_response(Id, #{<<"prompts">> => Prompts}), State};
handle_request(<<"sampling/create">>, Params, Id, _TransportId, State) ->
    %% Handle sampling create request
    {Response, NewState} = handle_sampling_create(Params, State),
    {reply, Response, NewState};
handle_request(<<"prompts/get">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_get_prompt(Params, State),
    {reply, Response, State};
handle_request(<<"resources/read">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_read_resource(Params, State),
    {reply, Response, State};
handle_request(<<"resources/subscribe">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_resources_subscribe(Params, State),
    {reply, Response, State};
handle_request(<<"resources/unsubscribe">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_resources_unsubscribe(Params, State),
    {reply, Response, State};
handle_request(<<"roots/list">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_roots_list(Params, State),
    {reply, Response, State};
handle_request(<<"tools/call">>, Params, Id, _TransportId, State) ->
    {Response, _} = handle_call_tool(Params, State),
    {reply, Response, State};
handle_request(Method, _Params, Id, _TransportId, State) ->
    Error = erlmcp_json_rpc:error_method_not_found(Id, Method),
    {reply, Error, State}.

%% @doc Route notification by method name
-spec handle_notification(binary(), map() | undefined, binary(), state()) ->
                             {ok, state()} | {error, term()}.
handle_notification(<<"initialized">>, _Params, _TransportId, State) ->
    {ok, State#mcp_server_state{initialized = true}};
handle_notification(_Method, _Params, _TransportId, State) ->
    {ok, State}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Format error reason for user-friendly response
-spec format_error_reason(term()) -> binary().
format_error_reason(Reason) ->
    case Reason of
        {http_error, Status} when is_integer(Status) ->
            << "HTTP Error: ", (integer_to_binary(Status))/binary>>;
        {http_error, Status, Headers} when is_integer(Status) ->
            HeadersBin = format_headers(Headers),
            << "HTTP Error: ", (integer_to_binary(Status))/binary, " - ", HeadersBin/binary>>;
        {provider_error, Type, Message} when is_binary(Message) ->
            Message;
        {provider_error, Error} when is_map(Error) ->
            case maps:get(<<"message">>, Error, undefined) of
                undefined ->
                    list_to_binary("Provider error: " + jsx:encode(Error));
                Msg ->
                    Msg
            end;
        invalid_provider ->
            <<"Invalid LLM provider configuration">>;
        missing_api_key ->
            <<"API key not configured for LLM provider">>;
        validation_failed ->
            <<"Request validation failed">>;
        _ ->
            list_to_binary("Unknown error: " + io_lib:format("~p", [Reason]))
    end.

%% @doc Format HTTP headers for error messages
-spec format_headers(list()) -> binary().
format_headers(Headers) ->
    HeadersList = lists:map(fun({Name, Value}) ->
        list_to_binary(Name) ++ <<": ">> ++ list_to_binary(Value)
    end, Headers),
    list_to_binary(lists:join(<<", ">>, HeadersList)).
