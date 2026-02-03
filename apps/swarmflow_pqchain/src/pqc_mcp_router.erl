%%% @doc MCP JSON-RPC Router
%%%
%%% Implements MCP 2025-11-25 specification as JSON-RPC 2.0 router.
%%% Routes all operations through Case syscalls - "MCP is just a port."
%%%
%%% Architecture:
%%% - Stateless router: all state lives in Cases
%%% - Each MCP session maps to one or more Cases
%%% - Tool calls create/reuse Cases via pqc_case_registry:ensure_case/3
%%% - Resources map to Case state via pqc_projection_mcp
%%% - Subscriptions use Case pg pubsub
%%%
%%% MCP Methods Implemented:
%%% - initialize: Return capabilities (tools, resources, prompts)
%%% - tools/list: List tools from pqc_tools:list()
%%% - tools/call: Route through Case via pqc_case:tool_call/3
%%% - resources/list: List from pqc_projection_mcp:list_resources()
%%% - resources/read: Read via pqc_projection_mcp:read_resource/1
%%% - resources/subscribe: Subscribe to Case updates
%%% - prompts/list: List workflow prompts
%%% - prompts/get: Get specific prompt
%%%
%%% JSON-RPC 2.0 Format:
%%% Request: {"jsonrpc": "2.0", "id": 1, "method": "tools/call", "params": {...}}
%%% Response: {"jsonrpc": "2.0", "id": 1, "result": {...}}
%%% Error: {"jsonrpc": "2.0", "id": 1, "error": {"code": -32600, "message": "..."}}
%%%
%%% @end
-module(pqc_mcp_router).

-include("pqchain.hrl").

%% API exports
-export([
    handle_jsonrpc/1,
    handle/2,
    format_error/2,
    format_response/2
]).

%%% ============================================================================
%%% Types
%%% ============================================================================

-type json_term() :: term().
-type method() :: binary().
-type params() :: map() | list().
-type request_id() :: integer() | binary() | null.

-type jsonrpc_request() :: #{
    jsonrpc := binary(),
    id => request_id(),
    method := method(),
    params => params()
}.

-type jsonrpc_response() :: #{
    jsonrpc := binary(),
    id := request_id(),
    result => json_term()
}.

-type jsonrpc_error() :: #{
    jsonrpc := binary(),
    id := request_id(),
    error := #{
        code := integer(),
        message := binary(),
        data => term()
    }
}.

-export_type([jsonrpc_request/0, jsonrpc_response/0, jsonrpc_error/0]).

%%% ============================================================================
%%% JSON-RPC Error Codes
%%% ============================================================================

-define(ERR_PARSE_ERROR, -32700).
-define(ERR_INVALID_REQUEST, -32600).
-define(ERR_METHOD_NOT_FOUND, -32601).
-define(ERR_INVALID_PARAMS, -32602).
-define(ERR_INTERNAL_ERROR, -32603).

%%% ============================================================================
%%% API Functions
%%% ============================================================================

%% @doc Main entry point: handle JSON-RPC binary request
%%
%% Parses JSON, routes to handler, returns JSON response.
%%
%% @end
-spec handle_jsonrpc(binary()) -> binary().
handle_jsonrpc(JsonBin) when is_binary(JsonBin) ->
    try
        %% Parse JSON request
        Request = jsx:decode(JsonBin, [return_maps]),

        %% Validate JSON-RPC 2.0 structure
        case validate_jsonrpc_request(Request) of
            {ok, ValidRequest} ->
                %% Extract method and params
                Method = maps:get(<<"method">>, ValidRequest),
                Params = maps:get(<<"params">>, ValidRequest, #{}),
                Id = maps:get(<<"id">>, ValidRequest, null),

                %% Route to handler
                case handle(Method, Params) of
                    {ok, Result} ->
                        Response = format_response(Id, Result),
                        jsx:encode(Response);
                    {error, Reason} ->
                        ErrorResponse = format_error(Id, Reason),
                        jsx:encode(ErrorResponse)
                end;
            {error, Reason} ->
                %% Invalid request structure
                ErrorResponse = format_error(null, {invalid_request, Reason}),
                jsx:encode(ErrorResponse)
        end
    catch
        error:badarg ->
            %% JSON parse error
            ErrorResponse = format_error(null, parse_error),
            jsx:encode(ErrorResponse);
        error:Reason:Stack ->
            %% Internal error
            logger:error("MCP router internal error: ~p~n~p", [Reason, Stack]),
            ErrorResponse = format_error(null, {internal_error, Reason}),
            jsx:encode(ErrorResponse)
    end.

%% @doc Handle MCP method with params
%%
%% Routes to appropriate MCP method handler.
%% Returns {ok, Result} or {error, Reason}.
%%
%% @end
-spec handle(method(), params()) -> {ok, term()} | {error, term()}.

%% Initialize - return MCP server capabilities
handle(<<"initialize">>, Params) ->
    handle_initialize(Params);

%% Tools methods
handle(<<"tools/list">>, _Params) ->
    handle_tools_list();
handle(<<"tools/call">>, Params) ->
    handle_tools_call(Params);

%% Resources methods
handle(<<"resources/list">>, _Params) ->
    handle_resources_list();
handle(<<"resources/read">>, Params) ->
    handle_resources_read(Params);
handle(<<"resources/subscribe">>, Params) ->
    handle_resources_subscribe(Params);
handle(<<"resources/unsubscribe">>, Params) ->
    handle_resources_unsubscribe(Params);

%% Prompts methods
handle(<<"prompts/list">>, _Params) ->
    handle_prompts_list();
handle(<<"prompts/get">>, Params) ->
    handle_prompts_get(Params);

%% Notifications (JSON-RPC notifications have no response)
handle(<<"notifications/", _/binary>>, _Params) ->
    {ok, #{}};  % Acknowledge notification

%% Unknown method
handle(Method, _Params) ->
    {error, {method_not_found, Method}}.

%%% ============================================================================
%%% Method Handlers - Initialize
%%% ============================================================================

%% @private
-spec handle_initialize(map()) -> {ok, map()}.
handle_initialize(Params) ->
    ClientInfo = maps:get(<<"clientInfo">>, Params, #{}),
    logger:info("MCP client initialized: ~p", [ClientInfo]),

    %% Return server capabilities per MCP 2025-11-25 spec
    Capabilities = #{
        protocolVersion => <<"2025-11-25">>,
        serverInfo => #{
            name => <<"SwarmFlow PQChain MCP Server">>,
            version => <<"1.0.0">>
        },
        capabilities => #{
            tools => #{},
            resources => #{
                subscribe => true,
                listChanged => true
            },
            prompts => #{},
            logging => #{}
        }
    },
    {ok, Capabilities}.

%%% ============================================================================
%%% Method Handlers - Tools
%%% ============================================================================

%% @private
-spec handle_tools_list() -> {ok, map()}.
handle_tools_list() ->
    case pqc_tools:list() of
        {ok, Tools} ->
            %% Convert tool schemas to MCP format
            McpTools = lists:map(fun tool_schema_to_mcp/1, Tools),
            {ok, #{tools => McpTools}};
        {error, Reason} ->
            {error, {tools_list_failed, Reason}}
    end.

%% @private
-spec handle_tools_call(map()) -> {ok, map()} | {error, term()}.
handle_tools_call(Params) ->
    ToolName = maps:get(<<"name">>, Params),
    Args = maps:get(<<"arguments">>, Params, #{}),

    %% Get or create Case ID (from params or default)
    CaseId = maps:get(<<"caseId">>, Params, <<"default">>),

    %% Ensure Case exists
    case ensure_case_for_tool_call(CaseId) of
        {ok, Pid} ->
            %% Route tool call through Case
            case pqc_case:tool_call(Pid, ToolName, Args) of
                {ok, Result} ->
                    %% Convert result to MCP format
                    McpResult = pqc_projection_mcp:case_to_mcp_tool_result({ok, Result}),
                    {ok, McpResult};
                {error, Reason} ->
                    %% Return error in MCP format
                    McpError = pqc_projection_mcp:case_to_mcp_tool_result({error, Reason}),
                    {ok, McpError}
            end;
        {error, Reason} ->
            {error, {case_creation_failed, Reason}}
    end.

%%% ============================================================================
%%% Method Handlers - Resources
%%% ============================================================================

%% @private
-spec handle_resources_list() -> {ok, map()}.
handle_resources_list() ->
    case pqc_projection_mcp:list_resources() of
        {ok, ResourceUris} ->
            %% Convert URIs to MCP resource templates
            Resources = lists:map(
                fun(Uri) ->
                    #{
                        uri => Uri,
                        name => extract_resource_name(Uri),
                        mimeType => <<"application/json">>
                    }
                end,
                ResourceUris
            ),
            {ok, #{resources => Resources}};
        {error, Reason} ->
            {error, {resources_list_failed, Reason}}
    end.

%% @private
-spec handle_resources_read(map()) -> {ok, map()} | {error, term()}.
handle_resources_read(Params) ->
    Uri = maps:get(<<"uri">>, Params),

    case pqc_projection_mcp:read_resource(Uri) of
        {ok, Resource} ->
            {ok, #{contents => [Resource]}};
        {error, Reason} ->
            {error, {resource_read_failed, Reason}}
    end.

%% @private
-spec handle_resources_subscribe(map()) -> {ok, map()} | {error, term()}.
handle_resources_subscribe(Params) ->
    Uri = maps:get(<<"uri">>, Params),

    %% Parse URI to get Case ID
    case pqc_projection_mcp:parse_resource_uri(Uri) of
        {case_snapshot, CaseId} ->
            subscribe_to_case(CaseId);
        {case_artifacts, CaseId} ->
            subscribe_to_case(CaseId);
        {case_receipt, CaseId} ->
            subscribe_to_case(CaseId);
        {case_history, CaseId} ->
            subscribe_to_case(CaseId);
        _ ->
            {error, {unsupported_subscription, Uri}}
    end.

%% @private
-spec handle_resources_unsubscribe(map()) -> {ok, map()} | {error, term()}.
handle_resources_unsubscribe(Params) ->
    Uri = maps:get(<<"uri">>, Params),

    %% Parse URI to get Case ID
    case pqc_projection_mcp:parse_resource_uri(Uri) of
        {case_snapshot, CaseId} ->
            unsubscribe_from_case(CaseId);
        {case_artifacts, CaseId} ->
            unsubscribe_from_case(CaseId);
        {case_receipt, CaseId} ->
            unsubscribe_from_case(CaseId);
        {case_history, CaseId} ->
            unsubscribe_from_case(CaseId);
        _ ->
            {error, {unsupported_unsubscription, Uri}}
    end.

%%% ============================================================================
%%% Method Handlers - Prompts
%%% ============================================================================

%% @private
-spec handle_prompts_list() -> {ok, map()}.
handle_prompts_list() ->
    case pqc_projection_mcp:list_prompts() of
        {ok, Prompts} ->
            {ok, #{prompts => Prompts}};
        {error, Reason} ->
            {error, {prompts_list_failed, Reason}}
    end.

%% @private
-spec handle_prompts_get(map()) -> {ok, map()} | {error, term()}.
handle_prompts_get(Params) ->
    Name = maps:get(<<"name">>, Params),
    Arguments = maps:get(<<"arguments">>, Params, #{}),

    case pqc_projection_mcp:get_prompt(Name) of
        {ok, Prompt} ->
            %% Generate prompt messages based on arguments
            Messages = generate_prompt_messages(Name, Arguments),
            {ok, #{
                description => maps:get(description, Prompt, <<>>),
                messages => Messages
            }};
        {error, not_found} ->
            {error, {prompt_not_found, Name}}
    end.

%%% ============================================================================
%%% Internal Functions - Case Management
%%% ============================================================================

%% @private
%% Ensure Case exists for tool call, creating with default net if needed
-spec ensure_case_for_tool_call(binary()) -> {ok, pid()} | {error, term()}.
ensure_case_for_tool_call(CaseId) ->
    %% Get default workflow net
    Net = pqc_projection_mcp:default_net(),

    %% Get or create PQC signing key
    SigningKey = get_or_create_signing_key(),

    %% Ensure Case exists via registry
    pqc_case_registry:ensure_case(CaseId, Net, SigningKey).

%% @private
-spec get_or_create_signing_key() -> #pqc_keypair{} | undefined.
get_or_create_signing_key() ->
    %% For now, return undefined (unsigned receipts)
    %% In production, this would fetch from pqc_identity or create ephemeral key
    undefined.

%% @private
-spec subscribe_to_case(binary()) -> {ok, map()} | {error, term()}.
subscribe_to_case(CaseId) ->
    case pqc_case_registry:subscribe(CaseId) of
        ok ->
            {ok, #{subscribed => true, uri => iolist_to_binary([<<"case://">>, CaseId])}};
        {error, Reason} ->
            {error, {subscription_failed, Reason}}
    end.

%% @private
-spec unsubscribe_from_case(binary()) -> {ok, map()} | {error, term()}.
unsubscribe_from_case(CaseId) ->
    case pqc_case_registry:unsubscribe(CaseId) of
        ok ->
            {ok, #{unsubscribed => true}};
        {error, Reason} ->
            {error, {unsubscription_failed, Reason}}
    end.

%%% ============================================================================
%%% Internal Functions - Conversions
%%% ============================================================================

%% @private
-spec tool_schema_to_mcp(map()) -> map().
tool_schema_to_mcp(Schema) ->
    #{
        name => maps:get(name, Schema),
        description => maps:get(description, Schema, <<>>),
        inputSchema => maps:get(input_schema, Schema, #{})
    }.

%% @private
-spec extract_resource_name(binary()) -> binary().
extract_resource_name(Uri) ->
    case binary:split(Uri, <<"/">>, [global, trim_all]) of
        [_, _CaseId, ResourceType] -> ResourceType;
        [_, ResourceType] -> ResourceType;
        _ -> <<"Resource">>
    end.

%% @private
-spec generate_prompt_messages(binary(), map()) -> [map()].
generate_prompt_messages(<<"approval_workflow">>, Args) ->
    Title = maps:get(<<"title">>, Args, <<"Approval Request">>),
    [
        #{
            role => <<"user">>,
            content => #{
                type => <<"text">>,
                text => iolist_to_binary([
                    <<"Create an approval workflow for: ">>,
                    Title,
                    <<"\n\nThis workflow should support submit, approve, and reject transitions.">>
                ])
            }
        }
    ];
generate_prompt_messages(<<"simple_workflow">>, Args) ->
    Name = maps:get(<<"name">>, Args, <<"Workflow">>),
    [
        #{
            role => <<"user">>,
            content => #{
                type => <<"text">>,
                text => iolist_to_binary([
                    <<"Create a simple workflow named: ">>,
                    Name
                ])
            }
        }
    ];
generate_prompt_messages(_Name, _Args) ->
    [
        #{
            role => <<"user">>,
            content => #{
                type => <<"text">>,
                text => <<"Execute the workflow prompt">>
            }
        }
    ].

%%% ============================================================================
%%% Internal Functions - JSON-RPC Protocol
%%% ============================================================================

%% @private
-spec validate_jsonrpc_request(map()) -> {ok, jsonrpc_request()} | {error, term()}.
validate_jsonrpc_request(Request) when is_map(Request) ->
    case maps:get(<<"jsonrpc">>, Request, undefined) of
        <<"2.0">> ->
            case maps:get(<<"method">>, Request, undefined) of
                Method when is_binary(Method) ->
                    {ok, Request};
                _ ->
                    {error, missing_method}
            end;
        _ ->
            {error, invalid_jsonrpc_version}
    end;
validate_jsonrpc_request(_) ->
    {error, request_not_map}.

%% @doc Format successful JSON-RPC response
-spec format_response(request_id(), term()) -> jsonrpc_response().
format_response(Id, Result) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        result => Result
    }.

%% @doc Format JSON-RPC error response
-spec format_error(request_id(), term()) -> jsonrpc_error().
format_error(Id, parse_error) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_PARSE_ERROR,
            message => <<"Parse error">>
        }
    };
format_error(Id, {invalid_request, Reason}) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_INVALID_REQUEST,
            message => <<"Invalid request">>,
            data => format_error_data(Reason)
        }
    };
format_error(Id, {method_not_found, Method}) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_METHOD_NOT_FOUND,
            message => <<"Method not found">>,
            data => #{method => Method}
        }
    };
format_error(Id, {invalid_params, Reason}) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_INVALID_PARAMS,
            message => <<"Invalid params">>,
            data => format_error_data(Reason)
        }
    };
format_error(Id, {internal_error, Reason}) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_INTERNAL_ERROR,
            message => <<"Internal error">>,
            data => format_error_data(Reason)
        }
    };
format_error(Id, Reason) ->
    #{
        jsonrpc => <<"2.0">>,
        id => Id,
        error => #{
            code => ?ERR_INTERNAL_ERROR,
            message => <<"Error">>,
            data => format_error_data(Reason)
        }
    }.

%% @private
-spec format_error_data(term()) -> term().
format_error_data(Reason) when is_binary(Reason) ->
    Reason;
format_error_data(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error_data(Reason) when is_map(Reason) ->
    Reason;
format_error_data(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
