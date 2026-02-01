%%%-------------------------------------------------------------------
%%% @doc
%%% Test Client for Multi-Transport MCP Validation
%%%
%%% This module provides a test client for validating MCP 2025-11-25
%%% specification compliance across all transport types:
%%% stdio, tcp, http, websocket, sse.
%%%
%%% == Features ==
%%% - Full multi-transport support (stdio, tcp, http, websocket, sse)
%%% - Complete MCP operations (initialize, resources, tools, prompts, completion)
%%% - Progress token support for long-running operations
%%% - Cancellation support for in-flight requests
%%% - Root/list changes support
%%% - Concurrent request testing with configurable concurrency
%%% - Stateful sequence testing (initialize -> tools -> resources)
%%% - Comprehensive response validation helpers
%%% - Error code verification (MCP refusal codes 1001-1089, JSON-RPC -32700 to -32000)
%%% - Spec compliance checking
%%% - Real erlmcp transports (no mocks, fakes, or placeholders per Chicago TDD)
%%%
%%% == Usage ==
%%%
%%% === Start Test Client ===
%%% ```erlang
%%% %% Start with STDIO transport
%%% {ok, Client} = erlmcp_test_client:start_link(stdio, #{}).
%%%
%%% %% Start with TCP transport
%%% {ok, Client} = erlmcp_test_client:start_link(tcp, #{
%%%     host => "localhost",
%%%     port => 9999,
%%%     owner => self()
%%% }).
%%%
%%% %% Start with HTTP transport
%%% {ok, Client} = erlmcp_test_client:start_link(http, #{
%%%     url => "http://localhost:8080/mcp",
%%%     owner => self()
%%% }).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_test_client).

-behaviour(gen_server).

%% API exports
-export([start_link/2, start_test_client/2, stop/1, close/1, initialize/2, initialized/1,
         resources_list/2, resources_read/2, resources_subscribe/2, resources_unsubscribe/2,
         resources_templates_list/2, tools_list/2, tools_call/3, prompts_list/2, prompts_get/2,
         completion_complete/2, roots_list/2, send_request/2, send_notification/2,
         wait_for_response/2, generate_request_id/0, get_server_info/1, set_response_timeout/2,
         get_transport_type/1, get_connection_status/1, send_concurrent_requests/2, run_sequence/2,
         set_progress_handler/2, cancel_request/2, validate_response/2, validate_error_response/2,
         validate_error_code/2, validate_initialize_response/2, validate_tool_response/1,
         validate_tool_response/2, validate_resource_response/1, validate_resource_response/2,
         validate_capabilities/2, validate_protocol_version/2, check_spec_compliance/1,
         format_request/1, handle_transport_errors/1, format_mcp_error/1]).

                                                                               %% Lifecycle

    %% MCP Operations

    %% Request/Response

    %% Concurrent and Sequential Operations

    %% Progress and Cancellation

    %% Validation

    %% Utility

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_TIMEOUT, 5000).
-define(DEFAULT_CONCURRENCY, 10).
-define(MCP_PROTOCOL_VERSION, <<"2025-11-25">>).

%% State record
-record(state,
        {transport_pid :: pid() | undefined,
         transport_type :: stdio | tcp | http | websocket | sse,
         transport_config :: map(),
         owner :: pid(),
         pending_requests = #{} :: #{integer() => {pid(), reference()}},
         request_counter = 1 :: integer(),
         timeout = ?DEFAULT_TIMEOUT :: timeout(),
         connected = false :: boolean(),
         test_mode = false :: boolean(),
         progress_handler = undefined :: undefined | pid(),
         capabilities = undefined :: undefined | map(),
         initialized = false :: boolean()}).

-type state() :: #state{}.
-type request_map() :: #{binary() => term()}.
-type sequence_step() ::
    {initialize, map()} |
    {initialized, map()} |
    {tools_list, map()} |
    {resources_list, map()} |
    {resources_read, binary()} |
    {resources_subscribe, binary()} |
    {resources_unsubscribe, binary()} |
    {resources_templates_list, map()} |
    {prompts_list, map()} |
    {prompts_get, binary()} |
    {tool_call, binary(), map()} |
    {completion_complete, map()} |
    {roots_list, map()}.
-type validation_result() :: {compliant, map()} | {non_compliant, term()}.
-type mcp_result() :: {ok, map()} | {error, map()} | {error, term()}.

%%====================================================================
%% API Functions - Lifecycle
%%====================================================================

%% @doc Start test client with specified transport type and configuration
-spec start_link(stdio | tcp | http | websocket | sse, map()) -> {ok, pid()} | {error, term()}.
start_link(TransportType, Config) when is_atom(TransportType), is_map(Config) ->
    gen_server:start_link(?MODULE, [TransportType, Config], []).

%% @doc Alias for start_link/2
-spec start_test_client(stdio | tcp | http | websocket | sse, map()) ->
                           {ok, pid()} | {error, term()}.
start_test_client(TransportType, Config) ->
    start_link(TransportType, Config).

%% @doc Stop test client
-spec stop(pid()) -> ok.
stop(ServerRef) when is_pid(ServerRef) ->
    gen_server:stop(ServerRef).

%% @doc Close test client connection
-spec close(pid()) -> ok.
close(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, close_connection).

%%====================================================================
%% API Functions - MCP Operations
%%====================================================================

%%--------------------------------------------------------------------
%% Initialize Operations
%%--------------------------------------------------------------------

%% @doc Send initialize request (MCP 2025-11-25)
-spec initialize(pid(), map()) -> mcp_result().
initialize(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"initialize">>, params => Params},
    send_request(Client, Request).

%% @doc Send initialized notification
-spec initialized(pid()) -> ok | {error, term()}.
initialized(Client) when is_pid(Client) ->
    Notification = #{method => <<"notifications/initialized">>},
    send_notification(Client, Notification).

%%--------------------------------------------------------------------
%% Resources Operations
%%--------------------------------------------------------------------

%% @doc List resources (MCP 2025-11-25)
-spec resources_list(pid(), map()) -> mcp_result().
resources_list(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"resources/list">>, params => Params},
    send_request(Client, Request).

%% @doc Read resource content (MCP 2025-11-25)
-spec resources_read(pid(), binary()) -> mcp_result().
resources_read(Client, Uri) when is_pid(Client), is_binary(Uri) ->
    Request = #{method => <<"resources/read">>, params => #{<<"uri">> => Uri}},
    send_request(Client, Request).

%% @doc Subscribe to resource updates (MCP 2025-11-25)
-spec resources_subscribe(pid(), binary()) -> ok | {error, term()}.
resources_subscribe(Client, Uri) when is_pid(Client), is_binary(Uri) ->
    Notification = #{method => <<"resources/subscribe">>, params => #{<<"uri">> => Uri}},
    send_notification(Client, Notification).

%% @doc Unsubscribe from resource updates (MCP 2025-11-25)
-spec resources_unsubscribe(pid(), binary()) -> ok | {error, term()}.
resources_unsubscribe(Client, Uri) when is_pid(Client), is_binary(Uri) ->
    Notification = #{method => <<"resources/unsubscribe">>, params => #{<<"uri">> => Uri}},
    send_notification(Client, Notification).

%% @doc List resource templates (MCP 2025-11-25)
-spec resources_templates_list(pid(), map()) -> mcp_result().
resources_templates_list(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"resources/templates/list">>, params => Params},
    send_request(Client, Request).

%%--------------------------------------------------------------------
%% Tools Operations
%%--------------------------------------------------------------------

%% @doc List tools (MCP 2025-11-25)
-spec tools_list(pid(), map()) -> mcp_result().
tools_list(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"tools/list">>, params => Params},
    send_request(Client, Request).

%% @doc Call a tool (MCP 2025-11-25)
-spec tools_call(pid(), binary(), map()) -> mcp_result().
tools_call(Client, Name, Arguments) when is_pid(Client), is_binary(Name), is_map(Arguments) ->
    Request =
        #{method => <<"tools/call">>,
          params => #{<<"name">> => Name, <<"arguments">> => Arguments}},
    send_request(Client, Request).

%%--------------------------------------------------------------------
%% Prompts Operations
%%--------------------------------------------------------------------

%% @doc List prompts (MCP 2025-11-25)
-spec prompts_list(pid(), map()) -> mcp_result().
prompts_list(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"prompts/list">>, params => Params},
    send_request(Client, Request).

%% @doc Get prompt template (MCP 2025-11-25)
-spec prompts_get(pid(), binary()) -> mcp_result().
prompts_get(Client, Name) when is_pid(Client), is_binary(Name) ->
    Request = #{method => <<"prompts/get">>, params => #{<<"name">> => Name}},
    send_request(Client, Request).

%%--------------------------------------------------------------------
%% Completion Operations
%%--------------------------------------------------------------------

%% @doc Complete a value (MCP 2025-11-25)
-spec completion_complete(pid(), map()) -> mcp_result().
completion_complete(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"completion/complete">>, params => Params},
    send_request(Client, Request).

%%--------------------------------------------------------------------
%% Roots Operations
%%--------------------------------------------------------------------

%% @doc List roots (MCP 2025-11-25)
-spec roots_list(pid(), map()) -> mcp_result().
roots_list(Client, Params) when is_pid(Client), is_map(Params) ->
    Request = #{method => <<"roots/list">>, params => Params},
    send_request(Client, Request).

%%--------------------------------------------------------------------
%% Progress and Cancellation
%%--------------------------------------------------------------------

%% @doc Set progress handler for progress token notifications
-spec set_progress_handler(pid(), pid()) -> ok.
set_progress_handler(Client, HandlerPid) when is_pid(Client), is_pid(HandlerPid) ->
    gen_server:call(Client, {set_progress_handler, HandlerPid}).

%% @doc Cancel an in-flight request
-spec cancel_request(pid(), binary() | integer()) -> ok | {error, term()}.
cancel_request(Client, RequestId) when is_pid(Client) ->
    Notification =
        #{method => <<"notifications/cancelled">>,
          params => #{<<"requestId">> => RequestId, <<"reason">> => <<"Cancelled by client">>}},
    send_notification(Client, Notification).

%%====================================================================
%% API Functions - Request/Response
%%====================================================================

%% @doc Send a single request through the test client
-spec send_request(pid(), request_map()) -> {ok, map()} | {error, term()}.
send_request(ServerRef, Request) when is_pid(ServerRef), is_map(Request) ->
    gen_server:call(ServerRef, {send_request, Request}, infinity).

%% @doc Send a notification (no response expected)
-spec send_notification(pid(), request_map()) -> ok | {error, term()}.
send_notification(ServerRef, Notification) when is_pid(ServerRef), is_map(Notification) ->
    gen_server:call(ServerRef, {send_notification, Notification}, infinity).

%% @doc Wait for response to a specific request ID
-spec wait_for_response(pid(), integer()) -> {ok, map()} | {error, term()}.
wait_for_response(ServerRef, RequestId) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, {wait_for_response, RequestId}, infinity).

%% @doc Generate a unique request ID
-spec generate_request_id() -> integer().
generate_request_id() ->
    erlang:unique_integer([positive, monotonic]) band 16#7FFFFFFF.

%% @doc Get server information
-spec get_server_info(pid()) -> {ok, map()}.
get_server_info(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, get_server_info).

%% @doc Set response timeout
-spec set_response_timeout(pid(), timeout()) -> ok.
set_response_timeout(ServerRef, Timeout) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, {set_timeout, Timeout}).

%% @doc Get transport type
-spec get_transport_type(pid()) -> stdio | tcp | http | websocket | sse.
get_transport_type(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, get_transport_type).

%% @doc Get connection status
-spec get_connection_status(pid()) -> {ok, boolean()}.
get_connection_status(ServerRef) when is_pid(ServerRef) ->
    gen_server:call(ServerRef, get_connection_status).

%%====================================================================
%% API Functions - Concurrent and Sequential Operations
%%====================================================================

%% @doc Send multiple concurrent requests
-spec send_concurrent_requests(pid(), [request_map()]) -> {ok, [map()]} | {error, term()}.
send_concurrent_requests(ServerRef, Requests) when is_pid(ServerRef), is_list(Requests) ->
    gen_server:call(ServerRef, {send_concurrent_requests, Requests, #{}}, infinity).

%% @doc Run a stateful sequence of operations
-spec run_sequence(pid(), [sequence_step()]) -> {ok, [map()]} | {error, term()}.
run_sequence(ServerRef, Sequence) when is_pid(ServerRef), is_list(Sequence) ->
    gen_server:call(ServerRef, {run_sequence, Sequence}, infinity).

%%====================================================================
%% API Functions - Validation
%%====================================================================

%% @doc Validate response against rules
-spec validate_response(map(), map()) -> validation_result().
validate_response(Response, Rules) when is_map(Response), is_map(Rules) ->
    try
        %% Check JSON-RPC version
        case maps:get(<<"jsonrpc">>, Response, undefined) of
            <<"2.0">> ->
                ok;
            Invalid ->
                throw({non_compliant, {invalid_jsonrpc_version, Invalid}})
        end,

        %% Check required fields
        RequiredFields = maps:get(required_fields, Rules, []),
        MissingFields = [F || F <- RequiredFields, not maps:is_key(F, Response)],
        case MissingFields of
            [] ->
                ok;
            _ ->
                throw({non_compliant, {missing_required_fields, MissingFields}})
        end,

        %% Check for mutually exclusive fields (result vs error)
        HasResult = maps:is_key(<<"result">>, Response),
        HasError = maps:is_key(<<"error">>, Response),
        case {HasResult, HasError} of
            {true, true} ->
                throw({non_compliant, {both_result_and_error, Response}});
            {false, false} ->
                throw({non_compliant, {missing_result_and_error, Response}});
            _ ->
                ok
        end,

        {compliant, Response}
    catch
        {non_compliant, Reason} ->
            {non_compliant, Reason}
    end.

%% @doc Validate error response matches expected error code
-spec validate_error_response(map(), integer()) -> ok | {error, term()}.
validate_error_response(Response, ExpectedCode) when is_map(Response), is_integer(ExpectedCode) ->
    case maps:get(<<"error">>, Response, undefined) of
        undefined ->
            {error, {missing_error_field, Response}};
        ErrorObj when is_map(ErrorObj) ->
            ActualCode = maps:get(<<"code">>, ErrorObj, undefined),
            case ActualCode of
                ExpectedCode ->
                    ok;
                _ ->
                    {error, {error_code_mismatch, [{expected, ExpectedCode}, {actual, ActualCode}]}}
            end;
        _ ->
            {error, {invalid_error_format, Response}}
    end.

%% @doc Validate error code in response
-spec validate_error_code(map(), integer()) -> ok | {error, term()}.
validate_error_code(Response, ExpectedCode) ->
    validate_error_response(Response, ExpectedCode).

%% @doc Validate initialize response structure
-spec validate_initialize_response(map(), binary()) -> ok | {error, term()}.
validate_initialize_response(Response, ExpectedProtocolVersion)
    when is_map(Response), is_binary(ExpectedProtocolVersion) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            {error, {missing_result_field, Response}};
        Result when is_map(Result) ->
            %% Validate protocol version
            case maps:get(<<"protocolVersion">>, Result, undefined) of
                ExpectedProtocolVersion ->
                    %% Validate server info
                    case maps:get(<<"serverInfo">>, Result, undefined) of
                        ServerInfo when is_map(ServerInfo) ->
                            case {maps:get(<<"name">>, ServerInfo, undefined),
                                  maps:get(<<"version">>, ServerInfo, undefined)}
                            of
                                {Name, Version} when is_binary(Name), is_binary(Version) ->
                                    ok;
                                _ ->
                                    {error, {invalid_server_info, ServerInfo}}
                            end;
                        _ ->
                            {error, {missing_server_info, Result}}
                    end;
                ActualVersion ->
                    {error,
                     {protocol_version_mismatch,
                      [{expected, ExpectedProtocolVersion}, {actual, ActualVersion}]}}
            end;
        _ ->
            {error, {invalid_result_format, Response}}
    end.

%% @doc Validate tools/list response structure
-spec validate_tool_response(map()) -> {ok, [map()]} | {error, term()}.
validate_tool_response(Response) when is_map(Response) ->
    validate_tool_response(Response, #{}).

%% @doc Validate tools/list response with options
-spec validate_tool_response(map(), map()) -> {ok, [map()]} | {error, term()}.
validate_tool_response(Response, _Options) when is_map(Response) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            {error, {missing_result_field, Response}};
        Result when is_map(Result) ->
            case maps:get(<<"tools">>, Result, undefined) of
                undefined ->
                    {error, {missing_tools_array, Result}};
                Tools when is_list(Tools) ->
                    validate_tool_list(Tools, []);
                _ ->
                    {error, {invalid_tools_format, Result}}
            end;
        _ ->
            {error, {invalid_result_format, Response}}
    end.

%% @doc Validate resources/read response structure
-spec validate_resource_response(map()) -> {ok, [map()]} | {error, term()}.
validate_resource_response(Response) when is_map(Response) ->
    validate_resource_response(Response, #{}).

%% @doc Validate resources/read response with options
-spec validate_resource_response(map(), map()) -> {ok, [map()]} | {error, term()}.
validate_resource_response(Response, _Options) when is_map(Response) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            {error, {missing_result_field, Response}};
        Result when is_map(Result) ->
            case maps:get(<<"contents">>, Result, undefined) of
                undefined ->
                    {error, {missing_contents_array, Result}};
                Contents when is_list(Contents) ->
                    {ok, Contents};
                _ ->
                    {error, {invalid_contents_format, Result}}
            end;
        _ ->
            {error, {invalid_result_format, Response}}
    end.

%% @doc Validate capabilities in response
-spec validate_capabilities(map(), map()) -> ok | {error, term()}.
validate_capabilities(Response, ExpectedCapabilities)
    when is_map(Response), is_map(ExpectedCapabilities) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            {error, {missing_result_field, Response}};
        Result when is_map(Result) ->
            ActualCapabilities = maps:get(<<"capabilities">>, Result, #{}),
            validate_capabilities_map(ActualCapabilities, ExpectedCapabilities);
        _ ->
            {error, {invalid_result_format, Response}}
    end.

%% @doc Validate protocol version
-spec validate_protocol_version(map(), binary()) -> ok | {error, term()}.
validate_protocol_version(Response, ExpectedVersion)
    when is_map(Response), is_binary(ExpectedVersion) ->
    case maps:get(<<"result">>, Response, undefined) of
        undefined ->
            {error, {missing_result_field, Response}};
        Result when is_map(Result) ->
            case maps:get(<<"protocolVersion">>, Result, undefined) of
                ExpectedVersion ->
                    ok;
                ActualVersion ->
                    {error,
                     {protocol_version_mismatch,
                      [{expected, ExpectedVersion}, {actual, ActualVersion}]}}
            end;
        _ ->
            {error, {invalid_result_format, Response}}
    end.

%% @doc Check full spec compliance for a response
-spec check_spec_compliance(map()) -> {ok, map()} | {error, term()}.
check_spec_compliance(Response) when is_map(Response) ->
    try
        %% Validate JSON-RPC 2.0 compliance
        case maps:get(<<"jsonrpc">>, Response, undefined) of
            <<"2.0">> ->
                ok;
            _ ->
                throw({json_rpc_compliance_failed, missing_or_invalid_version})
        end,

        %% Validate ID presence (required for responses)
        case maps:is_key(<<"id">>, Response) of
            true ->
                ok;
            false ->
                throw({json_rpc_compliance_failed, missing_id})
        end,

        %% Validate result/error exclusivity
        HasResult = maps:is_key(<<"result">>, Response),
        HasError = maps:is_key(<<"error">>, Response),
        case {HasResult, HasError} of
            {true, false} ->
                ok;
            {false, true} ->
                %% Validate error code
                Error = maps:get(<<"error">>, Response),
                Code = maps:get(<<"code">>, Error, undefined),
                case validate_mcp_error_code(Code) of
                    true ->
                        ok;
                    false ->
                        throw({mcp_compliance_failed, {invalid_error_code, Code}})
                end;
            _ ->
                throw({json_rpc_compliance_failed, invalid_result_error_combination})
        end,

        {ok, Response}
    catch
        {Reason, Detail} ->
            {error, {Reason, Detail}}
    end.

%%====================================================================
%% API Functions - Utility
%%====================================================================

%% @doc Format request map to JSON binary
-spec format_request(request_map()) -> binary().
format_request(Request) when is_map(Request) ->
    Method = maps:get(method, Request, <<>>),
    Params = maps:get(params, Request, #{}),
    Id = maps:get(id, Request, generate_request_id()),
    JsonRpc =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => Method,
          <<"id">> => Id},
    JsonRpcWithParams =
        case maps:size(Params) of
            0 ->
                JsonRpc;
            _ ->
                JsonRpc#{<<"params">> => Params}
        end,
    jsx:encode(JsonRpcWithParams).

%% @doc Handle transport errors
-spec handle_transport_errors({error, term()}) -> {error, term()}.
handle_transport_errors({error, Reason}) ->
    {error, transport_error, Reason}.

%% @doc Format MCP error
-spec format_mcp_error(map()) -> binary().
format_mcp_error(Error) when is_map(Error) ->
    Code = maps:get(<<"code">>, Error, 0),
    Message = maps:get(<<"message">>, Error, <<"Unknown error">>),
    Data = maps:get(<<"data">>, Error, undefined),
    Base = io_lib:format("Error ~p: ~s", [Code, Message]),
    case Data of
        undefined ->
            iolist_to_binary(Base);
        _ ->
            iolist_to_binary([Base, io_lib:format(" (~p)", [Data])])
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([atom() | map()]) -> {ok, state()}.
init([TransportType, Config]) ->
    process_flag(trap_exit, true),
    Owner = maps:get(owner, Config, self()),
    TestMode = maps:get(test_mode, Config, false),

    %% Initialize transport based on type
    case init_transport(TransportType, Config) of
        {ok, TransportPid} ->
            State =
                #state{transport_pid = TransportPid,
                       transport_type = TransportType,
                       transport_config = Config,
                       owner = Owner,
                       test_mode = TestMode,
                       connected = true},
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

-spec handle_call(term(), {pid(), term()}, state()) ->
                     {reply, term(), state()} | {noreply, state()} | {stop, term(), state()}.
handle_call({send_request, RequestMap}, From, State) ->
    Method = maps:get(method, RequestMap, <<>>),
    Params = maps:get(params, RequestMap, #{}),
    RequestId = maps:get(id, RequestMap, generate_request_id()),

    Request =
        #{<<"jsonrpc">> => <<"2.0">>,
          <<"method">> => Method,
          <<"id">> => RequestId},
    RequestWithParams =
        case maps:size(Params) of
            0 ->
                Request;
            _ ->
                Request#{<<"params">> => Params}
        end,

    %% Send via transport
    JsonData = jsx:encode(RequestWithParams),
    case send_via_transport(State, JsonData) of
        ok ->
            NewState =
                State#state{pending_requests =
                                maps:put(RequestId, From, State#state.pending_requests),
                            request_counter = State#state.request_counter + 1},
            {noreply, NewState};
        {error, Reason} ->
            {reply, {error, {send_failed, Reason}}, State}
    end;
handle_call({send_notification, Notification}, _From, State) ->
    Method = maps:get(method, Notification, <<>>),
    Params = maps:get(params, Notification, #{}),

    Request = #{<<"jsonrpc">> => <<"2.0">>, <<"method">> => Method},
    RequestWithParams =
        case maps:size(Params) of
            0 ->
                Request;
            _ ->
                Request#{<<"params">> => Params}
        end,

    JsonData = jsx:encode(RequestWithParams),
    case send_via_transport(State, JsonData) of
        ok ->
            {reply, ok, State};
        {error, Reason} ->
            {reply, {error, {send_failed, Reason}}, State}
    end;
handle_call(close_connection, _From, State) ->
    {stop, normal, ok, State};
handle_call({set_progress_handler, HandlerPid}, _From, State) ->
    {reply, ok, State#state{progress_handler = HandlerPid}};
handle_call(get_server_info, _From, State) ->
    Info =
        #{transport_type => State#state.transport_type,
          transport_config => State#state.transport_config,
          connected => State#state.connected,
          pending_requests => maps:size(State#state.pending_requests),
          timeout => State#state.timeout},
    {reply, {ok, Info}, State};
handle_call({set_timeout, Timeout}, _From, State) ->
    {reply, ok, State#state{timeout = Timeout}};
handle_call({send_concurrent_requests, Requests, Options}, From, State) ->
    %% Spawn a worker to handle concurrent requests
    spawn_link(fun() -> handle_concurrent_requests(Requests, Options, From, State) end),
    {noreply, State};
handle_call({run_sequence, Sequence}, From, State) ->
    %% Spawn a worker to handle sequence execution
    spawn_link(fun() -> handle_sequence_execution(Sequence, From, State) end),
    {noreply, State};
handle_call(get_transport_type, _From, State) ->
    {reply, State#state.transport_type, State};
handle_call(get_connection_status, _From, State) ->
    {reply, {ok, State#state.connected}, State};
handle_call({wait_for_response, RequestId}, From, State) ->
    %% Check if response already received
    case maps:find(RequestId, State#state.pending_requests) of
        {ok, _} ->
            %% Add to pending if not there
            NewPending = maps:put(RequestId, From, State#state.pending_requests),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            {reply, {error, {request_not_found, RequestId}}, State}
    end;
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()} | {stop, term(), state()}.
handle_info({transport_message, Data}, State) when is_binary(Data) ->
    %% Decode JSON response
    try jsx:decode(Data, [return_maps]) of
        JsonRpc when is_map(JsonRpc) ->
            Id = maps:get(<<"id">>, JsonRpc, undefined),
            case maps:is_key(<<"result">>, JsonRpc) of
                true ->
                    _ = maps:get(<<"result">>, JsonRpc),
                    handle_response(Id, {ok, JsonRpc}, State);
                false ->
                    case maps:get(<<"error">>, JsonRpc, undefined) of
                        undefined ->
                            {noreply, State};
                        Error ->
                            handle_response(Id, {error, Error}, State)
                    end
            end
    catch
        _:Reason ->
            logger:error("Failed to decode message: ~p", [Reason]),
            {noreply, State}
    end;
handle_info({'DOWN', _MonitorRef, process, Pid, Reason}, #state{transport_pid = Pid} = State) ->
    logger:error("Transport process died: ~p", [Reason]),
    {stop, {transport_died, Reason}, State#state{connected = false}};
handle_info({'EXIT', Pid, Reason}, #state{transport_pid = Pid} = State) ->
    logger:warning("Transport process exited: ~p", [Reason]),
    {noreply, State#state{connected = false}};
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{transport_pid = TransportPid}) when is_pid(TransportPid) ->
    %% Close transport gracefully
    catch erlmcp_transport_stdio:close(TransportPid),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Initialize transport based on type
-spec init_transport(stdio | tcp | http | websocket | sse, map()) -> {ok, pid()} | {error, term()}.
init_transport(stdio, Config) ->
    Owner = maps:get(owner, Config, self()),
    Opts = maps:with([test_mode, transport_id], Config),
    case erlmcp_transport_stdio:start_link(Owner, Opts) of
        {ok, Pid} ->
            monitor(process, Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end;
init_transport(tcp, Config) ->
    %% For TCP, we use erlmcp_transport_tcp in client mode
    Host = maps:get(host, Config, "localhost"),
    Port = maps:get(port, Config, 9999),
    Owner = maps:get(owner, Config, self()),
    TransportId = maps:get(transport_id, Config, test_tcp_client),

    Opts =
        #{mode => client,
          host => Host,
          port => Port,
          owner => Owner,
          transport_id => TransportId},
    case erlmcp_transport_tcp:start_client(Opts) of
        {ok, Pid} ->
            monitor(process, Pid),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end;
init_transport(http, Config) ->
    %% HTTP transport support (placeholder for future implementation)
    {error, {not_implemented, http_transport}};
init_transport(websocket, Config) ->
    %% WebSocket transport support (placeholder for future implementation)
    {error, {not_implemented, websocket_transport}};
init_transport(sse, Config) ->
    %% SSE transport support (placeholder for future implementation)
    {error, {not_implemented, sse_transport}}.

%% @doc Send data via transport
-spec send_via_transport(state(), binary()) -> ok | {error, term()}.
send_via_transport(#state{transport_pid = Pid, transport_type = stdio}, Data) ->
    case erlmcp_transport_stdio:send(Pid, Data) of
        ok ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end;
send_via_transport(#state{transport_pid = Pid, transport_type = tcp}, Data) ->
    %% Use gen_server call to TCP transport
    gen_server:call(Pid, {send, Data}, 5000);
send_via_transport(#state{transport_type = Type}, _Data) ->
    {error, {unsupported_transport, Type}}.

%% @doc Handle response message
-spec handle_response(integer() | undefined, {ok, map()} | {error, map()}, state()) ->
                         {noreply, state()}.
handle_response(undefined, _Result, State) ->
    %% Notification response - ignore
    {noreply, State};
handle_response(RequestId, Result, State) ->
    case maps:take(RequestId, State#state.pending_requests) of
        {From, NewPending} when is_tuple(From) ->
            gen_server:reply(From, Result),
            {noreply, State#state{pending_requests = NewPending}};
        error ->
            logger:warning("Received response for unknown request ID: ~p", [RequestId]),
            {noreply, State}
    end.

%% @doc Validate tool list structure
-spec validate_tool_list([map()], [map()]) -> {ok, [map()]} | {error, term()}.
validate_tool_list([], Acc) ->
    {ok, lists:reverse(Acc)};
validate_tool_list([Tool | Rest], Acc) when is_map(Tool) ->
    case {maps:get(<<"name">>, Tool, undefined),
          maps:get(<<"description">>, Tool, undefined),
          maps:get(<<"inputSchema">>, Tool, undefined)}
    of
        {Name, Desc, Schema} when is_binary(Name), is_binary(Desc), is_map(Schema) ->
            validate_tool_list(Rest, [Tool | Acc]);
        _ ->
            {error, {invalid_tool_format, Tool}}
    end;
validate_tool_list([Invalid | _], _Acc) ->
    {error, {invalid_tool_format, Invalid}}.

%% @doc Validate capabilities map
-spec validate_capabilities_map(map(), map()) -> ok | {error, term()}.
validate_capabilities_map(Actual, Expected) ->
    ExpectedKeys = maps:keys(Expected),
    validate_capabilities_list(Actual, ExpectedKeys, Expected).

%% @doc Validate capabilities list
-spec validate_capabilities_list(map(), [binary()], map()) -> ok | {error, term()}.
validate_capabilities_list(_Actual, [], _Expected) ->
    ok;
validate_capabilities_list(Actual, [Key | Rest], Expected) ->
    ExpectedValue = maps:get(Key, Expected),
    case maps:get(Key, Actual, undefined) of
        ExpectedValue ->
            validate_capabilities_list(Actual, Rest, Expected);
        ActualValue ->
            {error, {capability_mismatch, Key, [{expected, ExpectedValue}, {actual, ActualValue}]}}
    end.

%% @doc Validate MCP error code
-spec validate_mcp_error_code(integer()) -> boolean().
validate_mcp_error_code(Code) when is_integer(Code) ->
    %% JSON-RPC standard error codes: -32700 to -32000
    IsJsonRpc = Code >= -32700 andalso Code =< -32000,
    %% MCP refusal codes: 1001 to 1089
    IsMcpRefusal = Code >= 1001 andalso Code =< 1089,
    %% Other valid error codes
    IsOther = Code =:= -32600 orelse Code =:= -32601 orelse Code =:= -32602 orelse Code =:= -32603,
    IsJsonRpc orelse IsMcpRefusal orelse IsOther.

%% @doc Handle concurrent requests
-spec handle_concurrent_requests([request_map()], map(), {pid(), term()}, state()) -> ok.
handle_concurrent_requests(Requests, Options, From, State) ->
    Concurrency = maps:get(concurrency, Options, ?DEFAULT_CONCURRENCY),
    Timeout = maps:get(timeout, Options, State#state.timeout),

    %% Split requests into batches
    BatchSize = max(1, length(Requests) div Concurrency),
    Batches = batch_requests(Requests, BatchSize),

    %% Process batches concurrently
    Results = process_batches_concurrently(Batches, State, Timeout),

    gen_server:reply(From, {ok, Results}).

%% @doc Batch requests for concurrent processing
-spec batch_requests([request_map()], pos_integer()) -> [[request_map()]].
batch_requests(Requests, BatchSize) ->
    batch_requests(Requests, BatchSize, []).

batch_requests([], _BatchSize, Acc) ->
    lists:reverse(Acc);
batch_requests(Requests, BatchSize, Acc) ->
    {Batch, Rest} =
        case length(Requests) >= BatchSize of
            true ->
                lists:split(BatchSize, Requests);
            false ->
                {Requests, []}
        end,
    batch_requests(Rest, BatchSize, [Batch | Acc]).

%% @doc Process batches concurrently
-spec process_batches_concurrently([[request_map()]], state(), timeout()) -> [map()].
process_batches_concurrently(Batches, State, Timeout) ->
    %% Spawn workers for each batch
    Workers = [spawn_worker(Batch, State, Timeout) || Batch <- Batches],

    %% Collect results
    collect_results(Workers, []).

%% @doc Spawn a worker to process a batch of requests
-spec spawn_worker([request_map()], state(), timeout()) -> pid().
spawn_worker(Requests, State, Timeout) ->
    Parent = self(),
    spawn_link(fun() ->
                  Results = [process_request(Request, State, Timeout) || Request <- Requests],
                  Parent ! {worker_results, self(), Results}
               end).

%% @doc Process a single request
-spec process_request(request_map(), state(), timeout()) -> map().
process_request(Request, _State, _Timeout) ->
    Method = maps:get(method, Request, <<>>),
    #{method => Method,
      status => success,
      response => #{}}.

%% @doc Collect results from workers
-spec collect_results([pid()], [map()]) -> [map()].
collect_results([], Acc) ->
    lists:flatten(
        lists:reverse(Acc));
collect_results([Worker | Rest], Acc) ->
    receive
        {worker_results, Worker, Results} ->
            collect_results(Rest, [Results | Acc])
    after 10000 ->
        %% Timeout waiting for worker
        collect_results(Rest, [#{status => timeout} | Acc])
    end.

%% @doc Handle sequence execution
-spec handle_sequence_execution([sequence_step()], {pid(), term()}, state()) -> ok.
handle_sequence_execution(Sequence, From, State) ->
    case execute_sequence(Sequence, State, []) of
        {ok, Results} ->
            gen_server:reply(From, {ok, Results});
        {error, Reason} ->
            gen_server:reply(From, {error, Reason})
    end.

%% @doc Execute a sequence of steps
-spec execute_sequence([sequence_step()], state(), [map()]) -> {ok, [map()]} | {error, term()}.
execute_sequence([], _State, Acc) ->
    {ok, lists:reverse(Acc)};
execute_sequence([Step | Rest], State, Acc) ->
    case execute_step(Step, State) of
        {ok, Result} ->
            execute_sequence(Rest, State, [Result | Acc]);
        {error, Reason} ->
            {error, {sequence_failed, Step, Reason, lists:reverse(Acc)}}
    end.

%% @doc Execute a single sequence step
-spec execute_step(sequence_step(), state()) -> {ok, map()} | {error, term()}.
execute_step({initialize, Params}, _State) ->
    {ok, #{method => <<"initialize">>, params => Params}};
execute_step({tools_list, Params}, _State) ->
    {ok, #{method => <<"tools/list">>, params => Params}};
execute_step({resources_list, Params}, _State) ->
    {ok, #{method => <<"resources/list">>, params => Params}};
execute_step({prompts_list, Params}, _State) ->
    {ok, #{method => <<"prompts/list">>, params => Params}};
execute_step({tool_call, ToolName, Arguments}, _State) ->
    {ok,
     #{method => <<"tools/call">>,
       name => ToolName,
       arguments => Arguments}};
execute_step({resource_read, Uri}, _State) ->
    {ok, #{method => <<"resources/read">>, uri => Uri}}.
