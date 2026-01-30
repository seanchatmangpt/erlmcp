%%%-------------------------------------------------------------------
%%% @doc erlmcp_spec_parser - MCP Specification Parser
%%%
%%% This module provides hardcoded MCP 2025-11-25 specification metadata
%%% for validation and compliance checking. It serves as the single source
%%% of truth for MCP protocol requirements.
%%%
%%% Chicago School TDD: Real gen_server, state-based verification, no mocks
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_spec_parser).
-behaviour(gen_server).

%% API exports
-export([
    start_link/0,
    stop/0,
    get_spec/0,
    parse_spec/0,

    %% Method requirements
    get_method_requirements/0,
    get_method_requirements/1,
    list_methods/0,

    %% Error code requirements
    get_error_requirements/0,
    get_error_requirements/1,
    list_error_codes/0,

    %% Transport requirements
    get_transport_requirements/0,
    get_transport_requirements/1,
    list_transports/0,

    %% Capability requirements
    get_capability_requirements/0,
    get_capability_requirements/1,
    list_capabilities/0,

    %% Validation functions
    validate_message/1,
    validate_method_call/2,
    validate_error_code/1,
    check_capability_support/2,
    generate_validation_rules/0,

    %% Helper functions for spec metadata
    spec_version/0,
    is_valid_error_code/1,
    is_valid_request_type/1,
    is_valid_notification_type/1,
    get_required_capabilities/1,
    get_all_error_codes/0,
    get_all_request_types/0,
    get_all_notification_types/0,
    is_valid_uri/1,
    is_valid_request_id/1,
    get_method_params/1,
    get_method_result/1,
    get_capability_features/1,
    get_version_compatibility/1,
    is_deprecated_method/1
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

-include("erlmcp_spec_parser.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% API
%%%====================================================================

%% @doc Start the spec parser gen_server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the spec parser gen_server
stop() ->
    gen_server:stop(?MODULE).

%% @doc Get the full MCP specification record
get_spec() ->
    gen_server:call(?MODULE, get_spec).

%% @doc Parse and return the MCP specification (alias for get_spec)
parse_spec() ->
    get_spec().

%% @doc Get all method requirements
get_method_requirements() ->
    gen_server:call(?MODULE, get_method_requirements).

%% @doc Get requirements for a specific method
get_method_requirements(MethodName) when is_binary(MethodName) ->
    gen_server:call(?MODULE, {get_method_requirements, MethodName}).

%% @doc List all available method names
list_methods() ->
    gen_server:call(?MODULE, list_methods).

%% @doc Get all error code requirements
get_error_requirements() ->
    gen_server:call(?MODULE, get_error_requirements).

%% @doc Get requirements for a specific error code
get_error_requirements(ErrorCode) when is_integer(ErrorCode) ->
    gen_server:call(?MODULE, {get_error_requirements, ErrorCode}).

%% @doc List all error codes
list_error_codes() ->
    gen_server:call(?MODULE, list_error_codes).

%% @doc Get all transport requirements
get_transport_requirements() ->
    gen_server:call(?MODULE, get_transport_requirements).

%% @doc Get requirements for a specific transport
get_transport_requirements(TransportName) when is_binary(TransportName) ->
    gen_server:call(?MODULE, {get_transport_requirements, TransportName}).

%% @doc List all transport names
list_transports() ->
    gen_server:call(?MODULE, list_transports).

%% @doc Get all capability requirements
get_capability_requirements() ->
    gen_server:call(?MODULE, get_capability_requirements).

%% @doc Get requirements for a specific capability
get_capability_requirements(CapabilityName) when is_binary(CapabilityName) ->
    gen_server:call(?MODULE, {get_capability_requirements, CapabilityName}).

%% @doc List all capability names
list_capabilities() ->
    gen_server:call(?MODULE, list_capabilities).

%% @doc Validate a JSON-RPC message structure
validate_message(Message) when is_map(Message) ->
    gen_server:call(?MODULE, {validate_message, Message}).

%% @doc Validate a method call with parameters
validate_method_call(MethodName, Params) when is_binary(MethodName) ->
    gen_server:call(?MODULE, {validate_method_call, MethodName, Params}).

%% @doc Validate an error code
validate_error_code(ErrorCode) when is_integer(ErrorCode) ->
    gen_server:call(?MODULE, {validate_error_code, ErrorCode}).

%% @doc Check if a capability is supported
check_capability_support(CapabilityName, Features) when is_binary(CapabilityName), is_list(Features) ->
    gen_server:call(?MODULE, {check_capability_support, CapabilityName, Features}).

%% @doc Generate all validation rules
generate_validation_rules() ->
    gen_server:call(?MODULE, generate_validation_rules).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    {ok, #{} }.

handle_call(get_spec, _From, State) ->
    Spec = build_spec(),
    {reply, {ok, Spec}, State};

handle_call(get_method_requirements, _From, State) ->
    Methods = build_methods(),
    {reply, {ok, Methods}, State};

handle_call({get_method_requirements, MethodName}, _From, State) ->
    Methods = build_methods(),
    case lists:keyfind(MethodName, #method_req.name, Methods) of
        false -> {reply, {error, not_found}, State};
        MethodReq -> {reply, {ok, MethodReq}, State}
    end;

handle_call(list_methods, _From, State) ->
    Methods = build_methods(),
    MethodNames = [M#method_req.name || M <- Methods],
    {reply, {ok, MethodNames}, State};

handle_call(get_error_requirements, _From, State) ->
    Errors = build_error_codes(),
    {reply, {ok, Errors}, State};

handle_call({get_error_requirements, ErrorCode}, _From, State) ->
    Errors = build_error_codes(),
    case lists:keyfind(ErrorCode, #error_code_req.code, Errors) of
        false -> {reply, {error, not_found}, State};
        ErrorReq -> {reply, {ok, ErrorReq}, State}
    end;

handle_call(list_error_codes, _From, State) ->
    Errors = build_error_codes(),
    ErrorCodes = [E#error_code_req.code || E <- Errors],
    {reply, {ok, ErrorCodes}, State};

handle_call(get_transport_requirements, _From, State) ->
    Transports = build_transports(),
    {reply, {ok, Transports}, State};

handle_call({get_transport_requirements, TransportName}, _From, State) ->
    Transports = build_transports(),
    case lists:keyfind(TransportName, #transport_req.name, Transports) of
        false -> {reply, {error, not_found}, State};
        TransportReq -> {reply, {ok, TransportReq}, State}
    end;

handle_call(list_transports, _From, State) ->
    Transports = build_transports(),
    TransportNames = [T#transport_req.name || T <- Transports],
    {reply, {ok, TransportNames}, State};

handle_call(get_capability_requirements, _From, State) ->
    Capabilities = build_capabilities(),
    {reply, {ok, Capabilities}, State};

handle_call({get_capability_requirements, CapabilityName}, _From, State) ->
    Capabilities = build_capabilities(),
    case lists:keyfind(CapabilityName, #capability_req.name, Capabilities) of
        false -> {reply, {error, not_found}, State};
        CapabilityReq -> {reply, {ok, CapabilityReq}, State}
    end;

handle_call(list_capabilities, _From, State) ->
    Capabilities = build_capabilities(),
    CapabilityNames = [C#capability_req.name || C <- Capabilities],
    {reply, {ok, CapabilityNames}, State};

handle_call({validate_message, Message}, _From, State) ->
    Result = do_validate_message(Message),
    {reply, Result, State};

handle_call({validate_method_call, MethodName, Params}, _From, State) ->
    Result = do_validate_method_call(MethodName, Params),
    {reply, Result, State};

handle_call({validate_error_code, ErrorCode}, _From, State) ->
    Result = do_validate_error_code(ErrorCode),
    {reply, Result, State};

handle_call({check_capability_support, CapabilityName, Features}, _From, State) ->
    Result = do_check_capability_support(CapabilityName, Features),
    {reply, Result, State};

handle_call(generate_validation_rules, _From, State) ->
    Rules = build_validation_rules(),
    {reply, {ok, Rules}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%====================================================================
%%% Internal functions - Specification Builders
%%%====================================================================

%% @doc Build the complete MCP specification record
build_spec() ->
    #mcp_spec{
        version = ?MCP_2025_11_25_VERSION,
        specification_date = <<"2025-11-25">>,
        protocol_type = <<"JSON-RPC 2.0">>,
        methods = build_methods(),
        error_codes = build_error_codes(),
        transports = build_transports(),
        capabilities = build_capabilities()
    }.

%% @doc Build all method requirements (hardcoded MCP 2025-11-25 spec)
build_methods() ->
    [
        #method_req{
            name = <<"initialize">>,
            method_type = notification,
            direction = client_to_server,
            required = true,
            params_spec = #{
                protocolVersion => #{type => <<"string">>, required => true},
                capabilities => #{type => <<"object">>, required => true},
                clientInfo => #{type => <<"object">>, required => true}
            },
            result_spec = undefined,
            capability_required = undefined,
            deprecation_status = stable,
            documentation = <<"Initialize the MCP session">>
        },
        #method_req{
            name = <<"tools/list">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                cursor => #{type => <<"string">>, required => false}
            },
            result_spec = #{
                tools => #{type => <<"array">>,
                          items => #{type => <<"object">>}}
            },
            capability_required = <<"tools">>,
            deprecation_status = stable,
            documentation = <<"List available tools">>
        },
        #method_req{
            name = <<"tools/call">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                name => #{type => <<"string">>, required => true},
                arguments => #{type => <<"object">>, required => true}
            },
            result_spec = #{
                content => #{type => <<"array">>},
                isError => #{type => <<"boolean">>, required => false}
            },
            capability_required = <<"tools">>,
            deprecation_status = stable,
            documentation = <<"Call a specific tool">>
        },
        #method_req{
            name = <<"resources/read">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                uri => #{type => <<"string">>, required => true}
            },
            result_spec = #{
                contents => #{type => <<"array">>}
            },
            capability_required = <<"resources">>,
            deprecation_status = stable,
            documentation = <<"Read a resource">>
        },
        #method_req{
            name = <<"resources/list">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                cursor => #{type => <<"string">>, required => false}
            },
            result_spec = #{
                resources => #{type => <<"array">>}
            },
            capability_required = <<"resources">>,
            deprecation_status = stable,
            documentation = <<"List available resources">>
        },
        #method_req{
            name = <<"prompts/list">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                cursor => #{type => <<"string">>, required => false}
            },
            result_spec = #{
                prompts => #{type => <<"array">>}
            },
            capability_required = <<"prompts">>,
            deprecation_status = stable,
            documentation = <<"List available prompts">>
        },
        #method_req{
            name = <<"prompts/get">>,
            method_type = request,
            direction = client_to_server,
            required = false,
            params_spec = #{
                name => #{type => <<"string">>, required => true},
                arguments => #{type => <<"object">>, required => false}
            },
            result_spec = #{
                messages => #{type => <<"array">>}
            },
            capability_required = <<"prompts">>,
            deprecation_status = stable,
            documentation = <<"Get a specific prompt">>
        }
    ].

%% @doc Build all error code requirements (hardcoded MCP 2025-11-25 spec)
build_error_codes() ->
    [
        %% JSON-RPC 2.0 standard error codes
        #error_code_req{
            code = -32700,
            name = <<"Parse error">>,
            category = json_rpc,
            description = <<"Invalid JSON was received by the server">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32600,
            name = <<"Invalid Request">>,
            category = json_rpc,
            description = <<"The JSON sent is not a valid Request object">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32601,
            name = <<"Method not found">>,
            category = json_rpc,
            description = <<"The method does not exist / is not available">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32602,
            name = <<"Invalid params">>,
            category = json_rpc,
            description = <<"Invalid method parameter(s)">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32603,
            name = <<"Internal error">>,
            category = json_rpc,
            description = <<"Internal JSON-RPC error">>,
            severity = error,
            retry_strategy = retry
        },
        %% MCP protocol error codes
        #error_code_req{
            code = -32001,
            name = <<"Resource not found">>,
            category = mcp_protocol,
            description = <<"The requested resource does not exist">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32002,
            name = <<"Tool not found">>,
            category = mcp_protocol,
            description = <<"The requested tool does not exist">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = -32010,
            name = <<"Server overloaded">>,
            category = mcp_protocol,
            description = <<"The server is temporarily overloaded">>,
            severity = error,
            retry_strategy = retry
        },
        %% MCP refusal codes (1001-1089) - Complete set from MCP 2025-11-25 spec
        #error_code_req{
            code = 1001,
            name = <<"Request refused - unspecified">>,
            category = mcp_protocol,
            description = <<"Request refused for unspecified reason">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1002,
            name = <<"Request refused - invalid argument">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid argument">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1003,
            name = <<"Request refused - invalid argument name">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid argument name">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1004,
            name = <<"Request refused - invalid argument value">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid argument value">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1005,
            name = <<"Request refused - invalid argument type">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid argument type">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1006,
            name = <<"Request refused - missing required argument">>,
            category = mcp_protocol,
            description = <<"Request refused due to missing required argument">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1007,
            name = <<"Request refused - extra argument">>,
            category = mcp_protocol,
            description = <<"Request refused due to extra argument not allowed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1008,
            name = <<"Request refused - invalid request">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid request format">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1009,
            name = <<"Request refused - invalid request type">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid request type">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1010,
            name = <<"Request refused - invalid request ID">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid request ID">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1011,
            name = <<"Request refused - invalid method">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid method">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1012,
            name = <<"Request refused - method not found">>,
            category = mcp_protocol,
            description = <<"Request refused because method was not found">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1013,
            name = <<"Request refused - method not supported">>,
            category = mcp_protocol,
            description = <<"Request refused because method is not supported">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1014,
            name = <<"Request refused - invalid capability">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid capability">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1015,
            name = <<"Request refused - capability not supported">>,
            category = mcp_protocol,
            description = <<"Request refused because capability is not supported">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1016,
            name = <<"Request refused - invalid resource">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid resource">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1017,
            name = <<"Request refused - resource not found">>,
            category = mcp_protocol,
            description = <<"Request refused because resource was not found">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1018,
            name = <<"Request refused - invalid tool">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid tool">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1019,
            name = <<"Request refused - tool not found">>,
            category = mcp_protocol,
            description = <<"Request refused because tool was not found">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1020,
            name = <<"Request refused - invalid prompt">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid prompt">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1021,
            name = <<"Request refused - prompt not found">>,
            category = mcp_protocol,
            description = <<"Request refused because prompt was not found">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1022,
            name = <<"Request refused - invalid URI">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1023,
            name = <<"Request refused - invalid URI scheme">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI scheme">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1024,
            name = <<"Request refused - invalid URI authority">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI authority">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1025,
            name = <<"Request refused - invalid URI path">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI path">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1026,
            name = <<"Request refused - invalid URI query">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI query">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1027,
            name = <<"Request refused - invalid URI fragment">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid URI fragment">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1028,
            name = <<"Request refused - invalid JSON">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid JSON">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1029,
            name = <<"Request refused - invalid JSON structure">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid JSON structure">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1030,
            name = <<"Request refused - invalid JSON-RPC">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid JSON-RPC">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1031,
            name = <<"Request refused - invalid JSON-RPC version">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid JSON-RPC version">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1032,
            name = <<"Request refused - invalid JSON-RPC message type">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid JSON-RPC message type">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1033,
            name = <<"Request refused - invalid content">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid content">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1034,
            name = <<"Request refused - invalid content type">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid content type">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1035,
            name = <<"Request refused - invalid content encoding">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid content encoding">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1036,
            name = <<"Request refused - invalid message">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid message">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1037,
            name = <<"Request refused - invalid message type">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid message type">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1038,
            name = <<"Request refused - invalid message ID">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid message ID">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1039,
            name = <<"Request refused - invalid message timestamp">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid message timestamp">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1040,
            name = <<"Request refused - invalid message version">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid message version">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1041,
            name = <<"Request refused - invalid batch">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid batch">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1042,
            name = <<"Request refused - invalid batch size">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid batch size">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1043,
            name = <<"Request refused - invalid batch item">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid batch item">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1044,
            name = <<"Request refused - invalid pagination">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid pagination">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1045,
            name = <<"Request refused - invalid cursor">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid cursor">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1046,
            name = <<"Request refused - invalid limit">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid limit">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1047,
            name = <<"Request refused - invalid offset">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid offset">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1048,
            name = <<"Request refused - invalid sort">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid sort">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1049,
            name = <<"Request refused - invalid filter">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid filter">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1050,
            name = <<"Request refused - invalid authentication">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid authentication">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1051,
            name = <<"Request refused - authentication failed">>,
            category = mcp_protocol,
            description = <<"Request refused because authentication failed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1052,
            name = <<"Request refused - authentication required">>,
            category = mcp_protocol,
            description = <<"Request refused because authentication is required">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1053,
            name = <<"Request refused - invalid authorization">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid authorization">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1054,
            name = <<"Request refused - authorization failed">>,
            category = mcp_protocol,
            description = <<"Request refused because authorization failed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1055,
            name = <<"Request refused - authorization required">>,
            category = mcp_protocol,
            description = <<"Request refused because authorization is required">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1056,
            name = <<"Request refused - invalid token">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid token">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1057,
            name = <<"Request refused - token expired">>,
            category = mcp_protocol,
            description = <<"Request refused because token expired">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1058,
            name = <<"Request refused - token revoked">>,
            category = mcp_protocol,
            description = <<"Request refused because token was revoked">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1059,
            name = <<"Request refused - invalid credentials">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid credentials">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1060,
            name = <<"Request refused - insufficient permissions">>,
            category = mcp_protocol,
            description = <<"Request refused due to insufficient permissions">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1061,
            name = <<"Request refused - access denied">>,
            category = mcp_protocol,
            description = <<"Request refused because access was denied">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1062,
            name = <<"Request refused - quota exceeded">>,
            category = mcp_protocol,
            description = <<"Request refused because quota was exceeded">>,
            severity = warning,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1063,
            name = <<"Request refused - rate limited">>,
            category = mcp_protocol,
            description = <<"Request refused due to rate limiting">>,
            severity = warning,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1064,
            name = <<"Request refused - server overloaded">>,
            category = mcp_protocol,
            description = <<"Request refused because server is overloaded">>,
            severity = warning,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1065,
            name = <<"Request refused - server unavailable">>,
            category = mcp_protocol,
            description = <<"Request refused because server is unavailable">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1066,
            name = <<"Request refused - server error">>,
            category = mcp_protocol,
            description = <<"Request refused due to server error">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1067,
            name = <<"Request refused - network error">>,
            category = mcp_protocol,
            description = <<"Request refused due to network error">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1068,
            name = <<"Request refused - timeout">>,
            category = mcp_protocol,
            description = <<"Request refused due to timeout">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1069,
            name = <<"Request refused - connection lost">>,
            category = mcp_protocol,
            description = <<"Request refused because connection was lost">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1070,
            name = <<"Request refused - connection closed">>,
            category = mcp_protocol,
            description = <<"Request refused because connection was closed">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1071,
            name = <<"Request refused - connection refused">>,
            category = mcp_protocol,
            description = <<"Request refused because connection was refused">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1072,
            name = <<"Request refused - invalid state">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid state">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1073,
            name = <<"Request refused - state mismatch">>,
            category = mcp_protocol,
            description = <<"Request refused due to state mismatch">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1074,
            name = <<"Request refused - not initialized">>,
            category = mcp_protocol,
            description = <<"Request refused because not initialized">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1075,
            name = <<"Request refused - already initialized">>,
            category = mcp_protocol,
            description = <<"Request refused because already initialized">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1076,
            name = <<"Request refused - shutting down">>,
            category = mcp_protocol,
            description = <<"Request refused because server is shutting down">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1077,
            name = <<"Request refused - operation not supported">>,
            category = mcp_protocol,
            description = <<"Request refused because operation is not supported">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1078,
            name = <<"Request refused - operation cancelled">>,
            category = mcp_protocol,
            description = <<"Request refused because operation was cancelled">>,
            severity = warning,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1079,
            name = <<"Request refused - operation timeout">>,
            category = mcp_protocol,
            description = <<"Request refused because operation timed out">>,
            severity = error,
            retry_strategy = retry
        },
        #error_code_req{
            code = 1080,
            name = <<"Request refused - operation failed">>,
            category = mcp_protocol,
            description = <<"Request refused because operation failed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1081,
            name = <<"Request refused - validation failed">>,
            category = mcp_protocol,
            description = <<"Request refused because validation failed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1082,
            name = <<"Request refused - schema validation failed">>,
            category = mcp_protocol,
            description = <<"Request refused because schema validation failed">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1083,
            name = <<"Request refused - constraint violation">>,
            category = mcp_protocol,
            description = <<"Request refused due to constraint violation">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1084,
            name = <<"Request refused - data too large">>,
            category = mcp_protocol,
            description = <<"Request refused because data is too large">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1085,
            name = <<"Request refused - data too small">>,
            category = mcp_protocol,
            description = <<"Request refused because data is too small">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1086,
            name = <<"Request refused - invalid data format">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid data format">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1087,
            name = <<"Request refused - invalid data encoding">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid data encoding">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1088,
            name = <<"Request refused - invalid data compression">>,
            category = mcp_protocol,
            description = <<"Request refused due to invalid data compression">>,
            severity = error,
            retry_strategy = abort
        },
        #error_code_req{
            code = 1089,
            name = <<"Request refused - rate limited">>,
            category = mcp_protocol,
            description = <<"Request refused due to rate limiting">>,
            severity = warning,
            retry_strategy = retry
        }
    ].

%% @doc Build all transport requirements (hardcoded MCP 2025-11-25 spec)
build_transports() ->
    [
        #transport_req{
            name = <<"stdio">>,
            transport_type = stream_based,
            framing = json_delimiter,
            connection_oriented = true,
            multiplexing_support = false,
            required_features = [<<"newline_delimiter">>],
            optional_features = []
        },
        #transport_req{
            name = <<"stdio-transport">>,
            transport_type = stream_based,
            framing = json_delimiter,
            connection_oriented = true,
            multiplexing_support = false,
            required_features = [<<"newline_delimiter">>],
            optional_features = [<<"compression">>]
        },
        #transport_req{
            name = <<"sse">>,
            transport_type = stream_based,
            framing = json_delimiter,
            connection_oriented = true,
            multiplexing_support = true,
            required_features = [<<"http_sse">>],
            optional_features = [<<"compression">>, <<"retry">>]
        }
    ].

%% @doc Build all capability requirements (hardcoded MCP 2025-11-25 spec)
build_capabilities() ->
    [
        #capability_req{
            name = <<"resources">>,
            category = server,
            required = false,
            dependencies = [],
            features = [<<"subscribe">>, <<"list">>, <<"read">>],
            validation_rules = [
                <<"resources/list must return array">>,
                <<"resources/read must return contents array">>,
                <<"resources/subscribe must be supported if advertised">>
            ]
        },
        #capability_req{
            name = <<"tools">>,
            category = server,
            required = false,
            dependencies = [],
            features = [<<"list">>, <<"call">>],
            validation_rules = [
                <<"tools/list must return array">>,
                <<"tools/call must accept name and arguments">>,
                <<"tools/call must return content array">>
            ]
        },
        #capability_req{
            name = <<"prompts">>,
            category = server,
            required = false,
            dependencies = [],
            features = [<<"list">>, <<"get">>],
            validation_rules = [
                <<"prompts/list must return array">>,
                <<"prompts/get must return messages array">>
            ]
        },
        #capability_req{
            name = <<"logging">>,
            category = server,
            required = false,
            dependencies = [],
            features = [<<"level">>],
            validation_rules = [
                <<"logging/set must accept level parameter">>
            ]
        }
    ].

%% @doc Build all validation rules
build_validation_rules() ->
    [
        #validation_rule{
            rule_id = <<"jsonrpc_version_required">>,
            severity = error,
            category = protocol,
            check_function = validate_jsonrpc_version,
            error_message = <<"JSON-RPC version must be '2.0'">>,
            remediation = <<"Add jsonrpc: '2.0' to message">>
        },
        #validation_rule{
            rule_id = <<"method_name_required">>,
            severity = error,
            category = protocol,
            check_function = validate_method_name,
            error_message = <<"Method name is required">>,
            remediation = <<"Add method field to message">>
        },
        #validation_rule{
            rule_id = <<"id_required_for_request">>,
            severity = error,
            category = protocol,
            check_function = validate_request_id,
            error_message = <<"Request must have an id">>,
            remediation = <<"Add id field to request">>
        },
        #validation_rule{
            rule_id = <<"initialize_must_be_first">>,
            severity = error,
            category = protocol,
            check_function = validate_initialize_first,
            error_message = <<"initialize must be the first request">>,
            remediation = <<"Send initialize request before any other requests">>
        }
    ].

%%%====================================================================
%%% Internal functions - Validation
%%%====================================================================

%% @doc Validate JSON-RPC message structure
do_validate_message(Message) ->
    case {maps:is_key(jsonrpc, Message), maps:is_key(method, Message)} of
        {true, true} ->
            JsonRpcVersion = maps:get(jsonrpc, Message),
            case JsonRpcVersion of
                <<"2.0">> ->
                    Method = maps:get(method, Message),
                    case is_binary(Method) andalso byte_size(Method) > 0 of
                        true -> {ok, valid_message};
                        false -> {error, invalid_method_name}
                    end;
                _ -> {error, invalid_jsonrpc_version}
            end;
        _ -> {error, missing_required_fields}
    end.

%% @doc Validate method call with parameters
do_validate_method_call(MethodName, Params) ->
    Methods = build_methods(),
    case lists:keyfind(MethodName, #method_req.name, Methods) of
        false -> {error, method_not_found};
        MethodReq ->
            ParamSpec = MethodReq#method_req.params_spec,
            validate_params_against_spec(Params, ParamSpec)
    end.

%% @doc Validate parameters against specification
validate_params_against_spec(Params, Spec) when is_map(Spec) ->
    RequiredParams = [K || {K, V} <- maps:to_list(Spec),
                           is_map(V) andalso maps:get(required, V, false) =:= true],
    MissingParams = [P || P <- RequiredParams, not maps:is_key(P, Params)],
    case MissingParams of
        [] -> {ok, valid_params};
        _ -> {error, {missing_params, MissingParams}}
    end;
validate_params_against_spec(_, _) ->
    {ok, valid_params}.

%% @doc Validate error code
do_validate_error_code(ErrorCode) ->
    Errors = build_error_codes(),
    case lists:keyfind(ErrorCode, #error_code_req.code, Errors) of
        false -> {error, unknown_error_code};
        _ErrorReq -> {ok, valid_error_code}
    end.

%% @doc Check capability support
do_check_capability_support(CapabilityName, Features) ->
    Capabilities = build_capabilities(),
    case lists:keyfind(CapabilityName, #capability_req.name, Capabilities) of
        false -> {error, unknown_capability};
        CapabilityReq ->
            RequiredFeatures = CapabilityReq#capability_req.features,
            case lists:sort(RequiredFeatures) =< lists:sort(Features) of
                true -> {ok, capability_supported};
                false -> {error, {missing_features, RequiredFeatures -- Features}}
            end
    end.

%%%====================================================================
%%% Helper Functions - Spec Metadata
%%%====================================================================

%% @doc Get the MCP specification version
-spec spec_version() -> binary().
spec_version() ->
    ?MCP_2025_11_25_VERSION.

%% @doc Check if an error code is valid according to MCP 2025-11-25 spec
-spec is_valid_error_code(integer()) -> boolean().
is_valid_error_code(ErrorCode) ->
    case do_validate_error_code(ErrorCode) of
        {ok, valid_error_code} -> true;
        _ -> false
    end.

%% @doc Check if a request type is valid
-spec is_valid_request_type(binary()) -> boolean().
is_valid_request_type(RequestType) ->
    Requests = get_all_request_types(),
    lists:member(RequestType, Requests).

%% @doc Check if a notification type is valid
-spec is_valid_notification_type(binary()) -> boolean().
is_valid_notification_type(NotificationType) ->
    Notifications = get_all_notification_types(),
    lists:member(NotificationType, Notifications).

%% @doc Get required capabilities for an operation
-spec get_required_capabilities(binary()) -> [binary()] | undefined.
get_required_capabilities(Operation) ->
    Methods = build_methods(),
    case lists:keyfind(Operation, #method_req.name, Methods) of
        false -> undefined;
        MethodReq ->
            Cap = MethodReq#method_req.capability_required,
            case Cap of
                undefined -> [];
                _ -> [Cap]
            end
    end.

%% @doc Get all error codes as a list
-spec get_all_error_codes() -> [integer()].
get_all_error_codes() ->
    Errors = build_error_codes(),
    [E#error_code_req.code || E <- Errors].

%% @doc Get all request types
-spec get_all_request_types() -> [binary()].
get_all_request_types() ->
    Methods = build_methods(),
    [M#method_req.name || M <- Methods, M#method_req.method_type =:= request].

%% @doc Get all notification types
-spec get_all_notification_types() -> [binary()].
get_all_notification_types() ->
    Methods = build_methods(),
    [M#method_req.name || M <- Methods, M#method_req.method_type =:= notification].

%% @doc Validate URI format according to RFC 3986
-spec is_valid_uri(binary() | string()) -> boolean().
is_valid_uri(Uri) when is_binary(Uri) ->
    is_valid_uri(binary_to_list(Uri));
is_valid_uri(Uri) when is_list(Uri) ->
    try
        % Basic URI validation: scheme:[//authority]path[?query][#fragment]
        case re:run(Uri, "^[a-zA-Z][a-zA-Z0-9+\\.-]*:") of
            {match, _} ->
                % Has valid scheme, check rest
                case re:run(Uri, "^[a-zA-Z][a-zA-Z0-9+\\.-]*:(//[^/?#]*)?([^?#]*)(\\?[^#]*)?(#.*)?$") of
                    {match, _} -> true;
                    nomatch -> false
                end;
            nomatch ->
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Validate request ID format (can be string, number, or null)
-spec is_valid_request_id(any()) -> boolean().
is_valid_request_id(RequestId) when is_binary(RequestId) -> true;
is_valid_request_id(RequestId) when is_integer(RequestId) -> true;
is_valid_request_id(RequestId) when is_float(RequestId) -> false; % JSON-RPC doesn't allow floats
is_valid_request_id(null) -> true;
is_valid_request_id(_) -> false.

%% @doc Get method parameter specification
-spec get_method_params(binary()) -> map() | undefined.
get_method_params(MethodName) ->
    Methods = build_methods(),
    case lists:keyfind(MethodName, #method_req.name, Methods) of
        false -> undefined;
        MethodReq -> MethodReq#method_req.params_spec
    end.

%% @doc Get method result specification
-spec get_method_result(binary()) -> map() | undefined.
get_method_result(MethodName) ->
    Methods = build_methods(),
    case lists:keyfind(MethodName, #method_req.name, Methods) of
        false -> undefined;
        MethodReq -> MethodReq#method_req.result_spec
    end.

%% @doc Get capability features
-spec get_capability_features(binary()) -> [binary()] | undefined.
get_capability_features(CapabilityName) ->
    Capabilities = build_capabilities(),
    case lists:keyfind(CapabilityName, #capability_req.name, Capabilities) of
        false -> undefined;
        CapabilityReq -> CapabilityReq#capability_req.features
    end.

%% @doc Get version compatibility information
-spec get_version_compatibility(binary()) -> compatible | deprecated | incompatible | unknown.
get_version_compatibility(Version) ->
    case Version of
        ?MCP_2025_11_25_VERSION -> compatible;
        <<"2024-11-05">> -> deprecated;
        <<"2025-01-23">> -> compatible; % Future version example
        _ -> incompatible
    end.

%% @doc Check if a method is deprecated
-spec is_deprecated_method(binary()) -> boolean().
is_deprecated_method(MethodName) ->
    Methods = build_methods(),
    case lists:keyfind(MethodName, #method_req.name, Methods) of
        false -> false;
        MethodReq ->
            case MethodReq#method_req.deprecation_status of
                deprecated -> true;
                _ -> false
            end
    end.
