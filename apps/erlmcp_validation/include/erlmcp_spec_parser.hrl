%%%-------------------------------------------------------------------
%%% @doc erlmcp_spec_parser - MCP Specification Parser Records
%%% @end
%%%-------------------------------------------------------------------

%%%====================================================================
%%% Specification Records
%%%====================================================================

-record(mcp_spec,
        {version,
         specification_date,
         protocol_type,
         methods,
         error_codes,
         transports,
         capabilities}).
-record(method_req,
        {name,
         method_type,
         direction,
         required,
         params_spec,
         result_spec,
         capability_required,
         deprecation_status,
         documentation}).
-record(error_code_req,
        {code :: integer(),
         name :: binary(),
         category :: json_rpc | mcp_protocol | transport | validation,
         description :: binary(),
         severity :: error | warning,
         retry_strategy :: retry | abort | fallback}).
-record(transport_req,
        {name :: binary(),
         transport_type :: stream_based | message_based,
         framing :: json_delimiter | length_prefixing | custom,
         connection_oriented :: boolean(),
         multiplexing_support :: boolean(),
         required_features :: [binary()],
         optional_features :: [binary()]}).
-record(capability_req,
        {name :: binary(),
         category :: client | server,
         required :: boolean(),
         dependencies :: [binary()],
         features :: [binary()],
         validation_rules :: [binary()]}).
-record(validation_rule,
        {rule_id :: binary(),
         severity :: error | warning | info,
         category :: protocol | format | capability | transport,
         check_function :: atom(),
         error_message :: binary(),
         remediation :: binary()}).
-record(method_validator,
        {method_name :: binary(),
         required_params :: [binary()],
         optional_params :: [binary()],
         param_types :: #{binary() => binary()},
         param_validation :: #{binary() => map()},
         return_type :: binary() | undefined,
         error_codes :: [integer()],
         preconditions :: [binary()],
         postconditions :: [binary()]}).

%%%====================================================================
%%% Specification Constants
%%%====================================================================

-define(MCP_2025_11_25_VERSION, <<"2025-11-25">>).
-define(JSON_RPC_VERSION, <<"2.0">>).

%%%====================================================================
%%% Exported Types
%%%====================================================================

-type mcp_spec() :: #mcp_spec{}.
-type method_req() :: #method_req{}.
-type error_code_req() :: #error_code_req{}.
-type transport_req() :: #transport_req{}.
-type capability_req() :: #capability_req{}.
-type validation_rule() :: #validation_rule{}.
-type method_validator() :: #method_validator{}.
