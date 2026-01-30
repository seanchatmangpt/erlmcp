%%%-------------------------------------------------------------------
%%% @doc
%%% Common header file for erlmcp transports
%%% This file defines constants and macros used across all transport modules
%%% @end
%%%-------------------------------------------------------------------

%% JSON-RPC error codes (MCP specific)
-define(MCP_ERROR_MESSAGE_TOO_LARGE, -32012).
-define(MCP_MSG_MESSAGE_TOO_LARGE, <<"Message size exceeds maximum allowed">>).
