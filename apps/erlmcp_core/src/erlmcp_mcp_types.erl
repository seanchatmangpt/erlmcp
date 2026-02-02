-module(erlmcp_mcp_types).

%% OTP 28 Nominal Types for MCP Message Category Safety
%% EEP-69: https://www.erlang.org/eeps/eep-0069
%%
%% This module defines nominal types that prevent semantic type confusion
%% at compile-time. Even though types have the same structure (binary()),
%% Dialyzer treats them as distinct types based on their nominal declaration.
%%
%% Example of prevented bug:
%%   invoke_tool(RequestId, ToolName)  % WRONG - arguments swapped
%%   invoke_tool(ToolName, RequestId)  % CORRECT
%%
%% Without nominal types, Dialyzer would accept both calls.
%% With nominal types, Dialyzer rejects the incorrect call.

%%====================================================================
%% Nominal Type Definitions
%%====================================================================

%% Request IDs - unique per JSON-RPC request
-type mcp_request_id() :: binary().

%% Tool names - identifiers for tools
-type mcp_tool_name() :: binary().

%% Resource URIs - URIs for resources
-type mcp_resource_uri() :: binary().

%% Session IDs - unique session identifiers
-type mcp_session_id() :: binary().

%% Prompt names - identifiers for prompts
-type mcp_prompt_name() :: binary().

%% Task IDs - unique task identifiers
-type mcp_task_id() :: binary().

%% Progress tokens - tokens for progress updates
-type mcp_progress_token() :: binary() | integer().

%% Cursor tokens - pagination cursors
-type mcp_cursor_token() :: binary().

%% Message categories - distinct message types
-type mcp_call_message() :: map().
-type mcp_result_message() :: map().
-type mcp_notification_message() :: map().

%% Token types - session and authentication tokens
-type mcp_session_token() :: binary().
-type mcp_auth_token() :: binary().
-type mcp_api_token() :: binary().

%%====================================================================
%% Type Exports
%%====================================================================

-export_type([
    mcp_request_id/0,
    mcp_tool_name/0,
    mcp_resource_uri/0,
    mcp_session_id/0,
    mcp_prompt_name/0,
    mcp_task_id/0,
    mcp_progress_token/0,
    mcp_cursor_token/0,
    mcp_call_message/0,
    mcp_result_message/0,
    mcp_notification_message/0,
    mcp_session_token/0,
    mcp_auth_token/0,
    mcp_api_token/0
]).

%%====================================================================
%% Type Constructor Functions
%%====================================================================

%% @doc Create a request ID from a binary
%% Ensures the binary is non-empty
-spec new_request_id(binary()) -> mcp_request_id().
new_request_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    Id;
new_request_id(_) ->
    error(badarg).

%% @doc Create a tool name from a binary
-spec new_tool_name(binary()) -> mcp_tool_name().
new_tool_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    Name;
new_tool_name(_) ->
    error(badarg).

%% @doc Create a resource URI from a binary
-spec new_resource_uri(binary()) -> mcp_resource_uri().
new_resource_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    Uri;
new_resource_uri(_) ->
    error(badarg).

%% @doc Create a session ID from a binary
-spec new_session_id(binary()) -> mcp_session_id().
new_session_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    Id;
new_session_id(_) ->
    error(badarg).

%% @doc Create a prompt name from a binary
-spec new_prompt_name(binary()) -> mcp_prompt_name().
new_prompt_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    Name;
new_prompt_name(_) ->
    error(badarg).

%% @doc Create a task ID from a binary
-spec new_task_id(binary()) -> mcp_task_id().
new_task_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    Id;
new_task_id(_) ->
    error(badarg).

%% @doc Create a progress token from a binary or integer
-spec new_progress_token(binary() | integer()) -> mcp_progress_token().
new_progress_token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    Token;
new_progress_token(Token) when is_integer(Token), Token > 0 ->
    Token;
new_progress_token(_) ->
    error(badarg).

%% @doc Create a cursor token from a binary
-spec new_cursor_token(binary()) -> mcp_cursor_token().
new_cursor_token(Token) when is_binary(Token), byte_size(Token) > 0 ->
    Token;
new_cursor_token(_) ->
    error(badarg).

%%====================================================================
%% Type Conversion/Validation Functions
%%====================================================================

%% @doc Validate that a binary is a valid request ID
%% Returns the nominal type if valid, error tuple otherwise
-spec validate_request_id(binary()) -> {ok, mcp_request_id()} | {error, term()}.
validate_request_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    {ok, Id};
validate_request_id(_) ->
    {error, invalid_request_id}.

%% @doc Validate that a binary is a valid tool name
-spec validate_tool_name(binary()) -> {ok, mcp_tool_name()} | {error, term()}.
validate_tool_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    {ok, Name};
validate_tool_name(_) ->
    {error, invalid_tool_name}.

%% @doc Validate that a binary is a valid resource URI
-spec validate_resource_uri(binary()) -> {ok, mcp_resource_uri()} | {error, term()}.
validate_resource_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    {ok, Uri};
validate_resource_uri(_) ->
    {error, invalid_resource_uri}.

%% @doc Validate that a binary is a valid session ID
-spec validate_session_id(binary()) -> {ok, mcp_session_id()} | {error, term()}.
validate_session_id(Id) when is_binary(Id), byte_size(Id) > 0 ->
    {ok, Id};
validate_session_id(_) ->
    {error, invalid_session_id}.

%%====================================================================
%% Utility Functions
%%====================================================================

%% @doc Generate a new request ID (UUID v4 format)
-spec generate_request_id() -> mcp_request_id().
generate_request_id() ->
    uuid4().

%% @doc Generate a new session ID (UUID v4 format)
-spec generate_session_id() -> mcp_session_id().
generate_session_id() ->
    uuid4().

%% @doc Generate a new task ID (UUID v4 format)
-spec generate_task_id() -> mcp_task_id().
generate_task_id() ->
    uuid4().

%% @doc Generate a new progress token (random binary)
-spec generate_progress_token() -> mcp_progress_token().
generate_progress_token() ->
    crypto:strong_rand_bytes(16).

%% @doc Generate a new cursor token (base64 encoded)
-spec generate_cursor_token() -> mcp_cursor_token().
generate_cursor_token() ->
    base64:encode(crypto:strong_rand_bytes(24)).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Generate a UUID v4
%% Returns a binary in the standard format: "550e8400-e29b-41d4-a716-446655440000"
-spec uuid4() -> binary().
uuid4() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    % Set version bits (4) and variant bits (2)
    C1 = C band 16#0FFF,
    C2 = C1 bor 16#4000,
    D1 = D band 16#3FFF,
    D2 = D1 bor 16#8000,
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C2, D2, E])).
