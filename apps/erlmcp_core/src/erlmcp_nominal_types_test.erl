-module(erlmcp_nominal_types_test).

%% Test module to verify Dialyzer enforces nominal types correctly
%% This module intentionally contains type errors that Dialyzer SHOULD catch

%% Import nominal types
-import(erlmcp_mcp_types, [
    mcp_request_id/0,
    mcp_tool_name/0,
    mcp_resource_uri/0,
    mcp_prompt_name/0
]).

%% Test functions that intentionally misuse nominal types
%% Dialyzer SHOULD report warnings for these

%% @doc WRONG: Using mcp_tool_name() where mcp_request_id() is expected
%% Dialyzer should catch this type confusion
-spec bad_use_tool_as_id(mcp_tool_name()) -> mcp_request_id().
bad_use_tool_as_id(ToolName) ->
    %% TYPE ERROR: ToolName is mcp_tool_name(), but we're treating it as mcp_request_id()
    %% This should be caught by Dialyzer if nominal types work
    ToolName.

%% @doc WRONG: Using mcp_request_id() where mcp_tool_name() is expected
-spec bad_use_id_as_tool(mcp_request_id()) -> mcp_tool_name().
bad_use_id_as_tool(RequestId) ->
    %% TYPE ERROR: RequestId is mcp_request_id(), but we're treating it as mcp_tool_name()
    RequestId.

%% @doc WRONG: Swapping arguments - tool vs resource URI
-spec bad_swap_tool_and_resource(mcp_tool_name(), mcp_resource_uri()) -> {mcp_resource_uri(), mcp_tool_name()}.
bad_swap_tool_and_resource(ToolName, ResourceUri) ->
    %% TYPE ERROR: Arguments swapped in return
    {ToolName, ResourceUri}.

%% @doc A function that demonstrates correct type usage
-spec correct_type_usage(mcp_tool_name()) -> mcp_tool_name().
correct_type_usage(ToolName) ->
    %% CORRECT: Input and output both mcp_tool_name()
    ToolName.

%% @doc Function with mixed nominal types - Dialyzer should track each correctly
-spec process_types(mcp_request_id(), mcp_tool_name(), mcp_resource_uri()) ->
    {mcp_request_id(), mcp_tool_name(), mcp_resource_uri()}.
process_types(RequestId, ToolName, ResourceUri) ->
    %% CORRECT: Return values in same order as input
    {RequestId, ToolName, ResourceUri}.

%% @doc Function that tries to pass nominal type to a function expecting binary()
%% Since all nominal types are binary(), this may NOT be caught by Dialyzer
-spec nominal_to_binary(mcp_tool_name()) -> binary().
nominal_to_binary(ToolName) ->
    %% This is OK because mcp_tool_name() is a binary()
    ToolName.

%% @doc Function that tries to pass binary() where nominal type expected
%% Dialyzer may NOT catch this because nominal types are structural (binary())
-spec binary_to_nominal(binary()) -> mcp_tool_name().
binary_to_nominal(Bin) ->
    %% Dialyzer may NOT catch this because any binary() could be a tool name
    Bin.
