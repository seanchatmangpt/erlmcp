-module(erlmcp_nominal_types_test).

%% Test module to verify Dialyzer enforces nominal types correctly
%% This module intentionally contains type errors that Dialyzer SHOULD catch

%% Nominal types are now referenced with module qualification

%% Test functions that intentionally misuse nominal types
%% Dialyzer SHOULD report warnings for these

%% @doc WRONG: Using erlmcp_mcp_types:mcp_tool_name() where erlmcp_mcp_types:mcp_request_id() is expected
%% Dialyzer should catch this type confusion
-spec bad_use_tool_as_id(erlmcp_mcp_types:mcp_tool_name()) -> erlmcp_mcp_types:mcp_request_id().
bad_use_tool_as_id(ToolName) ->
    %% TYPE ERROR: ToolName is erlmcp_mcp_types:mcp_tool_name(), but we're treating it as erlmcp_mcp_types:mcp_request_id()
    %% This should be caught by Dialyzer if nominal types work
    ToolName.

%% @doc WRONG: Using erlmcp_mcp_types:mcp_request_id() where erlmcp_mcp_types:mcp_tool_name() is expected
-spec bad_use_id_as_tool(erlmcp_mcp_types:mcp_request_id()) -> erlmcp_mcp_types:mcp_tool_name().
bad_use_id_as_tool(RequestId) ->
    %% TYPE ERROR: RequestId is erlmcp_mcp_types:mcp_request_id(), but we're treating it as erlmcp_mcp_types:mcp_tool_name()
    RequestId.

%% @doc WRONG: Swapping arguments - tool vs resource URI
-spec bad_swap_tool_and_resource(erlmcp_mcp_types:mcp_tool_name(), erlmcp_mcp_types:mcp_resource_uri()) -> {erlmcp_mcp_types:mcp_resource_uri(), erlmcp_mcp_types:mcp_tool_name()}.
bad_swap_tool_and_resource(ToolName, ResourceUri) ->
    %% TYPE ERROR: Arguments swapped in return
    {ToolName, ResourceUri}.

%% @doc A function that demonstrates correct type usage
-spec correct_type_usage(erlmcp_mcp_types:mcp_tool_name()) -> erlmcp_mcp_types:mcp_tool_name().
correct_type_usage(ToolName) ->
    %% CORRECT: Input and output both erlmcp_mcp_types:mcp_tool_name()
    ToolName.

%% @doc Function with mixed nominal types - Dialyzer should track each correctly
-spec process_types(erlmcp_mcp_types:mcp_request_id(), erlmcp_mcp_types:mcp_tool_name(), erlmcp_mcp_types:mcp_resource_uri()) ->
    {erlmcp_mcp_types:mcp_request_id(), erlmcp_mcp_types:mcp_tool_name(), erlmcp_mcp_types:mcp_resource_uri()}.
process_types(RequestId, ToolName, ResourceUri) ->
    %% CORRECT: Return values in same order as input
    {RequestId, ToolName, ResourceUri}.

%% @doc Function that tries to pass nominal type to a function expecting binary()
%% Since all nominal types are binary(), this may NOT be caught by Dialyzer
-spec nominal_to_binary(erlmcp_mcp_types:mcp_tool_name()) -> binary().
nominal_to_binary(ToolName) ->
    %% This is OK because erlmcp_mcp_types:mcp_tool_name() is a binary()
    ToolName.

%% @doc Function that tries to pass binary() where nominal type expected
%% Dialyzer may NOT catch this because nominal types are structural (binary())
-spec binary_to_nominal(binary()) -> erlmcp_mcp_types:mcp_tool_name().
binary_to_nominal(Bin) ->
    %% Dialyzer may NOT catch this because any binary() could be a tool name
    Bin.
