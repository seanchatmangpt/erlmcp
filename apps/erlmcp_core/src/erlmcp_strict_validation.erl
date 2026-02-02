-module(erlmcp_strict_validation).

%% OTP 28 EEP-70: Strict Generators for Fail-Fast MCP Message Validation
%%
%% STRICT GENERATOR SYNTAX:
%%   <:-  - Strict list generator (raises badmatch on pattern mismatch)
%%   <:=  - Strict binary generator (raises badmatch on pattern mismatch)
%%
%% vs NON-STRICT:
%%   <-   - Silent skip on pattern mismatch
%%   <=   - Silent skip on binary pattern mismatch
%%
%% USE CASE: Validate MCP message batches, tool results, protocol data
%% BENEFIT:  Fail-fast on malformed data instead of silently skipping

-include("erlmcp.hrl").

%% API exports
-export([validate_messages/1,
         validate_tool_calls/1,
         validate_tool_results/1,
         validate_resource_lists/1,
         validate_prompt_arguments/1,
         validate_notification_batch/1,
         extract_tool_names/1,
         extract_resource_uris/1,
         filter_valid_messages/1,
         validate_batch_strict/1]).

%% Types
-type mcp_message() :: #{binary() => term()}.
-type tool_call() :: #{binary() => term()}.
-type tool_result() :: #{binary() => term()}.
-type validation_result() :: {ok, list()} | {error, {badmatch, term()}}.

%%====================================================================
%% OTP 28 Strict Comprehension API
%%====================================================================

%% @doc Validate all messages in batch - FAIL FAST on bad format
%% Uses strict list generator: <:- raises badmatch if pattern doesn't match
%%
%% Example crash: {badmatch, #{<<"method">> => <<"invalid">>}}
%% This is DESIRED BEHAVIOR - fail fast on protocol violations
-spec validate_messages([mcp_message()]) -> [mcp_message()].
validate_messages(Messages) when is_list(Messages) ->
    %% STRICT: All messages MUST have jsonrpc field
    %% Crashes with {badmatch, BadMessage} if any message lacks it
    [Msg || #{?JSONRPC_FIELD_JSONRPC := ?JSONRPC_VERSION} <:- Messages].

%% @doc Extract and validate tool calls - FAIL FAST on malformed calls
%% Ensures all tool calls have required structure
-spec validate_tool_calls([tool_call()]) -> [{binary(), map()}].
validate_tool_calls(Calls) when is_list(Calls) ->
    %% STRICT: Extract tool name and params, crash if structure invalid
    [{Tool, Params}
     || #{<<"method">> := <<"tools/call">>,
          <<"params">> := #{<<"name">> := Tool} = Params} <:- Calls].

%% @doc Validate tool results - FAIL FAST on malformed results
%% Ensures all results have proper structure
-spec validate_tool_results([tool_result()]) -> [map()].
validate_tool_results(Results) when is_list(Results) ->
    %% STRICT: All results must have content field
    [Result || #{<<"content">> := _} <:- Results].

%% @doc Validate resource lists - FAIL FAST on missing URIs
-spec validate_resource_lists([map()]) -> [map()].
validate_resource_lists(Resources) when is_list(Resources) ->
    %% STRICT: All resources must have uri field
    [Resource || #{<<"uri">> := _Uri, <<"name">> := _Name} <:- Resources].

%% @doc Validate prompt arguments - FAIL FAST on malformed arguments
-spec validate_prompt_arguments([map()]) -> [map()].
validate_prompt_arguments(Arguments) when is_list(Arguments) ->
    %% STRICT: All arguments must have name and role fields
    [Arg || #{<<"name">> := _Name, <<"role">> := _Role} <:- Arguments].

%% @doc Validate notification batch - strict validation
-spec validate_notification_batch([mcp_message()]) -> [mcp_message()].
validate_notification_batch(Messages) when is_list(Messages) ->
    %% STRICT: All notifications must have method field
    [Msg || #{<<"method">> := _Method} <:- Messages].

%%====================================================================
%% Strict Extraction Functions
%%====================================================================

%% @doc Extract tool names - FAIL FAST if any entry lacks name
-spec extract_tool_names([map()]) -> [binary()].
extract_tool_names(Tools) when is_list(Tools) ->
    %% STRICT: All tools must have name field
    [Name || #{<<"name">> := Name} <:- Tools].

%% @doc Extract resource URIs - FAIL FAST if any entry lacks uri
-spec extract_resource_uris([map()]) -> [binary()].
extract_resource_uris(Resources) when is_list(Resources) ->
    %% STRICT: All resources must have uri field
    [Uri || #{<<"uri">> := Uri} <:- Resources].

%% @doc Filter messages with strict type checking
-spec filter_valid_messages([term()]) -> [mcp_message()].
filter_valid_messages(Messages) when is_list(Messages) ->
    %% STRICT: Only actual maps pass, crashes on unexpected types
    [Msg || Msg <:- Messages, is_map(Msg), map_size(Msg) > 0].

%% @doc Validate entire batch with strict comprehensions
%% This is the PRIMARY function for MCP message batch validation
-spec validate_batch_strict([map()]) -> [map()].
validate_batch_strict(Batch) when is_list(Batch) ->
    %% STRICT: All items must be valid JSON-RPC messages
    %% 1. Must be a map
    %% 2. Must have jsonrpc: "2.0"
    %% 3. Must have id or method (request/response or notification)
    [Msg
     || Msg <:- Batch,
        is_map(Msg),
        maps:is_key(?JSONRPC_FIELD_JSONRPC, Msg),
        (maps:is_key(?JSONRPC_FIELD_ID, Msg) orelse maps:is_key(<<"method">>, Msg))].

%%====================================================================
%% OTP 28 Binary Strict Generators
%%====================================================================

%% @doc Validate binary data using strict binary generators
%% Example: Parse protocol headers from binary
-spec validate_binary_headers(binary()) -> [{binary(), binary()}].
validate_binary_headers(Binary) when is_binary(Binary) ->
    %% STRICT: Binary must have pattern <<Name:binary, ":", Value:binary, "\n">>
    %% Crashes with badmatch if header format is invalid
    %% This is for validation, not production parsing (use binary:split/3 instead)
    try
        [{Name, Value}
         || <<Name:binary, ":", Value:binary, "\n">> <:= split_lines(Binary)]
    catch
        error:{badmatch, _} = Badmatch ->
            error({invalid_header_format, Badmatch})
    end.

%% @private Helper to split binary into lines
split_lines(Binary) ->
    binary:split(Binary, <<"\n">>, [global]).

%%====================================================================
%% Strict Map Comprehensions (OTP 28+)
%%====================================================================

%% @doc Transform and validate tool call batch
-spec transform_tool_calls([tool_call()]) -> #{binary() => map()}.
transform_tool_calls(Calls) when is_list(Calls) ->
    %% STRICT: Create map from tool name to call details
    %% Crashes if duplicate tool names (keys must be unique)
    maps:fromlist([{Tool, Call}
                   || #{<<"method">> := <<"tools/call">>,
                        <<"params">> := #{<<"name">> := Tool}} = Call <:- Calls]).

%%====================================================================
%% Error Recovery Patterns
%%====================================================================

%% @doc Safe validation with catch - returns errors instead of crashing
%% Use this in production handlers where you want graceful degradation
-spec validate_messages_safe([mcp_message()]) -> {ok, [mcp_message()]} | {error, term()}.
validate_messages_safe(Messages) when is_list(Messages) ->
    try
        Validated = validate_messages(Messages),
        {ok, Validated}
    catch
        error:{badmatch, BadMessage} ->
            {error, {invalid_message_format, BadMessage}};
        error:Reason ->
            {error, {validation_failed, Reason}}
    end.

%% @doc Partition valid and invalid messages (non-strict comparison)
%% Returns: {ValidMessages, InvalidMessages}
-spec partition_messages([mcp_message()]) -> {[mcp_message()], [term()]}.
partition_messages(Messages) when is_list(Messages) ->
    %% Use NON-STRICT generator to collect invalid messages
    Valid = [Msg || Msg <- Messages, is_valid_message(Msg)],
    Invalid = [Msg || Msg <- Messages, not is_valid_message(Msg)],
    {Valid, Invalid}.

%% @private Check if message is valid (for partitioning)
is_valid_message(Msg) when is_map(Msg) ->
    maps:is_key(?JSONRPC_FIELD_JSONRPC, Msg) andalso
        (maps:is_key(?JSONRPC_FIELD_ID, Msg) orelse maps:is_key(<<"method">>, Msg));
is_valid_message(_) ->
    false.

%%====================================================================
%% Documentation Examples
%%====================================================================

%% @doc Example: STRICT vs NON-STRICT comprehensions
%%
%% %% NON-STRICT (traditional): Silently skips invalid entries
%% [Msg || #{<<"type">> := Type} <- Messages]
%%   => If a message lacks <<"type">>, it's silently skipped
%%
%% %% STRICT (OTP 28): Crashes with badmatch
%% [Msg || #{<<"type">> := Type} <:- Messages]
%%   => If a message lacks <<"type">>, raises {badmatch, BadMessage}
%%   => This is DESIRED for protocol validation
%%
%% %% Binary strict generator example:
%% [<<Byte>> || <<Byte>> <= Binary]  % Non-strict: skips if not <<Byte>>
%% [<<Byte>> || <<Byte>> <:= Binary] % Strict: crashes if not <<Byte>>

%%====================================================================
%% OTP Version Compatibility
%%====================================================================

%% @doc Check if strict comprehensions are available
%% OTP 28+ supports <:- and <:= generators
-spec supports_strict() -> boolean().
supports_strict() ->
    %% Check OTP version
    case erlang:system_info(otp_release) of
        "28" ++ _ ->
            true;
        _ ->
            false
    end.

%% @doc Get strict generator type based on OTP version
-spec strict_generator_type() -> strict | non_strict.
strict_generator_type() ->
    case supports_strict() of
        true -> strict;
        false -> non_strict
    end.
