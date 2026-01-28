-module(erlmcp_tool).

-include("erlmcp.hrl").

%% API exports
-export([
    validate_tool/1,
    validate_tool_name/1,
    validate_tool_description/1,
    validate_input_schema/1,
    encode_tool/1,
    decode_tool/1
]).

%% Types
-type tool_name() :: binary().

-export_type([tool_name/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec validate_tool(#mcp_tool{}) -> ok | {error, term()}.
validate_tool(#mcp_tool{name = Name, description = Desc}) ->
    case validate_tool_name(Name) of
        ok -> validate_tool_description(Desc);
        Error -> Error
    end.

-spec validate_tool_name(tool_name()) -> ok | {error, term()}.
validate_tool_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    ok;
validate_tool_name(_) ->
    {error, invalid_tool_name}.

-spec validate_tool_description(binary()) -> ok | {error, term()}.
validate_tool_description(Desc) when is_binary(Desc) ->
    %% Check description length (Gap #40: Tool Description Length)
    MaxLen = application:get_env(erlmcp, tool_description_max_length,
                                 ?MCP_TOOL_DESCRIPTION_MAX_LENGTH_DEFAULT),
    case byte_size(Desc) =< MaxLen of
        true -> ok;
        false -> {error, {description_too_long, MaxLen}}
    end;
validate_tool_description(_) ->
    {error, invalid_description}.

-spec validate_input_schema(map() | undefined) -> ok | {error, term()}.
validate_input_schema(undefined) ->
    ok;
validate_input_schema(Schema) when is_map(Schema) ->
    %% Basic schema validation - could be extended
    ok;
validate_input_schema(_) ->
    {error, invalid_input_schema}.

-spec encode_tool(#mcp_tool{}) -> map().
encode_tool(#mcp_tool{
    name = Name,
    description = Desc,
    input_schema = Schema
}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Desc
    },
    case Schema of
        undefined -> Base;
        _ -> Base#{<<"inputSchema">> => Schema}
    end.

-spec decode_tool(map()) -> #mcp_tool{}.
decode_tool(#{
    <<"name">> := Name,
    <<"description">> := Desc
} = Map) ->
    #mcp_tool{
        name = Name,
        description = Desc,
        input_schema = maps:get(<<"inputSchema">>, Map, undefined)
    }.

%%====================================================================
%% Internal Functions
%%====================================================================
