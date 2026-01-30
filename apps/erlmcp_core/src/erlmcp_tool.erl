-module(erlmcp_tool).

-include("erlmcp.hrl").

%% API exports
-export([
    validate_tool/1,
    validate_tool_name/1,
    validate_tool_description/1,
    validate_input_schema/1,
    validate_tool_metadata/1,
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
validate_tool(#mcp_tool{name = Name, description = Desc, metadata = Metadata, version = Version}) ->
    case validate_tool_name(Name) of
        ok ->
            case validate_tool_description(Desc) of
                ok ->
                    case validate_tool_metadata(Metadata) of
                        ok ->
                            validate_tool_version(Version);
                        Error -> Error
                    end;
                Error -> Error
            end;
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

%% @doc Validate tool metadata field (Task #211).
-spec validate_tool_metadata(map() | undefined) -> ok | {error, term()}.
validate_tool_metadata(undefined) ->
    ok;
validate_tool_metadata(Metadata) when is_map(Metadata) ->
    %% Validate metadata structure
    case validate_metadata_fields(Metadata) of
        ok -> ok;
        Error -> Error
    end;
validate_tool_metadata(_) ->
    {error, invalid_metadata}.

%% @doc Validate tool version field (Task #211).
-spec validate_tool_version(binary() | undefined) -> ok | {error, term()}.
validate_tool_version(undefined) ->
    ok;
validate_tool_version(Version) when is_binary(Version) ->
    %% Basic semantic version validation (e.g., "1.0.0", "2.1.3-beta")
    case validate_semver(Version) of
        true -> ok;
        false -> {error, invalid_version_format}
    end;
validate_tool_version(_) ->
    {error, invalid_version}.

-spec encode_tool(#mcp_tool{}) -> map().
encode_tool(#mcp_tool{
    name = Name,
    description = Desc,
    input_schema = Schema,
    metadata = Metadata,
    experimental = Experimental,
    version = Version
}) ->
    Base = #{
        <<"name">> => Name,
        <<"description">> => Desc
    },
    Base1 = case Schema of
        undefined -> Base;
        _ -> Base#{<<"inputSchema">> => Schema}
    end,
    Base2 = case Metadata of
        undefined -> Base1;
        _ -> Base1#{<<"metadata">> => Metadata}
    end,
    Base3 = case Experimental of
        undefined -> Base2;
        _ -> Base2#{<<"experimental">> => Experimental}
    end,
    case Version of
        undefined -> Base3;
        _ -> Base3#{<<"version">> => Version}
    end.

-spec decode_tool(map()) -> #mcp_tool{}.
decode_tool(#{
    <<"name">> := Name,
    <<"description">> := Desc
} = Map) ->
    #mcp_tool{
        name = Name,
        description = Desc,
        input_schema = maps:get(<<"inputSchema">>, Map, undefined),
        metadata = maps:get(<<"metadata">>, Map, undefined),
        experimental = maps:get(<<"experimental">>, Map, undefined),
        version = maps:get(<<"version">>, Map, undefined)
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Validate metadata fields (Task #211).
-spec validate_metadata_fields(map()) -> ok | {error, term()}.
validate_metadata_fields(Metadata) when is_map(Metadata) ->
    %% Validate common metadata keys
    case maps:size(Metadata) > 100 of
        true -> {error, metadata_too_large};
        false -> validate_metadata_values(maps:to_list(Metadata))
    end.

validate_metadata_values([]) -> ok;
validate_metadata_values([{Key, Value} | Rest]) when is_binary(Key) ->
    case validate_metadata_value(Value) of
        ok -> validate_metadata_values(Rest);
        Error -> Error
    end;
validate_metadata_values(_) ->
    {error, invalid_metadata_key_type}.

validate_metadata_value(Value) when is_binary(Value); is_number(Value); is_boolean(Value) -> ok;
validate_metadata_value(Value) when is_list(Value) ->
    case lists:all(fun(V) -> validate_metadata_value(V) =:= ok end, Value) of
        true -> ok;
        false -> {error, invalid_metadata_value}
    end;
validate_metadata_value(Value) when is_map(Value) ->
    case validate_metadata_fields(Value) of
        ok -> ok;
        Error -> Error
    end;
validate_metadata_value(_) ->
    {error, invalid_metadata_value}.

%% @doc Validate semantic version format (Task #211).
-spec validate_semver(binary()) -> boolean().
validate_semver(Version) when is_binary(Version) ->
    %% Basic semver pattern: major.minor.patch(-prerelease)(+build)
    %% Examples: "1.0.0", "2.1.3-beta", "3.0.0-rc.1+build.123"
    case re:run(Version, <<"^\\d+\\.\\d+\\.\\d+(-[0-9A-Za-z.-]+)?(\\+[0-9A-Za-z.-]+)?$">>) of
        {match, _} -> true;
        _ -> false
    end.
