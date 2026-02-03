-module(erlmcp_resource).

-include("erlmcp.hrl").

%% API exports
-export([validate_uri/1, validate_resource/1, validate_resource_template/1,
         decode_resource/1]).

%% Types
-type resource_uri() :: binary().

-export_type([resource_uri/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec validate_uri(resource_uri()) -> ok | {error, term()}.
validate_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    ok;
validate_uri(_) ->
    {error, invalid_uri}.

-spec validate_resource(#mcp_resource{}) -> ok | {error, term()}.
validate_resource(#mcp_resource{uri = Uri, name = Name}) when is_binary(Uri), is_binary(Name) ->
    validate_uri(Uri);
validate_resource(_) ->
    {error, invalid_resource}.

-spec validate_resource_template(#mcp_resource_template{}) -> ok | {error, term()}.
validate_resource_template(#mcp_resource_template{uri_template = UriTemplate, name = Name})
    when is_binary(UriTemplate), is_binary(Name) ->
    ok;
validate_resource_template(_) ->
    {error, invalid_resource_template}.

-spec encode_resource(#mcp_resource{}) -> map().
encode_resource(#mcp_resource{uri = Uri,
                              name = Name,
                              description = Desc,
                              mime_type = MimeType,
                              metadata = Metadata}) ->
    Base = #{<<"uri">> => Uri, <<"name">> => Name},
    Base1 =
        case Desc of
            undefined ->
                Base;
            _ ->
                Base#{<<"description">> => Desc}
        end,
    Base2 =
        case MimeType of
            undefined ->
                Base1;
            _ ->
                Base1#{<<"mimeType">> => MimeType}
        end,
    case Metadata of
        undefined ->
            Base2;
        _ ->
            Base2#{<<"metadata">> => Metadata}
    end.

-spec decode_resource(map()) -> #mcp_resource{}.
decode_resource(#{<<"uri">> := Uri, <<"name">> := Name} = Map) ->
    #mcp_resource{uri = Uri,
                  name = Name,
                  description = maps:get(<<"description">>, Map, undefined),
                  mime_type = maps:get(<<"mimeType">>, Map, undefined),
                  metadata = maps:get(<<"metadata">>, Map, undefined)}.

%%====================================================================
%% Internal Functions
%%====================================================================
