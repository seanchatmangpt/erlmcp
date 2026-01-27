%%%-------------------------------------------------------------------
%% @doc Utility functions for MCP Apps feature
%%
%% Provides helper functions for app ID generation, validation,
%% manifest verification, and security checks.
%%
%% @author ErlMCP Development Team
%% @since 0.8.0
%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_apps_util).

-include("erlmcp.hrl").

-export([
    generate_app_id/1,
    validate_app_manifest/1,
    validate_app_name/1,
    validate_permission/1,
    validate_uri/1,
    normalize_app_name/1,
    calculate_app_checksum/1,
    serialize_app/1,
    deserialize_app/1
]).

-type validation_result() :: ok | {error, term()}.

%% Constants
-define(MAX_APP_NAME_LENGTH, 255).
-define(MAX_DESCRIPTION_LENGTH, 1024).
-define(VALID_PERMISSION_PREFIXES, [
    <<"resources/">>,
    <<"tools/">>,
    <<"prompts/">>,
    <<"tasks/">>,
    <<"app/">>,
    <<"admin/">>
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Generate a unique app ID from app name
-spec generate_app_id(binary()) -> binary().
generate_app_id(Name) ->
    Normalized = normalize_app_name(Name),
    Timestamp = integer_to_binary(erlang:system_time(nanosecond)),
    Random = base64:encode(crypto:strong_rand_bytes(8)),
    <<Normalized/binary, "-", Timestamp/binary, "-", Random/binary>>.

%% @doc Validate an app manifest structure
-spec validate_app_manifest(map()) -> validation_result().
validate_app_manifest(Manifest) when is_map(Manifest) ->
    case Manifest of
        #{<<"name">> := Name, <<"version">> := Version} when
            is_binary(Name), is_binary(Version) ->
            case validate_app_name(Name) of
                ok -> ok;
                Error -> Error
            end;
        _ ->
            {error, <<"Missing required manifest fields: name, version">>}
    end;
validate_app_manifest(_) ->
    {error, <<"Manifest must be a map">>}.

%% @doc Validate an app name
-spec validate_app_name(binary()) -> validation_result().
validate_app_name(Name) when is_binary(Name), byte_size(Name) > 0 ->
    if
        byte_size(Name) > ?MAX_APP_NAME_LENGTH ->
            {error, <<"App name too long">>};
        not valid_name_chars(Name) ->
            {error, <<"App name contains invalid characters">>};
        true ->
            ok
    end;
validate_app_name(_) ->
    {error, <<"App name must be a non-empty binary">>}.

%% @doc Validate a permission string
-spec validate_permission(binary()) -> validation_result().
validate_permission(Permission) when is_binary(Permission) ->
    case has_valid_prefix(Permission) of
        true -> ok;
        false -> {error, <<"Permission has invalid prefix">>}
    end;
validate_permission(_) ->
    {error, <<"Permission must be a binary">>}.

%% @doc Validate a URI string
-spec validate_uri(binary()) -> validation_result().
validate_uri(Uri) when is_binary(Uri), byte_size(Uri) > 0 ->
    case uri_string:parse(Uri) of
        #{scheme := Scheme} when Scheme =:= <<"http">> orelse Scheme =:= <<"https">> ->
            ok;
        _ ->
            {error, <<"Invalid URI scheme. Must be http or https">>}
    end;
validate_uri(_) ->
    {error, <<"URI must be a non-empty binary">>}.

%% @doc Normalize app name (lowercase, replace spaces with underscores)
-spec normalize_app_name(binary()) -> binary().
normalize_app_name(Name) when is_binary(Name) ->
    Lower = string:lowercase(Name),
    re:replace(Lower, <<"\\s+">>, <<"_">>, [global, {return, binary}]).

%% @doc Calculate checksum of app manifest
-spec calculate_app_checksum(map()) -> binary().
calculate_app_checksum(Manifest) when is_map(Manifest) ->
    Json = jsx:encode(Manifest),
    Hash = crypto:hash(sha256, Json),
    base64:encode(Hash).

%% @doc Serialize app record to map
-spec serialize_app(term()) -> map().
serialize_app(App) when is_record(App, mcp_app) ->
    #{
        <<"id">> => App#mcp_app.id,
        <<"name">> => App#mcp_app.name,
        <<"version">> => App#mcp_app.version,
        <<"description">> => App#mcp_app.description,
        <<"status">> => atom_to_binary(App#mcp_app.status, utf8),
        <<"uri">> => App#mcp_app.uri,
        <<"manifest">> => App#mcp_app.manifest,
        <<"permissions">> => sets:to_list(App#mcp_app.permissions),
        <<"state">> => App#mcp_app.state,
        <<"created_at">> => App#mcp_app.created_at,
        <<"activated_at">> => App#mcp_app.activated_at,
        <<"resources">> => App#mcp_app.resources,
        <<"error">> => App#mcp_app.error
    };
serialize_app(Data) when is_map(Data) ->
    Data.

%% @doc Deserialize map to app record
-spec deserialize_app(map()) -> erlmcp_apps:mcp_app() | {error, term()}.
deserialize_app(#{<<"id">> := Id, <<"name">> := Name, <<"version">> := Version} = Map) ->
    try
        #mcp_app{
            id = Id,
            name = Name,
            version = Version,
            description = maps:get(<<"description">>, Map, <<"">>),
            status = binary_to_atom(maps:get(<<"status">>, Map, <<"initialized">>), utf8),
            uri = maps:get(<<"uri">>, Map, undefined),
            manifest = maps:get(<<"manifest">>, Map, undefined),
            permissions = sets:from_list(maps:get(<<"permissions">>, Map, [])),
            state = maps:get(<<"state">>, Map, #{}),
            created_at = maps:get(<<"created_at">>, Map, 0),
            activated_at = maps:get(<<"activated_at">>, Map, undefined),
            resources = maps:get(<<"resources">>, Map, []),
            error = maps:get(<<"error">>, Map, undefined)
        }
    catch
        _:_ -> {error, <<"Invalid app serialization">>}
    end;
deserialize_app(_) ->
    {error, <<"Missing required app fields">>}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Check if name contains only valid characters
-spec valid_name_chars(binary()) -> boolean().
valid_name_chars(Name) ->
    case re:run(Name, <<"^[a-zA-Z0-9_\\-\\s.]+$">>) of
        {match, _} -> true;
        nomatch -> false
    end.

%% @private Check if permission has valid prefix
-spec has_valid_prefix(binary()) -> boolean().
has_valid_prefix(Permission) ->
    lists:any(fun(Prefix) ->
        binary:match(Permission, Prefix) =/= nomatch
    end, ?VALID_PERMISSION_PREFIXES).
