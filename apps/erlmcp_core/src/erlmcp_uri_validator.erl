%%%-------------------------------------------------------------------
%%% @doc URI Validator for erlmcp resources
%%%
%%% Simple validation for URI templates and resource URIs.
%%% Follows Joe Armstrong's principle: "Validate everything that comes in."
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_uri_validator).

%% API exports
-export([
    validate_uri_template/1,
    validate_resource_uri_on_registration/1
]).

%% Types
-type validation_result() :: ok | {error, {atom(), binary()}}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate URI template format
%% TODO: Full RFC 6570 URI template validation per MCP spec
-spec validate_uri_template(binary()) -> validation_result().
validate_uri_template(UriTemplate) when is_binary(UriTemplate) ->
    case byte_size(UriTemplate) > 0 of
        true ->
            %% Basic check: must start with /
            case UriTemplate of
                <<"/", _/binary>> -> ok;
                _ -> {error, {invalid_uri_template, <<"URI template must start with /">>}}
            end;
        false ->
            {error, {empty_uri_template, <<"URI template cannot be empty">>}}
    end;
validate_uri_template(_) ->
    {error, {invalid_uri_type, <<"URI template must be a binary">>}}.

%% @doc Validate resource URI on registration
-spec validate_resource_uri_on_registration(binary()) -> validation_result().
validate_resource_uri_on_registration(Uri) when is_binary(Uri) ->
    case byte_size(Uri) > 0 of
        true ->
            %% Basic check: must start with /
            case Uri of
                <<"/", _/binary>> -> ok;
                _ -> {error, {invalid_uri, <<"Resource URI must start with /">>}}
            end;
        false ->
            {error, {empty_uri, <<"Resource URI cannot be empty">>}}
    end;
validate_resource_uri_on_registration(_) ->
    {error, {invalid_uri_type, <<"Resource URI must be a binary">>}}.
