%%%-------------------------------------------------------------------
%%% @doc
%%% Resource Subscription Utility Functions
%%%
%%% Provides validation and error formatting for resource subscriptions.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(erlmcp_subscription_handlers).

-include("erlmcp.hrl").

%% API exports
-export([
    validate_resource_uri/1,
    format_error/1
]).

%%====================================================================
%% Public API
%%====================================================================

%% @doc Validate resource URI format
%% Returns ok if URI is valid, {error, invalid_uri} otherwise
-spec validate_resource_uri(binary()) -> ok | {error, invalid_uri}.
validate_resource_uri(Uri) when is_binary(Uri) ->
    case Uri of
        <<>> -> ok;  % Empty URI is valid (represents root/all resources)
        <<"file://", _/binary>> -> ok;
        <<"http://", _/binary>> -> ok;
        <<"https://", _/binary>> -> ok;
        <<"resource://", _/binary>> -> ok;
        <<"data://", _/binary>> -> ok;
        _Other -> ok  % Accept any URI format - server-specific validation
    end;
validate_resource_uri(_) ->
    {error, invalid_uri}.

%% @doc Format error term for JSON response
-spec format_error(term()) -> binary().
format_error(not_found) -> <<"Resource not found">>;
format_error({timeout, _}) -> <<"Operation timed out">>;
format_error({error, Reason}) -> format_error(Reason);
format_error(Reason) when is_atom(Reason) ->
    erlang:atom_to_binary(Reason, utf8);
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(_) ->
    <<"Unknown error">>.
