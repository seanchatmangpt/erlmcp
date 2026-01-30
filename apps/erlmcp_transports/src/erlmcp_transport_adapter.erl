%%%-------------------------------------------------------------------
%%% @doc
%%% Transport Behavior Adapter
%%%
%%% This module provides adapter functions to bridge between gen_server-based
%%% transport implementations and the erlmcp_transport_behavior interface.
%%%
%%% Transport implementations (stdio, tcp, sse, etc.) are implemented as
%%% gen_servers for OTP compliance. This adapter allows them to work with
%%% the erlmcp_transport_behavior specification used by tests and documentation.
%%%
%%% == Architecture ==
%%%
%%% Transport modules are gen_servers with:
%%% - start_link/1 or start_link/2 for starting the process
%%% - send/2 for sending data (module:function interface, not callback)
%%% - close/1 for closing transport (module:function interface)
%%%
%%% The behavior module defines callbacks that transports SHOULD implement
%%% conceptually, but they use gen_server callbacks in practice.
%%%
%%% This adapter provides validation and helper functions that work with
%%% the actual transport implementations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_adapter).

-include("erlmcp.hrl").

%% API
-export([
    validate_transport_opts/2,
    validate_transport_init/1,
    validate_transport_send/2,
    validate_transport_close/1
]).

%% Type exports
-export_type([
    transport_type/0,
    transport_opts/0
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type transport_type() :: stdio | tcp | sse | http | websocket.
-type transport_opts() :: map().

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Validate transport options for a specific transport type
%%
%% This function validates that required options are present and have
%% correct types for the specified transport type.
%%
%% @param TransportType The type of transport (stdio, tcp, sse, http, websocket)
%% @param Opts Options map to validate
%% @returns ok | {error, Reason}
-spec validate_transport_opts(transport_type(), transport_opts()) -> ok | {error, term()}.
validate_transport_opts(stdio, Opts) when is_map(Opts) ->
    % stdio requires owner (pid)
    case maps:get(owner, Opts, undefined) of
        Pid when is_pid(Pid) -> ok;
        undefined -> {error, {invalid_opts, missing_owner}};
        _ -> {error, {invalid_opts, {invalid_owner_type, expected_pid}}}
    end;

validate_transport_opts(tcp, Opts) when is_map(Opts) ->
    % tcp requires mode, host, port
    ModeResult = case maps:get(mode, Opts, undefined) of
        Mode when Mode =:= client orelse Mode =:= server -> ok;
        undefined -> {error, {invalid_opts, missing_mode}};
        _ -> {error, {invalid_opts, {invalid_mode, expected_client_or_server}}}
    end,

    case ModeResult of
        ok ->
            HostResult = case maps:get(host, Opts, undefined) of
                Host when is_list(Host); is_tuple(Host) -> ok;
                undefined -> {error, {invalid_opts, missing_host}};
                _ -> {error, {invalid_opts, {invalid_host_type, expected_string_or_tuple}}}
            end,

            case HostResult of
                ok ->
                    case maps:get(port, Opts, undefined) of
                        Port when is_integer(Port), Port > 0, Port =< 65535 -> ok;
                        undefined -> {error, {invalid_opts, missing_port}};
                        _ -> {error, {invalid_opts, {invalid_port, expected_integer_1_to_65535}}}
                    end;
                Error -> Error
            end;
        Error -> Error
    end;

validate_transport_opts(sse, Opts) when is_map(Opts) ->
    % sse requires port
    case maps:get(port, Opts, undefined) of
        Port when is_integer(Port), Port > 0, Port =< 65535 -> ok;
        undefined -> {error, {invalid_opts, missing_port}};
        _ -> {error, {invalid_opts, {invalid_port, expected_integer_1_to_65535}}}
    end;

validate_transport_opts(http, Opts) when is_map(Opts) ->
    % http requires url
    case maps:get(url, Opts, undefined) of
        Url when is_binary(Url); is_list(Url) ->
            case validate_http_url(Url) of
                ok -> ok;
                {error, _} = Error -> Error
            end;
        undefined -> {error, {invalid_opts, missing_url}};
        _ -> {error, {invalid_opts, {invalid_url_type, expected_string_or_binary}}}
    end;

validate_transport_opts(websocket, Opts) when is_map(Opts) ->
    % websocket requires url with ws:// or wss:// scheme
    case maps:get(url, Opts, undefined) of
        Url when is_binary(Url); is_list(Url) ->
            case validate_websocket_url(Url) of
                ok -> ok;
                {error, _} = Error -> Error
            end;
        undefined -> {error, {invalid_opts, missing_url}};
        _ -> {error, {invalid_opts, {invalid_url_type, expected_string_or_binary}}}
    end;

validate_transport_opts(TransportType, _) ->
    {error, {unknown_transport_type, TransportType}}.

%% @doc Validate transport initialization parameters
%%
%% Validates that the parameters for starting a transport are correct.
%% This checks that the required options are present and valid.
%%
%% @param Params Initialization parameters (varies by transport type)
%% @returns ok | {error, Reason}
-spec validate_transport_init(term()) -> ok | {error, term()}.
validate_transport_init({stdio, Opts}) when is_map(Opts) ->
    validate_transport_opts(stdio, Opts);

validate_transport_init({tcp, Opts}) when is_map(Opts) ->
    validate_transport_opts(tcp, Opts);

validate_transport_init({sse, Opts}) when is_map(Opts) ->
    validate_transport_opts(sse, Opts);

validate_transport_init({http, Opts}) when is_map(Opts) ->
    validate_transport_opts(http, Opts);

validate_transport_init({websocket, Opts}) when is_map(Opts) ->
    validate_transport_opts(websocket, Opts);

validate_transport_init(Opts) when is_map(Opts) ->
    % Try to detect transport type from options
    case maps:get(type, Opts, undefined) of
        undefined ->
            {error, {missing_transport_type}};
        TransportType ->
            validate_transport_opts(TransportType, Opts)
    end;

validate_transport_init(_) ->
    {error, {invalid_init_params, expected_map_or_type_tuple}}.

%% @doc validate transport send operation
%%
%% Validates that send operation parameters are correct.
%%
%% @param Data Data to send (should be binary or iolist)
%% @param StateOrPid Transport state or process PID
%% @returns ok | {error, Reason}
-spec validate_transport_send(term(), term()) -> ok | {error, term()}.
validate_transport_send(Data, _StateOrPid) when is_binary(Data); is_list(Data) ->
    ok;
validate_transport_send(_, _) ->
    {error, {invalid_send_data, expected_binary_or_iolist}}.

%% @doc Validate transport close operation
%%
%% Validates that close operation parameters are correct.
%%
%% @param StateOrPid Transport state or process PID
%% @returns ok | {error, Reason}
-spec validate_transport_close(term()) -> ok.
validate_transport_close(_StateOrPid) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Validate HTTP URL (http:// or https://)
-spec validate_http_url(binary() | string()) -> ok | {error, term()}.
validate_http_url(Url) when is_binary(Url) ->
    validate_http_url(binary_to_list(Url));
validate_http_url(Url) when is_list(Url) ->
    case string:prefix(Url, "http://") of
        nomatch ->
            case string:prefix(Url, "https://") of
                nomatch ->
                    {error, {invalid_url, expected_http_or_https_scheme}};
                _ ->
                    validate_url_not_empty(Url)
            end;
        _ ->
            validate_url_not_empty(Url)
    end.

%% @private
%% Validate WebSocket URL (ws:// or wss://)
-spec validate_websocket_url(binary() | string()) -> ok | {error, term()}.
validate_websocket_url(Url) when is_binary(Url) ->
    validate_websocket_url(binary_to_list(Url));
validate_websocket_url(Url) when is_list(Url) ->
    case string:prefix(Url, "ws://") of
        nomatch ->
            case string:prefix(Url, "wss://") of
                nomatch ->
                    {error, {invalid_url, expected_ws_or_wss_scheme}};
                _ ->
                    validate_url_not_empty(Url)
            end;
        _ ->
            validate_url_not_empty(Url)
    end.

%% @private
%% Validate URL is not empty
-spec validate_url_not_empty(string()) -> ok | {error, term()}.
validate_url_not_empty("") ->
    {error, {invalid_url, url_is_empty}};
validate_url_not_empty(_) ->
    ok.
