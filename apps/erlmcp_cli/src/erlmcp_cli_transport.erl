%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_transport - Transport τ-interface Implementation
%%%
%%% Implements the transport behavior interface for CLI communications
%%% with stdio, tcp, http, ws, sse support.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_transport).

-behaviour(erlmcp_transport).

%% API
-export([start_link/2, send_data/2, close_transport/1]).

%% erlmcp_transport callbacks
-export([init/2, send/2, close/1]).

%% Transport types
-export([stdio/0, tcp/0, http/0, ws/0, sse/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start transport link
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
start_link(TransportType, Opts) ->
    case TransportType of
        stdio -> erlmcp_transport_stdio:start_link(Opts);
        tcp -> erlmcp_transport_tcp:start_link(Opts);
        http -> erlmcp_transport_http:start_link(Opts);
        ws -> erlmcp_transport_ws:start_link(Opts);
        sse -> erlmcp_transport_sse:start_link(Opts);
        _ -> {error, {invalid_transport_type, TransportType}}
    end.

%% @doc Send data through transport
-spec send_data(binary(), atom()) -> {ok, term()} | {error, term()}.
send_data(Data, TransportType) ->
    case TransportType of
        stdio -> erlmcp_transport_stdio:send(Data);
        tcp -> erlmcp_transport_tcp:send(Data);
        http -> erlmcp_transport_http:send(Data);
        ws -> erlmcp_transport_ws:send(Data);
        sse -> erlmcp_transport_sse:send(Data);
        _ -> {error, {invalid_transport_type, TransportType}}
    end.

%% @doc Close transport
-spec close_transport(atom()) -> ok | {error, term()}.
close_transport(TransportType) ->
    case TransportType of
        stdio -> erlmcp_transport_stdio:close();
        tcp -> erlmcp_transport_tcp:close();
        http -> erlmcp_transport_http:close();
        ws -> erlmcp_transport_ws:close();
        sse -> erlmcp_transport_sse:close();
        _ -> {error, {invalid_transport_type, TransportType}}
    end.

%% @doc Get transport type constants
-spec stdio() -> atom().
stdio() -> stdio.

-spec tcp() -> atom().
tcp() -> tcp.

-spec http() -> atom().
http() -> http.

-spec ws() -> atom().
ws() -> ws.

-spec sse() -> atom().
sse() -> sse.

%%====================================================================
%% erlmcp_transport callbacks
%%====================================================================

%% @doc Initialize transport (stub - implemented in specific modules)
-spec init(atom(), map()) -> {ok, term()} | {error, term()}.
init(TransportType, Opts) ->
    case TransportType of
        stdio -> erlmcp_transport_stdio:init(Opts);
        tcp -> erlmcp_transport_tcp:init(Opts);
        http -> erlmcp_transport_http:init(Opts);
        ws -> erlmcp_transport_ws:init(Opts);
        sse -> erlmcp_transport_sse:init(Opts);
        _ -> {error, {invalid_transport_type, TransportType}}
    end.

%% @doc Send data through transport (stub - implemented in specific modules)
-spec send(term(), term()) -> {ok, term()} | {error, term()}.
send(Data, State) ->
    %% This should be implemented in specific transport modules
    {error, not_implemented}.

%% @doc Close transport (stub - implemented in specific modules)
-spec close(term()) -> ok.
close(State) ->
    %% This should be implemented in specific transport modules
    ok.