%%%-------------------------------------------------------------------
%%% @doc
%%% HTTP Transport Behavior Adapter
%%%
%%% This module acts as an adapter between the erlmcp_transport_behavior
%%% and the actual HTTP transport implementation to avoid naming conflicts
%%% with gen_server callbacks.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_http_adapter).

-behaviour(erlmcp_transport_behavior).

%% Transport behavior callbacks
-export([init/1, send/2, close/1, get_info/1, handle_transport_call/2]).

%% @doc Adapter for transport behavior init/1
-spec init(map()) -> {ok, term()} | {error, term()}.
init(Config) ->
    erlmcp_transport_http:init_transport(maps:get(transport_id, Config), Config).

%% @doc Adapter for transport behavior send/2
-spec send(term(), binary()) -> ok | {error, term()}.
send(State, Data) ->
    erlmcp_transport_http:send(State, Data).

%% @doc Adapter for transport behavior close/1
-spec close(term()) -> ok | {error, term()}.
close(State) ->
    erlmcp_transport_http:close(State).

%% @doc Adapter for transport behavior get_info/1
-spec get_info(term()) -> {ok, map()} | {error, term()}.
get_info(State) ->
    erlmcp_transport_http:get_info(State).

%% @doc Adapter for transport behavior handle_transport_call/2
-spec handle_transport_call(term(), term()) -> {ok, term()} | {error, term()}.
handle_transport_call(Request, State) ->
    erlmcp_transport_http:handle_transport_call(Request, State).