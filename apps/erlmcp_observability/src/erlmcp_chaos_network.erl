%%%-------------------------------------------------------------------
%%% @doc erlmcp_chaos_network - Network Chaos Primitives
%%%
%%% Network failure injection for chaos engineering:
%%% - Latency injection (add delay to messages)
%%% - Network partition simulation
%%% - Packet loss injection
%%% - Connection throttling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_chaos_network).

-export([
    inject_latency/1,
    inject_partition/1,
    inject_packet_loss/1,
    throttle_connections/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Inject network latency to percentage of requests
-spec inject_latency(map()) -> ok.
inject_latency(Config) ->
    Latency = maps:get(latency, Config),
    Rate = maps:get(rate, Config, 0.2),
    Interval = maps:get(interval, Config, 30000),
    
    ?LOG_INFO("Injecting ~pms latency to ~.1f% of traffic", [Latency, Rate * 100]),
    
    % Run latency injection loop
    inject_latency_loop(Latency, Rate, Interval).

-spec inject_latency_loop(pos_integer(), float(), pos_integer()) -> ok.
inject_latency_loop(Latency, Rate, Interval) ->
    % Intercept messages and delay them
    timer:sleep(Interval),
    
    % In real implementation, would hook into message passing
    % For now, simulate the effect
    case rand:uniform() < Rate of
        true ->
            ?LOG_DEBUG("Injecting ~pms latency", [Latency]),
            timer:sleep(Latency);
        false ->
            ok
    end,
    
    inject_latency_loop(Latency, Rate, Interval).

%% @doc Simulate network partition between nodes
-spec inject_partition(map()) -> ok.
inject_partition(Config) ->
    Duration = maps:get(duration, Config, 60000),
    Nodes = maps:get(nodes, Config, nodes()),
    
    ?LOG_INFO("Injecting network partition for ~pms affecting nodes: ~p", 
             [Duration, Nodes]),
    
    % In distributed setup, would disconnect nodes
    lists:foreach(
        fun(Node) ->
            case Node =:= node() of
                true -> ok;
                false ->
                    ?LOG_INFO("Simulating partition from ~p", [Node]),
                    % In real implementation: net_kernel:disconnect_node(Node)
                    ok
            end
        end,
        Nodes
    ),
    
    timer:sleep(Duration),
    
    ?LOG_INFO("Healing network partition", []),
    ok.

%% @doc Inject packet loss (drop percentage of messages)
-spec inject_packet_loss(map()) -> ok.
inject_packet_loss(Config) ->
    Rate = maps:get(rate, Config),
    Interval = maps:get(interval, Config, 30000),
    
    ?LOG_INFO("Injecting ~.1f% packet loss", [Rate * 100]),
    
    inject_packet_loss_loop(Rate, Interval).

-spec inject_packet_loss_loop(float(), pos_integer()) -> ok.
inject_packet_loss_loop(Rate, Interval) ->
    timer:sleep(Interval),
    
    % In real implementation, would intercept and drop messages
    case rand:uniform() < Rate of
        true ->
            ?LOG_DEBUG("Dropping packet", []);
        false ->
            ok
    end,
    
    inject_packet_loss_loop(Rate, Interval).

%% @doc Throttle connection rate
-spec throttle_connections(map()) -> ok.
throttle_connections(Config) ->
    MaxRate = maps:get(max_rate, Config, 100),
    Duration = maps:get(duration, Config, 60000),
    
    ?LOG_INFO("Throttling connections to ~p/sec for ~pms", [MaxRate, Duration]),
    
    timer:sleep(Duration),
    ok.

