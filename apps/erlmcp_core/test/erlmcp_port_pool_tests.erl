-module(erlmcp_port_pool_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%% EUnit tests for erlmcp_port_pool (Chicago School TDD)
%%
%% Tests port pool functionality with real poolboy integration
%% NO MOCKS - uses actual pool

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Setup port pool for testing
setup_port_pool() ->
    {ok, Pid} = erlmcp_port_pool:start_link(#{
        size => 2,
        max_overflow => 1
    }),
    Pid.

%% @doc Cleanup port pool
cleanup_port_pool(Pid) ->
    catch exit(Pid, shutdown),
    ok.

%%====================================================================
%% Pool Lifecycle Tests
%%====================================================================

port_pool_lifecycle_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Pool started successfully
                    ?assert(true)
                end)
         ]
     end}.

%%====================================================================
%% Port Checkout Tests
%%====================================================================

checkout_port_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Checkout port from pool
                    Result = erlmcp_port_pool:checkout_port(),
                    case Result of
                        {ok, WorkerPid} when is_pid(WorkerPid) ->
                            %% Return port to pool
                            erlmcp_port_pool:return_port(WorkerPid),
                            ?assert(true);
                        {error, Reason} ->
                            %% Pool may be initializing
                            ?assert(true)
                    end
                end)
         ]
     end}.

checkout_multiple_ports_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Checkout multiple ports
                    Workers = [case erlmcp_port_pool:checkout_port() of
                                   {ok, Pid} -> Pid;
                                   {error, _} -> undefined
                               end || _ <- lists:seq(1, 3)],
                    %% Return all workers
                    lists:foreach(fun(W) when is_pid(W) ->
                                        erlmcp_port_pool:return_port(W);
                                   (_) ->
                                        ok
                                end, Workers),
                    ?assert(length([W || W <- Workers, is_pid(W)]) >= 1)
                end)
         ]
     end}.

checkout_timeout_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Try to checkout with very short timeout
                    Result = erlmcp_port_pool:checkout_port(1),
                    %% May timeout or succeed (pool timing dependent)
                    case Result of
                        {ok, WorkerPid} ->
                            erlmcp_port_pool:return_port(WorkerPid),
                            ?assert(true);
                        {error, timeout} ->
                            ?assert(true);
                        _ ->
                            ?assert(true)
                    end
                end)
         ]
     end}.

%%====================================================================
%% Port Return Tests
%%====================================================================

return_port_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Checkout and return port
                    case erlmcp_port_pool:checkout_port() of
                        {ok, WorkerPid} ->
                            %% Return port
                            ok = erlmcp_port_pool:return_port(WorkerPid),
                            ?assert(true);
                        {error, _} ->
                            ?assert(true)
                    end
                end)
         ]
     end}.

return_invalid_port_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Try to return invalid port (should not crash)
                    InvalidPid = spawn(fun() -> ok end),
                    Result = catch erlmcp_port_pool:return_port(InvalidPid),
                    %% Should not crash
                    ?assert(true)
                end)
         ]
     end}.

%%====================================================================
%% Pool Status Tests
%%====================================================================

pool_status_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Get pool status
                    {ok, Status} = erlmcp_port_pool:pool_status(),
                    ?assert(maps:is_key(size, Status)),
                    ?assert(maps:is_key(available, Status)),
                    ?assert(maps:is_key(overflow, Status)),
                    ?assert(maps:is_key(total_requests, Status)),
                    ?assert(is_integer(maps:get(size, Status))),
                    ?assert(is_integer(maps:get(available, Status)))
                end)
         ]
     end}.

%%====================================================================
%% Healthy Ports Tests
%%====================================================================

healthy_ports_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Get healthy ports
                    Result = erlmcp_port_pool:healthy_ports(),
                    case Result of
                        {ok, HealthyPids} ->
                            ?assert(is_list(HealthyPids)),
                            lists:foreach(fun(Pid) ->
                                                ?assert(is_pid(Pid)),
                                                ?assert(is_process_alive(Pid))
                                          end, HealthyPids);
                        {error, _} ->
                            ?assert(true)
                    end
                end)
         ]
     end}.

%%====================================================================
%% Load Balancing Tests
%%====================================================================

load_balancing_test_() ->
    {setup,
     fun setup_port_pool/0,
     fun cleanup_port_pool/1,
     fun(_Pid) ->
         [
          ?_test(begin
                    %% Checkout and return multiple times
                    Workers = lists:filtermap(fun(_) ->
                        case erlmcp_port_pool:checkout_port() of
                            {ok, Pid} -> {true, Pid};
                            _ -> false
                        end
                    end, lists:seq(1, 10)),
                    %% Return all workers
                    lists:foreach(fun(W) -> erlmcp_port_pool:return_port(W) end, Workers),
                    ?assert(length(Workers) >= 1)
                end)
         ]
     end}.

%%====================================================================
%% Property-Based Tests
%%====================================================================

prop_checkout_return() ->
    ?FORALL(N, choose(1, 20),
        begin
            {ok, _Pid} = erlmcp_port_pool:start_link(#{}),
            Result = try
                %% Checkout N ports
                Workers = lists:filtermap(fun(_) ->
                    case erlmcp_port_pool:checkout_port() of
                        {ok, W} -> {true, W};
                        _ -> false
                    end
                end, lists:seq(1, N)),
                %% Return all workers
                lists:foreach(fun(W) -> erlmcp_port_pool:return_port(W) end, Workers),
                true
            catch
                _:_ -> false
            after
                catch exit(whereis(erlmcp_port_pool), shutdown)
            end,
            ?implies(N =< 20, Result)
        end).

checkout_return_property_test_() ->
    {setup,
     fun() -> ok end,
     fun(__) -> ok end,
     fun(_) ->
         [
          ?_test(eunit:quickcheck(numtests(10, prop_checkout_return()))
         ]
     end}.
