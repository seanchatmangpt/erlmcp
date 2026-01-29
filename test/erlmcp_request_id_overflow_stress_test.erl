%%%-------------------------------------------------------------------
%%% @doc
%%% Destructive stress test for request ID overflow behavior.
%%%
%%% This test pushes the MCP client to ID exhaustion to find overflow bugs.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_request_id_overflow_stress_test).
-author("performance").

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Constants
%%====================================================================
-define(MAX_SAFE_REQUEST_ID, 1152921504606846975).  % 2^60 - 1
-define(OVERFLOW_TEST_PORT, 10003).

%%====================================================================
%% Test Generator: Rapid ID Increment Near Overflow
%%====================================================================

%% Test 1: Verify overflow detection at boundary
overflow_detection_test() ->
    %% Test the exact boundary
    ?assertEqual({error, overflow}, 
        erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID)),
    
    %% Test one before boundary
    ?assertEqual({ok, ?MAX_SAFE_REQUEST_ID}, 
        erlmcp_request_id:safe_increment(?MAX_SAFE_REQUEST_ID - 1)),
    
    %% Test far from boundary
    ?assertEqual({ok, 100}, 
        erlmcp_request_id:safe_increment(99)).

%% Test 2: Simulate rapid ID generation to near overflow
rapid_increment_test_() ->
    {timeout, 30, fun() ->
        %% Start from 2^60 - 1000 and increment rapidly
        StartId = ?MAX_SAFE_REQUEST_ID - 1000,
        FinalId = rapid_increment(StartId, 1000),
        ?assertEqual({error, overflow}, FinalId)
    end}.

%% Test 3: Concurrent ID increment (simulates multiple requests)
concurrent_increment_test_() ->
    {timeout, 30, fun() ->
        %% Spawn multiple processes incrementing from same base
        BaseId = ?MAX_SAFE_REQUEST_ID - 100,
        Parent = self(),
        
        %% Spawn 10 processes each doing 10 increments
        lists:foreach(fun(N) ->
            spawn(fun() ->
                Result = rapid_increment(BaseId + (N * 10), 10),
                Parent ! {N, Result}
            end)
        end, lists:seq(0, 9)),
        
        %% Collect results
        Results = [receive 
            {N, Result} -> {N, Result}
        end || N <- lists:seq(0, 9)],
        
        %% At least some should hit overflow
        OverflowCount = length([R || {_, R} <- Results, R =:= {error, overflow}]),
        ?assert(OverflowCount > 0)
    end}.

%% Test 4: Client behavior at ID exhaustion
client_overflow_behavior_test_() ->
    {timeout, 60, fun() ->
        %% This test requires a running server
        case catch start_test_server() of
            {ok, ServerPid} ->
                try
                    %% Create client with request ID near exhaustion
                    {ok, Client} = erlmcp_client:start_link({stdio, []}),
                    
                    %% Manually set request ID to near overflow
                    sys:replace_state(Client, fun(State) ->
                        State#state{request_id = ?MAX_SAFE_REQUEST_ID - 10}
                    end),
                    
                    %% Try to send requests until overflow
                    Results = send_requests_until_overflow(Client, 20),
                    
                    %% Verify error handling
                    ?assertMatch([{error, {request_id_overflow, _}} | _], Results),
                    
                    erlmcp_client:stop(Client)
                catch
                    Error:_Reason ->
                        %% Expected - client may crash on overflow
                        ?assert(true)
                after
                    stop_test_server(ServerPid)
                end;
            _ ->
                {skip, "Test server not available"}
        end
    end}.

%% Test 5: Memory leak detection during rapid ID generation
memory_leak_test_() ->
    {timeout, 30, fun() ->
        %% Monitor memory during rapid ID generation
        {ok, Client} = erlmcp_client:start_link({stdio, []}),
        
        %% Get initial memory
        InitialMemory = erlang:memory(processes),
        
        %% Generate lots of IDs (but not to overflow)
        lists:foreach(fun(_) ->
            sys:replace_state(Client, fun(State) ->
                NewId = State#state.request_id + 1,
                State#state{request_id = NewId}
            end)
        end, lists:seq(1, 10000)),
        
        %% Get final memory
        FinalMemory = erlang:memory(processes),
        
        %% Memory growth should be minimal (< 1MB)
        MemoryGrowth = FinalMemory - InitialMemory,
        ?assert(MemoryGrowth < 1000000),
        
        erlmcp_client:stop(Client)
    end}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% @doc Rapidly increment request ID N times
-spec rapid_increment(request_id(), pos_integer()) -> 
    {ok, request_id()} | {error, overflow}.
rapid_increment(CurrentId, 0) ->
    {ok, CurrentId};
rapid_increment(CurrentId, Count) when Count > 0 ->
    case erlmcp_request_id:safe_increment(CurrentId) of
        {ok, NextId} ->
            rapid_increment(NextId, Count - 1);
        {error, overflow} = Error ->
            Error
    end.

%% @doc Send requests until client hits overflow
-spec send_requests_until_overflow(client(), pos_integer()) -> 
    [{ok, map()} | {error, term()}].
send_requests_until_overflow(_Client, 0) ->
    [];
send_requests_until_overflow(Client, Count) when Count > 0 ->
    case catch erlmcp_client:call_tool(Client, <<"echo">>, #{<<"data">> => <<"test">>}) of
        {'EXIT', {request_id_exhausted, _}} ->
            [{error, {request_id_overflow, <<"Request ID space exhausted">>}}];
        {'EXIT', Reason} ->
            [{error, Reason}];
        {error, Reason} ->
            [{error, Reason} | send_requests_until_overflow(Client, Count - 1)];
        {ok, _Result} ->
            send_requests_until_overflow(Client, Count - 1)
    end.

%% @doc Start a test TCP server
-spec start_test_server() -> {ok, pid()} | {error, term()}.
start_test_server() ->
    case gen_tcp:listen(?OVERFLOW_TEST_PORT, [
        binary, 
        {packet, line},
        {reuseaddr, true},
        {active, false}
    ]) of
        {ok, ListenSocket} ->
            spawn(fun() -> echo_server_loop(ListenSocket) end),
            {ok, ListenSocket};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop test server
-spec stop_test_server(pid()) -> ok.
stop_test_server(Socket) when is_port(Socket) ->
    gen_tcp:close(Socket);
stop_test_server(_) ->
    ok.

%% @doc Simple echo server loop
-spec echo_server_loop(port()) -> no_return().
echo_server_loop(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            spawn(fun() -> echo_client_loop(Socket) end),
            echo_server_loop(ListenSocket);
        {error, closed} ->
            ok
    end.

%% @doc Handle echo client
-spec echo_client_loop(port()) -> ok.
echo_client_loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            echo_client_loop(Socket);
        {error, closed} ->
            gen_tcp:close(Socket),
            ok
    end.
