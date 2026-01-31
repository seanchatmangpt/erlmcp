-module(erlmcp_hibernation_tests).

-include_lib("eunit/include/eunit.hrl").
-include("erlmcp.hrl").

%%====================================================================
%% Test Descriptions
%%====================================================================

%% Tests verify hibernation behavior for idle MCP connections
%% Hibernation reduces memory per idle connection from ~50KB to ~5KB
%% through automatic garbage collection when gen_server/gen_statem hibernates

%%====================================================================
%% Hibernation Tests
%%====================================================================

%% Test that erlmcp_client hibernates after 30 seconds of inactivity
client_hibernation_test_() ->
    {timeout, 60, fun() ->
        % Start client with stdio transport
        {ok, Client} = erlmcp_client:start_link({stdio, []}),

        % Verify client is running
        ?assert(is_process_alive(Client)),

        % Get initial heap size
        {heap_size, InitialHeap} = process_info(Client, heap_size),
        ?assert(InitialHeap > 0),

        % Wait for hibernation (30s + 2s margin)
        timer:sleep(32000),

        % Verify process is still alive
        ?assert(is_process_alive(Client)),

        % Check if process has hibernated
        % When hibernated, current_function shows {erlang, hibernate, 3}
        case process_info(Client, current_function) of
            {current_function, {erlang, hibernate, 3}} ->
                % Successfully hibernated
                ok;
            {current_function, {gen_server, loop, 7}} ->
                % May be in gen_server loop, check heap reduction
                {heap_size, CurrentHeap} = process_info(Client, heap_size),
                % Heap should be significantly reduced (at least 50%)
                ?assert(CurrentHeap < (InitialHeap div 2))
            end,

        % Clean up
        erlmcp_client:stop(Client)
    end}.

%% Test that erlmcp_server hibernates after 30 seconds of inactivity
server_hibernation_test_() ->
    {timeout, 60, fun() ->
        % Start server with minimal capabilities
        Capabilities = #mcp_server_capabilities{},
        ServerId = make_ref(),
        {ok, Server} = erlmcp_server:start_link(ServerId, Capabilities),

        % Verify server is running
        ?assert(is_process_alive(Server)),

        % Get initial heap size
        {heap_size, InitialHeap} = process_info(Server, heap_size),
        ?assert(InitialHeap > 0),

        % Wait for hibernation (30s + 2s margin)
        timer:sleep(32000),

        % Verify process is still alive
        ?assert(is_process_alive(Server)),

        % Check if process has hibernated
        case process_info(Server, current_function) of
            {current_function, {erlang, hibernate, 3}} ->
                % Successfully hibernated
                ok;
            {current_function, {gen_server, loop, 7}} ->
                % May be in gen_server loop, check heap reduction
                {heap_size, CurrentHeap} = process_info(Server, heap_size),
                % Heap should be significantly reduced (at least 50%)
                ?assert(CurrentHeap < (InitialHeap div 2))
            end,

        % Clean up
        erlmcp_server:stop(Server)
    end}.

%% Test that erlmcp_circuit_breaker hibernates after 30 seconds of inactivity
circuit_breaker_hibernation_test_() ->
    {timeout, 60, fun() ->
        % Start circuit breaker with default config
        Config = #{
            failure_threshold => 5,
            success_threshold => 2,
            timeout => 60000
        },
        Name = test_hibernation_breaker,
        {ok, Breaker} = erlmcp_circuit_breaker:start_link(Name, Config),

        % Verify breaker is running
        ?assert(is_process_alive(Breaker)),

        % Get initial heap size
        {heap_size, InitialHeap} = process_info(Breaker, heap_size),
        ?assert(InitialHeap > 0),

        % Wait for hibernation (30s + 2s margin)
        timer:sleep(32000),

        % Verify process is still alive
        ?assert(is_process_alive(Breaker)),

        % Check if process has hibernated
        case process_info(Breaker, current_function) of
            {current_function, {erlang, hibernate, 3}} ->
                % Successfully hibernated
                ok;
            {current_function, {gen_statem, loop, _}} ->
                % May be in gen_statem loop, check heap reduction
                {heap_size, CurrentHeap} = process_info(Breaker, heap_size),
                % Heap should be significantly reduced (at least 50%)
                ?assert(CurrentHeap < (InitialHeap div 2))
            end,

        % Clean up
        erlmcp_circuit_breaker:stop(Breaker)
    end}.

%% Test that activity prevents hibernation
client_activity_prevents_hibernation_test_() ->
    {timeout, 45, fun() ->
        % Start client
        {ok, Client} = erlmcp_client:start_link({stdio, []}),

        % Verify client is running
        ?assert(is_process_alive(Client)),

        % Send periodic activity to prevent hibernation
        % Do 6 pings over 36 seconds (every 6 seconds)
        lists:foreach(fun(_) ->
            timer:sleep(6000),
            % Ping keeps the process active
            catch erlmcp_client:ping(Client)
        end, lists:seq(1, 6)),

        % Verify process is still alive and NOT hibernated
        ?assert(is_process_alive(Client)),

        % Check current function - should NOT be hibernated
        {current_function, CurrentFun} = process_info(Client, current_function),
        ?assertNotEqual({erlang, hibernate, 3}, CurrentFun),

        % Clean up
        erlmcp_client:stop(Client)
    end}.

%% Test that hibernated process wakes up correctly
hibernation_wakeup_test_() ->
    {timeout, 60, fun() ->
        % Start client
        {ok, Client} = erlmcp_client:start_link({stdio, []}),

        % Verify client is running
        ?assert(is_process_alive(Client)),

        % Wait for hibernation
        timer:sleep(32000),

        % Verify process is alive (may be hibernated)
        ?assert(is_process_alive(Client)),

        % Wake up by sending a request
        Result = erlmcp_client:ping(Client),

        % Verify client woke up and responded
        % (ping may fail if client is not initialized, but process should respond)
        ?assert(is_tuple(Result) orelse is_atom(Result)),

        % Verify process is still alive after wakeup
        ?assert(is_process_alive(Client)),

        % Clean up
        erlmcp_client:stop(Client)
    end}.

%%====================================================================
%% Memory Efficiency Tests
%%====================================================================

%% Test that hibernation reduces memory usage
memory_reduction_test_() ->
    {timeout, 60, fun() ->
        % Start client
        {ok, Client} = erlmcp_client:start_link({stdio, []}),

        % Get initial memory
        {memory, InitialMemory} = process_info(Client, memory),

        % Wait for hibernation
        timer:sleep(32000),

        % Get memory after hibernation
        {memory, HibernatedMemory} = process_info(Client, memory),

        % Verify memory reduction
        % Should be reduced by at least 30% (conservative estimate)
        ReductionPercent = ((InitialMemory - HibernatedMemory) * 100) / InitialMemory,
        ?assert(ReductionPercent > 30),

        % Clean up
        erlmcp_client:stop(Client)
    end}.
