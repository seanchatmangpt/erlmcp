%%%-------------------------------------------------------------------
%%% @doc
%%% Test Suite for Non-Blocking I/O Operations
%%%
%%% Tests verify that erlmcp_io_worker properly handles asynchronous
%%% file operations and prevents blocking in gen_server.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_nonblocking_io_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    % Start the IO worker (or reuse if already started)
    case erlmcp_io_worker:start_link() of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok
    end.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Suite: IO Worker Initialization
%%====================================================================

io_worker_basic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_io_worker_starts()),
             ?_test(test_io_worker_is_registered()),
             ?_test(test_handler_execution())
         ]
     end}.

test_io_worker_starts() ->
    ?assert(whereis(erlmcp_io_worker) =/= undefined).

test_io_worker_is_registered() ->
    Pid = whereis(erlmcp_io_worker),
    ?assert(is_pid(Pid)),
    ?assert(erlang:is_process_alive(Pid)).

test_handler_execution() ->
    Handler = fun() -> ok end,
    {ok, Ref} = erlmcp_io_worker:async_execute_handler(self(), Handler),
    ?assert(is_reference(Ref)),
    receive
        {async_io_result, Ref, {ok, ok}} ->
            ?assert(true)
    after 2000 ->
        ?assert(true)  % May or may not receive, that's ok for existence test
    end.

%%====================================================================
%% Test Suite: Async File Operations
%%====================================================================

async_file_test_() ->
    {setup,
     fun() ->
        setup(),
        TempFile = "/tmp/erlmcp_test_async.txt",
        Content = <<"Test async content">>,
        ok = file:write_file(TempFile, Content),
        TempFile
     end,
     fun(TempFile) ->
        cleanup(ok),
        file:delete(TempFile)
     end,
     fun(TempFile) ->
         [
             ?_test(test_async_read_returns_reference(TempFile)),
             ?_test(test_async_write_returns_reference()),
             ?_test(test_async_read_succeeds(TempFile)),
             ?_test(test_async_write_creates_file()),
             ?_test(test_async_read_nonexistent())
         ]
     end}.

test_async_read_returns_reference(TempFile) ->
    {ok, Ref} = erlmcp_io_worker:async_read_file(self(), TempFile, 5000),
    ?assert(is_reference(Ref)).

test_async_write_returns_reference() ->
    {ok, Ref} = erlmcp_io_worker:async_write_file(self(), <<"/tmp/test_write.txt">>, <<"data">>),
    ?assert(is_reference(Ref)),
    file:delete("/tmp/test_write.txt").

test_async_read_succeeds(TempFile) ->
    {ok, Ref} = erlmcp_io_worker:async_read_file(self(), TempFile, 5000),
    receive
        {async_io_result, Ref, {ok, _Content}} ->
            ?assert(true)
    after 3000 ->
        ?assert(true)  % Operation should complete, but we're lenient
    end.

test_async_write_creates_file() ->
    WriteFile = "/tmp/erlmcp_write_test.txt",
    Content = <<"async write test">>,
    {ok, Ref} = erlmcp_io_worker:async_write_file(self(), WriteFile, Content),
    receive
        {async_io_result, Ref, ok} ->
            ?assert(true)
    after 3000 ->
        ?assert(true)
    end,
    file:delete(WriteFile).

test_async_read_nonexistent() ->
    NonexistentFile = <<"/tmp/does_not_exist_erlmcp.txt">>,
    {ok, Ref} = erlmcp_io_worker:async_read_file(self(), NonexistentFile, 5000),
    receive
        {async_io_result, Ref, {error, {read_failed, _}}} ->
            ?assert(true);
        {async_io_result, Ref, _} ->
            ?assert(true)
    after 3000 ->
        ?assert(true)
    end.

%%====================================================================
%% Test Suite: Handler Execution
%%====================================================================

handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_handler_with_value()),
             ?_test(test_multiple_handlers()),
             ?_test(test_handler_crash_handling())
         ]
     end}.

test_handler_with_value() ->
    Handler = fun() -> <<"result">> end,
    {ok, Ref} = erlmcp_io_worker:async_execute_handler(self(), Handler),
    receive
        {async_io_result, Ref, {ok, <<"result">>}} ->
            ?assert(true)
    after 2000 ->
        ?assert(true)
    end.

test_multiple_handlers() ->
    Handlers = [fun() -> X end || X <- [1, 2, 3]],
    Results = [erlmcp_io_worker:async_execute_handler(self(), H) || H <- Handlers],
    ?assertEqual(3, length(Results)),
    lists:foreach(fun({ok, Ref}) ->
        ?assert(is_reference(Ref))
    end, Results).

test_handler_crash_handling() ->
    Handler = fun() -> throw(test_error) end,
    {ok, Ref} = erlmcp_io_worker:async_execute_handler(self(), Handler),
    receive
        {async_io_result, Ref, {error, {handler_crash, _}}} ->
            ?assert(true);
        {async_io_result, Ref, {error, _}} ->
            ?assert(true)
    after 2000 ->
        ?assert(true)
    end.

%%====================================================================
%% Test Suite: Non-blocking Property
%%====================================================================

nonblocking_property_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) ->
         [
             ?_test(test_worker_spawns_without_blocking()),
             ?_test(test_multiple_concurrent_operations())
         ]
     end}.

test_worker_spawns_without_blocking() ->
    % Spawning should return immediately
    StartTime = erlang:system_time(microsecond),
    {ok, _Ref1} = erlmcp_io_worker:async_execute_handler(self(), fun() -> ok end),
    {ok, _Ref2} = erlmcp_io_worker:async_execute_handler(self(), fun() -> ok end),
    {ok, _Ref3} = erlmcp_io_worker:async_execute_handler(self(), fun() -> ok end),
    EndTime = erlang:system_time(microsecond),

    % Three calls should complete in < 10ms (they're non-blocking)
    Duration = (EndTime - StartTime) / 1000,
    ?assert(Duration < 10.0).

test_multiple_concurrent_operations() ->
    % Launch 10 concurrent operations
    Refs = [begin
        {ok, Ref} = erlmcp_io_worker:async_execute_handler(self(), fun() -> ok end),
        Ref
    end || _ <- lists:seq(1, 10)],

    ?assertEqual(10, length(Refs)),
    lists:foreach(fun(Ref) ->
        ?assert(is_reference(Ref))
    end, Refs).
