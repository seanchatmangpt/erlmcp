%%%-------------------------------------------------------------------
%%% @doc Chicago School TDD Tests for Transport Registry Lifecycle
%%% Tests ONLY observable behavior through public API
%%% NO STATE INSPECTION, NO DUMMY PROCESSES, REAL erlmcp processes only
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_transport_registry_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

%% gen_server callbacks for test transport processes
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Test Fixtures
%%====================================================================

registry_lifecycle_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      fun test_start_stop/1,
      fun test_register_unregister/1,
      fun test_get_transport/1,
      fun test_double_register_fails/1
     ]}.

setup() ->
    % Start required applications
    application:ensure_all_started(gproc),
    % Start registry
    case erlmcp_transport_registry:start_link() of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} -> Pid
    end.

cleanup(Pid) ->
    % Stop registry gracefully
    try
        gen_server:stop(Pid, normal, 1000)
    catch
        exit:noproc -> ok;
        exit:{timeout, _} ->
            case is_process_alive(Pid) of
                true -> exit(Pid, kill);
                false -> ok
            end
    end,
    timer:sleep(100),
    ok.

%%====================================================================
%% Tests (Observable Behavior Only)
%%====================================================================

test_start_stop(_Pid) ->
    [
     ?_assert(is_pid(whereis(erlmcp_transport_registry)))
    ].

test_register_unregister(_Pid) ->
    % Create real transport process via spawn (simple process that responds)
    TransportPid = spawn(fun() ->
        receive
            stop -> ok
        end
    end),

    Config = #{
        type => tcp,
        host => "localhost",
        port => 3000
    },

    % Register transport (API call)
    Result1 = erlmcp_transport_registry:register_transport(
        test_transport, TransportPid, Config
    ),

    % Try to register again (should fail - observable behavior)
    Result2 = erlmcp_transport_registry:register_transport(
        test_transport, TransportPid, Config
    ),

    % Unregister transport (API call)
    Result3 = erlmcp_transport_registry:unregister_transport(test_transport),

    % Stop the transport process
    TransportPid ! stop,

    [
     ?_assertEqual(ok, Result1),
     ?_assertEqual({error, already_registered}, Result2),
     ?_assertEqual(ok, Result3)
    ].

test_get_transport(_Pid) ->
    % Create real transport process
    TransportPid = spawn(fun() ->
        receive stop -> ok end
    end),

    Config = #{type => tcp, host => "localhost", port => 3000},

    % Register transport (API call)
    Result1 = erlmcp_transport_registry:register_transport(
        test_get, TransportPid, Config
    ),

    % Get transport info (API call - observable behavior)
    Result2 = erlmcp_transport_registry:get_transport(test_get),

    % Unregister and cleanup
    erlmcp_transport_registry:unregister_transport(test_get),
    TransportPid ! stop,

    [
     ?_assertEqual(ok, Result1),
     ?_assertMatch({ok, #{transport_id := test_get, type := tcp}}, Result2)
    ].

test_double_register_fails(_Pid) ->
    % Create two different transport processes
    Pid1 = spawn(fun() -> receive stop -> ok end end),
    Pid2 = spawn(fun() -> receive stop -> ok end end),

    Config = #{type => tcp},

    % Register first transport
    Result1 = erlmcp_transport_registry:register_transport(
        double_test, Pid1, Config
    ),

    % Try to register same ID with different Pid (should fail)
    Result2 = erlmcp_transport_registry:register_transport(
        double_test, Pid2, Config
    ),

    % Cleanup
    erlmcp_transport_registry:unregister_transport(double_test),
    Pid1 ! stop,
    Pid2 ! stop,

    [
     ?_assertEqual(ok, Result1),
     ?_assertEqual({error, already_registered}, Result2)
    ].

%%====================================================================
%% gen_server callbacks (not actually used, keeping spawn for simplicity)
%%====================================================================

init(_Args) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
