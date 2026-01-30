%%%-----------------------------------------------------------------------------
%%% @doc Tests for erlmcp_debugger
%%% @end
%%%-----------------------------------------------------------------------------
-module(erlmcp_debugger_tests).

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% TEST FIXTURES
%%%=============================================================================

debugger_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
      {"Attach to process", fun test_attach/0},
      {"Inspect state", fun test_inspect_state/0},
      {"Trace calls", fun test_trace_calls/0},
      {"Trace messages", fun test_trace_messages/0},
      {"List attached processes", fun test_list_attached/0},
      {"Detach from process", fun test_detach/0}
     ]}.

setup() ->
    application:ensure_all_started(erlmcp_observability),
    %% Start inline test gen_server
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    register(test_server, Pid),
    Pid.

cleanup(Pid) ->
    catch gen_server:stop(Pid),
    %% Cleanup any attached processes
    [erlmcp_debugger:detach(P) || #{pid := P} <- erlmcp_debugger:list_attached()],
    ok.

%%%=============================================================================
%%% ATTACHMENT TESTS
%%%=============================================================================

test_attach() ->
    Pid = whereis(test_server),
    {ok, AttachedPid} = erlmcp_debugger:attach(test_server),
    
    ?assertEqual(Pid, AttachedPid),
    
    %% Verify it's in the list
    Attached = erlmcp_debugger:list_attached(),
    ?assert(lists:any(fun(#{pid := P}) -> P == Pid end, Attached)).

test_inspect_state() ->
    Pid = whereis(test_server),
    {ok, _} = erlmcp_debugger:attach(Pid),
    
    {ok, Snapshot} = erlmcp_debugger:inspect_state(Pid),
    
    ?assertMatch(#{
        timestamp := _,
        process_info := _,
        state := _
    }, Snapshot).

test_list_attached() ->
    Pid = whereis(test_server),
    
    %% Initially empty
    [] = erlmcp_debugger:list_attached(),
    
    %% Attach
    {ok, _} = erlmcp_debugger:attach(Pid),
    
    %% Should have one
    Attached = erlmcp_debugger:list_attached(),
    ?assertEqual(1, length(Attached)),
    
    [Info] = Attached,
    ?assertMatch(#{pid := Pid}, Info).

test_detach() ->
    Pid = whereis(test_server),
    {ok, _} = erlmcp_debugger:attach(Pid),
    
    %% Detach
    ok = erlmcp_debugger:detach(Pid),
    
    %% Should be empty now
    [] = erlmcp_debugger:list_attached().

%%%=============================================================================
%%% TRACING TESTS
%%%=============================================================================

test_trace_calls() ->
    %% Trace a simple function
    {ok, Ref} = erlmcp_debugger:trace_calls(lists, seq, 2),
    
    ?assert(is_reference(Ref)),
    
    %% Generate some calls
    _ = lists:seq(1, 10),
    _ = lists:seq(1, 20),
    
    timer:sleep(100),
    
    %% Stop and get results
    {ok, Traces} = erlmcp_debugger:stop_trace(Ref),
    
    ?assert(is_list(Traces)).

test_trace_messages() ->
    Pid = whereis(test_server),
    
    %% Start message trace
    {ok, Ref} = erlmcp_debugger:trace_messages(Pid, 500),
    
    ?assert(is_reference(Ref)),
    
    %% Send some messages
    gen_server:cast(Pid, test_message_1),
    gen_server:cast(Pid, test_message_2),
    
    timer:sleep(600),
    
    %% Get results
    {ok, Traces} = erlmcp_debugger:stop_trace(Ref),
    
    ?assert(is_list(Traces)).

%%%=============================================================================
%%% TEST GEN_SERVER (embedded helper module commented out - use separate file)
%%%=============================================================================

%% NOTE: This should be in a separate file: test_server.erl
%% Keeping it commented here for reference

% start_link() ->
%     gen_server:start_link({local, test_server}, test_server_module, [], []).

% init([]) ->
%     {ok, #{counter => 0}}.
%
% handle_call(_Request, _From, State) ->
%     {reply, ok, State}.
%
% handle_cast(_Msg, State = #{counter := N}) ->
%     {noreply, State#{counter => N + 1}}.
%
% handle_info(_Info, State) ->
%     {noreply, State}.
%
% terminate(_Reason, _State) ->
%     ok.

%%%=============================================================================
%%% GEN_SERVER CALLBACKS (inline test server)
%%%=============================================================================

%% These callbacks make this module a test gen_server

init([]) ->
    {ok, #{counter => 0}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State = #{counter := N}) ->
    {noreply, State#{counter => N + 1}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
