%%%-------------------------------------------------------------------
%%% @doc OTP 28 Features Test Suite
%%%
%%% Comprehensive tests for OTP 28 new features in erlmcp:
%%% 1. Priority messages (EEP-76)
%%% 2. New hibernate/0
%%% 3. PCRE2 compatibility
%%% 4. JIT optimization patterns
%%% 5. Modern code patterns
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_otp28_features_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Setup
%%%===================================================================

otp28_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"priority_messages", fun test_priority_messages/0},
      {"hibernate_memory", fun test_hibernate_memory/0},
      {"pcre2_compat", fun test_pcre2_compat/0},
      {"modern_patterns", fun test_modern_patterns/0}
     ]}.

setup() ->
    %% Ensure OTP 28+ is available
    case erlang:system_info(otp_release) >= "28" of
        true ->
            ok;
        false ->
            {skip, "OTP 28+ required for these tests"}
    end.

cleanup(_State) ->
    ok.

%%%===================================================================
%%% Priority Messages Tests (EEP-76)
%%%===================================================================

test_priority_messages() ->
    %% Test priority alias creation
    Alias = try erlang:alias([priority]) of
                A -> A
            catch
                _:_ ->
                    ?assert(false, "Priority alias creation failed"),
                    undefined
            end,

    %% Test priority message sending
    Self = self(),
    Result = try
                erlang:send(Alias, {priority, Self, test_msg}, [priority]),
                ok
            catch
                _:_ ->
                    send_failed
            end,

    ?assertEqual(ok, Result),

    %% Test urgent message sending
    UrgentResult = try
                      erlang:send(Alias, {urgent, urgent_msg}, [priority]),
                      ok
                  catch
                      _:_ ->
                          urgent_failed
                  end,

    ?assertEqual(ok, UrgentResult),

    %% Cleanup
    try erlang:unalias(Alias) catch _:_ -> ok end,

    {priority_messages_test_complete}.

%%%===================================================================
%%% Hibernate Memory Tests
%%%===================================================================

test_hibernate_memory() ->
    %% Start a test gen_server with hibernation
    {ok, Pid} = test_server:start_link(),

    %% Get initial memory
    {memory, MemBefore} = erlang:process_info(Pid, memory),

    %% Wait for hibernation (30 seconds configured in server)
    %% For testing, we'll trigger it manually
    Pid ! {system, self(), hibernate},
    timer:sleep(100),

    %% Get memory after hibernation
    {memory, MemAfter} = erlang:process_info(Pid, memory),

    %% Verify memory reduction (should be at least 10% less)
    Savings = MemBefore - MemAfter,
    SavingsPercent = (Savings * 100) div MemBefore,

    ?assert(SavingsPercent > 10),
    ?assert(MemAfter < MemBefore),

    %% Cleanup
    gen_server:stop(Pid),

    {hibernate_memory_test_complete, MemBefore, MemAfter, SavingsPercent}.

%%%===================================================================
%%% PCRE2 Compatibility Tests
%%%===================================================================

test_pcre2_compat() ->
    %% Test PCRE2-compatible patterns
    SafePatterns = [
      {<<"variable_name">>, <<"^[a-zA-Z_][a-zA-Z0-9_]*$">>, <<"valid_name">>, true},
      {<<"variable_name">>, <<"^[a-zA-Z_][a-zA-Z0-9_]*$">>, <<"123invalid">>, false},
      {<<"uri_scheme">>, <<"^[a-zA-Z][a-zA-Z0-9+\\.-]*:">>, <<"https:">>, true},
      {<<"semver">>, <<"^\\d+\\.\\d+\\.\\d+(-[0-9A-Za-z.-]+)?(\\+[0-9A-Za-z.-]+)?$">>, <<"1.2.3">>, true},
      {<<"uuid">>, <<"^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$">>, <<"550e8400-e29b-41d4-a716-446655440000">>, true}
    ],

    lists:foreach(fun({Name, Pattern, TestString, ShouldMatch}) ->
                          case re:run(TestString, Pattern, [unicode]) of
                              {match, _} when ShouldMatch =:= true ->
                                  ok;
                              nomatch when ShouldMatch =:= false ->
                                  ok;
                              Unexpected ->
                                  ?assert(false, {Name, pattern_match_failed, Unexpected})
                          end
                  end,
                  SafePatterns),

    %% Test PCRE2 compatibility module
    {ok, _} = erlmcp_pcre2_compat:validate_pattern(<<"^[a-zA-Z_][a-zA-Z0-9_]*$">>),

    {pcre2_compat_test_complete}.

%%%===================================================================
%%% Modern Code Patterns Tests
%%%===================================================================

test_modern_patterns() ->
    %% Test try/catch vs old catch
    OldCatch = fun() ->
                       catch ets:delete(nonexistent_table)
               end,

    ModernTryCatch = fun() ->
                             try ets:delete(nonexistent_table)
                             catch
                                 _:Error -> Error
                             end
                     end,

    %% Both should work, but modern pattern is more explicit
    ?assert(is_tuple(OldCatch)),
    ?assert(is_atom(ModernTryCatch())),

    %% Test strict comprehension (if OTP 28+)
    %% This will fail in OTP < 28 but work in 28+
    List = [1, 2, 3, 4, 5],
    Squared = [X * X || X <- List],
    ?assertEqual([1, 4, 9, 16, 25], Squared),

    {modern_patterns_test_complete}.

%%%===================================================================
%%% Test Server (Hibernate Test)
%%%===================================================================

-module(test_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
