%%%====================================================================
%%% @doc OTP 28 Zip Generator Tests - Chicago School TDD
%%%
%%% Tests follow Chicago School TDD methodology:
%%% - Tests drive behavior (not implementation details)
%%% - Observable behavior through API
%%% - Real processes, no mocks
%%% - Property-based testing with Proper
%%%
%%% @end
%%%====================================================================

-module(erlmcp_zip_utils_tests).

-include_lib("eunit/include/eunit.hrl").
%% Proper tests disabled for basic compilation
%% -include_lib("proper/include/proper.hrl").

%%%====================================================================
%%% Test Helpers
%%%====================================================================

%% @doc Setup test fixture with sample data
setup() ->
    {TestPids, TestRefs} = spawn_test_processes(3),
    {TestPids, TestRefs}.

%% @doc Cleanup test processes
cleanup({Pids, _Refs}) ->
    [exit(Pid, kill) || Pid <- Pids],
    ok.

%% @doc Spawn test processes that echo messages back
spawn_test_processes(Count) ->
    Parent = self(),
    {Pids, Refs} =
        lists:unzip(
            [begin
                 Ref = make_ref(),
                 Pid = spawn_link(fun() ->
                                           test_process_loop(Parent, Ref)
                                   end),
                 {Pid, Ref}
             end
             || _N <- lists:seq(1, Count)]),
    {Pids, Refs}.

%% @doc Test process loop that echoes cancel messages
test_process_loop(Parent, Ref) ->
    receive
        {cancel, Ref} ->
            Parent ! {cancelled, Ref, self()},
            test_process_loop(Parent, Ref);
        {get_state, Ref, From} ->
            From ! {Ref, {state, self()}},
            test_process_loop(Parent, Ref);
        stop ->
            ok
    end.

%%%====================================================================
%%% Unit Tests
%%%====================================================================

%%--------------------------------------------------------------------
%%% @doc Test cancel_tools with matching PIDs and Refs
%%--------------------------------------------------------------------
cancel_tools_basic_test() ->
    {Pids, Refs} = setup(),
    try
        Result = erlmcp_zip_utils:cancel_tools(Pids, Refs),
        ?assertEqual(ok, Result),

        %% Verify all processes received cancel messages
        [begin
             receive
                 {cancelled, Ref, Pid} ->
                     ?assert(lists:member(Pid, Pids)),
                     ?assert(lists:member(Ref, Refs))
             after 1000 ->
                      ?assert(false, "Timeout waiting for cancel message")
             end
         end
         || _ <- Pids],
        ok
    after
        cleanup({Pids, Refs})
    end.

%%--------------------------------------------------------------------
%%% @doc Test cancel_tools with length mismatch
%%--------------------------------------------------------------------
cancel_tools_length_mismatch_test() ->
    Pids = [self(), self()],
    Refs = [make_ref()],
    Result = erlmcp_zip_utils:cancel_tools(Pids, Refs),
    ?assertEqual({error, length_mismatch}, Result).

%%--------------------------------------------------------------------
%%% @doc Test cancel_tools with empty lists
%%--------------------------------------------------------------------
cancel_tools_empty_test() ->
    Result = erlmcp_zip_utils:cancel_tools([], []),
    ?assertEqual(ok, Result).

%%--------------------------------------------------------------------
%%% @doc Test collect_results with matching refs
%%--------------------------------------------------------------------
collect_results_basic_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    ExpectedRefs = [Ref1, Ref2, Ref3],
    ActualResults = [{Ref1, {ok, data1}}, {Ref2, {ok, data2}}, {Ref3, {ok, data3}}],

    Result = erlmcp_zip_utils:collect_results(ExpectedRefs, ActualResults),
    ?assertMatch({ok, _}, Result),
    {ok, Matched} = Result,
    ?assertEqual(3, length(Matched)),
    ?assertEqual({ok, data1}, proplists:get_value(Ref1, Matched)),
    ?assertEqual({ok, data2}, proplists:get_value(Ref2, Matched)),
    ?assertEqual({ok, data3}, proplists:get_value(Ref3, Matched)).

%%--------------------------------------------------------------------
%%% @doc Test collect_results filters non-matching results
%%--------------------------------------------------------------------
collect_results_filtering_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),
    Ref3 = make_ref(),

    ExpectedRefs = [Ref1, Ref3],
    ActualResults = [{Ref1, {ok, data1}}, {Ref2, {ok, data2}}, {Ref3, {ok, data3}}],

    Result = erlmcp_zip_utils:collect_results(ExpectedRefs, ActualResults),
    ?assertMatch({ok, _}, Result),
    {ok, Matched} = Result,
    ?assertEqual(2, length(Matched)),
    ?assertEqual({ok, data1}, proplists:get_value(Ref1, Matched)),
    ?assertEqual({ok, data3}, proplists:get_value(Ref3, Matched)).

%%--------------------------------------------------------------------
%%% @doc Test collect_results with length mismatch
%%--------------------------------------------------------------------
collect_results_length_mismatch_test() ->
    Ref1 = make_ref(),
    Ref2 = make_ref(),

    ExpectedRefs = [Ref1, Ref2],
    ActualResults = [{Ref1, {ok, data1}}],

    Result = erlmcp_zip_utils:collect_results(ExpectedRefs, ActualResults),
    ?assertEqual({error, length_mismatch}, Result).

%%--------------------------------------------------------------------
%%% @doc Test assign_sequence with chunks
%%--------------------------------------------------------------------
assign_sequence_basic_test() ->
    Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
    Result = erlmcp_zip_utils:assign_sequence(Chunks),

    ?assertEqual(3, length(Result)),
    ?assertEqual([{1, <<"chunk1">>}, {2, <<"chunk2">>}, {3, <<"chunk3">>}], Result).

%%--------------------------------------------------------------------
%%% @doc Test assign_sequence with empty list
%%--------------------------------------------------------------------
assign_sequence_empty_test() ->
    Chunks = [],
    Result = erlmcp_zip_utils:assign_sequence(Chunks),
    ?assertEqual([], Result).

%%--------------------------------------------------------------------
%%% @doc Test zip_with function
%%--------------------------------------------------------------------
zip_with_basic_test() ->
    List1 = [1, 2, 3],
    List2 = [a, b, c],
    Result = erlmcp_zip_utils:zip_with(fun(X, Y) -> {X, Y} end, List1, List2),

    ?assertEqual([{1, a}, {2, b}, {3, c}], Result).

%%--------------------------------------------------------------------
%%% @doc Test zip_with arithmetic
%%--------------------------------------------------------------------
zip_with_arithmetic_test() ->
    List1 = [1, 2, 3],
    List2 = [10, 20, 30],
    Result = erlmcp_zip_utils:zip_with(fun(X, Y) -> X + Y end, List1, List2),

    ?assertEqual([11, 22, 33], Result).

%%--------------------------------------------------------------------
%%% @doc Test zip_with length mismatch
%%--------------------------------------------------------------------
zip_with_length_mismatch_test() ->
    List1 = [1, 2, 3],
    List2 = [a, b],
    ?assertError(badarg, erlmcp_zip_utils:zip_with(fun(X, Y) -> {X, Y} end, List1, List2)).

%%--------------------------------------------------------------------
%%% @doc Test parallel_pair_process
%%--------------------------------------------------------------------
parallel_pair_process_basic_test() ->
    {Pids, Refs} = setup(),
    try
        Fun = fun(Pid, Ref) ->
                  Pid ! {get_state, Ref, self()},
                  receive
                      {Ref, State} -> State
                  after 1000 ->
                          {error, timeout}
                  end
              end,

        Pairs = lists:zip(Pids, Refs),
        Result = erlmcp_zip_utils:parallel_pair_process(Fun, Pairs),

        ?assertMatch({ok, _}, Result),
        {ok, Results} = Result,
        ?assertEqual(3, length(Results)),
        lists:foreach(fun(Res) -> ?assertMatch({state, _}, Res) end, Results)
    after
        cleanup({Pids, Refs})
    end.

%%--------------------------------------------------------------------
%%% @doc Test safe_zip with matching lengths
%%--------------------------------------------------------------------
safe_zip_basic_test() ->
    List1 = [1, 2, 3],
    List2 = [a, b, c],
    Result = erlmcp_zip_utils:safe_zip(List1, List2),

    ?assertMatch({ok, _}, Result),
    {ok, Zipped} = Result,
    ?assertEqual([{1, a}, {2, b}, {3, c}], Zipped).

%%--------------------------------------------------------------------
%%% @doc Test safe_zip with mismatched lengths
%%--------------------------------------------------------------------
safe_zip_length_mismatch_test() ->
    List1 = [1, 2, 3],
    List2 = [a, b],
    Result = erlmcp_zip_utils:safe_zip(List1, List2),

    ?assertEqual({error, length_mismatch}, Result).

%%--------------------------------------------------------------------
%%% @doc Test zip_map
%%--------------------------------------------------------------------
zip_map_basic_test() ->
    Keys = [a, b, c],
    Values = [1, 2, 3],
    Result = erlmcp_zip_utils:zip_map(Keys, Values),

    ?assertEqual(#{a => 1, b => 2, c => 3}, Result).

%%--------------------------------------------------------------------
%%% @doc Test zip_filter
%%--------------------------------------------------------------------
zip_filter_basic_test() ->
    List1 = [1, 2, 3, 4],
    List2 = [a, b, c, d],
    Result = erlmcp_zip_utils:zip_filter(fun(X, _Y) -> X rem 2 =:= 0 end, List1, List2),

    ?assertEqual([{2, b}, {4, d}], Result).

%%--------------------------------------------------------------------
%%% @doc Test batch_send
%%--------------------------------------------------------------------
batch_send_basic_test() ->
    {Pids, _Refs} = setup(),
    try
        Messages = [msg1, msg2, msg3],
        Result = erlmcp_zip_utils:batch_send(Pids, Messages),

        ?assertEqual(ok, Result),

        %% Verify all processes received messages
        [begin
             receive
                 Msg ->
                     ?assert(lists:member(Msg, Messages))
             after 1000 ->
                      ?assert(false, "Timeout waiting for message")
             end
         end
         || Pid <- Pids]
    after
        cleanup({Pids, []})
    end.

%%--------------------------------------------------------------------
%%% @doc Test batch_send with length mismatch
%%--------------------------------------------------------------------
batch_send_length_mismatch_test() ->
    Pids = [self()],
    Messages = [msg1, msg2],
    Result = erlmcp_zip_utils:batch_send(Pids, Messages),

    ?assertEqual({error, length_mismatch}, Result).

%%--------------------------------------------------------------------
%%% @doc Test pair_with_index
%%--------------------------------------------------------------------
pair_with_index_basic_test() ->
    Items = [a, b, c],
    Result = erlmcp_zip_utils:pair_with_index(Items),

    ?assertEqual([{1, a}, {2, b}, {3, c}], Result).

%%--------------------------------------------------------------------
%%% @doc Test pair_with_index with empty list
%%--------------------------------------------------------------------
pair_with_index_empty_test() ->
    Items = [],
    Result = erlmcp_zip_utils:pair_with_index(Items),

    ?assertEqual([], Result).

%%%====================================================================
%%% Property-Based Tests (Proper)
%%%====================================================================
%% NOTE: Property-based tests disabled for basic compilation
%% Enable by uncommenting -include_lib("proper/include/proper.hrl") above

%% prop_zip_with_preserves_length() ->
%%     ?FORALL({L1, L2},
%%             {non_empty(list(int())), non_empty(list(int()))},
%%             begin
%%                 MinLen = min(length(L1), length(L2)),
%%                 TruncatedL1 = lists:sublist(L1, MinLen),
%%                 TruncatedL2 = lists:sublist(L2, MinLen),
%%
%%                 Result = erlmcp_zip_utils:zip_with(fun(X, Y) -> {X, Y} end,
%%                                                     TruncatedL1,
%%                                                     TruncatedL2),
%%                 length(Result) =:= MinLen
%%             end).
%%
%% prop_assign_sequence_sequential() ->
%%     ?FORALL(List, list(term()),
%%             begin
%%                 Result = erlmcp_zip_utils:assign_sequence(List),
%%                 Indices = [I || {I, _} <- Result],
%%                 Indices =:= lists:seq(1, length(List))
%%             end).
%%
%% prop_safe_zip_length_mismatch() ->
%%     ?FORALL({L1, L2},
%%             ?LET({Len1, Len2},
%%                  {int(), int()},
%%                  {list(int(Len1)), list(int(Len2))}),
%%             begin
%%                 case length(L1) =:= length(L2) of
%%                     true ->
%%                         {ok, Zipped} = erlmcp_zip_utils:safe_zip(L1, L2),
%%                         length(Zipped) =:= length(L1);
%%                     false ->
%%                         {error, length_mismatch} =:= erlmcp_zip_utils:safe_zip(L1, L2)
%%                 end
%%             end).
%%
%% prop_zip_map_complete() ->
%%     ?FORALL({Keys, Values},
%%             {non_empty(list(atom())), non_empty(list(int()))},
%%             begin
%%                 MinLen = min(length(Keys), length(Values)),
%%                 TruncatedKeys = lists:sublist(Keys, MinLen),
%%                 TruncatedValues = lists:sublist(Values, MinLen),
%%
%%                 Result = erlmcp_zip_utils:zip_map(TruncatedKeys, TruncatedValues),
%%                 maps:size(Result) =:= MinLen andalso
%%                 lists:all(fun(K) -> maps:is_key(K, Result) end, TruncatedKeys)
%%             end).
%%
%% prop_pair_with_index_sequential() ->
%%     ?FORALL(List, list(term()),
%%             begin
%%                 Result = erlmcp_zip_utils:pair_with_index(List),
%%                 Indices = [I || {I, _} <- Result],
%%                 Indices =:= lists:seq(1, max(0, length(List))) andalso
%%                 length(Result) =:= length(List)
%%             end).
%%
%%%====================================================================
%%% Test Generators
%%%====================================================================
%%
%% non_empty(List) ->
%%     ?SUCHTHAT(L, List, L =/= []).
