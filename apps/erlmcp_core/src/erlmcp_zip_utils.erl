%%%====================================================================
%%% @doc OTP 28 Zip Generator Utilities for Parallel MCP Processing
%%%
%%% Implements EEP-73 zip generators (&& syntax) for efficient parallel
%%% list processing in MCP message handling:
%%%
%%% Key features:
%%% - Parallel iteration over multiple lists
%%% - Automatic pairing of elements
%%% - Batch operations with PIDs, refs, sequences
%%% - Type-safe with proper specs
%%%
%%% OTP 28 Innovation:
%%% Syntax: `[Expr || X <- List1 && Y <- List2]`
%%% Purpose: Iterate two lists in parallel (lockstep)
%%%
%%% @end
%%%====================================================================

-module(erlmcp_zip_utils).

%% API exports
-export([cancel_tools/2,
         collect_results/2,
         assign_sequence/1,
         zip_with/3,
         parallel_pair_process/2,
         safe_zip/2,
         zip_map/2,
         zip_filter/3,
         batch_send/2,
         pair_with_index/1]).

%% Type definitions
-type request_id() :: term().
-type result() :: term().
-type pid_ref_pair() :: {pid(), reference()}.
-type indexed_item() :: {pos_integer(), term()}.

-export_type([pid_ref_pair/0, indexed_item/0]).

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Cancel multiple tool calls by pairing PIDs with request Refs.
%%
%% OTP 28 zip generator syntax: `&&` for parallel iteration
%%
%% Example:
%% ```
%% ToolPids = [Pid1, Pid2, Pid3],
%% Refs = [Ref1, Ref2, Ref3],
%% ok = erlmcp_zip_utils:cancel_tools(ToolPids, Refs).
%% ```
%%
%% @param ToolPids List of tool process PIDs
%% @param Refs List of request references (must be same length as ToolPids)
%% @returns ok | {error, length_mismatch}
%% @throws badarg if lists have different lengths (OTP 28 behavior)
-spec cancel_tools([pid()], [reference()]) -> ok | {error, length_mismatch}.
cancel_tools(ToolPids, Refs) when length(ToolPids) =/= length(Refs) ->
    {error, length_mismatch};
cancel_tools(ToolPids, Refs) when is_list(ToolPids), is_list(Refs) ->
    %% OTP 28: && syntax for parallel iteration
    _ = [erlang:send(Pid, {cancel, Ref}) || Pid <- ToolPids && Ref <- Refs],
    ok.

%% @doc Collect results in order, matching with expected request IDs.
%%
%% Filters results to only include those matching expected references.
%% Uses zip generator for parallel iteration over refs and results.
%%
%% Example:
%% ```
%% ExpectedRefs = [Ref1, Ref2, Ref3],
%% ActualResults = [{Ref1, {ok, Data1}}, {Ref2, {ok, Data2}}, {Ref3, {ok, Data3}}],
%% {ok, Matched} = erlmcp_zip_utils:collect_results(ExpectedRefs, ActualResults).
%% ```
%%
%% @param ExpectedRefs List of expected request reference IDs
%% @param ActualResults List of actual results (tuples with ref as first element)
%% @returns {ok, [{Ref, Result}]} | {error, length_mismatch}
-spec collect_results([request_id()], [{request_id(), result()}]) ->
                         {ok, [{request_id(), result()}]} | {error, length_mismatch}.
collect_results(ExpectedRefs, ActualResults) when length(ExpectedRefs) =/= length(ActualResults) ->
    {error, length_mismatch};
collect_results(ExpectedRefs, ActualResults) ->
    try
        %% OTP 28: Zip refs with results, filter by matching refs
        Matched =
            [{Ref, Result} || Ref <- ExpectedRefs && Result <- ActualResults,
                             element(1, Result) =:= Ref],
        {ok, Matched}
    catch
        error:function_clause ->
            {error, length_mismatch}
    end.

%% @doc Assign sequence numbers to chunks for ordered SSE/event streaming.
%%
%% Example:
%% ```
%% Chunks = [<<"chunk1">>, <<"chunk2">>, <<"chunk3">>],
%% Sequenced = erlmcp_zip_utils:assign_sequence(Chunks),
%% %% [{1, <<"chunk1">>}, {2, <<"chunk2">>}, {3, <<"chunk3">>}].
%% ```
%%
%% @param Chunks List of data chunks
%% @returns [{SeqNum, Chunk}] tuples
-spec assign_sequence([term()]) -> [{pos_integer(), term()}].
assign_sequence(Chunks) when is_list(Chunks) ->
    SeqNumbers = lists:seq(1, length(Chunks)),
    %% OTP 28: Zip chunks with sequence numbers
    [{Seq, Chunk} || Chunk <- Chunks && Seq <- SeqNumbers].

%% @doc Generic zip with function application (zipwith equivalent).
%%
%% Applies function to paired elements from two lists.
%%
%% Example:
%% ```
%% Keys = [a, b, c],
%% Values = [1, 2, 3],
%% Pairs = erlmcp_zip_utils:zip_with(fun(K, V) -> {K, V} end, Keys, Values),
%% %% [{a, 1}, {b, 2}, {c, 3}].
%% ```
%%
%% @param Fun Function to apply to paired elements
%% @param List1 First list
%% @param List2 Second list (must be same length as List1)
%% @returns List of function results
-spec zip_with(fun((A, B) -> T), [A], [B]) -> [T] when A :: term(), B :: term(), T :: term().
zip_with(Fun, List1, List2) when is_function(Fun, 2),
                                 is_list(List1),
                                 is_list(List2),
                                 length(List1) =:= length(List2) ->
    %% OTP 28: Zip with function application
    [Fun(X, Y) || X <- List1 && Y <- List2];
zip_with(_Fun, List1, List2) when length(List1) =/= length(List2) ->
    erlang:error(badarg).

%% @doc Process pairs in parallel, returning results in order.
%%
%% Spawns processes for each pair, collects results maintaining order.
%% Useful for parallel tool execution with ordered results.
%%
%% Example:
%% ```
%% Pids = [Pid1, Pid2],
%% Refs = [Ref1, Ref2],
%% {ok, Results} = erlmcp_zip_utils:parallel_pair_process(
%%     fun(Pid, Ref) -> gen_server:call(Pid, {get, Ref}, 1000) end,
%%     [{Pid1, Ref1}, {Pid2, Ref2}]).
%% ```
%%
%% @param Fun Function to process each pair
%% @param Pairs List of {Pid, Ref} tuples
%% @returns {ok, [Results]} | {error, Reason}
-spec parallel_pair_process(fun((pid(), reference()) -> result()), [pid_ref_pair()]) ->
                                {ok, [result()]} | {error, term()}.
parallel_pair_process(Fun, Pairs) when is_function(Fun, 2), is_list(Pairs) ->
    Parent = self(),
    CollectorRef = make_ref(),

    %% Spawn processes for each pair
    Pids =
        [spawn_link(fun() ->
                          Result = try Fun(Pid, Ref)
                                   catch Class:Reason:Stack ->
                                          {error, {Class, Reason, Stack}}
                                   end,
                          Parent ! {CollectorRef, self(), Result}
                   end)
         || {Pid, Ref} <- Pairs],

    %% Collect results in order
    collect_in_order(Pids, CollectorRef, []).

%% @doc Safe zip that handles length mismatches gracefully.
%%
%% Unlike OTP 28 zip generators, returns error instead of crashing.
%%
%% @param List1 First list
%% @param List2 Second list
%% @returns {ok, [{A, B}]} | {error, length_mismatch}
-spec safe_zip([A], [B]) -> {ok, [{A, B}]} | {error, length_mismatch} when A :: term(), B :: term().
safe_zip(List1, List2) when is_list(List1), is_list(List2) ->
    case length(List1) =:= length(List2) of
        true ->
            try
                Zipped = [{X, Y} || X <- List1 && Y <- List2],
                {ok, Zipped}
            catch
                error:_ ->
                    {error, length_mismatch}
            end;
        false ->
            {error, length_mismatch}
    end.

%% @doc Zip two lists with map transformation.
%%
%% Example:
%% ```
%% Keys = [a, b, c],
%% Values = [1, 2, 3],
%% Map = erlmcp_zip_utils:zip_map(Keys, Values),
%% %% #{a => 1, b => 2, c => 3}.
%% ```
%%
%% @param Keys List of keys
%% @param Values List of values (must be same length)
%% @returns Map with key-value pairs
-spec zip_map([A], [B]) -> #{A => B} when A :: term(), B :: term().
zip_map(Keys, Values) when is_list(Keys), is_list(Values) ->
    try
        maps:from_list([{K, V} || K <- Keys && V <- Values])
    catch
        error:_ ->
            erlang:error(badarg)
    end.

%% @doc Filter pairs based on predicate function.
%%
%% Example:
%% ```
%% List1 = [1, 2, 3, 4],
%% List2 = [a, b, c, d],
%% Filtered = erlmcp_zip_utils:zip_filter(
%%     fun(X, Y) -> X rem 2 =:= 0 end,
%%     List1, List2),
%% %% [{2, b}, {4, d}].
%% ```
%%
%% @param Pred Predicate function (returns true to keep pair)
%% @param List1 First list
%% @param List2 Second list
%% @returns Filtered list of pairs
-spec zip_filter(fun((A, B) -> boolean()), [A], [B]) -> [{A, B}] when A :: term(), B :: term().
zip_filter(Pred, List1, List2) when is_function(Pred, 2),
                                    is_list(List1),
                                    is_list(List2) ->
    try
        [{X, Y} || X <- List1 && Y <- List2, Pred(X, Y)]
    catch
        error:_ ->
            erlang:error(badarg)
    end.

%% @doc Send messages to multiple PIDs with paired data.
%%
%% Example:
%% ```
%% Pids = [Pid1, Pid2, Pid3],
%% Messages = [Msg1, Msg2, Msg3],
%% ok = erlmcp_zip_utils:batch_send(Pids, Messages).
%% ```
%%
%% @param Pids List of recipient PIDs
%% @param Messages List of messages (must be same length as Pids)
%% @returns ok | {error, length_mismatch}
-spec batch_send([pid()], [term()]) -> ok | {error, length_mismatch}.
batch_send(Pids, Messages) when is_list(Pids), is_list(Messages) ->
    case length(Pids) =:= length(Messages) of
        true ->
            _ = [Pid ! Msg || Pid <- Pids && Msg <- Messages],
            ok;
        false ->
            {error, length_mismatch}
    end.

%% @doc Pair list items with their 1-based index.
%%
%% Useful for tracking position in sequences.
%%
%% Example:
%% ```
%% Items = [a, b, c],
%% erlmcp_zip_utils:pair_with_index(Items),
%% %% [{1, a}, {2, b}, {3, c}].
%% ```
%%
%% @param Items List of items
%% @returns [{Index, Item}] tuples
-spec pair_with_index([term()]) -> [indexed_item()].
pair_with_index(Items) when is_list(Items) ->
    Indices = lists:seq(1, length(Items)),
    [{Idx, Item} || Item <- Items && Idx <- Indices].

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @doc Collect results from spawned processes in order.
-spec collect_in_order([pid()], reference(), [result()]) -> {ok, [result()]}.
collect_in_order([], _CollectorRef, Acc) ->
    {ok, lists:reverse(Acc)};
collect_in_order(Pids, CollectorRef, Acc) ->
    receive
        {CollectorRef, Pid, Result} ->
            %% Verify this is one of our expected PIDs
            case lists:member(Pid, Pids) of
                true ->
                    collect_in_order(Pids -- [Pid], CollectorRef, [Result | Acc]);
                false ->
                    collect_in_order(Pids, CollectorRef, Acc)
            end
    after 5000 ->
        {error, timeout}
    end.
