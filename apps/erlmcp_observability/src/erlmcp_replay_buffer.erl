%%%-------------------------------------------------------------------
%%% @doc
%%% Experience Replay Buffer for Adaptive Learning
%%%
%%% Implements a prioritized experience replay buffer for storing and
%%% sampling operational experiences. Used for:
%%% - Training from historical trajectories
%%% - Preventing catastrophic forgetting
%%% - Focusing learning on high-impact experiences
%%%
%%% == Sampling Strategies ==
%%%
%%% - uniform: Random sampling (baseline)
%%% - prioritized: Sample by TD-error or reward magnitude
%%% - diversity: Maximize coverage of experience space
%%% - recency: Weight recent experiences higher
%%%
%%% == Priority Calculation ==
%%%
%%% Priority = |reward - 0.5| * (1 + novelty_bonus)
%%%
%%% High priority for:
%%% - Very successful outcomes (reward close to 1.0)
%%% - Clear failures (reward close to 0.0)
%%% - Novel/uncommon patterns
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_replay_buffer).

%% API
-export([new/1, new/2]).
-export([add/2, add_batch/2]).
-export([sample/2, sample/3]).
-export([update_priorities/2]).
-export([size/1, capacity/1, clear/1]).
-export([get_by_id/2]).
-export([serialize/1, deserialize/1]).

-include("erlmcp_learning.hrl").

%%%====================================================================
%%% Type Definitions
%%%====================================================================

-type buffer() :: #replay_buffer{}.
-type sampling_result() :: {ok, [#experience{}], buffer()}.

%%%====================================================================
%%% API Functions
%%%====================================================================

%% @doc Create a new replay buffer
-spec new(pos_integer()) -> buffer().
new(Capacity) ->
    new(Capacity, prioritized).

-spec new(pos_integer(), sampling_strategy()) -> buffer().
new(Capacity, Strategy) ->
    #replay_buffer{
        capacity = Capacity,
        experiences = [],
        priorities = #{},
        sampling_distribution = Strategy,
        total_samples = 0
    }.

%% @doc Add a single experience to the buffer
-spec add(buffer(), #experience{}) -> buffer().
add(Buffer, Experience) ->
    Experiences = Buffer#replay_buffer.experiences,
    Priorities = Buffer#replay_buffer.priorities,

    %% Check capacity
    NewExperiences = case length(Experiences) >= Buffer#replay_buffer.capacity of
        true ->
            %% Remove oldest (FIFO eviction)
            lists:sublist(Experiences, Buffer#replay_buffer.capacity - 1);
        false ->
            Experiences
    end,

    UpdatedExperiences = NewExperiences ++ [Experience],
    UpdatedPriorities = maps:put(Experience#experience.id, Experience#experience.priority, Priorities),

    Buffer#replay_buffer{
        experiences = UpdatedExperiences,
        priorities = UpdatedPriorities,
        total_samples = Buffer#replay_buffer.total_samples + 1
    }.

%% @doc Add multiple experiences at once
-spec add_batch(buffer(), [#experience{}]) -> buffer().
add_batch(Buffer, Batch) ->
    lists:foldl(fun(Exp, Acc) -> add(Acc, Exp) end, Buffer, Batch).

%% @doc Sample experiences from the buffer
-spec sample(buffer(), pos_integer()) -> sampling_result().
sample(Buffer, BatchSize) ->
    sample(Buffer, BatchSize, #{}).

-spec sample(buffer(), pos_integer(), map()) -> sampling_result().
sample(Buffer, BatchSize, Options) ->
    Experiences = Buffer#replay_buffer.experiences,
    Strategy = maps:get(strategy, Options, Buffer#replay_buffer.sampling_distribution),

    case length(Experiences) < BatchSize of
        true ->
            {ok, Experiences, Buffer};
        false ->
            Sampled = case Strategy of
                uniform ->
                    sample_uniform(Experiences, BatchSize);
                prioritized ->
                    sample_prioritized(Experiences, Buffer#replay_buffer.priorities, BatchSize);
                diversity ->
                    sample_diversity(Experiences, BatchSize);
                recency ->
                    sample_recency(Experiences, BatchSize)
            end,
            {ok, Sampled, Buffer}
    end.

%% @doc Update priorities for experiences
-spec update_priorities(buffer(), [{binary(), float()}]) -> buffer().
update_priorities(Buffer, PriorityUpdates) ->
    NewPriorities = lists:foldl(fun({ExpId, Priority}, Acc) ->
        maps:put(ExpId, Priority, Acc)
    end, Buffer#replay_buffer.priorities, PriorityUpdates),

    %% Update experiences with new priorities
    NewExperiences = lists:map(fun(Exp) ->
        case lists:keyfind(Exp#experience.id, 1, PriorityUpdates) of
            false -> Exp;
            {_, NewPriority} -> Exp#experience{priority = NewPriority}
        end
    end, Buffer#replay_buffer.experiences),

    Buffer#replay_buffer{
        experiences = NewExperiences,
        priorities = NewPriorities
    }.

%% @doc Get current buffer size
-spec size(buffer()) -> non_neg_integer().
size(Buffer) ->
    length(Buffer#replay_buffer.experiences).

%% @doc Get buffer capacity
-spec capacity(buffer()) -> pos_integer().
capacity(Buffer) ->
    Buffer#replay_buffer.capacity.

%% @doc Clear the buffer
-spec clear(buffer()) -> buffer().
clear(Buffer) ->
    Buffer#replay_buffer{
        experiences = [],
        priorities = #{},
        total_samples = 0
    }.

%% @doc Get experience by ID
-spec get_by_id(buffer(), binary()) -> {ok, #experience{}} | {error, not_found}.
get_by_id(Buffer, ExpId) ->
    case lists:search(fun(E) -> E#experience.id =:= ExpId end,
                     Buffer#replay_buffer.experiences) of
        {value, Experience} -> {ok, Experience};
        false -> {error, not_found}
    end.

%% @doc Serialize buffer to binary
-spec serialize(buffer()) -> binary().
serialize(Buffer) ->
    Experiences = Buffer#replay_buffer.experiences,
    %% Convert experiences to maps for JSON serialization
    Maps = [experience_to_map(E) || E <- Experiences],
    jsx:encode(#{
        capacity => Buffer#replay_buffer.capacity,
        sampling_distribution => Buffer#replay_buffer.sampling_distribution,
        total_samples => Buffer#replay_buffer.total_samples,
        experiences => Maps
    }).

%% @doc Deserialize buffer from binary
-spec deserialize(binary()) -> {ok, buffer()} | {error, term()}.
deserialize(Binary) ->
    try
        Data = jsx:decode(Binary, [return_maps]),
        Capacity = maps:get(<<"capacity">>, Data),
        Strategy = case maps:get(<<"sampling_distribution">>, Data, <<"prioritized">>) of
            <<"uniform">> -> uniform;
            <<"prioritized">> -> prioritized;
            <<"diversity">> -> diversity;
            <<"recency">> -> recency
        end,
        TotalSamples = maps:get(<<"total_samples">>, Data, 0),
        ExpMaps = maps:get(<<"experiences">>, Data, []),
        Experiences = [map_to_experience(M) || M <- ExpMaps],
        Priorities = maps:from_list([{E#experience.id, E#experience.priority} || E <- Experiences]),
        {ok, #replay_buffer{
            capacity = Capacity,
            experiences = Experiences,
            priorities = Priorities,
            sampling_distribution = Strategy,
            total_samples = TotalSamples
        }}
    catch
        Error:_ -> {error, Error}
    end.

%%%====================================================================
%%% Internal Functions
%%%====================================================================

%% @private Uniform random sampling
-spec sample_uniform([#experience{}], pos_integer()) -> [#experience{}].
sample_uniform(Experiences, BatchSize) ->
    Indices = lists:seq(1, length(Experiences)),
    Shuffled = shuffle_list(Indices),
    SelectedIndices = lists:sublist(Shuffled, BatchSize),
    [lists:nth(I, Experiences) || I <- SelectedIndices].

%% @private Prioritized sampling (higher priority = more likely to be sampled)
-spec sample_prioritized([#experience{}], #{binary() => float()}, pos_integer()) ->
    [#experience{}].
sample_prioritized(Experiences, Priorities, BatchSize) ->
    %% Calculate sampling probabilities based on priorities
    SumPriority = lists:sum([maps:get(E#experience.id, Priorities, 1.0) || E <- Experiences]),

    %% Normalize and create cumulative distribution
    {CumulativeProbs, ExpList} = lists:foldl(fun(E, {Probs, Acc}) ->
        Priority = maps:get(E#experience.id, Priorities, 1.0),
        Prob = Priority / SumPriority,
        PrevProb = case Probs of
            [] -> 0.0;
            _ -> hd(Probs)
        end,
        {[PrevProb + Prob | Probs], [E | Acc]}
    end, {[], []}, Experiences),

    %% Sample using roulette wheel selection
    sample_roulette(lists:reverse(CumulativeProbs), ExpList, BatchSize, []).

%% @private Roulette wheel sampling
-spec sample_roulette([float()], [#experience{}], pos_integer(), [#experience{}]) ->
    [#experience{}].
sample_roulette(_Probs, _Exps, 0, Acc) ->
    Acc;
sample_roulette(Probs, Exps, N, Acc) ->
    Rnd = rand:uniform(),
    %% Find selected experience
    {SelectedExp, RemainingProbs, RemainingExps} = select_roulette(Rnd, Probs, Exps),
    sample_roulette(RemainingProbs, RemainingExps, N - 1, [SelectedExp | Acc]).

%% @private Select single experience via roulette wheel
-spec select_roulette(float(), [float()], [#experience{}]) ->
    {#experience{}, [float()], [#experience{}]}.
select_roulette(Rnd, [Prob | RestProbs], [Exp | RestExps]) when Rnd < Prob ->
    {Exp, RestProbs, RestExps};
select_roulette(Rnd, [Prob | RestProbs], [_Exp | RestExps]) ->
    select_roulette(Rnd - Prob, RestProbs, RestExps).

%% @private Diversity-based sampling (maximize coverage)
-spec sample_diversity([#experience{}], pos_integer()) -> [#experience{}].
sample_diversity(Experiences, BatchSize) ->
    %% Simple diversity: cluster by agent type and sample from each
    %% In production, use k-means or other clustering
    ByAgent = lists:foldl(fun(E, Acc) ->
        Agent = maps:get(agent_type, E#experience.state, unknown),
        maps:update_with(Agent, fun(L) -> [E | L] end, [E], Acc)
    end, #{}, Experiences),

    %% Sample proportionally from each agent type
    AgentCounts = maps:map(fun(_K, V) -> length(V) end, ByAgent),
    Total = lists:sum(maps:values(AgentCounts)),

    maps:fold(fun(Agent, ExpList, Acc) ->
        Count = length(ExpList),
        Allocate = max(1, round(Count / Total * BatchSize)),
        ToSample = min(Allocate, Count),
        Sampled = sample_uniform(ExpList, ToSample),
        Sampled ++ Acc
    end, [], ByAgent).

%% @private Recency-biased sampling (favor recent experiences)
-spec sample_recency([#experience{}], pos_integer()) -> [#experience{}].
sample_recency(Experiences, BatchSize) ->
    %% Weight by recency (newer = higher weight)
    Now = erlang:system_time(millisecond),
    MaxAge = case Experiences of
        [] -> 1;
        _ -> Now - (hd(lists:reverse(Experiences)))#experience.timestamp + 1
    end,

    Weighted = lists:map(fun(E) ->
        Age = Now - E#experience.timestamp,
        Weight = 1.0 - (Age / MaxAge),  % 0.0 (oldest) to 1.0 (newest)
        {E, max(0.1, Weight)}  % Minimum weight of 0.1
    end, Experiences),

    %% Convert to list and use roulette selection
    TotalWeight = lists:sum([W || {_, W} <- Weighted]),
    {CumulativeWeights, WeightedList} = lists:foldl(fun({E, W}, {Cums, Acc}) ->
        Prev = case Cums of
            [] -> 0.0;
            _ -> hd(Cums)
        end,
        {[Prev + W/TotalWeight | Cums], [{E, W/TotalWeight} | Acc]}
    end, {[], []}, Weighted),

    FlatExps = [E || {E, _} <- WeightedList],
    sample_roulette(lists:reverse(CumulativeWeights), FlatExps, BatchSize, []).

%% @private Shuffle a list (Fisher-Yates)
-spec shuffle_list(list()) -> list().
shuffle_list(List) ->
    %% Convert to array for O(1) access
    Arr = array:from_list(List),
    Shuffled = shuffle_array(Arr, array:size(Arr)),
    array:to_list(Shuffled).

%% @private Shuffle array in place
-spec shuffle_array(array:array(), pos_integer()) -> array:array().
shuffle_array(Arr, 0) ->
    Arr;
shuffle_array(Arr, I) ->
    J = rand:uniform(I),
    ElemI = array:get(I, Arr),
    ElemJ = array:get(J, Arr),
    Arr1 = array:set(I, ElemJ, Arr),
    Arr2 = array:set(J, ElemI, Arr1),
    shuffle_array(Arr2, I - 1).

%% @private Convert experience to map for serialization
-spec experience_to_map(#experience{}) -> map().
experience_to_map(E) ->
    #{
        id => E#experience.id,
        trajectory_id => E#experience.trajectory_id,
        state => maps:map(fun(_K, V) when is_binary(V); is_number(V); is_boolean(V) -> V;
                             (_K, V) -> term_to_binary(V) end, E#experience.state),
        action => E#experience.action,
        reward => E#experience.reward,
        next_state => maps:map(fun(_K, V) when is_binary(V); is_number(V); is_boolean(V) -> V;
                                 (_K, V) -> term_to_binary(V) end, E#experience.next_state),
        done => E#experience.done,
        priority => E#experience.priority,
        timestamp => E#experience.timestamp
    }.

%% @private Convert map to experience
-spec map_to_experience(map()) -> #experience{}.
map_to_experience(M) ->
    #experience{
        id = maps:get(<<"id">>, M),
        trajectory_id = maps:get(<<"trajectory_id">>, M),
        state = maps:get(<<"state">>, M, #{}),
        action = maps:get(<<"action">>, M),
        reward = maps:get(<<"reward">>, M),
        next_state = maps:get(<<"next_state">>, M, #{}),
        done = maps:get(<<"done">>, M, false),
        priority = maps:get(<<"priority">>, M, 1.0),
        timestamp = maps:get(<<"timestamp">>, M, erlang:system_time(millisecond))
    }.
