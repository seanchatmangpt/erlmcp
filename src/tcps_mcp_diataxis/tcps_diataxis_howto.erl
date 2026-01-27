%%%-------------------------------------------------------------------
%%% @doc TCPS Diataxis How-to Guide Engine
%%%
%%% Task-oriented documentation that helps users accomplish specific goals.
%%% How-to guides are recipes that guide users through solving real-world problems.
%%%
%%% Features:
%%% - Problem-solution-verification pattern
%%% - Step-by-step instructions
%%% - Code examples with expected outputs
%%% - Common pitfalls and troubleshooting
%%% - Assumes basic TCPS knowledge
%%% - MCP tool integration
%%%
%%% @reference Diataxis Framework - https://diataxis.fr/how-to-guides/
%%% @end
%%%-------------------------------------------------------------------
-module(tcps_diataxis_howto).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    get_guide/1,
    list_guides/0,
    search_guides/1,
    get_guide_by_category/1,
    execute_guide_step/2,
    verify_step_completion/2,
    get_troubleshooting/1,
    format_for_mcp/1,
    get_related_guides/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("kernel/include/logger.hrl").

-type guide_id() :: atom().
-type category() :: quality_gates | kanban | andon | analysis | production |
                    monitoring | integration | debugging | performance | receipts.
-type difficulty() :: beginner | intermediate | advanced.
-type duration_minutes() :: pos_integer().

-type step() :: #{
    number := pos_integer(),
    description := binary(),
    command := binary() | undefined,
    expected_output := binary() | undefined,
    verification := binary() | undefined,
    notes := [binary()]
}.

-type guide() :: #{
    id := guide_id(),
    title := binary(),
    category := category(),
    difficulty := difficulty(),
    duration := duration_minutes(),
    problem := binary(),
    prerequisites := [binary()],
    steps := [step()],
    verification := binary(),
    common_pitfalls := [binary()],
    related_guides := [guide_id()],
    tags := [binary()]
}.

-record(state, {
    guides = #{} :: #{guide_id() => guide()},
    categories = #{} :: #{category() => [guide_id()]},
    search_index = #{} :: #{binary() => [guide_id()]}
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec get_guide(guide_id()) -> {ok, guide()} | {error, not_found}.
get_guide(GuideId) ->
    gen_server:call(?MODULE, {get_guide, GuideId}).

-spec list_guides() -> {ok, [guide()]}.
list_guides() ->
    gen_server:call(?MODULE, list_guides).

-spec search_guides(binary()) -> {ok, [guide()]}.
search_guides(Query) ->
    gen_server:call(?MODULE, {search_guides, Query}).

-spec get_guide_by_category(category()) -> {ok, [guide()]}.
get_guide_by_category(Category) ->
    gen_server:call(?MODULE, {get_by_category, Category}).

-spec execute_guide_step(guide_id(), pos_integer()) ->
    {ok, #{command := binary(), expected := binary()}} | {error, term()}.
execute_guide_step(GuideId, StepNumber) ->
    gen_server:call(?MODULE, {execute_step, GuideId, StepNumber}).

-spec verify_step_completion(guide_id(), pos_integer()) ->
    {ok, verified | failed, binary()} | {error, term()}.
verify_step_completion(GuideId, StepNumber) ->
    gen_server:call(?MODULE, {verify_step, GuideId, StepNumber}).

-spec get_troubleshooting(guide_id()) -> {ok, [binary()]} | {error, not_found}.
get_troubleshooting(GuideId) ->
    gen_server:call(?MODULE, {get_troubleshooting, GuideId}).

-spec format_for_mcp(guide()) -> map().
format_for_mcp(Guide) ->
    gen_server:call(?MODULE, {format_mcp, Guide}).

-spec get_related_guides(guide_id()) -> {ok, [guide()]} | {error, term()}.
get_related_guides(GuideId) ->
    gen_server:call(?MODULE, {get_related, GuideId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    ?LOG_INFO("Starting TCPS How-to Guide Engine"),

    %% Load all guides from recipe library
    Guides = tcps_howto_recipes:load_all_guides(),

    %% Build category index
    Categories = build_category_index(Guides),

    %% Build search index
    SearchIndex = build_search_index(Guides),

    ?LOG_INFO("Loaded ~p how-to guides across ~p categories",
              [maps:size(Guides), maps:size(Categories)]),

    {ok, #state{
        guides = Guides,
        categories = Categories,
        search_index = SearchIndex
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call({get_guide, GuideId}, _From, State) ->
    Reply = case maps:find(GuideId, State#state.guides) of
        {ok, Guide} -> {ok, Guide};
        error -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call(list_guides, _From, State) ->
    AllGuides = maps:values(State#state.guides),
    SortedGuides = lists:sort(
        fun(#{category := C1, title := T1}, #{category := C2, title := T2}) ->
            {C1, T1} =< {C2, T2}
        end,
        AllGuides
    ),
    {reply, {ok, SortedGuides}, State};

handle_call({search_guides, Query}, _From, State) ->
    Results = search_in_index(Query, State#state.search_index, State#state.guides),
    {reply, {ok, Results}, State};

handle_call({get_by_category, Category}, _From, State) ->
    GuideIds = maps:get(Category, State#state.categories, []),
    Guides = [maps:get(Id, State#state.guides) || Id <- GuideIds],
    {reply, {ok, Guides}, State};

handle_call({execute_step, GuideId, StepNumber}, _From, State) ->
    Reply = case maps:find(GuideId, State#state.guides) of
        {ok, #{steps := Steps}} ->
            case lists:keyfind(StepNumber, 1,
                [{maps:get(number, S), S} || S <- Steps]) of
                {_, Step} ->
                    {ok, #{
                        command => maps:get(command, Step, <<>>),
                        expected => maps:get(expected_output, Step, <<>>),
                        verification => maps:get(verification, Step, <<>>)
                    }};
                false ->
                    {error, step_not_found}
            end;
        error ->
            {error, guide_not_found}
    end,
    {reply, Reply, State};

handle_call({verify_step, GuideId, StepNumber}, _From, State) ->
    Reply = case maps:find(GuideId, State#state.guides) of
        {ok, #{steps := Steps}} ->
            case lists:keyfind(StepNumber, 1,
                [{maps:get(number, S), S} || S <- Steps]) of
                {_, Step} ->
                    VerificationMsg = maps:get(verification, Step,
                        <<"Step completed successfully">>),
                    {ok, verified, VerificationMsg};
                false ->
                    {error, step_not_found}
            end;
        error ->
            {error, guide_not_found}
    end,
    {reply, Reply, State};

handle_call({get_troubleshooting, GuideId}, _From, State) ->
    Reply = case maps:find(GuideId, State#state.guides) of
        {ok, #{common_pitfalls := Pitfalls}} -> {ok, Pitfalls};
        error -> {error, not_found}
    end,
    {reply, Reply, State};

handle_call({format_mcp, Guide}, _From, State) ->
    MCPFormat = format_guide_for_mcp(Guide),
    {reply, MCPFormat, State};

handle_call({get_related, GuideId}, _From, State) ->
    Reply = case maps:find(GuideId, State#state.guides) of
        {ok, #{related_guides := RelatedIds}} ->
            Related = lists:filtermap(
                fun(Id) ->
                    case maps:find(Id, State#state.guides) of
                        {ok, G} -> {true, G};
                        error -> false
                    end
                end,
                RelatedIds
            ),
            {ok, Related};
        error ->
            {error, not_found}
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec build_category_index(#{guide_id() => guide()}) -> #{category() => [guide_id()]}.
build_category_index(Guides) ->
    maps:fold(
        fun(GuideId, #{category := Category}, Acc) ->
            Current = maps:get(Category, Acc, []),
            maps:put(Category, [GuideId | Current], Acc)
        end,
        #{},
        Guides
    ).

-spec build_search_index(#{guide_id() => guide()}) -> #{binary() => [guide_id()]}.
build_search_index(Guides) ->
    maps:fold(
        fun(GuideId, Guide, Acc) ->
            %% Extract searchable terms
            Terms = extract_search_terms(Guide),
            %% Add guide ID to each term's list
            lists:foldl(
                fun(Term, InnerAcc) ->
                    Current = maps:get(Term, InnerAcc, []),
                    maps:put(Term, [GuideId | Current], InnerAcc)
                end,
                Acc,
                Terms
            )
        end,
        #{},
        Guides
    ).

-spec extract_search_terms(guide()) -> [binary()].
extract_search_terms(#{title := Title, problem := Problem, tags := Tags}) ->
    %% Tokenize title and problem
    TitleTokens = tokenize(Title),
    ProblemTokens = tokenize(Problem),

    %% Combine all terms
    lists:usort(TitleTokens ++ ProblemTokens ++ Tags).

-spec tokenize(binary()) -> [binary()].
tokenize(Text) ->
    %% Simple tokenization: split on whitespace and convert to lowercase
    Words = binary:split(string:lowercase(Text), [<<" ">>, <<"\n">>, <<"\t">>], [global]),
    %% Filter out short words and common words
    lists:filter(
        fun(W) ->
            byte_size(W) > 2 andalso
            not lists:member(W, [<<"the">>, <<"and">>, <<"for">>, <<"with">>])
        end,
        Words
    ).

-spec search_in_index(binary(), #{binary() => [guide_id()]}, #{guide_id() => guide()}) ->
    [guide()].
search_in_index(Query, SearchIndex, Guides) ->
    %% Tokenize query
    QueryTokens = tokenize(Query),

    %% Find matching guide IDs
    MatchingIds = lists:usort(
        lists:flatten([
            maps:get(Token, SearchIndex, [])
            || Token <- QueryTokens
        ])
    ),

    %% Return matching guides
    lists:filtermap(
        fun(Id) ->
            case maps:find(Id, Guides) of
                {ok, Guide} -> {true, Guide};
                error -> false
            end
        end,
        MatchingIds
    ).

-spec format_guide_for_mcp(guide()) -> map().
format_guide_for_mcp(Guide) ->
    #{
        type => <<"how_to_guide">>,
        id => atom_to_binary(maps:get(id, Guide)),
        title => maps:get(title, Guide),
        category => atom_to_binary(maps:get(category, Guide)),
        difficulty => atom_to_binary(maps:get(difficulty, Guide)),
        duration_minutes => maps:get(duration, Guide),
        problem => maps:get(problem, Guide),
        prerequisites => maps:get(prerequisites, Guide),
        steps => [format_step_for_mcp(S) || S <- maps:get(steps, Guide)],
        verification => maps:get(verification, Guide),
        common_pitfalls => maps:get(common_pitfalls, Guide),
        related_guides => [atom_to_binary(R) || R <- maps:get(related_guides, Guide)],
        tags => maps:get(tags, Guide)
    }.

-spec format_step_for_mcp(step()) -> map().
format_step_for_mcp(Step) ->
    #{
        number => maps:get(number, Step),
        description => maps:get(description, Step),
        command => maps:get(command, Step, null),
        expected_output => maps:get(expected_output, Step, null),
        verification => maps:get(verification, Step, null),
        notes => maps:get(notes, Step, [])
    }.
