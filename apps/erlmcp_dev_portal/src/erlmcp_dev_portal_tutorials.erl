-module(erlmcp_dev_portal_tutorials).

-behaviour(gen_server).

%% API exports
-export([start_link/0, create_tutorial/1, get_tutorial/1, get_tutorials/0, get_tutorials_by_category/1, search_tutorials/1, increment_views/1]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_tutorial(Tutorial) ->
    %% Create tutorial
    gen_server:call(?MODULE, {create_tutorial, Tutorial}).

get_tutorial(TutorialId) ->
    %% Get tutorial by ID
    gen_server:call(?MODULE, {get_tutorial, TutorialId}).

get_tutorials() ->
    %% Get all tutorials
    gen_server:call(?MODULE, get_tutorials).

get_tutorials_by_category(Category) ->
    %% Get tutorials by category
    gen_server:call(?MODULE, {get_tutorials_by_category, Category}).

search_tutorials(Query) ->
    %% Search tutorials
    gen_server:call(?MODULE, {search_tutorials, Query}).

increment_views(TutorialId) ->
    %% Increment tutorial view count
    gen_server:call(?MODULE, {increment_views, TutorialId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize tutorial management
    case mnesia:create_table(tutorials, [
        {attributes, record_info(fields, tutorial)},
        {disc_copies, [node()]},
        {type, set},
        {index, #tutorial.category},
        {index, #tutorial.author}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, tutorials}} ->
            ok
    end,

    {ok, #{}}.

handle_call({create_tutorial, TutorialData}, _From, State) ->
    %% Create tutorial
    TutorialId = ?GENERATE_ID(),
    Tutorial = #tutorial{
        id = TutorialId,
        title = maps:get(title, TutorialData),
        content = maps:get(content, TutorialData),
        author = maps:get(author, TutorialData),
        category = maps:get(category, TutorialData),
        created = ?TIMESTAMP(),
        updated = ?TIMESTAMP(),
        views = 0,
        rating = 0.0,
        tags = maps:get(tags, TutorialData, []),
        status = maps:get(status, TutorialData, published)
    },
    case save_tutorial(Tutorial) of
        {atomic, ok} ->
            {reply, {ok, Tutorial}, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_tutorial, TutorialId}, _From, State) ->
    %% Get tutorial by ID
    case mnesia:dirty_read(tutorials, TutorialId) of
        [Tutorial] ->
            %% Increment view count
            UpdatedTutorial = Tutorial#tutorial{
                views = Tutorial#tutorial.views + 1,
                updated = ?TIMESTAMP()
            },
            save_tutorial(UpdatedTutorial),
            {reply, {ok, UpdatedTutorial}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_tutorials, _From, State) ->
    %% Get all tutorials
    Tutorials = mnesia:dirty_all_objects(tutorials),
    SortedTutorials = lists:sort(fun(A, B) ->
        A#tutorial.created >= B#tutorial.created
    end, Tutorials),
    {reply, {ok, SortedTutorials}, State};

handle_call({get_tutorials_by_category, Category}, _From, State) ->
    %% Get tutorials by category
    Tutorials = mnesia:dirty_index_read(tutorials, Category, #tutorial.category),
    SortedTutorials = lists:sort(fun(A, B) ->
        A#tutorial.views >= B#tutorial.views
    end, Tutorials),
    {reply, {ok, SortedTutorials}, State};

handle_call({search_tutorials, Query}, _From, State) ->
    %% Search tutorials
    Tutorials = mnesia:dirty_all_objects(tutorials),
    MatchingTutorials = lists:filter(fun(Tutorial) ->
        case Tutorial#tutorial.title of
            Title when is_binary(Title) ->
                case re:run(Title, Query, [{case_insensitive, true}]) of
                    nomatch -> false;
                    _ -> true
                end;
            _ -> false
        end
    end, Tutorials),
    {reply, {ok, MatchingTutorials}, State};

handle_call({increment_views, TutorialId}, _From, State) ->
    %% Increment tutorial view count
    case mnesia:dirty_read(tutorials, TutorialId) of
        [Tutorial] ->
            UpdatedTutorial = Tutorial#tutorial{
                views = Tutorial#tutorial.views + 1,
                updated = ?TIMESTAMP()
            },
            save_tutorial(UpdatedTutorial),
            {reply, ok, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

save_tutorial(Tutorial) ->
    %% Save tutorial to mnesia
    F = fun() ->
        mnesia:write(Tutorial)
    end,
    mnesia:transaction(F).