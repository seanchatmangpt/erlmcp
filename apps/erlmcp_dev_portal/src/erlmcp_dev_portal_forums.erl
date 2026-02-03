-module(erlmcp_dev_portal_forums).

-behaviour(gen_server).

%% API exports
-export([start_link/0, create_post/1, get_post/1, get_posts/0, get_posts_by_category/1, reply_to_post/2, search_posts/1]).

%%====================================================================
%% API Functions
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_post(Post) ->
    %% Create forum post
    gen_server:call(?MODULE, {create_post, Post}).

get_post(PostId) ->
    %% Get forum post by ID
    gen_server:call(?MODULE, {get_post, PostId}).

get_posts() ->
    %% Get all forum posts
    gen_server:call(?MODULE, get_posts).

get_posts_by_category(Category) ->
    %% Get posts by category
    gen_server:call(?MODULE, {get_posts_by_category, Category}).

reply_to_post(PostId, Reply) ->
    %% Reply to forum post
    gen_server:call(?MODULE, {reply_to_post, PostId, Reply}).

search_posts(Query) ->
    %% Search forum posts
    gen_server:call(?MODULE, {search_posts, Query}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% Initialize forum management
    case mnesia:create_table(forum_posts, [
        {attributes, record_info(fields, forum_post)},
        {disc_copies, [node()]},
        {type, set},
        {index, #forum_post.category},
        {index, #forum_post.author}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, forum_posts}} ->
            ok
    end,

    case mnesia:create_table(forum_replies, [
        {attributes, record_info(fields, forum_reply)},
        {disc_copies, [node()]},
        {type, set},
        {index, #forum_reply.post_id}
    ]) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, forum_replies}} ->
            ok
    end,

    {ok, #{}}.

handle_call({create_post, PostData}, _From, State) ->
    %% Create forum post
    PostId = ?GENERATE_ID(),
    Post = #forum_post{
        id = PostId,
        title = maps:get(title, PostData),
        content = maps:get(content, PostData),
        author = maps:get(author, PostData),
        category = maps:get(category, PostData),
        created = ?TIMESTAMP(),
        updated = ?TIMESTAMP(),
        views = 0,
        replies = [],
        tags = maps:get(tags, PostData, []),
        status = open
    },
    case save_forum_post(Post) of
        {atomic, ok} ->
            {reply, {ok, Post}, State};
        {atomic, {error, Reason}} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_post, PostId}, _From, State) ->
    %% Get forum post by ID
    case mnesia:dirty_read(forum_posts, PostId) of
        [Post] ->
            %% Increment view count
            UpdatedPost = Post#forum_post{
                views = Post#forum_post.views + 1,
                updated = ?TIMESTAMP()
            },
            save_forum_post(UpdatedPost),
            {reply, {ok, UpdatedPost}, State};
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call(get_posts, _From, State) ->
    %% Get all forum posts
    Posts = mnesia:dirty_all_objects(forum_posts),
    SortedPosts = lists:sort(fun(A, B) ->
        A#forum_post.created >= B#forum_post.created
    end, Posts),
    {reply, {ok, SortedPosts}, State};

handle_call({get_posts_by_category, Category}, _From, State) ->
    %% Get posts by category
    Posts = mnesia:dirty_index_read(forum_posts, Category, #forum_post.category),
    SortedPosts = lists:sort(fun(A, B) ->
        A#forum_post.created >= B#forum_post.created
    end, Posts),
    {reply, {ok, SortedPosts}, State};

handle_call({reply_to_post, PostId, ReplyData}, _From, State) ->
    %% Reply to forum post
    case mnesia:dirty_read(forum_posts, PostId) of
        [Post] ->
            ReplyId = ?GENERATE_ID(),
            Reply = #forum_reply{
                id = ReplyId,
                post_id = PostId,
                content = maps:get(content, ReplyData),
                author = maps:get(author, ReplyData),
                created = ?TIMESTAMP(),
                parent_reply = maps:get(parent_reply, ReplyData, undefined)
            },
            case save_forum_reply(Reply) of
                {atomic, ok} ->
                    %% Update post with new reply
                    UpdatedPost = Post#forum_post{
                        replies = [ReplyId | Post#forum_post.replies],
                        updated = ?TIMESTAMP()
                    },
                    save_forum_post(UpdatedPost),
                    {reply, {ok, Reply}, State};
                {atomic, {error, Reason}} ->
                    {reply, {error, Reason}, State}
            end;
        [] ->
            {reply, {error, not_found}, State}
    end;

handle_call({search_posts, Query}, _From, State) ->
    %% Search forum posts
    Posts = mnesia:dirty_all_objects(forum_posts),
    MatchingPosts = lists:filter(fun(Post) ->
        case Post#forum_post.title of
            Title when is_binary(Title) ->
                case re:run(Title, Query, [{case_insensitive, true}]) of
                    nomatch -> false;
                    _ -> true
                end;
            _ -> false
        end
    end, Posts),
    {reply, {ok, MatchingPosts}, State};

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

save_forum_post(Post) ->
    %% Save forum post to mnesia
    F = fun() ->
        mnesia:write(Post)
    end,
    mnesia:transaction(F).

save_forum_reply(Reply) ->
    %% Save forum reply to mnesia
    F = fun() ->
        mnesia:write(Reply)
    end,
    mnesia:transaction(F).