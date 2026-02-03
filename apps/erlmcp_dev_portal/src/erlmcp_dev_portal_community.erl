-module(erlmcp_dev_portal_community).

-behaviour(cowboy_handler).

%% API exports
-export([init/2, handle/2, terminate/3]).

%%====================================================================
%% API Functions
%%====================================================================

init(Req, _Opts) ->
    %% Initialize community state
    State = #{},
    {ok, Req, State}.

handle(Req, State) ->
    %% Handle community requests
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case Method of
        <<"GET">> ->
            handle_get_community(Path, Req, State);
        <<"POST">> ->
            handle_post_community(Path, Req, State);
        _ ->
            cowboy_req:reply(405, #{}, <<"Method not allowed">>, Req)
    end.

terminate(_Reason, _Req, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

handle_get_community(<<"/community">>, Req, State) ->
    %% Return community home page
    {ok, Body} = render_community_home(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_community(<<"/community/forums">>, Req, State) ->
    %% Return forums page
    {ok, Body} = render_forums_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_community(<<"/community/tutorials">>, Req, State) ->
    %% Return tutorials page
    {ok, Body} = render_tutorials_page(),
    cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Body, Req);

handle_get_community(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

handle_post_community(<<"/community/forums/post">>, Req, State) ->
    %% Handle forum post creation
    {ok, Body, Req} = cowboy_req:read_body(Req),
    PostData = jsx:decode(Body, [{labels, atom}),

    case create_forum_post(PostData) of
        {ok, Post} ->
            cowboy_req:reply(302, #{<<"location">> => <<"/community/forums">>}, Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, render_post_error(Reason), Req)
    end;

handle_post_community(<<"/community/tutorials/create">>, Req, State) ->
    %% Handle tutorial creation
    {ok, Body, Req} = cowboy_req:read_body(Req),
    TutorialData = jsx:decode(Body, [{labels, atom}),

    case create_tutorial(TutorialData) of
        {ok, Tutorial} ->
            cowboy_req:reply(302, #{<<"location">> => <<"/community/tutorials">>}, Req);
        {error, Reason} ->
            cowboy_req:reply(400, #{<<"content-type">> => <<"text/html">>}, render_tutorial_error(Reason), Req)
    end;

handle_post_community(Path, Req, State) ->
    cowboy_req:reply(404, #{}, <<"Not found">>, Req).

render_community_home() ->
    %% Render community home page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Community - erlmcp</title></head>"
        "<body>"
        "<h1>Developer Community</h1>"
        "<div class='community-stats'>"
        "<h2>Community Stats</h2>"
        "<div class='stats-grid'>"
        "<div class='stat-card'>"
        "<h3>1,234</h3>"
        "<p>Developers</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>567</h3>"
        "<p>APIs</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>89</h3>"
        "<p>Companies</p>"
        "</div>"
        "<div class='stat-card'>"
        "<h3>2,345</h3>"
        "<p>Posts</p>"
        "</div>"
        "</div>"
        "</div>"
        "<div class='quick-links'>"
        "<h2>Community Quick Links</h2>"
        "<div class='links-grid'>"
        "<a href='/community/forums' class='link-card'>"
        "<h3>Forums</h3>"
        "<p>Join discussions and get help</p>"
        "</a>"
        "<a href='/community/tutorials' class='link-card'>"
        "<h3>Tutorials</h3>"
        "<p>Learn from community experts</p>"
        "</a>"
        "<a href='/community/events' class='link-card'>"
        "<h3>Events</h3>"
        "<p>Upcoming meetups and webinars</p>"
        "</a>"
        "<a href='/community/resources' class='link-card'>"
        "<h3>Resources</h3>"
        "<p>Community-driven resources</p>"
        "</a>"
        "</div>"
        "</div>"
        "<div class='recent-activity'>"
        "<h2>Recent Activity</h2>"
        "<div id='activity-feed'></div>"
        "</div>"
        "<script>"
        "fetchCommunityStats();"
        "loadRecentActivity();"
        ""
        "function fetchCommunityStats() {"
        "  fetch('/community/stats')"
        "    .then(response => response.json())"
        "    .then(stats => {"
        "      const grid = document.querySelector('.stats-grid');"
        "      // Update stat cards with fetched data"
        "    });"
        "}"
        ""
        "function loadRecentActivity() {"
        "  fetch('/community/activity')"
        "    .then(response => response.json())"
        "    .then(activity => {"
        "      const feed = document.getElementById('activity-feed');"
        "      activity.forEach(item => {"
        "        const div = document.createElement('div');"
        "        div.className = 'activity-item';"
        "        div.innerHTML = `<strong>${item.user}</strong> ${item.action}`;"
        "        feed.appendChild(div);"
        "      });"
        "    });"
        "}"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_forums_page() ->
    %% Render forums page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Forums - erlmcp</title></head>"
        "<body>"
        "<h1>Developer Forums</h1>"
        "<div class='forums-container'>"
        "<div class='categories'>"
        "<h2>Categories</h2>"
        "<div class='category-list'>"
        "<div class='category-item'>"
        "<h3>API Development</h3>"
        "<p>Questions and discussions about API development</p>"
        "<a href='/community/forums/api-dev'>View Posts</a>"
        "</div>"
        "<div class='category-item'>"
        "<h3>Integration</h3>"
        "<p>Help with integrating erlmcp with other systems</p>"
        "<a href='/community/forums/integration'>View Posts</a>"
        "</div>"
        "<div class='category-item'>"
        "<h3>General Discussion</h3>"
        "<p>General discussions about erlmcp</p>"
        "<a href='/community/forums/general'>View Posts</a>"
        "</div>"
        "</div>"
        "</div>"
        "<div class='recent-posts'>"
        "<h2>Recent Posts</h2>"
        "<div id='posts-list'></div>"
        "<button id='create-post'>Create New Post</button>"
        "</div>"
        "</div>"
        "<div id='post-modal' style='display: none;'>"
        "<h3>Create New Post</h3>"
        "<form id='post-form'>"
        "<input type='text' id='title' placeholder='Title' required>"
        "<select id='category'>"
        "<option value='api-dev'>API Development</option>"
        "<option value='integration'>Integration</option>"
        "<option value='general'>General Discussion</option>"
        "</select>"
        "<textarea id='content' placeholder='Content' required></textarea>"
        "<button type='submit'>Post</button>"
        "</form>"
        "</div>"
        "<script>"
        "loadRecentPosts();"
        ""
        "document.getElementById('create-post').addEventListener('click', function() {"
        "  document.getElementById('post-modal').style.display = 'block';"
        "});"
        ""
        "document.getElementById('post-form').addEventListener('submit', function(e) {"
        "  e.preventDefault();"
        "  const title = document.getElementById('title').value;"
        "  const category = document.getElementById('category').value;"
        "  const content = document.getElementById('content').value;"
        "  "
        "  fetch('/community/forums/post', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({title, category, content})"
        "  })"
        "    .then(response => response.json())"
        "    .then(result => {"
        "      if (result.success) {"
        "        location.reload();"
        "      }"
        "    });"
        "});"
        ""
        "function loadRecentPosts() {"
        "  fetch('/community/forums/posts')"
        "    .then(response => response.json())"
        "    .then(posts => {"
        "      const list = document.getElementById('posts-list');"
        "      posts.forEach(post => {"
        "        const div = document.createElement('div');"
        "        div.className = 'post-item';"
        "        div.innerHTML = `"
        "<h3><a href='/community/forums/posts/${post.id}'>${post.title}</a></h3>"
        "<p>By ${post.author} - ${post.date}</p>"
        "<p>${post.excerpt}</p>`;"
        "        list.appendChild(div);"
        "      });"
        "    });"
        "}"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

render_tutorials_page() ->
    %% Render tutorials page HTML
    Html = <<
        "<!DOCTYPE html>"
        "<html>"
        "<head><title>Tutorials - erlmcp</title></head>"
        "<body>"
        "<h1>Developer Tutorials</h1>"
        "<div class='tutorials-container'>"
        "<div class='tutorial-categories'>"
        "<h2>Categories</h2>"
        "<div class='category-filters'>"
        "<button class='filter-btn active' data-category='all'>All</button>"
        "<button class='filter-btn' data-category='getting-started'>Getting Started</button>"
        "<button class='filter-btn' data-category='advanced'>Advanced</button>"
        "<button class='filter-btn' data-category='integrations'>Integrations</button>"
        "</div>"
        "</div>"
        "<div class='tutorials-list'>"
        "<h2>Tutorials</h2>"
        "<div id='tutorials'></div>"
        "<button id='create-tutorial'>Create Tutorial</button>"
        "</div>"
        "</div>"
        "<div id='tutorial-modal' style='display: none;'>"
        "<h3>Create New Tutorial</h3>"
        "<form id='tutorial-form'>"
        "<input type='text' id='title' placeholder='Title' required>"
        "<select id='category'>"
        "<option value='getting-started'>Getting Started</option>"
        "<option value='advanced'>Advanced</option>"
        "<option value='integrations'>Integrations</option>"
        "</select>"
        "<input type='text' id='author' placeholder='Author' required>"
        "<textarea id='content' placeholder='Content' required></textarea>"
        "<button type='submit'>Create Tutorial</button>"
        "</form>"
        "</div>"
        "<script>"
        "loadTutorials();"
        ""
        "document.querySelectorAll('.filter-btn').forEach(btn => {"
        "  btn.addEventListener('click', function() {"
        "    document.querySelectorAll('.filter-btn').forEach(b => b.classList.remove('active'));"
        "    this.classList.add('active');"
        "    loadTutorials(this.dataset.category);"
        "  });"
        "});"
        ""
        "document.getElementById('create-tutorial').addEventListener('click', function() {"
        "  document.getElementById('tutorial-modal').style.display = 'block';"
        "});"
        ""
        "document.getElementById('tutorial-form').addEventListener('submit', function(e) {"
        "  e.preventDefault();"
        "  const title = document.getElementById('title').value;"
        "  const category = document.getElementById('category').value;"
        "  const author = document.getElementById('author').value;"
        "  const content = document.getElementById('content').value;"
        "  "
        "  fetch('/community/tutorials/create', {"
        "    method: 'POST',"
        "    headers: {'Content-Type': 'application/json'},"
        "    body: JSON.stringify({title, category, author, content})"
        "  })"
        "    .then(response => response.json())"
        "    .then(result => {"
        "      if (result.success) {"
        "        location.reload();"
        "      }"
        "    });"
        "});"
        ""
        "function loadTutorials(category = 'all') {"
        "  const url = category === 'all' ? '/community/tutorials' : `/community/tutorials?category=${category}`;"
        "  fetch(url)"
        "    .then(response => response.json())"
        "    .then(tutorials => {"
        "      const list = document.getElementById('tutorials');"
        "      list.innerHTML = '';"
        "      tutorials.forEach(tutorial => {"
        "        const div = document.createElement('div');"
        "        div.className = 'tutorial-item';"
        "        div.innerHTML = `"
        "<h3><a href='/community/tutorials/${tutorial.id}'>${tutorial.title}</a></h3>"
        "<p>By ${tutorial.author} - ${tutorial.date}</p>"
        "<p>${tutorial.excerpt}</p>"
        "<span class='category'>${tutorial.category}</span>`;"
        "        list.appendChild(div);"
        "      });"
        "    });"
        "}"
        "</script>"
        "</body>"
        "</html>"
    >>,
    {ok, Html}.

create_forum_post(PostData) ->
    %% Create forum post
    Title = proplists:get_value(title, PostData),
    Category = proplists:get_value(category, PostData),
    Content = proplists:get_value(content, PostData),

    %% Validate input
    case validate_post(Title, Content) of
        ok ->
            %% Create post
            Post = #{
                id => generate_id(),
                title => Title,
                category => Category,
                content => Content,
                author => get_current_user(),
                created => erlang:system_time(second),
                replies => []
            },
            erlmcp_dev_portal_forums:create_post(Post),
            {ok, Post};
        {error, Reason} -> {error, Reason}
    end.

create_tutorial(TutorialData) ->
    %% Create tutorial
    Title = proplists:get_value(title, TutorialData),
    Category = proplists:get_value(category, TutorialData),
    Author = proplists:get_value(author, TutorialData),
    Content = proplists:get_value(content, TutorialData),

    %% Validate input
    case validate_tutorial(Title, Content) of
        ok ->
            %% Create tutorial
            Tutorial = #{
                id => generate_id(),
                title => Title,
                category => Category,
                author => Author,
                content => Content,
                created => erlang:system_time(second),
                views => 0,
                rating => 0
            },
            erlmcp_dev_portal_tutorials:create_tutorial(Tutorial),
            {ok, Tutorial};
        {error, Reason} -> {error, Reason}
    end.

validate_post(Title, Content) ->
    %% Validate forum post
    case Title of
        undefined -> {error, title_required};
        _ when length(Title) < 5 -> {error, title_too_short};
        _ -> ok
    end,

    case Content of
        undefined -> {error, content_required};
        _ when length(Content) < 10 -> {error, content_too_short};
        _ -> ok
    end.

validate_tutorial(Title, Content) ->
    %% Validate tutorial
    case Title of
        undefined -> {error, title_required};
        _ when length(Title) < 5 -> {error, title_too_short};
        _ -> ok
    end,

    case Content of
        undefined -> {error, content_required};
        _ when length(Content) < 100 -> {error, content_too_short};
        _ -> ok
    end.

generate_id() ->
    %% Generate unique ID
    uuid:to_string(uuid:new()).

get_current_user() ->
    %% Get current user from session
    %% Implementation would check session cookies
    "current-user".