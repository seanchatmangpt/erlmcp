%%% @doc Offline Hybrid Search Provider for MCP Advisor
%%%
%%% Provides offline search capabilities using a combination of:
%%% - Text-based matching (TF-IDF inspired)
%%% - Vector similarity search (cosine similarity)
%%% - Keyword matching
%%%
%%% This provider works without external dependencies and maintains
%%% a local index of known MCP servers.
%%%
%%% @end
-module(erlmcp_advisor_provider_offline).

-behaviour(erlmcp_advisor_provider).

-include("erlmcp.hrl").

%% Behavior callbacks
-export([
    search/2,
    info/0
]).

%% Optional callbacks
-export([
    init/1,
    health/0,
    cleanup/0
]).

%% Index management
-export([
    add_server/1,
    remove_server/1,
    update_index/0,
    get_index_stats/0,
    seed_default_servers/0
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type search_options() :: erlmcp_advisor_provider:search_options().
-type search_result() :: erlmcp_advisor_provider:search_result().

-type server_entry() :: #{
    id := binary(),
    title := binary(),
    description := binary(),
    url => binary(),
    github_url => binary(),
    keywords := [binary()],
    vector => [float()],
    metadata => map()
}.

%%====================================================================
%% Constants
%%====================================================================

-define(INDEX_TABLE, erlmcp_advisor_offline_index).
-define(VECTOR_DIM, 64).
-define(TEXT_WEIGHT, 0.3).
-define(VECTOR_WEIGHT, 0.7).

%%====================================================================
%% Behavior Callbacks
%%====================================================================

%% @doc Search for MCP servers using hybrid text+vector matching
-spec search(binary(), search_options()) -> {ok, [search_result()]} | {error, term()}.
search(Query, Options) when is_binary(Query), is_map(Options) ->
    try
        ensure_index_exists(),

        Limit = maps:get(limit, Options, 10),
        MinSimilarity = maps:get(min_similarity, Options, 0.3),

        %% Get all indexed servers
        Servers = get_all_servers(),

        %% Generate query vector
        QueryVector = text_to_vector(Query),

        %% Score servers using hybrid approach
        Scored = hybrid_score_servers(Query, QueryVector, Servers),

        %% Filter by minimum similarity
        Filtered = [S || S <- Scored, maps:get(similarity, S, 0.0) >= MinSimilarity],

        %% Sort by similarity descending
        Sorted = lists:sort(fun(A, B) ->
            maps:get(similarity, A, 0.0) >= maps:get(similarity, B, 0.0)
        end, Filtered),

        %% Apply limit
        Results = lists:sublist(Sorted, Limit),

        {ok, Results}
    catch
        Type:Error:Stack ->
            logger:error("Offline search failed", #{
                type => Type,
                error => Error,
                stack => Stack
            }),
            {error, {search_failed, Error}}
    end.

%% @doc Provider information
-spec info() -> erlmcp_advisor_provider:provider_info().
info() ->
    #{
        name => <<"Offline Hybrid Search">>,
        version => <<"1.0.0">>,
        description => <<"Hybrid text+vector search for offline MCP server discovery">>,
        capabilities => [offline, text_search, vector_search, hybrid]
    }.

%% @doc Initialize the provider
-spec init(map()) -> ok | {error, term()}.
init(_Config) ->
    ensure_index_exists(),
    seed_default_servers(),
    ok.

%% @doc Health check
-spec health() -> ok | {error, term()}.
health() ->
    case ets:info(?INDEX_TABLE) of
        undefined -> {error, index_not_initialized};
        _ -> ok
    end.

%% @doc Cleanup
-spec cleanup() -> ok.
cleanup() ->
    catch ets:delete(?INDEX_TABLE),
    ok.

%%====================================================================
%% Index Management
%%====================================================================

%% @doc Add a server to the index
-spec add_server(server_entry()) -> ok.
add_server(#{id := Id} = Server) when is_binary(Id) ->
    ensure_index_exists(),

    %% Generate vector if not provided
    ServerWithVector = case maps:get(vector, Server, undefined) of
        undefined ->
            Description = maps:get(description, Server, <<>>),
            Title = maps:get(title, Server, <<>>),
            Vector = text_to_vector(<<Title/binary, " ", Description/binary>>),
            Server#{vector => Vector};
        _ ->
            Server
    end,

    %% Extract keywords if not provided
    ServerWithKeywords = case maps:get(keywords, ServerWithVector, undefined) of
        undefined ->
            Description = maps:get(description, ServerWithVector, <<>>),
            Title = maps:get(title, ServerWithVector, <<>>),
            Keywords = extract_keywords(<<Title/binary, " ", Description/binary>>),
            ServerWithVector#{keywords => Keywords};
        _ ->
            ServerWithVector
    end,

    ets:insert(?INDEX_TABLE, {Id, ServerWithKeywords}),
    ok.

%% @doc Remove a server from the index
-spec remove_server(binary()) -> ok.
remove_server(Id) when is_binary(Id) ->
    ensure_index_exists(),
    ets:delete(?INDEX_TABLE, Id),
    ok.

%% @doc Update the index from the registry
-spec update_index() -> ok.
update_index() ->
    ensure_index_exists(),

    %% Get servers from registry
    try
        Servers = erlmcp_registry:list_servers(local),
        lists:foreach(fun({ServerId, {_Pid, Config}}) ->
            Entry = registry_to_entry(ServerId, Config),
            add_server(Entry)
        end, Servers)
    catch
        _:_ -> ok
    end,
    ok.

%% @doc Get index statistics
-spec get_index_stats() -> map().
get_index_stats() ->
    ensure_index_exists(),
    #{
        size => ets:info(?INDEX_TABLE, size),
        memory => ets:info(?INDEX_TABLE, memory)
    }.

%% @doc Seed the index with default/well-known MCP servers
-spec seed_default_servers() -> ok.
seed_default_servers() ->
    DefaultServers = [
        #{
            id => <<"filesystem">>,
            title => <<"Filesystem">>,
            description => <<"File system operations - read, write, list directories, search files. Essential for file management tasks.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/filesystem">>,
            keywords => [<<"file">>, <<"filesystem">>, <<"directory">>, <<"read">>, <<"write">>, <<"search">>]
        },
        #{
            id => <<"git">>,
            title => <<"Git">>,
            description => <<"Git version control operations - clone, commit, push, pull, branch management, history.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/git">>,
            keywords => [<<"git">>, <<"version">>, <<"control">>, <<"commit">>, <<"branch">>, <<"repository">>]
        },
        #{
            id => <<"github">>,
            title => <<"GitHub">>,
            description => <<"GitHub API integration - repositories, issues, pull requests, actions, releases.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/github">>,
            keywords => [<<"github">>, <<"repository">>, <<"issue">>, <<"pull request">>, <<"actions">>]
        },
        #{
            id => <<"slack">>,
            title => <<"Slack">>,
            description => <<"Slack messaging integration - send messages, manage channels, search conversations.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/slack">>,
            keywords => [<<"slack">>, <<"message">>, <<"channel">>, <<"chat">>, <<"communication">>]
        },
        #{
            id => <<"postgres">>,
            title => <<"PostgreSQL">>,
            description => <<"PostgreSQL database operations - query, insert, update, schema management.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/postgres">>,
            keywords => [<<"postgres">>, <<"postgresql">>, <<"database">>, <<"sql">>, <<"query">>]
        },
        #{
            id => <<"sqlite">>,
            title => <<"SQLite">>,
            description => <<"SQLite database operations - lightweight embedded database queries and management.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/sqlite">>,
            keywords => [<<"sqlite">>, <<"database">>, <<"sql">>, <<"embedded">>, <<"lightweight">>]
        },
        #{
            id => <<"memory">>,
            title => <<"Memory">>,
            description => <<"In-memory key-value storage - persistent memory across conversations, knowledge graphs.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/memory">>,
            keywords => [<<"memory">>, <<"storage">>, <<"key-value">>, <<"persist">>, <<"knowledge">>]
        },
        #{
            id => <<"puppeteer">>,
            title => <<"Puppeteer">>,
            description => <<"Browser automation - web scraping, screenshots, PDF generation, testing.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/puppeteer">>,
            keywords => [<<"browser">>, <<"puppeteer">>, <<"web">>, <<"scraping">>, <<"automation">>, <<"screenshot">>]
        },
        #{
            id => <<"brave-search">>,
            title => <<"Brave Search">>,
            description => <<"Web search using Brave Search API - search the web, get search results.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/brave-search">>,
            keywords => [<<"search">>, <<"web">>, <<"brave">>, <<"internet">>, <<"query">>]
        },
        #{
            id => <<"fetch">>,
            title => <<"Fetch">>,
            description => <<"HTTP fetch - make HTTP requests, fetch web content, API calls.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/fetch">>,
            keywords => [<<"http">>, <<"fetch">>, <<"request">>, <<"api">>, <<"web">>]
        },
        #{
            id => <<"time">>,
            title => <<"Time">>,
            description => <<"Time and date utilities - current time, timezone conversion, date calculations.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/time">>,
            keywords => [<<"time">>, <<"date">>, <<"timezone">>, <<"calendar">>, <<"clock">>]
        },
        #{
            id => <<"aws-kb-retrieval">>,
            title => <<"AWS Knowledge Base">>,
            description => <<"AWS Bedrock Knowledge Base retrieval - semantic search over documents.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/aws-kb-retrieval">>,
            keywords => [<<"aws">>, <<"knowledge">>, <<"bedrock">>, <<"retrieval">>, <<"semantic">>]
        },
        #{
            id => <<"google-maps">>,
            title => <<"Google Maps">>,
            description => <<"Google Maps integration - geocoding, directions, places, distance matrix.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/google-maps">>,
            keywords => [<<"maps">>, <<"google">>, <<"location">>, <<"directions">>, <<"places">>]
        },
        #{
            id => <<"sentry">>,
            title => <<"Sentry">>,
            description => <<"Sentry error tracking - issues, errors, performance monitoring.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/sentry">>,
            keywords => [<<"sentry">>, <<"error">>, <<"monitoring">>, <<"tracking">>, <<"debugging">>]
        },
        #{
            id => <<"sequential-thinking">>,
            title => <<"Sequential Thinking">>,
            description => <<"Step-by-step reasoning - structured thinking, problem decomposition.">>,
            github_url => <<"https://github.com/modelcontextprotocol/servers/tree/main/src/sequentialthinking">>,
            keywords => [<<"thinking">>, <<"reasoning">>, <<"sequential">>, <<"step">>, <<"logic">>]
        }
    ],

    lists:foreach(fun(Server) ->
        add_server(Server)
    end, DefaultServers),

    logger:debug("Seeded offline index with default servers", #{count => length(DefaultServers)}),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Ensure the index table exists
-spec ensure_index_exists() -> ok.
ensure_index_exists() ->
    case ets:info(?INDEX_TABLE) of
        undefined ->
            ?INDEX_TABLE = ets:new(?INDEX_TABLE, [
                set,
                public,
                named_table,
                {read_concurrency, true},
                {write_concurrency, auto}
            ]),
            ok;
        _ ->
            ok
    end.

%% @private Get all servers from index
-spec get_all_servers() -> [server_entry()].
get_all_servers() ->
    case ets:info(?INDEX_TABLE) of
        undefined -> [];
        _ ->
            [Server || {_Id, Server} <- ets:tab2list(?INDEX_TABLE)]
    end.

%% @private Convert registry entry to indexable entry
-spec registry_to_entry(atom(), map()) -> server_entry().
registry_to_entry(ServerId, Config) ->
    Description = maps:get(description, Config, <<>>),
    #{
        id => atom_to_binary(ServerId, utf8),
        title => atom_to_binary(ServerId, utf8),
        description => Description,
        keywords => extract_keywords(Description),
        metadata => #{source => registry}
    }.

%% @private Score servers using hybrid approach
-spec hybrid_score_servers(binary(), [float()], [server_entry()]) -> [search_result()].
hybrid_score_servers(Query, QueryVector, Servers) ->
    lists:map(fun(Server) ->
        TextScore = compute_text_score(Query, Server),
        VectorScore = compute_vector_score(QueryVector, Server),

        %% Combine scores with weights
        HybridScore = (TextScore * ?TEXT_WEIGHT) + (VectorScore * ?VECTOR_WEIGHT),

        erlmcp_advisor_provider:normalize_result(Server#{
            similarity => HybridScore
        })
    end, Servers).

%% @private Compute text-based similarity score
-spec compute_text_score(binary(), server_entry()) -> float().
compute_text_score(Query, Server) ->
    Title = maps:get(title, Server, <<>>),
    Description = maps:get(description, Server, <<>>),
    Keywords = maps:get(keywords, Server, []),

    %% Title similarity
    TitleSim = erlmcp_advisor_provider:compute_text_similarity(Query, Title),

    %% Description similarity
    DescSim = erlmcp_advisor_provider:compute_text_similarity(Query, Description),

    %% Keyword match bonus
    QueryTokens = erlmcp_advisor_provider:tokenize(Query),
    KeywordMatches = length([K || K <- Keywords,
                                   Q <- QueryTokens,
                                   binary:match(K, Q) /= nomatch]),
    KeywordBonus = case length(Keywords) of
        0 -> 0.0;
        N -> min(0.3, KeywordMatches / N)
    end,

    %% Combined score
    min(1.0, (TitleSim * 0.4) + (DescSim * 0.4) + (KeywordBonus * 0.2)).

%% @private Compute vector-based similarity score
-spec compute_vector_score([float()], server_entry()) -> float().
compute_vector_score(QueryVector, Server) ->
    ServerVector = maps:get(vector, Server, undefined),
    case ServerVector of
        undefined ->
            %% Generate vector on the fly
            Description = maps:get(description, Server, <<>>),
            Title = maps:get(title, Server, <<>>),
            SVector = text_to_vector(<<Title/binary, " ", Description/binary>>),
            cosine_similarity(QueryVector, SVector);
        SVector when is_list(SVector) ->
            cosine_similarity(QueryVector, SVector)
    end.

%% @private Convert text to vector representation
-spec text_to_vector(binary()) -> [float()].
text_to_vector(Text) when is_binary(Text) ->
    %% Simple character-based hashing for vector generation
    %% In production, you would use actual embeddings
    Tokens = erlmcp_advisor_provider:tokenize(Text),

    %% Create bag-of-words style vector
    Vector = lists:foldl(fun(Token, Acc) ->
        %% Hash token to position
        Hash = erlang:phash2(Token, ?VECTOR_DIM),
        %% Update position
        update_vector_position(Hash, 1.0, Acc)
    end, lists:duplicate(?VECTOR_DIM, 0.0), Tokens),

    %% Normalize vector
    normalize_vector(Vector).

%% @private Update a position in the vector
-spec update_vector_position(non_neg_integer(), float(), [float()]) -> [float()].
update_vector_position(Pos, Value, Vector) ->
    {Before, [Current | After]} = lists:split(Pos, Vector),
    Before ++ [Current + Value | After].

%% @private Normalize a vector to unit length
-spec normalize_vector([float()]) -> [float()].
normalize_vector(Vector) ->
    Magnitude = math:sqrt(lists:sum([V * V || V <- Vector])),
    case Magnitude of
        0.0 -> Vector;
        M -> [V / M || V <- Vector]
    end.

%% @private Compute cosine similarity between two vectors
-spec cosine_similarity([float()], [float()]) -> float().
cosine_similarity(V1, V2) when length(V1) =:= length(V2) ->
    DotProduct = lists:sum([A * B || {A, B} <- lists:zip(V1, V2)]),
    Mag1 = math:sqrt(lists:sum([V * V || V <- V1])),
    Mag2 = math:sqrt(lists:sum([V * V || V <- V2])),
    case Mag1 * Mag2 of
        0.0 -> 0.0;
        M -> max(0.0, min(1.0, DotProduct / M))
    end;
cosine_similarity(_, _) ->
    0.0.

%% @private Extract keywords from text
-spec extract_keywords(binary()) -> [binary()].
extract_keywords(Text) when is_binary(Text) ->
    Tokens = erlmcp_advisor_provider:tokenize(Text),

    %% Filter stopwords
    Stopwords = [<<"the">>, <<"a">>, <<"an">>, <<"and">>, <<"or">>,
                 <<"is">>, <<"are">>, <<"was">>, <<"were">>, <<"be">>,
                 <<"been">>, <<"being">>, <<"have">>, <<"has">>, <<"had">>,
                 <<"do">>, <<"does">>, <<"did">>, <<"will">>, <<"would">>,
                 <<"could">>, <<"should">>, <<"may">>, <<"might">>, <<"must">>,
                 <<"for">>, <<"to">>, <<"of">>, <<"in">>, <<"on">>, <<"at">>,
                 <<"by">>, <<"with">>, <<"from">>, <<"as">>, <<"this">>,
                 <<"that">>, <<"these">>, <<"those">>, <<"it">>, <<"its">>],

    [T || T <- Tokens,
          byte_size(T) >= 3,
          not lists:member(T, Stopwords)].
