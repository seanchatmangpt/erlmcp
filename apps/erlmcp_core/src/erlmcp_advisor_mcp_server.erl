%%% @doc MCP Server Integration for Advisor
%%%
%%% Exposes the MCP Advisor service as an MCP server with tools and prompts.
%%% This allows AI assistants to discover and recommend MCP servers
%%% using the standard MCP protocol.
%%%
%%% Tools:
%%% - advisor/search: Search for MCP servers
%%% - advisor/recommend: Get recommendations for a use case
%%% - advisor/install: Get installation instructions
%%%
%%% @end
-module(erlmcp_advisor_mcp_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/0,
    start_link/1,
    stop/0,
    get_server/0
]).

%% Tool handlers
-export([
    handle_search/1,
    handle_recommend/1,
    handle_install/1,
    handle_list_providers/1,
    handle_status/1
]).

%%====================================================================
%% Macros
%%====================================================================

-define(SERVER_ID, erlmcp_advisor_server).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the advisor MCP server with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the advisor MCP server with custom configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    %% Create server capabilities
    Capabilities = #mcp_server_capabilities{
        tools = #mcp_tools_capability{listChanged = true},
        prompts = #mcp_prompts_capability{listChanged = false}
    },

    %% Start the MCP server
    case erlmcp_server:start_link(?SERVER_ID, Capabilities) of
        {ok, Pid} ->
            %% Register tools
            register_tools(Pid, Config),

            %% Register prompts
            register_prompts(Pid),

            %% Register with the registry
            erlmcp_registry:register_server(local, ?SERVER_ID, Pid, #{
                capabilities => Capabilities,
                description => <<"MCP Advisor - discover and recommend MCP servers">>
            }),

            logger:info("MCP Advisor server started", #{pid => Pid}),
            {ok, Pid};
        Error ->
            Error
    end.

%% @doc Stop the advisor MCP server
-spec stop() -> ok.
stop() ->
    case whereis(?SERVER_ID) of
        undefined -> ok;
        Pid ->
            erlmcp_registry:unregister_server(local, ?SERVER_ID),
            erlmcp_server:stop(Pid)
    end.

%% @doc Get the advisor MCP server pid
-spec get_server() -> {ok, pid()} | {error, not_running}.
get_server() ->
    case whereis(?SERVER_ID) of
        undefined -> {error, not_running};
        Pid -> {ok, Pid}
    end.

%%====================================================================
%% Tool Handlers
%%====================================================================

%% @doc Handle advisor/search tool call
-spec handle_search(map()) -> [#mcp_content{}].
handle_search(Args) ->
    Query = maps:get(<<"query">>, Args, <<>>),
    Limit = maps:get(<<"limit">>, Args, 10),
    MinSimilarity = maps:get(<<"minSimilarity">>, Args, 0.3),

    case erlmcp_advisor:search(Query, #{
        limit => Limit,
        min_similarity => MinSimilarity
    }) of
        {ok, Results} ->
            ResultsJson = format_search_results(Results),
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => true,
                    count => length(Results),
                    results => ResultsJson
                })
            }];
        {error, Reason} ->
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => false,
                    error => format_error(Reason)
                })
            }]
    end.

%% @doc Handle advisor/recommend tool call
-spec handle_recommend(map()) -> [#mcp_content{}].
handle_recommend(Args) ->
    Query = maps:get(<<"query">>, Args, <<>>),
    Limit = maps:get(<<"limit">>, Args, 5),

    case erlmcp_advisor:recommend(Query, #{limit => Limit}) of
        {ok, Recommendation} ->
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => true,
                    recommendation => format_recommendation(Recommendation)
                })
            }];
        {error, Reason} ->
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => false,
                    error => format_error(Reason)
                })
            }]
    end.

%% @doc Handle advisor/install tool call
-spec handle_install(map()) -> [#mcp_content{}].
handle_install(Args) ->
    ServerName = maps:get(<<"server">>, Args, <<>>),
    Platform = maps:get(<<"platform">>, Args, <<"generic">>),

    Instructions = generate_install_instructions(ServerName, Platform),

    [#mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = Instructions
    }].

%% @doc Handle advisor/list_providers tool call
-spec handle_list_providers(map()) -> [#mcp_content{}].
handle_list_providers(_Args) ->
    Providers = erlmcp_advisor:list_providers(),

    ProviderList = [#{
        id => Id,
        module => atom_to_binary(Module, utf8)
    } || {Id, Module} <- Providers],

    [#mcp_content{
        type = ?MCP_CONTENT_TYPE_TEXT,
        text = jsx:encode(#{
            success => true,
            providers => ProviderList
        })
    }].

%% @doc Handle advisor/status tool call
-spec handle_status(map()) -> [#mcp_content{}].
handle_status(_Args) ->
    case erlmcp_advisor:get_status() of
        {ok, Status} ->
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => true,
                    status => Status
                })
            }];
        {error, Reason} ->
            [#mcp_content{
                type = ?MCP_CONTENT_TYPE_TEXT,
                text = jsx:encode(#{
                    success => false,
                    error => format_error(Reason)
                })
            }]
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Register tools with the MCP server
-spec register_tools(pid(), map()) -> ok.
register_tools(Server, _Config) ->
    %% advisor/search tool
    SearchSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"query">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Natural language search query for MCP servers">>
            },
            <<"limit">> => #{
                <<"type">> => <<"integer">>,
                <<"description">> => <<"Maximum number of results (default: 10)">>,
                <<"default">> => 10
            },
            <<"minSimilarity">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"Minimum similarity threshold 0-1 (default: 0.3)">>,
                <<"default">> => 0.3
            }
        },
        <<"required">> => [<<"query">>]
    },
    erlmcp_server:add_tool_full(Server, <<"advisor/search">>,
        <<"Search for MCP servers using natural language. "
          "Find servers for specific tasks like file operations, "
          "database access, web scraping, or any other functionality.">>,
        fun handle_search/1,
        #{input_schema => SearchSchema}),

    %% advisor/recommend tool
    RecommendSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"query">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Describe your use case or what you want to accomplish">>
            },
            <<"limit">> => #{
                <<"type">> => <<"integer">>,
                <<"description">> => <<"Number of candidate servers to consider (default: 5)">>,
                <<"default">> => 5
            }
        },
        <<"required">> => [<<"query">>]
    },
    erlmcp_server:add_tool_full(Server, <<"advisor/recommend">>,
        <<"Get a recommendation for the best MCP server for your use case. "
          "Provides confidence score and reasons for the recommendation.">>,
        fun handle_recommend/1,
        #{input_schema => RecommendSchema}),

    %% advisor/install tool
    InstallSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"server">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Name or ID of the MCP server to install">>
            },
            <<"platform">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Target platform (generic, claude, vscode)">>,
                <<"enum">> => [<<"generic">>, <<"claude">>, <<"vscode">>],
                <<"default">> => <<"generic">>
            }
        },
        <<"required">> => [<<"server">>]
    },
    erlmcp_server:add_tool_full(Server, <<"advisor/install">>,
        <<"Get installation instructions for an MCP server. "
          "Provides configuration snippets for different platforms.">>,
        fun handle_install/1,
        #{input_schema => InstallSchema}),

    %% advisor/list_providers tool
    erlmcp_server:add_tool_full(Server, <<"advisor/list_providers">>,
        <<"List all registered search providers in the advisor.">>,
        fun handle_list_providers/1,
        #{input_schema => #{<<"type">> => <<"object">>, <<"properties">> => #{}}}),

    %% advisor/status tool
    erlmcp_server:add_tool_full(Server, <<"advisor/status">>,
        <<"Get the current status of the advisor service including cache stats.">>,
        fun handle_status/1,
        #{input_schema => #{<<"type">> => <<"object">>, <<"properties">> => #{}}}),

    ok.

%% @private Register prompts with the MCP server
-spec register_prompts(pid()) -> ok.
register_prompts(Server) ->
    %% MCP discovery prompt
    erlmcp_server:add_prompt_with_args(Server, <<"discover-mcp">>,
        fun(_Args) ->
            [#{
                role => ?MCP_ROLE_SYSTEM,
                content => #{
                    type => ?MCP_CONTENT_TYPE_TEXT,
                    text => <<"You are an expert at helping users discover MCP servers. "
                              "Use the advisor/search and advisor/recommend tools to find "
                              "the best MCP servers for their needs.">>
                }
            }]
        end,
        []),

    %% Setup guide prompt
    erlmcp_server:add_prompt_with_args(Server, <<"mcp-setup-guide">>,
        fun(Args) ->
            ServerName = maps:get(<<"server">>, Args, <<"filesystem">>),
            [#{
                role => ?MCP_ROLE_SYSTEM,
                content => #{
                    type => ?MCP_CONTENT_TYPE_TEXT,
                    text => iolist_to_binary([
                        <<"Help the user set up the ">>, ServerName, <<" MCP server. ">>,
                        <<"First use advisor/install to get the configuration, ">>,
                        <<"then guide them through the setup process.">>
                    ])
                }
            }]
        end,
        [#mcp_prompt_argument{
            name = <<"server">>,
            description = <<"Name of the MCP server to set up">>,
            required = false
        }]),

    ok.

%% @private Format search results for JSON output
-spec format_search_results([map()]) -> [map()].
format_search_results(Results) ->
    [format_result(R) || R <- Results].

%% @private Format a single search result
-spec format_result(map()) -> map().
format_result(Result) ->
    #{
        id => maps:get(id, Result, <<>>),
        title => maps:get(title, Result, <<>>),
        description => maps:get(description, Result, <<>>),
        similarity => maps:get(similarity, Result, 0.0),
        url => maps:get(url, Result, null),
        github_url => maps:get(github_url, Result, null),
        provider => maps:get(provider, Result, unknown)
    }.

%% @private Format recommendation for JSON output
-spec format_recommendation(map()) -> map().
format_recommendation(Recommendation) ->
    #{
        server => format_result(maps:get(server, Recommendation, #{})),
        confidence => maps:get(confidence, Recommendation, 0.0),
        reasons => maps:get(reasons, Recommendation, [])
    }.

%% @private Format error for JSON output
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) -> Reason;
format_error(Reason) when is_atom(Reason) -> atom_to_binary(Reason, utf8);
format_error(Reason) when is_list(Reason) -> list_to_binary(Reason);
format_error({Type, Details}) ->
    iolist_to_binary([atom_to_binary(Type, utf8), <<": ">>,
                      format_error(Details)]);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @private Generate installation instructions
-spec generate_install_instructions(binary(), binary()) -> binary().
generate_install_instructions(ServerName, Platform) ->
    case Platform of
        <<"claude">> ->
            generate_claude_config(ServerName);
        <<"vscode">> ->
            generate_vscode_config(ServerName);
        _ ->
            generate_generic_config(ServerName)
    end.

%% @private Generate Claude Desktop configuration
-spec generate_claude_config(binary()) -> binary().
generate_claude_config(ServerName) ->
    ConfigPath = case os:type() of
        {unix, darwin} ->
            <<"~/Library/Application Support/Claude/claude_desktop_config.json">>;
        {win32, _} ->
            <<"%AppData%\\Claude\\claude_desktop_config.json">>;
        _ ->
            <<"~/.config/claude/claude_desktop_config.json">>
    end,

    iolist_to_binary([
        <<"# Installation Instructions for ">>, ServerName, <<" (Claude Desktop)\n\n">>,
        <<"## Configuration File Location\n">>,
        ConfigPath, <<"\n\n">>,
        <<"## Configuration Snippet\n">>,
        <<"Add the following to your `mcpServers` section:\n\n">>,
        <<"```json\n">>,
        <<"{\n">>,
        <<"  \"mcpServers\": {\n">>,
        <<"    \"">>, ServerName, <<"\": {\n">>,
        <<"      \"command\": \"npx\",\n">>,
        <<"      \"args\": [\"-y\", \"@modelcontextprotocol/server-">>, ServerName, <<"\"]\n">>,
        <<"    }\n">>,
        <<"  }\n">>,
        <<"}\n">>,
        <<"```\n\n">>,
        <<"## Next Steps\n">>,
        <<"1. Open the configuration file\n">>,
        <<"2. Add the configuration snippet\n">>,
        <<"3. Restart Claude Desktop\n">>
    ]).

%% @private Generate VS Code configuration
-spec generate_vscode_config(binary()) -> binary().
generate_vscode_config(ServerName) ->
    iolist_to_binary([
        <<"# Installation Instructions for ">>, ServerName, <<" (VS Code)\n\n">>,
        <<"## Add to settings.json\n\n">>,
        <<"```json\n">>,
        <<"{\n">>,
        <<"  \"mcp.servers\": {\n">>,
        <<"    \"">>, ServerName, <<"\": {\n">>,
        <<"      \"command\": \"npx\",\n">>,
        <<"      \"args\": [\"-y\", \"@modelcontextprotocol/server-">>, ServerName, <<"\"]\n">>,
        <<"    }\n">>,
        <<"  }\n">>,
        <<"}\n">>,
        <<"```\n">>
    ]).

%% @private Generate generic configuration
-spec generate_generic_config(binary()) -> binary().
generate_generic_config(ServerName) ->
    iolist_to_binary([
        <<"# Installation Instructions for ">>, ServerName, <<"\n\n">>,
        <<"## Using npm/npx\n\n">>,
        <<"```bash\n">>,
        <<"npx -y @modelcontextprotocol/server-">>, ServerName, <<"\n">>,
        <<"```\n\n">>,
        <<"## Using Docker\n\n">>,
        <<"```bash\n">>,
        <<"docker run --rm -it mcp/server-">>, ServerName, <<"\n">>,
        <<"```\n\n">>,
        <<"## Generic MCP Configuration\n\n">>,
        <<"```json\n">>,
        <<"{\n">>,
        <<"  \"command\": \"npx\",\n">>,
        <<"  \"args\": [\"-y\", \"@modelcontextprotocol/server-">>, ServerName, <<"\"],\n">>,
        <<"  \"transport\": \"stdio\"\n">>,
        <<"}\n">>,
        <<"```\n">>
    ]).
