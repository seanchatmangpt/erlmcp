-module(simple_server).

-include("erlmcp.hrl").

-export([start/0]).

start() ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true},
        prompts = #mcp_capability{name = <<"prompts">>, enabled = true}
    },
    
    {ok, Server} = erlmcp_server:start_link({stdio, []}, Capabilities),
    
    erlmcp_server:add_resource(Server, <<"file://example.txt">>, 
        fun(_Uri) -> <<"This is example content from a resource.">> end),
    
    erlmcp_server:add_resource_template(Server, <<"file://{path}">>, <<"Dynamic File">>,
        fun(Uri) ->
            case re:run(Uri, <<"file://(.+)">>, [{capture, [1], binary}]) of
                {match, [Path]} ->
                    #mcp_content{
                        type = <<"text">>,
                        text = <<"Content for path: ", Path/binary>>,
                        mime_type = <<"text/plain">>
                    };
                nomatch ->
                    <<"Invalid file URI">>
            end
        end),
    
    AddSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"a">> => #{<<"type">> => <<"number">>},
            <<"b">> => #{<<"type">> => <<"number">>}
        },
        <<"required">> => [<<"a">>, <<"b">>]
    },
    
    erlmcp_server:add_tool_with_schema(Server, <<"add">>, 
        fun(#{<<"a">> := A, <<"b">> := B}) -> 
            Result = A + B,
            #mcp_content{
                type = <<"text">>,
                text = integer_to_binary(Result),
                mime_type = <<"text/plain">>
            }
        end, AddSchema),
    
    erlmcp_server:add_tool(Server, <<"echo">>, 
        fun(#{<<"message">> := Message}) -> 
            [#mcp_content{
                type = <<"text">>,
                text = <<"Echo: ", Message/binary>>,
                mime_type = <<"text/plain">>
            }]
        end),
    
    PromptArgs = [
        #mcp_prompt_argument{name = <<"topic">>, description = <<"The topic to write about">>, required = true},
        #mcp_prompt_argument{name = <<"style">>, description = <<"Writing style">>, required = false}
    ],
    
    erlmcp_server:add_prompt_with_args(Server, <<"write_essay">>, 
        fun(#{<<"topic">> := Topic} = Args) ->
            Style = maps:get(<<"style">>, Args, <<"formal">>),
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Write a ", Style/binary, " essay about ", Topic/binary>>
                }
            }]
        end, PromptArgs),
    
    io:format("Enhanced MCP Server started with resource templates, tool schemas, and structured prompts.~n"),
    
    receive
        stop -> ok
    end.
