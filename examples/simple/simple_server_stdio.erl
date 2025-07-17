-module(simple_server_stdio).
-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Start applications
    application:ensure_all_started(jsx),
    
    %% Initialize stdio server
    {ok, _} = erlmcp_stdio_server:start(),
    
    %% Add tools
    erlmcp_stdio_server:add_tool(<<"echo">>, <<"Echo back a message">>, 
        fun(#{<<"message">> := Message}) ->
            <<"Echo: ", Message/binary>>
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"message">> => #{<<"type">> => <<"string">>}
            },
            <<"required">> => [<<"message">>]
        }),
    
    erlmcp_stdio_server:add_tool(<<"add">>, <<"Add two numbers">>, 
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A + B,
            if
                is_integer(Result) -> integer_to_binary(Result);
                is_float(Result) -> float_to_binary(Result, [{decimals, 2}])
            end
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"a">> => #{<<"type">> => <<"number">>},
                <<"b">> => #{<<"type">> => <<"number">>}
            },
            <<"required">> => [<<"a">>, <<"b">>]
        }),
    
    erlmcp_stdio_server:add_tool(<<"system_info">>, <<"Get system information">>, 
        fun(_Args) ->
            {ok, Hostname} = inet:gethostname(),
            iolist_to_binary(io_lib:format("Hostname: ~s, Erlang: ~s, stdio working!",
                                           [Hostname, erlang:system_info(otp_release)]))
        end,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{},
            <<"required">> => []
        }),
    
    %% Add resources
    erlmcp_stdio_server:add_resource(<<"file://example.txt">>, <<"example.txt">>, 
        fun(_Uri) ->
            <<"This is example content from a stdio MCP resource.">>
        end),
    
    %% Add prompts
    erlmcp_stdio_server:add_prompt(<<"write_essay">>, <<"Generate essay writing prompt">>, 
        fun(Args) ->
            Topic = maps:get(<<"topic">>, Args, <<"general topic">>),
            Style = maps:get(<<"style">>, Args, <<"formal">>),
            
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Write a ", Style/binary, " essay about ", Topic/binary>>
                }
            }]
        end,
        [
            #{<<"name">> => <<"topic">>, <<"description">> => <<"Topic to write about">>, <<"required">> => true},
            #{<<"name">> => <<"style">>, <<"description">> => <<"Writing style">>, <<"required">> => false}
        ]),
    
    %% Run the server
    erlmcp_stdio_server:run().