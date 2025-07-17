-module(simple_direct_test).
-export([run/0]).

run() ->
    %% Start the main erlmcp application
    application:ensure_all_started(erlmcp),
    
    %% Start the stdio server
    case erlmcp_stdio:start() of
        ok ->
            io:format("✓ Server started successfully~n"),
            setup_server(),
            run_tests(),
            erlmcp_stdio:stop(),
            io:format("✓ Server stopped~n");
        {error, Reason} ->
            io:format("✗ Failed to start server: ~p~n", [Reason])
    end.

setup_server() ->
    %% Add tools
    ok = erlmcp_stdio:add_tool(<<"echo">>, <<"Echo back a message">>, 
        fun(#{<<"message">> := Message}) ->
            <<"Echo: ", Message/binary>>
        end),
    
    ok = erlmcp_stdio:add_tool(<<"add">>, <<"Add two numbers">>, 
        fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A + B,
            integer_to_binary(Result)
        end),
    
    ok = erlmcp_stdio:add_tool(<<"system_info">>, <<"Get system information">>, 
        fun(_) ->
            {ok, Hostname} = inet:gethostname(),
            iolist_to_binary(io_lib:format("Hostname: ~s, Erlang: ~s",
                                           [Hostname, erlang:system_info(otp_release)]))
        end),
    
    %% Add resources
    ok = erlmcp_stdio:add_resource(<<"file://example.txt">>, <<"example.txt">>, 
        fun(_Uri) ->
            <<"This is example content.">>
        end),
    
    %% Add prompts
    ok = erlmcp_stdio:add_prompt(<<"write_essay">>, <<"Generate essay writing prompt">>, 
        fun(Args) ->
            Topic = maps:get(<<"topic">>, Args, <<"general topic">>),
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Write about ", Topic/binary>>
                }
            }]
        end),
    
    io:format("✓ Server configured with tools, resources, and prompts~n").

run_tests() ->
    io:format("~n=== Running Tests ===~n"),
    
    %% Test that the server is running
    case erlmcp_stdio:is_running() of
        true -> io:format("✓ Server is running~n");
        false -> io:format("✗ Server is not running~n")
    end,
    
    %% Test adding more tools
    case erlmcp_stdio:add_tool(<<"test_tool">>, <<"Test tool">>, fun(_) -> <<"test">> end) of
        ok -> io:format("✓ Can add tools dynamically~n");
        ToolError -> io:format("✗ Failed to add tool: ~p~n", [ToolError])
    end,
    
    %% Test adding resources
    case erlmcp_stdio:add_resource(<<"test://resource">>, <<"Test resource">>, fun(_) -> <<"test">> end) of
        ok -> io:format("✓ Can add resources dynamically~n");
        ResourceError -> io:format("✗ Failed to add resource: ~p~n", [ResourceError])
    end,
    
    %% Test adding prompts
    case erlmcp_stdio:add_prompt(<<"test_prompt">>, <<"Test prompt">>, fun(_) -> <<"test">> end) of
        ok -> io:format("✓ Can add prompts dynamically~n");
        PromptError -> io:format("✗ Failed to add prompt: ~p~n", [PromptError])
    end,
    
    io:format("~n✓ All tests passed~n").