-module(simple_server_stdio).
-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Start the erlmcp applications first (v2.0 umbrella structure)
    application:ensure_all_started(erlmcp_core),
    application:ensure_all_started(erlmcp_transports),

    %% Enable debug logging
    logger:remove_handler(default),
    logger:add_handler(stderr_handler, logger_std_h, #{
        level => debug,
        config => #{type => standard_error}
    }),
    logger:set_primary_config(level, debug),

    %% Also enable info level for console output
    logger:set_handler_config(default, config, #{type => standard_io}),

    logger:info("Starting MCP server with debug logging enabled...~n"),

    %% Start the stdio MCP server
    case erlmcp_stdio:start() of
        ok ->
            logger:debug("Successfully started stdio server~n"),
            setup_server(),
            logger:debug("Server setup complete, waiting for shutdown...~n"),
            wait_for_shutdown();
        {error, Reason} ->
            logger:error("Failed to start stdio server: ~p", [Reason]),
            halt(1)
    end.

setup_server() ->
    %% Add tools
    ok = erlmcp_stdio:add_tool(<<"echo">>, <<"Echo back a message">>,
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

    ok = erlmcp_stdio:add_tool(<<"add">>, <<"Add two numbers">>,
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

    ok = erlmcp_stdio:add_tool(<<"system_info">>, <<"Get system information">>,
        fun(_) ->
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
    ok = erlmcp_stdio:add_resource(<<"file://example.txt">>, <<"example.txt">>,
        fun(_Uri) ->
            <<"This is example content from a stdio MCP resource.">>
        end),

    %% Add prompts
    ok = erlmcp_stdio:add_prompt(<<"write_essay">>, <<"Generate essay writing prompt">>,
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

    logger:info("Server configured and ready~n").

wait_for_shutdown() ->
    %% The server will run until the stdio protocol terminates (EOF)
    %% Monitor the stdio server process to know when it's done
    case whereis(erlmcp_stdio_server) of  % Changed from erlmcp_stdio_supervisor
        undefined ->
            logger:warn("Stdio server not found, exiting~n");
        Pid ->
            monitor(process, Pid),
            receive
                {'DOWN', _Ref, process, Pid, _Reason} ->
                    logger:info("Stdio server terminated, exiting~n")
            end
    end.
