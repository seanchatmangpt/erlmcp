-module(simple_server_tcp).

-include_lib("erlmcp/include/erlmcp.hrl").

-export([start/0, start/1, main/1]).

%% Default port
-define(DEFAULT_PORT, 8765).

start() ->
    start([]).

start(Args) ->
    main(Args).

main(Args) ->
    %% Parse port from arguments or use default
    Port = case Args of
        [PortStr] when is_list(PortStr) ->
            try list_to_integer(PortStr)
            catch _:_ -> ?DEFAULT_PORT
            end;
        [PortInt] when is_integer(PortInt) ->
            PortInt;
        _ ->
            ?DEFAULT_PORT
    end,

    %% Start applications
    io:format(standard_error, "Starting erlmcp applications...~n", []),
    application:ensure_all_started(erlmcp),
    io:format(standard_error, "Applications started~n", []),

    %% Create server capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true},
        prompts = #mcp_capability{name = <<"prompts">>, enabled = true}
    },

    %% Start TCP listener
    io:format(standard_error, "Starting TCP listener on port ~p...~n", [Port]),
    {ok, ListenSocket} = gen_tcp:listen(Port, [
        binary,
        {packet, line},
        {active, false},
        {reuseaddr, true},
        {keepalive, true}
    ]),

    io:format(standard_error, "TCP MCP Server listening on port ~p~n", [Port]),
    io:format(standard_error, "Waiting for connections...~n", []),

    %% Accept connections in a loop
    accept_loop(ListenSocket, Capabilities).

accept_loop(ListenSocket, Capabilities) ->
    case gen_tcp:accept(ListenSocket) of
        {ok, Socket} ->
            io:format(standard_error, "Client connected~n", []),
            %% Spawn a process to handle this client
            spawn(fun() -> handle_client(Socket, Capabilities) end),
            %% Continue accepting more connections
            accept_loop(ListenSocket, Capabilities);
        {error, Reason} ->
            io:format(standard_error, "Accept failed: ~p~n", [Reason]),
            gen_tcp:close(ListenSocket)
    end.

handle_client(Socket, Capabilities) ->
    %% Create a custom MCP server for this client connection
    %% We'll implement a simple JSON-RPC handler directly

    %% Set up tools and resources
    Tools = setup_tools(),
    Resources = setup_resources(),
    Prompts = setup_prompts(),

    %% Handle messages from this client
    client_loop(Socket, #{
        capabilities => Capabilities,
        tools => Tools,
        resources => Resources,
        prompts => Prompts,
        initialized => false
    }).

client_loop(Socket, State) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            %% Remove newline and parse JSON
            CleanData = string:trim(binary_to_list(Data)),
            case CleanData of
                "" ->
                    client_loop(Socket, State);
                _ ->
                    NewState = handle_message(Socket, CleanData, State),
                    client_loop(Socket, NewState)
            end;
        {error, closed} ->
            io:format(standard_error, "Client disconnected~n", []),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format(standard_error, "Client error: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.

handle_message(Socket, Message, State) ->
    try jsx:decode(list_to_binary(Message), [return_maps]) of
        #{<<"method">> := <<"initialize">>, <<"id">> := Id} ->
            send_initialize_response(Socket, Id),
            State#{initialized => true};
        #{<<"method">> := <<"tools/list">>, <<"id">> := Id} ->
            send_tools_list(Socket, Id, maps:get(tools, State)),
            State;
        #{<<"method">> := <<"resources/list">>, <<"id">> := Id} ->
            send_resources_list(Socket, Id, maps:get(resources, State)),
            State;
        #{<<"method">> := <<"tools/call">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_tool_call(Socket, Id, Params, maps:get(tools, State)),
            State;
        #{<<"method">> := <<"resources/read">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_resource_read(Socket, Id, Params, maps:get(resources, State)),
            State;
        #{<<"method">> := <<"prompts/list">>, <<"id">> := Id} ->
            send_prompts_list(Socket, Id, maps:get(prompts, State)),
            State;
        #{<<"method">> := <<"prompts/get">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_prompt_get(Socket, Id, Params, maps:get(prompts, State)),
            State;
        _ ->
            %% Unknown method, ignore
            State
    catch
        _:Error ->
            io:format(standard_error, "JSON parse error: ~p~n", [Error]),
            State
    end.

send_response(Socket, Response) ->
    Json = jsx:encode(Response),
    gen_tcp:send(Socket, [Json, "\n"]).

send_initialize_response(Socket, Id) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2024-11-05">>,
            <<"capabilities">> => #{
                <<"tools">> => #{},
                <<"resources">> => #{},
                <<"prompts">> => #{}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"tcp-erlmcp">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    send_response(Socket, Response).

send_tools_list(Socket, Id, Tools) ->
    ToolsList = [Tool || {_Name, Tool, _Handler} <- Tools],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"tools">> => ToolsList
        }
    },
    send_response(Socket, Response).

send_resources_list(Socket, Id, Resources) ->
    ResourcesList = [Resource || {_Uri, Resource, _Handler} <- Resources],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"resources">> => ResourcesList
        }
    },
    send_response(Socket, Response).

send_prompts_list(Socket, Id, Prompts) ->
    PromptsList = [Prompt || {_Name, Prompt, _Handler} <- Prompts],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"prompts">> => PromptsList
        }
    },
    send_response(Socket, Response).

handle_tool_call(Socket, Id, #{<<"name">> := Name, <<"arguments">> := Args}, Tools) ->
    case lists:keyfind(Name, 1, Tools) of
        {Name, _Tool, Handler} ->
            try
                Result = Handler(Args),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => Result
                        }]
                    }
                },
                send_response(Socket, Response)
            catch
                Class:Reason ->
                    io:format(standard_error, "Tool error: ~p:~p~n", [Class, Reason]),
                    send_error(Socket, Id, -32603, <<"Internal error">>)
            end;
        false ->
            send_error(Socket, Id, -32601, <<"Tool not found">>)
    end.

handle_resource_read(Socket, Id, #{<<"uri">> := Uri}, Resources) ->
    case lists:keyfind(Uri, 1, Resources) of
        {Uri, _Resource, Handler} ->
            try
                Content = Handler(Uri),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"contents">> => [#{
                            <<"uri">> => Uri,
                            <<"mimeType">> => <<"text/plain">>,
                            <<"text">> => Content
                        }]
                    }
                },
                send_response(Socket, Response)
            catch
                Class:Reason ->
                    io:format(standard_error, "Resource error: ~p:~p~n", [Class, Reason]),
                    send_error(Socket, Id, -32603, <<"Internal error">>)
            end;
        false ->
            send_error(Socket, Id, -32602, <<"Resource not found">>)
    end.

handle_prompt_get(Socket, Id, #{<<"name">> := Name} = Params, Prompts) ->
    case lists:keyfind(Name, 1, Prompts) of
        {Name, _Prompt, Handler} ->
            try
                Args = maps:get(<<"arguments">>, Params, #{}),
                Messages = Handler(Args),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"messages">> => Messages
                    }
                },
                send_response(Socket, Response)
            catch
                Class:Reason ->
                    io:format(standard_error, "Prompt error: ~p:~p~n", [Class, Reason]),
                    send_error(Socket, Id, -32603, <<"Internal error">>)
            end;
        false ->
            send_error(Socket, Id, -32602, <<"Prompt not found">>)
    end.

send_error(Socket, Id, Code, Message) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    },
    send_response(Socket, Response).

%% Setup functions
setup_tools() ->
    [
        {<<"echo">>, #{
            <<"name">> => <<"echo">>,
            <<"description">> => <<"Echo back a message">>
        }, fun(#{<<"message">> := Msg}) ->
            <<"TCP Echo: ", Msg/binary>>
        end},

        {<<"add">>, #{
            <<"name">> => <<"add">>,
            <<"description">> => <<"Add two numbers">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"a">> => #{<<"type">> => <<"number">>},
                    <<"b">> => #{<<"type">> => <<"number">>}
                },
                <<"required">> => [<<"a">>, <<"b">>]
            }
        }, fun(#{<<"a">> := A, <<"b">> := B}) ->
            Result = A + B,
            if
                is_integer(Result) -> integer_to_binary(Result);
                is_float(Result) -> float_to_binary(Result, [{decimals, 2}])
            end
        end},

        {<<"system_info">>, #{
            <<"name">> => <<"system_info">>,
            <<"description">> => <<"Get system information">>
        }, fun(_Args) ->
            {ok, Hostname} = inet:gethostname(),
            iolist_to_binary(io_lib:format("Hostname: ~s, Erlang: ~s, TCP Server Working!",
                                           [Hostname, erlang:system_info(otp_release)]))
        end}
    ].

setup_resources() ->
    [
        {<<"file://example.txt">>, #{
            <<"uri">> => <<"file://example.txt">>,
            <<"name">> => <<"example.txt">>,
            <<"mimeType">> => <<"text/plain">>
        }, fun(_Uri) ->
            <<"This is example content from a TCP MCP resource.">>
        end}
    ].

setup_prompts() ->
    [
        {<<"write_essay">>, #{
            <<"name">> => <<"write_essay">>,
            <<"description">> => <<"Generate essay writing prompt">>,
            <<"arguments">> => [
                #{<<"name">> => <<"topic">>, <<"description">> => <<"Topic to write about">>, <<"required">> => true},
                #{<<"name">> => <<"style">>, <<"description">> => <<"Writing style">>, <<"required">> => false}
            ]
        }, fun(#{<<"topic">> := Topic} = Args) ->
            Style = maps:get(<<"style">>, Args, <<"formal">>),
            [#{
                <<"role">> => <<"user">>,
                <<"content">> => #{
                    <<"type">> => <<"text">>,
                    <<"text">> => <<"Write a ", Style/binary, " essay about ", Topic/binary, " (via TCP MCP)">>
                }
            }]
        end}
    ].
