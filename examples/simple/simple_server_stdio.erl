-module(simple_server_stdio).
-export([start/0, main/1]).

start() ->
    main([]).

main(_Args) ->
    %% Start applications quietly
    application:ensure_all_started(jsx),

    %% Handle stdio JSON-RPC directly
    message_loop().

message_loop() ->
    case io:get_line("") of
        eof ->
            ok;
        Line when is_list(Line) ->
            CleanLine = string:trim(Line),
            case CleanLine of
                "" -> message_loop();
                _ ->
                    handle_message(CleanLine),
                    message_loop()
            end;
        {error, _} ->
            ok
    end.

handle_message(Message) ->
    try jsx:decode(list_to_binary(Message), [return_maps]) of
        #{<<"method">> := <<"initialize">>, <<"id">> := Id} ->
            io:format(standard_error, "Handling initialize request~n", []),
            send_initialize_response(Id);
        #{<<"method">> := <<"notifications/initialized">>} ->
            io:format(standard_error, "Received initialized notification~n", []),
            ok; % No response needed for notifications
        #{<<"method">> := <<"tools/list">>, <<"id">> := Id} ->
            io:format(standard_error, "Handling tools/list request~n", []),
            send_tools_list(Id);
        #{<<"method">> := <<"resources/list">>, <<"id">> := Id} ->
            io:format(standard_error, "Handling resources/list request~n", []),
            send_resources_list(Id);
        #{<<"method">> := <<"tools/call">>, <<"id">> := Id, <<"params">> := Params} ->
            io:format(standard_error, "Handling tools/call request~n", []),
            handle_tool_call(Id, Params);
        #{<<"method">> := <<"resources/read">>, <<"id">> := Id, <<"params">> := Params} ->
            io:format(standard_error, "Handling resources/read request~n", []),
            handle_resource_read(Id, Params);
        #{<<"method">> := <<"prompts/list">>, <<"id">> := Id} ->
            io:format(standard_error, "Handling prompts/list request~n", []),
            send_prompts_list(Id);
        #{<<"method">> := <<"prompts/get">>, <<"id">> := Id, <<"params">> := Params} ->
            io:format(standard_error, "Handling prompts/get request~n", []),
            handle_prompt_get(Id, Params);
        Other ->
            io:format(standard_error, "Unknown message: ~p~n", [Other]),
            ok
    catch
        Error:Reason ->
            io:format(standard_error, "JSON parse error: ~p:~p~n", [Error, Reason]),
            ok
    end.

send_response(Response) ->
    Json = jsx:encode(Response),
    io:format("~s~n", [Json]).

send_initialize_response(Id) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"protocolVersion">> => <<"2025-06-18">>,
            <<"capabilities">> => #{
                <<"tools">> => #{<<"listChanged">> => false},
                <<"resources">> => #{<<"subscribe">> => false, <<"listChanged">> => false},
                <<"prompts">> => #{<<"listChanged">> => false}
            },
            <<"serverInfo">> => #{
                <<"name">> => <<"stdio-erlmcp">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    send_response(Response).

send_tools_list(Id) ->
    Tools = [
        #{
            <<"name">> => <<"echo">>,
            <<"description">> => <<"Echo back a message">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"message">> => #{<<"type">> => <<"string">>}
                },
                <<"required">> => [<<"message">>]
            }
        },
        #{
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
        },
        #{
            <<"name">> => <<"system_info">>,
            <<"description">> => <<"Get system information">>,
            <<"inputSchema">> => #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{},
                <<"required">> => []
            }
        }
    ],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"tools">> => Tools
        }
    },
    send_response(Response).

send_resources_list(Id) ->
    Resources = [
        #{
            <<"uri">> => <<"file://example.txt">>,
            <<"name">> => <<"example.txt">>,
            <<"mimeType">> => <<"text/plain">>
        }
    ],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"resources">> => Resources
        }
    },
    send_response(Response).

send_prompts_list(Id) ->
    Prompts = [
        #{
            <<"name">> => <<"write_essay">>,
            <<"description">> => <<"Generate essay writing prompt">>,
            <<"arguments">> => [
                #{<<"name">> => <<"topic">>, <<"description">> => <<"Topic to write about">>, <<"required">> => true},
                #{<<"name">> => <<"style">>, <<"description">> => <<"Writing style">>, <<"required">> => false}
            ]
        }
    ],
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"prompts">> => Prompts
        }
    },
    send_response(Response).

handle_tool_call(Id, #{<<"name">> := <<"echo">>, <<"arguments">> := #{<<"message">> := Message}}) ->
    Result = <<"Echo: ", Message/binary>>,
    send_tool_response(Id, Result);

handle_tool_call(Id, #{<<"name">> := <<"add">>, <<"arguments">> := #{<<"a">> := A, <<"b">> := B}}) ->
    Result = A + B,
    ResultText = if
        is_integer(Result) -> integer_to_binary(Result);
        is_float(Result) -> float_to_binary(Result, [{decimals, 2}])
    end,
    send_tool_response(Id, ResultText);

handle_tool_call(Id, #{<<"name">> := <<"system_info">>}) ->
    {ok, Hostname} = inet:gethostname(),
    Result = iolist_to_binary(io_lib:format("Hostname: ~s, Erlang: ~s, stdio working!",
                                           [Hostname, erlang:system_info(otp_release)])),
    send_tool_response(Id, Result);

handle_tool_call(Id, _) ->
    send_error(Id, -32601, <<"Tool not found">>).

handle_resource_read(Id, #{<<"uri">> := <<"file://example.txt">>}) ->
    Content = <<"This is example content from a stdio MCP resource.">>,
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"contents">> => [#{
                <<"uri">> => <<"file://example.txt">>,
                <<"mimeType">> => <<"text/plain">>,
                <<"text">> => Content
            }]
        }
    },
    send_response(Response);

handle_resource_read(Id, _) ->
    send_error(Id, -32602, <<"Resource not found">>).

handle_prompt_get(Id, #{<<"name">> := <<"write_essay">>} = Params) ->
    Args = maps:get(<<"arguments">>, Params, #{}),
    Topic = maps:get(<<"topic">>, Args, <<"general topic">>),
    Style = maps:get(<<"style">>, Args, <<"formal">>),

    Messages = [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"Write a ", Style/binary, " essay about ", Topic/binary>>
        }
    }],

    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"messages">> => Messages
        }
    },
    send_response(Response);

handle_prompt_get(Id, _) ->
    send_error(Id, -32602, <<"Prompt not found">>).

send_tool_response(Id, Result) ->
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
    send_response(Response).

send_error(Id, Code, Message) ->
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => Code,
            <<"message">> => Message
        }
    },
    send_response(Response).
