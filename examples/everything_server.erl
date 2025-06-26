-module(everything_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PAGE_SIZE, 10).
-define(TOTAL_RESOURCES, 100).

-record(state, {
    server_pid,
    subscriptions = sets:new(),
    subs_timer,
    logs_timer,
    stderr_timer,
    log_level = debug
}).

start() ->
    start_link().

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true},
        prompts = #mcp_capability{name = <<"prompts">>, enabled = true},
        logging = #mcp_capability{name = <<"logging">>, enabled = true}
    },
    
    {ok, ServerPid} = erlmcp_server:start_link({stdio, []}, Capabilities),
    
    setup_resources(ServerPid),
    setup_tools(ServerPid),
    setup_prompts(ServerPid),
    
    SubsTimer = erlang:send_after(10000, self(), send_subscription_updates),
    LogsTimer = erlang:send_after(20000, self(), send_log_message),
    StderrTimer = erlang:send_after(30000, self(), send_stderr_message),
    
    {ok, #state{
        server_pid = ServerPid,
        subs_timer = SubsTimer,
        logs_timer = LogsTimer,
        stderr_timer = StderrTimer
    }}.

setup_resources(ServerPid) ->
    lists:foreach(fun(Id) ->
        Uri = <<"test://static/resource/", (integer_to_binary(Id))/binary>>,
        Name = <<"Resource ", (integer_to_binary(Id))/binary>>,
        Resource = #mcp_resource{
            uri = Uri,
            name = Name,
            description = <<"Test resource ", (integer_to_binary(Id))/binary>>,
            mime_type = case Id rem 2 of
                0 -> <<"text/plain">>;
                1 -> <<"application/octet-stream">>
            end
        },
        Handler = fun(_) -> generate_resource_content(Id) end,
        erlmcp_server:add_resource(ServerPid, Uri, Handler)
    end, lists:seq(1, ?TOTAL_RESOURCES)),
    
    erlmcp_server:add_resource_template(ServerPid, <<"test://static/resource/{id}">>, 
        <<"Static Resource Template">>,
        fun(Uri) ->
            case binary:split(Uri, <<"/">>, [global]) of
                [<<"test:">>, <<>>, <<"static">>, <<"resource">>, IdBin] ->
                    try
                        Id = binary_to_integer(IdBin),
                        case Id >= 1 andalso Id =< ?TOTAL_RESOURCES of
                            true -> generate_resource_content(Id);
                            false -> {error, <<"Resource ID out of range">>}
                        end
                    catch
                        _:_ -> {error, <<"Invalid resource ID">>}
                    end;
                _ ->
                    {error, <<"Invalid resource URI">>}
            end
        end).

setup_tools(ServerPid) ->
    EchoSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"message">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Message to echo">>
            }
        },
        <<"required">> => [<<"message">>]
    },
    
    AddSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"a">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"First number">>
            },
            <<"b">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"Second number">>
            }
        },
        <<"required">> => [<<"a">>, <<"b">>]
    },
    
    LongRunningSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"duration">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"Duration of the operation in seconds">>,
                <<"default">> => 10
            },
            <<"steps">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"Number of steps in the operation">>,
                <<"default">> => 5
            }
        }
    },
    
    PrintEnvSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{}
    },
    
    SampleLLMSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"prompt">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"The prompt to send to the LLM">>
            },
            <<"maxTokens">> => #{
                <<"type">> => <<"number">>,
                <<"description">> => <<"Maximum number of tokens to generate">>,
                <<"default">> => 100
            }
        },
        <<"required">> => [<<"prompt">>]
    },
    
    GetTinyImageSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{}
    },
    
    AnnotatedMessageSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"messageType">> => #{
                <<"type">> => <<"string">>,
                <<"enum">> => [<<"error">>, <<"success">>, <<"debug">>],
                <<"description">> => <<"Type of message to demonstrate different annotation patterns">>
            },
            <<"includeImage">> => #{
                <<"type">> => <<"boolean">>,
                <<"description">> => <<"Whether to include an example image">>,
                <<"default">> => false
            }
        },
        <<"required">> => [<<"messageType">>]
    },
    
    GetResourceRefSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"resourceId">> => #{
                <<"type">> => <<"number">>,
                <<"minimum">> => 1,
                <<"maximum">> => 100,
                <<"description">> => <<"ID of the resource to reference (1-100)">>
            }
        },
        <<"required">> => [<<"resourceId">>]
    },
    
    erlmcp_server:add_tool_with_schema(ServerPid, <<"echo">>, fun handle_echo/1, EchoSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"add">>, fun handle_add/1, AddSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"longRunningOperation">>, fun handle_long_running/1, LongRunningSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"printEnv">>, fun handle_print_env/1, PrintEnvSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"sampleLLM">>, fun handle_sample_llm/1, SampleLLMSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"getTinyImage">>, fun handle_get_tiny_image/1, GetTinyImageSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"annotatedMessage">>, fun handle_annotated_message/1, AnnotatedMessageSchema),
    erlmcp_server:add_tool_with_schema(ServerPid, <<"getResourceReference">>, fun handle_get_resource_ref/1, GetResourceRefSchema).

setup_prompts(ServerPid) ->
    SimplePromptArgs = [],
    ComplexPromptArgs = [
        #mcp_prompt_argument{
            name = <<"temperature">>,
            description = <<"Temperature setting">>,
            required = true
        },
        #mcp_prompt_argument{
            name = <<"style">>,
            description = <<"Output style">>,
            required = false
        }
    ],
    ResourcePromptArgs = [
        #mcp_prompt_argument{
            name = <<"resourceId">>,
            description = <<"Resource ID to include (1-100)">>,
            required = true
        }
    ],
    
    erlmcp_server:add_prompt_with_args(ServerPid, <<"simple_prompt">>, 
        fun handle_simple_prompt/1, SimplePromptArgs),
    erlmcp_server:add_prompt_with_args(ServerPid, <<"complex_prompt">>, 
        fun handle_complex_prompt/1, ComplexPromptArgs),
    erlmcp_server:add_prompt_with_args(ServerPid, <<"resource_prompt">>, 
        fun handle_resource_prompt/1, ResourcePromptArgs).

generate_resource_content(Id) ->
    case Id rem 2 of
        0 ->
            #mcp_content{
                type = <<"text">>,
                text = <<"Resource ", (integer_to_binary(Id))/binary, ": This is a plaintext resource">>,
                mime_type = <<"text/plain">>
            };
        1 ->
            Data = <<"Resource ", (integer_to_binary(Id))/binary, ": This is a base64 blob">>,
            #mcp_content{
                type = <<"text">>,
                data = base64:encode(Data),
                mime_type = <<"application/octet-stream">>
            }
    end.

handle_echo(#{<<"message">> := Message}) ->
    [#mcp_content{
        type = <<"text">>,
        text = <<"Echo: ", Message/binary>>,
        mime_type = <<"text/plain">>
    }].

handle_add(#{<<"a">> := A, <<"b">> := B}) ->
    Sum = A + B,
    SumBin = number_to_binary(Sum),
    [#mcp_content{
        type = <<"text">>,
        text = <<"The sum of ", (number_to_binary(A))/binary, " and ", (number_to_binary(B))/binary, " is ", SumBin/binary, ".">>,
        mime_type = <<"text/plain">>
    }].

handle_long_running(Args) ->
    Duration = maps:get(<<"duration">>, Args, 10),
    Steps = maps:get(<<"steps">>, Args, 5),
    StepDuration = Duration / Steps,
    
    lists:foreach(fun(Step) ->
        timer:sleep(round(StepDuration * 1000)),
        io:format("Step ~p/~p completed~n", [Step, Steps])
    end, lists:seq(1, Steps)),
    
    [#mcp_content{
        type = <<"text">>,
        text = <<"Long running operation completed. Duration: ", (number_to_binary(Duration))/binary, " seconds, Steps: ", (number_to_binary(Steps))/binary, ".">>,
        mime_type = <<"text/plain">>
    }].

handle_print_env(_Args) ->
    EnvVars = [
        {<<"PATH">>, list_to_binary(os:getenv("PATH", ""))},
        {<<"HOME">>, list_to_binary(os:getenv("HOME", ""))},
        {<<"USER">>, list_to_binary(os:getenv("USER", ""))},
        {<<"SHELL">>, list_to_binary(os:getenv("SHELL", ""))}
    ],
    EnvJson = jsx:encode(maps:from_list(EnvVars)),
    [#mcp_content{
        type = <<"text">>,
        text = EnvJson,
        mime_type = <<"application/json">>
    }].

handle_sample_llm(#{<<"prompt">> := Prompt} = Args) ->
    MaxTokens = maps:get(<<"maxTokens">>, Args, 100),
    [#mcp_content{
        type = <<"text">>,
        text = <<"LLM sampling result for prompt: '", Prompt/binary, "' (max tokens: ", (number_to_binary(MaxTokens))/binary, ") - This is a mock response since actual LLM sampling requires client support.">>,
        mime_type = <<"text/plain">>
    }].

handle_get_tiny_image(_Args) ->
    TinyImageData = get_tiny_image_data(),
    [
        #mcp_content{
            type = <<"text">>,
            text = <<"This is a tiny image:">>,
            mime_type = <<"text/plain">>
        },
        #mcp_content{
            type = <<"image">>,
            data = TinyImageData,
            mime_type = <<"image/png">>
        },
        #mcp_content{
            type = <<"text">>,
            text = <<"The image above is the MCP tiny image.">>,
            mime_type = <<"text/plain">>
        }
    ].

handle_annotated_message(#{<<"messageType">> := MessageType} = Args) ->
    IncludeImage = maps:get(<<"includeImage">>, Args, false),
    
    BaseContent = case MessageType of
        <<"error">> ->
            [#{
                type => <<"text">>,
                text => <<"Error: Operation failed">>,
                annotations => #{
                    priority => 1.0,
                    audience => [<<"user">>, <<"assistant">>]
                }
            }];
        <<"success">> ->
            [#{
                type => <<"text">>,
                text => <<"Operation completed successfully">>,
                annotations => #{
                    priority => 0.7,
                    audience => [<<"user">>]
                }
            }];
        <<"debug">> ->
            [#{
                type => <<"text">>,
                text => <<"Debug: Cache hit ratio 0.95, latency 150ms">>,
                annotations => #{
                    priority => 0.3,
                    audience => [<<"assistant">>]
                }
            }]
    end,
    
    case IncludeImage of
        true ->
            ImageContent = #{
                type => <<"image">>,
                data => get_tiny_image_data(),
                mime_type => <<"image/png">>,
                annotations => #{
                    priority => 0.5,
                    audience => [<<"user">>]
                }
            },
            BaseContent ++ [ImageContent];
        false ->
            BaseContent
    end.

handle_get_resource_ref(#{<<"resourceId">> := ResourceId}) ->
    case ResourceId >= 1 andalso ResourceId =< ?TOTAL_RESOURCES of
        true ->
            Uri = <<"test://static/resource/", (integer_to_binary(ResourceId))/binary>>,
            Resource = #mcp_resource{
                uri = Uri,
                name = <<"Resource ", (integer_to_binary(ResourceId))/binary>>,
                description = <<"Test resource ", (integer_to_binary(ResourceId))/binary>>,
                mime_type = case ResourceId rem 2 of
                    0 -> <<"text/plain">>;
                    1 -> <<"application/octet-stream">>
                end
            },
            [
                #mcp_content{
                    type = <<"text">>,
                    text = <<"Returning resource reference for Resource ", (integer_to_binary(ResourceId))/binary, ":">>,
                    mime_type = <<"text/plain">>
                },
                #mcp_content{
                    type = <<"resource">>,
                    text = jsx:encode(#{
                        uri => Uri,
                        name => <<"Resource ", (integer_to_binary(ResourceId))/binary>>,
                        mimeType => Resource#mcp_resource.mime_type
                    }),
                    mime_type = <<"application/json">>
                },
                #mcp_content{
                    type = <<"text">>,
                    text = <<"You can access this resource using the URI: ", Uri/binary>>,
                    mime_type = <<"text/plain">>
                }
            ];
        false ->
            throw({error, <<"Resource ID out of range">>})
    end.

handle_simple_prompt(_Args) ->
    [#{
        role => <<"user">>,
        content => #{
            type => <<"text">>,
            text => <<"This is a simple prompt without arguments.">>
        }
    }].

handle_complex_prompt(Args) ->
    Temperature = maps:get(<<"temperature">>, Args, <<"0.7">>),
    Style = maps:get(<<"style">>, Args, <<"formal">>),
    TinyImageData = get_tiny_image_data(),
    
    [
        #{
            role => <<"user">>,
            content => #{
                type => <<"text">>,
                text => <<"This is a complex prompt with arguments: temperature=", Temperature/binary, ", style=", Style/binary>>
            }
        },
        #{
            role => <<"assistant">>,
            content => #{
                type => <<"text">>,
                text => <<"I understand. You've provided a complex prompt with temperature and style arguments. How would you like me to proceed?">>
            }
        },
        #{
            role => <<"user">>,
            content => #{
                type => <<"image">>,
                data => TinyImageData,
                mime_type => <<"image/png">>
            }
        }
    ].

handle_resource_prompt(#{<<"resourceId">> := ResourceIdBin}) ->
    ResourceId = case is_binary(ResourceIdBin) of
        true -> binary_to_integer(ResourceIdBin);
        false -> ResourceIdBin
    end,
    
    case ResourceId >= 1 andalso ResourceId =< ?TOTAL_RESOURCES of
        true ->
            Uri = <<"test://static/resource/", (integer_to_binary(ResourceId))/binary>>,
            Resource = #{
                uri => Uri,
                name => <<"Resource ", (integer_to_binary(ResourceId))/binary>>,
                mimeType => case ResourceId rem 2 of
                    0 -> <<"text/plain">>;
                    1 -> <<"application/octet-stream">>
                end
            },
            [
                #{
                    role => <<"user">>,
                    content => #{
                        type => <<"text">>,
                        text => <<"This prompt includes Resource ", (integer_to_binary(ResourceId))/binary, ". Please analyze the following resource:">>
                    }
                },
                #{
                    role => <<"user">>,
                    content => #{
                        type => <<"resource">>,
                        resource => Resource
                    }
                }
            ];
        false ->
            throw({error, <<"Invalid resourceId. Must be a number between 1 and 100.">>})
    end.

get_tiny_image_data() ->
    <<"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">>.

number_to_binary(N) when is_integer(N) -> integer_to_binary(N);
number_to_binary(N) when is_float(N) -> float_to_binary(N, [{decimals, 2}]).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({subscribe, Uri}, #state{subscriptions = Subs} = State) ->
    NewSubs = sets:add_element(Uri, Subs),
    {noreply, State#state{subscriptions = NewSubs}};

handle_cast({unsubscribe, Uri}, #state{subscriptions = Subs} = State) ->
    NewSubs = sets:del_element(Uri, Subs),
    {noreply, State#state{subscriptions = NewSubs}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send_subscription_updates, #state{server_pid = ServerPid, subscriptions = Subs} = State) ->
    sets:fold(fun(Uri, _) ->
        erlmcp_server:send_notification(ServerPid, <<"notifications/resources/updated">>, #{uri => Uri})
    end, ok, Subs),
    
    Timer = erlang:send_after(10000, self(), send_subscription_updates),
    {noreply, State#state{subs_timer = Timer}};

handle_info(send_log_message, #state{server_pid = ServerPid, log_level = LogLevel} = State) ->
    Messages = [
        #{level => debug, data => <<"Debug-level message">>},
        #{level => info, data => <<"Info-level message">>},
        #{level => notice, data => <<"Notice-level message">>},
        #{level => warning, data => <<"Warning-level message">>},
        #{level => error, data => <<"Error-level message">>},
        #{level => critical, data => <<"Critical-level message">>},
        #{level => alert, data => <<"Alert level-message">>},
        #{level => emergency, data => <<"Emergency-level message">>}
    ],
    
    RandomMessage = lists:nth(rand:uniform(length(Messages)), Messages),
    case should_send_log_message(maps:get(level, RandomMessage), LogLevel) of
        true ->
            erlmcp_server:send_notification(ServerPid, <<"notifications/message">>, RandomMessage);
        false ->
            ok
    end,
    
    Timer = erlang:send_after(20000, self(), send_log_message),
    {noreply, State#state{logs_timer = Timer}};

handle_info(send_stderr_message, #state{server_pid = ServerPid} = State) ->
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:local_time(),
    Timestamp = io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Min, Sec]),
    Message = iolist_to_binary([Timestamp, ": A stderr message"]),
    
    erlmcp_server:send_notification(ServerPid, <<"notifications/stderr">>, #{content => Message}),
    
    Timer = erlang:send_after(30000, self(), send_stderr_message),
    {noreply, State#state{stderr_timer = Timer}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{subs_timer = SubsTimer, logs_timer = LogsTimer, stderr_timer = StderrTimer}) ->
    case SubsTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(SubsTimer)
    end,
    case LogsTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(LogsTimer)
    end,
    case StderrTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(StderrTimer)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

should_send_log_message(MessageLevel, CurrentLevel) ->
    Levels = [debug, info, notice, warning, error, critical, alert, emergency],
    MessageIndex = get_level_index(MessageLevel, Levels),
    CurrentIndex = get_level_index(CurrentLevel, Levels),
    MessageIndex >= CurrentIndex.

get_level_index(Level, Levels) ->
    case lists:member(Level, Levels) of
        true -> 
            length(lists:takewhile(fun(L) -> L =/= Level end, Levels)) + 1;
        false -> 
            1
    end.
