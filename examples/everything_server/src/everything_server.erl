-module(everything_server).
-behaviour(gen_server).

-include_lib("erlmcp/include/erlmcp.hrl").

%% API
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Server state
-record(state, {
    mcp_server :: pid(),
    subscriptions = sets:new() :: sets:set(),
    timers = #{} :: #{atom() => reference()},
    log_level = debug :: atom(),
    intervals :: #{atom() => pos_integer()}
}).

-define(SERVER, ?MODULE).
-define(PAGE_SIZE, 10).
-define(TOTAL_RESOURCES, 100).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link({stdio, []}).

start_link(TransportConfig) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [TransportConfig], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([TransportConfig]) ->
    process_flag(trap_exit, true),
    
    %% Get configuration from application environment
    LogLevel = application:get_env(everything_server, log_level, debug),
    Intervals = #{
        subscription => application:get_env(everything_server, subscription_interval, 10000),
        log => application:get_env(everything_server, log_interval, 20000),
        stderr => application:get_env(everything_server, stderr_interval, 30000)
    },
    
    %% Initialize MCP server capabilities
    Capabilities = #mcp_server_capabilities{
        resources = #mcp_capability{name = <<"resources">>, enabled = true},
        tools = #mcp_capability{name = <<"tools">>, enabled = true},
        prompts = #mcp_capability{name = <<"prompts">>, enabled = true},
        logging = #mcp_capability{name = <<"logging">>, enabled = true}
    },
    
    %% Start the MCP server
    {ok, McpServer} = erlmcp_server:start_link(TransportConfig, Capabilities),
    
    %% Setup all MCP components
    ok = setup_resources(McpServer),
    ok = setup_tools(McpServer),
    ok = setup_prompts(McpServer),
    
    %% Schedule periodic tasks
    Timers = schedule_periodic_tasks(Intervals),
    
    State = #state{
        mcp_server = McpServer,
        log_level = LogLevel,
        intervals = Intervals,
        timers = Timers
    },
    
    logger:info("Everything Server started with ~p transport", [element(1, TransportConfig)]),
    
    {ok, State}.

handle_call({subscribe, Uri}, _From, #state{subscriptions = Subs} = State) ->
    NewSubs = sets:add_element(Uri, Subs),
    {reply, ok, State#state{subscriptions = NewSubs}};

handle_call({unsubscribe, Uri}, _From, #state{subscriptions = Subs} = State) ->
    NewSubs = sets:del_element(Uri, Subs),
    {reply, ok, State#state{subscriptions = NewSubs}};

handle_call(get_state, _From, State) ->
    {reply, {ok, State}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(send_subscription_updates, State) ->
    send_subscription_notifications(State),
    Timer = schedule_timer(subscription, maps:get(subscription, State#state.intervals)),
    NewTimers = maps:put(subscription, Timer, State#state.timers),
    {noreply, State#state{timers = NewTimers}};

handle_info(send_log_message, State) ->
    send_random_log_message(State),
    Timer = schedule_timer(log, maps:get(log, State#state.intervals)),
    NewTimers = maps:put(log, Timer, State#state.timers),
    {noreply, State#state{timers = NewTimers}};

handle_info(send_stderr_message, State) ->
    send_stderr_notification(State),
    Timer = schedule_timer(stderr, maps:get(stderr, State#state.intervals)),
    NewTimers = maps:put(stderr, Timer, State#state.timers),
    {noreply, State#state{timers = NewTimers}};

handle_info({'EXIT', Pid, Reason}, #state{mcp_server = Pid} = State) ->
    logger:error("MCP server process died: ~p", [Reason]),
    {stop, {mcp_server_died, Reason}, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Cancel all timers
    maps:foreach(fun(_Type, Timer) ->
        erlang:cancel_timer(Timer)
    end, State#state.timers),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions - Setup
%%====================================================================

setup_resources(McpServer) ->
    %% Add static resources
    lists:foreach(fun(Id) ->
        Uri = build_resource_uri(Id),
        Handler = fun(_) -> generate_resource_content(Id) end,
        erlmcp_server:add_resource(McpServer, Uri, Handler)
    end, lists:seq(1, ?TOTAL_RESOURCES)),
    
    %% Add resource template
    erlmcp_server:add_resource_template(
        McpServer,
        <<"test://static/resource/{id}">>,
        <<"Static Resource Template">>,
        fun(Uri) -> handle_resource_template(Uri) end
    ),
    ok.

setup_tools(McpServer) ->
    %% Define tool schemas
    ToolDefinitions = [
        {<<"echo">>, fun handle_echo/1, echo_schema()},
        {<<"add">>, fun handle_add/1, add_schema()},
        {<<"longRunningOperation">>, fun handle_long_running/1, long_running_schema()},
        {<<"printEnv">>, fun handle_print_env/1, print_env_schema()},
        {<<"sampleLLM">>, fun handle_sample_llm/1, sample_llm_schema()},
        {<<"getTinyImage">>, fun handle_get_tiny_image/1, get_tiny_image_schema()},
        {<<"annotatedMessage">>, fun handle_annotated_message/1, annotated_message_schema()},
        {<<"getResourceReference">>, fun handle_get_resource_ref/1, get_resource_ref_schema()}
    ],
    
    lists:foreach(fun({Name, Handler, Schema}) ->
        erlmcp_server:add_tool_with_schema(McpServer, Name, Handler, Schema)
    end, ToolDefinitions),
    ok.

setup_prompts(McpServer) ->
    %% Simple prompt
    erlmcp_server:add_prompt_with_args(
        McpServer,
        <<"simple_prompt">>,
        fun handle_simple_prompt/1,
        []
    ),
    
    %% Complex prompt
    erlmcp_server:add_prompt_with_args(
        McpServer,
        <<"complex_prompt">>,
        fun handle_complex_prompt/1,
        [
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
        ]
    ),
    
    %% Resource prompt
    erlmcp_server:add_prompt_with_args(
        McpServer,
        <<"resource_prompt">>,
        fun handle_resource_prompt/1,
        [
            #mcp_prompt_argument{
                name = <<"resourceId">>,
                description = <<"Resource ID to include (1-100)">>,
                required = true
            }
        ]
    ),
    ok.

%%====================================================================
%% Internal functions - Periodic Tasks
%%====================================================================

schedule_periodic_tasks(Intervals) ->
    #{
        subscription => schedule_timer(subscription, maps:get(subscription, Intervals)),
        log => schedule_timer(log, maps:get(log, Intervals)),
        stderr => schedule_timer(stderr, maps:get(stderr, Intervals))
    }.

schedule_timer(subscription, Interval) ->
    erlang:send_after(Interval, self(), send_subscription_updates);
schedule_timer(log, Interval) ->
    erlang:send_after(Interval, self(), send_log_message);
schedule_timer(stderr, Interval) ->
    erlang:send_after(Interval, self(), send_stderr_message).

send_subscription_notifications(#state{mcp_server = McpServer, subscriptions = Subs}) ->
    sets:fold(fun(Uri, _) ->
        erlmcp_server:notify_resource_updated(McpServer, Uri, #{
            <<"timestamp">> => erlang:system_time(second),
            <<"source">> => <<"periodic_update">>
        })
    end, ok, Subs).

send_random_log_message(#state{mcp_server = McpServer, log_level = CurrentLevel}) ->
    LogLevels = [debug, info, notice, warning, error, critical, alert, emergency],
    Messages = [
        {debug, <<"Debug-level message">>},
        {info, <<"Info-level message">>},
        {notice, <<"Notice-level message">>},
        {warning, <<"Warning-level message">>},
        {error, <<"Error-level message">>},
        {critical, <<"Critical-level message">>},
        {alert, <<"Alert-level message">>},
        {emergency, <<"Emergency-level message">>}
    ],
    
    {Level, Message} = lists:nth(rand:uniform(length(Messages)), Messages),
    
    case should_send_log_message(Level, CurrentLevel, LogLevels) of
        true ->
            erlmcp_server:send_notification(McpServer, <<"notifications/message">>, #{
                <<"level">> => atom_to_binary(Level),
                <<"data">> => Message
            });
        false ->
            ok
    end.

send_stderr_notification(#state{mcp_server = McpServer}) ->
    Timestamp = format_timestamp(erlang:timestamp()),
    Message = iolist_to_binary([Timestamp, <<": A stderr message">>]),
    
    erlmcp_server:send_notification(McpServer, <<"notifications/stderr">>, #{
        <<"content">> => Message
    }).

should_send_log_message(MessageLevel, CurrentLevel, Levels) ->
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

%%====================================================================
%% Internal functions - Resources
%%====================================================================

build_resource_uri(Id) ->
    <<"test://static/resource/", (integer_to_binary(Id))/binary>>.

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

handle_resource_template(Uri) ->
    case parse_resource_id(Uri) of
        {ok, Id} when Id >= 1, Id =< ?TOTAL_RESOURCES ->
            generate_resource_content(Id);
        {ok, _} ->
            #mcp_content{
                type = <<"text">>,
                text = <<"Resource ID out of range">>,
                mime_type = <<"text/plain">>
            };
        error ->
            #mcp_content{
                type = <<"text">>,
                text = <<"Invalid resource URI">>,
                mime_type = <<"text/plain">>
            }
    end.

parse_resource_id(Uri) ->
    case binary:split(Uri, <<"/">>, [global]) of
        [<<"test:">>, <<>>, <<"static">>, <<"resource">>, IdBin] ->
            try
                {ok, binary_to_integer(IdBin)}
            catch
                _:_ -> error
            end;
        _ ->
            error
    end.

%%====================================================================
%% Internal functions - Tool Handlers
%%====================================================================

handle_echo(#{<<"message">> := Message}) ->
    [#mcp_content{
        type = <<"text">>,
        text = <<"Echo: ", Message/binary>>,
        mime_type = <<"text/plain">>
    }].

handle_add(#{<<"a">> := A, <<"b">> := B}) ->
    Sum = A + B,
    [#mcp_content{
        type = <<"text">>,
        text = format_number_result(A, B, Sum),
        mime_type = <<"text/plain">>
    }].

handle_long_running(Args) ->
    Duration = maps:get(<<"duration">>, Args, 10),
    Steps = maps:get(<<"steps">>, Args, 5),
    StepDuration = Duration / Steps,
    
    %% Simulate long running operation
    lists:foreach(fun(Step) ->
        timer:sleep(round(StepDuration * 1000)),
        logger:info("Step ~p/~p completed", [Step, Steps])
    end, lists:seq(1, Steps)),
    
    [#mcp_content{
        type = <<"text">>,
        text = format_long_running_result(Duration, Steps),
        mime_type = <<"text/plain">>
    }].

handle_print_env(_Args) ->
    EnvVars = get_environment_variables(),
    [#mcp_content{
        type = <<"text">>,
        text = jsx:encode(maps:from_list(EnvVars)),
        mime_type = <<"application/json">>
    }].

handle_sample_llm(#{<<"prompt">> := Prompt} = Args) ->
    MaxTokens = maps:get(<<"maxTokens">>, Args, 100),
    [#mcp_content{
        type = <<"text">>,
        text = format_llm_result(Prompt, MaxTokens),
        mime_type = <<"text/plain">>
    }].

handle_get_tiny_image(_Args) ->
    [
        #mcp_content{
            type = <<"text">>,
            text = <<"This is a tiny image:">>,
            mime_type = <<"text/plain">>
        },
        #mcp_content{
            type = <<"image">>,
            data = get_tiny_image_data(),
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
    
    BaseContent = create_annotated_content(MessageType),
    
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
    case validate_resource_id(ResourceId) of
        {ok, Id} ->
            Uri = build_resource_uri(Id),
            create_resource_reference_content(Id, Uri);
        error ->
            throw({error, <<"Resource ID out of range">>})
    end.

%%====================================================================
%% Internal functions - Prompt Handlers
%%====================================================================

handle_simple_prompt(_Args) ->
    [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => <<"This is a simple prompt without arguments.">>
        }
    }].

handle_complex_prompt(Args) ->
    Temperature = maps:get(<<"temperature">>, Args, <<"0.7">>),
    Style = maps:get(<<"style">>, Args, <<"formal">>),
    
    [
        #{
            <<"role">> => <<"user">>,
            <<"content">> => #{
                <<"type">> => <<"text">>,
                <<"text">> => format_complex_prompt(Temperature, Style)
            }
        },
        #{
            <<"role">> => <<"assistant">>,
            <<"content">> => #{
                <<"type">> => <<"text">>,
                <<"text">> => <<"I understand. You've provided a complex prompt with temperature and style arguments. How would you like me to proceed?">>
            }
        },
        #{
            <<"role">> => <<"user">>,
            <<"content">> => #{
                <<"type">> => <<"image">>,
                <<"data">> => get_tiny_image_data(),
                <<"mime_type">> => <<"image/png">>
            }
        }
    ].

handle_resource_prompt(#{<<"resourceId">> := ResourceIdBin}) ->
    case parse_prompt_resource_id(ResourceIdBin) of
        {ok, ResourceId} ->
            create_resource_prompt_messages(ResourceId);
        error ->
            throw({error, <<"Invalid resourceId. Must be a number between 1 and 100.">>})
    end.

%%====================================================================
%% Internal functions - Schemas
%%====================================================================

echo_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"message">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Message to echo">>
            }
        },
        <<"required">> => [<<"message">>]
    }.

add_schema() ->
    #{
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
    }.

long_running_schema() ->
    #{
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
    }.

print_env_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{}
    }.

sample_llm_schema() ->
    #{
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
    }.

get_tiny_image_schema() ->
    #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{}
    }.

annotated_message_schema() ->
    #{
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
    }.

get_resource_ref_schema() ->
    #{
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
    }.

%%====================================================================
%% Internal functions - Helpers
%%====================================================================

format_timestamp({_MegaSecs, _Secs, _MicroSecs} = Timestamp) ->
    {{_Year, _Month, _Day}, {Hour, Min, Sec}} = calendar:now_to_datetime(Timestamp),
    io_lib:format("~2..0w:~2..0w:~2..0w", [Hour, Min, Sec]).

format_number_result(A, B, Sum) ->
    iolist_to_binary(io_lib:format("The sum of ~p and ~p is ~p.", [A, B, Sum])).

format_long_running_result(Duration, Steps) ->
    iolist_to_binary(io_lib:format(
        "Long running operation completed. Duration: ~p seconds, Steps: ~p.", 
        [Duration, Steps]
    )).

format_llm_result(Prompt, MaxTokens) ->
    iolist_to_binary(io_lib:format(
        "LLM sampling result for prompt: '~s' (max tokens: ~p) - This is a mock response since actual LLM sampling requires client support.",
        [Prompt, MaxTokens]
    )).

format_complex_prompt(Temperature, Style) ->
    <<"This is a complex prompt with arguments: temperature=", Temperature/binary, 
      ", style=", Style/binary>>.

get_environment_variables() ->
    [
        {<<"PATH">>, safe_getenv("PATH")},
        {<<"HOME">>, safe_getenv("HOME")},
        {<<"USER">>, safe_getenv("USER")},
        {<<"SHELL">>, safe_getenv("SHELL")}
    ].

safe_getenv(Key) ->
    case os:getenv(Key) of
        false -> <<"">>;
        Value -> list_to_binary(Value)
    end.

create_annotated_content(<<"error">>) ->
    [#{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Error: Operation failed">>,
        <<"annotations">> => #{
            <<"priority">> => 1.0,
            <<"audience">> => [<<"user">>, <<"assistant">>]
        }
    }];
create_annotated_content(<<"success">>) ->
    [#{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Operation completed successfully">>,
        <<"annotations">> => #{
            <<"priority">> => 0.7,
            <<"audience">> => [<<"user">>]
        }
    }];
create_annotated_content(<<"debug">>) ->
    [#{
        <<"type">> => <<"text">>,
        <<"text">> => <<"Debug: Cache hit ratio 0.95, latency 150ms">>,
        <<"annotations">> => #{
            <<"priority">> => 0.3,
            <<"audience">> => [<<"assistant">>]
        }
    }].

validate_resource_id(ResourceId) when is_integer(ResourceId) ->
    case ResourceId >= 1 andalso ResourceId =< ?TOTAL_RESOURCES of
        true -> {ok, ResourceId};
        false -> error
    end;
validate_resource_id(_) ->
    error.

parse_prompt_resource_id(ResourceIdBin) when is_binary(ResourceIdBin) ->
    try
        ResourceId = binary_to_integer(ResourceIdBin),
        validate_resource_id(ResourceId)
    catch
        _:_ -> error
    end;
parse_prompt_resource_id(ResourceId) when is_integer(ResourceId) ->
    validate_resource_id(ResourceId);
parse_prompt_resource_id(_) ->
    error.

create_resource_reference_content(Id, Uri) ->
    [
        #mcp_content{
            type = <<"text">>,
            text = <<"Returning resource reference for Resource ", (integer_to_binary(Id))/binary, ":">>,
            mime_type = <<"text/plain">>
        },
        #mcp_content{
            type = <<"resource">>,
            text = jsx:encode(#{
                <<"uri">> => Uri,
                <<"name">> => <<"Resource ", (integer_to_binary(Id))/binary>>,
                <<"mimeType">> => get_resource_mime_type(Id)
            }),
            mime_type = <<"application/json">>
        },
        #mcp_content{
            type = <<"text">>,
            text = <<"You can access this resource using the URI: ", Uri/binary>>,
            mime_type = <<"text/plain">>
        }
    ].

create_resource_prompt_messages(ResourceId) ->
    Uri = build_resource_uri(ResourceId),
    [
        #{
            <<"role">> => <<"user">>,
            <<"content">> => #{
                <<"type">> => <<"text">>,
                <<"text">> => <<"This prompt includes Resource ", (integer_to_binary(ResourceId))/binary, 
                               ". Please analyze the following resource:">>
            }
        },
        #{
            <<"role">> => <<"user">>,
            <<"content">> => #{
                <<"type">> => <<"resource">>,
                <<"resource">> => #{
                    <<"uri">> => Uri,
                    <<"name">> => <<"Resource ", (integer_to_binary(ResourceId))/binary>>,
                    <<"mimeType">> => get_resource_mime_type(ResourceId)
                }
            }
        }
    ].

get_resource_mime_type(Id) ->
    case Id rem 2 of
        0 -> <<"text/plain">>;
        1 -> <<"application/octet-stream">>
    end.

%% Tiny image data (1x1 transparent PNG)
get_tiny_image_data() ->
    <<"iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==">>.
