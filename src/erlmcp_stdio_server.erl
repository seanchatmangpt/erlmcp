-module(erlmcp_stdio_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start/0, start/1,
    add_tool/3, add_tool/4,
    add_resource/3, add_resource/4,
    add_prompt/3, add_prompt/4,
    run/0,
    stop/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type tool_handler() :: fun((map()) -> binary() | iolist()).
-type resource_handler() :: fun((binary()) -> binary() | iolist()).
-type prompt_handler() :: fun((map()) -> binary() | iolist() | [map()]).

-record(state, {
    tools = #{} :: #{binary() => {binary(), tool_handler(), map() | undefined}},
    resources = #{} :: #{binary() => {binary(), resource_handler()}},
    prompts = #{} :: #{binary() => {binary(), prompt_handler(), [map()] | undefined}},
    initialized = false :: boolean()
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start(#{}).

-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Options) ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []) of
        {ok, Pid} ->
            put(stdio_server, Pid),
            {ok, Pid};
        Error ->
            Error
    end.

-spec add_tool(binary(), binary(), tool_handler()) -> ok.
add_tool(Name, Description, Handler) ->
    add_tool(Name, Description, Handler, undefined).

-spec add_tool(binary(), binary(), tool_handler(), map() | undefined) -> ok.
add_tool(Name, Description, Handler, Schema) when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    Server = get_server(),
    gen_server:call(Server, {add_tool, Name, Description, Handler, Schema}).

-spec add_resource(binary(), binary(), resource_handler()) -> ok.
add_resource(Uri, Description, Handler) ->
    add_resource(Uri, Description, Handler, <<"text/plain">>).

-spec add_resource(binary(), binary(), resource_handler(), binary()) -> ok.
add_resource(Uri, Description, Handler, MimeType) when is_binary(Uri), is_binary(Description), is_function(Handler, 1) ->
    Server = get_server(),
    gen_server:call(Server, {add_resource, Uri, Description, Handler, MimeType}).

-spec add_prompt(binary(), binary(), prompt_handler()) -> ok.
add_prompt(Name, Description, Handler) ->
    add_prompt(Name, Description, Handler, undefined).

-spec add_prompt(binary(), binary(), prompt_handler(), [map()] | undefined) -> ok.
add_prompt(Name, Description, Handler, Arguments) when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    Server = get_server(),
    gen_server:call(Server, {add_prompt, Name, Description, Handler, Arguments}).

-spec run() -> ok.
run() ->
    Server = get_server(),
    gen_server:call(Server, start_message_loop, infinity).

-spec stop() -> ok.
stop() ->
    case get(stdio_server) of
        undefined -> ok;
        Server -> gen_server:stop(Server)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([_Options]) ->
    process_flag(trap_exit, true),
    application:ensure_all_started(jsx),
    {ok, #state{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({add_tool, Name, Description, Handler, Schema}, _From, State) ->
    NewTools = maps:put(Name, {Description, Handler, Schema}, State#state.tools),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_resource, Uri, Description, Handler, _MimeType}, _From, State) ->
    NewResources = maps:put(Uri, {Description, Handler}, State#state.resources),
    {reply, ok, State#state{resources = NewResources}};

handle_call({add_prompt, Name, Description, Handler, Arguments}, _From, State) ->
    NewPrompts = maps:put(Name, {Description, Handler, Arguments}, State#state.prompts),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call(start_message_loop, _From, State) ->
    % This will block the calling process in the message loop
    message_loop(State),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec get_server() -> pid().
get_server() ->
    case get(stdio_server) of
        undefined -> error(stdio_server_not_started);
        Server -> Server
    end.

-spec message_loop(state()) -> ok.
message_loop(State) ->
    case io:get_line("") of
        eof ->
            ok;
        Line when is_list(Line) ->
            CleanLine = string:trim(Line),
            case CleanLine of
                "" -> 
                    message_loop(State);
                _ ->
                    NewState = handle_message(CleanLine, State),
                    message_loop(NewState)
            end;
        {error, _} ->
            ok
    end.

-spec handle_message(string(), state()) -> state().
handle_message(Message, State) ->
    try jsx:decode(list_to_binary(Message), [return_maps]) of
        #{<<"method">> := <<"initialize">>, <<"id">> := Id} ->
            send_initialize_response(Id),
            State#state{initialized = true};
        #{<<"method">> := <<"notifications/initialized">>} ->
            % No response needed for notifications
            State;
        #{<<"method">> := <<"tools/list">>, <<"id">> := Id} ->
            send_tools_list(Id, State),
            State;
        #{<<"method">> := <<"resources/list">>, <<"id">> := Id} ->
            send_resources_list(Id, State),
            State;
        #{<<"method">> := <<"prompts/list">>, <<"id">> := Id} ->
            send_prompts_list(Id, State),
            State;
        #{<<"method">> := <<"tools/call">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_tool_call(Id, Params, State),
            State;
        #{<<"method">> := <<"resources/read">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_resource_read(Id, Params, State),
            State;
        #{<<"method">> := <<"prompts/get">>, <<"id">> := Id, <<"params">> := Params} ->
            handle_prompt_get(Id, Params, State),
            State;
        Other ->
            io:format(standard_error, "Unknown message: ~p~n", [Other]),
            State
    catch
        Error:Reason ->
            io:format(standard_error, "JSON parse error: ~p:~p~n", [Error, Reason]),
            State
    end.

-spec send_response(map()) -> ok.
send_response(Response) ->
    Json = jsx:encode(Response),
    io:format("~s~n", [Json]).

-spec send_initialize_response(term()) -> ok.
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
                <<"name">> => <<"erlmcp-stdio">>,
                <<"version">> => <<"1.0.0">>
            }
        }
    },
    send_response(Response).

-spec send_tools_list(term(), state()) -> ok.
send_tools_list(Id, State) ->
    Tools = maps:fold(fun(Name, {Description, _Handler, Schema}, Acc) ->
        Tool = #{
            <<"name">> => Name,
            <<"description">> => Description
        },
        ToolWithSchema = case Schema of
            undefined -> Tool;
            _ -> Tool#{<<"inputSchema">> => Schema}
        end,
        [ToolWithSchema | Acc]
    end, [], State#state.tools),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"tools">> => Tools
        }
    },
    send_response(Response).

-spec send_resources_list(term(), state()) -> ok.
send_resources_list(Id, State) ->
    Resources = maps:fold(fun(Uri, {Description, _Handler}, Acc) ->
        Resource = #{
            <<"uri">> => Uri,
            <<"name">> => Description,
            <<"mimeType">> => <<"text/plain">>
        },
        [Resource | Acc]
    end, [], State#state.resources),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"resources">> => Resources
        }
    },
    send_response(Response).

-spec send_prompts_list(term(), state()) -> ok.
send_prompts_list(Id, State) ->
    Prompts = maps:fold(fun(Name, {Description, _Handler, Arguments}, Acc) ->
        Prompt = #{
            <<"name">> => Name,
            <<"description">> => Description
        },
        PromptWithArgs = case Arguments of
            undefined -> Prompt;
            _ -> Prompt#{<<"arguments">> => Arguments}
        end,
        [PromptWithArgs | Acc]
    end, [], State#state.prompts),
    
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{
            <<"prompts">> => Prompts
        }
    },
    send_response(Response).

-spec handle_tool_call(term(), map(), state()) -> ok.
handle_tool_call(Id, #{<<"name">> := Name, <<"arguments">> := Arguments}, State) ->
    case maps:get(Name, State#state.tools, undefined) of
        undefined ->
            send_error(Id, -32601, <<"Tool not found">>);
        {_Description, Handler, _Schema} ->
            try
                Result = Handler(Arguments),
                ResultBinary = to_binary(Result),
                send_tool_response(Id, ResultBinary)
            catch
                Class:Reason:Stack ->
                    io:format(standard_error, "Tool handler crashed: ~p:~p~n~p~n", [Class, Reason, Stack]),
                    send_error(Id, -32603, <<"Internal error">>)
            end
    end;
handle_tool_call(Id, _Params, _State) ->
    send_error(Id, -32602, <<"Invalid parameters">>).

-spec handle_resource_read(term(), map(), state()) -> ok.
handle_resource_read(Id, #{<<"uri">> := Uri}, State) ->
    case maps:get(Uri, State#state.resources, undefined) of
        undefined ->
            send_error(Id, -32602, <<"Resource not found">>);
        {_Description, Handler} ->
            try
                Content = Handler(Uri),
                ContentBinary = to_binary(Content),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"contents">> => [#{
                            <<"uri">> => Uri,
                            <<"mimeType">> => <<"text/plain">>,
                            <<"text">> => ContentBinary
                        }]
                    }
                },
                send_response(Response)
            catch
                Class:Reason:Stack ->
                    io:format(standard_error, "Resource handler crashed: ~p:~p~n~p~n", [Class, Reason, Stack]),
                    send_error(Id, -32603, <<"Internal error">>)
            end
    end;
handle_resource_read(Id, _Params, _State) ->
    send_error(Id, -32602, <<"Invalid parameters">>).

-spec handle_prompt_get(term(), map(), state()) -> ok.
handle_prompt_get(Id, #{<<"name">> := Name} = Params, State) ->
    case maps:get(Name, State#state.prompts, undefined) of
        undefined ->
            send_error(Id, -32602, <<"Prompt not found">>);
        {_Description, Handler, _Arguments} ->
            try
                Arguments = maps:get(<<"arguments">>, Params, #{}),
                Result = Handler(Arguments),
                Messages = normalize_prompt_result(Result),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"messages">> => Messages
                    }
                },
                send_response(Response)
            catch
                Class:Reason:Stack ->
                    io:format(standard_error, "Prompt handler crashed: ~p:~p~n~p~n", [Class, Reason, Stack]),
                    send_error(Id, -32603, <<"Internal error">>)
            end
    end;
handle_prompt_get(Id, _Params, _State) ->
    send_error(Id, -32602, <<"Invalid parameters">>).

-spec send_tool_response(term(), binary()) -> ok.
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

-spec send_error(term(), integer(), binary()) -> ok.
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

-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> iolist_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F) -> float_to_binary(F, [{decimals, 2}]);
to_binary(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).

-spec normalize_prompt_result(term()) -> [map()].
normalize_prompt_result(Result) when is_binary(Result) ->
    [#{
        <<"role">> => <<"user">>,
        <<"content">> => #{
            <<"type">> => <<"text">>,
            <<"text">> => Result
        }
    }];
normalize_prompt_result(Result) when is_list(Result) ->
    case Result of
        [#{} | _] -> Result;  % Already a list of message maps
        _ -> normalize_prompt_result(iolist_to_binary(Result))
    end;
normalize_prompt_result(Result) ->
    normalize_prompt_result(to_binary(Result)).