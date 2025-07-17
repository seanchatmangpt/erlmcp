-module(erlmcp_stdio_server).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API exports
-export([
    start_link/1,
    add_tool/3, add_tool/4,
    add_resource/3, add_resource/4,
    add_prompt/3, add_prompt/4
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type tool_handler() :: fun((map()) -> binary() | iolist()).
-type resource_handler() :: fun((binary()) -> binary() | iolist()).
-type prompt_handler() :: fun((map()) -> binary() | iolist() | [map()]).

-record(state, {
    tools = #{} :: #{binary() => {binary(), tool_handler(), map() | undefined}},
    resources = #{} :: #{binary() => {binary(), resource_handler()} | {binary(), resource_handler(), binary()}},
    prompts = #{} :: #{binary() => {binary(), prompt_handler(), [map()] | undefined}}
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

-spec add_tool(binary(), binary(), tool_handler()) -> ok.
add_tool(Name, Description, Handler) ->
    add_tool(Name, Description, Handler, undefined).

-spec add_tool(binary(), binary(), tool_handler(), map() | undefined) -> ok.
add_tool(Name, Description, Handler, Schema) when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    gen_server:call(?MODULE, {add_tool, Name, Description, Handler, Schema}).

-spec add_resource(binary(), binary(), resource_handler()) -> ok.
add_resource(Uri, Description, Handler) ->
    add_resource(Uri, Description, Handler, <<"text/plain">>).

-spec add_resource(binary(), binary(), resource_handler(), binary()) -> ok.
add_resource(Uri, Description, Handler, MimeType) when is_binary(Uri), is_binary(Description), is_function(Handler, 1) ->
    gen_server:call(?MODULE, {add_resource, Uri, Description, Handler, MimeType}).

-spec add_prompt(binary(), binary(), prompt_handler()) -> ok.
add_prompt(Name, Description, Handler) ->
    add_prompt(Name, Description, Handler, undefined).

-spec add_prompt(binary(), binary(), prompt_handler(), [map()] | undefined) -> ok.
add_prompt(Name, Description, Handler, Arguments) when is_binary(Name), is_binary(Description), is_function(Handler, 1) ->
    gen_server:call(?MODULE, {add_prompt, Name, Description, Handler, Arguments}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()} | {stop, term()}.
init([_Options]) ->
    process_flag(trap_exit, true),
    case catch application:ensure_all_started(jsx) of
        {'EXIT', Reason} ->
            logger:error("Failed to start jsx application: ~p", [Reason]),
            {stop, {jsx_start_error, Reason}};
        {error, Reason} ->
            logger:error("Failed to start jsx application: ~p", [Reason]),
            {stop, {jsx_start_error, Reason}};
        {ok, _Started} ->
            {ok, #state{}};
        ok ->
            {ok, #state{}}
    end.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call({add_tool, Name, Description, Handler, Schema}, _From, State) ->
    NewTools = maps:put(Name, {Description, Handler, Schema}, State#state.tools),
    {reply, ok, State#state{tools = NewTools}};

handle_call({add_resource, Uri, Description, Handler, MimeType}, _From, State) ->
    NewResources = maps:put(Uri, {Description, Handler, MimeType}, State#state.resources),
    {reply, ok, State#state{resources = NewResources}};

handle_call({add_prompt, Name, Description, Handler, Arguments}, _From, State) ->
    NewPrompts = maps:put(Name, {Description, Handler, Arguments}, State#state.prompts),
    {reply, ok, State#state{prompts = NewPrompts}};

handle_call({handle_method, Method, Id, Request}, _From, State) ->
    Response = handle_mcp_method(Method, Id, Request, State),
    {reply, Response, State};

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
%% Internal Functions - MCP Method Handlers
%%====================================================================

-spec handle_mcp_method(binary(), term(), map(), state()) -> {response, map()} | {error, map()}.
handle_mcp_method(<<"tools/list">>, Id, _Request, State) ->
    Tools = build_tools_list(State),
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{<<"tools">> => Tools}
    },
    {response, Response};

handle_mcp_method(<<"resources/list">>, Id, _Request, State) ->
    Resources = build_resources_list(State),
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{<<"resources">> => Resources}
    },
    {response, Response};

handle_mcp_method(<<"prompts/list">>, Id, _Request, State) ->
    Prompts = build_prompts_list(State),
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"result">> => #{<<"prompts">> => Prompts}
    },
    {response, Response};

handle_mcp_method(<<"tools/call">>, Id, #{<<"params">> := Params}, State) ->
    handle_tool_call(Id, Params, State);

handle_mcp_method(<<"resources/read">>, Id, #{<<"params">> := Params}, State) ->
    handle_resource_read(Id, Params, State);

handle_mcp_method(<<"prompts/get">>, Id, #{<<"params">> := Params}, State) ->
    handle_prompt_get(Id, Params, State);

handle_mcp_method(Method, Id, _Request, _State) ->
    Error = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32601,
            <<"message">> => <<"Method not found: ", Method/binary>>
        }
    },
    {error, Error}.

%% ... (rest of the internal functions remain the same as before)
%% build_tools_list/1, build_resources_list/1, build_prompts_list/1,
%% handle_tool_call/3, handle_resource_read/3, handle_prompt_get/3, etc.

-spec build_tools_list(state()) -> [map()].
build_tools_list(State) ->
    maps:fold(fun(Name, {Description, _Handler, Schema}, Acc) ->
        Tool = #{
            <<"name">> => Name,
            <<"description">> => Description
        },
        ToolWithSchema = case Schema of
            undefined -> Tool;
            _ -> Tool#{<<"inputSchema">> => Schema}
        end,
        [ToolWithSchema | Acc]
    end, [], State#state.tools).

-spec build_resources_list(state()) -> [map()].
build_resources_list(State) ->
    maps:fold(fun
        (Uri, {Description, _Handler}, Acc) ->
            Resource = #{
                <<"uri">> => Uri,
                <<"name">> => Description,
                <<"mimeType">> => <<"text/plain">>
            },
            [Resource | Acc];
        (Uri, {Description, _Handler, MimeType}, Acc) ->
            Resource = #{
                <<"uri">> => Uri,
                <<"name">> => Description,
                <<"mimeType">> => MimeType
            },
            [Resource | Acc]
    end, [], State#state.resources).

-spec build_prompts_list(state()) -> [map()].
build_prompts_list(State) ->
    maps:fold(fun(Name, {Description, _Handler, Arguments}, Acc) ->
        Prompt = #{
            <<"name">> => Name,
            <<"description">> => Description
        },
        PromptWithArgs = case Arguments of
            undefined -> Prompt;
            _ -> Prompt#{<<"arguments">> => Arguments}
        end,
        [PromptWithArgs | Acc]
    end, [], State#state.prompts).

%% Tool/Resource/Prompt handlers would go here...
%% (Similar to the existing implementation but returning {response, Response} or {error, Error})

-spec handle_tool_call(term(), map(), state()) -> {response, map()} | {error, map()}.
handle_tool_call(Id, #{<<"name">> := Name, <<"arguments">> := Arguments}, State) ->
    case maps:get(Name, State#state.tools, undefined) of
        undefined ->
            Error = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{
                    <<"code">> => -32601,
                    <<"message">> => <<"Tool not found">>
                }
            },
            {error, Error};
        {_Description, Handler, _Schema} ->
            try
                Result = Handler(Arguments),
                ResultBinary = to_binary(Result),
                Response = #{
                    <<"jsonrpc">> => <<"2.0">>,
                    <<"id">> => Id,
                    <<"result">> => #{
                        <<"content">> => [#{
                            <<"type">> => <<"text">>,
                            <<"text">> => ResultBinary
                        }]
                    }
                },
                {response, Response}
            catch
                Class:Reason:Stack ->
                    logger:error("Tool handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                    Error = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => Id,
                        <<"error">> => #{
                            <<"code">> => -32603,
                            <<"message">> => <<"Internal error">>
                        }
                    },
                    {error, Error}
            end
    end;
handle_tool_call(Id, _Params, _State) ->
    Error = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid parameters">>
        }
    },
    {error, Error}.

-spec handle_resource_read(term(), map(), state()) -> {response, map()} | {error, map()}.
handle_resource_read(Id, #{<<"uri">> := Uri}, State) ->
    case maps:get(Uri, State#state.resources, undefined) of
        undefined ->
            Error = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{
                    <<"code">> => -32602,
                    <<"message">> => <<"Resource not found">>
                }
            },
            {error, Error};
        {_Description, Handler} ->
            handle_resource_with_handler(Id, Uri, Handler, <<"text/plain">>);
        {_Description, Handler, MimeType} ->
            handle_resource_with_handler(Id, Uri, Handler, MimeType)
    end;
handle_resource_read(Id, _Params, _State) ->
    Error = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid parameters">>
        }
    },
    {error, Error}.

-spec handle_resource_with_handler(term(), binary(), fun(), binary()) -> {response, map()} | {error, map()}.
handle_resource_with_handler(Id, Uri, Handler, MimeType) ->
    try
        Content = Handler(Uri),
        ContentBinary = to_binary(Content),
        Response = #{
            <<"jsonrpc">> => <<"2.0">>,
            <<"id">> => Id,
            <<"result">> => #{
                <<"contents">> => [#{
                    <<"uri">> => Uri,
                    <<"mimeType">> => MimeType,
                    <<"text">> => ContentBinary
                }]
            }
        },
        {response, Response}
    catch
        Class:Reason:Stack ->
            logger:error("Resource handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
            Error = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{
                    <<"code">> => -32603,
                    <<"message">> => <<"Internal error">>
                }
            },
            {error, Error}
    end.

-spec handle_prompt_get(term(), map(), state()) -> {response, map()} | {error, map()}.
handle_prompt_get(Id, #{<<"name">> := Name} = Params, State) ->
    case maps:get(Name, State#state.prompts, undefined) of
        undefined ->
            Error = #{
                <<"jsonrpc">> => <<"2.0">>,
                <<"id">> => Id,
                <<"error">> => #{
                    <<"code">> => -32602,
                    <<"message">> => <<"Prompt not found">>
                }
            },
            {error, Error};
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
                {response, Response}
            catch
                Class:Reason:Stack ->
                    logger:error("Prompt handler crashed: ~p:~p~n~p", [Class, Reason, Stack]),
                    Error = #{
                        <<"jsonrpc">> => <<"2.0">>,
                        <<"id">> => Id,
                        <<"error">> => #{
                            <<"code">> => -32603,
                            <<"message">> => <<"Internal error">>
                        }
                    },
                    {error, Error}
            end
    end;
handle_prompt_get(Id, _Params, _State) ->
    Error = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32602,
            <<"message">> => <<"Invalid parameters">>
        }
    },
    {error, Error}.

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