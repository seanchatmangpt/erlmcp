-module(erlmcp_stdio_protocol).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    server_pid :: pid(),
    initialized = false :: boolean(),
    reader_pid :: pid() | undefined
}).

-type state() :: #state{}.

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Options], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

-spec init([map()]) -> {ok, state()}.
init([_Options]) ->
    process_flag(trap_exit, true),
    
    % Find the server process
    ServerPid = whereis(erlmcp_stdio_server),
    
    % Start the stdin reader process
    ReaderPid = spawn_link(fun() -> stdin_reader_loop(self()) end),
    
    {ok, #state{
        server_pid = ServerPid,
        reader_pid = ReaderPid
    }}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, term(), state()}.
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({stdin_line, Line}, State) ->
    NewState = handle_json_rpc_message(Line, State),
    {noreply, NewState};

handle_info({'EXIT', Pid, Reason}, #state{reader_pid = Pid} = State) ->
    case Reason of
        normal ->
            logger:info("Stdin reader finished normally"),
            {stop, normal, State};
        _ ->
            logger:error("Stdin reader died: ~p", [Reason]),
            {stop, {reader_died, Reason}, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, #state{reader_pid = ReaderPid}) when is_pid(ReaderPid) ->
    exit(ReaderPid, shutdown),
    ok;
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

-spec stdin_reader_loop(pid()) -> no_return().
stdin_reader_loop(Parent) ->
    case io:get_line("") of
        eof ->
            logger:info("EOF received from stdin, stopping reader"),
            exit(normal);
        {error, Reason} ->
            logger:error("Stdin read error: ~p", [Reason]),
            exit({read_error, Reason});
        Line when is_list(Line) ->
            CleanLine = string:trim(Line),
            case CleanLine of
                "" -> 
                    stdin_reader_loop(Parent);
                _ ->
                    Parent ! {stdin_line, CleanLine},
                    stdin_reader_loop(Parent)
            end
    end.

-spec handle_json_rpc_message(string(), state()) -> state().
handle_json_rpc_message(Message, State) ->
    try jsx:decode(list_to_binary(Message), [return_maps]) of
        #{<<"method">> := <<"initialize">>, <<"id">> := Id} = Request ->
            handle_initialize(Id, Request, State);
        #{<<"method">> := <<"notifications/initialized">>} ->
            % No response needed for notifications
            State;
        #{<<"method">> := Method, <<"id">> := Id} = Request ->
            handle_method_call(Method, Id, Request, State);
        Other ->
            logger:warning("Unknown message: ~p", [Other]),
            State
    catch
        Error:Reason ->
            logger:error("JSON parse error: ~p:~p", [Error, Reason]),
            State
    end.

-spec handle_initialize(term(), map(), state()) -> state().
handle_initialize(Id, _Request, State) ->
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
    send_response(Response),
    State#state{initialized = true}.

-spec handle_method_call(binary(), term(), map(), state()) -> state().
handle_method_call(Method, Id, Request, #state{server_pid = ServerPid} = State) ->
    case ServerPid of
        undefined ->
            send_error(Id, -32603, <<"Server not available">>);
        Pid ->
            % Forward to the server process
            case gen_server:call(Pid, {handle_method, Method, Id, Request}, 5000) of
                {response, Response} ->
                    send_response(Response);
                {error, Error} ->
                    send_response(Error);
                timeout ->
                    send_error(Id, -32603, <<"Server timeout">>)
            end
    end,
    State.

-spec send_response(map()) -> ok.
send_response(Response) ->
    Json = jsx:encode(Response),
    io:format("~s~n", [Json]).

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