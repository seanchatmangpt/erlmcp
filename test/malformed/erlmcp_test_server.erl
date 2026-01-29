%%%-------------------------------------------------------------------
%%% @doc Test Server for Malformed Injection Tests
%%% @purpose Simple TCP server that accepts malformed data
%%%-------------------------------------------------------------------
-module(erlmcp_test_server).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {
    port,
    listen_socket,
    connections = []
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port, Opts) ->
    gen_server:start_link(?MODULE, [Port, Opts], []).

stop(Pid) ->
    gen_server:stop(Pid).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Port, _Opts]) ->
    process_flag(trap_exit, true),
    
    case gen_tcp:listen(Port, [
        binary,
        {active, false},
        {packet, 0},
        {reuseaddr, true},
        {recbuf, 1024*1024},
        {sndbuf, 1024*1024}
    ]) of
        {ok, ListenSocket} ->
            io:format("Test server listening on port ~p~n", [Port]),
            self() ! {accept, ListenSocket},
            {ok, #state{port = Port, listen_socket = ListenSocket}};
        {error, Reason} ->
            io:format("Failed to listen on port ~p: ~p~n", [Port, Reason]),
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({accept, ListenSocket}, State) ->
    case gen_tcp:accept(ListenSocket, 1000) of
        {ok, Socket} ->
            {ok, {Ip, Port}} = inet:peername(Socket),
            io:format("Connection from ~p:~p~n", [Ip, Port]),
            
            % Spawn handler
            Pid = spawn_link(fun() -> handle_connection(Socket) end),
            ok = gen_tcp:controlling_process(Socket, Pid),
            
            self() ! {accept, ListenSocket},
            {noreply, State};
        {error, timeout} ->
            self() ! {accept, ListenSocket},
            {noreply, State};
        {error, Reason} ->
            io:format("Accept error: ~p~n", [Reason]),
            self() ! {accept, ListenSocket},
            {noreply, State}
    end;

handle_info({'EXIT', Pid, Reason}, State) ->
    io:format("Handler ~p exited: ~p~n", [Pid, Reason]),
    {noreply, State};

handle_info(Info, State) ->
    io:format("Unexpected info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, #state{listen_socket = ListenSocket}) ->
    gen_tcp:close(ListenSocket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Connection Handler
%%%===================================================================

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0, 5000) of
        {ok, Data} ->
            io:format("Received ~p bytes~n", [byte_size(Data)]),
            
            % Try to parse as JSON
            Response = try
                case jsx:is_json(Data) of
                    true ->
                        io:format("Valid JSON received~n"),
                        {ok, <<"{'status':'ok'}">>};
                    false ->
                        io:format("Invalid JSON received~n"),
                        {ok, <<"{'error':'invalid_json'}">>}
                end
            catch
                Type:Error:Stacktrace ->
                    io:format("Parse error: ~p:~p~n", [Type, Error]),
                    io:format("Stack: ~p~n", [Stacktrace]),
                    {ok, <<"{'error':'parse_error'}">>}
            end,
            
            case Response of
                {ok, ResponseData} ->
                    gen_tcp:send(Socket, ResponseData);
                _ ->
                    ok
            end,
            
            gen_tcp:close(Socket);
        {error, timeout} ->
            io:format("Receive timeout~n"),
            gen_tcp:close(Socket);
        {error, Reason} ->
            io:format("Receive error: ~p~n", [Reason]),
            gen_tcp:close(Socket)
    end.
