-module(erlmcp_transport_tcp).

-export([send/2, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {
    socket :: gen_tcp:socket(),
    owner :: pid()
}).

start_link({Host, Port, Owner}) ->
    gen_server:start_link(?MODULE, [Host, Port, Owner], []).

send(State, Message) when is_record(State, state) ->
    gen_tcp:send(State#state.socket, [Message, "\n"]);
send({Host, Port, _Owner}, Message) ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, line}]) of
        {ok, Socket} ->
            Result = gen_tcp:send(Socket, [Message, "\n"]),
            gen_tcp:close(Socket),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.

init([Host, Port, Owner]) ->
    case gen_tcp:connect(Host, Port, [binary, {active, true}, {packet, line}]) of
        {ok, Socket} ->
            {ok, #state{socket = Socket, owner = Owner}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    CleanData = binary:part(Data, 0, byte_size(Data) - 1),
    State#state.owner ! {transport_message, CleanData},
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket, Reason}, State) ->
    {stop, Reason, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
