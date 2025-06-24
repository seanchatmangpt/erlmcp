-module(erlmcp_transport_stdio).

-export([send/2, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-record(state, {
    owner :: pid()
}).

start_link(Owner) ->
    gen_server:start_link(?MODULE, [Owner], []).

send(_State, Message) ->
    io:format("~s~n", [Message]),
    ok.

init([Owner]) ->
    spawn_link(fun() -> read_loop(Owner) end),
    {ok, #state{owner = Owner}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

read_loop(Owner) ->
    case io:get_line("") of
        eof ->
            ok;
        Line ->
            CleanLine = string:trim(Line),
            Owner ! {transport_message, list_to_binary(CleanLine)},
            read_loop(Owner)
    end.
