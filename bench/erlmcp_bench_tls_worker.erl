%%%-------------------------------------------------------------------
%%% @doc
%%% TLS worker process for benchmarking
%%%
%%% This is a simple gen_server that maintains a TLS connection
%%% for throughput benchmarking.
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_bench_tls_worker).

-behaviour(gen_server).

%% API
-export([start_link/1, send/2, stop/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state,
        {socket :: ssl:sslsocket() | undefined,
         tls_opts :: [ssl:tls_client_option()]}).

%%====================================================================
%% API
%%====================================================================

-spec start_link([ssl:tls_client_option()]) -> {ok, pid()} | {error, term()}.
start_link(TLSOpts) ->
    gen_server:start_link(?MODULE, TLSOpts, []).

-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) ->
    gen_server:call(Pid, {send, Data}, 5000).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(TLSOpts) ->
    %% Connect to test server
    Host = erlmcp_bench_tls:TEST_HOST,
    Port = erlmcp_bench_tls:TEST_PORT,

    case ssl:connect(Host, Port, TLSOpts, 5000) of
        {ok, Socket} ->
            {ok, #state{socket = Socket, tls_opts = TLSOpts}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({send, Data}, _From, #state{socket = Socket} = State) ->
    Reply =
        case ssl:send(Socket, Data) of
            ok ->
                ssl:setopts(Socket, [{active, once}]),
                receive
                    {ssl, Socket, _Response} ->
                        ok
                after 1000 ->
                    {error, timeout}
                end;
            {error, Reason} ->
                {error, Reason}
        end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{socket = Socket}) ->
    case Socket of
        undefined ->
            ok;
        _ ->
            ssl:close(Socket)
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
