-module(calculator_client).
-behaviour(gen_server).

-include("erlmcp.hrl").

%% API
-export([
    start_link/0,
    start_link/1,
    connect/1,
    add/3,
    subtract/3,
    multiply/3,
    divide/3,
    power/3,
    sqrt/2,
    factorial/2,
    calculate/2,
    get_help/1,
    get_history/1,
    generate_problem/1,
    generate_problem/3,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).

%% State record
-record(state, {
    client :: pid() | undefined,
    connected = false :: boolean(),
    server_info :: map() | undefined,
    capabilities :: #mcp_server_capabilities{} | undefined
}).

-type state() :: #state{}.
-export_type([state/0]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

connect(Client) ->
    gen_server:call(Client, connect).

add(Client, A, B) ->
    call_tool(Client, <<"add">>, #{<<"a">> => A, <<"b">> => B}).

subtract(Client, A, B) ->
    call_tool(Client, <<"subtract">>, #{<<"a">> => A, <<"b">> => B}).

multiply(Client, A, B) ->
    call_tool(Client, <<"multiply">>, #{<<"a">> => A, <<"b">> => B}).

divide(Client, A, B) ->
    call_tool(Client, <<"divide">>, #{<<"a">> => A, <<"b">> => B}).

power(Client, A, B) ->
    call_tool(Client, <<"power">>, #{<<"a">> => A, <<"b">> => B}).

sqrt(Client, A) ->
    call_tool(Client, <<"sqrt">>, #{<<"a">> => A}).

factorial(Client, N) ->
    call_tool(Client, <<"factorial">>, #{<<"n">> => N}).

calculate(Client, Expression) ->
    call_tool(Client, <<"calculate">>, #{<<"expression">> => Expression}).

get_help(Client) ->
    read_resource(Client, <<"calculator://help">>).

get_history(Client) ->
    read_resource(Client, <<"calculator://history">>).

generate_problem(Client) ->
    generate_problem(Client, <<"medium">>, <<"mixed">>).

generate_problem(Client, Difficulty, Type) ->
    get_prompt(Client, <<"math_problem">>, #{
        <<"difficulty">> => Difficulty,
        <<"type">> => Type
    }).

stop(Client) ->
    gen_server:stop(Client).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(_Options) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

handle_call(connect, _From, State) ->
    case start_mcp_client() of
        {ok, Client} ->
            case initialize_client(Client) of
                {ok, InitResult} ->
                    ServerInfo = maps:get(<<"serverInfo">>, InitResult, #{}),
                    Capabilities = extract_capabilities(InitResult),
                    
                    NewState = State#state{
                        client = Client,
                        connected = true,
                        server_info = ServerInfo,
                        capabilities = Capabilities
                    },
                    
                    {reply, {ok, ServerInfo}, NewState};
                {error, Reason} ->
                    erlmcp_client:stop(Client),
                    {reply, {error, Reason}, State}
            end;
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({call_tool, Name, Args}, _From, #state{client = Client, connected = true} = State) ->
    Result = erlmcp_client:call_tool(Client, Name, Args),
    {reply, Result, State};

handle_call({read_resource, Uri}, _From, #state{client = Client, connected = true} = State) ->
    Result = erlmcp_client:read_resource(Client, Uri),
    {reply, Result, State};

handle_call({get_prompt, Name, Args}, _From, #state{client = Client, connected = true} = State) ->
    Result = erlmcp_client:get_prompt(Client, Name, Args),
    {reply, Result, State};

handle_call(_Request, _From, #state{connected = false} = State) ->
    {reply, {error, not_connected}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, Reason}, #state{client = Pid} = State) ->
    logger:error("MCP client died: ~p", [Reason]),
    {noreply, State#state{client = undefined, connected = false}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{client = Client}) when is_pid(Client) ->
    erlmcp_client:stop(Client),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_mcp_client() ->
    TransportOpts = {stdio, []},
    ClientOpts = #{
        strict_mode => false,
        timeout => ?TIMEOUT
    },
    erlmcp_client:start_link(TransportOpts, ClientOpts).

initialize_client(Client) ->
    Capabilities = #mcp_client_capabilities{
        roots = #mcp_capability{enabled = true},
        sampling = #mcp_capability{enabled = false}
    },
    erlmcp_client:initialize(Client, Capabilities).

extract_capabilities(InitResult) ->
    case maps:get(<<"capabilities">>, InitResult, #{}) of
        Caps when is_map(Caps) ->
            #mcp_server_capabilities{
                tools = case maps:is_key(<<"tools">>, Caps) of
                    true -> #mcp_capability{enabled = true};
                    false -> undefined
                end,
                resources = case maps:is_key(<<"resources">>, Caps) of
                    true -> #mcp_capability{enabled = true};
                    false -> undefined
                end,
                prompts = case maps:is_key(<<"prompts">>, Caps) of
                    true -> #mcp_capability{enabled = true};
                    false -> undefined
                end
            };
        _ ->
            undefined
    end.

call_tool(Client, Name, Args) ->
    case gen_server:call(Client, {call_tool, Name, Args}, ?TIMEOUT) of
        {ok, #{<<"content">> := [#{<<"text">> := Result} | _]}} ->
            {ok, Result};
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

read_resource(Client, Uri) ->
    case gen_server:call(Client, {read_resource, Uri}, ?TIMEOUT) of
        {ok, #{<<"contents">> := [#{<<"text">> := Content} | _]}} ->
            {ok, Content};
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

get_prompt(Client, Name, Args) ->
    case gen_server:call(Client, {get_prompt, Name, Args}, ?TIMEOUT) of
        {ok, #{<<"messages">> := Messages}} ->
            {ok, Messages};
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.