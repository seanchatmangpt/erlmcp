%%%-------------------------------------------------------------------
%%% @doc Module Reload Coordinator
%%%-------------------------------------------------------------------
-module(erlmcp_reload_coordinator).

-behaviour(gen_server).

%% API
-export([start_link/0, reload_module/1, get_reload_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    modules = #{} :: map(),
    versions = #{} :: map()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec reload_module(module()) -> ok | {error, term()}.
reload_module(Module) when is_atom(Module) ->
    gen_server:call(?MODULE, {reload_module, Module}).

-spec get_reload_status(module()) -> {ok, map()} | {error, term()}.
get_reload_status(Module) when is_atom(Module) ->
    gen_server:call(?MODULE, {get_reload_status, Module}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    State = #state{
        modules = #{},
        versions = #{}
    },
    {ok, State}.

handle_call({reload_module, Module}, _From, State) ->
    logger:info("Reloading module ~p", [Module]),
    {reply, ok, State};

handle_call({get_reload_status, Module}, _From, State) ->
    Status = #{module => Module, status => ok},
    {reply, {ok, Status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
