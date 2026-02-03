%%%-------------------------------------------------------------------
%%% @doc Rollback Manager for Module Versioning
%%%-------------------------------------------------------------------
-module(erlmcp_rollback_manager).

-behaviour(gen_server).

%% API
-export([start_link/0, save_version/2, rollback_last/1, rollback_to_version/2, get_version_history/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type module_name() :: module().
-type version() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
-type version_entry() :: #{
    version => version(),
    binary => binary(),
    timestamp => integer(),
    beam_path => string()
}.

%% State record
-record(state, {
    version_table :: ets:table(),
    metadata :: map()
}).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec save_version(module_name(), binary()) -> ok | {error, term()}.
save_version(Module, Binary) when is_atom(Module), is_binary(Binary) ->
    gen_server:call(?MODULE, {save_version, Module, Binary}).

-spec rollback_last(module_name()) -> ok | {error, term()}.
rollback_last(Module) when is_atom(Module) ->
    gen_server:call(?MODULE, {rollback_last, Module}).

-spec rollback_to_version(module_name(), version()) -> ok | {error, term()}.
rollback_to_version(Module, Version) when is_atom(Module), is_tuple(Version) ->
    gen_server:call(?MODULE, {rollback_to_version, Module, Version}).

-spec get_version_history(module_name()) -> {ok, [version_entry()]} | {error, term()}.
get_version_history(Module) when is_atom(Module) ->
    gen_server:call(?MODULE, {get_version_history, Module}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    VTable = ets:new(rollback_versions, [set, public]),
    State = #state{
        version_table = VTable,
        metadata = #{}
    },
    {ok, State}.

handle_call({save_version, Module, Binary}, _From, State) ->
    %% Simple stub that just acknowledges the save
    {reply, ok, State};

handle_call({rollback_last, Module}, _From, State) ->
    %% Simple stub that just returns ok
    logger:info("Rollback requested for ~p", [Module]),
    {reply, ok, State};

handle_call({rollback_to_version, Module, Version}, _From, State) ->
    %% Simple stub that just returns ok
    logger:info("Rollback to version requested for ~p: ~p", [Module, Version]),
    {reply, ok, State};

handle_call({get_version_history, Module}, _From, State) ->
    %% Return empty history
    {reply, {ok, []}, State};

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
