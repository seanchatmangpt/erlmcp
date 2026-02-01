-module(erlmcp_otp_builder).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    build_from_source/2,
    abort_build/0,
    get_build_status/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% erlmcp_otp_builder - Build OTP from source
%%
%% Role:
%% - Orchestrates multi-phase build (configure → make → install → test)
%% - Spawns ports for shell commands (./configure, make)
%% - Captures build output for debugging
%% - Reports progress to caller
%%
%% Build Phases:
%% 1. Configure - ./configure --prefix=... (~30s)
%% 2. Make - make -jN (~5-10 min)
%% 3. Install - make install (~30s)
%% 4. Test - smoke test (~2s)
%%====================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec build_from_source(binary(), file:filename()) -> {ok, reference()} | {error, term()}.
build_from_source(_Version, _TarballPath) ->
    gen_server:call(?SERVER, {build_from_source, _Version, _TarballPath}).

-spec abort_build() -> ok.
abort_build() ->
    gen_server:call(?SERVER, abort_build).

-spec get_build_status() -> {ok, #{phase => atom(), progress => binary()}} | {error, no_build}.
get_build_status() ->
    gen_server:call(?SERVER, get_build_status).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #{}, {continue, init_state}}.

handle_continue(init_state, State) ->
    %% TODO: Cleanup partial builds
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
