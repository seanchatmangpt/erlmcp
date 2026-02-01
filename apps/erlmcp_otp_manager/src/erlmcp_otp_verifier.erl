-module(erlmcp_otp_verifier).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    verify_installation/1,
    verify_binaries/1,
    verify_version/1,
    verify_libraries/1,
    verify_installation_async/1
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% erlmcp_otp_verifier - Verify OTP installations
%%
%% Role:
%% - Verifies OTP installation integrity
%% - Checks binaries, version, libraries, smoke test
%% - Caches verification results (ETS, 60s TTL)
%%
%% Verification Steps:
%% 1. Binaries - erl, erlc, escript exist and executable
%% 2. Version - erl -eval 'erlang:system_info(otp_release)' matches required
%% 3. Libraries - kernel, stdlib, etc. present in lib/erlang/lib/
%% 4. Smoke Test - erl -eval '1+1' returns 2
%%====================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec verify_installation(file:filename()) -> {ok, binary()} | {error, term()}.
verify_installation(_InstallPath) ->
    gen_server:call(?SERVER, {verify_installation, _InstallPath}).

-spec verify_binaries(file:filename()) -> ok | {error, {missing_binaries, [binary()]}}.
verify_binaries(_InstallPath) ->
    gen_server:call(?SERVER, {verify_binaries, _InstallPath}).

-spec verify_version(file:filename()) -> {ok, binary()} | {error, {version_mismatch, binary(), binary()}}.
verify_version(_InstallPath) ->
    gen_server:call(?SERVER, {verify_version, _InstallPath}).

-spec verify_libraries(file:filename()) -> ok | {error, {missing_libs, [binary()]}}.
verify_libraries(_InstallPath) ->
    gen_server:call(?SERVER, {verify_libraries, _InstallPath}).

-spec verify_installation_async(file:filename()) -> {ok, reference()}.
verify_installation_async(_InstallPath) ->
    gen_server:call(?SERVER, {verify_installation_async, _InstallPath}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
