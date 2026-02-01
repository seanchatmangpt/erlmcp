-module(erlmcp_otp_fetcher).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    download_package/2,
    download_source_tarball/2,
    abort_download/0,
    get_download_progress/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% erlmcp_otp_fetcher - Download OTP artifacts
%%
%% Role:
%% - Downloads source tarballs from GitHub or Erlang Solutions
%% - Verifies SHA256 checksums
%% - Retries on network failure (exponential backoff)
%% - Reports progress to caller
%%
%% State Machine:
%%   idle → downloading → verifying → idle
%%          ↓                       ↓
%%        failed ←─ retry ──────────┘
%%
%% Messages to Caller:
%% - {download_complete, Ref, Path}
%% - {download_failed, Ref, Reason}
%% - {download_progress, Ref, BytesDownloaded, TotalBytes}
%%====================================================================

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec download_package(binary(), binary()) -> {ok, reference()} | {error, term()}.
download_package(_Version, _Url) ->
    gen_server:call(?SERVER, {download_package, _Version, _Url}).

-spec download_source_tarball(binary(), binary()) -> {ok, reference()} | {error, term()}.
download_source_tarball(_Version, _Url) ->
    gen_server:call(?SERVER, {download_source_tarball, _Version, _Url}).

-spec abort_download() -> ok.
abort_download() ->
    gen_server:call(?SERVER, abort_download).

-spec get_download_progress() -> {ok, #{bytes => integer(), total => integer()}} | {error, no_download}.
get_download_progress() ->
    gen_server:call(?SERVER, get_download_progress).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    {ok, #{}, {continue, init_state}}.

handle_continue(init_state, State) ->
    %% TODO: Cleanup partial downloads
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
