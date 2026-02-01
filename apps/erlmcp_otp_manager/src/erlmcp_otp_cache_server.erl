-module(erlmcp_otp_cache_server).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([
    get_current_version/0,
    get_installation_path/0,
    get_installation_status/0,
    get_metadata/1,
    set_installation_status/1,
    update_metadata/2,
    clear_cache/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% erlmcp_otp_cache_server - Central metadata store and state coordinator
%%
%% Role:
%% - Maintains current OTP installation state (not_installed | installing | installed | failed)
%% - Stores version metadata (SHA256, download URL, build flags)
%% - Persists state to DETS for recovery after supervisor restart
%% - Coordinates state between fetcher, builder, verifier
%%
%% State Machine:
%%   not_installed → installing → installed
%%                             ↘ failed
%%
%% Crash Recovery:
%% - Rebuilds state from ~/.erlmcp/cache/otp-manager.state (DETS)
%% - If DETS missing → assumes not_installed
%% - Recovery time: ~500ms
%%====================================================================

-define(SERVER, ?MODULE).
-define(STATE_FILE, "otp-manager.state").

-type installation_status() :: not_installed | installing | installed | failed.

-record(state, {
    cache_dir :: file:filename(),
    required_version :: binary(),
    current_version :: binary() | undefined,
    installation_path :: file:filename() | undefined,
    installation_status :: installation_status(),
    metadata :: #{binary() => map()},
    dets_ref :: dets:tab_name() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Query operations (fast, read from state)

-spec get_current_version() -> {ok, binary()} | {error, not_installed}.
get_current_version() ->
    gen_server:call(?SERVER, get_current_version).

-spec get_installation_path() -> {ok, file:filename()} | {error, not_installed}.
get_installation_path() ->
    gen_server:call(?SERVER, get_installation_path).

-spec get_installation_status() -> installation_status().
get_installation_status() ->
    gen_server:call(?SERVER, get_installation_status).

-spec get_metadata(binary()) -> {ok, map()} | {error, not_found}.
get_metadata(Version) ->
    gen_server:call(?SERVER, {get_metadata, Version}).

%% State mutations

-spec set_installation_status(installation_status()) -> ok.
set_installation_status(Status) ->
    gen_server:call(?SERVER, {set_installation_status, Status}).

-spec update_metadata(binary(), map()) -> ok.
update_metadata(Version, Metadata) ->
    gen_server:call(?SERVER, {update_metadata, Version, Metadata}).

-spec clear_cache() -> ok.
clear_cache() ->
    gen_server:call(?SERVER, clear_cache).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init([]) ->
    %% NOTE: No blocking operations in init/1 (Armstrong principle)
    %% Async load state from DETS in handle_continue
    {ok, #state{}, {continue, init_state}}.

handle_continue(init_state, _State) ->
    %% TODO: Load persisted state from DETS
    {noreply, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
