%%%-------------------------------------------------------------------
%%% @doc PQChain Peer Supervisor
%%%
%%% Simple_one_for_one supervisor for dynamic peer channel processes.
%%% Each peer channel is a gen_server managing ML-KEM encrypted communication.
%%%
%%% Architecture:
%%% - Uses simple_one_for_one for dynamic peer connection spawning
%%% - Each child is a pqc_peer_channel gen_server process
%%% - Channels are transient - restart only on abnormal termination
%%% - Each channel maintains ML-KEM shared secret and session keys
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_peer_sup).

-behaviour(supervisor).

-include("pqchain.hrl").

%% API
-export([start_link/0,
         start_channel/2,
         start_channel/3,
         stop_channel/1,
         list_channels/0,
         count_channels/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds
-define(SHUTDOWN_TIMEOUT, 5000).  % 5 seconds for channel cleanup

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the peer supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new peer channel
%%
%% Args:
%%   LocalPeer: Local peer identity
%%   RemotePeer: Remote peer identity
%%
-spec start_channel(#peer_identity{}, #peer_identity{}) ->
    {ok, pid()} | {error, term()}.
start_channel(LocalPeer, RemotePeer) ->
    start_channel(LocalPeer, RemotePeer, #{}).

%% @doc Start a new peer channel with options
%%
%% Options can include:
%%   - kem_algorithm: KEM algorithm to use (default: ml_kem_768)
%%   - rekey_interval_ms: Rekey interval (default: 3600000 = 1 hour)
%%   - transport: tcp | quic | websocket
%%   - connection_timeout_ms: Connection timeout
%%   - keepalive_interval_ms: Keepalive ping interval
%%
-spec start_channel(#peer_identity{}, #peer_identity{}, map()) ->
    {ok, pid()} | {error, term()}.
start_channel(LocalPeer, RemotePeer, Options)
  when is_record(LocalPeer, peer_identity),
       is_record(RemotePeer, peer_identity),
       is_map(Options) ->
    supervisor:start_child(?SERVER, [LocalPeer, RemotePeer, Options]).

%% @doc Stop a peer channel gracefully
%%
%% Sends close message to the channel process and waits for clean shutdown.
%% Returns ok if channel was stopped, {error, not_found} if channel doesn't exist.
%%
-spec stop_channel(pid() | binary()) -> ok | {error, not_found | term()}.
stop_channel(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            try
                %% Try graceful shutdown
                ok = pqc_peer_channel:close(Pid),
                %% Wait for process to terminate
                MRef = monitor(process, Pid),
                receive
                    {'DOWN', MRef, process, Pid, _Reason} ->
                        ok
                after ?SHUTDOWN_TIMEOUT ->
                    demonitor(MRef, [flush]),
                    %% Force terminate if graceful shutdown fails
                    exit(Pid, shutdown),
                    ok
                end
            catch
                _:_ ->
                    %% Process may have already terminated
                    ok
            end;
        false ->
            {error, not_found}
    end;
stop_channel(ChannelId) when is_binary(ChannelId) ->
    case find_channel_pid(ChannelId) of
        {ok, Pid} ->
            stop_channel(Pid);
        error ->
            {error, not_found}
    end.

%% @doc List all active peer channel PIDs
-spec list_channels() -> [pid()].
list_channels() ->
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children,
            is_pid(Pid),
            is_process_alive(Pid)].

%% @doc Count active peer channels
-spec count_channels() -> non_neg_integer().
count_channels() ->
    length(list_channels()).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
%% @doc Initialize the supervisor with simple_one_for_one strategy
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => ?MAX_RESTARTS,
        period => ?RESTART_PERIOD
    },

    %% Child spec for pqc_peer_channel gen_server
    %% Args will be provided when start_child/2 is called
    ChannelChild = #{
        id => pqc_peer_channel,
        start => {pqc_peer_channel, start_link, []},
        restart => transient,  % Restart only on abnormal termination
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [pqc_peer_channel]
    },

    {ok, {SupFlags, [ChannelChild]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Find a peer channel PID by channel ID
-spec find_channel_pid(binary()) -> {ok, pid()} | error.
find_channel_pid(ChannelId) ->
    %% Try gproc first if available
    case code:is_loaded(gproc) of
        false ->
            find_channel_pid_by_scan(ChannelId);
        _ ->
            try
                Pid = gproc:lookup_pid({n, l, {pqc_peer_channel, ChannelId}}),
                {ok, Pid}
            catch
                _:_ ->
                    find_channel_pid_by_scan(ChannelId)
            end
    end.

%% @private
%% @doc Find channel PID by scanning all children
-spec find_channel_pid_by_scan(binary()) -> {ok, pid()} | error.
find_channel_pid_by_scan(ChannelId) ->
    Channels = list_channels(),
    find_channel_in_list(ChannelId, Channels).

%% @private
find_channel_in_list(_ChannelId, []) ->
    error;
find_channel_in_list(ChannelId, [Pid | Rest]) ->
    try
        case pqc_peer_channel:get_channel_info(Pid) of
            {ok, #{id := ChannelId}} ->
                {ok, Pid};
            _ ->
                find_channel_in_list(ChannelId, Rest)
        end
    catch
        _:_ ->
            find_channel_in_list(ChannelId, Rest)
    end.
