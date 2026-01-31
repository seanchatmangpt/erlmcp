-module(erlmcp_transport_http).
%% -behaviour(erlmcp_transport_behavior).  % Conflicts with gen_server init/1

%% Transport behavior callbacks
-export([send/2, close/1, init/1]).

%% API
-export([start_link/1]).

%% Types
-type http_opts() :: #{
    url := binary() | string(),
    owner := pid(),
    method => get | post,
    headers => [{string() | binary(), string() | binary()}],
    timeout => timeout(),
    connect_timeout => timeout(),
    max_retries => non_neg_integer(),
    retry_delay => pos_integer(),
    ssl_options => [ssl:tls_client_option()],
    pool_size => non_neg_integer()
}.

-export_type([http_opts/0]).

%%====================================================================
%% Transport Behavior Implementation
%%====================================================================

%% @doc Initialize transport (starts underlying gen_server)
-spec init(http_opts()) -> {ok, pid()} | {error, term()}.
init(Opts) when is_map(Opts) ->
    %% Delegate to gen_server implementation
    erlmcp_transport_http_server:start_link(Opts).

%% @doc Send data through transport
-spec send(pid(), iodata()) -> ok | {error, term()}.
send(Pid, Data) when is_pid(Pid) ->
    gen_server:call(Pid, {send, Data}).

%% @doc Close transport connection
-spec close(pid()) -> ok.
close(Pid) when is_pid(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link(http_opts()) -> {ok, pid()} | {error, term()}.
start_link(Opts) when is_map(Opts) ->
    erlmcp_transport_http_server:start_link(Opts).
