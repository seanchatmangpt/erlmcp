-module(erlmcp_port_pool).

-behaviour(gen_server).

%% Port Pool Manager for MCP External Tool Integration
%%
%% This module manages a pool of port workers using poolboy for
%% efficient resource reuse and connection limiting.
%%
%% Features:
%% - Port checkout/checkin
%% - Pool size limits (min: 2, max: 10, overflow: 5)
%% - Load balancing across ports
%% - Port health monitoring
%% - Overflow handling

%% API exports
-export([start_link/0, start_link/1,
         checkout_port/0, checkout_port/1,
         return_port/1,
         pool_status/0,
         healthy_ports/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Type exports
-export_type([pool_config/0, port_worker/0, pool_status/0]).

-include("erlmcp.hrl").

%%====================================================================
%% Types
%%====================================================================

-type pool_name() :: atom().
-type pool_size() :: pos_integer().
-type pool_overflow() :: non_neg_integer().

-record(pool_config,
        {name :: pool_name(),
         size :: pool_size(),
         max_overflow :: pool_overflow(),
         worker_timeout :: timeout()}).

-type pool_config() :: #pool_config{}.

-record(port_worker,
        {pid :: pid(),
        command :: binary() | string(),
        args :: [binary() | string()],
        created_at :: integer(),
        last_used :: integer(),
        request_count = 0 :: non_neg_integer()}).

-type port_worker() :: #port_worker{}.

-record(pool_state,
        {config :: pool_config(),
         workers = #{} :: #{pid() => port_worker()},
         checkout_count = 0 :: non_neg_integer()}).

-type pool_status() :: #{
    size => non_neg_integer(),
    available => non_neg_integer(),
    overflow => non_neg_integer(),
    total_requests => non_neg_integer()
}.

%%====================================================================
%% Macros
%%====================================================================

-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_MAX_OVERFLOW, 5).
-define(DEFAULT_WORKER_TIMEOUT, 5000).
-define(POOL_NAME, erlmcp_port_pool).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start port pool with default configuration
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start port pool with custom configuration
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Checkout port from pool (uses default timeout)
-spec checkout_port() -> {ok, pid()} | {error, term()}.
checkout_port() ->
    checkout_port(5000).

%% @doc Checkout port from pool with custom timeout
-spec checkout_port(timeout()) -> {ok, pid()} | {error, term()}.
checkout_port(Timeout) ->
    poolboy:checkout(?POOL_NAME, true, Timeout).

%% @doc Return port to pool
-spec return_port(pid()) -> ok.
return_port(WorkerPid) when is_pid(WorkerPid) ->
    poolboy:checkin(?POOL_NAME, WorkerPid).

%% @doc Get pool status
-spec pool_status() -> {ok, pool_status()}.
pool_status() ->
    gen_server:call(?MODULE, pool_status, 1000).

%% @doc Get healthy (available) port workers
-spec healthy_ports() -> {ok, [pid()]}.
healthy_ports() ->
    gen_server:call(?MODULE, healthy_ports, 1000).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @doc Initialize port pool
init(Options) ->
    %% Create pool configuration
    PoolConfig = #pool_config{
        name = maps:get(pool_name, Options, ?POOL_NAME),
        size = maps:get(size, Options, ?DEFAULT_POOL_SIZE),
        max_overflow = maps:get(max_overflow, Options, ?DEFAULT_MAX_OVERFLOW),
        worker_timeout = maps:get(worker_timeout, Options, ?DEFAULT_WORKER_TIMEOUT)
    },

    %% Start poolboy pool
    PoolArgs = [
        {name, {local, PoolConfig#pool_config.name}},
        {worker_module, erlmcp_port_tool},
        {size, PoolConfig#pool_config.size},
        {max_overflow, PoolConfig#pool_config.max_overflow}
    ],

    %% Start pool (simplified - in production use proper supervisor)
    case poolboy:start_pool(PoolConfig#pool_config.name,
                            PoolArgs,
                            [{}, #{timeout => PoolConfig#pool_config.worker_timeout}]) of
        {ok, _Pid} ->
            logger:info("Port pool started: size=~p, overflow=~p",
                       [PoolConfig#pool_config.size,
                        PoolConfig#pool_config.max_overflow]),
            {ok, #pool_state{config = PoolConfig}};
        {error, {already_started, _Pid}} ->
            %% Pool already exists
            {ok, #pool_state{config = PoolConfig}};
        {error, Reason} ->
            {stop, {pool_start_failed, Reason}}
    end.

%% @doc Handle pool status request
handle_call(pool_status, _From, State) ->
    Status = #{
        size => State#pool_state.config#pool_config.size,
        available => poolboy:status(?POOL_NAME),
        overflow => State#pool_state.config#pool_config.max_overflow,
        total_requests => State#pool_state.checkout_count
    },
    {reply, {ok, Status}, State};

%% @doc Handle healthy ports request
handle_call(healthy_ports, _From, State) ->
    %% Get all workers from pool
    Workers = poolboy:checkout(?POOL_NAME, true, 1000),
    HealthyPids = lists:filter(fun(Pid) ->
                                  is_process_alive(Pid) andalso
                                  case erlmcp_port_tool:port_info(Pid) of
                                      {ok, _Info} -> true;
                                      _ -> false
                                  end
                               end, Workers),
    %% Return all workers to pool
    lists:foreach(fun(Pid) -> poolboy:checkin(?POOL_NAME, Pid) end, Workers),
    {reply, {ok, HealthyPids}, State};

%% @doc Handle unknown calls
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handle cast messages
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc Handle info messages
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminate pool and cleanup
terminate(_Reason, #pool_state{config = Config}) ->
    %% Stop poolboy pool
    catch poolboy:stop_pool(Config#pool_config.name),
    ok.

%% @doc Handle code change
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
