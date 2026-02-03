%%%-------------------------------------------------------------------
%%% @doc PQChain Contract Supervisor
%%%
%%% Simple_one_for_one supervisor for dynamic smart contract instances.
%%% Each contract is a SwarmFlow workflow net case process.
%%%
%%% Architecture:
%%% - Uses simple_one_for_one for dynamic contract spawning
%%% - Each child is a pqc_contract gen_statem process
%%% - Contracts are permanent - always restarted (persistent state)
%%% - Each contract manages workflow net execution with blockchain state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_contract_sup).

-behaviour(supervisor).

-include("pqchain.hrl").

%% API
-export([start_link/0,
         start_contract/2,
         start_contract/3,
         stop_contract/1,
         list_contracts/0,
         count_contracts/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits - contracts are stateful
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds
-define(SHUTDOWN_TIMEOUT, 10000).  % 10 seconds for state persistence

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the contract supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new contract instance
%%
%% Args:
%%   ContractAddress: Blockchain address of the contract
%%   InitialState: Initial contract state (marking, variables)
%%
-spec start_contract(binary(), map()) -> {ok, pid()} | {error, term()}.
start_contract(ContractAddress, InitialState) ->
    start_contract(ContractAddress, InitialState, #{}).

%% @doc Start a new contract instance with options
%%
%% Options can include:
%%   - case_id: SwarmFlow case ID (generated if not provided)
%%   - workflow_net_id: Workflow net definition ID
%%   - creator: Contract creator address
%%   - gas_limit: Execution gas limit
%%
-spec start_contract(binary(), map(), map()) -> {ok, pid()} | {error, term()}.
start_contract(ContractAddress, InitialState, Options)
  when is_binary(ContractAddress), is_map(InitialState), is_map(Options) ->
    supervisor:start_child(?SERVER, [ContractAddress, InitialState, Options]).

%% @doc Stop a contract instance gracefully
%%
%% Sends stop message to the contract process and waits for clean shutdown.
%% Returns ok if contract was stopped, {error, not_found} if contract doesn't exist.
%%
-spec stop_contract(pid() | binary()) -> ok | {error, not_found | term()}.
stop_contract(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            try
                %% Try graceful shutdown with state persistence
                ok = pqc_contract:terminate(Pid),
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
stop_contract(ContractAddress) when is_binary(ContractAddress) ->
    case find_contract_pid(ContractAddress) of
        {ok, Pid} ->
            stop_contract(Pid);
        error ->
            {error, not_found}
    end.

%% @doc List all active contract PIDs
-spec list_contracts() -> [pid()].
list_contracts() ->
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children,
            is_pid(Pid),
            is_process_alive(Pid)].

%% @doc Count active contracts
-spec count_contracts() -> non_neg_integer().
count_contracts() ->
    length(list_contracts()).

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

    %% Child spec for pqc_contract gen_statem
    %% Args will be provided when start_child/2 is called
    ContractChild = #{
        id => pqc_contract,
        start => {pqc_contract, start_link, []},
        restart => transient,  % Restart only on abnormal termination
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [pqc_contract]
    },

    {ok, {SupFlags, [ContractChild]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Find a contract PID by contract address
-spec find_contract_pid(binary()) -> {ok, pid()} | error.
find_contract_pid(ContractAddress) ->
    %% Try gproc first if available
    case code:is_loaded(gproc) of
        false ->
            find_contract_pid_by_scan(ContractAddress);
        _ ->
            try
                Pid = gproc:lookup_pid({n, l, {pqc_contract, ContractAddress}}),
                {ok, Pid}
            catch
                _:_ ->
                    find_contract_pid_by_scan(ContractAddress)
            end
    end.

%% @private
%% @doc Find contract PID by scanning all children
-spec find_contract_pid_by_scan(binary()) -> {ok, pid()} | error.
find_contract_pid_by_scan(ContractAddress) ->
    Contracts = list_contracts(),
    find_contract_in_list(ContractAddress, Contracts).

%% @private
find_contract_in_list(_ContractAddress, []) ->
    error;
find_contract_in_list(ContractAddress, [Pid | Rest]) ->
    try
        case pqc_contract:get_address(Pid) of
            {ok, ContractAddress} ->
                {ok, Pid};
            _ ->
                find_contract_in_list(ContractAddress, Rest)
        end
    catch
        _:_ ->
            find_contract_in_list(ContractAddress, Rest)
    end.
