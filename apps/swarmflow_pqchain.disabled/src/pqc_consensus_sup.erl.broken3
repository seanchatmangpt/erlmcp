%%%-------------------------------------------------------------------
%%% @doc PQChain Consensus Supervisor
%%%
%%% Simple_one_for_one supervisor for dynamic consensus round processes.
%%% Each consensus round is a gen_statem managing the BFT consensus protocol.
%%%
%%% Architecture:
%%% - Uses simple_one_for_one for dynamic round creation
%%% - Each child is a pqc_consensus_round gen_statem process
%%% - Rounds are transient - restart only on abnormal termination
%%% - Each round manages propose/prevote/precommit/commit phases
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pqc_consensus_sup).

-behaviour(supervisor).

-include("pqchain.hrl").

%% API
-export([start_link/0,
         stop_round/1,
         list_rounds/0,
         count_rounds/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Restart intensity limits - more aggressive for consensus
-define(MAX_RESTARTS, 10).
-define(RESTART_PERIOD, 60).  % seconds
-define(SHUTDOWN_TIMEOUT, 5000).  % 5 seconds for round cleanup

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the consensus supervisor
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new consensus round
%%
%% Args:
%%   Height: Block height for this round
%%   Round: Round number (0-indexed, increments on timeout/failure)
%%   ValidatorSet: Current validator set for this height
%%
-spec start_round(non_neg_integer(), non_neg_integer(), #validator_set{}) ->
    {ok, pid()} | {error, term()}.
start_round(Height, Round, ValidatorSet)
  when is_integer(Height), Height >= 0,
       is_integer(Round), Round >= 0,
       element(1, ValidatorSet) =:= validator_set ->
    supervisor:start_child(?SERVER, [Height, Round, ValidatorSet]).

%% @doc Stop a consensus round gracefully
%%
%% Sends finalize message to the round process and waits for clean shutdown.
%% Returns ok if round was stopped, {error, not_found} if round doesn't exist.
%%
-spec stop_round(pid() | {non_neg_integer(), non_neg_integer()}) ->
    ok | {error, not_found | term()}.
stop_round(Pid) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            try
                %% Try graceful shutdown
                ok = pqc_consensus_round:finalize(Pid),
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
stop_round({Height, Round}) ->
    case find_round_pid(Height, Round) of
        {ok, Pid} ->
            stop_round(Pid);
        error ->
            {error, not_found}
    end.

%% @doc List all active consensus round PIDs
-spec list_rounds() -> [pid()].
list_rounds() ->
    Children = supervisor:which_children(?SERVER),
    [Pid || {_Id, Pid, _Type, _Modules} <- Children,
            is_pid(Pid),
            is_process_alive(Pid)].

%% @doc Count active consensus rounds
-spec count_rounds() -> non_neg_integer().
count_rounds() ->
    length(list_rounds()).

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

    %% Child spec for pqc_consensus_round gen_statem
    %% Args will be provided when start_child/2 is called
    RoundChild = #{
        id => pqc_consensus_round,
        start => {pqc_consensus_round, start_link, []},
        restart => transient,  % Restart only on abnormal termination
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [pqc_consensus_round]
    },

    {ok, {SupFlags, [RoundChild]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Find a consensus round PID by height and round number
-spec find_round_pid(non_neg_integer(), non_neg_integer()) -> {ok, pid()} | error.
find_round_pid(Height, Round) ->
    %% Try gproc first if available
    case code:is_loaded(gproc) of
        false ->
            find_round_pid_by_scan(Height, Round);
        _ ->
            try
                Pid = gproc:lookup_pid({n, l, {pqc_consensus_round, Height, Round}}),
                {ok, Pid}
            catch
                _:_ ->
                    find_round_pid_by_scan(Height, Round)
            end
    end.

%% @private
%% @doc Find round PID by scanning all children
-spec find_round_pid_by_scan(non_neg_integer(), non_neg_integer()) ->
    {ok, pid()} | error.
find_round_pid_by_scan(Height, Round) ->
    Rounds = list_rounds(),
    find_round_in_list(Height, Round, Rounds).

%% @private
find_round_in_list(_Height, _Round, []) ->
    error;
find_round_in_list(Height, Round, [Pid | Rest]) ->
    try
        case pqc_consensus_round:get_round_info(Pid) of
            {ok, #{height := Height, round := Round}} ->
                {ok, Pid};
            _ ->
                find_round_in_list(Height, Round, Rest)
        end
    catch
        _:_ ->
            find_round_in_list(Height, Round, Rest)
    end.
