%%%-------------------------------------------------------------------
%%% @doc
%%% Core Health Check Module
%%%
%%% Simple health check aggregator for core erlmcp services.
%%% Following Joe Armstrong's principle: "Health checks are for orchestration"
%%%
%%% Provides up/down status for Kubernetes and other orchestrators.
%%% Each check is a simple process-per-health-check pattern.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_health).

-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([check/0]).
-export([register_check/2, unregister_check/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Types
-type check_name() :: atom().
-type check_result() :: healthy | degraded | unhealthy.
-type check_fun() :: {module(), atom(), [term()]}.
-type health_report() :: #{healthy := boolean(), checks := #{check_name() := check_result()}}.

-record(state, {checks :: #{check_name() => check_fun()}}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start health check server
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Perform all health checks and return overall status
%% Returns #{healthy => boolean(), checks => map()}
-spec check() -> health_report().
check() ->
    gen_server:call(?MODULE, check).

%% @doc Register a health check function
%% CheckFun is {Module, Function, Args} - returns ok | {ok, _} | {error, _}
-spec register_check(check_name(), check_fun()) -> ok.
register_check(Name, CheckFun) ->
    gen_server:call(?MODULE, {register, Name, CheckFun}).

%% @doc Unregister a health check function
-spec unregister_check(check_name()) -> ok.
unregister_check(Name) ->
    gen_server:call(?MODULE, {unregister, Name}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),

    %% Register default checks for core services
    %% These checks verify that critical gen_servers are running
    DefaultChecks =
        #{registry => {erlmcp_registry, get_pid, []},
          session_manager => {erlmcp_session_manager, list_sessions, []}},

    logger:info("Starting health check server with ~p default checks", [maps:size(DefaultChecks)]),

    {ok, #state{checks = DefaultChecks}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(check, _From, State) ->
    %% Run all health checks and collect results
    Results = maps:map(fun(_Name, {M, F, A}) -> run_check(M, F, A) end, State#state.checks),

    %% Overall healthy if no checks returned unhealthy
    AllResults = maps:values(Results),
    Healthy = not lists:member(unhealthy, AllResults),

    Report = #{healthy => Healthy, checks => Results},

    {reply, Report, State};
handle_call({register, Name, CheckFun}, _From, State) ->
    NewChecks = maps:put(Name, CheckFun, State#state.checks),
    logger:info("Registered health check: ~p", [Name]),
    {reply, ok, State#state{checks = NewChecks}};
handle_call({unregister, Name}, _From, State) ->
    NewChecks = maps:remove(Name, State#state.checks),
    logger:info("Unregistered health check: ~p", [Name]),
    {reply, ok, State#state{checks = NewChecks}};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    logger:info("Health check server terminating"),
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Run a single health check
%% Returns healthy if process is alive and responding
%% Returns unhealthy if process is dead or not responding
-spec run_check(module(), atom(), [term()]) -> check_result().
run_check(M, F, A) ->
    try
        case apply(M, F, A) of
            Result when Result =:= ok; Result =:= [] ->
                %% No errors and empty results are considered healthy
                healthy;
            {ok, _} ->
                healthy;
            [_ | _] ->
                %% Non-empty list results are healthy (e.g., list_sessions)
                healthy;
            Atom when is_atom(Atom) ->
                %% Atom results (e.g., node(), undefined) are healthy if not error atom
                case Atom of
                    undefined ->
                        degraded;
                    error ->
                        unhealthy;
                    _ ->
                        healthy
                end;
            Int when is_integer(Int) ->
                %% Integer results (e.g., timestamps, counts) are healthy
                healthy;
            Tuple when is_tuple(Tuple) ->
                %% Tuple results (e.g., timestamps, MFA results) are healthy
                %% unless they're error tuples
                case Tuple of
                    {error, _} ->
                        unhealthy;
                    _ ->
                        healthy
                end;
            Pid when is_pid(Pid) ->
                %% Pid result - check if alive
                case erlang:is_process_alive(Pid) of
                    true ->
                        healthy;
                    false ->
                        unhealthy
                end;
            _ ->
                degraded
        end
    catch
        %% Exception means service is unhealthy
        _:_ ->
            logger:warning("Health check failed for ~p:~p/~p", [M, F, length(A)]),
            unhealthy
    end.
