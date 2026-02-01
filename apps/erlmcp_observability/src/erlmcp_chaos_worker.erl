%%%====================================================================
%%% @doc Chaos Experiment Worker
%%%
%%% Supervised gen_server for running chaos experiments.
%%% Replaces unsupervised spawn_link/1 with proper OTP supervision.
%%%
%%% Supervision: Managed by erlmcp_chaos_worker_sup
%%% Strategy: simple_one_for_one - one worker per experiment
%%%
%%% @end
%%%====================================================================
-module(erlmcp_chaos_worker).

-behaviour(gen_server).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/4]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type experiment_id() :: binary() | atom().
-type experiment_type() ::
    network_latency |
    network_partition |
    packet_loss |
    kill_servers |
    kill_random |
    resource_memory |
    resource_cpu |
    resource_disk |
    clock_skew.

-record(state,
        {parent :: pid(),
         experiment_id :: experiment_id(),
         experiment_type :: experiment_type(),
         config :: map()}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a chaos experiment worker
-spec start_link(pid(), experiment_id(), experiment_type(), map()) -> {ok, pid()} | {error, term()}.
start_link(Parent, ExperimentId, Type, Config) ->
    gen_server:start_link(?MODULE, [Parent, ExperimentId, Type, Config], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Parent, ExperimentId, Type, Config]) ->
    %% Start experiment execution immediately
    gen_server:cast(self(), run_experiment),
    {ok,
     #state{parent = Parent,
            experiment_id = ExperimentId,
            experiment_type = Type,
            config = Config}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(run_experiment, State) ->
    %% Execute experiment in handle_cast (async)
    run_experiment_worker(State#state.parent,
                          State#state.experiment_id,
                          State#state.experiment_type,
                          State#state.config),
    %% Worker exits after experiment completes or fails
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Run chaos experiment (same logic as before, now supervised)
-spec run_experiment_worker(pid(), experiment_id(), experiment_type(), map()) -> ok.
run_experiment_worker(Parent, ExperimentId, Type, Config) ->
    try
        case Type of
            network_latency ->
                erlmcp_chaos_network:inject_latency(Config);
            network_partition ->
                erlmcp_chaos_network:inject_partition(Config);
            packet_loss ->
                erlmcp_chaos_network:inject_packet_loss(Config);
            kill_servers ->
                erlmcp_chaos_process:kill_servers(Config);
            kill_random ->
                erlmcp_chaos_process:kill_random(Config);
            resource_memory ->
                erlmcp_chaos_resource:exhaust_memory(Config);
            resource_cpu ->
                erlmcp_chaos_resource:saturate_cpu(Config);
            resource_disk ->
                erlmcp_chaos_resource:fill_disk(Config);
            clock_skew ->
                erlmcp_chaos_process:inject_clock_skew(Config)
        end
    catch
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("Experiment ~p failed: ~p:~p~n~p",
                       [ExperimentId, Class, Reason, Stacktrace]),
            Parent ! {experiment_failed, ExperimentId, {Class, Reason}}
    end.
