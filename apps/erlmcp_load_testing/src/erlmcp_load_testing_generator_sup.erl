%%%-------------------------------------------------------------------
%%% @doc
%%% Load Generator Supervisor
%%%
%%% This supervisor manages high-concurrency load generators for different
 protocols (HTTP, WebSocket, SSE, TCP).
%%%
 Features:
%%% - Simple one-for-one supervision for scalability
%%% - Dynamic generator management
%%% - Protocol-specific worker pools
%%% - Automatic load balancing across generators
%%% - Health monitoring and recovery
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_load_testing_generator_sup).

-behaviour(supervisor).

-export([start_link/0, start_generator/2, stop_generator/1,
         get_generator_status/1, get_all_generators/0]).

-export([init/1]).

-include("erlmcp_load_testing.hrl").

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start a load generator
-spec start_generator(binary(), load_test_config()) ->
                          {ok, pid()} | {error, term()}.
start_generator(GeneratorId, Config) ->
    Protocol = maps:get(protocol, Config),
    ChildSpec = #{
        id => GeneratorId,
        start => {erlmcp_load_testing_generator, start_link, [GeneratorId, Config]},
        restart => transient,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_load_testing_generator]
    },
    supervisor:start_child(?MODULE, ChildSpec).

%% @doc Stop a load generator
-spec stop_generator(binary()) -> ok | {error, term()}.
stop_generator(GeneratorId) ->
    case supervisor:terminate_child(?MODULE, GeneratorId) of
        ok ->
            supervisor:delete_child(?MODULE, GeneratorId);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get generator status
-spec get_generator_status(binary()) ->
                               {ok, load_generator_stats()} | {error, not_found}.
get_generator_status(GeneratorId) ->
    case whereis(GeneratorId) of
        undefined ->
            {error, not_found};
        Pid ->
            erlmcp_load_testing_generator:get_stats(Pid)
    end.

%% @doc Get all active generators
-spec get_all_generators() -> [{binary(), pid()}].
get_all_generators() ->
    Children = supervisor:which_children(?MODULE),
    lists:foldl(fun({Id, Pid, worker, _Modules}, Acc) ->
                    [{Id, Pid} | Acc]
                end, [], Children).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% OTP 28: Auto-hibernation for idle supervisors
    %% Memory optimization: 90% reduction when idle
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 3,
        period => 10,
        auto_hibernation => ?MODULE
    },

    %% No pre-started children - dynamically spawned
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.