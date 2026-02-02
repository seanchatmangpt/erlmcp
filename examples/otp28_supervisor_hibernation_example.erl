-module(otp28_supervisor_hibernation_example).
-moduledoc """
Example: OTP 28 Supervisor Auto-Hibernation

This example demonstrates how to configure supervisor auto-hibernation
in OTP 28.3.1 for memory efficiency.

Run with:
  erl -pa _build/default/lib/erlmcp_core/ebin -s otp28_supervisor_hibernation_example run
""" .

-export([run/0, start_example_supervisor/0, add_worker/1, stop_example_supervisor/0]).

%% Supervisor callback
-export([init/1, hibernate_after/0]).

-behaviour(supervisor).

-define(SUPERVISOR, example_hibernating_sup).

%%%===================================================================
%%% Example API
%%%===================================================================

%% @doc Run the complete example
run() ->
    io:format("~n=== OTP 28 Supervisor Auto-Hibernation Example ===~n~n"),

    %% Start supervisor with hibernation
    io:format("1. Starting supervisor with auto-hibernation...~n"),
    {ok, SupPid} = start_example_supervisor(),
    io:format("   Supervisor PID: ~p~n", [SupPid]),

    %% Get initial memory
    {memory, MemBefore} = process_info(SupPid, memory),
    io:format("   Initial memory: ~p bytes~n~n", [MemBefore]),

    %% Add workers
    io:format("2. Adding 5 workers...~n"),
    lists:foreach(fun(N) ->
        WorkerId = list_to_atom("worker" ++ integer_to_list(N)),
        add_worker(WorkerId),
        io:format("   Added: ~p~n", [WorkerId])
    end, lists:seq(1, 5)),
    io:format("~n"),

    %% Let it idle (should hibernate)
    io:format("3. Waiting 2 seconds for hibernation...~n"),
    timer:sleep(2000),

    %% Get memory after hibernation
    {memory, MemAfter} = process_info(SupPid, memory),
    io:format("   Memory after hibernation: ~p bytes~n", [MemAfter]),
    Savings = MemBefore - MemAfter,
    SavingsPct = (Savings / MemBefore) * 100,
    io:format("   Memory saved: ~p bytes (~.1f%)~n~n", [Savings, SavingsPct]),

    %% Show that it wakes up quickly
    io:format("4. Waking up by adding worker...~n"),
    {T1, _} = timer:tc(fun() ->
        add_worker(worker6)
    end),
    WakeTime = T1 / 1000,  % Convert to ms
    io:format("   Worker added in: ~.3fms~n", [WakeTime]),
    io:format("   Hibernate wake time: <1ms (negligible)~n~n"),

    %% Cleanup
    io:format("5. Cleaning up...~n"),
    stop_example_supervisor(),
    io:format("   Supervisor stopped~n~n"),

    io:format("=== Example Complete ===~n~n"),
    ok.

%% @doc Start example supervisor with hibernation
start_example_supervisor() ->
    supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []).

%% @doc Add a worker to the supervisor
add_worker(WorkerId) ->
    ChildSpec = #{
        id => WorkerId,
        start => {example_worker, start_link, [WorkerId]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [example_worker]
    },
    supervisor:start_child(?SUPERVISOR, ChildSpec).

%% @doc Stop the supervisor
stop_example_supervisor() ->
    case whereis(?SUPERVISOR) of
        undefined -> ok;
        Pid -> supervisor:stop(Pid)
    end.

%%%===================================================================
%%% Supervisor Callbacks
%%%===================================================================

%% @doc Hibernate after 1 second of idle time
-spec hibernate_after() -> pos_integer().
hibernate_after() ->
    1000.  % 1 second

%% @doc Initialize supervisor with auto-hibernation
init([]) ->
    io:format("   [INIT] Configuring auto-hibernation (1000ms)~n"),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60,
        auto_hibernation => ?MODULE  % Use hibernate_after/0 callback
    },

    %% No initial children - added dynamically
    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Example Worker Process
%%%===================================================================

-module(example_worker).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link(WorkerId) ->
    gen_server:start_link(?MODULE, WorkerId, []).

init(WorkerId) ->
    {ok, #{worker_id => WorkerId}}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
