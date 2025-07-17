-module(erlmcp_stdio_supervisor).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, stop/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    start_link(#{}).

-spec start_link(map()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Options) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Options]).

-spec stop() -> ok.
stop() ->
    case whereis(?SERVER) of
        undefined -> ok;
        Pid -> 
            exit(Pid, shutdown),
            ok
    end.

%%====================================================================
%% Supervisor callbacks
%%====================================================================

-spec init([map()]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([Options]) ->
    SupFlags = #{
        strategy => one_for_all,
        intensity => 3,
        period => 60
    },
    
    ServerSpec = #{
        id => stdio_server,
        start => {erlmcp_stdio_server, start_link, [Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_stdio_server]
    },
    
    ProtocolSpec = #{
        id => stdio_protocol,
        start => {erlmcp_stdio_protocol, start_link, [Options]},
        restart => temporary,  % Don't restart when it terminates normally
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_stdio_protocol]
    },
    
    ChildSpecs = [ServerSpec, ProtocolSpec],
    {ok, {SupFlags, ChildSpecs}}.