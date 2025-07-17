-module(erlmcp_sup).
-behaviour(supervisor).

-export([start_link/0, start_stdio_server/0, start_stdio_server/1, stop_stdio_server/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% API for managing stdio server
-spec start_stdio_server() -> {ok, pid()} | {error, term()}.
start_stdio_server() ->
    start_stdio_server(#{}).

-spec start_stdio_server(map()) -> {ok, pid()} | {error, term()}.
start_stdio_server(Options) ->
    ChildSpec = #{
        id => stdio_supervisor,
        start => {erlmcp_stdio_supervisor, start_link, [Options]},
        restart => temporary,  % Don't restart automatically
        shutdown => 5000,
        type => supervisor,
        modules => [erlmcp_stdio_supervisor]
    },
    supervisor:start_child(?SERVER, ChildSpec).

-spec stop_stdio_server() -> ok | {error, term()}.
stop_stdio_server() ->
    case supervisor:terminate_child(?SERVER, stdio_supervisor) of
        ok ->
            supervisor:delete_child(?SERVER, stdio_supervisor);
        Error ->
            Error
    end.

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,  % Changed to one_for_one for better isolation
        intensity => 3,
        period => 60
    },
    
    % Start with the existing supervision tree
    % The stdio server will be added dynamically when needed
    ChildSpecs = [],
    
    {ok, {SupFlags, ChildSpecs}}.