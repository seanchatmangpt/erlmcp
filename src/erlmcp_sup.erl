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
        id => stdio_server,
        start => {erlmcp_stdio_server, start_link, [Options]},
        restart => temporary,  % Don't restart automatically
        shutdown => 5000,
        type => worker,
        modules => [erlmcp_stdio_server]
    },
    supervisor:start_child(?SERVER, ChildSpec).

-spec stop_stdio_server() -> ok.
stop_stdio_server() ->
    case supervisor:terminate_child(?SERVER, stdio_server) of
        ok ->
            case supervisor:delete_child(?SERVER, stdio_server) of
                ok -> ok;
                {error, not_found} -> ok  % Already deleted
            end;
        {error, not_found} ->
            ok  % Child doesn't exist, that's fine
    end.

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 3,
        period => 60
    },
    
    % Start with empty child specs - stdio server is added dynamically
    ChildSpecs = [],
    
    {ok, {SupFlags, ChildSpecs}}.