%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_session_sup - CLI Session Supervisor
%%%
%%% Manages CLI sessions with simple_one_for_one strategy.
%%% Each session is isolated with its own process-per-connection.
%%%
%%% Supervision Strategy: simple_one_for_one
%%% - Session crash: restart individual session only
%%% - Parser crash: restart individual parser only
%%% - Executor crash: restart individual executor only
%%% - Transport crash: restart individual transport only
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_session/2, stop_session/1, list_sessions/0]).
%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the CLI session supervisor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Start a new CLI session
-spec start_session(binary(), map()) -> {ok, pid()} | {error, term()}.
start_session(SessionId, Config) ->
    ChildSpec = build_session_child_spec(SessionId, Config),
    supervisor:start_child(?SERVER, ChildSpec).

%% @doc Stop a CLI session
-spec stop_session(binary()) -> ok | {error, term()}.
stop_session(SessionId) ->
    case supervisor:terminate_child(?SERVER, SessionId) of
        ok ->
            supervisor:delete_child(?SERVER, SessionId);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc List all active CLI sessions
-spec list_sessions() -> [{binary(), pid(), binary(), integer()}].
list_sessions() ->
    Children = supervisor:which_children(?SERVER),
    lists:filtermap(fun({Id, Pid, _Type, _Modules}) ->
        case is_pid(Pid) andalso is_process_alive(Pid) of
            true ->
                %% Get session info
                case erlmcp_cli_session:get_info(Pid) of
                    {ok, Info} ->
                        {true, {Id, Pid, Info#cli_session.status, Info#cli_session.created}};
                    _ ->
                        {true, {Id, Pid, unknown, erlang:system_time(millisecond)}}
                end;
            false ->
                false
        end
    end, Children).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Initialize the session supervisor
%% Strategy: simple_one_for_one - individual session isolation
-spec init(list()) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init(_Opts) ->
    SupFlags =
        #{strategy => simple_one_for_one,  % Dynamic session management
          intensity => 3,                   % Max 3 restarts per session
          period => 30},                    % Within 30 seconds

    %% Start with empty child specs - sessions are added dynamically
    Children = [],

    {ok, {SupFlags, Children}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Build session child specification
-spec build_session_child_spec(binary(), map()) -> supervisor:child_spec().
build_session_child_spec(SessionId, Config) ->
    #{
        id => SessionId,
        start => {erlmcp_cli_session, start_link, [SessionId, Config]},
        restart => transient,    % Don't restart failed sessions
        shutdown => 5000,        % Graceful shutdown
        type => worker,
        modules => [erlmcp_cli_session]
    }.