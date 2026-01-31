-module(erlmcp_notification_handler_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_handler/3]).

%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_handler(binary(), term(), map()) -> {ok, pid()} | {error, term()}.
start_handler(Method, Handler, Params) ->
    supervisor:start_child(?MODULE, [Method, Handler, Params]).

%%====================================================================
%% supervisor callbacks
%%====================================================================

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    %% Notification handler supervisor
    %% Strategy: simple_one_for_one - dynamic handler processes
    %% Restart: transient - restart only on abnormal termination
    %% This ensures handlers that exit normally are not restarted,
    %% but crashed handlers are automatically restarted.

    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 5,
        period => 60
    },

    %% CRITICAL FIX: For simple_one_for_one, start args MUST be a list.
    %% The args passed to start_handler/3 are appended to this list.
    %% We provide placeholder args that will be replaced.
    ChildSpecs = [
        #{
            id => notification_handler,
            start => {erlmcp_notification_handler, start_link, [undefined, undefined, #{}]},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [erlmcp_notification_handler]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
