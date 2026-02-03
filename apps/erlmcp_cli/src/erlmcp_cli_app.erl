%%%-------------------------------------------------------------------
%%% @doc
%%% erlmcp_cli_app - CLI Application
%%%
%%% Top-level application for CLI integration with OTEL.
%%% Coordinates initialization and shutdown of CLI components.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(erlmcp_cli_app).

-behaviour(application).

%% API
-export([start/0]).
%% Application callbacks
-export([start/2, stop/1, prep_stop/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the CLI application
-spec start() -> ok | {error, term()}.
start() ->
    application:start(erlmcp_cli).

%%====================================================================
%% Application callbacks
%%====================================================================

%% @doc Start the CLI application
-spec start(term(), term()) -> {ok, pid()} | {error, term()}.
start(_Type, _Args) ->
    %% Create OTEL span for CLI application start
    erlmcp_otel:with_span("cli.application.start",
                          #{<<"application">> => "erlmcp_cli", <<"version">> => "2.1.0"},
                          fun() ->
                             %% Create supervisor child spec
                             ChildSpec =
                                 #{id => erlmcp_cli_sup,
                                   start => {erlmcp_cli_sup, start_link, []},
                                   restart => permanent,
                                   shutdown => infinity,
                                   type => supervisor,
                                   modules => [erlmcp_cli_sup]},

                             %% Start the application with supervisor
                             erlmcp_sup:start_child(ChildSpec)
                          end).

%% @doc Stop the CLI application
-spec stop(term()) -> ok.
stop(_State) ->
    %% Create OTEL span for CLI application stop
    erlmcp_otel:with_span("cli.application.stop",
                          #{<<"application">> => "erlmcp_cli", <<"version">> => "2.1.0"},
                          fun() ->
                             %% Shutdown metrics
                             erlmcp_cli_metrics:stop(),

                             %% Terminate all sessions
                             Sessions = erlmcp_cli_session_sup:list_sessions(),
                             lists:foreach(fun({SessionId, _, _, _}) ->
                                              erlmcp_cli_session_sup:stop_session(SessionId)
                                           end,
                                           Sessions),

                             ok
                          end).

%% @doc Prepare for graceful shutdown
-spec prep_stop(term()) -> term().
prep_stop(State) ->
    logger:info("Preparing erlmcp_cli for graceful shutdown"),
    %% Signal sessions to complete gracefully
    try
        Sessions = erlmcp_cli_session_sup:list_sessions(),
        lists:foreach(fun({SessionId, _, _, _}) ->
                         erlmcp_cli_session_sup:prepare_shutdown(SessionId)
                      end, Sessions)
    catch
        _:_ -> ok
    end,
    State.
